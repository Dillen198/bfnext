# Plan: authenticated netidx (TLS, or Kerberos)

Status: **TLS implemented, opt-in, off by default** (wave 2). No production
config changed. Audit finding f3, infrastructure part. Operator guide:
`deploy/netidx-tls.md`.

## What was built

* `fowlbg::netidx` (MIT crate shared by the engines): a `netidx_tls` block
  (`trusted`, `certificate`, `private_key`, `resolver_name`, optional
  `domain`, `askpass`) and `load_config(path, tls)`. With no block it is
  `Config::load` / `load_default`, byte for byte today's behaviour. With
  one, the `client.json` is kept as the address book and switched to TLS in
  memory: one identity filed under `domain` (default: the last label of the
  resolver name), `default_auth: Tls`, every resolver address
  `Tls(resolver_name)`. So one `client.json` serves every account on a box
  and each process names its own certificate in its own config, instead of
  one client config per process.
* Engines: `netidx_tls` in the campaign `Cfg` (bflib) and in `RangeCfg`
  (bfrange), checked by `validate()`.
* bfdb: `--netidx-tls <file>` for the default client config, and an
  instance's `netidx_tls` for its own `netidx_config` (instances sharing a
  config must agree). Subscriber setup moved to `bfdb/src/netidx_client.rs`.
* procman: `bfdb.netidx_tls` (`dir`, `identity`, optional `resolver`,
  `grant`, `domain`, `trusted`, `perms`, `askpass`) writes
  `netidx-resolver.tls.json` (the Anonymous file is kept for rollback) and
  `netidx-tls.bfdb.json`; an instance's `netidx_tls_resolver` gets that
  instance a block for a TLS range resolver. Leaving out `resolver` keeps
  the campaign resolver Anonymous, which is the range-first rollout below.
* Checked on loopback with the doc's openssl recipe and netidx 0.27's
  in-process resolver: TLS engine publishes, TLS bfdb reads, Anonymous is
  `Denied`, a CA-signed certificate missing from `perms` can neither publish
  nor read. The unit tests need no resolver: they cover parsing, the
  overlay and the "off = Anonymous" load.

Facts from netidx 0.27's source that the plan below had open: a
certificate must hold exactly one DNS subjectAltName and the `perms` entity
is its CN; keys are PKCS#8 PEM; the certificate file holds one certificate
(no chain); a TLS client cannot use an Anonymous resolver and vice versa,
so a resolver and all its clients switch together; grant strings are
`!swlpd`.

Not done: `-TlsDir` for `setup-range-pc.ps1` / `setup-main-pc.ps1` (the
range resolver's config is a hand edit that re-running the script undoes),
a certificate script, and per-base `perms` narrowing tested against the
stats archive.

## Where things stand

netidx carries the engine's state and its RPCs: priority and commander
spawns, admin actions, and the range's `query-range`. Every resolver we run
is `"auth": "Anonymous"` with `"perms": {"/": {"Anonymous": "swlpd"}}`, so
anyone who can reach it may subscribe, write, list, publish and publish
default. And every publisher accepts anyone the resolver lets through.

| Where | Resolver listens on | Who can reach it |
|---|---|---|
| Campaign box (procman's default `netidx-resolver.json`) | `127.0.0.1:4564` | any process on the box, under any account |
| Range PC (`deploy/range-pc/setup-range-pc.ps1`) | `<range LAN ip>:4564` | the main PC (firewall rule), plus anything on the range PC |
| Range PC publisher (bfrange) | `<range LAN ip>:<5000-32767>` | the main PC (firewall rule) |

So today the firewall is the only boundary. Two things in this round narrow
it without changing behaviour:

* `setup-range-pc.ps1 -PublisherPort <port>` (opt-in) pins bfrange's
  publisher to one port (`netidx_bind` in the range config) and opens only
  that port to the main PC, instead of 5000-32767. Caveat: bfrange binds
  again on every mission start. If the old listener hasn't closed yet, that
  start logs "failed to start the netidx publisher" and its RPCs stay off
  until the next one. bfrange should retry the bind a few times; that is
  noted for the range session.
* The resolver and publisher rules were already limited to `-MainPcIp`.

That still leaves:

1. **Local processes** on either box can publish fake engine paths or call
   engine RPCs. That matters if anything untrusted runs on the box: a DCS
   mod, a compromised bot plugin, a second Windows user.
2. **Anyone who spoofs or takes over the main PC's LAN address** reaches
   the range resolver and RPCs, and the LAN traffic is plaintext.

## Options

netidx 0.27 offers four mechanisms per resolver member: `Anonymous`,
`Local` (unix-socket peer credentials; **stubbed out on Windows**, so not an
option), `Krb5` (Kerberos/GSSAPI, which needs AD) and `Tls` (mutual TLS
with our own CA).

**Recommendation: TLS.** Neither box is in a domain, so Kerberos would mean
standing up AD for two PCs. TLS needs only a private CA and a few files,
and it encrypts the LAN hop as well.

### What TLS looks like

1. **A private CA**, made once, offline, with its key kept like a release
   key (see `deploy/signing-keys.md`). It issues one certificate per
   identity. The netidx identity name is the certificate's CN/SAN:
   * `resolver.campaign.fowl`, `resolver.range.fowl`: the resolvers
   * `engine.campaign.fowl`, `engine.range.fowl`: bflib, bfrange (publishers)
   * `bfdb.fowl`: bfdb (subscriber, and the RPC caller)
   * `tools.fowl`: an admin's `netidx` CLI, if wanted
2. **Resolver config** (`member_servers[].auth`):

   ```json
   "auth": { "Tls": { "name": "resolver.range.fowl",
                      "trusted": "C:/fowl/tls/ca.pem",
                      "certificate": "C:/fowl/tls/resolver.range.fowl.crt",
                      "private_key": "C:/fowl/tls/resolver.range.fowl.key" } }
   ```

   with `perms` naming identities instead of `Anonymous`, e.g.
   `"/local/fowl/range": { "engine.range.fowl": "pd", "bfdb.fowl": "swl" }`.
   Check the exact grant strings against netidx 0.27's `perms` parser
   before rollout.
3. **Client config** (`client.json`) for each account that runs a process:
   `"addrs": [["<ip>:4564", {"Tls": "resolver.range.fowl"}]]`,
   `"default_auth": "Tls"`, and a `"tls"` block whose `identities` holds the
   CA, certificate and key for that process. `default_identity` picks
   between bflib's and bfdb's on a box that runs both under one account.
   That box would need two client configs (`NETIDX_CFG`, or the range
   config's `netidx_config`).
4. **Keys at rest.** netidx can run an `askpass` program for encrypted
   private keys. On Windows, the simplest workable setup is unencrypted
   keys in a folder ACL'd to the one account that needs each. Same model
   as `manager.json`.

### What would have to change in this repo

* **procman.py**: write a TLS resolver config and client config when a new
  `bfdb.netidx_tls:` block names the CA and certificate folder, and keep
  today's Anonymous loopback default when it is absent (opt-in, per the
  no-stricter-defaults rule).
* **setup-range-pc.ps1 / setup-main-pc.ps1**: a `-TlsDir` parameter that
  writes the TLS resolver and client configs and copies the right
  certificates to each PC.
* **bfrange/bflib/bfdb**: probably nothing. They load the netidx client
  config they are pointed at (`netidx_config`, `--netidx-config`). Check
  that bfdb's subscriber and RPC client pick up `default_auth`.
* **A script to make the CA and certificates** (openssl, or `rcgen` in a
  small tool). Its output never goes in the repo.
* **Docs**: deploy/range.md and multi-instance.md (each instance's base
  path gets its own grant).

### Rollout

1. Range PC first: one resolver, one publisher, one subscriber. Run it
   alongside the Anonymous resolver on another port, point bfdb's range
   instance at it, and check `/api/health` and the range RPCs.
2. Switch the range over, keep the Anonymous config for rollback, then
   close the Anonymous port.
3. Campaign box: lower priority, since its resolver is loopback-only. Do
   it once untrusted local code is a real concern.

### Kerberos, for completeness

If the boxes ever join a domain, `Krb5` gives the same per-identity
grants, with the service accounts as identities and no certificates to
renew. Every account that runs DCS, bfdb or the resolver then needs an SPN
and a keytab or logon session, and the DCS account would run as a domain
user.

## Open questions

* Certificate lifetime and renewal. Short-lived certificates need
  automation; one-year certificates need a calendar reminder.
* Whether the in-DCS publisher (bflib or bfrange, running as the DCS
  account) can read a key file that DCS's mods can't. Probably not: they
  run as the same account. TLS then protects the network and other
  accounts, not code inside DCS itself.
