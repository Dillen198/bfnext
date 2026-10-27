# Authenticated netidx (mutual TLS)

netidx carries the engines' live state and their RPCs (priority and
commander spawns, admin actions, the range's `query-range` and spawns).
Out of the box every resolver is Anonymous: anything that can reach it can
subscribe, publish and call. This page turns on mutual TLS with a private
CA, so only processes holding a certificate from that CA, and named in the
resolver's permissions, get in. It also encrypts the LAN hop to a range PC.

**Off by default.** With none of the keys below set, every process loads its
netidx client config exactly as before and nothing changes. The design and
the alternatives (Kerberos, `Local`) are in `docs/plans/netidx-auth.md`.

## How it fits together

* `client.json` stays what it is today: the address book (where the
  resolver is, the publisher bind). You do not edit it.
* Each process that talks netidx gets a `netidx_tls` block in its own
  config naming **its** certificate and the resolver's name. On load, the
  client config is switched to TLS in memory (`fowlbg::netidx::load_config`):
  every resolver address authenticates as that resolver name, and this
  process presents its certificate.
* The resolver gets a TLS member server and `perms` that name identities
  (a certificate's CN) instead of `Anonymous`.

netidx enforces these, so plan for them:

* **All or nothing per resolver.** A TLS client cannot talk to an Anonymous
  resolver ("requested authentication mechanism not supported"), and an
  Anonymous client is refused by a TLS resolver ("Denied"). The resolver and
  every engine and bfdb on it switch together.
* **One certificate per file, exactly one DNS subjectAltName, CN = that
  name.** No chain in the certificate file: issue straight from the CA.
* **Keys are PKCS#8 PEM** (`-----BEGIN PRIVATE KEY-----`, which
  `openssl genpkey` writes), optionally encrypted.
* Names are checked against the certificate, not DNS: `resolver.range.fowl`
  never has to resolve. The IP addresses stay in `client.json`.

The recipe below was checked end to end on loopback against netidx 0.27:
a TLS engine published, TLS bfdb read it, an Anonymous client was
refused, and a certificate from the same CA that is not in `perms` could
neither publish nor read.

## 1. The CA and the certificates

Once, on an admin PC (not a server), in Git Bash (it ships `openssl`; any
OpenSSL 3 works). Keep `ca.key` offline like a release key
(`deploy/signing-keys.md`): whoever holds it can mint a certificate the
resolver trusts.

```bash
mkdir fowl-tls && cd fowl-tls
export MSYS_NO_PATHCONV=1   # Git Bash: keep "/CN=..." from becoming a path
openssl genpkey -algorithm EC -pkeyopt ec_paramgen_curve:P-256 -out ca.key
openssl req -x509 -new -key ca.key -sha256 -days 3650 -subj "/CN=Vector Strike netidx CA" \
  -addext "basicConstraints=critical,CA:TRUE" -addext "keyUsage=critical,keyCertSign,cRLSign" -out ca.pem
for name in resolver.campaign.fowl engine.campaign.fowl bfdb.fowl resolver.range.fowl engine.range.fowl; do
  openssl genpkey -algorithm EC -pkeyopt ec_paramgen_curve:P-256 -out "$name.key"
  openssl req -new -key "$name.key" -subj "/CN=$name" -out "$name.csr"
  printf "basicConstraints=critical,CA:FALSE\nkeyUsage=critical,digitalSignature\nextendedKeyUsage=serverAuth,clientAuth\nsubjectAltName=DNS:%s\n" "$name" > "$name.ext"
  openssl x509 -req -in "$name.csr" -CA ca.pem -CAkey ca.key -CAcreateserial -days 365 -sha256 -extfile "$name.ext" -out "$name.crt"
done
rm -f *.csr *.ext
```

Both extended key usages go on every certificate: an engine is a TLS client
of the resolver and a TLS server to bfdb.

| Identity | Who presents it | Box |
|---|---|---|
| `resolver.campaign.fowl` | the resolver procman runs | campaign box |
| `engine.campaign.fowl` | bflib, in every campaign DCS server (or one name per server: `vs1.campaign.fowl`, ...) | campaign box |
| `bfdb.fowl` | bfdb (subscriber and RPC caller), for both resolvers | campaign box |
| `resolver.range.fowl` | the range PC's resolver | range PC |
| `engine.range.fowl` | bfrange | range PC |

Add `tools.fowl` if an admin wants the `netidx` CLI on a TLS resolver.
Every name here ends in `.fowl`: that suffix is the default `domain`
(below), which is how netidx picks the one identity a process has for every
server it talks to. Use another suffix consistently if you like.

## 2. Where the files go

Never copy `ca.key`. Each box gets `ca.pem` plus only the pairs it runs.

| Box | Folder (suggested) | Files | Readable by |
|---|---|---|---|
| campaign box | `C:\fowl\tls` | `ca.pem`, `resolver.campaign.fowl.*`, `bfdb.fowl.*` | the bot's account (it runs the resolver and bfdb) |
| campaign box | `C:\fowl\tls-dcs` | `ca.pem`, `engine.campaign.fowl.*` | the DCS account |
| range PC | `C:\fowl-range\tls` | `ca.pem`, `resolver.range.fowl.*`, `engine.range.fowl.*` | the resolver task's account and the DCS account |

Lock each folder to its account, e.g.

```powershell
icacls C:\fowl\tls /inheritance:r /grant:r "${env:USERNAME}:(OI)(CI)F" "SYSTEM:(OI)(CI)F" "Administrators:(OI)(CI)F"
```

If the bot and DCS run as the same account, one folder is enough, but read
the open question at the end first.

## 3. Config keys

### The block

The same shape everywhere (`fowlbg::netidx::Tls`):

```json
"netidx_tls": {
  "trusted": "C:/fowl/tls-dcs/ca.pem",
  "certificate": "C:/fowl/tls-dcs/engine.campaign.fowl.crt",
  "private_key": "C:/fowl/tls-dcs/engine.campaign.fowl.key",
  "resolver_name": "resolver.campaign.fowl"
}
```

| Key | Meaning |
|---|---|
| `trusted` | The CA certificate(s), PEM |
| `certificate`, `private_key` | This process's identity |
| `resolver_name` | The resolver's certificate name; every resolver address in `client.json` is switched to TLS under it |
| `domain` (optional) | The server names this identity is offered to. Default: the last label of `resolver_name` (`fowl`) |
| `askpass` (optional) | A program that prints an encrypted key's password (netidx tries the account's Windows Credential Manager first, entry `netidx` / the key path) |

Unknown keys are refused, and a `resolver_name` outside `domain` is refused
at config load, not at the first connection.

### Campaign engine (bflib)

`netidx_tls` at the top level of the engine config (`<sortie>_CFG`, the file
the admin config editor shows), beside `netidx_base`. bflib then logs
`netidx: TLS to resolver resolver.campaign.fowl as C:/fowl/tls-dcs/...` in
`Logs\bfnext.txt` on the next mission load.

### Range engine (bfrange)

`netidx_tls` in the range config (`<sortie>_RANGE`), beside `netidx_config`
and `netidx_bind`, with `resolver_name: "resolver.range.fowl"` and the
`engine.range.fowl` pair. `Logs\bfrange.txt` says `netidx: TLS to resolver
...`.

### bfdb

* `--netidx-tls <file>`: a JSON file holding one block (no `"netidx_tls":`
  wrapper) for the default client config, i.e. the campaign resolver.
* In `instances.json`, an instance's `netidx_tls` does the same for that
  instance's own `netidx_config` (the range PC's resolver), or overrides
  `--netidx-tls` for an instance on the default config. Instances sharing a
  client config share one connection, so they must carry the same block;
  bfdb refuses to start if they don't.

With the bot managing bfdb you do not write either by hand.

### The bot (`fowlengine.yaml`)

```yaml
bfdb:
  netidx_tls:
    dir: "C:\\fowl\\tls"                 # ca.pem, <name>.crt, <name>.key
    identity: "bfdb.fowl"                # bfdb's certificate
    resolver: "resolver.campaign.fowl"   # this box's resolver; omit to keep it Anonymous
    grant: ["engine.campaign.fowl"]      # the engines: full access, like bfdb
    # domain: "fowl"
    # trusted: "C:\\fowl\\tls\\ca.pem"   # default <dir>\ca.pem
    # perms: {...}                       # replaces the generated perms
    # askpass: "C:\\fowl\\askpass.exe"
  instances:
    - id: range
      netidx_resolver: "192.168.1.60:4564"
      netidx_tls_resolver: "resolver.range.fowl"   # that resolver runs TLS
```

What procman does with it, on every bfdb start:

* `resolver` set: runs the resolver from `<home>\netidx-resolver.tls.json`
  (rewritten each start, loopback, port 4564, TLS member server, `perms`
  granting `swlpd` on `/` to `identity` and each `grant`), and passes bfdb
  `--netidx-tls <home>\netidx-tls.bfdb.json`. `netidx-resolver.json` (the
  Anonymous one) is left alone for rolling back.
* `netidx_tls_resolver` on an instance: that instance's entry in
  `instances.json` gets `netidx_tls` with bfdb's identity and that name.
* `bfdb.netidx_resolver_config` set: that file is yours and is never
  rewritten; procman warns, and you make it TLS by hand (next section).
* A block missing `dir` or `identity`, or with `resolver` outside `domain`,
  is logged and ignored: netidx stays Anonymous rather than half-switched.

### A resolver config by hand

For the range PC (`C:\fowl-range\resolver.json`, written by
`setup-range-pc.ps1`) or your own `netidx_resolver_config`, replace the
member server's `"auth": "Anonymous"` and the `perms`:

```json
"auth": {"Tls": {"name": "resolver.range.fowl",
                 "trusted": "C:/fowl-range/tls/ca.pem",
                 "certificate": "C:/fowl-range/tls/resolver.range.fowl.crt",
                 "private_key": "C:/fowl-range/tls/resolver.range.fowl.key"}}
```

```json
"perms": {"/": {"engine.range.fowl": "swlpd", "bfdb.fowl": "swlpd"}}
```

Keep `addr`, `id_map_type: DoNotMap` and the rest. **Re-running
`setup-range-pc.ps1` rewrites `resolver.json` back to Anonymous**: re-apply
this edit after it (or keep a copy as `resolver.tls.json` and point the
resolver task at it).

### Permissions

Keys are paths, values a set of `s` subscribe, `w` write (RPC calls are
writes), `l` list, `p` publish, `d` publish default, with a leading `!` to
deny. The entity is a certificate's CN. Full access for every known
identity, as above, already shuts out everything without a certificate from
the CA and every certificate not listed. Finer grants are possible, e.g.
engines may only publish under their base:

```json
"perms": {"/local/fowl/campaign": {"engine.campaign.fowl": "pd", "bfdb.fowl": "swl"}}
```

but bflib's stats archive also subscribes and lists under its own base, so
test any narrowing on a test server before relying on it.

## 4. Rollout

Order matters because of "all or nothing per resolver". Do it in a quiet
window; each step below is a few minutes of no live data for that resolver.

### Range PC first

It is the one reached over the LAN.

1. Copy `ca.pem`, `resolver.range.fowl.*` and `engine.range.fowl.*` to
   `C:\fowl-range\tls` on the range PC; `ca.pem` and `bfdb.fowl.*` to
   `C:\fowl\tls` on the campaign box.
2. Range PC: edit `resolver.json` as above; add `netidx_tls` to
   `<sortie>_RANGE`.
3. Campaign box: in `fowlengine.yaml` add `bfdb.netidx_tls` with `dir` and
   `identity` **but no `resolver`** (the campaign resolver stays Anonymous),
   and `netidx_tls_resolver: "resolver.range.fowl"` on the range instance.
4. Restart the resolver task on the range PC (**FowlRange netidx
   resolver**), restart the range mission, restart bfdb from the bot.
5. Check (section 6). If it is wrong, roll back (section 5).

### Campaign box

Lower priority: its resolver only listens on loopback, so TLS there guards
against other local accounts and processes, not the network.

1. Copy the engine pair to the DCS account's folder (`C:\fowl\tls-dcs`)
   and `resolver.campaign.fowl.*` beside bfdb's.
2. Add `netidx_tls` (with `resolver_name: "resolver.campaign.fowl"`) to
   every campaign server's `<sortie>_CFG`.
3. Add `resolver` and `grant` to `bfdb.netidx_tls`.
4. Stop the running resolver: procman leaves a resolver that is already
   listening alone, so it would keep the Anonymous one
   (`Stop-Process -Name netidx`, or end `netidx.exe` in Task Manager).
   Then restart bfdb from the bot (it starts the TLS resolver) and restart
   each campaign mission so the engines reconnect with TLS.

## 5. Rollback

Undo the same set together:

* Range: put back the Anonymous `resolver.json` (re-running
  `setup-range-pc.ps1` does it), remove `netidx_tls` from `<sortie>_RANGE`,
  remove `netidx_tls_resolver` from the instance; restart the resolver task,
  the mission and bfdb.
* Campaign: remove `resolver` (or the whole `bfdb.netidx_tls` block) and
  the engines' `netidx_tls`; stop the TLS resolver; restart bfdb (procman
  starts the Anonymous `netidx-resolver.json` again) and the missions.

Nothing else is stored anywhere: removing the keys is the whole rollback.

## 6. Checking it

* Engine logs: `netidx: TLS to resolver <name> as <certificate>` once per
  mission load, then the usual `netidx: publisher bound to ...` /
  `netidx: publishing on ...`. A certificate problem shows up as `failed to
  load netidx config` / `failed to load the netidx config from ...` with
  netidx's reason (a file that can't be read, "certificate has no common
  name", "exactly 1 subjectAltName").
* bfdb log: `netidx: <which config> uses TLS to resolver ...`.
* `/api/health` and the dashboard's live views (map, engine log) for the
  campaign; the range site's live data and a spawn from it for the range.
* An Anonymous `netidx resolver list /local` against the resolver is
  refused: proof the old way in is closed.

## 7. Renewal

The recipe issues one-year certificates; put a calendar reminder a month
before. Re-issue with the same names (the loop in section 1, skipping the
CA), copy the new `.crt`/`.key` over the old ones, then restart the
resolver, the missions and bfdb: netidx reads the files when it connects
and caches the result for the process. The CA is good for ten years.

## Open questions

* **The DCS account can read the engine's key.** bflib and bfrange run
  inside DCS, as the DCS account, so the key file has to be readable by
  that account, and so can every Lua mod and DLL loaded into the same DCS
  (an encrypted key does not help: its password comes from the same
  account's Credential Manager or an `askpass` that account can run). TLS
  therefore protects the network hop and the box from other accounts and
  other processes that are not DCS. It does not protect the engine's
  identity from code running inside DCS. Narrow what that identity can do
  with `perms` (an engine needs to publish its base, not call bfdb's RPCs)
  if that matters, and run DCS under its own account rather than the bot's
  so at least bfdb's and the resolver's keys are out of reach.
* Certificate lifetime: short-lived certificates would need automation
  this repo does not have; one-year ones need the reminder above.
* `setup-range-pc.ps1` / `setup-main-pc.ps1` do not have a `-TlsDir` yet,
  so the range PC's resolver config is a hand edit that a re-run of the
  script undoes.
