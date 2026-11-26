# Release signing keys: passwords, offline backup, rotation

Three `tauri signer` (minisign, Ed25519) keys sign everything a server or a
player's PC installs by itself. They are separate on purpose: a leak of one
can't ship the others' updates.

| Key (secret file) | Signs | Trusted by (public half) |
|---|---|---|
| `%USERPROFILE%\.tauri\fowl-engine.key` | engine releases: `manifest.json.sig` (bflib, bfrange, bfdb, bftools, campaign packs, bot plugin) | every bot's `autoupdate.public_key` in `fowlengine.yaml`; `bfmanager/src-tauri/engine.pub` (New box wizard) |
| `%USERPROFILE%\.tauri\fowl-manager.key` | Fowl Engine Manager installers (`.sig`) | `bfmanager/src-tauri/updater.pub`, compiled into every installed Manager |
| `%USERPROFILE%\.tauri\fowl-cockpit.key` | cockpit plugin updates (`.minisig`) | `bfcockpit/net/updater.pub`, compiled into every installed `bfcockpit_net.dll` |

The engine key in use was made on 2026-10-18 (key id `44D2C7BA11CC8E7D`)
**without a password**, and no offline copy is documented. Until the steps
below are done, anyone who gets that file from the dev PC can sign an
engine that every server will install. The same check applies to the
Manager key.

None of this is automated, and no tool in this repo handles a secret key
for you. Every step below is done by hand on the PC that holds the key.

## 1. Put a password on a key

The password only protects the key file while it is stored. Signing needs
it (`FOWL_ENGINE_SIGNING_PASSWORD`, `FOWL_SIGNING_PASSWORD`,
`FOWL_COCKPIT_SIGNING_PASSWORD`, or the GitHub environment secret of the
same name).

`tauri signer` has no "change password" command. There are two ways:

**A. Make a new key with a password, and rotate (section 3).** This always
works, and it is the right choice whenever the old file may have been
copied somewhere, e.g. it sat unencrypted in a synced or backed-up profile.

**B. Re-encrypt the same key** (keeps the key id, nothing changes on the
servers). A `tauri signer` key file is base64 of a minisign secret key, so
the reference `minisign` CLI (jedisct1, 0.11+) can change its password:

```powershell
# work on a COPY, in a folder that is not synced anywhere
$k = "$env:USERPROFILE\.tauri\fowl-engine.key"
[IO.File]::WriteAllBytes("$PWD\engine.minisign.key", [Convert]::FromBase64String((Get-Content $k -Raw).Trim()))
minisign -C -s .\engine.minisign.key            # asks for the old password (empty) and a new one
[IO.File]::WriteAllText("$PWD\fowl-engine.key.new", [Convert]::ToBase64String([IO.File]::ReadAllBytes("$PWD\engine.minisign.key")))
```

Then prove that the new file still makes signatures the servers accept,
**before** replacing anything:

```powershell
"test" | Set-Content .\probe.txt
npx --prefix bfmanager tauri signer sign -f .\fowl-engine.key.new .\probe.txt   # asks for the new password
# check probe.txt.sig against the public key the servers pin, with the bot's own verifier:
python -c "import sys; sys.path.insert(0, 'DCSServerBot/plugins/fowlengine'); import minisign; minisign.verify(open('probe.txt','rb').read(), open('probe.txt.sig').read(), open('bfmanager/src-tauri/engine.pub').read()); print('OK')"
```

If it prints `OK`, replace the old key file with `fowl-engine.key.new` and
securely delete the copies (`engine.minisign.key`, `probe.*`). If any step
fails, don't experiment on the real file. Use A.

## 2. Keep an offline backup

Losing a key is nearly as bad as leaking it. Nothing installed accepts a
release from a new key until it has been told about it (section 3). For a
lost Manager or cockpit key, that means reinstalling every copy by hand.

For each key, keep **two** offline copies, in different places, plus the
password stored separately from both:

1. **The key file** (`*.key`, password-protected per section 1) on two
   removable media, e.g. two USB sticks, ideally encrypted with BitLocker
   To Go. Keep one off-site. Label the sticks with the key id, not with
   the word "signing".
2. **The password** in a password manager, *not* on the same sticks.
3. **The public half** (`*.key.pub`, and the `RW...` line) next to it. It
   isn't secret, but you need it to check that a restored key is the right
   one (`minisign.verify` as above).
4. **Test the backup once**: sign the probe file with the copy from the
   stick and verify it, as in section 1B.

The key belongs on the release PC and in the GitHub environment secrets
(`engine-release`, `manager-release`, `cockpit-release`, each with required
reviewers, as set up for `release.yml`, `manager-release.yml` and
`cockpit-release.yml`). It must not be in a synced profile folder (OneDrive
"Known folders" moves `%USERPROFILE%` subfolders), in a Fowl Engine
Manager backup zip of the dev PC, or anywhere in this repo. `.gitignore`
blocks `*.key`, but don't rely on that.

Write down for each key where its copies are and when they were last
tested. Keep that note with the password, not in this repo.

## 3. Rotate a key

Rotate when a key may have leaked, when it is lost, when its holder
changes, or to move off a key that ever existed without a password.

### Engine key (servers pin it in `fowlengine.yaml`)

A bot trusts exactly one `autoupdate.public_key`. That key can't be
changed from the OPS page, by design.

1. `npx tauri signer generate -w "$env:USERPROFILE\.tauri\fowl-engine-<yyyymm>.key"`,
   **with a password**. Note the new key id (the OPS page shows it once it
   is pinned, or use `minisign.parse_public_key`).
2. Back it up (section 2).
3. On **every** server, set `autoupdate.public_key` to the new `RW...` line
   in `config/plugins/fowlengine.yaml` and restart the bot. From now on it
   ignores anything signed with the old key, so a leaked old key can no
   longer ship to it.
4. Update `bfmanager/src-tauri/engine.pub` and the key comment in
   `DCSServerBot/plugins/fowlengine/fowlengine.sample.yaml`, and the key id
   in `deploy/auto-update.md`. Commit, and release a Manager so the New box
   wizard pins the new key.
5. Replace the `FOWL_ENGINE_SIGNING_KEY` / `_PASSWORD` secrets on the
   `engine-release` environment.
6. Publish a release with the new key (`publish-release.ps1 -SigningKey <new>`
   or the Action) and check that each server stages it (`/feops update_status`).
7. After a leak: delete the old key's copies, and look through recent
   releases for one you didn't publish (the tag list, and
   `/feops update_status` on each server).

### Manager key and cockpit key (compiled into installed copies)

An installed copy trusts the public key it was built with. So the new key
has to arrive in an update **signed with the old one**:

1. Generate the new key with a password, and back it up.
2. Put the new public key into `bfmanager/src-tauri/updater.pub` (or
   `bfcockpit/net/updater.pub`), bump the version, and release that build
   **signed with the old key**. Every copy that installs it now trusts only
   the new key.
3. Replace the GitHub environment secrets (`manager-release`:
   `TAURI_SIGNING_PRIVATE_KEY` / `FOWL_SIGNING_PASSWORD`; `cockpit-release`:
   `FOWL_COCKPIT_SIGNING_KEY` / `_PASSWORD`) with the new key. All later
   releases are signed with it.
4. Copies that were offline during step 2 can't take any later update until
   they are reinstalled by hand. Wait until most have updated before you
   retire the old key.

If the old key **leaked**, that transition release is the only safe use
left for it: publish it at once, then delete the old key. If the old key is
**lost**, there is no transition. Every installed copy has to be
reinstalled from a new installer.

## Related

- `deploy/auto-update.md` covers how releases are signed and checked.
- `bfcockpit/README.md` ("Releasing") covers the cockpit key's first setup.
- `bfmanager/README.md` covers the Manager key and Authenticode, which still
  needs a certificate.
