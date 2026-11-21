# Controlling the server boxes from your own PC (Fowl Engine Manager over Tailscale)

Fowl Engine Manager can run on a PC that is not a server -- yours -- and
control every server box from there: the campaign box and the standalone
range box. Per box you get:

- **ALL SERVERS**: every box side by side -- DCS up/down and players, bfdb,
  bot version, engine builds, update state, open issues;
- **DCS CONTROL**: start, stop and restart DCS, and restart the mission, per
  DCS instance;
- **SERVER OPS**: the dashboard's OPS page -- restart bfdb or the bot, engine
  updates and rollback, campaign packs, fowlengine.yaml editing, the issue list
  and the log archive;
- **LIVE LOGS**: the bot, bfdb, netidx, each instance's engine log
  (`bfnext.txt`, `bfrange.txt` on the range) and `dcs.log`, followed live;
- **BACKUP**: start a whole-box backup, list the backups, check one.
  **Restore is never remote**: it overwrites the box, so it is only done in
  the Manager on the box itself.

Every destructive action names the box it goes to in large type, so you
can't restart the wrong one.

**Nothing changes on a box until you follow this page.** By default the
bot's WebService (which carries the OPS API) listens on 127.0.0.1 only, and
the Manager on a box only talks to its own bot.

## How it fits together

```
 your PC                                   each server box
 Fowl Engine Manager ── Tailscale ──►  DCSServerBot WebService :9876 ──► fowlengine plugin (OPS API)
 (controller only:      (WireGuard,     0.0.0.0, but Windows Firewall:      │ DCS: Server.startup/shutdown/restart
  no bot here)           encrypted)     only 100.64.0.0/10 gets in          │ logs: read by id, secrets redacted
                                        + X-API-Key = ops_api.api_key       └ backups: a request file ──► the box's
                                                                              Manager service (LocalSystem) runs it
```

- The OPS API is plain HTTP with the key in a header. It must only ever be
  reached through the tailnet's encrypted tunnel: the Manager refuses any
  address that is not a Tailscale one (100.64.0.0 - 100.127.255.255,
  fd7a:115c:a1e0::/48, or a `*.ts.net` MagicDNS name).
- Each box's key is stored on your PC DPAPI-encrypted in
  `%ProgramData%\FowlEngine\manager.json` and never reaches the Manager's
  window; the Rust side adds it to each request.
- bfdb and the box's own Manager keep calling the WebService on 127.0.0.1;
  loopback is not affected by the firewall rules below.

> **Which address?** A box's Tailscale address is what `tailscale ip -4`
> prints on it, always 100.64.x.x - 100.127.x.x. The range PC's
> **100.17.2.86** (and the campaign PC's 100.17.2.84) are their *public*
> addresses, not Tailscale ones -- the Manager refuses them on purpose.

## Rollout order

The new remote-control routes live in the fowlengine bot plugin, and the
remote-backup bridge in the Manager service. A box with an older plugin
answers 404 for them; the controller then says "update the fowlengine plugin
on <box>" instead of failing.

1. On **each server box**, install the Fowl Engine Manager release that has
   this feature (its auto-update installs it once it is published on the
   box's channel, or install it by hand). The Manager syncs its bundled plugin into DCSServerBot before every
   bot start; OVERVIEW → Fowl Engine bot plugin → **Sync now** does it at
   once (DCS keeps running). An engine release can carry the plugin too.
2. Open each box to the tailnet (below).
3. Install the Manager on your PC and add the boxes.

## 1. Tailscale on every PC

Install [Tailscale](https://tailscale.com/download/windows) on both server
boxes and on your PC, signed in to the same tailnet. On a server box, in
Tailscale's settings, turn on **Run unattended** so it connects at boot
before anyone signs in, and consider **disabling key expiry** for the box in
the admin console (a box whose key expired drops off the tailnet until
someone signs it in again).

## 2. Open each server box (once per box, elevated PowerShell)

The helper does steps 2a-2c and tells you what is left:

```powershell
.\deploy\manager-remote-setup.ps1 -RestartBot
```

It reads DCSServerBot's folder from the box's Manager, adds the firewall
rules, switches the WebService to `listen: 0.0.0.0` (keeping a copy of the
old file), checks the OPS key settings and prints the address to type into
your PC's Manager. `-Undo` removes the rules and puts `listen: 127.0.0.1`
back. By hand, it is:

### 2a. Firewall first

```powershell
$port = 9876   # DEFAULT: port: in config\services\webservice.yaml
New-NetFirewallRule -DisplayName 'Fowl Engine OPS - allow tailnet' -Group 'Fowl Engine OPS (Tailscale)' `
  -Direction Inbound -Action Allow -Protocol TCP -LocalPort $port -RemoteAddress 100.64.0.0/10 `
  -InterfaceAlias Tailscale -Profile Any
New-NetFirewallRule -DisplayName 'Fowl Engine OPS - block everything else' -Group 'Fowl Engine OPS (Tailscale)' `
  -Direction Inbound -Action Block -Protocol TCP -LocalPort $port `
  -RemoteAddress 0.0.0.0-100.63.255.255,100.128.0.0-126.255.255.255,128.0.0.0-255.255.255.255 -Profile Any
```

The block rule matters: Windows offers to create an "allow python.exe on
all ports" rule the first time the bot listens on every interface, and a
block rule always beats an allow rule. Loopback (127/8) is left out of it.

### 2b. Let the WebService listen on every interface

`config\services\webservice.yaml`:

```yaml
DEFAULT:
  listen: 0.0.0.0      # was 127.0.0.1; the firewall rules decide who gets in
  port: 9876
```

DCSServerBot's WebService binds one address, so it is either loopback only
or every interface; binding it to the Tailscale address alone would break
bfdb and the box's own Manager, which call 127.0.0.1. `0.0.0.0` is IPv4
only. Restart the bot for it to take effect (Manager → Restart bot; DCS keeps
running).

The same WebService also carries the RestAPI plugin's routes, if you use
them; they become reachable from the tailnet too, behind their own key.

### 2c. Give the OPS API its own key, and only that key

`config\plugins\fowlengine.yaml`:

```yaml
DEFAULT:
  ops_api:
    api_key: "<a long random value, 32+ characters>"
    require_own_key: true      # refuse the RestAPI key (bfdb.dcsserverbot_api_key)
```

The box's Manager does the key for you: BOT CONFIG → checks → *OPS API key*
→ **Generate & set**. Add `require_own_key: true` in the editor. bfdb gets
the key from the plugin (it restarts with the bot). Use a different key on
each box.
Once both are set, BOT CONFIG shows the open WebService as *info* instead of
a warning.

### 2d. Remote backups (optional)

On the box: Manager → SETTINGS → tick **Let a controller PC start, list and
check backups of this box**, and make sure the desktop user the bot runs as
is set (Setup step 3). The Manager service then

- takes requests the plugin drops in `%ProgramData%\FowlEngine\remote\requests`
  (the bot's user gets Modify on `remote\` only; the rest of the folder stays
  Administrators + SYSTEM);
- runs a backup exactly as the box's own BACKUP tab would with nothing
  changed -- its usual folders, the bot's database, into its usual backup
  folder -- from a shadow copy, without stopping anything (the bot answering
  is how your PC watches it); an optional password encrypts the zip;
- checks only zips it found itself, and never restores.

The zip stays on the box: copy it off the box yourself.

## 3. Your PC

1. Install Fowl Engine Manager. In **Setup**, choose **This PC only controls
   other servers** (or SETTINGS → *This PC only controls other server
   boxes*): Setup then stops asking for a DCSServerBot folder or a service.
2. **ALL SERVERS → Add a server**, for each box:
   - a name you can't mix up (CAMPAIGN, RANGE) -- it is on every confirm;
   - the OPS address the helper printed, e.g.
     `http://100.101.2.86:9876/stats/fowlengine/ops` (or just
     `100.101.2.86:9876/stats`: the Manager adds the rest). The box's own
     Manager shows its OPS address at the bottom of its window with
     127.0.0.1 in it -- swap in the Tailscale address;
   - that box's `ops_api.api_key`.
3. **Test connection**: it reaches the box, checks the key, and says whether
   the box's plugin already has the remote-control routes.
4. Pick a box at the top. Every tab in the upper group acts on it.

A Manager on a server box can control the other box the same way: it
always lists its own bot as the first server, and you add the other box.

## 4. Tailscale policy: only your PC may reach those ports

With no policy, every device in a tailnet can reach every other. Tag the
servers and allow the WebService port from your devices only. In the admin
console → Access controls:

```jsonc
{
  "tagOwners": {
    "tag:fowl-server": ["autogroup:admin"]
  },
  "acls": [
    // you (all your devices) may reach the boxes' OPS API
    { "action": "accept", "src": ["you@example.com"], "dst": ["tag:fowl-server:9876"] },
    // ...keep whatever else your tailnet needs here; anything not accepted is denied
  ]
}
```

Then on each server box: `tailscale up --advertise-tags=tag:fowl-server`
(or tag it in the admin console). If you share the tailnet with other
people, narrow `src` to your controller PC (a tag such as
`tag:fowl-controller`, or its 100.x address). The firewall rule from step 2
accepts the whole 100.64.0.0/10 range; this policy is what limits it to you.

## What could not be tested without the real boxes

Everything was built and tested against fakes: the plugin's routes against
a mocked DCSServerBot (`DCSServerBot/tests/test_fowlengine_ops.py`), the
Manager's registry, URL rules, DPAPI storage and remote-backup bridge in its
Rust tests, and the whole UI against `npm run dev:manager` with
`http://localhost:5190/?mock` (two fake boxes; `&controller` for a PC with
no bot, `&oldrange` for a box with an old plugin). Before relying on it,
check on the real boxes:

- `Test connection` from your PC to each box (Tailscale, firewall, key);
- a mission restart and a DCS restart on an empty server
  (`Server.restart` / `startup` / `shutdown` on the DCSServerBot version you run);
- LIVE LOGS across a DCS restart (dcs.log is replaced at start: you should
  see "the log was rotated");
- a remote backup with **Remote backups** on, then *Check* on the zip.

## Turning it off

On the box: `.\deploy\manager-remote-setup.ps1 -Undo -RestartBot`, untick
Remote backups in its Manager, and remove the box from your PC's list (its
saved key is deleted).
