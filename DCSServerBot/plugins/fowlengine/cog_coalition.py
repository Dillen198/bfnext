"""CoalitionMixin: part of the FowlEngine cog (commands.py).

Per-coalition briefing channels, /fe_briefing, and the Discord roles mirrored
from the engine's own registrations (coalition and commander).

Moved out of commands.py unchanged. A mixin: it holds no state of its own and
relies on what FowlEngine.__init__ sets up; discord.py collects its commands,
groups, loops and listeners through the MRO."""
import functools
import discord
from core import utils, Server, Status, command
from discord import app_commands
from discord.ext import tasks
from .briefing import build_briefing_embed
from .icons import icon
from .common import (BRIEFING_HTTP_TIMEOUT, BRIEFING_UPDATE_MINUTES, BriefingUnavailable, COALITION_SYNC_MINUTES,
                     RANGE_ONLY_MSG, REVOKE_MAX_FRACTION, REVOKE_MIN_PER_TICK, bfdb_admin_get, bfdb_get_cached,
                     srv_path)


class CoalitionMixin:
        
    # ── Per-coalition briefing channels ─────────────────────────────────────
    #
    # Each DCS server gets two channels, one per coalition, each holding a
    # single embed that is edited in place from bfdb's GET /api/situation.
    #
    # WHY THIS IS NOT CHEATABLE. The report bfdb hands back is built by the
    # engine *for one side* and only contains what that side has earned --
    # threat rings from its own recon/ELINT, air tracks from its own radar net.
    # The bot never merges them. Read access to each channel is gated by a
    # coalition role, and `sync_coalition_roles` below only ever mirrors the
    # side the ENGINE registered (first slot pick, or a `-switch`), which it
    # reads from /api/admin/pilot-sides. Nothing a player can do in Discord
    # puts them in the other faction's channel; they have to actually go and
    # fly for that faction, and switching costs them a side switch in game.

    def _briefing_channels(self, config: dict) -> dict:
        """{"Blue": channel id, "Red": channel id} for one server, unset keys
        dropped. A server may configure one side only (or neither).

        A malformed id is dropped with a warning rather than raised: this is
        called from a `tasks.loop`, where an unhandled exception stops the loop
        for every server, not just the misconfigured one.
        """
        out = {}
        for side, key in (("Blue", "blue_briefing_channel"), ("Red", "red_briefing_channel")):
            cid = config.get(key)
            if not cid:
                continue
            try:
                out[side] = int(cid)
            except (TypeError, ValueError):
                self.log.warning(f"FowlEngine: {key} is not a channel id: {cid!r}")
        return out

    async def _fetch_situation(self, api_url: str, server_name: str, side: str,
                               username: str, password: str):
        """GET /api/situation for one side of one instance, as the bfdb admin.

        The endpoint is coalition-locked: it resolves the caller's own side and
        refuses to hand over anyone else's. A bfdb admin with no in-game
        registration of their own is the one caller allowed to name a side via
        `?side=`, which is exactly what the bot is -- so the *bot's* access is
        privileged while every player's is not, and the fog of war is re-imposed
        by which channel the embed is posted to.
        """
        path = srv_path(f"/api/situation?side={side.lower()}", server_name)
        status, data = await bfdb_get_cached(api_url, username, password, path,
                                             timeout=BRIEFING_HTTP_TIMEOUT)
        if status != 200 or data is None:
            # bfdb says why in {"error": ...}; this used to be dropped, so two
            # days of "HTTP 503" (Sept 28-30) never said which of engine down,
            # no sortie yet or RPC breaker open it was.
            why = data.get("error") if isinstance(data, dict) else None
            detail = f"/api/situation {side} -> HTTP {status}" + (f": {why}" if why else "")
            if status == 503:
                raise BriefingUnavailable(detail)
            raise RuntimeError(detail)
        return data

    @tasks.loop(minutes=BRIEFING_UPDATE_MINUTES)
    async def update_briefings(self):
        # One server's bad config or unreachable bfdb must not stop the others
        # -- and an exception that escapes a tasks.loop stops the loop for good.
        for server in self.bot.servers.values():
            try:
                if server.status not in [Status.RUNNING, Status.PAUSED]:
                    continue
                config = self.get_config(server) or {}
                channels = self._briefing_channels(config)
                if not channels:
                    continue
                if self._is_range(server):
                    continue  # no coalition situation report on a range
                api_url = config.get("api_url", "http://localhost:8880")
                username = config.get("admin_username", "")
                password = config.get("admin_password", "")
                if not username or not password:
                    self.log.warning(
                        f"FowlEngine: {server.name} has briefing channels configured but no "
                        f"admin_username/admin_password -- /api/situation is coalition-locked "
                        f"and cannot be read without them.")
                    continue
                for side, channel_id in channels.items():
                    key = (server.name, side)
                    try:
                        await self._update_one_briefing(server, config, api_url, username,
                                                        password, side, channel_id)
                        if self._briefing_down.pop(key, None) is not None:
                            self.log.info(f"FowlEngine: {side} briefing for {server.name} is updating again")
                    except BriefingUnavailable as ex:
                        # The engine isn't answering. That was an ERROR every
                        # tick for as long as it lasted (127 in two days);
                        # say it once per outage, and again if the reason
                        # changes.
                        if self._briefing_down.get(key) != str(ex):
                            self._briefing_down[key] = str(ex)
                            self.log.warning(f"FowlEngine: {side} briefing for {server.name} paused, "
                                             f"engine not answering: {ex}")
                    except Exception as ex:
                        self.log.error(f"FowlEngine: {side} briefing for {server.name}: {ex}")
            except Exception as ex:
                self.log.error(f"FowlEngine: briefing tick for {server.name}: {ex}")

    async def _update_one_briefing(self, server, config, api_url, username, password,
                                   side: str, channel_id: int):
        channel = self.bot.get_channel(channel_id)
        if not channel:
            # every 3 minutes forever otherwise
            self._warn_once(server.name, f"{side.lower()}_briefing_channel:{channel_id}",
                            f"FowlEngine: {side.lower()}_briefing_channel {channel_id} for {server.name} doesn't "
                            f"exist or the bot can't see it (wrong id, or no View Channel permission) -- fix the id "
                            f"in fowlengine.yaml or set it to null.")
            return
        report = await self._fetch_situation(api_url, server.name, side, username, password)
        embed = build_briefing_embed(
            report,
            icons=self.icons,
            embed_factory=functools.partial(self._vs_embed, server=server),
            dashboard_url=config.get("dashboard_url") or "",
            # Only label the instance when there is more than one, otherwise
            # every title carries a server name nobody needs.
            instance_label=server.name if len(self.bot.servers) > 1 else "",
        )
        per_server = self.briefing_msg_ids.setdefault(server.name, {})
        await self._upsert_embed(per_server, side, channel, embed, f"{side} briefing")

    @update_briefings.before_loop
    async def before_update_briefings(self):
        await self.bot.wait_until_ready()
        # Guild emoji are only readable once the bot is ready, so the icon set
        # resolves here rather than in cog_load.
        try:
            await self.icons.refresh()
        except Exception as ex:
            self.log.debug(f"FowlEngine: icon refresh skipped: {ex}")

    # ── Coalition roles, mirrored from the engine ───────────────────────────

    def _coalition_roles_cfg(self, config: dict) -> dict:
        cfg = config.get("coalition_roles") or {}
        return cfg if cfg.get("manage") else {}

    @staticmethod
    def _resolve_role(guild: discord.Guild, spec):
        """A role by id or by exact name. Ids are preferred -- a renamed role
        keeps working."""
        if spec is None:
            return None
        if isinstance(spec, int) or (isinstance(spec, str) and spec.isdigit()):
            return guild.get_role(int(spec))
        return discord.utils.get(guild.roles, name=str(spec))

    @tasks.loop(minutes=COALITION_SYNC_MINUTES)
    async def sync_coalition_roles(self):
        """Make the Discord coalition roles say what the engine says.

        One direction only: the engine decides a side and this reflects it. The
        bot never writes a side back, so a Discord role is evidence of a
        registration rather than a way to obtain one -- which is what makes the
        briefing channels honest.

        Servers are synced per ROLE PAIR, not per server. Two servers that
        share a Blue/Red pair used to be synced one after the other against
        their own rosters, so each revoked what the other had just granted and
        the roles flipped every pass. That was masked while bfdb filled every
        instance's roster with pilots from the others; once each roster became
        that instance's own, the 2008 pass tried to strip the Modern pilots.
        Now a pair's rosters are merged first: a member keeps a role if ANY
        server using it backs them.
        """
        # The coalition pair mirrors registrations; the one Commander role
        # (`commander`) mirrors who bfdb says commands -- rank or an admin's
        # grant -- on any server. Same machinery, so the same guards.
        for kind in ROLE_KINDS:
            await self._sync_role_kind(kind)

    async def _sync_role_kind(self, kind: dict):
        groups: dict = {}
        for server in self.bot.servers.values():
            try:
                config = self.get_config(server) or {}
                cr = self._coalition_roles_cfg(config)
                if not cr:
                    continue
                if self._is_range(server):
                    # The range registers no coalition. Syncing against its
                    # (empty) roster would only ever revoke -- and a range
                    # that inherited DEFAULT's coalition_roles would strip
                    # the campaign's roles.
                    self._warn_once(server.name, 'coalition_roles',
                                    f"FowlEngine: {server.name} is a training range -- "
                                    f"ignoring coalition_roles (set it to null in its section)")
                    continue
                roles = self._coalition_role_pair(server, cr, kind["keys"])
                if not roles:
                    continue
                key = tuple(sorted((side, r.id) for side, r in roles.items()))
                g = groups.setdefault(key, {"roles": roles, "cr": cr, "servers": [],
                                            "sides": {}, "complete": True})
                g["servers"].append(server.name)
                sides = await kind["fetch"](self, server, config)
                if sides is None:
                    # One unreadable roster makes revoking for the whole pair
                    # unsafe: its pilots would look unregistered.
                    g["complete"] = False
                    continue
                for ucid, side in sides.items():
                    g["sides"].setdefault(ucid, set()).add(side)
            except Exception as ex:
                self.log.error(f"FowlEngine: {kind['label']} role sync for {server.name}: {ex}")
        for g in groups.values():
            if len(g["servers"]) > 1 and not kind.get("shared"):
                self._warn_once(
                    ",".join(g["servers"]), f'{kind["label"]}_roles_shared',
                    f"FowlEngine: {', '.join(g['servers'])} share one pair of {kind['label']} roles; "
                    f"syncing them as one (a pilot keeps a role if any of them backs it). "
                    f"Give each server its own pair if the roles should be per server.")
            try:
                await self._apply_coalition_roles(g, kind)
            except Exception as ex:
                self.log.error(f"FowlEngine: {kind['label']} role sync for "
                               f"{', '.join(g['servers'])}: {ex}")

    def _coalition_role_pair(self, server, cr: dict, keys=(("Blue", "blue"), ("Red", "red"))) -> dict:
        roles = {}
        for side, key in keys:
            spec = cr.get(key)
            if not spec:
                continue
            for g in self.bot.guilds:
                role = self._resolve_role(g, spec)
                if role:
                    roles[side] = role
                    break
            else:
                self.log.warning(f"FowlEngine: role {spec!r} ({key}) not found "
                                 f"in any guild -- sync for {server.name} is incomplete.")
        return roles

    async def _fetch_pilot_sides(self, server, config: dict):
        """ucid -> "Blue"/"Red" from the engine's registrations for this
        server, or None when bfdb could not give a usable answer."""
        api_url = config.get("api_url", "http://localhost:8880")
        username = config.get("admin_username", "")
        password = config.get("admin_password", "")
        if not username or not password:
            self.log.warning(
                f"FowlEngine: coalition_roles.manage is on for {server.name} but no "
                f"admin_username/admin_password is set -- /api/admin/pilot-sides needs them.")
            return None
        status, data = await bfdb_get_cached(
            api_url, username, password,
            srv_path("/api/admin/pilot-sides", server.name))
        if status != 200 or not data:
            self.log.warning(f"FowlEngine: /api/admin/pilot-sides -> {status} "
                             f"for {server.name}")
            return None
        return {p["ucid"]: p["side"] for p in (data.get("pilots") or [])
                if p.get("ucid") and p.get("side") in ("Blue", "Red")}

    async def _fetch_commander_sides(self, server, config: dict):
        """ucid -> "Blue"/"Red" for this server's commanders (earned by rank
        or granted by an admin, as bfdb works it out), or None when bfdb could
        not give a usable answer."""
        api_url = config.get("api_url", "http://localhost:8880")
        username = config.get("admin_username", "")
        password = config.get("admin_password", "")
        if not username or not password:
            return None
        status, data = await bfdb_get_cached(
            api_url, username, password,
            srv_path("/api/admin/commanders", server.name))
        if status == 404:
            # A bfdb from before commander access: nothing to mirror yet.
            self._warn_once(server.name, 'commander_roles_404',
                            f"FowlEngine: {server.name}'s bfdb has no /api/admin/commanders "
                            f"yet -- commander roles wait for a bfdb update.")
            return None
        if status != 200 or not isinstance(data, dict):
            self.log.warning(f"FowlEngine: /api/admin/commanders -> {status} for {server.name}")
            return None
        # One role for every commander, whatever their side: the coalition
        # role already says which side that is, server by server.
        return {p["ucid"]: "Commander" for p in (data.get("pilots") or [])
                if p.get("commander") and p.get("ucid") and p.get("side") in ("Blue", "Red")}

    async def _apply_coalition_roles(self, g: dict, kind: dict = None):
        kind = kind or ROLE_KINDS[0]
        label = kind["label"]
        roles, cr, sides = g["roles"], g["cr"], g["sides"]
        name = ", ".join(g["servers"])
        # Discord member id -> (member, sides the engine says they fly on any
        # server using this pair). Built from the engine's side lists, not from
        # who currently holds a role, so a role someone was given by hand is
        # reconciled away below.
        want = {}
        for ucid, member_sides in sides.items():
            member_sides = {s for s in member_sides if s in roles}
            if not member_sides:
                continue
            try:
                member = await self.bot.get_member_by_ucid(ucid)
            except Exception:
                member = None
            if member:
                entry = want.setdefault(member.id, (member, set()))
                entry[1].update(member_sides)

        granted = revoked = 0
        for member, member_sides in want.values():
            for side in member_sides:
                target = roles[side]
                try:
                    if target not in member.roles:
                        await member.add_roles(target, reason=f"Fowl Engine: {kind['granted']}")
                        granted += 1
                except discord.Forbidden:
                    self.log.error(
                        f"FowlEngine: cannot manage {target.name} -- the bot's own role must sit "
                        f"ABOVE the {label} roles in the guild's role list, and it needs "
                        f"Manage Roles.")
                    return
                except Exception as ex:
                    self.log.debug(f"FowlEngine: role update for {member} failed: {ex}")
            # Switched sides: drop the other role now, whatever
            # revoke_when_unregistered says -- as before the pairs were merged.
            if g["complete"] and len(member_sides) == 1:
                other = roles.get("Red" if "Blue" in member_sides else "Blue")
                if other is not None and other in member.roles:
                    try:
                        await member.remove_roles(other, reason=f"Fowl Engine: {label}, switched coalition")
                        revoked += 1
                    except Exception as ex:
                        self.log.debug(f"FowlEngine: role update for {member} failed: {ex}")

        # Reconcile the other way: anyone holding a coalition role the engine
        # does not back loses it -- which also covers switching sides. This is
        # the half that actually closes the hole: without it, a role handed out
        # by an admin (or left over from a previous campaign) is a permanent
        # key to that side's briefing.
        #
        # Both guards below exist because this pass can strip roles en masse
        # and there is no undo. An EMPTY or unreadable roster means "bfdb has
        # no data right now" -- a campaign reset, a rebuilt database, a round
        # that has not started -- not "nobody is registered", and acting on it
        # would clear the coalition roles for the whole guild. The cap covers
        # the partial version of the same failure.
        # An empty coalition roster means bfdb has no data; an empty commander
        # roster that bfdb answered for is real (nobody commands yet).
        if cr.get("revoke_when_unregistered", True) and (sides or kind["empty_ok"]) and g["complete"]:
            holders = sum(len(r.members) for r in roles.values())
            cap = max(REVOKE_MIN_PER_TICK, int(holders * REVOKE_MAX_FRACTION))
            hit_cap = False
            for side, role in roles.items():
                for member in list(role.members):
                    entry = want.get(member.id)
                    if entry and side in entry[1]:
                        continue
                    if revoked >= cap:
                        hit_cap = True
                        break
                    try:
                        await member.remove_roles(
                            role, reason=f"Fowl Engine: {kind['revoked']}")
                        revoked += 1
                    except discord.Forbidden:
                        self.log.error(f"FowlEngine: cannot remove {role.name} from {member}")
                        break
                    except Exception as ex:
                        self.log.debug(f"FowlEngine: role revoke for {member} failed: {ex}")
                if hit_cap:
                    break
            if hit_cap:
                self.log.error(
                    f"FowlEngine: {label} role sync for {name} wanted to revoke more "
                    f"than {cap} of {holders} role holders in one pass and stopped. That is "
                    f"almost always bfdb answering with a stale or partial roster, not that "
                    f"many people genuinely losing the role -- check {kind['endpoint']} "
                    f"before assuming the roles are wrong.")
        elif cr.get("revoke_when_unregistered", True):
            self.log.warning(
                f"FowlEngine: no complete {kind['endpoint']} roster for {name}; skipping "
                f"revocation rather than stripping {label} roles.")
        if granted or revoked:
            self.log.info(f"FowlEngine: {label} roles for {name} -- "
                          f"{granted} granted, {revoked} revoked")

    @sync_coalition_roles.before_loop
    async def before_sync_coalition_roles(self):
        await self.bot.wait_until_ready()

    async def _ucid_for_member(self, member, pilots: list):
        """This Discord member's UCID, or None.

        Prefers the bot's own reverse lookup where the running DCSServerBot has
        one; otherwise walks the roster through `get_member_by_ucid`, which is
        the lookup this plugin already relies on elsewhere. `pilots` bounds that
        walk to people who actually have a registration.
        """
        lookup = getattr(self.bot, "get_ucid_by_member", None)
        if lookup:
            try:
                ucid = await lookup(member)
                if ucid:
                    return ucid
            except Exception as ex:
                self.log.debug(f"FowlEngine: get_ucid_by_member failed: {ex}")
        for p in pilots:
            u = p.get("ucid")
            if not u:
                continue
            try:
                m = await self.bot.get_member_by_ucid(u)
            except Exception:
                continue
            if m and m.id == member.id:
                return u
        return None

    @command(description='Your coalition briefing: posture, tasking, threats, comms.')
    @app_commands.guild_only()
    @utils.app_has_role('DCS')
    async def fe_briefing(self, interaction: discord.Interaction,
                          server: app_commands.Transform[Server, utils.ServerTransformer()]):
        """The same report the channel embed carries, on demand and private.

        The side is resolved from the caller's own in-game registration, never
        from anything they pass -- so this cannot be used to read the other
        faction's picture even by someone who can see both channels.
        """
        await interaction.response.defer(ephemeral=True)
        if self._is_range(server):
            await interaction.followup.send(RANGE_ONLY_MSG.format(name=server.name))
            return
        config = self.get_config(server) or {}
        api_url = config.get("api_url", "http://localhost:8880")
        username = config.get("admin_username", "")
        password = config.get("admin_password", "")
        if not username or not password:
            await interaction.followup.send(
                f"{icon('bad')} admin_username/admin_password must be set in fowlengine.yaml for this.")
            return
        status, data = await bfdb_admin_get(
            api_url, username, password,
            srv_path("/api/admin/pilot-sides", server.name))
        if status != 200 or not data:
            await interaction.followup.send(f"{icon('bad')} bfdb did not answer (HTTP {status}).")
            return
        pilots = data.get("pilots") or []
        ucid = await self._ucid_for_member(interaction.user, pilots)
        side = next((p["side"] for p in pilots if p.get("ucid") == ucid), None)
        if side not in ("Blue", "Red"):
            await interaction.followup.send(
                f"{icon('bad')} You have no coalition on this server yet. Take a slot in game — your "
                "first slot pick registers you — and try again.")
            return
        try:
            report = await self._fetch_situation(api_url, server.name, side, username, password)
        except Exception as ex:
            await interaction.followup.send(f"Error: {ex}")
            return
        embed = build_briefing_embed(
            report, icons=self.icons, embed_factory=functools.partial(self._vs_embed, server=server),
            dashboard_url=config.get("dashboard_url") or "",
            instance_label=server.name if len(self.bot.servers) > 1 else "")
        await interaction.followup.send(embed=embed)



# The Discord roles the plugin mirrors from bfdb: the coalition each pilot is
# registered to (a Blue/Red pair per server), and -- when `commander` is
# configured -- a Commander role for whoever commands on that server (one role
# per server is the recommended setup; servers that name the same role are
# synced as one, so it can also be a single Discord-wide role). Which side a
# commander commands stays the coalition role's business: it follows a side
# switch on its own.
ROLE_KINDS = [
    {"label": "coalition", "keys": (("Blue", "blue"), ("Red", "red")),
     "fetch": CoalitionMixin._fetch_pilot_sides, "endpoint": "/api/admin/pilot-sides",
     "granted": "registered coalition", "revoked": "no matching coalition registration",
     "empty_ok": False},
    {"label": "commander", "keys": (("Commander", "commander"),),
     "fetch": CoalitionMixin._fetch_commander_sides, "endpoint": "/api/admin/commanders",
     "granted": "commander", "revoked": "no longer a commander on any server",
     "empty_ok": True, "shared": True},
]
