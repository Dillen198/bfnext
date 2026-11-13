"""OpsMixin: part of the FowlEngine cog (commands.py).

/feops: server operations for admins -- bfdb, staged binaries, auto-update,
campaign packs, the log analyzer, database tools and the icon set.

Moved out of commands.py unchanged. A mixin: it holds no state of its own and
relies on what FowlEngine.__init__ sets up; discord.py collects its commands,
groups, loops and listeners through the MRO."""
import discord
from core import utils, Server, Status
from discord import app_commands
from datetime import datetime, timezone
import json
import os
import asyncio
import io
from typing import Optional
from .procman import sha256_of, effective_instance_gci
from .upload import ENGINE_DLLS, is_remote_node, BFTOOLS, bftools_path_of
from .icons import icon
from .common import _fit_rows, bfdb_admin_post, srv_path


class OpsMixin:
    # ── engine ops: staged binaries, bfdb control, GCI ──────────────────────

    feops = app_commands.Group(name="feops", description="Fowl Engine server operations (admin)")

    async def _procman_or_warn(self, interaction: discord.Interaction):
        if not self.procman or not self.procman.enabled:
            await interaction.followup.send(
                f"{icon('bad')} `bfdb.manage` is not enabled in fowlengine.yaml -- the bot isn't managing "
                "bfdb/netidx, so there's nothing to control here.")
            return None
        return self.procman

    @feops.command(name="bfdb_restart", description="Restart bfdb (re-renders gci.json, picks up a staged bfdb.exe).")
    @app_commands.guild_only()
    @utils.app_has_role('DCS Admin')
    async def feops_bfdb_restart(self, interaction: discord.Interaction):
        await interaction.response.defer(ephemeral=True)
        pm = await self._procman_or_warn(interaction)
        if not pm:
            return
        cfg = self.get_config() or {}
        pm.reload_config(cfg)
        self._bfdb_admin_password = (cfg.get("bfdb") or {}).get("admin_password") \
            or cfg.get("admin_password", "") or self._bfdb_admin_password
        await pm.restart(self._bfdb_admin_password)
        ok = await pm.health_ok()
        await interaction.followup.send(
            f"{icon('good')} bfdb restarted and answering." if ok else
            f"{icon('warning')} bfdb restarted but not answering yet -- give it a minute, then check `/feops` again.")

    @feops.command(name="gci_show", description="Show the effective gci.json that will be written for bfdb (secrets masked).")
    @app_commands.guild_only()
    @utils.app_has_role('DCS Admin')
    async def feops_gci_show(self, interaction: discord.Interaction):
        await interaction.response.defer(ephemeral=True)
        pm = await self._procman_or_warn(interaction)
        if not pm:
            return
        if pm.multi_instance:
            # One gci.<id>.json per DCS server -- show which ones are on,
            # not a single merged blob that belongs to none of them.
            lines = []
            for inst in pm.instances_cfg:
                merged = effective_instance_gci(pm.gci_cfg, inst)
                iid = inst.get("id", "?")
                if not merged.get("enabled"):
                    lines.append(f"**{iid}** - GCI off")
                    continue
                lines.append(
                    f"**{iid}** - blue `{merged.get('blue_freq_mhz', 251.0)} "
                    f"{merged.get('modulation', 'AM')}` ({merged.get('blue_controller_callsign', 'Magic')}) / "
                    f"red `{merged.get('red_freq_mhz', 252.0)}` "
                    f"({merged.get('red_controller_callsign', 'Overlord')}) "
                    f"via SRS {merged.get('srs_host', '127.0.0.1')}:{merged.get('srs_port', 5002)}"
                )
            await interaction.followup.send("\n".join(lines) or "no instances configured")
            return
        if not pm.gci_enabled():
            await interaction.followup.send("GCI is disabled (`gci.enabled: false`) -- no gci.json is written.")
            return
        body = json.dumps(pm.gci_effective(mask=True), indent=2)
        await interaction.followup.send(f"```json\n{body[:1900]}\n```")

    def _staged_engine_dlls(self, only=None) -> list:
        """[(dll name, staging dir, [server names], pending info)] -- one entry
        per distinct pending engine DLL. Each server's DLL is staged in its own
        BFBinaries staging dir (see upload.py); a pending file in the global
        dir that no server reads any more is listed with no servers."""
        pm = self.procman
        if not pm:
            return []
        seen: dict = {}
        servers = [only] if only is not None else list(self.bot.servers.values())
        for server in servers:
            if self._on_remote_node(server):
                continue  # see _remote_staged
            try:
                b = self._engine_binaries(server)
            except Exception as ex:  # noqa: BLE001
                self.log.debug(f"FowlEngine: {server.name}: engine binaries lookup failed: {ex}")
                continue
            sdir = b["staging_dir"] or pm.staging_dir
            key = (b["dll_name"], os.path.normcase(os.path.normpath(sdir)))
            if key in seen:
                seen[key][2].append(server.name)
                continue
            seen[key] = (b["dll_name"], sdir, [server.name], pm.pending_info(b["dll_name"], sdir))
        if only is None:
            for dll in ENGINE_DLLS:
                key = (dll, os.path.normcase(os.path.normpath(pm.staging_dir)))
                if key not in seen:
                    seen[key] = (dll, pm.staging_dir, [], pm.pending_info(dll))
        return [v for v in seen.values() if v[3]]

    @staticmethod
    def _on_remote_node(server) -> bool:
        """The server runs on a DCSServerBot agent node on another PC."""
        return is_remote_node(getattr(server, "node", None))

    async def _remote_staged(self, only=None) -> list:
        """[(server, dll name, pending path)] for servers on other PCs, asked
        through the node API (their staging dirs are not on this disk)."""
        out = []
        servers = [only] if only is not None else list(self.bot.servers.values())
        for server in servers:
            if not self._on_remote_node(server):
                continue
            try:
                b = self._engine_binaries(server)
            except Exception as ex:  # noqa: BLE001
                self.log.debug(f"FowlEngine: {server.name}: engine binaries lookup failed: {ex}")
                continue
            if not b["staging_dir"]:
                continue
            name = f"{b['dll_name']}.pending"
            try:
                _, files = await server.node.list_directory(b["staging_dir"], pattern=name)
            except Exception as ex:  # noqa: BLE001 - node down / dir missing
                self.log.debug(f"FowlEngine: {server.name}: cannot list {b['staging_dir']}: {ex}")
                continue
            if files:
                out.append((server, b["dll_name"], os.path.join(b["staging_dir"], name)))
        return out

    @staticmethod
    def _fmt_pending(name: str, info: dict, where: str = "") -> str:
        return (f"• `{name}`{where} — {info['size'] / 1024 / 1024:.1f} MB · "
                f"`sha256:{(info.get('sha256') or '')[:12]}` · by {info.get('uploader', '?')} "
                f"at {info.get('utc', '?')}" + (f"\n   notes: {info['notes']}" if info.get('notes') else ""))

    def _staged_bftools(self, only=None) -> list:
        """[(staging dir, [server names], pending info)] for hand-staged
        bftools.exe on this PC, one entry per staging dir."""
        pm = self.procman
        if not pm:
            return []
        seen: dict = {}
        servers = [only] if only is not None else list(self.bot.servers.values())
        for server in servers:
            if self._on_remote_node(server) or not bftools_path_of(self, server):
                continue
            try:
                sdir = self._stage_dir_of(server)
            except Exception:  # noqa: BLE001
                continue
            if not sdir:
                continue
            key = os.path.normcase(os.path.normpath(sdir))
            if key in seen:
                seen[key][1].append(server.name)
                continue
            seen[key] = (sdir, [server.name], pm.pending_info(BFTOOLS, sdir))
        return [v for v in seen.values() if v[2] and v[2].get("source") != "autoupdate"]

    @feops.command(name="stage_status", description="Show any staged bflib.dll / bfrange.dll / bftools.exe / bfdb.exe waiting to be applied.")
    @app_commands.guild_only()
    @utils.app_has_role('DCS Admin')
    async def feops_stage_status(self, interaction: discord.Interaction):
        await interaction.response.defer(ephemeral=True)
        pm = await self._procman_or_warn(interaction)
        if not pm:
            return
        lines = []
        for dll, sdir, names, info in self._staged_engine_dlls():
            where = f" → {', '.join(names)}" if names else f" in `{sdir}` (no server reads this dir)"
            lines.append(self._fmt_pending(dll, info, where))
        for sdir, names, info in self._staged_bftools():
            lines.append(self._fmt_pending(BFTOOLS, info, f" → {', '.join(names)}"))
        for server, dll, path in await self._remote_staged():
            lines.append(f"• `{dll}` → {server.name} (on node `{server.node.name}`: `{path}`)")
        info = pm.pending_info("bfdb.exe")
        if info:
            lines.append(self._fmt_pending("bfdb.exe", info, " → bfdb"))
        if not lines:
            await interaction.followup.send(
                "Nothing staged. Drop `bflib.dll`, `bfrange.dll`, `bftools.exe` or `bfdb.exe` into the admin "
                "channel to stage one.")
            return
        if self.stage_apply_when_empty:
            lines.append(f"\n{icon('pending')} applies when empty: "
                         + ", ".join(f"**{n}** (by {e.get('by', '?')})" for n, e in self.stage_apply_when_empty.items()))
        next_at = ""
        for server in self.bot.servers.values():
            rt = getattr(server, "restart_time", None)
            if rt:
                next_at = f"\n\nNext scheduled restart: <t:{int(rt.timestamp())}:R>"
                break
        await interaction.followup.send(("**Staged engine binaries:**\n" + "\n".join(lines) + next_at)[:1990])

    @feops.command(name="stage_cancel", description="Discard a staged binary.")
    @app_commands.guild_only()
    @utils.app_has_role('DCS Admin')
    @app_commands.choices(which=[
        app_commands.Choice(name="bflib.dll", value="bflib.dll"),
        app_commands.Choice(name="bfrange.dll", value="bfrange.dll"),
        app_commands.Choice(name="bftools.exe", value="bftools.exe"),
        app_commands.Choice(name="bfdb.exe", value="bfdb.exe"),
        app_commands.Choice(name="all", value="all"),
    ])
    async def feops_stage_cancel(self, interaction: discord.Interaction, which: app_commands.Choice[str],
                                 server: Optional[app_commands.Transform[Server, utils.ServerTransformer()]] = None):
        """`server` limits an engine-DLL discard to that server's staging dir;
        without it the DLL is discarded everywhere it is staged."""
        await interaction.response.defer(ephemeral=True)
        pm = await self._procman_or_warn(interaction)
        if not pm:
            return
        removed = []
        dlls = ENGINE_DLLS if which.value == "all" else tuple(d for d in ENGINE_DLLS if d == which.value)
        for dll, sdir, names, _info in self._staged_engine_dlls(only=server):
            if dll in dlls and pm.cancel_pending(dll, sdir):
                removed.append(f"`{dll}` ({', '.join(names) or sdir})")
        for srv, dll, path in await self._remote_staged(only=server):
            if dll not in dlls:
                continue
            try:
                await srv.node.remove_file(path)
                removed.append(f"`{dll}` ({srv.name}, node `{srv.node.name}`)")
            except Exception as ex:  # noqa: BLE001
                self.log.warning(f"FowlEngine: could not discard {path} on {srv.node.name}: {ex}")
        if which.value in (BFTOOLS, "all"):
            for sdir, names, _info in self._staged_bftools(only=server):
                if pm.cancel_pending(BFTOOLS, sdir):
                    removed.append(f"`{BFTOOLS}` ({', '.join(names)})")
        if which.value in ("bfdb.exe", "all") and pm.cancel_pending("bfdb.exe"):
            removed.append("`bfdb.exe`")
        # nothing left to apply -> drop the "apply when empty" asks it leaves
        for name in list(self.stage_apply_when_empty):
            srv = self.bot.servers.get(name)
            if srv is not None and (server is None or srv is server) and not await self._pending_swaps(srv):
                self.stage_apply_when_empty.pop(name, None)
        self.save_state()
        await interaction.followup.send(
            f"{icon('discard')} Discarded: {', '.join(removed)}" if removed else "Nothing to discard.")

    @feops.command(name="stage_apply", description="Apply a staged binary now, or as soon as the server is empty.")
    @app_commands.guild_only()
    @utils.app_has_role('DCS Admin')
    @app_commands.choices(which=[
        app_commands.Choice(name="engine DLL (bflib.dll / bfrange.dll)", value="dll"),
        app_commands.Choice(name="bftools.exe", value="bftools.exe"),
        app_commands.Choice(name="bfdb.exe", value="bfdb.exe"),
        app_commands.Choice(name="all", value="all"),
    ], when=[
        app_commands.Choice(name="now (the server must be shut down)", value="now"),
        app_commands.Choice(name="when empty (restarts DCS once nobody is on)", value="when_empty"),
        app_commands.Choice(name="restart DCS now (kicks anyone on)", value="restart"),
    ])
    async def feops_stage_apply(self, interaction: discord.Interaction,
                                server: app_commands.Transform[Server, utils.ServerTransformer()],
                                which: app_commands.Choice[str],
                                when: Optional[app_commands.Choice[str]] = None):
        await interaction.response.defer(ephemeral=True)
        pm = await self._procman_or_warn(interaction)
        if not pm:
            return
        want_dll = which.value in ("dll", "all", "bflib.dll", "bfrange.dll")
        want_tools = which.value in (BFTOOLS, "all")
        want_bfdb = which.value in ("bfdb.exe", "all")
        msgs = []
        mode = when.value if when else "now"
        if mode in ("when_empty", "restart") and (want_dll or want_tools):
            if want_bfdb and pm.pending_info("bfdb.exe"):
                await pm.restart(self._bfdb_admin_password)
                msgs.append("bfdb.exe: swapped and bfdb restarted.")
            if not await self._pending_swaps(server):
                msgs.append(f"Nothing staged for **{server.name}** that a DCS restart would swap in.")
            elif mode == "when_empty":
                msgs.append(self.schedule_apply_when_empty([server.name], str(interaction.user)))
            else:
                msgs.append(await self.restart_to_apply([server.name], str(interaction.user)))
            await interaction.followup.send("\n".join(msgs))
            return

        if want_bfdb and pm.pending_info("bfdb.exe"):
            await pm.restart(self._bfdb_admin_password)  # start() applies the staged exe
            msgs.append("bfdb.exe: swapped and bfdb restarted." if await pm.health_ok()
                        else "bfdb.exe: swapped, bfdb restarting (not answering yet).")

        b = self._engine_binaries(server)
        dll, sdir = b["dll_name"], (b["staging_dir"] or None)
        # bftools.exe first: the DLL branch below may start the server. DCS
        # never holds bftools open, so it can go in whatever the server's
        # state; it is used from the next mission build on. (Mid-build, the
        # swap fails safe and keeps the old one.)
        tools_live = bftools_path_of(self, server)
        if want_tools and self._on_remote_node(server):
            msgs.append(f"{BFTOOLS}: **{server.name}** is on node `{server.node.name}` -- BFBinaries there swaps it "
                        f"in on its next start (use `when: when empty`).")
        elif want_tools and tools_live and pm.pending_info(BFTOOLS, sdir):
            note = pm.apply_staged(BFTOOLS, tools_live, staging_dir=sdir)
            msgs.append(f"{BFTOOLS}: {note} -- used from the next mission build." if note
                        else f"{BFTOOLS}: nothing staged.")
        elif want_tools:
            msgs.append(f"{BFTOOLS}: nothing staged for **{server.name}**"
                        + ("" if tools_live else " (it has no BFWeather `bftools` path)") + ".")

        if want_dll and self._on_remote_node(server):
            # Its staging dir is on that PC. The BFBinaries extension there
            # swaps the DLL in when DCS starts, so applying it is a start.
            if not await self._remote_staged(only=server):
                msgs.append(f"{dll}: nothing staged for **{server.name}** (in `{sdir}` on node "
                            f"`{server.node.name}`).")
            elif server.status not in (Status.SHUTDOWN, Status.STOPPED):
                msgs.append(f"{dll}: **{server.name}** is `{server.status.name}` -- shut it down first, "
                            f"then it applies automatically on startup (or use `when: when empty`).")
            else:
                try:
                    await server.startup()
                    msgs.append(f"{dll}: {server.name} started; BFBinaries on node "
                                f"`{server.node.name}` swaps it in on the way up.")
                except Exception as ex:
                    msgs.append(f"{icon('warning')} {server.name} failed to start: {ex}")
        elif want_dll and pm.pending_info(dll, sdir):
            live = b["dll_path"]
            if not live:
                msgs.append(f"{dll}: skipped -- no live path configured (set `dll_path` on the "
                            f"BFBinaries extension in nodes.yaml).")
            elif server.status not in (Status.SHUTDOWN, Status.STOPPED):
                msgs.append(f"{dll}: **{server.name}** is `{server.status.name}` -- shut it down first, "
                            f"then it applies automatically on startup (or use `when: when empty`).")
            else:
                note = pm.apply_staged(dll, live, staging_dir=sdir)
                msgs.append(f"{dll}: {note}" if note else f"{dll}: nothing staged.")
                try:
                    await server.startup()
                    msgs.append(f"{server.name} started.")
                except Exception as ex:
                    msgs.append(f"{icon('warning')} {server.name} failed to start: {ex}")
        elif want_dll:
            msgs.append(f"{dll}: nothing staged for **{server.name}** (in `{sdir or pm.staging_dir}`).")

        await interaction.followup.send("\n".join(msgs) if msgs else "Nothing staged to apply.")

    @feops.command(name="versions", description="Show the git rev + build time of the running engine binaries.")
    @app_commands.guild_only()
    @utils.app_has_role('DCS Admin')
    async def feops_versions(self, interaction: discord.Interaction,
                             server: app_commands.Transform[Server, utils.ServerTransformer()]):
        await interaction.response.defer(ephemeral=True)
        builds = await asyncio.get_running_loop().run_in_executor(
            None, lambda: self._engine_builds(server))
        cfg = self.get_config(server) or {}
        exe = os.path.expandvars((cfg.get("bfdb") or {}).get("exe", ""))
        dll = self._bflib_dll_path(server)
        bftools = os.path.expandvars(self._ext_cfg(server, "BFWeather").get("bftools", ""))
        disk = {"bfdb": exe, "bflib": dll, "bftools": bftools}

        lines = []
        for name in ("bfdb", "bflib", "bftools"):
            b = builds.get(name) or {}
            path = disk.get(name) or ""
            if "error" in b:
                lines.append(f"**{name}** — {icon('warning')} {b['error']}")
            else:
                lines.append(f"**{name}** `v{b.get('version','?')}` `{b.get('git','?')}` "
                             f"built {b.get('built','?')}")
            if name == "bflib" and path and self._on_remote_node(server):
                lines.append(f"   ↳ file on node `{server.node.name}`: `{path}` (not checked from here)")
            elif path and os.path.exists(path):
                mt = datetime.fromtimestamp(os.path.getmtime(path), tz=timezone.utc)
                lines.append(f"   ↳ file `{sha256_of(path)[:10]}` · modified {mt:%Y-%m-%d %H:%M}Z")
            elif path:
                lines.append(f"   ↳ file missing: `{path}`")

        if self.procman and self.procman.enabled:
            staged = self._staged_for(server)
            if staged:
                lines.append(f"\n{icon('pending')} staged (applies next restart): " + ", ".join(f"`{s}`" for s in staged))

        embed = self._vs_embed("Engine Builds", color=discord.Color.blurple())
        embed.description = "\n".join(lines)
        embed.set_footer(text="'built' = compiled-in timestamp of the running binary · "
                              "'file' = what's on disk right now")
        await interaction.followup.send(embed=embed)

    # ── auto-update ─────────────────────────────────────────────────────────

    async def _updater_or_warn(self, interaction: discord.Interaction):
        if not self.updater:
            await interaction.followup.send(f"{icon('bad')} The auto-updater is not loaded.")
            return None
        return self.updater

    @feops.command(name="update_status", description="Auto-update: latest release, what's installed, probation.")
    @app_commands.guild_only()
    @utils.app_has_role('DCS Admin')
    async def feops_update_status(self, interaction: discord.Interaction):
        await interaction.response.defer(ephemeral=True)
        upd = await self._updater_or_warn(interaction)
        if not upd:
            return
        st = upd.status()
        c = st["config"]
        src = c["repo"] if c["source"] == "github" else c["folder"]
        lines = [
            f"**Auto-update** {icon('good') + ' on' if c['enabled'] else icon('paused') + ' off'}"
            + (" (paused)" if c["paused"] else "")
            + f" · {c['source']} `{src}` · {c['channel']} · every {c['check_minutes']:.0f} min",
            f"Apply: `{c['apply']}` (bfdb `{c['bfdb_apply']}`), idle ≥ {c['idle_minutes']:.0f} min"
            + (f", window {c['apply_window']}" if c["apply_window"] else ""),
        ]
        last = st.get("last_check") or {}
        if last:
            lines.append(f"Last check {last.get('at', '?')}: "
                         + (last.get("message") or last.get("error") or "?"))
        latest = st.get("latest") or {}
        if latest:
            lines.append(f"Latest release: **{latest.get('tag')}** `{latest.get('git') or '?'}` "
                         f"built {latest.get('built') or '?'}")
        for key, v in (st.get("installed") or {}).items():
            lines.append(f"• installed `{key}`: {v.get('tag') or '(manual upload)'} at {v.get('at')}")
        for p in st.get("probation") or []:
            where = p["server"] or "bfdb"
            lines.append(f"{icon('probation')} probation: `{p['dll']}` on {where} -- "
                         + ("loaded" if p["loaded"] else "waiting to load")
                         + (f", {p['crashes']} crash(es)" if p["crashes"] else ""))
        if st.get("bad"):
            lines.append(f"{icon('blocked')} marked bad: " + ", ".join(f"`{t}`" for t in st["bad"]))
        camps = st.get("campaigns") or {}
        for row in camps.get("servers") or []:
            inst, pack, prob = row.get("installed") or {}, row.get("pack") or {}, row.get("probation")
            if not (inst or pack or prob or row.get("last_error")):
                continue
            line = f"{icon('campaign')} **{row['server']}** campaign: {inst.get('tag') or 'none from a release yet'}"
            if prob:
                line += f" · {icon('probation')} {prob['tag']} on probation" + (" (up)" if prob["loaded"] else "")
            elif pack:
                line += f" · {pack.get('tag')} **{pack.get('status')}**"
                if pack.get("reason"):
                    line += f" -- {pack['reason']}"
            if row.get("last_error"):
                line += f" · {icon('warning')} {row['last_error']}"
            lines.append(line)
        if camps and not camps.get("enabled"):
            lines.append(f"{icon('campaign')} campaign packs: off (`autoupdate.campaigns`)")
        embed = self._vs_embed("Engine Auto-update", color=discord.Color.blurple())
        embed.description = "\n".join(lines)[:4000]
        await interaction.followup.send(embed=embed)

    @feops.command(name="update_check", description="Check for a new engine release now (and stage it).")
    @app_commands.guild_only()
    @utils.app_has_role('DCS Admin')
    async def feops_update_check(self, interaction: discord.Interaction):
        await interaction.response.defer(ephemeral=True)
        upd = await self._updater_or_warn(interaction)
        if not upd:
            return
        res = await upd.check(reason=f"manual (/feops by {interaction.user})")
        if res.get("ok"):
            await interaction.followup.send(f"{icon('good')} {res.get('message')}"
                                            + (f" -- latest `{res['latest']}`" if res.get("latest") else ""))
        else:
            await interaction.followup.send(f"{icon('bad')} check failed: {res.get('error')}")

    @feops.command(name="update_pause", description="Pause or resume automatic engine updates.")
    @app_commands.guild_only()
    @utils.app_has_role('DCS Admin')
    async def feops_update_pause(self, interaction: discord.Interaction, paused: bool):
        await interaction.response.defer(ephemeral=True)
        upd = await self._updater_or_warn(interaction)
        if not upd:
            return
        upd.set_overrides({"paused": paused})
        await interaction.followup.send(f"{icon('paused')} Auto-update paused." if paused else f"{icon('resume')} Auto-update resumed.")

    @feops.command(name="update_rollback", description="Roll bfdb or a server's engine DLL back to its previous build.")
    @app_commands.guild_only()
    @utils.app_has_role('DCS Admin')
    @app_commands.choices(which=[
        app_commands.Choice(name="engine DLL (restarts that DCS server)", value="dll"),
        app_commands.Choice(name="bfdb.exe (restores the pre-update DB snapshot)", value="bfdb"),
    ])
    async def feops_update_rollback(self, interaction: discord.Interaction, which: app_commands.Choice[str],
                                    server: Optional[app_commands.Transform[Server, utils.ServerTransformer()]] = None,
                                    confirm: bool = False):
        await interaction.response.defer(ephemeral=True)
        upd = await self._updater_or_warn(interaction)
        if not upd:
            return
        if not confirm:
            await interaction.followup.send("Re-run with `confirm: True`. A DLL rollback restarts that DCS "
                                            "server; a bfdb rollback restores the DB from right before the "
                                            "last bfdb update.")
            return
        why = f"rolled back by {interaction.user} via /feops"
        if which.value == "bfdb":
            pm = await self._procman_or_warn(interaction)
            if not pm:
                return
            await interaction.followup.send(await pm.rollback_bfdb(self._bfdb_admin_password, why))
            return
        if server is None:
            await interaction.followup.send(f"{icon('bad')} Pick the `server` whose engine DLL to roll back.")
            return
        await interaction.followup.send(await upd.rollback_dll(server, why))

    @feops.command(name="update_unmark", description="Allow a rolled-back release to be offered again.")
    @app_commands.guild_only()
    @utils.app_has_role('DCS Admin')
    async def feops_update_unmark(self, interaction: discord.Interaction, tag: str):
        await interaction.response.defer(ephemeral=True)
        upd = await self._updater_or_warn(interaction)
        if not upd:
            return
        await interaction.followup.send(f"{icon('good')} `{tag}` may be offered again." if upd.unmark_bad(tag)
                                        else f"`{tag}` is not marked bad.")

    async def _campaign_decide(self, interaction: discord.Interaction, server, decision: str):
        await interaction.response.defer(ephemeral=True)
        upd = await self._updater_or_warn(interaction)
        if not upd:
            return
        try:
            msg = await asyncio.to_thread(upd.campaign_decide, server.name, decision, str(interaction.user))
        except (LookupError, ValueError) as ex:
            await interaction.followup.send(f"{icon('bad')} {ex}")
            return
        await self.notify_ops(f"{icon('campaign')} **{server.name}**: {msg} (/feops by {interaction.user})")
        await interaction.followup.send(f"{icon('good')} {msg}")

    @feops.command(name="campaign_apply",
                   description="Write a HELD campaign pack over the server's edited cfg/mission (backup kept).")
    @app_commands.guild_only()
    @utils.app_has_role('DCS Admin')
    async def feops_campaign_apply(self, interaction: discord.Interaction,
                                   server: app_commands.Transform[Server, utils.ServerTransformer()],
                                   confirm: bool = False):
        if not confirm:
            await interaction.response.send_message(
                "Re-run with `confirm: True`. The pack replaces the cfg/mission edited on the server at its "
                "next DCS start; the current files are backed up first.", ephemeral=True)
            return
        await self._campaign_decide(interaction, server, "apply")

    @feops.command(name="campaign_keep",
                   description="Keep the server's edited cfg; dismiss the held campaign pack (other missions still go in).")
    @app_commands.guild_only()
    @utils.app_has_role('DCS Admin')
    async def feops_campaign_keep(self, interaction: discord.Interaction,
                                  server: app_commands.Transform[Server, utils.ServerTransformer()]):
        await self._campaign_decide(interaction, server, "keep")

    @feops.command(name="issues", description="Log analyzer: open issues, with the full report attached.")
    @app_commands.guild_only()
    @utils.app_has_role('DCS Admin')
    async def feops_issues(self, interaction: discord.Interaction, scan_now: bool = False):
        await interaction.response.defer(ephemeral=True)
        if not self.issues:
            await interaction.followup.send(f"{icon('bad')} The log analyzer is not loaded.")
            return
        if scan_now:
            await self.issues.scan()
        rows = self.issues.listing()
        lines = []
        for it in rows[:12]:
            flag = icon("restart") if it.get("status") == "regressed" else icon("new") if it.get("status") == "new" else "•"
            lines.append(f"{flag} `{it['id']}` **{it['level']}** ×{it['count']} `{it['source']}`\n"
                         f"   {it['signature'][:150]}")
        embed = self._vs_embed(f"Open issues: {len(rows)}", color=discord.Color.orange())
        embed.description = ("\n".join(lines) or f"No open issues. {icon('all_clear')}")[:4000]
        embed.set_footer(text="Full report attached -- hand it to Claude, or see the dashboard OPS page.")
        report = self.issues.report()
        await interaction.followup.send(
            embed=embed, file=discord.File(io.BytesIO(report.encode("utf-8")), filename="fowl-issues.md"))

    @feops.command(name="db_restore",
                   description="bfdb won't start (damaged database)? Restore the newest good copy of it.")
    @app_commands.guild_only()
    @utils.app_has_role('DCS Admin')
    @app_commands.choices(source=[
        app_commands.Choice(name="newest copy of either kind", value="newest"),
        app_commands.Choice(name="daily backup (always consistent)", value="daily"),
        app_commands.Choice(name="copy taken before the last bfdb.exe swap", value="swap"),
    ])
    async def feops_db_restore(self, interaction: discord.Interaction,
                               source: Optional[app_commands.Choice[str]] = None,
                               confirm: bool = False):
        """Stops bfdb, moves the damaged database aside (kept, not deleted),
        copies the chosen backup in and starts bfdb, which re-reads whatever
        the stats log has since that copy."""
        await interaction.response.defer(ephemeral=True)
        pm = await self._procman_or_warn(interaction)
        if not pm:
            return
        cands = pm.db_restore_candidates()
        if not confirm:
            listing = "\n".join(
                f"- `{os.path.basename(p)}` ({kind}, {datetime.fromtimestamp(t, tz=timezone.utc):%Y-%m-%d %H:%MZ})"
                for t, p, kind in cands[:6]) or "_none found_"
            await interaction.followup.send(
                f"{icon('warning')} This stops bfdb, moves the current database aside and restores a copy. "
                f"Copies available, newest first:\n{listing}\nRe-run with **confirm: True** to proceed.")
            return
        await interaction.followup.send(
            await pm.restore_db(self._bfdb_admin_password, source.value if source else "newest"))

    @feops.command(name="rebuild_stats",
                   description="Wipe & re-ingest all stats from the log to undo duplicated/inflated numbers.")
    @app_commands.guild_only()
    @utils.app_has_role('DCS Admin')
    async def feops_rebuild_stats(self, interaction: discord.Interaction,
                                  server: app_commands.Transform[Server, utils.ServerTransformer()],
                                  confirm: bool = False):
        """Stops bfdb, runs `bfdb --rebuild-stats` (wipes every stats-derived
        tree + rewinds the replay cursors), restarts it so it re-ingests the
        stats log cleanly. Fixes inflated career totals / kill counts left by
        the old whole-file re-reads. Auth, Discord links, bans, wiki and recon
        intel are preserved."""
        await interaction.response.defer(ephemeral=True)
        config = self.get_config(server) or {}
        api_url = config.get("api_url", "http://localhost:8880")
        username = config.get("admin_username")
        password = config.get("admin_password")
        if not username or not password:
            await interaction.followup.send(
                f"{icon('bad')} admin_username/admin_password must be set in fowlengine.yaml to use admin actions.")
            return
        if not confirm:
            await interaction.followup.send(
                f"{icon('warning')} This wipes all pilot stats / kills / sorties / objectives and rebuilds them "
                "from the stats log — takes a few minutes. bfdb stays up (in-process rebuild). "
                "Auth, Discord links, bans, wiki and recon intel are kept.\n"
                "Re-run with **confirm: True** to proceed.")
            return
        try:
            status, data = await bfdb_admin_post(
                api_url, username, password, "/api/admin/rebuild-stats", {})
            if status != 200:
                await interaction.followup.send(f"{icon('bad')} rebuild-stats failed: HTTP {status} {data}")
                return
            msg = (data or {}).get("message", str(data))
            await interaction.followup.send(
                f"{icon('good')} {msg}\nWatch the engine-log / perf embed. When it settles, run `/feops merge_rounds`.")
        except Exception as ex:
            await interaction.followup.send(f"Error: {ex}")

    @feops.command(name="fresh_db",
                   description="LAST RESORT: move a corrupt bfdb DB aside and rebuild it from the stats log.")
    @app_commands.guild_only()
    @utils.app_has_role('DCS Admin')
    async def feops_fresh_db(self, interaction: discord.Interaction,
                             server: app_commands.Transform[Server, utils.ServerTransformer()],
                             confirm: bool = False):
        await interaction.response.defer(ephemeral=True)
        pm = await self._procman_or_warn(interaction)
        if not pm:
            return
        if not confirm:
            await interaction.followup.send(
                f"{icon('warning')} **Only if the sled DB is corrupt** (a rebuild that can't get past a 'Read corrupted "
                "data at file offset' error). Moves the whole `bfdb` folder aside and rebuilds from "
                "`stats.jsonl`.\n"
                "**Rebuilt:** pilot stats, Discord links, a single clean round.\n"
                "**Lost:** dashboard login sessions (auto re-login), dashboard-added bans, in-dashboard "
                "wiki edits (reverts to seeded), per-round recon photos.\n"
                "The old folder is kept. Re-run with **confirm: True**.")
            return
        try:
            msg = await pm.fresh_db(self._bfdb_admin_password)
            await interaction.followup.send(msg)
        except Exception as ex:
            await interaction.followup.send(f"Error: {ex}")

    @feops.command(name="merge_rounds",
                   description="Collapse a campaign that shows as dozens of duplicated 'rounds' back into one.")
    @app_commands.guild_only()
    @utils.app_has_role('DCS Admin')
    async def feops_merge_rounds(self, interaction: discord.Interaction,
                                 server: app_commands.Transform[Server, utils.ServerTransformer()],
                                 confirm: bool = False):
        """Dry-run by default. Pass confirm:True to actually re-key the data.
        Repairs the old fork-on-restart bug where one continuous campaign
        fragmented into many round ids (duplicated sorties/kills/deploys)."""
        await interaction.response.defer(ephemeral=True)
        config = self.get_config(server) or {}
        api_url = config.get("api_url", "http://localhost:8880")
        username = config.get("admin_username")
        password = config.get("admin_password")
        if not username or not password:
            await interaction.followup.send(
                f"{icon('bad')} admin_username/admin_password must be set in fowlengine.yaml to use admin actions.")
            return
        dry = "false" if confirm else "true"
        try:
            status, data = await bfdb_admin_post(
                api_url, username, password,
                srv_path(f"/api/admin/merge-rounds?dry_run={dry}", server.name), {})
            if status != 200:
                await interaction.followup.send(f"{icon('bad')} merge-rounds failed: HTTP {status} {data}")
                return
            msg = (data or {}).get("message", str(data))
            if confirm:
                await interaction.followup.send(f"{icon('good')} {msg}")
            else:
                await interaction.followup.send(
                    f"{icon('inspect')} {msg}\n\nRe-run with **confirm: True** to apply.")
        except Exception as ex:
            await interaction.followup.send(f"Error: {ex}")


    # ── icon set ────────────────────────────────────────────────────────────

    @feops.command(name="icons_install",
                   description="Upload the Vector Strike icon set to Discord (custom emoji).")
    @app_commands.guild_only()
    @utils.app_has_role('DCS Admin')
    async def feops_icons_install(self, interaction: discord.Interaction):
        """Installs the PNGs in plugins/fowlengine/assets/icons as custom emoji.

        Deliberately a command and not a startup step: uploading emoji changes
        what the guild (or the application) owns, and that is the operator's
        call to make once, not something a bot restart should do on its own.
        Until it is run, every embed falls back to unicode and reads fine.
        """
        await interaction.response.defer(ephemeral=True)
        try:
            added, skipped, errors = await self.icons.install(interaction.guild)
        except Exception as ex:
            await interaction.followup.send(f"Error: {ex}")
            return
        from .icons import glyph_keys
        embed = self._vs_embed("Icon Set", color=discord.Color.blurple())
        parts = []
        if added:
            parts.append(f"**Added {len(added)}:** " + _fit_rows(
                [f"{self.icons(n[3:])} `{n[3:]}`" for n in added], 1800, "  "))
        parts.append(f"**Skipped {len(skipped)}** (already installed)")
        if errors:
            parts.append(f"**Failed {len(errors)}:**\n" + _fit_rows([f"`{e}`" for e in errors], 1200, "\n"))
        parts.append(f"{self.icons.installed}/{len(glyph_keys())} icons now resolve to custom emoji. "
                     f"Safe to re-run: only missing icons are uploaded.")
        embed.description = "\n\n".join(parts)[:4000]
        await interaction.followup.send(embed=embed)

    @feops.command(name="icons_status", description="Show which Fowl Engine icons are installed.")
    @app_commands.guild_only()
    @utils.app_has_role('DCS Admin')
    async def feops_icons_status(self, interaction: discord.Interaction):
        await interaction.response.defer(ephemeral=True)
        await self.icons.refresh()
        from .icons import ALIASES, glyph_keys
        rows = [f"{self.icons(key)} `{key}`" for key in glyph_keys()]
        embed = self._vs_embed("Icon Set", color=discord.Color.blurple())
        embed.description = (
            f"**{self.icons.installed} of {len(glyph_keys())}** installed as custom emoji; "
            f"the rest fall back to unicode.\n\n" + _fit_rows(rows, 3800, "  "))
        embed.add_field(name="Aliases (share a glyph)",
                        value=_fit_rows([f"`{a}`→`{k}`" for a, k in ALIASES.items()], 1000, " · "),
                        inline=False)
        embed.set_footer(text="Install with /feops icons_install · "
                              "regenerate the PNGs with assets/icons/render_icons.py")
        await interaction.followup.send(embed=embed)

    @feops.command(name="icons_uninstall",
                   description="Remove every Fowl Engine custom emoji (vs_*) from Discord.")
    @app_commands.guild_only()
    @utils.app_has_role('DCS Admin')
    async def feops_icons_uninstall(self, interaction: discord.Interaction, confirm: bool = False):
        await interaction.response.defer(ephemeral=True)
        if not confirm:
            await interaction.followup.send(
                f"{icon('warning')} This deletes every `vs_*` emoji this app or guild owns. Any message already "
                "using one renders it as plain text afterwards.\n\nRe-run with **confirm: True**.")
            return
        removed, errors = await self.icons.uninstall()
        msg = f"{icon('good')} Removed {len(removed)} icon(s)."
        if errors:
            msg += f"\n{icon('bad')} " + "\n".join(f"`{e}`" for e in errors[:10])
        await interaction.followup.send(msg)

    # ── briefing channels ───────────────────────────────────────────────────

    @feops.command(name="briefing_lock",
                   description="Lock the two briefing channels to their coalition roles.")
    @app_commands.guild_only()
    @utils.app_has_role('DCS Admin')
    async def feops_briefing_lock(self, interaction: discord.Interaction,
                                  server: app_commands.Transform[Server, utils.ServerTransformer()],
                                  confirm: bool = False):
        """Sets the channel permission overwrites that actually enforce the split.

        The bot posts one coalition's intel into each channel; Discord, not the
        bot, is what stops the other side reading it. This applies the overwrites
        for you -- deny @everyone, allow the coalition role, allow the bot -- but
        only on an explicit confirm, because it rewrites channel permissions.
        """
        await interaction.response.defer(ephemeral=True)
        config = self.get_config(server) or {}
        channels = self._briefing_channels(config)
        cr = config.get("coalition_roles") or {}
        if not channels:
            await interaction.followup.send(
                f"{icon('bad')} No `blue_briefing_channel` / `red_briefing_channel` set for "
                f"**{server.name}** in fowlengine.yaml.")
            return

        plan, targets = [], []
        for side, cid in channels.items():
            channel = self.bot.get_channel(cid)
            role = self._resolve_role(interaction.guild, cr.get(side.lower()))
            if not channel:
                plan.append(f"{icon('bad')} {side}: channel `{cid}` not found / no access")
                continue
            if not role:
                plan.append(f"{icon('bad')} {side}: role {cr.get(side.lower())!r} not found in this guild")
                continue
            plan.append(f"{icon('good')} {side}: {channel.mention} → viewable only by {role.mention}")
            targets.append((channel, role))

        if not confirm:
            embed = self._vs_embed("Briefing Channel Lock", color=discord.Color.orange())
            embed.description = (
                "\n".join(plan) +
                "\n\n@everyone will be **denied** View Channel; the coalition role and this "
                "bot will be allowed. Existing overwrites for other roles are left alone.\n\n"
                "Re-run with **confirm: True** to apply.")
            await interaction.followup.send(embed=embed)
            return

        applied = []
        for channel, role in targets:
            try:
                await channel.set_permissions(
                    interaction.guild.default_role, view_channel=False,
                    reason="Fowl Engine: coalition briefing")
                await channel.set_permissions(
                    role, view_channel=True, read_message_history=True,
                    reason="Fowl Engine: coalition briefing")
                await channel.set_permissions(
                    interaction.guild.me, view_channel=True, send_messages=True,
                    read_message_history=True, embed_links=True,
                    reason="Fowl Engine: coalition briefing")
                applied.append(f"{icon('good')} {channel.mention} locked to {role.mention}")
            except discord.Forbidden:
                applied.append(f"{icon('bad')} {channel.mention}: bot lacks Manage Permissions")
            except Exception as ex:
                applied.append(f"{icon('bad')} {channel.mention}: {ex}")
        await interaction.followup.send("\n".join(applied))
