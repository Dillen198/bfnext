"""PlayerCommandsMixin: part of the FowlEngine cog (commands.py).

The /fe_* player and admin slash commands: objective, card, dashboard,
ban / unban, commanders, side switch and the Commander Terminal.

Moved out of commands.py unchanged. A mixin: it holds no state of its own and
relies on what FowlEngine.__init__ sets up; discord.py collects its commands,
groups, loops and listeners through the MRO."""
import discord
from core import utils, Server, Status, command
from discord import app_commands
import asyncio
import io
from typing import Optional
from .icons import icon
from .common import (NO_PINGS, RANGE_ONLY_MSG, RANK_TITLES, bfdb_admin_get, bfdb_admin_post, srv_params, srv_path)
from .views import CommanderTerminalView
from .cog_coalition import ROLE_KINDS


class PlayerCommandsMixin:
    @command(description='Show detailed status for one objective.')
    @app_commands.guild_only()
    @utils.app_has_role('DCS')
    async def fe_objective(self, interaction: discord.Interaction,
                           server: app_commands.Transform[Server, utils.ServerTransformer(status=[Status.RUNNING, Status.PAUSED])],
                           name: str):
        if self._is_range(server):
            await interaction.response.send_message(RANGE_ONLY_MSG.format(name=server.name), ephemeral=True)
            return
        await interaction.response.defer()
        config = self.get_config(server) or {}
        api_url = config.get("api_url", "http://localhost:8880")
        try:
            import aiohttp
            async with aiohttp.ClientSession() as session:
                async with session.get(f"{api_url}/api/objectives", params=srv_params(server.name), timeout=10) as resp:
                    if resp.status != 200:
                        await interaction.followup.send("Failed to retrieve objectives from dashboard API.")
                        return
                    objs = await resp.json()
        except Exception as ex:
            await interaction.followup.send(f"Error: {ex}")
            return

        needle = name.strip().lower()
        matches = [o for o in objs if needle in o.get('name', '').lower()]
        if not matches:
            await interaction.followup.send(f"No objective matching `{name}` found.")
            return
        if len(matches) > 1:
            exact = [o for o in matches if o.get('name', '').lower() == needle]
            if len(exact) == 1:
                matches = exact
            else:
                names = ", ".join(o.get('name', '?') for o in matches[:10])
                await interaction.followup.send(f"Multiple objectives match `{name}`: {names}. Be more specific.")
                return

        o = matches[0]
        owner = o.get('owner', 'Unknown')
        health = o.get('health', 0)
        color = {"Blue": discord.Color.blue(), "Red": discord.Color.red()}.get(owner, discord.Color.light_grey())
        embed = self._vs_embed(f"{icon('target')} {o.get('name', 'Unknown')}", color=color, server=server)
        embed.add_field(name="Owner", value=owner, inline=True)
        embed.add_field(name="Health", value=f"{health}%", inline=True)
        if o.get('kind'):
            embed.add_field(name="Type", value=o['kind'], inline=True)
        if o.get('priority'):
            embed.add_field(name="Priority", value=f"{icon('priority')} Yes", inline=True)
        if health <= 20:
            embed.add_field(name="Status", value=f"{icon('pending')} Ready to capture!", inline=False)
        await interaction.followup.send(embed=embed)

    @command(description='Show a pilot card: level, squadron and headline stats.')
    @app_commands.guild_only()
    async def fe_card(self, interaction: discord.Interaction, member: Optional[discord.Member] = None):
        target = member or interaction.user
        await interaction.response.defer()
        ucid = self.bot.get_ucid_by_member(target, verified=True)
        if asyncio.iscoroutine(ucid):
            ucid = await ucid
        if not ucid:
            who = "You are" if target == interaction.user else f"{target.display_name} is"
            await interaction.followup.send(
                f"{who} not linked to a game account yet -- use /linkme, then -linkme <token> in DCS chat.")
            return
        config = self.get_config() or {}
        api_url = config.get("api_url", "http://localhost:8880")
        try:
            import aiohttp
            async with aiohttp.ClientSession() as session:
                async with session.get(f"{api_url}/api/pilot/{ucid}/card.png", timeout=15) as resp:
                    if resp.status == 404:
                        await interaction.followup.send("No stats for that pilot yet -- fly a sortie first.")
                        return
                    if resp.status != 200:
                        await interaction.followup.send(f"Could not draw the card (bfdb answered {resp.status}).")
                        return
                    png = await resp.read()
        except Exception as ex:
            await interaction.followup.send(f"Error: {ex}")
            return
        await interaction.followup.send(file=discord.File(io.BytesIO(png), filename="pilot-card.png"))

    @command(description='Get the link to the Fowl Engine web dashboard.')
    @app_commands.guild_only()
    async def fe_dashboard(self, interaction: discord.Interaction):
        config = self.get_config() or {}
        dashboard_url = config.get("dashboard_url", "https://bfweb.your-domain.com")
        embed = self._vs_embed("Dashboard", color=discord.Color.blue(), url=dashboard_url)
        embed.description = "Access your pilot profile, live map, and stats."
        # Login is Discord OAuth on the dashboard itself. (There used to be an
        # HMAC "auto-login" link here, signed with `dashboard_secret`, that
        # nothing on the bfdb side ever accepted.)
        embed.add_field(name="Standard Login", value=f"[Login with Discord]({dashboard_url}/login)", inline=False)

        await interaction.response.send_message(embed=embed, ephemeral=True)

    @command(description='Ban a pilot from the campaign (by UCID).')
    @app_commands.guild_only()
    @utils.app_has_role('DCS Admin')
    async def fe_ban(self, interaction: discord.Interaction,
                     server: app_commands.Transform[Server, utils.ServerTransformer(status=[Status.RUNNING, Status.PAUSED])],
                     ucid: str, name: str, reason: str = "", until: str = None):
        await interaction.response.defer(ephemeral=True)
        config = self.get_config(server) or {}
        api_url = config.get("api_url", "http://localhost:8880")
        username = config.get("admin_username")
        password = config.get("admin_password")
        if not username or not password:
            await interaction.followup.send(
                f"{icon('bad')} admin_username/admin_password must be set in fowlengine.yaml (must match bfdb's "
                "--admin-username/--admin-password) to use admin actions."
            )
            return
        try:
            status, data = await bfdb_admin_post(
                api_url, username, password,
                "/api/admin/ban", {"ucid": ucid, "name": name, "reason": reason, "until": until},
            )
            if status == 200:
                await interaction.followup.send(f"{icon('ban')} Banned **{name}** (`{ucid}`)" + (f" until {until}" if until else " indefinitely") + (f": {reason}" if reason else "."),
                                               allowed_mentions=NO_PINGS)
            else:
                await interaction.followup.send(f"{icon('bad')} Failed to ban: HTTP {status}")
        except Exception as ex:
            await interaction.followup.send(f"Error: {ex}")

    # ── Commanders ──────────────────────────────────────────────────────────

    def _admin_creds(self, server):
        config = self.get_config(server) or {}
        return (config.get("api_url", "http://localhost:8880"),
                config.get("admin_username"), config.get("admin_password"))

    async def _commander_roster(self, server):
        """(status, roster) from bfdb's /api/admin/commanders for one server."""
        api_url, username, password = self._admin_creds(server)
        if not username or not password:
            return None, None
        return await bfdb_admin_get(api_url, username, password,
                                    srv_path("/api/admin/commanders", server.name))

    @command(description='Who commands each side on one server: earned by rank, or granted.')
    @app_commands.guild_only()
    @utils.app_has_role('DCS')
    async def fe_commanders(self, interaction: discord.Interaction,
                            server: app_commands.Transform[Server, utils.ServerTransformer()]):
        await interaction.response.defer(ephemeral=True)
        status, data = await self._commander_roster(server)
        if status is None:
            await interaction.followup.send(f"{icon('bad')} admin_username/admin_password must be set "
                                            f"in fowlengine.yaml to read the commander roster.")
            return
        if status == 404:
            await interaction.followup.send(f"{icon('bad')} {server.name}'s bfdb predates commander access.")
            return
        if status != 200 or not isinstance(data, dict):
            await interaction.followup.send(f"{icon('bad')} commander roster: HTTP {status}")
            return
        need_tier = data.get("commander_rank", 4)
        need_score = data.get("commander_score", 50)
        embed = discord.Embed(title=f"Commanders · {server.name}", color=0xd4a017)
        for side in ("Blue", "Red"):
            rows = []
            for p in data.get("pilots") or []:
                if not p.get("commander") or p.get("side") != side:
                    continue
                why = "admin" if p.get("admin") else ("granted" if p.get("grant") == "granted" else "rank")
                rows.append(f"**{p.get('name', '?')}** · {RANK_TITLES[max(1, min(8, p.get('tier', 1))) - 1]}"
                            f" · {round(p.get('score', 0))} ({why})")
            embed.add_field(name=f"{side} ({len(rows)})", value="\n".join(rows[:20]) or "nobody yet", inline=True)
        embed.set_footer(text=f"Command unlocks at {RANK_TITLES[max(1, min(8, need_tier)) - 1]} "
                              f"(campaign score {round(need_score)}). Admins can grant or withdraw it.")
        await interaction.followup.send(embed=embed, allowed_mentions=NO_PINGS)

    @command(description='Grant or withdraw commander access for one member (admin).')
    @app_commands.guild_only()
    @utils.app_has_role('DCS Admin')
    @app_commands.describe(
        server="Which campaign server's roster to look the member up in (a grant applies on every server)",
        member="The Discord member (they must have linked their DCS account with /linkme)",
        access="Grant it whatever their rank, withdraw it, or go back to what their rank says",
    )
    @app_commands.choices(access=[
        app_commands.Choice(name="Grant", value="granted"),
        app_commands.Choice(name="Withdraw", value="revoked"),
        app_commands.Choice(name="By rank", value="rank"),
    ])
    async def fe_commander(self, interaction: discord.Interaction,
                           server: app_commands.Transform[Server, utils.ServerTransformer()],
                           member: discord.Member, access: app_commands.Choice[str]):
        await interaction.response.defer(ephemeral=True)
        api_url, username, password = self._admin_creds(server)
        status, data = await self._commander_roster(server)
        if status is None:
            await interaction.followup.send(f"{icon('bad')} admin_username/admin_password must be set "
                                            f"in fowlengine.yaml to manage commanders.")
            return
        if status != 200 or not isinstance(data, dict):
            await interaction.followup.send(f"{icon('bad')} commander roster: HTTP {status}")
            return
        ucid = await self._ucid_for_member(member, data.get("pilots") or [])
        if not ucid:
            await interaction.followup.send(f"{icon('bad')} {member.mention} hasn't linked a DCS account "
                                            f"(/linkme), so there is no pilot to give command to.",
                                            allowed_mentions=NO_PINGS)
            return
        grant = None if access.value == "rank" else access.value
        status, res = await bfdb_admin_post(api_url, username, password,
                                            srv_path("/api/admin/commanders", server.name),
                                            {"ucid": ucid, "grant": grant})
        if status != 200:
            await interaction.followup.send(f"{icon('bad')} HTTP {status}")
            return
        st = (res or {}).get("status") or {}
        now = "a commander" if st.get("commander") else "not a commander"
        what = {"granted": "granted command", "revoked": "withdrew command",
                "rank": "put command back on rank"}[access.value]
        self.log.info(f"FowlEngine: {interaction.user} {what} for {member} ({ucid})")
        await interaction.followup.send(f"{icon('good')} {what} for {member.mention}: on {server.name} they are "
                                        f"{now}. The Discord role follows within a few minutes.",
                                        allowed_mentions=NO_PINGS)
        try:
            await self._sync_role_kind(ROLE_KINDS[1])
        except Exception as ex:
            self.log.debug(f"FowlEngine: commander role sync after a grant: {ex}")

    @command(description='Move a pilot to the other coalition on one server.')
    @app_commands.guild_only()
    @utils.app_has_role('DCS Admin')
    @app_commands.describe(
        server="Which campaign server",
        player="In-game name (case-insensitive), player id, or UCID",
        side="The side to put them on")
    @app_commands.choices(side=[
        app_commands.Choice(name="Blue", value="Blue"),
        app_commands.Choice(name="Red", value="Red"),
    ])
    async def fe_side_switch(self, interaction: discord.Interaction,
                             server: app_commands.Transform[Server, utils.ServerTransformer(status=[Status.RUNNING, Status.PAUSED])],
                             player: str, side: app_commands.Choice[str]):
        # Goes to that server's engine, which owns the registration: the side
        # is per server, and a pilot sitting in a slot is sent to spectators
        # so they can't keep flying for the side they left. The Discord
        # coalition roles follow on the next role sync.
        await interaction.response.defer(ephemeral=True)
        config = self.get_config(server) or {}
        api_url = config.get("api_url", "http://localhost:8880")
        username = config.get("admin_username")
        password = config.get("admin_password")
        if not username or not password:
            await interaction.followup.send(
                f"{icon('bad')} admin_username/admin_password must be set in fowlengine.yaml (must match bfdb's "
                "--admin-username/--admin-password) to use admin actions."
            )
            return
        try:
            status, data = await bfdb_admin_post(
                api_url, username, password,
                srv_path("/api/admin/side-switch", server.name),
                {"player": player, "side": side.value},
            )
            if status == 200:
                await interaction.followup.send(
                    f"{icon('restart')} **{player}** is now {side.value} on {server.name}. If they were in a slot "
                    f"they've been sent to spectators.", allowed_mentions=NO_PINGS)
            else:
                detail = (data or {}).get("error") or (data or {}).get("message") or ""
                await interaction.followup.send(
                    f"{icon('bad')} Side switch failed (HTTP {status}){': ' + str(detail) if detail else ''}",
                    allowed_mentions=NO_PINGS)
        except Exception as ex:
            await interaction.followup.send(f"Error: {ex}")

    @command(description='Unban a pilot from the campaign (by UCID).')
    @app_commands.guild_only()
    @utils.app_has_role('DCS Admin')
    async def fe_unban(self, interaction: discord.Interaction,
                       server: app_commands.Transform[Server, utils.ServerTransformer(status=[Status.RUNNING, Status.PAUSED])],
                       ucid: str):
        await interaction.response.defer(ephemeral=True)
        config = self.get_config(server) or {}
        api_url = config.get("api_url", "http://localhost:8880")
        username = config.get("admin_username")
        password = config.get("admin_password")
        if not username or not password:
            await interaction.followup.send(
                f"{icon('bad')} admin_username/admin_password must be set in fowlengine.yaml (must match bfdb's "
                "--admin-username/--admin-password) to use admin actions."
            )
            return
        try:
            status, data = await bfdb_admin_post(
                api_url, username, password, "/api/admin/unban", {"ucid": ucid},
            )
            if status == 200:
                was_banned = bool(data and data.get("was_banned"))
                await interaction.followup.send(f"{icon('good')} Unbanned `{ucid}`." if was_banned else f"{icon('info')} `{ucid}` was not banned.")
            else:
                await interaction.followup.send(f"{icon('bad')} Failed to unban: HTTP {status}")
        except Exception as ex:
            await interaction.followup.send(f"Error: {ex}")

    @command(description='Open the interactive Commander Terminal.')
    @app_commands.guild_only()
    @utils.app_has_role('DCS Admin')
    async def fe_terminal(self, interaction: discord.Interaction,
                          server: app_commands.Transform[Server, utils.ServerTransformer(status=[Status.RUNNING, Status.PAUSED])]):
        await interaction.response.defer(ephemeral=True)
        if self._is_range(server):
            await interaction.followup.send(RANGE_ONLY_MSG.format(name=server.name))
            return
        try:
            import aiohttp
            config = self.get_config(server) or {}
            api_url = config.get("api_url", "http://localhost:8880")
            admin_username = config.get("admin_username")
            admin_password = config.get("admin_password")

            async with aiohttp.ClientSession() as session:
                async with session.get(f"{api_url}/api/objectives", params=srv_params(server.name)) as resp:
                    if resp.status != 200:
                        await interaction.followup.send("Failed to retrieve airbases from dashboard API.")
                        return
                    objs = await resp.json()
                    
                cfg = None
                cfg_path = config.get("cfg_path")
                if cfg_path:
                    try:
                        import json
                        with open(cfg_path, 'r', encoding='utf-8') as f:
                            cfg = json.load(f)
                    except Exception as e:
                        self.log.error(f"Failed to read CFG from {cfg_path}: {e}")
                        
                if not cfg:
                    async with session.get(f"{api_url}/api/config", params=srv_params(server.name)) as resp:
                        if resp.status != 200:
                            await interaction.followup.send("Failed to retrieve CFG from local file or dashboard API.")
                            return
                        cfg = await resp.json()
            
            # Filter for Airbases and FARPs
            airbases = [o for o in objs if o.get('kind') in ['Airbase', 'Farp']]
            
            dynamic_types = []
            if 'ground_vehicle_cargo' in cfg:
                for k in cfg['ground_vehicle_cargo'].keys():
                    dynamic_types.append((k, "Cargo Vehicle"))
            
            if 'deployables' in cfg:
                for side in ['Red', 'Blue']:
                    if side in cfg['deployables']:
                        for d in cfg['deployables'][side]:
                            if 'path' in d and len(d['path']) > 0:
                                name = d['path'][-1]
                                if (name, side) not in dynamic_types:
                                    dynamic_types.append((name, f"{side} Deployable"))
            
            # Fallback
            if not dynamic_types:
                dynamic_types.append(("No Deployables found", "CFG empty"))
                
            view = CommanderTerminalView(api_url, admin_username, admin_password, airbases,
                                         dynamic_types, objectives=objs, server_name=server.name)
            embed = self._vs_embed(f"{self._branding(server, config)['brand_name']} Commander Terminal",
                                   color=discord.Color.dark_red(), server=server)
            embed.description = f"Order logistics (top) or set objective priority (bottom).\nServer: **{server.name}**"
            await interaction.followup.send(embed=embed, view=view)
        except Exception as ex:
            await interaction.followup.send(f"Error: {ex}")
