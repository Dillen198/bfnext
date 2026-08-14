import discord
from core import Plugin, utils, Server, Status, command
from discord import app_commands
from discord.ext import tasks
from services.bot import DCSServerBot
from typing import Type
import json
import os

from .listener import FowlEngineEventListener

class CommanderTerminalView(discord.ui.View):
    def __init__(self, api_url: str, dashboard_secret: str, airbases: list, dynamic_types: list):
        super().__init__(timeout=None)
        self.api_url = api_url
        self.dashboard_secret = dashboard_secret
        self.selected_airbase = None
        self.selected_type = None

        options = []
        for ab in airbases[:25]:
            options.append(discord.SelectOption(label=ab['name'], description=f"Owner: {ab['owner']}"))
        if not options:
            options.append(discord.SelectOption(label="No Airbases Found", value="none"))
            
        self.airbase_select = discord.ui.Select(placeholder="Step 1: Select Airbase/FARP...", options=options, custom_id="ab_select")
        self.airbase_select.callback = self.ab_callback
        
        type_options = []
        for t_label, t_desc in dynamic_types[:25]:
            type_options.append(discord.SelectOption(label=t_label, description=t_desc))
            
        if not type_options:
            type_options.append(discord.SelectOption(label="No Deployables found in CFG", value="none"))
            
        self.type_select = discord.ui.Select(placeholder="Step 2: Select Deployable...", options=type_options, custom_id="type_select")
        self.type_select.callback = self.type_callback

        self.add_item(self.airbase_select)
        self.add_item(self.type_select)

    async def ab_callback(self, interaction: discord.Interaction):
        self.selected_airbase = self.airbase_select.values[0]
        await interaction.response.send_message(f"Base selected: {self.selected_airbase}", ephemeral=True)

    async def type_callback(self, interaction: discord.Interaction):
        self.selected_type = self.type_select.values[0]
        await interaction.response.send_message(f"Deployable selected: {self.selected_type}", ephemeral=True)

    @discord.ui.button(label="EXECUTE SPAWN", style=discord.ButtonStyle.success, row=2)
    async def spawn_btn(self, interaction: discord.Interaction, button: discord.ui.Button):
        if not self.selected_airbase or self.selected_airbase == "none":
            await interaction.response.send_message("Please select a valid Airbase first.", ephemeral=True)
            return
        if not self.selected_type:
            await interaction.response.send_message("Please select a Deployable first.", ephemeral=True)
            return
            
        await interaction.response.defer(ephemeral=True)
        import aiohttp
        async with aiohttp.ClientSession() as session:
            payload = {"airbase": self.selected_airbase, "type": self.selected_type}
            headers = {"Authorization": f"Bearer {self.dashboard_secret}"}
            async with session.post(f"{self.api_url}/api/commander/spawn", json=payload, headers=headers) as resp:
                if resp.status == 200:
                    await interaction.followup.send(f"✅ Successfully ordered {self.selected_type} at {self.selected_airbase}.")
                else:
                    await interaction.followup.send(f"❌ Failed to spawn: {resp.status}")

class FowlEngine(Plugin[FowlEngineEventListener]):
    """
    Fowl Engine plugin for DCSServerBot.
    Provides live status and objective events.
    """

    def __init__(self, bot: DCSServerBot, listener: Type[FowlEngineEventListener]):
        super().__init__(bot, listener)
        self.status_msg_id = None
        self.state_file = os.path.join(bot.node.config_dir, 'fowlengine_state.json')
        if os.path.exists(self.state_file):
            try:
                with open(self.state_file, 'r') as f:
                    state = json.load(f)
                    self.status_msg_id = state.get('status_msg_id')
            except Exception as ex:
                self.log.error(f"Failed to load Fowl Engine state: {ex}")

    async def cog_load(self) -> None:
        await super().cog_load()
        utils.safe_start(self.update_status)
        utils.safe_start(self.sync_ranks)

    async def cog_unload(self):
        await utils.safe_cancel(self.update_status)
        await utils.safe_cancel(self.sync_ranks)
        await super().cog_unload()

    def save_state(self):
        try:
            with open(self.state_file, 'w') as f:
                json.dump({'status_msg_id': self.status_msg_id}, f)
        except Exception as ex:
            self.log.error(f"Failed to save Fowl Engine state: {ex}")

    @tasks.loop(minutes=1.0)
    async def update_status(self):
        # We need to find the active server running Fowl Engine. 
        # For simplicity, we assume the first active server or a specific one.
        for server in self.bot.servers.values():
            if server.status not in [Status.RUNNING, Status.PAUSED]:
                continue
                
            config = self.get_config(server)
            if not config or 'status_channel' not in config:
                continue
                
            try:
                import aiohttp
                config = self.get_config(server) or {}
                api_url = config.get("api_url", "http://localhost:8765")
                async with aiohttp.ClientSession() as session:
                    async with session.get(f"{api_url}/api/stats") as resp:
                        if resp.status != 200:
                            continue
                        stats = await resp.json()
                        
                    async with session.get(f"{api_url}/api/objectives") as resp:
                        if resp.status == 200:
                            objs = await resp.json()
                        else:
                            objs = []
                        
                blue_objs = len([o for o in objs if o.get('owner') == 'Blue'])
                red_objs = len([o for o in objs if o.get('owner') == 'Red'])
                
                brand_name = config.get('brand_name', 'Fowl Engine')
                embed = discord.Embed(title=f"{brand_name} Campaign Status", color=discord.Color.blue())
                
                if 'active_round' in stats and stats['active_round']:
                    embed.description = f"**Active Scenario:** {stats['active_round'].get('scenario', 'Unknown')}"
                else:
                    embed.description = "**Server Offline or No Active Round**"
                    
                embed.add_field(name="🟦 Blue Faction", value=f"Pilots: {stats.get('blue_online', 0)}\nObjectives: {blue_objs}", inline=True)
                embed.add_field(name="🟥 Red Faction", value=f"Pilots: {stats.get('red_online', 0)}\nObjectives: {red_objs}", inline=True)
                
                weather = stats.get('weather')
                if weather:
                    temp = weather.get('temp_c', 0)
                    wind = weather.get('wind_speed_kts', 0)
                    clouds = weather.get('cloud_density', 0)
                    embed.add_field(name="⛅ Live Weather", value=f"🌡️ {temp:.1f}°C | 💨 {wind:.1f}kts | ☁️ Density {clouds}/10", inline=False)
                    
                restart_at = stats.get('restart_at')
                if restart_at:
                    embed.set_footer(text=f"Rotation scheduled for {restart_at}")    
                channel_id = int(config['status_channel'])
                channel = self.bot.get_channel(channel_id)
                if not channel:
                    self.log.error(f"FowlEngine: status_channel {channel_id} not found or bot lacks access.")
                    continue
                    
                if self.status_msg_id:
                    try:
                        msg = await channel.fetch_message(self.status_msg_id)
                        await msg.edit(embed=embed)
                        continue
                    except discord.NotFound:
                        self.status_msg_id = None
                    except discord.Forbidden:
                        self.log.error(f"FowlEngine: Bot lacks permissions to read/edit in channel {channel_id}")
                        self.status_msg_id = None
                        
                msg = await channel.send(embed=embed)
                self.status_msg_id = msg.id
                self.save_state()
                
            except Exception as ex:
                import traceback
                self.log.error(f"Error updating Fowl Engine status: {ex}\n{traceback.format_exc()}")

    @update_status.before_loop
    async def before_update_status(self):
        await self.bot.wait_until_ready()
        
    @command(description='Force an immediate update of the status embed.')
    @app_commands.guild_only()
    @utils.app_has_role('DCS Admin')
    async def fe_force_status(self, interaction: discord.Interaction):
        await interaction.response.defer(ephemeral=True)
        await self.update_status()
        await interaction.followup.send("Forced status update loop to run. Check the bot logs or the status channel!")

    @command(description='List Fowl Engine objectives.')
    @app_commands.guild_only()
    @utils.app_has_role('DCS')
    async def fe_objectives(self, interaction: discord.Interaction,
                     server: app_commands.Transform[Server, utils.ServerTransformer(status=[Status.RUNNING, Status.PAUSED])]):
        await interaction.response.defer(thinking=True)
        try:
            import aiohttp
            config = self.get_config(server) or {}
            api_url = config.get("api_url", "http://localhost:8765")
            async with aiohttp.ClientSession() as session:
                async with session.get(f"{api_url}/api/objectives") as resp:
                    if resp.status != 200:
                        await interaction.followup.send("Failed to retrieve objectives from dashboard API.")
                        return
                    objs = await resp.json()
            
            if not objs:
                await interaction.followup.send("No objectives found.")
                return

            embed = discord.Embed(title="Fowl Engine Objectives", color=discord.Color.green())
            blue_objs = [o['name'] for o in objs if o.get('owner') == 'Blue']
            red_objs = [o['name'] for o in objs if o.get('owner') == 'Red']
            neutral_objs = [o['name'] for o in objs if o.get('owner') not in ['Blue', 'Red']]

            embed.add_field(name=f"Blue ({len(blue_objs)})", value=", ".join(blue_objs) if blue_objs else "None", inline=False)
            embed.add_field(name=f"Red ({len(red_objs)})", value=", ".join(red_objs) if red_objs else "None", inline=False)
            embed.add_field(name=f"Neutral ({len(neutral_objs)})", value=", ".join(neutral_objs) if neutral_objs else "None", inline=False)
            
            await interaction.followup.send(embed=embed)
        except Exception as ex:
            await interaction.followup.send(f"Error: {ex}")

    @command(description='Get the link to the Fowl Engine web dashboard.')
    @app_commands.guild_only()
    async def fe_dashboard(self, interaction: discord.Interaction):
        config = self.get_config() or {}
        dashboard_url = config.get("dashboard_url", "https://bfweb.your-domain.com")
        dashboard_secret = config.get("dashboard_secret", None)
        
        embed = discord.Embed(title="Fowl Engine Dashboard", color=discord.Color.blue())
        embed.description = "Access your pilot profile, live map, and stats."
        
        embed.add_field(name="Standard Login", value=f"[Login with Discord]({dashboard_url}/login)", inline=False)
        
        if dashboard_secret:
            import hmac, hashlib, base64, time, json
            payload = {"id": str(interaction.user.id), "exp": int(time.time()) + 3600}
            payload_b64 = base64.urlsafe_b64encode(json.dumps(payload).encode()).decode().rstrip('=')
            signature = hmac.new(dashboard_secret.encode(), payload_b64.encode(), hashlib.sha256).digest()
            sig_b64 = base64.urlsafe_b64encode(signature).decode().rstrip('=')
            token = f"{payload_b64}.{sig_b64}"
            auto_login_url = f"{dashboard_url}/auth?token={token}"
            embed.add_field(name="Auto-Login (Expires in 1 hr)", value=f"[Click here for secure auto-login]({auto_login_url})\n*Do not share this link!*", inline=False)
            
        await interaction.response.send_message(embed=embed, ephemeral=True)

    @command(description='Join a faction (Red, Blue, Neutral).')
    @app_commands.guild_only()
    async def fe_join(self, interaction: discord.Interaction, 
                      server: app_commands.Transform[Server, utils.ServerTransformer(status=[Status.RUNNING, Status.PAUSED])]):
        await interaction.response.defer(ephemeral=True)
        await interaction.followup.send("⚠️ Faction selection in the Fowl Engine is now securely handled via the web dashboard.\n\nPlease use the `/fe_dashboard` command to securely login and select your faction there!", ephemeral=True)

    @command(description='Show your Fowl Engine statistics.')
    @app_commands.guild_only()
    async def fe_stats(self, interaction: discord.Interaction,
                       server: app_commands.Transform[Server, utils.ServerTransformer(status=[Status.RUNNING, Status.PAUSED])],
                       user: discord.Member = None):
        target = user or interaction.user
        await interaction.response.defer()
        
        config = self.get_config(server) or {}
        api_url = config.get("api_url", "http://localhost:8765")
        
        # Get the UCID for the discord member
        ucid = self.bot.get_ucid_by_member(target)
        if not ucid:
            await interaction.followup.send(f"{target.display_name} has not linked their Discord account to a DCS UCID.")
            return
            
        try:
            import aiohttp
            async with aiohttp.ClientSession() as session:
                async with session.get(f"{api_url}/api/pilot/{ucid}/breakdown") as resp:
                    if resp.status != 200:
                        await interaction.followup.send(f"Failed to fetch stats for {target.display_name}. Ensure the backend is running.")
                        return
                    rounds = await resp.json()
                    
            if not rounds:
                await interaction.followup.send(f"No stats found for {target.display_name}.")
                return
                
            # Aggregate stats across all rounds
            total_air = sum(r.get('air_kills', 0) for r in rounds)
            total_gnd = sum(r.get('ground_kills', 0) for r in rounds)
            total_caps = sum(r.get('captures', 0) for r in rounds)
            total_deaths = sum(r.get('deaths', 0) for r in rounds)
            total_hours = sum(r.get('hours', 0.0) for r in rounds)
            
            # Calculate synthetic points
            points = (total_air * 10) + (total_gnd * 2) + (total_caps * 50)
            
            brand_name = config.get('brand_name', 'Fowl Engine')
            embed = discord.Embed(title=f"{brand_name} Stats for {target.display_name}", color=discord.Color.gold())
            embed.add_field(name="Air Kills", value=str(total_air), inline=True)
            embed.add_field(name="Ground Kills", value=str(total_gnd), inline=True)
            embed.add_field(name="Captures", value=str(total_caps), inline=True)
            embed.add_field(name="Deaths", value=str(total_deaths), inline=True)
            embed.add_field(name="Flight Hours", value=f"{total_hours:.1f}h", inline=True)
            embed.add_field(name="Total Points", value=str(points), inline=True)
            
            await interaction.followup.send(embed=embed)
        except Exception as ex:
            self.log.error(f"Error in fe_stats: {ex}")
            await interaction.followup.send("An error occurred while fetching stats.")

    @command(description='Spawn a deployable at an airbase.')
    @app_commands.guild_only()
    @utils.app_has_role('DCS Admin')
    async def fe_spawn_deployable(self, interaction: discord.Interaction,
                                  server: app_commands.Transform[Server, utils.ServerTransformer(status=[Status.RUNNING, Status.PAUSED])],
                                  deployable_type: str, airbase: str):
        await interaction.response.defer(ephemeral=True)
        try:
            import aiohttp
            config = self.get_config(server) or {}
            api_url = config.get("api_url", "http://localhost:8765")
            dashboard_secret = config.get("dashboard_secret", "dummy_secret")
            
            async with aiohttp.ClientSession() as session:
                payload = {"airbase": airbase, "type": deployable_type}
                headers = {"Authorization": f"Bearer {dashboard_secret}"}
                async with session.post(f"{api_url}/api/commander/spawn", json=payload, headers=headers) as resp:
                    if resp.status == 200:
                        await interaction.followup.send(f"✅ Successfully ordered {deployable_type} at {airbase}.")
                    else:
                        await interaction.followup.send(f"❌ Failed to spawn: HTTP {resp.status}")
        except Exception as ex:
            await interaction.followup.send(f"Error: {ex}")

    @command(description='Set an objective as high priority.')
    @app_commands.guild_only()
    @utils.app_has_role('DCS Admin')
    async def fe_priority(self, interaction: discord.Interaction,
                          server: app_commands.Transform[Server, utils.ServerTransformer(status=[Status.RUNNING, Status.PAUSED])],
                          objective: str):
        await interaction.response.defer(ephemeral=True)
        await interaction.followup.send("Priority feature is pending backend API support in bfdb.", ephemeral=True)

    @command(description='Open the interactive Commander Terminal.')
    @app_commands.guild_only()
    @utils.app_has_role('DCS Admin')
    async def fe_terminal(self, interaction: discord.Interaction,
                          server: app_commands.Transform[Server, utils.ServerTransformer(status=[Status.RUNNING, Status.PAUSED])]):
        await interaction.response.defer(ephemeral=True)
        try:
            import aiohttp
            config = self.get_config(server) or {}
            api_url = config.get("api_url", "http://localhost:8765")
            dashboard_secret = config.get("dashboard_secret", "dummy_secret")
            
            async with aiohttp.ClientSession() as session:
                async with session.get(f"{api_url}/api/objectives") as resp:
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
                    async with session.get(f"{api_url}/api/config") as resp:
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
                
            view = CommanderTerminalView(api_url, dashboard_secret, airbases, dynamic_types)
            brand_name = config.get('brand_name', 'Fowl Engine')
            embed = discord.Embed(title=f"{brand_name} Commander Terminal", color=discord.Color.dark_red())
            embed.description = "Use the controls below to order logistics."
            await interaction.followup.send(embed=embed, view=view)
        except Exception as ex:
            await interaction.followup.send(f"Error: {ex}")

async def setup(bot: DCSServerBot):
    await bot.add_cog(FowlEngine(bot, FowlEngineEventListener))


    @tasks.loop(minutes=5.0)
    async def sync_ranks(self):
        for server in self.bot.servers.values():
            if server.status not in [Status.RUNNING, Status.PAUSED]:
                continue
                
            config = self.get_config(server)
            if not config or 'rank_thresholds' not in config:
                continue
                
            rank_thresholds = config['rank_thresholds']
            if not rank_thresholds:
                continue
                
            # Sort thresholds descending by points
            sorted_ranks = sorted(rank_thresholds.items(), key=lambda x: x[1], reverse=True)
            
            api_url = config.get("api_url", "http://localhost:8765")
            try:
                import aiohttp
                async with aiohttp.ClientSession() as session:
                    async with session.get(f"{api_url}/api/pilots") as resp:
                        if resp.status != 200:
                            continue
                        pilots = await resp.json()
                        
                for p in pilots:
                    ucid = p.get('ucid')
                    if not ucid:
                        continue
                        
                    total_air = p.get('air_kills', 0)
                    total_gnd = p.get('ground_kills', 0)
                    total_caps = p.get('captures', 0)
                    
                    points = (total_air * 10) + (total_gnd * 2) + (total_caps * 50)
                    
                    target_role_name = None
                    for r_name, r_thresh in sorted_ranks:
                        if points >= r_thresh:
                            target_role_name = r_name
                            break
                            
                    if not target_role_name:
                        continue
                        
                    member = self.bot.get_member_by_ucid(ucid)
                    if not member:
                        continue
                        
                    # Find role and assign
                    guild = member.guild
                    target_role = discord.utils.get(guild.roles, name=target_role_name)
                    if not target_role:
                        continue
                        
                    if target_role not in member.roles:
                        # Optionally remove other rank roles
                        roles_to_remove = [r for r in member.roles if r.name in rank_thresholds and r.name != target_role_name]
                        if roles_to_remove:
                            await member.remove_roles(*roles_to_remove, reason="Fowl Engine auto-demotion")
                        await member.add_roles(target_role, reason="Fowl Engine auto-promotion")
                        self.log.info(f"FowlEngine: Promoted {member.display_name} to {target_role_name}")
                        
            except Exception as ex:
                self.log.error(f"FowlEngine rank sync error: {ex}")

    @sync_ranks.before_loop
    async def before_sync_ranks(self):
        await self.bot.wait_until_ready()
