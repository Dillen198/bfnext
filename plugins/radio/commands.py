# plugins/radio/commands.py
import discord
from discord import app_commands, ui
from discord.ext import commands
from core import Plugin, utils
import asyncio
import os
import random
import json
import re
import aiohttp

# yt-dlp (and the PyNaCl voice codec it depends on via discord.py) are not
# part of DCSServerBot's own requirements, so they can easily be missing from
# the bot's venv. An unguarded `import yt_dlp` at module scope makes that a
# hard ImportError during plugin discovery, which DCSServerBot reports as the
# generic 'Plugin "Radio" not found!' -- no traceback, no hint it's a missing
# dependency. Guard it like smartmod guards google.generativeai so the plugin
# still loads and commands fail with a clear message instead.
try:
    import yt_dlp
    # Suppress yt-dlp warnings
    yt_dlp.utils.bug_reports_message = lambda *args, **kwargs: ''
    HAS_YTDLP = True
except ImportError as e:
    HAS_YTDLP = False
    print(f"\n==========================================================")
    print(f"🚨 RADIO WARNING: Missing yt-dlp! ({e})")
    print(f"Run `pip install yt-dlp PyNaCl` in the bot's venv, then restart.")
    print(f"==========================================================\n")

ytdl_format_options = {
    'format': 'bestaudio/best',
    'outtmpl': '%(extractor)s-%(id)s-%(title)s.%(ext)s',
    'restrictfilenames': True,
    'noplaylist': True,
    'nocheckcertificate': True,
    'ignoreerrors': False,
    'logtostderr': False,
    'quiet': True,
    'no_warnings': True,
    'default_search': 'ytsearch',
    'source_address': '0.0.0.0',
}
ytdl = yt_dlp.YoutubeDL(ytdl_format_options) if HAS_YTDLP else None

def format_duration(seconds):
    if not seconds or seconds == 0: return "Live Broadcast / ∞"
    m, s = divmod(int(seconds), 60)
    h, m = divmod(m, 60)
    if h > 0: return f"{h}:{m:02d}:{s:02d}"
    return f"{m}:{s:02d}"

async def fetch_itunes_metadata(query, logger=None):
    if query.startswith("http"): return None 
    url = "https://itunes.apple.com/search"
    params = {"term": query, "entity": "song", "limit": 1}
    headers = {"User-Agent": "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36"}
    try:
        async with aiohttp.ClientSession() as session:
            async with session.get(url, params=params, headers=headers, timeout=5) as resp:
                if resp.status == 200:
                    data = await resp.json(content_type=None)
                    if data.get("resultCount", 0) > 0:
                        track = data["results"][0]
                        artist = track.get("artistName", "Unknown Artist")
                        title = track.get("trackName", "Unknown Title")
                        artwork = track.get("artworkUrl100", "").replace("100x100bb", "512x512bb")
                        return {
                            "title": f"{artist} - {title}",
                            "thumbnail": artwork,
                            "duration": track.get("trackTimeMillis", 0) // 1000
                        }
    except Exception as e:
        if logger: logger.error(f"[Radio] iTunes API error: {e}")
    return None

def clean_title(data, default_query="Unknown Track"):
    artist = data.get('artist')
    track = data.get('track')
    if artist and track: return f"{artist} - {track}"
    title = data.get('title', default_query)
    title = re.sub(r'\(.*?(Official|Video|Audio|Lyric|Visualizer|Live).*?\)', '', title, flags=re.IGNORECASE)
    title = re.sub(r'\[.*?(Official|Video|Audio|Lyric|Visualizer|Live).*?\]', '', title, flags=re.IGNORECASE)
    return title.strip()

class YTDLSource(discord.PCMVolumeTransformer):
    def __init__(self, source, *, data, volume=0.5):
        super().__init__(source, volume)
        self.data = data
        self.title = data.get('title')
        self.url = data.get('url')

    @classmethod
    async def from_url(cls, url, *, loop=None, stream=False, audio_filter=None, override_title=None):
        if not HAS_YTDLP:
            raise Exception("yt-dlp is not installed in the bot's venv -- run `pip install yt-dlp PyNaCl` and restart.")
        loop = loop or asyncio.get_event_loop()
        try:
            data = await loop.run_in_executor(None, lambda: ytdl.extract_info(url, download=not stream))
        except Exception as e:
            raise Exception(f"YT-DLP extraction failed: {e}")

        if 'entries' in data: data = data['entries'][0]
            
        data['title'] = override_title or clean_title(data, data.get('title'))
        filename = data['url'] if stream else ytdl.prepare_filename(data)
        
        ffmpeg_exe = r"C:\VectorStrike\Tools\DCSServerBot\ffmpeg.exe"
        ffmpeg_opts = '-reconnect 1 -reconnect_streamed 1 -reconnect_delay_max 5'
        
        if audio_filter == 'comms':
            options_str = '-vn -af "highpass=f=300,lowpass=f=3000,acompressor=ratio=4"'
        elif audio_filter == 'bassboost':
            options_str = '-vn -af "bass=g=10:f=110:w=0.6"'
        elif audio_filter == 'nightcore':
            options_str = '-vn -af "asetrate=44100*1.25,aresample=44100"'
        else:
            options_str = '-vn'

        final_options = {'before_options': ffmpeg_opts, 'options': options_str}
        return cls(discord.FFmpegPCMAudio(filename, executable=ffmpeg_exe, **final_options), data=data)

class RadioControlView(ui.View):
    def __init__(self, plugin):
        super().__init__(timeout=None)
        self.plugin = plugin

    @ui.button(label="⏯️ Play/Pause", style=discord.ButtonStyle.blurple, custom_id="radio_pause_resume")
    async def toggle_playback(self, interaction: discord.Interaction, button: ui.Button):
        vc = interaction.guild.voice_client
        if not vc: 
            return await interaction.response.send_message("Radio is offline.", ephemeral=True)
        if vc.is_playing():
            vc.pause()
            await interaction.response.send_message("⏸️ Radio paused.", ephemeral=True)
        elif vc.is_paused():
            vc.resume()
            await interaction.response.send_message("▶️ Radio resumed.", ephemeral=True)
        else:
            await interaction.response.send_message("Nothing is currently loaded.", ephemeral=True)

    @ui.button(label="⏭️ Skip", style=discord.ButtonStyle.secondary, custom_id="radio_skip")
    async def skip_track(self, interaction: discord.Interaction, button: ui.Button):
        vc = interaction.guild.voice_client
        if vc and (vc.is_playing() or vc.is_paused()):
            state = self.plugin.get_state(interaction.guild.id)
            if state['loop'] == 'track':
                state['loop'] = 'off'
            vc.stop() 
            await interaction.response.send_message("⏭️ Track skipped.", ephemeral=True)
        else:
            await interaction.response.send_message("Nothing to skip.", ephemeral=True)

    @ui.button(label="📜 Queue", style=discord.ButtonStyle.gray, custom_id="radio_queue")
    async def show_queue(self, interaction: discord.Interaction, button: ui.Button):
        state = self.plugin.get_state(interaction.guild.id)
        if not state['queue']:
            return await interaction.response.send_message("The queue is empty.", ephemeral=True)
        q_list = "\n".join([f"**{i+1}.** {track['title']}" for i, track in enumerate(state['queue'][:10])])
        if len(state['queue']) > 10:
            q_list += f"\n...and {len(state['queue']) - 10} more tracks."
        embed = discord.Embed(title="📻 Upcoming Tracks", description=q_list, color=discord.Color.blue())
        await interaction.response.send_message(embed=embed, ephemeral=True)

    @ui.button(label="⏹️ Stop", style=discord.ButtonStyle.red, custom_id="radio_stop")
    async def stop_radio(self, interaction: discord.Interaction, button: ui.Button):
        state = self.plugin.get_state(interaction.guild.id)
        state['queue'].clear()
        state['loop'] = 'off'
        vc = interaction.guild.voice_client
        
        if vc:
            vc.stop()
            await self.plugin.bot.change_presence(activity=None)
            
        try:
            await interaction.message.delete()
        except:
            pass 
            
        await interaction.response.send_message("⏹️ Radio stopped and player removed.", ephemeral=True)


class LiveRadioView(ui.View):
    """Stage 2: Station Selection (Paginated)"""
    def __init__(self, stations, plugin, user_id, menu_title="📻 Live Internet Radio Stations"):
        super().__init__(timeout=120)
        self.stations = stations
        self.plugin = plugin
        self.user_id = user_id
        self.menu_title = menu_title
        self.page = 0
        self.per_page = 10
        self.max_page = max(0, (len(stations) - 1) // self.per_page)
        self.update_components()

    def update_components(self):
        self.clear_items()
        start = self.page * self.per_page
        end = min(start + self.per_page, len(self.stations))
        
        options = []
        for idx in range(start, end):
            station = self.stations[idx]
            name = station.get('name', 'Unknown Station').strip()[:90] 
            country = station.get('country', 'Global').strip()[:90]
            options.append(discord.SelectOption(label=f"{idx+1}. {name}", description=f"Country: {country}", value=str(idx)))
        
        class DynamicSelect(ui.Select):
            async def callback(inner_self, interaction: discord.Interaction):
                await self.handle_selection(interaction, inner_self.values[0])
                
        select = DynamicSelect(
            placeholder=f"Select a station (Page {self.page+1}/{self.max_page+1})...", 
            min_values=1, max_values=1, options=options
        )
        self.add_item(select)
        
        prev_btn = ui.Button(label="◀️ Prev", style=discord.ButtonStyle.secondary, disabled=(self.page == 0))
        async def prev_cb(interaction: discord.Interaction):
            self.page -= 1
            self.update_components()
            await interaction.response.edit_message(embed=self.generate_embed(), view=self)
        prev_btn.callback = prev_cb
        self.add_item(prev_btn)
        
        next_btn = ui.Button(label="Next ▶️", style=discord.ButtonStyle.secondary, disabled=(self.page == self.max_page))
        async def next_cb(interaction: discord.Interaction):
            self.page += 1
            self.update_components()
            await interaction.response.edit_message(embed=self.generate_embed(), view=self)
        next_btn.callback = next_cb
        self.add_item(next_btn)

    def generate_embed(self):
        start = self.page * self.per_page
        end = min(start + self.per_page, len(self.stations))
        
        station_list = ""
        for i in range(start, end):
            station = self.stations[i]
            
            name = station.get('name', 'Unknown Station').strip()
            if len(name) > 45: name = name[:45] + "..."
                
            country = station.get('country', 'Global').strip()
            if len(country) > 20: country = country[:20] + "..."
                
            tags = station.get('tags', '')
            tag_str = f" | Tags: {tags[:25]}..." if tags else ""
            
            station_list += f"**{i+1}.** {name} *({country}{tag_str})*\n"
            
        embed = discord.Embed(
            title=f"{self.menu_title} (Page {self.page+1}/{self.max_page+1})", 
            description=f"Use the buttons to explore, and the dropdown to tune in:\n\n{station_list}",
            color=discord.Color.blue()
        )
        return embed

    async def handle_selection(self, interaction: discord.Interaction, value_str: str):
        await interaction.response.defer()
        selected_idx = int(value_str)
        station = self.stations[selected_idx]
        
        title = station.get("name", "Unknown Station").strip()
        stream_url = station.get("url_resolved") or station.get("url")
        thumbnail = station.get("favicon", "")
        
        state = self.plugin.get_state(interaction.guild.id)
        state['channel'] = interaction.channel
        
        state['queue'].append({
            'query': stream_url, 
            'title': title, 
            'thumbnail': thumbnail if thumbnail else None, 
            'duration': 0,
            'requester_name': interaction.user.display_name,
            'is_live_stream': True
        })
        
        vc = interaction.guild.voice_client
        if not vc or (not vc.is_playing() and not vc.is_paused()):
            await self.plugin.play_next_async(interaction.guild.id)
            await interaction.followup.send(f"📡 Tuning into live broadcast: **{title}**...")
        else:
            embed = discord.Embed(description=f"📝 **Added live station to queue:** {title}", color=discord.Color.gold())
            if thumbnail: embed.set_thumbnail(url=thumbnail)
            await interaction.followup.send(embed=embed)
            
        try:
            await interaction.message.delete()
        except:
            pass

    async def interaction_check(self, interaction: discord.Interaction) -> bool:
        if interaction.user.id != self.user_id:
            await interaction.response.send_message("This menu is for the person who ran the command.", ephemeral=True)
            return False
        return True


class CountrySelectView(ui.View):
    """Stage 1: Country Selection (Paginated)"""
    def __init__(self, countries, plugin, user_id):
        super().__init__(timeout=120)
        self.countries = countries
        self.plugin = plugin
        self.user_id = user_id
        self.page = 0
        self.per_page = 20 
        self.max_page = max(0, (len(countries) - 1) // self.per_page)
        self.update_components()

    def update_components(self):
        self.clear_items()
        start = self.page * self.per_page
        end = min(start + self.per_page, len(self.countries))
        
        options = []
        for idx in range(start, end):
            country = self.countries[idx]
            name = country.get('name', 'Unknown').strip()[:90] 
            count = country.get('stationcount', 0)
            options.append(discord.SelectOption(label=f"{idx+1}. {name}", description=f"{count} active stations", value=str(idx)))
        
        class DynamicSelect(ui.Select):
            async def callback(inner_self, interaction: discord.Interaction):
                await self.handle_selection(interaction, inner_self.values[0])
                
        select = DynamicSelect(
            placeholder=f"Select a country (Page {self.page+1}/{self.max_page+1})...", 
            min_values=1, max_values=1, options=options
        )
        self.add_item(select)
        
        prev_btn = ui.Button(label="◀️ Prev", style=discord.ButtonStyle.secondary, disabled=(self.page == 0))
        async def prev_cb(interaction: discord.Interaction):
            self.page -= 1
            self.update_components()
            await interaction.response.edit_message(embed=self.generate_embed(), view=self)
        prev_btn.callback = prev_cb
        self.add_item(prev_btn)
        
        next_btn = ui.Button(label="Next ▶️", style=discord.ButtonStyle.secondary, disabled=(self.page == self.max_page))
        async def next_cb(interaction: discord.Interaction):
            self.page += 1
            self.update_components()
            await interaction.response.edit_message(embed=self.generate_embed(), view=self)
        next_btn.callback = next_cb
        self.add_item(next_btn)

    def generate_embed(self):
        start = self.page * self.per_page
        end = min(start + self.per_page, len(self.countries))
        
        country_list = ""
        for i in range(start, end):
            country = self.countries[i]
            name = country.get('name', 'Unknown').strip()
            if len(name) > 60: name = name[:60] + "..."
            count = country.get('stationcount', 0)
            country_list += f"**{i+1}.** {name} *(Stations: {count})*\n"
            
        embed = discord.Embed(
            title=f"🌍 Select a Country (Page {self.page+1}/{self.max_page+1})", 
            description=f"Use the buttons to explore, and the dropdown to view stations in that country:\n\n{country_list}",
            color=discord.Color.green()
        )
        return embed

    async def handle_selection(self, interaction: discord.Interaction, value_str: str):
        await interaction.response.defer()
        selected_idx = int(value_str)
        country_name = self.countries[selected_idx].get('name', '')
        
        url = "https://de1.api.radio-browser.info/json/stations/search"
        params = {"country": country_name, "limit": 150, "hidebroken": "true", "order": "clickcount", "reverse": "true"}
        headers = {"User-Agent": "DCSServerBotMusic/1.0"}
        
        try:
            async with aiohttp.ClientSession() as session:
                async with session.get(url, params=params, headers=headers, timeout=5) as resp:
                    if resp.status == 200:
                        data = await resp.json(content_type=None)
                        if data and len(data) > 0:
                            view = LiveRadioView(data, self.plugin, self.user_id, menu_title=f"📻 Top Stations in {country_name}")
                            embed = view.generate_embed()
                            await interaction.message.edit(embed=embed, view=view)
                            return
        except Exception as e:
            self.plugin.log.error(f"[Radio] API Error fetching stations for {country_name}: {e}")
            
        await interaction.followup.send(f"❌ Could not load stations for {country_name}.", ephemeral=True)

    async def interaction_check(self, interaction: discord.Interaction) -> bool:
        if interaction.user.id != self.user_id:
            await interaction.response.send_message("This menu is for the person who ran the command.", ephemeral=True)
            return False
        return True


class Radio(Plugin):
    MAX_CONSECUTIVE_FAILURES = 3

    def __init__(self, bot):
        super().__init__(bot)
        self.states = {}

    def get_state(self, guild_id):
        if guild_id not in self.states:
            self.states[guild_id] = {'queue': [], 'current_track_info': None, 'channel': None, 'filter': None, 'loop': 'off'}
        return self.states[guild_id]

    async def cog_load(self):
        await super().cog_load()
        self.bot.add_view(RadioControlView(self))
        self.bot.loop.create_task(self._startup_sync())
        self.bot.loop.create_task(self.status_loop())

    async def _startup_sync(self):
        await self.bot.wait_until_ready()
        await self.init_db()

    async def status_loop(self):
        await self.bot.wait_until_ready()
        while not self.bot.is_closed():
            try:
                for guild_id, state in self.states.items():
                    if state.get('current_track_info'):
                        activity = discord.Activity(
                            type=discord.ActivityType.listening, 
                            name=f"{state['current_track_info']['title']} 🎵"
                        )
                        await self.bot.change_presence(status=discord.Status.online, activity=activity)
            except Exception as e:
                self.log.error(f"[Radio] Status Loop Error: {e}")
            await asyncio.sleep(15)

    async def init_db(self):
        try:
            async with self.apool.connection() as conn:
                async with conn.transaction():
                    await conn.execute("""
                        CREATE TABLE IF NOT EXISTS radio_flightplans (
                            id SERIAL PRIMARY KEY,
                            guild_id BIGINT NOT NULL,
                            plan_name VARCHAR(100) NOT NULL,
                            tracks TEXT NOT NULL,
                            UNIQUE(guild_id, plan_name)
                        )
                    """)
        except Exception as e:
            self.log.error(f"[Radio] DB Init Error: {e}")

    def play_next_sync(self, guild_id, error=None):
        if error: self.log.error(f"[Radio] Player error: {error}")
        self.bot.loop.create_task(self.play_next_async(guild_id))

    async def play_next_async(self, guild_id, _consecutive_failures=0):
        state = self.get_state(guild_id)
        guild = self.bot.get_guild(guild_id)
        if not guild: return
        
        vc = guild.voice_client
        if not vc:
            state['current_track_info'] = None
            return

        if state['current_track_info']:
            if state['loop'] == 'track': state['queue'].insert(0, state['current_track_info'])
            elif state['loop'] == 'queue': state['queue'].append(state['current_track_info'])
            
        if not state['queue']:
            state['current_track_info'] = None
            await self.bot.change_presence(activity=None)
            return
            
        next_track = state['queue'].pop(0)
        track_title = next_track['title']
        thumbnail = next_track.get('thumbnail')
        duration = next_track.get('duration', 0)
        requester = next_track.get('requester_name', 'Unknown')
        
        try:
            if next_track.get('is_live_stream'):
                ffmpeg_exe = r"C:\VectorStrike\Tools\DCSServerBot\ffmpeg.exe"
                options_str = '-vn'
                if state['filter'] == 'comms': options_str += ' -af "highpass=f=300,lowpass=f=3000,acompressor=ratio=4"'
                elif state['filter'] == 'bassboost': options_str += ' -af "bass=g=10:f=110:w=0.6"'
                elif state['filter'] == 'nightcore': options_str += ' -af "asetrate=44100*1.25,aresample=44100"'
                
                player = discord.PCMVolumeTransformer(
                    discord.FFmpegPCMAudio(
                        next_track['query'], 
                        executable=ffmpeg_exe, 
                        before_options='-reconnect 1 -reconnect_streamed 1 -reconnect_delay_max 5', 
                        options=options_str
                    )
                )
                player.title = track_title
            else:
                player = await YTDLSource.from_url(next_track['query'], loop=self.bot.loop, stream=True, audio_filter=state['filter'], override_title=track_title)
            
            state['current_track_info'] = next_track
            vc.play(player, after=lambda e: self.play_next_sync(guild_id, e))
            
            activity = discord.Activity(type=discord.ActivityType.listening, name=f"{track_title} 🎵")
            await self.bot.change_presence(status=discord.Status.online, activity=activity)
            
            embed = discord.Embed(color=0x1DB954)
            
            display_title = track_title
            artist = "Live Radio Station" if next_track.get('is_live_stream') else "Unknown Artist"
            if " - " in track_title:
                artist, display_title = track_title.split(" - ", 1)
            
            embed.set_author(name="Music Player")
            embed.add_field(name="Track", value=f"**{display_title}**", inline=False)
            embed.add_field(name="Artist", value=f"{artist}", inline=True)
            embed.add_field(name="Requested By", value=f"{requester}", inline=True)
            
            time_str = format_duration(duration)
            embed.add_field(name="Duration", value=f"▶︎ ━━◉━━━━━━━━━━━━━━━━━ {time_str}", inline=False)
            
            if thumbnail:
                embed.set_image(url=thumbnail)
            
            footer_text = f"Filter: {state['filter'].upper() if state['filter'] else 'NONE'} | Loop: {state['loop'].upper()}"
            embed.set_footer(text=footer_text)
            
            if state['channel']:
                await state['channel'].send(embed=embed, view=RadioControlView(self))
                
        except Exception as e:
            self.log.error(f"[Radio] Streaming Error: {e}")
            failures = _consecutive_failures + 1
            if failures >= self.MAX_CONSECUTIVE_FAILURES:
                state['queue'].clear()
                state['current_track_info'] = None
                if state['channel']:
                    await state['channel'].send(
                        f"❌ Playback failed {failures} times in a row (likely a broken audio backend). "
                        "Clearing the queue — check the bot logs."
                    )
                return
            if state['channel']:
                await state['channel'].send(f"❌ Error playing track. Skipping...")
            await self.play_next_async(guild_id, failures)

    async def ensure_voice(self, interaction: discord.Interaction):
        if not interaction.user.voice or not interaction.user.voice.channel:
            await interaction.followup.send("❌ You need to join a voice channel first!", ephemeral=True)
            return False
        voice_channel = interaction.user.voice.channel
        voice_client = interaction.guild.voice_client
        if not voice_client: 
            await voice_channel.connect()
        elif voice_client.channel != voice_channel: 
            await voice_client.move_to(voice_channel)
        return True

    @app_commands.command(name="live_radio", description="Browse live internet radio stations with advanced filters")
    @app_commands.describe(
        query="Optional: Search for a specific station (e.g., TruckersFM)",
        country="Optional: Bypass menu and filter by country directly (e.g., UK, Japan)"
    )
    @app_commands.guild_only()
    async def live_radio(self, interaction: discord.Interaction, query: str = None, country: str = None):
        await interaction.response.defer()
        if not await self.ensure_voice(interaction): return

        headers = {"User-Agent": "DCSServerBotMusic/1.0"}

        # STAGE 1: IF NO ARGUMENTS -> Show Country Selection Menu
        if not query and not country:
            url = "https://de1.api.radio-browser.info/json/countries"
            params = {"order": "stationcount", "reverse": "true", "hidebroken": "true", "limit": 150}
            try:
                async with aiohttp.ClientSession() as session:
                    async with session.get(url, params=params, headers=headers, timeout=5) as resp:
                        if resp.status == 200:
                            data = await resp.json(content_type=None)
                            if data and len(data) > 0:
                                view = CountrySelectView(data, self, interaction.user.id)
                                embed = view.generate_embed()
                                await interaction.followup.send(embed=embed, view=view)
                                return
            except Exception as e:
                self.log.error(f"[Radio] Radio API Error: {e}")
            
            await interaction.followup.send("❌ Unable to load countries at the moment.", ephemeral=True)
            return

        # STAGE 2: IF ARGUMENTS PROVIDED -> Jump Straight to Station List
        url = "https://de1.api.radio-browser.info/json/stations/search"
        params = {
            "limit": 150, 
            "hidebroken": "true", 
            "order": "clickcount", 
            "reverse": "true"
        }
        
        if query: params["name"] = query
        if country: params["country"] = country
        
        embed_title = "📻 Live Radio"
        if query and country: embed_title += f" | '{query}' in {country}"
        elif query: embed_title += f" | '{query}'"
        elif country: embed_title += f" | Top in {country}"
        
        try:
            async with aiohttp.ClientSession() as session:
                async with session.get(url, params=params, headers=headers, timeout=5) as resp:
                    if resp.status == 200:
                        data = await resp.json(content_type=None)
                        if data and len(data) > 0:
                            view = LiveRadioView(data, self, interaction.user.id, menu_title=embed_title)
                            embed = view.generate_embed()
                            await interaction.followup.send(embed=embed, view=view)
                            return
                            
        except Exception as e:
            self.log.error(f"[Radio] Radio API Error: {e}")
            
        await interaction.followup.send("❌ Could not find any active streams matching those filters.", ephemeral=True)

    @app_commands.command(name="play", description="Adds a track to the queue")
    @app_commands.guild_only()
    async def play(self, interaction: discord.Interaction, query: str):
        await interaction.response.defer()
        if not await self.ensure_voice(interaction): return

        state = self.get_state(interaction.guild.id)
        state['channel'] = interaction.channel
        voice_client = interaction.guild.voice_client

        try:
            itunes_data = await fetch_itunes_metadata(query, logger=self.log)
            
            if itunes_data:
                title = itunes_data["title"]
                thumbnail = itunes_data["thumbnail"]
                duration = itunes_data.get("duration", 0)
                search_query = f"ytsearch1:{title} audio" 
            else:
                loop = self.bot.loop
                data = await loop.run_in_executor(None, lambda: ytdl.extract_info(query, download=False))
                if 'entries' in data: data = data['entries'][0]
                title = clean_title(data, query)
                search_query = data.get('webpage_url', data.get('url', query))
                thumbnail = data.get('thumbnail')
                duration = data.get('duration', 0)
            
            state['queue'].append({
                'query': search_query, 
                'title': title, 
                'thumbnail': thumbnail, 
                'duration': duration,
                'requester_name': interaction.user.display_name,
                'is_live_stream': False
            })
            
            if not voice_client.is_playing() and not voice_client.is_paused():
                await interaction.followup.send(f"📡 Tuned frequency to **{title}**...")
                await self.play_next_async(interaction.guild.id)
            else:
                embed = discord.Embed(description=f"📝 **Added to queue:** {title}", color=discord.Color.gold())
                await interaction.followup.send(embed=embed)
        except Exception as e:
            self.log.error(f"[Radio] Search failed: {e}")
            await interaction.followup.send("❌ Error finding track.", ephemeral=True)

    @app_commands.command(name="tune_frequency", description="Tune into a 24/7 radio station")
    @app_commands.choices(station=[
        app_commands.Choice(name="Lofi Tower (Chill/Study)", value="https://www.youtube.com/watch?v=jfKfPfyJRdk"),
        app_commands.Choice(name="Dogfight FM (Synthwave/Phonk)", value="https://www.youtube.com/watch?v=4xDzrUhVK28"),
        app_commands.Choice(name="Classic Rock Radio", value="ytsearch1:classic rock 24/7 live stream")
    ])
    @app_commands.guild_only()
    async def tune_frequency(self, interaction: discord.Interaction, station: app_commands.Choice[str]):
        await interaction.response.defer()
        if not await self.ensure_voice(interaction): return
        
        state = self.get_state(interaction.guild.id)
        state['channel'] = interaction.channel
        state['queue'].clear()
        state['loop'] = 'off'
        
        state['queue'].append({
            'query': station.value, 
            'title': station.name, 
            'thumbnail': None, 
            'duration': 0,
            'requester_name': interaction.user.display_name,
            'is_live_stream': False
        })
        
        vc = interaction.guild.voice_client
        if vc and (vc.is_playing() or vc.is_paused()): vc.stop()
        else: await self.play_next_async(interaction.guild.id)
        await interaction.followup.send(f"📻 Tuned main receiver to **{station.name}**.")

    @app_commands.command(name="filter", description="Apply an audio filter (Takes effect on the NEXT track)")
    @app_commands.choices(effect=[
        app_commands.Choice(name="Clear (No Filter)", value="clear"),
        app_commands.Choice(name="Aviation Comms", value="comms"),
        app_commands.Choice(name="Bassboost", value="bassboost"),
        app_commands.Choice(name="Nightcore", value="nightcore")
    ])
    @app_commands.guild_only()
    async def apply_filter(self, interaction: discord.Interaction, effect: app_commands.Choice[str]):
        state = self.get_state(interaction.guild.id)
        state['filter'] = None if effect.value == "clear" else effect.value
        await interaction.response.send_message(f"🎛️ Audio filter set to **{effect.name}**.")

    @app_commands.command(name="loop", description="Change the looping mode")
    @app_commands.choices(mode=[
        app_commands.Choice(name="Off", value="off"),
        app_commands.Choice(name="Track (Loop Current)", value="track"),
        app_commands.Choice(name="Queue (Loop All)", value="queue")
    ])
    @app_commands.guild_only()
    async def loop_mode(self, interaction: discord.Interaction, mode: app_commands.Choice[str]):
        state = self.get_state(interaction.guild.id)
        state['loop'] = mode.value
        await interaction.response.send_message(f"🔁 Loop mode set to: **{mode.name}**")

    @app_commands.command(name="shuffle", description="Randomizes the current queue")
    @app_commands.guild_only()
    async def shuffle(self, interaction: discord.Interaction):
        state = self.get_state(interaction.guild.id)
        if len(state['queue']) < 2:
            return await interaction.response.send_message("Not enough tracks.", ephemeral=True)
        random.shuffle(state['queue'])
        await interaction.response.send_message("🔀 Queue randomized.")

    @app_commands.command(name="remove", description="Removes a specific track number from the queue")
    @app_commands.guild_only()
    async def remove_track(self, interaction: discord.Interaction, position: int):
        state = self.get_state(interaction.guild.id)
        if position < 1 or position > len(state['queue']):
            return await interaction.response.send_message("Invalid position number. Check `/queue`.", ephemeral=True)
        removed = state['queue'].pop(position - 1)
        await interaction.response.send_message(f"🗑️ Removed **{removed['title']}** from the queue.")

    @app_commands.command(name="queue", description="Shows the upcoming transmission schedule")
    @app_commands.guild_only()
    async def show_queue_cmd(self, interaction: discord.Interaction):
        state = self.get_state(interaction.guild.id)
        if not state['queue']:
            return await interaction.response.send_message("The queue is empty.", ephemeral=True)
        q_list = "\n".join([f"**{i+1}.** {track['title']}" for i, track in enumerate(state['queue'][:10])])
        embed = discord.Embed(title="📻 Upcoming", description=q_list, color=discord.Color.blue())
        await interaction.response.send_message(embed=embed)
        
    @app_commands.command(name="stop", description="Cuts the radio transmission")
    @app_commands.guild_only()
    async def stop_cmd(self, interaction: discord.Interaction):
        state = self.get_state(interaction.guild.id)
        state['queue'].clear()
        state['loop'] = 'off'
        vc = interaction.guild.voice_client
        if vc and (vc.is_playing() or vc.is_paused()):
            vc.stop()
            await self.bot.change_presence(activity=None)
            await interaction.response.send_message("📻 Radio transmission cut.")
        else:
            await interaction.response.send_message("The radio is currently broadcasting static.", ephemeral=True)

    @app_commands.command(name="volume", description="Adjusts the radio output volume (0 to 200)")
    @app_commands.guild_only()
    async def volume(self, interaction: discord.Interaction, level: int):
        vc = interaction.guild.voice_client
        if not vc or not vc.is_connected():
            return await interaction.response.send_message("Radio is disconnected.", ephemeral=True)
        if not 0 <= level <= 200:
            return await interaction.response.send_message("Volume must be between 0 and 200.", ephemeral=True)
        if vc.source:
            vc.source.volume = level / 100.0
            await interaction.response.send_message(f"🔊 Master output adjusted to **{level}%**.")
        else:
            await interaction.response.send_message("Nothing is currently transmitting.", ephemeral=True)

    @app_commands.command(name="save_flightplan", description="Saves the current queue to the database")
    @app_commands.guild_only()
    async def save_flightplan(self, interaction: discord.Interaction, plan_name: str):
        await interaction.response.defer()
        state = self.get_state(interaction.guild.id)
        tracks = ([state['current_track_info']] if state['current_track_info'] else []) + state['queue']
        if not tracks: return await interaction.followup.send("❌ No tracks to save!")
        
        try:
            async with self.apool.connection() as conn:
                async with conn.transaction():
                    await conn.execute(
                        "INSERT INTO radio_flightplans (guild_id, plan_name, tracks) VALUES (%s, %s, %s) ON CONFLICT (guild_id, plan_name) DO UPDATE SET tracks = EXCLUDED.tracks", 
                        (interaction.guild.id, plan_name.lower().strip(), json.dumps(tracks))
                    )
            await interaction.followup.send(f"💾 Saved '{plan_name}'.")
        except Exception as e:
            self.log.error(f"[Radio] Save Error: {e}")
            await interaction.followup.send("❌ DB Error.")

    @app_commands.command(name="load_flightplan", description="Loads a saved playlist")
    @app_commands.guild_only()
    async def load_flightplan(self, interaction: discord.Interaction, plan_name: str):
        await interaction.response.defer()
        try:
            async with self.apool.connection() as conn:
                cursor = await conn.execute("SELECT tracks FROM radio_flightplans WHERE guild_id = %s AND plan_name = %s", (interaction.guild.id, plan_name.lower().strip()))
                result = await cursor.fetchone()
                
            if not result: return await interaction.followup.send(f"❌ '{plan_name}' not found.")
                
            state = self.get_state(interaction.guild.id)
            state['channel'] = interaction.channel
            state['queue'].extend(json.loads(result[0]))
            
            if await self.ensure_voice(interaction):
                vc = interaction.guild.voice_client
                if not vc.is_playing() and not vc.is_paused(): await self.play_next_async(interaction.guild.id)
            await interaction.followup.send(f"📂 Loaded '{plan_name}'.")
        except Exception as e:
            self.log.error(f"[Radio] Load Error: {e}")
            await interaction.followup.send("❌ DB Error.")

    @app_commands.command(name="leave", description="Powers down the radio")
    @app_commands.guild_only()
    async def leave(self, interaction: discord.Interaction):
        state = self.get_state(interaction.guild.id)
        state['queue'].clear()
        state['loop'] = 'off'
        if interaction.guild.voice_client:
            await interaction.guild.voice_client.disconnect()
            await self.bot.change_presence(activity=None)
            await interaction.response.send_message("👋 Radio off.")
        else:
            await interaction.response.send_message("Not connected.", ephemeral=True)

async def setup(bot):
    await bot.add_cog(Radio(bot))