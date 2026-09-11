import discord
from discord import app_commands, ui
from typing import Optional
from core import Plugin, utils, Server

# Same accent as the about / fowlengine embeds.
BRAND_COLOR = discord.Color.from_str('#c8102e')

# Discord hard-caps a select menu at 25 options. Servers are listed first, but
# capping them here means a node with many servers can never push the campaign
# help topics off the end of the menu -- which is what `options[:25]` used to do
# silently.
MAX_OPTIONS = 25
MAX_SERVER_OPTIONS = 8

# Config keys that are settings rather than help topics. Everything else in the
# plugin config that is a mapping with a `title` becomes a dropdown entry, so a
# new topic needs no code change -- but `panel` also has a title, and without
# this it would show up in its own menu.
RESERVED_KEYS = {"panel", "links"}


class UnifiedFAQSelect(ui.Select):
    """Live server telemetry and Fowl Engine help topics in one dropdown."""

    def __init__(self, plugin: Plugin):
        self.plugin = plugin
        config = plugin.get_config() or {}

        server_options = []
        servers: dict[str, Server] = self.plugin.bot.servers
        for server_name, server_obj in servers.items():
            running = server_obj.status.name == "RUNNING"
            server_options.append(
                discord.SelectOption(
                    label=f"Server: {server_name}"[:100],
                    value=f"server:{server_name}",
                    description=f"IP, port, password and SRS ({server_obj.status.name})"[:100],
                    emoji="\N{LARGE GREEN CIRCLE}" if running else "\N{LARGE RED CIRCLE}",
                )
            )

        topic_options = []
        for key, data in config.items():
            if key in RESERVED_KEYS or not isinstance(data, dict) or "title" not in data:
                continue  # plain settings (wiki_url, panel, links, ...) are not topics
            topic_options.append(
                discord.SelectOption(
                    label=data.get("title", key)[:100],
                    value=f"faq:{key}",
                    description=data.get("description", "Campaign help topic")[:100],
                    emoji=data.get("emoji") or "\N{BLACK QUESTION MARK ORNAMENT}",
                )
            )

        # Topics are the part that must never be truncated -- they're the whole
        # point of the panel, and unlike the servers they have no other home in
        # Discord.
        room_for_servers = max(0, MAX_OPTIONS - len(topic_options))
        server_options = server_options[:min(MAX_SERVER_OPTIONS, room_for_servers)]
        options = (server_options + topic_options)[:MAX_OPTIONS]

        if not options:
            options.append(discord.SelectOption(label="No topics available", value="none"))

        super().__init__(
            placeholder="Pick a server for connection info, or a help topic...",
            min_values=1,
            max_values=1,
            options=options,
            custom_id="unified_faq_select",
        )

    async def callback(self, interaction: discord.Interaction):
        selected = self.values[0]
        if selected == "none":
            await interaction.response.send_message("No topics available.", ephemeral=True)
            return

        prefix, key = selected.split(":", 1)
        if prefix == "server":
            embed = self.plugin.build_dynamic_server_embed(key)
        else:
            embed = self.plugin.build_faq_embed(key)

        if embed:
            await interaction.response.send_message(
                embed=embed, view=self.plugin.build_link_view() or discord.utils.MISSING,
                ephemeral=True)
        else:
            await interaction.response.send_message(
                "Could not fetch that topic.", ephemeral=True)


class UnifiedFAQPanelView(ui.View):
    """Persistent container for the dropdown."""

    def __init__(self, plugin: Plugin):
        super().__init__(timeout=None)
        self.add_item(UnifiedFAQSelect(plugin))


class LinkView(ui.View):
    """Link-style buttons: no custom_id, so no persistence registration needed."""

    def __init__(self, links: list[dict]):
        super().__init__(timeout=None)
        for link in links[:5]:
            url = (link.get("url") or "").strip()
            if not url.startswith(("http://", "https://")):
                continue
            self.add_item(ui.Button(
                label=link.get("label", "Open")[:80],
                url=url,
                emoji=link.get("emoji") or None,
                style=discord.ButtonStyle.link,
            ))

    @property
    def has_buttons(self) -> bool:
        return bool(self.children)


class FAQ(Plugin):
    """Live server specs and Fowl Engine campaign documentation."""

    async def cog_load(self):
        await super().cog_load()
        # Register the persistent view so the dropdown survives bot restarts.
        self.bot.add_view(UnifiedFAQPanelView(self))

    # -- shared branding ----------------------------------------------------

    def _cfg(self) -> dict:
        return self.get_config() or {}

    def _logo_url(self):
        cfg = self._cfg()
        explicit = (cfg.get("logo_url") or "").strip()
        if explicit:
            return explicit
        dash = (cfg.get("dashboard_url") or "").rstrip("/")
        return f"{dash}/vs-vectorstrike_hd-white.png" if dash else None

    def _embed(self, title: str, description: str = "", *,
               color: Optional[discord.Color] = None,
               url: Optional[str] = None) -> discord.Embed:
        cfg = self._cfg()
        brand = cfg.get("brand_name", "Vector Strike")
        embed = discord.Embed(title=title, description=description,
                              color=color or BRAND_COLOR)
        if url:
            embed.url = url
        embed.set_author(name=brand, icon_url=self._logo_url(),
                         url=(cfg.get("website") or "").strip() or None)
        return embed

    def build_link_view(self):
        cfg = self._cfg()
        links = cfg.get("links")
        if links is None:
            links = [
                {"label": "Full Wiki", "url": cfg.get("wiki_url", ""), "emoji": "\N{OPEN BOOK}"},
                {"label": "Dashboard", "url": cfg.get("dashboard_url", ""), "emoji": "\N{BAR CHART}"},
            ]
        view = LinkView(links)
        return view if view.has_buttons else None

    # -- embeds -------------------------------------------------------------

    def build_dynamic_server_embed(self, server_name: str) -> Optional[discord.Embed]:
        """Live connection parameters, straight from DCSServerBot core."""
        server = self.bot.servers.get(server_name)
        if not server:
            return None

        ip = getattr(server.node, 'public_ip', None) or server.node.listen_address
        port = server.port
        password = server.settings.get('password', None) or "*None (open server)*"

        srs_config = server.extensions.get('SRS')
        srs_port = getattr(srs_config, 'port', 5002) if srs_config else None
        srs_info = f"`{ip}:{srs_port}`" if srs_config else "Disabled / not loaded"

        current_mission = server.current_mission.name if server.current_mission else "No mission loaded"
        active_players = len(server.get_active_players())
        max_players = server.settings.get('maxPlayers', 'N/A')
        running = server.status.name == "RUNNING"

        embed = self._embed(
            f"Connection Info: {server.display_name}",
            color=discord.Color.green() if running else discord.Color.red(),
        )
        embed.add_field(name="Server IP & Port", value=f"`{ip}:{port}`", inline=True)
        embed.add_field(name="Join Password", value=f"`{password}`", inline=True)
        embed.add_field(name="SRS Address", value=srs_info, inline=True)
        embed.add_field(name="Current Mission", value=f"`{current_mission}`", inline=False)
        embed.add_field(name="Players", value=f"{active_players} / {max_players}", inline=True)
        embed.add_field(name="Status", value=f"`{server.status.name}`", inline=True)
        embed.set_footer(text="Live telemetry", icon_url=self._logo_url())
        return embed

    def build_faq_embed(self, topic_key: str) -> Optional[discord.Embed]:
        """Render one static topic from faq.yaml.

        A topic may carry a `url` pointing at the wiki page that covers the same
        ground in full; it becomes the embed's title link, because the FAQ can
        only ever be the short version.
        """
        cfg = self._cfg()
        faq_data = cfg.get(topic_key)
        if topic_key in RESERVED_KEYS or not isinstance(faq_data, dict):
            return None

        embed = self._embed(
            faq_data.get("title", "FAQ"),
            faq_data.get("description", ""),
            url=faq_data.get("url"),
        )
        for field in faq_data.get("fields", []):
            embed.add_field(
                name=field.get("name", "Info"),
                value=field.get("value", "N/A"),
                inline=field.get("inline", False),
            )
        wiki = (faq_data.get("url") or cfg.get("wiki_url") or "").strip()
        embed.set_footer(
            text="The wiki has the long version" if wiki else "Fowl Engine knowledge base",
            icon_url=self._logo_url(),
        )
        return embed

    # -- commands -----------------------------------------------------------

    @app_commands.command(name="faq", description="Server connection info and campaign help topics")
    @app_commands.guild_only()
    async def faq_cmd(self, interaction: discord.Interaction):
        """The panel on demand, private to the caller -- so a player can look
        something up without waiting for an admin to have posted the panel, and
        without spamming the channel."""
        await interaction.response.send_message(
            embed=self._panel_embed(), view=UnifiedFAQPanelView(self), ephemeral=True)

    def _panel_embed(self) -> discord.Embed:
        cfg = self._cfg()
        panel = cfg.get("panel", {})
        embed = self._embed(
            panel.get("title", "Information Hub"),
            panel.get("description",
                      "Pick a server below for live IP, password and SRS details, "
                      "or a topic for how the campaign actually works."),
        )
        wiki = (cfg.get("wiki_url") or "").strip()
        if wiki:
            embed.add_field(
                name="Everything else",
                value=f"The full player wiki lives at {wiki} - menus, logistics, "
                      f"capturing, tasking, CSAR, navaids and the rest.",
                inline=False,
            )
        embed.set_footer(text="Select a topic below", icon_url=self._logo_url())
        return embed

    @app_commands.command(name="setup_faq_panel", description="Post the server info and campaign FAQ panel")
    @app_commands.guild_only()
    @utils.app_has_role("Admin")
    async def setup_faq_panel(self, interaction: discord.Interaction):
        await interaction.response.defer(ephemeral=True)
        try:
            await interaction.channel.send(
                embed=self._panel_embed(), view=UnifiedFAQPanelView(self))
        except Exception as ex:
            await interaction.followup.send(f"Failed to post the panel: {ex}", ephemeral=True)
            return
        await interaction.followup.send("Posted the FAQ panel in this channel.", ephemeral=True)


async def setup(bot):
    await bot.add_cog(FAQ(bot))
