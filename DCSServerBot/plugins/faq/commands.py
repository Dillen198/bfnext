import discord
from discord import app_commands, ui
from typing import Optional
from core import Plugin, utils, Server


class UnifiedFAQSelect(ui.Select):
    """
    Interactive dropdown menu combining dynamic live server telemetry 
    and Fowl Engine / Campaign FAQ topics loaded from faq.yaml.
    """

    def __init__(self, plugin: Plugin):
        self.plugin = plugin
        options = []

        # 1. Fetch Live DCS Servers directly from DCSServerBot core memory
        servers: dict[str, Server] = self.plugin.bot.servers
        for server_name, server_obj in servers.items():
            status_emoji = "🟢" if server_obj.status.name == "RUNNING" else "🔴"
            options.append(
                discord.SelectOption(
                    label=f"Server: {server_name}"[:100],
                    value=f"server:{server_name}",
                    description=f"Live IP, Port, Password & SRS ({server_obj.status.name})",
                    emoji=status_emoji
                )
            )

        # 2. Add Fowl Engine & General FAQ Topics from config/plugins/faq.yaml
        config = self.plugin.get_config() or {}
        for key, data in config.items():
            if isinstance(data, dict) and "title" in data:
                title = data.get("title", key)[:100]
                description = data.get("description", "Fowl Engine Help Topic")[:100]
                options.append(
                    discord.SelectOption(
                        label=title,
                        value=f"faq:{key}",
                        description=description,
                        emoji="❓"
                    )
                )

        if not options:
            options.append(discord.SelectOption(label="No topics available", value="none"))

        super().__init__(
            placeholder="Select a DCS server or Fowl Engine help topic...",
            min_values=1,
            max_values=1,
            options=options[:25],  # Discord limits select menus to 25 items max
            custom_id="unified_faq_select"
        )

    async def callback(self, interaction: discord.Interaction):
        selected_value = self.values[0]

        if selected_value == "none":
            await interaction.response.send_message("No topics available.", ephemeral=True)
            return

        prefix, key = selected_value.split(":", 1)

        if prefix == "server":
            embed = self.plugin.build_dynamic_server_embed(key)
        else:
            embed = self.plugin.build_faq_embed(key)

        if embed:
            await interaction.response.send_message(embed=embed, ephemeral=True)
        else:
            await interaction.response.send_message("Failed to fetch information for the selected topic.", ephemeral=True)


class UnifiedFAQPanelView(ui.View):
    """Persistent View Container for the drop-down menu."""

    def __init__(self, plugin: Plugin):
        super().__init__(timeout=None)
        self.add_item(UnifiedFAQSelect(plugin))


class FAQ(Plugin):
    """DCSServerBot Plugin providing Live Server Specs and Fowl Engine Documentation."""

    async def cog_load(self):
        await super().cog_load()
        # Register persistent view so dropdown works across bot reboots
        self.bot.add_view(UnifiedFAQPanelView(self))

    def build_dynamic_server_embed(self, server_name: str) -> Optional[discord.Embed]:
        """Query live connection parameters from DCSServerBot core."""
        server = self.bot.servers.get(server_name)
        if not server:
            return None

        # Resolve IP and connection details
        ip = getattr(server.node, 'public_ip', None) or server.node.listen_address
        port = server.port
        password = server.settings.get('password', None) or "*None (Open Server)*"

        # Resolve SRS details
        srs_config = server.extensions.get('SRS')
        srs_port = getattr(srs_config, 'port', 5002) if srs_config else None
        srs_info = f"`{ip}:{srs_port}`" if srs_config else "Disabled / Not Loaded"

        # Resolve mission and player stats
        current_mission = server.current_mission.name if server.current_mission else "No Mission Loaded"
        active_players = len(server.get_active_players())
        max_players = server.settings.get('maxPlayers', 'N/A')
        player_count = f"{active_players} / {max_players}"

        embed = discord.Embed(
            title=f"🎮 Live Connection Info: {server.display_name}",
            color=discord.Color.green() if server.status.name == "RUNNING" else discord.Color.red()
        )
        embed.add_field(name="🌐 Server IP & Port", value=f"`{ip}:{port}`", inline=True)
        embed.add_field(name="🔑 Join Password", value=f"`{password}`", inline=True)
        embed.add_field(name="📻 SRS Address", value=srs_info, inline=True)
        embed.add_field(name="🗺️ Current Mission", value=f"`{current_mission}`", inline=False)
        embed.add_field(name="👥 Active Players", value=player_count, inline=True)
        embed.add_field(name="⚡ Server Status", value=f"`{server.status.name}`", inline=True)
        embed.set_footer(text="DCSServerBot | Live Telemetry")
        return embed

    def build_faq_embed(self, topic_key: str) -> Optional[discord.Embed]:
        """Parse static answers from config/plugins/faq.yaml."""
        config = self.get_config() or {}
        faq_data = config.get(topic_key)

        if not faq_data or not isinstance(faq_data, dict):
            return None

        embed = discord.Embed(
            title=faq_data.get("title", "FAQ Topic"),
            description=faq_data.get("description", ""),
            color=discord.Color.blue()
        )
        for field in faq_data.get("fields", []):
            embed.add_field(
                name=field.get("name", "Info"),
                value=field.get("value", "N/A"),
                inline=field.get("inline", False)
            )
        embed.set_footer(text="DCSServerBot | Fowl Engine Knowledge Base")
        return embed

    @app_commands.command(name="setup_faq_panel", description="Post the unified server info and campaign FAQ panel")
    @app_commands.guild_only()
    @utils.app_has_role("Admin")
    async def setup_faq_panel(self, interaction: discord.Interaction):
        await interaction.response.defer(ephemeral=True)

        panel_embed = discord.Embed(
            title="📚 Operation Fowl Intent - Information Hub",
            description=(
                "Select an option from the menu below to query **Live IP, Passwords, SRS info**, "
                "or read the **Fowl Engine Campaign Rules & Logistics Guides**."
            ),
            color=discord.Color.dark_blue()
        )
        panel_embed.set_footer(text="DCSServerBot | Select a topic below to view details")

        view = UnifiedFAQPanelView(self)
        await interaction.channel.send(embed=panel_embed, view=view)
        await interaction.followup.send("Unified FAQ panel successfully posted to the channel!", ephemeral=True)


async def setup(bot):
    await bot.add_cog(FAQ(bot))