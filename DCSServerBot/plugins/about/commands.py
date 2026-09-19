import discord
from discord import app_commands, ui
from core import Plugin, utils

# Matches the Vector Strike accent the fowlengine plugin's embeds use, so
# /about doesn't look like it came from a different bot than /feops and the
# server-info panels.
BRAND_COLOR = discord.Color.from_str('#c8102e')


class LinkView(ui.View):
    """Link buttons for the About panel.

    Link-style buttons carry no custom_id and fire no interaction, so this View
    keeps working across bot restarts without persistent-view registration --
    which matters for `/setup_about`, whose message is meant to sit in a channel
    indefinitely.
    """

    def __init__(self, links: list[dict]):
        super().__init__(timeout=None)
        for link in links[:5]:  # one action row
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


class About(Plugin):
    """About Us & Credits: an on-demand /about and a permanent channel panel."""

    # -- shared branding ----------------------------------------------------

    def _cfg(self) -> dict:
        return self.get_config() or {}

    def _logo_url(self):
        """The brand mark, served from the dashboard origin rather than uploaded
        as an attachment, so a panel message can be edited later without the
        image disappearing."""
        cfg = self._cfg()
        explicit = (cfg.get("logo_url") or "").strip()
        if explicit:
            return explicit
        dash = (cfg.get("dashboard_url") or "").rstrip("/")
        return f"{dash}/vs-vectorstrike_hd-white.png" if dash else None

    def _embed(self, title: str, description: str = "") -> discord.Embed:
        cfg = self._cfg()
        brand = cfg.get("brand_name", "Vector Strike")
        website = (cfg.get("website") or "").strip() or None
        color = cfg.get("color")
        embed = discord.Embed(
            title=title,
            description=description,
            color=discord.Color.from_str(color) if color else BRAND_COLOR,
        )
        embed.set_author(name=brand, icon_url=self._logo_url(), url=website)
        return embed

    # -- embed builders -----------------------------------------------------

    def _team_embed(self) -> discord.Embed:
        """Who we are. Team members become inline fields so they lay out as a
        grid rather than the single bulleted paragraph this used to be."""
        data = self._cfg().get("about_us", {})
        embed = self._embed(data.get("title", "About Us"), data.get("description", ""))
        logo = self._logo_url()
        if logo:
            embed.set_thumbnail(url=logo)
        for member in data.get("team", []):
            embed.add_field(
                name=member.get("name", "?"),
                value=member.get("role", ""),
                inline=True,
            )
        return embed

    def _mission_embed(self) -> discord.Embed:
        """What the campaign is. `features` accepts either plain strings (the
        old format, rendered as one bulleted field) or {name, value} pairs,
        which lay out as a two-column grid."""
        data = self._cfg().get("mission", {})
        embed = self._embed(data.get("title", "The Campaign"), data.get("description", ""))
        features = data.get("features", [])
        for feature in features:
            if isinstance(feature, dict):
                embed.add_field(
                    name=feature.get("name", "Feature"),
                    value=feature.get("value", ""),
                    inline=feature.get("inline", True),
                )
        bullets = [f for f in features if isinstance(f, str)]
        if bullets:
            embed.add_field(
                name="Key Features",
                value="\n".join(f"- {b}" for b in bullets),
                inline=False,
            )
        return embed

    def _credits_embed(self) -> discord.Embed:
        """One embed for every acknowledgement, rather than one embed each.

        Reads the `credits:` block when present; otherwise falls back to the
        legacy `credits_<name>` keys so an un-migrated about.yaml still renders.
        """
        cfg = self._cfg()
        data = cfg.get("credits", {})
        embed = self._embed(
            data.get("title", "Credits & Thanks"),
            data.get("description", ""),
        )
        entries = data.get("entries")
        if entries is None:
            entries = [
                {"name": cfg[key].get("title", key), "value": cfg[key].get("description", "")}
                for key in ("credits_ed", "credits_estokes", "credits_hacker", "credits_special_k")
                if isinstance(cfg.get(key), dict)
            ]
        for entry in entries:
            embed.add_field(
                name=entry.get("name", "Thanks"),
                value=entry.get("value", ""),
                inline=entry.get("inline", False),
            )
        return embed

    def build_about_embeds(self) -> list[discord.Embed]:
        embeds = [self._team_embed(), self._mission_embed(), self._credits_embed()]
        # Only the last embed carries a footer, so the three read as one panel
        # instead of three repeats of the same line.
        brand = self._cfg().get("brand_name", "Vector Strike")
        embeds[-1].set_footer(
            text=f"{brand} - powered by DCSServerBot & Fowl Engine",
            icon_url=self._logo_url(),
        )
        return embeds

    def build_link_view(self):
        """Website / wiki / dashboard as real buttons, instead of URLs buried in
        the body of the last embed, which is where they used to live."""
        cfg = self._cfg()
        links = cfg.get("links")
        if links is None:
            # Synthesize the obvious three from the individual settings.
            links = [
                {"label": "Website", "url": cfg.get("website", ""), "emoji": "\N{GLOBE WITH MERIDIANS}"},
                {"label": "Wiki", "url": cfg.get("wiki_url", ""), "emoji": "\N{OPEN BOOK}"},
                {"label": "Dashboard", "url": cfg.get("dashboard_url", ""), "emoji": "\N{BAR CHART}"},
            ]
        view = LinkView(links)
        return view if view.has_buttons else None

    # -- commands -----------------------------------------------------------

    @app_commands.command(name="about", description="About Vector Strike - the team, the campaign, and credits")
    @app_commands.guild_only()
    async def about_cmd(self, interaction: discord.Interaction):
        view = self.build_link_view()
        await interaction.response.send_message(
            embeds=self.build_about_embeds(),
            view=view or discord.utils.MISSING,
        )

    @app_commands.command(name="setup_about", description="Post the permanent About & Credits panel in this channel")
    @app_commands.guild_only()
    @utils.app_has_role("Admin")
    async def setup_about_cmd(self, interaction: discord.Interaction):
        await interaction.response.defer(ephemeral=True)
        try:
            view = self.build_link_view()
            await interaction.channel.send(
                embeds=self.build_about_embeds(),
                view=view or discord.utils.MISSING,
            )
        except Exception as ex:
            await interaction.followup.send(f"Failed to post the panel: {ex}", ephemeral=True)
            return
        await interaction.followup.send("Posted the About panel in this channel.", ephemeral=True)


async def setup(bot):
    await bot.add_cog(About(bot))
