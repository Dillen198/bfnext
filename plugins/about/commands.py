import discord
from discord import app_commands
from core import Plugin, utils


class About(Plugin):
    """About Us & Credits Plugin with current channel setup and on-demand command."""

    def build_about_embeds(self):
        config = self.get_config() or {}
        website = config.get("website", "https://yourcommunitywebsite.com")

        # --- Embed 1: About Us & Team ---
        about_data = config.get("about_us", {})
        embed1 = discord.Embed(
            title=about_data.get("title", "About Us"),
            description=about_data.get("description", ""),
            color=discord.Color.blue()
        )
        team_members = about_data.get("team", [])
        team_text = "\n".join([f"• **{m['name']}:** {m['role']}" for m in team_members])
        if team_text:
            embed1.add_field(name="🛠️ Core Team & Responsibilities", value=team_text, inline=False)

        # --- Embed 2: About the Mission ---
        mission_data = config.get("mission", {})
        embed2 = discord.Embed(
            title=mission_data.get("title", "About Mission"),
            description=mission_data.get("description", ""),
            color=discord.Color.dark_blue()
        )
        features = mission_data.get("features", [])
        features_text = "\n".join([f"• {f}" for f in features])
        if features_text:
            embed2.add_field(name="⚔️ Key Features", value=features_text, inline=False)

        # --- Embed 3: Credit for Eagle Dynamics (ED) ---
        ed_data = config.get("credits_ed", {})
        embed3 = discord.Embed(
            title=ed_data.get("title", "Eagle Dynamics"),
            description=ed_data.get("description", ""),
            color=discord.Color.green()
        )

        # --- Embed 4: Credit for Eric Stokes (original Fowl Engine author) ---
        estokes_data = config.get("credits_estokes", {})
        embed4 = discord.Embed(
            title=estokes_data.get("title", "Eric Stokes"),
            description=estokes_data.get("description", ""),
            color=discord.Color.orange()
        )

        # --- Embed 5: Credit for Hacker (server host, 61st ATP) ---
        hacker_data = config.get("credits_hacker", {})
        embed5 = discord.Embed(
            title=hacker_data.get("title", "Hacker"),
            description=hacker_data.get("description", ""),
            color=discord.Color.dark_gold()
        )

        # --- Embed 6: Credit for Special K ---
        sk_data = config.get("credits_special_k", {})
        embed6 = discord.Embed(
            title=sk_data.get("title", "Special K"),
            description=sk_data.get("description", ""),
            color=discord.Color.gold()
        )
        embed6.add_field(
            name="🌐 Official Portal",
            value=f"• **Website/Links:** [Visit Here]({website})",
            inline=False
        )
        embed6.set_footer(text="Vector Strike | Powered by DCSServerBot & Fowl Engine")

        return [embed1, embed2, embed3, embed4, embed5, embed6]

    @app_commands.command(name="about", description="Learn about Vector Strike, our team, mission design, and project credits")
    @app_commands.guild_only()
    async def about_cmd(self, interaction: discord.Interaction):
        embeds = self.build_about_embeds()
        await interaction.response.send_message(embeds=embeds, ephemeral=False)

    @app_commands.command(name="setup_about", description="Deploy the permanent About Us & Credits panel to the current channel")
    @app_commands.guild_only()
    @utils.app_has_role("Admin")
    async def setup_about_cmd(self, interaction: discord.Interaction):
        await interaction.response.defer(ephemeral=True)
        
        embeds = self.build_about_embeds()
        try:
            # Posts directly into the channel where the command was executed
            await interaction.channel.send(embeds=embeds)
            await interaction.followup.send("Successfully posted the About panel in this channel!", ephemeral=True)
        except Exception as e:
            await interaction.followup.send(f"Failed to post panel: {e}", ephemeral=True)


async def setup(bot):
    await bot.add_cog(About(bot))