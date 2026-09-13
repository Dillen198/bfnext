import discord
from discord import app_commands, ui
from core import Plugin, utils


class AnnouncementEditModal(ui.Modal, title="Edit General Announcement"):
    """Modal to overwrite a previously posted general text announcement."""

    message_id = ui.TextInput(
        label="Target Message ID",
        placeholder="Paste Discord Message ID...",
        required=True,
        max_length=30
    )

    new_content = ui.TextInput(
        label="Updated Announcement Text",
        style=discord.TextStyle.paragraph,
        placeholder="Write updated text here...",
        required=True,
        max_length=2000
    )

    def __init__(self, plugin):
        super().__init__()
        self.plugin = plugin

    async def on_submit(self, interaction: discord.Interaction):
        await interaction.response.defer(ephemeral=True)

        try:
            target_msg_id = int(self.message_id.value.strip())
        except ValueError:
            await interaction.followup.send("Invalid Message ID format. Must be numeric numbers.", ephemeral=True)
            return

        target_message = None
        target_channel = None
        for channel in interaction.guild.text_channels:
            try:
                target_message = await channel.fetch_message(target_msg_id)
                target_channel = channel
                break
            except (discord.NotFound, discord.Forbidden):
                continue

        if not target_message or not target_channel:
            await interaction.followup.send("Could not find a message with that ID in any accessible channel.", ephemeral=True)
            return

        try:
            await target_message.edit(content=self.new_content.value)
            await interaction.followup.send(f"Successfully updated message in {target_channel.mention}!", ephemeral=True)
        except Exception as e:
            await interaction.followup.send(f"Failed to edit message: {e}", ephemeral=True)


class AnnouncementModal(ui.Modal, title="Create General Announcement"):
    """Modal dialog for drafting normal text general announcements."""

    content = ui.TextInput(
        label="Announcement Message",
        style=discord.TextStyle.paragraph,
        placeholder="Type announcement here (Markdown supported)...",
        required=True,
        max_length=2000
    )

    def __init__(self, plugin, channel: discord.TextChannel, role: discord.Role | None):
        super().__init__()
        self.plugin = plugin
        self.target_channel = channel
        self.target_role = role

    async def on_submit(self, interaction: discord.Interaction):
        await interaction.response.defer(ephemeral=True)

        role_mention = f"{self.target_role.mention}\n" if self.target_role else ""
        final_message = f"{role_mention}{self.content.value}"

        try:
            msg = await self.target_channel.send(content=final_message)

            # Auto-publish if posted in a Discord Announcement (News) channel type
            if self.target_channel.type == discord.ChannelType.news:
                await msg.publish()

            await interaction.followup.send(f"Announcement successfully posted in {self.target_channel.mention}!", ephemeral=True)
        except Exception as e:
            await interaction.followup.send(f"Failed to send announcement: {e}", ephemeral=True)


class Announcements(Plugin):
    """General Announcements Plugin for DCSServerBot supporting normal text messages, channel selection, role pings, and editing."""

    @app_commands.command(name="announce", description="Post a general text announcement with custom channel & role targets")
    @app_commands.guild_only()
    @utils.app_has_role("Admin")
    @app_commands.describe(
        channel="The target text channel to publish the announcement",
        ping_role="Optional role to ping with this announcement"
    )
    async def announce_cmd(
        self,
        interaction: discord.Interaction,
        channel: discord.TextChannel,
        ping_role: discord.Role | None = None
    ):
        modal = AnnouncementModal(self, channel, ping_role)
        await interaction.response.send_modal(modal)

    @app_commands.command(name="edit_announcement", description="Edit a previously posted general text announcement by ID")
    @app_commands.guild_only()
    @utils.app_has_role("Admin")
    async def edit_announcement_cmd(self, interaction: discord.Interaction):
        modal = AnnouncementEditModal(self)
        await interaction.response.send_modal(modal)


async def setup(bot):
    await bot.add_cog(Announcements(bot))