import io
import discord
from discord import app_commands, ui
from core import Plugin, utils


class TicketModal(ui.Modal, title="Create Support Ticket"):
    """Form modal for user ticket details."""

    subject = ui.TextInput(
        label="Ticket Subject",
        placeholder="Brief subject/title...",
        required=True,
        max_length=100
    )

    details = ui.TextInput(
        label="Detailed Description",
        style=discord.TextStyle.paragraph,
        placeholder="Describe your issue or request in detail...",
        required=True,
        max_length=1500
    )

    def __init__(self, plugin):
        super().__init__()
        self.plugin = plugin

    async def on_submit(self, interaction: discord.Interaction):
        await interaction.response.defer(ephemeral=True)

        guild = interaction.guild
        user = interaction.user
        config = self.plugin.get_config() or {}

        # Fetch configured forum channel ID
        forum_id_raw = config.get("forum_channel_id")
        if not forum_id_raw:
            await interaction.followup.send("`forum_channel_id` is missing in `tickets.yaml` config.", ephemeral=True)
            return

        try:
            forum_channel_id = int(forum_id_raw)
        except ValueError:
            await interaction.followup.send(f"Invalid forum channel ID format: `{forum_id_raw}`", ephemeral=True)
            return

        forum_channel = guild.get_channel(forum_channel_id)
        if not forum_channel or not isinstance(forum_channel, discord.ForumChannel):
            await interaction.followup.send("Configured forum channel not found or is not a Forum Channel.", ephemeral=True)
            return

        # 1. Post Intro Embed
        intro_embed = discord.Embed(
            title=f"🎫 Support Ticket: {self.subject.value}",
            description=(
                f"Welcome {user.mention}!\n\n"
                f"Our admin and support team have been notified. Please use this forum thread to upload "
                f"screenshots, track files, or discuss details."
            ),
            color=discord.Color.blue()
        )
        intro_embed.add_field(name="Submitted By", value=f"{user.mention} (`{user.id}`)", inline=True)
        intro_embed.add_field(name="Subject", value=self.subject.value, inline=True)
        intro_embed.add_field(name="Issue Details", value=self.details.value, inline=False)
        intro_embed.set_footer(text="DCSServerBot | Click 'Close Ticket' below when resolved.")

        # 2. Create Forum Thread
        thread_with_msg = await forum_channel.create_thread(
            name=f"[{user.name}] {self.subject.value}"[:100],
            embed=intro_embed,
            view=TicketControlView(self.plugin)
        )
        thread = thread_with_msg.thread

        # 3. Add user explicitly to the thread
        try:
            await thread.add_user(user)
        except Exception as e:
            self.plugin.log.error(f"[Tickets] Failed adding user {user.id} to thread {thread.id}: {e}")

        # 4. Save ticket record to PostgreSQL
        await self.plugin.create_ticket_record(user.id, thread.id, self.subject.value)

        await interaction.followup.send(f"Ticket created! Access your forum post here: {thread.mention}", ephemeral=True)


class TicketControlView(ui.View):
    """Action button attached to the ticket intro message inside the Forum thread."""

    def __init__(self, plugin):
        super().__init__(timeout=None)
        self.plugin = plugin

    @ui.button(label="🔒 Close Ticket & Generate Transcript", style=discord.ButtonStyle.red, custom_id="close_forum_ticket_btn")
    async def close_ticket_button(self, interaction: discord.Interaction, button: ui.Button):
        if not await self.plugin.can_close_ticket(interaction.channel.id, interaction.user):
            await interaction.response.send_message(
                "Only the ticket creator or an Admin can close this ticket.", ephemeral=True
            )
            return
        await interaction.response.defer()
        await self.plugin.close_ticket_thread(interaction.channel, interaction.user)


class TicketPanelView(ui.View):
    """Persistent Embed Button to launch Ticket Creation Modal."""

    def __init__(self, plugin):
        super().__init__(timeout=None)
        self.plugin = plugin

    @ui.button(label="📩 Open Support Ticket", style=discord.ButtonStyle.green, custom_id="open_ticket_modal_btn")
    async def open_ticket_button(self, interaction: discord.Interaction, button: ui.Button):
        modal = TicketModal(self.plugin)
        await interaction.response.send_modal(modal)


class Tickets(Plugin):
    """Custom Forum-Based Ticket System with Chat Transcript Exporter for DCSServerBot"""

    async def cog_load(self):
        await super().cog_load()
        # Register persistent views
        self.bot.add_view(TicketPanelView(self))
        self.bot.add_view(TicketControlView(self))
        self.bot.loop.create_task(self._startup_sync())

    async def _startup_sync(self):
        await self.bot.wait_until_ready()
        await self.init_db()

    async def init_db(self):
        """Ensure ticket logging table exists in DB."""
        try:
            async with self.apool.connection() as conn:
                async with conn.transaction():
                    await conn.execute("""
                        CREATE TABLE IF NOT EXISTS ticket_records (
                            ticket_id SERIAL PRIMARY KEY,
                            user_id BIGINT NOT NULL,
                            channel_id BIGINT UNIQUE NOT NULL,
                            subject TEXT NOT NULL,
                            status TEXT DEFAULT 'OPEN',
                            created_at TIMESTAMP WITH TIME ZONE DEFAULT NOW(),
                            closed_at TIMESTAMP WITH TIME ZONE
                        )
                    """)
            self.log.info("[Tickets] Database table 'ticket_records' verified.")
        except Exception as e:
            self.log.error(f"[Tickets] Failed to initialize database table: {e}", exc_info=True)

    async def get_ticket_owner(self, channel_id: int) -> int | None:
        """Fetch the original ticket creator's user ID for a thread, or None if unknown."""
        try:
            async with self.apool.connection() as conn:
                cursor = await conn.execute(
                    "SELECT user_id FROM ticket_records WHERE channel_id = %s", (channel_id,)
                )
                row = await cursor.fetchone()
                return row[0] if row else None
        except Exception as e:
            self.log.error(f"[Tickets] Error fetching ticket owner for channel {channel_id}: {e}", exc_info=True)
            return None

    async def can_close_ticket(self, channel_id: int, member: discord.Member) -> bool:
        """Only the ticket creator or an Admin may close/lock/archive a ticket."""
        if discord.utils.get(member.roles, name="Admin"):
            return True
        owner_id = await self.get_ticket_owner(channel_id)
        return owner_id is not None and owner_id == member.id

    async def create_ticket_record(self, user_id: int, channel_id: int, subject: str):
        """Insert new ticket entry into database."""
        try:
            async with self.apool.connection() as conn:
                async with conn.transaction():
                    await conn.execute("""
                        INSERT INTO ticket_records (user_id, channel_id, subject, status)
                        VALUES (%s, %s, %s, 'OPEN')
                    """, (user_id, channel_id, subject))
        except Exception as e:
            self.log.error(f"[Tickets] Error logging ticket record to DB: {e}", exc_info=True)

    async def generate_transcript(self, thread: discord.Thread) -> discord.File:
        """Fetch all messages in thread and compile into formatted .txt file."""
        lines = [
            f"==================================================",
            f"TICKET TRANSCRIPT: {thread.name}",
            f"THREAD ID: {thread.id}",
            f"EXPORTED AT: {discord.utils.utcnow().strftime('%Y-%m-%d %H:%M:%S UTC')}",
            f"==================================================\n"
        ]

        # Fetch messages in chronological order
        messages = [msg async for msg in thread.history(limit=None, oldest_first=True)]

        for msg in messages:
            timestamp = msg.created_at.strftime("%Y-%m-%d %H:%M:%S")
            author = f"{msg.author.name} ({msg.author.id})"
            content = msg.clean_content or "[No text content]"

            lines.append(f"[{timestamp}] {author}:\n{content}")

            if msg.attachments:
                attachments_list = ", ".join([att.url for att in msg.attachments])
                lines.append(f"  📎 Attachments: {attachments_list}")

            lines.append("-" * 40)

        transcript_text = "\n".join(lines)
        file_data = io.BytesIO(transcript_text.encode("utf-8"))
        filename = f"transcript-{thread.id}.txt"

        return discord.File(file_data, filename=filename)

    async def close_ticket_thread(self, thread: discord.Thread, closed_by: discord.User | discord.Member):
        """Generate transcript, log closing to DB, post transcript, lock thread."""
        try:
            await thread.send("⏳ Generating ticket transcript...")

            # 1. Build and attach .txt transcript
            transcript_file = await self.generate_transcript(thread)

            close_embed = discord.Embed(
                title="🔒 Ticket Closed",
                description=f"This ticket has been closed by {closed_by.mention}.\nAttached below is the complete chat transcript.",
                color=discord.Color.red()
            )
            await thread.send(embed=close_embed, file=transcript_file)

            # 2. Update PostgreSQL Status
            async with self.apool.connection() as conn:
                async with conn.transaction():
                    await conn.execute("""
                        UPDATE ticket_records 
                        SET status = 'CLOSED', closed_at = NOW() 
                        WHERE channel_id = %s
                    """, (thread.id,))

            # 3. Archive and Lock Thread
            await thread.edit(archived=True, locked=True)
            self.log.info(f"[Tickets] Closed, exported, and locked ticket thread {thread.name} ({thread.id}).")

        except Exception as e:
            self.log.error(f"[Tickets] Error closing ticket thread {thread.id}: {e}", exc_info=True)

    @app_commands.command(name="setup_ticket_panel", description="Post the support ticket button embed")
    @app_commands.guild_only()
    @utils.app_has_role("Admin")
    async def setup_ticket_panel(self, interaction: discord.Interaction):
        await interaction.response.defer(ephemeral=True)

        embed = discord.Embed(
            title="❓ Community Support Center",
            description=(
                "Need assistance or need to report an issue?\n\n"
                "Click **'📩 Open Support Ticket'** below to open a private forum post. "
                "Only you and server admins will have access to the post."
            ),
            color=discord.Color.dark_teal()
        )
        embed.set_footer(text="DCSServerBot | Support Ticket System")

        view = TicketPanelView(self)
        await interaction.channel.send(embed=embed, view=view)
        await interaction.followup.send("Support panel embed posted!", ephemeral=True)

    @app_commands.command(name="close_ticket", description="Close, export transcript, and lock ticket thread")
    @app_commands.guild_only()
    async def close_ticket_cmd(self, interaction: discord.Interaction):
        if not isinstance(interaction.channel, discord.Thread):
            await interaction.response.send_message("This command can only be run inside a ticket forum thread.", ephemeral=True)
            return

        if not await self.can_close_ticket(interaction.channel.id, interaction.user):
            await interaction.response.send_message(
                "Only the ticket creator or an Admin can close this ticket.", ephemeral=True
            )
            return

        await interaction.response.defer()
        await self.close_ticket_thread(interaction.channel, interaction.user)


async def setup(bot):
    await bot.add_cog(Tickets(bot))