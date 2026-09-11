import discord
from discord import app_commands, ui
from core import Plugin, utils


class EditRulesModal(ui.Modal, title="Edit Community Rules"):
    """Interactive Modal Form for Admins to Edit Rules."""

    rules_content = ui.TextInput(
        label="Rules Content (Plain Text / Markdown)",
        style=discord.TextStyle.paragraph,
        placeholder="Enter plain text or markdown rules here...",
        required=True,
        max_length=4000
    )

    def __init__(self, plugin, current_text: str):
        super().__init__()
        self.plugin = plugin
        self.rules_content.default = (current_text or "")[:4000]

    async def on_submit(self, interaction: discord.Interaction):
        await interaction.response.defer(ephemeral=True)

        config = self.plugin.get_config() or {}
        channel_id_raw = config.get("channel")

        if not channel_id_raw:
            await interaction.followup.send("No `channel` configured in rules config.", ephemeral=True)
            return

        try:
            channel_id = int(channel_id_raw)
        except ValueError:
            await interaction.followup.send(f"Invalid channel ID format: `{channel_id_raw}`", ephemeral=True)
            return

        # Perform the rules update using content submitted via the modal
        await self.plugin.update_community_rules(override_text=self.rules_content.value)
        await interaction.followup.send(
            "Rules updated and saved. This text now overrides `rules.yaml` until an admin "
            "runs `/post_rules reset:True`.", ephemeral=True)


class Rules(Plugin):
    """Custom Rules Plugin with PostgreSQL Array Support & Automated Column Migration"""

    async def cog_load(self):
        await super().cog_load()
        # Ensure bot connection is established before starting task
        self.bot.loop.create_task(self._startup_sync())

    async def _startup_sync(self):
        await self.bot.wait_until_ready()
        await self.update_community_rules()

    async def init_db(self):
        """Ensure rules table exists and automatically migrate single message_id to message_ids BIGINT[]."""
        try:
            async with self.apool.connection() as conn:
                async with conn.transaction():
                    # 1. Create table with array support if it does not exist
                    await conn.execute("""
                        CREATE TABLE IF NOT EXISTS rules_messages (
                            channel_id BIGINT PRIMARY KEY,
                            message_ids BIGINT[] NOT NULL,
                            updated_at TIMESTAMP WITH TIME ZONE DEFAULT NOW()
                        )
                    """)

                    # 2. Automated DB Migration: Migrate old single message_id column to message_ids BIGINT[]
                    await conn.execute("""
                        DO $$ 
                        BEGIN 
                            IF EXISTS (
                                SELECT 1 
                                FROM information_schema.columns 
                                WHERE table_name = 'rules_messages' AND column_name = 'message_id'
                            ) THEN
                                ALTER TABLE rules_messages 
                                ADD COLUMN IF NOT EXISTS message_ids BIGINT[];

                                UPDATE rules_messages 
                                SET message_ids = ARRAY[message_id] 
                                WHERE message_id IS NOT NULL;

                                ALTER TABLE rules_messages 
                                DROP COLUMN message_id;

                                ALTER TABLE rules_messages 
                                ALTER COLUMN message_ids SET NOT NULL;
                            END IF;
                        END $$;
                    """)

                    # 3. Live edits made through /edit_rules are stored here.
                    # Without this they lived only in the posted message, and
                    # the next restart's startup sync silently reverted them to
                    # whatever rules.yaml said.
                    await conn.execute("""
                        ALTER TABLE rules_messages
                        ADD COLUMN IF NOT EXISTS override_text TEXT
                    """)
            self.log.info("[Rules] Database table 'rules_messages' verified and migrated.")
        except Exception as e:
            self.log.error(f"[Rules] Failed to initialize/migrate database table: {e}", exc_info=True)

    async def get_stored_message_ids(self, channel_id: int) -> list[int]:
        """Fetch all stored message IDs for a channel from DB."""
        try:
            async with self.apool.connection() as conn:
                cursor = await conn.execute(
                    "SELECT message_ids FROM rules_messages WHERE channel_id = %s",
                    (channel_id,)
                )
                row = await cursor.fetchone()
                if row and row[0]:
                    return list(row[0])
                return []
        except Exception as e:
            self.log.error(f"[Rules] Error fetching message IDs from DB: {e}", exc_info=True)
            return []

    async def save_message_ids(self, channel_id: int, message_ids: list[int]):
        """Insert or update channel message IDs array in DB.

        Deliberately names its columns so it can't clobber `override_text`.
        """
        try:
            async with self.apool.connection() as conn:
                async with conn.transaction():
                    await conn.execute("""
                        INSERT INTO rules_messages (channel_id, message_ids, updated_at)
                        VALUES (%s, %s, NOW())
                        ON CONFLICT (channel_id)
                        DO UPDATE SET message_ids = EXCLUDED.message_ids, updated_at = NOW()
                    """, (channel_id, message_ids))
            self.log.info(f"[Rules] Saved message_ids {message_ids} for channel {channel_id} to DB.")
        except Exception as e:
            self.log.error(f"[Rules] Error saving message IDs to DB: {e}", exc_info=True)

    async def get_override_text(self, channel_id: int):
        """The live text an admin last submitted through /edit_rules, if any."""
        try:
            async with self.apool.connection() as conn:
                cursor = await conn.execute(
                    "SELECT override_text FROM rules_messages WHERE channel_id = %s",
                    (channel_id,)
                )
                row = await cursor.fetchone()
                return row[0] if row and row[0] else None
        except Exception as e:
            self.log.error(f"[Rules] Error fetching override text from DB: {e}", exc_info=True)
            return None

    async def save_override_text(self, channel_id: int, text):
        """Persist (or with text=None, clear) the live override."""
        try:
            async with self.apool.connection() as conn:
                async with conn.transaction():
                    await conn.execute("""
                        INSERT INTO rules_messages (channel_id, message_ids, override_text, updated_at)
                        VALUES (%s, ARRAY[]::BIGINT[], %s, NOW())
                        ON CONFLICT (channel_id)
                        DO UPDATE SET override_text = EXCLUDED.override_text, updated_at = NOW()
                    """, (channel_id, text))
            self.log.info(
                f"[Rules] {'Cleared' if text is None else 'Stored'} live rules override "
                f"for channel {channel_id}.")
        except Exception as e:
            self.log.error(f"[Rules] Error saving override text to DB: {e}", exc_info=True)

    @staticmethod
    def build_image_embed(image_url: str | None) -> discord.Embed | None:
        if image_url and isinstance(image_url, str) and image_url.strip():
            return discord.Embed().set_image(url=image_url.strip())
        return None

    @staticmethod
    def chunk_text(text: str, limit: int = 1900) -> list[str]:
        if len(text) <= limit:
            return [text]

        chunks = []
        lines = text.split("\n")
        current_chunk = ""

        for line in lines:
            if len(current_chunk) + len(line) + 1 > limit:
                chunks.append(current_chunk.strip())
                current_chunk = line + "\n"
            else:
                current_chunk += line + "\n"

        if current_chunk.strip():
            chunks.append(current_chunk.strip())

        return chunks

    def format_rules_text(self, config: dict) -> str:
        title = config.get("title", "📜 **COMMUNITY RULES**")
        description = config.get("description", "")

        lines = [f"{title}\n", f"{description}\n", "---"] if description else [f"{title}\n", "---"]

        # A rule may carry a `section`; the first rule of each one gets a
        # heading above it. Rules without a section just continue the previous
        # one, so the old flat list still formats exactly as it used to.
        current_section = None
        for rule in config.get("rules", []):
            section = rule.get("section")
            if section and section != current_section:
                lines.append(f"\n## {section}")
                current_section = section
            t = rule.get("title", "")
            d = rule.get("description", "")
            lines.append(f"\n### {t}\n{d}")

        return "\n".join(lines)

    async def update_community_rules(self, override_text: str | None = None,
                                     ignore_override: bool = False):
        await self.bot.wait_until_ready()
        await self.init_db()

        config = self.get_config() or {}
        raw_channel_id = config.get("channel")

        if not raw_channel_id:
            self.log.warning("[Rules] No 'channel' key defined in config.")
            return

        try:
            channel_id = int(raw_channel_id)
        except ValueError:
            self.log.error(f"[Rules] Invalid channel ID '{raw_channel_id}' in config.")
            return

        channel = self.bot.get_channel(channel_id)
        if not channel:
            try:
                channel = await self.bot.fetch_channel(channel_id)
            except Exception as e:
                self.log.error(f"[Rules] Could not access channel {channel_id}: {e}")
                return

        # Precedence: an override passed in right now (a fresh /edit_rules
        # submission, which we also persist) > an override stored from a
        # previous one > rules.yaml. Without the stored tier, every restart's
        # startup sync quietly reverted an admin's live edit.
        if override_text is not None:
            await self.save_override_text(channel_id, override_text)
            text_content = override_text
        else:
            stored = None if ignore_override else await self.get_override_text(channel_id)
            if ignore_override:
                await self.save_override_text(channel_id, None)
            text_content = stored if stored is not None else self.format_rules_text(config)
        chunks = self.chunk_text(text_content)
        image_embed = self.build_image_embed(config.get("image_url"))

        stored_ids = await self.get_stored_message_ids(channel_id)

        # 1. EDIT IN PLACE: If stored message count matches new chunk count exactly
        if stored_ids and len(stored_ids) == len(chunks):
            try:
                for i, (msg_id, chunk) in enumerate(zip(stored_ids, chunks)):
                    msg = await channel.fetch_message(msg_id)
                    embed_to_send = image_embed if i == 0 else None
                    await msg.edit(content=chunk, embed=embed_to_send)

                self.log.info(f"[Rules] Successfully EDITED all {len(stored_ids)} rules messages in #{channel.name}")
                return
            except discord.NotFound:
                self.log.warning("[Rules] One or more stored messages were deleted on Discord. Cleaning and re-posting...")
            except discord.HTTPException as e:
                self.log.error(f"[Rules] Discord API error while editing messages: {e}. Aborting auto-update.")
                return

        # 2. PURGE OLD MESSAGES: Delete all old stored chunks if chunk count changed or edit failed
        if stored_ids:
            for msg_id in stored_ids:
                try:
                    old_msg = await channel.fetch_message(msg_id)
                    await old_msg.delete()
                except (discord.NotFound, discord.HTTPException):
                    pass

        # 3. POST NEW CHUNKS: Send fresh messages and write all generated IDs to DB
        posted_ids = []
        for i, chunk in enumerate(chunks):
            embed_to_send = image_embed if i == 0 else None
            new_msg = await channel.send(content=chunk, embed=embed_to_send)
            posted_ids.append(new_msg.id)

        await self.save_message_ids(channel_id, posted_ids)
        self.log.info(f"[Rules] Posted {len(posted_ids)} rules messages in #{channel.name}. IDs saved to DB.")

    @app_commands.command(name="post_rules", description="Post or update the community rules message")
    @app_commands.describe(
        reset="Discard any live /edit_rules changes and republish from rules.yaml")
    @app_commands.guild_only()
    @utils.app_has_role("Admin")
    async def post_rules_cmd(self, interaction: discord.Interaction, reset: bool = False):
        await interaction.response.defer(ephemeral=True)
        await self.update_community_rules(ignore_override=reset)
        await interaction.followup.send(
            "Rules republished from `rules.yaml`; any live edits were discarded."
            if reset else
            "Rules updated. (Live `/edit_rules` changes, if any, are still in effect — "
            "use `/post_rules reset:True` to go back to `rules.yaml`.)",
            ephemeral=True)

    @app_commands.command(name="edit_rules", description="Open interactive form to edit community rules")
    @app_commands.guild_only()
    @utils.app_has_role("Admin")
    async def edit_rules_cmd(self, interaction: discord.Interaction):
        config = self.get_config() or {}
        current_text = self.format_rules_text(config)
        try:
            channel_id = int(config.get("channel") or 0)
        except ValueError:
            channel_id = 0
        if channel_id:
            # Show what is actually posted right now, so a second edit doesn't
            # silently revert the first one back to the yaml text.
            stored = await self.get_override_text(channel_id)
            if stored is not None:
                current_text = stored
        if len(current_text) > 4000:
            await interaction.response.send_message(
                "⚠️ The current rules are longer than the 4000-character modal limit. Editing here would "
                "silently truncate and permanently overwrite everything past that point. Edit `rules.yaml` "
                "directly and run `/post_rules` instead.",
                ephemeral=True
            )
            return
        modal = EditRulesModal(self, current_text)
        await interaction.response.send_modal(modal)


async def setup(bot):
    await bot.add_cog(Rules(bot))