import discord
from discord import app_commands, ui
from discord.ext import commands
from core import Plugin, utils
from datetime import datetime, timezone, timedelta

# Safely attempt to load the Gemini library
try:
    import google.generativeai as genai
    HAS_GENAI = True
except ImportError as e:
    HAS_GENAI = False
    print(f"\n==========================================================")
    print(f"🚨 SMARTMOD WARNING: Missing AI Package! ({e})")
    print(f"The bot is running in a virtual environment. You must install")
    print(f"the library directly into the .venv folder for AI to work.")
    print(f"==========================================================\n")

class VerifyView(ui.View):
    """Persistent View for the Verification Button."""
    def __init__(self, role_id: int):
        super().__init__(timeout=None)
        self.role_id = role_id

    @ui.button(label="Click to Verify", style=discord.ButtonStyle.success, custom_id="smartmod:verify")
    async def verify_button(self, interaction: discord.Interaction, button: ui.Button):
        role = interaction.guild.get_role(self.role_id)
        if role:
            await interaction.user.add_roles(role)
            await interaction.response.send_message("Verification successful. Welcome to the server!", ephemeral=True)
        else:
            await interaction.response.send_message("Error: Verified role not found. Contact an admin.", ephemeral=True)

class Smartmod(Plugin):
    """Smart Moderation with AI Context Checking and Alt/Raid Protection"""

    def __init__(self, bot):
        super().__init__(bot)
        self.recent_joins = []
        self.ai_model = None

    async def cog_load(self):
        await super().cog_load()
        config = self.get_config() or {}
        
        # Register persistent views
        if "verified_role_id" in config:
            self.bot.add_view(VerifyView(config["verified_role_id"]))
            
        # Initialize database silently on boot
        self.bot.loop.create_task(self._startup_sync())
        
        # Load AI only if the package was successfully imported
        if HAS_GENAI:
            api_key = config.get("gemini_api_key")
            if api_key and api_key != "YOUR_GEMINI_API_KEY_HERE":
                genai.configure(api_key=api_key)
                self.ai_model = genai.GenerativeModel('gemini-1.5-flash')
                self.log.info("[Smartmod] Gemini AI integration active.")
            else:
                self.log.warning("[Smartmod] Gemini API key missing. Operating in keyword-only mode.")
        else:
            self.log.error("[Smartmod] AI disabled due to missing 'google.generativeai' package.")

    async def _startup_sync(self):
        await self.bot.wait_until_ready()
        await self.init_db()

    async def init_db(self):
        try:
            async with self.apool.connection() as conn:
                async with conn.transaction():
                    await conn.execute("""
                        CREATE TABLE IF NOT EXISTS smartmod_warnings (
                            id SERIAL PRIMARY KEY,
                            guild_id BIGINT NOT NULL,
                            user_id BIGINT NOT NULL,
                            points INT NOT NULL,
                            reason TEXT NOT NULL,
                            article VARCHAR(50),
                            created_at TIMESTAMP WITH TIME ZONE DEFAULT NOW()
                        )
                    """)
            self.log.info("[Smartmod] Database table 'smartmod_warnings' verified.")
        except Exception as e:
            self.log.error(f"[Smartmod] Failed to initialize database table: {e}", exc_info=True)

    async def send_log(self, guild: discord.Guild, embed: discord.Embed):
        config = self.get_config() or {}
        log_channel_id = config.get("log_channel_id")
        if log_channel_id:
            log_channel = guild.get_channel(log_channel_id)
            if log_channel:
                await log_channel.send(embed=embed)

    async def get_user_points(self, guild_id: int, user_id: int) -> int:
        try:
            async with self.apool.connection() as conn:
                cursor = await conn.execute(
                    "SELECT SUM(points) FROM smartmod_warnings WHERE guild_id = %s AND user_id = %s",
                    (guild_id, user_id)
                )
                result = await cursor.fetchone()
                return int(result[0]) if result and result[0] else 0
        except Exception:
            return 0

    async def apply_points(self, guild: discord.Guild, member: discord.Member, amount: int, reason: str, article: str = None):
        try:
            async with self.apool.connection() as conn:
                async with conn.transaction():
                    await conn.execute(
                        "INSERT INTO smartmod_warnings (guild_id, user_id, points, reason, article) VALUES (%s, %s, %s, %s, %s)",
                        (guild.id, member.id, amount, reason, article)
                    )
        except Exception as e:
            self.log.error(f"[Smartmod] Failed adding points: {e}")
            return f"⚠️ Failed to record a warning for {member.mention} due to a database error — no points were applied. Check the bot logs."

        current = await self.get_user_points(guild.id, member.id)
        config = self.get_config() or {}
        max_pts = config.get("max_points", 10)
        timeout_hours = config.get("timeout_duration_hours", 24)
        # Discord hard-caps timeouts at 28 days; clamp instead of letting the API call fail.
        timeout_hours = min(timeout_hours, 28 * 24)
        rule_text = f" (Article: {article})" if article else ""

        if current >= max_pts:
            try:
                duration = timedelta(hours=timeout_hours)
                await member.timeout(duration, reason=f"Exceeded max points. Offense: {reason}")
                await member.send(f"You have been timed out in **{guild.name}** for {timeout_hours} hours. Max warning points reached. Last offense: {reason}{rule_text}")
            except discord.Forbidden:
                pass
            except discord.HTTPException as e:
                self.log.error(f"[Smartmod] Failed to timeout {member.id}: {e}")
                return f"⚠️ {member.mention} reached {current}/{max_pts} points but the timeout could not be applied (Discord API error — check logs)."
            return f"🚨 {member.mention} reached {current}/{max_pts} points and was automatically **timed out for {timeout_hours} hours**."
        else:
            try:
                await member.send(f"⚠️ Warning in **{guild.name}**: {reason}{rule_text}. Points: {current}/{max_pts}")
            except discord.Forbidden:
                pass
            return f"⚠️ {member.mention} warned (+{amount} pts). Total: {current}/{max_pts}"

    @app_commands.command(name="warn", description="Warn a user, cite a rule, and assign points")
    @app_commands.guild_only()
    @utils.app_has_role("Admin")
    async def warn_cmd(self, interaction: discord.Interaction, user: discord.Member, article: str, reason: str, points: int = 2):
        if user.bot:
            await interaction.response.send_message("You cannot warn a bot.", ephemeral=True)
            return
            
        await interaction.response.defer()
        result_msg = await self.apply_points(interaction.guild, user, points, reason, article)
        
        embed = discord.Embed(title="Database Warning Issued", color=discord.Color.orange())
        embed.add_field(name="Moderator", value=interaction.user.mention, inline=True)
        embed.add_field(name="User", value=user.mention, inline=True)
        embed.add_field(name="Article", value=article, inline=False)
        embed.add_field(name="Reason", value=reason, inline=False)
        await self.send_log(interaction.guild, embed)
        
        await interaction.followup.send(result_msg)

    @app_commands.command(name="spawn_verification", description="Spawn the verification button panel")
    @app_commands.guild_only()
    @utils.app_has_role("Admin")
    async def spawn_verify(self, interaction: discord.Interaction):
        config = self.get_config() or {}
        view = VerifyView(config.get("verified_role_id", 0))
        
        embed = discord.Embed(
            title="🛡️ Server Verification",
            description="Please click the button below to prove you are human and gain access to the rest of the server.",
            color=discord.Color.dark_teal()
        )
        
        await interaction.response.send_message(embed=embed, view=view)

    @commands.Cog.listener()
    async def on_message(self, message: discord.Message):
        if message.author.bot or not message.guild:
            return
            
        content = message.content.strip()
        if not content:
            return
            
        lure_triggers = ["http", "www", "discord.gg", "free", "nitro", "crypto", "gift", "giveaway", "steam"]
        config = self.get_config() or {}
        
        # 1. AI Checking (Only runs if library was installed successfully)
        if HAS_GENAI and self.ai_model and any(trigger in content.lower() for trigger in lure_triggers):
            prompt = f'Analyze this Discord message. Is it a scam, phishing attempt, fake giveaway, or malicious server invite? Context matters. Message: "{content}". Reply ONLY YES if scam, NO if safe.'
            try:
                response = await self.ai_model.generate_content_async(prompt)
                if "YES" in response.text.strip().upper():
                    await message.delete()
                    action_log = await self.apply_points(message.guild, message.author, 5, "AI context-flagged scam attempt.")
                    await message.channel.send(f"🛑 {message.author.mention}'s message removed by AI AutoMod. {action_log}", delete_after=10)
                    
                    embed = discord.Embed(title="AI Scam Blocked", color=discord.Color.red())
                    embed.add_field(name="User", value=message.author.mention, inline=True)
                    embed.add_field(name="Content", value=content[:1024], inline=False)
                    await self.send_log(message.guild, embed)
                    return
            except Exception as e:
                self.log.error(f"[Smartmod] AI check error: {e}")

        # 2. Keyword Fallback
        if any(scam.lower() in content.lower() for scam in config.get("scam_keywords", [])):
            await message.delete()
            action_log = await self.apply_points(message.guild, message.author, 5, "Keyword-flagged scam text.")
            await message.channel.send(f"🛑 {message.author.mention}'s message removed. {action_log}", delete_after=10)

    @commands.Cog.listener()
    async def on_member_join(self, member: discord.Member):
        config = self.get_config() or {}
        now = discord.utils.utcnow()
        
        # Alt-Detection
        min_days = config.get("min_account_age_days", 7)
        account_age = now - member.created_at
        if account_age.days < min_days:
            q_role_id = config.get("quarantine_role_id")
            if q_role_id:
                q_role = member.guild.get_role(q_role_id)
                if q_role:
                    await member.add_roles(q_role, reason="Alt-detection: Brand new account.")
            
            embed = discord.Embed(title="⚠️ Suspicious New Account", color=discord.Color.gold())
            embed.add_field(name="User", value=member.mention, inline=True)
            embed.add_field(name="Age", value=f"{account_age.days} days", inline=True)
            await self.send_log(member.guild, embed)

        # Raid Detection
        self.recent_joins = [t for t in self.recent_joins if now - t < timedelta(seconds=10)]
        self.recent_joins.append(now)
        
        if len(self.recent_joins) >= config.get("raid_joins_per_10s", 5):
            try:
                await member.kick(reason="Anti-Raid wave triggered")
            except discord.Forbidden:
                pass

async def setup(bot):
    await bot.add_cog(Smartmod(bot))