"""BugsMixin: part of the FowlEngine cog (commands.py).

/fe_bug: players report a bug from Discord. The report is kept on the box
(bugreports.py) and the auto-fix runner on the admin's PC picks it up over the
OPS API (GET bugs). When the runner marks it fixed or not-a-bug, the reporter
gets a DM saying so.

A mixin: it holds no state of its own and relies on what FowlEngine.__init__
sets up (self.bugs); discord.py collects its commands through the MRO."""
import discord
from typing import Optional
from core import utils, command
from discord import app_commands
from .icons import icon
from .assistant import clean_player_note
from .bugreports import MAX_TEXT, MIN_TEXT

# Who counts as trusted when `bugs.trusted_roles` is not set: their reports
# may be fixed and published without an admin looking first.
DEFAULT_TRUSTED_KINDS = ("DCS Admin",)


class BugsMixin:
    def _bug_trusted(self, member) -> bool:
        cfg = (self.get_config() or {}).get("bugs") or {}
        names = set(cfg.get("trusted_roles") or [])
        roles = getattr(self.bot, "roles", None) or {}
        for kind in DEFAULT_TRUSTED_KINDS:
            names.update(roles.get(kind) or [kind])
        return any(r.name in names for r in getattr(member, "roles", []) or [])

    @command(description='Report a Fowl Engine bug: the campaign, range, menus, website or bot.')
    @app_commands.guild_only()
    @app_commands.describe(what='What went wrong, where (server, aircraft, menu), and what you expected instead.')
    @utils.app_has_role('DCS')
    async def fe_bug(self, interaction: discord.Interaction,
                     what: app_commands.Range[str, MIN_TEXT, MAX_TEXT]):
        bugs = getattr(self, "bugs", None)
        if bugs is None or ((self.get_config() or {}).get("bugs") or {}).get("enabled") is False:
            await interaction.response.send_message("Bug reports are switched off on this server.", ephemeral=True)
            return
        trusted = self._bug_trusted(interaction.user)
        try:
            bug = bugs.add(user_id=str(interaction.user.id), text=what, trusted=trusted)
        except ValueError as ex:
            await interaction.response.send_message(f"Not sent: {ex}.", ephemeral=True)
            return
        await interaction.response.send_message(
            f"{icon('good')} Thanks, report `{bug['id']}` is in. "
            + ("It goes straight to the auto-fixer." if trusted else "An admin will look at it.")
            + " You'll get a DM when it's resolved. Don't include passwords or personal details.",
            ephemeral=True)
        thread = await self.open_bug_thread(bug)
        if thread is not None:
            try:
                await interaction.followup.send(f"It's tracked in {thread.mention}: add details or a "
                                                f"Tacview there.", ephemeral=True)
            except discord.HTTPException:
                pass
        await self.notify_ops(f"{icon('warning')} Bug report `{bug['id']}`"
                              f"{' (trusted)' if trusted else ''}: {bug['text'][:300]}")

    async def tell_bug_reporter(self, bid: str, status: str, note: Optional[str]) -> None:
        """Tag the report's forum post and say so there, and DM the reporter
        when it is resolved. Best-effort."""
        try:
            await self.update_bug_thread(bid, status, note)
        except Exception as ex:  # noqa: BLE001
            self.log.warning(f"FowlEngine/bugs: could not update the forum post of {bid}: {ex}")
        bugs = getattr(self, "bugs", None)
        uid = bugs.reporter(bid) if bugs else None
        if not uid or status not in ("fixed", "no_fix"):
            return
        try:
            user = self.bot.get_user(int(uid)) or await self.bot.fetch_user(int(uid))
            what = ("is fixed, and the fix goes out with the next update" if status == "fixed"
                    else "got looked at, and it doesn't look like something in the Fowl Engine itself")
            note = clean_player_note(note, 400)
            await user.send(f"Hey, your bug report `{bid}` {what}." + (f"\n> {note}" if note else ""))
        except Exception as ex:  # noqa: BLE001 -- closed DMs, left the guild
            self.log.info(f"FowlEngine/bugs: could not DM the reporter of {bid}: {ex}")
