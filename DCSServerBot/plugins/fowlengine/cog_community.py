"""CommunityMixin: part of the FowlEngine cog (commands.py).

The player-facing Discord channels around the auto-fixer:

  * bug forum   every report is a forum post, tagged Open / Being fixed /
                Fixed / Not an engine bug / Closed as the auto-fix runner
                reports back. Players can open a post there themselves (it
                becomes a report, like /fe_bug), and the reporter's replies in
                it are added to their report.
  * changelog   when a release goes live on this box, its changelog is posted
                once: the player note the auto-fixer wrote, or, for a release
                without one, the release notes rewritten by the assistant.
  * questions   the assistant (assistant.py) answers players there, and
                anywhere it is @mentioned, from the server's wiki.

/fe_community_channels picks the channels, /fe_community_setup creates any
that are missing. Both are DCS Admin only.

A mixin: it holds no state of its own and relies on what FowlEngine.cog_load
sets up (self.community, self.assistant, self.bugs); discord.py collects its
commands and listeners through the MRO."""
import asyncio
from typing import Optional, Union

import discord
from discord import app_commands
from discord.ext import commands
from core import utils, command

from .assistant import AssistantConfig, clean_player_note, clean_reply
from .community import (DEFAULT_NAMES, STATUS_TAGS, changelog_post, new_report_line, status_line,
                        thread_title)
from .icons import icon, icon_emoji

NO_MENTIONS = discord.AllowedMentions.none()


class CommunityMixin:
    # ---- config ----------------------------------------------------------------------

    def _community_channels(self) -> dict:
        cfg = (self.get_config() or {}).get("community") or {}
        state = getattr(self, "community", None)
        return state.channels(cfg) if state else {"bug_forum": None, "changelog_channel": None, "questions": []}

    def _live_assistant(self):
        """The assistant with fowlengine.yaml's current settings, or None when it is off."""
        a = getattr(self, "assistant", None)
        if a is None:
            return None
        a.reconfigure(AssistantConfig.from_yaml((self.get_config() or {}).get("assistant")))
        return a if a.cfg.enabled else None

    async def _channel(self, cid: Optional[int]):
        if not cid:
            return None
        ch = self.bot.get_channel(cid)
        if ch is None:
            try:
                ch = await self.bot.fetch_channel(cid)
            except discord.HTTPException:
                return None
        return ch

    # ---- the bug forum -----------------------------------------------------------------

    async def _status_tags(self, forum) -> dict:
        """{status: ForumTag}, creating the status tags the forum lacks."""
        have = {t.name: t for t in forum.available_tags}
        for name in STATUS_TAGS.values():
            if name in have or len(have) >= 20:
                continue
            try:
                tag = await forum.create_tag(name=name, moderated=True, reason="Fowl Engine bug report states")
                have[name] = tag
            except discord.HTTPException as ex:
                self.log.warning(f"FowlEngine/community: can't add the '{name}' tag to #{forum.name} "
                                 f"(the bot needs Manage Channels): {ex}")
                break
        return {st: have[name] for st, name in STATUS_TAGS.items() if name in have}

    async def _tag_thread(self, thread, status: str) -> None:
        forum = thread.parent
        if not isinstance(forum, discord.ForumChannel):
            return
        tags = await self._status_tags(forum)
        if status not in tags:
            return
        status_names = set(STATUS_TAGS.values())
        keep = [t for t in thread.applied_tags if t.name not in status_names]
        try:
            await thread.edit(applied_tags=(keep + [tags[status]])[-5:], archived=False)
        except discord.HTTPException as ex:
            self.log.warning(f"FowlEngine/community: can't tag the post {thread.id} "
                             f"(the bot needs Manage Threads): {ex}")

    async def open_bug_thread(self, bug: dict) -> Optional[discord.Thread]:
        """A forum post for a report sent with /fe_bug. None without a forum."""
        forum = await self._channel(self._community_channels()["bug_forum"])
        if not isinstance(forum, discord.ForumChannel):
            return None
        tags = await self._status_tags(forum)
        try:
            made = await forum.create_thread(
                name=thread_title(bug["text"]),
                content=f"{bug['text']}\n-# Sent with /fe_bug · `{bug['id']}`",
                applied_tags=[tags["open"]] if "open" in tags else [],
                allowed_mentions=NO_MENTIONS)
        except discord.HTTPException as ex:
            self.log.warning(f"FowlEngine/community: can't post {bug['id']} in #{forum.name}: {ex}")
            return None
        self.bugs.set_thread(bug["id"], made.thread.id)
        await made.thread.send(new_report_line(bug["id"], bug.get("trusted")), allowed_mentions=NO_MENTIONS)
        return made.thread

    async def update_bug_thread(self, bid: str, status: str, note: Optional[str]) -> None:
        bugs = getattr(self, "bugs", None)
        tid = bugs.thread(bid) if bugs else None
        thread = await self._channel(tid)
        if not isinstance(thread, discord.Thread):
            return
        await self._tag_thread(thread, status)
        line = status_line(status, clean_player_note(note, 900))
        if line:
            try:
                await thread.send(line, allowed_mentions=NO_MENTIONS)
            except discord.HTTPException as ex:
                self.log.warning(f"FowlEngine/community: can't post in the thread of {bid}: {ex}")

    async def _forum_bug_post(self, message: discord.Message) -> None:
        """A player's own post in the bug forum: the starting message becomes a
        report; the reporter's later replies are added to it."""
        bugs = getattr(self, "bugs", None)
        thread = message.channel
        if bugs is None or ((self.get_config() or {}).get("bugs") or {}).get("enabled") is False:
            return
        if message.id != thread.id:     # a reply in the post, not the post itself
            bug = bugs.by_thread(thread.id)
            if bug and bugs.add_detail(bug["id"], str(message.author.id), message.clean_content):
                try:
                    await message.add_reaction(icon_emoji("good"))
                except discord.HTTPException:
                    pass
            return
        if bugs.by_thread(thread.id):
            return
        text = f"{thread.name}: {message.clean_content}"
        if message.attachments:
            text += f" ({len(message.attachments)} attachment(s) in the forum post)"
        trusted = self._bug_trusted(message.author)
        try:
            bug = bugs.add(user_id=str(message.author.id), text=text, trusted=trusted)
        except ValueError as ex:
            await thread.send(f"Can't log this one yet: {ex}.", allowed_mentions=NO_MENTIONS)
            return
        bugs.set_thread(bug["id"], thread.id)
        await self._tag_thread(thread, "open")
        await thread.send(new_report_line(bug["id"], trusted), allowed_mentions=NO_MENTIONS)
        await self.notify_ops(f"{icon('warning')} Bug report `{bug['id']}`{' (trusted)' if trusted else ''} "
                              f"in {thread.mention}: {bug['text'][:300]}")

    # ---- changelog ---------------------------------------------------------------------

    def release_went_live(self, tag: Optional[str]) -> None:
        """Called by the updater when part of a release goes live on this box."""
        if not tag:
            return
        try:
            asyncio.get_running_loop().create_task(self.post_changelog(tag))
        except RuntimeError:
            pass

    async def post_changelog(self, tag: str) -> None:
        state = getattr(self, "community", None)
        cid = self._community_channels()["changelog_channel"]
        if state is None or not cid or state.posted(tag):
            return
        latest = (getattr(self, "updater", None) and self.updater.state.get("latest")) or {}
        if latest.get("tag") != tag:
            return
        state.mark_posted(tag)      # first, so the next part of the same release doesn't post it again
        text = clean_reply(latest.get("changelog"))
        if not text:
            a = self._live_assistant()
            text = await a.write_changelog(latest.get("notes") or "") if a else None
        if not text:
            return
        channel = await self._channel(cid)
        if channel is None:
            return
        try:
            await channel.send(changelog_post(text, tag, latest.get("html_url")), allowed_mentions=NO_MENTIONS,
                               suppress_embeds=True)
        except discord.HTTPException as ex:
            self.log.warning(f"FowlEngine/community: can't post the changelog for {tag}: {ex}")

    # ---- questions -----------------------------------------------------------------------

    @commands.Cog.listener("on_message")
    async def community_on_message(self, message: discord.Message):
        if message.author.bot or message.webhook_id or not message.guild:
            return
        try:
            ch = self._community_channels()
            parent = getattr(message.channel, "parent_id", None)
            if ch["bug_forum"] and parent == ch["bug_forum"]:
                await self._forum_bug_post(message)
                return
            await self._maybe_answer(message, ch)
        except Exception as ex:  # noqa: BLE001 -- a listener must never take the cog down
            self.log.exception(f"FowlEngine/community: message handler error: {ex}")

    async def _maybe_answer(self, message: discord.Message, ch: dict) -> None:
        a = self._live_assistant()
        if a is None:
            return
        me = self.bot.user
        channel = message.channel
        in_questions = channel.id in ch["questions"] or getattr(channel, "parent_id", None) in ch["questions"]
        mentioned = me is not None and me in message.mentions
        ref = getattr(message.reference, "resolved", None)
        replying_to_me = isinstance(ref, discord.Message) and ref.author == me
        if not (in_questions or (a.cfg.answer_mentions and (mentioned or replying_to_me))):
            return
        if a.client() is None:
            self.log.warning(f"FowlEngine/assistant: off -- {a.unavailable}")
            return
        why_not = a.allow(str(message.author.id))
        if why_not:
            self.log.info(f"FowlEngine/assistant: not answering {message.author.id} ({why_not})")
            return
        question = message.clean_content
        if me is not None:
            for name in {me.name, me.display_name, message.guild.me.display_name}:
                question = question.replace(f"@{name}", "")
            question = question.strip()
        history = []
        try:
            async for m in channel.history(limit=8, before=message):
                history.append(("you" if m.author == me else m.author.display_name, m.clean_content))
        except discord.HTTPException:
            pass
        history.reverse()
        if isinstance(channel, discord.Thread):
            if message.id == channel.id:
                question = f"{channel.name}\n{question}"
            else:
                history.insert(0, ("post title", channel.name))
        async with channel.typing():
            reply = await a.answer(question, message.author.display_name, history,
                                   mentioned=mentioned or replying_to_me)
        if reply:
            await message.reply(reply, mention_author=False, allowed_mentions=NO_MENTIONS, suppress_embeds=True)

    # ---- admin commands ------------------------------------------------------------------

    def _channels_summary(self, guild: discord.Guild) -> str:
        ch = self._community_channels()

        def show(cid):
            c = guild.get_channel(cid) if cid else None
            return c.mention if c else ("`%s` (not found)" % cid if cid else "not set")
        a = self._live_assistant()
        state = ("on" if a and a.client() else
                 f"off ({a.unavailable})" if a else "off (`assistant.enabled` in fowlengine.yaml)")
        return (f"Bug forum: {show(ch['bug_forum'])}\nChangelog: {show(ch['changelog_channel'])}\n"
                f"Questions: {', '.join(show(c) for c in ch['questions']) or 'not set'}\n"
                f"Assistant: {state}")

    @command(description='Pick the bug forum, changelog and questions channels (admin).')
    @app_commands.guild_only()
    @app_commands.describe(bug_forum='Forum channel: each bug report is a post there.',
                           changelog='Text channel for the update notes.',
                           questions='Channel (text or forum) where the bot answers questions.',
                           reset='Forget the picks made here and use fowlengine.yaml again.')
    @utils.app_has_role('DCS Admin')
    async def fe_community_channels(self, interaction: discord.Interaction,
                                    bug_forum: Optional[discord.ForumChannel] = None,
                                    changelog: Optional[discord.TextChannel] = None,
                                    questions: Optional[Union[discord.TextChannel, discord.ForumChannel]] = None,
                                    reset: bool = False):
        if reset:
            self.community.pick(bug_forum=0, changelog_channel=0, questions=0)
        self.community.pick(bug_forum=bug_forum.id if bug_forum else None,
                            changelog_channel=changelog.id if changelog else None,
                            questions=[questions.id] if questions else None)
        if bug_forum:
            await self._status_tags(bug_forum)
        await interaction.response.send_message(self._channels_summary(interaction.guild), ephemeral=True)

    @command(description='Create the bug forum, changelog and questions channels that are missing (admin).')
    @app_commands.guild_only()
    @app_commands.describe(confirm='Create them. Without it, only says what it would do.')
    @utils.app_has_role('DCS Admin')
    async def fe_community_setup(self, interaction: discord.Interaction, confirm: bool = False):
        await interaction.response.defer(ephemeral=True)
        guild = interaction.guild
        ch = self._community_channels()
        wanted = [("bug_forum", "forum", ch["bug_forum"]),
                  ("changelog_channel", "text", ch["changelog_channel"]),
                  ("questions", "forum", ch["questions"][0] if ch["questions"] else None)]
        plan, todo = [], []
        for key, kind, cid in wanted:
            have = guild.get_channel(cid) if cid else None
            same = have or discord.utils.get(guild.channels, name=DEFAULT_NAMES[key])
            if same is not None:
                plan.append(f"{icon('good')} {key.replace('_', ' ')}: {same.mention}")
                todo.append((key, kind, same))
            else:
                plan.append(f"{icon('build')} {key.replace('_', ' ')}: create #{DEFAULT_NAMES[key]} ({kind})")
                todo.append((key, kind, None))
        if not confirm:
            await interaction.followup.send("\n".join(plan) + "\n\nRun again with `confirm: True` to set it up.")
            return
        try:
            cat = discord.utils.get(guild.categories, name=DEFAULT_NAMES["category"])
            if cat is None and any(c is None for _, _, c in todo):
                cat = await guild.create_category(DEFAULT_NAMES["category"], reason="Fowl Engine community setup")
            picks = {}
            for key, kind, existing in todo:
                c = existing
                if c is None and key == "bug_forum":
                    c = await guild.create_forum(
                        DEFAULT_NAMES[key], category=cat, reason="Fowl Engine community setup",
                        topic="Found something broken on the servers? Make a post: what happened, which server "
                              "and airframe, and roughly when (Zulu). Tacview or screenshots help.")
                elif c is None and key == "questions":
                    c = await guild.create_forum(
                        DEFAULT_NAMES[key], category=cat, reason="Fowl Engine community setup",
                        topic="Ask anything about the servers: menus, logistics, rules, where to start.")
                elif c is None:
                    c = await guild.create_text_channel(
                        DEFAULT_NAMES[key], category=cat, reason="Fowl Engine community setup",
                        topic="What changed on the servers, posted as each update goes live.",
                        overwrites={guild.default_role: discord.PermissionOverwrite(send_messages=False),
                                    guild.me: discord.PermissionOverwrite(send_messages=True)})
                picks[key] = [c.id] if key == "questions" else c.id
                if key == "bug_forum" and isinstance(c, discord.ForumChannel):
                    await self._status_tags(c)
            self.community.pick(**picks)
        except discord.Forbidden:
            await interaction.followup.send(f"{icon('bad')} The bot needs Manage Channels to create these.")
            return
        await interaction.followup.send(f"{icon('good')} Done.\n" + self._channels_summary(guild))
