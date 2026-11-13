"""The FowlEngine cog's Discord views (buttons and selects). Split out of
commands.py unchanged."""
import discord
from .icons import icon, icon_emoji
from .common import bfdb_admin_post, srv_path


class CommanderTerminalView(discord.ui.View):
    def __init__(self, api_url: str, admin_username: str, admin_password: str, airbases: list,
                 dynamic_types: list, objectives: list | None = None, server_name: str | None = None):
        super().__init__(timeout=None)
        # Which DCS server this terminal drives. Passed through to bfdb as
        # ?server= so a shared bfdb spawns on the right one.
        self.server_name = server_name
        self.api_url = api_url
        self.admin_username = admin_username
        self.admin_password = admin_password
        self.selected_airbase = None
        self.selected_type = None
        self.selected_objective = None

        options = []
        for ab in airbases[:25]:
            options.append(discord.SelectOption(label=ab['name'], description=f"Owner: {ab['owner']}"))
        if not options:
            options.append(discord.SelectOption(label="No Airbases Found", value="none"))

        self.airbase_select = discord.ui.Select(placeholder="Step 1: Select Airbase/FARP...", options=options, custom_id="ab_select")
        self.airbase_select.callback = self.ab_callback

        type_options = []
        for t_label, t_desc in dynamic_types[:25]:
            type_options.append(discord.SelectOption(label=t_label, description=t_desc))

        if not type_options:
            type_options.append(discord.SelectOption(label="No Deployables found in CFG", value="none"))

        self.type_select = discord.ui.Select(placeholder="Step 2: Select Deployable...", options=type_options, custom_id="type_select")
        self.type_select.callback = self.type_callback

        self.add_item(self.airbase_select)
        self.add_item(self.type_select)

        # Priority section (replaces the old standalone /fe_priority command).
        obj_options = []
        for o in (objectives or []):
            if o.get('owner') in ('Blue', 'Red'):
                # Option descriptions are plain text -- a custom emoji only
                # renders in the option's own emoji slot.
                obj_options.append(discord.SelectOption(
                    label=o['name'], description=f"{o.get('owner')} · {o.get('health', 0)}%",
                    emoji=icon_emoji("priority") if o.get('priority') else None))
        obj_options = obj_options[:25]
        if obj_options:
            self.objective_select = discord.ui.Select(
                placeholder="Priority: select an objective...", options=obj_options,
                custom_id="obj_select", row=2)
            self.objective_select.callback = self.obj_callback
            self.add_item(self.objective_select)
        else:
            self.objective_select = None

    async def ab_callback(self, interaction: discord.Interaction):
        self.selected_airbase = self.airbase_select.values[0]
        await interaction.response.send_message(f"Base selected: {self.selected_airbase}", ephemeral=True)

    async def type_callback(self, interaction: discord.Interaction):
        self.selected_type = self.type_select.values[0]
        await interaction.response.send_message(f"Deployable selected: {self.selected_type}", ephemeral=True)

    async def obj_callback(self, interaction: discord.Interaction):
        self.selected_objective = self.objective_select.values[0]
        await interaction.response.send_message(f"Objective selected: {self.selected_objective}", ephemeral=True)

    async def _set_priority(self, interaction: discord.Interaction, priority: bool):
        if not self.selected_objective:
            await interaction.response.send_message("Select an objective first.", ephemeral=True)
            return
        await interaction.response.defer(ephemeral=True)
        if not self.admin_username or not self.admin_password:
            await interaction.followup.send(f"{icon('bad')} admin_username/admin_password are not configured.")
            return
        try:
            status, _data = await bfdb_admin_post(
                self.api_url, self.admin_username, self.admin_password,
                srv_path("/api/admin/priority", self.server_name),
                {"objective": self.selected_objective, "priority": priority},
            )
        except Exception as ex:
            await interaction.followup.send(f"{icon('bad')} Failed: {ex}")
            return
        if status == 200:
            verb = "marked" if priority else "unmarked"
            await interaction.followup.send(f"{icon('priority')} **{self.selected_objective}** {verb} as priority.")
        else:
            await interaction.followup.send(f"{icon('bad')} Failed to set priority: HTTP {status}")

    @discord.ui.button(label="SET PRIORITY", style=discord.ButtonStyle.primary, row=3)
    async def priority_on_btn(self, interaction: discord.Interaction, button: discord.ui.Button):
        await self._set_priority(interaction, True)

    @discord.ui.button(label="CLEAR PRIORITY", style=discord.ButtonStyle.secondary, row=3)
    async def priority_off_btn(self, interaction: discord.Interaction, button: discord.ui.Button):
        await self._set_priority(interaction, False)

    @discord.ui.button(label="EXECUTE SPAWN", style=discord.ButtonStyle.success, row=4)
    async def spawn_btn(self, interaction: discord.Interaction, button: discord.ui.Button):
        if not self.selected_airbase or self.selected_airbase == "none":
            await interaction.response.send_message("Please select a valid Airbase first.", ephemeral=True)
            return
        if not self.selected_type:
            await interaction.response.send_message("Please select a Deployable first.", ephemeral=True)
            return
            
        await interaction.response.defer(ephemeral=True)
        if not self.admin_username or not self.admin_password:
            await interaction.followup.send(
                f"{icon('bad')} admin_username/admin_password are not configured for this server "
                "(required to authenticate against bfdb's commander API)."
            )
            return
        try:
            status, _data = await bfdb_admin_post(
                self.api_url, self.admin_username, self.admin_password,
                srv_path("/api/commander/spawn", self.server_name),
                {"airbase": self.selected_airbase, "type": self.selected_type},
            )
        except Exception as ex:
            await interaction.followup.send(f"{icon('bad')} Failed to spawn: {ex}")
            return
        if status == 200:
            await interaction.followup.send(f"{icon('good')} Successfully ordered {self.selected_type} at {self.selected_airbase}.")
        else:
            await interaction.followup.send(f"{icon('bad')} Failed to spawn: HTTP {status}")

class StagedApplyView(discord.ui.View):
    """Buttons under a staged-binary reply (upload.py): when the servers it
    was staged for should swap it in. The choice is the cog's; this only asks.
    Gone after `timeout` -- `/feops stage_apply` does the same later."""

    def __init__(self, cog, server_names: list, roles):
        super().__init__(timeout=6 * 3600)
        self.cog = cog
        self.server_names = list(server_names)
        self.roles = [str(r) for r in (roles or [])]

    def _allowed(self, member) -> bool:
        have = set()
        for r in getattr(member, "roles", None) or []:
            have.add(str(getattr(r, "name", "")))
            have.add(str(getattr(r, "id", "")))
        return any(r in have for r in self.roles)

    async def interaction_check(self, interaction: discord.Interaction) -> bool:
        if self._allowed(interaction.user):
            return True
        await interaction.response.send_message("Only the role that can stage a binary can pick when it applies.",
                                                ephemeral=True)
        return False

    async def _done(self, interaction: discord.Interaction, text: str):
        for child in self.children:
            child.disabled = True
        self.stop()
        try:
            await interaction.response.edit_message(view=self)
        except Exception:  # noqa: BLE001 - message gone; still answer below
            pass
        await interaction.followup.send(text)

    @discord.ui.button(label="Apply when empty", style=discord.ButtonStyle.primary)
    async def when_empty_btn(self, interaction: discord.Interaction, button: discord.ui.Button):
        text = self.cog.schedule_apply_when_empty(self.server_names, str(interaction.user))
        await self._done(interaction, text)

    @discord.ui.button(label="Restart now", style=discord.ButtonStyle.danger)
    async def now_btn(self, interaction: discord.Interaction, button: discord.ui.Button):
        await self._done(interaction, await self.cog.restart_to_apply(self.server_names, str(interaction.user)))

    @discord.ui.button(label="Next restart", style=discord.ButtonStyle.secondary)
    async def later_btn(self, interaction: discord.Interaction, button: discord.ui.Button):
        await self._done(interaction, f"{icon('pending')} Left staged -- swaps in on the next DCS restart of "
                                      f"{', '.join(self.server_names)}.")
