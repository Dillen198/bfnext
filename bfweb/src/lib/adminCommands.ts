/**
 * The engine's `-admin` chat commands, as forms for the admin page's
 * command console (components/admin/EngineCommands.tsx). Each one is built
 * into the text chat takes and sent through bfdb's /api/admin/command, which
 * the engine parses exactly as chat does (bflib `admin::remote_command`).
 * So an admin can run them without being in the game.
 *
 * Left out on purpose: `reset` and `shutdown`, which have their own
 * confirmed buttons, and `log-desc`, which reads the plane you are sitting
 * in. The free-text line on the console takes anything else as typed.
 */

export type ArgKind = 'text' | 'number' | 'side' | 'choice'

export interface CommandArg {
  name: string
  label: string
  kind: ArgKind
  /** For `choice`. */
  options?: string[]
  optional?: boolean
  placeholder?: string
  /** The rest of the line (a broadcast's text, a player name with spaces). */
  rest?: boolean
}

export interface AdminCommandDef {
  /** The chat command word, e.g. `repair`. */
  word: string
  label: string
  help: string
  args: CommandArg[]
  /** Text put between the word and the args, e.g. `all` for repair all. */
  fixed?: string
  /** Asks for a second click before it runs. */
  danger?: boolean
}

export interface CommandGroup {
  label: string
  commands: AdminCommandDef[]
}

const player = (label = 'Player'): CommandArg => ({
  name: 'player', label, kind: 'text', rest: true, placeholder: 'name, player id or UCID',
})
const objective = (label = 'Objective'): CommandArg => ({
  name: 'objective', label, kind: 'text', rest: true, placeholder: 'e.g. Damascus',
})
const LIFE_TYPES = ['standard', 'intercept', 'logistics', 'attack', 'recon']
const RULES = ['actions', 'cargo', 'troops', 'jtac', 'ca']

export const COMMAND_GROUPS: CommandGroup[] = [
  {
    label: 'Repairs & logistics',
    commands: [
      { word: 'repair', fixed: 'all', label: 'Repair all', danger: true,
        help: 'Rebuild every objective for free: every damaged group and every logistics building, fair-play damage included.',
        args: [] },
      { word: 'repair', label: 'Repair base', help: 'Rebuild one airbase outright: its groups and its logistics buildings.',
        args: [objective('Airbase')] },
      { word: 'tick', label: 'Logistics tick now', help: 'Run a logistics tick now.', args: [] },
      { word: 'deliver', label: 'Deliver now', help: 'Run a logistics delivery now.', args: [] },
      { word: 'transfer', label: 'Transfer supply', help: 'Move supplies from one objective to another.',
        args: [{ name: 'from', label: 'From', kind: 'text', placeholder: 'objective' },
               { name: 'to', label: 'To', kind: 'text', rest: true, placeholder: 'objective' }] },
      { word: 'reduce', label: 'Reduce supply', help: 'Cut an objective’s supplies by a percentage.',
        args: [{ name: 'objective', label: 'Objective', kind: 'text', placeholder: 'objective' },
               { name: 'percent', label: 'Percent', kind: 'number', placeholder: '50' }] },
      { word: 'reinit-warehouse', label: 'Reinit warehouse', help: 'Reinitialize an airbase’s warehouse.', danger: true,
        args: [objective('Airbase')] },
      { word: 'capture', label: 'Force capture', help: 'Make an objective change hands.', danger: true,
        args: [{ name: 'objective', label: 'Objective', kind: 'text', placeholder: 'objective' },
               { name: 'side', label: 'Side', kind: 'choice', options: ['blue', 'red', 'neutral'] }] },
      { word: 'remark', label: 'Redraw markup', help: 'Refresh an objective’s map markup.', args: [objective()] },
      { word: 'whythreat', label: 'Why not repairing', help: 'Threats, capture timer, logistics and supply holding a base back.',
        args: [objective()] },
      { word: 'logistics', label: 'Logistics dump', help: 'Write the whole logistics picture to the engine log.', args: [] },
    ],
  },
  {
    label: 'Players',
    commands: [
      { word: 'connected', label: 'Connected players', help: 'List who is connected.', args: [] },
      { word: 'search', label: 'Search players', help: 'Search the player list by regular expression.',
        args: [{ name: 'regex', label: 'Regex', kind: 'text', rest: true, placeholder: 'e.g. ^D4' }] },
      { word: 'kick', label: 'Kick', help: 'Kick a player off the server.', danger: true, args: [player()] },
      { word: 'deslot', label: 'Deslot', help: 'Send a player to spectators.', args: [player()] },
      { word: 'switch', label: 'Switch side', help: 'Move a player to the other coalition.',
        args: [{ name: 'side', label: 'Side', kind: 'side' }, player()] },
      { word: 'ban', label: 'Ban', help: 'Kick and ban. Duration like 10days, 12h, or forever.', danger: true,
        args: [{ name: 'duration', label: 'Duration', kind: 'text', placeholder: 'forever / 10days' }, player()] },
      { word: 'unban', label: 'Unban', help: 'Lift a ban.', args: [player()] },
      { word: 'banned', label: 'Banned list', help: 'List banned players.', args: [] },
      { word: 'blacklist', label: 'Blacklist', help: 'Deny a player a feature.',
        args: [{ name: 'rule', label: 'Rule', kind: 'choice', options: RULES }, player()] },
      { word: 'whitelist', label: 'Whitelist', help: 'Give a feature back.',
        args: [{ name: 'rule', label: 'Rule', kind: 'choice', options: RULES }, player()] },
      { word: 'broadcast', label: 'Broadcast', help: 'Send a message to every player, as chat and a panel.',
        args: [{ name: 'text', label: 'Message', kind: 'text', rest: true, placeholder: 'message' }] },
    ],
  },
  {
    label: 'Points & lives',
    commands: [
      { word: 'balance', label: 'Balance', help: 'Show a player’s point balance.', args: [player()] },
      { word: 'addpoints', label: 'Add points', help: 'Add points (negative to take away).',
        args: [{ name: 'player', label: 'Player', kind: 'text', placeholder: 'name, id or UCID' },
               { name: 'n', label: 'Points', kind: 'number', placeholder: '100' }] },
      { word: 'set-points', label: 'Set points', help: 'Set a player’s point balance.',
        args: [{ name: 'n', label: 'Points', kind: 'number', placeholder: '500' }, player()] },
      { word: 'lives', label: 'Set lives', help: 'Set how many lives of one type a player has left.',
        args: [{ name: 'player', label: 'Player', kind: 'text', placeholder: 'name, id or UCID' },
               { name: 'type', label: 'Type', kind: 'choice', options: LIFE_TYPES },
               { name: 'n', label: 'Lives', kind: 'number', placeholder: '3' }] },
      { word: 'reset-lives', label: 'Reset lives', help: 'Give one player their lives back.', args: [player()] },
      { word: 'reset-lives-all', label: 'Reset everyone’s lives', help: 'Give every player their lives back.', danger: true, args: [] },
    ],
  },
  {
    label: 'Map & spawns',
    commands: [
      { word: 'spawn', label: 'Spawn at mark', help: 'Spawn at the F10 marks with this text: <key> <troop|deployable> <side> <heading> <name>.',
        args: [{ name: 'key', label: 'Mark key', kind: 'text', placeholder: 'mark text' }] },
      { word: 'tim', label: 'Explosion at mark', help: 'Explosions at every F10 mark with this text.', danger: true,
        args: [{ name: 'key', label: 'Mark key', kind: 'text', placeholder: 'mark text' },
               { name: 'size', label: 'Size', kind: 'number', optional: true, placeholder: '3000' },
               { name: 'alt', label: 'Altitude', kind: 'number', optional: true }] },
      { word: 'jammer', label: 'Jammer at mark', help: 'A GPS/radio jammer truck at every F10 mark “<key> <side>”.',
        args: [{ name: 'key', label: 'Mark key', kind: 'text', placeholder: 'mark text' }] },
      { word: 'delete', label: 'Delete group', help: 'Delete a deployed group by id.', danger: true,
        args: [{ name: 'group', label: 'Group id', kind: 'number' }] },
    ],
  },
  {
    label: 'Diagnostics',
    commands: [
      { word: 'ghosts', label: 'Ghost units', help: 'Units alive in the campaign with no DCS object.', args: [] },
      { word: 'purge-ghosts', label: 'Purge ghosts', help: 'Mark every ghost unit dead.', danger: true, args: [] },
      { word: 'log-warehouse', label: 'Log warehouse', help: 'Write a warehouse’s contents to the engine log.',
        args: [{ name: 'kind', label: 'Which', kind: 'choice', options: ['objective', 'dcs'] }, objective('Airbase')] },
      { word: 'help', label: 'Engine help', help: 'Every command the engine knows.', args: [] },
    ],
  },
]

export type BuildResult = { ok: true; text: string } | { ok: false; error: string }

/** The chat text for `def` with `values`, or what is missing. */
export function buildCommand(def: AdminCommandDef, values: Record<string, string>): BuildResult {
  const parts = [def.word]
  if (def.fixed) parts.push(def.fixed)
  for (const arg of def.args) {
    const v = (values[arg.name] ?? '').trim()
    if (!v) {
      if (arg.optional) continue
      return { ok: false, error: `${arg.label} is needed` }
    }
    if (arg.kind === 'number' && !/^-?\d+(\.\d+)?$/.test(v)) {
      return { ok: false, error: `${arg.label} must be a number` }
    }
    if (!arg.rest && /\s/.test(v)) {
      return { ok: false, error: `${arg.label} can’t have spaces` }
    }
    parts.push(v)
  }
  return { ok: true, text: parts.join(' ') }
}

/** The command text the free-text line sends: `-admin ` is optional. */
export function cleanConsoleLine(line: string): string {
  return line.trim().replace(/^-admin\s+/i, '').trim()
}

/** What the console refuses before sending (the engine refuses them too). */
export function refusedHere(text: string): string | null {
  const word = text.trim().split(/\s+/)[0]?.toLowerCase() ?? ''
  if (word === 'reset' || word === 'shutdown') return `${word} has its own button on this page`
  if (word === 'log-desc') return 'log-desc reads the plane you are in: run it in game'
  return null
}
