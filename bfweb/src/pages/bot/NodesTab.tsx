// Nodes: the bot's nodes and DCS instances, and what can be done to a whole
// node (restart, upgrade, DCS update, take its servers off line) -- full
// admins only, the same as the bot's own console.

import { useState } from 'react'
import type { BotFleet, BotNode } from '../../api'
import { cell } from '../../lib/botconsole'
import { ActionForm, Btn, Card, ConfirmBtn, Empty, Pill, useAction, useRunner } from './ui'
import { CELL, MONO, TH } from './style'
import { Plus, RotateCw, Server, Square, Download, Trash2 } from '@icons'

function NodeCard({ n, fleet }: { n: BotNode; fleet: BotFleet }) {
  const runner = useRunner()
  const [form, setForm] = useState<null | 'update_dcs' | 'add_server' | 'take_node_offline' | 'bring_node_online'>(null)
  const a = {
    restart: useAction('restart_node'), shutdown: useAction('shutdown_node'), upgrade: useAction('upgrade_node'),
    update_dcs: useAction('update_dcs'), offline: useAction('take_node_offline'), online: useAction('bring_node_online'),
    add: useAction('add_server'), del: useAction('delete_server'),
  }
  const servers = fleet.servers.filter(s => s.node === n.name)
  const players = servers.reduce((x, s) => x + s.players.length, 0)
  const go = (name: string) => void runner.act(name, { node_name: n.name }, n.name)
  const formAction = form === 'update_dcs' ? a.update_dcs : form === 'add_server' ? a.add
    : form === 'take_node_offline' ? a.offline : form === 'bring_node_online' ? a.online : undefined
  return (
    <Card icon={<Server size={13} style={{ color: n.online ? 'var(--accent)' : '#ef4444' }} />} title={n.name}
      right={<>
        {n.master && <Pill tone="ok">master</Pill>}
        <Pill tone={n.online ? 'ok' : 'down'}>{n.online ? 'online' : 'offline'}</Pill>
      </>}>
      <div style={{ display: 'flex', flexDirection: 'column', gap: 10 }}>
        <div className="flex gap-4" style={{ fontSize: '0.66rem', color: 'var(--text-muted)', flexWrap: 'wrap' }}>
          <span>DCS <span style={MONO}>{cell(n.dcs_version)}</span>{n.dcs_branch ? ` (${cell(n.dcs_branch)})` : ''}</span>
          <span>bot <span style={MONO}>{cell(n.bot_version)}</span></span>
          <span>{servers.length} server{servers.length === 1 ? '' : 's'}, {players} player{players === 1 ? '' : 's'}</span>
        </div>
        {n.online && (
          <div className="flex items-center gap-2" style={{ flexWrap: 'wrap' }}>
            {a.restart && <ConfirmBtn label="Restart node" icon={<RotateCw size={10} />} confirm={`Restart the bot on ${n.name}?`} onConfirm={() => go('restart_node')} />}
            {a.upgrade && <ConfirmBtn label="Upgrade bot" icon={<Download size={10} />} confirm={`Upgrade DCSServerBot on ${n.name}?`} onConfirm={() => go('upgrade_node')} />}
            {a.update_dcs && <Btn active={form === 'update_dcs'} onClick={() => setForm(f => f === 'update_dcs' ? null : 'update_dcs')}>Update DCS…</Btn>}
            {a.offline && <Btn active={form === 'take_node_offline'} onClick={() => setForm(f => f === 'take_node_offline' ? null : 'take_node_offline')}>Take servers offline…</Btn>}
            {a.online && <Btn active={form === 'bring_node_online'} onClick={() => setForm(f => f === 'bring_node_online' ? null : 'bring_node_online')}>Bring servers online…</Btn>}
            {a.add && <Btn active={form === 'add_server'} onClick={() => setForm(f => f === 'add_server' ? null : 'add_server')}><Plus size={10} />Add server…</Btn>}
            {a.shutdown && <ConfirmBtn label="Shut down node" icon={<Square size={10} />} confirm={`Shut the bot on ${n.name} down? Someone has to start it again on the box.`} onConfirm={() => go('shutdown_node')} />}
          </div>
        )}
        {formAction && <ActionForm key={formAction.name} action={formAction} fixed={{ node_name: n.name }} compact />}
        {(n.instances ?? []).length > 0 && (
          <table style={{ width: '100%', borderCollapse: 'collapse' }}>
            <thead><tr>{['Instance', 'Server', 'DCS port', 'WebGUI port', ''].map(h => <th key={h} style={TH}>{h}</th>)}</tr></thead>
            <tbody>
              {(n.instances ?? []).map(i => (
                <tr key={i.name ?? ''}>
                  <td style={{ ...CELL, ...MONO }}>{cell(i.name)}</td>
                  <td style={CELL}>{cell(i.server)}</td>
                  <td style={{ ...CELL, ...MONO }}>{cell(i.dcs_port)}</td>
                  <td style={{ ...CELL, ...MONO }}>{cell(i.webgui_port)}</td>
                  <td style={{ ...CELL, textAlign: 'right' }}>
                    {a.del && i.server && <ConfirmBtn label="" icon={<Trash2 size={10} />} confirm={`Delete server ${i.server}? DCS is shut down first.`}
                      onConfirm={() => void runner.act('delete_server', { server_name: i.server }, i.server ?? '')} />}
                  </td>
                </tr>
              ))}
            </tbody>
          </table>
        )}
      </div>
    </Card>
  )
}

export default function NodesTab({ fleet }: { fleet: BotFleet }) {
  const canNodes = !!useAction('restart_node')
  if (!fleet.nodes.length) return <Empty>No nodes reported.</Empty>
  return (
    <div style={{ display: 'flex', flexDirection: 'column', gap: 16 }}>
      {!canNodes && <Empty>Node controls are for full admins.</Empty>}
      {fleet.nodes.map(n => <NodeCard key={n.name} n={n} fleet={fleet} />)}
    </div>
  )
}
