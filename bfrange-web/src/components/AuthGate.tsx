import { KeyRound, Link as LinkIcon } from '@icons'
import { useAuth } from '../context/AuthContext'

export function LoginPrompt({ why }: { why: string }) {
  const { login } = useAuth()
  return (
    <div className="panel panel-b max-w-xl">
      <div className="flex items-center gap-2 mb-2">
        <KeyRound size={18} style={{ color: 'var(--ball)' }} />
        <span className="caps" style={{ color: 'var(--chalk)' }}>Log in needed</span>
      </div>
      <p className="m-0 mb-3">Log in with Discord {why}. It is the same account you use on the Vector Strike dashboard.</p>
      <button className="btn-range primary" onClick={login}>Log in with Discord</button>
    </div>
  )
}

export function LinkGuidance() {
  const { me } = useAuth()
  return (
    <div className="panel panel-b max-w-2xl">
      <div className="flex items-center gap-2 mb-2">
        <LinkIcon size={18} style={{ color: 'var(--ball)' }} />
        <span className="caps" style={{ color: 'var(--chalk)' }}>Link your DCS account</span>
      </div>
      <p className="m-0">
        You are logged in{me?.name ? <> as <b>{me.name}</b></> : null}, but your Discord account is not linked to a DCS pilot yet, so the range cannot tell which results are yours.
      </p>
      <ol className="mt-3 mb-0 pl-5 flex flex-col gap-1.5 text-[13.5px]">
        <li>Join the Vector Strike Discord and open the bot channel.</li>
        <li>Run the bot's link command (<span className="mono">/linkme</span>). It gives you a short code.</li>
        <li>Join any Vector Strike server in DCS and type the code in chat (<span className="mono">-linkme &lt;code&gt;</span>).</li>
        <li>Come back here and reload. Your record and the spawner unlock.</li>
      </ol>
    </div>
  )
}
