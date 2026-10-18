import { Moon, Sun } from '@icons'
import { useTheme } from '../context/theme'

export default function ThemeToggle() {
  const { theme, toggle } = useTheme()
  const label = theme === 'dark' ? 'Switch to light theme' : 'Switch to dark theme'
  return (
    <button type="button" className="icon-btn" onClick={toggle} title={label} aria-label={label}>
      {theme === 'dark' ? <Sun size={14} /> : <Moon size={14} />}
    </button>
  )
}
