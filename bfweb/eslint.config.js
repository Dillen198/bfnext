import js from '@eslint/js'
import globals from 'globals'
import reactHooks from 'eslint-plugin-react-hooks'
import reactRefresh from 'eslint-plugin-react-refresh'
import tseslint from 'typescript-eslint'
import jsxA11y from 'eslint-plugin-jsx-a11y'
import { defineConfig, globalIgnores } from 'eslint/config'

// jsx-a11y's recommended rules, reported as warnings: they flag real problems
// (clickable divs, unlabelled icon buttons) without failing CI on the long
// tail. Fix what you touch.
const a11y = Object.fromEntries(
  Object.entries(jsxA11y.flatConfigs.recommended.rules).map(([rule, v]) => {
    const [level, ...opts] = Array.isArray(v) ? v : [v]
    return [rule, level === 'off' ? 'off' : ['warn', ...opts]]
  }),
)

export default defineConfig([
  globalIgnores(['dist']),
  {
    files: ['**/*.{ts,tsx}'],
    extends: [
      js.configs.recommended,
      tseslint.configs.recommended,
      reactHooks.configs.flat.recommended,
      reactRefresh.configs.vite,
    ],
    plugins: { 'jsx-a11y': jsxA11y },
    rules: a11y,
    languageOptions: {
      ecmaVersion: 2020,
      globals: globals.browser,
    },
  },
])
