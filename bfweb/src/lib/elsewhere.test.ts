import { describe, expect, it } from 'vitest'
import { VS_URLS } from '@shared/web/urls'
import { elsewhere, handOffUrl, onAdminPage } from './elsewhere'

describe('elsewhere', () => {
  it('sends Red Storm to its own dashboard and keeps Levant Ablaze here', () => {
    expect(elsewhere('redstorm')).toBe(VS_URLS.redstorm)
    expect(elsewhere('vs1')).toBeUndefined()
    expect(elsewhere('a-test-box')).toBeUndefined()
    expect(elsewhere(undefined)).toBeUndefined()
  })
  it('keeps the admin pages here', () => {
    expect(onAdminPage('/admin')).toBe(true)
    expect(onAdminPage('/admin/config')).toBe(true)
    expect(onAdminPage('/administrator')).toBe(false)
    expect(onAdminPage('/leaderboard')).toBe(false)
  })
  it('lands on the same page there, without the ?instance= that chose it', () => {
    expect(handOffUrl(VS_URLS.redstorm, { pathname: '/leaderboard', search: '?instance=redstorm', hash: '' }))
      .toBe(`${VS_URLS.redstorm}/leaderboard`)
    expect(handOffUrl(VS_URLS.redstorm, { pathname: '/pilots', search: '?ucid=abc&instance=redstorm', hash: '' }))
      .toBe(`${VS_URLS.redstorm}/pilots?ucid=abc`)
  })
})
