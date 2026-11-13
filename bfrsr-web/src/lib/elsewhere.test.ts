import { describe, expect, it } from 'vitest'
import { VS_URLS } from '@shared/web/urls'
import { OTHER_SERVERS, elsewhere, handOffUrl } from './elsewhere'

describe('elsewhere', () => {
  it('sends Levant Ablaze to the main dashboard and keeps Red Storm here', () => {
    expect(elsewhere('vs1')).toBe(VS_URLS.dashboard)
    expect(elsewhere('redstorm')).toBeUndefined()
    expect(elsewhere(undefined)).toBeUndefined()
  })
  it('lists only the campaigns that live on another dashboard', () => {
    expect(OTHER_SERVERS.map(s => s.instanceId)).toEqual(['vs1'])
  })
  it('lands on the same page there, without this app\'s ?instance=', () => {
    expect(handOffUrl(VS_URLS.dashboard, { pathname: '/kills', search: '?instance=redstorm&side=red', hash: '#x' }))
      .toBe(`${VS_URLS.dashboard}/kills?side=red#x`)
    expect(handOffUrl(VS_URLS.dashboard, { pathname: '/', search: '', hash: '' })).toBe(`${VS_URLS.dashboard}/`)
  })
})
