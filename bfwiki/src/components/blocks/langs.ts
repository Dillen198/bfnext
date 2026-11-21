/** Fenced-block languages drawn as live mockups (see MockupBlock). */
export const MOCKUP_LANGS = ['f10menu', 'dcsmessage', 'cockpitpanel', 'wheel', 'steps'] as const
export type MockupLang = (typeof MOCKUP_LANGS)[number]

export function isMockupLang(lang: string): lang is MockupLang {
  return (MOCKUP_LANGS as readonly string[]).includes(lang)
}
