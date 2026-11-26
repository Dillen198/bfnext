/** A story event, as a server page shows it. */
export interface StoryEvent {
  /** The side the event happens to: its losses, its will. */
  side: 'Blue' | 'Red'
  title: string
  /** What sets it off, in words. */
  when: string
  /** The announcement. `{by}` is the pilots the engine credits. */
  text: string
  effects: string[]
}

/** One side's orders: a heading, then numbered tasks, then the closing word. */
export interface Orders {
  side: 'Blue' | 'Red'
  /** "NATO · Operation GOLDEN FLEECE" */
  from: string
  mission: string
  tasks: { lead: string; body: string }[]
  closing: string[]
}
