import type { ComponentType, CSSProperties, ReactNode, SVGProps } from 'react'

export interface IconProps extends Omit<SVGProps<SVGSVGElement>, 'ref'> {
  /** Square edge length in px. Matches lucide's `size` prop. */
  size?: number | string
}

/**
 * Anything that can stand in for an icon in this app -- both lucide icons and
 * the ones in this directory satisfy it. Prefer it over `typeof SomeLucideIcon`
 * when typing an icon map, so the two sets stay interchangeable.
 */
export type IconComponent = ComponentType<{
  size?: number | string
  color?: string
  strokeWidth?: number | string
  className?: string
  style?: CSSProperties
}>

/**
 * Shared chassis for the Vector Strike icon set.
 *
 * Deliberately different from lucide's defaults in one respect: square caps and
 * mitred joins. Lucide's rounded terminals read as consumer-app friendly, which
 * is wrong next to a mono-spaced tactical readout -- cut corners match the rest
 * of the chrome. Everything else (24x24 box, 1.5 stroke, `currentColor`, a
 * `size` prop) mirrors lucide exactly so call sites swap one import and stop.
 */
export function createIcon(name: string, paths: ReactNode) {
  function Icon({ size = 24, strokeWidth = 1.5, ...rest }: IconProps) {
    return (
      <svg
        xmlns="http://www.w3.org/2000/svg"
        width={size}
        height={size}
        viewBox="0 0 24 24"
        fill="none"
        stroke="currentColor"
        strokeWidth={strokeWidth}
        strokeLinecap="square"
        strokeLinejoin="miter"
        aria-hidden="true"
        {...rest}
      >
        {paths}
      </svg>
    )
  }
  Icon.displayName = name
  return Icon
}
