import React from 'react'
import { useInView } from '../hooks/useInView'

// Wraps children in a div that fades + slides up the first time it scrolls
// into view. `delay` staggers siblings (e.g. cards in a grid) in milliseconds.
export default function Reveal({
  children,
  delay = 0,
  className,
  style,
  as: Tag = 'div',
}: {
  children: React.ReactNode
  delay?: number
  className?: string
  style?: React.CSSProperties
  as?: keyof JSX.IntrinsicElements
}) {
  const { ref, inView } = useInView<HTMLDivElement>()

  return (
    <Tag
      // @ts-expect-error -- ref type varies with the polymorphic `as` tag, safe at runtime
      ref={ref}
      className={className}
      style={{
        ...style,
        opacity: inView ? 1 : 0,
        transform: inView ? 'translateY(0)' : 'translateY(24px)',
        transition: `opacity 0.6s ease-out ${delay}ms, transform 0.6s ease-out ${delay}ms`,
      }}
    >
      {children}
    </Tag>
  )
}
