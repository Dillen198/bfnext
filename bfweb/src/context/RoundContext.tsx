import { createContext, useContext, useState } from 'react'

interface RoundContextValue {
  selectedRound: number | undefined
  setSelectedRound: (id: number | undefined) => void
}

const RoundContext = createContext<RoundContextValue>({
  selectedRound: undefined,
  setSelectedRound: () => {},
})

export function RoundProvider({ children }: { children: React.ReactNode }) {
  const [selectedRound, setSelectedRound] = useState<number | undefined>(undefined)
  return (
    <RoundContext.Provider value={{ selectedRound, setSelectedRound }}>
      {children}
    </RoundContext.Provider>
  )
}

export function useRound() {
  return useContext(RoundContext)
}
