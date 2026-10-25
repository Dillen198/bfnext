import type { Symbol as MsSymbol } from "milsymbol";
import { useEffect, useRef, type ReactElement } from "react";

export interface SymbolProps {
  symbol: MsSymbol;
}

export default function Symbol({ symbol }: SymbolProps): ReactElement {
  const ref = useRef<HTMLSpanElement>(null);

  useEffect(() => {
    const el = ref.current;
    if (el == null) {
      return;
    }

    while (el.firstChild != null) {
      el.removeChild(el.firstChild);
    }

    el.appendChild(symbol.asDOM());
  }, [symbol]);

  return <span className="block" ref={ref} />;
  // return <img className="block" src={`data:image/svg+xml;utf8,${encodeURIComponent(symbol.asSVG())}`} />
}
