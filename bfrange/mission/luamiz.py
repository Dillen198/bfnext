# Copyright (c) 2026 Dillen Weerasinghe. All rights reserved.
# Proprietary and confidential. No license is granted to use, copy, modify, or
# distribute this file. See bfrange/LICENSE and the repository NOTICE file.
"""Minimal parser/serializer for DCS mission Lua tables."""
import re

BS = chr(92)  # backslash

class LuaTable(dict):
    """dict preserving DCS key types (int or str); python dicts keep order."""
    pass

TOKEN = re.compile(r"""
    \s*(?:--[^\n]*)?\s*
    (?P<tok>
        \[\s*(?:"(?:[^"\\]|\\.)*"|-?[0-9]+)\s*\]
      | "(?:[^"\\]|\\.)*"
      | -?[0-9]+\.?[0-9]*(?:[eE][-+]?[0-9]+)?
      | \.[0-9]+(?:[eE][-+]?[0-9]+)?
      | true|false|nil
      | [A-Za-z_][A-Za-z_0-9]*
      | [{}=,;]
    )
""", re.X | re.S)

WS = re.compile(r"(?:\s+|--[^\n]*)+")

def _unescape(s):
    if BS not in s:
        return s
    out = []
    i = 0
    while i < len(s):
        c = s[i]
        if c == BS and i + 1 < len(s):
            n = s[i+1]
            out.append({'n': '\n', 't': '\t', 'r': '\r', '"': '"', "'": "'", BS: BS}.get(n, n))
            i += 2
        else:
            out.append(c)
            i += 1
    return ''.join(out)

class Lexer:
    __slots__ = ('text', 'pos')
    def __init__(self, text):
        self.text = text
        self.pos = 0
    def next(self):
        m = TOKEN.match(self.text, self.pos)
        if m is None:
            m2 = WS.match(self.text, self.pos)
            if m2:
                self.pos = m2.end()
                m = TOKEN.match(self.text, self.pos)
        if m is None:
            return None
        self.pos = m.end()
        return m.group('tok')
    def peek(self):
        p = self.pos
        t = self.next()
        self.pos = p
        return t

def parse_value(lx):
    t = lx.next()
    if t is None:
        raise ValueError("unexpected eof")
    if t == '{':
        tbl = LuaTable()
        nextidx = 1
        while True:
            t2 = lx.peek()
            if t2 is None:
                raise ValueError("unterminated table")
            if t2 == '}':
                lx.next()
                break
            if t2 in (',', ';'):
                lx.next()
                continue
            if t2.startswith('['):
                lx.next()
                inner = t2[1:-1].strip()
                key = _unescape(inner[1:-1]) if inner.startswith('"') else int(inner)
                eq = lx.next()
                if eq != '=':
                    raise ValueError("expected = after key, got %r" % eq)
                tbl[key] = parse_value(lx)
            else:
                save = lx.pos
                t3 = lx.next()
                if lx.peek() == '=':
                    lx.next()
                    key = _unescape(t3[1:-1]) if t3.startswith('"') else t3
                    tbl[key] = parse_value(lx)
                else:
                    lx.pos = save
                    tbl[nextidx] = parse_value(lx)
                    nextidx += 1
        return tbl
    if t.startswith('"'):
        return _unescape(t[1:-1])
    if t == 'true':
        return True
    if t == 'false':
        return False
    if t == 'nil':
        return None
    if re.fullmatch(r'-?[0-9]+', t):
        return int(t)
    try:
        return float(t)
    except ValueError:
        return t

def parse_mission(text):
    """Parse a DCS `name = { ... }` file. Returns (varname, table)."""
    m = re.match(r'\s*(?:local\s+)?([A-Za-z_][A-Za-z_0-9]*)\s*=\s*', text)
    if not m:
        raise ValueError("not a DCS lua assignment file")
    lx = Lexer(text)
    lx.pos = m.end()
    return m.group(1), parse_value(lx)

def esc(s):
    return (s.replace(BS, BS + BS).replace('"', BS + '"')
             .replace('\n', BS + 'n').replace('\r', BS + 'r'))

def fmtnum(v):
    if isinstance(v, bool):
        return 'true' if v else 'false'
    if isinstance(v, int):
        return str(v)
    if v != v or v in (float('inf'), float('-inf')):
        return '0'
    if v == int(v) and abs(v) < 1e15:
        return str(int(v))
    return repr(v)

def ser(v, indent, out):
    pad = '\t' * indent
    if isinstance(v, dict):
        out.append('\n' + '\t' * (indent - 1) + '{\n')
        for k, val in v.items():
            kk = ('[%d]' % k) if isinstance(k, int) else '["%s"]' % esc(k)
            out.append(pad + kk + ' = ')
            ser(val, indent + 1, out)
            out.append((', -- end of ' + kk + '\n') if isinstance(val, dict) else ',\n')
        out.append('\t' * (indent - 1) + '}')
    elif isinstance(v, str):
        out.append('"' + esc(v) + '"')
    elif v is None:
        out.append('nil')
    else:
        out.append(fmtnum(v))
    return out

def dump_mission(varname, tbl):
    return varname + ' = ' + ''.join(ser(tbl, 1, [])) + ' -- end of ' + varname + '\n'
