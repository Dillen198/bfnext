"""
Minisign signature verification, for engine releases (autoupdate.py).

A release's manifest.json carries the sha256 of every file in it, so signing
that one file signs the release. deploy/publish-release.ps1 signs it with
`tauri signer sign` -- the same tool and key format Fowl Engine Manager's own
updates use (bfmanager/release.ps1), but a SEPARATE key -- and publishes the
result as manifest.json.sig next to it. The bot refuses any release whose
manifest doesn't verify against the public key pinned in fowlengine.yaml
(`autoupdate.public_key`).

Formats accepted, both for the key and the signature:

  * minisign's own text files:
        untrusted comment: minisign public key: E3B1C9E95405E24F
        RWRP4gVU6cmx46LK...
    and
        untrusted comment: ...
        RURP4gVU6cmx45Wk...          <- "ED" (prehashed) or "Ed" (legacy)
        trusted comment: timestamp:1790380490	file:manifest.json
        aMc4dYWm/A6tDfEM...          <- global signature over sig || trusted comment
  * the same files base64-encoded once more, which is what `tauri signer`
    writes (updater.pub, <file>.sig)
  * for the key, just the bare "RW..." line

Ed25519 comes from `cryptography` when it is installed; DCSServerBot does not
depend on it, so there is a pure-Python verifier (RFC 8032 section 5.1.7) to
fall back on. Verification only -- nothing secret is ever handled here, so the
pure version's lack of constant-time arithmetic doesn't matter.
"""
from __future__ import annotations

import base64
import binascii
import hashlib
from typing import Optional

__all__ = ["parse_public_key", "parse_signature", "verify", "ed25519_verify", "SignatureError"]


class SignatureError(ValueError):
    """The key or signature is malformed, or the signature doesn't match."""


# ---- Ed25519 (verify only) --------------------------------------------------------

_P = 2 ** 255 - 19
_L = 2 ** 252 + 27742317777372353535851937790883648493
_D = -121665 * pow(121666, _P - 2, _P) % _P
_SQRT_M1 = pow(2, (_P - 1) // 4, _P)


def _recover_x(y: int, sign: int) -> Optional[int]:
    if y >= _P:
        return None
    x2 = (y * y - 1) * pow(_D * y * y + 1, _P - 2, _P) % _P
    if x2 == 0:
        return None if sign else 0
    x = pow(x2, (_P + 3) // 8, _P)
    if (x * x - x2) % _P != 0:
        x = x * _SQRT_M1 % _P
    if (x * x - x2) % _P != 0:
        return None
    if (x & 1) != sign:
        x = _P - x
    return x


def _decompress(s: bytes):
    if len(s) != 32:
        return None
    y = int.from_bytes(s, "little")
    sign = y >> 255
    y &= (1 << 255) - 1
    x = _recover_x(y, sign)
    if x is None:
        return None
    return (x, y, 1, x * y % _P)


def _add(p, q):
    a = (p[1] - p[0]) * (q[1] - q[0]) % _P
    b = (p[1] + p[0]) * (q[1] + q[0]) % _P
    c = 2 * p[3] * q[3] * _D % _P
    d = 2 * p[2] * q[2] % _P
    e, f, g, h = b - a, d - c, d + c, b + a
    return (e * f, g * h, f * g, e * h)


def _mul(s: int, p):
    q = (0, 1, 1, 0)
    while s > 0:
        if s & 1:
            q = _add(q, p)
        p = _add(p, p)
        s >>= 1
    return q


def _equal(p, q) -> bool:
    return ((p[0] * q[2] - q[0] * p[2]) % _P == 0
            and (p[1] * q[2] - q[1] * p[2]) % _P == 0)


_GY = 4 * pow(5, _P - 2, _P) % _P
_GX = _recover_x(_GY, 0)
_G = (_GX, _GY, 1, _GX * _GY % _P)


def _ed25519_verify_pure(public: bytes, msg: bytes, sig: bytes) -> bool:
    if len(public) != 32 or len(sig) != 64:
        return False
    a = _decompress(public)
    if a is None:
        return False
    r = _decompress(sig[:32])
    if r is None:
        return False
    s = int.from_bytes(sig[32:], "little")
    if s >= _L:
        return False
    h = int.from_bytes(hashlib.sha512(sig[:32] + public + msg).digest(), "little") % _L
    return _equal(_mul(s, _G), _add(r, _mul(h, a)))


def ed25519_verify(public: bytes, msg: bytes, sig: bytes, *, pure: bool = False) -> bool:
    """True if `sig` is `public`'s Ed25519 signature of `msg`."""
    if not pure:
        try:
            from cryptography.exceptions import InvalidSignature
            from cryptography.hazmat.primitives.asymmetric.ed25519 import Ed25519PublicKey
        except Exception:  # noqa: BLE001 - not installed: the pure version below
            pass
        else:
            try:
                Ed25519PublicKey.from_public_bytes(public).verify(sig, msg)
                return True
            except (InvalidSignature, ValueError):
                return False
    return _ed25519_verify_pure(public, msg, sig)


# ---- minisign files -------------------------------------------------------------

def _b64(s: str) -> bytes:
    try:
        return base64.b64decode(s.strip(), validate=True)
    except (binascii.Error, ValueError) as ex:
        raise SignatureError(f"not base64: {ex}") from None


def _unwrap(text: str) -> str:
    """`tauri signer` base64-encodes the whole minisign file once more."""
    t = (text or "").strip()
    if t and "\n" not in t and not t.startswith(("RW", "untrusted comment:")):
        try:
            inner = base64.b64decode(t, validate=True).decode("utf-8")
        except (binascii.Error, ValueError, UnicodeDecodeError):
            return t
        if inner.startswith("untrusted comment:"):
            return inner.strip()
    return t


def parse_public_key(text: str) -> tuple[bytes, bytes]:
    """(key id, 32-byte Ed25519 key) from a minisign public key in any of the
    forms this module accepts."""
    t = _unwrap(text)
    lines = [ln.strip() for ln in t.replace("\r", "").split("\n") if ln.strip()]
    if not lines:
        raise SignatureError("empty public key")
    line = lines[-1] if lines[0].startswith("untrusted comment:") else lines[0]
    raw = _b64(line)
    if len(raw) != 42 or raw[:2] != b"Ed":
        raise SignatureError("not a minisign Ed25519 public key")
    return raw[2:10], raw[10:]


def parse_signature(text: str) -> dict:
    """The parts of a minisign signature file."""
    t = _unwrap(text)
    lines = [ln.rstrip("\r") for ln in t.split("\n")]
    lines = [ln for ln in lines if ln.strip()]
    if len(lines) < 4 or not lines[0].startswith("untrusted comment:") \
            or not lines[2].startswith("trusted comment: "):
        raise SignatureError("not a minisign signature file")
    raw = _b64(lines[1])
    if len(raw) != 74 or raw[:2] not in (b"Ed", b"ED"):
        raise SignatureError("unsupported minisign signature algorithm")
    global_sig = _b64(lines[3])
    if len(global_sig) != 64:
        raise SignatureError("malformed minisign global signature")
    return {
        "prehashed": raw[:2] == b"ED",
        "key_id": raw[2:10],
        "sig": raw[10:],
        "trusted_comment": lines[2][len("trusted comment: "):],
        "global_sig": global_sig,
    }


def verify(data: bytes, signature: str, public_key: str, *, pure: bool = False) -> str:
    """Check `data` against a minisign signature made by `public_key`.
    Returns the (now authenticated) trusted comment; raises SignatureError."""
    key_id, pk = parse_public_key(public_key)
    s = parse_signature(signature)
    if s["key_id"] != key_id:
        raise SignatureError(f"signed with key {s['key_id'][::-1].hex().upper()}, not the pinned key "
                             f"{key_id[::-1].hex().upper()}")
    msg = hashlib.blake2b(data, digest_size=64).digest() if s["prehashed"] else data
    if not ed25519_verify(pk, msg, s["sig"], pure=pure):
        raise SignatureError("signature does not match the data")
    # The trusted comment is signed separately; without this check it could be
    # swapped for anything.
    if not ed25519_verify(pk, s["sig"] + s["trusted_comment"].encode("utf-8"), s["global_sig"], pure=pure):
        raise SignatureError("trusted comment signature does not match")
    return s["trusted_comment"]
