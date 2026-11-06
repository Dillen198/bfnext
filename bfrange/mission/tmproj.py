"""DCS terrain projections: WGS84 transverse Mercator, DCS x = north, z = east.

Each terrain's parameters come from its terrain.lua (the same numbers pydcs
ships). The forward and inverse use Krüger's series, good to well under a
millimetre at range distances, so no per-terrain fit is needed.
"""

import math

THEATRES = {
    "Caucasus": dict(central_meridian=33, false_easting=-99516.9999999732,
                     false_northing=-4998114.999999984, scale_factor=0.9996),
    "MarianaIslands": dict(central_meridian=147, false_easting=238417.99999989968,
                           false_northing=-1491840.000000048, scale_factor=0.9996),
}

_A = 6378137.0
_F = 1 / 298.257223563
_N = _F / (2 - _F)
_AA = _A / (1 + _N) * (1 + _N ** 2 / 4 + _N ** 4 / 64)
_ALPHA = (
    _N / 2 - 2 * _N ** 2 / 3 + 5 * _N ** 3 / 16,
    13 * _N ** 2 / 48 - 3 * _N ** 3 / 5,
    61 * _N ** 3 / 240,
)
_BETA = (
    _N / 2 - 2 * _N ** 2 / 3 + 37 * _N ** 3 / 96,
    _N ** 2 / 48 + _N ** 3 / 15,
    17 * _N ** 3 / 480,
)
_DELTA = (
    2 * _N - 2 * _N ** 2 / 3 - 2 * _N ** 3,
    7 * _N ** 2 / 3 - 8 * _N ** 3 / 5,
    56 * _N ** 3 / 15,
)
_E2N = 2 * math.sqrt(_N) / (1 + _N)


class Projection:
    def __init__(self, theatre):
        p = THEATRES[theatre]
        self.theatre = theatre
        self.lon0 = math.radians(p["central_meridian"])
        self.fe = p["false_easting"]
        self.fn = p["false_northing"]
        self.k0 = p["scale_factor"]

    def ll2xz(self, lat, lon):
        """lat/lon in degrees -> DCS (x north, z east) in metres."""
        phi, lam = math.radians(lat), math.radians(lon) - self.lon0
        s = math.sin(phi)
        t = math.sinh(math.atanh(s) - _E2N * math.atanh(_E2N * s))
        xi = math.atan2(t, math.cos(lam))
        eta = math.atanh(math.sin(lam) / math.sqrt(1 + t * t))
        e, n = eta, xi
        for j, a in enumerate(_ALPHA, 1):
            e += a * math.cos(2 * j * xi) * math.sinh(2 * j * eta)
            n += a * math.sin(2 * j * xi) * math.cosh(2 * j * eta)
        return (self.fn + self.k0 * _AA * n, self.fe + self.k0 * _AA * e)

    def xz2ll(self, x, z):
        """DCS (x north, z east) in metres -> lat/lon in degrees."""
        xi = (x - self.fn) / (self.k0 * _AA)
        eta = (z - self.fe) / (self.k0 * _AA)
        xp, ep = xi, eta
        for j, b in enumerate(_BETA, 1):
            xp -= b * math.sin(2 * j * xi) * math.cosh(2 * j * eta)
            ep -= b * math.cos(2 * j * xi) * math.sinh(2 * j * eta)
        chi = math.asin(math.sin(xp) / math.cosh(ep))
        phi = chi
        for j, d in enumerate(_DELTA, 1):
            phi += d * math.sin(2 * j * chi)
        lam = math.atan2(math.sinh(ep), math.cos(xp))
        return (math.degrees(phi), math.degrees(lam + self.lon0))
