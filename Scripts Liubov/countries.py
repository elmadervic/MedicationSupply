"""Resolve a country from an ISO code, a name, or a free-text address tail.

Word-boundary matching only. Substring matching is how "India" gets found
inside "Indiana" and a country count quietly shifts - that bug was already
caught once in this project's EPAR pipeline; do not reintroduce it.
"""
from __future__ import annotations

import re
from functools import lru_cache

try:  # optional, gives full ISO coverage
    import pycountry  # type: ignore
    _HAS_PYCOUNTRY = True
except ImportError:  # pragma: no cover
    pycountry = None
    _HAS_PYCOUNTRY = False

# Names and abbreviations that ISO tables miss or spell differently, plus the
# ones that actually turn up in CEP holder strings and EPAR addresses.
ALIASES = {
    "usa": "US", "u s a": "US", "united states of america": "US",
    "us": "US", "uk": "GB", "u k": "GB", "great britain": "GB",
    "england": "GB", "scotland": "GB", "wales": "GB",
    "northern ireland": "GB", "republic of ireland": "IE", "eire": "IE",
    "korea": "KR", "south korea": "KR", "republic of korea": "KR",
    "north korea": "KP", "russia": "RU", "russian federation": "RU",
    "czech republic": "CZ", "czechia": "CZ", "slovak republic": "SK",
    "the netherlands": "NL", "holland": "NL", "nederland": "NL",
    "deutschland": "DE", "germany": "DE", "brd": "DE",
    "espana": "ES", "spain": "ES", "italia": "IT", "italy": "IT",
    "france": "FR", "belgique": "BE", "belgie": "BE", "belgium": "BE",
    "osterreich": "AT", "austria": "AT", "schweiz": "CH", "suisse": "CH",
    "switzerland": "CH", "sverige": "SE", "danmark": "DK", "norge": "NO",
    "suomi": "FI", "polska": "PL", "poland": "PL", "magyarorszag": "HU",
    "hungary": "HU", "romania": "RO", "hrvatska": "HR", "croatia": "HR",
    "slovenija": "SI", "slovenia": "SI", "bulgaria": "BG", "greece": "GR",
    "hellas": "GR", "portugal": "PT", "turkiye": "TR", "turkey": "TR",
    "p r china": "CN", "pr china": "CN", "china": "CN",
    "peoples republic of china": "CN", "prc": "CN",
    "chinese taipei": "TW", "taiwan": "TW", "hong kong": "HK", "macau": "MO",
    "india": "IN", "japan": "JP", "israel": "IL", "canada": "CA",
    "mexico": "MX", "brasil": "BR", "brazil": "BR", "argentina": "AR",
    "australia": "AU", "new zealand": "NZ", "singapore": "SG",
    "malaysia": "MY", "indonesia": "ID", "thailand": "TH", "vietnam": "VN",
    "viet nam": "VN", "bangladesh": "BD", "pakistan": "PK", "iran": "IR",
    "egypt": "EG", "south africa": "ZA", "jordan": "JO", "saudi arabia": "SA",
    "united arab emirates": "AE", "uae": "AE", "puerto rico": "PR",
    "iceland": "IS", "ireland": "IE", "luxembourg": "LU", "malta": "MT",
    "cyprus": "CY", "estonia": "EE", "latvia": "LV", "lithuania": "LT",
    "serbia": "RS", "bosnia and herzegovina": "BA", "north macedonia": "MK",
    "albania": "AL", "ukraine": "UA", "belarus": "BY", "kazakhstan": "KZ",
    "liechtenstein": "LI", "monaco": "MC", "san marino": "SM",
    "netherlands": "NL", "sweden": "SE", "denmark": "DK", "norway": "NO",
    "finland": "FI", "slovakia": "SK",
    # plain forms that pycountry has but the offline fallback would miss
    "united states": "US", "united kingdom": "GB", "china": "CN",
    "germany": "DE", "france": "FR", "italy": "IT", "spain": "ES",
    "belgium": "BE", "austria": "AT", "switzerland": "CH", "greece": "GR",
    "japan": "JP", "india": "IN", "israel": "IL", "canada": "CA",
    "colombia": "CO", "chile": "CL", "peru": "PE", "morocco": "MA",
    "tunisia": "TN", "algeria": "DZ", "kenya": "KE", "nigeria": "NG",
    "philippines": "PH", "sri lanka": "LK", "nepal": "NP", "myanmar": "MM",
    "uzbekistan": "UZ", "georgia": "GE", "armenia": "AM", "azerbaijan": "AZ",
    "moldova": "MD", "montenegro": "ME", "kosovo": "XK",
}

EU_EEA = {
    "AT", "BE", "BG", "HR", "CY", "CZ", "DK", "EE", "FI", "FR", "DE", "GR",
    "HU", "IE", "IT", "LV", "LT", "LU", "MT", "NL", "PL", "PT", "RO", "SK",
    "SI", "ES", "SE", "IS", "LI", "NO",
}

_CLEAN = re.compile(r"[^a-z ]+")


@lru_cache(maxsize=1)
def _name_map() -> dict[str, str]:
    m = dict(ALIASES)
    if _HAS_PYCOUNTRY:
        for c in pycountry.countries:
            for attr in ("name", "official_name", "common_name"):
                v = getattr(c, attr, None)
                if v:
                    m.setdefault(_CLEAN.sub(" ", v.lower()).strip(), c.alpha_2)
    return m


@lru_cache(maxsize=1)
def _iso2_set() -> set[str]:
    if _HAS_PYCOUNTRY:
        return {c.alpha_2 for c in pycountry.countries}
    return set(ALIASES.values())


@lru_cache(maxsize=1)
def _iso3_map() -> dict[str, str]:
    if _HAS_PYCOUNTRY:
        return {c.alpha_3: c.alpha_2 for c in pycountry.countries}
    return {}


def resolve(value: str | None) -> str | None:
    """Return an ISO alpha-2 code, or None if nothing can be resolved.

    Tries, in order: bare ISO2, bare ISO3, full-string name match, then a
    right-to-left word-boundary scan of the string (addresses put the country
    last). Returns None rather than guessing.
    """
    if value is None:
        return None
    s = str(value).strip()
    if not s:
        return None

    up = s.upper()
    if len(up) == 2 and up in _iso2_set():
        return up
    if len(up) == 3 and up in _iso3_map():
        return _iso3_map()[up]

    low = _CLEAN.sub(" ", s.lower())
    low = re.sub(r"\s+", " ", low).strip()
    nm = _name_map()
    if low in nm:
        return nm[low]

    # scan right to left: the country is normally the last address element
    words = low.split()
    for n in (4, 3, 2, 1):
        for i in range(len(words) - n, -1, -1):
            cand = " ".join(words[i:i + n])
            if cand in nm:
                return nm[cand]

    # trailing bare ISO2 token, e.g. "Janssen Pharmaceutica NV BE 2340 Beerse"
    for tok in reversed(re.findall(r"\b[A-Z]{2}\b", s)):
        if tok in _iso2_set():
            return tok
    return None


def is_eu_eea(iso2: str | None) -> bool:
    return iso2 in EU_EEA
