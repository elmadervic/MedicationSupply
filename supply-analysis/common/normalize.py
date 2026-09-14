"""
Normalize substance names across different vocabularies.

Examples of the different vocabularies:

  ULCM        ATC-style, upper case, combinations as "A, B"       AMOXICILLIN
  EDQM CEP    Ph. Eur. monograph titles, salt/hydrate explicit     Amoxicillin trihydrate
  EMA         INN, combinations slash-separated                    amoxicillin / clavulanic acid
  HPRA        INN, occasional brand contamination                  Amoxicillin (as trihydrate)
  BfArM       German spelling                                      Amoxicillin-Trihydrat
"""
from __future__ import annotations

import re
import unicodedata
from dataclasses import dataclass, field

SALT_TOKENS = {
    "sodium", "potassium", "calcium", "magnesium", "zinc", "aluminium", "aluminum",
    "lithium", "ammonium", "meglumine", "lysine", "arginine", "diolamine", "olamine",
    "trometamol", "tromethamine", "choline", "benzathine", "procaine",
    "hydrochloride", "hcl", "dihydrochloride", "hydrobromide", "hydroiodide",
    "sulfate", "sulphate", "hemisulfate", "hemisulphate", "bisulfate",
    "phosphate", "diphosphate", "hydrogenphosphate", "nitrate", "acetate",
    "trifluoroacetate", "citrate", "dihydrogencitrate", "tartrate", "bitartrate",
    "hydrogentartrate", "maleate", "fumarate", "hemifumarate", "succinate",
    "malate", "lactate", "gluconate", "glucuronate", "mesylate", "mesilate",
    "besylate", "besilate", "tosylate", "tosilate", "esylate", "camsylate",
    "camsilate", "edisylate", "isetionate", "pamoate", "embonate", "napsylate",
    "oxalate", "adipate", "aspartate", "glutamate", "salicylate", "benzoate",
    "carbonate", "bicarbonate", "borate", "gluceptate", "xinafoate", "teoclate",
    "orotate", "picosulfate", "sulfonate", "tannate", "thiocyanate",
    "propionate", "dipropionate", "valerate", "acetonide", "palmitate",
    "stearate", "decanoate", "enantate", "enanthate", "cypionate", "undecanoate",
    "furoate", "butyrate", "caproate", "hexanoate", "pivalate", "axetil",
    "proxetil", "medoxomil", "cilexetil", "fosil", "etexilate", "dimeglumine",
    "monohydrate", "dihydrate", "trihydrate", "tetrahydrate", "pentahydrate",
    "hexahydrate", "heptahydrate", "hemihydrate", "sesquihydrate", "hydrate",
    "anhydrous", "hydrated", "dried", "heptahydrated",
    "chloride", "bromide", "iodide",
}

FORBIDDEN_STRIP = {
    "magnesium sulfate", "magnesium sulphate", "magnesium chloride",
    "potassium chloride", "sodium chloride", "calcium chloride",
    "calcium gluconate", "calcium carbonate", "sodium bicarbonate",
    "sodium citrate", "sodium acetate", "sodium lactate", "sodium phosphate",
    "potassium phosphate", "ferrous sulfate", "ferrous sulphate",
    "zinc sulfate", "zinc sulphate", "sodium nitrate", "sodium thiosulfate",
    "ammonium chloride", "potassium citrate", "sodium fluoride",
    "silver nitrate", "barium sulfate", "barium sulphate",
    "sodium perchlorate", "potassium iodide", "sodium iodide",
}

SYNONYMS = {
    "acetaminophen": "paracetamol",
    "albuterol": "salbutamol",
    "epinephrine": "adrenaline",
    "norepinephrine": "noradrenaline",
    "frusemide": "furosemide",
    "lignocaine": "lidocaine",
    "rifampin": "rifampicin",
    "glyburide": "glibenclamide",
    "meperidine": "pethidine",
    "isoproterenol": "isoprenaline",
    "cyclosporin": "ciclosporin",
    "cyclosporine": "ciclosporin",
    "amoxycillin": "amoxicillin",
    "cephalexin": "cefalexin",
    "cephazolin": "cefazolin",
    "cephradine": "cefradine",
    "oestradiol": "estradiol",
    "oestrogen": "estrogen",
    "thyroxine": "levothyroxine",
    "vitamin b12": "cyanocobalamin",
    "clavulanate": "clavulanic acid",
    "dimethicone": "dimeticone",
    "beclomethasone": "beclometasone",
    "hydroxycobalamin": "hydroxocobalamin",
    "sulphamethoxazole": "sulfamethoxazole",
    "amphotericin b": "amphotericin",
    "human coagulation factor viii": "coagulation factor viii",
    "human coagulation factor ix": "coagulation factor ix",
}

CHAR_FIXES = [
    (r"\bsulph", "sulf"),
    (r"\boe", "e"),
    (r"(?<=[a-z])oe", "e"),
    (r"\bph(?=osph)", "ph"),
]

_SPLIT = re.compile(r"\s*(?:/|\+|;|\band\b|\bund\b|,)\s*")
_CHEM_NOTATION = re.compile(
    r"\d\s*,\s*\d"
    r"|\d\s*-\s*[a-z]"
    r"|[a-z]\s*-\s*\d"
    r"|[-(]\s*(?:alpha|beta|gamma|cis|trans|ortho|meta|para|[rs])\s*[-)]"
    r"|\d/\d",
    re.IGNORECASE,
)
_PARENS = re.compile(r"\([^)]*\)")
_NONWORD = re.compile(r"[^a-z0-9 ]+")
_WS = re.compile(r"\s+")

NOISE_TOKENS = {
    "as", "form", "salt", "base", "free", "ph", "eur", "usp", "bp", "recombinant",
    "human", "purified", "concentrate", "solution", "powder", "injection",
}

GRADE_TOKENS = {
    "compacted", "micronised", "micronized", "granulated", "granular",
    "crystalline", "amorphous", "dried", "heavy", "light", "coarse", "fine",
    "milled", "spray", "dried", "sterile", "pyrogen", "densified", "powdered",
    "monohydrated", "hydrous", "extra", "pure", "grade", "type", "form",
    "alpha", "beta", "gamma", "delta", "polymorph",
}

NOISE_COMPONENTS = {
    "sterile", "isotonic", "hypertonic", "hypotonic", "water", "for injection",
    "solution", "concentrate", "powder", "diluent", "vehicle", "excipients",
    "others", "other", "combinations", "combination", "plain", "unspecified",
    "and", "or", "with",
} | GRADE_TOKENS
MIN_COMPONENT_LEN = 4

CATIONS = {
    "magnesium", "calcium", "sodium", "potassium", "zinc", "iron", "ferrous",
    "ferric", "aluminium", "aluminum", "lithium", "ammonium", "copper",
    "manganese", "silver", "barium", "bismuth", "strontium", "chromium",
}


@dataclass
class Norm:
    key: str
    components: tuple[str, ...]
    is_combination: bool
    trace: list[str] = field(default_factory=list)
    raw: str = ""


def _presplit_clean(s: str) -> str:
    s = unicodedata.normalize("NFKD", str(s))
    s = "".join(ch for ch in s if not unicodedata.combining(ch))
    s = s.lower().strip()
    s = _PARENS.sub(" ", s)
    return _WS.sub(" ", s).strip()


def _basic_clean(s: str) -> str:
    s = unicodedata.normalize("NFKD", str(s))
    s = "".join(ch for ch in s if not unicodedata.combining(ch))
    s = s.lower().strip()
    s = _PARENS.sub(" ", s)
    s = s.replace("-", " ").replace("\u2013", " ")
    for pat, rep in CHAR_FIXES:
        s = re.sub(pat, rep, s)
    s = _NONWORD.sub(" ", s)
    return _WS.sub(" ", s).strip()


def _strip_salts(s: str, trace: list[str]) -> str:
    if s in FORBIDDEN_STRIP:
        trace.append(f"salt-strip skipped (electrolyte): {s}")
        return s
    toks = s.split()
    while len(toks) > 1 and (toks[-1] in SALT_TOKENS or toks[-1] in GRADE_TOKENS):
        candidate = toks[:-1]
        if " ".join(candidate) in CATIONS:
            trace.append(f"salt-strip skipped: '{toks[-1]}' is the drug, "
                         f"'{candidate[0]}' is the counter-ion")
            break
        trace.append(f"stripped trailing '{toks[-1]}'")
        toks = candidate
        if " ".join(toks) in FORBIDDEN_STRIP:
            break
    while len(toks) > 1 and toks[-1] in {"hydrogen", "dihydrogen", "hemi", "mono", "di"}:
        trace.append(f"stripped trailing '{toks[-1]}'")
        toks.pop()
    kept = [t for t in toks if t not in NOISE_TOKENS] or toks
    if kept != toks:
        trace.append(f"dropped noise {sorted(set(toks) - set(kept))}")
    return " ".join(kept)


def _canon_one(s: str, trace: list[str]) -> str:
    s = _basic_clean(s)
    if not s:
        return ""
    if s in SYNONYMS:
        trace.append(f"synonym {s} -> {SYNONYMS[s]}")
        return SYNONYMS[s]
    s = _strip_salts(s, trace)
    if s in SYNONYMS:
        trace.append(f"synonym {s} -> {SYNONYMS[s]}")
        s = SYNONYMS[s]
    return s


def normalise(raw: str) -> Norm:
    trace: list[str] = []
    if raw is None or (isinstance(raw, float)) or not str(raw).strip():
        return Norm(key="", components=(), is_combination=False, trace=["empty"], raw=str(raw))

    pre = _presplit_clean(raw)
    if _CHEM_NOTATION.search(pre):
        trace.append("chemical notation detected - not split")
        parts = [pre]
    else:
        parts = [p for p in _SPLIT.split(pre) if p.strip()]
    comps = []
    for p in parts:
        c = _canon_one(p, trace)
        if not c:
            continue
        if c in NOISE_COMPONENTS or len(c) < MIN_COMPONENT_LEN:
            trace.append(f"dropped non-substance component '{c}'")
            continue
        if c not in comps:
            comps.append(c)
    if not comps:
        return Norm(key="", components=(), is_combination=False, trace=trace + ["no components"], raw=str(raw))

    comps_sorted = tuple(sorted(comps))
    key = "|".join(comps_sorted)
    if len(comps_sorted) > 1:
        trace.append(f"combination of {len(comps_sorted)}")
    return Norm(key=key, components=comps_sorted, is_combination=len(comps_sorted) > 1,
                trace=trace, raw=str(raw))


def normalise_series(values):
    out = []
    for v in values:
        n = normalise(v)
        out.append({
            "raw": n.raw,
            "norm_key": n.key,
            "n_components": len(n.components),
            "is_combination": n.is_combination,
            "trace": "; ".join(n.trace),
        })
    return out
