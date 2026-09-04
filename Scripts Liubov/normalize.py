"""Map substance strings from five vocabularies onto one key.

The vocabularies disagree in predictable ways:

  ULCM        ATC-style, upper case, combinations as "A, B"       AMOXICILLIN
  EDQM CEP    Ph. Eur. monograph titles, salt/hydrate explicit     Amoxicillin trihydrate
  EMA         INN, combinations slash-separated                    amoxicillin / clavulanic acid
  HPRA        INN, occasional brand contamination                  Amoxicillin (as trihydrate)
  BfArM       German spelling                                      Amoxicillin-Trihydrat

Every transformation is recorded in `trace` so QC can audit why two strings
collapsed. Silent normalisation is how a supply-chain count quietly doubles.
"""
from __future__ import annotations

import re
import unicodedata
from dataclasses import dataclass, field

# --- counter-ions, esters, hydrates -----------------------------------------
# Stripped only when they appear as trailing tokens.
SALT_TOKENS = {
    # inorganic counter-ions
    "sodium", "potassium", "calcium", "magnesium", "zinc", "aluminium", "aluminum",
    "lithium", "ammonium", "meglumine", "lysine", "arginine", "diolamine", "olamine",
    "trometamol", "tromethamine", "choline", "benzathine", "procaine",
    # acids
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
    # esters / prodrug tails
    "propionate", "dipropionate", "valerate", "acetonide", "palmitate",
    "stearate", "decanoate", "enantate", "enanthate", "cypionate", "undecanoate",
    "furoate", "butyrate", "caproate", "hexanoate", "pivalate", "axetil",
    "proxetil", "medoxomil", "cilexetil", "fosil", "etexilate", "dimeglumine",
    # hydration / solvation
    "monohydrate", "dihydrate", "trihydrate", "tetrahydrate", "pentahydrate",
    "hexahydrate", "heptahydrate", "hemihydrate", "sesquihydrate", "hydrate",
    "anhydrous", "hydrated", "dried", "heptahydrated",
    # halides (see FORBIDDEN below - these are drugs in their own right sometimes)
    "chloride", "bromide", "iodide",
}

# Substances where the "counter-ion" IS the drug. Stripping these turns
# "magnesium sulfate" into "magnesium" and merges three ULCM rows into one.
# The B05/A12 electrolyte block on the Union list makes this a real hazard,
# not a theoretical one.
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

# INN vs USAN vs British usage, and genuine spelling variants.
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

# Applied before tokenisation.
CHAR_FIXES = [
    (r"\bsulph", "sulf"),
    (r"\boe", "e"),          # oestradiol -> estradiol
    (r"(?<=[a-z])oe", "e"),  # foetal-style internal oe
    (r"\bph(?=osph)", "ph"), # no-op guard, keeps phosph* intact
]

_SPLIT = re.compile(r"\s*(?:/|\+|;|\band\b|\bund\b|,)\s*")
# Ph. Eur. monograph titles include systematic chemical names whose commas,
# slashes and hyphens are notation, not separators. "1,2-dihydrotriamcinolone"
# split on the comma becomes ["1", "2 dihydrotriamcinolone"] and the key is
# silently wrong. Detect the notation and refuse to split.
_CHEM_NOTATION = re.compile(
    r"\d\s*,\s*\d"          # 1,2-  (6,7)-
    r"|\d\s*-\s*[a-z]"       # 3-hydroxymethyl
    r"|[a-z]\s*-\s*\d"       # ceph-3
    r"|[-(]\s*(?:alpha|beta|gamma|cis|trans|ortho|meta|para|[rs])\s*[-)]"
    r"|\d/\d",               # 640/2
    re.IGNORECASE,
)
_PARENS = re.compile(r"\([^)]*\)")
_NONWORD = re.compile(r"[^a-z0-9 ]+")
_WS = re.compile(r"\s+")

# Qualifiers that carry no identity information.
NOISE_TOKENS = {
    "as", "form", "salt", "base", "free", "ph", "eur", "usp", "bp", "recombinant",
    "human", "purified", "concentrate", "solution", "powder", "injection",
}

# Physical-grade and processing descriptors. The CEP dump appends these after a
# comma ("Ampicillin, compacted"), so a comma split turns them into a second
# active substance and the key becomes "ampicillin|compacted".
GRADE_TOKENS = {
    "compacted", "micronised", "micronized", "granulated", "granular",
    "crystalline", "amorphous", "dried", "heavy", "light", "coarse", "fine",
    "milled", "spray", "dried", "sterile", "pyrogen", "densified", "powdered",
    "monohydrated", "hydrous", "extra", "pure", "grade", "type", "form",
    # polymorph labels: "Imatinib mesilate, form beta"
    "alpha", "beta", "gamma", "delta", "polymorph",
}

# Whole components that a comma/plus split can produce but which are not active
# substances. "Sodium chloride, sterile" must not become a two-substance combo.
NOISE_COMPONENTS = {
    "sterile", "isotonic", "hypertonic", "hypotonic", "water", "for injection",
    "solution", "concentrate", "powder", "diluent", "vehicle", "excipients",
    "others", "other", "combinations", "combination", "plain", "unspecified",
    "and", "or", "with",
} | GRADE_TOKENS
MIN_COMPONENT_LEN = 4

# Bare cations. If stripping a counter-ion leaves only one of these, the thing
# that was stripped WAS the drug: magnesium stearate is not magnesium. Encoding
# this as a rule rather than a list catches the cases nobody enumerated.
CATIONS = {
    "magnesium", "calcium", "sodium", "potassium", "zinc", "iron", "ferrous",
    "ferric", "aluminium", "aluminum", "lithium", "ammonium", "copper",
    "manganese", "silver", "barium", "bismuth", "strontium", "chromium",
}


@dataclass
class Norm:
    key: str                      # canonical single-substance key, or "a|b" for combos
    components: tuple[str, ...]   # sorted canonical components
    is_combination: bool
    trace: list[str] = field(default_factory=list)
    raw: str = ""


def _presplit_clean(s: str) -> str:
    """Case/accent-fold and drop parentheticals, but keep the separators."""
    s = unicodedata.normalize("NFKD", str(s))
    s = "".join(ch for ch in s if not unicodedata.combining(ch))
    s = s.lower().strip()
    s = _PARENS.sub(" ", s)
    return _WS.sub(" ", s).strip()


def _basic_clean(s: str) -> str:
    s = unicodedata.normalize("NFKD", str(s))
    s = "".join(ch for ch in s if not unicodedata.combining(ch))
    s = s.lower().strip()
    s = _PARENS.sub(" ", s)          # "amoxicillin (as trihydrate)" -> "amoxicillin"
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
        # "magnesium stearate" -> "magnesium" would say the cation is the drug
        if " ".join(candidate) in CATIONS:
            trace.append(f"salt-strip skipped: '{toks[-1]}' is the drug, "
                         f"'{candidate[0]}' is the counter-ion")
            break
        trace.append(f"stripped trailing '{toks[-1]}'")
        toks = candidate
        if " ".join(toks) in FORBIDDEN_STRIP:
            break
    # "clopidogrel hydrogen sulfate" leaves a dangling "hydrogen" once the
    # sulfate goes; it is part of the counter-ion, never of the moiety
    while len(toks) > 1 and toks[-1] in {"hydrogen", "dihydrogen", "hemi", "mono", "di"}:
        trace.append(f"stripped trailing '{toks[-1]}'")
        toks.pop()
    # drop noise tokens but never let the string empty out
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
    """Canonicalise one substance string. Combinations produce a sorted key."""
    trace: list[str] = []
    if raw is None or (isinstance(raw, float)) or not str(raw).strip():
        return Norm(key="", components=(), is_combination=False, trace=["empty"], raw=str(raw))

    # Split BEFORE punctuation is destroyed: _basic_clean removes "/" and "+",
    # so splitting after it silently welds combinations into one pseudo-substance.
    pre = _presplit_clean(raw)
    if _CHEM_NOTATION.search(pre):
        trace.append("chemical notation detected - not split")
        parts = [pre]
    else:
        parts = [p for p in _SPLIT.split(pre) if p.strip()]
    # A comma split can shatter "amoxicillin, beta lactamase inhibitor" correctly
    # but also "sodium chloride, sterile". Components that canonicalise to noise
    # are dropped rather than treated as a second active substance.
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
    """Vectorised helper -> list of dicts, ready for a DataFrame."""
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
