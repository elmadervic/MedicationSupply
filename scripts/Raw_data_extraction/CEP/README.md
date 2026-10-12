# CEP — EDQM Certificates of Suitability

Raw + cleaned data for the register-vs-trade match on antibiotics, using the EDQM
**Certificate of Suitability (CEP)** database as a *third* register (alongside BfArM
and EudraGMDP). A CEP certifies that an API manufacturer's substance is suitably
controlled by the relevant European Pharmacopoeia monograph — i.e. it is a
**substance-level, EU-wide list of who is certified to supply an API to the EU market**,
and it names the holder's country (India/China included).

## Why CEP (vs the other registers)
- **Substance-level, not HS-6.** Keyed by substance name → maps cleanly onto ATC/INN and
  onto the 4 antibiotic HS-6 classes. This sidesteps the *population mismatch* that made the
  BfArM/EudraGMDP ↔ HS-6 match not close (the BfArM comparison is section 6 of
  the trade notebook `Baci_systemic_risk.ipynb`).
- **Broader population than the critical-only register.** CEP lists critical *and*
  non-critical antibiotics of the same chemical class, so it is closer to what the HS-6
  trade line actually carries.
- **Caveat (same as the other registers).** A CEP is an *eligibility/quality* certificate,
  not a supply or volume measure. A holder with a CEP *may* supply the EU market, not that it
  *does*, nor how much. So the CEP↔trade match still measures certified-origin vs actual-flow,
  not tonnage.

## Provenance (raw)
- **Source:** EDQM CEP certification database — https://extranet.edqm.eu/publications/recherches_CEP.shtml
- **Download endpoint:** https://extranet.edqm.eu/4DLink1/4DCGI/EXPORT_WEB_CEP.txt
  (the site's own "download the online certification database" export; refreshed daily ~12:00–13:00 CET)
- **Downloaded:** 2026-07-28
- **File:** `raw/EXPORT_WEB_CEP.txt` — the **full** database, saved verbatim, unmodified.
- **Format:** tab-separated, UTF-8 with BOM, CRLF line endings.
- **Scope:** all CEP types (Chemical / TSE / Herbal) and all statuses (Valid, Withdrawn,
  Expired, Suspended …). Filtering to antibiotics / Valid happens downstream in the notebook.
- **Rows:** 9,388.

### Columns (raw)
`Monograph Number`, `Substance`, `Type CEP`, `Certificate (CEP) Holder`,
`Holder SPOR ORG-ID / SPOR LOC-ID`, `Certificate (CEP) Number`, `Issue Date CEP`,
`Status CEP`, `Renewal due`, `End date CEP`, `Closure Date of last Procedure`.

> The holder **country** is embedded as the last token of `Certificate (CEP) Holder`
> (ISO alpha-2), e.g. `Glaxo Wellcome London GB` → `GB`. The cleaning notebook extracts it
> and converts to ISO-3 (BACI codes).

## Usage
Run both steps from this folder (the notebook reads `raw/` relative to the working directory):
```bash
cd CEP
python download_cep.py          # → raw/EXPORT_WEB_CEP.txt
jupyter nbconvert --to notebook --execute --inplace cep_clean.ipynb   # or run it interactively
```
EDQM serves only the **current** database. A new download gives a newer version than the
2026-07-28 snapshot used in the paper (e.g. 9,509 rows on 2026-10-12, compared with 9,388).

## Pipeline (steps kept separate on purpose)
1. `download_cep.py` → `raw/EXPORT_WEB_CEP.txt`: the untouched download.
2. `cep_clean.ipynb` — the **only** transformation step. Loads the raw dump, parses the
   holder country, classifies each substance into one of the 4 antibiotic HS-6 classes by an
   explicit, auditable keyword map, and writes:
   - `cep_antibiotics_clean.csv` — one row per CEP in the 4 classes (long, with iso3, hs, status …)
   - `cep_origins_by_class.csv` — per HS class → certified origin countries + CEP counts
3. The CEP↔trade match is in section 7 of the trade notebook `Baci_systemic_risk.ipynb`
   and uses `cep_antibiotics_clean.csv`.

## The 4 antibiotic HS-6 classes
| HS-6 | class | ATC |
|------|-------|-----|
| 294110 | Penicillins | J01C* |
| 294130 | Tetracyclines | J01AA* |
| 294150 | Erythromycin / macrolides | J01FA* |
| 294190 | Other antibiotics (cephalosporins, aminoglycosides, glycopeptides, colistin, lincosamides) | J01D*, J01G*, J01FF, J01XA, J01XB |
