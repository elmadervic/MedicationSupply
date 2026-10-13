# EudraGMDP: EU GMP certificates

`scrape_eudragmdp_2026.py` downloads **all** GMP-compliance certificates from
**EudraGMDP** (EMA, https://eudragmdp.ema.europa.eu), using the GMP Compliance
search for compliant certificates only. It does **not** search by substance.
The script collects every certificate, and the critical substances are matched
afterwards in the analysis, using ATC codes.

## The two scripts

- **`scrape_eudragmdp_2026.py`** is the script to run. It produced the June 2026 snapshot.
- **`scrape_eudragmdp.py`** must sit in the same folder because the 2026 script
  imports its search-form and pagination code from it (`_submit_form`,
  `_fetch_range`, `CERT_URL`, `FILTERS`). If you run it on its own, it repeats the
  earlier May 2025 run (substance names only, plus name matching against
  `Data/critical_classified.csv`). That run is **not** used for the paper.

## Usage

Run the script from this folder. Output paths are relative to the folder you run it from.

```bash
cd EudraGMDP
python scrape_eudragmdp_2026.py            # both filters
python scrape_eudragmdp_2026.py eea        # one filter
python scrape_eudragmdp_2026.py third
```

| Filter | Search settings | Meaning |
|--------|-----------------|---------|
| `eea`   | human GMP cert + Art. 111(1), EEA countries | certificates for sites in the EEA |
| `third` | Art. 111(4), **all** countries | certificates including third-country inspections |

## Output (`Data/June_2026/`)

For each filter, the run works in two phases:

1. `eudragmdp_keys_<filter>.json` is a list with site identity:
   `key, cert_num, site_name, address, city, postcode, country`
2. `eudragmdp_certs_<filter>.json` is a dict keyed by the same `key`:
   `cert_type, issuing_authority, last_inspection_date, withdrawn, withdrawn_date, substances, scope`

The two files are joined with `keys[i]["key"]` ↔ `certs[str(key)]`. Both
phases checkpoint to disk, so an interrupted run resumes where it stopped.

## Things to know when using the output

- **`eea` / `third` are search-filter names, not geography.** `third` searches
  all countries, so EEA sites appear in both files. Deduplicate by certificate
  `key` when you merge them. Take geography from the site's `country` field.
- **`scope` and `substances` are filled only for `cert_type == "API"`**
  certificates. `scope` comes from section *"3. Manufacturing operations –
  active substances"* (activity codes `3.x.x`). Certificates for finished
  products, imports and IMPs are kept, but their scope is not parsed. The code
  labels are defined in EMA's *Interpretation of the Union format for GMP
  certificate*.
- **EudraGMDP gives the EU-certified site, not necessarily the true API origin.**
  An EU certificate holder can source the API from outside the EU (see the
  BfArM data for the true origin).
- **Snapshot used in the paper:** June 2026 (full re-fetch on 2026-06-23).
