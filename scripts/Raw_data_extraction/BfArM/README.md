# BfArM: API origin from the German drug register

`scrape_bfarm_api_origin.py` scrapes **BfArM AMGuiFree**
(https://portal.bfarm.de/amguifree/), the public view of the German register of
authorised medicinal products, to get the **true origin of the API**.

For each critical ATC code it searches the portal and opens the detail page of
**every** product authorised in Germany. From each page it records the
companies listed at each stage of the value chain, with their country:

| Role | Meaning |
|------|---------|
| `Wirkstoffherstellung` | **active-substance (API) producer** (the key stage) |
| `Hersteller/Endfreigabe` | finished-product manufacturer / batch release |
| `Zulassungsinhaber` | marketing-authorisation holder |
| `Vertreiber`, `Örtlicher Vertreter`, `Mitvertrieb` | distributors, kept as separate roles (not manufacturers) |

The scraper uses the detail view because the portal's CSV export only contains
the finished-product manufacturer and **not** the API producer.

## Usage

```bash
python scrape_bfarm_api_origin.py              # default = --j01
python scrape_bfarm_api_origin.py --j01        # the 35 antibiotic (J01*) critical ATCs
python scrape_bfarm_api_origin.py --rest       # all other critical ATCs
python scrape_bfarm_api_origin.py --all        # all 299 critical ATCs
python scrape_bfarm_api_origin.py J01CF05      # one or more explicit ATC codes
```

**Input:** `../Data/critical_with_inn.csv` (column `ATC_code`). See the
[top-level README](../README.md). This file is not needed when you pass
explicit ATC codes.

## Output (`BfArM/Data/API_origin/`)

```
raw/<ATC>.json                     parsed rows for one ATC (resume checkpoint)
bfarm_api_origin_<TAG>_LONG.csv    all ATCs combined; TAG = critical_J01 / critical_rest / critical_all / <ATC>
```

Each CSV row is one product × company × role:
`search_atc, atc_code, atc_text, eingangsnummer, arzneimittel, role, name, pu_nummer, plz_ort, land`

The run can be resumed: an ATC that already has a `raw/<ATC>.json` is loaded
from cache and not scraped again. To re-scrape an ATC, delete its JSON file.

## Notes

- **Snapshot used in the paper:** all 299 critical ATCs, scraped on 25–26 June 2026.
- **Coverage is the German market only:** products authorised in Germany.
- The API producer is the one declared in the German authorisation dossier.
  The key starting materials may come from further upstream.
- Country names (`land`) are in **German** (`Indien`, `Spanien`, …) and are
  mapped to ISO codes in the analysis.
- Validation: `J01CR02` returned the expected 114 products, and `J01CF05`
  (flucloxacillin) returned 22 products.
