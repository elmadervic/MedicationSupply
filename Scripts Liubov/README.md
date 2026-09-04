## Setup

```bash
python -m venv venv && source venv/bin/activate
pip install pandas openpyxl requests lxml pycountry pdfplumber
```

## Run order

Run these from inside `Scripts Liubov/` (with `venv` activated), as plain scripts —
not with `python -m`, since this folder isn't an importable package.

```bash
python fetch.py
```

```bash
python inspect_sources.py > schema_report.txt
```

Prints the real column names, row counts and sample values of every file in
`data/raw/`. 


```bash
python build_supply.py
```

Parses every source, normalises substance names, resolves countries, writes
`data/interim/supply_long.csv` — one row per substance × supplier × country ×
role. 


```bash
python analyse.py
```

Writes to `data/out/`:

| file | contents |
|---|---|
| `summary.txt` | headline numbers per role |
| `substance_supply.csv` | one row per substance per role |
| `country_totals.csv` | substances and suppliers per country |
| `distribution.csv` | how many substances have exactly *k* supplier countries |
| `sensitivity.csv` | the same numbers with valid-only vs all CEP statuses |

```bash
python qc.py
```

Runs the sanity checks and prints PASS / WARN / FAIL.