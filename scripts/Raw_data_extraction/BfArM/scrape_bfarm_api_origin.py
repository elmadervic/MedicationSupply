"""
BfArM AMGuiFree — API true-origin scraper (3-stage value chain).

For each ATC code, search by ATC-Code and walk EVERY authorised product's
DETAIL view, extracting the pharmaceutical-company roles WITH country:

  - Zulassungsinhaber       marketing-authorisation holder
  - Hersteller/Endfreigabe  finished-product manufacturer / batch release
  - Wirkstoffherstellung    ACTIVE-SUBSTANCE (API) producer   <-- the key stage
  - Vertreiber, Örtlicher Vertreter, Mitvertrieb   distributors, NOT makers
    (kept as their own roles: they sit right after Wirkstoffherstellung on the
     page, and mislabelling them as API producers overstates EU API origin)

Unlike the CSV export, the detail view carries the API producer + Land (country),
which reveals true API origin (EU-facade products whose API is e.g. from India).

Usage:
  python3 scrape_bfarm_api_origin.py                 # all critical J01 ATCs
  python3 scrape_bfarm_api_origin.py J01CF05         # one ATC (validation)
  python3 scrape_bfarm_api_origin.py J01CF05 J01CR02 # a few

Output (Data/API_origin/):
  raw/<ATC>.json                    per-ATC parsed records (resume-safe)
  bfarm_api_origin_<TAG>_LONG.csv   one row per (atc, product, role, company, country)
"""
import sys, csv, json, re, time
from pathlib import Path
from playwright.sync_api import sync_playwright

ROOT = Path(__file__).parent
OUT  = ROOT / "Data" / "API_origin"
RAW  = OUT / "raw"
RAW.mkdir(parents=True, exist_ok=True)
CRIT = ROOT.parent / "Data" / "critical_with_inn.csv"   # Data_extraction/Data/...

PORTAL = "https://portal.bfarm.de/amguifree/termsofuse.xhtml?accept=true"
BASE   = "https://portal.bfarm.de"
SF     = "searchForm:searchInputsComponent:"
def esc(i): return "#" + i.replace(":", "\\:")

ROLE_HEADERS = {
    "Zulassungsinhaber":        "Zulassungsinhaber",
    "Zulassungsinhaber*":       "Zulassungsinhaber",
    "Hersteller Endfreigabe":   "Hersteller/Endfreigabe",
    "Hersteller/Endfreigabe":   "Hersteller/Endfreigabe",
    "Wirkstoffherstellung":     "Wirkstoffherstellung",
    # distributors - NOT manufacturers. They follow Wirkstoffherstellung on the
    # page, so before this was fixed they inherited the API label (see below).
    "Vertreiber":               "Vertreiber",
    "Örtlicher Vertreter":      "Örtlicher Vertreter",
    "Mitvertrieb":              "Mitvertrieb",
}
COLS = ["search_atc", "atc_code", "atc_text", "eingangsnummer", "arzneimittel",
        "role", "name", "pu_nummer", "plz_ort", "land"]


def wait(page, t=30000):
    try: page.wait_for_load_state("networkidle", timeout=t)
    except Exception: pass
    time.sleep(0.8)


def _kv(line):
    if "\t" in line:
        a, b = line.split("\t", 1)
        return a.strip(), b.strip()
    return line.strip(), None


def parse_detail(text):
    """Return (product_dict, [entry,...]) from one product detail page."""
    lines = text.split("\n")
    prod = {"eingangsnummer": "", "arzneimittel": "", "atc_code": "", "atc_text": ""}
    for l in lines:
        k, v = _kv(l)
        if v is None:
            continue
        if k == "Eingangsnummer" and not prod["eingangsnummer"]:
            prod["eingangsnummer"] = v
        elif k == "Arzneimittelbezeichnung" and not prod["arzneimittel"]:
            prod["arzneimittel"] = v
        elif k in ("Indikation/ATC-Code", "Indikation/ATC") and not prod["atc_code"]:
            m = re.search(r"[A-Z]\d{2}[A-Z]{2}\d{2}", v)
            prod["atc_code"] = m.group(0) if m else v
        elif k == "Indikation/ATC-Bedeutung" and not prod["atc_text"]:
            prod["atc_text"] = v

    full = "\n".join(lines)
    s = full.find("Pharmazeutische Unternehmer")
    if s < 0:
        return prod, []
    e = full.find("Zusammensetzung", s + 1)
    block = full[s:e] if e >= 0 else full[s:]

    entries, role, cur = [], None, None
    for l in block.split("\n"):
        k, v = _kv(l)
        if v is None:
            head = k.rstrip("*").strip()
            if not head or head.startswith("*") or head == "Pharmazeutische Unternehmer":
                continue
            # EVERY standalone line starts a new section. Only switching on
            # recognised headers left `role` untouched at an unknown one, so the
            # companies under it inherited the previous role - which put
            # Vertreiber / Örtlicher Vertreter into Wirkstoffherstellung (or into
            # Hersteller/Endfreigabe on products with no API section). An
            # unfamiliar header now shows up as itself instead of contaminating
            # the role above it.
            role, cur = ROLE_HEADERS.get(head, head), None
            continue
        if k == "PU-Nummer":
            cur = {"role": role, "pu_nummer": v, "name": "", "plz_ort": "", "land": ""}
            entries.append(cur)          # appended now, then filled in below
        elif cur is not None:
            if k == "Name":      cur["name"] = v
            elif k == "PLZ Ort": cur["plz_ort"] = v
            elif k == "Land":    cur["land"] = v
    return prod, entries


def scrape_atc(page, atc):
    page.goto(PORTAL, timeout=60000); wait(page)
    page.select_option(esc(SF + "searchRows:0:searchField"), "MPD_ATC.ATC_TERM_CODE")
    page.fill(esc(SF + "searchRows:0:searchTerm"), atc)
    page.click(esc(SF + "suchestarten")); wait(page)
    if page.query_selector("#sortAlertForm\\:showUnsortedResultButton"):
        page.click("#sortAlertForm\\:showUnsortedResultButton"); wait(page)

    body = page.inner_text("body")
    m = (re.search(r"Gefundene Dokumente:\s*([\d\.]+)", body)
         or re.search(r"([\d\.]+)\s*Treffer", body))
    treffer = int(m.group(1).replace(".", "")) if m else 0

    # open first product detail (directdisplay), then browse all with 'weiter'
    href = page.evaluate("""() => {
        const a=[...document.querySelectorAll('a')]
                 .find(a=>(a.getAttribute('href')||'').includes('jpadocdisplay'));
        return a ? a.getAttribute('href') : null;
    }""")
    rows, seen = [], set()
    if not href:
        return treffer, rows
    page.goto(BASE + href, timeout=60000); wait(page)

    limit = treffer + 5 if treffer else 3000
    for _ in range(limit):
        prod, entries = parse_detail(page.inner_text("body"))
        key = prod["eingangsnummer"] or page.url
        if key in seen:               # wrapped around / no movement -> done
            break
        seen.add(key)
        for en in entries:
            rows.append({"search_atc": atc, **prod, **en})
        nxt = page.query_selector("a:has-text('weiter')")
        if not nxt:
            break
        try:
            nxt.click(); wait(page)
        except Exception:
            break
    return treffer, rows


def load_atcs():
    """Groups over the critical list (Data/critical_with_inn.csv):
       --j01  -> the 35 antibiotic (J01*) ATCs        tag=critical_J01
       --rest -> all critical EXCEPT J01              tag=critical_rest
       --all  -> every critical ATC                   tag=critical_all
       <ATC>… -> explicit ATC codes                   tag=ATC or custom
       (no args, default) -> --j01
    """
    args = [a.upper() for a in sys.argv[1:]]
    crit = [r["ATC_code"] for r in csv.DictReader(open(CRIT))]
    if args == ["--REST"]:
        return [a for a in crit if not a.startswith("J01")], "critical_rest"
    if args == ["--ALL"]:
        return crit, "critical_all"
    if args == ["--J01"] or not args:
        return [a for a in crit if a.startswith("J01")], "critical_J01"
    tag = args[0] if len(args) == 1 else "custom"
    return args, tag


def main():
    atcs, tag = load_atcs()
    print(f"ATCs to scrape: {len(atcs)}  (tag={tag})")
    all_rows = []
    with sync_playwright() as p:
        browser = p.chromium.launch(headless=True)
        page = browser.new_context().new_page()
        for i, atc in enumerate(atcs, 1):
            raw = RAW / f"{atc}.json"
            if raw.exists():
                rows = json.loads(raw.read_text())
                print(f"[{i}/{len(atcs)}] {atc}: cached {len(rows)} rows")
            else:
                try:
                    treffer, rows = scrape_atc(page, atc)
                except Exception as ex:
                    print(f"[{i}/{len(atcs)}] {atc}: ERROR {ex}")
                    continue
                raw.write_text(json.dumps(rows, ensure_ascii=False, indent=1))
                api = len({r["name"] for r in rows if r["role"] == "Wirkstoffherstellung"})
                print(f"[{i}/{len(atcs)}] {atc}: {treffer} products -> {len(rows)} rows, "
                      f"{api} distinct API producers")
            all_rows += rows
        browser.close()

    out = OUT / f"bfarm_api_origin_{tag}_LONG.csv"
    with open(out, "w", newline="", encoding="utf-8") as f:
        w = csv.DictWriter(f, fieldnames=COLS); w.writeheader(); w.writerows(all_rows)
    print(f"\nwrote {len(all_rows)} rows -> {out}")


if __name__ == "__main__":
    main()
