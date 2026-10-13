"""
EudraGMDP Active Substance Manufacturer scraper.

Phase 1: Playwright submits the search form, extracts session cookies.
         requests then paginates through all results collecting certificate
         keys + site metadata.
Phase 2: requests fetches each certificate page, extracts substance names.
Phase 3: matches against our 299 INN list, writes final CSV.

Intermediate results saved to JSON so the run can be resumed if interrupted.

Filter keys:
  eea       — Human/Art.111(5) + Art.111(1) + EEA  (both checkboxes, EEA only)
  third     — Art.111(4), all countries            (third-country API inspections)
"""

import csv, json, re, time, threading
import requests
from bs4 import BeautifulSoup
from playwright.sync_api import sync_playwright
from concurrent.futures import ThreadPoolExecutor, as_completed
from pathlib import Path

BASE       = "https://eudragmdp.ema.europa.eu"
SEARCH_URL = BASE + "/inspections/gmpc/searchGMPCompliance.do"
CERT_URL   = BASE + "/inspections/gmpc/prepReviewSubmittedGMPC.do?key={key}"
PAGE_TMPL  = BASE + "/inspections/gmpc/searchGMPCompliance.do?ctrl=searchGMPCResultControlList&action=Page&param={pg}"

# Which checkboxes to check and which country per filter
FILTERS = {
    "eea":   {"checkboxes": ["gmpcH", "gmpc_art_directive_111_1"], "country": "eu.countries"},
    "third": {"checkboxes": ["gmpc_art_directive_111_4"],          "country": "all.countries"},
}

SALT_WORDS = {
    "hydrochloride","dihydrochloride","trihydrochloride","sulfate","sulphate",
    "fumarate","maleate","phosphate","diphosphate","acetate","tartrate",
    "citrate","mesylate","tosylate","ethanolate","monohydrate","dihydrate",
    "trihydrate","sterile","micronised","milled","anhydrous",
    "besylate","bromide","chloride","iodide","nitrate","succinate",
}


def normalize(name: str) -> str:
    n = name.lower().strip()
    n = re.sub(r"\s*\([^)]*\)", "", n)
    tokens = [t for t in n.split() if t not in SALT_WORDS]
    return " ".join(tokens).strip()


def load_inns(path="Data/critical_classified.csv"):
    inns = {}
    with open(path, newline="", encoding="utf-8") as f:
        for row in csv.DictReader(f):
            key = normalize(row["INN"])
            if key:
                inns[key] = row
    return inns


# ── Phase 1: collect certificate keys ─────────────────────────────────────────

def _submit_form(filter_key: str):
    """Submit the search form via Playwright; return (session_cookies, total_count)."""
    cfg = FILTERS[filter_key]
    checkboxes = cfg["checkboxes"]
    country    = cfg["country"]

    with sync_playwright() as p:
        browser = p.chromium.launch(headless=True)
        page = browser.new_page()
        page.goto(SEARCH_URL, timeout=30000)
        try:
            page.wait_for_load_state("networkidle", timeout=15000)
        except Exception:
            pass

        # Uncheck all gmpc*/gmpnc* boxes, then check each requested one
        page.evaluate("""(targets) => {
            document.querySelectorAll('input[type=checkbox]').forEach(cb => {
                if (cb.name.startsWith('gmpc') || cb.name.startsWith('gmpnc')) {
                    cb.checked = false;
                }
            });
            targets.forEach(t => {
                const el = document.querySelector('input[name="' + t + '"]');
                if (el) el.checked = true;
            });
        }""", checkboxes)

        page.select_option("select[name=country]", value=country)

        try:
            page.click("#btnSearchGMPCLabel", timeout=120000)
        except Exception:
            pass
        try:
            page.wait_for_load_state("networkidle", timeout=60000)
        except Exception:
            pass
        time.sleep(2)

        body = page.evaluate("() => document.body.innerText")
        m = re.search(r"\d+ to \d+ of (\d+)", body)
        total = int(m.group(1)) if m else 0
        print(f"  Total results: {total}  (~{(total+9)//10} pages)")

        cookies = {c["name"]: c["value"] for c in page.context.cookies()}
        browser.close()

    return cookies, total


def _parse_page_html(html: str) -> list[dict]:
    """Extract certificate records from one results page."""
    soup = BeautifulSoup(html, "html.parser")
    records = []
    for table in soup.find_all("table", class_="ibody"):
        first_row = table.find("tr")
        if not first_row:
            continue
        headers = [td.get_text(strip=True) for td in first_row.find_all(["td", "th"])]
        if "Certificate Number" not in headers:
            continue
        for row in table.find_all("tr")[1:]:
            cells = [td.get_text(strip=True) for td in row.find_all("td")]
            if len(cells) < 7:
                continue
            cert_num  = cells[0]
            ref_num   = cells[1]
            doc_type  = cells[2]
            site_name = cells[6]  if len(cells) > 6  else ""
            address   = cells[7]  if len(cells) > 7  else ""
            city      = cells[8]  if len(cells) > 8  else ""
            postcode  = cells[9]  if len(cells) > 9  else ""
            country   = cells[10] if len(cells) > 10 else ""
            if doc_type == "GMPC" and re.match(r"^\d+$", ref_num):
                records.append({
                    "key":       int(ref_num),
                    "cert_num":  cert_num,
                    "site_name": site_name,
                    "address":   address,
                    "city":      city,
                    "postcode":  postcode,
                    "country":   country,
                })
    return records


def _fetch_range(filter_key: str, start_pg: int, end_pg: int,
                 worker_id: int, lock, shared: dict, cache_file: Path):
    """One worker: submits form, fetches pages [start_pg, end_pg)."""
    try:
        cookies, total = _submit_form(filter_key)
    except Exception as e:
        print(f"  [W{worker_id}] form submission failed: {e}")
        return

    total_pages = (total + 9) // 10
    end_pg = min(end_pg, total_pages)

    session = requests.Session()
    session.headers["User-Agent"] = "Mozilla/5.0"
    session.cookies.update(cookies)

    for pg in range(start_pg, end_pg):
        for attempt in range(3):
            try:
                r = session.get(PAGE_TMPL.format(pg=pg), timeout=60)
                page_recs = _parse_page_html(r.text)
                with lock:
                    for rec in page_recs:
                        if rec["key"] not in shared["seen"]:
                            shared["seen"].add(rec["key"])
                            shared["records"].append(rec)
                    shared["done"] += 1
                    done = shared["done"]
                    total_est = shared["total_pages"]
                    if done % 100 == 0:
                        cache_file.write_text(json.dumps(shared["records"], indent=2))
                        print(f"  [W{worker_id}] pg {pg+1} — checkpoint saved "
                              f"({done}/{total_est} done, {len(shared['records'])} keys)")
                    elif done % 20 == 0:
                        print(f"  [W{worker_id}] pg {pg+1} — "
                              f"{done}/{total_est} done, {len(shared['records'])} keys")
                time.sleep(0.1)
                break
            except Exception as e:
                wait = 5 * (attempt + 1)
                print(f"  [W{worker_id}] pg {pg+1} attempt {attempt+1}/3: {e} — retry in {wait}s")
                time.sleep(wait)
        else:
            print(f"  [W{worker_id}] pg {pg+1}: skipped after 3 failed attempts")


def collect_keys(filter_key="eea", n_workers=5) -> list[dict]:
    cache_file = Path(f"Data/eudragmdp_keys_{filter_key}.json")

    if cache_file.exists():
        data = json.loads(cache_file.read_text())
        if data:
            print(f"Loaded {len(data)} keys from cache ({cache_file})")
            return data
        cache_file.unlink()

    print(f"Collecting certificate keys — filter: {filter_key}  ({n_workers} parallel sessions)")

    # Get total count with one quick Playwright call
    cookies, total = _submit_form(filter_key)
    total_pages = (total + 9) // 10
    print(f"  {total_pages} pages to fetch across {n_workers} workers")

    chunk = (total_pages + n_workers - 1) // n_workers
    ranges = [(i * chunk, min((i + 1) * chunk, total_pages)) for i in range(n_workers)]

    lock = threading.Lock()
    shared = {"records": [], "seen": set(), "done": 0, "total_pages": total_pages}

    with ThreadPoolExecutor(max_workers=n_workers) as pool:
        futures = [
            pool.submit(_fetch_range, filter_key, s, e, i, lock, shared, cache_file)
            for i, (s, e) in enumerate(ranges)
        ]
        for f in as_completed(futures):
            f.result()  # surface any exceptions

    cache_file.write_text(json.dumps(shared["records"], indent=2))
    print(f"Saved {len(shared['records'])} unique keys to {cache_file}")
    return shared["records"]


# ── Phase 2: fetch certificate pages and extract substances ───────────────────

def scrape_certificate(key: int, session: requests.Session) -> list[str]:
    url = CERT_URL.format(key=key)
    try:
        r = session.get(url, timeout=15)
        if r.status_code != 200:
            return []
        soup = BeautifulSoup(r.text, "html.parser")
        text = soup.get_text(separator="\n")
        lines = [l.strip() for l in text.split("\n")]

        substances = []
        in_section = False
        lang_re = re.compile(r"\(([a-z]{2})\)$", re.IGNORECASE)

        for line in lines:
            if not line:
                continue
            if "Names of substances subject to" in line:
                in_section = True
                continue
            if in_section:
                m = lang_re.search(line)
                if m:
                    name = lang_re.sub("", line).strip()
                    name = re.sub(r"^\[\d+\]", "", name).strip()
                    if name:
                        substances.append(name)
                else:
                    in_section = False

        return substances
    except Exception:
        return []


def enrich_with_substances(records: list[dict], cache_file: str,
                            workers: int = 5) -> list[dict]:

    cache_path = Path(cache_file)
    done = {}
    if cache_path.exists():
        done = {int(k): v for k, v in json.loads(cache_path.read_text()).items()}
        print(f"Resuming: {len(done)} certificates already fetched")

    lock = threading.Lock()

    to_fetch = [r for r in records if r["key"] not in done]
    total = len(to_fetch)
    print(f"Fetching {total} certificate pages ({workers} concurrent workers)...")

    def fetch_one(rec):
        session = requests.Session()
        session.headers["User-Agent"] = "Mozilla/5.0"
        substances = scrape_certificate(rec["key"], session)
        time.sleep(0.1)
        return rec["key"], substances

    completed = 0
    with ThreadPoolExecutor(max_workers=workers) as pool:
        futures = {pool.submit(fetch_one, rec): rec for rec in to_fetch}
        for future in as_completed(futures):
            key, substances = future.result()
            with lock:
                done[key] = substances
                completed += 1
                if completed % 200 == 0:
                    cache_path.write_text(json.dumps(done, indent=2))
                    print(f"  {completed}/{total} fetched, checkpoint saved")

    cache_path.write_text(json.dumps(done, indent=2))
    print(f"Done. Saved to {cache_file}")

    for rec in records:
        rec["substances"] = done.get(rec["key"], [])
    return records


# ── Phase 3: match and output ─────────────────────────────────────────────────

def match_and_save(records: list[dict], filter_key: str):
    inns = load_inns()
    output_file = f"Data/eudragmdp_matched_{filter_key}.csv"

    matched = []
    for rec in records:
        for substance in rec["substances"]:
            norm = normalize(substance)
            if not norm:
                continue
            hit = inns.get(norm)
            if not hit:
                for inn_key, inn_row in inns.items():
                    if inn_key and (inn_key in norm or norm in inn_key):
                        hit = inn_row
                        break
            if hit:
                matched.append({
                    "ATC_code":          hit["ATC_code"],
                    "INN":               hit["INN"],
                    "Therapeutic_class": hit["Therapeutic_class"],
                    "eudragmdp_key":     rec["key"],
                    "cert_number":       rec["cert_num"],
                    "site_name":         rec["site_name"],
                    "address":           rec["address"],
                    "city":              rec["city"],
                    "country":           rec["country"],
                    "substance_on_cert": substance,
                })

    with open(output_file, "w", newline="", encoding="utf-8") as f:
        writer = csv.DictWriter(f, fieldnames=matched[0].keys() if matched else [])
        writer.writeheader()
        writer.writerows(matched)

    print(f"\n{'='*60}")
    print(f"Matched {len(matched)} substance-site pairs across "
          f"{len({m['ATC_code'] for m in matched})} distinct ATC codes")
    print(f"Written to {output_file}")
    return matched


# ── main ───────────────────────────────────────────────────────────────────────
if __name__ == "__main__":
    import sys
    filter_key = sys.argv[1] if len(sys.argv) > 1 else "eea"
    assert filter_key in FILTERS, f"Unknown filter: {filter_key}. Choose from: {list(FILTERS)}"

    print(f"\n{'='*60}")
    print(f"EudraGMDP scraper — filter: {filter_key}")
    print(f"{'='*60}\n")

    records = collect_keys(filter_key)
    records = enrich_with_substances(records, cache_file=f"Data/eudragmdp_certs_{filter_key}.json")
    match_and_save(records, filter_key)
