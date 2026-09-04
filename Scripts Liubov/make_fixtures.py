"""Synthetic raw files that mirror the real source schemas exactly.

Column names, header offsets, holder string format and CEP types are
copied from a live schema_report.txt, so a green run here means the
parsers match production, not a simplified stand-in.

Run:  python -m critmed.make_fixtures
Delete data/raw/* before a real run.
"""
import random

import pandas as pd

from .config import RAW


def main() -> int:
    from .config import RAW

    # --- ULCM: real layout (metadata rows, content header, group rows, junk cols)
    rows = []
    rows.append(["EMA/4040/2026", None, None, None])
    rows.append(["2026-01-19 00:00:00", None, None, None])
    rows.append(["Union list of critical medicines - version 2.1 (revision 1)", None, None, None])
    rows.append([None, None, None, None])
    rows.append(["ATC code",
                 "ATC description\nThe Anatomical Therapeutic Chemical code: a unique code assigned to a medicine...",
                 "Route of administration", "Date of inclusion"])
    body = [
      ("A", "A - Alimentary tract and metabolism", "", ""),
      ("A10A", "A10A - Insulins and analogues", "", ""),
      ("A10AE04", "INSULIN GLARGINE", "parenteral use", "2023-12-01 00:00:00"),
      ("B", "B - Blood and blood forming organs", "", ""),
      ("B03BA03", "HYDROXOCOBALAMIN", "oral use", "2023-12-01 00:00:00"),
      ("V03AB33", "HYDROXOCOBALAMIN", "intravenous use", "2023-12-01 00:00:00"),
      ("B05XA05", "MAGNESIUM SULFATE", "intravenous use", "2023-12-01 00:00:00"),
      ("B05XA01", "POTASSIUM CHLORIDE", "intravenous use", "2023-12-01 00:00:00"),
      ("J", "J - Antiinfectives for systemic use", "", ""),
      ("J01CA04", "AMOXICILLIN", "oral use", "2023-12-01 00:00:00"),
      ("J01CR02", "AMOXICILLIN, BETA-LACTAMASE INHIBITOR", "oral use", "2023-12-01 00:00:00"),
      ("J01MA02", "CIPROFLOXACIN", "oral use", "2023-12-01 00:00:00"),
      ("R05CB01", "ACETYLCYSTEINE", "oral use", "2024-12-16 00:00:00"),
      ("V03AB23", "ACETYLCYSTEINE", "intravenous use", "2024-12-16 00:00:00"),
      ("C07AB02", "METOPROLOL", "oral use", "2023-12-01 00:00:00"),
      ("N02BE01", "PARACETAMOL", "oral use", "2023-12-01 00:00:00"),
      ("L01FD01", "TRASTUZUMAB", "parenteral use", "2023-12-01 00:00:00"),
    ]
    rows.extend([list(r) for r in body])
    df = pd.DataFrame(rows)
    for k in range(4, 60):          # trailing empty columns, as in the real sheet
        df[k] = None
    with pd.ExcelWriter(RAW/"ulcm.xlsx") as w:
        df.to_excel(w, index=False, header=False, sheet_name="Final version")

    # --- EDQM CEP: real header, real holder format, TSE rows included
    hdr = ["Monograph Number","Substance","Type CEP","Certificate (CEP) Holder",
           "Holder SPOR ORG-ID / SPOR LOC-ID","Certificate (CEP) Number",
           "Issue Date CEP","Status CEP","Renewal due","End date CEP",
           "Closure Date of last Procedure"]
    holders = [("Dr Reddys Laboratories Ltd Hyderabad","IN"),
               ("Zhejiang Huahai Pharmaceutical Co Ltd Xunqiao","CN"),
               ("Teva Pharmaceutical Works Private Ltd Debrecen","HU"),
               ("Sandoz GmbH Kundl","AT"),("Centrient Pharmaceuticals BV Delft","NL"),
               ("Aurobindo Pharma Ltd Hyderabad","IN"),("Fresenius Kabi AG Bad Homburg","DE"),
               ("Indiana Pharma Inc Indianapolis","US")]
    subs = ["Amoxicillin trihydrate","Amoxicillin sodium","Magnesium sulphate heptahydrate",
            "Potassium chloride","Metoprolol tartrate","Metoprolol succinate","Paracetamol",
            "Ciprofloxacin hydrochloride","Hydroxocobalamin acetate","Acetylcysteine",
            "1,2-dihydrotriamcinolone"]
    random.seed(1); out=[]
    for i,s in enumerate(subs):
        for h,c in random.sample(holders, random.randint(1,5)):
            out.append(dict(zip(hdr,["0123",s,"Chemical purity",f"{h} {c}","",
                f"R1-CEP 2020-{i:03d} - Rev 00","21/11/2019",
                random.choice(["Valid","Valid","Valid","Withdrawn by Holder","Expired"]),
                "","",""])))
    # TSE certificates for an excipient-adjacent substance - must be excluded
    for h,c in holders:
        out.append(dict(zip(hdr,["0","Magnesium stearate","TSE",f"{h} {c}","",
            "R0-CEP 2001-999 - Rev 00","09/04/2002","Valid","","",""])))
    for h,c in holders[:6]:
        out.append(dict(zip(hdr,["0","Paracetamol","TSE",f"{h} {c}","",
            "R0-CEP 2001-888 - Rev 00","09/04/2002","Valid","","",""])))
    pd.DataFrame(out).to_csv(RAW/"edqm_cep.txt", sep="\t", index=False, encoding="utf-8-sig")

    # --- EPAR manufacturers from the existing pipeline
    pd.DataFrame([
     {"active_substance":"trastuzumab","manufacturer_name":"Roche Diagnostics GmbH","country":"Germany","manufacturer_step":"biological_active_substance"},
     {"active_substance":"trastuzumab","manufacturer_name":"Genentech Inc","country":"United States","manufacturer_step":"biological_active_substance"},
     {"active_substance":"trastuzumab","manufacturer_name":"Roche Pharma AG","country":"Germany","manufacturer_step":"batch_release"},
     {"active_substance":"insulin glargine","manufacturer_name":"Sanofi-Aventis Deutschland GmbH","country":"Germany","manufacturer_step":"biological_active_substance"},
     {"active_substance":"insulin glargine","manufacturer_name":"Sanofi Winthrop Industrie","country":"France","manufacturer_step":"batch_release"},
    ]).to_csv(RAW/"manufacturers.csv", sep=";", index=False)
    print(f"real-schema fixtures written to {RAW}")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
