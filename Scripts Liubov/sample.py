sup = pd.read_csv("data/interim/supply_long.csv")
sup[sup.norm_trace.str.contains("combination|dropped non-substance", na=False)] \
   [["substance_raw","norm_key","norm_trace"]].drop_duplicates("norm_key").head(40) \
   .to_csv("check_combos.csv", index=False)

pd.read_csv("data/out/substance_supply.csv") \
   .query("role == 'api_cep' and single_country == True") \
   [["ulcm_substance_raw","atc_codes","top_country","n_suppliers","countries"]] \
   .to_csv("check_single.csv", index=False)