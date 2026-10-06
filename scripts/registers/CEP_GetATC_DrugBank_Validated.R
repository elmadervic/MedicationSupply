## =================================================================
## FINAL PIPELINE
## EXPORT_WEB_CEP.txt (raw EDQM export)
##   -> keep Status CEP == "Valid" only
##   -> match substances to DrugBank ATC codes
##   -> filter out likely combination-product codes (level-3/4 text)
##   -> cross-check surviving codes against NLM RxNav/RxClass
##   -> keep only rows whose ATC code is on the critical list
##   -> save as EXPORT_WEB_CEP_with_ATC_drugbank.csv
##
## Required input files in the working directory:
##   - EXPORT_WEB_CEP.txt        (raw EDQM CEP export, tab-delimited)
##   - DrugBank_FullDatabase.xml (DrugBank full XML database)
##   - critical.csv              (one column "ATC level 5", list of
##                                critical-medicine ATC codes)
##
## DrugBank download: https://go.drugbank.com/releases/latest#full
## (free academic license, requires registration/approval) ->
## drugbank_all_full_database.xml.zip -> unzip to get "full
## database.xml" -> rename/place as DrugBank_FullDatabase.xml here.
##
## RxNav step makes ~2 API calls per unique matched substance with a
## polite delay -- expect ~10-20 minutes for a few thousand
## substances. Progress is cached to rxnav_atc_cache.csv so an
## interrupted run resumes instead of re-querying from scratch.
## =================================================================

library(xml2)
library(dplyr)
library(stringr)
library(purrr)
library(tibble)
library(readr)
library(httr)
library(jsonlite)

## ===================================================================
## STEP 1 -- READ RAW CEP EXPORT, KEEP VALID STATUS ONLY
## ===================================================================
cep_path <- "Data/EXPORT_WEB_CEP.txt"

cep_raw <- read_tsv(
  cep_path,
  locale = locale(encoding = "UTF-8"),
  show_col_types = FALSE
)
cat("Total CEP rows (all statuses):", nrow(cep_raw), "\n")

cep <- cep_raw |>
  mutate(Substance = str_trim(Substance)) |>
  filter(`Status CEP` == "Valid")

cat("CEP rows after keeping Status CEP == 'Valid':", nrow(cep), "\n")

unique_substances <- cep |> distinct(Substance) |> pull(Substance)
cat("Unique substances (Valid only):", length(unique_substances), "\n")

## ===================================================================
## STEP 2 -- BUILD CANDIDATE MATCH STRINGS PER SUBSTANCE
## ===================================================================
## CEP substance names often carry manufacturing-process / form
## modifiers after a comma, e.g. "Aceclofenac, Process-II", but some
## genuine chemical names also contain commas, e.g.
## "1,2-dihydrotriamcinolone". So we try multiple candidate strings
## per substance, strictest first, and stop at the first DrugBank hit.
## A wrong split just fails to match -- it can't silently attach the
## wrong ATC code, since matching is exact-string against DrugBank
## names/synonyms.
modifier_pattern <- regex(
  paste0(
    "\\b(process|form|modification|micronis|polymorph|crystal|anhydr|",
    "hydrate|standard|grade|milled|unmilled|type|code|product number|",
    "batch|plant|site|particle|sieved|non-?sterile|sterile|fine|coarse|",
    "granul|powder|salt|impurit)"
  ),
  ignore_case = TRUE
)

build_candidates <- function(name) {
  candidates <- name  # candidate 1: the full name, unmodified
  
  if (str_detect(name, ",")) {
    after_first_comma <- str_split(name, ",", n = 2)[[1]][2]
    
    ## candidate 2: strip from first comma onward, only if what
    ## follows looks like a process/form modifier -- protects genuine
    ## comma-containing chemical names.
    if (str_detect(after_first_comma, modifier_pattern)) {
      candidates <- c(candidates, str_trim(str_split(name, ",", n = 2)[[1]][1]))
    }
    
    ## candidate 3 (looser fallback): strip from first comma onward
    ## regardless -- tried last.
    candidates <- c(candidates, str_trim(str_split(name, ",", n = 2)[[1]][1]))
  }
  
  unique(candidates)
}

strip_parens <- function(name) {
  out <- str_trim(str_remove(name, "\\s*\\([^)]*\\)\\s*$"))
  if (out == "") name else out
}

substance_candidates <- tibble(Substance = unique_substances) |>
  mutate(
    candidates = map(Substance, ~ unique(c(build_candidates(.x), strip_parens(.x))))
  ) |>
  unnest(candidates) |>
  rename(match_candidate = candidates) |>
  mutate(match_norm = str_trim(str_to_lower(match_candidate))) |>
  distinct(Substance, match_norm, .keep_all = TRUE)

cat("Total (substance, candidate) pairs to try against DrugBank:",
    nrow(substance_candidates), "\n")

## ===================================================================
## STEP 3 -- PARSE THE DRUGBANK XML
## ===================================================================
xml_path <- "Data/DrugBank_FullDatabase.xml"
doc <- read_xml(xml_path)
ns <- xml_ns(doc)
print(ns)  # confirm namespace prefix (usually "d1")

## Deliberately root-scoped (/d1:drugbank/d1:drug), NOT the more
## obvious ".//d1:drug". The latter also matches <drug> fragments
## nested inside <pathway>, <products>, <mixtures>, <salts>,
## <drug-interactions>, etc. -- lightweight cross-references to OTHER
## real entries, not real records themselves. ".//d1:drug" pulled in
## ~74,000 nodes instead of the correct ~11,000-17,000 top-level
## entries.
drug_nodes <- xml_find_all(doc, paste0("/d1:", xml_name(xml_root(doc)), "/d1:drug"), ns)
cat("Total top-level drug entries in DrugBank:", length(drug_nodes), "\n")

## Words that show up in ATC level-3/level-4 group names when the
## group is a COMBINATION rather than a plain single agent, e.g.
## "Progestogens and estrogens, fixed combinations",
## "ANDROGENS AND FEMALE SEX HORMONES IN COMBINATION". Deliberately
## does NOT include bare "and" -- that also appears in genuinely
## plain single-agent group names, e.g. "Natural and semisynthetic
## estrogens, plain".
combo_keyword_pattern <- regex(
  "(combinations?|\\bpreparations\\b|in combination|\\bplus\\b|\\bwith\\b)",
  ignore_case = TRUE
)

extract_drug_info <- function(node) {
  db_id <- xml_text(xml_find_first(node, "./d1:drugbank-id[@primary='true']", ns))
  name  <- xml_text(xml_find_first(node, "./d1:name", ns))
  synonyms <- xml_find_all(node, "./d1:synonyms/d1:synonym", ns) |> xml_text()
  
  ## Level-5 ATC code (7 chars) lives as an attribute on <atc-code>.
  ## <level> children are the class hierarchy ABOVE it, ordered
  ## level4 -> level3 -> level2 -> level1. DrugBank does not expose a
  ## level-5 name. We grab level4 AND level3 to flag likely
  ## combination-product codes -- level2/level1 excluded, they're
  ## broad chapter names that themselves often contain "and" for
  ## unrelated reasons.
  atc_nodes <- xml_find_all(node, "./d1:atc-codes/d1:atc-code", ns)
  atc_codes <- xml_attr(atc_nodes, "code")
  level4_names <- map_chr(atc_nodes, function(n) {
    lvl <- xml_find_first(n, "./d1:level[1]", ns)
    if (is.na(lvl)) NA_character_ else xml_text(lvl)
  })
  level3_names <- map_chr(atc_nodes, function(n) {
    lvl <- xml_find_first(n, "./d1:level[2]", ns)
    if (is.na(lvl)) NA_character_ else xml_text(lvl)
  })
  
  if (length(atc_codes) == 0) return(NULL)
  
  tibble(
    drugbank_id = db_id,
    drug_name   = name,
    synonym     = c(name, synonyms),
    atc_code    = list(atc_codes),
    atc_level4_name = list(level4_names),
    atc_level3_name = list(level3_names)
  ) |>
    unnest(c(atc_code, atc_level4_name, atc_level3_name))
}

drugbank_atc <- map_dfr(seq_along(drug_nodes), function(i) {
  if (i %% 2000 == 0) cat("Parsed", i, "/", length(drug_nodes), "\n")
  extract_drug_info(drug_nodes[[i]])
})
cat("Total (name/synonym, ATC code) rows extracted from DrugBank:", nrow(drugbank_atc), "\n")

drugbank_atc <- drugbank_atc |>
  mutate(
    synonym_norm = str_trim(str_to_lower(synonym)),
    is_combo_code = str_detect(
      paste(coalesce(atc_level4_name, ""), coalesce(atc_level3_name, "")),
      combo_keyword_pattern
    )
  ) |>
  distinct(synonym_norm, atc_code, .keep_all = TRUE)

write_csv(drugbank_atc, "drugbank_name_atc_lookup.csv")

## ===================================================================
## STEP 4 -- MATCH SUBSTANCES TO DRUGBANK, FILTER COMBO CODES
## ===================================================================
all_matches_raw <- substance_candidates |>
  inner_join(
    drugbank_atc |> select(synonym_norm, drug_name, atc_code, atc_level4_name, atc_level3_name, is_combo_code),
    by = c("match_norm" = "synonym_norm")
  ) |>
  mutate(
    match_pass = if_else(str_to_lower(match_candidate) == str_to_lower(Substance),
                         "exact_full_name", "cleaned_name")
  )

## For each substance, if it has at least one non-combo match, drop
## the combo-flagged ones and keep only the plain-agent code(s). If
## EVERY match is combo-flagged, keep all rather than losing the
## substance entirely.
## NOTE: heuristic, not exact -- some combination codes sharing a
## level-4 group with a plain code (e.g. G03CA53 alongside G03CA03)
## will still slip through, since DrugBank exposes no level-5 name.
has_noncombo <- all_matches_raw |>
  group_by(Substance) |>
  summarise(any_noncombo = any(!is_combo_code), .groups = "drop")

all_matches <- all_matches_raw |>
  left_join(has_noncombo, by = "Substance") |>
  filter(!(is_combo_code & any_noncombo)) |>
  select(-any_noncombo)

cat("Combo-code filter: rows before =", nrow(all_matches_raw),
    ", after =", nrow(all_matches), "\n")

matched_substances <- unique(all_matches$Substance)
cat("Substances matched to a DrugBank ATC code:",
    length(matched_substances), "/", length(unique_substances),
    sprintf("(%.1f%%)", 100 * length(matched_substances) / length(unique_substances)), "\n")

atc_map_final <- bind_rows(
  all_matches |>
    distinct(Substance, atc_code, .keep_all = TRUE) |>
    transmute(
      base_substance = Substance, atc_code, atc_name = drug_name,
      atc_level4_name, atc_level3_name, is_combo_code,
      matched_via = match_candidate, match_pass, query_status = "drugbank_match"
    ),
  tibble(Substance = unique_substances) |>
    filter(!Substance %in% matched_substances) |>
    transmute(
      base_substance = Substance, atc_code = NA_character_, atc_name = NA_character_,
      atc_level4_name = NA_character_, atc_level3_name = NA_character_, is_combo_code = NA,
      matched_via = NA_character_, match_pass = NA_character_, query_status = "no_match"
    )
)

write_csv(atc_map_final, "cep_substance_atc_lookup_drugbank.csv")
cat("\nquery_status breakdown:\n")
print(atc_map_final |> distinct(base_substance, .keep_all = TRUE) |> count(query_status, sort = TRUE))

## ===================================================================
## STEP 5 -- CROSS-CHECK SURVIVING CODES AGAINST NLM RxNav/RxClass
## ===================================================================
## Spot checks found DrugBank's own atc-codes lists sometimes contain
## codes that are simply wrong for the substance (e.g. Aciclovir ->
## R03DA20 "Xanthines", unrelated). RxNav/RxClass (NLM, derived from
## RxNorm + WHO ATC) is used as an independent check.
##
## LIMITATION: RxClass's byRxcui/relaSource=ATC endpoint only returns
## ATC codes up to LEVEL 4 (5 characters), never the specific level-5
## code. So this can only catch a code in the flat-out WRONG
## therapeutic group (like the Aciclovir example) -- it can NOT catch
## a code that's in the correct group but wrong at level 5 (e.g.
## Sulpiride's real group is N05AL, and N05AL07 sits right there in
## it even though N05AL07 is actually Levosulpiride). That class of
## error needs the actual WHO ATC/DDD level-5 index to resolve.
matched <- atc_map_final |> filter(query_status == "drugbank_match")
rxnav_substances <- matched |> distinct(base_substance) |> pull(base_substance)
cat("\nUnique DrugBank-matched substances to verify against RxNav:", length(rxnav_substances), "\n")

atc_level4_pattern <- "^[A-Z][0-9]{2}[A-Z]{2}$"

get_rxcui <- function(name) {
  res <- tryCatch(
    GET("https://rxnav.nlm.nih.gov/REST/rxcui.json", query = list(name = name, search = 2)),
    error = function(e) NULL
  )
  if (is.null(res) || status_code(res) != 200) return(NA_character_)
  parsed <- tryCatch(fromJSON(content(res, as = "text", encoding = "UTF-8")), error = function(e) NULL)
  ids <- parsed$idGroup$rxnormId
  if (is.null(ids) || length(ids) == 0) return(NA_character_)
  ids[1]
}

get_rxnav_atc_codes <- function(rxcui) {
  if (is.na(rxcui)) return(character(0))
  res <- tryCatch(
    GET("https://rxnav.nlm.nih.gov/REST/rxclass/class/byRxcui.json",
        query = list(rxcui = rxcui, relaSource = "ATC")),
    error = function(e) NULL
  )
  if (is.null(res) || status_code(res) != 200) return(character(0))
  parsed <- tryCatch(fromJSON(content(res, as = "text", encoding = "UTF-8"), flatten = TRUE), error = function(e) NULL)
  info <- parsed$rxclassDrugInfoList$rxclassDrugInfo
  if (is.null(info) || nrow(info) == 0) return(character(0))
  classes <- unique(info$rxclassMinConceptItem.classId)
  classes[str_detect(classes, atc_level4_pattern)]
}

cache_path <- "rxnav_atc_cache.csv"
if (file.exists(cache_path)) {
  rxnav_cache <- read_csv(cache_path, show_col_types = FALSE)
  cat("Resuming from existing RxNav cache:", nrow(rxnav_cache), "substances already queried\n")
} else {
  rxnav_cache <- tibble(base_substance = character(), rxcui = character(), rxnav_atc_codes = character())
}

to_query <- setdiff(rxnav_substances, rxnav_cache$base_substance)
cat("Substances left to query:", length(to_query), "\n")

for (i in seq_along(to_query)) {
  s <- to_query[i]
  rxcui <- get_rxcui(s)
  Sys.sleep(0.15)
  codes <- get_rxnav_atc_codes(rxcui)
  Sys.sleep(0.15)
  rxnav_cache <- bind_rows(rxnav_cache,
                           tibble(base_substance = s, rxcui = rxcui, rxnav_atc_codes = paste(codes, collapse = ",")))
  if (i %% 50 == 0) {
    cat("Queried", i, "/", length(to_query), "-- saving cache\n")
    write_csv(rxnav_cache, cache_path)
  }
}
write_csv(rxnav_cache, cache_path)
cat("RxNav query complete. Total cached:", nrow(rxnav_cache), "\n")

verified <- matched |>
  left_join(rxnav_cache, by = "base_substance") |>
  mutate(
    atc_code_level4_prefix = str_sub(atc_code, 1, 5),
    rxnav_atc_list = str_split(coalesce(rxnav_atc_codes, ""), ","),
    rxnav_has_data = map_lgl(rxnav_atc_list, ~ length(.x) > 0 && any(.x != "")),
    rxnav_group_verified = map2_lgl(atc_code_level4_prefix, rxnav_atc_list, ~ .x %in% .y),
    verification_status = case_when(
      !rxnav_has_data ~ "no_rxnav_data",
      rxnav_group_verified ~ "rxnav_group_verified",
      TRUE ~ "rxnav_group_mismatch_flagged"
    )
  ) |>
  select(-rxnav_atc_list)

cat("\nVerification status breakdown:\n")
print(count(verified, verification_status, sort = TRUE))

write_csv(verified, "cep_substance_atc_lookup_rxnav_verified.csv")

## Drop flagged cross-group mismatches; keep verified + unconfirmed
## (unconfirmed is not the same as wrong -- RxNav simply had no data)
clean_atc_map <- verified |> filter(verification_status != "rxnav_group_mismatch_flagged")
write_csv(clean_atc_map, "cep_substance_atc_lookup_drugbank_rxnav_clean.csv")

## ===================================================================
## STEP 6 -- FILTER TO CRITICAL ATC CODES ONLY
## ===================================================================
critical_codes <- read_csv("Data/critical.csv") 
critical_codes <- critical_codes$`ATC level 5`
cat("\nUnique critical ATC codes loaded:", length(critical_codes), "\n")

critical_atc_map <- clean_atc_map |> filter(atc_code %in% critical_codes)
cat("Substance-code rows matching critical list:", nrow(critical_atc_map),
    "/ unique substances:", n_distinct(critical_atc_map$base_substance), "\n")

## ===================================================================
## STEP 7 -- RE-JOIN TO THE FULL, UNFILTERED CEP DATASET, SAVE FINAL FILE
## ===================================================================
## Final file keeps EVERY row from the original EXPORT_WEB_CEP.txt,
## exactly once each (all statuses, not just Valid) -- same row count
## as the raw input, no duplication and no dropped rows.
##
## Some substances still carry more than one surviving critical ATC
## code after filtering (e.g. Estradiol, or corticosteroids classified
## under multiple routes of administration). A plain left_join would
## duplicate those original rows -- one copy per code -- so instead we
## collapse to ONE row per substance first, joining multiple codes
## into a single comma-separated "ATC code" cell. This guarantees the
## final row count exactly matches the original file.
critical_atc_collapsed <- critical_atc_map |>
  group_by(base_substance) |>
  summarise(atc_code = paste(sort(unique(atc_code)), collapse = ", "), .groups = "drop")

cep_with_atc <- cep_raw |>
  mutate(Substance = str_trim(Substance)) |>
  left_join(critical_atc_collapsed, by = c("Substance" = "base_substance")) |>
  select(all_of(names(cep_raw)), `ATC code` = atc_code)

cat("\nFinal output: rows =", nrow(cep_with_atc),
    "(should equal original raw row count =", nrow(cep_raw), ")\n",
    " | rows with an ATC code filled in =", sum(!is.na(cep_with_atc$`ATC code`) & cep_with_atc$`ATC code` != ""), "\n",
    " | rows with more than one ATC code (comma-separated) =",
    sum(str_detect(coalesce(cep_with_atc$`ATC code`, ""), ",")), "\n")

names(cep_with_atc)[12] <- "atc_code"
write_csv(cep_with_atc, "Data/EXPORT_WEB_CEP_with_ATC_drugbank.csv")

cat("\nWrote:\n",
    " - drugbank_name_atc_lookup.csv                      (full DrugBank name/synonym -> ATC table)\n",
    " - cep_substance_atc_lookup_drugbank.csv              (one row per Valid-CEP substance, all DrugBank matches)\n",
    " - rxnav_atc_cache.csv                                (raw RxNav lookups, cached/resumable)\n",
    " - cep_substance_atc_lookup_rxnav_verified.csv         (every DrugBank match, tagged with verification_status)\n",
    " - cep_substance_atc_lookup_drugbank_rxnav_clean.csv   (flagged cross-group mismatches dropped)\n",
    " - EXPORT_WEB_CEP_with_ATC_drugbank.csv                (FINAL: Valid CEP rows x critical ATC codes only)\n")