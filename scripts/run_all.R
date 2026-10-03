invisible(Sys.setlocale("LC_CTYPE", "en_US.UTF-8"))
if (!l10n_info()$`UTF-8`) stop("A UTF-8 locale is required: the country maps contain non-ASCII names.")
scripts <- sort(list.files("scripts/registers", pattern = "^[0-9]{2}_.*\\.R$", full.names = TRUE))
for (s in scripts) {
  message("==> ", s)
  local(source(s, local = TRUE, encoding = "UTF-8"))
}
