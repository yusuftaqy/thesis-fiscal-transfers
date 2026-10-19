# ============================================================
# LAMPIRAN D: EMPAT TABEL ESTIMASI - FULL BASELINE CONTROL (3 TAHUN)
# Data revisi: alokasi nol dimasukkan -> ln(DAK + 1).
# Kontrol = 7 baseline DITAMPILKAN koefisiennya (extended TIDAK dipakai
# karena hanya tersedia 2019; extended ada di tabel robustness terpisah).
# Empat tabel: Vote kontinu / Core / Swing / Political Investment.
# Kolom = 6 bidang DAK x 3 tahun (2017, 2018, 2019), header dua tingkat.
# Robust SE HC1, province FE, konstanta, F robust.
# Output: docx (Times New Roman), satu file 4 tabel.
# ============================================================

# -- 0. PACKAGE --------------------------------------------------
pkg <- c("tidyverse", "readxl", "sandwich", "lmtest", "flextable", "officer")
miss <- pkg[!pkg %in% rownames(installed.packages())]
if (length(miss) > 0) install.packages(miss)
invisible(lapply(pkg, library, character.only = TRUE))

# -- 1. PATH (relatif terhadap root repo) ------------------------
data_dir   <- "data"
output_dir <- "output"
input_path  <- file.path(data_dir, "Full_Variable_Clean.xlsx")
output_docx <- file.path(output_dir, "Lampiran_D_FullControl_NolMasuk_3Tahun.docx")

# -- 2. LOAD & BERSIHKAN ----------------------------------------
df <- read_excel(input_path, sheet = "Data")
df <- df %>% filter(!is.na(id_daerah), !is.na(year))
df <- df %>% mutate(province_id = as.factor(substr(as.character(id_daerah), 1, 2)))

if ("swing" %in% names(df)) df$swing <- suppressWarnings(as.numeric(as.character(df$swing)))

# NOLKAN DAK kosong + ln(DAK+1) -- keenam bidang, semua tahun
dak_pc <- c("dak_total_pc","dak_jalan_pc","dak_pasar_pc",
            "dak_airminum_pc","dak_sanitasi_pc","dak_irigasi_pc")
for (c in dak_pc) {
  df[[c]][is.na(df[[c]])] <- 0
  df[[paste0("ln_", c)]] <- log(df[[c]] + 1)
}

# -- 3. VARIABEL -------------------------------------------------
vote_col <- "voteshare_jkw14"
# Kontrol baseline saja (7) -- tersedia di ketiga tahun.
controls_base <- c("ln_fiscap_pc","ln_pdrb_pc","L_hdi",
                   "ln_populasi","dummy_urban","L_sanitasi_akses","L_airminum_akses")

ctrl_label <- c(
  ln_fiscap_pc     = "Kapasitas Fiskal (ln)",
  ln_pdrb_pc       = "PDRB per Kapita (ln)",
  L_hdi            = "IPM",
  ln_populasi      = "Populasi (ln)",
  dummy_urban      = "Dummy Kota (Urban)",
  L_sanitasi_akses = "Akses Sanitasi (%)",
  L_airminum_akses = "Akses Air Minum (%)"
)

depvar_list <- paste0("ln_", dak_pc)
depvar_short <- c("DAK Total","DAK Jalan","DAK Pasar","DAK Air Minum","DAK Sanitasi","DAK Irigasi")
names(depvar_short) <- depvar_list
years <- c(2017, 2018, 2019)

# -- 4. ESTIMASI (per tahun, baseline control) ------------------
run_model <- function(data, depvar, polit_terms) {
  rhs <- paste(c(polit_terms, controls_base, "province_id"), collapse = " + ")
  fml <- as.formula(paste(depvar, "~", rhs))
  need <- c(depvar, polit_terms, controls_base)
  need <- need[need %in% names(data)]
  d <- data %>% filter(if_all(all_of(need), ~ !is.na(.)))
  if (nrow(d) < 15) return(NULL)
  fit <- lm(fml, data = d)
  V <- sandwich::vcovHC(fit, type = "HC1")
  cft <- lmtest::coeftest(fit, vcov = V)
  fit_r <- lm(as.formula(paste(depvar, "~ province_id")), data = d)
  wt <- tryCatch(lmtest::waldtest(fit_r, fit, vcov = V, test = "F"), error = function(e) NULL)
  list(cft=cft, n=nobs(fit), r2=summary(fit)$r.squared, adj_r2=summary(fit)$adj.r.squared,
       f=if(!is.null(wt)) wt$F[2] else NA_real_)
}

# -- 5. FORMAT --------------------------------------------------
fmt <- function(x,d=3) formatC(x, format="f", digits=d, decimal.mark=",")
stars <- function(p) ifelse(is.na(p),"",ifelse(p<0.01,"***",ifelse(p<0.05,"**",ifelse(p<0.10,"*",""))))
grab <- function(res, term) {
  if (is.null(res)) return(list(est="", se=""))
  rn <- rownames(res$cft)
  hit <- rn[rn==term]; if (length(hit)==0) hit <- rn[startsWith(rn, term)]; hit <- hit[1]
  if (is.na(hit)) return(list(est="", se=""))
  b<-res$cft[hit,"Estimate"]; se<-res$cft[hit,"Std. Error"]; p<-res$cft[hit,"Pr(>|t|)"]
  list(est=paste0(fmt(b),stars(p)), se=paste0("[",fmt(se),"]"))
}

# kolom = tiap kombinasi DAK x tahun (18 kolom)
col_defs <- expand.grid(dep=depvar_list, yr=years, stringsAsFactors=FALSE) %>%
  arrange(match(dep, depvar_list), yr) %>%
  mutate(colname = paste0(depvar_short[dep], " ", yr))

build_table <- function(polit_terms, polit_labels) {
  results <- list()
  for (i in seq_len(nrow(col_defs)))
    results[[col_defs$colname[i]]] <- run_model(df %>% filter(year==col_defs$yr[i]), col_defs$dep[i], polit_terms)
  make_row <- function(label, getter) c(Variabel=label, sapply(col_defs$colname, function(cn) getter(results[[cn]])))
  rows <- list()
  # variabel politik
  for (k in seq_along(polit_terms)) {
    rows[[length(rows)+1]] <- make_row(polit_labels[k], function(r) grab(r, polit_terms[k])$est)
    rows[[length(rows)+1]] <- make_row("", function(r) grab(r, polit_terms[k])$se)
  }
  # 7 baseline controls ditampilkan
  for (cv in controls_base) {
    rows[[length(rows)+1]] <- make_row(ctrl_label[[cv]], function(r) grab(r, cv)$est)
    rows[[length(rows)+1]] <- make_row("",               function(r) grab(r, cv)$se)
  }
  # konstanta + footer
  rows[[length(rows)+1]] <- make_row("Konstanta", function(r) grab(r, "(Intercept)")$est)
  rows[[length(rows)+1]] <- make_row("",          function(r) grab(r, "(Intercept)")$se)
  rows[[length(rows)+1]] <- make_row("Province FE", function(r) if(!is.null(r)) "Ya" else "")
  rows[[length(rows)+1]] <- make_row("N",           function(r) if(!is.null(r)) as.character(r$n) else "")
  rows[[length(rows)+1]] <- make_row("R\u00B2",          function(r) if(!is.null(r)) fmt(r$r2,3) else "")
  rows[[length(rows)+1]] <- make_row("Adjusted R\u00B2", function(r) if(!is.null(r)) fmt(r$adj_r2,3) else "")
  rows[[length(rows)+1]] <- make_row("F-statistik", function(r) if(!is.null(r) && !is.na(r$f)) fmt(r$f,2) else "")
  tbl <- as.data.frame(do.call(rbind, rows), stringsAsFactors=FALSE, check.names=FALSE)
  rownames(tbl) <- NULL
  tbl
}

# -- 6. EMPAT MODEL ---------------------------------------------
tbl1 <- build_table(vote_col, "Vote Share (0-100)")
tbl2 <- build_table("x6", "Core supporter (>70%)")
tbl3 <- build_table("swing", "Daerah kompetitif (margin <=5%)")
tbl4 <- build_table(c("x1","x2","x3","x4","x5"),
                    c("Dukungan sangat rendah (0-30%)","Dukungan rendah (30-40%)",
                      "Dukungan sedang-rendah (40-50%)","Dukungan sedang-tinggi (50-60%)",
                      "Dukungan tinggi (60-70%)"))

# -- 7. EXPORT DOCX (header dua tingkat: DAK membentang tahun) --
mk_ft <- function(tbl) {
  ft <- flextable(tbl)
  ck <- names(tbl); top <- c(""); bot <- c("Variabel")
  for (cn in ck[-1]) { parts <- strsplit(cn, " (?=[0-9]{4}$)", perl=TRUE)[[1]]; top <- c(top, parts[1]); bot <- c(bot, parts[2]) }
  ft <- set_header_df(ft, mapping=data.frame(col_keys=ck, top=top, bot=bot, stringsAsFactors=FALSE), key="col_keys")
  ft <- merge_h(ft, part="header"); ft <- merge_v(ft, j=1, part="header")
  ft <- theme_booktabs(ft); ft <- fontsize(ft, size=6, part="all"); ft <- bold(ft, part="header")
  ft <- font(ft, fontname="Times New Roman", part="all")
  ft <- align(ft, j=1, align="left", part="all")
  ft <- align(ft, j=2:ncol(tbl), align="center", part="all")
  ft <- align(ft, align="center", part="header")
  autofit(ft)
}

note <- paste0(
  "Catatan: Estimasi per tahun (2017, 2018, 2019) dengan seluruh baseline controls ditampilkan. ",
  "Variabel dependen ln(DAK per kapita + 1); alokasi nol ikut diestimasi. Standard errors dalam kurung siku ",
  "merupakan robust standard errors (HC1). F-statistik merupakan uji Wald robust atas variabel penjelas ",
  "(province fixed effects tidak diuji). *** p<0,01; ** p<0,05; * p<0,10."
)
note_pi <- paste("Kategori dukungan dibandingkan terhadap referensi core supporter (>70%); koefisien negatif berarti menerima lebih rendah dari core.", note)

doc <- read_docx()
doc <- body_add_par(doc, "Lampiran D1. Estimasi Vote Share Kontinu dengan Baseline Control (nol dimasukkan), 2017-2019", style="heading 1")
doc <- body_add_flextable(doc, mk_ft(tbl1)); doc <- body_add_par(doc, note, style="Normal"); doc <- body_add_par(doc,"")
doc <- body_add_par(doc, "Lampiran D2. Estimasi Model Core Voter dengan Baseline Control (nol dimasukkan), 2017-2019", style="heading 1")
doc <- body_add_flextable(doc, mk_ft(tbl2)); doc <- body_add_par(doc, note, style="Normal"); doc <- body_add_par(doc,"")
doc <- body_add_par(doc, "Lampiran D3. Estimasi Model Swing Voter dengan Baseline Control (nol dimasukkan), 2017-2019", style="heading 1")
doc <- body_add_flextable(doc, mk_ft(tbl3)); doc <- body_add_par(doc, note, style="Normal"); doc <- body_add_par(doc,"")
doc <- body_add_par(doc, "Lampiran D4. Estimasi Model Political Investment dengan Baseline Control (nol dimasukkan), 2017-2019", style="heading 1")
doc <- body_add_flextable(doc, mk_ft(tbl4)); doc <- body_add_par(doc, note_pi, style="Normal")
print(doc, target=output_docx)
message("SELESAI. Tersimpan di: ", normalizePath(output_docx, mustWork=FALSE))
