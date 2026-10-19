# ============================================================
# TABEL ROBUSTNESS CHECK - EXTENDED CONTROLS 2019 (format bertumpuk)
# Empat blok model ditumpuk vertikal dalam satu tabel:
#   Vote (kontinu) / Core / Swing / Political Investment (x1-x5)
# Kolom = 6 bidang DAK. Tiap blok: koefisien + SE, lalu R²/Adj R²/F.
# ln(DAK+1), robust SE HC1, province FE, extended controls, tahun 2019.
# Output: docx (Times New Roman) + tercetak di console.
# ============================================================

# -- 0. PACKAGE --------------------------------------------------
pkg <- c("tidyverse", "readxl", "sandwich", "lmtest", "openxlsx")
miss <- pkg[!pkg %in% rownames(installed.packages())]
if (length(miss) > 0) install.packages(miss)
invisible(lapply(pkg, library, character.only = TRUE))

# -- 1. PATH (relatif terhadap root repo) ------------------------
data_dir   <- "data"
output_dir <- "output"
input_path  <- file.path(data_dir, "Full_Variable_Clean.xlsx")
input_sheet <- "Data"
output_xlsx <- file.path(output_dir, "Tabel_Robustness_Extended_2019.xlsx")

# -- 2. LOAD & BERSIHKAN ----------------------------------------
df <- read_excel(input_path, sheet = input_sheet)
df <- df %>% filter(!is.na(id_daerah), !is.na(year))
df <- df %>% mutate(province_id = as.factor(substr(as.character(id_daerah), 1, 2)))

# luas -> numeric (cegah overfit)
if ("luas" %in% names(df) && !is.numeric(df$luas)) {
  lc <- as.character(df$luas); hc <- grepl(",", lc)
  lc[hc] <- gsub("\\.", "", lc[hc]); lc <- gsub(",", ".", lc)
  df$luas <- suppressWarnings(as.numeric(lc))
}
if ("swing" %in% names(df)) df$swing <- suppressWarnings(as.numeric(as.character(df$swing)))

# NOLKAN DAK kosong + ln(DAK+1)
dak_pc <- c("dak_total_pc","dak_jalan_pc","dak_pasar_pc",
            "dak_airminum_pc","dak_sanitasi_pc","dak_irigasi_pc")
for (c in dak_pc) {
  df[[c]][is.na(df[[c]])] <- 0
  df[[paste0("ln_", c)]] <- log(df[[c]] + 1)
}

# total_sekolah & total_faskes dari komponen
if (!"total_sekolah" %in% names(df) && all(c("L_schl_prm","L_schl_jrsec","L_schl_srsec") %in% names(df)))
  df <- df %>% mutate(total_sekolah = L_schl_prm + L_schl_jrsec + L_schl_srsec)
if (!"total_faskes" %in% names(df) && all(c("L_hosp","L_puskesmas") %in% names(df)))
  df <- df %>% mutate(total_faskes = L_hosp + L_puskesmas)

# -- 3. VARIABEL -------------------------------------------------
vote_col <- "voteshare_jkw14"
controls_base <- c("ln_fiscap_pc","ln_pdrb_pc","L_hdi",
                   "ln_populasi","dummy_urban","L_sanitasi_akses","L_airminum_akses")
extended_candidates <- c("total_sekolah","total_faskes","L_jalan_aspal","L_jalan_kerikil","luas")
extended_ctrl <- extended_candidates[extended_candidates %in% names(df)]
message("Extended controls: ", paste(extended_ctrl, collapse=", "))

depvar_list <- paste0("ln_", dak_pc)
depvar_short <- c("Total","Jalan","Pasar","Air Minum","Sanitasi","Irigasi")

df19 <- df %>% filter(year == 2019)

# -- 4. ESTIMASI (2019 + extended) ------------------------------
est <- function(depvar, polit_terms) {
  rhs <- paste(c(polit_terms, controls_base, extended_ctrl, "province_id"), collapse=" + ")
  fml <- as.formula(paste(depvar, "~", rhs))
  need <- c(depvar, polit_terms, controls_base, extended_ctrl)
  need <- need[need %in% names(df19)]
  d <- df19 %>% filter(if_all(all_of(need), ~ !is.na(.)))
  if (nrow(d) < 15) return(NULL)
  fit <- lm(fml, data=d); V <- sandwich::vcovHC(fit, type="HC1")
  cft <- lmtest::coeftest(fit, vcov=V)
  fit_r <- lm(as.formula(paste(depvar, "~ province_id")), data=d)
  wt <- tryCatch(lmtest::waldtest(fit_r, fit, vcov=V, test="F"), error=function(e) NULL)
  list(cft=cft, n=nobs(fit), r2=summary(fit)$r.squared, adj_r2=summary(fit)$adj.r.squared,
       f=if(!is.null(wt)) wt$F[2] else NA_real_)
}

# -- 5. FORMAT --------------------------------------------------
f3 <- function(x) formatC(x, format="f", digits=3, decimal.mark=",")
f2 <- function(x) formatC(x, format="f", digits=2, decimal.mark=",")
stars <- function(p) ifelse(is.na(p),"",ifelse(p<0.01,"***",ifelse(p<0.05,"**",ifelse(p<0.10,"*",""))))
cell <- function(res, term, want=c("est","se")) {
  want <- match.arg(want)
  if (is.null(res)) return("")
  rn <- rownames(res$cft); hit <- rn[rn==term]; if(length(hit)==0) hit <- rn[startsWith(rn,term)]; hit <- hit[1]
  if (is.na(hit)) return("")
  if (want=="est") paste0(f3(res$cft[hit,"Estimate"]), stars(res$cft[hit,"Pr(>|t|)"]))
  else paste0("(", f3(res$cft[hit,"Std. Error"]), ")")
}
statline <- function(res, which) {
  if (is.null(res)) return("")
  switch(which,
    r2  = f3(res$r2), adj = f3(res$adj_r2),
    f   = if(!is.na(res$f)) f2(res$f) else "", n = as.character(res$n))
}

# -- 6. BANGUN TABEL BERTUMPUK ----------------------------------
# Estimasi tiap model untuk 6 bidang
res_vote  <- lapply(depvar_list, function(dv) est(dv, vote_col))
res_core  <- lapply(depvar_list, function(dv) est(dv, "x6"))
res_swing <- lapply(depvar_list, function(dv) est(dv, "swing"))
res_pi    <- lapply(depvar_list, function(dv) est(dv, c("x1","x2","x3","x4","x5")))
names(res_vote) <- names(res_core) <- names(res_swing) <- names(res_pi) <- depvar_short

# helper: satu baris = label + nilai per bidang
rowvals <- function(reslist, fn) c(sapply(depvar_short, function(b) fn(reslist[[b]])))

R <- list()
addrow <- function(label, vals) R[[length(R)+1]] <<- c(Variabel=label, vals)

# BLOK 1: VOTE KONTINU
addrow("Vote (%)",   rowvals(res_vote, function(r) cell(r, vote_col, "est")))
addrow("",           rowvals(res_vote, function(r) cell(r, vote_col, "se")))
addrow("Konstanta",  rowvals(res_vote, function(r) cell(r, "(Intercept)", "est")))
addrow("",           rowvals(res_vote, function(r) cell(r, "(Intercept)", "se")))
addrow("R²",         rowvals(res_vote, function(r) statline(r,"r2")))
addrow("Adjusted R²",rowvals(res_vote, function(r) statline(r,"adj")))
addrow("F-statistik",rowvals(res_vote, function(r) statline(r,"f")))

# BLOK 2: CORE
addrow("Core supporter (>70%)", rowvals(res_core, function(r) cell(r,"x6","est")))
addrow("",                      rowvals(res_core, function(r) cell(r,"x6","se")))
addrow("Konstanta",  rowvals(res_core, function(r) cell(r,"(Intercept)","est")))
addrow("",           rowvals(res_core, function(r) cell(r,"(Intercept)","se")))
addrow("R²",         rowvals(res_core, function(r) statline(r,"r2")))
addrow("Adjusted R²",rowvals(res_core, function(r) statline(r,"adj")))
addrow("F-statistik",rowvals(res_core, function(r) statline(r,"f")))

# BLOK 3: SWING
addrow("Daerah kompetitif (margin ≤5%)", rowvals(res_swing, function(r) cell(r,"swing","est")))
addrow("",                                rowvals(res_swing, function(r) cell(r,"swing","se")))
addrow("Konstanta",  rowvals(res_swing, function(r) cell(r,"(Intercept)","est")))
addrow("",           rowvals(res_swing, function(r) cell(r,"(Intercept)","se")))
addrow("R²",         rowvals(res_swing, function(r) statline(r,"r2")))
addrow("Adjusted R²",rowvals(res_swing, function(r) statline(r,"adj")))
addrow("F-statistik",rowvals(res_swing, function(r) statline(r,"f")))

# BLOK 4: POLITICAL INVESTMENT (x1-x5)
pi_terms  <- c("x1","x2","x3","x4","x5")
pi_labels <- c("Dukungan sangat rendah (0-30%)","Dukungan rendah (30-40%)",
               "Dukungan sedang-rendah (40-50%)","Dukungan sedang-tinggi (50-60%)",
               "Dukungan tinggi (60-70%)")
for (k in seq_along(pi_terms)) {
  addrow(pi_labels[k], rowvals(res_pi, function(r) cell(r, pi_terms[k], "est")))
  addrow("",           rowvals(res_pi, function(r) cell(r, pi_terms[k], "se")))
}
addrow("Konstanta",  rowvals(res_pi, function(r) cell(r,"(Intercept)","est")))
addrow("",           rowvals(res_pi, function(r) cell(r,"(Intercept)","se")))
addrow("R²",         rowvals(res_pi, function(r) statline(r,"r2")))
addrow("Adjusted R²",rowvals(res_pi, function(r) statline(r,"adj")))
addrow("F-statistik",rowvals(res_pi, function(r) statline(r,"f")))

# FOOTER bersama (N dari model PI sebagai wakil, extended & FE)
addrow("N",                rowvals(res_pi, function(r) statline(r,"n")))
addrow("Extended controls", setNames(rep("Ya", length(depvar_short)), depvar_short))
addrow("Province FE",       setNames(rep("Ya", length(depvar_short)), depvar_short))

tabel <- as.data.frame(do.call(rbind, R), stringsAsFactors=FALSE, check.names=FALSE)
names(tabel) <- c("Variabel", depvar_short)
rownames(tabel) <- NULL
print(tabel)

# -- 7. EXPORT KE EXCEL -----------------------------------------
wb <- createWorkbook()
addWorksheet(wb, "Robustness_Extended2019")

# tulis judul, tabel, catatan
writeData(wb, 1, "Tabel. Robustness Check dengan Extended Controls, 2019", startRow=1, startCol=1)
addStyle(wb, 1, createStyle(textDecoration="bold", fontSize=12, fontName="Times New Roman"), rows=1, cols=1)

hdr_style <- createStyle(textDecoration="bold", halign="center", valign="center",
                         fgFill="#D9E1F2", border="TopBottomLeftRight", fontName="Times New Roman")
writeData(wb, 1, tabel, startRow=3, headerStyle=hdr_style, borders="all")

# style badan tabel
body_style <- createStyle(fontName="Times New Roman", halign="center", border="TopBottomLeftRight")
left_style <- createStyle(fontName="Times New Roman", halign="left", border="TopBottomLeftRight")
nr <- nrow(tabel); nc <- ncol(tabel)
addStyle(wb, 1, body_style, rows=4:(3+nr), cols=2:nc, gridExpand=TRUE, stack=TRUE)
addStyle(wb, 1, left_style, rows=4:(3+nr), cols=1, gridExpand=TRUE, stack=TRUE)

# catatan di bawah tabel
note <- paste0(
  "Catatan: Estimasi tahun 2019 dengan extended controls (total sekolah, total faskes, ",
  "jalan beraspal, jalan kerikil, luas wilayah). Variabel dependen ln(DAK per kapita + 1); ",
  "alokasi nol ikut diestimasi. Empat model diestimasi terpisah. Kategori Political Investment ",
  "dibandingkan terhadap referensi core supporter (>70%). Standard errors dalam kurung merupakan ",
  "robust standard errors (HC1). F-statistik merupakan uji Wald robust atas variabel penjelas ",
  "(province FE tidak diuji). *** p<0,01; ** p<0,05; * p<0,10."
)
writeData(wb, 1, note, startRow=3+nr+2, startCol=1)
addStyle(wb, 1, createStyle(fontName="Times New Roman", fontSize=9, wrapText=TRUE),
         rows=3+nr+2, cols=1)

setColWidths(wb, 1, cols=1, widths=32)
setColWidths(wb, 1, cols=2:nc, widths=13)
freezePane(wb, 1, firstActiveRow=4, firstActiveCol=2)

saveWorkbook(wb, output_xlsx, overwrite=TRUE)
message("SELESAI. Excel tersimpan di: ", normalizePath(output_xlsx, mustWork=FALSE))
