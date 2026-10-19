# ============================================================
# RE-ESTIMASI DENGAN ALOKASI NOL DIMASUKKAN (ln(DAK+1))
# Perubahan metodologis: alokasi DAK yang kosong (tidak menerima)
# dinolkan agar ikut masuk estimasi sebagai informasi substantif,
# lalu ditransformasi ln(DAK + 1) agar nol terdefinisi.
#
# Menghasilkan 4 tabel (memanjang 6 DAK x 3 tahun, header dua tingkat):
#   Tabel 1: Vote share kontinu
#   Tabel 2: Core Voter (x6)
#   Tabel 3: Political Investment (x1-x5, tanpa swing)
#   Tabel 4: Swing Voter (swing)
# Spesifikasi: OLS per tahun, province FE, robust SE HC1, konstanta tampil.
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
output_xlsx <- file.path(output_dir, "Hasil_Estimasi_NolMasuk.xlsx")

# -- 2. LOAD & BERSIHKAN ----------------------------------------
df <- read_excel(input_path, sheet = input_sheet)

# Buang baris non-data (legenda/keterangan di bawah tabel): wajib punya id & year.
n_awal <- nrow(df)
df <- df %>% filter(!is.na(id_daerah), !is.na(year))
message("Baris dibuang (legenda/kosong): ", n_awal - nrow(df), " | tersisa: ", nrow(df))

df <- df %>% mutate(province_id = as.factor(substr(as.character(id_daerah), 1, 2)))

# 'luas' bisa terbaca teks -> paksa numeric (mencegah overfit).
if ("luas" %in% names(df) && !is.numeric(df$luas)) {
  lc <- as.character(df$luas); hc <- grepl(",", lc)
  lc[hc] <- gsub("\\.", "", lc[hc]); lc <- gsub(",", ".", lc)
  df$luas <- suppressWarnings(as.numeric(lc))
}
if ("swing" %in% names(df)) df$swing <- suppressWarnings(as.numeric(as.character(df$swing)))

# -- 2b. NOLKAN DAK KOSONG + ln(DAK + 1) ------------------------
# Alokasi kosong = daerah tidak menerima DAK (terbukti: bila DAK Total kosong,
# seluruh bidang juga kosong; umumnya kota maju yang bukan sasaran DAK).
# Karena itu missing di-set 0, lalu ln(DAK + 1) agar nol terdefinisi (ln(1)=0).
dak_pc <- c("dak_total_pc","dak_jalan_pc","dak_pasar_pc",
            "dak_airminum_pc","dak_sanitasi_pc","dak_irigasi_pc")

cat("\n=== LAPORAN: sel missing -> 0 per bidang per tahun ===\n")
for (c in dak_pc) {
  for (yr in c(2017,2018,2019)) {
    n_miss <- sum(is.na(df[[c]][df$year==yr]))
    cat(sprintf("%-18s %d : %3d sel dinolkan\n", c, yr, n_miss))
  }
}

# Set nol lalu buat kolom ln(DAK+1)
for (c in dak_pc) {
  df[[c]][is.na(df[[c]])] <- 0
  df[[paste0("ln_", c)]] <- log(df[[c]] + 1)   # ln(DAK + 1)
}
cat("\nKolom ln(DAK+1) dibuat: ", paste0("ln_", dak_pc, collapse=", "), "\n\n")

# -- 3. VARIABEL -------------------------------------------------
vote_col <- "voteshare_jkw14"
required_political <- c("x1","x2","x3","x4","x5","x6","swing")
missing_pol <- setdiff(c(vote_col, required_political), names(df))
if (length(missing_pol) > 0) stop("Variabel tidak ditemukan: ", paste(missing_pol, collapse=", "), call.=FALSE)

controls_base <- c("ln_fiscap_pc","ln_pdrb_pc","L_hdi",
                   "ln_populasi","dummy_urban","L_sanitasi_akses","L_airminum_akses")

depvar_list <- paste0("ln_", dak_pc)
depvar_short <- c("DAK Total","DAK Jalan","DAK Pasar","DAK Air Minum","DAK Sanitasi","DAK Irigasi")
names(depvar_short) <- depvar_list
years <- c(2017, 2018, 2019)

# -- 4. ESTIMASI GENERIK ----------------------------------------
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
  # F robust: model penuh vs hanya province FE (dummy provinsi tidak diuji;
  # menghindari matriks singular pada N kecil, lebih bermakna secara substantif).
  fit_r <- lm(as.formula(paste(depvar, "~ province_id")), data = d)
  wt <- tryCatch(lmtest::waldtest(fit_r, fit, vcov = V, test = "F"), error = function(e) NULL)
  f_stat <- if (!is.null(wt)) wt$F[2] else NA_real_
  list(cft=cft, n=nobs(fit), r2=summary(fit)$r.squared, adj_r2=summary(fit)$adj.r.squared, f_stat=f_stat)
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

col_defs <- expand.grid(dep=depvar_list, yr=years, stringsAsFactors=FALSE) %>%
  arrange(match(dep, depvar_list), yr) %>%
  mutate(colname = paste0(depvar_short[dep], " ", yr))

build_table <- function(polit_terms, polit_labels) {
  results <- list()
  for (i in seq_len(nrow(col_defs)))
    results[[col_defs$colname[i]]] <- run_model(df %>% filter(year==col_defs$yr[i]), col_defs$dep[i], polit_terms)
  make_row <- function(label, getter) c(Variabel=label, sapply(col_defs$colname, function(cn) getter(results[[cn]])))
  rows <- list()
  for (k in seq_along(polit_terms)) {
    rows[[length(rows)+1]] <- make_row(polit_labels[k], function(r) grab(r, polit_terms[k])$est)
    rows[[length(rows)+1]] <- make_row("", function(r) grab(r, polit_terms[k])$se)
  }
  rows[[length(rows)+1]] <- make_row("Konstanta", function(r) grab(r, "(Intercept)")$est)
  rows[[length(rows)+1]] <- make_row("",          function(r) grab(r, "(Intercept)")$se)
  rows[[length(rows)+1]] <- make_row("Baseline controls", function(r) if(!is.null(r)) "Yes" else "")
  rows[[length(rows)+1]] <- make_row("Province FE",        function(r) if(!is.null(r)) "Yes" else "")
  rows[[length(rows)+1]] <- make_row("N",                  function(r) if(!is.null(r)) as.character(r$n) else "")
  rows[[length(rows)+1]] <- make_row("R²",                 function(r) if(!is.null(r)) fmt(r$r2,3) else "")
  rows[[length(rows)+1]] <- make_row("Adjusted R²",        function(r) if(!is.null(r)) fmt(r$adj_r2,3) else "")
  rows[[length(rows)+1]] <- make_row("F-statistik (robust)", function(r) if(!is.null(r) && !is.na(r$f_stat)) fmt(r$f_stat,2) else "")
  tbl <- as.data.frame(do.call(rbind, rows), stringsAsFactors=FALSE, check.names=FALSE)
  rownames(tbl) <- NULL
  tbl
}

# -- 6. EMPAT MODEL ---------------------------------------------
tbl1 <- build_table(vote_col, "Vote Share (0-100)")
tbl2 <- build_table("x6", "Core supporter (>70%)")
tbl3 <- build_table(c("x1","x2","x3","x4","x5"),
                    c("Dukungan sangat rendah (0-30%)","Dukungan rendah (30-40%)",
                      "Dukungan sedang-rendah (40-50%)","Dukungan sedang-tinggi (50-60%)",
                      "Dukungan tinggi (60-70%)"))
tbl4 <- build_table("swing", "Daerah kompetitif (margin <=5%)")

# -- 7. ROBUSTNESS: EXTENDED CONTROLS 2019 ----------------------
# Bangun total_sekolah & total_faskes dari komponen (nama sesuai data).
if (!"total_sekolah" %in% names(df) && all(c("L_schl_prm","L_schl_jrsec","L_schl_srsec") %in% names(df)))
  df <- df %>% mutate(total_sekolah = L_schl_prm + L_schl_jrsec + L_schl_srsec)
if (!"total_faskes" %in% names(df) && all(c("L_hosp","L_puskesmas") %in% names(df)))
  df <- df %>% mutate(total_faskes = L_hosp + L_puskesmas)

extended_candidates <- c("total_sekolah","total_faskes","L_jalan_aspal","L_jalan_kerikil","luas")
extended_ctrl <- extended_candidates[extended_candidates %in% names(df)]
message("Extended controls dipakai: ", paste(extended_ctrl, collapse=", "))

df2019 <- df %>% filter(year == 2019)

run_ext <- function(depvar, polit_terms) {
  rhs <- paste(c(polit_terms, controls_base, extended_ctrl, "province_id"), collapse=" + ")
  fml <- as.formula(paste(depvar, "~", rhs))
  need <- c(depvar, polit_terms, controls_base, extended_ctrl)
  need <- need[need %in% names(df2019)]
  d <- df2019 %>% filter(if_all(all_of(need), ~ !is.na(.)))
  if (nrow(d) < 15) return(NULL)
  fit <- lm(fml, data=d); V <- sandwich::vcovHC(fit, type="HC1")
  cft <- lmtest::coeftest(fit, vcov=V)
  fit_r <- lm(as.formula(paste(depvar, "~ province_id")), data=d)
  wt <- tryCatch(lmtest::waldtest(fit_r, fit, vcov=V, test="F"), error=function(e) NULL)
  f_stat <- if (!is.null(wt)) wt$F[2] else NA_real_
  list(cft=cft, n=nobs(fit), r2=summary(fit)$r.squared, adj_r2=summary(fit)$adj.r.squared, f_stat=f_stat)
}

# Tabel robustness: baris = bidang DAK; kolom = koef tiap model politik + stats.
# Untuk ringkas, robustness menampilkan koefisien variabel politik utama per model.
build_robust <- function() {
  out <- list()
  for (i in seq_along(depvar_list)) {
    dv <- depvar_list[i]; lab <- depvar_short[dv]
    core <- run_ext(dv, "x6")
    swing<- run_ext(dv, "swing")
    # PI: ambil kategori dengan pola (tampilkan 60-70% sebagai wakil, plus semua bisa dilihat di docx sebelumnya)
    pi5  <- run_ext(dv, c("x1","x2","x3","x4","x5"))
    row <- data.frame(
      Bidang = lab,
      Core_koef   = if(!is.null(core)) grab(core,"x6")$est else "",
      Core_N      = if(!is.null(core)) core$n else NA,
      Core_F      = if(!is.null(core) && !is.na(core$f_stat)) fmt(core$f_stat,2) else "",
      Swing_koef  = if(!is.null(swing)) grab(swing,"swing")$est else "",
      Swing_N     = if(!is.null(swing)) swing$n else NA,
      Swing_F     = if(!is.null(swing) && !is.na(swing$f_stat)) fmt(swing$f_stat,2) else "",
      PI_60_70    = if(!is.null(pi5)) grab(pi5,"x5")$est else "",
      PI_50_60    = if(!is.null(pi5)) grab(pi5,"x4")$est else "",
      PI_N        = if(!is.null(pi5)) pi5$n else NA,
      PI_F        = if(!is.null(pi5) && !is.na(pi5$f_stat)) fmt(pi5$f_stat,2) else "",
      stringsAsFactors=FALSE
    )
    out[[length(out)+1]] <- row
  }
  do.call(rbind, out)
}
tbl_robust <- build_robust()

note <- "Variabel dependen ln(DAK per kapita + 1); alokasi nol (daerah tidak menerima) ikut diestimasi. Standard errors dalam kurung siku merupakan robust standard errors (HC1). Seluruh model menyertakan baseline controls dan province FE. *** p<0,01; ** p<0,05; * p<0,10."
note_pi <- paste("Kategori dukungan dibandingkan terhadap referensi Core Supporter (>70%); koefisien negatif berarti menerima lebih rendah dari core.", note)

# -- 8. EXPORT KE EXCEL (multi-sheet) ---------------------------
wb <- createWorkbook()
hdr_style <- createStyle(textDecoration="bold", halign="center", valign="center",
                         fgFill="#D9E1F2", border="TopBottomLeftRight")
cell_style <- createStyle(halign="center", valign="center", border="TopBottomLeftRight")
left_style <- createStyle(halign="left", valign="center", border="TopBottomLeftRight")

add_sheet <- function(name, tbl, catatan) {
  addWorksheet(wb, name)
  writeData(wb, name, tbl, startRow=2, headerStyle=hdr_style, borders="all")
  writeData(wb, name, name, startRow=1, startCol=1)  # judul sheet di baris 1
  addStyle(wb, name, createStyle(textDecoration="bold", fontSize=12), rows=1, cols=1)
  # catatan di bawah tabel
  nr <- nrow(tbl) + 4
  writeData(wb, name, catatan, startRow=nr, startCol=1)
  setColWidths(wb, name, cols=1, widths=28)
  setColWidths(wb, name, cols=2:(ncol(tbl)+1), widths=14)
  freezePane(wb, name, firstActiveRow=3, firstActiveCol=2)
}

add_sheet("1_VoteShare_Kontinu", tbl1, note)
add_sheet("2_Core_Voter", tbl2, note)
add_sheet("3_Political_Investment", tbl3, note_pi)
add_sheet("4_Swing_Voter", tbl4, note)
add_sheet("5_Robustness_Extended2019", tbl_robust,
          "Robustness extended controls (2019): koefisien variabel politik utama per model + N. Extended: total sekolah, faskes, jalan aspal & kerikil, luas. ln(DAK+1), robust SE HC1, province FE. Koefisien lengkap PI (x1-x5) lihat sheet 3.")

# font Times New Roman untuk seluruh workbook
for (sh in names(wb)) {
  addStyle(wb, sh, createStyle(fontName="Times New Roman"),
           rows=1:60, cols=1:20, gridExpand=TRUE, stack=TRUE)
}

saveWorkbook(wb, output_xlsx, overwrite=TRUE)
message("SELESAI. Excel tersimpan di: ", normalizePath(output_xlsx, mustWork=FALSE))
