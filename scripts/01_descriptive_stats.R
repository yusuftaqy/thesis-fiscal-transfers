# ============================================================
# TABEL STATISTIK DESKRIPTIF (STANDALONE)
# - Luas wilayah dalam LEVEL (km2), bukan log
# - Format angka baku Indonesia: desimal = koma, ribuan = titik
# - Output: Word (.docx)
# ============================================================

# ── 0. PACKAGE ──────────────────────────────────────────────
pkg <- c("tidyverse", "readxl", "flextable", "officer")
miss <- pkg[!pkg %in% rownames(installed.packages())]
if (length(miss) > 0) install.packages(miss)
invisible(lapply(pkg, library, character.only = TRUE))

# ── 1. PATH ─────────────────────────────────────────────────
data_dir   <- "data"
output_dir <- "output"
input_path  <- file.path(data_dir, "Full_Variable_Clean.xlsx")
input_sheet <- "Data"
output_docx <- file.path(output_dir, "Tabel_Statistik_Deskriptif_FINAL.docx")

# ── 2. LOAD DATA ────────────────────────────────────────────
df <- read_excel(input_path, sheet = input_sheet)

# ── 3. SIAPKAN VARIABEL LOG (bila belum ada di data) ────────
# Luas dipakai dalam level (km2), tidak di-log.
safe_log <- function(x) {
  x <- suppressWarnings(as.numeric(x))
  ifelse(is.na(x) | x <= 0, NA_real_, log(x))
}

# DAK turunan: buat versi ln_ dari level bila belum tersedia.
ln_map <- list(
  ln_dak_total_pc    = "dak_total_pc",
  ln_dak_jalan_pc    = "dak_jalan_pc",
  ln_dak_pasar_pc    = "dak_pasar_pc",
  ln_dak_airminum_pc = "dak_airminum_pc",
  ln_dak_sanitasi_pc = "dak_sanitasi_pc",
  ln_dak_irigasi_pc  = "dak_irigasi_pc"
)
for (ln_name in names(ln_map)) {
  lvl <- ln_map[[ln_name]]
  if (!ln_name %in% names(df) && lvl %in% names(df)) {
    df[[ln_name]] <- safe_log(df[[lvl]])
  }
}

# Cari nama kolom LUAS (level) dari beberapa kandidat.
luas_cands <- c("luas", "luas_wilayah", "luas_km2", "area", "luas_total")
luas_col <- luas_cands[luas_cands %in% names(df)][1]
if (is.na(luas_col)) {
  warning("Kolom luas tidak ditemukan. Cek nama kolom: ",
          paste(grep("luas|area", names(df), value = TRUE, ignore.case = TRUE), collapse = ", "))
} else {
  df[["luas_level"]] <- suppressWarnings(as.numeric(df[[luas_col]]))
  message("Kolom luas (level) yang dipakai: ", luas_col)
}

# Total Sekolah = SD + SMP + SMA (komponen terpisah di data).
sekolah_komp <- c("L_schl_prm", "L_schl_jrsec", "L_schl_srsec")
sekolah_ada  <- sekolah_komp[sekolah_komp %in% names(df)]
if (length(sekolah_ada) > 0) {
  mat <- do.call(cbind, lapply(sekolah_ada, function(c) suppressWarnings(as.numeric(df[[c]]))))
  df[["total_sekolah"]] <- rowSums(mat, na.rm = TRUE)
  all_na <- rowSums(!is.na(mat)) == 0
  df[["total_sekolah"]][all_na] <- NA_real_
  message("Total Sekolah dibuat dari: ", paste(sekolah_ada, collapse = " + "))
} else {
  warning("Komponen sekolah tidak ditemukan.")
}

# Total Faskes = RS + Puskesmas (komponen terpisah di data).
faskes_komp <- c("L_hosp", "L_puskesmas")
faskes_ada  <- faskes_komp[faskes_komp %in% names(df)]
if (length(faskes_ada) > 0) {
  mat <- do.call(cbind, lapply(faskes_ada, function(c) suppressWarnings(as.numeric(df[[c]]))))
  df[["total_faskes"]] <- rowSums(mat, na.rm = TRUE)
  all_na <- rowSums(!is.na(mat)) == 0
  df[["total_faskes"]][all_na] <- NA_real_
  message("Total Faskes dibuat dari: ", paste(faskes_ada, collapse = " + "))
} else {
  warning("Komponen faskes tidak ditemukan.")
}

# ── 4. DAFTAR VARIABEL UNTUK TABEL ──────────────────────────
# Semua variabel moneter/jumlah ditampilkan dalam LEVEL asli (bukan log),
# agar mean dan SD informatif (skala rupiah / jiwa yang sebenarnya).
var_list <- tribble(
  ~var,                  ~label,
  "dak_total_pc",        "DAK Fisik Penugasan per Kapita (Rp)",
  "dak_jalan_pc",        "DAK Fisik Penugasan Bidang Jalan per Kapita (Rp)",
  "dak_pasar_pc",        "DAK Fisik Penugasan Bidang Pasar per Kapita (Rp)",
  "dak_airminum_pc",     "DAK Fisik Penugasan Bidang Air Minum per Kapita (Rp)",
  "dak_sanitasi_pc",     "DAK Fisik Penugasan Bidang Sanitasi per Kapita (Rp)",
  "dak_irigasi_pc",      "DAK Fisik Penugasan Bidang Irigasi per Kapita (Rp)",
  "voteshare_jkw14",     "Vote Share Jokowi 2014 (%)",
  "swing",               "Dummy Swing District",
  "fiscap_pc",           "Kapasitas Fiskal per Kapita (Rp)",
  "pdrb_pc",             "PDRB per Kapita (Rp)",
  "L_hdi",               "Indeks Pembangunan Manusia",
  "L_airminum_akses",    "Akses Air Minum (%)",
  "L_sanitasi_akses",    "Akses Sanitasi (%)",
  "total_sekolah",       "Total Sekolah (SD+SMP+SMA)",
  "total_faskes",        "Total Fasilitas Kesehatan (RS+Puskesmas)",
  "L_jalan_aspal",       "Jalan Beraspal (%)",
  "L_jalan_kerikil",     "Jalan Kerikil (%)",
  "populasi",            "Populasi (jiwa)",
  "dummy_urban",         "Dummy Kota (Urban)",
  "luas_level",          "Luas Wilayah (km²)"
)

# ── 4b. BUAT KOLOM LEVEL DARI LOG BILA BELUM ADA ────────────
# Jika data hanya menyimpan versi log (ln_*), rekonstruksi level via exp().
# Jika kolom level asli sudah ada, itu yang dipakai (lebih akurat).
level_from_log <- list(
  dak_total_pc    = "ln_dak_total_pc",
  dak_jalan_pc    = "ln_dak_jalan_pc",
  dak_pasar_pc    = "ln_dak_pasar_pc",
  dak_airminum_pc = "ln_dak_airminum_pc",
  dak_sanitasi_pc = "ln_dak_sanitasi_pc",
  dak_irigasi_pc  = "ln_dak_irigasi_pc",
  fiscap_pc       = "ln_fiscap_pc",
  pdrb_pc         = "ln_pdrb_pc",
  populasi        = "ln_populasi"
)
for (lvl in names(level_from_log)) {
  ln_src <- level_from_log[[lvl]]
  if (!lvl %in% names(df) && ln_src %in% names(df)) {
    df[[lvl]] <- exp(suppressWarnings(as.numeric(df[[ln_src]])))
    message("Level direkonstruksi dari log: ", lvl, " <- exp(", ln_src, ")")
  }
}

# Penanganan nama alternatif untuk beberapa kolom yang sering beda.
# (mis. vote share, akses, jalan). Cari kandidat bila nama utama tak ada.
alt_map <- list(
  voteshare_jkw14  = c("voteshare_jkw14", "vote_share", "voteshare", "vs_jkw14"),
  swing            = c("swing", "dummy_swing", "swing_district"),
  L_jalan_aspal    = c("L_jalan_aspal", "L_jalan_beraspal", "jalan_beraspal", "jalan_aspal", "L_road_paved", "road_paved"),
  L_jalan_kerikil  = c("L_jalan_kerikil", "jalan_kerikil", "L_road_gravel", "road_gravel"),
  L_airminum_akses = c("L_airminum_akses", "airminum_akses", "akses_airminum"),
  L_sanitasi_akses = c("L_sanitasi_akses", "sanitasi_akses", "akses_sanitasi")
)
for (canon in names(alt_map)) {
  if (!canon %in% names(df)) {
    found <- alt_map[[canon]][alt_map[[canon]] %in% names(df)][1]
    if (!is.na(found)) {
      df[[canon]] <- df[[found]]
      message("Alias: ", canon, " <- ", found)
    }
  }
}

# Lewati variabel yang benar-benar tidak ada agar tabel tetap jadi.
missing_vars <- setdiff(var_list$var, names(df))
if (length(missing_vars) > 0) {
  warning("Variabel dilewati (tidak ada di data): ", paste(missing_vars, collapse = ", "))
  var_list <- var_list %>% filter(var %in% names(df))
}

# ── 5. FORMAT ANGKA BAKU INDONESIA ──────────────────────────
# desimal = koma, ribuan = titik.
fmt_id <- function(x, digits = 2) {
  if (is.na(x)) return("")
  formatC(
    round(x, digits),
    format = "f",
    digits = digits,
    big.mark = ".",
    decimal.mark = ","
  )
}
fmt_id_int <- function(x) {
  if (is.na(x)) return("")
  formatC(
    as.integer(round(x, 0)),
    format = "d",
    big.mark = ".",
    decimal.mark = ","
  )
}

# ── 6. HITUNG STATISTIK ─────────────────────────────────────
# Variabel yang datanya hanya valid untuk 2019 dihitung HANYA dari baris 2019,
# agar N tidak menggelembung akibat nilai yang sama disalin ke 2017-2018.
vars_2019_only <- c("luas_level", "total_sekolah", "total_faskes",
                    "L_jalan_aspal", "L_jalan_kerikil")

has_year <- "year" %in% names(df)
if (!has_year) {
  warning("Kolom 'year' tidak ditemukan; variabel 2019-only dihitung dari seluruh baris.")
}

rows_out <- lapply(seq_len(nrow(var_list)), function(i) {
  v   <- var_list$var[i]
  lab <- var_list$label[i]

  if (v %in% vars_2019_only && has_year) {
    x <- suppressWarnings(as.numeric(df[[v]][df$year == 2019]))
  } else {
    x <- suppressWarnings(as.numeric(df[[v]]))
  }
  x_ok <- x[!is.na(x)]

  # Variabel berskala besar (rupiah / jiwa) dibulatkan tanpa desimal agar rapi;
  # variabel kecil (persen, indeks, dummy) tetap 2 desimal.
  big_scale <- c("dak_total_pc","dak_jalan_pc","dak_pasar_pc","dak_airminum_pc",
                 "dak_sanitasi_pc","dak_irigasi_pc","fiscap_pc","pdrb_pc",
                 "populasi","luas_level","total_sekolah","total_faskes")
  d_use <- if (v %in% big_scale) 0 else 2

  data.frame(
    Variabel     = lab,
    N            = fmt_id_int(length(x_ok)),
    Mean         = fmt_id(mean(x_ok), d_use),
    `Std. Dev.`  = fmt_id(sd(x_ok), d_use),
    `Min.`       = fmt_id(min(x_ok), d_use),
    `Maks.`      = fmt_id(max(x_ok), d_use),
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
})
desc_stats <- do.call(rbind, rows_out)

print(desc_stats)

# ── 7. FLEXTABLE + DOCX ─────────────────────────────────────
ft_font   <- "Times New Roman"
ft_border <- fp_border(color = "black", width = 1)
note_fp   <- fp_text(font.size = 9, italic = TRUE, color = "#555555", font.family = ft_font)

ft <- flextable(desc_stats)
ft <- fontsize(ft, size = 10, part = "all")
ft <- font(ft, fontname = ft_font, part = "all")
ft <- bold(ft, part = "header")
ft <- align(ft, j = 1, align = "left", part = "all")
ft <- align(ft, j = 2:6, align = "right", part = "all")
ft <- hline_top(ft, border = ft_border, part = "header")
ft <- hline_bottom(ft, border = ft_border, part = "header")
ft <- hline_bottom(ft, border = ft_border, part = "body")
ft <- padding(ft, padding.top = 1.5, padding.bottom = 1.5, part = "all")
ft <- autofit(ft)

note_txt <- paste(
  "Catatan: Variabel infrastruktur pendidikan, kesehatan, dan kondisi jalan",
  "hanya tersedia untuk tahun 2019 (N = 508). Pemisah desimal menggunakan tanda koma",
  "dan pemisah ribuan menggunakan tanda titik sesuai kaidah baku Bahasa Indonesia."
)

doc <- read_docx()
doc <- body_add_par(doc, "Tabel Statistik Deskriptif Variabel Penelitian", style = "heading 1")
doc <- body_add_flextable(doc, ft)
doc <- body_add_fpar(doc, fpar(ftext(note_txt, prop = note_fp)))

if (file.exists(output_docx)) file.remove(output_docx)
print(doc, target = output_docx)
message("SELESAI. Tersimpan di: ", normalizePath(output_docx, mustWork = FALSE))
