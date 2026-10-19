# ============================================================
# BAR CHART DISTRIBUSI KATEGORI VOTE SHARE (X1-X6)
# Jumlah kabupaten/kota per kategori dukungan, dengan angka + persentase.
# Mempertahankan urutan kategori (rendah -> tinggi). Untuk sub-bab 4.2/4.3.
# ============================================================

# -- 0. PACKAGE --------------------------------------------------
pkg <- c("tidyverse", "readxl")
miss <- pkg[!pkg %in% rownames(installed.packages())]
if (length(miss) > 0) install.packages(miss)
invisible(lapply(pkg, library, character.only = TRUE))

# -- 1. PATH (relatif terhadap root repo) ------------------------
data_dir   <- "data"
output_dir <- "output"
input_path  <- file.path(data_dir, "Full_Variable_Clean.xlsx")
input_sheet <- "Data"
output_png <- file.path(output_dir, "barchart_kategori_voteshare.png")

# -- 2. LOAD -----------------------------------------------------
df <- read_excel(input_path, sheet = input_sheet)
vote_col <- "voteshare_jkw14"
if (!vote_col %in% names(df)) stop("Kolom voteshare_jkw14 tidak ditemukan.", call. = FALSE)

# Ambil satu observasi per daerah (vote share 2014 konstan antar tahun).
# Kalau ada kolom tahun, pakai 2017 saja agar tiap daerah dihitung sekali.
if ("year" %in% names(df)) {
  df1 <- df %>% filter(year == min(year, na.rm = TRUE))
} else {
  df1 <- df
}

vs <- suppressWarnings(as.numeric(df1[[vote_col]]))
vs <- vs[!is.na(vs)]

# -- 3. KATEGORISASI (X1-X6) ------------------------------------
kategori <- cut(vs,
  breaks = c(-Inf, 30, 40, 50, 60, 70, Inf),
  labels = c("0-30%", "30-40%", "40-50%",
             "50-60%", "60-70%", ">70%"),
  right = FALSE)

tab <- as.data.frame(table(kategori))
names(tab) <- c("kategori", "jumlah")
tab$persen <- round(100 * tab$jumlah / sum(tab$jumlah), 1)
tab$label  <- paste0(tab$jumlah, "\n(", formatC(tab$persen, format="f", digits=1, decimal.mark=","), "%)")
print(tab)

# -- 4. BAR CHART -----------------------------------------------
# Core (>70%) diberi warna berbeda agar menonjol; lainnya seragam.
tab$is_core <- tab$kategori == ">70%"

p <- ggplot(tab, aes(x = kategori, y = jumlah, fill = is_core)) +
  geom_col(width = 0.72) +
  geom_text(aes(label = label), vjust = -0.35, size = 4.2, lineheight = 0.9, color = "gray20", fontface = "bold") +
  scale_fill_manual(values = c("FALSE" = "#8AB4D8", "TRUE" = "#E8820C")) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.20))) +
  labs(
    title = "Distribusi Kabupaten/Kota menurut Kategori Vote Share Jokowi 2014",
    x = "Kategori Dukungan Elektoral",
    y = "Jumlah Kabupaten/Kota"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title   = element_text(face = "bold", size = 12, margin = margin(b = 8)),
    axis.title.x = element_text(margin = margin(t = 8)),
    axis.title.y = element_text(margin = margin(r = 8)),
    panel.grid.major.x = element_blank(),
    panel.grid.minor   = element_blank(),
    legend.position = "none"
  )

print(p)

ggsave(output_png, plot = p, width = 22, height = 12, units = "cm", dpi = 300)
message("SELESAI. Bar chart tersimpan di: ", normalizePath(output_png, mustWork = FALSE))
