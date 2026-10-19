# Political Determinants of Fiscal Transfers in Indonesia

Undergraduate thesis (skripsi) analyzing how the **2014 presidential vote share** relates to the subsequent allocation of **Dana Alokasi Khusus (DAK / Special Allocation Fund)** to Indonesian districts (2017–2019). Empirical analysis in R.

> **Status:** Undergraduate thesis, Universitas Diponegoro · Language: **R**

---

## Research question

Do political factors — specifically, a district's level of support for the incumbent in the 2014 presidential election — influence how much central-government DAK that district subsequently receives, after controlling for fiscal, economic, and demographic characteristics?

The analysis tests three competing political-allocation logics:
- **Core voter** — incumbents reward loyal (high-support) districts.
- **Swing voter** — incumbents target competitive (close-margin) districts.
- **Political investment** — allocation tracks support in a graded way.

## Method

- **Model:** OLS per year (2017, 2018, 2019), with **province fixed effects** and **robust standard errors (HC1)**.
- **Dependent variables:** DAK per capita across six fields — total, roads (jalan), markets (pasar), drinking water (air minum), sanitation (sanitasi), irrigation (irigasi) — in `ln(DAK + 1)` form so that zero-allocation districts remain in the estimation as substantive information.
- **Key regressor:** 2014 Jokowi vote share (continuous), plus categorical core/swing specifications.
- **Controls (baseline, all years):** fiscal capacity, GRDP per capita, HDI, population, urban dummy, sanitation & water access.
- **Extended controls (2019 only):** education & health infrastructure, road condition — used in a separate robustness table.

## Repository structure

```
thesis-fiscal-transfers/
├── scripts/
│   ├── 01_descriptive_stats.R          # Descriptive statistics table
│   ├── 02_barchart_voteshare.R         # Vote-share category bar chart
│   ├── 03_main_estimation.R            # Main OLS estimation (4 tables)
│   ├── 04_appendix_full_controls.R     # Appendix: full baseline controls, 3 years
│   └── 05_robustness_extended_2019.R   # Robustness: extended controls, 2019
├── data/                               # (not included — see Data note below)
├── output/                             # Generated tables & figures
└── README.md
```

## Data note

**The dataset is not included in this repository.** The analysis uses a district-level panel (`data/Full_Variable_Clean.xlsx`, sheet `Data`) combining:

- **DAK allocation** — Kementerian Keuangan / DJPK (public).
- **District controls** — BPS (fiscal capacity, GRDP, HDI, population, access indicators).
- **2014 presidential vote share** — obtained from a restricted source and **not redistributable**; it is therefore excluded here.

To reproduce, supply a `Full_Variable_Clean.xlsx` with the expected columns (see `docs/variables.md`) in the `data/` folder and run the scripts in order.

## How to run

```r
# Requirements: R >= 4.0; packages: tidyverse, readxl, sandwich, lmtest,
# openxlsx, flextable, officer
# Run from the repository root, in order (01 → 05).
source("scripts/03_main_estimation.R")   # writes tables to output/
```

## Author

**Yusuf Taqy Mubarak** — Economics graduate, Universitas Diponegoro
[LinkedIn](https://www.linkedin.com/in/yusuftaqy/) · yusuftaqy12@gmail.com
