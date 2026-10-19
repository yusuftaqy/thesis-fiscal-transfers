# Variable dictionary — Full_Variable_Clean.xlsx (sheet: "Data")

Panel unit: district-year (`id_daerah` × `year`). One row per district per year (2017–2019).

## Identifiers
| Column | Description |
|---|---|
| `id_daerah` | District code (first 2 digits = province; used for province fixed effects) |
| `year` | Year (2017, 2018, 2019) |

## Dependent variables (DAK per capita, billion Rp)
| Column | Field |
|---|---|
| `dak_total_pc` | Total DAK per capita |
| `dak_jalan_pc` | Roads |
| `dak_pasar_pc` | Markets |
| `dak_airminum_pc` | Drinking water |
| `dak_sanitasi_pc` | Sanitation |
| `dak_irigasi_pc` | Irrigation |

Scripts create `ln_<var>` = `log(var + 1)` so zero-allocation districts stay in the estimation.

## Political variables
| Column | Description |
|---|---|
| `voteshare_jkw14` | 2014 presidential vote share (incumbent), 0–100 (RESTRICTED — not distributed) |
| `x1`–`x6` | Vote-share support-level categories (dummies) |
| `swing` | Competitive-district dummy (margin ≤ 5%) |

## Baseline controls (all years)
| Column | Description |
|---|---|
| `ln_fiscap_pc` | Fiscal capacity per capita (ln) |
| `ln_pdrb_pc` | GRDP per capita (ln) |
| `L_hdi` | Human Development Index (lag) |
| `ln_populasi` | Population (ln) |
| `dummy_urban` | Urban district dummy |
| `L_sanitasi_akses` | Sanitation access (lag) |
| `L_airminum_akses` | Drinking-water access (lag) |

## Extended controls (2019 only, robustness)
| Column | Description |
|---|---|
| `L_schl_prm`, `L_schl_jrsec`, `L_schl_srsec` | Schools by level (→ `total_sekolah`) |
| health-facility columns | → `total_faskes` |
| `luas` | District area (km², used in level, not log) |
