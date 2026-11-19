# TECH6 Term Paper: Replication of Meyersson (2014)

Replication and extension of Meyersson, E. (2014), "Islamic Rule and the
Empowerment of the Poor and Pious", _Econometrica_ 82(1), 229–269.

## How to run

From the main folder of the repository:

```bash
Rscript run_all.R
```

Or in RStudio: open `TECH6---Term-Paper.Rproj`, then run `source("run_all.R")` in the Console.

This regenerates every table and figure from `regdata0.dta` and saves them in `results/`.

## Structure

- `regdata0.dta`: the replication data
- `run_all.R`: runs every script in `code/` in order
- `code/`: one script per table, figure and extension; `helpers.R` holds shared functions for Figures 3 and 4
- `data/variables.R`: panel labels and controls used by Figures 3–4 and Extension 2
- `results/`: all tables (.csv and .docx) and figures (.png)

## Outputs

| Script                                         | Output in `results/`                                          |
| ---------------------------------------------- | ------------------------------------------------------------- |
| `table1.R`, `table2.R`, `table3.R`, `table7.R` | `tableX.csv`, `tableX_formatted.docx`                         |
| `figure1.R`                                    | `fig1.png`                                                    |
| `figure2.R`                                    | `fig2a.png`, `fig2b.png`                                      |
| `figure3.R`                                    | `figure3_covariates.png`                                      |
| `figure4.R`                                    | `figure4_outcomes.png`                                        |
| `ext_1.R`                                      | `ext1.csv`, `ext1_formatted.docx` (Table E.1)                 |
| `ext_2.R`                                      | `ext2_results.csv`, `ext2_change.png` (Table E.2, Figure E.2) |

## Package dependencies

- `tidyverse`
- `haven`
- `fixest`
- `flextable`
- `patchwork`
- `rddensity`
- `rdd`

## Installation

```r
install.packages(c("tidyverse", "haven", "fixest", "flextable", "patchwork", "rddensity"))
```

`rdd` is no longer on CRAN and is installed from the CRAN archive:

```r
install.packages(c("sandwich", "lmtest", "AER", "Formula"))
install.packages("https://cran.r-project.org/src/contrib/Archive/rdd/rdd_0.57.tar.gz",
                 repos = NULL, type = "source")
```

## Data and sources

- **Data:** `regdata0.dta`, the replication dataset shared on Canvas
- **Paper:** Meyersson, E. (2014), _Econometrica_ 82(1), 229–269. https://doi.org/10.3982/ECTA9878
- **Supplemental Material:** https://www.econometricsociety.org/publications/econometrica/2014/01/01/islamic-rule-and-empowerment-poor-and-pious/supp/9878_miscellaneous_0.pdf
- **Density test (Figure 2b):** McCrary, J. (2008), _Journal of Econometrics_ 142(2), 698–714. https://doi.org/10.1016/j.jeconom.2007.05.005
