# TECH6 Term Paper: Replication of Meyersson (2014)

Replication and extension of Meyersson, E. (2014), "Islamic Rule and the
Empowerment of the Poor and Pious", _Econometrica_ 82(1), 229–269.

## How to run

From the main folder of the repository:

```bash
Rscript run_all.R
```

This regenerates every table and figure from `regdata0.dta` and saves them in `results/`.

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
