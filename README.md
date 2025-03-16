# Propofol
Analysis of the Impact of Propofol Administration on the Survival Chances of Critically Ill Patients in the ICU Using Piece-Wise Exponential Additive Mixed Models (PAMMs).

To reproduce the results, execute all code chunks in `main.Rmd`. This will generate all relevant plots and models for the project.

The models are available in two formats: summed data (`code\models_data_summed.R`) and manual PED format (`code\models_manualPED.R`).

- **Summed Data Models**: These can be found directly in the `models` folder.
  - The naming pattern for these models is `model_[event]_[propDays/propCals]_[16kcal/70pct].rds`.
  - `event` refers to either `death` or `disc` (discharged).
  - Each model includes all 13 confounder variables, with either `propDays` or `propCals` and either `16kcal` or `70pct`.
  
- **Manual PED Format Models**: These follow a similar naming pattern, but instead of using `[16kcal/70pct]`, the format is `[calsAbove16kcal/calsAbove70pct]`.
  - These models can be found in the `models\manualPED` folder.

Forest and spline plots can be found in `plots\models`. Descriptive overviews of our dataset, such as bar plots or the Kaplan-Meier curve, are available in `plots\descriptive`.

**Note**: The process may take some time and require significant computational resources.

The session info is provided below:

```r
> devtools::session_info()
─ Session info ─────────────────────────────────────────────────────────────────────────────────────────
 setting  value
 version  R version 4.4.1 (2024-06-14 ucrt)
 os       Windows 11 x64 (build 22631)
 system   x86_64, mingw32
 ui       RStudio
 language (EN)
 collate  German_Germany.utf8
 ctype    German_Germany.utf8
 tz       Europe/Berlin
 date     2025-03-16

─ Packages ─────────────────────────────────────────────────────────────────────────────────────────────
 package     * version date (UTC) lib source
 cachem        1.1.0   2024-05-16 [1] CRAN (R 4.4.1)
 cli           3.6.3   2024-06-21 [1] CRAN (R 4.4.1)
 devtools      2.4.5   2022-10-11 [1] CRAN (R 4.4.3)
 digest        0.6.37  2024-08-19 [1] CRAN (R 4.4.1)
 ellipsis      0.3.2   2021-04-29 [1] CRAN (R 4.4.3)
 evaluate      1.0.1   2024-10-10 [1] CRAN (R 4.4.1)
 fastmap       1.2.0   2024-05-15 [1] CRAN (R 4.4.1)
 fs            1.6.4   2024-04-25 [1] CRAN (R 4.4.1)
 glue          1.8.0   2024-09-30 [1] CRAN (R 4.4.1)
 htmltools     0.5.8.1 2024-04-04 [1] CRAN (R 4.4.1)
 htmlwidgets   1.6.4   2023-12-06 [1] CRAN (R 4.4.2)
 httpuv        1.6.15  2024-03-26 [1] CRAN (R 4.4.2)
 knitr         1.48    2024-07-07 [1] CRAN (R 4.4.1)
 later         1.4.1   2024-11-27 [1] CRAN (R 4.4.2)
 lifecycle     1.0.4   2023-11-07 [1] CRAN (R 4.4.1)
 magrittr      2.0.3   2022-03-30 [1] CRAN (R 4.4.1)
 memoise       2.0.1   2021-11-26 [1] CRAN (R 4.4.1)
 mime          0.12    2021-09-28 [1] CRAN (R 4.4.0)
 miniUI        0.1.1.1 2018-05-18 [1] CRAN (R 4.4.3)
 pkgbuild      1.4.6   2025-01-16 [1] CRAN (R 4.4.1)
 pkgload       1.4.0   2024-06-28 [1] CRAN (R 4.4.2)
 profvis       0.4.0   2024-09-20 [1] CRAN (R 4.4.3)
 promises      1.3.2   2024-11-28 [1] CRAN (R 4.4.2)
 purrr         1.0.2   2023-08-10 [1] CRAN (R 4.4.1)
 R6            2.5.1   2021-08-19 [1] CRAN (R 4.4.1)
 Rcpp          1.0.13  2024-07-17 [1] CRAN (R 4.4.1)
 remotes       2.5.0   2024-03-17 [1] CRAN (R 4.4.2)
 rlang         1.1.4   2024-06-04 [1] CRAN (R 4.4.1)
 rmarkdown     2.28    2024-08-17 [1] CRAN (R 4.4.1)
 rstudioapi    0.17.1  2024-10-22 [1] CRAN (R 4.4.1)
 sessioninfo   1.2.3   2025-02-05 [1] CRAN (R 4.4.3)
 shiny         1.10.0  2024-12-14 [1] CRAN (R 4.4.2)
 urlchecker    1.0.1   2021-11-30 [1] CRAN (R 4.4.3)
 usethis       3.1.0   2024-11-26 [1] CRAN (R 4.4.3)
 vctrs         0.6.5   2023-12-01 [1] CRAN (R 4.4.1)
 xfun          0.48    2024-10-03 [1] CRAN (R 4.4.1)
 xtable        1.8-4   2019-04-21 [1] CRAN (R 4.4.2)
 yaml          2.3.10  2024-07-26 [1] CRAN (R 4.4.1)
 ```