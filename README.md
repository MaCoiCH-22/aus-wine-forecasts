# Australian Wine Sales: Forecast Model Explorer

Shiny app for ADS 506 (Applied Time Series Analytics), Week 5. It compares TSLM, ETS, and ARIMA
forecasts of monthly Australian wine sales by varietal (1980 to 1994, thousands of litres).

**Live app:** https://01a20708-748f-f3f1-9f6e-5121559eccd4.share.connect.posit.cloud/

**Code:** https://github.com/MaCoiCH-22/aus-wine-forecasts

## Run it locally

From R, with the working directory set to this `app/` folder:

```r
shiny::runApp()
```

Packages: shiny, bslib, fpp3 (tsibble, fable, feasts, fabletools), gt, urca. Exact versions are
pinned in `renv.lock` (restore with `renv::restore()`). The data file is read with the project-relative path
`data/AustralianWines.csv`, so no file paths need changing.

## What the app does

| Tab | What it shows |
|---|---|
| Overview | Time plot (dashed line marks the end of training), seasonal plot, or STL decomposition of the selected varietals and dates |
| Forecasts | Validation-window forecasts against the held-out actuals, or future forecasts after refitting on all selected data, with 80% or 95% prediction intervals; side-by-side grid (varietal by model) or overlay |
| Model specs | ETS form and ARIMA orders chosen automatically, a plain-language reading of each, key parameters, AICc, and the spec chosen when refit on all data |
| Accuracy | RMSE, MAE, MAPE, MASE for the training and validation windows side by side, sorted by validation RMSE, best model in bold, CSV download |
| About | Data source, method, and how to reproduce a figure |

Extras beyond the minimum: a seasonal naive benchmark, an equal-weight ensemble, an optional log
transform (back-transformed automatically, so metrics stay on the original scale), tooltips on every
control, and input debouncing so sliders do not trigger a refit on every movement.

## Method notes

* The data is a monthly `tsibble` with `index = Month`, `key = Varietal`.
* Rose has `*` for July and August 1994. These are read as missing and filled with the last observed
  value (the same handling used in Assignments 2.1 to 4.1) because ETS cannot fit a series with gaps.
* The last *N* months of the chosen date range are the validation window; everything before is training.
  The app refuses to fit if training is shorter than 3 years.
* `ETS()` and `ARIMA()` are called with no specification, so fable picks the form and orders by AICc.
  `TSLM()` uses `trend() + season()`.

## Deployment

Hosted on Posit Connect Cloud, published from this GitHub repo (`main` branch, `app.R`).
Every push to `main` republishes automatically. `manifest.json` lists the R version and packages
the server installs; regenerate it after adding a package with
`rsconnect::writeManifest(".", appFiles = c("app.R", "data/AustralianWines.csv", "README.md"))`.

## AI use and sources

The first draft of `app.R` was written with Claude Code (Claude Opus 5.5, Anthropic) from the ADS 506
Module 5 Suggested Workflow and then reviewed, tested, and edited by me. The modeling approach follows
Hyndman, R. J., & Athanasopoulos, G. (2021). *Forecasting: Principles and practice* (3rd ed.). OTexts.
https://otexts.com/fpp3/
