# ADS 506 Week 5: Australian Wine Forecast Explorer (Shiny)
# Author: Macoi Hollins
# AI-assisted (Claude Code, Claude Opus 5.5): first draft of this app written by
# Claude Code from the course's M5 Suggested Workflow; reviewed and edited by Macoi.
# See README.md for the full disclosure and the explanation of each section.

library(shiny)
library(bslib)
library(fpp3)   # tsibble, fable, feasts, fabletools, dplyr, tidyr, ggplot2, lubridate
library(gt)

# ---- 1. Load data once at startup -------------------------------------------
# Project-relative path: Shiny runs app.R with the app folder as the working
# directory, so "data/..." works locally and on shinyapps.io.
# Rose has "*" for Jul and Aug 1994: read "*" as NA, then carry the last value
# forward (same handling as Assignments 2.1 to 4.1) because ETS cannot fit with gaps.
wine_raw <- readr::read_csv("data/AustralianWines.csv", na = c("", "*"),
                            show_col_types = FALSE)

wine <- wine_raw |>
  rename_with(\(x) tools::toTitleCase(trimws(x))) |>          # "sparkling " -> "Sparkling"
  fill(Rose, .direction = "down") |>
  mutate(Month = yearmonth(lubridate::my(Month))) |>          # "Jan-80" -> 1980 Jan
  pivot_longer(-Month, names_to = "Varietal", values_to = "Sales") |>
  as_tsibble(index = Month, key = Varietal)                   # monthly tsibble keyed by Varietal

varietals  <- sort(unique(wine$Varietal))
date_min   <- as.Date(min(wine$Month))
date_max   <- as.Date(max(wine$Month))
model_cols <- c(TSLM = "#1b9e77", ETS = "#d95f02", ARIMA = "#7570b3",
                SNAIVE = "#7f7f7f", Ensemble = "#e7298a")

# Small helper: a label with an info icon that shows a tooltip on hover.
tip <- function(label, text) {
  tooltip(span(label, icon("circle-info")), text, placement = "right")
}

# ---- 2. Model-spec helpers ---------------------------------------------------
# Turn a fitted model into a plain-language description a non-expert can read.
describe_spec <- function(spec) {
  word <- c(A = "additive", M = "multiplicative", N = "none", Ad = "damped additive")
  if (grepl("^ETS", spec)) {
    p <- regmatches(spec, regexpr("\\(.*\\)", spec))
    p <- strsplit(gsub("[()]", "", p), ",")[[1]]
    sprintf("Error %s, trend %s, seasonality %s", word[p[1]], word[p[2]], word[p[3]])
  } else if (grepl("^ARIMA", spec)) {
    n <- as.integer(regmatches(spec, gregexpr("[0-9]+", spec))[[1]])
    txt <- sprintf("%d AR, %d differences, %d MA; seasonal %d AR, %d differences, %d MA (period %d)",
                   n[1], n[2], n[3], n[4], n[5], n[6], n[7])
    if (grepl("drift", spec)) txt <- paste(txt, "+ drift")
    txt
  } else if (grepl("^TSLM", spec)) {
    "Linear trend + 11 monthly dummies"
  } else if (grepl("^SNAIVE", spec)) {
    "Repeats the value from the same month last year"
  } else if (grepl("COMBINATION", spec)) {
    "Equal-weight average of the selected models"
  } else spec
}

# Key estimated parameters (ETS smoothing weights, ARIMA coefficients).
param_text <- function(mbl) {
  keep <- intersect(c("ETS", "ARIMA"), names(mbl))
  if (length(keep) == 0) return(tibble(Varietal = character(), .model = character(), Parameters = character()))
  tidy(select(mbl, Varietal, all_of(keep))) |>
    filter(!grepl("^(s|l|b)\\[", term)) |>             # drop initial states
    group_by(Varietal, .model) |>
    summarise(Parameters = paste(sprintf("%s = %.3g", term, estimate), collapse = ", "),
              .groups = "drop")
}

# ---- 3. UI -------------------------------------------------------------------
ui <- page_sidebar(
  title = "Australian Wine Sales: Forecast Model Explorer",
  theme = bs_theme(bootswatch = "flatly"),
  fillable = FALSE,   # let plots keep their set height and the page scroll
  sidebar = sidebar(
    width = 320,
    selectizeInput("varietals", tip("Varietals", "Pick one or more wine types. Each is modeled separately."),
                   choices = varietals, selected = c("Red", "Sparkling"), multiple = TRUE),
    sliderInput("dates", tip("Date range", "Only data in this window is used for plots and models."),
                min = date_min, max = date_max, value = c(date_min, date_max),
                timeFormat = "%b %Y", step = 31),
    sliderInput("n_valid", tip("Validation months",
                               "The last N months of the date range are held out. Models are fit on the earlier months and scored on these."),
                min = 6, max = 36, value = 12, step = 6),
    sliderInput("h", tip("Forecast horizon (months)",
                         "How far past the end of the date range to forecast, after refitting on all selected data."),
                min = 6, max = 36, value = 12, step = 6),
    checkboxGroupInput("models", tip("Models", "TSLM, ETS and ARIMA are auto-specified. SNAIVE is a benchmark; Ensemble averages the checked core models."),
                       choices = names(model_cols), selected = c("TSLM", "ETS", "ARIMA", "SNAIVE"),
                       inline = TRUE),
    checkboxInput("log", tip("Log-transform sales",
                             "Fits models to log(Sales) so seasonal swings that grow with the level become constant. Forecasts and metrics are back-transformed to the original scale."),
                  value = FALSE),
    radioButtons("level", "Prediction interval", choices = c("80%" = 80, "95%" = 95),
                 selected = 95, inline = TRUE)
  ),
  navset_card_underline(
    nav_panel("Overview",
      radioButtons("view", NULL, inline = TRUE,
                   choices = c("Time plot", "Seasonal plot", "STL decomposition")),
      plotOutput("overview", height = "520px")),
    nav_panel("Forecasts",
      layout_columns(
        radioButtons("fc_window", NULL, inline = TRUE,
                     choices = c("Validation window (vs. actuals)" = "valid",
                                 "Future (refit on all data)" = "future")),
        radioButtons("fc_layout", NULL, inline = TRUE,
                     choices = c("Side by side" = "grid", "Overlay" = "overlay"))
      ),
      plotOutput("fc_plot", height = "560px")),
    nav_panel("Model specs",
      p("Specifications chosen automatically on the training window. The refit column shows what the same
         procedure picks when it sees all selected data (used for the Future forecasts)."),
      gt_output("spec_table")),
    nav_panel("Accuracy",
      uiOutput("best_text"),
      gt_output("acc_table"),
      downloadButton("dl_acc", "Download metrics (CSV)", class = "btn-sm mt-2")),
    nav_panel("About",
      markdown("
**Data.** Monthly Australian wine sales by varietal (thousands of litres), January 1980 to December 1994,
from the course file `AustralianWines.csv`.

**How it works.** The selected varietals and dates form a monthly `tsibble` keyed by `Varietal`.
The last *N* validation months are held out. On the earlier training months the app fits, per varietal,
`TSLM(Sales ~ trend() + season())`, `ETS(Sales)` and `ARIMA(Sales)` with automatic specification
(fable picks the ETS form and ARIMA orders by AICc), plus a seasonal naive benchmark and an optional
equal-weight ensemble. Training metrics come from one-step fitted values; validation metrics compare the
multi-step forecasts to the held-out actuals. The Future view refits every model on all selected data and
forecasts *h* months ahead.

**Reproducing a figure.** Set the sidebar inputs listed in the figure caption of the write-up. Results are
deterministic for a given set of inputs.

**Credits.** Built with shiny, bslib, fpp3 (fable, feasts, tsibble) and gt, following the ADS 506 Module 5
Suggested Workflow and Hyndman and Athanasopoulos (2021), *Forecasting: Principles and Practice* (3rd ed.).
The first draft of the code was written with help from Claude Code (Anthropic); see the README.
"))
  )
)

# ---- 4. Server ---------------------------------------------------------------
server <- function(input, output, session) {

  # 4a. Filter by varietal and date range. debounce() waits until the user stops
  #     dragging a slider, so models are not refit on every pixel of movement.
  inputs_d <- reactive(list(v = input$varietals, d = input$dates, n = input$n_valid,
                            h = input$h, m = input$models, log = input$log)) |> debounce(600)

  filtered <- reactive({
    x <- inputs_d()
    validate(need(length(x$v) > 0, "Select at least one varietal."))
    wine |>
      filter(Varietal %in% x$v,
             Month >= yearmonth(x$d[1]), Month <= yearmonth(x$d[2]))
  })

  # 4b. Train/validation split: the last n_valid months of the range are validation.
  split <- reactive({
    dat <- filtered()
    n   <- inputs_d()$n
    cut <- max(dat$Month) - n
    train <- filter(dat, Month <= cut)
    validate(need(n_distinct(train$Month) >= 36,
                  "Training window is under 3 years. Widen the date range or use fewer validation months."))
    list(train = train, valid = filter(dat, Month > cut), cut = cut)
  })

  # 4c. Model definitions built from the checkboxes and the log toggle.
  model_defs <- reactive({
    x <- inputs_d()
    validate(need(length(x$m) > 0, "Select at least one model."))
    y <- if (isTRUE(x$log)) quote(log(Sales)) else quote(Sales)
    defs <- list(
      TSLM   = TSLM(!!rlang::new_formula(y, quote(trend() + season()))),
      ETS    = ETS(!!y),
      ARIMA  = ARIMA(!!y),
      SNAIVE = SNAIVE(!!y)
    )
    defs[intersect(names(defs), x$m)]
  })

  # Fit the selected models, then add the ensemble as an average of the core models.
  fit_models <- function(data) {
    mbl  <- model(data, !!!model_defs())
    core <- intersect(c("TSLM", "ETS", "ARIMA"), names(mbl))
    if ("Ensemble" %in% inputs_d()$m && length(core) >= 2) {
      avg <- rlang::parse_expr(sprintf("(%s) / %d", paste(core, collapse = " + "), length(core)))
      mbl <- mutate(mbl, Ensemble = !!avg)
    }
    mbl
  }

  fit_train <- reactive(withProgress(message = "Fitting models on training window...",
                                     fit_models(split()$train)))
  fit_full  <- reactive(withProgress(message = "Refitting on all selected data...",
                                     fit_models(filtered())))

  fc_valid  <- reactive(forecast(fit_train(), h = inputs_d()$n))
  fc_future <- reactive(forecast(fit_full(),  h = inputs_d()$h))

  # 4d. Overview plot ----------------------------------------------------------
  output$overview <- renderPlot({
    dat <- filtered()
    if (input$view == "Time plot") {
      autoplot(dat, Sales) + facet_wrap(~Varietal, scales = "free_y", ncol = 1) +
        geom_vline(xintercept = as.Date(split()$cut), linetype = "dashed") +
        labs(y = "Sales (thousand litres)", x = NULL,
             title = "Monthly sales (dashed line = end of training window)") +
        theme(legend.position = "none")
    } else if (input$view == "Seasonal plot") {
      gg_season(dat, Sales, labels = "right") +
        labs(y = "Sales (thousand litres)", x = NULL, title = "Seasonal plot by year")
    } else {
      dat |> model(STL(Sales ~ season(window = 13))) |> components() |> autoplot() +
        labs(title = "STL decomposition", x = NULL)
    }
  }, res = 96)

  # 4e. Forecast plot with prediction intervals ---------------------------------
  output$fc_plot <- renderPlot({
    lvl <- as.numeric(input$level)
    if (input$fc_window == "valid") {
      fc <- fc_valid(); hist <- split()$train; act <- split()$valid
      show_from <- max(hist$Month) - 48                     # last 4 training years for context
      ttl <- sprintf("Validation forecasts vs. actuals (black dashed). %s%% prediction intervals", lvl)
    } else {
      fc <- fc_future(); hist <- filtered(); act <- NULL
      show_from <- max(hist$Month) - 48
      ttl <- sprintf("%d-month forecasts after refitting on all selected data. %s%% prediction intervals",
                     inputs_d()$h, lvl)
    }
    p <- autoplot(fc, level = lvl, alpha = 0.6) +
      autolayer(filter(hist, Month >= show_from), Sales, colour = "grey45")
    # Actuals drawn dashed and thin so the coloured forecast line underneath stays visible.
    if (!is.null(act)) p <- p + autolayer(act, Sales, colour = "black", linetype = "dashed", linewidth = 0.5)
    p <- p + if (input$fc_layout == "grid") facet_grid(Varietal ~ .model, scales = "free_y")
             else facet_wrap(~Varietal, scales = "free_y", ncol = 1)
    p + scale_colour_manual(values = model_cols, name = "Model") +
      scale_fill_manual(values = model_cols, name = "Model") +
      labs(y = "Sales (thousand litres)", x = NULL, title = ttl) +
      theme(legend.position = "bottom", strip.text = element_text(face = "bold"))
  }, res = 96)

  # 4f. Model-spec table ---------------------------------------------------------
  spec_df <- function(mbl, col) {
    mbl |> pivot_longer(-Varietal, names_to = ".model", values_to = "fit") |>
      as_tibble() |>
      mutate(!!col := vapply(fit, \(f) format(f), character(1))) |>
      select(Varietal, .model, all_of(col))
  }
  output$spec_table <- render_gt({
    tr <- spec_df(fit_train(), "Spec") |>
      mutate(Spec = gsub("^<|>$", "", Spec),
             `Plain-language reading` = vapply(Spec, describe_spec, character(1)))
    fu <- spec_df(fit_full(), "Refit spec") |> mutate(`Refit spec` = gsub("^<|>$", "", `Refit spec`))
    ic <- glance(select(fit_train(), -any_of("Ensemble"))) |> select(Varietal, .model, any_of("AICc"))
    tr |> left_join(fu, by = c("Varietal", ".model")) |>
      left_join(param_text(fit_train()), by = c("Varietal", ".model")) |>
      left_join(ic, by = c("Varietal", ".model")) |>
      mutate(Parameters = coalesce(Parameters, "")) |>
      rename(Model = .model) |>
      gt(groupname_col = "Varietal") |>
      fmt_number(any_of("AICc"), decimals = 1) |>
      tab_source_note("AICc compares models of the same type only (not ETS vs. ARIMA vs. TSLM); use the Accuracy tab to compare across types.") |>
      sub_missing(missing_text = "") |>
      cols_width(Parameters ~ px(260)) |>
      tab_options(table.font.size = px(13), row_group.font.weight = "bold")
  })

  # 4g. Accuracy table: training (fitted values) vs validation (forecasts) -------
  acc_long <- reactive({
    bind_rows(
      accuracy(fit_train()) |> mutate(Window = "Training"),
      accuracy(fc_valid(), filtered()) |> mutate(Window = "Validation")
    ) |>
      select(Varietal, Model = .model, Window, RMSE, MAE, MAPE, MASE)
  })

  output$acc_table <- render_gt({
    acc_long() |>
      pivot_wider(names_from = Window, values_from = c(RMSE, MAE, MAPE, MASE), names_glue = "{Window}_{.value}") |>
      group_by(Varietal) |> arrange(Validation_RMSE, .by_group = TRUE) |>
      gt() |>
      tab_spanner("Training window", starts_with("Training_")) |>
      tab_spanner("Validation window", starts_with("Validation_")) |>
      cols_label_with(fn = \(x) sub("^(Training|Validation)_", "", x)) |>
      fmt_number(ends_with(c("RMSE", "MAE")), decimals = 0) |>
      fmt_number(ends_with(c("MAPE", "MASE")), decimals = 2) |>
      tab_style(cell_text(weight = "bold"),
                cells_body(rows = Validation_RMSE == ave(Validation_RMSE, Varietal, FUN = min))) |>
      tab_source_note("Sorted by validation RMSE within each varietal; bold = lowest. MAPE in percent.") |>
      tab_options(table.font.size = px(13), row_group.font.weight = "bold")
  })

  output$best_text <- renderUI({
    best <- acc_long() |> filter(Window == "Validation") |>
      group_by(Varietal) |> slice_min(RMSE, n = 1, with_ties = FALSE) |> ungroup()
    tags$p(tags$strong("Lowest validation RMSE: "),
           paste(sprintf("%s: %s (RMSE %s)", best$Varietal, best$Model,
                         format(round(best$RMSE), big.mark = ",")), collapse = "; "))
  })

  output$dl_acc <- downloadHandler(
    filename = function() "wine_model_accuracy.csv",
    content  = function(file) write.csv(acc_long(), file, row.names = FALSE)
  )
}

shinyApp(ui, server)
