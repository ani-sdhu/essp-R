# =============================================================================
# ESSP — exercise every exported command
#
# Runs all 65 commands, assigns each result to a named variable, and prints a
# pass/fail line with timing and a shape summary. Every file it writes goes
# under _essp_test/ and nowhere else.
#
# Run:  source("essp/docs/test_all_commands.R")
# Then: inspect any variable by name, e.g.  View(a_mix)  or  str(g_generation)
#
# Variable naming
#   g_*  gathering        a_*  analysis        v_*  visual / chart
# =============================================================================

library(ESSP)

OUT <- "_essp_test"
dir.create(file.path(OUT, "data"),    recursive = TRUE, showWarnings = FALSE)
dir.create(file.path(OUT, "figures"), recursive = TRUE, showWarnings = FALSE)

# ---- harness ----------------------------------------------------------------
.results <- list()

chk <- function(label, expr) {
  t0 <- Sys.time()
  val <- tryCatch(suppressWarnings(expr),
                  error = function(e) structure(conditionMessage(e), class = "essp_error"))
  secs <- as.numeric(difftime(Sys.time(), t0, units = "secs"))

  failed <- inherits(val, "essp_error")
  shape <- if (failed) substr(val, 1, 46)
    else if (inherits(val, "ggplot")) "ggplot object"
    else if (is.data.frame(val)) sprintf("%d x %d", nrow(val), ncol(val))
    else if (is.list(val)) sprintf("list[%d]", length(val))
    else sprintf("%s[%d]", class(val)[1], length(val))

  .results[[length(.results) + 1L]] <<- data.frame(
    command = label, ok = !failed, secs = round(secs, 2), shape = shape,
    stringsAsFactors = FALSE)
  cat(sprintf("  %-26s %-5s %6.2fs  %s\n", label, if (failed) "FAIL" else "ok", secs, shape))
  invisible(val)
}

hdr <- function(x) cat("\n", strrep("=", 74), "\n  ", x, "\n", strrep("=", 74), "\n", sep = "")

# =============================================================================
hdr("SET 1 — GATHERING")
# =============================================================================

# The friendly front doors first: one verb, plain strings.
g_catalog     <- chk("essp.catalog",     essp.catalog())
g_get_mix     <- chk("essp.get(mix)",    essp.get("mix", "CA,MD,DE", "2024"))
g_get_fuelmix <- chk("essp.get(fuelmix)", essp.get("fuelmix", "CISO", "July 2024"))

g_topics      <- chk("essp.topics",      essp.topics())
g_topic_one   <- chk("essp.topics(one)", essp.topics("hourly"))
g_resolve     <- chk("essp.resolve",     essp.resolve("923"))          # 1 form, 3 routes
g_facet_geo   <- chk("essp.facet",       essp.facet("generation", "geo"))
g_respondents <- chk("essp.respondents", essp.respondents())
g_regions     <- chk("essp.respondents(region)", essp.respondents("region"))

# Snapshots: fetch on first run, read from CSV thereafter.
g_generation <- chk("essp.gather + snapshot", essp.snapshot(
  file.path(OUT, "data", "generation_us_2024.csv"),
  essp.gather("generation", state = "US", years = 2024,
              freq = "annual", sector = "99", verbose = FALSE)))

g_retail <- chk("essp.gather (retail)", essp.snapshot(
  file.path(OUT, "data", "retail_ga.csv"),
  essp.gather("retail", state = c("GA", "OH"), years = 2010:2024,
              freq = "annual", sector = "RES", verbose = FALSE)))

g_prov     <- chk("essp.provenance",   essp.provenance(g_generation))
g_cite     <- chk("essp.cite",         essp.cite(g_generation))
g_snapinfo <- chk("essp.snapshotinfo", essp.snapshotinfo(
  file.path(OUT, "data", "generation_us_2024.csv")))

# =============================================================================
hdr("SET 2 — INTERPRETATION")
# =============================================================================

a_cpi      <- chk("essp.cpi",        essp.cpi(2015:2024))
a_deflate  <- chk("essp.deflate",    essp.deflate(c(1, 1, 1), c(2000, 2010, 2024), base = 2024))
a_fuelgrp  <- chk("essp.fuelgroups", essp.fuelgroups())
a_gasgrp   <- chk("essp.gasgroups",  essp.gasgroups())
a_analyzed <- chk("essp.analyze",    essp.analyze(g_retail, value = "price",
                                                  by = "stateid", derive = "growth"))
a_table    <- chk("essp.table",      essp.table(as.data.frame(a_analyzed),
                                                "stateid", "price", stat = "mean"))
a_export   <- chk("essp.export",     essp.export(a_analyzed,
                                                 file.path(OUT, "data", "prices.csv")))

# --- mix and fleet -----------------------------------------------------------
a_mix      <- chk("analyze.mix",            essp.snapshot(
  file.path(OUT, "data", "mix_us_2024.csv"), analyze.mix("US", 2024)))
a_capacity <- chk("analyze.capacity",       analyze.capacity("US", 2024))
a_gen      <- chk("analyze.generation",     analyze.generation("US", 2024))
a_cf       <- chk("analyze.capacityfactor", analyze.capacityfactor("US", 2024))
a_fleet    <- chk("analyze.fleet",          analyze.fleet("GA", 2024))
a_fleetage <- chk("analyze.fleetage",       analyze.fleetage("US", 2024))
a_adds     <- chk("analyze.additions",      analyze.additions("US", 2024, years = 2015:2024))
a_rets     <- chk("analyze.retirements",    analyze.retirements("US", 2024, through = 2032))
a_top      <- chk("analyze.fuelleaders",   analyze.fuelleaders("Solar", 2024, n = 10))
a_div      <- chk("analyze.diversity",      analyze.diversity("US", 2024))
a_compare  <- chk("multi-state diversity", analyze.diversity("GA,OH,WV", 2024))

# --- hourly / load -----------------------------------------------------------
JUL <- c("2024-07-01T00", "2024-07-31T23")

a_load     <- chk("analyze.load",          analyze.load("CISO", JUL[1], JUL[2]))
a_shape    <- chk("analyze.loadshape",     analyze.loadshape("CISO", JUL[1], JUL[2]))
a_shape_m  <- chk("analyze.loadshape(by)", analyze.loadshape("CISO", "2024-06-01T00",
                                                             "2024-07-31T23", by = "month"))
a_fuelshp  <- chk("analyze.fuelshape",     essp.snapshot(
  file.path(OUT, "data", "fuelshape_ciso.csv"), analyze.fuelshape("CISO", JUL[1], JUL[2])))
a_duration <- chk("analyze.durationcurve", analyze.durationcurve("CISO", JUL[1], JUL[2]))
a_reserve  <- chk("analyze.reservemargin", analyze.reservemargin("CISO", 2024))
a_imports  <- chk("analyze.imports",       analyze.imports("CISO", JUL[1], JUL[2]))

# --- gas split and storage ---------------------------------------------------
a_gasfleet <- chk("analyze.gasfleet",  analyze.gasfleet(ba = "CISO", year = 2024))
a_gassplit <- chk("analyze.gassplit",  analyze.gassplit(a_fuelshp, ba = "CISO", year = 2024))
# CISO reports no BAT series; the regional respondent CAL does.
a_storage  <- chk("analyze.storage",   analyze.storage("CAL", "2026-07-01T00", "2026-07-28T23"))
a_displace <- chk("analyze.storagedisplace",
                  analyze.storagedisplace(a_gassplit, a_storage))

# --- prices, emissions, series -----------------------------------------------
a_prices   <- chk("analyze.prices",      analyze.prices("GA", 2010:2024))
a_emis     <- chk("analyze.emissions",   analyze.emissions("GA", 2023))
a_season   <- chk("analyze.seasonality", analyze.seasonality(
  data.frame(period = sprintf("2023-%02d", 1:12),
             mcf = c(120,110,90,60,40,30,28,29,35,55,85,115)), "mcf", "period"))
a_growth   <- chk("essp.analyze(growth)", essp.analyze(
  data.frame(year = 2015:2024, v = seq(100, 145, length.out = 10)),
  value = "v", by = NULL, derive = "growth"))

# =============================================================================
hdr("SET 3 — VISUALIZATION")
# =============================================================================

v_theme     <- chk("essp.theme",      essp.theme("Fall", 2026))
v_colors    <- chk("essp.colors",     essp.colors())
v_fuelcols  <- chk("essp.fuelcolors", essp.fuelcolors())
v_palette   <- chk("essp.palette",    essp.palette("categorical", 6))
v_highlight <- chk("essp.highlight",  essp.highlight(
  c("Natural Gas","Coal","Solar"), highlight = "Solar"))
v_accent    <- chk("essp.accent",     essp.accent("Fall", 2026))
v_textcol   <- chk("essp.textcolor",  essp.textcolor(c("#00B4D8", "#D6E6F4")))
v_slots     <- chk("essp.slots",      essp.slots())

# --- charts ------------------------------------------------------------------
v_fleetmakeup <- chk("chart.fleetmakeup", chart.fleetmakeup(a_mix, highlight = "Solar"))
# Friendly one-call charts (fetch + draw from plain strings).
v_gen_str     <- chk("chart.generation(str)",  chart.generation("Solar", "US", "2024"))
v_demand_str  <- chk("chart.demandcurve(str)", chart.demandcurve("Solar", "CISO", "2024"))
v_demand      <- chk("chart.demandcurve", chart.demandcurve(
  a_displace, palette = "house", mark_peak = TRUE))
v_storage     <- chk("chart.storagebands",  chart.storagebands(a_storage))
v_duration    <- chk("chart.durationcurve", chart.durationcurve(a_duration, highlight_pct = 5))
v_heatmap     <- chk("chart.heatmap", chart.heatmap(
  transform(a_shape_m, month = factor(month, levels = month.abb)),
  "hour", "month", "mean_mw"))
v_timeseries  <- chk("chart.timeseries", chart.timeseries(
  a_prices, "year", "price_real", group = "state", highlight = "GA"))
v_multiples   <- chk("chart.multiples", chart.multiples(
  as.data.frame(a_analyzed), "year", "price", facet = "stateid"))
v_share       <- chk("chart.share", chart.share(
  as.data.frame(a_analyzed), "year", "stateid", "price"))
v_ranked      <- chk("chart.ranked", chart.ranked(
  a_top, "state_name", "share", highlight = "California"))
v_map         <- chk("chart.map",      chart.map(a_compare, "state", "hhi"))
v_box         <- chk("chart.box",      chart.box(
  data.frame(g = rep(c("A","B"), each = 20),
             v = c(rnorm(20, 50, 8), rnorm(20, 30, 5))), "g", "v"))
v_scatter     <- chk("chart.scatter",  chart.scatter(a_cf, "mw", "capacity_factor",
                                                     label = "resource"))
v_dumbbell    <- chk("chart.dumbbell", chart.dumbbell(
  data.frame(state = c("GA","OH","WV"), t1 = c(10,12,14), t2 = c(14,15,13)),
  "state", "t1", "t2"))
v_waterfall   <- chk("chart.waterfall", chart.waterfall(a_adds, a_rets, highlight = "Solar"))
v_latex       <- chk("chart.table", chart.table(
  data.frame(Fuel = c("Natural Gas","Coal"), MW = c(561783, 188382))))

# --- writing a figure --------------------------------------------------------
v_saved <- chk("essp.save", essp.save(
  v_fleetmakeup, file.path(OUT, "figures", "fleet_makeup.png"),
  slot = "profile", formats = "png"))

# =============================================================================
hdr("SUMMARY")
# =============================================================================

results <- do.call(rbind, .results)
cat(sprintf("\n  %d commands run | %d ok | %d failed | %.1fs total\n\n",
            nrow(results), sum(results$ok), sum(!results$ok), sum(results$secs)))

if (any(!results$ok)) {
  cat("  FAILURES:\n")
  print(results[!results$ok, c("command", "shape")], row.names = FALSE)
} else {
  cat("  All commands ran.\n")
}

cat("\n  Slowest five:\n")
print(utils::head(results[order(-results$secs), c("command", "secs")], 5), row.names = FALSE)

cat("\n  Files written under", OUT, ":\n")
for (f in list.files(OUT, recursive = TRUE)) cat("   ", f, "\n")

cat("\n  Inspect any result by variable name, e.g.:\n")
cat("    a_mix          capacity vs generation, US 2024\n")
cat("    a_gasfleet     gas fleet split into CC / steam / peakers\n")
cat("    a_storage      battery charge and discharge by hour\n")
cat("    g_prov         provenance of the gathered generation data\n")
cat("    results        this run's pass/fail table\n")
cat("\n  Clean up with:  unlink(\"", OUT, "\", recursive = TRUE)\n", sep = "")
