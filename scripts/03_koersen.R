# ============================================================
# ESG_Alpha Live — 03_koersen.R
# Real-time koersen ophalen voor gedetecteerde events
# Nabiel Mamnoen
# ============================================================

library(dplyr)
library(quantmod)

pad_data <- "data"

# Lege signals_live.csv template (altijd beschikbaar voor git)
lege_signals <- function() {
  data.frame(
    company      = character(),
    ticker       = character(),
    pub_date     = as.Date(character()),
    entry_date   = as.Date(character()),
    exit_date    = as.Date(character()),
    pillar       = character(),
    severity     = integer(),
    entry_price  = numeric(),
    exit_price   = numeric(),
    stock_return = numeric(),
    short_return = numeric(),
    status       = character(),
    title        = character(),
    stringsAsFactors = FALSE
  )
}

# ============================================================
# STAP 1: EVENTS INLADEN
# ============================================================

events_pad <- file.path(pad_data, "events_detected.csv")

if (!file.exists(events_pad)) {
  cat("events_detected.csv bestaat nog niet — eerste run?\n")
  write.csv(lege_signals(), file.path(pad_data, "signals_live.csv"), row.names = FALSE)
  quit(status = 0)
}

events <- read.csv(events_pad, stringsAsFactors = FALSE) %>%
  mutate(pub_date = as.Date(pub_date))

if (!"ticker" %in% names(events)) events$ticker <- NA

cat("Events ingeladen:", nrow(events), "\n")

events_met_ticker <- events %>%
  filter(!is.na(ticker), ticker != "NA", ticker != "", nchar(ticker) <= 6) %>%
  filter(pub_date >= Sys.Date() - 30)

cat("Events met ticker (laatste 30 dagen):", nrow(events_met_ticker), "\n\n")

if (nrow(events_met_ticker) == 0) {
  cat("Geen events met ticker gevonden — signals_live.csv leeg opgeslagen\n")
  write.csv(lege_signals(), file.path(pad_data, "signals_live.csv"), row.names = FALSE)
  quit(status = 0)
}

# ============================================================
# STAP 2: KOERSEN OPHALEN PER EVENT
# ============================================================

get_price_data <- function(ticker, from_date, to_date) {
  tryCatch({
    getSymbols(ticker, from = format(from_date), to = format(to_date),
               src = "yahoo", auto.assign = TRUE, warnings = FALSE)
    price_data <- get(ticker)
    data.frame(
      date  = as.Date(index(price_data)),
      close = as.numeric(Cl(price_data))
    ) %>% filter(!is.na(close)) %>% arrange(date)
  }, error = function(e) NULL)
}

get_return <- function(ticker, entry_date, exit_date) {
  from <- entry_date - 5
  to   <- min(exit_date + 2, Sys.Date())

  price_df <- get_price_data(ticker, from, to)
  if (is.null(price_df) || nrow(price_df) == 0) return(list(
    entry_price = NA, exit_price = NA,
    stock_return = NA, short_return = NA,
    status = "Geen data", confirmed = FALSE
  ))

  entry_row <- price_df %>% filter(date >= entry_date) %>% slice(1)
  if (nrow(entry_row) == 0) return(list(
    entry_price = NA, exit_price = NA,
    stock_return = NA, short_return = NA,
    status = "Geen entry data", confirmed = FALSE
  ))

  entry_price <- entry_row$close

  # ── KOERSBEVESTIGING ──────────────────────────────────────
  # Check of het aandeel op entry_date gedaald is t.o.v. dag ervoor
  dag_voor_entry <- price_df %>% filter(date < entry_date) %>% tail(1)
  
  confirmed <- if (nrow(dag_voor_entry) > 0) {
    entry_price < dag_voor_entry$close  # TRUE = gedaald = bevestigd signal
  } else {
    TRUE  # geen historische data beschikbaar → toch doorgaan
  }
  # ──────────────────────────────────────────────────────────

  if (nrow(price_df %>% filter(date >= exit_date)) == 0 || exit_date > Sys.Date()) {
    current_price  <- tail(price_df$close, 1)
    current_return <- (current_price - entry_price) / entry_price * 100
    return(list(
      entry_price  = round(entry_price, 2),
      exit_price   = round(current_price, 2),
      stock_return = round(current_return, 4),
      short_return = round(-current_return - 0.1, 4),
      status       = "Open",
      confirmed    = confirmed
    ))
  } else {
    exit_row     <- price_df %>% filter(date >= exit_date) %>% slice(1)
    exit_price   <- exit_row$close
    stock_return <- (exit_price - entry_price) / entry_price * 100
    return(list(
      entry_price  = round(entry_price, 2),
      exit_price   = round(exit_price, 2),
      stock_return = round(stock_return, 4),
      short_return = round(-stock_return - 0.1, 4),
      status       = "Gesloten",
      confirmed    = confirmed
    ))
  }
}

# ============================================================
# STAP 3: VOOR ELK EVENT KOERS OPHALEN + BEVESTIGING CHECK
# ============================================================

cat("Koersen ophalen voor", nrow(events_met_ticker), "events...\n\n")

results      <- list()
n_gefilterd  <- 0

for (i in 1:nrow(events_met_ticker)) {
  ev         <- events_met_ticker[i, ]
  ticker     <- ev$ticker
  entry_date <- ev$pub_date + 1
  exit_date  <- ev$pub_date + 6

  cat("  [", i, "/", nrow(events_met_ticker), "]",
      ev$company, "(", ticker, ") entry:", format(entry_date), "\n")

  koers <- get_return(ticker, entry_date, exit_date)

  # Signal overslaan als koers NIET gedaald is op entry dag
  if (!is.null(koers$confirmed) && !koers$confirmed) {
    cat("    ✗ Geen koersdaling op entry dag — signal overgeslagen\n")
    n_gefilterd <- n_gefilterd + 1
    Sys.sleep(0.3)
    next
  }

  results[[length(results) + 1]] <- data.frame(
    company      = ev$company,
    ticker       = ticker,
    pub_date     = ev$pub_date,
    entry_date   = entry_date,
    exit_date    = exit_date,
    pillar       = ev$pillar,
    severity     = ev$severity,
    entry_price  = koers$entry_price,
    exit_price   = koers$exit_price,
    stock_return = koers$stock_return,
    short_return = koers$short_return,
    status       = koers$status,
    title        = ev$title,
    stringsAsFactors = FALSE
  )

  cat("    ✓ Koersdaling bevestigd — signal toegevoegd\n")
  Sys.sleep(0.3)
}

# ============================================================
# STAP 4: SAMENVATTING
# ============================================================

if (length(results) == 0) {
  cat("\nGeen signals na koersbevestiging filter\n")
  cat("Gefilterd (geen daling):", n_gefilterd, "\n")
  write.csv(lege_signals(), file.path(pad_data, "signals_live.csv"), row.names = FALSE)
  quit(status = 0)
}

signals_df <- bind_rows(results)

cat("\n=== PERFORMANCE SAMENVATTING ===\n")
cat("Totaal signals:          ", nrow(signals_df), "\n")
cat("Gefilterd (geen daling): ", n_gefilterd, "\n")
cat("Open:                    ", sum(signals_df$status == "Open",     na.rm = TRUE), "\n")
cat("Gesloten:                ", sum(signals_df$status == "Gesloten", na.rm = TRUE), "\n\n")

perf <- signals_df %>% filter(!is.na(short_return))
if (nrow(perf) > 0) {
  cat("Gem. SHORT return:", round(mean(perf$short_return), 4), "%\n")
  cat("Hit ratio:        ", round(mean(perf$short_return > 0) * 100, 1), "%\n\n")
}

# ============================================================
# STAP 5: OPSLAAN
# ============================================================

write.csv(signals_df,
          file.path(pad_data, "signals_live.csv"),
          row.names = FALSE)

cat("\nsignals_live.csv opgeslagen:", nrow(signals_df), "signals\n")
cat("\n--- KOERSEN KLAAR ---", format(Sys.time(), "%d-%m-%Y %H:%M"), "\n")
