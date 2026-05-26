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

cat("Events ingeladen:", nrow(events), "\n")

# Alleen events met geldige ticker uit de laatste 30 dagen
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

get_return <- function(ticker, entry_date, exit_date) {
  tryCatch({
    from <- format(entry_date - 2)
    to   <- format(min(exit_date + 2, Sys.Date()))

    getSymbols(ticker, from = from, to = to,
               src = "yahoo", auto.assign = TRUE, warnings = FALSE)

    price_data <- get(ticker)
    closes     <- as.numeric(Cl(price_data))
    dates      <- as.Date(index(price_data))

    price_df <- data.frame(date = dates, close = closes) %>%
      filter(!is.na(close)) %>%
      arrange(date)

    entry_row <- price_df %>% filter(date >= entry_date) %>% slice(1)
    exit_row  <- price_df %>% filter(date >= exit_date)  %>% slice(1)

    if (nrow(entry_row) == 0) return(list(
      entry_price  = NA, exit_price   = NA,
      stock_return = NA, short_return = NA,
      status       = "Geen data"
    ))

    entry_price <- entry_row$close

    if (nrow(exit_row) == 0 || exit_date > Sys.Date()) {
      current_price  <- tail(price_df$close, 1)
      current_return <- (current_price - entry_price) / entry_price * 100
      return(list(
        entry_price  = round(entry_price, 2),
        exit_price   = round(current_price, 2),
        stock_return = round(current_return, 4),
        short_return = round(-current_return - 0.1, 4),
        status       = "Open"
      ))
    } else {
      exit_price   <- exit_row$close
      stock_return <- (exit_price - entry_price) / entry_price * 100
      return(list(
        entry_price  = round(entry_price, 2),
        exit_price   = round(exit_price, 2),
        stock_return = round(stock_return, 4),
        short_return = round(-stock_return - 0.1, 4),
        status       = "Gesloten"
      ))
    }

  }, error = function(e) {
    list(entry_price = NA, exit_price = NA,
         stock_return = NA, short_return = NA,
         status = paste0("Fout: ", conditionMessage(e)))
  })
}

# ============================================================
# STAP 3: VOOR ELK EVENT KOERS OPHALEN
# ============================================================

cat("Koersen ophalen voor", nrow(events_met_ticker), "events...\n\n")

results <- list()

for (i in 1:nrow(events_met_ticker)) {
  ev         <- events_met_ticker[i, ]
  ticker     <- ev$ticker
  entry_date <- ev$pub_date + 1
  exit_date  <- ev$pub_date + 6

  cat("  [", i, "/", nrow(events_met_ticker), "]",
      ev$company, "(", ticker, ") entry:", format(entry_date), "\n")

  koers <- get_return(ticker, entry_date, exit_date)

  results[[i]] <- data.frame(
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

  Sys.sleep(0.3)
}

signals_df <- bind_rows(results)

# ============================================================
# STAP 4: SAMENVATTING
# ============================================================

cat("\n=== PERFORMANCE SAMENVATTING ===\n")
cat("Totaal signals:", nrow(signals_df), "\n")
cat("Open:          ", sum(signals_df$status == "Open",     na.rm = TRUE), "\n")
cat("Gesloten:      ", sum(signals_df$status == "Gesloten", na.rm = TRUE), "\n\n")

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
