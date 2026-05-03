# ============================================================
# ESG_Alpha Live — 02_ai_agent.R
# AI agent die nieuws beoordeelt via Claude
# Nabiel Mamnoen
# ============================================================

library(dplyr)
library(httr)
library(jsonlite)

ANTHROPIC_KEY <- Sys.getenv("ANTHROPIC_KEY")
CURRENTS_KEY  <- Sys.getenv("CURRENTS_KEY")
# Naar:
pad_data <- "data"

sp500 <- read.csv(file.path(pad_data, "sp500_tickers.csv"),
                  stringsAsFactors = FALSE)

# ============================================================
# STAP 1: NIEUWS OPHALEN VIA CURRENTS API
# ============================================================

queries <- c(
  "SEC investigation fraud securities lawsuit",
  "environmental violation EPA fine pollution",
  "workplace harassment discrimination lawsuit settlement",
  "data breach privacy violation class action",
  "DOJ antitrust bribery corruption fine",
  "shareholder class action securities fraud",
  "labor violation wage theft human rights",
  "corporate misconduct penalty court ruling",
  "oil spill toxic contamination company",
  "executive misconduct insider trading charged"
)

fetch_currents <- function(query, api_key) {
  query_enc <- utils::URLencode(query)
  url <- paste0(
    "https://api.currentsapi.services/v1/search?",
    "keywords=", query_enc,
    "&language=en",
    "&apiKey=", api_key
  )
  tryCatch({
    resp <- GET(url, timeout(10))
    if (status_code(resp) != 200) return(NULL)
    data <- fromJSON(content(resp, "text", encoding = "UTF-8"))
    if (is.null(data$news) || nrow(data$news) == 0) return(NULL)
    data$news %>%
      select(title, published, url) %>%
      mutate(pub_date = as.Date(substr(published, 1, 10))) %>%
      filter(!is.na(pub_date), !is.na(title)) %>%
      select(title, pub_date, link = url)
  }, error = function(e) NULL)
}

cat("Nieuws ophalen...\n")
all_articles <- list()

for (q in queries) {
  articles <- fetch_currents(q, CURRENTS_KEY)
  if (!is.null(articles) && nrow(articles) > 0) {
    articles <- articles %>% filter(pub_date >= Sys.Date() - 7)
    if (nrow(articles) > 0) all_articles[[q]] <- articles
  }
  Sys.sleep(0.5)
}

if (length(all_articles) == 0) {
  cat("Geen nieuws gevonden\n")
  stop("Geen artikelen")
}

alle_artikelen <- bind_rows(all_articles) %>%
  distinct(title, .keep_all = TRUE)

cat("Artikelen gevonden:", nrow(alle_artikelen), "\n\n")

# ============================================================
# STAP 2: CLAUDE BEOORDEELT ELK ARTIKEL
# ============================================================

beoordeel_artikel <- function(titel, api_key) {
  
  prompt <- paste0(
    "Je bent een ESG analist. Beoordeel het volgende nieuwsartikel:\n\n",
    "TITEL: ", titel, "\n\n",
    "Beantwoord deze vragen:\n",
    "1. Gaat dit over een ESG controversy voor een beursgenoteerd bedrijf?\n",
    "2. Welk bedrijf?\n",
    "3. Pillar: E (Environmental), S (Social), G (Governance), Cross (meerdere)\n",
    "4. Severity: 1 (laag), 2 (midden), 3 (hoog)\n\n",
    "Antwoord ALLEEN in dit JSON formaat zonder extra tekst:\n",
    "{\"is_esg\": true, \"bedrijf\": \"NAAM\", \"pillar\": \"G\", \"severity\": 2}"
  )
  
  body <- list(
    model      = "claude-haiku-4-5-20251001",
    max_tokens = 150,
    messages   = list(list(role = "user", content = prompt))
  )
  
  tryCatch({
    resp <- POST(
      "https://api.anthropic.com/v1/messages",
      add_headers(
        "x-api-key"         = api_key,
        "anthropic-version" = "2023-06-01",
        "content-type"      = "application/json"
      ),
      body = toJSON(body, auto_unbox = TRUE),
      timeout(15)
    )
    
    if (status_code(resp) != 200) return(NULL)
    
    data  <- fromJSON(content(resp, "text", encoding = "UTF-8"))
    tekst <- data$content$text[1]
    tekst <- gsub("```json|```", "", tekst)
    tekst <- trimws(tekst)
    
    result <- fromJSON(tekst)
    return(result)
    
  }, error = function(e) {
    cat("    Fout:", conditionMessage(e), "\n")
    return(NULL)
  })
}

# ============================================================
# STAP 3: ALLE ARTIKELEN BEOORDELEN
# ============================================================

cat("Claude beoordeelt", nrow(alle_artikelen), "artikelen...\n\n")

nieuwe_events <- list()

for (i in 1:nrow(alle_artikelen)) {
  titel    <- alle_artikelen$title[i]
  pub_date <- alle_artikelen$pub_date[i]
  link     <- alle_artikelen$link[i]
  
  cat("  [", i, "/", nrow(alle_artikelen), "]", substr(titel, 1, 60), "...\n")
  
  result <- beoordeel_artikel(titel, ANTHROPIC_KEY)
  
  if (is.null(result)) { Sys.sleep(0.5); next }
  if (!isTRUE(result$is_esg)) { cat("    Geen ESG event\n"); Sys.sleep(0.5); next }
  if (is.null(result$bedrijf) || result$bedrijf == "null") { Sys.sleep(0.5); next }
  
  isin_match <- sp500 %>%
    filter(grepl(result$bedrijf, company, ignore.case = TRUE)) %>%
    slice(1)
  
  isin <- if (nrow(isin_match) > 0) isin_match$isin[1] else NA
  
  # Vervang de ISIN matching stap door dit:
  event <- data.frame(
    isin       = NA,          # geen ISIN matching meer
    company    = result$bedrijf,
    title      = titel,
    pub_date   = pub_date,
    link       = link,
    pillar     = result$pillar,
    severity   = as.integer(result$severity),
    scraped_at = as.numeric(Sys.Date()),
    stringsAsFactors = FALSE
  )
  
  nieuwe_events[[i]] <- event
  cat("    ESG event:", result$bedrijf, "| Pillar:", result$pillar, "| Severity:", result$severity, "\n")
  
  Sys.sleep(0.5)
}

# ============================================================
# STAP 4: SAMENVOEGEN MET BESTAANDE EVENTS
# ============================================================

if (length(nieuwe_events) == 0) {
  cat("\nGeen nieuwe ESG events gevonden\n")
} else {
  nieuwe_df <- bind_rows(nieuwe_events) %>% filter(!is.na(pillar))
  
  cat("\n=== NIEUWE EVENTS ===\n")
  cat("Gevonden:", nrow(nieuwe_df), "\n\n")
  print(nieuwe_df %>% select(company, pub_date, pillar, severity, title))
  
  bestaand_pad <- file.path(pad_data, "events_detected.csv")
  
  if (file.exists(bestaand_pad)) {
    bestaand <- read.csv(bestaand_pad, stringsAsFactors = FALSE) %>%
      mutate(pub_date = as.Date(pub_date))
    gecombineerd <- bind_rows(bestaand, nieuwe_df) %>%
      distinct(isin, title, .keep_all = TRUE) %>%
      arrange(desc(pub_date))
  } else {
    gecombineerd <- nieuwe_df
  }
  
  write.csv(gecombineerd,
            file.path(pad_data, "events_detected.csv"),
            row.names = FALSE)
  
  cat("\nTotaal events in database:", nrow(gecombineerd), "\n")
  cat("events_detected.csv bijgewerkt\n")
}

cat("\n--- AI AGENT KLAAR ---", format(Sys.time(), "%d-%m-%Y %H:%M"), "\n")

# Test op artikel 1
titel <- alle_artikelen$title[1]
cat("Artikel:", titel, "\n\n")

result <- beoordeel_artikel(titel, ANTHROPIC_KEY)
cat("is_esg:", result$is_esg, "\n")
cat("bedrijf:", result$bedrijf, "\n")
cat("pillar:", result$pillar, "\n")
cat("severity:", result$severity, "\n")

# Toon ruwe Claude response
titel <- alle_artikelen$title[1]

prompt <- paste0(
  "Je bent een ESG analist. Beoordeel het volgende nieuwsartikel:\n\n",
  "TITEL: ", titel, "\n\n",
  "Antwoord ALLEEN in dit JSON formaat zonder extra tekst:\n",
  "{\"is_esg\": true, \"bedrijf\": \"NAAM\", \"pillar\": \"G\", \"severity\": 2}"
)

body <- list(
  model      = "claude-haiku-4-5-20251001",
  max_tokens = 150,
  messages   = list(list(role = "user", content = prompt))
)

resp <- POST(
  "https://api.anthropic.com/v1/messages",
  add_headers(
    "x-api-key"         = ANTHROPIC_KEY,
    "anthropic-version" = "2023-06-01",
    "content-type"      = "application/json"
  ),
  body = toJSON(body, auto_unbox = TRUE),
  timeout(15)
)

cat("Status:", status_code(resp), "\n")
data  <- fromJSON(content(resp, "text", encoding = "UTF-8"))
tekst <- data$content$text[1]
cat("Ruwe response:\n", tekst, "\n")

# Data kopiëren naar dashboard map
file.copy(
  from = "/Users/nabiel/Desktop/Controversy_trading/ESG_Alpha_Live/data/events_detected.csv",
  to   = "/Users/nabiel/Desktop/Controversy_trading/ESG_Alpha_Live/dashboard/events_detected.csv",
  overwrite = TRUE
)

# Dashboard herdeployen
rsconnect::deployApp('/Users/nabiel/Desktop/Controversy_trading/ESG_Alpha_Live/dashboard')


file.copy(
  from = "/Users/nabiel/Desktop/Controversy_trading/ESG_Alpha_Live/data/events_detected.csv",
  to   = "/Users/nabiel/Desktop/Controversy_trading/ESG_Alpha_Live/dashboard/events_detected.csv",
  overwrite = TRUE
)
cat("Gekopieerd!\n")

# Controleer
df <- read.csv("/Users/nabiel/Desktop/Controversy_trading/ESG_Alpha_Live/dashboard/events_detected.csv")
cat("Aantal events in dashboard map:", nrow(df), "\n")

# Dashboard herdeployen
rsconnect::deployApp('/Users/nabiel/Desktop/Controversy_trading/ESG_Alpha_Live/dashboard')
