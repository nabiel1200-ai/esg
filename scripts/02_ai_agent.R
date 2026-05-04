# ============================================================
# ESG_Alpha Live — 02_ai_agent.R
# AI agent met Currents API + Google News RSS + beschrijving
# Nabiel Mamnoen
# ============================================================

library(dplyr)
library(httr)
library(jsonlite)
library(xml2)

ANTHROPIC_KEY <- Sys.getenv("ANTHROPIC_KEY")
CURRENTS_KEY  <- Sys.getenv("CURRENTS_KEY")
pad_data      <- "data"

# ============================================================
# ZOEKQUERIES
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

# ============================================================
# BRON 1: CURRENTS API (met beschrijving)
# ============================================================

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
    data <- fromJSON(httr::content(resp, "text"))
    if (is.null(data$news) || nrow(data$news) == 0) return(NULL)
    data$news %>%
      select(title, description, published, url) %>%
      mutate(
        pub_date    = as.Date(substr(published, 1, 10)),
        description = ifelse(is.na(description), "", substr(description, 1, 300)),
        source      = "currents"
      ) %>%
      filter(!is.na(pub_date), !is.na(title)) %>%
      select(title, description, pub_date, link = url, source)
  }, error = function(e) NULL)
}

# ============================================================
# BRON 2: GOOGLE NEWS RSS (geen beschrijving beschikbaar)
# ============================================================

fetch_google_news <- function(query) {
  query_enc <- utils::URLencode(query)
  url <- paste0(
    "https://news.google.com/rss/search?q=",
    query_enc,
    "&hl=en-US&gl=US&ceid=US:en"
  )
  tryCatch({
    resp <- GET(url, timeout(10))
    if (status_code(resp) != 200) return(NULL)

    xml   <- read_xml(httr::content(resp, "text"))
    items <- xml_find_all(xml, "//item")
    if (length(items) == 0) return(NULL)

    titels <- xml_text(xml_find_first(items, "title"))
    datums <- xml_text(xml_find_first(items, "pubDate"))
    links  <- xml_text(xml_find_first(items, "link"))

    data.frame(
      title       = titels,
      description = "",
      pub_date    = as.Date(sub(" GMT| UTC", "", datums),
                            format = "%a, %d %b %Y %H:%M:%S"),
      link        = links,
      source      = "google",
      stringsAsFactors = FALSE
    ) %>% filter(!is.na(pub_date), !is.na(title))

  }, error = function(e) NULL)
}

# ============================================================
# NIEUWS OPHALEN VAN BEIDE BRONNEN
# ============================================================

cat("Nieuws ophalen van Currents API + Google News...\n")

all_articles <- list()

for (q in queries) {
  c_articles <- fetch_currents(q, CURRENTS_KEY)
  if (!is.null(c_articles) && nrow(c_articles) > 0) {
    c_articles <- c_articles %>% filter(pub_date >= Sys.Date() - 7)
    if (nrow(c_articles) > 0) all_articles[[paste0("currents_", q)]] <- c_articles
  }

  g_articles <- fetch_google_news(q)
  if (!is.null(g_articles) && nrow(g_articles) > 0) {
    g_articles <- g_articles %>% filter(pub_date >= Sys.Date() - 7)
    if (nrow(g_articles) > 0) all_articles[[paste0("google_", q)]] <- g_articles
  }

  Sys.sleep(0.5)
}

if (length(all_articles) == 0) {
  cat("Geen nieuws gevonden vandaag\n")
  quit(status = 0)
}

alle_artikelen <- bind_rows(all_articles) %>%
  distinct(title, .keep_all = TRUE) %>%
  filter(pub_date >= Sys.Date() - 7)

cat("Artikelen gevonden:\n")
cat("  Totaal uniek:", nrow(alle_artikelen), "\n")
cat("  Currents:    ", sum(alle_artikelen$source == "currents"), "\n")
cat("  Google News: ", sum(alle_artikelen$source == "google"), "\n\n")

# ============================================================
# CLAUDE BEOORDEELT ELK ARTIKEL (met beschrijving)
# ============================================================

beoordeel_artikel <- function(titel, beschrijving, api_key) {

  # Beschrijving toevoegen als die beschikbaar is
  context <- if (nchar(trimws(beschrijving)) > 0) {
    paste0("BESCHRIJVING: ", beschrijving, "\n\n")
  } else {
    ""
  }

  prompt <- paste0(
    "Je bent een ESG analist. Beoordeel het volgende nieuwsartikel:\n\n",
    "TITEL: ", titel, "\n\n",
    context,
    "Beantwoord deze vragen:\n",
    "1. Gaat dit over een ESG controversy voor een beursgenoteerd bedrijf?\n",
    "2. Welk bedrijf? Geef ook de beursticker als je die kent\n",
    "3. Pillar: E (Environmental), S (Social), G (Governance), Cross (meerdere)\n",
    "4. Severity: 1 (laag), 2 (midden), 3 (hoog)\n",
    "   - Severity 3: crimineel, miljarden, class action, federaal onderzoek\n",
    "   - Severity 2: rechtszaak, boete, onderzoek, settlement\n",
    "   - Severity 1: klacht, beschuldiging, kleine overtreding\n\n",
    "Antwoord ALLEEN in dit JSON formaat zonder extra tekst:\n",
    "{\"is_esg\": true, \"bedrijf\": \"NAAM\", \"ticker\": \"TICK\", \"pillar\": \"G\", \"severity\": 2}"
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

    data  <- fromJSON(httr::content(resp, "text"))
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
# ALLE ARTIKELEN BEOORDELEN
# ============================================================

cat("Claude beoordeelt", nrow(alle_artikelen), "artikelen...\n\n")

nieuwe_events <- list()

for (i in 1:nrow(alle_artikelen)) {
  titel       <- alle_artikelen$title[i]
  beschrijving <- alle_artikelen$description[i]
  pub_date    <- alle_artikelen$pub_date[i]
  link        <- alle_artikelen$link[i]
  source      <- alle_artikelen$source[i]

  cat("  [", i, "/", nrow(alle_artikelen), "]", substr(titel, 1, 60), "...\n")

  result <- beoordeel_artikel(titel, beschrijving, ANTHROPIC_KEY)

  if (is.null(result))                                      { Sys.sleep(0.5); next }
  if (!isTRUE(result$is_esg))                              { cat("    Geen ESG event\n"); Sys.sleep(0.5); next }
  if (is.null(result$bedrijf) || result$bedrijf == "null") { Sys.sleep(0.5); next }

  ticker <- if (!is.null(result$ticker) && result$ticker != "null") result$ticker else NA

  event <- data.frame(
    isin        = NA,
    company     = result$bedrijf,
    ticker      = ticker,
    title       = titel,
    description = beschrijving,
    pub_date    = pub_date,
    link        = link,
    source      = source,
    pillar      = result$pillar,
    severity    = as.integer(pmin(result$severity, 3)),
    scraped_at  = as.numeric(Sys.Date()),
    stringsAsFactors = FALSE
  )

  nieuwe_events[[i]] <- event
  cat("    ESG event:", result$bedrijf, "(", ticker, ") | Pillar:",
      result$pillar, "| Severity:", result$severity, "\n")

  Sys.sleep(0.5)
}

# ============================================================
# SAMENVOEGEN MET BESTAANDE EVENTS
# ============================================================

if (length(nieuwe_events) == 0) {
  cat("\nGeen nieuwe ESG events gevonden vandaag\n")
} else {
  nieuwe_df <- bind_rows(nieuwe_events) %>% filter(!is.na(pillar))

  cat("\n=== NIEUWE EVENTS ===\n")
  cat("Gevonden:", nrow(nieuwe_df), "\n\n")
  print(nieuwe_df %>% select(company, ticker, pub_date, pillar, severity, title))

  bestaand_pad <- file.path(pad_data, "events_detected.csv")

  if (file.exists(bestaand_pad)) {
    bestaand <- read.csv(bestaand_pad, stringsAsFactors = FALSE) %>%
      mutate(pub_date = as.Date(pub_date))
    if (!"ticker"      %in% names(bestaand)) bestaand$ticker      <- NA
    if (!"source"      %in% names(bestaand)) bestaand$source      <- NA
    if (!"description" %in% names(bestaand)) bestaand$description <- NA
    gecombineerd <- bind_rows(bestaand, nieuwe_df) %>%
      distinct(company, title, .keep_all = TRUE) %>%
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
