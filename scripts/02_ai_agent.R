# ============================================================
# ESG_Alpha Live — 02_ai_agent.R
# AI agent met Reuters RSS, AP News RSS + Google News RSS
# Nabiel Mamnoen
# ============================================================

library(dplyr)
library(httr)
library(jsonlite)
library(xml2)

ANTHROPIC_KEY <- Sys.getenv("ANTHROPIC_KEY")
pad_data      <- "data"

# ============================================================
# FALSE POSITIVE KEYWORDS
# ============================================================

false_positive_keywords <- c(
  # Advocatenkantoor berichten
  "investors have opportunity to lead",
  "investors have opportunity to join",
  "shareholders who lost money",
  "claimsfiler reminds",
  "bronstein gewirtz",
  "hagens berman",
  "schall law firm",
  "gross law firm",
  "glancy prongay",
  "pomerantz law",
  "gainey mcnamee",
  "rosen law firm",
  "faruqi & faruqi",
  "levi & korsinsky",
  "wolf haldenstein",
  "law offices of",
  "deadline alert",
  "investor alert",
  "investor notice",
  "shareholder alert",
  "investors urged to contact",
  "investors may seek to lead",
  "investors with losses",
  "lead plaintiff deadline",
  "securities class action",
  "class action lawsuit",
  "securities fraud lawsuit",
  "fraud investigation",
  "remind investors",
  "encourage investors",
  "urges investors",
  "urges former",
  "if you purchased shares",
  "recover losses",
  "pursuing claims",
  "securities litigation",
  "investor rights",
  "important notice to",
  "notice to long-term shareholders",
  "shareholders who purchased",
  "investors who purchased",
  # Earnings / financieel
  "quarterly earnings",
  "q1 results", "q2 results", "q3 results", "q4 results",
  "earnings per share",
  "revenue guidance",
  "raised its outlook",
  "dividend declared",
  "stock buyback",
  "share repurchase",
  # Overige ruis
  "h-1b visa", "h1b visa",
  "premier league", "nfl", "nba",
  "bitcoin", "crypto", "blockchain",
  "analyst", "upgrade", "downgrade", "price target",
  "how to claim", "how to file a claim",
  "who qualifies", "here's how to"
)

is_false_positive <- function(title) {
  t_lower <- tolower(title)
  any(sapply(false_positive_keywords, function(k) grepl(k, t_lower, fixed = TRUE)))
}

# ============================================================
# BRON 1: REUTERS RSS (gratis, geen key)
# ============================================================

reuters_feeds <- c(
  "https://feeds.reuters.com/reuters/businessNews",
  "https://feeds.reuters.com/reuters/companyNews",
  "https://feeds.reuters.com/reuters/environment"
)

fetch_reuters <- function(feed_url) {
  tryCatch({
    resp <- GET(feed_url, timeout(15))
    if (status_code(resp) != 200) return(NULL)
    xml   <- read_xml(content(resp, "text", encoding = "UTF-8"))
    items <- xml_find_all(xml, "//item")
    if (length(items) == 0) return(NULL)
    titels <- xml_text(xml_find_first(items, "title"))
    datums <- xml_text(xml_find_first(items, "pubDate"))
    links  <- xml_text(xml_find_first(items, "link"))
    descr  <- xml_text(xml_find_first(items, "description"))
    data.frame(
      title       = titels,
      description = substr(ifelse(is.na(descr), "", descr), 1, 300),
      pub_date    = as.Date(sub(" GMT| UTC", "", datums), format = "%a, %d %b %Y %H:%M:%S"),
      link        = links,
      source      = "reuters",
      stringsAsFactors = FALSE
    ) %>% filter(!is.na(pub_date), !is.na(title), title != "")
  }, error = function(e) {
    cat("  Reuters fout:", conditionMessage(e), "\n")
    NULL
  })
}

# ============================================================
# BRON 2: AP NEWS RSS (gratis, geen key)
# ============================================================

ap_feeds <- c(
  "https://rsshub.app/apnews/topics/business",
  "https://rsshub.app/apnews/topics/climate-environment"
)

fetch_ap <- function(feed_url) {
  tryCatch({
    resp <- GET(feed_url, timeout(15))
    if (status_code(resp) != 200) return(NULL)
    xml   <- read_xml(content(resp, "text", encoding = "UTF-8"))
    items <- xml_find_all(xml, "//item")
    if (length(items) == 0) return(NULL)
    titels <- xml_text(xml_find_first(items, "title"))
    datums <- xml_text(xml_find_first(items, "pubDate"))
    links  <- xml_text(xml_find_first(items, "link"))
    descr  <- xml_text(xml_find_first(items, "description"))
    data.frame(
      title       = titels,
      description = substr(ifelse(is.na(descr), "", descr), 1, 300),
      pub_date    = as.Date(sub(" GMT| UTC", "", datums), format = "%a, %d %b %Y %H:%M:%S"),
      link        = links,
      source      = "ap_news",
      stringsAsFactors = FALSE
    ) %>% filter(!is.na(pub_date), !is.na(title), title != "")
  }, error = function(e) {
    cat("  AP News fout:", conditionMessage(e), "\n")
    NULL
  })
}

# ============================================================
# BRON 3: GOOGLE NEWS RSS (gerichte queries)
# ============================================================

google_queries <- c(
  "company environmental fine EPA penalty",
  "oil spill chemical leak contamination company",
  "workplace harassment discrimination settlement company",
  "corporate data breach privacy scandal",
  "executive bribery corruption arrested company",
  "factory safety workers killed injured company",
  "company pollution toxic waste fine"
)

fetch_google_news <- function(query) {
  query_enc <- utils::URLencode(query)
  url <- paste0("https://news.google.com/rss/search?q=", query_enc, "&hl=en-US&gl=US&ceid=US:en")
  tryCatch({
    resp <- GET(url, timeout(10))
    if (status_code(resp) != 200) return(NULL)
    xml   <- read_xml(content(resp, "text", encoding = "UTF-8"))
    items <- xml_find_all(xml, "//item")
    if (length(items) == 0) return(NULL)
    titels <- xml_text(xml_find_first(items, "title"))
    datums <- xml_text(xml_find_first(items, "pubDate"))
    links  <- xml_text(xml_find_first(items, "link"))
    data.frame(
      title       = titels,
      description = "",
      pub_date    = as.Date(sub(" GMT| UTC", "", datums), format = "%a, %d %b %Y %H:%M:%S"),
      link        = links,
      source      = "google",
      stringsAsFactors = FALSE
    ) %>% filter(!is.na(pub_date), !is.na(title), title != "")
  }, error = function(e) NULL)
}

# ============================================================
# STAP 1: NIEUWS OPHALEN
# ============================================================

cat("=== ESG_Alpha AI Agent gestart ===\n")
cat("Tijdstip:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n\n")

all_articles <- list()

cat("Reuters RSS ophalen...\n")
for (feed in reuters_feeds) {
  result <- fetch_reuters(feed)
  if (!is.null(result)) all_articles[[paste0("reuters_", feed)]] <- result
  Sys.sleep(0.5)
}

cat("AP News RSS ophalen...\n")
for (feed in ap_feeds) {
  result <- fetch_ap(feed)
  if (!is.null(result)) all_articles[[paste0("ap_", feed)]] <- result
  Sys.sleep(0.5)
}

cat("Google News RSS ophalen...\n")
for (q in google_queries) {
  result <- fetch_google_news(q)
  if (!is.null(result)) {
    result <- result %>% filter(pub_date >= Sys.Date() - 7)
    if (nrow(result) > 0) all_articles[[paste0("google_", q)]] <- result
  }
  Sys.sleep(1)
}

if (length(all_articles) == 0) {
  cat("Geen nieuws gevonden vandaag\n")
  quit(status = 0)
}

# ============================================================
# STAP 2: COMBINEREN + FALSE POSITIVES ERUIT
# ============================================================

alle_artikelen <- bind_rows(all_articles) %>%
  distinct(title, .keep_all = TRUE) %>%
  filter(pub_date >= Sys.Date() - 7) %>%
  filter(!sapply(title, is_false_positive))

cat("\nArtikelen na filtering:\n")
cat("  Totaal uniek: ", nrow(alle_artikelen), "\n")
cat("  Reuters:      ", sum(alle_artikelen$source == "reuters"), "\n")
cat("  AP News:      ", sum(alle_artikelen$source == "ap_news"), "\n")
cat("  Google News:  ", sum(alle_artikelen$source == "google"), "\n\n")

if (nrow(alle_artikelen) == 0) {
  cat("Geen artikelen over na filtering\n")
  quit(status = 0)
}

# ============================================================
# STAP 3: CLAUDE BEOORDEELT ELK ARTIKEL
# ============================================================

beoordeel_artikel <- function(titel, beschrijving, api_key) {
  context <- if (nchar(trimws(beschrijving)) > 0) paste0("BESCHRIJVING: ", beschrijving, "\n\n") else ""
  prompt <- paste0(
    "Je bent een ESG analist. Beoordeel het volgende nieuwsartikel:\n\n",
    "TITEL: ", titel, "\n\n",
    context,
    "Beantwoord deze vragen:\n",
    "1. Gaat dit over een ESG controversy voor een BEURSGENOTEERD bedrijf?\n",
    "   - Alleen bedrijven die op een beurs verhandeld worden (NYSE, NASDAQ, AEX etc.)\n",
    "   - Overheidsinstanties, NGOs en privébedrijven tellen NIET\n",
    "2. Welk beursgenoteerd bedrijf?\n",
    "3. Wat is de beursticker? (bijv. AAPL, SHEL, ASML)\n",
    "4. Pillar: E (Environmental), S (Social), G (Governance), Cross (meerdere)\n",
    "5. Severity: 1 (laag), 2 (midden), 3 (hoog)\n",
    "   - Severity 3: crimineel, miljarden, doden, federaal onderzoek\n",
    "   - Severity 2: boete, rechtszaak, settlement, onderzoek\n",
    "   - Severity 1: klacht, beschuldiging, kleine overtreding\n\n",
    "Antwoord ALLEEN in dit JSON formaat zonder extra tekst:\n",
    "{\"is_esg\": true, \"bedrijf\": \"NAAM\", \"ticker\": \"TICK\", \"pillar\": \"E\", \"severity\": 2}"
  )
  body <- list(
    model      = "claude-haiku-4-5-20251001",
    max_tokens = 150,
    messages   = list(list(role = "user", content = prompt))
  )
  tryCatch({
    resp <- POST(
      "https://api.anthropic.com/v1/messages",
      add_headers("x-api-key" = api_key, "anthropic-version" = "2023-06-01", "content-type" = "application/json"),
      body = toJSON(body, auto_unbox = TRUE),
      timeout(15)
    )
    if (status_code(resp) != 200) {
      cat("    Status:", status_code(resp), "\n")
      return(NULL)
    }
    data  <- fromJSON(content(resp, "text"))
    tekst <- gsub("```json|```", "", data$content$text[1])
    return(fromJSON(trimws(tekst)))
  }, error = function(e) {
    cat("    Fout:", conditionMessage(e), "\n")
    return(NULL)
  })
}

# ============================================================
# STAP 4: ALLE ARTIKELEN BEOORDELEN
# ============================================================

cat("Claude beoordeelt", nrow(alle_artikelen), "artikelen...\n\n")

nieuwe_events <- list()

for (i in 1:nrow(alle_artikelen)) {
  titel        <- alle_artikelen$title[i]
  beschrijving <- alle_artikelen$description[i]
  pub_date     <- alle_artikelen$pub_date[i]
  link         <- alle_artikelen$link[i]
  source       <- alle_artikelen$source[i]

  cat("  [", i, "/", nrow(alle_artikelen), "]", substr(titel, 1, 60), "...\n")

  result <- beoordeel_artikel(titel, beschrijving, ANTHROPIC_KEY)

  if (is.null(result))                                      { Sys.sleep(0.5); next }
  if (!isTRUE(result$is_esg))                              { cat("    Geen ESG event\n"); Sys.sleep(0.5); next }
  if (is.null(result$bedrijf) || result$bedrijf == "null") { Sys.sleep(0.5); next }

  ticker <- if (!is.null(result$ticker) && result$ticker != "null") result$ticker else NA

  nieuwe_events[[i]] <- data.frame(
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

  cat("    ESG event:", result$bedrijf, "(", ticker, ") | Pillar:", result$pillar, "| Severity:", result$severity, "\n")
  Sys.sleep(0.5)
}

# ============================================================
# STAP 5: SAMENVOEGEN MET BESTAANDE EVENTS
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

  write.csv(gecombineerd, file.path(pad_data, "events_detected.csv"), row.names = FALSE)
  cat("\nTotaal events in database:", nrow(gecombineerd), "\n")
  cat("events_detected.csv bijgewerkt\n")
}

cat("\n--- AI AGENT KLAAR ---", format(Sys.time(), "%d-%m-%Y %H:%M"), "\n")
