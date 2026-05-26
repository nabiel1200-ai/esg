# ============================================================
# ESG_Alpha Live — 02_ai_agent.R
# NewsAPI als primaire bron + Google News RSS als backup
# Nabiel Mamnoen
# ============================================================

library(dplyr)
library(httr)
library(jsonlite)
library(xml2)

ANTHROPIC_KEY <- Sys.getenv("ANTHROPIC_KEY")
NEWS_API_KEY  <- Sys.getenv("NEWS_API_KEY")
pad_data      <- "data"

# ============================================================
# FALSE POSITIVE FILTER
# ============================================================

false_positive_keywords <- c(
  # Advocatenkantoor berichten
  "investors have opportunity to lead",
  "shareholders who lost money",
  "claimsfiler",
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
  "deadline alert",
  "investor notice",
  "shareholder alert",
  "law offices of",
  "investors urged to contact",
  "investors may seek to lead",
  "investors with losses",
  "lead plaintiff deadline",
  "securities class action",
  "class action lawsuit",
  "remind investors",
  "encourage investors",
  "urges investors",
  "if you purchased shares",
  "recover losses",
  "pursuing claims",
  "securities fraud lawsuit",
  "securities litigation",
  "investor rights",
  "filed a class action",
  "investigation on behalf",
  "announces investigation",
  "notifies investors",
  # Earnings / financieel
  "quarterly earnings",
  "q1 results", "q2 results", "q3 results", "q4 results",
  "earnings per share", "eps beat", "eps miss",
  "revenue guidance",
  "raised its outlook",
  "dividend declared",
  "stock buyback",
  "share repurchase",
  "analyst rating",
  "price target",
  "upgrade", "downgrade",
  # Overige ruis
  "bitcoin", "crypto", "blockchain",
  "merger", "acquisition", "ipo",
  "how to claim", "how to file a claim",
  "premier league", "nfl", "nba"
)

is_false_positive <- function(title) {
  t_lower <- tolower(title)
  any(sapply(false_positive_keywords, function(k) grepl(k, t_lower, fixed = TRUE)))
}

# ============================================================
# BRON 1: NEWSAPI (primaire bron)
# ============================================================

newsapi_queries <- c(
  "company environmental fine penalty EPA",
  "oil spill chemical leak contamination",
  "factory pollution air water violation",
  "workplace harassment discrimination settlement",
  "corporate data breach privacy violation",
  "child labor supply chain violation",
  "factory safety workers killed injured",
  "executive bribery corruption arrested",
  "CEO fraud accounting scandal",
  "insider trading executive charged",
  "company misconduct fine court ruling",
  "corporate human rights violation",
  "toxic waste dumping company fined",
  "environmental crime company prosecuted",
  "labor abuse workers exploitation company"
)

fetch_newsapi <- function(query, api_key) {
  tryCatch({
    query_enc <- utils::URLencode(query)
    url <- paste0(
      "https://newsapi.org/v2/everything?",
      "q=", query_enc,
      "&language=en",
      "&sortBy=publishedAt",
      "&pageSize=20",
      "&from=", format(Sys.Date() - 3),
      "&apiKey=", api_key
    )

    resp <- GET(url, timeout(15))

    if (status_code(resp) != 200) {
      cat("  NewsAPI fout status:", status_code(resp), "\n")
      return(NULL)
    }

    data <- fromJSON(httr::content(resp, "text", encoding = "UTF-8"))

    if (is.null(data$articles) || nrow(data$articles) == 0) return(NULL)

    articles <- data$articles %>%
      mutate(
        pub_date    = as.Date(substr(publishedAt, 1, 10)),
        description = ifelse(is.na(description), "", substr(description, 1, 300)),
        source_name = ifelse(is.null(source$name), "newsapi", source$name)
      ) %>%
      filter(!is.na(pub_date), !is.na(title), title != "[Removed]") %>%
      select(
        title,
        description,
        pub_date,
        link   = url,
        source = source_name
      )

    return(articles)

  }, error = function(e) {
    cat("  NewsAPI fout:", conditionMessage(e), "\n")
    return(NULL)
  })
}

# ============================================================
# BRON 2: GOOGLE NEWS RSS (backup)
# ============================================================

google_queries <- c(
  "company environmental fine EPA penalty",
  "oil spill chemical leak contamination company",
  "workplace harassment discrimination settlement company",
  "corporate data breach privacy scandal",
  "executive bribery corruption arrested company",
  "company scandal misconduct fine court ruling",
  "corporate human rights violation labor abuse"
)

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

    xml   <- read_xml(httr::content(resp, "text", encoding = "UTF-8"))
    items <- xml_find_all(xml, "//item")
    if (length(items) == 0) return(NULL)

    titels <- xml_text(xml_find_first(items, "title"))
    datums <- xml_text(xml_find_first(items, "pubDate"))
    links  <- xml_text(xml_find_first(items, "link"))

    df <- data.frame(
      title       = titels,
      description = "",
      pub_date    = as.Date(sub(" GMT| UTC", "", datums),
                            format = "%a, %d %b %Y %H:%M:%S"),
      link        = links,
      source      = "google_news",
      stringsAsFactors = FALSE
    ) %>% filter(!is.na(pub_date), !is.na(title), title != "")

    return(df)

  }, error = function(e) {
    cat("  Google News fout:", conditionMessage(e), "\n")
    return(NULL)
  })
}

# ============================================================
# STAP 1: NIEUWS OPHALEN
# ============================================================

cat("=== ESG_Alpha AI Agent gestart ===\n")
cat("Tijdstip:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n\n")

all_articles <- list()

# NewsAPI
cat("NewsAPI ophalen...\n")
for (q in newsapi_queries) {
  result <- fetch_newsapi(q, NEWS_API_KEY)
  if (!is.null(result) && nrow(result) > 0) {
    all_articles[[paste0("newsapi_", q)]] <- result
    cat("  Query '", substr(q, 1, 40), "': ", nrow(result), " artikelen\n", sep="")
  }
  Sys.sleep(0.3)
}

newsapi_count <- sum(sapply(all_articles, nrow))
cat("NewsAPI totaal:", newsapi_count, "artikelen\n\n")

# Google News RSS als backup
cat("Google News RSS ophalen...\n")
for (q in google_queries) {
  result <- fetch_google_news(q)
  if (!is.null(result)) {
    result <- result %>% filter(pub_date >= Sys.Date() - 3)
    if (nrow(result) > 0) {
      all_articles[[paste0("google_", q)]] <- result
    }
  }
  Sys.sleep(0.3)
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
  filter(pub_date >= Sys.Date() - 3) %>%
  filter(!sapply(title, is_false_positive))

cat("\nArtikelen na filtering:\n")
cat("  Totaal uniek:  ", nrow(alle_artikelen), "\n")
cat("  NewsAPI:       ", sum(grepl("^Reuters|^AP|^BBC|^CNN|newsapi", alle_artikelen$source, ignore.case=TRUE)), "\n")
cat("  Google News:   ", sum(alle_artikelen$source == "google_news"), "\n\n")

if (nrow(alle_artikelen) == 0) {
  cat("Geen artikelen over na filtering\n")
  quit(status = 0)
}

# ============================================================
# STAP 3: CLAUDE BEOORDEELT ELK ARTIKEL
# ============================================================

beoordeel_artikel <- function(titel, beschrijving, api_key) {

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
    "1. Gaat dit over een ESG controversy voor een BEURSGENOTEERD bedrijf?\n",
    "   - Alleen bedrijven die op een beurs verhandeld worden (NYSE, NASDAQ, AEX etc.)\n",
    "   - Overheidsinstanties, NGOs en privébedrijven tellen NIET\n",
    "2. Welk beursgenoteerd bedrijf?\n",
    "3. Wat is de beursticker? Geef de echte ticker (bijv. AAPL voor Apple, XOM voor ExxonMobil)\n",
    "4. Pillar: E (Environmental), S (Social), G (Governance), Cross (meerdere pillars)\n",
    "5. Severity: 1 (laag), 2 (midden), 3 (hoog)\n",
    "   - Severity 3: crimineel, miljarden, doden, federaal onderzoek\n",
    "   - Severity 2: boete, rechtszaak, settlement, onderzoek\n",
    "   - Severity 1: klacht, beschuldiging, kleine overtreding\n\n",
    "Voorbeelden van correcte output:\n",
    "{\"is_esg\": true, \"bedrijf\": \"Apple Inc\", \"ticker\": \"AAPL\", \"pillar\": \"S\", \"severity\": 2}\n",
    "{\"is_esg\": true, \"bedrijf\": \"ExxonMobil\", \"ticker\": \"XOM\", \"pillar\": \"E\", \"severity\": 3}\n",
    "{\"is_esg\": false, \"bedrijf\": null, \"ticker\": null, \"pillar\": null, \"severity\": null}\n\n",
    "Antwoord ALLEEN in JSON formaat zonder extra tekst of uitleg."
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

    if (status_code(resp) != 200) {
      cat("    Anthropic API fout:", status_code(resp), "\n")
      return(NULL)
    }

    data  <- fromJSON(httr::content(resp, "text", encoding = "UTF-8"))
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

  if (is.null(result))         { Sys.sleep(0.5); next }
  if (!isTRUE(result$is_esg)) { cat("    Geen ESG event\n"); Sys.sleep(0.5); next }
  if (is.null(result$bedrijf) || identical(result$bedrijf, "null") ||
      is.na(result$bedrijf))   { Sys.sleep(0.5); next }

  # Ticker validatie
  ticker_raw <- result$ticker
  ticker <- if (!is.null(ticker_raw) &&
                !is.na(ticker_raw) &&
                !identical(ticker_raw, "null") &&
                nchar(trimws(ticker_raw)) >= 1 &&
                nchar(trimws(ticker_raw)) <= 6 &&
                grepl("^[A-Z]+$", trimws(ticker_raw))) {
    trimws(ticker_raw)
  } else {
    NA
  }

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
    severity    = as.integer(pmin(as.integer(result$severity), 3)),
    scraped_at  = as.numeric(Sys.Date()),
    stringsAsFactors = FALSE
  )

  nieuwe_events[[length(nieuwe_events) + 1]] <- event
  cat("    ✓ ESG event:", result$bedrijf,
      "| Ticker:", ifelse(is.na(ticker), "onbekend", ticker),
      "| Pillar:", result$pillar,
      "| Severity:", result$severity, "\n")

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
  cat("Gevonden:", nrow(nieuwe_df), "\n")
  cat("Met ticker:", sum(!is.na(nieuwe_df$ticker)), "\n\n")
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
