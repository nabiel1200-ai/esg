# ============================================================
# ESG_Alpha Live — 02_ai_agent.R
# NewsAPI + Guardian API + Google News RSS backup
# Nabiel Mamnoen
# ============================================================

library(dplyr)
library(httr)
library(jsonlite)
library(xml2)

ANTHROPIC_KEY <- Sys.getenv("ANTHROPIC_KEY")
NEWS_API_KEY  <- Sys.getenv("NEWS_API_KEY")
GUARDIAN_KEY  <- Sys.getenv("GUARDIAN_KEY")
pad_data      <- "data"

# ============================================================
# FALSE POSITIVE FILTER
# ============================================================

false_positive_keywords <- c(
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
  "quarterly earnings",
  "q1 results", "q2 results", "q3 results", "q4 results",
  "earnings per share", "eps beat", "eps miss",
  "revenue guidance", "raised its outlook",
  "dividend declared", "stock buyback", "share repurchase",
  "analyst rating", "price target",
  "bitcoin", "crypto", "blockchain",
  "merger", "acquisition", "ipo",
  "premier league", "nfl", "nba", "champions league",
  "how to claim", "how to file a claim"
)

is_false_positive <- function(title) {
  t_lower <- tolower(title)
  any(sapply(false_positive_keywords, function(k) grepl(k, t_lower, fixed = TRUE)))
}

# ============================================================
# BRON 1: NEWSAPI
# ============================================================

newsapi_queries <- c(
  "company environmental fine penalty EPA",
  "oil spill chemical leak contamination company",
  "factory pollution air water violation fine",
  "toxic waste dumping company fined prosecuted",
  "environmental crime company EPA charged",
  "carbon emissions violation company fined",
  "deforestation company illegal logging",
  "water pollution company fine settlement",
  "hazardous waste company illegal disposal",
  "greenhouse gas violation company penalty",
  "mining company environmental damage fine",
  "pipeline leak oil company spill",
  "pesticide contamination company lawsuit",
  "nuclear waste company violation fine",
  "air quality violation company factory",
  "plastic pollution company environmental fine",
  "wildlife habitat destruction company illegal",
  "soil contamination company toxic fine",
  "environmental permit violation company",
  "clean air act violation company fined",
  "workplace harassment discrimination settlement",
  "factory safety workers killed injured company",
  "child labor supply chain violation company",
  "corporate human rights violation labor abuse",
  "labor abuse workers exploitation company",
  "wage theft company workers lawsuit settlement",
  "unsafe working conditions company OSHA fine",
  "forced labor supply chain company exposed",
  "gender discrimination company lawsuit settlement",
  "racial discrimination company lawsuit settlement",
  "employee death workplace company negligence",
  "sexual harassment CEO executive company",
  "workers rights violation company strike",
  "modern slavery company supply chain",
  "health safety violation company workers fine",
  "disability discrimination company lawsuit",
  "whistleblower retaliation company lawsuit",
  "union busting company workers rights",
  "sweatshop company supply chain exposed",
  "immigrant workers exploitation company raid",
  "executive bribery corruption arrested company",
  "CEO fraud accounting scandal company",
  "insider trading executive charged company",
  "corporate fraud SEC investigation company",
  "accounting irregularities company SEC fine",
  "money laundering company executive arrested",
  "board misconduct company shareholders lawsuit",
  "corporate tax fraud company investigation",
  "CEO fired misconduct company scandal",
  "antitrust violation company fined DOJ",
  "price fixing company cartel fine",
  "bribery company foreign officials FCPA",
  "market manipulation company SEC charged",
  "false statements company SEC fine",
  "misleading investors company SEC enforcement",
  "company scandal misconduct fine court ruling",
  "corporate controversy company sued fined",
  "regulatory enforcement action company fined",
  "company under investigation misconduct",
  "corporate penalty court ruling company",
  "company settlement federal investigation",
  "company charged violation federal law",
  "bank money laundering fine settlement",
  "pharmaceutical company bribery fine FDA",
  "tech company privacy data violation fine",
  "energy company spill fine EPA penalty",
  "food company contamination recall safety",
  "auto company emissions scandal fine",
  "chemical company toxic spill fine",
  "retail company labor violation fine",
  "airline company safety violation fine",
  "defense company bribery corruption fine",
  "telecom company privacy violation fine",
  "social media company data privacy fine",
  "clothing brand child labor supply chain",
  "electronics company labor abuse factory",
  "fast fashion brand sweatshop labor abuse"
)

fetch_newsapi <- function(query, api_key) {
  tryCatch({
    query_enc <- utils::URLencode(query)
    url <- paste0(
      "https://newsapi.org/v2/everything?",
      "q=", query_enc,
      "&language=en",
      "&sortBy=publishedAt",
      "&pageSize=10",
      "&from=", format(Sys.Date() - 3),
      "&apiKey=", api_key
    )

    resp <- GET(url, timeout(15))
    if (status_code(resp) != 200) return(NULL)

    data <- fromJSON(httr::content(resp, "text", encoding = "UTF-8"), flatten = TRUE)
    if (is.null(data$articles) || length(data$articles) == 0) return(NULL)

    arts <- data$articles
    title_col <- if ("title"       %in% names(arts)) arts$title       else rep(NA,       nrow(arts))
    desc_col  <- if ("description" %in% names(arts)) arts$description else rep("",       nrow(arts))
    date_col  <- if ("publishedAt" %in% names(arts)) arts$publishedAt else rep(NA,       nrow(arts))
    url_col   <- if ("url"         %in% names(arts)) arts$url         else rep(NA,       nrow(arts))
    src_col   <- if ("source.name" %in% names(arts)) arts$source.name else rep("newsapi", nrow(arts))

    df <- data.frame(
      title       = as.character(title_col),
      description = substr(ifelse(is.na(as.character(desc_col)), "", as.character(desc_col)), 1, 300),
      pub_date    = as.Date(substr(as.character(date_col), 1, 10)),
      link        = as.character(url_col),
      source      = as.character(ifelse(is.na(src_col), "newsapi", src_col)),
      stringsAsFactors = FALSE
    ) %>% filter(!is.na(pub_date), !is.na(title), title != "[Removed]", title != "NA")

    if (nrow(df) == 0) return(NULL)
    return(df)

  }, error = function(e) {
    cat("  NewsAPI fout:", conditionMessage(e), "\n")
    return(NULL)
  })
}

# ============================================================
# BRON 2: THE GUARDIAN API (onbeperkt gratis)
# ============================================================

guardian_queries <- c(
  "company environmental fine pollution",
  "corporate fraud corruption bribery",
  "workplace harassment discrimination",
  "data breach privacy violation company",
  "child labour supply chain",
  "oil spill environmental damage",
  "workers rights violation company",
  "corporate tax evasion fraud",
  "human rights violation company",
  "company safety scandal workers",
  "environmental violation company penalty",
  "corporate misconduct scandal",
  "company fined regulatory breach",
  "executive misconduct company",
  "labor abuse exploitation company"
)

fetch_guardian <- function(query, api_key) {
  tryCatch({
    query_enc <- utils::URLencode(query)
    url <- paste0(
      "https://content.guardianapis.com/search?",
      "q=", query_enc,
      "&from-date=", format(Sys.Date() - 3),
      "&lang=en",
      "&page-size=20",
      "&order-by=newest",
      "&show-fields=trailText",
      "&api-key=", api_key
    )

    resp <- GET(url, timeout(15))
    if (status_code(resp) != 200) return(NULL)

    data <- fromJSON(httr::content(resp, "text", encoding = "UTF-8"), flatten = TRUE)
    results <- data$response$results
    if (is.null(results) || nrow(results) == 0) return(NULL)

    df <- data.frame(
      title       = as.character(results$webTitle),
      description = substr(ifelse(is.na(results$fields.trailText), "",
                                  gsub("<[^>]+>", "", results$fields.trailText)), 1, 300),
      pub_date    = as.Date(substr(results$webPublicationDate, 1, 10)),
      link        = as.character(results$webUrl),
      source      = "The Guardian",
      stringsAsFactors = FALSE
    ) %>% filter(!is.na(pub_date), !is.na(title), title != "")

    if (nrow(df) == 0) return(NULL)
    return(df)

  }, error = function(e) {
    cat("  Guardian fout:", conditionMessage(e), "\n")
    return(NULL)
  })
}

# ============================================================
# BRON 3: GOOGLE NEWS RSS (backup)
# ============================================================

google_queries <- c(
  "company environmental fine EPA penalty",
  "workplace harassment discrimination settlement company",
  "executive bribery corruption arrested company",
  "corporate data breach privacy violation company",
  "company misconduct fine court ruling",
  "corporate human rights labor violation company",
  "factory safety workers killed injured company"
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
  }, error = function(e) NULL)
}

# ============================================================
# STAP 1: NIEUWS OPHALEN
# ============================================================

cat("=== ESG_Alpha AI Agent gestart ===\n")
cat("Tijdstip:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n\n")

all_articles <- list()

# NewsAPI
cat("NewsAPI ophalen (", length(newsapi_queries), " queries)...\n", sep="")
for (q in newsapi_queries) {
  result <- fetch_newsapi(q, NEWS_API_KEY)
  if (!is.null(result) && nrow(result) > 0) {
    all_articles[[paste0("newsapi_", q)]] <- result
    cat("  '", substr(q, 1, 45), "': ", nrow(result), "\n", sep="")
  }
  Sys.sleep(0.3)
}
newsapi_count <- if (length(all_articles) > 0) sum(sapply(all_articles, nrow)) else 0
cat("NewsAPI totaal:", newsapi_count, "artikelen\n\n")

# Guardian API
cat("Guardian API ophalen (", length(guardian_queries), " queries)...\n", sep="")
guardian_start <- length(all_articles)
for (q in guardian_queries) {
  result <- fetch_guardian(q, GUARDIAN_KEY)
  if (!is.null(result) && nrow(result) > 0) {
    all_articles[[paste0("guardian_", q)]] <- result
    cat("  '", substr(q, 1, 45), "': ", nrow(result), "\n", sep="")
  }
  Sys.sleep(0.3)
}
guardian_count <- if (length(all_articles) > guardian_start)
  sum(sapply(all_articles[(guardian_start+1):length(all_articles)], nrow)) else 0
cat("Guardian totaal:", guardian_count, "artikelen\n\n")

# Google News RSS
cat("Google News RSS ophalen...\n")
for (q in google_queries) {
  result <- fetch_google_news(q)
  if (!is.null(result)) {
    result <- result %>% filter(pub_date >= Sys.Date() - 3)
    if (nrow(result) > 0) all_articles[[paste0("google_", q)]] <- result
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
cat("  Totaal uniek:", nrow(alle_artikelen), "\n")
cat("  Guardian:    ", sum(alle_artikelen$source == "The Guardian"), "\n")
cat("  Google News: ", sum(alle_artikelen$source == "google_news"), "\n\n")

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
  } else ""

  prompt <- paste0(
    "Je bent een ESG analist. Beoordeel het volgende nieuwsartikel:\n\n",
    "TITEL: ", titel, "\n\n",
    context,
    "Beantwoord deze vragen:\n",
    "1. Gaat dit over een ESG controversy voor een BEURSGENOTEERD bedrijf?\n",
    "   - Alleen bedrijven op NYSE, NASDAQ, AEX, LSE, etc.\n",
    "   - Overheid, NGOs, privébedrijven tellen NIET\n",
    "2. Welk beursgenoteerd bedrijf?\n",
    "3. Wat is de beursticker? (echte ticker, bijv. AAPL, XOM, SHEL)\n",
    "4. Pillar: E (Environmental), S (Social), G (Governance), Cross (meerdere)\n",
    "5. Severity: 1=laag, 2=midden, 3=hoog\n\n",
    "Voorbeelden:\n",
    "{\"is_esg\": true, \"bedrijf\": \"Apple Inc\", \"ticker\": \"AAPL\", \"pillar\": \"S\", \"severity\": 2}\n",
    "{\"is_esg\": true, \"bedrijf\": \"ExxonMobil\", \"ticker\": \"XOM\", \"pillar\": \"E\", \"severity\": 3}\n",
    "{\"is_esg\": false, \"bedrijf\": null, \"ticker\": null, \"pillar\": null, \"severity\": null}\n\n",
    "Antwoord ALLEEN in JSON, geen extra tekst."
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
      cat("    Anthropic fout:", status_code(resp), "\n")
      return(NULL)
    }

    data  <- fromJSON(httr::content(resp, "text", encoding = "UTF-8"))
    tekst <- data$content$text[1]
    tekst <- gsub("```json|```", "", tekst)
    result <- fromJSON(trimws(tekst))
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

  if (is.null(result)) { Sys.sleep(0.5); next }
  if (!isTRUE(result$is_esg)) { cat("    Geen ESG event\n"); Sys.sleep(0.5); next }

  # Veilig waarden extraheren
  safe_val <- function(x, default = NA_character_) {
    if (is.null(x) || length(x) == 0) return(default)
    v <- as.character(x[1])
    if (is.na(v) || v == "null" || v == "NULL") return(default)
    v
  }

  bedrijf  <- safe_val(result$bedrijf)
  pillar   <- safe_val(result$pillar)
  sev_raw  <- safe_val(result$severity, default = "1")

  if (is.na(bedrijf) || is.na(pillar)) { Sys.sleep(0.5); next }

  ticker_raw <- safe_val(result$ticker)
  ticker <- if (!is.na(ticker_raw) &&
                nchar(trimws(ticker_raw)) >= 1 &&
                nchar(trimws(ticker_raw)) <= 6 &&
                grepl("^[A-Z.]+$", trimws(ticker_raw))) {
    trimws(ticker_raw)
  } else NA_character_

  severity_int <- suppressWarnings(as.integer(sev_raw))
  if (is.na(severity_int) || severity_int < 1) severity_int <- 1L
  if (severity_int > 3) severity_int <- 3L

  event <- data.frame(
    isin        = NA_character_,
    company     = bedrijf,
    ticker      = ticker,
    title       = as.character(titel),
    description = as.character(beschrijving),
    pub_date    = pub_date,
    link        = as.character(link),
    source      = as.character(source),
    pillar      = pillar,
    severity    = severity_int,
    scraped_at  = as.numeric(Sys.Date()),
    stringsAsFactors = FALSE
  )

  nieuwe_events[[length(nieuwe_events) + 1]] <- event
  cat("    ✓", result$bedrijf,
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
