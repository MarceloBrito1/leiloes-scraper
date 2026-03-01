#!/usr/bin/env Rscript

`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0 || (length(x) == 1 && is.na(x))) y else x
}

normalize_ws <- function(x) {
  if (length(x) == 0 || is.na(x)) return(NA_character_)
  y <- stringr::str_squish(trimws(as.character(x)))
  if (!nzchar(y)) NA_character_ else y
}

safe_text <- function(node) {
  if (length(node) == 0 || inherits(node, "xml_missing")) return(NA_character_)
  normalize_ws(xml2::xml_text(node))
}

safe_text_vec <- function(nodes) {
  if (length(nodes) == 0) return(character(0))
  out <- vapply(nodes, safe_text, character(1))
  out[!is.na(out) & nzchar(out)]
}

safe_attr <- function(node, attr) {
  if (length(node) == 0 || inherits(node, "xml_missing")) return(NA_character_)
  normalize_ws(xml2::xml_attr(node, attr))
}

empty_results <- function() {
  data.frame(
    site = character(),
    listing_id = character(),
    title = character(),
    property_type = character(),
    city_uf = character(),
    address = character(),
    area_info = character(),
    auction_type = character(),
    lot = character(),
    status = character(),
    price_1 = numeric(),
    price_2 = numeric(),
    min_price = numeric(),
    auction_date_1 = character(),
    auction_date_2 = character(),
    auction_datetime_1 = character(),
    auction_datetime_2 = character(),
    image_url = character(),
    listing_url = character(),
    source_url = character(),
    scraped_at = character(),
    stringsAsFactors = FALSE
  )
}

rbind_safe <- function(a, b) {
  if (nrow(a) == 0) return(b)
  if (nrow(b) == 0) return(a)
  rbind(a, b)
}

dedupe_results <- function(df) {
  if (nrow(df) == 0) return(df)
  key <- paste(
    df$site,
    ifelse(is.na(df$listing_id), "", df$listing_id),
    ifelse(is.na(df$listing_url), "", df$listing_url),
    sep = "|"
  )
  df[!duplicated(key), , drop = FALSE]
}

parse_money_br <- function(x) {
  x <- normalize_ws(x)
  if (is.na(x)) return(NA_real_)
  m <- stringr::str_extract(x, "\\d{1,3}(?:\\.\\d{3})*,\\d{2}")
  y <- if (!is.na(m)) m else gsub("[^0-9,.-]", "", x)
  if (!nzchar(y)) return(NA_real_)
  y <- gsub(".", "", y, fixed = TRUE)
  y <- sub(",", ".", y, fixed = TRUE)
  suppressWarnings(as.numeric(y))
}

parse_datetime_br <- function(x) {
  x <- normalize_ws(x)
  if (is.na(x)) return(as.POSIXct(NA))
  m <- stringr::str_match(x, "(\\d{2}/\\d{2}/\\d{4})\\s*(?:as|ÃƒÂ s)\\s*(\\d{2}:\\d{2})")
  if (all(is.na(m))) return(as.POSIXct(NA))
  dt <- paste(m[, 2], m[, 3])
  as.POSIXct(dt, format = "%d/%m/%Y %H:%M", tz = "America/Sao_Paulo")
}

fmt_datetime <- function(x) {
  if (length(x) == 0 || is.na(x)) return(NA_character_)
  format(x, "%Y-%m-%d %H:%M:%S %z")
}

parse_query_value <- function(url, key) {
  p <- httr::parse_url(url)
  p$query[[key]] %||% NA_character_
}

set_query_value <- function(url, key, value) {
  p <- httr::parse_url(url)
  p$query[[key]] <- as.character(value)
  httr::build_url(p)
}

make_absolute_url <- function(base_url, href) {
  href <- normalize_ws(href)
  if (is.na(href)) return(NA_character_)
  if (grepl("^https?://", href, ignore.case = TRUE)) return(href)

  b <- httr::parse_url(base_url)
  if (is.null(b$scheme) || is.null(b$hostname)) return(href)
  origin <- paste0(b$scheme, "://", b$hostname)
  if (!is.null(b$port)) origin <- paste0(origin, ":", b$port)

  if (startsWith(href, "/")) {
    paste0(origin, href)
  } else {
    path <- b$path %||% ""
    dir <- sub("/[^/]*$", "/", path)
    paste0(origin, dir, href)
  }
}

extract_id_from_url <- function(url) {
  url <- normalize_ws(url)
  if (is.na(url)) return(NA_character_)
  segment <- sub("\\?.*$", "", basename(url))
  m1 <- stringr::str_match(segment, "-(\\d+)$")
  if (!is.na(m1[1, 2])) return(m1[1, 2])
  m2 <- stringr::str_match(segment, "([A-Za-z]\\d+)$")
  if (!is.na(m2[1, 2])) return(toupper(m2[1, 2]))
  m3 <- stringr::str_match(segment, "(\\d+)$")
  if (!is.na(m3[1, 2])) return(m3[1, 2])
  NA_character_
}

log_msg <- function(..., verbose = TRUE) {
  if (isTRUE(verbose)) {
    cat(sprintf("[%s] ", format(Sys.time(), "%Y-%m-%d %H:%M:%S")), ..., "\n", sep = "")
  }
}

fetch_html <- function(url, user_agent, timeout_sec = 60, headers = list(), verbose = FALSE) {
  log_msg("GET ", url, verbose = verbose)
  h <- unlist(headers, use.names = TRUE)
  h <- h[!is.na(h) & nzchar(h)]
  args <- list(
    url,
    httr::user_agent(user_agent),
    httr::timeout(timeout_sec),
    httr::accept("text/html,application/xhtml+xml,*/*")
  )
  if (length(h) > 0) {
    args <- c(args, list(httr::add_headers(.headers = h)))
  }
  req <- do.call(httr::GET, args)
  httr::stop_for_status(req)
  txt <- httr::content(req, as = "text", encoding = "UTF-8")
  doc <- xml2::read_html(txt, options = c("RECOVER", "NOERROR", "NOWARNING"))
  list(resp = req, text = txt, doc = doc)
}

post_form <- function(url, form, user_agent, timeout_sec = 60, headers = list(), verbose = FALSE) {
  log_msg("POST ", url, verbose = verbose)
  h <- unlist(headers, use.names = TRUE)
  h <- h[!is.na(h) & nzchar(h)]
  args <- list(
    url,
    httr::user_agent(user_agent),
    httr::timeout(timeout_sec),
    body = form,
    encode = "form"
  )
  if (length(h) > 0) {
    args <- c(args, list(httr::add_headers(.headers = h)))
  }
  req <- do.call(httr::POST, args)
  httr::stop_for_status(req)
  txt <- httr::content(req, as = "text", encoding = "UTF-8")
  list(resp = req, text = txt)
}

normalize_site <- function(site) {
  s <- tolower(normalize_ws(site) %||% "auto")
  s <- gsub("ÃƒÂ£", "a", s, fixed = TRUE)
  s <- gsub("ÃƒÂ©", "e", s, fixed = TRUE)
  s <- gsub("ÃƒÂ§", "c", s, fixed = TRUE)
  s <- gsub("ÃƒÂµ", "o", s, fixed = TRUE)
  s <- gsub("-", "", s, fixed = TRUE)
  s <- gsub("_", "", s, fixed = TRUE)
  if (s %in% c("auto")) return("auto")
  if (s %in% c("zuk", "portalzuk")) return("zuk")
  if (s %in% c("mega", "megaleiloes", "megaleiloescombr")) return("megaleiloes")
  if (s %in% c("leeilon", "leeiloncombr")) return("leeilon")
  NA_character_
}

detect_site_from_url <- function(url) {
  host <- tolower(httr::parse_url(url)$hostname %||% "")
  if (grepl("portalzuk\\.com\\.br$", host)) return("zuk")
  if (grepl("megaleiloes\\.com\\.br$", host)) return("megaleiloes")
  if (grepl("leeilon\\.com\\.br$", host)) return("leeilon")
  NA_character_
}

extract_zuk_cards <- function(doc, source_url) {
  cards <- xml2::xml_find_all(
    doc,
    ".//div[contains(concat(' ', normalize-space(@class), ' '), ' card-property ') and contains(concat(' ', normalize-space(@class), ' '), ' card_lotes_div ')]"
  )
  if (length(cards) == 0) return(empty_results())

  rows <- vector("list", length(cards))
  scraped_at <- format(Sys.time(), "%Y-%m-%d %H:%M:%S %z")

  for (i in seq_along(cards)) {
    card <- cards[[i]]
    fav <- xml2::xml_find_first(
      card,
      ".//span[contains(concat(' ', normalize-space(@class), ' '), ' card-property-favorite ') and contains(concat(' ', normalize-space(@class), ' '), ' star ')]"
    )
    listing_id <- safe_attr(fav, "id")

    link_node <- xml2::xml_find_first(card, ".//div[contains(@class,'card-property-image-wrapper')]//a[1]")
    href <- safe_attr(link_node, "href")
    listing_url <- make_absolute_url(source_url, href)
    title <- safe_attr(link_node, "title")
    property_type <- safe_text(xml2::xml_find_first(card, ".//span[contains(@class,'card-property-price-lote')][1]"))

    addr_spans <- safe_text_vec(xml2::xml_find_all(card, ".//address[contains(@class,'card-property-address')]//span"))
    city_uf <- if (length(addr_spans) >= 1) addr_spans[[1]] else NA_character_
    address <- if (length(addr_spans) > 0) paste(addr_spans, collapse = " | ") else NA_character_

    info <- safe_text_vec(xml2::xml_find_all(card, ".//ul[contains(@class,'card-property-infos')]//span[contains(@class,'card-property-info-label')]"))
    area_info <- if (length(info) > 0) paste(info, collapse = " | ") else NA_character_

    image_url <- safe_attr(xml2::xml_find_first(card, ".//div[contains(@class,'card-property-image-wrapper')]//img[1]"), "src")

    price_groups <- xml2::xml_find_all(card, ".//div[contains(@class,'card-property-content')]/ul[contains(@class,'card-property-prices')]")
    price_items <- if (length(price_groups) > 0) {
      xml2::xml_find_all(price_groups[[length(price_groups)]], ".//li[contains(@class,'card-property-price')]")
    } else {
      list()
    }

    price_values <- character(0)
    price_dates <- character(0)
    if (length(price_items) > 0) {
      price_values <- vapply(
        price_items,
        function(x) safe_text(xml2::xml_find_first(x, ".//span[contains(@class,'card-property-price-value')][1]")),
        character(1)
      )
      price_dates <- vapply(
        price_items,
        function(x) safe_text(xml2::xml_find_first(x, ".//span[contains(@class,'card-property-price-data')][1]")),
        character(1)
      )
    }

    price_1 <- parse_money_br(price_values[1] %||% NA_character_)
    price_2 <- parse_money_br(price_values[2] %||% NA_character_)
    min_price <- suppressWarnings(min(c(price_1, price_2), na.rm = TRUE))
    if (!is.finite(min_price)) min_price <- NA_real_

    auction_date_1 <- normalize_ws(price_dates[1] %||% NA_character_)
    auction_date_2 <- normalize_ws(price_dates[2] %||% NA_character_)
    dt1 <- parse_datetime_br(auction_date_1)
    dt2 <- parse_datetime_br(auction_date_2)

    if (is.na(listing_id)) listing_id <- extract_id_from_url(listing_url)
    if (is.na(title)) title <- property_type %||% city_uf

    rows[[i]] <- data.frame(
      site = "zuk",
      listing_id = listing_id,
      title = title,
      property_type = property_type,
      city_uf = city_uf,
      address = address,
      area_info = area_info,
      auction_type = NA_character_,
      lot = NA_character_,
      status = NA_character_,
      price_1 = price_1,
      price_2 = price_2,
      min_price = min_price,
      auction_date_1 = auction_date_1,
      auction_date_2 = auction_date_2,
      auction_datetime_1 = fmt_datetime(dt1),
      auction_datetime_2 = fmt_datetime(dt2),
      image_url = image_url,
      listing_url = listing_url,
      source_url = source_url,
      scraped_at = scraped_at,
      stringsAsFactors = FALSE
    )
  }

  out <- do.call(rbind, rows)
  dedupe_results(out)
}

extract_zuk_token <- function(doc) {
  safe_attr(xml2::xml_find_first(doc, ".//input[@name='_token'][1]"), "value")
}

extract_zuk_order <- function(doc) {
  selected <- safe_attr(xml2::xml_find_first(doc, ".//select[@id='ordenar']/option[@selected][1]"), "value")
  if (!is.na(selected)) return(selected)
  safe_attr(xml2::xml_find_first(doc, ".//select[@id='ordenar']/option[1]"), "value")
}

extract_zuk_total <- function(html_text) {
  m1 <- stringr::str_match(html_text, "var\\s+countLotes\\s*=\\s*\"?(\\d+)\"?")
  m2 <- stringr::str_match(html_text, "var\\s+count_lotes_zuk\\s*=\\s*\"?(\\d+)\"?")
  v <- suppressWarnings(as.integer(c(m1[1, 2], m2[1, 2])))
  v <- v[!is.na(v)]
  if (length(v) == 0) NA_integer_ else max(v)
}

scrape_zuk <- function(url, max_pages = 3L, sleep_sec = 0.75, user_agent, timeout_sec, verbose = FALSE) {
  first <- fetch_html(url, user_agent = user_agent, timeout_sec = timeout_sec, verbose = verbose)
  all_rows <- extract_zuk_cards(first$doc, source_url = url)

  token <- extract_zuk_token(first$doc)
  order <- extract_zuk_order(first$doc) %||% "relevancia"
  total_expected <- extract_zuk_total(first$text)
  log_msg("Zuk: lotes iniciais=", nrow(all_rows), "; total esperado=", total_expected %||% NA, verbose = verbose)

  if (is.na(token) || max_pages <= 1) {
    return(dedupe_results(all_rows))
  }

  endpoint <- "https://www.portalzuk.com.br/leilao-de-imoveis/mais"
  page_idx <- 2L

  while (page_idx <= max_pages) {
    loaded <- nrow(all_rows)
    if (!is.na(total_expected) && loaded >= total_expected) break

    form <- list(
      limit = as.character(loaded),
      count_imovel_zuk = as.character(loaded),
      path = url,
      order = order,
      div_parceiro_count = "0",
      "_token" = token
    )

    resp <- post_form(
      endpoint,
      form = form,
      user_agent = user_agent,
      timeout_sec = timeout_sec,
      headers = list(
        Referer = url,
        Origin = "https://www.portalzuk.com.br",
        `X-Requested-With` = "XMLHttpRequest"
      ),
      verbose = verbose
    )

    if (!grepl("card_lotes_div", resp$text, fixed = TRUE)) {
      log_msg("Zuk: sem novos cards no bloco de carregar mais.", verbose = verbose)
      break
    }

    block_doc <- xml2::read_html(
      paste0("<html><body>", resp$text, "</body></html>"),
      options = c("RECOVER", "NOERROR", "NOWARNING")
    )
    new_rows <- extract_zuk_cards(block_doc, source_url = url)
    before <- nrow(all_rows)
    all_rows <- dedupe_results(rbind_safe(all_rows, new_rows))
    added <- nrow(all_rows) - before
    log_msg("Zuk: pagina ", page_idx, " -> +", added, " registros", verbose = verbose)

    if (added <= 0) break
    if (sleep_sec > 0) Sys.sleep(sleep_sec)
    page_idx <- page_idx + 1L
  }

  dedupe_results(all_rows)
}

extract_mega_cards <- function(doc, source_url) {
  cards <- xml2::xml_find_all(
    doc,
    ".//div[contains(concat(' ', normalize-space(@class), ' '), ' card ') and contains(concat(' ', normalize-space(@class), ' '), ' open ')]"
  )
  if (length(cards) == 0) return(empty_results())

  rows <- vector("list", length(cards))
  scraped_at <- format(Sys.time(), "%Y-%m-%d %H:%M:%S %z")

  for (i in seq_along(cards)) {
    card <- cards[[i]]
    title_node <- xml2::xml_find_first(card, ".//a[contains(@class,'card-title')][1]")
    title <- safe_text(title_node)
    href <- safe_attr(title_node, "href")
    listing_url <- make_absolute_url(source_url, href)

    listing_id <- safe_text(xml2::xml_find_first(card, ".//div[contains(@class,'card-number')][1]"))
    if (is.na(listing_id)) listing_id <- extract_id_from_url(listing_url)

    city_uf <- safe_text(xml2::xml_find_first(card, ".//a[contains(@class,'card-locality')][1]"))
    auction_type <- safe_text(xml2::xml_find_first(card, ".//div[contains(@class,'card-instance-title')]//a[1]"))
    lot <- safe_text(xml2::xml_find_first(card, ".//div[contains(@class,'card-batch-number')][1]"))
    status <- safe_text(xml2::xml_find_first(card, ".//div[contains(@class,'card-status')][1]"))

    image_url <- safe_attr(xml2::xml_find_first(card, ".//a[contains(@class,'card-image')][1]"), "data-bg")
    if (is.na(image_url)) image_url <- safe_attr(xml2::xml_find_first(card, ".//a[contains(@class,'card-image')][1]"), "href")

    price_values <- safe_text_vec(xml2::xml_find_all(card, ".//div[contains(@class,'card-instance-info')]//span[contains(@class,'card-instance-value')]"))
    price_1 <- parse_money_br(price_values[1] %||% NA_character_)
    price_2 <- parse_money_br(price_values[2] %||% NA_character_)
    if (is.na(price_1)) price_1 <- parse_money_br(safe_text(xml2::xml_find_first(card, ".//div[contains(@class,'card-price')][1]")))
    min_price <- suppressWarnings(min(c(price_1, price_2), na.rm = TRUE))
    if (!is.finite(min_price)) min_price <- NA_real_

    auction_date_1 <- safe_text(xml2::xml_find_first(card, ".//span[contains(@class,'card-first-instance-date')][1]"))
    auction_date_2 <- safe_text(xml2::xml_find_first(card, ".//span[contains(@class,'card-second-instance-date')][1]"))
    dt1 <- parse_datetime_br(auction_date_1)
    dt2 <- parse_datetime_br(auction_date_2)

    property_type <- NA_character_
    p <- httr::parse_url(listing_url)
    seg <- Filter(nzchar, strsplit(p$path %||% "", "/", fixed = TRUE)[[1]])
    if (length(seg) >= 2 && identical(seg[[1]], "imoveis")) {
      property_type <- normalize_ws(seg[[2]])
    }

    rows[[i]] <- data.frame(
      site = "megaleiloes",
      listing_id = toupper(listing_id %||% NA_character_),
      title = title,
      property_type = property_type,
      city_uf = city_uf,
      address = NA_character_,
      area_info = NA_character_,
      auction_type = auction_type,
      lot = lot,
      status = status,
      price_1 = price_1,
      price_2 = price_2,
      min_price = min_price,
      auction_date_1 = auction_date_1,
      auction_date_2 = auction_date_2,
      auction_datetime_1 = fmt_datetime(dt1),
      auction_datetime_2 = fmt_datetime(dt2),
      image_url = image_url,
      listing_url = listing_url,
      source_url = source_url,
      scraped_at = scraped_at,
      stringsAsFactors = FALSE
    )
  }

  out <- do.call(rbind, rows)
  dedupe_results(out)
}

extract_mega_last_page <- function(doc, fallback = 1L) {
  last_href <- safe_attr(xml2::xml_find_first(doc, ".//link[@rel='last'][1]"), "href")
  if (!is.na(last_href)) {
    p <- suppressWarnings(as.integer(parse_query_value(last_href, "pagina")))
    if (!is.na(p)) return(p)
  }

  links <- safe_attr(xml2::xml_find_all(doc, ".//li/a[@data-page]"), "data-page")
  if (length(links) > 0) {
    nums <- suppressWarnings(as.integer(links))
    nums <- nums[!is.na(nums)]
    if (length(nums) > 0) return(max(nums) + 1L)
  }
  as.integer(fallback)
}

scrape_megaleiloes <- function(url, max_pages = 3L, sleep_sec = 0.75, user_agent, timeout_sec, verbose = FALSE) {
  start_page <- suppressWarnings(as.integer(parse_query_value(url, "pagina")))
  if (is.na(start_page) || start_page < 1L) start_page <- 1L

  all_rows <- empty_results()
  last_page <- NA_integer_

  for (i in seq_len(max_pages)) {
    page <- start_page + i - 1L
    page_url <- set_query_value(url, "pagina", page)
    fetched <- fetch_html(page_url, user_agent = user_agent, timeout_sec = timeout_sec, verbose = verbose)

    if (is.na(last_page)) {
      last_page <- extract_mega_last_page(fetched$doc, fallback = page)
      log_msg("MegaLeiloes: ultima pagina detectada=", last_page, verbose = verbose)
    }

    rows <- extract_mega_cards(fetched$doc, source_url = page_url)
    log_msg("MegaLeiloes: pagina ", page, " -> ", nrow(rows), " registros", verbose = verbose)
    if (nrow(rows) == 0) break

    all_rows <- dedupe_results(rbind_safe(all_rows, rows))
    if (!is.na(last_page) && page >= last_page) break
    if (sleep_sec > 0) Sys.sleep(sleep_sec)
  }

  dedupe_results(all_rows)
}

scrape_leeilon <- function(url, user_agent, timeout_sec, verbose = FALSE) {
  fetched <- fetch_html(url, user_agent = user_agent, timeout_sec = timeout_sec, verbose = verbose)
  links <- xml2::xml_find_all(
    fetched$doc,
    ".//a[contains(@href,'/imovel') or contains(@href,'/lote') or contains(@href,'/imoveis/')]"
  )
  hrefs <- unique(na.omit(vapply(links, function(n) safe_attr(n, "href"), character(1))))
  hrefs <- hrefs[grepl("/imovel|/lote|/imoveis/", hrefs)]

  if (length(hrefs) == 0) {
    log_msg("Leeilon: nenhum card estatico encontrado. Site usa renderizacao dinamica.", verbose = TRUE)
    return(empty_results())
  }

  scraped_at <- format(Sys.time(), "%Y-%m-%d %H:%M:%S %z")
  rows <- lapply(hrefs, function(h) {
    listing_url <- make_absolute_url(url, h)
    data.frame(
      site = "leeilon",
      listing_id = extract_id_from_url(listing_url),
      title = NA_character_,
      property_type = NA_character_,
      city_uf = NA_character_,
      address = NA_character_,
      area_info = NA_character_,
      auction_type = NA_character_,
      lot = NA_character_,
      status = NA_character_,
      price_1 = NA_real_,
      price_2 = NA_real_,
      min_price = NA_real_,
      auction_date_1 = NA_character_,
      auction_date_2 = NA_character_,
      auction_datetime_1 = NA_character_,
      auction_datetime_2 = NA_character_,
      image_url = NA_character_,
      listing_url = listing_url,
      source_url = url,
      scraped_at = scraped_at,
      stringsAsFactors = FALSE
    )
  })

  dedupe_results(do.call(rbind, rows))
}

usage <- function() {
  cat(
    "Uso:\n",
    "  Rscript scripts/scrape_leiloes.R --url <URL> [opcoes]\n\n",
    "Opcoes:\n",
    "  --site <auto|leeilon|zuk|megaleiloes>   Site alvo (padrao: auto)\n",
    "  --max-pages <N>                          Maximo de paginas/blocos (padrao: 3)\n",
    "  --sleep <segundos>                       Espera entre requisicoes (padrao: 0.75)\n",
    "  --format <csv|json>                      Formato de saida (padrao: csv)\n",
    "  --out <arquivo>                          Caminho do arquivo de saida\n",
    "  --verbose                                Logs detalhados (padrao: ligado)\n",
    "  --quiet                                  Sem logs\n",
    "  --help                                   Mostra esta ajuda\n\n",
    "Exemplos:\n",
    "  Rscript scripts/scrape_leiloes.R --url \"https://www.portalzuk.com.br/leilao-de-imoveis\"\n",
    "  Rscript scripts/scrape_leiloes.R --url \"https://www.megaleiloes.com.br/imoveis?pagina=1\" --site megaleiloes --max-pages 5\n",
    sep = ""
  )
}

parse_args <- function(args) {
  opt <- list(
    url = NULL,
    site = "auto",
    max_pages = 3L,
    sleep = 0.75,
    format = "csv",
    out = NULL,
    verbose = TRUE,
    help = FALSE
  )

  i <- 1L
  while (i <= length(args)) {
    a <- args[[i]]

    if (a %in% c("-h", "--help")) {
      opt$help <- TRUE
      i <- i + 1L
      next
    }
    if (a %in% c("--quiet", "--no-verbose")) {
      opt$verbose <- FALSE
      i <- i + 1L
      next
    }
    if (a == "--verbose") {
      opt$verbose <- TRUE
      i <- i + 1L
      next
    }

    if (startsWith(a, "--")) {
      if (i == length(args)) stop("Missing value for option: ", a)
      key <- sub("^--", "", a)
      val <- args[[i + 1L]]
      if (key == "url") {
        opt$url <- val
      } else if (key == "site") {
        opt$site <- val
      } else if (key == "max-pages") {
        opt$max_pages <- as.integer(val)
      } else if (key == "sleep") {
        opt$sleep <- as.numeric(val)
      } else if (key == "format") {
        opt$format <- tolower(val)
      } else if (key == "out") {
        opt$out <- val
      } else {
        stop("Unknown option: --", key)
      }
      i <- i + 2L
      next
    }

    if (is.null(opt$url)) {
      opt$url <- a
      i <- i + 1L
      next
    }
    stop("Unexpected argument: ", a)
  }

  opt
}

write_output <- function(df, out_path, fmt) {
  dir.create(dirname(out_path), recursive = TRUE, showWarnings = FALSE)
  if (fmt == "json") {
    jsonlite::write_json(df, out_path, pretty = TRUE, auto_unbox = TRUE, na = "null")
  } else {
    utils::write.csv(df, out_path, row.names = FALSE, fileEncoding = "UTF-8")
  }
}

main <- function() {
  required <- c("httr", "xml2", "jsonlite", "stringr")
  missing <- required[!vapply(required, requireNamespace, logical(1), quietly = TRUE)]
  if (length(missing) > 0) {
    stop("Missing packages: ", paste(missing, collapse = ", "),
         ". Install with: install.packages(c(", paste(sprintf("\"%s\"", missing), collapse = ", "), "))")
  }

  opt <- parse_args(commandArgs(trailingOnly = TRUE))
  if (isTRUE(opt$help)) {
    usage()
    quit(status = 0L)
  }
  if (is.null(opt$url) || !nzchar(opt$url)) {
    usage()
    stop("URL is required. Use --url <URL>.")
  }
  if (!grepl("^https?://", opt$url, ignore.case = TRUE)) {
    stop("URL must start with http:// or https://")
  }
  if (is.na(opt$max_pages) || opt$max_pages < 1) {
    stop("--max-pages must be >= 1")
  }
  if (is.na(opt$sleep) || opt$sleep < 0) {
    stop("--sleep must be >= 0")
  }
  if (!opt$format %in% c("csv", "json")) {
    stop("--format must be csv or json")
  }

  site <- normalize_site(opt$site)
  if (is.na(site)) stop("Unsupported --site value: ", opt$site)
  if (site == "auto") {
    site <- detect_site_from_url(opt$url)
    if (is.na(site)) {
      stop("Could not detect site from URL. Use --site leeilon|zuk|megaleiloes.")
    }
  }

  user_agent <- "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/132.0 Safari/537.36"
  timeout_sec <- 90

  log_msg("Site detectado: ", site, verbose = opt$verbose)
  log_msg("URL: ", opt$url, verbose = opt$verbose)
  log_msg("Max pages: ", opt$max_pages, verbose = opt$verbose)

  res <- switch(
    site,
    "zuk" = scrape_zuk(
      url = opt$url,
      max_pages = opt$max_pages,
      sleep_sec = opt$sleep,
      user_agent = user_agent,
      timeout_sec = timeout_sec,
      verbose = opt$verbose
    ),
    "megaleiloes" = scrape_megaleiloes(
      url = opt$url,
      max_pages = opt$max_pages,
      sleep_sec = opt$sleep,
      user_agent = user_agent,
      timeout_sec = timeout_sec,
      verbose = opt$verbose
    ),
    "leeilon" = scrape_leeilon(
      url = opt$url,
      user_agent = user_agent,
      timeout_sec = timeout_sec,
      verbose = opt$verbose
    ),
    stop("Unhandled site: ", site)
  )

  res <- dedupe_results(res)
  res <- res[order(res$site, res$listing_id, res$listing_url), , drop = FALSE]

  if (is.null(opt$out) || !nzchar(opt$out)) {
    ts <- format(Sys.time(), "%Y%m%d_%H%M%S")
    ext <- ifelse(opt$format == "json", "json", "csv")
    opt$out <- file.path(getwd(), sprintf("leiloes_%s_%s.%s", site, ts, ext))
  }

  write_output(res, opt$out, opt$format)

  cat("Registros:", nrow(res), "\n")
  cat("Site:", site, "\n")
  cat("Arquivo:", normalizePath(opt$out, winslash = "/", mustWork = FALSE), "\n")
}

main()

