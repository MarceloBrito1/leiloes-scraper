#!/usr/bin/env Rscript

`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0 || (length(x) == 1 && is.na(x))) y else x
}

repair_mojibake_once <- function(x) {
  if (length(x) == 0) return(x)
  out <- x
  idx <- !is.na(out) & grepl("[ÃÂâ]", out)
  if (!any(idx)) return(out)

  cand <- suppressWarnings(iconv(out[idx], from = "UTF-8", to = "latin1"))
  Encoding(cand) <- "UTF-8"
  valid <- !is.na(suppressWarnings(iconv(cand, from = "UTF-8", to = "UTF-8", sub = NA)))
  m_orig <- nchar(gsub("[^ÃÂâ]", "", out[idx]))
  cand_safe <- ifelse(valid, cand, "")
  m_cand <- nchar(gsub("[^ÃÂâ]", "", cand_safe))
  use <- valid & m_cand < m_orig
  out[idx][use] <- cand[use]
  out
}

repair_mojibake <- function(x, max_passes = 2L) {
  if (length(x) == 0) return(x)
  out <- x
  for (i in seq_len(max_passes)) {
    nxt <- repair_mojibake_once(out)
    if (identical(nxt, out)) break
    out <- nxt
  }
  out
}

normalize_ws <- function(x) {
  if (length(x) == 0 || is.na(x)) return(NA_character_)
  y <- stringr::str_squish(trimws(as.character(x)))
  y <- repair_mojibake(y)
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
  m <- stringr::str_match(x, "(\\d{2}/\\d{2}/\\d{4})\\s*(?:as|Ã s)\\s*(\\d{2}:\\d{2})")
  if (!all(is.na(m))) {
    dt <- paste(m[, 2], m[, 3])
    return(as.POSIXct(dt, format = "%d/%m/%Y %H:%M", tz = "America/Sao_Paulo"))
  }

  d <- stringr::str_match(x, "(\\d{2}/\\d{2}/\\d{4})")
  if (!all(is.na(d))) {
    return(as.POSIXct(d[, 2], format = "%d/%m/%Y", tz = "America/Sao_Paulo"))
  }
  as.POSIXct(NA)
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

is_retryable_http_status <- function(status_code) {
  status_code %in% c(408L, 425L, 429L) || (status_code >= 500L && status_code <= 599L)
}

extract_retry_after_seconds <- function(resp) {
  h <- httr::headers(resp)
  if (length(h) == 0) return(NA_real_)
  idx <- which(tolower(names(h)) == "retry-after")[1]
  if (is.na(idx)) return(NA_real_)
  raw <- normalize_ws(as.character(h[[idx]]))
  if (is.na(raw)) return(NA_real_)

  as_num <- suppressWarnings(as.numeric(raw))
  if (!is.na(as_num)) return(max(0, as_num))

  as_dt <- suppressWarnings(as.POSIXct(raw, tz = "GMT"))
  if (is.na(as_dt)) return(NA_real_)
  max(0, as.numeric(difftime(as_dt, Sys.time(), units = "secs")))
}

compute_retry_wait <- function(attempt, resp = NULL, retry_base_sec = 2, retry_max_sec = 90) {
  expo <- min(retry_max_sec, retry_base_sec * (2^(attempt - 1L)))
  jitter <- stats::runif(1, min = 0, max = retry_base_sec)
  wait <- expo + jitter
  if (!is.null(resp)) {
    retry_after <- extract_retry_after_seconds(resp)
    if (!is.na(retry_after)) {
      wait <- max(wait, retry_after)
    }
  }
  wait
}

request_with_retry <- function(request_fn, request_label, max_retries = 6L, retry_base_sec = 2, retry_max_sec = 90, verbose = FALSE) {
  max_attempts <- as.integer(max_retries) + 1L
  if (is.na(max_attempts) || max_attempts < 1L) max_attempts <- 1L

  attempt <- 1L
  repeat {
    req_or_err <- tryCatch(
      request_fn(),
      error = function(e) e
    )

    if (inherits(req_or_err, "error")) {
      if (attempt >= max_attempts) {
        stop("Falha em ", request_label, " apos ", max_attempts, " tentativas: ", conditionMessage(req_or_err))
      }
      wait <- compute_retry_wait(
        attempt = attempt,
        resp = NULL,
        retry_base_sec = retry_base_sec,
        retry_max_sec = retry_max_sec
      )
      log_msg(
        request_label, " falhou (tentativa ", attempt, "/", max_attempts, "): ",
        conditionMessage(req_or_err), ". Aguardando ", sprintf("%.1f", wait), "s para tentar novamente.",
        verbose = verbose
      )
      Sys.sleep(wait)
      attempt <- attempt + 1L
      next
    }

    req <- req_or_err
    status <- httr::status_code(req)
    if (status >= 200L && status < 300L) return(req)

    if (is_retryable_http_status(status) && attempt < max_attempts) {
      wait <- compute_retry_wait(
        attempt = attempt,
        resp = req,
        retry_base_sec = retry_base_sec,
        retry_max_sec = retry_max_sec
      )
      log_msg(
        request_label, " recebeu HTTP ", status,
        " (tentativa ", attempt, "/", max_attempts, "). Aguardando ",
        sprintf("%.1f", wait), "s para tentar novamente.",
        verbose = verbose
      )
      Sys.sleep(wait)
      attempt <- attempt + 1L
      next
    }

    httr::stop_for_status(req)
  }
}

fetch_html <- function(
  url,
  user_agent,
  timeout_sec = 60,
  headers = list(),
  handle = NULL,
  max_retries = 6L,
  retry_base_sec = 2,
  retry_max_sec = 90,
  verbose = FALSE
) {
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
  if (!is.null(handle)) {
    args <- c(args, list(handle = handle))
  }
  req <- request_with_retry(
    request_fn = function() do.call(httr::GET, args),
    request_label = paste0("GET ", url),
    max_retries = max_retries,
    retry_base_sec = retry_base_sec,
    retry_max_sec = retry_max_sec,
    verbose = verbose
  )
  txt <- httr::content(req, as = "text", encoding = "UTF-8")
  doc <- xml2::read_html(txt, options = c("RECOVER", "NOERROR", "NOWARNING"))
  list(resp = req, text = txt, doc = doc)
}

post_form <- function(
  url,
  form,
  user_agent,
  timeout_sec = 60,
  headers = list(),
  handle = NULL,
  max_retries = 6L,
  retry_base_sec = 2,
  retry_max_sec = 90,
  verbose = FALSE
) {
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
  if (!is.null(handle)) {
    args <- c(args, list(handle = handle))
  }
  req <- request_with_retry(
    request_fn = function() do.call(httr::POST, args),
    request_label = paste0("POST ", url),
    max_retries = max_retries,
    retry_base_sec = retry_base_sec,
    retry_max_sec = retry_max_sec,
    verbose = verbose
  )
  txt <- httr::content(req, as = "text", encoding = "UTF-8")
  doc <- xml2::read_html(txt, options = c("RECOVER", "NOERROR", "NOWARNING"))
  list(resp = req, text = txt, doc = doc)
}

normalize_site <- function(site) {
  s <- tolower(normalize_ws(site) %||% "auto")
  s <- gsub("ÃƒÆ’Ã‚Â£", "a", s, fixed = TRUE)
  s <- gsub("ÃƒÆ’Ã‚Â©", "e", s, fixed = TRUE)
  s <- gsub("ÃƒÆ’Ã‚Â§", "c", s, fixed = TRUE)
  s <- gsub("ÃƒÆ’Ã‚Âµ", "o", s, fixed = TRUE)
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

extract_embedded_url <- function(x) {
  x <- normalize_ws(x)
  if (is.na(x)) return(NA_character_)
  dec <- suppressWarnings(utils::URLdecode(x))
  if (!is.na(dec) && grepl("^https?://", dec, ignore.case = TRUE)) return(dec)
  if (grepl("^https?://", x, ignore.case = TRUE)) return(x)
  NA_character_
}

unwrap_tracking_url <- function(url, max_hops = 3L) {
  cur <- url
  for (i in seq_len(max_hops)) {
    p <- httr::parse_url(cur)
    q <- p$query %||% list()
    keys <- c("url", "u", "target", "redirect", "redirect_url", "dest", "destination", "next", "to", "href")
    cand <- NA_character_
    for (k in keys) {
      if (!is.null(q[[k]]) && nzchar(q[[k]])) {
        cand <- extract_embedded_url(q[[k]])
        if (!is.na(cand)) break
      }
    }
    if (is.na(cand)) break
    cur <- cand
  }
  cur
}

canonical_search_url <- function(url, site) {
  u <- unwrap_tracking_url(url)
  p <- httr::parse_url(u)
  if (is.null(p$query)) p$query <- list()
  path <- p$path %||% ""

  if (site == "zuk") {
    if (!grepl("^/?leilao-de-imoveis", path)) {
      p$path <- "leilao-de-imoveis"
    }
    return(httr::build_url(p))
  }

  if (site == "megaleiloes") {
    if (!grepl("^/?(imoveis|leiloes-judiciais|leiloes-extrajudiciais)", path)) {
      p$path <- "imoveis"
    }
    if (is.null(p$query$pagina) || !nzchar(p$query$pagina)) {
      p$query$pagina <- "1"
    }
    return(httr::build_url(p))
  }

  if (site == "leeilon") {
    if (!grepl("^/?busca-leilao", path)) {
      p$path <- "busca-leilao"
    }
    if (is.null(p$query$page) || !nzchar(p$query$page)) {
      p$query$page <- "1"
    }
    return(httr::build_url(p))
  }

  u
}

ascii_fold_lower <- function(x) {
  if (length(x) == 0 || is.na(x)) return("")
  y <- suppressWarnings(iconv(as.character(x), from = "", to = "ASCII//TRANSLIT"))
  if (is.na(y)) y <- as.character(x)
  tolower(y)
}

sanitize_credential <- function(x) {
  y <- normalize_ws(x)
  if (is.na(y) || !nzchar(y)) NA_character_ else y
}

sanitize_password <- function(x) {
  if (is.null(x) || length(x) == 0) return(NA_character_)
  y <- as.character(x[[1]])
  if (is.na(y) || !nzchar(y)) NA_character_ else y
}

site_origin <- function(url) {
  p <- httr::parse_url(url)
  if (is.null(p$scheme) || is.null(p$hostname)) return(NA_character_)
  out <- paste0(p$scheme, "://", p$hostname)
  if (!is.null(p$port)) out <- paste0(out, ":", p$port)
  out
}

site_login_candidates <- function(site, url) {
  generic <- c(
    url,
    make_absolute_url(url, "/login"),
    make_absolute_url(url, "/entrar"),
    make_absolute_url(url, "/acesso"),
    make_absolute_url(url, "/minha-conta"),
    make_absolute_url(url, "/minha-conta/login")
  )

  site_specific <- switch(
    site,
    "zuk" = c(
      make_absolute_url(url, "/entrar"),
      make_absolute_url(url, "/login"),
      make_absolute_url(url, "/minha-conta/login")
    ),
    "megaleiloes" = c(
      make_absolute_url(url, "/login"),
      make_absolute_url(url, "/entrar"),
      make_absolute_url(url, "/acesso")
    ),
    "leeilon" = c(
      make_absolute_url(url, "/login"),
      make_absolute_url(url, "/entrar"),
      make_absolute_url(url, "/acesso")
    ),
    character(0)
  )

  out <- unique(c(site_specific, generic))
  out[!is.na(out) & grepl("^https?://", out, ignore.case = TRUE)]
}

find_login_form <- function(doc) {
  forms <- xml2::xml_find_all(
    doc,
    ".//form[.//input[contains(translate(@type,'PASSWORD','password'),'password') or contains(translate(@name,'SENHAPASSWORD','senhapassword'),'senha') or contains(translate(@name,'SENHAPASSWORD','senhapassword'),'password')]]"
  )
  if (length(forms) == 0) return(NULL)
  forms[[1]]
}

extract_form_defaults <- function(form_node) {
  inputs <- xml2::xml_find_all(form_node, ".//input[@name]")
  if (length(inputs) == 0) return(list())
  out <- list()
  for (i in seq_along(inputs)) {
    input <- inputs[[i]]
    if (!is.na(xml2::xml_attr(input, "disabled"))) next
    name <- normalize_ws(xml2::xml_attr(input, "name"))
    if (is.na(name)) next
    value <- xml2::xml_attr(input, "value")
    if (is.na(value)) value <- ""
    out[[name]] <- as.character(value)
  }
  out
}

guess_login_fields <- function(form_node) {
  inputs <- xml2::xml_find_all(form_node, ".//input[@name]")
  if (length(inputs) == 0) return(list(user = NA_character_, pass = NA_character_))

  nms <- vapply(inputs, function(node) normalize_ws(xml2::xml_attr(node, "name")), character(1))
  typ <- vapply(inputs, function(node) {
    raw <- normalize_ws(xml2::xml_attr(node, "type"))
    if (is.na(raw)) "text" else tolower(raw)
  }, character(1))
  fold <- vapply(nms, ascii_fold_lower, character(1))

  idx_pass <- which(typ == "password" | grepl("senha|password|pass", fold))
  idx_text <- which(typ %in% c("text", "email", "tel", "number", "search", ""))
  idx_user_pref <- which(grepl("email|usuario|user|login|cpf|cnpj|document", fold))
  idx_user <- setdiff(idx_user_pref, idx_pass)
  if (length(idx_user) == 0) idx_user <- setdiff(idx_text, idx_pass)
  if (length(idx_user) == 0) idx_user <- setdiff(seq_along(nms), idx_pass)

  user <- nms[idx_user[1] %||% NA_integer_] %||% NA_character_
  pass <- nms[idx_pass[1] %||% NA_integer_] %||% NA_character_

  list(user = user, pass = pass)
}

form_action_url <- function(page_url, form_node) {
  action <- normalize_ws(xml2::xml_attr(form_node, "action"))
  if (is.na(action)) return(page_url)
  make_absolute_url(page_url, action)
}

form_method <- function(form_node) {
  m <- tolower(normalize_ws(xml2::xml_attr(form_node, "method")) %||% "post")
  if (!m %in% c("get", "post")) "post" else m
}

submit_form <- function(action_url, method, payload, referer_url, user_agent, timeout_sec, handle = NULL, verbose = FALSE) {
  headers <- list(Referer = referer_url)
  origin <- site_origin(referer_url)
  if (!is.na(origin)) headers$Origin <- origin

  if (method == "get") {
    p <- httr::parse_url(action_url)
    q <- p$query %||% list()
    for (k in names(payload)) q[[k]] <- as.character(payload[[k]])
    p$query <- q
    target <- httr::build_url(p)
    return(fetch_html(
      target,
      user_agent = user_agent,
      timeout_sec = timeout_sec,
      headers = headers,
      handle = handle,
      verbose = verbose
    ))
  }

  post_form(
    action_url,
    form = payload,
    user_agent = user_agent,
    timeout_sec = timeout_sec,
    headers = headers,
    handle = handle,
    verbose = verbose
  )
}

is_login_success <- function(doc, text) {
  txt <- ascii_fold_lower(text %||% "")
  err_pat <- paste(
    c(
      "senha\\s+incorret",
      "usuario\\s+incorret",
      "credenciais?\\s+invalid",
      "login\\s+invalid",
      "nao\\s+foi\\s+possivel\\s+entrar",
      "captcha",
      "recaptcha"
    ),
    collapse = "|"
  )
  if (grepl(err_pat, txt)) return(FALSE)

  ok_pat <- paste(
    c(
      "logout",
      "\\bsair\\b",
      "minha\\s+conta",
      "meus\\s+lances",
      "painel",
      "area\\s+do\\s+cliente"
    ),
    collapse = "|"
  )
  if (grepl(ok_pat, txt)) return(TRUE)

  is.null(find_login_form(doc))
}

attempt_login_on_page <- function(login_url, username, password, user_agent, timeout_sec, handle, verbose = FALSE) {
  page <- tryCatch(
    fetch_html(
      login_url,
      user_agent = user_agent,
      timeout_sec = timeout_sec,
      handle = handle,
      verbose = verbose
    ),
    error = function(e) {
      log_msg("Falha ao abrir pagina de login ", login_url, ": ", conditionMessage(e), verbose = verbose)
      NULL
    }
  )
  if (is.null(page)) return(list(found_form = FALSE, success = FALSE, reason = "open_failed"))

  form <- find_login_form(page$doc)
  if (is.null(form)) {
    return(list(found_form = FALSE, success = FALSE, reason = "form_not_found"))
  }

  fields <- guess_login_fields(form)
  if (is.na(fields$user) || is.na(fields$pass)) {
    return(list(found_form = TRUE, success = FALSE, reason = "fields_not_found"))
  }

  payload <- extract_form_defaults(form)
  payload[[fields$user]] <- username
  payload[[fields$pass]] <- password

  action_url <- form_action_url(login_url, form)
  method <- form_method(form)

  submitted <- tryCatch(
    submit_form(
      action_url = action_url,
      method = method,
      payload = payload,
      referer_url = login_url,
      user_agent = user_agent,
      timeout_sec = timeout_sec,
      handle = handle,
      verbose = verbose
    ),
    error = function(e) {
      log_msg("Falha no submit de login ", action_url, ": ", conditionMessage(e), verbose = verbose)
      NULL
    }
  )
  if (is.null(submitted)) return(list(found_form = TRUE, success = FALSE, reason = "submit_failed"))

  ok <- is_login_success(submitted$doc, submitted$text)
  list(found_form = TRUE, success = ok, reason = if (ok) "login_success" else "login_not_confirmed")
}

resolve_site_credentials <- function(site) {
  key <- toupper(gsub("[^A-Za-z0-9]", "_", site))
  user <- sanitize_credential(Sys.getenv(paste0("LEILOES_", key, "_USER"), unset = ""))
  pass <- sanitize_password(Sys.getenv(paste0("LEILOES_", key, "_PASS"), unset = ""))

  if (is.na(pass)) pass <- sanitize_password(Sys.getenv(paste0("LEILOES_", key, "_PASSWORD"), unset = ""))
  if (is.na(user)) user <- sanitize_credential(Sys.getenv("LEILOES_LOGIN_USER", unset = ""))
  if (is.na(pass)) pass <- sanitize_password(Sys.getenv("LEILOES_LOGIN_PASS", unset = ""))
  if (is.na(pass)) pass <- sanitize_password(Sys.getenv("LEILOES_LOGIN_PASSWORD", unset = ""))

  list(user = user, pass = pass)
}

start_site_session <- function(site, url, user_agent, timeout_sec, verbose = FALSE) {
  origin <- site_origin(url)
  if (is.na(origin)) stop("Could not infer site origin for session handle.")
  handle <- httr::handle(origin)

  creds <- resolve_site_credentials(site)
  if (is.na(creds$user) || is.na(creds$pass)) {
    log_msg("Login: credenciais nao informadas; executando sem autenticacao.", verbose = verbose)
    return(list(handle = handle, attempted = FALSE, authenticated = FALSE, reason = "no_credentials"))
  }

  log_msg("Login: tentando autenticar em ", site, " ...", verbose = verbose)
  candidates <- site_login_candidates(site, url)
  found_form <- FALSE

  for (login_url in candidates) {
    log_msg("Login: avaliando ", login_url, verbose = verbose)
    attempt <- attempt_login_on_page(
      login_url = login_url,
      username = creds$user,
      password = creds$pass,
      user_agent = user_agent,
      timeout_sec = timeout_sec,
      handle = handle,
      verbose = verbose
    )
    if (isTRUE(attempt$found_form)) found_form <- TRUE
    if (isTRUE(attempt$success)) {
      log_msg("Login: autenticacao confirmada em ", login_url, verbose = verbose)
      return(list(handle = handle, attempted = TRUE, authenticated = TRUE, reason = "ok", login_url = login_url))
    }
  }

  if (found_form) {
    log_msg("Login: formulario encontrado, mas autenticacao nao foi confirmada.", verbose = verbose)
    return(list(handle = handle, attempted = TRUE, authenticated = FALSE, reason = "not_confirmed"))
  }

  log_msg("Login: nenhum formulario de login detectado; continuando sem autenticacao.", verbose = verbose)
  list(handle = handle, attempted = TRUE, authenticated = FALSE, reason = "form_not_found")
}

normalize_auction_type <- function(x) {
  x <- tolower(normalize_ws(x))
  if (is.na(x)) return(NA_character_)
  if (grepl("extrajud", x)) return("extrajudicial")
  if (grepl("judicial", x)) return("judicial")
  NA_character_
}

infer_zuk_auction_type <- function(title, address = NA_character_) {
  txt <- paste(
    normalize_ws(title) %||% "",
    normalize_ws(address) %||% "",
    collapse = " "
  )
  txt <- tolower(txt)
  if (!nzchar(txt)) return(NA_character_)
  if (grepl("extrajudicial", txt)) return("extrajudicial")
  if (grepl("judicial|tribunal|justi[cÃ§]a|vara|tjsp|tj[a-z]{2}", txt)) return("judicial")
  if (grepl("banco|caixa|santander|bradesco|itau|sicoob|financeira|cooperativa", txt)) return("extrajudicial")
  NA_character_
}

parse_iso_datetime <- function(x) {
  x <- normalize_ws(x)
  if (is.na(x)) return(as.POSIXct(NA))
  suppressWarnings(as.POSIXct(x, format = "%Y-%m-%d %H:%M:%S %z", tz = "America/Sao_Paulo"))
}

safe_min_future_dt <- function(dt1, dt2, now) {
  cands <- c(dt1, dt2)
  cands <- cands[!is.na(cands) & cands >= now]
  if (length(cands) == 0) return(as.POSIXct(NA))
  min(cands)
}

compute_current_round <- function(dt1, dt2, now) {
  if (!is.na(dt1) && now <= dt1) return("1")
  if (!is.na(dt2) && now <= dt2) return("2")
  if (!is.na(dt1) || !is.na(dt2)) return("ended")
  NA_character_
}

parse_date_ymd <- function(x, arg_name) {
  x <- normalize_ws(x)
  if (is.na(x)) return(as.Date(NA))
  d <- suppressWarnings(as.Date(x, format = "%Y-%m-%d"))
  if (is.na(d)) stop(arg_name, " must be in YYYY-MM-DD format")
  d
}

in_date_window <- function(d, from, to) {
  if (is.na(d)) return(FALSE)
  if (!is.na(from) && d < from) return(FALSE)
  if (!is.na(to) && d > to) return(FALSE)
  TRUE
}

enrich_results <- function(df) {
  if (nrow(df) == 0) {
    df$auction_type_norm <- character(0)
    df$current_round <- character(0)
    df$next_auction_datetime <- character(0)
    return(df)
  }

  dt1 <- as.POSIXct(vapply(df$auction_datetime_1, function(x) parse_iso_datetime(x), as.POSIXct(NA)))
  dt2 <- as.POSIXct(vapply(df$auction_datetime_2, function(x) parse_iso_datetime(x), as.POSIXct(NA)))

  missing_1 <- is.na(dt1)
  missing_2 <- is.na(dt2)
  if (any(missing_1)) {
    dt1[missing_1] <- as.POSIXct(vapply(df$auction_date_1[missing_1], function(x) parse_datetime_br(x), as.POSIXct(NA)))
  }
  if (any(missing_2)) {
    dt2[missing_2] <- as.POSIXct(vapply(df$auction_date_2[missing_2], function(x) parse_datetime_br(x), as.POSIXct(NA)))
  }

  now <- as.POSIXct(Sys.time(), tz = "America/Sao_Paulo")
  next_dt <- as.POSIXct(vapply(seq_len(nrow(df)), function(i) safe_min_future_dt(dt1[i], dt2[i], now), as.POSIXct(NA)))
  round_vec <- vapply(seq_len(nrow(df)), function(i) compute_current_round(dt1[i], dt2[i], now), character(1))

  type_norm <- vapply(df$auction_type, normalize_auction_type, character(1))
  idx_zuk <- which((df$site %||% "") == "zuk" & is.na(type_norm))
  if (length(idx_zuk) > 0) {
    inf <- vapply(
      idx_zuk,
      function(i) infer_zuk_auction_type(df$title[i] %||% NA_character_, df$address[i] %||% NA_character_),
      character(1)
    )
    type_norm[idx_zuk] <- inf
  }

  df$auction_type_norm <- type_norm
  df$current_round <- round_vec
  df$next_auction_datetime <- vapply(next_dt, fmt_datetime, character(1))
  df
}

apply_report_filters <- function(df, opt, verbose = FALSE) {
  df <- enrich_results(df)
  if (nrow(df) == 0) return(df)

  keep <- rep(TRUE, nrow(df))

  if (!is.null(opt$auction_type) && opt$auction_type != "any") {
    keep <- keep & (!is.na(df$auction_type_norm) & df$auction_type_norm == opt$auction_type)
  }

  if (!is.null(opt$current_round) && opt$current_round != "any") {
    keep <- keep & (!is.na(df$current_round) & df$current_round == opt$current_round)
  }

  from_date <- parse_date_ymd(opt$date_from %||% NA_character_, "--date-from")
  to_date <- parse_date_ymd(opt$date_to %||% NA_character_, "--date-to")
  if (!is.na(from_date) || !is.na(to_date)) {
    dt1 <- as.POSIXct(vapply(df$auction_datetime_1, function(x) parse_iso_datetime(x), as.POSIXct(NA)))
    dt2 <- as.POSIXct(vapply(df$auction_datetime_2, function(x) parse_iso_datetime(x), as.POSIXct(NA)))
    d1 <- as.Date(dt1)
    d2 <- as.Date(dt2)
    dn <- as.Date(as.POSIXct(vapply(df$next_auction_datetime, function(x) parse_iso_datetime(x), as.POSIXct(NA))))

    field <- opt$date_field %||% "next"
    cond <- rep(FALSE, nrow(df))
    if (field == "first") {
      cond <- vapply(d1, in_date_window, logical(1), from = from_date, to = to_date)
    } else if (field == "second") {
      cond <- vapply(d2, in_date_window, logical(1), from = from_date, to = to_date)
    } else if (field == "next") {
      cond <- vapply(dn, in_date_window, logical(1), from = from_date, to = to_date)
    } else if (field == "any") {
      c1 <- vapply(d1, in_date_window, logical(1), from = from_date, to = to_date)
      c2 <- vapply(d2, in_date_window, logical(1), from = from_date, to = to_date)
      cond <- c1 | c2
    } else {
      stop("Unsupported --date-field: ", field)
    }
    keep <- keep & cond
  }

  out <- df[keep, , drop = FALSE]
  log_msg("Filtro aplicado: ", nrow(df), " -> ", nrow(out), " registros", verbose = verbose)
  out
}

apply_site_native_filters <- function(url, site, opt, verbose = FALSE) {
  out <- url
  if (is.null(opt$auction_type) || opt$auction_type == "any") return(out)

  if (site == "megaleiloes") {
    p <- httr::parse_url(out)
    p$path <- if (opt$auction_type == "judicial") "leiloes-judiciais" else "leiloes-extrajudiciais"
    if (is.null(p$query)) p$query <- list()
    if (is.null(p$query$pagina) || !nzchar(p$query$pagina)) p$query$pagina <- "1"
    out <- httr::build_url(p)
    log_msg("MegaLeiloes: filtro nativo aplicado (auction_type=", opt$auction_type, "): ", out, verbose = verbose)
    return(out)
  }

  if (site == "zuk") {
    p <- httr::parse_url(out)
    if (is.null(p$query)) p$query <- list()
    p$query$comitente_judicial <- if (opt$auction_type == "judicial") "1" else "0"
    out <- httr::build_url(p)
    log_msg("Zuk: tentativa de filtro nativo aplicada (comitente_judicial=", p$query$comitente_judicial, "): ", out, verbose = verbose)
    return(out)
  }

  if (site == "leeilon") {
    p <- httr::parse_url(out)
    if (is.null(p$query)) p$query <- list()
    p$query$modalidades <- if (opt$auction_type == "judicial") "Judicial" else "Extrajudicial"
    out <- httr::build_url(p)
    log_msg("Leeilon: filtro nativo aplicado (modalidades=", p$query$modalidades, "): ", out, verbose = verbose)
    return(out)
  }

  out
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

scrape_zuk <- function(url, max_pages = NA_integer_, sleep_sec = 0.75, user_agent, timeout_sec, handle = NULL, verbose = FALSE) {
  page_limit <- if (is.na(max_pages)) Inf else as.numeric(max_pages)
  first <- fetch_html(url, user_agent = user_agent, timeout_sec = timeout_sec, handle = handle, verbose = verbose)
  all_rows <- extract_zuk_cards(first$doc, source_url = url)

  token <- extract_zuk_token(first$doc)
  order <- extract_zuk_order(first$doc) %||% "relevancia"
  total_expected <- extract_zuk_total(first$text)
  log_msg("Zuk: lotes iniciais=", nrow(all_rows), "; total esperado=", total_expected %||% NA, verbose = verbose)

  if (is.na(token) || page_limit <= 1) {
    return(dedupe_results(all_rows))
  }

  endpoint <- "https://www.portalzuk.com.br/leilao-de-imoveis/mais"
  page_idx <- 2L

  while (page_idx <= page_limit) {
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

    resp <- tryCatch(
      post_form(
        endpoint,
        form = form,
        user_agent = user_agent,
        timeout_sec = timeout_sec,
        headers = list(
          Referer = url,
          Origin = "https://www.portalzuk.com.br",
          `X-Requested-With` = "XMLHttpRequest"
        ),
        handle = handle,
        max_retries = 8L,
        retry_base_sec = max(2, sleep_sec * 2),
        retry_max_sec = 180,
        verbose = verbose
      ),
      error = function(e) {
        log_msg(
          "Zuk: falha ao carregar pagina ", page_idx, ": ", conditionMessage(e),
          ". Encerrando com dados parciais.",
          verbose = verbose
        )
        NULL
      }
    )
    if (is.null(resp)) break

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
    adaptive_sleep <- max(0, sleep_sec + min(1.2, (page_idx - 1L) * 0.05))
    if (adaptive_sleep > 0) {
      log_msg("Zuk: pausa adaptativa ", sprintf("%.2f", adaptive_sleep), "s", verbose = verbose)
      Sys.sleep(adaptive_sleep)
    }
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

scrape_megaleiloes <- function(url, max_pages = NA_integer_, sleep_sec = 0.75, user_agent, timeout_sec, handle = NULL, verbose = FALSE) {
  page_limit <- if (is.na(max_pages)) Inf else as.numeric(max_pages)
  start_page <- suppressWarnings(as.integer(parse_query_value(url, "pagina")))
  if (is.na(start_page) || start_page < 1L) start_page <- 1L

  all_rows <- empty_results()
  last_page <- NA_integer_
  page <- start_page
  pages_done <- 0L

  repeat {
    if (pages_done >= page_limit) break
    page_url <- set_query_value(url, "pagina", page)
    fetched <- fetch_html(page_url, user_agent = user_agent, timeout_sec = timeout_sec, handle = handle, verbose = verbose)
    pages_done <- pages_done + 1L

    if (is.na(last_page)) {
      last_page <- extract_mega_last_page(fetched$doc, fallback = page)
      log_msg("MegaLeiloes: ultima pagina detectada=", last_page, verbose = verbose)
    }

    rows <- extract_mega_cards(fetched$doc, source_url = page_url)
    log_msg("MegaLeiloes: pagina ", page, " -> ", nrow(rows), " registros", verbose = verbose)
    if (nrow(rows) == 0) break

    before <- nrow(all_rows)
    all_rows <- dedupe_results(rbind_safe(all_rows, rows))
    added <- nrow(all_rows) - before
    if (added <= 0 && is.na(last_page)) {
      log_msg("MegaLeiloes: nenhum novo registro; encerrando por seguranca.", verbose = verbose)
      break
    }
    if (!is.na(last_page) && page >= last_page) break
    if (sleep_sec > 0) Sys.sleep(sleep_sec)
    page <- page + 1L
  }

  dedupe_results(all_rows)
}

slugify_ascii <- function(x) {
  x <- normalize_ws(x)
  if (is.na(x)) return(NA_character_)
  y <- suppressWarnings(iconv(x, from = "", to = "ASCII//TRANSLIT"))
  y <- normalize_ws(y)
  if (is.na(y)) return(NA_character_)
  y <- tolower(y)
  y <- gsub("[^a-z0-9\\s-]", "", y)
  y <- gsub("\\s+", "-", y)
  y <- gsub("-{2,}", "-", y)
  y <- gsub("^-|-$", "", y)
  if (!nzchar(y)) NA_character_ else y
}

leeilon_action_tree_header <- function(url) {
  p <- httr::parse_url(url)
  seg <- Filter(nzchar, strsplit(p$path %||% "", "/", fixed = TRUE)[[1]])
  slug <- if (length(seg) >= 1) seg[[1]] else "busca-leilao"
  tree_raw <- sprintf(
    '["",{"children":["(public)",{"children":["(busca)",{"children":[["slug","%s","c"],{"children":["__PAGE__",{},null,null]},null,null]},null,null]},null,null]},null,null,true]',
    slug
  )
  utils::URLencode(tree_raw, reserved = TRUE)
}

parse_leeilon_action_payload <- function(txt) {
  lines <- unlist(strsplit(txt %||% "", "\n", fixed = TRUE), use.names = FALSE)
  if (length(lines) == 0) return(NULL)

  err <- lines[grepl("^[0-9]+:E\\{", lines)]
  if (length(err) > 0) {
    stop("Leeilon action returned an error payload: ", err[[1]])
  }

  json_lines <- lines[grepl("^[0-9]+:\\{", lines)]
  if (length(json_lines) == 0) return(NULL)

  idx <- which(grepl("\"imoveis\"\\s*:", json_lines))[1]
  line <- if (!is.na(idx)) json_lines[[idx]] else json_lines[[length(json_lines)]]
  line <- sub("^[0-9]+:", "", line)
  jsonlite::fromJSON(line, simplifyDataFrame = FALSE)
}

leeilon_listing_url <- function(item) {
  id <- normalize_ws(item$id %||% NA_character_)
  if (is.na(id)) return(NA_character_)
  estado <- toupper(gsub("[^A-Za-z0-9-]", "", normalize_ws(item$estado %||% "")))
  cidade <- slugify_ascii(item$cidade %||% NA_character_)
  titulo <- slugify_ascii(item$titulo_propriedade %||% NA_character_)
  if (!nzchar(estado) || is.na(cidade) || is.na(titulo)) return(NA_character_)
  paste0("https://www.leeilon.com.br/imovel-em-leilao/", estado, "/", cidade, "/", titulo, "/", id)
}

extract_leeilon_rows <- function(items, source_url) {
  if (length(items) == 0) return(empty_results())
  if (!is.list(items)) return(empty_results())

  scraped_at <- format(Sys.time(), "%Y-%m-%d %H:%M:%S %z")
  rows <- vector("list", length(items))

  for (i in seq_along(items)) {
    item <- items[[i]]
    listing_url <- leeilon_listing_url(item)
    listing_id <- normalize_ws(item$id %||% NA_character_)
    if (is.na(listing_id)) listing_id <- extract_id_from_url(listing_url)

    city <- normalize_ws(item$cidade %||% NA_character_)
    uf <- normalize_ws(item$estado %||% NA_character_)
    city_uf <- if (!is.na(city) && !is.na(uf)) paste(city, uf, sep = "/") else normalize_ws(paste(city, uf))

    endereco <- normalize_ws(item$endereco %||% NA_character_)
    bairro <- normalize_ws(item$bairro %||% NA_character_)
    address_parts <- c(endereco, bairro, city_uf)
    address_parts <- address_parts[!is.na(address_parts)]
    address <- if (length(address_parts) > 0) paste(address_parts, collapse = " | ") else NA_character_

    auction_date_1 <- normalize_ws(item$data_1_praca %||% NA_character_)
    auction_date_2 <- normalize_ws(item$data_2_praca %||% NA_character_)
    dt1 <- parse_datetime_br(auction_date_1)
    dt2 <- parse_datetime_br(auction_date_2)

    price_1 <- parse_money_br(item$valor_1_praca %||% item$lance_inicial %||% NA_character_)
    price_2 <- parse_money_br(item$valor_2_praca %||% NA_character_)
    min_price <- suppressWarnings(min(c(price_1, price_2), na.rm = TRUE))
    if (!is.finite(min_price)) min_price <- NA_real_

    rows[[i]] <- data.frame(
      site = "leeilon",
      listing_id = listing_id,
      title = normalize_ws(item$titulo_propriedade %||% NA_character_),
      property_type = normalize_ws(item$tipo_propriedade %||% NA_character_),
      city_uf = city_uf,
      address = address,
      area_info = normalize_ws(item$descricao_propriedade %||% NA_character_),
      auction_type = normalize_ws(item$tipo_leilao %||% NA_character_),
      lot = normalize_ws(item$numero_processo %||% NA_character_),
      status = NA_character_,
      price_1 = price_1,
      price_2 = price_2,
      min_price = min_price,
      auction_date_1 = auction_date_1,
      auction_date_2 = auction_date_2,
      auction_datetime_1 = fmt_datetime(dt1),
      auction_datetime_2 = fmt_datetime(dt2),
      image_url = normalize_ws(item$imagem_propriedade %||% NA_character_),
      listing_url = listing_url,
      source_url = source_url,
      scraped_at = scraped_at,
      stringsAsFactors = FALSE
    )
  }

  dedupe_results(do.call(rbind, rows))
}

fetch_leeilon_action <- function(page_url, query, action_id, tree_header, user_agent, timeout_sec, handle = NULL, verbose = FALSE) {
  body_obj <- lapply(query %||% list(), function(v) {
    txt <- normalize_ws(as.character(v))
    if (is.na(txt)) return(NA_character_)
    gsub("+", " ", txt, fixed = TRUE)
  })
  body_obj <- body_obj[!vapply(body_obj, function(v) is.na(v) || !nzchar(v), logical(1))]
  body_json <- jsonlite::toJSON(list(body_obj), auto_unbox = TRUE, null = "null")

  log_msg("POST ", page_url, " (Leeilon action page=", body_obj$page %||% "?", ")", verbose = verbose)
  resp <- httr::POST(
    page_url,
    httr::user_agent(user_agent),
    httr::timeout(timeout_sec),
    httr::add_headers(
      Accept = "text/x-component",
      `next-action` = action_id,
      `next-router-state-tree` = tree_header,
      Referer = page_url,
      `Content-Type` = "text/plain;charset=UTF-8"
    ),
    handle = handle,
    body = body_json,
    encode = "raw"
  )

  txt <- httr::content(resp, as = "text", encoding = "UTF-8")
  status <- httr::status_code(resp)
  if (status >= 400) {
    stop("Leeilon action HTTP ", status, " for ", page_url, "; payload=", substr(txt, 1, 250))
  }
  payload <- parse_leeilon_action_payload(txt)
  if (is.null(payload)) {
    stop("Leeilon action returned an unrecognized payload for ", page_url)
  }
  payload
}

scrape_leeilon <- function(url, max_pages = NA_integer_, sleep_sec = 0.75, user_agent, timeout_sec, handle = NULL, verbose = FALSE) {
  page_limit <- if (is.na(max_pages)) Inf else as.numeric(max_pages)
  start_page <- suppressWarnings(as.integer(parse_query_value(url, "page")))
  if (is.na(start_page) || start_page < 1L) start_page <- 1L

  action_id <- "40933a495f66da989a9e402fa0b5fc34dab540b470"
  tree_header <- leeilon_action_tree_header(url)
  all_rows <- empty_results()
  last_page <- NA_integer_
  page <- start_page
  pages_done <- 0L

  repeat {
    if (pages_done >= page_limit) break
    page_url <- set_query_value(url, "page", page)
    query <- httr::parse_url(page_url)$query %||% list()
    query$page <- as.character(page)

    payload <- fetch_leeilon_action(
      page_url = page_url,
      query = query,
      action_id = action_id,
      tree_header = tree_header,
      user_agent = user_agent,
      timeout_sec = timeout_sec,
      handle = handle,
      verbose = verbose
    )
    pages_done <- pages_done + 1L

    tp <- suppressWarnings(as.integer(payload$total_pages %||% NA_integer_))
    if (is.na(last_page) && !is.na(tp)) {
      last_page <- tp
      log_msg("Leeilon: ultima pagina detectada=", last_page, verbose = verbose)
    }

    rows <- extract_leeilon_rows(payload$imoveis %||% list(), source_url = page_url)
    log_msg("Leeilon: pagina ", page, " -> ", nrow(rows), " registros", verbose = verbose)
    if (nrow(rows) == 0) break

    before <- nrow(all_rows)
    all_rows <- dedupe_results(rbind_safe(all_rows, rows))
    added <- nrow(all_rows) - before
    if (added <= 0 && page > start_page) {
      log_msg("Leeilon: nenhum novo registro; encerrando por seguranca.", verbose = verbose)
      break
    }

    if (!is.na(last_page) && page >= last_page) break
    if (sleep_sec > 0) Sys.sleep(sleep_sec)
    page <- page + 1L
  }

  dedupe_results(all_rows)
}

parse_max_pages_arg <- function(val) {
  txt <- tolower(trimws(as.character(val)))
  if (!nzchar(txt)) return(NA_integer_)
  if (txt %in% c("all", "todas", "tudo", "0", "-1", "inf", "infinite", "max")) {
    return(NA_integer_)
  }
  n <- suppressWarnings(as.integer(txt))
  if (is.na(n)) {
    stop("--max-pages must be an integer >= 1, or one of: all, 0, -1, inf")
  }
  n
}

max_pages_label <- function(x) {
  if (is.na(x)) "all" else as.character(x)
}

localize_output_columns <- function(df) {
  if (nrow(df) == 0) return(df)
  map <- c(
    site = "site",
    listing_id = "id_imovel",
    title = "titulo",
    property_type = "tipo_imovel",
    city_uf = "cidade_uf",
    address = "endereco",
    area_info = "informacoes_area",
    auction_type = "modalidade_leilao",
    lot = "lote",
    status = "status",
    price_1 = "valor_1_leilao",
    price_2 = "valor_2_leilao",
    min_price = "menor_valor",
    auction_date_1 = "data_1_leilao",
    auction_date_2 = "data_2_leilao",
    auction_datetime_1 = "datahora_1_leilao",
    auction_datetime_2 = "datahora_2_leilao",
    image_url = "url_imagem",
    listing_url = "url_imovel",
    source_url = "url_busca_origem",
    scraped_at = "coletado_em",
    auction_type_norm = "modalidade_normalizada",
    current_round = "fase_atual",
    next_auction_datetime = "proximo_leilao_em"
  )
  current <- colnames(df)
  renamed <- ifelse(current %in% names(map), unname(map[current]), current)
  colnames(df) <- renamed
  df
}

usage <- function() {
  cat(
    "Uso:\n",
    "  Rscript scripts/scrape_leiloes.R --url <URL> [opcoes]\n\n",
    "Opcoes:\n",
    "  --site <auto|leeilon|zuk|megaleiloes>   Site alvo (padrao: auto)\n",
    "  --max-pages <N|all>                      Maximo de paginas/blocos (padrao: all)\n",
    "  --sleep <segundos>                       Espera entre requisicoes (padrao: 0.75)\n",
    "  --auction-type <any|judicial|extrajudicial>  Filtra por modalidade\n",
    "  --current-round <any|1|2|ended>          Filtra pela fase atual do leilao\n",
    "  --date-from <YYYY-MM-DD>                 Inicio da janela de data\n",
    "  --date-to <YYYY-MM-DD>                   Fim da janela de data\n",
    "  --date-field <next|first|second|any>     Campo de data para filtro (padrao: next)\n",
    "  --format <csv|json|xlsx>                 Formato de saida (padrao: csv)\n",
    "  --out <arquivo>                          Caminho do arquivo de saida\n",
    "  --verbose                                Logs detalhados (padrao: ligado)\n",
    "  --quiet                                  Sem logs\n",
    "  --help                                   Mostra esta ajuda\n\n",
    "Autenticacao (variaveis de ambiente):\n",
    "  LEILOES_LOGIN_USER / LEILOES_LOGIN_PASS\n",
    "  LEILOES_<SITE>_USER / LEILOES_<SITE>_PASS (ex.: LEILOES_ZUK_USER)\n\n",
    "Exemplos:\n",
    "  Rscript scripts/scrape_leiloes.R --url \"https://www.portalzuk.com.br/leilao-de-imoveis\"\n",
    "  Rscript scripts/scrape_leiloes.R --url \"https://www.megaleiloes.com.br/imoveis?pagina=1\" --site megaleiloes\n",
    "  Rscript scripts/scrape_leiloes.R --url \"https://www.portalzuk.com.br/leilao-de-imoveis\" --site zuk --max-pages 5\n",
    "  Rscript scripts/scrape_leiloes.R --url \"https://www.megaleiloes.com.br/imoveis\" --auction-type judicial --current-round 2 --date-from 2026-03-01 --date-to 2026-03-31\n",
    sep = ""
  )
}

parse_args <- function(args) {
  opt <- list(
    url = NULL,
    site = "auto",
    max_pages = NA_integer_,
    sleep = 0.75,
    auction_type = "any",
    current_round = "any",
    date_from = NA_character_,
    date_to = NA_character_,
    date_field = "next",
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
        opt$max_pages <- parse_max_pages_arg(val)
      } else if (key == "sleep") {
        opt$sleep <- as.numeric(val)
      } else if (key == "auction-type") {
        opt$auction_type <- tolower(trimws(val))
      } else if (key == "current-round") {
        opt$current_round <- tolower(trimws(val))
      } else if (key == "date-from") {
        opt$date_from <- val
      } else if (key == "date-to") {
        opt$date_to <- val
      } else if (key == "date-field") {
        opt$date_field <- tolower(trimws(val))
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
  } else if (fmt == "xlsx") {
    if (!requireNamespace("writexl", quietly = TRUE)) {
      stop("Missing package: writexl. Install with: install.packages('writexl')")
    }
    writexl::write_xlsx(list(resultado = df), path = out_path)
  } else {
    # UTF-8 BOM + ';' separator improves accent/column rendering in Excel pt-BR.
    tmp <- tempfile(fileext = ".csv")
    on.exit(unlink(tmp), add = TRUE)
    utils::write.table(
      df,
      file = tmp,
      sep = ";",
      dec = ",",
      row.names = FALSE,
      col.names = TRUE,
      quote = TRUE,
      na = "",
      fileEncoding = "UTF-8"
    )
    raw_csv <- readBin(tmp, what = "raw", n = file.info(tmp)$size)
    con <- file(out_path, open = "wb")
    on.exit(close(con), add = TRUE)
    writeBin(as.raw(c(0xEF, 0xBB, 0xBF)), con)
    writeBin(raw_csv, con)
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
  if (!is.na(opt$max_pages) && opt$max_pages < 1) {
    stop("--max-pages must be >= 1")
  }
  if (is.na(opt$sleep) || opt$sleep < 0) {
    stop("--sleep must be >= 0")
  }
  if (!opt$format %in% c("csv", "json", "xlsx")) {
    stop("--format must be csv, json or xlsx")
  }
  if (!opt$auction_type %in% c("any", "judicial", "extrajudicial")) {
    stop("--auction-type must be one of: any, judicial, extrajudicial")
  }
  if (!opt$current_round %in% c("any", "1", "2", "ended")) {
    stop("--current-round must be one of: any, 1, 2, ended")
  }
  if (!opt$date_field %in% c("next", "first", "second", "any")) {
    stop("--date-field must be one of: next, first, second, any")
  }

  site <- normalize_site(opt$site)
  if (is.na(site)) stop("Unsupported --site value: ", opt$site)
  if (site == "auto") {
    site <- detect_site_from_url(opt$url)
    if (is.na(site)) {
      stop("Could not detect site from URL. Use --site leeilon|zuk|megaleiloes.")
    }
  }

  opt$url <- canonical_search_url(opt$url, site = site)

  user_agent <- "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/132.0 Safari/537.36"
  timeout_sec <- 90

  log_msg("Site detectado: ", site, verbose = opt$verbose)
  log_msg("URL: ", opt$url, verbose = opt$verbose)
  log_msg("Max pages: ", max_pages_label(opt$max_pages), verbose = opt$verbose)
  log_msg("Filtro auction_type: ", opt$auction_type, "; current_round: ", opt$current_round, verbose = opt$verbose)
  log_msg("Filtro data: field=", opt$date_field, "; from=", opt$date_from %||% "", "; to=", opt$date_to %||% "", verbose = opt$verbose)

  scrape_url <- apply_site_native_filters(opt$url, site = site, opt = opt, verbose = opt$verbose)
  session <- start_site_session(
    site = site,
    url = scrape_url,
    user_agent = user_agent,
    timeout_sec = timeout_sec,
    verbose = opt$verbose
  )

  res <- switch(
    site,
    "zuk" = scrape_zuk(
      url = scrape_url,
      max_pages = opt$max_pages,
      sleep_sec = opt$sleep,
      user_agent = user_agent,
      timeout_sec = timeout_sec,
      handle = session$handle,
      verbose = opt$verbose
    ),
    "megaleiloes" = scrape_megaleiloes(
      url = scrape_url,
      max_pages = opt$max_pages,
      sleep_sec = opt$sleep,
      user_agent = user_agent,
      timeout_sec = timeout_sec,
      handle = session$handle,
      verbose = opt$verbose
    ),
    "leeilon" = scrape_leeilon(
      url = scrape_url,
      max_pages = opt$max_pages,
      sleep_sec = opt$sleep,
      user_agent = user_agent,
      timeout_sec = timeout_sec,
      handle = session$handle,
      verbose = opt$verbose
    ),
    stop("Unhandled site: ", site)
  )

  res <- dedupe_results(res)
  res <- apply_report_filters(res, opt = opt, verbose = opt$verbose)
  res <- res[order(res$site, res$listing_id, res$listing_url), , drop = FALSE]
  res <- localize_output_columns(res)

  if (is.null(opt$out) || !nzchar(opt$out)) {
    ts <- format(Sys.time(), "%Y%m%d_%H%M%S")
    ext <- ifelse(opt$format == "json", "json", ifelse(opt$format == "xlsx", "xlsx", "csv"))
    opt$out <- file.path(getwd(), sprintf("leiloes_%s_%s.%s", site, ts, ext))
  }

  write_output(res, opt$out, opt$format)

  cat("Registros:", nrow(res), "\n")
  cat("Site:", site, "\n")
  cat("Arquivo:", normalizePath(opt$out, winslash = "/", mustWork = FALSE), "\n")
}

main()

