test_that("the services are listed with a supported request format", {
  s <- sa_services()
  expect_true(all(c("id", "label", "format", "url", "model", "key_env") %in% names(s)))
  expect_true(all(s$format %in% c("messages", "chat", "generate")))
  expect_true(all(startsWith(s$url, "https://")))
})

test_that("sa_ask() builds the request of every format and reads the answer", {
  s <- sa_services()
  answers <- list(
      messages = list(content = list(list(type = "text", text = "## 1. Statistical interpretation\nText.")))
    , chat     = list(choices = list(list(message = list(role = "assistant", content = "## 1. Statistical interpretation\nText."))))
    , generate = list(candidates = list(list(content = list(parts = list(list(text = "## 1. Statistical interpretation\nText."))))))
    )
  for (i in seq_len(nrow(s))) {
    seen <- NULL
    mock <- function(req) {
      seen <<- req
      httr2::response_json(body = answers[[s$format[i]]])
    }
    txt <- httr2::with_mocked_responses(mock, sa_ask(service = s$id[i], prompt = list(system = "S", user = "U"),
                                                      model = "m1", key = "k123"))
    expect_match(txt, "Statistical interpretation")
    body <- seen$body$data
    expect_true(grepl("m1", seen$url) || identical(body$model, "m1"))
    # httr2 >= 1.1.0 redacts the key headers (Authorization, x-api-key, ...) inside the request; reveal them here
    hdr <- tryCatch(unlist(httr2::req_get_headers(seen, redacted = "reveal")), error = function(e) NULL)
    if (is.null(hdr)) hdr <- unlist(lapply(seen$headers, function(v) if (is.function(v)) v() else as.character(v)))
    expect_true(any(grepl("k123", hdr)))
    if (!is.na(s$header[i]) && nzchar(s$header[i])) {
      expect_true(any(hdr == sub("^[^=]*=", "", s$header[i])))
    }
  }
})

test_that("errors of a service are reported with its message", {
  s <- sa_services()
  mock <- function(req) httr2::response_json(status_code = 401L, body = list(error = list(message = "invalid key")))
  expect_error(httr2::with_mocked_responses(mock, sa_ask(service = s$id[1], prompt = "x", key = "bad")), "invalid key")
  withr::local_envvar(stats::setNames("", s$key_env[1]))
  expect_error(sa_ask(service = s$id[1], prompt = "x", key = ""), "no key")
  expect_error(sa_ask(service = "none", prompt = "x", key = "k"), "unknown service")
})
