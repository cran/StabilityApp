#' External text services for a written interpretation
#'
#' @description
#' \code{sa_services()} lists the external text services the application can ask for a longer written
#' interpretation, and \code{sa_ask()} sends one request to such a service and returns its text. Using a service is
#' optional: the application always shows the interpretation of \code{stability::stab_interpret()}, which is
#' computed from the results and needs no connection.
#'
#' @details
#' The services are read from the file \code{services.csv} of the package (columns \code{id}, \code{label},
#' \code{format}, \code{url}, \code{model}, \code{key_env}, and \code{header}, an extra header \code{name=value}).
#' Three request formats are supported:
#' \code{"messages"}, \code{"chat"} and \code{"generate"}, the formats of the three services in the file. The key is
#' taken from the argument \code{key} or, when it is empty, from the environment variable named in \code{key_env},
#' so that it need not be typed into the application. The request is sent over HTTPS to the service's own address;
#' the key is never stored by the package.
#'
#' What is sent is the text built by \code{\link{sa_prompt}()}: the name of the analysis, the trait, the size of the
#' trial, the interpretation of \code{stab_interpret()}, its facts and the first rows of the main table. No data file
#' is sent.
#'
#' @param service identifier of a service (column \code{id} of \code{sa_services()}).
#' @param prompt a list with elements \code{system} and \code{user} (as returned by \code{\link{sa_prompt}()}), or a
#'   single character string.
#' @param model model name (default: the one of \code{sa_services()}).
#' @param key the user's key for the service; \code{""} (default) uses the environment variable of the service.
#' @param max_tokens largest length of the answer, in tokens (default \code{1500}).
#' @param timeout seconds to wait (default \code{90}).
#' @return \code{sa_services()}: a data frame of the services. \code{sa_ask()}: the text of the answer (character),
#'   or an error with the message of the service.
#' @author Muhammad Yaseen (\email{myaseen208@@gmail.com})
#' @seealso \code{\link{sa_prompt}}, \code{\link{StabilityApp}}
#' @examples
#' sa_services()[, c("id", "format", "model")]
#' \dontrun{
#' fit <-
#'   stability::stab_reg(
#'       .data = stability::ge_data
#'     , .y    = Yield
#'     , .rep  = Rep
#'     , .gen  = Gen
#'     , .env  = Env
#'     )
#' p <-
#'   sa_prompt(
#'       x             = fit
#'     , higher_better = TRUE
#'     , trait         = NULL
#'     , rows          = 15L
#'     )
#' sa_ask(
#'     service    = sa_services()$id[1]
#'   , prompt     = p
#'   , model      = sa_services()$model[1]
#'   , key        = ""
#'   , max_tokens = 1500
#'   , timeout    = 90
#'   )
#' }
#' @export
sa_services <- function() {
  f <- system.file("services", "services.csv", package = "StabilityApp")
  utils::read.csv(f, stringsAsFactors = FALSE)
}

#' @rdname sa_services
#' @export
sa_ask <- function(service, prompt, model = NULL, key = "", max_tokens = 1500, timeout = 90) {
  fn  <- "sa_ask"
  sv  <- sa_services()
  if (!service %in% sv$id) .sa_stop(fn, "unknown service \"", service, "\"; see sa_services().")
  s   <- sv[sv$id == service, ]
  model <- if (is.null(model) || !nzchar(model)) s$model else model
  if (!nzchar(key)) key <- Sys.getenv(s$key_env)
  if (!nzchar(key)) .sa_stop(fn, "no key: enter it, or set the environment variable ", s$key_env, ".")
  if (is.character(prompt)) prompt <- list(system = "", user = prompt)
  url <- sub("{model}", utils::URLencode(model, reserved = TRUE), s$url, fixed = TRUE)
  req <- httr2::request(url)
  # an extra header of the service (column `header`, "name=value"), such as the version of its interface
  if (!is.na(s$header) && nzchar(s$header)) {
    h <- strsplit(s$header, "=", fixed = TRUE)[[1L]]
    req <- httr2::req_headers(req, !!!stats::setNames(list(h[2L]), h[1L]))
  }
  req <- switch(
      s$format
    , messages = httr2::req_body_json(
          httr2::req_headers(req, `x-api-key` = key)
        , list(model = model, max_tokens = max_tokens, system = prompt$system,
               messages = list(list(role = "user", content = prompt$user)))
        )
    , chat = httr2::req_body_json(
          httr2::req_headers(req, Authorization = paste("Bearer", key))
        , list(model = model, max_tokens = max_tokens,
               messages = list(list(role = "system", content = prompt$system), list(role = "user", content = prompt$user)))
        )
    , generate = httr2::req_body_json(
          httr2::req_headers(req, `x-goog-api-key` = key)
        , list(system_instruction = list(parts = list(list(text = prompt$system))),
               contents = list(list(role = "user", parts = list(list(text = prompt$user)))),
               generationConfig = list(maxOutputTokens = max_tokens))
        )
    , .sa_stop(fn, "unknown request format \"", s$format, "\".")
    )
  req  <- httr2::req_timeout(httr2::req_error(req, is_error = function(resp) FALSE), timeout)
  resp <- tryCatch(httr2::req_perform(req), error = function(e) .sa_stop(fn, "no answer: ", conditionMessage(e)))
  body <- tryCatch(httr2::resp_body_json(resp), error = function(e) NULL)
  if (httr2::resp_status(resp) >= 400L) {
    msg <- if (!is.null(body$error$message)) body$error$message else httr2::resp_status_desc(resp)
    .sa_stop(fn, "the service answered ", httr2::resp_status(resp), ": ", msg)
  }
  txt <- switch(
      s$format
    , messages = paste(vapply(body$content, function(p) if (is.null(p$text)) "" else p$text, ""), collapse = "")
    , chat     = body$choices[[1L]]$message$content
    , generate = paste(vapply(body$candidates[[1L]]$content$parts, function(p) if (is.null(p$text)) "" else p$text, ""),
                       collapse = "")
    )
  if (is.null(txt) || !nzchar(txt)) .sa_stop(fn, "the service returned no text.")
  txt
}
