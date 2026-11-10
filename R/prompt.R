#' The request for a written interpretation of one analysis
#'
#' @description
#' \code{sa_prompt()} builds the text sent to an external text service (\code{\link{sa_ask}()}) for a written
#' interpretation of one result of the package \pkg{stability}. It is specific to the method: it contains how the
#' statistics of that method are read, what to concentrate on, the interpretation computed by
#' \code{stability::stab_interpret()} (to be kept to), the facts behind it and the first rows of the main table.
#'
#' @details
#' The text has two parts: \code{system} (the role and the rules: use only the numbers given, keep to the computed
#' findings, name genotypes and environments as given, say when something cannot be concluded) and \code{user} (the
#' material and the task: four sections, Statistical interpretation, Key agronomic findings, Recommendations and
#' Next steps, at most about 350 words). Because the computed findings are part of the request, the written text
#' cannot contradict the tests and rankings of the analysis without contradicting its own material, which the rules
#' forbid.
#'
#' @param x a result of a function of \pkg{stability} (or the \code{ggplot} of \code{stability::gge_biplot()}).
#' @param higher_better \code{TRUE} (default) when larger values of the trait are better.
#' @param trait name of the trait (default: the response of the analysis).
#' @param rows number of rows of the main table included (default \code{15}).
#' @return A list with \code{system} and \code{user} (character strings) and \code{interpretation} (the result of
#'   \code{stability::stab_interpret()}).
#' @author Muhammad Yaseen (\email{myaseen208@@gmail.com})
#' @seealso \code{\link{sa_ask}}, \code{\link{StabilityApp}}
#' @examples
#' fit <-
#'   stability::ammi(
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
#'     , trait         = "grain yield (kg/ha)"
#'     , rows          = 15
#'     )
#' cat(p$user)
#' @export
sa_prompt <- function(x, higher_better = TRUE, trait = NULL, rows = 15L) {
  it  <- stability::stab_interpret(x = x, higher_better = higher_better, top = 8L)
  reg <- .sa_registry()
  spec <- reg[[.sa_reg_id(it$analysis)]]
  meta <- if (!is.null(attr(x, "gge"))) attr(x, "gge")$meta else attr(x, "meta")
  if (is.null(trait)) trait <- if (!is.null(meta$response)) meta$response else "the trait"
  size <- if (!is.null(meta$genotypes)) sprintf("%d genotypes in %d environments%s; grand mean %s", meta$genotypes,
                                                meta$environments,
                                                if (is.na(meta$replicates)) "" else sprintf(", %s replicates", format(round(meta$replicates, 2))),
                                                format(signif(meta$grand_mean, 5))) else "see the table"
  tabs <- spec$tables(x)
  main <- tabs[[1L]]
  main <- utils::head(main, rows)
  num  <- vapply(main, is.numeric, NA)
  main[num] <- lapply(main[num], function(v) signif(v, 4L))
  tab_txt <- paste(utils::capture.output(print(main, row.names = FALSE)), collapse = "\n")
  facts <- .sa_facts_text(it$facts)
  system <- paste(
      "You write the interpretation of one statistical analysis of a multi-environment plant-breeding trial for plant",
      "breeders and agronomists. Rules: use only the numbers, names and findings given in the material; do not",
      "contradict the computed findings; name genotypes and environments exactly as given; do not invent values,",
      "locations or seasons; when the material does not allow a conclusion, say so. Write plain, precise English.")
  user <- paste0(
      "Analysis: ", it$title, " (function ", spec$id, " of the R package stability)\n",
      "Trait: ", trait, "; ", if (higher_better) "higher values are better" else "lower values are better", "\n",
      "Trial: ", size, "\n\n",
      "How this method is read:\n", it$guide, "\n\n",
      "Concentrate on: ", spec$focus, ".\n\n",
      "Computed findings (keep to them):\n", .sa_lines(c(it$headline, it$findings, it$cautions)), "\n\n",
      "Facts:\n", facts, "\n\n",
      "Main table (first ", nrow(main), " rows): ", names(tabs)[1L], "\n", tab_txt, "\n\n",
      "Task: write four sections with exactly these headings:\n",
      "## 1. Statistical interpretation\n(3 to 4 sentences: what the tests and statistics of this method show)\n",
      "## 2. Key agronomic findings\n(3 to 4 sentences: the genotypes and environments that matter, as this method sees them)\n",
      "## 3. Recommendations\n(2 to 3 sentences for selection or variety recommendation)\n",
      "## 4. Next steps\n(1 to 2 sentences: checks or further analyses)\n",
      "At most 350 words.")
  list(system = system, user = user, interpretation = it)
}

.sa_lines <- function(s) paste0("- ", s, collapse = "\n")

# the facts of an interpretation as lines "name: value"
.sa_facts_text <- function(f) {
  one <- function(v) {
    if (is.null(v) || !length(v)) return("none")
    if (is.data.frame(v)) return(paste(utils::capture.output(print(utils::head(v, 10L), row.names = FALSE)), collapse = "\n"))
    if (is.list(v)) return(paste(sprintf("%s: %s", names(v), vapply(v, function(z) paste(z, collapse = ", "), "")),
                                 collapse = "; "))
    if (is.numeric(v)) {
      s <- signif(v, 4L)
      return(paste(if (!is.null(names(v))) paste0(names(v), " = ", s) else s, collapse = ", "))
    }
    paste(if (!is.null(names(v))) paste0(names(v), " = ", v) else v, collapse = ", ")
  }
  paste(sprintf("%s: %s", names(f), vapply(f, one, "")), collapse = "\n")
}

# the registry entry of an interpretation class
.sa_reg_id <- function(cls) {
  map <- c(reg = "stab_reg", measures = "stab_measures", par = "stab_par", kang = "stab_kang", fox = "stab_fox",
           asv = "stab_asv", masv = "stab_masv", dist = "stab_dist", eskridge = "stab_eskridge",
           reliability = "stab_reliability", superiority = "stab_superiority", huehn = "stab_huehn")
  if (cls %in% names(map)) map[[cls]] else cls
}
