#-------------------------------------------------------------------------------------------------------------------
# The analyses of the application: one entry per function of stability
#-------------------------------------------------------------------------------------------------------------------

# a table of a result as a data frame (anova tables and matrices keep their row names in a first column)
.sa_df <- function(x, first = "Source") {
  if (data.table::is.data.table(x)) return(as.data.frame(x))
  d <- as.data.frame(x, check.names = FALSE, stringsAsFactors = FALSE)
  rn <- rownames(x)
  if (!is.null(rn) && !identical(rn, as.character(seq_len(nrow(d))))) {
    d <- cbind(stats::setNames(data.frame(trimws(rn), stringsAsFactors = FALSE), first), d)
  }
  rownames(d) <- NULL
  d
}

.sa_registry <- function() {
  # hb: the analysis takes `higher_better`, set from the Data page
  e <- function(id, label, group, rep = c("required", "optional", "no"), plots = NULL, tables, options = list(),
                focus, hb = FALSE) {
    list(id = id, label = label, group = group, rep = match.arg(rep), plots = plots, tables = tables,
         options = options, focus = focus, hb = hb)
  }
  list(
      ge_means = e("ge_means", "Means and rankings", "Tables", "no", c(heatmap = "heatmap", ranks = "ranks"),
                   function(x) list(`Means` = .sa_df(x$ge_means), `Environment means` = .sa_df(x$e_means),
                                    `Genotype means` = .sa_df(x$g_means),
                                    `Ranks in each environment` = .sa_df(x$ge_ranks, "Environment")),
                   focus = "the best genotype of each environment, how often the best changes, and the rank correlations between environments (crossover interaction)")
    , ge_effects = e("ge_effects", "Interaction effects", "Tables", "no", c(interaction = "ge", `genotype plus interaction` = "gge"),
                     function(x) list(`Interaction effects` = .sa_df(x$ge_effects, "Genotype"),
                                      `Genotype plus interaction effects` = .sa_df(x$gge_effects, "Genotype")),
                     focus = "the largest positive interaction effects (specific adaptation) and the genotypes with the smallest interaction")
    , indiv_anova = e("indiv_anova", "Each environment", "Analyses of variance", "required",
                      c(`environment means` = "means", `error mean squares` = "error", residuals = "residuals", `normal quantiles` = "qq"),
                      function(x) list(`Each environment` = .sa_df(x$Summary), `Homogeneity of error variances` = .sa_df(x$Homogeneity)),
                      focus = "where genotypes differ, the precision (CV) of each environment, and whether the error variances can be pooled")
    , add_anova = e("add_anova", "Combined analysis", "Analyses of variance", "required",
                    c(residuals = "residuals", `normal quantiles` = "qq"),
                    function(x) list(`Analysis of variance` = .sa_df(x$anova), Summary = .sa_df(x$Summary)),
                    focus = "the significance and relative size of environments, genotypes and their interaction, and the precision of the trial")
    , er_anova = e("er_anova", "Eberhart and Russell", "Analyses of variance", "required",
                   c(`mean against slope` = "slope", residuals = "residuals", `normal quantiles` = "qq"),
                   function(x) list(`Eberhart and Russell's analysis` = .sa_df(x$er_anova), Parameters = .sa_df(x$Parameters)),
                   focus = "the linear and non-linear parts of the interaction, and the genotypes meeting Eberhart and Russell's criteria (high mean, slope 1, small deviations)")
    , stab_reg = e("stab_reg", "Finlay and Wilkinson", "Regression and variance", "optional",
                   c(`mean against slope` = "slope", `regression lines` = "lines"),
                   function(x) list(`Regression of each genotype` = .sa_df(x$StabIndvReg)),
                   focus = "the slopes read together with the means: general adaptability, adaptation to favourable or to poor environments, and predictability (R squared, deviations)")
    , stab_measures = e("stab_measures", "Variance measures", "Regression and variance", "no",
                        c(variance = "var", ecovalence = "ecov", `stability variance` = "shukla"),
                        function(x) list(`Stability measures` = .sa_df(x$StabMeasures)),
                        focus = "static stability (variance) against dynamic stability (ecovalence, stability variance) and the genotypes combining high mean with small ecovalence")
    , stab_par = e("stab_par", "Shukla and Kang", "Regression and variance", "required",
                   c(`yield-stability statistic` = "ys", `stability variance` = "sigma"),
                   function(x) list(`Analysis of variance` = .sa_df(x$ANOVA), `Stability parameters` = .sa_df(x$StabPar),
                                    `Yield-stability statistic` = .sa_df(x$SimultSel)),
                   options = list(alpha = list(type = "numeric", label = "Level for the rating -2 (alpha)", value = 0.1,
                                               min = 0.05, max = 0.5, step = 0.05)),
                   focus = "the genotypes with significant stability variance (unstable) and the genotypes selected by Kang's yield-stability statistic")
    , stab_kang = e("stab_kang", "Kang's rank-sum", "Regression and variance", "no", c(ranks = "ranks"),
                    function(x) list(`Rank-sum` = .sa_df(x$Kang)),
                    focus = "the genotypes with the smallest rank-sum of yield and stability variance")
    , stab_fox = e("stab_fox", "Fox's top third", "Regression and variance", "no", c(counts = "counts"),
                   function(x) list(`Top-third counts` = .sa_df(x$Fox)),
                   focus = "how often each genotype ranks in the top third of an environment (wide adaptation)")
    , stab_eskridge = e("stab_eskridge", "Eskridge's safety-first", "Safety-first and nonparametric", "no",
                        c(indices = "indices", ranks = "ranks"),
                        function(x) list(`Safety-first indices` = .sa_df(x$Eskridge), Parameters = .sa_df(x$Parameters),
                                         `Rank correlations` = .sa_df(x$RankCor, "Index")),
                        options = list(alpha = list(type = "numeric", label = "Risk of a low yield (alpha)", value = 0.05,
                                                    min = 0.01, max = 0.5, step = 0.01)),
                        focus = "the genotypes with the largest lower confidence limits of yield under each definition of stability, and how much stability changes the choice made on the mean")
    , stab_reliability = e("stab_reliability", "Annicchiarico's reliability", "Safety-first and nonparametric", "no", c(index = "index"),
                           function(x) list(`Reliability index` = .sa_df(x$Reliability)),
                           options = list(alpha = list(type = "numeric", label = "Risk (alpha)", value = 0.25, min = 0.01,
                                                       max = 0.5, step = 0.01)),
                           focus = "the genotypes most likely to exceed the environment mean, on the relative scale", hb = TRUE)
    , stab_superiority = e("stab_superiority", "Lin and Binns' superiority", "Safety-first and nonparametric", "no",
                           c(superiority = "superiority"),
                           function(x) list(`Superiority measure` = .sa_df(x$Superiority)),
                           focus = "the genotypes closest to the best genotype of every environment, and whether their distance comes from the mean or from interaction",
                           hb = TRUE)
    , stab_huehn = e("stab_huehn", "Huehn's nonparametric", "Safety-first and nonparametric", "no", c(`mean against S1` = "mean"),
                     function(x) list(`Huehn's statistics` = .sa_df(x$Huehn), Tests = .sa_df(x$Tests)),
                     focus = "the genotypes that keep their rank across environments, with the tests of Nassar and Huehn")
    , ammi = e("ammi", "AMMI analysis", "AMMI and GGE", "required",
               c(`principal components` = "scree", `AMMI1 biplot` = "ammi1", `AMMI2 biplot` = "ammi2"),
               function(x) list(`Analysis of variance` = .sa_df(x$anova), `Interaction components` = .sa_df(x$pc_anova),
                                `Genotype scores` = .sa_df(x$gen_scores, "Genotype"),
                                `Environment scores` = .sa_df(x$env_scores, "Environment")),
               focus = "the number of significant interaction axes, the genotypes with small scores (stable) and the environments with the strongest interaction")
    , ammi_biplot = e("ammi_biplot", "AMMI biplots", "AMMI and GGE", "optional", c(`AMMI2 biplot` = "ammi2", `AMMI1 biplot` = "ammi1"),
                      function(x) list(Scores = .sa_df(x$scores)),
                      focus = "the genotypes near the origin with a high mean, and the environments and genotypes far from the origin")
    , stab_asv = e("stab_asv", "AMMI stability value", "AMMI and GGE", "no", c(ranks = "ranks", `mean against ASV` = "mean"),
                   function(x) list(`AMMI stability value` = .sa_df(x$ASD)),
                   focus = "the genotypes with the smallest ASV and the best yield stability index")
    , stab_masv = e("stab_masv", "Modified AMMI stability value", "AMMI and GGE", "no", c(ranks = "ranks", `mean against MASV` = "mean"),
                    function(x) list(`Modified AMMI stability value` = .sa_df(x$MASD)),
                    options = list(.m = list(type = "numeric", label = "Number of AMMI axes", value = 2, min = 2, max = 10, step = 1)),
                    focus = "the genotypes with the smallest MASV and the best modified yield stability index")
    , stab_dist = e("stab_dist", "AMMI stability distance", "AMMI and GGE", "no", c(ranks = "ranks", `mean against distance` = "mean"),
                    function(x) list(`AMMI stability distance` = .sa_df(x$stabDist)),
                    options = list(.m = list(type = "numeric", label = "Number of AMMI axes", value = 2, min = 1, max = 10, step = 1)),
                    focus = "the genotypes closest to the origin of the AMMI axes together with their yield ranks")
    , gge_biplot = e("gge_biplot", "GGE biplot", "AMMI and GGE", "optional", NULL,
                     function(x) {
                       g <- attr(x, "gge")
                       list(`Winner of each environment` = .sa_df(g$winners), `Mean and stability (ATC)` = .sa_df(g$atc),
                            `Environment vectors` = .sa_df(g$envs))
                     },
                     options = list(.which = list(type = "select", label = "View",
                                                  choices = c(`which won where` = "which-won-where", `mean against stability` = "mean-stability",
                                                              discrimination = "discrimination", basic = "basic"),
                                                  value = "which-won-where"),
                                    .svp = list(type = "select", label = "Scaling",
                                                choices = c(symmetric = "symmetric", `genotype-focused` = "genotype",
                                                            `environment-focused` = "environment"), value = "symmetric")),
                     focus = "which genotype wins where and the resulting groups of environments, mean against stability on the average environment axis, and the discriminating and representative environments")
    )
}
