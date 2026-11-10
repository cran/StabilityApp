

# StabilityApp 1.0.0

- A new application built on **stability** 1.0.0 (whose corrections it
  inherits): one page per analysis, twenty in all, each with options,
  interpretation, tables, plots and R code, including the new analyses
  of **stability** (Eskridge’s safety-first indices, Annicchiarico’s
  reliability index, Lin and Binns’ superiority measure, Huehn’s
  statistics).
- Results are easy to copy: an Output tab shows each result as R prints
  it (fixed-width font, aligned columns); the interpretation, the output
  and the R code have Copy buttons; tables have Copy, CSV and Excel
  buttons, an All-rows choice, p-values to three significant digits and
  other numbers rounded to three decimals.
- Text in IBM Plex Sans and output in IBM Plex Mono, with system fonts
  as fallback.
- The GGE biplot page offers every view in every scaling, and its
  interpretation follows the view and warns when the scaling does not
  suit it.
- The interpretation of every analysis is specific to its method and
  computed from its results by `stability::stab_interpret()`; it needs
  no service and no key.
- The optional written interpretation by an external text service is now
  specific to the method: the request (`sa_prompt()`) carries the
  reading guide and focus of the method, the computed findings (to be
  kept to), their facts and the first rows of the main table. In earlier
  versions every analysis sent the same general request, and the biplots
  sent no numbers at all, so the text could not be specific.
- `sa_ask()` and `sa_services()`: the services and their request formats
  in one place; errors of a service are shown with its message; keys can
  come from environment variables.
- Interface rebuilt with **bslib**; tables with **DT**; plots in the
  style of **stability**; downloads of plots and of the interpretation.
- Help pages, two vignettes (HTML and PDF), tests of every page of the
  application.

# StabilityApp 0.3.0

- Interpretation panels and a settings page for text services.

# StabilityApp 0.1.0

- First release.
