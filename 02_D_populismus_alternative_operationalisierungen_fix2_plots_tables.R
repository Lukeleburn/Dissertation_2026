###############################################################################
## DISS_2026 -- 02_D
## Alternative Operationalisierungen der populistischen Dimension
##
## Zweck:
##   Robustheitsanalyse zur Frage, ob sich eine vollstaendiger
##   operationalisierte populistische Dimension im CSES5 empirisch abbilden
##   laesst. Geprueft werden additive und nicht-kompensatorische Varianten.
##
## Output:
##   outputs/reli_vali_DE_v3_CFA_tables_wide/
##     02_D_populismus_alternative_operationalisierungen/
###############################################################################

rm(list = ls())

suppressPackageStartupMessages({
  library(tidyverse)
  library(here)
  library(psych)
  library(lavaan)
  library(gt)
})

###############################################################################
## 0) PFADE
###############################################################################

DIR_DERIVED <- here("data", "processed", "cses5", "derived")

## Primaere Eingabe: korrigierte Operationalisierung mit itemweiser
## skalenmittiger Transformation. Falls sie noch nicht existiert, wird auf den
## vorbereiteten Datensatz zurueckgegriffen; die notwendigen Itemtransformationen
## werden in diesem Skript ohnehin erneut erzeugt.
PATH_OPERATIONALISED <- file.path(
  DIR_DERIVED,
  "CSES5_20262301_operationalised_scaled_midpoint.rds"
)

PATH_PREPARED <- file.path(
  DIR_DERIVED,
  "CSES5_20262301_prepared.rds"
)

DIR_BASE_OUT <- here(
  "outputs", "reli_vali_DE_v3_CFA_tables_wide"
)

DIR_OUT <- file.path(
  DIR_BASE_OUT,
  "02_D_populismus_alternative_operationalisierungen"
)

DIR_TAB <- file.path(DIR_OUT, "tables")
DIR_PNG <- file.path(DIR_OUT, "png")
DIR_PDF <- file.path(DIR_OUT, "pdf")

for (d in c(DIR_OUT, DIR_TAB, DIR_PNG, DIR_PDF)) {
  dir.create(d, recursive = TRUE, showWarnings = FALSE)
}

if (file.exists(PATH_OPERATIONALISED)) {
  dat <- readRDS(PATH_OPERATIONALISED)
  cat("Datensatz geladen:", PATH_OPERATIONALISED, "\n")
} else if (file.exists(PATH_PREPARED)) {
  dat <- readRDS(PATH_PREPARED)
  cat("Operationalisierter Datensatz nicht gefunden. Verwende vorbereitetes RDS:\n")
  cat(PATH_PREPARED, "\n")
} else {
  stop("Weder operationalisierte noch vorbereitete RDS-Datei gefunden.")
}

stopifnot(
  "election_study_id" %in% names(dat),
  "respondent_id" %in% names(dat)
)

studies_keep <- c("AUT_2017", "DEU_2021", "GRC_2019")

study_labels <- c(
  AUT_2017 = "Österreich 2017",
  DEU_2021 = "Deutschland 2021",
  GRC_2019 = "Griechenland 2019"
)

###############################################################################
## 1) ITEM-DEFINITIONEN UND SKALIERUNG
###############################################################################

## Bisheriger populistischer Itempool im CSES-nahen Modell
popu_items_A <- c(
  "people_make_policy_rev",
  "pol_no_care_rev",
  "pol_for_rich_rev",
  "corruption_widespread_rev",
  "pol_main_problem_rev"
)

## Anti-Elitismus als Subdimension
anti_elite_items <- c(
  "pol_no_care_rev",
  "pol_for_rich_rev",
  "corruption_widespread_rev",
  "pol_main_problem_rev"
)

## In der Hauptoperationalisierung pragmatisch rekodiertes Kompromissitem.
## Fuer die alternative Populismusoperationalisierung wird es in die
## anti-kompromisshafte Richtung zurueckgedreht: Zustimmung zu
## "Kompromiss bedeutet Verrat/Ausverkauf" = hoehere populistische Auspraegung.
comp_prag_item <- "comp_sellout"
comp_popu_item <- "comp_anti_compromise_popu"

required_items <- c(popu_items_A, comp_prag_item)
missing_items <- setdiff(required_items, names(dat))
if (length(missing_items) > 0) {
  stop("Folgende benoetigte Items fehlen im Datensatz: ",
       paste(missing_items, collapse = ", "))
}

item_meta <- tribble(
  ~var,                         ~short,       ~long,
  "people_make_policy_rev",     "POPU1",      "Das Volk sollte Politik unmittelbar bestimmen",
  "pol_no_care_rev",            "POPU2",      "Politiker kümmern sich nicht um die Menschen",
  "pol_for_rich_rev",           "POPU3",      "Politiker dienen den Reichen und Mächtigen",
  "corruption_widespread_rev",  "POPU4",      "Politische Korruption ist weit verbreitet",
  "pol_main_problem_rev",       "POPU5",      "Politiker sind das Hauptproblem",
  "comp_sellout",               "PRG1",       "Kompromiss bedeutet Ausverkauf / Verrat, pragmatisch rekodiert",
  "comp_anti_compromise_popu",  "MANI1",      "Kompromiss bedeutet Ausverkauf / Verrat, populistisch rekodiert"
)

item_scale_meta <- tribble(
  ~var,                         ~min_val, ~max_val,
  "people_make_policy_rev",           1,       5,
  "pol_no_care_rev",                  1,       5,
  "pol_for_rich_rev",                 1,       5,
  "corruption_widespread_rev",        1,       4,
  "pol_main_problem_rev",             1,       5,
  "comp_sellout",                     1,       5,
  "comp_anti_compromise_popu",        1,       5
)

scale_midpoint_item <- function(x, min_val, max_val) {
  midpoint   <- (min_val + max_val) / 2
  half_range <- (max_val - min_val) / 2
  (x - midpoint) / half_range
}

scale_one_item <- function(dat_in, var) {
  info <- item_scale_meta %>% filter(.data$var == !!var)
  if (nrow(info) != 1) {
    stop("Keine eindeutige Skaleninformation fuer Item: ", var)
  }
  scale_midpoint_item(
    dat_in[[var]],
    min_val = info$min_val,
    max_val = info$max_val
  )
}

## Populistische Rueckrekodierung des Kompromissitems.
## Annahme: comp_sellout ist in der Hauptoperationalisierung pragmatisch kodiert.
## Deshalb bedeutet ein hoher Wert dort weniger Anti-Kompromiss-Haltung.
## Die alternative populistische Richtung wird mit 6 - x erzeugt.
dat_alt <- dat %>%
  filter(.data$election_study_id %in% studies_keep) %>%
  mutate(
    comp_anti_compromise_popu = if_else(
      is.na(.data[[comp_prag_item]]),
      NA_real_,
      6 - as.numeric(.data[[comp_prag_item]])
    )
  )

## Itemweise skalenmittige Variablen erzeugen
items_to_scale <- c(popu_items_A, comp_prag_item, comp_popu_item)

for (v in items_to_scale) {
  dat_alt[[paste0(v, "_smp")]] <- scale_one_item(dat_alt, v)
}

item_scale_check <- dat_alt %>%
  select(election_study_id, all_of(items_to_scale)) %>%
  pivot_longer(
    cols = -election_study_id,
    names_to = "item",
    values_to = "value"
  ) %>%
  group_by(election_study_id, item) %>%
  summarise(
    min_observed = suppressWarnings(min(value, na.rm = TRUE)),
    max_observed = suppressWarnings(max(value, na.rm = TRUE)),
    unique_values = paste(sort(unique(na.omit(value))), collapse = ", "),
    n_values = n_distinct(value, na.rm = TRUE),
    n_valid = sum(!is.na(value)),
    .groups = "drop"
  ) %>%
  mutate(
    min_observed = if_else(is.infinite(min_observed), NA_real_, min_observed),
    max_observed = if_else(is.infinite(max_observed), NA_real_, max_observed)
  )

write_csv(
  item_scale_check,
  file.path(DIR_TAB, "01_item_scale_check_alternative_populism.csv")
)

###############################################################################
## 2) ALTERNATIVE OPERATIONALISIERUNGEN
###############################################################################

popu_items_A_smp <- paste0(popu_items_A, "_smp")
popu_items_B_smp <- paste0(c(popu_items_A, comp_popu_item), "_smp")
anti_elite_smp   <- paste0(anti_elite_items, "_smp")

## Variante A: CSES-nahe additive Populismusdimension
## Variante B: additive Populismusdimension inklusive Anti-Kompromiss-Item
## Variante C: Syndromlogik nach Komponenten
##   People-Centrism / Volkssouveraenitaet: POPU1
##   Anti-Elitismus: Mittelwert POPU2-POPU5
##   Manichaeisch/Anti-Kompromiss: PRG1 in populistischer Richtung
##   Bollen/additiv: Mittelwert der drei Komponenten
##   Goertz/nicht-kompensatorisch: Minimum der drei Komponenten

dat_alt <- dat_alt %>%
  rowwise() %>%
  mutate(
    POPU_A_additive = mean(c_across(all_of(popu_items_A_smp)), na.rm = FALSE),
    POPU_B_additive_full = mean(c_across(all_of(popu_items_B_smp)), na.rm = FALSE),
    POPU_C_people = .data$people_make_policy_rev_smp,
    POPU_C_anti_elite = mean(c_across(all_of(anti_elite_smp)), na.rm = FALSE),
    POPU_C_manichean = .data$comp_anti_compromise_popu_smp,
    POPU_C_bollen_components = mean(
      c(.data$POPU_C_people,
        .data$POPU_C_anti_elite,
        .data$POPU_C_manichean),
      na.rm = FALSE
    ),
    POPU_C_goertz_minimum = min(
      c(.data$POPU_C_people,
        .data$POPU_C_anti_elite,
        .data$POPU_C_manichean),
      na.rm = FALSE
    )
  ) %>%
  ungroup()

model_overview <- tribble(
  ~variant, ~score_var, ~label, ~aggregation, ~items_or_components,
  "A", "POPU_A_additive", "CSES-nahe Populismusdimension", "Mittelwert der skalenmittigen Items", "POPU1 + POPU2 + POPU3 + POPU4 + POPU5",
  "B", "POPU_B_additive_full", "Vollere additive Populismusdimension mit Anti-Kompromiss", "Mittelwert der skalenmittigen Items", "POPU1 + POPU2 + POPU3 + POPU4 + POPU5 + PRG1 in populistischer Richtung",
  "C1", "POPU_C_bollen_components", "Populismus als Syndrom, additive Komponentenlogik", "Mittelwert der Komponenten", "People-Centrism + Anti-Elitismus + Anti-Kompromiss",
  "C2", "POPU_C_goertz_minimum", "Populismus als Syndrom, nicht-kompensatorische Logik", "Minimum der Komponenten", "People-Centrism + Anti-Elitismus + Anti-Kompromiss"
)

write_csv(
  model_overview,
  file.path(DIR_TAB, "00_model_overview_alternative_populism.csv")
)

###############################################################################
## 3) RELIABILITAET
###############################################################################

safe_alpha_omega <- function(dat_in, vars) {
  dat_cc <- dat_in %>%
    select(all_of(vars)) %>%
    drop_na()

  if (nrow(dat_cc) < 30 || length(vars) < 2) {
    return(tibble(n = nrow(dat_cc), alpha = NA_real_, omega = NA_real_))
  }

  alpha_val <- tryCatch(
    psych::alpha(dat_cc, check.keys = FALSE, warnings = FALSE)$total$raw_alpha,
    error = function(e) NA_real_
  )

  omega_val <- tryCatch(
    psych::omega(dat_cc, nfactors = 1, plot = FALSE, warnings = FALSE)$omega.tot,
    error = function(e) NA_real_
  )

  tibble(
    n = nrow(dat_cc),
    alpha = round(alpha_val, 3),
    omega = round(omega_val, 3)
  )
}

reliability_specs <- list(
  POPU_A_additive = list(
    label = "Variante A: POPU1–POPU5, additiv",
    vars = popu_items_A_smp
  ),
  POPU_B_additive_full = list(
    label = "Variante B: POPU1–POPU5 plus Anti-Kompromiss, additiv",
    vars = popu_items_B_smp
  ),
  POPU_C_anti_elite = list(
    label = "Subdimension Anti-Elitismus: POPU2–POPU5",
    vars = anti_elite_smp
  )
)

reliability_tbl <- map_dfr(studies_keep, function(study_i) {
  dat_i <- dat_alt %>% filter(.data$election_study_id == study_i)

  map_dfr(names(reliability_specs), function(spec_name) {
    spec <- reliability_specs[[spec_name]]
    safe_alpha_omega(dat_i, spec$vars) %>%
      mutate(
        election_study_id = study_i,
        study_label = study_labels[[study_i]],
        scale = spec_name,
        label = spec$label,
        n_items = length(spec$vars),
        items = paste(str_remove(spec$vars, "_smp$"), collapse = ", "),
        .before = 1
      )
  })
})

write_csv(
  reliability_tbl,
  file.path(DIR_TAB, "02_reliability_alternative_populism_alpha_omega.csv")
)

###############################################################################
## 4) CFA-VALIDIERUNG FUER ADDITIVE VARIANTEN
###############################################################################

fit_cfa_one_factor <- function(dat_in, vars, factor_name = "POPU") {
  dat_cc <- dat_in %>%
    select(all_of(vars)) %>%
    drop_na()

  if (nrow(dat_cc) < 50 || length(vars) < 3) {
    return(tibble(
      n = nrow(dat_cc), converged = FALSE,
      chisq = NA_real_, df = NA_real_, pvalue = NA_real_,
      cfi = NA_real_, tli = NA_real_, rmsea = NA_real_, srmr = NA_real_,
      aic = NA_real_, bic = NA_real_
    ))
  }

  names(dat_cc) <- paste0("i", seq_along(vars))
  model_txt <- paste0(
    factor_name, " =~ ",
    paste(names(dat_cc), collapse = " + ")
  )

  fit <- tryCatch(
    lavaan::cfa(
      model = model_txt,
      data = dat_cc,
      estimator = "MLR",
      missing = "listwise",
      std.lv = TRUE
    ),
    error = function(e) NULL
  )

  if (is.null(fit)) {
    return(tibble(
      n = nrow(dat_cc), converged = FALSE,
      chisq = NA_real_, df = NA_real_, pvalue = NA_real_,
      cfi = NA_real_, tli = NA_real_, rmsea = NA_real_, srmr = NA_real_,
      aic = NA_real_, bic = NA_real_
    ))
  }

  fm <- lavaan::fitMeasures(
    fit,
    c("chisq.scaled", "df.scaled", "pvalue.scaled",
      "cfi.scaled", "tli.scaled", "rmsea.scaled", "srmr", "aic", "bic")
  )

  tibble(
    n = nrow(dat_cc),
    converged = lavaan::lavInspect(fit, "converged"),
    chisq = round(unname(fm["chisq.scaled"]), 3),
    df = round(unname(fm["df.scaled"]), 0),
    pvalue = round(unname(fm["pvalue.scaled"]), 3),
    cfi = round(unname(fm["cfi.scaled"]), 3),
    tli = round(unname(fm["tli.scaled"]), 3),
    rmsea = round(unname(fm["rmsea.scaled"]), 3),
    srmr = round(unname(fm["srmr"]), 3),
    aic = round(unname(fm["aic"]), 1),
    bic = round(unname(fm["bic"]), 1)
  )
}

cfa_specs <- list(
  POPU_A_one_factor = list(
    label = "Ein-Faktor-Modell: POPU1–POPU5",
    vars = popu_items_A_smp
  ),
  POPU_B_one_factor_full = list(
    label = "Ein-Faktor-Modell: POPU1–POPU5 plus Anti-Kompromiss",
    vars = popu_items_B_smp
  ),
  Anti_Elite_one_factor = list(
    label = "Ein-Faktor-Modell: Anti-Elitismus POPU2–POPU5",
    vars = anti_elite_smp
  )
)

cfa_fit_tbl <- map_dfr(studies_keep, function(study_i) {
  dat_i <- dat_alt %>% filter(.data$election_study_id == study_i)

  map_dfr(names(cfa_specs), function(spec_name) {
    spec <- cfa_specs[[spec_name]]
    fit_cfa_one_factor(dat_i, spec$vars, factor_name = "POPU") %>%
      mutate(
        election_study_id = study_i,
        study_label = study_labels[[study_i]],
        model = spec_name,
        label = spec$label,
        n_items = length(spec$vars),
        items = paste(str_remove(spec$vars, "_smp$"), collapse = ", "),
        .before = 1
      )
  })
})

write_csv(
  cfa_fit_tbl,
  file.path(DIR_TAB, "03_cfa_fit_alternative_populism.csv")
)

###############################################################################
## 5) DESKRIPTION UND KORRELATIONEN DER SCORES
###############################################################################

score_vars <- c(
  "POPU_A_additive",
  "POPU_B_additive_full",
  "POPU_C_people",
  "POPU_C_anti_elite",
  "POPU_C_manichean",
  "POPU_C_bollen_components",
  "POPU_C_goertz_minimum"
)

score_labels <- c(
  POPU_A_additive = "A: additiv POPU1–POPU5",
  POPU_B_additive_full = "B: additiv plus Anti-Kompromiss",
  POPU_C_people = "C: People-Centrism",
  POPU_C_anti_elite = "C: Anti-Elitismus",
  POPU_C_manichean = "C: Anti-Kompromiss",
  POPU_C_bollen_components = "C: Bollen / additiv Komponenten",
  POPU_C_goertz_minimum = "C: Goertz / Minimum"
)

scores_long <- dat_alt %>%
  select(respondent_id, election_study_id, all_of(score_vars)) %>%
  pivot_longer(
    cols = all_of(score_vars),
    names_to = "score",
    values_to = "value"
  ) %>%
  mutate(
    study_label = study_labels[election_study_id],
    score_label = score_labels[score]
  )

write_csv(
  scores_long,
  file.path(DIR_TAB, "06_scores_long_alternative_populism.csv")
)

score_descriptives <- scores_long %>%
  group_by(election_study_id, study_label, score, score_label) %>%
  summarise(
    n = sum(!is.na(value)),
    mean = round(mean(value, na.rm = TRUE), 3),
    sd = round(sd(value, na.rm = TRUE), 3),
    median = round(median(value, na.rm = TRUE), 3),
    min = round(min(value, na.rm = TRUE), 3),
    max = round(max(value, na.rm = TRUE), 3),
    share_gt_0 = round(mean(value > 0, na.rm = TRUE), 3),
    share_gt_0_5 = round(mean(value > 0.5, na.rm = TRUE), 3),
    .groups = "drop"
  )

write_csv(
  score_descriptives,
  file.path(DIR_TAB, "04_score_descriptives_alternative_populism.csv")
)

score_correlations <- map_dfr(studies_keep, function(study_i) {
  dat_i <- dat_alt %>%
    filter(.data$election_study_id == study_i) %>%
    select(all_of(score_vars))

  cor_mat <- suppressWarnings(cor(dat_i, use = "pairwise.complete.obs"))

  cor_long <- as.data.frame(as.table(cor_mat), stringsAsFactors = FALSE)
  names(cor_long) <- c("score_1", "score_2", "correlation")

  as_tibble(cor_long) %>%
    mutate(
      score_1 = as.character(score_1),
      score_2 = as.character(score_2),
      election_study_id = study_i,
      study_label = study_labels[[study_i]],
      score_1_label = unname(score_labels[score_1]),
      score_2_label = unname(score_labels[score_2]),
      correlation = round(as.numeric(correlation), 3),
      .before = 1
    )
})

write_csv(
  score_correlations,
  file.path(DIR_TAB, "05_score_correlations_alternative_populism.csv")
)

###############################################################################
## 6) TABELLEN ALS PNG
###############################################################################

save_gt_png <- function(gt_obj, filename, vwidth = 1500, vheight = 1000, expand = 10) {
  out <- file.path(DIR_PNG, filename)
  tryCatch(
    {
      gt::gtsave(
        gt_obj,
        out,
        vwidth  = vwidth,
        vheight = vheight,
        expand  = expand
      )
      cat("PNG gespeichert:", out, "\n")
    },
    error = function(e) {
      cat("PNG konnte nicht gespeichert werden:", out, "\n")
      cat("Grund:", conditionMessage(e), "\n")
    }
  )
}

reliability_gt <- reliability_tbl %>%
  select(study_label, label, n, n_items, alpha, omega) %>%
  arrange(study_label, label) %>%
  gt(groupname_col = "study_label") %>%
  tab_header(
    title = md("**Alternative Populismusoperationalisierungen: Reliabilität**"),
    subtitle = "Nur Cronbachs α und McDonalds ω"
  ) %>%
  cols_label(
    label = "Skala",
    n = "N",
    n_items = "Items",
    alpha = "Cronbachs α",
    omega = "McDonalds ω"
  ) %>%
  cols_width(
    label ~ px(650),
    n ~ px(95),
    n_items ~ px(90),
    alpha ~ px(150),
    omega ~ px(160)
  ) %>%
  tab_options(table.width = px(1250))

save_gt_png(
  reliability_gt,
  "table_02_D_reliability_alternative_populism.png",
  vwidth = 1450,
  vheight = 980
)

cfa_gt <- cfa_fit_tbl %>%
  select(study_label, label, n, n_items, cfi, tli, rmsea, srmr, aic, bic) %>%
  arrange(study_label, label) %>%
  gt(groupname_col = "study_label") %>%
  tab_header(
    title = md("**Alternative Populismusoperationalisierungen: CFA-Fit**"),
    subtitle = "Ein-Faktor-Modelle für additive Varianten und Anti-Elitismus"
  ) %>%
  cols_label(
    label = "Modell",
    n = "N",
    n_items = "Items",
    cfi = "CFI",
    tli = "TLI",
    rmsea = "RMSEA",
    srmr = "SRMR",
    aic = "AIC",
    bic = "BIC"
  ) %>%
  cols_width(
    label ~ px(600),
    n ~ px(90),
    n_items ~ px(80),
    cfi ~ px(85),
    tli ~ px(85),
    rmsea ~ px(95),
    srmr ~ px(95),
    aic ~ px(120),
    bic ~ px(120)
  ) %>%
  tab_options(table.width = px(1450))

save_gt_png(
  cfa_gt,
  "table_02_D_cfa_fit_alternative_populism.png",
  vwidth = 1700,
  vheight = 1050
)

###############################################################################
## 7) PLOTS
###############################################################################

plot_score_palette <- c(
  "A: additiv POPU1–POPU5" = "#1f77b4",
  "B: additiv plus Anti-Kompromiss" = "#d62728",
  "C: Bollen / additiv Komponenten" = "#2ca02c",
  "C: Goertz / Minimum" = "#9467bd"
)

plot_score_linetypes <- c(
  "A: additiv POPU1–POPU5" = "solid",
  "B: additiv plus Anti-Kompromiss" = "dotted",
  "C: Bollen / additiv Komponenten" = "longdash",
  "C: Goertz / Minimum" = "twodash"
)

plot_score_density <- function(study_i) {
  scores_long %>%
    filter(.data$election_study_id == study_i,
           .data$score %in% c("POPU_A_additive",
                              "POPU_B_additive_full",
                              "POPU_C_bollen_components",
                              "POPU_C_goertz_minimum")) %>%
    ggplot(aes(x = value, color = score_label, linetype = score_label)) +
    geom_density(linewidth = 0.95, na.rm = TRUE) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "grey55") +
    scale_color_manual(values = plot_score_palette) +
    scale_linetype_manual(values = plot_score_linetypes) +
    scale_x_continuous(limits = c(-1, 1), breaks = seq(-1, 1, 0.5)) +
    labs(
      title = paste0(study_labels[[study_i]], " — alternative Populismusscores"),
      subtitle = "Skalenmittige Werte; 0 = theoretische Skalenmitte",
      x = "Score",
      y = "Dichte",
      color = "Operationalisierung",
      linetype = "Operationalisierung"
    ) +
    theme_minimal(base_size = 12) +
    theme(
      plot.title = element_text(face = "bold"),
      legend.position = "bottom",
      legend.title = element_text(face = "bold"),
      legend.text = element_text(size = 9),
      panel.grid.minor = element_blank()
    ) +
    guides(
      color = guide_legend(nrow = 2, byrow = TRUE),
      linetype = guide_legend(nrow = 2, byrow = TRUE)
    )
}

pdf_density <- file.path(DIR_PDF, "density_alternative_populism_scores_ALL.pdf")
pdf(pdf_density, width = 10, height = 6)
for (study_i in studies_keep) {
  p <- plot_score_density(study_i)
  print(p)
  ggsave(
    filename = file.path(DIR_PNG, paste0("density_alternative_populism_scores_", study_i, ".png")),
    plot = p,
    width = 11.5,
    height = 6.5,
    dpi = 300
  )
}
dev.off()
cat("PDF gespeichert:", pdf_density, "\n")

###############################################################################
## 8) ABSCHLUSS
###############################################################################

cat("\nFertig. Outputs gespeichert in:\n")
cat(DIR_OUT, "\n")
cat("\nZentrale Tabellen:\n")
cat("  00_model_overview_alternative_populism.csv\n")
cat("  01_item_scale_check_alternative_populism.csv\n")
cat("  02_reliability_alternative_populism_alpha_omega.csv\n")
cat("  03_cfa_fit_alternative_populism.csv\n")
cat("  04_score_descriptives_alternative_populism.csv\n")
cat("  05_score_correlations_alternative_populism.csv\n")
cat("  06_scores_long_alternative_populism.csv\n")
