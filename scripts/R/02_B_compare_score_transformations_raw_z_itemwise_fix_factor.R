###############################################################################
## DISS_2026 -- 02_B
## Vergleich verschiedener Score-Transformationen
##
## Zweck:
##   Dieses Skript prueft, wie sich unterschiedliche Transformationen der
##   validierten Demokratieperspektiven zueinander verhalten.
##
##   Wichtig: Die skalenmittige Hauptoperationalisierung wird NICHT in diesem
##   Skript neu berechnet, sondern aus der zentral erzeugten RDS-Datei gelesen.
##   Sie beruht damit auf der itemweisen Transformation unterschiedlicher
##   Itemskalen:
##      5-stufige Items: (x - 3) / 2
##      4-stufige Items: (x - 2.5) / 1.5
##
##   Verglichen werden je Land und Konstrukt:
##     A) EFA-basierter Faktorwert, nicht z-standardisiert (falls vorhanden)
##     B) Rohindex auf der urspruenglichen Antwortmetrik
##     C) z-standardisierter Rohindex, laender- und konstruktintern
##     D) skalenmittige Hauptoperationalisierung [-1, +1], itemweise gebildet
##
##   Zentrale Diagnose:
##     Wie viele Personen haben raw_index_z < 0, aber scaled_midpoint > 0?
##     Diese Faelle liegen relativ unter dem jeweiligen Landes-/Konstruktmittel,
##     aber weiterhin oberhalb der theoretischen Skalenmitte.
##
## Input:
##   data/processed/cses5/derived/CSES5_20262301_operationalised_scaled_midpoint.rds
##
## Output:
##   outputs/reli_vali_DE_v3_CFA_tables_wide/02_C_score_transformations/
###############################################################################

suppressPackageStartupMessages({
  library(tidyverse)
  library(gt)
  library(here)
})

###############################################################################
## 0) PFADE
###############################################################################

PATH_OP <- here(
  "data", "processed", "cses5", "derived",
  "CSES5_20262301_operationalised_scaled_midpoint.rds"
)

## Optional: alte factor-score-basierte Operationalisierung.
## Sie wird nur fuer den Vergleich als eigene Scorevariante genutzt.
## Die Hauptoperationalisierung bleibt die itemweise skalenmittige RDS-Datei oben.
PATH_FACTOR_OLD <- here(
  "data", "processed", "cses5", "derived",
  "CSES5_20262301_operationalised.rds"
)

DIR_BASE <- here(
  "outputs",
  "reli_vali_DE_v3_CFA_tables_wide",
  "02_C_score_transformations"
)

DIR_TABLES <- file.path(DIR_BASE, "tables")
DIR_PLOTS  <- file.path(DIR_BASE, "plots")

for (d in c(DIR_BASE, DIR_TABLES, DIR_PLOTS)) {
  dir.create(d, recursive = TRUE, showWarnings = FALSE)
}

if (!file.exists(PATH_OP)) {
  stop(
    "Operationalisierte RDS-Datei nicht gefunden: ", PATH_OP, "\n",
    "Bitte zuerst das zentrale itemweise Operationalisierungsskript ausfuehren."
  )
}

###############################################################################
## 1) DATEN UND MESSMODELLE
###############################################################################

CSES5_op <- readRDS(PATH_OP)

expected_studies <- c("AUT_2017", "DEU_2021", "GRC_2019")

factor_score_names <- c(
  "EXCL_score",
  "POPU_score",
  "PRG_score",
  "Anti_Elitist_score",
  "Minimal_PRG_score"
)

if (file.exists(PATH_FACTOR_OLD)) {
  message("Lese optionale alte Faktorwert-RDS fuer Vergleich: ", PATH_FACTOR_OLD)

  factor_old <- readRDS(PATH_FACTOR_OLD) %>%
    select(
      respondent_id,
      election_study_id,
      any_of(factor_score_names)
    ) %>%
    rename_with(
      ~ paste0(.x, "_factor_old"),
      .cols = any_of(factor_score_names)
    )

  CSES5_op <- CSES5_op %>%
    left_join(
      factor_old,
      by = c("respondent_id", "election_study_id")
    )
} else {
  message(
    "Keine alte Faktorwert-RDS gefunden. ",
    "Die Scorevariante 'EFA-Faktorwert' wird dann ausgelassen."
  )
}

study_labels_de <- c(
  AUT_2017 = "Österreich 2017",
  DEU_2021 = "Deutschland 2021",
  GRC_2019 = "Griechenland 2019"
)

all_items_excl <- c(
  "majority_over_minority_rev",
  "minorities_adapt_rev",
  "follow_customs_rev",
  "immig_harm_culture_rev",
  "born_country_rev"
)

all_items_popu <- c(
  "people_make_policy_rev",
  "pol_no_care_rev",
  "pol_for_rich_rev",
  "corruption_widespread_rev",
  "pol_main_problem_rev"
)

all_items_prg <- c(
  "comp_sellout",
  "voting_makes_difference",
  "power_makes_difference",
  "pol_trustworthy_rev"
)

study_models <- list(
  AUT_2017 = list(
    EXCL = list(
      items      = all_items_excl,
      label      = "Exklusionistische Demokratie",
      score_name = "EXCL_score"
    ),
    POPU = list(
      items      = all_items_popu[2:5],
      label      = "Anti-elitistische Demokratie",
      score_name = "Anti_Elitist_score"
    ),
    PRG = list(
      items      = c("voting_makes_difference",
                     "power_makes_difference"),
      label      = "Minimal-pragmatische Demokratie",
      score_name = "Minimal_PRG_score"
    )
  ),
  DEU_2021 = list(
    EXCL = list(
      items      = all_items_excl,
      label      = "Exklusionistische Demokratie",
      score_name = "EXCL_score"
    ),
    POPU = list(
      items      = all_items_popu,
      label      = "Populistische Demokratie",
      score_name = "POPU_score"
    ),
    PRG = list(
      items      = all_items_prg,
      label      = "Pragmatische Demokratie",
      score_name = "PRG_score"
    )
  ),
  GRC_2019 = list(
    EXCL = list(
      items      = all_items_excl,
      label      = "Exklusionistische Demokratie",
      score_name = "EXCL_score"
    ),
    POPU = list(
      items      = all_items_popu,
      label      = "Populistische Demokratie",
      score_name = "POPU_score"
    ),
    PRG = list(
      items      = all_items_prg,
      label      = "Pragmatische Demokratie",
      score_name = "PRG_score"
    )
  )
)

common_perspective_labels <- c(
  EXCL = "Exklusionistische Perspektive",
  POPU = "Populistische Perspektive",
  PRG  = "Pragmatische Perspektive"
)

get_optional_col <- function(dat, var) {
  if (var %in% names(dat)) {
    dat[[var]]
  } else {
    rep(NA_real_, nrow(dat))
  }
}

safe_cor <- function(x, y) {
  ok <- stats::complete.cases(x, y)
  if (sum(ok) < 2L) {
    return(NA_real_)
  }
  suppressWarnings(stats::cor(x[ok], y[ok]))
}

safe_z <- function(x) {
  if (sum(!is.na(x)) < 2L || isTRUE(all.equal(stats::sd(x, na.rm = TRUE), 0))) {
    return(rep(NA_real_, length(x)))
  }
  as.numeric(scale(x))
}

###############################################################################
## 2) SCORE-VARIANTEN BILDEN
###############################################################################

score_long_list <- list()

for (study_id in expected_studies) {
  sm <- study_models[[study_id]]

  dat_study <- CSES5_op %>%
    filter(election_study_id == .env$study_id)

  for (construct_key in c("EXCL", "POPU", "PRG")) {
    cfg <- sm[[construct_key]]

    scaled_var <- paste0(cfg$score_name, "_scaled_midpoint")

    required_vars <- c(cfg$items, scaled_var)

    missing_vars <- setdiff(required_vars, names(dat_study))
    if (length(missing_vars) > 0) {
      stop(
        "Fehlende Variablen fuer ", study_id, " / ", construct_key, ": ",
        paste(missing_vars, collapse = ", ")
      )
    }

    ## Faktorwert aus der alten EFA-basierten Operationalisierung, falls vorhanden.
    ## Nicht verwechseln mit den neuen itemweisen Scores.
    factor_vec <- get_optional_col(
      dat_study,
      paste0(cfg$score_name, "_factor_old")
    )

    score_long_list[[paste(study_id, construct_key, sep = "_")]] <-
      dat_study %>%
      transmute(
        study = study_id,
        study_label = study_labels_de[study_id],
        study_label_marked = if_else(study_id == "AUT_2017",
                                     paste0(study_labels_de[study_id], "*"),
                                     study_labels_de[study_id]),
        respondent_id,
        construct_key = construct_key,
        common_perspective = common_perspective_labels[construct_key],
        construct_label = cfg$label,
        factor_score = factor_vec,
        raw_index = rowMeans(across(all_of(cfg$items)), na.rm = FALSE),
        centered_raw = raw_index - 3,
        scaled_midpoint = .data[[scaled_var]]
      )
  }
}

score_long <- bind_rows(score_long_list) %>%
  group_by(study, construct_key) %>%
  mutate(
    raw_index_z = safe_z(raw_index),
    factor_score_z_for_diagnostics = safe_z(factor_score)
  ) %>%
  ungroup() %>%
  mutate(
    study_label = factor(
      study_label,
      levels = study_labels_de[expected_studies]
    ),
    study_label_marked = factor(
      study_label_marked,
      levels = c("Österreich 2017*",
                 "Deutschland 2021",
                 "Griechenland 2019")
    ),
    common_perspective = factor(
      common_perspective,
      levels = c("Exklusionistische Perspektive",
                 "Populistische Perspektive",
                 "Pragmatische Perspektive")
    )
  )

write_csv(
  score_long,
  file.path(DIR_TABLES, "score_transformations_long.csv")
)

###############################################################################
## 3) DIAGNOSE-TABELLEN
###############################################################################

diagnostics <- score_long %>%
  group_by(study, study_label, construct_key, common_perspective, construct_label) %>%
  summarise(
    n = sum(complete.cases(raw_index, raw_index_z, scaled_midpoint)),
    mean_raw = mean(raw_index, na.rm = TRUE),
    sd_raw = sd(raw_index, na.rm = TRUE),
    median_raw = median(raw_index, na.rm = TRUE),
    mean_scaled_midpoint = mean(scaled_midpoint, na.rm = TRUE),
    sd_scaled_midpoint = sd(scaled_midpoint, na.rm = TRUE),
    median_scaled_midpoint = median(scaled_midpoint, na.rm = TRUE),
    mean_raw_z = mean(raw_index_z, na.rm = TRUE),
    sd_raw_z = sd(raw_index_z, na.rm = TRUE),
    cor_raw_scaled_midpoint = safe_cor(raw_index, scaled_midpoint),
    cor_raw_z_scaled_midpoint = safe_cor(raw_index_z, scaled_midpoint),
    cor_factor_raw = safe_cor(factor_score, raw_index),
    cor_factor_scaled_midpoint = safe_cor(factor_score, scaled_midpoint),
    share_raw_above_midpoint = mean(raw_index > 3, na.rm = TRUE),
    share_scaled_midpoint_above_zero = mean(scaled_midpoint > 0, na.rm = TRUE),
    share_raw_z_below_zero = mean(raw_index_z < 0, na.rm = TRUE),
    share_raw_z_below_zero_but_scaled_above_zero =
      mean(raw_index_z < 0 & scaled_midpoint > 0, na.rm = TRUE),
    share_raw_z_above_zero_but_scaled_below_zero =
      mean(raw_index_z > 0 & scaled_midpoint < 0, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(across(where(is.numeric), ~ round(.x, 3)))

write_csv(
  diagnostics,
  file.path(DIR_TABLES, "score_transformation_diagnostics.csv")
)

diagnostics_pretty <- diagnostics %>%
  mutate(
    mean_raw = sprintf("%.2f", mean_raw),
    sd_raw = sprintf("%.2f", sd_raw),
    median_raw = sprintf("%.2f", median_raw),
    mean_scaled_midpoint = sprintf("%.2f", mean_scaled_midpoint),
    sd_scaled_midpoint = sprintf("%.2f", sd_scaled_midpoint),
    cor_raw_z_scaled_midpoint = sprintf("%.2f", cor_raw_z_scaled_midpoint),
    share_raw_above_midpoint = sprintf("%.1f%%", share_raw_above_midpoint * 100),
    share_scaled_midpoint_above_zero = sprintf("%.1f%%", share_scaled_midpoint_above_zero * 100),
    share_raw_z_below_zero_but_scaled_above_zero =
      sprintf("%.1f%%", share_raw_z_below_zero_but_scaled_above_zero * 100)
  ) %>%
  select(
    study_label,
    construct_label,
    n,
    mean_raw,
    sd_raw,
    median_raw,
    mean_scaled_midpoint,
    sd_scaled_midpoint,
    cor_raw_z_scaled_midpoint,
    share_raw_above_midpoint,
    share_scaled_midpoint_above_zero,
    share_raw_z_below_zero_but_scaled_above_zero
  )

tbl_diag <- diagnostics_pretty %>%
  gt(groupname_col = "study_label") %>%
  tab_header(
    title = "Vergleich der Score-Transformationen",
    subtitle = "Rohindex, z-standardisierter Rohindex und skalenmittige Hauptoperationalisierung"
  ) %>%
  cols_label(
    construct_label = "Konstrukt",
    n = "N",
    mean_raw = "M Roh",
    sd_raw = "SD Roh",
    median_raw = "Median Roh",
    mean_scaled_midpoint = "M skalenmittig",
    sd_scaled_midpoint = "SD skalenmittig",
    cor_raw_z_scaled_midpoint = "r(Roh-z, skalenmittig)",
    share_raw_above_midpoint = "Roh > 3",
    share_scaled_midpoint_above_zero = "Skalenmittig > 0",
    share_raw_z_below_zero_but_scaled_above_zero = "Roh-z < 0 & skalenmittig > 0"
  ) %>%
  tab_source_note(
    "Hinweis: Die letzte Spalte zeigt Faelle, die relativ unter dem Landes-/Konstruktmittel liegen, aber inhaltlich oberhalb der theoretischen Skalenmitte bleiben."
  ) %>%
  tab_options(table.width = px(1500))

gt::gtsave(
  tbl_diag,
  filename = file.path(DIR_PLOTS, "score_transformation_diagnostics.png"),
  expand = 15,
  vwidth = 1700,
  vheight = 900
)

###############################################################################
## 4) PLOTS: KOMBINIERTE DIAGNOSTIK
###############################################################################

note_austria <- paste(
  "* Österreich: POPU wird als anti-elitistische Dimension,",
  "PRG als minimal-pragmatische Dimension operationalisiert."
)

p_scatter <- score_long %>%
  ggplot(aes(x = scaled_midpoint, y = raw_index_z, color = common_perspective)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey45") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey45") +
  geom_point(alpha = 0.18, size = 0.75, na.rm = TRUE) +
  facet_grid(common_perspective ~ study_label_marked) +
  guides(color = "none") +
  labs(
    title = "Skalenmittige Hauptoperationalisierung vs. z-standardisierter Rohindex",
    subtitle = "x = inhaltliche Lage zur theoretischen Skalenmitte; y = relative Lage zum Landes-/Konstruktmittel",
    caption = note_austria,
    x = "Skalenmittiger Konstruktwert [-1, +1]",
    y = "Rohindex z-standardisiert"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 10, color = "grey35"),
    plot.caption = element_text(size = 9, color = "grey35", hjust = 0),
    strip.text = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  )

ggsave(
  filename = file.path(DIR_PLOTS, "scatter_scaled_midpoint_vs_raw_z_common.png"),
  plot = p_scatter,
  width = 13,
  height = 7.5,
  dpi = 320
)

score_variants_long <- score_long %>%
  select(
    study,
    study_label_marked,
    respondent_id,
    construct_key,
    common_perspective,
    construct_label,
    factor_score,
    raw_index,
    raw_index_z,
    scaled_midpoint
  ) %>%
  pivot_longer(
    cols = c(factor_score, raw_index, raw_index_z, scaled_midpoint),
    names_to = "score_type",
    values_to = "value"
  ) %>%
  filter(!(score_type == "factor_score" & is.na(value))) %>%
  mutate(
    score_type = recode(
      score_type,
      factor_score = "EFA-Faktorwert",
      raw_index = "Rohindex",
      raw_index_z = "Rohindex z-standardisiert",
      scaled_midpoint = "Skalenmittige Hauptoperationalisierung [-1, +1]"
    ),
    score_type = factor(
      score_type,
      levels = c("Rohindex",
                 "Rohindex z-standardisiert",
                 "Skalenmittige Hauptoperationalisierung [-1, +1]",
                 "EFA-Faktorwert")
    )
  )

write_csv(
  score_variants_long,
  file.path(DIR_TABLES, "score_transformations_variants_long.csv")
)

p_density_variants <- ggplot(
  score_variants_long,
  aes(x = value, color = score_type, fill = score_type)
) +
  geom_density(alpha = 0.08, linewidth = 0.75, na.rm = TRUE) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey45") +
  facet_grid(common_perspective ~ study_label_marked, scales = "free_x") +
  labs(
    title = "Verteilungen unterschiedlicher Score-Transformationen",
    subtitle = "Vergleich von Rohindex, Rohindex-z, skalenmittiger Hauptoperationalisierung und EFA-Faktorwert",
    caption = note_austria,
    x = "Scorewert",
    y = "Dichte",
    color = NULL,
    fill = NULL
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 10, color = "grey35"),
    plot.caption = element_text(size = 9, color = "grey35", hjust = 0),
    strip.text = element_text(face = "bold"),
    legend.position = "bottom",
    panel.grid.minor = element_blank()
  )

ggsave(
  filename = file.path(DIR_PLOTS, "density_score_transformations_common.png"),
  plot = p_density_variants,
  width = 14,
  height = 8.2,
  dpi = 320
)

###############################################################################
## 5) PLOTS JE LAND
###############################################################################

for (study_id in expected_studies) {
  study_caption <- if (study_id == "AUT_2017") note_austria else NULL
  study_title <- study_labels_de[study_id]

  dat_country <- score_long %>%
    filter(.data$study == .env$study_id) %>%
    mutate(
      construct_country = factor(
        as.character(construct_label),
        levels = c(study_models[[study_id]]$EXCL$label,
                   study_models[[study_id]]$POPU$label,
                   study_models[[study_id]]$PRG$label)
      )
    )

  p_scatter_country <- ggplot(
    dat_country,
    aes(x = scaled_midpoint, y = raw_index_z, color = construct_country)
  ) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "grey45") +
    geom_vline(xintercept = 0, linetype = "dashed", color = "grey45") +
    geom_point(alpha = 0.20, size = 0.8, na.rm = TRUE) +
    facet_wrap(~ construct_country, nrow = 1) +
    guides(color = "none") +
    labs(
      title = paste0(
        "Skalenmittige Hauptoperationalisierung vs. z-standardisierter Rohindex - ",
        study_title
      ),
      subtitle = "x = inhaltliche Lage zur theoretischen Skalenmitte; y = relative Lage zum Landes-/Konstruktmittel",
      caption = study_caption,
      x = "Skalenmittiger Konstruktwert [-1, +1]",
      y = "Rohindex z-standardisiert"
    ) +
    theme_minimal(base_size = 12) +
    theme(
      plot.title = element_text(face = "bold", size = 14),
      plot.subtitle = element_text(size = 10, color = "grey35"),
      plot.caption = element_text(size = 9, color = "grey35", hjust = 0),
      strip.text = element_text(face = "bold"),
      panel.grid.minor = element_blank()
    )

  ggsave(
    filename = file.path(
      DIR_PLOTS,
      paste0("scatter_scaled_midpoint_vs_raw_z_", study_id, ".png")
    ),
    plot = p_scatter_country,
    width = 12,
    height = 4.8,
    dpi = 320
  )

  dat_variants_country <- score_variants_long %>%
    filter(.data$study == .env$study_id) %>%
    left_join(
      dat_country %>%
        distinct(respondent_id, construct_key, construct_country),
      by = c("respondent_id", "construct_key")
    )

  p_density_country <- ggplot(
    dat_variants_country,
    aes(x = value, color = score_type, fill = score_type)
  ) +
    geom_density(alpha = 0.08, linewidth = 0.75, na.rm = TRUE) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "grey45") +
    facet_wrap(~ construct_country, nrow = 1, scales = "free_x") +
    labs(
      title = paste0(
        "Verteilungen unterschiedlicher Score-Transformationen - ",
        study_title
      ),
      subtitle = "Vergleich von Rohindex, Rohindex-z, skalenmittiger Hauptoperationalisierung und EFA-Faktorwert",
      caption = study_caption,
      x = "Scorewert",
      y = "Dichte",
      color = NULL,
      fill = NULL
    ) +
    theme_minimal(base_size = 12) +
    theme(
      plot.title = element_text(face = "bold", size = 14),
      plot.subtitle = element_text(size = 10, color = "grey35"),
      plot.caption = element_text(size = 9, color = "grey35", hjust = 0),
      strip.text = element_text(face = "bold"),
      legend.position = "bottom",
      panel.grid.minor = element_blank()
    )

  ggsave(
    filename = file.path(
      DIR_PLOTS,
      paste0("density_score_transformations_", study_id, ".png")
    ),
    plot = p_density_country,
    width = 12,
    height = 5.4,
    dpi = 320
  )
}

###############################################################################
## 6) HAUPTOPERATIONALISIERUNG: SCALED MIDPOINT
###############################################################################

DIR_MAIN_OPERATIONALISATION <- file.path(DIR_PLOTS, "main_operationalisation_scaled_midpoint")
dir.create(DIR_MAIN_OPERATIONALISATION, recursive = TRUE, showWarnings = FALSE)

p_scaled_density_common <- ggplot(
  score_long,
  aes(x = scaled_midpoint, color = common_perspective, fill = common_perspective)
) +
  geom_density(alpha = 0.18, linewidth = 0.75, na.rm = TRUE) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey45") +
  facet_grid(common_perspective ~ study_label_marked) +
  scale_x_continuous(limits = c(-1, 1), breaks = seq(-1, 1, 0.5)) +
  guides(color = "none", fill = "none") +
  labs(
    title = "Verteilung der Hauptoperationalisierung",
    subtitle = "Itemweise skalenmittige Transformation; gestrichelte Linie = theoretische Skalenmitte",
    caption = note_austria,
    x = "Skalenmittiger Konstruktwert [-1, +1]",
    y = "Dichte"
  ) +
  theme_minimal(base_size = 16) +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(size = 14, color = "grey35"),
    plot.caption = element_text(size = 14, color = "grey35", hjust = 0),
    strip.text = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  )

ggsave(
  filename = file.path(
    DIR_MAIN_OPERATIONALISATION,
    "main_scaled_midpoint_density_common.png"
  ),
  plot = p_scaled_density_common,
  width = 13,
  height = 7.2,
  dpi = 320
)

p_scaled_hist_common <- ggplot(
  score_long,
  aes(x = scaled_midpoint, fill = common_perspective)
) +
  geom_histogram(
    bins = 25,
    color = "white",
    linewidth = 0.25,
    na.rm = TRUE
  ) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey45") +
  facet_grid(common_perspective ~ study_label_marked, scales = "free_y") +
  scale_x_continuous(limits = c(-1, 1), breaks = seq(-1, 1, 0.5)) +
  guides(fill = "none") +
  labs(
    title = "Histogramme der Hauptoperationalisierung",
    subtitle = "Itemweise skalenmittige Transformation; gestrichelte Linie = theoretische Skalenmitte",
    caption = note_austria,
    x = "Skalenmittiger Konstruktwert [-1, +1]",
    y = "Anzahl"
  ) +
  theme_minimal(base_size = 16) +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(size = 14, color = "grey35"),
    plot.caption = element_text(size = 14, color = "grey35", hjust = 0),
    strip.text = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  )

ggsave(
  filename = file.path(
    DIR_MAIN_OPERATIONALISATION,
    "main_scaled_midpoint_histogram_common.png"
  ),
  plot = p_scaled_hist_common,
  width = 13,
  height = 7.2,
  dpi = 320
)

for (study_id in expected_studies) {
  study_caption <- if (study_id == "AUT_2017") note_austria else NULL
  study_title <- study_labels_de[study_id]

  dat_country <- score_long %>%
    filter(.data$study == .env$study_id) %>%
    mutate(
      construct_country = factor(
        as.character(construct_label),
        levels = c(study_models[[study_id]]$EXCL$label,
                   study_models[[study_id]]$POPU$label,
                   study_models[[study_id]]$PRG$label)
      )
    )

  p_scaled_density_country <- ggplot(
    dat_country,
    aes(x = scaled_midpoint, color = construct_country, fill = construct_country)
  ) +
    geom_density(alpha = 0.18, linewidth = 0.75, na.rm = TRUE) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "grey45") +
    facet_wrap(~ construct_country, nrow = 1) +
    scale_x_continuous(limits = c(-1, 1), breaks = seq(-1, 1, 0.5)) +
    guides(color = "none", fill = "none") +
    labs(
      title = paste0(
        "Verteilung der Hauptoperationalisierung - ",
        study_title
      ),
      subtitle = "Itemweise skalenmittige Transformation; gestrichelte Linie = theoretische Skalenmitte",
      caption = study_caption,
      x = "Skalenmittiger Konstruktwert [-1, +1]",
      y = "Dichte"
    ) +
    theme_minimal(base_size = 16) +
    theme(
      plot.title = element_text(face = "bold", size = 16),
      plot.subtitle = element_text(size = 14, color = "grey35"),
      plot.caption = element_text(size = 14, color = "grey35", hjust = 0),
      strip.text = element_text(face = "bold"),
      panel.grid.minor = element_blank()
    )

  ggsave(
    filename = file.path(
      DIR_MAIN_OPERATIONALISATION,
      paste0("main_scaled_midpoint_density_", study_id, ".png")
    ),
    plot = p_scaled_density_country,
    width = 12,
    height = 4.8,
    dpi = 320
  )

  p_scaled_hist_country <- ggplot(
    dat_country,
    aes(x = scaled_midpoint, fill = construct_country)
  ) +
    geom_histogram(
      bins = 25,
      color = "white",
      linewidth = 0.25,
      na.rm = TRUE
    ) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "grey45") +
    facet_wrap(~ construct_country, nrow = 1, scales = "free_y") +
    scale_x_continuous(limits = c(-1, 1), breaks = seq(-1, 1, 0.5)) +
    guides(fill = "none") +
    labs(
      title = paste0(
        "Histogramme der Hauptoperationalisierung - ",
        study_title
      ),
      subtitle = "Itemweise skalenmittige Transformation; gestrichelte Linie = theoretische Skalenmitte",
      caption = study_caption,
      x = "Skalenmittiger Konstruktwert [-1, +1]",
      y = "Anzahl"
    ) +
    theme_minimal(base_size = 16) +
    theme(
      plot.title = element_text(face = "bold", size = 16),
      plot.subtitle = element_text(size = 14, color = "grey35"),
      plot.caption = element_text(size = 14, color = "grey35", hjust = 0),
      strip.text = element_text(face = "bold"),
      panel.grid.minor = element_blank()
    )

  ggsave(
    filename = file.path(
      DIR_MAIN_OPERATIONALISATION,
      paste0("main_scaled_midpoint_histogram_", study_id, ".png")
    ),
    plot = p_scaled_hist_country,
    width = 12,
    height = 4.8,
    dpi = 320
  )
}

###############################################################################
## 7) ABSCHLUSS
###############################################################################

cat("\n============================================================\n")
cat("02_B/02_C Score-Transformationen abgeschlossen.\n")
cat("Outputs:\n")
cat("  Tabellen:", DIR_TABLES, "\n")
cat("  Plots:   ", DIR_PLOTS, "\n")
cat("============================================================\n")
