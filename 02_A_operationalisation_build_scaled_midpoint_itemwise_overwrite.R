###############################################################################
## DISS_2026 -- 02_D
## Operationalisierung: itemweise skalenmittig transformierte Scores
##
## Zweck:
##   Replikation der bisherigen Operationalisierungslogik aus 02_A, aber mit
##   korrigierter Scorebildung fuer Items mit unterschiedlichen Skalenlaengen:
##
##     5-Punkt-Items: item_scaled = (x - 3.0) / 2.0
##     4-Punkt-Items: item_scaled = (x - 2.5) / 1.5
##
##   Danach werden die Konstruktscores als Mittelwert der itemweise auf
##   [-1, +1] transformierten Items gebildet.
##
##   Die Output-RDS wird bewusst ueberschrieben, damit die Folgeskripte
##   mit denselben Dateinamen und Spaltennamen weiterlaufen koennen.
##
## Input:
##   data/processed/cses5/derived/CSES5_20262301_prepared.rds
##
## Output:
##   data/processed/cses5/derived/CSES5_20262301_operationalised_scaled_midpoint.rds
##   outputs/tables/cses5/operationalisation_DE_v3_items_short_raw_z_safe/
##   outputs/plots/cses5/histograms_DE_v3_items_short_raw_z_safe/
###############################################################################

suppressPackageStartupMessages({
  library(tidyverse)
  library(cowplot)
  library(ggh4x)
  library(here)
})

###############################################################################
## 0) PFADE
###############################################################################

DIR_DERIVED <- here("data", "processed", "cses5", "derived")
dir.create(DIR_DERIVED, recursive = TRUE, showWarnings = FALSE)

PATH_IN <- file.path(DIR_DERIVED, "CSES5_20262301_prepared.rds")

SAVE_OPERATIONALISED_RDS <- TRUE
OVERWRITE_OUTPUT_RDS <- TRUE
CREATE_BACKUP_IF_EXISTS <- FALSE

PATH_OUT <- file.path(
  DIR_DERIVED,
  "CSES5_20262301_operationalised_scaled_midpoint.rds"
)

DIR_TAB <- here(
  "outputs", "tables", "cses5",
  "operationalisation_DE_v3_items_short_raw_z_safe"
)
dir.create(DIR_TAB, recursive = TRUE, showWarnings = FALSE)

PATH_SUMMARY <- file.path(
  DIR_TAB,
  "summary_scaled_midpoint_20262301.csv"
)

PATH_SCORE_LONG <- file.path(
  DIR_TAB,
  "scores_scaled_midpoint_long_20262301.csv"
)

PATH_ITEM_SCALE_CHECK <- file.path(
  DIR_TAB,
  "item_scale_check_AUT_DEU_GRC_20262301.csv"
)

DIR_HIST <- here(
  "outputs", "plots", "cses5",
  "histograms_DE_v3_items_short_raw_z_safe"
)

DIR_HIST_SCORES <- file.path(DIR_HIST, "scores_scaled_midpoint_png")
DIR_HIST_ITEMS  <- file.path(DIR_HIST, "items_scaled_midpoint_png")
DIR_HIST_PDF    <- file.path(DIR_HIST, "pdf_scaled_midpoint")

for (d in c(DIR_HIST_SCORES, DIR_HIST_ITEMS, DIR_HIST_PDF)) {
  dir.create(d, recursive = TRUE, showWarnings = FALSE)
}

if (!file.exists(PATH_IN)) {
  stop("Input RDS nicht gefunden: ", PATH_IN)
}

if (file.exists(PATH_OUT)) {
  if (!isTRUE(OVERWRITE_OUTPUT_RDS)) {
    stop(
      "Output-RDS existiert bereits und wird nicht ueberschrieben: ", PATH_OUT,
      "\nSetze OVERWRITE_OUTPUT_RDS <- TRUE oder passe PATH_OUT an."
    )
  }

  if (isTRUE(CREATE_BACKUP_IF_EXISTS)) {
    backup_path <- paste0(
      PATH_OUT,
      ".backup_",
      format(Sys.time(), "%Y%m%d_%H%M%S")
    )
    file.copy(PATH_OUT, backup_path, overwrite = FALSE)
    cat("Bestehende Output-RDS wurde gesichert unter:\n", backup_path, "\n")
  } else {
    cat("Bestehende Output-RDS wird bewusst ueberschrieben:\n", PATH_OUT, "\n")
  }
}

###############################################################################
## 1) DATEN LADEN
###############################################################################

CSES5 <- readRDS(PATH_IN)

stopifnot(
  "election_study_id" %in% names(CSES5),
  "respondent_id"     %in% names(CSES5)
)

cat("Datensatz geladen | N =", nrow(CSES5), "| Variablen =", ncol(CSES5), "\n")
cat("N je Studie:\n")
print(table(CSES5$election_study_id))

###############################################################################
## 2) ITEM-DEFINITIONEN
###############################################################################

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

item_meta <- tribble(
  ~var,                          ~short,   ~long,
  "majority_over_minority_rev",  "EXCL1",  "Mehrheitswille vor Minderheitenrechten (rekodiert)",
  "minorities_adapt_rev",        "EXCL2",  "Minderheiten sollen sich an Gebräuche anpassen (rekodiert)",
  "follow_customs_rev",          "EXCL3",  "Gebräuchen und Traditionen zu folgen ist wichtig (rekodiert)",
  "immig_harm_culture_rev",      "EXCL4",  "Zuwanderung schadet der Kultur (rekodiert)",
  "born_country_rev",            "EXCL5",  "Im Land geboren zu sein ist wichtig (rekodiert)",
  "people_make_policy_rev",      "POPU1",  "Das Volk sollte Politik unmittelbar bestimmen (rekodiert)",
  "pol_no_care_rev",             "POPU2",  "Politiker kümmern sich nicht um die Menschen (rekodiert)",
  "pol_for_rich_rev",            "POPU3",  "Politiker dienen den Reichen (rekodiert)",
  "corruption_widespread_rev",   "POPU4",  "Korruption ist weit verbreitet (rekodiert)",
  "pol_main_problem_rev",        "POPU5",  "Politiker sind das Hauptproblem (rekodiert)",
  "comp_sellout",                "PRG1",   "Kompromiss bedeutet Ausverkauf (rekodiert)",
  "voting_makes_difference",     "PRG2",   "Wählen macht einen Unterschied",
  "power_makes_difference",      "PRG3",   "Es macht einen Unterschied, wer an der Macht ist",
  "pol_trustworthy_rev",         "PRG4",   "Politiker sind vertrauenswürdig (rekodiert)"
)

## Skalenlaengen der verwendeten Items.
## Drei CSES-Items sind vierstufig ohne empirischen Mittelpunkt; alle anderen
## verwendeten Items sind fuenfstufig. Fuer die LPA werden daher alle Items
## zunaechst itemweise auf eine gemeinsame skalenmittige Metrik [-1, +1]
## gebracht und erst danach zu Konstruktscores gemittelt.
item_scale_meta <- tribble(
  ~var,                          ~min_val, ~max_val,
  "majority_over_minority_rev",        1,       5,
  "minorities_adapt_rev",              1,       5,
  "follow_customs_rev",                1,       4,
  "immig_harm_culture_rev",            1,       5,
  "born_country_rev",                  1,       4,
  "people_make_policy_rev",            1,       5,
  "pol_no_care_rev",                   1,       5,
  "pol_for_rich_rev",                  1,       5,
  "corruption_widespread_rev",         1,       4,
  "pol_main_problem_rev",              1,       5,
  "comp_sellout",                      1,       5,
  "voting_makes_difference",           1,       5,
  "power_makes_difference",            1,       5,
  "pol_trustworthy_rev",               1,       5
)

scale_midpoint_item <- function(x, min_val, max_val) {
  midpoint   <- (min_val + max_val) / 2
  half_range <- (max_val - min_val) / 2
  (x - midpoint) / half_range
}

scale_items_to_midpoint <- function(dat, items) {
  dat_scaled <- dat[, items, drop = FALSE]

  for (v in items) {
    scale_info <- item_scale_meta %>% filter(var == v)

    if (nrow(scale_info) != 1) {
      stop("Keine eindeutige Skaleninformation fuer Item: ", v)
    }

    dat_scaled[[v]] <- scale_midpoint_item(
      dat_scaled[[v]],
      min_val = scale_info$min_val,
      max_val = scale_info$max_val
    )
  }

  dat_scaled
}

missing_scale_meta <- setdiff(
  unique(c(all_items_excl, all_items_popu, all_items_prg)),
  item_scale_meta$var
)

if (length(missing_scale_meta) > 0) {
  stop(
    "Fuer folgende Items fehlen Skaleninformationen: ",
    paste(missing_scale_meta, collapse = ", ")
  )
}


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

study_labels <- c(
  AUT_2017 = "Österreich 2017",
  DEU_2021 = "Deutschland 2021",
  GRC_2019 = "Griechenland 2019"
)


###############################################################################
## 1B) ITEM-SKALEN PRUEFEN UND DOKUMENTIEREN
###############################################################################

items_check <- unique(c(all_items_excl, all_items_popu, all_items_prg))

item_scale_check <- CSES5 %>%
  filter(election_study_id %in% names(study_models)) %>%
  select(election_study_id, all_of(items_check)) %>%
  pivot_longer(
    cols = -election_study_id,
    names_to = "item",
    values_to = "value"
  ) %>%
  filter(!is.na(value)) %>%
  group_by(election_study_id, item) %>%
  summarise(
    min_observed = min(value),
    max_observed = max(value),
    unique_values = paste(sort(unique(value)), collapse = ", "),
    n_values = n_distinct(value),
    n = n(),
    .groups = "drop"
  ) %>%
  left_join(item_scale_meta, by = c("item" = "var")) %>%
  arrange(item, election_study_id)

print(item_scale_check, n = Inf)
write_csv(item_scale_check, PATH_ITEM_SCALE_CHECK)
cat("Item-Skalencheck gespeichert:", PATH_ITEM_SCALE_CHECK, "
")

###############################################################################
## 3) SCORES BERECHNEN JE LAND
###############################################################################

scores_list  <- list()
summary_list <- list()
long_list    <- list()

for (study_id in names(study_models)) {

  cat("\n============================================================\n")
  cat(study_id, "\n")
  cat("============================================================\n")

  model <- study_models[[study_id]]
  items_study <- c(model$EXCL$items, model$POPU$items, model$PRG$items)

  dat_study <- CSES5 %>%
    filter(election_study_id == .env$study_id) %>%
    select(respondent_id, all_of(items_study)) %>%
    drop_na()

  cat("N (complete cases):", nrow(dat_study), "\n")

  construct_cfg <- tibble(
    construct_key = c("EXCL", "POPU", "PRG"),
    construct     = c(model$EXCL$label, model$POPU$label, model$PRG$label),
    score_name    = c(model$EXCL$score_name,
                      model$POPU$score_name,
                      model$PRG$score_name),
    items         = list(model$EXCL$items,
                         model$POPU$items,
                         model$PRG$items)
  )

  scores_wide <- dat_study %>%
    select(respondent_id)

  score_long_study <- list()

  for (i in seq_len(nrow(construct_cfg))) {
    cfg <- construct_cfg[i, ]
    items_i <- cfg$items[[1]]
    score_i <- cfg$score_name

    raw_i <- rowMeans(dat_study[, items_i, drop = FALSE], na.rm = FALSE)

    items_scaled_i <- scale_items_to_midpoint(dat_study, items_i)
    scaled_i <- rowMeans(items_scaled_i, na.rm = FALSE)

    ## Fuer rueckwaertskompatible Spaltennamen bleibt *_centered erhalten.
    ## Inhaltlich entspricht die zentrale metrische Groesse ab jetzt dem
    ## Mittelwert der itemweise skalenmittig transformierten Items.
    centered_i <- scaled_i

    scores_wide[[paste0(score_i, "_raw_index")]] <- raw_i
    scores_wide[[paste0(score_i, "_centered")]] <- centered_i
    scores_wide[[paste0(score_i, "_scaled_midpoint")]] <- scaled_i

    score_long_study[[cfg$construct_key]] <- tibble(
      study = study_id,
      study_label = study_labels[study_id],
      respondent_id = dat_study$respondent_id,
      construct_key = cfg$construct_key,
      construct = cfg$construct,
      score_name = score_i,
      raw_index = raw_i,
      centered = centered_i,
      scaled_midpoint = scaled_i
    )
  }

  scores_list[[study_id]] <- scores_wide
  long_list[[study_id]] <- bind_rows(score_long_study)

  summary_list[[study_id]] <- bind_rows(score_long_study) %>%
    group_by(study, study_label, construct_key, construct, score_name) %>%
    summarise(
      n_valid = sum(!is.na(scaled_midpoint)),
      n_items = length(construct_cfg$items[[match(first(construct_key),
                                                  construct_cfg$construct_key)]]),
      mean_raw_index = mean(raw_index, na.rm = TRUE),
      sd_raw_index = sd(raw_index, na.rm = TRUE),
      median_raw_index = median(raw_index, na.rm = TRUE),
      mean_centered = mean(centered, na.rm = TRUE),
      sd_centered = sd(centered, na.rm = TRUE),
      mean_scaled_midpoint = mean(scaled_midpoint, na.rm = TRUE),
      sd_scaled_midpoint = sd(scaled_midpoint, na.rm = TRUE),
      share_above_midpoint = mean(scaled_midpoint > 0, na.rm = TRUE),
      share_below_midpoint = mean(scaled_midpoint < 0, na.rm = TRUE),
      .groups = "drop"
    )

  cat("\nSummary:\n")
  print(summary_list[[study_id]])
}

score_long <- bind_rows(long_list)
summary_table <- bind_rows(summary_list) %>%
  mutate(
    across(
      where(is.numeric),
      ~ round(.x, 4)
    )
  )

###############################################################################
## 4) SCORES IN CSES5 JOINEN
###############################################################################

cat("\n============================================================\n")
cat("Skalenmittig transformierte Scores werden mit CSES5 verbunden...\n")

new_score_pattern <- paste(
  "EXCL_score_(raw_index|centered|scaled_midpoint)",
  "POPU_score_(raw_index|centered|scaled_midpoint)",
  "PRG_score_(raw_index|centered|scaled_midpoint)",
  "Anti_Elitist_score_(raw_index|centered|scaled_midpoint)",
  "Minimal_PRG_score_(raw_index|centered|scaled_midpoint)",
  sep = "|"
)

CSES5_clean <- CSES5 %>%
  select(-matches(new_score_pattern))

scores_combined <- bind_rows(
  lapply(names(scores_list), function(study_id) {
    scores_list[[study_id]] %>%
      mutate(election_study_id = study_id)
  })
)

CSES5_operationalised_scaled <- CSES5_clean %>%
  left_join(scores_combined, by = c("respondent_id", "election_study_id"))

stopifnot(!any(str_detect(names(CSES5_operationalised_scaled), "\\.x|\\.y")))

score_vars <- names(CSES5_operationalised_scaled)[
  str_detect(names(CSES5_operationalised_scaled), new_score_pattern)
]

cat("Neue Score-Variablen (", length(score_vars), "):\n", sep = "")
print(score_vars)

###############################################################################
## 5) PLOTS
###############################################################################

cat("\n============================================================\n")
cat("Erstelle Plots...\n")

plot_config <- list(
  AUT_2017 = tibble(
    score_scaled = c("EXCL_score_scaled_midpoint",
                     "Anti_Elitist_score_scaled_midpoint",
                     "Minimal_PRG_score_scaled_midpoint"),
    label = c("Exklusionistische Demokratie",
              "Anti-elitistische Demokratie",
              "Minimal-pragmatische Demokratie")
  ),
  DEU_2021 = tibble(
    score_scaled = c("EXCL_score_scaled_midpoint",
                     "POPU_score_scaled_midpoint",
                     "PRG_score_scaled_midpoint"),
    label = c("Exklusionistische Demokratie",
              "Populistische Demokratie",
              "Pragmatische Demokratie")
  ),
  GRC_2019 = tibble(
    score_scaled = c("EXCL_score_scaled_midpoint",
                     "POPU_score_scaled_midpoint",
                     "PRG_score_scaled_midpoint"),
    label = c("Exklusionistische Demokratie",
              "Populistische Demokratie",
              "Pragmatische Demokratie")
  )
)

plot_scores_scaled <- function(dat, config, study_id) {
  d_long <- dat %>%
    filter(election_study_id == .env$study_id) %>%
    select(all_of(config$score_scaled)) %>%
    pivot_longer(
      everything(),
      names_to = "score",
      values_to = "value"
    ) %>%
    drop_na(value) %>%
    left_join(config %>% rename(score = score_scaled), by = "score") %>%
    mutate(label = factor(label, levels = config$label))

  ggplot(d_long, aes(x = value, fill = label)) +
    geom_histogram(
      bins = 25,
      color = "white",
      linewidth = 0.2
    ) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "grey55") +
    facet_wrap(~ label, ncol = 3, scales = "free_y") +
    scale_x_continuous(limits = c(-1, 1), breaks = seq(-1, 1, 0.5)) +
    guides(fill = "none") +
    labs(
      title = paste0(study_labels[study_id],
                     " - Verteilungen der skalenmittigen Scores"),
      subtitle = "Mittelwert itemweise skalenmittig transformierter Items; gestrichelte Linie = theoretische Skalenmitte",
      x = "Skalenmittiger Score [-1, +1]",
      y = "Anzahl"
    ) +
    theme_minimal(base_size = 12) +
    theme(
      plot.title = element_text(face = "bold", size = 13),
      plot.subtitle = element_text(size = 10, color = "grey40"),
      strip.text = element_text(face = "bold"),
      panel.grid.minor = element_blank()
    )
}

plot_items <- function(dat, model, study_id) {
  items_excl <- model$EXCL$items
  items_popu <- model$POPU$items
  items_prg  <- model$PRG$items

  all_item_vars <- c(items_excl, items_popu, items_prg)

  item_short_map <- tibble(
    var = all_item_vars,
    short = c(paste0("EXCL", seq_along(items_excl)),
              paste0("POPU", seq_along(items_popu)),
              paste0("PRG", seq_along(items_prg))),
    block = c(rep(model$EXCL$label, length(items_excl)),
              rep(model$POPU$label, length(items_popu)),
              rep(model$PRG$label, length(items_prg)))
  )

  d_long <- dat %>%
    filter(election_study_id == .env$study_id) %>%
    select(any_of(all_item_vars)) %>%
    pivot_longer(
      everything(),
      names_to = "var",
      values_to = "value"
    ) %>%
    mutate(
      value = suppressWarnings(as.integer(as.character(value))),
      value = ifelse(value %in% 1:5, value, NA_integer_),
      value = factor(value, levels = 1:5)
    ) %>%
    left_join(item_short_map, by = "var") %>%
    mutate(
      short = factor(short, levels = item_short_map$short),
      block = factor(block, levels = c(model$EXCL$label,
                                       model$POPU$label,
                                       model$PRG$label))
    ) %>%
    filter(!is.na(short))

  ggplot(d_long, aes(x = value)) +
    geom_bar(na.rm = TRUE, fill = "grey30") +
    ggh4x::facet_nested(
      ~ block + short,
      scales = "fixed",
      nest_line = element_line(linewidth = 0.5)
    ) +
    scale_x_discrete(drop = FALSE) +
    labs(
      title = paste0(study_labels[study_id],
                     " - Itemverteilungen nach Konstrukt"),
      subtitle = paste0(model$EXCL$label, "  |  ",
                        model$POPU$label, "  |  ",
                        model$PRG$label),
      x = "Antwortskala (je nach Item 1-4 oder 1-5)",
      y = "Anzahl"
    ) +
    theme_minimal(base_size = 11) +
    theme(
      plot.title = element_text(face = "bold"),
      plot.subtitle = element_text(size = 9, color = "grey40"),
      strip.text = element_text(face = "bold", size = 9),
      panel.grid.minor = element_blank(),
      panel.spacing = unit(0.3, "lines")
    )
}

pdf_scores <- file.path(DIR_HIST_PDF, "hist_scores_scaled_midpoint_ALL_20262301.pdf")
pdf_items  <- file.path(DIR_HIST_PDF, "hist_items_scaled_midpoint_ALL_20262301.pdf")

score_plots <- list()
for (study_id in names(study_models)) {
  p <- plot_scores_scaled(
    CSES5_operationalised_scaled,
    plot_config[[study_id]],
    study_id
  )
  ggsave(
    file.path(DIR_HIST_SCORES, paste0("hist_scores_scaled_midpoint_", study_id, ".png")),
    p,
    width = 12,
    height = 5,
    dpi = 300
  )
  score_plots[[study_id]] <- p
  cat("PNG gespeichert: hist_scores_scaled_midpoint_", study_id, ".png\n", sep = "")
}

pdf(pdf_scores, width = 12, height = 5)
for (study_id in names(study_models)) print(score_plots[[study_id]])
dev.off()
cat("PDF gespeichert:", pdf_scores, "\n")

item_plots <- list()
for (study_id in names(study_models)) {
  p <- plot_items(CSES5_operationalised_scaled, study_models[[study_id]], study_id)
  ggsave(
    file.path(DIR_HIST_ITEMS, paste0("hist_items_scaled_midpoint_", study_id, ".png")),
    p,
    width = 13.5,
    height = 5.8,
    dpi = 300
  )
  item_plots[[study_id]] <- p
  cat("PNG gespeichert: hist_items_scaled_midpoint_", study_id, ".png\n", sep = "")
}

pdf(pdf_items, width = 13.5, height = 5.8)
for (study_id in names(study_models)) print(item_plots[[study_id]])
dev.off()
cat("PDF gespeichert:", pdf_items, "\n")

###############################################################################
## 6) SPEICHERN
###############################################################################

write_csv(summary_table, PATH_SUMMARY)
write_csv(score_long, PATH_SCORE_LONG)

if (isTRUE(SAVE_OPERATIONALISED_RDS)) {
  saveRDS(CSES5_operationalised_scaled, PATH_OUT)
  cat("RDS gespeichert:", PATH_OUT, "\n")
}

cat("\n============================================================\n")
cat("Fertig.\n\n")
cat("Neue RDS:       ", PATH_OUT, "\n")
cat("Summary CSV:    ", PATH_SUMMARY, "\n")
cat("Long CSV:       ", PATH_SCORE_LONG, "\n\n")
cat("Scores PNG:     ", DIR_HIST_SCORES, "\n")
cat("Items PNG:      ", DIR_HIST_ITEMS, "\n")
cat("PDFs:           ", DIR_HIST_PDF, "\n\n")
cat("Neue Hauptvariablen fuer LPA:\n")
cat("  AUT_2017: EXCL_score_scaled_midpoint, Anti_Elitist_score_scaled_midpoint, Minimal_PRG_score_scaled_midpoint\n")
cat("  DEU_2021: EXCL_score_scaled_midpoint, POPU_score_scaled_midpoint, PRG_score_scaled_midpoint\n")
cat("  GRC_2019: EXCL_score_scaled_midpoint, POPU_score_scaled_midpoint, PRG_score_scaled_midpoint\n")
cat("============================================================\n")

