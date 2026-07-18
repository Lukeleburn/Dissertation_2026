###############################################################################
## DISS_2026 — 01_B_deskriptive_item_skalen_histogramme_DE.R
## Deskriptive Analyse der verwendeten Demokratie-Items und Skalen
## + Histogramme der Items, Faktorwerte und Rohwert/z-Vergleiche
##
## Messmodelle:
##   AUT_2017:
##     EXCL = EXCL1–5        (exklusionistische Demokratie)          5 Items
##     POPU = POPU2–5        (anti-elitistische Demokratie)          4 Items
##     PRG  = PRG2–3         (minimal-pragmatische Demokratie)       2 Items
##
##   DEU_2021 + GRC_2019:
##     EXCL = EXCL1–5        (exklusionistische Demokratie)          5 Items
##     POPU = POPU1–5        (populistische Demokratie)              5 Items
##     PRG  = PRG1–4         (pragmatische Demokratie)               4 Items
##
## Methode: Faktorwerte aus EFA mit obliquer Target-Rotation (TargetQ)
##
## Input:  data/processed/cses5/derived/CSES5_20262301_prepared.rds
## Output: outputs/tables/cses5/01_B_deskriptive_item_skalen_histogramme_DE/
##         outputs/plots/cses5/01_B_deskriptive_item_skalen_histogramme_DE/ (PNG + PDF)
##
## Hinweis: Dieses Skript ist eine reine deskriptive Vorstufe. Es liest den
##          vorbereiteten CSES5-Datensatz ein, rekonstruiert die Skalenwerte
##          in-memory und schreibt keine Datendatei zurück.
###############################################################################
suppressPackageStartupMessages({
  library(tidyverse)
  library(psych)
  library(GPArotation)
  library(cowplot)
  library(ggh4x)
  library(here)
})

###############################################################################
## 0) PFADE
###############################################################################

DIR_DERIVED <- here("data", "processed", "cses5", "derived")
dir.create(DIR_DERIVED, recursive = TRUE, showWarnings = FALSE)

PATH_IN  <- file.path(DIR_DERIVED, "CSES5_20262301_prepared.rds")

DIR_TAB  <- here("outputs", "tables", "cses5", "01_B_deskriptive_item_skalen_histogramme_DE")
dir.create(DIR_TAB, recursive = TRUE, showWarnings = FALSE)
PATH_SUMMARY <- file.path(DIR_TAB, "summary_factorscores_20262301.csv")
PATH_ITEM_DESC <- file.path(DIR_TAB, "deskriptive_items_20262301.csv")
PATH_SCALE_DESC <- file.path(DIR_TAB, "deskriptive_skalen_20262301.csv")

DIR_HIST        <- here("outputs", "plots", "cses5", "01_B_deskriptive_item_skalen_histogramme_DE")
DIR_HIST_SCORES <- file.path(DIR_HIST, "scores_png")
DIR_HIST_ITEMS  <- file.path(DIR_HIST, "items_png")
DIR_HIST_ALL_ITEMS <- file.path(DIR_HIST, "all_postulated_items_png")
DIR_HIST_PDF    <- file.path(DIR_HIST, "pdf")
for (d in c(DIR_HIST_SCORES, DIR_HIST_ITEMS, DIR_HIST_ALL_ITEMS, DIR_HIST_PDF)) {
  dir.create(d, recursive = TRUE, showWarnings = FALSE)
}

if (!file.exists(PATH_IN)) {
  stop("Input RDS nicht gefunden: ", PATH_IN)
}

###############################################################################
## 1) DATEN LADEN
###############################################################################

CSES5 <- readRDS(PATH_IN)
stopifnot("election_study_id" %in% names(CSES5),
          "respondent_id"     %in% names(CSES5))

cat("Datensatz geladen | N =", nrow(CSES5), "| Variablen =", ncol(CSES5), "\n")
cat("N je Studie:\n"); print(table(CSES5$election_study_id))

###############################################################################
## 2) ITEM-DEFINITIONEN
###############################################################################

all_items_excl <- c("majority_over_minority_rev", "minorities_adapt_rev",
                    "follow_customs_rev",          "immig_harm_culture_rev",
                    "born_country_rev")

all_items_popu <- c("people_make_policy_rev",  "pol_no_care_rev",
                    "pol_for_rich_rev",         "corruption_widespread_rev",
                    "pol_main_problem_rev")

all_items_prg  <- c("comp_sellout",           "voting_makes_difference",
                    "power_makes_difference",  "pol_trustworthy_rev")

# Zentrale Item-Metadaten (Kurzname + langer Name)
item_meta <- tribble(
  ~var,                          ~short,   ~long,
  "majority_over_minority_rev",  "EXCL1",  "Mehrheit soll sich gegenüber Minderheitsrechten durchsetzen (rekodiert)",
  "minorities_adapt_rev",        "EXCL2",  "Minderheiten sollen sich nationalen Gepflogenheiten anpassen (rekodiert)",
  "follow_customs_rev",          "EXCL3",  "Befolgung nationaler Gepflogenheiten ist wichtig (rekodiert)",
  "immig_harm_culture_rev",      "EXCL4",  "Einwanderer schaden der nationalen Kultur (rekodiert)",
  "born_country_rev",            "EXCL5",  "Im Land geboren zu sein ist wichtig (rekodiert)",
  "people_make_policy_rev",      "POPU1",  "Das Volk, nicht Politiker, soll Politik bestimmen (rekodiert)",
  "pol_no_care_rev",             "POPU2",  "Politiker kümmern sich nicht um die Menschen (rekodiert)",
  "pol_for_rich_rev",            "POPU3",  "Politiker dienen nur Reichen und Mächtigen (rekodiert)",
  "corruption_widespread_rev",   "POPU4",  "Politische Korruption ist weit verbreitet (rekodiert)",
  "pol_main_problem_rev",        "POPU5",  "Politiker sind das Hauptproblem (rekodiert)",
  "comp_sellout",                "PRG1",   "Ablehnung von: Kompromiss bedeutet Ausverkauf (rekodiert)",
  "voting_makes_difference",     "PRG2",   "Wählen macht einen großen Unterschied",
  "power_makes_difference",      "PRG3",   "Wer Macht hat, macht einen großen Unterschied",
  "pol_trustworthy_rev",         "PRG4",   "Die meisten Politiker sind vertrauenswürdig (rekodiert)"
)

# Länderspezifische Messmodelle
study_models <- list(

  AUT_2017 = list(
    EXCL = list(items      = all_items_excl,
                label      = "Exklusionistische Demokratie",
                score_name = "EXCL_score"),
    POPU = list(items      = all_items_popu[2:5],
                label      = "Anti-elitistische Demokratie",
                score_name = "Anti_Elitist_score"),
    PRG  = list(items      = c("voting_makes_difference",
                               "power_makes_difference"),
                label      = "Minimal-pragmatische Demokratie",
                score_name = "Minimal_PRG_score")
  ),

  DEU_2021 = list(
    EXCL = list(items      = all_items_excl,
                label      = "Exklusionistische Demokratie",
                score_name = "EXCL_score"),
    POPU = list(items      = all_items_popu,
                label      = "Populistische Demokratie",
                score_name = "POPU_score"),
    PRG  = list(items      = all_items_prg,
                label      = "Pragmatische Demokratie",
                score_name = "PRG_score")
  ),

  GRC_2019 = list(
    EXCL = list(items      = all_items_excl,
                label      = "Exklusionistische Demokratie",
                score_name = "EXCL_score"),
    POPU = list(items      = all_items_popu,
                label      = "Populistische Demokratie",
                score_name = "POPU_score"),
    PRG  = list(items      = all_items_prg,
                label      = "Pragmatische Demokratie",
                score_name = "PRG_score")
  )
)

study_labels <- c(
  AUT_2017 = "Österreich 2017",
  DEU_2021 = "Deutschland 2021",
  GRC_2019 = "Griechenland 2019"
)

###############################################################################
## 3) HILFSFUNKTIONEN
###############################################################################

build_target <- function(model) {
  n_excl  <- length(model$EXCL$items)
  n_popu  <- length(model$POPU$items)
  n_prg   <- length(model$PRG$items)
  n_total <- n_excl + n_popu + n_prg
  target  <- matrix(0, nrow = n_total, ncol = 3)
  target[1:n_excl,                         1] <- 1
  target[(n_excl+1):(n_excl+n_popu),       2] <- 1
  target[(n_excl+n_popu+1):n_total,        3] <- 1
  target
}

align_factors <- function(loadings_raw, target_matrix, factor_names) {
  cong      <- psych::factor.congruence(loadings_raw, target_matrix)
  order_idx <- apply(abs(cong), 2, which.max)
  if (length(unique(order_idx)) < length(order_idx)) {
    warning("Faktor-Zuordnung nicht eindeutig")
  }
  loadings_ordered <- loadings_raw[, order_idx, drop = FALSE]
  colnames(loadings_ordered) <- factor_names
  for (j in seq_along(factor_names)) {
    target_items <- which(target_matrix[, j] == 1)
    if (mean(loadings_ordered[target_items, j], na.rm = TRUE) < 0) {
      loadings_ordered[, j] <- loadings_ordered[, j] * -1
      cat("  Vorzeichen gespiegelt:", factor_names[j], "\n")
    }
  }
  list(loadings = loadings_ordered, order_idx = order_idx)
}

###############################################################################
## 4) FAKTORWERTE BERECHNEN JE LAND
###############################################################################

scores_list  <- list()
summary_list <- list()
factor_names <- c("EXCL", "POPU", "PRG")

for (study in names(study_models)) {

  cat("\n============================================================\n")
  cat(study, "\n")
  cat("============================================================\n")

  model       <- study_models[[study]]
  items_study <- c(model$EXCL$items, model$POPU$items, model$PRG$items)

  dat_study <- CSES5 %>%
    filter(election_study_id == study) %>%
    select(respondent_id, all_of(items_study)) %>%
    drop_na()

  cat("N (complete cases):", nrow(dat_study), "\n")

  dat_items <- dat_study %>% select(all_of(items_study))
  target    <- build_target(model)

  fa_unrot <- psych::fa(dat_items, nfactors = 3,
                        rotate = "none", fm = "minres")

  rotated <- GPArotation::GPFoblq(
    A          = fa_unrot$loadings,
    method     = "targetQ",
    methodArgs = list(Target = target)
  )

  load_raw           <- rotated$loadings
  rownames(load_raw) <- items_study
  aligned            <- align_factors(load_raw, target, factor_names)
  load_aligned       <- aligned$loadings

  cat("\nLadungen (ausgerichtet):\n")
  print(round(load_aligned, 3))

  fs <- psych::factor.scores(
    x      = dat_items,
    f      = load_aligned,
    method = "regression",
    rho    = cor(dat_items, use = "pairwise.complete.obs")
  )

  scores_raw <- as.data.frame(fs$scores)
  colnames(scores_raw) <- factor_names

  scores_z <- as.data.frame(scale(scores_raw))
  colnames(scores_z) <- paste0(factor_names, "_z")

  sn_excl <- model$EXCL$score_name
  sn_popu <- model$POPU$score_name
  sn_prg  <- model$PRG$score_name

  scores_named <- scores_raw %>%
    rename(!!sn_excl := EXCL,
           !!sn_popu := POPU,
           !!sn_prg  := PRG)

  scores_named_z <- scores_z %>%
    rename(!!paste0(sn_excl, "_z") := EXCL_z,
           !!paste0(sn_popu, "_z") := POPU_z,
           !!paste0(sn_prg,  "_z") := PRG_z)

  scores_list[[study]] <- bind_cols(
    dat_study %>% select(respondent_id),
    scores_named,
    scores_named_z
  )

  tucker <- round(diag(psych::factor.congruence(load_aligned, target)), 3)
  names(tucker) <- factor_names
  cat("\nTucker's phi:\n"); print(tucker)

  if (!is.null(rotated$Phi)) {
    phi <- rotated$Phi[aligned$order_idx, aligned$order_idx]
    rownames(phi) <- colnames(phi) <- factor_names
    cat("\nFaktorkorrelationen:\n"); print(round(phi, 3))
  }

  summary_list[[study]] <- tibble(
    study      = study,
    construct  = c(model$EXCL$label, model$POPU$label, model$PRG$label),
    score_name = c(sn_excl, sn_popu, sn_prg),
    n_items    = c(length(model$EXCL$items),
                   length(model$POPU$items),
                   length(model$PRG$items)),
    tucker_phi = tucker,
    n_valid    = nrow(dat_study),
    mean_score = c(mean(scores_named[[sn_excl]]),
                   mean(scores_named[[sn_popu]]),
                   mean(scores_named[[sn_prg]])),
    sd_score   = c(sd(scores_named[[sn_excl]]),
                   sd(scores_named[[sn_popu]]),
                   sd(scores_named[[sn_prg]]))
  )

  cat("\nSummary:\n"); print(summary_list[[study]])
}

###############################################################################
## 5) SCORES IN CSES5 JOINEN
###############################################################################

cat("\n============================================================\n")
cat("Faktorwerte werden wieder mit CSES5 verbunden...\n")

score_pattern <- "EXCL_score|POPU_score|PRG_score|Anti_Elitist|Minimal_PRG"
CSES5_clean   <- CSES5 %>% select(-matches(score_pattern))

scores_combined <- bind_rows(
  lapply(names(scores_list), function(s) {
    scores_list[[s]] %>% mutate(election_study_id = s)
  })
)

CSES5_operationalised <- CSES5_clean %>%
  left_join(scores_combined, by = c("respondent_id", "election_study_id"))

stopifnot(!any(str_detect(names(CSES5_operationalised), "\\.x|\\.y")))

score_vars <- names(CSES5_operationalised)[
  str_detect(names(CSES5_operationalised), score_pattern)
]
cat("Score-Variablen (", length(score_vars), "):\n")
print(score_vars)

###############################################################################
## 6) DESKRIPTIVE TABELLEN
###############################################################################

cat("\n============================================================\n")
cat("Erstelle deskriptive Tabellen für Items und Skalen...\n")

make_item_descriptives <- function(dat, model, study) {
  item_map <- bind_rows(
    tibble(
      var = model$EXCL$items,
      construct = model$EXCL$label,
      item_short = paste0("EXCL", seq_along(model$EXCL$items))
    ),
    tibble(
      var = model$POPU$items,
      construct = model$POPU$label,
      item_short = paste0("POPU", seq_along(model$POPU$items))
    ),
    tibble(
      var = model$PRG$items,
      construct = model$PRG$label,
      item_short = paste0("PRG", seq_along(model$PRG$items))
    )
  ) %>%
    left_join(item_meta, by = "var") %>%
    mutate(item_label = coalesce(.data$long, .data$item_short))

  dat %>%
    filter(.data$election_study_id == .env$study) %>%
    select(any_of(item_map$var)) %>%
    pivot_longer(everything(), names_to = "var", values_to = "value") %>%
    mutate(value_num = suppressWarnings(as.numeric(as.character(.data$value)))) %>%
    left_join(item_map, by = "var") %>%
    group_by(var, item_short, item_label, construct) %>%
    summarise(
      study = .env$study,
      study_label = .env$study_labels[[.env$study]],
      n_total = n(),
      n_valid = sum(!is.na(.data$value_num)),
      n_missing = sum(is.na(.data$value_num)),
      mean = mean(.data$value_num, na.rm = TRUE),
      sd = sd(.data$value_num, na.rm = TRUE),
      min = min(.data$value_num, na.rm = TRUE),
      max = max(.data$value_num, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    select(study, study_label, construct, item_short, var, item_label,
           n_total, n_valid, n_missing, mean, sd, min, max)
}

make_scale_descriptives <- function(dat, model, study) {
  scale_map <- tibble(
    construct = c(model$EXCL$label, model$POPU$label, model$PRG$label),
    score_raw = c(model$EXCL$score_name,
                  model$POPU$score_name,
                  model$PRG$score_name),
    score_z = paste0(c(model$EXCL$score_name,
                       model$POPU$score_name,
                       model$PRG$score_name), "_z")
  )

  bind_rows(
    scale_map %>%
      transmute(construct, score = score_raw, scaling = "Faktorisiert"),
    scale_map %>%
      transmute(construct, score = score_z, scaling = " Faktorisiert + z-standardisiert")
  ) %>%
    left_join(
      dat %>%
        filter(.data$election_study_id == .env$study) %>%
        select(all_of(c(scale_map$score_raw, scale_map$score_z))) %>%
        pivot_longer(everything(), names_to = "score", values_to = "value"),
      by = "score"
    ) %>%
    group_by(construct, score, scaling) %>%
    summarise(
      study = .env$study,
      study_label = .env$study_labels[[.env$study]],
      n_total = n(),
      n_valid = sum(!is.na(.data$value)),
      n_missing = sum(is.na(.data$value)),
      mean = mean(.data$value, na.rm = TRUE),
      sd = sd(.data$value, na.rm = TRUE),
      min = min(.data$value, na.rm = TRUE),
      max = max(.data$value, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    select(study, study_label, construct, score, scaling,
           n_total, n_valid, n_missing, mean, sd, min, max)
}

item_descriptives <- bind_rows(
  lapply(names(study_models), function(study) {
    make_item_descriptives(CSES5, study_models[[study]], study)
  })
)

scale_descriptives <- bind_rows(
  lapply(names(study_models), function(study) {
    make_scale_descriptives(CSES5_operationalised, study_models[[study]], study)
  })
)

write_csv(item_descriptives, PATH_ITEM_DESC)
write_csv(scale_descriptives, PATH_SCALE_DESC)
cat("Item-Deskription gespeichert:  ", PATH_ITEM_DESC, "\n")
cat("Skalen-Deskription gespeichert:", PATH_SCALE_DESC, "\n")

###############################################################################
## 7) HISTOGRAMME
###############################################################################

cat("\n============================================================\n")
cat("Erstelle Histogramme...\n")

# Score-Konfiguration je Land
plot_config <- list(
  AUT_2017 = tibble(
    score_z = c("EXCL_score_z", "Anti_Elitist_score_z", "Minimal_PRG_score_z"),
    label   = c("Exklusionistische Demokratie (z)",
                "Anti-elitistische Demokratie (z)",
                "Minimal-pragmatische Demokratie (z)")
  ),
  DEU_2021 = tibble(
    score_z = c("EXCL_score_z", "POPU_score_z", "PRG_score_z"),
    label   = c("Exklusionistische Demokratie (z)",
                "Populistische Demokratie (z)",
                "Pragmatische Demokratie (z)")
  ),
  GRC_2019 = tibble(
    score_z = c("EXCL_score_z", "POPU_score_z", "PRG_score_z"),
    label   = c("Exklusionistische Demokratie (z)",
                "Populistische Demokratie (z)",
                "Pragmatische Demokratie (z)")
  )
)

# ----------------------------------------------------------
# 7a) Histogramme der standardisierten Skalenwerte
# ----------------------------------------------------------
plot_scores <- function(dat, config, study) {

  d_long <- dat %>%
    filter(election_study_id == study) %>%
    select(all_of(config$score_z)) %>%
    pivot_longer(everything(),
                 names_to  = "score",
                 values_to = "value") %>%
    drop_na(value) %>%                          # FIX: NAs entfernen
    left_join(config %>% rename(score = score_z), by = "score") %>%
    mutate(label = factor(label, levels = config$label))

  ggplot(d_long, aes(x = value)) +
    geom_histogram(bins = 40, fill = "grey30",
                   color = "white", linewidth = 0.2) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "grey60") +
    facet_wrap(~ label, ncol = 3, scales = "free_y") +
    labs(
      title    = paste0(study_labels[study],
                        " - Verteilung der standardisierten Skalenwerte"),
      subtitle = "z-standardisierte Faktorwerte",
      x        = "Standardisierter Skalenwert (z)",
      y        = "Anzahl der Befragten",
      caption  = "Quelle: CSES Modul 5; eigene Berechnung."
    ) +
    theme_minimal(base_size = 14) +
    theme(
      plot.title       = element_text(face = "bold", size = 14),
      plot.subtitle    = element_text(size = 12, color = "grey40"),
      strip.text       = element_text(face = "bold"),
      panel.grid.minor = element_blank()
    )
}

# ----------------------------------------------------------
# 7b) Item-Histogramme
# ----------------------------------------------------------
plot_items <- function(dat, model, study) {

  items_excl <- model$EXCL$items
  items_popu <- model$POPU$items
  items_prg  <- model$PRG$items

  all_item_vars <- c(items_excl, items_popu, items_prg)

  short_excl <- paste0("EXCL", seq_along(items_excl))
  short_popu <- paste0("POPU", seq_along(items_popu))
  short_prg  <- paste0("PRG",  seq_along(items_prg))
  all_shorts <- c(short_excl, short_popu, short_prg)

  item_short_map <- tibble(
    var   = all_item_vars,
    short = all_shorts,
    block = c(rep(model$EXCL$label, length(items_excl)),
              rep(model$POPU$label, length(items_popu)),
              rep(model$PRG$label,  length(items_prg)))
  )

  d_long <- dat %>%
    filter(election_study_id == study) %>%
    select(any_of(all_item_vars)) %>%
    pivot_longer(everything(),
                 names_to  = "var",
                 values_to = "value") %>%
    mutate(
      value = suppressWarnings(as.integer(as.character(value))),
      value = ifelse(value %in% 1:5, value, NA_integer_),
      value = factor(value, levels = 1:5)
    ) %>%
    left_join(item_short_map, by = "var") %>%
    mutate(
      short = factor(short, levels = all_shorts),
      block = factor(block, levels = c(model$EXCL$label,
                                       model$POPU$label,
                                       model$PRG$label))
    ) %>%
    filter(!is.na(short))

  ggplot(d_long, aes(x = value)) +
    geom_bar(na.rm = TRUE, fill = "grey30") +
    ggh4x::facet_nested(
      ~ block + short,
      scales    = "fixed",
      nest_line = element_line(linewidth = 0.5)
    ) +
    scale_x_discrete(drop = FALSE) +
    labs(
      title    = paste0(study_labels[study],
                        " - Itemverteilungen nach Konstrukt"),
      subtitle = paste0(model$EXCL$label, "  |  ",
                        model$POPU$label, "  |  ",
                        model$PRG$label),
      x = "Antwortskala (1-5)",
      y = "Anzahl der Antworten",
      caption = "Hinweis: Die Itemkürzel entsprechen der Reihenfolge im jeweiligen Messmodell. Quelle: CSES Modul 5; eigene Berechnung."
    ) +
    theme_minimal(base_size = 14) +
    theme(
      plot.title        = element_text(face = "bold"),
      plot.subtitle     = element_text(size = 12, color = "grey40"),
      strip.text        = element_text(face = "bold", size = 12),
      panel.grid.minor  = element_blank(),
      panel.spacing     = unit(0.3, "lines")
    )
}

# ----------------------------------------------------------
# 7c) Alle postulierten Items je Studie
# ----------------------------------------------------------
plot_all_postulated_items <- function(dat, study) {

  all_item_vars <- c(all_items_excl, all_items_popu, all_items_prg)

  all_item_map <- bind_rows(
    tibble(
      var = all_items_excl,
      construct = "Exklusionistische Demokratie",
      item_short = paste0("EXCL", seq_along(all_items_excl)),
      item_position = seq_along(all_items_excl)
    ),
    tibble(
      var = all_items_popu,
      construct = "Populistische Demokratie",
      item_short = paste0("POPU", seq_along(all_items_popu)),
      item_position = seq_along(all_items_popu)
    ),
    tibble(
      var = all_items_prg,
      construct = "Pragmatische Demokratie",
      item_short = paste0("PRG", seq_along(all_items_prg)),
      item_position = seq_along(all_items_prg)
    )
  ) %>%
    left_join(item_meta, by = "var") %>%
    mutate(
      construct = factor(
        .data$construct,
        levels = c(
          "Exklusionistische Demokratie",
          "Populistische Demokratie",
          "Pragmatische Demokratie"
        )
      ),
      item_position = factor(.data$item_position, levels = 1:5)
    )

  d_long <- dat %>%
    filter(.data$election_study_id == .env$study) %>%
    select(any_of(all_item_vars)) %>%
    pivot_longer(everything(),
                 names_to = "var",
                 values_to = "value") %>%
    mutate(
      value = suppressWarnings(as.integer(as.character(.data$value))),
      value = ifelse(.data$value %in% 1:5,
                     as.character(.data$value),
                     "NA"),
      value = factor(.data$value, levels = c("1", "2", "3", "4", "5", "NA"))
    ) %>%
    left_join(all_item_map, by = "var") %>%
    filter(!is.na(.data$item_position))

  panel_item_labels <- all_item_map %>%
    transmute(
      construct = .data$construct,
      item_position = .data$item_position,
      value = factor("3", levels = c("1", "2", "3", "4", "5", "NA")),
      y = Inf,
      item_label = stringr::str_wrap(.data$var, width = 20)
    )

  ggplot(d_long, aes(x = value)) +
    geom_bar(fill = "grey30") +
    geom_text(
      data = panel_item_labels,
      aes(x = value, y = y, label = item_label),
      inherit.aes = FALSE,
      vjust = 1.2,
      lineheight = 0.95,
      size = 3.0,
      color = "grey20"
    ) +
    facet_grid(
      rows = vars(construct),
      cols = vars(item_position),
      scales = "fixed",
      drop = FALSE
    ) +
    scale_x_discrete(drop = FALSE) +
    labs(
      title = paste0(study_labels[study], ""),
      subtitle = "",
      x = "Antwortskala (1-5)",
      y = "Anzahl der Antworten",
      caption = "Hinweis: Die Panelbeschriftungen zeigen die eigens verwendeten Namen der Items. Quelle: CSES Modul 5; eigene Berechnung."
    ) +
    theme_minimal(base_size = 15) +
    theme(
      plot.title = element_text(face = "bold", size = 20),
      plot.subtitle = element_text(size = 14, color = "grey40"),
      axis.title = element_text(size = 15),
      axis.text = element_text(size = 13),
      strip.text.x = element_text(face = "bold", size = 15),
      strip.text.y = element_text(face = "bold", size = 9),
      plot.caption = element_text(size = 11, hjust = 1),
      panel.grid.minor = element_blank(),
      panel.spacing.x = unit(0.35, "lines"),
      panel.spacing.y = unit(0.2, "lines"),
      plot.margin = margin(8, 12, 8, 8)
    )
}

# ----------------------------------------------------------
# 7d) Rohwerte und z-standardisierte Faktorwerte im Vergleich
# ----------------------------------------------------------
plot_raw_vs_z <- function(dat, model, study) {

  construct_map <- tibble(
    construct = c(model$EXCL$label, model$POPU$label, model$PRG$label),
    raw_score = c(model$EXCL$score_name,
                  model$POPU$score_name,
                  model$PRG$score_name),
    z_score   = paste0(raw_score, "_z")
  )

  score_map <- bind_rows(
    construct_map %>%
      transmute(score = raw_score,
                construct = construct,
                scaling = "Faktorisiert"),
    construct_map %>%
      transmute(score = z_score,
                construct = construct,
                scaling = "z-standardisiert")
  )

  d_long <- dat %>%
    filter(election_study_id == study) %>%
    select(all_of(score_map$score)) %>%
    pivot_longer(everything(),
                 names_to = "score",
                 values_to = "value") %>%
    drop_na(value) %>%
    left_join(score_map, by = "score") %>%
    mutate(
      construct = factor(construct, levels = construct_map$construct),
      scaling = factor(scaling, levels = c("Faktorisiert", "z-standardisiert"))
    )

  ggplot(d_long, aes(x = value)) +
    geom_histogram(bins = 40, fill = "grey30",
                   color = "white", linewidth = 0.2) +
    geom_vline(data = d_long %>% filter(scaling == "z-standardisiert"),
               xintercept = 0, linetype = "dashed", color = "grey60") +
    facet_grid(construct ~ scaling, scales = "free") +
    labs(
      title    = paste0(study_labels[study],
                        " - faktorisierte und standardisierte Skalenwerte im Vergleich"),
      subtitle = "Die z-Standardisierung verschiebt die Verteilung auf Mittelwert 0 und skaliert sie auf Standardabweichung 1; die Verteilungsform bleibt erhalten.",
      x        = "Wert",
      y        = "Anzahl der Befragten",
      caption  = "Quelle: CSES Modul 5; eigene Berechnung."
    ) +
    theme_minimal(base_size = 12) +
    theme(
      plot.title       = element_text(face = "bold", size = 13),
      plot.subtitle    = element_text(size = 9.5, color = "grey40"),
      strip.text       = element_text(face = "bold"),
      panel.grid.minor = element_blank(),
      panel.spacing    = unit(0.7, "lines")
    )
}

# ----------------------------------------------------------
# 7e) Plots speichern
# ----------------------------------------------------------

DIR_HIST_RAWZ <- file.path(DIR_HIST, "raw_vs_z_png")
dir.create(DIR_HIST_RAWZ, recursive = TRUE, showWarnings = FALSE)

pdf_scores <- file.path(DIR_HIST_PDF, "hist_scores_ALL_20262301.pdf")
pdf_items  <- file.path(DIR_HIST_PDF, "hist_items_ALL_20262301.pdf")
pdf_all_items <- file.path(DIR_HIST_PDF, "hist_all_postulated_items_ALL_20262301.pdf")
pdf_rawz   <- file.path(DIR_HIST_PDF, "hist_raw_vs_z_ALL_20262301.pdf")

# --- Score PNGs ---
score_plots <- list()
for (study in names(study_models)) {
  p <- plot_scores(CSES5_operationalised, plot_config[[study]], study)
  ggsave(
    file.path(DIR_HIST_SCORES, paste0("hist_scores_", study, ".png")),
    p, width = 12, height = 5, dpi = 300
  )
  score_plots[[study]] <- p
  cat("PNG gespeichert: hist_scores_", study, ".png\n", sep = "")
}

# --- Score PDF ---
pdf(pdf_scores, width = 12, height = 5)
for (study in names(study_models)) print(score_plots[[study]])
dev.off()
cat("PDF gespeichert:", pdf_scores, "\n")

# --- Item PNGs ohne ausformulierte Itemlegende ---
item_plots <- list()
for (study in names(study_models)) {
  p <- plot_items(CSES5_operationalised, study_models[[study]], study)
  ggsave(
    file.path(DIR_HIST_ITEMS, paste0("hist_items_", study, ".png")),
    p, width = 13.5, height = 5.8, dpi = 300
  )
  item_plots[[study]] <- p
  cat("PNG gespeichert: hist_items_", study, ".png\n", sep = "")
}

# --- Item PDF ---
pdf(pdf_items, width = 13.5, height = 5.8)
for (study in names(study_models)) print(item_plots[[study]])
dev.off()
cat("PDF gespeichert:", pdf_items, "\n")

# --- Alle postulierten Items PNGs ---
all_item_plots <- list()
for (study in names(study_models)) {
  p <- plot_all_postulated_items(CSES5, study)
  ggsave(
    file.path(DIR_HIST_ALL_ITEMS, paste0("hist_all_postulated_items_", study, ".png")),
    p, width = 10.8, height = 7.4, dpi = 300
  )
  all_item_plots[[study]] <- p
  cat("PNG gespeichert: hist_all_postulated_items_", study, ".png\n", sep = "")
}

# --- Alle postulierten Items PDF ---
pdf(pdf_all_items, width = 10.8, height = 7.4)
for (study in names(study_models)) print(all_item_plots[[study]])
dev.off()
cat("PDF gespeichert:", pdf_all_items, "\n")

# --- Rohwert/z-Vergleich PNGs ---
rawz_plots <- list()
for (study in names(study_models)) {
  p <- plot_raw_vs_z(CSES5_operationalised, study_models[[study]], study)
  ggsave(
    file.path(DIR_HIST_RAWZ, paste0("hist_raw_vs_z_", study, ".png")),
    p, width = 12, height = 8, dpi = 300
  )
  rawz_plots[[study]] <- p
  cat("PNG gespeichert: hist_raw_vs_z_", study, ".png\n", sep = "")
}

# --- Rohwert/z-Vergleich PDF ---
pdf(pdf_rawz, width = 12, height = 8)
for (study in names(study_models)) print(rawz_plots[[study]])
dev.off()
cat("PDF gespeichert:", pdf_rawz, "\n")

###############################################################################
## 8) ZUSAMMENFASSUNG SPEICHERN UND ABSCHLUSS
###############################################################################

summary_table <- bind_rows(summary_list)
write_csv(summary_table, PATH_SUMMARY)

cat("\n============================================================\n")
cat("Fertig.\n\n")
cat("Datendatei:     nicht gespeichert; dieses Skript schreibt nur Tabellen und Plots\n")
cat("Summary CSV:    ", PATH_SUMMARY,     "\n")
cat("Items CSV:      ", PATH_ITEM_DESC,   "\n")
cat("Skalen CSV:     ", PATH_SCALE_DESC,  "\n\n")
cat("Scores PNG:     ", DIR_HIST_SCORES,  "\n")
cat("Items PNG:      ", DIR_HIST_ITEMS,   "\n")
cat("Alle Items PNG: ", DIR_HIST_ALL_ITEMS, "\n")
cat("Raw/z PNG:      ", DIR_HIST_RAWZ,    "\n")
cat("PDFs:           ", DIR_HIST_PDF,     "\n\n")
cat("Score-Variablen:\n")
cat("  AUT_2017: EXCL_score(_z), Anti_Elitist_score(_z), Minimal_PRG_score(_z)\n")
cat("  DEU_2021: EXCL_score(_z), POPU_score(_z), PRG_score(_z)\n")
cat("  GRC_2019: EXCL_score(_z), POPU_score(_z), PRG_score(_z)\n")
cat("============================================================\n")
