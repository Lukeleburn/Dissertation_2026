###############################################################################
## DISS_2026 -- 02_B
## H1.2-Abbildungen und CFA-Robustheit
##
## Zweck:
##   1) Deskriptive Evidenz zu H1.2:
##      Die drei Demokratieperspektiven sollen keine statistischen Randphaenomene
##      sein. Dafuer werden die validierten Itemsets als Rohindizes auf der
##      urspruenglichen Antwortmetrik relativ zur konstruktspezifischen Rohwertmitte sowie als EFA-basierte Faktorwerte dargestellt.
##
##   2) CFA-Robustheit zur Differenzierungsthese:
##      Vergleich des differenzierten Drei-Faktoren-Modells mit einem
##      Zwei-Faktoren-Konkurrenzmodell, in dem EXCL und POPU/Anti-Elitismus
##      auf eine gemeinsame Dimension fallen.
##
## Input:
##   data/processed/cses5/derived/CSES5_20262301_prepared.rds
##   data/processed/cses5/derived/CSES5_20262301_operationalised_scaled_midpoint.rds
##
## Output:
##   outputs/reli_vali_DE_v3_CFA_tables_wide/02_B_robustness_h1/
###############################################################################

suppressPackageStartupMessages({
  library(tidyverse)
  library(psych)
  library(GPArotation)
  library(lavaan)
  library(gt)
  library(here)
})

###############################################################################
## 0) PFADE UND EINSTELLUNGEN
###############################################################################

PATH_IN <- here(
  "data", "processed", "cses5", "derived",
  "CSES5_20262301_prepared.rds"
)

PATH_IN_OPERATIONALISED <- here(
  "data", "processed", "cses5", "derived",
  "CSES5_20262301_operationalised_scaled_midpoint.rds"
)

DIR_BASE <- here(
  "outputs",
  "reli_vali_DE_v3_CFA_tables_wide",
  "02_B_robustness_h1"
)

DIR_H12        <- file.path(DIR_BASE, "h1_2_empirical_relevance")
DIR_H12_PLOTS  <- file.path(DIR_H12, "plots")
DIR_H12_TABLES <- file.path(DIR_H12, "tables")

DIR_CFA        <- file.path(DIR_BASE, "cfa_3f_vs_2f")
DIR_CFA_PLOTS  <- file.path(DIR_CFA, "plots")
DIR_CFA_TABLES <- file.path(DIR_CFA, "tables")

for (d in c(DIR_BASE,
            DIR_H12, DIR_H12_PLOTS, DIR_H12_TABLES,
            DIR_CFA, DIR_CFA_PLOTS, DIR_CFA_TABLES)) {
  dir.create(d, recursive = TRUE, showWarnings = FALSE)
}

expected_studies <- c("AUT_2017", "DEU_2021", "GRC_2019")
factor_names     <- c("EXCL", "POPU", "PRG")

study_labels <- c(
  AUT_2017 = "Oesterreich 2017",
  DEU_2021 = "Deutschland 2021",
  GRC_2019 = "Griechenland 2019"
)

study_labels_de <- c(
  AUT_2017 = "Österreich 2017",
  DEU_2021 = "Deutschland 2021",
  GRC_2019 = "Griechenland 2019"
)

###############################################################################
## 1) DATEN UND ITEMSETS
###############################################################################

if (!file.exists(PATH_IN)) {
  stop("Input RDS nicht gefunden: ", PATH_IN)
}
if (!file.exists(PATH_IN_OPERATIONALISED)) {
  stop("Operationalisierte RDS nicht gefunden: ", PATH_IN_OPERATIONALISED)
}

CSES5 <- readRDS(PATH_IN)
CSES5_op <- readRDS(PATH_IN_OPERATIONALISED)

stopifnot(
  "election_study_id" %in% names(CSES5),
  "respondent_id"     %in% names(CSES5),
  "election_study_id" %in% names(CSES5_op),
  "respondent_id"     %in% names(CSES5_op)
)

missing_studies <- setdiff(
  expected_studies,
  unique(CSES5$election_study_id)
)

if (length(missing_studies) > 0) {
  stop("Fehlende Studien: ", paste(missing_studies, collapse = ", "))
}

CSES5_sub <- CSES5 %>%
  filter(election_study_id %in% expected_studies)

CSES5_op_sub <- CSES5_op %>%
  filter(election_study_id %in% expected_studies)

cat("N je Studie:\n")
print(table(CSES5_sub$election_study_id))

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


## Theoretische Skalenmitten der Rohitems.
## Fuenfstufige Items haben die Rohwertmitte 3.0; vierstufige Items die
## Rohwertmitte 2.5. Die Konstruktmitte ergibt sich als Mittelwert der
## Itemmitten des jeweiligen fallbezogenen Itemsets.
four_point_items <- c(
  "follow_customs_rev",
  "born_country_rev",
  "corruption_widespread_rev"
)

item_raw_midpoint <- function(items) {
  ifelse(items %in% four_point_items, 2.5, 3.0)
}

construct_raw_midpoints <- purrr::imap_dfr(
  study_models,
  function(sm, study) {
    purrr::map_dfr(
      factor_names,
      function(construct_key) {
        items <- sm[[construct_key]]$items
        tibble(
          study = study,
          construct_key = construct_key,
          raw_midpoint = mean(item_raw_midpoint(items)),
          n_items = length(items),
          n_items_4pt = sum(items %in% four_point_items),
          n_items_5pt = sum(!(items %in% four_point_items))
        )
      }
    )
  }
)

###############################################################################
## 2) HILFSFUNKTIONEN
###############################################################################

build_target <- function(model) {
  n_excl  <- length(model$EXCL$items)
  n_popu  <- length(model$POPU$items)
  n_prg   <- length(model$PRG$items)
  n_total <- n_excl + n_popu + n_prg

  target <- matrix(0, nrow = n_total, ncol = 3)
  target[seq_len(n_excl), 1] <- 1
  target[(n_excl + 1):(n_excl + n_popu), 2] <- 1
  target[(n_excl + n_popu + 1):n_total, 3] <- 1
  target
}

align_factors <- function(loadings_raw, target_matrix, factor_names) {
  cong <- psych::factor.congruence(loadings_raw, target_matrix)
  order_idx <- apply(abs(cong), 2, which.max)

  if (length(unique(order_idx)) < length(order_idx)) {
    warning("Faktor-Zuordnung nicht eindeutig.")
  }

  loadings_ordered <- loadings_raw[, order_idx, drop = FALSE]
  colnames(loadings_ordered) <- factor_names

  for (j in seq_along(factor_names)) {
    target_items <- which(target_matrix[, j] == 1)
    if (mean(loadings_ordered[target_items, j], na.rm = TRUE) < 0) {
      loadings_ordered[, j] <- loadings_ordered[, j] * -1
    }
  }

  list(loadings = loadings_ordered, order_idx = order_idx)
}

calc_factor_scores <- function(dat_items, target_matrix, factor_names) {
  fa_unrot <- psych::fa(
    dat_items,
    nfactors = length(factor_names),
    rotate   = "none",
    fm       = "minres"
  )

  rotated <- GPArotation::GPFoblq(
    A          = fa_unrot$loadings,
    method     = "targetQ",
    methodArgs = list(Target = target_matrix)
  )

  load_raw <- rotated$loadings
  rownames(load_raw) <- names(dat_items)

  aligned <- align_factors(
    load_raw,
    target_matrix,
    factor_names
  )

  load_aligned <- aligned$loadings

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

  list(
    scores_raw = scores_raw,
    scores_z   = scores_z,
    loadings   = load_aligned
  )
}

build_cfa_3f <- function(study, add_residuals = TRUE) {
  sm <- study_models[[study]]

  base <- sprintf(
    "EXCL =~ %s\nPOPU =~ %s\nPRG  =~ %s",
    paste(sm$EXCL$items, collapse = " + "),
    paste(sm$POPU$items, collapse = " + "),
    paste(sm$PRG$items,  collapse = " + ")
  )

  if (!add_residuals) return(base)

  paste(
    base,
    build_residual_syntax(sm),
    sep = "\n"
  )
}

build_cfa_2f_excl_popu <- function(study, add_residuals = TRUE) {
  sm <- study_models[[study]]

  excl_popu_items <- c(sm$EXCL$items, sm$POPU$items)

  base <- sprintf(
    "EXCL_POPU =~ %s\nPRG =~ %s",
    paste(excl_popu_items, collapse = " + "),
    paste(sm$PRG$items, collapse = " + ")
  )

  if (!add_residuals) return(base)

  paste(
    base,
    build_residual_syntax(sm),
    sep = "\n"
  )
}

build_residual_syntax <- function(sm) {
  res <- c()

  excl <- sm$EXCL$items
  if (all(c("born_country_rev", "follow_customs_rev") %in% excl)) {
    res <- c(res, "born_country_rev ~~ follow_customs_rev")
  }
  if (all(c("born_country_rev", "minorities_adapt_rev") %in% excl)) {
    res <- c(res, "born_country_rev ~~ minorities_adapt_rev")
  }
  if (all(c("follow_customs_rev", "minorities_adapt_rev") %in% excl)) {
    res <- c(res, "follow_customs_rev ~~ minorities_adapt_rev")
  }

  popu <- sm$POPU$items
  if (all(c("pol_no_care_rev", "pol_for_rich_rev") %in% popu)) {
    res <- c(res, "pol_no_care_rev ~~ pol_for_rich_rev")
  }
  if (all(c("pol_no_care_rev", "pol_main_problem_rev") %in% popu)) {
    res <- c(res, "pol_no_care_rev ~~ pol_main_problem_rev")
  }

  prg <- sm$PRG$items
  if (all(c("voting_makes_difference", "power_makes_difference") %in% prg)) {
    res <- c(res, "voting_makes_difference ~~ power_makes_difference")
  }

  if (length(res) == 0) {
    return("")
  }

  paste(
    "# Theoretisch begruendete residuale Korrelationen",
    paste(res, collapse = "\n"),
    sep = "\n"
  )
}

extract_fit <- function(fit, study, model_label, n_obs) {
  idx_names <- c(
    "chisq", "df", "cfi", "tli",
    "rmsea", "rmsea.ci.lower",
    "rmsea.ci.upper", "srmr", "aic", "bic"
  )

  idx <- lavaan::fitMeasures(fit, idx_names)

  tibble(
    study       = study,
    study_label = study_labels_de[study],
    model       = model_label,
    n           = n_obs,
    chisq       = round(idx["chisq"], 2),
    df          = as.integer(idx["df"]),
    cfi         = round(idx["cfi"], 3),
    tli         = round(idx["tli"], 3),
    rmsea       = round(idx["rmsea"], 3),
    rmsea_lo    = round(idx["rmsea.ci.lower"], 3),
    rmsea_hi    = round(idx["rmsea.ci.upper"], 3),
    srmr        = round(idx["srmr"], 3),
    aic         = round(idx["aic"], 1),
    bic         = round(idx["bic"], 1)
  )
}

save_gt_png <- function(tbl, path, expand = 15) {
  gt::gtsave(tbl, filename = path, expand = expand)
  cat("Gespeichert:", path, "\n")
}

###############################################################################
## 3) H1.2: ROHINDIZES, ZENTRALE HAUPTOPERATIONALISIERUNG UND FAKTORWERTE
###############################################################################

cat("\n============================================================\n")
cat("H1.2: Rohindizes, zentrale skalenmittige Scores und Faktorwerte\n")
cat("============================================================\n")

raw_index_list <- list()
factor_score_list <- list()
scaled_score_list <- list()

for (study in expected_studies) {
  cat("\n---", study, "---\n")

  sm <- study_models[[study]]

  items_study <- c(
    sm$EXCL$items,
    sm$POPU$items,
    sm$PRG$items
  )

  ## Rohindizes werden weiterhin deskriptiv aus den rekodierten Originalitems
  ## gebildet. Sie dienen der Kontrolle auf der urspruenglichen Antwortmetrik.
  dat_study <- CSES5_sub %>%
    filter(election_study_id == study) %>%
    select(respondent_id, all_of(items_study)) %>%
    drop_na()

  dat_items <- dat_study %>%
    select(all_of(items_study))

  raw_index_list[[study]] <- dat_study %>%
    transmute(
      study       = study,
      study_label = study_labels_de[study],
      respondent_id,
      EXCL = rowMeans(across(all_of(sm$EXCL$items)), na.rm = FALSE),
      POPU = rowMeans(across(all_of(sm$POPU$items)), na.rm = FALSE),
      PRG  = rowMeans(across(all_of(sm$PRG$items)),  na.rm = FALSE)
    ) %>%
    pivot_longer(
      cols      = all_of(factor_names),
      names_to  = "construct_key",
      values_to = "raw_index"
    ) %>%
    mutate(
      construct = recode(
        construct_key,
        EXCL = sm$EXCL$label,
        POPU = sm$POPU$label,
        PRG  = sm$PRG$label
      )
    )

  ## Die Hauptoperationalisierung wird NICHT mehr lokal rekonstruiert.
  ## Stattdessen werden die im zentralen Operationalisierungsskript erzeugten
  ## itemweise skalenmittig transformierten Scores eingelesen.
  score_cols <- c(
    EXCL = paste0(sm$EXCL$score_name, "_scaled_midpoint"),
    POPU = paste0(sm$POPU$score_name, "_scaled_midpoint"),
    PRG  = paste0(sm$PRG$score_name,  "_scaled_midpoint")
  )

  missing_score_cols <- setdiff(unname(score_cols), names(CSES5_op_sub))
  if (length(missing_score_cols) > 0) {
    stop(
      "Folgende zentral operationalisierte Score-Spalten fehlen fuer ", study, ": ",
      paste(missing_score_cols, collapse = ", ")
    )
  }

  scaled_score_list[[study]] <- CSES5_op_sub %>%
    filter(election_study_id == study) %>%
    select(
      respondent_id,
      EXCL = all_of(score_cols[["EXCL"]]),
      POPU = all_of(score_cols[["POPU"]]),
      PRG  = all_of(score_cols[["PRG"]])
    ) %>%
    mutate(
      study       = study,
      study_label = study_labels_de[study]
    ) %>%
    pivot_longer(
      cols      = all_of(factor_names),
      names_to  = "construct_key",
      values_to = "scaled_midpoint"
    ) %>%
    mutate(
      construct = recode(
        construct_key,
        EXCL = sm$EXCL$label,
        POPU = sm$POPU$label,
        PRG  = sm$PRG$label
      )
    )

  target <- build_target(sm)
  fs <- calc_factor_scores(dat_items, target, factor_names)

  factor_score_list[[study]] <- bind_cols(
    dat_study %>% select(respondent_id),
    fs$scores_z
  ) %>%
    mutate(
      study       = study,
      study_label = study_labels_de[study]
    ) %>%
    pivot_longer(
      cols      = ends_with("_z"),
      names_to  = "construct_key",
      values_to = "score_z"
    ) %>%
    mutate(
      construct_key = str_remove(construct_key, "_z$"),
      construct = recode(
        construct_key,
        EXCL = sm$EXCL$label,
        POPU = sm$POPU$label,
        PRG  = sm$PRG$label
      )
    )
}

add_plot_vars <- function(dat) {
  dat %>%
    mutate(
      study_label_marked = if_else(study == "AUT_2017",
                                   paste0(study_label, "*"),
                                   study_label),
      study_label_marked = factor(
        study_label_marked,
        levels = c("Österreich 2017*",
                   "Deutschland 2021",
                   "Griechenland 2019")
      ),
      perspective = recode(
        construct_key,
        EXCL = "Exklusionistische Perspektive",
        POPU = "Populistische Perspektive",
        PRG  = "Pragmatische Perspektive"
      ),
      perspective = factor(
        perspective,
        levels = c("Exklusionistische Perspektive",
                   "Populistische Perspektive",
                   "Pragmatische Perspektive")
      ),
      construct = factor(construct)
    )
}

raw_indices <- bind_rows(raw_index_list) %>%
  add_plot_vars()

scaled_scores <- bind_rows(scaled_score_list) %>%
  add_plot_vars()

factor_scores <- bind_rows(factor_score_list) %>%
  add_plot_vars()

raw_scaled_join <- raw_indices %>%
  select(study, study_label, study_label_marked, respondent_id,
         construct_key, construct, perspective, raw_index) %>%
  left_join(
    scaled_scores %>%
      select(study, respondent_id, construct_key, scaled_midpoint),
    by = c("study", "respondent_id", "construct_key")
  )

h12_summary <- raw_indices %>%
  left_join(
    construct_raw_midpoints,
    by = c("study", "construct_key")
  ) %>%
  group_by(study, study_label, construct_key, construct) %>%
  summarise(
    n                         = sum(!is.na(raw_index)),
    mean_raw                  = mean(raw_index, na.rm = TRUE),
    sd_raw                    = sd(raw_index, na.rm = TRUE),
    median_raw                = median(raw_index, na.rm = TRUE),
    raw_midpoint              = dplyr::first(raw_midpoint),
    n_items                   = dplyr::first(n_items),
    n_items_4pt               = dplyr::first(n_items_4pt),
    n_items_5pt               = dplyr::first(n_items_5pt),
    share_above_midpoint      = mean(raw_index > raw_midpoint, na.rm = TRUE),
    share_at_or_above_midpoint = mean(raw_index >= raw_midpoint, na.rm = TRUE),
    share_below_midpoint      = mean(raw_index < raw_midpoint, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    across(
      c(mean_raw, sd_raw, median_raw, raw_midpoint,
        share_above_midpoint,
        share_at_or_above_midpoint,
        share_below_midpoint),
      ~ round(.x, 3)
    )
  )

h12_scaled_summary <- scaled_scores %>%
  group_by(study, study_label, construct_key, construct) %>%
  summarise(
    n              = sum(!is.na(scaled_midpoint)),
    mean_scaled    = mean(scaled_midpoint, na.rm = TRUE),
    sd_scaled      = sd(scaled_midpoint, na.rm = TRUE),
    median_scaled  = median(scaled_midpoint, na.rm = TRUE),
    q10_scaled     = quantile(scaled_midpoint, 0.10, na.rm = TRUE),
    q25_scaled     = quantile(scaled_midpoint, 0.25, na.rm = TRUE),
    q75_scaled     = quantile(scaled_midpoint, 0.75, na.rm = TRUE),
    q90_scaled     = quantile(scaled_midpoint, 0.90, na.rm = TRUE),
    share_above_0  = mean(scaled_midpoint > 0, na.rm = TRUE),
    share_below_0  = mean(scaled_midpoint < 0, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    across(
      c(mean_scaled, sd_scaled, median_scaled,
        q10_scaled, q25_scaled, q75_scaled, q90_scaled,
        share_above_0, share_below_0),
      ~ round(.x, 3)
    )
  )

write_csv(
  h12_summary,
  file.path(DIR_H12_TABLES, "h1_2_raw_index_summary.csv")
)

write_csv(
  h12_scaled_summary,
  file.path(DIR_H12_TABLES, "h1_2_scaled_midpoint_summary.csv")
)

write_csv(
  raw_indices,
  file.path(DIR_H12_TABLES, "h1_2_raw_indices_long.csv")
)

write_csv(
  scaled_scores,
  file.path(DIR_H12_TABLES, "h1_2_scaled_midpoint_scores_long.csv")
)

write_csv(
  raw_scaled_join,
  file.path(DIR_H12_TABLES, "h1_2_raw_and_scaled_midpoint_long.csv")
)

write_csv(
  factor_scores,
  file.path(DIR_H12_TABLES, "h1_2_factor_scores_z_long.csv")
)

###############################################################################
## 3a) H1.2 PLOTS
###############################################################################

note_austria <- paste(
  "* Österreich: POPU wird als anti-elitistische Dimension,",
  "PRG als minimal-pragmatische Dimension operationalisiert."
)

make_interval_data <- function(dat, value_var) {
  dat %>%
    group_by(study, study_label, study_label_marked,
             construct_key, construct, perspective) %>%
    summarise(
      n      = sum(!is.na({{ value_var }})),
      mean   = mean({{ value_var }}, na.rm = TRUE),
      median = median({{ value_var }}, na.rm = TRUE),
      q10    = quantile({{ value_var }}, 0.10, na.rm = TRUE),
      q25    = quantile({{ value_var }}, 0.25, na.rm = TRUE),
      q75    = quantile({{ value_var }}, 0.75, na.rm = TRUE),
      q90    = quantile({{ value_var }}, 0.90, na.rm = TRUE),
      .groups = "drop"
    )
}

raw_intervals <- make_interval_data(raw_indices, raw_index)
scaled_intervals <- make_interval_data(scaled_scores, scaled_midpoint)

## Alternative Beschriftungslogik fuer den Hauptplot:
## Die oesterreichische Abweichung wird nicht mehr als Fussnote gefuehrt,
## sondern direkt in den Facettenlabels und mit Sternchen an den betroffenen
## Verteilungslinien markiert.
perspective_inline_levels <- c(
  "Exklusionistische Perspektive",
  "Populistische Perspektive\nanti-elitäre Perspektive*",
  "Pragmatische Perspektive\nminimal-pragmatische Perspektive*"
)

add_inline_austria_labels <- function(dat) {
  dat %>%
    mutate(
      study_label_plain = factor(
        study_label,
        levels = c("Österreich 2017",
                   "Deutschland 2021",
                   "Griechenland 2019")
      ),
      perspective_inline = case_when(
        construct_key == "POPU" ~ "Populistische Perspektive\nanti-elitäre Perspektive*",
        construct_key == "PRG"  ~ "Pragmatische Perspektive\nminimal-pragmatische Perspektive*",
        TRUE ~ "Exklusionistische Perspektive"
      ),
      perspective_inline = factor(
        perspective_inline,
        levels = perspective_inline_levels
      )
    )
}

scaled_intervals_inline <- scaled_intervals %>%
  add_inline_austria_labels()

scaled_star_inline <- scaled_intervals_inline %>%
  filter(study == "AUT_2017", construct_key %in% c("POPU", "PRG")) %>%
  mutate(
    star_y = pmax(q10, -0.98),
    star_label = "*"
  )

p_raw_density <- ggplot(
  raw_indices,
  aes(x = raw_index, fill = perspective, color = perspective)
) +
  geom_density(alpha = 0.20, linewidth = 0.7, na.rm = TRUE) +
  geom_vline(xintercept = 3, linetype = "dashed", color = "grey45") +
  facet_grid(perspective ~ study_label_marked) +
  scale_x_continuous(limits = c(1, 5), breaks = 1:5) +
  guides(fill = "none", color = "none") +
  labs(
    title = "Empirische Verteilung der validierten Demokratieperspektiven",
    subtitle = "Dichtekurven der Rohindizes auf der ursprünglichen Antwortskala; gestrichelte Linie = Skalenmitte",
    caption = note_austria,
    x = "Rohindex (1-5)",
    y = "Dichte"
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
  filename = file.path(DIR_H12_PLOTS, "h1_2_raw_index_distributions_common_facets.png"),
  plot     = p_raw_density,
  width    = 13,
  height   = 7.2,
  dpi      = 320
)

p_raw_interval <- ggplot(
  raw_intervals,
  aes(x = study_label_marked, color = perspective)
) +
  geom_hline(yintercept = 3, linetype = "dashed", color = "grey45") +
  geom_linerange(aes(ymin = q10, ymax = q90), linewidth = 0.8, alpha = 0.55) +
  geom_linerange(aes(ymin = q25, ymax = q75), linewidth = 2.0, alpha = 0.90) +
  geom_point(aes(y = mean), color = "grey10", size = 2.8) +
  geom_point(aes(y = median), shape = 95, color = "grey10", size = 8) +
  facet_wrap(~ perspective, nrow = 1) +
  scale_y_continuous(limits = c(1, 5), breaks = 1:5) +
  guides(color = "none") +
  labs(
    title = "Verteilungsbreite der validierten Demokratieperspektiven",
    subtitle = "Punkt = Mittelwert; Querstrich = Median; dicke Linie = Interquartilsabstand; dünne Linie = 10.–90. Perzentil",
    caption = note_austria,
    x = NULL,
    y = "Rohindex (1-5)"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 10, color = "grey35"),
    plot.caption = element_text(size = 9, color = "grey35", hjust = 0),
    axis.text.x = element_text(angle = 0, hjust = 0.5),
    strip.text = element_text(face = "bold"),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank()
  )

ggsave(
  filename = file.path(DIR_H12_PLOTS, "h1_2_raw_index_intervalplot.png"),
  plot     = p_raw_interval,
  width    = 13,
  height   = 5.8,
  dpi      = 320
)
## Rueckwaertskompatibler Dateiname fuer bestehende Quarto-Referenzen.
ggsave(
  filename = file.path(DIR_H12_PLOTS, "h1_2_raw_index_dotplot.png"),
  plot     = p_raw_interval,
  width    = 13,
  height   = 5.8,
  dpi      = 320
)

p_scaled_density <- ggplot(
  scaled_scores,
  aes(x = scaled_midpoint, fill = perspective, color = perspective)
) +
  geom_density(alpha = 0.20, linewidth = 0.7, na.rm = TRUE) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey45") +
  facet_grid(perspective ~ study_label_marked) +
  scale_x_continuous(limits = c(-1, 1), breaks = seq(-1, 1, 0.5)) +
  guides(fill = "none", color = "none") +
  labs(
    title = "Verteilung der Hauptoperationalisierung",
    subtitle = "Zentrale itemweise skalenmittige Scores; 0 = theoretische Skalenmitte",
    caption = note_austria,
    x = "Konstruktreichweite [-1, +1]",
    y = "Dichte"
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
  filename = file.path(DIR_H12_PLOTS, "h1_2_scaled_midpoint_distributions_common_facets.png"),
  plot     = p_scaled_density,
  width    = 13,
  height   = 7.2,
  dpi      = 320
)

p_scaled_interval <- ggplot(
  scaled_intervals,
  aes(x = study_label_marked, color = perspective)
) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey45") +
  geom_linerange(aes(ymin = q10, ymax = q90), linewidth = 0.8, alpha = 0.55) +
  geom_linerange(aes(ymin = q25, ymax = q75), linewidth = 2.0, alpha = 0.90) +
  geom_point(aes(y = mean), color = "grey10", size = 2.8) +
  geom_point(aes(y = median), shape = 95, color = "grey10", size = 8) +
  facet_wrap(~ perspective, nrow = 1) +
  scale_y_continuous(limits = c(-1, 1), breaks = seq(-1, 1, 0.5)) +
  guides(color = "none") +
  labs(
    title = "Verteilungsbreite der Hauptoperationalisierung",
    subtitle = "Punkt = Mittelwert; Querstrich = Median; dicke Linie = Interquartilsabstand; dünne Linie = 10.–90. Perzentil",
    caption = note_austria,
    x = NULL,
    y = "Konstruktreichweite [-1, +1]"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 10, color = "grey35"),
    plot.caption = element_text(size = 9, color = "grey35", hjust = 0),
    axis.text.x = element_text(angle = 0, hjust = 0.5),
    strip.text = element_text(face = "bold"),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank()
  )

ggsave(
  filename = file.path(DIR_H12_PLOTS, "h1_2_scaled_midpoint_intervalplot.png"),
  plot     = p_scaled_interval,
  width    = 13,
  height   = 5.8,
  dpi      = 320
)
## Rueckwaertskompatibler Dateiname fuer bestehende Quarto-Referenzen.
ggsave(
  filename = file.path(DIR_H12_PLOTS, "h1_2_scaled_midpoint_dotplot.png"),
  plot     = p_scaled_interval,
  width    = 13,
  height   = 5.8,
  dpi      = 320
)

## Zusatzplot: gleiche Information, aber mit direkter Beschriftung der
## oesterreichischen Sonderoperationalisierung in den Facettenlabels.
p_scaled_interval_inline <- ggplot(
  scaled_intervals_inline,
  aes(x = study_label_plain, color = perspective_inline)
) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey45") +
  geom_linerange(aes(ymin = q10, ymax = q90), linewidth = 0.8, alpha = 0.55) +
  geom_linerange(aes(ymin = q25, ymax = q75), linewidth = 2.0, alpha = 0.90) +
  geom_point(aes(y = mean), color = "grey10", size = 2.8) +
  geom_point(aes(y = median), shape = 95, color = "grey10", size = 8) +
  geom_text(
    data = scaled_star_inline,
    aes(x = study_label_plain, y = star_y, label = star_label),
    inherit.aes = FALSE,
    nudge_x = 0.16,
    vjust = 0.5,
    size = 5,
    fontface = "bold",
    color = "grey10"
  ) +
  facet_wrap(~ perspective_inline, nrow = 1) +
  scale_y_continuous(limits = c(-1, 1), breaks = seq(-1, 1, 0.5)) +
  guides(color = "none") +
  labs(
    title = "Verteilungsbreite der Hauptoperationalisierung",
    subtitle = "Punkt = Mittelwert; Querstrich = Median; dicke Linie = Interquartilsabstand; dünne Linie = 10.–90. Perzentil",
    x = NULL,
    y = "Konstruktreichweite [-1, +1]"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 10, color = "grey35"),
    axis.text.x = element_text(angle = 0, hjust = 0.5),
    strip.text = element_text(face = "bold", lineheight = 0.95),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank()
  )

ggsave(
  filename = file.path(
    DIR_H12_PLOTS,
    "h1_2_scaled_midpoint_intervalplot_inline_austria_labels.png"
  ),
  plot     = p_scaled_interval_inline,
  width    = 13,
  height   = 5.8,
  dpi      = 320
)

p_raw_scaled_relation <- ggplot(
  raw_scaled_join,
  aes(x = raw_index, y = scaled_midpoint, color = perspective)
) +
  geom_point(alpha = 0.18, size = 0.75, na.rm = TRUE) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey45") +
  geom_vline(xintercept = 3, linetype = "dashed", color = "grey45") +
  facet_grid(perspective ~ study_label_marked) +
  scale_x_continuous(limits = c(1, 5), breaks = 1:5) +
  scale_y_continuous(limits = c(-1, 1), breaks = seq(-1, 1, 0.5)) +
  guides(color = "none") +
  labs(
    title = "Rohindex und Hauptoperationalisierung im Verhältnis",
    subtitle = "Die Hauptoperationalisierung stammt aus der zentralen itemweisen skalenmittigen Transformation",
    caption = note_austria,
    x = "Rohindex (1-5)",
    y = "Konstruktreichweite [-1, +1]"
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
  filename = file.path(DIR_H12_PLOTS, "h1_2_raw_index_vs_scaled_midpoint_common_facets.png"),
  plot     = p_raw_scaled_relation,
  width    = 13,
  height   = 7.2,
  dpi      = 320
)

for (study_id in expected_studies) {

  study_caption <- if (study_id == "AUT_2017") note_austria else NULL
  study_title   <- study_labels_de[study_id]

  construct_levels_country <- c(
    study_models[[study_id]]$EXCL$label,
    study_models[[study_id]]$POPU$label,
    study_models[[study_id]]$PRG$label
  )

  raw_country <- raw_indices %>%
    filter(.data$study == .env$study_id) %>%
    mutate(
      construct_country = factor(as.character(construct),
                                 levels = construct_levels_country)
    )

  scaled_country <- scaled_scores %>%
    filter(.data$study == .env$study_id) %>%
    mutate(
      construct_country = factor(as.character(construct),
                                 levels = construct_levels_country)
    )

  raw_scaled_country <- raw_scaled_join %>%
    filter(.data$study == .env$study_id) %>%
    mutate(
      construct_country = factor(as.character(construct),
                                 levels = construct_levels_country)
    )

  raw_intervals_country <- raw_intervals %>%
    filter(.data$study == .env$study_id) %>%
    mutate(
      construct_country = factor(as.character(construct),
                                 levels = construct_levels_country)
    )

  scaled_intervals_country <- scaled_intervals %>%
    filter(.data$study == .env$study_id) %>%
    mutate(
      construct_country = factor(as.character(construct),
                                 levels = construct_levels_country)
    )

  p_raw_density_country <- ggplot(
    raw_country,
    aes(x = raw_index, fill = construct_country, color = construct_country)
  ) +
    geom_density(alpha = 0.20, linewidth = 0.7, na.rm = TRUE) +
    geom_vline(xintercept = 3, linetype = "dashed", color = "grey45") +
    facet_wrap(~ construct_country, nrow = 1) +
    scale_x_continuous(limits = c(1, 5), breaks = 1:5) +
    guides(fill = "none", color = "none") +
    labs(
      title = paste0(
        "Empirische Verteilung der validierten Demokratieperspektiven - ",
        study_title
      ),
      subtitle = "Dichtekurven der Rohindizes auf der ursprünglichen Antwortskala; gestrichelte Linie = Skalenmitte",
      caption = study_caption,
      x = "Rohindex (1-5)",
      y = "Dichte"
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
      DIR_H12_PLOTS,
      paste0("h1_2_raw_index_distributions_", study_id, ".png")
    ),
    plot   = p_raw_density_country,
    width  = 12,
    height = 4.8,
    dpi    = 320
  )

  p_raw_interval_country <- ggplot(
    raw_intervals_country,
    aes(x = construct_country, color = construct_country)
  ) +
    geom_hline(yintercept = 3, linetype = "dashed", color = "grey45") +
    geom_linerange(aes(ymin = q10, ymax = q90), linewidth = 0.8, alpha = 0.55) +
    geom_linerange(aes(ymin = q25, ymax = q75), linewidth = 2.0, alpha = 0.90) +
    geom_point(aes(y = mean), color = "grey10", size = 2.8) +
    geom_point(aes(y = median), shape = 95, color = "grey10", size = 8) +
    scale_y_continuous(limits = c(1, 5), breaks = 1:5) +
    guides(color = "none") +
    labs(
      title = paste0(
        "Verteilungsbreite der validierten Demokratieperspektiven - ",
        study_title
      ),
      subtitle = "Punkt = Mittelwert; Querstrich = Median; dicke Linie = Interquartilsabstand; dünne Linie = 10.–90. Perzentil",
      caption = study_caption,
      x = NULL,
      y = "Rohindex (1-5)"
    ) +
    theme_minimal(base_size = 12) +
    theme(
      plot.title = element_text(face = "bold", size = 14),
      plot.subtitle = element_text(size = 10, color = "grey35"),
      plot.caption = element_text(size = 9, color = "grey35", hjust = 0),
      axis.text.x = element_text(angle = 0, hjust = 0.5),
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_blank()
    )

  ggsave(
    filename = file.path(
      DIR_H12_PLOTS,
      paste0("h1_2_raw_index_intervalplot_", study_id, ".png")
    ),
    plot   = p_raw_interval_country,
    width  = 9,
    height = 5,
    dpi    = 320
  )
  ggsave(
    filename = file.path(
      DIR_H12_PLOTS,
      paste0("h1_2_raw_index_dotplot_", study_id, ".png")
    ),
    plot   = p_raw_interval_country,
    width  = 9,
    height = 5,
    dpi    = 320
  )

  p_scaled_density_country <- ggplot(
    scaled_country,
    aes(x = scaled_midpoint, fill = construct_country, color = construct_country)
  ) +
    geom_density(alpha = 0.20, linewidth = 0.7, na.rm = TRUE) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "grey45") +
    facet_wrap(~ construct_country, nrow = 1) +
    scale_x_continuous(limits = c(-1, 1), breaks = seq(-1, 1, 0.5)) +
    guides(fill = "none", color = "none") +
    labs(
      title = paste0(
        "Verteilung der Hauptoperationalisierung - ",
        study_title
      ),
      subtitle = "Zentrale itemweise skalenmittige Scores; 0 = theoretische Skalenmitte",
      caption = study_caption,
      x = "Konstruktreichweite [-1, +1]",
      y = "Dichte"
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
      DIR_H12_PLOTS,
      paste0("h1_2_scaled_midpoint_distributions_", study_id, ".png")
    ),
    plot   = p_scaled_density_country,
    width  = 12,
    height = 4.8,
    dpi    = 320
  )

  p_scaled_interval_country <- ggplot(
    scaled_intervals_country,
    aes(x = construct_country, color = construct_country)
  ) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "grey45") +
    geom_linerange(aes(ymin = q10, ymax = q90), linewidth = 0.8, alpha = 0.55) +
    geom_linerange(aes(ymin = q25, ymax = q75), linewidth = 2.0, alpha = 0.90) +
    geom_point(aes(y = mean), color = "grey10", size = 2.8) +
    geom_point(aes(y = median), shape = 95, color = "grey10", size = 8) +
    scale_y_continuous(limits = c(-1, 1), breaks = seq(-1, 1, 0.5)) +
    guides(color = "none") +
    labs(
      title = paste0(
        "Verteilungsbreite der Hauptoperationalisierung - ",
        study_title
      ),
      subtitle = "Punkt = Mittelwert; Querstrich = Median; dicke Linie = Interquartilsabstand; dünne Linie = 10.–90. Perzentil",
      caption = study_caption,
      x = NULL,
      y = "Konstruktreichweite [-1, +1]"
    ) +
    theme_minimal(base_size = 12) +
    theme(
      plot.title = element_text(face = "bold", size = 14),
      plot.subtitle = element_text(size = 10, color = "grey35"),
      plot.caption = element_text(size = 9, color = "grey35", hjust = 0),
      axis.text.x = element_text(angle = 0, hjust = 0.5),
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_blank()
    )

  ggsave(
    filename = file.path(
      DIR_H12_PLOTS,
      paste0("h1_2_scaled_midpoint_intervalplot_", study_id, ".png")
    ),
    plot   = p_scaled_interval_country,
    width  = 9,
    height = 5,
    dpi    = 320
  )
  ggsave(
    filename = file.path(
      DIR_H12_PLOTS,
      paste0("h1_2_scaled_midpoint_dotplot_", study_id, ".png")
    ),
    plot   = p_scaled_interval_country,
    width  = 9,
    height = 5,
    dpi    = 320
  )

  p_raw_scaled_relation_country <- ggplot(
    raw_scaled_country,
    aes(x = raw_index, y = scaled_midpoint, color = construct_country)
  ) +
    geom_point(alpha = 0.20, size = 0.8, na.rm = TRUE) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "grey45") +
    geom_vline(xintercept = 3, linetype = "dashed", color = "grey45") +
    facet_wrap(~ construct_country, nrow = 1) +
    scale_x_continuous(limits = c(1, 5), breaks = 1:5) +
    scale_y_continuous(limits = c(-1, 1), breaks = seq(-1, 1, 0.5)) +
    guides(color = "none") +
    labs(
      title = paste0(
        "Rohindex und Hauptoperationalisierung im Verhältnis - ",
        study_title
      ),
      subtitle = "Die Hauptoperationalisierung stammt aus der zentralen itemweisen skalenmittigen Transformation",
      caption = study_caption,
      x = "Rohindex (1-5)",
      y = "Konstruktreichweite [-1, +1]"
    ) +
    theme_minimal(base_size = 12) +
    theme(
      plot.title = element_text(face = "bold", size = 14),
      plot.subtitle = element_text(size = 10, color = "grey35"),
      plot.caption = element_text(size = 10, color = "grey35", hjust = 0),
      strip.text = element_text(face = "bold"),
      panel.grid.minor = element_blank()
    )

  ggsave(
    filename = file.path(
      DIR_H12_PLOTS,
      paste0("h1_2_raw_index_vs_scaled_midpoint_", study_id, ".png")
    ),
    plot   = p_raw_scaled_relation_country,
    width  = 12,
    height = 4.8,
    dpi    = 320
  )
}

tbl_h12 <- h12_summary %>%
  mutate(
    mean_raw = sprintf("%.2f", mean_raw),
    sd_raw = sprintf("%.2f", sd_raw),
    median_raw = sprintf("%.2f", median_raw),
    raw_midpoint = sprintf("%.2f", raw_midpoint),
    share_above_midpoint = sprintf("%.1f%%", share_above_midpoint * 100),
    share_at_or_above_midpoint = sprintf("%.1f%%", share_at_or_above_midpoint * 100)
  ) %>%
  select(
    study_label, construct, n,
    mean_raw, sd_raw, median_raw, raw_midpoint,
    share_above_midpoint, share_at_or_above_midpoint
  ) %>%
  gt(groupname_col = "study_label") %>%
  tab_header(
    title = "Überblick der Rohindizes relativ zur Konstruktmitte",
    subtitle = "Rohindizes auf der ursprünglichen Antwortskala; Mitte konstrukt- und fallbezogen berechnet"
  ) %>%
  cols_label(
    construct = "Konstrukt",
    n = "N",
    mean_raw = "M",
    sd_raw = "SD",
    median_raw = "Median",
    raw_midpoint = "Mitte",
    share_above_midpoint = "Anteil > Mitte",
    share_at_or_above_midpoint = "Anteil >= Mitte"
  ) %>%
  tab_source_note(
    "Hinweis: Fünfstufige Items gehen mit einer Itemmitte von 3.0 ein, vierstufige Items mit einer Itemmitte von 2.5. Die Konstruktmitte ist der Mittelwert der enthaltenen Itemmitten."
  ) %>%
  tab_options(table.width = px(920))

save_gt_png(
  tbl_h12,
  file.path(DIR_H12_PLOTS, "h1_2_raw_index_summary.png"),
  expand = 15
)

tbl_h12_scaled <- h12_scaled_summary %>%
  mutate(
    mean_scaled = sprintf("%.2f", mean_scaled),
    sd_scaled = sprintf("%.2f", sd_scaled),
    median_scaled = sprintf("%.2f", median_scaled),
    share_above_0 = sprintf("%.1f%%", share_above_0 * 100),
    share_below_0 = sprintf("%.1f%%", share_below_0 * 100)
  ) %>%
  select(
    study_label, construct, n,
    mean_scaled, sd_scaled, median_scaled,
    share_above_0, share_below_0
  ) %>%
  gt(groupname_col = "study_label") %>%
  tab_header(
    title = "Überblick der Hauptoperationalisierung",
    subtitle = "Itemweise skalenmittig transformierte Konstruktwerte"
  ) %>%
  cols_label(
    construct = "Konstrukt",
    n = "N",
    mean_scaled = "M",
    sd_scaled = "SD",
    median_scaled = "Median",
    share_above_0 = "Anteil > 0",
    share_below_0 = "Anteil < 0"
  ) %>%
  tab_source_note(
    ""
  ) %>%
  tab_options(table.width = px(860))

save_gt_png(
  tbl_h12_scaled,
  file.path(DIR_H12_PLOTS, "h1_2_scaled_midpoint_summary.png"),
  expand = 15
)

for (study_id in expected_studies) {
  tbl_h12_country <- h12_summary %>%
    filter(.data$study == .env$study_id) %>%
    mutate(
      mean_raw = sprintf("%.2f", mean_raw),
      sd_raw = sprintf("%.2f", sd_raw),
      median_raw = sprintf("%.2f", median_raw),
      raw_midpoint = sprintf("%.2f", raw_midpoint),
      share_above_midpoint = sprintf("%.1f%%", share_above_midpoint * 100),
      share_at_or_above_midpoint = sprintf("%.1f%%", share_at_or_above_midpoint * 100)
    ) %>%
    select(
      construct, n,
      mean_raw, sd_raw, median_raw, raw_midpoint,
      share_above_midpoint, share_at_or_above_midpoint
    ) %>%
    gt() %>%
    tab_header(
      title = "Überblick der Rohindizes relativ zur Konstruktmitte",
      subtitle = study_labels_de[study_id]
    ) %>%
    cols_label(
      construct = "Konstrukt",
      n = "N",
      mean_raw = "M",
      sd_raw = "SD",
      median_raw = "Median",
      raw_midpoint = "Mitte",
      share_above_midpoint = "Anteil > Mitte",
      share_at_or_above_midpoint = "Anteil >= Mitte"
    ) %>%
    tab_source_note(
      "Hinweis: Fünfstufige Items gehen mit einer Itemmitte von 3.0 ein, vierstufige Items mit einer Itemmitte von 2.5. Die Konstruktmitte ist der Mittelwert der enthaltenen Itemmitten."
    ) %>%
    tab_options(table.width = px(920))

  save_gt_png(
    tbl_h12_country,
    file.path(
      DIR_H12_PLOTS,
      paste0("h1_2_raw_index_summary_", study_id, ".png")
    ),
    expand = 15
  )

  tbl_h12_scaled_country <- h12_scaled_summary %>%
    filter(.data$study == .env$study_id) %>%
    mutate(
      mean_scaled = sprintf("%.2f", mean_scaled),
      sd_scaled = sprintf("%.2f", sd_scaled),
      median_scaled = sprintf("%.2f", median_scaled),
      share_above_0 = sprintf("%.1f%%", share_above_0 * 100),
      share_below_0 = sprintf("%.1f%%", share_below_0 * 100)
    ) %>%
    select(
      construct, n,
      mean_scaled, sd_scaled, median_scaled,
      share_above_0, share_below_0
    ) %>%
    gt() %>%
    tab_header(
      title = "Überblick der Hauptoperationalisierung",
      subtitle = study_labels_de[study_id]
    ) %>%
    cols_label(
      construct = "Konstrukt",
      n = "N",
      mean_scaled = "M",
      sd_scaled = "SD",
      median_scaled = "Median",
      share_above_0 = "Anteil > 0",
      share_below_0 = "Anteil < 0"
    ) %>%
    tab_source_note(
      ""
    ) %>%
    tab_options(table.width = px(840))

  save_gt_png(
    tbl_h12_scaled_country,
    file.path(
      DIR_H12_PLOTS,
      paste0("h1_2_scaled_midpoint_summary_", study_id, ".png")
    ),
    expand = 15
  )
}

###############################################################################
## 4) CFA-ROBUSTHEIT: 3 FAKTOREN VS. 2 FAKTOREN
###############################################################################

cat("\n============================================================\n")
cat("CFA-Robustheit: 3 Faktoren vs. 2 Faktoren\n")
cat("============================================================\n")

cfa_results <- list()
model_syntax <- list()

for (study in expected_studies) {
  cat("\n---", study, "---\n")

  sm <- study_models[[study]]
  items_study <- unique(c(
    sm$EXCL$items,
    sm$POPU$items,
    sm$PRG$items
  ))

  dat_cfa <- CSES5_sub %>%
    filter(election_study_id == study) %>%
    select(all_of(items_study)) %>%
    drop_na()

  model_3f <- build_cfa_3f(study, add_residuals = TRUE)
  model_2f <- build_cfa_2f_excl_popu(study, add_residuals = TRUE)
  model_2f_label <- if (study == "AUT_2017") {
    "Zwei-Faktoren-Modell: EXCL + AE"
  } else {
    "Zwei-Faktoren-Modell: EXCL + POPU"
  }

  model_syntax[[paste0(study, "_3f")]] <- model_3f
  model_syntax[[paste0(study, "_2f")]] <- model_2f

  cat("3-Faktoren-Modell:\n")
  cat(model_3f, "\n\n")
  cat("2-Faktoren-Modell:\n")
  cat(model_2f, "\n\n")

  fit_3f <- lavaan::cfa(
    model_3f,
    data      = dat_cfa,
    estimator = "MLR",
    std.lv    = TRUE
  )

  fit_2f <- lavaan::cfa(
    model_2f,
    data      = dat_cfa,
    estimator = "MLR",
    std.lv    = TRUE
  )

  cfa_results[[study]] <- bind_rows(
    extract_fit(
      fit_3f,
      study,
      "Drei-Faktoren-Modell",
      nrow(dat_cfa)
    ),
    extract_fit(
      fit_2f,
      study,
      model_2f_label,
      nrow(dat_cfa)
    )
  )
}

cfa_fit <- bind_rows(cfa_results) %>%
  group_by(study) %>%
  mutate(
    delta_cfi_vs_3f = cfi - cfi[model == "Drei-Faktoren-Modell"],
    delta_tli_vs_3f = tli - tli[model == "Drei-Faktoren-Modell"],
    delta_rmsea_vs_3f = rmsea - rmsea[model == "Drei-Faktoren-Modell"],
    delta_srmr_vs_3f = srmr - srmr[model == "Drei-Faktoren-Modell"]
  ) %>%
  ungroup() %>%
  mutate(
    across(
      starts_with("delta_"),
      ~ round(.x, 3)
    )
  )

write_csv(
  cfa_fit,
  file.path(DIR_CFA_TABLES, "cfa_3f_vs_2f_fit_indices.csv")
)

syntax_path <- file.path(DIR_CFA_TABLES, "cfa_3f_vs_2f_model_syntax.txt")
writeLines(
  unlist(
    imap(model_syntax, ~ c(
      paste0("### ", .y),
      .x,
      ""
    ))
  ),
  con = syntax_path
)

cat("Modellsyntax gespeichert:", syntax_path, "\n")

tbl_cfa <- cfa_fit %>%
  mutate(
    chisq = sprintf("%.2f", chisq),
    rmsea_str = sprintf("%.3f [%.3f, %.3f]", rmsea, rmsea_lo, rmsea_hi),
    delta_cfi_vs_3f = ifelse(
      model == "Drei-Faktoren-Modell",
      "",
      sprintf("%.3f", delta_cfi_vs_3f)
    ),
    delta_rmsea_vs_3f = ifelse(
      model == "Drei-Faktoren-Modell",
      "",
      sprintf("%.3f", delta_rmsea_vs_3f)
    )
  ) %>%
  select(
    study_label, model, n,
    chisq, df, cfi, tli, rmsea_str, srmr,
    delta_cfi_vs_3f, delta_rmsea_vs_3f
  ) %>%
  gt(groupname_col = "study_label") %>%
  tab_header(
    title = "CFA-Robustheit: Drei-Faktoren-Modell vs. Zwei-Faktoren-Modell",
    subtitle = "Konkurrenzmodell mit gemeinsamer EXCL- und POPU- beziehungsweise AE-Dimension"
  ) %>%
  cols_label(
    model = "Modell",
    n = "N",
    chisq = "χ²",
    df = "df",
    cfi = "CFI",
    tli = "TLI",
    rmsea_str = "RMSEA [90%-KI]",
    srmr = "SRMR",
    delta_cfi_vs_3f = "ΔCFI",
    delta_rmsea_vs_3f = "ΔRMSEA"
  ) %>%
  tab_source_note(
    "Hinweis: EXCL = Exklusionistische Demokratie; POPU = Populistische Demokratie; AE = Anti-elitistische Demokratie. In Österreich bezeichnet das Zwei-Faktoren-Modell EXCL + AE. Beide Modelle enthalten dieselben theoretisch begruendeten residualen Korrelationen innerhalb der Itemdimensionen. Δ-Werte beziehen sich auf das Drei-Faktoren-Modell."
  )

save_gt_png(
  tbl_cfa,
  file.path(DIR_CFA_PLOTS, "cfa_3f_vs_2f_fit_indices.png"),
  expand = 15
)

for (study_id in expected_studies) {
  tbl_country <- cfa_fit %>%
    filter(.data$study == .env$study_id) %>%
    mutate(
      chisq = sprintf("%.2f", chisq),
      rmsea_str = sprintf("%.3f [%.3f, %.3f]", rmsea, rmsea_lo, rmsea_hi),
      delta_cfi_vs_3f = ifelse(
        model == "Drei-Faktoren-Modell",
        "",
        sprintf("%.3f", delta_cfi_vs_3f)
      ),
      delta_rmsea_vs_3f = ifelse(
        model == "Drei-Faktoren-Modell",
        "",
        sprintf("%.3f", delta_rmsea_vs_3f)
      )
    ) %>%
    select(
      model, n, chisq, df, cfi, tli,
      rmsea_str, srmr,
      delta_cfi_vs_3f, delta_rmsea_vs_3f
    ) %>%
    gt() %>%
    tab_header(
      title = "CFA-Robustheit: Drei Faktoren vs. zwei Faktoren",
      subtitle = study_labels_de[study_id]
    ) %>%
    cols_label(
      model = "Modell",
      n = "N",
      chisq = "χ²",
      df = "df",
      cfi = "CFI",
      tli = "TLI",
      rmsea_str = "RMSEA [90%-KI]",
      srmr = "SRMR",
      delta_cfi_vs_3f = "ΔCFI",
      delta_rmsea_vs_3f = "ΔRMSEA"
    ) %>%
    tab_source_note(
      if (study_id == "AUT_2017") {
        "Hinweis: EXCL = Exklusionistische Demokratie; AE = Anti-elitistische Demokratie. Δ-Werte beziehen sich auf das Drei-Faktoren-Modell."
      } else {
        "Hinweis: EXCL = Exklusionistische Demokratie; POPU = Populistische Demokratie. Δ-Werte beziehen sich auf das Drei-Faktoren-Modell."
      }
    )

  save_gt_png(
    tbl_country,
    file.path(DIR_CFA_PLOTS, paste0("cfa_3f_vs_2f_fit_indices_", study_id, ".png")),
    expand = 15
  )
}

###############################################################################
## 5) ABSCHLUSS
###############################################################################

cat("\n============================================================\n")
cat("02_B abgeschlossen.\n")
cat("Outputs:\n")
cat("  ", DIR_H12_PLOTS, "\n")
cat("  ", DIR_H12_TABLES, "\n")
cat("  ", DIR_CFA_PLOTS, "\n")
cat("  ", DIR_CFA_TABLES, "\n")
cat("============================================================\n")
