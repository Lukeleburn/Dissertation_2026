###############################################################################
## DISS_2026 — 03_B2_lpa_deu_solution_diagnostics_scaled_midpoint.R
## Deutschland-Diagnostik fuer konkurrierende LPA-Loesungen
##
## Zweck:
##   Prueft fuer Deutschland 2021 mehrere plausible Modell-Klassen-Kombinationen.
##   Entscheidend ist nicht nur der Fit, sondern vor allem:
##     1) Stabilitaet der Klassenzuordnung
##     2) Stabilitaet der Profilmittelwerte
##     3) keine extrem kleinen Klassen
##     4) interpretierbare Profilstruktur
##
## Vorgehen:
##   Fuer jeden Kandidaten wird zunaechst eine Referenzloesung geschaetzt.
##   Anschliessend wird dieselbe Modell-Klassen-Kombination mehrfach erneut
##   geschaetzt:
##     - Seed-Pruefung ohne Datenveraenderung
##     - Jitter-Pruefung mit minimal gestoerten Scores
##
## Output:
##   outputs/tables/tidylpa_scaled_midpoint/deu_solution_diagnostics/
##   outputs/plots/tidylpa_scaled_midpoint/deu_solution_diagnostics/
##
## Dieses Skript ueberschreibt nur die DEU-Diagnostikoutputs. Die finalen
## 03_B-Outputs bleiben unveraendert.
###############################################################################

suppressPackageStartupMessages({
  library(tidyverse)
  library(tidyLPA)
  library(mclust)
  library(here)
})

set.seed(20262301)

OVERWRITE_OUTPUTS <- TRUE

###############################################################################
## 0) EINSTELLUNGEN
###############################################################################

STUDY <- "DEU_2021"

## Bei Bedarf erhoehen. Fuer eine erste Diagnose sind diese Werte ausreichend.
N_REPS_SEED_ONLY <- 10L
N_REPS_JITTER    <- 25L
JITTER_SD        <- 0.01

BASE_SEED <- 20262301L

## Kandidatenlogik:
##   Modell 1: gleiche Varianzen, keine Kovarianzen
##   Modell 2: freie Varianzen, keine Kovarianzen
##   Modell 3: gleiche Varianzen, gleiche Kovarianzen
##   Modell 6: freie Varianzen, freie Kovarianzen
##
## Modell 6 mit 4 Klassen ist bewusst optional, weil es deutlich
## parameterreicher ist. Bei Bedarf include = TRUE setzen.
candidates_deu <- tribble(
  ~model, ~classes, ~include,
  1L,     3L,       TRUE,
  1L,     4L,       TRUE,
  2L,     3L,       TRUE,
  2L,     4L,       TRUE,
  3L,     3L,       TRUE,
  3L,     4L,       TRUE,
  6L,     3L,       TRUE,
  6L,     4L,       FALSE
) %>%
  filter(include) %>%
  mutate(
    candidate_id = paste0("M", model, "_G", classes)
  )

###############################################################################
## 1) PFADE
###############################################################################

DIR_DERIVED <- here("data", "processed", "cses5", "derived")
PATH_IN <- file.path(
  DIR_DERIVED,
  "CSES5_20262301_operationalised_scaled_midpoint.rds"
)

DIR_TABLES <- here("outputs", "tables",
                   "tidylpa_scaled_midpoint",
                   "deu_solution_diagnostics")
DIR_PLOTS <- here("outputs", "plots",
                  "tidylpa_scaled_midpoint",
                  "deu_solution_diagnostics")
DIR_MODELS <- here("outputs", "models",
                   "tidylpa_scaled_midpoint",
                   "deu_solution_diagnostics")

for (d in c(DIR_TABLES, DIR_PLOTS, DIR_MODELS)) {
  dir.create(d, recursive = TRUE, showWarnings = FALSE)
}

if (OVERWRITE_OUTPUTS) {
  unlink(list.files(DIR_TABLES, full.names = TRUE), recursive = TRUE, force = TRUE)
  unlink(list.files(DIR_PLOTS,  full.names = TRUE), recursive = TRUE, force = TRUE)
  unlink(list.files(DIR_MODELS, full.names = TRUE), recursive = TRUE, force = TRUE)
}

###############################################################################
## 2) LABELS UND VARIABLEN
###############################################################################

study_label <- "Deutschland 2021"

score_vars <- c(
  "EXCL_score_scaled_midpoint",
  "POPU_score_scaled_midpoint",
  "PRG_score_scaled_midpoint"
)

short_names <- c("EXCL", "POP", "PRG")

model_labels <- c(
  "1" = "Gleiche Varianzen / Keine Kovarianzen",
  "2" = "Freie Varianzen / Keine Kovarianzen",
  "3" = "Gleiche Varianzen / Gleiche Kovarianzen",
  "6" = "Freie Varianzen / Freie Kovarianzen"
)

###############################################################################
## 3) HILFSFUNKTIONEN
###############################################################################

make_scenarios <- function() {
  bind_rows(
    tibble(
      scenario = "seed_only",
      scenario_label = "Seed-Pruefung",
      rep_id = seq_len(N_REPS_SEED_ONLY),
      jitter_sd = 0
    ),
    tibble(
      scenario = "jitter",
      scenario_label = paste0("Jitter-Pruefung (SD = ", JITTER_SD, ")"),
      rep_id = seq_len(N_REPS_JITTER),
      jitter_sd = JITTER_SD
    )
  ) %>%
    mutate(seed = BASE_SEED + row_number())
}

pick_fit_value <- function(fit, name) {
  if (name %in% names(fit)) {
    return(as.numeric(fit[[name]][1]))
  }
  if (tolower(name) %in% names(fit)) {
    return(as.numeric(fit[[tolower(name)]][1]))
  }
  NA_real_
}

extract_fit <- function(mod) {
  fit <- suppressWarnings(get_fit(mod))

  tibble(
    AIC = pick_fit_value(fit, "AIC"),
    BIC = pick_fit_value(fit, "BIC"),
    SABIC = pick_fit_value(fit, "SABIC"),
    Entropy = pick_fit_value(fit, "Entropy")
  )
}

extract_means <- function(mod, classes_total) {
  get_estimates(mod) %>%
    filter(Category == "Means") %>%
    select(Parameter, Class, Estimate) %>%
    pivot_wider(names_from = Parameter,
                values_from = Estimate) %>%
    arrange(Class) %>%
    select(-Class) %>%
    as.matrix() %>%
    `colnames<-`(short_names) %>%
    `rownames<-`(as.character(seq_len(classes_total)))
}

all_permutations <- function(x) {
  if (length(x) == 1) {
    return(matrix(x, nrow = 1))
  }
  do.call(
    rbind,
    lapply(seq_along(x), function(i) {
      cbind(x[i], all_permutations(x[-i]))
    })
  )
}

match_classes_by_means <- function(ref_means, rep_means) {
  k <- nrow(ref_means)
  perms <- all_permutations(seq_len(k))

  losses <- apply(perms, 1, function(p) {
    sum((ref_means - rep_means[p, , drop = FALSE])^2)
  })

  best <- perms[which.min(losses), ]

  tibble(
    rep_class = as.integer(best),
    ref_class = seq_len(k),
    distance = sqrt(rowSums(
      (ref_means - rep_means[best, , drop = FALSE])^2
    ))
  )
}

run_lpa_once <- function(X, model, classes, seed, jitter_sd) {
  set.seed(seed)

  X_run <- X
  if (jitter_sd > 0) {
    X_run <- X_run %>%
      mutate(across(
        everything(),
        ~ .x + rnorm(length(.x), mean = 0, sd = jitter_sd)
      ))
  }

  warns <- character(0)

  mod <- withCallingHandlers(
    X_run %>%
      estimate_profiles(
        n_profiles = classes,
        models = model
      ),
    warning = function(w) {
      warns <<- c(warns, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )

  list(
    model = mod,
    warnings = warns
  )
}

make_candidate_label <- function(model, classes) {
  paste0(
    "M", model, " / G", classes, "\n",
    model_labels[as.character(model)]
  )
}

###############################################################################
## 4) DATEN LADEN
###############################################################################

if (!file.exists(PATH_IN)) {
  stop("Input-RDS nicht gefunden: ", PATH_IN, call. = FALSE)
}

df_full <- readRDS(PATH_IN)

dat_study <- df_full %>%
  filter(election_study_id == STUDY) %>%
  select(respondent_id, all_of(score_vars)) %>%
  drop_na()

X <- dat_study %>%
  select(all_of(score_vars)) %>%
  rename_with(~ short_names)

n_cc <- nrow(X)

cat("\n")
cat("============================================================\n")
cat("Deutschland-Diagnostik konkurrierender LPA-Loesungen\n")
cat("============================================================\n")
cat("N (vollstaendige Faelle):", n_cc, "\n")
cat("Kandidaten:\n")
print(
  candidates_deu %>%
    mutate(
      variance_structure = model_labels[as.character(model)]
    ) %>%
    select(candidate_id, model, classes, variance_structure),
  n = Inf
)

###############################################################################
## 5) REFERENZ- UND STABILITAETSSCHAETZUNGEN
###############################################################################

scenario_tbl <- make_scenarios()

reference_tbls <- list()
run_tbls <- list()
means_tbls <- list()
sizes_tbls <- list()
matches_tbls <- list()

for (cand_i in seq_len(nrow(candidates_deu))) {

  cand <- candidates_deu[cand_i, ]
  candidate_id <- cand$candidate_id
  model <- cand$model
  classes <- cand$classes
  candidate_label <- make_candidate_label(model, classes)

  cat("\n")
  cat("------------------------------------------------------------\n")
  cat("Kandidat:", candidate_id, "\n")
  cat("Modell:  ", model_labels[as.character(model)], "\n", sep = "")
  cat("Klassen: ", classes, "\n", sep = "")
  cat("------------------------------------------------------------\n")

  ref_seed <- BASE_SEED + cand_i * 1000L

  ref_result <- tryCatch(
    run_lpa_once(
      X = X,
      model = model,
      classes = classes,
      seed = ref_seed,
      jitter_sd = 0
    ),
    error = function(e) e
  )

  if (inherits(ref_result, "error")) {
    warning("Referenzloesung fehlgeschlagen fuer ",
            candidate_id, ": ", conditionMessage(ref_result))

    reference_tbls[[candidate_id]] <- tibble(
      candidate_id = candidate_id,
      candidate_label = candidate_label,
      model = model,
      variance_structure = model_labels[as.character(model)],
      classes_total = classes,
      reference_converged = FALSE,
      reference_warning_text = conditionMessage(ref_result),
      AIC = NA_real_,
      BIC = NA_real_,
      SABIC = NA_real_,
      Entropy = NA_real_
    )
    next
  }

  ref_mod <- ref_result$model
  ref_pred <- get_data(ref_mod) %>%
    as_tibble()
  ref_assign <- ref_pred$Class
  ref_means <- extract_means(ref_mod, classes)
  ref_fit <- extract_fit(ref_mod)

  saveRDS(
    ref_mod,
    file.path(DIR_MODELS,
              paste0("lpa_deu_reference_", candidate_id, ".rds"))
  )

  ref_sizes <- tibble(ref_class = ref_assign) %>%
    dplyr::count(ref_class, name = "n") %>%
    complete(ref_class = seq_len(classes),
             fill = list(n = 0L)) %>%
    mutate(
      pct = round(n / n_cc * 100, 1)
    )

  reference_tbls[[candidate_id]] <- tibble(
    candidate_id = candidate_id,
    candidate_label = candidate_label,
    model = model,
    variance_structure = model_labels[as.character(model)],
    classes_total = classes,
    n_complete = n_cc,
    reference_converged = TRUE,
    reference_warning_count = length(ref_result$warnings),
    reference_warning_text = paste(unique(ref_result$warnings),
                                   collapse = " | "),
    min_class_pct = min(ref_sizes$pct),
    max_class_pct = max(ref_sizes$pct)
  ) %>%
    bind_cols(ref_fit)

  ref_means_tbl <- as_tibble(ref_means,
                             rownames = "ref_class") %>%
    mutate(
      ref_class = as.integer(ref_class),
      candidate_id = candidate_id,
      candidate_label = candidate_label,
      model = model,
      classes_total = classes
    ) %>%
    relocate(candidate_id, candidate_label,
             model, classes_total, ref_class)

  write_csv(
    ref_means_tbl,
    file.path(DIR_TABLES,
              paste0("reference_means_", candidate_id, ".csv"))
  )

  write_csv(
    ref_sizes %>%
      mutate(
        candidate_id = candidate_id,
        candidate_label = candidate_label,
        model = model,
        classes_total = classes
      ) %>%
      relocate(candidate_id, candidate_label,
               model, classes_total),
    file.path(DIR_TABLES,
              paste0("reference_class_sizes_", candidate_id, ".csv"))
  )

  for (i in seq_len(nrow(scenario_tbl))) {
    sc <- scenario_tbl[i, ]

    cat(sprintf(
      "  [%s] %03d/%03d | seed=%d | jitter_sd=%.3f\n",
      candidate_id, i, nrow(scenario_tbl), sc$seed, sc$jitter_sd
    ))

    result <- tryCatch(
      run_lpa_once(
        X = X,
        model = model,
        classes = classes,
        seed = sc$seed,
        jitter_sd = sc$jitter_sd
      ),
      error = function(e) e
    )

    run_key <- paste(candidate_id, sc$scenario, sc$rep_id, sep = "_")

    if (inherits(result, "error")) {
      run_tbls[[run_key]] <- tibble(
        candidate_id = candidate_id,
        candidate_label = candidate_label,
        model = model,
        variance_structure = model_labels[as.character(model)],
        classes_total = classes,
        scenario = sc$scenario,
        scenario_label = sc$scenario_label,
        rep_id = sc$rep_id,
        seed = sc$seed,
        jitter_sd = sc$jitter_sd,
        n_complete = n_cc,
        converged = FALSE,
        warning_count = NA_integer_,
        warning_text = conditionMessage(result),
        ARI = NA_real_,
        mean_class_distance = NA_real_,
        max_class_distance = NA_real_,
        AIC = NA_real_,
        BIC = NA_real_,
        SABIC = NA_real_,
        Entropy = NA_real_
      )
      next
    }

    mod <- result$model
    pred_tbl <- get_data(mod) %>%
      as_tibble()

    class_assign <- pred_tbl$Class
    rep_means <- extract_means(mod, classes)
    class_match <- match_classes_by_means(ref_means, rep_means)

    class_map <- setNames(class_match$ref_class,
                          class_match$rep_class)
    aligned_class <- as.integer(
      class_map[as.character(class_assign)]
    )

    ari <- adjustedRandIndex(ref_assign, aligned_class)
    fit_row <- extract_fit(mod)

    relevant_warns <- result$warnings[
      str_detect(
        result$warnings,
        regex(
          paste0("could not estimate|did not converge|singular"),
          ignore_case = TRUE
        )
      )
    ]

    run_tbls[[run_key]] <- tibble(
      candidate_id = candidate_id,
      candidate_label = candidate_label,
      model = model,
      variance_structure = model_labels[as.character(model)],
      classes_total = classes,
      scenario = sc$scenario,
      scenario_label = sc$scenario_label,
      rep_id = sc$rep_id,
      seed = sc$seed,
      jitter_sd = sc$jitter_sd,
      n_complete = n_cc,
      converged = length(relevant_warns) == 0,
      warning_count = length(result$warnings),
      warning_text = paste(unique(result$warnings),
                           collapse = " | "),
      ARI = ari,
      mean_class_distance = mean(class_match$distance),
      max_class_distance = max(class_match$distance)
    ) %>%
      bind_cols(fit_row)

    means_aligned <- rep_means[class_match$rep_class, ,
                               drop = FALSE]
    rownames(means_aligned) <- as.character(class_match$ref_class)

    means_tbls[[run_key]] <- as_tibble(
      means_aligned,
      rownames = "ref_class"
    ) %>%
      pivot_longer(
        cols = all_of(short_names),
        names_to = "construct",
        values_to = "estimate"
      ) %>%
      mutate(
        candidate_id = candidate_id,
        candidate_label = candidate_label,
        model = model,
        variance_structure = model_labels[as.character(model)],
        classes_total = classes,
        scenario = sc$scenario,
        scenario_label = sc$scenario_label,
        rep_id = sc$rep_id,
        seed = sc$seed,
        ref_class = as.integer(ref_class)
      ) %>%
      relocate(candidate_id, candidate_label,
               model, variance_structure, classes_total,
               scenario, scenario_label, rep_id, seed,
               ref_class, construct)

    sizes_tbls[[run_key]] <- tibble(
      rep_class = class_assign
    ) %>%
      mutate(ref_class = as.integer(
        class_map[as.character(rep_class)]
      )) %>%
      dplyr::count(ref_class, name = "n") %>%
      complete(ref_class = seq_len(classes),
               fill = list(n = 0L)) %>%
      mutate(
        pct = round(n / n_cc * 100, 1),
        candidate_id = candidate_id,
        candidate_label = candidate_label,
        model = model,
        variance_structure = model_labels[as.character(model)],
        classes_total = classes,
        scenario = sc$scenario,
        scenario_label = sc$scenario_label,
        rep_id = sc$rep_id,
        seed = sc$seed
      ) %>%
      relocate(candidate_id, candidate_label,
               model, variance_structure, classes_total,
               scenario, scenario_label, rep_id, seed,
               ref_class)

    matches_tbls[[run_key]] <- class_match %>%
      mutate(
        candidate_id = candidate_id,
        candidate_label = candidate_label,
        model = model,
        variance_structure = model_labels[as.character(model)],
        classes_total = classes,
        scenario = sc$scenario,
        scenario_label = sc$scenario_label,
        rep_id = sc$rep_id,
        seed = sc$seed
      ) %>%
      relocate(candidate_id, candidate_label,
               model, variance_structure, classes_total,
               scenario, scenario_label, rep_id, seed)
  }
}

###############################################################################
## 6) OUTPUT-TABELLEN
###############################################################################

reference_tbl <- bind_rows(reference_tbls)
run_tbl <- bind_rows(run_tbls)
means_tbl <- bind_rows(means_tbls)
size_tbl <- bind_rows(sizes_tbls)
match_tbl <- bind_rows(matches_tbls)

stability_summary <- run_tbl %>%
  group_by(candidate_id, candidate_label, model,
           variance_structure, classes_total,
           scenario, scenario_label) %>%
  summarise(
    n_runs = n(),
    n_converged = sum(converged, na.rm = TRUE),
    pct_converged = round(n_converged / n_runs * 100, 1),
    ARI_mean = round(mean(ARI, na.rm = TRUE), 3),
    ARI_median = round(median(ARI, na.rm = TRUE), 3),
    ARI_min = round(min(ARI, na.rm = TRUE), 3),
    ARI_max = round(max(ARI, na.rm = TRUE), 3),
    mean_class_distance = round(
      mean(mean_class_distance, na.rm = TRUE), 3
    ),
    max_class_distance = round(
      max(max_class_distance, na.rm = TRUE), 3
    ),
    BIC_mean = round(mean(BIC, na.rm = TRUE), 1),
    SABIC_mean = round(mean(SABIC, na.rm = TRUE), 1),
    Entropy_mean = round(mean(Entropy, na.rm = TRUE), 3),
    .groups = "drop"
  )

candidate_decision_tbl <- stability_summary %>%
  select(candidate_id, candidate_label, model,
         variance_structure, classes_total,
         scenario, ARI_median, ARI_min,
         mean_class_distance, max_class_distance,
         pct_converged, BIC_mean, SABIC_mean,
         Entropy_mean) %>%
  pivot_wider(
    names_from = scenario,
    values_from = c(ARI_median, ARI_min,
                    mean_class_distance,
                    max_class_distance,
                    pct_converged)
  ) %>%
  left_join(
    reference_tbl %>%
      select(candidate_id, min_class_pct, max_class_pct,
             BIC_ref = BIC, SABIC_ref = SABIC,
             Entropy_ref = Entropy),
    by = "candidate_id"
  ) %>%
  mutate(
    stability_flag = case_when(
      ARI_median_seed_only >= 0.95 &
        ARI_median_jitter >= 0.90 &
        min_class_pct >= 5 ~ "hoch",
      ARI_median_seed_only >= 0.80 &
        ARI_median_jitter >= 0.75 &
        min_class_pct >= 3 ~ "mittel",
      TRUE ~ "niedrig"
    )
  ) %>%
  arrange(
    factor(stability_flag, levels = c("hoch", "mittel", "niedrig")),
    desc(ARI_median_jitter),
    mean_class_distance_jitter,
    SABIC_ref
  )

write_csv(reference_tbl,
          file.path(DIR_TABLES, "deu_candidate_reference_fit.csv"))
write_csv(run_tbl,
          file.path(DIR_TABLES, "deu_candidate_stability_runs.csv"))
write_csv(means_tbl,
          file.path(DIR_TABLES, "deu_candidate_means_long.csv"))
write_csv(size_tbl,
          file.path(DIR_TABLES, "deu_candidate_class_sizes.csv"))
write_csv(match_tbl,
          file.path(DIR_TABLES, "deu_candidate_class_matching.csv"))
write_csv(stability_summary,
          file.path(DIR_TABLES, "deu_candidate_stability_summary.csv"))
write_csv(candidate_decision_tbl,
          file.path(DIR_TABLES, "deu_candidate_decision_table.csv"))

###############################################################################
## 7) PLOTS
###############################################################################

theme_diag <- function(base_size = 12) {
  theme_minimal(base_size = base_size) +
    theme(
      plot.title = element_text(face = "bold", size = 16),
      plot.subtitle = element_text(size = 11, color = "grey35"),
      plot.caption = element_text(size = 10, color = "grey35",
                                  hjust = 0),
      strip.text = element_text(face = "bold"),
      panel.grid.minor = element_blank(),
      legend.position = "bottom"
    )
}

p_ari <- run_tbl %>%
  filter(converged) %>%
  mutate(
    candidate_label = fct_reorder(candidate_label, ARI, .fun = median)
  ) %>%
  ggplot(aes(x = candidate_label, y = ARI,
             color = scenario_label)) +
  geom_boxplot(width = 0.55, outlier.alpha = 0.5) +
  geom_jitter(width = 0.08, alpha = 0.25, size = 1) +
  coord_flip(ylim = c(0, 1)) +
  labs(
    title = "Deutschland 2021: Stabilitaet konkurrierender LPA-Loesungen",
    subtitle = "Adjusted Rand Index gegenueber der kandidatenspezifischen Referenzloesung",
    x = NULL,
    y = "Adjusted Rand Index",
    color = NULL,
    caption = "ARI = 1 bedeutet identische Klassenzuordnung; Klassennummern wurden vor dem Vergleich ueber Profilmittelwerte ausgerichtet."
  ) +
  theme_diag()

ggsave(
  file.path(DIR_PLOTS, "deu_candidate_stability_ari.png"),
  p_ari,
  width = 12,
  height = 7,
  dpi = 320
)

p_distance <- run_tbl %>%
  filter(converged) %>%
  mutate(
    candidate_label = fct_reorder(
      candidate_label,
      mean_class_distance,
      .fun = median,
      .desc = TRUE
    )
  ) %>%
  ggplot(aes(x = candidate_label,
             y = mean_class_distance,
             color = scenario_label)) +
  geom_boxplot(width = 0.55, outlier.alpha = 0.5) +
  geom_jitter(width = 0.08, alpha = 0.25, size = 1) +
  coord_flip() +
  labs(
    title = "Deutschland 2021: Stabilitaet der Profilmittelwerte",
    subtitle = "Mittlere Distanz der replizierten Profilzentren zur kandidatenspezifischen Referenzloesung",
    x = NULL,
    y = "Mittlere Distanz",
    color = NULL
  ) +
  theme_diag()

ggsave(
  file.path(DIR_PLOTS, "deu_candidate_mean_distances.png"),
  p_distance,
  width = 12,
  height = 7,
  dpi = 320
)

p_fit <- reference_tbl %>%
  filter(reference_converged) %>%
  select(candidate_label, BIC, AIC, SABIC, Entropy) %>%
  pivot_longer(
    cols = c(BIC, AIC, SABIC, Entropy),
    names_to = "fit_index",
    values_to = "value"
  ) %>%
  mutate(
    fit_index = recode(fit_index, Entropy = "Entropie"),
    candidate_label = fct_reorder(candidate_label, value,
                                  .fun = min)
  ) %>%
  ggplot(aes(x = candidate_label, y = value)) +
  geom_col(fill = "grey35", width = 0.65) +
  coord_flip() +
  facet_wrap(~ fit_index, scales = "free_x") +
  labs(
    title = "Deutschland 2021: Fit der Kandidatenloesungen",
    subtitle = "Referenzschaetzung je Modell-Klassen-Kombination",
    x = NULL,
    y = "Wert"
  ) +
  theme_diag() +
  theme(legend.position = "none")

ggsave(
  file.path(DIR_PLOTS, "deu_candidate_reference_fit.png"),
  p_fit,
  width = 12,
  height = 7,
  dpi = 320
)

reference_means_all <- map_dfr(
  candidates_deu$candidate_id,
  function(cid) {
    path <- file.path(DIR_TABLES,
                      paste0("reference_means_", cid, ".csv"))
    if (!file.exists(path)) return(tibble())
    read_csv(path, show_col_types = FALSE)
  }
)

p_profiles <- reference_means_all %>%
  pivot_longer(
    cols = all_of(short_names),
    names_to = "construct",
    values_to = "estimate"
  ) %>%
  mutate(
    ref_class = paste0("Klasse ", ref_class),
    candidate_label = fct_inorder(candidate_label)
  ) %>%
  ggplot(aes(x = construct, y = estimate,
             group = ref_class, color = ref_class)) +
  geom_hline(yintercept = 0, linetype = "dashed",
             color = "grey50") +
  geom_line(linewidth = 0.8) +
  geom_point(size = 2) +
  facet_wrap(~ candidate_label, ncol = 2) +
  labs(
    title = "Deutschland 2021: Profilmittelwerte der Kandidatenloesungen",
    subtitle = "Referenzloesungen auf skalenmittig transformierten Scores",
    x = NULL,
    y = "Profilmittelwert",
    color = NULL
  ) +
  theme_diag()

ggsave(
  file.path(DIR_PLOTS, "deu_candidate_reference_profiles.png"),
  p_profiles,
  width = 12,
  height = 10,
  dpi = 320
)

###############################################################################
## 8) KONSOLENZUSAMMENFASSUNG
###############################################################################

cat("\n")
cat("============================================================\n")
cat("Deutschland-Diagnostik abgeschlossen.\n\n")
cat("Entscheidungstabelle:\n")
print(candidate_decision_tbl, n = Inf)
cat("\nTabellen:\n  ", DIR_TABLES, "\n", sep = "")
cat("Plots:\n  ", DIR_PLOTS, "\n", sep = "")
cat("Referenzmodelle:\n  ", DIR_MODELS, "\n", sep = "")
cat("============================================================\n")
