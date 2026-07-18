###############################################################################
## DISS_2026 — 03_B_lpa_multi_init_scaled_midpoint.R
## LPA Schritt 2a: finale Multi-Init-Schaetzung der LPA
##
## Varianzstruktur + Klassenanzahl: aus 03_A / 03_B2 bestimmt.
## Dieses Skript erzeugt die verbindlichen finalen LPA-Ergebnisse fuer alle
## Folgeanalysen. Die finale Loesung wird aus mehreren Schaetzlaeufen auf den
## Originaldaten ausgewaehlt und anschliessend als RDS, Schaetzwerttabelle und
## Assignment-Datei inklusive Posterior-Wahrscheinlichkeiten gespeichert.
##
## Input:  data/processed/cses5/derived/
##         CSES5_20262301_operationalised_scaled_midpoint.rds
## Output: outputs/models/tidylpa_scaled_midpoint/lpa_final_*.rds
##         outputs/tables/tidylpa_scaled_midpoint/final/
##
## Hinweis zur Operationalisierung:
## Die verwendeten Scores beruhen auf der itemweisen skalenmittigen
## Transformation. Fuenfstufige Items werden mit (x - 3) / 2 transformiert,
## vierstufige Items mit (x - 2.5) / 1.5. Die Konstruktwerte entstehen danach
## als Mittelwerte der transformierten Items.
###############################################################################

suppressPackageStartupMessages({
  library(tidyverse)
  library(tidyLPA)
  library(here)
})

set.seed(20262301)

###############################################################################
## 0) EINSTELLUNGEN UND PFADE
###############################################################################

OVERWRITE_OUTPUTS <- TRUE
N_MULTI_STARTS    <- 100L
BASE_SEED         <- 20262301L
MIN_CLASS_PCT_WARN <- 2.0

DIR_DERIVED <- here("data", "processed", "cses5", "derived")
PATH_IN <- file.path(
  DIR_DERIVED,
  "CSES5_20262301_operationalised_scaled_midpoint.rds"
)

DIR_TABLES_FIT <- here("outputs", "tables", "tidylpa_scaled_midpoint", "fit")
DIR_MODELS <- here("outputs", "models", "tidylpa_scaled_midpoint")
DIR_TABLES_FINAL <- here("outputs", "tables", "tidylpa_scaled_midpoint", "final")

for (d in c(DIR_MODELS, DIR_TABLES_FINAL)) {
  dir.create(d, recursive = TRUE, showWarnings = FALSE)
}

if (OVERWRITE_OUTPUTS) {
  unlink(file.path(DIR_MODELS, "lpa_final_*.rds"))
  unlink(file.path(DIR_TABLES_FINAL, "*.csv"))
}

###############################################################################
## 1) MODELL-BEZEICHNUNGEN UND GEWAEHLTE LOESUNGEN
###############################################################################

model_labels <- c(
  "1" = "Gleiche Varianzen / Keine Kovarianzen",
  "2" = "Freie Varianzen / Keine Kovarianzen",
  "3" = "Gleiche Varianzen / Gleiche Kovarianzen",
  "6" = "Freie Varianzen / Freie Kovarianzen"
)

model_labels_long <- c(
  "1" = "Gleiche Varianzen, keine Kovarianzen",
  "2" = "Freie Varianzen, keine Kovarianzen",
  "3" = "Gleiche Varianzen, gleiche Kovarianzen",
  "6" = "Freie Varianzen, freie Kovarianzen"
)

chosen_solution <- list(
  AUT_2017 = list(model = 3L, classes = 4L),
  DEU_2021 = list(model = 2L, classes = 3L),
  GRC_2019 = list(model = 2L, classes = 3L)
)

study_vars <- list(
  AUT_2017 = list(
    vars  = c("EXCL_score_scaled_midpoint",
              "Anti_Elitist_score_scaled_midpoint",
              "Minimal_PRG_score_scaled_midpoint"),
    short = c("EXCL", "AE", "MPRG")
  ),
  DEU_2021 = list(
    vars  = c("EXCL_score_scaled_midpoint",
              "POPU_score_scaled_midpoint",
              "PRG_score_scaled_midpoint"),
    short = c("EXCL", "POP", "PRG")
  ),
  GRC_2019 = list(
    vars  = c("EXCL_score_scaled_midpoint",
              "POPU_score_scaled_midpoint",
              "PRG_score_scaled_midpoint"),
    short = c("EXCL", "POP", "PRG")
  )
)

study_name_map <- c(
  AUT_2017 = "Österreich 2017",
  DEU_2021 = "Deutschland 2021",
  GRC_2019 = "Griechenland 2019"
)

###############################################################################
## 2) HILFSFUNKTIONEN
###############################################################################

suggest_class_labels <- function(means_df, short_names) {
  labels <- apply(means_df, 1, function(row) {
    ranked  <- order(row, decreasing = TRUE)
    hi      <- short_names[ranked[1]]
    lo      <- short_names[ranked[length(ranked)]]
    mid_idx <- ranked[2]
    mid_dir <- ifelse(row[mid_idx] >= 0, "mid+", "mid-")
    paste0(hi, "-high / ", short_names[mid_idx], "-", mid_dir, " / ", lo, "-low")
  })
  dupes <- duplicated(labels) | duplicated(labels, fromLast = TRUE)
  if (any(dupes)) labels[dupes] <- paste0(labels[dupes], " (", which(dupes), ")")
  labels
}

relevant_warnings <- function(warns) {
  warns[
    str_detect(
      warns,
      regex("could not estimate|did not converge|singular|failed", ignore_case = TRUE)
    )
  ]
}

extract_fit_safe <- function(mod) {
  fit <- tryCatch(get_fit(mod), error = function(e) tibble())
  if (nrow(fit) == 0) {
    return(tibble(AIC = NA_real_, BIC = NA_real_, SABIC = NA_real_, Entropy = NA_real_))
  }

  fit <- fit %>% as_tibble() %>% slice(1)

  get_num <- function(dat, nm) {
    if (nm %in% names(dat)) {
      suppressWarnings(as.numeric(dat[[nm]][1]))
    } else {
      NA_real_
    }
  }

  tibble(
    AIC = get_num(fit, "AIC"),
    BIC = get_num(fit, "BIC"),
    SABIC = get_num(fit, "SABIC"),
    Entropy = get_num(fit, "Entropy")
  )
}

extract_means <- function(mod, short_names, K) {
  means_raw <- get_estimates(mod) %>%
    filter(Category == "Means") %>%
    select(Parameter, Class, Estimate) %>%
    pivot_wider(names_from = Parameter, values_from = Estimate) %>%
    arrange(Class)

  means_df <- means_raw %>% select(-Class) %>% as.data.frame()
  rownames(means_df) <- as.character(seq_len(K))
  colnames(means_df) <- short_names
  means_df
}

run_lpa_once <- function(X, sol, seed) {
  set.seed(seed)
  warns <- character(0)
  t_start <- proc.time()

  mod <- withCallingHandlers(
    X %>% estimate_profiles(n_profiles = sol$classes, models = sol$model),
    warning = function(w) {
      warns <<- c(warns, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )

  elapsed <- as.numeric((proc.time() - t_start)["elapsed"])
  list(model = mod, warnings = warns, elapsed = elapsed)
}

posterior_cols <- function(pred_tbl, K) {
  cprob <- pred_tbl %>%
    select(starts_with("CPROB"))

  if (ncol(cprob) != K) {
    warning("Anzahl der CPROB-Spalten weicht von K ab: ", ncol(cprob), " vs. ", K)
  }

  cprob %>%
    rename_with(~ paste0("posterior_class_", str_extract(.x, "\\d+"))) %>%
    mutate(across(everything(), ~ round(.x, 4)))
}

###############################################################################
## 3) DATEN LADEN
###############################################################################

if (!file.exists(PATH_IN)) stop("RDS nicht gefunden: ", PATH_IN, call. = FALSE)

df_full <- readRDS(PATH_IN) %>%
  filter(election_study_id %in% names(chosen_solution))

cat("Stichprobengroessen:\n")
print(table(df_full$election_study_id))
cat("\nMulti-Init-Laeufe je finaler Loesung:", N_MULTI_STARTS, "\n")

###############################################################################
## 4) HAUPTSCHLEIFE
###############################################################################

all_multi_init_summary <- list()

for (study in names(chosen_solution)) {

  cat("\n============================================================\n")
  cat("Finale Multi-Init-LPA:", study, "\n")
  cat("============================================================\n")

  cfg <- study_vars[[study]]
  sol <- chosen_solution[[study]]
  K <- sol$classes
  var_lbl <- model_labels_long[as.character(sol$model)]

  dat_study <- df_full %>%
    filter(election_study_id == study) %>%
    select(respondent_id, all_of(cfg$vars)) %>%
    drop_na()

  X <- dat_study %>%
    select(all_of(cfg$vars)) %>%
    rename_with(~ cfg$short)

  n_cc <- nrow(X)

  cat("Modell: ", var_lbl, "\n", sep = "")
  cat("Klassen:", K, "\n")
  cat("N vollstaendige Faelle:", n_cc, "\n\n")

  run_rows <- list()
  model_objects <- list()

  for (i in seq_len(N_MULTI_STARTS)) {
    seed_i <- BASE_SEED + i - 1L
    cat(sprintf("  Lauf %03d/%03d | seed=%d\n", i, N_MULTI_STARTS, seed_i))

    result <- tryCatch(
      run_lpa_once(X, sol, seed_i),
      error = function(e) e
    )

    if (inherits(result, "error")) {
      run_rows[[i]] <- tibble(
        study = study,
        study_label = study_name_map[study],
        run_id = i,
        seed = seed_i,
        model = sol$model,
        variance_structure = model_labels[as.character(sol$model)],
        classes_total = K,
        n_complete = n_cc,
        success = FALSE,
        converged = FALSE,
        warning_count = NA_integer_,
        warning_text = conditionMessage(result),
        elapsed_sec = NA_real_,
        min_class_pct = NA_real_,
        n_classes_nonempty = NA_integer_,
        AIC = NA_real_, BIC = NA_real_, SABIC = NA_real_, Entropy = NA_real_
      )
      next
    }

    mod <- result$model
    pred_tbl <- get_data(mod) %>% as_tibble()
    class_assign <- pred_tbl$Class
    class_counts <- table(factor(class_assign, levels = seq_len(K)))
    min_pct <- min(as.numeric(class_counts) / n_cc * 100)
    n_nonempty <- sum(class_counts > 0)
    rel_warns <- relevant_warnings(result$warnings)
    fit_row <- extract_fit_safe(mod)

    run_rows[[i]] <- tibble(
      study = study,
      study_label = study_name_map[study],
      run_id = i,
      seed = seed_i,
      model = sol$model,
      variance_structure = model_labels[as.character(sol$model)],
      classes_total = K,
      n_complete = n_cc,
      success = TRUE,
      converged = length(rel_warns) == 0,
      warning_count = length(result$warnings),
      warning_text = paste(unique(result$warnings), collapse = " | "),
      elapsed_sec = result$elapsed,
      min_class_pct = min_pct,
      n_classes_nonempty = n_nonempty
    ) %>% bind_cols(fit_row)

    model_objects[[as.character(i)]] <- mod
  }

  runs_tbl <- bind_rows(run_rows) %>%
    mutate(
      acceptable = success & converged & n_classes_nonempty == K & min_class_pct >= MIN_CLASS_PCT_WARN,
      selection_rank = case_when(
        acceptable ~ 1L,
        success & converged & n_classes_nonempty == K ~ 2L,
        success ~ 3L,
        TRUE ~ 4L
      )
    ) %>%
    arrange(selection_rank, BIC, AIC, desc(Entropy), run_id)

  write_csv(
    runs_tbl,
    file.path(DIR_TABLES_FINAL, paste0("lpa_multi_init_runs_", study, ".csv"))
  )

  selected_run <- runs_tbl %>% slice(1)
  selected_id <- as.character(selected_run$run_id)
  mod_best <- model_objects[[selected_id]]

  if (is.null(mod_best)) {
    stop("Keine schaetzbare LPA-Loesung fuer ", study, ".", call. = FALSE)
  }

  cat("\nAusgewaehlter Lauf:\n")
  print(selected_run %>% select(run_id, seed, BIC, AIC, SABIC, Entropy, min_class_pct, converged, warning_count))

  pred_tbl <- get_data(mod_best) %>% as_tibble()
  class_assign <- pred_tbl$Class
  posterior_tbl <- posterior_cols(pred_tbl, K)
  posterior_max <- posterior_tbl %>% apply(1, max)

  means_df <- extract_means(mod_best, cfg$short, K)
  class_counts <- table(factor(class_assign, levels = seq_len(K)))
  class_tbl <- tibble(
    class_num = as.character(seq_len(K)),
    n = as.integer(class_counts),
    pct = round(as.numeric(class_counts) / n_cc * 100, 1)
  )
  avg_posterior <- as.numeric(tapply(posterior_max, class_assign, mean))

  fit_selected <- extract_fit_safe(mod_best)

  estimates_tbl <- tibble(
    study = study,
    study_label = study_name_map[study],
    selected_run_id = selected_run$run_id,
    selected_seed = selected_run$seed,
    variance_structure = var_lbl,
    model = sol$model,
    classes_total = K,
    n_complete = n_cc
  ) %>%
    bind_cols(class_tbl %>% mutate(avg_posterior = round(avg_posterior, 3))) %>%
    bind_cols(as_tibble(round(means_df, 3))) %>%
    bind_cols(fit_selected %>% mutate(across(everything(), ~ round(.x, 3))))

  write_csv(
    estimates_tbl,
    file.path(DIR_TABLES_FINAL, paste0("lpa_estimates_", study, ".csv"))
  )

  assignment_tbl <- dat_study %>%
    mutate(
      lpa_class = as.integer(class_assign),
      lpa_posterior = round(posterior_max, 4)
    ) %>%
    bind_cols(posterior_tbl)

  write_csv(
    assignment_tbl,
    file.path(DIR_TABLES_FINAL, paste0("lpa_assignment_", study, ".csv"))
  )

  saveRDS(
    mod_best,
    file.path(DIR_MODELS, paste0("lpa_final_", study, ".rds"))
  )

  summary_tbl <- runs_tbl %>%
    summarise(
      study = first(study),
      study_label = first(study_label),
      model = first(model),
      variance_structure = first(variance_structure),
      classes_total = first(classes_total),
      n_complete = first(n_complete),
      n_runs = n(),
      n_success = sum(success, na.rm = TRUE),
      n_converged = sum(converged, na.rm = TRUE),
      n_acceptable = sum(acceptable, na.rm = TRUE),
      selected_run_id = selected_run$run_id,
      selected_seed = selected_run$seed,
      selected_BIC = selected_run$BIC,
      selected_AIC = selected_run$AIC,
      selected_SABIC = selected_run$SABIC,
      selected_Entropy = selected_run$Entropy,
      selected_min_class_pct = selected_run$min_class_pct,
      unique_BIC_values = n_distinct(round(BIC[success], 6), na.rm = TRUE)
    )

  write_csv(
    summary_tbl,
    file.path(DIR_TABLES_FINAL, paste0("lpa_multi_init_summary_", study, ".csv"))
  )

  all_multi_init_summary[[study]] <- summary_tbl

  cat("\nMittelwerte je Klasse (skalenmittig transformierte Scores):\n")
  print(round(means_df, 3))

  cat("\nKlassengroessen + Posterior:\n")
  print(class_tbl %>% mutate(avg_posterior = round(avg_posterior, 3)))

  suggested <- suggest_class_labels(means_df, cfg$short)
  names(suggested) <- as.character(seq_len(K))

  cat("\n-> Vorschlag fuer class_labels:\n")
  cat(study, "= c(\n", sep = "")
  for (k in names(suggested)) {
    comma <- ifelse(k != names(suggested)[length(suggested)], ",", "")
    cat(sprintf('  "%s" = "%s"%s\n', k, suggested[k], comma))
  }
  cat("),\n")

  cat("\nGespeichert:\n")
  cat("  -> ", file.path(DIR_MODELS, paste0("lpa_final_", study, ".rds")), "\n", sep = "")
  cat("  -> ", file.path(DIR_TABLES_FINAL, paste0("lpa_assignment_", study, ".csv")), "\n", sep = "")
  cat("  -> ", file.path(DIR_TABLES_FINAL, paste0("lpa_estimates_", study, ".csv")), "\n", sep = "")
  cat("  -> ", file.path(DIR_TABLES_FINAL, paste0("lpa_multi_init_runs_", study, ".csv")), "\n", sep = "")
}

bind_rows(all_multi_init_summary) %>%
  write_csv(file.path(DIR_TABLES_FINAL, "lpa_multi_init_summary_all.csv"))

cat("\n============================================================\n")
cat("Finale Multi-Init-Schaetzung abgeschlossen.\n")
cat("Die Assignment-Dateien enthalten lpa_class, lpa_posterior und posterior_class_*.\n")
cat("Naechster Schritt: 03_B1 Stabilitaetspruefung und danach 03_C Plots/Tabellen.\n")
cat("============================================================\n")
