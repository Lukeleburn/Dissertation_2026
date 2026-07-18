###############################################################################
## DISS_2026 — 03_B1_lpa_stability_scaled_midpoint.R
## LPA Stabilitaetspruefung fuer scaled-midpoint Operationalisierung
##
## Zweck:
##   Prueft, ob die in 03_B gewaehlten finalen LPA-Loesungen stabil bleiben,
##   wenn dieselbe Modell-Klassen-Kombination mehrfach geschaetzt wird.
##
## Wichtig:
##   tidyLPA/mclust fuehrt keine Mplus-artige Multi-Start-Diagnostik aus.
##   Diese Datei prueft deshalb zwei praktische Stabilitaetsaspekte:
##     1) Seed-Stabilitaet ohne Datenveraenderung
##     2) Jitter-Stabilitaet mit minimal gestoerten Scores
##
## Output:
##   outputs/tables/tidylpa_scaled_midpoint/stability/
##   outputs/plots/tidylpa_scaled_midpoint/stability/
##
## Dieses Skript ueberschreibt nur die Stabilitaetsoutputs. Die finalen
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

## Falls die Pruefung schneller laufen soll, diese Werte reduzieren.
N_REPS_SEED_ONLY <- 20L
N_REPS_JITTER    <- 50L
JITTER_SD        <- 0.01

BASE_SEED <- 20262301L

###############################################################################
## 1) PFADE
###############################################################################

DIR_DERIVED <- here("data", "processed", "cses5", "derived")
PATH_IN <- file.path(
  DIR_DERIVED,
  "CSES5_20262301_operationalised_scaled_midpoint.rds"
)

DIR_MODELS <- here("outputs", "models", "tidylpa_scaled_midpoint")
DIR_FINAL  <- here("outputs", "tables", "tidylpa_scaled_midpoint", "final")

DIR_TABLES <- here("outputs", "tables",
                   "tidylpa_scaled_midpoint", "stability")
DIR_PLOTS  <- here("outputs", "plots",
                   "tidylpa_scaled_midpoint", "stability")

for (d in c(DIR_TABLES, DIR_PLOTS)) {
  dir.create(d, recursive = TRUE, showWarnings = FALSE)
}

if (OVERWRITE_OUTPUTS) {
  unlink(list.files(DIR_TABLES, full.names = TRUE), recursive = TRUE, force = TRUE)
  unlink(list.files(DIR_PLOTS,  full.names = TRUE), recursive = TRUE, force = TRUE)
}

###############################################################################
## 2) MODELL- UND VARIABLENDEFINITIONEN
###############################################################################

chosen_solution <- list(
  AUT_2017 = list(model = 3, classes = 4),
  DEU_2021 = list(model = 2, classes = 3),
  GRC_2019 = list(model = 2, classes = 3)
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

study_labels <- c(
  AUT_2017 = "Österreich 2017",
  DEU_2021 = "Deutschland 2021",
  GRC_2019 = "Griechenland 2019"
)

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

extract_means <- function(mod, short_names, classes_total) {
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

extract_fit <- function(mod) {
  fit <- suppressWarnings(get_fit(mod))

  tibble(
    AIC = pick_fit_value(fit, "AIC"),
    BIC = pick_fit_value(fit, "BIC"),
    SABIC = pick_fit_value(fit, "SABIC"),
    Entropy = pick_fit_value(fit, "Entropy")
  )
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

run_lpa_once <- function(X, sol, seed, jitter_sd) {
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
        n_profiles = sol$classes,
        models = sol$model
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

###############################################################################
## 4) DATEN LADEN
###############################################################################

if (!file.exists(PATH_IN)) {
  stop("Input-RDS nicht gefunden: ", PATH_IN, call. = FALSE)
}

df_full <- readRDS(PATH_IN) %>%
  filter(election_study_id %in% names(chosen_solution))

###############################################################################
## 5) STABILITAETSPRUEFUNG
###############################################################################

scenario_tbl <- make_scenarios()

all_runs <- list()
all_means <- list()
all_class_sizes <- list()
all_matches <- list()

for (study in names(chosen_solution)) {

  cat("\n")
  cat("============================================================\n")
  cat("Stabilitaetspruefung:", study, "\n")
  cat("============================================================\n")

  cfg <- study_vars[[study]]
  sol <- chosen_solution[[study]]
  k <- sol$classes

  dat_study <- df_full %>%
    filter(election_study_id == study) %>%
    select(respondent_id, all_of(cfg$vars)) %>%
    drop_na()

  X <- dat_study %>%
    select(all_of(cfg$vars)) %>%
    rename_with(~ cfg$short)

  n_cc <- nrow(X)

  ref_assign_path <- file.path(
    DIR_FINAL,
    paste0("lpa_assignment_", study, ".csv")
  )
  ref_model_path <- file.path(
    DIR_MODELS,
    paste0("lpa_final_", study, ".rds")
  )

  if (!file.exists(ref_assign_path)) {
    stop("Referenz-Assignment nicht gefunden: ",
         ref_assign_path, call. = FALSE)
  }
  if (!file.exists(ref_model_path)) {
    stop("Referenzmodell nicht gefunden: ",
         ref_model_path, call. = FALSE)
  }

  ref_assign <- read_csv(ref_assign_path,
                         show_col_types = FALSE) %>%
    pull(lpa_class)

  ref_mod <- readRDS(ref_model_path)
  ref_means <- extract_means(ref_mod, cfg$short, k)

  cat("Referenzloesung:\n")
  cat("  Modell: ", model_labels[as.character(sol$model)], "\n", sep = "")
  cat("  Klassen:", k, "\n")
  cat("  N:      ", n_cc, "\n")
  cat("  Replikationen:",
      nrow(scenario_tbl), "\n\n")

  for (i in seq_len(nrow(scenario_tbl))) {
    sc <- scenario_tbl[i, ]

    cat(sprintf(
      "  [%s] %03d/%03d | seed=%d | jitter_sd=%.3f\n",
      study, i, nrow(scenario_tbl), sc$seed, sc$jitter_sd
    ))

    result <- tryCatch(
      run_lpa_once(
        X = X,
        sol = sol,
        seed = sc$seed,
        jitter_sd = sc$jitter_sd
      ),
      error = function(e) e
    )

    run_key <- paste(study, sc$scenario, sc$rep_id, sep = "_")

    if (inherits(result, "error")) {
      all_runs[[run_key]] <- tibble(
        study = study,
        study_label = study_labels[study],
        scenario = sc$scenario,
        scenario_label = sc$scenario_label,
        rep_id = sc$rep_id,
        seed = sc$seed,
        jitter_sd = sc$jitter_sd,
        model = sol$model,
        variance_structure = model_labels[as.character(sol$model)],
        classes_total = k,
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
    rep_means <- extract_means(mod, cfg$short, k)
    class_match <- match_classes_by_means(ref_means, rep_means)

    class_map <- setNames(class_match$ref_class,
                          class_match$rep_class)
    aligned_class <- as.integer(class_map[as.character(class_assign)])

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

    all_runs[[run_key]] <- tibble(
      study = study,
      study_label = study_labels[study],
      scenario = sc$scenario,
      scenario_label = sc$scenario_label,
      rep_id = sc$rep_id,
      seed = sc$seed,
      jitter_sd = sc$jitter_sd,
      model = sol$model,
      variance_structure = model_labels[as.character(sol$model)],
      classes_total = k,
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

    all_means[[run_key]] <- as_tibble(
      means_aligned,
      rownames = "ref_class"
    ) %>%
      pivot_longer(
        cols = all_of(cfg$short),
        names_to = "construct",
        values_to = "estimate"
      ) %>%
      mutate(
        study = study,
        study_label = study_labels[study],
        scenario = sc$scenario,
        scenario_label = sc$scenario_label,
        rep_id = sc$rep_id,
        seed = sc$seed,
        ref_class = as.integer(ref_class)
      ) %>%
      relocate(study, study_label, scenario, scenario_label,
               rep_id, seed, ref_class, construct)

    all_class_sizes[[run_key]] <- tibble(
      rep_class = class_assign
    ) %>%
      mutate(ref_class = as.integer(
        class_map[as.character(rep_class)]
      )) %>%
      dplyr::count(ref_class, name = "n") %>%
      complete(ref_class = seq_len(k),
               fill = list(n = 0L)) %>%
      mutate(
        pct = round(n / n_cc * 100, 1),
        study = study,
        study_label = study_labels[study],
        scenario = sc$scenario,
        scenario_label = sc$scenario_label,
        rep_id = sc$rep_id,
        seed = sc$seed
      ) %>%
      relocate(study, study_label, scenario, scenario_label,
               rep_id, seed, ref_class)

    all_matches[[run_key]] <- class_match %>%
      mutate(
        study = study,
        study_label = study_labels[study],
        scenario = sc$scenario,
        scenario_label = sc$scenario_label,
        rep_id = sc$rep_id,
        seed = sc$seed
      ) %>%
      relocate(study, study_label, scenario, scenario_label,
               rep_id, seed)
  }
}

###############################################################################
## 6) OUTPUT-TABELLEN
###############################################################################

run_tbl <- bind_rows(all_runs)
means_tbl <- bind_rows(all_means)
size_tbl <- bind_rows(all_class_sizes)
match_tbl <- bind_rows(all_matches)

summary_tbl <- run_tbl %>%
  group_by(study, study_label, scenario, scenario_label) %>%
  summarise(
    n_runs = n(),
    n_converged = sum(converged, na.rm = TRUE),
    pct_converged = round(n_converged / n_runs * 100, 1),
    ARI_mean = round(mean(ARI, na.rm = TRUE), 3),
    ARI_min = round(min(ARI, na.rm = TRUE), 3),
    ARI_max = round(max(ARI, na.rm = TRUE), 3),
    mean_class_distance = round(
      mean(mean_class_distance, na.rm = TRUE), 3
    ),
    max_class_distance = round(
      max(max_class_distance, na.rm = TRUE), 3
    ),
    BIC_mean = round(mean(BIC, na.rm = TRUE), 1),
    Entropy_mean = round(mean(Entropy, na.rm = TRUE), 3),
    .groups = "drop"
  )

write_csv(run_tbl, file.path(DIR_TABLES, "lpa_stability_runs.csv"))
write_csv(means_tbl, file.path(DIR_TABLES, "lpa_stability_means_long.csv"))
write_csv(size_tbl, file.path(DIR_TABLES, "lpa_stability_class_sizes.csv"))
write_csv(match_tbl, file.path(DIR_TABLES, "lpa_stability_class_matching.csv"))
write_csv(summary_tbl, file.path(DIR_TABLES, "lpa_stability_summary.csv"))

###############################################################################
## 7) PLOTS
###############################################################################

theme_stability <- function(base_size = 12) {
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
  ggplot(aes(x = scenario_label, y = ARI, color = scenario_label)) +
  geom_boxplot(width = 0.55, outlier.alpha = 0.5) +
  geom_jitter(width = 0.08, alpha = 0.35, size = 1.2) +
  facet_wrap(~ study_label, nrow = 1) +
  coord_cartesian(ylim = c(0, 1)) +
  labs(
    title = "Stabilitaet der LPA-Klassenzuordnung",
    subtitle = "Adjusted Rand Index gegenueber der finalen Referenzloesung",
    x = NULL,
    y = "Adjusted Rand Index",
    color = NULL,
    caption = "ARI = 1 bedeutet identische Klassenzuordnung; die Klassennummern wurden vor dem Vergleich ueber die Profilmittelwerte ausgerichtet."
  ) +
  theme_stability() +
  theme(axis.text.x = element_blank())

ggsave(
  file.path(DIR_PLOTS, "lpa_stability_ari.png"),
  p_ari,
  width = 12,
  height = 6,
  dpi = 320
)

p_distance <- run_tbl %>%
  filter(converged) %>%
  ggplot(aes(x = scenario_label,
             y = mean_class_distance,
             color = scenario_label)) +
  geom_boxplot(width = 0.55, outlier.alpha = 0.5) +
  geom_jitter(width = 0.08, alpha = 0.35, size = 1.2) +
  facet_wrap(~ study_label, nrow = 1) +
  labs(
    title = "Stabilitaet der Profilmittelwerte",
    subtitle = "Mittlere Distanz der replizierten Profilzentren zur Referenzloesung",
    x = NULL,
    y = "Mittlere Distanz",
    color = NULL
  ) +
  theme_stability() +
  theme(axis.text.x = element_blank())

ggsave(
  file.path(DIR_PLOTS, "lpa_stability_mean_distances.png"),
  p_distance,
  width = 12,
  height = 6,
  dpi = 320
)

p_fit <- run_tbl %>%
  filter(converged) %>%
  select(study_label, scenario_label, rep_id,
         BIC, AIC, SABIC, Entropy) %>%
  pivot_longer(
    cols = c(BIC, AIC, SABIC, Entropy),
    names_to = "fit_index",
    values_to = "value"
  ) %>%
  mutate(
    fit_index = recode(
      fit_index,
      Entropy = "Entropie"
    )
  ) %>%
  ggplot(aes(x = scenario_label, y = value,
             color = scenario_label)) +
  geom_boxplot(width = 0.55, outlier.alpha = 0.5) +
  facet_grid(fit_index ~ study_label, scales = "free_y") +
  labs(
    title = "Stabilitaet der Fit-Indizes",
    subtitle = "Streuung der Fit-Indizes ueber wiederholte Schaetzungen",
    x = NULL,
    y = "Wert",
    color = NULL
  ) +
  theme_stability() +
  theme(axis.text.x = element_blank())

ggsave(
  file.path(DIR_PLOTS, "lpa_stability_fit_indices.png"),
  p_fit,
  width = 12,
  height = 8,
  dpi = 320
)

###############################################################################
## 8) KONSOLENZUSAMMENFASSUNG
###############################################################################

cat("\n")
cat("============================================================\n")
cat("Stabilitaetspruefung abgeschlossen.\n\n")
cat("Zusammenfassung:\n")
print(summary_tbl, n = Inf)
cat("\nTabellen:\n  ", DIR_TABLES, "\n", sep = "")
cat("Plots:\n  ", DIR_PLOTS, "\n", sep = "")
cat("============================================================\n")
