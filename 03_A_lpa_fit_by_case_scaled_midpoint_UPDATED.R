###############################################################################
## DISS_2026 — 03_A_lpa_fit_by_case_scaled_midpoint.R
## LPA Schritt 1: Fit-Indizes zur Bestimmung der Klassenanzahl
##
## Replikation der LPA mit skalenmittig transformierter Operationalisierung:
##   5er-Items: (x - 3) / 2; 4er-Items: (x - 2.5) / 1.5;
##   Konstruktwerte = Mittelwerte der itemweise transformierten Items
##
## Modelle:   tidyLPA Model 1, 2, 3, 6 (laenderspezifisch)
##            Benannt nach Varianz-/Kovarianzstruktur
## Output:    Fit-Tabellen + Elbow-Diagnose + Plots
##            -> Grundlage fuer manuelle Loesung in 03_B
##
## Input:  data/processed/cses5/derived/
##         CSES5_20262301_operationalised_scaled_midpoint.rds
## Output: outputs/tables/tidylpa_scaled_midpoint/fit/
##         outputs/plots/tidylpa_scaled_midpoint/fit/
##
## Wichtig: Dieses Skript ueberschreibt die Fit-Outputs der scaled_midpoint-LPA.
###############################################################################

suppressPackageStartupMessages({
  library(tidyverse)
  library(tidyLPA)
  library(here)
})

set.seed(20262301)

OVERWRITE_OUTPUTS <- TRUE

###############################################################################
## 0) PFADE
###############################################################################

DIR_DERIVED    <- here("data", "processed", "cses5", "derived")
PATH_IN        <- file.path(DIR_DERIVED,
                            "CSES5_20262301_operationalised_scaled_midpoint.rds")

DIR_TABLES_FIT <- here("outputs", "tables", "tidylpa_scaled_midpoint", "fit")
DIR_PLOTS_FIT  <- here("outputs", "plots",  "tidylpa_scaled_midpoint", "fit")

for (d in c(DIR_TABLES_FIT, DIR_PLOTS_FIT)) {
  dir.create(d, recursive = TRUE, showWarnings = FALSE)
}

if (OVERWRITE_OUTPUTS) {
  unlink(list.files(DIR_TABLES_FIT, full.names = TRUE), recursive = TRUE, force = TRUE)
  unlink(list.files(DIR_PLOTS_FIT,  full.names = TRUE), recursive = TRUE, force = TRUE)
}

###############################################################################
## 1) MODELL-BEZEICHNUNGEN
##    Intern: tidyLPA-Modellnummern (1, 2, 3, 6)
##    Extern: qualitative Bezeichnung der Varianz-/Kovarianzstruktur
##    Model 4 + 5 von mclust generell nicht unterstuetzt
###############################################################################

model_labels <- c(
  "1" = "Gleiche Varianzen / Keine Kovarianzen",
  "2" = "Freie Varianzen / Keine Kovarianzen",
  "3" = "Gleiche Varianzen / Gleiche Kovarianzen",
  "6" = "Freie Varianzen / Freie Kovarianzen"
)

# Langform fuer Konsolen-Output
model_labels_long <- c(
  "1" = "Gleiche Varianzen, keine Kovarianzen",
  "2" = "Freie Varianzen, keine Kovarianzen",
  "3" = "Gleiche Varianzen, gleiche Kovarianzen",
  "6" = "Freie Varianzen, freie Kovarianzen"
)

###############################################################################
## 2) LAENDERSPEZIFISCHE SCORE-VARIABLEN
###############################################################################

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
## 3) EINSTELLUNGEN
##
##    study_models: laenderspezifisch
##    Erlaubte Modelle in tidyLPA/mclust nach Diagnostik:
##    AUT_2017: 1, 3
##    DEU_2021/GRC_2019: 1, 2, 3, 6
##
##    Workflow bei Konvergenzfehler:
##    -> Fehlermeldung zeigt betroffene Modelle
##    -> Modell aus dem jeweiligen Land entfernen
##    -> Script erneut laufen lassen
###############################################################################

G_MAX <- 5L

study_models <- list(
  AUT_2017 = c(1, 3),       # Modelle 2 + 6 konvergieren in AUT nur fuer G = 1
  DEU_2021 = c(1, 2, 3, 6),
  GRC_2019 = c(1, 2, 3, 6)
)

###############################################################################
## 4) FARBPALETTE
###############################################################################

# Intern nach Modellnummer, extern nach Label
pal_models <- c(
  "Gleiche Varianzen / Keine Kovarianzen"    = "#2166AC",
  "Freie Varianzen / Keine Kovarianzen"      = "#D6604D",
  "Gleiche Varianzen / Gleiche Kovarianzen"  = "#4DAC26",
  "Freie Varianzen / Freie Kovarianzen"      = "#8B008B"
)

###############################################################################
## 5) HILFSFUNKTIONEN
###############################################################################

# Modellnummern -> qualitative Labels
label_models <- function(model_nums) {
  model_labels[as.character(model_nums)]
}

# Dynamische Palette fuer vorhandene Modelle
get_palette <- function(model_nums) {
  lbls <- label_models(model_nums)
  pal_models[lbls[lbls %in% names(pal_models)]]
}

# Fit-Tabelle mit qualitativen Labels anreichern
enrich_fit_tbl <- function(fit_tbl) {
  fit_tbl %>%
    mutate(
      model_label = factor(
        model_labels[as.character(Model)],
        levels = unname(model_labels)
      )
    )
}

# Konvergenz-Check: bricht pro Land ab
check_convergence <- function(warns, study, models) {

  convergence_warns <- warns[
    str_detect(
      warns,
      regex(paste0("could not estimate|",
                   "did not converge|",
                   "singular|",
                   "failed"),
            ignore_case = TRUE)
    )
  ]

  if (length(convergence_warns) == 0) return(invisible(TRUE))

  cat("\n")
  cat("============================================================\n")
  cat("FEHLER: Konvergenzprobleme in", study, ":\n\n")
  for (w in convergence_warns) cat("  ->", w, "\n")

  failed_nums <- str_extract_all(
    convergence_warns,
    "(?<=model )\\d+"
  ) %>%
    unlist() %>%
    as.integer() %>%
    unique() %>%
    sort()

  remaining <- setdiff(models, failed_nums)

  cat("\nLoesungsvorschlag:\n")
  if (length(failed_nums) > 0) {
    failed_lbls <- model_labels_long[
      as.character(failed_nums)
    ]
    cat("  Nicht-konvergierte Modelle:\n")
    for (i in seq_along(failed_nums)) {
      cat(sprintf("    -> %s\n",
                  failed_lbls[i]))
    }
    cat("  study_models[[\"", study, "\"]] <- c(",
        paste(remaining, collapse = ", "), ")\n",
        sep = "")
  } else {
    cat("  study_models fuer", study,
        "reduzieren und erneut laufen lassen.\n")
  }
  cat("============================================================\n")

  stop(
    paste0("Konvergenzprobleme in ", study,
           ". Siehe Konsolen-Output oben."),
    call. = FALSE
  )
}

# Fit-Tabelle extrahieren — bricht bei NA in BIC ab
extract_fit_tbl <- function(lpa_obj, study) {

  raw <- tryCatch(
    get_fit(lpa_obj),
    error = function(e) {
      stop(
        "get_fit() fehlgeschlagen fuer ", study,
        ":\n", e$message,
        call. = FALSE
      )
    }
  )

  raw <- raw[, nchar(names(raw)) > 0, drop = FALSE]
  fit <- as_tibble(raw) %>% arrange(Model, Classes)

  na_bic <- fit %>%
    filter(is.na(BIC)) %>%
    mutate(
      lbl = paste0(
        model_labels[as.character(Model)],
        " | G = ", Classes
      )
    )

  if (nrow(na_bic) > 0) {
    cat("\n")
    cat("============================================================\n")
    cat("FEHLER: NA in BIC fuer", study,
        "— stiller Konvergenzfehler:\n")
    for (x in na_bic$lbl) cat("  ->", x, "\n")

    failed_models <- unique(na_bic$Model)
    remaining     <- setdiff(
      study_models[[study]], failed_models
    )
    cat("\nLoesungsvorschlag:\n")
    cat("  study_models[[\"", study, "\"]] <- c(",
        paste(remaining, collapse = ", "), ")\n",
        sep = "")
    cat("============================================================\n")
    stop(
      paste0("NA in BIC fuer ", study,
             ". Siehe Konsolen-Output oben."),
      call. = FALSE
    )
  }

  fit
}

# BIC-Differenzen je Modell
compute_elbow <- function(fit_tbl) {
  fit_tbl %>%
    filter(!is.na(BIC)) %>%
    arrange(Model, Classes) %>%
    group_by(Model) %>%
    mutate(
      BIC_diff     = lag(BIC) - BIC,
      BIC_diff_pct = BIC_diff /
        max(BIC_diff, na.rm = TRUE) * 100
    ) %>%
    ungroup()
}

###############################################################################
## 6) VISUALISIERUNG
###############################################################################

# --- 6A: Fit-Indizes (4 Panels) ---
plot_fit_indices <- function(fit_tbl, study_label,
                             model_nums) {

  df <- enrich_fit_tbl(fit_tbl) %>%
    select(model_label, Classes,
           any_of(c("AIC", "BIC",
                    "SABIC", "Entropy"))) %>%
    mutate(Classes = as.integer(Classes)) %>%
    pivot_longer(cols      = -c(model_label, Classes),
                 names_to  = "index",
                 values_to = "value") %>%
    filter(!is.na(value)) %>%
    mutate(
      index = recode(index, "Entropy" = "Entropie"),
      index = factor(
        index,
        levels = c("BIC", "AIC", "SABIC", "Entropie")
      )
    )

  pal <- get_palette(model_nums)

  ggplot(df,
         aes(x        = Classes,
             y        = value,
             group    = model_label,
             color    = model_label,
             linetype = model_label)) +
    geom_line(linewidth = 0.8) +
    geom_point(size = 2.2) +
    facet_wrap(~ index, scales = "free_y", ncol = 2) +
    scale_x_continuous(breaks = 1:G_MAX) +
    scale_color_manual(values = pal) +
    labs(
      title    = paste0(study_label,
                        " - Fit-Indizes (Schritt 1)"),
      subtitle = paste0(
        "G = 1:", G_MAX,
        " | niedrigere Werte bei BIC/AIC/SABIC = besser | ",
        "höhere Entropie = besser"),
      x        = "Anzahl der Klassen (G)",
      y        = "Wert",
      color    = "Varianzstruktur",
      linetype = "Varianzstruktur"
    ) +
    theme_minimal(base_size = 12) +
    theme(
      plot.title       = element_text(face = "bold"),
      plot.subtitle    = element_text(size  = 9,
                                      color = "grey40"),
      strip.text       = element_text(face = "bold"),
      legend.position  = "bottom",
      legend.title     = element_text(face = "bold"),
      panel.grid.minor = element_blank()
    ) +
    guides(color    = guide_legend(nrow = 2),
           linetype = guide_legend(nrow = 2))
}

# --- 6B: BIC-Differenzen ---
plot_bic_diff <- function(elbow_tbl, study_label,
                          model_nums) {

  df <- enrich_fit_tbl(elbow_tbl) %>%
    filter(!is.na(BIC_diff)) %>%
    mutate(
      transition = paste0("G=", Classes - 1,
                          "->", Classes),
      transition = factor(transition,
                          levels = unique(transition))
    )

  pal <- get_palette(model_nums)

  ggplot(df,
         aes(x     = transition,
             y     = BIC_diff,
             fill  = model_label,
             group = model_label)) +
    geom_col(position = "dodge", width = 0.65) +
    geom_text(
      aes(label = paste0(round(BIC_diff_pct, 0), "%")),
      position = position_dodge(width = 0.65),
      vjust    = -0.4,
      size     = 2.8,
      fontface = "bold"
    ) +
    scale_fill_manual(values = pal) +
    scale_y_continuous(
      expand = expansion(mult = c(0, 0.18))
    ) +
    labs(
      title    = paste0(study_label,
                        " - BIC-Verbesserung pro Schritt"),
      subtitle = paste0(
        "% = Anteil am größten BIC-Gewinn | ",
        "Elbow = deutlicher Rückgang des Zugewinns"),
      x    = "Übergang zwischen Klassenlösungen",
      y    = "BIC-Verbesserung",
      fill = "Varianzstruktur"
    ) +
    theme_minimal(base_size = 12) +
    theme(
      plot.title       = element_text(face = "bold"),
      plot.subtitle    = element_text(size  = 9,
                                      color = "grey40"),
      legend.position  = "bottom",
      legend.title     = element_text(face = "bold"),
      panel.grid.minor = element_blank()
    ) +
    guides(fill = guide_legend(nrow = 2))
}

# --- 6C: BIC-Kurve ---
plot_bic_curve <- function(fit_tbl, study_label,
                           model_nums) {

  df <- enrich_fit_tbl(fit_tbl) %>%
    filter(!is.na(BIC)) %>%
    mutate(Classes = as.integer(Classes))

  best <- df %>%
    group_by(model_label) %>%
    slice_min(BIC, n = 1, with_ties = FALSE) %>%
    ungroup()

  pal <- get_palette(model_nums)

  ggplot(df,
         aes(x     = Classes,
             y     = BIC,
             group = model_label,
             color = model_label)) +
    geom_line(linewidth = 0.8) +
    geom_point(size = 2.2) +
    geom_point(data  = best,
               shape = 18, size = 5,
               inherit.aes = TRUE) +
    geom_label(
      data        = best,
      aes(label   = paste0("G=", Classes)),
      size        = 3,
      nudge_y     = diff(range(df$BIC)) * 0.06,
      show.legend = FALSE
    ) +
    scale_x_continuous(breaks = 1:G_MAX) +
    scale_color_manual(values = pal) +
    labs(
      title    = paste0(study_label, " - BIC-Kurve"),
      subtitle = paste0(
        "Raute = BIC-Minimum | ",
        "sichtbarer Knick als zusätzlicher Orientierungspunkt"),
      x     = "Anzahl der Klassen (G)",
      y     = "BIC",
      color = "Varianzstruktur"
    ) +
    theme_minimal(base_size = 12) +
    theme(
      plot.title       = element_text(face = "bold"),
      plot.subtitle    = element_text(size  = 9,
                                      color = "grey40"),
      legend.position  = "bottom",
      legend.title     = element_text(face = "bold"),
      panel.grid.minor = element_blank()
    ) +
    guides(color = guide_legend(nrow = 2))
}

# --- 6D: Entropy ---
plot_entropy <- function(fit_tbl, study_label,
                         model_nums) {

  df <- enrich_fit_tbl(fit_tbl) %>%
    filter(!is.na(Entropy), Classes > 1) %>%
    mutate(Classes = as.integer(Classes))

  pal <- get_palette(model_nums)

  ggplot(df,
         aes(x     = Classes,
             y     = Entropy,
             group = model_label,
             color = model_label)) +
    geom_hline(yintercept = 0.80,
               linetype   = "dashed",
               color      = "grey50") +
    geom_line(linewidth = 0.8) +
    geom_point(size = 2.2) +
    annotate("text",
             x     = max(df$Classes),
             y     = 0.80,
             label = "Schwelle = 0.80",
             hjust = 1,
             vjust = -0.5,
             size  = 3,
             color = "grey40") +
    scale_x_continuous(breaks = 2:G_MAX) +
    scale_y_continuous(
      limits = c(
        min(0.5,
            min(df$Entropy, na.rm = TRUE) - 0.05),
        1.0
      ),
      breaks = seq(0.5, 1.0, 0.1)
    ) +
    scale_color_manual(values = pal) +
    labs(
      title    = paste0(study_label,
                        " - Entropie nach Klassenzahl"),
      subtitle = "Gestrichelte Linie = empfohlene Schwelle von 0.80",
      x        = "Anzahl der Klassen (G)",
      y        = "Entropie",
      color    = "Varianzstruktur"
    ) +
    theme_minimal(base_size = 12) +
    theme(
      plot.title       = element_text(face = "bold"),
      plot.subtitle    = element_text(size  = 9,
                                      color = "grey40"),
      legend.position  = "bottom",
      legend.title     = element_text(face = "bold"),
      panel.grid.minor = element_blank()
    ) +
    guides(color = guide_legend(nrow = 2))
}

###############################################################################
## 7) DATEN LADEN + VALIDIERUNG
###############################################################################

if (!file.exists(PATH_IN)) {
  stop(
    "Operationalised RDS nicht gefunden: ", PATH_IN, "\n",
    "Bitte zuerst 02_D_operationalisation_build_scaled_midpoint_safe.R ausfuehren.",
    call. = FALSE
  )
}

df_full <- readRDS(PATH_IN)
stopifnot("election_study_id" %in% names(df_full))

target_studies <- names(study_vars)

cat("Alle Studien im Datensatz:\n")
print(table(df_full$election_study_id))

df_full <- df_full %>%
  filter(election_study_id %in% target_studies)

cat("\nNach Filter (operationalisierte Studien):\n")
print(table(df_full$election_study_id))

for (study in target_studies) {
  missing <- setdiff(study_vars[[study]]$vars,
                     names(df_full))
  if (length(missing) > 0) {
    stop(
      "Fehlende Variablen fuer ", study, ": ",
      paste(missing, collapse = ", "),
      call. = FALSE
    )
  }
}

cat("\nLaenderspezifische Varianzstruktur-Modelle:\n")
for (s in target_studies) {
  lbls <- model_labels_long[
    as.character(study_models[[s]])
  ]
  cat(sprintf("  %s:\n", s))
  for (l in lbls) cat(sprintf("    - %s\n", l))
}
cat("\nAlle scaled_midpoint-Score-Variablen vorhanden. Starte LPA...\n")

###############################################################################
## 8) HAUPTSCHLEIFE
###############################################################################

fit_results  <- list()
all_fit_tbls <- list()

for (study in target_studies) {

  cat("\n============================================================\n")
  cat("Fit-Indizes:", study, "\n")
  cat("============================================================\n")

  cfg         <- study_vars[[study]]
  study_label <- study_name_map[study]
  models      <- study_models[[study]]

  dat_study <- df_full %>%
    filter(election_study_id == study) %>%
    select(respondent_id, all_of(cfg$vars)) %>%
    drop_na()

  X <- dat_study %>%
    select(all_of(cfg$vars)) %>%
    rename_with(~ cfg$short)

  n_cc <- nrow(X)
  cat("N (vollständige Fälle):", n_cc, "\n")
  cat("Varianzstruktur-Modelle:\n")
  for (m in models) {
    cat(sprintf("  - %s\n",
                model_labels_long[as.character(m)]))
  }
  cat("G_MAX:", G_MAX, "\n\n")

  # ----------------------------------------------------------
  # A) Modelle schaetzen
  # ----------------------------------------------------------
  cat("Schaetze Modelle...\n")

  warns   <- character(0)
  lpa_all <- withCallingHandlers(
    X %>%
      estimate_profiles(
        n_profiles = 1:G_MAX,
        models     = models
      ),
    warning = function(w) {
      warns <<- c(warns, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )

  check_convergence(warns, study, models)

  # ----------------------------------------------------------
  # B) Fit-Tabelle
  # ----------------------------------------------------------
  fit_tbl   <- extract_fit_tbl(lpa_all, study)
  elbow_tbl <- compute_elbow(fit_tbl)

  # ----------------------------------------------------------
  # C) Konsolen-Output
  # ----------------------------------------------------------
  cat("\n--- Fit-Indizes ---\n")

  fit_tbl_print <- fit_tbl %>%
    mutate(
      Varianzstruktur = model_labels[
        as.character(Model)
      ]
    ) %>%
    select(Varianzstruktur, Classes,
           any_of(c("AIC", "BIC",
                    "SABIC", "Entropy"))) %>%
    rename(
      Klassen = Classes,
      Entropie = Entropy
    ) %>%
    mutate(across(where(is.numeric),
                  ~ round(., 3)))

  print(fit_tbl_print, n = Inf)

  cat("\n--- BIC-Differenzen ---\n")

  elbow_print <- elbow_tbl %>%
    filter(!is.na(BIC_diff)) %>%
    mutate(
      Varianzstruktur = model_labels[
        as.character(Model)
      ]
    ) %>%
    select(Varianzstruktur, Classes,
           BIC, BIC_diff, BIC_diff_pct) %>%
    rename(
      Klassen = Classes,
      BIC_Differenz = BIC_diff,
      BIC_Differenz_Prozent = BIC_diff_pct
    ) %>%
    mutate(across(where(is.numeric),
                  ~ round(., 1)))

  print(elbow_print, n = Inf)

  cat("\n--- Elbow-Empfehlung (50%-Schwelle) ---\n")
  for (m in sort(unique(fit_tbl$Model))) {

    lbl      <- model_labels_long[as.character(m)]
    m_tbl    <- elbow_tbl %>%
      filter(Model == m, !is.na(BIC_diff))
    max_jump <- max(m_tbl$BIC_diff, na.rm = TRUE)

    elbow_g <- m_tbl %>%
      filter(BIC_diff < 0.50 * max_jump) %>%
      slice(1) %>%
      pull(Classes)

    best_g <- fit_tbl %>%
      filter(Model == m) %>%
      slice_min(BIC, n = 1,
                with_ties = FALSE) %>%
      pull(Classes)

    entropy_at_elbow <- if (length(elbow_g) > 0) {
      fit_tbl %>%
        filter(Model == m,
               Classes == elbow_g) %>%
        pull(Entropy) %>%
        round(3)
    } else NA

    cat(sprintf(
      paste0("  %s:\n",
             "    BIC-Minimum = G=%d | ",
             "Elbow (50%%) = G=%s | ",
             "Entropie am Elbow = %s\n"),
      lbl,
      best_g,
      ifelse(length(elbow_g) == 0, "-",
             as.character(elbow_g)),
      ifelse(is.na(entropy_at_elbow), "-",
             as.character(entropy_at_elbow))
    ))
  }

  cat("\n-> Eintragen in 03_B_lpa_multi_init_scaled_midpoint.R:\n")
  cat("  ", study,
      "= list(model = ?, classes = ?),\n")

  # ----------------------------------------------------------
  # D) Plots
  # ----------------------------------------------------------
  p_fit  <- plot_fit_indices(fit_tbl,  study_label,
                             models)
  p_diff <- plot_bic_diff(elbow_tbl,   study_label,
                          models)
  p_bic  <- plot_bic_curve(fit_tbl,    study_label,
                           models)
  p_ent  <- plot_entropy(fit_tbl,      study_label,
                         models)

  ggsave(
    file.path(DIR_PLOTS_FIT,
              paste0("fit_indices_", study, ".png")),
    p_fit,  width = 12, height = 8, dpi = 300
  )
  ggsave(
    file.path(DIR_PLOTS_FIT,
              paste0("bic_diff_",   study, ".png")),
    p_diff, width = 12, height = 6, dpi = 300
  )
  ggsave(
    file.path(DIR_PLOTS_FIT,
              paste0("bic_curve_",  study, ".png")),
    p_bic,  width = 10, height = 6, dpi = 300
  )
  ggsave(
    file.path(DIR_PLOTS_FIT,
              paste0("entropy_",    study, ".png")),
    p_ent,  width = 10, height = 6, dpi = 300
  )

  cat("\nPlots gespeichert fuer", study, "\n")

  # ----------------------------------------------------------
  # E) Tabellen + RDS speichern
  # ----------------------------------------------------------
  write_csv(
    fit_tbl %>%
      mutate(
        study              = study,
        n                  = n_cc,
        variance_structure = model_labels[
          as.character(Model)
        ]
      ) %>%
      select(study, n, variance_structure,
             Classes,
             any_of(c("AIC", "BIC",
                      "SABIC", "Entropy"))),
    file.path(DIR_TABLES_FIT,
              paste0("fit_indices_", study, ".csv"))
  )

  write_csv(
    elbow_tbl %>%
      filter(!is.na(BIC_diff)) %>%
      mutate(
        study              = study,
        n                  = n_cc,
        variance_structure = model_labels[
          as.character(Model)
        ]
      ) %>%
      select(study, n, variance_structure,
             Classes, BIC,
             BIC_diff, BIC_diff_pct),
    file.path(DIR_TABLES_FIT,
              paste0("bic_diff_",   study, ".csv"))
  )

  saveRDS(
    lpa_all,
    file.path(DIR_TABLES_FIT,
              paste0("lpa_all_",    study, ".rds"))
  )

  fit_results[[study]] <- list(
    fit_tbl   = fit_tbl,
    elbow_tbl = elbow_tbl,
    lpa_all   = lpa_all,
    n_cc      = n_cc
  )

  all_fit_tbls[[study]] <- fit_tbl %>%
    mutate(
      study              = study,
      n                  = n_cc,
      variance_structure = model_labels[
        as.character(Model)
      ]
    )
}

###############################################################################
## 9) KOMBINIERTE TABELLE ALLE LAENDER
###############################################################################

all_fit_combined <- bind_rows(all_fit_tbls) %>%
  select(study, n, variance_structure, Classes,
         any_of(c("AIC", "BIC", "SABIC", "Entropy"))) %>%
  mutate(across(where(is.numeric), ~ round(., 3)))

write_csv(
  all_fit_combined,
  file.path(DIR_TABLES_FIT,
            "fit_indices_ALL_studies.csv")
)

###############################################################################
## 10) FERTIG
###############################################################################

cat("\n============================================================\n")
cat("Fit-Index-Analyse abgeschlossen.\n\n")
cat("Tabellen: ", DIR_TABLES_FIT, "\n")
cat("Plots:    ", DIR_PLOTS_FIT,  "\n\n")
cat("Kombinierte Fit-Tabelle (alle Laender):\n")
print(all_fit_combined, n = Inf)
cat("\n")
cat("NAECHSTER SCHRITT:\n")
cat("  1) Fit-Plots und BIC-Differenzen inspizieren\n")
cat("  2) Loesung je Land auswaehlen\n")
cat("  3) In 03_B_lpa_multi_init_scaled_midpoint.R eintragen:\n")
for (s in target_studies) {
  cat("    ", s,
      "= list(model = ?, classes = ?),\n")
}
cat("============================================================\n")
