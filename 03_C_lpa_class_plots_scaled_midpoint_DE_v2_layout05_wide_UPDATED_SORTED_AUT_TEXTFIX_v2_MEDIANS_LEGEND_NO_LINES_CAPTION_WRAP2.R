###############################################################################
## DISS_2026 — 03_C_lpa_class_plots_scaled_midpoint_DE_v2_layout05_wide_UPDATED_SORTED.R
## LPA Schritt 2b: Klassenbenennung + deutschsprachige Plots + Tabellen
## scaled_midpoint-Version
## Update: Größenbalken absteigend; Profilachsen in festgelegter Konstrukt-Reihenfolge
##
## Voraussetzung: 03_B_lpa_multi_init_scaled_midpoint.R wurde ausgefuehrt
## und hat die finalen Multi-Init-Assignments inklusive posterior_class_* erzeugt.
##
## Gewichtung: LPA ungewichtet (Standard)
##             Deskriptive Statistiken:
##             - ungewichtet
##             - gewichtet (demographic)
##             - AUT zusaetzlich: political
##
## Input:  outputs/models/tidylpa_scaled_midpoint/lpa_final_*.rds
##         outputs/tables/tidylpa_scaled_midpoint/fit/fit_indices_*.csv
##         outputs/tables/tidylpa_scaled_midpoint/final/lpa_assignment_*.csv
##         data/processed/cses5/derived/
##         CSES5_20262301_operationalised_scaled_midpoint.rds
##
## Output: outputs/plots/tidylpa_scaled_midpoint/plots/{LAND}/...
##         outputs/tables/tidylpa_scaled_midpoint/plots/
###############################################################################

suppressPackageStartupMessages({
  library(tidyverse)
  library(tidyLPA)
  library(cowplot)
  library(scales)
  library(here)
})

OVERWRITE_OUTPUTS <- TRUE

###############################################################################
## 0) PFADE
###############################################################################

DIR_DERIVED      <- here("data", "processed",
                         "cses5", "derived")
PATH_IN          <- file.path(
  DIR_DERIVED,
  "CSES5_20262301_operationalised_scaled_midpoint.rds"
)

DIR_TABLES_FIT   <- here("outputs", "tables",
                         "tidylpa_scaled_midpoint", "fit")
DIR_MODELS       <- here("outputs", "models",
                         "tidylpa_scaled_midpoint")
DIR_PLOTS        <- here("outputs", "plots",
                         "tidylpa_scaled_midpoint", "plots")
DIR_TABLES_FINAL_IN <- here("outputs", "tables",
                            "tidylpa_scaled_midpoint", "final")
DIR_TABLES_FINAL <- here("outputs", "tables",
                         "tidylpa_scaled_midpoint",
                         "plots")

for (d in c(DIR_PLOTS, DIR_TABLES_FINAL)) {
  dir.create(d, recursive = TRUE, showWarnings = FALSE)
}

if (OVERWRITE_OUTPUTS) {
  unlink(list.files(DIR_TABLES_FINAL, full.names = TRUE), recursive = TRUE, force = TRUE)
  unlink(list.files(DIR_PLOTS,        full.names = TRUE), recursive = TRUE, force = TRUE)
}

###############################################################################
## 1) MODELL-BEZEICHNUNGEN
###############################################################################

model_labels <- c(
  "1" = "Gleiche Varianzen / Keine Kovarianzen",
  "2" = "Freie Varianzen / Keine Kovarianzen",
  "3" = "Gleiche Varianzen / Gleiche Kovarianzen",
  "6" = "Freie Varianzen / Freie Kovarianzen"
)

model_labels_long <- c(
  "1" = "gleiche Varianzen, keine Kovarianzen",
  "2" = "freie Varianzen, keine Kovarianzen",
  "3" = "gleiche Varianzen, gleiche Kovarianzen",
  "6" = "freie Varianzen, freie Kovarianzen"
)

###############################################################################
## 2) KLASSENNAMEN
###############################################################################

class_labels <- list(

  AUT_2017 = c(
    "1" = "exklusionistisch und anti-elitär geneigte Grauzone",
    "2" = "anti-anti-elitäre Minimalpragmatiker",
    "3" = "anti-exklusionistische Minimalpragmatiker",
    "4" = "anti-elitäre und exklusionistische Minimalpragmatiker"
  ),

  DEU_2021 = c(
    "1" = "anti-populistische Pragmatiker",
    "2" = "exklusionistisch neigende Populisten",
    "3" = "pragmatisch geneigte Grauzone"
  ),

  GRC_2019 = c(
    "1" = "Pragmatisten",
    "2" = "populistisch geneigte Grauzone",
    "3" = "exklusionistisch geneigte und anti-pragmatische Populisten"
  )
)

###############################################################################
## 3) KONFIGURATION
###############################################################################

studies <- c("AUT_2017", "DEU_2021", "GRC_2019")

study_name_map <- c(
  AUT_2017 = "Österreich 2017",
  DEU_2021 = "Deutschland 2021",
  GRC_2019 = "Griechenland 2019"
)

chosen_solution <- list(
  AUT_2017 = list(model = 3, classes = 4),
  DEU_2021 = list(model = 2, classes = 3),
  GRC_2019 = list(model = 2, classes = 3)
)

study_vars <- list(
  # Gewünschte Reihenfolge in den Profilplots:
  # populistisch/anti-elitär -> pragmatisch -> exklusionistisch
  AUT_2017 = list(
    vars       = c("Anti_Elitist_score_scaled_midpoint",
                   "Minimal_PRG_score_scaled_midpoint",
                   "EXCL_score_scaled_midpoint"),
    short      = c("AE", "MPRG", "EXCL"),
    top_labels = c("Anti-elitäre Demokratie",
                   "Minimal-pragmatische Demokratie",
                   "Exklusionistische Demokratie"),
    bot_labels = c("Anti-anti-elitär",
                   "Anti-minimal-pragmatisch",
                   "Anti-exklusionistisch")
  ),
  DEU_2021 = list(
    vars       = c("POPU_score_scaled_midpoint",
                   "PRG_score_scaled_midpoint",
                   "EXCL_score_scaled_midpoint"),
    short      = c("POP", "PRG", "EXCL"),
    top_labels = c("Populistische Demokratie",
                   "Pragmatische Demokratie",
                   "Exklusionistische Demokratie"),
    bot_labels = c("Anti-populistisch",
                   "Anti-pragmatisch",
                   "Anti-exklusionistisch")
  ),
  GRC_2019 = list(
    vars       = c("POPU_score_scaled_midpoint",
                   "PRG_score_scaled_midpoint",
                   "EXCL_score_scaled_midpoint"),
    short      = c("POP", "PRG", "EXCL"),
    top_labels = c("Populistische Demokratie",
                   "Pragmatische Demokratie",
                   "Exklusionistische Demokratie"),
    bot_labels = c("Anti-populistisch",
                   "Anti-pragmatisch",
                   "Anti-exklusionistisch")
  )
)

# Weight-Konfiguration je Land
# demographic: alle Laender
# political:   nur AUT
weight_config <- list(
  AUT_2017 = c("unweighted",
               "weighted_demographic",
               "weighted_political"),
  DEU_2021 = c("unweighted",
               "weighted_demographic"),
  GRC_2019 = c("unweighted",
               "weighted_demographic")
)

weight_vars <- list(
  weighted_demographic =
    "weight_polity_DEMOGRAPHIC",
  weighted_political   =
    "weight_polity_POLITICAL"
)

###############################################################################
## 4) FARBPALETTE — angepasst an 05_A
###############################################################################

# Gleiche Profilfarben wie in den 05_A-Skripten.
# Die Namen stehen ohne Zeilenumbrueche; get_named_pal()
# ordnet sie den jeweils verwendeten Plotlabels stabil zu.
class_colors_05 <- c(
  # AUT_2017
  "exklusionistisch und anti-elitär geneigte Grauzone" = "#999999",
  "anti-anti-elitäre Minimalpragmatiker" = "#56B4E9",
  "anti-exklusionistische Minimalpragmatiker" = "#0072B2",
  "anti-elitäre und exklusionistische Minimalpragmatiker" = "#E69F00",

  # DEU_2021
  "anti-populistische Pragmatiker" = "#009E73",
  "exklusionistisch neigende Populisten" = "#D55E00",
  "pragmatisch geneigte Grauzone" = "#999999",

  # GRC_2019
  "Pragmatisten" = "#009E73",
  "populistisch geneigte Grauzone" = "#999999",
  "exklusionistisch geneigte und anti-pragmatische Populisten" = "#E69F00"
)

# Fallback, falls spaeter weitere Profile ergaenzt werden.
pal_fallback <- c(
  "#1B9E77", "#D95F02", "#7570B3", "#E7298A",
  "#66A61E", "#E6AB02", "#A6761D", "#666666"
)

normalise_profile_label <- function(x) {
  as.character(x) %>%
    stringr::str_replace_all("\\n", " ") %>%
    stringr::str_squish()
}

get_pal <- function(study) {
  levels_full <- class_labels[[study]]
  keys <- normalise_profile_label(levels_full)
  pal <- unname(class_colors_05[keys])

  miss <- is.na(pal)
  if (any(miss)) {
    pal[miss] <- pal_fallback[seq_len(sum(miss))]
  }

  pal
}

get_named_pal <- function(study) {
  levels <- get_label_levels(study)

  keys <- normalise_profile_label(levels)
  pal <- unname(class_colors_05[keys])
  miss <- is.na(pal)
  if (any(miss)) {
    pal[miss] <- pal_fallback[seq_len(sum(miss))]
  }

  setNames(pal, levels)
}

###############################################################################
## 5) VALIDIERUNG
###############################################################################

pending <- names(Filter(is.null, class_labels))

if (length(pending) > 0) {
  cat("\n")
  cat("============================================================\n")
  cat("STOPP: Klassennamen fehlen noch:\n\n")
  for (s in pending) {
    K <- chosen_solution[[s]]$classes
    cat(sprintf("  %s = c(\n", s))
    for (k in 1:K) {
      comma <- ifelse(k < K, ",", "")
      cat(sprintf('    "%d" = "..."%s\n',
                  k, comma))
    }
    cat("  ),\n\n")
  }
  cat("Bitte:\n")
  cat("  1) 03b1 Konsolen-Output pruefen\n")
  cat("  2) Namen in Sektion 2 eintragen\n")
  cat("  3) Script erneut laufen lassen\n")
  cat("============================================================\n")
  stop("Klassennamen fehlen. Siehe oben.",
       call. = FALSE)
}

cat("Alle Klassennamen gesetzt. Starte...\n\n")

###############################################################################
## 6) DATEN LADEN
###############################################################################

# Rohdaten fuer Weights
if (!file.exists(PATH_IN)) {
  stop("Rohdaten nicht gefunden: ",
       PATH_IN, call. = FALSE)
}
df_raw <- readRDS(PATH_IN)
cat("✓ Rohdaten geladen | N =",
    nrow(df_raw), "\n")

fit_list        <- list()
assignment_list <- list()

for (study in studies) {

  # Fit-Indizes laden
  fit_path <- file.path(
    DIR_TABLES_FIT,
    paste0("fit_indices_", study, ".csv")
  )
  if (file.exists(fit_path)) {
    fit_list[[study]] <- read_csv(
      fit_path, show_col_types = FALSE
    )
  }

  # Assignment laden
  # Weight- und Label-Spalten entfernen
  # falls bereits aus vorherigem Lauf
  # in CSV vorhanden -> verhindert
  # doppelte .x/.y Spalten
  assign_path <- file.path(
    DIR_TABLES_FINAL_IN,
    paste0("lpa_assignment_",
           study, ".csv")
  )
  if (!file.exists(assign_path)) {
    stop(
      "Assignment CSV nicht gefunden: ",
      assign_path,
      "\nBitte zuerst 03b1 ausfuehren.",
      call. = FALSE
    )
  }

  assignment_list[[study]] <- read_csv(
    assign_path,
    show_col_types = FALSE
  ) %>%
    select(-any_of(c(
      "lpa_class_label",
      "weight_polity_DEMOGRAPHIC",
      "weight_polity_POLITICAL",
      "weight_polity_DEMOGRAPHIC.x",
      "weight_polity_DEMOGRAPHIC.y",
      "weight_polity_POLITICAL.x",
      "weight_polity_POLITICAL.y"
    )))

  cat("✓ Geladen:", study,
      "| N =",
      nrow(assignment_list[[study]]),
      "\n")
}

# Weights aus Rohdaten joinen
for (study in studies) {

  weight_cols <- c(
    "respondent_id",
    "weight_polity_DEMOGRAPHIC"
  )
  if (study == "AUT_2017") {
    weight_cols <- c(
      weight_cols,
      "weight_polity_POLITICAL"
    )
  }

  weights_study <- df_raw %>%
    filter(election_study_id == study) %>%
    select(any_of(weight_cols))

  assignment_list[[study]] <-
    assignment_list[[study]] %>%
    left_join(weights_study,
              by = "respondent_id")

  # Pruefen ob Join erfolgreich
  joined_cols <- names(
    assignment_list[[study]]
  )
  missing_w <- setdiff(
    weight_cols[-1],
    joined_cols
  )
  if (length(missing_w) > 0) {
    warning(sprintf(
      "⚠ Weight-Spalten nach Join nicht",
      " gefunden in %s: %s",
      study,
      paste(missing_w, collapse = ", ")
    ))
  } else {
    cat(sprintf(
      "✓ Weights gejoined: %s | Spalten: %s\n",
      study,
      paste(weight_cols[-1],
            collapse = ", ")
    ))
  }
}

###############################################################################
## 7) PROFIL-MITTELWERTE BERECHNEN
##    Funktion fuer gewichtet + ungewichtet
###############################################################################

compute_profile_means <- function(
    study,
    weight_type = "unweighted") {

  cfg   <- study_vars[[study]]
  sol   <- chosen_solution[[study]]
  tbl   <- assignment_list[[study]]
  lbl   <- class_labels[[study]]
  K     <- sol$classes

  # Entropy + Fit: Doppelfilter
  fit_row <- fit_list[[study]] %>%
    filter(
      Classes == sol$classes,
      variance_structure ==
        model_labels[as.character(sol$model)]
    ) %>%
    slice(1)

  entropy_val <- as.numeric(fit_row$Entropy)

  # Gewichte vorbereiten
  if (weight_type == "unweighted") {
    tbl <- tbl %>%
      mutate(.w = 1)
  } else {
    wvar <- weight_vars[[weight_type]]
    if (!wvar %in% names(tbl)) {
      stop(
        sprintf(
          "Weight-Variable '%s' nicht in %s",
          wvar, study
        ),
        call. = FALSE
      )
    }
    tbl <- tbl %>%
      mutate(.w = .data[[wvar]]) %>%
      filter(!is.na(.w))
  }

  # Gewichtete Mittelwerte + SD
  means <- tbl %>%
    select(lpa_class, all_of(cfg$vars),
           .w) %>%
    group_by(lpa_class) %>%
    summarise(
      across(
        all_of(cfg$vars),
        list(
          mean = ~ weighted.mean(
            .x, w = .w, na.rm = TRUE),
          sd   = ~ sqrt(
            sum(.w * (.x - weighted.mean(
              .x, w = .w,
              na.rm = TRUE))^2,
              na.rm = TRUE) /
              sum(.w, na.rm = TRUE)
          )
        )
      ),
      n      = n(),
      n_wtd  = sum(.w, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      pct         = n_wtd /
        sum(n_wtd) * 100,
      class_label = lbl[
        as.character(lpa_class)
      ],
      study        = study,
      study_label  = study_name_map[study],
      weight_type  = weight_type,
      entropy      = entropy_val,
      BIC          = as.numeric(fit_row$BIC),
      AIC          = as.numeric(fit_row$AIC),
      SABIC        = as.numeric(fit_row$SABIC)
    )

  # Spalten umbenennen
  for (i in seq_along(cfg$vars)) {
    old_m <- paste0(cfg$vars[i], "_mean")
    old_s <- paste0(cfg$vars[i], "_sd")
    new_m <- paste0(cfg$short[i], "_mean")
    new_s <- paste0(cfg$short[i], "_sd")
    means <- means %>%
      rename(!!new_m := all_of(old_m),
             !!new_s := all_of(old_s))
  }

  means
}

# Alle Kombinationen berechnen
profile_means_all <- list()

for (study in studies) {
  for (wtype in weight_config[[study]]) {
    key <- paste0(study, "__", wtype)
    profile_means_all[[key]] <-
      compute_profile_means(study, wtype)
    cat(sprintf(
      "✓ Mittelwerte: %s | %s | Entropy = %.3f\n",
      study, wtype,
      unique(profile_means_all[[key]]$entropy)
    ))
  }
}

# Ungewichtete Mittelwerte als Hauptreferenz
profile_means_list <- list()
for (study in studies) {
  key <- paste0(study, "__unweighted")
  profile_means_list[[study]] <-
    profile_means_all[[key]]
}

###############################################################################
## 8) HILFSFUNKTIONEN
###############################################################################

get_label_levels <- function(study) {
  means <- profile_means_list[[study]]
  means %>%
    arrange(lpa_class) %>%
    pull(class_label)
}

wrap_profile_labels <- function(x, width = 24) {
  stringr::str_wrap(as.character(x), width = width)
}

wrap_dimension_labels <- function(x, width = 20) {
  x %>%
    as.character() %>%
    stringr::str_replace_all(
      c(
        "Anti-elitäre Demokratie" = "Anti-elitäre\nDemokratie",
        "Anti-anti-elitistisch" = "Anti-anti-\nelitistisch",
        "Minimal-pragmatische Demokratie" = "Minimal-pragmatische\nDemokratie",
        "Anti-minimal-pragmatisch" = "Anti-minimal-\npragmatisch",
        "Exklusionistische Demokratie" = "Exklusionistische\nDemokratie",
        "Anti-exklusionistisch" = "Anti-\nexklusionistisch",
        "Populistische Demokratie" = "Populistische\nDemokratie",
        "Anti-populistisch" = "Anti-\npopulistisch",
        "Pragmatische Demokratie" = "Pragmatische\nDemokratie",
        "Anti-pragmatisch" = "Anti-\npragmatisch"
      )
    )
}

# Weight-Label fuer Plot-Titel
weight_label <- c(
  "unweighted"           = "ungewichtet",
  "weighted_demographic" = "demografisch gewichtet",
  "weighted_political"   = "politisch gewichtet"
)

###############################################################################
## 9) PLOT-FUNKTIONEN
###############################################################################

# ----------------------------------------------------------
# 9A: Profilplot
# ----------------------------------------------------------

make_profile_plot <- function(
    study,
    means = NULL,
    wtype = "unweighted") {

  if (is.null(means)) {
    means <- profile_means_list[[study]]
  }

  cfg     <- study_vars[[study]]
  K       <- chosen_solution[[study]]$classes
  pal     <- get_named_pal(study)
  var_lbl <- model_labels_long[
    as.character(
      chosen_solution[[study]]$model
    )
  ]
  entropy <- unique(means$entropy)
  levels  <- get_label_levels(study)

  df_long <- means %>%
    select(
      lpa_class,
      class_label,
      ends_with("_mean")
    ) %>%
    pivot_longer(
      cols      = ends_with("_mean"),
      names_to  = "dimension",
      values_to = "mean_value"
    ) %>%
    mutate(
      dim_bot = factor(
        str_remove(dimension, "_mean"),
        levels = cfg$short,
        labels = wrap_dimension_labels(
          cfg$bot_labels,
          width = 18
        )
      ),
      class_label = factor(
        class_label,
        levels = levels
      )
    )

  y_min <- -1
  y_max <-  1

  # Österreich hat besonders lange Dimensionsbezeichnungen. Nur dort
  # werden die oberen Labels stärker umgebrochen und etwas kleiner gesetzt.
  top_labels_plot <- if (study == "AUT_2017") {
    c(
      "Anti-elitäre\nDemokratie",
      "Minimal-\npragmatische\nDemokratie",
      "Exklusionistische\nDemokratie"
    )
  } else {
    wrap_dimension_labels(
      cfg$top_labels,
      width = 20
    )
  }

  top_label_size <- if (study == "AUT_2017") 4.6 else 4.6
  bottom_label_size <- if (study == "AUT_2017") 13 else 15

  ann_df <- tibble(
    x     = seq_along(cfg$top_labels),
    label = top_labels_plot,
    y     = y_max + 0.11
  )

  wtd_n <- if ("n_wtd" %in% names(means)) {
    round(sum(means$n_wtd))
  } else {
    sum(means$n)
  }

  ggplot(
    df_long,
    aes(
      x     = dim_bot,
      y     = mean_value,
      group = class_label,
      color = class_label
    )
  ) +
    geom_hline(
      yintercept = 0,
      linetype   = "dashed",
      color      = "grey65",
      linewidth  = 0.55
    ) +
    geom_line(
      linewidth = 1.25
    ) +
    geom_point(
      size = 4.1
    ) +
    annotate(
      "text",
      x          = ann_df$x,
      y          = ann_df$y,
      label      = ann_df$label,
      size       = top_label_size,
      color      = "grey25",
      hjust      = 0.5,
      vjust      = 0.5,
      fontface   = "bold",
      lineheight = 0.9
    ) +
    scale_color_manual(
      values = pal,
      name   = "Latentes Profil"
    ) +
    scale_y_continuous(
      breaks = seq(-1, 1, 0.5)
    ) +
    coord_cartesian(
      ylim = c(y_min, y_max),
      clip = "off"
    ) +
    labs(
      title = paste0(
        study_name_map[study],
        " — LPA-Profilplot"
      ),
      subtitle = paste0(
        K, " latente Profile | ",
        var_lbl, " | ",
        "N = ", wtd_n, " | ",
        "Entropy = ", round(entropy, 3),
        " | ", weight_label[wtype]
      ),
      x = NULL,
      y = "Profilmittelwerte"
    ) +
    theme_minimal(
      base_size = 18
    ) +
    theme(
      plot.title = element_text(
        face = "bold",
        size = 22
      ),
      plot.subtitle = element_text(
        size       = 13,
        color      = "grey40",
        lineheight = 0.95
      ),

      axis.title.y = element_text(
        size   = 17,
        margin = margin(r = 10)
      ),
      axis.text.y = element_text(
        size = 15
      ),
      axis.text.x = element_text(
        face       = "bold",
        size       = bottom_label_size,
        lineheight = 0.9
      ),

      legend.position = "right",
      legend.title = element_text(
        face = "bold",
        size = 15
      ),
      legend.text = element_text(
        size       = 13,
        lineheight = 0.95
      ),
      legend.key.height = unit(
        0.8,
        "cm"
      ),
      legend.key.width = unit(
        0.6,
        "cm"
      ),

      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_blank(),

      plot.margin = margin(
        42, 14, 12, 14
      )
    )
}

# ----------------------------------------------------------
# 9B: Klassengroessen
# ----------------------------------------------------------

make_size_plot <- function(
    study,
    means = NULL,
    wtype = "unweighted",
    hide_x_labels = FALSE) {

  if (is.null(means)) {
    means <- profile_means_list[[study]]
  }

  K      <- chosen_solution[[study]]$classes
  pal    <- get_named_pal(study)
  levels <- get_label_levels(study)

  # Balken nach Profilgröße sortieren: größtes Profil links,
  # danach absteigend. Die Farblegende/Profilfarben bleiben an den
  # ursprünglichen Profilbezeichnungen ausgerichtet.
  size_order <- means %>%
    arrange(desc(pct), lpa_class) %>%
    pull(class_label) %>%
    as.character()

  # Nur für Österreich werden die langen Profilnamen enger umgebrochen.
  profile_wrap_width <- if (study == "AUT_2017") 18 else 24

  df <- means %>%
    mutate(
      class_label = factor(
        class_label,
        levels = levels
      ),
      class_label_plot = factor(
        wrap_profile_labels(
          class_label,
          width = profile_wrap_width
        ),
        levels = wrap_profile_labels(
          size_order,
          width = profile_wrap_width
        )
      ),
      pct_label = paste0(
        round(pct, 1), "%\n(n=", n, ")"
      )
    )

  size_axis_text <- if (study == "AUT_2017") 7.2 else 8

  ggplot(df,
         aes(x    = class_label_plot,
             y    = pct,
             fill = class_label)) +
    geom_col(width = 0.65,
             show.legend = FALSE) +
    geom_text(aes(label = pct_label),
              vjust    = -0.3,
              size     = 3.2,
              fontface = "bold") +
    scale_fill_manual(values = pal) +
    scale_y_continuous(
      limits = c(0, max(df$pct) * 1.3),
      labels = label_percent(scale = 1)
    ) +
    labs(
      title    = paste0(
        study_name_map[study],
        " — Profilgrößen"),
      subtitle = paste0(
        "Gesamt-N = ", sum(df$n), " | ",
        "Entropy = ",
        round(unique(df$entropy), 3),
        " | ", weight_label[wtype]),
      x = NULL,
      y = "Anteil (%)"
    ) +
    theme_minimal(base_size = 16) +
    theme(
      plot.title         = element_text(
        face = "bold"),
      plot.subtitle      = element_text(
        size = 10, color = "grey40"),
      axis.text.x        = element_text(
        size       = size_axis_text,
        hjust      = 0.5,
        lineheight = 0.85),
      axis.ticks.x       = element_blank(),
      panel.grid.minor   = element_blank(),
      panel.grid.major.x = element_blank()
    ) +
    theme(
      axis.text.x = if (hide_x_labels) {
        element_blank()
      } else {
        element_text(
          size       = 8,
          hjust      = 0.5,
          lineheight = 0.85
        )
      }
    )
}

# ----------------------------------------------------------
# 9C: Posterior-Verteilung
# ----------------------------------------------------------

make_uncertainty_plot <- function(
    study,
    tbl,
    lbl,
    wtype = "unweighted") {

  pal     <- get_named_pal(study)
  levels  <- get_label_levels(study)
  var_lbl <- model_labels_long[
    as.character(
      chosen_solution[[study]]$model
    )
  ]

  df <- tbl %>%
    mutate(
      class_label = factor(
        lbl[as.character(lpa_class)],
        levels = levels
      )
    )

  # Profilspezifische Mediane. Sie werden nicht im Plot beschriftet,
  # sondern in Klammern an die jeweiligen Profilnamen der Legende angefügt.
  median_df <- df %>%
    filter(!is.na(lpa_posterior), !is.na(class_label)) %>%
    group_by(class_label) %>%
    summarise(
      median_posterior = median(lpa_posterior, na.rm = TRUE),
      .groups = "drop"
    )

  median_lookup <- setNames(
    label_number(
      accuracy = 0.01,
      decimal.mark = ","
    )(median_df$median_posterior),
    as.character(median_df$class_label)
  )

  legend_labels <- setNames(
    paste0(levels, " (Median: ", median_lookup[levels], ")"),
    levels
  )

  ggplot(df,
         aes(x     = lpa_posterior,
             fill  = class_label,
             color = class_label)) +
    geom_density(alpha     = 0.35,
                 linewidth = 0.7) +
    geom_vline(xintercept = 0.80,
               linetype   = "dashed",
               color      = "grey40") +
    annotate("text",
             x     = 0.80, y = Inf,
             label = "0,80",
             hjust = -0.2, vjust = 1.5,
             size  = 3, color = "grey40") +
    scale_fill_manual(
      values = pal,
      breaks = levels,
      labels = legend_labels,
      name   = "Latentes Profil"
    ) +
    scale_color_manual(
      values = pal,
      breaks = levels,
      labels = legend_labels,
      name   = "Latentes Profil"
    ) +
    scale_x_continuous(
      limits = c(0, 1.04),
      breaks = seq(0, 1, 0.2),
      labels = label_number(
        accuracy = 0.1,
        decimal.mark = ","
      ),
      expand = expansion(mult = c(0.01, 0.06))
    ) +
    coord_cartesian(clip = "off") +
    labs(
      title    = paste0(
        study_name_map[study],
        " — Klassifikationssicherheit"),
      subtitle = paste0(
        "gestrichelt = 0,80 | ",
        var_lbl,
        " | ", weight_label[wtype]),
      x = "Max. Posterior-Wahrscheinlichkeit",
      y = "Dichte",
      caption = paste0(
        "Dargestellt ist für jedes latente Profil die Verteilung der maximalen ",
        "posterioren Zugehörigkeitswahrscheinlichkeit der ihm zugeordneten Personen.\n",
        "Je weiter die Verteilung rechts liegt, desto eindeutiger ist die Klassenzuordnung.\n",
        "Die profilspezifischen Mediane sind in der Legende ausgewiesen."
      )
    ) +
    theme_minimal(base_size = 16) +
    theme(
      plot.title       = element_text(
        face = "bold"),
      plot.subtitle    = element_text(
        size = 12, color = "grey40"),
      plot.caption     = element_text(
        size = 8.5,
        color = "grey35",
        hjust = 0,
        lineheight = 1.05,
        margin = margin(t = 8)
      ),
      legend.position  = "right",
      legend.title     = element_text(
        face = "bold", size = 14),
      legend.text      = element_text(
        size = 9, lineheight = 0.9),
      panel.grid.minor = element_blank(),
      plot.margin      = margin(
        10, 28, 10, 10
      )
    )
}

# ----------------------------------------------------------
# 9D: Avg. Posterior je Klasse
# ----------------------------------------------------------

make_avg_posterior_plot <- function(
    study,
    tbl,
    lbl,
    wtype = "unweighted") {

  pal     <- get_named_pal(study)
  levels  <- get_label_levels(study)
  var_lbl <- model_labels_long[
    as.character(
      chosen_solution[[study]]$model
    )
  ]

  avg_tbl <- tbl %>%
    mutate(
      class_label = factor(
        lbl[as.character(lpa_class)],
        levels = levels
      )
    ) %>%
    group_by(class_label) %>%
    summarise(
      avg_post = mean(lpa_posterior,
                      na.rm = TRUE),
      n        = n(),
      .groups  = "drop"
    ) %>%
    mutate(
      class_label_plot = factor(
        wrap_profile_labels(class_label, width = 24),
        levels = wrap_profile_labels(levels, width = 24)
      )
    )

  ggplot(avg_tbl,
         aes(x    = class_label_plot,
             y    = avg_post,
             fill = class_label)) +
    geom_col(width = 0.65,
             show.legend = FALSE) +
    geom_text(
      aes(label = paste0(
        round(avg_post, 3),
        "\n(n=", n, ")"
      )),
      vjust    = -0.4,
      size     = 3.5,
      fontface = "bold"
    ) +
    geom_hline(yintercept = 0.80,
               linetype   = "dashed",
               color      = "grey40") +
    geom_hline(yintercept = 0.70,
               linetype   = "dotted",
               color      = "grey60") +
    scale_fill_manual(values = pal) +
    scale_y_continuous(
      limits = c(0, 1.15),
      breaks = seq(0, 1, 0.1),
      labels = label_number(accuracy = 0.1)
    ) +
    labs(
      title    = paste0(
        study_name_map[study],
        " — Durchschnittliche Posterior-Wahrscheinlichkeit nach Profil"),
      subtitle = paste0(
        "gestrichelt = 0,80 | gepunktet = 0,70 | ",
        var_lbl,
        " | ", weight_label[wtype]),
      x = NULL,
      y = "Durchschn. max. Posterior-Wahrscheinlichkeit"
    ) +
    theme_minimal(base_size = 16) +
    theme(
      plot.title         = element_text(
        face = "bold"),
      plot.subtitle      = element_text(
        size = 12, color = "grey40"),
      axis.text.x        = element_text(
        size       = 10,
        lineheight = 0.85),
      panel.grid.minor   = element_blank(),
      panel.grid.major.x = element_blank()
    )
}

# ----------------------------------------------------------
# 9E: Posterior-Verteilung je Profil
# ----------------------------------------------------------

make_posterior_by_profile_plot <- function(
    study,
    tbl,
    lbl,
    wtype = "unweighted") {

  pal     <- get_named_pal(study)
  levels  <- get_label_levels(study)
  var_lbl <- model_labels_long[
    as.character(
      chosen_solution[[study]]$model
    )
  ]

  df <- tbl %>%
    mutate(
      class_label = factor(
        lbl[as.character(lpa_class)],
        levels = levels
      ),
      class_label_plot = factor(
        wrap_profile_labels(class_label, width = 30),
        levels = wrap_profile_labels(levels, width = 30)
      )
    )

  ggplot(df,
         aes(x     = lpa_posterior,
             fill  = class_label,
             color = class_label)) +
    geom_histogram(
      aes(y = after_stat(density)),
      bins     = 25,
      alpha    = 0.35,
      linewidth = 0.25,
      boundary = 0
    ) +
    geom_density(
      linewidth = 0.9,
      alpha     = 0.85
    ) +
    geom_vline(
      xintercept = 0.80,
      linetype   = "dashed",
      color      = "grey40",
      linewidth  = 0.6
    ) +
    facet_wrap(
      ~ class_label_plot,
      ncol = 2,
      scales = "fixed"
    ) +
    scale_fill_manual(
      values = pal,
      guide  = "none"
    ) +
    scale_color_manual(
      values = pal,
      guide  = "none"
    ) +
    scale_x_continuous(
      limits = c(0, 1),
      breaks = seq(0, 1, 0.2),
      labels = label_number(
        accuracy = 0.1,
        decimal.mark = ","
      )
    ) +
    scale_y_continuous(
      limits = c(0, 10),
      breaks = seq(0, 10, 2.5),
      labels = label_number(
        accuracy = 0.1,
        decimal.mark = ","
      ),
      expand = expansion(mult = c(0, 0.02))
    ) +
    labs(
      title = paste0(
        study_name_map[study],
        " — Posterior-Verteilungen nach Profil"
      ),
      subtitle = paste0(
        "Histogramm und Dichte der maximalen Posterior-Wahrscheinlichkeit | ",
        var_lbl,
        " | ", weight_label[wtype]
      ),
      x = "Max. Posterior-Wahrscheinlichkeit",
      y = "Dichte"
    ) +
    theme_minimal(base_size = 16) +
    theme(
      plot.title       = element_text(
        face = "bold"),
      plot.subtitle    = element_text(
        size = 12, color = "grey40"),
      strip.text       = element_text(
        face = "bold",
        size = 12,
        lineheight = 0.9),
      panel.grid.minor = element_blank()
    )
}

# ----------------------------------------------------------
# 9F: Posterior-Verteilung je Profil über alle Personen
# ----------------------------------------------------------

make_posterior_membership_all_plot <- function(
    study,
    tbl,
    lbl,
    wtype = "unweighted") {

  pal     <- get_named_pal(study)
  levels  <- get_label_levels(study)
  var_lbl <- model_labels_long[
    as.character(
      chosen_solution[[study]]$model
    )
  ]

  post_cols <- names(tbl) %>%
    stringr::str_subset("^posterior_class_")

  if (length(post_cols) == 0) {
    stop(
      "Keine posterior_class_* Spalten gefunden fuer ",
      study,
      ". Bitte 03_B_lpa_multi_init_scaled_midpoint.R erneut laufen lassen.",
      call. = FALSE
    )
  }

  df <- tbl %>%
    select(all_of(post_cols)) %>%
    pivot_longer(
      cols      = everything(),
      names_to  = "posterior_class",
      values_to = "posterior_probability"
    ) %>%
    mutate(
      lpa_class = stringr::str_remove(
        posterior_class,
        "^posterior_class_"
      ),
      class_label = factor(
        lbl[lpa_class],
        levels = levels
      ),
      class_label_plot = factor(
        wrap_profile_labels(class_label, width = 30),
        levels = wrap_profile_labels(levels, width = 30)
      )
    )

  ggplot(df,
         aes(x     = posterior_probability,
             fill  = class_label,
             color = class_label)) +
    geom_histogram(
      aes(y = after_stat(density)),
      bins      = 25,
      alpha     = 0.35,
      linewidth = 0.25,
      boundary  = 0
    ) +
    geom_density(
      linewidth = 0.9,
      alpha     = 0.85
    ) +
    facet_wrap(
      ~ class_label_plot,
      ncol   = 2,
      scales = "free_y"
    ) +
    scale_fill_manual(
      values = pal,
      guide  = "none"
    ) +
    scale_color_manual(
      values = pal,
      guide  = "none"
    ) +
    scale_x_continuous(
      limits = c(0, 1),
      breaks = seq(0, 1, 0.2),
      labels = label_number(
        accuracy = 0.1,
        decimal.mark = ","
      )
    ) +
    labs(
      title = paste0(
        study_name_map[study],
        " — Profilwahrscheinlichkeiten über alle Personen"
      ),
      subtitle = paste0(
        "Histogramm und Dichte der posterior_class_* Wahrscheinlichkeiten | ",
        var_lbl,
        " | ", weight_label[wtype]
      ),
      x = "Posterior-Wahrscheinlichkeit für das jeweilige Profil",
      y = "Dichte"
    ) +
    theme_minimal(base_size = 16) +
    theme(
      plot.title       = element_text(
        face = "bold"),
      plot.subtitle    = element_text(
        size = 12, color = "grey40"),
      strip.text       = element_text(
        face = "bold",
        size = 12,
        lineheight = 0.9),
      panel.grid.minor = element_blank()
    )
}

# ----------------------------------------------------------
# 9G: Kombinierter Größen- und Profilplot
# ----------------------------------------------------------

make_size_profile_combined <- function(
    study,
    means = NULL,
    wtype = "unweighted") {

  if (is.null(means)) {
    means <- profile_means_list[[study]]
  }

  combined_size_axis_text <- if (study == "AUT_2017") 11 else 14

  p_size <- make_size_plot(
    study, means, wtype,
    hide_x_labels = FALSE
  ) +
    labs(title = "Profilgrößen") +
    theme(
      plot.title = element_text(
        face = "bold", size = 14),
      plot.subtitle = element_blank(),
      axis.text.x = element_text(
        size = combined_size_axis_text,
        hjust = 0.5,
        lineheight = 0.82
      )
    )

  p_prof <- make_profile_plot(
    study, means, wtype
  ) +
    labs(title = "") +
    theme(
      plot.title = element_text(
        face = "bold", size = 14),
      plot.subtitle = element_blank(),
      legend.position = "none"
    )

  legend_combined <- get_legend(
    make_profile_plot(
      study, means, wtype
    ) +
      theme(
        legend.position  = "bottom",
        legend.direction = "horizontal",
        legend.title     = element_text(
          face = "bold", size = 12),
        legend.text      = element_text(
          size = 9,
          lineheight = 0.9)
      ) +
      guides(
        color = guide_legend(
          nrow = 2,
          byrow = TRUE,
          override.aes = list(
            linewidth = 1.4,
            size = 3
          )
        )
      )
  )

  title_combined <- ggdraw() +
    draw_label(
      paste0(
        study_name_map[study],
        " — Profilgrößen und Profilmittelwerte"
      ),
      fontface = "bold",
      size     = 16,
      x        = 0,
      hjust    = 0
    )

  subtitle_combined <- ggdraw() +
    draw_label(
      paste0(
        weight_label[wtype],
        " | harte Klassenzuordnung und Profile entlang der Profilmittelwerte"
      ),
      color = "grey40",
      size  = 14,
      x     = 0,
      hjust = 0
    )

  body_combined <- plot_grid(
    p_size,
    p_prof,
    nrow       = 1,
    rel_widths = c(1.25, 1.35),
    align      = "hv"
  )

  plot_grid(
    title_combined,
    subtitle_combined,
    body_combined,
    legend_combined,
    nrow        = 4,
    rel_heights = c(0.06, 0.04, 1, 0.22)
  )
}

# ----------------------------------------------------------
# 9H: Dimensionspaare — 2D Scatterplot-Matrix
#     Alle Dimensions-Kombinationen
# ----------------------------------------------------------

make_clr_pairs <- function(
    study,
    wtype = "unweighted") {

  cfg    <- study_vars[[study]]
  tbl    <- assignment_list[[study]]
  lbl    <- class_labels[[study]]
  K      <- chosen_solution[[study]]$classes
  pal    <- get_named_pal(study)
  levels <- get_label_levels(study)
  vars   <- cfg$vars
  short  <- cfg$short
  tlabs  <- cfg$top_labels
  n_dim  <- length(vars)

  # Daten vorbereiten
  df_plot <- tbl %>%
    select(lpa_class,
           all_of(vars)) %>%
    mutate(
      class_label = factor(
        lbl[as.character(lpa_class)],
        levels = levels
      )
    ) %>%
    rename_with(
      ~ short,
      all_of(vars)
    )

  # Alle Paare bestimmen
  pairs <- combn(n_dim, 2,
                 simplify = FALSE)

  plot_list <- lapply(pairs, function(p) {

    xv  <- short[p[1]]
    yv  <- short[p[2]]
    xl  <- tlabs[p[1]]
    yl  <- tlabs[p[2]]

    ggplot(df_plot,
           aes(x     = .data[[xv]],
               y     = .data[[yv]],
               color = class_label)) +
      geom_point(alpha = 0.35,
                 size  = 1.2) +
      geom_density_2d(
        linewidth = 0.5,
        alpha     = 0.7
      ) +
      scale_color_manual(
        values = pal,
        name   = "Latentes Profil",
        breaks = levels,
        labels = wrap_profile_labels(levels, width = 34)
      ) +
      labs(
        x = xl,
        y = yl
      ) +
      theme_minimal(base_size = 10) +
      theme(
        legend.position  = "none",
        panel.grid.minor = element_blank(),
        axis.title       = element_text(
          size = 8, face = "bold",
          lineheight = 0.85)
      )
  })

  # Legende separat extrahieren
  legend_plot <- get_legend(
    ggplot(df_plot,
           aes(x     = .data[[short[1]]],
               y     = .data[[short[2]]],
               color = class_label)) +
      geom_point() +
      scale_color_manual(
        values = pal,
        name   = "Latentes Profil",
        breaks = levels,
        labels = wrap_profile_labels(levels, width = 34)
      ) +
      theme_minimal() +
      theme(
        legend.position = "right",
        legend.title    = element_text(
          face = "bold", size = 10),
        legend.text     = element_text(
          size = 9, lineheight = 0.9)
      )
  )

  # Grid zusammenbauen
  # Bei 3 Dimensionen: 3 Paare
  # Anordnung: 2x2 Grid + Legende rechts
  pairs_grid <- plot_grid(
    plotlist = plot_list,
    nrow     = ceiling(
      length(plot_list) / 2
    ),
    align    = "hv"
  )

  title_clr <- ggdraw() +
    draw_label(
      paste0(
        study_name_map[study],
        " — Dimensionspaare | ",
        weight_label[wtype]
      ),
      fontface = "bold",
      size     = 13,
      x        = 0.5,
      hjust    = 0.5
    )

  subtitle_clr <- ggdraw() +
    draw_label(
      paste0(
        "Zweidimensionale Klassifikationsräume | ",
        "Punkte = Befragte | ",
        "Konturen = Dichte"
      ),
      color = "grey40",
      size  = 9,
      x     = 0.5,
      hjust = 0.5
    )

  full_plot <- plot_grid(
    title_clr,
    subtitle_clr,
    plot_grid(pairs_grid, legend_plot,
              nrow       = 1,
              rel_widths = c(1, 0.25)),
    nrow        = 3,
    rel_heights = c(0.07, 0.04, 1)
  )

  full_plot
}

###############################################################################
## 10) HAUPTSCHLEIFE
###############################################################################

results <- list()

for (study in studies) {

  cat("\n")
  cat("============================================================\n")
  cat("Verarbeite:", study, "\n")
  cat("============================================================\n")

  lbl <- class_labels[[study]]
  tbl <- assignment_list[[study]]

  for (wtype in weight_config[[study]]) {

    cat(sprintf("\n  -> %s\n", wtype))

    # Unterordner
    DIR_PLOTS_STUDY <- file.path(
      DIR_PLOTS, study, wtype
    )
    dir.create(DIR_PLOTS_STUDY,
               recursive    = TRUE,
               showWarnings = FALSE)

    # Aktuelle Mittelwerte
    key   <- paste0(study, "__", wtype)
    means <- profile_means_all[[key]]

    # -- Profilplot --
    p_prof <- make_profile_plot(
      study, means, wtype
    )
    ggsave(
      file.path(DIR_PLOTS_STUDY,
                paste0("profile_",
                       study, ".png")),
      p_prof,
      width = 11.5, height = 6, dpi = 300
    )
    cat("     ✓ profile\n")

    # -- Klassengroessen --
    p_size <- make_size_plot(
      study, means, wtype
    )
    ggsave(
      file.path(DIR_PLOTS_STUDY,
                paste0("sizes_",
                       study, ".png")),
      p_size,
      width = 12, height = 5.5, dpi = 300
    )
    cat("     ✓ sizes\n")

    # -- Kombinierter Plot: Profilgroessen + Profilmittelwerte --
    p_combined <- make_size_profile_combined(
      study, means, wtype
    )
    ggsave(
      file.path(DIR_PLOTS_STUDY,
                paste0("combined_profile_sizes_",
                       study, ".png")),
      p_combined,
      width = 16, height = 7.2, dpi = 300
    )
    cat("     ✓ combined_profile_sizes\n")

    # Uncertainty + Posterior:
    # nur ungewichtet (basiert auf
    # Posterior-Wahrscheinlichkeiten,
    # nicht auf Weights)
    if (wtype == "unweighted") {

      p_unc <- make_uncertainty_plot(
        study, tbl, lbl, wtype
      )
      ggsave(
        file.path(DIR_PLOTS_STUDY,
                  paste0("uncertainty_",
                         study, ".png")),
        p_unc,
        width = 11, height = 5, dpi = 300
      )
      cat("     ✓ uncertainty\n")

      p_post <- make_avg_posterior_plot(
        study, tbl, lbl, wtype
      )
      ggsave(
        file.path(DIR_PLOTS_STUDY,
                  paste0("avg_posterior_",
                         study, ".png")),
        p_post,
        width = 10, height = 5, dpi = 300
      )
      cat("     ✓ avg_posterior\n")

      p_post_by_profile <- make_posterior_by_profile_plot(
        study, tbl, lbl, wtype
      )
      ggsave(
        file.path(DIR_PLOTS_STUDY,
                  paste0("posterior_by_profile_",
                         study, ".png")),
        p_post_by_profile,
        width = 11.5, height = 7, dpi = 300
      )
      cat("     ✓ posterior_by_profile\n")

      # Klassifikationsdiagnostik je Profil (ungewichtet)
      posterior_diagnostics <- tbl %>%
        mutate(
          class_label = lbl[as.character(lpa_class)]
        ) %>%
        group_by(lpa_class, class_label) %>%
        summarise(
          n = n(),
          avg_posterior = mean(lpa_posterior, na.rm = TRUE),
          median_posterior = median(lpa_posterior, na.rm = TRUE),
          pct_posterior_ge_070 = mean(lpa_posterior >= 0.70, na.rm = TRUE) * 100,
          pct_posterior_ge_080 = mean(lpa_posterior >= 0.80, na.rm = TRUE) * 100,
          .groups = "drop"
        ) %>%
        mutate(
          across(c(avg_posterior, median_posterior), ~ round(.x, 3)),
          across(starts_with("pct_"), ~ round(.x, 1))
        )

      write_csv(
        posterior_diagnostics,
        file.path(
          DIR_TABLES_FINAL,
          paste0("lpa_posterior_diagnostics_", study, ".csv")
        )
      )
      cat("     ✓ posterior diagnostics CSV\n")

      p_post_all <- make_posterior_membership_all_plot(
        study, tbl, lbl, wtype
      )
      ggsave(
        file.path(DIR_PLOTS_STUDY,
                  paste0("posterior_membership_all_",
                         study, ".png")),
        p_post_all,
        width = 11.5, height = 7, dpi = 300
      )
      cat("     ✓ posterior_membership_all\n")

      # Dimensionspaare: nur ungewichtet
      p_clr <- make_clr_pairs(
        study, wtype
      )
      ggsave(
        file.path(DIR_PLOTS_STUDY,
                  paste0("clr_pairs_",
                         study, ".png")),
        p_clr,
        width  = 12,
        height = 8,
        dpi    = 300
      )
      cat("     ✓ clr_pairs\n")
    }

    # -- Summary-Tabelle --
    summary_tbl <- means %>%
      select(study, study_label,
             weight_type,
             lpa_class, class_label,
             n, pct, entropy,
             BIC, AIC, SABIC,
             ends_with("_mean"),
             ends_with("_sd")) %>%
      mutate(
        pct     = round(pct,     1),
        entropy = round(entropy, 3),
        BIC     = round(BIC,     1),
        AIC     = round(AIC,     1),
        SABIC   = round(SABIC,   1),
        across(ends_with("_mean"),
               ~ round(.x, 3)),
        across(ends_with("_sd"),
               ~ round(.x, 3))
      )

    write_csv(
      summary_tbl,
      file.path(
        DIR_TABLES_FINAL,
        paste0("lpa_summary_",
               study, "_",
               wtype, ".csv")
      )
    )
    cat("     ✓ summary CSV\n")
  }

  # Assignment mit Labels updaten
  # (einmalig, nicht je wtype)
  assignment_updated <- tbl %>%
    mutate(
      lpa_class_label = lbl[
        as.character(lpa_class)
      ]
    )
  write_csv(
    assignment_updated,
    file.path(
      DIR_TABLES_FINAL,
      paste0("lpa_assignment_",
             study, ".csv")
    )
  )

  results[[study]] <- list(
    entropy = unique(
      profile_means_list[[study]]$entropy
    ),
    n       = sum(
      profile_means_list[[study]]$n
    )
  )
}

###############################################################################
## 11) FERTIG
###############################################################################

cat("\n")
cat("============================================================\n")
cat("03_C_lpa_class_plots_scaled_midpoint_DE_v2_layout05_wide abgeschlossen.\n\n")
cat("Ordnerstruktur:\n")
cat("  outputs/plots/tidylpa_scaled_midpoint/plots/\n")
for (s in studies) {
  cat(sprintf("    %s/\n", s))
  for (wt in weight_config[[s]]) {
    cat(sprintf("      %s/\n", wt))
    cat(sprintf(
      "        profile_%s.png\n", s))
    cat(sprintf(
      "        sizes_%s.png\n", s))
    cat(sprintf(
      "        combined_profile_sizes_%s.png\n", s))
    if (wt == "unweighted") {
      cat(sprintf(
        "        uncertainty_%s.png\n", s))
      cat(sprintf(
        "        avg_posterior_%s.png\n", s))
      cat(sprintf(
        "        posterior_by_profile_%s.png\n", s))
      cat(sprintf(
        "        posterior_membership_all_%s.png\n", s))
      cat(sprintf(
        "        clr_pairs_%s.png\n", s))
    }
  }
}
cat("\nEntropy-Werte (ungewichtet):\n")
for (s in studies) {
  cat(sprintf("  %s: %.3f\n",
              s, results[[s]]$entropy))
}
cat("============================================================\n")
