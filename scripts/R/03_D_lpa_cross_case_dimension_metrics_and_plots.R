###############################################################################
## DISS_2026 — 03_D: Dimensionsvergleich der latenten Einstellungsprofile
##
## Dieses Skript erzeugt die Tabellen für den Dimensionsvergleich sowie die
## zwei gesetzten fallvergleichenden Abbildungen. Es berechnet für jede
## Kombination aus Fall und Demokratiedimension fünf Kennwerte:
##
##   1) gewichtete mittlere Paardistanz,
##   2) Zustimmungsintensität,
##   3) Ablehnungsintensität,
##   4) Aktivierungswert,
##   5) gewichteten Gesamtmittelwert.
##
## Zusätzlich werden Rangtabellen für
##   a) Vergleiche der Dimensionen innerhalb eines Falls und
##   b) Vergleiche der Fälle innerhalb derselben Dimension
## ausgegeben.
##
## Abbildungen:
##   1) Profilmittelwerte im Fallvergleich
##   2) Grauzonenvergleich als Lollipop-Plot
##
## Voraussetzung:
##   Die ungewichteten Profilzusammenfassungen aus 03_C müssen vorliegen:
##
##   outputs/tables/tidylpa_scaled_midpoint/plots/
##     lpa_summary_AUT_2017_unweighted.csv
##     lpa_summary_DEU_2021_unweighted.csv
##     lpa_summary_GRC_2019_unweighted.csv
###############################################################################

suppressPackageStartupMessages({
  library(tidyverse)
  library(cowplot)
  library(scales)
  library(here)
})

###############################################################################
## 0) PFADE
###############################################################################

DIR_TABLES_IN <- here(
  "outputs", "tables", "tidylpa_scaled_midpoint", "plots"
)

DIR_TABLES_OUT <- here(
  "outputs", "tables", "tidylpa_scaled_midpoint",
  "cross_case_hypotheses"
)

DIR_PLOTS_OUT <- here(
  "outputs", "plots", "tidylpa_scaled_midpoint",
  "cross_case_hypotheses"
)

for (d in c(DIR_TABLES_OUT, DIR_PLOTS_OUT)) {
  dir.create(d, recursive = TRUE, showWarnings = FALSE)
}

###############################################################################
## 1) KONFIGURATION
###############################################################################

studies <- c("AUT_2017", "DEU_2021", "GRC_2019")

study_name_map <- c(
  AUT_2017 = "Österreich 2017",
  DEU_2021 = "Deutschland 2021",
  GRC_2019 = "Griechenland 2019"
)

# Die Bezeichnungen bleiben fallspezifisch:
# Österreich: anti-elitär und minimal-pragmatisch
# Deutschland/Griechenland: populistisch und pragmatisch
dimension_label_map <- tribble(
  ~study,     ~dimension_key, ~dimension_label,
  "AUT_2017", "POP_AE",       "Anti-elitär",
  "AUT_2017", "PRG_MPRG",     "Minimal-pragmatisch",
  "AUT_2017", "EXCL",         "Exklusionistisch",
  "DEU_2021", "POP_AE",       "Populistisch",
  "DEU_2021", "PRG_MPRG",     "Pragmatisch",
  "DEU_2021", "EXCL",         "Exklusionistisch",
  "GRC_2019", "POP_AE",       "Populistisch",
  "GRC_2019", "PRG_MPRG",     "Pragmatisch",
  "GRC_2019", "EXCL",         "Exklusionistisch"
)

dimension_order <- c("POP_AE", "PRG_MPRG", "EXCL")

hypothesis_dimension <- tribble(
  ~study,     ~dimension_key,
  "AUT_2017", "EXCL",
  "DEU_2021", "PRG_MPRG",
  "GRC_2019", "POP_AE"
)


# Profilbezeichnungen der Grauzonen
grey_zone_labels <- c(
  AUT_2017 = "exklusionistisch und anti-elitär geneigte Grauzone",
  DEU_2021 = "pragmatisch geneigte Grauzone",
  GRC_2019 = "populistisch geneigte Grauzone"
)

# Profilfarben für die beiden Abbildungen
class_colors <- c(
  "exklusionistisch und anti-elitär geneigte Grauzone" = "#999999",
  "anti-anti-elitäre Minimalpragmatiker" = "#56B4E9",
  "anti-exklusionistische Minimalpragmatiker" = "#0072B2",
  "anti-elitäre und exklusionistische Minimalpragmatiker" = "#E69F00",
  "anti-populistische Pragmatiker" = "#009E73",
  "exklusionistisch neigende Populisten" = "#D55E00",
  "pragmatisch geneigte Grauzone" = "#999999",
  "Pragmatisten" = "#009E73",
  "populistisch geneigte Grauzone" = "#999999",
  "exklusionistisch geneigte und anti-pragmatische Populisten" = "#E69F00"
)

hypothesis_fill <- c(
  AUT_2017 = "#D55E00",
  DEU_2021 = "#009E73",
  GRC_2019 = "#CC79A7"
)

axis_labels_by_study <- list(
  AUT_2017 = c(
    "Anti-elitäre\nDimension",
    "Minimal-pragmatische\nDimension",
    "Exklusionistische\nDimension"
  ),
  DEU_2021 = c(
    "Populistische\nDimension",
    "Pragmatische\nDimension",
    "Exklusionistische\nDimension"
  ),
  GRC_2019 = c(
    "Populistische\nDimension",
    "Pragmatische\nDimension",
    "Exklusionistische\nDimension"
  )
)

###############################################################################
## 2) DATEN LADEN UND HARMONISIEREN
###############################################################################

read_summary <- function(study_id) {
  path <- file.path(
    DIR_TABLES_IN,
    paste0("lpa_summary_", study_id, "_unweighted.csv")
  )

  if (!file.exists(path)) {
    stop(
      "Eingabedatei fehlt: ", path,
      "\nBitte zuerst das 03_C-Skript vollständig ausführen.",
      call. = FALSE
    )
  }

  read_csv(path, show_col_types = FALSE)
}

summary_raw <- map_dfr(studies, read_summary)

required_columns <- c("study", "lpa_class", "class_label", "pct")
missing_columns <- setdiff(required_columns, names(summary_raw))

if (length(missing_columns) > 0) {
  stop(
    "In den Eingabedateien fehlen folgende Spalten: ",
    paste(missing_columns, collapse = ", "),
    call. = FALSE
  )
}

profile_long <- summary_raw %>%
  mutate(
    study = as.character(study),
    profile_share_raw = pct / 100
  ) %>%
  pivot_longer(
    cols = matches("^(AE|POP|MPRG|PRG|EXCL)_mean$"),
    names_to = "source_dimension",
    values_to = "profile_mean"
  ) %>%
  mutate(
    dimension_key = case_when(
      source_dimension %in% c("AE_mean", "POP_mean") ~ "POP_AE",
      source_dimension %in% c("MPRG_mean", "PRG_mean") ~ "PRG_MPRG",
      source_dimension == "EXCL_mean" ~ "EXCL",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(
    study %in% studies,
    !is.na(dimension_key),
    !is.na(profile_mean),
    !is.na(profile_share_raw)
  ) %>%
  # Falls die Prozentanteile aufgrund von Rundung nicht exakt 100 ergeben,
  # werden sie innerhalb jedes Falls auf Summe 1 normiert.
  group_by(study) %>%
  mutate(
    profile_share = profile_share_raw / sum(
      profile_share_raw[!duplicated(lpa_class)],
      na.rm = TRUE
    )
  ) %>%
  ungroup() %>%
  left_join(
    dimension_label_map,
    by = c("study", "dimension_key")
  ) %>%
  mutate(
    study_label = unname(study_name_map[study]),
    dimension_key = factor(dimension_key, levels = dimension_order),
    profile_label_plot = paste0(
      class_label, " (",
      number(pct, accuracy = 0.1, decimal.mark = ","), " %)"
    ),
    profile_color = unname(class_colors[class_label])
  )

if (any(is.na(profile_long$dimension_label))) {
  stop(
    "Mindestens eine Dimension konnte keinem fallspezifischen Namen zugeordnet werden.",
    call. = FALSE
  )
}

if (any(is.na(profile_long$profile_color))) {
  missing_labels <- profile_long %>%
    filter(is.na(profile_color)) %>%
    distinct(class_label) %>%
    pull(class_label)

  stop(
    "Für folgende Profile fehlen Farben: ",
    paste(missing_labels, collapse = ", "),
    call. = FALSE
  )
}

###############################################################################
## 3) DEFINITION DER FÜNF KENNWERTE
###############################################################################

## 3.1 Gewichtete mittlere Paardistanz
##
## Für jedes Profilpaar i und j wird auf einer Dimension zunächst der absolute
## Abstand der beiden Profilmittelwerte berechnet:
##
##   d_ij = |x_i - x_j|
##
## Dieser Abstand wird mit dem Produkt der Profilanteile gewichtet:
##
##   w_ij = p_i * p_j
##
## Die gewichtete mittlere Paardistanz lautet:
##
##                      Summe_{i<j}(w_ij * d_ij)
##   D_ld = ------------------------------------------------
##                           Summe_{i<j}(w_ij)
##
## Interpretation:
##   Je größer D_ld, desto stärker unterscheiden sich die substanziell
##   relevanten Profile auf der betreffenden Dimension. Durch die Gewichtung
##   bestimmen kleine Randprofile das Ergebnis weniger stark als große Profile.
##
##
## 3.2 Zustimmungsintensität
##
## Nur positive Profilmittelwerte gehen in das Maß ein:
##
##   Z_ld = Summe_k p_k * max(x_k, 0)
##
## Interpretation:
##   Je größer Z_ld, desto stärker und breiter ist die betreffende Perspektive
##   in zustimmender Richtung in der Bevölkerung verankert.
##
##
## 3.3 Ablehnungsintensität
##
## Nur negative Profilmittelwerte gehen als positive Beträge in das Maß ein:
##
##   A_ld = Summe_k p_k * max(-x_k, 0)
##
## Interpretation:
##   Je größer A_ld, desto stärker und breiter ist die betreffende Perspektive
##   in ablehnender Richtung in der Bevölkerung verankert.
##
##
## 3.4 Aktivierungswert
##
## Positive und negative Ausprägungen werden unabhängig von ihrer Richtung
## über ihren absoluten Betrag berücksichtigt:
##
##   V_ld = Summe_k p_k * |x_k|
##        = Z_ld + A_ld
##
## Interpretation:
##   Je größer V_ld, desto stärker ist die Bevölkerung auf dieser Dimension
##   insgesamt positioniert. Der Wert unterscheidet nicht zwischen Zustimmung
##   und Ablehnung.
##
##
## 3.5 Gewichteter Gesamtmittelwert
##
##   M_ld = Summe_k p_k * x_k
##        = Z_ld - A_ld
##
## Interpretation:
##   Positive Werte zeigen eine durchschnittlich zustimmende, negative Werte
##   eine durchschnittlich ablehnende Ausrichtung. Ein Wert nahe null kann
##   allerdings entweder geringe Aktivierung oder eine Balance aus starker
##   Zustimmung und starker Ablehnung bedeuten. Deshalb wird M_ld stets
##   gemeinsam mit dem Aktivierungswert und der Paardistanz interpretiert.

###############################################################################
## 4) PAARWEISE DISTANZEN BERECHNEN
###############################################################################

make_pairwise_distances <- function(dat) {
  dat <- dat %>%
    arrange(lpa_class) %>%
    distinct(lpa_class, .keep_all = TRUE)

  if (nrow(dat) < 2) {
    stop(
      "Für mindestens eine Fall-Dimensions-Kombination wurden weniger als ",
      "zwei Profile gefunden.",
      call. = FALSE
    )
  }

  pairs <- combn(seq_len(nrow(dat)), 2)

  tibble(
    profile_1 = dat$class_label[pairs[1, ]],
    profile_2 = dat$class_label[pairs[2, ]],
    mean_1 = dat$profile_mean[pairs[1, ]],
    mean_2 = dat$profile_mean[pairs[2, ]],
    share_1 = dat$profile_share[pairs[1, ]],
    share_2 = dat$profile_share[pairs[2, ]],
    absolute_distance = abs(mean_1 - mean_2),
    pair_weight = share_1 * share_2,
    weighted_distance_component = absolute_distance * pair_weight
  )
}

pairwise_distances <- profile_long %>%
  group_by(
    study, study_label, dimension_key, dimension_label
  ) %>%
  group_modify(~ make_pairwise_distances(.x)) %>%
  ungroup()

pairwise_summary <- pairwise_distances %>%
  group_by(
    study, study_label, dimension_key, dimension_label
  ) %>%
  summarise(
    weighted_mean_pairwise_distance =
      sum(weighted_distance_component) / sum(pair_weight),
    n_profile_pairs = n(),
    .groups = "drop"
  )

###############################################################################
## 5) RICHTUNG, VERBREITUNG UND AKTIVIERUNG BERECHNEN
###############################################################################

direction_metrics <- profile_long %>%
  group_by(
    study, study_label, dimension_key, dimension_label
  ) %>%
  summarise(
    n_profiles = n_distinct(lpa_class),

    # Summe der positiven, mit Profilanteilen gewichteten Mittelwerte
    approval_intensity = sum(
      profile_share * pmax(profile_mean, 0)
    ),

    # Summe der negativen Beträge, mit Profilanteilen gewichtet
    rejection_intensity = sum(
      profile_share * pmax(-profile_mean, 0)
    ),

    # Gewichtete absolute Ausprägung unabhängig von der Richtung
    activation_value = sum(
      profile_share * abs(profile_mean)
    ),

    # Gerichteter profilgrößengewichteter Gesamtmittelwert
    weighted_overall_mean = sum(
      profile_share * profile_mean
    ),

    .groups = "drop"
  )

###############################################################################
## 6) GEMEINSAME HAUPTTABELLE
###############################################################################

dimension_metrics <- direction_metrics %>%
  left_join(
    pairwise_summary,
    by = c(
      "study", "study_label", "dimension_key", "dimension_label"
    )
  ) %>%
  left_join(
    hypothesis_dimension %>%
      mutate(hypothesis_relevant = TRUE),
    by = c("study", "dimension_key")
  ) %>%
  mutate(
    hypothesis_relevant = replace_na(hypothesis_relevant, FALSE),

    # Algebraische Kontrollidentitäten:
    activation_check =
      activation_value - (approval_intensity + rejection_intensity),

    mean_check =
      weighted_overall_mean -
      (approval_intensity - rejection_intensity)
  ) %>%
  arrange(
    factor(study, levels = studies),
    dimension_key
  )

# Sicherheitsprüfung der beiden Identitäten:
stopifnot(
  all(abs(dimension_metrics$activation_check) < 1e-10),
  all(abs(dimension_metrics$mean_check) < 1e-10)
)

dimension_metrics_output <- dimension_metrics %>%
  transmute(
    Fall = study_label,
    Dimension = dimension_label,
    `Gewichtete mittlere Paardistanz` =
      weighted_mean_pairwise_distance,
    Zustimmungsintensität = approval_intensity,
    Ablehnungsintensität = rejection_intensity,
    Aktivierungswert = activation_value,
    `Gewichteter Gesamtmittelwert` = weighted_overall_mean,
    `Hypothesenrelevante Dimension` = hypothesis_relevant
  )

write_csv(
  dimension_metrics_output,
  file.path(
    DIR_TABLES_OUT,
    "lpa_dimension_metrics_main.csv"
  )
)

###############################################################################
## 7) WITHIN-CASE-RÄNGE
###############################################################################

# Diese Tabelle ordnet innerhalb jedes Falls die drei Dimensionen.
#
# Rang 1 bedeutet:
#   - höchste Differenzierung bei der gewichteten Paardistanz,
#   - höchste positive Verbreitung bei der Zustimmungsintensität,
#   - höchste negative Verbreitung bei der Ablehnungsintensität,
#   - höchste allgemeine Positionierung beim Aktivierungswert.
#
# Beim gewichteten Gesamtmittelwert werden zwei Ränge ausgewiesen:
#   - Rang Zustimmung: vom positivsten zum negativsten Mittelwert,
#   - Rang Ablehnung: vom negativsten zum positivsten Mittelwert.

within_case_ranks <- dimension_metrics %>%
  group_by(study, study_label) %>%
  mutate(
    rank_differentiation_within_case =
      min_rank(desc(weighted_mean_pairwise_distance)),
    rank_approval_within_case =
      min_rank(desc(approval_intensity)),
    rank_rejection_within_case =
      min_rank(desc(rejection_intensity)),
    rank_activation_within_case =
      min_rank(desc(activation_value)),
    rank_overall_mean_approval_within_case =
      min_rank(desc(weighted_overall_mean)),
    rank_overall_mean_rejection_within_case =
      min_rank(weighted_overall_mean)
  ) %>%
  ungroup() %>%
  transmute(
    Fall = study_label,
    Dimension = dimension_label,
    `Rang Differenzierung innerhalb des Falls` =
      rank_differentiation_within_case,
    `Rang Zustimmungsintensität innerhalb des Falls` =
      rank_approval_within_case,
    `Rang Ablehnungsintensität innerhalb des Falls` =
      rank_rejection_within_case,
    `Rang Aktivierung innerhalb des Falls` =
      rank_activation_within_case,
    `Rang gerichteter Mittelwert: Zustimmung` =
      rank_overall_mean_approval_within_case,
    `Rang gerichteter Mittelwert: Ablehnung` =
      rank_overall_mean_rejection_within_case
  )

write_csv(
  within_case_ranks,
  file.path(
    DIR_TABLES_OUT,
    "lpa_dimension_metrics_within_case_ranks.csv"
  )
)

###############################################################################
## 8) CROSS-CASE-RÄNGE
###############################################################################

# Diese Tabelle vergleicht für jede inhaltlich korrespondierende Dimension
# die drei Fälle miteinander.
#
# Achtung:
#   Die erste Dimension heißt in Österreich "Anti-elitär", in Deutschland und
#   Griechenland "Populistisch". Ebenso ist die österreichische
#   minimal-pragmatische Dimension nicht vollständig identisch mit der
#   pragmatischen Dimension der beiden anderen Fälle. Die Rangbildung ist
#   rechnerisch möglich, muss aber im Text unter dieser konzeptionellen
#   Einschränkung interpretiert werden.

cross_case_ranks <- dimension_metrics %>%
  group_by(dimension_key) %>%
  mutate(
    rank_differentiation_cross_case =
      min_rank(desc(weighted_mean_pairwise_distance)),
    rank_approval_cross_case =
      min_rank(desc(approval_intensity)),
    rank_rejection_cross_case =
      min_rank(desc(rejection_intensity)),
    rank_activation_cross_case =
      min_rank(desc(activation_value)),
    rank_overall_mean_approval_cross_case =
      min_rank(desc(weighted_overall_mean)),
    rank_overall_mean_rejection_cross_case =
      min_rank(weighted_overall_mean)
  ) %>%
  ungroup() %>%
  transmute(
    Vergleichsdimension = case_when(
      dimension_key == "POP_AE" ~
        "Populistisch / anti-elitär",
      dimension_key == "PRG_MPRG" ~
        "Pragmatisch / minimal-pragmatisch",
      dimension_key == "EXCL" ~
        "Exklusionistisch"
    ),
    Fall = study_label,
    `Fallspezifische Dimensionsbezeichnung` = dimension_label,
    `Rang Differenzierung im Fallvergleich` =
      rank_differentiation_cross_case,
    `Rang Zustimmungsintensität im Fallvergleich` =
      rank_approval_cross_case,
    `Rang Ablehnungsintensität im Fallvergleich` =
      rank_rejection_cross_case,
    `Rang Aktivierung im Fallvergleich` =
      rank_activation_cross_case,
    `Rang gerichteter Mittelwert: Zustimmung` =
      rank_overall_mean_approval_cross_case,
    `Rang gerichteter Mittelwert: Ablehnung` =
      rank_overall_mean_rejection_cross_case
  )

write_csv(
  cross_case_ranks,
  file.path(
    DIR_TABLES_OUT,
    "lpa_dimension_metrics_cross_case_ranks.csv"
  )
)

###############################################################################
## 9) AUSFÜHRLICHE PAARWEISE PRÜFTABELLE
###############################################################################

pairwise_output <- pairwise_distances %>%
  transmute(
    Fall = study_label,
    Dimension = dimension_label,
    `Profil 1` = profile_1,
    `Profil 2` = profile_2,
    `Mittelwert Profil 1` = mean_1,
    `Mittelwert Profil 2` = mean_2,
    `Anteil Profil 1` = share_1,
    `Anteil Profil 2` = share_2,
    `Absolute Paardistanz` = absolute_distance,
    Paargewicht = pair_weight,
    `Gewichteter Distanzbeitrag` = weighted_distance_component
  )

write_csv(
  pairwise_output,
  file.path(
    DIR_TABLES_OUT,
    "lpa_dimension_metrics_pairwise_details.csv"
  )
)

###############################################################################
## 10) DEUTSCHLAND-/EXCEL-KOMPATIBLE VERSION DER HAUPTTABELLE
###############################################################################

# Diese zusätzliche Datei verwendet Semikolon als Feldtrenner und Komma als
# Dezimalzeichen. Dadurch werden Werte wie -0,466 in deutschsprachigen
# Tabellenkalkulationsprogrammen nicht fälschlich als -466 importiert.

dimension_metrics_output_de <- dimension_metrics_output %>%
  mutate(
    across(
      where(is.numeric),
      ~ format(
        .x,
        decimal.mark = ",",
        scientific = FALSE,
        trim = TRUE,
        digits = 10
      )
    )
  )

write_delim(
  dimension_metrics_output_de,
  file.path(
    DIR_TABLES_OUT,
    "lpa_dimension_metrics_main_de.csv"
  ),
  delim = ";",
  na = ""
)


###############################################################################
## 11) PROFILMITTELWERTE IM FALLVERGLEICH
###############################################################################

make_profile_panel <- function(study_id) {
  dat <- profile_long %>%
    filter(study == study_id) %>%
    mutate(
      axis_id = as.integer(dimension_key)
    ) %>%
    arrange(lpa_class, axis_id)

  labels_this_study <- axis_labels_by_study[[study_id]]

  salient_key <- hypothesis_dimension %>%
    filter(study == study_id) %>%
    pull(dimension_key)

  salient_x <- match(salient_key, dimension_order)

  pal <- dat %>%
    distinct(profile_label_plot, profile_color) %>%
    deframe()

  # Die äußeren Hervorhebungsrechtecke werden an die sichtbaren x-Grenzen
  # geklemmt. Andernfalls verwirft scale_x_continuous() das gesamte Rechteck,
  # sobald xmin (Griechenland) oder xmax (Österreich) außerhalb des Limits liegt.
  highlight_rect <- tibble(
    xmin = max(0.7, salient_x - 0.45),
    xmax = min(3.3, salient_x + 0.45),
    ymin = -1,
    ymax = 1
  )

  ggplot(dat, aes(x = axis_id, y = profile_mean)) +
    geom_rect(
      data = highlight_rect,
      aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
      inherit.aes = FALSE,
      fill = unname(hypothesis_fill[study_id]),
      alpha = 0.13,
      colour = NA
    ) +
    geom_hline(yintercept = 0, linewidth = 0.45, colour = "grey35") +
    geom_line(
      aes(
        group = lpa_class,
        colour = profile_label_plot
      ),
      linewidth = 0.85,
      lineend = "round"
    ) +
    geom_point(
      aes(
        colour = profile_label_plot,
        size = pct
      ),
      alpha = 0.95
    ) +
    scale_colour_manual(values = pal, drop = FALSE) +
    scale_size_continuous(
      range = c(2.5, 6.5),
      breaks = c(10, 20, 30, 40, 50, 60),
      labels = label_number(suffix = " %", decimal.mark = ",")
    ) +
    scale_x_continuous(
      breaks = 1:3,
      labels = labels_this_study,
      limits = c(0.7, 3.3),
      expand = expansion(mult = 0)
    ) +
    scale_y_continuous(
      limits = c(-1, 1),
      breaks = seq(-1, 1, by = 0.25),
      labels = label_number(decimal.mark = ",", accuracy = 0.01)
    ) +
    labs(
      title = unname(study_name_map[study_id]),
      x = NULL,
      y = if (study_id == studies[1]) "Profilmittelwert" else NULL,
      colour = NULL,
      size = "Profilanteil"
    ) +
    theme_minimal(base_size = 18) +
    theme(
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_blank(),
      plot.title = element_text(face = "bold", hjust = 0.5, size = 16),
      axis.text.x = element_text(face = "bold", size = 14),
      legend.position = "bottom",
      legend.box = "vertical",
      legend.text = element_text(size = 13),
      plot.margin = margin(8, 16, 8, 16)
    ) +
    guides(
      colour = guide_legend(
        ncol = 1,
        override.aes = list(linewidth = 1.1, size = 2.8)
      ),
      size = "none"
    )
}

profile_panels <- map(studies, make_profile_panel)

p_profile_grid <- plot_grid(
  plotlist = profile_panels,
  nrow = 1,
  align = "hv",
  axis = "tb"
)

p_profile_comparison <- ggdraw() +
  draw_plot(p_profile_grid, x = 0, y = 0, width = 1, height = 0.90) +
  draw_label(
    "Profilmittelwerte im Fallvergleich",
    x = 0.02, y = 0.992,
    hjust = 0, vjust = 1,
    fontface = "bold", size = 18
  ) +
  draw_label(
    paste(
      "Die Linien verbinden die drei Profilmittelwerte;",
      "die Hinterlegung markiert die jeweils hypothesenrelevante Dimension."
    ),
    x = 0.02, y = 0.952,
    hjust = 0, vjust = 1,
    size = 16
  )

ggsave(
  file.path(DIR_PLOTS_OUT, "02_profile_means_point_line_comparison.png"),
  p_profile_comparison,
  width = 20, height = 9.5, dpi = 320,
  bg = "white"
)

###############################################################################
## 12) GRAUZONENKENNWERTE UND LOLLIPOP-PLOT
###############################################################################

grey_zone_metrics <- profile_long %>%
  filter(class_label == unname(grey_zone_labels[study])) %>%
  group_by(study, study_label, class_label, pct) %>%
  summarise(
    mean_absolute_distance_from_zero = mean(abs(profile_mean)),
    max_absolute_distance_from_zero = max(abs(profile_mean)),
    rms_distance_from_zero = sqrt(mean(profile_mean^2)),
    .groups = "drop"
  ) %>%
  mutate(
    classic_grey_zone_score = 1 - mean_absolute_distance_from_zero
  )

write_csv(
  grey_zone_metrics,
  file.path(DIR_TABLES_OUT, "lpa_cross_case_grey_zone_metrics.csv")
)

grey_plot_long <- grey_zone_metrics %>%
  select(
    study, study_label, class_label, pct,
    mean_absolute_distance_from_zero,
    max_absolute_distance_from_zero
  ) %>%
  pivot_longer(
    cols = c(
      mean_absolute_distance_from_zero,
      max_absolute_distance_from_zero,
      pct
    ),
    names_to = "metric",
    values_to = "value"
  ) %>%
  mutate(
    metric = recode(
      metric,
      mean_absolute_distance_from_zero =
        "Mittlere absolute Distanz vom Nullpunkt",
      max_absolute_distance_from_zero =
        "Maximale absolute Distanz vom Nullpunkt",
      pct = "Profilanteil in Prozent"
    ),
    metric = factor(
      metric,
      levels = c(
        "Mittlere absolute Distanz vom Nullpunkt",
        "Maximale absolute Distanz vom Nullpunkt",
        "Profilanteil in Prozent"
      )
    ),
    study_label = factor(
      study_label,
      levels = rev(unname(study_name_map[studies]))
    ),
    value_label = if_else(
      metric == "Profilanteil in Prozent",
      paste0(number(value, accuracy = 0.1, decimal.mark = ","), " %"),
      number(value, accuracy = 0.01, decimal.mark = ",")
    )
  )

p_grey_zone <- ggplot(
  grey_plot_long,
  aes(y = study_label, x = value)
) +
  geom_segment(
    aes(x = 0, xend = value, yend = study_label),
    linewidth = 1.0,
    colour = "grey65",
    lineend = "round"
  ) +
  geom_point(
    size = 5.2,
    colour = "grey40"
  ) +
  geom_text(
    aes(label = value_label),
    hjust = -0.35,
    size = 5.0
  ) +
  facet_wrap(~ metric, scales = "free_x", nrow = 1) +
  scale_x_continuous(
    expand = expansion(mult = c(0, 0.22))
  ) +
  labs(
    title = "Vergleich der Grauzonenprofile",
    subtitle = paste(
      "Kleine Distanzwerte sprechen für schwache inhaltliche Konturierung;",
      "der Profilanteil beschreibt die empirische Reichweite der Grauzone."
    ),
    x = NULL,
    y = NULL
  ) +
  theme_minimal(base_size = 18) +
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    strip.text = element_text(face = "bold", size = 16),
    axis.text.y = element_text(face = "bold", size = 15),
    axis.text.x = element_text(size = 13),
    plot.title = element_text(face = "bold", size = 20),
    plot.subtitle = element_text(size = 16),
    plot.margin = margin(12, 28, 12, 18)
  )

ggsave(
  file.path(DIR_PLOTS_OUT, "05_grey_zone_comparison_lollipop.png"),
  p_grey_zone,
  width = 18, height = 7.2, dpi = 320,
  bg = "white"
)

###############################################################################
## 13) KONTROLLAUSGABE
###############################################################################

print(
  dimension_metrics_output %>%
    mutate(
      across(
        where(is.numeric),
        ~ round(.x, 3)
      )
    ),
  n = Inf
)

cat("\nErstellt:\n")
cat("  lpa_dimension_metrics_main.csv\n")
cat("  lpa_dimension_metrics_main_de.csv\n")
cat("  lpa_dimension_metrics_within_case_ranks.csv\n")
cat("  lpa_dimension_metrics_cross_case_ranks.csv\n")
cat("  lpa_dimension_metrics_pairwise_details.csv\n")
cat("  lpa_cross_case_grey_zone_metrics.csv\n")
cat("  02_profile_means_point_line_comparison.png\n")
cat("  05_grey_zone_comparison_lollipop.png\n\n")
