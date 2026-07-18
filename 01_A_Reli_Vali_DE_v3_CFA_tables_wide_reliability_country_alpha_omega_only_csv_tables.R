###############################################################################
## DISS_2026 — 01_A_Reli_Vali.R
## Reliabilität, CFA & EFA
## Konstrukte: Exclusionist Democracy (EXCL), Populist Democracy (POPU),
##             Pragmatist Democracy (PRG)
## Fälle:      AUT_2017, DEU_2021, GRC_2019
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
## 0) GLOBALE EINSTELLUNGEN
###############################################################################

DIR_OUT  <- "/Users/lukas/Desktop/Dissertation/DISS_2026/outputs/reli_vali_DE_v3_CFA_tables_wide"
DIR_PNG  <- file.path(DIR_OUT, "png")
DIR_PNGC <- file.path(DIR_OUT, "png_cfa")
DIR_PNGL <- file.path(DIR_OUT, "png_labels")
DIR_CSV  <- file.path(DIR_OUT, "csv")

for (d in c(DIR_OUT, DIR_PNG, DIR_PNGC, DIR_PNGL, DIR_CSV)) {
  dir.create(d, recursive = TRUE, showWarnings = FALSE)
}

ITALIC_CROSSLOADINGS <- TRUE
CUTOFF               <- 0.30

expected_studies <- c("AUT_2017",
                      "DEU_2021",
                      "GRC_2019")
factor_names     <- c("EXCL", "POPU", "PRG")

construct_labels <- c(
  EXCL = "Exklusionistische Demokratie",
  POPU = "Populistische Demokratie",
  PRG  = "Pragmatische Demokratie"
)

study_labels <- c(
  AUT_2017 = "Österreich 2017",
  DEU_2021 = "Deutschland 2021",
  GRC_2019 = "Griechenland 2019"
)

###############################################################################
## 1) DATEN & ITEMS
###############################################################################

CSES5 <- readRDS(here(
  "data", "processed", "cses5",
  "derived",
  "CSES5_20262301_prepared.rds"
))

stopifnot(
  "election_study_id" %in% names(CSES5),
  "respondent_id"     %in% names(CSES5)
)

missing_studies <- setdiff(
  expected_studies,
  unique(CSES5$election_study_id)
)
if (length(missing_studies) > 0) {
  stop("Fehlende Studien: ",
       paste(missing_studies,
             collapse = ", "))
}

CSES5_sub <- CSES5 %>%
  filter(
    election_study_id %in% expected_studies
  )
cat("N je Studie:\n")
print(table(CSES5_sub$election_study_id))

# -------------------------------------------------------
# Item-Pools (vollständig)
# -------------------------------------------------------
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

all_items <- c(
  all_items_excl,
  all_items_popu,
  all_items_prg
)

# -------------------------------------------------------
# Länderspezifische Messmodelle (nach EFA angepasst)
# -------------------------------------------------------
study_models <- list(

  # AUT: nach EFA angepasst
  #   POPU: people_make_policy_rev entfernt
  #   PRG:  nur voting + power (comp_sellout
  #         + pol_trustworthy_rev entfernt)
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
    PRG  = list(
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
    PRG  = list(
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
    PRG  = list(
      items      = all_items_prg,
      label      = "Pragmatische Demokratie",
      score_name = "PRG_score"
    )
  )
)

# -------------------------------------------------------
# EFA Item-Metadaten (für EFA: alle 14 Items)
# -------------------------------------------------------
item_labels_short <- c(
  paste0("EXCL", 1:5),
  paste0("POPU", 1:5),
  paste0("PRG",  1:4)
)
item_labels_long <- c(
  "Mehrheit soll sich gegenüber Minderheiten durchsetzen (rekodiert)",
  "Minderheiten sollen sich nationalen Gepflogenheiten anpassen (rekodiert)",
  "Befolgung nationaler Gepflogenheiten ist wichtig (rekodiert)",
  "Einwanderer schaden der nationalen Kultur (rekodiert)",
  "Im Land geboren zu sein ist wichtig (rekodiert)",
  "Das Volk, nicht Politiker, soll Politik bestimmen (rekodiert)",
  "Politiker kümmern sich nicht um die Menschen (rekodiert)",
  "Politiker dienen nur Reichen und Mächtigen (rekodiert)",
  "Politische Korruption ist weit verbreitet (rekodiert)",
  "Politiker sind das Hauptproblem (rekodiert)",
  "Ablehnung von: Kompromiss bedeutet Verrat an eigenen Prinzipien",
  "Es macht einen großen Unterschied, wer politische Macht ausübt",
  "Wählen macht einen großen Unterschied",
  "Die meisten Politiker sind vertrauenswürdig (rekodiert)"
)
item_construct <- c(
  rep("Exklusionistische Demokratie", 5),
  rep("Populistische Demokratie",     5),
  rep("Pragmatische Demokratie",   4)
)
item_factor <- c(
  rep(1, 5),
  rep(2, 5),
  rep(3, 4)
)

# Zielmatrix für Target Rotation
target_matrix <- matrix(
  c(1,0,0, 1,0,0, 1,0,0, 1,0,0, 1,0,0,
    0,1,0, 0,1,0, 0,1,0, 0,1,0, 0,1,0,
    0,0,1, 0,0,1, 0,0,1, 0,0,1),
  nrow = 14, ncol = 3, byrow = TRUE
)

###############################################################################
## 2) CFA MODELLE (länderspezifisch aus study_models)
###############################################################################

# Hilfsfunktion: lavaan Syntax aus study_models bauen
build_cfa_model <- function(study,
                            add_residuals = FALSE) {

  sm <- study_models[[study]]

  excl_items <- paste(
    sm$EXCL$items, collapse = " + "
  )
  popu_items <- paste(
    sm$POPU$items, collapse = " + "
  )
  prg_items  <- paste(
    sm$PRG$items,  collapse = " + "
  )

  base <- sprintf(
    "EXCL =~ %s\nPOPU =~ %s\nPRG  =~ %s",
    excl_items, popu_items, prg_items
  )

  if (!add_residuals) return(base)

  # Correlated residuals
  # (nur wenn Items vorhanden)
  res <- c()

  # EXCL: ascriptive & cultural belonging
  excl <- sm$EXCL$items
  if (all(c("born_country_rev",
            "follow_customs_rev") %in% excl)) {
    res <- c(res,
             "born_country_rev ~~ follow_customs_rev")
  }
  if (all(c("born_country_rev",
            "minorities_adapt_rev") %in% excl)) {
    res <- c(res,
             "born_country_rev ~~ minorities_adapt_rev")
  }
  if (all(c("follow_customs_rev",
            "minorities_adapt_rev") %in% excl)) {
    res <- c(res,
             "follow_customs_rev ~~ minorities_adapt_rev")
  }

  # POPU: elite alienation
  popu <- sm$POPU$items
  if (all(c("pol_no_care_rev",
            "pol_for_rich_rev") %in% popu)) {
    res <- c(res,
             "pol_no_care_rev ~~ pol_for_rich_rev")
  }
  if (all(c("pol_no_care_rev",
            "pol_main_problem_rev") %in% popu)) {
    res <- c(res,
             "pol_no_care_rev ~~ pol_main_problem_rev")
  }

  # PRG: external efficacy
  prg <- sm$PRG$items
  if (all(c("voting_makes_difference",
            "power_makes_difference") %in% prg)) {
    res <- c(res,
             "voting_makes_difference ~~ power_makes_difference")
  }

  if (length(res) > 0) {
    paste(base,
          "\n# Correlated residuals",
          paste(res, collapse = "\n"),
          sep = "\n")
  } else {
    base
  }
}

###############################################################################
## 3) HILFSFUNKTIONEN
###############################################################################

align_factors <- function(loadings_raw,
                          target_matrix,
                          factor_names) {
  cong      <- psych::factor.congruence(
    loadings_raw, target_matrix
  )
  order_idx <- apply(abs(cong), 2, which.max)
  if (length(unique(order_idx)) <
      length(order_idx)) {
    warning("Faktor-Zuordnung nicht eindeutig")
  }
  loadings_ordered <- loadings_raw[
    , order_idx, drop = FALSE
  ]
  colnames(loadings_ordered) <- factor_names
  for (j in seq_along(factor_names)) {
    target_items <- which(
      target_matrix[, j] == 1
    )
    if (mean(loadings_ordered[target_items, j],
             na.rm = TRUE) < 0) {
      loadings_ordered[, j] <-
        loadings_ordered[, j] * -1
      cat("Vorzeichen gespiegelt:",
          factor_names[j], "\n")
    }
  }
  list(loadings  = loadings_ordered,
       order_idx = order_idx)
}

save_png <- function(tbl,
                     filename,
                     dir    = DIR_PNG,
                     expand = 15) {
  path <- file.path(dir, filename)
  gt::gtsave(tbl,
             filename = path,
             expand   = expand)
  cat("Gespeichert:", path, "\n")
}

build_efa_gt <- function(loadings_ord,
                         item_labels_short,
                         item_construct,
                         item_factor,
                         eigenvalues,
                         var_explained,
                         factor_names,
                         study,
                         rotation_label,
                         cutoff      = 0.30,
                         italic_cross = TRUE) {

  n_items <- nrow(loadings_ord)
  df <- as.data.frame(
    matrix(
      sprintf("%.3f", loadings_ord),
      nrow = n_items
    ),
    stringsAsFactors = FALSE
  )
  colnames(df) <- factor_names
  df <- df %>%
    mutate(
      item_label = item_labels_short,
      construct  = item_construct,
      .before    = everything()
    )

  footer <- tibble(
    construct  = c("", ""),
    item_label = c("Eigenwert",
                   "Erklärte Varianz")
  )
  for (j in seq_along(factor_names)) {
    footer[[factor_names[j]]] <- c(
      sprintf("%.2f", eigenvalues[j]),
      sprintf("%.1f%%", var_explained[j])
    )
  }

  tbl <- bind_rows(df, footer) %>%
    gt(rowname_col  = "item_label",
       groupname_col = "construct") %>%
    tab_header(
      title    = paste("EFA-Ladungsmatrix —",
                       rotation_label),
      subtitle = study_labels[study]
    ) %>%
    tab_stubhead(label = "Item") %>%
    cols_label(
      EXCL = "Exklusionistische Demokratie",
      POPU = "Populistische Demokratie",
      PRG  = "Pragmatische Demokratie"
    ) %>%
    tab_style(
      cell_text(weight = "bold"),
      cells_column_labels()
    ) %>%
    tab_style(
      cell_text(weight = "bold"),
      cells_row_groups()
    ) %>%
    tab_style(
      cell_borders(sides  = "top",
                   weight = px(2)),
      cells_body(rows = n_items + 1)
    ) %>%
    tab_footnote(paste0(
      "Fett = primäre Ladung (|λ| ≥ ",
      cutoff, "). ",
      ifelse(italic_cross,
             paste0("Kursiv = Kreuzladung (|λ| ≥ ",
                    cutoff, ")."),
             paste0("Kreuzladungen (|λ| ≥ ",
                    cutoff, ") in normaler Schrift."))
    ))

  for (i in seq_len(n_items)) {
    for (j in seq_along(factor_names)) {
      val <- loadings_ord[i, j]
      if (item_factor[i] == j &
          abs(val) >= cutoff) {
        tbl <- tbl %>%
          tab_style(
            cell_text(weight = "bold"),
            cells_body(
              columns = factor_names[j],
              rows    = i
            )
          )
      } else if (item_factor[i] != j &
                 abs(val) >= cutoff &
                 italic_cross) {
        tbl <- tbl %>%
          tab_style(
            cell_text(style = "italic"),
            cells_body(
              columns = factor_names[j],
              rows    = i
            )
          )
      }
    }
  }
  tbl
}

build_efa_gt_labels <- function(loadings_ord,
                                item_labels_short,
                                item_labels_long,
                                item_construct,
                                item_factor,
                                eigenvalues,
                                var_explained,
                                factor_names,
                                study,
                                cutoff       = 0.30,
                                italic_cross = TRUE) {

  n_items <- nrow(loadings_ord)
  df <- as.data.frame(
    matrix(
      sprintf("%.3f", loadings_ord),
      nrow = n_items
    ),
    stringsAsFactors = FALSE
  )
  colnames(df) <- factor_names
  df <- df %>%
    mutate(
      item_short = item_labels_short,
      item_label = item_labels_long,
      construct  = item_construct,
      .before    = everything()
    )

  footer <- tibble(
    construct  = c("", ""),
    item_short = c("Eigenwert",
                   "Erklärte Varianz"),
    item_label = c("", "")
  )
  for (j in seq_along(factor_names)) {
    footer[[factor_names[j]]] <- c(
      sprintf("%.2f", eigenvalues[j]),
      sprintf("%.1f%%", var_explained[j])
    )
  }

  tbl <- bind_rows(df, footer) %>%
    gt(rowname_col  = "item_short",
       groupname_col = "construct") %>%
    tab_header(
      title    = paste(
        "EFA-Ladungsmatrix —",
        "Zielrotation (TargetQ, oblique)"
      ),
      subtitle = study_labels[study]
    ) %>%
    tab_stubhead(label = "Item") %>%
    cols_label(
      item_label = "Itemformulierung",
      EXCL = "Exklusionistische Demokratie",
      POPU = "Populistische Demokratie",
      PRG  = "Pragmatische Demokratie"
    ) %>%
    tab_style(
      cell_text(weight = "bold"),
      cells_column_labels()
    ) %>%
    tab_style(
      cell_text(weight = "bold"),
      cells_row_groups()
    ) %>%
    tab_style(
      cell_text(size = px(16)),
      cells_body(columns = "item_label")
    ) %>%
    tab_style(
      cell_borders(sides  = "top",
                   weight = px(2)),
      cells_body(rows = n_items + 1)
    ) %>%
    cols_width(
      item_label ~ px(320),
      EXCL ~ px(80),
      POPU ~ px(80),
      PRG  ~ px(80)
    ) %>%
    tab_footnote(paste0(
      "Die Items sind so ausgerichtet, dass hohe Skalenwerte hohe Zustimmung zum jeweiligen Konstrukt anzeigen. ",
      "(rekodiert) = umgepolt. ",
      "Fett = primäre Ladung (|λ| ≥ ",
      cutoff, "). ",
      ifelse(italic_cross,
             paste0("Kursiv = Kreuzladung (|λ| ≥ ",
                    cutoff, ")."),
             paste0("Kreuzladungen (|λ| ≥ ",
                    cutoff, ") in normaler Schrift."))
    ))

  for (i in seq_len(n_items)) {
    for (j in seq_along(factor_names)) {
      val <- loadings_ord[i, j]
      if (item_factor[i] == j &
          abs(val) >= cutoff) {
        tbl <- tbl %>%
          tab_style(
            cell_text(weight = "bold"),
            cells_body(
              columns = factor_names[j],
              rows    = i
            )
          )
      } else if (item_factor[i] != j &
                 abs(val) >= cutoff &
                 italic_cross) {
        tbl <- tbl %>%
          tab_style(
            cell_text(style = "italic"),
            cells_body(
              columns = factor_names[j],
              rows    = i
            )
          )
      }
    }
  }
  tbl
}

# ----------------------------------------------------------
# EFA-Tabellen zusätzlich als CSV vorbereiten
# ----------------------------------------------------------
make_efa_csv_table <- function(loadings_ord,
                               item_labels_short,
                               item_labels_long,
                               item_construct,
                               item_factor,
                               eigenvalues,
                               var_explained,
                               factor_names,
                               study,
                               rotation_label) {

  n_items <- nrow(loadings_ord)

  load_df <- as_tibble(
    round(as.data.frame(loadings_ord), 3)
  )
  names(load_df) <- factor_names

  load_df <- load_df %>%
    mutate(
      study          = study,
      study_label    = unname(study_labels[study]),
      rotation       = rotation_label,
      row_type       = "loading",
      construct      = item_construct,
      item_short     = item_labels_short,
      item_label     = item_labels_long,
      primary_factor = factor_names[item_factor],
      .before        = 1
    )

  footer_eigen <- tibble(
    study          = study,
    study_label    = unname(study_labels[study]),
    rotation       = rotation_label,
    row_type       = "eigenvalue",
    construct      = "",
    item_short     = "Eigenwert",
    item_label     = "",
    primary_factor = NA_character_
  )
  footer_var <- tibble(
    study          = study,
    study_label    = unname(study_labels[study]),
    rotation       = rotation_label,
    row_type       = "variance_pct",
    construct      = "",
    item_short     = "Erklärte Varianz (%)",
    item_label     = "",
    primary_factor = NA_character_
  )

  for (j in seq_along(factor_names)) {
    footer_eigen[[factor_names[j]]] <- round(eigenvalues[j], 3)
    footer_var[[factor_names[j]]]   <- round(var_explained[j], 1)
  }

  bind_rows(load_df, footer_eigen, footer_var)
}

###############################################################################
## 4) HAUPTSCHLEIFE: Reliabilität + EFA
###############################################################################

fit_results    <- list()
reli_results   <- list()
efa_tables     <- list()
efa_csv_tables <- list()

for (study in expected_studies) {

  cat("\n============================================================\n")
  cat(study, "\n")
  cat("============================================================\n")

  dat_study <- CSES5_sub %>%
    filter(election_study_id == study) %>%
    select(all_of(all_items)) %>%
    drop_na()
  cat("N:", nrow(dat_study), "\n")

  # ----------------------------------------------------------
  # 4a) Reliabilität (länderspezifische Items)
  # ----------------------------------------------------------
  sm <- study_models[[study]]
  items_model <- unique(c(
    sm$EXCL$items,
    sm$POPU$items,
    sm$PRG$items
  ))
  dat_model <- CSES5_sub %>%
    filter(election_study_id == study) %>%
    select(all_of(items_model)) %>%
    drop_na()

  for (construct in names(sm)) {
    items_c <- sm[[construct]]$items
    dat_c   <- dat_model %>%
      select(all_of(items_c))

    alpha_res <- psych::alpha(
      dat_c, warnings = FALSE
    )
    omega_res <- tryCatch(
      psych::omega(
        dat_c,
        nfactors = 1,
        plot     = FALSE,
        warnings = FALSE
      ),
      error = function(e) NULL
    )

    reli_results[[
      paste(study, construct, sep = "_")
    ]] <- tibble(
      study     = study,
      construct = construct,
      label     = sm[[construct]]$label,
      n         = nrow(dat_c),
      alpha     = round(
        alpha_res$total$raw_alpha, 3
      ),
      alpha_std = round(
        alpha_res$total$std.alpha, 3
      ),
      omega_h   = if (!is.null(omega_res))
        round(omega_res$omega_h,   3)
      else NA_real_,
      omega_t   = if (!is.null(omega_res))
        round(omega_res$omega.tot, 3)
      else NA_real_
    )
  }

  # ----------------------------------------------------------
  # 4b) EFA — Varimax
  # ----------------------------------------------------------
  fa_v <- psych::fa(
    dat_study,
    nfactors = 3,
    rotate   = "varimax",
    fm       = "minres"
  )
  aligned_v <- align_factors(
    as.matrix(fa_v$loadings),
    target_matrix,
    factor_names
  )
  load_v <- aligned_v$loadings
  ev_v   <- fa_v$values[1:3]
  var_v  <- round(
    colSums(load_v^2) / nrow(load_v) * 100, 1
  )

  gt_v <- build_efa_gt(
    load_v, item_labels_short,
    item_construct, item_factor,
    ev_v, var_v, factor_names, study,
    "Varimax (orthogonal)",
    CUTOFF, ITALIC_CROSSLOADINGS
  )

  # ----------------------------------------------------------
  # 4c) EFA — Oblimin
  # ----------------------------------------------------------
  fa_o <- psych::fa(
    dat_study,
    nfactors = 3,
    rotate   = "oblimin",
    fm       = "minres"
  )
  aligned_o <- align_factors(
    as.matrix(fa_o$loadings),
    target_matrix,
    factor_names
  )
  load_o <- aligned_o$loadings
  ev_o   <- fa_o$values[1:3]
  var_o  <- round(
    colSums(load_o^2) / nrow(load_o) * 100, 1
  )

  if (!is.null(fa_o$Phi)) {
    phi_o <- fa_o$Phi[
      aligned_o$order_idx,
      aligned_o$order_idx
    ]
    rownames(phi_o) <- colnames(phi_o) <-
      factor_names
    cat("Faktorkorrelationen (Oblimin):\n")
    print(round(phi_o, 3))
  }

  gt_o <- build_efa_gt(
    load_o, item_labels_short,
    item_construct, item_factor,
    ev_o, var_o, factor_names, study,
    "Oblimin (oblique)",
    CUTOFF, ITALIC_CROSSLOADINGS
  )

  # ----------------------------------------------------------
  # 4d) EFA — Target (TargetQ)
  # ----------------------------------------------------------
  fa_unrot <- psych::fa(
    dat_study,
    nfactors = 3,
    rotate   = "none",
    fm       = "minres"
  )
  rotated_t <- GPArotation::GPFoblq(
    A          = fa_unrot$loadings,
    method     = "targetQ",
    methodArgs = list(Target = target_matrix)
  )
  load_raw_t           <- rotated_t$loadings
  rownames(load_raw_t) <- all_items
  aligned_t <- align_factors(
    load_raw_t, target_matrix, factor_names
  )
  load_t <- aligned_t$loadings
  ev_t   <- fa_unrot$values[1:3]
  var_t  <- round(
    colSums(load_t^2) / nrow(load_t) * 100, 1
  )

  tucker_phi <- round(
    diag(psych::factor.congruence(
      load_t, target_matrix
    )), 3
  )
  names(tucker_phi) <- factor_names
  cat("Tucker's phi:\n")
  print(tucker_phi)

  if (!is.null(rotated_t$Phi)) {
    phi_t <- rotated_t$Phi[
      aligned_t$order_idx,
      aligned_t$order_idx
    ]
    rownames(phi_t) <- colnames(phi_t) <-
      factor_names
    cat("Faktorkorrelationen (Target):\n")
    print(round(phi_t, 3))
  }

  gt_t <- build_efa_gt(
    load_t, item_labels_short,
    item_construct, item_factor,
    ev_t, var_t, factor_names, study,
    "Zielrotation (TargetQ, oblique)",
    CUTOFF, ITALIC_CROSSLOADINGS
  )
  gt_t_labels <- build_efa_gt_labels(
    load_t, item_labels_short,
    item_labels_long, item_construct,
    item_factor, ev_t, var_t,
    factor_names, study,
    CUTOFF, ITALIC_CROSSLOADINGS
  )

  efa_tables[[study]] <- list(
    varimax    = gt_v,
    oblimin    = gt_o,
    target     = gt_t,
    target_lbl = gt_t_labels
  )

  efa_csv_tables[[study]] <- list(
    varimax = make_efa_csv_table(
      load_v, item_labels_short, item_labels_long,
      item_construct, item_factor,
      ev_v, var_v, factor_names, study,
      "Varimax (orthogonal)"
    ),
    oblimin = make_efa_csv_table(
      load_o, item_labels_short, item_labels_long,
      item_construct, item_factor,
      ev_o, var_o, factor_names, study,
      "Oblimin (oblique)"
    ),
    target = make_efa_csv_table(
      load_t, item_labels_short, item_labels_long,
      item_construct, item_factor,
      ev_t, var_t, factor_names, study,
      "Zielrotation (TargetQ, oblique)"
    )
  )

  # ----------------------------------------------------------
  # 4e) CFA — läuft NACH EFA
  #     mit länderspezifischen Items
  # ----------------------------------------------------------
  cat("\n--- CFA:", study, "---\n")

  model_base_s <- build_cfa_model(
    study,
    add_residuals = FALSE
  )
  model_mod_s  <- build_cfa_model(
    study,
    add_residuals = TRUE
  )

  cat("Basismodell:\n")
  cat(model_base_s, "\n\n")
  cat("Modifiziertes Modell:\n")
  cat(model_mod_s,  "\n\n")

  # CFA Daten: nur Items die im
  # länderspezifischen Modell sind
  items_study <- unique(c(
    sm$EXCL$items,
    sm$POPU$items,
    sm$PRG$items
  ))
  dat_cfa <- CSES5_sub %>%
    filter(election_study_id == study) %>%
    select(all_of(items_study)) %>%
    drop_na()
  cat("N CFA:", nrow(dat_cfa), "\n")

  fit_base <- lavaan::cfa(
    model_base_s,
    data      = dat_cfa,
    estimator = "MLR",
    std.lv    = TRUE
  )
  fit_mod <- lavaan::cfa(
    model_mod_s,
    data      = dat_cfa,
    estimator = "MLR",
    std.lv    = TRUE
  )

  idx_names <- c(
    "chisq", "df", "cfi", "tli",
    "rmsea", "rmsea.ci.lower",
    "rmsea.ci.upper", "srmr"
  )
  idx_b <- lavaan::fitMeasures(
    fit_base, idx_names
  )
  idx_m <- lavaan::fitMeasures(
    fit_mod, idx_names
  )

  cat(sprintf(
    "CFA Basismodell: CFI=%.3f | RMSEA=%.3f | SRMR=%.3f\n",
    idx_b["cfi"], idx_b["rmsea"],
    idx_b["srmr"]
  ))
  cat(sprintf(
    "CFA modifiziertes Modell: CFI=%.3f | RMSEA=%.3f | SRMR=%.3f\n",
    idx_m["cfi"], idx_m["rmsea"],
    idx_m["srmr"]
  ))

  fit_results[[study]] <- tibble(
    study    = study,
    model    = c("Basismodell", "Modifiziertes Modell"),
    chisq    = round(c(
      idx_b["chisq"], idx_m["chisq"]
    ), 2),
    df       = as.integer(c(
      idx_b["df"], idx_m["df"]
    )),
    cfi      = round(c(
      idx_b["cfi"], idx_m["cfi"]
    ), 3),
    tli      = round(c(
      idx_b["tli"], idx_m["tli"]
    ), 3),
    rmsea    = round(c(
      idx_b["rmsea"], idx_m["rmsea"]
    ), 3),
    rmsea_lo = round(c(
      idx_b["rmsea.ci.lower"],
      idx_m["rmsea.ci.lower"]
    ), 3),
    rmsea_hi = round(c(
      idx_b["rmsea.ci.upper"],
      idx_m["rmsea.ci.upper"]
    ), 3),
    srmr     = round(c(
      idx_b["srmr"], idx_m["srmr"]
    ), 3)
  )
}

###############################################################################
## 5) PNG OUTPUTS — EFA
###############################################################################

cat("\n--- Speichere EFA PNGs ---\n")

for (study in expected_studies) {
  save_png(
    efa_tables[[study]]$varimax,
    paste0("efa_varimax_", study, ".png"),
    dir = DIR_PNG
  )
  save_png(
    efa_tables[[study]]$oblimin,
    paste0("efa_oblimin_", study, ".png"),
    dir = DIR_PNG
  )
  save_png(
    efa_tables[[study]]$target,
    paste0("efa_target_", study, ".png"),
    dir = DIR_PNG
  )
  save_png(
    efa_tables[[study]]$target_lbl,
    paste0("efa_target_labels_",
           study, ".png"),
    dir = DIR_PNGL
  )
}

###############################################################################
## 5b) CSV OUTPUTS — EFA
###############################################################################

cat("\n--- Speichere EFA CSVs ---\n")

for (study in expected_studies) {
  readr::write_csv(
    efa_csv_tables[[study]]$varimax,
    file.path(DIR_CSV, paste0("efa_varimax_", study, ".csv"))
  )
  readr::write_csv(
    efa_csv_tables[[study]]$oblimin,
    file.path(DIR_CSV, paste0("efa_oblimin_", study, ".csv"))
  )
  readr::write_csv(
    efa_csv_tables[[study]]$target,
    file.path(DIR_CSV, paste0("efa_target_", study, ".csv"))
  )

  # Alias für die Target-Tabelle mit langen Labels; beide Dateien
  # enthalten item_short und item_label und können im QMD unterschiedlich
  # gesetzt werden.
  readr::write_csv(
    efa_csv_tables[[study]]$target,
    file.path(DIR_CSV, paste0("efa_target_labels_", study, ".csv"))
  )

  cat("Gespeichert:", file.path(DIR_CSV, paste0("efa_varimax_", study, ".csv")), "\n")
  cat("Gespeichert:", file.path(DIR_CSV, paste0("efa_oblimin_", study, ".csv")), "\n")
  cat("Gespeichert:", file.path(DIR_CSV, paste0("efa_target_", study, ".csv")), "\n")
  cat("Gespeichert:", file.path(DIR_CSV, paste0("efa_target_labels_", study, ".csv")), "\n")
}

###############################################################################
## 6) PNG OUTPUTS — CFA: Tabelle A (Fit-Indizes)
###############################################################################

cat("\n--- Speichere CFA PNGs ---\n")

fit_df_raw <- bind_rows(fit_results) %>%
  mutate(
    study_label = unname(study_labels[study])
  ) %>%
  select(
    study, study_label, model,
    chisq, df, cfi, tli,
    rmsea, rmsea_lo, rmsea_hi, srmr
  )

fit_df <- fit_df_raw %>%
  mutate(
    chisq_str   = sprintf("%.2f", chisq),
    rmsea_str   = sprintf(
      "%.3f [%.3f, %.3f]",
      rmsea, rmsea_lo, rmsea_hi
    )
  ) %>%
  select(
    study_label, model,
    chisq_str, df,
    cfi, tli,
    rmsea_str, srmr
  )

# ----------------------------------------------------------
# CSV OUTPUTS — CFA-Fit
# ----------------------------------------------------------
# Rohformat: numerische Spalten für flexible QMD-/LaTeX-Tabellen
readr::write_csv(
  fit_df_raw,
  file.path(DIR_CSV, "tableA_cfa_fit_indices_raw.csv")
)

# Anzeigeformat: entspricht der PNG-Tabelle mit formatiertem RMSEA-KI
readr::write_csv(
  fit_df %>%
    rename(
      Studie = study_label,
      Modell = model,
      `χ²` = chisq_str,
      `RMSEA [90%-KI]` = rmsea_str,
      CFI = cfi,
      TLI = tli,
      SRMR = srmr
    ),
  file.path(DIR_CSV, "tableA_cfa_fit_indices.csv")
)

cat("Gespeichert:", file.path(DIR_CSV, "tableA_cfa_fit_indices_raw.csv"), "\n")
cat("Gespeichert:", file.path(DIR_CSV, "tableA_cfa_fit_indices.csv"), "\n")

tbl_A <- fit_df %>%
  gt(groupname_col = "study_label") %>%
  tab_header(
    title    = "CFA-Modellfit-Indizes",
    subtitle = paste(
      "Basismodell vs. modifiziertes Modell —",
      "Österreich 2017, Deutschland 2021,",
      "Griechenland 2019"
    )
  ) %>%
  cols_label(
    model     = "Modell",
    chisq_str = "χ²",
    df        = "df",
    cfi       = "CFI",
    tli       = "TLI",
    rmsea_str = "RMSEA [90%-KI]",
    srmr      = "SRMR"
  ) %>%
  tab_style(
    cell_text(weight = "bold"),
    cells_column_labels()
  ) %>%
  tab_style(
    cell_text(weight = "bold"),
    cells_row_groups()
  ) %>%
  tab_footnote(paste(
    "Schätzung: MLR (Maximum-Likelihood-Schätzung).",
    "CFI/TLI: ≥ .95 = gut, ≥ .90 = akzeptabel.",
    "RMSEA: < .06 = gut, < .08 = akzeptabel.",
    "SRMR: < .08 = gut.",
    "Das modifizierte Modell enthält theoretisch begründete korrelierte Residuen.",
    "AUT_2017: POPU = anti-elitistisch (4 Items),",
    "PRG = minimal-pragmatisch (2 Items).",
    "DEU_2021/GRC_2019: vollständige Itemsets."
  )) %>%
  cols_width(
    model     ~ px(125),
    chisq_str ~ px(95),
    df        ~ px(55),
    cfi       ~ px(75),
    tli       ~ px(75),
    rmsea_str ~ px(230),
    srmr      ~ px(75)
  ) %>%
  tab_options(table.width = px(820))

save_png(
  tbl_A,
  "tableA_cfa_fit_indices.png",
  dir = DIR_PNGC
)

###############################################################################
## 7) PNG OUTPUTS — CFA: Tabelle B (Reliabilität)
###############################################################################

reli_df <- bind_rows(reli_results) %>%
  mutate(
    study_label = unname(study_labels[study])
  ) %>%
  arrange(study, construct) %>%
  transmute(
    study,
    study_label,
    construct,
    label,
    n,
    alpha = alpha,
    omega = omega_t
  )

# ----------------------------------------------------------
# CSV OUTPUTS — Reliabilität
# ----------------------------------------------------------
# Rohformat: länderspezifische Konstruktschlüssel bleiben erhalten
readr::write_csv(
  reli_df,
  file.path(DIR_CSV, "tableB_reliability_raw.csv")
)

# Anzeigeformat: entspricht der PNG-Tabelle
readr::write_csv(
  reli_df %>%
    select(study_label, label, n, alpha, omega) %>%
    rename(
      Studie = study_label,
      Konstrukt = label,
      N = n,
      `Cronbachs α` = alpha,
      `McDonalds ω` = omega
    ),
  file.path(DIR_CSV, "tableB_reliability.csv")
)

cat("Gespeichert:", file.path(DIR_CSV, "tableB_reliability_raw.csv"), "\n")
cat("Gespeichert:", file.path(DIR_CSV, "tableB_reliability.csv"), "\n")

tbl_B <- reli_df %>%
  select(-study, -construct) %>%
  gt(groupname_col = "study_label") %>%
  tab_header(
    title    = "Reliabilitätsschätzungen",
    subtitle = paste(
      "Cronbachs α und McDonalds ω —",
      "Österreich 2017, Deutschland 2021,",
      "Griechenland 2019"
    )
  ) %>%
  cols_label(
    label     = "Konstrukt",
    n         = "N",
    alpha     = "Cronbachs α",
    omega     = "McDonalds ω"
  ) %>%
  tab_style(
    cell_text(weight = "bold"),
    cells_column_labels()
  ) %>%
  tab_style(
    cell_text(weight = "bold"),
    cells_row_groups()
  ) %>%
  fmt_missing(
    columns      = everything(),
    missing_text = "—"
  ) %>%
  tab_footnote(paste(
    "α = Cronbachs Alpha.",
    "ω = McDonalds Omega.",
    "Akzeptable Reliabilität: α / ω ≥ .60.",
    "AUT_2017: länderspezifische Itemsets",
    "nach EFA-Anpassung."
  )) %>%
  cols_width(
    label     ~ px(300),
    n         ~ px(80),
    alpha     ~ px(120),
    omega     ~ px(120)
  ) %>%
  tab_options(table.width = px(820))

save_png(
  tbl_B,
  "tableB_reliability.png",
  dir = DIR_PNGC
)

for (study_i in expected_studies) {
  reli_country_df <- reli_df %>%
    filter(study == study_i) %>%
    select(label, n, alpha, omega)

  readr::write_csv(
    reli_country_df %>%
      rename(
        Konstrukt = label,
        N = n,
        `Cronbachs α` = alpha,
        `McDonalds ω` = omega
      ),
    file.path(DIR_CSV, paste0("tableB_reliability_", study_i, ".csv"))
  )
  cat("Gespeichert:", file.path(DIR_CSV, paste0("tableB_reliability_", study_i, ".csv")), "\n")

  tbl_B_country <- reli_country_df %>%
    gt() %>%
    tab_header(
      title    = "Reliabilitätsschätzungen",
      subtitle = paste(
        "Cronbachs α und McDonalds ω —",
        study_labels[study_i]
      )
    ) %>%
    cols_label(
      label   = "Konstrukt",
      n       = "N",
      alpha   = "Cronbachs α",
      omega   = "McDonalds ω"
    ) %>%
    tab_style(
      cell_text(weight = "bold"),
      cells_column_labels()
    ) %>%
    fmt_missing(
      columns      = everything(),
      missing_text = "—"
    ) %>%
    tab_footnote(paste(
      "α = Cronbachs Alpha.",
      "ω = McDonalds Omega.",
      "Akzeptable Reliabilität: α / ω ≥ .60."
    )) %>%
    cols_width(
      label   ~ px(300),
      n       ~ px(80),
      alpha   ~ px(120),
      omega   ~ px(120)
    ) %>%
    tab_options(table.width = px(820))

  save_png(
    tbl_B_country,
    paste0("tableB_reliability_", study_i, ".png"),
    dir = DIR_PNGC
  )
}

###############################################################################
## 8) FERTIG
###############################################################################

cat("\n============================================================\n")
cat("Alle Outputs gespeichert.\n\n")
cat("EFA CSVs (", DIR_CSV, "):\n")
for (study in expected_studies) {
  cat("  efa_varimax_",       study, ".csv\n", sep="")
  cat("  efa_oblimin_",       study, ".csv\n", sep="")
  cat("  efa_target_",        study, ".csv\n", sep="")
  cat("  efa_target_labels_", study, ".csv\n", sep="")
}
cat("\nEFA PNGs (", DIR_PNG, "):\n")
for (study in expected_studies) {
  cat("  efa_varimax_",       study, ".png\n", sep="")
  cat("  efa_oblimin_",       study, ".png\n", sep="")
  cat("  efa_target_",        study, ".png\n", sep="")
}
cat("\nEFA Label PNGs (", DIR_PNGL, "):\n")
for (study in expected_studies) {
  cat("  efa_target_labels_", study, ".png\n", sep="")
}
cat("\nCFA/Reliabilität CSVs (", DIR_CSV, "):\n")
cat("  tableA_cfa_fit_indices.csv\n")
cat("  tableA_cfa_fit_indices_raw.csv\n")
cat("  tableB_reliability.csv\n")
cat("  tableB_reliability_raw.csv\n")
for (study in expected_studies) {
  cat("  tableB_reliability_", study, ".csv\n", sep="")
}
cat("\nCFA PNGs (", DIR_PNGC, "):\n")
cat("  tableA_cfa_fit_indices.png\n")
cat("  tableB_reliability.png\n")
for (study in expected_studies) {
  cat("  tableB_reliability_", study, ".png\n", sep="")
}
cat("============================================================\n")
