###############################################################################
## DISS_2026 — 01_prepare_cses5.R
## MASTER IMPORT & PREP PIPELINE (CSES5_v3_input_20262301)
## - lädt Input-RDS (data/processed/cses5/CSES5_v3_input_20262301.rds)
## - erzeugt _rev Variablen (ohne löschen)
## - Party-Likes: long + best-like + AP (party/family)
## - Soziodemographie recodes
## - Party-Mapping via Lexikon (tribble)
## - schreibt Outputs in neue Projektstruktur
###############################################################################

suppressPackageStartupMessages({
  library(tidyverse)
  library(here)
})

# ============================================================
# 0) Pfade (Projektstruktur DISS_2026)
# ============================================================

PATH_CSES5_INPUT <- here("data", "processed", "cses5", "CSES5_v3_input_20262301.rds")

# abgeleitete Daten (RDS)
DIR_CSES5_DERIVED <- here("data", "processed", "cses5", "derived")

# Outputs
DIR_TAB <- here("outputs", "tables", "cses5")
DIR_LOG <- here("outputs", "logs", "cses5")

dir.create(DIR_CSES5_DERIVED, recursive = TRUE, showWarnings = FALSE)
dir.create(DIR_TAB, recursive = TRUE, showWarnings = FALSE)
dir.create(DIR_LOG, recursive = TRUE, showWarnings = FALSE)

# Output-Dateien (versioniert, aber in neuer Struktur)
PATH_CSES5_PREPARED      <- file.path(DIR_CSES5_DERIVED, "CSES5_20262301_prepared.rds")
PATH_LIKES_LONG          <- file.path(DIR_CSES5_DERIVED, "likes_long_20262301.rds")
PATH_LIKES_PERSON_PARTY  <- file.path(DIR_CSES5_DERIVED, "likes_person_party_20262301.rds")
PATH_MEAN_LIKE_BY_PARTY  <- file.path(DIR_CSES5_DERIVED, "mean_like_by_party_20262301.rds")

PATH_DESC_FAMILY_COUNTRY <- file.path(DIR_TAB, "desc_family_country_20262301.csv")
PATH_UNMAPPED_LIKES      <- file.path(DIR_LOG, "unmapped_party_likes_20262301.csv")
PATH_UNMAPPED_FAV        <- file.path(DIR_LOG, "unmapped_fav_party_20262301.csv")
PATH_UNMAPPED_CLOSE      <- file.path(DIR_LOG, "unmapped_close_party_20262301.csv")

# ============================================================
# 1) Datensatz laden (Input-RDS -> CSES5)
# ============================================================

if (!file.exists(PATH_CSES5_INPUT)) {
  stop(
    "❌ Input-RDS nicht gefunden:\n", PATH_CSES5_INPUT, "\n\n",
    "Erwartet wird:\n",
    "data/processed/cses5/CSES5_v3_input_20262301.rds\n\n",
    "Du bist im Projekt? Teste:\n",
    "  here::here()\n"
  )
}

CSES5 <- readRDS(PATH_CSES5_INPUT)

stopifnot(
  "election_study_id" %in% names(CSES5),
  "respondent_id" %in% names(CSES5)
)

cat("\n✅ Datensatz geladen: CSES5\n")
cat("Pfad:", PATH_CSES5_INPUT, "\n")
cat("Beobachtungen:", nrow(CSES5), "\nVariablen:", ncol(CSES5), "\n")
print(sort(unique(CSES5$election_study_id)))

# ============================================================
# 2) _rev Variablen erzeugen (ohne löschen)
# ============================================================

likert_1_4 <- c(
  "pol_interest","pol_media_follow","born_country","speak_lang","follow_customs",
  "corruption_widespread","satisfaction_democracy","gov_performance_overall"
)

likert_1_5 <- c(
  "internal_efficacy","comp_sellout","comp_important", "respect_institutions", "pol_no_care","pol_for_rich","pol_trustworthy",
  "pol_main_problem","strong_leader_rules","people_make_policy","minorities_adapt",
  "majority_over_minority","immig_good_econ","immig_harm_culture","immig_increase_crime",
  "lived_most_life","dominant_religion","feel_nationality","economy_state",
  "power_makes_difference","voting_makes_difference","gov_decrease_income_diff"
)

invert_1to4 <- function(x) ifelse(is.na(x), NA, 5 - x)
invert_1to5 <- function(x) ifelse(is.na(x), NA, 6 - x)

present_1_4 <- intersect(likert_1_4, names(CSES5))
present_1_5 <- intersect(likert_1_5, names(CSES5))

before_names <- names(CSES5)

if (length(present_1_4)) {
  CSES5 <- CSES5 %>%
    mutate(across(all_of(present_1_4), ~ invert_1to4(.), .names = "{.col}_rev"))
}
if (length(present_1_5)) {
  CSES5 <- CSES5 %>%
    mutate(across(all_of(present_1_5), ~ invert_1to5(.), .names = "{.col}_rev"))
}

added_rev <- setdiff(names(CSES5), before_names)
added_rev <- added_rev[grepl("_rev$", added_rev)]

cat("\n✅ Neu erzeugte invertierte Variablen (_rev):\n")
print(added_rev)

# ============================================================
# 3) Party name coding (Lexikon + Mapping)
# ============================================================

normalize_party_name <- function(x) {
  x %>%
    as.character() %>%
    str_replace_all("[–—−]", "-") %>%
    str_replace_all("\\s+", " ") %>%
    str_trim()
}

is_nonparty_response <- function(x) {
  x %in% c("Missing","Refused","Don't know","Other candidate/party (nfs)")
}

# ------------------------------------------------------------------
# PARTY-LEXIKON: HIER EINFÜGEN / PFLEGEN (genau hier ist die Stelle)
# ------------------------------------------------------------------
party_lexicon <- tribble(
  ~election_study_id, ~party_name_key, ~party_short, ~party_code,

  # ---------------- AUT_2017 ----------------
  "AUT_2017", "Die Grünen - Die Grüne Alternative", "GRUENE", "AUT_GRUENE",
  "AUT_2017", "Freie Liste Österreich / FPS Liste Dr. Karl Schnell (FLOE)", "FLOE", "AUT_FLOE",
  "AUT_2017", "Freiheitliche Partei Österreichs (FPÖ)", "FPOE", "AUT_FPOE",
  "AUT_2017", "Kommunistische Partei Österreichs und Plattform PLUS (KPÖ)", "KPOE", "AUT_KPOE",
  "AUT_2017", "Liste Peter Pilz (PILZ)", "PILZ", "AUT_PILZ",
  "AUT_2017", "Liste Sebastian Kurz - Die neue Volkspartei (ÖVP)", "OEVP", "AUT_OEVP",
  "AUT_2017", "NEOS - Das Neue Österreich", "NEOS", "AUT_NEOS",
  "AUT_2017", "Sozialdemokratische Partei Österreichs (SPÖ)", "SPOE", "AUT_SPOE",

  # ---------------- CHE_2019 ----------------
  "CHE_2019", "Christlich-soziale Partei (CsP/PCS) / Christian Social Party", "CSP", "CHE_CSP",
  "CHE_2019", "Christlichdemokratische Volkspartei (CVP/PDC) / Christian Democratic People's Party", "CVP", "CHE_CVP",
  "CHE_2019", "Die Liberalen (FDP/PLR) / The Liberals", "FDP", "CHE_FDP",
  "CHE_2019", "Eidgenössisch-Demokratische Union (EDU/UDF) / Federal Democratic Union", "EDU", "CHE_EDU",
  "CHE_2019", "Evangelische Volkspartei der Schweiz (EVP/PEP) / Evangelical People's Party", "EVP", "CHE_EVP",
  "CHE_2019", "Grüne Partei (GPS/PES) / Green Party", "GPS", "CHE_GPS",
  "CHE_2019", "Grünliberale Partei (GLP/PVL) / Green Liberal Party", "GLP", "CHE_GLP",
  "CHE_2019", "Lega dei Ticinesi (Lega) / Ticino League", "LEGA", "CHE_LEGA",
  "CHE_2019", "Mouvement Citoyens Genevois (MCG) / Geneva Citizens' Movement", "MCG", "CHE_MCG",
  "CHE_2019", "Partei der Arbeit der Schweiz (PdA/PST) / Swiss Party of Labor", "PDA", "CHE_PDA",
  "CHE_2019", "Schweizerische Volkspartei (SVP/UDC) / Swiss People's Party", "SVP", "CHE_SVP",
  "CHE_2019", "Sozialdemokratische Partei der Schweiz (SP/PS) / Social Democratic Party", "SP", "CHE_SP",

  # ---------------- CZE_2017 ----------------
  "CZE_2017", "Action of Dissatisfied Citizens (ANO 2011)", "ANO", "CZE17_ANO",
  "CZE_2017", "Christian & Democratic Union – Czechoslovak People's Party (KDU-ČSL)", "KDU-CSL", "CZE17_KDUCSL",
  "CZE_2017", "Civic Democratic Party (ODS)", "ODS", "CZE17_ODS",
  "CZE_2017", "Communist Party of Bohemia and Moravia (KSCM)", "KSCM", "CZE17_KSCM",
  "CZE_2017", "Czech Pirate Party (Pi)", "PIRATI", "CZE17_PIRATI",
  "CZE_2017", "Czech Social Democratic Party (CSSD)", "CSSD", "CZE17_CSSD",
  "CZE_2017", "Freedom and Direct Democracy (SPD)", "SPD", "CZE17_SPD",
  "CZE_2017", "Green Party (Strana zelených, SZ)", "SZ", "CZE17_SZ",
  "CZE_2017", "Mayors and Independents (STAN)", "STAN", "CZE17_STAN",
  "CZE_2017", "Party of Civic Rights (Strana práv občanů, SPO)", "SPO", "CZE17_SPO",
  "CZE_2017", "Party of Free Citizens (Svobodni)", "SVOBODNI", "CZE17_SVOBODNI",
  "CZE_2017", "Realists (Realisté)", "REALISTE", "CZE17_REALISTE",
  "CZE_2017", "Republican Party of Czechoslovakia (SPR-RSČ)", "SPR-RSC", "CZE17_SPRRSC",
  "CZE_2017", "TOP 09", "TOP09", "CZE17_TOP09",
  "CZE_2017", "Workers' Party of Social Justice (DSSS)", "DSSS", "CZE17_DSSS",

  # ---------------- CZE_2021 ----------------
  "CZE_2021", "Action of Dissatisfied Citizens (ANO 2011)", "ANO", "CZE21_ANO",
  "CZE_2021", "Christian and Democratic Union - Czechoslovak People's Party (KDU-CSL)", "KDU-CSL", "CZE21_KDUCSL",
  "CZE_2021", "Communist Party of Bohemia and Moravia (KSCM)", "KSCM", "CZE21_KSCM",
  "CZE_2021", "Czech Social Democratic Party (CSSD)", "CSSD", "CZE21_CSSD",
  "CZE_2021", "Free Block (Volny blok)", "VOLNY", "CZE21_VOLNY",
  "CZE_2021", "Freedom and Direct Democracy (SPD)", "SPD", "CZE21_SPD",
  "CZE_2021", "Green Party (Strana zelenych, SZ)", "SZ", "CZE21_SZ",
  "CZE_2021", "Mayors and Independents (STAN)", "STAN", "CZE21_STAN",
  "CZE_2021", "Oath (Prisaha, PSH)", "PRISAHA", "CZE21_PRISAHA",
  "CZE_2021", "Party of Free Citizens (Svobodni)", "SVOBODNI", "CZE21_SVOBODNI",
  "CZE_2021", "Pirates and Mayors (PirStan)", "PIRSTAN", "CZE21_PIRSTAN",
  "CZE_2021", "Together (SPOLU)", "SPOLU", "CZE21_SPOLU",
  "CZE_2021", "Tradition, Responsibility, Prosperity (TOP 09)", "TOP09", "CZE21_TOP09",
  "CZE_2021", "Tricolour Citizen's Movement (Trikolora-Svobodni-Soukromnici)", "TRIKOLORA", "CZE21_TRIKOLORA",

  # ---------------- DEU_2017 ----------------
  "DEU_2017", "Alliance 90 / The Greens (Bündnis 90 / Die Grünen)", "GRUENE", "DEU17_GRUENE",
  "DEU_2017", "Alternative for Germany (AfD)", "AFD", "DEU17_AFD",
  "DEU_2017", "Animal Protection Party (Tierschutzpartei)", "TIERSCHUTZ", "DEU17_TIERSCHUTZ",
  "DEU_2017", "Basic Income Alliance (Bündnis Grundeinkommen, BGE)", "BGE", "DEU17_BGE",
  "DEU_2017", "Christian Democratic Union / Christian Social Union (CDU/CSU)", "CDU", "DEU17_CDUCSU",
  "DEU_2017", "Christian Social Union in Bavaria (CSU)", "CSU", "DEU17_CSU",
  "DEU_2017", "Civil Movement for Solidarity (Bürgerrechtsbewegung Solidarität, BüSo)", "BUSO", "DEU17_BUSO",
  "DEU_2017", "Ecological Democratic Party (ÖDP)", "OEDP", "DEU17_OEDP",
  "DEU_2017", "Free Democratic Party (FDP)", "FDP", "DEU17_FDP",
  "DEU_2017", "Free Voters (Freie Wähler, FW)", "FW", "DEU17_FW",
  "DEU_2017", "National Democratic Party of Germany (NPD)", "NPD", "DEU17_NPD",
  "DEU_2017", "Party of Literal Exegesis (Partei Bibeltreuer Christen, PBC)", "PBC", "DEU17_PBC",
  "DEU_2017", "Pirate Party of Germany (Piratenpartei Deutschland)", "PIRATEN", "DEU17_PIRATEN",
  "DEU_2017", "Social Democratic Party of Germany (SPD)", "SPD", "DEU17_SPD",
  "DEU_2017", "The Left (Die Linke)", "LINKE", "DEU17_LINKE",
  "DEU_2017", "The Party (Die PARTEI)", "PARTEI", "DEU17_PARTEI",

  # ---------------- DEU_2021 ----------------
  "DEU_2021", "Alliance 90 / The Greens (Bündnis 90 / Die Grünen)", "GRUENE", "DEU21_GRUENE",
  "DEU_2021", "Alternative for Germany (AfD)", "AFD", "DEU21_AFD",
  "DEU_2021", "Animal Protection Party (Tierschutzpartei)", "TIERSCHUTZ", "DEU21_TIERSCHUTZ",
  "DEU_2021", "Basic Income Alliance (Bündnis Grundeinkommen, BGE)", "BGE", "DEU21_BGE",
  "DEU_2021", "Bavarian Party (Bayernpartei, BP)", "BP", "DEU21_BP",
  "DEU_2021", "Christian Democratic Union / Christian Social Union (CDU/CSU)", "CDU", "DEU21_CDUCSU",
  "DEU_2021", "Christian Social Union in Bavaria (CSU)", "CSU", "DEU21_CSU",
  "DEU_2021", "Democracy in Motion (DiB)", "DIB", "DEU21_DIB",
  "DEU_2021", "Ecological Democratic Party (ÖDP)", "OEDP", "DEU21_OEDP",
  "DEU_2021", "Free Democratic Party (FDP)", "FDP", "DEU21_FDP",
  "DEU_2021", "Free Voters (Freie Wähler, FW)", "FW", "DEU21_FW",
  "DEU_2021", "Grassroots Democratic Party of Germany (dieBasis)", "DIEBASIS", "DEU21_DIEBASIS",
  "DEU_2021", "Marxist-Leninist Party of Germany (MLPD)", "MLPD", "DEU21_MLPD",
  "DEU_2021", "National Democratic Party of Germany (NPD)", "NPD", "DEU21_NPD",
  "DEU_2021", "Pirate Party of Germany (Piratenpartei Deutschland)", "PIRATEN", "DEU21_PIRATEN",
  "DEU_2021", "Social Democratic Party of Germany (SPD)", "SPD", "DEU21_SPD",
  "DEU_2021", "South Schleswig Voters' Association (SSW)", "SSW", "DEU21_SSW",
  "DEU_2021", "Team Todenhöfer - The Justice Party", "TODENHOEFER", "DEU21_TODENHOEFER",
  "DEU_2021", "The Left (Die Linke)", "LINKE", "DEU21_LINKE",
  "DEU_2021", "The Party (Die PARTEI)", "PARTEI", "DEU21_PARTEI",
  "DEU_2021", "Volt Germany (Volt Deutschland)", "VOLT", "DEU21_VOLT",

  # ---------------- FRA_2017 ----------------
  "FRA_2017", "Democratic Movement (Mouvement Démocrate, MoDem)", "MODEM", "FRA_MODEM",
  "FRA_2017", "Europe Ecology - The Greens (Europe Écologie - Les Verts, EELV)", "EELV", "FRA_EELV",
  "FRA_2017", "France Arise (Debout la France, DLF)", "DLF", "FRA_DLF",
  "FRA_2017", "French Communist Party (Parti Communiste Français, PCF)", "PCF", "FRA_PCF",
  "FRA_2017", "Indomitable France (La France Insoumise, FI)", "LFI", "FRA_LFI",
  "FRA_2017", "National Front (Front National, FN)", "FN", "FRA_FN",
  "FRA_2017", "New Anticapitalist Party (Nouveau Parti Anticapitaliste, NPA)", "NPA", "FRA_NPA",
  "FRA_2017", "Socialist Party (Parti Socialiste, PS)", "PS", "FRA_PS",
  "FRA_2017", "The Republic Onwards! (La République En Marche!, LaREM)", "LAREM", "FRA_LAREM",
  "FRA_2017", "The Republicans (Les Républicains, LR)", "LR", "FRA_LR",
  "FRA_2017", "Workers' Struggle (Lutte Ouvrière, LO)", "LO", "FRA_LO",

  # ---------------- GRC_2015 ----------------
  "GRC_2015", "Coalition of the Radical Left (Syriza)", "SYRIZA", "GRC15_SYRIZA",
  "GRC_2015", "Communist Party of Greece (KKE)", "KKE", "GRC15_KKE",
  "GRC_2015", "Democratic Coalition (PASOK - DIMAR)", "PASOK-DIMAR", "GRC15_PASOK_DIMAR",
  "GRC_2015", "Front of the Greek Anticapitalist Left (ANTARSYA) - Workers Revolutionary Party (EEK)", "ANTARSYA-EEK", "GRC15_ANTARSYA_EEK",
  "GRC_2015", "Independent Greeks - National Patriotic Alliance (ANEL)", "ANEL", "GRC15_ANEL",
  "GRC_2015", "New Democracy (Nea Dimokratia, ND)", "ND", "GRC15_ND",
  "GRC_2015", "Popular Association - Golden Dawn (Chrysi Aygi, XA)", "XA", "GRC15_XA",
  "GRC_2015", "Popular Unity (Laiki Enotita, LAE)", "LAE", "GRC15_LAE",
  "GRC_2015", "Recreate Greece (Dimiourgia Xana, DX)", "DX", "GRC15_DX",
  "GRC_2015", "Society (Koinonia)", "KOINONIA", "GRC15_KOINONIA",
  "GRC_2015", "The River (To Potami)", "POTAMI", "GRC15_POTAMI",
  "GRC_2015", "Union of Centrists (Enosi Kentroon)", "EK", "GRC15_EK",
  "GRC_2015", "United Popular Front (EPAM)", "EPAM", "GRC15_EPAM",

  # ---------------- GRC_2019 ----------------
  "GRC_2019", "Coalition of the Radical Left (Syriza)", "SYRIZA", "GRC_SYRIZA",
  "GRC_2019", "Communist Party of Greece (KKE)", "KKE", "GRC_KKE",
  "GRC_2019", "Course of Freedom (Plefsi Eleftherias)", "PLEFSI", "GRC_PLEFSI",
  "GRC_2019", "European Realistic Disobedience Front (MeRA25)", "MERA25", "GRC_MERA25",
  "GRC_2019", "Free Homeland (Eleftheri Patrida)", "ELEFTHERI", "GRC_ELEFTHERI",
  "GRC_2019", "Front of the Greek Anticapitalist Left (ANTARSYA)", "ANTARSYA", "GRC_ANTARSYA",
  "GRC_2019", "Greek Solution (Elliniki Lisi)", "ELLINIKI", "GRC_ELLINIKI",
  "GRC_2019", "Movement for Change (Kinima Allagis, KINAL)", "KINAL", "GRC_KINAL",
  "GRC_2019", "New Democracy (Nea Dimokratia, ND)", "ND", "GRC_ND",
  "GRC_2019", "New Right (Nea Dexia)", "NEADEXIA", "GRC_NEADEXIA",
  "GRC_2019", "Popular Association - Golden Dawn (Chrysi Aygi, XA)", "XA", "GRC_XA",
  "GRC_2019", "Recreate Greece (Dimiourgia Xana!)", "DX", "GRC_DX",
  "GRC_2019", "United Popular Front (Eniaio Pallaiko Metopo, EPAM)", "EPAM", "GRC_EPAM",

  # ---------------- ITA_2018 ----------------
  "ITA_2018", "Brothers of Italy (Fratelli d'Italia, FdI)", "FDI", "ITA_FDI",
  "ITA_2018", "CasaPound Italy (CasaPound Italia, CPI)", "CPI", "ITA_CPI",
  "ITA_2018", "Communist Party (Partito Comunista, PC)", "PC", "ITA_PC",
  "ITA_2018", "Democratic Party (Partito Democratico, PD)", "PD", "ITA_PD",
  "ITA_2018", "Five Star Movement (Movimento 5 Stelle, M5S)", "M5S", "ITA_M5S",
  "ITA_2018", "For a Revolutionary Left (Per una Sinistra Rivoluzionaria, PuSR)", "PUSR", "ITA_PUSR",
  "ITA_2018", "Forza Italia (FI)", "FI", "ITA_FI",
  "ITA_2018", "Free and Equal (Liberi e Uguali, LeU)", "LEU", "ITA_LEU",
  "ITA_2018", "League (Lega, LN)", "LN", "ITA_LN",
  "ITA_2018", "More Europe (+Europa, +E)", "EUROPA", "ITA_EUROPA",
  "ITA_2018", "Popular Civic List (Civica Popolare, CP)", "CP", "ITA_CP",
  "ITA_2018", "Power to the People (Potere al Popolo, PaP)", "PAP", "ITA_PAP",
  "ITA_2018", "The People of Family (Il Popolo della Famiglia, PdF)", "PDF", "ITA_PDF",
  "ITA_2018", "Together List (Insieme)", "INSIEME", "ITA_INSIEME",
  "ITA_2018", "Us with Italy - Christian Democratic Union (Noi con l'Italia - UdC)", "UDC", "ITA_UDC",

  # ---------------- NLD_2021 ----------------
  "NLD_2021", "50Plus (50+)", "50PLUS", "NLD_50PLUS",
  "NLD_2021", "Christian Democratic Appeal (CDA)", "CDA", "NLD_CDA",
  "NLD_2021", "Christian Union (CU)", "CU", "NLD_CU",
  "NLD_2021", "Code Orange (Code Oranje, CO)", "CO", "NLD_CO",
  "NLD_2021", "Correct Answer 2021 (JA21)", "JA21", "NLD_JA21",
  "NLD_2021", "Democrats 66 (D66)", "D66", "NLD_D66",
  "NLD_2021", "Farmer-Citizen Movement (BoerBurgerBeweging, BBB)", "BBB", "NLD_BBB",
  "NLD_2021", "Forum for Democracy (FvD)", "FVD", "NLD_FVD",
  "NLD_2021", "Green Left (GL)", "GL", "NLD_GL",
  "NLD_2021", "Labour Party (PvdA)", "PVDA", "NLD_PVDA",
  "NLD_2021", "Libertarian Party (Libertaire Partij, LP)", "LP", "NLD_LP",
  "NLD_2021", "Netherlands Better (NLBeter)", "NLBETER", "NLD_NLBETER",
  "NLD_2021", "NIDA", "NIDA", "NLD_NIDA",
  "NLD_2021", "Party for Freedom (PVV)", "PVV", "NLD_PVV",
  "NLD_2021", "Party for the Animals (PvdD)", "PVDD", "NLD_PVDD",
  "NLD_2021", "People's Party for Freedom and Democracy (VVD)", "VVD", "NLD_VVD",
  "NLD_2021", "Pirate Party (PPNL)", "PPNL", "NLD_PPNL",
  "NLD_2021", "Political Movement Think (DENK)", "DENK", "NLD_DENK",
  "NLD_2021", "Reformed Political Party (SGP)", "SGP", "NLD_SGP",
  "NLD_2021", "Socialist Party (SP)", "SP", "NLD_SP",
  "NLD_2021", "Splinter", "SPLINTER", "NLD_SPLINTER",
  "NLD_2021", "Together (BIJ1)", "BIJ1", "NLD_BIJ1",
  "NLD_2021", "Volt Netherlands (Volt)", "VOLT", "NLD_VOLT",

  # ---------------- POL_2019 ----------------
  "POL_2019", "Civic Coalition (Koalicja Obywatelska, KO) - Civic Platform (Platforma Obywatelska, PO)", "KO_PO", "POL_KO_PO",
  "POL_2019", "Confederation Liberty and Independence (Konfederacja Wolność i Niepodległość, Konfederacja)", "KONFEDERACJA", "POL_KONFED",
  "POL_2019", "German Minority Electoral Committee (Komitet Wyborczy Mniejszość Niemiecka, MN)", "MN", "POL_MN",
  "POL_2019", "Kukiz'15 (K'15)", "K15", "POL_K15",
  "POL_2019", "Polish Coalition (Koalicja Polska, KP) - Polish People's Party (Polskie Stronnictwo Ludowe, PSL)", "KP_PSL", "POL_KP_PSL",
  "POL_2019", "The Left (Lewica) - Democratic Left Alliance (Sojusz Lewicy Demokratycznej, SLD)", "LEWICA_SLD", "POL_LEWICA_SLD",
  "POL_2019", "United Right (Zjednoczona Prawica, ZP) - Law and Justice (Prawo i Sprawiedliwość, PiS)", "PIS_ZP", "POL_PIS_ZP"

) %>%
  mutate(
    party_name_key = normalize_party_name(party_name_key),
    party_short    = normalize_party_name(party_short),
    party_code     = normalize_party_name(party_code)
  )

map_party <- function(study_id, party_name) {
  x <- normalize_party_name(party_name)
  if (is.na(x) || x == "" || is_nonparty_response(x)) return(c(NA_character_, NA_character_))
  hit <- party_lexicon %>%
    filter(election_study_id == study_id, party_name_key == x) %>%
    slice(1)
  if (nrow(hit) == 0) return(c(NA_character_, NA_character_))
  c(hit$party_short, hit$party_code)
}

party_short_code_vec <- function(study, party) {
  out <- map2(study, party, ~ map_party(.x, .y))
  tibble(
    party_short = map_chr(out, 1),
    party_code  = map_chr(out, 2)
  )
}

# ============================================================
# 4) PARTY_LIKES + PARTY_PREFERENCES (long + best-like)
# ============================================================

like_vars   <- intersect(paste0("party_like_", letters[1:9]), names(CSES5))
name_vars   <- intersect(paste0("party_like_", letters[1:9], "_name"), names(CSES5))
family_vars <- intersect(paste0("party_like_", letters[1:9], "_family"), names(CSES5))

stopifnot(length(like_vars) > 0, length(name_vars) > 0)

likes_rating_long <- CSES5 %>%
  select(respondent_id, election_study_id, all_of(like_vars)) %>%
  mutate(across(all_of(like_vars), as.character)) %>%
  pivot_longer(
    cols = all_of(like_vars),
    names_to = "slot",
    names_pattern = "^party_like_([a-i])$",
    values_to = "party_like"
  )

likes_name_long <- CSES5 %>%
  select(respondent_id, election_study_id, all_of(name_vars)) %>%
  mutate(across(all_of(name_vars), as.character)) %>%
  pivot_longer(
    cols = all_of(name_vars),
    names_to = "slot",
    names_pattern = "^party_like_([a-i])_name$",
    values_to = "party_like_name"
  )

likes_family_long <- if (length(family_vars) > 0) {
  CSES5 %>%
    select(respondent_id, election_study_id, all_of(family_vars)) %>%
    mutate(across(all_of(family_vars), as.character)) %>%
    pivot_longer(
      cols = all_of(family_vars),
      names_to = "slot",
      names_pattern = "^party_like_([a-i])_family$",
      values_to = "party_like_family"
    )
} else {
  tibble(respondent_id = character(0), election_study_id = character(0),
         slot = character(0), party_like_family = character(0))
}

likes_long <- likes_rating_long %>%
  left_join(likes_name_long, by = c("respondent_id","election_study_id","slot")) %>%
  left_join(likes_family_long, by = c("respondent_id","election_study_id","slot")) %>%
  mutate(
    party_like = suppressWarnings(as.numeric(party_like)),
    party_like_name = na_if(party_like_name, ""),
    party_like_family = na_if(party_like_family, "")
  ) %>%
  filter(!is.na(party_like), !is.na(party_like_name))

# Kontrolle: slots je Study
likes_long %>%
  count(election_study_id, slot) %>%
  arrange(election_study_id, slot) %>%
  print(n = 200)

likes_person_party <- likes_long %>%
  group_by(respondent_id, election_study_id, party_like_name, party_like_family) %>%
  summarise(party_like = max(party_like, na.rm = TRUE), .groups = "drop") %>%
  mutate(party_like_name_key = normalize_party_name(party_like_name))

mean_like_by_party <- likes_person_party %>%
  group_by(election_study_id, party_like_name) %>%
  summarise(mean_like = mean(party_like, na.rm = TRUE), .groups = "drop")

likes_person_party <- likes_person_party %>%
  left_join(mean_like_by_party, by = c("election_study_id","party_like_name")) %>%
  mutate(relative_like = party_like - mean_like,
         above_avg = relative_like > 0)

# Join close_party_party_id & best_party_id (falls vorhanden)
id_vars_possible <- intersect(c("close_party_party_id","best_party_id"), names(CSES5))
if (length(id_vars_possible) > 0) {
  likes_person_party <- likes_person_party %>%
    left_join(
      CSES5 %>% select(respondent_id, election_study_id, any_of(id_vars_possible)),
      by = c("respondent_id","election_study_id")
    )
}

best_like_per_person <- likes_person_party %>%
  group_by(election_study_id, respondent_id) %>%
  summarise(
    best_like_value = { v <- sort(party_like, decreasing = TRUE); if (length(v) >= 1) v[1] else NA_real_ },
    second_like_value = { v <- sort(party_like, decreasing = TRUE); if (length(v) >= 2) v[2] else NA_real_ },
    distance_best_second = ifelse(is.na(best_like_value) | is.na(second_like_value), NA_real_, best_like_value - second_like_value),
    n_above_avg_parties = sum(above_avg, na.rm = TRUE),

    best_liked_party = {
      max_parties <- party_like_name[party_like == best_like_value]
      cp <- if ("close_party_party_id" %in% names(cur_data())) close_party_party_id[1] else NA_character_
      bp <- if ("best_party_id" %in% names(cur_data())) best_party_id[1] else NA_character_

      if (length(max_parties) == 0) NA_character_
      else if (length(max_parties) == 1) max_parties[1]
      else if (!is.na(cp) && cp %in% max_parties) cp
      else if (!is.na(bp) && bp %in% max_parties) bp
      else NA_character_
    },

    best_liked_party_family = {
      if (is.na(best_liked_party[1])) NA_character_
      else {
        fams <- party_like_family[party_like_name == best_liked_party[1]]
        if (length(fams) >= 1) fams[1] else NA_character_
      }
    },
    .groups = "drop"
  )

# Merge best-like outputs into CSES5 (new columns only; remove if already there)
CSES5 <- CSES5 %>%
  select(-any_of(names(best_like_per_person)[!(names(best_like_per_person) %in% c("respondent_id","election_study_id"))])) %>%
  left_join(best_like_per_person, by = c("respondent_id","election_study_id"))

# Deskriptive Tabelle
desc_family_country <- likes_person_party %>%
  filter(!is.na(party_like_family), !is.na(party_like_name)) %>%
  group_by(election_study_id, party_like_family, party_like_name) %>%
  summarise(
    n_persons = n_distinct(respondent_id),
    n_above = sum(above_avg, na.rm = TRUE),
    n_not_above = sum(!above_avg & !is.na(above_avg)),
    share_above = n_above / n_persons,
    mean_like = mean(party_like, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(election_study_id, party_like_family, party_like_name)

write_csv(desc_family_country, PATH_DESC_FAMILY_COUNTRY)



# ============================================================
# 5) Sozio-Demographie (wie gehabt)
# ============================================================

to_chr <- function(x) as.character(x)

CSES5 <- CSES5 %>%
  mutate(
    gender = to_chr(gender),
    gender_female = case_when(gender == "Female" ~ 1L, gender == "Male" ~ 0L, TRUE ~ NA_integer_),
    gender_binary = factor(case_when(gender == "Female" ~ "Female", gender == "Male" ~ "Male", TRUE ~ NA_character_),
                           levels = c("Male","Female")),

    rural_urban = to_chr(rural_urban),
    rural_urban_recoded = factor(
      case_when(
        rural_urban == "Rural Area / Village" ~ "Rural",
        rural_urban == "Small or Middle-Sized Town" ~ "Town",
        rural_urban == "Suburbs of Large Town or City" ~ "Suburban",
        rural_urban == "Large Town or City" ~ "Large city",
        TRUE ~ NA_character_
      ),
      levels = c("Town","Rural","Suburban","Large city")
    ),

    employment_status = to_chr(employment_status),
    employment_status_recoded = factor(
      case_when(
        employment_status %in% c("Full-Time Employed (32+ hours)",
                                 "Part-Time Employed (15-32 hours)",
                                 "Employed - Less than 15 hours") ~ "Employed",
        employment_status == "Unemployed" ~ "Unemployed",
        employment_status == "Retired" ~ "Retired",
        employment_status == "Student / Vocational Training" ~ "Student",
        employment_status == "Housewife / Home Duties" ~ "Homemaker",
        employment_status %in% c("Civil / Military Service","Helping Family Member",
                                 "Other (Not in Labor Force)",
                                 "Temporary Job Leave (Maternity, Sick Leave, etc.)") ~ "Other",
        TRUE ~ NA_character_
      ),
      levels = c("Employed","Unemployed","Retired","Student","Homemaker","Other")
    ),

    education = to_chr(education),
    education_recoded = factor(
      case_when(
        education %in% c("Early Childhood Education (ISCED 0)","Primary Education (ISCED 1)","Lower Secondary (ISCED 2)") ~ "Low",
        education %in% c("Upper Secondary (ISCED 3)","Post-Secondary Non-Tertiary (ISCED 4)") ~ "Medium",
        education %in% c("Short-Cycle Tertiary (ISCED 5)","Bachelor or Equivalent (ISCED 6)",
                         "Master or Equivalent (ISCED 7)","Doctoral or Equivalent (ISCED 8)") ~ "High",
        TRUE ~ NA_character_
      ),
      levels = c("Medium","Low","High")
    ),

    age = suppressWarnings(as.numeric(age)),
    age_clean = case_when(
      is.na(age) ~ NA_real_,
      age >= 9990 ~ NA_real_,
      age > 100 ~ NA_real_,
      age < 16 ~ NA_real_,
      TRUE ~ age
    ),

    employment_sector = to_chr(employment_sector),
    employment_sector_recoded = factor(
      case_when(
        employment_sector == "Private Sector" ~ "Private",
        employment_sector == "Public Sector" ~ "Public",
        employment_sector == "Third Sector / Non-Profit" ~ "Third Sector",
        employment_sector == "Mixed" ~ "Mixed",
        TRUE ~ NA_character_
      ),
      levels = c("Private","Public","Third Sector","Mixed")
    ),

    urban_town = case_when(
      rural_urban_recoded == "Town" ~ 1L,
      rural_urban_recoded == "Rural" ~ 0L,
      rural_urban_recoded %in% c("Suburban","Large city") ~ 0L,
      TRUE ~ NA_integer_
    ),
    urban_city = case_when(
      rural_urban_recoded == "Large city" ~ 1L,
      rural_urban_recoded == "Rural" ~ 0L,
      rural_urban_recoded %in% c("Town","Suburban") ~ 0L,
      TRUE ~ NA_integer_
    ),

    unemployed = case_when(
      employment_status_recoded == "Unemployed" ~ 1L,
      employment_status_recoded %in% c("Employed","Retired","Student","Homemaker","Other") ~ 0L,
      TRUE ~ NA_integer_
    ),

    education_recodedLow = case_when(
      education_recoded == "Low" ~ 1L,
      education_recoded %in% c("Medium","High") ~ 0L,
      TRUE ~ NA_integer_
    ),
    education_recodedHigh = case_when(
      education_recoded == "High" ~ 1L,
      education_recoded %in% c("Medium","Low") ~ 0L,
      TRUE ~ NA_integer_
    ),

    left_right_clean = case_when(
      left_right_self >= 0 & left_right_self <= 10 ~ as.numeric(left_right_self),
      TRUE ~ NA_real_
    )
  )

# ============================================================
# 6) Party coding auf best_liked_party & close_party_party_id & likes
# ============================================================

tmp_fav   <- party_short_code_vec(CSES5$election_study_id, CSES5$best_liked_party)
tmp_close <- party_short_code_vec(CSES5$election_study_id, CSES5$close_party_party_id)

CSES5 <- CSES5 %>%
  mutate(
    fav_party_short_20262301   = tmp_fav$party_short,
    fav_party_code_20262301    = tmp_fav$party_code,
    close_party_short_20262301 = tmp_close$party_short,
    close_party_code_20262301  = tmp_close$party_code
  )

tmp_like <- party_short_code_vec(likes_person_party$election_study_id,
                                 likes_person_party$party_like_name)

likes_person_party <- likes_person_party %>%
  mutate(
    party_short_20262301 = tmp_like$party_short,
    party_code_20262301  = tmp_like$party_code
  )

# ============================================================
# 7) Affektive Polarisierung (neue Operationalisierung)
#    Partisanen-basiert: Ankerpunkt = close_party
# ============================================================

clean_party_short <- function(x) na_if(as.character(x), "")
clean_strength    <- function(x) {
  x <- suppressWarnings(as.numeric(x))
  ifelse(is.finite(x) & x >= 1 & x <= 3, x, NA_real_)
}

likes_pp <- likes_person_party |>
  transmute(
    respondent_id     = as.character(respondent_id),
    election_study_id = as.character(election_study_id),
    party_short       = clean_party_short(party_short_20262301),
    party_like        = suppressWarnings(as.numeric(party_like))
  ) |>
  filter(!is.na(respondent_id), !is.na(election_study_id),
         !is.na(party_short), is.finite(party_like))

fav_party_pp <- CSES5 |>
  transmute(
    respondent_id              = as.character(respondent_id),
    election_study_id          = as.character(election_study_id),
    close_party_short_20262301 = clean_party_short(close_party_short_20262301),
    close_party_strength       = clean_strength(close_party_strength)
  ) |>
  filter(!is.na(close_party_short_20262301)) |>
  distinct(respondent_id, election_study_id, .keep_all = TRUE)

likes_with_fav <- likes_pp |>
  inner_join(fav_party_pp, by = c("respondent_id", "election_study_id")) |>
  mutate(is_fav = (party_short == close_party_short_20262301))

ap_close_df <- likes_with_fav |>
  group_by(election_study_id, respondent_id) |>
  summarise(
    close_party_strength      = first(close_party_strength),
    ap_close_n_parties_rated  = sum(!is.na(party_like)),
    ap_close_has_fav_rating   = any(is_fav, na.rm = TRUE),
    ap_close_fav_like = if (any(is_fav, na.rm = TRUE)) {
      max(party_like[is_fav], na.rm = TRUE)
    } else NA_real_,
    ap_close_mean_others_like = if (any(is_fav, na.rm = TRUE) && sum(!is_fav) >= 1) {
      mean(party_like[!is_fav], na.rm = TRUE)
    } else NA_real_,
    ap_close_udist = ifelse(
      is.na(ap_close_fav_like) | is.na(ap_close_mean_others_like),
      NA_real_,
      ap_close_fav_like - ap_close_mean_others_like
    ),
    ap_close_uvar = ifelse(
      ap_close_n_parties_rated >= 2,
      stats::sd(party_like, na.rm = TRUE),
      NA_real_
    ),
    ap_close_udist_strength = ifelse(
      is.na(ap_close_udist) | is.na(close_party_strength),
      NA_real_,
      ap_close_udist * close_party_strength
    ),
    .groups = "drop"
  )

ap_close_new_cols <- c(
  "ap_close_udist", "ap_close_uvar", "ap_close_udist_strength",
  "ap_close_fav_like", "ap_close_mean_others_like",
  "ap_close_n_parties_rated", "ap_close_has_fav_rating"
)

CSES5 <- CSES5 |>
  select(-any_of(ap_close_new_cols)) |>
  left_join(
    ap_close_df |> select(respondent_id, election_study_id, all_of(ap_close_new_cols)),
    by = c("respondent_id", "election_study_id")
  )

cat("\n✅ Neue AP-Variablen (close-party basiert):\n")
print(ap_close_new_cols)
ap_close_df |>
  summarise(
    N_partisans          = n(),
    share_has_fav_rating = mean(ap_close_has_fav_rating, na.rm = TRUE),
    mean_udist           = mean(ap_close_udist, na.rm = TRUE),
    sd_udist             = sd(ap_close_udist, na.rm = TRUE),
    mean_uvar            = mean(ap_close_uvar, na.rm = TRUE)
  ) |>
  print()

# ============================================================
# 8) QUALITY CHECKS: Unmapped parties
# ============================================================

unmapped_likes <- likes_person_party %>%
  filter(!is.na(party_like_name)) %>%
  mutate(party_like_name_key = normalize_party_name(party_like_name)) %>%
  filter(!is_nonparty_response(party_like_name_key)) %>%
  filter(is.na(party_code_20262301) | is.na(party_short_20262301)) %>%
  distinct(election_study_id, party_like_name) %>%
  arrange(election_study_id, party_like_name)

unmapped_fav <- CSES5 %>%
  filter(!is.na(best_liked_party)) %>%
  mutate(fav_party_key = normalize_party_name(best_liked_party)) %>%
  filter(!is_nonparty_response(fav_party_key)) %>%
  filter(is.na(fav_party_code_20262301) | is.na(fav_party_short_20262301)) %>%
  distinct(election_study_id, best_liked_party) %>%
  arrange(election_study_id, best_liked_party)

unmapped_close <- CSES5 %>%
  filter(!is.na(close_party_party_id)) %>%
  mutate(close_party_key = normalize_party_name(close_party_party_id)) %>%
  filter(!is_nonparty_response(close_party_key)) %>%
  filter(is.na(close_party_code_20262301) | is.na(close_party_short_20262301)) %>%
  distinct(election_study_id, close_party_party_id) %>%
  arrange(election_study_id, close_party_party_id)


# ============================================================
# 9) Save outputs
# ============================================================

saveRDS(CSES5, PATH_CSES5_PREPARED)
saveRDS(likes_person_party, PATH_LIKES_PERSON_PARTY)
saveRDS(likes_long, PATH_LIKES_LONG)
saveRDS(mean_like_by_party, PATH_MEAN_LIKE_BY_PARTY)

cat("\n Fertig. Prepared objects gespeichert:\n")
cat(" -", PATH_CSES5_PREPARED, "\n")
cat(" -", PATH_LIKES_LONG, "\n")
cat(" -", PATH_LIKES_PERSON_PARTY, "\n")
cat(" -", PATH_MEAN_LIKE_BY_PARTY, "\n\n")
cat("⚠️ Falls unmapped CSVs nicht leer sind: Party-Lexikon ergänzen.\n")
