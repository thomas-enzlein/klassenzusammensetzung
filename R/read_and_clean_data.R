#' @importFrom readxl read_excel
#' @importFrom janitor clean_names
#' @importFrom dplyr as_tibble mutate case_when filter
#' @importFrom magrittr %>%

try_rename_columns <- function(df) {
  # Synonym-Wörterbuch (Name nach clean_names -> Interner Name)
  aliases <- c(
    "nr"                    = "id", 
    "grundschule"           = "abgebende_schule",
    "herkunftsschule"       = "abgebende_schule",
    "schule"                = "abgebende_schule", 
    "geschl"                = "geschlecht",
    "m_w"                   = "geschlecht",
    "sex"                   = "geschlecht",
    "migrationshintergrund" = "mig",
    "migration"             = "mig",
    "empfehlung"            = "ue",
    "uebergang"             = "ue",
    "uebergangsempfehlung"  = "ue",
    "durchschnitt_einfach"  = "de",
    "durchschnitt_gewichtet"= "dg",
    "durchschnitt_sprache"  = "ds",
    "foerderkind"           = "forderkind",
    "nachname"              = "name",
    "familienname"          = "name",
    "rufname"               = "vorname"
  )
  
  # Spalten umbenennen, falls Synonym gefunden
  current_cols <- names(df)
  for (i in seq_along(current_cols)) {
    if (current_cols[i] %in% names(aliases)) {
      names(df)[i] <- aliases[current_cols[i]]
    }
  }
  return(df)
}


#' @export
#' @name read_and_clean_data
#' @title Liest die Klassenzusammensetzungs-Excel ein und bereitet sie vor.
#' 
#' @description 
#' Importiert die Rohdaten aus einer Excel-Datei, bereinigt die Spaltennamen 
#' und fuehrt erste Datentransformationen durch. Dies umfasst das Umbenennen,
#' Typisieren und Filtern (z.B. Entfernung von Schuelern, die an andere Schulen
#' weiterziehen).
#' 
#' @param file_path Ein String. Der absolute oder relative Pfad zur Excel-Datei, 
#' die die Schuelerdaten enthaelt.
#'
#' @return Ein Dataframe (`tibble`) mit den bereinigten und typisierten Daten.
#' Die wichtigsten Spalten:
#' \describe{
#'   \item{name, vorname}{Name und Vorname des Kindes.}
#'   \item{geschlecht}{Geschlecht ("m" oder "w").}
#'   \item{abgebende_schule}{Der Name der Grundschule.}
#'   \item{dg}{Durchschnittsnote gewichtet.}
#'   \item{ds}{Durchschnittsnote Sprache.}
#'   \item{de}{Durchschnittsnote einfach.}
#'   \item{ue}{uebergangsempfehlung (numerisch kodiert 1 bis 5).}
#'   \item{mig}{Migrationshintergrund ("ja" oder "nein").}
#' }
read_and_clean_data <- function(file_path) {
  # 1. Daten einlesen und bereinigen
  df <- suppressWarnings(read_excel(file_path, skip = 7, guess_max = 10000)) %>%
    clean_names()
  
  # 2. Toleranz: Spaltennamen sanft anpassen
  df <- try_rename_columns(df)
  
  # 3. Striktes Pruefen auf Pflichtspalten
  required_cols <- c("name", "vorname", "geschlecht", "abgebende_schule", "dg", "ds", "de", "ue", "mig")
  missing_required <- setdiff(required_cols, names(df))
  
  if (length(missing_required) > 0) {
    stop(
      "Fehlerhafte Excel-Vorlage: Es fehlen zwingend benoetigte Spalten: '", 
      paste(missing_required, collapse = "', '"), 
      "'. Bitte ueberpruefen Sie Ihre Tabelle! (Gefundene Spalten: '", 
      paste(names(df), collapse = "', '"), "')"
    )
  }
  
  # 4. Weiches Pruefen auf optionale Spalten (Forderkind)
  if (!"forderkind" %in% names(df)) {
    warning("Hinweis: Die Spalte 'forderkind' wurde nicht gefunden. Es wird angenommen, dass kein Kind spezifischen Foerderbedarf hat.")
    df$forderkind <- "nein"
  }
  
  # 5. Typisierung und Transformation
  # Die Warnung bei as.Date abfangen, damit die Console sauber bleibt
  df <- df %>%
    mutate(
      geburtsdatum = suppressWarnings(as.Date(geburtsdatum)),
      geschlecht = as.factor(geschlecht),
      abgebende_schule = as.factor(abgebende_schule),
      mig = factor(case_when(
        is.na(mig) ~ "nein",
        tolower(mig) %in% c("x", "ja", "j", "yes", "1") ~ "ja",
        .default = "nein"
      )),
      forderkind = factor(case_when(
        is.na(forderkind) ~ "nein",
        tolower(forderkind) %in% c("x", "ja", "j", "yes", "1") ~ "ja",
        .default = "nein"
      )),
      de = as.numeric(de),
      dg = as.numeric(dg),
      ds = as.numeric(ds),
      # UE kann z. B. auf "H" matchen, auch wenn "h" oder "H " geschrieben wird
      ue = case_when(
        toupper(trimws(ue)) == "H" ~ 1,
        toupper(trimws(ue)) == "H/R" ~ 2,
        toupper(trimws(ue)) == "R" ~ 3,
        toupper(trimws(ue)) == "R/GY" ~ 4,
        toupper(trimws(ue)) == "GY" ~ 5,
        .default = NA_real_
      )
    )
  
  # Entferne leere oder kaputte Zeilen
  df <- df %>% 
    filter(!is.na(abgebende_schule) & abgebende_schule != "") %>%
    filter(!is.na(dg) & !is.na(ds))
    
  return(df)
}
