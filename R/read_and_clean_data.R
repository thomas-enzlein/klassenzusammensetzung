#' @importFrom readxl read_excel
#' @importFrom janitor clean_names
#' @importFrom dplyr as_tibble mutate case_when filter
#' @importFrom magrittr %>%
#' @export
#' @name read_and_clean_data
#' @title Liest die Klassenzusammensetzungs-Excel ein und bereitet sie vor.
#' 
#' @description 
#' Importiert die Rohdaten aus einer Excel-Datei, bereinigt die Spaltennamen 
#' und führt erste Datentransformationen durch. Dies umfasst das Umbenennen,
#' Typisieren und Filtern (z.B. Entfernung von Schülern, die an andere Schulen
#' weiterziehen).
#' 
#' @param file_path Ein String. Der absolute oder relative Pfad zur Excel-Datei, 
#' die die Schülerdaten enthält.
#'
#' @return Ein Dataframe (`tibble`) mit den bereinigten und typisierten Daten.
#' Die wichtigsten Spalten:
#' \describe{
#'   \item{name, vorname}{Name und Vorname des Kindes.}
#'   \item{geschlecht}{Geschlecht ("m" oder "w").}
#'   \item{abgebende_schule}{Der Name der Grundschule.}
#'   \item{dg}{Deutschnote genormt.}
#'   \item{ds}{Deutschnote schulintern.}
#'   \item{ue}{Übergangsempfehlung (numerisch kodiert 1 bis 5).}
#'   \item{mig}{Migrationshintergrund ("ja" oder "nein").}
#' }
read_and_clean_data <- function(file_path) {
  # read_excel wählt automatisch xls oder xlsx.
  # suppressWarnings und guess_max verhindern die massenhaften Datumswarnungen.
  df <- suppressWarnings(read_excel(file_path, skip = 7, guess_max = 10000)) %>%
    clean_names()
  
  # Standard-Aufbereitung
  df <- df %>%
    mutate(
      geburtsdatum = as.Date(geburtsdatum),
      konfession = as.factor(konfession),
      geschlecht = as.factor(geschlecht),
      abgebende_schule = as.factor(abgebende_schule),
      mig = factor(case_when(
        is.na(mig) ~ "nein",
        mig == "x" ~ "ja",
        .default = "nein"
      )),
      de = as.numeric(de),
      dg = as.numeric(dg),
      ds = as.numeric(ds),
      # Ordinale Kodierung für Übergangsempfehlung (UE)
      # H -> 1, H/R -> 2, R -> 3, R/GY -> 4, GY -> 5
      ue = case_when(
        ue == "H" ~ 1,
        ue == "H/R" ~ 2,
        ue == "R" ~ 3,
        ue == "R/GY" ~ 4,
        ue == "GY" ~ 5,
        .default = NA_real_
      )
    )
  
  # Entferne Datensätze ohne abgebende Schule oder ohne Noten
  df <- df %>% 
    filter(!is.na(abgebende_schule) & abgebende_schule != "") %>%
    filter(!is.na(dg) & !is.na(ds))
    
  return(df)
}
