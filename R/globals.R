# Deklaration von globalen Variablen, um R CMD check Warnungen ("no visible binding") bei 
# Non-Standard Evaluation (NSE) in dplyr/ggplot zu verhindern.

utils::globalVariables(c(
  # Aus 01_data_io.R
  "geburtsdatum", "konfession", "geschlecht", "abgebende_schule", "mig", "de", "dg", "ds", "ue",
  
  # Aus 02_leverage_must_links.R
  "n", "m", "leverage", "hebel_leistung", "hebel_sozial", "p", "max_leverage",
  
  # Aus 03_anticlustering.R
  "must_link_id",
  
  # Aus 04_ilp_matching.R
  "gruppe", "is_einzelmeldung", "n_total", "n_w", "n_m", "count", "raum",
  
  # Aus 05_micro_swaps.R
  "kid_id", "diff", "n_in_raum",
  
  # Aus 06_evaluation.R
  "mig_numeric", "n_in_bars", "is_isolated", "abgebende_schule_plot", "hover_text",
  "is_komplett_vereint", "has_isolated", "has_klumpen",
  "all_vereint", "any_isolated", "any_klumpen", 
  "legend_name", "n_in_raum", "vorname", "name", "hover_details",
  
  # Neu hinzugekommen fuer erweiterte Diagnostics (calculate_room_stats)
  "Anzahl Grundschulen", "Groesste Grundschule", "Gy (n)", "Gy/RS (n)", "HS (n)",
  "Mig.-Quote", "RS (n)", "RS/HS (n)", "Raum", "Schuelerzahl", "Verhaeltnis (J:M)", "UE (Bar)",
  "dev_de", "dev_dg", "dev_ds", "dev_gender", "dev_mig", "mean_de", "mean_dg", "mean_ds", "mean_ue",
  "mig_room", "prop_m_room", "sd_de", "sd_dg", "sd_ds", "sd_ue", "MW de (+/-SD)", "MW dg (+/-SD)", "MW ds (+/-SD)"
))
