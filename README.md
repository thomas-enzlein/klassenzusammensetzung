# Klassenzusammensetzung
[![R-CMD-check](https://github.com/thomas-enzlein/klassenzusammensetzung/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/thomas-enzlein/klassenzusammensetzung/actions/workflows/R-CMD-check.yaml)

[![R-CMD-check](https://github.com/USER/Klassenzusammensetzung/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/USER/Klassenzusammensetzung/actions/workflows/R-CMD-check.yaml)

R-Shiny App zur optimalen Zusammensetzung von Schulklassen. Nutzt K-Means Anticlustering und Integer Linear Programming (ILP) für perfekt ausbalancierte Klassen (Leistung, Geschlecht & Migration).

## Features

- **Gerechte Leistungsverteilung:** Alle generierten Schulklassen weisen identische Notendurchschnitte (Sprache und Kernfächer) auf.
- **Demografische Balance:** Exakte Verteilung von Geschlecht und wahlweise Migrationshintergrund und Übergangsempfehlungen (Gymnasium, Realschule, Hauptschule).
- **Must-Links für kleine Schulen:** Kinder aus sehr kleinen Grundschulen werden algorithmisch davor geschützt, voneinander getrennt und isoliert zu werden.
- **Keine isolierten Schüler:** Ein intelligenter Post-Hoc Micro-Swap-Algorithmus tauscht Schüler gleicher Leistung und Geschlecht aus, um zu garantieren, dass möglichst kein Kind ohne Kameraden aus seiner alten Schule in eine neue Klasse kommt.
- **Interaktive UI:** Komplett bedienbar über ein modernes Shiny Dashboard mit dynamischer Gewichtung und auswahl von Kriterien und interaktiven Plotly-Grafiken.

## Installation

Du kannst das R-Paket direkt über GitHub installieren:

```r
# Installiere devtools, falls noch nicht vorhanden
install.packages("devtools")

# Installiere Klassenzusammensetzung
devtools::install_github("thomas-enzlein/Klassenzusammensetzung")
```

## Benutzung

Um die Applikation zu starten, führe einfach diesen Befehl in deiner R-Konsole aus:

```r
Klassenzusammensetzung::run_app()
```

Die App öffnet sich daraufhin automatisch in deinem lokalen Webbrowser.

### Excel-Datenstruktur

Die App erwartet eine `.xls` oder `.xlsx` Datei mit mindestens folgenden Spalten:

*   **abgebende\_schule:** Name der ursprünglichen Grundschule
*   **geschlecht:** 'w' oder 'm'
*   **de, dg, ds:** Verschiedene Notendurchschnitte
*   **ue:** Übergangsempfehlung (z.B. Gym, RS, HS)
*   **mig:** Migrationshintergrund (z.B. Ja/Nein)

## Technischer Hintergrund

Der Algorithmus arbeitet in vier aufeinanderfolgenden Stufen:

1.  **Leverage & Must-Links:** Kleine Schulen, die den Schnitt verzerren würden, werden hart aufgeteilt. Kleine Schulen ohne starke Hebelwirkung werden zusammenbelassen sodass möglichst jeder Schüler einen sozialen Anker hat.
2.  **K-Means Anticlustering:** Die Schüler werden in homogene Kleingruppen (2x Anzahl Räume) geclustert, gewichtet nach den Nutzeingaben im UI. 
3.  **ILP Optimal Matching:** Die Kleingruppen werden mittels Linear Programming (`lpSolve`) zu finalen Räumen gepaart, wobei Varianz und Isolationsfälle (einzelne Schüler aus einer Schule) minimiert werden.
4.  **Micro-Swaps:** Verbliebene Isolierte Schüler werden durch das rekursive Tauschen von Statistik-Zwillingen endgültig eliminiert.
