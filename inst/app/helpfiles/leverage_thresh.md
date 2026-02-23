### Leverage-Schwelle (Zusammenhalt kleiner Gruppen)

Dieser Wert entscheidet, ob eine **kleine Schule** als homogene Gruppe zusammen in eine Klasse gebucht werden soll ("Must-Link").

Der Algorithmus berechnet für jede dieser kleinen Schulen einen "Gesamt-Hebel" (Leverage-Score). Er summiert, wie stark ihre Schüler vom restlichen Jahrgangsdurchschnitt in Leistung, Geschlecht oder Migration abweichen.

- **Liegt der Score UNTER der Schwelle:** Die Gruppe ist unauffällig und darf zusammenbleiben.
- **Liegt der Score ÜBER der Schwelle:** Die kleine Schule ist ein statistischer Ausreißer. Um die Fairness zu wahren, MUSS der Algorithmus die Schüler trennen und auf mehrere Klassen aufteilen. Das Gesamtwohl überwiegt.
