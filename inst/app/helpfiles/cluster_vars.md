### Optimierungsvariablen

Hier definieren Sie den zentralen Kern des Anticlustering-Algorithmus. Welche Merkmale sollen über alle Klassen hinweg absolut identisch verteilt sein?

Die Schüler werden iterativ in eine Matrix geworfen, aus der extrem homogene Cliquen gebildet werden. Typische Metriken:
- **DG / DS:** Noten der Kernfächer und Noten der sprachlichen Fächer.
- **Geschlecht:** Zwingt die Balance von exakt gleichen Jungen/Mädchen Verhältnissen je Klasse.
- **Migration & Übergangsempfehlung:** Können dynamisch dazugeschaltet werden.
