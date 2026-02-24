### Der Klassenzusammensetzungs-Algorithmus
        
Diese Applikation teilt künftige Erstklässler optimal auf Klassenzimmer auf. Das übergeordnete Ziel: Jede Klasse soll den **gleichen Notendurchschnitt** haben und das **gleiche Verhältnis von Jungen und Mädchen** aufweisen. 
Gleichzeitig soll vermieden werden, dass Kinder aus kleinen Grundschulen völlig alleine (isoliert) in einer Klasse landen.

**So funktioniert der Algorithmus in 4 Stufen:**

1. **Variable Leverage & Must-Links:** 
   Kleine Grundschulen können den Klassendurchschnitt unvorhergesehen verzerren falls sie einen großen Hebel haben. Der Algorithmus berechnet nun für jede Ihrer **gewählten Variablen** (Noten, Geschlecht etc.), wie stark eine kleine Schule vom Gesamtdurchschnitt abweicht. Dieser Hebel ("Leverage") wird gewichtet aufsummiert. Bei geringem Hebel werden die Kinder dieser kleinen Schule durch einen 'Must-Link' fest vereint. Ist der Hebel jedoch zu groß (über der gesetzten Schwelle), MÜSSEN sie getrennt werden, um die Leistungs-Fairness der anderen Klassen nicht zu gefährden.
   
2. **K-Means Anticlustering:** 
   Vor der finalen Raumzuteilung werden die Kinder anhand der gewählten Variablen (Noten, Geschlecht, Migration, Uebergangsempfehlung) in K-Means-Basierte **Kleingruppen** (genau doppelt so viele wie Ziel-Räume) geclustert. Diese Zwischengruppen sind bereits maximal heterogen durchmischt. Sie können über die Schieberegler links eigene Gewichtungen festlegen (z.B. Geschlecht doppelt so stark werten wie Noten).
   
3. **ILP Optimal Matching:** 
   Die homogenen Kleingruppen werden durch einen strikt mathematischen Ansatz (*Integer Linear Programming*, `lpSolve`) paarweise zu den finalen Räumen kombiniert. Der Algorithmus berechnet für jede erdenkliche Kombination Strafpunkte (Ungleichgewicht in Noten, Geschlecht und vor allem Strafen für Isolierung von Schülern) und findet garantiert die **optimale Paarkombination** für den gesamten Jahrgang.
   
4. **Micro-Swaps:** 
   Sollten durch ungünstige Verhältnisse dennoch einzelne Kinder nach dem ILP isoliert in einem Raum landen kommt ein Tausch-Mechanismus zum Einsatz. Er sucht in anderen Räumen nach "Statistik-Zwillingen" (gleiches Geschlecht, gleiche Schulformempfehlung, max 0.6 Punkte Notenabweichung) und **tauscht diese Kinder aus**. Dadurch eliminiert das Modell die letzten Isolierten konsequent, ohne dass die Durchschnittsnoten der Räume kippen.
   
***

### Glossar der Variablen
*   **DE:** Durchschnittsnote einfach (alle Fächer gleich gewichtet)
*   **DG:** Durchschnittsnote gewichtet (Hauptfächer doppelt gewichtet, ohne Religion)
*   **DS:** Durchschnittsnote Sprache
*   **LSF:** Letzte Schulform
*   **LKL:** Letzte Klasse
*   **ÜE:** Übergangsempfehlung
*   **GSK:** Geschwisterkind
*   **MIG:** Migrationshintergrund
*   **LG1 / LG2:** Leistungsgruppe 1 / Leistungsgruppe 2

***

### Aufbau des Hebel-Werts (Leverage) für Schulen
Die Tabelle 'Schulen mit Must-Links' zeigt an, warum bestimmte kleine Schulen geschützt werden. Der Algorithmus berechnet dafür einen 'Hebel', der aussagt, wie stark diese kleine Gruppe den Klassenschnitt verzerren würde.

*   **Hebel Leistung:** Vergleicht den Notenschnitt der Schule in DG und DS mit dem Schnitt aller angemeldeten Kinder (Globaler Jahrgangsschnitt). Die maximale standardisierte Abweichung davon wird als Leistungshebel gewertet.
*   **Hebel Geschlecht:** Berechnet, wie stark der Mädchenanteil dieser kleinen Schule vom Mädchenanteil des gesamten Jahrgangs abweicht. Da das Geschlecht eine extrem hohe Priorität hat, wird diese Abweichung mathematisch verdoppelt (x2).
*   **Gesamt-Hebel:** Die Summe aus Leistungs-Hebel und Geschlechter-Hebel. Liegt diese Summe unter dem in den Einstellungen definierten Schwellenwert, erhalten diese Kinder einen Must-Link und bleiben vereint.
