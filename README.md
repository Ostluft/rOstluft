
<img src="man/figures/logo.png" align="right" />

rOstluft
========

Eine Einleitung mit was und wieso.

Installation
============

Installation und Initialisierungsanweisungen

``` r
#install.packages("devtools")
devtools::install_github("Ostluft/rOstluft")
```

Einrichten Entwicklungsumgebung
===============================

Den Quellcode mit [git](https://git-scm.com/) auschecken:

``` bash
git clone https://github.com/Ostluft/rOstluft.git
```

Alternativ kann das Projekt über den New Project Wizard im RStudio initialisiert werden:

File &gt; New Project... &gt; Version Control &gt; Git &gt; Repository URL = <https://github.com/Ostluft/rOstluft.git>

Als nächstes installiert man die R Abhängigkeiten mit Hilfe der Konsole im Rstudio (wenn das Projekt geöffnet ist):

``` r
#install.packages("devtools")
devtools::install_dev_deps()
```

Demo
====

Ein paar kurze Code Bespiele

Aktueller Stand
===============

-   Grundgerüst des Packages ist definiert
-   Tools zur automatischen generierung der Dokumentation sind eingerichtet
-   Endgültige Ablage des Datensatz in Amazon S3
-   github Organziation Ostluft erstellt und die beiden Reprositories [Ostluft/rOstluft](https://github.com/Ostluft/rOstluft) und [Ostluft/rOstluft.data](https://github.com/Ostluft/rOstluft.data)
-   Dokumentation wird ebenfalls auf github via gh-pages gehostet
-   Keine Argumente in Prozente als 0-100 sondern immer 0-1 verwenden. In Ausgaben ok

TODO
====

-   Reihenfolge der üblichen Argumente (station, intervall, jahr, parameter, etc) in Funktionen bestimmen
-   Umbennung der Datenfelder
-   Schreiben eines kleines Scriptes zur Unterstützung von neuen Package Releases
-   Funktionen um mit AWS S3 datastore zu kommunizieren/interagieren
-   Parameter-Umbenennung lookup tabel etc
-   Metadaten-Tabellen aus OL MetaDB und von weiteren Datenquellen
-   Mitteln aller Datensätze auf diverse Mittelungsintervalle
-   Tests ob Struktur der Daten stimmt & sicherstellen, dass keine Duplikate in datensatz sind (mittels distinct() ?)
-   Zusammenführen des kompletten Datastore auf AWS S3
-   Dokumentation: Anwendungsbeispiele
-   ausführliche Vignette inkl. Auswertungs-Beispielen
