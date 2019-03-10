
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

Alternativ kann das Projekt Ã¼ber den New Project Wizard im RStudio initialisiert werden:

File &gt; New Project... &gt; Version Control &gt; Git &gt; Repository URL = <https://github.com/Ostluft/rOstluft.git>

Als nÃ¤chstes installiert man die R AbhÃ¤ngigkeiten mit Hilfe der Konsole im Rstudio (wenn das Projekt geÃ¶ffnet ist):

``` r
#install.packages("devtools")
devtools::install_dev_deps()
```

Demo
====

Ein paar kurze Code Bespiele

Aktueller Stand
===============

-   GrundgerÃ¼st des Packages ist definiert
-   Tools zur automatischen generierung der Dokumentation sind eingerichtet
-   EndgÃ¼ltige Ablage des Datensatz in Amazon S3
-   github Organziation Ostluft erstellt und die beiden Reprositories [Ostluft/rOstluft](https://github.com/Ostluft/rOstluft) und [Ostluft/rOstluft.data](https://github.com/Ostluft/rOstluft.data)
-   Dokumentation wird ebenfalls auf github via gh-pages gehostet
-   Keine Argumente in Prozente als 0-100 sondern immer 0-1 verwenden. In Ausgaben ok

TODO
====

-   Reihenfolge der Ã¼blichen Argumente (station, intervall, jahr, parameter, etc) in Funktionen bestimmen
-   Umbennung der Datenfelder
-   Schreiben eines kleines Scriptes zur UnterstÃ¼tzung von neuen Package Releases
-   Funktionen um mit AWS S3 datastore zu kommunizieren/interagieren
-   Parameter-Umbenennung lookup tabel etc
-   Metadaten-Tabellen aus OL MetaDB und von weiteren Datenquellen
-   Mitteln aller DatensÃ¤tze auf diverse Mittelungsintervalle
-   Tests ob Struktur der Daten stimmt & sicherstellen, dass keine Duplikate in datensatz sind (mittels distinct() ?)
-   ZusammenfÃ¼hren des kompletten Datastore auf AWS S3
-   Dokumentation: Anwendungsbeispiele
-   ausfÃ¼hrliche Vignette inkl. Auswertungs-Beispielen
