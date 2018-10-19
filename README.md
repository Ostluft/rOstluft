
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

Den Quellcode mit git auschecken:

``` bash
git clone https://github.com/Ostluft/rOstluft.git
```

Danach das Projekt in RStudio öffnen und folgende Befehle in der Konsole ausführen:

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
