
<img src="man/figures/logo.png" align="right" />

rOstluft
========

Eine Einleitung mit was und wieso.

Installation
============

Der Quellcode von [rOstluft](https://github.com/Ostluft/rOstluft) ist auf github gehosted. Die einfachste Variante ist die Installation mit Hilfe des Packages devtools:

``` r
#install.packages("devtools")
devtools::install_github("Ostluft/rOstluft")
```

Zusätzlich muss das Package `aws.s3` manuell aus dem cloudyr Repositorium installiert werden, weil die CRANR Version veraltet ist:

``` r
install.packages("aws.s3", repos = c("cloudyr" = "http://cloudyr.github.io/drat"))
```

Ist dies wegen Einschränkungen durch Firewalls oder Proxies nicht möglich. Muss der Quellcode manuell von github heruntergeladen werden (Clone or download &gt; Download as ZIP), entpackt und manuell installiert werden. Allerdings bestehen Abhängigkeiten zu Packages die auf CRAN bereitgestellt werden. Können auch keine CRAN Packages installiert werden, müssen zuerst alle CRAN Abhängkigkeiten und deren Abhängigkeiten installiert werden.

Zusätzlich besteht noch die Github Abhängkigkeit zu [rOstluft.data](https://github.com/Ostluft/rOstluft.data). Dieses Packages muss auf die gleiche Weise zuerst installiert werden mit folgenden Schritten:

``` r
download.file("https://github.com/Ostluft/rOstluft/archive/master.zip", "rOstluft-master.zip")
download.file("https://github.com/Ostluft/rOstluft.data/archive/master.zip", "rOstluft.data-master.zip")

install.packages("devtools")
install.packages("aws.s3", repos = c("cloudyr" = "http://cloudyr.github.io/drat"))

deps <- c('dplyr', 'tidyr', 'lubridate', 'R6', 'rappdirs', 'tibble', 'base64url', 'forcats', 
          'fs', 'purrr', 'readr', 'stringr', 'plyr', 'stringi', 'sp', 'rgdal', 'rlang', 'magrittr')

for (p in deps) {
  install.packages(p)
}

devtools::install_local("rOstluft.data-master.zip", dependencies = FALSE)
devtools::install_local("rOstluft-master.zip", dependencies = FALSE)
```

Falls das installieren von rOstluft scheitert, fehlt vermutlich eine Abhängigkeit. Welche das ist, kann der Fehlermeldung entnommen werden.

Nach der Installation kann das Packages verwendet werden:

``` r
library(rOstluft)
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
