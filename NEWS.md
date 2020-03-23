# rOstluft 1.3.0

* New `store_aqmet_public()` public access for airquality and meteo data

* New statistics 'n>10' and 'n>25' for PM2.5 limits for resampling data

* Fixed `get_seepolizei()` returning values as character


# rOstluft 1.2.0

* New `read_seepolizei()` to read historic meteorological data from 
  [Messwerte der Wetterstationen der Wasserschutzpolizei Zürich](https://data.stadt-zuerich.ch/dataset/sid_wapo_wetterstationen)

* New `get_seepolizei()` gets actual data from 
  [Messwerte der Wetterstationen der Wasserschutzpolizei Zürich](https://data.stadt-zuerich.ch/dataset/sid_wapo_wetterstationen)
  using the Tecdottir API
  
* New `read_airmo_webexport()` to read AIRMO exports in web format.


# rOstluft 1.1.0

* Added a `NEWS.md` file to track changes to the package.

* During the first put the storage saves the order and data types of all columns in the data frame. All following puts
must match the saved columns (#1).


# rOstluft 1.0.0

Initial Release
