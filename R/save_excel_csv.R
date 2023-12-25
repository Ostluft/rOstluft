#' Speichert Datein in einer Excel kompatiblen CSV Datei
#'
#' @description
#' Für eine Region Schweiz Excel kompatible Datei zu erstellen müssen folgende Bedingungen erfüllt sein:
#' * UTF-8 mit Byte Order Mark (UTF-8 BOM)
#' * Delimiter ist ";"
#' * Dezimalpunkt ist "."
#' * NA Werte sind "#NV"
#' * Datumsformat ist "%d.%m.%Y %H:%M:%S"
#'
#' Diese Funktion kümmert sich um all diese Kleinigkeiten. Zudem konventiert sie alle Zeitangeben als [POSIXct()] oder
#' [POSIXlt()] mit Hilfe von [format.POSIXct()], bzw. [format.POSIXlt()] unter berücksichtung des Arguments `tz` in
#' Characters vor dem Aufruf von [readr::write_excel_csv()].
#'
#'
#' @seealso
#'   * [readr::write_excel_csv()]
#'
#' @param data Zu speichernde Daten
#' @param fn Pfad zur Datei
#'
#' @return invisible(NULL)
#' @export
save_excel_csv <- function(data, fn) {
  fs::dir_create(fs::path_dir(fn))
  data <- dplyr::ungroup(data)
  data <- dplyr::mutate_if(data, lubridate::is.POSIXt, ~format(., "%d.%m.%Y %H:%M:%S", tz = lubridate::tz(.)))

  # Einige Funktionen wie mean geben NaN statt NA zurück in bestimmten Fällen. Zum Beispiel
  # mean(c(NA, NA), na.rm = TRUE) = NaN und nicht = NA_real_.
  # Dies wird hier gefixt ...
  data <- dplyr::mutate_if(data, rlang::is_bare_double, ~dplyr::if_else(is.nan(.), NA_real_, .))
  data <- dplyr::mutate_if(data, rlang::is_bare_integer, ~dplyr::if_else(is.nan(.), NA_integer_, .))

  readr::write_excel_csv(data, fn, delim = ";", na = "#NV")
  invisible(NULL)
}
