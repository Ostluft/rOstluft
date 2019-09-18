LocalNotFound <- function(name, path) {
  structure(list(name = name,
                 path = path,
                 message = sprintf("Local store %s not found under '%s'", name, path),
                 call = NULL),
            class = c("LocalNotFound", "error", "condition"))
}


ReadOnlyStore <- function(name) {
  structure(list(name = name,
                 message = sprintf("Store %s is read only", name),
                 call = NULL),
            class = c("ReadOnlyStore", "error", "condition"))
}

FunctionNotSupported <- function(name, path) {
  structure(list(name = name,
                 path = path,
                 message = sprintf("This function is not supported"),
                 call = NULL),
            class = c("FunctionNotSupported", "error", "condition"))
}


MetaKeyNotFound <- function(name, key) {
  structure(list(name = name,
                 key = key,
                 message = sprintf("Key %s not found for store %s", key, name),
                 call = NULL),
            class = c("MetaKeyNotFound", "error", "condition"))
}

IncompatibleColumns <- function(name, columns) {
  indent <- rep("  ", length(columns))
  columns_str <- stringr::str_c(indent, columns, collapse = "\n")

  structure(list(name = name,
               columns = columns,
               message = sprintf("Incompatible columns in put for store %s:\n%s", name, columns_str),
               call = NULL),
          class = c("IncompatibleColumns", "error", "condition"))
}
