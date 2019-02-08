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
