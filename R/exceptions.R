RdsLocalNotFound <- function(name, path) {
  structure(list(name = name,
                 path = path,
                 message = sprintf("Local rds store %s not found under '%s'", name, path),
                 call = NULL),
            class = c("RdsLocalNotFound", "error", "condition"))
}


ReadOnlyStore <- function(name) {
  structure(list(name = name,
                 message = sprintf("Store %s is read only", name),
                 call = NULL),
            class = c("ReadOnlyStore", "error", "condition"))
}
