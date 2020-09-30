interval <- function(df, na.rm = T) {
        x <- df[, purrr::map_lgl(df, is.numeric)]
        sapply(x, FUN = range, na.rm = na.rm)
}

variance <- function(df, na.rm = T) {
        x <- df[, purrr::map_lgl(df, is.numeric)]
        lapply(x, FUN = var, na.rm = na.rm)
}

sd <- function(df, na.rm = T) {
        x <- df[, purrr::map_lgl(df, is.numeric)]
        lapply(x, FUN = sd, na.rm = na.rm)
}

interquartile <- function(df, na.rm) {
        x <- df[, purrr::map_lgl(df, is.numeric)]
        lapply(x, FUN = IQR, na.rm = na.rm)
}