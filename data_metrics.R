col_types <- function(df) {
        apply(df, 2, FUN = class)
}

col_missing_values <- function(df) {
        apply(data,2, FUN = function(x) {
                sum(is.na(x))
        })
}