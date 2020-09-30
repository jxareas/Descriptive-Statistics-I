mode <- function(x, na.rm = T) {
        if(na.rm){
                x = x[!is.na(x)]
        }
        
        ux <- unique(x)
        return(ux[which.max(tabulate(match(x, ux)))])
}

calc_mode <- function(df, na.rm = T) {
        sapply(df, FUN = mode, na.rm = na.rm)
}

calc_mean <- function(df, na.rm = T) {
        x <- df[ , purrr::map_lgl(df, is.numeric)]
        sapply(x, FUN = mean, na.rm  = na.rm)
}

calc_median <- function(df, na.rm = T) {
        x <- df[, purrr::map_lgl(df, is.numeric)]
        sapply(x, FUN = median, na.rm = na.rm)
}