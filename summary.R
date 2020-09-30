summarize <- function(df) {
        if(is.null(df)) print("df must be a non-empty data frame")
        summary_table <- do.call(data.frame, 
                                 list(
                                         Min = sapply(df, FUN = min, na.rm = T),
                                         Max = sapply(df, FUN = max, na.rm = T),
                                         Mean = sapply(df, function(x) {mean(x, na.rm = T)}),
                                         SD = sapply(df, function(x) sd(x, na.rm = T)),
                                         NULLS = sapply(df, function(x) sum(is.na(x))),
                                         Unique = sapply(df, function(x) length(unique(x))),
                                         dataType = sapply(df, class),
                                         Length = apply(df, 2, length)
                                         
                                 ))
        nums <- vapply(summary_table, is.numeric, FUN.VALUE = logical(1))
        summary_table[, nums] <- round(summary_table[,nums], digits = 3)
        return(summary_table)
        
}

generate_report <- function(df) {
        report_df <- summarize(df)
        report_df <- cbind(Columns = rownames(report_df),
                           data.frame(report_df, row.names = NULL))
        
        write.csv(report_df, paste0("Report", 
                                    format(Sys.time(), "%d-%m-%Y-%H%M%S" ),
                                    ".csv"), 
                  row.names = FALSE)
}