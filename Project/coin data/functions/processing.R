swap_ht <- function(df){
  if(nrow(df) == 0)
    stop("improper selection")

  df$start <- gsub("h", "q", df$start, fixed = TRUE)
  df$start <- gsub("t", "w", df$start, fixed = TRUE)
  df$sequence <- gsub("h", "q", df$sequence, fixed = TRUE)
  df$sequence <- gsub("t", "w", df$sequence, fixed = TRUE)

  df$start <- gsub("q", "t", df$start, fixed = TRUE)
  df$start <- gsub("w", "h", df$start, fixed = TRUE)
  df$sequence <- gsub("q", "t", df$sequence, fixed = TRUE)
  df$sequence <- gsub("w", "h", df$sequence, fixed = TRUE)

  return(df)
}
