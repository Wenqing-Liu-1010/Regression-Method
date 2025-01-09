### this script performs main analyses with trimming the data set ----
source(file = "functions/binomial-test.R")


out <- list()

# remove 1-5 least and most biased people
for(i in 0:5){
  temp_out <- list()
  df       <- read.csv(file = "analyses/data_long.csv")

  person_flips <- do.call(rbind, lapply(unique(df$person), function(person){
    data.frame(
      person = person,
      same   = sum(df$toss_start[df$person == person] == df$toss_end[df$person == person]),
      N      = sum(df$person == person)
    )
  }))
  person_flips <- person_flips[order(person_flips$same/person_flips$N, decreasing = FALSE),]

  # remove i- least and most biased people
  if(i > 0){
    to_remove <- c(
      person_flips$person[1:i],
      rev(person_flips$person)[1:i]
    )
    df  <- df[!df$person %in% to_remove,]
  }

  temp_out[["N_same_side"]] <- sum(df$toss_end == df$toss_start)
  temp_out[["N_heads"]]     <- sum(df$toss_end == "h")
  temp_out[["N"]]           <- nrow(df)

  temp_out[["BF_same_side"]] <- binomial_test(x = sum(df$toss_end == df$toss_start), n = nrow(df), theta = 0.5, alpha = 5100, beta = 4900, positive = TRUE)
  temp_out[["Pr_same_side"]] <- binomial_est(x = sum(df$toss_end == df$toss_start), n = nrow(df))

  temp_out[["BF_heads"]] <- binomial_test(x = sum(df$toss_end == "h"), n = nrow(df), theta = 0.5, alpha = 5000, beta = 5000, positive = FALSE)
  temp_out[["Pr_heads"]] <- binomial_est(x = sum(df$toss_end == "h"), n = nrow(df))

  out[[i+1]] <- temp_out
}

# create a table
out_table <- do.call(rbind.data.frame, out)
out_table <- cbind(
  "people removed" = 0:5,
  out_table
)

rownames(out_table) <- NULL
coins_summary <- coins_summary[order(coins_summary$est),]
xtable::xtable(out_table, digits = 3, rownames = F)


