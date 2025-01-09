### this script performs main analyses ----
source(file = "functions/binomial-test.R")
df <- read.csv(file = "analyses/data_long.csv")


### the pre-registered iid binomial test ----
binomial_test(x = sum(df$toss_end == df$toss_start), n = nrow(df), theta = 0.5, alpha = 5100, beta = 4900, positive = TRUE)
binomial_est(x = sum(df$toss_end == df$toss_start), n = nrow(df))

# the frequentist counterpart
binom.test(sum(df$toss_end == df$toss_start), nrow(df))


### exploratory heads-tails iid test ----
binomial_test(x = sum(df$toss_end == "h"), n = nrow(df), theta = 0.5, alpha = 5000, beta = 5000, positive = FALSE)
binomial_est(x = sum(df$toss_end == "h"), n = nrow(df))

# the frequentist counterpart
binom.test(sum(df$toss_end == "h"), nrow(df))


### sensitivity analysis to outliers ----
person_flips <- do.call(rbind, lapply(unique(df$person), function(person){
  data.frame(
    person = person,
    same   = sum(df$toss_start[df$person == person] == df$toss_end[df$person == person]),
    N      = sum(df$person == person)
  )
}))
person_flips <- person_flips[order(person_flips$N, decreasing = FALSE),]

# remove people with more than 0.53% same side bias
to_remove <- person_flips$person[person_flips$same / person_flips$N > 0.53]
s_df      <- df[!df$person %in% to_remove,]

# the iid binomial test
binomial_test(x = sum(s_df$toss_end == s_df$toss_start), n = nrow(s_df), theta = 0.5, alpha = 5100, beta = 4900, positive = TRUE)
binomial_est(x = sum(s_df$toss_end == s_df$toss_start), n = nrow(s_df))

binom.test(sum(s_df$toss_end == s_df$toss_start), nrow(s_df)) # the frequentist counterpart


# exploratory heads-tails iid test
binomial_test(x = sum(s_df$toss_end == "h"), n = nrow(s_df), theta = 0.5, alpha = 5000, beta = 5000, positive = FALSE)
binomial_est(x = sum(s_df$toss_end == "h"), n = nrow(s_df))

binom.test(sum(s_df$toss_end == "h"), nrow(s_df)) # the frequentist counterpart


### create by-person summary ----
person_ordered <- names(sort(table(df$person), decreasing = TRUE))
people_summary <- do.call(rbind.data.frame, lapply(person_ordered, function(person){
  temp_bin <- binomial_est(sum(df$toss_end[df$person == person] == df$toss_start[df$person == person]), nrow(df[df$person == person,]), print = FALSE)
  temp_sum <- data.frame(
    person = abbreviate_names(person),
    same   = sum((df$toss_end == df$toss_start)[df$person == person]),
    tosses = nrow(df[df$person == person,]),
    coins  = length(unique(df$coin[df$person == person])),
    est    = temp_bin[1],
    lCI    = temp_bin[2],
    uCI    = temp_bin[3],
    BF     = binomial_test(x = sum((df$toss_end == df$toss_start)[df$person == person]), n = nrow(df[df$person == person,]), theta = 0.5, alpha = 5100, beta = 4900, positive = TRUE),
    joined = unique(df$dataset[df$person == person])[1]
  )
  return(temp_sum)
}))
rownames(people_summary) <- NULL
people_summary <- people_summary[order(people_summary$est),]
xtable::xtable(people_summary[,-8], digits = 3, rownames = F)

boxplot(people_summary$est ~people_summary$joined)

# add combined row
temp_bin <- binomial_est(sum(df$toss_end == df$toss_start), nrow(df))
data.frame(
  person  = "Combined" ,
  same    = sum(df$toss_end == df$toss_start),
  tosses  = nrow(df),
  coins   = length(unique(df$coin)),
  est     = temp_bin[1],
  lCI     = temp_bin[2],
  uCI     = temp_bin[3],
  BF      = binomial_test(x = sum(df$toss_end == df$toss_start), n = nrow(df), theta = 0.5, alpha = 5100, beta = 4900, positive = TRUE)
)


### create by-coin summary ----
coin_ordered <- names(sort(table(df$coin), decreasing = TRUE))
coins_summary <- do.call(rbind.data.frame, lapply(coin_ordered, function(coin){
  temp_bin <- binom.test(sum(df$toss_end[df$coin == coin] == "h"), nrow(df[df$coin == coin,]))
  temp_sum <- data.frame(
    coin    = coin,
    heads   = sum(df$toss_end[df$coin == coin] == "h"),
    tosses  = nrow(df[df$coin == coin,]),
    tossers = length(unique(df$person[df$coin == coin])),
    est     = temp_bin$estimate,
    lCI     = temp_bin$conf.int[1],
    uCI     = temp_bin$conf.int[2],
    BF      = binomial_test(x = sum(df$toss_end[df$coin == coin] == "h"), n = nrow(df[df$coin == coin,]), theta = 0.5, alpha = 5000, beta = 5000, positive = FALSE)
  )
  return(temp_sum)
}))
rownames(coins_summary) <- NULL
coins_summary <- coins_summary[order(coins_summary$est),]
xtable::xtable(coins_summary[,-8], digits = 3, rownames = F)

# add combined row
temp_bin <- binomial_est(sum(df$toss_end == "h"), nrow(df), print = FALSE)
data.frame(
  coin    = "Combined",
  heads   = sum(df$toss_end == "h"),
  tosses  = nrow(df),
  tossers = length(unique(df$person)),
  est     = temp_bin[1],
  lCI     = temp_bin[2],
  uCI     = temp_bin[3],
  BF      = binomial_test(x = sum(df$toss_end == "h"), n = nrow(df), theta = 0.5, alpha = 5000, beta = 5000, positive = FALSE)
)

