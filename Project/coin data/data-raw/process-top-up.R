df <- lapply(list.files("data-raw/top-up/", full.names = TRUE), read.csv)
names(df) <- list.files("data-raw/top-up/")

### consistent names for first Marathon participants
df$AlexandraS.csv$person <- "alexandraS"
df$FrantisekB.csv$person <- "frantisekB"


# add sequence id
for(i in seq_along(df)){
  df[[i]]$sequence_id <- 1:nrow(df[[i]])
}

df <- do.call(rbind, df)
rownames(df) <- NULL

# remove unrecognized input
df$sequence <- gsub("u", "", df$sequence)

### merge data files together ----
sel_columns <- c("person", "sequence_id", "coin", "start", "sequence")
df          <- df[,sel_columns]

### align heads/tails based on coin-sides file (number sides correspond to tail) ----
source("functions/processing.R")
# all OK

### save file ----
df$dataset <- "top-up"
write.csv(df, file = "data-raw/merged-top-up.csv", row.names = FALSE)
