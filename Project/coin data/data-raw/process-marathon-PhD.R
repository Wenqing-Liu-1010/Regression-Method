df <- lapply(list.files("data-raw/marathon-PhD/", full.names = TRUE), read.csv)
names(df) <- list.files("data-raw/marathon-PhD/")

### consistent names for first Marathon participants
df$`HeadsAndTails-Henrik.csv`$person     <- "henrikG"
df$`HeadsAndTails-Frantisek.csv`$person  <- "frantisekB"
df$`HeadsAndTails-Jonas.csv`$person      <- "jonasP"

for(i in seq_along(df)){
  df[[i]]$person <- gsub(" ", "", df[[i]]$person)
}

# add sequence id
for(i in seq_along(df)){
  df[[i]]$sequence_id <- 1:nrow(df[[i]])
}

df <- do.call(rbind, df)
rownames(df) <- NULL

# remove unrecognized input
df$sequence <- gsub("u", "", df$sequence)

# fix coin names
df$coin[df$coin == "1.0 CHF"] <- "1CHF"
df$coin[df$coin == "1.0 GPB"] <- "1GBP"
df$coin[df$coin == "1.0 EUR"] <- "1EUR"
df$coin[df$coin == "2.0 EUR"] <- "2EUR"
df$coin[df$coin == "1.00EUR"] <- "1EUR"
df$coin[df$coin == "2.00EUR"] <- "2EUR"
df$coin[df$coin == "10CZE"]   <- "10CZK"
df$coin[df$coin == "50CZE"]   <- "50CZK"

### merge data files together ----
sel_columns <- c("person", "sequence_id", "coin", "start", "sequence")
df          <- df[,sel_columns]

### align heads/tails based on coin-sides file (number sides correspond to tails) ----
source("functions/processing.R")

df[df$person == "frantisekB" & df$coin == "10CZK",]    <- swap_ht(df[df$person == "frantisekB" & df$coin == "10CZK",])
df[df$person == "frantisekB" & df$coin == "0.20GEL",]  <- swap_ht(df[df$person == "frantisekB" & df$coin == "0.20GEL",])
df[df$person == "henrikG" & df$coin == "10CZK",]       <- swap_ht(df[df$person == "henrikG" & df$coin == "10CZK",])
df[df$person == "JasonNak" & df$coin == "10CZK",]      <- swap_ht(df[df$person == "JasonNak" & df$coin == "10CZK",])
df[df$person == "JasonNak" & df$coin == "0.50EUR",]    <- swap_ht(df[df$person == "JasonNak" & df$coin == "0.50EUR",])
df[df$person == "jonasP" & df$coin == "10CZK",]        <- swap_ht(df[df$person == "jonasP" & df$coin == "10CZK",])
df[df$person == "jonasP" & df$coin == "0.20GEL",]      <- swap_ht(df[df$person == "jonasP" & df$coin == "0.20GEL",])
df[df$person == "KarolineH" & df$coin == "10CZK",]     <- swap_ht(df[df$person == "KarolineH" & df$coin == "10CZK",])
df[df$person == "KoenDerks" & df$coin == "0.20GEL",]   <- swap_ht(df[df$person == "KoenDerks" & df$coin == "0.20GEL",])
df[df$person == "RietvanBork" & df$coin == "0.20GEL",] <- swap_ht(df[df$person == "RietvanBork" & df$coin == "0.20GEL",])

### save file ----
df$dataset <- "Marathon-PhD"
write.csv(df, file = "data-raw/merged-marathon-PhD.csv", row.names = FALSE)
