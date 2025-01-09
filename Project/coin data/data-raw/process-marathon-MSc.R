df <- lapply(list.files("data-raw/marathon-Msc/", full.names = TRUE), read.csv)
names(df) <- list.files("data-raw/marathon-Msc/")

### consistent names for first Marathon participants
df$`HeadsAndTails-HenrikG.csv`$person               <- "henrikG"
df$`HeadsAndTails-Alexandra Sarafoglou.csv`$person  <- "alexandraS"
df$`HeadsAndTails-Frantisek Bartos.csv`$person      <- "frantisekB"

### fix some data collection issues:

# fix names
df$`HeadsAndTails-Adrian Karami.csv`$person <- "AdrianKarami"
df$`HeadsAndTails-Bohan Fu.csv`$person      <- "BohanFu"
df$`HeadsAndTails-Arne John.csv`$person     <- "ArneJohn"

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

# fix coding issues
df[df$person == "AdrianKarami"      & df$sequence_id %in%  1:19, "coin"] <- "0.20EUR"
df[df$person == "AdrianKarami"      & df$sequence_id %in% 20:29, "coin"] <- "2EUR"
df[df$person == "AdrianKarami"      & df$sequence_id %in% 30:34, "coin"] <- "1GBP"
df[df$person == "MaraBialas"        & df$sequence_id %in% 20:29, "coin"] <- "10CZK"
df[df$person == "TheresaLeidinger"  & df$sequence_id %in%  1:9 , "coin"] <- "2EUR"
df[df$person == "TianqiPeng"        & df$sequence_id %in%  1:9 , "coin"] <- "1EUR"

df$coin[df$coin == '"0.20EUR"']      <- "0.20EUR"
df$coin[df$coin == '"0.50EUR"']      <- "0.50EUR"
df$coin[df$coin == "0.2EUR"]         <- "0.20EUR"
df$coin[df$coin == "0.5 SGD"]        <- "0.50SGD"
df$coin[df$coin == "0.5SGD"]         <- "0.50SGD"
df$coin[df$coin == "1DeutscheMark"]  <- "1DEM"
df$coin[df$coin == "1DM"]            <- "1DEM"
df$coin[df$coin == "2Euro"]          <- "2EUR"
df$coin[df$coin == "CAD2"]           <- "2EUR"
df$coin[df$coin == "EUR.05"]         <- "0.05EUR"
df$coin[df$coin == "EUR2"]           <- "2EUR"
df$coin[df$coin == "RON.50"]         <- "0.50RON"
df$coin[df$coin == "USD.25"]         <- "0.25USD"


### merge data files together ----
sel_columns <- c("person", "sequence_id", "coin", "start", "sequence")
df          <- df[,sel_columns]


### align heads/tails based on coin-sides file (number sides correspond to tails) ----
source("functions/processing.R")

df[df$person == "AdrianKarami" & df$coin == "0.20EUR" & df$sequence_id %in% 10:19,]    <- swap_ht(df[df$person == "AdrianKarami" & df$coin == "0.20EUR" & df$sequence_id %in% 10:19,])
df[df$person == "AdrianKarami" & df$coin == "1GBP",]          <- swap_ht(df[df$person == "AdrianKarami" & df$coin == "1GBP",])
df[df$person == "alexandraS" & df$coin == "10CZK",]           <- swap_ht(df[df$person == "alexandraS" & df$coin == "10CZK",])
df[df$person == "ArneJohn" & df$coin == "5SEK",]              <- swap_ht(df[df$person == "ArneJohn" & df$coin == "5SEK",])
df[df$person == "BohanFu" & df$coin == "0.02EUR",]            <- swap_ht(df[df$person == "BohanFu" & df$coin == "0.02EUR",])
df[df$person == "henrikG" & df$coin == "10CZK",]              <- swap_ht(df[df$person == "henrikG" & df$coin == "10CZK",])
df[df$person == "henrikG" & df$coin == "50CZK",]              <- swap_ht(df[df$person == "henrikG" & df$coin == "50CZK",])
df[df$person == "JanYang" & df$coin == "0.50EUR",]            <- swap_ht(df[df$person == "JanYang" & df$coin == "0.50EUR",])
df[df$person == "JanYang" & df$coin == "0.20EUR",]            <- swap_ht(df[df$person == "JanYang" & df$coin == "0.20EUR",])
df[df$person == "JoycePang" & df$coin == "0.50SGD",]          <- swap_ht(df[df$person == "JoycePang" & df$coin == "0.50SGD",])
df[df$person == "JoycePang" & df$coin == "0.20EUR",]          <- swap_ht(df[df$person == "JoycePang" & df$coin == "0.20EUR",])
df[df$person == "JoycePang" & df$coin == "1SGD",]             <- swap_ht(df[df$person == "JoycePang" & df$coin == "1SGD",])
df[df$person == "MagdaMatetovici" & df$coin == "0.50RON",]    <- swap_ht(df[df$person == "MagdaMatetovici" & df$coin == "0.50RON",])
df[df$person == "MagdaMatetovici" & df$coin == "0.50EUR",]    <- swap_ht(df[df$person == "MagdaMatetovici" & df$coin == "0.50EUR",])
df[df$person == "MagdaMatetovici" & df$coin == "5SEK",]       <- swap_ht(df[df$person == "MagdaMatetovici" & df$coin == "5SEK",])
df[df$person == "MaraBialas" & df$coin == "10CZK",]           <- swap_ht(df[df$person == "MaraBialas" & df$coin == "10CZK",])
df[df$person == "RoyMichaelMoore" & df$coin == "1EUR",]       <- swap_ht(df[df$person == "RoyMichaelMoore" & df$coin == "1EUR",])
df[df$person == "RoyMichaelMoore" & df$coin == "0.50EUR",]    <- swap_ht(df[df$person == "RoyMichaelMoore" & df$coin == "0.50EUR",])
df[df$person == "SjoerdTerpstra" & df$coin == "0.50RON",]     <- swap_ht(df[df$person == "SjoerdTerpstra" & df$coin == "0.50RON",])
df[df$person == "TianqiPeng",]       <- swap_ht(df[df$person == "TianqiPeng",])
df[df$person == "TingPan",]          <- swap_ht(df[df$person == "TingPan",])
df[df$person == "XiaochangZhao",]    <- swap_ht(df[df$person == "XiaochangZhao",])
df[df$person == "XiaoyiLin",]        <- swap_ht(df[df$person == "XiaoyiLin",])


### save file ----
df$dataset <- "Marathon-MSc"
write.csv(df, file = "data-raw/merged-marathon-MSc.csv", row.names = FALSE)
