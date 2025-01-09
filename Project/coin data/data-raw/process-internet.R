df_felipe <- as.data.frame(readxl::read_excel("data-raw/internet/FelipeFontanaVieira - Coin tosses.xlsx"))
df_nippold <- read.csv2("data-raw/internet/Franziska Nippold.csv")

f_frederik  <- list.files("data-raw/internet/frederik/")
df_frederik <- do.call(rbind, lapply(f_frederik, function(f) read.csv(file.path("data-raw/internet/frederik/", f))))

#
df_felipe           <- df_felipe[-nrow(df_felipe),c(1, 2, 3, 7, 8)]
colnames(df_felipe) <- c("person", "sequence_id", "coin", "start", "sequence")
df_felipe$person    <- "FelipeFontanaVieira"
df_felipe$start     <- tolower(df_felipe$start)

df_felipe$coin <- gsub(" ", "", df_felipe$coin)
df_felipe$coin <- gsub(",", ".", df_felipe$coin)


df_nippold$person <- "FranziskaNippold"
df_nippold$coin[df_nippold$coin == "50ct"] <- "0.50EUR"
df_nippold$coin[df_nippold$coin == "20ct"] <- "0.20EUR"
df_nippold$coin[df_nippold$coin == "Rs 2 (Indian 2-rupee coin)"] <- "2INR"
df_nippold$sequence <- gsub("f", "x", df_nippold$sequence)
df_nippold$sequence <- gsub(" ", "", df_nippold$sequence)


df_frederik$person <- "FrederikAust"
df_frederik$coin[df_frederik$coin == "50 Euro cent"]  <- "0.50EUR"
df_frederik$coin[df_frederik$coin == "50 euro cent"]  <- "0.50EUR"
df_frederik$coin[df_frederik$coin == "50 Euro cents"] <- "0.50EUR"
df_frederik$coin[df_frederik$coin == "50 euro cents"] <- "0.50EUR"
df_frederik$coin[df_frederik$coin == "20 Euro cent"]  <- "0.20EUR"
df_frederik$coin[df_frederik$coin == "20 Euro cents"] <- "0.20EUR"
df_frederik$coin[df_frederik$coin == "10 Euro cents"] <- "0.10EUR"
df_frederik$coin[df_frederik$coin == "2 Euro"]        <- "2EUR"
df_frederik$coin[df_frederik$coin == "1 Euro"]        <- "1EUR"
df_frederik$sequence_id <- 1:nrow(df_frederik)
df_frederik$sequence <- gsub("u", "", df_frederik$sequence)


### merge data files together ----
sel_columns <- c("person", "sequence_id", "coin", "start", "sequence")

df_felipe     <- df_felipe[,sel_columns]
df_nippold    <- df_nippold[,sel_columns]
df_frederik   <- df_frederik[,sel_columns]


df <- rbind(
  df_felipe,
  df_nippold,
  df_frederik
)

### align heads/tails based on coin-sides file (number sides correspond to tails) ----
source("functions/processing.R")

df[df$person == "FelipeFontanaVieira" & df$coin == "1EUR",]    <- swap_ht(df[df$person == "FelipeFontanaVieira" & df$coin == "1EUR",])
df[df$person == "FranziskaNippold",]  <- swap_ht(df[df$person == "FranziskaNippold",])

### save file ----
df$dataset <- "Internet"
write.csv(df, file = "data-raw/merged-internet.csv", row.names = FALSE)
