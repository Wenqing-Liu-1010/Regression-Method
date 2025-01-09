# python formatted
df_amir    <- read.csv("data-raw/bachelor/Amir_coindata.csv", header = FALSE)
df_pierre  <- read.csv("data-raw/bachelor/pierreG_seq1-150.csv", header = TRUE)
df_davidV  <- data.frame(readxl::read_excel("data-raw/bachelor/Data coins David Voss.xlsx"))
df_kaleem  <- read.csv("data-raw/bachelor/data-kaleem.csv", header = TRUE)
df_davidKL <- read.csv("data-raw/bachelor/data-davidKL.csv")

# process
colnames(df_amir) <- c("person", "sequence_id", "coin", "start", "sequence")
df_amir$person <- "amirS"
df_amir$coin   <- gsub(" ", "", df_amir$coin)
df_amir$start  <- tolower(substr(df_amir$start, 1, 1))
df_amir$coin   <- ifelse(df_amir$coin == "0.5EUR", "0.50EUR", df_amir$coin)
df_amir$coin   <- ifelse(df_amir$coin == "0.5NZD", "0.05NZD", df_amir$coin)

# df_pierre
df_pierre$coin        <- ifelse(df_pierre$coin == "0.5EUR", "0.50EUR", df_pierre$coin)
# df_davidKL - perfectly formatted

colnames(df_davidV)   <- c("person", "sequence_id", "coin", "start", "sequence")
df_davidV$person      <- "davidV"
df_davidV$sequence_id <- gsub("V", "", df_davidV$sequence_id)
df_davidV$coin        <- ifelse(df_davidV$coin == "1 Euro", "1EUR", df_davidV$coin)
df_davidV$coin        <- ifelse(df_davidV$coin == "1 euro", "1EUR", df_davidV$coin)
df_davidV$coin        <- ifelse(df_davidV$coin == "1eu", "1EUR",   df_davidV$coin)
df_davidV$coin        <- ifelse(df_davidV$coin == "10ct", "0.10EUR", df_davidV$coin)
df_davidV$coin        <- ifelse(df_davidV$coin == "20ct", "0.20EUR", df_davidV$coin)
df_davidV$coin        <- ifelse(df_davidV$coin == "20 eurocent", "0.20EUR", df_davidV$coin)
df_davidV$coin        <- ifelse(df_davidV$coin == "50ct", "0.50EUR", df_davidV$coin)
df_davidV$coin        <- ifelse(df_davidV$coin == "50 cents", "0.50EUR", df_davidV$coin)
df_davidV$coin        <- ifelse(df_davidV$coin == "5ct euro", "0.05EUR", df_davidV$coin)
df_davidV$sequence    <- tolower(df_davidV$sequence)
# first 10 sequences are starting always the same side up ...
df_davidV.1 <- df_davidV[df_davidV$sequence_id %in% 1:10,1:5]
df_davidV.2 <- df_davidV[!df_davidV$sequence_id %in% 1:10,1:5]
df_davidV.1 <- do.call(rbind, lapply(1:nrow(df_davidV.1), function(i){
  return(data.frame(
    "person"        = df_davidV.1$person[i],
    "sequence_id"   = df_davidV.1$sequence_id[i],
    "coin"          = df_davidV.1$coin[i],
    "start"         = df_davidV.1$start[i],
    "sequence"      = strsplit(df_davidV.1$sequence[i], "")[[1]]
  ))
}))
df_davidV <- rbind(df_davidV.1, df_davidV.2)

df_kaleem$coin        <- ifelse(df_kaleem$coin == "1 euro", "1EUR", df_kaleem$coin)
df_kaleem$coin        <- ifelse(df_kaleem$coin == "2 euro", "2EUR", df_kaleem$coin)
df_kaleem$coin        <- ifelse(df_kaleem$coin == "10 cents", "0.10EUR", df_kaleem$coin)
df_kaleem$coin        <- ifelse(df_kaleem$coin == "20 cents", "0.20EUR", df_kaleem$coin)
df_kaleem$coin        <- ifelse(df_kaleem$coin == "50 cents", "0.50EUR", df_kaleem$coin)
df_kaleem$coin        <- ifelse(df_kaleem$coin == "2 Francs", "2CHF", df_kaleem$coin)
df_kaleem$coin        <- ifelse(df_kaleem$coin == "20 pence", "0.20GBP", df_kaleem$coin)
df_kaleem$coin        <- ifelse(df_kaleem$coin == "50 pence", "0.50GBP", df_kaleem$coin)
df_kaleem$sequence    <- gsub("y", "x", df_kaleem$sequence)

### merge data files together ----
sel_columns <- c("person", "sequence_id", "coin", "start", "sequence")

df_pierre  <- df_pierre[,sel_columns]
df_amir    <- df_amir[,sel_columns]
df_davidV  <- df_davidV[,sel_columns]
df_kaleem  <- df_kaleem[,sel_columns]
df_davidKL <- df_davidKL[,sel_columns]

df <- rbind(
  df_pierre,
  df_amir,
  df_davidV,
  df_kaleem,
  df_davidKL
)

### align heads/tails based on coin-sides file (number sides correspond to tails) ----
source("functions/processing.R")

df[df$person == "kaleemU" & df$coin == "2EUR" & df$sequence_id == 1,] <- swap_ht(df[df$person == "kaleemU" & df$coin == "2EUR" & df$sequence_id == 1,])
df[df$person == "kaleemU" & df$coin == "0.50GBP" ,]    <- swap_ht(df[df$person == "kaleemU" & df$coin == "0.50GBP",])
df[df$person == "kaleemU" & df$coin == "2CHF" ,]    <- swap_ht(df[df$person == "kaleemU" & df$coin == "2CHF",])

### save file ----
df$dataset <- "Bc Thesis"
write.csv(df, file = "data-raw/merged-bachelor.csv", row.names = FALSE)
