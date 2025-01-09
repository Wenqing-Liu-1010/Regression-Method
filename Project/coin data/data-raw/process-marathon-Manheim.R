# python formatted
df_anton <- read.table("data-raw/marathon-Manheim/HeadsandTails Anton.txt", header = TRUE, sep = ",")
df_chris <- read.table("data-raw/marathon-Manheim/HeadsandTails Chris.txt", header = TRUE, sep = ",")
df_malte <- read.table("data-raw/marathon-Manheim/HeadsandTails Malte.txt", header = TRUE, sep = ",")
df_sara  <- read.table("data-raw/marathon-Manheim/HeadsandTails Sara.txt", header = TRUE, sep = ",")

### process python formatted and recovered ----
df_anton$person  <- "AntonZoubek"
df_chris$person  <- "ChrisGabrielIslam"
df_malte$person  <- "MalteZoubek"
df_sara$person   <- "SaraShabani"

df <- list(
  df_anton,
  df_chris,
  df_malte,
  df_sara
)

sel_columns   <- c("person", "sequence_id", "coin", "start", "sequence")
df <- lapply(df, function(x){
  colnames(x)   <- c("time", "coin", "start", "sequence", "person")
  x$sequence_id <- 1:nrow(x)
  x$start       <- tolower(substr(x$start,1,1))
  x$coin        <- toupper(x$coin)
  x             <- x[,sel_columns]
  return(x)
})
df <- do.call(rbind, df)


# fix YEN-JPY (found in revision)
df$coin[df$coin == "5YEN"] <- "5JPY"


### align heads/tails based on coin-sides file (number sides correspond to tails) ----
source("functions/processing.R")
# all good


### save file ----
df$dataset <- "Marathon-Manheim"
write.csv(df, file = "data-raw/merged-marathon-Manheim.csv", row.names = FALSE)
