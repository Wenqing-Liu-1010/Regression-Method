# python formatted
df_frantisek <- read.table("data-raw/marathon/frantisek.txt", header = FALSE, sep = ",")
df_ingeborg  <- read.table("data-raw/marathon/ingeborg.txt", header = FALSE, sep = ",")

# python recovered
df_alexandra <- read.csv("data-raw/marathon/alexandra.csv", header = FALSE)
df_madlen    <- read.csv("data-raw/marathon/madlen.csv", header = FALSE)
df_ingeborg2 <- read.table("data-raw/marathon/ingeborg-log.txt", header = FALSE, sep = ",")
df_ingeborg3 <- read.table("data-raw/marathon/ingeborg-vid.txt", header = FALSE, sep = ",")

# custom formats
df_adam      <- read.csv("data-raw/marathon/adam.csv")
df_henrik    <- read.csv2("data-raw/marathon/henrik.csv")
df_jill      <- read.csv("data-raw/marathon/jill.csv", header = FALSE)

### jonas docx file mess ----
jonas_docx <- lapply(list.files("data-raw/marathon/jonas-doc/", full.names = TRUE), officer::read_docx)
jonas_docx <- sapply(jonas_docx, function(x) officer::docx_summary(x)$text)
jonas_fnames <- list.files("data-raw/marathon/jonas-doc/")
jonas_fnames <- gsub("coins", "", jonas_fnames)
jonas_fnames <- gsub(".docx", "", jonas_fnames)

jonas_docx <- gsub("1", "h", jonas_docx)
jonas_docx <- gsub("2", "t", jonas_docx)
jonas_docx <- gsub("0", "x", jonas_docx)

df_jonas <- data.frame(
  person   = "jonasP",
  sequence_id       = as.numeric(jonas_fnames),
  coin     = "0.20GEL",
  start    = substr(jonas_docx, 1, 1),
  sequence = substr(jonas_docx, 2, nchar(jonas_docx))
)

### process python formatted and recovered ----
colnames(df_frantisek) <- c("time", "coin", "start", "sequence")
colnames(df_ingeborg)  <- c("time", "coin", "start", "sequence")

df_frantisek$sequence_id <- 1:nrow(df_frantisek)
df_ingeborg$sequence_id  <- 1:nrow(df_ingeborg)

df_frantisek$person <- "frantisekB"
df_ingeborg$person  <- "ingeborgR"

df_frantisek$start <- tolower(substr(df_frantisek$start,1,1))
df_ingeborg$start  <- tolower(substr(df_ingeborg$start,1,1))

colnames(df_ingeborg2) <- c("sequence_id", "start", "sequence")
colnames(df_ingeborg3) <- c("sequence_id", "start", "sequence")

df_ingeborg2$person  <- "ingeborgR"
df_ingeborg3$person  <- "ingeborgR"

df_ingeborg$coin <- ifelse(df_ingeborg$coin == "kuna", "2HRK", df_ingeborg$coin)
df_ingeborg2$coin  <- "2HRK"
df_ingeborg3$coin  <- "2HRK"

df_frantisek$coin <- ifelse(df_frantisek$coin == "gauss20", "20DEM-silver", df_frantisek$coin)
df_frantisek$coin <- ifelse(df_frantisek$coin == "1czk", "1CZK", df_frantisek$coin)
df_frantisek$coin <- ifelse(df_frantisek$coin == "5czk", "5CZK", df_frantisek$coin)
df_frantisek$coin <- ifelse(df_frantisek$coin == "10czk", "10CZK", df_frantisek$coin)
df_frantisek$coin <- ifelse(df_frantisek$coin == "1/4USD", "0.25USD", df_frantisek$coin)
df_frantisek$coin <- ifelse(df_frantisek$coin == "1/2EUR", "0.50EUR", df_frantisek$coin)


colnames(df_alexandra) <- c("start", "coin", "sequence")
colnames(df_madlen)    <- c("start", "sequence_id", "coin", "sequence") # manually recovered sequence_ids as the logs ordering was broken

df_alexandra$sequence_id <- 1:nrow(df_alexandra)

df_alexandra$person  <- "alexandraS"
df_madlen$person     <- "madlenH"

df_alexandra$start <- tolower(substr(df_alexandra$start,1,1))
df_madlen$start    <- tolower(substr(df_madlen$start,1,1))

df_alexandra$coin <- ifelse(df_alexandra$coin == "Peso", "1MXN", df_alexandra$coin)
df_alexandra$coin <- ifelse(df_alexandra$coin == "peso", "1MXN", df_alexandra$coin)
df_madlen$coin    <- ifelse(df_madlen$coin == "50 cents (Euro)", "0.50EUR", df_madlen$coin)

df_madlen$sequence <- gsub(";", "",  df_madlen$sequence)
df_madlen$sequence <- gsub("n", "x", df_madlen$sequence)


### process the custom ones ----
df_henrik <- data.frame(
  person   = "henrikG",
  sequence_id       = 1:(ncol(df_henrik)-1),
  coin     = "2EUR",
  start    = unlist(df_henrik[1,2:ncol(df_henrik)]),
  sequence = sapply(2:ncol(df_henrik), function(i){
    paste0(df_henrik[2:nrow(df_henrik), i], collapse = "")
  })
)
df_henrik$sequence <- gsub("a", "h", df_henrik$sequence)
df_henrik$sequence <- gsub("f", "t", df_henrik$sequence)
df_henrik$start    <- tolower(substr(df_henrik$start,1,1))


colnames(df_adam) <- c("sequence_id", "person", "coin", "start", "sequence")
df_adam$person    <- "adamF"
df_adam$start     <- tolower(df_adam$start)
df_adam$sequence  <- tolower(df_adam$sequence)
df_adam$coin      <- ifelse(df_adam$coin == "rand5", "5ZAR", df_adam$coin)
df_adam$coin      <- ifelse(df_adam$coin == "eurCent5", "0.05EUR", df_adam$coin)
# the first sequence does not have start -- use the first valid outcome instead of start
# (previously wrong processing did not fix the issue -- Adam added one more flip after first revision to perserve the total number of valid flips)
df_adam$start[1]    <- substr(df_adam$sequence[1], 3, 3)
df_adam$sequence[1] <- substr(df_adam$sequence[1], 4, nchar(df_adam$sequence[1]))
# remove sequence 29 as it is a copy-paste error (same as sequence 28)
df_adam <- df_adam[df_adam$sequence_id != 29,]


df_jill <- data.frame(
  person   = "jillR",
  sequence_id       = 1:(ncol(df_jill)-1),
  coin     = c(rep("0.20EUR", 43), rep("0.20GEL", 6), rep("0.20EUR", 16)),
  start    = unlist(df_jill[1,2:ncol(df_jill)]),
  sequence = sapply(2:ncol(df_jill), function(i){
    paste0(df_jill[2:nrow(df_jill), i], collapse = "")
  })
)
df_jill$start    <- gsub("K", "h", df_jill$start)
df_jill$start    <- gsub("M", "t", df_jill$start)
df_jill$sequence <- gsub("K", "h", df_jill$sequence)
df_jill$sequence <- gsub("M", "t", df_jill$sequence)
df_jill$sequence <- gsub("F", "x", df_jill$sequence)


### merge data files together ----
sel_columns <- c("person", "sequence_id", "coin", "start", "sequence")

df_frantisek <- df_frantisek[,sel_columns]
df_ingeborg  <- df_ingeborg[,sel_columns]
df_alexandra <- df_alexandra[,sel_columns]
df_madlen    <- df_madlen[,sel_columns]

df_ingeborg2 <- df_ingeborg2[,sel_columns]
df_ingeborg3 <- df_ingeborg3[,sel_columns]

df_henrik    <- df_henrik[,sel_columns]
df_adam      <- df_adam[,sel_columns]
df_jill      <- df_jill[,sel_columns]

df <- rbind(
  df_frantisek,
  df_ingeborg,
  df_ingeborg2,
  df_ingeborg3,
  df_alexandra,
  df_madlen,
  df_henrik,
  df_adam,
  df_jill,
  df_jonas
)

### align heads/tails based on coin-sides file (number sides correspond to tails) ----
source("functions/processing.R")

df[df$person == "frantisekB" & df$coin == "10CZK",] <- swap_ht(df[df$person == "frantisekB" & df$coin == "10CZK",])
df[df$person == "adamF" & df$coin == "0.05EUR",] <- swap_ht(df[df$person == "adamF" & df$coin == "0.05EUR",])

### save file ----
df$dataset <- "Marathon"
write.csv(df, file = "data-raw/merged-marathon.csv", row.names = FALSE)
