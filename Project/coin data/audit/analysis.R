### process auditing ----
df       <- read.csv("audit/audit-dana.csv")
df$coded <- sapply(1:nrow(df), function(i) paste0(df[i, grepl("X", colnames(df))], collapse = ""))

df <- df[c("person", "dataset", "sequence", "coded")]
df <- df[df$coded != "",]

df$coded <- trimws(df$coded)
df$coded <- tolower(df$coded)
df$coded <- gsub("f", "x", df$coded)

# load data set
data <- rbind(
  read.csv(file = "data-raw/merged-marathon.csv"),
  read.csv(file = "data-raw/merged-bachelor.csv"),
  read.csv(file = "data-raw/merged-marathon-MSc.csv"),
  read.csv(file = "data-raw/merged-marathon-PhD.csv"),
  read.csv(file = "data-raw/merged-marathon-Manheim.csv"),
  read.csv(file = "data-raw/merged-internet.csv"),
  read.csv(file = "data-raw/merged-top-up.csv")
)


### merge ----
data$merge <- paste0(data$person, "-", data$dataset, "-", data$sequence_id)
df$merge   <- paste0(df$person,   "-", df$dataset,   "-", df$sequence)

df <- merge(data, df[,c("coded", "merge")], by = "merge", all = FALSE)

# deal with davidV odd sequences
df.david <- df[df$person == "davidV", ]
df.david <- do.call(rbind, lapply(unique(df.david$sequence_id), function(id) {
  temp <- df.david[df.david$sequence_id == id,]
  temp$sequence <- paste0(temp$sequence, collapse = "")
  return(temp[1,])
}))

df <- rbind(df[df$person != "davidV", ], df.david)


### check biassedness ----
get_p_same <- function(x){
  # create sequences
  x <- strsplit(x, "")[[1]]
  x <- cbind(start = x[-length(x)], stop = x[-1])

  # remove those that didn't start/land on heads/tails
  x <- x[x[,1] %in% c("h", "t") & x[,2] %in% c("h", "t"),]
  return(mean(x[,1] == x[,2]))
}
get_p_heads <- function(x){
  x <- strsplit(x, "")[[1]]
  x <- x[x %in% c("h", "t")]

  return(mean(x == "h"))
}

df$p_same.orig <- sapply(df$sequence, get_p_same)
df$p_same.new  <- sapply(df$coded,    get_p_same)

mean(df$p_same.orig)
mean(df$p_same.new)


df$p_heads.orig <- sapply(df$sequence, get_p_heads)
df$p_heads.new  <- sapply(df$coded,    get_p_heads)

mean(df$p_heads.orig)
mean(df$p_heads.new)
