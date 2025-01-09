### this script combines processed data from data collection sessions ###

# load the individual files
df <- rbind(
  read.csv(file = "data-raw/merged-marathon.csv"),
  read.csv(file = "data-raw/merged-bachelor.csv"),
  read.csv(file = "data-raw/merged-marathon-MSc.csv"),
  read.csv(file = "data-raw/merged-marathon-PhD.csv"),
  read.csv(file = "data-raw/merged-marathon-Manheim.csv"),
  read.csv(file = "data-raw/merged-internet.csv"),
  read.csv(file = "data-raw/merged-top-up.csv")
)

# split and merge the sequences
df <- lapply(1:nrow(df), function(i){

  temp_sequence <- strsplit(df$sequence[i], "")[[1]]

  # omit failed tosses
  temp_sequence <- temp_sequence[temp_sequence != "x"]
  if(length(temp_sequence) != 100){
    #warning(paste0(length(temp_sequence), " tosses in sequence ", i, " person: ", df$person[i]), immediate. = TRUE)
    if(length(temp_sequence) == 0)
      return(NULL)
  }

  # check data format
  if(any(!unique(temp_sequence) %in% c("h", "t"))){
    warning(paste0("unknown charactes [", paste0(unique(temp_sequence)[!unique(temp_sequence) %in% c("h", "t")], collapse = ", "), "] in sequence", i, " person: ", df$person[i]), immediate. = TRUE)
    temp_sequence <- temp_sequence[temp_sequence %in% c("h", "t")]
  }

  return(data.frame(
    person      = df$person[i],
    coin        = df$coin[i],
    dataset     = df$dataset[i],
    sequence_id = df$sequence_id[i],
    toss_id     = seq_along(temp_sequence),
    toss_start  = c(df$start[i], temp_sequence[-length(temp_sequence)]),
    toss_end    = temp_sequence
  ))
})
df <- do.call(rbind, df)

# export long format
write.csv(df, file = "analyses/data_long.csv", row.names = FALSE)

# create by-person aggregated data
df$person_coin <- with(df, paste0(person, "-", coin))

df_agg <- do.call(rbind, lapply(unique(df$person_coin), function(person_coin){

  temp_df <- df[df$person_coin == person_coin,]

  temp_heads_up <- temp_df[temp_df$toss_start == "h",]
  temp_tails_up <- temp_df[temp_df$toss_start == "t",]

  return(
    data.frame(
      heads_heads      = sum(temp_heads_up$toss_end == "h"),
      tails_heads      = sum(temp_tails_up$toss_end == "h"),
      N_start_heads_up = nrow(temp_heads_up),
      N_start_tails_up = nrow(temp_tails_up),
      person           = temp_df$person[1],
      coin             = temp_df$coin[1]
    )
  )
}))

write.csv(df_agg, file = "analyses/data_agg.csv", row.names = FALSE)


### create by-person-sequence aggregated data in chronological order
df <- read.csv(file = "analyses/data_long.csv")

# create variable encoding ordering across the data sets (sequence_id corresponds to ordering within data set)
dataset_order <- as.numeric(factor(df$dataset, levels = c( "Marathon", "Bc Thesis", "Marathon-PhD", "Marathon-MSc", "Marathon-Manheim", "Internet", "top-up")))
df <- df[order(dataset_order),]
df$person_dataset_sequence <- with(df, paste0(person, "-", dataset, "-", sequence_id))
df_time <- do.call(rbind, lapply(unique(df$person), function(person){
  temp_df <- df[df$person == person,]
  temp_df$toss_number <- 1:nrow(temp_df)
  return(temp_df)
}))
write.csv(df_time, file = "analyses/df_time.csv", row.names = FALSE)

# aggregate by ~ 100 flips with each coin for each person
df_time_agg <- do.call(rbind, lapply(unique(df$person), function(person){

  temp_df  <- df_time[df_time$person == person,]
  out      <- list()

  continue <- TRUE
  while(continue){

    if(all(temp_df$coin[1] == temp_df$coin)){
      this_df    <- temp_df
      continue   <- FALSE
    }else{
      temp_break <- which.min(temp_df$coin == temp_df$coin[1])
      this_df    <- temp_df[1:(temp_break-1),]
      temp_df    <- temp_df[temp_break:nrow(temp_df),]
    }

    n_splits <- floor(nrow(this_df) / 100)
    if(n_splits <= 1){
      splits <- list(this_df)
    }else{
      splits   <- split(this_df, cut(1:nrow(this_df), n_splits, labels = FALSE))
    }

    out <- c(out, splits)
  }

  for(i in seq_along(out)){
    out[[i]]$agg <- i
  }

  out <- do.call(rbind, lapply(out, function(xxx){

    temp_heads_up <- xxx[xxx$toss_start == "h",]
    temp_tails_up <- xxx[xxx$toss_start == "t",]
    same_side     <- xxx$toss_start == xxx$toss_end

    return(
      data.frame(
        heads_heads      = sum(temp_heads_up$toss_end == "h"),
        tails_heads      = sum(temp_tails_up$toss_end == "h"),
        N_start_heads_up = nrow(temp_heads_up),
        N_start_tails_up = nrow(temp_tails_up),
        same_side        = sum(same_side),
        N                = nrow(xxx),
        person           = xxx$person[1],
        coin             = xxx$coin[1],
        agg              = xxx$agg[1],
        from             = min(xxx$toss_number),
        to               = max(xxx$toss_number),
        mean_toss        = mean(xxx$toss_number)
      )
    )
  }))

  return(out)
}))
write.csv(df_time_agg, file = "analyses/df_time_agg.csv", row.names = FALSE)
