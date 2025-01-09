counter <- function(){

  n_target <- 100

  # try to log in the telegram bot and stream the data:
  if(system.file(package = "telegram.bot") == ""){
    try(install.packages("telegram.bot"))
  }
  bot <- tryCatch(telegram.bot::Bot(token = "5805745930:AAFnmIvYFuYydNEMq8ilROREyFxM8UDnPxI"), error = function(e) return(NULL))

  # define some functions
  get_key       <- function(prompt){
    key <- NULL
    while(is.null(key)){
      key <- tolower(readline(prompt))
      if(nchar(key) != 1){
        warning("Enter only one key!", immediate. = TRUE)
        key <- NULL
      }
    }
    return(key)
  }
  n_valid       <- function(x){
    return(length(x[x %in% c("h", "t")]))
  }
  last_valid    <- function(x){
    x <- x[x %in% c("h", "t")]
    return(x[length(x)])
  }
  log_response  <- function(start, mapping){
    key <- tolower(readline(print_mapping(start, mapping)))
    if(nchar(key) != 1 || !key %in% unlist(mapping)){
      warning("The entered key was not recognized!", immediate. = TRUE)
      key <- "u"
    }else if(key == mapping[["input_heads"]]){
      key <- "h"
    }else if(key == mapping[["input_tails"]]){
      key <- "t"
    }else if(key == mapping[["failed_toss"]]){
      key <- "x"
    }else if(key == mapping[["undo_toss"]]){
      key <- "undo"
    }else if(key == mapping[["change_coin"]]){
      key <- "change"
    }
    return(key)
  }
  print_mapping <- function(start, mapping){
    sprintf(
      "Press: '%2$s' for Heads, '%3$s' for Tails, '%4$s' for Fail, '%5$s' to Undo.    (Start on '%1$s')",
      switch(start, "h" = "Heads", "t" = "Tails"),
      mapping[["input_heads"]],
      mapping[["input_tails"]],
      mapping[["failed_toss"]],
      mapping[["undo_toss"]]
      )
  }
  print_info    <- function(this_sequence){
    cat(sprintf("(tosses: %1$s)\t %2$s", n_valid(this_sequence), paste0(this_sequence, collapse = "")))
  }
  set_start     <- function(sequence_id){
    side <- sample(c("h", "t"), 1)
    cat("\n---------------------- \n")
    cat(sprintf(
      "Sequence %1$s: Start with %2$s \n",
      sequence_id,
      switch(side, "h" = "Heads", "t" = "Tails")))
    cat("---------------------- \n")
    return(side)
  }
  post_data     <- function(bot, df){
    if(!is.null(bot)){
      suppressWarnings(bot$sendMessage(chat_id = "108780655", text = paste0(df[nrow(df),], collapse = ",")))
    }
  }

  # create the file if it does not exist
  if(!file.exists("HeadsAndTails.csv")){
    df <- data.frame(matrix(ncol = 5, nrow = 0))
    colnames(df) <- c("date", "person", "coin", "start", "sequence")
    write.csv(df, file = "HeadsAndTails.csv", row.names = FALSE)
  }

  # read the file
  df <- read.csv("HeadsAndTails.csv")

  if(nrow(df) == 0){
    print("Starting new data colection.")
  }else{
    sprintf("Cointinuing previous data collection. Number of previously recorded sequences: %1$s", nrow(df))
  }

  # specify keyboard and input
  input_heads <- get_key("Select the key you want to press for heads: ")
  input_tails <- get_key("Select the key you want to press for tails: ")
  failed_toss <- get_key("Select the key you want to press for a failed attempt: ")
  undo_toss   <- get_key("Select the key you want to use for undoing you previous input: ")
  #change_coin <- get_key("Select the key you want to use to change the coin: ")
  change_coin <- "not-used"

  mapping <- list(
    input_heads = input_heads,
    input_tails = input_tails,
    failed_toss = failed_toss,
    undo_toss   = undo_toss,
    change_coin = change_coin
  )


  # start collection
  if(nrow(df) == 0){
    this_id <- readline("Enter your name")
  }else{
    this_id <- df$person[1]
  }
  this_sequence    <- character()
  this_sequence_id <- nrow(df) + 1
  this_coin        <- readline("Which coin are you using?")
  this_start       <- set_start(this_sequence_id)

  repeat{

    while(n_valid(this_sequence) < n_target){

      this_response <- log_response(last_valid(c(this_start, this_sequence)), mapping)

      # deal with undo:
      if(this_response == "undo"){
        if(length(this_sequence) == 0){
          warning("You undid all responses in the current sequence.", immediate. = TRUE)
          next
        }else{
          this_sequence <- this_sequence[-length(this_sequence)]
          next
        }
      }

      # deal with coin change:
      if(this_response == "change"){

        df <- rbind(df, data.frame(
          "date"        = as.character(Sys.time()),
          "person"      = this_id,
          "coin"        = this_coin,
          "start"       = this_start ,
          "sequence"    = paste0(this_sequence, collapse = "")
        ))
        write.csv(df, file = "HeadsAndTails.csv", row.names = FALSE)
        try(post_data(bot, df))

        this_sequence    <- character()
        this_sequence_id <- nrow(df) + 1
        this_coin        <- readline("Which coin are you using?")
        this_start       <- set_start(this_sequence_id)

        next
      }

      this_sequence[length(this_sequence) + 1] <- this_response
      print_info(this_sequence)
    }

    # save and reset
    df <- rbind(df, data.frame(
      "date"        = as.character(Sys.time()),
      "person"      = this_id,
      "coin"        = this_coin,
      "start"       = this_start ,
      "sequence"    = paste0(this_sequence, collapse = "")
    ))
    write.csv(df, file = "HeadsAndTails.csv", row.names = FALSE)
    try(post_data(bot, df))

    this_sequence    <- character()
    this_sequence_id <- nrow(df) + 1

    # force change coin every 1000 trials
    if((this_sequence_id - 1) %% 10 == 0){
      cat("\nYou finished 1000 coin tosses with the current coin. Change the coin!\n")
      this_coin  <- readline("What is your new coin?")
    }

    this_start       <- set_start(this_sequence_id)

  }
}
counter()
