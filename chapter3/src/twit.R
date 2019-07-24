library(data.table)
library(parallel)
source(file.path("src", "twokenize.R"))

twit.timefmt <- "%Y-%m-%d %H:%M:%S"

vTokenizeStr <- function(tweets, mc.cores=1) {
  unlist(
    mclapply(mclapply(tweets, tokenize, mc.cores=mc.cores),
      paste, collapse=" ", mc.cores=mc.cores)
  )
}

vStripTwit <- function(tweets, pattern=regex_or(AtMention, url), replacement="") {
  stri_replace_all_regex(tweets, pattern, replacement)
}

mcHtmlUnescapeText <- function(text, mc.cores=1) {
    unlist(mclapply(text, mc.cores=mc.cores, FUN=function(t) {
        m <- stri_locate_all_regex(t, "&[a-zA-Z]{2,8};")[[1]]

        if (is.na(m[1])) {
          return(t)
        }

        # reverse order of matches, so later replacements don't change
        # character indices of earlier matches

        m <- matrix(m[nrow(m):1,], ncol=2)

        apply(m, 1, function(row) {
          t_ <- t
          ent <- stri_sub(t_, from=row[1], to=row[2])
          replacement <- htmlUnescapeEnt(ent)
          stri_sub(t_, from=row[1], to=row[2]) <- replacement
          t <<- t_
        })

        return(t)
    }))
}

twit.user.as.dt <- function (file, quiet=F) {
  # ingest a bunch of tweets from a single file

  if (quiet != T) {
    cat("Reading", file, "\n")
  }

  one.giant.string <- paste(
    scan(file, what="character", encoding="UTF-8", sep="\n", quiet=T),
    collapse="\n"
  )

  recs <- unlist(stri_split_fixed(
    str = one.giant.string,
    pattern = "\n}\n"
  ))

  na.omit(as.data.table(
    stri_match_first(
      str = recs,
      regex = "(\\d{4}-\\d{2}-\\d{2} \\d{2}:\\d{2}:\\d{2}) (\\d+) \\{\n(.+)"
    )[,2:4]
  ))
}

twit.ingest.corpus <- function(dir, ncores=1) {
  # ingest a whole corpus of tweets: one directory containing text files
  # which group tweets by user id

  cat("Checking corpus", dir, "... ")

  files <- grep("^\\d+\\.dat$", dir(dir), value=T, perl=T)
  cat("found", length(files), "files\n")

  rbindlist(
    mclapply(files, mc.cores = ncores, FUN=function(file) {
      tweets.this.user <- twit.user(file.path(dir, file), quiet=T)

      if (is.null(tweets.this.user)) { return(NULL) }

      userid <- substr(file, 1, nchar(file)-4)
      tweets.this.user[,userid := rep(userid, times=nrow(tweets.this.user))][]
    })
  )
}

twit.user.as.tess <- function(file, quiet=F) {
  # ingest a bunch of tweets from a single file

  twit.object <- new.env(hash=T)
  assign("file", file, envir=twit.object)

  tweets <- twit.user.as.dt(file=file, quiet=quiet)

  if (nrow(tweets) > 1) {
    setnames(tweets, c("time", "unitid", "tweet"))

    loc.table <- tweets[, .(
      time = as.POSIXct(time),
      unitid = unitid
    )]

    assign("loc", loc.table, envir=twit.object)

    tok.table <- tweets[,
      .(display = tokenizeRawTweetText(tweet)), by=unitid
    ][, `:=`(
      form = srt.standardize(display),
      tokenid = .I
    )][,
      type := factor(
          x = stri_detect(form, charclass = "\\p{letter}"),
          levels = c(T, F),
          labels = c("W", "P")),
    ]

    setkey(tok.table, tokenid)
    assign("tokens", tok.table, envir=twit.object)

  } else {
    warning("Parser found no tweets!")
  }

  return(twit.object)
}

twit.load.txt <- function(file, quiet=F) {
  # ingest a bunch of tweets from a single file

  twit.object <- new.env(hash=T)
  assign("file", file, envir=twit.object)

  if (quiet != T) {
    cat("Reading", file, "\n")
  }

  tweets <- data.table(read.table(
    file = file,
    encoding = "UTF-8",
    colClasses = "character",
    sep = "\t",
    quote = "",
    comment.char = "",
    row.names = NULL,
    col.names = c("label", "tweet")
  ))[,
    unitid := .I
  ]

  if (nrow(tweets) > 1) {

    loc.table <- tweets[, .(
      unitid = unitid,
      label = label
    )]

    assign("loc", loc.table, envir=twit.object)

    tok.table <- tweets[,
      .(display = tokenizeRawTweetText(tweet)), by=unitid
    ][, `:=`(
      form = srt.standardize(display),
      tokenid = .I
    )][,
      type := factor(
        x = stri_detect(form, charclass = "\\p{letter}"),
        levels = c(T, F),
        labels = c("W", "P")
      )
    ]

    setkey(tok.table, tokenid)
    assign("tokens", tok.table, envir=twit.object)

  } else {
    warning("Parser found no tweets!")
  }

  return(twit.object)
}


twit.load.small.corpus <- function(file) {
  corpus <- as.data.table(
    do.call(rbind,
            stri_split(scan(file, sep = "\n", what = "character"), fixed = "\t")
    )
  )
  setnames(corpus, unlist(corpus[1]))[2:.N]
}



twit.parse.phrase <- function(text, tokenizer=tokenize){
  # process a single srt phrase

  data.table(
    display = tokenizer(text)
  )[, `:=`(
    form = srt.standardize(display),
    type = factor(
      x = stri_detect(display, charclass = "\\p{letter}"),
      levels = c(T,F),
      labels = c("W","P"))
  )][]
}

twit.parse.unit <- function(unitid, text) {
  # just a wrapper for twit.parse.phrase
  # at least for now

  twit.parse.phrase(text)[, unitid := unitid][]
}

