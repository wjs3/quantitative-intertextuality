library(stringi)
library(data.table)
library(SnowballC)
source(file.path("src", "twit.R"))

# constants

# a set of useful regular expressions
srt.re <- new.env()
with(srt.re, {
  nonword <- "\\W+"
  word <- "\\w+"
  pbound <- "([:;\\.!\\?]+[”\"]?\\s*)"
  notseen <- "::::"
})


# functions

srt.standardize <- function(text) {
  # flatten orthographic variation
  stri_trans_tolower(
    stri_trim_both(text, "\\p{Letter}")
  )
}


srt.parse.phrase <- function(text) {
  # process a single srt phrase

  data.table(
    display = tokenize(text)
  )[,
    form := srt.standardize(display),
  ][,
    type := factor(
      x = stri_detect(form, charclass = "\\p{Letter}"),
      levels = c(T,F),
      labels = c("W","P"))
  ]
}


srt.parse.unit <- function(unitid, text) {
  # parse a single srt unit

  delimited <- gsub(srt.re$pbound, paste("\\1", srt.re$notseen, sep=""), text, perl=T)
  phrases <- strsplit(delimited, srt.re$notseen)[[1]]

  rbindlist(
    lapply(phrases, srt.parse.phrase)
  )[, unitid := unitid]
}


srt.as.dt <- function(file) {
  # read the file
  cat("Reading", file, "\n")
  s <- stri_replace_all(
    str = paste(scan(file=file, what="character", sep="\n"), collapse="\n"),
    regex = "([^\\n]+\\n\\d{2}:\\d{2}:\\d{2},\\d{3} --> \\d{2}:\\d{2}:\\d{2},\\d{3}\\n)",
    replacement="### END REC ###\n$1"
  )
  s <- paste(s, "### END REC ###", sep="")

  # parse the caption entries
  dt <- data.table(stri_match_all(s,
      regex = "([^\\n]+)\\n(\\d{2}:\\d{2}:\\d{2}),\\d{3} --> (\\d{2}:\\d{2}:\\d{2}),\\d{3}\\n(.+?)### END REC ###",
      dotall = T
    )[[1]][,2:5]
  )[,
    .(
      unitid = .I,
      start = as.POSIXct(strptime(V2, "%H:%M:%S")),
      end = as.POSIXct(strptime(V3, "%H:%M:%S")),
      text = stri_replace_all(V4, regex="<.*?>", replacement="")
    )
  ][,
    text := stri_trim_both(
      stri_replace_all(text, regex="\\n", replacement=" ")
    )
  ]
}


srt.as.tess <- function(file) {
  # ingest a captions file and create a tesserae-style object
  # containing token table and index

  srt.object <- new.env(hash = T)

  assign("file", file, envir=srt.object)

  # read the file
  dt <- srt.as.dt(file)

  # create the "locus" (unit) index
  #   - keep timestamps in case they're useful later
  assign("loc", dt[, .(unitid, start, end)], envir=srt.object)

  # create the "tokens" table
  assign("tokens", envir=srt.object,
    rbindlist(
     do.call(Vectorize(srt.parse.unit, SIMPLIFY=F), dt[, .(unitid, text)])
   )[, tokenid := .I]
  )
  setkey(srt.object$tokens, tokenid)

  return(srt.object)
}


got.load.txt <- function(file) {
  # ingest a captions file and create a tesserae-style object
  # containing token table and index

  text.object <- new.env(hash = T)

  # read the file
  cat("Reading", file, "\n")

  dt <- data.table(read.table(file,
    encoding = "UTF-8",
    colClasses = "character",
    sep = "\t",
    quote = "",
    comment.char = "",
    row.names = NULL,
    col.names = c("label", "text")
  ))[,
    unitid := .I
  ]

  assign("file", file, envir=text.object)

  # create the "locus" (unit) index
  #   - keep timestamps in case they're useful later
  assign("loc", dt[, .(unitid, label)], envir=text.object)

  # create the "tokens" table
  assign("tokens", envir=text.object,
    rbindlist(
      do.call(Vectorize(srt.parse.unit, SIMPLIFY=F), dt[, .(unitid, text)])
    )[,
      tokenid := .I
    ]
  )
  setkey(text.object$tokens, tokenid)

  return(text.object)
}


got.dl.file <- function(name, dest=tempdir(), remote.url="http://tvsubs.net/files/") {
  # download subtitles for an episode

  # strip ".srt" extension if present
  name <- stri_replace_first_regex(name, "\\.srt$", "")

  zip.file <- paste(name, "zip", sep=".")
  srt.file <- paste(name, "srt", sep=".")

  remote.zip <- paste(
    stri_replace_first_regex(remote.url, "/$", ""),
    zip.file,
    sep = "/"
  )
  temp.zip <- file.path(tempdir(), zip.file)
  temp.srt <- file.path(tempdir(), srt.file)

  # download the zip file
  dl.stat <- download.file(
    url = remote.zip,
    destfile = file.path(tempdir(), zip.file),
    mode = "wb"
  )
  if (dl.stat != 0) {
    stop(paste("Download failed for", remote.zip, sep=" "))
  }

  # check that expected file is there
  if (! srt.file %in% unzip(temp.zip, list=T)$Name) {
    stop(paste("Didn't find expected file", srt.file, "in", temp.zip, sep=" "))
  }

  # unzip
  unzip(temp.zip, files=srt.file, exdir=dest)

  return(file.path(dest, srt.file))
}

got.dl.episode <- function(season, episode) {
  # a convenience wrapper for getting episodes

  name <- sprintf("Game.of.Thrones.S%02iE%02i.HDTV.en", season, episode)

  got.dl.file(name = name, dest=file.path("data", "got"))
}


got.score.link <- function(tokid, freq.index) {
  tokid <- unlist(tokid)

  freq <- unlist(mget(s$tokens[tokid, form], freq.index))
  dist <- abs(diff(tokid[order(freq)][1:2]))

  log(sum(freq^-1) / dist)
}


got.score.doc <- function(linklist, freq.index) {
  lapply(linklist, got.score.link, freq.index=freq.index)
}


got.stemmer <- function(token) {
  # a stemmer for use with Game of Thrones exercises

  stri_replace_first_regex(
    wordStem(stri_replace_first_regex(token, contractions, "$1")),
    "^$",
    token
  )
}


