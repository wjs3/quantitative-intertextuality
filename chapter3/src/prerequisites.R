# prerequisites for Chapter 3 exercises

prereq <- c(
  "XML",
  "Unicode",
  "data.table",
  "stringi",
  "SnowballC",
  "textcat",
  "zoo"
)

needed <- setdiff(prereq, installed.packages())

if(length(needed) > 0) {
  install.packages(needed)
}
