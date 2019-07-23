# prerequisites for Chapter 4 exercises

prereq <- c(
  "lsa"
)

needed <- setdiff(prereq, installed.packages())

if(length(needed) > 0) {
  install.packages(needed)
}
