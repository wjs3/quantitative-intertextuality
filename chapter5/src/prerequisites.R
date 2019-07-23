# prerequisites for Chapter 5 exercises

prereq <- c(
  "e1071"
)

needed <- setdiff(prereq, installed.packages())

if(length(needed) > 0) {
  install.packages(needed)
}
