# prerequisites for Chapter 4 exercises

prereq <- c(
  "gtools",
  "RecordLinkage",
  "vecsets",
  "stylo",
  "lsa",
  "SparseM"
)

needed <- setdiff(prereq, installed.packages())

if(length(needed) > 0) {
  install.packages(needed)
}
