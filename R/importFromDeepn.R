importFromDeepn <- function(file) {
  raw <- readLines(file)
  config <- matrix(trimws(unlist(strsplit(raw, "="))), ncol=2, byrow=TRUE)
  key <- config[,1]
  val <- config[,2]
  ind <- order(key)
  key <- key[ind]
  val <- val[ind]
  vn <- val[grep("_nonsel_vector", key)]
  vs <- val[grep("_sel_vector", key)]
  bn <- val[grep("_nonsel_bait.", key)]
  bs <- val[grep("_sel_bait.", key)]
  Data <- import(vn, vs, bn, bs)
  applyFilter(Data, as.numeric(val[key=="Threshold"]))
}
