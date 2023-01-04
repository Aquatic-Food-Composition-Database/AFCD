# Custom user functions


# this function comes from Chris Free's 'freeR' package, which currently
# does not work due to issues with fishbase. I have moved his function here
# so we aren not
nwords_freeR <- function(x){
  nwords <- sapply(strsplit(x, " "), length)
  return(nwords)
}