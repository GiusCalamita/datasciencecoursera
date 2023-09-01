sheets.hendrickx <- function(res, T){
  res * (0.4470 + 1.4034 * exp(-T/26.815))^-1
}
