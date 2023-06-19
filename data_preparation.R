winsorize <- function(x, cut) {
  x <- replace(
    x,
    x > quantile(x, 1 - cut, na.rm = T),
    quantile(x, 1 - cut, na.rm = T)
  )
  x <- replace(
    x,
    x < quantile(x, cut, na.rm = T),
    quantile(x, cut, na.rm = T)
  )
  return(x)
}
