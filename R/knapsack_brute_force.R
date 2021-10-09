knapsack_brute_force = function(x, W) {
  stopifnot(
    is.data.frame(x),
    colnames(x) %in% c("v", "w"),
    all(x > 0),
    is.numeric(W),
    W > 0
  )
  n = length(x[["v"]])
  value = 0
  elements = c()
  for (i in 1:(2^n-1)) {
    index = as.integer(intToBits(i)) * 1:32
    if (sum(x[index, "w"]) <= W) {
      v = sum(x[index, "v"])
      if (v > value) {
        value = v
        elements = index[index > 0]
      }
    }
  }
  result = list(
    "value" = value,
    "elements" = elements
  )
  return(result)
}
