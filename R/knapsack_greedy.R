knapsack_greedy = function(x, W) {
  stopifnot(
    is.data.frame(x),
    colnames(x) %in% c("v", "w"),
    all(x > 0),
    is.numeric(W),
    W > 0
  )
  n = length(x[["v"]])
  x$p = x[["v"]] / x[["w"]]
  x = x[order(x$p, decreasing = TRUE),]
  value = 0
  weight = 0
  elements = vector("numeric", n)
  for (i in 1:n) {
    tmp_w = x[i, "w"]
    if (weight + tmp_w <= W) {
      weight = weight + tmp_w
      value = value + x[i, "v"]
      elements[i] = as.integer(rownames(x)[i])
    }
  }
  result = list(
    "value" = value,
    "elements" = elements[elements > 0]
  )
  return(result)
}
