knapsack_dynamic = function(x, W) {
  stopifnot(
    is.data.frame(x),
    colnames(x) %in% c("v", "w"),
    all(x > 0),
    is.numeric(W),
    W > 0
  )
  n = length(x[["v"]])
  m = matrix(0, n + 1, W + 1)
  for (i in 1:n) {
    for (j in 1:W) {
      if (x[i, "w"] <= j) {
        m[i+1, j+1] = max(m[i, j+1-x[i, "w"]] + x[i, "v"], m[i, j+1])
      }
      else {
        m[i+1, j+1] = m[i, j+1]
      }
    }
  }
  elements = vector("numeric", n)
  value = m[n+1, W+1]
  c = W + 1
  for (i in n:1) {
    if (value != m[i, c]) {
      elements[i] = i
      value = value - x[i, "v"]
      c = which(m[i,] == value)[1]
    }
  }
  result = list(
    "value" = m[n+1, W+1],
    "elements" = elements[elements > 0]
  )
  return(result)
}
