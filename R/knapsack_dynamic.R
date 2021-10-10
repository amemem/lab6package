knapsack_dynamic = function(x, W, fast = FALSE) {
  stopifnot(
    is.data.frame(x),
    colnames(x) %in% c("v", "w"),
    all(x > 0),
    is.numeric(W),
    W > 0
  )
  n = length(x[["v"]])
  m = matrix(0, n + 1, W + 1)
  if (!fast) {
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
  }
  else {
    Rcpp::cppFunction('NumericMatrix dp(int n, int W,
                                        NumericVector w,
                                        NumericVector v)
    {
      NumericMatrix m(n+1, W+1);
      for (int i = 1; i <= n; ++i) {
        for (int j = 1; j <= W; ++j) {
          if (w[i-1] <= j) {
            m(i, j) = std::max(m(i-1, j-w[i-1]) + v[i-1], m(i-1, j));
          }
          else {
            m(i, j) = m(i-1, j);
          }
        }
      }
      return m;
    }')
    m = dp(n, W, x$w, x$v)
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
