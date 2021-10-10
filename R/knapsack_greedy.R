#' Knapsack Greedy
#'
#' A greedy approximation to the knapsack problem where the aim is to pack the knapsack with the highest value to weight ratio items until the capacity is filled. It runs in \code{O(n log(n) + n)} due to the sorting algorithm (where \code{n log(n)} is the sorting, and \code{n} is the packing of items into the knapsack), but will not necessarily return the maximum value. The maximum value returned is greater than that of the test suite, because it doesn't pack the bag by weight, rather it packs it by the highest value to weight ratio. This, however, does not interfere with the complexity of the solution, i.e. the packing of items is done in O(n) time as can be seen in the function.
#'
#' @param x Data frame containing values \code{v} and weights \code{w} of each item.
#' @param W Maximum capacity of the knapsack.
#'
#' @return List containing the maximum value and the indices of the items.
#' @export
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
