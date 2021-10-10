#' Knapsack Brute Force
#'
#' A brute force (or exhaustive search) solution to the knapsack problem.
#'
#' @param x Data frame containing values \code{v} and weights \code{w} of each item.
#' @param W Maximum capacity of the knapsack.
#' @param parallel (Optional) If the main loop should be run in parallel
#'
#' @return List containing the maximum value and the indices of the items.
#' @importFrom parallel detectCores makeCluster parLapply stopCluster
#' @export
knapsack_brute_force = function(x, W, parallel = FALSE) {
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
  if (!parallel) {
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
  }
  else {
    cores = parallel::detectCores()
    cl = parallel::makeCluster(cores, "PSOCK")
    parL = parallel::parLapply(cl, 1:(2^n-1), function(i) {
      index = as.integer(intToBits(i)) * 1:32
      if (sum(x[index, "w"]) <= W) {
        return(list("value" = sum(x[index, "v"]),
                    "elements" = index[index > 0]))
      }
    })
    parallel::stopCluster(cl)
    for (i in 1:length(parL)) {
      v = parL[[i]]$value
      if (!is.null(v) && v > value) {
        value = v
        elements = parL[[i]]$elements
      }
    }
  }
  result = list(
    "value" = value,
    "elements" = elements
  )
  return(result)
}
