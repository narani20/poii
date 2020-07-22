#' Poisson model summary with estimated dispersion parameter
#'
#' @param data Input data
#' @param group Group variable in the data
#' @param Y Numeric variable in the data
#' @param value Numeric variable in the data
#'
#' @importFrom stats glm
#' @export
#'
#' @examples polyp_estdp(data=poly, group="trt", Y="number", value="age")
#'
polyp_estdp <- function(data, group, Y, value){
  group <- data[group][[1]]
  Y <- data[Y][[1]]
  value <- data[value][[1]]
  mod <- glm(Y ~ value + group, family = poisson, data = data)
  est <- sum(resid(mod, type="pearson")^2/(mod$df.residual))
  final <- list(summary(mod), est)
  names(final) <- c("Summary of Poisson model", "Estimated dispersion parameter")
  return(final)
}
