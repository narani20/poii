#' polyp_model
#' Description: select proper distribution based on AIC, and present a summary of distribution
#'
#' @param data Input data
#' @param group Group
#' @param Y Numeric variable
#' @param value Numeric variable
#'
#' @importFrom stats glm
#' @importFrom MASS glm.nb
#'
#' @export
#'
#' @examples polyp_model(data = poly, group = "trt", Y = "number", value = "age")
#'
polyp_model = function(data, group, Y, value) {
  group <- data[group][[1]]
  Y <- data[Y][[1]]
  value <- data[value][[1]]
  m0 <- glm(Y ~ value*group, data = data, family=poisson)
  m0.aic <- m0$aic

  m1 <- glm(Y ~ value+group, data = data, family=poisson)
  m1.aic <- m1$aic

  m2 <- glm(Y ~ value+group, data = data, family=quasipoisson)
  m2.aic <- m2$aic

  m3 <- glm.nb(Y ~ value+group, data = data)
  m3.aic <- m3$aic

  aic <- data.frame('distribution'=c('poisson','adjusted_poisson', 'quasipoisson', 'nb'),
                    'AIC'=c(m0.aic, m1.aic, m2.aic, m3.aic))
  min_aic <- aic[c(order(aic$AIC)),]
  rownames(min_aic) <- NULL



  aic2 <- data.frame(m0.aic, m1.aic, m2.aic, m3.aic)

  if (which.min(aic2) == 4) {
    return(list(aic_list=min_aic, select="nb",final_model=m3))

  } else if (which.min(aic2) == 3) {
    return(list(aic_list=min_aic, select="quasipoisson",final_model=m2))

  } else if (which.min(aic2) == 2) {
    return(list(aic_list=min_aic, select="adjusted_poisson",final_model=m1))

  } else if (which.min(aic2) == 1) {
    return(list(aic_list=min_aic, select="poisson",final_model=m0))
  }
}
