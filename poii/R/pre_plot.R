#' Predicted values plot about final model
#'
#' @param data Input data
#' @param group Group
#' @param Y Numeric variable
#' @param value Numeric variable
#'
#' @importFrom MASS glm.nb
#' @importFrom ggplot2 ggplot
#' @export
#'
#' @examples pre_plot(poly,"trt","number","age")
#'
pre_plot <- function(data,group,Y,value) {
  fit<-polyp_model(data,group,Y,value)

  Y <- data[Y][[1]]
  value <- data[value][[1]]
  group <- data[group][[1]]

  data$fit.pred <- fit$final_model$fitted.values

  ggplot(data,aes(x=value,y=fit.pred,col=group))+
      geom_line(aes(colour=group),size=1.3)+
        labs(title=as.character(fit$select),x="number", y="predicted values")

}


