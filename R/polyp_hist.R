#' poly_hist
#'
#' @param data Input data
#' @param group Group variable in the data
#' @param value Numeric variable in the data
#'
#' @importFrom ggplot2 ggplot
#' @export
#'
#' @examples polyp_hist(poly, "trt", "number")
polyp_hist=function(data,group,value)
{
  # group<-get(group)
  value<-data[value][[1]]
  ggplot(data, aes(x=value)) +
    geom_histogram(binwidth=5, color="darkblue", fill="lightblue") +
    theme(plot.title=element_text(size=20, face="bold")) +
    facet_grid(~ get(group)) +
    labs(title="histogram by TRT")
}
