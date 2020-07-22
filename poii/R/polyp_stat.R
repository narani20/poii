#' polyp_stat
#'
#' @param data Input data
#' @param group Group variable in the data
#' @param value Numeric variable in the data
#'
#' @importFrom dplyr summarise
#'
#' @export
#'
#' @examples polyp_stat(poly, "number")
polyp_stat=function(data, value)
{
  data %>%
    dplyr::summarise(N=n(),
                     Mean = mean(get(value)),
                     Max = max(get(value)),
                     Min = min(get(value)),
                     IQR = IQR(get(value)),
                     Var = var(get(value)),
                     Sd = sd(get(value)))
}
