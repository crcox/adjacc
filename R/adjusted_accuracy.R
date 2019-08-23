#' Compute adjusted accuracy given true and predicted labels.
#'
#' @param y true labels.
#' @param p predicted labels.
#' @return adjusted accuracy (HR + CR / 2)
#' @examples
#' y <- c(1,1,1,1,0,0,0,0,0,0,0,0,0)
#' p <- c(0,1,1,0,0,1,0,0,0,0,0,0,0)
#' mean(y == p) # accuracy
#' adjusted_accuracy(y, p)
#' y <- c(1,1,1,1,0,0,0,0,0,0,0,0,0)
#' p <- c(0,0,0,0,0,0,0,0,0,0,0,0,0)
#' mean(y == p) # accuracy
#' adjusted_accuracy(y, p)
adjusted_accuracy <- function(y,p) {
    y <- y > 0
    p <- p > 0
    HR <- sum(y & p) / sum(y)
    CR <- sum(!y & !p) / sum(!y)
    return( (HR + CR) / 2 )
}
