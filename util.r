output=function(y,treshold=.2){
  y=y[order(y[,4],decreasing=TRUE),]
  rez=paste((ifelse(y[,4]>treshold,y[,1],'')),collapse=' ')
  
  if(nchar(rez)==0)
    rez=' '
  rez
}

#' Compute the average precision at k
#'
#' This function computes the average precision at k
#' between two sequences
#'
#' @param k max length of predicted sequence
#' @param actual ground truth set (vector)
#' @param predicted predicted sequence (vector)
#' @export
apk <- function(k, actual, predicted)
{
  score <- 0.0
  cnt <- 0.0
  len_pred = length(predicted)
  len_actual = length(actual)
  
  for (i in 1:min(k,len_pred))
  {
    
    if (predicted[i] %in% actual &&!(predicted[i] %in% predicted[0:(i-1)]))
    {
      cnt <- cnt + 1
      score <- score + cnt/i 
    }
  }
  score <- score / min(len_actual, k)
  
  score
}

#' Compute the mean average precision at k
#'
#' This function computes the mean average precision at k
#' of two lists of sequences.
#'
#' @param k max length of predicted sequence
#' @param actual list of ground truth sets (vectors)
#' @param predicted list of predicted sequences (vectors)
#' @export
mapk <- function (k, actual, predicted)
{
  scores <- rep(0, length(actual))
  for (i in 1:length(scores))
  {
    scores[i] <- apk(k, actual[[i]], predicted[[i]])
  }
  score <- mean(scores)
  score
}