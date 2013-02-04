output=function(y,treshold=.2){
  y=y[order(y[,4],decreasing=TRUE),]
  result=paste((ifelse(y[,4]>treshold,y[,1],'')),collapse=' ')
  
  if(nchar(result)==0)
    result=' '
  result
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
  if(length(actual)==0)
    return(0)
  for (i in 1:min(k,length(predicted)))
  {
    if (predicted[i] %in% actual && !(predicted[i] %in% predicted[0:(i-1)]))
    {
      cnt <- cnt + 1
      score <- score + cnt/i 
      
    }
  }
  score <- score / min(length(actual), k)
  
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