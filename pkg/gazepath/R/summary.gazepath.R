summary.gazepath <-
function(object, ...){
  output <- numeric()
  for(i in 1:length(object[[16]])){
    sim <- object[[16]][[i]]
    l <- length(which(sim[,1] == 'f'))
    if(l != 0){
      output <- rbind(output, cbind(sim[sim[,1] == 'f', c(2, 9:11)], 1:l, i))
    }
  }
  names(output)[c(1, 5:6)] <- c('duration', 'order', 'trial')
  row.names(output) <- 1:dim(output)[1]
  return(output)
}
