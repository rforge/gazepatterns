precision <-
function(sim){
  if(length(sim[sim[,1] == 'f', 11]) == 0){
    return(NA)
  } else {
    return(mean(sim[sim[,1] == 'f', 11]))
  }
}
