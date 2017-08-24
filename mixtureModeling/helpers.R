roundDataframe<- function(df,precision) {
  
  df<- data.frame(lapply(df, function(y) if(is.numeric(y)) round(y, 2) else y)) #round
  return(df)
}