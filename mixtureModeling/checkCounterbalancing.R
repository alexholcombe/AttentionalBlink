checkAllGroupsOccurEquallyOften<- function(df,colNames,dropZeros=FALSE,verbose=FALSE) {
  #in data.frame df, check whether the factors in the list colNames reflect full factorial design (all combinations of levels occur equally often)
  #
  #dropZeros is useful if one of the factors nested in the others. E.g. testing different speeds for each level of    
  # something else, then a lot of the combos will occur 0 times because that speed not exist for that level.
  #but it's dangerous to dropZeros because it won't pick up on 0's that occur for the wrong reason- not fully crossed
  #
  #Returns:
  # true/false, and prints informational message
  #
  listOfCols <- as.list( df[colNames] )
  t<- table(listOfCols)
  
  if (dropZeros) {  
    t<- t[t!=0]   
  }           
  colNamesStr <- paste(colNames,collapse=",")
  if ( length(unique(t)) == 1 ) { #if fully crossed, all entries in table should be identical (all combinations occur equally often)
    print(paste(colNamesStr,"fully crossed- each combination occurred",unique(t)[1],'times'))
    ans <- TRUE
  } else {
    print(paste(colNamesStr,"NOT fully crossed,",length(unique(t)),'distinct repetition numbers.'  ))
    ans <- FALSE
    if (verbose) {
      print(t)
    }
  } 
  return(ans)
}