############################################################################
##Project: RAND Report Search Tool
##Code: Flag search terms not contained n database
##Date: 10 Mar 2021
##Author: Matt Walsh (mmw188@gmail.com)
############################################################################

missing.terms <- function(entry, df.target){
  
  if (entry == ''){
    output <- ''
    
  } else {
    entry <- gsub('-', '', entry)
    
    ## Get words from target data base
    add.stop.words <- df.target$add.stop.words
    dtm            <- df.target$dtm
    
    missing.terms <- lapply(unlist(strsplit(entry, ' ')),
                            function(x){
                              new.entry <- data.frame(entry = x, topic = NA_character_)
                              dtm_2 <- dtm.fun(new.entry, add.stop.words)
                              missing.terms <- setdiff(colnames(dtm_2), colnames(dtm))
                              
                              if (length(missing.terms) > 0){return(x)}
                            })
    
    missing.terms <- paste(unlist(missing.terms), collapse = ', ')

    if (missing.terms != ''){
      output <- paste('Search terms not included in data set:', missing.terms, sep = ' ')
    } else {
      output <- ''
    }
  }
  
  return(output)
}