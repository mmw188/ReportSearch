############################################################################
##Project: RAND Report Search Tool
##Code: Put search terms into same semantic space as searchable data set
##Date: 10 Mar 2021
##Author: Matt Walsh (mmw188@gmail.com)
############################################################################

add.search <- function(new.entry, df.source, df.target){
  
  ## Previously, we computed a dtm, tf-idf matrix, and LSA model for the target data set. This is the "target semantic space"
  ## Now, we will use those elements to place the user search inputs into the same semantic space
  add.stop.words <- df.target$add.stop.words
  dtm            <- df.target$dtm
  tf_mat         <- df.target$tf_mat
  mod            <- df.target$lsa_out
  
  ## Apply document term matrix parameters to new search
  dtm_2 <- dtm.fun(new.entry, add.stop.words)
  
  ## Use previous document term matrix as template for new search
  dtm_vec <- dtm[rep(1, length(new.entry$entry)), , drop = FALSE]*0
  id      <- which(colnames(dtm_2) %in% colnames(dtm))
  dtm_2   <- dtm_2[, id, drop = FALSE]
  dtm_vec[, match(colnames(dtm_2), colnames(dtm))] <- dtm_2
  
  ## Apply previous tfidf matrix to new search
  tfidf_vec <- t(dtm_vec[, tf_mat$term, drop = FALSE])*tf_mat$idf
  tfidf_vec <- t(tfidf_vec)
  
  ## Apply previous LSA model to new search
  pred2  <- predict(mod$model, tfidf_vec)
  rownames(pred2) <- new.entry$topic
  
  ## Calculate cosine distance from search input and all records in the target data base
  ## If the user entered a search phrase, the distance matrix will have one row.
  ## If the user selected two or more existing records, the distance matrix will have multiple rows
  dist.input <- expand.grid(x1 = 1:nrow(mod$pred), x2 = 1:nrow(pred2))
  new.dist <- unlist(lapply(1:nrow(dist.input),
                            function(x) 1-cosine(as.numeric(mod$pred[dist.input$x1[x],]), as.numeric(pred2[dist.input$x2[x],]))))
  
  new.dist <- matrix(unlist(new.dist), nrow = nrow(pred2), byrow = TRUE)

  new.dist <- colMeans(new.dist) ## If the distance matrix has multiple rows, convert to average distance from search inputs
  return(new.dist)
}