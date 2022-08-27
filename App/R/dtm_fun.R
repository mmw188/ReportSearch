############################################################################
##Project: RAND Report Search Tool
##Code: Add search terms to DTM
##Date: 10 Mar 2021
##Author: Matt Walsh (mmw188@gmail.com)
############################################################################

dtm.fun <- function(df.sub, add.stop.words){
  
  df.sub <- df.sub %>%
    mutate(entry = gsub('-', '', entry))
  
  dtm <- CreateDtm(doc_vec             = df.sub$entry, # character vector of documents
                   doc_names           = df.sub$topic, # document names
                   ngram_window        = c(1, 1), # minimum and maximum n-gram length
                   stopword_vec        = c(stopwords::stopwords('en'), # stopwords from tm
                                           stopwords::stopwords(source = 'smart')), # this is the default value
                   lower               = TRUE, # lowercase - this is the default value
                   remove_punctuation  = TRUE, # punctuation - this is the default
                   remove_numbers      = FALSE, # numbers - this is the default
                   verbose             = FALSE, # Turn off status bar for this demo
                   stem_lemma_function = function(x) SnowballC::wordStem(x, 'porter'),
                   cpus                = 2)
  
  return(dtm)
}