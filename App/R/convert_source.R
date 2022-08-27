############################################################################
##Project: RAND Report Search Tool
##Code: Return semantic distance between search inputs and records in searchable data base
##Date: 10 Mar 2021
##Author: Matt Walsh (mmw188@gmail.com)
############################################################################

convert.source <- function(entry, df.source, df.target){
  
  new.dist <- c()
  comp.lab <- c()
  
  if (!is.na(entry$entry[1])) { ##Calculate distance from search term to records in searchable dataset
    entry.sub <- data.frame(entry = entry$entry,
                            topic = NA_character_,
                            clust = NA_integer_)
    
    new.dist <- add.search(entry.sub, df.source, df.target)
    comp.lab <- entry$entry
    
  } else if (!is.na(entry$topic[1])){ ##Calculate distance from existing entry to records in searchable dataset

    id <- which(df.source$df.sub$topic %in% entry$topic)
    if (length(id) > 0){
      ##topic is the names of the entries
      ##entry is the text of the entries
      entry.sub <- data.frame(entry = df.source$df.sub$entry[id],
                              topic = df.source$df.sub$topic[id],
                              clust = NA_integer_)
  
      new.dist <- add.search(entry.sub, df.source, df.target)
      comp.lab <- paste(entry$topic, collapse = '; ')
    }
  } else if (!is.na(entry$clust[1])){ ##Calculate distance from existing cluster to records in searchable dataset

    id <- which(df.source$df.sub$`Record Cluster` %in% entry$clust)
    if (length(id) > 0){
      ##topic is the names of the entries
      ##entry is the text of the entries
      
      entry.sub <- data.frame(entry = df.source$df.sub$entry[id],
                              topic = df.source$df.sub$topic[id],
                              clust = NA_integer_)
      
      new.dist <- add.search(entry.sub, df.source, df.target)
      comp.lab <- paste(entry$clust, collapse = '; ')
    }
  }
  
  if (length(new.dist) == 0){new.dist <- NaN}
  if (length(comp.lab) == 0){comp.lab <- 'Comparison'}
 
  temp.df <- data.frame(Difference = new.dist,
                        Title      = rownames(df.target$lsa_out$pred),
                        Comparison = comp.lab)
  
  return(temp.df)
}