############################################################################
##Project: RAND Report Search Tool
##Code: Search databse from records or search terms
##Date: 10 Mar 2021
##Author: Matt Walsh (mmw188@gmail.com)
############################################################################

search.data <- function(entry, df.source, df.target){

  results <- convert.source(entry, df.source, df.target)
  
  ## 2.2. Add display text (description for hover-over in table)
  ret.cols <- c('Year', 'Program', 'internal.url') ## PACAF_AC
                
  extra.cols <- intersect(ret.cols, colnames(df.target$df.sub))
  results <- left_join(results, df.target$df.sub[c('topic', 'disp.text', extra.cols)], by = c('Title' = 'topic'))
  results <- results[, c(ncol(results), 1:(ncol(results)-1))]
  
  ## 2.3 Order by semantic similarity
  results <- results %>%
    arrange(`Difference`) %>%
    mutate(Report = gsub(".*_", "", Title),
           Title  = sub("_[^_]+$", "", Title))
  
  rownames(results) <- NULL
  
  sim_vec <- results$Difference[results$Difference > 0]
  cutoff  <- min(sim_vec) + .5*(median(sim_vec) - min(sim_vec))
  
  results <- filter(results, Difference <= cutoff)

  if (is.na(results$`Difference`[1])){
    results <- results[0, ]
  }
  
  raw.results <- results[, c('Title', 'Program', 'Year', 'internal.url', 'disp.text')] %>% rename(URL = internal.url, Summary = disp.text) %>% mutate_if(is.factor, as.character)
  
  results <- results[, c('disp.text', 'Title', 'Report', extra.cols)] %>% mutate_if(is.factor, as.character) %>%
    mutate(Report = format.entry(internal.url, Title, disp.text, Program, Year)) %>%
    select(Report, Year, Program)

  return(list(results = results, raw.results = raw.results)) 
}