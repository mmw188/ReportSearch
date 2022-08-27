############################################################################
##Project: RAND Report Search Tool
##Code: Generate interactive network
##Date: 10 Mar 2021
##Author: Matt Walsh (mmw188@gmail.com)
############################################################################

plot.network <- function(entry, df.source, df.target, group.vars){
  
  df <- df.target
  
  sem_nodes <- left_join(df$plot_network$p1$x$nodes, df.target$df.sub, by = c('id' = 'topic'))
  
  sem_nodes <- sem_nodes  %>%
    mutate(title.raw = title,
           title = format.entry(internal.url, title, disp.text, Program, Year)) %>%
    select(id, title, title.raw, Program, x, y)

  colors_fun <- colorRampPalette(brewer.pal(9, "Spectral"))
  colors_fun <- data.frame(color = colors_fun(length(group.vars)),
                           Program = group.vars,
                           stringsAsFactors = FALSE)
  
  sem_nodes <- left_join(sem_nodes, colors_fun, by = 'Program')
  
  ## 2.0 Prepare some inputs for network plot
  sem_edges <- df$plot_network$p1$x$edges

  ## In case that only a subset of the full network is displayed
  new.dist <- convert.source(entry, df.source, df.target)
  
  if (!is.na(new.dist$Difference[1])){
    new.dist <- new.dist$Difference
    v.val    <- data.frame(id = rownames(df.target$lsa_out$pred),
                           new.dist = new.dist,
                           stringsAsFactors = FALSE)
    
    v.val <- v.val %>% arrange(new.dist)
    v.val$new.dist <- c(seq(from = 0, to = 1, by = .05)^.75, rep(1, nrow(v.val)-21))
    
  } else {
    new.dist <- 0
    v.val   <- data.frame(id = rownames(df.target$lsa_out$pred),
                          new.dist = 0,
                          stringsAsFactors = FALSE)
  }
  

  sem_nodes <- left_join(sem_nodes, v.val, by = 'id') %>%
    mutate(value   = 3  - 2*new.dist,
           opacity = 1 - .6*new.dist,
           borderWidth = 1.5 - new.dist) %>%
    arrange(desc(new.dist))
  
  coords    <- sem_nodes[, c('x', 'y')] ## Pre-computed x, y coordinates for each node
  
  ## 6.0 Render network
  ## 6.1 Accent nodes linked to search inputs

  sem_edges <- left_join(sem_edges, sem_nodes[, c('id', 'new.dist')], by = c('from' = 'id')) %>%
    left_join(sem_nodes[, c('id', 'new.dist')], by = c('to' = 'id')) %>%
    mutate(color.opacity = .25+.75*(1-new.dist.x*new.dist.y))
  
  p1 <- visNetwork(sem_nodes, sem_edges, boarder = 'gray') %>%
    visIgraphLayout(layout = 'layout.norm', layoutMatrix = as.matrix(coords)) %>%
    visNodes(physics = FALSE,
             font = list(size = 0),
             labelHighlightBold = TRUE,
             scaling = list(min = 20, max = 50)) %>%
    visOptions(highlightNearest = list(enabled = TRUE, 
                                       algorithm = 'hierarchical', 
                                       degree = 1),
               nodesIdSelection = FALSE
               ) %>%
    visInteraction(multiselect = TRUE, 
                   hover = TRUE,
                   selectConnectedEdges = FALSE,
                   dragNodes = FALSE,
                   tooltipStyle = 'position: fixed;visibility:hidden;padding: 5px;font-family: verdana;
                   font-size:14px;font-color:#000000;background-color: #f5f4ed;-moz-border-radius: 3px;*
                   -webkit-border-radius: 3px;border-radius: 3px; border: 1px solid #808074;
                   box-shadow: 3px 3px 10px rgba(0, 0, 0, 0.2); max-width:600px; wrap')
    
  return(p1)
}