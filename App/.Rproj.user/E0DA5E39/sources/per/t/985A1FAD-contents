multi <- function(n.seq, thresholds){
  thresh <- c(as.numeric(thresholds$Value))
  combos <- lapply(n.seq,
               function(x) diff(c(0, exp(thresh-x)/(1+exp(thresh-x)), 1)))
  combos <- t(do.call("cbind", combos))
  colnames(combos) <- paste('Var', c(0:5))
  rownames(combos) <- n.seq
  return(combos)
}

gen.prior <- function(post, df.sub, wts){
  df.obs <- unlist(lapply(0:5,
                          function(x) sum(sum(filter(df.sub, Score == x)$obs))))
  
  prob <- as.matrix(post) %*% df.obs ##p(x|y)
  prob <- prob - logSumExp(prob) ##normalize
  prob <- log((exp(prob)*wts + rep(1/length(prob), length(prob))*(1-wts))) ##convolve with uniform
  prob <- list(prob)
  return(prob)
}

calc.collapse.u <- function(p.prior, combos, post, bns){
  p.out   <- combos
  p.post  <- as.numeric(p.prior) + post ##log likelihood of outcomes p(y|x)*p(x)
  
  norm    <- unlist(lapply(1:6,
                           function(x) logSumExp(p.post[, x]))) ##p(y)
  
  p.post <- sweep(p.post, 2, norm) ##update prior (p(y|x))p(x)/p(y) -- note, each column is per outcome
  
  c.post <- lapply(1:6,
                 function(x) apply(p.post[bns == x,], 2, logSumExp))
  
  c.post <- do.call("rbind", c.post)
  
  c.prior <- unlist(lapply(1:6,
                           function(x) logSumExp(p.prior[bns == x])))
  
  p.util <- as.numeric(colSums(exp(c.post)*c.post)) - sum(exp(c.prior)*c.prior) ##change in information (U) per outcome
  g.util <- exp(norm)*p.util
  g.util <- sum(g.util)
  return(g.util)
}

plot.multinomial <- function(combos){
  
  df <- data.frame(`Latent Value` = as.numeric(rownames(combos)), as.data.frame(combos))
  
  df <- gather(df, key = 'Rating', value = 'Probability', colnames(df)[grepl('Var.', colnames(df))]) %>%
    mutate(Rating = case_when(Rating == 'Var.0' ~ 'One',
                              Rating == 'Var.1' ~ 'Two',
                              Rating == 'Var.2' ~ 'Three',
                              Rating == 'Var.3' ~ 'Four',
                              Rating == 'Var.4' ~ 'Five',
                              Rating == 'Var.5' ~ 'Six'))
  
  df$Rating <- factor(df$Rating, levels = c('One', 'Two', 'Three', 'Four', 'Five', 'Six'))
  
  ggplot(df, aes(x = Rating, y = Latent.Value, fill = Probability)) +
    geom_tile(alpha = .5) +
    ggtitle('Multinomial Model') +
    scale_x_discrete(name = 'Assigned Scores', expand = c(0, 0)) +
    scale_y_continuous(name = 'Latent Value', expand = c(0, 0), breaks = c(-4.5, -3.0, -1.5, 0, 1.5, 3.0, 4.5), labels = c('-4.5', '-3.0', '-1.5', '0.0', '1.5', '3.0', '4.5')) +
    scale_fill_viridis(limits = c(0, 1), labels = scales::percent) +
    theme_bw() +
    theme(plot.title = element_text(hjust = 0.5))
}

plot.priors <- function(p.vec, n.seq){
  df <- lapply(1:nrow(p.vec),
               function(x){
                 data.frame(Attribute = p.vec$Attribute[x],
                            Latent.Value = n.seq,
                            Probability = exp(p.vec$p[[x]]))
               }) %>%
    bind_rows()
  
  ggplot(df, aes(x = Latent.Value, y = Probability, color = Attribute)) +
    geom_line() +
    scale_x_continuous(name = 'Latent Value', limits = c(-2, 2), breaks = c(-2, -1, 0, 1, 2)) +
    scale_y_continuous(labels = scales::percent) +
    scale_color_brewer(palette = 'Set1') +
    theme_bw()
}

plot.final <- function(outcomes){
  outcomes <- outcomes %>% bind_rows() %>%
    group_by(sim.round, method) %>%
    summarise(Prob.Correct = mean(Prob.Correct))
  
  ggplot(outcomes, aes(x = sim.round, y = Prob.Correct, color = method)) +
    geom_line() +
    scale_x_continuous(name = 'Round') +
    scale_y_continuous(name = 'Correct Label (%)', labels = scales::percent, limits = c(.4, 1)) +
    scale_color_brewer(palette = 'Set1') +
    theme_bw()
}

bin.probs <- function(p.vec, bns){
  p.vec <- unlist(lapply(unique(bns),
                         function(x) logSumExp(unlist(p.vec)[bns == x])))
  
  list(p.vec)
}

run_ado <- function(p.est, combos, post, bns, s.met){
  nr     <- nrow(p.est)  ## Number of candidate-attribute pairs
  batch  <- 10           ## Number of samples to draw per iteration
  iters  <- 400          ## Number of iterations (total samples = batch*iters)

  grid.sample <- rep(1:nr, ceiling(iters*batch/nr))[c(1:(batch*iters))] ## Generate sample order for grid
  rand.sample <- sample(1:nr, batch*iters, replace = TRUE)              ## Generate sample order for random
  
  master <- c()
  
  ## For n rounds of sampling, with batch draws per round
  for (n in 1:iters){
    
    ## 1. Select samples
    ## 1.1. ADO
    if (s.met == 'ADO'){
      info.gain <- unlist(mclapply(1:nr,
                                   function(x) calc.collapse.u(p.est$p[[x]], combos, post, bns)))
      
      ## 1.1.1 In particular, select samples that maximize information gain
      selection <- sort(-info.gain, index.return = TRUE)$ix
      selection <- selection[1:10]
      
    ## 1.2 Random
    } else if (s.met == 'Random'){
      selection <- rand.sample[(n-1)*batch+c(1:batch)]
      
    ## 1.3 Grid
    } else if (s.met == 'Grid'){
      selection <- grid.sample[(n-1)*batch+c(1:batch)]
      
    }
    
    ## 2. Simulate Outcomes 
    ## For total number of outcomes in batch
    for (ct in 1:batch){
      id  <- selection[ct]
      
      ## 2.1 Simulate outcome
      outcome <- unlist(lapply(id,
                               function(x) min(which(runif(1) < p.est$draw[[x]]))))
      
      ## 2.2 Update posterior
      p.dist <- unlist(p.est$p[id]) + post[, outcome] ##p(x)*p(y|x)
      p.dist <- p.dist - logSumExp(p.dist) ##normalize
      
      ## 2.3 Book keeping
      p.est$p[id]     <- list(p.dist)
      p.est$p.bin[id] <- list(unlist(lapply(1:6,
                                            function(x) logSumExp(p.dist[bns == x]))))
    }
    
    ## 3.0 Store results
    temp <- p.est[c('Bin', 'p.bin')] %>% mutate(sim.round  = n)

    master[[n]] <- temp
  }
  
  master <- master %>% bind_rows()
  
  master <- cbind(master, data.frame(do.call(rbind, master$p.bin))) %>%
    mutate(p_correct = case_when(Bin == 1 ~ X1,
                                 Bin == 2 ~ X2,
                                 Bin == 3 ~ X3,
                                 Bin == 4 ~ X4,
                                 Bin == 5 ~ X5,
                                 Bin == 6 ~ X6)) %>%
    group_by(sim.round) %>%
    summarise(Prob.Correct = mean(exp(p_correct))) %>%
    mutate(method = s.met)
  
  return(master)
}
