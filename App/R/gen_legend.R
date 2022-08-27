############################################################################
##Project: RAND Report Search Tool
##Code: Generate legend for network plot
##Date: 10 Mar 2021
##Author: Matt Walsh (mmw188@gmail.com)
############################################################################

gen.legend <- function(df, group.vars){
  
  colors_fun <- colorRampPalette(brewer.pal(9, "Spectral"))
  colors_fun <- data.frame(color = colors_fun(length(group.vars)),
                           Program = group.vars,
                           stringsAsFactors = FALSE,
                           posY = seq(from = 1, to = 2, length.out = length(group.vars)),
                           posX = .6) %>%
    mutate(Program = case_when(Program == 'Resource Management Program'        ~ 'Resource\nManagement',
                               Program == 'Strategy and Doctrine Program'      ~ 'Strategy and\nDoctrine',
                               Program == 'Workforce, Development, and Health' ~ 'Workforce,\nDevelopment,\nand Health',
                               Program == 'RAND Project AIR FORCE, Division'   ~ 'RAND PAF,\nDivision',
                               Program == 'Force Modernization and Employment' ~ 'Force\nModernization\nand Employment'))
  
  
  ggplot(colors_fun, aes(x = posX, y = posY, label = Program)) +
    geom_point(size = 10, shape = 21, color = 'black', fill = colors_fun$color) +
    geom_text(nudge_y = -.075, size = 4) +
    scale_y_continuous(limits = c(.9, 2.1)) +
    scale_x_continuous(limits = c(.4, .8)) +
    scale_fill_manual(values = colors_fun$color) +
    theme(panel.background      = element_rect(fill = "transparent"), # bg of the panel
          plot.background       = element_rect(fill = "transparent", color = NA), # bg of the plot
          panel.grid.major      = element_blank(), # get rid of major grid
          panel.grid.minor      = element_blank(), # get rid of minor grid
          panel.border          = element_rect(colour = "black", fill = NA, size = 3),
          legend.background     = element_rect(fill = "transparent"), # get rid of legend bg
          legend.box.background = element_rect(fill = "transparent"),
          axis.title.x          = element_blank(),
          axis.text.x           = element_blank(),
          axis.ticks.y          = element_blank(),
          axis.title.y          = element_blank(),
          axis.text.y           = element_blank(),
          axis.ticks.x          = element_blank(),
          legend.position = 'none')
}