############################################################################
##Project: RAND Report Search Tool
##Code: Formt entries to be displayed
##Date: 10 Mar 2021
##Author: Matt Walsh (mmw188@gmail.com)
############################################################################

format.entry <- function(internal.url, Title, disp.text, Program, Year){
  
  entry <- paste('<b>',
                 paste("<a href='", internal.url, "' target='_blank'>", Title, "</a>", sep =),
                 '</b>',
                 '<br>',
                 disp.text,
                 '<br>',
                 '<span style="color: #228B22;">',
                 paste('Program: ', Program, sep = ''),
                 "<br>",
                 paste('Year: ', Year, sep = ''),
                 '</span>', sep = '')

  return(entry)
}