############################################################################
##Project: RAND Report Search Tool
##Code: Define global workspace
##Date: 10 Mar 2021
##Author: Matt Walsh (mmw188@gmail.com)
############################################################################

rm(list = ls())

# 1. Load packages
library(dplyr)
library(tidyr)
library(lsa)
library(textmineR)
library(RColorBrewer)
library(visNetwork)
library(igraph)
library(DT)
library(shinydashboard)
library(shiny)
library(shinyjs)
library(shinyWidgets)
library(shinyhelper)
library(data.table)
library(ggplot2)
library(textreadr)
library(xlsx)

# 1. Define directories
input_dir <- 'input'
code_dir  <- 'R'

# 2. Load functions
file.sources = paste(code_dir, list.files(code_dir, pattern = '*.R'), sep = '/')
invisible(sapply(file.sources, source, .GlobalEnv))

fname.source <- file.path(input_dir, paste('RAND_PAF', 'rds', sep = '.'))
fname.target <- file.path(input_dir, paste('RAND_PAF', 'rds', sep = '.'))

df.source <- readRDS(fname.source)
df.target <- df.source

group.vars <- unique(df.target$df.sub$Program)
legend.plt  <- gen.legend(df.source, group.vars)

topic.list <- as.data.table(sort(df.source$df.sub$topic))
names(topic.list) <- 'Select one'

program.list <- as.data.table(sort(unique(df.source$df.sub$Program)))
names(program.list) <- 'Select one'

analysis.vals <- reactiveValues()
analysis.vals$net.view  <- 'full'
analysis.vals$df.source <- df.source
analysis.vals$df.target <- df.target
analysis.vals$df.user <- NA_character_
#########################################################################################################################
#########################################################################################################################
#########################################################################################################################
#########################################################################################################################
#########################################################################################################################