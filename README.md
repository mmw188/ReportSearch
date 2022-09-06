# 1.0 PAF Knowledge Map
A software application for exploring all publically-available RAND PAF reports published since 2000

# 2.0 Description
## PAF Knowledge Map Overview
The PAF Knowledge Map is a tool for exploring and retrieving reports about issues pertinent to the Department of the Air Force (DAF). The tool contains all unclassified, releasable RAND PAF reports published since 2000.

## Technical Approach
The PAF Knowledge Map reads all the words contained in each RAND PAF report. It applies a technique called latent semantic analysis (LSA) to the collections of words contained in reports. LSA discovers a set of underlying topics present across the complete set of reports. After LSA is applied, each document is represented as a mixture of topics, rather than as a collection of words.

## Use Cases
The Knowledge Map can be used for both targeted searches and exploratory searches. Example use cases include: (1) Finding RAND PAF reports on a particular topic in a specific time frame; (2) Seeking out reports related to a document of interest; (3) Discovering other reports related to one or a series of RAND PAF reports; and (4) Exploring clusters of related work.

# 3.0 Table of Contents
The app subdirectory contains an R Project file, App.Rproj, along with the main application files: global.R, server.R, and ui.R. The app call functions contained in the R subdirectory, and it reads in an .RDA object that contains outputs of the NLP pre-processing and modeling steps.

# 4.0 Running the Application
To run the application, select the R Project file, App.Rproj. If the file global.R does not automatically open, select it from the Files panel in the lower-right screen in RStudio. Once the file global.R is displayed, press the Run App button beside the green arrow.

This work was developed in R version 3.6.3 and uses several packages installed from CRAN:
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

# 5.0 Citations
Landauer, T. K., Foltz, P. W., & Laham, D. (1998). An introduction to latent semantic analysis. Discourse processes, 25(2-3), 259-284.
