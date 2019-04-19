library(shiny)
library(shinydashboard)
library(DT)
library(plotly)
library(forcats)
library(tidyverse)
library(feather)
library(lubridate)
library(plotly)
library(DT)
library(kableExtra)
library(ggthemes)
library(rlang)
library(PythonEmbedInR)
library(synapser)
# devtools::install_github("Sage-Bionetworks/syndccutils", subdir = "R")
# library(syndccutils)
library(stringr)
library(yaml)
library(pier)

config = yaml::yaml.load_file("configuration.yaml")

consortium_donut <- function(consortium_counts, key, key_label) {
  ntap_consortium_counts %>% 
  select(label = consortium, value = key) %>% 
    mutate(color = c("#999999","#E69F00", "#56B4E9", "#009E73", "#0072B2", "#D55E00", "#CC79A7") ) %>% 
    pier() %>% 
    pie.size(inner = 75, outer = 90, height = 100) %>% 
    pie.header(text = key_label, 
               size = 13,
               font='Lato', 
               location='pie-center') %>% 
    pie.labels(outer = list(format = 'value', pieDistance = 10), 
               percentage = list(fontSize = 0), 
               value = list(fontSize = 12, 
                            font = 'Lato', 
                            color = "#3C4A63")) 
}
