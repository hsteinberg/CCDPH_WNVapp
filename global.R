#Loading required packages
library(shiny)
library(dplyr)
library(ggplot2)
library(sp)
library(leaflet)
library(MMWRweek)
library(htmltools)
library(DT)
library(plotly)
library(waffle)
library(tidyr)
library(tidyverse)
library(tmap)
library(magrittr)
library(waffle)
library(RColorBrewer)
library(qdap)
#library(Cairo)

load("WNV.RData")

color_pal = c("#06aed5", "#086788", "#f0c808","#4c212a", "#dd1c1a")
greys = c("#b2b2b2", "#898989","#777777","#606060","#2d2d2d")

simpleCap <- function(x) {
  sapply(x, function(x){
    s <- tolower(x)
    s <- strsplit(s, " ")[[1]]
    paste(toupper(substring(s, 1,1)), substring(s, 2),
          sep="", collapse=" ")
  })
}






