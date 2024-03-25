##########
# PACKS ##
##########

# 1. Function --------------------------------------------------------------



ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}

# usage
packages <- c("plyr", "reshape2", "RColorBrewer", "scales", "grid",
              "dplyr","tidyr","magrittr","readr","readxl","openxlsx","ggplot2",
              "stringr","lubridate","sidrar","rbcb","janitor","tidyverse","ggplotly",
              "tibble")

ipak(packages)


