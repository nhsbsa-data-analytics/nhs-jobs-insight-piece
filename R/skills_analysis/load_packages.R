# Section Start: Loading packages ----

# Install pacman if necessary
if (!('pacman' %in% rownames(installed.packages()))) {
  install.packages('pacman')
}

# Libraries
pacman::p_load(
  #Packages to load in the data
  'readxl',
  'xlsx',
  
  
  #Packages to manipulate the data
  
  'dplyr',
  'dbplyr',
  'tidyverse',
  'data.table',
  'lubridate',
  'factoextra',
  'magrittr',
  'stringi',
  
  
  #Packages to visualise the data
  'ggplot2',
  'plotly',
  
  
  
  #Packages for the quarto document
  'crosstalk',
  'htmltools',
  'remotes',
  'here',
  'downloadthis',
  'DT',
  
  #Packages for data testing and unit testing
  'assertr',
  'testthat'
  

)


#remotes::install_version("knitr", version = "1.41")

# Libraries from github
pacman::p_load_gh("nhsbsa-data-analytics/nhsbsaR")

# Section End: Loading packages ----