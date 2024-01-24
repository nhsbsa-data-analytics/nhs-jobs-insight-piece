# Section Start: Loading packages ----

# Install pacman if necessary
if (!('pacman' %in% rownames(installed.packages()))) {
  install.packages('pacman')
}

# Libraries
pacman::p_load(
  #packages used for data manipulation
  'dplyr',
  'dbplyr',
  'readtext',
  'remotes',
  'tictoc',
  'parallel',
  'magrittr',
  'tidyverse',
  'data.table',
  'stringi',
  'stringr',
  'tidytext',
  'fuzzyjoin'
  
)


# Libraries from github
pacman::p_load_gh("nhsbsa-data-analytics/nhsbsaR")


# Install tabulizer if necessary
if (!('tabulizer' %in% rownames(installed.packages()))) {
  remotes::install_github(
    c("ropensci/tabulizerjars", "ropensci/tabulizer"),
    INSTALL_opts = "--no-multiarch",
    dependencies = c("Depends", "Imports")
  )
}

