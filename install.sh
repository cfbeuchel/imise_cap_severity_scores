#!/bin/bash

R -q -e "install.packages(c('shinyjs'), quiet = TRUE)"
R -q -e "install.packages(c('DT'), quiet = TRUE)"
R -q -e "install.packages(c('readxl'), quiet = TRUE)"
R -q -e "install.packages(c('data.table'), quiet = TRUE)"
R -q -e "install.packages(c('magrittr'), quiet = TRUE)"
R -q -e "install.packages(c('shinydashboard'), quiet = TRUE)"
R -q -e "devtools::install_github('maciejrosolowski/progressdatenbankderivate')"

