Easy Barplots
==========
[![License: GPL v3](https://img.shields.io/badge/License-GPLv3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)

Easy and customizable barplots, including statistical analysis.

# ShinyApp:
https://fuchsf.shinyapps.io/easy_barplots_ffw/

# System Requirements
The App can be accessed online using any of the most popular browsers. To run it locally, I recommend using R Studio (https://www.rstudio.org) - see their website for requirements-  

### R Dependencies
Easy Barplots relies on the following packages: 

packages = c("shiny", "shinydashboard", "DT", "tidyverse","shinyBS",
             "ggpubr","shinythemes", "readxl", "shinyjqui")
             
To install & load all of them at once:

package.check <- lapply(
  packages,
  FUN = function(x) {
    if (!require(x, character.only = TRUE)) {
      install.packages(x, dependencies = TRUE)
      library(x, character.only = TRUE)
    }
  }
)

# AUTHOR/SUPPORT
Federico Fuchs, fuchsf@fbmc.fcen.uba.ar </br>



