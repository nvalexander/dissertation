apt-get install libcairo2-dev libxt-dev r-cran-ggplot2 r-cran-stringr r-cran-plyr r-cran-gridextra r-cran-xtable r-cran-reshape r-cran-cairodevice r-cran-rcurl r-cran-multcomp
options(repos=structure(c(CRAN="http://watson.nci.nih.gov/cran_mirror/")))
#install.packages("ggplot2") # also found in Ubuntu repo
#install.packages("stringr") # also found in Ubuntu repo
#install.packages("plyr") # also found in Ubuntu repo
#install.packages("gridExtra") # also found in Ubuntu repo
#install.packages("xtable") # also found in Ubuntu repo
#install.packages("reshape2") # also found in Ubuntu repo
#install.packages("cairoDevice") # also found in Ubuntu repo
#install.packages("RCurl") # also found in Ubuntu repo
#install.packages("multcomp") # also found in Ubuntu repo
install.packages("RJSONIO")
install.packages("PKI")
install.packages("rstudioapi")
install.packages("packrat")
install.packages("grid")
install.packages("methods")
install.packages("knitr")
install.packages("dunn.test")
install.packages("htmltools")
install.packages("caTools")
install.packages("rmarkdown")
install.packages("pander")
install.packages("data.table")
install.packages("Cairo")
install.packages("extrafont")
library(extrafont)
font_import(prompt = FALSE)
