rm(list = ls(all = TRUE))
library(ggplot2)
library(stringr)
library(plyr)
library(grid)
library(gridExtra)
library(methods)
library(xtable)
library(knitr)
library(dunn.test)
library(pander)
library(reshape2)
library(data.table)
options(bitmapType="cairo")

codedirpath <- dirname(
  tryCatch(normalizePath(parent.frame(2)$ofile),  # works when using source
           error=function(e) # works when using R CMD
             normalizePath(unlist(strsplit(commandArgs()[grep('^--file=',
                                                              commandArgs())], '='))[2]))
)

datadir <- normalizePath(file.path(codedirpath, "data"))
invivodataonedays <- read.csv(file.path(datadir, "2012.12.09.1dayTD.csv"), header = TRUE)
invivocolnameonedays <- read.table(file.path(datadir, "2012.12.09.1dayTD.csv"), header = FALSE, sep = ",", nrows = 1)
levels(invivodataonedays$treatment)[levels(invivodataonedays$treatment)=="A"] <- "C"
levels(invivodataonedays$treatment)[levels(invivodataonedays$treatment)=="E"] <- "D"
levels(invivodataonedays$treatment)[levels(invivodataonedays$treatment)=="W"] <- "V"
levels(invivodataonedays$treatment)[levels(invivodataonedays$treatment)=="S"] <- "T"
invivodatathreedays <- read.csv(file.path(datadir, "2012.12.12.3daysTD.csv"), header = TRUE)
invivocolnamethreedays <- read.table(file.path(datadir, "2012.12.12.3daysTD.csv"), header = FALSE, sep = ",", nrows = 1)
levels(invivodatathreedays$treatment)[levels(invivodatathreedays$treatment)=="B"] <- "C"
levels(invivodatathreedays$treatment)[levels(invivodatathreedays$treatment)=="G"] <- "D"
levels(invivodatathreedays$treatment)[levels(invivodatathreedays$treatment)=="X"] <- "V"
levels(invivodatathreedays$treatment)[levels(invivodatathreedays$treatment)=="U"] <- "T"
invivodatasevendays <- read.csv(file.path(datadir, "2012.08.23.7daysTD.csv"), header = TRUE)
invivocolnamesevendays <- read.table(file.path(datadir, "2012.08.23.7daysTD.csv"), header = FALSE, sep = ",", nrows = 1)

textSize <- 11
condsVDC <- c("V", "D", "C")
condsVDTC <- c("V", "D", "T", "C")
conditionsVDC <- c("Veh", "Dexa", "Dexa + Testo")
conditionsVDTC <- c("Veh", "Dexa", "Testo", "Dexa + Testo")
contrastsfour <- c("D vs DT", "T vs DT", "D vs T", "V vs DT", "V vs D", "V vs T")
contraststhree <- c("D vs DT", "V vs DT", "V vs D")

nicepalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
meaningfulpalette <- c("#444444", "#dd0000", "#00dd00", "#0000dd", "#888800", "#880088", "#008888", "#dddddd")
stdplottimecourse <- theme_bw() + 
  theme(text = element_text(size = textSize, color = "black"),
        panel.grid.major.x = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        axis.title.x = element_text( size = textSize),
        axis.title.y = element_text( size = textSize),
        axis.text.x = element_text( size = textSize * .7 ),
        axis.text.y = element_text( size = textSize * .5 ),
        legend.title = element_blank())


SEM <- function(x) {
  return( sqrt(var(x, na.rm = TRUE) / truecount(x)) )
}

longdescription <- function(x) {
  return(paste0(
    signif(mean(x, na.rm = TRUE), digits = 3), 
    " (", 
    signif(SEM(x), digits = 3),
    "; n = ", 
    truecount(x), ")"))
}

CI95 <- function(x) {
  return( qt(.975, df = (truecount(x) - 1)) * SEM(x) )
}

sig3 <- function(x) {
  return(signif(x, digits = 3))
}

semInterval <- function(x) {
  lims <- c(
    mean(x, na.rm = TRUE) + SEM(x),
    mean(x, na.rm = TRUE) - SEM(x)
    )
  names(lims) <- c(
    'ymin',
    'ymax'
    )
  return(lims)
}

truecount <- function(x) {
  return( length(na.omit(x)) )
}

hview <- function(htmllines) {
  htmlFile <- tempfile(fileext = ".html")
  writeLines(htmllines, htmlFile)
  rstudio::viewer(htmlFile)
}

loadPMID12519877 <- function(datadirpath){
  df1 <- read.csv(file.path(datadirpath, "PMID12519877", "proteasomeactivity.csv"), header = FALSE, colClasses = c(NA, "NULL", NA))
  row.names(df1) <- df1$V1
  return(df1)
}

PMID12519877data <- loadPMID12519877(datadir)

reportstats <- function(invivodata, invivocolnames){
  myoutput <- ""
  invivodatasubset <- invivodata[invivodatasevendays$treatment %in% c("V", "D", "C"), ]
  invivodata$treatment <- factor(invivodata$treatment, 
                          levels = c("V", "D", "T", "C"), 
                          labels = c("Vehicle", "Dexa", "Testo", "Dexa + Testo"))
  invivodatasubset$treatment <- factor(invivodatasubset$treatment, 
                                levels = c("V", "D", "C"), 
                                labels = c("Vehicle", "Dexa", "Dexa + Testo"))
  
  for (i in 1:length(invivocolnames)) {
    if (invivocolnames[[i]] %in% c("animal", "TreatmentLong", "treatment")) next
    meanV <- mean(subset(invivodatasevendays, treatment == "V")[,i], na.rm = TRUE)
    meanD <- mean(subset(invivodatasevendays, treatment == "D")[,i], na.rm = TRUE)
    meanC <- mean(subset(invivodatasevendays, treatment == "C")[,i], na.rm = TRUE)
    myoutput <- paste0(myoutput, 
                       "# ", invivocolnames[[i]])
    myoutput <- paste(myoutput, 
                      pandoc.table.return(setNames(aggregate(invivodata[,i], list(invivodata$treatment), longdescription), c("Treatment", "Average (SD; n)")),
                                          style = "rmarkdown"))
    kw <- kruskal.test(as.formula(paste(colnames(invivodata)[i], "~treatment")), data = invivodata)
    myoutput <- paste(myoutput, 
                      paste0("Kruskal-Wallis p value for the four-way comparison is ", signif(kw$p.value, digits = 3)))
    dunns <- dunn.test(invivodata[,i], invivodata$treatment, method = "bonferroni")
    dunnsreport <- data.frame(contrastsfour, dunns$P)[c(5, 1), ]
    myoutput <- paste(myoutput,
                      pandoc.table.return(dunnsreport, style = "rmarkdown"))
    kw <- kruskal.test(as.formula(paste(colnames(invivodata)[i], "~treatment")), data = invivodatasubset)
    myoutput <- paste(myoutput, 
                      paste0("Kruskal-Wallis p value for the three-way comparison is ", signif(kw$p.value, digits = 3)))
    dunns <- dunn.test(invivodatasubset[,i], invivodatasubset$treatment, method = "bonferroni")
    dunnsreport <- data.frame(contraststhree, dunns$P)[c(3, 1), ]
    dunnsreport <- setNames(cbind(dunnsreport, c(ifelse(meanV > meanD, 'V > D' , 'V < D'), ifelse(meanD > meanC, 'D > DT' , 'D < DT'))),
                            c("Comparison", "P value", "Direction"))
    myoutput <- paste(myoutput,
                      pandoc.table.return(dunnsreport, style = "rmarkdown"))
    
  }
  return(myoutput)
}

#cat(myoutput)