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
library(extrafont)
library(Cairo)
library(scales)
#font_import(prompt = FALSE)
options(bitmapType="cairo")

# DATA loading and mangling
codedirpath <- dirname(
  tryCatch(normalizePath(parent.frame(2)$ofile),  # works when using source
           error=function(e) # works when using R CMD
             normalizePath(unlist(strsplit(commandArgs()[grep('^--file=',
                                                              commandArgs())], '='))[2]))
)
codedirpath <- "/media/dump/writingswork/draftthesis"
datadir <- normalizePath(file.path(codedirpath, "data"))
InvivoOneday <- read.csv(file.path(datadir, "2012.12.09.1dayTD.csv"), header = TRUE)
InvivoOnedayNicenames <- read.table(file.path(datadir, "2012.12.09.1dayTD.csv"), 
  header = FALSE, 
  sep = ",", 
  nrows = 1, 
  stringsAsFactors = FALSE)
levels(InvivoOneday$treatment)[levels(InvivoOneday$treatment)=="A"] <- "C"
levels(InvivoOneday$treatment)[levels(InvivoOneday$treatment)=="E"] <- "D"
levels(InvivoOneday$treatment)[levels(InvivoOneday$treatment)=="W"] <- "V"
levels(InvivoOneday$treatment)[levels(InvivoOneday$treatment)=="S"] <- "T"
InvivoThreeday <- read.csv(file.path(datadir, "2012.12.12.3daysTD.csv"), header = TRUE)
InvivoThreedayNicenames <- read.table(file.path(datadir, "2012.12.12.3daysTD.csv"), 
  header = FALSE, 
  sep = ",", 
  nrows = 1, 
  stringsAsFactors = FALSE)
levels(InvivoThreeday$treatment)[levels(InvivoThreeday$treatment)=="B"] <- "C"
levels(InvivoThreeday$treatment)[levels(InvivoThreeday$treatment)=="G"] <- "D"
levels(InvivoThreeday$treatment)[levels(InvivoThreeday$treatment)=="X"] <- "V"
levels(InvivoThreeday$treatment)[levels(InvivoThreeday$treatment)=="U"] <- "T"
InvivoSevenday <- read.csv(file.path(datadir, "2012.08.23.7daysTD.csv"), header = TRUE)
InvivoSevendayNicenames <- read.table(file.path(datadir, "2012.08.23.7daysTD.csv"), 
  header = FALSE, 
  sep = ",", 
  nrows = 1, 
  stringsAsFactors = FALSE)
#re-leveling
condsVDC <- c("V", "D", "C")
conditionsVDC <- c("Veh", "Dexa", "Comb")
condsVDTC <- c("V", "D", "T", "C")
conditionsVDTC <- c("Veh", "Dexa", "Testo", "Comb")
contrastsfour <- c("V vs D", "V vs T", "D vs T", "V vs DT", "D vs DT", "T vs DT")
VvsDfourways <- match("V vs D", contrastsfour)[[1]]
VvsTfourways <- match("V vs T", contrastsfour)[[1]]
DvsCfourways <- match("D vs DT", contrastsfour)[[1]]
contraststhree <- c("V vs D", "V vs DT", "D vs DT")
VvsDthreeways <- match("V vs D", contraststhree)[[1]]
DvsCthreeways <- match("D vs DT", contraststhree)[[1]]
VvsCthreeways <- match("V vs DT", contraststhree)[[1]]
InvivoOnedayCVD <- InvivoOneday[InvivoOneday$treatment %in% condsVDC, ]
InvivoOneday$treatment <- factor(InvivoOneday$treatment, 
                                      levels = condsVDTC)
InvivoOnedayCVD$treatment <- factor(InvivoOnedayCVD$treatment, 
                                            levels = condsVDC)

InvivoThreedayCVD <- InvivoThreeday[InvivoThreeday$treatment %in% condsVDC, ]
InvivoThreeday$treatment <- factor(InvivoThreeday$treatment, 
                                        levels = condsVDTC)
InvivoThreedayCVD$treatment <- factor(InvivoThreedayCVD$treatment, 
                                              levels = condsVDC)

InvivoSevendayCVD <- InvivoSevenday[InvivoSevenday$treatment %in% condsVDC, ]
InvivoSevenday$treatment <- factor(InvivoSevenday$treatment, 
                                        levels = condsVDTC)
InvivoSevendayCVD$treatment <- factor(InvivoSevendayCVD$treatment, 
                                              levels = condsVDC)
InvivoOnedayV <- InvivoOneday[InvivoOneday$treatment == "V", ]
InvivoOnedayD <- InvivoOneday[InvivoOneday$treatment == "D", ]
InvivoOnedayC <- InvivoOneday[InvivoOneday$treatment == "C", ]
InvivoThreedayT <- InvivoThreeday[InvivoThreeday$treatment == "T", ]
InvivoThreedayV <- InvivoThreeday[InvivoThreeday$treatment == "V", ]
InvivoThreedayD <- InvivoThreeday[InvivoThreeday$treatment == "D", ]
InvivoThreedayC <- InvivoThreeday[InvivoThreeday$treatment == "C", ]
InvivoThreedayT <- InvivoThreeday[InvivoThreeday$treatment == "T", ]
InvivoSevendayV <- InvivoSevenday[InvivoSevenday$treatment == "V", ]
InvivoSevendayD <- InvivoSevenday[InvivoSevenday$treatment == "D", ]
InvivoSevendayC <- InvivoSevenday[InvivoSevenday$treatment == "C", ]
InvivoSevendayT <- InvivoSevenday[InvivoSevenday$treatment == "T", ]

#literal constants
unistar <- sprintf('\u2736')
unidagger <- sprintf('\u2020')
threeemptystrings <- c("", "", "")
threeidenticalgroups <- c("a", "a", "a")

#helper maths functions
SEM <- function(x) {
  return( sqrt(var(x, na.rm = TRUE) / truecount(x)) )
}

longdescription <- function(x) {
  return(paste0(
    format(truemean(x), digits = 3), 
    " (", 
    format(SEM(x), digits = 3),
    "; n = ", 
    truecount(x), ")"))
}

CI95 <- function(x) {
  return( qt(.975, df = (truecount(x) - 1)) * SEM(x) )
}

sig3 <- function(x) {
  return(format(x, digits = 3))
}

semInterval <- function(x) {
  lims <- c(
    mean(x, na.rm = TRUE) - SEM(x),
    mean(x, na.rm = TRUE) + SEM(x)
    )
  names(lims) <- c('ymin','ymax')
  return(lims)
}

statstringyunderbar <-function(x){
  return(min(semInterval(x)))
}

statstringyoverbar <-function(x){
  return(max(semInterval(x)))
}

truecount <- function(x) {
  return( length(na.omit(x)) )
}

truemean <- function(x) {
  return( mean(x, na.rm = TRUE) )
}

hview <- function(htmllines) {
  htmlFile <- tempfile(fileext = ".html")
  writeLines(htmllines, htmlFile)
  rstudio::viewer(htmlFile)
}

rescaledtovehicleasunity <- function(x){
  #this function expects a two-column data frame
  #one column must be "treatments", and include some "V"
  #the other column will be rescaled by multiplication so that the average of V becomes 1
  y <- x
  y[,2] <- y[,2] / mean(subset(y, treatment == "V")[,2], na.rm = TRUE)
  return(y)
}

rescaledtovehicleaszero <- function(x){
  #this function expects a two-column data frame
  #one column must be "treatments", and include some "V"
  #the other column will be rescaled by addition so that the average of V becomes 0
  y <- x
  y[,2] <- y[,2] - mean(subset(y, treatment == "V")[,2], na.rm = TRUE)
  return(y)
}

#GENERIC PLOTTING
textSize <- 11
nicepalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
meaningfulpalette <- c("#444444", "#dd0000", "#00dd00", "#0000dd", "#888800", "#880088", "#008888", "#dddddd")
greypalette <- c("#ffffff", "#222222", "#999999", "#0000dd", "#00dd00", "#dd0000", "#008888", "#888800")

stdplottimecourse <- theme_bw() + 
  theme(text = element_text(size = textSize, color = "black", family="Liberation Sans Narrow"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        axis.title.x = element_text( size = textSize),
        axis.title.y = element_text( size = textSize),
        axis.text.x = element_text( size = textSize ),
        axis.text.y = element_text( size = textSize ),
        legend.title = element_blank())

stdbarplot <- 
  theme_bw() +
  theme(text = element_text(size = textSize, color = "black", family="Liberation Sans Narrow"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(color = "black"),
        axis.ticks.x = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x= element_blank(),
        axis.title.y = element_text( size = textSize))

anotatedtitle <- function(text, x, y) {
  return(annotate(geom = "text", label = text, x = x, y = y,
           vjust = 1, hjust = 0.5,  
           size = .45 * textSize, 
           family = "Liberation Sans Narrow"))
}

threecolumnplot <- function(skinnydataset, ylabel, ylimit, statstrings){
  #this function expects a dataframe with two columns, first designating the treatments
  completecasesdataset <- skinnydataset[complete.cases(skinnydataset),]
  return(ggplot(completecasesdataset) +
           aes_string( x = colnames(completecasesdataset)[1], 
                       y = colnames(completecasesdataset)[2], 
                       fill = colnames(completecasesdataset)[1]) +
           stat_summary(fun.y = mean, 
                        geom = "bar", 
                        colour = "black",
                        show_guide = FALSE) +
           stat_summary(geom = 'errorbar',
                        fun.data = 'semInterval',
                        width = 0.1) +
           stat_summary(geom = "text", 
                        size = textSize * .4,
                        aes(family = "Liberation Sans Narrow"),
                        fun.y = statstringyoverbar, 
                        hjust = .5,
                        vjust = -.6,
                        label = statstrings) +
           ylab(ylabel) +
           coord_cartesian(ylim = ylimit) + 
           scale_fill_manual(values = greypalette) +
           stdbarplot)
}


threegeneplot <- function(skinnydataset, ylabel, ylimit, statstrings){
  #this function expects a dataframe with two columns, first designating the treatments
  # second column described Ct(GOI)-Ct(housekeeping gene)
  completecasesdataset <- skinnydataset[complete.cases(skinnydataset),]
  completecasesdataset[,2] <- completecasesdataset[,2] * (-1)
  return(ggplot(completecasesdataset) +
           aes_string( x = colnames(completecasesdataset)[1], 
                       y = colnames(completecasesdataset)[2], 
                       group = colnames(completecasesdataset)[1]) +
           stat_summary(geom = "point",
                        size = 3,
                        colour = "black",
                        fun.y = mean,
                        show_guide = FALSE,
                        aes_string(shape = colnames(completecasesdataset)[1])) +
           stat_summary(geom = "text", 
                        size = textSize * .4,
                        aes(family = "Liberation Sans Narrow"),
                        fun.y = statstringyoverbar, 
                        hjust = .5,
                        vjust = -.6,
                        label = statstrings) + 
           stat_summary(geom = 'errorbar',
                        fun.data = 'semInterval',
                        width = 0.1,
                        show_guide = FALSE,
                        position=position_dodge(.05)) +
           ylab(ylabel) +
           scale_y_continuous(breaks = (-10:10), labels = prettyNum(2^(-10:10))) +
           coord_cartesian(ylim = ylimit) +
           scale_shape_manual(values = c(16, 4, 1), labels = conditionsVDC, guide = FALSE) +
           stdbarplot)
}

#PMID12519877
loadPMID12519877 <- function(datadirpath){
  df1 <- read.csv(file.path(datadirpath, "PMID12519877", "proteasomeactivity.csv"), header = FALSE, colClasses = c(NA, "NULL", NA))
  row.names(df1) <- df1$V1
  return(df1)
}
PMID12519877data <- loadPMID12519877(datadir)

#MASS-REPORTING of IN VIVO DATA
reportstats <- function(invivodata, invivocolnames){
  myoutput <- ""
  invivodatasubset <- invivodata[invivodata$treatment %in% condsVDC, ]
  invivodatasubset$treatment <- factor(invivodatasubset$treatment, 
                                levels = condsVDC)
  
  for (i in 1:length(invivocolnames)) {
    if (invivocolnames[[i]] %in% c("animal", "TreatmentLong", "treatment")) next
    myoutput <- paste0(myoutput, 
                       "# ", invivocolnames[[i]])
    myoutput <- paste(myoutput, 
                      pandoc.table.return(setNames(aggregate(invivodata[,i], list(invivodata$treatment), longdescription), 
                                                   c("Treatment", "Average (SD; n)")),
                                          style = "rmarkdown"))
    kwfour <- kruskal.test(as.formula(paste(colnames(invivodata)[i], "~treatment")), data = invivodata)
    myoutput <- paste(myoutput, 
                      paste0("Kruskal-Wallis p value for the four-way comparison is ", signif(kwfour$p.value, digits = 3)))
    dunnsfour <- dunn.test(invivodata[,i], invivodata$treatment, method = "bonferroni")
    dunnsreport <- data.frame(contrastsfour, dunnsfour$P.adjusted)[c(VvsDfourways, DvsCfourways, VvsTfourways), ]
    myoutput <- paste(myoutput,
                      pandoc.table.return(dunnsreport, style = "rmarkdown"))
    kwthree <- kruskal.test(as.formula(paste(colnames(invivodata)[i], "~treatment")), data = invivodatasubset)
    myoutput <- paste(myoutput, 
                      paste0("Kruskal-Wallis p value for the three-way comparison is ", signif(kwthree$p.value, digits = 3)))
    dunnsthree <- dunn.test(invivodatasubset[,i], invivodatasubset$treatment, method = "bonferroni")
    dunnsreport <- data.frame(contraststhree, dunnsthree$P.adjusted)[c(VvsDthreeways, DvsCthreeways, VvsCthreeways), ]
    meanV <- truemean(subset(invivodata, treatment == "V")[,i])
    meanD <- truemean(subset(invivodata, treatment == "D")[,i])
    meanC <- truemean(subset(invivodata, treatment == "C")[,i])
    dunnsreport <- setNames(cbind(dunnsreport, c(ifelse(meanV > meanD, 'V > D' , 'V < D'), 
                                                 ifelse(meanD > meanC, 'D > DT' , 'D < DT'), 
                                                 ifelse(meanV > meanC, 'V > DT' , 'V < DT'))),
                            c("Comparison", "P value", "Direction"))
    myoutput <- paste(myoutput,
                      pandoc.table.return(dunnsreport, style = "rmarkdown"))
    
  }
  return(myoutput)
}

#FIRST PLOT - body weights at sacrifice
plotbodyweightsatsacrifice <- function(){
  ylabel <- "body weight (g)"
  ylimit <- c(0, 32)
  onedayweightstat <- c("a", "b", "a,b")
  threedayweightstat <- c("a,b", "a", "b")
  sevendayweightstat <- threeidenticalgroups
  return(
    grid.arrange(
      threecolumnplot(InvivoOnedayCVD[, colnames(InvivoOnedayCVD) %in% c("treatment", "day.2.body.weight..g.")], 
                      ylabel, 
                      ylimit, 
                      onedayweightstat) +
        anotatedtitle("one day", 2, ylimit[[2]]) + 
        theme(axis.text.x = element_text(color = "black")) + 
        scale_x_discrete(labels = conditionsVDC),
      threecolumnplot(InvivoThreedayCVD[, colnames(InvivoThreedayCVD) %in% c("treatment", "day.4.body.weight..g.")], 
                      ylabel, 
                      ylimit, 
                      threedayweightstat) +
        anotatedtitle("three days", 2, ylimit[[2]]) + 
        theme(axis.text.x = element_text(color = "black")) +
        scale_x_discrete(labels = threeemptystrings),
      threecolumnplot(InvivoSevendayCVD[, colnames(InvivoSevendayCVD) %in% c("treatment", "day.8.body.weight..g.")], 
                      ylabel, 
                      ylimit, 
                      sevendayweightstat) +
        anotatedtitle("seven days", 2, ylimit[[2]]) + 
        theme(axis.text.x = element_text(color = "black")) +
        scale_x_discrete(labels = threeemptystrings),
      ncol=3))
  
#   #1
#   shortdf <- InvivoOnedayCVD[, colnames(InvivoOnedayCVD) %in% c("treatment", "day.2.body.weight..g.")]
#   onedaysweightplot <- threecolumnplot(shortdf, ylabel, ylimit, onedaysstat) +
#     anotatedtitle("one day", 2, ylimit[[2]]) + theme(axis.text.x = element_text(color = "black")) + scale_x_discrete(labels = conditionsVDC)
#   #2
#   shortdf <- InvivoThreedayCVD[, colnames(InvivoThreedayCVD) %in% c("treatment", "day.4.body.weight..g.")]
#   threedaysweightplot <-  threecolumnplot(shortdf, ylabel, ylimit, threedaysstat) +
#     anotatedtitle("three days", 2, ylimit[[2]]) + theme(axis.text.x = element_text(color = "black")) + scale_x_discrete(labels = threeemptystrings)
#   #3
#   shortdf <- InvivoSevendayCVD[, colnames(InvivoSevendayCVD) %in% c("treatment", "day.8.body.weight..g.")]
#   sevedaysweightplot <-  threecolumnplot(shortdf, ylabel, ylimit, sevendaysstat) +
#     anotatedtitle("seven days", 2, ylimit[[2]]) + theme(axis.text.x = element_text(color = "black")) + scale_x_discrete(labels = threeemptystrings)
#   
#   return(grid.arrange(onedaysweightplot, threedaysweightplot, sevedaysweightplot, ncol=3))
}

#SECOND PLOT - body weight time courses
plotbodyweightcourse <- function(){
  timeseriescolumns <- c("body.weight.gain.after.1.days..percent.",
                         "body.weight.gain.after.2.days..percent.",
                         "body.weight.gain.after.3.days..percent.",
                         "body.weight.gain.after.4.days..percent.",
                         "body.weight.gain.after.5.days..percent.",
                         "body.weight.gain.after.6.days..percent.",
                         "body.weight.gain.after.7.days..percent.",
                         "body.weight.gain.after.8.days..percent.")
  shortdf <- InvivoSevendayCVD[, colnames(InvivoSevendayCVD) %in% c("treatment", timeseriescolumns)]
  body.weight.gain.after.0.days..percent. <- rep(0, dim(shortdf)[1])
  statsstars <- c("", "", "",
                  "", paste0(unidagger, unistar),"",
                  "", unidagger,"",
                  "", unidagger,"",
                  "", unidagger,"",
                  "", unidagger,"",
                  "", unidagger,"",
                  "", paste0(unidagger, unistar),"")
  shortdf <- cbind(body.weight.gain.after.0.days..percent., shortdf)
  shortdf <- melt(shortdf, id = c("treatment"), value.name = "bodyweight")
  colnames(shortdf)[2] <- "day"
  setattr(shortdf$day, "levels", 1:8)
  topplot <- ggplot(shortdf) + 
    aes_string(x = colnames(shortdf)[2],
               y = colnames(shortdf)[3],
               group = colnames(shortdf)[1]) +
    stat_summary(geom = "point",
                 size = 3,
                 fun.y = mean,
                 position = position_dodge(.05),
                 aes_string(shape = colnames(shortdf)[1])) +
    stat_summary(geom = "line", 
                 size = .5, 
                 fun.y = mean, 
                 position = position_dodge(.05)) + 
    stat_summary(geom = "text", 
                 size = textSize * .4,
                 aes(family = "serif"),
                 fun.y = statstringyunderbar, 
                 hjust = -.2,
                 vjust = .25,
                 label = statsstars) + 
    stat_summary(geom = 'errorbar',
                 fun.data = 'semInterval',
                 width = 0.2,
                 show_guide = FALSE,
                 position=position_dodge(.05)) +
    coord_cartesian(ylim = c(-7, 10)) + 
    ylab("body weight gain (% of pre-treatment)") +
    scale_shape_manual(values = c(16, 4, 1), labels = conditionsVDC, guide = FALSE) +
    stdplottimecourse
  
  shortdf <- InvivoThreedayCVD[, colnames(InvivoThreedayCVD) %in% c("treatment", timeseriescolumns[1:4])]
  body.weight.gain.after.0.days..percent. <- rep(0, dim(shortdf)[1])
  shortdf <- cbind(body.weight.gain.after.0.days..percent., shortdf)
  shortdf <- melt(shortdf, id = c("treatment"), value.name = "bodyweight")
  colnames(shortdf)[2] <- "day"
  setattr(shortdf$day, "levels", 1:4)
  bottomleftplot <- ggplot(shortdf) + 
    aes_string(x = colnames(shortdf)[2],
               y = colnames(shortdf)[3],
               group = colnames(shortdf)[1]) +
    stat_summary(geom = "point",
                 size = 3,
                 fun.y = mean,
                 position = position_dodge(.05),
                 aes_string(shape = colnames(shortdf)[1])) +
    stat_summary(geom = "line", 
                 size = .5, 
                 fun.y = mean, 
                 position = position_dodge(.05)) + 
    stat_summary(geom = 'errorbar',
                 fun.data = 'semInterval',
                 width = 0.2,
                 show_guide = FALSE,
                 position=position_dodge(.05)) +
    coord_cartesian(ylim = c(-7, 10)) + 
    ylab("body weight gain (% of pre-treatment)") +
    scale_shape_manual(values = c(16, 4, 1), labels = conditionsVDC, guide = FALSE) +
    stdplottimecourse
    
  shortdf <- InvivoOnedayCVD[, colnames(InvivoOnedayCVD) %in% c("treatment", timeseriescolumns[1:1])]
  body.weight.gain.after.0.days..percent. <- rep(0, dim(shortdf)[1])
  shortdf <- cbind(body.weight.gain.after.0.days..percent., shortdf)
  shortdf <- melt(shortdf, id = c("treatment"), value.name = "bodyweight")
  colnames(shortdf)[2] <- "day"
  setattr(shortdf$day, "levels", 1:2)
  bottomrightplot <- ggplot(shortdf) + 
    aes_string(x = colnames(shortdf)[2],
               y = colnames(shortdf)[3],
               group = colnames(shortdf)[1]) +
    stat_summary(geom = "point",
                 size = 3,
                 fun.y = mean,
                 position = position_dodge(.05),
                 aes_string(shape = colnames(shortdf)[1])) +
    stat_summary(geom = "line", 
                 size = .5, 
                 fun.y = mean, 
                 position = position_dodge(.05)) + 
    stat_summary(geom = 'errorbar',
                 fun.data = 'semInterval',
                 width = 0.2,
                 show_guide = FALSE,
                 position=position_dodge(.05)) +
    coord_cartesian(ylim = c(-7, 10)) + 
    ylab("body weight gain (% of pre-treatment)") +
    scale_shape_manual(values = c(16, 4, 1), labels = conditionsVDC) +
    stdplottimecourse 
  
  return(grid.arrange(topplot, arrangeGrob(bottomleftplot, bottomrightplot, ncol=2, widths = c(1.25,1) ), 
                       ncol=1)  )
}

# THIRD PLOT - body mass composition
plotleanfat <- function(){
  leancolumn <- "lean.mass.gain..g."
  fatcolumn <- "fat.mass.gain..g."
  watercolumn <- "total.water.gain..g."
  leanlabel <- "lean mass loss (g)"
  fatlabel <- "fat mass gain (g)"
  waterlabel <- "water loss (g)"
  leanylim <- c(0, 4)
  fatylim <- c(0, 4)
  waterylim <- c(0, 5)
  leanonestat <- threeidenticalgroups
  leanthreestat <- c("a", "b", "a,b")
  leansevenstat <- c("a", "b", "a,b")
  fatonestat <- threeidenticalgroups
  fatthreestat <- c("a", "a,b", "b")
  fatsevenstat <- c("a", "a,b", "b")
  wateronestat <- threeidenticalgroups
  waterthreestat <- threeidenticalgroups
  watersevenstat <- threeidenticalgroups
  
  #1
  shortdf <- InvivoOnedayCVD[, colnames(InvivoOnedayCVD) %in% c("treatment", leancolumn)]
  shortdf[,2] <- shortdf[, 2] * (-1)
  leanoneplot <- threecolumnplot(shortdf, leanlabel, leanylim, leanonestat) +
    theme(axis.text.x = element_text(color = "black")) + scale_x_discrete(labels = conditionsVDC)
  
  #THIS FORCES THE DISPLAY OF A LEGEND
#   meanslayer <- leanoneplot$layers[[1]]
#   meanslayer$show_guide <- TRUE
#   leanoneplot <- leanoneplot + 
#     scale_fill_manual(values = greypalette, labels = conditionsVDC) + 
#     guides(fill = guide_legend(title = NULL, override.aes = list(colour = NULL))) + 
#     theme(legend.position = c(0, 1), legend.justification = c(0,1))
#   
  #2
  shortdf <- InvivoThreedayCVD[, colnames(InvivoThreedayCVD) %in% c("treatment", leancolumn)]
  shortdf[,2] <- shortdf[, 2] * (-1)
  leanthreeplot <- threecolumnplot(shortdf, leanlabel, leanylim, leanthreestat) +
    theme(axis.text.x = element_text(color = "black")) + scale_x_discrete(labels = conditionsVDC)
  
  #3
  shortdf <- InvivoSevendayCVD[, colnames(InvivoSevendayCVD) %in% c("treatment", leancolumn)]
  shortdf[,2] <- shortdf[, 2] * (-1)
  leansevenplot <- threecolumnplot(shortdf, leanlabel, leanylim, leansevenstat) +
    theme(axis.text.x = element_text(color = "black")) + scale_x_discrete(labels = conditionsVDC)
  
  #4
  shortdf <- InvivoOnedayCVD[, colnames(InvivoOnedayCVD) %in% c("treatment", fatcolumn)]
  fatoneplot <- threecolumnplot(shortdf, fatlabel, fatylim, fatonestat)
  
  #5
  shortdf <- InvivoThreedayCVD[, colnames(InvivoThreedayCVD) %in% c("treatment", fatcolumn)]
  fatthreeplot <- threecolumnplot(shortdf, fatlabel, fatylim, fatthreestat)
  
  #6
  shortdf <- InvivoSevendayCVD[, colnames(InvivoSevendayCVD) %in% c("treatment", fatcolumn)]
  fatsevenplot <- threecolumnplot(shortdf, fatlabel, fatylim, fatsevenstat)
  
  #7
  shortdf <- InvivoOnedayCVD[, colnames(InvivoOnedayCVD) %in% c("treatment", watercolumn)]
  shortdf[,2] <- shortdf[, 2] * (-1)
  wateroneplot <- threecolumnplot(shortdf, waterlabel, waterylim, wateronestat) +
    anotatedtitle("one day", 2, waterylim[[2]])
  
  #8
  shortdf <- InvivoThreedayCVD[, colnames(InvivoThreedayCVD) %in% c("treatment", watercolumn)]
  shortdf[,2] <- shortdf[, 2] * (-1)
  waterthreeplot <- threecolumnplot(shortdf, waterlabel, waterylim, waterthreestat) +
    anotatedtitle("three days", 2, waterylim[[2]])
  
  #9
  shortdf <- InvivoSevendayCVD[, colnames(InvivoSevendayCVD) %in% c("treatment", watercolumn)]
  shortdf[,2] <- shortdf[, 2] * (-1)
  watersevenplot <- threecolumnplot(shortdf, waterlabel, waterylim, watersevenstat) +
    anotatedtitle("seven days", 2, waterylim[[2]])

  return(grid.arrange(
    wateroneplot, waterthreeplot, watersevenplot,
    fatoneplot, fatthreeplot, fatsevenplot,
    leanoneplot, leanthreeplot, leansevenplot, 
    ncol=3))
}

# FOURTH PLOT SHOWS five muscles across three time points
plotmuscleweights <- function(){
  levatorcolumn <- "levator..mg."
  quadricepscolumn <- "quadriceps..mg."
  gastrocnemiuscolumn <- "gastrocnemius..mg."
  tricepscolumn <- "triceps..mg."
  tibialiscolumn <- "tibialis..mg."

  levatorlabel <- paste0("levator ani (g)")
  quadricepslabel <- paste0("quadriceps (g)")
  gastrocnemiuslabel <- paste0("gastrocnemius (g)")
  tricepslabel <- paste0("triceps br. (g)")
  tibialislabel <- paste0("tibialis ant. (g)")
  
  levatorylim <- c(0, 100)
  quadricepsylim <- c(0, 225)
  gastrocnemiusylim <- c(0, 180)
  tricepsylim <- c(0, 150)
  tibialisylim <- c(0, 75)

  levatoronestat <- threeidenticalgroups
  levatorthreestat <- threeidenticalgroups
  levatorsevenstat <- c("a", "b", "a,b")
  quadricepsonestat <- threeidenticalgroups
  quadricepsthreestat <- threeidenticalgroups
  quadricepssevenstat <- c("a", "b", "a,b")
  gastrocnemiusonestat <- threeidenticalgroups
  gastrocnemiusthreestat <- c("a", "b", "a,b")
  gastrocnemiussevenstat <- c("a", "b", "a,b")
  tricepsonestat <- threeidenticalgroups
  tricepsthreestat <- threeidenticalgroups
  tricepssevenstat <- c("a", "b", "b")
  tibialisonestat <- threeidenticalgroups
  tibialisthreestat <- threeidenticalgroups
  tibialissevenstat <- threeidenticalgroups
  
  #1
  shortdf <- InvivoOnedayCVD[, colnames(InvivoOnedayCVD) %in% c("treatment", levatorcolumn)]
  levatoroneplot <- threecolumnplot(shortdf, levatorlabel, levatorylim, levatoronestat) +
    anotatedtitle("one day", 2, levatorylim[[2]]) 
  
  #2
  shortdf <- InvivoThreedayCVD[, colnames(InvivoThreedayCVD) %in% c("treatment", levatorcolumn)]
  levatorthreeplot <- threecolumnplot(shortdf, levatorlabel, levatorylim, levatorthreestat) +
    anotatedtitle("three days", 2, levatorylim[[2]])
  
  #3
  shortdf <- InvivoSevendayCVD[, colnames(InvivoSevendayCVD) %in% c("treatment", levatorcolumn)]
  levatorsevenplot <- threecolumnplot(shortdf, levatorlabel, levatorylim, levatorsevenstat)  +
    anotatedtitle("seven days", 2, levatorylim[[2]]) 
  
  #4
  shortdf <- InvivoOnedayCVD[, colnames(InvivoOnedayCVD) %in% c("treatment", quadricepscolumn)]
  quadricepsoneplot <- threecolumnplot(shortdf, quadricepslabel, quadricepsylim, quadricepsonestat)
  
  #5
  shortdf <- InvivoThreedayCVD[, colnames(InvivoThreedayCVD) %in% c("treatment", quadricepscolumn)]
  quadricepsthreeplot <- threecolumnplot(shortdf, quadricepslabel, quadricepsylim, quadricepsthreestat)
  
  #6
  shortdf <- InvivoSevendayCVD[, colnames(InvivoSevendayCVD) %in% c("treatment", quadricepscolumn)]
  quadricepssevenplot <- threecolumnplot(shortdf, quadricepslabel, quadricepsylim, quadricepssevenstat) 
  
  #7
  shortdf <- InvivoOnedayCVD[, colnames(InvivoOnedayCVD) %in% c("treatment", gastrocnemiuscolumn)]
  gastrocnemiusoneplot <- threecolumnplot(shortdf, gastrocnemiuslabel, gastrocnemiusylim, gastrocnemiusonestat) 
  
  #8
  shortdf <- InvivoThreedayCVD[, colnames(InvivoThreedayCVD) %in% c("treatment", gastrocnemiuscolumn)]
  gastrocnemiusthreeplot <- threecolumnplot(shortdf, gastrocnemiuslabel, gastrocnemiusylim, gastrocnemiusthreestat) 
  
  #9
  shortdf <- InvivoSevendayCVD[, colnames(InvivoSevendayCVD) %in% c("treatment", gastrocnemiuscolumn)]
  gastrocnemiussevenplot <- threecolumnplot(shortdf, gastrocnemiuslabel, gastrocnemiusylim, gastrocnemiussevenstat)
  
  #10
  shortdf <- InvivoOnedayCVD[, colnames(InvivoOnedayCVD) %in% c("treatment", tricepscolumn)]
  tricepsoneplot <- threecolumnplot(shortdf, tricepslabel, tricepsylim, tricepsonestat)
  
  #11
  shortdf <- InvivoThreedayCVD[, colnames(InvivoThreedayCVD) %in% c("treatment", tricepscolumn)]
  tricepsthreeplot <- threecolumnplot(shortdf, tricepslabel, tricepsylim, tricepsthreestat) 
  
  #12
  shortdf <- InvivoSevendayCVD[, colnames(InvivoSevendayCVD) %in% c("treatment", tricepscolumn)]
  tricepssevenplot <- threecolumnplot(shortdf, tricepslabel, tricepsylim, tricepssevenstat)
  
  #13
  shortdf <- InvivoOnedayCVD[, colnames(InvivoOnedayCVD) %in% c("treatment", tibialiscolumn)]
  tibialisoneplot <- threecolumnplot(shortdf, tibialislabel, tibialisylim, tibialisonestat) +
    theme(axis.text.x = element_text(color = "black")) + scale_x_discrete(labels = conditionsVDC)
  
  #14
  shortdf <- InvivoThreedayCVD[, colnames(InvivoThreedayCVD) %in% c("treatment", tibialiscolumn)]
  tibialisthreeplot <- threecolumnplot(shortdf, tibialislabel, tibialisylim, tibialisthreestat) +
    theme(axis.text.x = element_text(color = "black")) + scale_x_discrete(labels = conditionsVDC)
  
  #15
  shortdf <- InvivoSevendayCVD[, colnames(InvivoSevendayCVD) %in% c("treatment", tibialiscolumn)]
  tibialissevenplot <- threecolumnplot(shortdf, tibialislabel, tibialisylim, tibialissevenstat) +
    theme(axis.text.x = element_text(color = "black")) + scale_x_discrete(labels = conditionsVDC)
  
  return(grid.arrange(levatoroneplot, levatorthreeplot, levatorsevenplot,
                      quadricepsoneplot, quadricepsthreeplot, quadricepssevenplot,
                      gastrocnemiusoneplot, gastrocnemiusthreeplot, gastrocnemiussevenplot,
                      tricepsoneplot, tricepsthreeplot, tricepssevenplot,
                      tibialisoneplot, tibialisthreeplot, tibialissevenplot,
                      ncol=3))
}

# FIFTH plot shows proteasome activity in three muscles at three time points
plotproteasomeactivity <- function(){
  quadricepscolumn <- "quadriceps.proteasome.activity..rel.u.."
  gastrocnemiuscolumn <- "gastrocnemius.proteasome.activity..rel.u.."
  tricepscolumn <- "triceps.proteasome.activity..rel.u.."
  
  quadricepslabel <- "quadriceps proteasome\nactivity (rel.u.)"
  gastrocnemiuslabel <- "gastrocnemius proteasome\nactivity (rel.u.)"
  tricepslabel <- "triceps proteasome\nactivity (rel.u.)"
  
  quadricepsylim <- c(0, 1.8)
  gastrocnemiusylim <- c(0, 2)
  tricepsylim <- c(0, 3)
  
  quadricepsonestat <- threeidenticalgroups
  quadricepsthreestat <- threeidenticalgroups
  quadricepssevenstat <- threeidenticalgroups
  gastrocnemiusonestat <- c("a,b", "a", "b")
  gastrocnemiusthreestat <- c("a", "b", "b")
  gastrocnemiussevenstat <- threeidenticalgroups
  tricepsonestat <- threeidenticalgroups
  tricepsthreestat <- threeidenticalgroups
  tricepssevenstat <- threeidenticalgroups  
  
  #1
  shortdf <- InvivoOnedayCVD[, colnames(InvivoOnedayCVD) %in% c("treatment", quadricepscolumn)]
  shortdf <- rescaledtovehicleasunity(shortdf)
  quadricepsoneplot <- threecolumnplot(shortdf, quadricepslabel, quadricepsylim, quadricepsonestat) +
    anotatedtitle("one day", 2, quadricepsylim[[2]]) 
  
  #2
  shortdf <- InvivoThreedayCVD[, colnames(InvivoThreedayCVD) %in% c("treatment", quadricepscolumn)]
  shortdf <- rescaledtovehicleasunity(shortdf)
  quadricepsthreeplot <- threecolumnplot(shortdf, quadricepslabel, quadricepsylim, quadricepsthreestat) +
    anotatedtitle("three days", 2, quadricepsylim[[2]])
  
  #3
  shortdf <- InvivoSevendayCVD[, colnames(InvivoSevendayCVD) %in% c("treatment", quadricepscolumn)]
  shortdf <- rescaledtovehicleasunity(shortdf)
  quadricepssevenplot <- threecolumnplot(shortdf, quadricepslabel, quadricepsylim, quadricepssevenstat) +
    anotatedtitle("seven days", 2, quadricepsylim[[2]]) 
  
  #4
  shortdf <- InvivoOnedayCVD[, colnames(InvivoOnedayCVD) %in% c("treatment", gastrocnemiuscolumn)]
  shortdf <- rescaledtovehicleasunity(shortdf)
  gastrocnemiusoneplot <- threecolumnplot(shortdf, gastrocnemiuslabel, gastrocnemiusylim, gastrocnemiusonestat) +
    theme(axis.text.x = element_text(color = "black")) + scale_x_discrete(labels = conditionsVDC)
  
  #5
  shortdf <- InvivoThreedayCVD[, colnames(InvivoThreedayCVD) %in% c("treatment", gastrocnemiuscolumn)]
  shortdf <- rescaledtovehicleasunity(shortdf)
  gastrocnemiusthreeplot <- threecolumnplot(shortdf, gastrocnemiuslabel, gastrocnemiusylim, gastrocnemiusthreestat)+
    theme(axis.text.x = element_text(color = "black")) + scale_x_discrete(labels = conditionsVDC)
  
  #6
  shortdf <- InvivoSevendayCVD[, colnames(InvivoSevendayCVD) %in% c("treatment", gastrocnemiuscolumn)]
  shortdf <- rescaledtovehicleasunity(shortdf)
  gastrocnemiussevenplot <- threecolumnplot(shortdf, gastrocnemiuslabel, gastrocnemiusylim, gastrocnemiussevenstat)+
    theme(axis.text.x = element_text(color = "black")) + scale_x_discrete(labels = conditionsVDC)
  
  #7
  shortdf <- InvivoOnedayCVD[, colnames(InvivoOnedayCVD) %in% c("treatment", tricepscolumn)]
  shortdf <- rescaledtovehicleasunity(shortdf)
  tricepsoneplot <- threecolumnplot(shortdf, tricepslabel, tricepsylim, tricepsonestat) 
  
  #8
  shortdf <- InvivoThreedayCVD[, colnames(InvivoThreedayCVD) %in% c("treatment", tricepscolumn)]
  shortdf <- rescaledtovehicleasunity(shortdf)
  tricepsthreeplot <- threecolumnplot(shortdf, tricepslabel, tricepsylim, tricepsthreestat)
  
  #9
  shortdf <- InvivoSevendayCVD[, colnames(InvivoSevendayCVD) %in% c("treatment", tricepscolumn)]
  shortdf <- rescaledtovehicleasunity(shortdf)
  tricepssevenplot <- threecolumnplot(shortdf, tricepslabel, tricepsylim, tricepssevenstat)
  
  return(grid.arrange(quadricepsoneplot, quadricepsthreeplot, quadricepssevenplot,
                      tricepsoneplot, tricepsthreeplot, tricepssevenplot,
                      #gastrocnemiusoneplot, gastrocnemiusthreeplot, gastrocnemiussevenplot,
                      ncol=3))
}


# SIXTH plot shows atrogenes in two muscles at three time points
plotatrogenes <- function(){
  quadricepsMafbxcolumn <- "quadriceps.Ct.Fbxo32....Ct.Gapdh."
  gastrocnemiusMafbxcolumn <- "gastrocnemius.Ct.Fbxo32....Ct.Gapdh."
  quadricepsMurfcolumn <- "quadriceps.Ct.Trim63....Ct.Gapdh."
  gastrocnemiusMurfcolumn <- "gastrocnemius.Ct.Trim63....Ct.Gapdh."
  
  quadricepsMafbxlabel <- "quadriceps\nFbxo32 mRNA"
  gastrocnemiusMafbxlabel <- "gastrocnemius\nFbxo32 mRNA"
  quadricepsMurflabel <- "quadriceps\nTrim63 mRNA"
  gastrocnemiusMurflabel <- "gastrocnemius\nTrim63 mRNA"
  
  quadricepsoneMafbxylim <- c(-1, 8.5)
  quadricepsthreeMafbxylim <- c(-1, 5.4)
  quadricepssevenMafbxylim <- c(-2, 2)
  gastrocnemiusoneMafbxylim <- c(-1, 2.5)
  gastrocnemiusthreeMafbxylim <- c(-1, 5.4)
  gastrocnemiussevenMafbxylim <- c(-2, 2)
  quadricepsoneMurfylim <- c(-1, 8.5)
  quadricepsthreeMurfylim <- c(-1, 5.4)
  quadricepssevenMurfylim <- c(-2, 2)
  gastrocnemiusoneMurfylim <- c(-1, 2.5)
  gastrocnemiusthreeMurfylim <- c(-1, 5.4)
  gastrocnemiussevenMurfylim <- c(-2, 2)
  
  gastrocnemiusoneMafbxstat <- c("a", "a,b", "b")
  gastrocnemiusoneMurfstat <- threeidenticalgroups
  quadricepsoneMafbxstat <- c("a", "b", "a,b")
  quadricepsoneMurfstat <- c("a", "b", "a,b")
  gastrocnemiusthreeMafbxstat <- threeidenticalgroups
  gastrocnemiusthreeMurfstat <- c("a", "b", "a,b")
  quadricepsthreeMafbxstat <- c("a,b", "a", "b")
  quadricepsthreeMurfstat <- c("a,b", "a", "b")
  gastrocnemiussevenMafbxstat <- threeidenticalgroups
  gastrocnemiussevenMurfstat <- threeidenticalgroups
  quadricepssevenMafbxstat <- threeidenticalgroups
  quadricepssevenMurfstat <- c("a", "a,b", "b")
  
  #1
  shortdf <- InvivoOnedayCVD[, colnames(InvivoOnedayCVD) %in% c("treatment", quadricepsMafbxcolumn)]
  shortdf <- rescaledtovehicleaszero(shortdf)
  quadricepsoneMafbxplot <- threegeneplot(shortdf, quadricepsMafbxlabel, quadricepsoneMafbxylim, quadricepsoneMafbxstat) +
    theme(axis.text.x = element_text(color = "black")) + scale_x_discrete(labels = conditionsVDC)
  
  #2
  shortdf <- InvivoThreedayCVD[, colnames(InvivoThreedayCVD) %in% c("treatment", quadricepsMafbxcolumn)]
  shortdf <- rescaledtovehicleaszero(shortdf)
  quadricepsthreeMafbxplot <- threegeneplot(shortdf, quadricepsMafbxlabel, quadricepsthreeMafbxylim, quadricepsthreeMafbxstat) +
    theme(axis.text.x = element_text(color = "black")) + scale_x_discrete(labels = conditionsVDC)
  
  #3
  shortdf <- InvivoSevendayCVD[, colnames(InvivoSevendayCVD) %in% c("treatment", quadricepsMafbxcolumn)]
  shortdf <- rescaledtovehicleaszero(shortdf)
  quadricepssevenMafbxplot <- threegeneplot(shortdf, quadricepsMafbxlabel, quadricepssevenMafbxylim, quadricepssevenMafbxstat) +
    theme(axis.text.x = element_text(color = "black")) + scale_x_discrete(labels = conditionsVDC)
  
  #4
  shortdf <- InvivoOnedayCVD[, colnames(InvivoOnedayCVD) %in% c("treatment", gastrocnemiusMafbxcolumn)]
  shortdf <- rescaledtovehicleaszero(shortdf)
  gastrocnemiusoneMafbxplot <- threegeneplot(shortdf, gastrocnemiusMafbxlabel, gastrocnemiusoneMafbxylim, gastrocnemiusoneMafbxstat)
  
  #5
  shortdf <- InvivoThreedayCVD[, colnames(InvivoThreedayCVD) %in% c("treatment", gastrocnemiusMafbxcolumn)]
  shortdf <- rescaledtovehicleaszero(shortdf)
  gastrocnemiusthreeMafbxplot <- threegeneplot(shortdf, gastrocnemiusMafbxlabel, gastrocnemiusthreeMafbxylim, gastrocnemiusthreeMafbxstat)
  
  #6
  shortdf <- InvivoSevendayCVD[, colnames(InvivoSevendayCVD) %in% c("treatment", gastrocnemiusMafbxcolumn)]
  shortdf <- rescaledtovehicleaszero(shortdf)
  gastrocnemiussevenMafbxplot <- threegeneplot(shortdf, gastrocnemiusMafbxlabel, gastrocnemiussevenMafbxylim, gastrocnemiussevenMafbxstat) 
  
  #Murf
  #7
  shortdf <- InvivoOnedayCVD[, colnames(InvivoOnedayCVD) %in% c("treatment", quadricepsMurfcolumn)]
  shortdf <- rescaledtovehicleaszero(shortdf)
  quadricepsoneMurfplot <- threegeneplot(shortdf, quadricepsMurflabel, quadricepsoneMurfylim, quadricepsoneMurfstat)
  
  #8
  shortdf <- InvivoThreedayCVD[, colnames(InvivoThreedayCVD) %in% c("treatment", quadricepsMurfcolumn)]
  shortdf <- rescaledtovehicleaszero(shortdf)
  quadricepsthreeMurfplot <- threegeneplot(shortdf, quadricepsMurflabel, quadricepsthreeMurfylim, quadricepsthreeMurfstat) 
  
  #9
  shortdf <- InvivoSevendayCVD[, colnames(InvivoSevendayCVD) %in% c("treatment", quadricepsMurfcolumn)]
  shortdf <- rescaledtovehicleaszero(shortdf)
  quadricepssevenMurfplot <- threegeneplot(shortdf, quadricepsMurflabel, quadricepssevenMurfylim, quadricepssevenMurfstat)
  
  #10
  shortdf <- InvivoOnedayCVD[, colnames(InvivoOnedayCVD) %in% c("treatment", gastrocnemiusMurfcolumn)]
  shortdf <- rescaledtovehicleaszero(shortdf)
  gastrocnemiusoneMurfplot <- threegeneplot(shortdf, gastrocnemiusMurflabel, gastrocnemiusoneMurfylim, gastrocnemiusoneMurfstat) +
    anotatedtitle("one day", 2, gastrocnemiusoneMurfylim[[2]])
  
  #11
  shortdf <- InvivoThreedayCVD[, colnames(InvivoThreedayCVD) %in% c("treatment", gastrocnemiusMurfcolumn)]
  shortdf <- rescaledtovehicleaszero(shortdf)
  gastrocnemiusthreeMurfplot <- threegeneplot(shortdf, gastrocnemiusMurflabel, gastrocnemiusthreeMurfylim, gastrocnemiusthreeMurfstat) +
    anotatedtitle("three days", 2, gastrocnemiusthreeMurfylim[[2]])
  
  #12
  shortdf <- InvivoSevendayCVD[, colnames(InvivoSevendayCVD) %in% c("treatment", gastrocnemiusMurfcolumn)]
  shortdf <- rescaledtovehicleaszero(shortdf)
  gastrocnemiussevenMurfplot <- threegeneplot(shortdf, gastrocnemiusMurflabel, gastrocnemiussevenMurfylim, gastrocnemiussevenMurfstat) +
    anotatedtitle("seven days", 2, quadricepssevenMafbxylim[[2]])
  
  return(grid.arrange(gastrocnemiusoneMurfplot, gastrocnemiusthreeMurfplot, gastrocnemiussevenMurfplot,
                      quadricepsoneMurfplot, quadricepsthreeMurfplot, quadricepssevenMurfplot,
                      gastrocnemiusoneMafbxplot, gastrocnemiusthreeMafbxplot, gastrocnemiussevenMafbxplot,
                      quadricepsoneMafbxplot, quadricepsthreeMafbxplot, quadricepssevenMafbxplot,
                      ncol=3))
}

plotmurf <- function(){
  shortdf <- InvivoThreedayCVD[, colnames(InvivoThreedayCVD) %in% c("treatment", "quadriceps.MuRF1.protein..normalized.to.GAPDH.")]
  
  shortdf <- 
  shortdf <- rescaledtovehicleasunity(shortdf)
  murfplot <- threecolumnplot(shortdf, "MuRF-1 protein\n(normalized to GAPDH)", c(0,1.5), c("a", "a", "a"))
  return(murfplot)
}
