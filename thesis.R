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
library(multcomp)
#font_import(prompt = FALSE)
options(bitmapType="cairo")

# DATA loading and mangling
codedirpath <- dirname(
  tryCatch(normalizePath(parent.frame(2)$ofile),  # works when using source
           error=function(e) # works when using R CMD
             normalizePath(unlist(strsplit(commandArgs()[
               grep('^--file=', commandArgs())], '='))[2])))
codedirpath <- "/media/dump/writingswork/draftthesis"
datadir <- normalizePath(file.path(codedirpath, "data"))
InvivoOneday <- read.csv(file.path(
  datadir, "2012.12.09.1dayTD.csv"), 
  header = TRUE)
InvivoOnedayNicenames <- read.table(
  file.path(datadir, "2012.12.09.1dayTD.csv"), 
  header = FALSE,  sep = ",", 
  nrows = 1, 
  stringsAsFactors = FALSE)
InvivoThreeday <- read.csv(
  file.path(datadir, "2012.12.12.3daysTD.csv"), 
  header = TRUE)
InvivoThreedayNicenames <- read.table(
  file.path(datadir, "2012.12.12.3daysTD.csv"), 
  header = FALSE, 
  sep = ",", 
  nrows = 1, 
  stringsAsFactors = FALSE)
InvivoSevenday <- read.csv(
  file.path(datadir, "2012.08.23.7daysTD.csv"), 
  header = TRUE)
InvivoSevendayNicenames <- read.table(
  file.path(datadir, "2012.08.23.7daysTD.csv"), 
  header = FALSE, 
  sep = ",", 
  nrows = 1, 
  stringsAsFactors = FALSE)
InvitroCelldiams <- read.csv(
  file.path(datadir, "2012.08.26.celldiameters.csv"), 
  header = TRUE)
SynthesisInCells <- read.csv(
  file.path(datadir, "2014.06.30.protein.synthesis.csv"), 
  header = TRUE)
colnames(SynthesisInCells)[colnames(SynthesisInCells)=="Condition"] = "treatment"
SynthesisInCells$cell_protein_density_microgram_per_cmsq_normalized_to_first_day <- NA
SynthesisInCells$cell_protein_density_microgram_per_cmsq_normalized_to_vehicle <- NA
DegradationInCells <- read.csv(
  file.path(datadir, "2014.06.30.protein.degradation.csv"), 
  header = TRUE)
colnames(DegradationInCells)[colnames(DegradationInCells)=="Condition"] = "treatment"
DegradationWithInhibitors <- read.csv(
  file.path(datadir, "2014.08.31.inhibitors.csv"),
  header = TRUE)
DegradationWithInhibitors <-DegradationWithInhibitors[
  !(DegradationWithInhibitors$treatment %in% c("VB", "DB", "DTB", "base", "DT", "DTP")), ]


#column additions
for (i in levels(SynthesisInCells$treatment)) {
  SynthesisInCells$cell_protein_density_microgram_per_cmsq_normalized_to_first_day[
    SynthesisInCells$treatment == as.character(i)]  <- 
    SynthesisInCells$cell_protein_density_microgram_per_cmsq[
      SynthesisInCells$treatment == as.character(i)] / 
    mean(SynthesisInCells$cell_protein_density_microgram_per_cmsq[
      (SynthesisInCells$TimeDays == 0 & 
         SynthesisInCells$treatment == as.character(i))])
}
for (i in 0:3) {
  SynthesisInCells$cell_protein_density_microgram_per_cmsq_normalized_to_vehicle[
    SynthesisInCells$TimeDays == as.character(i)]  <- 
    SynthesisInCells$cell_protein_density_microgram_per_cmsq[
      SynthesisInCells$TimeDays == as.character(i)] / 
    mean(SynthesisInCells$cell_protein_density_microgram_per_cmsq[
      (SynthesisInCells$TimeDays == as.character(i) & 
         SynthesisInCells$treatment == "V")])
}

#renaming of conditions
levels(InvivoOneday$treatment)[levels(InvivoOneday$treatment)=="A"] <- "C"
levels(InvivoOneday$treatment)[levels(InvivoOneday$treatment)=="E"] <- "D"
levels(InvivoOneday$treatment)[levels(InvivoOneday$treatment)=="W"] <- "V"
levels(InvivoOneday$treatment)[levels(InvivoOneday$treatment)=="S"] <- "T"
levels(InvivoThreeday$treatment)[levels(InvivoThreeday$treatment)=="B"] <- "C"
levels(InvivoThreeday$treatment)[levels(InvivoThreeday$treatment)=="G"] <- "D"
levels(InvivoThreeday$treatment)[levels(InvivoThreeday$treatment)=="X"] <- "V"
levels(InvivoThreeday$treatment)[levels(InvivoThreeday$treatment)=="U"] <- "T"

#frequently used sets of conditions
condsVDC <- c("V", "D", "C")
condsVDTC <- c("V", "D", "T", "C")
condsVDCdiameters <- c("V", "D", "DT")
condsVDCBdiameters <- c("V", "D", "DT", "DU") # DU is higher testosterone in cell diameter
condsVDCmetabolism <- c("V", "DD", "DDTT")
condsVDCAmetabolism <- c("V", "DD", "DDT", "DDTT")
condsVDCABmetabolism <- c("V", "D", "DD", "DDT", "DDTT")
condsVDCinhibitors <- c("V", "D", "DC", "DM", "DTT", "DTTP")
conditionsVDC <- c("Veh", "Dexa", "Comb")
conditionsVDCpresentation <- c("Veh", "Dexa", "Dexa + Testo")
conditionsVDCpresentationwrap <- c("Veh", "Dexa", "Dexa\n&Testo")
conditionsVDTC <- c("Veh", "Dexa", "Testo", "Comb")
conditionsVDCmetabolism <- c("Veh", "1 µM Dexa", "1 µM Dexa\n+ 500 nM T")
conditionsVDCAmetabolism <- c(
  "Veh", 
  "1 µM Dexa", 
  "1 µM Dexa\n+ 100 nM T", 
  "1 µM Dexa\n+ 500 nM T")
conditionsVDCABmetabolism <- c(
  "Veh", 
  "100 nM Dexa",
  "1 µM Dexa", 
  "1 µM Dexa\n+ 100 nM T", 
  "1 µM Dexa\n+ 500 nM T")
conditionsVDCinhibitors <- c(
  "Veh", 
  "100 nM Dexa",
  "100 nM Dexa\n+ 25 µM CHQ",
  "100 nM Dexa\n+ 5 µM MG132",
  "100 nM Dexa\n+ 300 nM T",
  "100 nM Dexa\n+ 300 nM T\n+ 50 nM PPP")
contraststhree <- c("V vs D", "V vs DT", "D vs DT")
contrastsfour <- c("V vs D", "V vs T", "D vs T", "V vs DT", "D vs DT", "T vs DT")
VvsDthreeways <- match("V vs D", contraststhree)[[1]]
DvsCthreeways <- match("D vs DT", contraststhree)[[1]]
VvsCthreeways <- match("V vs DT", contraststhree)[[1]]
VvsDfourways <- match("V vs D", contrastsfour)[[1]]
VvsTfourways <- match("V vs T", contrastsfour)[[1]]
DvsCfourways <- match("D vs DT", contrastsfour)[[1]]
#re-leveling
InvivoOneday$treatment <- factor(
  InvivoOneday$treatment,
  levels = condsVDTC)
InvivoThreeday$treatment <- factor(
  InvivoThreeday$treatment,
  levels = condsVDTC)
InvivoSevenday$treatment <- factor(
  InvivoSevenday$treatment,
  levels = condsVDTC)
InvitroCelldiams$treatment <- factor(
  InvitroCelldiams$treatment,
  levels = condsVDCBdiameters)
#subsetting for convenience
InvivoOnedayCVD <- InvivoOneday[InvivoOneday$treatment %in% condsVDC, ]
InvivoOnedayCVD$treatment <- factor(
  InvivoOnedayCVD$treatment, 
  levels = condsVDC)
InvivoOnedayV <- InvivoOneday[InvivoOneday$treatment == "V", ]
InvivoOnedayD <- InvivoOneday[InvivoOneday$treatment == "D", ]
InvivoOnedayC <- InvivoOneday[InvivoOneday$treatment == "C", ]
InvivoOnedayT <- InvivoOneday[InvivoOneday$treatment == "T", ]
InvivoThreedayCVD <- InvivoThreeday[InvivoThreeday$treatment %in% condsVDC, ]
InvivoThreedayCVD$treatment <- factor(
  InvivoThreedayCVD$treatment,
  levels = condsVDC)
InvivoThreedayV <- InvivoThreeday[InvivoThreeday$treatment == "V", ]
InvivoThreedayD <- InvivoThreeday[InvivoThreeday$treatment == "D", ]
InvivoThreedayC <- InvivoThreeday[InvivoThreeday$treatment == "C", ]
InvivoThreedayT <- InvivoThreeday[InvivoThreeday$treatment == "T", ]
InvivoSevendayCVD <- InvivoSevenday[InvivoSevenday$treatment %in% condsVDC, ]
InvivoSevendayCVD$treatment <- factor(
  InvivoSevendayCVD$treatment,
  levels = condsVDC)
InvivoSevendayV <- InvivoSevenday[InvivoSevenday$treatment == "V", ]
InvivoSevendayD <- InvivoSevenday[InvivoSevenday$treatment == "D", ]
InvivoSevendayC <- InvivoSevenday[InvivoSevenday$treatment == "C", ]
InvivoSevendayT <- InvivoSevenday[InvivoSevenday$treatment == "T", ]
InvitroCelldiamsCVD <-InvitroCelldiams[
  InvitroCelldiams$treatment %in% condsVDCdiameters , ]
InvitroCelldiamsCVD$treatment <- factor(
  InvitroCelldiamsCVD$treatment,
  levels = condsVDCdiameters)
SynthesisInCellsVDC <- SynthesisInCells[(
  SynthesisInCells$treatment %in% condsVDCmetabolism), ]
SynthesisInCellsVDC$treatment <- factor(
  SynthesisInCellsVDC$treatment, 
  levels = condsVDCmetabolism)
SynthesisInCellsVDCA <- SynthesisInCells[(
  SynthesisInCells$treatment %in% condsVDCAmetabolism), ]
SynthesisInCellsVDCA$treatment <- factor(
  SynthesisInCellsVDCA$treatment,
  levels = condsVDCAmetabolism)
SynthesisInCellsV <- SynthesisInCells[(SynthesisInCells$treatment == "V"), ]
SynthesisInCellsD <- SynthesisInCells[(SynthesisInCells$treatment == "DD"), ]
SynthesisInCellsC <- SynthesisInCells[(SynthesisInCells$treatment == "DDT"), ]
SynthesisInCellsA <- SynthesisInCells[(SynthesisInCells$treatment == "DDTT"), ]
SynthesisInCellsQ <- SynthesisInCells[(SynthesisInCells$treatment == "DDTTP"), ]
DegradationInCellsVDC <- DegradationInCells[(
  DegradationInCells$treatment %in% condsVDCmetabolism), ]
DegradationInCellsVDC$treatment <- factor(
  DegradationInCellsVDC$treatment,
  levels = condsVDCmetabolism)
DegradationInCellsVDCA <- DegradationInCells[(
  DegradationInCells$treatment %in% condsVDCAmetabolism), ]
DegradationInCellsVDCA$treatment <- factor(
  DegradationInCellsVDCA$treatment, 
  levels = condsVDCAmetabolism)
DegradationInCellsVDCAB <- DegradationInCells[(
  DegradationInCells$treatment %in% condsVDCABmetabolism), ]
DegradationInCellsVDCAB$treatment <- factor(
  DegradationInCellsVDCAB$treatment,
  levels = condsVDCABmetabolism)
DegradationInCellsV <- DegradationInCells[(DegradationInCells$treatment == "V"), ]
DegradationInCellsD <- DegradationInCells[(DegradationInCells$treatment == "DD"), ]
DegradationInCellsC <- DegradationInCells[(DegradationInCells$treatment == "DDT"), ]
DegradationInCellsA <- DegradationInCells[(DegradationInCells$treatment == "DDTT"), ]
DegradationInCellsQ <- DegradationInCells[(DegradationInCells$treatment == "DDTTP"), ]
Degradation6 <- DegradationInCells[(DegradationInCells$TimeDays == "0")  ,]
Degradation24 <- DegradationInCells[(DegradationInCells$TimeDays == "1")  ,]
Degradation48 <- DegradationInCells[(DegradationInCells$TimeDays == "2")  ,]
Degradation72 <- DegradationInCells[(DegradationInCells$TimeDays == "3")  ,]
DegradationWithInhibitors$treatment <- factor(
  DegradationWithInhibitors$treatment,
  levels = condsVDCinhibitors)

#literal constants
unistar <- sprintf('\u2736')
unidagger <- sprintf('\u2020')
threeemptystrings <- c("", "", "")
threespaces <- c(" ", " ", " ")
threetworowspaces <- c(" \n ", " ", " ")
threeidenticalgroups <- c("a", "a", "a")

#helper maths functions
SEM <- function(x) {
  return(sqrt(var(x, na.rm = TRUE) / truecount(x)))
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
  return(qt(.975, df = (truecount(x) - 1)) * SEM(x))
}

sig3 <- function(x) {
  return(format(x, digits = 3))
}

semInterval <- function(x) {
  lims <- c(
    mean(x, na.rm = TRUE) - SEM(x),
    mean(x, na.rm = TRUE) + SEM(x))
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
  return(length(na.omit(x)))
}

truemean <- function(x) {
  return(mean(x, na.rm = TRUE))
}

percentgrowth <- function(initial, final){
  return(100 * (truemean(final) - truemean(initial)) / truemean(initial))
}

pcramplificationrate <- function(initial, final){
  return(2^(truemean(initial) - truemean(final)))
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
presentationTextSize <- 12
nicepalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
meaningfulpalette <- c("#444444", "#dd0000", "#00dd00", "#0000dd", "#888800", "#880088", "#008888", "#dddddd")
greypalette <- c("#ffffff", "#222222", "#999999", "#0000dd", "#00dd00", "#dd0000", "#008888", "#888800")
presentationcolors <- c("grey25", "red3", "green3")
blankgrob <- rectGrob(gp = gpar(col = "white"))

stdplottimecourse <- theme_bw() + 
  theme(text = element_text(size = textSize, color = "black", family = "Liberation Sans Narrow"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        axis.title.x = element_text(size = textSize),
        axis.title.y = element_text(size = textSize),
        axis.text.x = element_text(size = textSize),
        axis.text.y = element_text(size = textSize),
        legend.title = element_blank())

presentationplottimecourse <- theme_bw() + 
  theme(text = element_text(size = presentationTextSize, color = "black", family = "Cabin"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        axis.title.x = element_text(size = presentationTextSize),
        axis.title.y = element_text(size = presentationTextSize),
        axis.text.x = element_text(size = presentationTextSize),
        axis.text.y = element_text(size = presentationTextSize),
        legend.title = element_blank(),
        legend.background = element_rect(fill = alpha('white', 1)))

stdbarplot <- 
  theme_bw() +
  theme(text = element_text(size = textSize, color = "black", family = "Liberation Sans Narrow"),
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
        axis.title.y = element_text(size = textSize))

presentationbarplot <- 
  theme_bw() +
  theme(text = element_text(size = presentationTextSize, color = "black", family = "Cabin"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(color = "black"),
        axis.ticks.x = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.title.y = element_text(size = presentationTextSize, angle = 0),
        axis.text.x = element_text(size = presentationTextSize * .7),
        axis.text.y = element_text(size = presentationTextSize * .7))

annotationastitle <- function(text, x, y) {
  return(annotate(geom = "text", label = text, x = x, y = y,
                  vjust = 1, hjust = 0.5,  
                  size = .45 * textSize, 
                  family = "Liberation Sans Narrow"))
}

annotationasaxis <- function(texts, xs, y) {
  return(
    annotate(geom = "text", label = texts[[1]], x = xs[[1]], y = y,
             vjust = 1, hjust = 0.5,  
             size = .2 * textSize, 
             family = "Liberation Sans Narrow") +
      annotate(geom = "text", label = texts[[2]], x = xs[[2]], y = y,
               vjust = 1, hjust = 0.5,  
               size = .2 * textSize, 
               family = "Liberation Sans Narrow") +
      annotate(geom = "text", label = texts[[3]], x = xs[[3]], y = y,
               vjust = 1, hjust = 0.5,  
               size = .2 * textSize, 
               family = "Liberation Sans Narrow"))
}

threecolumnplot <- function(skinnydataset, ylabel, ylimit, statstrings){
  #this function expects a dataframe with two columns, first designating the treatments
  completecasesdataset <- skinnydataset[complete.cases(skinnydataset),]
  return(
    ggplot(completecasesdataset) +
      aes_string(x = colnames(completecasesdataset)[1], 
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

threecolumnplotpresentation <- function(skinnydataset, ylabel, ylimit, statstrings){
  #this function expects a dataframe with two columns, first designating the treatments
  completecasesdataset <- skinnydataset[complete.cases(skinnydataset),]
  return(
    ggplot(completecasesdataset) +
      aes_string(x = colnames(completecasesdataset)[1], 
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
                   size = presentationTextSize * .4,
                   aes(family = "Cabin"),
                   fun.y = statstringyoverbar, 
                   hjust = .5,
                   vjust = -.6,
                   label = statstrings) +
      ylab(ylabel) +
      coord_cartesian(ylim = ylimit) + 
      scale_fill_manual(values = presentationcolors) +
      presentationbarplot)
}

threegeneplot <- function(skinnydataset, ylabel, ylimit, statstrings){
  # this function expects a dataframe with two columns, first designating the treatments
  # second column described Ct(GOI)-Ct(housekeeping gene)
  completecasesdataset <- skinnydataset[complete.cases(skinnydataset),]
  completecasesdataset[,2] <- completecasesdataset[,2] * (-1)
  return(
    ggplot(completecasesdataset) +
      aes_string(x = colnames(completecasesdataset)[1], 
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

plotthreetimecourses <- function(skinnydataset, ylimit, ylabel, statsstring){
  # This function expects a dataframe with three columns
  # - first column designates the treatments
  # - second column describes the time at which it was measured
  # - the name of the second column will become X axis label
  # - third column describes the actual numbers to plot.
  # statsstring can be a long vector of strings, showing: 
  # time1treatment1, time1treatment2, time1treatemnt3, time2treatement1 etc.
  completecasesdataset <- skinnydataset[complete.cases(skinnydataset),]
  return(
    ggplot(completecasesdataset) + 
      aes_string(x = colnames(completecasesdataset)[2],
                 y = colnames(completecasesdataset)[3],
                 group = colnames(completecasesdataset)[1]) +
      stat_summary(geom = "point",
                   size = 3,
                   fun.y = truemean,
                   position = position_dodge(.05),
                   aes_string(shape = colnames(completecasesdataset)[1])) +
      stat_summary(geom = "line", 
                   size = .5, 
                   fun.y = truemean, 
                   position = position_dodge(.05)) + 
      stat_summary(geom = "text", 
                   size = textSize * .4,
                   aes(family = "serif"),
                   fun.y = statstringyunderbar, 
                   hjust = -.2,
                   vjust = .25,
                   label = statsstring) + 
      stat_summary(geom = 'errorbar',
                   fun.data = 'semInterval',
                   width = 0.2,
                   show_guide = FALSE,
                   position=position_dodge(.05)) +
      coord_cartesian(ylim = ylimit) + 
      ylab(ylabel) +
      scale_shape_manual(values = c(16, 4, 1), labels = conditionsVDC) +
      stdplottimecourse)  
}

plotfourtimecourses <- function(skinnydataset, ylimit, ylabel, categories, statsstring){
  # This function expects a dataframe with three columns
  # - first column designates the treatments
  # - second column describes the time at which it was measured
  # - the name of the second column will become X axis label
  # - third column describes the actual numbers to plot.
  # statsstring can be a long vector of strings, showing: 
  # time1treatment1, time1treatment2, time1treatemnt3, time2treatement1 etc.
  completecasesdataset <- skinnydataset[complete.cases(skinnydataset),]
  return(
    ggplot(completecasesdataset) + 
      aes_string(x = colnames(completecasesdataset)[2],
                 y = colnames(completecasesdataset)[3],
                 group = colnames(completecasesdataset)[1]) +
      stat_summary(geom = "point",
                   size = 3,
                   fun.y = truemean,
                   position = position_dodge(.1),
                   aes_string(shape = "treatment", show_guide = FALSE)) +
      stat_summary(geom = "line", 
                   size = .5, 
                   fun.y = truemean, 
                   position = position_dodge(.1), show_guide = FALSE) + 
      #     stat_summary(geom = "text", 
      #                  size = textSize * .4,
      #                  aes(family = "serif"),
      #                  fun.y = statstringyunderbar, 
      #                  hjust = -.2,
      #                  vjust = .25,
      #                  label = statsstring) + 
      stat_summary(geom = 'errorbar',
                   fun.data = 'semInterval',
                   width = 0.2,
                   show_guide = FALSE,
                   position=position_dodge(.1)) +
      coord_cartesian(ylim = ylimit) + 
      ylab(ylabel) +
      scale_x_continuous(labels = c("1", "2", "3", "4")) +
      xlab("day") +
      stdplottimecourse)
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
        annotationastitle("one day", 2, ylimit[[2]]) + 
        theme(axis.text.x = element_text(color = "black")) + 
        scale_x_discrete(labels = conditionsVDC),
      threecolumnplot(InvivoThreedayCVD[, colnames(InvivoThreedayCVD) %in% c("treatment", "day.4.body.weight..g.")], 
                      ylabel, 
                      ylimit, 
                      threedayweightstat) +
        annotationastitle("three days", 2, ylimit[[2]]) + 
        theme(axis.text.x = element_text(color = "black")) +
        scale_x_discrete(labels = threeemptystrings),
      threecolumnplot(InvivoSevendayCVD[, colnames(InvivoSevendayCVD) %in% c("treatment", "day.8.body.weight..g.")], 
                      ylabel, 
                      ylimit, 
                      sevendayweightstat) +
        annotationastitle("seven days", 2, ylimit[[2]]) + 
        theme(axis.text.x = element_text(color = "black")) +
        scale_x_discrete(labels = threeemptystrings),
      ncol=3))
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
  topplot <- plotthreetimecourses(shortdf, 
                                  c(-7, 10), 
                                  "body weight gain (% of pre-treatment)", 
                                  statsstars)
  
  shortdf <- InvivoThreedayCVD[, colnames(InvivoThreedayCVD) %in% c("treatment", timeseriescolumns[1:4])]
  body.weight.gain.after.0.days..percent. <- rep(0, dim(shortdf)[1])
  shortdf <- cbind(body.weight.gain.after.0.days..percent., shortdf)
  shortdf <- melt(shortdf, id = c("treatment"), value.name = "bodyweight")
  colnames(shortdf)[2] <- "day"
  setattr(shortdf$day, "levels", 1:4)
  bottomleftplot <- plotthreetimecourses(shortdf, 
                                         c(-7, 10), 
                                         "body weight gain (% of pre-treatment)", 
                                         rep("", 12))
  
  shortdf <- InvivoOnedayCVD[, colnames(InvivoOnedayCVD) %in% c("treatment", timeseriescolumns[1:1])]
  body.weight.gain.after.0.days..percent. <- rep(0, dim(shortdf)[1])
  shortdf <- cbind(body.weight.gain.after.0.days..percent., shortdf)
  shortdf <- melt(shortdf, id = c("treatment"), value.name = "bodyweight")
  colnames(shortdf)[2] <- "day"
  setattr(shortdf$day, "levels", 1:2)
  bottomrightplot <- plotthreetimecourses(shortdf, 
                                          c(-7, 10), 
                                          "body weight gain (% of pre-treatment)", 
                                          rep("", 6))
  topplot$layers[[1]]$show_guide = FALSE
  bottomleftplot$layers[[1]]$show_guide = FALSE
  return(grid.arrange(topplot, arrangeGrob(bottomleftplot, bottomrightplot, ncol=2, widths = c(1.1,1)), 
                      ncol=1))
}

# THIRD PLOT - body mass composition
plotleanfat <- function(){
  #alphabetical order
  columnnames <- c("fat.mass.gain..g.", "lean.mass.gain..g.", "total.water.gain..g.")
  ylabels <- c("fat mass gain (g)", "lean mass loss (g)", "water loss (g)")
  ylims <- list(c(0, 4), c(0, 4.5), c(0, 5.2))
  statstrings <- list(
    #fat1, 3, 7:
    threeidenticalgroups,
    c("a", "a,b", "b"),
    c("a", "a,b", "b"),
    #lean1, 3, 7:
    threeidenticalgroups, 
    c("a", "b", "a,b"),
    c("a", "b", "a,b"),
    #water1, 3, 7:
    threeidenticalgroups,
    threeidenticalgroups,
    threeidenticalgroups)
  plotslist <- list()
  for (i in 1:3) {
    shortdf1 <- InvivoOnedayCVD[, colnames(InvivoOnedayCVD) %in% c("treatment", columnnames[[i]])]
    shortdf3 <- InvivoThreedayCVD[, colnames(InvivoThreedayCVD) %in% c("treatment", columnnames[[i]])]
    shortdf7 <- InvivoSevendayCVD[, colnames(InvivoSevendayCVD) %in% c("treatment", columnnames[[i]])]
    if (columnnames[[i]] != "fat.mass.gain..g.") {
      shortdf1[,2] <- shortdf1[, 2] * (-1)
      shortdf3[,2] <- shortdf3[, 2] * (-1)
      shortdf7[,2] <- shortdf7[, 2] * (-1)
    }
    plotslist[[i*3-2]] <- threecolumnplot(shortdf1, ylabels[[i]], ylims[[i]], statstrings[[(i*3-2)]])
    plotslist[[i*3-1]] <- threecolumnplot(shortdf3, ylabels[[i]], ylims[[i]], statstrings[[(i*3-1)]])
    plotslist[[i*3]] <- threecolumnplot(shortdf7, ylabels[[i]], ylims[[i]], statstrings[[(i*3)]])
    if (columnnames[[i]] == "total.water.gain..g.") {
      plotslist[[i*3-2]] <- plotslist[[i*3-2]] + 
        annotationastitle("one day", 2, ylims[[i]][[2]])
      plotslist[[i*3-1]] <- plotslist[[i*3-1]] + 
        annotationastitle("three days", 2, ylims[[i]][[2]])
      plotslist[[i*3]] <- plotslist[[i*3]] + 
        annotationastitle("seven days", 2, ylims[[i]][[2]])
    }
    if (columnnames[[i]] == "lean.mass.gain..g.") {
      plotslist[[i*3-2]] <- plotslist[[i*3-2]] + 
        theme(axis.text.x = element_text(color = "black")) + 
        scale_x_discrete(labels = conditionsVDC)
      plotslist[[i*3-1]] <- plotslist[[i*3-1]] + 
        theme(axis.text.x = element_text(color = "black")) + 
        scale_x_discrete(labels = conditionsVDC)
      plotslist[[i*3]] <- plotslist[[i*3]] + 
        theme(axis.text.x = element_text(color = "black")) + 
        scale_x_discrete(labels = conditionsVDC)
    }
  }
  return(grid.arrange(
    plotslist[[7]], plotslist[[8]], plotslist[[9]], # water
    plotslist[[1]], plotslist[[2]], plotslist[[3]], # fat
    plotslist[[4]], plotslist[[5]], plotslist[[6]], # lean
    ncol = 3))
}


# FOURTH PLOT SHOWS five muscles across three time points
plotmuscleweights <- function(){
  #alphabetical order
  columnnames <- c("gastrocnemius..mg.",
                   "levator..mg.",
                   "quadriceps..mg.",
                   "tibialis..mg.",
                   "triceps..mg.")
  ylabels <- c("gastrocnemius (mg)",
               "levator ani (mg)",
               "quadriceps (mg)",
               "tibialis ant. (mg)",
               "triceps br. (mg)")
  ylims <- list(c(0, 180),
                c(0, 100), 
                c(0, 225),
                c(0, 75),
                c(0, 150))
  statstrings <- list(
    #gastrocnemia1,3,7:
    threeidenticalgroups,
    c("a", "b", "a,b"),
    c("a", "b", "a,b"),
    #levators1,3,7:
    threeidenticalgroups,
    threeidenticalgroups,
    c("a", "b", "a,b"),
    #quadriceps:
    threeidenticalgroups,
    threeidenticalgroups,
    c("a", "b", "a,b"),
    #tibialis:
    threeidenticalgroups,
    threeidenticalgroups,
    threeidenticalgroups,
    #triceps:
    threeidenticalgroups,
    threeidenticalgroups,
    c("a", "b", "b")
  )
  plotslist <- list()
  for (i in 1:5){
    shortdf1 <- InvivoOnedayCVD[, colnames(InvivoOnedayCVD) %in% c("treatment", columnnames[[i]])]
    shortdf3 <- InvivoThreedayCVD[, colnames(InvivoThreedayCVD) %in% c("treatment", columnnames[[i]])]
    shortdf7 <- InvivoSevendayCVD[, colnames(InvivoSevendayCVD) %in% c("treatment", columnnames[[i]])]
    plotslist[[i*3-2]] <- threecolumnplot(shortdf1, ylabels[[i]], ylims[[i]], statstrings[[(i*3-2)]])
    plotslist[[i*3-1]] <- threecolumnplot(shortdf3, ylabels[[i]], ylims[[i]], statstrings[[(i*3-1)]])
    plotslist[[i*3]] <- threecolumnplot(shortdf7, ylabels[[i]], ylims[[i]], statstrings[[(i*3)]])
    if (columnnames[[i]] == "levator..mg.") {
      plotslist[[i*3-2]] <- plotslist[[i*3-2]] +
        annotationastitle("one day", 2, ylims[[i]][[2]])
      plotslist[[i*3-1]] <- plotslist[[i*3-1]] + 
        annotationastitle("three days", 2, ylims[[i]][[2]])
      plotslist[[i*3]] <- plotslist[[i*3]] + 
        annotationastitle("seven days", 2, ylims[[i]][[2]])
    }
    if (columnnames[[i]] == "tibialis..mg.") {
      plotslist[[i*3-2]] <- plotslist[[i*3-2]] + 
        theme(axis.text.x = element_text(color = "black")) + 
        scale_x_discrete(labels = conditionsVDC)
      plotslist[[i*3-1]] <- plotslist[[i*3-1]] + 
        theme(axis.text.x = element_text(color = "black")) + 
        scale_x_discrete(labels = conditionsVDC)
      plotslist[[i*3]] <- plotslist[[i*3]] + 
        theme(axis.text.x = element_text(color = "black")) + 
        scale_x_discrete(labels = conditionsVDC)
    }
  }
  return(grid.arrange(
    plotslist[[4]], plotslist[[5]], plotslist[[6]], # levator
    plotslist[[7]], plotslist[[8]], plotslist[[9]], # quadriceps
    plotslist[[1]], plotslist[[2]], plotslist[[3]], # gastrocnemius
    plotslist[[13]], plotslist[[14]], plotslist[[15]], # triceps
    plotslist[[10]], plotslist[[11]], plotslist[[12]], # tibialis
    ncol = 3))
}

# FIFTH plot shows proteasome activity in three muscles at three time points
plotproteasomeactivity <- function(){
  #alphabetical order
  columnnames <- c("gastrocnemius.proteasome.activity..rel.u..",
                   "quadriceps.proteasome.activity..rel.u..",
                   "triceps.proteasome.activity..rel.u..")
  ylabels <- c("gastrocnemius proteasome\nactivity (rel.u.)",
               "quadriceps proteasome\nactivity (rel.u.)",
               "triceps proteasome\nactivity (rel.u.)")
  ylims <- list(c(0, 2),
                c(0, 1.8), 
                c(0, 3))
  statstrings <- list(
    #gastrocnemius 1, 3, 7:
    c("a,b", "a", "b"),
    c("a", "b", "b"),
    threeidenticalgroups,
    #quadriceps:
    threeidenticalgroups,
    threeidenticalgroups,
    threeidenticalgroups,
    #triceps:
    threeidenticalgroups,
    threeidenticalgroups,
    threeidenticalgroups)
  
  plotslist <- list()
  for (i in 1:3){
    shortdf1 <- rescaledtovehicleasunity(
      InvivoOnedayCVD[, colnames(InvivoOnedayCVD) %in% c("treatment", columnnames[[i]])])
    shortdf3 <- rescaledtovehicleasunity(
      InvivoThreedayCVD[, colnames(InvivoThreedayCVD) %in% c("treatment", columnnames[[i]])])
    shortdf7 <- rescaledtovehicleasunity(
      InvivoSevendayCVD[, colnames(InvivoSevendayCVD) %in% c("treatment", columnnames[[i]])])
    plotslist[[i*3-2]] <- threecolumnplot(shortdf1, ylabels[[i]], ylims[[i]], statstrings[[(i*3-2)]])
    plotslist[[i*3-1]] <- threecolumnplot(shortdf3, ylabels[[i]], ylims[[i]], statstrings[[(i*3-1)]])
    plotslist[[i*3]] <- threecolumnplot(shortdf7, ylabels[[i]], ylims[[i]], statstrings[[(i*3)]])
    if (columnnames[[i]] == "quadriceps.proteasome.activity..rel.u..") {
      plotslist[[i*3-2]] <- plotslist[[i*3-2]] + 
        annotationastitle("one day", 2, ylims[[i]][[2]])
      plotslist[[i*3-1]] <- plotslist[[i*3-1]] +
        annotationastitle("three days", 2, ylims[[i]][[2]])
      plotslist[[i*3]] <- plotslist[[i*3]] +
        annotationastitle("seven days", 2, ylims[[i]][[2]])
    }
    if (columnnames[[i]] == "triceps.proteasome.activity..rel.u..") {
      plotslist[[i*3-2]] <- plotslist[[i*3-2]] + 
        theme(axis.text.x = element_text(color = "black")) + 
        scale_x_discrete(labels = conditionsVDC)
      plotslist[[i*3-1]] <- plotslist[[i*3-1]] + 
        theme(axis.text.x = element_text(color = "black")) + 
        scale_x_discrete(labels = conditionsVDC)
      plotslist[[i*3]] <- plotslist[[i*3]] + 
        theme(axis.text.x = element_text(color = "black")) + 
        scale_x_discrete(labels = conditionsVDC)
    }
  }
  return(grid.arrange(
    #plotslist[[1]], plotslist[[2]], plotslist[[3]], # gastrocnemius
    plotslist[[4]], plotslist[[5]], plotslist[[6]], # quadriceps
    plotslist[[7]], plotslist[[8]], plotslist[[9]], # triceps
    ncol = 3))
}


plotmurf <- function(){
  shortdf <- rescaledtovehicleasunity(
    InvivoThreedayCVD[, colnames(InvivoThreedayCVD) %in% c("treatment", "quadriceps.MuRF1.protein..normalized.to.GAPDH.")])
  murfplot <- threecolumnplot(shortdf, "MuRF-1 protein\n(normalized to GAPDH)", c(0,1.5), c("a", "a", "a"))
  return(murfplot)
}


# SIXTH plot shows atrogenes in two muscles at three time points
plotatrogenes <- function(){
  #alphabetical order
  columnnames <- c(
    "gastrocnemius.Ct.Fbxo32....Ct.Gapdh.",
    "quadriceps.Ct.Fbxo32....Ct.Gapdh.",
    "gastrocnemius.Ct.Trim63....Ct.Gapdh.",
    "quadriceps.Ct.Trim63....Ct.Gapdh.")
  ylabels <- c(
    "gastrocnemius\nFbxo32 mRNA",
    "quadriceps\nFbxo32 mRNA",
    "gastrocnemius\nTrim63 mRNA",
    "quadriceps\nTrim63 mRNA")
  ylims <- list(
    # gastrocnemius - Fbxo32 / Mafbx: 1, 3, 7:
    c(-1, 2.5), c(-1.5, 5.4), c(-2, 2),
    # quadriceps - Fbxo32 / Mafbx: 1, 3, 7:
    c(-1, 8.5), c(-1.5, 5.4), c(-2, 2),
    # gastrocnemius - Trim63 / Murf1: 1, 3, 7:
    c(-1, 2.5), c(-1.5, 5.4), c(-2, 2),
    # quadriceps - Trim63 / Murf1: 1, 3, 7:
    c(-1, 8.5), c(-1.5, 5.4), c(-2, 2))
  
  statstrings <- list(
    # gastrocnemius - Fbxo32 / Mafbx: 1, 3, 7:
    c("a", "a,b", "b"),
    threeidenticalgroups,
    threeidenticalgroups,
    # quadriceps - Fbxo32 / Mafbx: 1, 3, 7:
    c("a", "b", "a,b"),
    c("a,b", "a", "b"),
    threeidenticalgroups,
    # gastrocnemius - Trim63 / Murf1: 1, 3, 7:
    threeidenticalgroups,
    c("a", "b", "a,b"),
    threeidenticalgroups,
    # quadriceps - Trim63 / Murf1: 1, 3, 7:
    c("a", "b", "a,b"),
    c("a,b", "a", "b"),
    c("a", "a,b", "b"))
  
  plotslist <- list()
  for (i in 1:4){
    shortdf1 <- rescaledtovehicleaszero(
      InvivoOnedayCVD[, colnames(InvivoOnedayCVD) %in% c("treatment", columnnames[[i]])])
    shortdf3 <- rescaledtovehicleaszero(
      InvivoThreedayCVD[, colnames(InvivoThreedayCVD) %in% c("treatment", columnnames[[i]])])
    shortdf7 <- rescaledtovehicleaszero(
      InvivoSevendayCVD[, colnames(InvivoSevendayCVD) %in% c("treatment", columnnames[[i]])])
    plotslist[[i*3-2]] <- threegeneplot(shortdf1, ylabels[[i]], ylims[[(i*3-2)]], statstrings[[(i*3-2)]])
    plotslist[[i*3-1]] <- threegeneplot(shortdf3, ylabels[[i]], ylims[[(i*3-1)]], statstrings[[(i*3-1)]])
    plotslist[[i*3]] <- threegeneplot(shortdf7, ylabels[[i]], ylims[[(i*3)]], statstrings[[(i*3)]])
    if (columnnames[[i]] == "gastrocnemius.Ct.Trim63....Ct.Gapdh.") {
      plotslist[[i*3-2]] <- plotslist[[i*3-2]] + 
        annotationastitle("one day", 2, ylims[[(i*3-2)]][[2]])
      plotslist[[i*3-1]] <- plotslist[[i*3-1]] + 
        annotationastitle("three days", 2, ylims[[(i*3-1)]][[2]])
      plotslist[[i*3]] <- plotslist[[i*3]] + 
        annotationastitle("seven days", 2, ylims[[(i*3)]][[2]])
    }
    if (columnnames[[i]] == "quadriceps.Ct.Fbxo32....Ct.Gapdh.") {
      plotslist[[i*3-2]] <- plotslist[[i*3-2]] + 
        theme(axis.text.x = element_text(color = "black")) + 
        scale_x_discrete(labels = conditionsVDC)
      plotslist[[i*3-1]] <- plotslist[[i*3-1]] + 
        theme(axis.text.x = element_text(color = "black")) + 
        scale_x_discrete(labels = conditionsVDC)
      plotslist[[i*3]] <- plotslist[[i*3]] + 
        theme(axis.text.x = element_text(color = "black")) + 
        scale_x_discrete(labels = conditionsVDC)
    }
  }
  return(grid.arrange(
    plotslist[[7]], plotslist[[8]], plotslist[[9]], # gastrocnemius MuRF1
    plotslist[[10]], plotslist[[11]], plotslist[[12]], # quadriceps MuRF1
    plotslist[[1]], plotslist[[2]], plotslist[[3]], # gastrocnemius MAFbx
    plotslist[[4]], plotslist[[5]], plotslist[[6]], # quadriceps MAFbx
    ncol = 3))
}


# SEVENTH plot shows cathepsin activity in three muscles at three time points
plotcathepsinactivity <- function(){
  #alphabetical order
  columnnames <- c("gastrocnemius.cathepsin.activity..rel.u..",
                   "quadriceps.cathepsin.activity..rel.u..",
                   "triceps.cathepsin.activity..rel.u..")
  ylabels <- c("gastrocnemius cathepsin\nactivity (rel.u.)",
               "quadriceps cathepsin\nactivity (rel.u.)",
               "triceps cathepsin\nactivity (rel.u.)")
  ylims <- c(0, 1.5)
  statstrings <- list(
    #gastrocnemius 1, 3, 7:
    c("a", "b", "a,b"),
    threeidenticalgroups,
    c("a", "b", "b"),
    #quadriceps:
    c("a", "b", "a"),
    c("a", "b", "a,b"),
    c("a", "b", "b"),
    #triceps:
    c("a", "b", "a,b"),
    c("a", "b", "a,b"),
    c("a", "b", "b"))
  
  plotslist <- list()
  for (i in 1:3){
    shortdf1 <- rescaledtovehicleasunity(
      InvivoOnedayCVD[, colnames(InvivoOnedayCVD) %in% c("treatment", columnnames[[i]])])
    shortdf3 <- rescaledtovehicleasunity(
      InvivoThreedayCVD[, colnames(InvivoThreedayCVD) %in% c("treatment", columnnames[[i]])])
    shortdf7 <- rescaledtovehicleasunity(
      InvivoSevendayCVD[, colnames(InvivoSevendayCVD) %in% c("treatment", columnnames[[i]])])
    plotslist[[i*3-2]] <- threecolumnplot(shortdf1, ylabels[[i]], ylims, statstrings[[(i*3-2)]])
    plotslist[[i*3-1]] <- threecolumnplot(shortdf3, ylabels[[i]], ylims, statstrings[[(i*3-1)]])
    plotslist[[i*3]] <- threecolumnplot(shortdf7, ylabels[[i]], ylims, statstrings[[(i*3)]])
    if (columnnames[[i]] == "gastrocnemius.cathepsin.activity..rel.u..") {
      plotslist[[i*3-2]] <- plotslist[[i*3-2]] + 
        annotationastitle("one day", 2, ylims[[2]])
      plotslist[[i*3-1]] <- plotslist[[i*3-1]] + 
        annotationastitle("three days", 2, ylims[[2]])
      plotslist[[i*3]] <- plotslist[[i*3]] + 
        annotationastitle("seven days", 2, ylims[[2]])
    }
    if (columnnames[[i]] == "triceps.cathepsin.activity..rel.u..") {
      plotslist[[i*3-2]] <- plotslist[[i*3-2]] + 
        theme(axis.text.x = element_text(color = "black")) + 
        scale_x_discrete(labels = conditionsVDC)
      plotslist[[i*3-1]] <- plotslist[[i*3-1]] + 
        theme(axis.text.x = element_text(color = "black")) + 
        scale_x_discrete(labels = conditionsVDC)
      plotslist[[i*3]] <- plotslist[[i*3]] + 
        theme(axis.text.x = element_text(color = "black")) + 
        scale_x_discrete(labels = conditionsVDC)
    }
  }
  return(grid.arrange(
    plotslist[[1]], plotslist[[2]], plotslist[[3]], # gastrocnemius
    plotslist[[4]], plotslist[[5]], plotslist[[6]], # quadriceps
    plotslist[[7]], plotslist[[8]], plotslist[[9]], # triceps
    ncol = 3))
}

#EIGHTH plot shows gastrocnemius genes associated with autophagy - three genes
plotgastrocnemiusautophagy <- function(){
  #alphabetical order
  columnnames <- c(
    "gastrocnemius.Ct.Becn1....Ct.Gapdh.",
    "gastrocnemius.Ct.Ctsl....Ct.Gapdh.",
    "gastrocnemius.Ct.Map1lc3b....Ct.Gapdh.")
  ylabels <- c(
    "Becn1 mRNA",
    "Ctsl mRNA",
    "Map1lc3b mRNA")
  ylims <- list(
    # Becn1: 1, 3, 7:
    c(-3.5, 3), c(-3.5, 3), c(-3.5, 3),
    # Ctsl: 1, 3, 7:
    c(-3, 2.5), c(-3, 2.5), c(-3, 2.5),
    # Map1lc3b: 1, 3, 7:
    c(-3.5, 2.5), c(-3.5, 2.5), c(-3.5, 2.5))
  statstrings <- list(
    # Becn1: 1, 3, 7:
    threeidenticalgroups,
    c("a", "b", "a,b"),
    c("a", "a,b", "b"),
    # Ctsl: 1, 3, 7:
    threeidenticalgroups,
    threeidenticalgroups,
    threeidenticalgroups,
    # Map1lc3b: 1, 3, 7:
    threeidenticalgroups,
    threeidenticalgroups,
    threeidenticalgroups)
  
  plotslist <- list()
  for (i in 1:3){
    shortdf1 <- rescaledtovehicleaszero(
      InvivoOnedayCVD[, colnames(InvivoOnedayCVD) %in% c("treatment", columnnames[[i]])])
    shortdf3 <- rescaledtovehicleaszero(
      InvivoThreedayCVD[, colnames(InvivoThreedayCVD) %in% c("treatment", columnnames[[i]])])
    shortdf7 <- rescaledtovehicleaszero(
      InvivoSevendayCVD[, colnames(InvivoSevendayCVD) %in% c("treatment", columnnames[[i]])])
    plotslist[[i*3-2]] <- threegeneplot(shortdf1, ylabels[[i]], ylims[[(i*3-2)]], statstrings[[(i*3-2)]])
    plotslist[[i*3-1]] <- threegeneplot(shortdf3, ylabels[[i]], ylims[[(i*3-1)]], statstrings[[(i*3-1)]])
    plotslist[[i*3]] <- threegeneplot(shortdf7, ylabels[[i]], ylims[[(i*3)]], statstrings[[(i*3)]])
    if (columnnames[[i]] == "gastrocnemius.Ct.Becn1....Ct.Gapdh.") {
      plotslist[[i*3-2]] <- plotslist[[i*3-2]] + annotationastitle("one day", 2, ylims[[(i*3-2)]][[2]])
      plotslist[[i*3-1]] <- plotslist[[i*3-1]] + annotationastitle("three days", 2, ylims[[(i*3-1)]][[2]])
      plotslist[[i*3]] <- plotslist[[i*3]] + annotationastitle("seven days", 2, ylims[[(i*3)]][[2]])
    }
    if (columnnames[[i]] == "gastrocnemius.Ct.Map1lc3b....Ct.Gapdh.") {
      plotslist[[i*3-2]] <- plotslist[[i*3-2]] + 
        theme(axis.text.x = element_text(color = "black")) + 
        scale_x_discrete(labels = conditionsVDC)
      plotslist[[i*3-1]] <- plotslist[[i*3-1]] + 
        theme(axis.text.x = element_text(color = "black")) + 
        scale_x_discrete(labels = conditionsVDC)
      plotslist[[i*3]] <- plotslist[[i*3]] + 
        theme(axis.text.x = element_text(color = "black")) + 
        scale_x_discrete(labels = conditionsVDC)
    }
  }
  return(grid.arrange(
    plotslist[[1]], plotslist[[2]], plotslist[[3]], # Becn1
    plotslist[[4]], plotslist[[5]], plotslist[[6]], # Ctsl
    plotslist[[7]], plotslist[[8]], plotslist[[9]], # Map1lc3b
    ncol = 3))
}


#NINTH plot shows densitometry of LC3
plotlcprotein <- function(){
  columnnames <- c(
    "levator.LC3.II..normalized.to.GAPDH.",
    "levator.LC3.II...LC.I",
    "gastrocnemius.LC3.II..normalized.to.GAPDH.",
    "gastrocnemius.LC3.II...LC.I")
  ylabels <- c(
    "LC3-II protein normalized to GAPDH",
    "LC3-II protein normalized to LC3-I",
    "LC3-II protein normalized to GAPDH",
    "LC3-II protein normalized to LC3-I")
  ylims <- list(
    c(0,8),
    c(0,20),
    c(0,3),
    c(0,3)
  )
  statstrings <- list(
    threeidenticalgroups,
    threeidenticalgroups,
    threeidenticalgroups,
    threeidenticalgroups)
  
  plotslist <- list()
  for (i in 1:4){
    shortdf1 <- rescaledtovehicleasunity(
      InvivoSevendayCVD[, colnames(InvivoSevendayCVD) %in% c("treatment", columnnames[[i]])])
    plotslist[[i]] <- threecolumnplot(shortdf1, ylabels[[i]], ylims[[i]], statstrings[[i]])
  }
  
  return(grid.arrange(
    #plotslist[[1]],
    #plotslist[[2]], 
    plotslist[[3]],
    plotslist[[4]],
    ncol = 2))
}

# TENTH plot shows cathepsin activity in three muscles at three time points
plotcalpainactivity <- function(){
  #alphabetical order
  columnname <- "gastrocnemius.calpain.activity..rel.u.."
  ylabel <- "gastrocnemius calpain\nactivity (rel.u.)"
  ylims <- c(0, 2)
  statstrings <- list(
    #gastrocnemius 1, 3, 7:
    c("a", "b", "b"),
    threeidenticalgroups,
    threeidenticalgroups)
  plotslist <- list()
  shortdf1 <- rescaledtovehicleasunity(
    InvivoOnedayCVD[, colnames(InvivoOnedayCVD) %in% c("treatment", columnname)])
  shortdf3 <- rescaledtovehicleasunity(
    InvivoThreedayCVD[, colnames(InvivoThreedayCVD) %in% c("treatment", columnname)])
  shortdf7 <- rescaledtovehicleasunity(
    InvivoSevendayCVD[, colnames(InvivoSevendayCVD) %in% c("treatment", columnname)])
  plotslist[[1]] <- threecolumnplot(shortdf1, ylabel, ylims, statstrings[[1]])+ 
    theme(axis.text.x = element_text(color = "black")) + 
    scale_x_discrete(labels = conditionsVDC) + 
    annotationastitle("one day", 2, ylims)
  plotslist[[2]] <- threecolumnplot(shortdf3, ylabel, ylims, statstrings[[2]])+ 
    theme(axis.text.x = element_text(color = "black")) + 
    scale_x_discrete(labels = conditionsVDC) + 
    annotationastitle("three days", 2, ylims)
  plotslist[[3]] <- threecolumnplot(shortdf7, ylabel, ylims, statstrings[[3]])+ 
    theme(axis.text.x = element_text(color = "black")) + 
    scale_x_discrete(labels = conditionsVDC) + 
    annotationastitle("seven days", 2, ylims)
  return(grid.arrange(
    plotslist[[1]], plotslist[[2]], plotslist[[3]], 
    ncol = 3))
}


#ELEVENTH plot
plotfoxogene <- function(){
  columnnames <- c(
    "quadriceps.Ct.Foxo3a....Ct.Gapdh.",
    "quadriceps.Ct.Klf15....Ct.Gapdh.")
  ylabels <- c(
    "Foxo3a mRNA",
    "Klf15 mRNA")
  ylims <- list(
    #     # quadriceps Foxo1 - days 1, 3, 7:
    #     c(-1,10),
    #     c(-1.5,3.75),
    #     c(-4,2),
    # quadriceps Foxo3 - days 1, 3, 7:
    c(-1,10),
    c(-1.5,3.75),
    c(-4,2),
    #     # quadriceps Foxo4 - days 1, 3, 7:
    #     c(-1,10),
    #     c(-1.5,3.75),
    #     c(-4,2),
    # quadriceps Klf15 - days 1, 3, 7:
    c(-1,6),
    c(-1,3.75),
    c(-4,2))
  statstrings <- list(
    #     # quadriceps Foxo1 - days 1, 3, 7:
    #     c("a", "b", "a,b"),
    #     c("a", "b", "a,b"),
    #     c("a", "a,b", "b"),
    # quadriceps Foxo3a - days 1, 3, 7:
    c("a", "b", "a,b"),
    threeidenticalgroups,
    c("a", "b", "a,b"),
    #     # quadriceps Foxo4 - days 1, 3, 7:
    #     c("a", "b", "a,b"),
    #     threeidenticalgroups,
    #     c("a", "b", "a,b"),
    # quadriceps Klf15 - days 1, 3, 7:
    c("a", "b", "a,b"),
    c("a,b", "a", "b"),
    c("a", "a,b", "b"))
  
  plotslist <- list()
  for (i in 1:2){
    shortdf1 <- rescaledtovehicleaszero(
      InvivoOnedayCVD[, colnames(InvivoOnedayCVD) %in% c("treatment", columnnames[[i]])])
    shortdf3 <- rescaledtovehicleaszero(
      InvivoThreedayCVD[, colnames(InvivoThreedayCVD) %in% c("treatment", columnnames[[i]])])
    shortdf7 <- rescaledtovehicleaszero(
      InvivoSevendayCVD[, colnames(InvivoSevendayCVD) %in% c("treatment", columnnames[[i]])])
    plotslist[[i*3-2]] <- threegeneplot(shortdf1, ylabels[[i]], ylims[[(i*3-2)]], statstrings[[(i*3-2)]])
    plotslist[[i*3-1]] <- threegeneplot(shortdf3, ylabels[[i]], ylims[[(i*3-1)]], statstrings[[(i*3-1)]])
    plotslist[[i*3]] <- threegeneplot(shortdf7, ylabels[[i]], ylims[[(i*3)]], statstrings[[(i*3)]])
  }
  
  plotslist[[1]] <- plotslist[[1]] + annotationastitle("one day", 2, ylims[[1]][[2]])
  plotslist[[2]] <- plotslist[[2]] + annotationastitle("three days", 2, ylims[[2]][[2]])
  plotslist[[3]] <- plotslist[[3]] + annotationastitle("seven days", 2, ylims[[3]][[2]])
  
  plotslist[[4]] <- plotslist[[4]] + 
    theme(axis.text.x = element_text(color = "black")) + 
    scale_x_discrete(labels = conditionsVDC)
  plotslist[[85]] <- plotslist[[5]] + 
    theme(axis.text.x = element_text(color = "black")) + 
    scale_x_discrete(labels = conditionsVDC)
  plotslist[[6]] <- plotslist[[6]] + 
    theme(axis.text.x = element_text(color = "black")) + 
    scale_x_discrete(labels = conditionsVDC)
  
  return(grid.arrange(
    plotslist[[1]], plotslist[[2]], plotslist[[3]],
    plotslist[[4]], plotslist[[5]], plotslist[[6]],
    ncol = 3))
}

ploteiftwo <- function(){
  shortdf7 <- rescaledtovehicleasunity(
    InvivoSevendayCVD[, colnames(InvivoSevendayCVD) %in% c("treatment", "gastrocnemius.phospho.eIF2alpha..normalized.to.GAPDH.")])
  return(threecolumnplot(shortdf7, "phospho-eIF2alpha\n(normalized to GAPDH)", c(0,1.5), threeidenticalgroups)+ 
           theme(axis.text.x = element_text(color = "black")) + 
           scale_x_discrete(labels = conditionsVDC))
}

ploteifthree <- function(){
  shortdf7 <- rescaledtovehicleasunity(
    InvivoSevendayCVD[, colnames(InvivoSevendayCVD) %in% c("treatment", "levator.eIF3f.protein..normalized.to.GAPDH.")])
  return(threecolumnplot(shortdf7, "eIF3f\n(normalized to GAPDH)", c(0,1.5), threeidenticalgroups)+ 
           theme(axis.text.x = element_text(color = "black")) + 
           scale_x_discrete(labels = conditionsVDC))
}

plotfourebp <- function(){
  totalfourebpdf <- rescaledtovehicleasunity(
    InvivoSevendayCVD[, colnames(InvivoSevendayCVD) %in% c("treatment", "gastrocnemius.4EBP.protein..normalized.to.GAPDH.")])
  phosphofourebpdf <- rescaledtovehicleasunity(
    InvivoSevendayCVD[, colnames(InvivoSevendayCVD) %in% c("treatment", "gastrocnemius.phospho.4EBP..normalized.to.GAPDH.")])
  return(grid.arrange(
    threecolumnplot(totalfourebpdf, "total4EBP\n(normalized to GAPDH)", c(0,2), threeidenticalgroups) + 
      theme(axis.text.x = element_text(color = "black")) + 
      scale_x_discrete(labels = conditionsVDC),
    threecolumnplot(phosphofourebpdf, "phospho 4EBP\n(normalized to GAPDH)", c(0,2), threeidenticalgroups) + 
      theme(axis.text.x = element_text(color = "black")) + 
      scale_x_discrete(labels = conditionsVDC),
    ncol = 2))
}

plotlevatorakt <- function(){
  return(grid.arrange(
    threecolumnplot(rescaledtovehicleasunity(InvivoSevendayCVD[, colnames(InvivoSevendayCVD) %in% c("treatment", "levator.Akt.protein..normalized.to.GAPDH.")]), "levator total Akt", c(0,2),threeidenticalgroups) + 
      theme(axis.text.x = element_text(color = "black")) + 
      scale_x_discrete(labels = conditionsVDC),
    threecolumnplot(rescaledtovehicleasunity(InvivoSevendayCVD[, colnames(InvivoSevendayCVD) %in% c("treatment", "levator.phospho.Akt.Ser473..normalized.to.GAPDH.")]), "levator phospho-Ser473 Akt", c(0,2),c("a", "a,b", "b")) + 
      theme(axis.text.x = element_text(color = "black")) + 
      scale_x_discrete(labels = conditionsVDC),
    threecolumnplot(rescaledtovehicleasunity(InvivoSevendayCVD[, colnames(InvivoSevendayCVD) %in% c("treatment", "levator.phospho.Akt.Ser473...total.Akt")]), "levator phospho/total Akt", c(0,2),threeidenticalgroups) + 
      theme(axis.text.x = element_text(color = "black")) + 
      scale_x_discrete(labels = conditionsVDC),
    ncol = 3))
}

plotgastrocnemiusakt <- function(){
  return(grid.arrange(
    threecolumnplot(rescaledtovehicleasunity(InvivoSevendayCVD[, colnames(InvivoSevendayCVD) %in% c("treatment", "gastrocnemius.Akt.protein..normalized.to.GAPDH.")]), "gastrocnemius total Akt", c(0,2),threeidenticalgroups) + 
      theme(axis.text.x = element_text(color = "black")) + 
      scale_x_discrete(labels = conditionsVDC),
    threecolumnplot(rescaledtovehicleasunity(InvivoSevendayCVD[, colnames(InvivoSevendayCVD) %in% c("treatment", "gastrocnemius.phospho.Akt.Ser473..normalized.to.GAPDH.")]), "gastrocnemius phospho-Ser473 Akt", c(0,2),threeidenticalgroups) + 
      theme(axis.text.x = element_text(color = "black")) + 
      scale_x_discrete(labels = conditionsVDC),
    threecolumnplot(rescaledtovehicleasunity(InvivoSevendayCVD[, colnames(InvivoSevendayCVD) %in% c("treatment", "gastrocnemius.phospho.Akt.Ser473...total.Akt")]), "gastrocnemius phospho/total Akt", c(0,2), c("a", "a,b", "b")) + 
      theme(axis.text.x = element_text(color = "black")) + 
      scale_x_discrete(labels = conditionsVDC),
    ncol = 3))
}

plotIgfr <-function(){
  return(grid.arrange(
    #     threecolumnplot(rescaledtovehicleasunity(InvivoSevendayCVD[, colnames(InvivoSevendayCVD) %in% c("treatment", "levator.phospho.IGF1R..normalized.to.GAPDH.")]), "levator.phospho.IGF1R", c(0,2),threeemptystrings),
    #     threecolumnplot(rescaledtovehicleasunity(InvivoSevendayCVD[, colnames(InvivoSevendayCVD) %in% c("treatment", "levator.IGF1R.protein..normalized.to.GAPDH.")]), "levator.IGF1R.protein.", c(0,2),threeemptystrings),
    #     threecolumnplot(rescaledtovehicleasunity(InvivoSevendayCVD[, colnames(InvivoSevendayCVD) %in% c("treatment", "levator.phospho.IGF1R...total.IGF1R")]), "levator.phosph/total.IGF1R", c(0,2),threeemptystrings),
    #     threegeneplot(rescaledtovehicleaszero(InvivoSevendayCVD[, colnames(InvivoSevendayCVD) %in% c("treatment", "gastrocnemius.Ct.Igf1r....Ct.Gapdh.")]), "gastrocnemius IGF1R mRNA", c(-1.8,.7),threeemptystrings) + 
    #       theme(axis.text.x = element_text(color = "black")) + 
    #       scale_x_discrete(labels = conditionsVDC),
    threecolumnplot(rescaledtovehicleasunity(InvivoSevendayCVD[, colnames(InvivoSevendayCVD) %in% c("treatment", "gastrocnemius.IGF1R.protein..normalized.to.GAPDH.")]), "gastrocnemius IGF1R protein", c(0,2),threeemptystrings) + 
      theme(axis.text.x = element_text(color = "black")) + 
      scale_x_discrete(labels = conditionsVDC),
    threecolumnplot(rescaledtovehicleasunity(InvivoSevendayCVD[, colnames(InvivoSevendayCVD) %in% c("treatment", "gastrocnemius.phospho.IGF1R..normalized.to.GAPDH.")]), "gastrocnemius phospho-IGF1R", c(0,2),threeemptystrings) + 
      theme(axis.text.x = element_text(color = "black")) + 
      scale_x_discrete(labels = conditionsVDC),
    #     threecolumnplot(rescaledtovehicleasunity(InvivoSevendayCVD[, colnames(InvivoSevendayCVD) %in% c("treatment", "gastrocnemius.phospho.IGF1R...total.IGF1R")]), "gastrocnemius phospho/total IGF1R", c(0,2),threeemptystrings) + 
    #       theme(axis.text.x = element_text(color = "black")) + 
    #       scale_x_discrete(labels = conditionsVDC),
    ncol = 2))
}

plotIgf <- function(){
  return(grid.arrange(
    threegeneplot(rescaledtovehicleaszero(InvivoOnedayCVD[, colnames(InvivoOnedayCVD) %in% c("treatment", "quadriceps.Ct.Igf1....Ct.Gapdh.")]), "quadriceps Igf1 mRNA", c(-3,3), c("a", "a,b", "b")) +
      annotationastitle("one day", 2, 3),
    threegeneplot(rescaledtovehicleaszero(InvivoThreedayCVD[, colnames(InvivoThreedayCVD) %in% c("treatment", "quadriceps.Ct.Igf1....Ct.Gapdh.")]), "quadriceps Igf1 mRNA", c(-3,3), c("a", "a,b", "b")) +
      annotationastitle("three days", 2, 3),
    threegeneplot(rescaledtovehicleaszero(InvivoSevendayCVD[, colnames(InvivoSevendayCVD) %in% c("treatment", "quadriceps.Ct.Igf1....Ct.Gapdh.")]), "quadriceps Igf1 mRNA", c(-3,3), c("a", "b", "a,b")) +
      annotationastitle("seven days", 2, 3),
    threegeneplot(rescaledtovehicleaszero(InvivoOnedayCVD[, colnames(InvivoOnedayCVD) %in% c("treatment", "gastrocnemius.Ct.Igf1....Ct.Gapdh.")]), "gastrocnemius Igf1 mRNA", c(-3,3), c("a", "b", "a,b")) + 
      theme(axis.text.x = element_text(color = "black")) + 
      scale_x_discrete(labels = conditionsVDC),
    threegeneplot(rescaledtovehicleaszero(InvivoThreedayCVD[, colnames(InvivoThreedayCVD) %in% c("treatment", "gastrocnemius.Ct.Igf1....Ct.Gapdh.")]), "gastrocnemius Igf1 mRNA", c(-3,3), c("a", "b", "a,b")) + 
      theme(axis.text.x = element_text(color = "black")) + 
      scale_x_discrete(labels = conditionsVDC),
    threegeneplot(rescaledtovehicleaszero(InvivoSevendayCVD[, colnames(InvivoSevendayCVD) %in% c("treatment", "gastrocnemius.Ct.Igf1....Ct.Gapdh.")]), "gastrocnemius Igf1 mRNA", c(-3,3), c("a", "b", "a,b")) + 
      theme(axis.text.x = element_text(color = "black")) + 
      scale_x_discrete(labels = conditionsVDC),
    ncol = 3))
}

plotredd <- function(){
  return(grid.arrange(
    threegeneplot(
      rescaledtovehicleaszero(
        InvivoOnedayCVD[, colnames(InvivoOnedayCVD) %in% 
                          c("treatment", "gastrocnemius.Ct.Ddit4....Ct.Gapdh.")]), 
      "gastrocnemius Ddit4 mRNA", 
      c(-.9,7.5), 
      c("a", "b", "b")) + 
      theme(axis.text.x = element_text(color = "black")) + 
      scale_x_discrete(labels = conditionsVDC) +
      annotationastitle("one day", 2, 7.5),
    threegeneplot(
      rescaledtovehicleaszero(
        InvivoThreedayCVD[, colnames(InvivoThreedayCVD) %in% 
                            c("treatment", "gastrocnemius.Ct.Ddit4....Ct.Gapdh.")]),
      "gastrocnemius Ddit4 mRNA", 
      c(-.9,7.5),
      c("a", "a,b", "b")) + 
      theme(axis.text.x = element_text(color = "black")) + 
      scale_x_discrete(labels = conditionsVDC) +
      annotationastitle("three days", 2, 7.5),
    threegeneplot(
      rescaledtovehicleaszero(
        InvivoSevendayCVD[, colnames(InvivoSevendayCVD) %in% 
                            c("treatment", "gastrocnemius.Ct.Ddit4....Ct.Gapdh.")]),
      "gastrocnemius Ddit4 mRNA", 
      c(-.9,7.5),
      threeidenticalgroups) + 
      theme(axis.text.x = element_text(color = "black")) + 
      scale_x_discrete(labels = conditionsVDC) +
      annotationastitle("seven days", 2, 7.5),
    ncol = 3))
}

# In vitro cell diameters
plotcelldiams <- function() {
  return(threecolumnplot(rescaledtovehicleasunity(
    InvitroCelldiamsCVD[, colnames(InvitroCelldiamsCVD) %in% 
                          c("treatment", "mean")]), 
    "mean diameter (rel. u.)", 
    c(0, 1.2), 
    c("a", "b", "a")) + 
      theme(axis.text.x = element_text(color = "black")) + 
      scale_x_discrete(labels = conditionsVDC))
}

anovaDiameters <- 
  aov(mean ~ treatment, data = InvitroCelldiamsCVD)
tukeyDiameters <-
  data.frame(TukeyHSD(x=anovaDiameters, 'treatment', conf.level=0.95)$treatment)

printdiameterstats <-function() {
  print("Diameters normality test:")
  print(shapiro.test(InvitroCelldiams$mean))
  print("Diameters ANOVA:")
  print(summary(anovaDiameters))
  print("Diameters Tukey post-hoc:")
  print(tukeyDiameters)
}

#protein synthesis and accretion in C2C12
plottotalprotein <- function(){
  unnormalizeddata <- 
    SynthesisInCellsVDCA[, 
                         colnames(SynthesisInCellsVDCA) %in% c(
                           "treatment", 
                           "TimeDays", 
                           "cell_protein_density_microgram_per_cmsq")]
  unnormalizeddata$treatment <- factor(
    unnormalizeddata$treatment,
    levels = condsVDCAmetabolism)
  normalizeddata <- 
    SynthesisInCellsVDCA[, 
                         colnames(SynthesisInCellsVDCA) %in% c(
                           "treatment", 
                           "TimeDays", 
                           "cell_protein_density_microgram_per_cmsq_normalized_to_first_day")]
  normalizeddata$treatment <- factor(
    normalizeddata$treatment,
    levels = condsVDCAmetabolism)
  return(grid.arrange(
    plotfourtimecourses(
      unnormalizeddata,  
      c(65, 90), 
      "total protein density\n(µg / cm2)",
      conditionsVDCAcells,
      c("", "", "", "",
        "", "", "", "",
        "", "", "", "",
        "", "", "", "")) + 
      scale_shape_manual(
        values = c(16, 4, 1, 13), 
        labels = conditionsVDCAmetabolism) +
      theme(
        legend.position = c(0, 0), 
        legend.justification = c(0, 0), 
        legend.direction = "horizontal"),
    plotfourtimecourses(
      normalizeddata,  
      c(0.83, 1.13), 
      "total protein density\n(normalized to initial time point)",
      conditionsVDCAcells,
      c("", "", "", "",
        "", "", "", "",
        "", "", "", "",
        "", "", "", "")) +
      scale_shape_manual(
        values = c(16, 4, 1, 13), 
        labels = conditionsVDCAmetabolism, 
        guide = "none") +
      scale_y_continuous(breaks = ((18:22*.05)), labels = percent),
    ncol=1))
}

anovaProteinDensityTreatmentAndDate <- aov(
  cell_protein_density_microgram_per_cmsq ~ TimeDays + treatment, 
  data = SynthesisInCellsVDCA)
anovaPvaluesProteinDensityTreatmentAndDate <-
  summary(anovaProteinDensityTreatmentAndDate)[[1]]$`Pr(>F)`

anovaProteinDensityTreatmentAndDateVonlyOneToThree <- aov(
  cell_protein_density_microgram_per_cmsq ~ TimeDays, 
  data = SynthesisInCellsV[SynthesisInCellsV$TimeDays != "3", ])
anovaPvaluesProteinDensityTreatmentAndDateVonlyOneToThree <- 
  summary(anovaProteinDensityTreatmentAndDateVonlyOneToThree)[[1]]$`Pr(>F)`

anovaProteinDensityTreatmentAndDateDonlyOneToThree <- aov(
  cell_protein_density_microgram_per_cmsq ~ TimeDays, 
  data = SynthesisInCellsV[SynthesisInCellsD$TimeDays != "3", ])
anovaPvaluesProteinDensityTreatmentAndDateDonlyOneToThree <- 
  summary(anovaProteinDensityTreatmentAndDateDonlyOneToThree)[[1]]$`Pr(>F)`

anovaProteinDensityTreatmentAndDateConlyOneToThree <- aov(
  cell_protein_density_microgram_per_cmsq ~ TimeDays, 
  data = SynthesisInCellsV[SynthesisInCellsC$TimeDays != "3", ])
anovaPvaluesProteinDensityTreatmentAndDateConlyOneToThree <- 
  summary(anovaProteinDensityTreatmentAndDateConlyOneToThree)[[1]]$`Pr(>F)`

anovaProteinDensityTreatmentAndDateAonlyOneToThree <- aov(
  cell_protein_density_microgram_per_cmsq ~ TimeDays, 
  data = SynthesisInCellsV[SynthesisInCellsA$TimeDays != "3", ])
anovaPvaluesProteinDensityTreatmentAndDateAonlyOneToThree <- 
  summary(anovaProteinDensityTreatmentAndDateAonlyOneToThree)[[1]]$`Pr(>F)`

# anovaProteinDensityTreatmentAndDateNormalizedToFirst <- 
#   summary(
#     aov(
#       cell_protein_density_microgram_per_cmsq_normalized_to_first_day ~ TimeDays + treatment, 
#       data = SynthesisInCellsVDCA))[[1]]$`Pr(>F)`

anovaSynthesisTreatmentAndDateActivityPerWell <- aov(
  activity_in_cell_protein_extract_picoCi ~ TimeDays + treatment, 
  data = SynthesisInCellsVDC)
anovaPvaluesSynthesisTreatmentAndDateActivityPerWell <- 
  summary(anovaSynthesisTreatmentAndDateActivityPerWell)[[1]]$`Pr(>F)`

plotproteinsynthesis <- function() {
  unnormalizeddata <- 
    SynthesisInCellsVDC[, 
                        colnames(SynthesisInCellsVDC) %in% c(
                          "treatment", 
                          "TimeDays", 
                          "activity_in_cell_protein_extract_picoCi")]
  unnormalizeddata$treatment <- factor(
    unnormalizeddata$treatment,
    levels = condsVDCmetabolism)
  normalizeddata <- SynthesisInCellsVDC[, 
                                        colnames(SynthesisInCellsVDC) %in% c(
                                          "treatment", 
                                          "TimeDays", 
                                          "femtomol_radio_Phe_per_g_cell_protein")]
  normalizeddata$treatment <- factor(
    normalizeddata$treatment,
    levels = condsVDCmetabolism)
  return(grid.arrange(
    plotthreetimecourses(
      unnormalizeddata,  
      c(1100, 1600), 
      "activity in cell protein\nextract (pCi/well)",
      c("", "", "", "",
        "", "", "", "",
        "", "", "", "")) + 
      scale_shape_manual(
        values = c(16, 4, 1), 
        labels = conditionsVDCmetabolism) +
      theme(legend.position = c(0, 0), 
            legend.justification = c(0, 0), 
            legend.direction = "horizontal") +
      scale_x_continuous(labels = c("1", "2", "3", "4")) +
      xlab("day"),
    plotthreetimecourses(
      normalizeddata,  
      c(7, 11), 
      "tracer enrichment in cell\nprotein (fmol/g)",
      c("", "", "", "",
        "", "", "", "",
        "", "", "", "")) +
      scale_shape_manual(
        values = c(16, 4, 1), 
        labels = conditionsVDCmetabolism,
        guide = "none")+
      scale_x_continuous(labels = c("1", "2", "3", "4")) +
      xlab("day"),
    ncol=1))
}

anovaDegradationTreatmentAndDate <- aov(
  curie_ratio_protein_depleted_medium_over_cell_protein ~ TimeDays + treatment, 
  data = DegradationInCellsVDCA)
anovaPvaluesDegradationTreatmentAndDate <- 
  summary(anovaDegradationTreatmentAndDate)[[1]]$`Pr(>F)`

anovaDegradationTreatment24 <- aov(
  curie_ratio_protein_depleted_medium_over_cell_protein ~ treatment, 
  data = DegradationInCellsVDCA[DegradationInCellsVDCA$TimeDays == "1", ])
anovaPvaluesDegradationTreatment24 <- 
  summary(anovaDegradationTreatment24)[[1]]$`Pr(>F)`
tukeyDegradationTreatment24 <- 
  data.frame(TukeyHSD(
    x = anovaDegradationTreatment24, 
    'treatment', 
    conf.level=0.95)$treatment)

anovaDegradationTreatment48 <- aov(
  curie_ratio_protein_depleted_medium_over_cell_protein ~ treatment, 
  data = DegradationInCellsVDCA[DegradationInCellsVDCA$TimeDays == "2", ])
anovaPvaluesDegradationTreatment48 <- 
  summary(anovaDegradationTreatment48)[[1]]$`Pr(>F)`
tukeyDegradationTreatment48 <- 
  data.frame(TukeyHSD(
    x = anovaDegradationTreatment48, 
    'treatment', 
    conf.level=0.95)$treatment)

plotproteindegradation <- function(){
  degradationtimecourse <- 
    DegradationInCells[ 
      (DegradationInCells$treatment %in% condsVDCAmetabolism), 
      colnames(DegradationInCells) %in% c(
        "treatment", 
        "TimeDays", 
        "curie_ratio_protein_depleted_medium_over_cell_protein")]
  degradationtimecourse$treatment <- 
    factor(
      degradationtimecourse$treatment, 
      levels = condsVDCAmetabolism)
  degradationtimecourse24 <- 
    DegradationInCells[
      (DegradationInCells$TimeDays == "1" &
         DegradationInCells$treatment %in% condsVDCAmetabolism)  ,
      colnames(DegradationInCells) %in% c(
        "treatment", 
        "curie_ratio_protein_depleted_medium_over_cell_protein")]
  degradationtimecourse24$treatment <- 
    factor(
      degradationtimecourse24$treatment, 
      levels = condsVDCAmetabolism)
  return(grid.arrange(
    ggplot(degradationtimecourse) + 
      aes_string(x = colnames(degradationtimecourse)[2],
                 y = colnames(degradationtimecourse)[3],
                 group = colnames(degradationtimecourse)[1]) +
      stat_summary(geom = "point",
                   size = 3,
                   fun.y = truemean,
                   position = position_dodge(.2),
                   aes_string(shape = colnames(degradationtimecourse)[1])) +
      stat_summary(geom = "line", 
                   size = .5, 
                   fun.y = truemean, 
                   position = position_dodge(.2)) +  
      stat_summary(geom = 'errorbar',
                   fun.data = 'semInterval',
                   width = 0.2,
                   show_guide = FALSE,
                   position=position_dodge(.2))  + 
      stat_summary(geom = "text", 
                   size = textSize * .4,
                   aes(family = "serif"),
                   fun.y = statstringyunderbar, 
                   hjust = -.2,
                   vjust = .25,
                   label = "") +
      coord_cartesian(ylim = c(0, 1)) + 
      ylab("medium to cell protein tracer ratio") +
      stdplottimecourse +
      scale_shape_manual(
        values = c(16, 4, 1, 9), 
        labels = conditionsVDCAmetabolism) +
      theme(
        legend.position = c(1,1),
        legend.justification = c(1, 1),
        legend.direction = "horizontal") +
      scale_x_continuous(labels = c("6", "24", "48", "72")) +
      xlab("hours"),
    ggplot(degradationtimecourse24) +
      aes_string(x = colnames(degradationtimecourse24)[1], 
                 y = colnames(degradationtimecourse24)[2], 
                 fill = colnames(degradationtimecourse24)[1]) +
      stat_summary(fun.y = truemean, 
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
                   label = c("a", "a", "a", "a")) +
      ylab("medium to cell protein tracer\nratio (24 hour)") +
      coord_cartesian(ylim = c(0, 1.1)) + 
      scale_fill_manual(values = c("#ffffff", "#222222", "#777777", "#dddddd"),
                        labels = conditionsVDCAmetabolism) +
      stdbarplot + 
      theme(axis.text.x = element_text(color = "black")) + 
      scale_x_discrete(labels = conditionsVDCAmetabolism),
    ncol = 1))
}

plotinhibitors <- function() {
  return(ggplot(DegradationWithInhibitors) +
           aes_string(x = "treatment", 
                      y = "curie_ratio_protein_depleted_medium_over_cell_protein", 
                      fill = "treatment") +
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
                        label = "") +
           ylab("medium to cell protein tracer\nratio (24 hour)") +
           coord_cartesian(ylim = c(0, 1.9)) + 
           scale_fill_manual(values = c(
             "#ffffff", 
             "#222222", 
             "#777777", 
             "#dddddd", 
             "#555555", 
             "#777777"),
             labels = conditionsVDCinhibitors) +
           stat_summary(geom = "text", 
                        size = textSize * .4,
                        aes(family = "Liberation Sans Narrow"),
                        fun.y = statstringyoverbar, 
                        hjust = .5,
                        vjust = -.6,
                        label = c("a", "b", "a", "c", "a", "a")) +
           stdbarplot + 
           theme(axis.text.x = element_text(color = "black")) + 
           scale_x_discrete(labels = conditionsVDCinhibitors))
}


anovaInhibitors <- aov(
  curie_ratio_protein_depleted_medium_over_cell_protein ~ treatment, 
  data = DegradationWithInhibitors)
anovaPvaluesInhibitors <- 
  summary(anovaInhibitors)[[1]]$`Pr(>F)`
tukeyInhibitors <- 
  data.frame(TukeyHSD(
    x = anovaInhibitors, 
    'treatment', 
    conf.level=0.95)$treatment)

presentationbodyweightcourse <- function(){
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
  completecasesdataset <- shortdf[complete.cases(shortdf),]
  return(ggplot(completecasesdataset) + 
           aes_string(x = colnames(completecasesdataset)[2],
                      y = colnames(completecasesdataset)[3],
                      group = colnames(completecasesdataset)[1]) +
           stat_summary(geom = "point",
                        size = 3,
                        fun.y = truemean,
                        position = position_dodge(.1),
                        aes_string(shape = colnames(completecasesdataset)[1], 
                                   color = colnames(completecasesdataset)[1])) +
           stat_summary(geom = "line", 
                        size = 2, 
                        fun.y = truemean, 
                        position = position_dodge(.1),
                        aes_string(color = colnames(completecasesdataset)[1])) + 
           stat_summary(geom = "text", 
                        size = presentationTextSize * .4,
                        aes(family = "serif"),
                        fun.y = statstringyunderbar, 
                        hjust = -.2,
                        vjust = .25,
                        label = c("", "", "",
                                  "", paste0(unidagger, unistar),"",
                                  "", unidagger,"",
                                  "", unidagger,"",
                                  "", unidagger,"",
                                  "", unidagger,"",
                                  "", unidagger,"",
                                  "", paste0(unidagger, unistar),"")) + 
           stat_summary(geom = 'errorbar',
                        fun.data = 'semInterval',
                        width = 1,
                        show_guide = FALSE,
                        position=position_dodge(.1),
                        aes_string(color = colnames(completecasesdataset)[1])) +
           coord_cartesian(ylim = c(-7, 10)) + 
           ylab("") +
           scale_shape_manual(values = c(16, 4, 1), labels = conditionsVDCpresentation) +
           scale_color_manual(values = presentationcolors, labels = conditionsVDCpresentation) +
           presentationplottimecourse +
           theme(axis.title.y = element_text(size = presentationTextSize, angle = 0)))
}
#svg("weight time course.svg", width = 9, height = 5); presentationbodyweightcourse(); dev.off()

presentationbodyweights <- function(){
  ylabel <- "body\nweight\n(percent\ngain)"
  ylimit <- c(0, 12)
  onedayweightstat <- threeidenticalgroups
  threedayweightstat <- threeidenticalgroups
  sevendayweightstat <- c("a", "b", "a")
  return(grid.arrange(
    threecolumnplotpresentation(InvivoOnedayCVD[, colnames(InvivoOnedayCVD) %in% c("treatment", "body.weight.gain.after.1.days..percent.")], 
                                "", 
                                ylimit, 
                                onedayweightstat) +
      annotationastitle("one day", 2, ylimit[[2]]) + 
      theme(axis.text.x = element_text(color = "black")) + 
      scale_x_discrete(labels = conditionsVDCpresentationwrap),
    threecolumnplotpresentation(InvivoThreedayCVD[, colnames(InvivoThreedayCVD) %in% c("treatment", "body.weight.gain.after.3.days..percent.")], 
                                "", 
                                ylimit, 
                                threedayweightstat) +
      annotationastitle("three days", 2, ylimit[[2]]) + 
      theme(axis.text.x = element_text(color = "black")) +
      scale_x_discrete(labels = threetworowspaces),
    threecolumnplotpresentation(InvivoSevendayCVD[, colnames(InvivoSevendayCVD) %in% c("treatment", "body.weight.gain.after.7.days..percent.")], 
                                "", 
                                ylimit, 
                                sevendayweightstat) +
      annotationastitle("seven days", 2, ylimit[[2]]) + 
      theme(axis.text.x = element_text(color = "black")) +
      scale_x_discrete(labels = threetworowspaces),
    ncol=3))
}
#svg("weights.svg", width = 9, height = 2); presentationbodyweights(); dev.off()

plotleanfatpresentation <- function(){
  #alphabetical order
  columnnames <- c("fat.mass.gain..g.", "lean.mass.gain..g.", "total.water.gain..g.")
  ylabels <- c("fat mass\ngain (g)", "lean mass\nloss (g)", "water loss (g)")
  ylabels <- c("", "", "")
  ylims <- list(c(0, 4), c(0, 4.5), c(0, 5.2))
  statstrings <- list(
    #fat1, 3, 7:
    threeidenticalgroups,
    c("a", "a,b", "b"),
    c("a", "a,b", "b"),
    #lean1, 3, 7:
    threeidenticalgroups, 
    c("a", "b", "a,b"),
    c("a", "b", "a,b"),
    #water1, 3, 7:
    threeidenticalgroups,
    threeidenticalgroups,
    threeidenticalgroups)
  plotslist <- list()
  for (i in 1:3) {
    shortdf1 <- InvivoOnedayCVD[, colnames(InvivoOnedayCVD) %in% c("treatment", columnnames[[i]])]
    shortdf3 <- InvivoThreedayCVD[, colnames(InvivoThreedayCVD) %in% c("treatment", columnnames[[i]])]
    shortdf7 <- InvivoSevendayCVD[, colnames(InvivoSevendayCVD) %in% c("treatment", columnnames[[i]])]
    if (columnnames[[i]] != "fat.mass.gain..g.") {
      shortdf1[,2] <- shortdf1[, 2] * (-1)
      shortdf3[,2] <- shortdf3[, 2] * (-1)
      shortdf7[,2] <- shortdf7[, 2] * (-1)
    }
    plotslist[[i*3-2]] <- threecolumnplotpresentation(shortdf1, ylabels[[i]], ylims[[i]], statstrings[[(i*3-2)]])
    plotslist[[i*3-1]] <- threecolumnplotpresentation(shortdf3, "", ylims[[i]], statstrings[[(i*3-1)]])
    plotslist[[i*3]] <- threecolumnplotpresentation(shortdf7, "", ylims[[i]], statstrings[[(i*3)]])
    if (columnnames[[i]] == "fat.mass.gain..g.") {
      plotslist[[i*3-2]] <- plotslist[[i*3-2]] + 
        annotationastitle("one day", 2, ylims[[i]][[2]])
      plotslist[[i*3-1]] <- plotslist[[i*3-1]] + 
        annotationastitle("three days", 2, ylims[[i]][[2]])
      plotslist[[i*3]] <- plotslist[[i*3]] + 
        annotationastitle("seven days", 2, ylims[[i]][[2]])
    }
    if (columnnames[[i]] == "lean.mass.gain..g.") {
      plotslist[[i*3-2]] <- plotslist[[i*3-2]] + 
        theme(axis.text.x = element_text(color = "black")) + 
        scale_x_discrete(labels = threetworowspaces)
      plotslist[[i*3-1]] <- plotslist[[i*3-1]] + 
        theme(axis.text.x = element_text(color = "black")) + 
        scale_x_discrete(labels = threetworowspaces)
      plotslist[[i*3]] <- plotslist[[i*3]] + 
        theme(axis.text.x = element_text(color = "black")) + 
        scale_x_discrete(labels = conditionsVDCpresentationwrap)
    }
  }
  return(grid.arrange(
    plotslist[[1]], plotslist[[2]], plotslist[[3]], # fat
    plotslist[[4]], plotslist[[5]], plotslist[[6]], # lean
    ncol = 3))
}
#svg("leanfat.svg", width = 9, height = 4 ); plotleanfatpresentation(); dev.off()

plotmuscleweightspresentation <- function(){
  #alphabetical order
  columnnames <- c("gastrocnemius..mg.",
                   "levator..mg.",
                   "quadriceps..mg.",
                   "tibialis..mg.",
                   "triceps..mg.")
  ylabels <- c("gastrocnemius (mg)",
               "levator ani (mg)",
               "quadriceps (mg)",
               "tibialis ant. (mg)",
               "triceps br. (mg)")
  ylabels <- rep("", 5)
  ylims <- list(c(0, 200),
                c(0, 100), 
                c(0, 225),
                c(0, 75),
                c(0, 150))
  statstrings <- list(
    #gastrocnemia1,3,7:
    threeidenticalgroups,
    c("a", "b", "a,b"),
    c("a", "b", "a,b"),
    #levators1,3,7:
    threeidenticalgroups,
    threeidenticalgroups,
    c("a", "b", "a,b"),
    #quadriceps:
    threeidenticalgroups,
    threeidenticalgroups,
    c("a", "b", "a,b"),
    #tibialis:
    threeidenticalgroups,
    threeidenticalgroups,
    threeidenticalgroups,
    #triceps:
    threeidenticalgroups,
    threeidenticalgroups,
    c("a", "b", "b")
  )
  plotslist <- list()
  for (i in 1:5){
    shortdf1 <- InvivoOnedayCVD[, colnames(InvivoOnedayCVD) %in% c("treatment", columnnames[[i]])]
    shortdf3 <- InvivoThreedayCVD[, colnames(InvivoThreedayCVD) %in% c("treatment", columnnames[[i]])]
    shortdf7 <- InvivoSevendayCVD[, colnames(InvivoSevendayCVD) %in% c("treatment", columnnames[[i]])]
    plotslist[[i*3-2]] <- threecolumnplotpresentation(shortdf1, ylabels[[i]], ylims[[i]], statstrings[[(i*3-2)]])
    plotslist[[i*3-1]] <- threecolumnplotpresentation(shortdf3, ylabels[[i]], ylims[[i]], statstrings[[(i*3-1)]])
    plotslist[[i*3]] <- threecolumnplotpresentation(shortdf7, ylabels[[i]], ylims[[i]], statstrings[[(i*3)]])
    if ((columnnames[[i]] == "gastrocnemius..mg.") | (columnnames[[i]] == "triceps..mg.")){
      plotslist[[i*3-2]] <- plotslist[[i*3-2]] +
        annotationastitle("one day", 2, ylims[[i]][[2]])
      plotslist[[i*3-1]] <- plotslist[[i*3-1]] + 
        annotationastitle("three days", 2, ylims[[i]][[2]])
      plotslist[[i*3]] <- plotslist[[i*3]] + 
        annotationastitle("seven days", 2, ylims[[i]][[2]])
    }
  }
  return(grid.arrange(
    blankgrob,
    plotslist[[1]], plotslist[[2]], plotslist[[3]], # gastrocnemius
    blankgrob,
    plotslist[[13]], plotslist[[14]], plotslist[[15]], # triceps
    blankgrob,
    plotslist[[7]], plotslist[[8]], plotslist[[9]], # quadriceps
    blankgrob,
    plotslist[[4]], plotslist[[5]], plotslist[[6]], # levator
    blankgrob,
    plotslist[[10]], plotslist[[11]], plotslist[[12]], # tibialis
    ncol = 8,
    widths = c(0.15, 1, 1, 1, 0.15, 1, 1, 1)))
}
