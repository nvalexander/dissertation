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
#font_import(prompt = FALSE)
options(bitmapType="cairo")

codedirpath <- dirname(
  tryCatch(normalizePath(parent.frame(2)$ofile),  # works when using source
           error=function(e) # works when using R CMD
             normalizePath(unlist(strsplit(commandArgs()[grep('^--file=',
                                                              commandArgs())], '='))[2]))
)


codedirpath <- "/media/dump/writingswork/draftthesis"

condsVDTC <- c("V", "D", "T", "C")
contrastsfour <- c("V vs D", "V vs T", "D vs T", "V vs DT", "D vs DT", "T vs DT")
VvsDfourways <- match("V vs D", contrastsfour)[[1]]
VvsTfourways <- match("V vs T", contrastsfour)[[1]]
DvsCfourways <- match("D vs DT", contrastsfour)[[1]]
condsVDC <- c("V", "D", "C")
contraststhree <- c("V vs D", "V vs DT", "D vs DT")
VvsDthreeways <- match("V vs D", contraststhree)[[1]]
DvsCthreeways <- match("D vs DT", contraststhree)[[1]]
VvsCthreeways <- match("V vs DT", contraststhree)[[1]]
conditionsVDC <- c("Veh", "Dexa", "Comb")
conditionsVDTC <- c("Veh", "Dexa", "Testo", "Comb")
unistar <- sprintf('\u2736')
unidagger <- sprintf('\u2020')
threeemptystrings <- c("", "", "")

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


invivodatasubsetonedays <- invivodataonedays[invivodataonedays$treatment %in% condsVDC, ]
invivodataonedays$treatment <- factor(invivodataonedays$treatment, 
                               levels = condsVDTC)
invivodatasubsetonedays$treatment <- factor(invivodatasubsetonedays$treatment, 
                                     levels = condsVDC)

invivodatasubsetthreedays <- invivodatathreedays[invivodatathreedays$treatment %in% condsVDC, ]
invivodatathreedays$treatment <- factor(invivodatathreedays$treatment, 
                                      levels = condsVDTC)
invivodatasubsetthreedays$treatment <- factor(invivodatasubsetthreedays$treatment, 
                                            levels = condsVDC)

invivodatasubsetsevendays <- invivodatasevendays[invivodatasevendays$treatment %in% condsVDC, ]
invivodatasevendays$treatment <- factor(invivodatasevendays$treatment, 
                                      levels = condsVDTC)
invivodatasubsetsevendays$treatment <- factor(invivodatasubsetsevendays$treatment, 
                                            levels = condsVDC)


textSize <- 11
nicepalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
meaningfulpalette <- c("#444444", "#dd0000", "#00dd00", "#0000dd", "#888800", "#880088", "#008888", "#dddddd")
greypalette <- c("#ffffff", "#222222", "#999999", "#0000dd", "#00dd00", "#dd0000", "#008888", "#888800")
stdplottimecourse <- theme_bw() + 
  theme(text = element_text(size = textSize, color = "black", family="Liberation Sans Narrow"),
        panel.grid.major.x = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        axis.title.x = element_text( size = textSize),
        axis.title.y = element_text( size = textSize),
        axis.text.x = element_text( size = textSize * 1 ),
        axis.text.y = element_text( size = textSize * 1 ),
        legend.title = element_blank())

stdbarplot <- 
  theme_bw() +
  theme(text = element_text(size = textSize, color = "black", family="Liberation Sans Narrow"),
        panel.grid.major.x = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(color = "black"),
        axis.ticks.x = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_text( size = textSize),
        axis.text.x = element_text( size = textSize * 1 ),
        axis.text.y = element_text( size = textSize * 1 ))

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
  return(format(x, digits = 3))
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

statstringyunderbar <-function(x){
  return(semInterval(x)[[1]])
}

statstringyoverbar <-function(x){
  return(semInterval(x)[[1]])
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
  invivodatasubset <- invivodata[invivodata$treatment %in% condsVDC, ]
  invivodatasubset$treatment <- factor(invivodatasubset$treatment, 
                                levels = condsVDC)
  
  for (i in 1:length(invivocolnames)) {
    if (invivocolnames[[i]] %in% c("animal", "TreatmentLong", "treatment")) next
    meanV <- mean(subset(invivodata, treatment == "V")[,i], na.rm = TRUE)
    meanD <- mean(subset(invivodata, treatment == "D")[,i], na.rm = TRUE)
    meanC <- mean(subset(invivodata, treatment == "C")[,i], na.rm = TRUE)
    myoutput <- paste0(myoutput, 
                       "# ", invivocolnames[[i]])
    myoutput <- paste(myoutput, 
                      pandoc.table.return(setNames(aggregate(invivodata[,i], list(invivodata$treatment), longdescription), c("Treatment", "Average (SD; n)")),
                                          style = "rmarkdown"))
    kw <- kruskal.test(as.formula(paste(colnames(invivodata)[i], "~treatment")), data = invivodata)
    myoutput <- paste(myoutput, 
                      paste0("Kruskal-Wallis p value for the four-way comparison is ", signif(kw$p.value, digits = 3)))
    dunns <- dunn.test(invivodata[,i], invivodata$treatment, method = "bonferroni")
    dunnsreport <- data.frame(contrastsfour, dunns$P.adjusted)[c(VvsDfourways, DvsCfourways, VvsTfourways), ]
    myoutput <- paste(myoutput,
                      pandoc.table.return(dunnsreport, style = "rmarkdown"))
    kw <- kruskal.test(as.formula(paste(colnames(invivodata)[i], "~treatment")), data = invivodatasubset)
    myoutput <- paste(myoutput, 
                      paste0("Kruskal-Wallis p value for the three-way comparison is ", signif(kw$p.value, digits = 3)))
    dunns <- dunn.test(invivodatasubset[,i], invivodatasubset$treatment, method = "bonferroni")
    dunnsreport <- data.frame(contraststhree, dunns$P.adjusted)[c(VvsDthreeways, DvsCthreeways, VvsCthreeways), ]
    dunnsreport <- setNames(cbind(dunnsreport, c(ifelse(meanV > meanD, 'V > D' , 'V < D'), 
                                                 ifelse(meanD > meanC, 'D > DT' , 'D < DT'), 
                                                 ifelse(meanV > meanC, 'V > DT' , 'V < DT'))),
                            c("Comparison", "P value", "Direction"))
    myoutput <- paste(myoutput,
                      pandoc.table.return(dunnsreport, style = "rmarkdown"))
    
  }
  return(myoutput)
}

plotbodyweightcourse <- function(){
  timeseriescolumns <- c("body.weight.gain.after.1.days..percent.",
                         "body.weight.gain.after.2.days..percent.",
                         "body.weight.gain.after.3.days..percent.",
                         "body.weight.gain.after.4.days..percent.",
                         "body.weight.gain.after.5.days..percent.",
                         "body.weight.gain.after.6.days..percent.",
                         "body.weight.gain.after.7.days..percent.",
                         "body.weight.gain.after.8.days..percent.")
  shortdf <- invivodatasubsetsevendays[, colnames(invivodatasubsetsevendays) %in% c("treatment", timeseriescolumns)]
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
  
  shortdf <- invivodatasubsetthreedays[, colnames(invivodatasubsetthreedays) %in% c("treatment", timeseriescolumns[1:4])]
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
    
  shortdf <- invivodatasubsetonedays[, colnames(invivodatasubsetonedays) %in% c("treatment", timeseriescolumns[1:1])]
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

plotbodyweightsatsacrifice <- function(){
  ylabel <- "body weight (g) "
  ylimit <- c(0, 32)
  
  shortdf <- invivodatasubsetonedays[, colnames(invivodatasubsetonedays) %in% c("treatment", "day.2.body.weight..g.")]
  leftplot <- threecolumnplot(shortdf, ylabel, ylimit, c("a", "b", "a,b")) + 
    geom_text(aes(2, 32, 
                  label="one day", 
                  vjust = 1, 
                  hjust = .5,
                  family = "Liberation Sans Narrow",
                  show_guide = FALSE),
              size = textSize * .3) +
    scale_x_discrete(labels = conditionsVDC) 

  
  shortdf <- invivodatasubsetthreedays[, colnames(invivodatasubsetthreedays) %in% c("treatment", "day.4.body.weight..g.")]
  midplot <-  threecolumnplot(shortdf, ylabel, ylimit, c("a,b", "a", "b")) + 
    geom_text(aes(2, 32, 
                  label="three days", 
                  vjust = 1, 
                  hjust = .5,
                  family = "Liberation Sans Narrow",
                  show_guide = FALSE),
              size = textSize * .3)+
    scale_x_discrete(labels = threeemptystrings)
  
  shortdf <- invivodatasubsetsevendays[, colnames(invivodatasubsetsevendays) %in% c("treatment", "day.8.body.weight..g.")]
  rightplot <-  threecolumnplot(shortdf, ylabel, ylimit, c("a", "a", "a")) + 
    geom_text(aes(2, 32, 
                  label="seven days", 
                  vjust = 1, 
                  hjust = .5,
                  family = "Liberation Sans Narrow",
                  show_guide = FALSE),
              size = textSize * .3)+
    scale_x_discrete(labels = threeemptystrings)
  
  return(grid.arrange(leftplot, midplot, rightplot, ncol=3))
}

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
  
  #1
  shortdf <- invivodatasubsetonedays[, colnames(invivodatasubsetonedays) %in% c("treatment", leancolumn)]
  shortdf[,2] <- shortdf[, 2] * (-1)
  leanoneplot <- threecolumnplot(shortdf, leanlabel, leanylim, c("a", "a", "a"))+
    scale_x_discrete(labels = threeemptystrings)
  
  #2
  shortdf <- invivodatasubsetthreedays[, colnames(invivodatasubsetthreedays) %in% c("treatment", leancolumn)]
  shortdf[,2] <- shortdf[, 2] * (-1)
  leanthreeplot <- threecolumnplot(shortdf, leanlabel, leanylim, c("a", "b", "a,b"))+
    scale_x_discrete(labels = threeemptystrings)
  
  #3
  shortdf <- invivodatasubsetsevendays[, colnames(invivodatasubsetsevendays) %in% c("treatment", leancolumn)]
  shortdf[,2] <- shortdf[, 2] * (-1)
  leansevenplot <- threecolumnplot(shortdf, leanlabel, leanylim, c("a", "b", "a,b"))+
    scale_x_discrete(labels = conditionsVDC)

  #4
  shortdf <- invivodatasubsetonedays[, colnames(invivodatasubsetonedays) %in% c("treatment", fatcolumn)]
  fatoneplot <- threecolumnplot(shortdf, fatlabel, fatylim, c("a", "a", "a"))+
    theme(axis.text.x=element_blank())
  
  #5
  shortdf <- invivodatasubsetthreedays[, colnames(invivodatasubsetthreedays) %in% c("treatment", fatcolumn)]
  fatthreeplot <- threecolumnplot(shortdf, fatlabel, fatylim, c("a", "a,b", "b"))+
    theme(axis.text.x=element_blank())
  
  #6
  shortdf <- invivodatasubsetsevendays[, colnames(invivodatasubsetsevendays) %in% c("treatment", fatcolumn)]
  fatsevenplot <- threecolumnplot(shortdf, fatlabel, fatylim, c("a", "a,b", "b"))+
    theme(axis.text.x=element_blank())
  
  #7
  shortdf <- invivodatasubsetonedays[, colnames(invivodatasubsetonedays) %in% c("treatment", watercolumn)]
  shortdf[,2] <- shortdf[, 2] * (-1)
  wateroneplot <- threecolumnplot(shortdf, waterlabel, waterylim, c("a", "a", "a")) +
    theme(axis.text.x = element_blank()) +
    ggtitle("one day")
  
  #8
  shortdf <- invivodatasubsetthreedays[, colnames(invivodatasubsetthreedays) %in% c("treatment", watercolumn)]
  shortdf[,2] <- shortdf[, 2] * (-1)
  waterthreeplot <- threecolumnplot(shortdf, waterlabel, waterylim, c("a", "a", "a"))+
    theme(axis.text.x = element_blank()) +
    ggtitle("three days")
  
  #9
  shortdf <- invivodatasubsetsevendays[, colnames(invivodatasubsetsevendays) %in% c("treatment", watercolumn)]
  shortdf[,2] <- shortdf[, 2] * (-1)
  watersevenplot <- threecolumnplot(shortdf, waterlabel, waterylim, c("a", "a", "a")) +
    theme(axis.text.x = element_blank()) +
    ggtitle("seven days")

  return(grid.arrange(
    wateroneplot, waterthreeplot, watersevenplot,
    fatoneplot, fatthreeplot, fatsevenplot,
    leanoneplot, leanthreeplot, leansevenplot, 
    ncol=3))
}

threecolumnplot <- function(skinnydataset, ylabel, ylimit, statstrings){ 
  return(ggplot(skinnydataset) +
    aes_string( x = colnames(skinnydataset)[1], 
                y = colnames(skinnydataset)[2], 
                fill = colnames(skinnydataset)[1]) +
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

plotmuscleweights <- function(){
  levatorcolumn <- "levator..permille."
  quadricepscolumn <- "quadriceps..permille."
  gastrocnemiuscolumn <- "gastrocnemius..permille."
  tricepscolumn <- "triceps..permille."
  tibialiscolumn <- "tibialis..permille."
  
  levatorlabel <- paste0("levator ani (", sprintf('\u2030'), " BW)")
  quadricepslabel <- paste0("quadriceps (", sprintf('\u2030'), " BW)")
  gastrocnemiuslabel <- paste0("gastrocnemius (", sprintf('\u2030'), " BW)")
  tricepslabel <- paste0("triceps br. (", sprintf('\u2030'), " BW)")
  tibialislabel <- paste0("tibialis ant. (", sprintf('\u2030'), " BW)")
  
  levatorylim <- c(0, 4)
  quadricepsylim <- c(0, 10)
  gastrocnemiusylim <- c(0, 7)
  tricepsylim <- c(0, 6)
  tibialisylim <- c(0, 3)
  
  #1
  shortdf <- invivodatasubsetonedays[, colnames(invivodatasubsetonedays) %in% c("treatment", levatorcolumn)]
  levatoroneplot <- threecolumnplot(shortdf, levatorlabel, levatorylim, c("a", "a", "a")) +
    theme(axis.text.x=element_blank()) +
    ggtitle("one day")
  
  #2
  shortdf <- invivodatasubsetthreedays[, colnames(invivodatasubsetthreedays) %in% c("treatment", levatorcolumn)]
  levatorthreeplot <- threecolumnplot(shortdf, levatorlabel, levatorylim, c("a", "a", "a")) +
    theme(axis.text.x=element_blank()) 
  
  #3
  shortdf <- invivodatasubsetsevendays[, colnames(invivodatasubsetsevendays) %in% c("treatment", levatorcolumn)]
  levatorsevenplot <- threecolumnplot(shortdf, levatorlabel, levatorylim, c("a", "a", "a")) +
    theme(axis.text.x=element_blank()) +
    theme(plot.title = element_text(vjust=.5)) + labs(title="seven days")
  
  #4
  shortdf <- invivodatasubsetonedays[, colnames(invivodatasubsetonedays) %in% c("treatment", quadricepscolumn)]
  quadricepsoneplot <- threecolumnplot(shortdf, quadricepslabel, quadricepsylim, c("a", "a", "a")) +
    theme(axis.text.x=element_blank())
  
  #5
  shortdf <- invivodatasubsetthreedays[, colnames(invivodatasubsetthreedays) %in% c("treatment", quadricepscolumn)]
  quadricepsthreeplot <- threecolumnplot(shortdf, quadricepslabel, quadricepsylim, c("a", "a", "a")) +
    theme(axis.text.x=element_blank())
  
  #6
  shortdf <- invivodatasubsetsevendays[, colnames(invivodatasubsetsevendays) %in% c("treatment", quadricepscolumn)]
  quadricepssevenplot <- threecolumnplot(shortdf, quadricepslabel, quadricepsylim, c("a", "b", "a,b")) +
    theme(axis.text.x=element_blank())
  
  #7
  shortdf <- invivodatasubsetonedays[, colnames(invivodatasubsetonedays) %in% c("treatment", gastrocnemiuscolumn)]
  gastrocnemiusoneplot <- threecolumnplot(shortdf, gastrocnemiuslabel, gastrocnemiusylim, c("a", "b", "a,b")) +
    theme(axis.text.x=element_blank())
  
  #8
  shortdf <- invivodatasubsetthreedays[, colnames(invivodatasubsetthreedays) %in% c("treatment", gastrocnemiuscolumn)]
  gastrocnemiusthreeplot <- threecolumnplot(shortdf, gastrocnemiuslabel, gastrocnemiusylim, c("a", "b", "a,b")) +
    theme(axis.text.x=element_blank())
  
  #9
  shortdf <- invivodatasubsetsevendays[, colnames(invivodatasubsetsevendays) %in% c("treatment", gastrocnemiuscolumn)]
  gastrocnemiussevenplot <- threecolumnplot(shortdf, gastrocnemiuslabel, gastrocnemiusylim, c("a", "b", "a,b")) +
    theme(axis.text.x=element_blank())
  
  #10
  shortdf <- invivodatasubsetonedays[, colnames(invivodatasubsetonedays) %in% c("treatment", tricepscolumn)]
  tricepsoneplot <- threecolumnplot(shortdf, tricepslabel, tricepsylim, c("a", "a", "a"))+
    theme(axis.text.x=element_blank())
  
  #11
  shortdf <- invivodatasubsetthreedays[, colnames(invivodatasubsetthreedays) %in% c("treatment", tricepscolumn)]
  tricepsthreeplot <- threecolumnplot(shortdf, tricepslabel, tricepsylim, c("a", "a", "a"))+
    theme(axis.text.x=element_blank())
  
  #13
  shortdf <- invivodatasubsetsevendays[, colnames(invivodatasubsetsevendays) %in% c("treatment", tricepscolumn)]
  tricepssevenplot <- threecolumnplot(shortdf, tricepslabel, tricepsylim, c("a", "b", "a,b"))+
    theme(axis.text.x=element_blank())
  
  #14
  shortdf <- invivodatasubsetonedays[, colnames(invivodatasubsetonedays) %in% c("treatment", tibialiscolumn)]
  tibialisoneplot <- threecolumnplot(shortdf, tibialislabel, tibialisylim, c("a", "a", "a"))+
    theme(axis.text.x=element_blank())+
    scale_x_discrete(labels = threeemptystrings)
  
  #15
  shortdf <- invivodatasubsetthreedays[, colnames(invivodatasubsetthreedays) %in% c("treatment", tibialiscolumn)]
  tibialisthreeplot <- threecolumnplot(shortdf, tibialislabel, tibialisylim, c("a", "a", "a"))+
    theme(axis.text.x=element_blank())+
    scale_x_discrete(labels = threeemptystrings)
  
  #12
  shortdf <- invivodatasubsetsevendays[, colnames(invivodatasubsetsevendays) %in% c("treatment", tibialiscolumn)]
  tibialissevenplot <- threecolumnplot(shortdf, tibialislabel, tibialisylim, c("a", "a", "a"))+
    theme(axis.text.x=element_blank())+
    scale_x_discrete(labels = conditionsVDC)
  
  
  
  return(grid.arrange(levatoroneplot, levatorthreeplot, levatorsevenplot,
                      quadricepsoneplot, quadricepsthreeplot, quadricepssevenplot,
                      gastrocnemiusoneplot, gastrocnemiusthreeplot, gastrocnemiussevenplot,
                      tricepsoneplot, tricepsthreeplot, tricepssevenplot,
                      tibialisoneplot, tibialisthreeplot, tibialissevenplot,
                      ncol=3))
}