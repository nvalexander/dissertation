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
options(bitmapType="cairo")

codedirpath <- dirname(
  tryCatch(normalizePath(parent.frame(2)$ofile),  # works when using source
           error=function(e) # works when using R CMD
             normalizePath(unlist(strsplit(commandArgs()[grep('^--file=',
                                                              commandArgs())], '='))[2]))
)
datadir <- normalizePath(file.path(codedirpath, "data"))
invivodataonedays <- read.csv(file.path(datadir, "2012.12.09.1dayTD.csv"), header = FALSE, skip = 1)
invivocolnameonedays <- read.table(file.path(datadir, "2012.12.09.1dayTD.csv"), header = FALSE, sep = ",", nrows = 1)
levels(invivodataonedays$V2)[levels(invivodataonedays$V2)=="A"] <- "C"
levels(invivodataonedays$V2)[levels(invivodataonedays$V2)=="E"] <- "D"
levels(invivodataonedays$V2)[levels(invivodataonedays$V2)=="W"] <- "V"
levels(invivodataonedays$V2)[levels(invivodataonedays$V2)=="S"] <- "T"
invivodatathreedays <- read.csv(file.path(datadir, "2012.12.12.3daysTD.csv"), header = FALSE, skip = 1)
invivocolnamethreedays <- read.table(file.path(datadir, "2012.12.12.3daysTD.csv"), header = FALSE, sep = ",", nrows = 1)
levels(invivodatathreedays$V2)[levels(invivodatathreedays$V2)=="B"] <- "C"
levels(invivodatathreedays$V2)[levels(invivodatathreedays$V2)=="G"] <- "D"
levels(invivodatathreedays$V2)[levels(invivodatathreedays$V2)=="X"] <- "V"
levels(invivodatathreedays$V2)[levels(invivodatathreedays$V2)=="U"] <- "T"
invivodatasevendays <- read.csv(file.path(datadir, "2012.08.23.7daysTD.csv"), header = FALSE, skip = 1)
invivocolnamesevendays <- read.table(file.path(datadir, "2012.08.23.7daysTD.csv"), header = FALSE, sep = ",", nrows = 1)


potentialConditionColumnNames <- c(
  "condition",
  "TreatmentShort",
  "treatment"
  )

potentialBaselineConditionNames <- c(
  "placebo",
  "vehicle",
  "V"
)

contrastsfour <- c("D vs DT", "T vs DT", "D vs T", "V vs DT", "V vs D", "V vs T")
contraststhree <- c("D vs DT", "V vs DT", "V vs D")

nicepalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
meaningfulpalette <- c("#444444", "#dd0000", "#00dd00", "#0000dd", "#888800", "#880088", "#008888", "#dddddd")

SEM <- function(x) {
  return( sqrt(var(x, na.rm = TRUE) / length(na.omit(x))) )
}

CI95 <- function(x) {
  return( qt(.975, df = (length(na.omit(x)) - 1)) * SEM(x) )
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

truelength <- function(x) {
  return( length(na.omit(x)) )
}

hview <- function(htmllines) {
  htmlFile <- tempfile(fileext = ".html")
  writeLines(htmllines, htmlFile)
  rstudio::viewer(htmlFile)
}

columnNameToNo <- function(inputdf, columnname = integer()) {
## this function decides on a column number when ONE column name is given
#  it first tries perfect match, then partial match
#  if no match, it returns integer()
  if ( 0 == sum(!(is.na(inputdf))) ){
    # no data frame to analyze
    warning("No dataframe to analyze in columnNameToNo.")
    return(integer())
  }
  if ( 0 == length(na.omit(columnname))) {
    # no column name to process
    warning("No column name to process in columnNameToNo.")
    return(integer())
  }
  # stick to one column name, even if multiple columns were on input
  if ( 1 < length(na.omit(columnname)) ) {
    # use at most the first columnname
    warning("Multiple column names were given to columnNameToNo; only the first one will be used.")
  }
  columnnameLocal <- na.omit(columnname)[1]
  putativeColumnNos <- grep( paste0("^", columnnameLocal, "$"),
                             colnames(inputdf),
                             ignore.case = TRUE
                             )
  if ( 0 == length(na.omit(putativeColumnNos)) ) {
    # perfect match failed,
    # so test for incomplete match
    putativeColumnNos <- grep(columnnameLocal,
                              colnames(inputdf),
                              ignore.case = TRUE
                              )
  }
  if ( 0 == length(na.omit(putativeColumnNos)) ) {
    # imperfect match failed too
    warning(paste("Invalid column name:", columnnameLocal, "in columnNameToNo"))
    return(integer())
  }
  return( putativeColumnNos[1] )
}

columnNoAndNameToNo <- function(inputdf, columnno = integer(), columnname = character()){
## this function decides on a column number when column name and number are given
#  if it fails, it returns NA
#  if both are set, but contradict each other, return NA
  if ( 0 == sum(!(is.na(inputdf))) ){
    # no data frame to analyze
    warning("No dataframe to analyze in columnNoAndNameToNo.")
    return(integer())
  }
  if ( ( 0 == length(na.omit(columnno))) & ( 0 == length(na.omit(columnname))) ) {
    # both column name and number are missing
    warning("No column name, nor column number was given to process in columnNoAndNameToNo.")
    return(integer())
  }
  putativeColNo1 <- integer()
  if ( 0 < length(na.omit(columnno))) {
    if ( 1 < length(na.omit(columnno))) {
      warning("Multiple column numbers were given to columnNoAndNameToNo, only the first one will be used.")
    }
    putativeColNo1 <- columnno[1]
    if ( ( 0 >= putativeColNo1) | (  dim(inputdf)[2] < putativeColNo1) ) {
      warning("Invalid column numbers given to columnNoAndNameToNo: ", putativeColNo1)
      putativeColNo1 <- integer()
    }
  }
  putativeColNo2 <- columnNameToNo(inputdf, columnname)
  if ( (0 == length(na.omit(putativeColNo1))) & ( 0 == length(na.omit(putativeColNo2))) ) {
    # both column name and number were useless
    return(integer())
  }
  if ( ( 0 != length(na.omit(putativeColNo1))) & ( 0 != length(na.omit(putativeColNo2))) ) {
    if (putativeColNo1[1] != putativeColNo2[1]) {
      # column name and number were both set, but contradicted each other
      warning("columnNoAndNameToNo received both a column name and a number, but they contradicted each other.")
      return(integer())
    } else {
      return(putativeColNo1[1])
    }
  }
  #this point can be reached if only either putative number is correct
  if ( 0 != length(na.omit(putativeColNo1))) {
    #if 1 is invalid, 2 is valid
    return(putativeColNo1[1])
  } else {
    return(putativeColNo2[1])
  }
}

conditionColumnNumber <- function(inputdf, conditionColumnNo = integer(), conditionColumnName = character()) {
  if ( 0 == sum(!(is.na(inputdf))) ){
    # no data frame to analyze
    warning("No dataframe to analyze in conditionColumnNumber.")
    return(integer())
  }
  if ( ( 0 == length(na.omit(conditionColumnNo)) ) &
         ( 0 == length(na.omit(conditionColumnName)) ) ) {
    # find the default condition column
    conditionColumnNos <- c()
    for (potentialConditionColumnName in potentialConditionColumnNames) {
      conditionColumnNos <- c(conditionColumnNos,
                              columnNoAndNameToNo(inputdf, columnname = potentialConditionColumnName)
      )
    }
    conditionColumnNos <- na.omit(unique(conditionColumnNos))
    if ( 0 == length(conditionColumnNos) ) {
      return(integer())
    } else {
      warning("conditionColumnNumber chose a default condition column, based on column names.")
      return( min(conditionColumnNos) )
    }
  } else {
    # sure make column is valid
    return(columnNoAndNameToNo(inputdf, columnno = conditionColumnNo, columnname = conditionColumnName))
  }
}

columnNosAndNamesToNos <- function(inputdf, columnnos = integer(), columnnames = character()){
  ## this function builds a list of column numbers, when some column names and numbers are given
  #  if it fails, it returns integer()
  if ( 0 == sum(!(is.na(inputdf))) ){
    # no data frame to analyze
    warning("No dataframe to analyze in columnNosAndNamesToNos.")
    return(integer())
  }
  if ( ( 0 == length(na.omit(columnnos))) & ( 0 == length(na.omit(columnnames))) ) {
    # both column names and numbers are missing
    warning("No column numbers, nor column  nameswas given to process in columnNosAndNamesToNos.")
    return(integer())
  }
  putativeColumnNos <- c()
  for (somecolno in na.omit(columnnos)) {
    putativeColumnNos <- c(putativeColumnNos, columnNoAndNameToNo(inputdf, columnno = somecolno))
  }
  for (somecolname in na.omit(columnnames)) {
    putativeColumnNos <- c(putativeColumnNos, columnNoAndNameToNo(inputdf, columnname = somecolname))
  }
  putativeColumnNos <- na.omit(unique(putativeColumnNos))
  if ( 0 == length(putativeColumnNos) ) {
    return(integer())
  } else {
    return( putativeColumnNos )
  }
}

numericColumnNos <- function(inputdf, conditionColumnNo = integer(), conditionColumnName = character()) {
  # returns integer() if no numeric columns exist
  putativeReturn <- integer()
  for (somecolno in 1:dim(inputdf)[2]) {
    if ( ("integer" == class(inputdf[ ,somecolno]) ) |
           ("numeric" == class(inputdf[ ,somecolno]) ) ) {
      putativeReturn <- c(putativeReturn, somecolno)
    }
  }
  putativeReturn <- putativeReturn[!putativeReturn %in% conditionColumnNumber(inputdf,
                                                                              conditionColumnNo = conditionColumnNo,
                                                                              conditionColumnName = conditionColumnName
                                                                              )
                                   ]
  return(putativeReturn)
}

cleanedConditionsList <- function(inputdf,
                                  conditionSet = character(),
                                  conditionColumnNo = integer(), conditionColumnName = character()) {
  if ( 0 == sum(!(is.na(inputdf))) ) {
    # no data frame to analyze
    warning("No dataframe to analyze in cleanedConditionsList.")
    return(character())
  }
  actualConditionColumnNo <- conditionColumnNumber(inputdf,
                                               conditionColumnNo = conditionColumnNo,
                                               conditionColumnName = conditionColumnName
                                               )
  if (0 == length(is.na(actualConditionColumnNo))) {
    # no condition column was found
    warning("No valid condition column was given to, nor found by cleanedConditionsList.")
    return(character())
  }
  possibleConditions <- levels(factor(as.character(inputdf[ , actualConditionColumnNo ])))
  putativeReturn <- character()
  for (somecond in as.character(conditionSet) ) {
    if ( somecond %in% as.character(possibleConditions) ) {
      putativeReturn <- c(putativeReturn, somecond)
    }
  }
  return(unique(putativeReturn))
}

verifiedConditionSet <- function(inputdf,
                                 conditionSet = character(),
                                 conditionColumnNo = integer(), conditionColumnName = character())
{
  if ( 0 == sum(!(is.na(inputdf))) ) {
    # no data frame to analyze
    warning("No dataframe to analyze in verifiedConditionSet.")
    return(character())
  }
  actualConditionColumnNo <- conditionColumnNumber(inputdf,
                                                   conditionColumnNo = conditionColumnNo,
                                                   conditionColumnName = conditionColumnName
                                                   )
  if (0 == length(is.na(actualConditionColumnNo))) {
    # no condition column was found
    warning("No valid condition column was given to, nor found by verifiedConditionSet.")
    return(character())
  }
  if(0 == length(na.omit(conditionSet))) {
    # no specific conditions were given, default to full list of levels
    warning("verifiedConditionSet defaulted to full list of levels")
    return(levels(factor(inputdf[,actualConditionColumnNo])))
  } else {
    return(cleanedConditionsList(inputdf,
                                 conditionSet = conditionSet,
                                 conditionColumnNo = conditionColumnNo,
                                 conditionColumnName = conditionColumnName
                                 ))
  }
}

removeOutliers <- function(inputdf,
                           cleanedColNos = integer(), cleanedColNames = character(),
                           conditionColumnNo = integer(), conditionColumnName = character()) {
  ## returns a df with almost the same content, except outliers are replaced with NA
  ## outliers are calculated with Tukey's rule inside the same column and condition
  if ( 0 == sum(!(is.na(inputdf))) ){
    # no data frame to analyze
    warning("No dataframe to analyze in removeOutliers.")
    return(inputdf)
  }
  actualConditionColumnNo <- conditionColumnNumber(inputdf,
                                               conditionColumnNo = conditionColumnNo,
                                               conditionColumnName = conditionColumnName
  )
  if (0 == length(na.omit(actualConditionColumnNo))) {
    # no condition column was found
    warning("No valid condition column was given to, nor found by removeOutliers.")
    return(inputdf)
  }
  if ( ( 0 == length(na.omit(cleanedColNos))) & ( 0 == length(na.omit(cleanedColNames))) ) {
    # no columns were indicated, we'll take all numerical columns
    probedColumnNos <- numericColumnNos(inputdf)
  } else {
    probedColumnNos <- columnNosAndNamesToNos(inputdf,
                                              columnnos = cleanedColNos,
                                              columnnames = cleanedColNames)
  }
  if ( 0 == length(na.omit(probedColumnNos))) {
    # no data frame to analyze
    warning("No columns to analyze by removeOutliers.")
    return(inputdf)
  }
  outputdf <- inputdf
  for(somecondition in levels(factor(as.character(outputdf[ , actualConditionColumnNo ])))) {
    # determine which rows meet specific condition
    probedRows <- which( somecondition == outputdf[ , actualConditionColumnNo ] )
    if (2 < length(probedRows)){
      for (somecolno in probedColumnNos)  {
        setofreads <- outputdf[probedRows, somecolno]
        #calculates Tukey's fences for each condition and each fraction separately
        lowthreshold <- quantile(setofreads, c(.25), na.rm = TRUE) -
          1.5 * IQR(setofreads, na.rm = TRUE)
        highthreshold <- quantile(setofreads, c(.25), na.rm = TRUE) +
          1.5 * IQR(setofreads, na.rm = TRUE)
        outliers <- c(
          which(setofreads < lowthreshold),
          which(setofreads > highthreshold)
        )
        if (0 < length(outliers)) {
          outputdf[ probedRows[outliers], somecolno] <- NA
        }
      }
    }
  }
  return(outputdf)
}

extractDataSet <- function(inputdf,
                           selectColNo = integer(), selectColName = character(),
                           condition = character(),
                           conditionColumnNo = integer(), conditionColumnName = character(),
                           na.rm = TRUE) {
  # this function retrieves from a column the numbers corresponding to a condition
  # if something is wrong with the question (absent columns etc), it returns numeric()
  # because one or more NAs form actual valid data
  if ( 0 == sum(!(is.na(inputdf))) ){
    # no data frame to analyze
    warning("No dataframe to analyze in extractDataSet.")
    return(numeric())
  }
  actualConditionColumnNo <- conditionColumnNumber(inputdf,
                                               conditionColumnNo = conditionColumnNo,
                                               conditionColumnName = conditionColumnName
  )
  if (0 == length(na.omit(actualConditionColumnNo))) {
    # no condition column was found
    warning("No valid condition column was given nor found in extractDataSet.")
    return(NA)
  }
  returnedColumn <- columnNoAndNameToNo(inputdf,
                                        columnno = selectColNo,
                                        columnname = selectColName)
  if (0 == length(na.omit(returnedColumn))) {
    # column requested was absent or invalid
    warning("Invalid requested colummn to extractDataSet.")
    return(numeric())
  }
  # select only the data from the column we settled on
  outputdf <- inputdf[ , returnedColumn]
  # select only rows that match condition
  outputdf <- outputdf[ ( inputdf[ , actualConditionColumnNo] %in% condition )]
  if (na.rm) {
    # if requested, drop NAs
    outputdf <- outputdf[!outputdf %in% NA]
  }
  return(outputdf)
}

extractDataSets <- function(inputdf,
                            selectColNo = integer(), selectColName = character(),
                            conditionSet = character(),
                            conditionColumnNo = integer(), conditionColumnName = character(),
                            na.rm = TRUE) {
  if ( 0 == sum(!(is.na(inputdf))) ){
    # no data frame to analyze
    warning("No dataframe to analyze by extractDataSets.")
    return(NA)
  }
  actualConditionColumnNo <- conditionColumnNumber(inputdf,
                                               conditionColumnNo = conditionColumnNo,
                                               conditionColumnName = conditionColumnName
  )
  if (0 == length(na.omit(actualConditionColumnNo))) {
    # no condition column was found
    warning("No valid condition column was given to, nor found by extractDataSets.")
    return(NA)
  }

  returnedColumn <- columnNoAndNameToNo(inputdf,
                                        columnno = selectColNo,
                                        columnname = selectColName
                                        )

  if ( 0 == length(na.omit(returnedColumn))) {
    # no valid returnedColumn was found, let's try the other column
    # (works only if we have two columns, and is acceptable only if selectColNo was undefined)
    if ( 2 == dim(inputdf)[2] ) {
      if ( ( 0 == length(selectColNo)) & (0 == length(selectColName))) {
        returnedColumn <- 3 - actualConditionColumnNo
      }
    }
  }
  if ( 0 == length(na.omit(returnedColumn))) {
    warning("Invalid requested column in extractDataSets.")
    # column requested was absent or invalid
    return(NA)
  }
  # start with all the input, constrained to the two columns of interest
  outputdf <- inputdf[, c(actualConditionColumnNo, returnedColumn)]
  actualConditions <- verifiedConditionSet(outputdf,
                            conditionSet = conditionSet,
                            conditionColumnNo = 1)
  if ( 0 == length(na.omit(actualConditions))){
    # no condition asked for actually exist, so exit with zero-rows frame
    warning("Invalid requested conditions in extractDataSets.")
    return(outputdf[integer(),])
  }
  #reorder levels
  outputdf[,1] <- factor(outputdf[,1], levels = actualConditions)
  #remove incomplete records if needed
  if (na.rm) {
    outputdf <- na.omit(outputdf)
  }
  returnedRows <- ( outputdf[, 1] %in% actualConditions )
  outputdf <- outputdf[returnedRows,]
  return(outputdf)
}

extractMultiColumn <- function(inputdf, selectedColNos = integer(), selectedColNames = character(),
                               conditionSet = character(),
                               conditionColumnNo = integer(), conditionColumnName = character(),
                               incomplete.rm = TRUE) {
  if ( 0 == sum(!(is.na(inputdf))) ){
    # no data frame to analyze
    warning("No dataframe to analyze in extractMultiColumn.")
    return(NA)
  }
  actualConditionColumnNo <- conditionColumnNumber(inputdf,
                                               conditionColumnNo = conditionColumnNo,
                                               conditionColumnName = conditionColumnName
  )
  if (0 == length(na.omit(actualConditionColumnNo))) {
    # no condition column was found
    warning("No valid condition column was given to, nor found by extractMultiColumn.")
    return(NA)
  }
  returnedColumns <- columnNosAndNamesToNos(inputdf,
                                            columnnos = selectedColNos,
                                            columnnames = selectedColNames
                                            )
  if (0 == length(na.omit(returnedColumns))) {
    # default to all columns, only if selectedColNo / selectedColName were undefined
    if ( ( 0 == length(na.omit(selectedColNos))) & ( 0 == length(na.omit(selectedColNames)))) {
      returnedColumns <- 1:dim(inputdf)[2]
    }
  }
  returnedColumns <- returnedColumns[!returnedColumns %in% actualConditionColumnNo]
  if (0 == length(na.omit(returnedColumns))) {
    warning("Invalid requested column(s) in extractMultiColumn.")
    # column requested was absent or invalid
    return(NA)
  }
  # create output df, starting with first column describing conditions
  outputdf <- data.frame(  condition = inputdf[,actualConditionColumnNo],
                           row.names = rownames(inputdf)
                           )
  outcolno <- 2
  for (incolno in returnedColumns) {
    outputdf[, outcolno ] <- inputdf[ , incolno]
    colnames(outputdf)[outcolno] <- colnames(inputdf)[incolno]
    outcolno <- outcolno + 1
  }
  actualConditions <- verifiedConditionSet(outputdf,
                                           conditionSet = conditionSet,
                                           conditionColumnNo = 1)
  if ( 0 == length(na.omit(actualConditions))){
    # no condition asked for actually exist, so exit with zero-rows frame
    warning("Invalid requested conditions in extractDataSets.")
    return(outputdf[integer(),])
  }
  #reorder levels
  outputdf[,1] <- factor(outputdf[,1], levels = actualConditions)
  # remove incomplete rows
  if (incomplete.rm) {
    outputdf <- outputdf[complete.cases(outputdf),]
  }
  #filter to rows fitting one of the conditions
  returnedRows <- ( outputdf[, 1] %in% actualConditions )
  outputdf <- outputdf[returnedRows,]
  return(outputdf)
}

conditionMean <- function(inputdf,
                          columnno = integer(), columnname = character(),
                          condition = character(),
                          conditionColumnNo = integer(), conditionColumnName = character()) {
  return( mean(extractDataSet(inputdf,
                              selectColNo = columnno,
                              selectColName = columnname,
                              condition = condition,
                              conditionColumnNo = conditionColumnNo,
                              conditionColumnName = conditionColumnName,
                              na.rm = TRUE
                              )
               )
  )
}

conditionSD <- function(inputdf,
                          columnno = integer(), columnname = character(),
                          condition = character(),
                          conditionColumnNo = integer(), conditionColumnName = character()) {
  return( sd(extractDataSet(inputdf,
                            selectColNo = columnno,
                            selectColName = columnname,
                            condition = condition,
                            conditionColumnNo = conditionColumnNo,
                            conditionColumnName = conditionColumnName,
                            na.rm = TRUE
                            )
             )
          )
}

conditionSEM <- function(inputdf,
                        columnno = integer(), columnname = character(),
                        condition = character(),
                        conditionColumnNo = integer(), conditionColumnName = character()) {
  return( SEM(extractDataSet(inputdf,
                            selectColNo = columnno,
                            selectColName = columnname,
                            condition = condition,
                            conditionColumnNo = conditionColumnNo,
                            conditionColumnName = conditionColumnName,
                            na.rm = TRUE
                            )
              )
          )
}

conditionSampleSize <- function(inputdf,
                         columnno = integer(), columnname = character(),
                         condition = character(),
                         conditionColumnNo = integer(), conditionColumnName = character()) {
  return( truelength(extractDataSet(inputdf,
                             selectColNo = columnno,
                             selectColName = columnname,
                             condition = condition,
                             conditionColumnNo = conditionColumnNo,
                             conditionColumnName = conditionColumnName,
                             na.rm = TRUE
                             )
                     )
          )
}

conditionsPValue <-  function(inputdf, analyzedColNo = integer(), analyzedColName = character(),
                              conditionSet = character(),
                              conditionColumnNo = integer(), conditionColumnName = character(),
                              parametric = TRUE) {
  # all checks are passed to extractDataSets
  shortdf <- extractDataSets(inputdf,
                             selectColNo = analyzedColNo,
                             selectColName = analyzedColName,
                             conditionSet = conditionSet,
                             conditionColumnNo = conditionColumnNo,
                             conditionColumnName = conditionColumnName,
                             na.rm = TRUE
                             )
  if ( 0 == sum(!is.na(shortdf)) ) {
    # filtering data failed
    # one last hope is that the data set had two columns, and analyzedColumnNo should have been guessed
    if ( ( 2 == dim(inputdf)[2]) ) {
      actualConditionColumnNo <- conditionColumnNumber(inputdf,
                                                       conditionColumnNo = conditionColumnNo,
                                                       conditionColumnName = conditionColumnName
                                                       )
      if (0 < length(na.omit(actualConditionColumnNo))) {
        # again, all checks are passed to extractDataSets
        shortdf <- extractDataSets(inputdf,
                                   selectColNo = (3 - actualConditionColumnNo),
                                   conditionSet = conditionSet,
                                   conditionColumnNo = actualConditionColumnNo,
                                   na.rm = TRUE
                                   )
      }
    }
  }
  if ( 0 == sum(!is.na(shortdf)) ) {
    # filtering data failed
    # no condition column was found
    warning("No valid condition column was given to, nor found by conditionsPValue")
    return(NA)
  }
  colnames(shortdf)[1] <- "condition"
  colnames(shortdf)[2] <- "dependentvariable"
  testedConditions <- levels(shortdf$condition)
  if ( 2 > length(testedConditions) ) {
    # fewer than two conditions are present, no statistical test can be performed
    warning("Fewer than two conditions are present, no statistical test can be performed by conditionsPValue.")
    return(NA)
  }
  if ( 2 == length(testedConditions) ) {
    if (parametric) {
      # return the p value of a t test
      somettest <- t.test(dependentvariable ~ condition, data = shortdf)
      return( as.numeric(somettest$p.value) )
    } else {
      # return the p value of a Mann-Whitney U test
      somemanwhitney <- wilcox.test(dependentvariable ~ condition, data = shortdf)
      return( as.numeric(somemanwhitney$p.value) )
    }
  } else {
    if (parametric) {
      # return the p value of an ANOVA
      someanova <- anova(lm(dependentvariable ~ condition, data = shortdf))
      return( as.numeric(someanova["Pr(>F)"][[1]][1]) )
    } else {
      # return the p value of a Kruskal Wallis test
      somekruskal <- kruskal.test(dependentvariable ~ condition, data = shortdf)
      return( as.numeric(somekruskal$p.value) )
    }
  }
}

deunderscorefy <- function(somestring = "") {
  if (0 < length(grep("__", somestring) ) ) {
    somestring2 <- gsub("$", ")", somestring)
    somestring2 <- gsub("__", " (", somestring2)
  } else {
    somestring2 <- somestring
  }
  somestring2 <- gsub( "_", " ", somestring2 )
  return(somestring2)
}

plotColumn <- function(inputdf,
                       analyzedColNo = integer(), analyzedColName = character(),
                       conditionSet = character(),
                       conditionColumnNo = integer(), conditionColumnName = character(),
                       textSize = 12,
                       legendOff = FALSE) {
## sample call:
# someplot <- plotColumn(inputdf, "activity_CPM_TCA_soluble_cell_extract", conds )
# print(someplot)
  #pas all checks to extractDataSets
  shortdf <- extractDataSets(inputdf,
                             selectColNo = analyzedColNo,
                             selectColName = analyzedColName,
                             conditionSet = conditionSet,
                             conditionColumnNo = conditionColumnNo,
                             conditionColumnName = conditionColumnName,
                             na.rm = TRUE)
  if ( 0 == sum(!is.na(shortdf)) ) {
    #no data fit conditions
    warning("No data satisfied conditions in plotColumn.")
    return(NA)
  }
  labelswrap  <- lapply(strwrap(levels(shortdf[,1]), 7, simplify = FALSE),
                        paste, collapse = "\n")
  thisylabel <- paste( strwrap( deunderscorefy( colnames(shortdf)[2] ),
                                28,
                                simplify = TRUE),
                       collapse = "\n")
  myplot <- ggplot(shortdf) +
    aes_string( x = colnames(shortdf)[1], y = colnames(shortdf)[2], fill = colnames(shortdf)[1]) +
    stat_summary(fun.y = mean, geom = "bar") +
    stat_summary(geom = 'errorbar',
                 fun.data = 'semInterval',
                 width = 0.1,
                 show_guide = FALSE) +
    ylab(thisylabel) +
    scale_x_discrete(labels = labelswrap) +
    expand_limits(y = 0) +
    theme_bw() +
    scale_fill_manual( values = meaningfulpalette ) +
    theme(text = element_text(size = textSize, color = "black"),
          panel.grid.major.x = element_blank(),
          panel.border = element_blank(),
          panel.background = element_blank(),
          axis.line = element_line(color = "black"),
          axis.ticks.x = element_blank(),
          axis.title.x = element_blank(),
          axis.title.y = element_text( size = textSize),
          axis.text.x = element_text( size = textSize * .8 ),
          axis.text.y = element_text( size = textSize * .5 ))
  if (legendOff) {
    myplot <- myplot + theme(legend.position = "none")
  }
  return(myplot)

}

tableColumns <- function(inputdf,
                         analyzedColNos = integer(), analyzedColNames = character(),
                         conditionSet = character(),
                         conditionColumnNo = integer(), conditionColumnName = character()) {
## sample calls
# grid.table( tableColumns(inputdf, depVars, conds),
#               gpar.coretext = gpar(fontsize = 7),
#               show.rownames = FALSE,
#               show.colnames = FALSE,
#               h.even.alpha = 0.5,
#  caption="Characteristics of the four fractions", label='table:rando')
# OR
#   print(xtable(tableColumns(inputdf, depVars, conds),
#                align = "p{0.001in}p{1in}p{0.4in}p{0.4in}p{0.4in}p{0.4in}p{0.4in}p{0.4in}p{0.4in}p{0.4in}p{0.5in}",
#                caption = "Comparison of the four fractions"),
#         hline.after = c(-1:(1+length(depVars))),
#         include.rownames = FALSE,
#         include.colnames = FALSE,
#         table.placement = "p")
  # pass all checks to extractMultiColumn
  shortdf <- extractMultiColumn(inputdf,
                                selectedColNos = analyzedColNos,
                                selectedColNames = analyzedColNames,
                                conditionSet = conditionSet,
                                conditionColumnNo = conditionColumnNo,
                                conditionColumnName = conditionColumnName,
                                incomplete.rm = FALSE
                                )
  if ( 0 == sum(!is.na(shortdf)) ) {
    # exit for being unable to find data satisfying conditons
    warning("No data fitting the conditions in tableColumns.")
    return(NA)
  }
  probedColumnCount <- (dim(shortdf)[2]) - 1
  probedColumnnames <- colnames(shortdf)[1+(1:probedColumnCount)]
  actualConditions <- verifiedConditionSet(shortdf,
                                             conditionSet = conditionSet,
                                             conditionColumnNo = 1)
  shortdf[,1] <- factor(shortdf[,1], levels = actualConditions)
  probedConditions <- levels(factor(actualConditions, levels = actualConditions))
  probedConditionCount <- length(probedConditions)
  #vectorial ops
  samplelengths <- aggregate(x = shortdf[, ( 1 + (1:probedColumnCount) )],
                             by = list(shortdf$condition),
                             FUN = truelength)
  colnames(samplelengths)[( "Group.1" == colnames(samplelengths) )] <- "condition"
  samplelengthsmin <- apply(samplelengths[, ( 1 + (1:probedColumnCount) )], 1, min )
  samplelengthsmax <- apply(samplelengths[, ( 1 + (1:probedColumnCount) )], 1, max )
  means <- aggregate(x = shortdf[, ( 1 + (1:probedColumnCount) )],
                     by = list(shortdf$condition),
                     FUN = mean,
                     na.rm = TRUE)
  colnames(means)[( "Group.1" == colnames(means) )] <- "condition"
  SEMs <- aggregate(x = shortdf[, ( 1 + (1:probedColumnCount) )],
                    by = list(shortdf[, 1]),
                    FUN = SEM)
  colnames(SEMs)[( "Group.1" == colnames(SEMs) )] <- "condition"

  statstable <- data.frame(row.names = c("header", probedColumnnames) )
  statstable[1,1] <- "Characteristic, mean (SEM)"
  for (j in (1 : (length(probedConditions)))) {
    statstable[1, j+1] <- paste0(
      deunderscorefy( probedConditions[j] ),
        " (n = ",
        ifelse( samplelengthsmin[j] == samplelengthsmax[j],
                samplelengthsmin[j],
                paste(samplelengthsmin[j], "-", samplelengthsmax[j])),
        ")"
      )
  }
  statstable[1, (length(probedConditions) + 2)] <- "P Value (parametric)"
  statstable[1, (length(probedConditions) + 3)] <- "P Value (non-parametric)"
  for (i in c(1 : probedColumnCount)) {
    statstable[ i+1, 1 ] <- deunderscorefy( probedColumnnames[i] )
    for (j in c(1:length(probedConditions))) {
      if ( 0 == samplelengths[j,i+1] ) {
        statstable[ i+1, j+1 ] <- "NA"
      } else if  ( 1  == samplelengths[j,i+1] ) {
        statstable[ i+1, j+1 ] <- paste0( format( means[j,i+1], digits = 3 ),
                                         " (NA)"
                                         )
      } else {
        statstable[ i+1, j+1 ] <- paste0( format( means[j,i+1], digits = 3 ),
                                         " (",
                                         format( SEMs[j,i+1], digits = 3 ),
                                         ")"
                                         )

      }
    }
    shorterdf <- extractDataSets(shortdf, conditionColumnNo = 1, selectColName = probedColumnnames[i], na.rm = TRUE)
    statstable[ i+1, length(probedConditions) + 2 ] <- format(conditionsPValue(shorterdf, conditionColumnNo = 1, parametric = TRUE), digits = 3)
    if ( 2 == length(levels(shorterdf$condition)) ) {
      statstable[ i+1, length(probedConditions) + 2 ] <- paste0(
        statstable[ i+1, length(probedConditions) + 2 ],
        ifelse(means[1,i+1] > means[2,i+1], " (down)", " (up)" )
        )
    }
    statstable[ i+1, length(probedConditions) + 3 ] <- format(conditionsPValue(shorterdf, conditionColumnNo = 1, parametric = FALSE), digits = 3)
  }
  return(statstable)
}

extractWithTwoIndependentVars <- function(inputdf,
                                          analyzedColNo = integer(), analyzedColName = character(),
                                          groupedConditionSet = character(),
                                          groupedConditionColumnNo = integer(), groupedConditionColumnName = character(),
                                          otherConditionSet = character(),
                                          otherConditionColumnNo = integer(), otherConditionColumnName = character()) {
  if ( 0 == sum(!is.na(inputdf)) ) {
    # exit if no data was given
    warning("No dataframe to analyze by extractWithTwoIndependentVars.")
    return(NA)
  }
  # delegate checks to extractDataSets
  actualGroupedConditionColumnNo <- columnNoAndNameToNo(inputdf,
                                                        columnno = groupedConditionColumnNo,
                                                        columnname = groupedConditionColumnName
  )
  actualOtherConditionColumnNo <- columnNoAndNameToNo(inputdf,
                                                      columnno = otherConditionColumnNo,
                                                      columnname = otherConditionColumnName
  )
  actualReturnedColumnNo <- columnNoAndNameToNo(inputdf,
                                                columnno = analyzedColNo,
                                                columnname = analyzedColName
  )
  if(3 != length(na.omit(c( actualGroupedConditionColumnNo,
                            actualOtherConditionColumnNo,
                            actualReturnedColumnNo
  )))) {
    # exit if no data was given
    warning("extractWithTwoIndependentVars did not receive three valid columns.")
    return(NA)
  }
  shortdf <- extractMultiColumn(inputdf,
                                selectedColNos = c(actualOtherConditionColumnNo, actualReturnedColumnNo),
                                conditionSet = groupedConditionSet,
                                conditionColumnNo = actualGroupedConditionColumnNo,
                                incomplete.rm = TRUE
  )
  shortdf <- extractMultiColumn(shortdf,
                                selectedColNos = c(1, 3),
                                conditionSet = otherConditionSet,
                                conditionColumnNo = 2,
                                incomplete.rm = TRUE
  )
  if ( 0 == sum(!is.na(shortdf)) ) {
    #no data fit conditions
    warning("No data satisfied conditions in extractWithTwoIndependentVars.")
    return(NA)
  }
  colnames(shortdf)[1] <- colnames(inputdf)[actualOtherConditionColumnNo]
  colnames(shortdf)[2] <- colnames(inputdf)[actualGroupedConditionColumnNo]
  colnames(shortdf)[3] <- colnames(inputdf)[actualReturnedColumnNo]
  return(shortdf)
}


plotColumnTwoGroups <- function(inputdf,
                       analyzedColNo = integer(), analyzedColName = character(),
                       groupedConditionSet = character(),
                       groupedConditionColumnNo = integer(), groupedConditionColumnName = character(),
                       otherConditionSet = character(),
                       otherConditionColumnNo = integer(), otherConditionColumnName = character(),
                       xtitle = character(),
                       textSize = 12,
                       legendOff = FALSE) {
  #delegate all checks to extractWithTwoIndependentVars
  shortdf <- extractWithTwoIndependentVars(inputdf,
                                           analyzedColNo = analyzedColNo, analyzedColName = analyzedColName,
                                           groupedConditionSet = groupedConditionSet,
                                           groupedConditionColumnNo = groupedConditionColumnNo, groupedConditionColumnName = groupedConditionColumnName,
                                           otherConditionSet = otherConditionSet,
                                           otherConditionColumnNo = otherConditionColumnNo, otherConditionColumnName = otherConditionColumnName)
  if ( 0 == sum(!is.na(shortdf)) ) {
    #no data fit conditions
    warning("No data satisfied conditions in plotColumnTwoGroups")
    return(NA)
  }
  labelswrap  <- lapply(strwrap(levels(shortdf[,1]), 7, simplify = FALSE),
                        paste, collapse = "\n")
  thisylabel <- paste( strwrap( deunderscorefy( colnames(shortdf)[2] ),
                                28,
                                simplify = TRUE),
                       collapse = "\n")
  myplot <- ggplot(shortdf) +
    aes_string( x = colnames(shortdf)[2], y = colnames(shortdf)[3], fill = colnames(shortdf)[1]) +
    stat_summary(fun.y = mean, geom = "bar", position="dodge") +
    stat_summary(geom = 'errorbar',
                 fun.data = 'semInterval',
                 width = 0.1,
                 show_guide = FALSE,
                 position=position_dodge(.9)) +
    expand_limits(y = 0) +
    theme_bw() +
    ylab(deunderscorefy(colnames(shortdf)[3])) +
    scale_fill_manual( values = meaningfulpalette, name = deunderscorefy(colnames(shortdf)[1])) +
    theme(text = element_text(size = textSize, color = "black"),
          panel.grid.major.x = element_blank(),
          panel.border = element_blank(),
          panel.background = element_blank(),
          axis.line = element_line(colour = "black"),
          axis.title.x = element_text( size = textSize),
          axis.title.y = element_text( size = textSize),
          axis.text.x = element_text( size = textSize * .7 ),
          axis.text.y = element_text( size = textSize * .5 ))
  if( 0 == sum(!is.na(xtitle)) ) {
    actualxlabel <- ""
  } else {
    actualxlabel <- xtitle
  }
  if ("" != actualxlabel) {
    myplot <- myplot + labs(x = actualxlabel)
  }
  if (legendOff) {
    myplot <- myplot + theme(legend.position = "none")
  }
  return(myplot)

}

loadNMRdata <- function(filepath = "", colsuffix = "") {
  if ( 0 == sum(!is.na(filepath)) ) {
    #no data fit conditions
    warning("Input file name missing.")
    return(NA)
  }
  if ( "" == filepath ) {
    #no data fit conditions
    warning("Input file name missing.")
    return(NA)
  }
  if ( 0 == sum(!is.na(colsuffix)) ) {
    #no data fit conditions
    colsuffix <- basename(filepath)
  }
  if ( "" == colsuffix ) {
    #no data fit conditions
    colsuffix <- basename(filepath)
  }
  inputdf <-
    read.table(filepath,
      header = TRUE,
      sep = "\t",
      skip = 1)
  colnames(inputdf)[colnames(inputdf) == "Label"] <- "animal"
  inputdf$Fatpermille <- 1000 * inputdf$Fat / inputdf$Weight
  inputdf$Leanpermille <- 1000 * inputdf$Lean / inputdf$Weight
  inputdf$FreeWaterpermille <- 1000 * inputdf$FreeWater / inputdf$Weight
  inputdf$TotalWaterpermille <- 1000 * inputdf$TotalWater / inputdf$Weight
  colnames(inputdf)[colnames(inputdf) == "Fat"] <- paste0("fat_body_weight_", colsuffix, "__g")
  colnames(inputdf)[colnames(inputdf) == "Lean"] <- paste0("lean_body_weight_", colsuffix, "__g")
  colnames(inputdf)[colnames(inputdf) == "FreeWater"] <- paste0("free_water_", colsuffix, "__g")
  colnames(inputdf)[colnames(inputdf) == "TotalWater"] <- paste0("total_water_", colsuffix, "__g")
  colnames(inputdf)[colnames(inputdf) == "Fatpermille"] <- paste0("fat_body_weight_", colsuffix, "__permille")
  colnames(inputdf)[colnames(inputdf) == "Leanpermille"] <- paste0("lean_body_weight_", colsuffix, "__permille")
  colnames(inputdf)[colnames(inputdf) == "FreeWaterpermille"] <- paste0("free_water_", colsuffix, "__permille")
  colnames(inputdf)[colnames(inputdf) == "TotalWaterpermille"] <- paste0("total_water_", colsuffix, "__permille")
  inputdf[["TimeDateDura"]] <- NULL
  inputdf[["Accumulation"]] <- NULL
  inputdf[["RecNumber"]] <- NULL
  inputdf[["Weight"]] <- NULL
  inputdf[["Comments"]] <- NULL
  return(inputdf)
}

loadWeightsFile <- function(filepath = "") {
  if ( 0 == sum(!is.na(filepath)) ) {
    #no data fit conditions
    warning("Input file name missing.")
    return(NA)
  }
  if ( "" == filepath ) {
    #no data fit conditions
    warning("Input file name missing.")
    return(NA)
  }
  inputdf <- read.csv(filepath)
  inputdf$animal <- as.character(inputdf$animal)
  daysEntered <- length(colnames(inputdf)) - 1
  colnames(inputdf)[ (colnames(inputdf) != "animal") ] <-
    paste0("body_weight_day_", (1:daysEntered), "__g")
  for (day in c(1:daysEntered)) {
    inputdf$newcol1 <- inputdf[[paste0("body_weight_day_", day, "__g")]] - inputdf[["body_weight_day_1__g"]]
    inputdf$newcol2 <- inputdf$newcol1 / inputdf[["body_weight_day_1__g"]]
    colnames(inputdf)[colnames(inputdf) == "newcol1"] <- paste0("body_weight_gain_day_", day, "__g")
    colnames(inputdf)[colnames(inputdf) == "newcol2"] <- paste0("body_weight_relative_gain_day_", day)
  }
  return(inputdf)
}

loadMusclesFile <- function(filepath = "") {
  if ( 0 == sum(!is.na(filepath)) ) {
    #no data fit conditions
    warning("Input file name missing.")
    return(NA)
  }
  if ( "" == filepath ) {
    #no data fit conditions
    warning("Input file name missing.")
    return(NA)
  }
  inputdf <- read.csv(filepath)
  inputdf$animal <- as.character(inputdf$animal)
  colnamesStore <- colnames(inputdf)
  for (somecolname in colnamesStore) {
    if ( grepl("1", somecolname) ) {
      #column name contains 1
      pairedcolname <- gsub("1", "2", somecolname)
      if (pairedcolname %in% colnamesStore) {
        inputdf$newcol <- rowMeans(data.frame(inputdf[[somecolname]], inputdf[[pairedcolname]]), na.rm = TRUE)
        colnames(inputdf)[colnames(inputdf) == "newcol"] <- gsub("1", "", somecolname)
        inputdf[[somecolname]] <- NULL
        inputdf[[pairedcolname]] <- NULL
      }
    }
  }
  return(inputdf)
}


appendNormalizedToBaselineColumn <- function(inputdf,
                           toNormalizeColNos = integer(), toNormalizeColNames = character(),
                           conditionColumnNo = integer(), conditionColumnName = character(),
                           baselineName = character() ) {
  ## returns a df with appended columns
  if ( 0 == sum(!(is.na(inputdf))) ){
    # no data frame to analyze
    warning("No dataframe to analyze in appendNormalizedToBaselineColumn")
    return(inputdf)
  }
  actualConditionColumnNo <- conditionColumnNumber(inputdf,
                                                   conditionColumnNo = conditionColumnNo,
                                                   conditionColumnName = conditionColumnName
    )
  if (0 == length(na.omit(actualConditionColumnNo))) {
    # no condition column was found
    warning("No valid condition column was given to, nor found by appendNormalizedToBaselineColumn")
    return(inputdf)
  }
  if ( ( 0 == length(na.omit(toNormalizeColNos))) & ( 0 == length(na.omit(toNormalizeColNames))) ) {
    # no columns were indicated, we'll take all numerical columns
    probedColumnNos <- numericColumnNos(inputdf)
  } else {
    probedColumnNos <- columnNosAndNamesToNos(inputdf,
                                              columnnos = toNormalizeColNos,
                                              columnnames = toNormalizeColNames)
  }
  if ( 0 == length(na.omit(probedColumnNos))) {
    # no data frame to analyze
    warning("No columns to analyze by appendNormalizedToBaselineColumn")
    return(inputdf)
  }
  probedColumnNames <- colnames(inputdf)[probedColumnNos]
  if ( 0 == sum(!(is.na(baselineName))) ) {
    # find the default baseline condition
    putativebaselineConditions <- c()
    for (potentialBaselineConditionName in potentialBaselineConditionNames) {
      if (tolower(potentialBaselineConditionName) %in% tolower(levels(inputdf[,actualConditionColumnNo]))) {
        putativebaselineConditions <- c(putativebaselineConditions,
                                        levels(inputdf[,actualConditionColumnNo])[tolower(levels(inputdf[,actualConditionColumnNo]))
                                                                                  %in%
                                                                                    tolower(potentialBaselineConditionName)]
        )
      }
    }
    putativebaselineConditions <- na.omit(unique(putativebaselineConditions))
    if ( 0 == length(putativebaselineConditions) ) {
      # no data frame to analyze
      warning("No baseline condition found in appendNormalizedToBaselineColumn")
      return(inputdf)
    }
    actualBaselineName <- putativebaselineConditions[1]
  } else {
    actualBaselineName <- baselineName[1]
  }
  baselineCases <- inputdf[(actualBaselineName == inputdf[,actualConditionColumnNo] ), ]
  if ( 0 == dim(baselineCases)[1] ) {
    warning("No cases in putative baseline condition")
    return(inputdf)
  }
  outputdf <- inputdf
  for (somecol in probedColumnNames) {
    if ("numeric" == class(inputdf[[somecol]])) {
      outputdf$newcol <- inputdf[[somecol]] / mean(baselineCases[[somecol]], na.rm=TRUE)
      colnames(outputdf)[colnames(outputdf) == "newcol"] <- gsub("_m?g", "_relative_to_baseline", somecol)
    }
  }
  return(outputdf)
}


plotTimeline <- function(inputdf,
                         analyzedColNo = integer(), analyzedColName = character(),
                         timeConditionSet = character(),
                         timeConditionColumnNo = integer(), timeConditionColumnName = character(),
                         otherConditionSet = character(),
                         otherConditionColumnNo = integer(), otherConditionColumnName = character(),
                         xtitle = character(),
                         textSize = 12,
                         legendOff = FALSE) {
  #delegate all checks to extractWithTwoIndependentVars
  shortdf <- extractWithTwoIndependentVars(inputdf,
                                           analyzedColNo = analyzedColNo, analyzedColName = analyzedColName,
                                           groupedConditionSet = timeConditionSet,
                                           groupedConditionColumnNo = timeConditionColumnNo, groupedConditionColumnName = timeConditionColumnName,
                                           otherConditionSet = otherConditionSet,
                                           otherConditionColumnNo = otherConditionColumnNo, otherConditionColumnName = otherConditionColumnName)
  if ( 0 == sum(!is.na(shortdf)) ) {
    #no data fit conditions
    warning("No data satisfied conditions in plotColumnTwoGroups")
    return(NA)
  }
  labelswrap  <- lapply(strwrap(levels(shortdf[,1]), 7, simplify = FALSE),
                        paste, collapse = "\n")
  thisylabel <- paste( strwrap( deunderscorefy( colnames(shortdf)[2] ),
                                28,
                                simplify = TRUE),
                       collapse = "\n")
  myplot <- ggplot(shortdf) +
    aes_string( x = colnames(shortdf)[2], y = colnames(shortdf)[3], group = colnames(shortdf)[1]) +
    stat_summary( geom = "line",
                  size = 2,
                  fun.y = mean,
                  position = position_dodge(.2),
                  aes_string(color = colnames(shortdf)[1]) ) +
    scale_color_manual( values = meaningfulpalette ) +
    #scale_color_manual(labels = levels(shortdf[ , 1]), values = levels(shortdf[ , 1])) +
    #stat_summary(fun.y = mean, geom = "bar", position="dodge") +
    #stat_smooth(size = 1) +
    stat_summary(geom = 'errorbar',
                 fun.data = 'semInterval',
                 width = 0.1,
                 show_guide = FALSE,
                 position = position_dodge(.2)) +
    expand_limits(y = 0) +
    #theme_bw() +
    ylab(deunderscorefy(colnames(shortdf)[3])) +
    #scale_fill_grey(name = deunderscorefy(colnames(shortdf)[1])) +
    theme(text = element_text(size = textSize, color = "black"),
          panel.grid.major.x = element_blank(),
          panel.border = element_blank(),
          panel.background = element_blank(),
          axis.line = element_line(colour = "black"),
          axis.title.x = element_text( size = textSize),
          axis.title.y = element_text( size = textSize),
          axis.text.x = element_text( size = textSize * .8 ),
          axis.text.y = element_text( size = textSize * .5 ))
  if( 0 == sum(!is.na(xtitle)) ) {
    actualxlabel <- ""
  } else {
    actualxlabel <- xtitle
  }
  if ("" != actualxlabel) {
    myplot <- myplot + labs(x = actualxlabel)
  }
  if (legendOff) {
    myplot <- myplot + theme(legend.position = "none")
  }
  return(myplot)

}


appendChangesColumn <- function(inputdf,
                                probedColNos = integer(),
                                probedColNames = character()) {
  ## returns a df with appended columns
  if ( 0 == sum(!(is.na(inputdf))) ){
    # no data frame to analyze
    warning("No dataframe to analyze in appendChangesColumn")
    return(inputdf)
  }
  if ( ( 0 == length(na.omit(probedColNos))) & ( 0 == length(na.omit(probedColNames))) ) {
    # no columns were indicated, we'll take all numerical columns
    actualprobedColumnNos <- numericColumnNos(inputdf)
  } else {
    actualprobedColumnNos <- columnNosAndNamesToNos(inputdf,
                                              columnnos = probedColNos,
                                              columnnames = probedColNames)
  }
  if ( 0 == length(na.omit(actualprobedColumnNos))) {
    # no data frame to analyze
    warning("No columns to analyze by appendChangesColumn")
    return(inputdf)
  }
  actualprobedColumnNames <- colnames(inputdf)[actualprobedColumnNos]
  actualprobedColumnNames <- c(
    actualprobedColumnNames[grepl("before", actualprobedColumnNames)],
    actualprobedColumnNames[grepl("after", actualprobedColumnNames)])
  outputdf <- inputdf
  for (somecol in actualprobedColumnNames) {
    colnamepattern <- gsub("before", "XXX", somecol)
    colnamepattern <- gsub("after", "XXX", colnamepattern)
    beforecolname <- gsub("XXX", "before", colnamepattern)
    aftercolname <- gsub("XXX", "after", colnamepattern)
    if ( (beforecolname %in% colnames(inputdf)) &  (aftercolname %in% colnames(inputdf)) ) {
      if ( ("numeric" == class(inputdf[[beforecolname]]) ) & ("numeric" == class(inputdf[[beforecolname]]) ) ) {
        abschangecolname <- gsub("XXX", "change", colnamepattern)
        relchangecolname <- gsub("__.*", "", gsub("XXX", "relative_change", colnamepattern))
        outputdf[[abschangecolname]] <- NULL
        outputdf[[relchangecolname]] <- NULL
        outputdf$abschange <- outputdf[[aftercolname]] - outputdf[[beforecolname]]
        outputdf$relchange <- outputdf$abschange / outputdf[[beforecolname]]
        colnames(outputdf)[colnames(outputdf) == "abschange"] <- abschangecolname
        colnames(outputdf)[colnames(outputdf) == "relchange"] <- relchangecolname
      }
    }
  }
  return(outputdf)
}

loadPMID12519877 <- function(datadirpath){
  df1 <- read.csv(file.path(datadirpath, "PMID12519877", "proteasomeactivity.csv"), header = FALSE, colClasses = c(NA, "NULL", NA))
  row.names(df1) <- df1$V1
  return(df1)
}

PMID12519877data <- loadPMID12519877(datadir)

reportstats <- function(invivodata, invivocolnames){
  myoutput <- ""
  invivodatasubset <- subset(invivodata, ((V2 == "D") | (V2 == "V") | (V2 == "C")) )
  invivodata$V2 <- factor(invivodata$V2, 
                          levels = c("V", "D", "T", "C"), 
                          labels = c("Vehicle", "Dexa", "Testo", "Dexa + Testo"))
  invivodatasubset$V2 <- factor(invivodatasubset$V2, 
                                levels = c("V", "D", "C"), 
                                labels = c("Vehicle", "Dexa", "Dexa + Testo"))
  
  for (i in 4:length(invivocolnames)) {
    myoutput <- paste0(myoutput, 
                       "# ", invivocolnames[[i]])
    myoutput <- paste(myoutput, 
                      pandoc.table.return(aggregate(invivodata[,i], list(invivodata$V2), mean, na.rm = TRUE), style = "rmarkdown"))
    kw <- kruskal.test(as.formula(paste(colnames(invivocolnames)[i], "~V2")), data = invivodata)
    myoutput <- paste(myoutput, 
                      paste0("Kruskal-Wallis p value for the four-way comparison is ", signif(kw$p.value, digits = 3)))
    dunns <- dunn.test(invivodata[,i], invivodata$V2, method = "bonferroni")
    dunnsreport <- data.frame(contrastsfour, dunns$P)
    myoutput <- paste(myoutput,
                      pandoc.table.return(dunnsreport, style = "rmarkdown"))
    kw <- kruskal.test(as.formula(paste(colnames(invivodatasubset)[i], "~V2")), data = invivodatasubset)
    myoutput <- paste(myoutput, 
                      paste0("Kruskal-Wallis p value for the three-way comparison is ", signif(kw$p.value, digits = 3)))
    dunns <- dunn.test(invivodatasubset[,i], invivodatasubset$V2, method = "bonferroni")
    dunnsreport <- data.frame(contraststhree, dunns$P)
    myoutput <- paste(myoutput,
                      pandoc.table.return(dunnsreport, style = "rmarkdown"))
    
  }
  return(myoutput)
}

#cat(myoutput)