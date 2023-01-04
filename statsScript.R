# [TODO] set to the absolute path of the folder containing this script file 
source <- ""

################################################################################

libraries <- c("plotrix", "rstatix", "FSA", "gplots", "devtools", "scales", "ggradar", "gridExtra", "dplyr", "tibble", "fmsb")

for(mylibrary in libraries){
  ## [SC] installing packages
  if (!(mylibrary %in% rownames(installed.packages()))) {
    if (mylibrary == "ggradar"){
      devtools::install_github("ricardo-bion/ggradar", dependencies = TRUE)
    }
    else {
      install.packages(mylibrary)
    }
  }
  library(mylibrary, character.only = TRUE)
}

inputData <- "outputData/"

conceptTypesDF <- data.frame(
  sub=c('conamount', 'amount', 'eventquality', 'objconobjconpro', 'field', 
        'eveconobjconpro', 'boolfield', 'objconamount', 'network', 'distfield', 
        'aggre', 'object', 'objectquality', 'distanceBand', 'grid',
        'networkquality', 'allocation', 'proportion', 'event', 'covamount',
        'location', 'eveconamount'
        #, 'placename'
    ),
  super=c('amount', 'amount', 'quality', "proportion", "field",
          "proportion", "field", "amount", "network", "field",
          "aggre", "object", "quality", "field", "object",
          "quality", "field", "proportion", "event", "amount",
          "location", "amount"
          #, "object"
    ),
  stringsAsFactors=FALSE
)
conceptTypesDF <- conceptTypesDF[order(conceptTypesDF$super),]

allCorporaV <- c("Geo201", "GeoCLEF", "Giki", "GeoQuery", "GeoAnQu")


loadCorpusData <- function(corpusId, suffix="_r"){
  return(read.csv(paste0(source, inputData, corpusId, "_ParserStats", suffix, ".txt"), stringsAsFactors=FALSE, sep=";"))
}

subConceptTypeFreqPerCorpus <- function(){
  print("")
  print("=================================== START subConceptTypeFreqPerCorpus")
  
  typeFreqM <- matrix(nrow=nrow(conceptTypesDF), ncol=length(allCorporaV), dimnames=list(conceptTypesDF$sub, allCorporaV))
  
  #par(mfcol=c(2,3), mar=c(9, 4, 3, 3))
  par(mar=c(10, 2, 1, 1), oma=c(0,3,3,0))
  layout(matrix(c(1,2,3,4,5,5), nrow=3, ncol=2, byrow=TRUE))
  for(corpusId in allCorporaV){
    corpStatsDF <- loadCorpusData(corpusId)
    corpusSize <- nrow(corpStatsDF)
    
    for(cType in conceptTypesDF$sub){
      valuesV <- corpStatsDF[,cType]
      cTypeFreq <- round(length(valuesV[valuesV>0])/corpusSize,3)
      typeFreqM[cType, corpusId] <- cTypeFreq
    }
    
    tempM <- typeFreqM[order(typeFreqM[,corpusId]),]
    tempM <- tempM[tempM[,corpusId]!=0,]
    barplot(tempM[,corpusId], names.arg=rownames(tempM), ylim=c(0, 1), las=2
            , cex.names=1.3, cex.axis=1.3, cex.lab=1.3, cex.main=1.3
            , col="tan2", density=100, angle=45
            , main=corpusId, ylab="")
    print(paste0("Frequency of concepts - ", corpusId))
    print(tempM)
  }
  mtext("Frequency of concepts", side=3, line=1, outer=TRUE, font=2)
  mtext("Proportion of questions", side=2, line=1.5, outer=TRUE)
  
  if(TRUE){
  zeroIndexV <- numeric()
  nozeroIndexV <- numeric()
  tempTypeFreqM <- typeFreqM[,colnames(typeFreqM)!="GeoAnQu"]
  for(rowIndex in 1:nrow(tempTypeFreqM)){
    if(sum(tempTypeFreqM[rowIndex,]) > 0){
      nozeroIndexV <- c(nozeroIndexV, rowIndex)
    }
    else{
      zeroIndexV <- c(zeroIndexV, rowIndex)
    }
  }
  par(mfcol=c(2,1), mar=c(3, 4, 3, 3))
  barplot(t(typeFreqM[zeroIndexV,]), beside=TRUE, ylim=c(0, 0.2)
          , main="Types that occur in the GeoAnQu corpus only", ylab="Proportion of questions"
          )
  barplot(t(typeFreqM[nozeroIndexV,]), beside=TRUE, ylim=c(0, 1), legend=TRUE
          , main="Types that occur in corpora other than GeoAnQu", ylab="Proportion of questions")
  }
}


outputFrequencyPerCorpus <- function(){
  print("")
  print("=================================== START outputFrequencyPerCorpus")
  
  typeFreqM <- matrix(0, nrow=nrow(conceptTypesDF), ncol=length(allCorporaV), dimnames=list(conceptTypesDF$sub, allCorporaV))
  
  # par(mfcol=c(2,3), mar=c(9, 4, 3, 3))
  par(mar=c(10, 2, 1, 1), oma=c(0,3,3,0))
  layout(matrix(c(1,2,3,4,5,5), nrow=3, ncol=2, byrow=TRUE))
  for(corpusId in allCorporaV){
    corpStatsDF <- loadCorpusData(corpusId)
    corpStatsDF <- cbind(corpStatsDF, freq=1)
    outFreqDF <- aggregate(freq ~ qOutputType, corpStatsDF, sum)
    outFreqDF$freq <- round(outFreqDF$freq/nrow(corpStatsDF),3)
    outFreqDF <- outFreqDF[order(outFreqDF$freq),]
    barplot(outFreqDF$freq, names.arg=outFreqDF$qOutputType, ylim=c(0, 1), las=2
            , cex.names=1.3, cex.axis=1.3, cex.lab=1.3, cex.main=1.3
            , col="lightpink3", density=100, angle=45
            , main=corpusId, ylab="")
    
    for(rowIndex in 1:nrow(outFreqDF)){
      typeFreqM[outFreqDF$qOutputType[rowIndex],corpusId] <- outFreqDF$freq[rowIndex]
    }
  }
  mtext("Frequency of output concepts", side=3, line=1, outer=TRUE, font=2)
  mtext("Proportion of questions", side=2, line=1.5, outer=TRUE)
  
  nozeroIndexV <- numeric()
  for(rowIndex in 1:nrow(typeFreqM)){
    if(sum(typeFreqM[rowIndex,]) > 0){
      nozeroIndexV <- c(nozeroIndexV, rowIndex)
    }
  }
  typeFreqM <- typeFreqM[nozeroIndexV,]
  
  
  zeroIndexV <- numeric()
  nozeroIndexV <- numeric()
  tempTypeFreqM <- typeFreqM[,colnames(typeFreqM)!="GeoAnQu"]
  for(rowIndex in 1:nrow(tempTypeFreqM)){
    if(sum(tempTypeFreqM[rowIndex,]) > 0){
      nozeroIndexV <- c(nozeroIndexV, rowIndex)
    }
    else{
      zeroIndexV <- c(zeroIndexV, rowIndex)
    }
  }
  # par(mfcol=c(2,1), mar=c(3, 4, 3, 3))
  # barplot(t(typeFreqM[zeroIndexV,]), beside=TRUE, ylim=c(0, 0.2)
  #         , main="Output types that occur in the GeoAnQu corpus only", ylab="Proportion of questions")
  # barplot(t(typeFreqM[nozeroIndexV,]), beside=TRUE, ylim=c(0, 1), legend=TRUE
  #         , main="Output types that occur in a corpus other than GeoAnQu", ylab="Proportion of questions")
}


transStats <- function(){
  print("")
  print("=================================== START transStats")
  
  allCountDF <- data.frame(cId=NA, tCount=NA)
  
  # par(mfcol=c(2,5), mar=c(7, 4, 3, 3))
  for(corpusId in allCorporaV){
    corpStatsDF <- loadCorpusData(corpusId)
    
    transCountDF <- data.frame(transCount=corpStatsDF$qTransCount, freq=1)
    transCountDF <- aggregate(freq ~ transCount, transCountDF, sum)
    for(transCountVal in 0:6){
      if (!(transCountVal %in% transCountDF$transCount)){
        transCountDF <- rbind(transCountDF, data.frame(transCount=transCountVal, freq=0))
      }
    }
    transCountDF <- transCountDF[order(transCountDF$transCount),]
    transCountDF <- cbind(transCountDF, prop=round(transCountDF$freq/sum(transCountDF$freq), 3))
    
    # barplot(transCountDF$freq, names.arg=transCountDF$transCount
    #         , main=paste0("Number of transformation - ", corpusId)
    #         , xlab="Number of transformations", ylab="Frequency of questions")
    barplot(transCountDF$prop, names.arg=transCountDF$transCount
            #, main=paste0("Number of transformation - ", corpusId)
            , main=corpusId
            , xlab="Number of transformations", ylab="Proportion of questions", ylim=c(0, 1)
            , col="#b9e38d", density=100, angle=45
            , cex.axis=1.2, cex.names=1.2, cex.lab=1.2)
    
    allCountDF <- rbind(allCountDF, data.frame(cId=corpusId, tCount=corpStatsDF$qTransCount))
  }
  
  allCountDF <- allCountDF[-1,]
  
  # [SC] statistics based on frequencies of the unique type counts
  aggrMeanDF <- aggregate(tCount ~ cId, allCountDF, mean)
  colnames(aggrMeanDF)[colnames(aggrMeanDF)=="tCount"] <- "mean"
  
  aggrErrorDF <- aggregate(tCount ~ cId, allCountDF, std.error)
  colnames(aggrErrorDF)[colnames(aggrErrorDF)=="tCount"] <- "se"
  aggrMeanDF <- merge(aggrMeanDF, aggrErrorDF)
  
  aggrMedianDF <- aggregate(tCount ~ cId, allCountDF, median)
  colnames(aggrMedianDF)[colnames(aggrMedianDF)=="tCount"] <- "median"
  aggrMeanDF <- merge(aggrMeanDF, aggrMedianDF)
  
  print("===== Average number of transformations per question in each corpus")
  print(aggrMeanDF[order(aggrMeanDF$mean),])
  print("===== Statistics of the aggregated corpus")
  aggCorpusDF <- subset(allCountDF, cId != "GeoAnQu")
  print(paste0("Average number of transformations per question - Mean: ", mean(aggCorpusDF$tCount)
               , ", Mean SE: ", std.error(aggCorpusDF$tCount)
               , ", Median: ", median(aggCorpusDF$tCount)))
  
  print("===== Non-parametric alternative to ANOVA:")
  print(kruskal.test(tCount ~ cId, data = allCountDF))
  ## [SC] follow-up pairwise tests with Holm-Bonferroni P-value adjustment on per participant accuracies between cohorts
  ## [SC the Dunn's test is for independent groups
  print("===== Follow-up pairwise comparison:")
  print(dunn_test(tCount ~ cId, data = allCountDF, p.adjust.method = "bonferroni")) # [SC] uses rstatix package
  
  # [SC] linear regression of unique type count with "GeoCLEF" as intercept
  print("===== Linear regression of unique type count with GeoCLEF as intercept:")
  allCountFactorDF <- allCountDF
  allCountFactorDF$cId <- as.factor(allCountFactorDF$cId)
  allCountFactorDF <- within(allCountFactorDF, cId <- relevel(cId, ref = "GeoCLEF"))
  print(summary(lm(tCount ~ cId, data = allCountFactorDF)))
  
  print("===== Non-parametric alternative to ANOVA against aggregated corpus:")
  allCountDF[allCountDF$cId!="GeoAnQu",]$cId <- "aggregated"
  print(kruskal.test(tCount ~ cId, data = allCountDF))
  
  return(aggrMeanDF)
}

conceptStats <- function(){
  print("")
  print("=================================== START conceptStats")
  
  allCountDF <- data.frame(cId=NA, tCount=NA)
  
  # par(mfcol=c(2,5), mar=c(7, 4, 3, 3))
  for(corpusId in allCorporaV){
    corpStatsDF <- loadCorpusData(corpusId)
    
    typeCountDF <- data.frame(typeCount=corpStatsDF$qTypesCount, freq=1)
    typeCountDF <- aggregate(freq ~ typeCount, typeCountDF, sum)
    for(typeCountVal in 0:10){
      if (!(typeCountVal %in% typeCountDF$typeCount)){
        typeCountDF <- rbind(typeCountDF, data.frame(typeCount=typeCountVal, freq=0))
      }
    }
    typeCountDF <- typeCountDF[order(typeCountDF$typeCount),]
    typeCountDF <- cbind(typeCountDF, prop=round(typeCountDF$freq/sum(typeCountDF$freq), 3))
    
    # barplot(typeCountDF$freq, names.arg=typeCountDF$typeCount
    #         , main=paste0("Number of concepts - ", corpusId)
    #         , xlab="Number of concepts", ylab="Frequency of questions")
    barplot(typeCountDF$prop, names.arg=typeCountDF$typeCount
            #, main=paste0("# of concepts - ", corpusId)
            , main=corpusId
            , xlab="Number of concepts", ylab="Proportion of questions", ylim=c(0, 0.6)
            , col="#a1e9f0", density=100, angle=45
            , cex.axis=1.2, cex.names=1.2, cex.lab=1.2)
    
    allCountDF <- rbind(allCountDF, data.frame(cId=corpusId, tCount=corpStatsDF$qTypesCount))
  }
  
  allCountDF <- allCountDF[-1,]
  
  # [SC] statistics based on frequencies of the unique type counts
  aggrMeanDF <- aggregate(tCount ~ cId, allCountDF, mean)
  colnames(aggrMeanDF)[colnames(aggrMeanDF)=="tCount"] <- "mean"
  
  aggrErrorDF <- aggregate(tCount ~ cId, allCountDF, std.error)
  colnames(aggrErrorDF)[colnames(aggrErrorDF)=="tCount"] <- "se"
  aggrMeanDF <- merge(aggrMeanDF, aggrErrorDF)
  
  aggrMedianDF <- aggregate(tCount ~ cId, allCountDF, median)
  colnames(aggrMedianDF)[colnames(aggrMedianDF)=="tCount"] <- "median"
  aggrMeanDF <- merge(aggrMeanDF, aggrMedianDF)
  
  print("===== Average number of concepts per question in each corpus")
  print(aggrMeanDF[order(aggrMeanDF$mean),])
  print("===== Statistics of the aggregated corpus")
  aggCorpusDF <- subset(allCountDF, cId != "GeoAnQu")
  print(paste0("Average number of concepts per question - Mean: ", mean(aggCorpusDF$tCount)
               , ", Mean SE: ", std.error(aggCorpusDF$tCount)
               , ", Median: ", median(aggCorpusDF$tCount)))
  
  print("===== Non-parametric alternative to ANOVA:")
  print(kruskal.test(tCount ~ cId, data = allCountDF))
  ## [SC] follow-up pairwise tests with Holm-Bonferroni P-value adjustment on per participant accuracies between cohorts
  ## [SC the Dunn's test is for independent groups
  print("===== Follow-up pairwise comparison:")
  print(dunn_test(tCount ~ cId, data = allCountDF, p.adjust.method = "bonferroni")) # [SC] uses rstatix package
  
  # [SC] linear regression of unique type count with "GeoCLEF" as intercept
  print("===== Linear regression of unique type count with GeoCLEF as intercept:")
  allCountFactorDF <- allCountDF
  allCountFactorDF$cId <- as.factor(allCountFactorDF$cId)
  allCountFactorDF <- within(allCountFactorDF, cId <- relevel(cId, ref = "GeoCLEF"))
  print(summary(lm(tCount ~ cId, data = allCountFactorDF)))
  
  print("===== Non-parametric alternative to ANOVA against aggregated corpus:")
  allCountDF[allCountDF$cId!="GeoAnQu",]$cId <- "aggregated"
  print(kruskal.test(tCount ~ cId, data = allCountDF))
  
  return(aggrMeanDF)
}

plotMeans <- function(cMeanDF, tMeanDF){
  par(mfrow=c(1,2), mar=c(7, 2, 3, 1), oma=c(0,0,0,0))
  cMeanDF <- cMeanDF[order(match(cMeanDF$cId, allCorporaV)),]
  tMeanDF <- tMeanDF[order(match(tMeanDF$cId, allCorporaV)),]
  
  barplot2(cMeanDF$mean, plot.ci=TRUE
          , ci.l=(cMeanDF$mean-cMeanDF$se), ci.u=(cMeanDF$mean+cMeanDF$se)
          , names.arg=cMeanDF$cId, las=2
          , cex.names=1.3, cex.axis=1.3, cex.lab=1.3, cex.main=1.3
          , col="#a1e9f0", density=100, angle=45
          , main="Mean number of concepts", ylab="", xlab="")
  
  barplot2(tMeanDF$mean, plot.ci=TRUE
           , ci.l=(tMeanDF$mean-tMeanDF$se), ci.u=(tMeanDF$mean+tMeanDF$se)
           , names.arg=tMeanDF$cId, las=2
           , cex.names=1.3, cex.axis=1.3, cex.lab=1.3, cex.main=1.3
           , col="#b9e38d", density=100, angle=45
           , main="Mean number of transformations", ylab="", xlab="")
}

calcSubConceptTypeDiversityIndex <- function(){
  print("")
  print("=================================== START calcSubConceptTypeDiversityIndex")
  
  indexM <- matrix(NA, nrow=7, ncol=length(allCorporaV)
                   , dimnames=list(c("cTypeHi","cTypeQuHi"
                                     ,"cTypeSi","cTypeQuSi"
                                     ,"cTypeE","cTypeQuE"
                                     ,"richness")
                                   , allCorporaV))
  
  for(corpusId in allCorporaV){
    corpStatsDF <- loadCorpusData(corpusId)
    corpusSize <- nrow(corpStatsDF)
    
    freqStatDF <- data.frame(cType=NA, cTypeFreq=NA, cTypeQuFreq=NA, qProp=NA)
    for(cTypeN in conceptTypesDF$sub){
      valuesV <- corpStatsDF[,cTypeN]
      cTypeFreqN <- sum(valuesV)
      cTypeQuFreqN <- length(valuesV[valuesV>0])
      qPropN <- round(length(valuesV[valuesV>0])/corpusSize,3)
      
      freqStatDF <- rbind(freqStatDF, data.frame(cType=cTypeN, cTypeFreq=cTypeFreqN, cTypeQuFreq=cTypeQuFreqN, qProp=qPropN))
    }
    freqStatDF <- freqStatDF[-1,]
    
    freqStatDF <- cbind(freqStatDF, cTypeProp=round(freqStatDF$cTypeFreq/sum(freqStatDF$cTypeFreq), 3))
    freqStatDF <- cbind(freqStatDF, cTypeQuProp=round(freqStatDF$cTypeQuFreq/sum(freqStatDF$cTypeQuFreq), 3))
    
    subFreqStatDF <- subset(freqStatDF, cTypeQuFreq>0)
    
    print(paste(corpusId, corpusSize))
    # print(freqStatDF)
    print(subFreqStatDF)
    
    indexM["cTypeHi",corpusId] <- round(-sum(subFreqStatDF$cTypeProp*log(subFreqStatDF$cTypeProp)), 3)
    indexM["cTypeQuHi",corpusId] <- round(-sum(subFreqStatDF$cTypeQuProp*log(subFreqStatDF$cTypeQuProp)), 3)
    
    indexM["cTypeSi",corpusId] <- round(1/sum(subFreqStatDF$cTypeProp^2), 3)
    indexM["cTypeQuSi",corpusId] <- round(1/sum(subFreqStatDF$cTypeQuProp^2), 3)
    
    indexM["cTypeE",corpusId] <- round(indexM["cTypeHi",corpusId]/log(nrow(subFreqStatDF)), 3)
    indexM["cTypeQuE",corpusId] <- round(indexM["cTypeQuHi",corpusId]/log(nrow(subFreqStatDF)), 3)
    
    indexM["richness",corpusId] <- length(unique(subFreqStatDF$cType))
  }
  
  print("cTypeHi: Shannon index based on total frequency of concepts")
  print("cTypeQuHi: Shannon index based on question-based frequency of concepts")
  print("cTypeSi: Simpson index based on total frequency of concepts")
  print("cTypeQuSi: Simpson index based on question-based frequency of concepts")
  print("cTypeE: Pielou's evenness index based on cTypeHi")
  print("cTypeQuE: Pielou's evenness index based on cTypeQuHi")
  print(indexM)
  
  indexDF <- as.data.frame(t(indexM[c("richness","cTypeQuHi","cTypeQuE"),]))
  colnames(indexDF)[colnames(indexDF)=="richness"] <- "R"
  colnames(indexDF)[colnames(indexDF)=="cTypeQuHi"] <- "H'"
  colnames(indexDF)[colnames(indexDF)=="cTypeQuE"] <- "J'"
  print(indexDF)
  
  return(indexDF)
}

calcAggregateSubConceptTypeDiversityIndex <- function(){
  print("")
  print("=================================== START calcAggregateSubConceptTypeDiversityIndex")
  
  aggrCorpusDF <- NULL
  for(corpusId in allCorporaV){
    if (corpusId == "GeoAnQu") {
      next
    }
    
    corpStatsDF <- loadCorpusData(corpusId)
    if (is.null(aggrCorpusDF)) { aggrCorpusDF <- corpStatsDF }
    else { aggrCorpusDF <- rbind(aggrCorpusDF, corpStatsDF) }
  }
  
  corpusSize <- nrow(aggrCorpusDF)
  
  freqStatDF <- data.frame(cType=NA, cTypeFreq=NA, cTypeQuFreq=NA, qProp=NA)
  for(cTypeN in conceptTypesDF$sub){
    valuesV <- aggrCorpusDF[,cTypeN]
    cTypeFreqN <- sum(valuesV)
    cTypeQuFreqN <- length(valuesV[valuesV>0])
    qPropN <- round(length(valuesV[valuesV>0])/corpusSize,3)
    
    freqStatDF <- rbind(freqStatDF, data.frame(cType=cTypeN, cTypeFreq=cTypeFreqN, cTypeQuFreq=cTypeQuFreqN, qProp=qPropN))
  }
  freqStatDF <- freqStatDF[-1,]
  
  freqStatDF <- cbind(freqStatDF, cTypeProp=round(freqStatDF$cTypeFreq/sum(freqStatDF$cTypeFreq), 3))
  freqStatDF <- cbind(freqStatDF, cTypeQuProp=round(freqStatDF$cTypeQuFreq/sum(freqStatDF$cTypeQuFreq), 3))
  
  subFreqStatDF <- subset(freqStatDF, cTypeQuFreq>0)
  
  print(paste(corpusId, corpusSize))
  print(freqStatDF)
  print(subFreqStatDF)
  
  cTypeHi <- round(-sum(subFreqStatDF$cTypeProp*log(subFreqStatDF$cTypeProp)), 3)
  cTypeQuHi <- round(-sum(subFreqStatDF$cTypeQuProp*log(subFreqStatDF$cTypeQuProp)), 3)
    
  print(paste("cTypeHi: Shannon index based on total frequency of concepts",cTypeHi))
  print(paste("cTypeQuHi: Shannon index based on question-based frequency of concepts",cTypeQuHi))
  print(paste("cTypeSi: Simpson index based on total frequency of concepts",round(1/sum(subFreqStatDF$cTypeProp^2), 3)))
  print(paste("cTypeQuSi: Simpson index based on question-based frequency of concepts",round(1/sum(subFreqStatDF$cTypeQuProp^2), 3)))
  print(paste("cTypeE: Pielou's evenness index based on cTypeHi",round(cTypeHi/log(nrow(subFreqStatDF)), 3)))
  print(paste("cTypeQuE: Pielou's evenness index based on cTypeQuHi",round(cTypeQuHi/log(nrow(subFreqStatDF)), 3)))
}

calcOutputDiversityIndex <- function(){
  print("")
  print("=================================== START calcOutputDiversityIndex")
  
  indexM <- matrix(NA, nrow=4, ncol=length(allCorporaV)
                   , dimnames=list(c("cTypeQuHi","cTypeQuSi","cTypeQuE","richness")
                                   , allCorporaV))
  
  for(corpusId in allCorporaV){
    corpStatsDF <- loadCorpusData(corpusId)
    corpusSize <- nrow(corpStatsDF)
    corpStatsDF <- cbind(corpStatsDF, freq=1)
    outFreqDF <- aggregate(freq ~ qOutputType, corpStatsDF, sum)
    outFreqDF <- cbind(outFreqDF, prop=round(outFreqDF$freq/corpusSize,3))
    outFreqDF <- outFreqDF[order(outFreqDF$freq),]
    
    print(paste(corpusId, corpusSize))
    print(outFreqDF)
    print(nrow(outFreqDF))
    
    indexM["cTypeQuHi",corpusId] <- round(-sum(outFreqDF$prop*log(outFreqDF$prop)),3)
    indexM["cTypeQuSi",corpusId] <- round(1/sum(outFreqDF$prop^2), 3)
    indexM["cTypeQuE",corpusId] <- round(indexM["cTypeQuHi",corpusId]/log(nrow(outFreqDF)), 3)
    indexM["richness",corpusId] <- length(unique(outFreqDF$qOutputType))
  }
  
  print("cTypeQuHi: Shannon index based on question-based frequency of concepts")
  print("cTypeQuSi: Simpson index based on question-based frequency of concepts")
  print("cTypeQuE: Pielou's evenness index based on cTypeQuHi")
  print(indexM)
  
  indexDF <- as.data.frame(t(indexM[c("richness","cTypeQuHi","cTypeQuE"),]))
  colnames(indexDF)[colnames(indexDF)=="richness"] <- "R"
  colnames(indexDF)[colnames(indexDF)=="cTypeQuHi"] <- "H'"
  colnames(indexDF)[colnames(indexDF)=="cTypeQuE"] <- "J'"
  print(indexDF)
  
  return(indexDF)
}

calcAggregateOutputDiversityIndex <- function(){
  print("")
  print("=================================== START calcAggregateOutputDiversityIndex")
  
  aggrCorpusDF <- NULL
  for(corpusId in allCorporaV){
    if (corpusId == "GeoAnQu") {
      next
    }
    
    corpStatsDF <- loadCorpusData(corpusId)
    if (is.null(aggrCorpusDF)) { aggrCorpusDF <- corpStatsDF }
    else { aggrCorpusDF <- rbind(aggrCorpusDF, corpStatsDF) }
  }
  
  corpusSize <- nrow(aggrCorpusDF)
  aggrCorpusDF <- cbind(aggrCorpusDF, freq=1)
  outFreqDF <- aggregate(freq ~ qOutputType, aggrCorpusDF, sum)
  outFreqDF <- cbind(outFreqDF, prop=round(outFreqDF$freq/corpusSize,3))
  outFreqDF <- outFreqDF[order(outFreqDF$freq),]
  
  print(paste("Aggregate corpus", corpusSize))
  print(outFreqDF)
  print(nrow(outFreqDF))
  
  cTypeQuHi <- round(-sum(outFreqDF$prop*log(outFreqDF$prop)),3)
  
  print(paste("cTypeQuHi: Shannon index based on question-based frequency of concepts",cTypeQuHi))
  print(paste("cTypeQuSi: Simpson index based on question-based frequency of concepts",round(1/sum(outFreqDF$prop^2), 3)))
  print(paste("cTypeQuE: Pielou's evenness index based on cTypeQuHi",round(cTypeQuHi/log(nrow(outFreqDF)), 3)))
}

plotDiversity <- function(allDivDF, outDivDF){
  all_index_radar <- allDivDF %>%
    as_tibble(rownames = "cId") %>%
    mutate_at(vars(-cId), rescale)
  all_index_radar <- ggradar(all_index_radar, plot.title="Diversity of all concepts"
                             , legend.text.size=12, legend.position="right"
                             , group.point.size=3, group.line.width=1)

  out_index_radar <- outDivDF %>%
    as_tibble(rownames = "cId") %>%
    mutate_at(vars(-cId), rescale)
  out_index_radar <- ggradar(out_index_radar, plot.title="Diversity of goal concepts"
                             , legend.text.size=12, legend.position="right"
                             , group.point.size=3, group.line.width=1)
  
  grid.arrange(all_index_radar, out_index_radar, nrow = 2)
}

par(mfrow=c(2,5), mar=c(4.5, 4.5, 3, 1))
conceptMeanDF <- conceptStats()
transMeanDF <- transStats()
plotMeans(conceptMeanDF, transMeanDF)

subConceptTypeFreqPerCorpus()
allDiversDF <- calcSubConceptTypeDiversityIndex()
calcAggregateSubConceptTypeDiversityIndex()

outputFrequencyPerCorpus()
outDiversDF <- calcOutputDiversityIndex()
calcAggregateOutputDiversityIndex()

plotDiversity(allDiversDF,outDiversDF)
