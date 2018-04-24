#Final project part 2 and 3
# Huiyu Hu

library(dplyr)
library(ggplot2)

# Constants
#sampleSize <- 100
assessments <- c("Baseline","Exit","FOLLOWUP1","FOLLOWUP2")
assessmentPoints <- length(assessments)

baseMaleMean = c(3.2105, 2.5263, 5.0947, 2.2526, 6.4737)
baseMaleStd  = c(2.1826, 1.8728, 2.7447, 1.7982, 1.9066)

baseFemaleMean = c(4.1064, 2.1702, 4.2234, 2.3830, 7.0426)
baseFemaleStd  = c(2.1526, 1.7391, 2.5407, 1.6919, 2.0368)

# Treatment effect for four assessment points
treatmentEffect = c(1.0, 0.65, 0.8, 1.05)
treatmentEffectPBS = c(1.0, 1.35, 1.2, 0.95)

# Missing mechanism 1 and 2 probability
#probMissingOne <- 0.1
#probMissingTwo <- c(0.1,0.17,0.18,0.55)

####################################
# Function to conduct one round of t-test
OneRoundTest <- function (sampleSize, probMissingOne, probMissingTwo){
  
  # Step 1: create the empty data
  mydata <- data.frame("id" = 1:(sampleSize * assessmentPoints), 
                       "gender"=NA, "age"=NA, "assessment"=NA,
                       "ESS"=NA, "CPS"=NA, "IHA"=NA, "PPS"=NA, "PSB"=NA)
  mydata$id <- floor((mydata$id+3)/4)
  
  # Step 2: Assign gender
  genderData <- sample(c("Male", "Female"), size=sampleSize, replace=TRUE, prob=c(0.5,0.5))
  genderData <- as.data.frame(genderData)
  genderData <- genderData[rep(seq_len(sampleSize), each=4),]
  mydata$gender <- genderData
  
  # Step 3: Assign age
  ageData <- sample(4:17, sampleSize, replace=T) 
  ageData <- as.data.frame(ageData)
  ageData <- ageData[rep(seq_len(sampleSize), each=4),]
  mydata$age <- ageData
  
  # Stepb 4: Assign assessment point
  mydata$assessment <- rep(assessments, sampleSize, sampleSize * assessmentPoints, 1)
  
  for(i in 0:(sampleSize-1)){
    bl <- 4*i+1
    
    baseMean <- baseMaleMean
    baseStd <- baseMaleStd
    if(mydata[bl, 2]== "Female"){
      baseMean <- baseFemaleMean
      baseStd <- baseFemaleStd
    }
    
    # For BASE
    for (j in 1:4){
      mydata[bl, j+4] <- rnorm(1, treatmentEffect[1] * baseMean[j], baseStd[j])
    }
    # Special treatment effect for PBS
    mydata[bl, 9] <- rnorm(1, treatmentEffectPBS[1] * baseMean[5], baseStd[5])
    
    # For other three points
    for (j in 1:4){
      mydata[bl+1, j+4] <- rnorm(1, treatmentEffect[2] * mydata[bl, j+4], baseStd[j])
      mydata[bl+2, j+4] <- rnorm(1, treatmentEffect[3] * mydata[bl+1, j+4], baseStd[j])
      mydata[bl+3, j+4] <- rnorm(1, treatmentEffect[4] * mydata[bl+2, j+4], baseStd[j])
    }
    # Special treatment effect for PBS
    mydata[bl+1, 9] <- rnorm(1, treatmentEffectPBS[2] * mydata[bl, 9], baseStd[5])
    mydata[bl+2, 9] <- rnorm(1, treatmentEffectPBS[3] * mydata[bl+1, 9], baseStd[5])
    mydata[bl+3, 9] <- rnorm(1, treatmentEffectPBS[4] * mydata[bl+2, 9], baseStd[5])
  }
  
  #Apply missing mechnism one
  mydata_m1 <- mydata
  mydata_m1$ismissing <-"na"
  for(i in 0:(sampleSize - 1)){
    # BASE: no missing
    bl<-4*i+1
    mydata_m1[bl,10] <- "N"
    
    # Other three points, apply probMissingOne
    for (j in 1:3){
      mydata_m1[bl+j,10] <- sample(c("Y","N"), 1, replace=TRUE, prob=c(probMissingOne, (1 - probMissingOne)))
    }
  }
  
  for(i in 1:(sampleSize * assessmentPoints)){
    if (mydata_m1[i,10] == "Y"){
      for(j in 5:9){
        mydata_m1[i,j] <- NA
      }
    }
  }
  
  #2missing
  lastvisit <- sample(assessments, size=sampleSize, replace=TRUE, prob=probMissingTwo)
  mydata_m2<-mydata_m1
  lastvisit <-as.data.frame(lastvisit)
  lastv <- lastvisit[rep(seq_len(sampleSize), each=assessmentPoints),]
  mydata_m2$lastv <- lastv
  for(last in 1:sampleSize){
    if(lastvisit[last,]=="FOLLOWUP2"){
      next
    }
    
    # For other three quit points
    for (j in 5:9) {
      mydata_m2[4*last, j]<-NA
      if(lastvisit[last,]=="Exit"){
        mydata_m2[4*last - 1, j]<-NA
      }
      else if(lastvisit[last,]=="Baseline") {
        mydata_m2[4*last - 1, j]<-NA
        mydata_m2[4*last - 2, j]<-NA
      }
    }
  }
  
  #Final step
  for (i in 1:(assessmentPoints * sampleSize)){
    for(j in 5:9){
      if (!is.na(mydata_m2[i,j])){
        if(mydata_m2[i,j] <= 0) {
          mydata_m2[i,j] <- 0
        }
        
        if(mydata_m2[i,j] >=10) {
          mydata_m2[i,j] <-10
        }
        mydata_m2[i,j] <- round(mydata_m2[i,j])
      }
    }
  }
  finaldata <- mydata_m2
  #the final data used in t test should not include the part which last visit is baseline
  finaldatafortest <- filter(finaldata, finaldata$lastv != "Baseline")
  bldata <- finaldata[finaldata$assessment %in% c("Baseline"), ]
  lastdata <- filter(finaldata,finaldata$assessment==finaldata$lastv)
  lastdata <- data.frame(lastdata)
  ttest <- t.test(bldata$ESS, y = lastdata$ESS, paired = TRUE, alternative = "two.sided")
  #ttest$p.value
  #if (is.nan(ttest$p.value)){
  #  print(bldata$PSB)
  #  print(lastdata$PSB)
  #  return(0.0)
  #}
  return(ttest$p.value)
}
# Call OneRoundTest function to get p-value
OneRoundTest(100,0.1,c(0.1,0.17,0.18,0.55))


####################################
# Function to conduct 500 rounds of t-test and compute the rejection rate
ManyRoundTest <- function(sampleSize, probMissingOne, probMissingTwo){
  rejects <- 0
  
  # When probMissingOne is close to 1.0, PSB will be NA, resulting in t-test of Nan.
  # We need to exclude these tests
  ttestsNonNan <- 0
  rounds <- 500
  for (a in 1:rounds){
    # ptm <- Sys.time()
    pvalue <- OneRoundTest(sampleSize, probMissingOne, probMissingTwo)
    #cat(sprintf('Test Round %1.0f/%1.0f: p-value: %f, using time:%f \n', a, rounds, pvalue, Sys.time() - ptm))
    cat(sprintf('Test Round %1.0f/%1.0f with sample size %1.0f and missing prob %1.2f: p-value: %f \n', a, rounds, sampleSize, probMissingOne, pvalue))
    if (is.nan(pvalue)){
      next
    }
    ttestsNonNan <- ttestsNonNan + 1
    if (pvalue < 0.05) {
      rejects <- rejects + 1
    }
  }
  
  finalprop <- 100.0 * rejects / rounds
  cat(sprintf('T-test results with sample size %1.0f and missing prob %1.2f: %f%% for %1.0f/%1.0f rounds with meaningful t-tests \n', sampleSize, probMissingOne, finalprop, ttestsNonNan, rounds))
  return(finalprop)
  }#function end

# Call OneRoundTest function to get p-value
ManyRoundTest(100,0.1,c(0.1,0.17,0.18,0.55))

#########################################################################
# Test 1: Tests by varying sample size, 150 is close to 100%
# Test parameters
#sampleSize <- 300
probMissingOne <- 0.1
probMissingTwo <- c(0.1,0.17,0.18,0.55)

sampleSizeList <- seq(10, 200, 5)
results1 <- vector(length = length(sampleSizeList))
for (i in 1:length(sampleSizeList)) {
  results1[i] <- ManyRoundTest(sampleSizeList[i], probMissingOne, probMissingTwo)  
}

# Plot figure
results <- results1
results <- as.data.frame(results)
results$sampleSize <- sampleSizeList

sampleSizePlot <- ggplot(results, aes(x = sampleSize, y = results)) + geom_point() + geom_line(colour = 'red')
sampleSizePlot + labs(
  y="t-test rejection rate (%)", 
  x = "Sample Size", 
  title = "Percentage significant treatment effect \non sample size (ESS)") + theme(plot.title = element_text(hjust = 0.5))

###############################################################
# Test 2: Tests by varying missing prob
# Test parameters
sampleSize <- 100
#probMissingOne <- 0.1
probMissingTwo <- c(0.1,0.17,0.18,0.55)
#ManyRoundTest(sampleSize, probMissingOne, probMissingTwo)

probMissingOneList <- seq(0, 0.98, 0.02)
results2 <- vector(length = length(probMissingOneList))
for (i in 1:length(probMissingOneList)) {
  # 3 minutes per run, so total is about 300 minutes = 5 hours
  results2[i] <- ManyRoundTest(sampleSize, probMissingOneList[i], probMissingTwo)
}
results2

# Plot figure
results <- results2
results <- as.data.frame(results)
results$probMissingOne <- probMissingOneList

sampleSizePlot <- ggplot(results, aes(x = probMissingOne, y = results)) + geom_point() + geom_line(colour = 'red')
sampleSizePlot + labs(
  y="t-test rejection rate (%)", 
  x = "Probability of the First Missing Mechanism", 
  title = "Percentage significant treatment effect \non missing rate (ESS)") + theme(plot.title = element_text(hjust = 0.5))


