# functions used in paper
# functions will appear in order in which they are used
# all function defintions appear before they are called

cooccurence <- function(character1,character2,extinctDataset){
  sum((extinctDataset[character1,]-extinctDataset[character2,])==0,na.rm=T) -> tempSum
  return(tempSum/ncol(extinctDataset))
}

createCorrelationMatrix <- function(extinctDataset){
  output <- matrix(nrow=nrow(extinctDataset),ncol=nrow(extinctDataset))
  for(trait1 in 1:nrow(extinctDataset)-1){
    for(trait2 in 1:nrow(extinctDataset)-1){
      cooccurence(
        character1=trait1, 
        character2=trait2+1,
        extinctDataset=extinctDataset) -> output[trait1,trait2+1]
    }
  }
  # fill in values for the last row of correlation matrix
  output[nrow(extinctDataset),] <- output[,nrow(extinctDataset)]
  output[nrow(extinctDataset), nrow(extinctDataset)] <- 1
  return(output)
}

calculateGlobalCharacterLoss <- function(extinctDataset=extinctDataset){

  GlobalLoss<-function(trait,data){
    1 - (sum(data[trait,]>=0)/ncol(data))
  }
  
  sapply(1:nrow(extinctDataset),GlobalLoss,data=extinctDataset) -> output
  output
}

FPSsample <- function(weights){
  #FPS (fitness proportionate sampling) is a genetic algorithm that samples values based on their "fitness"
  #FPS sampling is done by: 
  
  # 1) sorting the weights while retaining information about original indicies
  sortedChars <- sort(weights,index.return=T)

  # 2) normalizing the weights such that the sum of all weights = 1
  sortedChars[[1]] <- sortedChars[[1]]/sum(sortedChars[[1]])
  
  # 3) transforming the normalized weights so each value shows the cumulative sum
  sortedChars[[1]] <- cumsum(sortedChars[[1]])

  # 4) generate a number from a uniform distribution, and finding which value of sorted characters corresponds to that random number
  min(which(sortedChars[[1]]>=runif(1))) -> selected
  
  sortedChars[[2]][selected]
}

computeWeights <- function(
  initialWeights,
  characterCorrelations,
  removedChars,
  whichLostChar,
  globalLoss){  

    # in order include info about correlations with previous characters lost in simulation:

    # 1) identify the character lost via removedChars[whichLostChar]
    characterLost <- removedChars[whichLostChar]
    
    # 2) calculate the conditional probability of losing other characters given "characterLost"
    adjustedOccurence <- initialWeights*characterCorrelations[characterLost,]
    
    # 3) divide by the probability of losing "characterLost" to fit Bayes' theorem
    adjustedOccurence <- adjustedOccurence/globalLoss[characterLost]
    
    return(adjustedOccurence)
}

withoutReplacement <- function(weights,characterLost){
  weights[characterLost] <- 0
  return(weights)
}

updateCharsLost <- function(charsLost,lastCharLost,numCharsLost){
  charsLost[numCharsLost] <- lastCharLost
}


chooseCharsToRemove <- function(
  numCharsToLose,
  weights,
  correlations,
  globalLoss,
  softChars,
  maxLength,
  totalCharsLost,
  totalCharsToBeLost,
  whichIndividualInGroup
  ){

  #added parameter whichIndividualInGroup to get information about already missing characters
  #in this case, the extant matrix is actually the group with less missing characters
  
  #before running algorithm, set probabilities of already missing characters to 0
  currentOccurence <- weights

  alreadyMissing<-which(is.na(extantDataset[,whichIndividualInGroup]))
  for(z in as.numeric(alreadyMissing)){
    withoutReplacement(currentOccurence,z)->currentOccurence  
  }

  #debugging outputs
  cat(length(which(currentOccurence==0))," ",file="occurenceAdjustmentLogSUMS.txt",append=T)
  cat(which(currentOccurence==0),"\n",file="occurenceAdjustmentLogCHARS.txt",append=T)
  
  # removedChars is empty output, will be filled in in the following loops
  removedChars <- rep(NA, numCharsToLose)
  numRemovedChars <- 0

  while(numRemovedChars<numCharsToLose){
    if(totalCharsLost==totalCharsToBeLost)
      break
    if(numRemovedChars>=softChars){
      FPSsample(weights=weights) -> candidate
    } else { #seed soft characters
      candidate <- length(weights)-numRemovedChars
    }
    
    numRemovedChars <- numRemovedChars+1
    totalCharsLost <- totalCharsLost+1
    
    removedChars[numRemovedChars] <- as.numeric(candidate)

    computeWeights(
      initialWeights=weights,
      characterCorrelations=correlations,
      removedChars=removedChars,
      whichLostChar=numRemovedChars,
      globalLoss=globalLoss) -> weights #multiplies global with co-occurrence

    withoutReplacement(
      weights,
      candidate) -> weights #sets sampled index weight to 0, no replacement
    
    totalCharsLost <- totalCharsLost+1

    updateCharsLost(
      removedChars,
      candidate,
      numRemovedChars)
  }
  
  # manually set the length of removedChars to create complete matrix
  # empty entries will be populated with NA values
  length(removedChars) <- maxLength
  return(list(totalCharsLost,removedChars))
}

chooseCharsNolinkage <- function(
  numCharsToLose,
  GlobalLoss,
  maxLength=NULL) {
  sample(length(GlobalLoss), numCharsToLose, prob=GlobalLoss) -> removedChars

  # (OPTIONAL) force length of characters to remove
  if(!is.null(maxLength))
    length(removedChars) <- maxLength
  
  return(removedChars)
}

adjustNumCharsToRemove <- function(
  charsToRemove,
  numSims,
  averageCharactersMostMissing,
  numTaxa,
  within,
  extantDataset
  ){
  output <- as.vector(charsToRemove)
  cat("number of taxa:\t",numTaxa,"\n")
  alreadyMissing <- sum(is.na(extantDataset))
  
  for(simNum in 1:numSims){
    tempCharsToRemove <- output[(((simNum - 1)*numTaxa) + 1):((simNum)*numTaxa)]
    cat(sum(tempCharsToRemove) + alreadyMissing,"vs.\t",averageCharactersMostMissing*numTaxa*(1 - within),"\n")
    
    while(sum(tempCharsToRemove) + alreadyMissing < (averageCharactersMostMissing*numTaxa*(1 - within))){
      tempCharsToRemove <- tempCharsToRemove + 1
      cat(sum(tempCharsToRemove) + alreadyMissing,"vs.\t",averageCharactersMostMissing*numTaxa*(1 - within),"\n")
    }
    output[((simNum*numTaxa) - numTaxa + 1):(((simNum)*numTaxa))] <- tempCharsToRemove
  }
  
  output
}

adjustSoftCharCap <- function(
  charsToRemove,
  numSims,
  extantDataset,
  cap) {
  
  numTaxa <- ncol(extantDataset)
  output <- as.vector(charsToRemove)
  alreadyMissing <- vector(mode="numeric",ncol(extantDataset))
  
  for(i in 1:ncol(extantDataset)){
    sum(is.na(extantDataset[,i])) -> alreadyMissing[i]
  }
  
  for(simNum in 1:numSims){
    tempCharsToRemove <- output[(((simNum - 1)*numTaxa) + 1):((simNum)*numTaxa)]
    rep(nrow(extantDataset),ncol(extantDataset)) - tempCharsToRemove - alreadyMissing -> differences
    exceed <- differences<cap
    tempCharsToRemove[exceed] <- rep(nrow(extantDataset),ncol(extantDataset))[exceed] - alreadyMissing[exceed] - cap
    output[((simNum*numTaxa) - numTaxa + 1):(((simNum)*numTaxa))] <- tempCharsToRemove
  }
  
  if(any(exceed)){
    cat("capping characters at",cap,"\n")
  }
  output
}

simulateCharacterLoss<-function(
  extinctDataset,
  extantDataset,
  numSims=1000,
  numCharsToLose=1,
  linkage=T,
  softChars=11,
  sigma=NULL,
  normDist=F,
  poisDist=T,
  withinPercent=withinPercent,
  characterAdjustment=characterAdjustment,
  cap=cap,
  protectedTaxa=0){

  #argument checks
  if(normDist==T&poisDist==T)
    stop("Normal and Poisson Distribution were BOTH chosen.")
  if(normDist==T&is.null(sigma))
    stop("No sigma value specified for normal distribution")
  
  extinctDataset -> extinctDatasetChars #import full dataset  
  (!is.na(extinctDataset))*1 -> extinctDataset
  
  createCorrelationMatrix(extinctDataset)->correlationMatrix
  calculateGlobalCharacterLoss(extinctDataset)->GlobalLoss

  
  array(dim=c(nrow(extinctDataset),ncol(extinctDataset),numSims))->charsLostArray
  
  if(normDist==F&poisDist==F) { #using uniform distribution of character loss
    numCharsToLose -> maxCharsLost
    for(simulationNumber in 1:numSims){
      totalCharsLost <- 0
      totalCharsToBeLost <- numCharsToLose*ncol(extantDataset)
      for(taxon in 1:ncol(extantDataset)) {
        if(linkage) {
          chooseCharsToRemove(
            numCharsToLose=numCharsToLose,
            weights=GlobalLoss,
            correlations=correlationMatrix,
            globalLoss=GlobalLoss,
            softChars=softChars,
            maxLength=nrow(extantDataset),
            totalCharsLost=totalCharsLost,
            totalCharsToBeLost=totalCharsToBeLost,
            whichIndividualInGroup=taxon
          ) -> characterLossOutput
          
          characterLossOutput[[1]] -> totalCharsLost
          characterLossOutput[[2]] -> charsLostArray[,taxon,simulationNumber] 
        
        } else {
          chooseCharsNolinkage(
            numCharsToLose=numCharsToLose,
            globalLoss=GlobalLoss) -> charsLostArray[,taxon,simulationNumber]
        }
      }
    }
  } 
  else {#normDist or poisDist
    charsToRemove=vector(mode="numeric",length=ncol(extantDataset)*numSims)
    #charsToRemove is a vector of the number of characters that should be lost for each taxon for all sims
    k=1 #counter for while loop
    
    if(normDist==T){
      while(k<=length(charsToRemove)){
        tempNum=rnorm(1,mean=numCharsToLose,sd=sigma)
        if(tempNum>=0&tempNum<=nrow(extantDataset)){
          charsToRemove[k]=tempNum
          k=k+1
        }
      }
      numCharsLost<<-mean(charsToRemove)
    }
    
    if(poisDist==T){
      while(k<=length(charsToRemove)){
        tempNum=rpois(1,lambda=numCharsToLose)
        
        if(tempNum>=0&tempNum<=(nrow(extantDataset)-5)){

          #protectedTaxa is the index of data that should be protected
          if(!any(is.na(protectedTaxa))){
            if(any(k%%ncol(extantDataset)==protectedTaxa)){
              charsToRemove[k]=0
            } else {
              charsToRemove[k]=tempNum #charsToRemove is a vector of the number of characters that should be lost for each taxon for all sims
            }
         
          }
            else {
              charsToRemove[k]=tempNum #charsToRemove is a vector of the number of characters that should be lost for each taxon for all sims
            }
      

         k=k+1
        }
      }
      numCharsLost<<-mean(charsToRemove)
    }
    
    if(!is.null(cap)){
      adjustSoftCharCap(charsToRemove=charsToRemove,numSims=numSims,extantDataset=extantDataset,cap=cap)
    }
    
    if(characterAdjustment){
      adjustNumCharsToRemove(charsToRemove=charsToRemove,
                             numSims=numSims,
                             averageCharactersMostMissing=charactersMostMissing,
                             numTaxa=ncol(extantDataset),
                             within=withinPercent,
                             extantDataset=extantDataset)->charsToRemove
    }
    
    
    nrow(extinctDataset)->maxCharsLost
    array(dim=c(nrow(extinctDataset),ncol(extantDataset),numSims))->charsLostArray
    
    #convert to matrix
    charsToRemove=matrix(round(charsToRemove),ncol=ncol(extantDataset),byrow=T) #each row is a simulation, each column is a taxon

    print(charsToRemove)

    
    totalCharsToBeLost<<-numCharsToLose*ncol(extantDataset)
    for(i in 1:numSims)
    {
      
      totalCharsLost<<-0
      
      randomIndex=1:ncol(extantDataset) #cant randomize anymore since keeping track of previous NAs
      for(j in 1:(ncol(extantDataset))){
        if(linkage){
          chooseCharsToRemove(
            charsToRemove[i,j],
            GlobalLoss,
            correlationMatrix,
            GlobalLoss,
            softChars,
            maxCharsLost,
            totalCharsLost,
            totalCharsToBeLost,
            j) ->> tempSim
          tempSim[[2]]->charsLostArray[,randomIndex[j],i]
        } else {
          chooseCharsNoInteraction(charsToRemove[i,j],GlobalLoss,maxCharsLost)->charsLostArray[,j,i]
        }
      }
    }
  }
  
  endTime<-proc.time()[3]
  
  charsLostArray
}




removeCharacters <- function(
  taxa,
  dataMatrix,
  lossMatrix,
  sim) {

  dataMatrix[lossMatrix[,taxa,sim],taxa,sim] <- NA
  dataMatrix[,taxa,sim] }

###Apply character loss array to extantDataset
applyToExtant <- function(
  simArray, 
  extantDataset) {  
    # get dimensions

  numSims <- dim(simArray)[3]
  numChars <- dim(extantDataset)[1]
  numTaxa <- dim(simArray)[2]
  
  #create array of extant before removing
  output <- rep(extantDataset,numSims)
  dim(output) <- c(numChars,numTaxa,numSims)
  
  #for loop to remove characters for each simulation
  for(simNum in 1:numSims) {#i is each simulation
    for(taxNum in 1:numTaxa) {
      removeCharacters(taxNum, output, simArray, simNum) -> output[,taxNum,simNum]
    }
  } 
  return(output)
}


applyToExtinct<-function(simArray)
{
  
  #get dimensions
  extinctData=extinctWithChars
  numSims<-dim(simArray)[3]
  numChars<-dim(extinctData)[1]
  numTaxa<-dim(simArray)[2]
  
  #create array of extant before removing
  output<-rep(as.matrix(extinctData),numSims)
  dim(output)<-c(numChars,numTaxa,numSims)
  
  #for loop to remove characters for each simulation
  for(i in 1:numSims) #i is each simulation
  {
    for(j in 1:numTaxa)
    {
      removeCharacters(j,output,simArray,i)->output[,j,i]
    }
  }
  
  output
}


correctionFactor<-function(
  Data=Data,
  numSims=1000,
  linkage=T,
  softChars=0,
  sigma=NULL,
  normDist=F,
  poisDist=T,
  axes=10,
  withinPercent=0.05,
  characterAdjustment=T,
  cap=5){
  
  
  extinctWithChars<<-Data[-(1:3),]

#create vectors that have information about groups
#allows for single taxon to be included in multiple groups, separated via |
groupsVector<<-as.character(as.matrix(Data[2,]))

#create vector for every "group" indicating which taxa belong to each group
groupBins <- list()
allLevelsList <- strsplit(groupsVector,split="\\|")
allLevels <- unlist(allLevelsList)
allLevels <- allLevels[allLevels!="\\|"]
groupLevels <- unique(allLevels)

for(currentLevel in groupLevels){
  tempBin <- NULL
  for(taxon in 1:length(allLevelsList)){
    if(any(allLevelsList[[taxon]]==currentLevel))
      tempBin <- c(tempBin, taxon)
  }
  groupBins[[currentLevel]] <- tempBin
}
groupBins<<-groupBins

importExtinct=Data[-(1:3),]

for(i in 1:ncol(importExtinct)){
  importExtinct[,i]=as.numeric(as.character(importExtinct[,i]))
}
extinctDataset <- importExtinct

  
  
  
  
  ###divide into groups and find which are the lowest groups###
  groupMostMissing<<-NULL
  charactersMostMissing<<-0
  
  #sort the bins from most missing to least missing
  binSummary <- data.frame(group=groupLevels, percentMissing=rep(NA,length(groupLevels)))
  
  for(group in groupLevels){
    tempGroup=importExtinct[,groupBins[[group]]]
    tempMissing=sum(is.na(tempGroup))/length(groupBins[[group]])
    binSummary[binSummary[,1]==group,2] <- tempMissing
    if(tempMissing>charactersMostMissing){
      charactersMostMissing<<-tempMissing
      groupMostMissing<<-group
    }
  }
  
  binSummary <- binSummary[sort(binSummary[,2],index.return=T,decreasing=T)[[2]],]
  groupLevels <- as.character(binSummary[,1])
    
  #following code calls global data extinct and extant datasets. We can manipulate the following code by iterating changes in extant dataset
  
  simulatedCharLossArray <<- array(data=rep(-1,numSims*nrow(importExtinct)*ncol(importExtinct)),dim=c(nrow(importExtinct),ncol(importExtinct),numSims))
  
  for(group in groupLevels){
    cat("\ngroup:\t",group,"\n")
    tempIndex=groupBins[[group]]
    protectedTaxa = 0
    # Keep track of taxa which are also in the group with most loss
    if(group != groupMostMissing){
      intersect(groupBins[[group]], groupBins[[groupMostMissing]]) -> tempProtect
      if(length(tempProtect)>0)
        protectedTaxa <- tempProtect
    }

    extantDataset <<- importExtinct[,tempIndex]
    extantProtect <<- match(protectedTaxa, tempIndex)
    extantDataset ->> importExtant
    tempMissing=sum(is.na(extantDataset))/length(tempIndex)
    
    #check to see how much loss needs to occur
    charsToLoseCorrection=charactersMostMissing-tempMissing
    
    
    options(warn=-1)
    simulateCharacterLoss(
      extinctDataset=extinctDataset,
      extantDataset=extantDataset,
      numSims=numSims,
      numCharsToLose=charsToLoseCorrection,
      linkage=linkage,
      softChars=softChars,
      sigma=sigma,
      normDist=normDist,
      poisDist=poisDist,
      withinPercent=withinPercent,
      characterAdjustment=characterAdjustment,
      cap=cap,
      protectedTaxa=extantProtect) -> tempSimulatedCharLossArray
    tempSimulatedCharLossArray ->> MYTEST
    
    
    options(warn=0)
    #along=2 sets binding to have same number of rows constant, acts as multidimensional cbind 
    #loop to fill in information into simulatedCharLossArray
    for(i in 1:length(groupBins[[group]])){
      if(any(simulatedCharLossArray[,groupBins[[group]][i],]<0, na.rm=T)){ #previously untouched column
        tempSimulatedCharLossArray[,i,] ->> simulatedCharLossArray[,groupBins[[group]][i],]
      } else { #taxa is protected from being changed
        cat("Taxon",groupBins[[group]][i],"is protected. \n",sep=" ")
      }
    }
  }
  
  #apply character loss to extant matrix
  simulatedCharLossArray<<-simulatedCharLossArray
  applyToExtinct(simulatedCharLossArray)->simulatedLossArray
  
  
  #need list, flip chars and taxa
  simulatedLossList<-list()
  for(i in 1:dim(simulatedLossArray)[3]){
    simulatedLossList[[i]]<-t(simulatedLossArray[,,i])
  }
  
  ###Distance matrix and PCO matrix defaults contained here
  ###run MatrixList function with output
  MATRIXList(data=simulatedLossList,axes=axes, method="manhattan")->preparedList
  
  ###Disparity analysis defaults contained here
  ###Run a missing data corrected disparity analysis on the time bin data
  for(i in 1:length(preparedList)){
    Disaprity.Results<-DisparityCalc (preparedList[[i]], iterations=1000,axes=axes,C.I.L=0.025,C.I.U=0.975)
    
    
    #SoR = Sum of Ranges metric    #SoV = Sum of Variances metric
    #PoR = Product of Ranges metric    #PoV = Product of Variances metric
    #U.C.I = Upper Confidence Interval #L.C.I Lower Confidence Interval
    colnames(Disaprity.Results)<-c("Taxa_n", "Mean_SoR", "L.C.I_SoR", "U.C.I_SoR", "Mean_SoV", "L.C.I_SoV", "U.C.I_SoV",        "Mean_PoR", "L.C.I_PoR", "U.C.I_PoR", "Mean_PoV", "L.C.I_PoV", "U.C.I_PoV")
    
    #Writes the output file to your current working directory
    write.csv(Disaprity.Results, paste("Disaprity.Results.Group", i, ".csv", sep = ""), row.names=F)
  }
  #Done!
}

#Remaining scripts are the actual functions that calculate disparity and remove data from your taxon-by-character matrix.

######################################
##Distance Matrix and PCO Calculator##
######################################

MATRIXList <- function(data=data, axes=axes, method=method, GroupBins= groupBins, GroupsVector=groupsVector) {
  Final.PCO.Output<-NULL
  Final.PCO.Output<-as.list(Final.PCO.Output)
  
  Final.PCO.Group<-NULL
  Final.PCO.Group<-as.list(Final.PCO.Group)
  
  PCO.Output.List<-NULL
  PCO.Output.List <-as.list(PCO.Output.List)
  
  for(i in 1:length(data)){
    #Calculating the distance matrix, usually euclidean or manhattan distance
    EuMatx<-vegdist(data[[i]], method=method, na.rm=TRUE)
    EuMatx[is.na(EuMatx)] <- 0
    FMatx <- as.matrix(EuMatx)
    
    #Using the distance matrix to calculate the PCO matrix 
    PCO.Output<-cmdscale(FMatx, k = axes, eig = FALSE, add = FALSE, x.ret = FALSE)
    
Test<<-    PCO.Output
    PCO.Output.List[i]<-list(PCO.Output)
    
    
  }
  
  #Setting up an empty vector to contain the mean PCO matrix
  PCOout<-matrix(data=NA, nrow = nrow(PCO.Output.List[[1]]), ncol = ncol(PCO.Output.List[[1]])) 
  PCOout1<-vector("numeric", length=length(PCO.Output.List))
  
  #Calculating a mean PCO matrix
  for (j in 1:nrow(PCO.Output.List[[1]])){
    for (k in 1:ncol(PCO.Output.List[[1]])){
      for (i in 1:length(PCO.Output.List)){
        PCOout1[i]<-(PCO.Output.List[[i]][j,k])
        PCOout[j,k]<-mean(PCOout1)
      }
    }
  }
  
  #Saving the mean PCO matrix
  write.csv(PCOout, "MeanPCOMatrix.csv")
  
  
  ##Setting up the PCO matrix for the disparity script
  PCO.Matrix<-as.data.frame(PCOout)
  
  #Adding the time bins back in
  PCO.Matrix$Bins<-GroupsVector
  
  PCO.Matrix$Bins<-factor(PCO.Matrix$Bins)
  
  Bins <- length(GroupBins)
  
  ##This section separates out into bins, then deletes those bin assignments once finished
  RunIndex<-list()
  
  for(i in 1:Bins){
    RunIndex[[i]]<-PCO.Matrix[GroupBins[[i]],]
    RunIndex[[i]]<-RunIndex[[i]][,-ncol(RunIndex[[i]])]
  }
  
  RunIndex
  
}


###############################################
##Start of the Disparity Calculation Function##
###############################################
DisparityCalc <- function(data=data, axes=axes, iterations=1000, C.I.L=0.025, C.I.U=0.975){
  
  #Bootstrapping the PCO matrix
  #Rarefaction also occurs at this step.
  result<-list()
  NSresult<-list()
  num.sp<-dim(data)[1]
  
  for(n in 1:num.sp){
    for(i in 1:iterations){
      
      output<-as.matrix(data[sample(1:nrow(data),n,T),])
      result[i] <- list(output)
    }
    NSresult[n]<-list(result)
  }
  
  #This removes the first list from memory. It would only contain a single taxon, so all disparity distance measures would be 0. Removing it is required to avoid errors.
  NSresult[[1]]=NULL
  
  #Sets up empty vectors to put data inside.
  try1<-NULL
  try1<-vector("list", length(NSresult))
  try2<-NULL
  try2<-vector("list", length(NSresult))
  
  #Produce empty space for future data. 
  for(k in 1:length(NSresult)){ 
    try<-matrix(data=NA, nrow = length(NSresult[[1]]), ncol = ncol(data))
    try1[[k]]<-try
    
    try2[[k]]<-try
    
  } 
  
  
  #This section calculates the range and variance. Then places it into the empty vectors you constructed in the previous step.
  #Range Calculations.
  for(v in 1:length(NSresult)){
    for(a in 1:length(result)){
      for(j in 1:ncol(data)){
        try1[[v]][a,j] <-((max(NSresult[[v]][[a]][,j]))-(min(NSresult[[v]][[a]][,j])))
        
      }
    }
  }
  
  #Variance Calculations.
  for(vv in 1:length(NSresult)){
    for(aa in 1:length(result)){
      for(jj in 1:ncol(data)){
        try2[[vv]][aa,jj] <-var(NSresult[[vv]][[aa]][,jj])
        
      }
    }
  }
  
  
  
  #Set-up for the NthRoot function in order to scale your products correctly.
  NthRoot<-function(x=data, n=axes){
    x^(1/n)
  }
  
  ###Start of the Products###
  ####Product of Variances####
  PoV.prod<-NULL
  PoV.prod<-vector("list", length(NSresult))
  
  #Scale by Nth root
  for(ss in 1:length(NSresult)){
    PoV.prod[[ss]]<-NthRoot(apply(try2[[ss]],1,prod))
  }
  
  PoV.quant <- NULL
  PoV.quant <-vector("list", length(NSresult))
  
  #Setting up for 95% confidence intervals
  for(tt in 1:length(NSresult)){
    PoV.quant[[tt]]<-quantile(PoV.prod[[tt]], probs=c(C.I.L,C.I.U))
  }
  
  P.Variance.Mean <- NULL
  P.Variance.Mean <-vector("list", length(NSresult))
  
  #Setting up for means
  for(vv in 1:length(NSresult)){
    P.Variance.Mean[[vv]]<-mean(PoV.prod[[vv]])
  }
  
  
  
  P.FinalResultV<-list(P.Variance.Mean, PoV.quant)
  
  
  
  P.Quant.List.V<-NULL  
  
  P.Quant.Trans.V<-matrix(data=NA, nrow = 1, ncol = length(P.FinalResultV[[2]][[1]]))
  P.Quant.List.V<-vector("list", length(NSresult))
  
  for(q in 1:length(P.FinalResultV[[2]])){
    for(n in 1:length(P.FinalResultV[[2]][[1]])){       
      trans.PV<-matrix(P.FinalResultV[[2]][[q]][[n]])
      P.Quant.Trans.V[,n]<-trans.PV
      
    }
    
    P.Quant.List.V[[q]]<-P.Quant.Trans.V
    
  }
  
  
  P.Mean.Vecs.V<-NULL
  P.Mean.Vecs.V<-matrix(data=NA, nrow = length(P.FinalResultV[[1]]), ncol = 1)
  
  for(aa in 1:length(P.FinalResultV[[1]])){
    P.Mean.Vecs.V[aa,]<-as.vector(P.FinalResultV[[1]][aa], mode="numeric")
  }
  
  
  P.Quant.Vec.V<-NULL
  P.Quant.Vec.V<-matrix(data=NA, nrow = length(P.Quant.List.V), ncol = ncol(P.Quant.List.V[[1]]))
  
  
  for(ee in 1:length(P.Quant.List.V)){
    for(cc in 1:ncol(P.Quant.List.V[[1]])){         
      P.Quant.Vec.V[ee,cc]<-as.vector(P.Quant.List.V[[ee]][,cc])
      
    }
  }
  
  
  Product.of.Variances<-cbind(P.Mean.Vecs.V,P.Quant.Vec.V)
  
  
  ####Product of Ranges####
  PoR.prod<-NULL
  PoR.prod<-vector("list", length(NSresult))
  
  for(ss in 1:length(NSresult)){
    PoR.prod[[ss]]<-NthRoot(apply(try1[[ss]],1,prod))
  }
  
  PoR.quant <- NULL
  PoR.quant <-vector("list", length(NSresult))
  
  for(tt in 1:length(NSresult)){
    PoR.quant[[tt]]<-quantile(PoR.prod[[tt]], probs=c(C.I.L,C.I.U))
  }
  
  P.Range.Mean <- NULL
  P.Range.Mean <-vector("list", length(NSresult))
  
  for(vv in 1:length(NSresult)){
    P.Range.Mean[[vv]]<-mean(PoR.prod[[vv]])
  }
  
  
  
  P.FinalResult<-list(P.Range.Mean, PoR.quant)
  
  
  
  P.Quant.List<-NULL    
  
  P.Quant.Trans<-matrix(data=NA, nrow = 1, ncol = length(P.FinalResult[[2]][[1]]))
  P.Quant.List<-vector("list", length(NSresult))
  
  for(q in 1:length(P.FinalResult[[2]])){
    for(n in 1:length(P.FinalResult[[2]][[1]])){        
      trans.PR<-matrix(P.FinalResult[[2]][[q]][[n]])
      P.Quant.Trans[,n]<-trans.PR
      
    }
    
    P.Quant.List[[q]]<-P.Quant.Trans
    
  }
  
  
  P.Mean.Vecs<-NULL
  P.Mean.Vecs<-matrix(data=NA, nrow = length(P.FinalResult[[1]]), ncol = 1)
  
  for(aa in 1:length(P.FinalResult[[1]])){
    
    P.Mean.Vecs[aa,]<-as.vector(P.FinalResult[[1]][aa], mode="numeric")
    
    
  }
  
  
  P.Quant.Vec<-NULL
  P.Quant.Vec<-matrix(data=NA, nrow = length(P.Quant.List), ncol = ncol(P.Quant.List[[1]]))
  
  
  for(ee in 1:length(P.Quant.List)){
    for(cc in 1:ncol(P.Quant.List[[1]])){           
      P.Quant.Vec[ee,cc]<-as.vector(P.Quant.List[[ee]][,cc])
    }
  }
  
  
  Product.of.Ranges<-cbind(P.Mean.Vecs,P.Quant.Vec)
  
  
  ###Start of the Sums###
  ####Sum of Variances####
  SoV.sum<-NULL
  SoV.sum<-vector("list", length(NSresult))
  
  for(ss in 1:length(NSresult)){
    SoV.sum[[ss]]<-apply(try2[[ss]],1,sum)
  }
  
  
  
  SoV.quant <- NULL
  SoV.quant <-vector("list", length(NSresult))
  
  for(tt in 1:length(NSresult)){
    SoV.quant[[tt]]<-quantile(SoV.sum[[tt]], probs=c(C.I.L,C.I.U))
  }
  
  Range.Mean.V <- NULL
  Range.Mean.V <-vector("list", length(NSresult))
  
  for(vv in 1:length(NSresult)){
    Range.Mean.V[[vv]]<-mean(SoV.sum[[vv]])
  }
  
  
  
  FinalResult.V<-list(Range.Mean.V, SoV.quant)
  
  
  
  Quant.List.V<-NULL    
  
  Quant.Trans.V<-matrix(data=NA, nrow = 1, ncol = length(FinalResult.V[[2]][[1]]))
  Quant.List.V<-vector("list", length(NSresult))
  
  for(q in 1:length(FinalResult.V[[2]])){
    for(n in 1:length(FinalResult.V[[2]][[1]])){        
      trans.SV<-matrix(FinalResult.V[[2]][[q]][[n]])
      Quant.Trans.V[,n]<-trans.SV
      
    }
    
    Quant.List.V[[q]]<-Quant.Trans.V
    
  }
  
  
  Mean.Vecs.V<-NULL
  Mean.Vecs.V<-matrix(data=NA, nrow = length(FinalResult.V[[1]]), ncol = 1)
  
  for(aa in 1:length(FinalResult.V[[1]])){
    
    Mean.Vecs.V[aa,]<-as.vector(FinalResult.V[[1]][aa], mode="numeric")
    
    
  }
  
  
  Quant.Vec.V<-NULL
  Quant.Vec.V<-matrix(data=NA, nrow = length(Quant.List.V), ncol = ncol(Quant.List.V[[1]]))
  
  
  for(ee in 1:length(Quant.List.V)){
    for(cc in 1:ncol(Quant.List.V[[1]])){           
      Quant.Vec.V[ee,cc]<-as.vector(Quant.List.V[[ee]][,cc])
      
    }
    
  }
  
  
  Sum.of.Variances<-cbind(Mean.Vecs.V,Quant.Vec.V)
  
  
  ####Sum of Ranges####
  SoR.sum<-NULL
  SoR.sum<-vector("list", length(NSresult))
  
  for(ss in 1:length(NSresult)){
    SoR.sum[[ss]]<-apply(try1[[ss]],1,sum)
  }
  
  
  
  SoR.quant <- NULL
  SoR.quant <-vector("list", length(NSresult))
  
  for(tt in 1:length(NSresult)){
    SoR.quant[[tt]]<-quantile(SoR.sum[[tt]], probs=c(C.I.L,C.I.U))
  }
  
  Range.Mean <- NULL
  Range.Mean <-vector("list", length(NSresult))
  
  for(vv in 1:length(NSresult)){
    Range.Mean[[vv]]<-mean(SoR.sum[[vv]])
  }
  
  
  
  FinalResult<-list(Range.Mean, SoR.quant)
  
  
  
  Quant.List<-NULL  
  
  Quant.Trans<-matrix(data=NA, nrow = 1, ncol = length(FinalResult[[2]][[1]]))
  Quant.List<-vector("list", length(NSresult))
  
  for(q in 1:length(FinalResult[[2]])){
    for(n in 1:length(FinalResult[[2]][[1]])){      
      trans.SR<-matrix(FinalResult[[2]][[q]][[n]])
      Quant.Trans[,n]<-trans.SR
      
    }
    
    Quant.List[[q]]<-Quant.Trans
    
  }
  
  
  Mean.Vecs<-NULL
  Mean.Vecs<-matrix(data=NA, nrow = length(FinalResult[[1]]), ncol = 1)
  
  for(aa in 1:length(FinalResult[[1]])){
    
    Mean.Vecs[aa,]<-as.vector(FinalResult[[1]][aa], mode="numeric")
    
    
  }
  
  
  Quant.Vec<-NULL
  Quant.Vec<-matrix(data=NA, nrow = length(Quant.List), ncol = ncol(Quant.List[[1]]))
  
  
  for(ee in 1:length(Quant.List)){
    for(cc in 1:ncol(Quant.List[[1]])){         
      Quant.Vec[ee,cc]<-as.vector(Quant.List[[ee]][,cc])
    }
  }
  
  
  Sum.of.Ranges<-cbind(Mean.Vecs,Quant.Vec)
  
  #End of all disparity calculations.
  #Remaining section combines them together for saving.
  
  ##Putting it all together##
  n<-as.matrix(seq(2, dim(data)[1], 1))
  
  Final.Output<-cbind(n, Sum.of.Ranges, Sum.of.Variances, Product.of.Ranges, Product.of.Variances)
  
  Disparity.Output<-as.data.frame(Final.Output)
  
  
}### This is the end of the disparity function.




DisparityNoCorr<-function(Data=Data, axes=axes, method='manhattan', iterations=1000, C.I.L=0.025, C.I.U=0.975){
    
groupsVector<<-as.character(as.matrix(Data[2,]))

#create vector for every "group" indicating which taxa belong to each group
groupBins <- list()
allLevelsList <- strsplit(groupsVector,split="\\|")
allLevels <- unlist(allLevelsList)
allLevels <- allLevels[allLevels!="\\|"]
groupLevels <- unique(allLevels)

for(currentLevel in groupLevels){
  tempBin <- NULL
  for(taxon in 1:length(allLevelsList)){
    if(any(allLevelsList[[taxon]]==currentLevel))
      tempBin <- c(tempBin, taxon)
  }
  groupBins[[currentLevel]] <- tempBin
}
groupBins<<-groupBins
importExtinct=Data[-(1:3),]

for(i in 1:ncol(importExtinct)){
  importExtinct[,i]=as.numeric(as.character(importExtinct[,i]))
}
extinctDataset <- importExtinct

PCO.Matrix<-calculate.pco(extinctDataset, axes, method=method)
write.csv(PCO.Matrix, "PCOMatrix.csv")

  ##Setting up the PCO matrix for the disparity script
  PCO.Matrix<-as.data.frame(PCO.Matrix)
  
  #Adding the time bins back in
  PCO.Matrix$Bins<-groupsVector
  
  PCO.Matrix$Bins<-factor(PCO.Matrix$Bins)

  Bins<-length(levels(PCO.Matrix$Bins))
  
  ##This section separates out into bins, then deletes those bin assignments once finished
  RunIndex<-list()
  
  for(i in 1:Bins){
    RunIndex[[i]]<-PCO.Matrix[groupBins[[i]],]
    RunIndex[[i]]<-RunIndex[[i]][,-ncol(RunIndex[[i]])]
  }

  


####Running the Analysis####    
for(i in 1:length(RunIndex)){
    Disparity.Results<-DisparityCalc(RunIndex[[i]], axes=axes, iterations=iterations, C.I.L=C.I.L, C.I.U=C.I.U)

    colnames(Disparity.Results)<-c("Taxa_n", "Mean_SoR", "L.C.I_SoR", "U.C.I_SoR", "Mean_SoV", "L.C.I_SoV", "U.C.I_SoV",        "Mean_PoR", "L.C.I_PoR", "U.C.I_PoR", "Mean_PoV", "L.C.I_PoV", "U.C.I_PoV")

    write.csv(Disparity.Results, paste("Disparity.Results.Group", i, ".csv", sep = ""), row.names=F)
}



###############################################
##Start of the Disparity Calculation Function##
###############################################
DisparityCalc <- function(data=data, axes=axes, iterations=iterations, C.I.L=C.I.L, C.I.U=C.I.U){

#Bootstrapping the PCO matrix
#Rarefaction also occurs at this step.
result<-NULL
result<-as.list(result)
NSresult<-NULL
NSresult<-as.list(NSresult)
num.sp<-dim(data)[1]

for(n in 1:num.sp){
    for(i in 1:iterations){
        
            output<-as.matrix(data[sample(1:nrow(data),n,T),])
                    result[i] <- list(output)
            }
        NSresult[n]<-list(result)
    }

#This removes the first list from memory. It would only contain a single taxon, so all disparity distance measures would be 0. Removing it is required to avoid errors.
NSresult[[1]]=NULL

#Sets up empty vectors to put data inside.
try1<-NULL
try1<-vector("list", length(NSresult))
try2<-NULL
try2<-vector("list", length(NSresult))


for(k in 1:length(NSresult)){   
        try<-matrix(data=NA, nrow = length(NSresult[[1]]), ncol = ncol(data))
            try1[[k]]<-try
            
            try2[[k]]<-try
            
        }   


#This section calculates the range and variance. Then places it into the empty vectors you constructed in the previous step.
#Range Calculations.
for(v in 1:length(NSresult)){
        for(a in 1:length(result)){
            for(j in 1:ncol(data)){
                    try1[[v]][a,j] <-((max(NSresult[[v]][[a]][,j]))-(min(NSresult[[v]][[a]][,j])))
    
        }
    }
}

#Variance Calculations.
for(vv in 1:length(NSresult)){
        for(aa in 1:length(result)){
            for(jj in 1:ncol(data)){
                    try2[[vv]][aa,jj] <-var(NSresult[[vv]][[aa]][,jj])
    
        }
    }
}



#Set-up for the NthRoot function in order to scale your products correctly.
NthRoot<-function(x=data, n=axes){
    x^(1/n)
}

####Start of the Products####
###Product of Variances
PoV.prod<-NULL
PoV.prod<-vector("list", length(NSresult))

for(ss in 1:length(NSresult)){
PoV.prod[[ss]]<-NthRoot(apply(try2[[ss]],1,prod))
    }

PoV.quant <- NULL
PoV.quant <-vector("list", length(NSresult))

for(tt in 1:length(NSresult)){
PoV.quant[[tt]]<-quantile(PoV.prod[[tt]], probs=c(C.I.L,C.I.U))
    }

P.Variance.Mean <- NULL
P.Variance.Mean <-vector("list", length(NSresult))

for(vv in 1:length(NSresult)){
P.Variance.Mean[[vv]]<-mean(PoV.prod[[vv]])
    }



P.FinalResultV<-list(P.Variance.Mean, PoV.quant)

    
    
P.Quant.List.V<-NULL    
        
P.Quant.Trans.V<-matrix(data=NA, nrow = 1, ncol = length(P.FinalResultV[[2]][[1]]))
P.Quant.List.V<-vector("list", length(NSresult))
    
for(q in 1:length(P.FinalResultV[[2]])){
    for(n in 1:length(P.FinalResultV[[2]][[1]])){       
        trans.PV<-matrix(P.FinalResultV[[2]][[q]][[n]])
            P.Quant.Trans.V[,n]<-trans.PV
        
        }
    
        P.Quant.List.V[[q]]<-P.Quant.Trans.V
    
    }


P.Mean.Vecs.V<-NULL
P.Mean.Vecs.V<-matrix(data=NA, nrow = length(P.FinalResultV[[1]]), ncol = 1)

    for(aa in 1:length(P.FinalResultV[[1]])){
                    
            P.Mean.Vecs.V[aa,]<-as.vector(P.FinalResultV[[1]][aa], mode="numeric")
            
        
    }


P.Quant.Vec.V<-NULL
P.Quant.Vec.V<-matrix(data=NA, nrow = length(P.Quant.List.V), ncol = ncol(P.Quant.List.V[[1]]))


for(ee in 1:length(P.Quant.List.V)){
    for(cc in 1:ncol(P.Quant.List.V[[1]])){         
            P.Quant.Vec.V[ee,cc]<-as.vector(P.Quant.List.V[[ee]][,cc])
            
        }
    
    }


Product.of.Variances<-cbind(P.Mean.Vecs.V,P.Quant.Vec.V)
    
    
###Product of Ranges
PoR.prod<-NULL
PoR.prod<-vector("list", length(NSresult))

for(ss in 1:length(NSresult)){
PoR.prod[[ss]]<-NthRoot(apply(try1[[ss]],1,prod))
    }



PoR.quant <- NULL
PoR.quant <-vector("list", length(NSresult))

for(tt in 1:length(NSresult)){
PoR.quant[[tt]]<-quantile(PoR.prod[[tt]], probs=c(C.I.L,C.I.U))
    }

P.Range.Mean <- NULL
P.Range.Mean <-vector("list", length(NSresult))

for(vv in 1:length(NSresult)){
P.Range.Mean[[vv]]<-mean(PoR.prod[[vv]])
    }



P.FinalResult<-list(P.Range.Mean, PoR.quant)

    
    
P.Quant.List<-NULL  
        
P.Quant.Trans<-matrix(data=NA, nrow = 1, ncol = length(P.FinalResult[[2]][[1]]))
P.Quant.List<-vector("list", length(NSresult))
    
for(q in 1:length(P.FinalResult[[2]])){
    for(n in 1:length(P.FinalResult[[2]][[1]])){        
        trans.PR<-matrix(P.FinalResult[[2]][[q]][[n]])
            P.Quant.Trans[,n]<-trans.PR
        
        }
    
        P.Quant.List[[q]]<-P.Quant.Trans
    
    }


P.Mean.Vecs<-NULL
P.Mean.Vecs<-matrix(data=NA, nrow = length(P.FinalResult[[1]]), ncol = 1)

    for(aa in 1:length(P.FinalResult[[1]])){
                    
            P.Mean.Vecs[aa,]<-as.vector(P.FinalResult[[1]][aa], mode="numeric")
            
        
    }


P.Quant.Vec<-NULL
P.Quant.Vec<-matrix(data=NA, nrow = length(P.Quant.List), ncol = ncol(P.Quant.List[[1]]))


for(ee in 1:length(P.Quant.List)){
    for(cc in 1:ncol(P.Quant.List[[1]])){           
            P.Quant.Vec[ee,cc]<-as.vector(P.Quant.List[[ee]][,cc])
        }
    }


Product.of.Ranges<-cbind(P.Mean.Vecs,P.Quant.Vec)


###Start of the Sums###
#Sum of Variances
SoV.sum<-NULL
SoV.sum<-vector("list", length(NSresult))

for(ss in 1:length(NSresult)){
SoV.sum[[ss]]<-apply(try2[[ss]],1,sum)
    }



SoV.quant <- NULL
SoV.quant <-vector("list", length(NSresult))

for(tt in 1:length(NSresult)){
SoV.quant[[tt]]<-quantile(SoV.sum[[tt]], probs=c(C.I.L,C.I.U))
    }

Range.Mean.V <- NULL
Range.Mean.V <-vector("list", length(NSresult))

for(vv in 1:length(NSresult)){
Range.Mean.V[[vv]]<-mean(SoV.sum[[vv]])
    }



FinalResult.V<-list(Range.Mean.V, SoV.quant)

    
    
Quant.List.V<-NULL  
        
Quant.Trans.V<-matrix(data=NA, nrow = 1, ncol = length(FinalResult.V[[2]][[1]]))
Quant.List.V<-vector("list", length(NSresult))
    
for(q in 1:length(FinalResult.V[[2]])){
    for(n in 1:length(FinalResult.V[[2]][[1]])){        
        trans.SV<-matrix(FinalResult.V[[2]][[q]][[n]])
            Quant.Trans.V[,n]<-trans.SV
        
        }
    
        Quant.List.V[[q]]<-Quant.Trans.V
    
    }


Mean.Vecs.V<-NULL
Mean.Vecs.V<-matrix(data=NA, nrow = length(FinalResult.V[[1]]), ncol = 1)

    for(aa in 1:length(FinalResult.V[[1]])){
                    
            Mean.Vecs.V[aa,]<-as.vector(FinalResult.V[[1]][aa], mode="numeric")
            
        
    }


Quant.Vec.V<-NULL
Quant.Vec.V<-matrix(data=NA, nrow = length(Quant.List.V), ncol = ncol(Quant.List.V[[1]]))


for(ee in 1:length(Quant.List.V)){
    for(cc in 1:ncol(Quant.List.V[[1]])){           
            Quant.Vec.V[ee,cc]<-as.vector(Quant.List.V[[ee]][,cc])
            
        }
    
    }


Sum.of.Variances<-cbind(Mean.Vecs.V,Quant.Vec.V)


###Sum of Ranges
SoR.sum<-NULL
SoR.sum<-vector("list", length(NSresult))

for(ss in 1:length(NSresult)){
SoR.sum[[ss]]<-apply(try1[[ss]],1,sum)
    }



SoR.quant <- NULL
SoR.quant <-vector("list", length(NSresult))

for(tt in 1:length(NSresult)){
SoR.quant[[tt]]<-quantile(SoR.sum[[tt]], probs=c(C.I.L,C.I.U))
    }

Range.Mean <- NULL
Range.Mean <-vector("list", length(NSresult))

for(vv in 1:length(NSresult)){
Range.Mean[[vv]]<-mean(SoR.sum[[vv]])
    }



FinalResult<-list(Range.Mean, SoR.quant)

    
    
Quant.List<-NULL    
        
Quant.Trans<-matrix(data=NA, nrow = 1, ncol = length(FinalResult[[2]][[1]]))
Quant.List<-vector("list", length(NSresult))
    
for(q in 1:length(FinalResult[[2]])){
    for(n in 1:length(FinalResult[[2]][[1]])){      
        trans.SR<-matrix(FinalResult[[2]][[q]][[n]])
            Quant.Trans[,n]<-trans.SR
        
        }
    
        Quant.List[[q]]<-Quant.Trans
    
    }


Mean.Vecs<-NULL
Mean.Vecs<-matrix(data=NA, nrow = length(FinalResult[[1]]), ncol = 1)

    for(aa in 1:length(FinalResult[[1]])){
                    
            Mean.Vecs[aa,]<-as.vector(FinalResult[[1]][aa], mode="numeric")
            
        
    }


Quant.Vec<-NULL
Quant.Vec<-matrix(data=NA, nrow = length(Quant.List), ncol = ncol(Quant.List[[1]]))


for(ee in 1:length(Quant.List)){
    for(cc in 1:ncol(Quant.List[[1]])){         
            Quant.Vec[ee,cc]<-as.vector(Quant.List[[ee]][,cc])
        }
    }


Sum.of.Ranges<-cbind(Mean.Vecs,Quant.Vec)

##Putting it all together##
n<-as.matrix(seq(2, dim(data)[1], 1))

Final.Output<-cbind(n, Sum.of.Ranges, Sum.of.Variances, Product.of.Ranges, Product.of.Variances)
    
Disparity.Output<-as.data.frame(Final.Output)


    }
}


calculate.pco <- function(input, axes, method) {

input<-t(input)

 distance.matrix <- vegdist(input, method=method, na.rm=TRUE)

  distance.matrix[is.na(distance.matrix)] <- 0
  
  distance.matrix.no.na <- as.matrix(distance.matrix)


 pco.output <- round(cmdscale(distance.matrix.no.na, k = axes, eig = FALSE, add = FALSE, x.ret = FALSE), 5)

}

###########################
#Disparity T-test Function#
###########################
    #Script adapted from Anderson and Friedman 2012.
DisparityTTest<-function(data, replicates=1000, upper.bound=0.975, lower.bound=0.025, method=c("Sum of Ranges","Sum of Variances","Product of Ranges","Product of Variances")){

##Sorting out bins
groupsVector<<-as.character(as.matrix(data[ ,2]))

#create vector for every "group" indicating which taxa belong to each group
groupBins <- list()
allLevelsList <- strsplit(groupsVector,split="\\|")
allLevels <- unlist(allLevelsList)
allLevels <- allLevels[allLevels!="\\|"]
groupLevels <- unique(allLevels)

for(currentLevel in groupLevels){
  tempBin <- NULL
  for(taxon in 1:length(allLevelsList)){
    if(any(allLevelsList[[taxon]]==currentLevel))
      tempBin <- c(tempBin, taxon)
  }
  groupBins[[currentLevel]] <- tempBin
}
groupBins<<-groupBins


bins <- groupLevels

number.bins <- length (groupLevels)

standard.error <- array (dim = number.bins)

sample.size <- array (dim = number.bins)

#specify arrays to hold dates and disparity values

complete.variance <- array (dim = c(replicates, number.bins))

mean.variance <- array (dim = number.bins)

t.statistics <- array (dim = c(number.bins, number.bins))

degrees.freedom <- array (dim = c (number.bins, number.bins))

significance.values <- array (dim = c(number.bins, number.bins))

number.axes <- ncol (data) - 2 #calculates the number of axes to be analyzed

CI <- array (dim = c(number.bins, 2))

#Set up for the product disparity metrics to scale by the nth root.
NthRoot<-function(x=data, n=number.axes){
    x^(1/n)
}

Methods<-sort(method)

IList<-list()

####Sum of Ranges Statistics####
if (any(Methods == "Sum of Ranges")){

    for (i in 1: replicates){

        for (k in 1: number.bins){

    bin.data <- data[groupBins[[k]],]

    

    dimensions <- dim (bin.data)

    height <- dimensions [1]

    bin.data <- bin.data [sample (1:height, replace = TRUE), ]
    
    sample.size [k] <- height

    variance.holder <- 0
    
    for (m in 1:number.axes){

        variance.holder <- variance.holder + ((max(bin.data[, (m+2)]))-(min(bin.data[, (m+2)])))
    
        }
    
    complete.variance [i, k] <- variance.holder

    }

}


for (j in 1:number.bins){
    
    standard.error [j] <- sd (complete.variance [, j])
    
    mean.variance [j] <- mean (complete.variance [,j])

    CI [j, 1] <- quantile (complete.variance [, j], probs = lower.bound)

    CI [j, 2] <- quantile (complete.variance [, j], probs = upper.bound)

}


for (x in 1: number.bins){ # go down rows

    for (q in 1:number.bins){ #goes across columns

    denominator <- (mean.variance[x]-mean.variance[q])

    numerator.one <- ((sample.size[x]-1)*(sample.size[x])*((standard.error[x])^2) + (sample.size[q]-1)*(sample.size[q])*((standard.error[q])^2))/(sample.size[x]+sample.size[q]+2) 

    numerator.two <- (sample.size[x] + sample.size [q])/(sample.size[x] * sample.size [q]) 
    
    t.statistics [x, q] <- denominator/((numerator.one*numerator.two)^.5)

    degrees.freedom [x, q] <- sample.size[x] + sample.size [q] - 2

    significance.values [x, q] <- 1-pt(t.statistics [x, q], df = degrees.freedom [x, q])

    
    #make test two-tailed

    if (significance.values [x,q] > 0.5)

    {
        
        significance.values [x,q] <- 2*(1-significance.values[x,q])
    
    }

    else if (significance.values [x,q] < 0.5)

    {
    
        significance.values [x,q] <- 2*(significance.values[x,q])   

    }


    else if (significance.values [x,q] == 0.5)

    {

        significance.values [x,q] <- 1  

    }   

    }

}


 IList[[Methods[1]]] <- significance.values

}

####Sum of Variances Statistics####
if (any(Methods == "Sum of Variances")){

    for (i in 1: replicates){

        for (k in 1: number.bins){

        bin.data <- data[groupBins[[k]],]

    

    dimensions <- dim (bin.data)

    height <- dimensions [1]

    bin.data <- bin.data [sample (1:height, replace = TRUE), ]
    
    sample.size [k] <- height

    variance.holder <- 0
    
    for (m in 1:number.axes)

    {   

    
    variance.holder <- variance.holder + var (bin.data[, (m+2)])
    
    }

    
    complete.variance [i, k] <- variance.holder

}

}


for (j in 1:number.bins){
    
    standard.error [j] <- sd (complete.variance [, j])
    
    mean.variance [j] <- mean (complete.variance [,j])

    CI [j, 1] <- quantile (complete.variance [, j], probs = lower.bound)

    CI [j, 2] <- quantile (complete.variance [, j], probs = upper.bound)


}


for (x in 1: number.bins){ # go down rows

    for (q in 1:number.bins){ #goes across columns

    denominator <- (mean.variance[x]-mean.variance[q])

    numerator.one <- ((sample.size[x]-1)*(sample.size[x])*((standard.error[x])^2) + (sample.size[q]-1)*(sample.size[q])*((standard.error[q])^2))/(sample.size[x]+sample.size[q]+2) 

    numerator.two <- (sample.size[x] + sample.size [q])/(sample.size[x] * sample.size [q]) 
    
    t.statistics [x, q] <- denominator/((numerator.one*numerator.two)^.5)

    degrees.freedom [x, q] <- sample.size[x] + sample.size [q] - 2

    significance.values [x, q] <- 1-pt(t.statistics [x, q], df = degrees.freedom [x, q])

    
    #make test two-tailed

    if (significance.values [x,q] > 0.5)

    {
        
        significance.values [x,q] <- 2*(1-significance.values[x,q])
    
    }

    else if (significance.values [x,q] < 0.5)

    {
    
        significance.values [x,q] <- 2*(significance.values[x,q])   

    }


    else if (significance.values [x,q] == 0.5)

        {

        significance.values [x,q] <- 1  

        }   


        }

    }


IList[[Methods[2]]] <- significance.values


}

####Product of Ranges Statistics####
if (any(Methods == "Product of Ranges")){

    for (i in 1: replicates){

        for (k in 1: number.bins){

        bin.data <- data[groupBins[[k]],]

    

    dimensions <- dim (bin.data)

    height <- dimensions [1]

    bin.data <- bin.data [sample (1:height, replace = TRUE), ]
    
    sample.size [k] <- height

    variance.holder <- 1
    
    for (m in 1:number.axes)

    {   

    
    variance.holder <- variance.holder * ((max(bin.data[, (m+2)]))-(min(bin.data[, (m+2)])))
    
    }
    
    complete.variance [i, k] <- variance.holder

    }

}


complete.variance <-NthRoot(complete.variance)


for (j in 1:number.bins){
    
    standard.error [j] <- sd (complete.variance [, j])
    
    mean.variance [j] <- mean (complete.variance [,j])

    CI [j, 1] <- quantile (complete.variance [, j], probs = lower.bound)

    CI [j, 2] <- quantile (complete.variance [, j], probs = upper.bound)

}


for (x in 1: number.bins){ # go down rows

    for (q in 1:number.bins){ #goes across columns

    denominator <- (mean.variance[x]-mean.variance[q])

    numerator.one <- ((sample.size[x]-1)*(sample.size[x])*((standard.error[x])^2) + (sample.size[q]-1)*(sample.size[q])*((standard.error[q])^2))/(sample.size[x]+sample.size[q]+2) 

    numerator.two <- (sample.size[x] + sample.size [q])/(sample.size[x] * sample.size [q]) 
    
    t.statistics [x, q] <- denominator/((numerator.one*numerator.two)^.5)

    degrees.freedom [x, q] <- sample.size[x] + sample.size [q] - 2

    significance.values [x, q] <- 1-pt(t.statistics [x, q], df = degrees.freedom [x, q])

    
    #make test two-tailed

    if (significance.values [x,q] > 0.5)

    {
        
        significance.values [x,q] <- 2*(1-significance.values[x,q])
    
    }

    else if (significance.values [x,q] < 0.5)

    {
    
        significance.values [x,q] <- 2*(significance.values[x,q])   

    }


    else if (significance.values [x,q] == 0.5)

    {

        significance.values [x,q] <- 1  

    }   


    }

}


IList[[Methods[3]]] <- significance.values

}

####Product of Variances Statistics####
if (any(Methods == "Product of Variances")){
    
    for (i in 1: replicates){

        for (k in 1: number.bins){


            bin.data <- data[groupBins[[k]],]

    

    dimensions <- dim (bin.data)

    height <- dimensions [1]

    bin.data <- bin.data [sample (1:height, replace = TRUE), ]
    
    sample.size [k] <- height

    variance.holder <- 1
    
    for (m in 1:number.axes)

    {   

    
    variance.holder <- variance.holder * var (bin.data[, (m+2)])
    
    }
    
    complete.variance [i, k] <- variance.holder

    }

}

complete.variance <-NthRoot(complete.variance)


for (j in 1:number.bins){
    
    standard.error [j] <- sd (complete.variance [, j])
    
    mean.variance [j] <- mean (complete.variance [,j])

    CI [j, 1] <- quantile (complete.variance [, j], probs = lower.bound)

    CI [j, 2] <- quantile (complete.variance [, j], probs = upper.bound)

}


for (x in 1: number.bins){ # go down rows

    for (q in 1:number.bins){ #goes across columns

    denominator <- (mean.variance[x]-mean.variance[q])

    numerator.one <- ((sample.size[x]-1)*(sample.size[x])*((standard.error[x])^2) + (sample.size[q]-1)*(sample.size[q])*((standard.error[q])^2))/(sample.size[x]+sample.size[q]+2) 

    numerator.two <- (sample.size[x] + sample.size [q])/(sample.size[x] * sample.size [q]) 
    
    t.statistics [x, q] <- denominator/((numerator.one*numerator.two)^.5)

    degrees.freedom [x, q] <- sample.size[x] + sample.size [q] - 2

    significance.values [x, q] <- 1-pt(t.statistics [x, q], df = degrees.freedom [x, q])

    
    #make test two-tailed

    if (significance.values [x,q] > 0.5)

    {
        
        significance.values [x,q] <- 2*(1-significance.values[x,q])
    
    }

    else if (significance.values [x,q] < 0.5)

    {
    
        significance.values [x,q] <- 2*(significance.values[x,q])   

    }


    else if (significance.values [x,q] == 0.5)

    {

        significance.values [x,q] <- 1  

    }   


    }

}


    IList[[Methods[4]]] <- significance.values


    }


IList

 }