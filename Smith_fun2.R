calculate.pco <- function(input, axes, method) {

input<-t(input)

 distance.matrix <- vegdist(input, method=method, na.rm=TRUE)

  distance.matrix[is.na(distance.matrix)] <- 0
  
  distance.matrix.no.na <- as.matrix(distance.matrix)


 pco.output <- round(cmdscale(distance.matrix.no.na, k = axes, eig = FALSE, add = FALSE, x.ret = FALSE), 5)

}


calculate.pco.list <- function(input, axes, method) {

 pco.output.list <- NULL
 pco.output.list <- as.list(pco.output.list)

for(i in 1:length(input)){
    
 distance.matrix <- vegdist(input[[i]], method=method, na.rm=TRUE)

  distance.matrix[is.na(distance.matrix)] <- 0
  
  distance.matrix.no.na <- as.matrix(distance.matrix)


 pco.output <- round(cmdscale(distance.matrix.no.na, k = axes, eig = FALSE, add = FALSE, x.ret = FALSE), 5)
  
  pco.output.list[i] <- list(pco.output)
 
  }
 
  pco.output.list

}

calculate.ordination.difference <- function(pco.data, no.loss.pco, axes, method) {

#Bring in the original PCO matrix with no loss and calculate distance.
 pco.distance.no.loss <- vegdist(no.loss.pco, method=method, na.rm=TRUE)
 pco.distance.no.loss[is.na(pco.distance.no.loss)] <- 0
  
  pco.distance.no.loss.matrix <- as.matrix(pco.distance.no.loss)
 
#Calculate the euclidean distance of each simulated PCO matrix. 
 pco.distance.list <- NULL
 pco.distance.list <- as.list(pco.distance.list)
  
 for (i in 1:length(pco.data)) {
  pco.distance <- vegdist(pco.data[[i]], method=method, na.rm=TRUE)
  
   pco.distance[is.na(pco.distance)] <- 0
  
    pco.distance.list[[i]] <- as.matrix(pco.distance)
 
 }

#Caluclate the difference between the original and simulated taxa distances in PCO morphospace.
 sum.pco.difference <- NULL

 for(i in 1:length(pco.distance.list)) {
   pco.difference <- sqrt((pco.distance.no.loss.matrix - pco.distance.list[[i]]) ^2)

   sum.pco.difference[i] <- sum(pco.difference)

 }
 
  sum.pco.difference

}


plot.pco <- function(pco.data, no.loss.pco, Add=F, xlimit, ylimit, colour="red") {

#Calculating the distance matrix for the PCO ordination with no loss.
 if(Add==F){
  plot(no.loss.pco[ ,1], no.loss.pco[ ,2], xlim=xlimit, ylim=ylimit, pch=20, xlab="PCO Axis 1", ylab="PCO Axis 2")
 }

 if(Add==T){
  for(i in 1:length(pco.data)){
   points(pco.data[[i]][ ,1], pco.data[[i]][ ,2], pch=19, col=colour)
   }
 }

}


simulateCharacterLoss <- function(extinctDataset, extantDataset, numSims=100, charsToLose=1, linkage=F, softChars=13, sigma, normDist=F, poisDist=F) {

extinctDataset->>extinctDataset #import full dataset
  

  createCorrelationMatrix(extinctDataset)->correlationMatrix
  createGlobalLoss(extinctDataset)->GlobalLoss
  
extantDataset->>extantDataset #import full dataset
  
  #accessing by column gives indices of traits to lose for each column
  array(dim=c(charsToLose,ncol(extantDataset),numSims))->charsLostArray
  
   startTime<-proc.time()[3]
  if(normDist==F&poisDist==F)
  {
    for(i in 1:numSims)#i is simulation number
    {
      for(j in 1:ncol(extantDataset)) #j is column in extant matrix
      {
        if(linkage)
        {
            #candidate approval algorithm and manipulation of Global occurence algorithm
    chooseCharsToRemove5(charsToLose,GlobalLoss,correlationMatrix,softChars)->charsLostArray[,j,i]
        } 
        else 
        {
          chooseCharsNoInteraction(charsToLose,GlobalLoss)->charsLostArray[,j,i]
        }
      }
    }
  } 
  else #normDist or poisDist
  {
    charsToRemove=vector(mode="numeric",length=ncol(extantDataset)*numSims)
    k=1 #counter for while loop
    
    if(normDist==T){
      while(k<=length(charsToRemove)){
        tempNum=rnorm(1,mean=charsToLose,sd=sigma)
        if(tempNum>=0&tempNum<=nrow(extantDataset)){
          charsToRemove[k]=tempNum
          k=k+1
        }
      }
      numCharsLost<<-mean(charsToRemove)
    }
      
    if(poisDist==T){
      while(k<=length(charsToRemove)){
        tempNum=rpois(1,lambda=charsToLose)
        
         if(tempNum>=0&tempNum<=(nrow(extantDataset)-5)){
          charsToRemove[k]=tempNum
          k=k+1
        }
      }
      numCharsLost<<-mean(charsToRemove)
  }
    max(round(charsToRemove))->maxCharsLost
    array(dim=c(maxCharsLost,ncol(extantDataset),numSims))->charsLostArray
    
#convert to matrix
    charsToRemove=matrix(round(charsToRemove),ncol=ncol(extantDataset)) #each row is a simulation, each column is a taxon
    
    

    
    totalCharsToBeLost<<-charsToLose*ncol(extantDataset)
    for(i in 1:numSims)
    {
      totalCharsLost<<-0
      randomIndex<-sample(ncol(extantDataset),ncol(extantDataset))
      
      for(j in 1:(ncol(extantDataset))){
        if(linkage==T){
          print(charsToRemove[i,j])
          chooseCharsToRemove6(charsToRemove[i,j],GlobalLoss,correlationMatrix,softChars,maxCharsLost)->charsLostArray[,randomIndex[j],i]
        } else {
          chooseCharsNoInteraction(charsToRemove[i,j],GlobalLoss,maxCharsLost)->charsLostArray[,j,i]
        }
      }
    }
  }
  
  endTime<-proc.time()[3]
  charsLostArray
}




####Other Functions####
#Correlaion Matrix
createCorrelationMatrix<-function(data)
{
   output=matrix(nrow=nrow(data),ncol=nrow(data))
   for(i in 1:nrow(data)-1)
   {
     for(j in 1:nrow(data)-1)
     {
       cooccurence(i,j+1,data)->output[i,j+1]
     }
   }
   output[nrow(data),]=output[,nrow(data)]
   output[nrow(data),nrow(data)]=1
   output->>COR
   output
}

createCorrelationMatrix2<-function(data,addit)
{
   output=matrix(nrow=nrow(data),ncol=nrow(data))
   for(i in 1:nrow(data)-1)
   {
     for(j in 1:nrow(data)-1)
     {
       cooccurence(i,j+1,data)->output[i,j+1]
     }
   }
   output[nrow(data),]=output[,nrow(data)]
   output[nrow(data),nrow(data)]=1
   addit+output->>COR
   addit+output
}

createCorrelationMatrix3<-function(data,addit)
{
   output=matrix(0,nrow=nrow(data),ncol=nrow(data))
   for(i in 1:nrow(data)-1)
   {
     for(j in 1:nrow(data)-1)
     {
       cooccurenceLOSS(i,j+1,data)->output[i,j+1]
     }
   }
   output[nrow(data),]=output[,nrow(data)]
   output[nrow(data),nrow(data)]=1
   addit+output->>COR
   addit+output
}

cooccurence<-function(a,b,data)
{
  tempSum<-sum(data[a,]-data[b,]==0,na.rm=T)/ncol(data)
}

cooccurenceLOSS<-function(a,b,data)
{
  if(sum(data[a,]==0)!=0) #if there is character loss
  {
    sum((data[a,(data[a,]==0)]-data[b,data[a,]==0])==0,na.rm=T)/sum(data[a,]==0)
  }
  else
  {return(0)}
}

createGlobalLoss<-function(data=extinctDataset) #distribution of MISSINGNESS
{

  GlobalLoss<-function(trait,data)
  {
    1-(sum(data[trait,]>0)/ncol(data))
  }
  
  sapply(1:nrow(data),GlobalLoss,data=data)->output
  output->>GL
  output
}


chooseCandidate<-function(diagonal)
{
  currentWeights=1-diagonal
  sample(length(diagonal),1,prob=currentWeights)
}

checkCandidate<-function(charCheck,correlation,removed)
{
  runif(1)->ran

  if(ran<prod(correlation[charCheck,removed]))
    return(TRUE)
  else
    return(FALSE)
}

adjustOccurence<-function(occurence,candidate) #->currentOccurence
{
  occurence[candidate]=0
  occurence
}

adjustChars<-function(chars,candidate,i){
  chars[i]=candidate
}


createSampleDistribution<-function(occurence,correlation,removedChars,i) #->currentWeights
{  
  occurence*correlation[removedChars[i],]->adjustedOccurence
  adjustedOccurence
}


createGlobalLoss2<-function(data=extinctDataset,addition) #distribution of MISSINGNESS
{

  GlobalLoss<-function(trait,data,addit)
  {
    (1-(sum(data[trait,]>0)/ncol(data)))
  }
  
  output<-matrix(0,nrow=nrow(data),ncol=nrow(data))
  sapply(1:nrow(data),GlobalLoss,data=data,addit=addition)->output
  addition+output->>GL
  addition+output
}

chooseCharsToRemove5<-function(charsToLose,occurence,correlation,softChars) #->removedChars
{
  currentOccurence=occurence
  removedChars<-rep(0,charsToLose)
  numRemovedChars<-0
  

  
  while(numRemovedChars<charsToLose)
  {
    if(numRemovedChars>softChars)
    {
      FPSsample(length(occurence),weights=currentOccurence)->candidate
    }
    else #seeding soft characters
    {
      candidate<-length(occurence)-numRemovedChars
    }
    numRemovedChars=numRemovedChars+1
    
    removedChars[numRemovedChars]=as.numeric(candidate)
    createSampleDistribution2(currentOccurence,correlation,removedChars,numRemovedChars)->currentOccurence #multiplies global with co-occurrence
    adjustOccurence(currentOccurence,candidate)->currentOccurence #sets sampled index weight to 0, no replacement

    adjustChars(removedChars,candidate,numRemovedChars)
  }
  removedChars
}

FPSsample<-function(n,weights)#->candidate
{
  sortedChars<-sort(weights,index.return=T)
  sortedChars[[1]]=sortedChars[[1]]/sum(sortedChars[[1]])
  sortedChars[[1]]=cumsum(sortedChars[[1]])
  min(which(sortedChars[[1]]>=runif(1)))->selected
  
  sortedChars[[2]][selected]
}
  
createSampleDistribution2<-function(occurence,correlation,removedChars,i) #->currentWeights WITH Bayes' normalization
{  
  occurence*correlation[removedChars[i],]/GL[removedChars[i]]->adjustedOccurence
  
  adjustedOccurence
}

#no linkage
chooseCharsNoInteraction<-function(charsToLose,GlobalLoss,maxlength=NULL)
{
  sample(length(GlobalLoss),charsToLose,prob=GlobalLoss)->removedChars

  if(!is.null(maxlength))
    length(removedChars)=maxlength
  removedChars
}

chooseCharsToRemove6<-function(charsToLoseNorm,occurence,correlation,softChars,maxlength) #->removedChars
{
  currentOccurence=occurence
  removedChars<-rep(NA,charsToLoseNorm)
  numRemovedChars<-0

  
  while(numRemovedChars<charsToLoseNorm)
  {
    if(totalCharsLost==totalCharsToBeLost)
      { break }
    if(numRemovedChars>softChars)
    {
      FPSsample(length(occurence),weights=currentOccurence)->candidate
    }
    else #seeding soft characters
    {
      candidate<-length(occurence)-numRemovedChars
    }
    numRemovedChars=numRemovedChars+1
    
    removedChars[numRemovedChars]=as.numeric(candidate)
    createSampleDistribution2(currentOccurence,correlation,removedChars,numRemovedChars)->currentOccurence #multiplies global with co-occurrence
    adjustOccurence(currentOccurence,candidate)->currentOccurence #sets sampled index weight to 0, no replacement
    
    totalCharsLost<<-totalCharsLost+1
    adjustChars(removedChars,candidate,numRemovedChars)
  }
  
  length(removedChars)=maxlength
  removedChars
}



###Apply character loss array to extantDataset
applyToExtant<-function(simArray)
{
  
  #get dimensions
  extantData=extantDataset
  numSims<-dim(simArray)[3]
  numChars<-dim(extantData)[1]
  numTaxa<-dim(simArray)[2]
  
  #create array of extant before removing
  output<-rep(extantData,numSims)
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

removeCharacters<-function(taxa,dataMatrix,lossMatrix,sim)
{
  dataMatrix[lossMatrix[,taxa,sim],taxa,sim]=NA
  dataMatrix[,taxa,sim]
}


PCOsimulateAndanalyze <- function(extinctDataset=importExtinct, extantDataset=importExtant, importPCO=importPCO, numSims=100, charsToLose=1, linkage=T, softChars=0, sigma=NULL, normDist=F, poisDist=T, raw.dist.method, pco.dist.method, axes=10) {
    
    numCharsLost<<-charsToLose
    
  simulatedCharLossArray<- simulateCharacterLoss(extinctDataset=extinctDataset, extantDataset=extantDataset, numSims=numSims, charsToLose=charsToLose, linkage=linkage, softChars=softChars, sigma=sigma, normDist=normDist, poisDist=poisDist)
  
  #apply character loss to extant matrix
  applyToExtant(simulatedCharLossArray)->simulatedLossArray
  
  #need list, flip chars and taxa
  simulatedLossList<-list()
  for(i in 1:dim(simulatedLossArray)[3]){
    simulatedLossList[[i]]<-t(simulatedLossArray[,,i])
  }
  

  pco.list<-calculate.pco.list(input=simulatedLossList, axes=axes, method=raw.dist.method)
   
   PCO.Sim <<- pco.list
   
   pco.distance <- calculate.ordination.difference(pco.data=pco.list, no.loss.pco=importPCO, axes=axes, method=pco.dist.method)

 
  
  cat(numCharsLost, as.matrix(pco.distance), axes, "\n", file="PCO.output.txt", append=T)
}






##Disparity

DisparitysimulateAndanalyze<-function(extinctDataset=importExtinct, extantDataset=importExtant, numSims=10, charsToLose=1, linkage=T, softChars=0, sigma,normDist=F, poisDist=T,axes=10){
    
    numCharsLost<<-charsToLose
    
  simulateCharacterLoss(extinctDataset=extinctDataset, extantDataset=extantDataset,numSims=numSims,charsToLose=charsToLose,linkage=linkage,
                        softChars=softChars,sigma=sigma,normDist=normDist,poisDist=poisDist)->simulatedCharLossArray
  
  #apply character loss to extant matrix
  applyToExtant(simulatedCharLossArray)->simulatedLossArray
  
  #need list, flip chars and taxa
  simulatedLossList<-list()
  for(i in 1:dim(simulatedLossArray)[3]){
    simulatedLossList[[i]]<-t(simulatedLossArray[,,i])
  }
  
 #Bringing in the disparity calculating functions.
  MATRIXList(data=simulatedLossList,axes=10,method="manhattan")->preparedList
  DisparityList(data=preparedList,iterations=1000,axes=10,C.I.L=0.025,C.I.U=0.975)->disparity
    
    #The script will save to space delimited text file and continually update as more simulations are run. Sebsequent simulation outputs are added to this file at the bottom, nothing is overwritten.
  cat(numCharsLost,as.matrix(disparity),axes,"\n",file="Disparity.output.txt",append=T)
}

####Distance Matrix and PCO Calculator####
MATRIXList <- function(data=data, axes=axes, method=method) {
Final.PCO.Output<-NULL
Final.PCO.Output<-as.list(Final.PCO.Output)

Final.PCO.Group<-NULL
Final.PCO.Group<-as.list(Final.PCO.Group)

for(i in 1:length(data)){
    EuMatx<-vegdist(data[[i]], method=method, na.rm=TRUE)

    EuMatx[is.na(EuMatx)] <- 0

    FMatx <- as.matrix(EuMatx)


    PCO.Output<-cmdscale(FMatx, k = axes, eig = FALSE, add = FALSE, x.ret = FALSE)
    
    PCO.Output.Total<-PCO.Output[1:24,]
    PCO.Output.Taxonomic.Group.1<-PCO.Output[1:14,]
    PCO.Output.Taxonomic.Group.2<-PCO.Output[15:24,]
    
    Final.PCO.Group<-list(PCO.Output.Total, PCO.Output.Taxonomic.Group.1, PCO.Output.Taxonomic.Group.2)
    Final.PCO.Output[i]<-list(Final.PCO.Group)
    
    }
    Final.PCO.Output
}


####Disparity Script####
DisparityList <- function(data=data, iterations=iterations, axes=axes, C.I.L=C.I.L, C.I.U=C.I.U) {

result<-NULL
result<-as.list(result)
Fresult<-NULL
Fresult<-as.list(Fresult)
Gresult<-NULL
Gresult<-as.list(Gresult)


for(p in 1:length(data)){
    for(g in 1:length(data[[1]])){
        for(i in 1:iterations){
        
            output<-as.matrix(data[[p]][[g]][sample(1:nrow(data[[p]][[g]]),nrow(data[[p]][[g]]),T),])
                    result[i] <- list(output)
            
                    }
                Gresult[g]<-list(result)
            }
        
        Fresult[p]<-list(Gresult)
        
    }

#Setting up empty space for the sum and product metrics.
try1<-NULL
try1<-vector("list", length(Fresult))
try2<-NULL
try2<-vector("list", length(Fresult))
try3<-NULL
try3<-vector("list", length(Fresult))


for(k in 1:length(Fresult)){
    for(l in 1:length(Fresult[[1]])){       
        
        Try<-matrix(data=NA, nrow = length(result), ncol = ncol(data[[p]][[g]])) 
            try1[[k]][[l]]<-Try
          try2[[k]][[l]]<-Try
        }
    }   

#Setting up empty space for the MPD
for(k in 1:length(Fresult)){
  for(l in 1:length(Fresult[[1]])){     

      Try<-list()
      try3[[k]][[l]]<-Try
    }
}



for(v in 1:length(Fresult)){
    for(z in 1:length(Fresult[[1]])){   
        for(a in 1:length(result)){
            for(j in 1:ncol(data[[p]][[g]])){
                    try1[[v]][[z]][a,j] <-((max(Fresult[[v]][[z]][[a]][,j]))-(min(Fresult[[v]][[z]][[a]][,j])))
            }
        }
    }
}

for(vv in 1:length(Fresult)){
    for(zz in 1:length(Fresult[[1]])){  
        for(aa in 1:length(result)){
            for(jj in 1:ncol(data[[p]][[g]])){
                    try2[[vv]][[zz]][aa,jj] <-var(Fresult[[vv]][[zz]][[aa]][,jj])
            }
    }
    }
}

for(vvv in 1:length(Fresult)){
  for(zzz in 1:length(Fresult[[1]])){   
    for(aaa in 1:length(result)){
        try3[[vvv]][[zzz]][[aaa]] <-vegdist(Fresult[[vvv]][[zzz]][[aaa]], method="euclidean", na.rm=TRUE)
    }
  }
}


#Set-up for the NthRoot function
NthRoot<-function(x=data, n=axes){
    x^(1/n)
}


##Start of Mean pairwise Distance##
MPD<-NULL
MPD<-vector("list", length(Fresult))
  
for(k in 1:length(Fresult)){
    MPD[[k]]<-matrix(data=NA, nrow = length(Fresult[[1]]), ncol = length(result))  
  }
  
for(ss in 1:length(Fresult)){
  for(ff in 1:length(Fresult[[1]])){    
    for(kk in 1:length(result)){
        MPD[[ss]][ff,kk]<-mean(try3[[ss]][[ff]][[kk]])
    }
  }
}


MPD.quant <- NULL
MPD.quant <-vector("list", length(Fresult))

for(tt in 1:length(Fresult)){
  for(dd in 1:length(Fresult[[1]])){
    MPD.quant[[tt]][[dd]]<-quantile(MPD[[tt]][dd,], probs=c(C.I.L,C.I.U))
  }
}


MPD.Mean <- NULL
MPD.Mean <-vector("list", length(Fresult))

for(vv in 1:length(Fresult)){
  for(bb in 1:length(Fresult[[1]])){
    MPD.Mean[[vv]][[bb]]<-(mean(MPD[[vv]][bb,]))
  }
}



MPD.FinalResult<-list(MPD.Mean, MPD.quant)



MPD.Quant.List<-NULL    

MPD.Quant.Trans<-matrix(data=NA, nrow = 2, ncol = length(MPD.FinalResult[[2]][[1]]))
MPD.Quant.List<-vector("list", length(Fresult))

for(q in 1:length(MPD.FinalResult[[2]])){
  for(n in 1:length(MPD.FinalResult[[2]][[1]])){        
    trans.MPD<-matrix(MPD.FinalResult[[2]][[q]][[n]])
    MPD.Quant.Trans[,n]<-trans.MPD
    
  }
  
  MPD.Quant.List[[q]]<-MPD.Quant.Trans
  
}

MPD.Mean.Vecs<-NULL
MPD.Mean.Vecs<-matrix(data=NA, nrow = length(MPD.FinalResult[[1]][[1]]), ncol = length(MPD.FinalResult[[1]]))

for(bb in 1:length(MPD.FinalResult[[1]][[1]])){ 
  for(aa in 1:length(MPD.FinalResult[[1]])){
    
    MPD.Mean.Vecs[bb,aa]<-as.vector(MPD.FinalResult[[1]][[aa]][bb], mode="numeric")
    
  }
}


MPD.Quant.Vec<-NULL
MPD.Quant.Vecs<-NULL
MPD.Quant.Vecs<-matrix(data=NA, nrow = 2, ncol = ncol(MPD.Quant.List[[1]]))

for(cc in 1:ncol(MPD.Quant.List[[1]])){
  for(dd in 1:nrow(MPD.Quant.List[[1]])){
    for(ee in 1:length(MPD.Quant.List)){
      MPD.Quant.Vec[ee]<-as.vector(MPD.Quant.List[[ee]][dd,cc])
      
    }
    MPD.Quant.Vecs[dd,cc]<-mean(MPD.Quant.Vec)
    
  }
  
}

MPD.Final.Mean<-rowMeans(MPD.Mean.Vecs)

#Refer to README for changing empty vectors
MPD.Disparity.Out<-cbind(MPD.Final.Mean[1], MPD.Quant.Vecs[1,1], MPD.Quant.Vecs[2,1], MPD.Final.Mean[2], MPD.Quant.Vecs[1,2], MPD.Quant.Vecs[2,2], MPD.Final.Mean[3], MPD.Quant.Vecs[1,3], MPD.Quant.Vecs[2,3])

MPD.Disparity.Output<-as.data.frame(MPD.Disparity.Out)


####Start of the Products####

PoV.prod<-NULL
PoV.prod<-vector("list", length(Fresult))

for(ss in 1:length(Fresult)){
    for(ff in 1:length(Fresult[[1]])){  
PoV.prod[[ss]][[ff]]<-NthRoot(apply(try2[[ss]][[ff]],1,prod))
    }
}


PoV.quant <- NULL
PoV.quant <-vector("list", length(Fresult))

for(tt in 1:length(Fresult)){
    for(dd in 1:length(Fresult[[1]])){
PoV.quant[[tt]][[dd]]<-quantile(PoV.prod[[tt]][[dd]], probs=c(C.I.L,C.I.U))
    }
}



P.Variance.Mean <- NULL
P.Variance.Mean <-vector("list", length(Fresult))

for(vv in 1:length(Fresult)){
    for(bb in 1:length(Fresult[[1]])){
P.Variance.Mean[[vv]][[bb]]<-(mean(PoV.prod[[vv]][[bb]]))
    }
}



P.FinalResultV<-list(P.Variance.Mean, PoV.quant)

    
    
P.Quant.List.V<-NULL    
        
P.Quant.Trans.V<-matrix(data=NA, nrow = 2, ncol = length(P.FinalResultV[[2]][[1]]))
P.Quant.List.V<-vector("list", length(Fresult))
    
for(q in 1:length(P.FinalResultV[[2]])){
    for(n in 1:length(P.FinalResultV[[2]][[1]])){       
        trans.v<-matrix(P.FinalResultV[[2]][[q]][[n]])
            P.Quant.Trans.V[,n]<-trans.v
        
        }
    
        P.Quant.List.V[[q]]<-P.Quant.Trans.V
    
    }


P.Mean.Vecs.V<-NULL
P.Mean.Vecs.V<-matrix(data=NA, nrow = length(P.FinalResultV[[1]][[1]]), ncol = length(P.FinalResultV[[1]]))

for(bb in 1:length(P.FinalResultV[[1]][[1]])){  
    for(aa in 1:length(P.FinalResultV[[1]])){
                    
            P.Mean.Vecs.V[bb,aa]<-as.vector(P.FinalResultV[[1]][[aa]][bb], mode="numeric")
            
        }
    }


P.Quant.Vec.V<-NULL
P.Quant.Vecs.V<-NULL
P.Quant.Vecs.V<-matrix(data=NA, nrow = 2, ncol = ncol(P.Quant.List.V[[1]]))

for(cc in 1:ncol(P.Quant.List.V[[1]])){
    for(dd in 1:nrow(P.Quant.List.V[[1]])){
        for(ee in 1:length(P.Quant.List.V)){
            P.Quant.Vec.V[ee]<-as.vector(P.Quant.List.V[[ee]][dd,cc])
            
        }
    P.Quant.Vecs.V[dd,cc]<-mean(P.Quant.Vec.V)
    
    }
    
}

P.Final.Mean.V<-rowMeans(P.Mean.Vecs.V)
    
#Refer to README for changing empty vectors 
P.Disparity.Out.V<-cbind(P.Final.Mean.V[1], P.Quant.Vecs.V[1,1], P.Quant.Vecs.V[2,1], P.Final.Mean.V[2], P.Quant.Vecs.V[1,2], P.Quant.Vecs.V[2,2], P.Final.Mean.V[3], P.Quant.Vecs.V[1,3], P.Quant.Vecs.V[2,3])

    P.Disparity.Output.V<-as.data.frame(P.Disparity.Out.V)



PoR.sum<-NULL
PoR.sum<-vector("list", length(Fresult))

for(s in 1:length(Fresult)){
    for(f in 1:length(Fresult[[1]])){   
PoR.sum[[s]][[f]]<-NthRoot(apply(try1[[s]][[f]],1,prod))
    }
}

PoR.quant <- NULL
PoR.quant <-vector("list", length(Fresult))

for(t in 1:length(Fresult)){
    for(d in 1:length(Fresult[[1]])){
PoR.quant[[t]][[d]]<-quantile(PoR.sum[[t]][[d]], probs=c(C.I.L,C.I.U))
    }
}



P.Ranges.Mean <- NULL
P.Ranges.Mean <-vector("list", length(Fresult))

for(v in 1:length(Fresult)){
    for(b in 1:length(Fresult[[1]])){
P.Ranges.Mean[[v]][[b]]<-(mean(PoR.sum[[v]][[b]]))
    }
}



P.FinalResultR<-list(P.Ranges.Mean, PoR.quant)

    
    
P.Quant.List<-NULL  
        
P.Quant.Trans<-matrix(data=NA, nrow = 2, ncol = length(P.FinalResultR[[2]][[1]]))
P.Quant.List<-vector("list", length(Fresult))
    
for(q in 1:length(P.FinalResultR[[2]])){
    for(n in 1:length(P.FinalResultR[[2]][[1]])){       
        trans<-matrix(P.FinalResultR[[2]][[q]][[n]])
            P.Quant.Trans[,n]<-trans
        
        }
    
        P.Quant.List[[q]]<-P.Quant.Trans
    
    }


P.Mean.Vecs<-NULL
P.Mean.Vecs<-matrix(data=NA, nrow = length(P.FinalResultR[[1]][[1]]), ncol = length(P.FinalResultR[[1]]))

for(bb in 1:length(P.FinalResultR[[1]][[1]])){  
    for(aa in 1:length(P.FinalResultR[[1]])){
                    
            P.Mean.Vecs[bb,aa]<-as.vector(P.FinalResultR[[1]][[aa]][bb], mode="numeric")
            
        }
    }

P.Quant.Vec<-NULL
P.Quant.Vecs<-NULL
P.Quant.Vecs<-matrix(data=NA, nrow = 2, ncol = ncol(P.Quant.List[[1]]))

for(cc in 1:ncol(P.Quant.List[[1]])){
    for(dd in 1:nrow(P.Quant.List[[1]])){
        for(ee in 1:length(P.Quant.List)){
            P.Quant.Vec[ee]<-as.vector(P.Quant.List[[ee]][dd,cc])
            
        }
    P.Quant.Vecs[dd,cc]<-mean(P.Quant.Vec)
    
    }
    
}

P.Final.Mean<-rowMeans(P.Mean.Vecs)
    
#Refer to README for changing empty vectors 
P.Disparity.Out<-cbind(P.Final.Mean[1], P.Quant.Vecs[1,1], P.Quant.Vecs[2,1], P.Final.Mean[2], P.Quant.Vecs[1,2], P.Quant.Vecs[2,2], P.Final.Mean[3], P.Quant.Vecs[1,3], P.Quant.Vecs[2,3])

    P.Disparity.Output<-as.data.frame(P.Disparity.Out)


    P.Disparity.Output.Final<-cbind(P.Disparity.Output, P.Disparity.Output.V)
    


###Start of the sums



SoV.sum<-NULL
SoV.sum<-vector("list", length(Fresult))

for(ss in 1:length(Fresult)){
    for(ff in 1:length(Fresult[[1]])){  
SoV.sum[[ss]][[ff]]<-apply(try2[[ss]][[ff]],1,sum)
    }
}



SoV.quant <- NULL
SoV.quant <-vector("list", length(Fresult))

for(tt in 1:length(Fresult)){
    for(dd in 1:length(Fresult[[1]])){
SoV.quant[[tt]][[dd]]<-quantile(SoV.sum[[tt]][[dd]], probs=c(C.I.L,C.I.U))
    }
}



Variance.Mean <- NULL
Variance.Mean <-vector("list", length(Fresult))

for(vv in 1:length(Fresult)){
    for(bb in 1:length(Fresult[[1]])){
Variance.Mean[[vv]][[bb]]<-(mean(SoV.sum[[vv]][[bb]]))
    }
}



FinalResultV<-list(Variance.Mean, SoV.quant)

    
    
Quant.List.V<-NULL  
        
Quant.Trans.V<-matrix(data=NA, nrow = 2, ncol = length(FinalResultV[[2]][[1]]))
Quant.List.V<-vector("list", length(Fresult))
    
for(q in 1:length(FinalResultV[[2]])){
    for(n in 1:length(FinalResultV[[2]][[1]])){     
        trans.v<-matrix(FinalResultV[[2]][[q]][[n]])
            Quant.Trans.V[,n]<-trans.v
        
        }
    
        Quant.List.V[[q]]<-Quant.Trans.V
    
    }


Mean.Vecs.V<-NULL
Mean.Vecs.V<-matrix(data=NA, nrow = length(FinalResultV[[1]][[1]]), ncol = length(FinalResultV[[1]]))

for(bb in 1:length(FinalResultV[[1]][[1]])){    
    for(aa in 1:length(FinalResultV[[1]])){
                    
            Mean.Vecs.V[bb,aa]<-as.vector(FinalResultV[[1]][[aa]][bb], mode="numeric")
            
        }
    }

Quant.Vec.V<-NULL
Quant.Vecs.V<-NULL
Quant.Vecs.V<-matrix(data=NA, nrow = 2, ncol = ncol(Quant.List.V[[1]]))

for(cc in 1:ncol(Quant.List.V[[1]])){
    for(dd in 1:nrow(Quant.List.V[[1]])){
        for(ee in 1:length(Quant.List.V)){
            Quant.Vec.V[ee]<-as.vector(Quant.List.V[[ee]][dd,cc])
            
        }
    Quant.Vecs.V[dd,cc]<-mean(Quant.Vec.V)
    
    }
    
}

Final.Mean.V<-rowMeans(Mean.Vecs.V)
    
#Refer to README for changing empty vectors 
Disparity.Out.V<-cbind(Final.Mean.V[1], Quant.Vecs.V[1,1], Quant.Vecs.V[2,1], Final.Mean.V[2], Quant.Vecs.V[1,2], Quant.Vecs.V[2,2], Final.Mean.V[3], Quant.Vecs.V[1,3], Quant.Vecs.V[2,3])

    Disparity.Output.V<-as.data.frame(Disparity.Out.V)





SoR.sum<-NULL
SoR.sum<-vector("list", length(Fresult))

for(s in 1:length(Fresult)){
    for(f in 1:length(Fresult[[1]])){   
SoR.sum[[s]][[f]]<-apply(try1[[s]][[f]],1,sum)
    }
}

SoR.quant <- NULL
SoR.quant <-vector("list", length(Fresult))

for(t in 1:length(Fresult)){
    for(d in 1:length(Fresult[[1]])){
SoR.quant[[t]][[d]]<-quantile(SoR.sum[[t]][[d]], probs=c(C.I.L,C.I.U))
    }
}



Ranges.Mean <- NULL
Ranges.Mean <-vector("list", length(Fresult))

for(v in 1:length(Fresult)){
    for(b in 1:length(Fresult[[1]])){
Ranges.Mean[[v]][[b]]<-(mean(SoR.sum[[v]][[b]]))
    }
}



FinalResultR<-list(Ranges.Mean, SoR.quant)

    
    
Quant.List<-NULL    
        
Quant.Trans<-matrix(data=NA, nrow = 2, ncol = length(FinalResultR[[2]][[1]]))
Quant.List<-vector("list", length(Fresult))
    
for(q in 1:length(FinalResultR[[2]])){
    for(n in 1:length(FinalResultR[[2]][[1]])){     
        trans<-matrix(FinalResultR[[2]][[q]][[n]])
            Quant.Trans[,n]<-trans
        
        }
    
        Quant.List[[q]]<-Quant.Trans
    
    }


Mean.Vecs<-NULL
Mean.Vecs<-matrix(data=NA, nrow = length(FinalResultR[[1]][[1]]), ncol = length(FinalResultR[[1]]))

for(bb in 1:length(FinalResultR[[1]][[1]])){    
    for(aa in 1:length(FinalResultR[[1]])){
                    
            Mean.Vecs[bb,aa]<-as.vector(FinalResultR[[1]][[aa]][bb], mode="numeric")
            
        }
    }


Quant.Vec<-NULL
Quant.Vecs<-NULL
Quant.Vecs<-matrix(data=NA, nrow = 2, ncol = ncol(Quant.List[[1]]))

for(cc in 1:ncol(Quant.List[[1]])){
    for(dd in 1:nrow(Quant.List[[1]])){
        for(ee in 1:length(Quant.List)){
            Quant.Vec[ee]<-as.vector(Quant.List[[ee]][dd,cc])
            
        }
    Quant.Vecs[dd,cc]<-mean(Quant.Vec)
    
    }
    
}

Final.Mean<-rowMeans(Mean.Vecs)
    
#Refer to README for changing empty vectors 
Disparity.Out<-cbind(Final.Mean[1], Quant.Vecs[1,1], Quant.Vecs[2,1], Final.Mean[2], Quant.Vecs[1,2], Quant.Vecs[2,2], Final.Mean[3], Quant.Vecs[1,3], Quant.Vecs[2,3])



    Disparity.Output<-as.data.frame(Disparity.Out)



    S.Disparity.Output.Final<-cbind(Disparity.Output, Disparity.Output.V)
        
    Complete.Disparity.Output.Final<-cbind(S.Disparity.Output.Final, P.Disparity.Output.Final, MPD.Disparity.Output)
    Complete.Disparity.Output.Final

}