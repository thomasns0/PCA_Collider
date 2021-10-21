PCA_Select<-function(Pheno, Environment, Confounders, 
                     PCA_Method="hetcorEigen", ParallelIterations=1000, TrimCorrMatrix=TRUE, Imputation="None", FailSafeCorrelations=FALSE){

  #######################################  #######################################
  #######################################
  #Trim cor matrix#######################
  #######################################
  if(TrimCorrMatrix==TRUE){
    print(" ")
    print("************************************")
    print("Variable pool trimmed by hetcor")
    print("Variable retained if 95%CI for cor with Pheno and Env does not include 0")
    print("hetcor() https://www.rdocumentation.org/packages/polycor/versions/0.7-10/topics/hetcor")
    print("************************************")
    print(" ")
    keep<-data.frame(matrix(nrow=ncol(Confounders), ncol=1))
    for ( i in 1:ncol(Confounders)){
      
      possibleError1<-tryCatch(
      PhenoCor<-hetcor(data.frame(Pheno, Confounders[,i]), ML=TRUE, use="pairwise.complete.obs") ,
      error=function(e) e )
      if(inherits(possibleError1, "error")) {
        keep[i,]<-0
      }
      if(inherits(possibleError1, "error")) next
      
      possibleError2<-tryCatch(
        EnvCor<-hetcor(data.frame(Environment, Confounders[,i]), ML=TRUE, use="pairwise.complete.obs") , 
        error=function(e) e )
      if(inherits(possibleError2, "error")) {
        keep[i,]<-0
      }
      if(inherits(possibleError2, "error")) next
      
      PhenoCor<-possibleError1
      EnvCor<-possibleError2
      CI95_1<-1.96*PhenoCor$std.errors[1,2]
      lo_1<-PhenoCor$correlations[1,2]-CI95_1
      hi_1<-PhenoCor$correlations[1,2]+CI95_1
      CI95_2<-1.96*EnvCor$std.errors[1,2]
      lo_2<-EnvCor$correlations[1,2]-CI95_2
      hi_2<-EnvCor$correlations[1,2]+CI95_2
      keep[i,]<-ifelse(  ( (0>=lo_1&0<=hi_1) & (0>=lo_2&0<=hi_2) ), 0, 1)
       }
  
    if( sum(keep) != ncol(Confounders)){
      print(" ")
      print("************************************")
      print("******Variables removed from PCA******")
      print("************************************")
      print(" ")
      print(colnames(Confounders)[keep==0])
    }
  
    if( sum(keep) == ncol(Confounders)){
      print(" ")
      print("************************************")
      print("******No variables removed from PCA******")
      print("************************************")
      print(" ")
    }
  
    ConfoundersUse<-Confounders[,keep==1]
  }
  
  if(TrimCorrMatrix==FALSE){
    ConfoundersUse<-Confounders
  }
  
  #######################################  #######################################
  #######################################
  #IMPUTATIONS###########################
  #######################################
    
  #######################################
  #######################################
  if (Imputation=="None"){
    print(" ")
    print("************************************")
    print("******No Imputation******")
    print("******Complete Cases Only******")
    print("************************************")
    print(" ")
  }
    
  #######################################
  #######################################
  if (Imputation=="knn"){
    print(" ")
    print("************************************")
    print("******knn Imputation******")
    print("Mean aggregation for continuous, maxCat for factors")
    print("kNN() https://rdrr.io/cran/VIM/man/kNN.html")
    print("************************************")
    print(" ")

    #sort from least missing to most
    temp <- data.frame(matrix(nrow=ncol(ConfoundersUse), ncol= 2))
    for ( i in 1:ncol(ConfoundersUse)){
      temp[i,1] <- colnames(ConfoundersUse)[i]
      temp[i,2]<-sum(is.na(ConfoundersUse[,i]))
    }
    temp<-temp[order(temp$X2),]
    ConfoundersUse<-ConfoundersUse[,temp$X1]
    
    #kNN imputation
    ConfoundersUse<-kNN(
      ConfoundersUse,
      variable = colnames(ConfoundersUse),
      metric = NULL,
      k = 5,
      dist_var = colnames(ConfoundersUse),
      weights = "auto",
      numFun = mean,
      catFun = maxCat,
      makeNA = NULL,
      NAcond = NULL,
      impNA = FALSE,
      donorcond = NULL,
      mixed = vector(),
      mixed.constant = NULL,
      trace = FALSE,
      imp_var = FALSE,
      imp_suffix = "imp",
      addRF = FALSE,
      onlyRF = FALSE,
      addRandom = FALSE,
      useImputedDist = TRUE,
      weightDist = FALSE
    )
  }

  #######################################
  #######################################
  if (Imputation=="Mean"){
    print(" ")
    print("************************************")
    print("******Mean Imputation for Continuous, Skipped for Factors******")
    print("******Subjects are removed if they have any missingness on factor variables******")
      for(i in 1:ncol(ConfoundersUse)) {
        if (class(ConfoundersUse[ , i]) == "factor"){print(paste0("Factor variable ", colnames(ConfoundersUse)[i]," skipped for mean imputation"))}
        if (class(ConfoundersUse[ , i]) == "factor"){next}
      ConfoundersUse[ , i][is.na(ConfoundersUse[ , i])] <- mean(ConfoundersUse[ , i], na.rm = TRUE)
      }
    print("************************************")
    print(" ")
  }
  #######################################
  #######################################
  if (Imputation=="MeanAndMedian"){
    print(" ")
    print("************************************")
    print("******Mean Imputation for Continuous, Median for Factors******")
    print("Not recommended, this is a placeholder method for factors with incomplete data")
    print("This is equivalent to Imputation=Mean if there are no factor variables")
    for(i in 1:ncol(ConfoundersUse)) {
      if (class(ConfoundersUse[ , i]) == "factor"){
        print(paste0("Missings on factor variable ", colnames(ConfoundersUse)[i]," set to median value"))
        temp<-levels(ConfoundersUse[ , i])
        ConfoundersUse[ , i]<-as.numeric(as.character(ConfoundersUse[ , i]))
        ConfoundersUse[ , i][is.na(ConfoundersUse[ , i])] <-round(median(ConfoundersUse[ , i], na.rm=T), digits=0)
        ConfoundersUse[ , i]<-factor(ConfoundersUse[ , i], levels=temp)
      }
      if (class(ConfoundersUse[ , i]) == "factor"){next}
      ConfoundersUse[ , i][is.na(ConfoundersUse[ , i])] <- mean(ConfoundersUse[ , i], na.rm = TRUE)
    }
    print("************************************")
    print(" ")
  }
  #######################################
  #######################################
  if (Imputation=="SVDmiss"){
    print(" ")
    print("************************************")
    print("******Mean Imputation followed by Iterative SVD******")
    print("******Does not work with factors******")
    print("******SVD requires continuous data, coercing all to numeric******")
    print("************************************")
    print(" ")
    print("SVDmiss() https://www.rdocumentation.org/packages/SpatioTemporal/versions/1.1.9.1/topics/SVDmiss")
    print("Citation for this method")
    print("M. Fuentes, P. Guttorp, and P. D. Sampson. (2006) Using Transforms to Analyze Space-Time Processes in Statistical methods for spatio-temporal systems (B. Finkenstadt, L. Held, V. Isham eds.) 77-150")
    for ( i in 1:ncol(ConfoundersUse)){
      ConfoundersUse[,i] <- as.numeric(as.character(ConfoundersUse[,i]))
    }
    SVDout<-SVDmiss(ConfoundersUse, niter = 5000, ncomp=ncol(ConfoundersUse), conv.reldiff=0.01)
    ConfoundersUse<-SVDout$Xfill
  }

  #######################################  #######################################
  #######################################
  #PCA METHODS###########################
  
  if (FailSafeCorrelations==TRUE){
    print(" ")
    print("************************************")
    print("******Fail Safe Correlations******")
    print("******Check every cell in polychoric correlation matrix for fit problems before PCA******")
    print("******Remove variable that returns highest n errors/warnings/NA******")
    print("******Repeat until no errors/warnings/NA******")
    print("************************************")
    print(" ")
    j<-1
    repeat{
    print(paste0("repeat", j))
    ListNums<-1:ncol(ConfoundersUse)
    ListNums<-expand.grid(ListNums,ListNums)
    outError<-NULL
    outWarning<-NULL
    outNA <- NULL
    for ( i in 1:nrow(ListNums)){
      possibleError<-tryCatch(
        PhenoCor<-hetcor(data.frame(ConfoundersUse[,ListNums$Var1[i]], 
                                    ConfoundersUse[,ListNums$Var2[i]]), ML=TRUE, use="pairwise.complete.obs") ,
        error=function(e) e ,
        warning = function(w) w)
      if(inherits(possibleError, "error")&(ListNums$Var1[i]!=ListNums$Var2[i])) {
      print( paste0("error for correlation of ",colnames(ConfoundersUse)[ListNums$Var1[i]]," ", colnames(ConfoundersUse)[ListNums$Var2[i]]) )
        outError<- rbind(outError, paste0(colnames(ConfoundersUse)[ListNums$Var1[i]],"ZZZZ",colnames(ConfoundersUse)[ListNums$Var2[i]]))
      }
      if(inherits(possibleError, "warning")&(ListNums$Var1[i]!=ListNums$Var2[i])) {
        print( paste0("warning for correlation of ", colnames(ConfoundersUse)[ListNums$Var1[i]]," ", colnames(ConfoundersUse)[ListNums$Var2[i]]) )
        outWarning<- rbind(outWarning, paste0(colnames(ConfoundersUse)[ListNums$Var1[i]],"ZZZZ",colnames(ConfoundersUse)[ListNums$Var2[i]]))
      }
      if(is.na(PhenoCor$correlations[1,2])&(ListNums$Var1[i]!=ListNums$Var2[i])){
        print( paste0("NA for correlation of ", colnames(ConfoundersUse)[ListNums$Var1[i]]," ", colnames(ConfoundersUse)[ListNums$Var2[i]]) )
        outNA<- rbind(outNA, paste0(colnames(ConfoundersUse)[ListNums$Var1[i]],"ZZZZ",colnames(ConfoundersUse)[ListNums$Var2[i]]))
      }
    }
    DROPerror<-names(sort(-table(unlist(str_split(outError, "ZZZZ", n=2)))))[1]
    DROPwarning<-names(sort(-table(unlist(str_split(outWarning, "ZZZZ", n=2)))))[1]
    DROPna<-names(sort(-table(unlist(str_split(outNA, "ZZZZ", n=2)))))[1]
    print(paste0("Removing variable that causes most errors:", DROPerror))
    print(paste0("Removing variable that causes most warnings:", DROPwarning))
    print(paste0("Removing variable that causes most na:", DROPna))
    ConfoundersUse<-ConfoundersUse[,colnames(ConfoundersUse) %in% c(DROPerror,DROPwarning,DROPna) ==FALSE]
    j<-j+1
    if(is.null(DROPerror) & is.null(DROPwarning) & is.null(DROPna)){
      print("No more errors/warnings/NA, ending troubleshoot")
      print(" ")
      print("************************************")
      print("Updated list of variables for PCA:")
      print("************************************")
      print(" ")
      print(colnames(ConfoundersUse))
      }
    if(is.null(DROPerror) & is.null(DROPwarning) & is.null(DROPna)){break}
    }
  }
    
  VariablesInPCA<-colnames(ConfoundersUse)
  #######################################
  #######################################
  #SVD is fine for continuous data
  if(PCA_Method=="SVD"){
    print(" ")
    print("************************************")
    print("******Generating PCs by SVD of imputed data******")    
    print("******SVD requires continuous data, coercing all to numeric******")
    print("************************************")
    print(" ")
    for ( i in 1:ncol(ConfoundersUse)){
      ConfoundersUse[,i] <- as.numeric(as.character(ConfoundersUse[,i]))
    }
    PC_Observed<-tryCatch(
    PC_Observed<-prcomp(ConfoundersUse, retx=TRUE, center=TRUE, scale=TRUE) ,
    error=function(e) e ,
    warning = function(w) w)
  if(inherits(PC_Observed, "warning")|inherits(PC_Observed, "error")){
    print("SVD of imputed correlation matrix produced errors or warnings")
    print("Edit input to Confounders argument manually or set FailSafeCorrelations=TRUE to systematically remove variables causing errors/warnings/NA in correlation matrix")
    print(e)
    print(w)
    break
  }
  print(" ")
  print("************************************")
  print("******Parallel Analysis (pearson, raw data)******")
  print("************************************")
  print(" ")
  keep_PCs<-paran(ConfoundersUse, iterations=ParallelIterations)
  PC_Confounders1<-list(PC_Observed$x, VariablesInPCA, keep_PCs$Retained)
  names(PC_Confounders1) <-c("PCs", "Variables", "Recommended_N_PCs")
  }
  describe(ConfoundersUse)
  #######################################
  #######################################
  #method for eigen decomposition of polychoric correlations to accomodate ordinal data
  if(PCA_Method=="hetcorEigen"){
    print(" ")
    print("************************************")
    print("******Generating PCs by eigendecomposition of imputed mixed correlation matrix******")    
    print("hetcor() https://www.rdocumentation.org/packages/polycor/versions/0.7-10/topics/hetcor")
    print("************************************")
    print(" ")
    #hetcor
    HETCORobject<-hetcor(ConfoundersUse, ML=TRUE, use="pairwise.complete.obs")
    #print cor type
    print(" ")
    print("************************************")
    print("******Input Correlation Matrix******")
    print("************************************")
    temp<-data.frame(HETCORobject$type)
    diag(temp)<-1
    print(colnames(ConfoundersUse))
    print(temp)
    #eigen decomp
    EigenOut<-tryCatch(
      EigenOut<- eigen(HETCORobject$correlations) ,
      error=function(e) e ,
      warning = function(w) w)
  if(inherits(EigenOut, "warning")|inherits(EigenOut, "error")){
    print("Eigendecomposition of imputed correlation matrix produced errors or warnings")
    print("Edit input to Confounders argument manually or set FailSafeCorrelations=TRUE to systematically remove variables causing errors/warnings/NA in correlation matrix")
    print(e)
    print(w)
    break
    }
    Eigenvectors <-EigenOut$vectors
    #no factors in obs for project back to data
    for ( i in 1:ncol(ConfoundersUse)){
      ConfoundersUse[,i] <- as.numeric(as.character(ConfoundersUse[,i]))
    }
    #2 methods for projecting back to data
    EigenMethod1<-as.matrix(ConfoundersUse) %*% Eigenvectors
    EigenMethod2<-t(t(ConfoundersUse) - rowMeans(t(ConfoundersUse))) %*% Eigenvectors
    #check 2 methods are the same
    #print(" ")
    #print("************************************")
    #print("******Correlation between first PCs 10 for 2 ways to handle Eigenvectors ")
    #print("(as.matrix(dat) %*% Eigenvectors)")
    #print("******VS******")
    #print("(t(t(dat) - rowMeans(t(dat))) %*% Eigenvectors)")
    #print("These should all be 1")
    #print("************************************")
    out <- NULL
    for ( i in 1:10){out<-rbind(out,(cor(EigenMethod1[,i], EigenMethod2[,i], use="pairwise.complete.obs")))}
    out<-sum(out)
    if (out != 10){
       print("Internal check that as.matrix(dat) %*% Eigenvectors AND (t(t(dat) - rowMeans(t(dat))) %*% Eigenvectors) are correlated at 1 failed. check eigendecomposition of imputed data correlation matrix")
       print("Returning imputed data instead of PCs")
       return(ConfoundersUse)
       break}
     #print(" ")
    print("************************************")
    print("******Parallel Analysis (hetcor matrix, n=nrow)******")
    print("************************************")
    print("paran() https://www.rdocumentation.org/packages/paran/versions/1.5.2/topics/paran")
    print(" ")
    keep_PCs<-paran(mat=HETCORobject$correlations, n=nrow(ConfoundersUse), iterations=ParallelIterations)
    PC_Confounders1<-list(EigenMethod1, VariablesInPCA, keep_PCs$Retained)
    names(PC_Confounders1) <-c("PCs", "Variables", "Recommended_N_PCs")
  }
    
    ####################################### #######################################
    #######################################
    #OUTPUT
  return(PC_Confounders1)
}