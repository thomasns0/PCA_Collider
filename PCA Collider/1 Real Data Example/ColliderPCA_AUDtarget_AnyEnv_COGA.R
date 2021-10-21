PRSlist <- c("ALC","EXT")
EnvList <- c("MAX_ftnd_sx", "EDU_max")
TargetPheno <- c("MAX_aud_5sx")#No index for target pheno rn, only use 1 per job

FactorForBinary<-TRUE

readCorePheno<-TRUE

SharedAxis <- TRUE
SharedMin <- (-0.21)
SharedMax <- 0.46
PRSLowerAxis<-  0
PRSUpperAxis <- 0.23

###############################################################
###############################################################
###############################################################
###############################################################
library(psych)
library(plyr)
library(paran)
library(stringr)
library(haven)
library(polycor)
library(VIM)
#source("/Users/thomasns/Documents/0home/CuttingRoomFloor.R")
source("/vcu_gpfs2/home/thomasns/ColliderPCA/PCA_Select.R")

for (PRS_INDEX in 1:length(PRSlist)){
for (ENVIRONMENT_INDEX in 1:length(EnvList)){
   
WhichPRS<-PRSlist[PRS_INDEX]
WhichENV<-EnvList[ENVIRONMENT_INDEX]

#PREP DATA
###########################################
###########################################
###########################################
###########################################
AUDres <- read.table("Resistance_phenos_03022020.csv",sep=",", header=T)
colnames(AUDres)
table(AUDres$base_pheno_peer_drink, AUDres$second_pheno_peer_drink, exclude=NULL)
AUDres<-AUDres[,c("ind_id", "base_pheno_peer_drink")]
colnames(AUDres) <- c("IND_ID", "peer_drink")
table(AUDres$peer_drink)
AUDres[AUDres=="."|AUDres=="K"]<-NA
AUDres$peer_drink <- as.numeric(AUDres$peer_drink)
AUDres$peer_drink<-as.numeric(scale(AUDres$peer_drink))
describe(AUDres)

w<-read.table("VTSEM_CleanedData_6.8.21.csv", sep=",", header=T)
w2<-read.table("ParentalAlcoholPhenos.csv", sep=",", header=T)
w_vtsem<-join(w,w2,by="IND_ID", type="left")
rm(w2)
#w2zz<-read.table("DataForScript_CoxGRMforFenn_12102020.csv", header=T,sep=",")
w2<-read.table("DataForScript_CoxGRMforFenn_12102020_NOexclusionCriteria.csv", header=T,sep=",")
#cbind(describe(w2)[,"n"],describe(w2zz)[,"n"]) # a previous run inadvertently applied exclusion criteria from previous study
#these commands show difference between n with and without exclusion criteria
#proceding with no exclusion criteria version

w<-join(w_vtsem,w2, by="IND_ID", type="full")
rm(w2)
rm(w_vtsem)

w<-join(w, AUDres, by="IND_ID", type="full")
table(w$sex)
table(w$NeverAlcoholInit)
w<-w[w$NeverAlcoholInit==0,]

w$EDU_max[w$EDU_max==20] <- NA
#w<-read.table("DivorceRawMaster_12_9_2020.txt", header=T, sep="\t")

if (WhichENV=="EverMarriageNeverDivorce"){
table(w$AnyMarriageEver,w$AnyDivorceEver, exclude=NULL)
w$EverMarriageNeverDivorce<-ifelse(w$AnyMarriageEver==1&w$AnyDivorceEver==0,1,
       ifelse(is.na(w$AnyMarriageEver)|is.na(w$AnyDivorceEver), NA,
              0))
table(w$EverMarriageNeverDivorce, exclude=NULL)
}
###########################################
###########################################
###########################################
#PRS 
if(WhichPRS=="EXT"){
   EA<-read.table("EVERYONE_EA_PCs.csv", header=T, sep=",")
   colnames(EA)[1]<-c("IND_ID")
   PRS <- read.table("COGA_EXT_PRScs_SS_RKL.profile", header=T)
   colnames(PRS)[2]<-c("IND_ID")
}

if(WhichPRS=="ALC"){
   EA<-read.table("EVERYONE_EA_PCs.csv", header=T, sep=",")
   colnames(EA)[1]<-c("IND_ID")
   PRS <- read.table("PROB_ALC_PRScs.profile", header=T)
   colnames(PRS)[2]<-c("IND_ID")
}


###########################################
###########################################
###########################################
#Merge and clean up variable pool
###########################################
EAPRS<-join(EA, PRS, by="IND_ID", type="left")
w2_EA<-join(EAPRS, w, by="IND_ID", type="inner")
w_EA<-w2_EA
colnames(w2_EA)
table(w2_EA$sex)
w2_EA<-w2_EA[,grep(names(w2_EA), pattern="IND_ID|EARLIEST|Earliest|earliest|MAX|MIN|sex|age|Parmar|Alcohol|Divorce|Marriage|.data", value=T)]
w2_EA[,grep(names(w2_EA), pattern="nValid|SOURCE", value=T)] <- NULL
w2_EA$MAX_aud_5dx <- NULL
w2_EA$MAX_aud_5sx <- NULL
w2_EA$MAX_aud_5dx_COMBINEDwithVMProbableAUD <- NULL
w2_EA$MAX_aud_5dx_COMBINEDwithVMBinary <- NULL
w2_EA$sex.1<-NULL
w2_EA$MAX_ad_4dx <- NULL
w2_EA$MAX_al_4ax <- NULL
w2_EA$EARLIEST_pardiv_father <- NULL
w2_EA$EARLIEST_pardiv_mother <- NULL
w2_EA$Earliest_abs_motherOrfather <- NULL
w2_EA$EARLIEST_fatherabs <-NULL
w2_EA$EARLIEST_motherabs <-NULL
w2_EA$ExclusionFlag4_Married3NoDivorce <- NULL
w2_EA$EarliestParmardiscProRatedSum <- NULL
w2_EA$.data_GenZ <- NULL
w2_EA$NeverAlcoholInit <- NULL
w2_EA$MAX_DM10_anyCohab.data_5<-NULL
w2_EA$AnyMarriageEver<-NULL
w2_EA$LengthMarriageRightCensored <- NULL
colnames(w2_EA)
describe(w2_EA)

#drop if more than 75% missing
DROP<-data.frame(matrix(nrow=ncol(w2_EA), ncol=1))
for ( i in 1:ncol(w2_EA)){
DROP[i,]<-ifelse( sum((is.na(w2_EA[,i]))) > (nrow(w2_EA)*0.75), 1, 0)
}
colnames(w2_EA)[DROP==1]
ncol(w2_EA)
w2_EA<-w2_EA[,DROP==0]
ncol(w2_EA)
w2_EA$AnyDivorceEver <- NULL
w2_EA$MAX_ftnd_sx <- NULL
w2_EA$MAX_EverTobac <- NULL
w2_EA$EARLIEST_livedwith <- NULL
w2_EA$MAX_DM11_MarriageCount <- NULL
w2_EA$MIN_aud_5ao <- NULL
w2_EA$MIN_AL40AgeOns  <- NULL
w2_EA$Earliest_livedwith_DivorceCodes <- NULL
w2_EA$Earliest_pardiv_motherOrfather <- NULL
w2_EA$AgeAtFirstMarriage <- NULL
w2_EA$MaxLengthOfMarriage <- NULL

colnames(w2_EA)
describe(w2_EA)
table(w2_EA$sex)
###########################################
###########################################
###########################################
###########################################
###########################################
#read in corepheno 
if(readCorePheno==TRUE){
   library(sjlabelled)
   corepheno<-haven::read_sas("core_pheno_20201120.sas7bdat")
   labs<-get_label(corepheno)
   colnames(corepheno)[1]
   colnames(corepheno)[1] <- "IND_ID"
   
   corepheno<-join(corepheno, data.frame(IND_ID=w_EA$IND_ID), type="right", by="IND_ID")
   
   if (WhichENV=="MeanIntRating"){
   colnames(corepheno)
   int_rating<-corepheno[,grep(names(corepheno), pattern="int_rating", value=T)]
   MeanIntRating<-ifelse(rowSums(!is.na(int_rating))>0,
                         rowMeans(int_rating, na.rm=T), 
                         NA)
   table(MeanIntRating)
   w_EA<-data.frame(w_EA, MeanIntRating)
   }
   
   Sum50MissThresh<-function(df){
      df[df==9]<-NA
      df[df==1]<-0
      df[df==5]<-1
      out<-ifelse(  ((rowSums(!is.na(df))) > (ncol(df)*0.5)),rowSums(df, na.rm=T), NA)
      return(out)
   }
IllicitDrugs<-data.frame(
IND_ID=corepheno[,"IND_ID"],
Sum50MissThresh(corepheno[,grep(names(corepheno), pattern="co_dep_max_sx[1-7]", value=T)]),
Sum50MissThresh(corepheno[,grep(names(corepheno), pattern="co_abuse_max_sx[1-4]", value=T)]),
Sum50MissThresh(corepheno[,grep(names(corepheno), pattern="op_dep_max_sx[1-7]", value=T)]),
Sum50MissThresh(corepheno[,grep(names(corepheno), pattern="op_abuse_max_sx[1-4]", value=T)]),
Sum50MissThresh(corepheno[,grep(names(corepheno), pattern="sd_dep_max_sx[1-7]", value=T)]),
Sum50MissThresh(corepheno[,grep(names(corepheno), pattern="sd_abuse_max_sx[1-4]", value=T)]),
Sum50MissThresh(corepheno[,grep(names(corepheno), pattern="st_dep_max_sx[1-7]", value=T)]),
Sum50MissThresh(corepheno[,grep(names(corepheno), pattern="st_abuse_max_sx[1-4]", value=T)]))
colnames(IllicitDrugs) <- c("IND_ID",
                            "co_dep_max_sx", "co_abuse_max_sx", 
                            "op_dep_max_sx", "op_abuse_max_sx", 
                            "sd_dep_max_sx", "sd_abuse_max_sx", 
                            "st_dep_max_sx", "st_abuse_max_sx")
describe(IllicitDrugs)

w2_EA<-join(IllicitDrugs, w2_EA, by="IND_ID", type="right")
describe(w2_EA)
w2_EA$IND_ID <- NULL
   #out1 <- NULL
   #out2 <- NULL
   #for ( i in 1:ncol(corepheno)){
   #   out1<-rbind(out1, names(corepheno)[i])
   #   out2<-rbind(out2, cor(w_EA$SCORE, as.numeric(corepheno[,i]), use="pairwise.complete.obs"))
   #}
   #out<-data.frame(cbind(out1,out2, labs))
   #colnames(out) <- c("name", "r", "label")
   #out<-out[order(out$r, decreasing=TRUE),]
   #table(!is.na(corepheno$age_p3))
}



###########################################
###########################################
###########################################
###########################################
w_EA_RAW <- w_EA

#standardized if n levels > 2, otherwise factor
   w_EA$SCORE<-as.numeric(scale(w_EA$SCORE))
   w_EA[,TargetPheno]<-as.numeric(scale(w_EA[,TargetPheno]))
   
   if ( length(levels(as.factor(w_EA[,WhichENV]))) >2 ){
   w_EA[,WhichENV]<-as.numeric(scale(w_EA[,WhichENV]))
   }
   if ( length(levels(as.factor(w_EA[,WhichENV]))) ==2 ){
      w_EA[,WhichENV]<-factor(w_EA[,WhichENV])
   }
   #################################################################################################################################
   ###########################################
   #set factors
   for ( i in 1:ncol(w2_EA)){
      if (length(levels(as.factor(w2_EA[,i])))==2) {
         print(colnames(w2_EA)[i])
         print(min(w2_EA[,i], na.rm=T))
         print(max(w2_EA[,i], na.rm=T))
         }}
               
   #check how many ordinals / binary
   if(FactorForBinary==TRUE){
   for ( i in 1:ncol(w2_EA)){
      if (length(levels(as.factor(w2_EA[,i])))<5) {print(names(w2_EA)[i])}
   }
   
   #set factor on binary
   for ( i in 1:ncol(w2_EA)){
   if (length(levels(as.factor(w2_EA[,i])))==2) {
      temp<-w2_EA[,i]==min(w2_EA[,i], na.rm=T)
      temp[is.na(temp)] <- FALSE
      w2_EA[temp,i] <- 0 
      temp<-w2_EA[,i]==max(w2_EA[,i], na.rm=T)
      temp[is.na(temp)] <- FALSE
      w2_EA[temp,i] <- 1
      print(names(w2_EA)[i])
      w2_EA[,i] <- factor(w2_EA[,i])
      print(table(w2_EA[,i]))
      print(levels(w2_EA[,i]))
   }}
   }
   describe(w2_EA)

   #set factor on ordinal (this doesn't work yet)
   #for ( i in 1:ncol(w2_EA)){
   #   if ( length(levels(as.factor(w2_EA[,i])))!=2 & length(levels(as.factor(w2_EA[,i])))<5 ) {
   #      print(names(w2_EA)[i])
   #      w2_EA[,i] <- ordered(w2_EA[,i])
   #      print(table(w2_EA[,i]))
   #      print(levels(w2_EA[,i]))
   #      print(w2_EA[,i])
   #   }}
      
   #################################################################################################################################
   ###########################################
   #With Trimmed cor matrix EDU
   ###########################################
   ###########################################
   summary(lm(paste0(TargetPheno, "~SCORE+PC1+PC2+PC3+PC4+PC5+PC6+PC7+PC8+PC9+PC10"), data=w_EA))
   summary(lm(paste0(TargetPheno, "~SCORE+PC1+PC2+PC3+PC4+PC5+PC6+PC7+PC8+PC9+PC10+", WhichENV), data=w_EA))

   write.table(describe(w2_EA), paste0(WhichPRS,"prs_",WhichENV, "_PCAvarsDesc.csv"), sep=",")
   
   PCASelect_Out<-PCA_Select(Pheno=w_EA[,TargetPheno], 
                                 Environment=w_EA[,WhichENV], 
                                 Confounders=w2_EA, 
                                 PCA_Method = "hetcorEigen",
                                 ParallelIterations=1000,
                                 TrimCorrMatrix=TRUE,
                                 Imputation="knn",
                                 FailSafeCorrelations = TRUE)
   PCASelect_Out$Variables
   PCASelect_Out$Recommended_N_PCs
   colnames(w2_EA)
   colnames(w2_EA)[colnames(w2_EA) %in%    PCASelect_Out$Variables ==FALSE]
   
   PC_covariates<-PCASelect_Out$PCs[,1:PCASelect_Out$Recommended_N_PCs] #follow paran
   
   write.table(data.frame(PCASelect_Out$Variables), paste0(WhichPRS,"prs_",WhichENV, "_PCAvars.csv"), sep=",")



   ###################################################### 
   #residualize on PRS
   IND_ID<-lm(PC_covariates[,1] ~ w_EA$SCORE+w_EA$IND_ID)$model[,"w_EA$IND_ID"]
   PC_covariates_RESIDUAL<-data.frame(matrix(nrow=length(IND_ID), ncol=ncol(PC_covariates)))
   for ( i in 1:PCASelect_Out$Recommended_N_PCs){
   PC_covariates_RESIDUAL[,i]<-resid(lm(PC_covariates[,i] ~ w_EA$SCORE))
   }
   PC_covariates_RESIDUAL<-data.frame(IND_ID, PC_covariates_RESIDUAL)
   temp<-data.frame(w_EA$IND_ID)
   colnames(temp)<-c("IND_ID")
   PC_covariates_RESIDUAL<-join(temp, PC_covariates_RESIDUAL, by="IND_ID", typ="left")
   colnames(PC_covariates_RESIDUAL)<-c("IND_ID", paste0("PhenoPC", 1:PCASelect_Out$Recommended_N_PCs))
   w_EA<-join(w_EA, PC_covariates_RESIDUAL, by="IND_ID")
   colnames(w_EA)

   #################################################################################################################################
######################################################
######################################################

   #n needs to be constant across models
   w_EA_RAW<-w_EA_RAW[!is.na(w_EA[,TargetPheno])&
                   !is.na(w_EA[,WhichENV])&
                   !is.na(w_EA[,"SCORE"])&
                   !is.na(w_EA[,"PC1"])&!is.na(w_EA[,"PC2"])&!is.na(w_EA[,"PC3"])&!is.na(w_EA[,"PC4"])&!is.na(w_EA[,"PC5"])&
                   !is.na(w_EA[,"PC6"])&!is.na(w_EA[,"PC7"])&!is.na(w_EA[,"PC8"])&!is.na(w_EA[,"PC9"])&!is.na(w_EA[,"PC10"]),] 
   
   
   w2_EA<-w2_EA[!is.na(w_EA[,TargetPheno])&
                   !is.na(w_EA[,WhichENV])&
                   !is.na(w_EA[,"SCORE"])&
                   !is.na(w_EA[,"PC1"])&!is.na(w_EA[,"PC2"])&!is.na(w_EA[,"PC3"])&!is.na(w_EA[,"PC4"])&!is.na(w_EA[,"PC5"])&
                   !is.na(w_EA[,"PC6"])&!is.na(w_EA[,"PC7"])&!is.na(w_EA[,"PC8"])&!is.na(w_EA[,"PC9"])&!is.na(w_EA[,"PC10"]),] 
   
   w_EA<-w_EA[!is.na(w_EA[,TargetPheno])&
                 !is.na(w_EA[,WhichENV])&
                 !is.na(w_EA[,"SCORE"])&
                 !is.na(w_EA[,"PC1"])&!is.na(w_EA[,"PC2"])&!is.na(w_EA[,"PC3"])&!is.na(w_EA[,"PC4"])&!is.na(w_EA[,"PC5"])&
                 !is.na(w_EA[,"PC6"])&!is.na(w_EA[,"PC7"])&!is.na(w_EA[,"PC8"])&!is.na(w_EA[,"PC9"])&!is.na(w_EA[,"PC10"]),] 
   table(w_EA_RAW$MAX_maxdrinks == w_EA$MAX_maxdrinks)#lined up
   
   w_EA_RAW$sex<-as.numeric(as.character(w_EA_RAW$sex))
   w_EA_RAW$sex <- (w_EA_RAW$sex-1)
   write.table(describe(w_EA_RAW[,c(TargetPheno, WhichENV, "sex", "MAX_age")]), paste0(WhichPRS,"prs_",WhichENV, "_MODELvars_RAW.csv"), sep=",")
   
   rGE<-hetcor(w_EA$SCORE, w_EA[,WhichENV])$correlations[1,2]
   save(rGE, file=paste0(WhichPRS,"prs_",WhichENV, "targetEnv_hetcor"))
   w_EA[,WhichENV]<-as.numeric(as.character(w_EA[,WhichENV]))
   
   
   #standardize again in analytic sample 
   
   #standardize variables if n levels > 2
   tempNames <- colnames(w_EA)
   for ( i in 1:ncol(w_EA)){
      if ( length(levels(as.factor(w_EA[,i]))) >2 ){
   w_EA[,i] <- as.numeric(scale(as.numeric(w_EA[,i])))
      }
   }
   colnames(w_EA) <- tempNames

   if ( length(levels(as.factor(w_EA[,WhichENV]))) ==2 ){
      w_EA[,WhichENV]<-as.numeric(as.character(w_EA[,WhichENV])) #back to numeric if was factor
   }
   write.table(describe(w_EA[,c(TargetPheno, WhichENV)]), paste0(WhichPRS,"prs_",WhichENV, "_MODELvars_SCALED.csv"), sep=",")
   
   M1_A<-summary(lm(paste0(TargetPheno, "~SCORE"), data=w_EA))
   M2_A<-summary(lm(paste0(TargetPheno, "~", WhichENV), data=w_EA))
   M3_A<-summary(lm(paste0(TargetPheno, "~SCORE+", WhichENV), data=w_EA))
   M4_A<-summary(lm( paste0(TargetPheno, "~SCORE+", WhichENV, "+",
                     paste(paste0("PhenoPC", seq(1:PCASelect_Out$Recommended_N_PCs)),  collapse="+")), data=w_EA))

   M1_B<-summary(lm(paste0(TargetPheno, "~SCORE+PC1+PC2+PC3+PC4+PC5+PC6+PC7+PC8+PC9+PC10"), data=w_EA))
   M2_B<-summary(lm(paste0(TargetPheno, "~PC1+PC2+PC3+PC4+PC5+PC6+PC7+PC8+PC9+PC10+", WhichENV), data=w_EA))
   M3_B<-summary(lm(paste0(TargetPheno, "~SCORE+PC1+PC2+PC3+PC4+PC5+PC6+PC7+PC8+PC9+PC10+", WhichENV), data=w_EA))
   M4_B<-summary(lm( paste0(TargetPheno, "~SCORE+PC1+PC2+PC3+PC4+PC5+PC6+PC7+PC8+PC9+PC10+", WhichENV, "+",
                      paste(paste0("PhenoPC", seq(1:PCASelect_Out$Recommended_N_PCs)),  collapse="+")), data=w_EA))
   
   M0_A<-summary(lm( paste0(TargetPheno, "~",
                            paste(paste0("PhenoPC", seq(1:PCASelect_Out$Recommended_N_PCs)),  collapse="+")), data=w_EA))
   M0_B<-summary(lm(paste0(TargetPheno, "~PC1+PC2+PC3+PC4+PC5+PC6+PC7+PC8+PC9+PC10"), data=w_EA))
   M0_AB <-summary(lm( paste0(TargetPheno, "~PC1+PC2+PC3+PC4+PC5+PC6+PC7+PC8+PC9+PC10+", 
                             paste(paste0("PhenoPC", seq(1:PCASelect_Out$Recommended_N_PCs)),  collapse="+")), data=w_EA))
   
   M_env1<-summary(lm( paste0(TargetPheno, "~", WhichENV, "+",
                            paste(paste0("PhenoPC", seq(1:PCASelect_Out$Recommended_N_PCs)),  collapse="+")), data=w_EA))
   
   M_env2<-summary(lm( paste0(TargetPheno, "~PC1+PC2+PC3+PC4+PC5+PC6+PC7+PC8+PC9+PC10+", WhichENV, "+",
                              paste(paste0("PhenoPC", seq(1:PCASelect_Out$Recommended_N_PCs)),  collapse="+")), data=w_EA))
   
   #################################################################################################################################
   
   #OUTPUT
   #without genetic PCs
   JustPRS<-M1_A$coef["SCORE",c("Estimate", "Std. Error")]
   JustEnv<-M2_A$coef[c(WhichENV),c("Estimate", "Std. Error")]
   PRSandEnv<-M3_A$coef[c("SCORE", WhichENV),c("Estimate", "Std. Error")]
   PRSandEnvAndPhenoPC<-M4_A$coef[c("SCORE", WhichENV),c("Estimate", "Std. Error")]

   JustPRS<-data.frame(rbind(JustPRS, JustEnv), cbind("NoCov", "No"))
   PRSandEnv<-data.frame(PRSandEnv, cbind("PRS+Env", "No"))
   PRSandEnvAndPhenoPC<-data.frame(PRSandEnvAndPhenoPC, cbind("PRS+Env+PC", "No"))
   
   colnames(JustPRS)<-c("B", "SE", "Model", "GeneticPCs")
   colnames(PRSandEnv)<-c("B", "SE", "Model", "GeneticPCs")
   colnames(PRSandEnvAndPhenoPC)<-c("B", "SE", "Model", "GeneticPCs")
   
   RESULTS<-rbind(JustPRS, PRSandEnv, PRSandEnvAndPhenoPC)
   RESULTS$Variable <- c("PRS", "Env", "PRS", "Env", "PRS", "Env")
   table(RESULTS$Variable, row.names(RESULTS))# ok 
   
   #with genetic PCs
   JustPRS<-M1_B$coef["SCORE",c("Estimate", "Std. Error")]
   JustEnv<-M2_B$coef[c(WhichENV),c("Estimate", "Std. Error")]
   PRSandEnv<-M3_B$coef[c("SCORE", WhichENV),c("Estimate", "Std. Error")]
   PRSandEnvAndPhenoPC<-M4_B$coef[c("SCORE", WhichENV),c("Estimate", "Std. Error")]
   JustPRS<-data.frame(rbind(JustPRS, JustEnv), cbind("NoCov", "Yes"))
   PRSandEnv<-data.frame(PRSandEnv, cbind("PRS+Env", "Yes"))
   PRSandEnvAndPhenoPC<-data.frame(PRSandEnvAndPhenoPC, cbind("PRS+Env+PC", "Yes"))
   
   colnames(JustPRS)<-c("B", "SE", "Model", "GeneticPCs")
   colnames(PRSandEnv)<-c("B", "SE", "Model", "GeneticPCs")
   colnames(PRSandEnvAndPhenoPC)<-c("B", "SE", "Model", "GeneticPCs")
   
   temp<-rbind(JustPRS, PRSandEnv, PRSandEnvAndPhenoPC)
   temp$Variable <- c("PRS", "Env", "PRS", "Env", "PRS", "Env")
   table(temp$Variable, row.names(temp))
   
   RESULTS<-rbind(RESULTS, temp)
   
   RESULTS$LowerCI95 <- RESULTS$B - (RESULTS$SE*1.96)
   RESULTS$UpperCI95 <- RESULTS$B + (RESULTS$SE*1.96)
   

   write.table(RESULTS, paste0(WhichPRS,"prs_", WhichENV,"_RESULTS.csv"), sep=",", row.names=FALSE)
   
   
   if (SharedAxis==FALSE){
      PlotMin<-min( c(RESULTS$LowerCI95) - 0.01)
      PlotMax<-max( c(RESULTS$UpperCI95) + 0.01)
   }
   
   if (SharedAxis==TRUE){
      PlotMin <- SharedMin
      PlotMax <- SharedMax
   }
   
   #PLOTS
   library(ggplot2)
   library(gridExtra)
   g_legend<-function(a.gplot){
      tmp <- ggplot_gtable(ggplot_build(a.gplot))
      leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
      legend <- tmp$grobs[[leg]]
      return(legend)}
   library(grid)

   # #######################3 
   plotdf<-RESULTS[RESULTS$Variable=="PRS",]
   a<-ggplot(data=plotdf, aes(x=Model, y=B,group=GeneticPCs, color=GeneticPCs)) +
      geom_line(stat="identity", size=1)+
      geom_point()+
      geom_errorbar(
         aes(ymin = LowerCI95, ymax = UpperCI95),
         width = 0.1,,
         position=position_dodge(width=0.1)) +
      ylim(PlotMin,PlotMax)+
      guides(color=guide_legend(nrow=2))+
      theme(axis.text=element_text(size=20),
            axis.title=element_text(size=20),
            legend.title = element_text(size=20),
            legend.text=element_text(size=15),
            title = element_text(size=10),
            legend.position="bottom")+
      labs(y="PRS B")
   
   plotdf<-RESULTS[RESULTS$Variable=="Env",]
   b<-ggplot(data=plotdf, aes(x=Model, y=B,group=GeneticPCs, color=GeneticPCs)) +
      geom_line(stat="identity", size=1)+
      geom_point()+
      geom_errorbar(
         aes(ymin = LowerCI95, ymax = UpperCI95),
         width = 0.1,,
         position=position_dodge(width=0.1)) +
      ylim(PlotMin,PlotMax)+
      guides(color=guide_legend(nrow=2))+
      theme(axis.text=element_text(size=20),
            axis.title=element_text(size=20),
            legend.title = element_text(size=20),
            legend.text=element_text(size=15),
            title = element_text(size=10),
            legend.position="bottom")+
      labs(y="Environment B")
   
   grid.arrange(arrangeGrob(a + theme(legend.position="none", 
                                      title = element_blank(), 
                                      axis.text=element_text(size=12))+labs(y="PRS B"),
                            b + theme(legend.position="none", 
                                      title = element_blank(), 
                                      axis.text=element_text(size=12))+labs(y="Environment B"),
                            nrow=1),
                g_legend(a), nrow=2,heights=c(10, 1),
                top = textGrob(paste0(TargetPheno, " Tar, ", WhichPRS, " PRS, ", WhichENV," Env, rGE=", round(rGE,digits=3)), 
                               gp = gpar(fontsize = 15)))

   
   png(paste0(WhichPRS,"prs_", WhichENV,"_COGA.png"),
       width = 2250, height = 2550, units = 'px', res = 300)
   grid.arrange(arrangeGrob(a + theme(legend.position="none", 
                                      title = element_blank(), 
                                      axis.text=element_text(size=12))+labs(y="PRS B"),
                            b + theme(legend.position="none", 
                                      title = element_blank(), 
                                      axis.text=element_text(size=12))+labs(y="Environment B"),
                            nrow=1),
                g_legend(a), nrow=2,heights=c(10, 1),
                top = textGrob(paste0(TargetPheno, " Tar, ", WhichPRS, " PRS, ", WhichENV," Env, rGE=", round(rGE,digits=3)), 
                               gp = gpar(fontsize = 15)))
   dev.off()
   
   plotdf<-RESULTS[RESULTS$Variable=="PRS",]
   
   if (SharedAxis==FALSE){
      PlotMin<-min( c(plotdf$LowerCI95) - 0.01)
      PlotMax<-max( c(plotdf$UpperCI95) + 0.01)
   }
   if (SharedAxis==TRUE){
      PlotMin<-( PRSLowerAxis)
      PlotMax<-( PRSUpperAxis)
   }
   d<-ggplot(data=plotdf, aes(x=Model, y=B,group=GeneticPCs, color=GeneticPCs)) +
      geom_line(stat="identity", size=1)+
      geom_point()+
      geom_errorbar(
         aes(ymin = LowerCI95, ymax = UpperCI95),
         width = 0.1,,
         position=position_dodge(width=0.1)) +
      ylim(PlotMin,PlotMax)+
      guides(color=guide_legend(nrow=2))+
      theme(axis.text=element_text(size=20),
            axis.title=element_text(size=20),
            legend.title = element_text(size=20),
            legend.text=element_text(size=15),
            title = element_text(size=10),
            legend.position="bottom")+
      labs(y="PRS B")
   
   
   png(paste0(WhichPRS,"prs_", WhichENV,"_COGA_justPRS.png"),
       width = 2250, height = 2550, units = 'px', res = 300)
   grid.arrange(arrangeGrob(d + theme(legend.position="none", 
                                      title = element_blank(), 
                                      axis.text=element_text(size=12))+labs(y="PRS B"),nrow=1),
                g_legend(a), nrow=2,heights=c(10, 1),
                top = textGrob(paste0(TargetPheno, " Tar, ", WhichPRS, " PRS, ", WhichENV," Env, rGE=", round(rGE,digits=3)), 
                               gp = gpar(fontsize = 15)))
   dev.off()
   
  ObjectsOut<- list(
      ResultsTable=RESULTS,
      NoGeneticPC_JustPRS= M1_A,
      NoGeneticPC_JustEnv= M2_A,
      NoGeneticPC_PRSandENV=M3_A,
      NoGeneticPC_PRSandENVandPhenoPCs=M4_A,
      YesGeneticPC_JustPRS= M1_B,
      YesGeneticPC_JustEnv= M2_B,
      YesGeneticPC_PRSandENV=M3_B,
      YesGeneticPC_PRSandENVandPhenoPCs=M4_B,
      JustPhenoPCs=M0_A,
      JustGeneticPCs=M0_B,
      JustPhenoAndGeneticPCs=M0_AB,
      EnvAndPhenoPCs=M_env1,
      EnvPhenoPCsAndGeneticPCs=M_env2,
      PCASelectObject= PCASelect_Out,
      DATA= w_EA[,c(TargetPheno, 
                    WhichENV, 
                    "SCORE", 
                    paste0("PhenoPC", 
                           seq(1:PCASelect_Out$Recommended_N_PCs)),
                    paste0("PC", seq(1:10)) )])
   save(ObjectsOut, file=paste0("ObjectsOut__", WhichPRS,"prs__", WhichENV))

   print("XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX")
   print("XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX")
   print("XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX")
   print("XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX")
   print("XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX")
   print("XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX")
   print("XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX")
   print(PRS_INDEX)
   print(ENVIRONMENT_INDEX)
   print(paste0("CONSOLE OUT__", WhichPRS,"prs__", WhichENV))
   flush.console()
   rm(list=setdiff(ls(), c("PRSlist", "EnvList", "TargetPheno", 
                           "readCorePheno", 
                           "SharedAxis", 
                           "SharedMin", "SharedMax",
                           "PRSLowerAxis", "PRSUpperAxis",
                           "PRS_INDEX", "ENVIRONMENT_INDEX", 
                           "FactorForBinary", "PCA_Select")))
}}
   
   

   

