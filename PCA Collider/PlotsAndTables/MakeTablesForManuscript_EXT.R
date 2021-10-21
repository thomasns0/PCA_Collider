EDU_Results<-read.table("00ResultsForManuscript/EXTprs_EDU_max_RESULTS.csv", sep=",", header=T)
EDU_PCA_Vars<-read.table("00ResultsForManuscript/EXTprs_EDU_max_PCAvarsDesc.csv", sep=",", header=T)
EDU_PCA_Vars_RETAINED<-read.table("00ResultsForManuscript/EXTprs_EDU_max_PCAvars.csv", sep=",", header=T)
EDU_PCA_MODELVARS_raw<-read.table("00ResultsForManuscript/EXTprs_EDU_max_MODELvars_RAW.csv", sep=",", header=T)
EDU_PCA_MODELVARS_scaled<-read.table("00ResultsForManuscript/EXTprs_EDU_max_MODELvars_SCALED.csv", sep=",", header=T)


EDU_PCA_Vars<-data.frame(Variable=row.names(EDU_PCA_Vars),
                         EDU_PCA_Vars[,c("n", "mean", "sd", "min", "max")])
EDU_PCA_VarsFACTORS<-EDU_PCA_Vars[grep(EDU_PCA_Vars$Variable, pattern="\\*", value=T),]
EDU_PCA_VarsNUMERIC<-EDU_PCA_Vars[(EDU_PCA_Vars$Variable%in%grep(EDU_PCA_Vars$Variable, pattern="\\*", value=T)) ==FALSE,]

freq<-EDU_PCA_VarsFACTORS$n * (EDU_PCA_VarsFACTORS$mean - 1)
prop<- ((EDU_PCA_VarsFACTORS$n * (EDU_PCA_VarsFACTORS$mean - 1)) / EDU_PCA_VarsFACTORS$n)

EDU_PCA_VarsFACTORS$mean <- freq
EDU_PCA_VarsFACTORS$sd <- prop
EDU_PCA_VarsFACTORS$min <- ""
EDU_PCA_VarsFACTORS$max <- ""

out_EDU<-rbind(EDU_PCA_VarsFACTORS, EDU_PCA_VarsNUMERIC)
colnames(out_EDU) <- c("Variable", "Total n", "Mean / n*", "SD / Proportion*", "Minimum", "Maximum")

#only keep if in PCA
nrow(EDU_PCA_Vars_RETAINED)
nrow(out_EDU)
out_EDU<-out_EDU[out_EDU$Variable %in% 
                   c(EDU_PCA_Vars_RETAINED$PCASelect_Out.Variables, paste0(EDU_PCA_Vars_RETAINED$PCASelect_Out.Variables, "*")),]

#apply labels
out_EDU$Variable[ out_EDU$Variable == "EarliestParmardisc2*"] <- "Parents enjoy eachother*"
out_EDU$Variable[ out_EDU$Variable == "EarliestParmardisc3*"] <- "Parents fight in front of offspring*"
out_EDU$Variable[ out_EDU$Variable == "EarliestParmardisc4*"] <- "Parents hit each other*"
out_EDU$Variable[ out_EDU$Variable == "sex*"] <- "Female*"
out_EDU$Variable[ out_EDU$Variable == "Earliest_pardiv.livedwith_Divorce*"] <- "Parental Divorce*"
out_EDU$Variable[ out_EDU$Variable == ".data_Boomer*"] <- "Baby Boomer Generation*"
out_EDU$Variable[ out_EDU$Variable == ".data_Millenial*"] <- "Millenial Generation*"
out_EDU$Variable[ out_EDU$Variable == ".data_GenX*"] <- "Generation X*"
out_EDU$Variable[ out_EDU$Variable == ".data_Silent*"] <- "Silent Generation (REMOVE)"
out_EDU$Variable[ out_EDU$Variable == "MAX_DM10_anyCohab.data_1*"] <- "Ever lived with someone or at least a year as though you were married*"
out_EDU$Variable[ out_EDU$Variable == "MAX_DM10_anyCohab.data_2*"] <- "Ever lived as married and ever married*"
out_EDU$Variable[ out_EDU$Variable == "MAX_mj_4dx*"] <- "Cannabis Dependence Dx*"
out_EDU$Variable[ out_EDU$Variable == "MAX_mj_4ax*"] <- "Cannabis Abuse Dx*"
out_EDU$Variable[ out_EDU$Variable == "MAX_MDD*"] <- "Major Depression Disorder Dx*"
out_EDU$Variable[ out_EDU$Variable == "MAX_cd_4dx*"] <- "Conduct Disorder Dx*"
out_EDU$Variable[ out_EDU$Variable == "MAX_asp_4dx*"] <- "Antisocial Personality Disorder Dx*"
out_EDU$Variable[ out_EDU$Variable == "co_dep_max_sx"] <- "Cocaine Dependence Sx"
out_EDU$Variable[ out_EDU$Variable == "co_abuse_max_sx"] <- "Cocaine Abuse Sx"
out_EDU$Variable[ out_EDU$Variable == "op_dep_max_sx"] <- "Opioid Dependence Sx"
out_EDU$Variable[ out_EDU$Variable == "op_abuse_max_sx"] <- "Opioid Abuse Sx"
out_EDU$Variable[ out_EDU$Variable == "st_dep_max_sx"] <- "Stimulant Dependence Sx"
out_EDU$Variable[ out_EDU$Variable == "st_abuse_max_sx"] <- "Stimulant Abuse Sx"
out_EDU$Variable[ out_EDU$Variable == "sd_dep_max_sx"] <- "Sedative Dependence Sx"
out_EDU$Variable[ out_EDU$Variable == "sd_abuse_max_sx"] <- "Sedative Abuse Sx"
out_EDU$Variable[ out_EDU$Variable == "EarliestParmardisc1"] <- "Parental Relationship Quality"
out_EDU$Variable[ out_EDU$Variable == "EarliestParmardisc5"] <- "Conflict in Household"
out_EDU$Variable[ out_EDU$Variable == "MAX_maxdrinks"] <- "Max Drinks in 24 Hrs"
out_EDU$Variable[ out_EDU$Variable == "EARLIEST_EXTAgeInit"] <- "Age of EXTohol Initiation"
out_EDU$Variable[ out_EDU$Variable == "EARLIEST_EXTAgeIntox"] <- "Age of First EXTohol Intoxication"
out_EDU$Variable[ out_EDU$Variable == "MAX_age"] <- "Maximum Recorded Age"
out_EDU$Variable[ out_EDU$Variable == "EARLIEST_EXTAgeInit_maternal"] <- "Maternal Age of EXTohol Initiation"
out_EDU$Variable[ out_EDU$Variable == "EARLIEST_EXTAgeInit_paternal"] <- "Paternal Age of EXTohol Initiation"
out_EDU$Variable[ out_EDU$Variable == "EARLIEST_EXTAgeIntox_maternal"] <- "Maternal Age of First EXTohol Intoxication"
out_EDU$Variable[ out_EDU$Variable == "EARLIEST_EXTAgeIntox_paternal"] <- "Paternal Age of First EXTohol Intoxication"
out_EDU$Variable[ out_EDU$Variable == "MAX_maxdrinks_maternal"] <- "Maternal Max Drinks in 24 Hrs"
out_EDU$Variable[ out_EDU$Variable == "MAX_maxdrinks_paternal"] <- "Paternal Max Drinks in 24 Hrs"
out_EDU$Variable[ out_EDU$Variable == "MAX_aud_5sx_maternal"] <- "Maternal AUD Sx"
out_EDU$Variable[ out_EDU$Variable == "MAX_aud_5sx_paternal"] <- "Paternal AUD Sx"
write.table(out_EDU, "extEDU_Table_For_Manuscript.csv", sep=",", row.names=FALSE)

FTND_PCA_MODELVARS_raw
FTND_PCA_MODELVARS_scaled


#############################################
FTND_Results<-read.table("00ResultsForManuscript/EXTprs_MAX_ftnd_sx_RESULTS.csv", sep=",", header=T)
FTND_PCA_Vars<-read.table("00ResultsForManuscript/EXTprs_MAX_ftnd_sx_PCAvarsDesc.csv", sep=",", header=T)
FTND_PCA_Vars_RETAINED<-read.table("00ResultsForManuscript/EXTprs_MAX_ftnd_sx_PCAvars.csv", sep=",", header=T)
FTND_PCA_MODELVARS_raw<-read.table("00ResultsForManuscript/EXTprs_MAX_ftnd_sx_MODELvars_RAW.csv", sep=",", header=T)
FTND_PCA_MODELVARS_scaled<-read.table("00ResultsForManuscript/EXTprs_MAX_ftnd_sx_MODELvars_SCALED.csv", sep=",", header=T)

FTND_PCA_Vars<-data.frame(Variable=row.names(FTND_PCA_Vars),
                          FTND_PCA_Vars[,c("n", "mean", "sd", "min", "max")])
FTND_PCA_VarsFACTORS<-FTND_PCA_Vars[grep(FTND_PCA_Vars$Variable, pattern="\\*", value=T),]
FTND_PCA_VarsNUMERIC<-FTND_PCA_Vars[(FTND_PCA_Vars$Variable%in%grep(FTND_PCA_Vars$Variable, pattern="\\*", value=T)) ==FALSE,]

freq<-FTND_PCA_VarsFACTORS$n * (FTND_PCA_VarsFACTORS$mean - 1)
prop<- ((FTND_PCA_VarsFACTORS$n * (FTND_PCA_VarsFACTORS$mean - 1)) / FTND_PCA_VarsFACTORS$n)

FTND_PCA_VarsFACTORS$mean <- freq
FTND_PCA_VarsFACTORS$sd <- prop
FTND_PCA_VarsFACTORS$min <- ""
FTND_PCA_VarsFACTORS$max <- ""

out_FTND<-rbind(FTND_PCA_VarsFACTORS, FTND_PCA_VarsNUMERIC)
colnames(out_FTND) <- c("Variable", "Total n", "Mean / n", "SD / Proportion", "Minimum", "Maximum")

#only keep if in PCA
nrow(FTND_PCA_Vars_RETAINED)
nrow(out_FTND)
out_FTND<-out_FTND[out_FTND$Variable %in% 
                     c(FTND_PCA_Vars_RETAINED$PCASelect_Out.Variables, paste0(FTND_PCA_Vars_RETAINED$PCASelect_Out.Variables, "*")),]

#apply labels
out_FTND$Variable[ out_FTND$Variable == "EarliestParmardisc2*"] <- "Parents enjoy eachother*"
out_FTND$Variable[ out_FTND$Variable == "EarliestParmardisc3*"] <- "Parents fight in front of offspring*"
out_FTND$Variable[ out_FTND$Variable == "EarliestParmardisc4*"] <- "Parents hit each other*"
out_FTND$Variable[ out_FTND$Variable == "sex*"] <- "Female*"
out_FTND$Variable[ out_FTND$Variable == "Earliest_pardiv.livedwith_Divorce*"] <- "Parental Divorce*"
out_FTND$Variable[ out_FTND$Variable == ".data_Boomer*"] <- "Baby Boomer Generation*"
out_FTND$Variable[ out_FTND$Variable == ".data_GenX*"] <- "Generation X*"
out_FTND$Variable[ out_FTND$Variable == ".data_Millenial*"] <- "Millenial Generation*"
out_FTND$Variable[ out_FTND$Variable == ".data_Silent*"] <- "Silent Generation (REMOVE)"
out_FTND$Variable[ out_FTND$Variable == "MAX_DM10_anyCohab.data_1*"] <- "Ever lived with someone or at least a year as though you were married*"
out_FTND$Variable[ out_FTND$Variable == "MAX_DM10_anyCohab.data_2*"] <- "Ever lived as married and ever married*"
out_FTND$Variable[ out_FTND$Variable == "MAX_mj_4dx*"] <- "Cannabis Dependence Dx*"
out_FTND$Variable[ out_FTND$Variable == "MAX_mj_4ax*"] <- "Cannabis Abuse Dx*"
out_FTND$Variable[ out_FTND$Variable == "MAX_MDD*"] <- "Major Depression Disorder Dx*"
out_FTND$Variable[ out_FTND$Variable == "MAX_cd_4dx*"] <- "Conduct Disorder Dx*"
out_FTND$Variable[ out_FTND$Variable == "MAX_asp_4dx*"] <- "Antisocial Personality Disorder Dx*"
out_FTND$Variable[ out_FTND$Variable == "co_dep_max_sx"] <- "Cocaine Dependence Sx"
out_FTND$Variable[ out_FTND$Variable == "co_abuse_max_sx"] <- "Cocaine Abuse Sx"
out_FTND$Variable[ out_FTND$Variable == "op_dep_max_sx"] <- "Opioid Dependence Sx"
out_FTND$Variable[ out_FTND$Variable == "op_abuse_max_sx"] <- "Opioid Abuse Sx"
out_FTND$Variable[ out_FTND$Variable == "st_dep_max_sx"] <- "Stimulant Dependence Sx"
out_FTND$Variable[ out_FTND$Variable == "st_abuse_max_sx"] <- "Stimulant Abuse Sx"
out_FTND$Variable[ out_FTND$Variable == "sd_dep_max_sx"] <- "Sedative Dependence Sx"
out_FTND$Variable[ out_FTND$Variable == "sd_abuse_max_sx"] <- "Sedative Abuse Sx"
out_FTND$Variable[ out_FTND$Variable == "EarliestParmardisc1"] <- "Parental Relationship Quality"
out_FTND$Variable[ out_FTND$Variable == "EarliestParmardisc5"] <- "Conflict in Household"
out_FTND$Variable[ out_FTND$Variable == "MAX_maxdrinks"] <- "Max Drinks in 24 Hrs"
out_FTND$Variable[ out_FTND$Variable == "EARLIEST_EXTAgeInit"] <- "Age of EXTohol Initiation"
out_FTND$Variable[ out_FTND$Variable == "EARLIEST_EXTAgeIntox"] <- "Age of First EXTohol Intoxication"
out_FTND$Variable[ out_FTND$Variable == "MAX_age"] <- "Maximum Recorded Age"
out_FTND$Variable[ out_FTND$Variable == "EARLIEST_EXTAgeInit_maternal"] <- "Maternal Age of EXTohol Initiation"
out_FTND$Variable[ out_FTND$Variable == "EARLIEST_EXTAgeInit_paternal"] <- "Paternal Age of EXTohol Initiation"
out_FTND$Variable[ out_FTND$Variable == "EARLIEST_EXTAgeIntox_maternal"] <- "Maternal Age of First EXTohol Intoxication"
out_FTND$Variable[ out_FTND$Variable == "EARLIEST_EXTAgeIntox_paternal"] <- "Paternal Age of First EXTohol Intoxication"
out_FTND$Variable[ out_FTND$Variable == "MAX_maxdrinks_maternal"] <- "Maternal Max Drinks in 24 Hrs"
out_FTND$Variable[ out_FTND$Variable == "MAX_maxdrinks_paternal"] <- "Paternal Max Drinks in 24 Hrs"
out_FTND$Variable[ out_FTND$Variable == "MAX_aud_5sx_maternal"] <- "Maternal AUD Sx"
out_FTND$Variable[ out_FTND$Variable == "MAX_aud_5sx_paternal"] <- "Paternal AUD Sx"
write.table(out_FTND, "extFTND_Table_For_Manuscript.csv", sep=",", row.names=FALSE)

############################################################################
#RESULTS
source("/Users/thomasns/Documents/0home/CuttingRoomFloor.R")
RESULTS <- EDU_Results
EnvironmentName <- "EDU"

RESULTS$Variable[RESULTS$Variable!="PRS"] <- EnvironmentName

MakeResultsTable<-function(RESULTS,EnvironmentName){
  #temp objects for table structure
  UNIVARIATE_MODEL<-data.frame(matrix(nrow=1,ncol=10, rep("Univariate", 10)))
  colnames(UNIVARIATE_MODEL) <-(rep(c("Variable", "B", "SE", "LowerCI95", "UpperCI95"),2))
  
  UNCORRECTED_MODEL<-data.frame(matrix(nrow=1,ncol=10, rep("Uncorrected", 10)))
  colnames(UNCORRECTED_MODEL) <-(rep(c("Variable", "B", "SE", "LowerCI95", "UpperCI95"),2))
  
  CORRECTED_MODEL<-data.frame(matrix(nrow=1,ncol=10, rep("Corrected", 10)))
  colnames(CORRECTED_MODEL) <-(rep(c("Variable", "B", "SE", "LowerCI95", "UpperCI95"),2))
  
  GENETIC_PC<-data.frame(matrix(nrow=1,ncol=10, c(rep("Without Genetic PCs", 5), rep("With Genetic PCs", 5))))
  colnames(GENETIC_PC) <- (rep(c("Variable", "B", "SE", "LowerCI95", "UpperCI95"),2))
  
  out<-rbind(
    UNIVARIATE_MODEL,
    GENETIC_PC,
    cbind(
      RESULTS[RESULTS$GeneticPCs=="No"&RESULTS$Model=="NoCov", c("Variable", "B", "SE", "LowerCI95", "UpperCI95")],
      RESULTS[RESULTS$GeneticPCs=="Yes"&RESULTS$Model=="NoCov",c("Variable", "B", "SE", "LowerCI95", "UpperCI95")]),
    
    UNCORRECTED_MODEL,
    GENETIC_PC,
    cbind(
      RESULTS[RESULTS$GeneticPCs=="No"&RESULTS$Model=="PRS+Env", c("Variable", "B", "SE", "LowerCI95", "UpperCI95")],
      RESULTS[RESULTS$GeneticPCs=="Yes"&RESULTS$Model=="PRS+Env",c("Variable", "B", "SE", "LowerCI95", "UpperCI95")]),
    
    CORRECTED_MODEL,
    GENETIC_PC,
    cbind(
      RESULTS[RESULTS$GeneticPCs=="No"&RESULTS$Model=="PRS+Env+PC", c("Variable", "B", "SE", "LowerCI95", "UpperCI95")],
      RESULTS[RESULTS$GeneticPCs=="Yes"&RESULTS$Model=="PRS+Env+PC",c("Variable", "B", "SE", "LowerCI95", "UpperCI95")]))
  return(out)}

write.table(MakeResultsTable(FTND_Results, "TOB"), "extTOB_ResultsTable.csv", sep=",", row.names=FALSE)
write.table(MakeResultsTable(EDU_Results, "EDU"), "extEDU_ResultsTable.csv", sep=",", row.names=FALSE)

########### 
########### 
########### 
########### 
########### 
########### #VARIANCE ACCOUNTED FOR
#NoGeneticPC_JustPRS= M1_A,
#NoGeneticPC_JustEnv= M2_A,
#NoGeneticPC_PRSandENV=M3_A,
#NoGeneticPC_PRSandENVandPhenoPCs=M4_A,
#YesGeneticPC_JustPRS= M1_B,
#YesGeneticPC_JustEnv= M2_B,
#YesGeneticPC_PRSandENV=M3_B,
#YesGeneticPC_PRSandENVandPhenoPCs=M4_B,
#JustPhenoPCs=M0_A,
#JustGeneticPCs=M0_B,
#JustPhenoAndGeneticPCs=M0_AB,
#EnvAndPhenoPCs=M_env1,
#EnvPhenoPCsAndGeneticPCs=M_env2,
R2_TABLE<-function(PCAOUT, EnvironmentName){
  out2<-cbind(
    #LABELS
    c(
      paste0("~  "),
      paste0("~ ", EnvironmentName), #ENV
      paste0("~ ", EnvironmentName, " + Phenotypic PCs") # ENV + PHENOPC
    ),
    #BASE MODELS
    c(
      0, #PRS
      PCAOUT$NoGeneticPC_JustEnv$r.squared, #ENV
      PCAOUT$EnvAndPhenoPCs$r.squared  # ENV + PHENOPC
    ),
    #CHANGE R2s with PRS
    c(
      PCAOUT$NoGeneticPC_JustPRS$r.squared, #Just PRS
      PCAOUT$NoGeneticPC_PRSandENV$r.squared - PCAOUT$NoGeneticPC_JustEnv$r.squared, #(PRS+ENV) - ENV
      PCAOUT$NoGeneticPC_PRSandENVandPhenoPCs$r.squared - PCAOUT$EnvAndPhenoPCs$r.squared  # (PRS + ENV + PHENOPC) - (ENV + PHENOPC)
    )
  )
  out3<-cbind(
    #LABELS
    c(
      paste0("~ Genetic PCs "),
      paste0("~ Genetic PCs + ", EnvironmentName), #ENV
      paste0("~ Genetic PCs + ", EnvironmentName, " + Phenotypic PCs") # ENV + PHENOPC
    ),
    #BASE MODELS
    c(
      PCAOUT$JustGeneticPCs$r.squared, #G PC
      PCAOUT$YesGeneticPC_JustEnv$r.squared, #G PC + ENV
      PCAOUT$EnvPhenoPCsAndGeneticPCs$r.squared  # G PC + ENV + PHENOPC
    ),
    #CHANGE R2s with PRS
    c(
      PCAOUT$YesGeneticPC_JustPRS$r.squared - PCAOUT$JustGeneticPCs$r.squared, #(PRS + GPC) - GPC
      PCAOUT$YesGeneticPC_PRSandENV$r.squared - PCAOUT$YesGeneticPC_JustEnv$r.squared, # (PRS + GPC + ENV) - (GPS +ENV)
      PCAOUT$YesGeneticPC_PRSandENVandPhenoPCs$r.squared - PCAOUT$EnvPhenoPCsAndGeneticPCs$r.squared  # (PRS + ENV + PHENOPC + GPC) - (ENV + PHENOPC + GPC)
    )
  )
  out2<-data.frame(out2)
  out3<-data.frame(out3)
  colnames(out2) <- c("Base Model", "Base Model R-squared", "Change R-squared with PRS")
  colnames(out3) <- c("Base Model", "Base Model R-squared", "Change R-squared with PRS")
  #out2[,"Total R-squared with PRS"]<- as.numeric(out2[,"Base Model R-squared"]) + as.numeric(out2[,"Change R-squared with PRS"])
  return(list(out2,out3))
}


load("00ResultsForManuscript/ObjectsOut__EXTprs__EDU_max")
ObjectsOut_EDU <- ObjectsOut
temp<-R2_TABLE(ObjectsOut_EDU, "EDU")
write.table(rbind(temp[[1]], temp[[2]]), "extR2_Table_EDU.csv", sep=",", row.names=FALSE)



load("00ResultsForManuscript/ObjectsOut__EXTprs__MAX_ftnd_sx")
ObjectsOutFTND <- ObjectsOut
temp<-R2_TABLE(ObjectsOutFTND, "TOB")
write.table(rbind(temp[[1]], temp[[2]]), "extR2_Table_TOB.csv", sep=",", row.names=FALSE)


#rGE 
load("00ResultsForManuscript/EXTprs_EDU_maxtargetEnv_hetcor")

load("00ResultsForManuscript/EXTprs_MAX_ftnd_sxtargetEnv_hetcor")





