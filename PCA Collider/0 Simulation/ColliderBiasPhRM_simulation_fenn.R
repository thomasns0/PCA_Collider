
directory <-"/vcu_gpfs2/home/thomasns/ColliderBiasPhRM"
scriptNames<-NULL
OutputNames<-NULL

for ( i in 1:50){
  Rscript1<-paste0(
    '
    set.seed(',paste0(i),'*1000+12345)
    options(stringsAsFactors = T)

complement <- function(y, rho, x) {
  if (missing(x)) x <- rnorm(length(y)) # Optional: supply a default if `x` is not given
  y.perp <- residuals(lm(x ~ y))
  rho * sd(y.perp) * y + y.perp * sd(y) * sqrt(1 - rho^2)
}
library(mvtnorm)
library(psych)
library(lme4qtl)
PhRMSimulation<-function(UnObsToEnv, UnObsToPheno, EnvToPheno, PRStoPheno, PRStoEnv, propObserved, UnObsMaxCor, UnObsMinCor){
#create unobserved data, 100 variables and decompose
V<-matrix(ncol=100, nrow=100, round(runif(n=100^2, min=UnObsMinCor, max=UnObsMaxCor), digits=1)) #highish correlations here bc need V * t(V)
V[upper.tri(V)] <- V[lower.tri(V)] 
diag(V) <- 1
V <- V * t(V) #force matrix to symmetric
V
Unobserved<-rmvnorm(n=1000, mean=rep(0, nrow(V)), sigma = V)
#PCA of unobserved data
#EigenUnobserved<-eigen(cor((Unobserved)))
PC_Unobserved<-prcomp(Unobserved, retx=TRUE, center=TRUE, scale=TRUE)
#scaling <- PC_Unobserved$sdev[1:2] * sqrt(nrow(Unobserved))
#pc1_Unobserved <- as.numeric(scale(rowSums(t(t(sweep(Unobserved, 2 ,colMeans(Unobserved))) * EigenUnobserved$vectors[,1] * -1) / scaling[1])))
#pc2_Unobserved <- as.numeric(scale(rowSums(t(t(sweep(Unobserved, 2, colMeans(Unobserved))) * EigenUnobserved$vectors[,2]) / scaling[2])))

pc1_Unobserved <- PC_Unobserved$x[,1]
pc2_Unobserved <- PC_Unobserved$x[,2]

#PRS 
PRS <-rnorm(n=1000, 
           mean= 0, 
           sd=1)
#simulate phenotype that is highly correlated with first PC of the unobserved data
Environment <- rnorm(n=1000, 
                     mean= ((pc1_Unobserved * UnObsToEnv) + (PRS*PRStoEnv)), 
                     sd=sqrt(1-(UnObsToEnv^2) - (PRStoEnv^2)))
#Pheno is a function of Unobserved, Enivironment, and PRS
Pheno <-rnorm(n=1000, 
              mean=((pc1_Unobserved * UnObsToPheno) + (Environment * EnvToPheno) + (PRS * PRStoPheno)), 
              sd=sqrt(1-(UnObsToPheno^2)-(EnvToPheno^2)-(PRStoPheno^2)))
#data objects
Unobserved<-data.frame(Unobserved)
ID <- seq(1:length(Pheno))
df<-data.frame(ID, Pheno, Environment, PRS, pc1_Unobserved, pc2_Unobserved)
#generate proportion of unobserved confounders that are available
Observed<-Unobserved[,sample(1:100, size=ncol(Unobserved)*propObserved, replace=FALSE)]
PhRM<-cor(t(Observed))
colnames(PhRM) <- df$ID
rownames(PhRM) <- df$ID
#PCs from available subset
#EigenObserved<-eigen(cor((Observed)))
PC_Observed<-prcomp(Observed, retx=TRUE, center=TRUE, scale=TRUE)
#scaling <- PC_Observed$sdev[1:2] * sqrt(nrow(Observed))
#pc1_Observed <- as.numeric(scale(rowSums(t(t(sweep(Observed, 2 ,colMeans(Observed))) * EigenObserved$vectors[,1] * -1) / scaling[1])))
#pc2_Observed <- as.numeric(scale(rowSums(t(t(sweep(Observed, 2, colMeans(Observed))) * EigenObserved$vectors[,2]) / scaling[2])))
pc1_Observed <- PC_Observed$x[,1]
pc2_Observed <- PC_Observed$x[,2]

df<-data.frame(df, pc1_Observed, pc2_Observed)
#cor(df)
Mod_PhenPRS<-lm(Pheno ~ PRS, data=df)
Mod_PhenoPrsEnv<-lm(Pheno ~ PRS + Environment, data=df)
Mod_PhenoPrsEnvUnobsPC<-lm(Pheno ~ PRS + Environment + pc1_Unobserved, data=df)
Mod_PhenoPrsEnvObsPC<-lm(Pheno ~ PRS + Environment + pc1_Observed, data=df)
PhenPRS<-summary(Mod_PhenPRS)$coef["PRS",c("Estimate", "Pr(>|t|)")]
PhenoPrsEnv<-summary(Mod_PhenoPrsEnv)$coef["PRS",c("Estimate", "Pr(>|t|)")]
PhenPrsEnvUnobsPC<-summary(Mod_PhenoPrsEnvUnobsPC)$coef["PRS",c("Estimate", "Pr(>|t|)")]
PhenPrsEnvObsPC<-summary(Mod_PhenoPrsEnvObsPC)$coef["PRS",c("Estimate", "Pr(>|t|)")]
Estimates<-data.frame(cbind(
                 c(PhenPRS[1], PhenoPrsEnv[1], PhenPrsEnvUnobsPC[1], PhenPrsEnvObsPC[1]), 
                 c("~PRS", "~Prs+Env", "~Prs+Env+UnobsPC", "~Prs+Env+ObsPC"),
                 c(propObserved,propObserved,propObserved,propObserved),
                 c(PRStoEnv, PRStoEnv, PRStoEnv, PRStoEnv),
                 c(UnObsMinCor, UnObsMinCor, UnObsMinCor, UnObsMinCor),
                 c(UnObsMaxCor, UnObsMaxCor, UnObsMaxCor, UnObsMaxCor)))
colnames(Estimates)<-c("B", "Model", "ProportionObserved", "PRStoEnv", "UnObsMinCor", "UnObsMaxCor")
rownames(Estimates)<-seq(1:4)
Pvalues<-data.frame(cbind(c(PhenPRS[2], PhenoPrsEnv[2], PhenPrsEnvUnobsPC[2], PhenPrsEnvObsPC[2]), 
                          c("~PRS", "~Prs+Env", "~Prs+Env+UnobsPC", "~Prs+Env+ObsPC"),
                          c(propObserved,propObserved,propObserved,propObserved),
                          c(PRStoEnv, PRStoEnv, PRStoEnv, PRStoEnv),
                          c(UnObsMinCor, UnObsMinCor, UnObsMinCor, UnObsMinCor),
                          c(UnObsMaxCor, UnObsMaxCor, UnObsMaxCor, UnObsMaxCor)))
colnames(Pvalues)<-c("B", "Model", "ProportionObserved", "PRStoEnv", "UnObsMinCor", "UnObsMaxCor")
rownames(Pvalues)<-seq(1:4)
return(list(Estimates,Pvalues))
}

#Path from Unobserved PC1 to Environment
UnObsToEnv<-0.5
#Path from Unobserved to Pheno
UnObsToPheno<-0.5
#Path from Environment to Pheno
EnvToPheno<-0.01
#Path from PRS to Pheno
PRStoPheno<-sqrt(0.01)
#Correlation of PRS with Environment
rGE_range<-seq(from=0, to=0.5, by= 0.1)
#unobserved data correlations
UnObsCor <-data.frame(matrix(nrow=4,ncol=2))
colnames(UnObsCor)<-c("UnObsMinCor", "UnObsMaxCor")
UnObsCor[1,]<-c(0.05, 0.1)
UnObsCor[2,]<-c(0.2, 0.3)
UnObsCor[3,]<-c(0.5, 0.6)
UnObsCor[4,]<-c(0.8, 0.9)
#proportion of unobserved data that is available for PhRM
PropObserved_range <- seq(from=0.1, to=1.0, by= 0.01)
#run 
outB<- NULL
outP<- NULL
NumberofSimulationReps <- 25
zzzz<-1
repeat {
  ptm <- proc.time()
  print(zzzz)
for ( i in 1:nrow(UnObsCor)){
UnObsMinCor<-UnObsCor[i,1]
UnObsMaxCor<-UnObsCor[i,2]
for (PRStoEnv in rGE_range){
for (propObserved in PropObserved_range){
out<-PhRMSimulation(UnObsToEnv, UnObsToPheno, EnvToPheno, PRStoPheno, PRStoEnv, propObserved, UnObsMaxCor, UnObsMinCor)
outB<-rbind(outB, out[[1]])
outP<-rbind(outP, out[[2]])
}
}
}
   zzzz = zzzz+1
  if (zzzz == (NumberofSimulationReps+1)){
    break
  }
}
proc.time() - ptm
ALLoutB<-outB
ALLoutP<-outP
outputfile<-ALLoutB'
  )
  
  Rscript2<-
    paste0(
      "
  write.table(outputfile,", paste0('"',"/vcu_gpfs2/home/thomasns/ColliderBiasPhRM/ALLoutB", "_", i,".csv", '"'), ",sep=',', col.names = FALSE, row.names = FALSE, quote = FALSE)
  "
    )
  
  Rscript<-paste0(Rscript1, Rscript2)
  
  write.table(Rscript, paste0('/vcu_gpfs2/home/thomasns/ColliderBiasPhRM/ALLoutB',"_", i, '.R'), sep=",", col.names = FALSE, row.names = FALSE, quote = FALSE)
  scriptNames<-rbind(scriptNames,paste0(directory, '/ALLoutB',"_", i, '.R'))
  OutputNames<-rbind(OutputNames, paste0(directory, '/ALLoutB',"_", i, '.csv'))
  
}


for ( i in 1:length(scriptNames)){
  startSH<- paste0(
    '#!/usr/bin/env bash
######################################################
#  execute script in current directory
#$ -cwd
#  want any .e/.o stuff to show up here too
#$ -e ./
#$ -o ./
#  shell for qsub to use:
#$ -S /bin/bash
#  name for the job; used by qstat
#$ -N ',paste0('ALLoutB', i,"_"),'

# examples which are not enabled by default:
# only run on a node with given minimum memory free

##  #$ -l mem_free=9G

# submit an smp job which needs 4 slots

##  #$ -pe smp 4

######################################################



R_SCRIPT=',paste0(scriptNames[i]),'

RVER="3.6"

source /usr/global/R-${RVER}/R_env.sh

START=$(date +%s)
NSTART=$(date +%s.%N)

java -version
javac -version
R --version

# $1 is the first paramater, if you want the others add $2 $3 etc

R CMD BATCH ${R_SCRIPT} $1 > Rout 2> Rerr'
  )
  script <- paste0(directory, '/ALLoutB',"_", i, '.sh')
  write.table(startSH, file = script, col.names = FALSE, row.names = FALSE, quote = FALSE)
  runJob <- paste0('qsub ', script)
  system(runJob)
}
