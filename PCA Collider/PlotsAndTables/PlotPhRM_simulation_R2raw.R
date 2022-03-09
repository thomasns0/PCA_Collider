library(ggplot2)
options(scipen = 999)
w<-read.table('CombinedOutputALLoutR2raw.csv', sep=",", header=FALSE)

colnames(w)<-c("R2", "Model", "ProportionObserved", "PRStoEnv", "UnObsMinCor", "UnObsMaxCor")
head(w)

###################################################################################
###################################################################################
###################################################################################
#Parameters in simulation
#Correlation of PRS with Environment
rGE_range<-round(seq(from=0, to=0.5, by= 0.1), digits=1)

#unobserved data correlations
UnObsCor <-data.frame(matrix(nrow=4,ncol=2))
colnames(UnObsCor)<-c("UnObsMinCor", "UnObsMaxCor")
UnObsCor[1,]<-c(0.05, 0.1)
UnObsCor[2,]<-c(0.2, 0.3)
UnObsCor[3,]<-c(0.5, 0.6)
UnObsCor[4,]<-c(0.8, 0.9)
#unique combinations
SUBSETS<-expand.grid(rGE_range,UnObsCor[,1])
colnames(SUBSETS)<-c("rGE", "UnObsMinCor")
nrow(w)/24

#rename models
class(w$Model)
unique(w$Model)
w$Model<-factor(w$Model, levels=c("JustPRS", "R2_Env", "R2_PrsEnv", "R2_PrsEnvUnobsPC","R2_EnvUnobsPC","R2_PrsEnvObsPC", "R2_EnvObsPC"))
levels(w$Model)
#w$Model<-as.character(w$Model)

nrow(w)/ #number of rows
(length(unique(w$PRStoEnv)) * #6 rGE
  length(unique(w$ProportionObserved)) * #91 proportion observed
  length(unique(w$UnObsMinCor)) * # 4 correlation structures
  length(unique(w$Model))) #4 models
#1250 reps for each permutation

lowRGE_lowConfoundCor<-w[w$PRStoEnv==0.1&w$UnObsMinCor==0.2,]
lowRGE_highConfoundCor<-w[w$PRStoEnv==0.1&w$UnObsMinCor==0.5,]
highRGE_lowConfoundCor<-w[w$PRStoEnv==0.5&w$UnObsMinCor==0.2,]
highRGE_highConfoundCor<-w[w$PRStoEnv==0.5&w$UnObsMinCor==0.5,]

lowRGE_highestConfoundCor<-w[w$PRStoEnv==0.1&w$UnObsMinCor==0.8,]
highRGE_highestConfoundCor<-w[w$PRStoEnv==0.5&w$UnObsMinCor==0.8,]

lowRGE_lowestConfoundCor<-w[w$PRStoEnv==0.1&w$UnObsMinCor==0.05,]
highRGE_lowestConfoundCor<-w[w$PRStoEnv==0.5&w$UnObsMinCor==0.05,]

#calculate % change for R&R

R2Change<-function(subsetdf){
  
  PRS_10.25<-subsetdf[subsetdf$Model=="JustPRS"& (subsetdf$ProportionObserved>=0.10&subsetdf$ProportionObserved<=0.25),]
  PRS_10.25<-PRS_10.25[order(row.names(PRS_10.25)),]
  PRS_25.50<-subsetdf[subsetdf$Model=="JustPRS"& (subsetdf$ProportionObserved>0.25&subsetdf$ProportionObserved<=0.50),]
  PRS_25.50<-PRS_25.50[order(row.names(PRS_25.50)),]
  PRS_50.75<-subsetdf[subsetdf$Model=="JustPRS"& (subsetdf$ProportionObserved>0.50&subsetdf$ProportionObserved<=0.75),]
  PRS_50.75<-PRS_50.75[order(row.names(PRS_50.75)),]
  PRS_75.99<-subsetdf[subsetdf$Model=="JustPRS"& (subsetdf$ProportionObserved>0.75&subsetdf$ProportionObserved<=0.99),]
  PRS_75.99<-PRS_75.99[order(row.names(PRS_75.99)),]
  
  ENV_10.25<-subsetdf[subsetdf$Model=="R2_Env"& (subsetdf$ProportionObserved>=0.10&subsetdf$ProportionObserved<=0.25),]
  ENV_10.25<-ENV_10.25[order(row.names(ENV_10.25)),]
  ENV_25.50<-subsetdf[subsetdf$Model=="R2_Env"& (subsetdf$ProportionObserved>0.25&subsetdf$ProportionObserved<=0.50),]
  ENV_25.50<-ENV_25.50[order(row.names(ENV_25.50)),]
  ENV_50.75<-subsetdf[subsetdf$Model=="R2_Env"& (subsetdf$ProportionObserved>0.50&subsetdf$ProportionObserved<=0.75),]
  ENV_50.75<-ENV_50.75[order(row.names(ENV_50.75)),]
  ENV_75.99<-subsetdf[subsetdf$Model=="R2_Env"& (subsetdf$ProportionObserved>0.75&subsetdf$ProportionObserved<=0.99),]
  ENV_75.99<-ENV_75.99[order(row.names(ENV_75.99)),]
  
  PRSENV_10.25<-subsetdf[subsetdf$Model=="R2_PrsEnv"& (subsetdf$ProportionObserved>=0.10&subsetdf$ProportionObserved<=0.25),]
  PRSENV_10.25<-PRSENV_10.25[order(row.names(PRSENV_10.25)),]
  PRSENV_25.50<-subsetdf[subsetdf$Model=="R2_PrsEnv"& (subsetdf$ProportionObserved>0.25&subsetdf$ProportionObserved<=0.50),]
  PRSENV_25.50<-PRSENV_25.50[order(row.names(PRSENV_25.50)),]
  PRSENV_50.75<-subsetdf[subsetdf$Model=="R2_PrsEnv"& (subsetdf$ProportionObserved>0.50&subsetdf$ProportionObserved<=0.75),]
  PRSENV_50.75<-PRSENV_50.75[order(row.names(PRSENV_50.75)),]
  PRSENV_75.99<-subsetdf[subsetdf$Model=="R2_PrsEnv"& (subsetdf$ProportionObserved>0.75&subsetdf$ProportionObserved<=0.99),]
  PRSENV_75.99<-PRSENV_75.99[order(row.names(PRSENV_75.99)),]

  ENVINCPC_10.25<-subsetdf[subsetdf$Model=="R2_EnvObsPC"& (subsetdf$ProportionObserved>=0.10&subsetdf$ProportionObserved<=0.25),]
  ENVINCPC_10.25<-ENVINCPC_10.25[order(row.names(ENVINCPC_10.25)),]
  ENVINCPC_25.50<-subsetdf[subsetdf$Model=="R2_EnvObsPC"& (subsetdf$ProportionObserved>0.25&subsetdf$ProportionObserved<=0.50),]
  ENVINCPC_25.50<-ENVINCPC_25.50[order(row.names(ENVINCPC_25.50)),]
  ENVINCPC_50.75<-subsetdf[subsetdf$Model=="R2_EnvObsPC"& (subsetdf$ProportionObserved>0.50&subsetdf$ProportionObserved<=0.75),]
  ENVINCPC_50.75<-ENVINCPC_50.75[order(row.names(ENVINCPC_50.75)),]
  ENVINCPC_75.99<-subsetdf[subsetdf$Model=="R2_EnvObsPC"& (subsetdf$ProportionObserved>0.75&subsetdf$ProportionObserved<=0.99),]
  ENVINCPC_75.99<-ENVINCPC_75.99[order(row.names(ENVINCPC_75.99)),]
  
  PRSENVINCPC_10.25<-subsetdf[subsetdf$Model=="R2_PrsEnvObsPC"& (subsetdf$ProportionObserved>=0.10&subsetdf$ProportionObserved<=0.25),]
  PRSENVINCPC_10.25<-PRSENVINCPC_10.25[order(row.names(PRSENVINCPC_10.25)),]
  PRSENVINCPC_25.50<-subsetdf[subsetdf$Model=="R2_PrsEnvObsPC"& (subsetdf$ProportionObserved>0.25&subsetdf$ProportionObserved<=0.50),]
  PRSENVINCPC_25.50<-PRSENVINCPC_25.50[order(row.names(PRSENVINCPC_25.50)),]
  PRSENVINCPC_50.75<-subsetdf[subsetdf$Model=="R2_PrsEnvObsPC"& (subsetdf$ProportionObserved>0.50&subsetdf$ProportionObserved<=0.75),]
  PRSENVINCPC_50.75<-PRSENVINCPC_50.75[order(row.names(PRSENVINCPC_50.75)),]
  PRSENVINCPC_75.99<-subsetdf[subsetdf$Model=="R2_PrsEnvObsPC"& (subsetdf$ProportionObserved>0.75&subsetdf$ProportionObserved<=0.99),]
  PRSENVINCPC_75.99<-PRSENVINCPC_75.99[order(row.names(PRSENVINCPC_75.99)),]
  
  RawMeans=c(
    PRS_10.25= mean(PRS_10.25$R2),
    PRS_25.50= mean(PRS_25.50$R2),
    PRS_50.75= mean(PRS_50.75$R2),
    PRS_75.99= mean(PRS_75.99$R2),
    Env_10.25= mean(ENV_10.25$R2),
    Env_25.50= mean(ENV_25.50$R2),
    Env_50.75= mean(ENV_50.75$R2),
    Env_75.99= mean(ENV_75.99$R2),
    PRS_Env_10.25= mean(PRSENV_10.25$R2),
    PRS_Env_25.50= mean(PRSENV_25.50$R2),
    PRS_Env_50.75= mean(PRSENV_50.75$R2),
    PRS_Env_75.99= mean(PRSENV_75.99$R2),
    Env_PC10.25= mean(ENVINCPC_10.25$R2),
    Env_PC25.50= mean(ENVINCPC_25.50$R2),
    Env_PC50.75= mean(ENVINCPC_50.75$R2),
    Env_PC75.99= mean(ENVINCPC_75.99$R2),
    PRS_Env_PC10.25= mean(PRSENVINCPC_10.25$R2),
    PRS_Env_PC25.50= mean(PRSENVINCPC_25.50$R2),
    PRS_Env_PC50.75= mean(PRSENVINCPC_50.75$R2),
    PRS_Env_PC75.99= mean(PRSENVINCPC_75.99$R2))
  RawMeans<-data.frame(Model_R2=RawMeans, Model=names(RawMeans))
  
  #change R2s
  ChangeMeans=c(
  PRS_10.25=mean(PRS_10.25$R2),
  PRS_25.50=mean(PRS_25.50$R2),
  PRS_50.75=mean(PRS_50.75$R2),
  PRS_75.99=mean(PRS_75.99$R2),
  PRSaboveENV_10.25=mean(PRSENV_10.25$R2 - ENV_10.25$R2),
  PRSaboveENV_25.50=mean(PRSENV_25.50$R2 - ENV_25.50$R2),
  PRSaboveENV_50.75=mean(PRSENV_50.75$R2 - ENV_50.75$R2),
  PRSaboveENV_75.99=mean(PRSENV_75.99$R2 - ENV_75.99$R2),
  PRSaboveENVPC_10.25=mean(PRSENVINCPC_10.25$R2-ENVINCPC_10.25$R2),
  PRSaboveENVPC_25.50=mean(PRSENVINCPC_25.50$R2-ENVINCPC_25.50$R2),
  PRSaboveENVPC_50.75=mean(PRSENVINCPC_50.75$R2-ENVINCPC_50.75$R2),
  PRSaboveENVPC_75.99=mean(PRSENVINCPC_75.99$R2-ENVINCPC_75.99$R2))
  ChangeMeans<-data.frame(PRS_R2=ChangeMeans, Model=names(ChangeMeans))
  #raw values
  return(list(RawMeans,ChangeMeans))
}
plotdf<-rbind(
data.frame(R2Change(lowRGE_lowestConfoundCor)[[1]], Parameters="rGE=0.1,ConfounderCor=0.05-0.1"),
data.frame(R2Change(highRGE_lowestConfoundCor)[[1]], Parameters="rGE=0.5,ConfounderCor=0.05-0.1"),
data.frame(R2Change(lowRGE_lowConfoundCor)[[1]], Parameters="rGE=0.1, ConfounderCor=0.2-0.3"),
data.frame(R2Change(highRGE_lowConfoundCor)[[1]], Parameters="rGE=0.5, ConfounderCor=0.2-0.3"),
data.frame(R2Change(lowRGE_highConfoundCor)[[1]], Parameters="rGE=0.1, ConfounderCor=0.5-0.6"),
data.frame(R2Change(highRGE_highConfoundCor)[[1]], Parameters="rGE=0.5, ConfounderCor=0.5-0.6"),
data.frame(R2Change(lowRGE_highestConfoundCor)[[1]], Parameters="rGE=0.1,ConfounderCor=0.8-0.9"),
data.frame(R2Change(highRGE_highestConfoundCor)[[1]], Parameters="rGE=0.5,ConfounderCor=0.8-0.9"))

plotdf_change<-rbind(
  data.frame(R2Change(lowRGE_lowestConfoundCor)[[2]], Parameters="rGE=0.1,ConfounderCor=0.05-0.1"),
  data.frame(R2Change(highRGE_lowestConfoundCor)[[2]], Parameters="rGE=0.5,ConfounderCor=0.05-0.1"),
  data.frame(R2Change(lowRGE_lowConfoundCor)[[2]], Parameters="rGE=0.1, ConfounderCor=0.2-0.3"),
  data.frame(R2Change(highRGE_lowConfoundCor)[[2]], Parameters="rGE=0.5, ConfounderCor=0.2-0.3"),
  data.frame(R2Change(lowRGE_highConfoundCor)[[2]], Parameters="rGE=0.1, ConfounderCor=0.5-0.6"),
  data.frame(R2Change(highRGE_highConfoundCor)[[2]], Parameters="rGE=0.5, ConfounderCor=0.5-0.6"),
  data.frame(R2Change(lowRGE_highestConfoundCor)[[2]], Parameters="rGE=0.1,ConfounderCor=0.8-0.9"),
  data.frame(R2Change(highRGE_highestConfoundCor)[[2]], Parameters="rGE=0.5,ConfounderCor=0.8-0.9"))

###################################################################################
###################################################################################
###################################################################################
 
###################################################################################

Colors <-c("#E69F00", "#D55E00", "#56B4E9","#009E73" ,"#CC79A7","#0072B2")

###################################################################################
g_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)}
library(grid)
library(gridExtra)


###################################################################################
###################################################################################
###################################################################################
###################################################################################
#PLOTS

#make grouper
plotdf$Group<-as.numeric(stringr::str_split_fixed(plotdf$Model, n=2, pattern="\\.")[,2])
plotdf_change$Group<-as.numeric(stringr::str_split_fixed(plotdf_change$Model, n=2, pattern="\\.")[,2])

#group
plotdf$Group[plotdf$Group==25] <- "10-25%"
plotdf$Group[plotdf$Group==50] <- "26-50%"
plotdf$Group[plotdf$Group==75] <- "51-75%"
plotdf$Group[plotdf$Group==99] <- "76-99%"
plotdf$Group<-factor(plotdf$Group, levels=c("10-25%", "26-50%", "51-75%", "76-99%" ))

plotdf_change$Group[plotdf_change$Group==25] <- "10-25%"
plotdf_change$Group[plotdf_change$Group==50] <- "26-50%"
plotdf_change$Group[plotdf_change$Group==75] <- "51-75%"
plotdf_change$Group[plotdf_change$Group==99] <- "76-99%"
plotdf_change$Group<-factor(plotdf_change$Group, levels=c("10-25%", "26-50%", "51-75%", "76-99%" ))

#model
plotdf$Model<-as.character(stringr::str_split_fixed(plotdf$Model, n=2, pattern="[0-9]")[,1])
plotdf$Model[plotdf$Model=="PRS_"] <- "PRS"
plotdf$Model[plotdf$Model=="Env_"] <- "ENV"
plotdf$Model[plotdf$Model=="PRS_Env_"] <- "PRS_ENV"
plotdf$Model[plotdf$Model=="PRS_Env_PC"] <- "PRS_ENV_PC"
plotdf$Model[plotdf$Model=="Env_PC"] <- "ENV_PC"

plotdf_change$Model<-as.character(stringr::str_split_fixed(plotdf_change$Model, n=2, pattern="[0-9]")[,1])
plotdf_change$Model[plotdf_change$Model=="PRS_"] <- "PRS"
plotdf_change$Model[plotdf_change$Model=="PRSaboveENV_"] <- "PRSaboveENV"
plotdf_change$Model[plotdf_change$Model=="PRSaboveENVPC_"] <- "PRSaboveENVPC"

#rGE
plotdf$rGE<-as.numeric(stringr::str_split_fixed(stringr::str_split_fixed(plotdf$Parameters, n=2, pattern=",")[,1], n=2, pattern="=")[,2])
plotdf_change$rGE<-as.numeric(stringr::str_split_fixed(stringr::str_split_fixed(plotdf_change$Parameters, n=2, pattern=",")[,1], n=2, pattern="=")[,2])

#cor
plotdf$ConfounderCor<-(
  stringr::str_split_fixed(
    stringr::str_split_fixed(
      plotdf$Parameters, n=2, pattern=",")[,2],
    n=2, pattern="=")[,2])
plotdf_change$ConfounderCor<-(
  stringr::str_split_fixed(
    stringr::str_split_fixed(
      plotdf_change$Parameters, n=2, pattern=",")[,2],
    n=2, pattern="=")[,2])

#subset 
plotdf2<-plotdf[plotdf$Model=="PRS" | plotdf$Model=="PRS_ENV" | plotdf$Model=="PRS_ENV_PC" ,]
plotdf2$Model<-factor(plotdf2$Model, levels=c("PRS", "PRS_ENV", "PRS_ENV_PC"))

#total R2
png("PLOT_R&R_TOTALR2.tiff", width = 2250, height = 2550, units = 'px', res = 300)
ggplot(plotdf2, aes(x=Model, 
                    y=Model_R2, 
                    group=paste0(Group,rGE,ConfounderCor), 
                    color=ConfounderCor, 
                    shape=as.character(rGE),
                    linetype=Group)) +
  geom_point()+
  geom_path()+    
  scale_linetype_manual(values=c("dotted","dashed", "longdash", "solid"))+
  ylab("Total r-squared")+
  scale_color_manual(values=c(Colors))+
  theme_bw()+theme(
    plot.title= element_text(size = 15),
    axis.title.x =  element_blank(),
    axis.text.x = element_text(size = 10),
    axis.text.y = element_text(size=12),
    axis.title.y = element_text(size=12),
    legend.text = element_text(size = 15),
    legend.title= element_blank(),
    legend.position="bottom", legend.box="vertical",legend.margin=margin())+
  guides(fill=guide_legend(reverse=FALSE), 
         colour=guide_legend(reverse=FALSE))
dev.off()

#change R2
png("PLOT_R&R_ChangeR2.tiff", width = 2250, height = 2550, units = 'px', res = 300)
ggplot(plotdf_change, aes(x=Model, y=PRS_R2, 
                          group=paste0(Group,rGE,ConfounderCor), 
                          color=ConfounderCor, 
                          shape=as.character(rGE),
                          linetype=Group)) +
  geom_point()+
  geom_path()+    
  scale_linetype_manual(values=c("dotted","dashed", "longdash", "solid"))+
  ylab("PRS Change r-squared")+
  scale_color_manual(values=c(Colors))+
  theme_bw()+theme(
    plot.title= element_text(size = 15),
    axis.title.x =  element_blank(),
    axis.text.x = element_text(size = 10),
    axis.text.y = element_text(size=12),
    axis.title.y = element_text(size=12),
    legend.text = element_text(size = 15),
    legend.title= element_blank(),
    legend.position="bottom", legend.box="vertical",legend.margin=margin())+
  guides(fill=guide_legend(reverse=FALSE), 
         colour=guide_legend(reverse=FALSE))
dev.off()
  
  

###################################################################################

