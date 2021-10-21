library(ggplot2)
w<-read.table('00resultsFiles/CombinedOutputALLoutB_nVars100_Orthog.csv', sep=",", header=FALSE)
colnames(w)<-c("B", "Model", "ProportionObserved", "PRStoEnv", "UnObsMinCor", "UnObsMaxCor")
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
w$Model[w$Model=="~Prs+Env+UnobsPC"]<-"~Prs+Env+CompletePC"
w$Model[w$Model=="~Prs+Env+ObsPC"]<-"~Prs+Env+IncompletePC"

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
MakePlot<-function(outB){
  a<-ggplot(outB,aes(x=as.numeric(as.character(ProportionObserved)), 
                y=as.numeric(as.character(B)), 
                color=Model))+
  geom_smooth(method='lm', formula= y~x, size=1.5, se=FALSE)+
  #geom_point(aes(group=Model), size=0.1, alpha=0.01)+
  labs(title=paste0("rGE= ",outB$PRStoEnv[1]),
       x ="Proportion Observed", y = "PRS B", color = "Model")+
  geom_hline(yintercept = 0, linetype="solid")+
    geom_hline(yintercept = 0.1, linetype="dashed")+
  ylim(-0.6,0.75)+
    scale_color_manual(values=c(Colors))+
    theme_bw()+theme(
      plot.title= element_text(size = 15),
      axis.title.x =  element_blank(),
      axis.text.x = element_text(size = 12),
      axis.text.y = element_text(size = 12),
      axis.title.y = element_text(size = 20),
      legend.text = element_blank(),
      legend.title= element_blank(),
      legend.position="none")+
    guides(fill=guide_legend(reverse=TRUE), 
           colour=guide_legend(reverse=TRUE))

  
  b<-ggplot(outB,aes(x=Model, 
                  y=as.numeric(as.character(B)), 
                  fill=Model))+
    geom_violin()+
    #geom_point(aes(group=Model), size=0.1, alpha=0.05)+
    labs(title=paste0(""),
         x ="", y = "PRS B", fill = "Model")+
    geom_hline(yintercept = 0, linetype="solid")+
    geom_hline(yintercept = 0.1, linetype="dashed")+
    scale_x_discrete(labels=c("","","",""))+
    ylim(-0.6,0.75)+
    scale_fill_manual(values=c(Colors))+
    theme_bw()+theme(
      plot.title= element_text(size = 15),
      axis.title.x = element_blank(),
      axis.text.x = element_text(size = 12),
      axis.text.y = element_blank(),
      axis.title.y = element_blank(),
      legend.text = element_text(size = 12),
      legend.title= element_blank(),
      legend.position="top")+
    guides(fill=guide_legend(reverse=FALSE), 
           colour=guide_legend(reverse=FALSE))
  
  PLOT<- list(a,b)
return(PLOT)
  }

#
lowRGE_lowConfoundCor$Model<-as.factor(lowRGE_lowConfoundCor$Model)
plot1<-MakePlot(lowRGE_lowConfoundCor)

highRGE_lowConfoundCor$Model<-as.factor(highRGE_lowConfoundCor$Model)
plot2<-MakePlot(highRGE_lowConfoundCor)

#
lowRGE_highConfoundCor$Model<-as.factor(lowRGE_highConfoundCor$Model)
plot3<-MakePlot(lowRGE_highConfoundCor)

highRGE_highConfoundCor$Model<-as.factor(highRGE_highConfoundCor$Model)
plot4<-MakePlot(highRGE_highConfoundCor)

#
lowRGE_lowestConfoundCor$Model<-as.factor(lowRGE_lowestConfoundCor$Model)
plot5<-MakePlot(lowRGE_lowConfoundCor)

highRGE_lowestConfoundCor$Model<-as.factor(highRGE_lowestConfoundCor$Model)
plot6<-MakePlot(highRGE_lowestConfoundCor)

#
lowRGE_highestConfoundCor$Model<-as.factor(lowRGE_highestConfoundCor$Model)
plot7<-MakePlot(lowRGE_highConfoundCor)

highRGE_highestConfoundCor$Model<-as.factor(highRGE_highestConfoundCor$Model)
plot8<-MakePlot(highRGE_highestConfoundCor)

highRGE_lowestConfoundCor
highRGE_highestConfoundCor


lm(B~ProportionObserved, 
   data=highRGE_lowestConfoundCor[highRGE_lowestConfoundCor$Model=="~Prs+Env+IncompletePC",])

lm(B~ProportionObserved, 
   data=highRGE_highestConfoundCor[highRGE_highestConfoundCor$Model=="~Prs+Env+IncompletePC",])

lm(B~ProportionObserved, 
   data=lowRGE_lowestConfoundCor[lowRGE_lowestConfoundCor$Model=="~Prs+Env+IncompletePC",])

lm(B~ProportionObserved, 
   data=lowRGE_highestConfoundCor[lowRGE_highestConfoundCor$Model=="~Prs+Env+IncompletePC",])


#PLOT 1
####################################################################
####################################################################
####################################################################
####################################################################
png("PLOT1.tiff", width = 2250, height = 2550, units = 'px', res = 300)

grid.arrange(             g_legend(plot1[[2]]), 
                          nrow=2,heights=c(1, 10),
  arrangeGrob(plot1[[1]] + theme(legend.position="none", 
                                   title = element_blank(), 
                                   axis.title.y = element_blank(),
                                   axis.text=element_blank()),
                         
                         plot1[[2]] + theme(legend.position="none", 
                                   axis.title.y = element_blank(),
                                   axis.text=element_blank(),
                                   title = element_blank()),
                         
                         plot2[[1]] + theme(legend.position="none", 
                                            axis.title.y = element_blank(),
                                            axis.text=element_blank(),
                                            title = element_blank() ),
                         plot2[[2]] + theme(legend.position="none", 
                                            axis.title.y = element_blank(),
                                            title = element_blank() ),
                         nrow=2),

             top = textGrob(paste0("Confounder Correlations ~ 0.2 to 0.3"),
                            gp=gpar(fontsize=20,font=1)),
             left=textGrob(paste0("PRS Beta"), rot=90,
                           gp=gpar(fontsize=18,font=1)),
             bottom=textGrob(paste0("LEFT: Proportion of Confounders | RIGHT: Model"), 
                             gp=gpar(fontsize=18,font=1))

             )
dev.off()
#PLOT 2
####################################################################
####################################################################
####################################################################
####################################################################
png("PLOT2.tiff", width = 2250, height = 2550, units = 'px', res = 300)
grid.arrange(             g_legend(plot3[[2]]), 
                          nrow=2,heights=c(1, 10),
                          arrangeGrob(plot3[[1]] + theme(legend.position="none", 
                                                         title = element_blank(), 
                                                         axis.title.y = element_blank(),
                                                         axis.text=element_blank()),
                                      
                                      plot3[[2]] + theme(legend.position="none", 
                                                         axis.title.y = element_blank(),
                                                         axis.text=element_blank(),
                                                         title = element_blank()),
                                      
                                      plot4[[1]] + theme(legend.position="none", 
                                                         axis.title.y = element_blank(),
                                                         axis.text=element_blank(),
                                                         title = element_blank() ),
                                      plot4[[2]] + theme(legend.position="none", 
                                                         axis.title.y = element_blank(),
                                                         title = element_blank() ),
                                      nrow=2),
                          
                          top = textGrob(paste0("Confounder Correlations ~ 0.5 to 0.6"),
                                         gp=gpar(fontsize=20,font=1)),
                          left=textGrob(paste0("PRS Beta"), rot=90,
                                        gp=gpar(fontsize=18,font=1)),
                          bottom=textGrob(paste0("LEFT: Proportion of Confounders | RIGHT: Model"), 
                                          gp=gpar(fontsize=18,font=1))
                          
)
dev.off()
#PLOT 3
####################################################################
####################################################################
####################################################################
####################################################################
png("PLOT3.tiff", width = 2250, height = 2550, units = 'px', res = 300)
grid.arrange(             g_legend(plot5[[2]]), 
                          nrow=2,heights=c(1, 10),
                          arrangeGrob(plot5[[1]] + theme(legend.position="none", 
                                                         title = element_blank(), 
                                                         axis.title.y = element_blank(),
                                                         axis.text=element_blank()),
                                      
                                      plot5[[2]] + theme(legend.position="none", 
                                                         axis.title.y = element_blank(),
                                                         axis.text=element_blank(),
                                                         title = element_blank()),
                                      
                                      plot6[[1]] + theme(legend.position="none", 
                                                         axis.title.y = element_blank(),
                                                         axis.text=element_blank(),
                                                         title = element_blank() ),
                                      plot6[[2]] + theme(legend.position="none", 
                                                         axis.title.y = element_blank(),
                                                         title = element_blank() ),
                                      nrow=2),
                          
                          top = textGrob(paste0("Confounder Correlations ~ 0.05 to 0.1"),
                                         gp=gpar(fontsize=20,font=1)),
                          left=textGrob(paste0("PRS Beta"), rot=90,
                                        gp=gpar(fontsize=18,font=1)),
                          bottom=textGrob(paste0("LEFT: Proportion of Confounders | RIGHT: Model"), 
                                          gp=gpar(fontsize=18,font=1))
                          
)
dev.off()
#PLOT 4
####################################################################
####################################################################
####################################################################
####################################################################
png("PLOT4.tiff", width = 2250, height = 2550, units = 'px', res = 300)
grid.arrange(             g_legend(plot7[[2]]), 
                          nrow=2,heights=c(1, 10),
                          arrangeGrob(plot7[[1]] + theme(legend.position="none", 
                                                         title = element_blank(), 
                                                         axis.title.y = element_blank(),
                                                         axis.text=element_blank()),
                                      
                                      plot7[[2]] + theme(legend.position="none", 
                                                         axis.title.y = element_blank(),
                                                         axis.text=element_blank(),
                                                         title = element_blank()),
                                      
                                      plot8[[1]] + theme(legend.position="none", 
                                                         axis.title.y = element_blank(),
                                                         axis.text=element_blank(),
                                                         title = element_blank() ),
                                      plot8[[2]] + theme(legend.position="none", 
                                                         axis.title.y = element_blank(),
                                                         title = element_blank() ),
                                      nrow=2),
                          
                          top = textGrob(paste0("Confounder Correlations ~ 0.8 to 0.9"),
                                         gp=gpar(fontsize=20,font=1)),
                          left=textGrob(paste0("PRS Beta"), rot=90,
                                        gp=gpar(fontsize=18,font=1)),
                          bottom=textGrob(paste0("LEFT: Proportion of Confounders | RIGHT: Model"), 
                                          gp=gpar(fontsize=18,font=1))
                          
)
dev.off()

  
  
  
  
  

###################################################################################

