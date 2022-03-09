library(ggplot2)
library(gridExtra)
g_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)}
library(grid)

EDU_Results<-read.table("00ResultsForManuscript/ALCprs_EDU_max_RESULTS.csv", sep=",", header=T)
EDU_PCA_Vars<-read.table("00ResultsForManuscript/ALCprs_EDU_max_PCAvarsDesc.csv", sep=",", header=T)

FTND_Results<-read.table("00ResultsForManuscript/ALCprs_MAX_ftnd_sx_RESULTS.csv", sep=",", header=T)
FTND_PCA_Vars<-read.table("00ResultsForManuscript/ALCprs_MAX_ftnd_sx_PCAvarsDesc.csv", sep=",", header=T)

#plots
SharedMin <- (-0.20)
SharedMax <- 0.90
PRSLowerAxis<-  0.0
PRSUpperAxis <- 0.2


colnames(EDU_Results)[4] <- "AncestryPCs"
colnames(FTND_Results)[4] <- "AncestryPCs"
#############################################################
#############################################################
#############################################################
#############################################################
#############################################################

RESULTS <-FTND_Results
PlotMin<-SharedMin
PlotMax<-SharedMax

Colors <-c("#56B4E9","#009E73" ,"#CC79A7","#0072B2","#E69F00", "#D55E00" )


# #######################3 
plotdf<-RESULTS[RESULTS$Variable=="PRS",]
plotdf[plotdf=="NoCov"] <- "B1"
plotdf[plotdf=="PRS+Env"] <- "B2"
plotdf[plotdf=="PRS+Env+PC"] <- "B3"
plotdf<-plotdf[plotdf$AncestryPCs=="Yes",]
plotdf1<-plotdf

RESULTS <-EDU_Results
# #######################3 
plotdf<-RESULTS[RESULTS$Variable=="PRS",]
plotdf[plotdf=="NoCov"] <- "B1"
plotdf[plotdf=="PRS+Env"] <- "B2"
plotdf[plotdf=="PRS+Env+PC"] <- "B3"
plotdf<-plotdf[plotdf$AncestryPCs=="Yes",]
plotdf2<-plotdf

plotdf1$Environment <- "TOB"
plotdf2$Environment <- "EDU"
plotdf<-rbind(plotdf1,plotdf2)

png("PLOT5_justPRS_anc_tob_edu.tiff", width = 2250, height = 2550, units = 'px', res = 300)
ggplot(data=plotdf, aes(x=Model, y=B,group=Environment, color=Environment)) +
  geom_line(stat="identity", size=1)+
  geom_point()+
  geom_errorbar(
    aes(ymin = LowerCI95, ymax = UpperCI95),
    width = 0.1,,
    position=position_dodge(width=0.1)) +
  scale_y_continuous(breaks=seq(PRSLowerAxis,PRSUpperAxis,0.1), limits=c(PRSLowerAxis,PRSUpperAxis))+
  guides(color=guide_legend(nrow=2))+
  theme_bw()+
  theme(axis.text=element_text(size=12, color="black"),
        axis.title=element_text(size=20, color="black"),
        legend.title = element_text(size=12, color="black"),
        legend.text=element_text(size=12, color="black"),
        title = element_text(size=15, color="black"),
        legend.position="bottom")+
  labs(y="Beta")+ggtitle("PRS Beta Change Across Models")+
  scale_color_manual(values=c(Colors))+
  guides(color=guide_legend(nrow=1,ncol=2))
dev.off()
# #######################3 
