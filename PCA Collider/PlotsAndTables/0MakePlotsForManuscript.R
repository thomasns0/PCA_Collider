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
PRSLowerAxis<-  0
PRSUpperAxis <- 0.23


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
plotdf[plotdf=="NoCov"] <- "~"
plotdf[plotdf=="PRS+Env"] <- "~TOB"
plotdf[plotdf=="PRS+Env+PC"] <- "~TOB + Pheno PCs"
plotdf$Covariates <- factor(plotdf$Model, levels=c( "~", "~TOB", "~TOB + Pheno PCs"))

png("PLOT5_justPRS.tiff", width = 2250, height = 2550, units = 'px', res = 300)
ggplot(data=plotdf, aes(x=Covariates, y=B,group=AncestryPCs, color=AncestryPCs)) +
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
  labs(y="Beta")+ggtitle("PRS Beta Change Across Models, Environment=TOB")+
  scale_color_manual(values=c(Colors))+
  guides(color=guide_legend(nrow=1,ncol=2))+
  geom_hline(yintercept = 0)
dev.off()
# #######################3 
plotdf<-RESULTS[RESULTS$Variable=="PRS",]
plotdf[plotdf=="NoCov"] <- "Univariate"
plotdf[plotdf=="PRS+Env"] <- "Uncorrected"
plotdf[plotdf=="PRS+Env+PC"] <- "Corrected"
plotdf$Model <- factor(plotdf$Model, levels=c( "Univariate", "Uncorrected", "Corrected"))

a<-ggplot(data=plotdf, aes(x=Model, y=B,group=AncestryPCs, color=AncestryPCs)) +
  geom_line(stat="identity", size=1)+
  geom_point()+
  geom_errorbar(
    aes(ymin = LowerCI95, ymax = UpperCI95),
    width = 0.1,,
    position=position_dodge(width=0.1)) +
  scale_y_continuous(breaks=seq(PlotMin,PlotMax,0.1), limits=c(PlotMin,PlotMax))+
  guides(color=guide_legend(nrow=2))+
  theme_bw()+
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=20),
        legend.title = element_text(size=12),
        legend.text=element_text(size=12),
        title = element_text(size=15),
        legend.position="bottom")+
  labs(y="Beta")+ggtitle("PRS")+
  scale_color_manual(values=c(Colors))+
  guides(color=guide_legend(nrow=1,ncol=2))+
  geom_hline(yintercept = 0)


plotdf<-RESULTS[RESULTS$Variable=="Env",]
plotdf[plotdf=="NoCov"] <- "Univariate"
plotdf[plotdf=="PRS+Env"] <- "Uncorrected"
plotdf[plotdf=="PRS+Env+PC"] <- "Corrected"
plotdf$Model <- factor(plotdf$Model, levels=c( "Univariate", "Uncorrected", "Corrected"))

b<-ggplot(data=plotdf, aes(x=Model, y=B,group=AncestryPCs, color=AncestryPCs)) +
  geom_line(stat="identity", size=1)+
  geom_point()+
  geom_errorbar(
    aes(ymin = LowerCI95, ymax = UpperCI95),
    width = 0.1,,
    position=position_dodge(width=0.1)) +
  scale_y_continuous(breaks=seq(PlotMin,PlotMax,0.1), limits=c(PlotMin,PlotMax))+
  guides(color=guide_legend(nrow=2))+
  theme_bw()+
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=20),
        legend.title = element_text(size=12),
        legend.text=element_text(size=12),
        title = element_text(size=15),
        legend.position="bottom")+
  labs(y="Beta")+ggtitle("TOB")+
  scale_color_manual(values=c(Colors))+
  guides(color=guide_legend(nrow=1,ncol=2))+
  geom_hline(yintercept = 0)


grid.arrange(             g_legend(a), 
                          nrow=2,heights=c(1, 10),
                          
                          arrangeGrob(a + theme(legend.position="none", 
                                                title = element_text(size=12), 
                                                axis.title.y=element_blank(),
                                                axis.title.x= element_blank(),
                                                axis.text.x=element_text(size=12, color="black"),
                                                axis.text.y=element_text(size=12, color="black")),
                                      b + theme(legend.position="none", 
                                                title = element_text(size=12), 
                                                axis.title.y= element_blank(),
                                                axis.title.x= element_blank(),
                                                axis.text.x =element_text(size=12, color="black"),
                                                axis.text.y=element_blank()),
                                      nrow=1),
                          
                          top = textGrob(paste0("Beta Change Across Models, Environment=TOB"),
                                         gp=gpar(fontsize=20,font=1)),
                          left=textGrob(paste0("Beta"), rot=90,
                                        gp=gpar(fontsize=18,font=1)),
                          bottom=textGrob(paste0("Model"), 
                                          gp=gpar(fontsize=18,font=1)))

######################################
######################################
######################################
######################################
######################################
png("PLOT5.tiff", width = 2250, height = 2550, units = 'px', res = 300)

grid.arrange(             g_legend(a), 
                          nrow=2,heights=c(1, 10),
                          
                          arrangeGrob(a + theme(legend.position="none", 
                                                title = element_text(size=12), 
                                                axis.title.y=element_blank(),
                                                axis.title.x= element_blank(),
                                                axis.text.x=element_text(size=12, color="black"),
                                                axis.text.y=element_text(size=12, color="black")),
                                      b + theme(legend.position="none", 
                                                title = element_text(size=12), 
                                                axis.title.y= element_blank(),
                                                axis.title.x= element_blank(),
                                                axis.text.x =element_text(size=12, color="black"),
                                                axis.text.y=element_blank()),
                                      nrow=1),
                          
                          top = textGrob(paste0("Change in Beta Across Models, Environment=TOB"),
                                         gp=gpar(fontsize=20,font=1)),
                          left=textGrob(paste0("Beta"), rot=90,
                                        gp=gpar(fontsize=18,font=1)),
                          bottom=textGrob(paste0("Model"), 
                                          gp=gpar(fontsize=18,font=1)))

dev.off()


#####################################

#############################################################
#############################################################
#############################################################
#############################################################
#############################################################

RESULTS <-EDU_Results
PlotMin<-SharedMin
PlotMax<-SharedMax

Colors <-c("#56B4E9","#009E73" ,"#CC79A7","#0072B2","#E69F00", "#D55E00" )

# #######################3 
plotdf<-RESULTS[RESULTS$Variable=="PRS",]
plotdf[plotdf=="NoCov"] <- "~"
plotdf[plotdf=="PRS+Env"] <- "~EDU"
plotdf[plotdf=="PRS+Env+PC"] <- "~EDU + Pheno PCs"
plotdf$Covariates <- factor(plotdf$Model, levels=c( "~", "~EDU", "~EDU + Pheno PCs"))

png("PLOT6_justPRS.tiff", width = 2250, height = 2550, units = 'px', res = 300)
ggplot(data=plotdf, aes(x=Covariates, y=B,group=AncestryPCs, color=AncestryPCs)) +
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
  labs(y="Beta")+ggtitle("PRS Change in Beta Across Models, Environment=EDU")+
  scale_color_manual(values=c(Colors))+
  guides(color=guide_legend(nrow=1,ncol=2))+
  geom_hline(yintercept = 0)
dev.off()

###################
plotdf<-RESULTS[RESULTS$Variable=="PRS",]
plotdf[plotdf=="NoCov"] <- "Univariate"
plotdf[plotdf=="PRS+Env"] <- "Uncorrected"
plotdf[plotdf=="PRS+Env+PC"] <- "Corrected"
plotdf$Model <- factor(plotdf$Model, levels=c( "Univariate", "Uncorrected", "Corrected"))

a<-ggplot(data=plotdf, aes(x=Model, y=B,group=AncestryPCs, color=AncestryPCs)) +
  geom_line(stat="identity", size=1)+
  geom_point()+
  geom_errorbar(
    aes(ymin = LowerCI95, ymax = UpperCI95),
    width = 0.1,,
    position=position_dodge(width=0.1)) +
  scale_y_continuous(breaks=seq(PlotMin,PlotMax,0.1), limits=c(PlotMin,PlotMax))+
  guides(color=guide_legend(nrow=2))+
  theme_bw()+
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=20),
        legend.title = element_text(size=12),
        legend.text=element_text(size=12),
        title = element_text(size=15),
        legend.position="bottom")+
  labs(y="Beta")+ggtitle("PRS")+
  scale_color_manual(values=c(Colors))+
  guides(color=guide_legend(nrow=1,ncol=2))+
  geom_hline(yintercept = 0)

plotdf<-RESULTS[RESULTS$Variable=="Env",]
plotdf[plotdf=="NoCov"] <- "Univariate"
plotdf[plotdf=="PRS+Env"] <- "Uncorrected"
plotdf[plotdf=="PRS+Env+PC"] <- "Corrected"
plotdf$Model <- factor(plotdf$Model, levels=c( "Univariate", "Uncorrected", "Corrected"))

b<-ggplot(data=plotdf, aes(x=Model, y=B,group=AncestryPCs, color=AncestryPCs)) +
  geom_line(stat="identity", size=1)+
  geom_point()+
  geom_errorbar(
    aes(ymin = LowerCI95, ymax = UpperCI95),
    width = 0.1,,
    position=position_dodge(width=0.1)) +
  scale_y_continuous(breaks=seq(PlotMin,PlotMax,0.1), limits=c(PlotMin,PlotMax))+
  guides(color=guide_legend(nrow=2))+
  theme_bw()+
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=20),
        legend.title = element_text(size=12),
        legend.text=element_text(size=12),
        title = element_text(size=15),
        legend.position="bottom")+
  labs(y="Beta")+ggtitle("EDU")+
  scale_color_manual(values=c(Colors))+
  guides(color=guide_legend(nrow=1,ncol=2))+
  geom_hline(yintercept = 0)


grid.arrange(             g_legend(a), 
                          nrow=2,heights=c(1, 10),
                          
                          arrangeGrob(a + theme(legend.position="none", 
                                                title = element_text(size=12), 
                                                axis.title.y=element_blank(),
                                                axis.title.x= element_blank(),
                                                axis.text.x=element_text(size=12, color="black"),
                                                axis.text.y=element_text(size=12, color="black")),
                                      b + theme(legend.position="none", 
                                                title = element_text(size=12), 
                                                axis.title.y= element_blank(),
                                                axis.title.x= element_blank(),
                                                axis.text.x =element_text(size=12, color="black"),
                                                axis.text.y=element_blank()),
                                      nrow=1),
                          
                          top = textGrob(paste0("Change in Beta Across Models, Environment=EDU"),
                                         gp=gpar(fontsize=20,font=1)),
                          left=textGrob(paste0("Beta"), rot=90,
                                        gp=gpar(fontsize=18,font=1)),
                          bottom=textGrob(paste0("Model"), 
                                          gp=gpar(fontsize=18,font=1)))

######################################
######################################
######################################
######################################
######################################
png("PLOT6.tiff", width = 2250, height = 2550, units = 'px', res = 300)

grid.arrange(             g_legend(a), 
                          nrow=2,heights=c(1, 10),
                          
                          arrangeGrob(a + theme(legend.position="none", 
                                                title = element_text(size=12), 
                                                axis.title.y=element_blank(),
                                                axis.title.x= element_blank(),
                                                axis.text.x=element_text(size=12, color="black"),
                                                axis.text.y=element_text(size=12, color="black")),
                                      b + theme(legend.position="none", 
                                                title = element_text(size=12), 
                                                axis.title.y= element_blank(),
                                                axis.title.x= element_blank(),
                                                axis.text.x =element_text(size=12, color="black"),
                                                axis.text.y=element_blank()),
                                      nrow=1),
                          
                          top = textGrob(paste0("Change in Beta Across Models, Environment=EDU"),
                                         gp=gpar(fontsize=20,font=1)),
                          left=textGrob(paste0("Beta"), rot=90,
                                        gp=gpar(fontsize=18,font=1)),
                          bottom=textGrob(paste0("Model"), 
                                          gp=gpar(fontsize=18,font=1)))

dev.off()

