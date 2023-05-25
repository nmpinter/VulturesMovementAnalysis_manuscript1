##First run this so you have Degree_coFlight etc. and the observed values to caluculate p-values of reference distribution against###########

####################    Breeding Layer  ###################

##################    Centrality Per Layer #############

setwd("C:/XXXXX/IgraphNetworksRandomCoods")
dat<-read.csv("BreedingCentralitySimplified2021.csv", header = T)

subset(dat, Layer =="Aggr")
LayerIDs<-as.data.frame(cbind(c(1:3, "Aggr"), c("CoFlight","NocturnalGroundInteractions","DiurnalGroundInteractions","Aggregate")))

colnames(LayerIDs)<-c("Layer","LayerName")

dat1<-dplyr::left_join(dat, LayerIDs, by = "Layer")


library(tidyr)
str(dat1)
dat_wide<-as.data.frame(dat1[,-c(1,2)] %>%
                          pivot_wider(names_from = c(LayerName), 
                                      values_from = c(Degree, Strength, PageRank, StrengthPerDegree)))

nrow(dat_wide)
ncol(dat_wide)

head(as.data.frame(dat_wide))
colMeans((dat_wide)[-1])
class(dat_wide)

##for supp mat Table 1 for manuscript1 #####
setwd("C:/XXXXXX/FiguresForVultureManuscript1/FiguresForVultureManuscript2021")
write.csv((as.data.frame(cbind(Range_lower = round(apply((dat_wide)[-1],2,min),3),
                               Range_upper = round(apply((dat_wide)[-1],2,max),3),
                               Mean = round(apply((dat_wide)[-1],2,mean),3),
                               Median = round(apply((dat_wide)[-1],2,median),3),
                               StdDev=round(apply((dat_wide)[-1],2,sd),3)))), "SuppMatTable1SummStats.csv")



sd(as.numeric(as.character(dat_wide[,2])))
nrow(dat_wide)
tail(dat_wide)

unique(dat_wide$Degree_NocturnalGroundInteractions)
unique(dat_wide$Degree_Aggregate)


Obs_Degrees<-dat_wide[,c("Label","Degree_CoFlight",
                         "Degree_NocturnalGroundInteractions","Degree_DiurnalGroundInteractions","Degree_Aggregate")]

Obs_Strengths<-dat_wide[,c("Label","Strength_CoFlight",
                           "Strength_NocturnalGroundInteractions","Strength_DiurnalGroundInteractions","Strength_Aggregate")]

Obs_PageRanks<-dat_wide[,c("Label","PageRank_CoFlight",
                           "PageRank_NocturnalGroundInteractions","PageRank_DiurnalGroundInteractions","PageRank_Aggregate")]

Obs_StrengthPerDegree<-dat_wide[,c("Label","StrengthPerDegree_CoFlight",
                                   "StrengthPerDegree_NocturnalGroundInteractions","StrengthPerDegree_DiurnalGroundInteractions","StrengthPerDegree_Aggregate")]

corrMatrix1<-cor(as.data.frame(lapply(Obs_Degrees[,-1], function(x) as.numeric(as.character(x)))), method = "spearman")
corrMatrix1<-cor(as.data.frame(lapply(dat_wide[,-1], function(x) as.numeric(as.character(x)))), method = "spearman")

corrMatrix<-corrMatrix1
is.na(corrMatrix) <- abs(corrMatrix) < 0.6
library(ggcorrplot)
par(mar=c(2,2,2,3))
ggcorrplot(corrMatrix1, hc.order = FALSE, 
           type = "lower",method = "circle",insig = "blank",
           tl.cex = 0.8,sig.level = 0.05,
           lab = TRUE, lab_col = "black", lab_size =2.5, pch.cex =0.75)+
  ggtitle("Correlation: Movement Personality & Versatility") +
  theme(axis.text.x=element_text(size=8, angle=45, vjust=1, hjust=1, 
                                 margin=margin(-3,0,0,0)),
        axis.text.y=element_text(size=10, margin=margin(0,-3,0,0)),
        panel.grid.major=element_blank())



corrMatrix1<-cor(as.data.frame(lapply(Obs_Degrees[,-1], function(x) as.numeric(as.character(x)))), method = "spearman")
##only keeping abs correlations >0.6
corrMatrix<-corrMatrix1
is.na(corrMatrix) <- abs(corrMatrix) < 0.6
library(ggcorrplot)
#?ggcorrplot
ggcorrplot(corrMatrix, hc.order = FALSE, 
           type = "lower",method = "square",insig = "blank",
           tl.cex = 0.6,
           sig.level = 0.05, p.mat = cor_pmat(corrMatrix),
           lab = TRUE, lab_col = "black", lab_size =1.75, pch.cex =0.1)+
  ggtitle("Correlation Movement Personality & Centrality") +
  theme(axis.text.x=element_text(size=15, angle=45, vjust=1, hjust=1, 
                                 margin=margin(-3,0,0,0)),
        axis.text.y=element_text(size=15, margin=margin(0,-3,0,0)),
        panel.grid.major=element_blank())


##################### For degree coflight, NocturnalGroundInteractions, DiurnalGroundInteractions correlation with Aggregate Degree ###########

Degree_CoFlight <- cor.test(Obs_Degrees$Degree_CoFlight, Obs_Degrees$Degree_Aggregate, 
                            method = "spearman")

Degree_NocturnalGroundInteractions <- cor.test(Obs_Degrees$Degree_NocturnalGroundInteractions, Obs_Degrees$Degree_Aggregate, 
                                               method = "spearman")

Degree_DiurnalGroundInteractions <- cor.test(Obs_Degrees$Degree_DiurnalGroundInteractions, Obs_Degrees$Degree_Aggregate, 
                                             method = "spearman")

par(mar=c(3,3,3,3))
##Visualize
library("ggpubr")
A<-ggscatter(Obs_Degrees, x = "Degree_Aggregate", y = "Degree_CoFlight", 
             add = "reg.line", conf.int = TRUE, 
             #cor.coef = TRUE, 
             cor.method = "spearman",color = "deepskyblue", 
             fill = "dodgerblue",size = 0.9, shape=0,stroke=1.75,
             #xlab = "Aggregate Degree", 
             ylab = "\nCoFlight", position = position_jitter(w = 0.4, h = 0.4)
)+ylim(c(7,28))+
  theme(axis.title.x=element_blank(),
        axis.title.y = element_text(color = "black", size = 13.5, angle = 90, hjust = 0.5,
                                    vjust = 0.5, 
                                    face = "bold")
  )+ggtitle("Degree")+
  theme(plot.title = element_text(size = 25, face = "bold",hjust = 0.5))
#theme(plot.title = element_text(size=40,hjust = 0.5))#+ geom_abline(intercept = 0, slope = 1)

A#+geom_abline (intercept=0, slope=1,linetype=2,size=0.5)

max(Obs_Degrees$Degree_Aggregate)
B<-ggscatter(Obs_Degrees, x = "Degree_Aggregate", y = "Degree_NocturnalGroundInteractions", 
             add = "reg.line", conf.int = TRUE, add.params = list(linetype ="dashed"),
             #cor.coef = TRUE, 
             cor.method = "spearman",fill = "darkgreen", 
             color = "chartreuse4", size = 0.9, shape=2,stroke=1.75
             #,xlab = "Aggregate Degree"
             , ylab = "Nocturnal  \nGround  Interactions", position = position_jitter(w = 0.4, h = 0.4)
)+ylim(c(7,28))+
  theme(axis.title.x=element_blank(),
        axis.title.y = element_text(color = "black", size = 13.5, angle = 90, hjust = 0.5,
                                    vjust = 0.5, 
                                    face = "bold"))#+ 
# geom_smooth(aes(linetype = 3))+
# scale_linetype_manual(values = 3)
B
C<-ggscatter(Obs_Degrees, x = "Degree_Aggregate", y = "Degree_DiurnalGroundInteractions", 
             add = "reg.line", conf.int = TRUE,stroke=1.75,
             #cor.coef = TRUE, 
             cor.method = "spearman",color = "gold3", fill = "gold3", 
             size = 0.9, shape = 5
             ,xlab = "Aggregate Degree"
             , ylab = "Diurnal  \nGround  Interactions", position = position_jitter(w = 0.4, h = 0.4)
)+ylim(c(7,28))+
  theme(axis.title.x=element_blank(),
        axis.title.y = element_text(color = "black", size = 13.5, angle = 90, hjust = 0.5,
                                    vjust = 0.5, 
                                    face = "bold"))


C

##################### For Strength coflight, NocturnalGroundInteractions, DiurnalGroundInteractions correlation with Aggregate Strength ###########

Strength_CoFlight <- cor.test(Obs_Strengths$Strength_CoFlight, Obs_Strengths$Strength_Aggregate, 
                              method = "spearman")

Strength_NocturnalGroundInteractions <- cor.test(Obs_Strengths$Strength_NocturnalGroundInteractions, Obs_Strengths$Strength_Aggregate, 
                                                 method = "spearman")

Strength_DiurnalGroundInteractions <- cor.test(Obs_Strengths$Strength_DiurnalGroundInteractions, Obs_Strengths$Strength_Aggregate, 
                                               method = "spearman")


##Visualize
library("ggpubr")
D<-ggscatter(Obs_Strengths, x = "Strength_Aggregate", y = "Strength_CoFlight", 
             add = "reg.line", conf.int = TRUE, stroke=1.75,
             #cor.coef = TRUE, 
             cor.method = "spearman",color = "deepskyblue", 
             fill = "dodgerblue",size = 0.9, shape=0, position = position_jitter(w = 0, h = 0)
             #,xlab = "Aggregate Strength", ylab = "CoFlight Strength"
)+ylim(c(0,15))+
  theme(axis.title.x=element_blank(), axis.title.y=element_blank())+ggtitle("Strength")+
  theme(plot.title = element_text(size = 25, face = "bold",hjust = 0.5))

E<-ggscatter(Obs_Strengths, x = "Strength_Aggregate", y = "Strength_NocturnalGroundInteractions", 
             add = "reg.line", conf.int = TRUE,stroke=1.75, 
             #cor.coef = TRUE, 
             cor.method = "spearman",fill = "darkgreen", 
             color = "chartreuse4", size = 0.9, shape=2, position = position_jitter(w = 0, h = 0)
             #,xlab = "Aggregate Strength", ylab = "NocturnalGroundInteractions Strength"
)+ylim(c(0,15))+
  theme(axis.title.x=element_blank(), axis.title.y=element_blank())
f<-ggscatter(Obs_Strengths, x = "Strength_Aggregate", y = "Strength_DiurnalGroundInteractions", 
             add = "reg.line", conf.int = TRUE, stroke=1.75,
             #cor.coef = TRUE, 
             cor.method = "spearman",color = "gold3", fill = "gold3", 
             size = 0.9, shape = 5
             , position = position_jitter(w = 0, h = 0)
             ,xlab = "Aggregate Strength"#, ylab = "DiurnalGroundInteractions Strength"
)+ylim(c(0,15))+
  theme(axis.title.x=element_blank(), axis.title.y=element_blank())

##################### For PageRank coflight, NocturnalGroundInteractions, DiurnalGroundInteractions correlation with Aggregate PageRank ###########

PageRank_CoFlight <- cor.test(Obs_PageRanks$PageRank_CoFlight, Obs_PageRanks$PageRank_Aggregate, 
                              method = "spearman")

PageRank_NocturnalGroundInteractions <- cor.test(Obs_PageRanks$PageRank_NocturnalGroundInteractions, Obs_PageRanks$PageRank_Aggregate, 
                                                 method = "spearman")

PageRank_DiurnalGroundInteractions <- cor.test(Obs_PageRanks$PageRank_DiurnalGroundInteractions, Obs_PageRanks$PageRank_Aggregate, 
                                               method = "spearman")


##Visualize
library("ggpubr")
G<-ggscatter(Obs_PageRanks, x = "PageRank_Aggregate", y = "PageRank_CoFlight", 
             add = "reg.line", conf.int = TRUE, stroke=1.75,
             #cor.coef = TRUE, 
             cor.method = "spearman",color = "deepskyblue", 
             fill = "dodgerblue",size = 0.9, shape=0, position = position_jitter(w = 0, h = 0)
             #,xlab = "Aggregate PageRank", ylab = "CoFlight PageRank"
)+
  theme(axis.title.x=element_blank(), axis.title.y=element_blank())+ggtitle("PageRank")+
  theme(plot.title = element_text(size = 25, face = "bold",hjust = 0.5))+ylim(c(0,0.08))

H<-ggscatter(Obs_PageRanks, x = "PageRank_Aggregate", y = "PageRank_NocturnalGroundInteractions", 
             add = "reg.line", conf.int = TRUE, 
             #cor.coef = TRUE, 
             cor.method = "spearman",fill = "darkgreen", 
             color = "chartreuse4", size = 0.9, shape=2,stroke=1.75, position = position_jitter(w = 0, h = 0)
             #,xlab = "Aggregate PageRank", ylab = "NocturnalGroundInteractions PageRank"
)+ylim(c(0,0.08))+
  theme(axis.title.x=element_blank(), axis.title.y=element_blank())

I<-ggscatter(Obs_PageRanks, x = "PageRank_Aggregate", y = "PageRank_DiurnalGroundInteractions", 
             add = "reg.line", conf.int = TRUE, stroke=1.75,
             #cor.coef = TRUE, 
             cor.method = "spearman",color = "gold3", fill = "gold3", 
             size = 0.9, shape = 5
             , position = position_jitter(w = 0, h = 0)
             ,xlab = "Aggregate PageRank"#, ylab = "DiurnalGroundInteractions PageRank"
)+ylim(c(0,0.08))+
  theme(axis.title.x=element_blank(), axis.title.y=element_blank())

#ggarrange(A,B,C,D,E,f,G,H,I, ncol = 3, nrow = 3)

####Exporting figure 5 with dotted non-significant reg line coroost #####
setwd("C:/XXXXXX/FiguresForVultureManuscript2021")
tiff("Fig5CorrsWithDottedRegLine.tiff", units="in", width=9, height=5, res=300)

ggarrange(A,D,G,B,E,H,C,f,I, ncol=3,nrow=3)
dev.off()
##################### For StrengthPerDegree coflight, NocturnalGroundInteractions, DiurnalGroundInteractions correlation with Aggregate StrengthPerDegree ###########

StrengthPerDegree_CoFlight <- cor.test(Obs_StrengthPerDegree$StrengthPerDegree_CoFlight, Obs_StrengthPerDegree$StrengthPerDegree_Aggregate, 
                                       method = "spearman")

StrengthPerDegree_NocturnalGroundInteractions <- cor.test(Obs_StrengthPerDegree$StrengthPerDegree_NocturnalGroundInteractions, Obs_StrengthPerDegree$StrengthPerDegree_Aggregate, 
                                                          method = "spearman")

StrengthPerDegree_DiurnalGroundInteractions <- cor.test(Obs_StrengthPerDegree$StrengthPerDegree_DiurnalGroundInteractions, Obs_StrengthPerDegree$StrengthPerDegree_Aggregate, 
                                                        method = "spearman")


##Visualize
library("ggpubr")
ggscatter(Obs_StrengthPerDegree, x = "StrengthPerDegree_Aggregate", y = "StrengthPerDegree_CoFlight", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "spearman",
          #xlab = "Miles/(US) gallon", ylab = "Weight (1000 lbs)"
)

ggscatter(Obs_StrengthPerDegree, x = "StrengthPerDegree_Aggregate", y = "StrengthPerDegree_NocturnalGroundInteractions", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "spearman",
          #xlab = "Miles/(US) gallon", ylab = "Weight (1000 lbs)"
)
ggscatter(Obs_StrengthPerDegree, x = "StrengthPerDegree_Aggregate", y = "StrengthPerDegree_DiurnalGroundInteractions", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "spearman",
          #xlab = "Miles/(US) gallon", ylab = "Weight (1000 lbs)"
)




########Step 2 is to run the remaining section that calculates the refernce distribution ######
