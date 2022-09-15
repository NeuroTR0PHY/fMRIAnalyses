#setwd("E:/MRI_analyses06_67/06_67_Maps/ToExport")
setwd("~/Downloads/MRI_analyses06_67/10_2021/Interaction10_ROI/10_19_2021_int10")

require(ggplot2)
require(psych)  
require(Publish)
require(Rmisc)
require(Hmisc)
require(nlme)
require(car)
require(multcompView)
require(ez)
require(ggsignif)
require(ggpubr)
require(tidyverse)
require(rstatix)
require(datarium)
require(nlme)
require(multcomp)
require(ez)
require(lmerTest)

#roi = c("BA6_L","BA6_R","IFG_L","IFG_R", "IPS_L", "IPS_R", "SMA", "SPL_L", "SPL_R", "INSULA" )
roi = c(130:189)
n = 20; data = matrix(nrow = n*4,ncol = 4 ); data = data.frame(data)
names(data)=c("Subject","Grasp","Vision","Mean_PE")
subject=c("f01","f02","f03","f04","f05","f07","f08","f09","f10",
          "f11","f12","f13","f14","f15","f16","f17","f18","f19",
          "f20","f21")
cope=c("int_cope11","int_cope12","int_cope13","int_cope14")
grasp=c("Grasp","Grasp","Reach","Reach")
vision=c("Vision","No-Vision","Vision","No-Vision")

###############

for(ROI in 1:length(roi)){
data = data.frame(data)
col=6
i=1
for(sub in 1:n){
  for(cond in 1:4){
    x = read.table(paste0(cope[cond],"/",roi[ROI],"/",subject[sub],"/report.txt"))
    data$Subject[i] = subject[sub]; data$Grasp[i] = grasp[cond]
    data$Vision[i] = vision[cond]; data$Mean_PE[i] = x[1,col]
    i=i+1
  }
}
assign(paste0("d_",roi[ROI]),data)
}
#BA6_L = data





##dIFFERENCE sCORES
name = "BA6right"


####avoid####
diff = GV$Mean_PE - RV$Mean_PE
VisDif = cbind(GV,diff)
diff = GNV$Mean_PE - RNV$Mean_PE
NoVisDif = cbind(GNV, diff)

DiffData = rbind(VisDif,NoVisDif)
DiffType = rep("blank",40)
DiffData = cbind(DiffData, DiffType)
DiffData$DiffType = as.character(DiffData$DiffType)
DiffData$DiffType[DiffData$Vision == "No-Vision"] = "GNV - RNV" 
DiffData$DiffType[DiffData$Vision == "Vision"] = "GV - RV" 
DiffData$DiffType = as.factor(DiffData$DiffType)
mode(DiffData$DiffType)
###ci.mean(Mean_PE~Grasp + Vision, data = data)
nv = DiffData[DiffData$DiffType == "GNV - RNV",]
v = DiffData[DiffData$DiffType == "GV - RV",]
test = t.test(DiffData$diff~DiffData$DiffType, paiered=T, alternative = "greater")$p
ci.mean(diff~Vision, data = DiffData)

DiffData = DiffData[-c(2,3,4)]
write.csv(DiffData, "ROI_output/SPOC_R.csv")



###start here to plot
#for proper layout of ggplot error bars
pd = position_dodge(.2)

#set global ggplot text size
theme_set(theme_pubr(base_size = 24)) 

#
options(contrasts=c("contr.sum","contr.poly"))


data = d_188
name = "188"

data$diff = rep(0,20)
data$diff[data$Vision == "Vision" & data$Grasp == "Grasp"] = "GVS"
data$diff[data$Vision == "No-Vision" & data$Grasp == "Grasp"] = "GNV"
data$diff[data$Vision == "Vision" & data$Grasp == "Reach"] = "TVS"
data$diff[data$Vision == "No-Vision" & data$Grasp == "Reach"] = "TNV"

data$Grasp = as.factor(data$Grasp)
data$Vision = as.factor(data$Vision)
data$diff = as.factor(data$diff)
lme_model2 = lme(Mean_PE ~ diff, random = ~1|Subject,correlation = corCompSymm(form=~1|Subject), data = data )
summary(glht(lme_model2, linfct=mcp(diff = "Tukey")), test = adjusted(type = "single-step"))

#pairwise.t.test(data$Mean_PE,data$diff,paired = T, p.adjust.method = "holm", alternative = "less")


min(data$Mean_PE)
max(data$Mean_PE)

if(min(data$Mean_PE) > 0){
  ylowlimit = min(data$Mean_PE) - (min(data$Mean_PE) * .2 )
}
if(min(data$Mean_PE) < 0){
  ylowlimit = min(data$Mean_PE) + (min(data$Mean_PE) * .2 )
}

yuplimit = max(data$Mean_PE) + (max(data$Mean_PE) * .3)


x = 12

  plot = ggplot(data = data, aes(y = Mean_PE, x = diff )) + 
  geom_boxplot(outlier.shape = NA)+
    #geom_bar(stat = "summary")+
  geom_jitter(shape=16, position=position_jitter(0.2), size = 2) +
   # ggtitle(name) +
  scale_x_discrete(name = "", limits=c("GNV", "TNV", "GVS", "TVS")) +
  scale_y_continuous(name = "% BOLD Signal Change", limits = c(ylowlimit,yuplimit)) +
  theme(axis.text.y = element_text(face = 'bold', size = x),axis.text.x = element_text(face = 'bold', size =x),strip.text = element_text(face="bold", size = x), axis.title.y = element_text(face='bold', size = x), axis.title.x = element_blank()) +
    geom_signif(comparisons=list(c("GNV", "TNV")), annotations="p<0.001", y_position = (yuplimit * .81), tip_length = 0.02, vjust=0.0) +
   geom_signif(comparisons=list(c("GVS", "TNV")), annotations="p<0.001", y_position = (yuplimit * .95), tip_length = 0.02, vjust=0.0) +
   geom_signif(comparisons=list(c("GNV", "TVS")), annotations="p<0.001", y_position = (yuplimit * .875), tip_length = 0.02, vjust=0.0) +
   geom_signif(comparisons=list(c("GVS", "TVS")), annotations="p=0.0644", y_position = (yuplimit * .81), tip_length = 0.02, vjust=0.0) +
   geom_signif(comparisons=list(c("TNV", "TVS")), annotations="p=0.0785", y_position = (yuplimit * .725), tip_length = 0.02, vjust=0.0)
    
    
    
    #geom_signif(comparisons = list(c("GNV", "TNV"), c("GVS","TVS")), map_signif_level=T, test = "t.test", test.args = list(alternative="greater",paired=T), y_position = (yuplimit * .75), tip_length = .05, size = 1, textsize = 5) +
  #geom_signif(comparisons = list(c("GNV","GVS")), map_signif_level=T, test = "t.test", test.args = list(alternative="greater",paired=T), y_position = yuplimit * .9, tip_length = .05, size = 1, textsize = 5)

  plot
  
  ggsave(paste0("BoxPlots/",name,"Boxplot",".png"), 
         plot = plot, width = 3, height = 3.5, units = "in", dpi = 600) 

 #plot
tapply(data$Mean_PE, data$diff, summary)
tapply(data$Mean_PE, data$Vision, describe)

#model = aov(Mean_PE ~ Grasp * Vision + Error(Subject/Grasp * Vision), data = data) 
#summary(model)

#model = ezANOVA(data = data, dv = Mean_PE, wid = Subject, within = .(Grasp, Vision), detailed = T, type = 3)
#model

model = anova_test(data = data, dv = Mean_PE, wid = Subject, within = c(Grasp, Vision))
get_anova_table(model)

  

lme_model = lme(Mean_PE ~ Grasp * Vision, data = data, random = ~1|Subject)
summary(glht(lme_model, linfct=mcp(Grasp = "Tukey")), test = adjusted(type = "bonferroni"))
summary(glht(lme_model, linfct=mcp(Vision = "Tukey")), test = adjusted(type = "bonferroni"))

TukeyHSD(model, conf.level=0.95)
#t.test(data$Mean_PE[data$diff == "GNV"], data$Mean_PE[data$diff == "TNV"], paired = T)
