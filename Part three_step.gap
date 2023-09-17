#### analysis 2 (yuan_ms2) ####
#### I revised some analyses here, and change the figure 4. ####
library(ggplot2)
library(plyr)
library(ggthemes)
library(lme4)
library(nlme)
library(sjPlot)
library(ggrepel)
library(MuMIn)
library(ggsci)
set.seed(1234)
fulldata$OSR_V <- 1-fulldata$AdultNoMonk

df2 <- data_summary(fulldata, varname="steps",
                    groupnames=c("sex",'OSR_V','PVID','Ethnic'))
head(df2)


df3 <- reshape(df2, idvar = "PVID", timevar = "sex",direction = "wide")
df3$step.gap <- df3$steps.F-df3$steps.M
df3
cor.test(df2$OSR_V,df2$step.gap)

fulldata$step.gap <- NA
for (i in 1:659){
  fulldata$step.gap[i] <- df3$step.gap[df3$PVID==fulldata$PVID[i]]
}
count(unique(fulldata$PVID))
fulldata$step.gap[(which(fulldata$OSR_V<0.4))]

hist(fulldata$step.gap)

step.gap <- list()

step.gap[[1]] <- gls(step.gap~1,method = 'ML',na.action = na.exclude,data=fulldata)
step.gap[[2]] <- lme(step.gap~1,random=~1|Ethnic,method = 'ML',na.action = na.exclude,data = fulldata)
step.gap[[3]] <- lme(step.gap~OffWristbandRef0+lac_period+relevel(factor(age),ref='60~')+BMI,random=~1|Ethnic,method = 'ML',na.action = na.exclude,data = fulldata)

step.gap[[4]] <- lme(step.gap~OSR_V,random=~1|Ethnic,method = 'ML',na.action = na.exclude,data = fulldata)

step.gap[[5]] <- lme(step.gap~OffWristbandRef0+lac_period+relevel(factor(age),ref='60~')+BMI+OSR_V,random=~1|Ethnic,method = 'ML',na.action = na.exclude,data = fulldata)

m1 <- model.sel(step.gap[[4]],step.gap[[5]],step.gap[[2]],step.gap[[1]],step.gap[[3]])
model.sel(step.gap[[4]],step.gap[[5]])
m1
summary(step.gap[[5]])
tab_model(step.gap[[5]])

write.csv(m1, 'ms_step_gap.csv')
step.gap[[7]] <- model.avg(step.gap[[4]],step.gap[[5]])
summary(step.gap[[7]])
tab_model(step.gap[[2]],step.gap[[3]],step.gap[[4]],step.gap[[5]])
summary(step.gap[[5]])

fulldata$prestep.gap1 <- predict(step.gap[[5]])
fulldata$PVID[which(fulldata$Ethnic=='Mosuo')]

step.gap[[2]] <- lmer(step.gap~OffWristbandRef0+lac_period+relevel(factor(age),ref='60~')+BMI+OSR_V+(1|Ethnic),REML = FALSE,na.action = na.exclude,data = fulldata)

tab_model(step.gap[[2]])
fulldata$prestep.gap <- predict(step.gap[[1]])


fulldata$AdultNoMonk_digits3 <- round(fulldata$AdultNoMonk,digits = 3)

fulldata$VillageID <- NA
for (i in 1:659){
  fulldata$VillageID[i] <- sexratio$ï..VILLAGEID[sexratio$OSR_V==fulldata$AdultNoMonk_digits3[i]]
}
count(unique(fulldata$VillageID))
ggplot(fulldata[complete.cases(fulldata$prestep.gap1),],aes(x=OSR_V,y=prestep.gap1))+
  geom_point(size=2,alpha=0.2,color='grey'
  )+geom_smooth(aes(color=Ethnic,fill=Ethnic,y=prestep.gap1),method = 'lm',alpha=1)+
  theme_few(base_size = 14)+xlab("% reproductive males per village")+
  ylab("Gender inequality (workload gap)")+geom_label_repel(aes(label=VillageID,color=Ethnic),hjust=0, size=3, segment.size=0.05, nudge_x=0.5, max.overlaps = getOption("ggrepel.max.overlaps", default = 0),direction='y')+#I controlled for culture difference among these ethnic groups in replace of phylogeny
  theme(axis.title = element_text(size = 12),axis.text = element_text(size=9),
        strip.text = element_text(size = 13),
        legend.position = c(0.1,0.15),legend.title = element_text(size=7),
        legend.key.size = unit(0.2, 'mm'),legend.margin=unit(5,"cm"),
        legend.spacing.y = unit(0.1, 'cm'),legend.text = element_text(size=7))+
  guides(color = guide_legend(title = 'Ethnic group',order=1),
         linetype= FALSE,fill=FALSE)
ggsave('workload gap vary with OSR1.tiff',width = 4.62,height = 3.86,dpi = 800)
ggsave('workload gap vary with OSR2.tiff',width = 4.62,height = 3.86,dpi = 800)
ggsave('workload gap vary with OSR3.tiff',width = 4.62,height = 4.22,dpi = 800)
ggsave('workload gap vary with OSR4.tiff',width = 4.92,height = 4.52,dpi = 800)

step.gap[[7]] <- lme(steps~sex*OSR_V,random=~1|Ethnic,method = 'ML',na.action = na.exclude,data = fulldata)

fulldata$prestep_null <- predict(step.gap[[7]])
ggplot(fulldata[complete.cases(fulldata$prestep.gap1),],aes(x=OSR_V,y=prestep.gap1))+
  geom_point(size=1,alpha=0.2#,color='grey'
  )+geom_smooth(aes(color=Ethnic,fill=Ethnic,y=prestep.gap1),method = 'lm',alpha=0.2)+
  theme_few(base_size = 14)+xlab("% reproductive males per village")+
  ylab("Gender inequality (workload gap)")+#geom_label(aes(label=VillageID,color=Ethnic))+#I controlled for culture difference among these ethnic groups in replace of phylogeny
  theme(axis.title = element_text(size = 12),axis.text = element_text(size=9),
        strip.text = element_text(size = 13),
        legend.position = c(0.90,0.89),legend.title = element_text(size=7),
        legend.key.size = unit(0.2, 'mm'),legend.margin=unit(5,"cm"),
        legend.spacing.y = unit(0.1, 'cm'),legend.text = element_text(size=7))+
  guides(color = guide_legend(title = 'Ethnic group',order=1),
         linetype= FALSE,fill=FALSE)
