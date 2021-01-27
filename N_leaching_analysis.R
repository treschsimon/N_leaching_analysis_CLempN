# R script developed by Simon Tresch (simon.tresch@iap.ch)
# R version 4.0.3 (2020-10-10)
# Used packages
library(tidyverse) # ggplot and dplyr for data manipulation: Wickham et al., (2019). JOSS 43
library(readxl) # import xlsx data
library(latex2exp) # LaTex expressions for plots
library(lme4) # mixed effect models Bates et al. 2018 JSS 67
library(jtools) # mixed models effect plots, tables for model outputs
library(ggeffects) # predicted values for all possible levels or values from a model’s predictor: Lüdecke D (2018). JOSS 26

# original Figure 2010 Assessment:
# Figure 9.2. N leaching fluxes (kg N ha-1 yr-1) set against N input in throughfall for sites with C:N < 22. (Source: UNECE 2005)

# 1. Load data ####
mydata <- read_excel("data/ICP_Level_II_sites_N_data_2021.xlsx",
                     sheet="data_R_export")

# 2.1 adding CH IAP data sets  ####

mydata_CH<- read.csv("data/NO3_leaching_data_export_CLempN_2021.csv",header = TRUE,
                     sep=";",dec = ".")
mydata_CH <- mydata_CH %>%
  mutate(site_name=as.factor(site_name)) %>%
  mutate(tree_species=recode(tree_species,"Buche" = "Beech","Fichte" = "Spruce", "Buche/Fichte" = "Beech/Spruce")) # rename rows of a factor

levels(mydata_CH$site_name)

# remove new sites <5 years data and Sagno (not included N deposition from Italy)

mydata_CH <- mydata_CH %>%
           dplyr::filter(!site_code%in% c("1201","1200", "1199", "1198","1197","1196","1195","1194","1122"))

# merge data
mydata <- bind_rows(mydata,mydata_CH)

# 2. Data manipulation ####
test <- mydata %>% 
  dplyr::mutate(C_N_22= case_when(C_N <=22 ~ 1, TRUE ~ 2))

test <- test%>% 
  dplyr::select(country_code, site_name, C_N_22) %>% 
  distinct()

test <- test%>% dplyr::filter(C_N_22<2) %>% 
  distinct()

summary(as.factor(test$country_code))
# 33 sites from CH amd 4 from CZ are below C/N ratio of 22

data_15_19<- mydata %>%  
              filter(year>2014 & year<2020) #data from 2015-2019 -> 5 year average

# average 2015-2019

data_mean<-data_15_19 %>%
              group_by(site_name,tree_species) %>% 
              mutate_if(is.numeric, funs(mean(.,na.rm=T))) %>% 
              distinct()
# 2.1 adding UK data sets (already averaged mean values 2015-2019) ####
data_UK <- mydata %>% 
            filter(country_code=="UK")


final_data <- bind_rows(data_mean, data_UK)


# final_data <- final_data %>% dplyr::filter(!tree_species=="Oak") %>%
#   droplevels()#without UK oaks



# 2.3 adding CZ data sets (already averaged mean values 2015-2019) ####
data_UK <- mydata %>% 
  filter(country_code=="CZ")


final_data <- bind_rows(final_data, data_UK)



# 3. plot data ####
# N deposition (throughfall and modelled) vs. No3 leaching

plot_N_leaching<-ggplot(final_data,aes(x=NO3_leaching,y=N_troughfall))+
  geom_point(size=2, aes(colour=as.factor(country_code)))+
  labs(x=TeX("NO_3 leaching (kg ha^{-1} a^{-1})"),y= TeX("N deposition (kg ha^{-1} a^{-1})"),title = "Avarage measurements 2015-2019")+
  scale_colour_viridis_d(end = 0.6, name="Country")+
  theme(axis.title=element_text(size=12),axis.ticks = element_line(size=0.1), axis.line=element_line(colour="black"),text = element_text(size=14, colour = "black"),
        axis.text.y = element_text(size = 12, colour = "black"),axis.text.x = element_text(size =12, colour = "black"),axis.line.x=element_line(size = 0.1),axis.line.y=element_line(size = 0.1)) +
  theme(panel.background=element_blank(),panel.border = element_rect(colour = "black", fill=NA, size=0.1),panel.grid.minor=element_blank(),panel.grid.major=element_blank(),
        legend.key=element_blank(),legend.background=element_blank(),legend.title=element_text(),legend.justification = c(1, 1), legend.position = c(1,1),legend.direction="horizontal",legend.key.height=unit(1,"line"),plot.title = element_text(hjust = 0.5))+
  theme(legend.justification = c(1, 1), legend.position = c(0.99,0.99),legend.direction="vertical")+
  theme(legend.title=element_text(size=12),legend.box.just = "left",title=element_text(size=10))
plot_N_leaching

# ggsave(plot_N_leaching,file="plots/plot_N_leaching.pdf", width = 10, height = 10, units = "cm", dpi = 1000)



# 4. modeling ####
# LMEM

hist(final_data$N_troughfall)
hist(final_data$NO3_leaching)
hist(log(final_data$NO3_leaching))

# UK data
mod <- lm(log(NO3_leaching)  ~ N_troughfall ,data = final_data %>% 
            filter(country_code=="UK"))

summ(mod)
plot_summs(mod)
# effect not significant

# BE data
mod <- lm(log(NO3_leaching)  ~ N_troughfall ,data = final_data %>% 
            filter(country_code=="BE"))

summ(mod)
plot_summs(mod)
# effect not significant for BE data set

# CH data
mod <- lm(log(NO3_leaching)  ~ N_troughfall ,data = final_data %>% 
            filter(country_code=="CH"))

summ(mod)
plot_summs(mod)
# effect significant for CH data set


# CZ data
mod <- lm(log(NO3_leaching)  ~ N_troughfall ,data = final_data %>% 
            filter(country_code=="CZ"))

summ(mod)
plot_summs(mod)
# effect not significant for CZ data set

# final model all data
mod0<-lmer(log(NO3_leaching)  ~ N_troughfall  +  (1|country),data = final_data )
summ(mod0)
plot_summs(mod0)
AIC(mod0)
# sign. relationship

# final model with interaction N_dep and rainfall 
mod1<-lmer(log(NO3_leaching)  ~ N_troughfall * rainfall + (1|country) ,data = final_data )
summ(mod1)
plot_summs(mod1)
# model not better

# without UK data
mod_data<-final_data %>%  filter(!country_code=="UK")
mod2<-lmer(log(NO3_leaching)  ~ N_troughfall  + C_N +  (1|country),data = mod_data)
summ(mod2)
plot_summs(mod2)
# not sign. anymore

# model with C/N ratio
mod3<-lmer(log(NO3_leaching) ~ N_troughfall  + C_N +  (1|country),data = mod_data)
summ(mod3)
plot_summs(mod3)
AIC(mod3)
# model improved (AIC much lower) -> C/N ratio has an positive effect on NO3 leaching

# model with C/N ratio interaction with ndep
mod4<-lmer(log(NO3_leaching)  ~ N_troughfall  *  C_N +  (1|country),data = mod_data)
summ(mod4)
plot_summs(mod4)
anova(mod0,mod4)
# model not better


# final model selection
mod<-mod3
# mod<-mod0
summ(mod,digits=4)

# iid evaluation
par(mfrow=c(2,2), mar=c(4,4,2,1), mgp=c(2.2,0.8,0))
scatter.smooth(fitted(mod), resid(mod)); abline(h=0, lty=2)
mtext("Tukey-Anscombe Plot", 3, line=0.8, cex=0.8)  # residuals vs. fitted
qqnorm(resid(mod), main="Normal qq-plot, residuals", cex.main=0.8) # qq of residuals
qqline(resid(mod))
scatter.smooth(fitted(mod), sqrt(abs(resid(mod)))) # res. var vs. fitted
qqnorm(ranef(mod)$country[,1], main="Normal qq-plot, random effects", cex.main=0.8)
qqline(ranef(mod)$country[,1]) # qq of random effects


# effect plots with ggpredict
(LMEM_pred <- ggpredict(mod,terms = c("N_troughfall [all]")))

p_LMEM_pred_n_dep<-plot(LMEM_pred,show.title = F)+
  theme_classic()+
  ylab("NO3 leaching")+
  xlab("N deposition")
p_LMEM_pred_n_dep

p_LMEM_pred_n_dep<-ggplot(LMEM_pred,aes(x=x,y=predicted))+
   # geom_point(data=final_data,size=3, aes(x=NO3_leaching,y=N_troughfall,colour=as.factor(country_code)))+
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = .1,show.legend = F,size=0.0001) +
  geom_line(size=1.5)+
  labs(x=TeX("NO_3 leaching (kg ha^{-1} a^{-1})"),y= TeX("N deposition (kg ha^{-1} a^{-1})"))+
  scale_colour_brewer(palette="Dark2",name="Country" )+
  scale_y_continuous(limits=c(0,38), breaks = c(0,5,10,15,20,25,30,35))+
   scale_x_continuous(limits=c(4,42), breaks = c(5,10,15,20,25,30,35,40))+
   annotate("text", x=5,y=37, hjust=0, label = c("N deposition p < 0.01 **"),size=2.7)+
  # annotate("text", x=5,y=10, hjust=0, vjust=2,label = c("C/N ratio p < 0.05 * "),size=2.7)+
    theme(axis.title=element_text(size=12),axis.ticks = element_line(size=0.1), axis.line=element_line(colour="black"),text = element_text(size=14, colour = "black"),
        axis.text.y = element_text(size = 12, colour = "black"),axis.text.x = element_text(size =12, colour = "black"),axis.line.x=element_line(size = 0.1),axis.line.y=element_line(size = 0.1)) +
  theme(panel.background=element_blank(),panel.border = element_rect(colour = "black", fill=NA, size=0.1),panel.grid.minor=element_blank(),panel.grid.major=element_blank(),
        legend.key=element_blank(),legend.background=element_blank(),legend.title=element_text(),legend.justification = c(1, 1), legend.position = c(1,1),legend.direction="horizontal",legend.key.height=unit(1,"line"),plot.title = element_text(hjust = 0.5))+
  theme(legend.justification = c(1, 1), legend.position = c(0.99,0.99),legend.direction="vertical")+
  theme(legend.title=element_text(size=10),legend.box.just = "left")
p_LMEM_pred_n_dep

# ggsave(p_LMEM_pred_n_dep,file="plots/p_LMEM_pred_n_dep.pdf", width = 10, height = 10, units = "cm", dpi = 1000)


# CN ratio
(LMEM_pred <- ggpredict(mod,terms = c("C_N [all]")))

p_LMEM_pred_drought<-plot(LMEM_pred,show.title = F)+
  theme_classic()+
  ylab("NO3 leaching")+
  xlab("C/N ratio")
p_LMEM_pred_drought
