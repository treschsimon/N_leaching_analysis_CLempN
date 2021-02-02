# R script developed by Simon Tresch (simon.tresch@iap.ch)
# R version 4.0.3 (2020-10-10)
# Used packages
library(tidyverse) # ggplot and dplyr for data manipulation: Wickham et al., (2019). JOSS 43
library(readxl) # import xlsx data
library(latex2exp) # LaTex expressions for plots
library(lme4) # mixed effect models Bates et al. 2018 JSS 67
library(jtools) # mixed models effect plots, tables for model outputs
library(ggeffects) # predicted values for all possible levels or values from a model’s predictor: Lüdecke D (2018). JOSS 26
library(splines) # modelling natural cubic splines

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
  mutate(tree_species=recode(tree_species,"Buche       " = "Beech","Fichte      " = "Spruce", "Buche/Fichte" = "Beech/Spruce")) # rename rows of a factor

mydata_CH <- mydata_CH %>%
  mutate(tree_group=case_when(tree_species=="Beech"~ "deciduous",
                              tree_species=="Spruce"~ "evergreen",
                              tree_species=="Beech/Spruce"~ "mixed"))


levels(mydata_CH$site_name)

levels(as.factor(mydata_CH$tree_species))
levels(as.factor(mydata_CH$tree_group))

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
# 33 sites from CH and 4 from CZ are below C/N ratio of 22

data_15_19<- mydata %>%  
              filter(year>2014 & year<2020) #data from 2015-2019 -> 5 year average

# average 2015-2019

data_mean<-data_15_19 %>%
              group_by(site_name,tree_species) %>% 
              mutate_if(is.numeric, funs(mean(.,na.rm=T))) %>% 
              distinct()
# 2.1 adding UK data sets (already averaged mean values but only from 2000-2006 no newer data available from Elena Vanguelova) ####
data_UK <- mydata %>% 
            filter(country_code=="UK")


final_data <- bind_rows(data_mean, data_UK)


# final_data <- final_data %>% dplyr::filter(!tree_species=="Oak") %>%
#   droplevels()#without UK oaks


# 2.3 adding CZ data sets (already averaged mean values 2015-2019) ####
data_UK <- mydata %>% 
  filter(country_code=="CZ")


final_data <- bind_rows(final_data, data_UK)


levels(as.factor(final_data$tree_species))


# 2.4 data description ####
# - Nin: Total N (kg ha-1 a-1 in troughfall)
# 
# - Nout: NO3 leaching measured in the soil solution of the mineral soil (UK: 50 cm soil depth, CH 80 cm soil depth)
# 
# # Data discrepancy: - UK data is from 2000-2006
# 
# - Nin for CH plots is the modelled N deposition (Rihm et al. 2019)
# 
# - Nout for CZ plots measured in stream  - outlet point of the catchment


# 3. plot data ####
# N deposition (throughfall and modelled) vs. No3 leaching

plot_N_leaching<-ggplot(final_data,aes(y=NO3_leaching,x=N_throughfall))+
  geom_point(size=2, aes(colour=as.factor(country_code)))+
  scale_y_continuous(trans='log', breaks=c(0.01,0.1, 1,10 ))+
  geom_smooth(span = 0.9, colour="grey", alpha=0.1, fullrange = T) +
  labs(y=TeX("log N_{out} (kg ha^{-1} a^{-1})"),x= TeX("N_{in} (kg ha^{-1} a^{-1})"),title = "Avarage measurements")+
  scale_colour_viridis_d(end = 0.92, name="Country")+
  theme(axis.title=element_text(size=12),axis.ticks = element_line(size=0.1), axis.line=element_line(colour="black"),text = element_text(size=14, colour = "black"),
        axis.text.y = element_text(size = 12, colour = "black"),axis.text.x = element_text(size =12, colour = "black"),axis.line.x=element_line(size = 0.1),axis.line.y=element_line(size = 0.1)) +
  theme(panel.background=element_blank(),panel.border = element_rect(colour = "black", fill=NA, size=0.1),panel.grid.minor=element_blank(),panel.grid.major=element_blank(),
        legend.key=element_blank(),legend.background=element_blank(),legend.title=element_text(),legend.justification = c(1, 1), legend.position = c(1,1),legend.direction="horizontal",legend.key.height=unit(1,"line"),plot.title = element_text(hjust = 0.5))+
  theme(legend.justification = c(1, 1), legend.position = "right",legend.direction="vertical")+
  theme(legend.title=element_text(size=12),legend.box.just = "left",title=element_text(size=10))
plot_N_leaching

# ggsave(plot_N_leaching,file="plots/plot_N_leaching.pdf", width = 12, height = 10, units = "cm", dpi = 1000)

# The Locally Weighted Least Squares Regression (applied with the LOESS function (Local Polynomial Regression Fitting)), uses a ‘nearest neighbour’ method indicates that there is a positive relationship between N in and N out. 



# 3.1 CN threshold value ####
# acidic forest soils C/N < 25 -> N losses are negligible and N is accumulated in all compartments of the ecosystem
# Various studies on deciduous and coniferous stands have demonstrated that if N loads in throughfall exceed 12 to 15 kgha1yr1, NO3- leaching increases exponentially
#  Below a C:N ratio of 25, the rate of nitrate leaching increases, endangering groundwater quality (MacDonald et al. 2002; Moldan et al. 2006). 

# Data from ICP Forests (UNECE 2005)
# Figure 9.2. N leaching fluxes (kg N ha-1 yr-1) set against N input in throughfall for sites with C:N < 22. (Source: UNECE 2005)

final_data<-final_data %>%
  mutate(C_N_25=case_when(C_N <= 25 ~ "< 25", 
                          C_N > 25 ~ "> 25")) %>%
  mutate(C_N_25_fact=as.factor(as.character(C_N_25)))
plot(final_data$C_N_25_fact)

final_data<-final_data %>%
  mutate(C_N_22=case_when(C_N <= 22 ~ "< 22", 
                          C_N > 22 ~ "> 22")) %>%
  mutate(C_N_22_fact=as.factor(as.character(C_N_22)))
plot(final_data$C_N_22_fact)
# more obs with C:N ratio threshold at 22, more used (pers. communication Braun 2021)

# Oak sites tend to have large C:N ratios -> try model without Oak sites


str(as.factor(final_data$tree_species))


final_data <- final_data %>% 
              drop_na(C_N) #1 observation contains NA in CN (Czech Republic	LIT)

final_data <- final_data %>% 
                mutate(N_bulk = coalesce(N_precipitation,N_throughfall)) # N_bulk for CH plots = N deposition

# 4. Model selection ####
# LMEM

hist(final_data$N_throughfall)
hist(final_data$N_bulk)
hist(final_data$NO3_leaching)
hist(log(final_data$NO3_leaching))

# final model all data
mod0<-lmer(log(NO3_leaching)  ~ N_throughfall  +  (1|country),data = final_data )
summ(mod0)
plot_summs(mod0)
AIC(mod0)
# sign. relationship Nin Nout

# MODEL FIT:
# AIC = 274.88, BIC = 283.51
# Pseudo-R² (fixed effects) = 0.12
# Pseudo-R² (total) = 0.20 
# 
# FIXED EFFECTS:

#   Est.   S.E.   t val.    d.f.      p

# (Intercept)          -0.98   0.67    -1.45    9.99   0.18
# N_throughfall          0.08   0.03     2.50   24.02   0.02



# final model with interaction N_dep and rainfall 
mod1<-lmer(log(NO3_leaching)  ~ N_throughfall * rainfall + (1|country) ,data = final_data )
summ(mod1)
plot_summs(mod1)
# model not better (AIC much bigger, interaction n.s.)

# final model with natural cubic splines (non-linearity)
mod2<-lmer(log(NO3_leaching)  ~ ns(N_throughfall,2)  +  (1|country),data = final_data )
summ(mod2)
plot_summs(mod2)
anova(mod0,mod2) #mod2 not s. improved keep mod0

# final model with natural orthogonal polinomials (non-linearity)
mod2.1<-lmer(log(NO3_leaching)  ~ poly(N_throughfall,2)  +  (1|country),data = final_data )
summ(mod2.1)
plot_summs(mod2.1)
anova(mod0,mod2.1) #mod2.1 not s. improved keep mod0


# model with C/N ratio
mod3<-lmer(log(NO3_leaching) ~ N_throughfall  + C_N +  (1|country),data = final_data)
summ(mod3)
plot_summs(mod3)
AIC(mod3)
# [1] 281.4215
# MODEL FIT:
#   AIC = 281.42, BIC = 292.22
# Pseudo-R² (fixed effects) = 0.10
# Pseudo-R² (total) = 0.18 
# 
# FIXED EFFECTS:

#   Est.   S.E.   t val.    d.f.      p

#   (Intercept)          -1.51   1.08    -1.39    6.51   0.21
# N_throughfall          0.09   0.03     2.50   60.85   0.02
# C_N                   0.02   0.03     0.63    2.14   0.59


# model with C/N ratio interaction with ndep
mod4<-lmer(log(NO3_leaching)  ~ N_throughfall  *  C_N +  (1|country),data = final_data)
summ(mod4)
plot_summs(mod4)
anova(mod3,mod4)# model not better keep mod3
AIC(mod4)

# model with C/N ratio grouped <22
mod5<-lmer(log(NO3_leaching)  ~ N_throughfall  +  C_N_22_fact +  (1|country),data = final_data)
summ(mod5)
plot_summs(mod5)
anova(mod3,mod5)
AIC(mod5)
# 275.2315 # delta AIC = -6.2 -> model with grouped C/N ratio better 

# MODEL FIT:
#   AIC = 275.23, BIC = 286.03
# Pseudo-R² (fixed effects) = 0.16
# Pseudo-R² (total) = 0.31 
# 
# FIXED EFFECTS:
#   Est.   S.E.   t val.    d.f.      p
#   (Intercept)             -0.54   1.07    -0.50   13.00   0.63
# N_throughfall             0.07   0.04     2.06   60.38   0.04
# C_N_22_fact> 22         -0.55   0.84    -0.65   18.68   0.53

# model with C/N ratio interaction with ndep
mod6<-lmer(log(NO3_leaching)  ~ N_throughfall  +  C_N_25_fact +  (1|country),data = final_data)
summ(mod6)
plot_summs(mod6)
anova(mod5,mod6)# model not better keep mod5
AIC(mod6)
# [1] 275.2128

# model without oak sites (3 UK sites with relatively high C:N ratios)
mod7<-lmer(log(NO3_leaching)  ~ N_throughfall  +  C_N_22_fact +  (1|country),
           data = final_data %>% dplyr::filter(!tree_species=="Oak"))
summ(mod7)
plot_summs(mod7)
AIC(mod7)# model not better keep mod3



# model including tree_group
mod8<-lmer(log(NO3_leaching)  ~ N_throughfall  +  C_N_22_fact + tree_group +  (1|country),
           data = final_data)
summ(mod8)
plot_summs(mod8)
anova(mod5,mod8)# model not better keep mod5
AIC(mod8)
# [1] 273.9482

# model only tree_group no CN
mod9<-lmer(log(NO3_leaching)  ~ N_throughfall  +  tree_group +  (1|country),
           data = final_data)
summ(mod9)
plot_summs(mod9)
AIC(mod9)# 
# [1] 273.9259 
# delta AIC -1.3041 -> keep model with CN


# model only tree_group no CN
mod10<-lmer(log(NO3_leaching)  ~ N_bulk  +  C_N_22_fact +  (1|country),
           data = final_data)
summ(mod10)
plot_summs(mod10)
AIC(mod10)# 
# [1] 275.6
# delta AIC 0.38-> keep mod5

# final model selection
mod<-mod3
mod<-mod0
mod<-mod9
mod<-mod5

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

# 4.1 Plotting effect plots of LMEM####
# effect plots with ggpredict
(LMEM_pred <- ggpredict(mod,terms = c("N_throughfall [all]")))

p_LMEM_pred_n_dep<-plot(LMEM_pred,show.title = F)+
  theme_classic()+
  ylab("NO3 leaching")+
  xlab("N deposition")
p_LMEM_pred_n_dep

p_LMEM_pred_n_dep<-ggplot(LMEM_pred,aes(x=x,y=predicted))+
   geom_point(data=final_data,size=1.5, aes(y=NO3_leaching,x=N_throughfall, colour=as.factor(country_code),shape=C_N_22))+
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = .1,show.legend = F,size=0.0001) +
  geom_line(size=1.5)+
  labs(y=TeX("N_{out} (kg ha^{-1} yr^{-1})"),x= TeX("N_{in} (kg ha^{-1} yr^{-1})"))+
  scale_colour_viridis_d(end = 0.92, name="Country")+
  scale_shape_manual( values = c(16,15), name="C:N ratio",na.translate = F)+
   scale_y_continuous( breaks = c(0,5,10,15,20,25,30,35))+
   scale_x_continuous( breaks = c(5,10,15,20,25,30,35,40))+
   annotate("text", x=4,y=30, hjust=0, label = c(TeX("N_{in} p = 0.04 *")),size=2.7)+
  # annotate("text", x=5,y=10, hjust=0, vjust=2,label = c("C/N ratio p < 0.05 * "),size=2.7)+
    theme(axis.title=element_text(size=12),axis.ticks = element_line(size=0.1), axis.line=element_line(colour="black"),text = element_text(size=14, colour = "black"),
        axis.text.y = element_text(size = 12, colour = "black"),axis.text.x = element_text(size =12, colour = "black"),axis.line.x=element_line(size = 0.1),axis.line.y=element_line(size = 0.1)) +
  theme(panel.background=element_blank(),panel.border = element_rect(colour = "black", fill=NA, size=0.1),panel.grid.minor=element_blank(),panel.grid.major=element_blank(),
        legend.key=element_blank(),legend.background=element_blank(),legend.title=element_text(),legend.key.height=unit(0.01,"line"),plot.title = element_text(hjust = 0.5))+
  theme(legend.justification = c(1, 1), legend.position = "right",legend.direction="vertical")+
  theme(legend.title=element_text(size=10),legend.box.just = "left",legend.spacing = unit(0, 'cm'))+
    coord_cartesian(ylim = c(0, 30), xlim = c(4,41)) #first plot than zoom in
p_LMEM_pred_n_dep

 ggsave(p_LMEM_pred_n_dep,file="plots/p_LMEM_pred_n_dep.pdf", width = 13, height = 10, units = "cm", dpi = 1000)

 
 # Effect similar to Waldner et al. 2019
 
 # plot for mod9 only tree group instead of C/N
 # p_LMEM_pred_n_dep<-ggplot(LMEM_pred,aes(x=x,y=predicted))+
 #   geom_point(data=final_data,size=1.5, aes(y=NO3_leaching,x=N_throughfall, colour=as.factor(country_code),shape=tree_group))+
 #   geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = .1,show.legend = F,size=0.0001) +
 #   geom_line(size=1.5)+
 #   labs(y=TeX("N_{out} (kg ha^{-1} yr^{-1})"),x= TeX("N_{in} (kg ha^{-1} yr^{-1})"))+
 #   scale_colour_viridis_d(end = 0.92, name="Country")+
 #   scale_shape_manual( values = c(16,15,17), name="Temperate\nForest",na.translate = F)+
 #   scale_y_continuous( breaks = c(0,5,10,15,20,25,30,35))+
 #   scale_x_continuous( breaks = c(5,10,15,20,25,30,35,40))+
 #   annotate("text", x=4,y=30, hjust=0, label = c(TeX("N_{in} p = 0.015 *")),size=2.7)+
 #   # annotate("text", x=5,y=10, hjust=0, vjust=2,label = c("C/N ratio p < 0.05 * "),size=2.7)+
 #   theme(axis.title=element_text(size=12),axis.ticks = element_line(size=0.1), axis.line=element_line(colour="black"),text = element_text(size=14, colour = "black"),
 #         axis.text.y = element_text(size = 12, colour = "black"),axis.text.x = element_text(size =12, colour = "black"),axis.line.x=element_line(size = 0.1),axis.line.y=element_line(size = 0.1)) +
 #   theme(panel.background=element_blank(),panel.border = element_rect(colour = "black", fill=NA, size=0.1),panel.grid.minor=element_blank(),panel.grid.major=element_blank(),
 #         legend.key=element_blank(),legend.background=element_blank(),legend.title=element_text(),legend.key.height=unit(0.01,"line"),plot.title = element_text(hjust = 0.5))+
 #   theme(legend.justification = c(1, 1), legend.position = "right",legend.direction="vertical")+
 #   theme(legend.title=element_text(size=10),legend.box.just = "left",legend.spacing = unit(0, 'cm'))+
 #   coord_cartesian(ylim = c(0, 30), xlim = c(4,41)) #first plot than zoom in
 # p_LMEM_pred_n_dep
 # 
 # ggsave(p_LMEM_pred_n_dep,file="plots/p_LMEM_pred_n_dep_2.pdf", width = 14, height = 10, units = "cm", dpi = 1000)
 # 
 # 


# CN ratio
(LMEM_pred <- ggpredict(mod,terms = c("C_N [all]")))

p_LMEM_pred_drought<-plot(LMEM_pred,show.title = F)+
  theme_classic()+
  ylab("NO3 leaching")+
  xlab("C/N ratio")
p_LMEM_pred_drought
