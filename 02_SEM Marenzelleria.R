########################################################!
#2: SEM analysis for Marenzelleria and Figure 3A #######!
########################################################!
remove(list=ls())
#Load libraries and functions
source("./Code/Libraries_Functions.R")

# 2.1: DATA EXPLORATION --------------------------------------------------------
#Multicolinearity of variables
test = read.csv("./Data/Marenzelleria_data_metrics.txt",header=T,dec=".",sep="\t", check.names = FALSE)
variables <- test %>% select(NIS_Di_nw,Shannon,richness,Eveness,FRic,FEve,bot_T_var,bot_sal_var,bot_oxy_var,depth,bot_sal,bot_oxy,bot_T)

chart.Correlation(variables, histogram=TRUE, pch=19)

#Variance inflation factor (VIF) analysis (Fig. S2A)
vif <- lm(log(NIS_relab) ~ NIS_Di_nw + Eveness + richness + FRic + FEve + bot_T_var +
            bot_sal_var + bot_oxy_var + depth + bot_sal + bot_oxy + bot_T, test); summary (vif)

vif_values <- data.frame(vars = names(vif(vif)), vif = vif(vif)); sort(vif(vif))

#Fig. S2A
vif_plot_Mar = ggplot(vif_values, aes(x = vars, y = vif)) + 
  geom_bar(stat = "identity", color = "steelblue", fill = "steelblue") + ylim(0, 10) +
  scale_y_continuous(labels = function(x) format(x, nsmall = 1)) + 
  coord_flip() + labs(x = "Variables", y = "VIF values") + geom_hline(yintercept = 5, color = "darkred", size = 1) + 
  theme_minimal(); vif_plot_Mar

#ggsave(file="./Plots/VIF_Mar.svg", plot=vif_plot_Mar, width=7.5, height=5.5)

#test for spatial autocorrelation
autocorr_test = test
data.spatialCor.lm <- lm(NIS_Di ~ 1, autocorr_test)
autocorr_test$Resid <- rstandard(data.spatialCor.lm)
coordinates(autocorr_test) <- ~lon + lat  #effectively convert the data into a spatial data frame
bubble(autocorr_test, "Resid")

#Skewness of data
hist(log(test$FRic))
skewness(test$NIS_relab, na.rm = TRUE)

#Gam for testing linearity of the responses, to see if we need to include a quadratic term
gam<- gam(log(NIS_relab) ~ 
            s(NIS_Di_nw,k=3) + 
            s(FRic,k=3) + 
            s(richness,k=3)+ 
            s(FEve,k=3) + 
            s(Eveness,k=3) + 
            s(bot_oxy,k=3) + 
            s(bot_sal,k=3)+ 
            s(bot_sal_var,k = 3)+
            s(bot_oxy_var, k = 3)+            
            s(bot_T,k=3)+  
            s(depth,k=3),
          #s(station, bs = 're', k = 3),
          #family = Gamma(link = "log"),
          #s(station, bs = 're', k = 3),
          #family = betar(link = "logit"),
          correlation = corGaus(form = ~ lon + lat),
          data=test)

summary(gam)
par(mfrow=c(4,3),mar=c(2,4,0,2,0.2))
plot(gam, shade=T, shade.col="grey",rug=F,res=T)

# Squared term
test$Eveness2 <- (test$Eveness-mean(test$Eveness))^2
test$bot_oxy2 <- (test$bot_oxy-mean(test$bot_oxy))^2
test$bot_oxy_var2 <- (test$bot_oxy_var-mean(test$bot_oxy_var))^2
test$bot_sal2 <- (test$bot_sal-mean(test$bot_sal))^2
test$depth2 <- (test$depth-mean(test$depth))^2
test$bot_sal_var2 <- (test$bot_sal_var-mean(test$bot_sal_var))^2

#Remove outlier
test <- test %>% dplyr::filter(FRic < 200)

# 2.2: EXPLORATORY SEM, only direct links of biotic variables --------------
# Single models ---------------------------------------------------
#FULL MODEL
system.time(gls1<- lme(log(NIS_relab) ~ 
                         NIS_Di_nw + log(FRic) + 
                         FEve + log(richness) + Eveness + Eveness2,
                       data = test, method = "REML", random = ~ 1 | year, na.action = "na.omit",
                       correlation=corGaus(form= ~ lon + lat))); #summary(gls1)

#saveRDS(gls1, "./Models/1st Models/FULL_model")

#Biotic variables affected by the environment
#Distinctiveness
gls2<- lme(NIS_Di_nw ~  bot_oxy + bot_oxy_var + bot_sal_var + bot_sal + bot_T + 
             depth +
             bot_oxy_var2 + bot_sal2 + depth2 + bot_oxy2, 
           method = "REML", na.action = "na.omit",
           data = test, random = ~ 1 | year, correlation=corGaus(form= ~ lon + lat))
#saveRDS(gls2, "./Models/1st Models/2ndlay_Di")

#Functional richness
gls3<-lme(log(FRic) ~  bot_oxy + bot_oxy_var + 
            bot_sal_var + bot_sal + bot_T + depth +
            bot_oxy_var2 + bot_sal2 + depth2 + bot_oxy2, 
          method = "REML", random = ~ 1 | year, na.action = "na.omit",
          data = test, correlation=corGaus(form= ~ lon + lat))
#saveRDS(gls3, "./Models/1st Models/2ndlay_FRic")

#Evenness
gls4<-lme(Eveness ~  bot_oxy_var + bot_oxy + 
            bot_sal_var + bot_sal + 
            bot_T + depth +
            bot_oxy_var2 + bot_sal2  + depth2 + bot_oxy2, 
          method = "REML", random = ~ 1 | year, na.action = "na.omit",
          data = test, correlation=corGaus(form= ~ lon + lat))
#saveRDS(gls4, "./Models/1st Models/2ndlay_Eve")

#Richness
gls5<-lme(log(richness) ~ bot_oxy + bot_oxy_var + bot_sal_var + 
            bot_sal + bot_T + depth +
            bot_oxy_var2 + bot_sal2 + depth2 + bot_oxy2, 
          method = "REML", random = ~ 1 | year, na.action = "na.omit",
          data = test, correlation=corGaus(form= ~ lon + lat))
#saveRDS(gls5, "./Models/1st Models/2ndlay_Rich")

#Functional Evenness
gls6<-lme(FEve ~ bot_oxy_var + bot_oxy + bot_sal_var + 
            bot_sal + bot_T + 
            depth +
            bot_oxy_var2 + bot_sal2 + depth2 + bot_oxy2, 
          method = "REML", random = ~ 1 | year, na.action = "na.omit",
          data = test, correlation=corGaus(form= ~ lon + lat))
#saveRDS(gls6, "./Models/1st Models/2ndlay_FEve")

# Fitting the initial SEM ------------------
model <- psem(
  gls1,
  gls2,
  gls3,
  gls4,
  gls5,
  gls6,
  log(FRic) %~~% Eveness2,
  FEve %~~% Eveness2,
  NIS_Di_nw %~~% Eveness2,
  log(richness) %~~% Eveness2,
  Eveness %~~% Eveness2,
  log(richness) %~~% log(FRic),
  FEve %~~% log(FRic), 
  Eveness  %~~% log(FRic),
  log(richness) %~~% FEve,
  Eveness %~~% FEve,
  Eveness %~~%  log(richness),
  log(richness) %~~% NIS_Di_nw,
  log(FRic) %~~% NIS_Di_nw,
  FEve %~~% NIS_Di_nw,
  Eveness  %~~% NIS_Di_nw
)
sum <- summary(model,.progressBar = T); sum
#saveRDS(sum, "./Models/SEM/Marenz_explor_SEM_sum")

# 2.3: FINAL SEM -------------
# Single models ---------
#FULL MODEL
system.time(gls1<- lme(log(NIS_relab) ~ 
                         NIS_Di_nw + log(FRic) + 
                         FEve + log(richness) + Eveness + Eveness2 + bot_oxy_var + bot_sal_var +
                         bot_sal + bot_T + depth + bot_sal2 + bot_oxy_var2  
                       + depth2 , 
                       data = test, method = "REML", random = ~ 1 | year, na.action = "na.omit",
                       correlation=corGaus(form= ~ lon + lat))); 

#saveRDS(gls1, "./Models/Final Models/FULL_model")

#Biotic variables affected by the environment
#Distinctiveness
gls2<- lme(NIS_Di_nw ~  bot_oxy + 
             bot_oxy_var + bot_sal_var + bot_sal + bot_T + 
             depth +
             bot_oxy_var2 + bot_sal2 + bot_oxy2  
           + depth2 , 
           method = "REML", na.action = "na.omit",
           data = test, random = ~ 1 | year, correlation=corGaus(form= ~ lon + lat))
#saveRDS(gls2, "./Models/Final Models/1stlay_Di")

#Functional richness
gls3<-lme(log(FRic) ~  bot_oxy + 
            bot_oxy_var + 
            bot_sal_var + bot_sal + bot_T + depth +
            bot_oxy_var2 + bot_sal2 + bot_oxy2  
          + depth2 , 
          method = "REML", random = ~ 1 | year, na.action = "na.omit",
          data = test, correlation=corGaus(form= ~ lon + lat))
#saveRDS(gls3, "./Models/Final Models/2ndlay_FRic")

#Evenness
gls4<-lme(Eveness ~  bot_oxy_var + bot_oxy + 
            bot_sal_var + bot_sal + 
            bot_T + depth +
            bot_oxy_var2 + bot_sal2 + bot_oxy2  
          + depth2, 
          method = "REML", random = ~ 1 | year, na.action = "na.omit",
          data = test, correlation=corGaus(form= ~ lon + lat))
#saveRDS(gls4, "./Models/Final Models/2ndlay_Eve")

#Richness
gls5<-lme(log(richness) ~ bot_oxy_var + bot_oxy + 
            bot_sal_var + bot_sal + 
            bot_T + depth +
            bot_oxy_var2 + bot_sal2 + bot_oxy2  
          + depth2, 
          method = "REML", random = ~ 1 | year, na.action = "na.omit",
          data = test, correlation=corGaus(form= ~ lon + lat))
#saveRDS(gls5, "./Models/Final Models/2ndlay_Rich")

gls6<-lme(FEve ~ bot_oxy_var + bot_oxy + 
            bot_sal_var + 
            bot_sal + bot_T + 
            depth +
            bot_oxy_var2 + bot_sal2 + bot_oxy2  
          + depth2 , 
          method = "REML", random = ~ 1 | year, na.action = "na.omit",
          data = test, correlation=corGaus(form= ~ lon + lat))
#saveRDS(gls5, "./Models/Final Models/2ndlay_FEve")

# Fitting the final SEM --------------
model <- psem(
  gls1,
  gls2,
  gls3,
  gls4,
  gls5,
  gls6,
  log(FRic) %~~% Eveness2,
  FEve %~~% Eveness2,
  NIS_Di_nw %~~% Eveness2,
  log(richness) %~~% Eveness2,
  Eveness %~~% Eveness2,
  log(richness) %~~% log(FRic),
  FEve %~~% log(FRic), 
  Eveness  %~~% log(FRic),
  log(richness) %~~% FEve,
  Eveness %~~% FEve,
  Eveness %~~%  log(richness),
  log(richness) %~~% NIS_Di_nw,
  log(FRic) %~~% NIS_Di_nw,
  FEve %~~% NIS_Di_nw,
  Eveness  %~~% NIS_Di_nw
)
sum <- summary(model,.progressBar = T); sum
#saveRDS(sum, "./Models/SEM/Marenz_SEM_final_sum")

# 2.4: MODEL DIAGNOSTICS ----------------------

### Visual model diagnostics
par(mfrow=c(2,2), mar=c(4,5,1,2))
# FULL model RESIDUALS
reg <- gls1
rawresiduals <- resid(reg, 'pearson')
hist(resid(reg))
#hist(resid(reg), xlim = c(-20,20), breaks=50, main='', xlab='Residuals', cex.lab=1.5, cex.axis=1.5)
plot( fitted(reg),resid(reg),
      col = "black", xlab = "Fitted class", ylab = "Residuals", cex.lab=1.5, cex.axis=1.5)
abline(h=0, lty='dashed')
qqnorm(resid(reg), main='' ,cex.lab=1.5, cex.axis=1.5)
qqline(resid(reg))

par(mfrow=c(2,2), mar=c(4,5,1,2))
# Distinctiveness RESIDUALS
reg <- gls2
rawresiduals <- resid(reg, 'pearson')
hist(resid(reg))
plot( fitted(reg),resid(reg),
      col = "black", xlab = "Fitted class", ylab = "Residuals", cex.lab=1.5, cex.axis=1.5)
abline(h=0, lty='dashed')
qqnorm(resid(reg), main='' ,cex.lab=1.5, cex.axis=1.5)
qqline(resid(reg))

par(mfrow=c(2,2), mar=c(4,5,1,2))
# FRic RESIDUALS
reg <- gls3
#rawresiduals <- resid(reg, 'pearson')
hist(resid(reg))
plot(fitted(reg),resid(reg),
     col = "black", xlab = "Fitted class", ylab = "Residuals", cex.lab=1.5, cex.axis=1.5)
abline(h=0, lty='dashed')
qqnorm(resid(reg), main='' ,cex.lab=1.5, cex.axis=1.5)
qqline(resid(reg))

par(mfrow=c(2,2), mar=c(4,5,1,2))
# EVENNESS RESIDUALS
reg <- gls4
rawresiduals <- resid(reg, 'pearson')
hist(resid(reg))
plot( fitted(reg),resid(reg),
      col = "black", xlab = "Fitted class", ylab = "Residuals", cex.lab=1.5, cex.axis=1.5)
abline(h=0, lty='dashed')
qqnorm(resid(reg), main='' ,cex.lab=1.5, cex.axis=1.5)
qqline(resid(reg))

par(mfrow=c(2,2), mar=c(4,5,1,2))
# Richness RESIDUALS
reg <- gls5
#rawresiduals <- resid(reg, 'pearson')
hist(resid(reg), xlim = c(-2,2), breaks=50, main='', xlab='Residuals', cex.lab=1.5, cex.axis=1.5)
plot(fitted(reg),resid(reg),
     col = "black", xlab = "Fitted class", ylab = "Residuals", cex.lab=1.5, cex.axis=1.5)
abline(h=0, lty='dashed')
qqnorm(resid(reg), main='' ,cex.lab=1.5, cex.axis=1.5)
qqline(resid(reg))

par(mfrow=c(2,2), mar=c(4,5,1,2))
# Functional Eveness RESIDUALS
reg <- gls6
#rawresiduals <- resid(reg, 'pearson')
hist(resid(reg), xlim = c(-2,2), breaks=50, main='', xlab='Residuals', cex.lab=1.5, cex.axis=1.5)
plot(fitted(reg),resid(reg),
     col = "black", xlab = "Fitted class", ylab = "Residuals", cex.lab=1.5, cex.axis=1.5)
abline(h=0, lty='dashed')
qqnorm(resid(reg), main='' ,cex.lab=1.5, cex.axis=1.5)
qqline(resid(reg))


# 2.5: PLOTTING THE SEM ----------
# Exploratory SEM (Fig. S1A) -----
#Create the needed objects
sum <- readRDS("./Models/SEM/Marenz_explor_SEM_sum")
factor_list <- list ("Response" = c("logNIS_relab"),
                     "1st layer"= c("NIS_Di_nw","logrichness","logFRic","Eveness","FEve"),
                     "2nd layer" = c("bot_oxy","bot_oxy_var","bot_sal","bot_T","depth","bot_sal_var"))

factor_list <- lapply(factor_list, sort)

quadratic <- c("Eveness2","bot_oxy2","bot_oxy_var2","bot_sal2","bot_sal_var2","depth2")
non.quadratic <- c("Eveness","bot_oxy","bot_oxy_var","bot_sal","bot_sal_var","depth")

#Obtain the output and change names
trial <- piecewiseSEM_ggplot(sum, factors_list = factor_list, quadratic, non.quadratic, pval = 0.05)

trial[[1]][,1:2] <- apply(trial[[1]][,1:2],2, function (x){x = str_replace_all(x, c("logNIS_relab" = "NIS relative biomass","NIS_Di_nw" = "Distinctiveness",
                                                                                    "logrichness" = "Richness","logFRic" = "Functional richness", "Eveness" = "Evenness",
                                                                                    "FEve" = "Functional Evenness", "bot_oxy" = "Bottom oxygen", "bot_oxy_var" = "Bottom oxygen variation",
                                                                                    "bot_sal" = "Bottom salinity","bot_T" = "Bottom temperature","depth" = "Depth","Eveness2" = "EvennessQ",
                                                                                    "bot_oxy2" = "Bottom oxygenQ",
                                                                                    "bot_oxy_var2" = "Bottom oxygen variationQ","bot_sal2" = "Bottom salinityQ","bot_sal_var2" = "Bottom salinity variationQ",
                                                                                    "depth2" = "DepthQ","bot_sal_var" = "Bottom salinity variation"))})

for(i in 2:length(trial)){
  trial[[i]][,1] = str_replace_all(trial[[i]][,1], c("logNIS_relab" = "NIS relative biomass","NIS_Di_nw" = "Distinctiveness",
                                                     "logrichness" = "Richness","logFRic" = "Functional richness", "Eveness" = "Evenness",
                                                     "FEve" = "Functional Evenness", "bot_oxy" = "Bottom oxygen", "bot_oxy_var" = "Bottom oxygen variation",
                                                     "bot_sal" = "Bottom salinity","bot_T" = "Bottom temperature","depth" = "Depth","Eveness2" = "EvennessQ",
                                                     "bot_oxy2" = "Bottom oxygenQ",
                                                     "bot_oxy_var2" = "Bottom oxygen variationQ","bot_sal2" = "Bottom salinityQ","bot_sal_var2" = "Bottom salinity variationQ",
                                                     "depth2" = "DepthQ","bot_sal_var" = "Bottom salinity variation"))
}

factor_list <- lapply(factor_list, function (x){x = str_replace_all(x, c("logNIS_relab" = "NIS relative biomass","NIS_Di_nw" = "Distinctiveness",
                                                                         "logrichness" = "Richness","logFRic" = "Functional richness", "Eveness" = "Evenness",
                                                                         "FEve" = "Functional Evenness", "bot_oxy" = "Bottom oxygen", "bot_oxy_var" = "Bottom oxygen variation",
                                                                         "bot_sal" = "Bottom salinity","bot_T" = "Bottom temperature","depth" = "Depth","Eveness2" = "EvennessQ",
                                                                         "bot_oxy2" = "Bottom oxygenQ",
                                                                         "bot_oxy_var2" = "Bottom oxygen variationQ","bot_sal2" = "Bottom salinityQ","bot_sal_var2" = "Bottom salinity variationQ",
                                                                         "depth2" = "DepthQ","bot_sal_var" = "Bottom salinity variation"))})

prova <- trial[[1]]; resp <- trial[[2]]; pred <- trial[[3]]; pnts <- trial[[4]]; quad <- trial[[5]]
prova = prova %>% mutate(direction = ifelse(Response == "NIS relative biomass", "direct", "indirect"),
                         effect_group = ifelse(layer == 2, "Environment", Predictor)) 

#Plot all the effects
p = ggplot(pnts, aes(x, y)) + geom_blank() + 
  theme_void() + #Comment this line if you would like to see the created axis
  geom_curve(prova[prova$quadratic == "non-quadratic" &
                     prova$direction == "indirect",],mapping = aes(x=xPred,y=yPred,xend=xResp,yend=yResp,
                                                                   linewidth = size, colour = as.factor(effect)), alpha = 1, curvature = -0.15)+
  scale_color_manual(values= c("gray70", "mistyrose")) +
  geom_curve(prova[prova$quadratic == "non-quadratic" &
                     prova$layer == "between",],mapping = aes(x=xPred,y=yPred,xend=xResp,yend=yResp,
                                                              linewidth = size), color = "lightsteelblue", curvature = -0.15)+
  geom_curve(prova[prova$layer == "2" & prova$quadratic == "non-quadratic" &
                     prova$Response == "NIS relative abundance",],mapping = aes(x=xPred,y=yPred,xend=xResp,yend=yResp,
                                                                                linewidth = size), color = "lightsteelblue", curvature = -0.15)+
  new_scale_color() +
  geom_curve(prova[prova$quadratic == "non-quadratic" & prova$layer == "1" &
                     prova$direction == "direct",],mapping = aes(x=xPred,y=yPred,xend=xResp,yend=yResp,
                                                                 linewidth = size, colour = as.factor(effect)), curvature = -0.15)+
  scale_color_manual(values= c("black", "darkred")) +
  geom_label(quad,mapping = aes(x = xPred, y = yPred, label = Predictor), fill = "green3") + #Add quadratic terms behind the non-quadratic in red
  geom_label(resp, mapping = aes(x = xResp, y = yResp, label = Response)) +
  geom_label(pred, mapping = aes(x = xPred, y = yPred, label = Predictor)) + 
  ylim(-0.25,6)+xlim(-0.25,16)+theme(legend.position="none"); p

#ggsave(file="./Plots/Marenz_explor_SEM.svg", plot=p, width=15, height=11, unit = "in")

# Final SEM (Fig. 3A) -----
sum <- readRDS("./Models/SEM/Marenz_final_SEM_sum")
factor_list <- list ("Response" = c("logNIS_relab"),
                     "1st layer"= c("NIS_Di_nw","logrichness","logFRic","Eveness","FEve"),
                     "2nd layer" = c("bot_oxy","bot_oxy_var","bot_sal","bot_T","depth","bot_sal_var"))

factor_list <- lapply(factor_list, sort)

quadratic <- c("Eveness2","bot_oxy2","bot_oxy_var2","bot_sal2","bot_sal_var2","depth2")
non.quadratic <- c("Eveness","bot_oxy","bot_oxy_var","bot_sal","bot_sal_var","depth")

#Obtain the output and change names
trial <- piecewiseSEM_ggplot(sum, factors_list = factor_list, quadratic, non.quadratic, pval = 0.05)

trial[[1]][,1:2] <- apply(trial[[1]][,1:2],2, function (x){x = str_replace_all(x, c("logNIS_relab" = "NIS relative biomass","NIS_Di_nw" = "Distinctiveness",
                                                                                    "logrichness" = "Richness","logFRic" = "Functional richness", "Eveness" = "Evenness",
                                                                                    "FEve" = "Functional Evenness", "bot_oxy" = "Bottom oxygen", "bot_oxy_var" = "Bottom oxygen variation",
                                                                                    "bot_sal" = "Bottom salinity","bot_T" = "Bottom temperature","depth" = "Depth","Eveness2" = "EvennessQ",
                                                                                    "bot_oxy2" = "Bottom oxygenQ",
                                                                                    "bot_oxy_var2" = "Bottom oxygen variationQ","bot_sal2" = "Bottom salinityQ","bot_sal_var2" = "Bottom salinity variationQ",
                                                                                    "depth2" = "DepthQ","bot_sal_var" = "Bottom salinity variation"))})

for(i in 2:length(trial)){
  trial[[i]][,1] = str_replace_all(trial[[i]][,1], c("logNIS_relab" = "NIS relative biomass","NIS_Di_nw" = "Distinctiveness",
                                                     "logrichness" = "Richness","logFRic" = "Functional richness", "Eveness" = "Evenness",
                                                     "FEve" = "Functional Evenness", "bot_oxy" = "Bottom oxygen", "bot_oxy_var" = "Bottom oxygen variation",
                                                     "bot_sal" = "Bottom salinity","bot_T" = "Bottom temperature","depth" = "Depth","Eveness2" = "EvennessQ",
                                                     "bot_oxy2" = "Bottom oxygenQ",
                                                     "bot_oxy_var2" = "Bottom oxygen variationQ","bot_sal2" = "Bottom salinityQ","bot_sal_var2" = "Bottom salinity variationQ",
                                                     "depth2" = "DepthQ","bot_sal_var" = "Bottom salinity variation"))
}

factor_list <- lapply(factor_list, function (x){x = str_replace_all(x, c("logNIS_relab" = "NIS relative biomass","NIS_Di_nw" = "Distinctiveness",
                                                                         "logrichness" = "Richness","logFRic" = "Functional richness", "Eveness" = "Evenness",
                                                                         "FEve" = "Functional Evenness", "bot_oxy" = "Bottom oxygen", "bot_oxy_var" = "Bottom oxygen variation",
                                                                         "bot_sal" = "Bottom salinity","bot_T" = "Bottom temperature","depth" = "Depth","Eveness2" = "EvennessQ",
                                                                         "bot_oxy2" = "Bottom oxygenQ",
                                                                         "bot_oxy_var2" = "Bottom oxygen variationQ","bot_sal2" = "Bottom salinityQ","bot_sal_var2" = "Bottom salinity variationQ",
                                                                         "depth2" = "DepthQ","bot_sal_var" = "Bottom salinity variation"))})

prova <- trial[[1]]; resp <- trial[[2]]; pred <- trial[[3]]; pnts <- trial[[4]]; quad <- trial[[5]]
prova = prova %>% mutate(direction = ifelse(Response == "NIS relative biomass", "direct", "indirect"),
                         effect_group = ifelse(layer == 2, "Environment", Predictor)) 

#Plot all the effects
p = ggplot(pnts, aes(x, y)) + geom_blank() + 
  theme_void() + #Comment this line if you would like to see the created axis
  geom_curve(prova[prova$quadratic == "non-quadratic" &
                     prova$direction == "indirect",],mapping = aes(x=xPred,y=yPred,xend=xResp,yend=yResp,
                                                                   linewidth = size, colour = as.factor(effect)), alpha = 1, curvature = -0.15)+
  scale_color_manual(values= c("gray70", "mistyrose")) +
  geom_curve(prova[prova$quadratic == "non-quadratic" &
                     prova$layer == "between",],mapping = aes(x=xPred,y=yPred,xend=xResp,yend=yResp,
                                                              linewidth = size), color = "lightsteelblue", curvature = -0.15)+
  new_scale_color() +
  geom_curve(prova[prova$quadratic == "non-quadratic" &
                     prova$direction == "direct",],mapping = aes(x=xPred,y=yPred,xend=xResp,yend=yResp,
                                                                 linewidth = size, colour = as.factor(effect)), curvature = -0.15)+
  scale_color_manual(values= c("black", "darkred")) +
  geom_label(quad,mapping = aes(x = xPred, y = yPred, label = Predictor), fill = "green3") + #Add quadratic terms behind the non-quadratic in red
  geom_label(resp, mapping = aes(x = xResp, y = yResp, label = Response)) +
  geom_label(pred, mapping = aes(x = xPred, y = yPred, label = Predictor)) + 
  ylim(-0.25,6)+xlim(-0.25,16)+theme(legend.position="none"); p

#ggsave(file="./Plots/Marenz_final_SEM.svg", plot=p, width=15, height=11, units = "in")