#######################################!
#3: SEM analysis for Round goby #######!
#######################################!
remove(list=ls())
#Load libraries and functions
source("./Code/Libraries_Functions.R")

# 3.1: DATA EXPLORATION --------------------------------------------------------
test = read.csv("./Data/round_goby_data_metrics.txt",header=T,dec=".",sep="\t", check.names = FALSE)

#Multicolinearity of variables -- dominance
variables <- test %>% select(NIS_Di_nw, richness,Eveness,FRic,FEve,bot_t_var,bot_sal_var,bot_oxy_var, 
                             chl_var, depth,bot_sal,bot_oxy,bot_t, dist_offshore)
chart.Correlation(variables, histogram=TRUE, pch=19)

#Variance inflation factors
variables <- test %>% select(NIS_relab,NIS_Di_nw,richness, Eveness,FRic,FEve, bot_t_var,bot_sal_var,bot_oxy_var, chl,
                             chl_var, depth,bot_sal,bot_oxy,bot_t, dist_offshore)

vif <- lm(NIS_relab ~ ., variables); summary (vif)

vif_values <- data.frame(vars = names(vif(vif)), vif = vif(vif)); sort(vif(vif))

#Fig. S2B

vif_plot_RG = ggplot(vif_values, aes(x = vars, y = vif)) + 
  geom_bar(stat = "identity", color = "steelblue", fill = "steelblue") + #ylim(0, 10) +
  coord_flip() + scale_y_continuous(labels = function(x) format(x, nsmall = 1)) + 
  labs(x = "Variables", y = "VIF values") + geom_hline(yintercept = 5, color = "darkred", size = 1) + 
  theme_minimal(); vif_plot_RG

#ggsave(file="VIF_RG.svg", plot=vif_plot_RG, width=7.5, height=5.5)

# test for spatial autocorrelation
autocorr_test = test
data.spatialCor.lm <- lm(NIS_Di ~ 1, autocorr_test)
autocorr_test$Resid <- rstandard(data.spatialCor.lm)
coordinates(autocorr_test) <- ~lon + lat  #effectively convert the data into a spatial data frame
bubble(autocorr_test, "Resid")

# Skewness of data and transformations
par(mfrow=c(1,1),mar=c(5,4,2,2))
hist(test$NIS_relab) #histogram to see data distribution
skewness(test$NIS_relab, na.rm = TRUE)

# Gam for testing linearity of the responses, to see if we need to include a quadratic term 
gam<- gam(log(NIS_relab)~ 
               s(NIS_Di_nw, k = 3) +
               s(Eveness, k = 3) +
               s(FRic, k = 3) +
               s(FEve, k = 3) +
               s(depth, k = 3) + 
               s(bot_t, k = 3) +
               s(bot_t_var, k = 3)+
               s(bot_sal, k = 3) +
               s(bot_sal_var, k = 3) +
               s(bot_oxy, k = 3) + 
               s(chl, k = 3) +
               s(chl_var, k = 3) +
               s(dist_offshore, k = 3) + 
               s(Sample, by = time_step, bs = 're') +
               s(tool, bs = 're'),
             na.action = "na.omit",
             #family = Gamma(link = "log"),
             family = gaussian(link = "identity"), 
             niterPQL=20,
             #correlation = corGaus(form = ~ lon + lat|time_step), # Can try other error structures
             #control=lmeControl(opt="optim", msMaxIter = 150),
             data = test)

summary(gam)
par(mfrow=c(2,2),mar=c(2,4,0,2,0.2))
plot(gam, shade=T, shade.col="grey",rug=F,res=T)

# Squared terms 
test$Evenessq <- (test$Eveness-mean(test$Eveness))^2
test$bot_salq <- (test$bot_sal-mean(test$bot_sal))^2
test$dist_offshoreq <- (test$dist_offshore-mean(test$dist_offshore))^2

# 3.2: EXPLORATORY SEM, only direct links of biotic variables -----------
# Single models ------
#FULL MODEL
system.time(gls1<- lme(log(NIS_relab) ~ 
                         NIS_Di_nw + log(FRic) + FEve + Eveness,
                       random = list(year = ~ 1, Sample = ~ 1, tool = ~ 1), 
                       method = "REML", na.action = "na.fail", data = test)) 

#saveRDS(gls1, "./Models/1st Models/FULL_model")

#Biotic variables affected by the environment
#Distinctiveness
gls2<- lme(NIS_Di_nw ~  bot_oxy + bot_sal + 
             bot_t + 
             #dist_offshoreq + 
             depth + chl_var + dist_offshore, 
           random = list(year = ~ 1, Sample = ~ 1, tool = ~ 1), method = "REML", na.action = "na.omit", data = test)

#saveRDS(gls2, "./Models/1st Models/2ndlay_Di")

#Evenness
gls3<-lme(Eveness ~   bot_oxy + bot_sal + 
            bot_t + 
            #dist_offshoreq + 
            depth + chl_var + dist_offshore, 
          random = list(year = ~ 1, Sample = ~ 1, tool = ~ 1), method = "REML", na.action = "na.omit", data = test)

#saveRDS(gls3, "./Models/1st Models/2ndlay_Eve")

#Functional Evenness
gls4<-lme(FEve ~ bot_oxy + bot_sal + 
            bot_t + 
            #dist_offshoreq + 
            depth + chl_var + dist_offshore, 
          random = list(year = ~ 1, Sample = ~ 1, tool = ~ 1), method = "REML", na.action = "na.omit", data = test)

#saveRDS(gls4, "./Models/1st Models/2ndlay_FEve")

#Functional richness
gls5<-lme(log(FRic) ~ bot_oxy + bot_sal + 
            bot_t + 
            #dist_offshoreq + 
            depth + chl_var + dist_offshore, 
          random = list(year = ~ 1, Sample = ~ 1, tool = ~ 1), method = "REML", na.action = "na.omit", data = test)

#saveRDS(gls5, "./Models/1st Models/2ndlay_FRic")


# Fitting the initial SEM ------------------
model <- psem(
  gls1,
  gls2,
  gls3,
  gls4,
  gls5,
  log(FRic) %~~% Eveness,
  #FEve %~~% Eveness,
  log(FRic) %~~% NIS_Di_nw,
  #Eveness  %~~% NIS_Di_nw,
  FEve %~~% NIS_Di_nw,
  log(FRic) %~~% log(richness),
  FEve %~~% log(FRic),
  data = test)

sum <- summary(model,.progressBar = T, direction = c(), conserve = T); sum
#saveRDS(sum, "./Models/SEM/RG_explor_SEM_sum")

# 3.3: FINAL SEM ---------
# Single models -------
#FULL MODEL
system.time(gls1<- lme(log(NIS_relab) ~ 
                         NIS_Di_nw + 
                         log(FRic) + FEve + Eveness + 
                         bot_oxy + bot_t +
                         #chl_var + dist_offshore +
                         #dist_offshoreq +
                         depth,
                       random = list(year = ~ 1, Sample = ~ 1, tool = ~ 1), 
                       method = "REML", na.action = "na.fail", data = test))

#saveRDS(gls1, "./Models/Final Models/FULL_model")

#Biotic variables affected by the environment
#Distinctiveness
gls2<- lme(NIS_Di_nw ~  bot_oxy + bot_sal + 
             bot_t + 
             #dist_offshoreq + 
             depth + chl_var + dist_offshore, 
           random = list(year = ~ 1, Sample = ~ 1, tool = ~ 1), method = "REML", na.action = "na.omit", data = test)

#saveRDS(gls2, "./Models/Final Models/2ndlay_Di")

#Evenness
gls3<-lme(Eveness ~   bot_oxy + bot_sal + 
            bot_t + 
            #dist_offshoreq + 
            depth + chl_var + dist_offshore, 
          random = list(year = ~ 1, Sample = ~ 1, tool = ~ 1), method = "REML", na.action = "na.omit", data = test)

#saveRDS(gls3, "./Models/Final Models/2ndlay_Eve")

#Functional Evenness
gls4<-lme(FEve ~ bot_oxy + bot_sal + 
            bot_t + 
            #dist_offshoreq + 
            depth + chl_var + dist_offshore, 
          random = list(year = ~ 1, Sample = ~ 1, tool = ~ 1), method = "REML", na.action = "na.omit", data = test)

#saveRDS(gls4, "./Models/Final Models/2ndlay_FEve")

#Functional richness
gls5<-lme(log(FRic) ~ bot_oxy + bot_sal + 
            bot_t + 
            #dist_offshoreq + 
            depth + chl_var + dist_offshore, 
          random = list(year = ~ 1, Sample = ~ 1, tool = ~ 1), method = "REML", na.action = "na.omit", data = test)

#saveRDS(gls5, "./Models/Final Models/2ndlay_FRic")


# Fitting the final SEM --------------
model <- psem(
  gls1,
  gls2,
  gls3,
  gls4,
  gls5,
  log(FRic) %~~% Eveness,
  #FEve %~~% Eveness,
  log(FRic) %~~% NIS_Di_nw,
  #Eveness  %~~% NIS_Di_nw,
  FEve %~~% NIS_Di_nw,
  FEve %~~% log(FRic),
  Eveness %~~% log(FRic),
  #Eveness %~~% FEve,
  data = test)

system.time(sum <- summary(model,.progressBar = T, direction = c(), conserve = T)); sum
#saveRDS(sum, "./Models/SEM/RG_SEM_final_sum")

# 3.4: MODEL DIAGNOSTICS --------------------- 
### Visual model diagnostics
windows(100,110)
#par(mfrow=c(5,3), mar=c(4,5,1,2))

# FULL model RESIDUALS
par(mfrow=c(2,2), mar=c(4,5,1,2))
reg <- gls1
rawresiduals <- resid(reg, 'pearson')
hist(resid(reg), main='', xlab='Residuals', cex.lab=1.5, cex.axis=1.5)
#hist(resid(reg), xlim = c(-20,20), breaks=50, main='', xlab='Residuals', cex.lab=1.5, cex.axis=1.5)
plot(fitted(reg),resid(reg),
     col = "black", xlab = "Fitted class", ylab = "Residuals", cex.lab=1.5, cex.axis=1.5)
abline(h=0, lty='dashed')
qqnorm(resid(reg), main='' ,cex.lab=1.5, cex.axis=1.5)
qqline(resid(reg))

# Distinctiveness RESIDUALS
reg <- gls2
rawresiduals <- resid(reg, 'pearson')
hist(resid(reg), main='', xlab='Residuals', cex.lab=1.5, cex.axis=1.5)
plot( fitted(reg),resid(reg),
      col = "black", xlab = "Fitted class", ylab = "Residuals", cex.lab=1.5, cex.axis=1.5)
abline(h=0, lty='dashed')
qqnorm(resid(reg), main='' ,cex.lab=1.5, cex.axis=1.5)
qqline(resid(reg))

# EVENNESS RESIDUALS
reg <- gls3
rawresiduals <- resid(reg, 'pearson')
hist(resid(reg), main='', xlab='Residuals', cex.lab=1.5, cex.axis=1.5)
plot( fitted(reg),resid(reg),
      col = "black", xlab = "Fitted class", ylab = "Residuals", cex.lab=1.5, cex.axis=1.5)
abline(h=0, lty='dashed')
qqnorm(resid(reg), main='' ,cex.lab=1.5, cex.axis=1.5)
qqline(resid(reg))

# Functional Eveness RESIDUALS
reg <- gls4
rawresiduals <- resid(reg, 'pearson')
hist(resid(reg), main='', xlab='Residuals', cex.lab=1.5, cex.axis=1.5)
plot( fitted(reg),resid(reg),
      col = "black", xlab = "Fitted class", ylab = "Residuals", cex.lab=1.5, cex.axis=1.5)
abline(h=0, lty='dashed')
qqnorm(resid(reg), main='' ,cex.lab=1.5, cex.axis=1.5)
qqline(resid(reg))  

# FRic RESIDUALS
reg <- gls5
rawresiduals <- resid(reg, 'pearson')
hist(resid(reg), main='', xlab='Residuals', cex.lab=1.5, cex.axis=1.5)
plot( fitted(reg),resid(reg),
      col = "black", xlab = "Fitted class", ylab = "Residuals", cex.lab=1.5, cex.axis=1.5)
abline(h=0, lty='dashed')
qqnorm(resid(reg), main='' ,cex.lab=1.5, cex.axis=1.5)
qqline(resid(reg))


# 3.5: PLOTTING THE SEM ----------
# Exploratory SEM (Fig. S1B) -----

#Create the needed objects for the function
sum <- readRDS("./Models/SEM/RG_explor_SEM_sum")
factor_list <- list ("Response" = c("logNIS_relab"),
                     "1st layer"= c("NIS_Di_nw","logrichness","logFRic","Eveness","FEve"),
                     "2nd layer" = c("bot_oxy","bot_sal","bot_t","depth", "dist_offshore", "chl_var"))

factor_list <- lapply(factor_list, sort)

quadratic <- c("dist_offshoreq")
non.quadratic <- c("dist_offshore")

#Obtain the output and change names
trial <- piecewiseSEM_ggplot(sum, factors_list = factor_list, quadratic, non.quadratic, pval = 0.05)

trial[[1]][,1:2] <- apply(trial[[1]][,1:2],2, function (x){x = str_replace_all(x, c("logNIS_relab" = "NIS relative biomass","NIS_Di_nw" = "Distinctiveness",
                                                                                    "logrichness" = "Richness","logFRic" = "Functional richness", "Eveness" = "Evenness",
                                                                                    "FEve" = "Functional Evenness", "bot_oxy" = "Bottom oxygen", "bot_oxy_var" = "Bottom oxygen variation",
                                                                                    "bot_sal" = "Bottom salinity","bot_t" = "Bottom temperature","depth" = "Depth","dist_offshore" = "Exposure", 
                                                                                    "dist_offshoreq" = "ExposureQ", "bot_sal_var" = "Bottom salinity variation", "chl_var" = "Chlorophyll variation",
                                                                                    "bot_t_var" = "Bottom temperature variation",
                                                                                    "bot_salq" = "Bottom salinityQ"))})

for(i in 2:length(trial)){
  trial[[i]][,1] = str_replace_all(trial[[i]][,1], c("logNIS_relab" = "NIS relative biomass","NIS_Di_nw" = "Distinctiveness",
                                                     "logrichness" = "Richness","logFRic" = "Functional richness", "Eveness" = "Evenness",
                                                     "FEve" = "Functional Evenness", "bot_oxy" = "Bottom oxygen", "bot_oxy_var" = "Bottom oxygen variation",
                                                     "bot_sal" = "Bottom salinity","bot_t" = "Bottom temperature","depth" = "Depth","dist_offshore" = "Exposure", 
                                                     "dist_offshoreq" = "ExposureQ", "bot_sal_var" = "Bottom salinity variation", "chl_var" = "Chlorophyll variation",
                                                     "bot_t_var" = "Bottom temperature variation",
                                                     "bot_salq" = "Bottom salinityQ"))
}

factor_list <- lapply(factor_list, function (x){x = str_replace_all(x, c("logNIS_relab" = "NIS relative biomass","NIS_Di_nw" = "Distinctiveness",
                                                                         "logrichness" = "Richness","logFRic" = "Functional richness", "Eveness" = "Evenness",
                                                                         "FEve" = "Functional Evenness", "bot_oxy" = "Bottom oxygen", "bot_oxy_var" = "Bottom oxygen variation",
                                                                         "bot_sal" = "Bottom salinity","bot_t" = "Bottom temperature","depth" = "Depth","dist_offshore" = "Exposure", 
                                                                         "dist_offshoreq" = "ExposureQ", "bot_sal_var" = "Bottom salinity variation", "chl_var" = "Chlorophyll variation",
                                                                         "bot_t_var" = "Bottom temperature variation",
                                                                         "bot_salq" = "Bottom salinityQ"))})

prova <- trial[[1]]; resp <- trial[[2]]; pred <- trial[[3]]; pnts <- trial[[4]]; quad <- trial[[5]]

prova = prova %>% mutate(direction = ifelse(Response == "NIS relative biomass", "direct", "indirect"),
                         effect_group = ifelse(layer == 2, "Environment", Predictor))

#Plot all the effects 
p = ggplot(pnts, aes(x, y)) + geom_blank() + 
  theme_void() + #Comment this line if you would like to see the created axis
  geom_curve(prova[prova$quadratic == "non-quadratic" &
                     prova$direction == "indirect",],
             mapping = aes(x=xPred,y=yPred,xend=xResp,yend=yResp,
                           linewidth = size, colour = as.factor(effect)), alpha = 1, curvature = -0.15) +
  scale_color_manual(values= c("gray70", "mistyrose")) +
  geom_curve(prova[prova$quadratic == "non-quadratic" &
                     prova$layer == "between",],mapping = aes(x=xPred,y=yPred,xend=xResp,yend=yResp,
                                                              linewidth = size), color = "lightsteelblue", curvature = -0.15)+
  geom_curve(prova[prova$layer == "2" &
                     prova$Response == "NIS relative biomass",],mapping = aes(x=xPred,y=yPred,xend=xResp,yend=yResp,
                                                                              linewidth = size), color = "lightsteelblue",curvature = -0.15)+
  new_scale_color() +
  geom_curve(prova[prova$quadratic == "non-quadratic" & 
                     prova$Response == "NIS relative biomass",],mapping = aes(x=xPred,y=yPred,xend=xResp,yend=yResp,
                                                                              linewidth = size, colour = as.factor(effect)), curvature = -0.15)+
  scale_color_manual(values= c("black", "darkred")) +
  geom_label(quad,mapping = aes(x = xPred, y = yPred, label = Predictor), fill = "green3") + #Add quadratic terms behind the non-quadratic in red
  geom_label(resp, mapping = aes(x = xResp, y = yResp, label = Response)) +
  geom_label(pred, mapping = aes(x = xPred, y = yPred, label = Predictor)) + 
  ylim(-0.25,6)+xlim(-0.25,16)+theme(legend.position="none"); p

#ggsave(file="./Plots/RG_explor_SEM.svg", plot=p, width=15, height=11, unit = "in")


# Final SEM (Fig. 3B) ------
#Create the needed objects for the function
sum <- readRDS("./Models/SEM/RG_SEM_final_sum")
factor_list <- list ("Response" = c("logNIS_relab"),
                     "1st layer"= c("NIS_Di_nw","logrichness","logFRic","Eveness","FEve"),
                     "2nd layer" = c("bot_oxy","bot_sal","bot_t","depth", "dist_offshore", "chl_var"))

factor_list <- lapply(factor_list, sort)

quadratic <- c("dist_offshoreq")
non.quadratic <- c("dist_offshore")

#Obtain the output and change names
trial <- piecewiseSEM_ggplot(sum, factors_list = factor_list, quadratic, non.quadratic, pval = 0.05)

trial[[1]][,1:2] <- apply(trial[[1]][,1:2],2, function (x){x = str_replace_all(x, c("logNIS_relab" = "NIS relative biomass","NIS_Di_nw" = "Distinctiveness",
                                                                                    "logrichness" = "Richness","logFRic" = "Functional richness", "Eveness" = "Evenness",
                                                                                    "FEve" = "Functional Evenness", "bot_oxy" = "Bottom oxygen", "bot_oxy_var" = "Bottom oxygen variation",
                                                                                    "bot_sal" = "Bottom salinity","bot_t" = "Bottom temperature","depth" = "Depth","dist_offshore" = "Exposure", 
                                                                                    "dist_offshoreq" = "ExposureQ", "bot_sal_var" = "Bottom salinity variation", "chl_var" = "Chlorophyll variation",
                                                                                    "bot_t_var" = "Bottom temperature variation",
                                                                                    "bot_salq" = "Bottom salinityQ"))})

for(i in 2:length(trial)){
  trial[[i]][,1] = str_replace_all(trial[[i]][,1], c("logNIS_relab" = "NIS relative biomass","NIS_Di_nw" = "Distinctiveness",
                                                     "logrichness" = "Richness","logFRic" = "Functional richness", "Eveness" = "Evenness",
                                                     "FEve" = "Functional Evenness", "bot_oxy" = "Bottom oxygen", "bot_oxy_var" = "Bottom oxygen variation",
                                                     "bot_sal" = "Bottom salinity","bot_t" = "Bottom temperature","depth" = "Depth","dist_offshore" = "Exposure", 
                                                     "dist_offshoreq" = "ExposureQ", "bot_sal_var" = "Bottom salinity variation", "chl_var" = "Chlorophyll variation",
                                                     "bot_t_var" = "Bottom temperature variation",
                                                     "bot_salq" = "Bottom salinityQ"))
}

factor_list <- lapply(factor_list, function (x){x = str_replace_all(x, c("logNIS_relab" = "NIS relative biomass","NIS_Di_nw" = "Distinctiveness",
                                                                         "logrichness" = "Richness","logFRic" = "Functional richness", "Eveness" = "Evenness",
                                                                         "FEve" = "Functional Evenness", "bot_oxy" = "Bottom oxygen", "bot_oxy_var" = "Bottom oxygen variation",
                                                                         "bot_sal" = "Bottom salinity","bot_t" = "Bottom temperature","depth" = "Depth","dist_offshore" = "Exposure", 
                                                                         "dist_offshoreq" = "ExposureQ", "bot_sal_var" = "Bottom salinity variation", "chl_var" = "Chlorophyll variation",
                                                                         "bot_t_var" = "Bottom temperature variation",
                                                                         "bot_salq" = "Bottom salinityQ"))})

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
                     prova$Response == "NIS relative biomass",],mapping = aes(x=xPred,y=yPred,xend=xResp,yend=yResp,
                                                                              linewidth = size, colour = as.factor(effect)), curvature = -0.15)+
  scale_color_manual(values= c("black", "darkred")) +
  geom_label(quad,mapping = aes(x = xPred, y = yPred, label = Predictor), fill = "green3") + #Add quadratic terms behind the non-quadratic in red
  geom_label(resp, mapping = aes(x = xResp, y = yResp, label = Response)) +
  geom_label(pred, mapping = aes(x = xPred, y = yPred, label = Predictor)) + 
  ylim(-0.25,6)+xlim(-0.25,16)+theme(legend.position="none"); p

#ggsave(file="./Plots/RG_final_SEM.svg", plot=p, width=15, height=11, unit = "in")