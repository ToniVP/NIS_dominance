###########################################!
#5: Figure 5; Partial effects plot  #######!
###########################################!
remove(list=ls())
#Load libraries and functions
source("./Code/Libraries_Functions.R")

# 5.1: Partial effects plot for Marenzelleria -------
test = read.csv("./Data/Marenzelleria_data_metrics.txt",header=T,dec=".",sep="\t", check.names = FALSE)

#Quadratic terms
test$Eveness2 <- (test$Eveness-mean(test$Eveness))^2
test$bot_oxy2 <- (test$bot_oxy-mean(test$bot_oxy))^2
test$bot_oxy_var2 <- (test$bot_oxy_var-mean(test$bot_oxy_var))^2
test$bot_sal2 <- (test$bot_sal-mean(test$bot_sal))^2
test$depth2 <- (test$depth-mean(test$depth))^2
test$bot_sal_var2 <- (test$bot_sal_var-mean(test$bot_sal_var))^2

#Remove outlier
test <- test %>% dplyr::filter(FRic < 200); test_Mar = test

#Define the Full model (lmer) in this case
system.time(gls1<- lmer(log(NIS_relab) ~ 
                          NIS_Di_nw + log(FRic) + 
                          FEve + log(richness) + Eveness + Eveness2 + bot_oxy_var + bot_sal_var +
                          bot_sal + bot_T + depth + bot_sal2 + bot_oxy_var2  
                        + depth2 +
                          (1|station) + (1|year),
                        REML = T, na.action = "na.omit", data = test))

#Partial plot for biotic and abiotic variables without a squared term - loop
variables = c("NIS_Di_nw","richness", "FRic", "FEve", "bot_T"); partial.plot.noq = data.frame()

for(i in 1:length(variables)){
  
  var1 <- seq(min(test[,which(colnames(test) == variables[i])]),
              max(test[,which(colnames(test) == variables[i])]), length = 300)
  
  vars <- variables[-i]
  
  var2 = rep(mean(test[,which(colnames(test) == vars[1])]),length(var1))
  var3 = rep(mean(test[,which(colnames(test) == vars[2])]),length(var1))
  var4 = rep(mean(test[,which(colnames(test) == vars[3])]), length(var1))
  var5 = rep(mean(test[,which(colnames(test) == vars[4])]), length(var1))
  
  station = rep(NA,length(var1)); year = rep(NA,length(var1)) # set the random effect to zero
  
  # get the mean of other variables as constant
  Eveness = rep(mean(test$Eveness),length(var1))
  Eveness2 = rep(mean(test$Eveness2),length(var1))
  #NIS_Di_nw = rep(mean(test$NIS_Di_nw),length(var1))
  #bot_oxy = rep(mean(test$bot_oxy),length(var1))
  #bot_oxy2 = rep(mean(test$bot_oxy2),length(var1))
  bot_oxy_var = rep(mean(test$bot_oxy_var),length(var1))
  bot_oxy_var2 = rep(mean(test$bot_oxy_var2),length(var1))
  bot_sal = rep(mean(test$bot_sal),length(var1))
  bot_sal2 = rep(mean(test$bot_sal2),length(var1))
  bot_sal_var = rep(mean(test$bot_sal_var),length(var1))
  bot_sal_var2 = rep(mean(test$bot_sal_var2),length(var1))
  #bot_T = rep(mean(test$bot_T),length(var1))
  depth = rep(mean(test$depth),length(var1))
  depth2 = rep(mean(test$depth2),length(var1))
  
  newdat <- data.frame(var1, var2, var3, Eveness, Eveness2, var4, var5, 
                       #NIS_Di_nw,
                       station, year, # set the random effect to zero
                       bot_sal,bot_sal_var,depth,
                       bot_sal2, depth2)
  
  colnames(newdat)[1:5] <- c(variables[i], vars)
  
  rel_abund_NIS <- stats::predict(gls1, newdata = newdat, re.form = NA) # predict with new data
  #rel_abund_NIS <- 10^rel_abund_NIS
  
  # plot
  #plot(rel_abund_NIS~newdat[,1],col="blue",lwd=2,type="l")
  
  # calculate the 95% confidence interval
  pred <- bootMer(gls1, function(x) predict(x, newdata = newdat, re.form = NA), nsim = 100)
  CI.lower = apply(pred$t, 2, function(x) as.numeric(quantile(x, probs=.025, na.rm=TRUE)))
  CI.upper = apply(pred$t, 2, function(x) as.numeric(quantile(x, probs=.975, na.rm=TRUE)))
  #lines(CI.upper~var1,lty=2,col="blue")
  #lines(CI.lower~var1,lty=2,col="blue")
  
  partial<- data.frame(rel_abund_NIS, variable = var1 ,CI.lower, CI.upper)
  partial<- partial %>% mutate(species = "Marenzelleria", variable = variables[i], x_axis = var1)
  
  partial.plot.noq = rbind(partial.plot.noq, partial)
  
}

#Partial plot for Evenness plus environmental variables and its quadratic term - loop
sum_lmer = summary(gls1); coeff = as.data.frame(sum_lmer$coefficients)
quad <- function(x){y = coeff[rownames(coeff) == "(Intercept)", "Estimate"] + coeff[rownames(coeff) == var, "Estimate"]*x + 
  coeff[rownames(coeff) == paste(var, "2", sep = ""), "Estimate"]*x^2}

variables = c("Eveness", "bot_oxy_var", "bot_sal","depth"); partial.plot.q = data.frame()

for(i in 1:length(variables)){
  var = variables[i]
  x = seq(min(test[,var]), max(test[,var]), length = 300)
  
  rel_abund_NIS <- sapply(x, quad)
  
  #plot(rel_abund_NIS ~ x,col="blue",lwd=2,type="l")
  
  summary(gls1)
  
  CI.lower = sapply(x,function(x) y = (coeff[rownames(coeff) == "(Intercept)", "Estimate"] - coeff[rownames(coeff) == "(Intercept)", "Std. Error"]) + 
                      (coeff[rownames(coeff) == var, "Estimate"] - coeff[rownames(coeff) == var, "Std. Error"])*x + 
                      (coeff[rownames(coeff) == paste(var, "2", sep = ""), "Estimate"]-coeff[rownames(coeff) == paste(var, "2", sep = ""), "Std. Error"])*x^2)
  
  CI.upper = sapply(x, function(x) y = (coeff[rownames(coeff) == "(Intercept)", "Estimate"] + coeff[rownames(coeff) == "(Intercept)", "Std. Error"]) + 
                      (coeff[rownames(coeff) == var, "Estimate"] + coeff[rownames(coeff) == var, "Std. Error"])*x + 
                      (coeff[rownames(coeff) == paste(var, "2", sep = ""), "Estimate"] + coeff[rownames(coeff) == paste(var, "2", sep = ""), "Std. Error"])*x^2)
  
  #lines(CI.upper~ x,lty=2,col="blue")
  #lines(CI.lower~ x,lty=2,col="blue")
  
  partial.x <- data.frame(rel_abund_NIS, variable = var, CI.lower, CI.upper, species = "Marenzelleria", x_axis = x)
  partial.plot.q <- rbind(partial.plot.q, partial.x)
  
}

partial.plot.noq = partial.plot.noq %>% dplyr::filter(variable %in% c("NIS_Di_nw","richness", "bot_T")) #only significant variables in the SEM
partial.plot <- rbind(partial.plot.q, partial.plot.noq)
partial.plot[which(partial.plot$variable == "bot_T"), "variable"] = "bot_t" #Change names to merge with round goby

#write.table(partial.plot,file="./Data/partial_plot_Marenzelleria.txt",sep="\t", row.names = TRUE)

# 5.2: Partial effects plot for Round goby -----------
test = read.csv("./Data/round_goby_data_metrics.txt",header=T,dec=".",sep="\t", check.names = FALSE)

#Quadratic terms
test$Evenessq <- (test$Eveness-mean(test$Eveness))^2
test$bot_salq <- (test$bot_sal-mean(test$bot_sal))^2
test$dist_offshoreq <- (test$dist_offshore-mean(test$dist_offshore))^2
test_RG = test

#Define the Full model (lmer) in this case
system.time(gls1 <- lmer(log(NIS_relab) ~ 
                           NIS_Di_nw + log(FRic) + FEve + Eveness + 
                           bot_oxy + bot_t + depth +
                           (1|tool) + (1|Sample:year), REML = T, 
                         na.action = "na.omit", data = test)); summary(gls1)

#Partial plot for all variables (no significant quadratic terms in this case)- loop
variables = c("NIS_Di_nw","FRic", "Eveness", "bot_t", "bot_oxy"); partial.plot.noq = data.frame()

for(i in 1:length(variables)){
  
  var1 <- seq(min(test[,which(colnames(test) == variables[i])]),
              max(test[,which(colnames(test) == variables[i])]), length = 300)
  
  vars <- variables[-i]
  
  var2 = rep(mean(test[,which(colnames(test) == vars[1])]),length(var1))
  var3 = rep(mean(test[,which(colnames(test) == vars[2])]),length(var1))
  var4 = rep(mean(test[,which(colnames(test) == vars[3])]), length(var1))
  var5 = rep(mean(test[,which(colnames(test) == vars[4])]), length(var1))
  
  station = rep(NA,length(var1)); year = rep(NA,length(var1)) # set the random effect to zero
  
  # get the mean of other variables as constant
  FEve = rep(mean(test$FEve),length(var1))
  depth = rep(mean(test$depth),length(var1))
  
  newdat <- data.frame(var1, var2, var3,var4, var5,
                       station, year, # set the random effect to zero
                       depth, FEve)
  
  colnames(newdat)[1:5] <- c(variables[i], vars)
  
  rel_abund_NIS <- stats::predict(gls1, newdata = newdat, re.form = NA) # predict with new data
  
  # plot
  #plot(rel_abund_NIS~newdat[,1],col="blue",lwd=2,type="l")
  
  # calculate the 95% confidence interval
  pred <- bootMer(gls1, function(x) predict(x, newdata = newdat, re.form = NA), nsim = 100)
  CI.lower = apply(pred$t, 2, function(x) as.numeric(quantile(x, probs=.025, na.rm=TRUE)))
  CI.upper = apply(pred$t, 2, function(x) as.numeric(quantile(x, probs=.975, na.rm=TRUE)))
  # lines(CI.upper~var1,lty=2,col="blue")
  # lines(CI.lower~var1,lty=2,col="blue")
  
  partial<- data.frame(rel_abund_NIS, variable = var1 ,CI.lower, CI.upper)
  partial<- partial %>% mutate(species = "Round goby", variable = variables[i], x_axis = var1)
  
  partial.plot.noq = rbind(partial.plot.noq, partial)
  
}

partial.plot <- partial.plot.noq

#write.table(partial.plot,file="./Data/partial_plot_RG.txt",sep="\t", row.names = TRUE)

# 5.3: Merge partial plots from Marenzelleria and Round goby ---------------

#Merge all the data
RG_partial<-data.frame(read.csv("./Data/partial_plot_RG.txt",header=T,dec=".",sep="\t", check.names = F), structure = "all") 
Marenz_partial <- data.frame(read.csv("./Data/partial_plot_Marenzelleria.txt",
                                       header=T,dec=".",sep="\t", check.names = F), structure = "all")
partial.plot <- rbind(RG_partial, Marenz_partial); 
rm(RG_partial, Marenz_partial)
partial.plot = partial.plot %>% mutate(species = as.factor(species), variable = as.factor(variable), structure = as.factor(structure))
partial.plot$filter = ifelse(partial.plot$variable %in% c("NIS_Di_nw","richness", "Eveness", "FRic"), "biot", "env")

#Real data points for both species
colnames(test_Mar)[9] = "bot_t"; test_Mar$species = "Marenzelleria"; test_RG$species = "Round goby"
points_sp = bind_rows(test_Mar, test_RG); points_sp$species = as.factor(points_sp$species)

#Partial plots for the all the effects
colors <- c("Round goby" ="steelblue4", "Marenzelleria" = "orange2")
fill <- c("Round goby" ="slategray1", "Marenzelleria" = "peachpuff")

#Modify this function to modify the plots
plot_partial <- function(variable, data){
  
  var <- data[data$variable %in% variable,]; sp = unique(var$species)
  
  p <- ggplot() + ylim(-5,5)+
    labs(x = variable, y = "log(NIS relative biomass)") + 
    geom_point(data = points_sp[points_sp$species %in% sp,],
               mapping = aes(x = points_sp[points_sp$species %in% sp, colnames(points_sp) == variable], y = log(NIS_relab), 
                             color = species, fill = species), alpha = 0) +
    geom_ribbon(data = var,
                aes(x_axis, ymin = CI.lower, ymax = CI.upper, color = species, fill = species), #fill = "gray99",
                size = 1, alpha = 0.05, linetype = 2, show.legend = F) +
    geom_line(data = var,
              aes(x_axis, rel_abund_NIS, color = species), linewidth = 2) +
    xlim(min(var$x_axis), max(var$x_axis)) +
    scale_color_manual(values= colors) +
    scale_fill_manual(values= fill) +
    theme_minimal() +
    theme(legend.background = element_rect(fill = "transparent", color = NA), legend.key = element_rect(fill = "transparent", color = NA),
          text = element_text(size = 20), plot.title = element_text(hjust = 0.5), legend.position = "none", axis.title.y = element_blank())
  
  p = ggMarginal(p, groupFill = T, groupColour = T); p
  
  return(list(p))
  
}

variables <- unique(partial.plot$variable); plots <- list()

for (i in 1:length(variables)){
  
  p <- plot_partial(variables[i], partial.plot); names(p) <- variables[i]
  name <- variables[i]
  plots <- append(plots, p); 
  #ggsave(file=paste(name, "./Plots/log.svg", sep = ""), plot = p[[1]], width=15, height=12)
  
}
#plots

#Merge all plots
part_all <- ggarrange(plots$Eveness, plots$richness, plots$FRic, plots$NIS_Di_nw, 
                  plots$depth, plots$bot_sal, plots$bot_t, plots$bot_oxy_var, plots$bot_oxy, ncol=3, nrow=3, labels = c("A", "B", "C", "D", 
                                                                                                                        "E", "F", "G",
                                                                                                                        "H", "I"))

part_all <- annotate_figure(part_all,left = text_grob("log(NIS relative biomass)", color = "black", rot = 90, size = 30))

#ggsave(file="./Plots/partial_plot_all_eff.svg", plot=part_all, width=15, height=15)



