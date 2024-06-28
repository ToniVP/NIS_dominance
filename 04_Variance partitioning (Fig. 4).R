########################################################################################!
#4: Figure 4; Variance partitioning, inidirect and combined direct effects plot  #######!
########################################################################################!
remove(list=ls())
#Load libraries and functions
source("./Code/Libraries_Functions.R")

# 4.1: MARENZELLERIA ------
# Load the SEM output and prepare the data ------------
sum <- readRDS("./Models/SEM/Marenz_SEM_final_sum")
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

# Fig. 4A: All direct effects from abiotic and biotic variables -----------

prova<- trial[[1]] #Here trial is the output dataframe from the summary of the SEM
direct_eff <- prova[prova$Response %in% c("NIS relative biomass", "Distinctiveness"), 2]; direct_eff <- c(direct_eff, "NIS relative biomass")
prova = prova %>% dplyr::filter(Response %in% direct_eff, !layer %in% c("between"), 
                                !Predictor %in% c("Bottom salinity_var2", "Bottom oxygen_var2", "Evenness2", "Bottom salinity2",
                                                  "Depth2", "Bottom oxygen2")) %>% 
  mutate(direction = ifelse(Response == "NIS relative biomass", "direct", "indirect"),
         effect_group = ifelse(layer == 2, "Environment", Predictor))


env <- unique(prova[which(prova$effect_group == "Environment"), c("Predictor")])
prova <- prova[complete.cases(prova),]

prova$Predictor <- factor(prova$Predictor, levels = c(env,"Functional Evenness", "Richness", "Evenness", "Distinctiveness"))
prova$Response <- factor(prova$Response, levels = rev(c("Functional Evenness", "Richness", "Evenness", "Distinctiveness", "NIS relative biomass")))

colors <- c("Evenness" ="yellow", "Functional Evenness" = "goldenrod1", 
            "NIS relative biomass" = "darkorchid4", "Richness" = "coral", "Distinctiveness" = "navy"); #colors[1:2,4:5] <- "grey"

var_mar = ggplot(prova, aes(color = as.factor(Response), fill=as.factor(Response), y=Std.Estimate, x=Predictor)) + 
  labs(x = "", y = "Standard estimate", fill = "Effect type") +
  geom_bar(position="stack", stat="identity", width = 0.8) + theme_minimal() + 
  geom_vline(xintercept = 6.5, size = 1.2, linetype = "dashed") +
  scale_fill_manual(values = colors) + 
  coord_flip() + 
  scale_color_manual(values = colors, guide = "none") + 
  geom_hline(aes(yintercept = 0), color = "black", linetype = 2, size = 0.5) +
  theme(legend.background = element_rect(fill = "transparent", color = NA), text = element_text(size = 20),
        legend.key = element_rect(fill = "transparent", color = NA),
        plot.title = element_text(hjust = 0.5), legend.position = "none",
        axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank()); var_mar

#ggsave(file="./Plots/Variance_partitioning_Marenz_all.svg", plot=var_mar, width=15, height=11)

# Fig. 4C: Direct and indirect effects from the environment -------
prova<- trial[[1]] #Here trial is the output dataframe from the summary of the SEM
direct_eff <- prova[prova$Response %in% c("NIS relative biomass", "Distinctiveness"), c("Predictor")]; direct_eff <- c(direct_eff, "NIS relative biomass")
prova = prova %>% dplyr::filter(Response %in% direct_eff, !layer %in% c("between"), 
                                !Predictor %in% c("Bottom salinity_var2", "Bottom oxygen_var2", "Evenness2", "Bottom salinity2",
                                                  "Depth2", "Bottom oxygen2")) %>% 
  mutate(direction = ifelse(Response == "NIS relative biomass", "direct", "indirect"),
         effect_group = ifelse(layer == 2, "Environment", "Biotic"))

#Obtain the real indirect effects of environmental variables
direct = prova %>% dplyr::filter(direction == "direct") %>% mutate(indirect_eff = 0); indirect = prova %>% dplyr::filter(direction == "indirect")

for (i in 1:nrow(indirect)){
  
  DE = direct[direct$Predictor == indirect$Response[i], "Std.Estimate"]
  
  indirect$indirect_eff[i] = DE * indirect[i, "Std.Estimate"]
  
} 

prova = rbind(direct, indirect)

env <- unique(prova[which(prova$effect_group == "Environment"), c("Predictor")])
prova <- prova[prova$Predictor %in% env,] %>% mutate(Std.Estimate = ifelse(direction == "indirect", indirect_eff, Std.Estimate))

colors <- c("direct" ="orange2", "indirect" = "peachpuff"); 

env_eff_mar = ggplot(prova, aes(color = as.factor(direction), fill=as.factor(direction), y=Std.Estimate, x=Predictor)) + 
  labs(x = "", y = "Standard estimate", fill = "Effect type") +
  geom_bar(position=position_stack(reverse = T), stat="identity", width = 0.8) + theme_minimal() + 
  scale_fill_manual(values = colors) + 
  coord_flip() + 
  scale_color_manual(values = colors, guide = "none") + 
  geom_hline(aes(yintercept = 0), color = "black", linetype = 2, size = 0.5) +
  theme(legend.background = element_rect(fill = "transparent", color = NA), text = element_text(size = 20),
        legend.key = element_rect(fill = "transparent", color = NA),
        plot.title = element_text(hjust = 0.5), legend.position = "none",
        axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank()); env_eff_mar
#ggsave(file="./Plots/Env_effects_Marenz.svg", plot=var_mar, width=15, height=11)

# Data preparation for the combined direct effects from abiotic and biotic variables (Fig. 4E) -----------
prova<- trial[[1]] #Here trial is the output dataframe from the summary of the SEM
direct_eff <- prova[prova$Response %in% c("NIS relative biomass", "Distinctiveness"), c("Predictor")]; direct_eff <- c(direct_eff, "NIS relative biomass")
prova = prova %>% dplyr::filter(Response %in% direct_eff, !layer %in% c("between"), 
                                !Predictor %in% c("Bottom salinity_var2", "Bottom oxygen_var2", "Evenness2", "Bottom salinity2",
                                                  "Depth2", "Bottom oxygen2")) %>% 
  mutate(direction = ifelse(Response == "NIS relative biomass", "direct", "indirect"),
         effect_group = ifelse(layer == 2, "Environment", "Biotic"))

colors <- c("Environment" ="orange2", "Biotic" = "orange2"); #colors[1:2,4:5] <- "grey"

env <- unique(prova[which(prova$effect_group == "Environment"), c("Predictor")])
prova <- prova[complete.cases(prova),]
prova = prova %>% dplyr::filter(direction %in% "direct") %>% 
  mutate(Cumul.effect = abs(Std.Estimate))

prova$effect_group <- factor(prova$effect_group, levels = c("Environment","Biotic"))
aggregate(prova$Cumul.effect, by=list(Group=prova$effect_group), FUN=sum)

prova_Mar = prova #Database to merge the plot with round goby

# 4.2: ROUND GOBY --------
# Load the SEM output and prepare the data ----------
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

# Fig. 4B: All direct effects from abiotic and biotic variables ----------
prova<- trial[[1]] #Here trial is the output dataframe from the summary of the SEM
direct_eff <- prova[prova$Response %in% "NIS relative biomass", c("Predictor")]; direct_eff <- c(direct_eff, "NIS relative biomass")
prova = prova %>% dplyr::filter(Response %in% direct_eff, !layer %in% c("between")) %>% 
  mutate(direction = ifelse(Response == "NIS relative biomass", "direct", "indirect"),
         effect_group = ifelse(layer == 2, "Environment", Predictor)) 

env <- unique(prova[which(prova$effect_group == "Environment"), c("Predictor")])

prova$Predictor <- factor(prova$Predictor, levels = c(env,"Functional richness", "Richness", "Evenness", "Distinctiveness"))
prova$Response <- factor(prova$Response, levels = rev(c("Functional richness", "Richness", "Evenness", "Distinctiveness", "NIS relative biomass")))

colors <- c("Evenness" ="yellow", "Functional richness" = "goldenrod1", 
            "NIS relative biomass" = "darkorchid4", "Richness" = "coral", "Distinctiveness" = "navy"); #colors[1:2,4:5] <- "grey"

var_rg = ggplot(prova, aes(color = as.factor(Response), fill=as.factor(Response), y=Std.Estimate, x=Predictor)) + 
  labs(x = "", y = "Standard estimate", fill = "Effect type") +
  geom_bar(position="stack", stat="identity", width = 0.6) + theme_minimal() + 
  geom_vline(xintercept = 4.5, size = 1.2, linetype = "dashed") +
  scale_fill_manual(values = colors) + coord_flip() + 
  scale_color_manual(values = colors, guide = "none") + 
  geom_hline(aes(yintercept = 0), color = "black", linetype = 2, size = 0.5) +
  theme(legend.background = element_rect(fill = "transparent", color = NA), text = element_text(size = 20),
        legend.key = element_rect(fill = "transparent", color = NA),
        plot.title = element_text(hjust = 0.5), legend.position = "none",
        axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank()); var_rg

#ggsave(file="./Plots/Variance_partitioning_RG_all.svg", plot=var_rg, width=15, height=11)

# Fig. 4D: Direct and indirect effects from the environment -----------
prova<- trial[[1]] #Here trial is the output dataframe from the summary of the SEM
direct_eff <- prova[prova$Response %in% "NIS relative biomass", c("Predictor")]; direct_eff <- c(direct_eff, "NIS relative biomass")
prova = prova %>% dplyr::filter(Response %in% direct_eff, !layer %in% c("between")) %>% 
  mutate(direction = ifelse(Response == "NIS relative biomass", "direct", "indirect"),
         effect_group = ifelse(layer == 2, "Environment", Predictor)) 

#Obtain the real indirect effects of environmental variables
direct = prova %>% dplyr::filter(direction == "direct") %>% mutate(indirect_eff = 0); indirect = prova %>% dplyr::filter(direction == "indirect")

for (i in 1:nrow(indirect)){
  
  DE = direct[direct$Predictor == indirect$Response[i], "Std.Estimate"]
  
  indirect$indirect_eff[i] = DE * indirect[i, "Std.Estimate"]
  
} 

prova = rbind(direct, indirect)

env <- unique(prova[which(prova$effect_group == "Environment"), c("Predictor")])
prova <- prova[prova$Predictor %in% env,] %>% mutate(Std.Estimate = ifelse(direction == "indirect", indirect_eff, Std.Estimate))

colors <- c("direct" ="steelblue4", "indirect" = "slategray1");

env_eff_RG = ggplot(prova, aes(color = as.factor(direction), fill=as.factor(direction), y=Std.Estimate, x=Predictor)) + 
  labs(x = "", y = "Standard estimate", fill = "Effect type") +
  geom_bar(position=position_stack(reverse = T), stat="identity", width = 0.6) + theme_minimal() + 
  scale_fill_manual(values = colors) + 
  coord_flip() + 
  scale_color_manual(values = colors, guide = "none") + 
  geom_hline(aes(yintercept = 0), color = "black", linetype = 2, size = 0.5) +
  theme(legend.background = element_rect(fill = "transparent", color = NA), text = element_text(size = 20),
        legend.key = element_rect(fill = "transparent", color = NA),
        plot.title = element_text(hjust = 0.5), legend.position = "none",
        axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank()); env_eff_RG
#ggsave(file="./Plots/Env_effects_RG.svg", plot=var_mar, width=15, height=11)

# Data preparation for the combined direct effects from abiotic and biotic variables (Fig. 4E) --------
prova<- trial[[1]] #Here trial is the output dataframe from the summary of the SEM
direct_eff <- prova[prova$Response %in% "NIS relative biomass", c("Predictor")]; direct_eff <- c(direct_eff, "NIS relative biomass")
prova = prova %>% dplyr::filter(Response %in% direct_eff, !layer %in% c("between")) %>% 
  mutate(direction = ifelse(Response == "NIS relative biomass", "direct", "indirect"),
         effect_group = ifelse(layer == 2, "Environment", "Biotic")) 

colors <- c("Environment" ="steelblue4", "Biotic" = "steelblue4"); #colors[1:2,4:5] <- "grey"

env <- unique(prova[which(prova$effect_group == "Environment"), c("Predictor")])
prova <- prova[complete.cases(prova),]
prova = prova %>% dplyr::filter(direction %in% "direct") %>% 
  mutate(Cumul.effect = ifelse(Std.Estimate < 0, Std.Estimate*-1, Std.Estimate))

prova$effect_group <- factor(prova$effect_group, levels = c("Environment","Biotic"))
aggregate(prova$Cumul.effect, by=list(Group=prova$effect_group), FUN=sum)

prova_RG = prova

# 4.3: Merge all plots in one (Fig. 4) -----------
#Combine both dataframes
prova_Mar$species = "Mar"; prova_RG$species = "RG"
filt = rbind(prova_Mar, prova_RG)
filt = filt %>% group_by(species, effect_group) %>% summarize(Cumul.effect = sum(Cumul.effect), .groups = "drop_last")

var_filt = ggplot(filt, aes(fill=as.factor(species), y= Cumul.effect, x=as.factor(effect_group))) + 
  scale_fill_manual(values = c("Mar" ="orange2", "RG" = "steelblue4")) + 
  labs(x = "Community filter", y = "Absolute direct effects") +
  geom_bar(position="dodge", stat="identity", width = 0.5) + 
  theme_minimal() + coord_flip() + 
  theme(legend.background = element_rect(fill = "transparent", color = NA), text = element_text(size = 20),
        legend.key = element_rect(fill = "transparent", color = NA),
        plot.title = element_text(hjust = 0.5), legend.position = "none",
        axis.line = element_line(colour = "black"), axis.title.y = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank()); var_filt

#ggsave(file="./Plots/Total_effects.svg", plot=var_filt, width=15, height=11)

# Merge all plots
var_total <- ggarrange(var_mar,var_rg, env_eff_mar, env_eff_RG, var_filt, 
                       ncol=2, nrow=3, labels = c("A", "B", "C", "D", "E")); var_total

#ggsave(file="Variance_partitioning_RG_Marenz_all.svg", plot=var_total, width=19.5, height=20)

