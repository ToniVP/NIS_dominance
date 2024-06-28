#####################!
# LIST OF LIBRARIES #!
#####################

listoflibrary<-c("MASS", "dplyr", "ggplot2", "stringr", "ggnewscale", "forecast", "MuMIn", "scatterpie", "cowplot", 
                 "ggExtra", "ggpubr", "ggrepel", "PerformanceAnalytics", "car", "piecewiseSEM", "sp", "mgcv", "lme4",
                 "nlme","viridis", "svglite", "egg")

for (pkg in listoflibrary){
  if(!eval(bquote(require(.(pkg))))) {eval(bquote(install.packages(.(pkg))))}
  eval(bquote(library(.(pkg))))
}

#Functions needed
piecewiseSEM_ggplot <- function(sum, factors_list, quadratic, non.quadratic, pval){
  
  #Extract the coefficients table from the summary
  coeff <- sum$coefficients
  coeff[,1:2] <- apply(coeff[,1:2], 2,
                       function (x){ x = gsub("~~","",x);
                       x = gsub("[()]","",x)})
  colnames(coeff)[9] <- "significance"; coeff$significance <- as.factor(coeff$significance)
  
  
  #Create a coordinates data frame for the SEM structure
  fact <- unique(c(unlist(factor_list))) #put all the factor names in a vector
  
  #Create vertical position for the different layers
  lap = 2; q = 2; y = c()
  repeat{
    
    vert <- length(factor_list)*2 - 1
    
    y = append(y, rep(vert-q, length(factor_list[[lap]])))
    
    q = q + 2
    lap = lap + 1
    
    if(lap > length(factor_list)){break}
  }
  y = c(vert,y)
  
  #Create horizontal positions for the different factors
  # lap = 2; q = 2; x = c()
  # repeat{
  
  horiz <- (length(factor_list[["2nd layer"]])*2 - 1)/2
  
  x = seq(1, length(factor_list[["2nd layer"]])*2 - 1, by = 2)
  
  central = 0.5*(x[1:length(x)-1] + x[2:length(x)])
  
  med = which(central %in% median(central))
  
  lay1 = central[(med-floor(length(factor_list[["1st layer"]])/2)):(med+floor(length(factor_list[["1st layer"]])/2))]
  
  #   q = q + 2
  #   lap = lap + 1
  #   
  #   if(lap > length(factor_list)){break}
  # }
  x = c(horiz,lay1,x)
  
  pnts <- data.frame(fact = fact, x =x, y =y)
  
  #Create a database for plotting
  prova <- merge(coeff, pnts, by.x = "Predictor", by.y = "fact", all.x = T)
  prova <- merge(prova, pnts, by.x = "Response", by.y = "fact")
  colnames(prova)[10:13] <- c("xPred", "yPred","xResp","yResp")
  resp <- prova[prova$Response %in% factor_list[[1]],]; resp <- resp[1, c("Response","xResp","yResp")] #get your response variable names
  pred <- unique(prova[,c("Predictor", "xPred", "yPred")]) #get your predictor names
  
  #Put the same coordinates in quadratic terms as in the non-quadratic
  
  for (i in 1:length(quadratic)){
    
    prova[prova$Predictor %in% quadratic[i], 
          c("xPred","yPred")] <- c(unique(prova[prova$Predictor %in% non.quadratic[i], 
                                                c("xPred","yPred")]))
  }
  
  
  #Get different colour groups based on effects
  prova$effect <- ifelse(prova$Std.Estimate > 0, "positive", "negative")
  
  #Get the scaling sizes for the lines
  prova$size <- ifelse(prova$Std.Estimate > 0, prova$Std.Estimate, prova$Std.Estimate*-1)
  
  #Classify the effects according to their direction
  prova$layer <- NA; q = 2; layer = 1; lap = 1
  repeat {
    
    prova$layer <- ifelse(prova$yPred == vert - q, layer, prova$layer)
    prova$layer <- ifelse(prova$yPred == vert - q & prova$yResp == vert - q, "between", prova$layer)
    
    lap = lap +1
    q = q + 2
    layer = layer + 1
    
    if(lap > length(factor_list)){break}
  }
  
  #Determine if a variable is quadratic or not
  prova$quadratic <- ifelse(prova$Predictor %in% quadratic, "quadratic", "non-quadratic")
  quad <- unique(prova[which(prova$quadratic == "quadratic"),c("Predictor", "xPred", "yPred")])
  
  #Filter by p-value
  prova <- prova[which(prova$P.Value <= pval),]
  
  #Delete those effects that appear from a quadratic term to a non-quadratic one
  temp <- prova[which(prova$xPred == prova$xResp & prova$yPred == prova$yResp),]
  prova <- anti_join(prova,temp)
  
  return(list(prova,resp,pred,pnts,quad))
}
#Function to translate the SEM output to a database to be used in ggplot
