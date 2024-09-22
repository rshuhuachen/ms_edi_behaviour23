# load in environment with source("teams/function_collect_model_output.R")

collect_out <- function(model, null, name, n_factors, type=c("qa", "survey", "likert", "exp"), 
                        save = c("yes", "no"), dir){
  
  #load packages 
  
  pacman::p_load(lme4, jtools, dplyr, data.table, performance, lmtest)
  
  #### QA models without a null model bc only intercept ####
  if (n_factors == 0 & (isS4(null) == FALSE) & type == "qa"){
    ## Cannot do an LRT
    lrt <- NA
    lrt_pval <- NA
    lrt_chisq <- NA
    
    ## Collect info on estimate and 95% CI of intercept, but summ function won't work to get CI
    confinterval <- as.data.frame(confint(model))
    summary <- summary(model)$coefficients
    intercept_estimate <- summary["(Intercept)","Estimate"]
    intercept_estimate_prop <- plogis(intercept_estimate)
    intercept_pval <- summary["(Intercept)","Pr(>|z|)"]
    intercept_ci_lower <- confinterval["(Intercept)","2.5 %"]
    intercept_ci_higher <- confinterval["(Intercept)","97.5 %"]
    
    fixed_effects <- data.frame(n_factors = n_factors) 
  }
  
  #### QA models with a null model to look at effects of factors ####
  
  if (n_factors > 0 & (isS4(null) == TRUE)& type == "qa"){
    ## LRT to see if alternative model is a better fit compared to null
    lrt <- as.data.frame(anova(model, null))
    lrt_pval <- lrt["model", "Pr(>Chisq)"]
    lrt_chisq <- lrt["model", "Chisq"]
  
    ## Collect info on estimate and 95% CI of intercept
    summary <- as.data.frame(summ(model, confint=TRUE)$coeftable)
    intercept_estimate <- summary[1,"Est."]
    intercept_estimate_prop <- plogis(intercept_estimate)
    intercept_pval <- summary["(Intercept)","p"]
    intercept_ci_lower <- summary["(Intercept)","2.5%"]
    intercept_ci_higher <- summary["(Intercept)","97.5%"]
  
    ## For each factor, collect estimate, probability and 95% CI
    
    fixed_effects <- data.frame(n_factors = n_factors)
    for (i in 1:n_factors){
      name_factor = rownames(summary)[i+1] #intercept + factor number
      est = summary[i+1,"Est."]
     # est_probabitily = plogis(est) # convert the raw estimate to a probability
      lowerCI = summary[i+1,"2.5%"]
      higherCI = summary[i+1,"97.5%"]
      zval = summary[i+1,"z val."]
      pval = summary[i+1,"p"]
      
      # combine everything of this factor in a single data frame
      
      df_factor <- data.frame(est = est,
                            #  est_probabitily = est_probabitily,
                              lowerCI = lowerCI,
                              higherCI = higherCI,
                              pval = pval,
                              zval = zval)
      
      # rename the col names to include the name of the factor
      
      for (j in 1:ncol(df_factor)){
        names(df_factor)[j] <- paste0(names(df_factor)[j], "_", name_factor)}
        
      # combine all factor statistics in a single df
      
      fixed_effects <- cbind(fixed_effects, df_factor)
      }}
  #### QA models experimental no intercept ####
  
  if (n_factors > 0 & (isS4(null) == TRUE)& type == "exp"){
    ## LRT to see if alternative model is a better fit compared to null
    lrt <- as.data.frame(anova(model, null))
    lrt_pval <- lrt["model", "Pr(>Chisq)"]
    lrt_chisq <- lrt["model", "Chisq"]
    
    ## Collect info on estimate and 95% CI of intercept
    summary <- as.data.frame(summ(model, confint=TRUE)$coeftable)
    
    ## For each factor, collect estimate, probability and 95% CI
    
    fixed_effects <- data.frame(n_factors = n_factors)
    for (i in 1:n_factors){
      name_factor = rownames(summary)[i] # factor number
      est = summary[i,"Est."]
      est_probabitily = plogis(est) # convert the raw estimate to a probability
      lowerCI = summary[i,"2.5%"]
      higherCI = summary[i,"97.5%"]
      zval = summary[i,"z val."]
      pval = summary[i,"p"]
      
      # combine everything of this factor in a single data frame
      
      df_factor <- data.frame(est = est,
                              est_probabitily = est_probabitily,
                              lowerCI = lowerCI,
                              higherCI = higherCI,
                              pval = pval,
                              zval = zval)
      
      # rename the col names to include the name of the factor
      
      for (j in 1:ncol(df_factor)){
        names(df_factor)[j] <- paste0(names(df_factor)[j], "_", name_factor)}
      
      # combine all factor statistics in a single df
      
      fixed_effects <- cbind(fixed_effects, df_factor)
    }}
  
  #### Survey models without a null model ####
  
  if (n_factors > 0 & (isS4(null) == FALSE)& type == "survey"){
    ## LRT to see if alternative model is a better fit compared to null
    lrt <- as.data.frame(drop1(model, test="Chisq"))
    lrt_pval <- lrt[2, "Pr(>Chi)"]
    lrt_chisq <- lrt[2, "LRT"]
    if (is.null(lrt_pval)){
      lrt_pval <- lrt[2, "Pr(Chi)"]
    }
    ## Collect info on estimate and 95% CI of intercept
    summary <- as.data.frame(summ(model, confint=TRUE)$coeftable)
    intercept_estimate <- summary[1,"Est."]
    intercept_estimate_prop <- plogis(intercept_estimate)
    intercept_pval <- summary["(Intercept)","p"]
    intercept_ci_lower <- summary["(Intercept)","2.5%"]
    intercept_ci_higher <- summary["(Intercept)","97.5%"]
    
    ## For each factor, collect estimate, probability and 95% CI
    
    fixed_effects <- data.frame(n_factors = n_factors)
    for (i in 1:n_factors){
      name_factor = rownames(summary)[i+1] #intercept + factor number
      est = summary[i+1,"Est."]
      # est_probabitily = plogis(est) # convert the raw estimate to a probability
      lowerCI = summary[i+1,"2.5%"]
      higherCI = summary[i+1,"97.5%"]
      zval = summary[i+1,"z val."]
      pval = summary[i+1,"p"]
      
      # combine everything of this factor in a single data frame
      
      df_factor <- data.frame(est = est,
                              #  est_probabitily = est_probabitily,
                              lowerCI = lowerCI,
                              higherCI = higherCI,
                              pval = pval,
                              zval = zval)
      
      # rename the col names to include the name of the factor
      
      for (j in 1:ncol(df_factor)){
        names(df_factor)[j] <- paste0(names(df_factor)[j], "_", name_factor)}
      
      # combine all factor statistics in a single df
      
      fixed_effects <- cbind(fixed_effects, df_factor)}
    }
  
  #### QA models with likert scale data and null model ####
  if (n_factors > 0 & (is.null(null) == FALSE)& type == "likert"){
    ## LRT to see if alternative model is a better fit compared to null
    lrt <- as.data.frame(anova(model, null))
    lrt_pval <- lrt[2, "Pr(Chi)"]
    lrt_chisq <- lrt[2, "LR stat."]
    
    ## Collect info on estimate and 95% CI of intercept
    summary <- as.data.frame(summary(model)$coef)
    intercept_12 <- summary["1|2", "Value"]
    intercept_23 <- summary["2|3", "Value"]
    intercept_34 <- summary["3|4", "Value"]
    intercept_45 <- summary["4|5", "Value"]
    intercept_56 <- summary["5|6", "Value"]
    intercept_67 <- summary["6|7", "Value"]
    
    ## For each factor, collect estimate, probability and 95% CI
    
    fixed_effects <- data.frame(n_factors = n_factors)
    for (i in 1:n_factors){
      pvals <- as.data.frame(coeftest(model)[,])
      #ci <- as.data.frame(confint(model))
      ci <- as.data.frame(coefci(model))
      if (rownames(pvals)[i] == "Estimate"){
        pvals <- t(pvals)
      }
      if (rownames(ci)[i] == "2.5 %"){
        ci <- t(ci)
      }
      name_factor = rownames(summary(model)$coef)[i]
      est = pvals[i,"Estimate"]
      lowerCI = ci[i,"2.5 %"]
      higherCI =  ci[i,"97.5 %"]
      se = pvals[i,"Std. Error"]
      tval = pvals[i,"t value"]
      pval = pvals[i,"Pr(>|t|)"]
      
      # combine everything of this factor in a single data frame
      
      df_factor <- data.frame(est = est,
                              lowerCI = lowerCI,
                              higherCI = higherCI,
                              se = se,
                              tval = tval,
                              pval = pval)
      
      # rename the col names to include the name of the factor
      
      for (j in 1:ncol(df_factor)){
        names(df_factor)[j] <- paste0(names(df_factor)[j], "_", name_factor)}
      
      # combine all factor statistics in a single df
      
      fixed_effects <- cbind(fixed_effects, df_factor)
    }
    
  }
  #### Combine ####
  ### for every model 
  
  #combine LRT, intercept, fixed effects and r2 parameters
  if (type == "exp"){
    out <- cbind(
      data.frame(model_name = name,
                 AIC = AIC(model),
                 n_obs = nobs(model),
                 lrt_pval = lrt_pval,
                 lrt_chisq = lrt_chisq),
      fixed_effects)
  }
  
  if (type != "likert" & type != "exp"){
  out <- cbind(
    data.frame(model_name = name,
               AIC = AIC(model),
               n_obs = nobs(model),
               lrt_pval = lrt_pval,
               lrt_chisq = lrt_chisq,
               intercept_estimate = intercept_estimate,
               intercept_estimate_prop = intercept_estimate_prop,
               intercept_pval = intercept_pval,
               intercept_ci_lower = intercept_ci_lower,
               intercept_ci_higher = intercept_ci_higher),
    fixed_effects)}
  
  if (type == "likert"){
    out <- cbind(
      data.frame(model_name = name,
                 AIC = AIC(model),
                 n_obs = nobs(model),
                 lrt_pval = lrt_pval,
                 lrt_chisq = lrt_chisq,
                 intercept_12 = intercept_12,
                 intercept_23 = intercept_23,
                 intercept_34 = intercept_34,
                 intercept_45 = intercept_45,
                 intercept_56 = intercept_56,
                 intercept_67 = intercept_67),
      fixed_effects)
  }
   
  # round off to 3 SI
  out <- out %>% mutate_if(is.numeric, round, digits=3)
  
  # save df
  
  if(save == "yes"){
  write.csv(out, file = paste0(dir, "/model_", name, ".csv"), 
            quote=F, row.names = F)
  }
  return(out)
  }
