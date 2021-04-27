#Reelection backfire: matching 

rm(list=ls())
### Packages
library(tidyverse)
library(PanelMatch)
require(haven)
 
#working directory:
setwd("~/Dropbox/Dissertation/GovernmentStrategies/reelection_backfire/Dofiles/")

#check this:
# https://stats.stackexchange.com/questions/61218/propensity-score-matching-with-panel-data
# https://cran.r-project.org/web/packages/PanelMatch/vignettes/using_panelmatch.html 
#Load dataset:
data <- read_dta("../Data/ConstructionDatabase/municipalities_id_2010_2019_whomicideSNSPnew_old_wcovariates_v2.dta")
collapsed.data <- read_dta("../Data/ConstructionDatabase/collapseddata_forR.dta")
data.wleadlags<- read_dta("../Data/ConstructionDatabase/data_wleads&lags.dta")
data.wleadlags2<- read_dta("../Data/ConstructionDatabase/data_wleads&lags2.dta")
data.incumbency<- read_dta("../Data/ConstructionDatabase/data_wleads&lags_incumbency.dta")
final<- read_dta("../Data/ConstructionDatabase/data_wleads&lags2_weights.dta")
dem<-dem

#Load dataset:
data.wleadlags<- read_dta("../../Data/ConstructionDatabase/data_wleads&lags.dta")
data.wleadlags2<- read_dta("../../Data/ConstructionDatabase/data_wleads&lags2.dta")
data.final<- read_dta("../../Data/ConstructionDatabase/data_final.dta")
collapsed.data <- read_dta("../../Data/ConstructionDatabase/collapseddata_forR.dta")
data.final2<-read_dta("../../Data/ConstructionDatabase/data_final2_forR.dta")
data.final2.balanced<-read_dta("../../Data/ConstructionDatabase/data_final2_forR_balanced.dta")
data.as<-read_dta("../../Data/ConstructionDatabase/municipalities_id_2010_2019_whomicideSNSPnew_old_wcovariates_v2.dta")



#show treatment
data.final$year2<-as.integer(data.final$year)
#as data frame
data.final<-as.data.frame(data.final)
#state level
DisplayTreatment(unit.id = "estado", time.id = "year2", legend.position = "none",
                 xlab="year", ylab = "Municipality",
                 treatment="reform", data=data.final)
dev.copy(png,'../Figures/reform_treatment_statelevel.png')
dev.off()

#municipal level
DisplayTreatment(unit.id = "inegi", time.id = "year2", legend.position = "none",
                 xlab="year", ylab = "Municipality",
                 treatment="reform", data=data.final, dense.plot = T)
dev.copy(png,'../Figures/reform_treatment_municipallevel.png')
dev.off()

 
#PanelMatch
PM.results.none <- PanelMatch(lag = 5, time.id = "year2", unit.id = "inegi", 
                              treatment = "reform", refinement.method = "none", 
                              data = data.final, match.missing = TRUE, 
                              covs.formula = ~ I(lag(logdefuncionespc, 1:5)) + I(lag(winning_margin_governor, 1:5)) + I(lag(alignment_governor_strong, 1:5)) + I(lag(winning_margin, 1:5)) + I(lag(hayCarteles, 1:5)) + I(lag(pan_mayor2, 1:5)) + I(lag(pri_mayor2, 1:5)), 
                              size.match = 5, qoi = "att", outcome.var = "logdefuncionespc",
                              lead = 0:3, forbid.treatment.reversal = FALSE, 
                              use.diagonal.variance.matrix = TRUE)
plot(PM.results.none$att)
PM.results.none.results <- PanelEstimate(sets = PM.results.none, data = data.final)
summary(PM.results.none.results) #negative effect with acuerdo_estcom
plot(PM.results.none.results)
dev.copy(png,'../Figures/panelmatch_logdefuncionespc.png')
dev.off()

##My plot style: 
broom::tidy(PM.results.none.results, conf.int = TRUE) %>% 
  # keep just the variables we are going to plot
  filter(term %in% treatments_full) %>% 
  # reformat d data
  mutate(t = c(0:3)) %>% 
  subset(select=c(t, estimate, conf.low, conf.high))  %>% 
  # plot
  ggplot(aes(x = t, y = estimate)) + 
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high, group = band_groups), 
              color = "lightgrey", alpha = 1/2) + 
  geom_point(color = "red") + 
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high)) + 
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = -0.5, linetype = "dashed") + 
  scale_x_continuous(breaks = 0:3) + 
  theme(axis.title.x = element_blank()) + theme_bw()
dev.copy(png,'../Figures/PM_logdefuncionespc')
dev.off()

#Identifying assumption:
balance.plot<-get_covariate_balance(PM.results.none$att,
                      data = data.final,
                      covariates = "logdefuncionespc", 
                      plot = TRUE, # visualize by setting plot to TRUE
                      ylim = c(-.2, .2))


cov_bal <- get_covariate_balance(PM.results.none$att, data.final, 
                                 covariates = "logdefuncionespc", plot = F)  %>%
                                  as.data.frame() %>%
                                  mutate(t = c(-4:0))

bal_dv <- ggplot(cov_bal, aes(t, stdmean)) +
  geom_line() +
  annotate("rect", xmin = -4, xmax = 0, ymin = 0.2, ymax = -0.2,  alpha = .3, fill = "grey") + # this is conventional acceptance thresholds
  geom_vline(xintercept = -0, linetype = 3) +
  geom_hline(yintercept = 0, linetype = 3) +
  xlim(-4,0) +
  labs(x = "Years Before Treatment", y = "Std. Mean Diff.") +
  scale_y_continuous(breaks = c(-0.4, -0.2, 0, 0.2, 0.4), limits = c(-0.4,0.4)) +
  theme_bw()


#Robustness 1: replace treatment indicator with its lead value (could do a lag too)
library(dplyr)
data.final$leadreform<-lead(data.final$reform,1)
data.final$lead2reform<-lead(data.final$reform,2)
data.final$lead3reform<-lead(data.final$reform,3)


PM.results.placebo <- PanelMatch(lag = 5, time.id = "year2", unit.id = "inegi", 
                              treatment = "leadreform", refinement.method = "none", 
                              data = data.final, match.missing = TRUE, 
                              covs.formula = ~ I(lag(logdefuncionespc, 1:5)) + I(lag(winning_margin_governor, 1:5)) + I(lag(alignment_governor_strong, 1:5)) + I(lag(winning_margin, 1:5)) + I(lag(hayCarteles, 1:5)) + I(lag(pan_mayor2, 1:5)) + I(lag(pri_mayor2, 1:5)), 
                              size.match = 5, qoi = "att", outcome.var = "logdefuncionespc",
                              lead = 0:3, forbid.treatment.reversal = FALSE, 
                              use.diagonal.variance.matrix = TRUE)
plot(PM.results.placebo$att)
PM.results.placebo.results <- PanelEstimate(sets = PM.results.placebo, data = data.final)
summary(PM.results.placebo.results) #negative effect with acuerdo_estcom
plot(PM.results.placebo.results)
dev.copy(png,'../Figures/panelmatch_logdefuncionespc_placebo_leadreform.png')
dev.off()

PM.results.placebo2 <- PanelMatch(lag = 5, time.id = "year2", unit.id = "inegi", 
                                 treatment = "lead2reform", refinement.method = "none", 
                                 data = data.final, match.missing = TRUE, 
                                 covs.formula = ~ I(lag(logdefuncionespc, 1:5)) + I(lag(winning_margin_governor, 1:5)) + I(lag(alignment_governor_strong, 1:5)) + I(lag(winning_margin, 1:5)) + I(lag(hayCarteles, 1:5)) + I(lag(pan_mayor2, 1:5)) + I(lag(pri_mayor2, 1:5)), 
                                 size.match = 5, qoi = "att", outcome.var = "logdefuncionespc",
                                 lead = 0:3, forbid.treatment.reversal = FALSE, 
                                 use.diagonal.variance.matrix = TRUE)
plot(PM.results.placebo2$att)
PM.results.placebo2.results <- PanelEstimate(sets = PM.results.placebo2, data = data.final)
summary(PM.results.placebo2.results) #negative effect with acuerdo_estcom
plot(PM.results.placebo2.results)
dev.copy(png,'../Figures/panelmatch_logdefuncionespc_placebo_lead2reform.png')
dev.off()

PM.results.placebo3 <- PanelMatch(lag = 5, time.id = "year2", unit.id = "inegi", 
                                  treatment = "lead3reform", refinement.method = "none", 
                                  data = data.final, match.missing = TRUE, 
                                  covs.formula = ~ I(lag(logdefuncionespc, 1:5)) + I(lag(winning_margin_governor, 1:5)) + I(lag(alignment_governor_strong, 1:5)) + I(lag(winning_margin, 1:5)) + I(lag(hayCarteles, 1:5)) + I(lag(pan_mayor2, 1:5)) + I(lag(pri_mayor2, 1:5)), 
                                  size.match = 5, qoi = "att", outcome.var = "logdefuncionespc",
                                  lead = 0:3, forbid.treatment.reversal = FALSE, 
                                  use.diagonal.variance.matrix = TRUE)
plot(PM.results.placebo3$att)
PM.results.placebo3.results <- PanelEstimate(sets = PM.results.placebo3, data = data.final)
summary(PM.results.placebo3.results) #negative effect with acuerdo_estcom
plot(PM.results.placebo3.results)
dev.copy(png,'../Figures/panelmatch_logdefuncionespc_placebo_lead3reform.png')
dev.off()

#Robustness 2:I follow Altonji et al. (2005) and check whether unobserved variation
#is likely to explain variation in the outcome, taking variation in observables as a proxy for
#unobserved variation. 
#How to do it:
## 1. I regress the treatment (whether the municipality held reelection) on all the available covariates.
## 2. then take the fitted value from the regression and use it to predict each outcome, this time including unit and year fixed effects
## 3. This test suggests that – under the assumption that observables are representative of unobservables – selection on unobservables is not driving the results.

#Robustness 3:results are robust to different number of lags used to define matched sets of data (i.e., L 2 [1; 6], and alternative lags of treatment and outcomes).

#Robutness 4: other methods (AS 2021, Chaisemartin 2021 Callaway 2020, etc.)

#MANDO UNICO
covariates_all <-c("logdefuncionespc_mean", "winning_margin_governor_mean", "alignment_governor_strong_mean", "winning_margin_mean", "hayCarteles_mean", "pan_mayor2_mean", "pri_mayor2_mean")
covariates.paste<-paste(covariates_all, collapse = " + ")

mando.unico <- PanelMatch(lag = 5, time.id = "year2", unit.id = "inegi", 
                          treatment = "reform", refinement.method = "none", # use Mahalanobis distance 
                          data = data.final, match.missing = TRUE, 
                          covs.formula = ~ I(lag(acuerdo_estcom, 1:5)) + I(lag(logpop, 1:5)) + I(lag(logdefuncionespc, 1:5)) + I(lag(winning_margin_governor, 1:5)) + I(lag(alignment_governor_strong, 1:5)) + I(lag(winning_margin, 1:5)) + I(lag(hayCarteles, 1:5)) + I(lag(pan_mayor2, 1:5)) + I(lag(pri_mayor2, 1:5)), 
                          size.match = 5, qoi = "att" , outcome.var = "acuerdo_estcom",
                          lead = 0:3, forbid.treatment.reversal = FALSE, 
                          use.diagonal.variance.matrix = TRUE)


mando.unico.results <- PanelEstimate(sets = mando.unico, data = data.final)
summary(mando.unico.results) #negative effect with acuerdo_estcom
plot(mando.unico.results)
dev.copy(png,'../Figures/acuerdo_estcom.png')
dev.off()

#with acuerdo_gobestatal
mando.unico2 <- PanelMatch(lag = 5, time.id = "year2", unit.id = "inegi", 
                          treatment = "reform", refinement.method = "none", # use Mahalanobis distance 
                          data = data.final, match.missing = TRUE, 
                          covs.formula = ~   I(lag(logpop, 1:5)) + I(lag(logdefuncionespc, 1:5)) + I(lag(winning_margin_governor, 1:5)) + I(lag(alignment_governor_strong, 1:5)) + I(lag(winning_margin, 1:5)) + I(lag(hayCarteles, 1:5)) + I(lag(pan_mayor2, 1:5)) + I(lag(pri_mayor2, 1:5)), 
                          size.match = 5, qoi = "att" , outcome.var = "acuerdo_gobestatal2",
                          lead = 0:3, forbid.treatment.reversal = FALSE, 
                          use.diagonal.variance.matrix = TRUE)


mando.unico2.results <- PanelEstimate(sets = mando.unico2, data = data.final)
summary(mando.unico2.results) #negative effect with acuerdo_estcom
plot(mando.unico2.results)
dev.copy(png,'../Figures/acuerdo_gobestatal.png')
dev.off()

#Placebo
mando.unico.placebo <- PanelMatch(lag = 5, time.id = "year2", unit.id = "inegi", 
                          treatment = "lead3reform", refinement.method = "none", # use Mahalanobis distance 
                          data = data.final, match.missing = TRUE, 
                          covs.formula = ~I(lag(acuerdo_estcom, 1:5)) + I(lag(logpop, 1:5)) + I(lag(logdefuncionespc, 1:5)) + I(lag(winning_margin_governor, 1:5)) + I(lag(alignment_governor_strong, 1:5)) + I(lag(winning_margin, 1:5)) + I(lag(hayCarteles, 1:5)) + I(lag(pan_mayor2, 1:5)) + I(lag(pri_mayor2, 1:5)), 
                          size.match = 3, qoi = "att" , outcome.var = "acuerdo_estcom",
                          lead = 0:3, forbid.treatment.reversal = FALSE, 
                          use.diagonal.variance.matrix = TRUE)


mando.unico.placebo.results <- PanelEstimate(sets = mando.unico.placebo, data = data.final)
summary(mando.unico.placebo.results) #negative effect with acuerdo_estcom
plot(mando.unico.placebo.results)
dev.copy(png,'../Figures/mando_unico_placeboleadreform.png')
dev.off()

#Covariate balance:
cov_bal <- get_covariate_balance(mando.unico$att, data.final, 
                                 covariates = "acuerdo_estcom", plot = F) %>%
  as.data.frame() %>%
  mutate(t = c(-4:0))

bal_dv <- ggplot(cov_bal, aes(t, stdmean)) +
  geom_line() +
  annotate("rect", xmin = -4, xmax = 0, ymin = 0.2, ymax = -0.2,  alpha = .3, fill = "grey") + # this is conventional acceptance thresholds
  geom_vline(xintercept = -0, linetype = 3) +
  geom_hline(yintercept = 0, linetype = 3) +
  xlim(-4,0) +
  labs(x = "Years Before Treatment", y = "Std. Mean Diff.") +
  scale_y_continuous(breaks = c(-0.4, -0.2, 0, 0.2, 0.4), limits = c(-0.4,0.4)) +
  theme_bw()

get_covariate_balance(mando.unico$att,
                                  data = data.final,
                                  covariates = c("acuerdo_estcom"),
                                  #covariates = c("logdefuncionespc", "winning_margin_governor", "alignment_governor_strong", "winning_margin_mean", "hayCarteles", "pan_mayor2", "pri_mayor2"), 
                                  plot = TRUE, # visualize by setting plot to TRUE
                                  ylim = c(-.2, .2))
dev.copy(png,'../Figures/covariate_balance.png')
dev.off()

library(doParallel)
numCores<-detectCores()
numCores<-6
balance<-function(covariates){
  PanelMatch::get_covariate_balance(mando.unico$att,
                                    data = data.final,
                                    covariates = covariates,
                                    #covariates = c("logdefuncionespc", "winning_margin_governor", "alignment_governor_strong", "winning_margin_mean", "hayCarteles", "pan_mayor2", "pri_mayor2"), 
                                    plot = TRUE, # visualize by setting plot to TRUE
                                    ylim = c(-.2, .2))
}

system.time(
  results <- mclapply(acuerdo_estcom,  balance, mc.cores=numCores)
)

rm(large_df, large_list, large_vector, temp_variables)

#Mando Unico Refinements:
mando.unico.2 <- PanelMatch(lag = 5, time.id = "year2", unit.id = "inegi", 
                            treatment = "reform", refinement.method = "mahalanobis", # use Mahalanobis distance 
                            data = final, match.missing = TRUE, 
                            covs.formula = ~ I(lag(logdefuncionespc, 1:5)) + I(lag(winning_margin_governor, 1:5)) + I(lag(alignment_governor_strong, 1:5)) + I(lag(winning_margin, 1:5)) + I(lag(hayCarteles, 1:5)) + I(lag(pan_mayor2, 1:5)) + I(lag(pri_mayor2, 1:5)), 
                            size.match = 5, qoi = "att" , outcome.var = "acuerdo_estcom",
                            lead = 0:3, forbid.treatment.reversal = FALSE, 
                            use.diagonal.variance.matrix = TRUE)
mando.unico.2.results <- PanelEstimate(sets = mando.unico.2, data = data.final)
summary(mando.unico.2.results) #no effect
plot(mando.unico.2.results)
dev.copy(png,'../Figures/mando_unico2.png')
dev.off()

#====================================================
#Extract the matched.set object 

msets <- PM.results.none$att  
names(msets)
#data frame printing view: useful as a summary view with large data sets
print(msets)
# first column is unit id variable, second is time variable, and 
# third is the number of controls in that matched set
msets[1]
msets[[1]]
plot(msets, xlim = c(0, 4))
print(summary(msets))

#Matched sets displayed
DisplayTreatment(unit.id = "inegi", time.id = "year2", treatment = 'reform', data = data.final, matched.set = msets[1])

DisplayTreatment(unit.id = "inegi", time.id = "year2", treatment = 'reform', 
                 data = final, matched.set = msets[1], show.set.only = TRUE, y.size = 15, x.size = 13)

#Below, we will use the mahalanobis option for refinement.method and will use only
#contemporaneous values of the tradewb to define similarity.

PM.results <- PanelMatch(lag = 5, time.id = "year2", unit.id = "inegi", 
                         treatment = "reform", refinement.method = "mahalanobis", # use Mahalanobis distance 
                         data = final, match.missing = TRUE, 
                         covs.formula = ~ logdefuncionespc, 
                         size.match = 5, qoi = "att" , outcome.var = "logdefuncionespc",
                         lead = 0:3, forbid.treatment.reversal = FALSE, 
                         use.diagonal.variance.matrix = TRUE)

#Next, we will include 4 lags of the tradewb variable and the outcome variable, 
#excluding any contemporaneous values.
PM.results <- PanelMatch(lag = 5, time.id = "year2", unit.id = "inegi", 
treatment = "reform", refinement.method = "mahalanobis", 
data = final, match.missing = TRUE, 
covs.formula = ~ I(lag(winning_margin_governor, 1:5)) + I(lag(logdefuncionespc, 1:5)), # lags
size.match = 5, qoi = "att", outcome.var = "logdefuncionespc",
lead = 0:3, forbid.treatment.reversal = FALSE, 
use.diagonal.variance.matrix = TRUE)

#We can also apply listwise deletion of units for missing data.
PM.results1 <- PanelMatch(lag = 5, time.id = "year2", unit.id = "inegi", 
                          treatment = "reform", refinement.method = "mahalanobis", 
                          data = final, match.missing = FALSE, listwise.delete = TRUE, # listwise deletion used 
                          covs.formula = ~ I(lag(winning_margin_governor, 1:5)) + I(lag(logdefuncionespc, 1:5)), 
                          size.match = 5, qoi = "att", outcome.var = "logdefuncionespc",
                          lead = 0:3, forbid.treatment.reversal = FALSE, 
                          use.diagonal.variance.matrix = TRUE)

#Let’s try out a weighting method using propensity scores and then compare performance.
PM.results2 <- PanelMatch(lag = 5, time.id = "year2", unit.id = "inegi", 
                          treatment = "reform", refinement.method = "ps.weight", 
                          data = final, match.missing = FALSE, listwise.delete = TRUE, 
                          covs.formula = ~ I(lag(winning_margin_governor, 1:5)) + I(lag(logdefuncionespc, 1:5)), 
                          size.match = 5, qoi = "att", outcome.var = "logdefuncionespc",
                          lead = 0:3, forbid.treatment.reversal = FALSE, 
                          use.diagonal.variance.matrix = TRUE)

#Let's compare matches size
summary(PM.results.none$att)
summary(PM.results$att)
summary(PM.results1$att)

#assess covariate balance 
# get covariate balance for sets that are unrefined

get_covariate_balance(PM.results.none$att,
                      data = final,
                      covariates = c("winning_margin_governor", "logdefuncionespc"),
                      plot = FALSE)


# compare with sets that have had various refinements applied

get_covariate_balance(PM.results$att,
                      data = final,
                      covariates = c("winning_margin_governor", "logdefuncionespc"),
                      plot = FALSE)

get_covariate_balance(PM.results1$att,
                      data = final,
                      covariates = c("winning_margin_governor", "logdefuncionespc"), 
                      plot = FALSE)

get_covariate_balance(PM.results2$att,
                      data = final,
                      covariates = c("winning_margin_governor", "logdefuncionespc"), 
                      plot = FALSE)

#e can also create plots showing covariate balance throughout the lag window 
#period by setting the plot argument to TRUE, as shown below.
get_covariate_balance(PM.results1$att,
                      data = final,
                      covariates = c("winning_margin_governor", "logdefuncionespc"), 
                      plot = TRUE, # visualize by setting plot to TRUE
                      ylim = c(-.2, .2))

get_covariate_balance(PM.results2$att,
                      data = final,
                      covariates = c("winning_margin_governor", "logdefuncionespc"), 
                      plot = TRUE, # visualize by setting plot to TRUE
                      ylim = c(-.2, .2))
#We can also evaluate our results using the balance_scatter function:
balance_scatter(non_refined_set = PM.results.none$att,
                refined_list = list(PM.results$att, PM.results2$att),
                data = final,
                covariates = c("logdefuncionespc", "winning_margin_governor"))
#so refinements are really good 

#results 
PM.none <- PanelEstimate(sets = PM.results.none, data = data.final)
summary(PM.none)
plot(PM.none)
PM <- PanelEstimate(sets = PM.results, data = final)
summary(PM) #similar to the matching with clustering 
plot(PM)
results1 <- PanelEstimate(sets = PM.results1, data = final)
summary(results1) #similar to the matching with clustering 
plot(results1)
results2 <- PanelEstimate(sets = PM.results2, data = final)
summary(results2) #no effect
plot(results2)

#extract weights 
get.weights(ps1,
            stop.method = NULL,
            estimand = NULL,
            withSampW = TRUE)


#######


#detained quality
detained <- PanelMatch(lag = 5, time.id = "year2", unit.id = "inegi", 
                          treatment = "reform", refinement.method = "mahalanobis", # use Mahalanobis distance 
                          data = final, match.missing = TRUE, 
                          covs.formula = ~ I(lag(winning_margin_governor, 1:5)) + I(lag(logdefuncionespc, 1:5)) + I(lag(logdetenidospc, 1:5)), 
                          size.match = 5, qoi = "att" , outcome.var = "logdetenidospc",
                          lead = 0:3, forbid.treatment.reversal = FALSE, 
                          use.diagonal.variance.matrix = TRUE)
detained.results <- PanelEstimate(sets = detained, data = final)
summary(detained.results) #no effect
plot(detained.results)

#heroine 
heroine <- PanelMatch(lag = 5, time.id = "year2", unit.id = "inegi", 
                       treatment = "reform", refinement.method = "mahalanobis", # use Mahalanobis distance 
                       data = final, match.missing = TRUE, 
                       covs.formula = ~ I(lag(winning_margin_governor, 1:5)) + I(lag(logdefuncionespc, 1:5)) + I(lag(logheroina_kg, 1:5)), 
                       size.match = 5, qoi = "att" , outcome.var = "logheroina_kg",
                       lead = 0:3, forbid.treatment.reversal = FALSE, 
                       use.diagonal.variance.matrix = TRUE)
heroine.results <- PanelEstimate(sets = heroine, data = final)
summary(heroine.results) #no effect
plot(heroine.results)

#heroine 
heroine2 <- PanelMatch(lag = 5, time.id = "year2", unit.id = "inegi", 
                      treatment = "reform", refinement.method = "mahalanobis", # use Mahalanobis distance 
                      data = final, match.missing = TRUE, 
                      covs.formula = ~ I(lag(winning_margin_governor, 1:5)) + I(lag(logdefuncionespc, 1:5)) + I(lag(logheroina_kg_2, 1:5)), 
                      size.match = 5, qoi = "att" , outcome.var = "logheroina_kg_2",
                      lead = 0:3, forbid.treatment.reversal = FALSE, 
                      use.diagonal.variance.matrix = TRUE)
heroine2.results <- PanelEstimate(sets = heroine2, data = final)
summary(heroine2.results) #no effect
plot(heroine2.results)

#logmetanfetamina_kg 
meta <- PanelMatch(lag = 5, time.id = "year2", unit.id = "inegi", 
                       treatment = "reform", refinement.method = "mahalanobis", # use Mahalanobis distance 
                       data = final, match.missing = TRUE, 
                       covs.formula = ~ I(lag(winning_margin_governor, 1:5)) + I(lag(logdefuncionespc, 1:5)) + I(lag(logmetanfetamina_kg, 1:5)), 
                       size.match = 5, qoi = "att" , outcome.var = "logmetanfetamina_kg",
                       lead = 0:3, forbid.treatment.reversal = FALSE, 
                       use.diagonal.variance.matrix = TRUE)
meta.results <- PanelEstimate(sets = meta, data = final)
summary(meta.results) #no effect
plot(meta.results)

#laboratories 
labs <- PanelMatch(lag = 5, time.id = "year2", unit.id = "inegi", 
                   treatment = "reform", refinement.method = "mahalanobis", # use Mahalanobis distance 
                   data = final, match.missing = TRUE, 
                   covs.formula = ~ I(lag(winning_margin_governor, 1:5)) + I(lag(logdefuncionespc, 1:5)) + I(lag(loglaboratorio, 1:5)), 
                   size.match = 5, qoi = "att" , outcome.var = "loglaboratorio",
                   lead = 0:3, forbid.treatment.reversal = FALSE, 
                   use.diagonal.variance.matrix = TRUE)
labs.results <- PanelEstimate(sets = labs, data = final)
summary(labs.results) #no effect
plot(labs.results)

#laboratories 
labs2 <- PanelMatch(lag = 5, time.id = "year2", unit.id = "inegi", 
                   treatment = "reform", refinement.method = "mahalanobis", # use Mahalanobis distance 
                   data = final, match.missing = TRUE, 
                   covs.formula = ~ I(lag(winning_margin_governor, 1:5)) + I(lag(logdefuncionespc, 1:5)) + I(lag(loglaboratorio_2, 1:5)), 
                   size.match = 5, qoi = "att" , outcome.var = "loglaboratorio_2",
                   lead = 0:3, forbid.treatment.reversal = FALSE, 
                   use.diagonal.variance.matrix = TRUE)
labs2.results <- PanelEstimate(sets = labs2, data = final)
summary(labs2.results) #no effect
plot(labs2.results)


