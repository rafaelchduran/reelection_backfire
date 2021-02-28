#Reelection backfire: matching 

rm(list=ls())
### Packages
library(tidyverse)
library(PanelMatch)
require(haven)

#working directory:
setwd("~/Dropbox/Dissertation/GovernmentStrategies/Dofiles/")

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
#show treatment
final$year2<-as.integer(final$year)
#as data frame
final<-as.data.frame(final)
#state level
DisplayTreatment(unit.id = "estado", time.id = "year2", legend.position = "none",
                 xlab="year", ylab = "Municipality",
                 treatment="reform", data=final)
dev.copy(png,'../Figures/reform_treatment_statelevel.png')
dev.off()

#municipal level
DisplayTreatment(unit.id = "inegi", time.id = "year2", legend.position = "none",
                 xlab="year", ylab = "Municipality",
                 treatment="reform", data=final, dense.plot = T)
dev.copy(png,'../Figures/reform_treatment_municipallevel.png')
dev.off()

 
#PanelMatch
PM.results.none <- PanelMatch(lag = 5, time.id = "year2", unit.id = "inegi", 
                              treatment = "reform", refinement.method = "none", 
                              data = final, match.missing = TRUE, 
                              size.match = 5, qoi = "att", outcome.var = "logdefuncionespc",
                              lead = 0:3, forbid.treatment.reversal = FALSE, 
                              use.diagonal.variance.matrix = TRUE)
plot(PM.results.none$att)

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
DisplayTreatment(unit.id = "inegi", time.id = "year2", treatment = 'reform', data = final, matched.set = msets[1])

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
PM.none <- PanelEstimate(sets = PM.results.none, data = final)
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

#MANDO UNICO

mando.unico <- PanelMatch(lag = 5, time.id = "year2", unit.id = "inegi", 
                         treatment = "reform", refinement.method = "mahalanobis", # use Mahalanobis distance 
                         data = final, match.missing = TRUE, 
                         covs.formula = ~ logdefuncionespc, 
                         size.match = 5, qoi = "att" , outcome.var = "acuerdo2",
                         lead = 0:3, forbid.treatment.reversal = FALSE, 
                         use.diagonal.variance.matrix = TRUE)
mando.unico.results <- PanelEstimate(sets = mando.unico, data = final)
summary(mando.unico.results) #no effect
plot(mando.unico.results)
dev.copy(png,'../Figures/mando_unico.png')
dev.off()

mando.unico.2 <- PanelMatch(lag = 5, time.id = "year2", unit.id = "inegi", 
                          treatment = "reform", refinement.method = "mahalanobis", # use Mahalanobis distance 
                          data = final, match.missing = TRUE, 
                          covs.formula = ~ I(lag(winning_margin_governor, 1:5)) + I(lag(logdefuncionespc, 1:5)) + I(lag(acuerdo2, 1:5)), 
                          size.match = 5, qoi = "att" , outcome.var = "acuerdo2",
                          lead = 0:3, forbid.treatment.reversal = FALSE, 
                          use.diagonal.variance.matrix = TRUE)
mando.unico.2.results <- PanelEstimate(sets = mando.unico.2, data = final)
summary(mando.unico.2.results) #no effect
plot(mando.unico.2.results)
dev.copy(png,'../Figures/mando_unico2.png')
dev.off()

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


