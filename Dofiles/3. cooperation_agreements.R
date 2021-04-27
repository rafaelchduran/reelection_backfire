#Reelection backfire

rm(list=ls())
### Packages
library(tidyverse)
library(lfe)
library(bacondecomp)
library(MCPanel)
library(kableExtra)
library(multcomp)
library(gsynth)
library(panelView)
library(readr)
require(foreign)
require(haven)
library(stargazer)
#############################################
#Notes
#1. Naive event study: positive for acuerdo, acuerdo2; negative for acuerdo3 and acuerdo4. Not significant and issues with parallel trends.

#############################################


#working directory:
setwd("~/Dropbox/Dissertation/GovernmentStrategies/reelection_backfire/Dofiles/")

#check this:
# https://stackoverflow.com/questions/62881774/formula-with-interaction-terms-in-event-study-designs-using-r

#Load dataset:
data.wleadlags<- read_dta("../../Data/ConstructionDatabase/data_wleads&lags.dta")
data.wleadlags2<- read_dta("../../Data/ConstructionDatabase/data_wleads&lags2.dta")
data.final<- read_dta("../../Data/ConstructionDatabase/data_final.dta")
collapsed.data <- read_dta("../../Data/ConstructionDatabase/collapseddata_forR.dta")
data.final2<-read_dta("../../Data/ConstructionDatabase/data_final2_forR.dta")
data.final2.balanced<-read_dta("../../Data/ConstructionDatabase/data_final2_forR_balanced.dta")
data.as<-read_dta("../../Data/ConstructionDatabase/municipalities_id_2010_2019_whomicideSNSPnew_old_wcovariates_v2.dta")

# make things pretty
theme_set(theme_bw())


#############################################
# Event-study estimates:

# isolate the variables that we want to plot
treatments_short2 <- c("lag_5", "lag_4", "lag_3", "lag_2", "date_0", "lead_1", "lead_2", "lead_3")
treatments_short <- c("lag_7", "lag_6","lag_5", "lag_4", "lag_3", "lag_2", "date_0", "lead_1", "lead_2", "lead_3")
treatments_full <- c("lag_8", "lag_7", "lag_6","lag_5", "lag_4", "lag_3", "lag_2", "date_0", "lead_1", "lead_2", "lead_3")

pre_parties<-c("pri_mayor2_y_1", "pri_mayor2_y_2", "pri_mayor2_y_3", "pri_mayor2_y_4", "pri_mayor2_y_5", "pri_mayor2_y_6", "pri_mayor2_y_7", "pri_mayor2_y_8", 
               "pan_mayor2_y_1", "pan_mayor2_y_2", "pan_mayor2_y_3", "pan_mayor2_y_4", "pan_mayor2_y_5", "pan_mayor2_y_6", "pan_mayor2_y_7", "pan_mayor2_y_8"
)
pre_homicides<-c("logdefuncionespc_mean_y_1", "logdefuncionespc_mean_y_2","logdefuncionespc_mean_y_3", "logdefuncionespc_mean_y_4", "logdefuncionespc_mean_y_5", "logdefuncionespc_mean_y_6", "logdefuncionespc_mean_y_7", "logdefuncionespc_mean_y_8", "logdefuncionespc_mean_y_9")
pre_align_gov<-c("align_gov_y_1", "align_gov_y_2", "align_gov_y_3", "align_gov_y_4", "align_gov_y_5", "align_gov_y_6", "align_gov_y_7", "align_gov_y_8", "align_gov_y_9")
pre_margin_gov<-c("margin_gov_y_1", "margin_gov_y_2", "margin_gov_y_3", "margin_gov_y_4", "margin_gov_y_5", "margin_gov_y_6", "margin_gov_y_7", "margin_gov_y_8", "margin_gov_y_9")
pre_caretels<-c("hayCarteles_y_1", "hayCarteles_y_2", "hayCarteles_y_3", "hayCarteles_y_4", "hayCarteles_y_5", "hayCarteles_y_6", "hayCarteles_y_7", "hayCarteles_y_8", "hayCarteles_y_9")
pre_insecuritypref<-c("ap4_2_3_mean_y_1", "ap4_2_3_mean_y_2", "ap4_2_3_mean_y_3", "ap4_2_3_mean_y_4", "ap4_2_3_mean_y_5", "ap4_2_3_mean_y_6", "ap4_2_3_mean_y_7", "ap4_2_3_mean_y_8", "ap4_2_3_mean_y_9")
pre_winning_mar<-c("winning_margin_mean_y_1", "winning_margin_mean_y_2", "winning_margin_mean_y_3", "winning_margin_mean_y_4", "winning_margin_mean_y_5", "winning_margin_mean_y_6", "winning_margin_mean_y_7", "winning_margin_mean_y_8")
pre_acuerdo<-c("acuerdo_mean_y_1", "acuerdo_mean_y_2", "acuerdo_mean_y_3", "acuerdo_mean_y_4", "acuerdo_mean_y_5", "acuerdo_mean_y_6", "acuerdo_mean_y_7", "acuerdo_mean_y_8", "acuerdo_mean_y_9")
pre_acuerdo2<-c("acuerdo2_mean_y_1", "acuerdo2_mean_y_2", "acuerdo2_mean_y_3", "acuerdo2_mean_y_4", "acuerdo2_mean_y_5", "acuerdo2_mean_y_6", "acuerdo2_mean_y_7", "acuerdo2_mean_y_8", "acuerdo2_mean_y_9")
pre_acuerdo3<-c("acuerdo3_mean_y_1", "acuerdo3_mean_y_2", "acuerdo3_mean_y_3", "acuerdo3_mean_y_4", "acuerdo3_mean_y_5", "acuerdo3_mean_y_6", "acuerdo3_mean_y_7", "acuerdo3_mean_y_8", "acuerdo3_mean_y_9")
pre_acuerdo4<-c("acuerdo4_mean_y_1", "acuerdo4_mean_y_2", "acuerdo4_mean_y_3", "acuerdo4_mean_y_4", "acuerdo4_mean_y_5", "acuerdo4_mean_y_6", "acuerdo4_mean_y_7", "acuerdo4_mean_y_8", "acuerdo4_mean_y_9")
#covariates_all <-do.call(c, list(pre_homicides, pre_align_gov , pre_caretels, pre_insecuritypref, pre_acuerdo))
#covariates_all <-do.call(c, list(pre_homicides, pre_align_gov, pre_margin_gov, pre_caretels, pre_insecuritypref))
covariates_all <-do.call(c, list(pre_homicides, pre_align_gov,pre_margin_gov, pre_caretels, pre_insecuritypref, pre_winning_mar, pre_parties))



# put in the model formula:
##1. w/o covariates
model_formula <- as.formula(paste("acuerdo_estcom ~", 
                                  paste(treatments_full, collapse = " + "),
                                  "| estado + year | 0 | estado")
)

##2. w/ covariates
treatments.paste<-paste(treatments_full, collapse = " + ")
model_formula_cov <- as.formula(paste("acuerdo_estcom ~ lag_8 + lag_7 + lag_6 + lag_5 + lag_4 + lag_3 + lag_2 + date_0 + lead_1 + lead_2 + lead_3 +", 
                                      paste(covariates_all, collapse = " + "), 
                                      "| estado + year | 0 | estado")
)


# run model
#1) w/o covariates
data.final %>% do(fit = felm(model_formula, data = ., 
                                 exactDOF = TRUE, cmethod = "reghdfe")) %>% #omit lag_8 and lag_1, the first one because of collinearity and the second one to have a comparison group
  broom::tidy(fit, conf.int = TRUE) %>% 
  # keep just the variables we are going to plot
  filter(term %in% treatments_full) %>% 
  # reformat d data
  mutate(t = c(-8:-2, 0:3)) %>% 
  subset(select=c(t, estimate, conf.low, conf.high))  %>% 
  # select(df, t, estimate, conf.low, conf.high) %>% 
  # add in data for year -1
  bind_rows(tibble(t = -1, estimate = 0, 
                   conf.low = 0, conf.high = 0
  )) %>% 
  # split the error bands by pre-post
  mutate(band_groups = case_when(
    t < -1 ~ "Pre",
    t >= 0 ~ "Post",
    t == -1 ~ ""
  )) %>% 
  # plot
  ggplot(aes(x = t, y = estimate)) + 
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high, group = band_groups), 
              color = "lightgrey", alpha = 1/2) + 
  geom_point(color = "red") + 
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high)) + 
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = -0.5, linetype = "dashed") + 
  scale_x_continuous(breaks = -7:3) + 
  theme(axis.title.x = element_blank()) + theme_bw()
dev.copy(png,'../Figures/event_study_acuerdo.png')
dev.off()


#2) w/ covariates
data.final %>% do(fit = felm(model_formula_cov, data = ., 
                             exactDOF = TRUE, cmethod = "reghdfe")) %>% #omit lag_8 and lag_1, the first one because of collinearity and the second one to have a comparison group
  broom::tidy(fit, conf.int = TRUE) %>% 
  # keep just the variables we are going to plot
  filter(term %in% treatments_full) %>% 
  # reformat d data
  mutate(t = c(-8:-2, 0:3)) %>% 
  subset(select=c(t, estimate, conf.low, conf.high))  %>% 
  # select(df, t, estimate, conf.low, conf.high) %>% 
  # add in data for year -1
  bind_rows(tibble(t = -1, estimate = 0, 
                   conf.low = 0, conf.high = 0
  )) %>% 
  # split the error bands by pre-post
  mutate(band_groups = case_when(
    t < -1 ~ "Pre",
    t >= 0 ~ "Post",
    t == -1 ~ ""
  )) %>% 
  # plot
  ggplot(aes(x = t, y = estimate)) + 
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high, group = band_groups), 
              color = "lightgrey", alpha = 1/2) + 
  geom_point(color = "red") + 
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high)) + 
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = -0.5, linetype = "dashed") + 
  scale_x_continuous(breaks = -8:3) + 
  theme(axis.title.x = element_blank()) + theme_bw()
dev.copy(png,'../Figures/event_study_wcovariates_acuerdo.png')
dev.off()


#create table
#install.packages("stargazer")
library(stargazer)

acuerdo<- felm(model_formula, data = data.final2, 
                    exactDOF = TRUE, cmethod = "reghdfe")
acuerdo_wcov<- felm(model_formula_cov, data = data.final2, 
                         exactDOF = TRUE, cmethod = "reghdfe")

#summary(loghomicides)$coefficients
event.study<-stargazer(acuerdo, acuerdo_wcov,
                       title=" ", align=TRUE, type = "text",
                       keep=c("lag_7", "lag_6","lag_5", "lag_4", "lag_3", "lag_2", "date_0", "lead_1", "lead_2", "lead_3"),
                       keep.stat=c("n","rsq"),
                       covariate.lables=c("Lag 7","Lag 6", "Lag 5", "Lag 4", "Lag 3", "Lag 2", "Date 0", "Lead 1", "Lead 2", "Lead 3"),
                       out = '../Tables/event_study_table_acuerdo.tex'
)

#############################################
#Abraham and Sun (2021): TO ADDRESS LEADS AND LAGS DEPENDENCE, THAT MAY CREATE A SPURIOUS PARALLEL TREND

# first get percentage contribution to each lead/lag indicator by treatment cohort for weights
# calculate weights
weights <- data.wleadlags %>% 
  # reformat leadlags to long
  pivot_longer(cols = treatments_short, 
               names_to = "whichlead",
               values_to = "ll_value") %>% 
  # filter data to drop missing values of dep variable and keep only effective obs
  filter(ll_value == 1) %>% 
  # get counts
  group_by(adopt_year, whichlead) %>% 
  count %>% 
  # get total count by leadlag and calc percentage
  ungroup() %>% 
  group_by(whichlead) %>% 
  mutate(total = sum(n),
         perc = n / total) %>% 
  # keep just the variables we need
  subset(select=c(whichlead, adopt_year, perc))  %>% 
  #select(whichlead, adopt_year, perc) %>% 
  ungroup()

# make variable name to merge in for indicators. Want only the effective 
# indicators that we actually need to estimate
merge_weights <- weights %>% 
  # grab the unique values of year:leadlag
  subset(select=c(whichlead, adopt_year))  %>%
  #select(whichlead, adopt_year) %>% 
  unique() %>% 
  # make indicator variable
  mutate(indic = paste(whichlead, adopt_year, sep = "__"))

# merge into our dataset and make indicator variables
#data.wleadlags2 <- data.wleadlags2 %>%   filter(!is.na(logdefuncionespc))

AS_data <- data.final2 %>% 
  left_join(., merge_weights, 
            by = c("adopt_year", "whichlead")) %>% 
  # make indicator variables
  mutate(val = 1) %>% 
  pivot_wider(names_from = indic, values_from = val, values_fill = list(val = 0))

# get name of lead/lags in one vector
leadlags <- unique(merge_weights$indic)

# get covariates and make felm formula
covariates_saturated <- paste(leadlags, collapse = "+")

# make fomula to run within our FE specification: this is AS (2021) full saturated model
formula_saturated <- as.formula(paste("acuerdo ~ ", covariates_saturated,  
                                      "| inegi + year | 0 | estado"))

# formula with covariates: 
controls_saturated <- paste(covariates_all, collapse = "+")
formula_saturated_cov <- as.formula(acuerdo ~ 
                                      date_0__2015+lag_2__2015+lag_3__2015+lag_4__2015+lag_5__2015+lead_1__2015+lead_2__2015+lead_3__2015+date_0__2016+lag_2__2016+lag_3__2016+lag_4__2016+lag_5__2016+lag_6__2016+lead_1__2016+lead_2__2016+date_0__2017+lag_2__2017+lag_3__2017+lag_4__2017+lag_5__2017+lag_6__2017+lag_7__2017+lead_1__2017+date_0__2018+lag_2__2018+lag_3__2018+lag_4__2018+lag_5__2018+lag_6__2018+lag_7__2018
                                    + logdefuncionespc_mean_y_1+logdefuncionespc_mean_y_2+logdefuncionespc_mean_y_3+logdefuncionespc_mean_y_4+logdefuncionespc_mean_y_5+logdefuncionespc_mean_y_6+logdefuncionespc_mean_y_7+logdefuncionespc_mean_y_8
                                    +align_gov_y_1+align_gov_y_2+align_gov_y_3+align_gov_y_4+align_gov_y_5+align_gov_y_6+align_gov_y_7+align_gov_y_8
                                    +margin_gov_y_1+margin_gov_y_2+margin_gov_y_3+margin_gov_y_4+margin_gov_y_5+margin_gov_y_6+margin_gov_y_7+margin_gov_y_8
                                    +hayCarteles_y_1 +hayCarteles_y_2+hayCarteles_y_3+hayCarteles_y_4+hayCarteles_y_5+hayCarteles_y_6+hayCarteles_y_7+hayCarteles_y_8
                                    +ap4_2_3_mean_y_1+ap4_2_3_mean_y_2+ap4_2_3_mean_y_3+ap4_2_3_mean_y_4+ap4_2_3_mean_y_5+ap4_2_3_mean_y_6+ap4_2_3_mean_y_7+ap4_2_3_mean_y_8
                                    | inegi + year | 0 | estado)
# isolate the variables that we want to plot; note that lag_1 is the comparison group
covariates_interest <- treatments_short

# make function that will weight the different lead lag estimators 
weight_leadlags <- function(data){
  
  # fit the model
  fit = felm(formula_saturated, data = data, exactDOF = TRUE, cmethod = "reghdfe")
  
  # get the non-missing coefficients
  coefs <- fit$coefficients %>%
    # add in coefficient name to tibble
    as_tibble(rownames = "coef") %>% 
    # pull the first year and the relevant lag
    rowwise() %>% 
    # get the value for which lead/lag and which year
    mutate(whichlead = word(coef, 1, sep = "__"),
           adopt_year = as.numeric(str_sub(coef, -4, -1))) %>% 
    # drop the post variable
    filter(whichlead %in% covariates_interest) %>% 
    # merge in the weights 
    left_join(., weights)
  
  # get the relevant coefficients and weights into a string to get the linear combination
  get_lincom <- function(ll) {
    # get just the coefficients for a specific lead lag
    cf2 <- coefs %>% filter(whichlead == ll)
    # paste the function that goes into the linear combination function
    F <- paste(paste(cf2$perc, cf2$coef, sep = " * ", collapse = " + "), " = 0")
    # take linear combination and put into a data frame
    broom::tidy(
      confint(glht(fit, linfct = F)),
      #confint(linearHypothesis(fit, F, singular.ok = TRUE)),
      conf.int = TRUE
    ) %>% mutate(whichlead = ll)
  }
  # run over all lead/lags
  map_df(covariates_interest, get_lincom) %>% 
    # add time variable
    mutate(t = c(-7:-2, 0:3))
}

# 2. w/ covariates
AS_data %>% 
  # fit the model
  do(fit = weight_leadlags(.)) %>% 
  unnest(fit) %>% 
  subset(select=c(t, estimate, conf.low, conf.high))  %>%
  #select(t, estimate, conf.low, conf.high) %>% 
  # add in data for year -1
  bind_rows(tibble(t = -1, estimate = 0, 
                   conf.low = 0, conf.high = 0
  )) %>% 
  # split the error bands by pre-post
  mutate(band_groups = case_when(
    t < -1 ~ "Pre",
    t >= 0 ~ "Post",
    t == -1 ~ ""
  )) %>%
  # plot
  ggplot(aes(x = t, y = estimate)) + 
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high, group = band_groups), color = "lightgrey", alpha = 1/2) + 
  geom_point(color = "red") + 
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high)) + 
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = -0.5, linetype = "dashed") + 
  scale_x_continuous(breaks = -7:3) + 
  theme(axis.title.x = element_blank()) + theme_bw()
dev.copy(png,'../Figures/CATTs_wcovariates.png')
dev.off()

#CONCLUSION: THERE ARE POSITIVE EFFECTS OF THE REFORM. VERY IMPORTANT TO CONTROL FOR 
##PRETREATMENT GOVERNOR WINNING MARGIN AND FOR HOMICIDES


#############################################
#Cengiz, Dube, Lindner, and Zipperer (2019): I could use this to state that there is no difference in timing.
# get all states treated during our data sample which have at least 2 pre and 2 post observations
obs <- data.final2 %>% 
  filter(!is.na(adopt_year)) %>% 
  group_by(estado) %>% 
  # get number of 0s and 1s
  mutate(num_0 = length(which(reform == 0)),
         num_1 = length(which(reform > 0))) %>% 
  # keep if there are at least 1 pre and 1 post periods
  filter(num_0 >= 1 & num_1 >= 1) %>% 
  pull(estado) %>% 
  unique()

# make fomula to run with reduced datasets
formula_cldz <- as.formula("acuerdo_estcom ~ reform | inegi + year | 0 | estado")
formula_cldz_cov <- as.formula(paste("acuerdo_estcom ~ reform +",
                                     paste(covariates_all, collapse = " + "),
                                     "| inegi + year | 0 | estado"))

formula_cldz_ihs <- as.formula("acuerdo_estcom2 ~ reform | inegi + year | 0 | estado")
formula_cldz_ihs_cov <- as.formula(paste("acuerdo_estcom2 ~ reform +",
                                         paste(covariates_all, collapse = " + "),
                                         "| inegi + year | 0 | estado"))

# formula to calculate state-event-specific effects
rundid <- function(st) {
  
  # get the treatment year
  treat_yr <- data.final2 %>% filter(estado == st) %>% slice(1) %>% pull(adopt_year)
  
  # get a dataset with the the treated state and clean control states, keep only the years -3 to 3
  did_data <- data.final2 %>% 
    # keep treated unit and all units not treated within -5 to 5
    filter(estado == st | is.na(adopt_year) | adopt_year > treat_yr + 3) %>% 
    # keep just year -5, 5
    filter(year >= treat_yr - 1 & year <= treat_yr + 1)
  
  # run regs over the models
  did_data %>%
    do(fit = felm(formula_cldz, data = ., exactDOF = TRUE, cmethod = "reghdfe")) %>%
    broom::tidy(fit, conf.int = TRUE) %>%
    # keep just the indicator variable
    filter(term == "reform") %>% 
    # add in additional needed variables
    mutate(estado = st,
           cohort = treat_yr)
}

rundid_cov <- function(st) {
  
  # get the treatment year
  treat_yr <- data.final2 %>% filter(estado == st) %>% slice(1) %>% pull(adopt_year)
  
  # get a dataset with the the treated state and clean control states, keep only the years -3 to 3
  did_data <- data.final2 %>% 
    # keep treated unit and all units not treated within -5 to 5
    filter(estado == st | is.na(adopt_year) | adopt_year > treat_yr + 3) %>% 
    # keep just year -5, 5
    filter(year >= treat_yr - 1 & year <= treat_yr + 1)
  
  # run regs over the models
  did_data %>%
    do(fit = felm(formula_cldz_cov, data = ., exactDOF = TRUE, cmethod = "reghdfe")) %>%
    broom::tidy(fit, conf.int = TRUE) %>%
    # keep just the indicator variable
    filter(term == "reform") %>% 
    # add in additional needed variables
    mutate(estado = st,
           cohort = treat_yr)
}

rundid_ihs <- function(st) {
  
  # get the treatment year
  treat_yr <- data.final2 %>% filter(estado == st) %>% slice(1) %>% pull(adopt_year)
  
  # get a dataset with the the treated state and clean control states, keep only the years -3 to 3
  did_data <- data.final2 %>% 
    # keep treated unit and all units not treated within -5 to 5
    filter(estado == st | is.na(adopt_year) | adopt_year > treat_yr + 3) %>% 
    # keep just year -5, 5
    filter(year >= treat_yr - 1 & year <= treat_yr + 1)
  
  # run regs over the models
  did_data %>%
    do(fit = felm(formula_cldz_ihs, data = ., exactDOF = TRUE, cmethod = "reghdfe")) %>%
    broom::tidy(fit, conf.int = TRUE) %>%
    # keep just the indicator variable
    filter(term == "reform") %>% 
    # add in additional needed variables
    mutate(estado = st,
           cohort = treat_yr)
}

rundid_ihs_cov <- function(st) {
  
  # get the treatment year
  treat_yr <- data.final2 %>% filter(estado == st) %>% slice(1) %>% pull(adopt_year)
  
  # get a dataset with the the treated state and clean control states, keep only the years -3 to 3
  did_data <- data.final2 %>% 
    # keep treated unit and all units not treated within -5 to 5
    filter(estado == st | is.na(adopt_year) | adopt_year > treat_yr + 3) %>% 
    # keep just year -5, 5
    filter(year >= treat_yr - 1 & year <= treat_yr + 1)
  
  # run regs over the models
  did_data %>%
    do(fit = felm(formula_cldz_ihs_cov, data = ., exactDOF = TRUE, cmethod = "reghdfe")) %>%
    broom::tidy(fit, conf.int = TRUE) %>%
    # keep just the indicator variable
    filter(term == "reform") %>% 
    # add in additional needed variables
    mutate(estado = st,
           cohort = treat_yr)
}


# run over our states
plotdata <- map_df(obs, rundid)
plotdata_cov <- map_df(obs, rundid_cov)
plotdata_ihs <- map_df(obs, rundid_ihs)
plotdata_ihs_cov <- map_df(obs, rundid_ihs_cov)

#Plots
#1. w/o covariates
plotdata %>% 
  mutate(rank = rank(estimate),
         cohort_type = ifelse(cohort <= 2015, "2015", "2016-2018")) %>% 
  ggplot(aes(x = rank, y = estimate, 
             color = factor(cohort_type), group = factor(cohort_type))) + 
  geom_linerange(aes(ymin = conf.low, ymax = conf.high)) + 
  geom_point() + 
  labs(x = "Event (State)", y = "Estimate and 95% CI") + 
  theme_bw() + 
  theme(legend.position = 'bottom',
        legend.title = element_blank()) + 
  geom_hline(yintercept = 0, linetype = "dashed") + 
  coord_flip() + 
  scale_colour_brewer(palette = 'Set1')
dev.copy(png,'../Figures/CDLZ.png')
dev.off()

#2. w/ covariates
plotdata_cov %>% 
  mutate(rank = rank(estimate),
         cohort_type = ifelse(cohort <= 2015, "2015", "2016-2018")) %>% 
  ggplot(aes(x = rank, y = estimate, 
             color = factor(cohort_type), group = factor(cohort_type))) + 
  geom_linerange(aes(ymin = conf.low, ymax = conf.high)) + 
  geom_point() + 
  labs(x = "Event (State)", y = "Estimate and 95% CI") + 
  theme_bw() + 
  theme(legend.position = 'bottom',
        legend.title = element_blank()) + 
  geom_hline(yintercept = 0, linetype = "dashed") + 
  coord_flip() + 
  scale_colour_brewer(palette = 'Set1')
dev.copy(png,'../Figures/CDLZ_cov_acuerdo.png')
dev.off()


#3.IHS w/o covariates
plotdata_ihs %>% 
  mutate(rank = rank(estimate),
         cohort_type = ifelse(cohort <= 2015, "2015", "2016-2018")) %>% 
  ggplot(aes(x = rank, y = estimate, 
             color = factor(cohort_type), group = factor(cohort_type))) + 
  geom_linerange(aes(ymin = conf.low, ymax = conf.high)) + 
  geom_point() + 
  labs(x = "Event (State)", y = "Estimate and 95% CI") + 
  theme_bw() + 
  theme(legend.position = 'bottom',
        legend.title = element_blank()) + 
  geom_hline(yintercept = 0, linetype = "dashed") + 
  coord_flip() + 
  scale_colour_brewer(palette = 'Set1')
dev.copy(png,'../Figures/CDLZ_ihs.png')
dev.off()

#4.IHS w/ covariates
plotdata_ihs_cov %>% 
  mutate(rank = rank(estimate),
         cohort_type = ifelse(cohort <= 2015, "2015", "2016-2018")) %>% 
  ggplot(aes(x = rank, y = estimate, 
             color = factor(cohort_type), group = factor(cohort_type))) + 
  geom_linerange(aes(ymin = conf.low, ymax = conf.high)) + 
  geom_point() + 
  labs(x = "Event (State)", y = "Estimate and 95% CI") + 
  theme_bw() + 
  theme(legend.position = 'bottom',
        legend.title = element_blank()) + 
  geom_hline(yintercept = 0, linetype = "dashed") + 
  coord_flip() + 
  scale_colour_brewer(palette = 'Set1')
dev.copy(png,'../Figures/CDLZ_ihs_cov_acuerdo2.png')
dev.off()

###############
#Stacked dataset analysis from CLDZ

# get the cohort years
obs <- data.final2 %>% 
  filter(!is.na(adopt_year)) %>% 
  pull(adopt_year) %>% 
  unique()

# make fomula to run within our FE specification
# get the lead lags in one set of covariates
leadlags <- c("lag_8", "lag_7", "lag_6", "lag_5", "lag_4", "lag_3", "lag_2", "date_0",
              "lead_1", "lead_2", "lead_3")

# Make the estimating equation
controls <- paste(covariates_all, collapse = "+")

formula_cldz <- as.formula(paste("acuerdo_estcom ~ ",
                                 paste(leadlags, collapse= "+"),
                                 "| factor(inegi):factor(df) + factor(year):factor(df) | 0 | estado"))


leadlags_all <- paste(leadlags, collapse = "+")

formula_cldz2 <- as.formula(paste("acuerdo_estcom2 ~ lag_8+lag_7+lag_6+lag_5+lag_4+lag_3+lag_2+date_0+lead_1+lead_2+lead_3+",
                                  paste(covariates_all, collapse= "+"),
                                  "| factor(inegi):factor(df) + factor(year):factor(df) | 0 | estado"))

formula_cldz2 <- as.formula(paste("acuerdo_estcom2 ~ lag_8+lag_7+lag_6+lag_5+lag_4+lag_3+lag_2+date_0+lead_1+lead_2+lead_3+",
                                  paste(covariates_all, collapse= "+"),
                                  "| inegi + year | 0 | estado"))


# make formula to create the dataset
getdata <- function(i) {
  
  #keep what we need
  data.final2 %>% 
    # keep treated units and all units not treated within -5 to 5
    filter(adopt_year == i | is.na(adopt_year) | adopt_year > i + 3) %>% 
    # keep just year -3 to 3
    filter(year >= i - 8 & year <= i + 3) %>%
    # create an indicator for the dataset
    mutate(df = i) %>% 
    # replace lead/lag indicators if not in the treatment cohort
    mutate(
      lag_8 = ifelse(is.na(adopt_year) | adopt_year != df, 0, lag_8),
      lag_7 = ifelse(is.na(adopt_year) | adopt_year != df, 0, lag_7),
      lag_6 = ifelse(is.na(adopt_year) | adopt_year != df, 0, lag_6),     
      lag_5 = ifelse(is.na(adopt_year) | adopt_year != df, 0, lag_5),
      lag_4 = ifelse(is.na(adopt_year) | adopt_year != df, 0, lag_4),
      lag_3 = ifelse(is.na(adopt_year) | adopt_year != df, 0, lag_3),
      lag_2 = ifelse(is.na(adopt_year) | adopt_year != df, 0, lag_2),
      date_0 = ifelse(is.na(adopt_year) | adopt_year != df, 0, date_0),
      lead_1 = ifelse(is.na(adopt_year) | adopt_year != df, 0, lead_1),
      lead_2 = ifelse(is.na(adopt_year) | adopt_year != df, 0, lead_2),
      lead_3 = ifelse(is.na(adopt_year) | adopt_year != df, 0, lead_3))
}

# get data stacked
stacked_data <- map_df(obs, getdata)

#1. w/o controls
stacked_data %>% 
  # fit the model
  do(fit = felm(formula_cldz, data = ., exactDOF = TRUE, cmethod = "reghdfe")) %>% 
  broom::tidy(fit, conf.int = TRUE) %>% 
  # keep just the variables we are going to plot
  filter(term %in% covariates_interest) %>% 
  # make a relative time variable
  mutate(t = c(-7:-2, 0:3)) %>% 
  subset(select=c(t, estimate, conf.low, conf.high))  %>% 
  #select(t, estimate, conf.low, conf.high) %>% 
  # add in data for year -1
  bind_rows(tibble(t = -1, estimate = 0, 
                   conf.low = 0, conf.high = 0
  )) %>% 
  # split the error bands by pre-post
  mutate(band_groups = case_when(
    t < -1 ~ "Pre",
    t >= 0 ~ "Post",
    t == -1 ~ ""
  )) %>%
  # plot
  ggplot(aes(x = t, y = estimate, group = band_groups)) + 
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), color = "lightgrey", alpha = 1/2) + 
  geom_point(color = "red") + 
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high)) + 
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = -0.5, linetype = "dashed") + 
  scale_x_continuous(breaks = -7:3) + 
  theme_bw()
dev.copy(png,'../Figures/stacked_dataset_acuerdo.png')
dev.off()


#2. w/ controls
stacked_data %>% 
  # fit the model
  do(fit = felm(formula_cldz2, data = ., exactDOF = TRUE, cmethod = "reghdfe")) %>% 
  broom::tidy(fit, conf.int = TRUE) %>% 
  # keep just the variables we are going to plot
  filter(term %in% covariates_interest) %>% 
  # make a relative time variable
  mutate(t = c(-7:-2, 0:3)) %>% 
  subset(select=c(t, estimate, conf.low, conf.high))  %>% 
  #select(t, estimate, conf.low, conf.high) %>% 
  # add in data for year -1
  bind_rows(tibble(t = -1, estimate = 0, 
                   conf.low = 0, conf.high = 0
  )) %>% 
  # split the error bands by pre-post
  mutate(band_groups = case_when(
    t < -1 ~ "Pre",
    t >= 0 ~ "Post",
    t == -1 ~ ""
  )) %>%
  # plot
  ggplot(aes(x = t, y = estimate, group = band_groups)) + 
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), color = "lightgrey", alpha = 1/2) + 
  geom_point(color = "red") + 
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high)) + 
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = -0.5, linetype = "dashed") + 
  scale_x_continuous(breaks = -7:3) + 
  theme_bw()
dev.copy(png,'../Figures/stacked_dataset_wcontrols_acuerdo.png')
dev.off()


###############
#Callaway and Sant'Ana (2019)
#https://bcallaway11.github.io/did/articles/did-basics.html 
#install.packages("did")
#devtools::install_github("bcallaway11/did")
library(did)

# need to replace the adopt year to 0 if missing

data_CS <- data.final2 %>% 
  mutate(adopt_year = ifelse(is.na(adopt_year), 0, adopt_year))

data_CS <- data_CS %>% 
  filter(!is.na(acuerdo))

#new data:
data_CS2 <- data.final2 %>% 
  mutate(adopt_year = ifelse(is.na(adopt_year), 0, adopt_year))
data_CS2 <- data_CS2 %>% 
  filter(!is.na(acuerdo))

# run the CS algorithm
CS_out <- att_gt("acuerdo3", idname="inegi", tname="year",
                 data = data_CS,
                 first.treat.name="adopt_year", panel = T, control.group = "nottreated",
                 alp = 0.05, cband=T, bstrap=T, biters = 1000,
                 printdetails = F, clustervars = "estado")


CS_out.cov <- att_gt("acuerdo", idname="inegi", tname="year",
                     data = data_CS2,
                     #xformla = logdefuncionespc_mean_y_1+logdefuncionespc_mean_y_2+logdefuncionespc_mean_y_3+logdefuncionespc_mean_y_4+logdefuncionespc_mean_y_5+logdefuncionespc_mean_y_6+logdefuncionespc_mean_y_7+logdefuncionespc_mean_y_8
                     #+align_gov_y_1+align_gov_y_2+align_gov_y_3+align_gov_y_4+align_gov_y_5+align_gov_y_6+align_gov_y_7+align_gov_y_8
                     #+margin_gov_y_1+margin_gov_y_2+margin_gov_y_3+margin_gov_y_4+margin_gov_y_5+margin_gov_y_6+margin_gov_y_7+margin_gov_y_8
                     #+hayCarteles_y_1 +hayCarteles_y_2+hayCarteles_y_3+hayCarteles_y_4+hayCarteles_y_5+hayCarteles_y_6+hayCarteles_y_7+hayCarteles_y_8
                     #+ap4_2_3_mean_y_1+ap4_2_3_mean_y_2+ap4_2_3_mean_y_3+ap4_2_3_mean_y_4+ap4_2_3_mean_y_5+ap4_2_3_mean_y_6+ap4_2_3_mean_y_7+ap4_2_3_mean_y_8,
                     #xformla = ~logdefuncionespc_mean + hayCarteles+ap4_2_3_mean + margin_gov + align_gov ,                     
                     xformla = ~logdefuncionespc_mean + hayCarteles+ap4_2_3_mean + margin_gov  + align_gov , 
                     first.treat.name="adopt_year", panel = T, control.group = "notyettreated",
                     alp = 0.05, cband=T, bstrap=T, biters = 1000,
                     printdetails = F, clustervars = "estado")

summary(CS_out)
summary(CS_out.cov)

# plot the results
ggdid(CS_out)
ggdid(CS_out.cov)

#aggregation
agg.simple <- aggte(CS_out)
summary(agg.simple)

agg.simple <- aggte(CS_out.ihs)
summary(agg.simple)

#Dynamic Effects and Event Studies
agg.es <- aggte(CS_out, type="dynamic")
summary(agg.es)
ggdid(agg.es)

agg.es2 <- aggte(CS_out.cov, type="dynamic")
summary(agg.es2)
ggdid(agg.es2)

#Selective Treatment Timing / Group-Specific Effects
agg.gs <- aggte(CS_out, type="selective")
summary(agg.gs)
ggdid(agg.gs)

#Alternative Estimation Methods
example.attgt.reg <- att_gt(yname="logdefuncionespc",
                            tname="year",
                            idname="inegi",
                            first.treat.name="adopt_year",
                            data=data_CS,
                            printdetails=FALSE,
                            estMethod="reg"
)
summary(example.attgt.reg)
ggdid(example.attgt.reg)

#Table:
#1. w/o covariates
tibble(
  t = -6:3,
  estimate = agg.es$att.egt,
  se = agg.es$se.egt,
  conf.low = estimate - 1.96*se,
  conf.high = estimate + 1.96*se) %>% 
  # split the error bands by pre-post
  mutate(band_groups = case_when(
    t < 0 ~ "Pre",
    t >= 1 ~ "Post",
    t == 0 ~ ""
  )) %>%
  # plot
  ggplot(aes(x = t, y = estimate, group = band_groups)) + 
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), color = "lightgrey", alpha = 1/2) + 
  geom_point(color = "red") + 
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high)) + 
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = -0.5, linetype = "dashed") + 
  scale_x_continuous(breaks = -6:3) + 
  theme(axis.title.x = element_blank()) + 
  theme_bw()
dev.copy(png,'../Figures/callaway_santana_acuerdo.png')
dev.off()


#2. w/ covariates
tibble(
  t = -6:3,
  estimate = agg.es2$att.egt,
  se = agg.es2$se.egt,
  conf.low = estimate - 1.96*se,
  conf.high = estimate + 1.96*se)%>%
  #add the reference group
  #bind_rows(tibble(t = 0, estimate = 0, 
  #      conf.low = 0, conf.high = 0
  #)) %>% 
  # split the error bands by pre-post
  mutate(band_groups = case_when(
    t < 0 ~ "Pre",
    t >= 1 ~ "Post",
    t == 0 ~ ""
  )) %>%
  # plot
  ggplot(aes(x = t, y = estimate, group = band_groups)) + 
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), color = "lightgrey", alpha = 1/2) + 
  geom_point(color = "red") + 
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high)) + 
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = -0.5, linetype = "dashed") + 
  scale_x_continuous(breaks = -6:3) + 
  theme(axis.title.x = element_blank()) + 
  theme_bw()
dev.copy(png,'../Figures/callaway_santana_wcovariates.png')
dev.off()

#CONCLUSION: controls matter a lot. There is a big difference between treated cohorts
##Early cohort are more negative, specially 2017. 2018 is always positive. 

###############
#NON-DID METHODS
###############
#1) GENERALIZED SYNTHETIC CONTROLS
#http://yiqingxu.org/software/gsynth/gsynth_examples.html
#install.packages('gsynth', type = 'source')
library(gsynth)

#erase missing values
data_noNA <- data.final2 %>% 
  filter(!is.na(acuerdo_estcom)) 

data_noNAdet <- data.final2 %>% 
  filter(!is.na(acuerdo2))

data_noNAinc <- data.final2 %>% 
  filter(!is.na(acuerdo3)) 

data_noNAinc2 <- data.final2 %>% 
  filter(!is.na(acuerdo4)) 

#Fix
cl <- parallel::makeCluster(2, setup_strategy = "sequential")
# ## WORKAROUND: https://github.com/rstudio/rstudio/issues/6692
## Revert to 'sequential' setup of PSOCK cluster in RStudio Console on macOS and R 4.0.0
if (Sys.getenv("RSTUDIO") == "1" && !nzchar(Sys.getenv("RSTUDIO_TERM")) && 
    Sys.info()["sysname"] == "Darwin" && getRversion() == "4.0.0") {
  parallel:::setDefaultClusterOptions(setup_strategy = "sequential")
}

#another fix
# no_cores <- detectCores() - 1
#install.packages("doParallel")
# library(doParallel)
# create the cluster for caret to use
#cl <- makePSOCKcluster(no_cores)
#cl <- parallel::makeCluster(no_cores, setup_strategy = "sequential")
#registerDoParallel(cl)

# estimate the generalized synthetic control method
##1) Using log transformation
out.acuerdo <- gsynth(acuerdo_estcom ~ reform, data = data_noNA, index = c("inegi", "year"),
              force = "two-way", CV = TRUE, se = TRUE, nboots = 1000, cores = 8, r = c(0, 3), 
              min.T0 = 5)


out.acuerdo_cov <- gsynth(acuerdo_estcom ~ reform, X= paste(covariates_all, collapse= "+"), 
                  data = data_noNA, index = c("inegi", "year"),
                  force = "two-way", CV = TRUE, se = TRUE, nboots = 1000, cores = 8, r = c(0, 3), 
                  inference = "nonparametric",
                  min.T0 = 5)


##Other agreements measured:
out.acuerdo2 <- gsynth(acuerdo2 ~ reform, data = data_noNAdet, index = c("inegi", "year"),
                        force = "two-way", CV = TRUE, se = TRUE, nboots = 1000, cores = 8, r = c(0, 3), 
                        min.T0 = 5)
out.acuerdo3 <- gsynth(acuerdo3 ~ reform, data = data_noNAinc, index = c("inegi", "year"),
                       force = "two-way", CV = TRUE, se = TRUE, nboots = 1000, cores = 8, r = c(0, 3), 
                       min.T0 = 5)
out.acuerdo4 <- gsynth(acuerdo4 ~ reform, data = data_noNAinc2, index = c("inegi", "year"),
                       force = "two-way", CV = TRUE, se = TRUE, nboots = 1000, cores = 8, r = c(0, 3), 
                       min.T0 = 5)

summary(out)
summary(out_cov)

#plot
##1) log homicides per capita: 
##without covariates:
out.acuerdo$est.att %>% 
  as_tibble(rownames = "t") %>% 
  mutate(t = as.numeric(t)) %>% 
  ggplot(aes(x = t, y = ATT)) + 
  geom_ribbon(aes(ymin = CI.lower, ymax = CI.upper), color = "lightgrey", alpha = 1/2) + 
  geom_line(color = "red") + 
  geom_point(color = "red") + 
  geom_hline(yintercept = 0) + 
  geom_vline(xintercept = -0.5, linetype = "dashed") + 
  theme(axis.title.x = element_text("Time relative to Treatment (2014)")) + 
  theme_bw() 
dev.copy(png,'../Figures/gsynth_acuerdo.png')
dev.off()

##with covariates:
out.acuerdo_cov$est.att %>% 
  as_tibble(rownames = "t") %>% 
  mutate(t = as.numeric(t)) %>% 
  ggplot(aes(x = t, y = ATT)) + 
  geom_ribbon(aes(ymin = CI.lower, ymax = CI.upper), color = "lightgrey", alpha = 1/2) + 
  geom_line(color = "red") + 
  geom_point(color = "red") + 
  geom_hline(yintercept = 0) + 
  geom_vline(xintercept = -0.5, linetype = "dashed") + 
  theme(axis.title.x = element_text("Time relative to Treatment (2014)")) + 
  theme_bw() 
dev.copy(png,'../Figures/gsynth_wcov_acuerdo.png')
dev.off()

#Other measures
out.acuerdo2$est.att %>% 
  as_tibble(rownames = "t") %>% 
  mutate(t = as.numeric(t)) %>% 
  ggplot(aes(x = t, y = ATT)) + 
  geom_ribbon(aes(ymin = CI.lower, ymax = CI.upper), color = "lightgrey", alpha = 1/2) + 
  geom_line(color = "red") + 
  geom_point(color = "red") + 
  geom_hline(yintercept = 0) + 
  geom_vline(xintercept = -0.5, linetype = "dashed") + 
  theme(axis.title.x = element_text("Time relative to Treatment (2014)")) + 
  theme_bw() 
dev.copy(png,'../Figures/gsynth_acuerdo2.png')
dev.off()

out.acuerdo3$est.att %>% 
  as_tibble(rownames = "t") %>% 
  mutate(t = as.numeric(t)) %>% 
  ggplot(aes(x = t, y = ATT)) + 
  geom_ribbon(aes(ymin = CI.lower, ymax = CI.upper), color = "lightgrey", alpha = 1/2) + 
  geom_line(color = "red") + 
  geom_point(color = "red") + 
  geom_hline(yintercept = 0) + 
  geom_vline(xintercept = -0.5, linetype = "dashed") + 
  theme(axis.title.x = element_text("Time relative to Treatment (2014)")) + 
  theme_bw() 
dev.copy(png,'../Figures/gsynth_acuerdo3.png')
dev.off()

out.acuerdo4$est.att %>% 
  as_tibble(rownames = "t") %>% 
  mutate(t = as.numeric(t)) %>% 
  ggplot(aes(x = t, y = ATT)) + 
  geom_ribbon(aes(ymin = CI.lower, ymax = CI.upper), color = "lightgrey", alpha = 1/2) + 
  geom_line(color = "red") + 
  geom_point(color = "red") + 
  geom_hline(yintercept = 0) + 
  geom_vline(xintercept = -0.5, linetype = "dashed") + 
  theme(axis.title.x = element_text("Time relative to Treatment (2014)")) + 
  theme_bw() 
dev.copy(png,'../Figures/gsynth_acuerdo4.png')
dev.off()

#counterfactual plot
plot(out.acuerdo, type = "ct", raw = "none", main = "", ylab = "Sec. Coop. Agreeements",
     xlab="Time relative to Treatment (2014)", shade.post = T)
dev.copy(png,'../Figures/gsynth_counterfactual_acuerdo.png')
dev.off()

plot(out.acuerdo_cov, type = "ct", raw = "none", main = "", ylab = "Sec. Coop. Agreeements",
     xlab="Time relative to Treatment (2014)", shade.post = T)
dev.copy(png,'../Figures/gsynth_wcov_counterfactual_acuerdo.png')
dev.off()

plot(out.acuerdo2, type = "ct", raw = "none", main = "", ylab = "Sec. Coop. Agreeements",
     xlab="Time relative to Treatment (2014)", shade.post = T)
dev.copy(png,'../Figures/gsynth_counterfactual_acuerdo2.png')
dev.off()

plot(out.acuerdo3, type = "ct", raw = "none", main = "", ylab = "Sec. Coop. Agreeements",
     xlab="Time relative to Treatment (2014)", shade.post = T)
dev.copy(png,'../Figures/gsynth_counterfactual_acuerdo3.png')
dev.off()

plot(out.acuerdo4, type = "ct", raw = "none", main = "", ylab = "Sec. Coop. Agreeements",
     xlab="Time relative to Treatment (2014)", shade.post = T)
dev.copy(png,'../Figures/gsynth_counterfactual_acuerdo3.png')
dev.off()
###############
#2) Matrix completion

#Remove the following units:
#dropthis<-c(1002, 1003, 1004, 1005, 1006, 1007, 1008, 1009, 1010, 1011, 3001, 3002, 3003, 3008, 3009, 4001, 4002, 4003, 4004, 4005, 4006, 4007, 4008, 4009, 4011, 5016, 5020, 5031, 5038, 6001, 6002, 6003, 6004, 6005, 6006, 6007, 6008, 6009, 6010, 7001, 7002, 7003, 7004, 7006, 7007, 7008, 7009, 7010, 7011, 7012, 7013, 7014, 7015, 7016, 7017, 7018, 7019, 7020, 7021, 7022, 7023, 7024, 7025, 7026, 7027, 7028, 7029, 7030, 7031, 7032, 7033, 7034, 7035, 7036, 7037, 7038, 7039, 7040, 7041, 7042, 7043, 7044, 7045, 7046, 7047, 7048, 7049, 7050, 7051, 7052, 7053, 7054, 7055, 7056, 7057, 7058, 7059, 7060, 7061, 7062, 7063, 7064, 7065, 7066, 7067, 7068, 7069, 7071, 7072, 7073, 7074, 7075, 7076, 7077, 7078, 7079, 7080, 7081, 7082, 7083, 7084, 7085, 7086, 7087, 7088, 7089, 7090, 7091, 7092, 7093, 7094, 7096, 7097, 7098, 7099, 7100, 7101, 7102, 7103, 7104, 7105, 7106, 7107, 7108, 7109, 7110, 7111, 7112, 8004, 8028, 8038, 8053, 8054, 8065, 8067, 10002, 10011, 10012, 10013, 10016, 10018, 10022, 10027, 10031, 10032, 10034, 11001, 11002, 11003, 11004, 11005, 11006, 11007, 11008, 11009, 11010, 11011, 11012, 11013, 11014, 11015, 11016, 11017, 11018, 11019, 11020, 11021, 11022, 11023, 11024, 11025, 11026, 11027, 11028, 11029, 11030, 11031, 11032, 11033, 11034, 11035, 11036, 11037, 11038, 11039, 11040, 11041, 11042, 11043, 11044, 11045, 11046, 12001, 12002, 12003, 12004, 12005, 12006, 12007, 12008, 12009, 12010, 12011, 12012, 12013, 12014, 12015, 12016, 12017, 12018, 12019, 12020, 12021, 12022, 12023, 12024, 12025, 12026, 12027, 12028, 12029, 12030, 12031, 12032, 12033, 12034, 12035, 12036, 12037, 12039, 12040, 12041, 12042, 12043, 12044, 12045, 12046, 12047, 12048, 12049, 12050, 12051, 12052, 12053, 12054, 12055, 12056, 12057, 12058, 12059, 12060, 12061, 12062, 12063, 12064, 12065, 12066, 12067, 12068, 12069, 12070, 12071, 12072, 12073, 12074, 12075, 12076, 12077, 12078, 12079, 12080, 12081, 14001, 14002, 14003, 14004, 14005, 14006, 14007, 14008, 14009, 14010, 14011, 14012, 14013, 14014, 14015, 14016, 14017, 14018, 14019, 14020, 14021, 14022, 14023, 14024, 14025, 14026, 14027, 14028, 14029, 14030, 14031, 14032, 14033, 14034, 14035, 14036, 14037, 14038, 14039, 14040, 14041, 14042, 14043, 14044, 14045, 14046, 14047, 14048, 14049, 14050, 14051, 14052, 14053, 14054, 14055, 14056, 14057, 14058, 14059, 14060, 14061, 14062, 14063, 14064, 14065, 14066, 14067, 14068, 14069, 14070, 14071, 14072, 14073, 14074, 14075, 14076, 14077, 14078, 14079, 14080, 14081, 14082, 14083, 14084, 14085, 14086, 14087, 14088, 14089, 14090, 14091, 14092, 14093, 14094, 14095, 14096, 14097, 14098, 14099, 14100, 14101, 14102, 14103, 14104, 14105, 14106, 14107, 14108, 14109, 14110, 14111, 14112, 14113, 14114, 14115, 14116, 14117, 14118, 14119, 14120, 14121, 14122, 14123, 14124, 14125, 15001, 15002, 15003, 15004, 15005, 15006, 15007, 15008, 15009, 15010, 15011, 15012, 15013, 15014, 15015, 15016, 15017, 15018, 15019, 15020, 15021, 15022, 15023, 15024, 15025, 15026, 15027, 15028, 15029, 15030, 15031, 15032, 15033, 15034, 15035, 15036, 15037, 15038, 15039, 15040, 15041, 15042, 15043, 15044, 15045, 15046, 15047, 15048, 15049, 15050, 15051, 15052, 15053, 15054, 15055, 15056, 15057, 15058, 15059, 15060, 15061, 15062, 15063, 15064, 15065, 15066, 15067, 15068, 15069, 15070, 15071, 15072, 15073, 15074, 15075, 15076, 15077, 15078, 15079, 15080, 15081, 15082, 15083, 15084, 15085, 15086, 15087, 15088, 15089, 15090, 15091, 15092, 15093, 15094, 15095, 15096, 15097, 15098, 15099, 15100, 15101, 15102, 15103, 15104, 15105, 15106, 15107, 15108, 15109, 15110, 15111, 15112, 15113, 15114, 15115, 15116, 15117, 15118, 15119, 15120, 15121, 15122, 15123, 15124, 15125, 16001, 16002, 16003, 16004, 16005, 16006, 16007, 16008, 16009, 16010, 16011, 16012, 16013, 16014, 16015, 16016, 16017, 16018, 16019, 16020, 16021, 16022, 16023, 16024, 16025, 16026, 16027, 16028, 16029, 16030, 16031, 16032, 16033, 16034, 16035, 16036, 16037, 16038, 16039, 16040, 16041, 16042, 16043, 16044, 16045, 16046, 16047, 16048, 16049, 16050, 16051, 16052, 16053, 16054, 16055, 16056, 16057, 16058, 16059, 16060, 16061, 16062, 16063, 16064, 16065, 16066, 16067, 16068, 16069, 16070, 16071, 16072, 16073, 16074, 16075, 16076, 16077, 16078, 16079, 16080, 16081, 16082, 16083, 16084, 16085, 16086, 16087, 16088, 16089, 16090, 16091, 16092, 16093, 16094, 16095, 16096, 16097, 16098, 16099, 16100, 16101, 16102, 16103, 16104, 16105, 16106, 16107, 16108, 16109, 16110, 16111, 16112, 16113, 17001, 17002, 17003, 17004, 17005, 17006, 17007, 17008, 17009, 17010, 17011,
            #17012,17013, 17014, 17015, 17016, 17017, 17018, 17019, 17020, 17021, 17022, 17023, 17024, 17025, 17026, 17027, 17028, 17029, 17030, 17031, 17032, 17033, 19001, 19002, 19003, 19004, 19005, 19006, 19007, 19008, 19009, 19010, 19011, 19012, 19013, 19014, 19015, 19016, 19017, 19018, 19019, 19020, 19021, 19022, 19023, 19024, 19025, 19026, 19027, 19028, 19029, 19030, 19031, 19032, 19033, 19034, 19035, 19036, 19037, 19038, 19039, 19040, 19041, 19042, 19043, 19044, 19045, 19046, 19047, 19048, 19049, 19050, 19051, 20009, 20013, 20020, 20025, 20041, 20044, 20057, 20065, 20087, 20107, 20124, 20172, 20198, 20207, 20212, 20242, 20245, 20272, 20282, 20325, 20333, 20334, 20376, 20439, 20499, 20517, 20554, 21028, 21049, 21156, 21170, 21212, 22001, 22002, 22003, 22004, 22005, 22006, 22007, 22008, 22009, 22010, 22011, 22012, 22013, 22014, 22015, 22016, 22017, 22018, 24001, 24002, 24003, 24004, 24005, 24006, 24007, 24008, 24009, 24010, 24011, 24012, 24013, 24014, 24015, 24016, 24017, 24018, 24019, 24020, 24021, 24022, 24023, 24024, 24025, 24026, 24027, 24028, 24029, 24030, 24031, 24032, 24033, 24034, 24035, 24036, 24037, 24038, 24039, 24040, 24041, 24042, 24043, 24044, 24045, 24046, 24047, 24048, 24049, 24050, 24051, 24052, 24053, 24054, 24055, 24056, 24057, 24058, 25003, 25004, 25010, 25012, 25017, 27001, 27002, 27003, 27005, 27006, 27007, 27008, 27009, 27010, 27011, 27012, 27013, 27014, 27015, 27016, 27017, 28003, 28004, 28014, 28019, 28024, 28025, 28027, 28028, 28032, 28034, 28036, 28038, 28043, 31001, 31002, 31003, 31004, 31005, 31006, 31007, 31008, 31010, 31011, 31012, 31013, 31016, 31017, 31018, 31019, 31020, 31021, 31023, 31024, 31026, 31027, 31028, 31029, 31030, 31031, 31033, 31034, 31035, 31037, 31038, 31040, 31041, 31042, 31044, 31045, 31046, 31047, 31048, 31049, 31050, 31051, 31052, 31053, 31054, 31055, 31056, 31057, 31058, 31059, 31061, 31062, 31063, 31064, 31066, 31067, 31068, 31069, 31072, 31073, 31074, 31076, 31077, 31079, 31080, 31081, 31082, 31084, 31085, 31087, 31088, 31089, 31090, 31091, 31092, 31093, 31096, 31097, 31098, 31099, 31100, 31101, 31102, 31103, 31104, 31106, 32009, 32010, 32013, 32014, 32015, 32023, 32034, 32040, 32046, 32057)

#drop the municipalities with few pre-treatment observations:
## remove those from dropthis list:
#data_noNA_nomuns<-data_noNA[apply(data_noNA, 1, function(x)  !any(x %in% dropthis)),]
## remove those with few observations:
#data_noNA_nomuns<-data_noNA_nomuns %>% group_by(inegi) %>% filter(n()>= 4) %>% ungroup()


# estimate the generalized synthetic control method
out2.acuerdo <- gsynth(acuerdo_estcom ~ reform, data = data_noNA, index = c("inegi", "year"), 
               estimator = "mc",
               force = "two-way", CV = TRUE, se = TRUE, nboots = 1000, cores = 8, r = c(0, 3), 
               min.T0 = 5)
out2_wcov.acuerdo <- gsynth(acuerdo_estcom ~ reform, X= paste(covariates_all, collapse= "+"), 
                    data = data_noNA, index = c("inegi", "year"), 
                    estimator = "mc",
                    force = "two-way", CV = TRUE, se = TRUE, nboots = 1000, cores = 8, r = c(0, 3), 
                    min.T0 = 5)
##takes an hour to run

#plot
out2.acuerdo$est.att %>% 
  as_tibble(rownames = "t") %>% 
  mutate(t = as.numeric(t)) %>% 
  ggplot(aes(x = t, y = ATT)) + 
  geom_ribbon(aes(ymin = CI.lower, ymax = CI.upper), color = "lightgrey", alpha = 1/2) + 
  geom_line(color = "red") + 
  geom_point(color = "red") + 
  geom_hline(yintercept = 0) + 
  geom_vline(xintercept = -0.5, linetype = "dashed") + 
  theme(axis.title.x = element_text("Time relative to Treatment (2014)")) + 
  theme_bw() 
dev.copy(png,'../Figures/matrix_completion_acuerdo.png')
dev.off()

#plot
out2_wcov.acuerdo$est.att %>% 
  as_tibble(rownames = "t") %>% 
  mutate(t = as.numeric(t)) %>% 
  ggplot(aes(x = t, y = ATT)) + 
  geom_ribbon(aes(ymin = CI.lower, ymax = CI.upper), color = "lightgrey", alpha = 1/2) + 
  geom_line(color = "red") + 
  geom_point(color = "red") + 
  geom_hline(yintercept = 0) + 
  geom_vline(xintercept = -0.5, linetype = "dashed") + 
  theme(axis.title.x = element_text("Time relative to Treatment (2014)")) + 
  theme_bw() 
dev.copy(png,'../Figures/matrix_completion_wcovariates_acuerdo.png')
dev.off()


#############################################
#Goodman-Bacon decomposition
# calculate the bacon decomposition without covariates

bacon_out <- bacon(acuerdo ~ reform,
                   data = data.final2.balanced,
                   id_var = "estado",
                   time_var = "year")

# plot
bacon_out %>% 
  ggplot(aes(x = weight, y = estimate, shape = factor(type), color = factor(type))) +
  geom_point(size = 2) +
  geom_hline(yintercept = 0) +
  scale_colour_brewer(palette = 'Set1') + 
  theme_bw() + 
  labs(x = "Weight", y = "Estimate") + 
  theme(legend.position = "bottom",
        legend.title = element_blank())

###############
##Rambachan and Roth (2019): Honest DID - sensitivity of results to violations of pretrends
devtools::install_github("asheshrambachan/HonestDiD", force = T)
library('HonestDiD')
covs <-paste(covariates_all, collapse = " + ")
# Estimate event study using lfe package
homicides.EventStudy=lfe::felm(acuerdo ~  lag_7 + lag_6 + lag_5 + lag_4 + lag_3 
                               + lag_2  + date_0 + lead_1 + lead_2 + lead_3 
                               + logdefuncionespc_mean_y_1 + logdefuncionespc_mean_y_2 + logdefuncionespc_mean_y_3 + logdefuncionespc_mean_y_4 + logdefuncionespc_mean_y_5 + logdefuncionespc_mean_y_6 + logdefuncionespc_mean_y_7 + logdefuncionespc_mean_y_8 + logdefuncionespc_mean_y_9 + align_gov_y_1 + align_gov_y_2 + align_gov_y_3 + align_gov_y_4 + align_gov_y_5 + align_gov_y_6 + align_gov_y_7 + align_gov_y_8 + align_gov_y_9 + margin_gov_y_1 + margin_gov_y_2 + margin_gov_y_3 + margin_gov_y_4 + margin_gov_y_5 + margin_gov_y_6 + margin_gov_y_7 + margin_gov_y_8 + margin_gov_y_9 + hayCarteles_y_1 + hayCarteles_y_2 + hayCarteles_y_3 + hayCarteles_y_4 + hayCarteles_y_5 + hayCarteles_y_6 + hayCarteles_y_7 + hayCarteles_y_8 + hayCarteles_y_9 + ap4_2_3_mean_y_1 + ap4_2_3_mean_y_2 + ap4_2_3_mean_y_3 + ap4_2_3_mean_y_4 + ap4_2_3_mean_y_5 + ap4_2_3_mean_y_6 + ap4_2_3_mean_y_7 + ap4_2_3_mean_y_8 + ap4_2_3_mean_y_9
                               | estado + year | 0 | 0, 
                               data = data.final2
)
summary(homicides.EventStudy)

#with weights
data.wleadlags.weights<- read_dta("../../Data/ConstructionDatabase/data_wleads&lags2_weights.dta")
#erase missing values
data_noNA_weights <- data.wleadlags.weights %>% 
  filter(!is.na(perc))


homicides.EventStudy.weights=lfe::felm(logdefuncionespc ~  lag_7 + lag_6 + lag_5 + lag_4 + lag_3 
                                       + lag_2  + date_0 + lead_1 + lead_2 + lead_3 
                                       + logdefuncionespc_mean_y_1 + logdefuncionespc_mean_y_2 + logdefuncionespc_mean_y_3 + logdefuncionespc_mean_y_4 + logdefuncionespc_mean_y_5 + logdefuncionespc_mean_y_6 + logdefuncionespc_mean_y_7 + logdefuncionespc_mean_y_8 + logdefuncionespc_mean_y_9 + align_gov_y_1 + align_gov_y_2 + align_gov_y_3 + align_gov_y_4 + align_gov_y_5 + align_gov_y_6 + align_gov_y_7 + align_gov_y_8 + align_gov_y_9 + margin_gov_y_1 + margin_gov_y_2 + margin_gov_y_3 + margin_gov_y_4 + margin_gov_y_5 + margin_gov_y_6 + margin_gov_y_7 + margin_gov_y_8 + margin_gov_y_9 + hayCarteles_y_1 + hayCarteles_y_2 + hayCarteles_y_3 + hayCarteles_y_4 + hayCarteles_y_5 + hayCarteles_y_6 + hayCarteles_y_7 + hayCarteles_y_8 + hayCarteles_y_9 + ap4_2_3_mean_y_1 + ap4_2_3_mean_y_2 + ap4_2_3_mean_y_3 + ap4_2_3_mean_y_4 + ap4_2_3_mean_y_5 + ap4_2_3_mean_y_6 + ap4_2_3_mean_y_7 + ap4_2_3_mean_y_8 + ap4_2_3_mean_y_9
                                       | estado + year | 0 | 0, 
                                       data = data_noNA_weights, weights = data_noNA_weights$perc
)

summary(homicides.EventStudy.weights)

#Extract coefficients of regression associated with event study coefficients
coefIndex<-c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)

betahat = homicides.EventStudy$beta[coefIndex, ]

# Extract estimated variance-covariance matrix of event study coefficients
#install.packages("nlme")
#library(nlme)
sigma =vcov(homicides.EventStudy)[coefIndex, coefIndex]

#Rescale by 100 so that results will be in units of percentage points
betahat = 100 * betahat
sigma = 100^2 * sigma

# Construct vector of event times and the scalar reference period
timeVec = c(seq(from = -7, to = -1, by = 1), seq(from = 0, to = 3, by = 1))
referencePeriod = -1
postPeriodIndices = which(timeVec > -1)
prePeriodIndices = which(timeVec < -1)
# Construct standard errors associated with event study coefficients
stdErrors = summary(homicides.EventStudy)$coefficients[coefIndex,2]

#stdErrors = 100 * stdErrors

# Create list containing objects produced by the event study
data_EventStudy = list(
  betahat = betahat,
  sigma = sigma,
  timeVec = timeVec,
  referencePeriod = referencePeriod,
  prePeriodIndices = prePeriodIndices,
  postPeriodIndices = postPeriodIndices,
  stdErrors = stdErrors
)

#Constructing a sensitivity plot
# Number of pre-periods
numPrePeriods = length(data_EventStudy$prePeriodIndices)
numPostPeriods = length(data_EventStudy$postPeriodIndices)

###Create l_vec corresponding with 3 years of exposure
#Reference is 0 years of exposure, so want effect 3 years later
l_vec = basisVector(3 - (-1), numPostPeriods)
# Construct robust confidence intervals for Delta^{SD}(M) for 15 years of exposure
DeltaSD_RobustResults = createSensitivityResults(betahat = data_EventStudy$betahat,
                                                 sigma = data_EventStudy$sigma,
                                                 numPrePeriods = numPrePeriods,
                                                 numPostPeriods = numPostPeriods,
                                                 l_vec = l_vec,
                                                 Mvec = seq(from = 0, to = 0.6, by = 0.1))
head(DeltaSD_RobustResults)



# Construct dataframe with OLS confidence interval for theta
OriginalResults = constructOriginalCS(betahat = data_EventStudy$betahat,
                                      sigma = data_EventStudy$sigma,
                                      numPrePeriods = numPrePeriods,
                                      numPostPeriods = numPostPeriods,
                                      l_vec = l_vec )
# Construct sensitivity plot
DeltaSD_SensitivityPlot = createSensitivityPlot(robustResults = DeltaSD_RobustResults,
                                                originalResults = OriginalResults)


DeltaSD_SensitivityPlot
dev.copy(png,'../Figures/pretrends_sensitivity.png')
dev.off()

#Benchmarking M
lowerBound_M = DeltaSD_lowerBound_Mpre(betahat = data_EventStudy$betahat,
                                       sigma = data_EventStudy$sigma,
                                       numPrePeriods = numPrePeriods)
upperBound_M = DeltaSD_upperBound_Mpre(betahat = data_EventStudy$betahat,
                                       sigma = data_EventStudy$sigma,
                                       numPrePeriods = numPrePeriods)

#Event study plot:
event.study.plot<-createEventStudyPlot(data_EventStudy$betahat, stdErrors = stdErrors, sigma = sigma,
                                       numPrePeriods, numPostPeriods, timeVec=timeVec,
                                       referencePeriod=referencePeriod, useRelativeEventTime = T)

event.study.plot

# Construct robust confidence intervals for Delta^{SD}(M): monotonically decreasing case
##install.packages("Rglpk")
library(Rglpk)
DeltaSDD_RobustResults.decreasing = createSensitivityResults(betahat = data_EventStudy$betahat,
                                                             sigma = data_EventStudy$sigma,
                                                             monotonicityDirection = "decreasing",
                                                             numPrePeriods = numPrePeriods,
                                                             numPostPeriods = numPostPeriods,
                                                             l_vec = l_vec,
                                                             Mvec = seq(from = 0, to = 0.6, by = 0.1))
# Construct sensitivity plot
DeltaSDD_SensitivityPlot = createSensitivityPlot(robustResults = DeltaSDD_RobustResults.decreasing,
                                                 originalResults = OriginalResults)
DeltaSDD_SensitivityPlot
dev.copy(png,'../Figures/pretrends_sensitivity_decreasing.png')
dev.off()

# Construct robust confidence intervals for Delta^{SD}(M): monotonically decreasing case
DeltaSDD_RobustResults.increasing = createSensitivityResults(betahat = data_EventStudy$betahat,
                                                             sigma = data_EventStudy$sigma,
                                                             monotonicityDirection = "increasing",
                                                             numPrePeriods = numPrePeriods,
                                                             numPostPeriods = numPostPeriods,
                                                             l_vec = l_vec,
                                                             Mvec = seq(from = 0, to = 0.6, by = 0.1))
# Construct sensitivity plot
DeltaSDD_SensitivityPlot = createSensitivityPlot(robustResults = DeltaSDD_RobustResults.increasing,
                                                 originalResults = OriginalResults)
DeltaSDD_SensitivityPlot
dev.copy(png,'../Figures/pretrends_sensitivity_increasing.png')
dev.off()

#####With increasing bias:


#####Better plot
data.final2 %>% do(fit = felm(model_formula_cov, data = ., 
                              exactDOF = TRUE, cmethod = "reghdfe")) %>% #omit lag_8 and lag_1, the first one because of collinearity and the second one to have a comparison group
  broom::tidy(fit, conf.int = TRUE) %>% 
  # keep just the variables we are going to plot
  filter(term %in% treatments_full) %>% 
  # reformat d data
  mutate(t = c(-8:-2, 0:3)) %>% 
  subset(select=c(t, estimate, conf.low, conf.high))  %>% 
  # select(df, t, estimate, conf.low, conf.high) %>% 
  # add in data for year -1
  bind_rows(tibble(t = -1, estimate = 0, 
                   conf.low = 0, conf.high = 0
  )) %>% 
  # split the error bands by pre-post
  mutate(band_groups = case_when(
    t < -1 ~ "Pre",
    t >= 0 ~ "Post",
    t == -1 ~ ""
  )) %>% 
  # plot
  ggplot(aes(x = t, y = estimate)) + 
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high, group = band_groups), 
              color = "lightgrey", alpha = 1/2) + 
  geom_point(color = "red") + 
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high)) + 
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = -0.5, linetype = "dashed") + 
  scale_x_continuous(breaks = -7:3) + 
  theme(axis.title.x = element_blank()) + theme_bw()
dev.copy(png,'../Figures/event_study.png')
dev.off()

###############
##ERRORS TO CHECK



