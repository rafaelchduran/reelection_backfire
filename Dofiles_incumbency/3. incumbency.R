#Reelection backfire

rm(list=ls())
### Packages
#install.packages("scales")
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

#working directory:
setwd("~/Dropbox/Dissertation/GovernmentStrategies/reelection_backfire/Dofiles/")

#check this:
# https://stackoverflow.com/questions/62881774/formula-with-interaction-terms-in-event-study-designs-using-r

#Load dataset:
data.incumbency<- read_dta("../../Data/ConstructionDatabase/data_final_incumbency.dta")
collapsed.data <- read_dta("../../Data/ConstructionDatabase/collapseddata_incumbency_forR.dta")

# make things pretty
theme_set(theme_bw())

#SEE STAGGERED TREATMENT
#a)  Probability of winning
panelView(incumbent_yesterday_w_tomorrow2 ~ TREAT, 
          data = as.data.frame(collapsed.data %>% mutate(TREAT = ifelse(reform > 0, 1, 0))),
          index = c("state", "year"), type="outcome", pre.post = T, 
          theme.bw = 1, ylab = "Probability of winning at t+1", 
          main = "Evolution of Probability of Winning in Next Election, 2010-2018")
dev.copy(png,'../Figures_incumbency/incumbency_probability_winning.png')
dev.off()

#############################################
# Event-study estimates:

# isolate the variables that we want to plot
treatments_short <- c("lag_5", "lag_4", "date_0")
treatments_full <- c("lag_8", "lag_6","lag_5", "lag_4", "date_0")
margin <-c("margin_lag_1", "margin_lead_1", "margin_date0", "margin_lag_2", "margin_lead_2", "margin_lag_3", "margin_lead_3", "margin_lag_4", "margin_lead_4", "margin_lag_5", "margin_lag_6", "margin_lag_7", "margin_lag_8")
ncand <-c("ncand_lag_1", "ncand_lead_1", "ncand_date0", "ncand_lag_2", "ncand_lead_2", "ncand_lag_3", "ncand_lead_3", "ncand_lag_4", "ncand_lead_4", "ncand_lag_5", "ncand_lag_6", "ncand_lag_7")
HHI <-c("HHI_lag_1", "HHI_lead_1", "HHI_date0", "HHI_lag_2", "HHI_lead_2", "HHI_lag_3", "HHI_lead_3", "HHI_lag_4", "HHI_lead_4", "HHI_lag_5", "HHI_lag_6", "HHI_lag_7", "HHI_lag_8")
laakso<-c("effectiveparties_lag_1", "effectiveparties_lead_1", "effectiveparties_date0", "effectiveparties_lag_2", "effectiveparties_lead_2", "effectiveparties_lag_3", "effectiveparties_lead_3", "effectiveparties_lag_4", "effectiveparties_lead_4", "effectiveparties_lag_5", "effectiveparties_lag_6", "effectiveparties_lag_7", "effectiveparties_lag_8")
molinar<-c("NP_lag_1", "NP_lead_1", "NP_date0", "NP_lag_2", "NP_lead_2", "NP_lag_3", "NP_lead_3", "NP_lag_4", "NP_lead_4", "NP_lag_5", "NP_lag_6", "NP_lag_7", "NP_lag_8")
golosov<-c("golosov_lag_1", "golosov_lead_1", "golosov_date0", "golosov_lag_2", "golosov_lead_2", "golosov_lag_3", "golosov_lead_3", "golosov_lag_4", "golosov_lead_4", "golosov_lag_5", "golosov_lag_6", "golosov_lag_7", "golosov_lag_8")
coal<-c("dcoal_lag_1", "dcoal_lead_1", "dcoal_date0", "dcoal_lag_2", "dcoal_lead_2", "dcoal_lag_3", "dcoal_lead_3", "dcoal_lag_4", "dcoal_lead_4", "dcoal_lag_5", "dcoal_lag_6", "dcoal_lag_7", "dcoal_lag_8")
alignment<-c("governor_alignment_lag_8", "governor_alignment_lag_7", "governor_alignment_lag_6", "governor_alignment_lag_5", "governor_alignment_lag_4", "governor_alignment_lag_3", "governor_alignment_lag_2")
margin<-c("margin_lag_8", "margin_lag_7", "margin_lag_6", "margin_lag_5", "margin_lag_4", "margin_lag_3", "margin_lead_2")
covariates_all <-do.call(c, list(margin, alignment))
covariates_all_noleadslags<-c("winning_margin_governor", "governor_alignment")
covariates_all_wpop<-c("winning_margin_governor", "governor_alignment", "pop")

# put in the model formula

model_formula <- as.formula(paste("incumbent_yesterday_w_tomorrow2 ~", 
        paste(treatments_full, collapse = " + "),
        "| estado + year | 0 | 0 ")
)



treatments.paste<-paste(treatments_full, collapse = " + ")

model_formula_cov <- as.formula(paste("incumbent_yesterday_w_tomorrow2 ~ lag_8 + lag_7 + lag_6 + lag_5 + lag_4 + lag_3 + lag_2 + date_0 + lead_1 + lead_2 + lead_3 +", 
                                       paste(covariates_all, collapse = " + "), 
                                       "| estado + year | 0 | 0 ")
)


# run model by dependent variable
#1) probability of winning in next election
data.incumbency %>% do(fit = felm(model_formula, data = ., 
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
dev.copy(png,'../Figures_incumbency/event_study.png')
dev.off()


# run model by dependent variable, WITH COVARIATES
 data.wleadlags %>% do(fit = felm(model_formula_cov, data = ., 
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
dev.copy(png,'../Figures_incumbency/event_study_wcovariates.png')
dev.off()



#2) ihs(homicides per capita)
data.wleadlags %>% do(fit = felm(model_formula_ihs, data = ., 
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
dev.copy(png,'../Figures_incumbency/event_study_ihs.png')
dev.off()


# run model by dependent variable, WITH COVARIATES
data.wleadlags %>% do(fit = felm(model_formula_ihs_cov, data = ., 
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
dev.copy(png,'../Figures_incumbency/event_study_ihs_wcovariates.png')
dev.off()

# 3) INCUMBENCY, WITH COVARIATES
alignment2<-c("governor_alignment_lag_6", "governor_alignment_lag_5", "governor_alignment_lag_4", "governor_alignment_lag_3")
margin2<-c("margin_lag_6", "margin_lag_5", "margin_lag_4", "margin_lag_3")
covariates_all2 <-do.call(c, list(margin2, alignment2))

model_formula_incumbency <- as.formula(incumbent_yesterday_w_tomorrow2 ~ lag_6 + lag_5 + lag_4 + lag_3 + lag_2 + lag_1
                                  + date_0 + lead_1 + lead_2  + lead_3 | estado + year | 0 | 0 )

model_formula_incumbency_cov <- as.formula(paste("incumbent_yesterday_w_tomorrow2 ~ lag_6 + lag_5 + lag_4 + lag_3 + date_0 + lead_1 + lead_2 +", 
                                             paste(covariates_all2, collapse = " + "), 
                                             "| estado + year | 0 | 0 ")
)



data.wleadlags %>% do(fit = felm(model_formula_incumbency, data = ., 
                                 exactDOF = TRUE, cmethod = "reghdfe")) %>% #omit lag_8 and lag_1, the first one because of collinearity and the second one to have a comparison group
  broom::tidy(fit, conf.int = TRUE) %>% 
  # keep just the variables we are going to plot
  filter(term %in% treatments_full) %>% 
  # reformat d data
  mutate(t = c(-6:-3, 0:2)) %>% 
  subset(select=c(t, estimate, conf.low, conf.high))  %>% 
  # select(df, t, estimate, conf.low, conf.high) %>% 
  # add in data for year -1
  bind_rows(tibble(t = c(-2, -1), estimate = c(0,0), 
                   conf.low = c(0,0), conf.high = c(0,0)
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
  scale_x_continuous(breaks = -6:2) + 
  theme(axis.title.x = element_blank()) + theme_bw()
dev.copy(png,'../Figures_incumbency/event_study_incumbency.png')
dev.off()

#create table
#install.packages("stargazer")
library(stargazer)

loghomicides<- felm(model_formula, data = data.wleadlags, 
                    exactDOF = TRUE, cmethod = "reghdfe")
loghomicides_wcov<- felm(model_formula_cov, data = data.wleadlags, 
                         exactDOF = TRUE, cmethod = "reghdfe")
ihshomicides<- felm(model_formula_ihs, data = data.wleadlags, 
                    exactDOF = TRUE, cmethod = "reghdfe")
ihshomicides_wcov<- felm(model_formula_ihs_cov, data = data.wleadlags, 
                         exactDOF = TRUE, cmethod = "reghdfe")
loghomicidesold<- felm(model_formula_old, data = data.wleadlags, 
                    exactDOF = TRUE, cmethod = "reghdfe")
loghomicidesold_wcov<- felm(model_formula_old_cov, data = data.wleadlags, 
                         exactDOF = TRUE, cmethod = "reghdfe")

#summary(loghomicides)$coefficients


#event.study<-stargazer(loghomicides, loghomicides_wcov, ihshomicides, ihshomicides_wcov, loghomicidesold, loghomicidesold_wcov,
event.study<-stargazer(loghomicides, loghomicides_wcov, ihshomicides, ihshomicides_wcov,
          title=" ", align=TRUE, type = "text",
          keep=c("lag_7", "lag_6","lag_5", "lag_4", "lag_3", "lag_2", "date_0", "lead_1", "lead_2", "lead_3"),
          keep.stat=c("n","rsq"),
          covariate.lables=c("Lag 7","Lag 6", "Lag 5", "Lag 4", "Lag 3", "Lag 2", "Date 0", "Lead 1", "Lead 2", "Lead 3"),
          out = '../Tables_incumbency/event_study_table.tex'
)

### Table with different transformations: 
# put in the model formula
#without controls:
model_formula_homicides <- as.formula(paste("defunciones ~", 
                                  paste(treatments_full, collapse = " + "),
                                  "| estado + year | 0 | 0 ")
)
model_formula_homicidespc <- as.formula(paste("defuncionespc ~", 
                                              paste(treatments_full, collapse = " + "),
                                              "| estado + year | 0 | 0 ")
)
#with controls
model_formula_homicides_cov <- as.formula(paste("defunciones ~ lag_8 + lag_7 + lag_6 + lag_5 + lag_4 + lag_3 + lag_2 + date_0 + lead_1 + lead_2 + lead_3 +", 
                                      paste(covariates_all_wpop, collapse = " + "), 
                                      "| estado + year | 0 | 0 ")
)
model_formula_homicidespc_cov <- as.formula(paste("defuncionespc ~ lag_8 + lag_7 + lag_6 + lag_5 + lag_4 + lag_3 + lag_2 + date_0 + lead_1 + lead_2 + lead_3 +", 
                                                paste(covariates_all, collapse = " + "), 
                                                "| estado + year | 0 | 0 ")
)


#Estimates:
homicides<- felm(model_formula_homicides, data = data.wleadlags, 
                    exactDOF = TRUE, cmethod = "reghdfe")
homicidespc<- felm(model_formula_homicidespc, data = data.wleadlags, 
                         exactDOF = TRUE, cmethod = "reghdfe")
homicides_wcov<- felm(model_formula_homicides_cov, data = data.wleadlags, 
                 exactDOF = TRUE, cmethod = "reghdfe")
homicidespc_wcov<- felm(model_formula_homicidespc_cov, data = data.wleadlags, 
                   exactDOF = TRUE, cmethod = "reghdfe")

event.study<-stargazer(homicides, homicides_wcov, homicidespc, homicidespc_wcov,  loghomicides, loghomicides_wcov,
                       title=" ", align=TRUE, 
                       keep=c("lag_7", "lag_6","lag_5", "lag_4", "lag_3", "lag_2", "date_0", "lead_1", "lead_2", "lead_3"),
                       keep.stat=c("n","rsq"),
                       #covariate.lables=c("Lag 7","Lag 6", "Lag 5", "Lag 4", "Lag 3", "Lag 2", "Date 0", "Lead 1", "Lead 2", "Lead 3"),
                       out = '../Tables_incumbency/event_study_transformations_table.tex'
)

#############################################
#Abraham and Sun (2020): TO ADDRESS LEADS AND LAGS DEPENDENCE, THAT MAY CREATE A SPURIOUS PARALLEL TREND

# first get percentage contribution to each lead/lag indicator by treatment cohort for weights
# calculate weights
weights <- data.wleadlags %>% 
  # reformat leadlags to long
  pivot_longer(cols = all_of(treatments_full), 
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
##data.wleadlags2 <- data.wleadlags %>%   filter(!is.na(logdefuncionespc))

AS_data <- data.wleadlags2 %>% 
  left_join(., merge_weights, 
            by = c("adopt_year", "whichlead")) %>% 
  # make indicator variables
  mutate(val = 1) %>% 
  pivot_wider(names_from = indic, values_from = val, values_fill = list(val = 0))

# get name of lead/lags in one vector
leadlags <- unique(merge_weights$indic)

# get covariates and make felm formula
covariates_saturated <- paste(leadlags, collapse = "+")

# make fomula to run within our FE specification: this is AS (2019) full saturated model
formula_saturated <- as.formula(paste("logdefuncionespc ~ ", covariates_saturated,  
                                      "| inegi + year | 0 | 0"))

# formula without covariates: 
formula_saturated <- as.formula(logdefuncionespc ~ 
                                  date_0__2015 + lag_2__2015 + lag_3__2015 + 
                                  lag_4__2015 + lag_5__2015 + lead_1__2015 + lead_2__2015 + 
                                  lead_3__2015 + date_0__2016 + lag_2__2016 + lag_3__2016 + 
                                  lag_4__2016 + lag_5__2016 + lag_6__2016 + lead_1__2016 + 
                                  lead_2__2016  + date_0__2017 + lag_2__2017 + 
                                  lag_3__2017 + lag_4__2017 + lag_5__2017 + lag_6__2017 + lag_7__2017 + 
                                  lead_1__2017  + date_0__2018 + lag_2__2018 + 
                                  lag_3__2018 + lag_4__2018 + lag_5__2018 + lag_6__2018 + lag_7__2018 + 
                                  lag_8__2018  
                                | inegi + year | 0 | 0)
#remove: lead_3__2016, lead_2__2017, lead_1__2018

# formula with covariates: 
controls_saturated <- paste(covariates_all, collapse = "+")

covariates_all
formula_saturated_cov <- as.formula(paste("logdefuncionespc ~ 
                                  date_0__2015 + lag_2__2015 + lag_3__2015 + 
                                          lag_4__2015 + lag_5__2015 + lead_1__2015 + lead_2__2015 + 
                                          lead_3__2015 + date_0__2016 + lag_2__2016 + lag_3__2016 + 
                                          lag_4__2016 + lag_5__2016 + lag_6__2016 + lead_1__2016 + 
                                          lead_2__2016  + date_0__2017 + lag_2__2017 + 
                                          lag_3__2017 + lag_4__2017 + lag_5__2017 + lag_6__2017 + lag_7__2017 + 
                                          lead_1__2017  + date_0__2018 + lag_2__2018 + 
                                          lag_3__2018 + lag_4__2018 + lag_5__2018 + lag_6__2018 + lag_7__2018 + 
                                          lag_8__2018+", controls_saturated,  
                                      "| inegi + year | 0 | 0"))


# isolate the variables that we want to plot; note that lag_1 is the comparison group
covariates_interest <- treatments_full

# make function that will weight the different lead lag estimators 
weight_leadlags <- function(formula,data) {
  
  # fit the model
  fit = felm(formula = formula, data = data, exactDOF = TRUE, cmethod = "reghdfe")
  
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
      conf.int = TRUE
    ) %>% mutate(whichlead = "ll")
  }
  # run over all lead/lags
  newdata<-map_df(covariates_interest, get_lincom) %>% 
    # add time variable
    mutate(t = c(-8:-2, 0:3))
}

# run model by dependent variable
AS_data  %>% 
  # fit the model
  do(fit = weight_leadlags(formula_saturated,.)) %>% 
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
  scale_x_continuous(breaks = -8:3) + 
  theme(axis.title.x = element_blank()) + theme_bw()
dev.copy(png,'../Figures_incumbency/CATTs.png')
dev.off()


# ABRAHAM AND SUN (2020): INCUMBENCY ADVANTAGE
# first get percentage contribution to each lead/lag indicator by treatment cohort for weights
# calculate weights
treatments.incumbency <- c("lag_5", "lag_4", "lag_3", "lag_2" "date_0")

data.incumbency_noNA <- data.incumbency %>% 
  filter(!is.na(incumbent_yesterday_w_tomorrow2)) 

data.incumbency_noNA <- data.incumbency_noNA %>% 
  filter(!is.na(whichlead)) 

weights.incumbency <- data.incumbency_noNA %>% 
  # reformat leadlags to long
  pivot_longer(cols = all_of(treatments.incumbency), 
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
##data.wleadlags2 <- data.wleadlags %>%   filter(!is.na(logdefuncionespc))

AS_data <- data.wleadlags2 %>% 
  left_join(., merge_weights, 
            by = c("adopt_year", "whichlead")) %>% 
  # make indicator variables
  mutate(val = 1) %>% 
  pivot_wider(names_from = indic, values_from = val, values_fill = list(val = 0))


covariates_interest_incumbency <- c("lag_6","lag_5", "lag_4", "lag_3", "date_0", "lead_1", "lead_2")
formula_saturated_incumbency <- as.formula(incumbent_yesterday_w_tomorrow ~ 
                                  date_0__2015 + lag_2__2015 + lag_3__2015 + 
                                  lag_4__2015 + lag_5__2015 + lead_1__2015 + lead_2__2015 + 
                                  lead_3__2015 + date_0__2016 + lag_2__2016 + lag_3__2016 + 
                                  lag_4__2016 + lag_5__2016 + lag_6__2016 + lead_1__2016 + 
                                  lead_2__2016  + date_0__2017 + lag_2__2017 + 
                                  lag_3__2017 + lag_4__2017 + lag_5__2017 + lag_6__2017 + lag_7__2017 + 
                                  lead_1__2017  + date_0__2018 + lag_2__2018 + 
                                  lag_3__2018 + lag_4__2018 + lag_5__2018 + lag_6__2018 + lag_7__2018 + 
                                  lag_8__2018  
                                | inegi + year | 0 | 0)

weight_leadlags_incumbency <- function(formula,data) {
  
  # fit the model
  fit = felm(formula = formula, data = data, exactDOF = TRUE, cmethod = "reghdfe")
  
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
    filter(whichlead %in% covariates_interest_incumbency) %>% 
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
      conf.int = TRUE
    ) %>% mutate(whichlead = "ll")
  }
  # run over all lead/lags
  newdata<-map_df(covariates_interest_incumbency, get_lincom) %>% 
    # add time variable
    mutate(t = c(-6:-3, 0:2))
}

AS_data  %>% 
  # fit the model
  do(fit = weight_leadlags_incumbency(formula_saturated_incumbency,.)) %>% 
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
  scale_x_continuous(breaks = -8:3) + 
  theme(axis.title.x = element_blank()) + theme_bw()
dev.copy(png,'../Figures_incumbency/CATTs.png')
dev.off()


#############################################
#Cengiz, Dube, Lindner, and Zipperer (2019): I could use this to state that there is no difference in timing.
# get all states treated during our data sample which have at least 2 pre and 2 post observations
obs <- data.wleadlags %>% 
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
formula_cldz <- as.formula("logdefuncionespc ~ reform | inegi + year | 0 | 0")
formula_cldz_cov <- as.formula(paste("logdefuncionespc ~ reform +",
                                 paste(covariates_all, collapse = " + "),
                                 "| inegi + year | 0 | 0"))

formula_cldz_ihs <- as.formula("ihs_defuncionespc ~ reform | inegi + year | 0 | 0")
formula_cldz_ihs_cov <- as.formula(paste("ihs_defuncionespc ~ reform +",
                                     paste(covariates_all, collapse = " + "),
                                     "| inegi + year | 0 | 0"))

# formula to calculate state-event-specific effects
rundid <- function(st) {
  
  # get the treatment year
  treat_yr <- data.wleadlags %>% filter(estado == st) %>% slice(1) %>% pull(adopt_year)
  
  # get a dataset with the the treated state and clean control states, keep only the years -3 to 3
  did_data <- data.wleadlags %>% 
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
  treat_yr <- data.wleadlags %>% filter(estado == st) %>% slice(1) %>% pull(adopt_year)
  
  # get a dataset with the the treated state and clean control states, keep only the years -3 to 3
  did_data <- data.wleadlags %>% 
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
  treat_yr <- data.wleadlags %>% filter(estado == st) %>% slice(1) %>% pull(adopt_year)
  
  # get a dataset with the the treated state and clean control states, keep only the years -3 to 3
  did_data <- data.wleadlags %>% 
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
  treat_yr <- data.wleadlags %>% filter(estado == st) %>% slice(1) %>% pull(adopt_year)
  
  # get a dataset with the the treated state and clean control states, keep only the years -3 to 3
  did_data <- data.wleadlags %>% 
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

# plot
plotdata %>% 
  mutate(rank = rank(estimate),
         cohort_type = ifelse(cohort <= 2015, "2015-2016", "2017-2018")) %>% 
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
dev.copy(png,'../Figures_incumbency/CDLZ.png')
dev.off()

# plot
plotdata_cov %>% 
  mutate(rank = rank(estimate),
         cohort_type = ifelse(cohort <= 2015, "2015-2016", "2017-2018")) %>% 
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
dev.copy(png,'../Figures_incumbency/CDLZ_cov.png')
dev.off()


# plot
plotdata_ihs %>% 
  mutate(rank = rank(estimate),
         cohort_type = ifelse(cohort <= 2015, "2015-2016", "2017-2018")) %>% 
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
dev.copy(png,'../Figures_incumbency/CDLZ_ihs.png')
dev.off()

# plot
plotdata_ihs_cov %>% 
  mutate(rank = rank(estimate),
         cohort_type = ifelse(cohort <= 2015, "2015-2016", "2017-2018")) %>% 
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
dev.copy(png,'../Figures_incumbency/CDLZ_ihs_cov.png')
dev.off()

###############
#Stacked dataset analysis from CLDZ

# get the cohort years
obs <- data.wleadlags %>% 
  filter(!is.na(adopt_year)) %>% 
  pull(adopt_year) %>% 
  unique()

# make fomula to run within our FE specification
# get the lead lags in one set of covariates
leadlags <- c("lag_8", "lag_7", "lag_6", "lag_5", "lag_4", "lag_3", "lag_2", "date_0",
              "lead_1", "lead_2", "lead_3")

# Make the estimating equation
controls <- paste(margin, collapse = "+")

formula_cldz <- as.formula(paste("logdefuncionespc ~ ",
                                  paste(leadlags, collapse= "+"),
                                  "| factor(inegi):factor(df) + factor(year):factor(df) | 0 | 0"))


leadlags_all <- paste(leadlags, collapse = "+")

formula_cldz2 <- as.formula(paste("logdefuncionespc ~ lag_8+lag_7+lag_6+lag_5+lag_4+lag_3+lag_2+date_0+lead_1+lead_2+lead_3+",
                                  paste(covariates_all, collapse= "+"),
                                  "| factor(inegi):factor(df) + factor(year):factor(df) | 0 | 0"))

formula_cldz2 <- as.formula(paste("logdefuncionespc ~ lag_8+lag_7+lag_6+lag_5+lag_4+lag_3+lag_2+date_0+lead_1+lead_2+lead_3+",
                                  paste(covariates_all, collapse= "+"),
                                  "| inegi + year | 0 | 0"))


# make formula to create the dataset
getdata <- function(i) {
  
  #keep what we need
  data.wleadlags %>% 
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

# estimate the model on our stacked data
stacked_data %>% 
  # fit the model
  do(fit = felm(formula_cldz, data = ., exactDOF = TRUE, cmethod = "reghdfe")) %>% 
  broom::tidy(fit, conf.int = TRUE) %>% 
  # keep just the variables we are going to plot
  filter(term %in% covariates_interest) %>% 
  # make a relative time variable
  mutate(t = c(-8:-2, 0:3)) %>% 
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
  scale_x_continuous(breaks = -8:3) + 
  theme_bw()
dev.copy(png,'../Figures_incumbency/stacked_dataset.png')
dev.off()


# with controls
stacked_data %>% 
  # fit the model
  do(fit = felm(formula_cldz2, data = ., exactDOF = TRUE, cmethod = "reghdfe")) %>% 
  broom::tidy(fit, conf.int = TRUE) %>% 
  # keep just the variables we are going to plot
  filter(term %in% covariates_interest) %>% 
  # make a relative time variable
  mutate(t = c(-8:-2, 0:3)) %>% 
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
  scale_x_continuous(breaks = -8:3) + 
  theme_bw()
dev.copy(png,'../Figures_incumbency/stacked_dataset_wcontrols.png')
dev.off()


###############
#Callaway and Sant'Ana (2019)
#https://bcallaway11.github.io/did/articles/did-basics.html 
#install.packages("did")
#devtools::install_github("bcallaway11/did")
library(did)

# need to replace the adopt year to 0 if missing
data_CS <- data.incumbency %>% 
  mutate(adopt_year = ifelse(is.na(adopt_year), 0, adopt_year))

data_CS <- data_CS %>% 
  filter(!is.na(incumbent_yesterday_w_tomorrow2))

# run the CS algorithm
CS_out <- att_gt("incumbent_yesterday_w_tomorrow2", idname="inegi", tname="year",
                 data = data_CS,
                 first.treat.name="adopt_year", panel = T, control.group = "nottreated",
                 alp = 0.05, cband=T, bstrap=T, biters = 1000,
                 printdetails = F)

CS_out.cov <- att_gt("incumbent_yesterday_w_tomorrow2", idname="inegi", tname="year",
                 xformla = ~ winning_margin_governor + governor_alignment,
                 data = data_CS,
                 first.treat.name="adopt_year", panel = T, control.group = "nottreated",
                 alp = 0.05, cband=T, bstrap=T, biters = 1000,
                 printdetails = F)


summary(CS_out)

# plot the results
ggdid(CS_out)

#aggregation
agg.simple <- aggte(CS_out)
summary(agg.simple)

agg.simple <- aggte(CS_out.ihs)
summary(agg.simple)

#Dynamic Effects and Event Studies
agg.es <- aggte(CS_out, type="dynamic")
summary(agg.es)
ggdid(agg.es)

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


#My type of plot:
estimate=-0.0032727
se=0.04629
tibble(
  t = -8:3,
  #estimate = CS_out$aggte$dynamic.att.e,
  #se = CS_out$aggte$dynamic.se.e,
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
  scale_x_continuous(breaks = -8:3) + 
  theme(axis.title.x = element_blank()) + 
  theme_bw()

###############
#NON-DID METHODS
###############
#1) GENERALIZED SYNTHETIC CONTROLS
#http://yiqingxu.org/software/gsynth/gsynth_examples.html
#install.packages('gsynth', type = 'source')
library(gsynth)

#erase missing values
data_noNA <- data.wleadlags %>% 
  filter(!is.na(logdefuncionespc)) 

data_noNAdet <- data.wleadlags %>% 
  filter(!is.na(ihs_defuncionespc))

data_noNAinc <- data.wleadlags %>% 
  filter(!is.na(incumbent_yesterday_w_tomorrow2)) 


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
out <- gsynth(logdefuncionespc ~ reform, data = data_noNA, index = c("inegi", "year"),
              force = "two-way", CV = TRUE, se = TRUE, nboots = 1000, cores = 8, r = c(0, 3), 
              min.T0 = 5)

covariates_list<-paste(covariates_all_noleadslags, collapse= "+")


out_cov <- gsynth(logdefuncionespc ~ reform, X= winning_margin_governor + governor_alignment, 
                  data = data_noNA, index = c("inegi", "year"),
                  force = "two-way", CV = TRUE, se = TRUE, nboots = 1000, cores = 8, r = c(0, 3), 
                  inference = "nonparametric",
                  min.T0 = 5)

##2) Using ihs transformation
out.ihs <- gsynth(ihs_defuncionespc ~ reform, data = data_noNA, index = c("inegi", "year"),
              force = "two-way", CV = TRUE, se = TRUE, nboots = 1000, cores = 8, r = c(0, 3), 
              min.T0 = 5)

out.ihs_cov <- gsynth(ihs_defuncionespc ~ reform, X= winning_margin_governor + governor_alignment, 
                  data = data_noNA, index = c("inegi", "year"),
                  force = "two-way", CV = TRUE, se = TRUE, nboots = 1000, cores = 8, r = c(0, 3), 
                  min.T0 = 5)

##Mechanism: detained
out.detenidos <- gsynth(logdetenidospc ~ reform, data = data_noNAdet, index = c("inegi", "year"),
                  force = "two-way", CV = TRUE, se = TRUE, nboots = 1000, cores = 8, r = c(0, 3), 
                  min.T0 = 5)


summary(out)

#plot
##1) log homicides per capita: 
##without covariates:
out$est.att %>% 
  as_tibble(rownames = "t") %>% 
  mutate(t = as.numeric(t)) %>% 
  ggplot(aes(x = t, y = ATT)) + 
  geom_ribbon(aes(ymin = CI.lower, ymax = CI.upper), color = "lightgrey", alpha = 1/2) + 
  geom_line(color = "red") + 
  geom_point(color = "red") + 
  geom_hline(yintercept = 0) + 
  geom_vline(xintercept = -0.5, linetype = "dashed") + 
  theme(axis.title.x = element_text("Time relative to Treatment (2015)")) + 
  theme_bw() 
dev.copy(png,'../Figures_incumbency/gsynth.png')
dev.off()

##with covariates:
out_cov$est.att %>% 
  as_tibble(rownames = "t") %>% 
  mutate(t = as.numeric(t)) %>% 
  ggplot(aes(x = t, y = ATT)) + 
  geom_ribbon(aes(ymin = CI.lower, ymax = CI.upper), color = "lightgrey", alpha = 1/2) + 
  geom_line(color = "red") + 
  geom_point(color = "red") + 
  geom_hline(yintercept = 0) + 
  geom_vline(xintercept = -0.5, linetype = "dashed") + 
  theme(axis.title.x = element_text("Time relative to Treatment (2015)")) + 
  theme_bw() 
dev.copy(png,'../Figures_incumbency/gsynth_wcov.png')
dev.off()

##2) ihs homicides per capita: 
##without covariates:
out.ihs$est.att %>% 
  as_tibble(rownames = "t") %>% 
  mutate(t = as.numeric(t)) %>% 
  ggplot(aes(x = t, y = ATT)) + 
  geom_ribbon(aes(ymin = CI.lower, ymax = CI.upper), color = "lightgrey", alpha = 1/2) + 
  geom_line(color = "red") + 
  geom_point(color = "red") + 
  geom_hline(yintercept = 0) + 
  geom_vline(xintercept = -0.5, linetype = "dashed") + 
  theme(axis.title.x = element_text("Time relative to Treatment (2015)")) + 
  theme_bw() 
dev.copy(png,'../Figures_incumbency/gsynth_ihs.png')
dev.off()

##with covariates:
out.ihs_cov$est.att %>% 
  as_tibble(rownames = "t") %>% 
  mutate(t = as.numeric(t)) %>% 
  ggplot(aes(x = t, y = ATT)) + 
  geom_ribbon(aes(ymin = CI.lower, ymax = CI.upper), color = "lightgrey", alpha = 1/2) + 
  geom_line(color = "red") + 
  geom_point(color = "red") + 
  geom_hline(yintercept = 0) + 
  geom_vline(xintercept = -0.5, linetype = "dashed") + 
  theme(axis.title.x = element_text("Time relative to Treatment (2015)")) + 
  theme_bw() 
dev.copy(png,'../Figures_incumbency/gsynth_ihs_wcov.png')
dev.off()


#counterfactual plot
plot(out, type = "ct", raw = "none", main = "", ylab = "log(homicides per capita)",
     xlab="Time relative to Treatment (2015)", shade.post = T)
dev.copy(png,'../Figures_incumbency/gsynth_counterfactual.png')
dev.off()

plot(out_cov, type = "ct", raw = "none", main = "", ylab = "log(homicides per capita)",
     xlab="Time relative to Treatment (2015)", shade.post = T)
dev.copy(png,'../Figures_incumbency/gsynth_wcov_counterfactual.png')
dev.off()


plot(out.ihs, type = "ct", raw = "none", main = "", ylab = "log(homicides per capita)",
     xlab="Time relative to Treatment (2015)", shade.post = T)
dev.copy(png,'../Figures_incumbency/gsynth_counterfactual_ihs.png')
dev.off()

plot(out.ihs_cov, type = "ct", raw = "none", main = "", ylab = "log(homicides per capita)",
     xlab="Time relative to Treatment (2015)", shade.post = T)
dev.copy(png,'../Figures_incumbency/gsynth_wcov_counterfactual_ihs.png')
dev.off()




###############
#2) Matrix completion

#Remove the following units:
dropthis<-c(1003, 1004, 1009, 1011, 3001, 3009, 4001, 4005, 4006, 4007, 5007, 
            5013, 5015, 5023, 5026, 6003, 6006, 6008, 7001, 7004, 7007, 7013, 
            7014, 7015, 7016, 7018, 7021, 7022, 7023, 7024, 7025, 7026, 7028, 
            7029, 7030, 7033, 7034, 7035, 7038, 7039, 7041, 7042, 7043, 7044, 
            7045, 7046, 7047, 7048, 7050, 7051, 7052, 7055, 7056, 7060, 7064, 
            7067, 7071, 7073, 7075, 7076, 7077, 7079, 7082, 7083, 7084, 7085, 
            7090, 7091, 7092, 7093, 7094, 7096, 7100, 7103, 7104, 7105, 7106, 
            7109, 7111, 7112, 8015, 8022, 8023, 8025, 8033, 8042, 8049, 8056, 
            8057, 10002, 10006, 10016, 10020, 10024, 10027, 10031, 10033, 11001, 
            11016, 11018, 11022, 11025, 11029, 11036, 11038, 11040, 11043, 11045, 
            12008, 12009, 12024, 12037, 12045, 12047, 12063, 12065, 12070, 12081, 
            14004, 14010, 14011, 14014, 14017, 14019, 14020, 14025, 14026, 14028, 
            14032, 14034, 14036, 14038, 14040, 14041, 14042, 14048, 14051, 14052, 
            14054, 14057, 14058, 14059, 14060, 14061, 14062, 14064, 14068, 14071, 
            14072, 14074, 14075, 14077, 14080, 14089, 14090, 14091, 14092, 14095, 
            14099, 14103, 14104, 14106, 14107, 14110, 14111, 14112, 14113, 14114, 
            14115, 14116, 14117, 14118, 14123, 14125, 15004, 15006, 15012, 15017, 
            15022, 15027, 15034, 15036, 15041, 15047, 15048, 15049, 15055, 15064, 
            15066, 15069, 15072, 15077, 15078, 15083, 15089, 15094, 15102, 15107, 
            15116, 15123, 16004, 16005, 16013, 16017, 16018, 16021, 16022, 16024, 
            16026, 16027, 16028, 16031, 16032, 16036, 16037, 16039, 16040, 16042,
            16043, 16044, 16046, 16047, 16049, 16052, 16054, 16056, 16058, 16059, 
            16060, 16061, 16063, 16067, 16070, 16072, 16074, 16075, 16077, 16078, 
            16080, 16081, 16084, 16090, 16091, 16093, 16094, 16095, 16096, 16101, 
            16109, 16113, 17003, 17013, 17014, 17022, 17023, 17027, 19001, 19002, 
            19003, 19007, 19008, 19014, 19020, 19022, 19023, 19032, 19035, 19036, 
            19037, 19040, 19042, 19045, 19047, 19051, 20001, 20004, 20009, 20010, 
            20011, 20015, 20016, 20019, 20026, 20030, 20031, 20032, 20036, 20042, 
            20045, 20052, 20053, 20055, 20060, 20061, 20065, 20066, 20071, 20072, 
            20077, 20080, 20081, 20082, 20083, 20087, 20091, 20092, 20094, 20095, 
            20098, 20099, 20102, 20106, 20110, 20112, 20114, 20116, 20117, 20123, 
            20124, 20131, 20133, 20135, 20136, 20137, 20139, 20140, 20141, 20142, 
            20145, 20146, 20150, 20154, 20158, 20163, 20164, 20166, 20167, 20168, 
            20169, 20170, 20172, 20174, 20176, 20178, 20180, 20183, 20187, 20188, 
            20189, 20199, 20200, 20201, 20202, 20203, 20205, 20206, 20208, 20211, 
            20212, 20213, 20224, 20225, 20226, 20227, 20230, 20231, 20232, 20235, 
            20236, 20240, 20241, 20242, 20244, 20248, 20253, 20254, 20255, 20259, 
            20261, 20262, 20263, 20265, 20269, 20274, 20275, 20279, 20283, 20286, 
            20291, 20292, 20293, 20294, 20298, 20301, 20302, 20305, 20307, 20308, 
            20316, 20322, 20325, 20337, 20338, 20340, 20341, 20345, 20347, 20351, 
            20356, 20357, 20358, 20363, 20366, 20367, 20369, 20384, 20387, 20391, 
            20398, 20403, 20407, 20409, 20410, 20412, 20415, 20420, 20421, 20424, 
            20428, 20431, 20434, 20435, 20437, 20440, 20441, 20446, 20449, 20453, 
            20456, 20460, 20462, 20473, 20474, 20475, 20477, 20478, 20480, 20483, 
            20484, 20485, 20487, 20490, 20491, 20492, 20494, 20498, 20501, 20502, 
            20508, 20513, 20516, 20520, 20528, 20533, 20534, 20537, 20546, 20548, 
            20549, 20550, 20554, 20557, 20558, 20559, 20563, 20567, 21002, 21005, 
            21012, 21013, 21020, 21023, 21031, 21033, 21038, 21040, 21042, 21044, 
            21046, 21050, 21056, 21058, 21059, 21062, 21067, 21070, 21082, 21084, 
            21088, 21092, 21095, 21097, 21101, 21102, 21117, 21123, 21125, 21128, 
            21130, 21137, 21139, 21144, 21147, 21152, 21163, 21167, 21189, 21193, 
            21196, 21200, 21201, 21211, 21212, 21214, 22002, 22003, 22005, 22007, 
            22010, 22013, 22015, 22017, 22018, 23006, 23007, 24001, 24002, 24003, 
            24004, 24005, 24006, 24007, 24009, 24014, 24015, 24016, 24017, 24018,
            24019, 24022, 24023, 24026, 24027, 24029, 24032, 24033, 24038, 24039, 
            24041, 24042, 24043, 24044, 24045, 24047, 24048, 24049, 24053, 24056, 
            24057, 24058, 26006, 26044, 26045, 26066, 26068, 27009, 27011, 27015, 
            28004, 28005, 28006, 28008, 28020, 28023, 28028, 28029, 28031, 31001, 
            31002, 31011, 31018, 31028, 31029, 31030, 31031, 31033, 31040, 31041, 
            31048, 31052, 31055, 31058, 31059, 31061, 31062, 31067, 31073, 31076, 
            31077, 31079, 31084, 31085, 31091, 31096, 31098, 31100, 31101, 31102, 
            31103, 32001, 32002, 32003, 32004, 32006, 32008, 32011, 32013, 32018, 
            32027, 32030, 32031, 32032, 32033, 32035, 32043, 32044, 32046, 32048, 
            32050, 32052, 32053, 32054, 32058)

#drop the municipalities with few pre-treatment observations:
## remove those from dropthis list:
data_noNA_nomuns<-data_noNA[apply(data_noNA, 1, function(x)  !any(x %in% dropthis)),]
## remove those with few observations:
data_noNA_nomuns<-data_noNA_nomuns %>% group_by(inegi) %>% filter(n()>= 4) %>% ungroup()
                                  
                    
# estimate the generalized synthetic control method
out2 <- gsynth(logdefuncionespc ~ reform, data = data_noNA_nomuns, index = c("inegi", "year"), 
               estimator = "mc",
              force = "two-way", CV = TRUE, se = TRUE, nboots = 1000, cores = 8, r = c(0, 3), 
              min.T0 = 5)
out2_wcov <- gsynth(logdefuncionespc ~ reform, X= winning_margin_governor + governor_alignment, 
                    data = data_noNA_nomuns, index = c("inegi", "year"), 
                     estimator = "mc",
                     force = "two-way", CV = TRUE, se = TRUE, nboots = 1000, cores = 8, r = c(0, 3), 
                     min.T0 = 5)
##takes an hour to run

#plot
out2$est.att %>% 
  as_tibble(rownames = "t") %>% 
  mutate(t = as.numeric(t)) %>% 
  ggplot(aes(x = t, y = ATT)) + 
  geom_ribbon(aes(ymin = CI.lower, ymax = CI.upper), color = "lightgrey", alpha = 1/2) + 
  geom_line(color = "red") + 
  geom_point(color = "red") + 
  geom_hline(yintercept = 0) + 
  geom_vline(xintercept = -0.5, linetype = "dashed") + 
  theme(axis.title.x = element_text("Time relative to Treatment (2015)")) + 
  theme_bw() 
dev.copy(png,'../Figures_incumbency/matrix_completion.png')
dev.off()

#plot
out2_wcov$est.att %>% 
  as_tibble(rownames = "t") %>% 
  mutate(t = as.numeric(t)) %>% 
  ggplot(aes(x = t, y = ATT)) + 
  geom_ribbon(aes(ymin = CI.lower, ymax = CI.upper), color = "lightgrey", alpha = 1/2) + 
  geom_line(color = "red") + 
  geom_point(color = "red") + 
  geom_hline(yintercept = 0) + 
  geom_vline(xintercept = -0.5, linetype = "dashed") + 
  theme(axis.title.x = element_text("Time relative to Treatment (2015)")) + 
  theme_bw() 
dev.copy(png,'../Figures_incumbency/matrix_completion_wcovariates.png')
dev.off()


#############################################
#Goodman-Bacon decomposition
# calculate the bacon decomposition without covariates

bacon_out <- bacon(logdefuncionespc ~ reform,
                   data = balanced,
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
homicides.EventStudy=lfe::felm(logdefuncionespc ~  lag_7 + lag_6 + lag_5 + lag_4 + lag_3 
                               + lag_2  + date_0 + lead_1 + lead_2 + lead_3 
                               + margin_lag_8 + margin_lag_7 + margin_lag_6 
                               + margin_lag_5 + margin_lag_4 + margin_lag_3 
                               + margin_lead_2 + governor_alignment_lag_8 
                               + governor_alignment_lag_7 + governor_alignment_lag_6 
                               + governor_alignment_lag_5 + governor_alignment_lag_4 
                               + governor_alignment_lag_3 + governor_alignment_lag_2
                              | estado + year | 0 | 0, 
                              data = data.wleadlags
                              )
summary(homicides.EventStudy)

#with weights
data.wleadlags.weights<- read_dta("../Data/ConstructionDatabase/data_wleads&lags2_weights.dta")
#erase missing values
data_noNA_weights <- data.wleadlags.weights %>% 
  filter(!is.na(perc))


homicides.EventStudy.weights=lfe::felm(logdefuncionespc ~  lag_7 + lag_6 + lag_5 + lag_4 + lag_3 
                               + lag_2  + date_0 + lead_1 + lead_2 + lead_3 
                               + margin_lag_8 + margin_lag_7 + margin_lag_6 
                               + margin_lag_5 + margin_lag_4 + margin_lag_3 
                               + margin_lead_2 + governor_alignment_lag_8 
                               + governor_alignment_lag_7 + governor_alignment_lag_6 
                               + governor_alignment_lag_5 + governor_alignment_lag_4 
                               + governor_alignment_lag_3 + governor_alignment_lag_2
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
dev.copy(png,'../Figures_incumbency/pretrends_sensitivity.png')
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
                     numPrePeriods, numPostPeriods, timeVec,
                     referencePeriod, useRelativeEventTime = T)

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
dev.copy(png,'../Figures_incumbency/pretrends_sensitivity_decreasing.png')
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
dev.copy(png,'../Figures_incumbency/pretrends_sensitivity_increasing.png')
dev.off()

#####With increasing bias:


#####Better plot
data.wleadlags %>% do(fit = felm(model_formula, data = ., 
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
dev.copy(png,'../Figures_incumbency/event_study.png')
dev.off()

###############
##ERRORS TO CHECK



