library(PanelMatch)

# I changed a bit the code because I had multiple outcomes but hopefully it makes sense

# Build matched sets
PM <- PanelMatch(lag = 4, time.id = "year", unit.id = "unit_id",
                 treatment = "t", 
                 refinement.method = "ps.weight",
                 data = d, 
                 match.missing = TRUE,
                 covs.formula = ~ I(lag(t, 1:4)) + I(lag(dv, 1:4)) + covariates,
                 qoi = "att", 
                 outcome.var = "dv",
                 lead = 0:3,
                 forbid.treatment.reversal = F)

# Extract covarite balance for outcome
cov_bal <- get_covariate_balance(PM$att, d, covariates = "dv", plot = F) %>%
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



