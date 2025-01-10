
# PE linear mixed effects model 
library(lme4)
library(car)

# Model using the P1D (daily) subsampled data with the PE calculated using a 50 day moving window
glimpse(PE_ts_P1D)

# add a day-of-year variable
PE_ts_P1D <- PE_ts_P1D |> 
  mutate(day = yday(date)) 

###########################################################
# ----- Testing among variable/reservoir differences -----
# Filter to only surface observations
# Chla will be listed as the first variable because it is alphabetical
PE_ts_P1D_surface <-  PE_ts_P1D |> 
  filter(depth_m == 'surface')
# Site_id, variable tests
# Fit the model for surface variables
model_surface <- lmer(PE ~ site_id + variable + site_id*variable + (site_id*variable | day), 
                      data = PE_ts_P1D_surface)

# Evaluate model
summary(model_surface)
car::Anova(model_surface, type = 3, test.statistic = 'F')


## ----- Reorder the factors and check output
# Water temp will be listed first in this model
# Filter to only surface observations
PE_ts_P1D_surface_reorder <-  PE_ts_P1D |> 
  filter(depth_m == 'surface') |> 
  mutate(variable = factor(variable, levels = c("Tw_C","Chla_ugL", "DO_mgL", "SpCond_uScm",  "fDOM_QSU")))
# Site_id, variable tests
# Fit the model for surface variables
model_surface_reorder <- lmer(PE ~ site_id + variable + site_id*variable + (site_id*variable | day), 
                      data = PE_ts_P1D_surface_reorder)

# Evaluate model
summary(model_surface_reorder)
car::Anova(model_surface_reorder, type = 3, test.statistic = 'F')


###########################################################
# ---- Testing for depth/reservoir/variable differences ----
# Filter to only include DO and Tw (that have both depths and sites)
PE_ts_P1D_Tw_DO <-  PE_ts_P1D |> 
  filter(variable %in% c('Tw_C', 'DO_mgL'))

model_Tw_DO <- lmer(PE ~ site_id + variable + depth_m + 
                      variable*depth_m + variable*site_id + site_id*depth_m + 
                      site_id*variable*depth_m + 
                      (site_id*variable*depth_m | day),
                    data = PE_ts_P1D_Tw_DO)

# Evaluate model
summary(model_Tw_DO)
car::Anova(model_Tw_DO, type = 3, test.statistic = 'F')
