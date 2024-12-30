
# PE linear mixed effects model 
library (lme4)
library (car)

# Model using the P1D (daily) subsampled data with the PE calculated using a 50 day moving window
glimpse(PE_ts_P1D)

# add a day-of-year variable
PE_ts_P1D <- PE_ts_P1D |> 
  mutate(day_of_year = yday(date))

# Fit the model with site, variable and day of year
model_test <- lmer(PE ~ site_id + variable + site_id*variable + (site_id + variable | day_of_year), 
                  data = PE_ts_P1D)

# Evaluate model
summary(model_test)
car::Anova(model_test, type = 3, test.statistic = 'F')
