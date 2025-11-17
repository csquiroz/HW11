## Homework 11 ##
## Cody Quiroz ##
## 11-November-25 ##

## Objective 1 ##
library(ggplot2)
set.seed(67) #reprod

#params
n <- 100 #total obs
true_int <- 20 #intercept
true_slope <- 3 #slope for free-flow
true_group_effect <- 10 #fake slope difference between free and impounded
true_interaction <- 1.5 #interaction (increase slope) in addition to effect slope for impoundment
sigma <- 2 #residual stdev, normal

#make fake predictor data
DOC <- runif(n, 2, 15)
ReachType <- factor(rep(c("Free-flowing", "Impounded"), each = n/2))

#error, normal, homoscedastic
eps <- rnorm(n, mean = 0, sd = sigma)

#make fake response variables
CH4_flux <- true_int + true_slope * DOC +
  ifelse(ReachType == "Impounded",
         true_group_effect + true_interaction * DOC,
         0) + eps

# assemble dataframe
ancova_data <- data.frame(
  ReachType = ReachType,
  DOC_mgL = round(DOC, 3),
  CH4_flux_umol = round(CH4_flux, 3)
)

#fit ancova w interaction
ancova_model <- lm(CH4_flux_umol ~ DOC_mgL * ReachType, data = ancova_data)
print(summary(ancova_model))

#save to csv for sharing
write.csv(ancova_data, "simulated_data_csq.csv", row.names = FALSE)

#visualize ancova
ggplot(ancova_data, aes(x = DOC_mgL, y = CH4_flux_umol, color = ReachType)) +
  geom_point(size = 2, alpha = 0.7) +
  geom_smooth(method = "lm", se = FALSE, linewidth = 1.2) +
  theme_bw() +
  labs(
    x = "Dissolved Organic Carbon (mg/L)",
    y = expression(paste("Methane Flux (", mu, "mol CH"[4], " ", m^-2, " ", h^-1, ")")),
    color = "Reach Type"
  ) 

#Does the relationship between DOC and Methane differ in free-flowing and impounded reaches of a stream?


## Objective 2 ##
cond_data <- read_csv("simulated_data.csv") #file from aubrey

#make sure temp c a factor
cond_data <- cond_data %>%
  mutate(Temperature_C = factor(Temperature_C))

glimpse(cond_data) #check is factor

#fit model
#question: Does the relationship between Cond and Cl differ in low and high temperature conditions?
mod_full <- lm(Cl_mgL ~ Cond_uscm * Temperature_C, data = cond_data)
summary(mod_full)
anova(mod_full)

#backward reduction
drop1(mod_full, test = "F")

#interaction not significant so removing
mod_no_int <- lm(Cl_mgL ~ Cond_uscm + Temperature_C, data = cond_data)
drop1(mod_no_int, test = "F")

#based on significance, choosing this as final model
final_mod <- mod_no_int
summary(final_mod)

#ecological interpretation
#Conductivity was strongly and positively related to chloride concentration. The temperature did not alter the slope of this relationship, meaning the effect of conductivity on chloride was consistent across temperature conditions. The values were consistently lower in the when temperature was low (different intercepts)

#My partner's parameters were slope = 0.5 for both low and high temp data, while the intercept was 3 for low temp conditions and 5 for high temp. The slope I calculated was ~0.49, so I was able to get very close to the true values. I also matched the significantly different slopes. I found there was no interaction as intended. 
