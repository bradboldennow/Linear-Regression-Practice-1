# read the states data
states.data <- readRDS("states.rds") 
#get labels
states.info <- data.frame(attributes(states.data)[c("names", "var.labels")])
#look at last few labels
tail(states.info, 8)

library(ggplot2)

# Plot the data we initially want to test
ggplot(states.data, aes(x = metro, y = energy)) +
  geom_point()

# Calculate the mean energy level
mean_energy = mean(states.data$energy, na.rm = TRUE)

# Plot the data with the mean energy level line plotted
ggplot(states.data, aes(x = metro, y = energy)) +
  geom_point() +
  geom_hline(aes(yintercept = mean_energy))

# Use lm to fit a regression line through these data
energy_reg = lm(energy~ metro, data = states.data)
summary(energy_reg)

# Plot the regression line 
ggplot(states.data, aes(x = metro, y = energy)) +
  geom_point() +
  geom_hline(aes(yintercept = mean_energy)) +
  geom_smooth(method = "lm", formula = y~x)

# Plot the model
plot(energy_reg)

# Plot the residuals
ggplot(energy_reg, aes(x = .fitted, .resid)) +
  geom_point()

# Plot showing additional variable: toxic
ggplot(states.data, aes(x = toxic, y = energy)) +
  geom_point() +
  geom_hline(aes(yintercept = mean_energy))

# Use lm to find a different model that predicts energy based on two variables
energy_reg2 = lm(energy~ metro + toxic, data = states.data)
summary(energy_reg2)

plot(energy_reg2)
#Using metro + toxic seems to produce a much better linear repression model. 

# Same lm model but with an added interaction term
energy_regInt = lm(energy~ metro + toxic*density, data = states.data)
summary(energy_regInt) # Performs slightly better than before

# Use lm model to predict energy by region
energy.region = lm(energy~ region, data = states.data)
summary(energy.region)

ggplot(states.data, aes(x = region, y = energy)) +
  geom_jitter(alpha = .3)
