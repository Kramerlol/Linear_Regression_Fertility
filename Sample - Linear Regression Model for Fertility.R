

## Regression model for fertility

install.packages("tidyverse")  # For data manipulation
install.packages("broom")      # For regression output formatting
install.packages("viridis")
install.packages("viridisLite")

library(tidyverse)
library(viridisLite)
library(viridis)
library(tidyverse)
library(broom)

dir()
## Read CSV to create the dataset
## Alternatively, create a dataset directly from the source using an R package
## E.g. Human Fertility Database (HMDHFDplus), UN WPP, World Bank
data <- read.csv("USA_YOUTH.csv")
colnames(data)

##Normalize the GDP per capita with log scale; 
## Large GDP per capita values can lead to very small coefficients for GDPpc.
## Data would come fro m
data$log_GDPpc <- log(data$GDPpc)


##Standardize scale (If variables are on vastly different scales, the regression coefficients can be misleading.)
names(data) <- c("Year", "TFR", "TDR", "FLFP", "GDPpc")
str(data)  # Check if all columns are numeric
data_scaled <- data  # Create a copy of the dataset 
data_scaled[, c("FLFP")] <- scale(data[, c("FLFP")]) #Standardize the data
#data_scaled[, c("FLFP", "Edu", "GDPpc")] <- scale(data[, c("FLFP", "Edu", "GDPpc")])
#data$TDR <- data$TDR * sd(original_TDR) + mean(original_TDR) --no need to scale any longer?
data_scaled$log_GDPpc <- log(data_scaled$GDPpc) 
model_scaled <- lm(TFR ~ TDR + FLFP + GDPpc, data = data_scaled)

##Add time - removed
model_time <- lm(TFR ~ Year + TDR + FLFP + Edu + GDPpc, data = data_scaled)
summary(model_time)
summary(model_scaled) #Regression on standardized data

##Create Regression Model
model <- lm(TFR ~ TDR + FLFP + EDU + GDPpc, data = data)
summary(data)
str(data_scaled)

library(ggplot2)
library(car) #for VIF


ggplot(model_interact, aes(x = TDR, y = TFR)) +
  geom_point() +
  geom_smooth(method = "lm", col = "blue") +
  labs(title = "Relationship between TDR and TFR", x = "Total Dependency Ratio", y = "TFR")
#scale_color_viridis(discrete = TRUE, option = "D")
#+ scale_fill_viridis(discrete = TRUE) 

# Test if Since dependency ratio (TDR) and female labor force participation (FLFP) may interact, try adding an interaction term
# Why? This tests whether the effect of TDR on fertility depends on FLFP levels.
model_interact <- lm(TFR ~ TDR * FLFP + Edu + GDPpc, data = data_scaled)
summary(model_interact)

#Without year
model_no_year <- lm(TFR ~ TDR + FLFP + Edu + GDPpc, data = data_scaled)
vif(model_no_year)

summary(data$TDR)
summary(data$PC1)

cor(data_scaled)
PC1_centered = PC1 - mean(PC1)
data_scaled$FLFP_std = scale(data_scaled$FLFP)

## C. Use Principal Component Analysis (PCA)
##If removing variables isn’t an option, PCA can combine highly correlated variables into one factor:
pca_data <- prcomp(data_scaled[, c("FLFP", "log_GDPpc")], scale = TRUE)
summary(pca_data)
## Extract only PCA1
data$PC1 <- pca_data$x[, 1]
# PCA model:
model_pca <- lm(TFR ~ TDR + PC1, data = data)
summary(model_pca)
vif(model_pca)

head(pca_data$x)
head(data_scaled)

## Make all of the values flat and run the model again
data$FLFP <- data$FLFP / 100
data$Edu <- data$Edu / 100
data$TDR <- data$TDR / 100
data$log_GDPpc <- log(data$GDPpc)
data_scaled <- scale(data[, c("TDR", "FLFP", "Edu", "GDPpc")])

## If stored as strings, convert with these
data$FLFP <- as.numeric(data$FLFP)
data$Edu <- as.numeric(data$Edu)
data$TDR <- as.numeric(data$TDR)

install.packages("car")
library(carData)
library(car)
vif(model_pca)

## + Edu + GDPpc
model_interact <- lm(TFR ~ TDR * PC1, data = data_scaled)
summary(model_interact)

model_interact <- lm(TFR ~ TDR * FLFP, data = data_scaled)
colnames(data_scaled)

library(MASS)
stepwise_model <- stepAIC(model_pca, direction = "both")
summary(stepwise_model)
data$PC1_resid <- residuals(lm(PC1 ~ FLFP, data = data_scaled))

install.packages("ggthemes")
library(ggthemes)


p <- ggplot(model_pca, aes(x = TDR, y = TFR)) +
  geom_point(color='darkmagenta') +
  geom_smooth(method = "lm", col = "blue")


p + theme_wsj()+ scale_colour_wsj("colors6")+
  labs(title = "Relationship between TDR and TFR, USA: 1975-2020", x = "Total Dependency Ratio (TDR)", y = "Tempo-adj. Total Fertility Rate (TFR)")

complete.cases(data_scaled)

model_simpler <- lm(TFR ~ TDR + FLFP, data = data_scaled)
summary(model_simpler)
vif(model_simpler)

model_interaction <- lm(TFR ~ TDR * FLFP, data = data_scaled)
summary(model_interaction)

par(mfrow = c(2, 2))  # Arrange plots in 2x2 grid
plot(model_quad)

model_quad <- lm(TFR ~ TDR + PC1, data = data_scaled) 
  #lm(TFR ~ TDR + log(GDPpc), data = data_scaled) #lm(TFR ~ TDR + FLFP + log(GDPpc), data = data_scaled)
summary(model_quad)
vif(model_pca)

cor(data_scaled)
FLFP_centered = FLFP - mean(FLFP)
data_scaled$FLFP_std = scale(data_scaled$FLFP)



#Wall Street Journal Theme
theme_wsj <- function(base_size = 12,
                      color = "brown",
                      base_family = "sans",
                      title_family = "mono") {
  colorhex <- ggthemes::ggthemes_data$wsj$bg[color]
  theme_foundation(base_size = base_size, base_family = base_family) +
    theme(line = element_line(linetype = 1, colour = "black"),
          rect = element_rect(fill = colorhex, linetype = 0, colour = NA),
          text = element_text(colour = "black"),
          title = element_text(family = title_family,
                               size = rel(1)),
          axis.title = element_text(face = "bold", size=12),
          axis.text = element_text(face = "bold", size = rel(1)),
          axis.text.x = element_text(colour = NULL),
          axis.text.y = element_text(colour = NULL),
          axis.ticks = element_line(colour = NULL),
          axis.ticks.y = element_blank(),
          axis.ticks.x = element_line(colour = NULL),
          axis.line = element_line(),
          axis.line.y = element_blank(),
          legend.background = element_rect(),
          legend.position = "top",
          legend.direction = "horizontal",
          legend.box = "vertical",
          panel.grid = element_line(colour = NULL, linetype = 3),
          panel.grid.major = element_line(colour = "black"),
          panel.grid.major.x = element_blank(),
          panel.grid.minor = element_blank(),
          plot.title = element_text(hjust = 0, face = "bold"),
          plot.margin = unit(c(1, 1, 1, 1), "lines"),
          strip.background = element_rect())
}

