Data <- read.csv("epi_results_2024_pop_gdp_v2.csv")

unique(Data$region)
colnames(Data)

ME_subset = Data[Data$region == "Greater Middle East", ]

GW_subset = Data[Data$region == "Global West", ]

#### Question 1 ####

 ## 1.1 ##

library(ggplot2)

# Boxplot Greater Middle East

ggplot(ME_subset, aes(y = GTI.new)) +
  geom_boxplot(fill = "#228B22") +
  labs(title = "GTI Boxplot Greater Middle East",  y = "GTI Score")

# Histogram with Density Greater Middle East

ggplot(ME_subset, aes(x = GTI.new)) +
  geom_histogram(aes(y = after_stat(density)),
                 bins = 10,
                 fill = "#228B22",
                 color = "black") +
  geom_density(alpha = 0.5, color = "red", linewidth = 1) +
  labs(title = "GTI Histogram in Greater Middle East", x = "EPI Score")

# Boxplot Global West 

ggplot(GW_subset, aes(y = GTI.new)) +
  geom_boxplot(fill = "#aa1") +
  labs(title = "GTI Boxplot Greater Middle East",  y = "GTI Score")

# Histogram with Density Global West

ggplot(GW_subset, aes(x = GTI.new)) +
  geom_histogram(aes(y = after_stat(density)),
                 bins = 10,
                 fill = "#aa1",
                 color = "black") +
  geom_density(alpha = 0.5, color = "#22a", linewidth = 1) +
  labs(title = "GTI Histogram in Global West", x = "EPI Score")

 ## 1.2 ##

GTI_ME <- na.omit(ME_subset$GTI.new)

GTI_GW <- na.omit(GW_subset$GTI.new)

qqplot(GTI_ME, GTI_GW,
       main = "Greater Middle East vs Global West",
       xlab = "GME",
       ylab = "GW")

#### Question 2 ####

 ## 2.1 ##

  # GTI vs GDP Model #

GTI_GDP <- lm(GTI.new ~ gdp, data = Data)
summary(GTI_GDP)

ggplot(Data, aes(x = gdp, y = GTI.new)) +
  geom_point(alpha = 0.6, na.rm = TRUE) +
  geom_smooth(method = "lm", color = "blue", na.rm = TRUE) +
  labs(title = "GDP vs GTI", 
       x = "GDP", 
       y = "GTI")

plot(GTI_GDP$fitted.values, GTI_GDP$residuals,
     main = "Residuals vs Fitted (GDP)",
     xlab = "Fitted Vals", ylab = "Residuals")
abline(h = 0, col = "red")

  # GTI vs Population Model #

GTI_POP <- lm(GTI.new ~ population, data = Data)
summary(GTI_POP)

ggplot(Data, aes(x = population, y = GTI.new)) +
  geom_point(alpha = 0.6, na.rm = TRUE) +
  geom_smooth(method = "lm", color = "blue", na.rm = TRUE) +
  labs(title = "Population vs GTI", 
       x = "Population", 
       y = "GTI")

plot(GTI_POP$fitted.values, GTI_POP$residuals,
     main = "Residuals vs Fitted (Population)",
     xlab = "Fitted Vals", ylab = "Residuals")
abline(h = 0, col = "red")

 ## 2.2 ##

GTI_GDP_log <- lm(GTI.new ~ log(gdp), data = Data)
summary(GTI_GDP_log)

GTI_POP_log <- lm(GTI.new ~ log(population), data = Data)
summary(GTI_POP_log)

ggplot(Data, aes(x = log(gdp), y = GTI.new)) +
  geom_point(alpha = 0.6, na.rm = TRUE) +
  geom_smooth(method = "lm", color = "blue", na.rm = TRUE) +
  labs(title = "log GDP vs GTI", 
       x = "GDP", 
       y = "GTI")

plot(GTI_GDP$fitted.values, GTI_GDP_log$residuals,
     main = "Residuals vs Fitted (log GDP)",
     xlab = "Fitted Vals", ylab = "Residuals")
abline(h = 0, col = "red")

#### Question 3 ####

library(class)
set.seed(1)

 ## 3.1 ##

Data_Clean <- na.omit(Data)


features <- Data_Clean[, c("AIR.new", "H2O.new", "CCH.new")]
class_labels <- Data_Clean$region

k_value <- 5

predictions <- knn(train = features, 
                          test = features, 
                          cl = class_labels, 
                          k = k_value)

conf_matrix <- table(Predicted = predictions, Actual = class_labels)
print(conf_matrix)

accuracy <- sum(diag(conf_matrix)) / sum(conf_matrix)

 ## 3.2 ##

features2 <- Data_Clean[, c("SOE.new", "NOD.new", "VOE.new")]

predictions2 <- knn(train = features2, 
                    test = features2, 
                    cl = class_labels, 
                    k = k_value)

conf_matrix2 <- table(Predicted = predictions2, Actual = class_labels)
print(conf_matrix2)

accuracy2 <- sum(diag(conf_matrix2)) / sum(conf_matrix2)

print(paste("Model 1 Accuracy:", round(accuracy, 4)))
print(paste("Model 2 Accuracy:", round(accuracy2, 4)))
