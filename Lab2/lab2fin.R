library(ggplot2)

Dataset = read.csv("NY-House-Dataset.csv")

View(Dataset)

Prices <- Dataset$PRICE
SqFt <- Dataset$PROPERTYSQFT
Beds <- Dataset$BEDS
Baths <- Dataset$BATH

# Linear Models
first_model <- lm(Prices ~ Beds, data = Dataset)
second_model <- lm(Prices ~ Beds + Baths, data = Dataset)
third_model <- lm(Prices ~ Beds + Baths + SqFt, data = Dataset)

summary(first_model)
summary(second_model)
summary(third_model)

# Cleaning

# Remove rows with NA values
Dataset <- na.omit(Dataset)

# Remove outliers
Dataset <- Dataset[Dataset$BEDS < 10 & Dataset$BATH < 10, ]
Dataset <- Dataset[Dataset$PRICE < 10000000 & Dataset$PROPERTYSQFT < 10000, ]
Dataset <- Dataset[Dataset$BEDS > 0 & Dataset$BATH > 0 & Dataset$PRICE > 0 & Dataset$PROPERTYSQFT > 0, ]

View(Dataset)

Prices <- Dataset$PRICE
SqFt <- Dataset$PROPERTYSQFT
Beds <- Dataset$BEDS
Baths <- Dataset$BATH

# Refitting models
first_model <- lm(Prices ~ SqFt, data = Dataset)
second_model <- lm(Prices ~ SqFt + Beds, data = Dataset)
third_model <- lm(Prices ~ SqFt + Beds + Baths, data = Dataset)

# Model 1 #
summary(first_model)

ggplot(Dataset, aes(x = SqFt, y = Prices)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm") 

ggplot(data = Dataset, aes(x = first_model$fitted.values, y = first_model$residuals)) +
  geom_point() +
  geom_hline(yintercept = 0, color = "red") +
  labs(x = "Fitted Values", y = "Residuals")

# Model 2 #
summary(second_model)

ggplot(Dataset, aes(x = SqFt, y = Prices)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm") 

ggplot(data = Dataset, aes(x = second_model$fitted.values, y = second_model$residuals)) +
  geom_point() +
  geom_hline(yintercept = 0, color = "red") +
  labs(x = "Fitted Values", y = "Residuals")

summary(third_model)

ggplot(Dataset, aes(x = SqFt, y = Prices)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm") 

ggplot(data = Dataset, aes(x = third_model$fitted.values, y = third_model$residuals)) +
  geom_point() +
  geom_hline(yintercept = 0, color = "red") +
  labs(x = "Fitted Values", y = "Residuals")

