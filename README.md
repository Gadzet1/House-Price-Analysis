# House-Price-Analysis
```{r include=FALSE}
library(ggplot2)
library(dplyr)
library(corrplot)
houses <- read.csv(file = "houses.csv")
```

# Introduction

The data we will be working with pertains to real estate in the USA. The dataset consists of 4600 observations and 18 columns. We aim to investigate how variables such as property size, number of bathrooms, or property condition affect the price. An analysis of the impact of various factors (such as property size, number of bathrooms or bedrooms) on the classification of property prices will be conducted.

# Hypothesis

Many factors influence property prices.

# Variables
|variable |description|
| --- | --- |
|`date` | data collection date|
|`price`| property price ($)|
|`bedrooms`| number of bedrooms|
|`bathrooms` |number of bathrooms (1 - sink, toilet, and bathtub/shower, 0.75 - sink, toilet, and bathtub OR shower, 0.5 - sink and toilet, 0.25 - only toilet)|
|`sqft_living`| living area of the building (in square feet)|
|`sqft_lot`| lot size (in square feet)|
|`floors`| number of floors|
|`waterfront` |1 - waterfront property, 0 - no waterfront|
|`view` |quality of the view from the property (on a scale of 0-4)|
|`condition`| property condition (on a scale of 0-5)|
|`sqft_above` |living area above ground level (excludes basement area)|
|`sqft_basement` |basement area (in square feet)|
|`yr_built` |year built|
|`yr_renovated` |year renovated|
|`street`| street|
|`city` |city|
|`statezip` |state code|
|`country` |country (USA)|

# Data transformation

```{r}
head(houses)
sum(is.na(houses))
```
Not all parameters will be useful for modeling. Therefore, we proceed to eliminate some of them. Information about the date, similar to the address, does not contribute anything to the study, so we remove it from our database. The information about the number of floors $floors$ seems rather unusual (not whole numbers) and somewhat duplicates information about the property size, so we do not include it in further studies. The same goes for the view characteristic $view$ (almost all cases have a value of 0).

The remaining variables are retained, but some require modification. All parameters with "sqft" (square foot) are converted to $m^{2}$.

# Conversion to $m^2$

The data is based on property prices from the United States, where the area is given in square feet. We will convert them to square meters, as this unit is more familiar to us.
```{r}
houses$sqft_living <- houses$sqft_living*0.092903
houses$sqft_lot <- houses$sqft_lot*0.092903
houses$sqft_above <- houses$sqft_above*0.092903
```

# Renaming variables

When converting to square meters, we also rename the columns, as they suggest the previous unit that we changed.
```{r}
houses <- houses %>% 
  rename("m2_living" = "sqft_living", "m2_lot" = "sqft_lot", 
         "m2_above" = "sqft_above")
```

# Conversion to Categorical Variables

We convert the `sqft_basement` feature to a binary variable (where 1 means the property has a basement and 0 means it does not), as it does not make sense to compare basement area when more than half of the properties do not have one.

```{r}
houses %>% count(sqft_basement == 0)
```

Similarly, the year of renovation is changed to a binary variable, where 1 means the property has been renovated, and 0 means no renovation took place. The final change is in the construction year of the property. This results in old (0) and new (1) properties, with 1970 as the cutoff point, as it is close to the median.

```{r}
median(houses$yr_built)
```

Some data is converted to binary variables for easier analysis. For example, the year of renovation is not as significant. More useful information will be whether the property has been renovated or is in its original condition.
```{r}
houses$is_renovated <- ifelse(houses$yr_renovated > 0, 1, 0)
houses$is_new <- ifelse(houses$yr_built> 1970, 1, 0)
houses$is_basement <- ifelse(houses$sqft_basement > 0, 1, 0)
head(houses)
```

# Plotting Against the Dependent Variable

The variables are preliminarily organized, but the cases should also be examined. Our dependent variable is the property price. Let’s look at its values.
```{r}
plot(houses$price, xlab = "Numer obserwacji", ylab = "Cena [w $]")
```
As we can see, there are values that stand out significantly. These are properties with very high prices. Therefore, we decide to eliminate cases where the property price exceeds 1 million dollars, as they are highly atypical in the studied problem.

# Removing Outliers

```{r}
houses <- houses[,c(2:14, 19:21)]
houses$price <- ifelse(houses$price == 0, NA, houses$price)
houses$price <- ifelse(houses$price > 1000000, NA, houses$price)
houses <- houses[complete.cases(houses), ]
plot(houses$price, xlab = "Numer obserwacji", ylab = "Cena [w $]")
```
After removing property prices above one million dollars, the distribution of observations on the plot is clearer.
We will also add a new column containing categorical variables, which we will refer to in our analysis. This column will indicate whether the property price is below $500,000 (0) or above this amount (1).

```{r}
houses$price_classification <- ifelse(houses$price > 500000.0, 1, 0)
```

After all transformations, our data looks as follows:
```{r}
houses_clean <- houses[,c(1:5,7,9,10,14:17)]
head(houses_clean)
```
# Data Types

Sprawdzamy jakie mamy typy danych w poszczególnych kolumnach w celu ułatwienia dalszej analizy.
```{r}
sapply(houses_clean,class)
```

# Variables After Transformation
|variable |description|
|---|---|
|`price`| property price ($)
|`bedrooms` |number of bedrooms
|`bathrooms` |number of bathrooms (1 - sink, toilet, and bathtub/shower, 0.75 - sink, toilet, and bathtub OR shower, 0.5 - sink and toilet, 0.25 - only toilet)
|`m2_living` |living area of the building (in square meters)
|`m2_lot` |lot size (in square meters)
|`waterfront` |1 - waterfront property, 0 - no waterfront
|`condition`| property condition (on a scale of 0-5)
|`m2_above`| living area above ground level (excludes basement area)
|`is_renovated` |whether it has been renovated (1 - yes, 0 - no)
|`is_new` |whether it is new - built after 1970 (1 - yes, 0 - no)
|`is_basement` |whether there is a basement (1 - yes, 0 - no)
|`price_classification` |price above 500,000 (1), below (0)

With our database of potential variables prepared, it's time to conduct a preliminary analysis. In our case, the dependent variable will be `price_classification`.

```{r}
summary(houses_clean)
```
The lowest property price in our data is $7,800, and the highest reaches $1 million. The average living area is 185.67 $m^2$. The largest discrepancy is in the values of the lot size ($m^2$). The lowest value is 59.27, and the highest is 99,798.07.

The average price hovers around $500,000, but let's see how the data is quantitatively distributed between the two price categories.
```{r}
ggplot(houses_clean, aes(x = factor(price_classification),fill = factor(price_classification))) +
  geom_bar() +
  labs(title = "Number of houses per price",
       x = "Price classification",
       y = "Number of houses") +
  scale_x_discrete(labels = c("Less than 500 000", "Over 500 000"))+
  scale_fill_manual(values = c("lightblue", "lightpink"))

```
We see that the majority of properties in our data are priced below $500,000.

Let's see on a plot whether the number of bathrooms affects the house price.
```{r}
ggplot(houses_clean, aes(x = factor(price_classification), y = bathrooms)) +
geom_boxplot() +
labs(title = "",
x = "price",
y = "number of bathrooms")
```
The median number of bathrooms in houses above $500,000 is higher than in houses below $500,000, suggesting that more expensive properties tend to have more bathrooms. In the higher price category, the maximum number of bathrooms is 4.75, whereas in the lower price category, it is 3.75. Both price groups contain outliers, but they are more prominent in the group of houses priced above $500,000. The range of the number of bathrooms in houses below $500,000 is greater, indicating a wider variety in the number of bathrooms in this price category. On average, higher-priced properties have more bathrooms. This observation aligns with the expectation that the more a house has to offer, the higher its price.

Now, let's examine how the house price relates to its size and whether it has been renovated.

```{r}
ggplot(houses_clean, aes(x = price, y = m2_living,color = factor(is_renovated))) + geom_point() +
  labs(title = "Price vs Area", x = "Cena ($)", y = "House area (m2)") +
  scale_color_manual(values = c("darkblue", "cornflowerblue"))
```
The data shows that there are houses priced close to a million dollars with only 150 m² (the largest area being 553 m²) and that have not been renovated. There are also houses with almost 500 m², renovated, priced at around $200,000. As expected, on average, the larger the living area, the higher the price. However, whether a house has been renovated does not seem to significantly impact its price. Let's confirm this with our model.

# Models

Let's analyze how the living area and the condition of the property affect the likelihood that the property costs more than $500,000. We'll create a logistic regression model (GLM) with the dependent variable being the price (`price_classification`) and the independent variables being the living area (`m2_living`) and whether the property has been renovated (`is_renovated`).

```{r}
model1 <- glm(price_classification ~ m2_living + is_renovated, houses_clean, family=binomial) 
summary(model1)
```
The estimated coefficient for `is_renovated` is 0.04387, suggesting that renovation does not significantly affect the probability of a house being priced above $500,000. The variable's z-value is low (0.58) and the p-value is statistically insignificant, indicating that it is not a good predictor in this model. This confirms our hypothesis that renovation does not significantly influence house prices.

The estimated coefficient for `m2_living` is 0.01875, meaning that each additional square meter of living area increases the likelihood that the property will cost more than $500,000. The variable's z-value is high (28.32) and the p-value is low, indicating a statistically significant influence on the probability that the house falls into the higher price category.

Let's now examine the diagnostic plots of the model.

```{r}
plot(model1)
```
Residuals vs. Fitted:
This plot shows residuals against fitted values. Points should be evenly distributed around the x-axis. The red line (smooth) indicates slight model misfit for higher values.
Normal Q-Q:
Points should align along the straight line, suggesting a normal distribution of residuals. Initial deviation indicates that residuals are not perfectly normally distributed.
Scale-Location:
This plot checks for homoscedasticity. The red curve should be horizontal, indicating constant variance of residuals along the fitted values. Our red curve is not horizontal for average values, indicating heteroscedasticity.
Residuals vs. Leverage:
Two notable points (4433, 4399) may indicate influential observations that significantly affect the model fit.

In the next model, we will examine how the number of bathrooms and bedrooms affects the likelihood of the property costing more than $500,000.

```{r}
model2 <- glm(price_classification ~ bathrooms + bedrooms, houses_clean, family=binomial) 
summary(model2)
```
Based on the p-value, both variables affect whether the property price exceeds $500,000. Each additional bathroom increases the likelihood of the property being priced above $500,000. Based on the z-value, we can infer that the number of bathrooms influences price classification more than the number of bedrooms.

```{r}
plot(model2)
```
Residuals vs. Fitted:
Points should be evenly distributed. The red line indicates misfit for high values.
Normal Q-Q:
Points deviate more from the line towards the ends, suggesting residuals are not perfectly normally distributed.
Scale-Location:
Variance of residuals is uneven for average values, indicating heteroscedasticity.
Residuals vs. Leverage:
Three influential points (3912, 2280, 1018) significantly affect the model fit, with point 3912 being the most influential.

In the final model, we will include variables that previously proved significant. In earlier models, we analyzed four independent variables, but one—`is_renovated`—was not influential, so we omit it.

```{r}
model3 <- glm(price_classification ~ m2_living + bathrooms + bedrooms, houses_clean, family=binomial) 
summary(model3)
```
This model reveals two interesting relationships. First, although `bathrooms` was previously significant, it is no longer significant in this model due to the highly influential variable `m2_living`. Adding `m2_living` to the model makes the number of bathrooms statistically insignificant, meaning it no longer significantly influences the likelihood that the property will cost more than $500,000.

Second, the negative coefficient (-0.2996) suggests that a higher number of bedrooms is associated with a lower probability of the house being in the higher price category. In other words, as the number of bedrooms increases, the chance of the house being priced higher decreases. The number of bedrooms is a significant factor affecting the house price but in a counterintuitive way. A higher number of bedrooms does not always translate to a higher price, which could be due to various factors, such as the total size of the house.

```{r}
plot(model3)
```
Residuals vs. Fitted:
The red line suggests slight misfit for higher values.
Normal Q-Q:
Points deviate from the line at the beginning, indicating residuals are not perfectly normally distributed.
Scale-Location:
Variance of residuals is uneven for average values, indicating heteroscedasticity.
Residuals vs. Leverage:
No influential points are visible in this plot, with the red line nearly perfectly horizontal.

# Summary

Based on the above analysis, we can conclude that whether a property's price exceeds $500,000 depends on various factors such as the living area, the number of bathrooms, and the number of bedrooms.
