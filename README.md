# Stat-172-Final-Project

## Project Overview
This project, created as part of the Stat-172 course at Drake University, uses R-Studio to analyze food insecurity in Iowa. Working with Wesley Life, a non-profit organization dedicated to delivering food to those in need, we aimed to identify counties with the highest levels of food insecurity to assist their planned expansion beyond Polk County.

The analysis uses PUMA (Public Use Microdata Area) data to provide insights and help Wesley Life prioritize counties for their expansion.

## Data Sources
We worked with two primary datasets:

#### CPS (Current Population Survey):
Collected at the individual level with additional household information.

Includes data on food insecurity but is limited as it does not reach all counties, making it insufficient for a full analysis on its own.

#### ACS (American Community Survey):
Census data collected across Iowa, including PUMA data.

Does not directly measure food insecurity but contains other relevant variables for predictive modeling.

## Method
#### Model Training and Testing:
We trained a predictive model using 70% of the CPS data, focusing on food insecurity measures, using the other 30% as a testing model to ensure accuracy.

We used both a Lasso (alpha = 1) and Ridge (alpha = 0) regressions for these models, as well as cross-validation to select the best penalty parameter (lambda) for each model based on performance metrics.

We generated ROC curves to assess the predictive accuracy of the models on the testing dataset, including sensitivity and specificity at different thresholds. Comparing AUC (Area Under the Curve) values for both the Lasso and Ridge regression models.

#### Model Application
We applied the trained models to our ACS data, which does not include direct food insecurity measures, to predict food insecurity probabilities at the PUMA level.

#### Model Views
We utilized PUMA shapefiles for Iowa to create choropleth maps that visualize food insecurity probabilities. Mapping overall probabilities and focusing on the elderly population, grouping by PUMA and calculating weighted means.

#### Predictions with ACS Data:
The tested model was applied to the ACS data to predict food insecurity at the county level.

#### Variable Selection:
Our selected predictor for food insecurity is:

FSWROUTY - Worry that food would run out before being able to afform more (during the past year).

## Using Our Code
#### To replicate our analysis or to explore the data:
1. Download the R.Markdown file from this repository.
2. Open the file in R-Studio.
3. Run the script to see our methods and results.
4. Conclusions are commented within the file for easy reference.

## Conclusions
Our recommendations for Wesley Life's expansion are:

[Conclusion 1]

[Conclusion 2]

These conclusions are based on the predictions from our model and highlight the counties where Wesley Life could make the biggest impact.

## Acknowledements
We thank Wesley Life for their collaboration and the course instructor Lendie Follett for her guidance.
