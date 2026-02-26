# Econometric Analysis of US Health Expenses

**Date:** 2025-06-02

This repository contains an econometric analysis focused on predicting the medical treatment costs of individuals covered by health insurance in the United States. The project uses R to clean data, perform exploratory data analysis, and construct robust linear regression models to understand how demographic and health characteristics impact medical charges. 

## Data Source
The dataset used in this analysis originates from the book *Machine Learning with R* by Brett Lantz. It is publicly available on Kaggle (https://www.kaggle.com/datasets/mirichoi0218/insurance).

## Technologies
* **Language:** R 

## Key Analyses
The analysis is structured into several core sections:

* **Data Preprocessing & EDA:** Generating descriptive statistics, tracking value distributions and analyzing correlation matrices to identify initial relationships between charges and explanatory variables. Categorical variables were transformed into dummy variables.
* **Feature Engineering & Selection:** Creating an `obesity` binary variable (for BMI > 30) to capture a critical nonlinear relationship with medical costs. Variables were systematically selected using Hellwig's capacity method.
* **Structural Break Detection:** Using the Chow test to confirm structural changes in the data. This justified splitting the dataset into two separate models: one for smokers and one for non-smokers.
* **Model Diagnostics:** Conducting a rigorous suite of econometric tests on the divided models. This includes the Ramsey RESET test for correct functional specification (leading to the use of orthogonal polynomial age transformations for non-smokers), VIF for multicollinearity, the Durbin-Watson test for autocorrelation, and the Breusch-Pagan test for heteroskedasticity.
* **Robust Inference:** Addressing the lack of residual normality (confirmed via the Shapiro-Wilk test) by implementing Bootstrap confidence intervals to ensure valid parameter significance testing.
* **Prediction & Error Evaluation:** Splitting the data into an 80:20 train-test split to evaluate predictive performance. Model accuracy was assessed using metrics such as R-squared, MAE, MAPE, and RMSE. The combined model achieved a highly accurate test R-squared of 0.849.

## Project Structure
* `econometric_analysis.R` - The main R script containing all data preprocessing, econometric testing, bootstrap resampling, and visualization logic.
* `reports/Regresja-KosztyLeczenia.pdf` - The final project report (in Polish) detailing the statistical methodology, diagnostic plots, step-by-step test results, and final conclusions.
