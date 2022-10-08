# Financial Literacy and Financial Well-being

## Project Description
This project investigates the effect of financial literacy on a widely used measure of financial well-being, controlling for socioeconomic and financial behavioral factors. 

We analyze survey data from the 2018 National Financial Capability Study (NFCS) by the Financial Industry Regulatory Authority (FINRA). It uses the financial well-being scale developed by the Consumer Financial Protection Bureau (CFPB) for the outcome variable. We measure financial literacy as the number of correct answers to literacy questions in the 2018 NFCS.  

Our empirical analysis utilizes three models to estimate the causal effect of financial literacy on financial well-being: a multiple linear regression model, a two-step model using regularized Bayesian linear regression, and a two-stage model using Bayesian Causal Forests to study treatment effect heterogeneity. 

In all three models, the overall causal effect of financial literacy on financial well-being is estimated to be significantly negative, standing in stark contrast to the existing empirical literature on financial literacy. Based on the results of the last model providing heterogeneous causal effects (i.e., treatment effects for all individuals in our study), we create a posterior summary regression tree with estimates of the subgroup average treatment effects, splitting the respondents into eight subpopulations.

## File Description
- 2018NFCS.csv is the raw survey data we analyzed.
- CFPBscoringworksheet.csv contains the financial well-being scale developed by CFPB.
- cleandata.RData is the clean data.
- FinancialWellbeing2022.R is the R script to clean the raw data
- FinLit_bcf.R is the R script to analyze the data
- stateq.csv contains the state data 
