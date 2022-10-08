# Financial Literacy and Financial Well-being

## Project Description
### Participating Researchers: 
Myeongrok Doh (The University of Texas at Austin)

David Puelz (The University of Texas at Austin)

Robert Puelz (Southern Methodist University Cox School of Business)

### Project Summary
This project investigates the effect of financial literacy on a widely used measure of financial well-being, controlling for socioeconomic and financial behavioral factors. 

We analyze survey data from the 2018 National Financial Capability Study (NFCS) by the Financial Industry Regulatory Authority (FINRA). It uses the financial well-being scale developed by the Consumer Financial Protection Bureau (CFPB) for the outcome variable. We measure financial literacy as the number of correct answers to literacy questions in the 2018 NFCS.  

Our empirical analysis utilizes three models to estimate the causal effect of financial literacy on financial well-being: a multiple linear regression model, a two-step model using regularized Bayesian linear regression, and a two-stage model using Bayesian Causal Forests (BCF) to study treatment effect heterogeneity. The technical details about BCF could be found in the paper: [Hahn, Murray, and Carvalho 2020](https://projecteuclid.org/journals/bayesian-analysis/volume-15/issue-3/Bayesian-Regression-Tree-Models-for-Causal-Inference--Regularization-Confounding/10.1214/19-BA1195.full).

In all three models, the overall causal effect of financial literacy on financial well-being is estimated to be significantly negative, standing in stark contrast to the existing empirical literature on financial literacy. Based on the results of the last model providing heterogeneous causal effects (i.e., treatment effects for all individuals in our study), we create a posterior summary regression tree with estimates of the subgroup average treatment effects, splitting the respondents into eight subpopulations.

## File Description
- 2018NFCS.csv: raw survey data we analyzed
- CFPBscoringworksheet.csv: financial well-being scale developed by CFPB
- cleandata.RData: clean data
- FinancialWellbeing2022.R: R script to clean the raw data
- FinLit_bcf.R: R script to analyze the data
- stateq.csv: state data 
