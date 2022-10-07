# Financial Literacy and Financial Well-being

This project investigates the effect of financial literacy on a widely used measure of financial well-being, controlling for socioeconomic and financial behavioral factors. 

We analyzed survey data from the 2018 National Financial Capability Study (NFCS) by the Financial Industry Regulatory Authority (FINRA). It uses the financial well-being scale developed by the Consumer Financial Protection Bureau (CFPB) for the outcome variable. We measure financial literacy as the number of correct answers to literacy questions in the 2018 NFCS.  

Our empirical analysis utilizes three models to estimate the causal effect of financial literacy on financial well-being: a multiple linear regression model, a two-step model using regularized Bayesian linear regression, and a two-stage model using Bayesian Causal Forests to study treatment effect heterogeneity. Details about the models are discussed in the appendix section (Modeling Details).

In all three models, the overall causal effect of financial literacy on financial well-being is estimated to be significantly negative, standing in stark contrast to the existing empirical literature on financial literacy. Based on the results of the last model providing heterogeneous causal effects (i.e., treatment effects for all individuals in our study), we created a posterior summary regression tree with estimates of the subgroup average treatment effects, splitting the respondents into eight subpopulations as below.



2018NFCS is the survey data we analyzed
