# Prediction of Concrete Compressive Strength 
## Project Overview
Concrete is a fundamental building material in modern society, and its quality control is extremely important. Concrete is used in many of our familiar buildings such as bridges, houses, and dams. The compressive strength of concrete is one of the most important indicators to guarantee the safety of such buildings, and various compressive strength standards are established for different types of buildings. Compressive strength is a quantitative measure of how much weight a concrete can withstand and is a basic indicator to measure its reliability as a building material.This project tries to answer the research question: how various concrete formulation components and concrete age effect on compressive strength, by developing several predictive models.

## Predictive Models
We used four different approaches to create the model to predict the compressive strength:

1. Full Linear Model
2. Linear Model (Best subset selection)
3. Polynomial Regression
4. Interaction Model

## Data
We used the Concrete Compressive Strength data set from UC Irvine Machine Learning Respitory. (https://archive.ics.uci.edu/dataset/165/concrete+compressive+strength)

## Evaluation
5-fold cross validation was carried out in order to compare the performance of 3 models fitted in the above sections, and to see which model performs the best in predicting the concrete compressive strength. 

Procedure of the test is the following:
1. Randomly and equally split the samples into 5 groups
2. Take one group as test set and the other 4 sets as training set
3. Fit the model with the training set and find RMSE values for the test set
4. Repeat steps 2 and 3 for 5 times and take the average of the RMSE values 

## Result
It was shown that the polynomial model performs the best among the 3 models. Though it has the negative Cp value, it has the smallest RMSE value, the largest adjusted R-squared value, and the smallest BIC value, making it the best model to predict the concrete compressive strength. 

## Source Code
