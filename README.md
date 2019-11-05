# R Projects

## A "Shiny Application" for the simulation of a simplified economy

### Project description
Following the book "Economics with Heterogeneous Interacting Agents: A Practical Guide to Agent-Based Modeling " (Caiani et al., 2016), this project contributed to my deeper understanding of the subject. Using the "Shiny" package from RStudio, I developed an application (R programming language), which can simulate a simplified economy. The application's interface gave me a structured way to adjust model parameters and at the same time observe their contribution in the provided graph.

## Backtesting a Neural Network and Technical Analysis trading strategy in R

### Project description
This project serves a double purpose: 1) To evaluate trading strategies based on Neural Network and Technical Analysis (TA) indicators, 2) To implement a framework that can be reused for future research on different trading strategies and evaluate their performance. I investigated a strategy where the Neural Network acts as a classifier, producing buying and selling signals, which are afterwards grouped with the ones of the TA indicators. This combination of signals eventually determined the optimal action that should be performed at any point in time. 


## Required libraries for this repository

* Simplified economy simulation
  * shiny
  * ggplot2
  
  
* Backtesting a Technical Analysis-Neural Network trading Strategy
  * quantstrat
  * blotter
  * xts
  * quantmod
  * keras
  * caret
  * TTR
  * reticulate
