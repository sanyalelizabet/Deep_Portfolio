# Deep Portfolio

A research repository on deep-learning-based portfolio construction for cross-sectional equity allocation.

## Overview

This project studies whether neural-network architectures can improve portfolio formation relative to simpler allocation rules by directly learning portfolio weights from panel features and asset returns. The repository is structured as an applied quantitative research workflow: data preparation, model construction, rolling backtesting, hyperparameter tuning, and ex-post performance attribution.

The focus is not on prediction in isolation, but on portfolio formation under realistic constraints. Models are trained to produce portfolio weights and are evaluated on risk-adjusted portfolio outcomes, turnover, and factor-adjusted performance.

## Research objective

The central idea of the project is to map firm-level or cross-sectional features into portfolio weights using deep learning, while preserving the logic that matters in practice for portfolio management:

- portfolio-level objective rather than point forecast accuracy
- explicit control of portfolio weights
- walk-forward evaluation instead of static train/test splits
- comparison across alternative network architectures
- performance measurement beyond raw returns

In that sense, the repository sits between academic asset-pricing research and systematic portfolio construction.

## Methodology

The workflow can be summarized in five stages.

### 1. Data preparation

Raw panel data are transformed into a wide feature matrix and a return matrix suitable for cross-sectional modeling. The preparation pipeline includes:

- reshaping security-level data into model-ready wide format
- winsorization of returns
- row-wise imputation of missing values within feature groups
- handling of incomplete return histories where needed
- creation of time-series datasets for sequential models

This design allows the same dataset to be used across dense, recurrent, and convolutional architectures.

### 2. Model construction

The repository contains several portfolio-model specifications:

- **Simple feed-forward model**
- **Complex hybrid model** combining linear and non-linear branches
- **LSTM model** for sequential structure
- **1D convolutional model** for time-dependent feature extraction

The complex architecture is particularly motivated by the idea that some portfolio signals may be approximately linear while others are inherently non-linear. The model therefore combines both channels before producing portfolio weights.

### 3. Portfolio objective

The models are trained with custom loss functions linked to portfolio outcomes rather than standard classification or regression losses. In particular, the framework emphasizes a **Sharpe-ratio-based objective**, with variants that account for missing values and transaction-cost effects.


### 4. Portfolio constraints

Predicted outputs are transformed into portfolio weights under practical constraints. The framework is designed for constrained long-short allocation, with configurable exposure limits for long and short books.

This is important because unconstrained machine-learning outputs are often difficult to interpret economically. By imposing portfolio normalization and leverage rules, the model output becomes directly usable as a portfolio allocation.

### 5. Backtesting and evaluation

The repository uses a walk-forward backtesting framework with either:

- **expanding windows**, or
- **fixed windows**

This avoids in-sample bias from static splits and better reflects how a live strategy would be trained and updated through time.

Performance is then evaluated using both portfolio statistics and factor-adjusted attribution.

## Performance evaluation

The project evaluates strategies using metrics that are standard in quantitative portfolio research:

- average portfolio return
- return volatility
- Sharpe ratio
- turnover
- transaction-cost-adjusted Sharpe ratio
- maximum and minimum portfolio weights
- distribution of long and short exposures
- CAPM alpha
- Fama-French 3-factor alpha
- Newey-West adjusted inference for alpha statistics

This makes the output more informative than simply reporting cumulative return curves.

## Repository structure

```text
Deep_Portfolio/
├── data_preparation.R                    # data cleaning, reshaping, winsorization, imputation
├── model_construction.R                 # dense, hybrid, LSTM, and convolutional portfolio models
├── BacktestClass.R                      # rolling backtest engine and custom portfolio losses
├── performance_metrics.R                # turnover, Sharpe, alpha, and summary statistics
├── Bayesian_Optimisation.Rmd            # hyperparameter tuning experiments
├── backtest.R                           # backtest execution scripts
├── custom_nn_keras.R                    # custom neural-network components
├── Contrast_Weights_Two_Portfolio_Shiny_App.R
├── Difference_EW_predefined_Stocks_SR_Expand.R
└── *.Rmd / *.RData                      # research notebooks and saved datasets
