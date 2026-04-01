# Marketing Mix Modeling & Time Series Analysis

## Project Overview
This project focuses on analyzing 207 weeks of sales, pricing, and advertising data for a premium brand (Ixmør). The goal is to quantify the effectiveness of marketing investments and understand the long-term dynamic interactions between business variables.

## Technical Workflow
* **Data Preparation:** Log-log transformations to estimate constant elasticities.
* **Unit Root Testing:** Applied Augmented Dickey-Fuller (ADF) tests to check for stationarity.
* **VAR Modeling:** Developed Vector Autoregression models to capture endogenous relationships between variables.
* **Granger Causality:** Tested directional influence between advertising spend and market share.
* **Impulse Response Functions (IRF):** Simulated how sales react over time to a one-standard-deviation shock in advertising.

## Key Insights
* Estimated the price elasticity and advertising elasticity for the brand.
* Evaluated the impact of a 176% price premium on long-term market share.

## 🔒 Data Privacy Note
The dataset used in this analysis is proprietary academic data from the University of Groningen. In compliance with GDPR and institutional policies, the raw CSV files are not included in this repository. This script is intended to demonstrate the analytical methodology and R programming proficiency.

## Tech Stack
* **Language:** R
* **Main Libraries:** `vars`, `tseries`, `stats`
