# 📊 GLM in R — Car Price Prediction & Heart Disease Classification

#  🇺🇸 English

This repository contains two applied Generalized Linear Models (GLM) in R:

1. Car Price Prediction — Gamma regression with log link to model car prices and quantify the impact of technical and brand features. 
2. Heart Disease Classification — Logistic regression to identify key risk factors and estimate the probability of heart disease. 


🔍 What’s inside
- Report (PDF): full methodology, EDA, modeling, diagnostics, and results. 
- GLM #1 — Cars: USA market context, feature engineering (brand extraction, collinearity handling), model selection (AIC stepwise), diagnostics, CIs and multiplicative effects. 
- GLM #2 — Heart: binary outcome (0/1), logistic regression with stepwise selection and discussion of significant predictors and dataset limits. 

🧠 Methods (brief)

- Car Prices: Response ~ price. Distribution ≈ Gamma; log link used after exploring raw/log price distributions. Two candidates: (i) main effects; (ii) main + quadratic terms for numeric covariates. Best model chosen via AIC, pseudo-R², and deviance. 
- Heart Disease: Logistic regression on Kaggle “Heart Failure Prediction”, stepwise AIC to select key factors (e.g., sex, chest-pain types, exercise-induced angina, and cholesterol²). 

📈 Highlights (from the report)

- Car Prices: Rear engine ~ ≈2× price multiplier; BMW positive brand effect; several body types reduce price vs. convertible baseline; many numeric covariates show quadratic patterns; pseudo-R² ≈ 0.966 for the selected model; adequacy confirmed by deviance tests and residual diagnostics. 
- Heart Disease: Significant risk factors include sex, exercise-induced angina, chest-pain categories, and cholesterol (squared); dataset skewed toward older ages (≥28). 

🗂️ Data

- Car Price: Kaggle “Car Price Prediction” (cleaned: removed IDs, normalized car_company, addressed collinearity like engine type vs. cylinders). 
- Heart Failure Prediction: Kaggle dataset, 918 subjects, 11 covariates + binary target; no missing values reported.

▶️ How to run (R)

```bash
# install.packages(c("readr","dplyr","ggplot2","ggpubr","MASS"))
library(readr); library(dplyr); library(ggplot2); library(ggpubr); library(MASS)

# === Car Price (example skeleton) ===
car <- read_csv("data/CarPrice_Assignment.csv")         # path as in your project
# ... cleaning: extract car_company, fix typos, drop IDs, factorize chars ...
# glm Gamma with log link (illustrative formula)
m_full <- glm(price ~ ., data = car, family = Gamma(link = "log"))
m_step <- stepAIC(m_full, direction = "both")           # model selection

# === Heart Disease (example skeleton) ===
heart <- read_csv("data/heart.csv")
m_logit <- glm(HeartDisease ~ ., data = heart, family = binomial())
m_logit_step <- stepAIC(m_logit, direction = "both")
```
⚠️ Notes & limitations

- Car dataset lacks EVs and luxury/sport brands, limiting generalization. 
- Heart dataset excludes <28 y/o; class balance leans male; mortality not modeled.

#  🇮🇹 Italiano

Questo repository include due applicazioni di GLM in R:
- Predizione Prezzi Auto — Regressione Gamma con link log per modellare il prezzo e valutare l’effetto di caratteristiche tecniche e del marchio. 
- Classificazione Cardiopatia — Regressione logistica per individuare i fattori di rischio principali e stimare la probabilità di cardiopatia. 

🔍 Contenuti
- Report (PDF): EDA, metodologia, selezione modelli (AIC), diagnostica e risultati. 
- Auto: estrazione car_company, gestione collinearità, modelli con/ senza termini quadratici, confronto con AIC/pseudo-R²/devianza e intervalli di confidenza. 
- Cuore: regressione logistica con selezione stepwise e discussione dei predittori significativi e dei limiti del dataset. 

📈 Evidenze
- Auto: motore posteriore ≈×2 sul prezzo; BMW effetto positivo; alcune carrozzerie riducono il prezzo; pattern quadratici nelle variabili numeriche; pseudo-R² ≈ 0,966; test di devianza e residui a supporto dell’adeguatezza. 
- Cuore: predittori rilevanti: sesso, angina da sforzo, tipi di dolore toracico, colesterolo²; campione senza età <28. 

▶️ Esecuzione (R)
Vedi gli snippet sopra; i nomi delle variabili seguono quelli del progetto e del PDF.
