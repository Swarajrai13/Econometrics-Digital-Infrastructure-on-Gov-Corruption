# Econometrics-Digital-Infrastructure-on-Gov-Corruption
This repository contains the code, data, and supporting documentation for an empirical political economy analysis examining how digital infrastructure—proxied by mobile subscriptions—influences corruption globally, leveraging panel regression methods and data from the World Bank (WGI, CPIA, WBL).

Using panel regression models and data from 176 countries between 2011 and 2021, the study investigates how mobile technology interacts with demographic, institutional, and infrastructural mechanisms to influence corruption outcomes.

**📁 Contents**

(1) CON326 Final Project.pdf — Full research paper

(2) ECON326 FP Code.R — Final cleaned and annotated regression code

(3) ECON326 Project.R — Initial development and exploratory code

***Dataset Note: Corruption Data Distinction***

This project uses two separate corruption measures, each with unique properties and sample coverage:

**(1) CPIA Corruption Score (cpi_score):**

Source: World Bank CPIA

Range: 1 (worst) to 6 (best)

Coverage: Only low- and middle-income countries

Institutional focus, based on expert assessments tied to aid allocation.

**(2) WGI Control of Corruption (corruption_control):**

Source: Worldwide Governance Indicators

Range: −2.5 (worst) to +2.5 (best)

Coverage: All countries, including high-income

Perception-based, drawn from survey and expert data.

Both are used to cross-validate the findings under varying assumptions and country samples. The WGI models are especially important due to their broader global coverage and robustness under region-year fixed effects.

➡️ [Download merged panel datasets (Google Drive)]  (1): (https://drive.google.com/file/d/19dKab7HSl5n86XoY4AW2E2lyxC3SDEeD/view?usp=drive_link)) 
(2): https://drive.google.com/file/d/13wsF6Lln60yXmCFjeKI6wDvXe7VfPlaw/view?usp=drive_link



**🧪 Statistical Methods**
Pooled OLS and two-way fixed effects models

Region-year fixed effects

Mechanism interaction terms (urbanization, WBL Index, working-age population)

Robustness checks using both CPIA and WGI corruption metrics

**📊 Data Sources**
World Bank Indicators

Worldwide Governance Indicators (WGI)

Women, Business and the Law (WBL)

UN M49 Classification

