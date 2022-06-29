# Flexible societies excelled in saving lives in the first phase of the COVID-19 pandemic 

## Project Abstract
**Background:** Previous studies have shown that national cultural traits, such as collectivism-individualism and tightness-looseness are associated with COVID-19 infection and mortality rates. However, although East Asian countries have outperformed other countries in containing COVID infections and lowering mortality in the first pandemic waves, no studies to date have examined flexibility-monumentalism, a cultural trait that uniquely distinguishes East Asia from the rest of the world. Moreover, none of the previous studies have explored mechanisms underpinning the association between national culture and COVID-19 mortality.<br/> 
**Aims:** Our study fills in these gaps by examining the association between flexibility-monumentalism and COVID-19 mortality in 37 countries, adjusting for important covariates and by analyzing mask wearing and fear of COVID-19 during the first weeks of the pandemic as plausible mechanisms underpinning this association.<br/> 
**Methods:**  We constructed and analyzed a dataset including 37 countries that have valid information on flexibility vs monumentalism, COVID-19 deaths as of 31 October 2020 (before the start of vaccination campaigns) and relevant covariates including two other dominant national cultural traits and other national characteristics (economic, political, demographic and health). Multivariate linear regression with heteroscedasticity-consistent standard errors was used to assess the independent effect of flexibility vs monumentalism on COVID-19 mortality. Mediation was assessed by examining the indirect effects of flexibility through mask wearing and fear of COVID-19 and determining the statistical significance through bootstrapping. Graphical and delete-one analysis was used to assess the robustness of the results.<br/> 
**Results:** We found that flexibility was associated with a significant reduction in COVID-19 mortality as of 31 October 2020, independent of level of democracy, per capita GDP, urbanization, population density, supply of hospital beds and median age of the population. This association with mortality is stronger and more robust than for is also two other prominent national cultural traits (individualism-collectivism and tightness-looseness). We also found tentative evidence that the effect of flexibility on COVID-19 mortality may be partially mediated through mask wearing in the first weeks of the pandemic


## Purpose of the Repository
The repository **FlexCov** was created to provide all data and code to reproduce findings reported in:
> XXX (2022). *Flexible societies excelled in saving lives in the first phase of the COVID-19 pandemic.*

## Project Structure
**`/data`**
- contains all raw data files necessary for the analysis

**`/r-scripts`**
- contains all r-scripts that clean YouGov and OWID data 

**`/do-files`**
- contains all do-files analysis files necessary to perform data cleaning and analysis reported

**`/data_out`**
- contains the analysis data set , to be then used in `/do-files/FlexCoV_analyses.do.`





