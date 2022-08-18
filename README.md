# Acute-pancreatitis-PEX-treatment-analysis

Analysis and comparision of Plasma Exchange Therapy vs Vietnam's ministry of health guidelines for the treatment of Acute Pancreatitis in patients. Data cleaning, translating, regression modelling, Linear discriminant analysis and K Nearest Neighnour analysis in R

The dataset consists of only 165 rows / patient records with 195 variables obtained from a hospital in Vietnam. The dataset is characterized with high amount of missing data, high dimensionality, data inaccuracies and recorded in Vietnamese language. 

The entire dataset was converted to English language by translating every feature name. At the same time, I studied the entire course of treatment given to a patient who is diagnosed with acute pancreatitis. This way, I was able to understand all the variables in the dataset and was able to strategize the approach of analysis. 

I performed in depth data analysis, like tabulated the statistical summaries and necessary graphs like histograms, boxplots and scatter plots to study the distribution of variables, cleaned the data for inaccuracies, visualized missing data and missing data patterns with graphs in R, categorised and understood the type of missingness into 3 categories that is MCAR (Missing Completely at Random), MAR (Missing at Random) and MNAR (Missing not at Random) which allowed me to impute the missing data eliminating bias of imputation, performed multivariate imputations of data using MICE, Amelia and miss forest packages in R and compared the performance of these imputations. Developed regression models, linear discriminant analysis models and KNN model on this dataset and observed that the PEX treatment significantly reduced the number of days of hospitalization of patients.
