# 🚔 Exploratory Data Analysis – North Carolina State Patrol Traffic Stops  

## 📌 Project Overview  
This project conducts an **Exploratory Data Analysis (EDA)** of traffic stop data from the **North Carolina State Patrol**, sourced from the [Stanford Open Policing Project](https://openpolicing.stanford.edu/).  
The aim is to uncover demographic patterns, analyze search likelihood, and identify potential disparities in law enforcement practices.  

---

## 📂 Repository Contents  
- **`Jayesh-Final Project.pdf`** – Full EDA report including:  
  - Data preparation & cleaning steps  
  - Frequency tables and cross-tabulations  
  - Visualizations (bar charts, scatter plots, jitter plots)  
  - Statistical tests (t-test, chi-square)  
  - Logistic regression analysis  
  - Conclusions & key findings  

- **`Jayesh-Final Project.R`** – R source code used to perform the data cleaning, visualizations, statistical testing, and logistic regression modeling.  

---

## 📊 Key Insights  
- **Search Rates by Race:**  
  - Hispanic: 4.6%  
  - Black: 4.5%  
  - White: 2.2%  
  - Asian/Pacific Islander: 1.7%  

- **Demographics Influencing Searches:**  
  - Males, Black, and Hispanic individuals are more likely to be searched.  
  - Older individuals and those with “unknown” race are less likely to be searched.  

- **Search & Arrest Link:**  
  - Moderate positive correlation (0.50) – searches often result in arrests.  

- **Warnings vs. Citations:**  
  - Strong negative correlation (-0.89) – rarely issued together.  

---

## 🛠 Methodology  

**Data Cleaning:**  
- Removed irrelevant columns and unwanted race categories.  
- Converted date and categorical variables to correct formats.  
- Created derived features (e.g., `dayofweek`).  
- Converted binary columns to logical types.  

**Analysis Techniques:**  
- Frequency tables and cross-tabulations.  
- Histograms, bar charts, scatter plots, jitter plots.  
- Hypothesis testing (t-test, chi-square).  
- Logistic regression for search probability prediction.  

---

## 📦 Technologies Used  
- **Languages:** R  
- **Libraries:**  
  - `ggplot2`, `dplyr`, `stats` (R)  
- **Visualization:** Bar charts, scatter plots, jitter plots, correlation matrices  

---

## 📜 License  
This project is for **academic and research purposes only**.  
