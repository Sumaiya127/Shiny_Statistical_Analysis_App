# Shiny_Statistical_Analysis_App
An interactive Shiny Web Application that performs Differential and Inferential Statistical Analysis on any uploaded CSV dataset. Includes Descriptive Stats, Box Plots, Hypothesis Testing, Confidence intervals, and Correlation Analysis.


#Shiny Statistical Analysis App

This Shiny app allows users to perform both DIFFERENTIAL and INFERENTIAL statistical analysis on any uploaded CSV dataset. Built using R and Shiny, this interactive web application is ideal for students, analysts, and researchers who want to explore, visualize, and analyze data with ease—no coding required.



## FEATURES

### Differential Statistics
- Upload your own `.csv` dataset via the user interface
- Select variables (2 categorical, 1 numerical)
- Display descriptive statistics:
  - Mean, Median, Mode
- Visualize distributions using BOX PLOTS

### Inferential Statistics
- Choose appropriate hypothesis tests:
  - **T-test** for numerical vs categorical
  - **Chi-squared test** for categorical vs categorical
- Display:
  - P-values
  - Conclusion about null hypothesis
- Compute and show CONFIDENCE INTERVALS
- Perform CORRELATION ANALYSIS between two numerical variables:
  - Correlation coefficient
  - Scatter plot

### Dynamic and Interactive
- Results update instantly based on user inputs
- Clear and responsive layout using Shiny

---

## HOW TO USE THE APP

1. Download or clone this repository.
2. Open `app.R` in **RStudio**.
3. Click **Run App**.
4. Upload a `.csv` file containing your dataset.
5. Use the dropdowns to select variables and run analyses.



## REQUIREMENTS

Make sure you have the following R packages installed:

r
install.packages(c("shiny", "ggplot2", "dplyr", "readr", "tools"))

## AUTHOR
-Developed by Sumaiya Irshad
