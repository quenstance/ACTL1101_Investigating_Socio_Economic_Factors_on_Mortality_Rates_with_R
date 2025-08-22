# Technical Details: Socio-economic Disparities in Australian Mortality

---

### Project Overview

This project was a comprehensive data analysis undertaken to investigate the relationship between various socio-economic factors and mortality rates across Australia. The analysis focuses on both Indigenous and Non-Indigenous populations. The work was conducted using **RStudio** and relied on quantitative data from the **Australian Bureau of Statistics (ABS)** and the **Australian Institute of Health and Welfare (AIHW)**.

---

### Methodology

Our methodology was structured to explore different socio-economic factors in five distinct sections:

1.  **Section 1: Mortality Rate and Indigenous Population:** We analyzed trends in Australia's mortality rate and its distribution across individual Local Government Areas (LGAs). Our analysis supported a positive relationship between increased mortality rates and the proportion of the Aboriginal population in each LGA.
2.  **Section 2: Mortality and Remoteness:** This section investigated the relationship between mortality rates and the level of remoteness in a specific area. We found that rural and remote areas generally had increased mortality rates, possibly linked to lower socio-economic and healthcare standards.
3.  **Section 3: Mortality and Income:** We examined the relationship between income per capita and the Standardised Death Rate (SDR). A clear inverse relationship was identified, showing that as income increases, the death rate decreases.
4.  **Section 4: Mortality and Education:** We explored the relationship between education levels and mortality rates, finding a negative correlation where higher percentages of educated persons correlated with lower standardized mortality rates.
5.  **Section 5: Mortality and Crime Rates:** This section focused on the relationship between offender rates and mortality rates, identifying a strong positive correlation between the two factors across different states.

The analytical techniques used throughout the project included:
* Statistical analysis
* Correlation and regression analysis
* Data visualization using **heatmaps**, **box plots**, **scatter plots**, **simple regression lines**, and **bar plots**

### Data

Data was sourced from the ABS and AIHW. 

Key preprocessing steps were performed to ensure data integrity and prepare the datasets for analysis.
* **Missing Value Removal:** Rows containing missing or null values (indicated by '-') were removed from the raw data.
* **Data Filtering:** Datasets were filtered to specific years (e.g., 2011 for ATSI population data) to ensure consistency.
* **Data Merging:** Various data frames were merged based on common identifiers (e.g., LGA codes) to create a comprehensive dataset for analysis.

### Limitations

* **Data Exclusions:** Due to the data cleaning and merging process, some regions were excluded. Most notably, data for all regions within the Australian Capital Territory (ACT) and other territories were eliminated from the analysis.
* **ATSI Data Limitations:** The analysis of Aboriginal and Torres Strait Islander (ATSI) statistics was constrained by data limitations, primarily due to the reluctance of some individuals to be identified as ATSI, which may impact the accuracy of related findings.
* **Project Date:** This project is based on 2017 data and analysis and does not represent an up-to-date assessment of the current situation.