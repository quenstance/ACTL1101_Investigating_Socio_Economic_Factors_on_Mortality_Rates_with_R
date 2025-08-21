# Socio-economic Disparities in Australian Mortality

### Overview

This project, originally developed for the **ACTL1101 Introduction to Actuarial Studies** course at UNSW, investigates the relationship between various socio-economic factors and mortality rates across Australia. The analysis focuses on both Indigenous and Non-Indigenous populations.

Using the R programming language, we sourced and cleaned data from the Australian Bureau of Statistics (ABS) and the Australian Institute of Health and Welfare (AIHW). The analysis employed a variety of techniques, including **statistical analysis**, **correlation**, and **regression**. Specifically, we used **summary statistics**, **scatter plots**, **simple regression lines**, **heatmaps**, **box plots**, and **bar plots** to investigate our findings and communicate them effectively through data visualization.

Our **Key Conclusions** are:
* A positive relationship exists between a higher proportion of the Aboriginal and Torres Strait Islander (ATSI) population in an area and increased mortality rates.
* Rural and remote areas generally had higher mortality rates, likely linked to lower socio-economic and healthcare standards.
* An inverse relationship was found between income per capita and the Standardised Death Rate (SDR); as income increases, the death rate decreases.
* A negative correlation was identified between education levels and mortality rates, indicating that as the percentage of educated persons increases, the standardized mortality rate decreases.
* A strong positive correlation exists between offender rates and mortality rates in different states.

For more details on the project, please visit the project website: https://quenstance.pages.dev/projects/social-economic_disparities_in_australian_mortality

### How to Use

To run the analysis, you will need **R** and the required libraries.

* The data used in this project is located in the `data` folder.
* Install the required R packages:
    ```R
    install.packages(c("readxl", "data.table", "ggplot2", "grid", "gridExtra", "readr", "moments"))
    ```
* Run the R script:
    ```R
    source("ACTL1101 Project.R")
    ```

### Limitations

Please note that this is based on project work from 2017 and is not an up-to-date analysis. The findings are derived from cleaned data, which resulted in the exclusion of some regions due to missing information. Notably, data for all regions within the Australian Capital Territory (ACT) and other territories were eliminated. The data for ATSI statistics was also limited due to the reluctance of some individuals to be identified as such, which may constrain the accuracy of findings.

---

### Authors

* **Quenstance Lau**
    * **Web Portfolio:** https://quenstance.pages.dev/
    * **LinkedIn:** https://www.linkedin.com/in/quenstance/
    * **GitHub:** https://github.com/quenstance
* **Kevin Chen**
* **Benny Cordeschi**
* **Brendan Navaneethan**
* **Alice Yang**
