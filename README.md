# Exploratory Data Analysis (EDA) of the AHIES Dataset

## **Overview**
This project conducts an exploratory data analysis (EDA) on the **Annual Household Income and Expenditure Survey dataset**. The dataset provides panel data at the regional levels on expenditure, income, and living conditions of households and individuals in Ghana. The goal of this project is to uncover insights into wage distribution, education levels, employment patterns, and regional disparities.

The analysis is performed in  `**R**`.

---

## **Table of Contents**
1. [Project Overview](#overview)
2. [Dataset Description](#dataset-description)
3. [Project Structure](#project-structure)
4. [Installation and Usage](#installation-and-usage)
5. [Contributing](#contributing)

---

## **Dataset Description**
**Source**: [AHIES 2023](https://www.statsghana.gov.gh/) (Annual Household Expenditure and Income Survey)
The dataset used in this analysis is a 1% sample of the main AHIES dataset. The 1% contains **3,564 observations** and **608 variables**. For this analysis, the following key variables were selected:

| Variable Name         | Description                                      |
|-----------------------|--------------------------------------------------|
| `region`              | Region of the respondent                         |
| `s1aq1`               | Sex of the respondent                            |
| `s1aq4y`              | Age of the respondent                            |
| `urbrur`              | Locality type (Urban/Rural)                      |
| `s1aq5`               | Marital status                                   |
| `s2aq3`               | Highest level of education                       |
| `s2aq11a29`           | Lump sum (education spending)                    |
| `s4aq2`               | Total hours worked in a week (primary job)       |
| `s4aq40a1`            | Main occupation (primary job)                    |
| `s4aq55a`             | Wage of primary job                              |
| `s4gq13c`             | Is it minimum wage?                              |
| `hhid`                | Household ID                                     |
| `s3aq23hhid`          | Household member ID                              |

---

## **Project Structure**
The project is organized as follows:
```AHIES-EDA/
├── data/                   # Contains the dataset
│   └── 10%AHIES.csv        # Sample dataset
├── scripts/                # Contains R executables
|    ├── main.R             # main R file
│   └── theme.R             # GSS theme
├── outputs/                # Output files (plots and results)
│   ├── plots/              # Visualizations
│   └── results/            # Aggregated results and tables
├── README.md               # Project documentation
```

---

## **Installation and Usage**
To run this project locally, follow these steps:

1. **Clone the repository:**
   ```bash
   git clone https://github.com/kcnewman/AHIES-EDA.git
   cd AHIES-EDA

2. Run the `main.R` script from start to finish. The script will:

    - Clean and preprocess the data.
    - Perform exploratory data analysis.
    - Generate visualizations and save them in the `outputs/plots/` directory.
    - Save cleaned and aggregated data in the `outputs/results/` directory.

---
## **Contributing**
Contributions are welcome! If you'd like to contribute to this project, please follow these steps:

1. Fork the repository
2. Create a new branch.
3. Commit and push your changes to the branch.
4. Open a pull request.
