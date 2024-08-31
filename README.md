# Global Monkeypox Data Analysis

## Overview

This project provides a comprehensive analysis of monkeypox cases and deaths across different locations from Jan 2022 to Aug 2024. The dataset, sourced from the [Our World in Data (OWID) project](https://ourworldindata.org/), contains **85,250 rows** and **15 columns**, with each row representing a unique combination of location, date, and corresponding monkeypox cases and deaths. The dataset is complete with no missing values.

The primary aim of this project is to visualize the distribution and trends of monkeypox cases globally, providing insights into how the disease has spread across different regions.

## Key Visualizations

### 1. Choropleth Map

This map visualizes the distribution of **total monkeypox cases per million population** across various countries. The color gradient represents the number of cases per million people, with the following color scheme:

- **Dark Purple**: Fewer cases per million.
- **Yellow**: Higher cases per million.

**Key Observations:**

- **Europe** and parts of **North and South America** show higher concentrations of cases, with lighter shades indicating higher case rates.
- **Africa** and **Asia** exhibit fewer cases relative to their population, represented by darker shades or less intense colors.

This visualization highlights the global disparities in monkeypox case distribution, which may be influenced by factors such as healthcare infrastructure, reporting capabilities, and public health interventions.

### 2. Scatter Plot of New Cases vs. New Deaths

This scatter plot visualizes the relationship between **New Monkeypox Cases** per million population (x-axis) and **New Deaths** per million population (y-axis). The color gradient represents the number of new cases per million, with a scale ranging from purple (fewer cases) to yellow (more cases).

**Key Observations:**

- Most data points are clustered near the origin, suggesting that many regions have relatively low numbers of new cases and new deaths per million population.
- Outliers indicate regions with higher new cases per million but low new deaths per million, with colors varying based on case intensity.

This plot helps understand the correlation between new cases and deaths, showing that while new cases are prevalent, the number of new deaths per million population remains relatively low for most regions.

## Dataset Details

- **Source**: [Our World in Data Monkeypox Dataset](https://ourworldindata.org/mpox)
- **Date Range**: 5 January 2022 to 25 August 2024
- **Rows**: 85,250
- **Columns**: 15
- **Missing Values**: None

## Getting Started

To explore the dataset and visualizations, clone this repository and open the R Markdown file (`mpox-analysis.Rmd`). You can run the code to generate the plots and explore the data.

