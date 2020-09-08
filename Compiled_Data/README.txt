/* This readme created 3 April 2020, by Mike Jackson */

This readme describes the contents of /../nCoV_ABM/Compiled_Data.

Data in this folder are compiled from the raw data in /../nCoV_ABM/Source_Data.

(A) Google folder has mobility data from Google, in R format, downloaded from: https://www.gstatic.com/covid19/mobility/Global_Mobility_Report.csv?cachebust=724703ce99ccb906

(1) Census.xlsx is an extract of relevant tables from the downloaded census data from the American Community Survey, restricted to King County, Washington. It also includes spreadsheeds that compile the survey data to save as .csv files for importing into R. All other .csv files in this directory come from Census.xlsx.

(2) Export_AgeSex.csv. Counts of King County population, stratified by age group and sex.

(3) Export_Family.csv. Counts of family types by parent (Married, male only, female only), presence of children, and child age groups (<6 years, >=6 years, or both).

(4) Export_NonFamily.csv. Counts of household types that are not family units, stratified by presence of seniors.

(5) Export_Exact.csv. Counts of persons living alone, stratified by sex and by senior/non-senior.

(6) Export_Sizes.csv. Counts of households, stratified by type (family vs. non-family) and by number of persons (2 through 7+).

(7) Export_Workforce.csv. Percent of the population that is part of the workforce, by age and sex.

(8) King-County-hospitalizations-yyyy-mm-dd.csv. Daily COVID-19 hospital admissions for King County, expected to be complete through the date listed. Downloaded from https://www.kingcounty.gov/depts/health/covid-19/data/daily-summary.aspx

(9) mobility-yyyy-mm-dd.csv. Compiled mobility data to enter into GAMA, from Compile_Google_Mobility_Data.R.
