# Identifying Social Distancing Strategies to Control SARS-CoV-2 Using an Agent Based Model
_**Disclaimer**: This repository is under active development and may be incomplete_

## Introduction
With a few exceptions, the primary governmental response to the SARS-CoV-2 pandemic has been to implement strict social distancing restrictions. These restrictions are aimed at at reducing COVID-19 incidence (or keeping incidence low) until the public health and healthcare infrastructure can be enhanced by wide-spread laboratory testing, contact tracing capacity, and availability of hospital beds and supplies. While social distancing restrictions have largely been effective at reducing SARS-CoV-2 transmission, these gains against the virus have generally come at a staggering economic cost.

The purpose of this modeling project is to provide data to policy makers to guide the re-opening of various economic and social activities. The hope is to identify a minimum set of interventions that can keep COVID-19 incidence to manageable levels. The model is built using the [GAMA platform](https://gama-platform.github.io/).
```
Taillandier P, Gaudou B, et al. 
Building, composing and experimenting complex spatial models with the GAMA platform. 
Geoinformatica, (2019), 23 (2), pp. 299-322
```

## Contact
This repository was developed by Mike Jackson at the [Kaiser Permanente Washington Health Research Institute](https://www.kpwashingtonresearch.org/). For more infomation, please e-mail michael.l.jackson@kp.org.

## Modeling approach
This project is based around an agent-based model (ABM) from a large urban population. The model is based approximately on the population of Seattle, Washington. Using data mainly from the 2018 American Community Survey (ACS) for King County, Washington, the model constructs a pseudo-population of roughly 500,000 people. The pseudo-population is built to reflect the age, sex, household composition, workplace, and school structure of King County.

The model divides the population into different classes with defined characteristics, including Toddlers (age <6 years, do not go to school or daycare); Children (ages 0-17 who go to either school or daycare during the week); Adults (ages 18-74, some of whom may work at workplaces, schools, nursing homes, or group quarters); and Seniors (ages 75-99, who do not go to workplaces). These classes all have a home that they go to in the evening, and have the option of going to community gathering places. Two additional classes are nursing home residents and group quarter residents, who do not move locations.

The model divides each day into three time periods - morning, afternoon, and evening. During the week (Monday - Friday) in the mornings, children go to schools and some adults go to work. In the afternoon, all persons (except nursing home and group quarter residents) may go to community gathering spaces. In the evening, all persons go to their homes. On the weekends, persons may go to community gathering spaces in the afternoon.

## Organization
### Data for creating pseudo-population
The Compiled_Data directory contains an Excel workbook (.xlsx) with compiled data from the 2018 ACS, which is output as summary .csv files.

### R code
The R program `Create_Pseudopop.R` takes the compiled ACS data and creates input files for the ABM. This includes dividing the population into different classes and assigning simulated persons to households, workplaces, schools, and group quarters. Other R programs import GAMA output for analysis.

### GAMA code
The central GAMA program is `00_Base_Model.gaml`, which defines the agents and global environment. Other GAMA programs use (or modify) this base program to (for example) estimate the basic reproductive number (`01_Estimate_R0.gaml`) or run the model matching the actual social distancing interventions in King County (`03_Compare_To_Observed.gaml`). 

Note that, in order to run the models, after setting the GAMA workspace to ../Programming/GAMA_workspace, it may be necessary within GAMA to import the project SEIR_ABM.


