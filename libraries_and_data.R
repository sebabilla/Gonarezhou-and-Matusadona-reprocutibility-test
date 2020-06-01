#library list
#Exploratory factor analysis
library(psych); library(GPArotation)
#ordinal logistic regression
library(MASS)
#general (last to make them first choice)
library(readr); library(dplyr); library(tibble); library(gridExtra)

#importing data
raw <- read_csv("Data/raw.csv")
raw #see the data
#str(raw) #type of data
head(raw)
names <- read_csv("Data/names.csv")
as.data.frame(names) #questionnaire topics

#sources
#https://cran.r-project.org/web/packages/psychTools/vignettes/factor.pdf
#https://rstudio-pubs-static.s3.amazonaws.com/363499_73a1c1a94da148b6ad81e6eb8dc1b771.html
