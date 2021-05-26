knitr::opts_chunk$set(echo = TRUE)

# ----------------------------- renv ---------------------------------
if (!requireNamespace("remotes"))
  install.packages("remotes")

remotes::install_github("rstudio/renv")

renv::restore()

# ----------------------------- libs ---------------------------------
library(RcppCNPy)
library(e1071)
library(mice)
library(ggplot2)
library(caTools)
library(caret)
library(skimr)
library(pROC)
library(ROCR)
library(xgboost)
library(cvms)       # custom cross-val  
library(groupdata2) # fold()

# Enable parallelization
library(doParallel)
registerDoParallel(8) # 8 cores