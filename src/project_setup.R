knitr::opts_chunk$set(echo = TRUE)

# ----------------------------- renv ---------------------------------
if (!requireNamespace("remotes"))
  install.packages("remotes")

if (!("renv" %in% rownames(installed.packages())))
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
library(knitr)      # kable() : formats the output as a table
library(glmnet)
library(randomForest)
library(ROSE)

# Enable parallelization
library(doParallel)
registerDoParallel(JOB_CORES)

# Set seed
set.seed(1)

