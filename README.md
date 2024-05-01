# RStudioProject
This is a R package contains the following functions:
- logLikBernoulli = function(data) : Calculates the parameter p that maximizes the log-likelihood
- survCurv = function(status,time) : Plot the survival curve using only tidyverse
- unscale = function(x) : Unscale vectors that has been passed through scale()
- pcApprox = function(x, npc) : Calculate the approximation to a data x based on PCs
- standardizeNames = function(data) : Standardize column names to camelCase
- minimumN = function(x1,x2) : Calculate minimum sample size for T-Tests,and smallest sample size can be returned was set to 2
- downloadRedcapReport = function(redcapTokenName,redcapUrl,redcapReportId) : Download a report from REDCap

# Installation
Please use library(devtools):
install_github("mu-xiaofan/RStudioProject/Functions")
