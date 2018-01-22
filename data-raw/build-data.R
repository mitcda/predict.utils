### =========================================================================
### Filename:     build-data.R
### Created:      2018-01-22
### Updated:      <2018-01-22 22:06:35 david at grover>
### Author:       David Mitchell <david.p.mitchell@homemail.com.au>
### Description:  Build predict.utils data sets
### =========================================================================

######  Section 1 - Load & write data
###  Load data set
load("AvData.RData")
load("FcstData.RData");

###  Write data sets
devtools::use_data(AvData, FcstData, overwrite=TRUE);

### =============================== EOF =====================================
