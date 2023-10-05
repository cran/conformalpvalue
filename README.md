## Installation

To install the package, run:

```R 
devtools::install_github("ct364/conformalpvalue")
```


To use the function "naive_bayes_with_conformal_pvalues", use
```
# Train data with target variable "y"
# Calibration data with target variable "y"
# Test data without target variable "y"

result <- conformal_pvalues(train_data, calib_data, test_data, "y",method="naiveBayes")

```


This returns a matrix of conformal p-values.
