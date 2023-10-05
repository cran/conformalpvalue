#' Conformal P-values Calculation
#'
#' This function calculates conformal p-values based of binary class labels for test data.
#'
#' @param method A character string specifying the classification method to use.
#'               Options are 'naiveBayes', 'svm', and 'glm'.
#'
#' This function trains a Naive Bayes classifier, computes non-conformity scores on the calibration data and test data, and calculates conformal p-values of both classes "0" and "1" using the conformal prediction for a binary classification problem.
#'
#' @param train_data A data frame containing the training data with the target variable.
#' @param calib_data A data frame containing the calibration data with the target variable.
#' @param test_data A data frame containing the test data.
#' @param target_col The name of the target variable column.
#' @return A matrix containing p-values for each test case and class.
#' @examples
#' \donttest{
#' # Create dummy train_data, calib_data, and test_data
#' train_data <- data.frame(
#'   x1 = as.numeric(rnorm(50, 1, 2)),
#'   x2 = as.numeric(rnorm(50, 2.5, 3)),
#'   target = as.factor(rbinom(50, 1, 0.5))
#' )
#' calib_data <- data.frame(
#'   x1 = as.numeric(rnorm(50, 1, 2)),
#'   x2 = as.numeric(rnorm(50, 2.5, 3)),
#'   target = as.factor(rbinom(50, 1, 0.5))
#' )
#' test_data <- data.frame(
#'   x1 = as.numeric(rnorm(50, 1, 2)),
#'   x2 = as.numeric(rnorm(50, 2.5, 3))
#' )
#' p_values <- conformal_pvalues(train_data, calib_data, test_data, target="target", method="svm")
#'}
#' @importFrom e1071 naiveBayes
#' @importFrom stats as.formula predict runif
#' @importFrom stats binomial glm
#' @export

conformal_pvalues <- function(train_data, calib_data, test_data, target_col,method) {
  # Train the Naive Bayes model
  formula <- as.formula(paste(target_col, "~ ."))
  if (method == 'naiveBayes') {
    model <- e1071::naiveBayes(formula, data = train_data)
    calib_pred <- data.frame(1 - predict(model, calib_data[, -which(names(calib_data) == target_col)], type = "raw"))
    test_scores <- data.frame(1 - predict(model, test_data, type = "raw"))
  } else if (method == 'svm') {
    model <- e1071::svm(formula, data = train_data,probability=TRUE)
    calib_data_filter <- calib_data[, -which(names(calib_data) == target_col), drop = FALSE]
    calib_predict <- predict(model, newdata = calib_data_filter, probability = TRUE)
    calib_pred<-data.frame(attr(calib_predict, "probabilities"))
    test_pred <- predict(model, test_data, probability = TRUE)
    test_scores<-data.frame(attr(test_pred, "probabilities"))


  } else if (method == 'glm') {
    model <- glm(formula, data = train_data, family = binomial)
    test_pred<- data.frame(predict(model, newdata=test_data, type="response"))
    test_scores<- data.frame(test_pred,1-test_pred)
    calib_predict<- data.frame(predict(model, newdata=calib_data, type="response"))
    calib_pred<-data.frame(calib_predict,1-calib_predict)
  } else {
    stop("Invalid method specified")
  }
  # Extract the actual labels from the calibration data
  actuals <- calib_data[[target_col]]

  # Get non-conformity scores on calibration data corresponding to the actual class of calibration data.
  calib_scores <- list()
  for(i in 1:ncol(calib_pred)) {
    class_members <- which(actuals == i - 1)
    calib_scores[[i]] <- as.numeric(calib_pred[class_members, i])
  }

  # Calculate conformal p-values for both classes "0" and "1" of the test data
  nr_testcases <- nrow(test_scores)
  nr_labels <- ncol(test_scores)
  pValues <- matrix(0, nr_testcases, nr_labels)

  for(k in 1:nr_testcases) {
    for(j in 1:nr_labels) {
      alpha1 <- as.numeric(test_scores[k, j])
      calib_scores_nonconform <- as.vector(unlist(calib_scores[[j]]))
      c1 <- append(calib_scores_nonconform, alpha1)
      pVal <- length(which(c1 > alpha1)) + runif(1) * length(which(c1 == alpha1))
      pValues[k, j] <- pVal / length(c1)
    }
  }

  return(pValues)
}
