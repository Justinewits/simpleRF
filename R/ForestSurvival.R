
##' @title Survival forest class
##' @description Subclass for survival forest. 
##' Contains all fields and methods used special for survival forests.
##' @importFrom ipred sbrier
ForestSurvival <- setRefClass("ForestSurvival", 
  contains = "Forest", 
  fields = list(
     timepoints = "numeric",
     sample_size = "numeric"), 
  methods = list(
    
    grow = function(num_threads) {      
      treetype <<- "Survival"
      
      ## Create trees
      trees <<- replicate(num_trees, TreeSurvival$new(timepoints = timepoints, sample_size=sample_size))
      
      ## Call parent method
      callSuper(num_threads)
    }, 
    
    predict = function(newdata) {
      callSuper(newdata)
    },
    
    aggregatePredictions = function(predictions) {
      ## For each person and timepoint mean over trees      
      result <- apply(predictions, c(2,1), mean)      
      return(result)
    }, 
    
    predictionError = function() {      
      ## For each sample mean over trees where sample is OOB
      tree_predictions <- simplify2array(lapply(trees, function(x) {
        oob_samples <- x$oob_sampleIDs
        result <- matrix(NA, length(timepoints), data$nrow)
        result[, oob_samples] <- x$predictOOB()        
        return(result)
      }))
      oob_predictions <- apply(tree_predictions, c(2,1), mean, na.rm = TRUE)
      
      ## Return brier score for OOB predictions
      idx <- rowSums(is.na(oob_predictions)) == 0
      #bs <- ipred::sbrier(data$subset(idx, 1), t(oob_predictions[idx, ]), timepoints)
      #brier <- riskRegression::Score(list("Pred" = t(oob_predictions[idx, ])), formula = Surv(time, status) ~ 1, data = data, times = timepoints)$Brier$score["Pred", ]
      
      conc  <-Hmisc::rcorr.cens(rowSums(oob_predictions[idx, ]), data$subset(idx, 1))["C Index"]
      return(1-conc)
    })
)


