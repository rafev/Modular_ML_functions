## --------------
# GLMNET model functions
## --------------

GLMNET_mod.search_binary = function(modlist, covariate_interest = NULL, train_grp, train_pheno, valid_grp, valid_pheno, module_df, trControl=NULL, metric = "ROC", maximize=T) {
  require(caret)
  require(progressr)
  
  #Set up progress bar
  handlers(global = TRUE)
  handlers(handler_progress(
    format = ":spin :current/:total [:bar] (:message) :percent in :elapsed ETA: :eta",
    width = 150,
    complete = "=")
  )
  
  if(is.null(trControl)){
    trControl = trainControl(method = "repeatedcv",
                             savePredictions = "final",
                             number = 5,     # number of folds
                             repeats = 3,  # number of repeats
                             classProbs = T,     # assigns class probability
                             summaryFunction = twoClassSummary, # for binary class problems
                             selectionFunction = best, # chooses the class with the highest probability
                             sampling = "up")  # method of sumbampling within Train's sampling routine
  }
  if(is.null(covariate_interest)){
    cat("No response variable selected!!\n")
  } else {
    res_out = list()
    best_run = NULL
    p = progressor(along = modlist)
    for (i in modlist) {
      # Run GLMNET
      set.seed(32)
      Net.fit = train(x = train_grp[, module_df[module_df$colors %in% strsplit(i, "_")[[1]], 1]],
                      y = train_pheno[[covariate_interest]],
                      method = "glmnet",
                      trControl = trControl,
                      metric = metric,
                      tuneGrid = expand.grid(.alpha = seq(.05, 1, length = 15),
                                             .lambda = seq(0.001, 1, 0.005)),
                      family = "binomial",
                      maximize = maximize)
      Prediction.train = predict(Net.fit, newdata = train_grp[, module_df[module_df$colors %in% strsplit(i, "_")[[1]], 1]])
      Prediction.valid = predict(Net.fit, newdata = valid_grp[, module_df[module_df$colors %in% strsplit(i, "_")[[1]], 1]])
      CM.train = confusionMatrix(Prediction.train, train_pheno[[covariate_interest]])
      CM.valid = confusionMatrix(Prediction.valid, valid_pheno[[covariate_interest]])
      
      if(is.null(best_run)){
      	best_run = list(i, round(CM.valid[["byClass"]][11],4))
      } else if(round(CM.valid[["byClass"]][11],4) > best_run[[2]]) {
      	best_run = list(i, round(CM.valid[["byClass"]][11],4))
      }
      cat("Module ", i,"- ", round(CM.valid[["byClass"]][11],4), ": Best Run - Module ", best_run[[1]]," (",best_run[[2]],")\n", sep="")
      training = data.frame(Prediction = Prediction.train, Actual = train_pheno[[covariate_interest]])
      valid = data.frame(Prediction = Prediction.valid, Actual = valid_pheno[[covariate_interest]])
      p(sprintf("i=%s", i))
      ## --- Export Iteration Results
      res_out[[i]] = list(model = Net.fit,
                          CM.train = CM.train,
                          CM.valid = CM.valid,
                          training = training,
                          validation = valid)
      rm(Net.fit,Prediction.train,Prediction.valid,CM.train,CM.valid,training,valid)
      gc()
    }
    return(res_out)
  }
}



GLMNET_mod.search_multiclass = function(modlist, covariate_interest = NULL, train_grp, train_pheno, valid_grp, valid_pheno, module_df, trControl=NULL, type.multinomial = "grouped", metric = "Mean_Balanced_Accuracy", maximize=T) {
  require(caret)
  require(progressr)
  
  #Set up progress bar
  handlers(global = TRUE)
  handlers(handler_progress(
    format = ":spin :current/:total [:bar] (:message) :percent in :elapsed ETA: :eta",
    width = 150,
    complete = "=")
  )
  
  if(is.null(trControl)){
    trControl = trainControl(method = "repeatedcv",
                             savePredictions = "final",
                             number = 5,     # number of folds
                             repeats = 3,  # number of repeats
                             classProbs = T,     # assigns class probability
                             summaryFunction = multiClassSummary, # for multi-class problems
                             selectionFunction = best, # chooses the class with the highest probability
                             sampling = "up")  # method of sumbampling within Train's sampling routine
  }
  if(is.null(covariate_interest)){
    cat("No response variable selected!!\n")
  } else {
    res_out = list()
    p = progressor(along = modlist)
    for (i in modlist) {
      # Run GLMNET
      set.seed(32)
      Net.fit = train(x = train_grp[, module_df[module_df$colors %in% strsplit(i, "_")[[1]], 1]],
                      y = train_pheno[[covariate_interest]],
                      method = "glmnet",
                      trControl = trControl,
                      metric = metric,
                      tuneGrid = expand.grid(.alpha = seq(.05, 1, length = 15),
                                             .lambda = seq(0.001, 1, 0.005)),
                      family = "multinomial",
                      type.multinomial = type.multinomial,
                      maximize = maximize)
      
      Prediction.train = predict(Net.fit, newdata = train_grp[, module_df[module_df$colors %in% strsplit(i, "_")[[1]], 1]])
      Prediction.valid = predict(Net.fit, newdata = valid_grp[, module_df[module_df$colors %in% strsplit(i, "_")[[1]], 1]])
      
      CM.train = confusionMatrix(Prediction.train, train_pheno[[covariate_interest]])
      CM.valid = confusionMatrix(Prediction.valid, valid_pheno[[covariate_interest]])
      
      if(is.null(best_run)){
      	best_run = list(i, round(mean(CM.valid[["byClass"]][,11]),4))
      } else if(round(mean(CM.valid[["byClass"]][,11]),4) > best_run[[2]]) {
      	best_run = list(i, round(mean(CM.valid[["byClass"]][,11]),4))
      }
      cat("Module ", i,"- ", round(CM.valid[["byClass"]][,11],4), ": Best Run - Module ", best_run[[1]]," (Mean Bal.Acc=",best_run[[2]],")\n", sep="")
      
      training = data.frame(Prediction = Prediction.train, Actual = train_pheno[[covariate_interest]])
      valid = data.frame(Prediction = Prediction.valid, Actual = valid_pheno[[covariate_interest]])
      
      
      p(sprintf("i=%s", i))
      
      ## --- Export Iteration Results
      res_out[[i]] = list(model = Net.fit,
                          CM.train = CM.train,
                          CM.valid = CM.valid,
                          training = training,
                          validation = valid)
    }
    return(res_out)
    
  }
}


## --------------
# SVM_Linear model function (binary/multiclass)
## --------------
SVM.L_mod.search = function(modlist, covariate_interest = NULL, train_grp, train_pheno, valid_grp, valid_pheno, module_df, trControl=NULL, metric=NULL, maximize=NULL, seq_length=40) {
  require(caret)
  require(progressr)
  
  #Set up progress bar
  handlers(global = TRUE)
  handlers(handler_progress(
    format = ":spin :current/:total [:bar] (:message) :percent in :elapsed ETA: :eta",
    width = 150,
    complete = "=")
  )
  
  if(is.null(trControl)){
    trControl = trainControl(method = "repeatedcv",
                             savePredictions = "final",
                             number = 5,     # number of folds
                             repeats = 3,  # number of repeats
                             classProbs = T,     # assigns class probability
                             summaryFunction = twoClassSummary, # for binary class problems
                             selectionFunction = best, # chooses the class with the highest probability
                             sampling = "up")  # method of sumbampling within Train's sampling routine
  }
  if(is.null(metric)) {
    cat("No metric set! Assigning defaults...\n")
    if(length(unique(train_pheno[[covariate_interest]])) > 2){
      trControl$summaryFunction = multiClassSummary
      metric = "Mean_Balanced_Accuracy"
      maximize = T
    } else {
      metric = "ROC"
      maximize = T
    }
  }
  
  if(is.null(covariate_interest)){
    cat("No response variable selected!!\n")
    stop()
  } else {
    res_out = list()
    best_run = NULL
    p = progressor(along = modlist)
    
    train_grp.B_SVM.DF = merge(as.data.frame(train_pheno[,c(id_column, covariate_interest), with = F]),
                               as.data.frame(train_grp[, module_df[module_df$colors %in% strsplit(i, "_")[[1]], 1]]),
                               by.x = id_column, by.y = 0)
    colnames(train_grp.B_SVM.DF)[2] = "phenotype"
    for (i in modlist) {
      # Run svm-L
      set.seed(32)
      Net.fit = train(phenotype~.,
                      data = train_grp.B_SVM.DF[,2:ncol(train_grp.B_SVM.DF)],
                      method="svmLinear", trControl= trControl,
                      metric = metric, maximize = maximize,
                      tuneGrid = expand.grid(C = seq(0, 2, length = seq_length)))
      
      Prediction.train = predict(Net.fit, newdata = train_grp[, module_df[module_df$colors %in% strsplit(i, "_")[[1]], 1]])
      Prediction.valid = predict(Net.fit, newdata = valid_grp[, module_df[module_df$colors %in% strsplit(i, "_")[[1]], 1]])
      CM.train = confusionMatrix(Prediction.train, train_pheno[[covariate_interest]])
      CM.valid = confusionMatrix(Prediction.valid,  valid_pheno[[covariate_interest]])
      
      if(length(unique(train_pheno[[covariate_interest]])) > 2) {
        
        if(is.null(best_run)){
          best_run = list(i, round(mean(CM.valid[["byClass"]][,11]),4))
        } else if(round(mean(CM.valid[["byClass"]][,11]),4) > best_run[[2]]) {
          best_run = list(i, round(mean(CM.valid[["byClass"]][,11]),4))
        }
        cat("Module ", i,"- ", round(CM.valid[["byClass"]][,11],4), ": Best Run - Module ", best_run[[1]]," (Mean Bal.Acc=",best_run[[2]],")\n", sep="")
        
      } else {
        
        if(is.null(best_run)){
        	best_run = list(i, round(CM.valid[["byClass"]][11],4))
        } else if(round(CM.valid[["byClass"]][11],4) > best_run[[2]]) {
        	best_run = list(i, round(CM.valid[["byClass"]][11],4))
        }
        cat("Module ", i,"- ", round(CM.valid[["byClass"]][11],4), ": Best Run - Module ", best_run[[1]]," (",best_run[[2]],")\n", sep="")
        
      }
      
      training = data.frame(Prediction = Prediction.train, Actual = train_pheno[[covariate_interest]])
      valid = data.frame(Prediction = Prediction.valid, Actual = valid_pheno[[covariate_interest]])
      p(sprintf("i=%s", i))
      ## --- Export Iteration Results
      res_out[[i]] = list(model = Net.fit,
                          CM.train = CM.train,
                          CM.valid = CM.valid,
                          training = training,
                          validation = valid)
      rm(Net.fit,Prediction.train,Prediction.valid,CM.train,CM.valid,training,valid)
      gc()
    }
    return(res_out)
  }
}


## --------------
# RF model function (binary/multiclass)
## --------------

RF_mod.search = function(modlist, covariate_interest = NULL, train_grp, train_pheno, valid_grp, valid_pheno, module_df, trControl=NULL, tuneLength=20, metric=NULL, maximize=NULL) {
  require(caret)
  require(progressr)
  
  #Set up progress bar
  handlers(global = TRUE)
  handlers(handler_progress(
    format = ":spin :current/:total [:bar] (:message) :percent in :elapsed ETA: :eta",
    width = 150,
    complete = "=")
  )
  
  if(is.null(trControl)){
    trControl = trainControl(method = "repeatedcv",
                             savePredictions = "final",
                             number = 5,     # number of folds
                             repeats = 3,  # number of repeats
                             classProbs = T,     # assigns class probability
                             summaryFunction = twoClassSummary, # for binary class problems
                             selectionFunction = best, # chooses the class with the highest probability
                             sampling = "up")  # method of sumbampling within Train's sampling routine
  }
  if(is.null(metric)) {
    cat("No metric set! Assigning defaults...\n")
    if(length(unique(train_pheno[[covariate_interest]])) > 2){
      trControl$summaryFunction = multiClassSummary
      metric = "Mean_Balanced_Accuracy"
      maximize = T
    } else {
      metric = "ROC"
      maximize = T
    }
  }
  
  if(is.null(covariate_interest)){
    cat("No response variable selected!!\n")
    stop()
  } else {
    res_out = list()
    best_run = NULL
    p = progressor(along = modlist)
    for (i in modlist) {
      # Run RF
      set.seed(32)
      Net.fit = train(x = train_grp[, module_df[module_df$colors %in% strsplit(i, "_")[[1]], 1]],
                           y = train_pheno[[covariate_interest]],
                           metric = metric,
                           maximize = maximize,
                           tuneLength = tuneLength,
                           method = "rf", trControl = trControl)
                           
      Prediction.train = predict(Net.fit, newdata = train_grp[, module_df[module_df$colors %in% strsplit(i, "_")[[1]], 1]])
      Prediction.valid = predict(Net.fit, newdata = valid_grp[, module_df[module_df$colors %in% strsplit(i, "_")[[1]], 1]])
      CM.train = confusionMatrix(Prediction.train, train_pheno[[covariate_interest]])
      CM.valid = confusionMatrix(Prediction.valid, valid_pheno[[covariate_interest]])
      
      if(length(unique(train_pheno[[covariate_interest]])) > 2) {
        
        if(is.null(best_run)){
          best_run = list(i, round(mean(CM.valid[["byClass"]][,11]),4))
        } else if(round(mean(CM.valid[["byClass"]][,11]),4) > best_run[[2]]) {
          best_run = list(i, round(mean(CM.valid[["byClass"]][,11]),4))
        }
        cat("Module ", i,"- ", round(CM.valid[["byClass"]][,11],4), ": Best Run - Module ", best_run[[1]]," (Mean Bal.Acc=",best_run[[2]],")\n", sep="")
        
      } else {
        
        if(is.null(best_run)){
          best_run = list(i, round(CM.valid[["byClass"]][11],4))
        } else if(round(CM.valid[["byClass"]][11],4) > best_run[[2]]) {
          best_run = list(i, round(CM.valid[["byClass"]][11],4))
        }
        cat("Module ", i,"- ", round(CM.valid[["byClass"]][11],4), ": Best Run - Module ", best_run[[1]]," (",best_run[[2]],")\n", sep="")
        
      }
      
      training = data.frame(Prediction = Prediction.train, Actual = train_pheno[[covariate_interest]])
      valid = data.frame(Prediction = Prediction.valid, Actual = valid_pheno[[covariate_interest]])
      p(sprintf("i=%s", i))
      ## --- Export Iteration Results
      res_out[[i]] = list(model = Net.fit,
                          CM.train = CM.train,
                          CM.valid = CM.valid,
                          training = training,
                          validation = valid)
      rm(Net.fit,Prediction.train,Prediction.valid,CM.train,CM.valid,training,valid)
      gc()
    }
    return(res_out)
  }
}


## --------------
# Generic model function (binary/multiclass)
## --------------

Generic_mod.search_binary = function(model_method = NULL, modlist, covariate_interest = NULL, trControl=NULL,
                                     train_grp, train_pheno, valid_grp, valid_pheno, module_df, metric = NULL, maximize = NULL) {
  require(caret)
  require(progressr)
  
  #Set up progress bar
  handlers(global = TRUE)
  handlers(handler_progress(
    format = ":spin :current/:total [:bar] (:message) :percent in :elapsed ETA: :eta",
    width = 150,
    complete = "=")
  )
  
  if(is.null(trControl)){
    trControl = trainControl(method = "repeatedcv",
                             savePredictions = "final",
                             number = 5,     # number of folds
                             repeats = 3,  # number of repeats
                             classProbs = T,     # assigns class probability
                             summaryFunction = twoClassSummary, # for binary class problems
                             selectionFunction = best, # chooses the class with the highest probability
                             sampling = "up")  # method of sumbampling within Train's sampling routine
  }
  if(is.null(metric)) {
    cat("No metric set! Assigning defaults...\n")
    if(length(unique(train_pheno[[covariate_interest]])) > 2){
      trControl$summaryFunction = multiClassSummary
      metric = "Mean_Balanced_Accuracy"
      maximize = T
    } else {
      metric = "ROC"
      maximize = T
    }
  }
  
  if(is.null(covariate_interest)){
    cat("No response variable selected!!\n")
    stop()
  } else {
    cat("Running ", model_method, "models\n")
    res_out = list()
    best_run = NULL
    p = progressor(along = modlist)
    for (i in modlist) {
      tryCatch({
        # Run CARET
        set.seed(32)
        Net.fit = train(x = train_grp[, module_df[module_df$colors %in% strsplit(i, "_")[[1]], 1]],
                        y = train_pheno[[covariate_interest]],
                        metric = metric,
                        maximize = maximize,
                        method = model_method,
                        trControl = trControl)
        
        Prediction.train = predict(Net.fit, newdata = train_grp[, module_df[module_df$colors %in% strsplit(i, "_")[[1]], 1]])
        Prediction.valid = predict(Net.fit, newdata = valid_grp[, module_df[module_df$colors %in% strsplit(i, "_")[[1]], 1]])
        CM.train = confusionMatrix(Prediction.train, train_pheno[[covariate_interest]])
        CM.valid = confusionMatrix(Prediction.valid, valid_pheno[[covariate_interest]])
        
        if(length(unique(train_pheno[[covariate_interest]])) > 2) {
          
          if(is.null(best_run)){
            best_run = list(i, round(mean(CM.valid[["byClass"]][,11]),4))
          } else if(round(mean(CM.valid[["byClass"]][,11]),4) > best_run[[2]]) {
            best_run = list(i, round(mean(CM.valid[["byClass"]][,11]),4))
          }
          cat("Module ", i,"- ", round(CM.valid[["byClass"]][,11],4), ": Best Run - Module ", best_run[[1]]," (Mean Bal.Acc=",best_run[[2]],")\n", sep="")
          
        } else {
          
          if(is.null(best_run)){
            best_run = list(i, round(CM.valid[["byClass"]][11],4))
          } else if(round(CM.valid[["byClass"]][11],4) > best_run[[2]]) {
            best_run = list(i, round(CM.valid[["byClass"]][11],4))
          }
          cat("Module ", i,"- ", round(CM.valid[["byClass"]][11],4), ": Best Run - Module ", best_run[[1]]," (",best_run[[2]],")\n", sep="")
          
        }
        
        training = data.frame(Prediction = Prediction.train, Actual = train_pheno[[covariate_interest]])
        valid = data.frame(Prediction = Prediction.valid, Actual = valid_pheno[[covariate_interest]])
        
        ## --- Export Iteration Results
        res_out[[i]] = list(model = Net.fit,
                            CM.train = CM.train,
                            CM.valid = CM.valid,
                            training = training,
                            validation = valid)
        rm(Net.fit,Prediction.train,Prediction.valid,CM.train,CM.valid,training,valid)
        gc()
        
      }, error=function(e){cat("ERROR : Iteratrion failed")})
      p(sprintf("i=%s", i))
      
    }
    return(res_out)
  }
}




## --------------
# Full ML scan function
# Useful when have already narrowed down to a set of features 
## --------------

ML_suite_scan = function(modules_final, covariate_interest = NULL, id_column, trControl, 
                         train_grp, train_pheno, valid_grp, valid_pheno,
                         tuneLength=20, metric = NULL, maximize = NULL,
                         family = "binomial") {
  require(caret)
  require(progressr)
  
  #Set up progress bar
  handlers(global = TRUE)
  handlers(handler_progress(
    format = ":spin :current/:total [:bar] (:message) :percent in :elapsed ETA: :eta",
    width = 150,
    complete = "=")
  )
  
  if(is.null(trControl)){
    trControl = trainControl(method = "repeatedcv",
                             savePredictions = "final",
                             number = 5,     # number of folds
                             repeats = 3,  # number of repeats
                             classProbs = T,     # assigns class probability
                             summaryFunction = twoClassSummary, # for binary class problems
                             selectionFunction = best, # chooses the class with the highest probability
                             sampling = "up")  # method of sumbampling within Train's sampling routine
  }
  if(is.null(metric)) {
    cat("No metric set! Assigning defaults...\n")
    if(length(unique(train_pheno[[covariate_interest]])) > 2){
      trControl$summaryFunction = multiClassSummary
      metric = "Mean_Balanced_Accuracy"
      maximize = T
    } else {
      metric = "ROC"
      maximize = T
    }
  }
  
  if(is.null(covariate_interest)){
    cat("No response variable selected!!\n")
    stop()
  }
  
  for (i in modules_final) {
    cat("\nWorking on module:", i,"\n")
    
    train_grp.B_SVM.DF = merge(as.data.frame(train_pheno[,c(id_column, covariate_interest), with = F]),
                               as.data.frame(train_grp[, module_df[module_df$colors %in% strsplit(i, "_")[[1]], 1]]),
                               by.x = id_column, by.y = 0)
    colnames(train_grp.B_SVM.DF)[2] = "phenotype"
    
    # Bagged CART
    cat("Bagged CART\n")
    set.seed(32)
    fit.treebag <- train(x = train_grp[, module_df[module_df$colors %in% strsplit(i, "_")[[1]], 1]],
                                y = train_pheno[[covariate_interest]],
                                metric = metric,
                                maximize = maximize,
                                method = "treebag", trControl = trControl)
    
    # RF
    cat("RF\n")
    set.seed(32)
    fit.rf <- train(x = train_grp[, module_df[module_df$colors %in% strsplit(i, "_")[[1]], 1]],
                           y = train_pheno[[covariate_interest]],
                           metric = metric,
                           tuneLength = 20,
                           maximize = maximize,
                           method = "rf", trControl = trControl)
    
    # C5.0
    cat("C5.0\n")
    set.seed(32)
    fit.c50 <- train(x = train_grp[, module_df[module_df$colors %in% strsplit(i, "_")[[1]], 1]],
                            y = train_pheno[[covariate_interest]],
                           	metric = metric,
                           	maximize = maximize,
                            method = "C5.0", trControl = trControl)
    
    # LDA - Linear Discriminate Analysis
    cat("LDA\n")
    set.seed(32)
    fit.lda <- train(x = train_grp[, module_df[module_df$colors %in% strsplit(i, "_")[[1]], 1]],
                            y = train_pheno[[covariate_interest]],
                            metric = metric, maximize = maximize,
                            method="lda", trControl= trControl)
    
    # GLMNET - Regularized Logistic Regression
    cat("ENET\n")
    set.seed(32)
    fit.glmnet <- train(x = train_grp[, module_df[module_df$colors %in% strsplit(i, "_")[[1]], 1]],
                               y = train_pheno[[covariate_interest]],
                               method = "glmnet",
                               trControl = trControl,
                               tuneGrid = expand.grid(.alpha = seq(.05, 1, length = 15),
                                                      .lambda = seq(0.001, 1, 0.005)),
                               family = family,
                               metric = metric, maximize = maximize)
                               
    # KNN - k-Nearest Neighbors 
    cat("KNN\n")
    set.seed(32)
    fit.knn <- train(x = train_grp[, module_df[module_df$colors %in% strsplit(i, "_")[[1]], 1]],
                            y = train_pheno[[covariate_interest]],
                            metric = metric, maximize = maximize,
                            method="knn",
                            tuneGrid = data.frame(k = seq(11,85,by=2)),
                            trControl= trControl)
    
    # SVM Linear - Support Vector Machine, 
    set.seed(32)
    cat("SVM-L\n")
    fit.svmLin <- train(phenotype~.,
                        data = train_grp.B_SVM.DF[,2:ncol(train_grp.B_SVM.DF)],
                        method="svmLinear", trControl= trControl,
                        metric = metric, maximize = maximize,
                        tuneGrid = expand.grid(C = seq(0, 2, length = 20)))
    
    # SVM Radial - Support Vector Machine, 
    cat("SVM-R\n")
    set.seed(32)
    fit.svmRad <- train(phenotype~.,
                        data = train_grp.B_SVM.DF[,2:ncol(train_grp.B_SVM.DF)],
                        method="svmRadial", trControl= trControl,
                        metric = metric, maximize = maximize,
                        tuneLength = 20)
    
    
    # Colate models into single object
    ML_models = list("Bagged CART" = fit.treebag,
                     "Random Forest" = fit.rf,
                     "C5.0" = fit.c50,
                     "LDA" = fit.lda,
                     "GLMNET" = fit.glmnet,
                     "KNN" = fit.knn,
                     "Linear SVM" = fit.svmLin,
                     "Radial SVM" = fit.svmRad
    )
    rm(fit.treebag, fit.rf, fit.c50, fit.lda, fit.glmnet, fit.knn, fit.svm.Lin, fit.svmRad)
    gc()
    
    
    ## ------------------------------
    # Assess validation results
    ## ------------------------------

    ML.Bal.Acc_validation = sapply(names(ML_models), function(j){
      
      Prediction.valid = predict(object = ML_models[[j]],
                                 newdata = valid_grp[, module_df[module_df$colors %in% strsplit(i, "_")[[1]], 1]])
      
      out = confusionMatrix(Prediction.valid,
                                  valid_pheno[[covariate_interest]])
      
      return(out)
    }, simplify = F, USE.NAMES = T)
    
    
    res_out_all[[i]] = list(ML_models = ML_models,
                            ML.Bal.Acc_validation = ML.Bal.Acc_validation)
                       
  }
  return(res_out_all)
}



## --------------
# Full ML Stack ENSEMBLE scan function
# Used for generating many STACKED models
## --------------

ENSEMBLE_ML_suite_scan = function(Ensemble_models_list, covariate_interest = NULL,
                                  trControl, tuneLength=20, valid_grp, valid_pheno,
                                  metric = NULL, maximize = NULL,
                                  family = "binomial") {
  require(caret)
  require(progressr)
  
  #Set up progress bar
  handlers(global = TRUE)
  handlers(handler_progress(
    format = ":spin :current/:total [:bar] (:message) :percent in :elapsed ETA: :eta",
    width = 150,
    complete = "=")
  )
  
  if(is.null(trControl)){
    trControl = trainControl(method = "repeatedcv",
                             savePredictions = "final",
                             number = 5,     # number of folds
                             repeats = 3,  # number of repeats
                             classProbs = T,     # assigns class probability
                             summaryFunction = twoClassSummary, # for binary class problems
                             selectionFunction = best, # chooses the class with the highest probability
                             sampling = "up")  # method of sumbampling within Train's sampling routine
  }
  if(is.null(metric)) {
    cat("No metric set! Assigning defaults...\n")
    if(length(unique(train_pheno[[covariate_interest]])) > 2){
      trControl$summaryFunction = multiClassSummary
      metric = "Mean_Balanced_Accuracy"
      maximize = T
    } else {
      metric = "ROC"
      maximize = T
    }
  }
  
  if(is.null(covariate_interest)){
    cat("No response variable selected!!\n")
    stop()
  }
  
  # Bagged CART
  cat("Bagged CART\n")
  set.seed(32)
  fit.treebag <- caretEnsemble::caretStack(Ensemble_models_list,
                                           metric = metric, maximize = maximize,
                                           method = "treebag", trControl = trControl)
  
  # RF
  cat("RF\n")
  set.seed(32)
  fit.rf <- caretEnsemble::caretStack(Ensemble_models_list,
                                      metric = metric, maximize = maximize,
                                      tuneLength = 20,
                                      method = "rf", trControl = trControl)
  
  # C5.0
  cat("C5.0\n")
  set.seed(32)
  fit.c50 <- caretEnsemble::caretStack(Ensemble_models_list,
                                       metric = metric, maximize = maximize,
                                       method = "C5.0", trControl = trControl)
  
  # LDA - Linear Discriminate Analysis
  cat("LDA\n")
  set.seed(32)
  fit.lda <- caretEnsemble::caretStack(Ensemble_models_list,
                                       metric = metric, maximize = maximize,
                                       method="lda", trControl= trControl)
  
  # GLMNET - Regularized Logistic Regression
  cat("ENET\n")
  set.seed(32)
  fit.glmnet <- caretEnsemble::caretStack(Ensemble_models_list,
                                          metric = metric, maximize = maximize,
                                          method = "glmnet", trControl = trControl,
                                          tuneGrid = expand.grid(.alpha = seq(.05, 1, length = 15),
                                                    .lambda = seq(0.001, 1, 0.005)),
                                          family = family)
  
  # KNN - k-Nearest Neighbors 
  cat("KNN\n")
  set.seed(32)
  fit.knn <- caretEnsemble::caretStack(Ensemble_models_list,
                                       metric = metric, maximize = maximize,
                                       method="knn", trControl= trControl,
                                       tuneGrid = data.frame(k = seq(11,85,by=2)))
  
  # SVM Linear - Support Vector Machine, 
  set.seed(32)
  cat("SVM-L\n")
  fit.svmLin <- caretEnsemble::caretStack(Ensemble_models_list,
                                          metric = metric, maximize = maximize,
                                          method="svmLinear", trControl= trControl,
                                          tuneGrid = expand.grid(C = seq(0, 2, length = 20)))
  
  # SVM Radial - Support Vector Machine, 
  cat("SVM-R\n")
  set.seed(32)
  fit.svmRad <- caretEnsemble::caretStack(Ensemble_models_list,
                                          metric = metric, maximize = maximize,
                                          method="svmRadial", trControl= trControl,
                                          tuneLength = 20)
  
  
  # Colate models into single object
  ML_models = list("Bagged CART" = fit.treebag,
                   "Random Forest" = fit.rf,
                   "C5.0" = fit.c50,
                   "LDA" = fit.lda,
                   "GLMNET" = fit.glmnet,
                   "KNN" = fit.knn,
                   "Linear SVM" = fit.svmLin,
                   "Radial SVM" = fit.svmRad
  )
  rm(fit.treebag, fit.rf, fit.c50, fit.lda, fit.glmnet, fit.knn, fit.svm.Lin, fit.svmRad)
  gc()
  
  
  
  ## ------------------------------
  # Assess validation results
  ## ------------------------------
  
  ML.Bal.Acc_validation = sapply(names(ML_models), function(j){
    cat("\nCM for", j)
    Prediction.valid = predict(object = ML_models[[j]],
                               newdata = valid_grp) %>% 
      mutate("Prediction"=names(.)[apply(., 1, which.max)])
    Prediction.valid$Prediction = factor(Prediction.valid$Prediction,
                                         levels=levels(valid_pheno[[covariate_interest]]))
    
    
    out = caret::confusionMatrix(Prediction.valid$Prediction,
                                 valid_pheno[[covariate_interest]])
    
    return(out)
  }, simplify = F, USE.NAMES = T)
  
  
  res_out_all = list(Ensemble_ML_models = ML_models,
                     ML.Bal.Acc_validation = ML.Bal.Acc_validation)
  
  return(res_out_all)
}
