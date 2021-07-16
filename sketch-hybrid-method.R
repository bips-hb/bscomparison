library(caret)
library(dplyr)
library(simsham)

instance <- simsham::simsham()

#data("mtcars")
# Center y, X will be standardized in the modelling function
#y <- mtcars %>% select(mpg) %>% scale(center = TRUE, scale = FALSE) %>% as.matrix()
#X <- mtcars %>% select(-mpg) %>% as.matrix()

# Set training control
train_control <- trainControl(method = "repeatedcv",
                              number = 5,
                              repeats = 5,
                              search = "random",
                              returnData = FALSE,
                              verboseIter = TRUE, 
                              savePredictions = FALSE,
                              classProbs = FALSE)

#colnames(y) <- 'y'

# Train the model
Enet_model <- train(instance$y ~ .,
                           data = cbind(instance$y, instance$X),
                           method = "glmnet",
                           metric = "Rsquared",
                           preProcess = c("center", "scale"),
                           maximize = FALSE, 
                           tuneLength = 25,
                           trControl = train_control)

#c = coef(elastic_net_model$finalModel)
#model = elastic_net_model
#coef(model$finalModel, model$bestTune)

Enet_model$bestTune

fit <- glmnet::glmnet(instance$X, instance$y, alpha = Enet_model$bestTune$alpha, lambda = Enet_model$bestTune$lambda)
# determine the active sets for all lambdas
#fit$beta[2:3] = 0
selected_covariates <- c(2, which(as.vector(fit$beta) != 0)) # change
#selected_covariates <- which(as.vector(fit$beta) != 0) 
# if there is only one variable, make sure it is in matrix form

selected_covariates <- as.matrix(selected_covariates)


# what if selected covariates is equal to zero?

X_subset <- as.matrix(instance$X[, selected_covariates])

# apply best subset
range <- 1:ap$k
fit <- bestsubset::bs(instance$X, instance$y, k = range, intercept = FALSE, time.limit = 10*60)
# return list. each entry is for a different k, e.g., 0:20 
# each entry contains the variables that were select for that 
# k 
output <- apply(fit$beta, 2, function(x) which(x != 0))

# take into account that you took a subset of the original covariates
# make sure that the indices reflect that
output <- apply(output, 2, function(o) selected_covariates[o])


y
