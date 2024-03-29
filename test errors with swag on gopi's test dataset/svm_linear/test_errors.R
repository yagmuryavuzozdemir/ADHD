library(e1071)

adhd = read.csv("C:/Users/yagmu/Desktop/Auburn/Research/project/datasetADHD/train_data.csv")
adhd = t(adhd)
colnames(adhd) = as.character(adhd[2,])
adhd = adhd[-(1:2),]
X = apply(adhd[,-1], 2, as.numeric)
adhd = data.frame(adhd[,1], X)
adhd = as.data.frame(adhd)

rownames(adhd) = 1:nrow(adhd)

adhd[adhd == "Controls"]=0
adhd[adhd == "ADHD-C" | adhd == "ADHD-H" | adhd == "ADHD-I"]=1

predictors = as.data.frame(adhd[,-1])
y = as.factor(adhd[,1])

set.seed(2705) 
ind = sample(1:dim(predictors)[1],dim(predictors)[1]*0.2)  
y_test = y[ind]
y_train = y[-ind]
x_test = predictors[ind,]
x_train = predictors[-ind,]

load("SVMLinear_Results.Rda")

min_error = min(train_swag_svml$cv_alpha)
varmat_ind = list() #saves the varmat index from CV errors
for(i in 1:20){
  varmat_ind[[i]]=which(train_swag_svml$CVs[[i]]<=min_error, arr.ind = T)
}

post_sel = list() # models selected after post-processing
for(i in 1:20){
  post_sel[[i]] = train_swag_svml$VarMat[[i]][,varmat_ind[[i]]]
}

post_sel[[1]] = t(as.matrix(post_sel[[1]]))

for(i in 1:20){
  if(!is.matrix(post_sel[[i]])){
    post_sel[[i]]=t(as.matrix(post_sel[[i]]))
  }
}


x = c() #non-empty elements of post-group
for(i in 1:20){
  if(length(post_sel[[i]])!=0){
    x = c(x,i)
  }
  x = x
}

for(i in 1:length(x)){
  if(nrow(post_sel[[x[i]]])<20){
    diff = 20 - nrow(post_sel[[x[i]]])
    post_sel[[x[i]]] = rbind(post_sel[[x[i]]],matrix(NA,nrow = diff, ncol = ncol(post_sel[[x[i]]])))
  } 
}


models = matrix(NA, nrow = 20,ncol=ncol(post_sel[[x[1]]]))

models[1:nrow(post_sel[[x[1]]]),]=post_sel[[x[1]]]

for(i in 2:length(x)){
  models = cbind(models,post_sel[[x[i]]])
}

models = t(models)

### loading test data
test_data = read.csv("test_data.csv")
str(test_data)
test_data = t(test_data)
colnames(test_data) = as.character(test_data[2,])
test_data = test_data[-(1:2),]
X = apply(test_data[,-1], 2, as.numeric)
test_data = data.frame(test_data[,1], X)
test_data = as.data.frame(test_data)
str(test_data)


rownames(test_data) = 1:nrow(test_data)

test_data[test_data == "Controls"]=0
test_data[test_data == "ADHD-C" | test_data == "ADHD-H" | test_data == "ADHD-I"]=1


#### testing the models
errors = rep(NA, dim(models)[1])
for(i in 1:nrow(models)){
  ind_models = models[i,]
  ind_models = na.omit(ind_models)
  var_index = letters[1:length(ind_models)]
  data_train = data.frame("y" = y_train, x_train[, ind_models])
  names(data_train)[-1] = var_index
  formula = as.formula(paste0("y~", paste0(var_index, collapse = "+")))
  
  classifier = svm(formula = formula, data = data_train, kernel = 'linear')
  
  data_test = data.frame("y" = test_data[,1], test_data[, ind_models+1])
  names(data_test)[-1] = var_index
  pred = predict(classifier, newdata = data_test)
  #glm_pred = rep(0, length(pred))
  #glm_pred[pred>.5]=1
  table(pred,data_test[,1])
 errors[i] = 1 - mean(pred==data_test[,1])
}

save(errors, file = "errors.Rda")


### training errors
training_errors = rep(NA, dim(models)[1])
for(i in 1:nrow(models)){
  ind_models = models[i,]
  ind_models = na.omit(ind_models)
  var_index = letters[1:length(ind_models)]
  data_train = data.frame("y" = y_train, x_train[, ind_models])
  names(data_train)[-1] = var_index
  formula = as.formula(paste0("y~", paste0(var_index, collapse = "+")))
  
  classifier = svm(formula = formula, data = data_train, kernel = 'linear')
  
  data_test = data.frame("y" = y_train, x_train[, ind_models])
  names(data_test)[-1] = var_index
  pred = predict(classifier, newdata = data_test)
  #glm_pred = rep(0, length(pred))
  #glm_pred[pred>.5]=1
  table(pred,data_test[,1])
  training_errors[i] = 1 - mean(pred==data_test[,1])
}


save(training_errors, file = "training_errors.Rda")
