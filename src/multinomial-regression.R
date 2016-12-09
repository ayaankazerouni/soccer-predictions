library(nnet)

# data import and preprocessing
difference.vectors = read.csv('difference_vectors.csv', row.names = 'match_api_id')

# multinomial regression  (using the nnet package)
# I declare thee a factor
difference.vectors$outcome = factor(difference.vectors$outcome)

# create 5 folds for cross-validation
folds = cut(seq(1, nrow(difference.vectors)), breaks=5, labels=FALSE)

accuracy.total = 0.0

for (i in 1:5) {
  # 0.8/0.2 train/test
  test.indices = which(folds == 1, arr.ind = TRUE)
  data.test = difference.vectors[test.indices, ]
  data.train = difference.vectors[-test.indices, ]
  
  # train the model
  multinomial.full = multinom(outcome ~ ., data = data.train)
  
  # predictions
  predictions = predict(multinomial.full, newdata = data.test)
  misclassification.error = mean(predictions != data.test$outcome)
  accuracy = 1 - misclassification.error
  accuracy.total = accuracy.total + accuracy
}

accuracy.average = accuracy.total / 5