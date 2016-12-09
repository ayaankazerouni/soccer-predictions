library(caret)

# data import and preprocessing
difference.vectors = read.csv('difference_vectors.csv', row.names = 'match_api_id')

# omit NAs (about 3000 rows have nas for the odds columns)
difference.vectors = na.omit(difference.vectors)

# stepwise logistic regression
# turn the outcome variable into a binary variable (1 = win, 0 = not win)
difference.vectors$outcome[difference.vectors$outcome == 1] = 0
difference.vectors$outcome[difference.vectors$outcome == 2] = 1
drop = c('away_team_odds', 'home_team_odds', 'draw_odds')
difference.vectors = difference.vectors[, !names(difference.vectors) %in% drop]

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
  logistic.full = glm(outcome ~ ., data = data.train, family = binomial(link = 'logit'))
  logistic.null = glm(outcome ~ 1, data = data.train, family = binomial(link = 'logit'))
  logistic.final = step(logistic.full, scope = c(logistic.null, logistic.full), direction = 'backward', trace = -1)
  
  # predictions
  predictions = predict(logistic.final, newdata = data.test)
  predictions = ifelse(predictions > 0.5, 1, 0)
  misclassification.error = mean(predictions != data.test$outcome)
  accuracy = 1 - misclassification.error
  accuracy.total = accuracy.total + accuracy
  
  # ROC curve
  library(ROCR)
  pr = prediction(predictions, data.test$outcome)
  prf = performance(pr, measure = 'tpr', x.measure = 'fpr')
  plot(prf)
  
  auc = performance(pr, measure = 'auc')
  auc = auc@y.values[[1]]
}

accuracy.average = accuracy.total / 5