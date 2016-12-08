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

# 0.8/0.2 train/test
smp_size = floor(0.80 * nrow(difference.vectors))
set.seed(123)
train.indices = sample(seq_len(nrow(difference.vectors)), size=smp_size)
data.train = difference.vectors[train.indices, ]
data.test = difference.vectors[-train.indices, ]

# train the model
logistic.full = glm(outcome ~ ., data = data.train, family = binomial(link = 'logit'))
logistic.null = glm(outcome ~ 1, data = data.test, family = binomial(link = 'logit'))
logistic.final = step(logistic.full, scope = c(logistic.null, logistic.full), direction = 'backward')

# predictions
predictions = predict(logistic.final, newdata = data.test)
predictions = ifelse(predictions > 0.5, 1, 0)
misclassification.error = mean(predictions != data.test$outcome)
accuracy = 1 - misclassification.error

# ROC curve
library(ROCR)
pr = prediction(predictions, data.test$outcome)
prf = performance(pr, measure = 'tpr', x.measure = 'fpr')
plot(prf)

auc = performance(pr, measure = 'auc')
auc = auc@y.values[[1]]
