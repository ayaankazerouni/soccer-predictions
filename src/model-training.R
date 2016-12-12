# data import and preprocessing
difference.vectors = read.csv('difference_vectors_new.csv')

# stepwise logistic regression
# create outcome variables for win/not-win and lose/not-lose
difference.vectors$win[difference.vectors$outcome %in% c(1, 0)] = 0
difference.vectors$win[difference.vectors$outcome == 2] = 1
difference.vectors$lose[difference.vectors$outcome %in% c(1, 2)] = 0
difference.vectors$lose[difference.vectors$outcome == 0] = 1

# I declare thee a factor
difference.vectors$win = factor(difference.vectors$win)
difference.vectors$lose = factor(difference.vectors$lose)

# create 5 folds for cross-validation
folds = cut(seq(1, nrow(difference.vectors)), breaks=5, labels=FALSE)

win.accuracy.total = 0.0
lose.accuracy.total = 0.0
multinomial.accuracy.total = 0.0
tree.accuracy.total = 0.0

for (i in 1:5) {
  # 0.8/0.2 train/test
  test.indices = which(folds == i, arr.ind = TRUE)
  data.test = difference.vectors[test.indices, ]
  data.train = difference.vectors[-test.indices, ]
  
  # train the model to predict win/not-win
  win.null = glm(win ~ 1, data = data.train, family = binomial(link = 'logit'))
  win.full = glm(win ~ win_percentage + buildUpPlaySpeed + buildUpPlayPassing + chanceCreationPassing
                 + chanceCreationCrossing + chanceCreationShooting + defencePressure + defenceAggression
                 + defenceTeamWidth + pos_percentage, data = data.train, family = binomial(link = 'logit'), na.action = na.pass)
  win.final = step(win.full, scope = list(upper=win.full, lower=win.null), direction = 'backward', trace = -1)
  
  # train the model to predict lose/not-lose
  lose.null = glm(lose ~ 1, data = data.train, family = binomial(link = 'logit'))
  lose.full = glm(lose ~ win_percentage + buildUpPlaySpeed + buildUpPlayPassing + chanceCreationPassing
                 + chanceCreationCrossing + chanceCreationShooting + defencePressure + defenceAggression
                 + defenceTeamWidth + pos_percentage, data = data.train, family = binomial(link = 'logit'), na.action = na.pass)
  lose.final = step(lose.full, scope = list(upper = lose.full, lower = lose.null), direction = 'backward', trace = -1)
  
  # train the multinomial model to predict win/lose/draw
  library(nnet)
  multinomial.full = multinom(outcome ~ win_percentage, data = data.train, na.action = na.pass)
  
  # train a decision tree to predict win/lose/draw
  library(rpart)
  tree = rpart(outcome ~ win_percentage + buildUpPlaySpeed + buildUpPlayPassing + chanceCreationPassing
               + chanceCreationCrossing + chanceCreationShooting + defencePressure + defenceAggression
               + defenceTeamWidth + pos_percentage, data = data.train, method = 'class', 
               control=rpart.control(minsplit=30, cp=0.001))
  # automatically prune at the most optimum point by looking at complexity parameters
  prune(tree, tree$cptable[which.min(tree$cptable[,"xerror"]),"CP"])
  
  # win/not-win predictions
  win.predictions = predict(win.final, newdata = data.test)
  win.predictions = ifelse(win.predictions > 0.5, 1, 0)
  win.error = mean(win.predictions != data.test$win)
  win.accuracy = 1 - win.error
  win.accuracy.total = win.accuracy.total + win.accuracy
  data.test$win.predictions = win.predictions
  
  # lose/not-lose predictions
  lose.predictions = predict(lose.final, newdata = data.test)
  lose.predictions = ifelse(lose.predictions > 0.5, 1, 0)
  lose.error = mean(lose.predictions != data.test$lose)
  lose.accuracy = 1 - lose.error
  lose.accuracy.total = lose.accuracy.total + lose.accuracy
  data.test$lose.predictions = lose.predictions
  
  # win/lose/draw predictions using multinomial regression
  multinomial.predictions = predict(multinomial.full, newdata = data.test)
  multinomial.error = mean(multinomial.predictions != data.test$outcome)
  multinomial.accuracy = 1 - multinomial.error
  multinomial.accuracy.total = multinomial.accuracy.total + multinomial.accuracy
  data.test$multinomial.predictions = multinomial.predictions
  
  # win/lose/draw/predictions using a decision tree
  tree.predictions = predict(tree, newdata = data.test, type='class')
  tree.error = mean(tree.predictions != data.test$outcome)
  tree.accuracy = 1 - tree.error
  tree.accuracy.total = tree.accuracy.total + tree.accuracy
  data.test$tree.predictions = tree.predictions
}

win.accuracy.average = win.accuracy.total / 5
lose.accuracy.average = lose.accuracy.total / 5
multinomial.accuracy.average = multinomial.accuracy.total / 5
tree.accuracy.average = tree.accuracy.total / 5