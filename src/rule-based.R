rule.predict = function(win, lose) {
  if (win == lose) {
    return(0)
  }
  
  if (win == 0 & lose == 1) {
    return(0)
  }
  
  if (win == 1 & lose == 0) {
    return(1)
  }
}

get.prediction = function(x) {
  win = x[['win.predictions']]
  lose = x[['lose.predictions']]
  final = rule.predict(win, lose)
  return(final)
}

predictions = apply(data.test, 1, get.prediction)
error = mean(data.test$win != predictions)
accuracy = 1 - error
