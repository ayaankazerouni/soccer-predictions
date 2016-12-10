rule.predict = function(win, lose) {
  if (win == 1 & lose == 1) { 
    # win thinks you won, lose thinks you didn't lose
    return(2)
  }
  
  if (win == 0 & lose == 1) {
    # win thinks you didn't win, lose thinks you didn't lose
    return(1)
  }
  
  if (win == 0 & lose == 0) {
    # win thinks you didn't win, lose thinks you lost
    return(0)
  }
  
  if (win == 1 & lose == 0) {
    # win thinks you won, lose thinks you lost
    return(sample(1:3, 1))
  }
}

get.prediction = function(x) {
  win = x[['win.predictions']]
  lose = x[['lose.predictions']]
  final = rule.predict(win, lose)
  return(final)
}

predictions = apply(data.test, 1, get.prediction)
error = mean(data.test$outcome != predictions)
accuracy = 1 - error
