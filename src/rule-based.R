rule.predict = function(win, lose) {
  if (win == 1 & lose == 0) {
    # win model thinks you won, lose model thinks you didn't lose
    return(2) # won
  }
  
  if (win == 0 & lose == 1) {
    # win model thinks you didn't win, lose model thinks you lost
    return(0) # lost
  }
  
  if (win == 1 & lose == 1) {
    # win model thinks you won, lose model thinks you lost
    return(0) # conflict, go with the lose model, since it's more accurate
  }
  
  if (win == 0 & lose == 0) {
    # win model thinks you didn't win, lose model thinks you didn't lose
    return(1) # draw
  }
}

get.prediction = function(x) {
  win = x[['win.predictions']]
  lose = x[['lose.predictions']]
  multi = x[['multinomial.predictions']]
  final = rule.predict(win, lose)
  return(final)
}

predictions = apply(data.test, 1, get.prediction)
error = mean(data.test$outcome != predictions)
accuracy = 1 - error
