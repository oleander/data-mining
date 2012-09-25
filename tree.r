tree.classify = function(x, tr) {

}

tree.grow = function(x, y, nmin, minleaf) {

}

tree.impurity = function (v) {
  sum(v) / length(v) * (length(v) - sum(v)) / length(v)
}

tree.bestsplit = function (x, y) {
  x_ <- x[order(x)]
  y_ <- y[order(x)]

  bestR = -1
  bestS = NULL  

  for (i in 1:  (length(x_) - 1))  {
    if (x_[i] == x_[i + 1]) {
      next
    }

    a <- mean(x_[i]:x_[i + 1])
    left = y_[x_ <= a]
    right = y_[x_ > a]
    piLeft <- length(left)/length(x_)
    piRight <- length(right)/length(x_)

    currentR = impurity(x) - (piLeft*impurity(left) + piRight*impurity(right))
    if(currentR > bestR){
      bestR = currentR
      bestS = a
    }
  }

  return(bestS)
}
