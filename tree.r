tree.classify = function(x, tr) {

}

#
# @x List<List> Matrix
#
tree.grow = function(x, y, nmin=2, minleaf=1) {
  if(nrow(x) < nmin){
    return(-1)
  }

  for (i in seq) {

  }
}

tree.clean = function() {
  matrix = read.csv('./pima.txt')
  tree.grow(matrix[,1:8], matrix[,9])
}

tree.impurity = function (v) {
  (sum(v) / length(v)) * (length(v) - sum(v)) / length(v)
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

    currentR = tree.impurity(y) - (piLeft*tree.impurity(left) + piRight*tree.impurity(right))
    if(currentR > bestR){
      bestR = currentR
      bestS = a
    }
  }

  return(c(bestS, bestR))
}
