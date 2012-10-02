tree.classify = function(x, tr) {

}

tree.grow = function(matrix, class, nmin=2, minleaf=1) {
  if(nrow(matrix) < nmin){
    return(tree.createLeaf(class))
  }

  bestR = -1
  bestS = NULL
  bestI = NULL
  for (i in 1:ncol(matrix)) {
    result = tree.bestsplit(matrix[,i], class)
    if(result[2] > bestR) {
      bestR = result[2]
      bestS = result[1]
      bestI = i
    }
  }

  left = matrix[matrix[,bestI] <= bestS,]
  leftClass = class[matrix[,bestI] <= bestS]

  right = matrix[matrix[,bestI] > bestS,]
  rightClass = class[matrix[,bestI] > bestS]

  if(nrow(left) < minleaf || nrow(right) < minleaf) {
    return(tree.createLeaf(class))
  }

  leftG = tree.grow(left, leftClass, nmin, minleaf)
  rightG = tree.grow(right, rightClass, nmin, minleaf)

  return(
    list(bestI, bestS, leftG, rightG)
  )
}

tree.main = function() {
  matrix = read.csv('pima.txt')
  tree.grow(matrix[,1:8], matrix[,9])
}

tree.impurity = function (v) {
  (sum(v) / length(v)) * (length(v) - sum(v)) / length(v)
}

tree.createLeaf = function(class) {
  return(round((sum(class) / length(class)) + 0.01))
}

tree.bestsplit = function (x, y) {
  x_ <- x[order(x)]
  y_ <- y[order(x)]

  bestR = -1
  bestS = 0 # "Should be" NULL

  for (i in 1:(length(x_) - 1))  {
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
