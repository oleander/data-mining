tree.grow = function(matrix, class, nmin=2, minleaf=1) {
  nClass0 = length(which(class == 0))
  nClass1 = length(which(class == 1))
  
  if(nrow(matrix) < nmin || nClass0 == 0 || nClass1 == 0){
    return(list(nClass0, nClass1))
  }

  bestR = -1
  bestS = NULL
  bestI = NULL

  for (i in 1:ncol(matrix)) {
    result = tree.bestsplit(matrix[,i], class, minleaf)
    if(result[2] > bestR) {
      bestR = result[2]
      bestS = result[1]
      bestI = i
    }
  }

  if(is.null(bestI)){
    return(list(nClass0, nClass1))
  }

  left = matrix[matrix[,bestI] <= bestS,]
  leftClass = class[matrix[,bestI] <= bestS]

  right = matrix[matrix[,bestI] > bestS,]
  rightClass = class[matrix[,bestI] > bestS]

  leftG = tree.grow(left, leftClass, nmin, minleaf)
  rightG = tree.grow(right, rightClass, nmin, minleaf)

  return(
    list(nClass0, nClass1, bestI, bestS, leftG, rightG)
  )
}

#
# @xs Array<Array<Integer>>
# @tr A custom tree
#
tree.classify = function(cases, tr) {
  classes = c()
  for (i in 1:(nrow(cases))) {
    classes[i] = tree.calcClassify(cases[i,], tr)
  }
  return(classes)
}

tree.calcClassify = function(case, tr) {
  if(tree.isNode(tr)){
    bestSplit = case[tr[[3]]]

    # Walk left
    if(bestSplit <= tr[[4]]){
      return(tree.calcClassify(case, tr[[5]]))
    # Walk right
    } else {
      return(tree.calcClassify(case, tr[[6]]))
    }
  } else if(tree.isLeaf(tr)){
    if (tr[[2]] > tr[[1]]) {
      return(1)
    } else {
      return(0)
    }
  }
}

tree.calcMinErrorRate = function(lowNmin, maxNmin, lowMinleaf, maxMinLeaf, data) {
  print(sprintf("%d calculations", (lowNmin - maxNmin) * (lowMinleaf - maxMinLeaf)))
  
  lowestErrorRate = Inf
  bestNmin = NULL
  bestMinleaf = NULL
  for (nmin in lowNmin:maxNmin) {
    for (minleaf in lowMinleaf:maxMinLeaf) {
      cat(".")
      currentLowestErrorRate = tree.errorRate(nmin, minleaf, data)
      if(currentLowestErrorRate < lowestErrorRate) {
        bestNmin = nmin
        bestMinleaf = minleaf
        lowestErrorRate = currentLowestErrorRate
      }
    }
  }

  return(list(bestNmin, bestMinleaf, lowestErrorRate))
}

tree.errorRate = function(nmin, minleaf, data) {
    x = ncol(data)
    classes = data[,x]
    tree   = tree.grow(data[,1:(x - 1)], classes, nmin, minleaf)
    classes_ = tree.classify(data[,1:(x - 1)], tree)
    return(100 - 100 * (length(classes[(classes == classes_) == TRUE]) / length(classes)))
}

tree.pimaConfusion = function(nmin = 20, minleaf = 5) {
  data = read.csv('pima.txt', header = FALSE)
  classes = data[,9]
  tree   = tree.grow(data[,1:8], classes, nmin, minleaf)
  classes_ = tree.classify(data[,1:8], tree)

  matrix = matrix(c(
    length(classes[classes == 0 & classes_ == 0]),
    length(classes[classes == 1 & classes_ == 0]),
    length(classes[classes == 0 & classes_ == 1]),
    length(classes[classes == 1 & classes_ == 1])
  ), 2)
  return(matrix)
}

tree.main = function() {
  matrix = read.csv('credit.txt')
  small = tree.grow(matrix[,1:5], matrix[,6])

  # return(small)

  matrix = read.csv('pima.txt', header = FALSE)
  large = tree.grow(matrix[,1:8], matrix[,9])

  return(large)
  return(c(small, large))
}

tree.isNode = function(tr) {
  return(length(tr) == 6)
}

tree.isLeaf = function(tr) {
  return(length(tr) == 2)
}

tree.print = function(node, level = 0) {
  indent = sprintf(paste0("%", level, "s"), "")
  if(tree.isNode(node)) {
    cat(indent, sprintf("Node: (%s|%s) { bestI: %s, bestS: %s }", node[1], node[2], node[3], node[4]), "\n")
    tree.print(node[[5]], level + 2)
    tree.print(node[[6]], level + 2)
  } else {
    cat(indent, sprintf("Leaf: (%s|%s)", node[1], node[2]), "\n")
  }

}

tree.impurity = function (classes) {
  (
    sum(classes) / length(classes)
  ) * (
    length(classes) - sum(classes)
  ) / length(classes)
}

#
# @x List<Integer> A list of values
# @y List<Integer> A list of binary classes
#
tree.main = function() {
  matrix = read.csv('credit.txt')
  small = tree.grow(matrix[,1:5], matrix[,6])

  matrix = read.csv('pima.txt')
  large = tree.grow(matrix[,1:8], matrix[,9])

  return(large)#(c(small, large))[[2]]
}

tree.createLeaf = function(class) {
  return(round((sum(class) / length(class)) + 0.01))
}

tree.pimaConfusion = function(nmin = 20, minleaf = 5) {
  data = read.csv('pima.txt', header = FALSE)
  classes = data[,9]
  tree   = tree.grow(data[,1:8], classes, nmin, minleaf)
  classes_ = tree.classify(data[,1:8], tree)

  matrix = matrix(c(
    length(classes[classes == 0 & classes_ == 0]),
    length(classes[classes == 1 & classes_ == 0]),
    length(classes[classes == 0 & classes_ == 1]),
    length(classes[classes == 1 & classes_ == 1])
  ), 2)
  return(matrix)
}

tree.bestsplit = function (x, y, minleaf) {
  x_ <- x[order(x)]
  y_ <- y[order(x)]

  bestReduction = -1
  bestSplit = 0 # "Should be" NULL

  for (i in 1:(length(x_) - 1))  {
    if (x_[i] == x_[i + 1]) {
      next
    }

    meanValue <- mean(x_[i]:x_[i + 1])
    left = y_[x_ <= meanValue]
    right = y_[x_ > meanValue]
    
    if (length(left) < minleaf || length(right) < minleaf) {
      next
    }
    
    piLeft <- length(left)/length(x_)
    piRight <- length(right)/length(x_)

    currentReduction = 
      tree.impurity(y) - 
      (piLeft * tree.impurity(left) + piRight * tree.impurity(right))

    if(currentReduction > bestReduction){
      bestReduction = currentReduction
      bestSplit = meanValue
    }
  }

  return(c(bestSplit, bestReduction))
}
