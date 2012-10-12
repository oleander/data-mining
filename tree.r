#
# Creates a classification tree based on ingoing matrix
#
# @matrix List<List<Integer>> Matrix to grow a tree from
# @class List<Integer> A list of binary classes
# @nmin Integer Number of observations that a node must contain for it to be split
# @minleaf Integer Minimum amount of leafs in tree
# @return Tree A custom tree
#
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
# Classifies ingoing cases
#
# @cases List<List<Integer>> A list of items to be classified
# @tr Tree A custom tree
# @return List<Integer> A list of binary classes
#
tree.classify = function(cases, tr) {
  classes = c()
  for (i in 1:(nrow(cases))) {
    classes[i] = tree.calcClassify(cases[i,], tr)
  }
  return(classes)
}

#
# Recursively classifies one single case
#
# @case List<Integer> An item to be classified
# @tr Tree A custom tree
# @return Integer A binary class, 0 or 1
#
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

#
# Uses bruteforce to calculate the best values for nMin and minLeaft
#
# @testFactor Float How many percent of the pima data should be used for testing? [0, 1]
# @return Matrix See example below
#
#           1        3        5        7        9       11       13       15       17       19
# 2  28.12500 27.08333 29.16667 22.91667 20.31250 21.35417 23.95833 25.52083 23.43750 23.43750
# 9  27.60417 27.60417 29.16667 22.91667 20.31250 21.35417 23.95833 25.52083 23.43750 23.43750
# 16 23.95833 24.47917 26.04167 23.43750 20.31250 21.35417 23.95833 25.52083 23.43750 23.43750
# 23 23.95833 23.43750 22.91667 21.87500 20.83333 21.35417 23.95833 25.52083 23.43750 23.43750
# 30 26.56250 26.56250 26.04167 26.56250 26.04167 25.00000 23.43750 25.52083 23.43750 23.43750
# 37 21.87500 21.87500 21.35417 21.35417 21.35417 21.35417 23.43750 23.95833 23.43750 23.43750
# 44 21.87500 21.87500 21.35417 21.35417 21.35417 21.35417 23.43750 23.43750 23.43750 23.43750
# 51 24.47917 24.47917 23.95833 23.95833 23.95833 23.95833 23.95833 23.95833 23.95833 23.95833
# 58 23.95833 23.95833 23.95833 23.95833 23.95833 23.95833 23.95833 23.95833 23.95833 23.95833
#
tree.analyse = function(testFactor = 0.25) {
  data = read.csv('pima.txt', header = FALSE)
  indexes = 1 : nrow(data)
  testIndexes = sample(indexes, testFactor * nrow(data))
  trainingIndexes = indexes[-testIndexes]
  
  nMins = seq(2, 10, 1)
  minLeafs = seq(1, 50, 3)

  result = matrix(nrow = length(nMins), ncol = length(minLeafs), dimnames = list(nMins, minLeafs))
  
  bestErrorRate = Inf
  bestNmin = NULL
  bestMinleaf = NULL

  x = c()
  y = c()
  z = c()
  cc = 0
  for (i in 1:length(nMins)) {
    nmin = nMins[i]
    for (j in 1:length(minLeafs)) {
      minLeaf = minLeafs[j]
      errorRate = tree.errorRate(nmin, minLeaf, data[trainingIndexes,], data[testIndexes,])
      result[i,j] = errorRate

      x[cc] = minLeaf
      y[cc] = nmin
      z[cc] = errorRate

      if(errorRate < bestErrorRate){
        bestErrorRate = errorRate
        bestNmin = nmin
        bestMinleaf = minLeaf
      }

      cat(".")
      cc = cc + 1
    }
  }
  cat("\n")

  print(sprintf("nMin: %f, minLeaf: %f, errorRate: %f", bestNmin, bestMinleaf, bestErrorRate))

  planes3d(x,y,z)
  plot3d(x,y,z, zlab="errorRate", xlab="minLeaf", ylab="nmin")

  return(result)
}

tree.sample = function(nmin, minLeaf, testFactor = 0.25, iterations = 100) {
  data = read.csv('pima.txt', header = FALSE)
  indexes = 1 : nrow(data)
  results = c()
  
  for (i in 1 : iterations) {
    testIndexes = sample(indexes, testFactor * nrow(data))
    trainingIndexes = indexes[-testIndexes]
    errorRate = tree.errorRate(nmin, minLeaf, data[trainingIndexes,], data[testIndexes,])
    results[i] = errorRate
    cat(".")
  }
  cat("\n")
  
  return(results)
}

#
# Calculates the error rate based on training (@trainingData) and test (@testData) data
#
# @nmin Integer Number of observations that a node must contain for it to be split
# @minleaf Integer Minimum amount of leafs in tree
# @trainingData Matrix Data used for building the classification tree
# @testData Matrix Data to used for comparison
# @return Float Error rate in percent [0, 100]
#
tree.errorRate = function(nmin, minleaf, trainingData, testData) {
    x = ncol(trainingData)
    tree   = tree.grow(trainingData[,1:(x - 1)], trainingData[,x], nmin, minleaf)
    classes = testData[,x]
    classes_ = tree.classify(testData[,1:(x - 1)], tree)
    return(100 - 100 * (length(classes[(classes == classes_) == TRUE]) / length(classes)))
}

#
# A visual presentation of the error rate using a matrix as output
#
# @nmin Integer Number of observations that a node must contain for it to be split
# @minleaf Integer Minimum amount of leafs in tree
# @return Matrix See example below
#
#      [,1] [,2]
# [1,]  444   56
# [2,]   54  214 
#
tree.pimaConfusion = function(nmin = 2, minleaf = 1) {
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

#
# Is the given @item a node?
#
# @item Node || Leaf
# @return Boolean Is the @item a node?
#
tree.isNode = function(item) {
  return(length(item) == 6)
}

#
# Is the given @item a leaf?
#
# @item Node || Leaf
# @return Boolean Is the @item a leaf?
#
tree.isLeaf = function(item) {
  return(length(item) == 2)
}

#
# Prints ingoing @node in a readable format
#
# @node Leaf || Node Item to be printed
# @level Integer The amount of indentations used for each hierarchy
#
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

#
# Calculates impurity
#
# @classes List<Integer> A list of classes
# @return Integer The impurity
#
tree.impurity = function(classes) {
  (
    sum(classes) / length(classes)
  ) * (
    length(classes) - sum(classes)
  ) / length(classes)
}

#
# Calculates the best split value
#
# @values List<Integer> A list of values
# @classes List<Integer> A list of binary classes
# @minleaf Integer Minimum amount of leafs in tree
# @return List<Integer, Integer>
#  bestSplit What value should one split on?
#  bestReduction How much was the reduction?
#
tree.bestsplit = function (values, classes, minleaf) {
  x_ <- values[order(values)]
  y_ <- classes[order(values)]

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
