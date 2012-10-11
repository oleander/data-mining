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
      return(tree.calcClassify(case, tr[[4]]))
    }
  } else if(tree.isLeaf(tr)){
    if (tr[[1]] > tr[[2]]) {
      return(0)
    } else {
      return(1)
    }
  }
}

tree.isNode = function(tr) {
  return(length(tr) == 4)
}

tree.isLeaf = function(tr) {
  return(class(tr) == "numeric")
}

tree.grow = function(matrix, class, nmin=2, minleaf=1) {
  if(nrow(matrix) < nmin){
    return(tree.createLeaf(class))
  }

  bestReduction = -1
  bestSplit = NULL
  bestAttributeIndex = NULL
  for (i in 1:ncol(matrix)) {
    result = tree.bestsplit(matrix[,i], class)
    if(result[2] > bestReduction) {
      bestReduction = result[2]
      bestSplit = result[1]
      bestAttributeIndex = i
    }
  }

  left = matrix[matrix[,bestAttributeIndex] <= bestSplit,]
  leftClass = class[matrix[,bestAttributeIndex] <= bestSplit]

  right = matrix[matrix[,bestAttributeIndex] > bestSplit,]
  rightClass = class[matrix[,bestAttributeIndex] > bestSplit]

  if(nrow(left) < minleaf || nrow(right) < minleaf) {
    return(tree.createLeaf(class))
  }

  leftG = tree.grow(left, leftClass, nmin, minleaf)
  rightG = tree.grow(right, rightClass, nmin, minleaf)

  return(
    list(bestAttributeIndex, bestSplit, leftG, rightG)
  )
}

tree.print = function(node, level = 0) {
  
  indent = sprintf(paste0("%", level, "s"), "")
  
  if(tree.isNode(node)) {
    # print(node)
    cat(indent, sprintf("Node: { bestI: %s, bestS: %s }", node[1], node[2]), "\n")
    tree.print(node[[3]], level + 2)
    tree.print(node[[4]], level + 2)
  } else {
    # print(length(node))
    cat(indent, sprintf("Leaf: %s", node), "\n")

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

  return(c(small, large))
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

tree.bestsplit = function (x, y) {
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
