#
# Creates the best possible model (cliques) based on ingoing arguments
#
# @observed Table Observed data
# @graph.init List<List<Integer>> A matrix representing the graph
# @forward Boolean (Optional) Are we allowed to add edges?
# @backward Boolean (Optional) Are we allowed to remove edges?
# @scoreType String<"bic", "aic"> What algorithm should be used to calculate the score?
# @return$model List<List<Integer>> A list containing the cliques of the final model
# @return$score Float The AIC or BIC score of the final model
# @return$call The call to the function gm.search that produced this result
#
gm.search = function(observed, graph.init, forward = T, backward = T, scoreType){
  model = graph.init
  score = Inf
  while(T) {
    result = gm.findBestN(model, observed, scoreType, forward, backward)
    if(result$score < score){
      score = result$score
      # Model hasn't been changed
      if(is.null(result$v1) || is.null(result$v2)){ break }

      newModelResult = gm.toggleV(model, result$v1, result$v2)
      model = newModelResult$model
      what = newModelResult$what
      if(newModelResult$what == 1){
        what = "Added"
      } else {
        what = "Removed"
      }

      cat(sprintf("%s: %s - %s (score = %f)\n", what, result$v1, result$v2, result$score))
    } else {
      break
    }
  }
  
  cliques = gm.calcCliques(model)

  cat(sprintf("\n\tTotal score %s\n\n", score))  
  print(model)
  cat(sprintf("----------------------------------------\n\n"))
  cat("\n\tCliques:\n\n")
  for( c in cliques) {
    cat(sprintf("%s\n", paste(c, collapse = " - ")))
  }
  
  return(list(model = cliques, score = score, call = match.call(), graph = model)) 
}

#
# Toggles (i, j) and (j, i) in @model
#
# @model List<List<Integer>> A matrix representing the graph
# @i Integer Coordinate
# @j Integer Coordinate
# @return$model List<List<Integer>> Model with (i, j) and (j, i) toggled
# @return$what Integer
#   == 1 An edge was added
#   == 0 An edge was removed
#
gm.toggleV = function(model, i, j) {
  if(model[i, j] == 1){
    model[i, j] = 0
    model[j, i] = 0
    what = 0 # Removed
  } else {
    model[i, j] = 1
    model[j, i] = 1
    what = 1 # Added
  }

  return(list(model = model, what = what))
}

#
# Creates a random matrix with the given dimensions 
#
# @size Integer Height and width of matrix
# @prob Float Specifies the probability of an edge between any pair of nodes
# @return List<List<Integer>> A newly generated matrix
#
gm.createRandomMatrix = function(size, prob){
  mat = matrix(0, size, size)
  for (i in 1:(size - 1)){
    j = i + 1
    mat[i,j:size] = rbinom(size - i, 1, prob)
    mat[j:size,i] = mat[i,j:size]    
  }
       
  return(mat) 
}

#
# What changes should be made to @model based on @observed to get the best score?
#
# @model List<List<Integer>> A matrix representing the graph
# @observed Table Observed data
# @scoreType String<"bic", "aic"> What algorithm should be used to calculate the score?
# @forward Boolean Are we allowed to add edges?
# @backward Boolean Are we allowed to remove edges?
# @return$score Float Score for the given @model with new node @return$v1, @return$v2
# @return$v1, @return$v2 Integer<0,1> New node in @model
#
gm.findBestN = function(model, observed, scoreType, forward, backward) {
  bestScore = gm.score(model, observed, scoreType)
  bestV1 = NULL
  bestV2 = NULL
  numberOfThings = ncol(model)

  for (i in 1:(numberOfThings - 1)) {
    for (j in (i + 1):numberOfThings) {
      if(model[i, j] == 1 && !backward){ next } 
      if(model[i, j] == 0 && !forward){ next } 

      model = gm.toggleV(model, i, j)$model

      currentScore = gm.score(model, observed, scoreType)
      if(currentScore < bestScore) {
        bestScore = currentScore
        bestV1 = i
        bestV2 = j
      }

      model = gm.toggleV(model, i, j)$model
    }
  }
  return(list(score = bestScore, v1 = bestV1, v2 = bestV2))
}

#
# Use random restarts from diffrent initial models
#
# @nstart Integer The number of restarts to be performed
# @prob Float Specifies the probability of an edge between any pair of nodes
# @seed Integer (Optional) A random seed so the results can be reproduced
# @observed Table Observed data
# @forward Boolean Are we allowed to add edges?
# @backward Boolean Are we allowed to remove edges?
# @scoreType String<"bic", "aic"> What algorithm should be used to calculate the score?
# @return$model List<List<Integer>> Best found model
# @return$score Float Score for the given model
# @return$call the call to the function gm.search that produced this result. 
#
gm.restart = function(nstart, prob, seed, observed, forward, backward, scoreType){
  if(!missing(seed)){
    set.seed(seed)
  }
  
  size = summary(observed)$n.vars
  
  bestScore = Inf
  bestModel = NULL
  
  for (i in 1:nstart){
    graph = gm.createRandomMatrix(size, prob)
    result = gm.search(observed, graph, forward, backward, scoreType)
    
    if(result$score < bestScore){
      bestScore = result$score
      bestModel = result$model
    }
  }
  
  return(list(model = bestModel, score = bestScore, call = match.call()))
}

#
# Calculates the score for a given @model
#
# @model List<List<Integer>> A matrix representing the graph
# @observed Table Observed data to base the score on
# @scoreType String<"bic", "aic"> What algorithm should be used to calculate the score?
# @return Float The AIC or BIC score of the @model
#
gm.score = function(model, observed, scoreType){
  if(scoreType != "aic" && scoreType != "bic"){
    stop("scoreType must equal 'aic' or 'bic'")
  }

  cliques = gm.calcCliques(model)
  result = loglin(observed, cliques, print = F, iter = 40)
  deviance = result$lrt
  noOfCases = summary(observed)$n.cases
  noOfParam = 2**nrow(model) - result$df
  
  if(scoreType == "aic"){
    return (deviance + 2 * noOfParam)
  } else {
    return (deviance + log(noOfCases, exp(1)) * noOfParam)
  }
}

#
# Generates a list of cliques based on ingoing @graph
# It uses the BronKerbosch2 algorithm with pivot from Wikipedia
#
# @graph List<List<Integer>> A Graph
# @return$P List<List<Integer>> A list of cliques
#
gm.calcCliques = function(graph) {
  # find neighbors of vertex v in graph
  neighbors = function(v) {
    which(graph[v,] == 1, arr.in=TRUE)
  }
  
  # BronKerbosch2 (R,P,X):
  #     if P and X are both empty:
  #         report R as a maximal clique
  #     choose a pivot vertex u in P ⋃ X
  #     for each vertex v in P \ N(u):
  #         BronKerbosch1(R ⋃ {v}, P ⋂ N(v), X ⋂ N(v))
  #         P := P \ {v}
  #         X := X ⋃ {v}
  bk = function(R = c(), P, X = c()) {
    if(length(P) == 0 && length(X) == 0){
      return(list(R))
    }
    cliques = list()
    u = union(P, X)[1]
    for (v in setdiff(P, neighbors(u))) {
      cliques = c(cliques, bk(union(R, v), intersect(P, neighbors(v)), intersect(X, neighbors(v))))
      P = setdiff(P, c(v)) # P[-1]
      X = union(X, v)
    }
    return(cliques)
  }
  
  bk(P = 1:nrow(graph))
}

gm.dot = function(matrix) {
  for (i in 1:(nrow(matrix) - 1)) {
    for (j in (i+1):(nrow(matrix))) {
      if (matrix[i,j] == 1) {
        cat(sprintf("%s -- %s;\n", i, j))
      }
    }
  }
}