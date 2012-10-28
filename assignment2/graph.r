#      [,1] [,2] [,3] [,4]
# [1,]    0    1    1    0
# [2,]    1    0    1    0
# [3,]    1    1    0    1
# [4,]    0    0    1    0

# graph.init = matrix(c(
#   0,1,1,0,
#   1,0,1,0,
#   1,1,0,1,
#   0,0,1,0
# ), 4, 4)

#      [,1] [,2] [,3] [,4] [,5]
# [1,]    0    1    1    0    1
# [2,]    1    0    0    1    0
# [3,]    1    0    0    1    1
# [4,]    0    1    1    0    0
# [5,]    1    0    1    0    0
graph.init = matrix(c(
  0,1,1,0,1,1,0,0,1,0,1,0,0,1,1,0,1,1,0,0,1,0,1,0,0
), 5, 5)

gm.search = function(observed, graph.init, forward, backward, scoreType){
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
      cat(sprintf("%s: %s - %s (score = %f)\n", newModelResult$what, result$v1, result$v2, result$score))
    } else {
      break
    }
  }

  cat(sprintf("\n\tTotal score %s\n\n", score))  
  print(model)
  cat(sprintf("----------------------------------------\n\n"))
  
  return(list(model = gm.calcCliques(model), score = score, call = match.call())) 
}

gm.toggleV = function(model, i, j) {
  if(model[i, j] == 1){
    model[i, j] = 0
    model[j, i] = 0
    what = "Removed"
  } else {
    model[i, j] = 1
    model[j, i] = 1
    what = "Added"
  }

  return(list(model = model, what = what))
}

gm.createRandomMatrix = function(size, prob){
  
  mat = matrix(0, size, size)
  
#   for (i in 1:(size - 1)){    
#     for (j in (i + 1):size){
#       mat[i,j] = rbinom(1, 1, prob)
#       mat[j,i] = mat[i,j]      
#     }
#   }
  
  for (i in 1:(size - 1)){
    j = i + 1
    mat[i,j:size] = rbinom(size - i, 1, prob)
    mat[j:size,i] = mat[i,j:size]    
  }
       
  return(mat) 
}

gm.findBestN = function(model, observed, scoreType, forward, backward) {
  bestScore = gm.score(model, observed, scoreType)
  bestV1 = NULL
  bestV2 = NULL
  numberOfThings = ncol(model)

  for (i in 1:numberOfThings) {
    for (j in i:numberOfThings) {
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


gm.restart = function(nstart, prob, seed, observed, forward, backward, scoreType){
  if(!missing(seed)){
    set.seed(seed)
  }
  
  bestScore = Inf
  bestModel = NULL
  
  for (i in 1:nstart){
    graph = gm.createRandomMatrix(ncol(observed), prob)
    result = gm.search(observed, graph, forward, backward, scoreType)
    
    if(result$score < bestScore){
      bestScore = result$score
      bestModel = result$model
    }
  }
  
  return(list(model = bestModel, score = bestScore, call = match.call()))
}

gm.score = function(model, observed, scoreType){
  cliques = gm.calcCliques(model)
  result = loglin(table(observed), cliques, print = F)
  deviance = result$lrt
  noOfParam = 2**nrow(model) - result$df
  
  if(scoreType == "aic"){
    return (deviance + 2 * noOfParam)
  } else if (scoreType == "bic"){
    return (deviance + log(nrow(observed), exp(1)) * noOfParam)
  } else {
    return -1
  }
}

gm.calcCliques = function(graph) {
  # find neighbors of vertex v in graph
  neighbors = function(v) {
    which(graph[v,] == 1, arr.in=TRUE)
  }
  
  # BronKerbosch1(R,P,X):
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
      P = P[-1]
      X = union(X, v)
    }
    return(cliques)
  }
  
  bk(P = 1:nrow(graph))
}