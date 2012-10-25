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
  cliques = bk(graph.init)
  deviance = loglin(observed, cliques)[1]
  
  score(graph.init, deviance)
  
  hillClimb(observed, graph.init, forward, backward, scoreType)
  
  
  bestNeighbour = NULL

    
    bestScore = NULL
    for (v1 in 1:nrow(observed)){
      for (v2 in 1:(v1 - 1)){
        evalModel = model  
        if(model[vertex1, vertex2] == 1 && backward){
          
          evalModel[vertex1, vertex2] == 0
          score(evalModel, )
          
        } else if (model[vertex1, vertex2] == 0 && forward){
          evalModel[vertex1, vertex2] == 1
          score(evalModel, )
          
        }
      }
    }
    
    
  
   
}

gm.hillClimb = function(scoreValue, model, forward, backward, scoreType  ){
  
}





gm.restart = function(nstart, prob, seed, observed, graph.init, forward, backward, score){
  
}

gm.aic = function(model, deviance){
  deviance + 2 * nrow(model)
}

gm.bic = function(model, deviance, observed){
  deviance + log(nrow(observed), exp(1)) * nrow(model)
}

graph.result = list()


n = function(v, matrix) {
  which(matrix[v,] == 1, arr.in=TRUE)
}

calcC = function(matrix) {
  bk(c(), 1:nrow(matrix), c(), matrix)
}

# BronKerbosch1(R,P,X):
#     if P and X are both empty:
#         report R as a maximal clique
#     choose a pivot vertex u in P ⋃ X
#     for each vertex v in P \ N(u):
#         BronKerbosch1(R ⋃ {v}, P ⋂ N(v), X ⋂ N(v))
#         P := P \ {v}
#         X := X ⋃ {v}
bk = function(R = c(),P,X = c(), matrix) {
  if(length(P) == 0 && length(X) == 0){
    return(list(R))
  }
  cliques = list()
  u = union(P, X)[1]
  for (v in setdiff(P,n(u, matrix))) {
    cliques = c(cliques, bk(union(R, v), intersect(P, n(v, matrix)), intersect(X, n(v, matrix))))
    P = P[-1]
    X = union(X, v)
  }
  return(cliques)
}