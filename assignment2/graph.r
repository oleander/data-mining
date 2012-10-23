#      [,1] [,2] [,3] [,4]
# [1,]    0    1    1    0
# [2,]    1    0    1    0
# [3,]    1    1    0    1
# [4,]    0    0    1    0

graph.init = matrix(c(0,1,1,0,1,0,1,0,1,1,0,1,0,0,1,0), 4, 4)

n = function(v) {
  return(which(graph.init[v,] == 1, arr.in=TRUE))
}

# BronKerbosch1(R,P,X):
#     if P and X are both empty:
#         report R as a maximal clique
#     for each vertex v in P:
#         BronKerbosch1(R ⋃ {v}, P ⋂ N(v), X ⋂ N(v))
#         P := P \ {v}
#         X := X ⋃ {v}
bk = function(R,P,X) {
  print(P)
  print(X)
  if(nrow(P) == 0 && length(X) == 0){
    return(X)
  }
  
  for (v in 1:nrow(P)) {
    # bk(c(R, v), intersect(P, n(v)), intersect(X, n(v)))
    bk(c(R, v), P[n(v)-v,], intersect(X, n(v)))
    P = P[-1,]
    X = c(C, v)
  }
}