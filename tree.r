
tree.classify = function(x, tr) {

}

tree.grow = function(x, y, nmin, minleaf) {

}



tree.impurity = function (v) {
  sum(v) / length(v) * (length(v) - sum(v)) / length(v)
}

tree.bestsplit = function (x, y) {
	x_ = x[order(x)]
	y_ = y[order(x)]
	
	for (a in unique(x)) {
		
		left = y_[x_ < a]
		right = y_[x_ >= a]

		impurity(left)
		
		impurity(right)
		
	}
}
