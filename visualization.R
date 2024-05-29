# Libraries
library(ggplot2)
library(ggthemes)
library(gridExtra)
library(lattice)

# node mode (each node's mode, in total 264 nodes)
load(url("https://raw.githubusercontent.com/Emory-CBIS/dynaLOCUS_tutorial_data/main/node.mode.RData"))
modID = data.frame(id = 0:10, modname = c("Other","med vis","op vis","lat vis","DMN","CB","SM","Aud","EC","FPR","FPL"))
# Note: UC stands for 'uncertain'

# change order
# 1-3 6 7 4 8 10  9 5  0 orig
# 1-3 4 5 6 7  8  9 10 11 new
modname.new = c("med vis","op vis","lat vis","SM","Aud","DMN","EC","FPL","FPR","CB","Other")
ids = sapply(c(0, 6, 7, 4, 8, 10, 5), match, x = node.mode, nomatch = 0)
for (i in 5:8) {
  node.mode[which(ids[, i-3] == 1)] = i-1
}
node.mode[which(ids[, 1] == 1)] = 11
node.mode[which(ids[, 6] == 1)] = 8
node.mode[which(ids[, 7] == 1)] = 10

# count the number of nodes <=i (sum up each mode)
k = 0
grid.mode = vector()
count.mode = table(node.mode)
for (i in 1:(length(count.mode)-1)) {   
  k = k + count.mode[i]
  grid.mode[i] = k
}
V = 264

loc = (c(grid.mode, V) + c(0, grid.mode)) / 2

# plot the latent source based on source-specific color bar
re.mod = function(Mat) {
  Mat = as.matrix(Mat)
  return(Mat[order(node.mode), order(node.mode)])  # Determines S's layout
}
Ltrans = function(X, d = TRUE) { 
  X[upper.tri(X, d)]  
}
Ltrinv = function(x, V, d = TRUE) { 
  Y = matrix(0, ncol = V, nrow = V)
  Y[upper.tri(Y, d)] = x
  return(Y + t(Y) - d * diag(diag(Y)))  
}
levelplotX = function(X, maint = "", p = 0.99, colbar = FALSE, li = NULL) {
  if (is.null(li)) {
    li = quantile(abs(X), probs = p)
  }
  p1 = levelplot(re.mod(Ltrinv(X, V, FALSE)),
                  col.regions = colorRampPalette(c("blue", "white", "red"))(1e3),
                  at = c(-Inf, do.breaks(c(-li, li), 100), Inf), 
                  panel = function(...) {
                    panel.levelplot(...)
                    panel.abline(v = grid.mode + 1, col = "black")
                    panel.abline(h = grid.mode + 1, col = "black")
                  },
                  scales = list(x = list(rot = 65, at = loc + 1, labels = modname.new, cex = 1.05),
                                y = list(at = loc + 1, labels = modname.new, cex = 1.05)),
                  xlab = "", ylab = "", main = maint, colorkey = colbar,
                  par.settings = list(axis.text = list(fontfamily = "Arial"),
                                      par.main.text = list(fontfamily = "Arial")))
  return(p1)
}