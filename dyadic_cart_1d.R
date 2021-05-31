#n = 2^l, the number of leaves of the binary tree.
#depth of the tree is l + 1. Root is at depth 1. 
#I will encode a node by its depth and position from left to right. 
#At depth i there are 2^{i - 1} nodes. 
#Node (i,j) represents the interval [n/(2^{i - 1})]*(j - 1) + 1 to n/(2^{i - 1})*j.


###Bottom up pass#########
#initialization
###opt,split,sum,sumsq,size are all lists indexed by i the depth of the tree.
###For each i they are a vector.
dyadic_1d = function(l,y,lambda){
n = 2^l
opt = list()
split = list()
sum = list()
sumsq = list()
size = list()
var = list()
partition = list()
##partition is a list of lists. Each vertex has a list which denotes the optimal
##partition of that vertex. A partition is denoted by a list of two tuples 
##representing the end points of the intervals.
for (i in 1:l){
  opt[[i]] = rep(0,2^{i - 1})
  split[[i]] = rep(0,2^{i - 1})
  sum[[i]] = rep(0,2^{i - 1})
  sumsq[[i]] = rep(0,2^{i - 1})
  var[[i]] = rep(0,2^{i - 1})
  depth = l + 2 - i
  size[[i]] = rep(2^{l-i+1},2^{i - 1})
  }
  

opt[[l + 1]] = rep(lambda,2^{l})
  split[[l + 1]] = rep(0,2^{l})
  sum[[l + 1]] = y
  sumsq[[l + 1]] = y^2
  size[[l + 1]] = rep(1,n)
  var[[l + 1]] = rep(0,n)
  
##defining the current list. It is a list of lists. 
currentlist = list()
for (i in 1:n){
currentlist[[i]] = list(c(i,i))
#currentlist[[i]][1] = c(i,i)
}
####Figure out how to remove for loop here. Or merge this with the bottom
##up pass.

##bottom up pass
for (i in 2:(l + 1)){
  depth = l + 2 - i
  templist = currentlist
  currentlist = NULL
  currentlist = list()
  for (j in 1:(2^{depth - 1})){
    childdep = depth + 1
    lchild = 2*(j - 1) + 1
    rchild = 2*(j - 1) + 2
    sum[[depth]][j] = sum[[depth + 1]][lchild] + sum[[depth + 1]][rchild]
    sumsq[[depth]][j] = sumsq[[depth + 1]][lchild] + sumsq[[depth + 1]][rchild]
    var[[depth]][j] = sumsq[[depth]][j] - (sum[[depth]][j])^2/size[[depth]][j]
    temp = opt[[depth + 1]][lchild] + opt[[depth + 1]][rchild]
    opt[[depth]][j] = min(var[[depth]][j] + lambda,temp)
    currentlist[[j]] = list()
    currentlist[[j]][[1]] = c((n/(2^{depth - 1}))*(j - 1) + 1,(n/(2^{depth - 1}))*(j)) 
    if (var[[depth]][j] + lambda > temp){
    split[[depth]][j] = 1
    #currentlist[[j]][[1]] = templist[[2*(j - 1) + 1]]
    #currentlist[[2]] = templist[[2*(j - 1) + 2]]
    currentlist[[j]] = c(templist[[2*(j - 1) + 1]],templist[[2*(j - 1) + 2]])
    }
    }
}
finalpart = currentlist[[1]]
output = rep(0,n)
for (i in 1:length(finalpart)){
  output[finalpart[[i]][1]:finalpart[[i]][2]] = mean(y[finalpart[[i]][1]:finalpart[[i]][2]])
}
return(list(finalpart,output))
}
#########################Bottom up pass done######################
##Main thing: The optimal partition is being computed bottom up. 
##Every step of the for loop creates the optimal partition at each node.
##The optimal partition is encoded via a list. The list is a list of two
##tuples.
##################MSE experiment###########



ind = function(x,a,b){
  if (x > a && x <= b){
    return(1)
    }
  else {
    return(0)
  }
}

f = function(x){
  return(ind(x,0,0.4))
}


f2 = function(x){
  a1 = 2*ind(x,0.2,0.4)
  a2 = 4*ind(x,0.4,0.6)
  a3 = ind(x,0.6,0.8)
  a4 = 4*ind(x,0.8,1)
  return(a1 + a2 + a3 + a4)
}

mse = function(iter){
  grid = seq(5,20,by = 1)
  lambda = 
  for (l in grid){
    n = 2^l
    theta = sapply(seq(1:n)/n,f2)
    for (j in 1:iter){
    y = theta + rnorm(2^l,0,sigma) #theta is signal, rnorm is noise
    #ans = dyadic_1d(l,y,lambda[l])[[2]]
    ans = dyadic_1d(l,y,lambda)[[2]]
    mse[j] = mean((theta - ans)^2)
    }
    return(mse)
    }
}
  ####################################
  

#playing with dyadic cart----
#step 1: run code
#step 2: set l, n

#function that spits out theta values for each lambda
create_grid = function(l, y) { #l is used for n = 2^l in dyadic_1d, y is function
  
  n = 2^l
  lambda_grid = vector(mode = "integer", length = log(n))
  theta_vector = list()
  
  for (i in 0:log(n)) {
    lambda_grid[i+1] = 2^i #adds power of 2 as a lambda value in grid
  }
  
  for (i in 1:n) {
    theta_vector[i] = dyadic_1d(l, y, lambda_grid[i]) #creates vector of theta values
  }
  
  return(lambda_grid)
  
}

#function that minimizes prediction error
minimize_pe = function(y, theta_hat) {
  
  n = length(y)
  k = 
  even_obs = vector(mode = "integer", length = floor(n/2))
  pe_list = vector(mode = "integer", length = k)
  
  for (i in 1:n) { # create vector containing even indexed observations of y
    for (j in 1:floor(n/2)) {
      if (i %% 2 == 0) {
        even_obs[j] = y[i]
      }
    }
  }
  
  for (i in 1:n) {
    pe_list[i] = (y[i] - )
  }
}