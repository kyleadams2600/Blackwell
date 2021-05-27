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
    y = theta + rnorm(2^l,0,sigma)
    #ans = dyadic_1d(l,y,lambda[l])[[2]]
    ans = dyadic_1d(l,y,lambda)[[2]]
    mse[j] = mean((theta - ans)^2)
    }
    return(mse)
    }
}
  ####################################
  
#####Algorithm for Two-Fold Cross Validation----
#Step 1: Take odd observations from list of data points
#Step 2: Create a new data vector of these that looks like y1, y1+y3/2, y3, ...
#Step 3: Apply dyadic CART algorithm w grid of lambda in {1, 2 , 2^2, ..., 2^(log(n))}
#Step 4: Now we should have log(n) theta hats each with a different lambda
#Step 5: Now we want to minimize "prediction error" = [sum of even observations](yi - theta hat, lambdaj,i)
#Step 6: Now the final fit for the even observations is going to be theta hat (lambda hat odd) @ even observations
#Step 7: Reverse even and odd
#Step 8: Combine thetas to have estimate

#Made for loop for step 1
listexample = c(1,2,3,4,5,6,7,8)
newlist = list()
count = 1

for (i in 1:(length(listexample)/2)-1){
  newlist[i] = listexample[count]
  count = count + 2
}

count2 = 1
averagelist = list()
for (i in 1:length(listexample)) {
  if (i %% 2 == 1) {
    averagelist[i] = newlist[count]
  }
  else {
    averagelist[i] = sum(newlist[count],newlist[count+1])/2
  }
    ###how the heck do you add elements of a list together!!
}

  
