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

#####Algorithm for Two-Fold Cross Validation----
#Step 1: Take odd observations from list of data points
#Step 2: Create a new data vector of these that looks like y1, y1+y3/2, y3, ...
#Step 3: Apply dyadic CART algorithm w grid of lambda in {1, 2 , 2^2, ..., 2^(log(n))}
#Step 4: Now we should have log(n) theta hats each with a different lambda
#Step 5: Now we want to minimize "prediction error" = [sum of even observations](yi - theta hat, lambdaj,i)
#Step 6: Now the final fit for the even observations is going to be theta hat (lambda hat odd) @ even observations
#Step 7: Reverse even and odd
#Step 8: Combine thetas to have estimate

#Need 4 Functions

# step 1 + step 2 

crossval_odd = function(y) { # y is the list of observations
  
  n = length(y)
  y_odd = vector(mode = "numeric", length = n) # list of odd indexed observations from y
  
  for (i in 1:n) {
    if (i %% 2 != 0) { # odd indexes
      y_odd[i] = y[i]
    } else if (i %% 2 == 0 && i != n) { # even indexes
      y_odd[i] = (y[i-1] + y[i+1])/2 # fill in missing observations with average of neighboring observations
      
    } else if (i == n && i %% 2 == 0) { #if last entry and even length, make last entry an average of the first and second to last observation
      y_odd[i] = (y[1] + y[i-1])/2
    }
  }
  
  return(y_odd)
} 

#Even observations

crossval_even = function(y) { # y is the list of observations
  
  n = length(y)
  y_even = vector(mode = "numeric", length = n) # list of even indexed observations from y
  
  for (i in 1:n) {
    if (i == 1) {
      y_even[i] = (y[i+1] + y[n-1]) / 2
    }
    else {
      if (i %% 2 != 1) { # even indexes
        y_even[i] = y[i]
      } 
      else if (i %% 2 == 1 && i != n) { # even indexes
        y_even[i] = (y[i-1] + y[i+1]) / 2 # fill in odd indexes with average of neighboring observations
      
      } 
      else if (i == n && i %% 2 == 1) { #if last entry and odd length, make last entry an average of the first and second to last observation
        y_even[i] = (y[1] + y[i-1])/2
      }
    }
  }
  
  return(y_even)
} 
  
#HOW MANY LAMBDAS?
  # n = sample size = 2^l
  # lambda grid = {1, 2^1, 2^2, ... , 2^log(n)}
  # so number of lambdas will be log(n) (rounded down to nearest int) + 1

#creating lambda grid
lambdas = vector(mode = "numeric", length = log(n))

for (i in 0:log(n)) {
  lambdas[i+1] = 2^i #adds power of 2 as a lambda value in grid
}

#function that spits out theta values for each lambda
create_theta_vector = function(l, y) {
  # y can be either y_even or y_odd
  n = 2^l
  theta_vector = list()
  
  for (i in 1:length(lambdas)) {
    theta_vector[i] = as.vector(dyadic_1d(l, y, lambdas[i])[2]) #creates vector of theta values
  } #yields error from line 68
  
  return(theta_vector)
}

#function that minimizes prediction error
minimize_pe = function(y, l) { #spits out theta vector with minimum prediction error

  
  k = length(y)
  n = 2^l
  m = as.integer(log(n)) + 1 #length of lambda grid
  
  pe_even = vector(mode = "numeric", length = k) # pred. errors for even observations
  
  for(lambda in 1:m) {
    y_odd = seq(from = 1, to = n, by = 2) #odd observations
    y_odd = y_odd[lambda]
    pe_even[i] = sum(y_odd^2)
  }
  
  theta_odd = create_theta_vector(l, y_odd)
  min_index = which.min(pe_even) # returns index of smallest error
  lambda_odd = lambdas[min_index] # lambda which has the smallest error
  fit_even = vector(mode = "numeric", length = k) # final fit for even observations
  
  for (i in 1:k) {
    fit_even[i] = theta_odd[[lambda_odd]][i]
  }
  
  # repeat process with odd and even switched
  
  pe_odd = vector(mode = "numeric", length = k)
  
  for(lambda in 1:m) {
    y_even = seq(from = 2, to = n, by = 2) #even observations
    y_even = y_even[lambda]
      pe_odd[i] = sum(y_even^2)
  }
  
  theta_even = create_theta_vector(l, y_even)
  min_odd = which.min(pe_odd)
  lambda_even = lambdas[min_odd]
  fit_odd = vector(mode = "numeric", length = k) # final fit for odd observations
  
  for (i in 1:k) {
    odd_fit[i] = theta_even[[lambda_even]][i]
  }
  
  # now just combine odd and even for final fit
  
  final_fit = vector(mode = "numeric", length = k)
  
  for (i in 1:k) {
    if (i %% 2 == 1) {
      final_fit[i] = fit_odd[i]
    } else {
      final_fit[i] = fit_odd[i]
    }
  }
  
  return(final_fit)
}
