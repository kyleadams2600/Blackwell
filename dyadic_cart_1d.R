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
  a1 = 2*ind(x,0,0.4)
  a2 = 4*ind(x,0.4,0.6)
  a3 = ind(x,0.6,0.8)
  a4 = 4*ind(x,0.8,1)
  return(a1 + a2 + a3 + a4)
}

f3 = function(x) {
  return(sin(x))
}

f4 = function(x) {
  return(x^2)
}

f5 = function(x){
  a1 = 2*ind(x,0.2,0.4)
  a2 = 5*ind(x,0.4,0.6)
  a3 = ind(x,0.6,0.8)
  a4 = 4*ind(x,0.8,0.9)
  a5 = 3*ind(x, 0.9, 1)
  return(a1 + a2 + a3 + a4 +a5)
}

f6 = function(x) {
  a1 = 3*ind(x, 0.1, 0.2)
  a2 = 2*ind(x, 0.2, 0.4)
  a3 = 5*ind(x, 0.4, 0.5)
  a4 = ind(x, 0.5, 0.6)
  a5 = 4*ind(x, 0.6, 0.75)
  a6 = 2*ind(x, 0.75, 0.9)
  a7 = 3*ind(x, 0.9, 1)
}

f7 = function(x) {
  return(1/x)
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

#####Algorithm Background Code for Two-Fold Cross Validation----

#this function takes a vector y, and uses the odd observations, and fills the even elements with the average of neighbors
crossval_odd = function(y) { # y is the list of observations
  
  n = length(y)
  y_odd = vector(mode = "numeric", length = n) # list of odd indexed observations from y
  
  for (i in 1:n) {
    if (i %% 2 != 0) { # odd indexes
      y_odd[i] = y[i]
    } else if (i %% 2 == 0 && i != n) { # even indexes
      y_odd[i] = (y[i-1] + y[i+1])/2 # fill in missing observations with average of neighboring observations
      
    } else if (i == n && i %% 2 == 0) { #make last entry same as previous average
      y_odd[i] = y_odd[i-2] 
    }
  }
  
  return(y_odd)
} 

cv_y_odd = crossval_odd(y)

#Even observations
#does the same thing as crossval_odd, with even and odd reversed
crossval_even = function(y) { # y is the list of observations
  
  n = length(y)
  y_even = vector(mode = "numeric", length = n) # list of even indexed observations from y
  
  for (i in 1:n) {
    if (i == 1) {
      y_even[i] = (y[2] + y[4]) / 2 #fills in first entry, makes same as third
    }
    else {
      if (i %% 2 != 1) { # even indexes
        y_even[i] = y[i]
      } 
      else if (i %% 2 == 1 && i != n) { # even indexes
        y_even[i] = (y[i-1] + y[i+1]) / 2 # fill in odd indexes with average of neighboring observations
        
      } 
      #else if (i == n && i %% 2 == 1) { #if last entry and odd length, make last entry an average of the first and second to last observation
      # y_even[i] = (y[1] + y[i-1])/2
      #} #this won't ever enter because in our case y is always even length
    }
  }
  
  return(y_even)
} 


#HOW MANY LAMBDAS?
# n = sample size = 2^l
# lambda grid = {1, 2^1, 2^2, ... , 2^log(n)}
# so number of lambdas will be log(n) (rounded down to nearest int) + 1

#gets lambdas by powers of 2 until log(n)
get_lambdas = function(y) {
  lambdas = c(0)
  for (i in 0:log(length(y), base = 2)) {
    lambdas[i+1] = 2^i 
  }
  return(lambdas)
}


#function that spits out theta values for each lambda
#will return a vector for each lambda, calls like "fitteddatapoint = theta_vector[[lambda]][datapoint]"
#or can get entire thetahat with thetahat = theta_vector[lambda]
create_theta_vector = function(l, y) {
  # y can be either y_even or y_odd
  n = 2^l
  theta_vector = vector(mode = "numeric", length = length(lambdas))
  
  for (i in 1:length(lambdas)) {
    theta_vector[i] = as.vector(dyadic_1d(l, y, lambdas[i])[2]) #creates vector of theta values
  } 
  
  return(theta_vector)
}

#function that minimizes prediction error, returns final fit after combining even and odd
minimize_pe = function(y, l) { 
  #initiliazations
  pe_odd = c(0)
  pe_even = c(0)
  min_index = 1
  best_lambda_odd = 1
  fit_even = c(0)
  min_index_even = 1
  best_lambda_even = 1
  fit_odd = c(0)
  final_fit_odd = c(0)
  final_fit_even = c(0)
  final_fit = c(0)
  
  
  #pe_odd uses even observations and vice versa
  for(i in 1:length(lambdas)) { #length is same as #of lambdas
    pe_odd[i] = sum((y[seq(2,length(y),2)] - theta_hat_odd[[i]][seq(2,length(y),2)])^2) #sums squared difference of even observations of y and even observations of thetahat
    #y_even = y[c(TRUE, FALSE)] #even observations
    #y_even = y_even[i]
    #pe_odd[i] = sum(y_even^2)
  }
  
  for(i in 1:length(lambdas)) {
    pe_even[i] = sum((y[seq(1,length(y),2)] - theta_hat_even[[i]][seq(1,length(y),2)])^2) #same as a above but reversed
    #y_odd = y[c(FALSE, TRUE)] #odd observations
    #y_odd = y_odd[i]
    #pe_even[i] = sum(y_odd^2)
  }
  
  min_index_even = which.min(pe_even) # returns index of smallest prediction error
  best_lambda_even = lambdas[min_index_even] # returns lambda which has the smallest error
  fit_even = theta_hat_even[[min_index_even]] # final fit for even observations
  
  # repeat process with odd and even switched
  
  
  min_index_odd = which.min(pe_odd) # returns index of smallest error
  best_lambda_odd = lambdas[min_index_odd] # lambda which has the smallest error
  fit_odd = theta_hat_odd[[min_index_odd]] # final fit for odd observations
  
  # now just combine odd and even for final fit
  
  final_fit_odd = fit_odd[seq(1,length(fit_odd),2)] #just the odd obsv
  final_fit_even = fit_even[seq(2,length(fit_even),2)] #just the even obsv
  final_fit = c(rbind(final_fit_odd, final_fit_even)) #combining even and odd observations
  
  return(final_fit)
}

##Run the Algorithm----
#l = 10
#n = 2^l
#sigma = 0.4
#theta = sapply(seq(1:n)/n,f4)
#y = theta + rnorm(2^l,0,sigma); plot(y)

#lambdas = get_lambdas(y); #lambdas
#lambdas = c(0.5, 0.75, 1, 1.25, 1.5, 1.75, 2, 3, 4, 5, 6, 7, 8, 9)
#cv_y_odd = crossval_odd(y);# cv_y_odd
#cv_y_even = crossval_even(y);# cv_y_even
#theta_hat_even = create_theta_vector(l, cv_y_even);# theta_hat_even
#theta_hat_odd = create_theta_vector(l, cv_y_odd);# theta_hat_odd
#best_fit = minimize_pe(y,l);# best_fit

###plotting----
#plot(y, main = "Best Fit mapped onto Y") #original function is black
#lines(seq(1,n,1),best_fit, type = "p", col = "red") #fit is red


####Estimating CDFs----

make_t_grid = function(y) {
  t_grid = vector(mode = "numeric", length = length(y))
  y_sorted = sort(y, decreasing = FALSE)
  
  for (i in 1:length(y)) {
    t_grid[i] = y_sorted[i]
  }
  
  return(t_grid)
}
make_new_data = function(y, t_grid) { #makes new vector, 1 if y <= t, 0 if not
  
  w = list()
  temp_y = c(0)
  
  for (t in 1:length(t_grid)) {
    for (i in 1:length(y)) {
      if (y[i] <= t_grid[t]) {
        temp_y[i] = 1
      } else {
        temp_y[i] = 0
      }
      
      
    }
    w[[t]] = (temp_y)
  }
  return(w)
}

#plots cdfs as continuous

plot_cdf = function(xelem, firstfunction, secondfunction) {
  
  xval = x[xelem]
  
  plot(t_grid, pnorm(t_grid, firstfunction(xval), secondfunction(xval)), xlab = "t values", ylim = c(0,1), type = "l", col = "blue")
  lines(t_grid, w_matrix[,xelem], xlab = "t values", ylim = c(0,1), type = "l", col = "red")
  
  legend("bottomright", legend=c("original", paste("x = ", round(xval, digits = 5))),
         col=c("blue", "red"), lty=1, cex=0.65)
}

addxplot = function(xelem, colorpick) {
  
  xval = x[xelem]
  
  lines(t_grid, w_matrix[,xelem], xlab = "t values", ylim = c(0,1), type = "l", col = colorpick)
  
}



#add new x's with labels

add3xplots = function(xelem1, xelem2, xelem3, col1, col2, col3) {
  
  x1 = round(x[xelem1], digits = 4)
  x2 = round(x[xelem2], digits = 4)
  x3 = round(x[xelem3], digit = 4)
  
  lines(t_grid, w_matrix[, xelem1], ylim = c(0,1), type = "l", col = col1)
  lines(t_grid, w_matrix[, xelem2], ylim = c(0,1), type = "l", col = col2)
  lines(t_grid, w_matrix[, xelem3], ylim = c(0,1), type = "l", col = col3)
  
  
  legend("bottomright", legend=c(x1, x2, x3),
         col=c(col1, col2, col3), lty=1, cex=0.65)
  
}

#for any given x
random_x = function(anyx, firstfunction, secondfunction) {
  
  cdf_index = which.min(abs(anyx - x)) #finds index in x vector that the given value is closest to
  averagecdf = (w_matrix[ ,cdf_index + 1] + w_matrix[ ,cdf_index - 1]) / 2
  plot(t_grid, pnorm(t_grid, firstfunction(anyx), secondfunction(anyx)), main = paste("cdf at x = ", anyx), xlab = "t values", ylim = c(0,1), type = "l", col = "blue")
  lines(t_grid, sort(averagecdf), main = paste("cdf at x = ", anyx), xlab = "t values", ylim = c(0,1), type = "l", col = "red")
  
  legend("bottomright", legend=c("original", paste("x = ", anyx)),
         col=c("blue", "red"), lty=1, cex=0.65)
}

random_t = function(anyt) {
  
  wvec = c(0)
  
  for (i in 1:length(y)) {
    if (y[i] <= anyt) {
      wvec[i] = 1
    } else {
      wvec[i] = 0
    }
  }
  
  cv_w_odd = crossval_odd(wvec); #cv_w_odd
  cv_w_even = crossval_even(wvec); #cv_w_even
  theta_hat_even = create_theta_vector(l, cv_w_even); #theta_hat_even
  theta_hat_odd = create_theta_vector(l, cv_w_odd); #theta_hat_odd
  best_fit = minimize_pe(wvec,l);
  plot(best_fit, main = paste("t = ", anyt), xlab = "x values", ylim = c(0,1),col = "blue")
  
}

find_best_fit = function(vec, l) {
  
  cv_odd = crossval_odd(vec); #cv_w_odd
  cv_even = crossval_even(vec); #cv_w_even
  theta_hat_even = create_theta_vector(l, cv_even); #theta_hat_even
  theta_hat_odd = create_theta_vector(l, cv_odd); #theta_hat_odd
  best_fit = minimize_pe(vec,l)
  return(best_fit)
}

##to run----
l = 5
n = 2^l
sigma = 0.2
firstfunction = f
secondfunction = f2
theta = sapply(seq(1:n)/n,firstfunction)
#y = theta + rnorm(2^l,0,sigma); plot(y)
x = runif(n, min = 0, max = 1)
mean_y = sapply(x, firstfunction)
sigma_y = sapply(x, secondfunction)
y = rnorm(2^l,mean_y,sigma_y); plot(y)
y = y[order(x)]
x = x[order(x)]
#w_matrix = get_w_matrix(y)

#get_w_matrix = function(y) { #doesnt work as function atm so it's commented out

t_grid = make_t_grid(y)
w_matrix = matrix(nrow = length(t_grid), ncol = length(y))
w = make_new_data(y, t_grid)
lambdas = c(0.5, 0.75, 1, 1.25, 1.5, 1.75, 2, 3, 4, 5, 6, 7, 8, 9) #choose between automated lambdas, or a specific set of lambdas
#lambdas = get_lambdas(y)

for (t in 1:length(t_grid)) {
  
  
  cv_w_odd = crossval_odd(w[[t]]); #cv_w_odd
  cv_w_even = crossval_even(w[[t]]); #cv_w_even
  theta_hat_even = create_theta_vector(l, cv_w_even); #theta_hat_even
  theta_hat_odd = create_theta_vector(l, cv_w_odd); #theta_hat_odd
  best_fit = minimize_pe(w[[t]],l); #best_fit
  w_matrix[t, ] = best_fit #each row represents yhat based on t_grid[t], #matrix[t, ] = best fit for t'th entry in t_grid, matrix [ ,X] is the cdf of xX, # so matrix [ ,5] is the cdf of x5, based on each t in t_grid

  }

for (k in 1:length(w_matrix[1,])) {
  w_matrix[,k] = sort(w_matrix[,k])
}

#return(w_matrix)
#}

plot_cdf(31)
random_x(0.32)
random_t(3)


#one way to plot piecewise cdf
#lines(stepfun(t_grid[seq(1,n-1,1)], sort(w_matrix[, t])), ylim = c(0,1), col = "green")

#generate_y = function(l,sigma,f,f2) {

#  l = 5  
#n = 2^l
#  sigma = 0.5
#theta = sapply(seq(1:n)/n,f)
#y = theta + rnorm(2^l,0,sigma); plot(y)
#x = runif(n, min = 0, max = 1)
#mean_y = sapply(x, f)
#sigma_y = sapply(x, f2)
#y = rnorm(2^l,mean_y,sigma_y); plot(y)

#return(y)
#}



##TESTING COMBINING CODE INTO FUNCTIONS
#ytest = generate_y(6, 0.3, f3, f4)
#wtestmatrix = fit_cdf(ytest)

#takes average fhat and plots against empirical cdf of y
#averagefhat = c(0)

#for (j in 1:length(y)) {
#  averagefhat[j] = sum(w_matrix[j,])/n
#}
#plot(ecdf(y))
#lines(t_grid[seq(1,n,1)], averagefhat, ylim = c(0,1), type = "p", col = "blue", pch = 7)



###plotting cdfs and using matrix
#l = 4
#n = 2^l
#sigma = 0.3
#x = runif(n, min = 0, max = 1) #generate uniform distribution for x
#theta = sapply(x,f4) #f4 is applied to the uniform distribution x
#theta = sapply(seq(1:n)/n,f3)
#y = theta + rnorm(2^l,0,sigma); plot(y)
#y = theta +rnorm(2^l, 0, 0.2)
#sigma = 0.3
#x = runif(n, min = 0, max = 1) #generate uniform distribution for x
#mean_y = sapply(x,f3) #f3 is applied to the uniform distribution x
#sigma_y = sapply(x,f4)
#theta = sapply(seq(1:n)/n,f3)
#y = rnorm(2^l,mean_y,sigma_y); plot(y)

#currently you have to go back to minimize_pe and run line by line for first 4 vals
#best_lambda_even #best lambda for even observations
#best_lambda_odd #best lambda for odd observations
#pe_even[min_index] #prediction error for even obsv
#pe_odd[min_index_even] #prediction error for odd obsv
#mean((y - best_fit)^2) #MSE 
