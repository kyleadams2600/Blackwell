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
  y_odd = vector(mode = "integer", length = n) # list of odd indexed observations from y
  
  for (i in 1:n) {
    if (i %% 2 != 0) { # odd indexes
      y_odd[i] = y[i]
    } 
    else if (i %% 2 == 0 && i != n) { # even indexes
      y_odd[i] == ((y[i-1] + y[i+1])/2)# fill in missing observations with average of neighboring observations
      message(y[i-1], y[i], y[i+1])
      message((y[i-1] + y[i+1])/2)
    } 
    else if (i == n && i %% 2 == 0) { #if last entry and even length, make last entry an average of the first and second to last observation
      y_odd[i] == (y[1] + y[i-1])/2
    }
  }
   
  return(y_odd)
} 

#Even observations

crossval_even = function(y) { # y is the list of observations
  
  n = length(y)
  y_even = vector(mode = "integer", length = n) # list of odd indexed observations from y
  
  for (i in 1:n) {
    if (i %% 2 != 1) { # odd indexes
      y_even[i] = y[i]
    } 
    else if (i %% 2 == 1 && i != n) { # even indexes
      y_even[i] == ((y[i-1] + y[i+1])/2)# fill in missing observations with average of neighboring observations
      message(y[i-1], y[i], y[i+1])
      message((y[i-1] + y[i+1])/2)
    } 
    else if (i == n && i %% 2 == 1) { #if last entry and even length, make last entry an average of the first and second to last observation
      y_even[i] == (y[1] + y[i-1])/2
    }
  }
  
  return(y_even)
} 
