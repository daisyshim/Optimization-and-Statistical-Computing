
eval_f <- function(x) {
  (x-1)^2
}

intval <- c(-5, 5)
x_min <- -3
x_max <- 3


if(eval_f(x_min) < eval_f(x_max)) {
  intval <- c(-5, x_max)
} else {intval <- c(x_min, 5) }



if(eval_f(x_min) < eval_f(x_max)) {
  intval[2] <- x_max
} else {intval[1] <- x_min}




if(eval_f(x_min) < eval_f(x_max)) {
  intval <- c(intval[1], x_max)
} else {intval <- c(x_min, intval[1])}




intval

###잘못된 코드
bisection <- function(fnt, intval, t=0.05){
  for(i=0; i<=5; i++){
    if(fnt(x_min) < fnt(x_max)) {
      intval[2] <- x_max
    } else {intval[1] <- x_min}
    x_min = x_min+t
    x_max = x_max-t
  }
}



bisection <- function(fnt, intval, t = 0.9) {
  x_min <- intval[1]
  x_max <- intval[2]
  for (i in 1:8) {
    if (fnt(x_min) < fnt(x_max)) {
      intval[2] <- x_max
    } else {
      intval[1] <- x_min
    }
    x_min <- x_min + t
    x_max <- x_max - t
  }
  return(intval)
}

bisection(eval_f, intval)

intval <- c(-3, 3)








method_bisect <- function(fn, intval, tol = 0.001){
  ## choose two candidates
  
  ## Loop
  cnt <- 1
  while(abs(intval[2] - intval[1]) > tol){
    ## update intval
    x_min <- (intval[1]*2/3)+(intval[2]*1/3)
    x_max <- (intval[1]*1/3)+(intval[2]*2/3)
    
    if(fn(x_min) < fn(x_max)) {
      intval <- c(intval[1], x_max)
    } else {
      intval <- c(x_min, intval[2])
    }
    
    cnt <- cnt + 1
  }
  
  ## return
  return(list(cnt = cnt,
              intval = intval,
              val_opt = mean(intval),
              val_fun = fn(mean(intval))))
}


method_bisect(eval_f, 
              c(-3,3))




###Gradient descent Method

eval_f1 <- function(x){
  2 * (x-1)
}

x_old <- 10
eta <- 0.0001
x_new <- x_old - eta*eval_f1(x_old)




#Use 'if' in 'while' by using 'break' 
method_gd <- function(fn, fn_deriv, init_val, tol=0.01, learn_rate = eta){
  
  x_old <- init_val
  
  ##Loop
  cnt <- 1
  while(1){
    ## update the current point
    x_new <- x_old - learn_rate*fn_deriv(x_old)
    cnt <- cnt+1
    
    ## check the convergence
    if(abs(fn(x_new) - fn(x_old)) < tol){
      break
    }
    
    x_old <- x_new
  }
  
  ##return
  return(list(cnt = cnt,
              val_opt = x_new,
              val_fun = fn(x_new)))
}


method_gd(fn = eval_f, 
          fn_deriv = eval_f1, 
          init_val = 10, 
          tol = 0.000001)





#### This time, we don't use the static 'eta'.
##### We want to calculate the eta

eta = (x_old-1)/(2*x_new-1)



method_gd_exact <- function(fn, fn_deriv, init_val, tol=0.01){
  
  # fn = eval_f
  # fn_deriv = eval_f1
  # init_val = 5
  # tol = 0.01
  
  
  x_old <- init_val
  
  
  ##Loop
  cnt <- 1
  while(1){
    
    ## update the current point
    
    learn_rate <- (x_old-1)/fn_deriv(x_old)
    if(fn_deriv(x_old) == 0){
      break
    }
    
    x_new <- x_old - learn_rate*fn_deriv(x_old)
    cnt <- cnt+1
    
    ## check the convergence
    if(abs(fn(x_new) - fn(x_old)) < tol){
      break
    }
    
    x_old <- x_new
  }
  
  ##return
  return(list(cnt = cnt,
              val_opt = x_new,
              val_fun = fn(x_new)))
}




method_gd_exact(fn = eval_f, 
          fn_deriv = eval_f1, 
          init_val = 10, 
          tol = 0.01)

