data(USArrests)
Y <- matrix(USArrests[,1])
X <- as.matrix(cbind(1, USArrests[,-1]))

beta_sol <- solve(t(X) %*% X) %*% t(X) %*% Y
print(beta_sol)

##다변량으로 넘어가기 어려울 것 같아서 일단 X를 수정
X <- as.matrix(cbind(1, USArrests[, 2]))


#Coordinate descent method
method_cd <- function(fn, init_val, tol = 0.01, learn_rate = 0.001){
  x_old <- init_val
  x_new <- init_val
  
  ## Loop
  cnt <- 1
  
  while(1){
    ## update the current point
    # for(j in 1:length(x_new)){
    #   x_new[j] <- ...
    # }
    x_new[1] <- sum((Y - X[,2] * x_old[2])*X[,1]) / sum(X[,1]^2)
    x_new[2] <- sum((Y - X[,1] * x_new[1])*X[,2]) / sum(X[,2]^2)
    
    
    cnt <- cnt + 1
    if(abs(fn(x_new) - fn(x_old)) < tol){
      break
    }
    x_old <- x_new
  }
  ## return
  return(list(cnt = cnt,
              val_opt = x_new,
              val_fun = fn(x_new))
  )
}



eval_f <- function(Y, X, beta){
  0.5 * sum((Y - X %*% beta)^2)
}


method_cd(fn = function(x) eval_f(Y, X, beta = x), 
          init_val = c(0,0), 
          learn_rate = 1e-4, 
          tol = 1e-7)





#readjust eval_f1
eval_f1 <- function(Y, X, beta) {
  -t(X)%*%Y + (t(X) %*% X) %*% beta
}

#check eval_f1
eval_f1(Y=Y, X=X, beta=c(0,0))

eta <- 0.0001

##gradient descent algorithm
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


#파라미터 조정하면서 결과 확인해보기
method_gd(fn = function(x) eval_f(Y, X, beta = x), 
          fn_deriv = function(x) eval_f1(Y, X, beta = x), 
          init_val = c(0.5, 0), 
          learn_rate = 1e-7, 
          tol = 1e-7)




#learn_rate = 1e-5, 수렴하지 않는 경우라서 에러
method_gd(fn = function(x) eval_f(Y, X, beta = x), 
          fn_deriv = function(x) eval_f1(Y, X, beta = x), 
          init_val = c(0.5, 0), 
          learn_rate = 1e-7, 
          tol = 1e-7)


#코디네이트의 경우, 바로 수렴
#나눠서 푸는 게 어떤 경우에는 더 빠를 수도 있다.
  