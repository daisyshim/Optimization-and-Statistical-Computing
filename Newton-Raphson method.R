data(USArrests)
Y <- matrix(USArrests[,1])
X <- as.matrix(cbind(1, USArrests[,-1]))

eval_f <- function(Y, X, beta){
  0.5 * sum((Y - X %*% beta)^2)
}

eval_f1 <- function(Y, X, beta){
  -t(X)%*%Y + (t(X) %*% X) %*% beta
}

eval_f2 <- function(Y, X, beta){
  t(X)%*%X
}

solve(eval_f2(Y, X, x))
eval_f2(Y, X, x)

method_nr <- function(fn, fn_deriv, fn_hessian, init_val, tol = 0.01){
  
  x_old <- init_val
  
  ## Loop
  cnt <- 1
  while(1){
    ## update the current point
    
    #역행렬 => solve
    #이전 x값에 영향을 받아야해서 행렬에 x_old를 넣어줘야함
    x_new <- x_old - solve(fn_hessian(x_old)) %*% fn_deriv(x_old)
    cnt <- cnt + 1
    ## check the convergence
    if(abs(fn(x_new) - fn(x_old)) < tol){
      break
    }
    ## switch the role
    x_old <- x_new
  }
  
  ## return
  return(list(cnt = cnt,
              val_opt = x_new,
              val_fun = fn(x_new))
  )
}

method_nr(fn = function(x) eval_f(Y, X, x),
          fn_deriv = function(x) eval_f1(Y, X, x),
          fn_hessian = function(x) eval_f2(Y, X, x),
          init_val = c(0, 0, 0, 0),
          tol = 1e-4)

solve(t(X) %*% X) %*% t(X) %*% Y
