sample_bank <- data.matrix(iris)

sorteia <- function (bank) (bank[randperm(nrow(bank)),])
pdfnvar <- function(x,m,K,n) ((1/(sqrt((2*pi)^n*(det(K)))))*exp(-0.5*(t(x-m) %*% (solve(K)) %*% (x-m))))

loop <- function(count, sample_bank) {
  global_error <- array(data = 0, dim = count)
  for(co in 1:count) {
    
    u_bank <- sorteia(sample_bank)
    
    size <- dim(u_bank)[1]
    limite <- floor(0.7*size)
    treinamento <- u_bank[1:limite,]
    
    x <- treinamento[,-5]
    y <- treinamento[,5]
    
    teste <- u_bank[(limite+1):nrow(u_bank), ]
    x_teste <- teste[, -5]
    y_teste <- teste[, 5]
    
    x_setosa <- x[y==1,]
    x_versicolor <- x[y==2,]
    x_virginica <- x[y==3,]
    
    m_setosa = colMeans(x_setosa)
    m_versicolor = colMeans(x_versicolor)
    m_virginica = colMeans(x_virginica)
    
    c_setosa = cov(x_setosa)
    c_versicolor = cov(x_versicolor)
    c_virginica = cov(x_virginica)
    
    saida <- matrix(0, nrow=y_teste, ncol=1)
    l <- length(x_teste[,1])
    for(i in 1:l){
      s <- x_teste[i,]
      set <- pdfnvar(s, m_setosa, c_setosa, 4)
      ver <- pdfnvar(s, m_versicolor, c_versicolor, 4)
      vir <- pdfnvar(s, m_virginica, c_virginica, 4)
      saida[i] <- which.max(c(set, ver, vir))
    }
      error <- (y_teste - saida)
      e_l <- length(which(error != 0))
      global_error[co] <- (e_l/l)*100
  }
  a <- mean(global_error)
  print(paste('The mean error for', count, 'executions is', format(round(a, 2), nsmall = 2), '%'))
  return(global_error)
}
error <- loop(100, sample_bank)

