
make.plot <- function(xmax=150, ymax=75, pch='.') {
  xvals <- vector(length=2*sqrt(ymax)*ymax) ; yvals <- xvals
  cur <- rep(0, xmax) ; prev <- cur
  mid <- xmax/2 ; prev[mid] = 1
  n = 1 ; xvals[1] = mid ; yvals[1] = ymax
  s = mid ; t = mid
  
  for (y in (ymax-1):1) {
    s = s - 1 ; t = t + 1
    for (x in s:t) {
      left <- ifelse(x==1, 0, prev[x-1])
      right <- ifelse(x==xmax, 0, prev[x+1])
      cur[x] = (left + right) %% 2
      if (cur[x] == 1) {
        n = n + 1 ; xvals[n] = x ; yvals[n] = y
      }
    }
    prev <- cur
  }
  print(sprintf("There are n=%d points", n))
  plot(xvals[1:n], yvals[1:n], pch=pch, xlim=c(0,xmax), ylim=c(0,ymax), xlab="", ylab="")
}

make.plot(32, 16, pch=19)    #     81 points
make.plot(64, 32, pch=18)    #    243 points
make.plot(128, 64, pch=18)   #    729 points
make.plot(256, 128, pch=18)  #   2187 points
make.plot(512, 256, pch=18)  #   6581 points
make.plot(1024, 512, pch=18) #  19683 points
make.plot(2048, 1024)        #  59049 points
make.plot(4096, 2048, '.')   # 177147 points

## 2*sqrt(x)*x is a good upper-bound on the number of points set
x <- 2^(4:11)
xx <- 2*sqrt(x)*x
yy <- c(81, 243, 729, 2187, 6581, 19683, 59049, 177147)
rbind(xx,yy)



#### A very general function for elementary cellualar automata ####

## Returns a vector containing the bitfield (?) for a given integer n with the LSBit on the left
## Todo:  make this completely generic
as.bitstring <- function(n) {
  vals <- vector(mode='integer', length=8)
  for (i in 1:(log(n, base=2)+1)) {
    vals[i] = n %% 2
    n = n %/% 2    
  }
  return(vals)
}

## TODO:  write a generic function to 'make' the rules
rule110 <- function(x) {
  v <- as.bitstring(110)
  n <- (4*x[1]) + (2*x[2]) + x[3] + 1
  return(v[n])
}

rule28 <- function(x) {
  v <- as.bitstring(28)
  n <- (4*x[1]) + (2*x[2]) + x[3] + 1
  return(v[n])
}

## Slightly more generic rule function
rule <- function(x, rule.num=110) {
  v <- as.bitstring(rule.num)
  n <- (4*x[1]) + (2*x[2]) + x[3] + 1
  return(v[n])
}


plot.automata <- function(init, iterations, rule.num, pch='.') {
  xmax <- length(init) ; ymax <- iterations
  prev <- init ; cur <- prev
  xvals <- which(cur > 0) ; yvals <- rep(ymax, length(xvals))
  
  for (y in (ymax-1):1) {
    for (x in 1:length(init)) {      
      left <- ifelse(x==1, 0, prev[x-1])
      center <- prev[x]
      right <- ifelse(x==xmax, 0, prev[x+1])
      cur[x] = rule(c(left, center, right), rule.num)
    }
    tmp <- which(cur > 0)
    xvals <- c(xvals,tmp) ; yvals <- c(yvals,rep(y,length(tmp)))
    prev <- cur
  }
  print(sprintf("There are n=%d points", length(xvals)))
  plot(xvals, yvals, pch=pch, xlim=c(0,xmax), ylim=c(0,ymax), 
       xlab="", ylab="", main=sprintf("Rule %d", rule.num))
}


init <- rep(0,128)
init[length(init)/2] = 1
plot.automata(init, length(init), 30, pch='+')


rules <- c(9,11,15,18,22,26,30,41,45,46,54,56,60,73,90,105,106,110,126,146,150,184)
init <- rep(0,256)
init[1] = 1
sapply(rules, function(rule) plot.automata(init, length(init), rule, pch=',')) -> tmp




init <- rep(0,256)
init[sample(256, sample(7,1))]=1
init[c(1,256)]=1
plot.automata(init,length(init)/2,pch=18)


init <- rep(c(1,1,0,1,0),40)
plot.automata(init,128,30,pch='+')



