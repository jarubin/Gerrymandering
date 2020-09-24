library(ggplot2)
towns <- 169 # number of towns
districts <- 12 # number of districts
x <- sample(1:200, towns, replace = TRUE) # x-coordinates of towns
y <- sample(1:100, towns, replace = TRUE) # y-coordinates of towns
pop <- 100+rexp(towns,.01) # populations of towns
pop <- round(pop)
A <- cbind(x, y, pop) # matrix of towns
A
width <- max(A[,1]) - min(A[,1]) # width of state
width
height <- max(A[,2]) - min(A[,2]) # height of state
height
ratiostate <- width/height
ratiostate
vdistrict <- c(1:districts)
vdistrict
vratio <- c(districts/vdistrict^2)
vratio
vratio-ratiostate
a <- order(abs(vratio-ratiostate))[1] # number of rows
a
b <- round(districts/a) # number of columns
b
vb <- c(0:b) # vector from 0 to b
vb
d1 <- c(round(districts*vb/b)) # dividing districts into b columns
d1
e1 <- 0
for (i in 1:b) {
  e1[i] <- (d1[i+1] - d1[i])
}   # number of districts in each column
e1
B <- A[order(A[,1]),] # towns sorted from west to east
B
population <- sum(A[,3]) # total population of all towns
population
d2 <- c(round(population*d1/districts)) # target people in each band
d2
e2 <- 0
for (i in 1:b) {
  e2[i] <- (d2[i+1] - d2[i])
}   # target population for each band
e2
T <- 0; ppart <- 0
T; ppart
for (i in 1:towns) {
  T <- T + B[i, 3]
  ppart[i] <- T
} # this loop creates the cumulative sum of the population by town
ppart

d3 <- 0; j <- 2
d3; j
for (i in 1:towns) {
  if (ppart[i] > d2[j]){ 
    if (abs(ppart[i] - d2[j]) < abs(ppart[i-1] - d2[j]))
    { (d3[j] <- i) & (j <- j+1) } 
    else { (d3[j] <- (i-1)) & (j <- j+1) }
  }
}  # this loop determines how many towns are in each vertical band
d3[b+1] <- towns
d3
e3 <- 0
for (i in 1:b) {
  e3[i] <- (d3[i+1] - d3[i])
}   # number of towns in each band
e3
sum(e3)

e4 <- 0 # actual population of each band
for (i in 1:b) {
  e4[i] <- sum(B[(d3[i]+1):d3[i+1], 3])
}
e4
sum(e4)

C <- list(); D <- list()
for (i in 1:b) {
  C[[i]] <- B[(d3[i]+1):d3[i+1],]
} # This loop creates a matrix for each column from the B matrix
C
for (i in 1:b) {
  D[[i]] <- C[[i]][order(C[[i]][,2]), ]
}  # This loop sorts each of the C matrices by y-value
D

d5 <- 0; k <- 2 # d5 number of towns in each district cumulative

for (i in 1:b) {
  T <- 0; ppart <- 0
  
  for (j in 1:e3[i]) {
    T <- T + D[[i]][j, 3]
    ppart[j] <- T     # cumulative population vector
  }
  
  avg <- e4[i]/e1[i]
  vavg <- 0
  for (j in 1:e1[i]) {
    vavg[j] <- j*avg
  }
  
  q <- 1
  for (j in 1:(e3[i]-1)) {
    if (ppart[j+1] >= vavg[q]){ 
      if (abs(ppart[j+1] - vavg[q]) < abs(ppart[j] - vavg[q]))
      { (d5[k] <- j+1+d3[i] ) & (k <- k+1) & (q <- q+1) } 
      else {(d5[k] <- j+d3[i]) & (k <- k+1) & (q <- q+1) } }
  }
  
}

d5

E <- matrix(ncol=3) # E will be the full matrix of sorted towns
E
for (i in 1:b) {
  E <- rbind(E,D[[i]])
}
E <- E[-1,]
E

e5 <- 0
for (i in 1:districts) {
  e5[i] <- d5[i+1]-d5[i] 
}
e5

t <- 1; v <- 0
for (i in 1:districts) {
  for (j in 1:e5[i]) {
    v[t] <- i  
    t <- t+1
  }
}
v

F <- cbind(E,v)
F

dF <- data.frame(F)
colnames(dF) <- c("lat", "long", "pop", "distr")
plott <- ggplot(dF, aes(lat, long)) + geom_point(aes(color = factor(distr)))
plott
# this is a testing section

