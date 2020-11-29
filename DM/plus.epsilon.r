plus.epsilon = function(x,d=10){
## For nonnegative vector x adds epsilon to all 0s in x
## where epsilon is defined as the smallest nonzero x divided by d>1
epsilon = min(x[!x==0])/d
x[x==0] = epsilon
x
}
