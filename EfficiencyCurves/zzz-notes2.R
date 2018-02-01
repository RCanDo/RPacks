mm <- matrix(1:24)
dim(mm) <- c(2,3,4)
mm

dim(mm)

efficiency(mm)
efficiency(mm,T,margin=c(2,3))
aperm(efficiency(mm,T,margin=c(1,3)),c(2,1,3))
aperm(efficiency(mm,T,margin=c(3,1)),c(3,1,2))

aperm(efficiency(mm,T,margin=c(1,2)),c(2,3,1))


