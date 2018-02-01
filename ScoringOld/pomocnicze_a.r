##################################
percent.string <- function(x)
{
paste(round(x*100,2),"%",sep="")
}

##################################
median.dist <- function(data.set,target.variable)
{
variables.names<-attr(data.set,"names")
target.variable.index<-which(variables.names==target.variable)
  x<-data.set[data.set[,target.variable.index]==1,dim(data.set)[2]]
  y<-data.set[data.set[,target.variable.index]==0,dim(data.set)[2]]

x.median=median(x)
y.median=median(y)
median.dist<-x.median-y.median
median.dist
}

##################################
scale.axis <-function(y1,y1.labels,y2)
{
ay1=min(y1)
by1=max(y1)
ay2=min(y2)
by2=max(y2)

scaled.y2=(y2-ay2)/(by2-ay2)*(by1-ay1)+ay1
y2.labels=(y1.labels-ay1)/(by1-ay1)*(by2-ay2)+ay2

list(scaled.y2=scaled.y2,y2.labels=y2.labels)
}

##################################

median.ratio <- function(data.set,target.variable)
{
variables.names<-attr(data.set,"names")
target.variable.index<-which(variables.names==target.variable)
  x<-data.set[data.set[,target.variable.index]==1,dim(data.set)[2]]
  y<-data.set[data.set[,target.variable.index]==0,dim(data.set)[2]]

x.median=median(x)
y.median=median(y)
median.ratio<-x.median/y.median
median.ratio
}


################################################################################


mean.ratio <- function(data.set,target.variable)
{
variables.names<-attr(data.set,"names")
target.variable.index<-which(variables.names==target.variable)
  x<-data.set[data.set[,target.variable.index]==1,dim(data.set)[2]]
  y<-data.set[data.set[,target.variable.index]==0,dim(data.set)[2]]

x.mean=mean(x)
y.mean=mean(y)
mean.ratio<-x.mean/y.mean
mean.ratio
}

##################################################################################

mean.dist <-function(data.set,target.variable)
{
variables.names<-attr(data.set,"names")
target.variable.index<-which(variables.names==target.variable)
  x<-data.set[data.set[,target.variable.index]==1,dim(data.set)[2]]
  y<-data.set[data.set[,target.variable.index]==0,dim(data.set)[2]]

x.mean=mean(x)
y.mean=mean(y)
mean.dist<-x.mean-y.mean
mean.dist
}


################################################################################

wsp_zmiennosci <- function(x){
sapply(x, sd, na.rm=TRUE)/sapply(x, mean,na.rm=TRUE)
}

