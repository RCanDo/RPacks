########################################################################################################################
##

ass = function(z,name){
  assign(name,z,pos=parent.frame(n=2))
}

f1 = function(y,name){
   z = 2*y
   ass(z,name)
}

f0 = function(x){
   f1(x,"k")
   print(k)
   k
}


rm(ass,f1,f0)

########################################################################################################################

ff = function(a,env = -1){
	assign(a , pi , pos = env)
}

rm(kk)
ff("kk",0)
ff("kk",".GlobalEnv")

	ls(pattern = '^k')
	kk

rm(ff)

fac = factor( sample(letters[1:4] , 20 , replace = TRUE ))
table(fac)
(lev = levels(fac))
class(lev)


########################################################################################################################
  ##
  #model.l = glm(CzyDobra~variable,family=binomial(link="logit"))
#  model.lp = glm(sucandfail~variable,family=binomial(link="logit"))
#  if(!is.factor(variable)){
#    model.a = gam(CzyDobra~s(variable),family=binomial(link="logit"))
#    model.ap = gam(sucandfail~s(variable),family=binomial(link="logit"))
#  }


  #! poprawic te wykresy dla factorow bo sa troche bez sensu...
    if(is.factor(variable)){
    # 1
    plot(variable)
    # 2
    plot(density(skutecznosc[variable==levels(variable)[1]]), xlim=range(skutecznosc))
    for(j in 2:length(levels(variable))){
      lines(density(skutecznosc[variable==levels(variable)[j]]), col=j)
    }
    # 3
    plot(CzyDobra~variable,xlab=var.name,pch=".")
      lines(predict(model.l,type="response")[order(variable)]~sort(variable), pch=".",col=2)
    ### panel = function(x,y){points(x,y);lines(lowess(x,y),col=2,lwd=2)}
    # 4
    plot(skutecznosc~variable,xlab=var.name,pch=".")
      abline(h=1,col=4)
      lines(predict(model.lp,type="response")[order(variable)]~sort(variable), pch=".",col=2)
    mtext(var.name,outer=TRUE)
    }else{
    # 1
      ####### histogram zwykly
        hist(variable,ylab=var.name,freq=FALSE)
        #!??? lines(density(variable)$y*length(variable)~density(variable)$x, col=2)
        lines(density(variable), col=2)
      ####### barplot podwojny -- do poprawy -- na razie wylaczone
      #  v.good = variable[CzyDobra==1]
      #  v.bad = variable[CzyDobra==0]
      #  v.both = c(v.good,v.bad)
      #  #ff = rep(c("a","b"),each=100)
      #  #boxplot(xx~ff)
      #      dd = 13 ; dd2 = 2*dd
      #  c.good = cut(v.good,breaks=seq(min(v.both),max(v.both),diff(range(v.both))/dd),dig.lab=2)
      #  table(c.good)
      #  c.bad = cut(v.bad,breaks=seq(min(v.both),max(v.both),diff(range(v.both))/dd),dig.lab=2)
      #  table(c.bad)
      #  both = numeric(dd2)
      #  both[1:dd2%%2!=0]=table(c.good)
      #  both[1:dd2%%2==0]=table(c.bad)
      #  #barplot(both,col=rep(c("grey","orange"),dd),ylab="frequency")
      #  ## etykiety na x -- do porawy
      #  xscale = barplot(both,col=rep(c("red","green"),13),ylab="frequency")
      #     ## points at which bars are plotted, but we need different ticks
      #  dis = diff( xscale[c(1,3),1] )
      #  aa = mean(xscale[1:2,1]); zz = aa+(dd-1)*dis
      #  mtext(levels(c.good),side=1,at=seq(aa,zz,dis),cex=.4,srt=60)   ## srt = rotacja -- nie dziala..
      #######
    # 2
      cdf = (1:length(variable))/length(variable)
    plot(cdf~sort(variable), xlab=var.name, pch = ".")
    # 3
    plot(CzyDobra~variable,xlab=var.name,pch=".")
      lines(predict(model.l,type="response")[order(variable)]~sort(variable), pch=".",col=2)
      lines(predict(model.a,type="response")[order(variable)]~sort(variable), pch=".",col=5)
    ### panel = function(x,y){points(x,y);lines(lowess(x,y),col=2,lwd=2)}
    # 4
    plot(skutecznosc~variable,xlab=var.name,pch=".")
      abline(h=1,col=4)
      lines(predict(model.lp,type="response")[order(variable)]~sort(variable), pch=".",col=2)
      lines(predict(model.ap,type="response")[order(variable)]~sort(variable), pch=".",col=5)
    mtext(var.name,outer=TRUE)
    ## ustalamy granice lft i rgt
    }
#   } #2

myfun = function(x,FUN=function(x)x){
FUN(x)
}
