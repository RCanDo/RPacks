## ---------------------------------------------------------------------------------------------------------------------—•°
## FUNCTIONS HERE    {DM}
##  unattr()
##  fix.classes()
##
## DEPENDENCIES
##  whileif()
##  Uses options set up in  {DM} options.R
##    getOption("class.attributes")
##    getOption("class.groups")
##
## TODO
## •
##
## ---------------------------------------------------------------------------------------------------------------------—•°

## ---------------------------------------------------------------------------------------------------------------------—•°
unattr = function(x,what=NULL){
## ------------------------------------------------------------------------------------------------—•°
## Getting rid of all attributes mentioned in 'what' instead of class, dim, dimnames, names.
## If 'what' is NULL or TRUE then all attributes are removed except these four.
## If FALSE then all attributes are preserved (nothing is removed).
##    Comment
## There should be some inbuilt R function to achieve this goal! But what???
## PS. After some survey it seems that there's none!
## ------------------------------------------------------------------------------------------------—•°

if(!is.null(attributes(x))){

   attrs.all <- names(attributes(x))
   attrs.mustbe <- getOption("class.attributes")$basic

   attrs.off <- whileif(what,ifnull=TRUE,iftrue=attrs.all,iffalse=character(0))
   attrs.off <- setdiff(attrs.off,attrs.mustbe)   ## we don't want to switch of basic attributes

#   attrs.on  <- setdiff(setdiff(attrs.all,attrs.off),attrs.main)   ## not used

   ## now:     attrs.all  =  attrs.off  u  attrs.on  u  attrs.main
   ## attrs.main will be always preserved
   ## attrs.off  will be removed
   ## attrs.on   will be retained

   attributes(x)[attrs.off] <- NULL
        ## this doesn't work for x being "goruped_df" (from dplyr) and attribute "groups"; try e.g.
        ##   x <- dplyr::group_by(Iris, Species)
        ## see options.R  class.attributes$basic  where "groups" where added (but shouldn't be relly there!)
        ## only for this reason !
        ## HOWEVER, see indentr.R comment at `attrs.mustbe` line ~222

}

fix.classes(x)

}
## ------------------------------------------------------------------------------------------------—•°
## EXAMPLES
dummy = function(){
    ## RELOADER —— before it works you need to source("PacksAK.R"); it's best to use EfficiencyCurves.R within pack's dir.
    loadPacksAK("DM")

x = sample(10)
names(x) = letters[1:10]
x
unattr(x)
margins(x) <- 2
x
unattr(x)
x

(mm = matrix(sample(1:12), nrow=3))
comment(mm) <- "Just an example"
attributes(mm)
mm
unattr(mm)

colnames(mm) <- letters[1:4]
rownames(mm) <- letters[21:23]
mm
mm[]
attributes(mm)
unattr(mm)
attributes(unattr(mm))

## "margins" attribute defined in ...
margins(mm) <- 1:2
mm
mm[]
attributes(mm)
unattr(mm)[]
attributes(unattr(mm))
unattr(mm)
unattr(mm)[]
class(unattr(mm))

names(dimnames(mm)) <- c("X", "Y")
mm
mm[]
attributes(mm)
unattr(mm)
attributes(unattr(mm))

## more attributes
mm[] <- rnorm(12)
mm

round.attr(mm) <- 3    ## now mm inherits from  "tm" and print.tm() method is used
attr(mm, "a1") <-  1
attr(mm, "a2") <-  2
attr(mm, "ch1") <-  "a"
attr(mm, "ch2") <-  letters[3:5]
mm
class(mm)
mm[]

unattr(mm)
unattr(mm)[]
unattr(mm, c("a1", "ch2"))
unattr(mm, c("a1", "ch2"))[]

unattr(mm, c("margins"))
unattr(mm, c("margins"))[]
class(unattr(mm, c("margins")))

unattr(mm, c("margins", "round"))
class(unattr(mm, c("margins", "round")))  ## no "tm" as attributes rendering it "tm" are removed

(ff <- factor(sample(letters[1:5], 100, replace=T)))
ff[]
attributes(ff)
ff <- ordered(ff)
ff
attributes(ff)
attributes(unattr(ff))

}; rm(dummy)
## ---------------------------------------------------------------------------------------------------------------------—•°

## ---------------------------------------------------------------------------------------------------------------------—•°
fix.classes <- function(x){
## ------------------------------------------------------------------------------------------------—•°
## Checks and fixes class of the object according to its attributes.
## Uses functions {class_name}.class(x).
## Currently only tm.class(x) and compact.class(x) are defined.
## ------------------------------------------------------------------------------------------------—•°
   x <- tm.class(x)
   x <- compact.class(x)
   # x <- {class_name}.class(x)
   # ...
   x
}
## ---------------------------------------------------------------------------------------------------------------------—•°

## ------------------------------------------------------------------------------------------------—•°
## EXAMPLES
dummy = function(){
    ## RELOADER —— before it works you need to source("PacksAK.R"); it's best to use EfficiencyCurves.R within pack's dir.
    loadPacksAK("DM")


y <- rnorm(10)
round.attr(y) <- 3
y
is.tm(y)
attributes(y)
class(y)

## removing class "tm"
class(y) <- setdiff(class(y),"tm")
y           ## not "tm" thus print.tm() method NOT applied
is.tm(y)
y           ## again "tm" thus print.tm() method applied
class(y)

round.attr(y) <- NULL
class(y)    ## no "tm"
attributes(y)

## add "tm" by hand
class(y) <- union(class(y),"tm")
class(y)
y
attributes(y)   ## there are no attributes which would made y an "tm" object
is.tm(y)
class(y)       ## now y doesn't inherit from "tm"

}; rm(dummy)
## ---------------------------------------------------------------------------------------------------------------------—•°




## ---------------------------------------------------------------------------------------------------------------------—•°
