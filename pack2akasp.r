DISK="D:"
## clean the environment and run first demanded commands from "\ROBOCZY\R\moj.r"
setwd(paste(DISK,"/ROBOCZY/R/myR",sep=""))
source("../moj.r")

rm(DISK)
ls()
package.skeleton(name = "akasp", environment = .GlobalEnv,
                 path = ".", force = FALSE, namespace = TRUE)
