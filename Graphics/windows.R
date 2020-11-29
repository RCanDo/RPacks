windows <- function(...){
  
  if(Sys.info()[['sysname']] == "Widows"){
    windows(...)
  }else{
    X11(...)
  }
  
}
