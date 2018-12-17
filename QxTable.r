sourceFiles <- c("ag2014", "col2003")
sapply(paste(sourceFiles,".r",sep=""),function(x){source(x)})

qxt <- QxTable <- function(class){

    if(class %in% sourceFiles){
        if(class=="ag2014"){
            return(new("ag2014"))
        } else if(class=="col2003"){
            return(new("col2003"))
        } else {
            return(NULL)
        }
    } else{
        cat("~~~Not a correct class argument~~~\n")
        cat(c("~~~Choose one of", paste(sourceFiles,sep=", "), "~~~~\n"))
    }
}

# A general function needs to be replaced

monnb <- function(d) { lt <- as.POSIXlt(as.Date(d, origin="1900-01-01"));lt$year*12 + lt$mon }

mondf <- function(d1, d2) { monnb(d2) - monnb(d1) }
