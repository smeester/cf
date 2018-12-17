setClass(Class="HxFrequency",
         representation=representation(endAge="numeric", startAge="numeric"),contains=c("data.frame"))

if(!isGeneric("getHx")){
    setGeneric(
        name= "getHx",
        def=function(object, sex="character", age="numeric"){standardGeneric("getHx")}
    )
    lockBinding("getHx",.GlobalEnv)
}

setMethod(f="getHx",
          signature = "HxFrequency",
          definition=function(object, sex="character", age="numeric"){
              if(!(sex %in% c("M","F"))){
                  cat("~~~~ sex muste be either M or F~~~~\n")
                  return(NULL)
              }
              if(!missing(age)){
                  age <- floor(age)
                  if(age > object@endAge){
                      age <- object@endAge
                  }
                  if( age < object@startAge){
                      age <- object@startAge
                  }
              }
              if(missing(age)){
                  return(object[sex])
              }
              return(object[[age,sex]])
          })

setMethod(
    f="initialize",
    signature="HxFrequency",
    definition=function(.Object){
        cat("~~~ Hx Frequency: initializator ~~~ \n")

        .Object@startAge <- 0
        .Object@endAge <- 67

        as(.Object,"data.frame") <- as.data.frame(matrix(c(0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.01, 0.05, 0.08, 0.15, 0.15, 0.25, 0.22, 0.35, 0.29, 0.45, 0.36, 0.55, 0.43, 0.65, 0.5, 0.75, 0.54, 0.771, 0.58, 0.792, 0.62, 0.813, 0.66, 0.834, 0.7, 0.85, 0.74, 0.85, 0.78, 0.85, 0.82, 0.85, 0.86, 0.85, 0.9, 0.85, 0.9, 0.85, 0.9, 0.85, 0.9, 0.85, 0.9, 0.85, 0.9, 0.85, 0.9, 0.85, 0.9, 0.85, 0.9, 0.85, 0.9, 0.85, 0.9, 0.85, 0.9, 0.85, 0.9, 0.85, 0.9, 0.85, 0.9, 0.85, 0.9, 0.85, 0.9, 0.84, 0.9, 0.83, 0.9, 0.82, 0.9, 0.81, 0.9, 0.8, 0.9, 0.79, 0.9, 0.78, 0.9, 0.77, 0.9, 0.76, 0.9, 0.75, 0.9, 0.74, 0.9, 0.73, 0.9, 0.72, 0.9, 0.71, 0.9, 0.7, 0.9, 0.69, 0.9, 0.68),byrow=TRUE,ncol=2,nrow=68),
                                                  row.names=as.character(c(0:67)))
        colnames(.Object) <- c("M","F")
        return(.Object)
    })
