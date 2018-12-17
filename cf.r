library(lubridate)

setwd("/home/smeester/Documents/AGAI/R/CF-tool")
getwd()
source("QxTable.r")
                                        # set Variables
calDate <- as.Date("2016-01-01")
projectionYears <- 80 # Number of years to calculate CF
projectionMonths <- projectionYears * 12 + 1 
ag14 <- qxt("ag2014")

hx <- read.table("Hxy_tabel.csv",header=T)   # Read the hx table
endHxAge <- as.numeric(tail(rownames(hx),1)) # Set age at which hx table ends
# setup policy holders

people <- read.table("Invoerbestand.csv",sep=";",header=T,na.strings = c(""," ","NA")) 
# test gevalletje mdy(people[[1,"dob"]]) + years(3)

#    P <- data.frame(dob=as.Date("1951-11-15"), sex="M", penAge=65, dobPartner= as.Date("1966-08-15"), sexPartner = "V", Lop = 10000, Lpp = 7000)



# calculate CF AG2014
resultsList <- vector("list",length = nrow(people))
names(resultsList) <- as.character(people$id)
i=1

for( i in 1:nrow(people)){
    cf <- data.frame(t= seq(0, projectionYears,1/12))    
    cf$date <- seq(calDate, by="month", length.out= projectionMonths)

                                        # 1e verzekerde (LOP ENZO)
    cf$age <- seq((mondf(mdy(people$dob[i]), calDate)/12), by=1/12,length.out=projectionMonths)
    cf$payLop <- cf$age >= people$penAge[i]

    if(people$Sex[i]=="M"){ #Bepalen huwelijksfrequentie
        cf$hx <- hx[cbind(pmin(floor(cf[["age"]]),endHxAge)+1,1)]
    } else {
        cf$hx <- hx[cbind(pmin(floor(cf[["age"]]),endHxAge)+1,2)]
    }

    cf$hx[cf[["age"]]>=people$penAge[i]] <- 1 #ivm uitruilbaarpartnerpensioen

    cf$px <- (1- apply(subset(cf, select=c("date","age")),1,
                       function(x){
                           getQx(ag14,people$Sex[i],year=year(x[1]),age=as.numeric(x[2]))
                       })) ^ (1/12)
    
    cf$qx = 1 - cf$px # Bepaal sterftekans per maand
    cf$tPx <-  cumprod(cf$px) # Cummulatieve overlevingskans

    
    # Partner gedeelte
    if(is.na(people$dobPartner[i])){
        if(people$Sex[i]=="M"){ # +3 years if it is a male
            cf$agePartner <- seq((mondf(mdy(people$dob[i]) + years(3), calDate)/12), by=1/12,length.out=projectionMonths) # 3 years age differend unknonw partner
        } else { # -3 years if it is not a male
            cf$agePartner <- seq((mondf(mdy(people$dob[i]) - years(3), calDate)/12), by=1/12,length.out=projectionMonths) # 3 years age differend unknonw partner
        }
    }else{
        cf$agePartner <- seq((mondf(mdy(people$dobPartner[i]), calDate)/12), by=1/12,length.out=projectionMonths)
    }


    cf$py <- (1- apply(subset(cf, select=c("date","agePartner")),1,
                       function(x){
                           getQx(ag14,people$Sex[i],year=year(x[1]),age=as.numeric(x[2]))
                       })) ^ (1/12)

                                        
    cf$py[cf[["age"]]<people$penAge[i]] <- 1 #sterfte ppi zit al in de hx tabel

    cf$qy = 1 - cf$py 
    cf$tPy <-  cumprod(cf$py)
    cf$payLpp <- rep(TRUE,projectionMonths)

    cf$Lop <- people$Lop[i]/12 * cf$tPx * cf$payLop
    
    cf$Lpp <- people$Lpp[i]/12 * (1-cf$tPx) * cf$tPy * cf$payLpp * cf$hx 
        
    resultsList[[i]] <- cf
}

#list <- lapply(resultsList,function(x){x[["Lop"]]})
#mat <- matrix(unlist(list), ncol = length(resultsList))
#mat2 <- matrix(sapply(resultsList,function(x){x[["Lop"]]}), ncol = length(resultsList), byrow=TRUE)
#Lop <- apply(matrix(sapply(resultsList,function(x){x[["Lop"]]}), ncol = length(resultsList), byrow=FALSE),1,sum)
#Lpp <- apply(matrix(sapply(resultsList,function(x){x[["Lpp"]]}), ncol = length(resultsList), byrow=FALSE),1,sum)
#cf <- Lop + Lpp
