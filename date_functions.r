calcAge <- function(date1, date2, afronding = 101 ){
  print(date1)
  print(date2)
  diff <- deltaMonths(date1, date2)
  return(diff/12)
}

dateInMonths <- function(d) { 
  lt <- as.POSIXlt(as.Date(d, origin="1900-01-01"))
  return(lt$year*12 + lt$mon) 
  }

deltaMonths <- function(d1, d2) { dateInMonths(d2) - dateInMonths(d1) }
