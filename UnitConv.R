#' conversion function
#' 
#' this is my description
#' 
#' @param inches numeric US inches
#' @author alle
#' @export 

InchToCm <- function(inch){
  
  cm <- inch*2.54
  return(cm)
}


UnitConv <- function(value, from = c("inch", "cm", "pounds", "kg"), to = c("inch", "pounds", "cm", "kg")){
#from and to identical
  if (from == to){
    result <- value
  
#Height
  #From Inch to...
  } else if (from == "inch"){
    
    #to cm
    if (to == "cm"){
      result <- value*2.54
    }
  
  #From Cm to... 
  } else if (from == "cm") {
    
    #to inch
    if (to == "inch"){
      result <- value/2.54
    }
    
#Weight
  #From pounds to...
  } else if (from == "pounds"){
    
    #to kg
    if (to == "kg"){
      result <- value*.453592
    }
    
  #From kg to...
  } else if (from == "kg"){
    
    #to pounds
    if (to == "pounds"){
      result <- value/.453592
    }
  }
  
  return(result)
}

UnitConv2 <- function(value, from = c("inch", "cm", "pounds", "kg"), to = c("inch", "pounds", "cm", "kg")){
  
  #from and to identical
  if (from == to){
    result <- value
  }
  
  else switch(from, 

         #From Inch to...
         "inch" = {
           
           #to cm
           if (to == "cm"){
             result <- value*2.54
           }
           
           #to m
           else if (to == "m"){
             result <- (value*2.54)/100
           }
         },
         
         #From cm to...
         "cm"= {
           
           #to inch
           if (to == "inch"){
             result <- value/2.54
           }
           
           #to xxx
           
         },
         
         #From pounds to...
         "pounds"= {
           
           #to kg
           if (to == "kg"){
             result <- value*.453592
           }
           
           #to xxx
         },
         
         #From kg to...
         "kg" = {
           
           #to pounds
           if (to == "pounds"){
             result <- value/.453592
           }
           
           #to xxx
         }
         )

  
  return(result)
}
