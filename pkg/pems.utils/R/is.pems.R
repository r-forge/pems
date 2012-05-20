is.pems <- function(x, full.test=TRUE, ...){

   #####################
   #is.pems -two level tester
   #####################
   #kr 23/10/2010 v 0.0.1

   #####################
   #to do
   #####################
   #make test more robust?

   #standard test
   output <- if(is(x)[1]=="pems") TRUE else FALSE
   #full.test
   if(full.test){
       if(is.null(x)) comment(output) <- "NULL" else 
           if(is(x)[1]=="pems") comment(output) <- "pems" else
               if(is.data.frame(x)) comment(output) <- "data.frame" else
                    comment(output) <- "other"
   }
   #output
   output
}
