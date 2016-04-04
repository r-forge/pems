##########################
##########################
##above all else code 
##(hidden here because 
## first .r code file 
## alphabetically)
##########################
##########################

#example
#utils::globalVariables(c("a", "othervar"))

globalVariables(c("pems.scheme", "ref.unit.conversions", "ref.chem", "ref.diesel", "ref.petrol",
                  "panel.surfaceSmooth"))

#panel.surfaceSmooth only needed until new version of loa gets on CRAN...




#setup

setup <- function(){
             print("Setting up for pems.utils")
             print("(this should run without errors, warnings)")
             
             #do following for all packages unique to non-CRAN build

#             if(length(find.package("segmented", quiet=TRUE))<1){
#                 warning("adding missing package: segmented")
#                 install.packages("segmented")
#             }

             #then after installing package, if library or require fails 
             #pems.utils:::setup()
                 
         }

