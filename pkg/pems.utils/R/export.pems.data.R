########################
########################
##export.pems.data
########################
########################

#in place
#################
#pemsData
#



#TO DO
################
#tidy
#document
#



#questions
###################
#develop this subject to requests 
#





########################
########################
##exportData
########################
########################

#version 0.2.0
#karl 17/09/2010


exportData <- function(data, file="tempfile.txt", ..., sep="\t", file.writer = write.table,
                       row.names = FALSE){

    if(isPEMS(data))
        data <- pemsData(data)
    
    file.writer(data, file = file, sep="\t", row.names = row.names)

}

