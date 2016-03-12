#########################################################################################################
#                                       Function pollutantmean                                          #
#                                                                                                       #
#Calculates the mean of a given pollutant in a pollution monitor output file                            #
#Parameters                                                                                             #
#directory: A string containing the directory where the output files can be found                       #
#pollutant: A string ("sulfate" or "nitrate")                                                           #
#id:        A numeric vector with the monitors ids of interest                                          #
#                                                                                                       #
#Reads all the selected files through lapply and rbinds them with do.call                               #
#########################################################################################################

pollutantmean <- function(directory, pollutant, id = 1:332) {
        #reads the column classes from the 1st 500 rows of the 1st file. Used to optimize the read.csv
        classes <- sapply(read.csv(list.files(directory, full.names = TRUE)[1], nrows = 500), class)
        
        #lapplies the filelist subset to a read.csv and then do.calls rbind on the result list 
        monitor_data <- do.call(rbind, lapply(list.files(directory, full.names = TRUE)[id],read.csv, comment.char = "", colClasses = classes))

        #Returns the pollutant mean
        mean(monitor_data[[pollutant]], na.rm = TRUE)
}



#########################################################################################################
#                                       Function complete                                               # 
#                                                                                                       #
#Returns the number of complete cases in a series of files in a directory                               #
#Parameters                                                                                             #
#directory: A string containing the directory where the output files can be found                       #
#id:        A numeric vector with the file ids of interest                                              #
#                                                                                                       #
#Sapplies an annonymous function to the list of files. This functions counts the number of valid        #
#observations on each file. Then returns a data frame containing the file id, file name and nobs        #
#########################################################################################################

complete <- function(directory, id = 1:332) {
        #the list of selected files
        files <- list.files(directory, full.names = TRUE)[id]
        #selected files number of valid observations
        nobs <- sapply(files, function(x) sum(complete.cases(read.csv(x))), USE.NAMES = FALSE)
        #The I function doesn`t convert the string to a factor when initializing the data frame
        data.frame("id" = id, "file" = I(list.files(directory)[id]),"nobs" = nobs)
}



#########################################################################################################
#                                         Function corr                                                 #
#                                                                                                       #
#Takes a directory of data files and a threshold for complete cases and calculates the correlation      #
#between sulfate and nitrate for monitor locations where the number of completely observed cases (on all#
#variables) is greater than the threshold.                                                              #
#Parameters                                                                                             #
#directory: A string containing the directory where the output files can be found                       #
#threshold: The minimum number of complete cases to calculate the correlation                           #
#                                                                                                       #
#Iterates through a list of files and, if the number of complete cases is bigger than the threshold     #
#calculates the correlation between sulfate and nitrate.                                                #
#Returns a vector with the correlations                                                                 #
#########################################################################################################

corr <- function(directory, threshold = 0) {
        cr <- vector("numeric")
        ccases <- complete(directory)
        ccases <- ccases[ccases$"nobs" > threshold,]
        
        if (nrow(ccases) > 0) {
                files <- paste(directory, ccases$"file", sep = "/")
                data <- lapply(files, read.csv)
                cr <- sapply(data, function(x) cor(x$sulfate, x$nitrate, use = "complete.obs"), USE.NAMES = F)
        }
        cr
}








