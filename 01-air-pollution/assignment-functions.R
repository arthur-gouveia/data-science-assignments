#########################################################################################################
#                                       Function pollutantmean                                          #
#                                                                                                       #
#Calculates the mean of a given pollutant in a pollution monitor output file                            #
#Parameters                                                                                             #
#directory: A string containing the directory where the output files can be found                       #
#pollutant: A string ("sulfate" or "nitrate")                                                           #
#id:        A numeric vector with the monitors ids of interest                                          #
#                                                                                                       #
#Iterates through a list of files and saves the pollutant means in a vector that is returned            #
#########################################################################################################

pollutantmean <- function(directory, pollutant, id = 1:332) {
        #Initializes the variables
        monitor_data <- data.frame()                    #The data from the monitor output file
        pollutantmeans <- vector(mode = "numeric")      #The vector that will contain the poluttant mean
        files <- list.files(directory, full.names = TRUE)[id]
        
        #reads the column classes from a sample of the first file. Used to optimize the read.csv 
        class_initialization <- read.csv(files[1], nrows = 500)
        classes <- sapply(class_initialization, class)
        
        #Iterating through the file list
        for (f in files) {
                #reads the file and calculates the pollutant mean ignoring the NAs 
                monitor_data <- rbind(monitor_data, read.csv(f, comment.char = "", colClasses = classes))
        }
        #Returns the pollutantmeans
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
#Iterates through a list of files and counts the number of complete cases in each file                  #
#Returns a data frame containing the file id, file name, nobs (num of observations)                     #
#########################################################################################################

complete <- function(directory, id = 1:332) {
        #files contains the full path filenames and file2 contais only the filenames
        files <- list.files(directory, full.names = TRUE)[id]
        files2 <- list.files(directory)[id]
        #The I function doesn`t convert the string to a factor when initializing the data frame
        nobs <- data.frame(0,I("Str"),0)

        #loops through the list of files
        for (i in seq_along(id)) {
                nobs[i,1] <- id[i] #inserts the file ID
                nobs[i,2] <- files2[i] #inserts the file name
                nobs[i,3] <- sum(complete.cases(read.csv(files[i]))) #Inserts the number of valid observations
        }
        names(nobs) <- c("ID", "File Name", "N Obs") #Sets the name of the data frame
        nobs #ane returns it
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
        ccases <- ccases[ccases$"N Obs" > threshold,]
        
        if (nrow(ccases) > 0) {
                files <- paste(directory, ccases$"File Name", sep = "/")
                for (i in 1:length(files)) {
                        data <- read.csv(files[i])
                        cr[i] <- cor(data$sulfate, data$nitrate, use = "complete.obs")
                }
        }
        cr
}








