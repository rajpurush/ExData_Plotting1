getdatafromcsv <- function(startdatestr="2007-02-01", enddatestr="2007-02-02", chunksof=1000) {
 		
	# Start date and end date for we need to get the data
	startdate <- as.Date(startdatestr, "%Y-%m-%d")
	enddate <- as.Date(enddatestr, "%Y-%m-%d")
		

	# For performance reason file() is used instead of read.csv. Even if "skip" is used
	# it does seem to reduce the performance. Using skip to traverse the file does not 
	# help from a performance objective.
	# The example below shows that there is no performance benefit with skip:
	# > system.time(t <- read.csv("household_power_consumption.txt", sep=";"))
    #   user  system elapsed 
    #   15.50    0.34   15.86 
    # > system.time(t <- read.csv("household_power_consumption.txt", skip=100000, sep=";"))
    #   user  system elapsed 
    #   14.65    0.26   14.92 
	#  Hence for performance better to use file pointer i.e., fileconn <- file(name, "r") 
	#  to traverse a file and select only a few lines
	fname <- "household_power_consumption.txt"
	fileconn <- file(fname, "r") 
	
	# first line is header information
	line <- readLines(fileconn, 1) 
	headers <- unlist(strsplit(line, ";"))
	
	# Created an empty data frame to hold the data which is collected from csv
	# Note: The column type are statically defined where the user needs to know the type at the coding. 
	#       A preferred approach is to dynamically discover the variable type and specify for the 
    #       data.frame definition
	entries <- data.frame(Date=character(), Time=character(), Global_active_power=numeric(), 
	                 Global_reactive_power=numeric(), Voltage=numeric(), Global_intensity=numeric(), 
					 Sub_metering_1=numeric(), Sub_metering_2=numeric(), Sub_metering_3=numeric(), stringsAsFactors=FALSE)


	datatoberead <- TRUE
	rowcount <- 1
	
	# The data from start and end date is read. 
	# To optimize on memory, only data between start and end are read. The csv file is traversed
	# in chunks (default 1000 rows) until the first entry is found. Then data from csv is populated 
	# into a data.frame until the last entry corresponding to the end date is found. After that the
	# traverse is stopped.
	
	datatoberead <- TRUE
	rowcount <- 1
	
	lines <- readLines(fileconn, chunksof) 
	while ((length(lines) > 0) & (datatoberead)) {
	    
		for (line in lines) {
		    entry <- unlist(strsplit(line, ";"))
			currdate <- as.Date(entry[1], "%d/%m/%Y")
		    if(currdate >= startdate) {
			    if (currdate <= enddate) {
				    
					row <- list(entry[1], entry[2], as.numeric(entry[3]), as.numeric(entry[4]),
					            as.numeric(entry[5]), as.numeric(entry[6]), as.numeric(entry[7]), 
								as.numeric(entry[8]), as.numeric(entry[9]))
		            entries[rowcount,] <- row
					rowcount <- rowcount + 1	
		        }
				else {
				   datatoberead <- FALSE
				   break # after breaking from the loop an additional read is done 
				         # Do'nt know how to do a go so that while loop is broken
						 # Hence using datatoberead variable 
				} 
			}	
	    }		

		lines <- readLines(fileconn, chunksof) 
	}


    close(fileconn)
	
	if (nrow(entries) == 0 ) {
	   stop("invalid: data range not found in input data")
	}
	entries
}

# Change working directory to where the csv file is located       
wd <- "E:/Rprog/ExploratoryAnalysis/week1/assignment"
setwd(wd)	
	
# Get data between "2007-02-01" and "2007-02-02"   
df <- getdatafromcsv(startdatestr="2007-02-01", enddatestr="2007-02-02", chunksof=1000)

# Histogram
hist(df$Global_active_power, col="red", xlab="Global Active Power (kilowatts)", ylab="frequency", main="Global Active Power")
dev.copy(png, file="plot1.png")
dev.off()

   