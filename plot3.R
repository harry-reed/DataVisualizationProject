##########################################################
#                     p l o t 3 . R                      #
#        JHU Data Science Course 4, Assignment 1         #
#                      Part 3 of 4                       #
#                                                        #
# From the data file 'household_power_consumption.txt',  #
# creates a PNG image (480 X 480, titled 'plot3.png'),   #
# a multiple, stair-step line plot of a household's ...  #
#            Sub_metering_1        (Y-axis, black)       #
#            Sub_metering_2        (Y-axis, red)         #
#            Sub_metering_3        (Y-axis, blue) versus #
#            Day & Time            (X-axis)              #
# ... during two days (Feb 1st & 2nd of 2007).           #
#                                                        #
# Please see the README file for a description of the    #
# database format.                                       #
#                                                        #
# Goal is to reproduce the plot image presented in the   #
# first class assignment, part three (Plot 3).           #
##########################################################

##########################################################
# Begin code section common to all plot assignments.     #
#                                                        #
# Purpose of the common code is to ...                   #
#                                                        #
#          1. Open the raw data file, converting the     #
#             numeric fields as they are read-in for     #
#             efficiency. The Date and Time values are   #
#             initially entered as character strings.    #
#                                                        #                              #          2. Exclude data not associated with Feb 1st   #
#             or 2nd 2007 to significantly reduce        #
#             subsequent processing complexity.          #
#                                                        #
#          3. Convert Date and Time strings to a form    #
#             of date recognized / sorted by R.          #
#                                                        #
#          4. Further reduce complexity keeping only     #
#             columns necessary to produce all 4 plots.  #
#                                                        #
# NOTE: Would convert this to a function for production- #
#       quality result. For now, just left as-is.        #
##########################################################

# Modify 'dataFile' to point to 'household_power_consumption.txt'.
dataFile <- "household_power_consumption.txt"

# Define data format to speed-up reading and conversion.
dataFormat <- c(
  "character", ## Date: Calendar Date '%d/%m/%Y' As string
  "character", ## Time: Clock Time '%H:%H:%S'    As string
    "numeric", ## Global_active_power: household, minute-avg (kW)
    "numeric", ## Global_reactive_power: household, minute-avg (kW)
    "numeric", ## Voltage: minute-avg (volt)
    "numeric", ## Global_intensity: household, minute-avg (amp)
    "numeric", ## Sub_metering_1: kitchen (watt-hour active enrgy)
    "numeric", ## Sub_metering_2: lndry rm (watt-hour active enrgy)
    "numeric"  ## Sub_metering_3: el wtr htr, AC (watt-hr act enrgy)
)

# Get raw data converting as comes in to speed-up process. Doc
# stated no NAs in Date or Time cols.
theData <- read.table(file=dataFile,
                      header=TRUE,
                      sep=";",
                      colClasses=dataFormat,
                      na.strings="?"
)

# Only Feb 1st & 2nd of 2007 data, exclude others (huge reduction).
theData <- subset(theData, Date=="1/2/2007" | Date=="2/2/2007")

# Paste Date and Time strings and convert to a form that R expects.
theData$DateTime <- strptime(paste(theData[,1], theData[,2], sep="_"),
                             "%d/%m/%Y_%H:%M:%S")

# Keep only visualized columns. Beside column see plot(s) where used.
theData <- data.frame(theData[,c(
                      "DateTime",               ## Plots 2,3,4
                      "Global_active_power",    ## Plots 1,2,4
                      "Global_reactive_power",  ## Plot 4
                      "Voltage",                ## Plot 4
                      "Sub_metering_1",         ## Plots 3,4
                      "Sub_metering_2",         ## Plots 3,4
                      "Sub_metering_3"          ## Plots 3,4
)])

##########################################################
# Begin code section specific to plot assignment 3.      #
#                                                        #
# Plot is a multiple, stair-step line plot of ...        #
#                                                        #
#             Sub_metering_1        (Y-axis, black)      #
#             Sub_metering_2        (Y-axis, red)        #
#             Sub_metering_3        (Y-axis, blue)       #
#             Day & Time            (X-axis)             #
#                                                        #
# NOTE: Plot created is a PNG image with 480X480 pixel   #
#       resolution, named 'plot3.png'.                   #
##########################################################

if (file.exists("plot3.png")) {          ## Remove old plot result.
    file.remove("plot3.png")
}

png("plot3.png", width=480, height=480)  ## Plot display is PNG.

    with(theData, {

        # Determine x and y value ranges to size area.
        xrange <- range(DateTime)
        y_max <- max(max(Sub_metering_1),
                     max(Sub_metering_2),
                     max(Sub_metering_3)
                 )
        yrange <- c(0, y_max)
        
        # Create the plot area without plotting the points.
        plot(xrange, yrange,
            type="n",                    ## Area size, no plt pnts.
            mar=c(3,3,1,1),              ## Plot margin.
            oma=c(0,0,0,0),              ## No outer margin 4 img.
            mgp=c(2,0.75,0),             ## Label, valu, tick dist.
            cex.lab=0.75,                ## Label font size.
            cex.axis=0.75,               ## Axis value font size.
            xlab="",
            ylab="Energy sub metering"
        )

        # Create Sub_metering_1 stair-step line plot in black.
        lines(DateTime, Sub_metering_1,
            type="s",                    ## Stair-step line type
            col="black"
        )

        # Create Sub_metering_2 stair-step line plot in red.
        lines(DateTime, Sub_metering_2,
            type="s",                    ## Stair-step line type
            col="red"
        )

        # Create Sub_metering_3 stair-step line plot in blue.
        lines(DateTime, Sub_metering_3,
            type="s",                    ## Stair-step line type
            col="blue"
        )

        # Create legend with lines and names.
        legend("topright",
            legend=c( "Sub_metering_1",
                      "Sub_metering_2", 
                      "Sub_metering_3" ),
            lwd=1,                        ## Line thick be4 label
            col=c("black","red","blue"),  ## Color of lines be4
            cex=0.75,                     ## Text size of labels
            horiz=FALSE
        )

    })  ## End 'with(theData'

dev.off()                                ## Close device when done.