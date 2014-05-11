plot4 <- function(){

  #options(warn=1) #print warnings

get_lines <- c()

conn <- file("household_power_consumption.txt","r") # open a coonection to the file

while(TRUE) {
      rl <- readLines(conn,n=100) 
      indx <- grep("^(01|02|1|2)/(02|2)/2007", rl)          # index vector where the 1/2/2007 or 2/2/2007 recs occur
      get_lines<-c(get_lines, rl[indx])    
      if (length(rl) %% 100 != 0) {
          break
        }
   }

close(conn)

textconn<-textConnection(get_lines,"r") #now we create a text connection and load data
data<-read.csv(textconn,sep=";",header=FALSE)
names(data)<- c("Date", "Time", "Global_active_power","Global_reactive_power","Voltage","Global_intensity","Sub_metering_1",
                "Sub_metering_2","Sub_metering_3")

datetime <- paste(data$Date, data$Time)                   # character vector combining date and time part
datetimeformat <- strptime(datetime, "%d/%m/%Y %H:%M:%S") # specify the format of the input data, returns POSIXlt

png(filename="plot4.png", width=480, height=480)
par(mfrow=c(2,2))
with(data,{
  plot(datetimeformat, data$Global_active_power,type='l', xlab="", 
       ylab="Global Active Power")
  plot(datetimeformat, data$Voltage,type='l', ylab="Voltage")
  plot(datetimeformat, data$Sub_metering_1,type='l', col='black', xlab="",
       ylab="Energy sub metering")
  points(datetimeformat,data$Sub_metering_2, type='l',col = 'red')
  points(datetimeformat,data$Sub_metering_3, type='l', col = 'blue')
  legend("topright",legend = c("Sub_metering_1","Sub_metering_2","Sub_metering_3"), 
         col = c("black", "red", "blue"),bty="n")  # box.col=NA, bty="n" doesn't border the legend)
  plot(datetimeformat, data$Global_reactive_power, type ='l', ylab="Global Reactive Power")
})
dev.off()
#return(data)

}