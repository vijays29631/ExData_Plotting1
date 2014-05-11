plot1 <- function(){

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
#dev.off()
  png(filename="plot1.png", width=480, height=480)
  hist(data$Global_active_power,col="red",xlab='Global Active Power(kilowatts)',
     ylab='frequency',main='Global Active Power')
dev.off()
#return(data)

}