# import provided dataset into R

u<- read.csv("Uber Request Data.csv", stringsAsFactors = FALSE, na.strings = TRUE)

# installing & using 'tidyr' package here. will be using the separate() function. The whole motive here is to first separate 
# the date & time from the same datetime column.Storing all the values in a temporary dataframe called u2.

install.packages("tidyr")
require("tidyr")
u2 <- separate(data = u, col = Request.timestamp, into = c("date", "time"), 10)

# will be using the POSIXct function to easily separate the hour component from the time component.
# will also be changing the datatype to numeric. else, the categorization will be messed up.

u2$timep <- as.POSIXct(u2$time, format = "%H:%M") %>% format("%H:%M:%S")
u2$timeo <- as.POSIXct(u2$timep, format = "%H:%M:%S") %>% format("%H")
u2$timeo <- as.numeric(u2$timeo)

# sanity check to ensure the column 'timeo' based on which all the further categorization, plotting
# will happen, is of the correct numeric format

sapply(uber_final,class)

# will now load all the columns together in one data frame

uber_final <- data.frame(u2,u2$timeo)

# will now categorize all the time into 6 different day zones - early morning, morning,
# afternoon, evening, late evening & night. all these will be stored in a new column called Day_category
# proper inundations have been used to ensure good code readability


uber_final$Day_category <- with(uber_final,  ifelse( uber_final$timeo>= 00 & uber_final$timeo<=03, "early morning",
                                            ifelse(uber_final$timeo>= 04 & uber_final$timeo<=07, "morning", 
                                                   ifelse(uber_final$timeo>= 08 & uber_final$timeo<=11, "afternoon",
                                                          ifelse(uber_final$timeo>= 12 & uber_final$timeo<=15, "evening",
                                                                 ifelse(uber_final$timeo>= 16 & uber_final$timeo<=19, "late evening","night"))))))


# the data set is ready for visualisation & infering insights out of it. Column for day categorization - Day_category and 
# the column used for hour is u2.timeo
# will plot the first graph to find number of requests fetched by Uber per hour During different times of the day
# Pickup points to be projected in two different colors

require(ggplot2)
perhour_request_graph <- ggplot(uber_final,aes(x=factor(timeo),fill=factor(Pickup.point))) + geom_bar(position = "dodge")+labs(x="Time (Hours of the day)", y="Cabs Requested per hour")+  labs(title = "Cab Requested per hour vs hours of the day" ,fill="Pickup Area")
perhour_request_graph

# inference 1 - there's a clear complimentary relationship between the airport & city pick-up requests.
# clearly, there is NEGATIVE CO-RELATION BETWEEN airport & city pick-up Requests.
# Ideally, this would mean that there should not be any problem but upon looking more deeply, important insights come to the forefront


# visual cues are gathered to Solve Results Expected point #1 - Visually identify the most pressing problems for Uber.
# the graph is plotted so as to find the frequency of occurance of 2 most difficult problems at hand - cancelled / no cars.

rhrs_status <-  ggplot(data = uber_final, mapping = aes(x = timeo, fill = Status)) +  geom_bar()+labs(title ="Cab Requested per hour ", x= "Booking reqst in a day (hrs)")
rhrs_status

# after plotting rhrs_status, we can easily say that the frequency of occurance of no cars available during evening
# increases as opposed to cancelled trips in the morning time, is what hurting the Uber business




# plotting the graphs with respect to the different time zones of the day gives us a clearer picture.
# morning to afternoon (time ranges ~ 4-11) and late evening to night (time ranges ~ 16-23) are when the most cabs get cancelled
# in the city are & there are no cabs available for pick up in the Airport 


day_cat <- ggplot(uber_final,aes(x=factor(Day_category),fill=factor(Status))) + geom_bar() + facet_wrap(~Pickup.point) + ggtitle("Cab Requested per Day-Category slot") + xlab("Day Category") + ylab("Count of booking Requests")
day_cat


# Next we try to resolve problem statement #2 - Find the types of requests (city-airport or airport-city) for which the gap is the most severe in the identified time slots
# plotting the particular trip Status of 'No Cars Available' selectively against morning / evening rush hours helped in figuring that out.
# the visual cues recieved in the 1st plot is a clear indicator of what times to pick to categorize as Morning & evening Rush hours.


require(dplyr)  
no_cars = uber_final %>% filter( uber_final$Status == "No Cars Available" ) %>% group_by(Driver.id)

no_cars$Rush_hour_check = factor(ifelse(no_cars$timeo>="04" & 
                                           no_cars$timeo <="11",
                                         "Morning_Rush_time",
                                         ifelse(no_cars$timeo>="16" & 
                                                  no_cars$timeo <="23",
                                                "Evening_Rush_time","Normal_Demand_time")))


ggplot(data = no_cars, mapping = aes(x = timeo, fill = Rush_hour_check)) + 
  geom_bar() + facet_wrap(~Pickup.point) + 
    ggtitle("No Cars Available during Peak hours To/from City/Airport") + xlab("Booking hour")


# plotting the particular trip Status of 'Cancelled' selectively against morning / evening rush hours helped in figuring that out.
# the visual cues recieved in the 1st plot is a clear indicator of what times to pick to categorize as Morning & evening Rush hours.
# the gap is the most severe in the identified time slots


cancelled_trip_set = uber_final %>% filter( uber_final$Status == "Cancelled" ) %>% group_by(Driver.id)

cancelled_trip_set$Rush_hour_check = factor(ifelse(cancelled_trip_set$timeo>="00" & 
                                          cancelled_trip_set$timeo <="12",
                                        "Morning_Rush_time",
                                        ifelse(cancelled_trip_set$timeo>="16" & 
                                                 cancelled_trip_set$timeo <="23",
                                               "Evening_Rush_time","Normal_demand_time")))

ggplot(data = cancelled_trip_set, mapping = aes(x = timeo, fill = Rush_hour_check)) + 
  geom_bar() + facet_wrap(~Pickup.point) + 
  theme(title = element_text(size=9, face="bold")) +
  ggtitle("Trip Cancellation during Peak hours To/from City/Airport") + xlab("Booking hour")



# last graph plot showing visually that no cars available is the actual culprit

timeslot_request_count <- ggplot(uber_final,aes(x=factor(Day_category),fill=factor(Status)))
plot3 <- timeslot_request_count+geom_bar(stat="count",position = "stack",col="black")+
  ggtitle("Trips during Different Time Slots")+
  scale_x_discrete(limits=c("Evening_Rush","Morning_Rush","Day_Time",
                            "Late_Night","Pre_Morning"))+
  labs(x="Time Slots",y="Number of Requests")+labs(fill="Trip Status")+
  scale_fill_discrete(limits=c("Trip Completed","No Cars Available","Cancelled"))

plot3


