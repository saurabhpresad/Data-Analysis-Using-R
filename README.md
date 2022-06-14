# Data-Analysis-Using-R
Data analysis of a telecom company using R


#Importing the libraries

library(stringr)
library(dplyr)
library(ggplot2)
library(lubridate)
library(plotrix)
#Importing the data set
comcast <-  read.csv("D:/My Study Place/Simplilearn/Data Science Using R Studio/Project Data set/Comcast Project/Comcast Telecom Complaints data.csv")
#View(comcast)
#Checking the names of headers 
names(comcast)

#Removing all the special characters from the string and creating names without those characters
names(comcast) <- str_replace_all(names(comcast), "[^[:alnum:]]", "")
names(comcast)


#Changing the format of Date to a dd-mm-yyyy format
comcast$Date<- dmy(comcast$Date)

#Grouping the dataset according to the Data and summarising the count of those tickets on those dates
daily_tickets <- summarise(group_by(comcast,Date),Count_1 = n())
View(daily_tickets)

#converting the Months in Date column as integer
Month <- as.integer(month(comcast$Date))
Month                    

# grouping the dataset according to the months and summarsing it with the counts
monthly_tickets <- summarise(group_by(comcast,Month=as.integer(month(Date))),Count =n())
View(monthly_tickets)

# Renaming the months to factors
Months.name <-  c("Jan","Feb","Mar",
                  "Apr","May","Jun",
                  "Jul","Aug","Sep",
                  "Oct","Nov","Dec")
monthly_tickets$Month <- Months.name[monthly_tickets$Month]
monthly_tickets$Month <- as.factor(monthly_tickets$Month)
View(monthly_tickets)

# Plotting the number of complaints at daily granularity levels
daily_plot <- ggplot(data = daily_tickets,aes(as.POSIXct(Date),Count_1))+
  geom_line(size = 1)+
  geom_point(color='red',size = 2)+
  scale_x_datetime(breaks = "1 weeks",date_labels = "%d/%m")+
  labs(title = "Ticket Counts  - Daily",x= "Days",y ="No. of Tickets")+
  theme(axis.text.x = element_text(angle = 75),
        plot.title = element_text(hjust = 0.5))
daily_plot

#Plotting the number of complaints at monthly granularity level
monthly_plot <- ggplot(monthly_tickets, aes(Month, Count, group = 1)) + 
  geom_point(color='red',size = 2) + 
  geom_line(size =1) + 
  xlab("Months") + 
  ylab("Number of Complaints") +
  ggtitle("Number of complaints per Month")+
  theme(plot.title = element_text(hjust = 0.5))
monthly_plot
#its found that June has the maximum number of tickets compared to other months


#Creating a frequency table of complaint types
#Searching and matching the ticket names from the CustomerComplaint column in the table

network_tickets<- contains(comcast$CustomerComplaint,match = 'network',ignore.case = T)
internet_tickets<- contains(comcast$CustomerComplaint,match = 'internet',ignore.case = T)
billing_tickets<- contains(comcast$CustomerComplaint,match = 'bill',ignore.case = T)
email_tickets<- contains(comcast$CustomerComplaint,match = 'email',ignore.case = T)
charges_ticket<- contains(comcast$CustomerComplaint,match = 'charge',ignore.case = T)
data_tickets <- contains(comcast$CustomerComplaint,match='data',ignore.case = T)

#Assigning names to different complaint types
comcast$ComplaintType[network_tickets] <- 'Network'
comcast$ComplaintType[internet_tickets] <- 'Internet'
comcast$ComplaintType[billing_tickets] <- 'Billing'
comcast$ComplaintType[email_tickets] <- 'Email'
comcast$ComplaintType[charges_ticket] <- 'Charges'
comcast$ComplaintType[data_tickets] <- 'Data'
comcast$ComplaintType[-c(internet_tickets,network_tickets,billing_tickets,email_tickets,service_tickets,charges_ticket)] <- 'Others'

# Creating Frequency table for each complaint type
Complaint_frequency_table_1 <- table(comcast$ComplaintType)
Complaint_frequency_table <- as.data.frame(Complaint_frequency_table_1)
names(Complaint_frequency_table)[1] <- 'Complaint_Type'
names(Complaint_frequency_table)[2] <- 'Frequency'
View(Complaint_frequency_table)

#from the table its clear that the internet related complaints are the highest

#PLotting a pie chart accroding to the distribution of complaints
No_of_tickets <- Complaint_frequency_table$Frequency
Complaint_type <- Complaint_frequency_table$Complaint_Type
pie3D(No_of_tickets,labels=Complaint_type,cex =  1,col=rainbow(length(Complaint_type)),main = "Complaint types and its number of tickets")



#Creating a categorical variable

open <- (comcast$Status =='Open'| comcast$Status == 'Pending')
closed <-  (comcast$Status =='Closed'|comcast$Status=='Solved')

comcast$ComplaintStatus[open] <- 'Open'
comcast$ComplaintStatus[closed] <- 'Closed'

#creating a table with States and its frequency of each complaint status (either open or closed)
stack <- table(comcast$ComplaintStatus,comcast$State)
stack1 <- as.data.frame(stack)
names(stack1)[1]='Complaint Status'
names(stack1)[2]='State '
names(stack1)[3]='Frequency'
View(stack1)

#grouping the state and status and plotting a graph depicting the number of complaints on each state

comcast_Data <- group_by(comcast,State,ComplaintStatus)
chart_data <- summarise(comcast_Data,Count = n())
ggplot(as.data.frame(chart_data) ,mapping = aes(State,Count))+
  geom_col(aes(fill = ComplaintStatus),width = 0.95)+
  theme(axis.text.x = element_text(angle = 90),
        axis.title.y = element_text(size = 15),
        axis.title.x = element_text(size = 15),
        title = element_text(size = 16,colour = "#0073C2FF"),
        plot.title = element_text(hjust =  0.5))+
  labs(title = "Stacked Bar Chart of Complaint Status ",
       x = "States",y = "No of Tickets",fill= "Status")
  
# from the chart its evident that Georgia has the maximum number of complaints     
  
#Checking which state has maximum unresolved complaints  
 states_max_complaints <-  comcast %>% filter(ComplaintStatus=='Open') %>% group_by(State) %>% summarize(NumOfComplaints=n()) %>% arrange(desc(NumOfComplaints))
states_max_complaints <- as.data.frame(states_max_complaints)
View(states_max_complaints)

# from the table, Georgia has the maximum number of unresolved complaints
  
#to find the percentage of unresolved complaints
  
  total<-comcast%>% group_by(ComplaintStatus) %>% summarize(NumOfComplaints=n())
  total
  complaint_freq<-tot$NumOfComplaints
  percnt<-round((complaint_freq/sum(complaint_freq)*100),2)
  percnt
  lbls<-paste(total$ComplaintStatus," ",percnt,"%",sep="")
pie(complaint_freq,labels =lbls)  

# From the pie chart , its clear that 23.25% complaints are open.

#checking the percentage of complaints received via Internet and Customer Care Call

internet_complaints<-comcast %>% filter(ReceivedVia=='Internet',ComplaintStatus=='Closed') %>% group_by(ReceivedVia,ComplaintStatus) %>% summarize(NumOfComplaints=n()) 
customer_care_complaints<-comcast%>% filter(ReceivedVia=='Customer Care Call',ComplaintStatus=='Closed') %>% group_by(ReceivedVia,ComplaintStatus) %>% summarize(NumOfComplaints=n())
internet_percnt<-round(internet_complaints$NumOfComplaints/sum(total$NumOfComplaints)*100,2)
internet_percnt
customer_care_percnt<-round(customer_care_complaints$NumOfComplaints/sum(total$NumOfComplaints)*100,2)
customer_care_percnt

#Percentage of complaints received via Internet : 37.9%
#Percentage of complaints received via Customer Care Calls : 38.85%

