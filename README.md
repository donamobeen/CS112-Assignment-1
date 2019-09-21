# CS112-Assignment-1

foo <- read.csv("https://tinyurl.com/yb4phxx8")
names(foo)
dim(foo)
head(foo)
date.columns <- c(11, 12, 14, 15, 16, 17, 18, 25)
for(i in date.columns)
    {
    which_values_are_missing <- which(as.character(foo[, i]) == "")
    foo [which_values_are_missing, i] <- NA
    foo [, i] <- as.Date(as.character(foo[, i]))
    }
which.have.NAs <- which(is.na(foo$CirculationDate == TRUE))

foo_no_NAs <- foo[-which.have.NAs, ]

two_k_9 <- (which(foo_no_NAs$CirculationDate >= '2009-01-01'))

foo_09_no_NAs<- foo_no_NAs[two_k_9,]
dim(foo_09_no_NAs)




#Question 1a
planned_duration <- as.numeric(foo_09_no_NAs$OriginalCompletionDate - foo_09_no_NAs$ApprovalDate)

mean(planned_duration, na.rm=TRUE)

median(planned_duration, na.rm=TRUE)

quantile(planned_duration, na.rm=TRUE)
Histogram <- hist(as.numeric(((planned_duration)/365)*12), xlab = "Planned Duration (in months)") #creates a histogram for the planned duration
Histogram



#Question 1b
#Removing NAs from Original Completion Date and Revised Completion Date
#Calculating the project delay by subtracting the Original Completion Date from the Revised Completion Date

foo_09_OCD=subset(foo, foo$OriginalCompletionDate>= "2009-01-01")
foo_09_RCD=subset(foo, foo$RevisedCompletionDate>= "2009-01-01")

which.have.NAs <- which(is.na(foo_09_no_NAs$OriginalCompletionDate == TRUE))
foo_09_no_NAs_OCD <- foo_09_no_NAs[-which.have.NAs, ]
dim(foo_09_no_NAs_OCD)

which.have.NAs <- which(is.na(foo_09_no_NAs$RevisedCompletionDate == TRUE))
foo_09_no_NAs_RCD <- foo_09_no_NAs[-which.have.NAs, ]
dim(foo_09_no_NAs_RCD)

project_delay <- as.numeric(foo_09_no_NAs_RCD$RevisedCompletionDate - foo_09_no_NAs_OCD$OriginalCompletionDate)
summary(project_delay)






#Question 1c
which.have.NAs <- which(is.na(foo_09_no_NAs$OriginalCompletionDate == TRUE))
foo_09_no_NAs_OCD <- foo_09_no_NAs[-which.have.NAs, ]
actual_duration <- as.numeric(foo_09_no_NAs$RevisedCompletionDate - foo_09_no_NAs$ApprovalDate)
planned_duration <- as.numeric(foo_09_no_NAs_OCD$OriginalCompletionDate - foo_09_no_NAs_OCD$ApprovalDate)

mean(actual_duration)
median(actual_duration)
quantile(actual_duration)

mean(planned_duration)
median(planned_duration)
quantile(planned_duration)

difference = (mean(actual_duration) - mean(planned_duration))
difference
Histogram <- hist(as.numeric(((actual_duration)/365)*12), xlab = "Actual Duration")





#Question 2

foo_10=subset(foo, foo$CirculationDate>= "2010-01-01")
which.have.NAs <- which(is.na(foo_10$Rating == TRUE))
foo_10_no_NAs <- foo_10[-which.have.NAs, ]

dim(foo_10_no_NAs)


rating_zero <- foo_10_no_NAs[which(foo_10_no_NAs$Rating=='0'),] #identifies the rows with rating = 0
rating_one <- foo_10_no_NAs[which(foo_10_no_NAs$Rating=='1'),] #identifies the rows with rating = 1
rating_two <- foo_10_no_NAs[which(foo_10_no_NAs$Rating=='2'),] #identifies the rows with rating = 2
rating_three <- foo_10_no_NAs[which(foo_10_no_NAs$Rating=='3'),]
rating0 <- nrow(rating_zero)/nrow(foo_10_no_NAs) *100
rating1 <- nrow(rating_one)/nrow(foo_10_no_NAs) *100
rating2 <- nrow(rating_two)/nrow(foo_10_no_NAs) *100
rating3 <- nrow(rating_three)/nrow(foo_10_no_NAs) *100
print(rating0) 
print(rating1)
print(rating2)
print(rating3)




#Question 3

PATA <- subset(foo_10_no_NAs, foo_10_no_NAs$Type == "PATA")
#aggregate(data.frame(count = foo$Type), list(value = foo$Type), length)

rating_zero_PATA = which(PATA$Rating ==0) 

rating_one_PATA = which(PATA$Rating ==1) 

rating_two_PATA = which(PATA$Rating ==2) 

rating_three_PATA = which(PATA$Rating ==3) 

Percentage0_PATA = (length(rating_zero_PATA)/nrow(PATA))*100

Percentage1_PATA = (length(rating_one_PATA)/nrow(PATA))*100

Percentage2_PATA = (length(rating2_PATA)/nrow(PATA))*100

Percentage3_PATA = (length(rating3_PATA)/nrow(PATA))*100

print(Percentage0_PATA)
print(Percentage1_PATA)
print(Percentage2_PATA)
print(Percentage3_PATA)




#Question 4
#Removing all NAs from "Rating"
which.have.NAs <- which(is.na(foo_no_NAs$Rating == TRUE))
foo_no_NAs_RA <- foo_no_NAs[-which.have.NAs, ]

#Filters out the top 10% of budgets in the revised amount
Upper10 <- head(foo_no_NAs_RA[order(foo_no_NAs_RA$RevisedAmount,decreasing=T),],0.10*nrow(foo_no_NAs_RA))

#Filters out the bottom 10% of budgets in the revised amount
Lower10 <- head(foo_no_NAs_RA[order(foo_no_NAs_RA$RevisedAmount,decreasing=F),],0.10*nrow(foo_no_NAs_RA))

#Calculating means of the upper 10% and lower 10% ratings
mean(Upper10$Rating)
mean(Lower10$Rating)

#Calculating means of the upper 10% and lower 10% ratings of budgets for Education in the category "LTAA"
which.Education <- which(Upper10$LTAA == "Education")
Upper10.Edu <- Upper10[which.Education,]
Lower10.Edu <- Lower10[which.Education,]

mean(Upper10.Edu$Rating)
mean(Lower10.Edu$Rating)

#Calculating means of the upper 10% and lower 10% ratings of budgets for Health in the category "LTAA"
which.Health <- which(Lower10$LTAA == "Health")
Lower10.Health <- Lower10[which.Health,]
Upper10.Health <- Upper10[which.Health,]

mean(Lower10.Health$Rating)
mean(Upper10.Health$Rating)
