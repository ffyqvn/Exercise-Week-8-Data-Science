
#week 8
#upload data
student_data<- read.csv("C:/Users/nurfa/Downloads/student_data.csv")
View(student_data)

#student fail
Student_fail<-Student_fail<-student_data[student_data$final_exam_mark< 40,]
View(Student_fail)

mydata1<- student_data %>% filter(final_exam_mark< 40) %>% arrange(desc(final_exam_mark))
View(mydata1)

mydata2<- student_data %>% filter(final_exam_mark< 40) %>% arrange(final_exam_mark)
View(mydata2)

#select function
mydata2 <- student_data%>% select(student_id,coursework_mark,final_exam_mark)
View(mydata2)

#glimpse
glimpse(student_data)

#Mutate
mydata4 <- student_data%>% mutate(Total_Mark=(coursework_mark+ final_exam_mark/200*100))
View(mydata4)

#Descriptive Statistics
data(iris)
View(iris)
head(iris)
tail(iris)
head(iris,10)
tail(iris,10)
str(iris)

min(iris$Petal.Length) 
max(iris$Petal.Length) 
range(iris$Sepal.Length)
sd(iris$Sepal.Length)
var(iris$Sepal.Length)
summary(iris)

#quantile
A<-c(170.2, 181.5, 188.9, 163.9, 166.4, 163.7, 160.4, 175.8, 181.5)
quantile(A)
quantile(A,0.25)
quantile(A,0.75)

#counting column and row
nrow(iris)
ncol(iris)
sum(is.na(iris$Sepal.Length))

#Histogram
hist(iris$Sepal.Length,
     main = "Histogram of Sepal Length",
     xlab = "Sepal Length (cm)",
     ylab = "Frequency",
     col = "lightblue",
     border = "black")

data<-c(30,24,26,28,29,28,27,26,32,34,13,15,14,31,29,28,24,25,30,34,35,27,30,34,44,48)
boxplot(data, main = "Boxplot")
first_q<-quantile(data,0.25)
third_q<-quantile(data,0.75)
inter_q<-quantile(data,0.5)

#Quartile
iqr <- IQR(data)

#lower extreme
lower_extreme<-first_q - 1.5 * iqr

#upper extreme
upper_extreme<-third_q+ 1.5 * iqr

#how to deal with outlier
data_new<-data
data_new<- data_new[!data_new<lower_extreme]
data_new<- data_new[!data_new>upper_extreme]
data_new

#Replace the outlier with mean
data_new<- data
avg <- round(mean(data_new))#for the purpose of example we round up value
data_new[data_new<lower_extreme] <- avg
data_new[data_new>upper_extreme] <- avg
data_new

#replace with upper and lower value
data_new<- data
data_new[data_new<lower_extreme] <- lower_extreme
data_new[data_new>upper_extreme] <- upper_extreme
data_new

#Q1
student_above80 <- student_data[student_data$final_exam_mark > 80, ]
View(student_above80)

#Q2
student_coursework <- student_data %>%
  arrange(coursework_mark)

View(student_coursework)

#Q3
student_result <- student_data %>%
  select(student_name, final_exam_mark)

View(student_result)

#Q4
# Number of rows
nrow(student_data)

# Number of columns
ncol(student_data)

#Q5
str(student_data)
summary(student_data)
glimpse(student_data)
head(student_data)
tail(student_data)

#Q6
#Boxplot
boxplot(student_data$final_exam_mark,
        main="Boxplot of Final Exam Mark",
        ylab="Final Exam Mark")

Q1 <- quantile(student_data$final_exam_mark, 0.25)
Q3 <- quantile(student_data$final_exam_mark, 0.75)
IQR_value <- IQR(student_data$final_exam_mark)

Lower <- Q1 - 1.5 * IQR_value
Upper <- Q3 + 1.5 * IQR_value

outliers <- student_data %>%
  filter(final_exam_mark < Lower |
           final_exam_mark > Upper)

View(outliers)

#sv file

write.csv(Student_fail,
          "C:/Users/nurfa/Downloads/Student_fail.csv",
          row.names = FALSE)

write.csv(mydata1,
          "C:/Users/nurfa/Downloads/mydata1.csv",
          row.names = FALSE)

write.csv(mydata2,
          "C:/Users/nurfa/Downloads/mydata2.csv",
          row.names = FALSE)

write.csv(mydata4,
          "C:/Users/nurfa/Downloads/mydata4.csv",
          row.names = FALSE)

write.csv(student_above80, "C:/Users/nurfa/Downloads/student_above80.csv", row.names = FALSE)

write.csv(student_coursework, "C:/Users/nurfa/Downloads/student_coursework.csv", row.names = FALSE)

write.csv(student_result, "C:/Users/nurfa/Downloads/student_result.csv", row.names = FALSE)

write.csv(outliers, "C:/Users/nurfa/Downloads/outliers.csv", row.names = FALSE)

png("C:/Users/nurfa/Downloads/final_exam_boxplot.png",
    width = 800,
    height = 600)

boxplot(student_data$final_exam_mark,
        main = "Boxplot of Final Exam Mark",
        ylab = "Final Exam Mark",
        col = "lightblue")

dev.off()
png("C:/Users/nurfa/Downloads/histogram_sepal_length.png",
    width = 800,
    height = 600)

hist(iris$Sepal.Length,
     main = "Histogram of Sepal Length",
     xlab = "Sepal Length (cm)",
     ylab = "Frequency",
     col = "lightblue",
     border = "black")

dev.off()