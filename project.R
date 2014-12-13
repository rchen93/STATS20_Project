### STATS 20 Project: Group 12 ###

### Locations ###
# C:/Users/Roger Chen/Dropbox/STATS/20/Project/usnews.dat.txt
# C:/Users/Roger/Dropbox/STATS/20/Project/usnews.dat.txt

### Libraries ###
library(ggplot2)
library(lattice)
library(reshape2)
library(plyr)
library(MASS)

### Reading data ###
usnews.data <- read.csv("C:/Users/Roger Chen/Dropbox/STATS/20/Project/usnews.dat.txt", 
                        header=F, na.strings="*")
names <- c("FICE", "College name", "State", "School_type",
           "Average Math SAT score", "Average Verbal SAT score", "Average Combined SAT score",
           "Average ACT score", "First quartile - Math SAT", "Third quartile - Math SAT",
           "First quartile - Verbal SAT", "Third quartile - Verbal SAT",
           "First quartile - ACT", "Third quartile - ACT", "Applications_received",
           "Applicants_accepted", "New_students_enrolled", 
           "Percentage of new students from top 10% of H.S. class", 
           "Percentage of new students from top 25% of H.S. class",
           "Fulltime_undergrads", "Parttime_undergrads",
           "Instate_tuition", "Outstate_tuition", "Room_board_costs",
           "Room_costs", "Board_costs", "Additional_fees", "Estimated_book_costs",
           "Estimated_personal_spending", "Percentage_faculty_Phd",
           "Percentage_faculty_terminal_degree", "Student_faculty_ratio",
           "Percentage_alumni_donate", "Instructional_expenditure_per_student",
           "Grad_rate")
colnames(usnews.data) <- names

### Data pre-processing ###
drop <- c("FICE", "Average Math SAT score", "Average Verbal SAT score", "Average Combined SAT score",
               "Average ACT score", "First quartile - Math SAT", "Third quartile - Math SAT",
               "First quartile - Verbal SAT", "Third quartile - Verbal SAT", 
               "First quartile - ACT", "Third quartile - ACT", 
               "Percentage of new students from top 10% of H.S. class", 
               "Percentage of new students from top 25% of H.S. class")
temp.data <- usnews.data[, !(names(usnews.data) %in% drop)]  # Drop irrelevant columns
final.data <- na.omit(temp.data)  # Remove rows with missing values
# Transform factors into proper data types
final.data <- transform(final.data, 
                        Applications_received = as.numeric(Applications_received),
                        Applicants_accepted = as.numeric(Applicants_accepted),
                        New_students_enrolled = as.numeric(New_students_enrolled),
                        Fulltime_undergrads = as.numeric(Fulltime_undergrads),
                        Parttime_undergrads = as.numeric(Parttime_undergrads),
                        Instate_tuition = as.numeric(Instate_tuition),
                        Outstate_tuition = as.numeric(Outstate_tuition),
                        Room_board_costs = as.numeric(Room_board_costs),
                        Room_costs = as.numeric(Room_costs),
                        Board_costs = as.numeric(Board_costs),
                        Additional_fees = as.numeric(Additional_fees),
                        Estimated_book_costs = as.numeric(Estimated_book_costs),
                        Estimated_personal_spending = as.numeric(Estimated_personal_spending),
                        Percentage_faculty_Phd = as.numeric(Percentage_faculty_Phd),
                        Percentage_faculty_terminal_degree = as.numeric(Percentage_faculty_terminal_degree),
                        Student_faculty_ratio = as.numeric(Student_faculty_ratio),
                        Percentage_alumni_donate = as.numeric(Percentage_alumni_donate),
                        Instructional_expenditure_per_student = as.numeric(Instructional_expenditure_per_student),
                        Grad_rate = as.numeric(Grad_rate))

final.data$School_type[final.data$School_type == 1] <- "Public"
final.data$School_type[final.data$School_type == 2] <- "Private"

# Data without college/state columns
drop <- c("College.name", "State")
no.id.data <- final.data[, !(names(final.data) %in% drop)]

# Creates dataframe with school type as a numeric
school.type.data <- final.data
school.type.data$School_type <- as.factor(school.type.data$School_type)
school.type.data$School_type <- as.numeric(school.type.data$School_type)

sapply(final.data, class)  # Check that relevant columns are numeric
                        
# Add total tuition = Instate + Outstate tuition column to data
Total_tuition <- rowSums(final.data[, c("Instate_tuition", "Outstate_tuition")])
final.data <- cbind(final.data, Total_tuition)  # Adds total tuition column to data

# Separate data into public/private schools
public.data <- subset(final.data, School_type == "Public")
private.data <- subset(final.data, School_type == "Private")

# NOTE: AK, AZ, HI, WY only have 1 data point

### Summary statistics ###

# Creates boxplot of graduation rate by school type
ggplot(final.data, aes(x=School_type, y=Grad_rate, fill=School_type)) +
  geom_boxplot() + guides(fill=FALSE) +
  stat_summary(fun.y=mean, geom="point", shape=1, size=5) +
  xlab("School Type") + ylab("Graduation rate") + 
  ggtitle("Graduation Rate vs School Type")

# Creates five-number summary of graduation rates
summary(public.data$Grad_rate)
summary(private.data$Grad_rate)

# Calculates the average graduation rate for each state based on public/private
avg.gradrate.public <- ddply(public.data, .(State), summarize, 
                      Avg_gradrate=mean(Grad_rate))

avg.gradrate.private <- ddply(private.data, .(State), summarize,
                              Avg_gradrate=mean(Grad_rate))

# Creates bargraph of average graduation rate by state for public/private
ggplot(avg.gradrate.public, aes(x=State, y=Avg_gradrate)) + 
  geom_bar(stat="identity", fill="lightblue", colour="black") + 
  coord_flip() + ylab("Average graduation rate") + 
  ggtitle("Average graduation rate for Public Universities")

ggplot(avg.gradrate.private, aes(x=State, y=Avg_gradrate)) + 
  geom_bar(stat="identity", fill="lightpink", colour="black") + 
  coord_flip() + ylab("Average graduation rate") +
  ggtitle("Average graduation rate for Private Universities")

# Creates boxplot of tuition by school type
ggplot(final.data, aes(x=School_type, y=Instate_tuition, fill=School_type)) +
  geom_boxplot() + xlab("School Type") + ylab("Instate tuition") +
  guides(fill=FALSE) + stat_summary(fun.y=mean, geom="point", shape=1, size=5) +
  ggtitle("Instate Tuition vs School Type")

# Creates five-number summary of instate tuition by school type 
summary(public.data$Instate_tuition)
summary(private.data$Instate_tuition)

# Creates average instate/outstate tuition by state
avg.tuition <- ddply(final.data, .(State), summarize, 
                     avg_instate = mean(Instate_tuition),
                     avg_outstate = mean(Outstate_tuition))

# Changes average tuition data into long format
tuition.long <- melt(
  avg.tuition,
  id.vars = "State",
  measure.vars = c("avg_instate", "avg_outstate"),
  variable.name="Tuition_Type",
  value.name="Tuition")

# Creates a stacked bargraph of tuition by state
ggplot(tuition.long, aes(State, Tuition, fill=Tuition_Type)) +
  geom_bar(stat = "identity", colour="black") +
  scale_fill_brewer(palette="Pastel1") + 
  guides(fill=guide_legend(title="Tuition Type")) +
  coord_flip() + ggtitle("Instate/Outstate Tuition per State")

### Investigative questions ###

# Can we explain instate tuition as a model of other variables?

# t-test to compare public/private
t.test(public.data$Instate_tuition, private.data$Instate_tuition)

# PUBLIC MODEL
public.instate.tuition.model <- lm(Instate_tuition ~ Applications_received + Applicants_accepted +
                             New_students_enrolled + Fulltime_undergrads + Parttime_undergrads +
                             Room_board_costs + Additional_fees + 
                             Estimated_book_costs + Estimated_personal_spending + 
                             Percentage_faculty_Phd + Percentage_faculty_terminal_degree + 
                             Student_faculty_ratio + Percentage_alumni_donate +
                             Instructional_expenditure_per_student, data=public.data)
summary(public.instate.tuition.model)
par(mfrow=c(2,2))
plot(public.instate.tuition.model)
stepAIC(public.instate.tuition.model, direction="backward")  # Remove insignificant variables

# PRIVATE MODEL
private.instate.tuition.model <- lm(Instate_tuition ~ Applications_received + Applicants_accepted +
                               New_students_enrolled + Fulltime_undergrads + Parttime_undergrads +
                               Room_board_costs + Additional_fees + 
                               Estimated_book_costs + Estimated_personal_spending + 
                               Percentage_faculty_Phd + Percentage_faculty_terminal_degree + 
                               Student_faculty_ratio + Percentage_alumni_donate +
                               Instructional_expenditure_per_student, data=private.data)
summary(private.instate.tuition.model)
par(mfrow=c(2,2))
plot(private.instate.tuition.model)
stepAIC(private.instate.tuition.model, direction="backward")  # Remove insignificant variables

# Does a higher student/faculty ratio affect instructional costs?

# Calculates correlation between student/faculty ratio and instructional expenditure 
cor(final.data$Student_faculty_ratio, final.data$Instructional_expenditure_per_student)

# OVERALL MODEL
instructional.ratio.model <- lm(Instructional_expenditure_per_student ~ Student_faculty_ratio, 
                                data=final.data)
summary(instructional.ratio.model)
par(mfrow=c(2,2))
plot(instructional.ratio.model)

# Creates scatter plot of instructional expenditure vs student/faculty ratio
ggplot(final.data, aes(x=Student_faculty_ratio, y=Instructional_expenditure_per_student,
                       colour=School_type)) + geom_point() +
  geom_smooth(method=lm, se=FALSE, fullrange=TRUE) +
  geom_abline(aes(intercept=instructional.ratio.model$coefficients[1], 
                  slope=instructional.ratio.model$coefficients[2],
                  data=final.data)) +
  xlab("Student/faculty ratio") + ylab("Instructional expenditure per student")

# Do higher instructional costs lead to higher graduation rates?
gradrate.model <- lm(Grad_rate ~ Instructional_expenditure_per_student,
                     data=final.data)
summary(gradrate.model)

# Creates scatter plot of instructional expenditure vs graduation rate
ggplot(final.data, aes(x=Instructional_expenditure_per_student, y=Grad_rate)) + 
  geom_point() + 
  geom_smooth(method=lm, se=FALSE, fullrange=TRUE) +
  xlab("Instructional expenditure per student") + ylab("Graduation Rate")

# Can we predict the number of applications received
pairs(~Applications_received + Fulltime_undergrads + Parttime_undergrads +
        Instate_tuition + Outstate_tuition + Room_board_costs + Additional_fees + 
        Estimated_book_costs + Estimated_personal_spending + Percentage_faculty_Phd +
        Percentage_faculty_terminal_degree + Student_faculty_ratio + 
        Percentage_alumni_donate + Instructional_expenditure_per_student + Grad_rate, 
      data=no.id.data)

pairs(~log(Applications_received) + Fulltime_undergrads + Parttime_undergrads +
        Instate_tuition + Outstate_tuition + Room_board_costs + Additional_fees + 
        Estimated_book_costs + Estimated_personal_spending + Percentage_faculty_Phd +
        Percentage_faculty_terminal_degree + Student_faculty_ratio + 
        Percentage_alumni_donate + Instructional_expenditure_per_student + Grad_rate, 
      data=no.id.data)

# Creates linear model of applications received
app.received.model1 < - lm(Applications_received ~ . - Applicants_accepted - 
                             New_students_enrolled - Room_costs - Board_costs - Total_tuition,
                           data=no.id.data)
summary(app.received.model1)
par(mfrow=c(2,2))
plot(app.received.model1)

# Creates linear model of log-transformed applications received
app.received.log.model1 <- lm(log(Applications_received) ~ . - Applicants_accepted - 
                           New_students_enrolled - Room_costs - Board_costs - Total_tuition,
                         data=no.id.data)
summary(app.received.log.model1)
par(mfrow=c(2,2))
plot(app.received.log.model1)
