#Step 1 - Begin by checking to see if a data directory exists otherwise create one for us to load our data

if(!file.exists("./data")){dir.create("./data")}

#Step 2 - Read in features data. This data set contains column names of the data that was measured
features <- read.table("features.txt", header = F)

#Step 3 -  Read in the training data set and check for NAs

train <- read.table("X_train.txt", header = F)
any(is.na(test))

#Step 4 - Rename Training data set columns to the names provided in features
colnames(train) <- features[, 2]

#Step 5 - Read in the training data set and check for NAs
test <- read.table("X_test.txt", header = F)
any(is.na(test))

#Step 6 - Rename Training data set columns to the names provided in features
colnames(test) <- features[, 2]

#Step 7 - load the training data set containing activity nunbers from the train directory
l_train <- read.table("train/y_train.txt", header = F)

#Step 8 - load the training data set containing activity nunbers from the train directory
l_test <- read.table("test/y_test.txt", header = F)

#Step 9 - Combine Test & Training Data with activity numbers
y_activity_number <- rbind(l_train,l_test)

#Step 10 - read in activity table which contains activity lables
activity_labels <- read.table("activity_labels.txt", header = F)

#Step 11 - Rename second column of activity labels to "Activity"
names(activity_labels)[2]<-"Activity"

#Step 12 - Replace activity numbers in with Activities from activity table
y_activity_labels  <- merge(y_activity_number,activity_labels,by="V1")

#Step 13 - Remove column 1 of y_act to leave only activity
y_activity_labels$V1 <- NULL
View(y_activity_labels)

#Step 14 -  Read in the training Subject dataset with IDs for each Subject and check for NAs
subject_train <- read.table("./train/subject_train.txt", header = F)
any(is.na(subject_train))

#Step 15 - Rename Column header to Subject
names(subject_train)[1] = "Subject"

#Step 16 -  Read in the test Subject dataset with IDs for each Subject and check for NAs
subject_test <- read.table("./test/subject_test.txt", header = F)
any(is.na(subject_test))

#Step 17 - Rename Column header to Subject
names(subject_test)[1] = "Subject"

#Step 18 - Merge Training and Test Subject ID Data
subjects <- rbind(subject_train,subject_test)

#Step 19 Merge training data set with training subject ID data Set
Train_Data = c(subject_train, train)

#Step 20 Merge test data set with test subject ID data Set
Test_Data = c(subject_test, test)

#Step 21 - Merge Test & Training Data using RBIND
merged_data<- rbind(train,test)

#Step 22 - Rename headers in merged data to attributes that were tested. Provided in features table
colnames(merged_data) <- features[,2]

#Step 23 - Remove all non Mean or Std Columns
new_merged_data <- merged_data[ , grepl("[Mm]ean\\(\\)|std\\(\\)",colnames(merged_data))] 

#Step 24 - Create a new data fram of merged data. Contains Subject ID, Activity, and Test data
t_data <- data.frame(cbind(subjects,y_activity_labels,new_merged_data))

#Step 25 - Convert data fram to table. Will Contain 68 columns and 10299 rows
t_table <- data.table(t_data)

#Step 26 - Create final tiday data set of 30 ROws & 68 Columns by summarizing by Subject/Activity
tidy_data <- t_data%>% group_by(Subject)%>% summarise_each(funs(mean))

#Step 27 - Format and Write tidy data to file for export and viewing
format_tidy_data <- format(tidy_data, digits=4, scientific=F, justify='right')
write.table(format_tidy_data, file='tidyDataSet.txt',row.names=F, sep='\t', quote=F)

#Step 28 - Write Codebook with column names for tidy data set
write.table(colnames(tidy_data), file = 'CodeBook.md', row.name = F, sep='\t', quote=F)
