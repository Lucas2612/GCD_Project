library(dplyr)

directoryName = ".\\UCI HAR Dataset"

# reading X_train File
x_train_row <- read.table(paste(directoryName, "\\train\\X_train.txt", sep=""), nrows=5)
classes_train <- sapply(x_train_row, class)
x_train_tbl <- tbl_df(read.table(paste(directoryName, "\\train\\X_train.txt", sep=""), colClasses=classes_train))

# reading x_test file
x_test_row <- read.table(paste(directoryName, "\\test\\X_test.txt", sep=""), nrows=5)
classes_test <- sapply(x_test_row, class)
x_test_tbl <- tbl_df(read.table(paste(directoryName, "\\test\\X_test.txt", sep=""), colClasses=classes_test))

# merging x_train and x_test files
x_tbl <- rbind(x_train_tbl, x_test_tbl)

# reading y_train file
y_train_tbl <- tbl_df(read.table(paste(directoryName, "\\train\\y_train.txt", sep="")))

# reading y_test file
y_test_tbl <- tbl_df(read.table(paste(directoryName, "\\test\\y_test.txt", sep="")))
y_tbl <- rbind(y_train_tbl, y_test_tbl)


subject_train_tbl <- tbl_df(read.table(paste(directoryName, "\\train\\subject_train.txt", sep="")))
subject_test_tbl <- tbl_df(read.table(paste(directoryName, "\\test\\subject_test.txt", sep="")))
subject_tbl <- rbind(subject_train_tbl, subject_test_tbl)
colnames(subject_tbl) <- "Subject"

features_tbl <- tbl_df(read.table(paste(directoryName, "\\features.txt", sep="")))
features_tbl <- mutate(features_tbl, filter= grepl("mean", V2, ignore.case=TRUE) | grepl("std", V2, ignore.case=TRUE))


column_mean_std <- c(select(filter(features_tbl, filter==TRUE), V1))$V1
name_column_mean_std <- c(select(filter(features_tbl, filter==TRUE), V2))$V2

x_extract <- select(x_tbl, column_mean_std)
name_column_mean_std <- make.names(name_column_mean_std, unique=TRUE)
name_column_mean_std  <- gsub("...", "_", name_column_mean_std, fixed=TRUE)
name_column_mean_std  <- gsub("..", "_", name_column_mean_std, fixed=TRUE)
name_column_mean_std  <- gsub(".", "_", name_column_mean_std, fixed=TRUE)

colnames(x_extract) <- name_column_mean_std

activity_labels_tbl <- tbl_df(read.table(paste(directoryName, "\\activity_labels.txt", sep="")))

activity_names <- select(inner_join(activity_labels_tbl, y_tbl, by="V1"), V2)

colnames(activity_names)[1] <- "Activity"

x_extract <- tbl_df(cbind(x_extract, activity_names))
x_extract <- tbl_df(cbind(x_extract, subject_tbl))

by_activity_subject <- group_by(x_extract, Activity, Subject)
tidy_tbl <- summarise_each(by_activity_subject, funs(mean))

write.table(tidy_tbl, file="tidy_file_avg.txt", row.name=FALSE)