####################################################################
# Course Title: Getting and Cleaning Data
# Course Project
####################################################################
# Variable name convention:
# Prefix
#    df. - data frame, e.g. df.activity_labels
#    dt. - data table, e.g. dt.
#    m.  - matrix, e.g. m.X
####################################################################


run_analysis <- function(fname="getdata_projectfiles_UCI HAR Dataset.zip") {

  ## Reads data
  df.activity_labels <- read.csv(unz(fname, 
          "UCI HAR Dataset/activity_labels.txt"), 
          sep="", header=F)
  df.features <- read.csv(unz(fname, 
          "UCI HAR Dataset/features.txt"), 
          sep="", header=F)
  df.subject_train <- read.csv(unz(fname, 
          "UCI HAR Dataset/train/subject_train.txt"), 
          sep="", header=F)
  df.X_train <- read.csv(unz(fname, 
          "UCI HAR Dataset/train/X_train.txt"), 
          sep="", header=F)
  df.y_train <- read.csv(unz(fname, 
          "UCI HAR Dataset/train/y_train.txt"), 
          sep="", header=F)
  df.subject_test <- read.csv(unz(fname, 
          "UCI HAR Dataset/test/subject_test.txt"), 
          sep="", header=F)
  df.X_test <- read.csv(unz(fname, 
          "UCI HAR Dataset/test/X_test.txt"),
          sep="", header=F)
  df.y_test <- read.csv(unz(fname, 
          "UCI HAR Dataset/test/y_test.txt"), 
          sep="", header=F)
  
  #-----------------------------------------------------------------
  # 1. Merges the training and the test sets to create one data set.
  #-----------------------------------------------------------------
  df.X <- rbind(df.X_train, df.X_test)
  df.y <- rbind(df.y_train, df.y_test)
  df.subject <- rbind(df.subject_train, df.subject_test)
  
  #---------------------------------------------------------------------
  # 2. Extracts only the measurements on the mean and standard deviation 
  #    for each measurement. 
  #---------------------------------------------------------------------
  col_index_mean_std <- grep("-mean()|-std()", df.features[,2])
  colnames(df.X) <- df.features[,2]
  df.X_mean_std <- df.X[,col_index_mean_std]
  
  #--------------------------------------------------------------------------
  # 3. Uses descriptive activity names to name the activities in the data set
  #--------------------------------------------------------------------------
  m.activity_labels <- as.matrix(df.activity_labels)
  map = setNames(m.activity_labels[,2],m.activity_labels[,1])
  m.y <- as.matrix(df.y)
  m.y[] <- map[m.y]
  df.y <- as.data.frame(m.y)
  
  #--------------------------------------------------------------------------
  # 4. Appropriately labels the data set with descriptive variable names.
  #--------------------------------------------------------------------------
  colnames(df.y) <- c("Activity")
  colnames(df.subject) <- c("Subject")
  df.merged <- cbind(df.subject, df.y, df.X_mean_std)
  
  #----------------------------------------------------------------------------
  # 5. From the data set in step 4, creates a second, independent tidy data set 
  #    with the average of each variable for each activity and each subject.
  #----------------------------------------------------------------------------
  df.new <- aggregate(df.X_mean_std, 
                      by=list(df.subject$Subject, 
                      df.y$Activity), 
                      FUN=mean, na.rm=TRUE)
  df.new <-rename(df.new, Subject = Group.1, Activity = Group.2)
  write.table(df.new, file="tidy_data.txt", row.name=FALSE)
}


