run_analysis <- function(directory1,directory2) {
  # set the path to the files where they were already downloaded
  # note the working directory is 
  # "C:/my stuff/sabbatical stuff/data science/getting and cleaning data/course project/UCI HAR Dataset"
  
  # read in the labels for the features and activities of the data
  features_labels <- read.table("features.txt",sep="")
  activities_labels <- read.table("activity_labels.txt",sep="")
  
  # set the directory for the train and test data
  directory1 <- "train"
  directory2 <- "test"
  
  traindir <- paste(getwd(),"/",directory1,sep="")
  testdir <- paste(getwd(),"/",directory2,sep="")
  
  # the data sets to be combined are:
  # 1. the activity data plus its labels (x_train and features)
  # 2. then the subjects of that data (... add the subjects)
  # 3. then label the data (...)
  
  # do the training data first then the test data then merge them
  
  # TRAINING DATA
      # read in the data
      train_x <- read.table(paste(traindir,"/X_train.txt",sep=""),sep="")
      
      # label the data
      names(train_x) <- features_labels$V2
      
      # read in the activities and label the column
      train_y <- read.table(paste(traindir,"/y_train.txt",sep=""),sep="")
      names(train_y) <- c("activities")
      
      # read in the subjects performing the activities and label the column
      train_sub <- read.table(paste(traindir,"/subject_train.txt",sep=""),sep="")
      names(train_sub) <- c("subjects")
      
      # combine the data as columns
      train_y_x <- cbind(train_y,train_x)
      train_sub_y_x <- cbind(train_sub,train_y_x)
  

  # TEST DATA
      # read in the data
      test_x <- read.table(paste(testdir,"/X_test.txt",sep=""),sep="")
      
      # label the data
      names(test_x) <- features_labels$V2
      
      # read in the activities and label the column
      test_y <- read.table(paste(testdir,"/y_test.txt",sep=""),sep="")
      names(test_y) <- c("activities")
      
      # read in the subjects performing the activities and label the column
      test_sub <- read.table(paste(testdir,"/subject_test.txt",sep=""),sep="")
      names(test_sub) <- c("subjects")
      
      # combine the data as columns
      test_y_x <- cbind(test_y,test_x)
      test_sub_y_x <- cbind(test_sub,test_y_x)      
      
  # now combine the train and test data
  train_test_data <- rbind(train_sub_y_x,test_sub_y_x)
  
  # subset the data to give only those columns with mean or std in the name of the column
  train_test_data_extr <-  train_test_data[,c("subjects","activities",colnames(train_test_data)[grep("mean\\(\\)",colnames(train_test_data))],colnames(train_test_data)[grep("std\\(\\)",colnames(train_test_data))])]
 
  # modify activities column to use a descriptive name from the activities_labels
  train_test_data_extr$activities <- activities_labels$V2[match(train_test_data_extr$activities, activities_labels$V1)]
  
  # relabel the data set with more descriptive names
  colnames(train_test_data_extr) <- gsub("^t", "Time", colnames(train_test_data_extr))
  colnames(train_test_data_extr) <- gsub("^f", "Frequency", colnames(train_test_data_extr))
  colnames(train_test_data_extr) <- gsub("std\\()", "StandardDev", colnames(train_test_data_extr))
  colnames(train_test_data_extr) <- gsub("mean\\()", "Mean", colnames(train_test_data_extr))
  colnames(train_test_data_extr) <- gsub("Acc", "Acceleration", colnames(train_test_data_extr))
  colnames(train_test_data_extr) <- gsub("Gyro", "Gyroscope", colnames(train_test_data_extr))
  colnames(train_test_data_extr) <- gsub("Mag", "Magnitude", colnames(train_test_data_extr))
  colnames(train_test_data_extr) <- gsub("BodyBody", "Body", colnames(train_test_data_extr))
  
  # create independent data set with average of activties by subject
  train_test_data_avg <- aggregate(. ~subjects + activities, train_test_data_extr, mean)
  train_test_data_avg <- train_test_data_avg[order(train_test_data_avg$subjects,train_test_data_avg$activities),]
  
  # write it to a file
  write.table(train_test_data_avg, file = "train_test_data_avg.txt",row.name=FALSE)
  
}