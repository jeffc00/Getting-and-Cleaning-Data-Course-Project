The dataset includes the following files:
=========================================
- 'features_info.txt': Shows information about the variables used on the feature vector.
- 'features.txt': List of all features.
- 'activity_labels.txt': Links the class labels with their activity name.
- 'train/X_train.txt': Training set.
- 'train/y_train.txt': Training labels.
- 'test/X_test.txt': Test set. / Each row identifies the subject who performed the activity for each window sample. Its range is from 1 to 30.
- 'test/y_test.txt': Test labels. / Each row identifies the subject who performed the activity for each window sample. Its range is from 1 to 30.

The R script called run_analysis.R does the following:
======================================================
1. Merges the training and the test sets to create one data set.
2. Extracts only the measurements on the mean and standard deviation for each measurement.
3. Uses descriptive activity names to name the activities in the data set.
4. Appropriately labels the data set with descriptive variable names.
5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.