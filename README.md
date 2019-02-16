# Coursera *Getting and Cleaning Data* course project

One of the most exciting areas in all of data science right now is wearable computing - see for example [this article](http://www.insideactivitytracking.com/data-science-activity-tracking-and-the-battle-for-the-worlds-top-sports-brand/). Companies like Fitbit, Nike, and Jawbone Up are racing to develop the most advanced algorithms to attract new users.

In this project, data collected from the gyroscope  and accelerometer of the Samsung Galaxy S smartphone. The data was cleaned and transformed in a tidy data. The tidy data can be used for later analysis.

This repository contains the following files:

- `README.md` : provide an overview of the data set and how it was created.
- `tidy_data.txt`: contains the data set.
- `CodeBook.md`: contents of the data set. The file describe the data, variables and transformations used to generate the tidy data in tidy_data.txt
- `run_analysis.R`, the R script that was used to create the data set. 

## Data description <a name="study-design"></a>

The source data set that this project was based on was obtained from the [Human Activity Recognition Using Smartphones Data Set](http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones#), which describes how the data was initially collected.

Training and test data were first merged together to create one data set, then the measurements on the mean and standard deviation were extracted for each measurement (79 variables extracted from the original 561), and then the measurements were averaged for each subject and activity, resulting in the final data set.

## Data set creating<a name="creating-data-set"></a>

In order to create the tdy data set `run_analysis.R` can be used. It download the data from the source and produce the final data with the following stpes:

- Download and unzip source data if it doesn't exist.
- Read data.
- Merge the training and the test sets to create one data set.
- Extract only the measurements on the mean and standard deviation for each measurement.
- Use descriptive activity names to name the activities in the data set.
- Appropriately label the data set with descriptive variable names.
- Create a second, independent tidy set with the average of each variable for each activity and each subject.
- Write the data set to the `tidy_data.txt` file.

The `tidy_data.txt` was created by running the `run_analysis.R` script using R version 3.4.4 (2018-03-15) Platform: x86_64-pc-linux-gnu (64-bit) Running under: Ubuntu 16.04.5 LTS
This script requires the `dplyr` package (version 0.7.8 was used).
