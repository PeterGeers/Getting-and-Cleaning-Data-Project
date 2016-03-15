# Code Book

## The Data files extracted from Zipfile to be used for purpose
- XTrain  = train/X_train.txt
- XTest    = test/X_test.txt
- YTrain   = train/y_train.txt    
- YTest    = test/y_test.txt
- Featurenames = features.txt
- ActivityNames = activity_labels.txt
- SubjectTrain = subject_train.txt
- SubjectTest  = subject_test.txt

## Subject and Activity
These variables identify the unique subject/activity pair the variables relate to:
 - Subject: the integer subject ID.
 - Activity: the string activity name:
  - Walking
  - Walking Upstairs
  - Walking Downstairs
  - Sitting
  - Standing
  - Laying

## tidy is dataset with only the obeservations for features related to Mean and StdDev

## tidyMeans is the tidy dataset 

- 1. 180 rows (30 by 6 for activities) 
- 2. 66 Observations

## Measurement Means (tidiMeans is Mean of Tidy) List of Features

-  [1] "MeanTimeBodyAccMeanX is Mean of TimeBodyAccMeanX"                            
-  [2] "MeanTimeBodyAccMeanY is Mean of TimeBodyAccMeanY"                            
-  [3] "MeanTimeBodyAccMeanZ is Mean of TimeBodyAccMeanZ"                            
-  [4] "MeanTimeBodyAccStdDevX is Mean of TimeBodyAccStdDevX"                        
-  [5] "MeanTimeBodyAccStdDevY is Mean of TimeBodyAccStdDevY"                        
-  [6] "MeanTimeBodyAccStdDevZ is Mean of TimeBodyAccStdDevZ"                        
-  [7] "MeanTimeGravityAccMeanX is Mean of TimeGravityAccMeanX"                      
-  [8] "MeanTimeGravityAccMeanY is Mean of TimeGravityAccMeanY"                      
-  [9] "MeanTimeGravityAccMeanZ is Mean of TimeGravityAccMeanZ"                      
- [10] "MeanTimeGravityAccStdDevX is Mean of TimeGravityAccStdDevX"                  
- [11] "MeanTimeGravityAccStdDevY is Mean of TimeGravityAccStdDevY"                  
- [12] "MeanTimeGravityAccStdDevZ is Mean of TimeGravityAccStdDevZ"                  
- [13] "MeanTimeBodyAccJerkMeanX is Mean of TimeBodyAccJerkMeanX"                    
- [14] "MeanTimeBodyAccJerkMeanY is Mean of TimeBodyAccJerkMeanY"                    
- [15] "MeanTimeBodyAccJerkMeanZ is Mean of TimeBodyAccJerkMeanZ"                    
- [16] "MeanTimeBodyAccJerkStdDevX is Mean of TimeBodyAccJerkStdDevX"                
- [17] "MeanTimeBodyAccJerkStdDevY is Mean of TimeBodyAccJerkStdDevY"                
- [18] "MeanTimeBodyAccJerkStdDevZ is Mean of TimeBodyAccJerkStdDevZ"                
- [19] "MeanTimeBodyGyroMeanX is Mean of TimeBodyGyroMeanX"                          
- [20] "MeanTimeBodyGyroMeanY is Mean of TimeBodyGyroMeanY"                          
- [21] "MeanTimeBodyGyroMeanZ is Mean of TimeBodyGyroMeanZ"                          
- [22] "MeanTimeBodyGyroStdDevX is Mean of TimeBodyGyroStdDevX"                      
- [23] "MeanTimeBodyGyroStdDevY is Mean of TimeBodyGyroStdDevY"                      
- [24] "MeanTimeBodyGyroStdDevZ is Mean of TimeBodyGyroStdDevZ"                      
- [25] "MeanTimeBodyGyroJerkMeanX is Mean of TimeBodyGyroJerkMeanX"                  
- [26] "MeanTimeBodyGyroJerkMeanY is Mean of TimeBodyGyroJerkMeanY"                  
- [27] "MeanTimeBodyGyroJerkMeanZ is Mean of TimeBodyGyroJerkMeanZ"                  
- [28] "MeanTimeBodyGyroJerkStdDevX is Mean of TimeBodyGyroJerkStdDevX"              
- [29] "MeanTimeBodyGyroJerkStdDevY is Mean of TimeBodyGyroJerkStdDevY"              
- [30] "MeanTimeBodyGyroJerkStdDevZ is Mean of TimeBodyGyroJerkStdDevZ"              
- [31] "MeanTimeBodyAccMagMean is Mean of TimeBodyAccMagMean"                        
- [32] "MeanTimeBodyAccMagStdDev is Mean of TimeBodyAccMagStdDev"                    
- [33] "MeanTimeGravityAccMagMean is Mean of TimeGravityAccMagMean"                  
- [34] "MeanTimeGravityAccMagStdDev is Mean of TimeGravityAccMagStdDev"              
- [35] "MeanTimeBodyAccJerkMagMean is Mean of TimeBodyAccJerkMagMean"                
- [36] "MeanTimeBodyAccJerkMagStdDev is Mean of TimeBodyAccJerkMagStdDev"            
- [37] "MeanTimeBodyGyroMagMean is Mean of TimeBodyGyroMagMean"                      
- [38] "MeanTimeBodyGyroMagStdDev is Mean of TimeBodyGyroMagStdDev"                  
- [39] "MeanTimeBodyGyroJerkMagMean is Mean of TimeBodyGyroJerkMagMean"              
- [40] "MeanTimeBodyGyroJerkMagStdDev is Mean of TimeBodyGyroJerkMagStdDev"          
- [41] "MeanFrequencyBodyAccMeanX is Mean of FrequencyBodyAccMeanX"                  
- [42] "MeanFrequencyBodyAccMeanY is Mean of FrequencyBodyAccMeanY"                  
- [43] "MeanFrequencyBodyAccMeanZ is Mean of FrequencyBodyAccMeanZ"                  
- [44] "MeanFrequencyBodyAccStdDevX is Mean of FrequencyBodyAccStdDevX"              
- [45] "MeanFrequencyBodyAccStdDevY is Mean of FrequencyBodyAccStdDevY"              
- [46] "MeanFrequencyBodyAccStdDevZ is Mean of FrequencyBodyAccStdDevZ"              
- [47] "MeanFrequencyBodyAccJerkMeanX is Mean of FrequencyBodyAccJerkMeanX"          
- [48] "MeanFrequencyBodyAccJerkMeanY is Mean of FrequencyBodyAccJerkMeanY"          
- [49] "MeanFrequencyBodyAccJerkMeanZ is Mean of FrequencyBodyAccJerkMeanZ"          
- [50] "MeanFrequencyBodyAccJerkStdDevX is Mean of FrequencyBodyAccJerkStdDevX"      
- [51] "MeanFrequencyBodyAccJerkStdDevY is Mean of FrequencyBodyAccJerkStdDevY"      
- [52] "MeanFrequencyBodyAccJerkStdDevZ is Mean of FrequencyBodyAccJerkStdDevZ"      
- [53] "MeanFrequencyBodyGyroMeanX is Mean of FrequencyBodyGyroMeanX"                
- [54] "MeanFrequencyBodyGyroMeanY is Mean of FrequencyBodyGyroMeanY"                
- [55] "MeanFrequencyBodyGyroMeanZ is Mean of FrequencyBodyGyroMeanZ"                
- [56] "MeanFrequencyBodyGyroStdDevX is Mean of FrequencyBodyGyroStdDevX"            
- [57] "MeanFrequencyBodyGyroStdDevY is Mean of FrequencyBodyGyroStdDevY"            
- [58] "MeanFrequencyBodyGyroStdDevZ is Mean of FrequencyBodyGyroStdDevZ"            
- [59] "MeanFrequencyBodyAccMagMean is Mean of FrequencyBodyAccMagMean"              
- [60] "MeanFrequencyBodyAccMagStdDev is Mean of FrequencyBodyAccMagStdDev"          
- [61] "MeanFrequencyBodyAccJerkMagMean is Mean of FrequencyBodyAccJerkMagMean"      
- [62] "MeanFrequencyBodyAccJerkMagStdDev is Mean of FrequencyBodyAccJerkMagStdDev"  
- [63] "MeanFrequencyBodyGyroMagMean is Mean of FrequencyBodyGyroMagMean"            
- [64] "MeanFrequencyBodyGyroMagStdDev is Mean of FrequencyBodyGyroMagStdDev"        
- [65] "MeanFrequencyBodyGyroJerkMagMean is Mean of FrequencyBodyGyroJerkMagMean"    
- [66] "MeanFrequencyBodyGyroJerkMagStdDev is Mean of FrequencyBodyGyroJerkMagStdDev"

