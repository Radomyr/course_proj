library(readr)
get_labels<-function(Path){
      lab<-read_file(Path)
      lab<-strsplit(lab,split = "\n")
      lab<-as.integer(unlist(lab))
      lab
}
get_actLab_vec<-function(activitylabels,lab_tab){
   actLab_vec<-c()
   for(j in activitylabels){
      for(i in 1:length(lab_tab$V1)){
         if(j==lab_tab$V1[i]){
            actLab_vec<-c(actLab_vec,lab_tab$V2[i])
            break
         }
      }
   }
   actLab_vec
}
get_df_with_labels<-function(dataPath,activitylabels,subjectLabels,actLabNames,rnam){
      X_data<-read_table(dataPath,col_names = FALSE)
      #each examination in separate column and have numeric label of subject
      X_data<-t(X_data)
      colnames(X_data)<-subjectLabels
      #first row is proper activity label
      row.names(X_data)<-rnam$V2
      actLabVec<-get_actLab_vec(activitylabels,actLabNames)
      X_data<-as.data.frame(rbind(actLabVec,X_data))
      X_data
}
##read activity labels and features names 
actLabNames<-read.table("UCI HAR Dataset/activity_labels.txt")
rnam<-read.table("UCI HAR Dataset/features.txt")
##get clean test data
dataPath<-"UCI HAR Dataset/test/X_test.txt"
subjectLabels<-get_labels("UCI HAR Dataset/test/subject_test.txt")
actLabels<-get_labels("UCI HAR Dataset/test/y_test.txt")
X_test<-get_df_with_labels(dataPath,actLabels,subjectLabels,actLabNames,rnam)
##get clean train data
dataPath<-"UCI HAR Dataset/train/X_train.txt"
actLabels<-get_labels("UCI HAR Dataset/train/y_train.txt")
subjectLabels<-get_labels("UCI HAR Dataset/train/subject_train.txt")
X_train<-get_df_with_labels(dataPath,actLabels,subjectLabels,actLabNames,rnam)
##combine datasets and extract only required data
clean_dataset<-cbind(X_test,X_train)
clean_dataset<-rbind(clean_dataset[1:7,],clean_dataset[122:127,],clean_dataset[267:272,])
##make second dataset
subjects<-unique(names(clean_dataset))
tidy_dataset<-data.frame(row.names = row.names(clean_dataset))
colnam<-NULL
for(s in subjects){
   for(a in actLabNames$V2){
      sub_dataset<-clean_dataset[colnames(clean_dataset)==s]
      colnames(sub_dataset)<-sub_dataset[1,]
      sub_dataset<-sub_dataset[2:nrow(sub_dataset),]
      sub_dataset<-sub_dataset[colnames(sub_dataset)==a]
      sub_dataset[]<-lapply(sub_dataset, as.numeric)
      colnam<-c(colnam,s)
      tidy_dataset<-cbind(tidy_dataset,c(a,rowMeans(sub_dataset)))
   }
}
colnames(tidy_dataset)<-colnam
write.table(tidy_dataset,"tidyDataset.txt",row.names = FALSE)



