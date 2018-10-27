####add point function
#addpoint<-function(img){
  #for (i in 1:3){
    #img_addrow<-array(NA, dim=c(nrow(img)+2,ncol(img),3))
    #r1<-imageData(img)[1, ,i]
    #rn<-imageData(img)[nrow(img), ,i]
    #img_addrow[ , ,i]<-rbind(r1,imageData(img)[ , ,i],rn)
    #c1<-img_addrow[,1,i]
    #cn<-img_addrow[,ncol(img),i]
    #img_add<-array(NA, dim=c(nrow(img)+2,ncol(img)+2,3))
    #img_add[ , , i]<-cbind(c1,img_addrow[ , ,i],cn)
    #return(img_add)
  #}
#}




#####addblack around the image
addblack <- function(img) {
  c1mat <- imageData(img)[, , 1]
  c2mat <- imageData(img)[, , 2]
  c3mat <- imageData(img)[, , 3]
  
  c1matblack <- cbind(0, rbind(0, c1mat, 0), 0)
  c2matblack <- cbind(0, rbind(0, c2mat, 0), 0)
  c3matblack <- cbind(0, rbind(0, c3mat, 0), 0)
  
  imgblack <- abind(c1matblack, c2matblack, c3matblack, along = 3)
  return(imgblack)
}


###sample function
sample_point<-function(imgLR,npoints=1000){
  samp_row <- sample(1:nrow(imgLR), n_points, replace = TRUE)
  samp_col <- sample(1:ncol(imgLR), n_points, replace=TRUE)
  index<-data.frame(samp_row,samp_col)
  index$freq1<-8
  index$freq2<-4
  return(index)
}
###index data
data_index<-sample_point(imgLR = imgLR)

###extract 8 point from the center
extract_eight<-function(samp_row,samp_col){
  point1<-c(samp_row - 1, samp_col- 1)
  point2<-c(samp_row - 1, samp_col)
  point3<-c(samp_row -1, samp_col + 1)
  point4<-c(samp_row, samp_col - 1)
  point5<-c(samp_row, samp_col + 1)
  point6<-c(samp_row+ 1, samp_col - 1)
  point7<-c(samp_row + 1, samp_col)
  point8<- c(samp_row + 1, samp_col+ 1)
  return(list(rbind(point1,point2,point3,point4,point5,point6,point7,point8)))
}
##index of neighbor 8 points
neighbor_list<-mapply(extract_eight,data_index$samp_row, data_index$samp_col)
require(data.table)
df_neighbor <- data.frame(do.call(rbind,neighbor_list))

library(splitstackshape)
center_data1<-expandRows(data_index, "freq1")
center_data2<-expandRows(data_index, "freq2")
###add black around the LR image
addblack_img<-addblack(imgLR)
##extract value
value_img_LR<-function(row,col){
  value<-c()
  value<- addblack_img[c(row)+1,c(col)+1,]
  return(value)
}
###value of neighbor 8 pixels
neighbor_value<-mapply(value_img_LR,x1,x2)
###value of center pixel
center_value<-mapply(value_img_LR,center_data1$samp_row,center_data1$samp_col)
##subtract the value of center from the value of neighbor 8 pixels
value_LR<-neighbor_value-center_value




##extract 4 pixels index from HR
extract_four<-function(samp_row,samp_col){
  point1<-c(2*samp_row - 1, 2*samp_col- 1)
  point2<-c(2*samp_row, 2*samp_col-1)
  point3<-c(2*samp_row -1, 2*samp_col)
  point4<-c(2*samp_row, 2*samp_col)
  return(list(rbind(point1,point2,point3,point4)))
}
###value of neighbor 4 sub-pixels
Subpixel_list<-mapply(extract_four,data_index$samp_row, data_index$samp_col)
df_Subpixel <- data.frame(do.call(rbind,Subpixel_list))
addblack_img<-addblack(imgHR)
##extract value of 4 pixels
value_img_HR<-function(row,col){
  value<-c()
  addblack_img_HR<-addblack(imgHR)
  value<- addblack_img_HR[row+1,col+1,]
  return(value)
}
y1<-df_Subpixel$X1
y2<-df_Subpixel$X2
subpixel_value<-mapply(value_img_HR,y1,y2)
###value of center pixel
HR_row<-center_data2$samp_row
HR_col<-center_data2$samp_col
center_value<-mapply(value_img_HR,HR_row,HR_col)
##subtract the value of center from the value of neighbor 8 pixels
value_HR<-subpixel_value-center_value



#library(dplyr)
#neighbor_list %>% bind_rows()


###
##This function fills in FeatMat but doesn't take into account boundaries
library("EBImage")
library("gbm")

set.seed(2018)
##setwd("..")

train_dir <- "./data/train_set/" # This will be modified for different data sets.
LR_dir <- paste(train_dir, "LR/", sep="")
HR_dir <- paste(train_dir, "HR/", sep="")
train_label_path <- paste(train_dir, "label.csv", sep="")

n_files <- length(list.files(LR_dir))
n_points = 1000
###

imgLR<- readImage("~/Desktop/5243project3/train_set/LR/img_0001.jpg")
imgHR<-readImage("~/Desktop/5243project3/train_set/HR/img_0001.jpg")

####function extract value of 8 pixels and 4 subpixels
extract_value<-function(image_index){
  imgLR <- readImage(paste0(LR_dir,  "img_", sprintf("%04d", image_index), ".jpg"))
  imgHR <- readImage(paste0(HR_dir,  "img_", sprintf("%04d", image_index), ".jpg")) 
  data_index<-sample_point(imgLR = imgLR)
  ###8 pixels
  neighbor_list<-mapply(extract_eight,data_index$samp_row, data_index$samp_col)
  df_neighbor <- data.frame(do.call(rbind,neighbor_list))
  center_data1<-expandRows(data_index, "freq1")
  center_data2<-expandRows(data_index, "freq2")
  #add black around the LR image
  addblack_img<-addblack(imgLR)
  #value of neighbor 8 pixels
  x1<-df_neighbor$X1
  x2<-df_neighbor$X2
  neighbor_value<-mapply(value_img_LR,x1,x2)
  #value of center pixel
  center_value<-mapply(value_img_LR,center_data1$samp_row,center_data1$samp_col)
  #subtract the value of center from the value of neighbor 8 pixels
  value_LR<-neighbor_value-center_value
  ###4 subpixels
  Subpixel_list<-mapply(extract_four,data_index$samp_row, data_index$samp_col)
  df_Subpixel <- data.frame(do.call(rbind,Subpixel_list))
  addblack_img<-addblack(imgHR)
  y1<-df_Subpixel$X1
  y2<-df_Subpixel$X2
  subpixel_value<-mapply(value_img_HR,y1,y2)
  ###value of center pixel
  HR_row<-center_data2$samp_row
  HR_col<-center_data2$samp_col
  center_value<-mapply(value_img_HR,HR_row,HR_col)
  ##subtract the value of center from the value of neighbor 8 pixels
  value_HR<-subpixel_value-center_value
  return(list(value_LR,value_HR))
}


data_index<-sample_point(imgLR)

###extract value from low resolution dataset
extract_value_LR<-function(image_index){
  imgLR <- readImage(paste0(LR_dir,  "img_", sprintf("%04d", image_index), ".jpg"))
  ###8 pixels
  neighbor_list<-mapply(extract_eight,data_index$samp_row, data_index$samp_col)
  df_neighbor <- data.frame(do.call(rbind,neighbor_list))
  center_data1<-expandRows(data_index, "freq1")
  #add black around the LR image
  addblack_img<-addblack(imgLR)
  #value of neighbor 8 pixels
  x1<-df_neighbor$X1
  x2<-df_neighbor$X2
  neighbor_value<-mapply(value_img_LR,x1,x2)
  #value of center pixel
  center_value<-mapply(value_img_LR,center_data1$samp_row,center_data1$samp_col)
  #subtract the value of center from the value of neighbor 8 pixels
  value_LR<-neighbor_value-center_value
  return(value_LR)
}



###extract value from high resolution dataset
###4 subpixels
extract_value_HR<-function(image_index){
  imgHR <- readImage(paste0(HR_dir,  "img_", sprintf("%04d", image_index), ".jpg")) 
  Subpixel_list<-mapply(extract_four,data_index$samp_row, data_index$samp_col)
  df_Subpixel <- data.frame(do.call(rbind,Subpixel_list))
  center_data2<-expandRows(data_index, "freq2")
  addblack_img<-addblack(imgHR)
  y1<-df_Subpixel$X1
  y2<-df_Subpixel$X2
  subpixel_value<-mapply(value_img_LR,y1,y2)
  ###value of center pixel
  center_value<-mapply(value_img_LR,center_data2$samp_row,center_data2$samp_col)
  #subtract the value of center from the value of neighbor 8 pixels
  value_HR<-subpixel_value-center_value
  return(value_HR)
}
