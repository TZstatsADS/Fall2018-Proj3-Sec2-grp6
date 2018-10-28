#############################################################
### Construct features and responses for training images###
#############################################################

### Authors: Chengliang Tang/Tian Zheng
### Project 3
LR_dir <- "/Users/Janice/Desktop/Fall2018-Proj3-Sec2-grp6-yaxin/data/train_set/LR/"
HR_dir <- "/Users/Janice/Desktop/Fall2018-Proj3-Sec2-grp6-yaxin/data/train_set/HR/"


addblack <- function(img) {
  c1mat <- imageData(img)[, , 1]
  c2mat <- imageData(img)[, , 2]
  c3mat <- imageData(img)[, , 3]
  
  c1matblack <- cbind(0, rbind(0, c1mat, 0), 0)
  c2matblack <- cbind(0, rbind(0, c2mat, 0), 0)
  c3matblack <- cbind(0, rbind(0, c3mat, 0), 0)
  
  imgblack <- abind(c1matblack, c2matblack, c2matblack, along = 3)
  return(imgblack)
}


feature <- function(LR_dir, HR_dir, n_points=1000){
  n_files <- length(list.files(LR_dir))
  ### Construct process features for training images (LR/HR pairs)
  
  ### Input: a path for low-resolution images + a path for high-resolution images 
  ###        + number of points sampled from each LR image
  ### Output: an .RData file contains processed features and responses for the images
  
  ### store feature and responses
  featMat <- array(NA, c(n_files * n_points, 8, 3))
  labMat <- array(NA, c(n_files * n_points, 4, 3))
  
  ### read LR/HR image pairs
  for(i in 1:1){
    imgLR <- readImage(paste0(LR_dir,  "img_", sprintf("%04d", i), ".jpg"))
    imgHR <- readImage(paste0(HR_dir,  "img_", sprintf("%04d", i), ".jpg"))
    
    ### step 1. sample n_points from imgLR
    samp_row = sample(1:nrow(imgLR), n_points, replace = TRUE)
    samp_col = sample(1:ncol(imgLR), n_points, replace = TRUE)
    imgLR.b <- addblack(imgLR) 
    
    ### step 2. for each sampled point in imgLR,
    for (j in 1:1000){
      y = samp_row[j]
      x = samp_col[j]
      for (c in c(1:3)){
        ### step 2.1. save (the neighbor 8 pixels - central pixel) in featMat
        featMat[i*j, , c] <- imgLR.b[,,c][c(y,y+2),c(x,x+2)][-5]
        ### step 2.2. save the corresponding 4 sub-pixels of imgHR in labMat
        labMat[i*j, , c] = as.vector(imgHR@.Data[c(2*y-1,2*y),c(2*x-1,2*x),c])[c(3,1,2,4)]
      }
        
    }
          
  }
  return(list(featMat=featMat,labMat=labMat))
}

    
#feature(LR_dir, HR_dir, n_points=1000)
    

