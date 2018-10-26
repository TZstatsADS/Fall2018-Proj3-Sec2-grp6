#############################################################
### Construct features and responses for training images###
#############################################################

### Authors: Chengliang Tang/Tian Zheng
### Project 3
LRdir <- "/Users/brett/Downloads/train_set/LR/"
HRdir <- "/Users/brett/Downloads/train_set/HR/"
source("http://bioconductor.org/biocLite.R")
biocLite("EBImage")
### load libraries
library("EBImage")

addblack <- function(img) {
  c1mat <- imageData(img)[, , 1]
  c2mat <- imageData(img)[, , 2]
  c3mat <- imageData(img)[, , 3]
  
  c1matblack <- cbind(0, rbind(0, c1mat, 0), 0)
  c2matblack <- cbind(0, rbind(0, c2mat, 0), 0)
  c3matblack <- cbind(0, rbind(0, c3mat, 0), 0)
  
  imgblack <- abind(c1matwhite, c2matwhite, c3matwhite, along = 3)
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
  for(i in 1:n_files){
    imgLR <- readImage(paste0(LR_dir,  "img_", sprintf("%04d", i), ".jpg"))
    imgHR <- readImage(paste0(HR_dir,  "img_", sprintf("%04d", i), ".jpg"))
    
    ### step 1. sample n_points from imgLR
    samp_row = sample(1:nrow(imgLR), n_points, replace = TRUE)
<<<<<<< HEAD
    samp_col = sample(1:ncol(imgLR), n_points, replace = TRUE)
    
    ### step 2. for each sampled point in imgLR,
    ### step 2.1. save (the neighbor 8 pixels - central pixel) in featMat
    ###           tips: padding zeros for boundary points
        
    col = ncol(imgLR) #total number of columns
    row = nrow(imgLR) #total number of rows
    
=======
    samp_col = sample(1:ncol(imgLR), n_points, replace=TRUE)
    
    ### step 2. for each sampled point in imgLR,
    # add whitespace to array
    imgLR.w <- lapply(imgLR, addwhite)
    
    ### step 2.1. save (the neighbor 8 pixels - central pixel) in featMat
    ###           tips: padding zeros for boundary points
    
    col = ncol(imgLR) #total number of columns
    row = nrow(imgLR) #total number of rows
    
    
>>>>>>> wbh
    for (j in 1:length(samp_row))
    {
      #find row and column indices for each of the 8 neighboring pixels:
      neighbors = rbind(
        c(samp_row[j] - 1, samp_col[j] - 1), c(samp_row[j] - 1, samp_col[j]), c(samp_row[j] -1, samp_col[j] + 1),
        c(samp_row[j], samp_col[j] - 1), c(samp_row[j], samp_col[j] + 1),
        c(samp_row[j] + 1, samp_col[j] - 1), c(samp_row[j] + 1, samp_col[j]), c(samp_row[j] + 1, samp_col[j] + 1))
      for (r in 1:8)
      {
        for (c in 1:3)
        {
          #Find the value of rth neighboring pixel at channel c
<<<<<<< HEAD
          neighbor_value = imgLR[neighbors[r,1], neighbors[r,2], c][1]
          
          #find the difference between this value and the center pixel:
          featMat[i*j, r, c] = imgLR[samp_row[j],samp_col[j], c][1] - neighbor_value
        }
      }
      
      ### step 2.2. save the corresponding 4 sub-pixels of imgHR in labMat
      y = samp_row[j]
      x = samp_col[j]
      for(c in 1:3){
        labMat[i*j, 1, c] = imgHR[2*y-1, 2*x-1, c]
        labMat[i*j, 2, c] = imgHR[2*y, 2*x-1, c]
        labMat[i*j, 3, c] = imgHR[2*y-1, 2*x, c]
        labMat[i*j, 4, c] = imgHR[2*y, 2*x, c]
      }
    }
=======
          neighbor_value = imgLR.w[neighbors[r,1], neighbors[r,2], c][1]
          
          #find the difference between this value and the center pixel:
          featMat[i*j, r, c] = imgLR.w[samp_row[j],samp_col[j], c][1] - neighbor_value
        }
      }
    }
    
    
    
    ### step 2.2. save the corresponding 4 sub-pixels of imgHR in labMat
>>>>>>> wbh
    
    ### step 3. repeat above for three channels
    
  }
}
