##This function fills in FeatMat but doesn't take into account boundaries

source("http://bioconductor.org/biocLite.R")
biocLite("EBImage")
library("EBImage")
n_files <- length(list.files(LR_dir))

### store feature and responses
featMat <- array(NA, c(n_files * n_points, 8, 3))
labMat <- array(NA, c(n_files * n_points, 4, 3))

for(i in 1:n_files){
  imgLR <- readImage(paste0(LR_dir,  "img_", sprintf("%04d", i), ".jpg"))
  imgHR <- readImage(paste0(HR_dir,  "img_", sprintf("%04d", i), ".jpg"))

  ### step 1. sample n_points from imgLR
  samp_row = sample(1:nrow(imgLR), n_points, replace = TRUE)
  samp_col = sample(1:ncol(imgLR), n_points, replace=TRUE)
  
  ### step 2. for each sampled point in imgLR,
  
  ### step 2.1. save (the neighbor 8 pixels - central pixel) in featMat
  ###           tips: padding zeros for boundary points
  
  col = ncol(imgLR) #total number of columns
  row = nrow(imgLR) #total number of rows
  
  
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
        neighbor_value = imgLR[neighbors[r,1], neighbors[r,2], c][1]
        
        #find the difference between this value and the center pixel:
        featMat[i*j, r, c] = imgLR[samp_row[j],samp_col[j], c][1] - neighbor_value
      }
    }
  }
  featMat
  
  
  
  ### step 2.2. save the corresponding 4 sub-pixels of imgHR in labMat
  
  ### step 3. repeat above for three channels
  
}
