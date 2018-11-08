#############################################################
### Construct features and responses for training images###
#############################################################

### Authors: Chengliang Tang/Tian Zheng
### Project 3

## Initilize global variables
LR_R<-c()
LR_G<-c()
LR_B<-c()
LR<-c()
HR_R<-c()
HR_G<-c()
HR_B<-c()
HR<-c()
center<-c()

feature <- function(LR_dir, HR_dir){
  #library(parallel)
  
  n_files <- length(list.files(LR_dir))
  ### Construct process features for training images (LR/HR pairs)
  
  ### Input: a path for low-resolution images + a path for high-resolution images 
  ###        + number of points sampled from each LR image
  ### Output: an .RData file contains processed features and responses for the images
  
  ## Initilize global variables
  LR_R<<-c()
  LR_G<<-c()
  LR_B<<-c()
  LR<<-c()
  HR_R<<-c()
  HR_G<<-c()
  HR_B<<-c()
  HR<<-c()
  center<<-c()
  
  ###extract 8 point from the center function
  extract_points<-function(samp_row, samp_col){
    p1 <- LR[samp_row, samp_col    ]
    p2 <- LR[samp_row, samp_col + 1]
    p3 <- LR[samp_row, samp_col + 2]
    p4 <- LR[samp_row + 1, samp_col    ]
    p5 <- LR[samp_row + 1, samp_col + 2]
    p6 <- LR[samp_row + 2, samp_col    ]
    p7 <- LR[samp_row + 2, samp_col + 1]
    p8 <- LR[samp_row + 2, samp_col + 2]
    center <<- LR[samp_row + 1, samp_col + 1]
    h1 <- HR[2*samp_row - 1, 2*samp_col - 1]
    h2 <- HR[2*samp_row    , 2*samp_col - 1]
    h3 <- HR[2*samp_row - 1, 2*samp_col    ]
    h4 <- HR[2*samp_row    , 2*samp_col    ]
    l <- c(p1,p2,p3,p4,p5,p6,p7,p8) - center
    h <- c(p1,p2,p3,p4) - center
    return(list(l,h))
  }
  
  ###extract value from low resolution dataset
  extract_value<-function(n){
    LR <<- cbind(0, rbind(0, imageData(imgLR)[, , n], 0), 0)
    HR <<- imageData(imgHR)[, , n]
    out <- mapply(extract_points, samp_row, samp_col)
    
    l <- unlist(out[1,])
    h <- unlist(out[2,])
    dim(l) <- c(8,length(l)/8)
    dim(h) <- c(4,length(h)/4)
    
    if (n == 1){
      LR_R <<- cbind(LR_R,l)
      HR_R <<- cbind(HR_R,h)
    }else if(n == 2){
      LR_G <<- cbind(LR_G,l)
      HR_G <<- cbind(HR_G,h)
    }else{
      LR_B <<- cbind(LR_B,l)
      HR_B <<- cbind(HR_B,h)
    }
  }
  
  ### read LR/HR image pairs
  for (i in 1:n_files){#n_files
    imgLR <- readImage(paste0(LR_dir,  "img_", sprintf("%04d", i), ".jpg"))
    imgHR <- readImage(paste0(HR_dir,  "img_", sprintf("%04d", i), ".jpg"))
    n_points <- 1000
    #LR_row <- dim(imageData(imgLR)[,,1])[1]
    #LR_col <- dim(imageData(imgLR)[,,1])[2]
    #n_points = LR_col* LR_row
    
    ### step 1. sample n_points from imgLR
    samp_row = sample(1:nrow(imgLR), n_points, replace = TRUE)
    samp_col = sample(1:ncol(imgLR), n_points, replace = TRUE)
    #samp_row <- ceiling(c(1:n_points)/LR_col)
    #samp_col <- c(1:n_points) - (samp_row - 1)* LR_col
    
    extract_value(1)
    extract_value(2)
    extract_value(3)
  }
  
  L <- abind(LR_R, LR_G, LR_B, along = 3)
  H <- abind(HR_R, HR_G, HR_B, along = 3)
  featMat <- aperm(L,c(2,1,3))
  labMat  <- aperm(H,c(2,1,3))
  ##return(labMat)
  return(list(feature=featMat,label=labMat))
}
