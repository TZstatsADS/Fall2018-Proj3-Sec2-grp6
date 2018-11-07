########################
### Super-resolution ###
########################

### Author: Chengliang Tang
### Project 3

## Initilize global variables
LR_R<-c()
LR_G<-c()
LR_B<-c()
LR<-c()
out_HR <- c()
sol_HR <- c()
imgLR <- c()
pathHR <- c()
LR_row <- c()
LR_col <- c()
n_points = c()
samp_row <-c()
samp_col <-c()
predMat <- c()

superResolution <- function(LR_dir, HR_dir, modelList){
  library(parallel)
  cl <- detectCores()
  ### Construct high-resolution images from low-resolution images with trained predictor
  
  ### Input: a path for low-resolution images + a path for high-resolution images 
  ###        + a list for predictors
  
  n_files <- length(list.files(LR_dir))
  
  ###extract 8 point from the center function
  extract_eight<-function(samp_row, samp_col){
    p1 <- LR[samp_row, samp_col    ]
    p2 <- LR[samp_row, samp_col + 1]
    p3 <- LR[samp_row, samp_col + 2]
    p4 <- LR[samp_row + 1, samp_col    ]
    p5 <- LR[samp_row + 1, samp_col + 2]
    p6 <- LR[samp_row + 2, samp_col    ]
    p7 <- LR[samp_row + 2, samp_col + 1]
    p8 <- LR[samp_row + 2, samp_col + 2]
    center <- LR[samp_row + 1, samp_col + 1]
    return(c(p1,p2,p3,p4,p5,p6,p7,p8) - center)
  }
  
  ###extract 8 point from the center function
  mextract_eight<-function(x){
    samp_row <- ceiling(x/LR_col)
    samp_col <- x - (samp_row - 1)* LR_col
    p1 <- LR[samp_row, samp_col    ]
    p2 <- LR[samp_row, samp_col + 1]
    p3 <- LR[samp_row, samp_col + 2]
    p4 <- LR[samp_row + 1, samp_col    ]
    p5 <- LR[samp_row + 1, samp_col + 2]
    p6 <- LR[samp_row + 2, samp_col    ]
    p7 <- LR[samp_row + 2, samp_col + 1]
    p8 <- LR[samp_row + 2, samp_col + 2]
    center <- LR[samp_row + 1, samp_col + 1]
    return(c(p1,p2,p3,p4,p5,p6,p7,p8) - center)
  }
  
  ###extract value from low resolution dataset
  mextract_value_LR<-function(){
    LR <<- cbind(0, rbind(0, imageData(imgLR)[, , 1], 0), 0)
    neighbor_R <- mclapply(1:n_points, mextract_eight, mc.cores=cl)
    LR <<- cbind(0, rbind(0, imageData(imgLR)[, , 2], 0), 0)
    neighbor_G <- mclapply(1:n_points, mextract_eight, mc.cores=cl)
    LR <<- cbind(0, rbind(0, imageData(imgLR)[, , 3], 0), 0)
    neighbor_B <- mclapply(1:n_points, mextract_eight, mc.cores=cl)
    
    neighbor_R <- matrix(unlist(neighbor_R),c(n_points,8),byrow=TRUE)
    neighbor_G <- matrix(unlist(neighbor_G),c(n_points,8),byrow=TRUE)
    neighbor_B <- matrix(unlist(neighbor_B),c(n_points,8),byrow=TRUE)

    LR_R <<- rbind(LR_R,neighbor_R)
    LR_G <<- rbind(LR_G,neighbor_G)
    LR_B <<- rbind(LR_B,neighbor_B)
  }
  
  ###extract value from low resolution dataset
  extract_value_LR<-function(){
    LR <<- cbind(0, rbind(0, imageData(imgLR)[, , 1], 0), 0)
    neighbor_R <- mapply(extract_eight, samp_row, samp_col)
    LR <<- cbind(0, rbind(0, imageData(imgLR)[, , 2], 0), 0)
    neighbor_G <- mapply(extract_eight, samp_row, samp_col)
    LR <<- cbind(0, rbind(0, imageData(imgLR)[, , 3], 0), 0)
    neighbor_B <- mapply(extract_eight, samp_row, samp_col)
    LR_R <<- cbind(LR_R,neighbor_R)
    LR_G <<- cbind(LR_G,neighbor_G)
    LR_B <<- cbind(LR_B,neighbor_B)
  }
  
  trans_HR <- function(num_of_p){
    n_col <- num_of_p * LR_col * 2
    sol_HR <<- rbind(sol_HR, out_HR[1:2, (n_col-LR_col*2+1):n_col])
  }
  
  get_HR <- function(num_of_p,n){
    out_HR <<- aperm(predMat[,,n],c(2,1))
    dim(out_HR) <<- c(2,2*num_of_p)
    sol_HR <<- c()
    invisible(mapply(trans_HR, c(1:(num_of_p/LR_col))))
  }
  
  HR_img <- function(num_of_p){
    get_HR(num_of_p,1)
    R <- sol_HR
    get_HR(num_of_p,2)
    G <- sol_HR
    get_HR(num_of_p,3)
    B <- sol_HR
    return(abind(R,G,B,along=3))
  }
  
  ### read LR/HR image pairs
  #for(i in 1:100){#n_files
  speed<-function(i){
    imgLR <<- readImage(paste0(LR_dir,  "img", "_", sprintf("%04d", i), ".jpg"))
    pathHR <<- paste0(HR_dir,  "img", "_", sprintf("%04d", i), ".jpg")
    LR_row <<- dim(imageData(imgLR)[,,1])[1]
    LR_col <<- dim(imageData(imgLR)[,,1])[2]
    n_points <<- LR_col* LR_row
    samp_row <<- ceiling(c(1:n_points)/LR_col)
    samp_col <<- c(1:n_points) - (samp_row - 1)* LR_col
    
    ### step 1. for each pixel and each channel in imgLR:
    ###           save (the neighbor 8 pixels - central pixel) in featMat
    ###           tips: padding zeros for boundary points
    
    ## Initilize global variables
    LR_R<<-c()
    LR_G<<-c()
    LR_B<<-c()
    LR<<-c()
    
    #mextract_value_LR()
    extract_value_LR()
    L <- abind(LR_R, LR_G, LR_B, along = 3)
    featMat <- aperm(L,c(2,1,3))
    print(dim(featMat))
    
    # step 2. apply the modelList over featMat
    out <- test(modelList, featMat)
    #out <- modelList
    R <- c(aperm(imageData(imgLR)[,,1],c(2,1)))
    G <- c(aperm(imageData(imgLR)[,,2],c(2,1)))
    B <- c(aperm(imageData(imgLR)[,,3],c(2,1)))
    out_R <- out[,,1] + R
    out_G <- out[,,2] + G
    out_B <- out[,,3] + B
    predMat <<- abind(out_R,out_G,out_B, along=3)

    ### step 3. recover high-resolution from predMat and save in HR_dir
    sol <- HR_img(n_points)
    pathHR <- HR_dir
    pathHR <- paste0(pathHR,  "img", "_", sprintf("%04d", i), ".jpg")
    writeImage(Image(sol, colormode=Color), pathHR)
  }
  invisible(mclapply(1:n_files,speed,mc.cores = 8))
}