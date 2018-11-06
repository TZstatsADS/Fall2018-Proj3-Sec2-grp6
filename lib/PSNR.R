# PSNR Calculation
psnr <- function(TrueLRmat, PredLRmat) {
  m <- dim(TrueLRmat)[1]
  n <- dim(TrueLRmat)[2]
  mse <- sum((TrueLRmat - PredLRmat)^2)/(3*m*n)
  return(20*log(max(TrueLRmat), base = 10) - 10*log(mse, base = 10))
}

img1_pred <- readImage("/Users/brett/Desktop/img1_pred.jpg")
img1HR <- readImage("/Users/brett/Downloads/train_set/HR/img_0001.jpg")

psnr(img1HR, img1_pred)
