# PSNR Calculation
psnr <- function(TrueLRmat, PredLRmat) {
  mse <- sum((TrueLRmat - PredLRmat)^2)
  return(20*log(max(TrueLRmat), base = 10) - 10*log(mse, base = 10))
}
