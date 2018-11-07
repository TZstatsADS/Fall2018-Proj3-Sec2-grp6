# PSNR Calculation
psnr <- function(TrueLRmat, PredLRmat) {
  m <- dim(TrueLRmat)[1]
  n <- dim(TrueLRmat)[2]
  mse <- sum((TrueLRmat - PredLRmat)^2)/(3*m*n)
  return(20*log(max(TrueLRmat), base = 10) - 10*log(mse, base = 10))
}

a.psnr <- function(hrdir, hrpdir) {
  n_file <- length(list.files(hrdir))
  res <- c()
  for(i in 1:n_file) {
    ti <- readImage(paste0(hrdir,  "img", "_", sprintf("%04d", i), ".jpg"))
    pi <- readImage(paste0(hrpdir,  "img", "_", sprintf("%04d", i), ".jpg"))
    res[i] <- psnr(ti, pi)
  }
  return(res)
}
