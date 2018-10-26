tune_gbm=function(t,i,s,n,b,K,data)
{
  row=1
  mat=matrix(NA,ncol=7,nrow=length(i)*length(s)*length(n)*length(b)*length(K))
  colnames(mat)=c("trees","interdepth","shrinkage","n.minobsinnode","bag.fraction","k","error")
  for (inter in i)
  {
    for (shr in s)
    {
      for (n.m in n)
      {
        for (bf in b)
        {
          for (k in K)
          {
            set.seed(1)
            gbm=gbm(labMat ~ featMat, verbose=TRUE, distribution="gaussian", data=data, n.trees=t, interaction.depth=inter, shrinkage=shr, n.minobsinnode=n.m, bag.fraction=bf, cv.folds=k)
            error=min(gbm$cv.error)
            mat[row,]=c(which.min(gbm$cv.error),inter,shr,n.m,bf,k,error)
            row=row+1 
          }
        }
      }
    }
  }
  
  return(mat)
}