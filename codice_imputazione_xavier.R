#FUNZIONI PER L'IMPUTAZIONE

starting.values=function(dat,m) {
  ini <- mice(dat, m = m, maxit = 0)
  dat.comp=complete(ini,1)
  for (i.col in 1:dim(dat)[2]) {
    if (sum(is.na(dat.comp[,i.col]))>0) {
      # variables with missing values in the original dataset, without initial
      # values (imputations) in the completed dataset
      y=dat.comp[,i.col]
      ry=!is.na(y)
      mat.aux=matrix(NA,nrow=sum(is.na(y)),ncol=m)
      for (i.imp in 1:m) {
        mat.aux[,i.imp]=mice.impute.sample(y,ry)
      }
      df=data.frame(mat.aux)
      rownames(df)=rownames(dat.comp)[is.na(y)]
      names(df)=as.character(1:m)
      
      ini$imp[[i.col]]=df
    }
  }
  ini
}

as.mids.xavi = function (data, .imp = 1, .id = 2) {
  data[,.imp] <- as.numeric(as.character(data[, .imp]))
  data[,.id] <- as.numeric(as.character(data[, .id]))
  data <- data[order(data[,.imp]),]
  imps <- as.numeric(as.character(data[, .imp]))
  if (is.factor(imps)) {
    m = max(as.numeric(levels(imps))[imps])
  } else {
    m = max(imps)
  }
  if(all(imps!=0)) stop("the non-imputed values need to be provided in data")
  ini=starting.values(data[imps == 0, ],m=m)
  names <- names(ini$imp)
  if (!is.null(.id)) {
    rownames(ini$data) <- data[imps == 0, .id]
  }
  for (i in 1:length(names)) {
    for (j in 1:m) {
      if (!is.null(ini$imp[[names[i]]])) {
        indic <- imps == j & is.na(data[imps == 0, names[i]])
        ini$imp[[names[i]]][j] <- data[indic, names[i]]
      }
    }
  }
  return(ini)
}

#DATASET IMPUTED CREATED WITH XAVIER FUNCTION
dat <- dat0[order(dat0$.imp,dat0$.id),]
dat=dat[,c(2,1,3:dim(dat)[2])]#reorder the columns so it starts with .imp and then .id
dat2<-as.mids.xavi(dat)
dat3 <- dat2$data
View(dat2$data)

#IMPUTATION N1
imp1 <- dat %>% filter(.imp == "1")
names(imp1)
names(imp1_cexp)

imp1_cexp <- imp1[-c(1:150)] 
imp1_cexp <- imp1_cexp[-c(137)]

library(foreign)
write.dta(imp1_cexp,"D:/PhD/HELIX/datasets/imp1_cexp.dta" )
