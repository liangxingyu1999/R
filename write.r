# R
# author:liangxingyu1999
# coding:utf-8

data <- read.table('eligible_data.csv',sep=',',header=TRUE,as.is=TRUE)

n <- 0
for(i in 1:length(data[,1])){
    if(data[i,]$YR %in% c(2011:2020)){
        cat(paste('RT ',as.character(data[i,]$RT),'\n',sep=''),file='output.txt',append=TRUE)
        cat(paste('A1 ',as.character(data[i,]$A1),'\n',sep=''),file='output.txt',append=TRUE)
        cat(paste('AD ',as.character(data[i,]$AD),'\n',sep=''),file='output.txt',append=TRUE)
        cat(paste('T1 ',as.character(data[i,]$T1),'\n',sep=''),file='output.txt',append=TRUE)
        cat(paste('JF ',as.character(data[i,]$JF),'\n',sep=''),file='output.txt',append=TRUE)
        cat(paste('YR ',as.character(data[i,]$YR),'\n',sep=''),file='output.txt',append=TRUE)
        cat(paste('K1 ',as.character(data[i,]$K1),'\n',sep=''),file='output.txt',append=TRUE)
        cat(paste('DS ',as.character(data[i,]$DS),'\n',sep=''),file='output.txt',append=TRUE)
        cat('\n',file='output.txt',append=TRUE)
        n <- n+1
        print(n)
    }
}
