# R
# author:liangxingyu1999
# coding:utf-8

# screen函数
# 读取文件并提取数据
screen <- function(f){
        lines <- readLines(f)
        # make new vector
        RT <- c('RT')
        T1 <- c('T1')
        JF <- c('JF')
        YR <- c(1999)
        K1 <- c('K1')
        A1 <- c('A1')
        AD <- c('AD')
        # 建立sign，去重用
        SIGN <- c('SIGN')
        # 建立DS
        DS <- c('CNKI')
        # 块flag 0-初始化 1-正在读取 2-读取完成
        flag <- 0
        # ADflag 0-未连续 1-连续
        ADflag <- 0
        # 循环计数
        n <- 0
        # 遍历文件
        for(i in lines){
            # 循环计数
            n <- n+1
            # AD flag
            if(substring(i,1,2)!='AD'){
                ADflag <- 0
            }
            # 数据块flag
            if(substring(i,1,2)=='RT' | n==length(lines)){
                flag <- flag+1
            }
            # 判断块flag，如果等于2，合成SIGN，写入，重置flag
            if(flag==2){
                RT <- append(RT,RTT)
                T1 <- append(T1,T1T)
                JF <- append(JF,JFT)
                YR <- append(YR,YRT)
                K1 <- append(K1,K1T)
                A1 <- append(A1,A1T)
                AD <- append(AD,ADT)
                SIGN <- append(SIGN,paste(strsplit(as.character(A1T),';')[[1]][1],sep=',',T1T))
                DS <- append(DS,'CNKI')
                flag <- 1
            }
            # 初始化数据暂存变量
            if(substring(i,1,2)=='RT' | n==length(lines)){
                RTT <- T1T <- JFT <- YRT <- K1T <- A1T <- ADT <- NA
            }
            # 每行数据提取
            if(substring(i,1,2)=='RT'){
                RTT <- substring(i,4,nchar(i))
                # CNKI Journal Article; Conference Proceeding; Dissertation/Thesis; Book; Newspaper Article;
                # VIP Journal; 
                # WF Journal; Conference Proceeding; Dissertation;
                # CBM Journal Article; 
                RTT <- gsub('Journal Article','Journal',RTT)
                RTT <- gsub('Journal','Journal Article',RTT)
                RTT <- gsub('Dissertation/Thesis','Dissertation',RTT)
                RTT <- gsub('Dissertation','Dissertation/Thesis',RTT)
            }else if(substring(i,1,2)=='T1'){
                T1T <- substring(i,4,nchar(i))
                # T1规范化
                T1T <- gsub(' ','',T1T)
            }else if(substring(i,1,2)=='JF'){
                if(is.na(JFT)){
                    JFT <- substring(i,4,nchar(i))
                }
            }else if(substring(i,1,2)=='YR'){
                YRT <- as.numeric(substring(i,4,7))
            }else if(substring(i,1,2)=='K1'){
                # 关键词提取
                # 万方 空格替换为';'
                if(is.na(K1T)){
                    K1T <- gsub('  ',';',substring(i,4,nchar(i)))
                }
            }else if(substring(i,1,2)=='A1'){
                # 作者提取
                # wf '; '替换为';'
                A1T <- gsub('; ',';',substring(i,4,nchar(i)))
                # wf '  '替换为';'
                A1T <- gsub('  ',';',A1T)
                # wf去除[]
                A1T <- gsub('\\[.*\\]','',A1T)
                # vip,wf,cbm末尾加';'
                if(substring(A1T,nchar(A1T),nchar(A1T))!=';'){
                    A1T <- paste(A1T,sep='',';')
                }
            }else if(substring(i,1,2)=='AD'){
                # 机构提取
                # 先在末尾加上';'
                ADTT <- substring(i,4,nchar(i))
                if(substring(ADTT,nchar(ADTT),nchar(ADTT))!=';'){
                    ADTT <- paste(ADTT,sep='',';')
                }
                # 去除地址，使用gsub('x.*x','n',n) 替换
                ADTT <- gsub(',.*;',';',ADTT)
                # 去掉(1)
                ADTT <- gsub('\\(.\\)','',ADTT)
                # 去掉空格
                ADTT <- gsub(' ','',ADTT)
                # 去除邮编
                ADTT <- gsub('[[:digit:]]{6}','',ADTT)
                # 使用flag
                if(ADflag==1){
                    # 若AD相邻，就加在一起
                    ADTT <- paste(ADT,sep='',ADTT)
                }
                ADT <- ADTT
                # change flag
                ADflag <- 1
            }
            # 判断分类完成
        }
        # for遍历完成
        # 去除创始行
        RT <- RT[2:length(RT)]
        T1 <- T1[2:length(T1)]
        JF <- JF[2:length(JF)]
        YR <- YR[2:length(YR)]
        K1 <- K1[2:length(K1)]
        A1 <- A1[2:length(A1)]
        AD <- AD[2:length(AD)]
        SIGN <- SIGN[2:length(SIGN)]
        DS <- DS[2:length(DS)]
        # 合成data frame
        all_frame <- data.frame(RT=RT,T1=T1,JF=JF,YR=YR,K1=K1,A1=A1,AD=AD,SIGN=SIGN,DS=DS,stringsAsFactors = FALSE)
        print(length(all_frame[,1]))
        return(all_frame)
    }
#======== screen over ==========

# 创建汇总空dataframe
    sum_frame <- data.frame(RT='RT',T1='T1',JF='JF',YR=1999,K1='K1',A1='A1',AD='AD',SIGN='SIGN',DS='DS',stringsAsFactors = FALSE)

# 设置目录
path <- getwd()
inputpath <- paste(path,'/input',sep='')

# 读取文件
setwd(inputpath)
filelist <- list.files(pattern="*.txt")
for(f in filelist){
    print(f)
    ind_data <- screen(f)
    # 汇总
    sum_frame <- rbind(sum_frame,ind_data)
}
setwd(path)

# 去除初始行
sum_frame <- sum_frame[-1,]
# 得到所有数据
print('提取总数据：')
print(length(sum_frame[,1]))

# 清洗NA值
sum_frame <- na.omit(sum_frame)
# 得到有效数据
print('提取有效数据：')
print(length(sum_frame[,1]))

# 清洗重复数据
sum_frame <- sum_frame[!duplicated(sum_frame$SIGN),]
print('去除重复数据：')
print(length(sum_frame[,1]))

# 清洗英文数据
sum_frame <- sum_frame[!( grepl("[A-z]",substring(sum_frame$T1,2,3)) & grepl("[A-z]",substring(sum_frame$T1,nchar(sum_frame$T1)-2,nchar(sum_frame$T1)-1)) ),]
print('去除英文文献数据：')
print(length(sum_frame[,1]))

# 删除SIGN
sum_frame <- sum_frame[,-8]

# 分析年份数据量
# 起始，结束
print('起始年份 结束年份')
print(c(min(sum_frame$YR),max(sum_frame$YR)))
# 包括最近1年，近10年，近10年*，近5年，近5年*
# *包括最近的年份
n1 <- 0
n2 <- 0
n3 <- 0
n4 <- 0
n5 <- 0
for(i in sum_frame$YR){
    if(i == max(sum_frame$YR)){
        n1 <- n1+1
    }else if(i %in% (max(sum_frame$YR)-10):max(sum_frame$YR)-1){
        # 09-19
        n2 <- n2+1
    }else if(i %in% (max(sum_frame$YR)-9):max(sum_frame$YR)){
        # 10-20
        n3 <- n3+1
    }else if(i %in% (max(sum_frame$YR)-5):max(sum_frame$YR-1)){
        n4 <- n4+1
    }else if(i %in% (max(sum_frame$YR)-4):max(sum_frame$YR)){
        n5 <- n5+1
    }
}

print('最近1年 近10年 近10年* 近5年 近5年*')
print(c(n1,n2,n3,n4,n5))
print('*包括最近的年份')


# 写入
write.table(sum_frame,file='eligible_data.csv',sep=',',row.names = FALSE)
#read.table('eligible_data.csv',sep=',',header=TRUE,as.is=TRUE)

#for(i in 1:length(sum_frame[,1])){
#1    cat(paste('RT ',as.character(sum_frame[i,]$RT),'\n',sep=''),file='output.txt',append=TRUE)
#    cat(paste('A1 ',as.character(sum_frame[i,]$A1),'\n',sep=''),file='output.txt',append=TRUE)
#    cat(paste('AD ',as.character(sum_frame[i,]$AD),'\n',sep=''),file='output.txt',append=TRUE)
#    cat(paste('T1 ',as.character(sum_frame[i,]$T1),'\n',sep=''),file='output.txt',append=TRUE)
#    cat(paste('JF ',as.character(sum_frame[i,]$JF),'\n',sep=''),file='output.txt',append=TRUE)
#    cat(paste('YR ',as.character(sum_frame[i,]$YR),'\n',sep=''),file='output.txt',append=TRUE)
#    cat(paste('K1 ',as.character(sum_frame[i,]$K1),'\n',sep=''),file='output.txt',append=TRUE)
#    cat(paste('DS ',as.character(sum_frame[i,]$DS),'\n',sep=''),file='output.txt',append=TRUE)
#    cat('\n',file='output.txt',append=TRUE)
#}
