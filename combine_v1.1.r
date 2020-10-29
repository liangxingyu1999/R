# 读取文件并提取数据
screen <- function(f){
    lines <- readLines(f)
    # make new vector
    RT <- c('RT')
    T1 <- c('T1')
    JF <- c('JF')
    YR <- c('YR')
    K1 <- c('K1')
    A1 <- c('A1')
    AD <- c('AD')
    # flag
    flag <- 0
    # 遍历文件
    for(i in lines){
        # change flag
        if(substring(i,1,2)!='AD'){
            flag <- 0
        }
        if(substring(i,1,2)=='RT'){
            RT <- append(RT,substring(i,4,nchar(i)))
        }else if(substring(i,1,2)=='T1'){
            T1 <- append(T1,substring(i,4,nchar(i)))
        }else if(substring(i,1,2)=='JF'){
            JF <- append(JF,substring(i,4,nchar(i)))
        }else if(substring(i,1,2)=='YR'){
            YR <- append(YR,substring(i,4,nchar(i)))
        }else if(substring(i,1,2)=='K1'){
            # 关键词提取
            # 万方 空格替换为';'
            KK <- gsub('  ',';',substring(i,4,nchar(i)))
            K1 <- append(K1,KK)
        }else if(substring(i,1,2)=='A1'){
            # 作者提取
            # wf '; '替换为','
            AA <- gsub('; ',';',substring(i,4,nchar(i)))
            # vip,wf,cbm末尾加';'
            if(substring(AA,nchar(AA),nchar(AA))!=';'){
                AA <- paste(AA,sep='',';')
            }
            A1 <- append(A1,AA)
        }else if(substring(i,1,2)=='AD'){
            # 机构提取
            # 先在末尾加上';'
            DD <- substring(i,4,nchar(i))
            if(substring(DD,nchar(DD),nchar(DD))!=';'){
                DD <- paste(DD,sep='',';')
            }
            # 去除地址，使用gsub('x.*x','n',n) 替换
            DD <- gsub(',.*;',';',DD)
            # 去掉(1)
            DD <- gsub('\\(.\\)','',DD)
            # 去掉空格
            DD <- gsub(' ','',DD)
            # 使用flag
            print(flag)
            if(flag==1){
                # 若AD相邻，就加在一起
                oldDD <- AD[length(AD)]
                AD <- AD[1:length(AD)-1]
                DD <- paste(oldDD,sep='',DD)
                AD <- append(AD,DD)
            }else{
                # 若不相邻，追加vector
                AD <- append(AD,DD)
            }
            # change flag
            flag <- 1
        }
    }
    # 遍历完成
    # 去除创始行
    RT <- RT[2:length(RT)]
    T1 <- T1[2:length(T1)]
    JF <- JF[2:length(JF)]
    YR <- YR[2:length(YR)]
    K1 <- K1[2:length(K1)]
    A1 <- A1[2:length(A1)]
    AD <- AD[2:length(AD)]
    print(K1)
    # 合成data frame
    all_frame <- data.frame(RT=RT,T1=T1,JF=JF,YR=YR,K1=K1,A1=A1,AD=AD,stringsAsFactors = FALSE)
    print(summary(all_frame))
    return(all_frame)
}





a=screen('cbm.txt')




#filelist <- list.files(pattern="*.txt")

#for( f in filelist){
#    data <- screen(f)
#    final_data <- rbind()
#}

