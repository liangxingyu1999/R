# merge4
----------------
## 概述

Citespace支持对CNKI数据库导出格式进行分析，但对于其余中文数据库的兼容性不佳。为了扩展Citespace的文献分析广度，merge4将对其余中文数据库导出数据进行合并、清洗，用以辅助数据分析。

## 程序原理

Citespace支持对CNKI导出的Refworks格式数据进行分析。通过merge4，国内4大中文数据库：中国知网，维普，万方，CBM数据库导出的Refworks格式的中文文献数据，被提取合并，清洗，去重，最终输出符合citespace软件能够分析的数据文件。

## 程序功能

* 合并4大数据库检索数据
* 剔除无效数据
* 去除重复数据
* 输出结果

## 使用方法

1. 在程序所在的目录下，建立名为input的文件夹，并向其中放入以Refworks格式导出的检索结果文件，文件应为纯文本文件(\*.txt)
2. 使用`Rscript combine_v*.r`运行软件
3. 将产生类似以下输出结果，并在软件所在目录生成名为**eligible\_data.csv**的输出文件(文件**eligible_data.csv**为R语言data frame格式。)
> [1] "提取总数据："  
> [1] 301  
> [1] "提取有效数据："  
> [1] 169  
> [1] "去除重复数据："  
> [1] 131  
> [1] "去除英文文献数据："  
> [1] 131  
> [1] "起始年份 结束年份"  
> [1] 1992 2020  
> [1] "最近1年 近10年 近5年 文献数量"  
> [1]  5 28 28  
4. 使用`Rscript write.r`将数据以Refworks格式编码，并产生**output.txt**
5. **output.txt**即可被Citespace识别分析。

## 例程结构

merge4  
 |-combine\_v2.8.r  
 |-write.r  
 |-input  
 | |-cbm.txt  
 | |-cnki.txt  
 | |-cqvip.txt  
 | |-WanFangdata.txt  
 |-eligible\_data.csv  
 |-output.txt  
 |-README.md  


