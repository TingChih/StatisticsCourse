library(httr)
library(XML)
library(bitops)
library(RCurl)
library(NLP)
library(tm)

data <- read.table("pop1998.csv", header = TRUE, sep = ",")

dataSongName <- gsub(" ", "+", data$song)

for(i in 58:63){
    
    url <- paste("http://mojim.com/", dataSongName[i], ".html?t3", sep = "")
        url <- content(GET(url), as = "text")
            html <- htmlParse(url)
                extractSinger <- xpathSApply(html, '//span[@class="mxsh_ss2"]/a', xmlValue)
                extractLink <- xpathSApply(html, '//span[@class="mxsh_ss4"]/a', xmlGetAttr, 'href')
    
    index <- TRUE
    j <- 1
    repeat{
        if(j <= length(extractSinger)){
            if(grepl(data$singer[i] ,extractSinger[j])){
                trueLink <- extractLink[j]
                break
            }
        }else{
            index <- FALSE
            break
        }
        j <- j+1
    }
    
    if(index){
        url1 <- paste("https://mojim.com", trueLink, sep = "")
            html1 <- htmlParse(getURL(url1))
                lyr <- xpathSApply(html1, '//dd[@id="fsZx3"]/text()', xmlValue)
        
        data$lyricist[i] <- sub("作詞：", "", lyr[grepl("作詞：", lyr)])[1]
        
        lyr <- lyr[!grepl("國際 DJ", lyr)]
        lyr <- lyr[!grepl("片頭曲", lyr)]
        lyr <- lyr[!grepl("片尾曲", lyr)]
        lyr <- lyr[!grepl("主題曲", lyr)]
        lyr <- lyr[!grepl("插曲", lyr)]
        lyr <- lyr[!grepl("廣告", lyr)]
        lyr <- lyr[!grepl("演唱：", lyr)]
        lyr <- lyr[!grepl("作詞：", lyr)]
        lyr <- lyr[!grepl("作曲：", lyr)]
        lyr <- lyr[!grepl("編曲：", lyr)]
        lyr <- lyr[!grepl("監製：", lyr)]
        lyr <- lyr[!grepl("製作：", lyr)]
        lyr <- lyr[!grepl("中文詞：", lyr)]
        lyr <- lyr[!grepl("製作人：", lyr)]
        lyr <- lyr[!grepl("更多更詳盡歌詞", lyr)]
        lyr <- lyr[!grepl("\\[", lyr)]
        
        start <- regexpr("：", lyr)
            for(k in 1:length(lyr)){
                if(start[k] == -1){
                    start[k] <- 1
                }else{
                    start[k] <- start[k]+1
                }
            }
        
        start <- regexpr(":", lyr)
            for(k in 1:length(lyr)){
                if(start[k] == -1){
                    start[k] <- 1
                }else{
                    start[k] <- start[k]+1
                }
            }
        
        lyr <- substr(lyr, start = start, stop = nchar(lyr))
        
        rowNum = length(lyr)
            lyr <- paste(lyr[1:rowNum], collapse = "")
        
        lyr <- removePunctuation(lyr)
        lyr <- gsub("～", "", lyr)
        lyr <- gsub(" ", "", lyr)
        lyr <- gsub("　", "", lyr)
        lyr <- gsub("\t", "", lyr)
        lyr <- gsub("<U\\+9EBD>", "麼", lyr)
        lyr <- gsub("<U\\+7232>", "為", lyr)
        lyr <- gsub("<U\\+7740>", "著", lyr)
        lyr <- gsub("<U\\+60A6>", "悅", lyr)
        lyr <- gsub("<U\\+5185>", "內", lyr)
        lyr <- gsub("<U\\+6E29>", "溫", lyr)
        lyr <- gsub("<U\\+8AAC>", "說", lyr)
        lyr <- gsub("\u2028", "", lyr)
        lyr <- gsub("<", "", lyr)
        lyr <- gsub(">", "", lyr)
        lyr <- gsub("\\+", "", lyr)
        lyr <- gsub("[0-9]", "", lyr)
        lyr <- gsub("[a-zA-Z]", "", lyr)
        data$lyricis[i] <- lyr
    }
}

write.table(data, "pop1998.csv", row.names = FALSE, col.names = TRUE, sep = ",")

url1 <- "https://mojim.com/twy100051x2x2.htm"
i <- 62

test <- readLines("test.txt")
    rowNum <- length(test)
        test <- paste(test[1:rowNum], collapse = "")
        test <- gsub("　", "", test)
        test <- gsub(" ", "", test)

#############################################################################################################

data <- read.table("pop1998.csv", header = TRUE, sep = ",")
    data$singer = as.character(data$singer)
    data$lyricist = as.character(data$lyricist)
    data$lyricis = as.character(data$lyricis)

test1 = NULL
    for(i in 1:nrow(data)){
        if(regexpr("、", data$singer[i]) == -1){
            test1 = c(test1, grepl(data$singer[i], data$lyricis[i]))
        }else{
            test1 = c(test1, grepl(substr(data$singer[i], start = 1, stop = regexpr("、", data$singer[i])-1), data$lyricis[i]))
        }
    }
        which(test1)

test2 = NULL
    for(i in 1:nrow(data)){
        if(regexpr("、", data$singer[i]) == -1){
            test2 = c(test2, grepl(data$singer[i], data$lyricis[i]))
        }else{
            test2 = c(test2, grepl(substr(data$singer[i], start = regexpr("、", data$singer[i]), stop = nchar(data$singer[i])), data$lyricis[i]))
        }
    }
        which(test2)

test3 = NULL
    for(i in 1:nrow(data)){
        if(regexpr("、", data$lyricist[i]) == -1){
            test3 = c(test3, grepl(data$lyricist[i], data$lyricis[i]))
        }else{
            test3 = c(test3, grepl(substr(data$lyricist[i], start = 1, stop = regexpr("、", data$lyricist[i])-1), data$lyricis[i]))
        }
    }
        which(test3)
        
test4 = NULL
    for(i in 1:nrow(data)){
            test4 = c(test4, (grepl("男", data$lyricis[i]) && grepl("女", data$lyricis[i])))
    }
        which(test4)

single <- list()
    for(i in 1:nrow(data)){
        word <- NULL
        for(j in 1:nchar(data$lyricis[i])){
            word <- c(word, substr(data$lyricis[i], j, j))
        }
        single[i] <- word
    }

singleTable <- list()
    for(i in 1:nrow(data)){
        singleTable[[i]] <- unique(single[[i]])
    }

singleTable

#############################################################################################################

data <- pop2016

for(i in 1:nrow(data)){
    word <- NULL
    for(j in 1:nchar(data$lyricis[i])){
        word <- c(word, substr(data$lyricis[i], j, j))
    }
    data$single[[i]] <- unique(word)
}

for(i in 1:nrow(data)){
    word <- NULL
    for(j in 1:(nchar(data$lyricis[i])-1)){
        word <- c(word, substr(data$lyricis[i], j, j+1))
    }
    data$double[[i]] <- unique(word)
}

for(i in 1:nrow(data)){
    data$howMany[i] <- length(data$single[[i]])
}

pop2016 <- data


boxplot(pop1998$howMany, pop1999$howMany, pop2000$howMany, pop2001$howMany, pop2002$howMany, 
        pop2003$howMany, pop2004$howMany, pop2005$howMany, pop2006$howMany, pop2007$howMany, 
        pop2008$howMany, pop2009$howMany, pop2010$howMany, pop2011$howMany, pop2012$howMany, 
        pop2013$howMany, pop2014$howMany, pop2015$howMany, pop2016$howMany, names = c(1998, 1999, 2000, 
        2001, 2002, 2003, 2004, 2005, 2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016))