library(NLP)
library(tm)
library(wordcloud)

term <- c("�Ĥ@��.txt", "�ĤG��.txt", "�ĤT��.txt", "�ĥ|��.txt", "�Ĥ���.txt", "�Ĥ���.txt", "�ĤC��.txt", 
          "�ĤK��.txt", "�ĤE��.txt", "�ĤQ��.txt", "�ĤQ�@��.txt", "�ĤQ�G��.txt", "�ĤQ�T��.txt", "�ĤQ�|��.txt")

speaking <- NULL
for(i in 1:14){
    speak <- readLines(term[i])
    speak <- removePunctuation(speak)
    speak <- removeNumbers(speak)
    row = length(speak)
    speak <- paste(speak[1:row], collapse = "")
    speak <- gsub("�@", "", speak)
    speak <- gsub(" ", "", speak)
    speaking <- c(speaking, speak)
}

total <- NULL
for(i in 1:14){
    total <- c(total, nchar(speaking[i]))
}

differ <- NULL
for(i in 1:14){
    differ <- c(differ, length(unique(single[[i]])))
}

simpson <- NULL
for(i in 1:14){
    simpson <- c(simpson, sum((singleTable[[i]]/total[i])^2))
}

single <- list()
for(i in 1:14){
    word <- NULL
    for(j in 1:total[i]){
        word <- c(word, substr(speaking[i], j, j))
    }
    single[[i]] <- word
}

singleTable <- list()
for(i in 1:14){
    singleTable[[i]] <- sort(table(single[[i]]), decreasing = TRUE)
    write.table(singleTable[[i]],  paste("single", i, ".csv", sep = ""), sep=",")
}

double <- list()
for(i in 1:14){
    word <- NULL
    for(j in 1:(total[i]-1)){
        word <- c(word, substr(speaking[i], j, j+1))
    }
    double[[i]] <- word
}

doubleTable <- list()
for(i in 1:14){
    doubleTable[[i]] <- sort(table(double[[i]]), decreasing = TRUE)
    write.table(doubleTable[[i]],  paste("double", i, ".csv", sep = ""), sep=",")
}

doubleFrame <- data.frame(word = names(doubleTable[[i]]), freq = as.numeric(doubleTable[[i]]))

wordcloud(doubleFrame$word, doubleFrame$freq, scale = c(8,.2), min.freq = 3, max.words = 100, 
          random.order = FALSE, colors = c("#7F7F7F", "#5F9EA0", "#FF8C69"))

##################################################################################################

chapDragon <- c("���s�K���Ĥ@�^.txt", "���s�K���ĤG�^.txt", "���s�K���ĤT�^.txt", "���s�K���ĥ|�^.txt", "���s�K���Ĥ��^.txt")

dragonAll <- NULL
for(i in 1:5){
    dragon <- readLines(chapDragon[i])
    dragon <- removePunctuation(dragon)
    dragon <- removeNumbers(dragon)
    row = length(dragon)
    dragon <- paste(dragon[1:row], collapse = "")
    dragon <- gsub("�@", "", dragon)
    dragon <- gsub(" ", "", dragon)
    dragonAll <- paste(dragonAll, dragon, sep = "")
}

totalDragon <- nchar(dragonAll)

singleDragon <- NULL
for(i in 1:totalDragon){
    singleDragon <- c(singleDragon, substr(dragonAll, i, i))
}

singleDragonTable <- sort(table(singleDragon), decreasing = TRUE)

chapRed <- c("���ӹڲĤ@�^.txt", "���ӹڲĤG�^.txt", "���ӹڲĤT�^.txt", "���ӹڲĥ|�^.txt", "���ӹڲĤ��^.txt")

redAll <- NULL
for(i in 1:5){
    red <- readLines(chapRed[i])
    red <- removePunctuation(red)
    red <- removeNumbers(red)
    row = length(red)
    red <- paste(red[1:row], collapse = "")
    red <- gsub("�@", "", red)
    red <- gsub(" ", "", red)
    redAll <- paste(redAll, red, sep = "")
}

totalRed <- nchar(redAll)

singleRed <- NULL
for(i in 1:totalRed){
    singleRed <- c(singleRed, substr(redAll, i, i))
}

singleRedTable <- sort(table(singleRed), decreasing = TRUE)


classical <- c("�o", "�G", "�j", "�j", "�v", "��", "�Z", "��", "�D", "�L")

modern <- c("��", "�O", "��", "��", "�F", "�M", "��", "��", "��", "�a")

classicalDragon <- 0
for(i in 1:10){
    classicalDragon <- sum(classicalDragon, sum(grepl(classical[i], singleDragon)))
}

classicalRed <- 0
for(i in 1:10){
    classicalRed <- sum(classicalRed, sum(grepl(classical[i], singleRed)))
}

prop.test(c(classicalDragon, classicalRed), c(totalDragon, totalRed))

modernDragon <- 0
for(i in 1:10){
    modernDragon <- sum(modernDragon, sum(grepl(modern[i], singleDragon)))
}

modernRed <- 0
for(i in 1:10){
    modernRed <- sum(modernRed, sum(grepl(modern[i], singleRed)))
}

prop.test(c(modernDragon, modernRed), c(totalDragon, totalRed))

##################################################################################################

dragon <- NULL
for(i in 1:5){
    dragon <- c(dragon, readLines(chapDragon[i]))
}

row = length(dragon)
dragon <- paste(dragon[1:row], collapse = "")
dragon <- gsub("�@", "", dragon)
dragon <- gsub(" ", "", dragon)
dragon <- gsub("\t", "", dragon)
dragon <- unlist(strsplit(dragon, '�K|�B|�A|�C|�J|�G|�Q|�F|�I|�H'))
dragon <- removePunctuation(dragon)
dragon <- removeNumbers(dragon)
xx <- table(nchar(dragon))
xx <- xx[-1]
xx[20] <- 53
xx <- xx[-(21:26)]
xx <- xx/sum(xx)

red <- NULL
for(i in 1:5){
    red <- c(red, readLines(chapRed[i]))
}

row = length(red)
red <- paste(red[1:row], collapse = "")
red <- gsub("�@", "", red)
red <- gsub(" ", "", red)
red <- gsub("\t", "", red)
red <- unlist(strsplit(red, '�K|�B|�A|�C|�J|�G|�Q|�F|�I|�H'))
red <- removePunctuation(red)
red <- removeNumbers(red)
yy <- table(nchar(red))
yy <- yy[-1]
yy[7] <- 553
yy[8] <- 451
yy[8] <- 293
yy[15] <- 39
yy[20] <- 15
yy <- yy[-(21:28)]

yy <- yy/sum(yy)
xy <- cbind(xx, yy)

matplot(xy, type="l", lty = c(1, 2), col = c(1, 2), xlab = "�r��", ylab = "���")
legend(16.5, 0.18, c("���s�K��", "���ӹ�"), lty = c(1, 2), col = c(1, 2))