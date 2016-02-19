library(data.table)
library(e1071)
library(rpart)
library(randomForest)
library(jiebaR)
library(rvest)
stopword <- list.files("stopword", full.names = TRUE)
stopword <- unlist(sapply(stopword, scan, what = 'char', sep = "\n"), use.names = FALSE)

url <- 'https://zh.wikipedia.org/zh-tw/国家列表_(按洲排列)'
tag_table <- html_nodes(read_html(url), '.wikitable')
country <- sapply(tag_table, function(x) {
    html_table(x, fill = T)[, 2:3] 
})
country <- grep("\\S", unlist(country), value = TRUE)
write(country, file = USERPATH, append = TRUE)
china <- c('中華民國', '臺北', '台北','中國', '北京',
           '香港', '澳門', '兩岸', '大陸', '臺商', 
           '人民幣', '台商')
country <- country[!country %in% china[1:7]]

pronoun <- c("妳", "你", "我")

seg <- worker()
train_raw <- fread("train.csv", sep = ",")
train_raw$news <- iconv(train_raw$news, from = 'BIG5', to = "UTF-8")
train_raw$news <- gsub("[[:punct:]]", " ", train_raw$news)
train_raw$news <- sapply(train_raw$news, function(news) {
    txt <- seg <= news
    txt[!txt %in% stopword]
})
train_raw$category <- as.factor(train_raw$category)

#---------------------------------------
set.seed(1234)
index <-sample(10000, 8000)
train <- train_raw[index, ]
test <- train_raw[-index, ]

words <- tapply(train$news, train$category, function(x) unlist(x))
word_freq <- lapply(words, function(x) names(tail(sort(table(x)), 5000)))

word_diff <- data.frame(category = 1:10, diff = NA)
for (i in 1:10) {
    word_diff$diff[i] <- list(word_freq[[i]][!word_freq[[i]] %in% unlist(word_freq[-i])])
}
word_diff$diff <- lapply(word_diff$diff, tail, n = 500)


train_category <- lapply(train$news, function(news) {
    sapply(word_diff$diff, function(word) {
        sum(unique(news) %in% word)
    })
})

test_category <- lapply(test$news, function(news) {
    sapply(word_diff$diff, function(word) {
        sum(unique(news) %in% word)
    })
})

train$country <- sapply(train$news, function(x) sum(x %in% country))
train$china <- sapply(train$news, function(x) sum(x %in% china[-(1:3)]))
train$pronoun <- sapply(train$news, function(x) sum(x %in% pronoun)) > 4
train <- cbind(train, as.matrix(t(as.data.frame(train_category))))
train <- train[, c(2, 4:ncol(train)), with = FALSE]

test$country <- sapply(test$news, function(x) sum(x %in% country))
test$china <- sapply(test$news, function(x) sum(x %in% china[-(1:3)]))
test$pronoun <- sapply(test$news, function(x) sum(x %in% pronoun)) > 4
test <- cbind(test, as.matrix(t(as.data.frame(test_category))))
test <- test[, c(2, 4:ncol(test)), with = FALSE]


tree <- rpart(category ~ ., data = train)
result <- predict(tree, test, type = 'class')
(cross_table <- table(result, test$category))
sum(diag(cross_table))

SVM <- svm(category ~ ., data = train)
result <- predict(SVM, test, type = 'class')
(cross_table <- table(result, test$category))
sum(diag(cross_table))

forest <- randomForest(category ~ ., data = train)
result <- predict(forest, test, type = 'class')
(cross_table <- table(result, test$category))
sum(diag(cross_table))


#---------------------------------------

TEST <- fread("test.csv")

TEST$news <- gsub("[[:punct:]]", " ", TEST$news)
TEST$news <- sapply(TEST$news, function(news) {
    txt <- seg <= news
    txt[!txt %in% stopword]
})

TEST_category <- lapply(TEST$news, function(news) {
    sapply(word_diff$diff, function(word) {
        sum(unique(news) %in% word)
    })
})

TEST$country <- sapply(TEST$news, function(x) sum(x %in% country))
TEST$china <- sapply(TEST$news, function(x) sum(x %in% china[-(1:3)]))
TEST$pronoun <- sapply(TEST$news, function(x) sum(x %in% pronoun)) > 4
TEST <- cbind(TEST, as.matrix(t(as.data.frame(TEST_category))))
TEST <- TEST[, c(3:ncol(TEST)), with = FALSE]


RESULT <- predict(tree, TEST, type = 'class')
output <- data.table(id = 10001:11000, cagegory = RESULT)
write.table(output, file = "output_DTree.csv", quote = FALSE, sep = ",", row.names = FALSE)

RESULT <- predict(forest, TEST, type = 'class')
output <- data.table(id = 10001:11000, cagegory = RESULT)
write.table(output, file = "output_RF.csv", quote = FALSE, sep = ",", row.names = FALSE)
