#' ---
#' title: 《Hit FM排行榜預測》
#' author: Group 8
#' date:  Dec, 2015
#' output:
#'   html_document:
#'     toc: true
#'     toc_depth: 2
#'     number_sections: true
#'     highlight: pygments
#' ---

library(ggplot2)
library(RSQLite)
library(jiebaR)
library(data.table)


#' # 匯入資料
conn <- dbConnect(SQLite(), "music163-2.db")
songs <- dbReadTable(conn, "songs")
setDT(songs, key = "id")
dbDisconnect(conn = conn)

Encoding(songs$name) <- "UTF-8"
Encoding(songs$artist) <- "UTF-8"
Encoding(songs$album) <- "UTF-8"
Encoding(songs$lyric) <- "UTF-8"
Encoding(songs$comment) <- "UTF-8"

rank <- fread("HitFM-hot.tsv", sep = "\t")

songs <- merge(songs, rank[, !c("name", "artist") , with = FALSE], by = "id")

POS <- readLines("sentiment_dict/POS.txt", encoding = "UTF-8")
NEG <- readLines("sentiment_dict/NEG.txt", encoding = "UTF-8")
emoji <- readLines("emoji", encoding = "UTF-8")

#' # 爬 Wiki 的日韓電視劇
#+ engine='python', engine.path='python3', highlight=TRUE
#import re
#import requests
#from bs4 import BeautifulSoup
#
#p = re.compile(r'\b\w+\b')
#
#r = requests.get('https://zh.wikipedia.org/zh-cn/韩国电视剧列表')
#s = BeautifulSoup(r.text, 'lxml')
#TV_kr = {x2 for x in s.select('.NavFrame li') for x2 in p.findall(x.text)}
#with open('TV_kr', 'w') as f:
#    for show in TV_kr:
#        f.write(show + '\n')
#
#r = requests.get('https://zh.wikipedia.org/zh-cn/日本電視劇列表')
#s = BeautifulSoup(r.text, 'lxml')
#TV_jp = {x2 for x in s.select('.NavFrame li') for x2 in p.findall(x.text)}
#with open('TV_jp', 'w') as f:
#    for show in TV_jp:
#        f.write(show + '\n')

#' # 匯入影劇資料
TV_kr <- readLines("TV_kr", encoding = "UTF-8")
TV_jp <- readLines("TV_jp", encoding = "UTF-8")


#' # 將情緒用詞、emoji、影劇名、歌手名加入jieba字典庫
fpath <- gsub("jieba\\.", "user.", jiebaR::DICTPATH)
lapply(mget(c("TV_kr", "TV_jp", "POS", "NEG", "emoji")), write, file = fpath)
artist_fullName <- grep("\\s", songs$artist)
artist_underline <- gsub("\\s", "_", songs[artist_fullName, artist]
artist_seg <- songs[, .(regmatches(artist, gregexpr("(\\b\\S+\\b)", artist)))]

fullName_count <- sapply(artist_fullName,
    function(i) NROW(songs[i, gregexpr(artist, comment, fixed = TRUE, ignore.case = T)]))

artist_count <- sapply(artist_fullName, 
songs[i gsub(artist[57], artist_underline[99], songs$comment[57], fixed = TRUE)]))


#' # 斷詞、計算留言數
SEG <- worker()
p1 <- "\\[(.*?)\\]\\s+作[词曲](.*?)\n"
p2 <- "\\[\\d+:\\d+.\\d+\\](.*?)\n"

songs[, `:=`(
    cmmt_seg = lapply(comment, function(x) SEG <= x),
    lyric_seg = lapply(lyric,
        function(x) SEG <= gsub(p2, "\\1 ", gsub(p1, "", x))),
    cmmt_number = sapply(comment,
        function(x) length(unlist(strsplit(x, "<|>", fixed = TRUE))), 
        USE.NAMES = FALSE)
    )]

#' # 情感分析：運用情緒字典，計算情感分數
songs[, `:=`(
    POS_score = sapply(cmmt_seg, function(x) sum(x %in% POS)),
    POS_lyric = sapply(lyric_seg, function(x) sum(x %in% POS)),
    NEG_score = sapply(cmmt_seg, function(x) sum(x %in% NEG)),
    NEG_lyric = sapply(lyric_seg, function(x) sum(x %in% NEG))
    )]

#' # 排名與歌手相關性
which(coef(summary(lm(rank ~ artist, data = songs)))[, 'Pr(>|t|)'] < 0.1)

#' # 排名與評論數相關性
cor.test(songs$rank, songs$cmmt_number, method = "kendall")# use kendall to handle ties

#' # 排名與評論情緒相關性
cor.test(songs$rank, songs[, POS_score - NEG_score], method = "kendall")
cor.test(songs$POS_score, songs$NEG_score, method = "kendall") # postive emotion is significantly associated with negative emotion
cor.test(songs$rank, songs$NEG_score, method = "kendall")
cor.test(songs$rank, songs$POS_score, method = "kendall")

#' # 人工tagging 歌詞的語種
#' # 排名與語種相關性
model <- aov(rank ~ lang, data = songs)
summary(model)
TukeyHSD(model)
plot(TukeyHSD(model))

g <- ggplot(songs) + geom_boxplot(aes(lang, rank, fill = lang)) + coord_flip()
print(g)

#' # 排名與國藉相關性
model <- aov(rank ~ nation, data = songs)
summary(model)
TukeyHSD(model)
plot(TukeyHSD(model))

g <- ggplot(songs) + geom_boxplot(aes(nation, rank, fill = nation)) + coord_flip()
print(g)

#' # 計算歌詞相關評論佔討論串的比例 
songs[, r_lyricCmmt := mean(unlist(cmmt_seg) %in% unlist(lyric_seg)), by = .(id, name)]
model <- aov(r_lyricCmmt ~ lang, data = songs)
summary(model)
TukeyHSD(model)
g <- ggplot(songs) + 
    geom_text(aes(seq_len(nrow(songs)), r_lyricCmmt, label = name, color = lang)) +
    labs(color = "language", x = "song", y = "ratio") +
    ggtitle("歌詞相關評論佔討論串之比例")
print(g)

#' ## 小結：除了中文歌以外，其他語種的歌雖然大多有翻譯歌詞，但鮮少與歌詞共鳴。

#' # 排名與影劇相關性
#+ warning=F
songs[, `:=`(
        kr_score = sapply(cmmt_seg, function(x) sum(x %in% TV_kr)),
        jp_score = sapply(cmmt_seg, function(x) sum(x %in% TV_jp))
    )]

cor.test(songs$kr_score, songs$jp_score, method = "kendall")
cor.test(songs$rank, songs$kr_score, method = "kendall")
cor.test(songs$rank, songs$jp_score, method = "kendall")


#' # 檢查
songs[kr_score >= 20, .(name, artist)]
songs[jp_score >= 20, .(name, artist)]


#' # 分語言分析排名與日韓影劇的相關性
#+ warning=F
table(songs$lang)
songs_kr <- songs[lang == 'kr']
songs_jp <- songs[lang == 'jp']

cor.test(songs_kr$rank, songs_kr$kr_score, method = "kendall")
cor.test(songs_jp$rank, songs_jp$jp_score, method = "kendall")

lbl <- c("非前30名", "前30名")
songs_kr[, rank_two := factor(ifelse(rank >= 30, 1, 0), labels = lbl)]
songs_jp[, rank_two := factor(ifelse(rank >= 30, 1, 0), labels = lbl)]

model <- aov(kr_score ~ rank_two, data = songs_kr)
summary(model)
model <- aov(jp_score ~ rank_two, data = songs_jp)
summary(model) # jp_score is moderately associated with top30 rank or not in japanese language group

g <- ggplot(songs_jp) + 
        geom_boxplot(aes(rank_two, jp_score, group = rank_two)) +
        xlab("排名") +
        ylab("回覆提到戲劇名稱的次數")
print(g)

#' # 分語言分析排名與情緒的相關性(加入中文英文歌的分群)
#+ warning=F
cor.test(songs_kr$rank, songs_kr[, POS_score - NEG_score], method = "kendall")
cor.test(songs_kr$rank, songs_kr$NEG_score, method = "kendall")
cor.test(songs_kr$rank, songs_kr$POS_score, method = "kendall")

cor.test(songs_jp$rank, songs_jp[, POS_score - NEG_score], method = "kendall")
cor.test(songs_jp$rank, songs_jp$NEG_score, method = "kendall")
cor.test(songs_jp$rank, songs_jp$POS_score, method = "kendall")

songs_ch <- songs[lang == 'ch']
songs_en <- songs[lang == 'en']

cor.test(songs_ch$rank, songs_ch[, POS_score - NEG_score], method = "kendall")
cor.test(songs_ch$rank, songs_ch$NEG_score, method = "kendall")
cor.test(songs_ch$rank, songs_ch$POS_score, method = "kendall")

cor.test(songs_en$rank, songs_en[, POS_score - NEG_score], method = "kendall")
cor.test(songs_en$rank, songs_en$NEG_score, method = "kendall")
cor.test(songs_en$rank, songs_en$POS_score, method = "kendall")

#' # Logistic regression for Janpanese language songs (top30 or not) 
#+ warning=F
fit <- glm(rank_two ~ POS_score + NEG_score + jp_score, data = songs_jp, family = binomial())
summary(fit)

#' ## 小結:情緒對不同語言歌曲的排名都沒有顯著影響.但是否和日劇類相關,則會顯著影響日文歌曲進入前30名的機率(p<0.1)

#' # 分國籍分析排名與日韓影劇的相關性
#+ warning=F
table(songs$nation)
songs_kr <- songs[nation == 'kr']
songs_jp <- songs[nation == 'jp']

cor.test(songs_kr$rank, songs_kr$kr_score, method = "kendall")
cor.test(songs_jp$rank, songs_jp$jp_score, method = "kendall")

lbl <- c("非前30名", "前30名")
songs_kr[, rank_two := factor(ifelse(rank >= 30, 1, 0), labels = lbl)]
songs_jp[, rank_two := factor(ifelse(rank >= 30, 1, 0), labels = lbl)]

model <- aov(kr_score ~ rank_two, data = songs_kr)
summary(model)
model <- aov(jp_score ~ rank_two, data = songs_jp)
summary(model)


#' # 分語言分析排名與情緒的相關性(加入中文英文歌手的分群)
#+ warning=F
cor.test(songs_kr$rank, songs_kr[, POS_score - NEG_score], method = "kendall")
cor.test(songs_kr$rank, songs_kr$NEG_score, method = "kendall")
cor.test(songs_kr$rank, songs_kr$POS_score, method = "kendall")

cor.test(songs_jp$rank, songs_jp[, POS_score - NEG_score], method = "kendall")
cor.test(songs_jp$rank, songs_jp$NEG_score, method = "kendall")
cor.test(songs_jp$rank, songs_jp$POS_score, method = "kendall")

songs_ch <- songs[nation == 'ch']
songs_en <- songs[nation == 'en']

cor.test(songs_ch$rank, songs_ch[, POS_score - NEG_score], method = "kendall")
cor.test(songs_ch$rank, songs_ch$NEG_score, method = "kendall")
cor.test(songs_ch$rank, songs_ch$POS_score, method = "kendall")

cor.test(songs_en$rank, songs_en[, POS_score - NEG_score], method = "kendall")
cor.test(songs_en$rank, songs_en$NEG_score, method = "kendall")
cor.test(songs_en$rank, songs_en$POS_score, method = "kendall")
