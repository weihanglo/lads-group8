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
library(rvest)
library(jiebaR)
library(data.table)


#' # 匯入資料
conn <- dbConnect(SQLite(), "music163-2.db")
songs <- dbReadTable(conn, "songs")
setDT(songs, key = "id")
dbDisconnect(conn = conn)

rank <- fread("HitFM-hot.tsv", sep = "\t")

songs <- merge(songs, rank[, !c("name", "artist") , with = FALSE], by = "id")

POS <- readLines("sentiment_dict/POS.txt")
NEG <- readLines("sentiment_dict/NEG.txt")
emoji <- readLines("emoji")

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
TV_kr <- readLines("TV_kr")
TV_jp <- readLines("TV_jp")


#' 將情緒用詞、emoji、影劇名、歌手名加入jieba字典庫
fpath <- gsub("jieba\\.", "user.", jiebaR::DICTPATH)
#regmatches(songs$artist, gregexpr("(\\b\\S+\\b)", songs$artist))
#write(songs$artist, fpath, append = TRUE)
lapply(mget(c("TV_kr", "TV_jp", "POS", "NEG", "emoji")), write, file = fpath)


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

#' # 情感分析
songs[, `:=`(
    POS_score = sapply(cmmt_seg, function(x) sum(x %in% POS)),
    POS_lyric = sapply(lyric_seg, function(x) sum(x %in% POS)),
    NEG_score = sapply(cmmt_seg, function(x) sum(x %in% NEG)),
    NEG_lyric = sapply(lyric_seg, function(x) sum(x %in% NEG))
    )]
    sapply(songs$cmmt_seg, function(x) sum(x %in% TV_jp))

#' # 排名與歌手相關性
which(coef(summary(lm(rank ~ artist, data = songs)))[, 'Pr(>|t|)'] < 0.1)

#' # 排名與評論數相關性
cor(songs$rank, songs$cmmt_number, method = "spearman")

#' # 排名與評論情緒相關性
cor(songs$rank, songs[, POS_score - NEG_score], method = "spearman")
cor(songs$POS_score, songs$NEG_score, method = "spearman")
cor(songs$rank, songs$NEG_score, method = "spearman")
cor(songs$rank, songs$POS_score, method = "spearman")


#' # 人工tagging 歌詞的語種
#' # 排名與語種相關性
model <- aov(rank ~ lang, data = songs)
summary(model)
TukeyHSD(model)

g <- ggplot(songs) + 
    geom_boxplot(aes(lang, rank, fill = lang)) + 
    coord_flip()
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

#' # 計算影劇相關評論佔討論串的比例 
