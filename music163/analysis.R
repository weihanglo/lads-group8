#' ---
#' title: 《Hit FM排行榜預測》
#' subtitle: "李蕙宇　王中玉　蘇瑜寧　洪晨碩　簡汝芸　羅偉航"
#' author: "Github: https://github.com/weihanglo/lads-group8"
#' date:  Jan, 2016
#' output:
#'   pdf_document:
#'     toc: true
#'     toc_depth: 2
#'     number_sections: true
#'     highlight: pygments
#'     latex_engine: xelatex
#'     includes:
#'       in_header: header.tex
#' ---

#' # 研究議題
#' 目前國外對於歌詞的語言分析，多著重在歌詞或旋律帶給聽眾的情緒感受．忽略聽眾對
#' 於歌曲的詮釋，除了情感的面向外．還包括歌曲的社會層面．一首成功熱銷的歌曲，經
#' 常與歌手名氣，當時的流行曲風，熱播戲劇，國家產生連結．近年來大陸和歐美吹起的
#' 韓式K-pop音樂風潮（如少女時代，super junior），就是一個很好的例子．  

#' <br>

#' 本研究立基在這樣的觀察上，希望透過分析大陸歌曲網站－網易雲－的網友評價，了解
#' 一首歌曲之所以熱門，到底該歸功於它本身引發的情緒共鳴，還是歸功於它的社會條件
#' ．我們研究的社會條件包括歌手名氣，歌手國籍，以及有沒有搭配熱門戲劇．  

#' <br>

#' 進一步，我們希望把這些歌曲依據語言分成中文組，日文組，韓文組，英文組．探索哪
#' 一種語言的歌，其熱門程度是因為它的社會條件，而非歌曲引發的情緒效果．我們初步
#' 假設，大陸網友對日文歌與韓文歌的喜好程度，主要取決於歌手本人的名氣，國籍，還
#' 有是否搭配熱門戲劇．至於中文和英文歌，則比較是受到歌曲本身引發的情緒所影響．

#' 

#' # 研究假設   

#' -   KR v.s. JP  
#'     -   在韓文和日文歌的group中，網友提到電視劇的比例（提到偶像劇的次數／總留言數）與歌曲的排名＂正相關＂．  
#'     -   在韓文和日文歌的group中，網友提到的情緒用詞比例（提到情緒用詞的數量／總token數），與歌曲排名＂無相關＂．  

#' 

#' # 操作方式

#' -   **資料來源**：網易雲Hit FM 2010 - 2015 榜單
#'     -   2015: http://music.163.com/m/playlist?id=148858141
#'     -   2014: http://music.163.com/m/playlist?id=148822545
#'     -   2013: http://music.163.com/m/playlist?id=148821771
#'     -   2012: http://music.163.com/m/playlist?id=148822543
#'     -   2011: http://music.163.com/m/playlist?id=148813956
#'     -   2010: http://music.163.com/m/playlist?id=148818810
#' -   **情緒用詞**：以中研院，大連理工的情緒辭庫當作辭典，抓取網友回覆的情緒用詞比例    
#'     -   計算每首歌的留言的情緒分數均值／總和
#'     -   計算每首歌的正負向留言總數／比例
#' -   **歌手名氣**：由小組中的專家判斷XD
#' -   **歌手國籍**：由小組成員共同coding
#' -   **是否與影劇有關**：根據wikipedia的各國電視劇列表，建立辭典，抓取網友回覆提到電視劇的比例．
#'     -   https://zh.wikipedia.org/zh-cn/韩国电视剧列表
#'     -   https://zh.wikipedia.org/zh-cn/日本電視劇列表

#'

#' # 討論

#' 本研究針對韓文和日文歌曲進行排名與情緒還有戲劇名的比較分析．與原先假設不同，
#' 我們發現韓文歌與韓劇是否相關，不會影響到排名的高低．情緒則與原先預期相同，都
#' 與排名無顯著關係．有趣的是，是否和日劇類相關,則會顯著影響日文歌曲進入前30名
#' 的機率(p<0.1)．顯示對網易云的網友來說，能進到排名前30名的日文歌曲，通常會與
#' 日劇有很強的連結．韓劇則無此現象。     

#' <br>

#' 在資料探索的過程中，我們發現有些歌手不一定會唱自己國籍的歌，像是韓國團體super 
#' junior就唱了不少中文歌曲，我們認為這類情形會干擾到我們對歌曲的分組，例如把知
#' 名韓國歌手的中文歌，編碼到中文語言類別，針對這個狀況，我們另外以歌手的國籍進
#' 行分組，分析韓國歌手和日文歌手中，排名與情緒還有戲劇名的關係，結果並沒有顯著
#' 相關，顯示國籍的影響沒有預期中的大。  

#' 


#' # 源代碼
#+ warning=F, message=F, error=F, results="hide"
library(ggplot2)
library(RSQLite)
library(jiebaR)
library(data.table)


#' ## 匯入資料
conn <- dbConnect(SQLite(), "music163-2.db")
songs <- dbReadTable(conn, "songs")
setDT(songs, key = "id")
dbDisconnect(conn = conn)

rank <- fread("HitFM-hot.tsv", sep = "\t")
songs <- merge(songs, rank[, !c("name", "artist") , with = FALSE], by = "id")
cols <- c("name", "artist", "album", "lyric", "comment")
songs[, (cols) := lapply(.SD, tolower), .SDcols = cols]
songs[, (cols) := lapply(.SD, `Encoding<-`, value = "UTF-8"), .SDcols = cols]

POS <- tolower(readLines("sentiment_dict/POS.txt", encoding = "UTF-8"))
NEG <- tolower(readLines("sentiment_dict/NEG.txt", encoding = "UTF-8"))
emoji <- tolower(readLines("emoji", encoding = "UTF-8"))

#' ## 爬 Wiki 的日韓電視劇（Python）
#+ engine='python', engine.path='python3'
import re
import requests
from bs4 import BeautifulSoup

p = re.compile(r'\b\w+\b')

r = requests.get('https://zh.wikipedia.org/zh-cn/韩国电视剧列表')
s = BeautifulSoup(r.text, 'lxml')
TV_kr = {x2 for x in s.select('.NavFrame li') for x2 in p.findall(x.text)}
with open('TV_kr', 'w') as f:
    for show in TV_kr:
        f.write(show + '\n')

r = requests.get('https://zh.wikipedia.org/zh-cn/日本電視劇列表')
s = BeautifulSoup(r.text, 'lxml')
TV_jp = {x2 for x in s.select('.NavFrame li') for x2 in p.findall(x.text)}
with open('TV_jp', 'w') as f:
    for show in TV_jp:
        f.write(show + '\n')

#+ message=F
TV_kr <- tolower(readLines("TV_kr", encoding = "UTF-8"))
TV_jp <- tolower(readLines("TV_jp", encoding = "UTF-8"))


#' ## 將情緒用詞、emoji、影劇名、加入jieba字典庫
#+ eval=F
fpath <- gsub("jieba\\.", "user.", jiebaR::DICTPATH)
lapply(mget(c("TV_kr", "TV_jp", "POS", "NEG", "emoji")), write, file = fpath)



#' ## 斷詞、計算留言數
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

#' ## 計算歌詞相關評論佔討論串的比例 
songs[, r_lyricCmmt := mean(unlist(cmmt_seg) %in% unlist(lyric_seg)), by = .(id, name)]
model <- aov(r_lyricCmmt ~ lang, data = songs)
summary(model)
TukeyHSD(model)

#+ warning=F, message=F, error=F, fig.width=13, fig.height=7, fig.showtext=T
g <- ggplot(songs[order(lang)]) + 
    geom_text(aes(seq_len(nrow(songs)), r_lyricCmmt, label = name, color = lang)) +
    labs(color = "language", x = "song", y = "ratio") +
    ggtitle("歌詞相關評論佔討論串之比例")
print(g)

#' ## 排名與評論數相關性
cor.test(songs$rank, songs$cmmt_number, method = "kendall")# use kendall to handle ties

#' ### 小結：除了中文歌以外，其他語種的歌雖然大多有翻譯歌詞，但鮮少與歌詞共鳴。



#' ## 計算歌手名在評論中被吶喊的次數
songs[, artist_N := length(gregexpr(artist, comment, fixed = TRUE)[[1]]), by = id]
songs[, artist_seg := regmatches(artist, gregexpr("(\\b\\S+\\b)", artist))]
songs[, artist_seg_N := sum(sapply(unlist(artist_seg), function(x) 
    length(gregexpr(x, gsub(artist, "", comment, fixed = TRUE), fixed = TRUE)[[1]]))), by = id]
songs[, artist_sumN := sum(artist_N, artist_seg_N), by = id]

#+ warning=F, message=F, error=F, fig.width=13, fig.height=7, fig.showtext=T
g <- ggplot(songs[, .(N = sum(artist_sumN)), keyby = .(nation, artist)]) +
    geom_text(aes(seq_along(artist), N , label = artist, color = nation)) +
    labs(x = "artist", y = "times") +
    ggtitle("歌手在評論中被吶喊的總次數")
print(g)

#+ warning=F, message=F, error=F, fig.width=13, fig.height=7, fig.showtext=T
g <- ggplot(songs[, .(N = log(sum(artist_sumN)/ sum(cmmt_number))), keyby = .(nation, artist)]) +
    geom_text(aes(seq_along(artist), N , label = artist, color = nation)) +
    labs(x = "artist", y = "log(吶喊的機率)") +
    ggtitle("歌手在單一評論中被吶喊的機率")
print(g)

#' ## 排名與歌手在單一評論中被吶喊的機率相關性
cor.test(songs[-254, rank], songs[-254, artist_sumN / cmmt_number], method = "kendall")

#' ## 排名與歌手本身相關性
which(coef(summary(lm(rank ~ artist, data = songs)))[, 'Pr(>|t|)'] < 0.1)

summary(lm(artist_sumN ~ nation, data = songs))

#' ### 小結:由此看出，日本歌手的評論中較少提及歌手本身(P < 0.1) ，其餘國籍則不顯著。




#' ## 情感分析：運用情緒字典，計算情感分數
songs[, `:=`(
    POS_score = sapply(cmmt_seg, function(x) sum(x %in% POS)),
    POS_lyric = sapply(lyric_seg, function(x) sum(x %in% POS)),
    NEG_score = sapply(cmmt_seg, function(x) sum(x %in% NEG)),
    NEG_lyric = sapply(lyric_seg, function(x) sum(x %in% NEG))
    )]


#' ## 排名與評論情緒相關性
cor.test(songs$rank, songs[, POS_score - NEG_score], method = "kendall")
cor.test(songs$POS_score, songs$NEG_score, method = "kendall") # postive emotion is significantly associated with negative emotion
cor.test(songs$rank, songs$NEG_score, method = "kendall")
cor.test(songs$rank, songs$POS_score, method = "kendall")

#' ## 排名與語種相關性
#+ warning=F, message=F, error=F, fig.width=7, fig.height=7, fig.showtext=T
model <- aov(rank ~ lang, data = songs)
summary(model)
TukeyHSD(model)
plot(TukeyHSD(model))

#+ warning=F, message=F, error=F, fig.width=13, fig.height=7, fig.showtext=T
g <- ggplot(songs) + geom_boxplot(aes(lang, rank, fill = lang)) + coord_flip()
print(g)

#' ## 排名與國籍相關性
#+ warning=F, message=F, error=F, fig.width=7, fig.height=7, fig.showtext=T
model <- aov(rank ~ nation, data = songs)
summary(model)
TukeyHSD(model)
plot(TukeyHSD(model))

#+ warning=F, message=F, error=F, fig.width=13, fig.height=7, fig.showtext=T
g <- ggplot(songs) + geom_boxplot(aes(nation, rank, fill = nation)) + coord_flip()
print(g)


#' ## 排名與影劇相關性
#+ warning=F
songs[, `:=`(
        kr_score = sapply(cmmt_seg, function(x) sum(x %in% TV_kr)),
        jp_score = sapply(cmmt_seg, function(x) sum(x %in% TV_jp))
    )]

cor.test(songs$kr_score, songs$jp_score, method = "kendall")
cor.test(songs$rank, songs$kr_score, method = "kendall")
cor.test(songs$rank, songs$jp_score, method = "kendall")


#' ## 檢查
#' ### 評論出現韓劇次數 > 20 的 歌手與歌曲
#+ echo=F
knitr::kable(songs[kr_score >= 20, .(name, artist)])

#' ### 評論出現日劇次數 > 20 的 歌手與歌曲

#+ echo=F
knitr::kable(songs[jp_score >= 20, .(name, artist)])

#' 檢查後發現，因為有些日韓影劇名稱太過大眾化，容易出現在網友的留言中，以至於有
#' 不少提到影劇名稱多次的歌曲，與這些影劇沒有直接關係．是本研究的限制之一。這也
#' 是為何我們後面要把歌曲根據語言或國籍做分組，目的是要在分析時，盡量排除那些與
#' 日韓影劇沒有直接關係的歌曲樣本。   

#' 

#' ## 分語言分析排名與日韓影劇的相關性
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

#+ warning=F, message=F, error=F, fig.width=13, fig.height=7, fig.showtext=T
g <- ggplot(songs_jp) + 
    geom_boxplot(aes(rank_two, jp_score, group = rank_two)) +
    xlab("排名") +
    ylab("回覆提到戲劇名稱的次數") +
    coord_flip()
print(g)

#' ## 分語言分析排名與情緒的相關性(加入中文英文歌的分群)
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

#' ## Logistic regression for Janpanese language songs (top30 or not) 
#+ warning=F
fit <- glm(rank_two ~ POS_score + NEG_score + jp_score, data = songs_jp, family = binomial)
summary(fit)

#' ## Regression
#' ### 榜單名次 = 歌手ｘ評論數ｘ情緒分數ｘ日韓影劇　回歸分析
fit <- lm(rank ~ artist + cmmt_number + POS_score + NEG_score + jp_score, data = songs_jp)
summary(fit)
fit <- lm(rank ~ artist + cmmt_number + POS_score + NEG_score + kr_score, data = songs_kr)
summary(fit)

#' ### 小結:情緒對不同語言歌曲的排名都沒有顯著影響.但是否和日劇類相關,則會顯著影響日文歌曲進入前30名的機率(p<0.1)

#' ## 分國籍分析排名與日韓影劇的相關性
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


#' ## 分國籍分析排名與情緒的相關性(加入中文英文歌手的分群)
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

#' ## Regression
#' ### 榜單名次 = 歌手ｘ評論數ｘ情緒分數ｘ日韓影劇　回歸分析
fit <- lm(rank ~ artist + cmmt_number + POS_score + NEG_score + jp_score, data = songs_jp)
summary(fit)
fit <- lm(rank ~ artist + cmmt_number + POS_score + NEG_score + kr_score, data = songs_kr)
summary(fit)
