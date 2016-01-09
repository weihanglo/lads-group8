library(RSQLite)
library(jiebaR)
library(data.table)

# Read files
conn <- dbConnect(dbDriver("SQLite"), "music163-2.db")
songs <- dbReadTable(conn, "songs")
dbDisconnect(conn = conn)
rank <- fread("out.txt", col.names = c("name", "id", "year", "rank"))
#setDT(songs, key = "id")
songs <- merge(songs, rank, by = "id")

POS <- readLines("sentiment_dict/POS.txt")
NEG <- readLines("sentiment_dict/NEG.txt")
emoji <- sub("(.*)", "[\\2]", scan("emoji.txt", what = 'char'))

# Positive Negitive
SEG <- worker()
songs$cmmt_seg <- lapply(songs$comment, function(x) SEG <= x)
songs$POS_score <- sapply(songs$cmmt_seg, function(x) sum(x %in% POS))
songs$NEG_score <- sapply(songs$cmmt_seg, function(x) sum(x %in% NEG))
songs$lyric_seg <- lapply(songs$lyric, function(x) SEG <= x)
songs$POS_lyric <- sapply(songs$lyric_seg, function(x) sum(x %in% POS))
songs$NEG_lyric <- sapply(songs$lyric_seg, function(x) sum(x %in% NEG))


# Number of comment
songs$cmmt_number <- sapply(songs$comment, function(x) length(unlist(strsplit(x, "<|>", fixed = TRUE))), USE.NAMES = FALSE)

summary(lm(rank ~ artist + NEG_lyric + POS_score + NEG_lyric + POS_score, data = songs))

songs$rank50 <- ifelse(songs$rank <= 50, 1, 0)
