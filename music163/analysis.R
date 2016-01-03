library(RSQLite)
library(data.table)

emoji <- sub("(.*)", "[\\1]", scan("emoji.txt", what = 'char'))

conn <- dbConnect(dbDriver("SQLite"), "music163.db")
songs <- dbReadTable(conn, "songs")
tags <- dbReadTable(conn, "tags")
dbDisconnect(conn = conn)
setDT(songs, key = "id")
setDT(tags, key = "song_id")

CHid <- tags[tags$tag == "华语", song_id]
emotion <- tags[tags$tag %in% c("快乐", "伤感")]
CHsongs <- merge(songs[id %in% CHid], emotion, by.x = "id", by.y = "song_id")
CHsongs$id[which(duplicated(CHsongs$id))]

tag_cmmtLen <- CHsongs[, .(lapply(comment, function(c) nchar(strsplit(c, "<|>", fixed = TRUE)[[1]]))), by = tag]

# Document Emoji Matrix
DEM <- sapply(emoji, function(e) {
    print(e)
    sapply(gregexpr(e, CHsongs$comment), function(c) sum(ifelse(c != -1, 1, 0)))
})
as.data.table(DEM)[, lapply(.SD, sum), by = .(tag = CHsongs$tag)]

i <- sample(1:nrow(CHsongs), 1); strsplit(CHsongs$comment[i], "<|>", fixed = TRUE); CHsongs[i, .(tag, name, id, artist)]
