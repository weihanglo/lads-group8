# -*- coding: utf-8 -*-
import sqlite3
import sys
import Scrape

def dumpToDB(data):
    count_s = 0
    count_t = 0
    for s in songs.get('songs'):
        try:
            print("Song '{}' dumps into database".format(s[0]))
            cur.execute("""INSERT INTO songs (id, name, artist, album, duration, lyric, comment)
                VALUES (?, ?, ?, ?, ?, ?, ?)""", s)
            new_tags = {[(s[0], t) for t in songs['tags']]}
            cur.executemany("INSERT INTO tags VALUES (?, ?)", new_tags)
            count_s += 1
        except sqlite3.IntegrityError as IntErr:
            print("{err}\n  Duplicated ID: '{ID}'".format(err=IntErr, ID=s[0]))
            print("Trying to updat tags of song '{}'".format(s[0]))
            old_tags = set(cur.execute("SELECT * FROM tags WHERE song_id = ?", (s[0], )))
            new_tags = set([(s[0], t) for t in songs['tags']])
            new_tags.difference_update(old_tags)
            cur.executemany("INSERT INTO tags VALUES (?, ?)", new_tags)
            count_t += 1
    print("\n{} new songs added\n{} tags of songs updated".format(count_s, count_t))

if __name__ == '__main__':
    conn = sqlite3.connect('music163.db')
    cur = conn.cursor()

#    cur.execute("""CREATE TABLE IF NOT EXISTS songs (
#        id INTEGER PRIMARY KEY, 
#        name TINYTEXT,
#        artist TINYTEXT,
#        album TINYTEXT,
#        duration INTEGER,
#        lyric TEXT(3000),
#        comment MEDIUMTEXT,
#        modified_time TIMESTAMP DEFAULT CURRENT_TIMESTAMP)""")
#    cur.execute("""CREATE TABLE IF NOT EXISTS tags (
#        song_id INTEGER,
#        tag TINYTEXT,
#        FOREIGN KEY (song_id) REFERENCES songs(id))""")
#    cur.execute("PRAGMA foreign_keys = 1")

    for url in sys.argv[1:]:
        songs = Scrape.getSongDetail(url)
        dumpToDB(songs)

    conn.commit()
    conn.close()
