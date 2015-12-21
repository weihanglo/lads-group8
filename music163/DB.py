# -*- coding: utf-8 -*-
import os
import random
import re
import sqlite3
import sys
from time import sleep

import requests
from bs4 import BeautifulSoup

import Scrape

def dumpToDB(data):
    count_s = 0
    count_t = 0
    for s in songs.get('songs'):
        try:
            print("Song '{}' dumps into database".format(s[0]))
            cur.execute("""INSERT INTO songs (id, name, artist, album, duration, lyric, comment)
                VALUES (?, ?, ?, ?, ?, ?, ?)""", s)
            new_tags = set([(s[0], t) for t in songs['tags']])
            cur.executemany("INSERT INTO tags VALUES (?, ?)", new_tags)
            count_s += 1
        except sqlite3.IntegrityError as IntErr:
            print("{err}\n  Duplicated ID: '{ID}'".format(err=IntErr, ID=s[0]))
            print("Trying to update tags of song '{}'".format(s[0]))
            old_tags = set(cur.execute("SELECT * FROM tags WHERE song_id = ?", (s[0], )))
            new_tags = set([(s[0], t) for t in songs['tags']])
            new_tags.difference_update(old_tags)
            cur.executemany("INSERT INTO tags VALUES (?, ?)", new_tags)
            count_t += 1
    print("\n{} new songs added\n{} tags of songs updated".format(count_s, count_t))

if __name__ == '__main__':
    conn = sqlite3.connect('music163.db')
    cur = conn.cursor()

    cur.execute("""CREATE TABLE IF NOT EXISTS songs (
        id INTEGER PRIMARY KEY, 
        name TINYTEXT,
        artist TINYTEXT,
        album TINYTEXT,
        duration INTEGER,
        lyric TEXT(3000),
        comment MEDIUMTEXT,
        modified_time TIMESTAMP DEFAULT CURRENT_TIMESTAMP)""")
    cur.execute("""CREATE TABLE IF NOT EXISTS tags (
        song_id INTEGER,
        tag TINYTEXT,
        FOREIGN KEY (song_id) REFERENCES songs(id))""")
    cur.execute("PRAGMA foreign_keys = 1")

    cat = ['伤感', '快乐', '放松']
    discover = 'http://music.163.com/discover/playlist/?cat={}'.format(cat[1])
    soup = BeautifulSoup(requests.get(discover).text, 'lxml')
    p1 = re.compile(r'\d+$')
    listID = [p1.search(s.get('href')).group() for s in soup.select('p.dec > a')]

    p2 = re.compile(r'(\d+)\.log')
    logf = [f[:-4] for f in os.listdir('./logfile') if p2.search(f) is not None]

    baseURL = 'http://music.163.com/playlist?id='
    for ID in set(listID).difference(logf):
        with open('./logfile/{}.log'.format(ID), 'w') as f:
            sys.stdout = f
            sys.stderr = f
            url = baseURL + ID
            songs = Scrape.getSongDetail(url)
            dumpToDB(songs)
            sleep(random.gauss(20, 5))

    conn.commit()
    conn.close()
