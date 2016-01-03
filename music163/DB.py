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

def dumpToDB(songs, conn):
    cur = conn.cursor()
    count_s = 0
    for s in songs:
        try:
            print("Song '{}' dumps into database".format(s[0]))
            cur.execute("""INSERT INTO songs (id, name, artist, album, duration, lyric, comment)
                VALUES (?, ?, ?, ?, ?, ?, ?)""", s)
            count_s += 1
            conn.commit()
        except sqlite3.IntegrityError as IntErr:
            print("{err}\n  Duplicated ID: '{ID}'".format(err=IntErr, ID=s[0]))
    print("\n{} new songs added".format(count_s))


def main():
    conn = sqlite3.connect('music163-2.db')
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
    cur.close()

    listID = ['148858141', '148822545', '148821771', '148822543', 
        '148813956', '148818810']

    baseURL = 'http://music.163.com/playlist?id='
    for ID in listID:
        with open('./logfile-2/{}.log'.format(ID), 'w') as f:
            sys.stdout = f
            sys.stderr = f
            url = baseURL + ID
            songs = Scrape.getSongDetail(url)
            dumpToDB(songs, conn)
            sleep(abs(random.gauss(6, 2.5)) % 15)
    conn.close()

if __name__ == '__main__':
    main()
