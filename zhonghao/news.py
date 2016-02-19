#!/usr/bin/env python3
# -*- coding: utf-8 -*-
import queue
import random
import re
import requests
import sqlite3
import time

from datetime import date, datetime, timedelta
from threading import Thread
from pyquery import PyQuery as pq

source = {
    '蘋果': 'http://www.appledaily.com.tw/appledaily/archive/',
    '自由': 'http://news.ltn.com.tw/list/politics/',
    '中時': 'http://www.chinatimes.com/history-by-date/',
    '民報': 'http://www.peoplenews.tw/resource/lists/NEWS/政治',
    '風傳媒': 'http://www.storm.mg/category/118/',
    '新頭殼': 'http://newtalk.tw/news/tag/政治/', # http://newtalk.tw/news/category/1/政治經濟/
}

schema = """CREATE TABLE IF NOT EXISTS news (
    url TINYTEXT(255) NOT NULL,
    source TINYTEXT(10) NOT NULL,
    title TINYTEXT(100),
    time DATETIME,
    txt TEXT(65535),
    PRIMARY KEY (url))"""

sleep = lambda x: time.sleep(abs(random.gauss(x, 4)) % 18)

# database -----------------------------
class database:
    """Docstring for database. 
    """
    def __init__(self, db, new=False, schema=None):
        self.conn = sqlite3.connect(db, check_same_thread=False) 
        self.schema = schema
        self.cur = self.conn.cursor()
        if new:
            self.cur.execute(schema)
    def checkUnique(self, src, urls):
        self.cur.execute('SELECT url FROM news WHERE source = ?', (src, ))
        new_urls = list(set(urls) - set(self.cur.fetchall()))
        return new_urls
    def insertNews(self, data):
        self.cur.executemany('INSERT INTO news VALUES (?, ?, ?, ?, ?)', data)
        self.conn.commit()
        print('Inserted {} news from {}'.format(self.cur.rowcount, data[0][1]))


# getURL -------------------------------
def getURL(src, limit):
    """Docstring for getURL
    :src: TODO
    :limit: TODO
    """
    urls = []
    if src == '蘋果':
        today = date.today()
        base = 'http://www.appledaily.com.tw'
        while len(urls) <= limit:
            now = today.timetuple()
            try:
                r = requests.get("{}{}{:02d}{:02d}".format(source[src], *now))
                print('Trying to scrape {}'.format(r.url))
                d = pq(r.text)
                a_tag = d('article > h2:contains("政治") ~ ul > li > a')
                if not a_tag:
                    raise 
                newURL = [(base + a.attr('href'), ) for a in a_tag.items()] 
                if not newURL: 
                    break
                urls.extend(newURL)
            except:
                print('Failed to scrape {}'.format(r.url))
            today -= timedelta(days=1)
            sleep(7)
    if src == '自由':
        base = 'http://news.ltn.com.tw/'
        params = {'page': 1}
        while len(urls) < limit:
            try:
                r = requests.get(source[src], params=params)
                print('Trying to scrape {}'.format(r.url))
                d = pq(r.text)
                a_tag = d('#newslistul a')
                newURL = [(base + a.attr('href'), ) for a in a_tag.items()] 
                if not newURL: 
                    break
                urls.extend(newURL)
            except:
                print('Failed to scrape {}'.format(r.url))
            params['page'] += 1
            sleep(7)
    if src == '中時':
        today = date.today()
        base = 'http://www.chinatimes.com'
        while len(urls) <= limit:
            now = today.isoformat()
            params = {'page': 1}
            url = "{}{}-260407".format(source[src], now)
            while True:
                try:
                    r = requests.get(url, params=params)
                    print('Trying to scrape {}'.format(r.url))
                    d = pq(r.text)
                    a_tag = d('.listRight > ul > li > h2 > a')
                    newURL = [(base + a.attr('href'), ) for a in a_tag.items()] 
                    if not newURL: 
                        break
                    urls.extend(newURL)
                except:
                    print('Failed to scrape {}'.format(r.url))
                params['page'] += 1
                sleep(7)
            today -= timedelta(days=1)
    if src == '民報':
        base = 'http://www.peoplenews.tw/news/'
        data = dict(page=1, status=1)
        while len(urls) < limit:
            try:
                r = requests.post(source[src], data=data)
                print('Trying to scrape {} with data: {}'.format(r.url, data))
                r_json = r.json().get('data_list')
                newURL = [(base + j.get('EID'), ) for j in r_json] 
                if not newURL: 
                    break
                urls.extend(newURL)
            except:
                print('Failed to scrape {}'.format(r.url))
            data['page'] += 1
            sleep(7)
    if src == '風傳媒':
        base = 'http://www.storm.mg'
        page = 1
        while len(urls) < limit:
            try:
                r = requests.get(source[src] + str(page))
                print('Trying to scrape {}'.format(r.url))
                d = pq(r.text)
                a_tag = d('.main_content > p > a.gtm-article-list-news')
                newURL = [(base + a.attr('href'), ) for a in a_tag.items()] 
                if not newURL: 
                    break
                urls.extend(newURL)
            except:
                print('Failed to scrape {}'.format(r.url))
            page += 1
            sleep(7)
    if src == '新頭殼':
        page = 1
        while len(urls) < limit:
            try:
                r = requests.get(source[src] + str(page))
                print('Trying to scrape {}'.format(r.url))
                d = pq(r.content)
                a_tag = d('div.news-list-item div.news_title > a')
                newURL = [(a.attr('href'), ) for a in a_tag.items()] 
                if not newURL: 
                    break
                urls.extend(newURL)
            except:
                print('Failed to scrape {}'.format(r.url))
            page += 1
            sleep(7)
    return urls


# getContent ---------------------------
def getContent(src, urls):
    """Docstring for getContent
    :src: TODO
    :urls: TODO
    """
    content = []
    fmt = '%Y-%m-%d %H:%M'
    if src == '蘋果':
        for url in urls:
            url = url[0]
            try:
                r_news = requests.get(url)
                print('Trying to scrape news: {}'.format(url))
            except:
                print('Failed to scrape news: {}'.format(url))
                continue
            d_news = pq(r_news.text)
            title = d_news('hgroup > #h1').text()
            Time = d_news('.gggs time').attr('datetime')
            Time = datetime.strptime(Time, '%Y/%m/%d/').strftime(fmt)
            txt1 = d_news('.articulum p, .articulum h2').text()
            txt2 = d_news('.articulum').clone().children().remove().end().text()
            content.append((url, src, title, Time, txt1 + txt2))
            sleep(13)
    if src == '自由':
        p = re.compile('breakingnews')
        for url in urls:
            url = url[0]
            try:
                r_news = requests.get(url)
                print('Trying to scrape news: {}'.format(url))
            except:
                print('Failed to scrape news: {}'.format(url))
                continue
            d_news = pq(r_news.text)
            title = d_news('div.guide + h1').text()
            Time = d_news('#newstext span').text()
            _fmt = fmt if p.search(url) else '%Y-%m-%d'
            Time = datetime.strptime(Time, _fmt).strftime(fmt)
            txt = d_news('#newstext p').text()
            content.append((url, src, title, Time, txt))
            sleep(13)
    if src == '中時':
        for url in urls:
            url = url[0]
            try:
                r_news = requests.get(url)
                print('Trying to scrape news: {}'.format(url))
            except:
                print('Failed to scrape news: {}'.format(url))
                continue
            d_news = pq(r_news.text)
            title = d_news('header > h1').text()
            Time = d_news('time').text()
            Time = datetime.strptime(Time, '%Y年%m月%d日  %H:%M').strftime(fmt)
            txt = d_news('article > p').text()
            content.append((url, src, title, Time, txt))
            sleep(13)
    if src == '民報':
        for url in urls:
            url = url[0]
            try:
                r_news = requests.get(url)
                print('Trying to scrape news: {}'.format(url))
            except:
                print('Failed to scrape news: {}'.format(url))
                continue
            d_news = pq(r_news.text)
            news = d_news('h1.news_title, .date, div#newscontent')
            content.append((url, src) + tuple(n.text_content() for n in news))
            sleep(13)
    if src == '風傳媒':
        for url in urls:
            url = url[0]
            try:
                r_news = requests.get(url)
                print('Trying to scrape news: {}'.format(url))
            except:
                print('Failed to scrape news: {}'.format(url))
                continue
            d_news = pq(r_news.text)
            title = d_news('h1#article_title').text()
            Time = d_news('span.date').text()
            Time = datetime.strptime(Time, '%Y年%m月%d日  %H:%M').strftime(fmt)
            txt = d_news('article > p').text()
            content.append((url, src, title, Time, txt))
            sleep(13)
    if src == '新頭殼':
        p = re.compile(r'(\d+.*\d+:\d+)')
        for url in urls:
            url = url[0]
            try:
                r_news = requests.get(url)
                print('Trying to scrape news: {}'.format(url))
            except:
                print('Failed to scrape news: {}'.format(url))
                continue
            d_news = pq(r_news.content)
            title = d_news('.content_title').text()
            Time = p.search(d_news('.content_date').text()).group()
            Time = datetime.strptime(Time, '%Y.%m.%d | %H:%M').strftime(fmt)
            txt = d_news('txt').text()
            content.append((url, src, title, Time, txt))
            sleep(13)
    return content


def doJobs(*args):
    srcQ = args[0]
    limit = args[1]
    db = args[2]
    while srcQ.qsize():
        src = srcQ.get()
        urls = getURL(src, limit)
        urls = db.checkUnique(src, urls)
        while urls:
            data = getContent(src, urls[:20])
            db.insertNews(data)
            del urls[:20]

        
def main():
    limit = 10
    db = database('news.db', new=True, schema=schema)
    que = queue.Queue()
    for src in source.keys():
        que.put(src)
    thd1 = Thread(target=doJobs, name='Thd1', args=(que, limit, db))
    thd2 = Thread(target=doJobs, name='Thd2', args=(que, limit, db))
    thd3 = Thread(target=doJobs, name='Thd3', args=(que, limit, db))
    thd1.start()
    thd2.start()
    thd3.start()
    thd1.join()
    thd2.join()
    thd3.join()
    db.conn.close()

if __name__ == "__main__":
    main()
