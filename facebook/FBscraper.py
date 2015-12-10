# -*- coding: utf-8 -*-
import csv
import sys
import re
import requests

def fb_comments(href):
    """For scraping Facebook Comments Plugin websites in Taiwan
    href : URL that has the facebook comments plugin

    third party module:
        requests

    """
    url = 'https://graph.facebook.com/comments/?ids={}'.format(href)
    data = requests.get(url).json().get(href).get('comments').get('data')
    p = re.compile('<title>(.*?)</title>')
    title = p.search(requests.get(href).content.decode('utf8')).group(1)
    name = [c.get('from').get('name') for c in data]
    time = [c.get('created_time') for c in data]
    like = [c.get('like_count') for c in data]
    text = [c.get('message') for c in data]

    with open('{}.csv'.format(title), 'w+') as f:
        w = csv.writer(f)
        w.writerow(['name', 'time', 'like', 'message'])
        w.writerows(zip(name, time, like, text))


if __name__ == '__main__':
    urls = sys.argv[1:]
    for url in urls:
        print("Scraping", url)
        fb_comments(url)
