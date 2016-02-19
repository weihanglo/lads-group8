# -*- coding: utf-8 -*-
import argparse
import re
import requests
from bs4 import BeautifulSoup

url = 'https://www.google.com/search?'

media = [
    'udn.com',
    'ltn.com',
    'chinatimes.com',
    'appledaily.com',
    'strom.mg',
    'newtalk.tw'
]

def get_tags(url, params):
    tag_a = []
    tag_span = []
    while params['start'] // params['num'] < 1:
        r = requests.get(url, params)
        if not r.ok:
            raise ConnectionError('\n\n\tError {}.'.format(r.status_code))
        s = BeautifulSoup(r.text, 'lxml')
        tag_a.extend(s.select('h3 > a'))
        tag_span.extend(s.select('div.slp > span.f'))
        params['start'] += 100
        if params['start'] % 100 != 0:
            break
    return tag_a, tag_span


def get_links(tag_a, tag_span):
    p1 = re.compile(r'/url\?q=(https?://.*?)(&\S+=\S+)+')
    p2 = re.compile(r'(.*) - (.*)')
    p3 = re.compile(re.sub(r'\.', '\.', '|'.join(media)))
    
    ln = [p1.match(tag.get('href')).group(1) for tag in tag_a]
    check = [p3.search(url) is not None for url in ln]

    ln = [l for c, l in zip(check, ln) if c]
    title = [a.text for c, a  in zip(check ,tag_a) if c]

    span = [span for c, span in zip(check, tag_span) if c]
    source = [p2.match(tag.text).group(1) for tag in span]
    time = [p2.match(tag.text).group(2) for tag in span]
    
    return title, ln, source, time


if __name__ == '__main__':
    parser = argparse.ArgumentParser()
    parser.add_argument('query', help='Text to query.')
    parser.add_argument('range', help='Time range of query (%%m/%%d/%%Y-%%m/%%d/%%Y).')
    parser.add_argument('limit', help='Maximum results of query', type=int)
    parser.add_argument('-o', help='Output file (csv). Default: stdout')
    parser.add_argument('-s', help='Offset of query. Default: 0', type=int, default=0)
    parser.add_argument('-c', help='Country of query. Default: TW', default='TW')
    args = parser.parse_args()

    timerange = args.range.split('-')
    params = {
        'q': args.query,
        'tbs' : 'cd_min:{},cd_max:{}'.format(timerange[0], timerange[1]),
        'num': args.limit,
        'cr' : args.c,
        'start': args.s,
        'tbm': 'nws',
    }

    tag_a, tag_span = get_tags(url, params)
    title, ln, source, time = get_links(tag_a, tag_span)

    for h, l, s, t in zip(title, ln, source, time):
        print("{},{},{},{}".format(h, l, s, t))
