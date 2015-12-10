# -*- coding: utf8 -*-
import csv
import re
import requests
from bs4 import BeautifulSoup

url = 'http://linguistlist.org/jobs/browse-jobs.cfm'
data = {
    'sortBy': 'ISSUEDATEPOSTED',
    'order': 'DESC',
    'current': 2,
    'startrow': 1,
}

f = open('jobs.csv', 'w')
w = csv.writer(f)

print("Scrape {} page...".format(data['startrow'] // 15 + 1))
r = requests.post(url, data=data)
s = BeautifulSoup(r.text, 'lxml')
limit = int(re.search('\d+', s.select_one('#content > h1').text).group())
table = s.select_one('span#jobs-count + table')
rows = table.select('tr')
for tr in rows:
    cols = tr.select('td')
    w.writerow([td.text for td in cols])

while data['startrow'] < limit:
    data['startrow'] += 15
    print("Scrape {} page...".format(data['startrow'] // 15 + 1))
    r = requests.post(url, data=data)
    s = BeautifulSoup(r.text, 'lxml')
    table = s.select_one('span#jobs-count + table')
    rows = table.select('tr')
    for tr in rows[1:]:
        cols = tr.select('td')
        w.writerow([td.text for td in cols])

f.close()
