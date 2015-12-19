# -*- coding: utf-8 -*-
"""
Reference: 
    https://github.com/darknessomi/musicbox/wiki/网易云音乐新登录API分析
    https://github.com/darknessomi/musicbox/blob/master/NEMbox/api.py
"""

import base64
import binascii
import codecs
import json
import os
import re
import random
import requests
import time

from operator import itemgetter
from bs4 import BeautifulSoup
from Crypto.Cipher import AES


def aesEncrypt(text, secKey):
    pad = 16 - len(text) % 16
    text = text + pad * chr(pad)
    encryptor = AES.new(secKey, 2, '0102030405060708')
    ciphertext = encryptor.encrypt(text)
    ciphertext = base64.b64encode(ciphertext)
    return ciphertext


def rsaEncrypt(text, pubKey, modulus):
    text = text[::-1]
    rs = int(binascii.hexlify(text.encode('utf-8')), 16)**int(pubKey, 16) % int(modulus, 16)
    return format(rs, 'x').zfill(256)


def createSecretKey(size):
    return (''.join(map(lambda xx: hex(xx)[2:], os.urandom(size))))[0:16]


def getLyricComment(ID, text):
    headers = {
        'Cookie': 'appver=1.5.0.75771;',
        'Host': 'music.163.com',
        'User-Agent': 'Mozilla/5.0 (X11; Fedora; Linux x86_64; rv:42.0) Gecko/20100101 Firefox/42.0',
        'Referer': 'http://music.163.com/song?id={}'.format(ID),
        'Connection': 'keep-alive'
    }
    url_comments = 'http://music.163.com/weapi/v1/resource/comments/R_SO_4_{}/?&limit=99999&csrf_token='.format(ID)
    url_lyric= 'http://music.163.com/weapi/song/lyric?id={}&lv=-1&kv=-1&tv=-1'.format(ID)
    try:
        print("Scraping comments of song {}".format(ID))
        raw_comments = requests.post(url_comments, data=data, headers=headers).text
        comments = [(comment['content'], comment['likedCount']) for comment in json.loads(raw_comments)['comments']]
        comments.sort(key=itemgetter(1), reverse=True)
        top100comments = '<|>'.join([c[0] for c in comments[0:100]])
    except:
        print("Failed to scrape comments of song '{}'".format(ID))
        top100comments = None
    try:
        print("Scraping lyric of song '{}'".format(ID))
        raw_lyric = requests.post(url_lyric, data=data, headers=headers).text
        lyric = json.loads(raw_lyric)['lrc']['lyric']
    except KeyError:
        lyric = None
    except:
        print("Failed to scrape lyric of song '{}'".format(ID))
        lyric = None
    time.sleep(abs(random.gauss(10, 5)))
    return (lyric, top100comments)


def getSongDetail(url):
    """
    Get all songs with details in a playlist
    """
    r = requests.get(url)
    soup = BeautifulSoup(r.text, 'lxml')
    songlist = json.loads(soup.select_one('textarea').text)
    tags = [s.string for s in soup.select('a.u-tag > i')]
    songDetails = [(
        s['id'], 
        s['name'], 
        s['artists'][0]['name'], 
        s['album']['name'], 
        s['duration']) + getLyricComment(s['id'], text) for s in songlist]
    return {'songs': songDetails, 'tags': tags}


text = {
    'username': '',
    'password': '',
    'rememberLogin': ''
}
text = json.dumps(text)
modulus = '00e0b509f6259df8642dbc35662901477df22677ec152b5ff68ace615bb7b725152b3ab17a876aea8a5aa76d2e417629ec4ee341f56135fccf695280104e0312ecbda92557c93870114af6c9d05c4f7f0c3685b7a46bee255932575cce10b424d813cfe4875d3e82047b97ddef52741d546b8e289dc6935b3ece0462db0a22b8e7'
nonce = '0CoJUm6Qyw8W8jud'
pubKey = '010001'
secKey = createSecretKey(16)
encText = aesEncrypt(aesEncrypt(text, nonce).decode('utf-8'), secKey)
encSecKey = rsaEncrypt(secKey, pubKey, modulus)
data = {
    'params': encText,
    'encSecKey': encSecKey
}
