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
import requests

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


def getSong(ID, text):
    text = json.dumps(text)
    secKey = createSecretKey(16)
    encText = aesEncrypt(aesEncrypt(text, nonce).decode('utf-8'), secKey)
    encSecKey = rsaEncrypt(secKey, pubKey, modulus)
    data = {
        'params': encText,
        'encSecKey': encSecKey
    }
    headers = {
        'User-Agent': 'Mozilla/5.0',
        'Cookie': 'appver=1.5.0.75771;',
        'Referer': 'http://music.163.com/song?id='.format(ID)
    }
    
    url_comments = 'http://music.163.com/weapi/v1/resource/comments/R_SO_4_{}/?&limit=99999&csrf_token='.format(ID)
    url_lyrics= 'http://music.163.com/weapi/song/lyric?id={}&lv=-1&kv=-1&tv=-1'.format(ID)
    print(requests.post(url_comments, data=data, headers=headers).text)
    print(requests.post(url_lyrics, data=data, headers=headers).text)

def getPlaylist(url, database=False):
    """
    """
    r = requests.get(url)
    soup = BeautifulSoup(r.text)
    songlist = soup.select('ul.f-hide > li > a')
    songs = [a.text for a in songlist]
    links = [a.get('href') for a in songlist]
    
    return dict(zip(songs, links))


if __name__ == "__main__":
    p = re.compile(r'id=(\d+)')
    text = {
        'username': '',
        'password': '',
        'rememberLogin': ''
    }
    modulus = '00e0b509f6259df8642dbc35662901477df22677ec152b5ff68ace615bb7b725152b3ab17a876aea8a5aa76d2e417629ec4ee341f56135fccf695280104e0312ecbda92557c93870114af6c9d05c4f7f0c3685b7a46bee255932575cce10b424d813cfe4875d3e82047b97ddef52741d546b8e289dc6935b3ece0462db0a22b8e7'
    nonce = '0CoJUm6Qyw8W8jud'
    pubKey = '010001'
    urls = sys.argv[1:]

    for url in urls:
        playlist = getPlaylist(url)
        for ID in playlist.value():
            getSong(ID, text)
