# -*- coding: utf-8 -*-
"""
Reference: 
    https://github.com/darknessomi/musicbox/wiki/网易云音乐新登录API分析
    https://github.com/darknessomi/musicbox/blob/master/NEMbox/api.py
"""
import os
import json
import codecs
import base64
import binascii
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


text = {
    'username': '',
    'password': '',
    'rememberLogin': '',
}

modulus = '00e0b509f6259df8642dbc35662901477df22677ec152b5ff68ace615bb7b725152b3ab17a876aea8a5aa76d2e417629ec4ee341f56135fccf695280104e0312ecbda92557c93870114af6c9d05c4f7f0c3685b7a46bee255932575cce10b424d813cfe4875d3e82047b97ddef52741d546b8e289dc6935b3ece0462db0a22b8e7'
nonce = '0CoJUm6Qyw8W8jud'
pubKey = '010001'

text = json.dumps(text)
secKey = createSecretKey(16)
encText = aesEncrypt(aesEncrypt(text, nonce).decode('utf-8'), secKey)
encSecKey = rsaEncrypt(secKey, pubKey, modulus)
data = {
    'params': encText,
    'encSecKey': encSecKey
}

#url = 'http://music.163.com/weapi/song/lyric?id=65337&lv=-1&kv=-1&tv=-1'
#url = 'http://music.163.com/weapi/v1/resource/comments/R_SO_4_65337/?limit=99999&csrf_token='
url = 'http://music.163.com/weapi/playlist/list?order=hot&cat=感动&limit=99999'
headers = {
    'Cookie': 'appver=1.5.0.75771;',
    'Referer': 'http://music.163.com/song?id=65337'
}

import requests
requests.post(url, headers=headers, data=data).text
