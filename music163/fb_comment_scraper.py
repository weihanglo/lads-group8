#! /usr/bin/python3
# -*- coding: utf-8 -*-
"""
For scraping Facebook Comments Plugin on news websites in Taiwan
Third party module:
    chromedriver
    selenium
"""

import csv
import sys
from datetime import datetime
from chromedriver import CHROMEDRV_PATH
from selenium import webdriver
from selenium.webdriver.common.by import By
#from selenium.webdriver.common.keys import Keys
from selenium.webdriver.support.ui import WebDriverWait
from selenium.webdriver.support import expected_conditions as EC
from selenium.common.exceptions import TimeoutException

def init_driver():
    """Setup an initial webdriver"""

    driver = webdriver.Chrome(CHROMEDRV_PATH)
    driver.set_window_size(640, 480)
    driver.wait = WebDriverWait(driver, 5)
    return driver



def more_comments(driver, url):
    """Click all 'more comments' button on Facebook Comment Plugin"""
    
    driver.set_page_load_timeout(40)
    try:
        print('Open ' + url)
        driver.get(url)
        print('Get commments on ' + driver.title)
    except TimeoutException:
        driver.close()
        driver.quit()
        print('\nTimeout Error. Check your connection or url.\n\n\n')

    # To facebook comments plugin iframe
    music163 = driver.find_element(By.CSS_SELECTOR, "iframe.fb_ltr")
    driver.switch_to_frame(facebook)

    # Show more parenets comments
    while True:
        try:
            button = driver.wait.until(
                EC.element_to_be_clickable((By.TAG_NAME,'button'))
            )
            button.click()
            print('Click on a button...')
        except TimeoutException:
            print("Could not locate more parents comments button")
            break

    # Show more children comments
    while True:
        try:
            button = driver.wait.until(
                EC.element_to_be_clickable((By.CSS_SELECTOR,'._5yct a'))
            )
            button.click()
            print('Click on a button...')
        except TimeoutException:
            print("Could not locate more children comments button")
            break

    return driver



def output_comments(driver):
    """Write a csv file with all comments on Facebook Comments Plugin"""

    accounts = driver.find_elements(By.CLASS_NAME, 'UFICommentActorName')
    fb_links = [tag.get_attribute('href') for tag in accounts]
    accounts = [a.text for a in accounts]

    comments = driver.find_elements(By.CLASS_NAME, '_5mdd')
    comments = [c.text for c in comments]

    timestamp = driver.find_elements(By.CLASS_NAME, 'livetimestamp')
    timestamp = [tag.get_attribute('data-utime') for tag in timestamp]
    time_reply = [str(datetime.fromtimestamp(int(t))) for t in timestamp]

    fb_likes = driver.find_elements(By.CSS_SELECTOR, '._2vq9 > span:nth-child(5)')
    fb_likes = [l.text for l in fb_likes]
    for i in range(len(fb_likes)):
        try:
            fb_likes[i] = int(fb_likes[i])
        except:
            fb_likes[i] = 0

    # Back to default
    driver.switch_to_default_content()

    with open(driver.title + '.csv', 'w') as f:
        w = csv.writer(f)
        w.writerows(zip(accounts, time_reply, fb_likes, comments, fb_links))
    print('Save {}.csv with {} comments at {}'.format(driver.title, len(comments), str(datetime.now())))


if __name__ == "__main__":
    urls = sys.argv[1:]
    for url in urls:
        driver = init_driver()
        more_comments(driver, url)
        output_comments(driver)
        driver.close()
        driver.quit()

