#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Mon Jun  7 10:41:04 2021

@author: rintukutum
"""
# conda create --name py39-webscrapper python=3.9
##---------- INSTALL ESSENTIALS
# conda activate py39-webscrapper
# conda install -c anaconda requests
# conda install -c conda-forge selenium
# conda install -c anaconda beautifulsoup4
# python 3.9
#---------
##---------- START THE WORK
# conda activate py39-webscrapper
# ESSENTIAL MODULES
import requests
from selenium import webdriver
from selenium.webdriver.common.keys import Keys
# auto login
from selenium.webdriver.support.ui import WebDriverWait
from selenium.webdriver.support import expected_conditions as cond

# SEARCH
from selenium.webdriver.common.by import By

import time


url = "https://platform.gisaid.org/epi3/frontend"
#---------
# CHANGE DRIVER ACCORDINGLY
driver = webdriver.Safari()
driver.get(url)

#----- LOGIN PAGE
time.sleep(5)
driver.maximize_window()
time.sleep(1)
driver.find_element_by_id("elogin").send_keys('rintu.kutum')
driver.find_element_by_id("epassword").send_keys('gp41@GISAID')

#login_box = driver.find_element_by_class_name("form_button_submit")
driver.execute_script("document.getElementById('sys_curtain').remove()")
driver.execute_script("document.getElementsByClassName('form_button_submit')[0].click()")
time.sleep(5)
# WebDriverWait(driver, 30).until(cond.staleness_of(login_box))

#----- GO TO SEARCH
#<div class="sys-actionbar-action" onclick="sys.getC('c_qudqdk_om').onclick('ce_qudqdk_9h', '',
# 'page_corona2020.Corona2020BrowsePage', false)" style="float: left;">
#<img align="absmiddle" border="0"
#src="/epi3/app_entities/entities/icons/24x24/data_find.png"
# style="margin-right:5px"/>Search</div>
#<div class="sys-actionbar-action" onclick="sys.getC('c_qudqdk_om').onclick('ce_qudqdk_9i', '',
# 'page_corona2020.PartnerDownloadsPage', false)" style="float: left;">

# search_box = driver.find_element_by_class_name("sys-actionbar-action")
#WebDriverWait(driver, 30).until(cond.element_to_be_clickable(
#    (By.XPATH, "//img[@src='/epi3/app_entities/entities/icons/24x24/data_find.png']"))
#    ).click()

#driver.execute_script("document.getElementById('sys_curtain').remove()")
#driver.execute_script("document.getElementsByClassName('form_button_submit')[0].click()")
#driver.find_elements_by_xpath("//*[contains(text(), 'Search')]")[0].click()

driver.execute_script("document.getElementById('sys_curtain').remove()")
driver.execute_script("document.getElementsByClassName('sys-actionbar-action')[1].click()")
time.sleep(5)
# doesn't WORK
# driver.find_elements_by_class_name("sys-actionbar-action")[1].click()


#--- Find the index for complete, high coverage, low coverage, w/Patient status
#    collection date compl
sys_event_hooks = driver.find_elements_by_class_name("sys-event-hook")
values = []
index = []
idx = 0
for seh in sys_event_hooks:
    val = seh.get_attribute('value')
    if len(val) != 0:
        values.append(val)
        index.append(idx)
    idx = idx + 1

seh_dict = {}
for i in range(len(values)):
    seh_dict[values[i]] = index[i]

#seh_dict
#{'complete': 4, 'highq': 5, 'lowco': 6, 'pstat': 7, 'coldc': 12}
driver.execute_script("document.getElementsByClassName('sys-event-hook')[4].click()")
driver.execute_script("document.getElementsByClassName('sys-event-hook')[5].click()")
driver.execute_script("document.getElementsByClassName('sys-event-hook')[7].click()")
# collection date completion
driver.execute_script("document.getElementsByClassName('sys-event-hook')[12].click()")
time.sleep(3)
#----- Submission
# id = ce_qudqdk_a3_input, YEAR-MONTH-DATE
# START
#from selenium.webdriver.common.action_chains import ActionChains

#----GET THE SUBMISSION IDS
sys_forms = driver.find_elements_by_class_name("sys-event-hook.sys-fi-mark")
date_ids = []

for sf in sys_forms:
    id = sf.get_attribute('id')
    if "input" in id:
        date_ids.append(id)

collection_ids = date_ids[:2]
submission_ids = date_ids[2:]

from calendar import monthrange
year = 2021
month = 2
lday = monthrange(year, month)[1]
import datetime
start_month = datetime.datetime(year, month, 1).strftime('%Y-%m-%d')
end_month = datetime.datetime(year, month, lday).strftime('%Y-%m-%d')

driver.find_element_by_id(submission_ids[0]).send_keys(start_month)
time.sleep(3)
#start_date = driver.find_element_by_id("ce_qudqdk_a3_input")
#ActionChains(driver).move_to_element(start_date).click().send_keys('2021-05-01').perform()
# END
driver.find_element_by_id(submission_ids[1]).send_keys(end_month)
time.sleep(3)
driver.switch_to.default_content()
time.sleep(3)

def get_info(id):
    iframe_link = "document.getElementById('" + id + "').click()"
    driver.execute_script(iframe_link)
    time.sleep(3)
    iframe = driver.find_elements_by_tag_name("iframe")[0]
    driver.switch_to.frame(iframe)
    time.sleep(2)
    fasta = driver.find_elements_by_tag_name("pre")[0].text
    metadata = driver.find_elements_by_xpath("//b[contains(text(), 'Virus detail')]/../../following-sibling::tr")
    metacontent = process_metadata(metadata)
    # process metadata here only
    driver.execute_script("document.getElementsByTagName('button')[1].click()")
    time.sleep(1)
    driver.switch_to.default_content()
    info = {'metadata': metacontent,'fasta':fasta}
    return info

def process_metadata(meta):
    metatext = []
    for txt in meta:
        metatext.append(txt.text.strip().replace("\n",""))
    metaoutput = '\n'.join(metatext)
    return metaoutput

def get_last_page_no():
    driver.execute_script("document.getElementsByClassName('yui-pg-last')[0].click()")
    time.sleep(5)
    pages = driver.find_elements_by_class_name("yui-pg-page")
    pg_no = []
    for pg in pages:
        pg_no.append(pg.get_attribute('page'))
    driver.execute_script("document.getElementsByClassName('yui-pg-first')[0].click()")
    time.sleep(5)
    return int(pg_no[3])


pg_last = get_last_page_no()+1
cur_names = []
for curpg in range(pg_last):
    print("Page no. ",curpg+1)
    parent_form = driver.find_element_by_class_name("yui-dt-data")
    trs = parent_form.find_elements_by_tag_name("tr")
    for i in range(len(trs)):
        tr = trs[i]
        ID = tr.get_attribute('id')
        name = tr.find_elements_by_tag_name("td")[3].text.replace("/", "_")
        if name not in old_name:
            output = get_info(ID)
        cur_names.append(name)
    if curpg < pg_last:
        driver.execute_script("document.getElementsByClassName('yui-pg-next')[0].click()")
        time.sleep(3)


total = driver.find_elements_by_xpath("//*[contains(text(), 'Total:')]")[0].text
tn = int(total.split(' ')[1].replace(',',''))
if len(cur_names) == tn:
    print('Download completed\n')
