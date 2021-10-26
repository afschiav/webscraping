import csv #import csv reader
import os
from selenium import webdriver #import webdriver
from selenium.webdriver.common.by import By
from selenium.webdriver.support.ui import WebDriverWait
from selenium.webdriver.support import expected_conditions as EC
from selenium.common.exceptions import NoSuchElementException
from os import listdir
from os.path import isfile, join


path='C:/Users/Ansel/Box Sync/Documents/Structural Change/Job market paper/Input data/State zips'
state_files = [f for f in listdir(path) if isfile(join(path, f))]

#establish driver:
driver=webdriver.Chrome(executable_path='C:/webdrivers/chromedriver.exe')

#retrieve url:
driver.get('https://www.electricforall.org/electric-utility/')

#select search bar:
searchbox= driver.find_element_by_xpath('//*[@id="zipcode-input"]')

for file in state_files:
    file_ext = 'State zips/'+file
    with open(file_ext, 'r') as csv_file:
        csv_reader = csv.reader(csv_file, dialect='excel') #attach csv file
        utility_list = []
        
        for line in csv_reader: #for each line in csv file...

            #delete automatic input:
            searchbox.clear()

            #input search key (zip code):
            searchbox.send_keys(line[1])

            #select search button:
            searchButton=driver.find_element_by_xpath('//*[@id="zipcode-submit"]')

            #click search
            searchButton.click()

            try: #wait for result
                element = WebDriverWait(driver, 10).until(
                    EC.presence_of_element_located((By.ID, 'incentives-results'))
                )
            finally:
                #obtain result:
                try:
                    result=driver.find_element_by_xpath('//*[@id="incentives-results"]/div[2]/div[1]')
                    #append result:
                    utility_list.append((line[1],line[7],line[8],line[15],result.text))
                except NoSuchElementException: #handle case where there is no result
                    pass
                    utility_list.append((line[1],line[7],line[8],line[15],'NA'))


    #write to csv file:
    output_file=file_ext+'_utility_data.csv'
    with open(output_file,'w') as out:
            csv_out=csv.writer(out, lineterminator='\n')
            csv_out.writerows(utility_list)
        
