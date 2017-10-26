# for janet project 
from selenium import webdriver
from selenium.webdriver.common.by import By
from selenium.webdriver.support.ui import WebDriverWait
from selenium.webdriver.support import expected_conditions as EC
import time
import csv
from selenium.webdriver.chrome.options import Options



chrome_options = webdriver.ChromeOptions()
chrome_options.add_argument("--disable-infobars")
driver = webdriver.Chrome(chrome_options=chrome_options)

driver.get("https://www.producthunt.com/")

csv_file = open('website.csv', 'w')

writer = csv.writer(csv_file)
writer.writerow(['url'])


website_set=set()
times = 1
w=0
while times<=2500:
#not in list(range(1,2500,100))	
	if times not in list(range(1,2500,500)):
		print("Scraping Page number " + str(times))
		print(w)
		times=times+1 
		lastHeight = driver.execute_script("return document.body.scrollHeight")
		print (lastHeight)
		driver.execute_script("window.scrollTo(0, document.body.scrollHeight);")
		time.sleep(1)
		newHeight = driver.execute_script("return document.body.scrollHeight")
		print (newHeight)
		if newHeight == lastHeight:

			continue
		lastHeight = newHeight
		time.sleep(2)
	else:

		try:
			print("Scraping Page number " + str(times))
			print(w)
			times=times+1 
			links=driver.find_elements_by_xpath('//div[@class="item_54fdd"]')

			for link in links:
				
				website_dict={}
				url='https://www.producthunt.com'+link.find_element_by_xpath('.//a[@class="link_523b9"]').get_attribute('href')


					
				if url in website_set:
					continue
				else:
					print(w)
					w=w+1
					website_set.add(url)
					website_dict['url']=url
					writer.writerow(website_dict.values())


			lastHeight = driver.execute_script("return document.body.scrollHeight")
			print (lastHeight)
			driver.execute_script("window.scrollTo(0, document.body.scrollHeight);")
			time.sleep(1)
			newHeight = driver.execute_script("return document.body.scrollHeight")
			print (newHeight)
			if newHeight == lastHeight:

				continue
			else:
				lastHeight = newHeight
				
						
		 
		 

			time.sleep(2)
			
		except Exception as e:
			print(e)
			csv_file.close()
			driver.close()
			break
					
		
		
		
		
