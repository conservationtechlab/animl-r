from selenium import webdriver
from selenium.webdriver.common.keys import Keys
from selenium.webdriver.support.ui import WebDriverWait
from selenium.webdriver.common.by import By
from selenium.webdriver.support import expected_conditions as EC
import time

options = webdriver.ChromeOptions()
options.add_argument("--enable-javascript")
driver = webdriver.Chrome(options=options)
driver.get("https://www.strikeforcewireless.com/login?redirect=%2Fcameras")
name= driver.find_element_by_id("username")
name.clear()
name.send_keys()
pw = driver.find_element_by_id("password")
pw.send_keys()
pw.send_keys(Keys.RETURN)

#wait to login
time.sleep(2)

#navigate to photos page
driver.get("https://www.strikeforcewireless.com/photos")

#wait for javascript to load
time.sleep(2)

images = driver.find_elements_by_tag_name('img')
for image in images:
    print(image.get_attribute('src'))
driver.close()

