from selenium import webdriver
from time import sleep

options = webdriver.ChromeOptions()

#options.binary_location = 'chromium'

domain_block_path = "~/.config/chromium/Default/Extensions/ggdcjplapccgoinblmidpkoocfafajfa/1.6.2_0"
ublock_origin_path = "~/.config/chromium/Default/Extensions/cjpalhdlnbpafiamejdnhcphjbkeiagm/1.18.10_0"
options.add_argument(f'load-extension={domain_block_path},{ublock_origin_path}')

# options.add_argument('headless')

driver = webdriver.Chrome(options=options)

sleep(2)
driver.get('http://www.audiklub.org/dane/a6/c6/color/100.html')
# driver.implicitly_wait(10000)
sleep(10)
driver.quit()

