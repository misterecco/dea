import argparse

from selenium import webdriver
from time import sleep


domain_block_path = "~/.config/chromium/Default/Extensions/ggdcjplapccgoinblmidpkoocfafajfa/1.6.2_1"
ublock_origin_path = "~/.config/chromium/Default/Extensions/cjpalhdlnbpafiamejdnhcphjbkeiagm/1.18.10_0"
chrome_path = "/home/tomek/msc/chromium/src/out/Opt/chrome"
chromedriver_path = "/home/tomek/msc/chromedriver/chromedriver"

def open_browser_with_extensions(extensions=[], trace_file=None):
    options = webdriver.ChromeOptions()
    options.binary_location = chrome_path
    # options.add_argument('headless')
    options.add_argument('--no-sandbox')

    if len(extensions) > 0:
        arg = f"--load-extension={extensions[0]}"
        for e in extensions[1:]:
            arg += f",{e}"
        options.add_argument(arg)

    if trace_file is not None:
        # arg = f"--js-flags='--trace-dea --trace-dea-file={trace_file}'"
        arg = f"--js-flags='--trace-dea'"
        print(f"trace arg: {arg}")
        options.add_argument(arg)

    return webdriver.Chrome(options=options, executable_path=chromedriver_path,
                            service_args=["--verbose", "--log-path=/home/tomek/msc/dea/automation/cd.log"])

def open_browser_with_ublock(trace_file=None):
    return open_browser_with_extensions(extensions=[ublock_origin_path], trace_file=trace_file)

def open_browser_without_ublock(trace_file=None):
    return open_browser_with_extensions(extensions=[], trace_file=trace_file)

def configure_ublock(browser):
    sleep(0.5)
    browser.get('chrome-extension://cjpalhdlnbpafiamejdnhcphjbkeiagm/dashboard.html#3p-filters.html')
    sleep(0.5)
    browser.switch_to.frame('iframe')
    browser.find_element_by_xpath("//li[@data-listkey='POL-2']/input").click()
    sleep(0.5)
    browser.find_element_by_css_selector('#buttonApply').click()
    sleep(0.5)


def open_website_and_quit(browser, website):
    browser.get(website)
    # browser.implicitly_wait(10000)
    sleep(1000)
    browser.quit()

def open_website_with_ublock(website, trace_file=None):
    browser = open_browser_with_ublock(trace_file=trace_file)
    configure_ublock(browser)
    open_website_and_quit(browser, website)

def open_website_without_ublock(website, trace_file=None):
    browser = open_browser_without_ublock(trace_file=trace_file)
    open_website_and_quit(browser, website)

# open_website_with_ublock('http://www.audiklub.org/dane/a6/c6/color/100.html')
# open_website_without_ublock('http://www.audiklub.org/dane/a6/c6/color/100.html')

if __name__ == "__main__":
    parser = argparse.ArgumentParser()
    parser.add_argument('website', help="Website to open")
    parser.add_argument('--ublock', dest='ublock', help="Turn on the uBlock extension",
                        default=False, action='store_true')
    parser.add_argument('--trace-file', help="Where to save the trace",
                        default=None)
    args = parser.parse_args()

    if args.ublock:
        open_website_with_ublock(args.website, trace_file=args.trace_file)
    else:
        open_website_without_ublock(args.website, trace_file=args.trace_file)

