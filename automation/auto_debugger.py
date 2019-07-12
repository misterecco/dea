import argparse
import os
import socket
import subprocess
import random

from selenium import webdriver
from selenium.webdriver.common.desired_capabilities import DesiredCapabilities
from time import sleep


chrome_path = "/home/tomek/msc/chromium/src/out/Opt/chrome"
chromedriver_path = "/home/tomek/msc/chromedriver/chromedriver"
profile_ublock = "/home/tomek/msc/dea/automation/profile_ublock"
profile_no_ublock = "/home/tomek/msc/dea/automation/profile_no_ublock"
tries_count = 3

def connect_to_chrome(debug_port):
    options = webdriver.ChromeOptions()
    options.add_experimental_option('debuggerAddress', f"localhost:{debug_port}")
    return webdriver.Chrome(options=options, executable_path=chromedriver_path)


def run_chrome_with_ublock(trace_file=None, debug_port=None):
    return run_chrome(trace_file=trace_file, debug_port=debug_port,
                      profile_path=profile_ublock)


def run_chrome_without_ublock(trace_file=None, debug_port=None):
    return run_chrome(trace_file=trace_file, debug_port=debug_port,
                      profile_path=profile_no_ublock)


def run_chrome(trace_file=None, profile_path=None, debug_port=None):
    args = [
        chrome_path,
        "--disable-background-networking",
        "--disable-client-side-phishing-detection",
        "--disable-default-apps",
        "--disable-hang-monitor",
        "--disable-popup-blocking",
        "--disable-prompt-on-repost",
        "--disable-sync",
        "--disable-web-resources",
        "--enable-automation",
        "--ignore-certificate-errors",
        "--no-first-run",
        "--no-sandbox",
        "--use-mock-keychain",
        "about:blank",
    ]

    if trace_file is not None:
        args.append(f"--js-flags='--trace-dea --trace-dea-file={trace_file}'")
        pass
        # args.append(f"--js-flags='--trace'")

    if profile_path is not None:
        args.append(f"--user-data-dir={profile_path}")

    if debug_port is None:
        debug_port = random.randint(10000, 60000)

    args.append(f"--remote-debugging-port={debug_port}")

    cmd=""
    for arg in args:
        cmd += f"{arg} "

    print(cmd)

    return subprocess.Popen(cmd, shell=True)


def open_website_and_quit(website, browser, webdriver):
    sleep(50)
    webdriver.get(website)
    sleep(15)
    webdriver.close()
    # browser.terminate()


def get_random_port():
    code = 0
    sock = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
    sock.settimeout(0.2)
    while code == 0:
        port = random.randint(10000, 60000)
        code = sock.connect_ex(('localhost',port))
    return port


def collect_trace(website, trace_file, profile_path):
    port = get_random_port()
    chrome = run_chrome(debug_port=port, trace_file=trace_file, profile_path=profile_path)
    webdriver = connect_to_chrome(debug_port=port)
    open_website_and_quit(website, chrome, webdriver)


def collect_positive_trace(website, trace_file):
    collect_trace(website, trace_file, profile_ublock)


def collect_negative_trace(website, trace_file):
    collect_trace(website, trace_file, profile_no_ublock)


def url_to_path(url):
    path = url.replace("https://", "")
    path = path.replace("http://", "")
    path = path.replace("/", ".")
    return path


def collect_traces(website, traces_dir):
    if traces_dir[-1] == '/':
        traces_dir = traces_dir[:-1]
    website_subdir = url_to_path(website)
    website_path = f"{traces_dir}/{website_subdir}"
    try:
        os.mkdir(website_path)
    except FileExistsError:
        pass
    for i in range(tries_count):
        positive_trace = f"{website_path}/p_{i}_"
        negative_trace = f"{website_path}/n_{i}_"
        collect_positive_trace(website, positive_trace)
        collect_negative_trace(website, negative_trace)


if __name__ == "__main__":
    parser = argparse.ArgumentParser()
    parser.add_argument('website', help="Website to open")
    parser.add_argument('--traces-dir', help="Where to save the trace",
                        default=None, required=True)
    args = parser.parse_args()

    collect_traces(args.website, args.traces_dir)

    print()

