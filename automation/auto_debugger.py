import argparse
import os
import socket
import subprocess
import random
import logging

from selenium import webdriver
from selenium.common.exceptions import TimeoutException
from selenium.webdriver.common.desired_capabilities import DesiredCapabilities
from time import sleep
from tqdm import tqdm


CHROME_PATH = "/home/tomek/msc/chromium/src/out/Opt/chrome"
CHROMEDRIVER_PATH = "/home/tomek/msc/chromedriver/chromedriver"
PROFILE_UBLOCK = "/home/tomek/msc/dea/automation/profile_ublock"
PROFILE_NO_UBLOCK = "/home/tomek/msc/dea/automation/profile_no_ublock"
TRIES_COUNT = 3
UBLOCK_ID = 'cjpalhdlnbpafiamejdnhcphjbkeiagm'
DOMAIN_BLOCKER_ID = 'ggdcjplapccgoinblmidpkoocfafajfa'


def connect_to_chrome(debug_port):
    options = webdriver.ChromeOptions()
    options.add_experimental_option('debuggerAddress', f"localhost:{debug_port}")
    return webdriver.Chrome(options=options, executable_path=CHROMEDRIVER_PATH)


def run_chrome_with_ublock(trace_file=None, debug_port=None):
    return run_chrome(trace_file=trace_file, debug_port=debug_port,
                      profile_path=PROFILE_UBLOCK)


def run_chrome_without_ublock(trace_file=None, debug_port=None):
    return run_chrome(trace_file=trace_file, debug_port=debug_port,
                      profile_path=PROFILE_NO_UBLOCK)

def run_chrome(trace_file=None, profile_path=None, debug_port=None):
    args = [
        CHROME_PATH,
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
        # "--headless",
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
    args.append("2> /dev/null")

    cmd=""
    for arg in args:
        cmd += f"{arg} "

    logging.debug(cmd)

    return subprocess.Popen(cmd, shell=True)


def open_website_and_quit(website, browser, webdriver, prep_time):
    sleep(prep_time)
    try:
        webdriver.get(website)
        sleep(30)
        webdriver.close()
        sleep(2)
        browser.terminate()
    except TimeoutException as e:
        logging.warning(f"TIMEOUT {website}")
    except Exception as e:
        webdriver.close()
        browser.kill()
        raise e


def get_random_port():
    code = 0
    sock = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
    sock.settimeout(0.2)
    while code == 0:
        port = random.randint(10000, 60000)
        code = sock.connect_ex(('localhost', port))
    return port


def collect_trace(website, trace_file, profile_path, prep_time):
    port = get_random_port()
    chrome = run_chrome(debug_port=port, trace_file=trace_file, profile_path=profile_path)
    webdriver = connect_to_chrome(debug_port=port)
    open_website_and_quit(website, chrome, webdriver, prep_time)


def collect_positive_trace(website, trace_file):
    collect_trace(website, trace_file, PROFILE_UBLOCK, 50)


def collect_negative_trace(website, trace_file):
    collect_trace(website, trace_file, PROFILE_NO_UBLOCK, 10)


def url_to_path(url):
    path = remove_protocol(url)
    path = path.replace("/", ".")
    return path


def remove_protocol(url):
    return url.replace("https://", "").replace("http://", "")


def remove_extension_traces(website, website_path, file_prefix):
    traces = os.listdir(website_path)
    current_traces = []

    for t in traces:
        if file_prefix in t:
            current_traces.append(t)

    found = False
    dst_path = f'{website_path}/{file_prefix[:-1]}.tr'

    for t in current_traces:
        short_url = remove_protocol(website)
        path = f'{website_path}/{t}'
        text = f"at https?:\/\/{short_url}"
        result = subprocess.run(["grep", "-P", "-c", text, path], capture_output=True)
        count = int(result.stdout)

        logging.debug(f"{t}:{count}")

        if count == 0:
            logging.debug(f"Remove: {t}")
            os.remove(path)
        else:
            if found:
                raise Exception(f"Two suitable traces found for prefix: {file_prefix}")
            logging.debug(f"Save: {t}")
            os.rename(path, dst_path)
            found = True

    if not found:
        raise Exception(f"No suitable trace found for prefix: {file_prefix}")


def collect_traces(website, traces_dir):
    logging.info(f"START {website}")
    if traces_dir[-1] == '/':
        traces_dir = traces_dir[:-1]
    website_subdir = url_to_path(website)
    website_path = f"{traces_dir}/{website_subdir}"
    try:
        os.mkdir(website_path)
    except FileExistsError:
        logging.info(f"SKIP {website}")
        return
    try:
        for i in range(TRIES_COUNT):
            positive_prefix = f"p_{i}_"
            negative_prefix = f"n_{i}_"
            positive_trace = f"{website_path}/{positive_prefix}"
            negative_trace = f"{website_path}/{negative_prefix}"
            collect_negative_trace(website, negative_trace)
            remove_extension_traces(website, website_path, negative_prefix)
            collect_positive_trace(website, positive_trace)
            remove_extension_traces(website, website_path, positive_prefix)
    except Exception as e:
        logging.error(e)
        return
    logging.info(f"FINISHED {website}")


if __name__ == "__main__":
    parser = argparse.ArgumentParser()
    parser.add_argument('--traces-dir', help="Where to save the trace",
                        default=None, required=True)
    parser.add_argument('--log-file', help="Where to save the log", default=None)
    parser.add_argument('--verbose', '-v', help="Verbose logging",
                        default=False, action='store_true')

    group = parser.add_mutually_exclusive_group()
    group.add_argument('website', help="Website to open", nargs="?")
    group.add_argument('--list-file', default=None,
                        help="File with a list of websites to collect traces from")

    args = parser.parse_args()

    logger_config = {
        'format': '%(levelname)s: %(asctime)s: %(message)s',
        'datefmt': '%m/%d/%Y %H:%M:%S',
    }
    logger_config['level'] = logging.DEBUG if args.verbose else logging.INFO
    if args.log_file:
        logger_config['filename'] = args.log_file

    logging.basicConfig(**logger_config)
    logging.debug(args)

    if args.website is not None:
        collect_traces(args.website, args.traces_dir)

    if args.list_file:
        for line in tqdm(list(open(args.list_file))):
            website = line.strip()
            collect_traces(website, args.traces_dir)
