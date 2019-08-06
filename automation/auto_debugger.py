import argparse
import logging
import os
import shutil
import socket
import subprocess
import random
import re

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


def open_website_and_quit(website, browser, webdriver):
    sleep(3)
    try:
        webdriver.get(website)
        sleep(30)
        webdriver.close()
        sleep(2)
        browser.terminate()
    except TimeoutException as e:
        logging.warning(f"TIMEOUT {website}")
        sleep(90)
        webdriver.close()
        browser.kill()
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


def collect_trace(website, trace_file, profile_path):
    os.system("killall -9 chrome")
    port = get_random_port()
    chrome = run_chrome(debug_port=port, trace_file=trace_file, profile_path=profile_path)
    webdriver = connect_to_chrome(debug_port=port)
    open_website_and_quit(website, chrome, webdriver)


def collect_positive_trace(website, trace_file):
    collect_trace(website, trace_file, PROFILE_UBLOCK)


def collect_negative_trace(website, trace_file):
    collect_trace(website, trace_file, PROFILE_NO_UBLOCK)


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
        text = f"at https?:\/\/{re.escape(short_url)}"
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


def get_website_path(parent_dir, website):
    if parent_dir[-1] == '/':
        parent_dir = parent_dir[:-1]
    website_subdir = url_to_path(website)
    return f"{parent_dir}/{website_subdir}"


def collect_traces(website, traces_dir):
    logging.info(f"[COLLECTION] START {website}")

    website_path = get_website_path(traces_dir, website)

    if (website[0:2] == '--' or website[0:2] == "++"):
        logging.info(f"[COLLECTION] SKIP (disabled) {website}")
        return

    try:
        os.mkdir(website_path)
    except FileExistsError:
        logging.info(f"[COLLECTION] SKIP {website}")
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
    logging.info(f"[COLLECTION] FINISHED {website}")


def remove_traces(website, traces_dir):
    logging.info(f"[CLEANUP] Delete traces of {website}")

    website_path = get_website_path(traces_dir, website)
    logging.debug(f"[CLEANUP] Delete directory {website_path}")

    shutil.rmtree(website_path, ignore_errors=True)


def escape_shell(path):
    return path.replace(";", "\;").replace("(", "\(").replace(")", "\)") \
        .replace("<", "\<").replace(">", "\>").replace("&", "\&")


def analyze_traces(website, traces_dir, results_dir):
    logging.info(f"[ANALYSIS] {website}")

    website_traces_path = get_website_path(traces_dir, website)
    website_results_path = get_website_path(results_dir, website)

    if (website[0:2] == '--' or website[0:2] == "++"):
        logging.info(f"[ANALYSIS] SKIP (disabled) {website}")
        return

    try:
        os.mkdir(website_results_path)
    except FileExistsError:
        logging.info(f"[ANALYSIS] SKIP {website}")
        return

    try:
        cmd = "stack exec dea-exe -- "
        for i in range(3):
            cmd += escape_shell(f"{website_traces_path}/p_{i}.tr ")
        for i in range(3):
            cmd += escape_shell(f"{website_traces_path}/n_{i}.tr ")

        out_file = escape_shell(f"{website_results_path}/analysis.out")
        err_file = escape_shell(f"{website_results_path}/analysis.err")
        cmd += f"> {out_file} "
        cmd += f"2> {err_file} "
        cmd += "+RTS -M26G -RTS "

        analysis = subprocess.Popen(cmd, shell=True)
        analysis.wait()
        logging.info(f"[ANALYSIS] return code: {analysis.returncode}")

        if analysis.returncode == 0:
            remove_traces(website, traces_dir)

    except Exception as e:
        logging.error(e)
        return
    logging.info(f"[ANALYSIS] FINISHED {website}")


if __name__ == "__main__":
    parser = argparse.ArgumentParser()
    parser.add_argument('--traces-dir', help="Where to save the trace",
                        default=None, required=True)
    parser.add_argument('--results-dir', help="Where to save the trace",
                        default=None)
    parser.add_argument('--log-file', help="Where to save the log", default=None)
    parser.add_argument('--verbose', '-v', help="Verbose logging",
                        default=False, action='store_true')
    parser.add_argument('--skip-first', help="Number of websites to skip",
                        default='0', type=int)

    action = parser.add_argument_group('Actions', "Actions that script should perform")
    action.add_argument('--collect', help="Trace collection",
                        default=False, action='store_true')
    action.add_argument('--analyze', help="Trace analysis",
                        default=False, action='store_true')
    action.add_argument('--remove-traces', help="Remove traces after analysis",
                        default=False, action='store_true')

    source = parser.add_mutually_exclusive_group()
    source.add_argument('website', help="Website to open", nargs="?")
    source.add_argument('--list-file', default=None,
                        help="File with a list of websites to collect traces from")

    args = parser.parse_args()

    if args.analyze and args.results_dir is None:
        parser.error("--analyze requires --results-dir")

    logger_config = {
        'format': '%(levelname)s: %(asctime)s: %(message)s',
        'datefmt': '%m/%d/%Y %H:%M:%S',
    }
    logger_config['level'] = logging.DEBUG if args.verbose else logging.INFO
    if args.log_file:
        logger_config['filename'] = args.log_file

    logging.basicConfig(**logger_config)
    logging.debug(args)

    website_list = [args.website] if args.website is not None else list(open(args.list_file))

    for line in tqdm(website_list[args.skip_first:]):
        website = line.strip()
        if args.analyze:
            results_path = get_website_path(args.results_dir, website)
            if os.path.exists(results_path):
                logging.info(f"[COLLECTION][ANALYSIS] SKIP {website}")
                continue

        if args.collect:
            collect_traces(website, args.traces_dir)
        if args.analyze:
            analyze_traces(website, args.traces_dir, args.results_dir)
        if args.remove_traces:
            remove_traces(website, args.traces_dir)
