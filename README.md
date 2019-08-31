Detecting Anti-Adblockers using Differential Execution Analysis

Author: Tomasz Kępa

# Installation

## Intrumenting Chromium

The patch was developed and tested with Chromium version 74.0.3715.1
which corresponds to V8 version 7.4.207.

To instrument the browser please check out its code in the aforementioned
version and patch the diff from `patches` over the v8 subdirectory.

Please provide the path to `chrome` executable in `automation/auto_debugger.py`.

It is also possible to patch a standalone V8, just use the same patch
on a standalone v8 repository.

## ChromeDriver

Selenium does not communicate with Chromium directly. It uses a specialized
application called ChromeDriver. Each major version of Chromium requries
a different version of ChromeDriver. Please download an appropriate version
and provide the path in `automation/auto_debugger.py`.

## User profiles

The system uses two configurations of Chromium to detect anti-adblockers.
The configurations, however, have to be set up manually.

In our experiments the first profile did not contain any extensions,
the second one had AdBlock Plus installed.

Please provide the paths to both profiles in `automation/auto_debugger.py`.

## Python Dependencies

The scheduling program has a few dependencies. All of them are listed
in `automation/requirements.txt`.

The recommended way to install them is to create a virtual environment
and use `pip` for installation.

## Haskell dependencies

The program was prepared using `stack`.

To install all dependencies please type `stack setup`.

To build the analyzing program please type `stack build --ghc-options=-O2`.


# Usage

The entire pipeline is run by `automation/auto_debugger.py`.
Please refer to its help (`-h`) for a list of flags.


# Troubleshooting

Please turn on the logging and inspect the log file. If it turns out not
to be enough, more verbose logging can be turned on by the `--verbose` flag.
