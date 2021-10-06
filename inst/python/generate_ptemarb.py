#!/usr/bin/env python
# -*- coding: utf-8 -*-

"""
Creating input file for CALPUFF

Author: Hubert Thieriot, hubert@energyandcleanair.org
Created: 2020-07-13
"""

import pandas as pd
import argparse
import json
import os
import glob

from writers import init_stream
from writers import write_header_general
from writers import write_ptemarbs
from parsers import read_emissions
from parsers import read_sources


if __name__ == "__main__":

    parser = argparse.ArgumentParser(description='Prepare input file for CALPUFF')
    parser.add_argument('config_file', metavar='config-file', type=str, nargs='?',
                        help='Configuration file (json)')

    args = parser.parse_args()
    with open(args.config_file) as json_file:
        configs = json.load(json_file)

    config = None
    if isinstance(configs, dict):
        configs = [configs]

    for config in configs:
        emissions_df = read_emissions(config["EMISSIONS_FILE"], config)
        sources_df = read_sources(config["SOURCES_FILE"], config)
        write_ptemarbs(folder=config.get("OUTPUT_FOLDER", ""),
                       config=config,
                       emissions_df=emissions_df,
                       sources_df=sources_df)

