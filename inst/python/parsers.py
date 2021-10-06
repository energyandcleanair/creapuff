import glob
import os
import pandas as pd
from datetime import datetime
from datetime import timedelta
from glob import glob


def fill_ts(emission_df):

    emission_df["date"] = pd.to_datetime(emission_df.date)
    emission_df = emission_df.groupby(['source_id', 'unit']). \
        apply(lambda x: x.set_index('date').resample('H').mean().fillna(0)). \
        reset_index()
    return emission_df


def read_emissions(file, config):

    date_from = config.get("BEGIN_DATE")
    date_to = config.get("END_DATE")

    # Read emission files
    emission_df = pd.read_csv(file)

    # Finish filtering by date
    if date_from:
        emission_df = emission_df[pd.to_datetime(emission_df.date) >= datetime.strptime(date_from, "%Y-%m-%d")]
    if date_to:
        emission_df = emission_df[pd.to_datetime(emission_df.date) <= datetime.strptime(date_to, "%Y-%m-%d") + timedelta(hours=23)]

    emission_df = fill_ts(emission_df)

    return emission_df


def read_sources(file, config):
    sources_df = pd.read_csv(file)

    if not "easting_km" in sources_df.columns:
        sources_df["easting_km"] = sources_df["easting_m"] / 1000

    if not "northing_km" in sources_df.columns:
        sources_df["northing_km"] = sources_df["northing_m"] / 1000

    return sources_df
