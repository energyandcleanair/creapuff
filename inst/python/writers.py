
"""
PTERMARB.DAT file consists of the following sections:

# Header Records - General Data: infos about timezone, projection, species etc.

# Time-Invariant Data Records: list of sources

# Time-Varying Data Records: list of emission per sources




"""
import os
import sys
import numpy as np
from tqdm import tqdm

from serializers import serialize_header_general
from serializers import serialize_header_sources
from serializers import serialize_date_emissions


def init_stream(folder, filename):
    return open(os.path.join(folder, filename), "w", encoding="utf-8")


def write_header_general(stream, config, cluster_group, cluster_emission_df):
    header = serialize_header_general(config, cluster_group, cluster_emission_df)
    stream.write(header)
    return


def write_header_sources(stream, config, cluster_group, cluster_emission_df):
    header_sources = serialize_header_sources(config, cluster_group, cluster_emission_df)
    stream.write(header_sources)
    return


def write_records(stream, config, cluster_group, cluster_emission_df):
    dates = cluster_emission_df.groupby("date")

    with open(os.path.join("templates", "emission_header.DAT")) as file:
        template_header = file.read()
    with open(os.path.join("templates", "emission_record.DAT")) as file:
        template_record = file.read()

    pdates = tqdm(dates)
    pdates.set_description("Processing %s" % cluster_emission_df.cluster_id.unique())

    for date, cluster_date_emissions in pdates:
        date_records = serialize_date_emissions(config, date, cluster_date_emissions,
                                                template_header=template_header,
                                                template_record=template_record)
        stream.write(date_records+"\n")
    return


def write_cluster(folder, config, cluster_name, cluster_group, cluster_emission_df):
    prefix = config.get("PREFIX", "")
    filename = cluster_name.replace(" ", "_").lower()
    stream = init_stream(folder, filename=prefix+filename+".DAT")

    write_header_general(stream, config, cluster_group, cluster_emission_df)
    write_header_sources(stream, config, cluster_group, cluster_emission_df)
    write_records(stream, config, cluster_group, cluster_emission_df)

    stream.close()
    return


def write_ptemarbs(folder, config, emissions_df, sources_df):

    # Fill default values and keep those with enough information
    sources_df = sources_df\
        .fillna(value={
            "stack_height_m": config.get("DEFAULT_STACK_HEIGHT", np.nan),
            "stack_diameter_m": config.get("DEFAULT_STACK_DIAMETER", np.nan)})\
        .dropna(subset=["stack_height_m", "stack_diameter_m", "easting_km", "northing_km", "utm_zone"])

    for cluster_id, cluster_group in sources_df.groupby("cluster_id"):
        try:
            cluster_emission_df = cluster_group.merge(emissions_df, how="inner")
            write_cluster(
                folder,
                config,
                cluster_name=cluster_id,
                cluster_group=cluster_group,
                cluster_emission_df=cluster_emission_df)

        except ValueError as e:
            print("Failed for cluster: " + cluster_id)
            continue

    return
