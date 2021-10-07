import os
import math
from datetime import datetime
from datetime import timedelta
import pandas as pd


def serialize_header_general(config, cluster_group, station_emission_df):

    DATASET = config.get("DATASET", "PTEMARB.DAT")
    DATAVER = config.get("DATAVER", "2.1")
    SCENARIO = config.get("SCENARIO")
    UTCZONE = config.get("UTCZONE")

    UTMZONES = cluster_group.utm_zone.unique()
    if len(UTMZONES) != 1:
        raise ValueError("One station should have a single UTM zone")
    else:
        UTMZONE = UTMZONES[0]


    BEGIN_DATE = pd.to_datetime(min(station_emission_df.date))
    END_DATE = pd.to_datetime(max(station_emission_df.date))

    N_SOURCES = len(cluster_group.index)

    SPECIES = config.get("SPECIES")
    N_SPECIES = len(SPECIES)
    SPECIES_NAMES = "'" + "'\t'".join([x['NAME'] for x in SPECIES]) + "'"
    SPECIES_WEIGHTS = "\t".join([str(x['WEIGHT']) for x in SPECIES])

    with open(os.path.join("templates", "header.DAT")) as file:
        template = file.read()

    header = template.format(
        DATASET=DATASET,
        DATAVER=DATAVER,
        SCENARIO=SCENARIO,
        UTMZONE=UTMZONE,
        UTCZONE=UTCZONE,
        BEGIN_YEAR=BEGIN_DATE.year,
        BEGIN_JDAY=BEGIN_DATE.timetuple().tm_yday,
        BEGIN_HOUR=BEGIN_DATE.hour,
        BEGIN_SECOND=BEGIN_DATE.second,
        END_YEAR=END_DATE.year,
        END_JDAY=END_DATE.timetuple().tm_yday,
        END_HOUR=END_DATE.hour,
        END_SECOND=END_DATE.second,
        N_SOURCES=N_SOURCES,
        N_SPECIES=N_SPECIES,
        SPECIES_NAMES=SPECIES_NAMES,
        SPECIES_WEIGHTS=SPECIES_WEIGHTS
    )
    return header


def serialize_header_sources(config, cluster_group, cluster_emission_df):

    headers = []

    with open(os.path.join("templates", "header_source.DAT")) as file:
        template = file.read()

    sources = cluster_group.to_dict(orient="records")

    for source in sources:
        SOURCE_NAME = source.get("source_id")
        EASTING = source.get("easting_km")
        NORTHING = source.get("northing_km")
        STACK_HEIGHT = source.get("stack_height_m")
        STACK_DIAMETER = source.get("stack_diameter_m")
        STACK_BASE_ELEVATION = source.get("stack_base_elevation", 0)
        DOWNWASH_FLAG = source.get("downwash_flag", 0)
        VERTICAL_MOMENTUM = source.get("vertical_momentum", 1)
        USER_FLAG = source.get("user_flag", "0")

        header = template.format(
            SOURCE_NAME=SOURCE_NAME,
            EASTING=EASTING,
            NORTHING=NORTHING,
            STACK_HEIGHT=STACK_HEIGHT,
            STACK_DIAMETER=STACK_DIAMETER,
            STACK_BASE_ELEVATION=STACK_BASE_ELEVATION,
            DOWNWASH_FLAG=DOWNWASH_FLAG,
            VERTICAL_MOMENTUM=VERTICAL_MOMENTUM,
            USER_FLAG=USER_FLAG
        )

        headers.append(header)

    return "\n".join(headers) + "\n"


def serialize_date_emissions(config, date, cluster_date_emissions, template_header, template_record):

    record_duration = timedelta(seconds=config["RECORD_DURATION_SEC"])
    date = pd.to_datetime(date)
    date_end = date + record_duration - timedelta(seconds=1) # To have 3600sec rather than 1 hour

    header = template_header.format(
        IBYEAR=date.year,
        IBJUL=date.timetuple().tm_yday,
        IBHOUR=date.hour,
        IBSEC=date.minute * 60 + date.second,
        IEYEAR=date_end.year,
        IEJUL=date_end.timetuple().tm_yday,
        IEHOUR=date_end.hour,
        # The example was 3600
        IESEC=date_end.minute * 60 + date_end.second + 1
    )

    def row_to_record(row, config, template_record):

        # TEMP_K = row.temp_k
        # PRES_ATM = row.PRESION_GASES_SALIDA_ATM or 1.0 # Some values have PRES=0

        # We use Ideal Gas Law to adjust for temperature and pression
        # DILATATION = TEMP_K/273.15 * 1/PRES_ATM
        # DIAMETER = row.stack_diameter

        # load factor * 27 m/s
        # VEXIT = "%0.6f" % (row.capacity_mw / 2600 * 27,)
        #
        species = config["SPECIES"]
        #
        # Emission rate (g/s)
        species_g_s = []
        for s in species:
            if row.unit == "kg/h":
                s_g_s = "%0.6f" % (row[s['NAME']] * 1000 / 3600,)
            elif row.unit == "g/s":
                s_g_s = "%0.6f" % (row[s['NAME']])
            else:
                raise ValueError("Unknown unit: ", row.unit)

            if s_g_s == "0":
                s_g_s = "0.0"
            species_g_s.append(s_g_s)

        QEMIT = " ".join(['0.0' if x == 'nan' else x for x in species_g_s])

        record = template_record.format(
            SOURCE_NAME=row.source_id,
            TEMPK=row.temp_k,
            VEXIT=row.vexit_m_s,
            SIGY="0.0",
            SIGZ="0.0",
            QEMIT=QEMIT
        )
        return record

    records = cluster_date_emissions.apply(lambda row: row_to_record(row, config, template_record), axis=1)

    return header + "\n" + "\n".join(list(records))



