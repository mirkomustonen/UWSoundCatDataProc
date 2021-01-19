#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Mon Jan 11 22:57:18 2021

@author: mirko
"""

import os  # Provides portable way of using operating system dependent functionality
import pandas as pd  # Pandas package: easy-to-use data structures and analysis tools)
from datetime import datetime, timezone
from distance_az import distance_az


def calc_dist_az_ais(location_name):
    """
    Function calculating and adding distances, azimuths to the interpolated AIS data
    input:  str format number denoting the sound monitoring location "20", "21", "22", "23"
    output: pandas dataframe time-regularized AIS with calculated distance and azimuth
    """
    ais_dir = 'AIS_data_interpolated'  # Directory with monitoring location AIS data folders
    # File name of the AIS data to be read
    ais_int_file_name = 'B' + location_name + '_AIS_interpolated.csv'
    # Make full path of the AIS data file to be read
    ais_file_path = os.path.join(ais_dir, ais_int_file_name)

    ais_data_l = pd.read_csv(ais_file_path)  # Read the data from the file

    dep_meta_tab = pd.read_table('dep_meta_data.txt', sep=' ')  # Read deployment meta data
    # Subset deployment meta data for the station
    dep_meta_tab_st = dep_meta_tab[dep_meta_tab["station"] == int(location_name)].copy()

    k = 0
    for row in dep_meta_tab_st.itertuples():
        data_beg_t_n = datetime.strptime(str(row.Data_beg_t), '%Y%m%d%H%M%S')
        data_beg_t_d = data_beg_t_n.replace(tzinfo=timezone.utc).timestamp()

        data_end_t_n = datetime.strptime(str(row.Data_end_t), '%Y%m%d%H%M%S')
        data_end_t_d = data_end_t_n.replace(tzinfo=timezone.utc).timestamp()

        ais_data_l_s = ais_data_l[(ais_data_l["Time"] >= data_beg_t_d) &
                                  (ais_data_l["Time"] <= data_end_t_d)].copy()

        dist_dep, az_dep = distance_az(row.Longitude, row.Latitude,
                                       ais_data_l_s[['Longitude']].to_numpy(),
                                       ais_data_l_s[['Latitude']].to_numpy())

        ais_data_l_s['Distance'] = dist_dep
        ais_data_l_s['Azimuth'] = az_dep

        if k == 0:
            ais_data_l_l = ais_data_l_s
            k = 1
        else:
            ais_data_l_l = ais_data_l_l.append(ais_data_l_s)
    return ais_data_l_l


def make_ts_ais(location_name):
    """
    Function for reading the AIS data.
    input:  str format number denoting the sound monitoring location "20", "21", "22", "23"
    output: pandas dataframe containing all the AIS data from one monitoring location
    """
    ais_data_l_l = calc_dist_az_ais(location_name)
    ais_ts_l = ais_data_l_l.loc[ais_data_l_l.groupby('Time', sort=False)['Distance'].idxmin()]

    return ais_ts_l

loc_name = '23'
ais_ts = make_ts_ais(loc_name)
ais_ts.to_csv(os.path.join('AIS_data_interpolated', 'B' + loc_name
                           + '_AIS_closest_ts.csv'), index=False)
