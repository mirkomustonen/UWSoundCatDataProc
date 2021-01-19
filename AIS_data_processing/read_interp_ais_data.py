#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Fri Jan  8 14:31:19 2021

@author: mirko
"""
import os
import pandas as pd
import numpy as np
from scipy.interpolate import CubicSpline

def read_ais(location_name):
    """
    Function for reading the AIS data.
    input:  str format number denoting the sound monitoring location "20", "21", "22", "23"
    output: pandas dataframe containing all the AIS data from one monitoring location
    """
    ais_dir = 'AIS_BIASstations'  # Directory with monitoring location AIS data folders
    # List locations' monthly AIS data files
    ais_loc_path = os.path.join(ais_dir, 'BIAS' + location_name)
    # Filter out files except .txt files
    files_list = list(filter(lambda k: '.txt' in k, os.listdir(ais_loc_path)))
    k = 0
    # Loop through the monthly AIS data files
    for i_file in files_list:
        i_file = os.path.join(ais_loc_path, i_file)  # Construct file path for read
        data = pd.read_table(i_file, sep=';')  # Read the data from the file
        daytime = data.DAY + ' ' + data.TIME  # Add the date and time together into one timestamp

        # Construct a new dataframe with new variable names from the read data
        ais_data = pd.DataFrame(data={'Time': daytime, 'Latitude': data.LAT,
                                      'Longitude': data.LON, 'MMSI': data.MMSI,
                                      'Vs': data.Vs, 'Ship_type': data.TYPEc,
                                      'L1': data.L1, 'L2': data.L2, 'B1': data.B1,
                                      'B2': data.B2, 'Draught': data.DRAUGHT})

        # Convert the timestamp to the datetime format
        ais_data.Time = pd.to_datetime(ais_data.Time, format='%d-%m-%Y %H:%M:%S', utc=True)
        # Add the AIS data from one location together into one variable
        if k == 0:
            ais_data_l = ais_data.copy()  # Create a file to append first iteration data
            k = 1
        else:
            ais_data_l = ais_data_l.append(ais_data)  # Append to the data frame
    return ais_data_l  # return the pandas dataframe only

# from shapely.geometry import Point
# from distance_az import calc_los_area_polygon
# def read_spacial_filt_ais(location_name):
#     """
#     Function for spacial filtering the AIS data.
#     input:  str format number denoting the sound monitoring location "20", "21", "22", "23"
#     output: pandas dataframe containing all the AIS data from one monitoring location with
#     all points outside 20 km or behind land excluded
#     """
#     ais_data_l = read_ais(location_name)
#     # If MMSI appears only once discard the data
#     ais_data_l = ais_data_l[ais_data_l.groupby('MMSI').MMSI.transform(len) > 1]

#     #---Spacial filter----
#     dep_meta_tab = pd.read_table('dep_meta_data.txt', sep=' ')
#     dep_meta_tab_st = dep_meta_tab[dep_meta_tab["station"] == int(location_name)]
#     dep_lat_mean = dep_meta_tab_st['Latitude'].mean()
#     dep_lon_mean = dep_meta_tab_st['Longitude'].mean()

#     los_poly = calc_los_area_polygon(dep_lat_mean, dep_lon_mean)
#     filt = np.zeros(len(ais_data_l), dtype=bool)

#     for i in np.arange(len(ais_data_l)):
#         ship_point = Point(ais_data_l.Longitude.values[i], ais_data_l.Latitude.values[i])
#         filt[i] = los_poly.contains(ship_point)

#     filt = pd.Series(filt, name='bools')
#     ais_data_l = ais_data_l[filt.values]
#     return ais_data_l


def read_add_voyages_ais(location_name):
    """
    Function for reading the AIS data and adding the voyage numbers to the data
    input:  str format number denoting the sound monitoring location "20", "21", "22", "23"
    output: pandas dataframe containing all the AIS data from one monitoring location with the
    Voyages variable listing the voyage numbers for different MMSI-s
    """
    ais_data_l = read_ais(location_name)  # read_ais function for reading the AIS data
    # Remove the data a ship has only one row in the dataframe
    ais_data_l = ais_data_l[ais_data_l.groupby('MMSI').MMSI.transform(len) > 1]

    ships = ais_data_l.MMSI.unique()  # Make a list of unique MMSI numbers
    time_thresh = int(20*60)  # Set the time threshold for comparing the data in int(min*seconds)
    time_dif_min = 15  # For filtering out points closer than 15 seconds in time
    # Loop through all the different MMSI numbers in the data
    for one_ship in ships:
        # Create dataframe with on MMSI AIS data only
        ais_one_ship = ais_data_l[ais_data_l.MMSI == one_ship]
        # Sort the AIS data according to time
        ais_one_ship = ais_one_ship.sort_values(by='Time')

        i_no = 0  # Assign numbers to ship voyages, the first voyage is numbered 0
        voyage_no = [0] * len(ais_one_ship) # Make list zeros for voyage numbers
        # Create empty list for filtering points closer > 15 s in time
        filter_ls = list(bytearray(len(ais_one_ship)))
        filter_ls[0] = True  # Set first to be True
        # Loop through all the AIS data points from a single ship
        for i in range(1, len(ais_one_ship)):
            # Calculate the time difference between two consecutive AIS data points in UNIX time
            i_time_dif = int(ais_one_ship.Time.values[i])/10.**9 - int(ais_one_ship.Time.values[i-1])/10.**9
            # If the time difference between two consecutive points is larger
            # than the time threshold the second point is from a new voyage of a ship
            if i_time_dif > time_thresh:
                i_no += 1
            voyage_no[i] = i_no  # Add the voyage number to the list of voyages
            # Check a wether points close in time and add result to list
            filter_ls[i] = i_time_dif > time_dif_min
        # Add the voyage numbers to the dataframe of the single ship
        ais_one_ship['Voyage'] = pd.Series(voyage_no, index=ais_one_ship.index)
        ais_one_ship = ais_one_ship[filter_ls]  # Filtering points closer > 15 s in time
        # If ships voyage only has one data point discard the data
        ais_one_ship = ais_one_ship[ais_one_ship.groupby('Voyage').Voyage.transform(len) > 1]
        if one_ship == ships[0]:
            ais_data_voy_l = ais_one_ship.copy()  # Create a df to append first iteration data
        else:
            ais_data_voy_l = ais_data_voy_l.append(ais_one_ship)  # Append to the df
    return ais_data_voy_l  # return the pandas dataframe only


def read_interpolate_ais(location_name):
    """
    Function that reads AIS data, adds voyage no-s and interpolates the ship locations
    for a regular 20 second time step
    input:  str format number denoting the sound monitoring location "20", "21", "22", "23"
    output: ais_data_l - pandas dataframe containing interpolated AIS data with 20 s time steps
            ais_voyages_l - pandas dataframe containing ships voyages data length, width, draught
    """
    # read_add_voyages_ais function for reading the AIS data and adding voyage numbers
    ais_data_voy_l = read_add_voyages_ais(location_name)
    # Remove data if a voyage has less than 3 points
    ais_data_voy_l = ais_data_voy_l[ais_data_voy_l.groupby('Voyage').Voyage.transform(len) > 3]
    # Make new variable MMSI_Voy listing MMSI number and voyage number together in str format
    ais_data_voy_l["MMSI_Voy"] = ais_data_voy_l["MMSI"].astype("str") + ' ' + ais_data_voy_l["Voyage"].astype("str")
    # Make a list of unique MMSI_Voy numbers
    ships_voys = ais_data_voy_l.MMSI_Voy.unique()

    count = 1  # Set counter for tracking progress
    # Loop through all the different MMSI_Voy-s in the data
    for one_voy in ships_voys:
        print(count, '/', len(ships_voys))  # Show progress by printing number of MMSI_Voy
        count += 1
        # Subset data to be from one voyage of a single ship
        ais_one_ship_voy = ais_data_voy_l[ais_data_voy_l.MMSI_Voy == one_voy]
        # Convert the timestamps from dateimetime to UNIX time
        time = ais_one_ship_voy.Time.values.astype(int)/10.**9
        # Fit cubic splines through the existing Longitude and time data
        csx = CubicSpline(time, ais_one_ship_voy.Longitude.values)
        # Fit cubic splines through the existing Latitude and time data
        csy = CubicSpline(time, ais_one_ship_voy.Latitude.values)

        # Creata an array with one second unix time stamps from first to last point of one voyage of a ship
        n_time = np.arange(int(ais_one_ship_voy.Time.values[0])/10.**9,
                           int(ais_one_ship_voy.Time.values[-1])/10.**9 + 1)

        # Create a function select 20 sec multiple time stamps from 1 sec time stamps
        # t is True if the remainder when divided by 100 (i.e. the last two digits)
        # are in a list [0,20,40,80]
        sec_20_func = lambda t: t % 100 in [0, 20, 40, 60, 80]
        v_sec_20_func = np.vectorize(sec_20_func)  # Vectorize the functions

        n_time_20 = n_time[v_sec_20_func(n_time)]  # Select only timestamps for seconds 0,20,40
        lons_20 = csx(n_time_20).round(8)  # Int. Lons for 20 sec time steps, round 8 digits
        lats_20 = csy(n_time_20).round(8)  # Int. Lats for 20 sec time steps, round 8 digits

        # Put the interpolated data from one ships voyage in a single pd df
        ais_data = pd.DataFrame(data={'Time': n_time_20, 'Latitude': lats_20, 'Longitude': lons_20,
                                      'MMSI': ais_one_ship_voy.MMSI.values[0],
                                      'Voyage': ais_one_ship_voy.Voyage.values[0]})
        # Put the one ship voyage data of listed ship dimensions and draught in another pd df
        ais_voyages = ais_one_ship_voy[["MMSI", "Ship_type", "L1", "L2", "B1", "B2", "Voyage"]]
        ais_voyages = ais_voyages.drop_duplicates() # Drop duplicate rows from the df

        # Append if ship had more than one voyage passed the monitoring location
        if one_voy == ships_voys[0]:
            ais_data_l = ais_data.copy()  # Create a df to append first iteration data
            ais_voyages_l = ais_voyages.copy()  # Create a df to append first iteration data
        else:
            ais_data_l = ais_data_l.append(ais_data)  # Append to the df
            ais_voyages_l = ais_voyages_l.append(ais_voyages)  # Append to the df
    return ais_voyages_l, ais_data_l  # Return the pd df of ship positions and voyages data only

# locations = ['20', '21', '22', '23']
# ais_voyages_data, ais_int_data = read_interpolate_ais(locations[3])
