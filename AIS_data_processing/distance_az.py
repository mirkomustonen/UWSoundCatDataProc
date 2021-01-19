#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Wed Jun  6 13:22:27 2018

@author: mirko (translated from some other code)
"""

def distance_az(lon_1, lat_1, lon_2, lat_2):
    import numpy as np
    '''
    DISTANCE_AZ Function to calculate distance, azimuth between geographic coorinates. The distance is calculated 
    by using the haversine formula to calculate the great-circle distance between two points that is, the 
    shortest distance over the earths surface giving an as-the-crow-flies distance between the points 
    (ignoring any hills they fly over, of course!). The azimuth is the forward azimuth. Which if followed 
    in a straight line along a great-circle arc will take you from the start point to the end point.

    INPUT:  
         lat_1 - latitude of the first coordinate in decimal degrees.
         lon_1 - longitude of the first coordinate in decimal degrees.
         lat_2 - latitude of the second coordinate in decimal degrees.
         lon_2 - longitude of the second coordinate in decimal degrees.

    OUTPUT: 
        1X2 list where 1X1 is the distance between the two geographic 
        points in km and 1X2 the azimuth between the two geographic points in degrees.
    '''
    lon_1, lat_1 = np.asarray(lon_1), np.asarray(lat_1)
    lon_2, lat_2 = np.asarray(lon_2), np.asarray(lat_2)
    # The haversine formula needs the input coordinates to be in radians
    lat_1r = (np.pi/180.)*(lat_1)
    lon_1r = (np.pi/180.)*(lon_1)
    lat_2r = (np.pi/180.)*(lat_2)
    lon_2r = (np.pi/180.)*(lon_2)
    d_lat = lat_2r-lat_1r
    d_lon = lon_2r-lon_1r 

    R = 6371. # Mean radius of the earth in km
    # For two points on a sphere, calculating the haversine of the central  angle between them
    a = np.sin(d_lat/2)**2 + np.cos(lat_1r) * np.cos(lat_2r)*(np.sin(d_lon/2)**2) # the haversine formula 
    # From the central angle, calculating the distance between the points
    c = 2 * np.arctan2(np.sqrt(a), np.sqrt(1-a)) 
    d = R * c # Distance in km

    # Calculating the forward azimuth between the two geographical points.
    az = np.arctan2(np.sin(d_lon) * np.cos(lat_2r), np.cos(lat_1r)*np.sin(lat_2r) - np.sin(lat_1r)*np.cos(lat_2r)*np.cos(d_lon))
    # Convert the azimuth from radians to degrees.    
    az = (180./np.pi)*(az)
    return d, az




def deg2rad(angle):
    from math import pi
    return angle*pi/180


def rad2deg(angle):
    from math import pi
    return angle*180/pi


def coords_dist_az(Latitude_1, Longitude_1, Distance, Azimuth):
#COORDS_DIST_AZ Function to calculate geographic coords. at azimuth and distance
#   Detailed explanation goes here

#   INPUT:  
#       Latitude_1 - latitude of the origin coordinate in decimal degrees.
#       Longitude_1 - longitude of the origin coordinate in decimal degrees.
#       Distance - distance to the second geographical point.
#       Azimuth - azimuth to the second geographical point.
#
#   OUTPUT: 
#       1X2 list where 1X1 "Latitude_2" is Latitude of the second 
#       geographical coordinate and 1X2 "Longitude_2" is the Longitude of
#       the second geographical coordinate.


    from math import pi, asin, sin, cos, atan2

    R = 6371.01  # mean radius of the Earth, in km
    Lat1 = (pi/180.)*(Latitude_1);  # Degrees to radians
    Lon1 = (pi/180.)*(Longitude_1); # Degrees to radians
    
    brng = (pi/180.)*(-Azimuth);  # Positive direction of the azimuthal angle is opposite of the bearing angle
    lat2 = asin(sin(Lat1)*cos(Distance/R) + cos(Lat1)*sin(Distance/R)*cos(brng));  # Formula to calculate latitude of a point certain distance and angle from the origin point
    lon2 = Lon1 + atan2(cos(Distance/R)-sin(Lat1)*sin(lat2), sin(brng)*sin(Distance/R)*cos(Lat1));  # Formula to calculate longitude of a point certain distance and angle from the origin point
    Latitude_2 = (180./pi)*lat2; # Convert radians back to degrees
    Longitude_2 = (180./pi)*lon2-90; # Convert radians back to degrees
    
    return Latitude_2, Longitude_2

def calc_sea_area_polygon(Latitude, Longitude):
    import numpy as np
    from shapely import geometry
    import shapefile
    
    pol = []
    for az in np.arange(-180,180,0.25):
        Lat_20, Lon_20 = coords_dist_az(Latitude, Longitude, 20.5, az)
        pol.append([round(Lon_20,8), round(Lat_20,8)])
    pol.append(pol[0])
    
    poly20 = geometry.Polygon(pol)
    sf=shapefile.Reader('water-polygons-split-4326/Baltic.shp')
    flag = 0
    for i in np.arange(sf.numRecords):
        
        poly=sf.shape(i).__geo_interface__
        sing_poly = geometry.shape(poly)
        
        int_poly = sing_poly.intersects(poly20)
        if int_poly:
            polyg = sing_poly.intersection(poly20)
            if flag == 0:
                big_pol = polyg
            else:
                big_pol = polyg.union(big_pol)
            flag += 1
    return big_pol


def calc_los_area_polygon(Latitude, Longitude):
    import numpy as np
    from shapely import geometry
    
    big_pol = calc_sea_area_polygon(Latitude, Longitude)
    if big_pol.geom_type == 'MultiPolygon':
        Polygons = list(big_pol)
        big_pol = Polygons[0]
    
    pol_lon, pol_lat = big_pol.exterior.coords.xy

    pol = []
    for i in np.arange(len(pol_lon)):
        line = geometry.LineString([(Longitude, Latitude),(pol_lon[i], pol_lat[i])])
        if line.intersection(big_pol).geom_type == 'LineString':
            pol.append([pol_lon[i], pol_lat[i]])
    
    los_poly = geometry.Polygon(pol)
    los_poly = big_pol.intersection(los_poly)
    return los_poly