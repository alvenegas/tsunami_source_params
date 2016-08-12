# tsunami_source_params

ForTran code to compute tsunami source parameters.
Original F77 version

Given latitude, longitude and azimuth of rupture area 
reference position(i.e. hypocenter), plus the length 
and width of the rupture area, this program 
will compute the four corners of the tsunami source area
contributing to the tsunami.  The program will also ask
for the focal mechanism parameters (strike, dip, and rake).

Seismic moment in dyn-cm is computed given
an average slip (in meters) of the rupture, as well as 
inform of the corresponding Mw.

Program developed originally by Alberto M. López before major
upgrades and updates by Elizabeth Vanacore (Puerto Rico Seismic
Network).
