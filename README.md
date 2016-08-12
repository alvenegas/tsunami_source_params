# tsunami_source_params

ForTran code to compute tsunami source parameters.

Given latitude, longitude and azimuth of rupture area 
reference position(i.e. hypocenter), plus the length 
and width of the rupture area, this program 
will compute the four corners of the tsunami source area
contributing to the tsunami.  The program will also ask
for the focal mechanism parameters (strike, dip, and rake).

Seismic moment in both dyn-cm and N-m are computed given
an average slip (in meters) of the rupture, as well as 
inform of the corresponding Mw.

Program developed originally by Alberto M. López and edited
and updated by Elizabeth Vanacore (Puerto Rico Seismic Network)
before making public (August 12,2016).
