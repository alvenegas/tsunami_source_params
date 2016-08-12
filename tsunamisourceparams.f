c
c     Alberto M. Lopez                              July 16, 2002     
c      
c     Program tsunamisourceparams   - Given lat long and azimuth of 
c  	reference position of the source segment (hypocenter), plus the length (le) 
c	and width (wi) of the rupture area, this program 
c	will compute the four corners of the box.
c	Will also produce a file for plotting the rupture area on a map.
c
c      
      real mew, phi, theta, le, hle, wi, hwi, alat, alon
      real xkmpdeg, pi, radeg, ctheta, stheta, dip, dipp
      real Mw, Mo, S, D
c
      xkmpdeg=111.195
      pi=3.141592654
      radeg=pi/180.
      mew = 3.3e11
c
c      
      write(6,*) 'Tsunami Source Parameters' 
      write(6,*) 'Enter lat, long, depth of hypocenter'
      read(5,*) alat, alon, adep 
      write(6,*) 'Enter rupture area information: '
      write(6,*) '1. length and width in km'
      read(5,*) le, wi
      write(6,*) '2. strike and dip in degrees'
      read(5,*) phi,dipp
      open (unit=12, file='boxdimensions')
      write(12,*) 'Box length: ',le
      write(12,*) '  width: ', wi
      write(12,*) '  strike: ', phi
      write(12,*) '  dip: ', dipp
c --  Compute apparent width of rupture area for map plotting:
      dip=dipp*radeg
      wap=wi*cos(dip)
      write(12,*) '  Apparent width (Wap): ', wap
      write(12,*) ' Hypocenter (lat,long,depth):', alat, alon, adep
c
c --  length (le), width (wi), half length (hle), half width (hwi)      
c --  half length based on apparent width for correct position of corner points.
       hle=le/2.
       hwi=wap/2.
c
c --  Estimate depths: updip limit of fault (sh) and downdip limit (dh)
c     variable fy will use adep (which is the hypocenter depth)
c     to find sh and dh by using tangent of the dip (here first converted to radians)
      fy=hwi*tan(dip)
      sh=adep-fy
      dh=adep+fy
c
c     Angle from horizontal (theta) and cosine and sine of theta
       thet=90.0-(phi)
       theta=thet*radeg 
       ctheta=cos(theta)
       stheta=sin(theta)
       write(6,*) phi, thet, theta, ctheta, stheta
c
c --  Read longitude, latitude and depth of each event
       open (unit=11, file='box.xy')
       open (unit=19, file='boxinfo.xy')
       
11     conv=xkmpdeg*cos(alat*radeg)
c
c --  Make transformation to primed coordinate system that is 
c     rotated 90-phi degrees, which is the angle from the horizontal
c     to the strike of the rupture area.
c
c --  Find the corners of the box A, B, C, D in latitude & longitude
c  --  Corners A and D are at the downdip limit, whereas B and C are at the upper limit.
c  --  Corners start at the left inferior downdip corner of the rupture (A) and move clockwise
c  --  from there to the left superior updip corner (B), then to the right superior updip corner (C)
c  --  and finally back to the dip corner at the right inferior corner (D).  Hence A and D are deep, 
c  --  whereas B and C are shallow.
c
c       ax=((hle*ctheta - hwi*stheta)/conv) + alon
c       ay=((hle*stheta + hwi*ctheta)/xkmpdeg) + alat
c       bx=((hle*ctheta - (-hwi)*stheta)/conv) + alon
c       by=((hle*stheta + (-hwi)*ctheta)/xkmpdeg) + alat
c       cx=(((-hle)*ctheta - (-hwi)*stheta)/conv) + alon
c       cy=(((-hle)*stheta + (-hwi)*ctheta)/xkmpdeg) + alat
c       dx=(((-hle)*ctheta - hwi*stheta)/conv) + alon
c       dy=(((-hle)*stheta + hwi*ctheta)/xkmpdeg) + alat

c        c es a, d es b, a es c, b es d:

c antes era c
       ax=(((-hle)*ctheta - (-hwi)*stheta)/conv) + alon
       ay=(((-hle)*stheta + (-hwi)*ctheta)/xkmpdeg) + alat
c antes era d
       bx=(((-hle)*ctheta - hwi*stheta)/conv) + alon
       by=(((-hle)*stheta + hwi*ctheta)/xkmpdeg) + alat
c antes era a 
       cx=((hle*ctheta - hwi*stheta)/conv) + alon
       cy=((hle*stheta + hwi*ctheta)/xkmpdeg) + alat
c antes era b
       dx=((hle*ctheta - (-hwi)*stheta)/conv) + alon
       dy=((hle*stheta + (-hwi)*ctheta)/xkmpdeg) + alat


       write(19,*) 'A = (',ax,' ,',ay,' ,',dh,')'
       write(19,*) 'B = (',bx,' ,',by,' ,',sh,')'
       write(19,*) 'C = (',cx,' ,',cy,' ,',sh,')'
       write(19,*) 'D = (',dx,' ,',dy,' ,',dh,')'
       write(11,*) ax,ay
       write(11,*) bx,by
       write(11,*) cx,cy
       write(11,*) dx,dy
c --  Write to STDOUT and file 12:
       write(6,*) 'Corners of rupture area: (lon, lat, depth):'
       write(6,*) 'A = (',ax,' ,',ay,' ,',dh,')'
       write(6,*) 'B = (',bx,' ,',by,' ,',sh,')'
       write(6,*) 'C = (',cx,' ,',cy,' ,',sh,')'
       write(6,*) 'D = (',dx,' ,',dy,' ,',dh,')'
       write(12,*) 'Corners of rupture area: (lon, lat, depth):'
       write(12,*) 'A = (',ax,' ,',ay,' ,',dh,')'
       write(12,*) 'B = (',bx,' ,',by,' ,',sh,')'
       write(12,*) 'C = (',cx,' ,',cy,' ,',sh,')'
       write(12,*) 'D = (',dx,' ,',dy,' ,',dh,')'
c
c  --  Compute Mw and Mo:
       write (6,*) 'Enter average slip in meters '
       write (6,*) ' ( a std rigidity of 3.3e11 dyne/cm² will be used'
       read (5,*) D
c               Converting Fault area from Km^2 to cm^2
         S = (le * wi) * 1e10
         Mo = mew * S * (D * 100)
         Mw = (log10(Mo) /  1.5) - 10.73
       write (6,13) Mo, Mw
       write (12,13) Mo, Mw
13     format (" Seismic Moment: ",e13.4," dyn-cm   or  Mw =",f5.2)
       write(6,14) D
       write(12,14) D
14     format (" Avg slip:",f4.2," m")

c       
c
3     end
