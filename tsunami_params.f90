      program tsunami_params
!
!     Alberto M. Lopez                              July 16, 2002     
!      
!     Program tsunamisourceparams   - Given lat long and azimuth of 
!     reference position of the source segment (hypocenter), plus the length (le) 
!    and width (wi) of the rupture area, this program 
!    will compute the four corners of the box.
!    Will also produce a file for plotting the rupture area on a map.
!    
!    Edited 07/2016 E.A. Vanacore 
!    1. Make code command line compatible
!    2. Update to Fortran 90
!    3. Added dip direction to assign corners (general logical test only! NEEDS
!    TESTING!!!!!!!)  
!    4. Changed output to a single file (previously 3)
!   Program notes add 07/2016 EAV
!  Make commands in Linux:
!         gfortran -c tsunami_params.f90 
!         gfortran -o {$BIN_DIRECTORY} tsunami_params tsunami_params.o 
!  The command line format: 
!  USAGE: tsunami_params [-E evlo/evla/evdp]  [-D length/width] 
!         [-F strike/dip/dip_direction] [-S ave_slip] ([-o output_file])
!         ([-h help_message]) 
!  [] Indicate variables that must be defined for a complete non-interactive run
!  ([]) Indicate help message calls or variables that have default values
!  
! Command line example:
!  Event location (lon, lat, depth) : -76.35 deg, 19.625 deg, 20 km
!  Fault Dimensions (length, width) : 270 km, 40 km
!  Fault Parameters (strike, dip, dip_direction): 270 deg, 70 deg, N
!  Ave Slip: 8 m 
!  Output file: Cuba_Tsunami_params.txt
!
!  Command Line Call for the above event
!  tsunami_params -E -76.35/19.625/20 -D 270/40 -F 270/70/N -S 8 -o Cuba_Tsunami_params.txt
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!       

      real :: mew, phi=-12345, theta, le=-12345, hle, wi=-12345
      real ::  hwi, alat=-12345, alon=-12345
      real :: xkmpdeg, pi, radeg, ctheta, stheta, dip, dipp=-12345
      real :: Mw, Modyn, MoN, S, D=-12345
      integer :: numcml, argcnt, dloc, dloc2
      character(len=2) :: flg
      character(len=20) :: argval, ofile='out.txt', tmpstr
      character(len=3) :: dipdir='NAN' 
      
!
!     Set up some constant values for calculations
      xkmpdeg=111.195
      pi=3.141592654
      radeg=pi/180.
      mew = 3.3e11
!
!      
!    Check for command line arguments, if no arguments use interactive
!    mode.
      numcml = iargc()
      if (numcml < 1) then
      write(6,*) 'Tsunami Source Parameters' 
      write(6,*) 'Enter lat, long, depth of hypocenter'
      read(5,*) alat, alon, adep 
      write(6,*) 'Enter rupture area information: '
      write(6,*) '1. length and width in km'
      read(5,*) le, wi
      write(6,*) '2. strike, dip in degrees and dip direction (e.g. S)'
      write(6,*) ' (Rake assumed to be 90 degrees for full frontal convergence)'
      read(5,*) phi,dipp, dipdir
      open (unit=12, file='tsumodel_info.txt')
      write(12,*) 'Box length (km): ',le
      write(12,*) '  width (km): ', wi
      write(12,*) '  strike: ', phi
      write(12,*) '  dip: ', dipp, dipdir
      write (6,*) 'Enter average slip in meters '
      read (5,*) D
      write(12,*) '  avg slip (m): ', D

      else 
!    Command line mode. Retrieve the arguments and assign the variables 
     argcnt = 1   ! Argument count initialization
      write(6,*) 'Using commandline arguments'
!    While arguments exist, retrieve, determine type and assign values
      do while (argcnt <= numcml)
      call getarg(argcnt,flg)
      argcnt = argcnt + 1
      call getarg(argcnt, argval)
      argval = trim(argval)
      select case (flg)
       case ('-E')
       dloc = scan (argval,'/')
       dloc2 = scan (argval,'/', .TRUE.)
       tmpstr = argval(1:dloc-1) 
       read(tmpstr,*) alon
       tmpstr = argval(dloc+1:dloc2-1)
       read(tmpstr,*) alat
        tmpstr = argval(dloc2+1:20)
       read(tmpstr,*) adep
       case ('-D')
       dloc = scan (argval,'/')
       tmpstr = argval(1:dloc-1)
       read(tmpstr,*) le
       tmpstr = argval(dloc+1:20)
       read(tmpstr,*) wi
       case ('-F')
       dloc = scan (argval,'/')
       dloc2 = scan (argval,'/', .TRUE.)
       tmpstr = argval(1:dloc-1)
       read(tmpstr,*) phi
       tmpstr = argval(dloc+1:dloc2-1)
       read(tmpstr,*) dipp
        tmpstr = argval(dloc2+1:20)
       read(tmpstr,*) dipdir
       dipdir = trim(dipdir)
       case ('-S')
       read(argval,*) D
       case ('-o') 
       read(argval,*) ofile
       open (unit=12, file=ofile)
       case ('-h') 
       write (6,*) 'USAGE: tsunami_params [-E evlo/evla/evdp] &
    &  [-D length/width] [-F strike/dip/dip_direction] [-S ave_slip] &
    &  [-o output_file] [-h help message]'
       stop
       end select
       argcnt = argcnt + 1
      end do
! Check if all parameters have command arguments
! If no value require user input or set output file to default
! Check Hypocenter 
       if(alon==-12345 .OR. alat==-12345 .OR. adep==-12345)then
        write (6,*) 'Event hypocenter not initialized.'
        write(6,*) 'Enter lat, long, depth of hypocenter'
        read(5,*) alat, alon, adep
       endif
! Check Fault Dimensions
       if(le==-12345 .OR. wi==-12345)then
        write (6,*) 'Event Fault dimensions not initialized.'
        write(6,*) 'Enter Fault length, width'
        read(5,*) le, wi 
       endif
! Check Fault Plane Info
       if(phi==-12345 .OR. dipp==-12345 .OR. dipdir=='NAN')then
        write (6,*) 'Event Fault Plane Information not initialized.'
        write(6,*) 'Enter Fault Plane Strike, dip in degrees, and dip direction'
        read(5,*) phi, dipp, dipdir
       endif
! Check Average Fault Slip
       if(D==-12345)then
        write (6,*) 'Event Fault Ave Slip not initialized.'
        write(6,*) 'Enter Ave Slip in km'
        read(5,*) D
       endif
! Check for output file name
      if ( ofile=='out.txt') then
       write (6,*) 'Output filename not initialized.'
       write (6,*) 'Output will be saved to tsumodel_info.txt'
       open (unit=12, file='tsumodel_info.txt') 
      endif
! End of INPUT for Command line or interactive options     
      endif 

! --  Compute apparent width of rupture area for map plotting:
      dip=dipp*radeg
      wap=wi*cos(dip)
      write(12,*) '  Apparent width (Wap): ', wap
      write(12,*) ' Hypocenter (lat,long,depth):', alat, alon, adep
!
! --  length (le), width (wi), half length (hle), half width (hwi)      
! --  half length based on apparent width for correct position of corner points.
       hle=le/2.
       hwi=wap/2.
!
! --  Estimate depths: updip limit of fault (sh) and downdip limit (dh)
!     variable fy will use adep (which is the hypocenter depth)
!     to find sh and dh by using tangent of the dip (here first converted to radians)
      fy=hwi*tan(dip)
      sh=adep-fy
      dh=adep+fy
!
!     Angle from horizontal (theta) and cosine and sine of theta
       thet=90.0-(phi)
       theta=thet*radeg 
       ctheta=cos(theta)
       stheta=sin(theta)
!       write(6,*) phi, thet, theta, ctheta, stheta
!
! --  Read longitude, latitude and depth of each event
!     Commented out 07/2016 EAV reduce output files
!       open (unit=11, file='box.xy')
!       open (unit=19, file='boxinfo.xy')
       
      conv=xkmpdeg*cos(alat*radeg)
!
! --  Make transformation to primed coordinate system that is 
!     rotated 90-phi degrees, which is the angle from the horizontal
!     to the strike of the rupture area.
!
! --  Find the corners of the box A, B, C, D in latitude & longitude
!  --  Corners A and D are at the downdip limit, whereas B and C are at the upper limit.
!  --  Corners start at the left inferior downdip corner of the rupture (A) and move clockwise
!  --  from there to the left superior updip corner (B), then to the right superior updip corner (C)
!  --  and finally back to the dip corner at the right inferior corner (D).  Hence A and D are deep, 
!  --  whereas B and C are shallow.

       ax=(((-hle)*ctheta - (-hwi)*stheta)/conv) + alon
       ay=(((-hle)*stheta + (-hwi)*ctheta)/xkmpdeg) + alat
       bx=(((-hle)*ctheta - hwi*stheta)/conv) + alon
       by=(((-hle)*stheta + hwi*ctheta)/xkmpdeg) + alat
       cx=((hle*ctheta - hwi*stheta)/conv) + alon
       cy=((hle*stheta + hwi*ctheta)/xkmpdeg) + alat
       dx=((hle*ctheta - (-hwi)*stheta)/conv) + alon
       dy=((hle*stheta + (-hwi)*ctheta)/xkmpdeg) + alat
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! ADDED EAV 07/2016
! Logic test to assign the corners based on the dip direction
! SEE ICG Tsunami Handout for details
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
       if(dipdir == 'S' .OR.  dipdir == 'SW'.OR. dipdir == 'SSW' .OR. &
    &    dipdir == 'SE' .OR. dipdir== 'SSE' .OR. dipdir== 'W' .OR. dipdir == 'WSW') then
!     Commented out 07/2016 EAV to reduce output files
!       write(19,*) 'A = (',ax,' ,',ay,' ,',dh,')'
!       write(19,*) 'B = (',bx,' ,',by,' ,',sh,')'
!       write(19,*) 'C = (',cx,' ,',cy,' ,',sh,')'
!       write(19,*) 'D = (',dx,' ,',dy,' ,',dh,')'
!       write(11,*) ax,ay
!       write(11,*) bx,by
!       write(11,*) cx,cy
!       write(11,*) dx,dy
! --  Write to STDOUT and file 12:
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
       else
!     Commented out 07/2016 EAV to reduce output files 
!       write(19,*) 'A = (',bx,' ,',by,' ,',dh,')'
!       write(19,*) 'B = (',ax,' ,',ay,' ,',sh,')'
!       write(19,*) 'C = (',dx,' ,',dy,' ,',sh,')'
!       write(19,*) 'D = (',cx,' ,',cy,' ,',dh,')'
!       write(11,*) bx,by
!       write(11,*) ax,ay
!       write(11,*) dx,dy
!       write(11,*) cx,cy
! --  Write to STDOUT and file 12:
       write(6,*) 'Corners of rupture area: (lon, lat, depth):'
       write(6,*) 'A = (',bx,' ,',by,' ,',dh,')'
       write(6,*) 'B = (',ax,' ,',ay,' ,',sh,')'
       write(6,*) 'C = (',dx,' ,',dy,' ,',sh,')'
       write(6,*) 'D = (',cx,' ,',cy,' ,',dh,')'
       write(12,*) 'Corners of rupture area: (lon, lat, depth):'
       write(12,*) 'A = (',bx,' ,',by,' ,',dh,')'
       write(12,*) 'B = (',ax,' ,',ay,' ,',sh,')'
       write(12,*) 'C = (',dx,' ,',dy,' ,',sh,')'
       write(12,*) 'D = (',cx,' ,',cy,' ,',dh,')'
       endif
!
!  --  Compute Mw and Mo:
!       write (6,*) 'Enter average slip in meters '
       write (6,*) ' Computing Mo (using a std rigidity of 3.3e11 dyne/cm²)'
       write (12,*) ' Computing Mo (using a std rigidity of 3.3e11 dyne/cm²)'
!               Converting Fault area from Km^2 to cm^2
         S = (le * wi) * 1e10
         Modyn = mew * S * (D * 100)
         MoN = Modyn * 1e-7
         Mw = (log10(Modyn) /  1.5) - 10.73
       write (6,13) Modyn, MoN
       write (12,13) Modyn, MoN
       write (6,14) Mw
       write (12,14) Mw
13       format (" Seismic Moment is",e13.4," dyn-cm   or  ",e13.4," N-m")
14       format (" Mw =",f5.2)

!       
!
      end program

