MODULE module_llxy



































































































































   USE module_wrf_error

   INTEGER, PARAMETER :: HH=4, VV=5

   REAL, PARAMETER :: PI = 3.141592653589793
   REAL, PARAMETER :: OMEGA_E = 7.292e-5

   REAL, PARAMETER :: DEG_PER_RAD = 180./PI
   REAL, PARAMETER :: RAD_PER_DEG = PI/180.

   REAL, PARAMETER :: A_WGS84  = 6378137.
   REAL, PARAMETER :: B_WGS84  = 6356752.314
   REAL, PARAMETER :: RE_WGS84 = A_WGS84
   REAL, PARAMETER :: E_WGS84  = 0.081819192

   REAL, PARAMETER :: A_NAD83  = 6378137.
   REAL, PARAMETER :: RE_NAD83 = A_NAD83
   REAL, PARAMETER :: E_NAD83  = 0.0818187034

   REAL, PARAMETER :: EARTH_RADIUS_M = 6370000.
   REAL, PARAMETER :: EARTH_CIRC_M = 2.*PI*EARTH_RADIUS_M

   INTEGER, PUBLIC, PARAMETER  :: PROJ_LATLON = 0
   INTEGER, PUBLIC, PARAMETER  :: PROJ_LC = 1
   INTEGER, PUBLIC, PARAMETER  :: PROJ_PS = 2
   INTEGER, PUBLIC, PARAMETER  :: PROJ_PS_WGS84 = 102
   INTEGER, PUBLIC, PARAMETER  :: PROJ_MERC = 3
   INTEGER, PUBLIC, PARAMETER  :: PROJ_GAUSS = 4
   INTEGER, PUBLIC, PARAMETER  :: PROJ_CYL = 5
   INTEGER, PUBLIC, PARAMETER  :: PROJ_CASSINI = 6
   INTEGER, PUBLIC, PARAMETER  :: PROJ_ALBERS_NAD83 = 105
   INTEGER, PUBLIC, PARAMETER  :: PROJ_ROTLL = 203

   
   INTEGER, PRIVATE, PARAMETER :: HIGH = 8
 
   TYPE proj_info
 
      INTEGER          :: code     
      INTEGER          :: nlat     
                                   
      INTEGER          :: nlon     
                                   
      INTEGER          :: ixdim    
                                   
      INTEGER          :: jydim    
      INTEGER          :: stagger  
      REAL             :: phi      
                                   
      REAL             :: lambda   
                                   
      REAL             :: lat1     
      REAL             :: lon1     
      REAL             :: lat0     
      REAL             :: lon0     
      REAL             :: dx       
                                   
      REAL             :: dy       
                                   
      REAL             :: latinc   
      REAL             :: loninc   
                                   
      REAL             :: dlat     
      REAL             :: dlon     
      REAL             :: stdlon   
      REAL             :: truelat1 
      REAL             :: truelat2 
      REAL             :: hemi     
      REAL             :: cone     
      REAL             :: polei    
      REAL             :: polej    
      REAL             :: rsw      
      REAL             :: rebydx   
      REAL             :: knowni   
      REAL             :: knownj   
      REAL             :: re_m     
      REAL             :: rho0     
      REAL             :: nc       
      REAL             :: bigc     
      LOGICAL          :: init     
                                   
      LOGICAL          :: wrap     
                                   
      REAL, POINTER, DIMENSION(:) :: gauss_lat  
 
   END TYPE proj_info

 
 CONTAINS
 
 
   SUBROUTINE map_init(proj)
      
  
      IMPLICIT NONE
      TYPE(proj_info), INTENT(INOUT)  :: proj
  
      proj%code     = -999
      proj%lat1     = -999.9
      proj%lon1     = -999.9
      proj%lat0     = -999.9
      proj%lon0     = -999.9
      proj%dx       = -999.9
      proj%dy       = -999.9
      proj%latinc   = -999.9
      proj%loninc   = -999.9
      proj%stdlon   = -999.9
      proj%truelat1 = -999.9
      proj%truelat2 = -999.9
      proj%phi      = -999.9
      proj%lambda   = -999.9
      proj%ixdim    = -999
      proj%jydim    = -999
      proj%stagger  = HH
      proj%nlat     = 0
      proj%nlon     = 0
      proj%hemi     = 0.0
      proj%cone     = -999.9
      proj%polei    = -999.9
      proj%polej    = -999.9
      proj%rsw      = -999.9
      proj%knowni   = -999.9
      proj%knownj   = -999.9
      proj%re_m     = EARTH_RADIUS_M
      proj%init     = .FALSE.
      proj%wrap     = .FALSE.
      proj%rho0     = 0.
      proj%nc       = 0.
      proj%bigc     = 0.
      nullify(proj%gauss_lat)
   
   END SUBROUTINE map_init


   SUBROUTINE map_set(proj_code, proj, lat1, lon1, lat0, lon0, knowni, knownj, dx, latinc, &
                      loninc, stdlon, truelat1, truelat2, nlat, nlon, ixdim, jydim, &
                      stagger, phi, lambda, r_earth)
      
      
      
      
      
      
      
      
      
  
      IMPLICIT NONE
      
      
      INTEGER, INTENT(IN)               :: proj_code
      INTEGER, INTENT(IN), OPTIONAL     :: nlat
      INTEGER, INTENT(IN), OPTIONAL     :: nlon
      INTEGER, INTENT(IN), OPTIONAL     :: ixdim
      INTEGER, INTENT(IN), OPTIONAL     :: jydim
      INTEGER, INTENT(IN), OPTIONAL     :: stagger
      REAL, INTENT(IN), OPTIONAL        :: latinc
      REAL, INTENT(IN), OPTIONAL        :: loninc
      REAL, INTENT(IN), OPTIONAL        :: lat1
      REAL, INTENT(IN), OPTIONAL        :: lon1
      REAL, INTENT(IN), OPTIONAL        :: lat0
      REAL, INTENT(IN), OPTIONAL        :: lon0
      REAL, INTENT(IN), OPTIONAL        :: dx
      REAL, INTENT(IN), OPTIONAL        :: stdlon
      REAL, INTENT(IN), OPTIONAL        :: truelat1
      REAL, INTENT(IN), OPTIONAL        :: truelat2
      REAL, INTENT(IN), OPTIONAL        :: knowni
      REAL, INTENT(IN), OPTIONAL        :: knownj
      REAL, INTENT(IN), OPTIONAL        :: phi
      REAL, INTENT(IN), OPTIONAL        :: lambda
      REAL, INTENT(IN), OPTIONAL        :: r_earth
      TYPE(proj_info), INTENT(OUT)      :: proj

      INTEGER :: iter
      REAL :: dummy_lon1
      REAL :: dummy_lon0
      REAL :: dummy_stdlon
  
      
      IF ( proj_code == PROJ_LC ) THEN
         IF ( .NOT.PRESENT(truelat1) .OR. &
              .NOT.PRESENT(truelat2) .OR. &
              .NOT.PRESENT(lat1) .OR. &
              .NOT.PRESENT(lon1) .OR. &
              .NOT.PRESENT(knowni) .OR. &
              .NOT.PRESENT(knownj) .OR. &
              .NOT.PRESENT(stdlon) .OR. &
              .NOT.PRESENT(dx) ) THEN
            PRINT '(A,I2)', 'The following are mandatory parameters for projection code : ', proj_code
            PRINT '(A)', ' truelat1, truelat2, lat1, lon1, knowni, knownj, stdlon, dx'
            CALL wrf_error_fatal3("<stdin>",323,&
'MAP_INIT' )
         END IF
      ELSE IF ( proj_code == PROJ_PS ) THEN
         IF ( .NOT.PRESENT(truelat1) .OR. &
              .NOT.PRESENT(lat1) .OR. &
              .NOT.PRESENT(lon1) .OR. &
              .NOT.PRESENT(knowni) .OR. &
              .NOT.PRESENT(knownj) .OR. &
              .NOT.PRESENT(stdlon) .OR. &
              .NOT.PRESENT(dx) ) THEN
            PRINT '(A,I2)', 'The following are mandatory parameters for projection code : ', proj_code
            PRINT '(A)', ' truelat1, lat1, lon1, knonwi, knownj, stdlon, dx'
            CALL wrf_error_fatal3("<stdin>",336,&
'MAP_INIT' )
         END IF
      ELSE IF ( proj_code == PROJ_PS_WGS84 ) THEN
         IF ( .NOT.PRESENT(truelat1) .OR. &
              .NOT.PRESENT(lat1) .OR. &
              .NOT.PRESENT(lon1) .OR. &
              .NOT.PRESENT(knowni) .OR. &
              .NOT.PRESENT(knownj) .OR. &
              .NOT.PRESENT(stdlon) .OR. &
              .NOT.PRESENT(dx) ) THEN
            PRINT '(A,I2)', 'The following are mandatory parameters for projection code : ', proj_code
            PRINT '(A)', ' truelat1, lat1, lon1, knonwi, knownj, stdlon, dx'
            CALL wrf_error_fatal3("<stdin>",349,&
'MAP_INIT' )
         END IF
      ELSE IF ( proj_code == PROJ_ALBERS_NAD83 ) THEN
         IF ( .NOT.PRESENT(truelat1) .OR. &
              .NOT.PRESENT(truelat2) .OR. &
              .NOT.PRESENT(lat1) .OR. &
              .NOT.PRESENT(lon1) .OR. &
              .NOT.PRESENT(knowni) .OR. &
              .NOT.PRESENT(knownj) .OR. &
              .NOT.PRESENT(stdlon) .OR. &
              .NOT.PRESENT(dx) ) THEN
            PRINT '(A,I2)', 'The following are mandatory parameters for projection code : ', proj_code
            PRINT '(A)', ' truelat1, truelat2, lat1, lon1, knonwi, knownj, stdlon, dx'
            CALL wrf_error_fatal3("<stdin>",363,&
'MAP_INIT' )
         END IF
      ELSE IF ( proj_code == PROJ_MERC ) THEN
         IF ( .NOT.PRESENT(truelat1) .OR. &
              .NOT.PRESENT(lat1) .OR. &
              .NOT.PRESENT(lon1) .OR. &
              .NOT.PRESENT(knowni) .OR. &
              .NOT.PRESENT(knownj) .OR. &
              .NOT.PRESENT(dx) ) THEN
            PRINT '(A,I2)', 'The following are mandatory parameters for projection code : ', proj_code
            PRINT '(A)', ' truelat1, lat1, lon1, knowni, knownj, dx'
            CALL wrf_error_fatal3("<stdin>",375,&
'MAP_INIT' )
         END IF
      ELSE IF ( proj_code == PROJ_LATLON ) THEN
         IF ( .NOT.PRESENT(latinc) .OR. &
              .NOT.PRESENT(loninc) .OR. &
              .NOT.PRESENT(knowni) .OR. &
              .NOT.PRESENT(knownj) .OR. &
              .NOT.PRESENT(lat1) .OR. &
              .NOT.PRESENT(lon1) ) THEN
            PRINT '(A,I2)', 'The following are mandatory parameters for projection code : ', proj_code
            PRINT '(A)', ' latinc, loninc, knowni, knownj, lat1, lon1'
            CALL wrf_error_fatal3("<stdin>",387,&
'MAP_INIT' )
         END IF
      ELSE IF ( proj_code == PROJ_CYL ) THEN
         IF ( .NOT.PRESENT(latinc) .OR. &
              .NOT.PRESENT(loninc) .OR. &
              .NOT.PRESENT(stdlon) ) THEN
            PRINT '(A,I2)', 'The following are mandatory parameters for projection code : ', proj_code
            PRINT '(A)', ' latinc, loninc, stdlon'
            CALL wrf_error_fatal3("<stdin>",396,&
'MAP_INIT' )
         END IF
      ELSE IF ( proj_code == PROJ_CASSINI ) THEN
         IF ( .NOT.PRESENT(latinc) .OR. &
              .NOT.PRESENT(loninc) .OR. &
              .NOT.PRESENT(lat1) .OR. &
              .NOT.PRESENT(lon1) .OR. &
              .NOT.PRESENT(lat0) .OR. &
              .NOT.PRESENT(lon0) .OR. &
              .NOT.PRESENT(knowni) .OR. &
              .NOT.PRESENT(knownj) .OR. &
              .NOT.PRESENT(stdlon) ) THEN
            PRINT '(A,I2)', 'The following are mandatory parameters for projection code : ', proj_code
            PRINT '(A)', ' latinc, loninc, lat1, lon1, knowni, knownj, lat0, lon0, stdlon'
            CALL wrf_error_fatal3("<stdin>",411,&
'MAP_INIT' )
         END IF
      ELSE IF ( proj_code == PROJ_GAUSS ) THEN
         IF ( .NOT.PRESENT(nlat) .OR. &
              .NOT.PRESENT(lat1) .OR. &
              .NOT.PRESENT(lon1) .OR. &
              .NOT.PRESENT(loninc) ) THEN
            PRINT '(A,I2)', 'The following are mandatory parameters for projection code : ', proj_code
            PRINT '(A)', ' nlat, lat1, lon1, loninc'
            CALL wrf_error_fatal3("<stdin>",421,&
'MAP_INIT' )
         END IF
      ELSE IF ( proj_code == PROJ_ROTLL ) THEN
         IF ( .NOT.PRESENT(ixdim) .OR. &
              .NOT.PRESENT(jydim) .OR. &
              .NOT.PRESENT(phi) .OR. &
              .NOT.PRESENT(lambda) .OR. &
              .NOT.PRESENT(lat1) .OR. &
              .NOT.PRESENT(lon1) .OR. &
              .NOT.PRESENT(stagger) ) THEN
            PRINT '(A,I2)', 'The following are mandatory parameters for projection code : ', proj_code
            PRINT '(A)', ' ixdim, jydim, phi, lambda, lat1, lon1, stagger'
            CALL wrf_error_fatal3("<stdin>",434,&
'MAP_INIT' )
         END IF
      ELSE
         PRINT '(A,I2)', 'Unknown projection code: ', proj_code
         CALL wrf_error_fatal3("<stdin>",439,&
'MAP_INIT' )
      END IF
  
      
      IF ( PRESENT(lat1) ) THEN
         IF ( ABS(lat1) .GT. 90. ) THEN
            PRINT '(A)', 'Latitude of origin corner required as follows:'
            PRINT '(A)', '    -90N <= lat1 < = 90.N'
            CALL wrf_error_fatal3("<stdin>",448,&
'MAP_INIT' )
         ENDIF
      ENDIF
  
      IF ( PRESENT(lon1) ) THEN
         dummy_lon1 = lon1
         IF ( ABS(dummy_lon1) .GT. 180.) THEN
            iter = 0 
            DO WHILE (ABS(dummy_lon1) > 180. .AND. iter < 10)
               IF (dummy_lon1 < -180.) dummy_lon1 = dummy_lon1 + 360.
               IF (dummy_lon1 > 180.) dummy_lon1 = dummy_lon1 - 360.
               iter = iter + 1
            END DO
            IF (abs(dummy_lon1) > 180.) THEN
               PRINT '(A)', 'Longitude of origin required as follows:'
               PRINT '(A)', '   -180E <= lon1 <= 180W'
               CALL wrf_error_fatal3("<stdin>",465,&
'MAP_INIT' )
            ENDIF
         ENDIF
      ENDIF
  
      IF ( PRESENT(lon0) ) THEN
         dummy_lon0 = lon0
         IF ( ABS(dummy_lon0) .GT. 180.) THEN
            iter = 0 
            DO WHILE (ABS(dummy_lon0) > 180. .AND. iter < 10)
               IF (dummy_lon0 < -180.) dummy_lon0 = dummy_lon0 + 360.
               IF (dummy_lon0 > 180.) dummy_lon0 = dummy_lon0 - 360.
               iter = iter + 1
            END DO
            IF (abs(dummy_lon0) > 180.) THEN
               PRINT '(A)', 'Longitude of pole required as follows:'
               PRINT '(A)', '   -180E <= lon0 <= 180W'
               CALL wrf_error_fatal3("<stdin>",483,&
'MAP_INIT' )
            ENDIF
         ENDIF
      ENDIF
  
      IF ( PRESENT(dx) ) THEN
         IF ((dx .LE. 0.).AND.(proj_code .NE. PROJ_LATLON)) THEN
            PRINT '(A)', 'Require grid spacing (dx) in meters be positive'
            CALL wrf_error_fatal3("<stdin>",492,&
'MAP_INIT' )
         ENDIF
      ENDIF
  
      IF ( PRESENT(stdlon) ) THEN
         dummy_stdlon = stdlon
         IF ((ABS(dummy_stdlon) > 180.).AND.(proj_code /= PROJ_MERC)) THEN
            iter = 0 
            DO WHILE (ABS(dummy_stdlon) > 180. .AND. iter < 10)
               IF (dummy_stdlon < -180.) dummy_stdlon = dummy_stdlon + 360.
               IF (dummy_stdlon > 180.) dummy_stdlon = dummy_stdlon - 360.
               iter = iter + 1
            END DO
            IF (abs(dummy_stdlon) > 180.) THEN
               PRINT '(A)', 'Need orientation longitude (stdlon) as: '
               PRINT '(A)', '   -180E <= stdlon <= 180W' 
               CALL wrf_error_fatal3("<stdin>",509,&
'MAP_INIT' )
            ENDIF
         ENDIF
      ENDIF
  
      IF ( PRESENT(truelat1) ) THEN
         IF (ABS(truelat1).GT.90.) THEN
            PRINT '(A)', 'Set true latitude 1 for all projections'
            CALL wrf_error_fatal3("<stdin>",518,&
'MAP_INIT' )
         ENDIF
      ENDIF
     
      CALL map_init(proj) 
      proj%code  = proj_code
      IF ( PRESENT(lat1) )     proj%lat1     = lat1
      IF ( PRESENT(lon1) )     proj%lon1     = dummy_lon1
      IF ( PRESENT(lat0) )     proj%lat0     = lat0
      IF ( PRESENT(lon0) )     proj%lon0     = dummy_lon0
      IF ( PRESENT(latinc) )   proj%latinc   = latinc
      IF ( PRESENT(loninc) )   proj%loninc   = loninc
      IF ( PRESENT(knowni) )   proj%knowni   = knowni
      IF ( PRESENT(knownj) )   proj%knownj   = knownj
      IF ( PRESENT(dx) )       proj%dx       = dx
      IF ( PRESENT(stdlon) )   proj%stdlon   = dummy_stdlon
      IF ( PRESENT(truelat1) ) proj%truelat1 = truelat1
      IF ( PRESENT(truelat2) ) proj%truelat2 = truelat2
      IF ( PRESENT(nlat) )     proj%nlat     = nlat
      IF ( PRESENT(nlon) )     proj%nlon     = nlon
      IF ( PRESENT(ixdim) )    proj%ixdim    = ixdim
      IF ( PRESENT(jydim) )    proj%jydim    = jydim
      IF ( PRESENT(stagger) )  proj%stagger  = stagger
      IF ( PRESENT(phi) )      proj%phi      = phi
      IF ( PRESENT(lambda) )   proj%lambda   = lambda
      IF ( PRESENT(r_earth) )  proj%re_m     = r_earth
  
      IF ( PRESENT(dx) ) THEN 
         IF ( (proj_code == PROJ_LC) .OR. (proj_code == PROJ_PS) .OR. &
              (proj_code == PROJ_PS_WGS84) .OR. (proj_code == PROJ_ALBERS_NAD83) .OR. &
              (proj_code == PROJ_MERC) ) THEN
            proj%dx = dx
            IF (truelat1 .LT. 0.) THEN
               proj%hemi = -1.0 
            ELSE
               proj%hemi = 1.0
            ENDIF
            proj%rebydx = proj%re_m / dx
         ENDIF
      ENDIF

      pick_proj: SELECT CASE(proj%code)
  
         CASE(PROJ_PS)
            CALL set_ps(proj)

         CASE(PROJ_PS_WGS84)
            CALL set_ps_wgs84(proj)

         CASE(PROJ_ALBERS_NAD83)
            CALL set_albers_nad83(proj)
   
         CASE(PROJ_LC)
            IF (ABS(proj%truelat2) .GT. 90.) THEN
               proj%truelat2=proj%truelat1
            ENDIF
            CALL set_lc(proj)
      
         CASE (PROJ_MERC)
            CALL set_merc(proj)
      
         CASE (PROJ_LATLON)
   
         CASE (PROJ_GAUSS)
            CALL set_gauss(proj)
      
         CASE (PROJ_CYL)
            CALL set_cyl(proj)
      
         CASE (PROJ_CASSINI)
            CALL set_cassini(proj)
      
         CASE (PROJ_ROTLL)
     
      END SELECT pick_proj
      proj%init = .TRUE.

      RETURN

   END SUBROUTINE map_set


   SUBROUTINE latlon_to_ij(proj, lat, lon, i, j)
      
      
  
      IMPLICIT NONE
      TYPE(proj_info), INTENT(IN)          :: proj
      REAL, INTENT(IN)                     :: lat
      REAL, INTENT(IN)                     :: lon
      REAL, INTENT(OUT)                    :: i
      REAL, INTENT(OUT)                    :: j
  
      IF (.NOT.proj%init) THEN
         PRINT '(A)', 'You have not called map_set for this projection'
         CALL wrf_error_fatal3("<stdin>",614,&
'LATLON_TO_IJ' )
      ENDIF
  
      SELECT CASE(proj%code)
   
         CASE(PROJ_LATLON)
            CALL llij_latlon(lat,lon,proj,i,j)
   
         CASE(PROJ_MERC)
            CALL llij_merc(lat,lon,proj,i,j)
   
         CASE(PROJ_PS)
            CALL llij_ps(lat,lon,proj,i,j)

         CASE(PROJ_PS_WGS84)
            CALL llij_ps_wgs84(lat,lon,proj,i,j)
         
         CASE(PROJ_ALBERS_NAD83)
            CALL llij_albers_nad83(lat,lon,proj,i,j)
         
         CASE(PROJ_LC)
            CALL llij_lc(lat,lon,proj,i,j)
   
         CASE(PROJ_GAUSS)
            CALL llij_gauss(lat,lon,proj,i,j)
   
         CASE(PROJ_CYL)
            CALL llij_cyl(lat,lon,proj,i,j)

         CASE(PROJ_CASSINI)
            CALL llij_cassini(lat,lon,proj,i,j)

         CASE(PROJ_ROTLL)
            CALL llij_rotlatlon(lat,lon,proj,i,j)
   
         CASE DEFAULT
            PRINT '(A,I2)', 'Unrecognized map projection code: ', proj%code
            CALL wrf_error_fatal3("<stdin>",652,&
'LATLON_TO_IJ' )
    
      END SELECT

      RETURN

   END SUBROUTINE latlon_to_ij


   SUBROUTINE ij_to_latlon(proj, i, j, lat, lon)
      
      
  
      IMPLICIT NONE
      TYPE(proj_info),INTENT(IN)          :: proj
      REAL, INTENT(IN)                    :: i
      REAL, INTENT(IN)                    :: j
      REAL, INTENT(OUT)                   :: lat
      REAL, INTENT(OUT)                   :: lon
  
      IF (.NOT.proj%init) THEN
         PRINT '(A)', 'You have not called map_set for this projection'
         CALL wrf_error_fatal3("<stdin>",675,&
'IJ_TO_LATLON' )
      ENDIF
      SELECT CASE (proj%code)
  
         CASE (PROJ_LATLON)
            CALL ijll_latlon(i, j, proj, lat, lon)
   
         CASE (PROJ_MERC)
            CALL ijll_merc(i, j, proj, lat, lon)
   
         CASE (PROJ_PS)
            CALL ijll_ps(i, j, proj, lat, lon)

         CASE (PROJ_PS_WGS84)
            CALL ijll_ps_wgs84(i, j, proj, lat, lon)
   
         CASE (PROJ_ALBERS_NAD83)
            CALL ijll_albers_nad83(i, j, proj, lat, lon)
   
         CASE (PROJ_LC)
            CALL ijll_lc(i, j, proj, lat, lon)
   
         CASE (PROJ_CYL)
            CALL ijll_cyl(i, j, proj, lat, lon)
   
         CASE (PROJ_CASSINI)
            CALL ijll_cassini(i, j, proj, lat, lon)
   
         CASE (PROJ_ROTLL)
            CALL ijll_rotlatlon(i, j, proj, lat, lon)
   
         CASE DEFAULT
            PRINT '(A,I2)', 'Unrecognized map projection code: ', proj%code
            CALL wrf_error_fatal3("<stdin>",709,&
'IJ_TO_LATLON' )
  
      END SELECT
      RETURN
   END SUBROUTINE ij_to_latlon


   SUBROUTINE set_ps(proj)
      
      
      
      
      IMPLICIT NONE
   
      
      TYPE(proj_info), INTENT(INOUT)    :: proj
  
      
      REAL                              :: ala1
      REAL                              :: alo1
      REAL                              :: reflon
      REAL                              :: scale_top
  
      
      reflon = proj%stdlon + 90.
  
      
      scale_top = 1. + proj%hemi * SIN(proj%truelat1 * rad_per_deg)
  
      
      ala1 = proj%lat1 * rad_per_deg
      proj%rsw = proj%rebydx*COS(ala1)*scale_top/(1.+proj%hemi*SIN(ala1))
  
      
      alo1 = (proj%lon1 - reflon) * rad_per_deg
      proj%polei = proj%knowni - proj%rsw * COS(alo1)
      proj%polej = proj%knownj - proj%hemi * proj%rsw * SIN(alo1)

      RETURN

   END SUBROUTINE set_ps


   SUBROUTINE llij_ps(lat,lon,proj,i,j)
      
      
      
      
  
      IMPLICIT NONE
  
      
      REAL, INTENT(IN)               :: lat
      REAL, INTENT(IN)               :: lon
      TYPE(proj_info),INTENT(IN)     :: proj
  
      
      REAL, INTENT(OUT)              :: i 
      REAL, INTENT(OUT)              :: j 
  
      
      
      REAL                           :: reflon
      REAL                           :: scale_top
      REAL                           :: ala
      REAL                           :: alo
      REAL                           :: rm
  
      
    
      reflon = proj%stdlon + 90.
     
      
  
      scale_top = 1. + proj%hemi * SIN(proj%truelat1 * rad_per_deg)
  
      
      ala = lat * rad_per_deg
      rm = proj%rebydx * COS(ala) * scale_top/(1. + proj%hemi *SIN(ala))
      alo = (lon - reflon) * rad_per_deg
      i = proj%polei + rm * COS(alo)
      j = proj%polej + proj%hemi * rm * SIN(alo)
   
      RETURN

   END SUBROUTINE llij_ps


   SUBROUTINE ijll_ps(i, j, proj, lat, lon)
 
      
      
      
  
      IMPLICIT NONE
  
      
      REAL, INTENT(IN)                    :: i    
      REAL, INTENT(IN)                    :: j    
      TYPE (proj_info), INTENT(IN)        :: proj
      
      
      REAL, INTENT(OUT)                   :: lat     
      REAL, INTENT(OUT)                   :: lon     
  
      
      REAL                                :: reflon
      REAL                                :: scale_top
      REAL                                :: xx,yy
      REAL                                :: gi2, r2
      REAL                                :: arccos
  
      
  
      
      
      reflon = proj%stdlon + 90.
     
      
      scale_top = 1. + proj%hemi * SIN(proj%truelat1 * rad_per_deg)
  
      
      xx = i - proj%polei
      yy = (j - proj%polej) * proj%hemi
      r2 = xx**2 + yy**2
  
      
      IF (r2 .EQ. 0.) THEN 
         lat = proj%hemi * 90.
         lon = reflon
      ELSE
         gi2 = (proj%rebydx * scale_top)**2.
         lat = deg_per_rad * proj%hemi * ASIN((gi2-r2)/(gi2+r2))
         arccos = ACOS(xx/SQRT(r2))
         IF (yy .GT. 0) THEN
            lon = reflon + deg_per_rad * arccos
         ELSE
            lon = reflon - deg_per_rad * arccos
         ENDIF
      ENDIF
    
      
      IF (lon .GT. 180.) lon = lon - 360.
      IF (lon .LT. -180.) lon = lon + 360.

      RETURN
   
   END SUBROUTINE ijll_ps


   SUBROUTINE set_ps_wgs84(proj)
      
      
      
      

      IMPLICIT NONE
   
      
      TYPE(proj_info), INTENT(INOUT)    :: proj
  
      
      real :: h, mc, tc, t, rho

      h = proj%hemi

      mc = cos(h*proj%truelat1*rad_per_deg)/sqrt(1.0-(E_WGS84*sin(h*proj%truelat1*rad_per_deg))**2.0)
      tc = sqrt(((1.0-sin(h*proj%truelat1*rad_per_deg))/(1.0+sin(h*proj%truelat1*rad_per_deg)))* &
                (((1.0+E_WGS84*sin(h*proj%truelat1*rad_per_deg))/(1.0-E_WGS84*sin(h*proj%truelat1*rad_per_deg)))**E_WGS84 ))

      
      t = sqrt(((1.0-sin(h*proj%lat1*rad_per_deg))/(1.0+sin(h*proj%lat1*rad_per_deg)))* &
               (((1.0+E_WGS84*sin(h*proj%lat1*rad_per_deg))/(1.0-E_WGS84*sin(h*proj%lat1*rad_per_deg)) )**E_WGS84 ) )
      rho = h * (A_WGS84 / proj%dx) * mc * t / tc
      proj%polei = rho * sin((h*proj%lon1 - h*proj%stdlon)*rad_per_deg)
      proj%polej = -rho * cos((h*proj%lon1 - h*proj%stdlon)*rad_per_deg)

      RETURN

   END SUBROUTINE set_ps_wgs84


   SUBROUTINE llij_ps_wgs84(lat,lon,proj,i,j)
      
      
      
      
  
      IMPLICIT NONE
  
      
      REAL, INTENT(IN)               :: lat
      REAL, INTENT(IN)               :: lon
      REAL, INTENT(OUT)              :: i 
      REAL, INTENT(OUT)              :: j 
      TYPE(proj_info),INTENT(IN)     :: proj
  
      
      real :: h, mc, tc, t, rho

      h = proj%hemi

      mc = cos(h*proj%truelat1*rad_per_deg)/sqrt(1.0-(E_WGS84*sin(h*proj%truelat1*rad_per_deg))**2.0)
      tc = sqrt(((1.0-sin(h*proj%truelat1*rad_per_deg))/(1.0+sin(h*proj%truelat1*rad_per_deg)))* &
                (((1.0+E_WGS84*sin(h*proj%truelat1*rad_per_deg))/(1.0-E_WGS84*sin(h*proj%truelat1*rad_per_deg)))**E_WGS84 ))

      t = sqrt(((1.0-sin(h*lat*rad_per_deg))/(1.0+sin(h*lat*rad_per_deg))) * &
               (((1.0+E_WGS84*sin(h*lat*rad_per_deg))/(1.0-E_WGS84*sin(h*lat*rad_per_deg)))**E_WGS84))

      
      rho = (A_WGS84 / proj%dx) * mc * t / tc
      i = h *  rho * sin((h*lon - h*proj%stdlon)*rad_per_deg)
      j = h *(-rho)* cos((h*lon - h*proj%stdlon)*rad_per_deg)

      
      i = proj%knowni + (i - proj%polei)
      j = proj%knownj + (j - proj%polej)
  
      RETURN

   END SUBROUTINE llij_ps_wgs84


   SUBROUTINE ijll_ps_wgs84(i, j, proj, lat, lon)
 
      
      
      
  
      implicit none
  
      
      REAL, INTENT(IN)                    :: i    
      REAL, INTENT(IN)                    :: j    
      REAL, INTENT(OUT)                   :: lat     
      REAL, INTENT(OUT)                   :: lon     
      TYPE (proj_info), INTENT(IN)        :: proj

      
      real :: h, mc, tc, t, rho, x, y
      real :: chi, a, b, c, d

      h = proj%hemi
      x = (i - proj%knowni + proj%polei)
      y = (j - proj%knownj + proj%polej)

      mc = cos(h*proj%truelat1*rad_per_deg)/sqrt(1.0-(E_WGS84*sin(h*proj%truelat1*rad_per_deg))**2.0)
      tc = sqrt(((1.0-sin(h*proj%truelat1*rad_per_deg))/(1.0+sin(h*proj%truelat1*rad_per_deg))) * &
                (((1.0+E_WGS84*sin(h*proj%truelat1*rad_per_deg))/(1.0-E_WGS84*sin(h*proj%truelat1*rad_per_deg)))**E_WGS84 ))

      rho = sqrt((x*proj%dx)**2.0 + (y*proj%dx)**2.0)
      t = rho * tc / (A_WGS84 * mc) 

      lon = h*proj%stdlon*rad_per_deg + h*atan2(h*x,h*(-y))

      chi = PI/2.0-2.0*atan(t)
      a = 1./2.*E_WGS84**2. + 5./24.*E_WGS84**4. +  1./40.*E_WGS84**6.  +    73./2016.*E_WGS84**8.
      b =                     7./24.*E_WGS84**4. + 29./120.*E_WGS84**6. + 54113./40320.*E_WGS84**8.
      c =                                           7./30.*E_WGS84**6.  +    81./280.*E_WGS84**8.
      d =                                                                  4279./20160.*E_WGS84**8.

      lat = chi + sin(2.*chi)*(a + cos(2.*chi)*(b + cos(2.*chi)*(c + d*cos(2.*chi))))
      lat = h * lat

      lat = lat*deg_per_rad
      lon = lon*deg_per_rad

      RETURN
   
   END SUBROUTINE ijll_ps_wgs84


   SUBROUTINE set_albers_nad83(proj)
      
      
      
      

      IMPLICIT NONE
   
      
      TYPE(proj_info), INTENT(INOUT)    :: proj
  
      
      real :: h, m1, m2, q1, q2, theta, q, sinphi

      h = proj%hemi

      m1 = cos(h*proj%truelat1*rad_per_deg)/sqrt(1.0-(E_NAD83*sin(h*proj%truelat1*rad_per_deg))**2.0)
      m2 = cos(h*proj%truelat2*rad_per_deg)/sqrt(1.0-(E_NAD83*sin(h*proj%truelat2*rad_per_deg))**2.0)

      sinphi = sin(proj%truelat1*rad_per_deg)
      q1 = (1.0-E_NAD83**2.0) * &
           ((sinphi/(1.0-(E_NAD83*sinphi)**2.0)) - 1.0/(2.0*E_NAD83) * log((1.0-E_NAD83*sinphi)/(1.0+E_NAD83*sinphi)))

      sinphi = sin(proj%truelat2*rad_per_deg)
      q2 = (1.0-E_NAD83**2.0) * &
           ((sinphi/(1.0-(E_NAD83*sinphi)**2.0)) - 1.0/(2.0*E_NAD83) * log((1.0-E_NAD83*sinphi)/(1.0+E_NAD83*sinphi)))

      if (proj%truelat1 == proj%truelat2) then
         proj%nc = sin(proj%truelat1*rad_per_deg)
      else
         proj%nc = (m1**2.0 - m2**2.0) / (q2 - q1)
      end if

      proj%bigc = m1**2.0 + proj%nc*q1

      
      sinphi = sin(proj%lat1*rad_per_deg)
      q = (1.0-E_NAD83**2.0) * &
           ((sinphi/(1.0-(E_NAD83*sinphi)**2.0)) - 1.0/(2.0*E_NAD83) * log((1.0-E_NAD83*sinphi)/(1.0+E_NAD83*sinphi)))

      proj%rho0 = h * (A_NAD83 / proj%dx) * sqrt(proj%bigc - proj%nc * q) / proj%nc 
      theta = proj%nc*(proj%lon1 - proj%stdlon)*rad_per_deg

      proj%polei = proj%rho0 * sin(h*theta) 
      proj%polej = proj%rho0 - proj%rho0 * cos(h*theta)

      RETURN

   END SUBROUTINE set_albers_nad83


   SUBROUTINE llij_albers_nad83(lat,lon,proj,i,j)
      
      
      
      
  
      IMPLICIT NONE
  
      
      REAL, INTENT(IN)               :: lat
      REAL, INTENT(IN)               :: lon
      REAL, INTENT(OUT)              :: i 
      REAL, INTENT(OUT)              :: j 
      TYPE(proj_info),INTENT(IN)     :: proj
  
      
      real :: h, q, rho, theta, sinphi

      h = proj%hemi

      sinphi = sin(h*lat*rad_per_deg)

      
      q = (1.0-E_NAD83**2.0) * &
           ((sinphi/(1.0-(E_NAD83*sinphi)**2.0)) - 1.0/(2.0*E_NAD83) * log((1.0-E_NAD83*sinphi)/(1.0+E_NAD83*sinphi)))

      rho = h * (A_NAD83 / proj%dx) * sqrt(proj%bigc - proj%nc * q) / proj%nc
      theta = proj%nc * (h*lon - h*proj%stdlon)*rad_per_deg

      i = h*rho*sin(theta)
      j = h*proj%rho0 - h*rho*cos(theta)

      
      i = proj%knowni + (i - proj%polei)
      j = proj%knownj + (j - proj%polej)

      RETURN

   END SUBROUTINE llij_albers_nad83


   SUBROUTINE ijll_albers_nad83(i, j, proj, lat, lon)
 
      
      
      
  
      implicit none
  
      
      REAL, INTENT(IN)                    :: i    
      REAL, INTENT(IN)                    :: j    
      REAL, INTENT(OUT)                   :: lat     
      REAL, INTENT(OUT)                   :: lon     
      TYPE (proj_info), INTENT(IN)        :: proj

      
      real :: h, q, rho, theta, beta, x, y
      real :: a, b, c

      h = proj%hemi

      x = (i - proj%knowni + proj%polei)
      y = (j - proj%knownj + proj%polej)

      rho = sqrt(x**2.0 + (proj%rho0 - y)**2.0)
      theta = atan2(x, proj%rho0-y)

      q = (proj%bigc - (rho*proj%nc*proj%dx/A_NAD83)**2.0) / proj%nc

      beta = asin(q/(1.0 - log((1.0-E_NAD83)/(1.0+E_NAD83))*(1.0-E_NAD83**2.0)/(2.0*E_NAD83)))
      a = 1./3.*E_NAD83**2. + 31./180.*E_NAD83**4. + 517./5040.*E_NAD83**6.
      b =                     23./360.*E_NAD83**4. + 251./3780.*E_NAD83**6.
      c =                                            761./45360.*E_NAD83**6.

      lat = beta + a*sin(2.*beta) + b*sin(4.*beta) + c*sin(6.*beta)

      lat = h*lat*deg_per_rad
      lon = proj%stdlon + theta*deg_per_rad/proj%nc

      RETURN
   
   END SUBROUTINE ijll_albers_nad83


   SUBROUTINE set_lc(proj)
      
      
  
      IMPLICIT NONE
      
      TYPE(proj_info), INTENT(INOUT)     :: proj
  
      REAL                               :: arg
      REAL                               :: deltalon1
      REAL                               :: tl1r
      REAL                               :: ctl1r
  
      
      CALL lc_cone(proj%truelat1, proj%truelat2, proj%cone)
  
      
      
      deltalon1 = proj%lon1 - proj%stdlon
      IF (deltalon1 .GT. +180.) deltalon1 = deltalon1 - 360.
      IF (deltalon1 .LT. -180.) deltalon1 = deltalon1 + 360.
  
      
      tl1r = proj%truelat1 * rad_per_deg
      ctl1r = COS(tl1r)
  
      
      proj%rsw = proj%rebydx * ctl1r/proj%cone * &
             (TAN((90.*proj%hemi-proj%lat1)*rad_per_deg/2.) / &
              TAN((90.*proj%hemi-proj%truelat1)*rad_per_deg/2.))**proj%cone
  
      
      arg = proj%cone*(deltalon1*rad_per_deg)
      proj%polei = proj%hemi*proj%knowni - proj%hemi * proj%rsw * SIN(arg)
      proj%polej = proj%hemi*proj%knownj + proj%rsw * COS(arg)  
  
      RETURN

   END SUBROUTINE set_lc                             


   SUBROUTINE lc_cone(truelat1, truelat2, cone)
 
   
 
      IMPLICIT NONE
      
      
      REAL, INTENT(IN)             :: truelat1  
      REAL, INTENT(IN)             :: truelat2  
  
      
      REAL, INTENT(OUT)            :: cone
  
      
  
      
  
      
      
      
      
      
      IF (ABS(truelat1-truelat2) .GT. 0.1) THEN
         cone = ALOG10(COS(truelat1*rad_per_deg)) - &
                ALOG10(COS(truelat2*rad_per_deg))
         cone = cone /(ALOG10(TAN((45.0 - ABS(truelat1)/2.0) * rad_per_deg)) - &
                ALOG10(TAN((45.0 - ABS(truelat2)/2.0) * rad_per_deg)))        
      ELSE
         cone = SIN(ABS(truelat1)*rad_per_deg )  
      ENDIF

      RETURN

   END SUBROUTINE lc_cone


   SUBROUTINE ijll_lc( i, j, proj, lat, lon)
 
   
   
 
   
   
   
      IMPLICIT NONE
  
      
      REAL, INTENT(IN)              :: i        
      REAL, INTENT(IN)              :: j        
      TYPE(proj_info),INTENT(IN)    :: proj     
  
      
      REAL, INTENT(OUT)             :: lat      
      REAL, INTENT(OUT)             :: lon      
  
      
      REAL                          :: inew
      REAL                          :: jnew
      REAL                          :: r
      REAL                          :: chi,chi1,chi2
      REAL                          :: r2
      REAL                          :: xx
      REAL                          :: yy
  
      
  
      chi1 = (90. - proj%hemi*proj%truelat1)*rad_per_deg
      chi2 = (90. - proj%hemi*proj%truelat2)*rad_per_deg
  
      
      
      inew = proj%hemi * i
      jnew = proj%hemi * j
  
      
      xx = inew - proj%polei
      yy = proj%polej - jnew
      r2 = (xx*xx + yy*yy)
      r = SQRT(r2)/proj%rebydx
     
      
      IF (r2 .EQ. 0.) THEN
         lat = proj%hemi * 90.
         lon = proj%stdlon
      ELSE
         
         
         lon = proj%stdlon + deg_per_rad * ATAN2(proj%hemi*xx,yy)/proj%cone
         lon = MOD(lon+360., 360.)
   
         
         
         
         
           
         IF (chi1 .EQ. chi2) THEN
            chi = 2.0*ATAN( ( r/TAN(chi1) )**(1./proj%cone) * TAN(chi1*0.5) )
         ELSE
            chi = 2.0*ATAN( (r*proj%cone/SIN(chi1))**(1./proj%cone) * TAN(chi1*0.5)) 
         ENDIF
         lat = (90.0-chi*deg_per_rad)*proj%hemi
  
      ENDIF
  
      IF (lon .GT. +180.) lon = lon - 360.
      IF (lon .LT. -180.) lon = lon + 360.
 
      RETURN

   END SUBROUTINE ijll_lc


   SUBROUTINE llij_lc( lat, lon, proj, i, j)
 
   
   
     
      IMPLICIT NONE
  
      
      REAL, INTENT(IN)              :: lat      
      REAL, INTENT(IN)              :: lon      
      TYPE(proj_info),INTENT(IN)      :: proj     
  
      
      REAL, INTENT(OUT)             :: i        
      REAL, INTENT(OUT)             :: j        
  
      
      REAL                          :: arg
      REAL                          :: deltalon
      REAL                          :: tl1r
      REAL                          :: rm
      REAL                          :: ctl1r
      
  
      
      
      
      
      deltalon = lon - proj%stdlon
      IF (deltalon .GT. +180.) deltalon = deltalon - 360.
      IF (deltalon .LT. -180.) deltalon = deltalon + 360.
      
      
      tl1r = proj%truelat1 * rad_per_deg
      ctl1r = COS(tl1r)     
     
      
      rm = proj%rebydx * ctl1r/proj%cone * &
           (TAN((90.*proj%hemi-lat)*rad_per_deg/2.) / &
            TAN((90.*proj%hemi-proj%truelat1)*rad_per_deg/2.))**proj%cone
  
      arg = proj%cone*(deltalon*rad_per_deg)
      i = proj%polei + proj%hemi * rm * SIN(arg)
      j = proj%polej - rm * COS(arg)
  
      
      
      
      
      
      i = proj%hemi * i  
      j = proj%hemi * j

      RETURN
   END SUBROUTINE llij_lc


   SUBROUTINE set_merc(proj)
   
      
  
      IMPLICIT NONE
      TYPE(proj_info), INTENT(INOUT)       :: proj
      REAL                                 :: clain
  
  
      
  
      clain = COS(rad_per_deg*proj%truelat1)
      proj%dlon = proj%dx / (proj%re_m * clain)
  
      
      
  
      proj%rsw = 0.
      IF (proj%lat1 .NE. 0.) THEN
         proj%rsw = (ALOG(TAN(0.5*((proj%lat1+90.)*rad_per_deg))))/proj%dlon
      ENDIF

      RETURN

   END SUBROUTINE set_merc


   SUBROUTINE llij_merc(lat, lon, proj, i, j)
 
      
    
      IMPLICIT NONE
      REAL, INTENT(IN)              :: lat
      REAL, INTENT(IN)              :: lon
      TYPE(proj_info),INTENT(IN)    :: proj
      REAL,INTENT(OUT)              :: i
      REAL,INTENT(OUT)              :: j
      REAL                          :: deltalon
  
      deltalon = lon - proj%lon1
      IF (deltalon .LT. -180.) deltalon = deltalon + 360.
      IF (deltalon .GT. 180.) deltalon = deltalon - 360.
      i = proj%knowni + (deltalon/(proj%dlon*deg_per_rad))
      j = proj%knownj + (ALOG(TAN(0.5*((lat + 90.) * rad_per_deg)))) / &
             proj%dlon - proj%rsw
  
      RETURN

   END SUBROUTINE llij_merc


   SUBROUTINE ijll_merc(i, j, proj, lat, lon)
 
      
  
      IMPLICIT NONE
      REAL,INTENT(IN)               :: i
      REAL,INTENT(IN)               :: j    
      TYPE(proj_info),INTENT(IN)    :: proj
      REAL, INTENT(OUT)             :: lat
      REAL, INTENT(OUT)             :: lon 
  
  
      lat = 2.0*ATAN(EXP(proj%dlon*(proj%rsw + j-proj%knownj)))*deg_per_rad - 90.
      lon = (i-proj%knowni)*proj%dlon*deg_per_rad + proj%lon1
      IF (lon.GT.180.) lon = lon - 360.
      IF (lon.LT.-180.) lon = lon + 360.
      RETURN

   END SUBROUTINE ijll_merc


   SUBROUTINE llij_latlon(lat, lon, proj, i, j)
  
      
      IMPLICIT NONE
      REAL, INTENT(IN)             :: lat
      REAL, INTENT(IN)             :: lon
      TYPE(proj_info), INTENT(IN)  :: proj
      REAL, INTENT(OUT)            :: i
      REAL, INTENT(OUT)            :: j
  
      REAL                         :: deltalat
      REAL                         :: deltalon
  
      
      
      deltalat = lat - proj%lat1
      deltalon = lon - proj%lon1      
      
      
      i = deltalon/proj%loninc
      j = deltalat/proj%latinc

      i = i + proj%knowni
      j = j + proj%knownj
  
      RETURN

   END SUBROUTINE llij_latlon


   SUBROUTINE ijll_latlon(i, j, proj, lat, lon)
  
      
      IMPLICIT NONE
      REAL, INTENT(IN)             :: i
      REAL, INTENT(IN)             :: j
      TYPE(proj_info), INTENT(IN)  :: proj
      REAL, INTENT(OUT)            :: lat
      REAL, INTENT(OUT)            :: lon
  
      REAL                         :: i_work, j_work
      REAL                         :: deltalat
      REAL                         :: deltalon
  
      i_work = i - proj%knowni
      j_work = j - proj%knownj

      
      deltalat = j_work*proj%latinc
      deltalon = i_work*proj%loninc
  
      lat = proj%lat1 + deltalat
      lon = proj%lon1 + deltalon
  
      RETURN

   END SUBROUTINE ijll_latlon


   SUBROUTINE set_cyl(proj)

      implicit none

      
      type(proj_info), intent(inout) :: proj

      proj%hemi = 1.0

   END SUBROUTINE set_cyl


   SUBROUTINE llij_cyl(lat, lon, proj, i, j)

      implicit none
    
      
      real, intent(in) :: lat, lon
      real, intent(out) :: i, j
      type(proj_info), intent(in) :: proj

      
      real :: deltalat
      real :: deltalon

      
      
      deltalat = lat - proj%lat1

      deltalon = lon - proj%lon1

      if (deltalon <   0.) deltalon = deltalon + 360.
      if (deltalon > 360.) deltalon = deltalon - 360.

      
      i = deltalon/proj%loninc
      j = deltalat/proj%latinc

      if (i <= 0.)              i = i + 360./proj%loninc
      if (i > 360./proj%loninc) i = i - 360./proj%loninc

      i = i + proj%knowni
      j = j + proj%knownj

   END SUBROUTINE llij_cyl


   SUBROUTINE ijll_cyl(i, j, proj, lat, lon)
   
      implicit none
    
      
      real, intent(in) :: i, j
      real, intent(out) :: lat, lon
      type(proj_info), intent(in) :: proj

      
      real :: deltalat
      real :: deltalon
      real :: i_work, j_work

      i_work = i - proj%knowni 
      j_work = j - proj%knownj

      if (i_work < 0.)              i_work = i_work + 360./proj%loninc
      if (i_work >= 360./proj%loninc) i_work = i_work - 360./proj%loninc

      
      deltalat = j_work*proj%latinc
      deltalon = i_work*proj%loninc

      lat = deltalat + proj%lat1

      lon = deltalon + proj%lon1

      if (lon < -180.) lon = lon + 360.
      if (lon >  180.) lon = lon - 360.

   END SUBROUTINE ijll_cyl


   SUBROUTINE set_cassini(proj)

      implicit none

      
      type(proj_info), intent(inout) :: proj

      
      real :: comp_lat, comp_lon
      logical :: global_domain

      proj%hemi = 1.0

      
      if (abs(proj%lat1 - proj%latinc/2. + 90.) < 0.001 .and. &
          abs(mod(proj%lon1 - proj%loninc/2. - proj%stdlon,360.)) < 0.001) then
         global_domain = .true.
      else
         global_domain = .false.
      end if

      if (abs(proj%lat0) /= 90. .and. .not.global_domain) then
         call rotate_coords(proj%lat1,proj%lon1,comp_lat,comp_lon,proj%lat0,proj%lon0,proj%stdlon,-1)
         proj%lat1 = comp_lat
         proj%lon1 = comp_lon
      end if

   END SUBROUTINE set_cassini


   SUBROUTINE llij_cassini(lat, lon, proj, i, j)

      implicit none
    
      
      real, intent(in) :: lat, lon
      real, intent(out) :: i, j
      type(proj_info), intent(in) :: proj

      
      real :: comp_lat, comp_lon

      
      if (abs(proj%lat0) /= 90.) then
         call rotate_coords(lat,lon,comp_lat,comp_lon,proj%lat0,proj%lon0,proj%stdlon,-1)
      else
         comp_lat = lat
         comp_lon = lon
      end if

      
      call llij_cyl(comp_lat, comp_lon, proj, i, j)

   END SUBROUTINE llij_cassini


   SUBROUTINE ijll_cassini(i, j, proj, lat, lon)
   
      implicit none
    
      
      real, intent(in) :: i, j
      real, intent(out) :: lat, lon
      type(proj_info), intent(in) :: proj

      
      real :: comp_lat, comp_lon

      
      call ijll_cyl(i, j, proj, comp_lat, comp_lon)

      
      if (abs(proj%lat0) /= 90.) then
         call rotate_coords(comp_lat,comp_lon,lat,lon,proj%lat0,proj%lon0,proj%stdlon,1)
      else
         lat = comp_lat
         lon = comp_lon
      end if

   END SUBROUTINE ijll_cassini


   
   
   
   
   
   SUBROUTINE rotate_coords(ilat,ilon,olat,olon,lat_np,lon_np,lon_0,direction)

      IMPLICIT NONE

      REAL, INTENT(IN   ) :: ilat, ilon
      REAL, INTENT(  OUT) :: olat, olon
      REAL, INTENT(IN   ) :: lat_np, lon_np, lon_0
      INTEGER, INTENT(IN  ), OPTIONAL :: direction
      
      

      REAL :: rlat, rlon
      REAL :: phi_np, lam_np, lam_0, dlam
      REAL :: sinphi, cosphi, coslam, sinlam

      
      phi_np = lat_np * rad_per_deg
      lam_np = lon_np * rad_per_deg
      lam_0  = lon_0  * rad_per_deg
      rlat = ilat * rad_per_deg
      rlon = ilon * rad_per_deg

      IF ( PRESENT(direction) ) THEN
         IF (direction < 0) THEN
            
            
            dlam = PI - lam_0
         ELSE
            dlam = lam_np
         END IF
      ELSE
         dlam = lam_np
      END IF
      sinphi = COS(phi_np)*COS(rlat)*COS(rlon-dlam) + SIN(phi_np)*SIN(rlat)
      cosphi = SQRT(1.-sinphi*sinphi)
      coslam = SIN(phi_np)*COS(rlat)*COS(rlon-dlam) - COS(phi_np)*SIN(rlat)
      sinlam = COS(rlat)*SIN(rlon-dlam)
      IF ( cosphi /= 0. ) THEN
         coslam = coslam/cosphi
         sinlam = sinlam/cosphi
      END IF
      olat = deg_per_rad*ASIN(sinphi)
      olon = deg_per_rad*(ATAN2(sinlam,coslam)-dlam-lam_0+lam_np)
      
      
      DO
         IF (olon >= -180.) EXIT
         olon = olon + 360.
      END DO
      DO
         IF (olon <=  180.) EXIT
         olon = olon - 360.
      END DO

   END SUBROUTINE rotate_coords


   SUBROUTINE llij_rotlatlon(lat, lon, proj, i_real, j_real)
   
      IMPLICIT NONE
    
      
      REAL, INTENT(IN) :: lat, lon
      REAL             :: i, j
      REAL, INTENT(OUT) :: i_real, j_real
      TYPE (proj_info), INTENT(IN) :: proj
      
      
      INTEGER :: ii,imt,jj,jmt,k,krows,ncol,nrow,iri
      REAL(KIND=HIGH) :: dphd,dlmd 
      REAL(KIND=HIGH) :: glatd  
      REAL(KIND=HIGH) :: glond  
      REAL(KIND=HIGH) :: col,d1,d2,d2r,dlm,dlm1,dlm2,dph,glat,glon,    &
                         pi,r2d,row,tlat,tlat1,tlat2,              &
                         tlon,tlon1,tlon2,tph0,tlm0,x,y,z

      glatd = lat
      glond = -lon
  
      dphd = proj%phi/REAL((proj%jydim-1)/2)
      dlmd = proj%lambda/REAL(proj%ixdim-1)

      pi = ACOS(-1.0)
      d2r = pi/180.
      r2d = 1./d2r
  
      imt = 2*proj%ixdim-1
      jmt = proj%jydim/2+1

      glat = glatd*d2r
      glon = glond*d2r
      dph = dphd*d2r
      dlm = dlmd*d2r
      tph0 = proj%lat1*d2r
      tlm0 = -proj%lon1*d2r
  
      x = COS(tph0)*COS(glat)*COS(glon-tlm0)+SIN(tph0)*SIN(glat)
      y = -COS(glat)*SIN(glon-tlm0)
      z = COS(tph0)*SIN(glat)-SIN(tph0)*COS(glat)*COS(glon-tlm0)
      tlat = r2d*ATAN(z/SQRT(x*x+y*y))
      tlon = r2d*ATAN(y/x)

      row = tlat/dphd+jmt
      col = tlon/dlmd+proj%ixdim

      if ( (row - INT(row)) .gt. 0.999) then
         row = row + 0.0002
      else if ( (col - INT(col)) .gt. 0.999) then
         col = col + 0.0002
      end if

      nrow = INT(row)
      ncol = INT(col)




      tlat = tlat*d2r
      tlon = tlon*d2r

  
      IF (proj%stagger == HH) THEN

         IF (mod(nrow,2) .eq. 0) then
            i_real = col / 2.0
         ELSE
            i_real = col / 2.0 + 0.5
         ENDIF
         j_real=row

  
         IF ((abs(MOD(nrow,2)) == 1 .AND. abs(MOD(ncol,2)) == 1) .OR. &
             (MOD(nrow,2) == 0 .AND. MOD(ncol,2) == 0)) THEN

            tlat1 = (nrow-jmt)*dph
            tlat2 = tlat1+dph
            tlon1 = (ncol-proj%ixdim)*dlm
            tlon2 = tlon1+dlm

            dlm1 = tlon-tlon1
            dlm2 = tlon-tlon2
            d1 = ACOS(COS(tlat)*COS(tlat1)*COS(dlm1)+SIN(tlat)*SIN(tlat1))
            d2 = ACOS(COS(tlat)*COS(tlat2)*COS(dlm2)+SIN(tlat)*SIN(tlat2))

            IF (d1 > d2) THEN
               nrow = nrow+1
               ncol = ncol+1
            END IF
   
         ELSE
            tlat1 = (nrow+1-jmt)*dph
            tlat2 = tlat1-dph
            tlon1 = (ncol-proj%ixdim)*dlm
            tlon2 = tlon1+dlm
            dlm1 = tlon-tlon1
            dlm2 = tlon-tlon2
            d1 = ACOS(COS(tlat)*COS(tlat1)*COS(dlm1)+SIN(tlat)*SIN(tlat1))
            d2 = ACOS(COS(tlat)*COS(tlat2)*COS(dlm2)+SIN(tlat)*SIN(tlat2))

            IF (d1 < d2) THEN
               nrow = nrow+1
            ELSE
               ncol = ncol+1
            END IF
         END IF
  
      ELSE IF (proj%stagger == VV) THEN

         IF (mod(nrow,2) .eq. 0) then
            i_real = col / 2.0 + 0.5
         ELSE
            i_real = col / 2.0 
         ENDIF
         j_real=row
  
         IF ((MOD(nrow,2) == 0 .AND. abs(MOD(ncol,2)) == 1) .OR. &
             (abs(MOD(nrow,2)) == 1 .AND. MOD(ncol,2) == 0)) THEN
            tlat1 = (nrow-jmt)*dph
            tlat2 = tlat1+dph
            tlon1 = (ncol-proj%ixdim)*dlm
            tlon2 = tlon1+dlm
            dlm1 = tlon-tlon1
            dlm2 = tlon-tlon2
            d1 = ACOS(COS(tlat)*COS(tlat1)*COS(dlm1)+SIN(tlat)*SIN(tlat1))
            d2 = ACOS(COS(tlat)*COS(tlat2)*COS(dlm2)+SIN(tlat)*SIN(tlat2))
    
            IF (d1 > d2) THEN
               nrow = nrow+1
               ncol = ncol+1
            END IF
   
         ELSE
            tlat1 = (nrow+1-jmt)*dph
            tlat2 = tlat1-dph
            tlon1 = (ncol-proj%ixdim)*dlm
            tlon2 = tlon1+dlm
            dlm1 = tlon-tlon1
            dlm2 = tlon-tlon2
            d1 = ACOS(COS(tlat)*COS(tlat1)*COS(dlm1)+SIN(tlat)*SIN(tlat1))
            d2 = ACOS(COS(tlat)*COS(tlat2)*COS(dlm2)+SIN(tlat)*SIN(tlat2))
    
            IF (d1 < d2) THEN
               nrow = nrow+1
            ELSE
               ncol = ncol+1
            END IF
         END IF
      END IF
  


      if (ncol .le. 0) ncol=ncol-1

      jj = nrow
      ii = ncol/2

      IF (proj%stagger == HH) THEN
         IF (abs(MOD(jj,2)) == 1) ii = ii+1
      ELSE IF (proj%stagger == VV) THEN
         IF (MOD(jj,2) == 0) ii=ii+1
      END IF

      i = REAL(ii)
      j = REAL(jj)

   END SUBROUTINE llij_rotlatlon


   SUBROUTINE ijll_rotlatlon(i, j, proj, lat,lon)
   
      IMPLICIT NONE
    
      
      REAL, INTENT(IN) :: i, j
      REAL, INTENT(OUT) :: lat, lon
      TYPE (proj_info), INTENT(IN) :: proj
      
      
      INTEGER :: ih,jh
      INTEGER :: midcol,midrow,ncol,iadd1,iadd2,imt,jh2,knrow,krem,kv,nrow
      REAL :: i_work, j_work
      REAL :: dphd,dlmd 
      REAL(KIND=HIGH) :: arg1,arg2,d2r,fctr,glatr,glatd,glond,pi, &
              r2d,tlatd,tlond,tlatr,tlonr,tlm0,tph0
      REAL :: col

      i_work = i
      j_work = j
  
      if ( (j - INT(j)) .gt. 0.999) then
         j_work = j + 0.0002
      endif

      jh = INT(j_work)
  
      dphd = proj%phi/REAL((proj%jydim-1)/2)
      dlmd = proj%lambda/REAL(proj%ixdim-1)
    
      pi = ACOS(-1.0)
      d2r = pi/180.
      r2d = 1./d2r
      tph0 = proj%lat1*d2r
      tlm0 = -proj%lon1*d2r

      midrow = (proj%jydim+1)/2
      midcol = proj%ixdim

      col = 2*i_work-1+abs(MOD(jh+1,2))
      tlatd = (j_work-midrow)*dphd
      tlond = (col-midcol)*dlmd

       IF (proj%stagger == VV) THEN
          if (mod(jh,2) .eq. 0) then
             tlond = tlond - DLMD
          else
             tlond = tlond + DLMD
          end if
       END IF
    
      tlatr = tlatd*d2r
      tlonr = tlond*d2r
      arg1 = SIN(tlatr)*COS(tph0)+COS(tlatr)*SIN(tph0)*COS(tlonr)
      glatr = ASIN(arg1)
     
      glatd = glatr*r2d
     
      arg2 = COS(tlatr)*COS(tlonr)/(COS(glatr)*COS(tph0))-TAN(glatr)*TAN(tph0)
      IF (ABS(arg2) > 1.) arg2 = ABS(arg2)/arg2
      fctr = 1.
      IF (tlond > 0.) fctr = -1.
     
      glond = tlm0*r2d+fctr*ACOS(arg2)*r2d

      lat = glatd
      lon = -glond

      IF (lon .GT. +180.) lon = lon - 360.
      IF (lon .LT. -180.) lon = lon + 360.
   
   END SUBROUTINE ijll_rotlatlon


   SUBROUTINE set_gauss(proj)
    
      IMPLICIT NONE
 
      
      type (proj_info), intent(inout) :: proj
 
      
 
      IF ( ASSOCIATED( proj%gauss_lat ) ) THEN
         DEALLOCATE ( proj%gauss_lat )
      END IF
 
      
 
      ALLOCATE ( proj%gauss_lat(proj%nlat*2) )
 
      
 
      CALL gausll( proj%nlat*2 , proj%gauss_lat )
 
      
      
      
 
      IF ( ABS(proj%gauss_lat(1) - proj%lat1) .GT. 0.01 ) THEN
         proj%gauss_lat = -1. * proj%gauss_lat
      END IF
 
      
 
      IF ( ABS(proj%gauss_lat(1) - proj%lat1) .GT. 0.01 ) THEN
         PRINT '(A)','Oops, something is not right with the Gaussian latitude computation.'
         PRINT '(A,F8.3,A)','The input data gave the starting latitude as ',proj%lat1,'.'
         PRINT '(A,F8.3,A)','This routine computed the starting latitude as +-',ABS(proj%gauss_lat(1)),'.'
         PRINT '(A,F8.3,A)','The difference is larger than 0.01 degrees, which is not expected.'
         CALL wrf_error_fatal3("<stdin>",1965,&
'Gaussian_latitude_computation' )
      END IF
 
   END SUBROUTINE set_gauss


   SUBROUTINE gausll ( nlat , lat_sp )
 
      IMPLICIT NONE
   
      INTEGER                            :: nlat , i
      REAL (KIND=HIGH) , PARAMETER       :: pi = 3.141592653589793
      REAL (KIND=HIGH) , DIMENSION(nlat) :: cosc , gwt , sinc , colat , wos2 , lat
      REAL             , DIMENSION(nlat) :: lat_sp
   
      CALL lggaus(nlat, cosc, gwt, sinc, colat, wos2)
   
      DO i = 1, nlat
         lat(i) = ACOS(sinc(i)) * 180._HIGH / pi
         IF (i.gt.nlat/2) lat(i) = -lat(i)
      END DO
   
      lat_sp = REAL(lat)
 
   END SUBROUTINE gausll


   SUBROUTINE lggaus( nlat, cosc, gwt, sinc, colat, wos2 )
 
      IMPLICIT NONE
 
      
      
      
      
      
            integer NLAT 
      
      
      
      
      
      
      
 
      REAL (KIND=HIGH) , DIMENSION(nlat) :: cosc , gwt , sinc , colat  , wos2 
      REAL (KIND=HIGH) , PARAMETER       :: pi = 3.141592653589793
 
      
 
      REAL , PARAMETER :: xlim  = 1.0E-14
 
      INTEGER :: nzero, i, j
      REAL (KIND=HIGH) :: fi, fi1, a, b, g, gm, gp, gt, delta, c, d
 
      
 
      nzero = nlat/2
 
      
 
      DO i=1,nzero
         cosc(i) = SIN( (i-0.5)*pi/nlat + pi*0.5 )
      END DO
 
      
      fi  = nlat
      fi1 = fi+1.0
      a   = fi*fi1 / SQRT(4.0*fi1*fi1-1.0)
      b   = fi1*fi / SQRT(4.0*fi*fi-1.0)
 
      
 
      DO i=1,nzero
         j=0
 
         
         
 
         DO
            CALL lgord( g, cosc(i), nlat )
   
            
   
            CALL lgord( gm, cosc(i), nlat-1 )
            CALL lgord( gp, cosc(i), nlat+1 )
            gt = (cosc(i)*cosc(i)-1.0) / (a*gp-b*gm)
   
            
   
            delta   = g*gt
            cosc(i) = cosc(i) - delta
   
            
   
            j = j+1
            IF( ABS(delta).GT.xlim ) CYCLE
   
            
   
            c      = 2.0 *( 1.0-cosc(i)*cosc(i) )
            CALL lgord( d, cosc(i), nlat-1 )
            d      = d*d*fi*fi
            gwt(i) = c *( fi-0.5 ) / d
            EXIT
 
         END DO
 
      END DO
 
      
 
      DO i=1,nzero
         colat(i)= ACOS(cosc(i))
         sinc(i) = SIN(colat(i))
         wos2(i) = gwt(i) /( sinc(i)*sinc(i) )
      END DO
 
      
 
      IF( MOD(nlat,2) .NE. 0 ) THEN
         i       = nzero+1
         cosc(i) = 0.0
         c       = 2.0
         CALL lgord( d, cosc(i), nlat-1 )
         d       = d*d*fi*fi
         gwt(i)  = c *( fi-0.5 ) / d
         colat(i)= pi*0.5
         sinc(i) = 1.0
         wos2(i) = gwt(i)
      END IF
 
      
 
      DO i=nlat-nzero+1,nlat
         cosc(i) =-cosc(nlat+1-i)
         gwt(i)  = gwt(nlat+1-i)
         colat(i)= pi-colat(nlat+1-i)
         sinc(i) = sinc(nlat+1-i)
         wos2(i) = wos2(nlat+1-i)
      END DO
 
   END SUBROUTINE lggaus


   SUBROUTINE lgord( f, cosc, n )
 
      IMPLICIT NONE
 
      
      
      
      
      
      
      
      
      
      
 
      REAL (KIND=HIGH) :: s1, c4, a, b, fk, f, cosc, colat, c1, fn, ang
      INTEGER :: n, k
 
      
 
      colat = ACOS(cosc)
 
      c1 = SQRT(2.0_HIGH)
      DO k=1,n
         c1 = c1 * SQRT( 1.0 - 1.0/(4*k*k) )
      END DO
 
      fn = n
      ang= fn * colat
      s1 = 0.0
      c4 = 1.0
      a  =-1.0
      b  = 0.0
      DO k=0,n,2
         IF (k.eq.n) c4 = 0.5 * c4
         s1 = s1 + c4 * COS(ang)
         a  = a + 2.0
         b  = b + 1.0
         fk = k
         ang= colat * (fn-fk-2.0)
         c4 = ( a * (fn-b+1.0) / ( b * (fn+fn-a) ) ) * c4
      END DO 
 
      f = s1 * c1
 
   END SUBROUTINE lgord


   SUBROUTINE llij_gauss (lat, lon, proj, i, j) 
 
      IMPLICIT NONE
 
      REAL    , INTENT(IN)  :: lat, lon
      REAL    , INTENT(OUT) :: i, j
      TYPE (proj_info), INTENT(IN) :: proj
 
      INTEGER :: n , n_low
      LOGICAL :: found = .FALSE.
      REAL    :: diff_1 , diff_nlat
 
      
      
      
      
 
      i = ( lon - proj%lon1 ) / proj%loninc + 1.
 
      
      
 















 
      
      
      
      
 
      IF ( ABS(lat) .GT. ABS(proj%gauss_lat(1)) ) THEN
 
         diff_1    = lat - proj%gauss_lat(1)
         diff_nlat = lat - proj%gauss_lat(proj%nlat*2)
 
         IF ( ABS(diff_1) .LT. ABS(diff_nlat) ) THEN
            j = 1
         ELSE
            j = proj%nlat*2
         END IF
 
      
 
      ELSE
 
         DO n = 1 , proj%nlat*2 -1
            IF ( ( proj%gauss_lat(n) - lat ) * ( proj%gauss_lat(n+1) - lat ) .LE. 0 ) THEN
               found = .TRUE.
               n_low = n
               EXIT
            END IF
         END DO
 
         
  
         IF ( .NOT. found ) THEN
            PRINT '(A)','Troubles in river city.  No bounding values of latitude found in the Gaussian routines.'
            CALL wrf_error_fatal3("<stdin>",2229,&
'Gee_no_bounding_lats_Gaussian' )
         END IF
 
         j = ( ( proj%gauss_lat(n_low) - lat                     ) * ( n_low + 1 ) + &
               ( lat                   - proj%gauss_lat(n_low+1) ) * ( n_low     ) ) / &
               ( proj%gauss_lat(n_low) - proj%gauss_lat(n_low+1) )
 
      END IF

   END SUBROUTINE llij_gauss 
  
END MODULE module_llxy
