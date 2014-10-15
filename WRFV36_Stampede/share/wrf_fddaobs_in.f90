

































  SUBROUTINE wrf_fddaobs_in (grid ,config_flags)

    USE module_domain
    USE module_configure
    USE module_model_constants        

    IMPLICIT NONE
    TYPE(domain) :: grid
    TYPE(grid_config_rec_type),  INTENT(IN)    :: config_flags



    integer            :: ktau            
    integer            :: krest           
    integer            :: inest           
    integer            :: infreq          
    integer            :: nstlev          
    real               :: dtmin           
    real               :: xtime           
    logical            :: iprt_in4dob     

    INTEGER ids , ide , jds , jde , kds , kde , &
            ims , ime , jms , jme , kms , kme , &
            ips , ipe , jps , jpe , kps , kpe
    INTEGER ij, its, ite, jts, jte





    ktau   = grid%itimestep               
    krest  = grid%fdob%ktaur
    inest  = grid%grid_id
    nstlev = grid%fdob%levidn(inest) 
    infreq = grid%obs_ionf*(grid%parent_grid_ratio**nstlev)
    iprt_in4dob = grid%obs_ipf_in4dob

    IF( ((ktau.GT.krest.AND.MOD(ktau,infreq).EQ.0)                            &
                                         .OR.(ktau.EQ.krest)) .AND. grid%xtime <= grid%fdda_end ) then

      dtmin = grid%dt/60.
      xtime = grid%xtime

      CALL get_ijk_from_grid (  grid ,                                       &
                                ids, ide, jds, jde, kds, kde,                &
                                ims, ime, jms, jme, kms, kme,                &
                                ips, ipe, jps, jpe, kps, kpe    )

      !$OMP PARALLEL DO   &
      !$OMP PRIVATE ( ij )

      DO ij = 1 , grid%num_tiles
         its = grid%i_start(ij)
         ite = min(grid%i_end(ij),ide-1)
         jts = grid%j_start(ij)
         jte = min(grid%j_end(ij),jde-1)

         CALL in4dob(inest, xtime, ktau, krest, dtmin,                              &
                     grid%julyr, grid%julday, grid%gmt,                             &    
                     grid%obs_nudge_opt,  grid%obs_nudge_wind, grid%obs_nudge_temp, &
                     grid%obs_nudge_mois, grid%obs_nudge_pstr, grid%obs_coef_wind,  &
                     grid%obs_coef_temp,  grid%obs_coef_mois,  grid%obs_coef_pstr,  &
                     grid%obs_rinxy,      grid%obs_rinsig,     grid%fdob%window,    &
                     grid%obs_npfi,       grid%obs_ionf,                            &
                     grid%obs_prt_max,    grid%obs_prt_freq,                        &
                     grid%obs_idynin,                                               &
                     grid%obs_dtramp,     grid%fdob,           grid%fdob%varobs,    &
                     grid%fdob%timeob,    grid%fdob%nlevs_ob,  grid%fdob%lev_in_ob, &
                     grid%fdob%plfo,      grid%fdob%elevob,    grid%fdob%rio,       &
                     grid%fdob%rjo,       grid%fdob%rko,                            &
                     grid%xlat, grid%xlong,                                         &
                     config_flags%cen_lat,                                          &
                     config_flags%cen_lon,                                          &
                     config_flags%stand_lon,                                        &
                     config_flags%truelat1, config_flags%truelat2,                  &
                     grid%fdob%known_lat, grid%fdob%known_lon,                      &
                     config_flags%dx, config_flags%dy, rovg, t0,                    &
                     grid%fdob%obsprt,                                              &
                     grid%fdob%latprt, grid%fdob%lonprt,                            &
                     grid%fdob%mlatprt, grid%fdob%mlonprt,                          &
                     grid%fdob%stnidprt,                                            &
                     ide, jde,                                                      &
                     ims, ime, jms, jme,                                            &
                     its, ite, jts, jte,                                            &
                     config_flags%map_proj,                                         &
                     model_config_rec%parent_grid_ratio,                            &
                     model_config_rec%i_parent_start(inest),                        &
                     model_config_rec%j_parent_start(inest),                        &
                     model_config_rec%max_dom,                                      &
                     model_config_rec%nobs_ndg_vars, grid%max_obs, iprt_in4dob)
       ENDDO
      !$OMP END PARALLEL DO

    ENDIF

    RETURN

  END SUBROUTINE wrf_fddaobs_in




  SUBROUTINE in4dob(inest, xtime, ktau, ktaur, dtmin,                    &
                    myear, julday, gmt,                                  &      
                    nudge_opt, iswind, istemp,                           &
                    ismois, ispstr, giv,                                 &
                    git, giq, gip,                                       &
                    rinxy, rinsig, twindo,                               &
                    npfi, ionf, prt_max, prt_freq, idynin,               &
                    dtramp, fdob, varobs,                                &
                    timeob, nlevs_ob, lev_in_ob,                         &
                    plfo, elevob, rio,                                   &
                    rjo, rko,                                            &
                    xlat, xlong,                                         &
                    cen_lat,                                             &
                    cen_lon,                                             &
                    stand_lon,                                           &
                    true_lat1, true_lat2,                                &
                    known_lat, known_lon,                                &
                    dxm, dym, rovg, t0,                                  &
                    obs_prt,                                             &
                    lat_prt, lon_prt,                                    &
                    mlat_prt, mlon_prt,                                  &
                    stnid_prt,                                           &
                    e_we, e_sn,                                          &
                    ims, ime, jms, jme,                                  &
                    its, ite, jts, jte,                                  &
                    map_proj,                                            &
                    parent_grid_ratio,                                   &
                    i_parent_start,                                      &
                    j_parent_start,                                      &
                    maxdom,                                              &
                    nndgv, niobf, iprt)

  USE module_domain
  USE module_model_constants, ONLY : rcp
  USE module_date_time , ONLY : geth_idts
  USE module_llxy

  IMPLICIT NONE























  INTEGER, intent(in) :: niobf          
  INTEGER, intent(in) :: nndgv          
  INTEGER, intent(in)  :: INEST         
  REAL, intent(in)     :: xtime         
  INTEGER, intent(in)  :: KTAU          
  INTEGER, intent(in)  :: KTAUR         
  REAL, intent(in)     :: dtmin         
  INTEGER, intent(in)  :: myear         
  INTEGER, intent(in)  :: julday        
  REAL, intent(in)     :: gmt           
  INTEGER, intent(in)  :: nudge_opt     
  INTEGER, intent(in)  :: iswind        
  INTEGER, intent(in)  :: istemp        
  INTEGER, intent(in)  :: ismois        
  INTEGER, intent(in)  :: ispstr        
  REAL, intent(in)     :: giv           
  REAL, intent(in)     :: git           
  REAL, intent(in)     :: giq           
  REAL, intent(in)     :: gip           
  REAL, intent(in)     :: rinxy         
  REAL, intent(in)     :: rinsig        
  REAL, intent(inout)  :: twindo        
  INTEGER, intent(in)  :: npfi          
  INTEGER, intent(in)  :: ionf          
  INTEGER, intent(in)  :: prt_max       
  INTEGER, intent(in)  :: prt_freq      
  INTEGER, intent(in)  :: idynin        
  REAL, intent(in)     :: dtramp        
  TYPE(fdob_type), intent(inout)  :: fdob     
  REAL, intent(inout) :: varobs(nndgv,niobf)  
  REAL, intent(inout) :: timeob(niobf)        
  REAL, intent(inout) :: nlevs_ob(niobf)      
  REAL, intent(inout) :: lev_in_ob(niobf)     
  REAL, intent(inout) :: plfo(niobf)          
  REAL, intent(inout) :: elevob(niobf)        
  REAL, intent(inout) :: rio(niobf)           
  REAL, intent(inout) :: rjo(niobf)           
  REAL, intent(inout) :: rko(niobf)           
  REAL, DIMENSION( ims:ime, jms:jme ),                            &
        INTENT(IN )       :: xlat, xlong      
  REAL, intent(in) :: cen_lat                 
  REAL, intent(in) :: cen_lon                 
  REAL, intent(in) :: stand_lon               
  REAL, intent(in) :: true_lat1               
  REAL, intent(in) :: true_lat2               
  REAL, intent(in) :: known_lat               
  REAL, intent(in) :: known_lon               
  REAL, intent(in) :: dxm                     
  REAL, intent(in) :: dym                     
  REAL, intent(in) :: rovg                    
  REAL, intent(in) :: t0                      
  INTEGER, intent(inout) :: obs_prt(prt_max)  
  REAL, intent(inout) :: lat_prt(prt_max)     
  REAL, intent(inout) :: lon_prt(prt_max)     
  REAL, intent(inout) :: mlat_prt(prt_max)    
  REAL, intent(inout) :: mlon_prt(prt_max)    
  INTEGER, intent(inout) :: stnid_prt(40,prt_max) 
  INTEGER, intent(in) :: e_we                 
  INTEGER, intent(in) :: e_sn                 
  INTEGER, intent(in) :: ims                  
  INTEGER, intent(in) :: ime                  
  INTEGER, intent(in) :: jms                  
  INTEGER, intent(in) :: jme                  
  INTEGER, intent(in) :: its                  
  INTEGER, intent(in) :: ite                  
  INTEGER, intent(in) :: jts                  
  INTEGER, intent(in) :: jte                  
  INTEGER, intent(in) :: map_proj             
  INTEGER, intent(in) :: parent_grid_ratio    
  INTEGER, intent(in) :: i_parent_start       
  INTEGER, intent(in) :: j_parent_start       
  INTEGER, intent(in) :: maxdom               
  LOGICAL, intent(in) :: iprt                 
      

  integer :: n, ndum, nopen, nvol, idate, imm, iss
  integer :: nlast                      
  integer :: nsta                       
  integer :: nstaw                      
  integer :: nprev                      
  integer :: meas_count, imc, njend, njc, njcc, julob, kn
  real    :: hourob, rjulob
  real    :: xhour, tback, tforwd, rjdate1, timanl1, rtimob
  real    :: rj, ri, elevation, pressure_data
  real    :: pressure_qc, height_data, height_qc, temperature_data
  real    :: temperature_qc, u_met_data, u_met_qc, v_met_data
  real    :: v_met_qc, rh_data, rh_qc, r_data, slp_data, slp_qc
  real    :: ref_pres_data, ref_pres_qc, psfc_data, psfc_qc
  real    :: precip_data, precip_qc, tbar, twdop
  real*8  :: tempob
  INTEGER, EXTERNAL :: nvals_le_limit         


  TYPE (PROJ_INFO)   :: obs_proj        
  character*14 date_char
  character*19 obs_date                                                        
  integer idts                                                                 
  character*40 platform,source,id,namef
  character*2 fonc
  character(len=200) :: msg       
  real latitude,longitude
  logical :: newpass          
  logical is_sound,bogus
  LOGICAL OPENED,exist
  integer :: ieof(5),ifon(5)
  data ieof/0,0,0,0,0/
  data ifon/0,0,0,0,0/
  integer :: nmove, nvola
  integer :: iyear, itimob                                                     
  integer :: errcnt
  DATA NMOVE/0/,NVOLA/61/






  IF(nudge_opt.NE.1)RETURN


  obs_prt = -999
  newpass = .true.
  errcnt  = 0 


  IF(KTAU.EQ.0.OR.KTAU.EQ.KTAUR) THEN
    DO N=1,NIOBF
      TIMEOB(N)=99999.
    ENDDO
    fdob%xtime_at_rest = xtime    
  ENDIF

  IF(KTAU.EQ.KTAUR)fdob%NSTAT=0
  NSTA=fdob%NSTAT

  XHOUR=XTIME/60.
  XHOUR=AMAX1(XHOUR,0.0)


  TBACK=XHOUR-TWINDO
  TFORWD=XHOUR+TWINDO

  IF (iprt) then
     write(msg,fmt='(2(a,f8.3),a,i2)')                                            &
                  'OBS NUDGING: Reading new obs for time window TBACK = ',  &
                  tback,' TFORWD = ',tforwd,' for grid = ',inest
     call wrf_message(msg)
  ENDIF




  IF(NSTA.NE.0) THEN
    NDUM=0
    t_window : DO N=1,NSTA+1
      IF((TIMEOB(N)-TBACK).LT.0) THEN
        TIMEOB(N)=99999.
      ENDIF
      IF(TIMEOB(N).LT.9.E4) EXIT t_window
      NDUM=N
    ENDDO t_window

    IF (iprt .and. ndum>0) THEN
      write(msg,fmt='(a,i5,2a)') 'OBS NUDGING: ',ndum,' previously read obs ',  &
           'are now too old for the current window and have been removed.'
      call wrf_message(msg)
    ENDIF



    NDUM=ABS(NDUM)
    NMOVE=NIOBF-NDUM
    IF(NMOVE.GT.0 .AND. NDUM.NE.0 ) THEN  
      DO N=1,NMOVE
        do KN = 1,nndgv
          VAROBS(KN,N)=VAROBS(KN,N+NDUM)
        enddo

        RJO(N)=RJO(N+NDUM)
        RIO(N)=RIO(N+NDUM)
        RKO(N)=RKO(N+NDUM)
        TIMEOB(N)=TIMEOB(N+NDUM)
        nlevs_ob(n)=nlevs_ob(n+ndum)
        lev_in_ob(n)=lev_in_ob(n+ndum)
        plfo(n)=plfo(n+ndum)
        elevob(n)=elevob(n+ndum) 
      ENDDO
    ENDIF
    NOPEN=NMOVE+1
    IF(NOPEN.LE.NIOBF) THEN
      DO N=NOPEN,NIOBF
        do KN = 1,nndgv
          VAROBS(KN,N)=99999.
        enddo
        RIO(N)=99999.
        RJO(N)=99999.
        RKO(N)=99999.
        TIMEOB(N)=99999.
        nlevs_ob(n)=99999.
        lev_in_ob(n)=99999.
        plfo(n)=99999.
        elevob(n)=99999.
      ENDDO
    ENDIF
  ENDIF


  call set_projection(obs_proj, map_proj, cen_lat, cen_lon,            &
                      true_lat1, true_lat2, stand_lon,                 &
                      known_lat, known_lon,                            &
                      e_we, e_sn, dxm, dym )


  NLAST=0
  last_ob : DO N=1,NIOBF

    IF(TIMEOB(N).GT.9.E4) EXIT last_ob
    NLAST=N
  ENDDO last_ob



  IF(KTAU.EQ.0.OR.KTAU.EQ.KTAUR) THEN
    fdob%RTLAST=-999.
    INQUIRE (NVOLA+INEST-1,OPENED=OPENED)
    IF (.NOT. OPENED) THEN
      ifon(inest)=1
      write(fonc(1:2),'(i2)')ifon(inest)
      if(fonc(1:1).eq.' ')fonc(1:1)='0'
      INQUIRE (file='OBS_DOMAIN'//CHAR(INEST+ICHAR('0'))//fonc(1:2)  &
              ,EXIST=exist)
      if(exist)then
        IF (iprt) THEN
          write(msg,*) 'opening first fdda obs file, fonc=',              &
                       fonc,' inest=',inest
          call wrf_message(msg)
          write(msg,*) 'ifon=',ifon(inest)
          call wrf_message(msg)
        ENDIF
        OPEN(NVOLA+INEST-1,                                          &
        FILE='OBS_DOMAIN'//CHAR(INEST+ICHAR('0'))//fonc(1:2),        &
              FORM='FORMATTED',STATUS='OLD')
      else

        IF (iprt) call wrf_message("there are no fdda obs files to open")
        return
      endif

    ENDIF
  ENDIF  

 








  N=NLAST
  IF(N.EQ.0)GOTO 110

 1001 continue



    IF(IEOF(inest).GT.1) then
      GOTO 130
    endif

100 CONTINUE

    IF(N.ne.0) THEN
      IF(TIMEOB(N).GT.TFORWD.and.timeob(n).lt.99999.) THEN
         GOTO 130
      ENDIF
    ENDIF
 


    if(ieof(inest).eq.1 )then
      ieof(inest)=2
      goto 130
    endif




  110 continue



      IF(N.GT.NIOBF-1)GOTO 120

      NVOL=NVOLA+INEST-1
      IF(fdob%IEODI.EQ.1)GOTO 111
      read(nvol,101,end=111,err=111)date_char
 101  FORMAT(1x,a14)

      n=n+1


      call fmt_date(date_char, obs_date)




      call geth_idts(obs_date, fdob%sdate(1:19), idts)


      
      idts = idts + nint(fdob%xtime_at_rest*60.)  

      rtimob =float(idts)/3600.
      timeob(n)=rtimob


      IF(IDYNIN.EQ.1.AND.TIMEOB(N)*60..GT.fdob%DATEND) THEN
        IF (iprt) THEN
          write(msg,*) ' IN4DOB: FOR INEST = ',INEST,' AT XTIME = ',XTIME,    &
                       ' TIMEOB = ',TIMEOB(N)*60.,' AND DATEND = ',fdob%DATEND,' :'
          call wrf_message(msg)
          write(msg,*) '         END-OF-DATA FLAG SET FOR OBS-NUDGING',       &
                       ' DYNAMIC INITIALIZATION'
          call wrf_message(msg)
        ENDIF
        fdob%IEODI=1
        TIMEOB(N)=99999.
        rtimob=timeob(n)
      ENDIF
      read(nvol,102)latitude,longitude
 102  FORMAT(2x,2(f9.4,1x))




          


      CALL latlon_to_ij(obs_proj, latitude, longitude, ri, rj)




      ri = ri + .5      
      rj = rj + .5      

      rio(n)=ri
      rjo(n)=rj

      read(nvol,1021)id,namef
 1021 FORMAT(2x,2(a40,3x))
      read(nvol,103)platform,source,elevation,is_sound,bogus,meas_count
 103  FORMAT( 2x,2(a16,2x),f8.0,2x,2(l4,2x),i5)






      elevob(n)=elevation


      if(namef(2:9).eq.'PROFILER')platform(7:14)='PROFILER'

      if(namef(2:6).eq.'ACARS')platform(7:11)='ACARS'
      if(namef(1:7).eq.'SATWNDS') platform(1:11)='SATWNDS    '
      if(namef(1:8).eq.'CLASS DA')platform(7:10)='TEMP'

 
      rko(n)=-99.





      if(.NOT. is_sound) rko(n)=1.0



      plfo(n)=99.
      if(platform(7:11).eq.'METAR')plfo(n)=1.
      if(platform(7:11).eq.'SPECI')plfo(n)=2.
      if(platform(7:10).eq.'SHIP')plfo(n)=3.
      if(platform(7:11).eq.'SYNOP')plfo(n)=4.
      if(platform(7:10).eq.'TEMP')plfo(n)=5.
      if(platform(7:11).eq.'PILOT')plfo(n)=6.
      if(platform(1:7).eq.'SATWNDS')plfo(n)=7.
      if(platform(7:11).eq.'SATWI')plfo(n)=7.
      if(platform(1:4).eq.'SAMS')plfo(n)=8.
      if(platform(7:14).eq.'PROFILER')plfo(n)=9.

      if(platform(7:11).eq.'ACARS')plfo(n)=7.

      if(plfo(n).eq.99.) then
         IF (iprt) then
           write(msg,*) 'n=',n,' unknown ob of type ',platform
           call wrf_message(msg)
         ENDIF
      endif




      IF(is_sound)THEN
        nlevs_ob(n)=real(meas_count)
        lev_in_ob(n)=1.
        do imc=1,meas_count




          if(imc.gt.1)then                          
            n=n+1
            if(n.gt.niobf)goto 120
            nlevs_ob(n)=real(meas_count)
            lev_in_ob(n)=real(imc)
            timeob(n)=rtimob
            rio(n)=ri
            rjo(n)=rj
            rko(n)=-99.
            plfo(n)=plfo(n-imc+1)
            elevob(n)=elevation
          endif

          read(nvol,104)pressure_data,pressure_qc,                  &
                        height_data,height_qc,                      &
                        temperature_data,temperature_qc,            &
                        u_met_data,u_met_qc,                        &
                        v_met_data,v_met_qc,                        &
                        rh_data,rh_qc
 104      FORMAT( 1x,6(f11.3,1x,f11.3,1x))

























 



 
          if(temperature_qc.eq.-777777.)temperature_qc=0.
          if(pressure_qc.eq.-777777.)pressure_qc=0.
          if(height_qc.eq.-777777.)height_qc=0.
          if(u_met_qc.eq.-777777.)u_met_qc=0.
          if(v_met_qc.eq.-777777.)v_met_qc=0.
          if(rh_qc.eq.-777777.)rh_qc=0.
          if(temperature_data.eq.-888888.)temperature_qc=-888888.
          if(pressure_data.eq.-888888.)pressure_qc=-888888.
          if(height_data.eq.-888888.)height_qc=-888888.
          if(u_met_data.eq.-888888.)u_met_qc=-888888.
          if(v_met_data.eq.-888888.)v_met_qc=-888888.
          if(rh_data.eq.-888888.)rh_qc=-888888.
 

















          if(plfo(n).eq.5..and.(u_met_qc.eq.256..or.v_met_qc.eq.256.))then
            u_met_data=-888888.
            v_met_data=-888888.
            u_met_qc=-888888.
            v_met_qc=-888888.
          endif


          if(plfo(n).eq.6.)then
            temperature_data=-888888.
            rh_data=-888888.
            temperature_qc=-888888.
            rh_qc=-888888.
          endif






          if(temperature_qc.ge.0..and.temperature_qc.lt.30000.)then

            if( (pressure_qc.ge.0..and.pressure_qc.lt.30000.) .or.    &
                (height_qc  .ge.0..and.height_qc  .lt.30000.) ) then

              varobs(3,n) = temperature_data
            else
              varobs(3,n)=-888888.
            endif

          else
            varobs(3,n)=-888888.
          endif


          if(height_qc.ge.0..and.height_qc.lt.30000.)then
            varobs(6,n)=height_data
          else
            varobs(6,n)=-888888.
          endif

          if(pressure_qc.ge.0..and.pressure_qc.lt.30000.)then

            varobs(5,n)=pressure_data
          else
            varobs(5,n)=-888888.
            IF (iprt) THEN
              if(varobs(6,n).eq.-888888.000) then
                if (errcnt.le.10) then
                  write(msg,*) '*** PROBLEM: sounding, p and ht undefined',latitude,longitude
                  call wrf_message(msg)
                  errcnt = errcnt + 1
                  if (errcnt.gt.10) call wrf_message("MAX of 10 warnings issued.")
                endif
              endif
            ENDIF
          endif 
          if(varobs(5,n).ge.0.)varobs(5,n)=varobs(5,n)*1.e-3

          if((varobs(5,n).gt.0.).and.(varobs(5,n).le.8.))then
            u_met_data=-888888.
            v_met_data=-888888.
            u_met_qc=-888888.
            v_met_qc=-888888.
            temperature_data=-888888.
            temperature_qc=-888888.
            rh_data=-888888.
            rh_qc=-888888.
          endif



          if((u_met_qc.ge.0..and.u_met_qc.lt.30000.).and.  &
             (v_met_qc.ge.0..and.v_met_qc.lt.30000.).and.  &

             (u_met_data.ne.0..or.v_met_data.ne.0.))then


               if(u_met_qc.eq.129. .and. v_met_qc.eq.129.) then
                  CALL rotate_vector(longitude,u_met_data,v_met_data,   &
                                     obs_proj,map_proj)
               endif
               varobs(1,n)=u_met_data
               varobs(2,n)=v_met_data
          else
               varobs(1,n)=-888888.
               varobs(2,n)=-888888.
          endif

          r_data=-888888.

          if(rh_qc.ge.0..and.rh_qc.lt.30000.)then
            if((pressure_qc.ge.0.).and.(temperature_qc.ge.0.).and.       &
              (pressure_qc.lt.30000.).and.(temperature_qc.lt.30000.))then
              call rh2r(rh_data,temperature_data,pressure_data*.01,      &
                        r_data,0)            
            else


              r_data=-888888.
            endif
          endif
          varobs(4,n)=r_data
        enddo    



      ELSEIF(.NOT.is_sound)THEN
        nlevs_ob(n)=1.
        lev_in_ob(n)=1.
        read(nvol,105)slp_data,slp_qc,                                 &
                      ref_pres_data,ref_pres_qc,                       &
                      height_data,height_qc,                           &
                      temperature_data,temperature_qc,                 &
                      u_met_data,u_met_qc,                             &
                      v_met_data,v_met_qc,                             &
                      rh_data,rh_qc,                                   &
                      psfc_data,psfc_qc,                               &
                      precip_data,precip_qc
 105    FORMAT( 1x,9(f11.3,1x,f11.3,1x))
























        if((psfc_qc.lt.0.).and.(slp_qc.ge.0..and.slp_qc.lt.30000.).and.   &
              (temperature_qc.ge.0..and.temperature_qc.lt.30000.).and.    &
              (slp_data.gt.90000.))then
          tbar=temperature_data+0.5*elevation*.0065
          psfc_data=slp_data*exp(-elevation/(rovg*tbar))
          varobs(5,n)=psfc_data
          psfc_qc=0.
        endif







        if((psfc_qc.lt.0.).and.                                          &
          (temperature_qc.ge.0..and.temperature_qc.lt.30000.))then
          tbar=temperature_data+0.5*elevation*.0065
          psfc_data=100000.*exp(-elevation/(rovg*tbar))
          varobs(5,n)=psfc_data
          psfc_qc=0.
        endif

        if((psfc_qc.ge.0..and.psfc_qc.lt.30000.).and.(psfc_data.gt.70000.  &
        .and.psfc_data.lt.105000.))then
          varobs(5,n)=psfc_data
        else
          varobs(5,n)=-888888.
        endif

        if(varobs(5,n).ge.0.)varobs(5,n)=varobs(5,n)*1.e-3



        if(temperature_qc.ge.0..and.temperature_qc.lt.30000.)then

          if((psfc_qc.ge.0..and.psfc_qc.lt.30000.).and.          &
             (psfc_data.gt.70000. .and.psfc_data.lt.105000.))then

            varobs(3,n) = temperature_data
          else
            varobs(3,n)=-888888.
          endif
        else
          varobs(3,n)=-888888.
        endif


        if((u_met_qc.ge.0..and.u_met_qc.lt.30000.).and.            &
           (v_met_qc.ge.0..and.v_met_qc.lt.30000.).and.            &

           (u_met_data.ne.0..or.v_met_data.ne.0.))then


             if(u_met_qc.eq.129. .and. v_met_qc.eq.129.) then
                CALL rotate_vector(longitude,u_met_data,v_met_data,   &
                                   obs_proj,map_proj)
             endif
             varobs(1,n)=u_met_data
             varobs(2,n)=v_met_data
        else
             varobs(1,n)=-888888.
             varobs(2,n)=-888888.
        endif




        if(plfo(n).eq.3..and.rh_qc.ge.0..and.rh_data.lt.70.)then
          rh_qc=-888888.
          rh_data=-888888.
        endif

        r_data=-888888.
        if(rh_qc.ge.0..and.rh_qc.lt.30000.)then
          if((psfc_qc.ge.0..and.psfc_qc.lt.30000.)                       &
          .and.(temperature_qc.ge.0..and.temperature_qc.lt.30000.))then

            call rh2r(rh_data,temperature_data,psfc_data*.01,            &
                      r_data,0)            
          else


            r_data=-888888.
          endif
        endif
        varobs(4,n)=r_data
      ELSE
        IF (iprt) THEN
           call wrf_message(" ======  ")
           call wrf_message(" NO Data Found ")
        ENDIF
      ENDIF   




      IF(RTIMOB.LT.TBACK-TWINDO)then
        IF (iprt) call wrf_message("ob too early")
        n=n-1
        GOTO 110
      ENDIF



      njend=n-1
      if(is_sound)njend=n-meas_count
      do njc=1,njend



        if( (timeob(n).eq.timeob(njc)) .and.                     &
            (rio(n).eq.rio(njc))       .and.                     &
            (rjo(n).eq.rjo(njc))       .and.                     &
            (plfo(njc).ne.99.) ) then






          if( ( (plfo(n).le.4.).and.(plfo(njc).le.4.) ) .or.     &
                (plfo(n).eq.plfo(njc)) ) then


            if((.not.is_sound).and.(rko(njc).eq.rko(n))) then



              do KN = 1,nndgv
                VAROBS(KN,njc)=VAROBS(KN,n)
              enddo










              n=n-1
              goto 100






            elseif( (is_sound).and.(plfo(njc).eq.plfo(n)) .and.            &
                    ( (plfo(njc).eq.5.).or.(plfo(njc).eq.9.).or.           &
                    ( (plfo(njc).eq.6.).and.                               &
                      (nlevs_ob(n).ge.nlevs_ob(njc)) ) ) )then
              IF (iprt) THEN
                write(msg,*) 'duplicate sounding - eliminate first occurrence', &
                                       n,inest,meas_count,nlevs_ob(njc),        &
                                       latitude,longitude,plfo(njc)
                call wrf_message(msg)
              ENDIF
              if(lev_in_ob(njc).ne.1.) then
                IF (iprt) THEN
                  write(msg,*) 'problem ******* - dup sndg ',                   &
                               lev_in_ob(njc),nlevs_ob(njc)
                  call wrf_message(msg)
                ENDIF
              endif


              do njcc=njc,njc+nint(nlevs_ob(njc))-1
                do KN = 1,nndgv
                  VAROBS(KN,njcc)=-888888.
                enddo
                plfo(njcc)=99.
              enddo
              goto 100

            elseif( (is_sound).and.(plfo(njc).eq.plfo(n)) .and.            &
                    (plfo(njc).eq.6.).and.                                 &
                    (nlevs_ob(n).lt.nlevs_ob(njc)) )then
              IF (iprt) THEN
                write(msg,*)                                               &
                 'duplicate pilot sounding - eliminate second occurrence', &
                                 n,inest,meas_count,nlevs_ob(njc),         &
                                 latitude,longitude,plfo(njc)
                call wrf_message(msg)
              ENDIF
              if(lev_in_ob(njc).ne.1.) then
                IF (iprt) THEN
                  write(msg,*) 'problem ******* - dup sndg ',              &
                           lev_in_ob(njc),nlevs_ob(njc)
                  call wrf_message(msg)
                ENDIF
              endif
              n=n-meas_count


              do imc = n+1, n+meas_count
                timeob(imc) = 99999.
              enddo
              goto 100

            elseif( (is_sound).and.                                        &
                    (nlevs_ob(njc).eq.1.).and.                             &
                    (nlevs_ob(n).eq.1.).and.                               &
                    (varobs(5,njc).eq.varobs(5,n)).and.                    &
                    (plfo(njc).eq.7.).and.(plfo(n).eq.7.) ) then
              IF (iprt) then
                write(msg,*)                                               &
                'duplicate single lev sat-wind ob - replace first',n,      &
                                 inest,meas_count,varobs(5,n)
                call wrf_message(msg)
              ENDIF

              do KN = 1,nndgv
                VAROBS(KN,njc)=VAROBS(KN,n)
              enddo









              n=n-1
              goto 100
            else






            endif
          endif
        endif

      enddo


      if( plfo(n).eq.4..and.(platform(7:16).eq.'SYNOP PRET').and.          &
          (id(7:15).eq.'METNET= 3') )then

        n=n-1
        goto 100
      endif


      if( (ri.lt.2.).or.(ri.gt.real(e_we-1)).or.(rj.lt.2.).or.         &
          (rj.gt.real(e_sn-1)) ) then

          n=n-meas_count

          do imc = n+1, n+meas_count
            timeob(imc) = 99999.
          enddo
          goto 100
      endif

      IF(TIMEOB(N).LT.fdob%RTLAST) THEN
        IF (iprt) THEN
          call wrf_message("2 OBS ARE NOT IN CHRONOLOGICAL ORDER")
          call wrf_message("NEW YEAR?")
          write(msg,*) 'timeob,rtlast,n=',timeob(n),fdob%rtlast,n
          call wrf_message(msg)
        ENDIF
        call wrf_error_fatal3("wrf_fddaobs_in.b",1103,&
'wrf_fddaobs_in: in4dob STOP 111' )
      ELSE
        fdob%RTLAST=TIMEOB(N)
      ENDIF

      CALL collect_obs_info(newpass,inest,n,latitude,longitude,              &
                         nlast,nprev,niobf,id,stnid_prt,                     &
                         rio,rjo,prt_max,prt_freq,xlat,xlong,                &
                         obs_prt,lat_prt,lon_prt,mlat_prt,mlon_prt,          &
                         e_we,e_sn,ims,ime,jms,jme,its,ite,jts,jte)
      GOTO 100
  111 CONTINUE




      if (iprt) then
        write(msg,5403) NVOL,XTIME
        call wrf_message(msg)
      endif
      IEOF(inest)=1

      close(NVOLA+INEST-1)
      IF (iprt) then
         write(msg,*) 'closed fdda file for inest=',inest,nsta
         call wrf_message(msg)
      ENDIF


  goto 1001

120 CONTINUE




  IF (iprt) THEN
    write(msg,121) N,NIOBF
    call wrf_message(msg)
  ENDIF
  call wrf_error_fatal3("wrf_fddaobs_in.b",1144,&
'wrf_fddaobs_in: in4dob STOP 122' )

130 CONTINUE










  IF(KTAU.EQ.KTAUR)THEN
    NSTA=0
    keep_obs : DO N=1,NIOBF



      IF(TIMEOB(N).GT.9.e4) EXIT keep_obs
      if(timeob(n).gt.tforwd) then
        if(iprt) then
           write(msg,950) inest
           call wrf_message(msg)
           write(msg,951) n,timeob(n),tforwd
           call wrf_message(msg)
        endif
 950    FORMAT('Saving index of first ob after end of current time window ', &
               'for nest = ', i3,':')
 951    FORMAT('  ob index = ',i8,',   time of ob = ',f8.4,                  &
               ' hrs,   end of time window = ',f8.4,' hrs')
      endif
      NSTA=N
    ENDDO keep_obs

    NDUM=0


    old_obs : DO N=1,NSTA+1
      IF((TIMEOB(N)-TBACK).LT.0)THEN
        TIMEOB(N)=99999.
      ENDIF

      IF(TIMEOB(N).LT.9.E4) EXIT old_obs
      NDUM=N
    ENDDO old_obs


    IF (iprt .and. ktaur > 0) THEN
      write(msg,fmt='(a,i5,a)') 'OBS NUDGING: Upon restart, skipped over ',ndum,   &
                ' obs that are now too old for the current obs window.'
      call wrf_message(msg)
    ENDIF

    NDUM=ABS(NDUM)
    NMOVE=NIOBF-NDUM
    IF( NMOVE.GT.0 .AND. NDUM.NE.0) THEN
      DO N=1,NMOVE
        do KN = 1,nndgv
          VAROBS(KN,N)=VAROBS(KN,N+NDUM)
        enddo
        RJO(N)=RJO(N+NDUM)
        RIO(N)=RIO(N+NDUM)
        RKO(N)=RKO(N+NDUM)
        TIMEOB(N)=TIMEOB(N+NDUM)
        nlevs_ob(n)=nlevs_ob(n+ndum)
        lev_in_ob(n)=lev_in_ob(n+ndum)
        plfo(n)=plfo(n+ndum)
      ENDDO
    ENDIF

    NOPEN=NMOVE+1
    IF(NOPEN.LE.NIOBF) THEN
      DO N=NOPEN,NIOBF
        do KN = 1,nndgv
          VAROBS(KN,N)=99999.
        enddo
        RIO(N)=99999.
        RJO(N)=99999.
        RKO(N)=99999.
        TIMEOB(N)=99999.
      ENDDO
    ENDIF
  ENDIF

  NSTA=0


  recalc : DO N=1,NIOBF

    IF(TIMEOB(N).GT.9.e4) EXIT recalc
    NSTA=N

  ENDDO recalc


  nstaw = nvals_le_limit(nsta, timeob, tforwd)

  IF (iprt) then
      write(msg,160) KTAU,XTIME,NSTAW
      call wrf_message(msg)
  ENDIF
  IF(KTAU.EQ.KTAUR)THEN
    IF(nudge_opt.EQ.1)THEN
      TWDOP=TWINDO*60.
      IF (iprt) THEN
        write(msg,1449) INEST,RINXY,RINSIG,TWDOP
        call wrf_message(msg)
        IF(ISWIND.EQ.1) then
          write(msg,1450) GIV
          call wrf_message(msg)
        ELSE
          write(msg,1455) INEST
          call wrf_message("")
          call wrf_message(msg)
          call wrf_message("")
        ENDIF
        IF(ISTEMP.EQ.1) then
          write(msg,1451) GIT
          call wrf_message(msg)
        ELSE
          write(msg,1456) INEST
          call wrf_message("")
          call wrf_message(msg)
        ENDIF
        IF(ISMOIS.EQ.1) then
          call wrf_message("")
          write(msg,1452) GIQ
          call wrf_message(msg)
        ELSE
          write(msg,1457) INEST
          call wrf_message("")
          call wrf_message(msg)
          call wrf_message("")
        ENDIF
      ENDIF
    ENDIF
  ENDIF
  IF(KTAU.EQ.KTAUR)THEN
    IF(fdob%IWTSIG.NE.1)THEN
      IF (iprt) THEN
        write(msg,555)
        call wrf_message(msg)
        write(msg,556) fdob%RINFMN*RINXY,fdob%RINFMX*RINXY,fdob%PFREE*10.
        call wrf_message(msg)
      ENDIF
      IF(fdob%RINFMN.GT.fdob%RINFMX) then
         call wrf_error_fatal3("wrf_fddaobs_in.b",1292,&
'wrf_fddaobs_in: in4dob STOP 556' )
      ENDIF


      IF (iprt) then
        write(msg,557) fdob%DPSMX*10.,fdob%DCON
        call wrf_message(msg)
      ENDIF
      IF(fdob%DPSMX.GT.10.) then
         call wrf_error_fatal3("wrf_fddaobs_in.b",1302,&
'wrf_fddaobs_in: in4dob STOP 557' )
      ENDIF
    ENDIF
  ENDIF
 
  IF(KTAU.EQ.KTAUR)THEN
    IF (iprt) then
      write(msg,601) INEST,IONF
      call wrf_message(msg)
      call wrf_message("")
    ENDIF
  ENDIF
  fdob%NSTAT=NSTA
  fdob%NSTAW=NSTAW

555   FORMAT(1X,'   ABOVE THE SURFACE LAYER, OBS NUDGING IS PERFORMED',  &
      ' ON PRESSURE LEVELS,')
556   FORMAT(1X,'   WHERE RINXY VARIES LINEARLY FROM ',E11.3,' KM AT',   &
      ' THE SURFACE TO ',E11.3,' KM AT ',F7.2,' MB AND ABOVE')
557   FORMAT(1X,'   IN THE SURFACE LAYER, WXY IS A FUNCTION OF ',        &
      'DPSMX = ',F7.2,' MB WITH DCON = ',E11.3,                          &
      ' - SEE SUBROUTINE NUDOB')
601   FORMAT('FOR EFFICIENCY, THE OBS NUDGING FREQUENCY ',               &
        'FOR MESH #',I2,' IS ',1I2,' CGM TIMESTEPS ')
121   FORMAT('  WARNING: NOBS  = ',I4,' IS GREATER THAN NIOBF = ',       &
      I4,': INCREASE PARAMETER NIOBF')
5403  FORMAT(1H0,'-------------EOF REACHED FOR NVOL = ',I3,              &
       ' AND XTIME = ',F10.2,'-------------------')
160   FORMAT('****** CALL IN4DOB AT KTAU = ',I5,' AND XTIME = ',         &
      F10.2,':  NSTA = ',I7,' ******')
1449  FORMAT('*****NUDGING INDIVIDUAL OBS ON MESH #',I2,                 &
       ' WITH RINXY = ',                                                 &
      E11.3,' KM, RINSIG = ',E11.3,' AND TWINDO (HALF-PERIOD) = ',       &
      E11.3,' MIN')
1450  FORMAT(1X,'NUDGING IND. OBS WINDS WITH GIV = ',E11.3)
1451  FORMAT(1X,'NUDGING IND. OBS TEMPERATURE WITH GIT = ',E11.3)
1452  FORMAT(1X,'NUDGING IND. OBS MOISTURE WITH GIQ = ',E11.3)
1455  FORMAT(1X,'*** OBS WIND NUDGING FOR MESH ',I2,' IS TURNED OFF!!')
1456  FORMAT(1X,'*** OBS TEMPERATURE NUDGING FOR MESH ',I2,' IS TURNED OFF!!')
1457  FORMAT(1X,'*** OBS MOISTURE NUDGING FOR MESH ',I2,' IS TURNED OFF!!')

  RETURN
  END SUBROUTINE in4dob

  SUBROUTINE julgmt(mdate,julgmtn,timanl,julday,gmt,ind)





      INTEGER, intent(in) :: MDATE
      REAL, intent(out) :: JULGMTN
      REAL, intent(out) :: TIMANL
      INTEGER, intent(in) :: JULDAY
      REAL, intent(in) :: GMT
      INTEGER, intent(in) :: IND 


      real :: MO(12), rjulanl, houranl, rhr

      integer :: iyr, idate1, imo, idy, ihr, my1, my2, my3, ileap
      integer :: juldayn, juldanl, idymax, mm
      
      
      IF(IND.EQ.2)GOTO 150
      IYR=INT(MDATE/1000000.+0.001)
      IDATE1=MDATE-IYR*1000000
      IMO=INT(IDATE1/10000.+0.001)
      IDY=INT((IDATE1-IMO*10000.)/100.+0.001)
      IHR=IDATE1-IMO*10000-IDY*100
      MO(1)=31
      MO(2)=28

      IYR=IYR+1900
      MY1=MOD(IYR,4)
      MY2=MOD(IYR,100)
      MY3=MOD(IYR,400)
      ILEAP=0


      IF(MY1.EQ.0)THEN
        ILEAP=1
        MO(2)=29
      ENDIF
      IF(IND.EQ.1)GOTO 200
      MO(3)=31
      MO(4)=30
      MO(5)=31
      MO(6)=30
      MO(7)=31
      MO(8)=31
      MO(9)=30
      MO(10)=31
      MO(11)=30
      MO(12)=31
      JULDAYN=0
      DO 100 MM=1,IMO-1
        JULDAYN=JULDAYN+MO(MM)
 100     CONTINUE

      IF(IHR.GE.24)THEN
        IDY=IDY+1
        IHR=IHR-24
      ENDIF
      JULGMTN=(JULDAYN+IDY)*100.+IHR

 150   CONTINUE
      JULDANL=INT(JULGMTN/100.+0.000001)
      RJULANL=FLOAT(JULDANL)*100.
      HOURANL=JULGMTN-RJULANL
      TIMANL=(FLOAT(JULDANL-JULDAY)*24.-GMT+HOURANL)*60.
      RETURN
 200   CONTINUE
      RHR=GMT+TIMANL/60.+0.000001
      IDY=JULDAY
      IDYMAX=365+ILEAP
 300   IF(RHR.GE.24.0)THEN
        RHR=RHR-24.0
        IDY=IDY+1
        GOTO 300
      ENDIF
      IF(IDY.GT.IDYMAX)IDY=IDY-IDYMAX
      JULGMTN=FLOAT(IDY)*100.+RHR
      RETURN
  END SUBROUTINE julgmt

  SUBROUTINE rh2r(rh,t,p,r,iice)
 







      REAL, intent(in)  :: rh
      REAL, intent(in)  :: t
      REAL, intent(in)  :: p
      REAL, intent(out) :: r
      INTEGER, intent(in)  :: iice


      real eps, e0, eslcon1, eslcon2, esicon1, esicon2, t0, rh1
      real esat, rsat

      eps=0.62197
      e0=6.1078
      eslcon1=17.2693882
      eslcon2=35.86
      esicon1=21.8745584
      esicon2=7.66
      t0=260.
 

      rh1=rh*.01
 
      if(iice.eq.1.and.t.le.t0)then
        esat=e0*exp(esicon1*(t-273.16)/(t-esicon2))
      else
        esat=e0*exp(eslcon1*(t-273.16)/(t-eslcon2))
      endif
      rsat=eps*esat/(p-esat)

      r=rh1*rsat
 

 
      return
  END SUBROUTINE rh2r

  SUBROUTINE rh2rb(rh,t,p,r,iice)
 






 
      REAL, intent(in)  :: rh
      REAL, intent(in)  :: t
      REAL, intent(in)  :: p
      REAL, intent(out) :: r
      INTEGER, intent(in)  :: iice


      real eps, e0, eslcon1, eslcon2, esicon1, esicon2, t0, rh1
      real esat, rsat
      character(len=200) :: msg       

      eps=0.622
      e0=6.112
      eslcon1=17.67
      eslcon2=29.65
      esicon1=22.514
      esicon2=6.15e3
      t0=273.15
 
      write(msg,*) 'rh2r input=',rh,t,p
      call wrf_message(msg)
      rh1=rh*.01
 
      if(iice.eq.1.and.t.le.t0)then
        esat=e0*exp(esicon1-esicon2/t)
      else
        esat=e0*exp(eslcon1*(t-t0)/(t-eslcon2))
      endif
      rsat=eps*esat/(p-esat)

      r=rh1*eps*rsat/(eps+rsat*(1.-rh1))
 
      write(msg,*) 'rh2r rh,t,p,r=',rh1,t,p,r
      call wrf_message(msg)
      rh1=rh*.01
 
      return
END SUBROUTINE rh2rb

  SUBROUTINE set_projection (obs_proj, map_proj, cen_lat, cen_lon,     &
                             true_lat1, true_lat2, stand_lon,          &
                             known_lat, known_lon,                     &
                             e_we, e_sn, dxm, dym )

  USE module_llxy








      IMPLICIT NONE

  TYPE(PROJ_INFO), intent(out)  :: obs_proj   
  INTEGER, intent(in) :: map_proj             
  REAL, intent(in) :: cen_lat                 
  REAL, intent(in) :: cen_lon                 
  REAL, intent(in) :: true_lat1               
  REAL, intent(in) :: true_lat2               
  REAL, intent(in) :: stand_lon               
  INTEGER, intent(in) :: e_we                 
  INTEGER, intent(in) :: e_sn                 
  REAL, intent(in) :: known_lat               
  REAL, intent(in) :: known_lon               
  REAL, intent(in) :: dxm                     
  REAL, intent(in) :: dym                     


      CALL map_init(obs_proj)

      
      IF (map_proj == PROJ_MERC) THEN
         CALL map_set(PROJ_MERC, obs_proj,                                &
                      truelat1 = true_lat1,                               &
                      lat1     = known_lat,                               &
                      lon1     = known_lon,                               &
                      knowni   = 1.,                                      &
                      knownj   = 1.,                                      &
                      dx       = dxm)

      
      ELSE IF (map_proj == PROJ_LC) THEN
      CALL map_set(PROJ_LC, obs_proj,                                     &
                      truelat1 = true_lat1,                               &
                      truelat2 = true_lat2,                               &
                      stdlon   = stand_lon,                               &
                      lat1     = known_lat,                               &
                      lon1     = known_lon,                               &
                      knowni   = 1.,                                      &
                      knownj   = 1.,                                      &
                      dx       = dxm)

      
      ELSE IF (map_proj == PROJ_PS) THEN
         CALL map_set(PROJ_PS, obs_proj,                                  &
                      truelat1 = true_lat1,                               &
                      stdlon   = stand_lon,                               &
                      lat1     = known_lat,                               &
                      lon1     = known_lon,                               &
                      knowni   = 1.,                                      &
                      knownj   = 1.,                                      &
                      dx       = dxm)
      
      ELSE IF (map_proj == PROJ_CASSINI) THEN
         CALL map_set(PROJ_CASSINI, obs_proj,                             &
                      latinc   = dym*360.0/(2.0*EARTH_RADIUS_M*PI),       &
                      loninc   = dxm*360.0/(2.0*EARTH_RADIUS_M*PI),       &
                      lat1     = known_lat,                               &
                      lon1     = known_lon,                               &


                      lat0     = 90.0,                                    &
                      lon0     = 0.0,                                     &
                      knowni   = 1.,                                      &
                      knownj   = 1.,                                      &
                      stdlon   = stand_lon)

      
      ELSE IF (map_proj == PROJ_ROTLL) THEN
         CALL map_set(PROJ_ROTLL, obs_proj,                               &

                      ixdim    = e_we-1,                                  &
                      jydim    = e_sn-1,                                  &
                      phi      = real(e_sn-2)*dym/2.0,                    &
                      lambda   = real(e_we-2)*dxm,                        &
                      lat1     = cen_lat,                                 &
                      lon1     = cen_lon,                                 &
                      latinc   = dym,                                     &
                      loninc   = dxm,                                     &
                      stagger  = HH)

      END IF

  END SUBROUTINE set_projection

  SUBROUTINE fmt_date(idate,odate)                                             










      IMPLICIT NONE

      CHARACTER*14, intent(in)  :: idate        
      CHARACTER*19, intent(out) :: odate        

      odate(1:19) = "0000-00-00_00:00:00"
      odate(1:4)   = idate(1:4)                 
      odate(6:7)   = idate(5:6)                 
      odate(9:10)  = idate(7:8)                 
      odate(12:13) = idate(9:10)                
      odate(15:16) = idate(11:12)               
      odate(18:19) = idate(13:14)               

      RETURN
  END SUBROUTINE fmt_date

  INTEGER FUNCTION nvals_le_limit(isize, values, limit)






  IMPLICIT NONE

  INTEGER, INTENT(IN) :: isize           
  REAL,    INTENT(IN) :: values(isize)   
  REAL,    INTENT(IN) :: limit           


  integer :: n


   find_nvals: DO n = isize, 1, -1
                 if(values(n).le.limit) EXIT find_nvals
               ENDDO find_nvals
  nvals_le_limit = n

  RETURN
  END FUNCTION nvals_le_limit

  SUBROUTINE collect_obs_info(newpass,inest,n,latitude,longitude,             &
                              nlast,nprev,niobf,station_id,stnid,             &
                              rio,rjo,prt_max,prt_freq,xlat,xlong,            &
                              obs, lat,lon, mlat,mlon,                        &
                              e_we,e_sn,ims,ime,jms,jme,its,ite,jts,jte)


























  IMPLICIT NONE

  LOGICAL, intent(inout) :: newpass        
  INTEGER, intent(in)    :: inest          
  INTEGER, intent(in)    :: n              
  REAL,    intent(in)    :: latitude       
  REAL,    intent(in)    :: longitude      
  INTEGER, intent(in)    :: nlast          
  INTEGER, intent(inout) :: nprev          
  INTEGER, intent(in)    :: niobf          
  CHARACTER*15, intent(in) :: station_id   
  INTEGER, intent(in)    :: prt_max        
  INTEGER, intent(inout) :: stnid(40,prt_max) 
  REAL,    intent(in)    :: rio(niobf)     
  REAL,    intent(in)    :: rjo(niobf)     
  INTEGER, intent(in)    :: prt_freq       
  REAL, DIMENSION( ims:ime, jms:jme ),                                   &
           intent(in )   :: xlat, xlong    
  INTEGER, intent(inout) :: obs(prt_max)   
  REAL,    intent(inout) :: lat(prt_max)   
  REAL,    intent(inout) :: lon(prt_max)   
  REAL,    intent(inout) :: mlat(prt_max)  
  REAL,    intent(inout) :: mlon(prt_max)  
  INTEGER, intent(in)    :: e_we           
  INTEGER, intent(in)    :: e_sn           
  INTEGER, intent(in)    :: ims            
  INTEGER, intent(in)    :: ime            
  INTEGER, intent(in)    :: jms            
  INTEGER, intent(in)    :: jme            
  INTEGER, intent(in)    :: its            
  INTEGER, intent(in)    :: ite            
  INTEGER, intent(in)    :: jts            
  INTEGER, intent(in)    :: jte            


  integer i                       
  integer nn                      
  integer ndx,ndxp                
  real    :: ri, rj               
  integer :: ril, rjl             
  integer :: iend, jend           
  real    :: dxob, dyob           
  logical :: llsave               
  character(len=200) :: msg       

  if(newpass) then
    newpass = .false.
    nprev = nlast       
  endif





    if(prt_freq.gt.0) then
       ndx  = (n-nlast-1)/prt_freq + 1
       ndxp = (nprev-nlast-1)/prt_freq + 1
    else
       write(msg,*) 'STOP! OBS NAMELIST INPUT obs_prt_freq MUST BE GREATER THAN ZERO.'
       call wrf_message(msg)
       write(msg,*) 'THE NAMELIST VALUE IS',prt_freq,' FOR NEST ',inest
       call wrf_message(msg)
       call wrf_error_fatal3("wrf_fddaobs_in.b",1764,&
'wrf_fddaobs_in: in4dob STOP' )
    endif





    if(ndxp .lt. prt_max) then

   MODCHK : do nn = nprev+1, n
        llsave = .false.


        if( mod(nn-nlast-1,prt_freq) .eq. 0 ) then
           ndx = (nn-nlast-1)/prt_freq + 1
           if(ndx.gt.prt_max) EXIT MODCHK       
           llsave = .true.
        endif
        if(llsave) then


          obs(ndx) = nn
          lat(ndx) = latitude
          lon(ndx) = longitude


          do i = 1,15
            stnid(i,ndx) = ichar(station_id(i:i))
          enddo


          CALL get_model_latlon(nn,niobf,rio,rjo,xlat,xlong,e_we,e_sn,    &
                                ims,ime,jms,jme,its,ite,jts,jte,          &
                                mlat(ndx),mlon(ndx))
        endif  
      enddo MODCHK

    endif  


    nprev = n

  END SUBROUTINE collect_obs_info

  SUBROUTINE get_model_latlon(n,niobf,rio,rjo,xlat,xlong,e_we,e_sn,   &
                              ims,ime,jms,jme,its,ite,jts,jte,        &
                              mlat,mlon)





  IMPLICIT NONE

  INTEGER, intent(in)    :: n              
  INTEGER, intent(in)    :: niobf          
  REAL,    intent(in)    :: rio(niobf)     
  REAL,    intent(in)    :: rjo(niobf)     
  REAL, DIMENSION( ims:ime, jms:jme ),                                   &
           intent(in )   :: xlat, xlong    
  INTEGER, intent(in)    :: e_we           
  INTEGER, intent(in)    :: e_sn           
  INTEGER, intent(in)    :: ims            
  INTEGER, intent(in)    :: ime            
  INTEGER, intent(in)    :: jms            
  INTEGER, intent(in)    :: jme            
  INTEGER, intent(in)    :: its            
  INTEGER, intent(in)    :: ite            
  INTEGER, intent(in)    :: jts            
  INTEGER, intent(in)    :: jte            
  REAL,    intent(out)   :: mlat           
  REAL,    intent(out)   :: mlon           


  integer ndx                     
  real    :: ri, rj               
  integer :: ril, rjl             
  integer :: iend, jend           
  real    :: dxob, dyob           


  ri  = rio(n) - .5            
  rj  = rjo(n) - .5            
  ril = int(ri)
  rjl = int(rj)
  dxob = ri - float(ril)
  dyob = rj - float(rjl)
  iend = min(ite+1,e_we-2)
  jend = min(jte+1,e_sn-2)
  mlat = -999
  mlon = -999

  if(ri.ge.its .and. ri.lt.iend .and. rj.ge.jts .and. rj.lt.jend) then


     mlat = ((1.-dyob)*((1.-dxob)*xlat(ril,rjl)+             &
            dxob*xlat(ril+1,rjl)                             &
            )+dyob*((1.-dxob)*xlat(ril,rjl+1)+               &
            dxob*xlat(ril+1,rjl+1)))

     mlon = ((1.-dyob)*((1.-dxob)*xlong(ril,rjl)+            &
            dxob*xlong(ril+1,rjl)                            &
            )+dyob*((1.-dxob)*xlong(ril,rjl+1)+              &
            dxob*xlong(ril+1,rjl+1)))
  endif

  END SUBROUTINE get_model_latlon

  SUBROUTINE rotate_vector(lon,u,v,obs_proj,map_proj)

  USE module_llxy






  IMPLICIT NONE

  REAL,           intent(in)    :: lon        
  REAL,           intent(inout) :: u          
  REAL,           intent(inout) :: v          
  TYPE(PROJ_INFO),intent(in)    :: obs_proj   
  INTEGER,        intent(in)    :: map_proj   


  real diff, alpha
  double precision udbl, vdbl


  if (map_proj == PROJ_LC .or. map_proj == PROJ_PS) then

     diff = obs_proj%stdlon - lon
     if (diff > 180.) then
        diff = diff - 360.
     else if (diff < -180.) then
        diff = diff + 360.
     end if


     if (map_proj == PROJ_LC) then
        alpha = diff * obs_proj%cone * rad_per_deg * obs_proj%hemi
     else
        alpha = diff * rad_per_deg * obs_proj%hemi
     end if

     udbl = v*sin(alpha) + u*cos(alpha)
     vdbl = v*cos(alpha) - u*sin(alpha)
     u = udbl
     v = vdbl

  endif
  END SUBROUTINE rotate_vector





