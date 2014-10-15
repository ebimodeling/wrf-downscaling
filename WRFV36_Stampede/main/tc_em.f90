

PROGRAM tc_data
   USE module_machine
   USE module_domain, ONLY : domain, alloc_and_configure_domain, &
        domain_clock_set, head_grid, program_name, domain_clockprint, &
        set_current_grid_ptr
   USE module_io_domain
   USE module_initialize_real, ONLY : wrfu_initialize
   USE module_driver_constants
   USE module_configure, ONLY : grid_config_rec_type, model_config_rec, &
        initial_config, get_config_as_buffer, set_config_as_buffer
   USE module_timing
   USE module_state_description, ONLY: tconly

   USE module_dm, ONLY: wrf_dm_initialize




   USE module_symbols_util, ONLY: wrfu_cal_gregorian

   USE module_utility, ONLY : WRFU_finalize

   IMPLICIT NONE


   REAL    :: time , bdyfrq

   INTEGER :: loop , levels_to_process , debug_level


   TYPE(domain) , POINTER :: null_domain
   TYPE(domain) , POINTER :: grid , another_grid
   TYPE(domain) , POINTER :: grid_ptr , grid_ptr2
   TYPE (grid_config_rec_type)              :: config_flags
   INTEGER                :: number_at_same_level

   INTEGER :: max_dom, domain_id , grid_id , parent_id , parent_id1 , id
   INTEGER :: e_we , e_sn , i_parent_start , j_parent_start
   INTEGER :: idum1, idum2 

   INTEGER                 :: nbytes
   INTEGER, PARAMETER      :: configbuflen = 4* 65536
   INTEGER                 :: configbuf( configbuflen )
   LOGICAL , EXTERNAL      :: wrf_dm_on_monitor

   LOGICAL found_the_id

   INTEGER :: ids , ide , jds , jde , kds , kde
   INTEGER :: ims , ime , jms , jme , kms , kme
   INTEGER :: ips , ipe , jps , jpe , kps , kpe
   INTEGER :: ijds , ijde , spec_bdy_width
   INTEGER :: i , j , k , idts, rc
   INTEGER :: sibling_count , parent_id_hold , dom_loop

   CHARACTER (LEN=80)     :: message

   INTEGER :: start_year , start_month , start_day , start_hour , start_minute , start_second
   INTEGER ::   end_year ,   end_month ,   end_day ,   end_hour ,   end_minute ,   end_second
   INTEGER :: interval_seconds , real_data_init_type
   INTEGER :: time_loop_max , time_loop, bogus_id, storm
   real::t1,t2
   real    :: latc_loc(max_bogus),lonc_loc(max_bogus),vmax(max_bogus),rmax(max_bogus)
   real    :: rankine_lid
   INTERFACE
     SUBROUTINE Setup_Timekeeping( grid )
      USE module_domain, ONLY : domain
      TYPE(domain), POINTER :: grid
     END SUBROUTINE Setup_Timekeeping
   END INTERFACE

   CHARACTER (LEN=10) :: release_version = 'V3.6      '

   

   program_name = "TC_EM " // TRIM(release_version) // " PREPROCESSOR"






   IF ( .NOT. wrf_dm_on_monitor() ) THEN
      CALL wrf_error_fatal3("<stdin>",85,&
'TC bogus must run with a single processor only, re-run with num procs set to 1' )
   END IF



   CALL disable_quilting


   
   
   
   

   CALL       wrf_debug ( 100 , 'real_em: calling init_modules ' )
   CALL init_modules(1)   



   CALL WRFU_Initialize( defaultCalKind=WRFU_CAL_GREGORIAN, rc=rc )

   CALL init_modules(2)   

   


   IF ( wrf_dm_on_monitor() ) THEN
      CALL initial_config
   END IF
   CALL get_config_as_buffer( configbuf, configbuflen, nbytes )
   CALL wrf_dm_bcast_bytes( configbuf, nbytes )
   CALL set_config_as_buffer( configbuf, configbuflen )
   CALL wrf_dm_initialize





   CALL nl_get_debug_level ( 1, debug_level )
   CALL set_wrf_debug_level ( debug_level )

   CALL  wrf_message ( program_name )

   
   
   

   CALL nl_set_use_wps_input ( 1 , TCONLY )

   

   NULLIFY( null_domain )
   CALL       wrf_debug ( 100 , 'real_em: calling alloc_and_configure_domain ' )
   CALL alloc_and_configure_domain ( domain_id  = 1           , &
                                     grid       = head_grid   , &
                                     parent     = null_domain , &
                                     kid        = -1            )

   grid => head_grid
   CALL nl_get_max_dom ( 1 , max_dom )

   IF ( model_config_rec%interval_seconds .LE. 0 ) THEN
     CALL wrf_error_fatal3("<stdin>",147,&
'namelist value for interval_seconds must be > 0')
   END IF

   all_domains : DO domain_id = 1 , max_dom

      IF ( ( model_config_rec%input_from_file(domain_id) ) .OR. &
           ( domain_id .EQ. 1 ) ) THEN

         CALL Setup_Timekeeping ( grid )
         CALL set_current_grid_ptr( grid )
         CALL domain_clockprint ( 150, grid, &
                'DEBUG real:  clock after Setup_Timekeeping,' )
         CALL domain_clock_set( grid, &
                                time_step_seconds=model_config_rec%interval_seconds )
         CALL domain_clockprint ( 150, grid, &
                'DEBUG real:  clock after timeStep set,' )


         CALL       wrf_debug ( 100 , 'tc_em: calling set_scalar_indices_from_config ' )
         CALL set_scalar_indices_from_config ( grid%id , idum1, idum2 )





         CALL       wrf_debug ( 100 , 'tc_em: calling model_to_grid_config_rec ' )
         lonc_loc(:) = -999.
         latc_loc(:) = -999.
         vmax(:)     = -999.
         rmax(:)     = -999.
         CALL model_to_grid_config_rec ( grid%id , model_config_rec , config_flags )
         lonc_loc(1) = config_flags%lonc_loc
         latc_loc(1) = config_flags%latc_loc
         vmax(1)     = config_flags%vmax_meters_per_second
         rmax(1)     = config_flags%rmax
         rankine_lid = config_flags%rankine_lid
         do storm = 2,config_flags%num_storm
             bogus_id = storm
             CALL model_to_grid_config_rec ( bogus_id , model_config_rec , config_flags )
             lonc_loc(storm) = config_flags%lonc_loc
             latc_loc(storm) = config_flags%latc_loc
             vmax(storm)     = config_flags%vmax_meters_per_second
             rmax(storm)     = config_flags%rmax

         end do
         CALL model_to_grid_config_rec ( grid%id , model_config_rec , config_flags )

         

         CALL       wrf_debug ( 100 , 'tc_em: calling init_wrfio' )
         CALL init_wrfio

         
         


         CALL       wrf_debug ( 100 , 'tc_em: re-broadcast the configuration records' )
         CALL get_config_as_buffer( configbuf, configbuflen, nbytes )
         CALL wrf_dm_bcast_bytes( configbuf, nbytes )
         CALL set_config_as_buffer( configbuf, configbuflen )


         

         CALL       wrf_debug ( 100 , 'calling tc_med_sidata_input' )
         CALL tc_med_sidata_input ( grid , config_flags, latc_loc, lonc_loc, &
                                    vmax,rmax,rankine_lid)
         CALL       wrf_debug ( 100 , 'backfrom tc_med_sidata_input' )

      ELSE 
         CYCLE all_domains
      END IF

   END DO all_domains

   CALL set_current_grid_ptr( head_grid )

   

   CALL       wrf_debug (   0 , 'tc_em: SUCCESS COMPLETE TC BOGUS' )

   CALL wrf_shutdown

   CALL WRFU_Finalize( rc=rc )


END PROGRAM tc_data



SUBROUTINE tc_med_sidata_input ( grid , config_flags, latc_loc, lonc_loc, &
                                 vmax, rmax,rankine_lid)
  
   USE module_domain
   USE module_io_domain
  
   USE module_configure
   USE module_bc_time_utilities
   USE module_optional_input

   USE module_date_time
   USE module_utility

   IMPLICIT NONE


  
   INTERFACE
     SUBROUTINE start_domain ( grid , allowed_to_read )  
       USE module_domain
       TYPE (domain) grid
       LOGICAL, INTENT(IN) :: allowed_to_read
     END SUBROUTINE start_domain
   END INTERFACE

  
   TYPE(domain)                :: grid
   TYPE (grid_config_rec_type) :: config_flags
  
   INTEGER                :: time_step_begin_restart
   INTEGER                :: idsi , ierr , myproc, internal_time_loop,iflag

   INTEGER                ::nf_inq

   CHARACTER (LEN=80)     :: si_inpname
   CHARACTER (LEN=80)     :: message

   CHARACTER(LEN=19) :: start_date_char , end_date_char , current_date_char , next_date_char
   CHARACTER(LEN=8)  :: flag_name

   INTEGER :: time_loop_max , loop, rc,icnt,itmp
   INTEGER :: julyr , julday ,metndims, metnvars, metngatts, nunlimdimid,rcode
   REAL    :: gmt
   real    :: t1,t2,t3,t4
   real    :: latc_loc(max_bogus), lonc_loc(max_bogus)
   real    :: vmax(max_bogus),rmax(max_bogus),rankine_lid

   grid%input_from_file = .true.
   grid%input_from_file = .false.

   CALL tc_compute_si_start ( model_config_rec%start_year  (grid%id) , &
                                   model_config_rec%start_month (grid%id) , &
                                   model_config_rec%start_day   (grid%id) , &
                                   model_config_rec%start_hour  (grid%id) , &
                                   model_config_rec%start_minute(grid%id) , &
                                   model_config_rec%start_second(grid%id) , &
                                   model_config_rec%interval_seconds      , &
                                   model_config_rec%real_data_init_type   , &
                                   start_date_char)

   end_date_char = start_date_char
   IF ( end_date_char .LT. start_date_char ) THEN
      CALL wrf_error_fatal3("<stdin>",300,&
'Ending date in namelist ' // end_date_char // ' prior to beginning date ' // start_date_char )
   END IF
   print *,"the start date char ",start_date_char
   print *,"the end date char ",end_date_char

   time_loop_max = 1
   
   CALL domain_clock_set( grid, stop_timestr=end_date_char )

   
   CALL WRFU_ClockStopTimeDisable( grid%domain_clock, rc=rc ) 
   CALL wrf_check_error( WRFU_SUCCESS, rc, &
                         'WRFU_ClockStopTimeDisable(grid%domain_clock) FAILED', &
                         "tc_em.F" , &
                         312  )
   CALL domain_clockprint ( 150, grid, &
          'DEBUG med_sidata_input:  clock after stopTime set,' )

   
   
   current_date_char = start_date_char
   start_date = start_date_char // '.0000'
   current_date = start_date

   CALL nl_set_bdyfrq ( grid%id , REAL(model_config_rec%interval_seconds) )


   CALL cpu_time ( t1 )
   DO loop = 1 , time_loop_max

      internal_time_loop = loop
      IF ( ( grid%id .GT. 1 ) .AND. ( loop .GT. 1 ) .AND. &
           ( model_config_rec%grid_fdda(grid%id) .EQ. 0 ) .AND. &
           ( model_config_rec%sst_update .EQ. 0 ) ) EXIT

      print *,' '
      print *,'-----------------------------------------------------------------------------'
      print *,' '
      print '(A,I2,A,A,A,I4,A,I4)' , &
      ' Domain ',grid%id,': Current date being processed: ',current_date, ', which is loop #',loop,' out of ',time_loop_max

      

      CALL geth_julgmt ( config_flags%julyr , config_flags%julday , config_flags%gmt )

        print *,'configflags%julyr, %julday, %gmt:',config_flags%julyr, config_flags%julday, config_flags%gmt
      

      CALL nl_set_gmt (grid%id, config_flags%gmt)
      CALL nl_set_julyr (grid%id, config_flags%julyr)
      CALL nl_set_julday (grid%id, config_flags%julday)

      
      

      CALL cpu_time ( t3 )
      WRITE ( wrf_err_message , FMT='(A,A)' )'med_sidata_input: calling open_r_dataset for ', &
                                             TRIM(config_flags%auxinput1_inname)
      CALL wrf_debug ( 100 , wrf_err_message )
      IF ( config_flags%auxinput1_inname(1:8) .NE. 'wrf_real' ) THEN
         CALL construct_filename4a( si_inpname , config_flags%auxinput1_inname , grid%id , 2 , &
                                    current_date_char , config_flags%io_form_auxinput1 )
      ELSE
         CALL construct_filename2a( si_inpname , config_flags%auxinput1_inname , grid%id , 2 , &
                                    current_date_char )
      END IF
      CALL open_r_dataset ( idsi, TRIM(si_inpname) , grid , config_flags , "DATASET=AUXINPUT1", ierr )
      IF ( ierr .NE. 0 ) THEN
         CALL wrf_error_fatal3("<stdin>",369,&
'error opening ' // TRIM(si_inpname) // &
                               ' for input; bad date in namelist or file not in directory' )
      END IF

      

      CALL wrf_debug ( 100 , 'med_sidata_input: calling input_auxinput1' )
      CALL input_auxinput1 ( idsi ,   grid , config_flags , ierr )
      WRITE ( wrf_err_message , FMT='(A,I10,A)' ) 'Timing for input ',NINT(t4-t3) ,' s.'
      CALL wrf_debug( 0, wrf_err_message )

      

      CALL cpu_time ( t3 )
      CALL       wrf_debug ( 100 , 'med_sidata_input: calling init_module_optional_input' )
      CALL init_module_optional_input ( grid , config_flags )
      CALL       wrf_debug ( 100 , 'med_sidata_input: calling optional_input' )
      CALL optional_input ( grid , idsi , config_flags )



      flag_name(1:8) = 'SM000010'
      CALL wrf_get_dom_ti_integer ( idsi, 'FLAG_' // flag_name, itmp, 1, icnt, ierr ) 
      IF ( ierr .EQ. 0 ) THEN
          grid%flag_sm000010 = 1
      end if

       flag_name(1:8) = 'SM010040'
       CALL wrf_get_dom_ti_integer ( idsi, 'FLAG_' // flag_name, itmp, 1, icnt, ierr ) 
       IF ( ierr .EQ. 0 ) THEN
          grid%flag_sm010040 = 1
       end if

       flag_name(1:8) = 'SM040100'
       CALL wrf_get_dom_ti_integer ( idsi, 'FLAG_' // flag_name, itmp, 1, icnt, ierr ) 
       IF ( ierr .EQ. 0 ) THEN
            grid%flag_sm040100 = itmp   
       end if


       flag_name(1:8) = 'SM100200'
       CALL wrf_get_dom_ti_integer ( idsi, 'FLAG_' // flag_name, itmp, 1, icnt, ierr ) 
       IF ( ierr .EQ. 0 ) THEN
            grid%flag_sm100200 = itmp  
       end if









        flag_name(1:8) = 'ST000010'
        CALL wrf_get_dom_ti_integer ( idsi, 'FLAG_' // flag_name, itmp, 1, icnt, ierr ) 
        IF ( ierr .EQ. 0 ) THEN
            grid%flag_st000010 = 1
        END IF


         flag_name(1:8) = 'ST010040'
         CALL wrf_get_dom_ti_integer ( idsi, 'FLAG_' // flag_name, itmp, 1, icnt, ierr ) 
         IF ( ierr .EQ. 0 ) THEN
            grid%flag_st010040 = 1
         END IF

         flag_name(1:8) = 'ST040100'
         CALL wrf_get_dom_ti_integer ( idsi, 'FLAG_' // flag_name, itmp, 1, icnt, ierr ) 
         IF ( ierr .EQ. 0 ) THEN
            grid%flag_st040100 = 1
         END IF


         flag_name(1:8) = 'ST100200'
         CALL wrf_get_dom_ti_integer ( idsi, 'FLAG_' // flag_name, itmp, 1, icnt, ierr ) 
         IF ( ierr .EQ. 0 ) THEN
            grid%flag_st100200 = 1
         END IF

         CALL wrf_get_dom_ti_integer ( idsi, 'FLAG_SOIL_LAYERS', itmp, 1, icnt, ierr ) 
         IF ( ierr .EQ. 0 ) THEN
            grid%flag_soil_layers = 1
         END IF




      CALL close_dataset ( idsi , config_flags , "DATASET=AUXINPUT1" )
      CALL cpu_time ( t4 )

      
      
      
      
      CALL cpu_time ( t3 )
      already_been_here = .FALSE.
      CALL model_to_grid_config_rec ( grid%id , model_config_rec , config_flags )


      CALL cpu_time ( t3 )

      CALL assemble_output ( grid , config_flags , loop , time_loop_max, current_date_char, &
                             latc_loc, lonc_loc, vmax, rmax, rankine_lid,si_inpname)
      CALL cpu_time ( t4 )
      WRITE ( wrf_err_message , FMT='(A,I10,A)' ) 'Timing for output ',NINT(t4-t3) ,' s.'
      CALL wrf_debug( 0, wrf_err_message )
      CALL cpu_time ( t2 )
      WRITE ( wrf_err_message , FMT='(A,I4,A,I10,A)' ) 'Timing for loop # ',loop,' = ',NINT(t2-t1) ,' s.'
      CALL wrf_debug( 0, wrf_err_message )

      CALL cpu_time ( t1 )
   END DO

END SUBROUTINE tc_med_sidata_input



SUBROUTINE tc_compute_si_start(  &
   start_year , start_month , start_day , start_hour , start_minute , start_second , &
   interval_seconds , real_data_init_type , &
   start_date_char)

   USE module_date_time

   IMPLICIT NONE

   INTEGER :: start_year , start_month , start_day , start_hour , start_minute , start_second
   INTEGER ::   end_year ,   end_month ,   end_day ,   end_hour ,   end_minute ,   end_second
   INTEGER :: interval_seconds , real_data_init_type
   INTEGER :: time_loop_max , time_loop

   CHARACTER(LEN=19) :: current_date_char , start_date_char , end_date_char , next_date_char





   WRITE ( start_date_char , FMT = '(I4.4,"-",I2.2,"-",I2.2,"_",I2.2,":",I2.2,":",I2.2)' ) &
           start_year,start_month,start_day,start_hour,start_minute,start_second



END SUBROUTINE tc_compute_si_start


SUBROUTINE assemble_output ( grid , config_flags , loop , time_loop_max,current_date_char, &
                             latc_loc, lonc_loc,vmax,rmax,rankine_lid,si_inpname)

   USE module_big_step_utilities_em
   USE module_domain
   USE module_io_domain
   USE module_configure
   USE module_date_time
   USE module_bc
   IMPLICIT NONE

   TYPE(domain)                 :: grid
   TYPE (grid_config_rec_type)  :: config_flags

   INTEGER , INTENT(IN)         :: loop , time_loop_max



   real    :: vmax(max_bogus),vmax_ratio,rankine_lid
   real    :: rmax(max_bogus),stand_lon,cen_lat,ptop_in_pa
   real    :: latc_loc(max_bogus),lonc_loc(max_bogus)

   INTEGER :: ijds , ijde , spec_bdy_width
   INTEGER :: i , j , k , idts,map_proj,remove_only,storms

   INTEGER :: id1 , interval_seconds , ierr, rc, sst_update, grid_fdda
   INTEGER , SAVE :: id, id2,  id4 
   CHARACTER (LEN=80) :: tcoutname , bdyname,si_inpname
   CHARACTER(LEN= 4) :: loop_char
   CHARACTER(LEN=19) ::  current_date_char
   
character *19 :: temp19
character *24 :: temp24 , temp24b

real::t1,t2,truelat1,truelat2


   

   spec_bdy_width = model_config_rec%spec_bdy_width
   interval_seconds = model_config_rec%interval_seconds
   sst_update = model_config_rec%sst_update
   grid_fdda = model_config_rec%grid_fdda(grid%id)
   truelat1  = config_flags%truelat1
   truelat2  = config_flags%truelat2

   stand_lon = config_flags%stand_lon
   cen_lat   = config_flags%cen_lat
   map_proj  = config_flags%map_proj

   vmax_ratio = config_flags%vmax_ratio
   ptop_in_pa = config_flags%p_top_requested
   remove_only = 0
   if(config_flags%remove_storm) then
      remove_only = 1
   end if

   storms = config_flags%num_storm
   print *,"number of storms ",config_flags%num_storm
   call tc_bogus(cen_lat,stand_lon,map_proj,truelat1,truelat2, &
                 grid%dx,grid%e_we,grid%e_sn,grid%num_metgrid_levels,ptop_in_pa, &
                 rankine_lid,latc_loc,lonc_loc,vmax,vmax_ratio,rmax,remove_only, &
                 storms,grid)



   
   CALL construct_filename4a( tcoutname , config_flags%auxinput1_outname , grid%id , 2 , &
                                    current_date_char , config_flags%io_form_auxinput1 )

   print *,"outfile name from construct filename ",tcoutname
   CALL open_w_dataset ( id1, TRIM(tcoutname) , grid , config_flags ,output_auxinput1,"DATASET=AUXINPUT1",ierr )
   IF ( ierr .NE. 0 ) THEN
        CALL wrf_error_fatal3("<stdin>",589,&
'tc_em: error opening tc bogus file for writing' )
   END IF
   CALL output_auxinput1( id1, grid , config_flags , ierr )
   CALL close_dataset ( id1 , config_flags , "DATASET=AUXINPUT1" )


END SUBROUTINE assemble_output



SUBROUTINE tc_bogus(centerlat,stdlon,nproj,truelat1,truelat2,dsm,ew,ns,nz,ptop_in_pa, &
                    rankine_lid,latc_loc,lonc_loc,vmax,vmax_ratio,rmax,remove_only, &
                    storms,grid)




















  

  






















  USE module_llxy

  USE module_domain



  IMPLICIT NONE 
  TYPE(domain)                 :: grid
  integer ew,ns,nz
  integer nproj
  integer storms,nstrm
  real :: centerlat,stdlon,conef,truelat1,truelat2,dsm,dx,rankine_lid
  real :: latc_loc(max_bogus),lonc_loc(max_bogus),vmax(max_bogus),vmax_ratio,rmax(max_bogus)
  
  real :: press(ew-1,nz,ns-1),rhmx(nz), vwgt(nz),old_slp(ew-1,ns-1)
  real, dimension(:,:,:) , allocatable :: u11,v11,t11,rh11,phi11
  real, dimension(:,:,:) , allocatable :: u1 , v1 , t1 , rh1 , phi1
  real, dimension(ew-1,ns-1) :: lond,terrain,cor,pslx



  real, dimension(ew,ns-1)    :: msfu   
  real, dimension(ew-1,ns)    :: msfv   
  real, dimension(ew-1,ns-1)  :: msfm   

  CHARACTER*2  jproj
  LOGICAL :: l_tcbogus


  real :: r_search,r_vor,beta,devps,humidity_max
  real :: devpc,const,r_vor2,cnst,alphar,epsilon,vormx , rad , sum_q 
  real :: avg_q ,q_old,ror,q_new,dph,dphx0
  real :: rh_max,min_RH_value,ps
  integer :: vert_variation
  integer :: i,k,j,kx,remove_only
  integer :: k00,kfrm ,kto ,k85,n_iter,ew_mvc,ns_mvc,nct,itr
  integer :: strmci(nz), strmcj(nz)
  real :: disx,disy,alpha,degran,pie,rovcp,cp
  REAL :: rho,pprm,phip0,x0,y0,vmx,xico,xjco,xicn,xjcn,p85,xlo,rconst,ew_gcntr,ns_gcntr
  real :: ptop_in_pa,themax,themin
  real :: latinc,loninc
  real :: rtemp,colat0,colat
  REAL :: q1(ew-1,nz,ns-1), psi1(ew-1,nz,ns-1) 


  TYPE(proj_info) :: proj

  

  REAL :: lat1 , lon1

   real :: knowni,knownj


   REAL utcr(ew,nz,ns-1),  vtcr(ew-1,nz,ns)
   REAL utcp(ew,nz,ns-1),  vtcp(ew-1,nz,ns)
   REAL psitc(ew-1,nz,ns-1), psiv(nz)
   REAL vortc(ew-1,nz,ns-1), vorv(nz)
   REAL tptc(ew-1,nz,ns-1)
   REAL phiptc(ew-1,nz,ns-1)


   REAL uuwork(nz), vvwork(nz), temp2(ew,ns)
   REAL vort(ew-1,nz,ns-1), div(ew-1,nz,ns-1)
   REAL vortsv(ew-1,nz,ns-1)
   REAL theta(ew-1,nz,ns-1), t_reduce(ew-1,nz,ns-1)
   REAL ug(ew,nz,ns-1),   vg(ew-1,nz,ns),  vorg(ew-1,nz,ns-1)
   REAL delpx(ew-1,ns-1)


   REAL outold(ew-1,ns-1)
   REAL rd(ew-1,ns-1),     ff(ew-1,ns-1)
   REAL tmp1(ew-1,ns-1),   tmp2(ew-1,ns-1) 


   REAL , DIMENSION (ew-1,nz,ns-1) :: t0, t00, rh0, q0, phi0, psi0, chi


   REAL , DIMENSION (ew-1,nz,ns-1) :: psipos, tpos, psi ,phipos, phip
      

   REAL  u2(ew,nz,ns-1),  v2(ew-1,nz,ns)                         
   REAL  t2(ew-1,nz,ns-1),z2(ew-1,nz,ns-1)                      
   REAL  phi2(ew-1,nz,ns-1),rh2(ew-1,nz,ns-1)
      
   print *,"the dimensions: north-south = ",ns," east-west =",ew
   IF (nproj .EQ. 1) THEN
        jproj = 'LC'
        print *,"Lambert Conformal projection"
   ELSE IF (nproj .EQ. 2) THEN
        jproj = 'ST'
   ELSE IF (nproj .EQ. 3) THEN
        jproj = 'ME'
        print *,"A mercator projection"
   END IF


  knowni = 1.
  knownj = 1.
  pie     = 3.141592653589793
  degran = pie/180.
  rconst = 287.04
  min_RH_value = 5.0
  cp = 1004.0
  rovcp = rconst/cp
   
   r_search = 400000.0
   r_vor = 300000.0
   r_vor2 = r_vor * 4
   beta = 0.5
   devpc= 40.0
   vert_variation = 1   
   humidity_max   = 95.0 
   alphar         = 1.8
   latinc        = -999.
   loninc        = -999.

   if(remove_only .eq. 1) then
     do nstrm=1,storms
         vmax(nstrm) = 0.1
     end do
   end if

  
  
  
  
  
   
   dx = dsm
   lat1 = grid%xlat_gc(1,1)
   lon1 = grid%xlong_gc(1,1)
   IF( jproj .EQ. 'ME' )THEN
       IF ( lon1  .LT. -180. ) lon1  = lon1  + 360.
       IF ( lon1  .GT.  180. ) lon1  = lon1  - 360.
       IF ( stdlon .LT. -180. ) stdlon = stdlon + 360.
       IF ( stdlon .GT.  180. ) stdlon = stdlon - 360.
       CALL map_set ( proj_merc, proj, lat1, lon1, lat1, lon1, knowni, knownj, dx, &
                      latinc,loninc,stdlon , truelat1 , truelat2)
       conef = 0.
   ELSE IF ( jproj .EQ. 'LC' ) THEN
        if((truelat1 .eq. 0.0)  .and. (truelat2 .eq. 0.0)) then
            print *,"Truelat1 and Truelat2 are both 0"
            stop
         end if
        CALL map_set (proj_lc,proj, lat1, lon1, lat1, lon1, knowni, knownj, dx, &
                       latinc,loninc,stdlon , truelat1 , truelat2)
       conef = proj%cone
   ELSE IF ( jproj .EQ. 'ST' ) THEN
        conef = 1.
        CALL map_set ( proj_ps,proj,lat1, lon1, lat1, lon1, knowni, knownj, dx, &
                      latinc,loninc,stdlon , truelat1 , truelat2)
   END IF


 kx = nz
 do j = 1,ns-1
    do k = 1,nz
       do i = 1,ew-1
           press(i,k,j) = grid%p_gc(i,k,j)*0.01
       end do
    end do
 end do




   IF ( ( ptop_in_pa .EQ. 40000. ) .OR. ( ptop_in_pa .EQ. 60000. ) ) THEN
         PRINT '(A)','Hold on pardner, your value for PTOP is gonna cause problems for the TC bogus option.'
         PRINT '(A)','Make it higher up than 400 mb.'
         STOP 'ptop_woes_for_tc_bogus'
   END IF

 IF ( vert_variation .EQ. 1 ) THEN
    DO k=1,kx
       IF ( press(1,k,1) .GT. 400. ) THEN
               rhmx(k) = humidity_max
       ELSE
               rhmx(k) = humidity_max * MAX( 0.1 , (press(1,k,1) - ptop_in_pa/100.)/(400.-ptop_in_pa/100.) )
       END IF

        IF ( press(1,k,1) .GT. 600. ) THEN
             vwgt(k) = 1.0
        ELSE IF ( press(1,k,1) .LE. 100. ) THEN
             vwgt(k) = 0.0001
        ELSE
             vwgt(k) = MAX ( 0.0001 , (press(1,k,1)-ptop_in_pa/100.)/(600.-ptop_in_pa/100.) )
        END IF
      END DO

 ELSE IF ( vert_variation .EQ. 2 ) THEN
         IF ( kx .eq. 24 ) THEN
            rhmx = (/ 95.,       95., 95., 95., 95., 95., 95., 95.,      &
                      95., 95.,  95., 95., 95., 90., 85., 80., 75.,      &
                      70., 66.,  60., 39., 10., 10., 10./)
            vwgt = (/ 1.0000,         1.0000, 1.0000, 1.0000, 1.0000, 1.0000, 1.0000, 0.9850,      &
                      0.9680, 0.9500, 0.9290, 0.9060, 0.8810, 0.8500, 0.7580, 0.6500, 0.5100,      &
                      0.3500, 0.2120, 0.0500, 0.0270, 0.0001, 0.0001, 0.0001/)
         ELSE
            PRINT '(A)','Number of vertical levels assumed to be 24 for AFWA TC bogus option'
            STOP 'AFWA_TC_BOGUS_LEVEL_ERROR'
         END IF
 END IF









 allocate(u11 (1:ew, 1:nz, 1:ns-1))
 allocate(u1  (1:ew, 1:nz, 1:ns-1))      
 allocate(v11 (1:ew-1, 1:nz, 1:ns))
 allocate(v1  (1:ew-1, 1:nz, 1:ns))
 do j = 1,ns-1
    do k = 1,nz
       do i = 1,ew
            u11(i,k,j) = grid%u_gc(i,k,j)
             u1(i,k,j) = grid%u_gc(i,k,j)
             msfu(i,j) = grid%msfu(i,j) 
       end do
    end do
 end do


 do j = 1,ns
    do k = 1,nz
       do i = 1,ew-1
            v11(i,k,j) = grid%v_gc(i,k,j)
             v1(i,k,j) = grid%v_gc(i,k,j)
           msfv(i,j)   = grid%msfv(i,j)  
       end do
    end do
 end do





 allocate(t11  (1:ew-1, 1:nz, 1:ns-1))
 allocate(t1   (1:ew-1, 1:nz, 1:ns-1))
 allocate(rh11 (1:ew-1, 1:nz, 1:ns-1))
 allocate(rh1  (1:ew-1, 1:nz, 1:ns-1))
 allocate(phi11(1:ew-1, 1:nz, 1:ns-1))
 allocate(phi1 (1:ew-1, 1:nz, 1:ns-1))
 do j = 1,ns-1
    do k = 1,nz
       do i = 1,ew-1
             t11(i,k,j)  =  grid%t_gc(i,k,j)
              t1(i,k,j)  =  grid%t_gc(i,k,j)
            rh11(i,k,j)  =  grid%rh_gc(i,k,j)
             rh1(i,k,j)  =  grid%rh_gc(i,k,j)
              msfm(i,j)  = grid%msft(i,j)
            if(k .eq. 1)then
               phi11(i,k,j) =  grid%ht_gc(i,j)
               phi1(i,k,j)  =  grid%ht_gc(i,j) * 9.81
            else
               phi11(i,k,j) =  grid%ght_gc(i,k,j)
               phi1(i,k,j)  =  grid%ght_gc(i,k,j) * 9.81 
            end if
       end do
    end do
 end do



 do j = 1,ns-1
    do i = 1,ew-1
       pslx(i,j)    = grid%pslv_gc(i,j) * 0.01
       cor(i,j)     = grid%f(i,j)               
       lond(i,j)    = grid%xlong_gc(i,j)
       terrain(i,j) = grid%ht_gc(i,j)
       old_slp(i,j) = grid%pslv_gc(i,j)
    end do
 end do




   
 l_tcbogus = .FALSE.
 all_storms : DO nstrm=1,storms



 if(rmax(nstrm) .eq. -999.) then
    print *,"Please enter a value for rmax in the namelist"
    stop
 end if


 k00  = 2
 kfrm = k00
 p85  = 850.

 kto  = kfrm
 DO k=kfrm+1,kx
     IF ( press(1,k,1) .GE. p85 ) THEN
           kto = kto + 1
     END IF
 END DO
 k85 = kto 



 rho  = 1.2
 pprm = devpc*100.
 phip0= pprm/rho 




 CALL latlon_to_ij ( proj , latc_loc(nstrm) , lonc_loc(nstrm) , x0 , y0 )
 IF ( ( x0 .LT. 1. ) .OR. ( x0 .GT. REAL(ew-1) ) .OR. &
              ( y0 .LT. 1. ) .OR. ( y0 .GT. REAL(ns-1) ) ) THEN
         PRINT '(A,I3,A,A,A)','         Storm position is outside the computational domain.'
         PRINT '(A,2F6.2,A)' ,'         Storm postion: (x,y) = ',x0,y0,'.'
         stop
 END IF

 l_tcbogus = .TRUE.

 vmx = vmax(nstrm)  * vmax_ratio

 IF (  latc_loc(nstrm) .LT. 0.  ) THEN
       vmx = -vmx
 END IF
   
 IF (  vmax(nstrm)  .LE. 0.  ) THEN
       vmx = SQRT( 2.*(1-beta)*ABS(phip0) )  
 END IF

 ew_gcntr    = x0  
 ns_gcntr    = y0  



 ew_gcntr    = x0 + 0.5
 ns_gcntr    = y0 + 0.5

 n_iter  = 1



 PRINT '(/,A,I3,A,A,A)'     ,'---> TC: Processing storm number= ',nstrm
 PRINT '(A,F6.2,A,F7.2,A)'  ,'         Storm center lat= ',latc_loc(nstrm),' lon= ',lonc_loc(nstrm),'.'
 PRINT '(A,2F6.2,A)'        ,'         Storm center grid position (x,y)= ',ew_gcntr,ns_gcntr,'.'
 PRINT '(A,F5.2,F9.2,A)'    ,'         Storm max wind (m/s) and max radius (m)= ',vmx,rmax(nstrm),'.'
 PRINT '(A,F5.2,A)'         ,'         Estimated central press dev (mb)= ',devpc,'.'




  DO k=1,kx
     strmci(k) = 1
     strmcj(k) = 1
  END DO
 




  utcp(:,:,:) = 0.0
  vtcp(:,:,:) = 0.0
  print *,"nstrm  ",rmax(nstrm),ew_gcntr,ns_gcntr
  DO j=1,ns-1
     DO i=1,ew-1
        disx = REAL(i) - ew_gcntr 
        disy = REAL(j) - ns_gcntr 
        CALL rankine(disx,disy,dx,kx,vwgt,rmax(nstrm),vmx,uuwork,vvwork,psiv,vorv)
        DO k=1,kx
            utcp(i,k,j) = uuwork(k)
            vtcp(i,k,j) = vvwork(k)
           psitc(i,k,j) = psiv(k)
           vortc(i,k,j) = vorv(k)
        END DO
     END DO
  END DO
  call stagger_rankine_winds(utcp,vtcp,ew,ns,nz)


  utcr(:,:,:) = 0.0
  vtcr(:,:,:) = 0.0

  DO j=1,ns-1
     DO i=2,ew-1
        xlo = stdlon-grid%xlong_u(i,j)
        IF ( xlo .GT. 180.)xlo = xlo-360.
        IF ( xlo .LT.-180.)xlo = xlo+360.
   
        alpha = xlo*conef*degran*SIGN(1.,centerlat)
        DO k=1,kx
           utcr(i,k,j) = (vtcp(i-1,k,j)+vtcp(i,k,j)+vtcp(i,k,j+1)+vtcp(i-1,k,j+1))/4 *SIN(alpha)+utcp(i,k,j)*COS(alpha)
           if(utcr(i,k,j) .gt. 300.) then
              print *,i,k,j,"a very bad value of utcr"
              stop
           end if           
        END DO
     END DO
  END DO


  DO j=2,ns-1
     DO i=1,ew-1
        xlo = stdlon-grid%xlong_v(i,j)
        IF ( xlo .GT. 180.)xlo = xlo-360.
        IF ( xlo .LT.-180.)xlo = xlo+360.
   
        alpha = xlo*conef*degran*SIGN(1.,centerlat)
        DO k=1,kx
           vtcr(i,k,j) = vtcp(i,k,j)*COS(alpha)-(utcp(i,k,j-1)+utcp(i+1,k,j-1)+utcp(i+1,k,j)+utcp(i,k,j))/4*SIN(alpha)
           if(vtcr(i,k,j) .gt. 300.) then
              print *,i,k,j,"a very bad value of vtcr"
              stop
           end if
        END DO
     END DO
  END DO



  do j = 1,ns-1
     utcr(1,:,j)  = utcr(2,:,j)
     utcr(ew,:,j) = utcr(ew-1,:,j)
 end do


  do i = 1,ew-1
     vtcr(i,:,1)  = vtcr(i,:,2)
     vtcr(i,:,ns) = vtcr(i,:,ns-1)
  end do

  



   CALL vor(u1,v1,msfu,msfv,msfm,ew,ns,kx,dx,vort)



   CALL diverg(u1,v1,msfu,msfv,msfm,ew,ns,kx,dx,div)



   CALL mxratprs(rh1,t1,press*100.,ew,ns,kx,q1,min_RH_value)
   q1(:,1,:) = q1(:,2,:)



   vortsv = vort
   q0 = q1
   


   DO k=1,kx 
      DO j=1,ns-1
         DO i=1,ew-1
            ff(i,j) = vort(i,k,j)
            tmp1(i,j)= 0.0
         END DO
      END DO
      epsilon = 1.E-2
      CALL relax(tmp1,ff,rd,ew,ns,dx,epsilon,alphar)
      DO j=1,ns-1
         DO i=1,ew-1
            psi1(i,k,j) = tmp1(i,j)
         END DO
      END DO
   END DO

   
   DO k=1,kx  
      IF ( latc_loc(nstrm) .GE. 0. ) THEN
           vormx = -1.e10
      ELSE
           vormx =  1.e10
      END IF
   
      ew_mvc = 1
      ns_mvc = 1

      DO j=1,ns-1
         DO i=1,ew-1
            rad = SQRT((REAL(i)-ew_gcntr)**2.+(REAL(j)-ns_gcntr)**2.)*dx
            IF ( rad .LE. r_search ) THEN
               IF ( latc_loc(nstrm) .GE. 0. ) THEN
                   IF ( vortsv(i,k,j) .GT. vormx ) THEN
                        vormx = vortsv(i,k,j)
                        ew_mvc = i
                        ns_mvc = j
                    END IF
               ELSE IF (latc_loc(nstrm) .LT. 0. ) THEN
                    IF ( vortsv(i,k,j) .LT. vormx ) THEN
                         vormx = vortsv(i,k,j)
                         ew_mvc = i
                         ns_mvc = j
                    END IF
               END IF
            END IF
         END DO
      END DO
      
      strmci(k) = ew_mvc 
      strmcj(k) = ns_mvc

      DO j=1,ns-1
         DO i=1,ew-1
            rad = SQRT(REAL((i-ew_mvc)**2.+(j-ns_mvc)**2.))*dx
            IF ( rad .GT. r_vor ) THEN
                 vort(i,k,j) = 0.
                 div(i,k,j)  = 0.
            END IF
         END DO
      END DO   

      DO itr=1,n_iter
         sum_q = 0.
         nct = 0
         DO j=1,ns-1
            DO i=1,ew-1
               rad = SQRT(REAL(i-ew_mvc)**2.+REAL(j-ns_mvc)**2.)*dx
               IF ( (rad .LT. r_vor2).AND.(rad .GE. 0.8*r_vor2) ) THEN
                     sum_q = sum_q + q0(i,k,j)
                     nct = nct + 1
               END IF
             END DO
          END DO
          avg_q = sum_q/MAX(REAL(nct),1.)
   
          DO j=1,ns-1
             DO i=1,ew-1
                 q_old = q0(i,k,j)
                 rad = SQRT(REAL(i-ew_mvc)**2.+REAL(j-ns_mvc)**2.)*dx
                 IF ( rad .LT. r_vor2 ) THEN
                      ror = rad/r_vor2
                      q_new = ((1.-ror)*avg_q) + (ror*q_old)
                      q0(i,k,j) = q_new
                 END IF
              END DO
           END DO
     END DO 
 END DO 



   DO k=1,kx
      DO j=1,ns-1
         DO i=1,ew-1
            ff(i,j) = div(i,k,j)
            tmp1(i,j)= 0.0
         END DO
      END DO

      epsilon = 1.e-2
      CALL relax(tmp1,ff,rd,ew,ns,dx,epsilon,alphar)
      DO j=1,ns-1
         DO i=1,ew-1
            chi(i,k,j) = tmp1(i,j)
         END DO
      END DO
    END DO 





     DO k=1,kx 
         DO j=1,ns-1
            DO i=1,ew-1
               ff(i,j)=vort(i,k,j)
               tmp1(i,j)=0.0
            END DO
         END DO
         epsilon = 1.e-2
         CALL relax(tmp1,ff,rd,ew,ns,dx,epsilon,alphar)
         DO j=1,ns-1
            DO i=1,ew-1
               psi(i,k,j)=tmp1(i,j)
            END DO
         END DO
     END DO


 
   call final_ew_velocity(u2,u1,chi,psi,utcr,dx,ew,ns,nz)
   call final_ns_velocity(v2,v1,chi,psi,vtcr,dx,ew,ns,nz)

     DO k=1,kx
        DO j=1,ns-1
           DO i=1,ew-1
              psi0(i,k,j) = psi1(i,k,j)-psi(i,k,j)
           END DO
        END DO
     END DO

     DO k=k00,kx
        DO j=1,ns-1
           DO i=1,ew-1
              psipos(i,k,j)=psi(i,k,j)
           END DO
        END DO
     END DO






     CALL geowind(phi1,ew,ns,kx,dx,ug,vg)
     CALL vor(ug,vg,msfu,msfv,msfm,ew,ns,kx,dx,vorg)

     DO k=1,kx
        ew_mvc = strmci(k)
        ns_mvc = strmcj(k)

         DO j=1,ns-1
           DO i=1,ew-1
               rad = SQRT(REAL(i-ew_mvc)**2.+REAL(j-ns_mvc)**2.)*dx
               IF ( rad .GT. r_vor ) THEN
                    vorg(i,k,j) = 0.
               END IF
           END DO
         END DO
     END DO
   
      DO k=k00,kx
         DO j=1,ns-1
            DO i=1,ew-1
               ff(i,j) = vorg(i,k,j)
               tmp1(i,j)= 0.0
            END DO
         END DO
         epsilon = 1.e-3
         CALL relax(tmp1,ff,rd,ew,ns,dx,epsilon,alphar)
         DO j=1,ns-1
            DO i=1,ew-1
               phip(i,k,j) = tmp1(i,j)
            END DO
         END DO
     END DO


     
     DO k=k00,kx
         DO j=1,ns-1
            DO i=1,ew-1
               phi0(i,k,j) = phi1(i,k,j) - phip(i,k,j) 
            END DO
         END DO
     END DO


     
     DO k=k00,kx 
        DO j=1,ns-1
           DO i=1,ew-1
              IF( k .EQ.  2 ) THEN
                  tpos(i,k,j) = (-1./rconst)*(phip(i,k+1,j)-phip(i,k,j  ))/LOG(press(i,k+1,j)/press(i,k,j))
              ELSE IF ( k .EQ. kx ) THEN
                  tpos(i,k,j) = (-1./rconst)*(phip(i,k  ,j)-phip(i,k-1,j))/LOG(press(i,k,j  )/press(i,k-1,j))
              ELSE
                  tpos(i,k,j) = (-1./rconst)*(phip(i,k+1,j)-phip(i,k-1,j))/LOG(press(i,k+1,j)/press(i,k-1,j))
              END IF
              t0(i,k,j) = t1(i,k,j)-tpos(i,k,j)
              t00(i,k,j) = t0(i,k,j)
              if(t0(i,k,j) .gt. 400) then
                 print *,"interesting temperature ",t0(i,k,j)," at ",i,j,k
                 stop
              end if
           END DO
        END DO
     END DO

     
     CALL qvtorh (q0,t0,press*100.,k00,ew,ns,kx,rh0,min_RH_value)
     call final_RH(rh2,rh0,rhmx,strmci,strmcj,rmax(nstrm),ew,ns,nz,k00,dx,ew_gcntr,ns_gcntr,r_vor2)



     
     DO k=k00,kx
        DO j=1,ns-1
           DO i=1,ew-1
              theta(i,k,j) = t1(i,k,j)*(1000./press(i,k,j))**rovcp
           END DO
        END DO
     END DO


     ew_mvc = strmci(k00)
     ns_mvc = strmcj(k00)
     DO k=kfrm,kto
        DO j=1,ns-1
           DO i=1,ew-1
              rad = SQRT(REAL(i-ew_mvc)**2.+REAL(j-ns_mvc)**2.)*dx
              IF ( rad .LT. r_vor2 ) THEN
                  t_reduce(i,k,j) = theta(i,k85,j)-0.03*(press(i,k,j)-press(i,k85,j))
                  t0(i,k,j) = t00(i,k,j)*(rad/r_vor2) + (((press(i,k,j)/1000.)**rovcp)*t_reduce(i,k,j))*(1.-(rad/r_vor2))
              END IF
           END DO
        END DO
     END DO

    
    DO k=1,kx
       DO j=1,ns-1
          DO i=1,ew-1
              tmp1(i,j)=psitc(i,k,j)
          END DO
       END DO
       CALL balance(cor,tmp1,ew,ns,dx,outold)
       DO j=1,ns-1
          DO i=1,ew-1
             ff(i,j)=outold(i,j)
             tmp1(i,j)=0.0
          END DO
       END DO
       epsilon = 1.e-3
       CALL relax (tmp1,ff,rd,ew,ns,dx,epsilon,alphar)
       DO j=1,ns-1
          DO i=1,ew-1
             phiptc(i,k,j) = tmp1(i,j)
          END DO
       END DO
    END DO     



   DO j=1,ns-1
      DO k=1,kx
         DO i=1,ew-1
            phi2(i,k,j)  = phi0(i,k,j) + phiptc(i,k,j)
         END DO
      END DO
   END DO


   
    DO j=1,ns-1
       DO k=k00,kx
          DO i=1,ew-1
             IF( k .EQ.  2 ) THEN
                 tptc(i,k,j)=(-1./rconst)*(phiptc(i,k+1,j)-phiptc(i,k,j  ))/LOG(press(i,k+1,j)/press(i,k,j))
             ELSE IF ( k .EQ. kx ) THEN
                 tptc(i,k,j)=(-1./rconst)*(phiptc(i,k,j  )-phiptc(i,k-1,j))/LOG(press(i,k,j)/press(i,k-1,j))
             ELSE
                 tptc(i,k,j)=(-1./rconst)*(phiptc(i,k+1,j)-phiptc(i,k-1,j))/LOG(press(i,k+1,j)/press(i,k-1,j))
             END IF
             t2(i,k,j) = t0(i,k,j) + tptc(i,k,j)
             if(t2(i,k,j) .gt. 400) then
                print *,"interesting temperature "
                print *,t2(i,k,j),i,k,j,tptc(i,k,j)
                stop
             end if
           END DO
        END DO
    END DO


   
      DO j=1,ns-1
         DO i=1,ew-1
            dph = phi2(i,k00,j)-phi1(i,k00,j)
            delpx(i,j) = rho*dph*0.01
         END DO
      END DO


    

      DO j=1,ns-1
         DO i=1,ew-1
            pslx(i,j) = pslx(i,j)+delpx(i,j) 
            grid%pslv_gc(i,j) = pslx(i,j) * 100.

         END DO
      END DO

  
     DO j=1,ns-1
        DO i=1,ew-1
           z2(i,1,j) = terrain(i,j) 
        END DO
     END DO

  

     DO j=1,ns-1
        DO k=k00,kx
           DO i=1,ew-1
               z2(i,k,j) = phi2(i,k,j)/9.81 
            END DO
         END DO
     END DO
     

     

     DO j=1,ns-1
        DO i=1,ew-1
           ps = pslx(i,j)
           t2(i,1,j) = t2(i,k00,j)*((ps/1000.)**rovcp)
           if(t2(i,1,j) .gt. 400) then
              print *,"Interesting surface temperature"
              print *,t2(i,1,j),t2(i,k00,j),ps,i,j
              stop
           end if
        END DO
     END DO


     
     DO j=1,ns-1
        DO i=1,ew-1
           rh2(i,1,j) = rh2(i,k00,j)
        END DO
     END DO

    
    PRINT '(A,I3,A)'       ,'         Bogus storm number ',nstrm,' completed.'

   do j = 1,ns-1
      do k = 1,nz
         do i = 1,ew
            u1(i,k,j) =  u2(i,k,j)
            grid%u_gc(i,k,j) = u2(i,k,j)
         end do
      end do
   end do

   do j = 1,ns
      do k = 1,nz
         do i = 1,ew-1
            v1(i,k,j)   = v2(i,k,j)
            grid%v_gc(i,k,j) = v2(i,k,j)
         end do
      end do
   end do

    do j = 1,ns-1
      do k = 1,nz
         do i = 1,ew-1  
            t1(i,k,j)   = t2(i,k,j)
            grid%t_gc(i,k,j) = t2(i,k,j)
            rh1(i,k,j)  = rh2(i,k,j)
            grid%rh_gc(i,k,j)  = rh2(i,k,j)
            phi1(i,k,j) = phi2(i,k,j)
            grid%ght_gc(i,k,j) = z2(i,k,j)
         END DO
      END DO
   END DO


END DO all_storms
 deallocate(u11)
 deallocate(v11)
 deallocate(t11)
 deallocate(rh11)
 deallocate(phi11)
 deallocate(u1)
 deallocate(v1)
 deallocate(t1)
 deallocate(rh1)
 deallocate(phi1)

 do j = 1,ns-1
    do i = 1,ew-1
       if(grid%ht_gc(i,j) .gt. 1) then
         grid%p_gc(i,1,j)  = grid%p_gc(i,1,j)  + (pslx(i,j) * 100. - old_slp(i,j))
         grid%psfc(i,j) = grid%psfc(i,j) + (pslx(i,j) * 100. - old_slp(i,j))
       else 
         grid%p_gc(i,1,j)  = pslx(i,j) * 100.
         grid%psfc(i,j) = pslx(i,j) * 100.
       end if
    end do
 end do

END SUBROUTINE tc_bogus




   SUBROUTINE rankine(dx,dy,ds,nlvl,vwgt,rmax,vmax,uu,vv,psi,vor)

   

      IMPLICIT NONE

      INTEGER nlvl
      REAL , DIMENSION(nlvl) :: uu, vv, psi, vor
      REAL , DIMENSION(nlvl) :: vwgt
      REAL :: dx,dy,ds,rmax,vmax
 
      REAL , PARAMETER :: alpha1= 1.
      REAL , PARAMETER :: alpha2= -0.75
      real :: pi


      INTEGER :: k
      REAL :: vr , ang , rr , term1 , bb , term2 , alpha


      pi = 3.141592653589793
      

      DO k=1,nlvl
         rr = SQRT(dx**2+dy**2)*ds
         IF ( rr .LT. rmax ) THEN
            alpha = 1.
         ELSE IF ( rr .GE. rmax ) THEN
            alpha = alpha2
         END IF
         vr = vmax * (rr/rmax)**(alpha)
         IF ( dx.GE.0. ) THEN
            ang = (pi/2.) - ATAN2(dy,MAX(dx,1.e-6))
            uu(k) = vwgt(k)*(-vr*COS(ang))
            vv(k) = vwgt(k)*( vr*SIN(ang))
         ELSE IF ( dx.LT.0. ) THEN
            ang = ((3.*pi)/2.) + ATAN2(dy,dx)
            uu(k) = vwgt(k)*(-vr*COS(ang))
            vv(k) = vwgt(k)*(-vr*SIN(ang))
         END IF
      END DO

      
      
      DO k=1,nlvl
         rr = SQRT(dx**2+dy**2)*ds
         IF ( rr .LT. rmax ) THEN
            psi(k) = vwgt(k) * (vmax*rr*rr)/(2.*rmax)
         ELSE IF ( rr .GE. rmax ) THEN
            IF (alpha1.EQ.1.0 .AND. alpha2.eq.-1.0) THEN
               psi(k) = vwgt(k) * vmax*rmax*(0.5+LOG(rr/rmax))
            ELSE IF (alpha1.EQ.1.0 .AND. alpha2.NE.-1.0) THEN
               term1 = vmax/(rmax**alpha1)*(rmax**(alpha1+1)/(alpha1+1))
               bb    = (rr**(alpha2+1)/(alpha2+1))-(rmax**(alpha2+1))/(alpha2+1)
               term2 = vmax/(rmax**alpha2)*bb
               psi(k) = vwgt(k) * (term1 + term2)
            END IF
         END IF
      END DO

      

      DO k=1,nlvl
         rr = SQRT(dx**2+dy**2)*ds
         IF ( rr .LT. rmax ) THEN
            vor(k) = vwgt(k) * (2.*vmax)/rmax
         ELSE IF ( rr .GE. rmax ) THEN
            vor(k) = vwgt(k) * ( (vmax/rmax**alpha2)*(rr**(alpha2-1.))*(1.+alpha2) )
         END IF
      END DO

   END SUBROUTINE rankine




   SUBROUTINE vor(uin,vin,msfu,msfv,msfm,ew,ns,nz,ds,vort)





      IMPLICIT NONE

      INTEGER :: jp1,jm1,ip1,im1,i,j,k
      INTEGER :: ns, ew, nz, k1

      REAL , DIMENSION(ew,nz,ns-1)   :: uin   
      REAL , DIMENSION(ew-1,nz,ns)   :: vin   
      REAL , DIMENSION(ew-1,nz,ns-1) :: vort  

      REAL , DIMENSION(ew,ns-1)    :: msfu  
      REAL , DIMENSION(ew-1,ns)    :: msfv  
      REAL , DIMENSION(ew-1,ns-1)  :: msfm  

      real :: u(ew,ns-1),v(ew-1,ns)
      

      REAL :: ds

      REAL :: dsx,dsy , u1 , u2 , u3 , u4 , v1 , v2 , v3 , v4
      real :: dudy,dvdx,mm

      
      vort(:,:,:) = -999.
      do k = 1,nz

         do j = 1,ns-1
            do i = 1,ew
               u(i,j) = uin(i,k,j)
            end do
         end do


         do j = 1,ns
            do i = 1,ew-1
               v(i,j) = vin(i,k,j)
            end do
         end do



         do j = 2,ns-2
            do i = 2,ew-2
               mm = msfm(i,j) * msfm(i,j)
               u1 = u(i  ,j-1)/msfu(i  ,j-1)
               u2 = u(i+1,j-1)/msfu(i+1,j-1)
               u3 = u(i+1,j+1)/msfu(i+1,j+1)
               u4 = u(i  ,j+1)/msfu(i  ,j+1)
               dudy = mm * (u4 + u3 -(u1 + u2)) /(4*ds)

               v1 = v(i-1,j  )/msfv(i-1,j)
               v2 = v(i+1,j  )/msfv(i+1,j)
               v3 = v(i-1 ,j+1)/msfv(i-1,j+1)
               v4 = v(i+1,j+1)/msfv(i+1,j+1)
               dvdx = mm * (v4 + v2 - (v1 + v3))/(4*ds)

               vort(i,k,j) = dvdx - dudy
            end do
         end do


         do i = 2,ew-2
            vort(i,k,1)    = vort(i,k,2)    
            vort(i,k,ns-1) = vort(i,k,ns-2) 
         end do

         do j = 1,ns-1
            vort(ew-1,k,j) = vort(ew-2,k,j) 
            vort(1,k,j)    = vort(2,k,j)    
         end do

     end do 

   END SUBROUTINE 




   SUBROUTINE diverg(uin,vin,msfu,msfv,msfm,ew,ns,nz,ds,div)

   
   
   

      IMPLICIT NONE

      INTEGER :: jp1,jm1,ip1,im1,i,j,k
      INTEGER :: ns, ew, nz, k1

      REAL , DIMENSION(ew,nz,ns-1)   :: uin   
      REAL , DIMENSION(ew-1,nz,ns)   :: vin   
      REAL , DIMENSION(ew-1,nz,ns-1) :: div   
      REAL , DIMENSION(ew,ns-1)    :: msfu  
      REAL , DIMENSION(ew-1,ns)    :: msfv  
      REAL , DIMENSION(ew-1,ns-1)  :: msfm  

      real :: u(ew,ns-1),v(ew-1,ns)
      

      REAL :: ds

      REAL :: dsr,u1,u2,v1,v2
      real :: dudx,dvdy,mm,arg1,arg2

      dsr = 1/ds
      do k = 1,nz

         do j = 1,ns-1
            do i = 1,ew
               u(i,j) = uin(i,k,j)
            end do
         end do


         do j = 1,ns
            do i = 1,ew-1
               v(i,j) = vin(i,k,j)
            end do
         end do


         do j = 2,ns-2
            do i = 2,ew-2
               mm = msfm(i,j) * msfm(i,j)
               u1 = u(i+1,j)/msfu(i+1,j)
               u2 = u(i  ,j)/msfu(i  ,j)
       
               v1 = v(i,j+1)/msfv(i,j+1)
               v2 = v(i,j)  /msfv(i,j)

               div(i,k,j) = mm * (u1 - u2 + v1 - v2) * dsr
            end do
          end do


         do i = 2,ew-2
            div(i,k,1)    = div(i,k,2)    
            div(i,k,ns-1) = div(i,k,ns-2) 
         end do

         do j = 1,ns-1
            div(ew-1,k,j) = div(ew-2,k,j) 
            div(1,k,j)    = div(2,k,j)    
         end do

     end do 

   END SUBROUTINE diverg




   SUBROUTINE mxratprs (rh, t, ppa, ew, ns, nz, q, min_RH_value)

      
      IMPLICIT NONE

      INTEGER   :: i , ew , j , ns , k , nz


      REAL      :: min_RH_value
      REAL      :: ppa(ew-1,nz,ns-1)
      REAL      :: p( ew-1,nz,ns-1 )
      REAL      :: q (ew-1,nz,ns-1),rh(ew-1,nz,ns-1),t(ew-1,nz,ns-1)

      REAL      :: es
      REAL      :: qs
      REAL      :: cp              = 1004.0
      REAL      :: svp1,svp2,svp3
      REAL      :: celkel
      REAL      :: eps
      

      
      

      
      p = ppa * 0.01

      DO j = 1, ns - 1
         DO k = 1, nz
            DO i = 1, ew - 1
                  rh(i,k,j) = MIN ( MAX ( rh(i,k,j) ,min_RH_value ) , 100. ) 
            END DO
        END DO
     END DO

      svp3   =  29.65
      svp1   =  0.6112
      svp2   =  17.67
      celkel =  273.15
         eps =  0.622

      DO j = 1, ns-1
         DO k = 1, nz  
            DO i = 1,ew-1
               es = svp1 * 10. * EXP(svp2 * (t(i,k,j) - celkel ) / (t(i,k,j) - svp3 ))
               qs = eps * es / (p(i,k,j) - es)
               q(i,k,j) = MAX(0.01 * rh(i,k,j) * qs,0.0)
            END DO
         END DO
      END DO

   END SUBROUTINE mxratprs



SUBROUTINE mass2_Ustag(field,dim1,dim2,dim3)

   IMPLICIT NONE

   INTEGER :: dim1 , dim2 , dim3
   REAL , DIMENSION(dim1,dim2,dim3) :: field,dummy

   dummy = 0.0
   dummy(:,2:dim2-1,:)         = ( field(:,1:dim2-2,:) + &
                                   field(:,2:dim2-1,:) ) * 0.5
   dummy(:,1,:)                = field(:,1,:)
   dummy(:,dim2,:)             = field(:,dim2-1,:)

   field                       =   dummy

END SUBROUTINE mass2_Ustag



SUBROUTINE mass2_Vstag(field,dim1,dim2,dim3)

   IMPLICIT NONE

   INTEGER :: dim1 , dim2 , dim3
   REAL , DIMENSION(dim1,dim2,dim3) :: field,dummy

   dummy = 0.0
   dummy(2:dim1-1,:,:)         = ( field(1:dim1-2,:,:) + &
                                   field(2:dim1-1,:,:) ) * 0.5
   dummy(1,:,:)                = field(1,:,:)
   dummy(dim1,:,:)             = field(dim1-1,:,:)

   field                       =   dummy

END SUBROUTINE mass2_Vstag





   SUBROUTINE relax (chi, ff, rd, ew, ns, ds, smallres, alpha)

      IMPLICIT NONE

      INTEGER, PARAMETER    :: mm = 20000

      INTEGER               :: i
      INTEGER               :: ie
      INTEGER               :: ew  
      INTEGER               :: iter
      INTEGER               :: j
      INTEGER               :: je
      INTEGER               :: jm
      INTEGER               :: ns  
      INTEGER               :: mi

      REAL                  :: alpha
      REAL                  :: alphaov4
      REAL                  :: chi(ew-1,ns-1)
      REAL                  :: chimx(ns-1) 
      REAL                  :: ds
      REAL                  :: epx
      REAL                  :: fac
      REAL                  :: ff(ew-1,ns-1)
      REAL                  :: rd(ew-1,ns-1)
      REAL                  :: rdmax(ns-1)
      REAL                  :: smallres

      LOGICAL               :: converged = .FALSE.

      fac = ds * ds
      alphaov4 = alpha * 0.25

      ie=ew-2
      je=ns-2

      DO j = 1, ns-1
         DO i = 1, ew-1
            ff(i,j) = fac * ff(i,j)
            rd(i,j) = 0.0
         END DO
      END DO

      iter_loop : DO iter = 1, mm
         mi = iter
         chimx = 0.0


         DO j = 2, ns-1
            DO i = 2, ew-1
               chimx(j) = MAX(ABS(chi(i,j)),chimx(j))
            END DO
         END DO

         epx = MAXVAL(chimx) * SMALLRES * 4.0 / alpha

         DO j = 2, ns-2
            DO i = 2, ew-2
               rd(i,j) = chi(i,j+1) + chi(i,j-1) + chi(i+1,j) + chi(i-1,j) - 4.0 * chi(i,j) - ff(i,j)
               chi(i,j) = chi(i,j) + rd(i,j) * alphaov4
            END DO
         END DO

         rdmax = 0.0

         DO j = 2, ns-2
            DO i = 2, ew-2
               rdmax(j) = MAX(ABS(rd(i,j)),rdmax(j))
            END DO
         END DO


         IF (MAXVAL(rdmax) .lt. epx) THEN
            converged = .TRUE.
            EXIT iter_loop
         END IF

      END DO iter_loop

      IF (converged ) THEN

      ELSE
         PRINT '(A,I5,A)','Relaxation did not converge in',mm,' iterations.'
         STOP 'no_converge'
      END IF


      do i = 2,ew-2
            chi(i,ns-1) = chi(i,ns-2) 
            chi(i,1)    = chi(i,2)    
      end do

      do j = 2,ns-2
            chi(ew-1,j) = chi(ew-2,j) 
            chi(1,j)    = chi(2,j)    
      end do

 
      chi(1,1)       = chi(2,1)
      chi(ew-1,1)    = chi(ew-2,1)
      chi(1,ns-1)    = chi(2,ns-1)
      chi(ew-1,ns-1) = chi(ew-2,ns-1)



   END SUBROUTINE relax


   SUBROUTINE geowind(height,ew,ns,nz,ds,ug,vg)

      IMPLICIT NONE

      
      
      
      
      
      
      

      INTEGER :: ew , ns , nz
      REAL :: ds
      REAL , DIMENSION(ew-1,nz,ns-1) :: height
      REAL , DIMENSION(ew,nz,ns-1) :: ug 
      REAL , DIMENSION(ew-1,nz,ns) :: vg

      REAL :: ds2r , h1 , h2 , h3 , h4, ds4r
      INTEGER :: i , j , k

      ds4r=1./(4.*ds)






      ug(:,:,:) = -999.
      do j=2,ns-2
         do k=1,nz
            do i=2,ew-1
              h1 = height(i,k,j+1)
              h2 = height(i-1,k,j+1)
              h3 = height(i  ,k,j-1)
              h4 = height(i-1,k,j-1)
              ug(i,k,j) = -( (h1 + h2) - ( h3 + h4) ) * ds4r
           end do
        end do
      end do

       do i = 2,ew-1
          ug(i,:,1)    = ug(i,:,2)    
          ug(i,:,ns-1) = ug(i,:,ns-2) 
       end do

       do j = 2,ns-2
          ug(1,:,j)  = ug(2,:,j)    
          ug(ew,:,j) = ug(ew-1,:,j) 
       end do  
     
       ug(1,:,1)     = ug(2,:,1)         
       ug(1,:,ns-1)  = ug(2,:,ns-1)      
       ug(ew,:,1)    = ug(ew-1,:,1)      
       ug(ew,:,ns-1) = ug(ew-1,:,ns-1)   



    vg(:,:,:) = -999.
    DO j=2,ns-1
       DO k=1,nz
          DO i=2,ew-2
              h1 = height(i+1,k,j)
              h2 = height(i-1,k,j)
              h3 = height(i+1,k,j-1)
              h4 = height(i-1,k,j-1)
              vg(i,k,j) = ( (h1 + h3) - ( h2 + h4) ) * ds4r
          end do
       end do
    end do

    do i = 2,ew-2
       vg(i,:,1)  = vg(i,:,2)    
       vg(i,:,ns) = vg(i,:,ns-1) 
    end do   

    do j = 2,ns-1
       vg(1,:,j)    = vg(2,:,j)    
       vg(ew-1,:,j) = vg(ew-2,:,j) 
   end do  
      
   vg(1,:,1)     = vg(2,:,1)        
   vg(1,:,ns)    = vg(2,:,ns)       
   vg(ew-1,:,1)  = vg(ew-2,:,1)     
   vg(ew-1,:,ns) = vg(ew-2,:,ns)    
   

   END SUBROUTINE geowind



   SUBROUTINE balance (f,psi,ew,ns,ds,out)

   

   IMPLICIT NONE

      
      
      
      
      
  
      INTEGER :: ew , ns,nslast,ewlast,ifill
      REAL , DIMENSION(ew-1,ns-1) :: f,psi,out
      REAL :: ds

      REAL :: psixx , psiyy , psiy , psix, psixy 
      REAL :: dssq , ds2 , dssq4,arg1,arg2,arg3,arg4

      INTEGER :: i , j

      dssq  = ds * ds
      ds2   = ds * 2.
      dssq4 = ds * ds * 4.


      out(:,:) = -999.0
      DO j=2,ns-2
         DO i=2,ew-2
            psiyy = ( psi(i,j+1) + psi(i,j-1) - 2.*psi(i,j) ) / dssq
            psixx = ( psi(i+1,j) + psi(i-1,j) - 2.*psi(i,j) ) / dssq
            psiy  = ( psi(i,j+1) - psi(i,j-1) ) / ds2
            psix  = ( psi(i+1,j) - psi(i-1,j) ) / ds2
            psixy = ( psi(i+1,j+1)+psi(i-1,j-1)-psi(i-1,j+1)-psi(i+1,j-1)) / dssq4

            arg1  = f(i,j)* (psixx+psiyy)
            arg2  = ( ( f(i,j+1) - f(i,j-1)) / ds2 ) * psiy
            arg3  = ( ( f(i+1,j) - f(i-1,j)) / ds2 ) * psix
            arg4  = 2 *(psixy*psixy-psixx*psiyy)
            out(i,j)= arg1 + arg2  + arg3 - arg4
         END DO
      END DO

      do i = 2,ew-2
            out(i,ns-1) = out(i,ns-2) 
            out(i,1)    = out(i,2)    
      end do

      do j = 2,ns-2
            out(ew-1,j) = out(ew-2,j) 
            out(1,j)    = out(2,j)    
      end do

 
      out(1,1)       = out(2,1)
      out(ew-1,1)    = out(ew-2,1)
      out(1,ns-1)    = out(2,ns-1)
      out(ew-1,ns-1) = out(ew-2,ns-1)

   END SUBROUTINE balance




   SUBROUTINE qvtorh ( q , t , p , k00, ew , ns , nz , rh, min_RH_value   )

      IMPLICIT NONE

      INTEGER , INTENT(IN) :: ew , ns , nz , k00
      REAL , INTENT(IN) ,  DIMENSION(ew-1,nz,ns-1) :: q ,t, p
      REAL , INTENT(OUT) , DIMENSION(ew-1,nz,ns-1) :: rh

      real    min_RH_value

      

      INTEGER :: i , j , k,fill
      REAL      :: es
      REAL      :: qs
      REAL      :: cp              = 1004.0
      REAL      :: svp1,svp2,svp3
      REAL      :: celkel
      REAL      :: eps
      svp3   =  29.65
      svp1   =  0.6112
      svp2   =  17.67
      celkel =  273.15
         eps =  0.622

      DO j = 1 , ns - 1
         DO k = k00 , nz
            DO i = 1 , ew -1
               es = svp1 * 10. * EXP(svp2 * (t(i,k,j) - celkel ) / (t(i,k,j) - svp3 ))
               qs = eps*es/(0.01*p(i,k,j) - es)
               rh(i,k,j) = MIN ( 100. , MAX ( 100.*q(i,k,j)/qs , min_RH_value ) )
            END DO
         END DO
      END DO

   END SUBROUTINE qvtorh




   SUBROUTINE stagger_rankine_winds(utcp,vtcp,ew,ns,nz)













 INTEGER :: ew, ns, nz, i,k,j
 REAL utcp(ew,nz,ns-1),  vtcp(ew-1,nz,ns)



  DO j=1,ns-1
     DO i=2,ew-1
        DO k=1,nz
           utcp(i,k,j)  = ( utcp(i-1,k,j) + utcp(i,k,j) ) /2
        end do
    end do
  end do


 do j = 1,ns
    utcp(1,:,j)  = utcp(2,:,j)
    utcp(ew,:,j) = utcp(ew-1,:,j)
 end do



  DO j=2,ns-1
     DO i=1,ew-1
        DO k=1,nz
           vtcp(i,k,j)  = ( vtcp(i,k,j+1) + vtcp(i,k,j-1) ) /2
        end do
    end do
  end do


  do i = 1,ew
     vtcp(i,:,1)  = vtcp(i,:,2)
     vtcp(i,:,ns) = vtcp(i,:,ns-1)
  end do


   END SUBROUTINE stagger_rankine_winds




  subroutine final_ew_velocity(u2,u1,chi,psi,utcr,dx,ew,ns,nz)
  

  integer :: ew,ns,nz,i,j,k
  real :: u1(ew,nz,ns-1),utcr(ew,nz,ns-1)
  real :: psi(ew-1,nz,ns-1),chi(ew-1,nz,ns-1)  
















  real :: u2(ew,nz,ns-1)


  
  real upos(ew,nz,ns-1),u0(ew,nz,ns-1),uchi(ew,nz,ns-1) 




  real    :: dx,arg1,arg2



   uchi(:,:,:) = -999.
   DO k=1,nz 
      DO j=1,ns-1
         DO i=2,ew-1
            uchi(i,k,j) = ( chi(i,k,j) - chi(i-1,k,j) )/dx
         END DO
      END DO
     
      do j = 1,ns-1
       uchi(1,k,j)    = uchi(2,k,j)    
       uchi(ew,k,j)   = uchi(ew-1,k,j) 
      end do
   end do 



    upos(:,:,:) = -999.
    DO k=1,nz

       DO j=2,ns-2
          DO i=2,ew-1
              arg1 = psi(i,k,j+1) + psi(i-1,k,j+1)
              arg2 = psi(i,k,j-1) + psi(i-1,k,j-1)
              upos(i,k,j) = -( arg1 - arg2 )/(4.*dx)
          END DO
       END DO

       do i = 2,ew-1
          upos(i,k,1)    = upos(i,k,2)    
          upos(i,k,ns-1) = upos(i,k,ns-2) 
       end do

       do j = 1,ns-2
          upos(1,k,j)  = upos(2,k,j)    
          upos(ew,k,j) = upos(ew-1,k,j) 
       end do       


       upos(1,k,1)     = upos(2,k,1)         
       upos(1,k,ns-1)  = upos(2,k,ns-1)      
       upos(ew,k,1)    = upos(ew-1,k,1)      
       upos(ew,k,ns-1) = upos(ew-1,k,ns-1)   

    end do  







   do j=1,ns-1
      do k=1,nz
         do i=1,ew
            u0(i,k,j) = u1(i,k,j)-(upos(i,k,j)+uchi(i,k,j))
         end do
      end do
   end do
   


    do j=1,ns-1
       do k=1,nz
          do i=1,ew
             u2(i,k,j) = u0(i,k,j)+utcr(i,k,j)
          end do
       end do
    end do

 end subroutine final_ew_velocity




  subroutine final_ns_velocity(v2,v1,chi,psi,vtcr,dx,ew,ns,nz)
  

  integer :: ew,ns,nz,i,j,k
  real :: v1(ew-1,nz,ns),vtcr(ew-1,nz,ns)
  real :: psi(ew-1,nz,ns-1),chi(ew-1,nz,ns-1)  














  real :: v2(ew-1,nz,ns)


  
  real vpos(ew-1,nz,ns),v0(ew-1,nz,ns),vchi(ew-1,nz,ns)




  real    :: dx,arg1,arg2



 vchi(:,:,:) = -999.0

    do k = 1,nz
       DO j=2,ns-1
          DO i=1,ew-1
              vchi(i,k,j) = ( chi(i,k,j) - chi(i,k,j-1))/dx
          END DO
       END DO

    do i = 1,ew-1
       vchi(i,k,1)  = vchi(i,k,2)
       vchi(i,k,ns) = vchi(i,k,ns-1)
    end do
       
    end do 



    vpos(:,:,:) = -999.

    DO k=1,nz
       DO j=2,ns-1
          DO i=2,ew-2
              arg1 = psi(i+1,k,j) + psi(i+1,k,j-1)
              arg2 = psi(i-1,k,j) + psi(i-1,k,j-1)
              vpos(i,k,j) =  ( arg1 - arg2 )/(4.*dx)
          END DO
       END DO

       do i = 2,ew-2
          vpos(i,k,1)  = vpos(i,k,2)    
          vpos(i,k,ns) = vpos(i,k,ns-1) 
      end do   

       do j = 1,ns
          vpos(1,k,j)    = vpos(2,k,j)    
          vpos(ew-1,k,j) = vpos(ew-2,k,j) 
      end do  


      vpos(1,k,1)     = vpos(2,k,1)        
      vpos(1,k,ns)    = vpos(2,k,ns)       
      vpos(ew-1,k,1)  = vpos(ew-2,k,1)     
      vpos(ew-1,k,ns) = vpos(ew-2,k,ns)    
   
    END DO
    

    do j=1,ns
       do k=1,nz
          do i=1,ew-1
              v0(i,k,j) = v1(i,k,j)-(vpos(i,k,j)+vchi(i,k,j))
              if( v0(i,k,j) .gt. 100.) then
                print *,vchi(i,k,j),i,k,j
                stop
              end if
          end do
       end do
    end do
    


    do j=1,ns
       do k=1,nz
          do i=1,ew-1
             v2(i,k,j) = v0(i,k,j)+vtcr(i,k,j)
          end do
       end do
    end do

    end subroutine final_ns_velocity


subroutine final_RH(rh2,rh0,rhmx,strmci,strmcj,rmax_nstrm,ew,ns,nz,k00, &
                    dx,ew_gcntr,ns_gcntr,r_vor2)



     integer :: ew,ns,nz
     real :: rh2(ew-1,nz,ns-1)  
     real :: rh0(ew-1,nz,ns-1)  
     real :: rhmx(nz)
     real :: ew_gcntr 
     real :: ns_gcntr 
     real :: dx       
     real :: rmax_nstrm



     real :: sum_rh,avg_rh,rh_min,rhbkg,rhbog,r_ratio
     real :: rad
     real :: rhtc(ew-1,nz,ns-1)

     integer :: nct,k00,i,j,k,ew_mvc,ns_mvc
     integer :: strmci(nz), strmcj(nz)



     DO k=k00,nz
        rh_max= rhmx(k)
        ew_mvc = strmci(k)
        ns_mvc = strmcj(k)
   

        sum_rh = 0.
        nct = 0
        DO j=1,ns-1
           DO i=1,ew-1
              rad = SQRT(REAL(i-ew_mvc)**2.+REAL(j-ns_mvc)**2.)*dx
              IF ( (rad .LT. r_vor2).AND.(rad .GE. 0.8*r_vor2) ) THEN
                  sum_rh = sum_rh + rh0(i,k,j)
                  nct = nct + 1
              END IF
           END DO
        END DO
        avg_rh = sum_rh/MAX(REAL(nct),1.)
   
        DO j=1,ns-1
            DO i=1,ew-1
               rh_min = avg_rh 
               rad = SQRT((REAL(i)-ew_gcntr)**2.+(REAL(j)-ns_gcntr)**2.)*dx
               IF ( rad .LE. rmax_nstrm ) THEN
                  rhtc(i,k,j) = rh_max
               ELSE
                  rhtc(i,k,j) = (rmax_nstrm/rad)*rh_max+(1.-(rmax_nstrm/rad))*rh_min
               END IF
            END DO
         END DO
     END DO


     
     DO j=1,ns-1
        DO k=k00,nz
           DO i=1,ew-1
              rhbkg = rh0(i,k,j)
              rhbog = rhtc(i,k,j)
              rad = SQRT((REAL(i)-ew_mvc)**2.+(REAL(j)-ns_mvc)**2.)*dx
               IF ( (rad.GT.rmax_nstrm) .AND. (rad.LE.r_vor2) ) THEN
                    r_ratio = (rad-rmax_nstrm)/(r_vor2-rmax_nstrm)
                    rh2(i,k,j) = ((1.-r_ratio)*rhbog) + (r_ratio*rhbkg)
              ELSE IF (rad .LE. rmax_nstrm ) THEN
                    rh2(i,k,j) = rhbog
              ELSE
                    rh2(i,k,j) = rhbkg
              END IF

          END DO
        END DO
    END DO

 

    end subroutine final_RH



