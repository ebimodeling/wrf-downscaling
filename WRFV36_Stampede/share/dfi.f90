   SUBROUTINE dfi_accumulate( grid )

      USE module_domain, ONLY : domain

      USE module_driver_constants
      USE module_machine

      USE module_model_constants
      USE module_state_description

      IMPLICIT NONE

      REAL hn
      CHARACTER*80 mess

      

      TYPE(domain) , POINTER          :: grid

      IF ( grid%dfi_opt .EQ. DFI_NODFI .OR. grid%dfi_stage .EQ. DFI_FST ) RETURN




      hn = grid%hcoeff(grid%itimestep+1)

      
       grid%dfi_mu(:,:)    = grid%dfi_mu(:,:)    + grid%mu_2(:,:)    * hn
       grid%dfi_u(:,:,:)   = grid%dfi_u(:,:,:)   + grid%u_2(:,:,:)   * hn
       grid%dfi_v(:,:,:)   = grid%dfi_v(:,:,:)   + grid%v_2(:,:,:)   * hn
       grid%dfi_w(:,:,:)   = grid%dfi_w(:,:,:)   + grid%w_2(:,:,:)   * hn
       grid%dfi_ww(:,:,:)  = grid%dfi_ww(:,:,:)  + grid%ww(:,:,:)    * hn
       grid%dfi_t(:,:,:)   = grid%dfi_t(:,:,:)   + grid%t_2(:,:,:)   * hn
       grid%dfi_phb(:,:,:) = grid%dfi_phb(:,:,:) + grid%phb(:,:,:)   * hn
       grid%dfi_ph0(:,:,:) = grid%dfi_ph0(:,:,:) + grid%ph0(:,:,:)   * hn
       grid%dfi_php(:,:,:) = grid%dfi_php(:,:,:) + grid%php(:,:,:)   * hn
       grid%dfi_p(:,:,:)   = grid%dfi_p(:,:,:)   + grid%p(:,:,:)     * hn
       grid%dfi_ph(:,:,:)  = grid%dfi_ph(:,:,:)  + grid%ph_2(:,:,:)  * hn
       grid%dfi_tke(:,:,:) = grid%dfi_tke(:,:,:) + grid%tke_2(:,:,:) * hn
       grid%dfi_al(:,:,:)  = grid%dfi_al(:,:,:)  + grid%al(:,:,:)    * hn
       grid%dfi_alt(:,:,:) = grid%dfi_alt(:,:,:) + grid%alt(:,:,:)   * hn
       grid%dfi_pb(:,:,:)  = grid%dfi_pb(:,:,:)  + grid%pb(:,:,:)    * hn
      
       grid%moist(:,:,:,:) = max(0.,grid%moist(:,:,:,:))
       grid%dfi_scalar(:,:,:,:) = max(0.,grid%dfi_scalar(:,:,:,:))
      
      
      
      
     IF ( grid%dfi_savehydmeteors .EQ. 0 ) then  
       grid%dfi_moist(:,:,:,:)  = grid%dfi_moist(:,:,:,:)  + grid%moist(:,:,:,:)  * hn
       grid%dfi_scalar(:,:,:,:) = grid%dfi_scalar(:,:,:,:) + grid%scalar(:,:,:,:) * hn
     ELSE
       grid%dfi_moist(:,:,:,P_QV) = grid%dfi_moist(:,:,:,P_QV) + grid%moist(:,:,:,P_QV) * hn
     ENDIF




      
      grid%hcoeff_tot = grid%hcoeff_tot + hn



   END SUBROUTINE dfi_accumulate

   SUBROUTINE dfi_bck_init ( grid )

      USE module_domain, ONLY : domain, head_grid, domain_get_stop_time, domain_get_start_time
      USE module_utility
      USE module_state_description

      IMPLICIT NONE

      TYPE (domain) , POINTER                 :: grid
      INTEGER rc
      CHARACTER*80 mess

      INTERFACE
         SUBROUTINE Setup_Timekeeping(grid)
            USE module_domain, ONLY : domain
            TYPE (domain), POINTER :: grid
         END SUBROUTINE Setup_Timekeeping

         SUBROUTINE dfi_save_arrays(grid)
            USE module_domain, ONLY : domain
            TYPE (domain), POINTER :: grid
         END SUBROUTINE dfi_save_arrays

         SUBROUTINE dfi_clear_accumulation(grid)
            USE module_domain, ONLY : domain
            TYPE (domain), POINTER :: grid
         END SUBROUTINE dfi_clear_accumulation

         SUBROUTINE optfil_driver(grid)
            USE module_domain, ONLY : domain
            TYPE (domain), POINTER :: grid
         END SUBROUTINE optfil_driver

         SUBROUTINE start_domain(grid, allowed_to_read)
            USE module_domain, ONLY : domain
            TYPE (domain)          :: grid
            LOGICAL, INTENT(IN)    :: allowed_to_read
         END SUBROUTINE start_domain

         SUBROUTINE rebalance_driver_dfi (grid)
            USE module_domain, ONLY : domain
            TYPE (domain)          :: grid
         END SUBROUTINE rebalance_driver_dfi

      END INTERFACE

      grid%dfi_stage = DFI_BCK

      
      IF ( grid%time_step_dfi .gt. 0 ) THEN
        CALL nl_set_time_step ( 1, -grid%time_step_dfi )
      ELSE
        CALL nl_set_time_step ( 1, -grid%time_step )
        CALL nl_set_time_step_fract_num ( 1, -grid%time_step_fract_num )
      ENDIF

      CALL Setup_Timekeeping (grid)

      
      CALL start_domain ( grid , .TRUE. )
      
      CALL dfi_save_arrays ( grid )

      
      CALL nl_set_mp_physics( grid%id, 0 )
      CALL nl_set_ra_lw_physics( grid%id, 0 )
      CALL nl_set_ra_sw_physics( grid%id, 0 )
      CALL nl_set_sf_surface_physics( grid%id, 0 )
      CALL nl_set_sf_sfclay_physics( grid%id, 0 )
      CALL nl_set_sf_urban_physics( grid%id, 0 )
      CALL nl_set_bl_pbl_physics( grid%id, 0 )
      CALL nl_set_cu_physics( grid%id, 0 )
      CALL nl_set_cu_diag( grid%id, 0 )
      CALL nl_set_damp_opt( grid%id, 0 )
      CALL nl_set_sst_update( grid%id, 0 )
      CALL nl_set_fractional_seaice( grid%id, 0 )
      CALL nl_set_gwd_opt( grid%id, 0 )
      CALL nl_set_feedback( grid%id, 0 )
      
      CALL nl_set_diff_6th_opt( grid%id, 0 )
      CALL nl_set_constant_bc(1, grid%constant_bc)
      CALL nl_set_use_adaptive_time_step( grid%id, .false. )


      

      grid%km_opt_dfi = grid%km_opt
      grid%diff_opt_dfi = grid%diff_opt
      CALL nl_set_km_opt( grid%id, 1) 
      CALL nl_set_diff_opt( grid%id, 0) 
      CALL nl_set_moist_adv_dfi_opt( grid%id, grid%moist_adv_dfi_opt)
      IF ( grid%moist_adv_opt == 2 ) THEN
         CALL nl_set_moist_adv_opt( grid%id, 0)
      ENDIF

      
      

      CALL nl_set_p_lev_diags( grid%id, grid%p_lev_diags_dfi)


      grid%start_subtime = domain_get_start_time ( grid )
      grid%stop_subtime = domain_get_stop_time ( grid )

      CALL WRFU_ClockSet(grid%domain_clock, currTime=grid%start_subtime, rc=rc)



      CALL dfi_clear_accumulation( grid )
      CALL optfil_driver(grid)

      
      CALL start_domain ( grid , .TRUE. )
      
      
      if(grid%cycling) then

         CALL rebalance_driver_dfi ( grid )
      endif

   END SUBROUTINE dfi_bck_init


   SUBROUTINE dfi_fwd_init ( grid )

      USE module_domain, ONLY : domain, head_grid, domain_get_stop_time, domain_get_start_time
      USE module_utility, ONLY : WRFU_ClockSet
      USE module_state_description

      IMPLICIT NONE

      TYPE (domain) , POINTER                 :: grid
      INTEGER rc
      CHARACTER*80 mess

      INTERFACE
         SUBROUTINE Setup_Timekeeping(grid)
            USE module_domain, ONLY : domain
            TYPE (domain), POINTER :: grid
         END SUBROUTINE Setup_Timekeeping

         SUBROUTINE dfi_save_arrays(grid)
            USE module_domain, ONLY : domain
            TYPE (domain), POINTER :: grid
         END SUBROUTINE dfi_save_arrays

         SUBROUTINE dfi_clear_accumulation(grid)
            USE module_domain, ONLY : domain
            TYPE (domain), POINTER :: grid
         END SUBROUTINE dfi_clear_accumulation

         SUBROUTINE optfil_driver(grid)
            USE module_domain, ONLY : domain
            TYPE (domain), POINTER :: grid
         END SUBROUTINE optfil_driver

         SUBROUTINE rebalance_driver_dfi (grid)
            USE module_domain, ONLY : domain
            TYPE (domain)          :: grid
         END SUBROUTINE rebalance_driver_dfi

         SUBROUTINE start_domain(grid, allowed_to_read)
            USE module_domain, ONLY : domain
            TYPE (domain)          :: grid
            LOGICAL, INTENT(IN)    :: allowed_to_read
         END SUBROUTINE start_domain
      END INTERFACE

      grid%dfi_stage = DFI_FWD

      

      IF ( grid%time_step_dfi .gt. 0 ) THEN
        CALL nl_set_time_step ( grid%id, grid%time_step_dfi )
      ELSE

      CALL nl_get_time_step( grid%id, grid%time_step )
        CALL nl_get_time_step_fract_num( grid%id, grid%time_step_fract_num )

      grid%time_step = abs(grid%time_step)
      CALL nl_set_time_step( grid%id, grid%time_step )

        grid%time_step_fract_num = abs(grid%time_step_fract_num)
        CALL nl_set_time_step_fract_num( grid%id, grid%time_step_fract_num )

      ENDIF

      grid%itimestep=0
      grid%xtime=0.

      
      CALL nl_set_mp_physics( grid%id, grid%mp_physics)
      CALL nl_set_ra_lw_physics( grid%id, grid%ra_lw_physics)
      CALL nl_set_ra_sw_physics( grid%id, grid%ra_sw_physics)
      CALL nl_set_sf_surface_physics( grid%id, grid%sf_surface_physics)
      CALL nl_set_sf_sfclay_physics( grid%id, grid%sf_sfclay_physics)
      CALL nl_set_sf_urban_physics( grid%id, grid%sf_urban_physics)
      CALL nl_set_bl_pbl_physics( grid%id, grid%bl_pbl_physics)
      CALL nl_set_cu_physics( grid%id, grid%cu_physics)
      CALL nl_set_cu_diag( grid%id, grid%cu_diag )
      CALL nl_set_damp_opt( grid%id, grid%damp_opt)
      CALL nl_set_sst_update( grid%id, 0)
      CALL nl_set_fractional_seaice( grid%id, grid%fractional_seaice)
      CALL nl_set_gwd_opt( grid%id, grid%gwd_opt)
      CALL nl_set_feedback( grid%id, 0 )
      CALL nl_set_diff_6th_opt( grid%id, grid%diff_6th_opt)
      CALL nl_set_use_adaptive_time_step( grid%id, .false. )
      
      CALL nl_set_constant_bc(grid%id, grid%constant_bc)









      
      grid%km_opt = grid%km_opt_dfi
      grid%diff_opt = grid%diff_opt_dfi
      CALL nl_set_km_opt( grid%id, grid%km_opt_dfi)
      CALL nl_set_diff_opt( grid%id, grid%diff_opt_dfi)
      CALL nl_set_moist_adv_opt( grid%id, grid%moist_adv_opt)


      CALL Setup_Timekeeping (grid)
      grid%start_subtime = domain_get_start_time ( head_grid )
      grid%stop_subtime = domain_get_stop_time ( head_grid )

      CALL WRFU_ClockSet(grid%domain_clock, currTime=grid%start_subtime, rc=rc)

      IF ( grid%dfi_opt .EQ. DFI_DFL ) THEN
         CALL dfi_save_arrays ( grid )
      END IF
      CALL dfi_clear_accumulation( grid )
      CALL optfil_driver(grid)

      
      CALL start_domain ( grid , .TRUE. )

   END SUBROUTINE dfi_fwd_init

   SUBROUTINE dfi_fst_init ( grid )

      USE module_domain, ONLY : domain, domain_get_stop_time, domain_get_start_time
      USE module_state_description

      IMPLICIT NONE

      TYPE (domain) , POINTER :: grid
      CHARACTER (LEN=80)      :: wrf_error_message

      INTERFACE
         SUBROUTINE Setup_Timekeeping(grid)
            USE module_domain, ONLY : domain
            TYPE (domain), POINTER :: grid
         END SUBROUTINE Setup_Timekeeping

         SUBROUTINE dfi_save_arrays(grid)
            USE module_domain, ONLY : domain
            TYPE (domain), POINTER :: grid
         END SUBROUTINE dfi_save_arrays

         SUBROUTINE dfi_clear_accumulation(grid)
            USE module_domain, ONLY : domain
            TYPE (domain), POINTER :: grid
         END SUBROUTINE dfi_clear_accumulation

         SUBROUTINE optfil_driver(grid)
            USE module_domain, ONLY : domain
            TYPE (domain), POINTER :: grid
         END SUBROUTINE optfil_driver

         SUBROUTINE rebalance_driver_dfi (grid)
            USE module_domain, ONLY : domain
            TYPE (domain)          :: grid
         END SUBROUTINE rebalance_driver_dfi

         SUBROUTINE start_domain(grid, allowed_to_read)
            USE module_domain, ONLY : domain
            TYPE (domain)          :: grid
            LOGICAL, INTENT(IN)    :: allowed_to_read
         END SUBROUTINE start_domain
      END INTERFACE

      grid%dfi_stage = DFI_FST 

     
      CALL nl_set_time_step( grid%id, grid%time_step )

      grid%itimestep=0
      grid%xtime=0.         

      if (grid%id == 1) then
         CALL nl_set_use_adaptive_time_step( 1, grid%use_adaptive_time_step )
      endif
      CALL nl_set_sst_update( grid%id, grid%sst_update)
      
      CALL nl_set_constant_bc(grid%id, .false.)
      CALL nl_set_feedback( grid%id, grid%feedback )

    
      
      CALL nl_set_p_lev_diags ( grid%id, grid%p_lev_diags)


      CALL Setup_Timekeeping (grid)
      grid%start_subtime = domain_get_start_time ( grid )
      grid%stop_subtime = domain_get_stop_time ( grid )

      CALL start_domain ( grid , .TRUE. )

   END SUBROUTINE dfi_fst_init


   SUBROUTINE dfi_write_initialized_state( grid )

      
      USE module_domain, ONLY : domain, head_grid
      USE module_io_domain
      USE module_configure, ONLY : grid_config_rec_type, model_config_rec

      IMPLICIT NONE

      TYPE (domain) , POINTER :: grid
      INTEGER             :: fid, ierr
      CHARACTER (LEN=80)  :: wrf_error_message
      CHARACTER (LEN=80)  :: rstname
      CHARACTER (LEN=132) :: message

      TYPE (grid_config_rec_type) :: config_flags

      CALL model_to_grid_config_rec ( grid%id , model_config_rec , config_flags )

      WRITE (wrf_err_message,'(A,I4)') 'Writing out initialized model state'
      CALL wrf_message(TRIM(wrf_err_message))

      WRITE (rstname,'(A,I2.2)')'wrfinput_initialized_d',grid%id
      CALL open_w_dataset ( fid, TRIM(rstname), grid, config_flags, output_input, "DATASET=INPUT", ierr )
      IF ( ierr .NE. 0 ) THEN
         WRITE( message , '("program wrf: error opening ",A," for writing")') TRIM(rstname)
         CALL wrf_error_fatal3("<stdin>",409,&
message )
      END IF
      CALL output_input ( fid,   grid, config_flags, ierr )
      CALL close_dataset ( fid, config_flags, "DATASET=INPUT" )

   END SUBROUTINE dfi_write_initialized_state

   SUBROUTINE tdfi_write_analyzed_state ( grid )

      
      USE module_domain, ONLY : domain, head_grid
      USE module_io_domain
      USE module_configure, ONLY : grid_config_rec_type, model_config_rec

      IMPLICIT NONE

      TYPE (domain) , POINTER :: grid
      INTEGER             :: fid, ierr
      CHARACTER (LEN=80)  :: wrf_error_message
      CHARACTER (LEN=80)  :: rstname
      CHARACTER (LEN=132) :: message

      TYPE (grid_config_rec_type) :: config_flags

      CALL model_to_grid_config_rec ( grid%id , model_config_rec , config_flags )

      rstname = 'wrfout_analyzed_d01'
      CALL open_w_dataset ( fid, TRIM(rstname),grid, config_flags, output_input, "DATASET=INPUT", ierr )
      IF ( ierr .NE. 0 ) THEN
         WRITE( message , '("program wrf: error opening ",A," for writing")') TRIM(rstname)
         CALL wrf_error_fatal3("<stdin>",440,&
message )
      END IF
      CALL output_input ( fid,   grid, config_flags, ierr )
      CALL close_dataset ( fid, config_flags, "DATASET=INPUT" )

   END SUBROUTINE tdfi_write_analyzed_state






   SUBROUTINE wrf_dfi_array_reset ( )

      USE module_domain, ONLY : domain, head_grid, set_current_grid_ptr

      IMPLICIT NONE

      INTERFACE
         RECURSIVE SUBROUTINE dfi_array_reset_recurse(grid)
            USE module_domain, ONLY : domain
            TYPE (domain), POINTER :: grid
         END SUBROUTINE dfi_array_reset_recurse
      END INTERFACE

      
      

      CALL dfi_array_reset_recurse(head_grid)

      CALL set_current_grid_ptr( head_grid )

   END SUBROUTINE wrf_dfi_array_reset

   SUBROUTINE dfi_array_reset( grid )

      USE module_domain, ONLY : domain




      USE module_model_constants
      USE module_state_description

      IMPLICIT NONE

      INTEGER :: its, ite, jts, jte, kts, kte, &
                 i, j, k

      
      TYPE(domain) , POINTER          :: grid

      

      real eps

      parameter (eps=0.622)
      integer nsoil , ncount
      REAL es,qs,pol,tx,temp,pres,rslf,rsif             
      CHARACTER*80 mess

      ncount = 0
      nsoil = grid%num_soil_layers 

      IF ( grid%dfi_opt .EQ. DFI_NODFI ) RETURN


      
      

      grid%mu_2(:,:)    = grid%dfi_mu(:,:)      / grid%hcoeff_tot
      grid%u_2(:,:,:)   = grid%dfi_u(:,:,:)     / grid%hcoeff_tot
      grid%v_2(:,:,:)   = grid%dfi_v(:,:,:)     / grid%hcoeff_tot
      grid%w_2(:,:,:)   = grid%dfi_w(:,:,:)     / grid%hcoeff_tot
      grid%ww(:,:,:)    = grid%dfi_ww(:,:,:)    / grid%hcoeff_tot
      grid%t_2(:,:,:)   = grid%dfi_t(:,:,:)     / grid%hcoeff_tot
      grid%phb(:,:,:)   = grid%dfi_phb(:,:,:)   / grid%hcoeff_tot
      grid%ph0(:,:,:)   = grid%dfi_ph0(:,:,:)   / grid%hcoeff_tot
      grid%php(:,:,:)   = grid%dfi_php(:,:,:)   / grid%hcoeff_tot
      grid%p(:,:,:)     = grid%dfi_p(:,:,:)     / grid%hcoeff_tot
      grid%ph_2(:,:,:)  = grid%dfi_ph(:,:,:)    / grid%hcoeff_tot
      grid%tke_2(:,:,:) = grid%dfi_tke(:,:,:)   / grid%hcoeff_tot
      grid%al(:,:,:)    = grid%dfi_al(:,:,:)    / grid%hcoeff_tot
      grid%alt(:,:,:)   = grid%dfi_alt(:,:,:)   / grid%hcoeff_tot
      grid%pb(:,:,:)    = grid%dfi_pb(:,:,:)    / grid%hcoeff_tot
    IF ( grid%dfi_savehydmeteors .EQ. 0 ) then

      grid%moist(:,:,:,:)  = max(0.,grid%dfi_moist(:,:,:,:)  / grid%hcoeff_tot)
      grid%scalar(:,:,:,:) = max(0.,grid%dfi_scalar(:,:,:,:) / grid%hcoeff_tot)
    ELSE

      grid%moist(:,:,:,P_QV)  = max(0.,grid%dfi_moist(:,:,:,P_QV)  / grid%hcoeff_tot)
    ENDIF
     if ( grid%sf_surface_physics .EQ. RUCLSMSCHEME ) then

     endif

    IF ( grid%dfi_savehydmeteors .EQ. 1 ) then

      
      grid%SNOW  (:,:) = grid%dfi_SNOW  (:,:)
      grid%SNOWH (:,:) = grid%dfi_SNOWH (:,:)
      grid%SNOWC (:,:) = grid%dfi_SNOWC (:,:)
      grid%CANWAT(:,:) = grid%dfi_CANWAT(:,:)
      grid%TSK   (:,:) = grid%dfi_TSK   (:,:)

      grid%TSLB  (:,:,:)       = grid%dfi_TSLB   (:,:,:)
      grid%SMOIS (:,:,:)       = grid%dfi_SMOIS  (:,:,:)
      IF ( grid%sf_surface_physics .EQ. RUCLSMSCHEME ) then
         grid%QVG   (:,:) = grid%dfi_QVG   (:,:)
         grid%TSNAV (:,:) = grid%dfi_TSNAV (:,:)  
         grid%SOILT1(:,:) = grid%dfi_SOILT1(:,:)   
         grid%SMFR3D(:,:,:)       = grid%dfi_SMFR3D (:,:,:)
         grid%KEEPFR3DFLAG(:,:,:) = grid%dfi_KEEPFR3DFLAG(:,:,:)
      ENDIF

    ELSE

      
            its = grid%sp31 ; ite = grid%ep31 ;
            jts = grid%sp33 ; jte = grid%ep33 ;
    DO j=jts,jte
       DO i=its,ite

        grid%SNOW  (i,j) = max(grid%SNOW  (i,j),grid%dfi_SNOW  (i,j))
        grid%SNOWH (i,j) = max(grid%SNOWH (i,j),grid%dfi_SNOWH (i,j))
        grid%SNOWC (i,j) = max(grid%SNOWC (i,j),grid%dfi_SNOWC (i,j))
        grid%CANWAT(i,j) = max(grid%CANWAT(i,j),grid%dfi_CANWAT(i,j))
      IF(grid%SNOWC (i,j) .eq.0.) then
        grid%TSK   (i,j) = grid%dfi_TSK   (i,j)
        do k=1,nsoil
          grid%TSLB  (i,k,j)       = grid%dfi_TSLB   (i,k,j)
          grid%SMOIS (i,k,j)       = grid%dfi_SMOIS  (i,k,j)
         IF ( grid%sf_surface_physics .EQ. RUCLSMSCHEME ) then
          grid%KEEPFR3DFLAG(i,k,j) = grid%dfi_KEEPFR3DFLAG(i,k,j)
          grid%SMFR3D(i,k,j)       = grid%dfi_SMFR3D (i,k,j)
         ENDIF
        enddo
         IF ( grid%sf_surface_physics .EQ. RUCLSMSCHEME ) then
          grid%QVG   (i,j)         = grid%dfi_QVG   (i,j)
          grid%TSNAV (i,j)         = grid%dfi_TSNAV (i,j)
          grid%SOILT1(i,j)         = grid%dfi_SOILT1(i,j)
         ENDIF
      ENDIF

      ENDDO
    ENDDO

    ENDIF

      
    IF ( grid%dfi_savehydmeteors .EQ. 1 ) then


      grid%moist(:,:,:,P_QC)      = grid%dfi_moist(:,:,:,P_QC)    
      grid%moist(:,:,:,P_QR)      = grid%dfi_moist(:,:,:,P_QR)    
      grid%moist(:,:,:,P_QI)      = grid%dfi_moist(:,:,:,P_QI)    
      grid%moist(:,:,:,P_QS)      = grid%dfi_moist(:,:,:,P_QS)    
      grid%moist(:,:,:,P_QG)      = grid%dfi_moist(:,:,:,P_QG)    
      grid%scalar(:,:,:,:)        = grid%dfi_scalar(:,:,:,:)

       if(grid%dfi_stage .EQ. DFI_FWD) then

            its = grid%sp31 ; ite = grid%ep31 ;  
            kts = grid%sp32 ; kte = grid%ep32 ; 
            jts = grid%sp33 ; jte = grid%ep33 ;
   DO j=jts,jte
      DO i=its,ite
         do k = kts , kte
          temp = (grid%t_2(i,k,j)+t0)*( (grid%p(i,k,j)+grid%pb(i,k,j))/p1000mb )&
                     ** (r_d / Cp)
          pres = grid%p(i,k,j)+grid%pb(i,k,j)

          qs = rslf(pres, temp)










        Tx = temp-273.15
        POL = 0.99999683       + TX*(-0.90826951E-02 +          &
           TX*(0.78736169E-04   + TX*(-0.61117958E-06 +         &
           TX*(0.43884187E-08   + TX*(-0.29883885E-10 +         &
           TX*(0.21874425E-12   + TX*(-0.17892321E-14 +         &
           TX*(0.11112018E-16   + TX*(-0.30994571E-19)))))))))
        es = 6.1078/POL**8











      IF(grid%dfi_rh(i,k,j) == 1. ) then
         grid%moist (i,k,j,P_QV) = MAX(grid%moist (i,k,j,P_QV),grid%dfi_rh(i,k,j)*QS)
          ncount=ncount+1
      ENDIF




         enddo
      ENDDO
   ENDDO

       endif
      

         CALL rebalance_driver_dfi ( grid )
   ENDIF




   END SUBROUTINE dfi_array_reset

   SUBROUTINE optfil_driver( grid )

      USE module_domain, ONLY : domain
      USE module_utility




      USE module_state_description
      USE module_model_constants

      IMPLICIT NONE

      TYPE (domain) , POINTER                 :: grid

      
      integer :: nstep2, nstepmax, rundfi, i, rc,hr,min,sec
      integer :: yr,jday
      real :: timestep, tauc
      TYPE(WRFU_TimeInterval) :: run_interval
      CHARACTER*80 mess

      timestep=abs(grid%dt)
      run_interval = grid%stop_subtime - grid%start_subtime
      CALL WRFU_TimeGet(grid%start_subtime, YY=yr, dayofYear=jday, H=hr, M=min, S=sec, rc=rc)
      CALL WRFU_TimeGet(grid%stop_subtime, YY=yr, dayofYear=jday, H=hr, M=min, S=sec, rc=rc)

      CALL WRFU_TimeIntervalGet( run_interval, S=rundfi, rc=rc )
      rundfi = abs(rundfi)

      nstep2= ceiling((1.0 + real(rundfi)/timestep) / 2.0)

      
      

      tauc = real(grid%dfi_cutoff_seconds)

      
      grid%hcoeff(:) = 0.0
      IF ( grid%dfi_nfilter < 0 .OR. grid%dfi_nfilter > 8 ) THEN
write(mess,*) 'Invalid filter specified in namelist.'
call wrf_message(mess)
      ELSE
         call dfcoef(nstep2-1, grid%dt, tauc, grid%dfi_nfilter, grid%hcoeff)
      END IF

      IF ( MOD(int(1.0 + real(rundfi)/timestep),2) /= 0 ) THEN
         DO i=1,nstep2-1 
            grid%hcoeff(2*nstep2-i) = grid%hcoeff(i)
         END DO
      ELSE
         DO i=1,nstep2 
            grid%hcoeff(2*nstep2-i+1) = grid%hcoeff(i)
         END DO
      END IF

   END SUBROUTINE optfil_driver


   SUBROUTINE dfi_clear_accumulation( grid )

      USE module_domain, ONLY : domain





      USE module_state_description

      IMPLICIT NONE

      
      TYPE(domain) , POINTER          :: grid

      grid%dfi_mu(:,:)       = 0.
      grid%dfi_u(:,:,:)      = 0.
      grid%dfi_v(:,:,:)      = 0.
      grid%dfi_w(:,:,:)      = 0.
      grid%dfi_ww(:,:,:)     = 0.
      grid%dfi_t(:,:,:)      = 0.
      grid%dfi_phb(:,:,:)    = 0.
      grid%dfi_ph0(:,:,:)    = 0.
      grid%dfi_php(:,:,:)    = 0.
      grid%dfi_p(:,:,:)      = 0.
      grid%dfi_ph(:,:,:)     = 0.
      grid%dfi_tke(:,:,:)    = 0.
      grid%dfi_al(:,:,:)     = 0.
      grid%dfi_alt(:,:,:)    = 0.
      grid%dfi_pb(:,:,:)     = 0.
    IF ( grid%dfi_savehydmeteors .EQ. 0 ) then  

      grid%dfi_moist(:,:,:,:)  = 0.
      grid%dfi_scalar(:,:,:,:) = 0.
    ELSE

      grid%dfi_moist(:,:,:,P_QV) = 0.
    ENDIF
     if ( grid%sf_surface_physics .EQ. RUCLSMSCHEME ) then

     endif


      grid%hcoeff_tot  = 0.0

   END SUBROUTINE dfi_clear_accumulation


   SUBROUTINE dfi_save_arrays( grid )

      USE module_domain, ONLY : domain




      USE module_model_constants
      USE module_state_description

      IMPLICIT NONE

      INTEGER :: its, ite, jts, jte, kts, kte, &
                 i, j, k

      
      TYPE(domain) , POINTER          :: grid
      

      REAL es,qs,pol,tx,temp,pres,rslf

      
      grid%dfi_SNOW(:,:)   = grid%SNOW(:,:) 
      grid%dfi_SNOWH(:,:)  = grid%SNOWH(:,:) 
      grid%dfi_SNOWC(:,:)  = grid%SNOWC(:,:) 
      grid%dfi_CANWAT(:,:) = grid%CANWAT(:,:) 
      grid%dfi_TSK(:,:)    = grid%TSK(:,:) 

      
      grid%dfi_TSLB(:,:,:)         = grid%TSLB(:,:,:) 
      grid%dfi_SMOIS(:,:,:)        = grid%SMOIS(:,:,:) 
      
      IF ( grid%sf_surface_physics .EQ. RUCLSMSCHEME ) then
      grid%dfi_QVG(:,:)            = grid%QVG(:,:) 
      grid%dfi_SOILT1(:,:)         = grid%SOILT1(:,:) 


      grid%dfi_TSNAV(:,:)          = grid%TSNAV(:,:) 
      grid%dfi_SMFR3D(:,:,:)       = grid%SMFR3D(:,:,:) 
      grid%dfi_KEEPFR3DFLAG(:,:,:) = grid%KEEPFR3DFLAG(:,:,:) 
      ENDIF


      
    IF ( grid%dfi_savehydmeteors .EQ. 1 ) then    


      grid%dfi_moist(:,:,:,P_QC)  =   grid%moist(:,:,:,P_QC)
      grid%dfi_moist(:,:,:,P_QR)  =   grid%moist(:,:,:,P_QR)
      grid%dfi_moist(:,:,:,P_QI)  =   grid%moist(:,:,:,P_QI)
      grid%dfi_moist(:,:,:,P_QS)  =   grid%moist(:,:,:,P_QS)
      grid%dfi_moist(:,:,:,P_QG)  =   grid%moist(:,:,:,P_QG)
      grid%dfi_scalar(:,:,:,:)    =   grid%scalar(:,:,:,:)
    ENDIF

       if(grid%dfi_stage .EQ. DFI_BCK) then

            its = grid%sp31 ; ite = grid%ep31 ;  
            kts = grid%sp32 ; kte = grid%ep32 ; 
            jts = grid%sp33 ; jte = grid%ep33 ;
   DO j=jts,jte
      DO i=its,ite
         do k = kts , kte
          temp = (grid%t_2(i,k,j)+t0)*( (grid%p(i,k,j)+grid%pb(i,k,j))/p1000mb )&
                     ** (r_d / Cp)
          pres = grid%p(i,k,j)+grid%pb(i,k,j)

          qs = rslf(pres, temp)










        Tx = temp-svpt0
        POL = 0.99999683       + TX*(-0.90826951E-02 +     &
           TX*(0.78736169E-04   + TX*(-0.61117958E-06 +    &
           TX*(0.43884187E-08   + TX*(-0.29883885E-10 +    &
           TX*(0.21874425E-12   + TX*(-0.17892321E-14 +    & 
           TX*(0.11112018E-16   + TX*(-0.30994571E-19)))))))))
        es = 6.1078/POL**8





          grid%dfi_rh (i,k,j) = MIN(1.,MAX(0.,grid%moist(i,k,j,P_QV)/qs))






      IF ((grid%moist (i,k,j,P_QC) .GT. 1.e-6  .or.        &
          grid%moist (i,k,j,P_QI) .GT. 1.e-6)  .and.       &
          grid%dfi_rh (i,k,j) .lt. 1.) THEN 
               grid%dfi_rh (i,k,j)=1.
      ENDIF
         
        end do
      END DO
    ENDDO
       endif


   END SUBROUTINE dfi_save_arrays


   SUBROUTINE dfcoef (NSTEPS,DT,TAUC,NFILT,H)


















 
      implicit none

      integer, intent(in)   :: nsteps, nfilt
      real   , intent(in)   :: dt, tauc
      real, intent(out)  :: h(1:nsteps+1)
      
      

      integer               :: n
      real                  :: pi, omegac, x, window, deltat
      real                  :: hh(0:nsteps)

      pi=4*ATAN(1.)
      deltat=ABS(dt)

      

      if ( nfilt .eq. -1) call debug    (nsteps,h)

      IF ( NFILT .EQ. 0 ) CALL UNIFORM  (NSTEPS,HH)
      IF ( NFILT .EQ. 1 ) CALL LANCZOS  (NSTEPS,HH)
      IF ( NFILT .EQ. 2 ) CALL HAMMING  (NSTEPS,HH)
      IF ( NFILT .EQ. 3 ) CALL BLACKMAN (NSTEPS,HH)
      IF ( NFILT .EQ. 4 ) CALL KAISER   (NSTEPS,HH)
      IF ( NFILT .EQ. 5 ) CALL POTTER2  (NSTEPS,HH)
      IF ( NFILT .EQ. 6 ) CALL DOLPHWIN (NSTEPS,HH)

      IF ( NFILT .LE. 6 ) THEN     

         
         OMEGAC = 2.*PI/TAUC

         DO N=0,NSTEPS
            WINDOW = HH(N)
            IF ( N .EQ. 0 ) THEN
              X = (OMEGAC*DELTAT/PI)
            ELSE
              X = SIN(N*OMEGAC*DELTAT)/(N*PI)
            END IF
            HH(N) = X*WINDOW
         END DO

         
         CALL NORMLZ(HH,NSTEPS)

         DO N=0,NSTEPS
            H(N+1) = HH(NSTEPS-N)
         END DO

      ELSE IF ( NFILT .EQ. 7 ) THEN     

         CALL DOLPH(DT,TAUC,NSTEPS,H)

      ELSE IF ( NFILT .EQ. 8 ) THEN     

         CALL RHOFIL(DT,TAUC,2,NSTEPS*2,2,H,NSTEPS)

      END IF

      RETURN

   END SUBROUTINE dfcoef


   SUBROUTINE NORMLZ(HH,NMAX)

   

      implicit none
  
      integer, intent(in)                       :: nmax
      real   , dimension(0:nmax), intent(out)   :: hh

      
      real     :: sumhh
      integer  :: n

      SUMHH = HH(0)
      DO N=1,NMAX
        SUMHH = SUMHH + 2*HH(N)
      ENDDO
      DO N=0,NMAX
        HH(N)  = HH(N)/SUMHH
      ENDDO

      RETURN

   END subroutine normlz


   subroutine debug(nsteps, ww)

      implicit none

      integer, intent(in)                        :: nsteps
      real   , dimension(0:nsteps), intent(out)  :: ww
      integer :: n

      do n=0,nsteps
         ww(n)=0
      end do

      ww(int(nsteps/2))=1

      return

   end subroutine debug


   SUBROUTINE UNIFORM(NSTEPS,WW)

   

      implicit none

      integer, intent(in)                        :: nsteps
      real   , dimension(0:nsteps), intent(out)  :: ww
       
      integer          :: n

      DO N=0,NSTEPS
        WW(N) = 1.
      ENDDO

      RETURN

   END subroutine uniform


   SUBROUTINE LANCZOS(NSTEPS,WW)

   

      implicit none 

      integer,  parameter                      :: nmax = 1000
      integer,  intent(in)                     :: nsteps
      real   ,  dimension(0:nmax), intent(out) :: ww
      integer  ::  n
      real     :: power, pi, w

      
      POWER = 1

      PI=4*ATAN(1.)
      DO N=0,NSTEPS
         IF ( N .EQ. 0 ) THEN
            W = 1.0
         ELSE
            W = SIN(N*PI/(NSTEPS+1)) / ( N*PI/(NSTEPS+1))
         ENDIF
         WW(N) = W**POWER
      ENDDO

      RETURN

   END SUBROUTINE lanczos


   SUBROUTINE HAMMING(NSTEPS,WW)

   

      implicit none

      integer, intent(in)           :: nsteps
      real, dimension(0:nsteps)    :: ww
      integer   ::   n
      real      :: alpha, pi, w

      
      
      ALPHA=0.54

      PI=4*ATAN(1.)
      DO N=0,NSTEPS
         IF ( N .EQ. 0 ) THEN
            W = 1.0
         ELSE
            W = ALPHA + (1-ALPHA)*COS(N*PI/(NSTEPS))
         ENDIF
         WW(N) = W
      ENDDO

      RETURN

   END SUBROUTINE hamming


   SUBROUTINE BLACKMAN(NSTEPS,WW)

   

      implicit none

      integer, intent(in)           :: nsteps
      real, dimension(0:nsteps)    :: ww
      integer  :: n

      real :: pi, w

      PI=4*ATAN(1.)
      DO N=0,NSTEPS
         IF ( N .EQ. 0 ) THEN
            W = 1.0
         ELSE
            W = 0.42 + 0.50*COS(  N*PI/(NSTEPS))   &
                     + 0.08*COS(2*N*PI/(NSTEPS))
         ENDIF
         WW(N) = W
      ENDDO

      RETURN

   END SUBROUTINE blackman


   SUBROUTINE KAISER(NSTEPS,WW)

   

      implicit none

      real, external :: bessi0

      integer, intent(in)           :: nsteps
      real, dimension(0:nsteps)    :: ww
      integer   :: n
      real      :: alpha, xi0a, xn, as

      ALPHA = 1

      XI0A =  BESSI0(ALPHA)
      DO N=0,NSTEPS
         XN = N
         AS = ALPHA*SQRT(1.-(XN/NSTEPS)**2)
         WW(N) = BESSI0(AS) / XI0A
      ENDDO

      RETURN

   END SUBROUTINE kaiser


   REAL FUNCTION BESSI0(X)

   

      implicit none

      real(8)   :: Y
      real(8)   :: P1 = 1.0d0
      real(8)   :: P2 = 3.5156230D0
      real(8)   :: P3 = 3.0899424D0
      real(8)   :: P4 = 1.2067492D0
      real(8)   :: P5 = 0.2659732D0
      real(8)   :: P6 = 0.360768D-1
      real(8)   :: P7 = 0.45813D-2

      real*8    :: Q1 = 0.39894228D0
      real*8    :: Q2 = 0.1328592D-1
      real*8    :: Q3 = 0.225319D-2
      real*8    :: Q4 = -0.157565D-2
      real*8    :: Q5 = 0.916281D-2
      real*8    :: Q6 = -0.2057706D-1
      real*8    :: Q7 = 0.2635537D-1
      real*8    :: Q8 = -0.1647633D-1
      real*8    :: Q9 = 0.392377D-2

      real            :: x, ax


      IF (ABS(X).LT.3.75) THEN
        Y=(X/3.75)**2
        BESSI0=P1+Y*(P2+Y*(P3+Y*(P4+Y*(P5+Y*(P6+Y*P7)))))
      ELSE
        AX=ABS(X)
        Y=3.75/AX
        BESSI0=(EXP(AX)/SQRT(AX))*(Q1+Y*(Q2+Y*(Q3+Y*(Q4    &
           +Y*(Q5+Y*(Q6+Y*(Q7+Y*(Q8+Y*Q9))))))))
      ENDIF
      RETURN

   END FUNCTION bessi0


   SUBROUTINE POTTER2(NSTEPS,WW)

      
      

      implicit none

      integer, intent(in)                       :: nsteps
      real, dimension(0:nsteps),intent(out)     ::  ww
      integer  :: n
      real     :: ck, sum, arg

      
      real, dimension(0:3)   :: d 
      real                   :: pi
      integer                :: ip

      d(0) = 0.35577019
      d(1) = 0.2436983
      d(2) = 0.07211497
      d(3) = 0.00630165

      PI=4*ATAN(1.)

      CK = 1.0
      DO N=0,NSTEPS
         IF (N.EQ.NSTEPS) CK = 0.5
         ARG = PI*FLOAT(N)/FLOAT(NSTEPS)

         ARG = ARG/2.

         SUM = D(0)
         DO IP=1,3
            SUM = SUM + 2.*D(IP)*COS(ARG*FLOAT(IP))
         END DO
         WW(N) = CK*SUM
      END DO

      RETURN

   END SUBROUTINE potter2


   SUBROUTINE dolphwin(m, window)















      IMPLICIT NONE

      
      INTEGER, INTENT(IN)                  ::  m
      REAL, DIMENSION(0:M), INTENT(OUT)    ::  window

      
      REAL, DIMENSION(0:2*M)               :: t
      REAL, DIMENSION(0:M)                 :: w, time
      REAL    :: pi, thetas, x0, term1, term2, rr, r, db, sum, arg
      INTEGER :: n, nm1, nt, i

      PI = 4*ATAN(1.D0)
      THETAS = 2*PI/M

      N = 2*M+1
      NM1 = N-1
      X0 = 1/COS(THETAS/2)

      TERM1 = (X0 + SQRT(X0**2-1))**(FLOAT(N-1))
      TERM2 = (X0 - SQRT(X0**2-1))**(FLOAT(N-1))
      RR = 0.5*(TERM1+TERM2)
      R = 1/RR
      DB = 20*LOG10(R)
      WRITE(*,'(1X,''DOLPH: M,N='',2I8)')M,N
      WRITE(*,'(1X,''DOLPH: THETAS (STOP-BAND EDGE)='',F10.3)')THETAS
      WRITE(*,'(1X,''DOLPH: R,DB='',2F10.3)')R, DB

      DO NT=0,M
        SUM = RR
        DO I=1,M
          ARG = X0*COS(I*PI/N)
          CALL CHEBY(T,NM1,ARG)
          TERM1 = T(NM1)
          TERM2 = COS(2*NT*PI*I/N)
          SUM = SUM + 2*TERM1*TERM2
        ENDDO
        W(NT) = SUM/N
        TIME(NT) = NT
      ENDDO


      DO NT=0,M
        WINDOW(NT) = W(NT)
      ENDDO

      RETURN

   END SUBROUTINE dolphwin


   SUBROUTINE dolph(deltat, taus, m, window)












      IMPLICIT NONE

      
      INTEGER, INTENT(IN)                  ::  m
      REAL, DIMENSION(0:M), INTENT(OUT)    ::  window
      REAL, INTENT(IN)                     :: deltat, taus

      
      integer, PARAMETER        :: NMAX = 5000
      REAL, dimension(0:NMAX)   :: t, w, time
      real, dimension(0:2*nmax) :: w2
      INTEGER                   :: NPRPE=0        
      CHARACTER*80              :: MES

      real    :: pi, thetas, x0, term1, term2, rr, r,db, sum, arg, sumw
      integer :: n, nm1, i, nt
      
      PI = 4*ATAN(1.D0)

      WRITE (mes,'(A,F8.2,A,F10.2)') 'In dolph, deltat = ',deltat,' taus = ',taus
      CALL wrf_message(TRIM(mes))

      N = 2*M+1
      NM1 = N-1

      THETAS = 2*PI*ABS(DELTAT/TAUS)
      X0 = 1/COS(THETAS/2)
      TERM1 = (X0 + SQRT(X0**2-1))**(FLOAT(N-1))
      TERM2 = (X0 - SQRT(X0**2-1))**(FLOAT(N-1))
      RR = 0.5*(TERM1+TERM2)
      R = 1/RR
      DB = 20*LOG10(R)

      WRITE (mes,'(A,2I8)') 'In dolph: M,N = ', M,N
      CALL wrf_message(TRIM(mes))
      WRITE (mes,'(A,F10.3)') 'In dolph: THETAS (STOP-BAND EDGE) = ', thetas
      CALL wrf_message(TRIM(mes))
      WRITE (mes,'(A,2F10.3)') 'In dolph: R,DB = ', R,DB
      CALL wrf_message(TRIM(mes))

      DO NT=0,M
         SUM = 1
         DO I=1,M
            ARG = X0*COS(I*PI/N)
            CALL CHEBY(T,NM1,ARG)
            TERM1 = T(NM1)
            TERM2 = COS(2*NT*PI*I/N)
            SUM = SUM + R*2*TERM1*TERM2
         ENDDO
         W(NT) = SUM/N
         TIME(NT) = NT
         WRITE (mes,'(A,F10.6,2x,E17.7)') 'In dolph: TIME, W = ', TIME(NT), W(NT)
         CALL wrf_message(TRIM(mes))
      ENDDO

      DO NT=0,M
         W2(M+NT) = W(NT)
         W2(M-NT) = W(NT)
      ENDDO


      SUMW = 0.
      DO NT=0,2*M
         SUMW = SUMW + W2(NT)
      ENDDO
      WRITE (mes,'(A,F10.4)') 'In dolph: SUM OF WEIGHTS W2 = ', sumw
      CALL wrf_message(TRIM(mes))

      DO NT=0,M
         WINDOW(NT) = W2(NT)
      ENDDO

      RETURN

   END SUBROUTINE dolph


   SUBROUTINE cheby(t, n, x)







      IMPLICIT NONE

      
      INTEGER, INTENT(IN)  :: n
      REAL, INTENT(IN)     :: x
      REAL, DIMENSION(0:N) :: t
 
      integer  :: nn

      T(0) = 1
      T(1) = X
      IF(N.LT.2) RETURN
      DO NN=2,N
         T(NN) = 2*X*T(NN-1) - T(NN-2)
      ENDDO

      RETURN

   END SUBROUTINE cheby


   SUBROUTINE rhofil(dt, tauc, norder, nstep, ictype, frow, nosize)




























      IMPLICIT DOUBLE PRECISION (A-H,O-Z)

      DOUBLE PRECISION MUC


      REAL, intent(in)    ::  DT,TAUC


      integer, intent(in) ::  norder, nstep, ictype, nosize
      REAL   , dimension(0:nosize), intent(out)::  FROW


      integer, PARAMETER  :: NOMAX=100
      real   , dimension(0:NOMAX) :: acoef, bcoef
      real   , dimension(0:NOMAX,0:NOMAX) :: filter

      real   , dimension(0:NOMAX) :: alpha, beta

      real   :: DTT

      DTT = ABS(DT)
      PI = 2*DASIN(1.D0)
      IOTA = CMPLX(0.,1.)


      THETAC = 2*PI*DTT/(TAUC)
      MUC = tan(THETAC/2) 
      FC = THETAC/(2*PI)


    DO NC=0,NOMAX
       ACOEF(NC) = 0.
       BCOEF(NC) = 0.
       ALPHA(NC) = 0.
       BETA (NC) = 0.
       FROW (NC) = 0.
       DO NR=0,NOMAX
          FILTER(NR,NC) = 0.
       ENDDO
    ENDDO


    FILTER(0,0) = 1.


    IF ( ICTYPE.eq.2 ) THEN
       CALL RHOCOF(NORDER,NOMAX,MUC, ACOEF,BCOEF)
    ENDIF

    DO 100 NROW=1,NSTEP

       IF ( ICTYPE.eq.1 ) THEN
          NORD = MIN(NROW,NORDER)
          IF ( NORD.le.NORDER) THEN
             CALL RHOCOF(NORD,NOMAX,MUC, ACOEF,BCOEF)
          ENDIF
       ENDIF

       DO K=0,NROW
          ALPHA(K) = ACOEF(NROW-K)
          IF(K.lt.NROW) BETA(K) = BCOEF(NROW-K)
       ENDDO


       IF ( ICTYPE.eq.2 ) THEN
          IF ( NROW.lt.NORDER ) THEN
             CN = 0.
             DO NN=NROW+1,NORDER
                CN = CN + (ACOEF(NN)+BCOEF(NN))
             ENDDO
             ALPHA(0) = ALPHA(0) + CN
          ENDIF
       ENDIF


      SUMAB = 0.
      DO NN=0,NROW
        SUMAB = SUMAB + ALPHA(NN)
          IF(NN.lt.NROW) SUMAB = SUMAB + BETA(NN)
      ENDDO

      DO KK=0,NROW-1
         SUMBF = 0.
         DO LL=0,NROW-1
            SUMBF = SUMBF + BETA(LL)*FILTER(LL,KK)
         ENDDO
         FILTER(NROW,KK) = ALPHA(KK)+SUMBF
      ENDDO
      FILTER(NROW,NROW) = ALPHA(NROW)


      SUMROW = 0.
      DO NN=0,NROW
        SUMROW = SUMROW + FILTER(NROW,NN)
      ENDDO

100 CONTINUE

      DO NC=0,NSTEP
        FROW(NC) = FILTER(NSTEP,NC)
      ENDDO

      RETURN

   END SUBROUTINE rhofil


   SUBROUTINE rhocof(nord, nomax, muc, ca, cb)




      IMPLICIT NONE 

      
      integer, intent(in)      :: nord, nomax
      real, dimension(0:nomax) :: ca, cb

      
      double precision, external :: cnr

      
      INTEGER                  :: nn
      COMPLEX                  :: IOTA
      DOUBLE PRECISION         :: MUC, ZN
      DOUBLE PRECISION         :: pi, root2, rn, sigma, gain, sumcof

      PI = 2*ASIN(1.)
      ROOT2 = SQRT(2.)
      IOTA = (0.,1.)

      RN = 1./FLOAT(NORD)
      SIGMA = 1./( SQRT(2.**RN-1.) )

      GAIN  = (MUC*SIGMA/(1+MUC*SIGMA))**NORD
      ZN = (1-MUC*SIGMA)/(1+MUC*SIGMA)

      DO NN=0,NORD
        CA(NN) = CNR(NORD,NN)*GAIN
        IF(NN.gt.0) CB(NN) = -CNR(NORD,NN)*(-ZN)**NN
      ENDDO


      SUMCOF = 0.
      DO NN=0,NORD
        SUMCOF = SUMCOF + CA(NN)
        IF(NN.gt.0) SUMCOF = SUMCOF + CB(NN)
      ENDDO

      RETURN

   END SUBROUTINE RHOCOF


   DOUBLE PRECISION FUNCTION cnr(n,r)

   


      IMPLICIT NONE

      
      INTEGER , intent(in)    :: n, R

      
      INTEGER          :: k
      DOUBLE PRECISION :: coeff, xn, xr, xk

      IF ( R.eq.0 ) THEN
         CNR = 1.0
         RETURN
      ENDIF
      Coeff = 1.0
      XN = DFLOAT(N)
      XR = DFLOAT(R)
      DO K=1,R
        XK = DFLOAT(K)
        COEFF = COEFF * ( (XN-XR+XK)/XK )
      ENDDO
      CNR = COEFF

      RETURN

   END FUNCTION cnr


    SUBROUTINE optfil (grid,NH,DELTAT,NHMAX)






















     USE module_domain, ONLY : domain
    
     TYPE(domain) , POINTER :: grid

     REAL,DIMENSION( 20) :: EDGE
     REAL,DIMENSION( 10) :: FX, WTX, DEVIAT
     REAL,DIMENSION(2*NHMAX+1) :: H
     logical LPRINT
     REAL, INTENT (IN) :: DELTAT
     INTEGER, INTENT (IN) :: NH, NHMAX

         TAUP = 3.
         TAUS = 1.5
         LPRINT = .true.


         NL=2*NHMAX+1
        do 101 n=1,NL
          H(n)=0.
 101    continue

        NFILT = 2*NH+1
        print *,' start optfil, NFILT=', nfilt



        IF(NFILT.LE.0 .OR. NFILT.GT.128 ) THEN
	   WRITE(6,*) 'NH=',NH
       CALL wrf_error_fatal3("<stdin>",1686,&
' Sorry, error 1 in call to OPTFIL ')
	ENDIF


        JTYPE = 1
        NBANDS = 2

        LGRID = 16


	  DT = ABS(DELTAT)
          FS = DT/(TAUS*3600.)
          FP = DT/(TAUP*3600.)
          IF(FS.GT.0.5) then

       CALL wrf_error_fatal3("<stdin>",1702,&
' FS too large in OPTFIL ')

          end if
          IF(FP.LT.0.0) then

       CALL wrf_error_fatal3("<stdin>",1708,&
' FP too small in OPTFIL ')

          end if


          WTP = 1.0
          WTS = 1.0










          IF ( LPRINT ) THEN
               TAUC = 2./((1/TAUS)+(1/TAUP))
               DTAU = 2./((1/TAUS)-(1/TAUP))
               FC = DT/(TAUC*3600.)
               DF = DT/(DTAU*3600.)
               WRITE(6,*) ' DT ' , dt
               WRITE(6,*) ' TAUS, TAUP ' , TAUS,TAUP
               WRITE(6,*) ' TAUC, DTAU ' , TAUC,DTAU
               WRITE(6,*) '   FP,   FS ' ,   FP,  FS   
               WRITE(6,*) '   FC,   DF ' ,   FC,  DF
               WRITE(6,*) '  WTS,  WTP ' ,  WTS, WTP
          ENDIF


        EDGE(1) = 0.0
        EDGE(2) = FP
        EDGE(3) = FS
        EDGE(4) = 0.5
        FX(1) = 1.0
        FX(2) = 0.0
        WTX(1) = WTP
        WTX(2) = WTS

        CALL MCCPAR(NFILT,JTYPE,NBANDS,LPRINT,LGRID,       &
                   EDGE,FX,WTX,DEVIAT, h )


        DP = DEVIAT(1)
        DS = DEVIAT(2)


      IF(MOD(NFILT,2).EQ.0) THEN
         NHALF = ( NFILT )/2
      ELSE
         NHALF = (NFILT+1)/2
      ENDIF
      DO 100 nn=1,NHALF
         H(NFILT+1-nn) = h(nn)
  100 CONTINUE


        sumh = 0 
        do 150 n=1,NFILT
          sumh = sumh + H(n)
  150   continue
  print *,'SUMH =', sumh

        do 200 n=1,NFILT 
          H(n)  = H(n)/sumh
  200   continue
        do 201 n=1,NFILT
        grid%hcoeff(n)=H(n)
  201   continue


   END SUBROUTINE optfil


      SUBROUTINE MCCPAR (NFILT,JTYPE,NBANDS,LPRINT,LGRID,    &
                   EDGE,FX,WTX,DEVIAT,h )


















      DIMENSION IEXT(66),AD(66),ALPHA(66),X(66),Y(66)
      DIMENSION H(66)
      DIMENSION DES(1045),GRID(1045),WT(1045)
      DIMENSION EDGE(20),FX(10),WTX(10),DEVIAT(10)
      DOUBLE PRECISION PI2,PI
      DOUBLE PRECISION AD,DEV,X,Y
      LOGICAL LPRINT
      
      PI  = 3.141592653589793
      PI2 = 6.283185307179586



      NFMAX = 128
100      CONTINUE





      IF(NFILT.GT.NFMAX.OR.NFILT.LT.3) THEN
         CALL wrf_error_fatal3("<stdin>",1825,&
' **** ERROR IN INPUT DATA ****' )
      END IF
      IF(NBANDS.LE.0) NBANDS = 1



      IF(LGRID.LE.0) LGRID = 16
      JB = 2*NBANDS



      IF(JTYPE.EQ.0) THEN
         CALL wrf_error_fatal3("<stdin>",1838,&
' **** ERROR IN INPUT DATA ****' )
      END IF
      NEG = 1
      IF(JTYPE.EQ.1) NEG = 0
      NODD = NFILT/2
      NODD = NFILT-2*NODD
      NFCNS = NFILT/2
      IF(NODD.EQ.1.AND.NEG.EQ.0) NFCNS = NFCNS+1



      GRID(1) = EDGE(1)
      DELF = LGRID*NFCNS
      DELF = 0.5/DELF
      IF(NEG.EQ.0) GOTO 135
      IF(EDGE(1).LT.DELF) GRID(1) = DELF
135      CONTINUE
      J = 1
      L = 1
      LBAND = 1
140      FUP = EDGE(L+1)
145      TEMP = GRID(J)



      DES(J) = EFF(TEMP,FX,WTX,LBAND,JTYPE)
      WT(J) = WATE(TEMP,FX,WTX,LBAND,JTYPE)
      J = J+1
      GRID(J) = TEMP+DELF
      IF(GRID(J).GT.FUP) GOTO 150
      GOTO 145
150      GRID(J-1) = FUP
      DES(J-1) = EFF(FUP,FX,WTX,LBAND,JTYPE)
      WT(J-1) = WATE(FUP,FX,WTX,LBAND,JTYPE)
      LBAND = LBAND+1
      L = L+2
      IF(LBAND.GT.NBANDS) GOTO 160
      GRID(J) = EDGE(L)
      GOTO 140
160      NGRID = J-1
      IF(NEG.NE.NODD) GOTO 165
      IF(GRID(NGRID).GT.(0.5-DELF)) NGRID = NGRID-1
165      CONTINUE



      IF(NEG) 170,170,180
170      IF(NODD.EQ.1) GOTO 200
      DO 175 J=1,NGRID
            CHANGE = DCOS(PI*GRID(J))
            DES(J) = DES(J)/CHANGE
            WT(J) = WT(J)*CHANGE
175      CONTINUE
      GOTO 200
180      IF(NODD.EQ.1) GOTO 190
      DO 185 J = 1,NGRID
            CHANGE = DSIN(PI*GRID(J))
            DES(J) = DES(J)/CHANGE
            WT(J)  = WT(J)*CHANGE
185      CONTINUE
      GOTO 200
190      DO 195 J =1,NGRID
            CHANGE = DSIN(PI2*GRID(J))
            DES(J) = DES(J)/CHANGE
            WT(J)  = WT(J)*CHANGE
195      CONTINUE



200      TEMP = FLOAT(NGRID-1)/FLOAT(NFCNS)
      DO 210 J = 1,NFCNS
            IEXT(J) = (J-1)*TEMP+1
210      CONTINUE
      IEXT(NFCNS+1) = NGRID
      NM1 = NFCNS-1
      NZ  = NFCNS+1



      CALL REMEZ(EDGE,NBANDS,PI2,AD,DEV,X,Y,GRID,DES,WT,ALPHA,IEXT,NFCNS,NGRID)



      IF(NEG) 300,300,320
300      IF(NODD.EQ.0) GOTO 310
      DO 305 J=1,NM1
            H(J) = 0.5*ALPHA(NZ-J)
305      CONTINUE
      H(NFCNS)=ALPHA(1)
      GOTO 350
310      H(1) = 0.25*ALPHA(NFCNS)
      DO 315 J = 2,NM1
            H(J) = 0.25*(ALPHA(NZ-J)+ALPHA(NFCNS+2-J))
315      CONTINUE
      H(NFCNS) = 0.5*ALPHA(1)+0.25*ALPHA(2)
      GOTO 350
320      IF(NODD.EQ.0) GOTO 330
      H(1) = 0.25*ALPHA(NFCNS)
      H(2) = 0.25*ALPHA(NM1)
      DO 325 J = 3,NM1
            H(J) = 0.25*(ALPHA(NZ-J)-ALPHA(NFCNS+3-J))
325      CONTINUE
      H(NFCNS) = 0.5*ALPHA(1)-0.25*ALPHA(3)
      H(NZ) = 0.0
      GOTO 350
330      H(1) = 0.25*ALPHA(NFCNS)
      DO 335 J =2,NM1
            H(J) = 0.25*(ALPHA(NZ-J)-ALPHA(NFCNS+2-J))
335      CONTINUE
      H(NFCNS) = 0.5*ALPHA(1)-0.25*ALPHA(2)



350   CONTINUE

      IF(LPRINT) THEN

         print *, '****************************************************'
         print *, 'FINITE IMPULSE RESPONSE (FIR)'
         print *, 'LINEAR PHASE DIGITAL FILTER DESIGN'
         print *, 'REMEZ EXCHANGE ALGORITHM'
      
      IF(JTYPE.EQ.1) WRITE(6,365)
365      FORMAT(25X,'BANDPASS FILTER'/)

      IF(JTYPE.EQ.2) WRITE(6,370)
370      FORMAT(25X,'DIFFERENTIATOR '/)

      IF(JTYPE.EQ.3) WRITE(6,375)
375      FORMAT(25X,'HILBERT TRANSFORMER '/)

      WRITE(6,378) NFILT
378      FORMAT(15X,'FILTER LENGTH =',I3/)

      WRITE(6,380)
380      FORMAT(15X,'***** IMPULSE RESPONSE *****')

      DO 381 J = 1,NFCNS
            K = NFILT+1-J
            IF(NEG.EQ.0) WRITE(6,382) J,H(J),K
            IF(NEG.EQ.1) WRITE(6,383) J,H(J),K
381      CONTINUE
382      FORMAT(20X,'H(',I3,') = ',E15.8,' = H(',I4,')')
383      FORMAT(20X,'H(',I3,') = ',E15.8,' = -H(',I4,')')

      IF(NEG.EQ.1.AND.NODD.EQ.1) WRITE(6,384) NZ
384      FORMAT(20X,'H(',I3,') = 0.0')

      DO 450 K=1,NBANDS,4
            KUP = K+3
            IF(KUP.GT.NBANDS) KUP = NBANDS
	    print *
            WRITE(6,385) (J,J=K,KUP)
385            FORMAT(24X,4('BAND',I3,8X))
            WRITE(6,390) (EDGE(2*J-1),J=K,KUP)
390            FORMAT(2X,'LOWER BAND EDGE',5F15.8)
            WRITE(6,395) (EDGE(2*J),J=K,KUP)
395            FORMAT(2X,'UPPER BAND EDGE',5F15.8)
            IF(JTYPE.NE.2) WRITE(6,400) (FX(J),J=K,KUP)
400            FORMAT(2X,'DESIRED VALUE',2X,5F15.8)
            IF(JTYPE.EQ.2) WRITE(6,405) (FX(J),J=K,KUP)
405            FORMAT(2X,'DESIRED SLOPE',2X,5F15.8)
            WRITE(6,410) (WTX(J),J=K,KUP)
410            FORMAT(2X,'WEIGHTING',6X,5F15.8)
            DO 420 J = K,KUP
                  DEVIAT(J) = DEV/WTX(J)
420            CONTINUE
            WRITE(6,425) (DEVIAT(J),J=K,KUP)
425            FORMAT(2X,'DEVIATION',6X,5F15.8)
            IF(JTYPE.NE.1) GOTO 450
            DO 430 J = K,KUP
                  DEVIAT(J) = 20.0*ALOG10(DEVIAT(J))
430            CONTINUE
            WRITE(6,435) (DEVIAT(J),J=K,KUP)
435            FORMAT(2X,'DEVIATION IN DB',5F15.8)
450      CONTINUE
      print *, 'EXTREMAL FREQUENCIES'
      WRITE(6,455) (GRID(IEXT(J)),J=1,NZ)
455      FORMAT((2X,5F15.7))
      WRITE(6,460)
460      FORMAT(1X,70(1H*))

      ENDIF



   END SUBROUTINE mccpar


      FUNCTION EFF(TEMP,FX,WTX,LBAND,JTYPE)
         DIMENSION FX(5),WTX(5)
         IF(JTYPE.EQ.2) GOTO 1
         EFF = FX(LBAND)
         RETURN
1        EFF = FX(LBAND)*TEMP
      END FUNCTION eff


      FUNCTION WATE(TEMP,FX,WTX,LBAND,JTYPE)
         DIMENSION FX(5),WTX(5)
         IF(JTYPE.EQ.2) GOTO 1
         WATE = WTX(LBAND)
         RETURN
1        IF(FX(LBAND).LT.0.0001) GOTO 2
         WATE = WTX(LBAND)/TEMP
         RETURN
2        WATE = WTX(LBAND)
      END FUNCTION wate







      
      SUBROUTINE REMEZ(EDGE,NBANDS,PI2,AD,DEV,X,Y,GRID,DES,WT,ALPHA,IEXT,NFCNS,NGRID)












      DIMENSION EDGE(20)
      DIMENSION IEXT(66),AD(66),ALPHA(66),X(66),Y(66)
      DIMENSION DES(1045),GRID(1045),WT(1045)
      DIMENSION A(66),P(65),Q(65)
      DOUBLE PRECISION PI2,DNUM,DDEN,DTEMP,A,P,Q
      DOUBLE PRECISION AD,DEV,X,Y
      DOUBLE PRECISION, EXTERNAL :: D, GEE



      ITRMAX=25
      DEVL=-1.0
      NZ=NFCNS+1
      NZZ=NFCNS+2
      NITER=0
  100 CONTINUE
      IEXT(NZZ)=NGRID+1
      NITER=NITER+1
      IF(NITER.GT.ITRMAX) GO TO 400
      DO 110 J=1,NZ
      DTEMP=GRID(IEXT(J))
      DTEMP=DCOS(DTEMP*PI2)
  110 X(J)=DTEMP
      JET=(NFCNS-1)/15+1
      DO 120 J=1,NZ
  120 AD(J)=D(J,NZ,JET,X)
      DNUM=0.0
      DDEN=0.0
      K=1
      DO 130 J=1,NZ
      L=IEXT(J)
      DTEMP=AD(J)*DES(L)
      DNUM=DNUM+DTEMP
      DTEMP=K*AD(J)/WT(L)
      DDEN=DDEN+DTEMP
  130 K=-K
      DEV=DNUM/DDEN
      NU=1
      IF(DEV.GT.0.0) NU=-1
      DEV=-NU*DEV
      K=NU
      DO 140 J=1,NZ
      L=IEXT(J)
      DTEMP=K*DEV/WT(L)
      Y(J)=DES(L)+DTEMP
  140 K=-K
      IF(DEV.GE.DEVL) GO TO 150
      WRITE(6,*) ' ******** FAILURE TO CONVERGE *********** '
      WRITE(6,*) ' PROBABLE CAUSE IS MACHINE ROUNDING ERROR '
      WRITE(6,*) ' THE IMPULSE RESPONSE MAY BE CORRECT '
      WRITE(6,*) ' CHECK WITH A FREQUENCY RESPONSE '
      WRITE(6,*) ' **************************************** '
      GO TO 400
  150 DEVL=DEV
      JCHNGE=0
      K1=IEXT(1)
      KNZ=IEXT(NZ)
      KLOW=0
      NUT=-NU
      J=1




  200 IF(J.EQ.NZZ) YNZ=COMP
      IF(J.GE.NZZ) GO TO 300
      KUP=IEXT(J+1)
      L=IEXT(J)+1
      NUT=-NUT
      IF(J.EQ.2) Y1=COMP
      COMP=DEV
      IF(L.GE.KUP) GO TO 220
      ERR=GEE(L,NZ,GRID,PI2,X,Y,AD)
      ERR=(ERR-DES(L))*WT(L)
      DTEMP=NUT*ERR-COMP
      IF(DTEMP.LE.0.0) GO TO 220
      COMP=NUT*ERR
  210 L=L+1
      IF(L.GE.KUP) GO TO 215
      ERR=GEE(L,NZ,GRID,PI2,X,Y,AD)
      ERR=(ERR-DES(L))*WT(L)
      DTEMP=NUT*ERR-COMP
      IF(DTEMP.LE.0.0) GO TO 215
      COMP=NUT*ERR
      GO TO 210
  215 IEXT(J)=L-1
      J=J+1
      KLOW=L-1
      JCHNGE=JCHNGE+1
      GO TO 200
  220 L=L-1
  225 L=L-1
      IF(L.LE.KLOW) GO TO 250
      ERR=GEE(L,NZ,GRID,PI2,X,Y,AD)
      ERR=(ERR-DES(L))*WT(L)
      DTEMP=NUT*ERR-COMP
      IF(DTEMP.GT.0.0) GO TO 230
      IF(JCHNGE.LE.0) GO TO 225
      GO TO 260
  230 COMP=NUT*ERR
  235 L=L-1
      IF(L.LE.KLOW) GO TO 240
      ERR=GEE(L,NZ,GRID,PI2,X,Y,AD)
      ERR=(ERR-DES(L))*WT(L)
      DTEMP=NUT*ERR-COMP
      IF(DTEMP.LE.0.0) GO TO 240
      COMP=NUT*ERR
      GO TO 235
  240 KLOW=IEXT(J)
      IEXT(J)=L+1
      J=J+1
      JCHNGE=JCHNGE+1
      GO TO 200
  250 L=IEXT(J)+1
      IF(JCHNGE.GT.0) GO TO 215
  255 L=L+1
      IF(L.GE.KUP) GO TO 260
      ERR=GEE(L,NZ,GRID,PI2,X,Y,AD)
      ERR=(ERR-DES(L))*WT(L)
      DTEMP=NUT*ERR-COMP
      IF(DTEMP.LE.0.0) GO TO 255
      COMP=NUT*ERR
      GO TO 210
  260 KLOW=IEXT(J)
      J=J+1
      GO TO 200
  300 IF(J.GT.NZZ) GO TO 320
      IF(K1.GT.IEXT(1)) K1=IEXT(1)
      IF(KNZ.LT.IEXT(NZ)) KNZ=IEXT(NZ)
      NUT1=NUT
      NUT=-NU
      L=0
      KUP=K1
      COMP=YNZ*(1.00001)
      LUCK=1
  310 L=L+1
      IF(L.GE.KUP) GO TO 315
      ERR=GEE(L,NZ,GRID,PI2,X,Y,AD)
      ERR=(ERR-DES(L))*WT(L)
      DTEMP=NUT*ERR-COMP
      IF(DTEMP.LE.0.0) GO TO 310
      COMP=NUT*ERR
      J=NZZ
      GO TO 210
  315 LUCK=6
      GO TO 325
  320 IF(LUCK.GT.9) GO TO 350
      IF(COMP.GT.Y1) Y1=COMP
      K1=IEXT(NZZ)
  325 L=NGRID+1
      KLOW=KNZ
      NUT=-NUT1
      COMP=Y1*(1.00001)
  330 L=L-1
      IF(L.LE.KLOW) GO TO 340
      ERR=GEE(L,NZ,GRID,PI2,X,Y,AD)
      ERR=(ERR-DES(L))*WT(L)
      DTEMP=NUT*ERR-COMP
      IF(DTEMP.LE.0.0) GO TO 330
      J=NZZ
      COMP=NUT*ERR
      LUCK=LUCK+10
      GO TO 235
  340 IF(LUCK.EQ.6) GO TO 370
      DO 345 J=1,NFCNS
  345 IEXT(NZZ-J)=IEXT(NZ-J)
      IEXT(1)=K1
      GO TO 100
  350 KN=IEXT(NZZ)
      DO 360 J=1,NFCNS
  360 IEXT(J)=IEXT(J+1)
      IEXT(NZ)=KN
      GO TO 100
  370 IF(JCHNGE.GT.0) GO TO 100




  400 CONTINUE
      NM1=NFCNS-1
      FSH=1.0E-06
      GTEMP=GRID(1)
      X(NZZ)=-2.0
      CN=2*NFCNS-1
      DELF=1.0/CN
      L=1
      KKK=0
      IF(EDGE(1).EQ.0.0.AND.EDGE(2*NBANDS).EQ.0.5) KKK=1
      IF(NFCNS.LE.3) KKK=1
      IF(KKK.EQ.1) GO TO 405
      DTEMP=DCOS(PI2*GRID(1))
      DNUM=DCOS(PI2*GRID(NGRID))
      AA=2.0/(DTEMP-DNUM)
      BB=-(DTEMP+DNUM)/(DTEMP-DNUM)
  405 CONTINUE
      DO 430 J=1,NFCNS
      FT=(J-1)*DELF
      XT=DCOS(PI2*FT)
      IF(KKK.EQ.1) GO TO 410
      XT=(XT-BB)/AA

      FT=ACOS(XT)/PI2
  410 XE=X(L)
      IF(XT.GT.XE) GO TO 420
      IF((XE-XT).LT.FSH) GO TO 415
      L=L+1
      GO TO 410
  415 A(J)=Y(L)
      GO TO 425
  420 IF((XT-XE).LT.FSH) GO TO 415
      GRID(1)=FT
      A(J)=GEE(1,NZ,GRID,PI2,X,Y,AD)
  425 CONTINUE
      IF(L.GT.1) L=L-1
  430 CONTINUE
      GRID(1)=GTEMP
      DDEN=PI2/CN
      DO 510 J=1,NFCNS
      DTEMP=0.0
      DNUM=(J-1)*DDEN
      IF(NM1.LT.1) GO TO 505
      DO 500 K=1,NM1
  500 DTEMP=DTEMP+A(K+1)*DCOS(DNUM*K)
  505 DTEMP=2.0*DTEMP+A(1)
  510 ALPHA(J)=DTEMP
      DO 550 J=2,NFCNS
  550 ALPHA(J)=2*ALPHA(J)/CN
      ALPHA(1)=ALPHA(1)/CN
      IF(KKK.EQ.1) GO TO 545
      P(1)=2.0*ALPHA(NFCNS)*BB+ALPHA(NM1)
      P(2)=2.0*AA*ALPHA(NFCNS)
      Q(1)=ALPHA(NFCNS-2)-ALPHA(NFCNS)
      DO 540 J=2,NM1
      IF(J.LT.NM1) GO TO 515
      AA=0.5*AA
      BB=0.5*BB
  515 CONTINUE
      P(J+1)=0.0
      DO 520 K=1,J
      A(K)=P(K)
  520 P(K)=2.0*BB*A(K)
      P(2)=P(2)+A(1)*2.0*AA
      JM1=J-1
      DO 525 K=1,JM1
  525 P(K)=P(K)+Q(K)+AA*A(K+1)
      JP1=J+1
      DO 530 K=3,JP1
  530 P(K)=P(K)+AA*A(K-1)
      IF(J.EQ.NM1) GO TO 540
      DO 535 K=1,J
  535 Q(K)=-A(K)
      Q(1)=Q(1)+ALPHA(NFCNS-1-J)
  540 CONTINUE
      DO 543 J=1,NFCNS
  543 ALPHA(J)=P(J)
  545 CONTINUE
      IF(NFCNS.GT.3) RETURN
      ALPHA(NFCNS+1)=0.0
      ALPHA(NFCNS+2)=0.0
   END SUBROUTINE remez

   DOUBLE PRECISION FUNCTION D(K,N,M,X)

      DIMENSION IEXT(66),AD(66),ALPHA(66),X(66),Y(66)
      DIMENSION DES(1045),GRID(1045),WT(1045)
      DOUBLE PRECISION AD,DEV,X,Y
      DOUBLE PRECISION Q
      DOUBLE PRECISION PI2
      D = 1.0
      Q = X(K)
      DO 3 L = 1,M
      DO 2 J = L,N,M
            IF(J-K) 1,2,1
1                  D = 2.0*D*(Q-X(J))
2      CONTINUE
3      CONTINUE
      D = 1.0/D
   END FUNCTION D


   DOUBLE PRECISION FUNCTION GEE(K,N,GRID,PI2,X,Y,AD)

      DIMENSION IEXT(66),AD(66),ALPHA(66),X(66),Y(66)
      DIMENSION DES(1045),GRID(1045),WT(1045)
      DOUBLE PRECISION AD,DEV,X,Y
      DOUBLE PRECISION P,C,D,XF
      DOUBLE PRECISION PI2
      P = 0.0
      XF = GRID(K)
      XF = DCOS(PI2*XF)
      D = 0.0
      DO 1 J =1,N
            C = XF-X(J)
            C = AD(J)/C
            D = D+C
            P = P+C*Y(J)
1      CONTINUE
      GEE = P/D
   END FUNCTION GEE






      REAL FUNCTION RSLF(P,T)

      IMPLICIT NONE
      REAL, INTENT(IN):: P, T
      REAL:: ESL,X
      REAL, PARAMETER:: C0= .611583699E03
      REAL, PARAMETER:: C1= .444606896E02
      REAL, PARAMETER:: C2= .143177157E01
      REAL, PARAMETER:: C3= .264224321E-1
      REAL, PARAMETER:: C4= .299291081E-3
      REAL, PARAMETER:: C5= .203154182E-5
      REAL, PARAMETER:: C6= .702620698E-8
      REAL, PARAMETER:: C7= .379534310E-11
      REAL, PARAMETER:: C8=-.321582393E-13

      X=MAX(-80.,T-273.16)


      ESL=C0+X*(C1+X*(C2+X*(C3+X*(C4+X*(C5+X*(C6+X*(C7+X*C8)))))))
      RSLF=.622*ESL/(P-ESL)

      END FUNCTION RSLF




      REAL FUNCTION RSIF(P,T)

      IMPLICIT NONE
      REAL, INTENT(IN):: P, T
      REAL:: ESI,X
      REAL, PARAMETER:: C0= .609868993E03
      REAL, PARAMETER:: C1= .499320233E02
      REAL, PARAMETER:: C2= .184672631E01
      REAL, PARAMETER:: C3= .402737184E-1
      REAL, PARAMETER:: C4= .565392987E-3
      REAL, PARAMETER:: C5= .521693933E-5
      REAL, PARAMETER:: C6= .307839583E-7
      REAL, PARAMETER:: C7= .105785160E-9
      REAL, PARAMETER:: C8= .161444444E-12

      X=MAX(-80.,T-273.16)
      ESI=C0+X*(C1+X*(C2+X*(C3+X*(C4+X*(C5+X*(C6+X*(C7+X*C8)))))))
      RSIF=.622*ESI/(P-ESI)







      END FUNCTION RSIF







   SUBROUTINE wrf_dfi_startfwd_init ( )

     USE module_domain, ONLY : domain, head_grid, domain_get_stop_time, domain_get_start_time, set_current_grid_ptr
     USE module_utility
     
     IMPLICIT NONE

     INTERFACE
        SUBROUTINE dfi_startfwd_init_recurse(grid)
          USE module_domain, ONLY : domain
          TYPE (domain), POINTER :: grid
        END SUBROUTINE dfi_startfwd_init_recurse
     END INTERFACE

     

     CALL dfi_startfwd_init_recurse(head_grid)
     
     CALL set_current_grid_ptr( head_grid )

   END SUBROUTINE wrf_dfi_startfwd_init


   RECURSIVE SUBROUTINE dfi_startfwd_init_recurse(grid)

     USE module_domain, ONLY : domain, head_grid, domain_get_stop_time, domain_get_start_time, max_nests, set_current_grid_ptr

     IMPLICIT NONE

      INTERFACE
         SUBROUTINE dfi_startfwd_init(grid)
            USE module_domain, ONLY : domain
            TYPE (domain), POINTER :: grid
         END SUBROUTINE dfi_startfwd_init
      END INTERFACE

     INTEGER :: kid
     TYPE (domain), POINTER :: grid
     TYPE (domain), POINTER :: grid_ptr
    
     grid_ptr => grid

     DO WHILE ( ASSOCIATED( grid_ptr ) )
        
        
        
        
        grid_ptr%dt = abs(grid_ptr%dt)
        grid_ptr%time_step = abs(grid_ptr%time_step)
        CALL set_current_grid_ptr( grid_ptr )
        CALL dfi_startfwd_init( grid_ptr )
        DO kid = 1, max_nests
           IF ( ASSOCIATED( grid_ptr%nests(kid)%ptr ) ) THEN
              CALL dfi_startfwd_init_recurse(grid_ptr%nests(kid)%ptr)
           ENDIF
        END DO
        grid_ptr => grid_ptr%sibling
     END DO
     
   END SUBROUTINE dfi_startfwd_init_recurse


   SUBROUTINE dfi_startfwd_init ( grid )

      USE module_domain, ONLY : domain, head_grid, domain_get_stop_time, domain_get_start_time
      USE module_utility
      USE module_state_description

      IMPLICIT NONE

      TYPE (domain) , POINTER                 :: grid
      INTEGER rc

      INTERFACE
         SUBROUTINE Setup_Timekeeping(grid)
            USE module_domain, ONLY : domain
            TYPE (domain), POINTER :: grid
         END SUBROUTINE Setup_Timekeeping

      END INTERFACE

      grid%dfi_stage = DFI_STARTFWD

      
      CALL nl_set_use_adaptive_time_step( grid%id, .false. )

      CALL Setup_Timekeeping (grid)
      grid%start_subtime = domain_get_start_time ( head_grid )
      grid%stop_subtime = domain_get_stop_time ( head_grid )

      CALL WRFU_ClockSet(grid%domain_clock, currTime=grid%start_subtime, rc=rc)

   END SUBROUTINE dfi_startfwd_init





   SUBROUTINE wrf_dfi_startbck_init ( )

     USE module_domain, ONLY : domain, head_grid, domain_get_stop_time, domain_get_start_time, set_current_grid_ptr
     USE module_utility
     
     IMPLICIT NONE

     INTERFACE
        SUBROUTINE dfi_startbck_init_recurse(grid)
          USE module_domain, ONLY : domain
          TYPE (domain), POINTER :: grid
        END SUBROUTINE dfi_startbck_init_recurse
     END INTERFACE

     

     CALL dfi_startbck_init_recurse(head_grid)
     
     CALL set_current_grid_ptr( head_grid )

   END SUBROUTINE wrf_dfi_startbck_init


   RECURSIVE SUBROUTINE dfi_startbck_init_recurse(grid)

     USE module_domain, ONLY : domain, head_grid, domain_get_stop_time, domain_get_start_time, max_nests, set_current_grid_ptr

     IMPLICIT NONE

      INTERFACE
         SUBROUTINE dfi_startbck_init(grid)
            USE module_domain, ONLY : domain
            TYPE (domain), POINTER :: grid
         END SUBROUTINE dfi_startbck_init
      END INTERFACE

     INTEGER :: kid
     TYPE (domain), POINTER :: grid
     TYPE (domain), POINTER :: grid_ptr
    
     grid_ptr => grid

     DO WHILE ( ASSOCIATED( grid_ptr ) )
        
        
        
        
        grid_ptr%dt = abs(grid_ptr%dt)
        grid_ptr%time_step = abs(grid_ptr%time_step)
        CALL set_current_grid_ptr( grid_ptr )
        CALL dfi_startbck_init( grid_ptr )
        DO kid = 1, max_nests
           IF ( ASSOCIATED( grid_ptr%nests(kid)%ptr ) ) THEN
              CALL dfi_startbck_init_recurse(grid_ptr%nests(kid)%ptr)
           ENDIF
        END DO
        grid_ptr => grid_ptr%sibling
     END DO
     
   END SUBROUTINE dfi_startbck_init_recurse


   SUBROUTINE dfi_startbck_init ( grid )

      USE module_domain, ONLY : domain, head_grid, domain_get_stop_time, domain_get_start_time
      USE module_utility
      USE module_state_description

      IMPLICIT NONE

      TYPE (domain) , POINTER                 :: grid
      INTEGER rc

      INTERFACE
         SUBROUTINE Setup_Timekeeping(grid)
            USE module_domain, ONLY : domain
            TYPE (domain), POINTER :: grid
         END SUBROUTINE Setup_Timekeeping

      END INTERFACE

      grid%dfi_stage = DFI_STARTBCK

      
      CALL nl_set_mp_physics( grid%id, 0 )
      CALL nl_set_ra_lw_physics( grid%id, 0 )
      CALL nl_set_ra_sw_physics( grid%id, 0 )
      CALL nl_set_sf_surface_physics( grid%id, 0 )
      CALL nl_set_sf_sfclay_physics( grid%id, 0 )
      CALL nl_set_sf_urban_physics( grid%id, 0 )
      CALL nl_set_bl_pbl_physics( grid%id, 0 )
      CALL nl_set_cu_physics( grid%id, 0 )
      CALL nl_set_cu_diag( grid%id, 0 )
      CALL nl_set_damp_opt( grid%id, 0 )
      CALL nl_set_sst_update( grid%id, 0 )
      CALL nl_set_gwd_opt( grid%id, 0 )
      CALL nl_set_diff_6th_opt( grid%id, 0 )
      CALL nl_set_use_adaptive_time_step( grid%id, .false. )
      CALL nl_set_feedback( grid%id, 0 )
      
      CALL nl_set_constant_bc( grid%id, head_grid%constant_bc)


      
      grid%km_opt_dfi = grid%km_opt
      grid%diff_opt_dfi = grid%diff_opt
      CALL nl_set_km_opt( grid%id, grid%km_opt_dfi)
      CALL nl_set_diff_opt( grid%id, grid%diff_opt_dfi)
      CALL nl_set_moist_adv_dfi_opt( grid%id, grid%moist_adv_dfi_opt)
      IF ( grid%moist_adv_opt == 2 ) THEN
         CALL nl_set_moist_adv_opt( grid%id, 0)
      ENDIF

      
      

      CALL nl_set_p_lev_diags( grid%id, grid%p_lev_diags_dfi)

      
      
      if (grid%id == 1) then
        CALL start_domain ( grid , .TRUE. )
      endif

      

      
      CALL nl_set_time_step ( grid%id, -grid%time_step )

      CALL Setup_Timekeeping (grid) 

      grid%start_subtime = domain_get_start_time ( grid )
      grid%stop_subtime = domain_get_stop_time ( grid )

      CALL WRFU_ClockSet(grid%domain_clock, currTime=grid%start_subtime, rc=rc)

   END SUBROUTINE dfi_startbck_init


   SUBROUTINE wrf_dfi_bck_init ( )

     USE module_domain, ONLY : domain, head_grid, domain_get_stop_time, domain_get_start_time
     USE module_utility
     USE module_state_description
     
     IMPLICIT NONE

     INTERFACE
        SUBROUTINE dfi_bck_init_recurse(grid)
          USE module_domain, ONLY : domain
          TYPE (domain), POINTER :: grid
        END SUBROUTINE dfi_bck_init_recurse
     END INTERFACE

     
     
     
     
     CALL dfi_bck_init_recurse(head_grid)

   END SUBROUTINE wrf_dfi_bck_init

   RECURSIVE SUBROUTINE dfi_bck_init_recurse(grid)

     USE module_domain, ONLY : domain, domain_get_stop_time, domain_get_start_time, max_nests, set_current_grid_ptr

     IMPLICIT NONE

      INTERFACE
         SUBROUTINE dfi_bck_init(grid)
            USE module_domain, ONLY : domain
            TYPE (domain), POINTER :: grid
         END SUBROUTINE dfi_bck_init
      END INTERFACE

     INTEGER :: kid
     TYPE (domain), POINTER :: grid
     TYPE (domain), POINTER :: grid_ptr
    
     grid_ptr => grid

     DO WHILE ( ASSOCIATED( grid_ptr ) )
        
        
        
        
        grid_ptr%dt = abs(grid_ptr%dt)
        grid_ptr%time_step = abs(grid_ptr%time_step)
        CALL set_current_grid_ptr( grid_ptr )
        CALL dfi_bck_init( grid_ptr )
        DO kid = 1, max_nests
           IF ( ASSOCIATED( grid_ptr%nests(kid)%ptr ) ) THEN
              CALL dfi_bck_init_recurse(grid_ptr%nests(kid)%ptr)
           ENDIF
        END DO
        grid_ptr => grid_ptr%sibling
     END DO
     
   END SUBROUTINE dfi_bck_init_recurse





   SUBROUTINE wrf_dfi_fwd_init ( )

     USE module_domain, ONLY : domain, head_grid, domain_get_stop_time, domain_get_start_time, set_current_grid_ptr
     USE module_utility
     
     IMPLICIT NONE

     INTERFACE
        SUBROUTINE dfi_fwd_init_recurse(grid)
          USE module_domain, ONLY : domain
          TYPE (domain), POINTER :: grid
        END SUBROUTINE dfi_fwd_init_recurse
     END INTERFACE

     

     CALL dfi_fwd_init_recurse(head_grid)
     
     CALL set_current_grid_ptr( head_grid )

   END SUBROUTINE wrf_dfi_fwd_init

   RECURSIVE SUBROUTINE dfi_fwd_init_recurse(grid)

     USE module_domain, ONLY : domain, head_grid, domain_get_stop_time, domain_get_start_time, max_nests, set_current_grid_ptr

     IMPLICIT NONE

      INTERFACE
         SUBROUTINE dfi_fwd_init(grid)
            USE module_domain, ONLY : domain
            TYPE (domain), POINTER :: grid
         END SUBROUTINE dfi_fwd_init
      END INTERFACE

     INTEGER :: kid
     TYPE (domain), POINTER :: grid
     TYPE (domain), POINTER :: grid_ptr
    
     grid_ptr => grid

     DO WHILE ( ASSOCIATED( grid_ptr ) )
        
        
        
        
        grid_ptr%dt = abs(grid_ptr%dt)
        grid_ptr%time_step = abs(grid_ptr%time_step)
        CALL set_current_grid_ptr( grid_ptr )
        CALL dfi_fwd_init( grid_ptr )
        DO kid = 1, max_nests
           IF ( ASSOCIATED( grid_ptr%nests(kid)%ptr ) ) THEN
              CALL dfi_fwd_init_recurse(grid_ptr%nests(kid)%ptr)
           ENDIF
        END DO
        grid_ptr => grid_ptr%sibling
     END DO
     
   END SUBROUTINE dfi_fwd_init_recurse





   SUBROUTINE wrf_dfi_fst_init ( )

     USE module_domain, ONLY : domain, head_grid, domain_get_stop_time, domain_get_start_time, set_current_grid_ptr
     USE module_utility
     
     IMPLICIT NONE

     INTERFACE
        SUBROUTINE dfi_fst_init_recurse(grid)
          USE module_domain, ONLY : domain
          TYPE (domain), POINTER :: grid
        END SUBROUTINE dfi_fst_init_recurse
     END INTERFACE

     

     CALL dfi_fst_init_recurse(head_grid)
     
     CALL set_current_grid_ptr( head_grid )

   END SUBROUTINE wrf_dfi_fst_init

   RECURSIVE SUBROUTINE dfi_fst_init_recurse ( grid )

     USE module_domain, ONLY : domain, domain_get_stop_time, domain_get_start_time, max_nests, set_current_grid_ptr

     IMPLICIT NONE

      INTERFACE
         SUBROUTINE dfi_fst_init(grid)
            USE module_domain, ONLY : domain
            TYPE (domain), POINTER :: grid
         END SUBROUTINE dfi_fst_init
      END INTERFACE

     INTEGER :: kid
     TYPE (domain), POINTER :: grid
     TYPE (domain), POINTER :: grid_ptr
    
     grid_ptr => grid

     DO WHILE ( ASSOCIATED( grid_ptr ) )
        
        
        
        
        grid_ptr%dt = abs(grid_ptr%dt)
        grid_ptr%time_step = abs(grid_ptr%time_step)
        CALL set_current_grid_ptr( grid_ptr )
        CALL dfi_fst_init( grid_ptr )
        DO kid = 1, max_nests
           IF ( ASSOCIATED( grid_ptr%nests(kid)%ptr ) ) THEN
              CALL dfi_fst_init_recurse(grid_ptr%nests(kid)%ptr)
           ENDIF
        END DO
        grid_ptr => grid_ptr%sibling
     END DO
     
   END SUBROUTINE dfi_fst_init_recurse





   SUBROUTINE wrf_dfi_write_initialized_state( )

     USE module_domain, ONLY : domain, head_grid

     INTERFACE
        SUBROUTINE dfi_write_initialized_state_recurse(grid)
          USE module_domain, ONLY : domain
          TYPE (domain), POINTER :: grid
        END SUBROUTINE dfi_write_initialized_state_recurse
     END INTERFACE

     

     CALL dfi_write_initialized_state_recurse(head_grid)
     
   END SUBROUTINE wrf_dfi_write_initialized_state

   RECURSIVE SUBROUTINE dfi_write_initialized_state_recurse( grid )

     USE module_domain, ONLY : domain, max_nests
     
     IMPLICIT NONE
     
     INTERFACE
        SUBROUTINE dfi_write_initialized_state( grid )
          USE module_domain, ONLY : domain
          TYPE (domain), POINTER :: grid
        END SUBROUTINE dfi_write_initialized_state
     END INTERFACE
     
     INTEGER :: kid
     TYPE (domain), POINTER :: grid
     TYPE (domain), POINTER :: grid_ptr
     
     grid_ptr => grid
     
     DO WHILE ( ASSOCIATED( grid_ptr ) )
        
        
        
        
        CALL dfi_write_initialized_state( grid_ptr )
        DO kid = 1, max_nests
           IF ( ASSOCIATED( grid_ptr%nests(kid)%ptr ) ) THEN
              CALL dfi_write_initialized_state_recurse(grid_ptr%nests(kid)%ptr)
           ENDIF
        END DO
        grid_ptr => grid_ptr%sibling
     END DO
     
   END SUBROUTINE dfi_write_initialized_state_recurse

   SUBROUTINE wrf_tdfi_write_analyzed_state( )

     USE module_domain, ONLY : domain, head_grid

     INTERFACE
        SUBROUTINE tdfi_write_analyzed_state_recurse(grid)
          USE module_domain, ONLY : domain
          TYPE (domain), POINTER :: grid
        END SUBROUTINE tdfi_write_analyzed_state_recurse
     END INTERFACE

     

     CALL tdfi_write_analyzed_state_recurse(head_grid)

   END SUBROUTINE wrf_tdfi_write_analyzed_state

   RECURSIVE SUBROUTINE tdfi_write_analyzed_state_recurse( grid )

     USE module_domain, ONLY : domain, max_nests

     IMPLICIT NONE

     INTERFACE
        SUBROUTINE tdfi_write_analyzed_state( grid )
          USE module_domain, ONLY : domain
          TYPE (domain), POINTER :: grid
        END SUBROUTINE tdfi_write_analyzed_state
     END INTERFACE

     INTEGER :: kid
     TYPE (domain), POINTER :: grid
     TYPE (domain), POINTER :: grid_ptr

     grid_ptr => grid

     DO WHILE ( ASSOCIATED( grid_ptr ) )
        
        
        
        
        CALL tdfi_write_analyzed_state( grid_ptr )
        DO kid = 1, max_nests
           IF ( ASSOCIATED( grid_ptr%nests(kid)%ptr ) ) THEN
              CALL tdfi_write_analyzed_state_recurse(grid_ptr%nests(kid)%ptr)
           ENDIF
        END DO
        grid_ptr => grid_ptr%sibling
     END DO

   END SUBROUTINE tdfi_write_analyzed_state_recurse

   RECURSIVE SUBROUTINE dfi_array_reset_recurse(grid)

     USE module_domain, ONLY : domain, max_nests, set_current_grid_ptr

     IMPLICIT NONE

      INTERFACE
         SUBROUTINE dfi_array_reset(grid)
            USE module_domain, ONLY : domain
            TYPE (domain), POINTER :: grid
         END SUBROUTINE dfi_array_reset
      END INTERFACE

     INTEGER :: kid
     TYPE (domain), POINTER :: grid
     TYPE (domain), POINTER :: grid_ptr
    
     grid_ptr => grid

     DO WHILE ( ASSOCIATED( grid_ptr ) )
        CALL set_current_grid_ptr( grid_ptr )
        CALL dfi_array_reset( grid_ptr )
        DO kid = 1, max_nests
           IF ( ASSOCIATED( grid_ptr%nests(kid)%ptr ) ) THEN
              CALL dfi_array_reset_recurse(grid_ptr%nests(kid)%ptr)
           ENDIF
        END DO
        grid_ptr => grid_ptr%sibling
     END DO
     
   END SUBROUTINE dfi_array_reset_recurse



   SUBROUTINE rebalance_driver_dfi ( grid )

      USE module_domain, ONLY : domain

      IMPLICIT NONE

      TYPE (domain)          :: grid

      CALL rebalance_dfi( grid &







,grid%moist,grid%moist_bxs,grid%moist_bxe,grid%moist_bys,grid%moist_bye,grid%moist_btxs,grid%moist_btxe,grid%moist_btys, &
grid%moist_btye,grid%dfi_moist,grid%dfi_moist_bxs,grid%dfi_moist_bxe,grid%dfi_moist_bys,grid%dfi_moist_bye,grid%dfi_moist_btxs, &
grid%dfi_moist_btxe,grid%dfi_moist_btys,grid%dfi_moist_btye,grid%scalar,grid%scalar_bxs,grid%scalar_bxe,grid%scalar_bys, &
grid%scalar_bye,grid%scalar_btxs,grid%scalar_btxe,grid%scalar_btys,grid%scalar_btye,grid%dfi_scalar,grid%dfi_scalar_bxs, &
grid%dfi_scalar_bxe,grid%dfi_scalar_bys,grid%dfi_scalar_bye,grid%dfi_scalar_btxs,grid%dfi_scalar_btxe,grid%dfi_scalar_btys, &
grid%dfi_scalar_btye,grid%aerod,grid%ozmixm,grid%aerosolc_1,grid%aerosolc_2,grid%fdda3d,grid%fdda2d,grid%advh_t,grid%advz_t, &
grid%nba_mij,grid%nba_rij,grid%chem,grid%tracer,grid%tracer_bxs,grid%tracer_bxe,grid%tracer_bys,grid%tracer_bye, &
grid%tracer_btxs,grid%tracer_btxe,grid%tracer_btys,grid%tracer_btye &


      )

   END SUBROUTINE rebalance_driver_dfi



   SUBROUTINE rebalance_dfi ( grid  &







,moist,moist_bxs,moist_bxe,moist_bys,moist_bye,moist_btxs,moist_btxe,moist_btys,moist_btye,dfi_moist,dfi_moist_bxs,dfi_moist_bxe, &
dfi_moist_bys,dfi_moist_bye,dfi_moist_btxs,dfi_moist_btxe,dfi_moist_btys,dfi_moist_btye,scalar,scalar_bxs,scalar_bxe,scalar_bys, &
scalar_bye,scalar_btxs,scalar_btxe,scalar_btys,scalar_btye,dfi_scalar,dfi_scalar_bxs,dfi_scalar_bxe,dfi_scalar_bys, &
dfi_scalar_bye,dfi_scalar_btxs,dfi_scalar_btxe,dfi_scalar_btys,dfi_scalar_btye,aerod,ozmixm,aerosolc_1,aerosolc_2,fdda3d,fdda2d, &
advh_t,advz_t,nba_mij,nba_rij,chem,tracer,tracer_bxs,tracer_bxe,tracer_bys,tracer_bye,tracer_btxs,tracer_btxe,tracer_btys, &
tracer_btye &


                        )
      USE module_domain, ONLY : domain
      USE module_utility
      USE module_state_description
      USE module_model_constants
      USE module_driver_constants
   USE module_dm
   USE module_comm_dm, ONLY : &
                           HALO_EM_INIT_1_sub   &
                          ,HALO_EM_INIT_2_sub   &
                          ,HALO_EM_INIT_3_sub   &
                          ,HALO_EM_INIT_4_sub   &
                          ,HALO_EM_INIT_5_sub   


      IMPLICIT NONE

      TYPE (domain)        :: grid







real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33,num_moist)           :: moist
real      ,DIMENSION(grid%sm33:grid%em33,grid%sm32:grid%em32,grid%spec_bdy_width,num_moist)           :: moist_bxs
real      ,DIMENSION(grid%sm33:grid%em33,grid%sm32:grid%em32,grid%spec_bdy_width,num_moist)           :: moist_bxe
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%spec_bdy_width,num_moist)           :: moist_bys
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%spec_bdy_width,num_moist)           :: moist_bye
real      ,DIMENSION(grid%sm33:grid%em33,grid%sm32:grid%em32,grid%spec_bdy_width,num_moist)           :: moist_btxs
real      ,DIMENSION(grid%sm33:grid%em33,grid%sm32:grid%em32,grid%spec_bdy_width,num_moist)           :: moist_btxe
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%spec_bdy_width,num_moist)           :: moist_btys
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%spec_bdy_width,num_moist)           :: moist_btye
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33,num_dfi_moist)           :: dfi_moist
real      ,DIMENSION(grid%sm33:grid%em33,grid%sm32:grid%em32,grid%spec_bdy_width,num_dfi_moist)           :: dfi_moist_bxs
real      ,DIMENSION(grid%sm33:grid%em33,grid%sm32:grid%em32,grid%spec_bdy_width,num_dfi_moist)           :: dfi_moist_bxe
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%spec_bdy_width,num_dfi_moist)           :: dfi_moist_bys
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%spec_bdy_width,num_dfi_moist)           :: dfi_moist_bye
real      ,DIMENSION(grid%sm33:grid%em33,grid%sm32:grid%em32,grid%spec_bdy_width,num_dfi_moist)           :: dfi_moist_btxs
real      ,DIMENSION(grid%sm33:grid%em33,grid%sm32:grid%em32,grid%spec_bdy_width,num_dfi_moist)           :: dfi_moist_btxe
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%spec_bdy_width,num_dfi_moist)           :: dfi_moist_btys
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%spec_bdy_width,num_dfi_moist)           :: dfi_moist_btye
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33,num_scalar)           :: scalar
real      ,DIMENSION(grid%sm33:grid%em33,grid%sm32:grid%em32,grid%spec_bdy_width,num_scalar)           :: scalar_bxs
real      ,DIMENSION(grid%sm33:grid%em33,grid%sm32:grid%em32,grid%spec_bdy_width,num_scalar)           :: scalar_bxe
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%spec_bdy_width,num_scalar)           :: scalar_bys
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%spec_bdy_width,num_scalar)           :: scalar_bye
real      ,DIMENSION(grid%sm33:grid%em33,grid%sm32:grid%em32,grid%spec_bdy_width,num_scalar)           :: scalar_btxs
real      ,DIMENSION(grid%sm33:grid%em33,grid%sm32:grid%em32,grid%spec_bdy_width,num_scalar)           :: scalar_btxe
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%spec_bdy_width,num_scalar)           :: scalar_btys
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%spec_bdy_width,num_scalar)           :: scalar_btye
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33,num_dfi_scalar)           :: dfi_scalar
real      ,DIMENSION(grid%sm33:grid%em33,grid%sm32:grid%em32,grid%spec_bdy_width,num_dfi_scalar)           :: dfi_scalar_bxs
real      ,DIMENSION(grid%sm33:grid%em33,grid%sm32:grid%em32,grid%spec_bdy_width,num_dfi_scalar)           :: dfi_scalar_bxe
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%spec_bdy_width,num_dfi_scalar)           :: dfi_scalar_bys
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%spec_bdy_width,num_dfi_scalar)           :: dfi_scalar_bye
real      ,DIMENSION(grid%sm33:grid%em33,grid%sm32:grid%em32,grid%spec_bdy_width,num_dfi_scalar)           :: dfi_scalar_btxs
real      ,DIMENSION(grid%sm33:grid%em33,grid%sm32:grid%em32,grid%spec_bdy_width,num_dfi_scalar)           :: dfi_scalar_btxe
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%spec_bdy_width,num_dfi_scalar)           :: dfi_scalar_btys
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%spec_bdy_width,num_dfi_scalar)           :: dfi_scalar_btye
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33,num_aerod)           :: aerod
real      ,DIMENSION(grid%sm31:grid%em31,1:grid%levsiz,grid%sm33:grid%em33,num_ozmixm)           :: ozmixm
real      ,DIMENSION(grid%sm31:grid%em31,1:grid%paerlev,grid%sm33:grid%em33,num_aerosolc)           :: aerosolc_1
real      ,DIMENSION(grid%sm31:grid%em31,1:grid%paerlev,grid%sm33:grid%em33,num_aerosolc)           :: aerosolc_2
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33,num_fdda3d)           :: fdda3d
real      ,DIMENSION(grid%sm31:grid%em31,1:1,grid%sm33:grid%em33,num_fdda2d)           :: fdda2d
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33,num_advh_t)           :: advh_t
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33,num_advz_t)           :: advz_t
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33,num_nba_mij)           :: nba_mij
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33,num_nba_rij)           :: nba_rij
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33,num_chem)           :: chem
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33,num_tracer)           :: tracer
real      ,DIMENSION(grid%sm33:grid%em33,grid%sm32:grid%em32,grid%spec_bdy_width,num_tracer)           :: tracer_bxs
real      ,DIMENSION(grid%sm33:grid%em33,grid%sm32:grid%em32,grid%spec_bdy_width,num_tracer)           :: tracer_bxe
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%spec_bdy_width,num_tracer)           :: tracer_bys
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%spec_bdy_width,num_tracer)           :: tracer_bye
real      ,DIMENSION(grid%sm33:grid%em33,grid%sm32:grid%em32,grid%spec_bdy_width,num_tracer)           :: tracer_btxs
real      ,DIMENSION(grid%sm33:grid%em33,grid%sm32:grid%em32,grid%spec_bdy_width,num_tracer)           :: tracer_btxe
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%spec_bdy_width,num_tracer)           :: tracer_btys
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%spec_bdy_width,num_tracer)           :: tracer_btye


      REAL :: p_surf ,  pd_surf, p_surf_int , pb_int , ht_hold
      REAL :: qvf , qvf1 , qvf2, qtot
      REAL :: pfu, pfd, phm

      

      INTEGER :: n_moist

      INTEGER                             ::                       &
                                     ids, ide, jds, jde, kds, kde, &
                                     ims, ime, jms, jme, kms, kme, &
                                     its, ite, jts, jte, kts, kte, &
                                     ips, ipe, jps, jpe, kps, kpe, &
                                     i, j, k, ispe, ktf

      SELECT CASE ( model_data_order )
         CASE ( DATA_ORDER_ZXY )
            kds = grid%sd31 ; kde = grid%ed31 ;
            ids = grid%sd32 ; ide = grid%ed32 ;
            jds = grid%sd33 ; jde = grid%ed33 ;

            kms = grid%sm31 ; kme = grid%em31 ;
            ims = grid%sm32 ; ime = grid%em32 ;
            jms = grid%sm33 ; jme = grid%em33 ;

            kts = grid%sp31 ; kte = grid%ep31 ;   
            its = grid%sp32 ; ite = grid%ep32 ;   
            jts = grid%sp33 ; jte = grid%ep33 ;   

         CASE ( DATA_ORDER_XYZ )
            ids = grid%sd31 ; ide = grid%ed31 ;
            jds = grid%sd32 ; jde = grid%ed32 ;
            kds = grid%sd33 ; kde = grid%ed33 ;

            ims = grid%sm31 ; ime = grid%em31 ;
            jms = grid%sm32 ; jme = grid%em32 ;
            kms = grid%sm33 ; kme = grid%em33 ;

            its = grid%sp31 ; ite = grid%ep31 ;   
            jts = grid%sp32 ; jte = grid%ep32 ;   
            kts = grid%sp33 ; kte = grid%ep33 ;   

         CASE ( DATA_ORDER_XZY )
            ids = grid%sd31 ; ide = grid%ed31 ;
            kds = grid%sd32 ; kde = grid%ed32 ;
            jds = grid%sd33 ; jde = grid%ed33 ;

            ims = grid%sm31 ; ime = grid%em31 ;
            kms = grid%sm32 ; kme = grid%em32 ;
            jms = grid%sm33 ; jme = grid%em33 ;

            its = grid%sp31 ; ite = grid%ep31 ;   
            kts = grid%sp32 ; kte = grid%ep32 ;   
            jts = grid%sp33 ; jte = grid%ep33 ;   

      END SELECT

            ktf=MIN(kte,kde-1)

      DO j=jts,jte
         DO i=its,ite
            grid%ph_2(i,1,j) = 0.
         END DO
      END DO

      
      
      

         n_moist = num_moist-1



       DO j = jts, MIN(jte,jde-1)
         DO i = its, MIN(ite,ide-1)

               
               
               
               k = kte-1

               qtot = 0.
               DO ispe=PARAM_FIRST_SCALAR,n_moist
                 qtot = qtot + 0.5*(moist(i,k,j,ispe)+moist(i,k,j,ispe))
               ENDDO
               qvf2 = 1./(1.+qtot)
               qvf1 = qtot*qvf2

               grid%p(i,k,j) = - 0.5*(grid%mu_2(i,j)+qvf1*grid%mub(i,j))/grid%rdnw(k)/qvf2
               qvf = 1.+rvovrd*moist(i,k,j,P_QV)
               grid%alt(i,k,j) = (r_d/p1000mb)*(grid%t_2(i,k,j)+t0)*qvf*         &
                      (((grid%p(i,k,j)+grid%pb(i,k,j))/p1000mb)**cvpm)
               grid%al(i,k,j) = grid%alt(i,k,j) - grid%alb(i,k,j)
               grid%p_hyd(i,k,j) = grid%p(i,k,j) + grid%pb(i,k,j)

               
               

           DO k=kte-2,1,-1
               qtot = 0.
               DO ispe=PARAM_FIRST_SCALAR,n_moist
               qtot = qtot + 0.5*(  moist(i,k  ,j,ispe) + moist(i,k+1,j,ispe) )
               ENDDO
               qvf2 = 1./(1.+qtot)
               qvf1 = qtot*qvf2
               grid%p(i,k,j) = grid%p(i,k+1,j) - (grid%mu_2(i,j) +       &
                               qvf1*grid%mub(i,j))/qvf2/grid%rdn(k+1)
               qvf = 1. + rvovrd*moist(i,k,j,P_QV)
               grid%alt(i,k,j) = (r_d/p1000mb)*(grid%t_2(i,k,j)+t0)*qvf* &
                           (((grid%p(i,k,j)+grid%pb(i,k,j))/p1000mb)**cvpm)
               grid%al(i,k,j) = grid%alt(i,k,j) - grid%alb(i,k,j)
               grid%p_hyd(i,k,j) = grid%p(i,k,j) + grid%pb(i,k,j)
           ENDDO

               
               

               IF (grid%hypsometric_opt == 1) THEN
                  DO k  = 2,kte
                     grid%ph_2(i,k,j) = grid%ph_2(i,k-1,j) - &
                                   grid%dnw(k-1) * ( (grid%mub(i,j)+grid%mu_2(i,j))*grid%al(i,k-1,j) &
                                 + grid%mu_2(i,j)*grid%alb(i,k-1,j) )
                     grid%ph0(i,k,j) = grid%ph_2(i,k,j) + grid%phb(i,k,j)
                  END DO
               ELSE IF (grid%hypsometric_opt == 2) THEN
                
                
                
                
                

                  grid%ph_2(i,1,j) = grid%phb(i,1,j)
                  DO k = 2,kte
                     pfu = ( grid%mub(i,j)+grid%mu_2(i,j))*grid%znw(k)   + grid%p_top
                     pfd = ( grid%mub(i,j)+grid%mu_2(i,j))*grid%znw(k-1) + grid%p_top
                     phm = ( grid%mub(i,j)+grid%mu_2(i,j))*grid%znu(k-1) + grid%p_top
                     grid%ph_2(i,k,j) = grid%ph_2(i,k-1,j) + grid%alt(i,k-1,j)*phm*LOG(pfd/pfu)
                  END DO

                  DO k = 1,kte
                     grid%ph_2(i,k,j) = grid%ph_2(i,k,j) - grid%phb(i,k,j)
                  END DO

               END IF

         ENDDO  
        ENDDO  

      ips = its ; ipe = ite ; jps = jts ; jpe = jte ; kps = kts ; kpe = kte






CALL HALO_EM_INIT_1_sub ( grid, &
  local_communicator, &
  mytask, ntasks, ntasks_x, ntasks_y, &
  ids, ide, jds, jde, kds, kde,       &
  ims, ime, jms, jme, kms, kme,       &
  ips, ipe, jps, jpe, kps, kpe )







CALL HALO_EM_INIT_2_sub ( grid, &
  local_communicator, &
  mytask, ntasks, ntasks_x, ntasks_y, &
  ids, ide, jds, jde, kds, kde,       &
  ims, ime, jms, jme, kms, kme,       &
  ips, ipe, jps, jpe, kps, kpe )







CALL HALO_EM_INIT_3_sub ( grid, &
  local_communicator, &
  mytask, ntasks, ntasks_x, ntasks_y, &
  ids, ide, jds, jde, kds, kde,       &
  ims, ime, jms, jme, kms, kme,       &
  ips, ipe, jps, jpe, kps, kpe )







CALL HALO_EM_INIT_4_sub ( grid, &
  local_communicator, &
  mytask, ntasks, ntasks_x, ntasks_y, &
  ids, ide, jds, jde, kds, kde,       &
  ims, ime, jms, jme, kms, kme,       &
  ips, ipe, jps, jpe, kps, kpe )







CALL HALO_EM_INIT_5_sub ( grid, &
  num_moist, &
  moist, &
  num_chem, &
  chem, &
  num_scalar, &
  scalar, &
  num_tracer, &
  tracer, &
  local_communicator, &
  mytask, ntasks, ntasks_x, ntasks_y, &
  ids, ide, jds, jde, kds, kde,       &
  ims, ime, jms, jme, kms, kme,       &
  ips, ipe, jps, jpe, kps, kpe )

   END SUBROUTINE rebalance_dfi


