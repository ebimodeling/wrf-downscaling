















 MODULE module_lightning_driver
 CONTAINS









 SUBROUTINE lightning_init ( &
                              itimestep, restart, dt, dx               &
                            
                             ,cu_physics,mp_physics,do_radar_ref       &
                             ,lightning_option, lightning_dt           &
                             ,lightning_start_seconds                  &
                             ,iccg_prescribed_num, iccg_prescribed_den &
                             ,cellcount_method                         &
                            
                             ,ids, ide, jds, jde, kds, kde             &
                             ,ims, ime, jms, jme, kms, kme             &
                             ,its, ite, jts, jte, kts, kte             &
                            
                             ,ic_flashcount, ic_flashrate              &
                             ,cg_flashcount, cg_flashrate              &





                            )

 USE module_state_description
 USE module_wrf_error
 IMPLICIT NONE


 INTEGER,  INTENT(IN)        :: itimestep
 LOGICAL,  INTENT(IN)        :: restart
 REAL,     INTENT(IN)        :: dt,dx
 INTEGER,  INTENT(IN)        :: cu_physics,mp_physics,do_radar_ref,lightning_option
 REAL,     INTENT(IN)        :: lightning_dt, lightning_start_seconds
 REAL,     INTENT(IN)        :: iccg_prescribed_num, iccg_prescribed_den
 INTEGER,  INTENT(INOUT)     :: cellcount_method
 INTEGER , INTENT(IN)        :: ids, ide, jds, jde, kds, kde,  &
                                ims, ime, jms, jme, kms, kme,  &
                                its, ite, jts, jte, kts, kte


 REAL, OPTIONAL, DIMENSION( ims:ime,jms:jme ), &
                 INTENT(OUT) :: ic_flashcount, ic_flashrate, &
                                cg_flashcount, cg_flashrate








 CHARACTER (LEN=80) :: message




 IF (itimestep .gt. 0 .or. lightning_option .eq. 0) return


 IF ( MOD(lightning_dt,dt) .ne. 0. ) THEN
    CALL wrf_error_fatal3("<stdin>",87,&
' lightning_init: lightning_dt needs to be a multiple of model time step dt')
 ENDIF


 IF (iccg_prescribed_den .eq. 0. .and. iccg_prescribed_num .eq. 0.) THEN
    CALL wrf_error_fatal3("<stdin>",93,&
' lightning_init: iccg_prescribed cannot be 0.0/0.0')
 ENDIF
 IF (iccg_prescribed_den .ne. 0.) THEN
    IF (iccg_prescribed_num/iccg_prescribed_den .eq. -1.) THEN
        CALL wrf_error_fatal3("<stdin>",98,&
' lightning_init: iccg_prescribed cannot be -1')
    ENDIF
 ENDIF






 ltng_select: SELECT CASE(lightning_option)

    
    CASE (ltng_crm_PR92w,ltng_crm_PR92z)
        IF ( do_radar_ref .eq. 0 .or. mp_physics .eq. 0) THEN
          CALL wrf_error_fatal3("<stdin>",113,&
' lightning_init: Selected lightning option requires microphysics and do_radar_ref=1' )
        ENDIF

        WRITE(message, * ) ' lightning_init: CRM lightning option used: ', lightning_option
        CALL wrf_debug ( 100 , message )

    
    CASE (ltng_cpm_PR92z)
        IF ( cu_physics .ne. GDSCHEME .and. cu_physics .ne. G3SCHEME  ) THEN
          CALL wrf_error_fatal3("<stdin>",123,&
' lightning_init: Selected lightning option requires GD or G3 convective parameterization' )
        ENDIF

        WRITE(message, * ) ' lightning_init: CPM lightning option selected: ', lightning_option
        CALL wrf_debug ( 100 , message )

    
    CASE DEFAULT
        CALL wrf_error_fatal3("<stdin>",132,&
' lightning_init: invalid lightning_option')
 END SELECT ltng_select


 IF (restart) return


 IF ( PRESENT( ic_flashcount ) .and. PRESENT( ic_flashrate ) .and. &
      PRESENT( cg_flashcount ) .and. PRESENT( cg_flashrate ) ) THEN
    CALL wrf_debug ( 100 , ' lightning_init: flash initializing lightning flash arrays' )

    ic_flashrate(:,:)  = 0.
    ic_flashcount(:,:) = 0.
    cg_flashrate(:,:)  = 0.
    cg_flashcount(:,:) = 0.
 ELSE
    CALL wrf_error_fatal3("<stdin>",149,&
' lightning_init: flash arrays not present' )
 ENDIF


 IF ( ( cellcount_method .eq. 0 ) .and. (lightning_option .eq. ltng_crm_PR92w )) THEN
   IF ( (ime-ims+1)*dx .gt. 1E4 ) THEN 
     cellcount_method = 1
     WRITE(message, * ) ' lightning_init: setting auto cellcount_method to patch (cellcount_method=1'
   ELSE
     cellcount_method = 2
     WRITE(message, * ) ' lightning_init: setting auto cellcount_method to domain (cellcount_method=2'
   ENDIF
   CALL wrf_debug( 100, message )
 ENDIF


 CALL wrf_debug( 200, ' lightning_init: finishing')

 END SUBROUTINE lightning_init










 SUBROUTINE lightning_driver ( &
                          
                            itimestep, dt, dx, dy,                &
                            xlat, xlon, xland, ht,                &
                            t_phy, p_phy, rho, u, v, w,           &
                            z, moist,                             &
                          
                            ktop_deep,                            &
                            refl,                                 &
                            current_time,                         &
                          
                            lightning_option,                     &
                            lightning_dt,                         &
                            lightning_start_seconds,              &
                            flashrate_factor,                     &
                          
                            iccg_method,                          &
                            iccg_prescribed_num,                  &
                            iccg_prescribed_den,                  &
                          
                            iccg_in_num, iccg_in_den,             &
                          
                            cellcount_method,                     &
                            cldtop_adjustment,                    &
                          
                            ids, ide, jds, jde, kds, kde,         &
                            ims, ime, jms, jme, kms, kme,         &
                            its, ite, jts, jte, kts, kte,         &
                          
                            ic_flashcount, ic_flashrate,          &
                            cg_flashcount, cg_flashrate           &
                          )


 USE module_state_description
 USE module_utility


 USE module_model_constants
 USE module_wrf_error


 USE module_ltng_crmpr92       
                               
 USE module_ltng_cpmpr92z      


 USE module_ltng_iccg

 IMPLICIT NONE



 INTEGER, INTENT(IN   )    ::       itimestep
 REAL,    INTENT(IN   )    ::       dt, dx, dy

 REAL,    DIMENSION( ims:ime,          jms:jme ),           INTENT(IN   ) :: xlat, xlon, xland, ht
 REAL,    DIMENSION( ims:ime, kms:kme, jms:jme ),           INTENT(IN   ) :: t_phy, p_phy, rho
 REAL,    DIMENSION( ims:ime, kms:kme, jms:jme ),           INTENT(IN   ) :: u, v, w, z
 REAL,    DIMENSION( ims:ime, kms:kme, jms:jme, num_moist), INTENT(IN   ) :: moist


 INTEGER, DIMENSION( ims:ime,          jms:jme ),           INTENT(IN   ) :: ktop_deep     
 REAL,    DIMENSION( ims:ime, kms:kme, jms:jme ),           INTENT(IN   ) :: refl          
 TYPE(WRFU_Time),                                           INTENT(IN   ) :: current_time  


 INTEGER, INTENT(IN   )    ::       lightning_option
 REAL,    INTENT(IN   )    ::       lightning_dt, lightning_start_seconds, flashrate_factor


 INTEGER, INTENT(IN   )    ::       iccg_method
 REAL,    INTENT(IN   )    ::       iccg_prescribed_num, iccg_prescribed_den
 REAL,    DIMENSION( ims:ime, jms:jme, 12), INTENT(IN   ) :: iccg_in_num, iccg_in_den


 INTEGER, INTENT(IN   )    ::       cellcount_method                    
 REAL,    INTENT(IN   )    ::       cldtop_adjustment                   


 INTEGER, INTENT(IN   )    ::       ids,ide, jds,jde, kds,kde
 INTEGER, INTENT(IN   )    ::       ims,ime, jms,jme, kms,kme
 INTEGER, INTENT(IN   )    ::       its,ite, jts,jte, kts,kte


 REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT) :: ic_flashcount , cg_flashcount
 REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(  OUT) :: ic_flashrate  , cg_flashrate


 REAL, DIMENSION( ims:ime, jms:jme ) :: total_flashrate
 CHARACTER (LEN=80) :: message

 REAL, PARAMETER            :: reflthreshold = 20. 
 REAL, DIMENSION( kms:kme ) :: cellcount



 IF ( lightning_option .eq. 0 ) RETURN

 IF ( itimestep * dt .lt. lightning_start_seconds ) RETURN

 IF ( MOD((itimestep * dt - lightning_start_seconds), lightning_dt ) .ne. 0 ) RETURN












 IF ( lightning_option .eq. ltng_crm_PR92w .or. &
      lightning_option .eq. ltng_crm_PR92z ) THEN
   CALL wrf_debug ( 100, ' lightning_driver: determining cloud extents for CRM' )
   CALL countCells( &
          
            refl, reflthreshold, cellcount_method,     &
          
            ids, ide, jds, jde, kds, kde,              &
            ims, ime, jms, jme, kms, kme,              &
            its, ite, jts, jte, kts, kte,              &
          
            cellcount )
   WRITE(message, * ) ' lightning_driver: Max cell count = ', maxval(cellcount)
   CALL wrf_debug ( 100, message )
 ENDIF



 CALL wrf_debug ( 100, ' lightning_driver: calculating flash rate' )
 flashrate_select: SELECT CASE(lightning_option)

    
    CASE( ltng_crm_PR92w )
        CALL wrf_debug ( 100, ' lightning_driver: calling Price and Rind 1992 (w_max, CRM)' )

        CALL ltng_crmpr92w ( &
                  
                    dx, dy, xland, ht, z, t_phy,          &
                  
                    w, refl, reflthreshold, cellcount,    &
                  
                    cellcount_method,                     &
                  
                    ids, ide, jds, jde, kds, kde,         &
                    ims, ime, jms, jme, kms, kme,         &
                    its, ite, jts, jte, kts, kte,         &
                  
                    total_flashrate                       &
                  )
    CASE( ltng_crm_PR92z )
        CALL wrf_debug ( 100, ' lightning_driver: calling Price and Rind 1992 (z_top, CRM)' )
        CALL ltng_crmpr92z ( &
                  
                    dx, dy, xland, ht, z, t_phy,          &
                  
                    refl, reflthreshold, cellcount,       &
                  
                    cellcount_method,                     &
                  
                    ids, ide, jds, jde, kds, kde,         &
                    ims, ime, jms, jme, kms, kme,         &
                    its, ite, jts, jte, kts, kte,         &
                  
                    total_flashrate                       &
                  )



    
    CASE( ltng_cpm_PR92z )
        CALL wrf_debug ( 100, ' lightning_driver: calling Price and Rind 1992 (z_top, CPM)' )

        CALL ltng_cpmpr92z ( &
                  
                    dx, dy, xland, ht, z, t_phy,      &
                    ktop_deep, cldtop_adjustment,     &
                  
                    ids, ide, jds, jde, kds, kde,     &
                    ims, ime, jms, jme, kms, kme,     &
                    its, ite, jts, jte, kts, kte,     &
                  
                    total_flashrate                   &
                  )




    
    CASE DEFAULT
        WRITE(wrf_err_message, * ) ' lightning_driver: The lightning option does not exist: lightning_opt = ', lightning_option
        CALL wrf_error_fatal3("<stdin>",373,&
wrf_err_message )

 END SELECT flashrate_select



 CALL wrf_debug ( 100, ' lightning_driver: partitioning IC:CG')
 iccg_select: SELECT CASE(iccg_method)
    
    CASE( 0 ) iccg_select
        CALL wrf_debug( 100, ' lightning_driver: using option-default IC:CG method' )
        iccg_method_default: SELECT CASE(lightning_option)

            CASE( ltng_crm_PR92w, ltng_crm_PR92z, ltng_cpm_PR92z ) iccg_method_default
                CALL iccg_boccippio( &
                            xlat, xlon,                                &
                            iccg_prescribed_num, iccg_prescribed_den,  &
                          
                            ids, ide, jds, jde, kds, kde,              &
                            ims, ime, jms, jme, kms, kme,              &
                            its, ite, jts, jte, kts, kte,              &
                          
                            total_flashrate,                           &
                          
                            ic_flashrate, cg_flashrate                 &
                          )

            CASE DEFAULT iccg_method_default
                CALL wrf_debug ( 100, ' lightning_driver: no method-default IC:CG implemented, using user-prescribed constant')
                CALL iccg_user_prescribed( &
                            iccg_prescribed_num,                  &
                            iccg_prescribed_den,                  &
                          
                            ids, ide, jds, jde, kds, kde,         &
                            ims, ime, jms, jme, kms, kme,         &
                            its, ite, jts, jte, kts, kte,         &
                          
                            total_flashrate,                      &
                          
                            ic_flashrate, cg_flashrate            &
                          )

        END SELECT iccg_method_default

    
    CASE( 1 ) iccg_select
        WRITE(message, * ) ' lightning_driver: using user-prescribed IC:CG ratio = ', iccg_prescribed_num, iccg_prescribed_den
        CALL wrf_debug ( 100, message )
        CALL iccg_user_prescribed( &
                    iccg_prescribed_num,                  &
                    iccg_prescribed_den,                  &
                  
                    ids, ide, jds, jde, kds, kde,         &
                    ims, ime, jms, jme, kms, kme,         &
                    its, ite, jts, jte, kts, kte,         &
                  
                    total_flashrate,                      &
                  
                    ic_flashrate, cg_flashrate            &
                  )

    
    CASE( 2 ) iccg_select
        CALL wrf_debug ( 100, ' lightning_driver: using Boccippio 2001 IC:CG climatology')
        CALL iccg_boccippio( &
                    xlat, xlon,                                &
                    iccg_prescribed_num, iccg_prescribed_den,  &
                  
                    ids, ide, jds, jde, kds, kde,              &
                    ims, ime, jms, jme, kms, kme,              &
                    its, ite, jts, jte, kts, kte,              &
                  
                    total_flashrate,                           &
                  
                    ic_flashrate, cg_flashrate                 &
                  )

    
    CASE( 3 ) iccg_select
        iccg_pr93_select: SELECT CASE(lightning_option)
        CASE( ltng_crm_PR92w, ltng_crm_PR92z ) iccg_pr93_select
            CALL wrf_debug ( 100, ' lightning_driver: using Price and Rind 1993 IC:CG ratio (CRM)')
            CALL iccg_crm_pr93( &
                    refl, reflthreshold, t_phy, z,             &
                  
                    ids, ide, jds, jde, kds, kde,              &
                    ims, ime, jms, jme, kms, kme,              &
                    its, ite, jts, jte, kts, kte,              &
                  
                    total_flashrate,                           &
                  
                    ic_flashrate, cg_flashrate                 &
                )

        CASE DEFAULT iccg_pr93_select
            CALL wrf_debug ( 100, ' lightning_driver: using Price and Rind 1993 IC:CG ratio (CPM)')
            CALL iccg_pr93( &
                    ktop_deep, cldtop_adjustment, t_phy, z,    &
                  
                    ids, ide, jds, jde, kds, kde,              &
                    ims, ime, jms, jme, kms, kme,              &
                    its, ite, jts, jte, kts, kte,              &
                  
                    total_flashrate,                           &
                  
                    ic_flashrate, cg_flashrate                 &
                )
        END SELECT iccg_pr93_select

    CASE( 4 ) iccg_select
        CALL wrf_debug ( 100, ' lightning_driver: using input IC:CG ratio from iccg_in_(num|den)' )
        CALL iccg_input( &
                    iccg_prescribed_num, iccg_prescribed_den,  &
                    iccg_in_num, iccg_in_den, current_time,    &
                  
                    ids, ide, jds, jde, kds, kde,              &
                    ims, ime, jms, jme, kms, kme,              &
                    its, ite, jts, jte, kts, kte,              &
                  
                    total_flashrate,                           &
                  
                    ic_flashrate, cg_flashrate                 &
                  )

    
    CASE DEFAULT iccg_select
        WRITE(wrf_err_message, * ) ' lightning_driver: Invalid IC:CG method (iccg_method) = ', lightning_option
        CALL wrf_error_fatal3("<stdin>",501,&
wrf_err_message )

 END SELECT iccg_select



 CALL wrf_debug( 200, ' lightning_driver: converting flash rates to flash counts')

 ic_flashrate(its:ite,jts:jte) = ic_flashrate(its:ite,jts:jte) * flashrate_factor
 cg_flashrate(its:ite,jts:jte) = cg_flashrate(its:ite,jts:jte) * flashrate_factor

 ic_flashcount(its:ite,jts:jte) = ic_flashcount(its:ite,jts:jte) + ic_flashrate(its:ite,jts:jte) * lightning_dt
 cg_flashcount(its:ite,jts:jte) = cg_flashcount(its:ite,jts:jte) + cg_flashrate(its:ite,jts:jte) * lightning_dt



 CALL wrf_debug ( 100, ' lightning_driver: returning from')

 END SUBROUTINE lightning_driver















 SUBROUTINE countCells( &
          
            refl, reflthreshold, cellcount_method,     &
          
            ids, ide, jds, jde, kds, kde,              &
            ims, ime, jms, jme, kms, kme,              &
            its, ite, jts, jte, kts, kte,              &
          
            cellcount )

 USE module_dm, only: wrf_dm_sum_real

 IMPLICIT NONE



 REAL,    DIMENSION( ims:ime,kms:kme,jms:jme ), INTENT(IN   ) :: refl
 REAL,    INTENT(IN   ) :: reflthreshold
 INTEGER, INTENT(IN   ) :: cellcount_method


 INTEGER, INTENT(IN   )    ::       ids,ide, jds,jde, kds,kde
 INTEGER, INTENT(IN   )    ::       ims,ime, jms,jme, kms,kme
 INTEGER, INTENT(IN   )    ::       its,ite, jts,jte, kts,kte



 REAL,    DIMENSION( kms:kme ), INTENT(  OUT) :: cellcount


 INTEGER :: i,k,j



 cellcount(kts:kte) = 0.
 DO j=jts,jte
   DO k=kts,kte
     DO i=its,ite
       IF ( refl(i,k,j) .gt. reflthreshold ) THEN
         cellcount(k) = cellcount(k) + 1
       ENDIF
     ENDDO
   ENDDO
 ENDDO

 IF ( cellcount_method .eq. 2 ) THEN
   DO k=kts,kte
     cellcount(k) = wrf_dm_sum_real(cellcount(k))
   ENDDO
 ENDIF

 END SUBROUTINE

 END MODULE module_lightning_driver
