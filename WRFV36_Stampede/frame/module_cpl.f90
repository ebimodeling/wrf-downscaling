MODULE module_cpl

   USE module_domain          , ONLY : domain, get_ijk_from_grid
   USE module_configure       , ONLY : grid_config_rec_type
   USE module_model_constants , ONLY : stbolt
   USE module_driver_constants, ONLY : max_domains, max_cplfld, max_extdomains
   USE module_cpl_oasis3 

   IMPLICIT NONE
   PRIVATE

   PUBLIC cpl_init
   PUBLIC cpl_set_dm_communicator
   PUBLIC cpl_defdomain
   PUBLIC cpl_settime
   PUBLIC cpl_snd
   PUBLIC cpl_rcv
   PUBLIC cpl_store_input
   PUBLIC cpl_finalize
   PUBLIC cpl_abort





   LOGICAL     , PARAMETER, PUBLIC :: coupler_on = .FALSE.
   CHARACTER(4), PARAMETER         :: coupler_name = 'none'

   INTEGER :: nsecrun             
   INTEGER, PARAMETER :: charlen = 64
   CHARACTER(charlen), DIMENSION(max_domains,max_extdomains,max_cplfld) :: rcvname, sndname   

   CHARACTER(256) :: cltxt        
   INTEGER :: nlevdbg  = 1        
   INTEGER :: nlevdbg2 = 10       


   INCLUDE 'mpif.h'               




CONTAINS

   SUBROUTINE cpl_init( kl_comm ) 
      
      
      
      
      
      INTEGER, INTENT(OUT) :: kl_comm       
      
      INTEGER       :: jwrf,jext,jfld       
      CHARACTER( 3) :: clwrfdom, clextdom   
      CHARACTER(16) :: clprefix             
      

      
      rcvname(:,:,:) = 'not defined'
      sndname(:,:,:) = 'not defined'
      
      
      
      DO jext = 1, max_extdomains
         
         WRITE(clextdom, fmt="('d',i2.2)") jext
         
         DO jwrf = 1, max_domains
            
            WRITE(clwrfdom, fmt="('d',i2.2)") jwrf          
            
            clprefix = 'WRF_'//clwrfdom//'_EXT_'//clextdom//'_' 
            
            
            rcvname(jwrf,jext,1) = clprefix//'SST'                  
            rcvname(jwrf,jext,2) = clprefix//'UOCE'                 
            rcvname(jwrf,jext,3) = clprefix//'VOCE'                 
            
            
            sndname(jwrf,jext,1) = clprefix//'EVAP-PRECIP'          
            sndname(jwrf,jext,2) = clprefix//'SURF_NET_SOLAR'       
            sndname(jwrf,jext,3) = clprefix//'SURF_NET_NON-SOLAR'   
            sndname(jwrf,jext,4) = clprefix//'TAUX'                 
            sndname(jwrf,jext,5) = clprefix//'TAUY'                 
            sndname(jwrf,jext,6) = clprefix//'TAUMOD'               
            
         END DO
      END DO
      
      IF ( coupler_name == 'oasis' ) CALL cpl_oasis_init( kl_comm ) 
      
   END SUBROUTINE cpl_init
   

   SUBROUTINE cpl_set_dm_communicator( kdm_comm )
      
      
      
      
      
      INTEGER, INTENT(IN) :: kdm_comm       
      

      IF ( coupler_name == 'oasis' ) THEN 
         IF ( kdm_comm == MPI_COMM_NULL ) THEN
            CALL cpl_oasis_define( sndname, rcvname )   
         ELSE
            CALL cpl_oasis_def_dmcomm( kdm_comm )       
         END IF
      END IF

   END SUBROUTINE cpl_set_dm_communicator


   SUBROUTINE cpl_defdomain( grid )
      
      
      
      
      
      TYPE(domain), INTENT(IN), POINTER ::   grid
      
      INTEGER :: jwrf,jext,jfld          
      REAL    :: zmin,zmax               
      INTEGER :: ips,ipe,jps,jpe,kps,kpe 
      INTEGER :: ims,ime,jms,jme,kms,kme 
      INTEGER :: ids,ide,jds,jde,kds,kde 
      


      CALL get_ijk_from_grid( grid, ids, ide, jds, jde, kds, kde, &
         &                          ims, ime, jms, jme, kms, kme, &
         &                          ips, ipe, jps, jpe, kps, kpe  )

      
      

      
      CALL wrf_debug(nlevdbg, 'cpl_init: defined variables to be potentially received' )
      DO jfld = 1, max_cplfld
         DO jext = 1, grid%num_ext_model_couple_dom
            DO jwrf = 1, grid%max_dom
               IF( TRIM(sndname(jwrf,jext,jfld)) /= 'not defined' ) THEN
                  WRITE(cltxt,*) '   jwrf, jext, jfld: ', jwrf, jext, jfld ,' name: ', TRIM(sndname(jwrf,jext,jfld))
                  CALL wrf_debug(nlevdbg2, cltxt)
               END IF
            END DO
         END DO
      END DO
      CALL wrf_debug(nlevdbg, 'cpl_init: defined variables to be potentially sent' )
      DO jfld = 1, max_cplfld
         DO jext = 1, grid%num_ext_model_couple_dom
            DO jwrf = 1, grid%max_dom
               IF( TRIM(rcvname(jwrf,jext,jfld)) /= 'not defined' ) THEN
                  WRITE(cltxt,*) '   jwrf, jext, jfld: ', jwrf, jext, jfld ,' name: ', TRIM(rcvname(jwrf,jext,jfld))
                  CALL wrf_debug(nlevdbg2, cltxt)
               END IF
            END DO
         END DO
      END DO
      
      
      DO jext = 1, grid%num_ext_model_couple_dom

         WRITE(cltxt,*) 'checks on cplmask of external model domain: ', jext               ;   CALL wrf_debug(nlevdbg, cltxt)

         zmin = MINVAL(grid%cplmask(ips:ipe,jext,jps:jpe))
         IF( zmin < 0. ) THEN
            WRITE(cltxt,*) 'min of external model domain cplmask: ',jext,' < 0. : ',zmin   ;   CALL cpl_abort('cpl_defdomain',cltxt)
         END IF
         WRITE(cltxt,*) '   minval(grid%cplmask(ips:ipe,jext,jps:jpe)): ', zmin            ;   CALL wrf_debug(nlevdbg, cltxt)

         zmax = MAXVAL(grid%cplmask(ips:ipe,jext,jps:jpe))
         IF( zmax > 1. ) THEN
            WRITE(cltxt,*) 'max of external model domain cplmask: ',jext,' > 1. : ',zmax   ;   CALL cpl_abort('cpl_defdomain',cltxt)
         END IF
         IF( zmax == 0. ) THEN
            WRITE(cltxt,*) 'max of external model domain cplmask: ',jext,' = 0 '           ;   CALL wrf_message(cltxt)
            WRITE(cltxt,*) '  => no coupling between this external model domain and this WRF patch'   ;   CALL wrf_message(cltxt)
         END IF
         WRITE(cltxt,*) '   maxval(grid%cplmask(ips:ipe,jext,jps:jpe)): ', zmax            ;   CALL wrf_debug(nlevdbg, cltxt)

      END DO

      
      IF ( coupler_name == 'oasis' ) CALL cpl_oasis_define( sndname, rcvname, grid )

   END SUBROUTINE cpl_defdomain


   SUBROUTINE cpl_settime( psec )
      
      
      
      
      
      REAL, INTENT(in) :: psec
      

      nsecrun = NINT( psec )
      WRITE(cltxt,*) 'store number of second since the beginning of the job: ', nsecrun   ;   CALL wrf_debug(nlevdbg2, cltxt)

   END SUBROUTINE cpl_settime


   FUNCTION cpl_toreceive( kdomwrf, kdomext, kfldid )
      
      
      
      
      
      INTEGER, INTENT(IN) :: kdomwrf   
      INTEGER, INTENT(IN) :: kdomext   
      INTEGER, INTENT(IN) :: kfldid    
      
      LOGICAL :: cpl_toreceive
      

      IF ( coupler_name == 'oasis' ) cpl_toreceive = cpl_oasis_toreceive( kdomwrf, kdomext, kfldid ) 

   END FUNCTION cpl_toreceive


   FUNCTION cpl_tosend( kdomwrf, kfldid, max_edom )
      
      
      
      
      
      
      INTEGER, INTENT(IN) :: kdomwrf   
      INTEGER, INTENT(IN) :: kfldid    
      INTEGER, INTENT(IN) :: max_edom  
      
      LOGICAL,DIMENSION(max_edom) :: cpl_tosend
      INTEGER                     :: jext          
      

      DO jext = 1, max_edom
         IF ( coupler_name == 'oasis' )   cpl_tosend(jext) = cpl_oasis_tosend( kdomwrf, jext, kfldid ) 
      END DO
      
   END FUNCTION cpl_tosend


   FUNCTION cpl_get_fldid( cdsuffix )
      
      
      
      
      
      CHARACTER(*), INTENT(IN) :: cdsuffix   
      
      INTEGER       :: cpl_get_fldid     
      INTEGER       :: jfld              
      CHARACTER(16) :: clprefix          
      
      cpl_get_fldid = -1   
         
      clprefix = 'WRF_d01_EXT_d01_' 
      DO jfld = 1, max_cplfld
         IF( clprefix//TRIM(cdsuffix) == TRIM(sndname(1,1,jfld)) )   cpl_get_fldid = jfld
         IF( clprefix//TRIM(cdsuffix) == TRIM(rcvname(1,1,jfld)) )   cpl_get_fldid = jfld
      END DO
          
      IF( cpl_get_fldid == -1 )   CALL cpl_abort( 'cpl_get_fldid', 'variable suffix not found '//TRIM(cdsuffix) )
      WRITE(cltxt,*) 'The id of variable'//TRIM(cdsuffix)//' is: ', cpl_get_fldid   ;   CALL wrf_debug(nlevdbg2, cltxt)

   END FUNCTION cpl_get_fldid

   
   SUBROUTINE cpl_snd( grid )
         
      
      
      
      
      TYPE(domain), INTENT(IN), POINTER :: grid
      
      INTEGER :: ips,ipe,jps,jpe,kps,kpe 
      INTEGER :: ims,ime,jms,jme,kms,kme 
      INTEGER :: ids,ide,jds,jde,kds,kde 
      
      CALL get_ijk_from_grid( grid, ids, ide, jds, jde, kds, kde, &
         &                          ims, ime, jms, jme, kms, kme, &
         &                          ips, ipe, jps, jpe, kps, kpe  )


      CALL cpl_snd2( grid, grid%num_ext_model_couple_dom,   &
         &                 ids, ide, jds, jde, kds, kde,    &
         &                 ims, ime, jms, jme, kms, kme,    &
         &                 ips, ipe, jps, jpe, kps, kpe )


   END SUBROUTINE cpl_snd


   SUBROUTINE cpl_snd2( grid, max_edom                &
      &                     , ids,ide,jds,jde,kds,kde &
      &                     , ims,ime,jms,jme,kms,kme &
      &                     , ips,ipe,jps,jpe,kps,kpe )
      
      
      
      
      
      TYPE(domain), INTENT(IN), POINTER :: grid
      INTEGER,      INTENT(IN)          :: max_edom    
      INTEGER,      INTENT(IN)          :: ids,ide,jds,jde,kds,kde
      INTEGER,      INTENT(IN)          :: ims,ime,jms,jme,kms,kme
      INTEGER,      INTENT(IN)          :: ips,ipe,jps,jpe,kps,kpe
      
      REAL, DIMENSION( ips:ipe, jps:jpe ) :: cplsnd
      REAL, DIMENSION( ips:ipe, jps:jpe ) :: u_uo
      REAL, DIMENSION( ips:ipe, jps:jpe ) :: v_vo
      REAL, DIMENSION( ips:ipe, jps:jpe ) :: wspd
      REAL, DIMENSION( ips:ipe, jps:jpe ) :: taut
      INTEGER :: icnt
      INTEGER :: ifldid
      LOGICAL,DIMENSION(max_edom) :: lltosend
      



      
      
      ifldid      = cpl_get_fldid( 'EVAP-PRECIP' )
      lltosend(:) = cpl_tosend( grid%id, ifldid, max_edom )
      IF ( COUNT(lltosend) > 0 ) THEN 
         cplsnd(ips:ipe,jps:jpe) = grid%QFX(ips:ipe,jps:jpe) &
            &                  - ( grid%RAINCV(ips:ipe,jps:jpe)+grid%RAINNCV(ips:ipe,jps:jpe) ) / grid%DT
         CALL cpl_sndfield( grid%id, lltosend, ifldid, cplsnd )
      END IF
      
      ifldid      = cpl_get_fldid( 'SURF_NET_SOLAR' )
      lltosend(:) = cpl_tosend( grid%id, ifldid, max_edom )
      IF ( COUNT(lltosend) > 0 ) THEN 
         CALL cpl_sndfield( grid%id, lltosend, ifldid, grid%GSW(ips:ipe,jps:jpe) )
      END IF
      
      ifldid      = cpl_get_fldid( 'SURF_NET_NON-SOLAR' )
      lltosend(:) = cpl_tosend( grid%id, ifldid, max_edom )
      IF ( COUNT(lltosend) > 0 ) THEN 
         cplsnd(ips:ipe,jps:jpe) = grid%GLW(ips:ipe,jps:jpe) &
            &                      - STBOLT * grid%EMISS(ips:ipe,jps:jpe) * grid%SST(ips:ipe,jps:jpe)**4 &
            &                      - grid%LH(ips:ipe,jps:jpe) - grid%HFX(ips:ipe,jps:jpe) 
         CALL cpl_sndfield( grid%id, lltosend, ifldid, cplsnd )
      END IF
      
      
      icnt   =        COUNT( cpl_tosend( grid%id, cpl_get_fldid( 'TAUMOD' ), max_edom ) )
      icnt   = icnt + COUNT( cpl_tosend( grid%id, cpl_get_fldid( 'TAUX'   ), max_edom ) )
      icnt   = icnt + count( cpl_tosend( grid%id, cpl_get_fldid( 'TAUY'   ), max_edom ) )
      IF ( icnt > 0 ) THEN 
         u_uo(ips:ipe,jps:jpe) = grid%u_phy(ips:ipe,kps,jps:jpe) - grid%uoce(ips:ipe,jps:jpe)
         v_vo(ips:ipe,jps:jpe) = grid%v_phy(ips:ipe,kps,jps:jpe) - grid%voce(ips:ipe,jps:jpe)
         wspd(ips:ipe,jps:jpe) = MAX( SQRT( u_uo(ips:ipe,jps:jpe)**2 + v_vo(ips:ipe,jps:jpe)**2 ), 1.e-7 )
         taut(ips:ipe,jps:jpe) = grid%rho(ips:ipe,kps,jps:jpe) * grid%ust(ips:ipe,jps:jpe)**2
      END IF
      
      ifldid      = cpl_get_fldid( 'TAUX' )
      lltosend(:) = cpl_tosend( grid%id, ifldid, max_edom )
      IF ( COUNT(lltosend) > 0 ) THEN 
         cplsnd(ips:ipe,jps:jpe) = taut(ips:ipe,jps:jpe) * u_uo(ips:ipe,jps:jpe) / wspd(ips:ipe,jps:jpe)
         CALL cpl_sndfield( grid%id, lltosend, ifldid, cplsnd )
      END IF
      
      ifldid      = cpl_get_fldid( 'TAUY' )
      lltosend(:) = cpl_tosend( grid%id, ifldid, max_edom )
      IF ( COUNT(lltosend) > 0 ) THEN 
         cplsnd(ips:ipe,jps:jpe) = taut(ips:ipe,jps:jpe) * v_vo(ips:ipe,jps:jpe) / wspd(ips:ipe,jps:jpe)
         CALL cpl_sndfield( grid%id, lltosend, ifldid, cplsnd )
      END IF
      
      ifldid      = cpl_get_fldid( 'TAUMOD' )
      lltosend(:) = cpl_tosend( grid%id, ifldid, max_edom )
      IF ( COUNT(lltosend) > 0 ) THEN 
         CALL cpl_sndfield( grid%id, lltosend, ifldid, taut )
      END IF
      

   END SUBROUTINE cpl_snd2


   SUBROUTINE cpl_sndfield( kdomwrf, ldtosend, kfldid, pdata )
      
      
      
      
      
      INTEGER,              INTENT(IN) :: kdomwrf   
      LOGICAL,DIMENSION(:), INTENT(IN) :: ldtosend
      INTEGER,              INTENT(IN) :: kfldid    
      REAL, DIMENSION(:,:), INTENT(IN) :: pdata     
      
      INTEGER :: jext          
      

      DO jext = 1, SIZE(ldtosend)
         IF( ldtosend(jext) ) THEN
            IF ( coupler_name == 'oasis' ) CALL cpl_oasis_snd( kdomwrf, jext, kfldid, nsecrun, pdata )
         END IF
      END DO

   END SUBROUTINE cpl_sndfield


   SUBROUTINE cpl_rcv( kdomwrf, cdsuffix,            &
      &                ids, ide, jds, jde, kds, kde, &
      &                ims, ime, jms, jme, kms, kme, &
      &                ips, ipe, jps, jpe, kps, kpe, &
      &                max_edom, pcplmask, pdatacpl, pdataobs )
      
      
      
      
      
      INTEGER,                                                   INTENT(IN   ) :: kdomwrf     
      CHARACTER(*),                                              INTENT(IN   ) :: cdsuffix    
      INTEGER,                                                   INTENT(IN   ) :: ids,ide,jds,jde,kds,kde
      INTEGER,                                                   INTENT(IN   ) :: ims,ime,jms,jme,kms,kme
      INTEGER,                                                   INTENT(IN   ) :: ips,ipe,jps,jpe,kps,kpe
      INTEGER,                                                   INTENT(IN   ) :: max_edom    
      REAL, DIMENSION( ims:ime, 1:max_edom, jms:jme ),           INTENT(IN   ) :: pcplmask    
      REAL, DIMENSION( ims:ime,             jms:jme ),           INTENT(  OUT) :: pdatacpl    
      REAL, DIMENSION( ims:ime,             jms:jme ), OPTIONAL, INTENT(IN   ) :: pdataobs    
      
      INTEGER :: jext                                
      INTEGER :: ifldid                              
      REAL, DIMENSION( ips:ipe, jps:jpe ) :: zdata   
      

      ifldid = cpl_get_fldid( cdsuffix )
         
      IF( PRESENT(pdataobs) ) THEN
         pdatacpl(ips:ipe,jps:jpe) = pdataobs(ips:ipe,jps:jpe) * ( 1.0 - SUM( pcplmask(ips:ipe,1:max_edom,jps:jpe), dim = 2 ) )
      ELSE 
         pdatacpl(ips:ipe,jps:jpe) = 0.0
      END IF

      DO jext = 1, max_edom
         IF( cpl_toreceive( kdomwrf, jext, ifldid ) ) THEN
            IF( coupler_name == 'oasis' )   CALL cpl_oasis_rcv( kdomwrf, jext, ifldid, nsecrun, zdata )
            pdatacpl(ips:ipe,jps:jpe) = pdatacpl(ips:ipe,jps:jpe) + zdata(ips:ipe,jps:jpe) * pcplmask(ips:ipe,jext,jps:jpe)
         END IF
      END DO

   END SUBROUTINE cpl_rcv


   SUBROUTINE cpl_store_input( grid, config_flags )
      
      
      
      
      
      TYPE(domain)                , INTENT(INOUT) :: grid
      TYPE (grid_config_rec_type) , INTENT(IN   ) :: config_flags
      
      INTEGER :: ips,ipe,jps,jpe,kps,kpe 
      INTEGER :: ims,ime,jms,jme,kms,kme 
      INTEGER :: ids,ide,jds,jde,kds,kde 
      LOGICAL :: llmust_store
      INTEGER :: jext          
      


      CALL get_ijk_from_grid( grid, ids, ide, jds, jde, kds, kde, &
         &                          ims, ime, jms, jme, kms, kme, &
         &                          ips, ipe, jps, jpe, kps, kpe  )
      
      
      
      
      
      
      IF( ( config_flags%auxinput4_interval .NE. 0 .AND. config_flags%io_form_auxinput4 .NE. 0 .AND. grid%just_read_auxinput4 ) &
         .OR. grid%itimestep .EQ. 1 ) THEN
         
         
         llmust_store = .FALSE.
         DO jext = 1, grid%num_ext_model_couple_dom
            llmust_store = llmust_store .OR. cpl_toreceive( grid%id, jext, cpl_get_fldid( 'SST' ) )
         END DO
         IF( llmust_store )   grid%sst_input(ips:ipe,jps:jpe) = grid%sst(ips:ipe,jps:jpe)   
         
         grid%just_read_auxinput4 = .FALSE.  
      
      END IF

 
   END SUBROUTINE cpl_store_input


   SUBROUTINE cpl_finalize()
      
      
      
      
      
      IF ( coupler_name == 'oasis' ) CALL cpl_oasis_finalize()

   END SUBROUTINE cpl_finalize


   SUBROUTINE cpl_abort( cdroutine, cdtxt )
      
      
      
      
      
      CHARACTER(*), INTENT(IN) :: cdroutine   
      CHARACTER(*), INTENT(IN) :: cdtxt       
      

      IF ( coupler_name == 'oasis' ) CALL cpl_oasis_abort( cdroutine, cdtxt )

   END SUBROUTINE cpl_abort


END MODULE module_cpl
