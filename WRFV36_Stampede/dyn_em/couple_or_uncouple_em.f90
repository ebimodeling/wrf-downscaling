

SUBROUTINE couple_or_uncouple_em ( grid , config_flags , couple &







,moist,moist_bxs,moist_bxe,moist_bys,moist_bye,moist_btxs,moist_btxe,moist_btys,moist_btye,dfi_moist,dfi_moist_bxs,dfi_moist_bxe, &
dfi_moist_bys,dfi_moist_bye,dfi_moist_btxs,dfi_moist_btxe,dfi_moist_btys,dfi_moist_btye,scalar,scalar_bxs,scalar_bxe,scalar_bys, &
scalar_bye,scalar_btxs,scalar_btxe,scalar_btys,scalar_btye,dfi_scalar,dfi_scalar_bxs,dfi_scalar_bxe,dfi_scalar_bys, &
dfi_scalar_bye,dfi_scalar_btxs,dfi_scalar_btxe,dfi_scalar_btys,dfi_scalar_btye,aerod,ozmixm,aerosolc_1,aerosolc_2,fdda3d,fdda2d, &
advh_t,advz_t,nba_mij,nba_rij,chem,tracer,tracer_bxs,tracer_bxe,tracer_bys,tracer_bye,tracer_btxs,tracer_btxe,tracer_btys, &
tracer_btye &


                 )





   USE module_domain, ONLY : domain, get_ijk_from_grid
   USE module_configure, ONLY : grid_config_rec_type
   USE module_driver_constants
   USE module_machine
   USE module_tiles
   USE module_dm, ONLY : local_communicator, mytask, ntasks, ntasks_x, ntasks_y, local_communicator_periodic
   USE module_comm_dm, ONLY : halo_em_couple_a_sub,halo_em_couple_b_sub,period_em_couple_a_sub,period_em_couple_b_sub
   USE module_bc


   USE module_state_description

   IMPLICIT NONE

   

   TYPE(domain) , TARGET         :: grid

   






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


   
   TYPE (grid_config_rec_type) , INTENT(IN)          :: config_flags

   LOGICAL, INTENT(   IN) :: couple

   

   INTEGER                         :: k_start , k_end
   INTEGER                         :: ids , ide , jds , jde , kds , kde , &
                                      ims , ime , jms , jme , kms , kme , &
                                      ips , ipe , jps , jpe , kps , kpe

   INTEGER                         :: i,j,k, im
   INTEGER                         :: num_3d_c, num_3d_m, num_3d_s
   REAL                            :: mu_factor

   REAL, DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33) :: mut_2, muut_2, muvt_2, muwt_2



   CALL get_ijk_from_grid (  grid ,                   &
                             ids, ide, jds, jde, kds, kde,    &
                             ims, ime, jms, jme, kms, kme,    &
                             ips, ipe, jps, jpe, kps, kpe    )

   num_3d_m        = num_moist
   num_3d_c        = num_chem
   num_3d_s        = num_scalar

   
   
















   IF ( config_flags%periodic_x .OR. config_flags%periodic_y ) THEN
     CALL set_physical_bc2d( grid%mub, 't',  &
                             config_flags,           &
                             ids,ide, jds,jde,   & 
                             ims,ime, jms,jme,   & 
                             ips,ipe, jps,jpe,   & 
                             ips,ipe, jps,jpe   )
     CALL set_physical_bc2d( grid%mu_1, 't',  &
                             config_flags,           &
                             ids,ide, jds,jde,   & 
                             ims,ime, jms,jme,   & 
                             ips,ipe, jps,jpe,   & 
                             ips,ipe, jps,jpe   )
     CALL set_physical_bc2d( grid%mu_2, 't',  &
                             config_flags,           &
                             ids,ide, jds,jde,   & 
                             ims,ime, jms,jme,   & 
                             ips,ipe, jps,jpe,   & 
                             ips,ipe, jps,jpe   )
   ENDIF








CALL HALO_EM_COUPLE_A_sub ( grid, &
  local_communicator, &
  mytask, ntasks, ntasks_x, ntasks_y, &
  ids, ide, jds, jde, kds, kde,       &
  ims, ime, jms, jme, kms, kme,       &
  ips, ipe, jps, jpe, kps, kpe )







CALL PERIOD_EM_COUPLE_A_sub ( grid, &
  config_flags, &
  local_communicator_periodic, &
  mytask, ntasks, ntasks_x, ntasks_y, &
  ids, ide, jds, jde, kds, kde,       &
  ims, ime, jms, jme, kms, kme,       &
  ips, ipe, jps, jpe, kps, kpe )


   

   IF( couple ) THEN



     DO j = max(jds,jps),min(jde-1,jpe)
     DO i = max(ids,ips),min(ide-1,ipe)
       mut_2(i,j) = grid%mub(i,j) + grid%mu_2(i,j)
       muwt_2(i,j) = (grid%mub(i,j) + grid%mu_2(i,j))/grid%msfty(i,j) 
     ENDDO
     ENDDO





     DO j = max(jds,jps),min(jde-1,jpe)
     DO i = max(ids,ips),min(ide-1,ipe)
       muut_2(i,j) = 0.5*(grid%mub(i,j)+grid%mub(i-1,j) + grid%mu_2(i,j) + grid%mu_2(i-1,j))/grid%msfuy(i,j) 
       muvt_2(i,j) = 0.5*(grid%mub(i,j)+grid%mub(i,j-1) + grid%mu_2(i,j) + grid%mu_2(i,j-1))/grid%msfvx(i,j) 
     ENDDO
     ENDDO

     IF ( config_flags%nested .or. config_flags%specified .or. config_flags%polar ) THEN

       IF ( jpe .eq. jde ) THEN
         j = jde
         DO i = max(ids,ips),min(ide-1,ipe)
           muvt_2(i,j) = (grid%mub(i,j-1) + grid%mu_2(i,j-1))/grid%msfvx(i,j) 
         ENDDO
       ENDIF
       IF ( ipe .eq. ide .AND. .NOT. config_flags%periodic_x ) THEN
         i = ide
         DO j = max(jds,jps),min(jde-1,jpe)
           muut_2(i,j) = (grid%mub(i-1,j) + grid%mu_2(i-1,j))/grid%msfuy(i,j) 
         ENDDO
       ENDIF

     ELSE

       IF ( jpe .eq. jde ) THEN
         j = jde
         DO i = max(ids,ips),min(ide-1,ipe)
           muvt_2(i,j) = 0.5*(grid%mub(i,j)+grid%mub(i,j-1) + grid%mu_2(i,j) + grid%mu_2(i,j-1))/grid%msfvx(i,j) 
         ENDDO
       ENDIF
       IF ( ipe .eq. ide ) THEN
         i = ide       
         DO j = max(jds,jps),min(jde-1,jpe)
           muut_2(i,j) = 0.5*(grid%mub(i,j)+grid%mub(i-1,j) + grid%mu_2(i,j) + grid%mu_2(i-1,j))/grid%msfuy(i,j) 
         ENDDO
       ENDIF

     END IF

   ELSE
   


     DO j = max(jds,jps),min(jde-1,jpe)
     DO i = max(ids,ips),min(ide-1,ipe)
       mut_2(i,j) = 1./(grid%mub(i,j) + grid%mu_2(i,j))
       muwt_2(i,j) = grid%msfty(i,j)/(grid%mub(i,j) + grid%mu_2(i,j)) 
     ENDDO
     ENDDO



     DO j = max(jds,jps),min(jde-1,jpe)
     DO i = max(ids,ips),min(ide-1,ipe)
       muut_2(i,j) = 2.*grid%msfuy(i,j)/(grid%mub(i,j)+grid%mub(i-1,j) + grid%mu_2(i,j) + grid%mu_2(i-1,j)) 
     ENDDO
     ENDDO

     DO j = max(jds,jps),min(jde-1,jpe)
     DO i = max(ids,ips),min(ide-1,ipe)
       muvt_2(i,j) = 2.*grid%msfvx(i,j)/(grid%mub(i,j)+grid%mub(i,j-1) + grid%mu_2(i,j) + grid%mu_2(i,j-1)) 
     ENDDO
     ENDDO

     IF ( config_flags%nested .or. config_flags%specified .or. config_flags%polar ) THEN

       IF ( jpe .eq. jde ) THEN
         j = jde 
         DO i = max(ids,ips),min(ide-1,ipe)
           muvt_2(i,j) = grid%msfvx(i,j)/(grid%mub(i,j-1) + grid%mu_2(i,j-1)) 
         ENDDO
       ENDIF
       IF ( ipe .eq. ide .AND. .NOT. config_flags%periodic_x ) THEN
         i = ide
         DO j = max(jds,jps),min(jde-1,jpe)
           muut_2(i,j) = grid%msfuy(i,j)/(grid%mub(i-1,j) + grid%mu_2(i-1,j)) 
         ENDDO
       ENDIF

     ELSE

       IF ( jpe .eq. jde ) THEN
         j = jde
         DO i = max(ids,ips),min(ide-1,ipe)
           muvt_2(i,j) = 2.*grid%msfvx(i,j)/(grid%mub(i,j)+grid%mub(i,j-1) + grid%mu_2(i,j) + grid%mu_2(i,j-1)) 
         ENDDO
       ENDIF
       IF ( ipe .eq. ide ) THEN
         i = ide       
         DO j = max(jds,jps),min(jde-1,jpe)
           muut_2(i,j) = 2.*grid%msfuy(i,j)/(grid%mub(i,j)+grid%mub(i-1,j) + grid%mu_2(i,j) + grid%mu_2(i-1,j)) 
         ENDDO
       ENDIF

     END IF

   END IF

   

   !$OMP PARALLEL DO   &
   !$OMP PRIVATE ( i,j,k,im )
   DO j = max(jds,jps),min(jde-1,jpe)

     DO k = kps,kpe
     DO i = max(ids,ips),min(ide-1,ipe)
       grid%ph_2(i,k,j) = grid%ph_2(i,k,j)*mut_2(i,j)
       grid%w_2(i,k,j)  =  grid%w_2(i,k,j)*muwt_2(i,j)
     ENDDO
     ENDDO

     DO k = kps,kpe-1
     DO i = max(ids,ips),min(ide-1,ipe)
       grid%t_2(i,k,j)  =  grid%t_2(i,k,j)*mut_2(i,j)
     ENDDO
     ENDDO

     IF (num_3d_m >= PARAM_FIRST_SCALAR )  THEN
       DO im = PARAM_FIRST_SCALAR, num_3d_m
         DO k = kps,kpe-1
         DO i = max(ids,ips),min(ide-1,ipe)
           moist(i,k,j,im)  =  moist(i,k,j,im)*mut_2(i,j)
         ENDDO
         ENDDO
       ENDDO
     END IF

     IF (num_3d_c >= PARAM_FIRST_SCALAR )  THEN
       DO im = PARAM_FIRST_SCALAR, num_3d_c
         DO k = kps,kpe-1
         DO i = max(ids,ips),min(ide-1,ipe)
           chem(i,k,j,im)  =  chem(i,k,j,im)*mut_2(i,j)
         ENDDO
         ENDDO
       ENDDO
     END IF

     IF (num_3d_s >= PARAM_FIRST_SCALAR )  THEN
       DO im = PARAM_FIRST_SCALAR, num_3d_s
         DO k = kps,kpe-1
         DO i = max(ids,ips),min(ide-1,ipe)
           scalar(i,k,j,im)  =  scalar(i,k,j,im)*mut_2(i,j)
         ENDDO
         ENDDO
       ENDDO
     END IF

     IF (num_tracer >= PARAM_FIRST_SCALAR )  THEN
       DO im = PARAM_FIRST_SCALAR, num_tracer
         DO k = kps,kpe-1
         DO i = max(ids,ips),min(ide-1,ipe)
           tracer(i,k,j,im)  =  tracer(i,k,j,im)*mut_2(i,j)
         ENDDO
         ENDDO
       ENDDO
     END IF



     DO k = kps,kpe-1
     DO i = max(ids,ips),min(ide,ipe)
       grid%u_2(i,k,j)  =  grid%u_2(i,k,j)*muut_2(i,j)
     ENDDO
     ENDDO

   ENDDO   
   !$OMP END PARALLEL DO

   !$OMP PARALLEL DO   &
   !$OMP PRIVATE ( i,j,k )
   DO j = max(jds,jps),min(jde,jpe)
     DO k = kps,kpe-1
     DO i = max(ids,ips),min(ide-1,ipe)
       grid%v_2(i,k,j)  =  grid%v_2(i,k,j)*muvt_2(i,j)
     ENDDO
     ENDDO
   ENDDO
   !$OMP END PARALLEL DO

   IF ( config_flags%periodic_x .OR. config_flags%periodic_y ) THEN
     CALL set_physical_bc3d( grid%ph_1, 'w',        &
                             config_flags,                   &
                             ids,ide, jds,jde, kds,kde,  & 
                             ims,ime, jms,jme, kms,kme,  & 
                             ips,ipe, jps,jpe, kps,kpe,  & 
                             ips,ipe, jps,jpe, kps,kpe )
     CALL set_physical_bc3d( grid%ph_2, 'w',        &
                             config_flags,                   &
                             ids,ide, jds,jde, kds,kde,  & 
                             ims,ime, jms,jme, kms,kme,  & 
                             ips,ipe, jps,jpe, kps,kpe,  & 
                             ips,ipe, jps,jpe, kps,kpe )
     CALL set_physical_bc3d( grid%w_1, 'w',        &
                             config_flags,                   &
                             ids,ide, jds,jde, kds,kde,  & 
                             ims,ime, jms,jme, kms,kme,  & 
                             ips,ipe, jps,jpe, kps,kpe,  & 
                             ips,ipe, jps,jpe, kps,kpe )
     CALL set_physical_bc3d( grid%w_2, 'w',        &
                             config_flags,                   &
                             ids,ide, jds,jde, kds,kde,  & 
                             ims,ime, jms,jme, kms,kme,  & 
                             ips,ipe, jps,jpe, kps,kpe,  & 
                             ips,ipe, jps,jpe, kps,kpe )
     CALL set_physical_bc3d( grid%t_1, 't',        &
                             config_flags,                   &
                             ids,ide, jds,jde, kds,kde,  & 
                             ims,ime, jms,jme, kms,kme,  & 
                             ips,ipe, jps,jpe, kps,kpe,  & 
                             ips,ipe, jps,jpe, kps,kpe )
     CALL set_physical_bc3d( grid%t_2, 't',        &
                             config_flags,                   &
                             ids,ide, jds,jde, kds,kde,  & 
                             ims,ime, jms,jme, kms,kme,  & 
                             ips,ipe, jps,jpe, kps,kpe,  & 
                             ips,ipe, jps,jpe, kps,kpe )
     CALL set_physical_bc3d( grid%u_1, 'u',        &
                             config_flags,                   &
                             ids,ide, jds,jde, kds,kde,  & 
                             ims,ime, jms,jme, kms,kme,  & 
                             ips,ipe, jps,jpe, kps,kpe,  & 
                             ips,ipe, jps,jpe, kps,kpe )
     CALL set_physical_bc3d( grid%u_2, 'u',        &
                             config_flags,                   &
                             ids,ide, jds,jde, kds,kde,  & 
                             ims,ime, jms,jme, kms,kme,  & 
                             ips,ipe, jps,jpe, kps,kpe,  & 
                             ips,ipe, jps,jpe, kps,kpe )
     CALL set_physical_bc3d( grid%v_1, 'v',        &
                             config_flags,                   &
                             ids,ide, jds,jde, kds,kde,  & 
                             ims,ime, jms,jme, kms,kme,  & 
                             ips,ipe, jps,jpe, kps,kpe,  & 
                             ips,ipe, jps,jpe, kps,kpe )
     CALL set_physical_bc3d( grid%v_2, 'v',        &
                             config_flags,                   &
                             ids,ide, jds,jde, kds,kde,  & 
                             ims,ime, jms,jme, kms,kme,  & 
                             ips,ipe, jps,jpe, kps,kpe,  & 
                             ips,ipe, jps,jpe, kps,kpe )

     IF (num_3d_m >= PARAM_FIRST_SCALAR) THEN
       DO im = PARAM_FIRST_SCALAR , num_3d_m

     CALL set_physical_bc3d( moist(ims,kms,jms,im), 'p',        &
                             config_flags,                   &
                             ids,ide, jds,jde, kds,kde,  & 
                             ims,ime, jms,jme, kms,kme,  & 
                             ips,ipe, jps,jpe, kps,kpe,  & 
                             ips,ipe, jps,jpe, kps,kpe )
       ENDDO
     ENDIF


     IF (num_3d_c >= PARAM_FIRST_SCALAR) THEN
       DO im = PARAM_FIRST_SCALAR , num_3d_c

     CALL set_physical_bc3d( chem(ims,kms,jms,im), 'p',        &
                             config_flags,                   &
                             ids,ide, jds,jde, kds,kde,  & 
                             ims,ime, jms,jme, kms,kme,  & 
                             ips,ipe, jps,jpe, kps,kpe,  & 
                             ips,ipe, jps,jpe, kps,kpe )
     ENDDO
     ENDIF

     IF (num_3d_s >= PARAM_FIRST_SCALAR) THEN
       DO im = PARAM_FIRST_SCALAR , num_3d_s

     CALL set_physical_bc3d( scalar(ims,kms,jms,im), 'p',        &
                             config_flags,                   &
                             ids,ide, jds,jde, kds,kde,  & 
                             ims,ime, jms,jme, kms,kme,  & 
                             ips,ipe, jps,jpe, kps,kpe,  & 
                             ips,ipe, jps,jpe, kps,kpe )
     ENDDO
     ENDIF

     IF (num_tracer >= PARAM_FIRST_SCALAR) THEN
       DO im = PARAM_FIRST_SCALAR , num_tracer
 
     CALL set_physical_bc3d( tracer(ims,kms,jms,im), 'p',        &
                             config_flags,                   &
                             ids,ide, jds,jde, kds,kde,  & 
                             ims,ime, jms,jme, kms,kme,  & 
                             ips,ipe, jps,jpe, kps,kpe,  & 
                             ips,ipe, jps,jpe, kps,kpe )
     ENDDO
     ENDIF

   ENDIF







CALL HALO_EM_COUPLE_B_sub ( grid, &
  num_moist, &
  moist, &
  num_chem, &
  chem, &
  num_tracer, &
  tracer, &
  num_scalar, &
  scalar, &
  local_communicator, &
  mytask, ntasks, ntasks_x, ntasks_y, &
  ids, ide, jds, jde, kds, kde,       &
  ims, ime, jms, jme, kms, kme,       &
  ips, ipe, jps, jpe, kps, kpe )







CALL PERIOD_EM_COUPLE_B_sub ( grid, &
  config_flags, &
  num_moist, &
  moist, &
  num_chem, &
  chem, &
  num_tracer, &
  tracer, &
  num_scalar, &
  scalar, &
  local_communicator_periodic, &
  mytask, ntasks, ntasks_x, ntasks_y, &
  ids, ide, jds, jde, kds, kde,       &
  ims, ime, jms, jme, kms, kme,       &
  ips, ipe, jps, jpe, kps, kpe )


END SUBROUTINE couple_or_uncouple_em

LOGICAL FUNCTION cd_feedback_mask( pig, ips_save, ipe_save , pjg, jps_save, jpe_save, xstag, ystag )
   IMPLICIT NONE
   INTEGER, INTENT(IN) :: pig, ips_save, ipe_save , pjg, jps_save, jpe_save
   LOGICAL, INTENT(IN) :: xstag, ystag

   INTEGER ioff, joff, spec_zone

   CALL nl_get_spec_zone( 1, spec_zone )
   ioff = 0 ; joff = 0 
   IF ( xstag  ) ioff = 1
   IF ( ystag  ) joff = 1

   cd_feedback_mask = ( pig .ge. ips_save+spec_zone        .and.      &
                           pjg .ge. jps_save+spec_zone        .and.      &
                           pig .le. ipe_save-spec_zone  +ioff .and.      &
                           pjg .le. jpe_save-spec_zone  +joff           )


END FUNCTION cd_feedback_mask

