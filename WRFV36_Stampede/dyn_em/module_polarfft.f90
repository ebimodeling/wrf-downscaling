MODULE module_polarfft

  USE module_model_constants
  USE module_wrf_error
  CHARACTER (LEN=256) , PRIVATE :: a_message

CONTAINS

SUBROUTINE couple_scalars_for_filter ( field    &
                 ,mu,mub                        &
                 ,ids,ide,jds,jde,kds,kde       &
                 ,ims,ime,jms,jme,kms,kme       &
                 ,ips,ipe,jps,jpe,kps,kpe       )
   IMPLICIT NONE
   INTEGER, INTENT(IN) :: ids,ide,jds,jde,kds,kde       &
                         ,ims,ime,jms,jme,kms,kme       &
                         ,ips,ipe,jps,jpe,kps,kpe
   REAL , DIMENSION(ims:ime,kms:kme,jms:jme) , INTENT(INOUT) :: field
   REAL , DIMENSION(ims:ime,jms:jme) , INTENT(IN) :: mu,mub

   INTEGER :: i , j , k

   DO j = jps, MIN(jpe,jde-1)
   DO k = kps, kpe-1
   DO i = ips, MIN(ipe,ide-1)
      field(i,k,j)=field(i,k,j)*(mu(i,j)+mub(i,j))
   END DO
   END DO
   END DO

END SUBROUTINE couple_scalars_for_filter

SUBROUTINE uncouple_scalars_for_filter ( field    &
                 ,mu,mub                        &
                 ,ids,ide,jds,jde,kds,kde       &
                 ,ims,ime,jms,jme,kms,kme       &
                 ,ips,ipe,jps,jpe,kps,kpe       )
   IMPLICIT NONE
   INTEGER, INTENT(IN) :: ids,ide,jds,jde,kds,kde       &
                         ,ims,ime,jms,jme,kms,kme       &
                         ,ips,ipe,jps,jpe,kps,kpe
   REAL , DIMENSION(ims:ime,kms:kme,jms:jme) , INTENT(INOUT) :: field
   REAL , DIMENSION(ims:ime,jms:jme) , INTENT(IN) :: mu,mub

   INTEGER :: i , j , k

   DO j = jps, MIN(jpe,jde-1)
   DO k = kps, kpe-1
   DO i = ips, MIN(ipe,ide-1)
      field(i,k,j)=field(i,k,j)/(mu(i,j)+mub(i,j))
   END DO
   END DO
   END DO

END SUBROUTINE uncouple_scalars_for_filter

SUBROUTINE pxft ( grid                          &
                 ,lineno       &
                 ,flag_uv,flag_rurv             &
                 ,flag_wph,flag_ww              &
                 ,flag_t                        &
                 ,flag_mu,flag_mut              &
                 ,flag_moist                    &
                 ,flag_chem                     &
                 ,flag_tracer                   &
                 ,flag_scalar                   &
                 ,fft_filter_lat, dclat         &
                 ,positive_definite             &
                 ,moist,chem,tracer,scalar      &
                 ,ids,ide,jds,jde,kds,kde       &
                 ,ims,ime,jms,jme,kms,kme       &
                 ,ips,ipe,jps,jpe,kps,kpe       &
                 ,imsx,imex,jmsx,jmex,kmsx,kmex &
                 ,ipsx,ipex,jpsx,jpex,kpsx,kpex )
   USE module_state_description
   USE module_domain, ONLY : domain

   USE module_dm, ONLY : local_communicator, mytask, ntasks, ntasks_x, ntasks_y &
                       , local_communicator_periodic, itrace                    &
                       , local_communicator_x 
   USE module_driver_constants
   IMPLICIT NONE
   
   TYPE(domain) , TARGET          :: grid
integer, intent(in) :: lineno
integer myproc, i, j, k
   LOGICAL, INTENT(IN) :: positive_definite
   INTEGER, INTENT(IN) :: ids,ide,jds,jde,kds,kde       &
                         ,ims,ime,jms,jme,kms,kme       &
                         ,ips,ipe,jps,jpe,kps,kpe       &
                         ,imsx,imex,jmsx,jmex,kmsx,kmex &
                         ,ipsx,ipex,jpsx,jpex,kpsx,kpex
   REAL  , INTENT(IN) :: fft_filter_lat
   REAL,    INTENT(IN) :: dclat
   INTEGER, INTENT(IN) :: flag_uv                       &
                         ,flag_rurv                     &
                         ,flag_ww                       &
                         ,flag_t,flag_wph               &
                         ,flag_mu,flag_mut              &
                         ,flag_moist                    &
                         ,flag_chem                     &
                         ,flag_tracer                   &
                         ,flag_scalar
    REAL, DIMENSION(ims:ime,kms:kme,jms:jme,*) , INTENT(INOUT) :: moist, chem, scalar,tracer

   
   LOGICAL piggyback_mu, piggyback_mut
   INTEGER ij, k_end



   piggyback_mu  = flag_mu .EQ. 1
   piggyback_mut = flag_mut .EQ. 1































call wrf_get_myproc(myproc)









   IF ( flag_uv .EQ. 1 ) THEN
     IF ( piggyback_mu ) THEN
       grid%u_2(ips:ipe,kde,jps:jpe) = grid%mu_2(ips:ipe,jps:jpe) 
     ENDIF






  call trans_z2x ( ntasks_x, local_communicator_x, 1, 4, 4, DATA_ORDER_XZY , &
                   grid%v_2, &  
                   grid%sd31, grid%ed31, grid%sd32, grid%ed32, grid%sd33, grid%ed33, &
                   grid%sp31, grid%ep31, grid%sp32, grid%ep32, grid%sp33, grid%ep33, &
                   grid%sm31, grid%em31, grid%sm32, grid%em32, grid%sm33, grid%em33, &
                   grid%v_xxx, &  
                   grid%sp31x, grid%ep31x, grid%sp32x, grid%ep32x, grid%sp33x, grid%ep33x, &
                   grid%sm31x, grid%em31x, grid%sm32x, grid%em32x, grid%sm33x, grid%em33x ) 


     CALL polar_filter_3d( grid%v_xxx, grid%clat_xxx, .false.,     &
                                fft_filter_lat, dclat,                 &
                                ids, ide, jds, jde, kds, kde-1,         &
                                imsx, imex, jmsx, jmex, kmsx, kmex,     &
                                ipsx, ipex, jpsx, jpex, kpsx, MIN(kde-1,kpex ) )







  call trans_z2x ( ntasks_x, local_communicator_x, 0, 4, 4, DATA_ORDER_XZY , &
                   grid%v_2, &  
                   grid%sd31, grid%ed31, grid%sd32, grid%ed32, grid%sd33, grid%ed33, &
                   grid%sp31, grid%ep31, grid%sp32, grid%ep32, grid%sp33, grid%ep33, &
                   grid%sm31, grid%em31, grid%sm32, grid%em32, grid%sm33, grid%em33, &
                   grid%v_xxx, &  
                   grid%sp31x, grid%ep31x, grid%sp32x, grid%ep32x, grid%sp33x, grid%ep33x, &
                   grid%sm31x, grid%em31x, grid%sm32x, grid%em32x, grid%sm33x, grid%em33x ) 







  call trans_z2x ( ntasks_x, local_communicator_x, 1, 4, 4, DATA_ORDER_XZY , &
                   grid%u_2, &  
                   grid%sd31, grid%ed31, grid%sd32, grid%ed32, grid%sd33, grid%ed33, &
                   grid%sp31, grid%ep31, grid%sp32, grid%ep32, grid%sp33, grid%ep33, &
                   grid%sm31, grid%em31, grid%sm32, grid%em32, grid%sm33, grid%em33, &
                   grid%u_xxx, &  
                   grid%sp31x, grid%ep31x, grid%sp32x, grid%ep32x, grid%sp33x, grid%ep33x, &
                   grid%sm31x, grid%em31x, grid%sm32x, grid%em32x, grid%sm33x, grid%em33x ) 

     k_end = MIN(kde-1,kpex)
     IF ( piggyback_mu ) k_end = MIN(kde,kpex)

     CALL polar_filter_3d( grid%u_xxx, grid%clat_xxx, piggyback_mu,     &
                                fft_filter_lat, 0.,                &
                                ids, ide, jds, jde, kds, kde,       &
                                imsx, imex, jmsx, jmex, kmsx, kmex, &
                                ipsx, ipex, jpsx, jpex, kpsx, k_end )







  call trans_z2x ( ntasks_x, local_communicator_x, 0, 4, 4, DATA_ORDER_XZY , &
                   grid%u_2, &  
                   grid%sd31, grid%ed31, grid%sd32, grid%ed32, grid%sd33, grid%ed33, &
                   grid%sp31, grid%ep31, grid%sp32, grid%ep32, grid%sp33, grid%ep33, &
                   grid%sm31, grid%em31, grid%sm32, grid%em32, grid%sm33, grid%em33, &
                   grid%u_xxx, &  
                   grid%sp31x, grid%ep31x, grid%sp32x, grid%ep32x, grid%sp33x, grid%ep33x, &
                   grid%sm31x, grid%em31x, grid%sm32x, grid%em32x, grid%sm33x, grid%em33x ) 


     IF ( piggyback_mu ) THEN
       grid%mu_2(ips:ipe,jps:jpe) = grid%u_2(ips:ipe,kde,jps:jpe)
       piggyback_mu = .FALSE.
     ENDIF
   ENDIF



   IF ( flag_t .EQ. 1 ) THEN
     IF ( piggyback_mu ) THEN
       grid%t_2(ips:ipe,kde,jps:jpe) = grid%mu_2(ips:ipe,jps:jpe)
     ENDIF






  call trans_z2x ( ntasks_x, local_communicator_x, 1, 4, 4, DATA_ORDER_XZY , &
                   grid%t_2, &  
                   grid%sd31, grid%ed31, grid%sd32, grid%ed32, grid%sd33, grid%ed33, &
                   grid%sp31, grid%ep31, grid%sp32, grid%ep32, grid%sp33, grid%ep33, &
                   grid%sm31, grid%em31, grid%sm32, grid%em32, grid%sm33, grid%em33, &
                   grid%t_xxx, &  
                   grid%sp31x, grid%ep31x, grid%sp32x, grid%ep32x, grid%sp33x, grid%ep33x, &
                   grid%sm31x, grid%em31x, grid%sm32x, grid%em32x, grid%sm33x, grid%em33x ) 

     k_end = MIN(kde-1,kpex)
     IF ( piggyback_mu ) k_end = MIN(kde,kpex)

     CALL polar_filter_3d( grid%t_xxx, grid%clat_xxx,piggyback_mu,     &
                                fft_filter_lat, 0.,                &
                                ids, ide, jds, jde, kds, kde-1,     &
                                imsx, imex, jmsx, jmex, kmsx, kmex, &
                                ipsx, ipex, jpsx, jpex, kpsx, k_end )







  call trans_z2x ( ntasks_x, local_communicator_x, 0, 4, 4, DATA_ORDER_XZY , &
                   grid%t_2, &  
                   grid%sd31, grid%ed31, grid%sd32, grid%ed32, grid%sd33, grid%ed33, &
                   grid%sp31, grid%ep31, grid%sp32, grid%ep32, grid%sp33, grid%ep33, &
                   grid%sm31, grid%em31, grid%sm32, grid%em32, grid%sm33, grid%em33, &
                   grid%t_xxx, &  
                   grid%sp31x, grid%ep31x, grid%sp32x, grid%ep32x, grid%sp33x, grid%ep33x, &
                   grid%sm31x, grid%em31x, grid%sm32x, grid%em32x, grid%sm33x, grid%em33x ) 

     IF ( piggyback_mu ) THEN
       grid%mu_2(ips:ipe,jps:jpe) = grid%t_2(ips:ipe,kde,jps:jpe)
       piggyback_mu = .FALSE.
     ENDIF
   ENDIF



   IF ( flag_wph .EQ. 1 ) THEN
      






  call trans_z2x ( ntasks_x, local_communicator_x, 1, 4, 4, DATA_ORDER_XZY , &
                   grid%w_2, &  
                   grid%sd31, grid%ed31, grid%sd32, grid%ed32, grid%sd33, grid%ed33, &
                   grid%sp31, grid%ep31, grid%sp32, grid%ep32, grid%sp33, grid%ep33, &
                   grid%sm31, grid%em31, grid%sm32, grid%em32, grid%sm33, grid%em33, &
                   grid%w_xxx, &  
                   grid%sp31x, grid%ep31x, grid%sp32x, grid%ep32x, grid%sp33x, grid%ep33x, &
                   grid%sm31x, grid%em31x, grid%sm32x, grid%em32x, grid%sm33x, grid%em33x ) 

      CALL polar_filter_3d( grid%w_xxx, grid%clat_xxx, .false.,     &
                                 fft_filter_lat, 0.,                &
                                 ids, ide, jds, jde, kds, kde,       &
                                 imsx, imex, jmsx, jmex, kmsx, kmex, &
                                 ipsx, ipex, jpsx, jpex, kpsx, kpex )







  call trans_z2x ( ntasks_x, local_communicator_x, 0, 4, 4, DATA_ORDER_XZY , &
                   grid%w_2, &  
                   grid%sd31, grid%ed31, grid%sd32, grid%ed32, grid%sd33, grid%ed33, &
                   grid%sp31, grid%ep31, grid%sp32, grid%ep32, grid%sp33, grid%ep33, &
                   grid%sm31, grid%em31, grid%sm32, grid%em32, grid%sm33, grid%em33, &
                   grid%w_xxx, &  
                   grid%sp31x, grid%ep31x, grid%sp32x, grid%ep32x, grid%sp33x, grid%ep33x, &
                   grid%sm31x, grid%em31x, grid%sm32x, grid%em32x, grid%sm33x, grid%em33x ) 







  call trans_z2x ( ntasks_x, local_communicator_x, 1, 4, 4, DATA_ORDER_XZY , &
                   grid%ph_2, &  
                   grid%sd31, grid%ed31, grid%sd32, grid%ed32, grid%sd33, grid%ed33, &
                   grid%sp31, grid%ep31, grid%sp32, grid%ep32, grid%sp33, grid%ep33, &
                   grid%sm31, grid%em31, grid%sm32, grid%em32, grid%sm33, grid%em33, &
                   grid%ph_xxx, &  
                   grid%sp31x, grid%ep31x, grid%sp32x, grid%ep32x, grid%sp33x, grid%ep33x, &
                   grid%sm31x, grid%em31x, grid%sm32x, grid%em32x, grid%sm33x, grid%em33x ) 


      CALL polar_filter_3d( grid%ph_xxx, grid%clat_xxx, .false.,     &
                                 fft_filter_lat, 0.,                &
                                 ids, ide, jds, jde, kds, kde,       &
                                 imsx, imex, jmsx, jmex, kmsx, kmex, &
                                 ipsx, ipex, jpsx, jpex, kpsx, kpex )







  call trans_z2x ( ntasks_x, local_communicator_x, 0, 4, 4, DATA_ORDER_XZY , &
                   grid%ph_2, &  
                   grid%sd31, grid%ed31, grid%sd32, grid%ed32, grid%sd33, grid%ed33, &
                   grid%sp31, grid%ep31, grid%sp32, grid%ep32, grid%sp33, grid%ep33, &
                   grid%sm31, grid%em31, grid%sm32, grid%em32, grid%sm33, grid%em33, &
                   grid%ph_xxx, &  
                   grid%sp31x, grid%ep31x, grid%sp32x, grid%ep32x, grid%sp33x, grid%ep33x, &
                   grid%sm31x, grid%em31x, grid%sm32x, grid%em32x, grid%sm33x, grid%em33x ) 

   ENDIF



   IF ( flag_ww .EQ. 1 ) THEN
      






  call trans_z2x ( ntasks_x, local_communicator_x, 1, 4, 4, DATA_ORDER_XZY , &
                   grid%ww_m, &  
                   grid%sd31, grid%ed31, grid%sd32, grid%ed32, grid%sd33, grid%ed33, &
                   grid%sp31, grid%ep31, grid%sp32, grid%ep32, grid%sp33, grid%ep33, &
                   grid%sm31, grid%em31, grid%sm32, grid%em32, grid%sm33, grid%em33, &
                   grid%ww_xxx, &  
                   grid%sp31x, grid%ep31x, grid%sp32x, grid%ep32x, grid%sp33x, grid%ep33x, &
                   grid%sm31x, grid%em31x, grid%sm32x, grid%em32x, grid%sm33x, grid%em33x ) 

      CALL polar_filter_3d( grid%ww_xxx, grid%clat_xxx, .false.,     &
                                 fft_filter_lat, 0.,                &
                                 ids, ide, jds, jde, kds, kde,       &
                                 imsx, imex, jmsx, jmex, kmsx, kmex, &
                                 ipsx, ipex, jpsx, jpex, kpsx, kpex )







  call trans_z2x ( ntasks_x, local_communicator_x, 0, 4, 4, DATA_ORDER_XZY , &
                   grid%ww_m, &  
                   grid%sd31, grid%ed31, grid%sd32, grid%ed32, grid%sd33, grid%ed33, &
                   grid%sp31, grid%ep31, grid%sp32, grid%ep32, grid%sp33, grid%ep33, &
                   grid%sm31, grid%em31, grid%sm32, grid%em32, grid%sm33, grid%em33, &
                   grid%ww_xxx, &  
                   grid%sp31x, grid%ep31x, grid%sp32x, grid%ep32x, grid%sp33x, grid%ep33x, &
                   grid%sm31x, grid%em31x, grid%sm32x, grid%em32x, grid%sm33x, grid%em33x ) 

   ENDIF



   IF ( flag_rurv .EQ. 1 ) THEN
     IF ( piggyback_mut ) THEN
       grid%ru_m(ips:ipe,kde,jps:jpe) = grid%mut(ips:ipe,jps:jpe)
     ENDIF






  call trans_z2x ( ntasks_x, local_communicator_x, 1, 4, 4, DATA_ORDER_XZY , &
                   grid%rv_m, &  
                   grid%sd31, grid%ed31, grid%sd32, grid%ed32, grid%sd33, grid%ed33, &
                   grid%sp31, grid%ep31, grid%sp32, grid%ep32, grid%sp33, grid%ep33, &
                   grid%sm31, grid%em31, grid%sm32, grid%em32, grid%sm33, grid%em33, &
                   grid%rv_xxx, &  
                   grid%sp31x, grid%ep31x, grid%sp32x, grid%ep32x, grid%sp33x, grid%ep33x, &
                   grid%sm31x, grid%em31x, grid%sm32x, grid%em32x, grid%sm33x, grid%em33x ) 

     CALL polar_filter_3d( grid%rv_xxx, grid%clat_xxx, .false.,     &
                                fft_filter_lat, dclat,             &
                                ids, ide, jds, jde, kds, kde,       &
                                imsx, imex, jmsx, jmex, kmsx, kmex, &
                                ipsx, ipex, jpsx, jpex, kpsx, MIN(kpex,kde-1) )







  call trans_z2x ( ntasks_x, local_communicator_x, 0, 4, 4, DATA_ORDER_XZY , &
                   grid%rv_m, &  
                   grid%sd31, grid%ed31, grid%sd32, grid%ed32, grid%sd33, grid%ed33, &
                   grid%sp31, grid%ep31, grid%sp32, grid%ep32, grid%sp33, grid%ep33, &
                   grid%sm31, grid%em31, grid%sm32, grid%em32, grid%sm33, grid%em33, &
                   grid%rv_xxx, &  
                   grid%sp31x, grid%ep31x, grid%sp32x, grid%ep32x, grid%sp33x, grid%ep33x, &
                   grid%sm31x, grid%em31x, grid%sm32x, grid%em32x, grid%sm33x, grid%em33x ) 







  call trans_z2x ( ntasks_x, local_communicator_x, 1, 4, 4, DATA_ORDER_XZY , &
                   grid%ru_m, &  
                   grid%sd31, grid%ed31, grid%sd32, grid%ed32, grid%sd33, grid%ed33, &
                   grid%sp31, grid%ep31, grid%sp32, grid%ep32, grid%sp33, grid%ep33, &
                   grid%sm31, grid%em31, grid%sm32, grid%em32, grid%sm33, grid%em33, &
                   grid%ru_xxx, &  
                   grid%sp31x, grid%ep31x, grid%sp32x, grid%ep32x, grid%sp33x, grid%ep33x, &
                   grid%sm31x, grid%em31x, grid%sm32x, grid%em32x, grid%sm33x, grid%em33x ) 

     k_end = MIN(kde-1,kpex)
     IF ( piggyback_mut ) k_end = MIN(kde,kpex)

     CALL polar_filter_3d( grid%ru_xxx, grid%clat_xxx, piggyback_mut,     &
                                fft_filter_lat, 0.,                &
                                ids, ide, jds, jde, kds, kde,       &
                                imsx, imex, jmsx, jmex, kmsx, kmex, &
                                ipsx, ipex, jpsx, jpex, kpsx, k_end )






  call trans_z2x ( ntasks_x, local_communicator_x, 0, 4, 4, DATA_ORDER_XZY , &
                   grid%ru_m, &  
                   grid%sd31, grid%ed31, grid%sd32, grid%ed32, grid%sd33, grid%ed33, &
                   grid%sp31, grid%ep31, grid%sp32, grid%ep32, grid%sp33, grid%ep33, &
                   grid%sm31, grid%em31, grid%sm32, grid%em32, grid%sm33, grid%em33, &
                   grid%ru_xxx, &  
                   grid%sp31x, grid%ep31x, grid%sp32x, grid%ep32x, grid%sp33x, grid%ep33x, &
                   grid%sm31x, grid%em31x, grid%sm32x, grid%em32x, grid%sm33x, grid%em33x ) 

     IF ( piggyback_mut ) THEN
       grid%mut(ips:ipe,jps:jpe) = grid%ru_m(ips:ipe,kde,jps:jpe)
       piggyback_mut = .FALSE.
     ENDIF
   ENDIF



   IF ( flag_moist .GE. PARAM_FIRST_SCALAR ) THEN
     itrace = flag_moist






  call trans_z2x ( ntasks_x, local_communicator_x, 1, 4, 4, DATA_ORDER_XZY , &
                   moist(grid%sm31,grid%sm32,grid%sm33,itrace ), &  
                   grid%sd31, grid%ed31, grid%sd32, grid%ed32, grid%sd33, grid%ed33, &
                   grid%sp31, grid%ep31, grid%sp32, grid%ep32, grid%sp33, grid%ep33, &
                   grid%sm31, grid%em31, grid%sm32, grid%em32, grid%sm33, grid%em33, &
                   grid%fourd_xxx, &  
                   grid%sp31x, grid%ep31x, grid%sp32x, grid%ep32x, grid%sp33x, grid%ep33x, &
                   grid%sm31x, grid%em31x, grid%sm32x, grid%em32x, grid%sm33x, grid%em33x ) 

     CALL polar_filter_3d( grid%fourd_xxx, grid%clat_xxx, .false. ,     &
                           fft_filter_lat, 0.,                &
                           ids, ide, jds, jde, kds, kde,       &
                           imsx, imex, jmsx, jmex, kmsx, kmex, &
                           ipsx, ipex, jpsx, jpex, kpsx, MIN(kpex,kde-1), &
                           positive_definite = positive_definite )






  call trans_z2x ( ntasks_x, local_communicator_x, 0, 4, 4, DATA_ORDER_XZY , &
                   moist(grid%sm31,grid%sm32,grid%sm33,itrace ), &  
                   grid%sd31, grid%ed31, grid%sd32, grid%ed32, grid%sd33, grid%ed33, &
                   grid%sp31, grid%ep31, grid%sp32, grid%ep32, grid%sp33, grid%ep33, &
                   grid%sm31, grid%em31, grid%sm32, grid%em32, grid%sm33, grid%em33, &
                   grid%fourd_xxx, &  
                   grid%sp31x, grid%ep31x, grid%sp32x, grid%ep32x, grid%sp33x, grid%ep33x, &
                   grid%sm31x, grid%em31x, grid%sm32x, grid%em32x, grid%sm33x, grid%em33x ) 

   ENDIF



   IF ( flag_chem .GE. PARAM_FIRST_SCALAR ) THEN
     itrace = flag_chem






  call trans_z2x ( ntasks_x, local_communicator_x, 1, 4, 4, DATA_ORDER_XZY , &
                   chem(grid%sm31,grid%sm32,grid%sm33,itrace ), &  
                   grid%sd31, grid%ed31, grid%sd32, grid%ed32, grid%sd33, grid%ed33, &
                   grid%sp31, grid%ep31, grid%sp32, grid%ep32, grid%sp33, grid%ep33, &
                   grid%sm31, grid%em31, grid%sm32, grid%em32, grid%sm33, grid%em33, &
                   grid%fourd_xxx, &  
                   grid%sp31x, grid%ep31x, grid%sp32x, grid%ep32x, grid%sp33x, grid%ep33x, &
                   grid%sm31x, grid%em31x, grid%sm32x, grid%em32x, grid%sm33x, grid%em33x ) 

     CALL polar_filter_3d( grid%fourd_xxx, grid%clat_xxx, .false. ,     &
                           fft_filter_lat, 0.,                &
                           ids, ide, jds, jde, kds, kde,       &
                           imsx, imex, jmsx, jmex, kmsx, kmex, &
                           ipsx, ipex, jpsx, jpex, kpsx, MIN(kpex,kde-1), &
                           positive_definite = positive_definite )






  call trans_z2x ( ntasks_x, local_communicator_x, 0, 4, 4, DATA_ORDER_XZY , &
                   chem(grid%sm31,grid%sm32,grid%sm33,itrace ), &  
                   grid%sd31, grid%ed31, grid%sd32, grid%ed32, grid%sd33, grid%ed33, &
                   grid%sp31, grid%ep31, grid%sp32, grid%ep32, grid%sp33, grid%ep33, &
                   grid%sm31, grid%em31, grid%sm32, grid%em32, grid%sm33, grid%em33, &
                   grid%fourd_xxx, &  
                   grid%sp31x, grid%ep31x, grid%sp32x, grid%ep32x, grid%sp33x, grid%ep33x, &
                   grid%sm31x, grid%em31x, grid%sm32x, grid%em32x, grid%sm33x, grid%em33x ) 

   ENDIF



   IF ( flag_tracer .GE. PARAM_FIRST_SCALAR ) THEN
     itrace = flag_tracer






  call trans_z2x ( ntasks_x, local_communicator_x, 1, 4, 4, DATA_ORDER_XZY , &
                   tracer(grid%sm31,grid%sm32,grid%sm33,itrace ), &  
                   grid%sd31, grid%ed31, grid%sd32, grid%ed32, grid%sd33, grid%ed33, &
                   grid%sp31, grid%ep31, grid%sp32, grid%ep32, grid%sp33, grid%ep33, &
                   grid%sm31, grid%em31, grid%sm32, grid%em32, grid%sm33, grid%em33, &
                   grid%fourd_xxx, &  
                   grid%sp31x, grid%ep31x, grid%sp32x, grid%ep32x, grid%sp33x, grid%ep33x, &
                   grid%sm31x, grid%em31x, grid%sm32x, grid%em32x, grid%sm33x, grid%em33x ) 


     CALL polar_filter_3d( grid%fourd_xxx, grid%clat_xxx, .false. ,     &
                           fft_filter_lat, 0.,                &
                           ids, ide, jds, jde, kds, kde,       &
                           imsx, imex, jmsx, jmex, kmsx, kmex, &
                           ipsx, ipex, jpsx, jpex, kpsx, MIN(kpex,kde-1), &
                           positive_definite = positive_definite )







  call trans_z2x ( ntasks_x, local_communicator_x, 0, 4, 4, DATA_ORDER_XZY , &
                   tracer(grid%sm31,grid%sm32,grid%sm33,itrace ), &  
                   grid%sd31, grid%ed31, grid%sd32, grid%ed32, grid%sd33, grid%ed33, &
                   grid%sp31, grid%ep31, grid%sp32, grid%ep32, grid%sp33, grid%ep33, &
                   grid%sm31, grid%em31, grid%sm32, grid%em32, grid%sm33, grid%em33, &
                   grid%fourd_xxx, &  
                   grid%sp31x, grid%ep31x, grid%sp32x, grid%ep32x, grid%sp33x, grid%ep33x, &
                   grid%sm31x, grid%em31x, grid%sm32x, grid%em32x, grid%sm33x, grid%em33x ) 

   ENDIF



   IF ( flag_scalar .GE. PARAM_FIRST_SCALAR ) THEN
     itrace = flag_scalar






  call trans_z2x ( ntasks_x, local_communicator_x, 1, 4, 4, DATA_ORDER_XZY , &
                   scalar(grid%sm31,grid%sm32,grid%sm33,itrace ), &  
                   grid%sd31, grid%ed31, grid%sd32, grid%ed32, grid%sd33, grid%ed33, &
                   grid%sp31, grid%ep31, grid%sp32, grid%ep32, grid%sp33, grid%ep33, &
                   grid%sm31, grid%em31, grid%sm32, grid%em32, grid%sm33, grid%em33, &
                   grid%fourd_xxx, &  
                   grid%sp31x, grid%ep31x, grid%sp32x, grid%ep32x, grid%sp33x, grid%ep33x, &
                   grid%sm31x, grid%em31x, grid%sm32x, grid%em32x, grid%sm33x, grid%em33x ) 

     CALL polar_filter_3d( grid%fourd_xxx , grid%clat_xxx, .false. ,     &
                           fft_filter_lat, 0.,                &
                           ids, ide, jds, jde, kds, kde,       &
                           imsx, imex, jmsx, jmex, kmsx, kmex, &
                           ipsx, ipex, jpsx, jpex, kpsx, MIN(kpex,kde-1), &
                           positive_definite = positive_definite )






  call trans_z2x ( ntasks_x, local_communicator_x, 0, 4, 4, DATA_ORDER_XZY , &
                   scalar(grid%sm31,grid%sm32,grid%sm33,itrace ), &  
                   grid%sd31, grid%ed31, grid%sd32, grid%ed32, grid%sd33, grid%ed33, &
                   grid%sp31, grid%ep31, grid%sp32, grid%ep32, grid%sp33, grid%ep33, &
                   grid%sm31, grid%em31, grid%sm32, grid%em32, grid%sm33, grid%em33, &
                   grid%fourd_xxx, &  
                   grid%sp31x, grid%ep31x, grid%sp32x, grid%ep32x, grid%sp33x, grid%ep33x, &
                   grid%sm31x, grid%em31x, grid%sm32x, grid%em32x, grid%sm33x, grid%em33x ) 

   ENDIF

   IF ( flag_mu .EQ. 1 .AND. piggyback_mu ) THEN
      CALL wrf_error_fatal3("<stdin>",671,&
"mu needed to get piggybacked on a transpose and did not")
   ENDIF
   IF ( flag_mut .EQ. 1 .AND. piggyback_mut ) THEN
      CALL wrf_error_fatal3("<stdin>",675,&
"mut needed to get piggybacked on a transpose and did not")
   ENDIF


   RETURN
END SUBROUTINE pxft

SUBROUTINE polar_filter_3d( f, xlat, piggyback, fft_filter_lat, dvlat, &
                            ids, ide, jds, jde, kds, kde,    &
                            ims, ime, jms, jme, kms, kme,    &
                            its, ite, jts, jte, kts, kte,    &
                            positive_definite               )

  IMPLICIT NONE

  INTEGER ,       INTENT(IN   ) :: ids, ide, jds, jde, kds, kde, &
                                   ims, ime, jms, jme, kms, kme, &
                                   its, ite, jts, jte, kts, kte
  REAL   ,       INTENT(IN   ) :: fft_filter_lat

  REAL , DIMENSION( ims:ime , kms:kme, jms:jme ) , INTENT(INOUT) ::  f
  REAL , DIMENSION( ims:ime , jms:jme ) , INTENT(IN) ::  xlat
  REAL , INTENT(IN) ::  dvlat
  LOGICAL , INTENT(IN), OPTIONAL :: positive_definite
  LOGICAL , INTENT(IN) :: piggyback

  REAL , DIMENSION(1:ide-ids,1:kte-kts+1) :: sheet
  REAL , DIMENSION(1:kte-kts+1) :: sheet_total
  REAL :: lat, avg, rnboxw
  INTEGER :: ig, jg, i, j, j_end, nx, ny, nmax, kw
  INTEGER :: k, nboxw, nbox2, istart, iend, overlap
  INTEGER, DIMENSION(6) :: wavenumber = (/ 1, 3, 7, 10, 13, 16 /)

  INTEGER :: fftflag

  
  
  
  

  
  IF ((its /= ids) .OR. (ite /= ide)) THEN
     WRITE ( wrf_err_message , * ) 'module_polarfft: 3d: (its /= ids) or (ite /= ide)',its,ids,ite,ide
     CALL wrf_error_fatal3("<stdin>",719,&
TRIM( wrf_err_message ) )
  END IF

  fftflag= 1  


  nx = ide-ids 
  ny = kte-kts+1 
  lat = 0.
  j_end = MIN(jte, jde-1)
  IF (dvlat /= 0. .and. j_end == jde-1) j_end = jde
  DO j = jts, j_end
     
     jg = j-jds+1

     

     if(xlat(ids,j).gt.0.) then
        lat = xlat(ids,j)-dvlat
     else
        lat = xlat(ids,j)+dvlat
     endif

     IF (abs(lat) >= fft_filter_lat) THEN

        DO k=kts,kte
        DO i=ids,ide-1
           sheet(i-ids+1,k-kts+1) = f(i,k,j)
        END DO
        END DO

        call polar_filter_fft_2d_ncar(nx,ny,sheet,lat,fft_filter_lat,piggyback,fftflag)

        DO k=kts,kte
           DO i=ids,ide-1
              f(i,k,j) = sheet(i-ids+1,k-kts+1)
           END DO
           
           

           DO i=1,ids-ims
              f(ids-i,k,j)=f(ide-i,k,j)
           END DO
           DO i=1,ime-ide+1
              f(ide+i-1,k,j)=f(ids+i-1,k,j)
           END DO
        END DO

     END IF
  END DO 

END SUBROUTINE polar_filter_3d



SUBROUTINE polar_filter_fft_2d_ncar(nx,ny,fin,lat,filter_latitude,piggyback,fftflag)
  IMPLICIT NONE
  INTEGER , INTENT(IN) :: nx, ny
  REAL , DIMENSION(nx,ny), INTENT(INOUT) :: fin
  REAL , INTENT(IN) :: lat, filter_latitude
  LOGICAL, INTENT(IN) :: piggyback

  REAL :: pi, rcosref, freq, c, cf

  INTEGER :: k, fftflag
  REAL, DIMENSION(NX,NY) :: work
  REAL, DIMENSION(NX+15) :: wsave
  REAL(KIND=8), DIMENSION(NX,NY) :: fin8, work8 
  REAL(KIND=8), DIMENSION(NX+15) :: wsave8

  INTEGER :: i, j
  REAL, dimension(nx,ny) :: fp

  INTEGER :: lensave, ier, nh, n1
  INTEGER :: lot, jump, n, inc, lenr, lensav, lenwrk

  REAL, PARAMETER :: alpha = 0.0
  REAL :: factor_k

  INTEGER :: ntop

  pi = ACOS(-1.)
  rcosref = 1./COS(filter_latitude*pi/180.)



  n = nx
  lot = ny
  lensav = n+15
  inc = 1
  lenr = nx*ny
  jump = nx
  lenwrk = lenr
  ntop = ny
  IF(piggyback) ntop = ny-1





  if(fftflag.eq.0) then
     call rfftmi(n,wsave,lensav,ier)
  else
     call dfft1i(n,wsave8,lensav,ier)
  endif

  IF(ier /= 0) THEN
    write(a_message,*) ' error in rfftmi ',ier
    CALL wrf_message ( a_message ) 
  END IF



  if(fftflag.eq.0) then
     call rfftmf(lot, jump, n, inc, fin, lenr, wsave, lensav, work, lenwrk, ier)
  else 
     fin8 = fin
     do k=1,ny 
        call dfft1f(n, inc, fin8(1,k), lenr, wsave8, lensav, work8, lenwrk, ier)
     enddo 
  endif

  IF(ier /= 0) THEN
    write(a_message,*) ' error in rfftmf ',ier
    CALL wrf_message ( a_message ) 
  END IF

  if(MOD(n,2) == 0) then
    nh = n/2 - 1
  else
    nh = (n-1)/2
  end if

  DO j=1,ny
   fp(1,j) = 1.
  ENDDO

  DO i=2,nh+1
    freq=REAL(i-1)/REAL(n)
    c = (rcosref*COS(lat*pi/180.)/SIN(freq*pi))**2


    do j=1,ntop
      factor_k = (1.-alpha)+alpha*min(1.,float(ntop - j)/10.)
      cf = c*factor_k*factor_k
      cf = MAX(0.,MIN(1.,cf))
      fp(2*(i-1),j) = cf
      fp(2*(i-1)+1,j) = cf
    enddo
    if(piggyback) then
      cf = MAX(0.,MIN(1.,c))
      fp(2*(i-1),ny) = cf
      fp(2*(i-1)+1,ny) = cf
    endif
  END DO

  IF(MOD(n,2) == 0) THEN
    c = (rcosref*COS(lat*pi/180.))**2

    do j=1,ntop
      factor_k = (1.-alpha)+alpha*min(1.,float(ntop - j)/10.)
      cf = c*factor_k*factor_k
      cf = MAX(0.,MIN(1.,cf))
      fp(n,j) = cf
    enddo
    if(piggyback) then
      cf = MAX(0.,MIN(1.,c))
      fp(n,ny) = cf
    endif
  END IF

  if(fftflag.eq.0) then
     do j=1,ny
       do i=1,nx
          fin(i,j) = fp(i,j)*fin(i,j)
       enddo
     enddo
  else
     do j=1,ny
       do i=1,nx
          fin8(i,j) = fp(i,j)*fin8(i,j)
       enddo
     enddo
  endif



  if(fftflag.eq.0) then
     call rfftmb(lot, jump, n, inc, fin, lenr, wsave, lensav, work, lenwrk, ier)
  else
     do k=1,ny 
        call dfft1b(n, inc, fin8(1,k), lenr, wsave8, lensav, work8, lenwrk, ier)
     enddo
     fin= fin8     
  endif

  IF(ier /= 0) THEN
    write(a_message,*) ' error in rfftmb ',ier
    CALL wrf_message ( a_message ) 
  END IF

END SUBROUTINE polar_filter_fft_2d_ncar



END MODULE module_polarfft

