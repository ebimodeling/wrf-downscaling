
MODULE module_comm_dm_2

   IMPLICIT NONE

   PRIVATE module_comm_dm_dummy_2


   INTEGER, PRIVATE :: rsl_sendw_p, rsl_sendbeg_p, rsl_recvw_p, rsl_recvbeg_p
   INTEGER, PRIVATE :: rsl_sendw_m, rsl_sendbeg_m, rsl_recvw_m, rsl_recvbeg_m
   LOGICAL, EXTERNAL :: rsl_comm_iter


   INTEGER, PRIVATE :: idim1, idim2, idim3, idim4, idim5, idim6, idim7


CONTAINS

   
   SUBROUTINE module_comm_dm_dummy_2
     USE module_domain, ONLY:domain
     USE module_configure, ONLY:grid_config_rec_type,in_use_for_config
     USE module_state_description, ONLY:PARAM_FIRST_SCALAR
     USE module_driver_constants
     RETURN
   END SUBROUTINE module_comm_dm_dummy_2








SUBROUTINE HALO_EM_CHEM_E_5_sub ( grid, &
  num_chem, &
  chem, &
  local_communicator, &
  mytask, ntasks, ntasks_x, ntasks_y, &
  ids, ide, jds, jde, kds, kde,       &
  ims, ime, jms, jme, kms, kme,       &
  ips, ipe, jps, jpe, kps, kpe )
  USE module_domain, ONLY:domain
  USE module_configure, ONLY:grid_config_rec_type,in_use_for_config
  USE module_state_description, ONLY:PARAM_FIRST_SCALAR
  USE module_driver_constants
  TYPE(domain) ,               INTENT(IN) :: grid
  INTEGER, INTENT(IN) :: num_chem
  real, INTENT(INOUT) :: chem ( grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33,num_chem)
  INTEGER ,                    INTENT(IN) :: local_communicator
  INTEGER ,                    INTENT(IN) :: mytask, ntasks, ntasks_x, ntasks_y
  INTEGER ,                    INTENT(IN) :: ids, ide, jds, jde, kds, kde
  INTEGER ,                    INTENT(IN) :: ims, ime, jms, jme, kms, kme
  INTEGER ,                    INTENT(IN) :: ips, ipe, jps, jpe, kps, kpe
  INTEGER :: itrace
  INTEGER :: rsl_sendw_p, rsl_sendbeg_p, rsl_recvw_p, rsl_recvbeg_p
  INTEGER :: rsl_sendw_m, rsl_sendbeg_m, rsl_recvw_m, rsl_recvbeg_m
  LOGICAL, EXTERNAL :: rsl_comm_iter
  INTEGER :: idim1, idim2, idim3, idim4, idim5, idim6, idim7
  






CALL wrf_debug(2,'calling inc/HALO_EM_CHEM_E_5_inline.inc')
CALL rsl_comm_iter_init(3,jps,jpe)
DO WHILE ( rsl_comm_iter( grid%id , grid%is_intermediate, 3 , &
                         0 , jds,jde,jps,jpe, grid%njds, grid%njde , & 
     rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p,   & 
     rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p    ))
 CALL RSL_LITE_INIT_EXCH ( local_communicator, 3, 0, &
     rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p,   & 
     rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p,   & 
     0  &
   + num_chem   &
     , 0, 4, &
     0, 0, 4, &
     0, 0, 8, &
      0,  0, 4, &
      mytask, ntasks, ntasks_x, ntasks_y,   &
      ips, ipe, jps, jpe, kps, MAX(1,1&
,kpe &
))
DO itrace = PARAM_FIRST_SCALAR, num_chem
 IF ( SIZE(chem,1)*SIZE(chem,3) .GT. 1 ) THEN
  CALL RSL_LITE_PACK ( local_communicator,&
chem ( grid%sm31,grid%sm32,grid%sm33,itrace),3,&
rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p, &
rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p, &
4, 0, 0, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
 ENDIF
ENDDO
   CALL RSL_LITE_EXCH_Y ( local_communicator , mytask, ntasks, ntasks_x, ntasks_y, &
                          rsl_sendw_m,  rsl_sendw_p, rsl_recvw_m,  rsl_recvw_p    )
DO itrace = PARAM_FIRST_SCALAR, num_chem
 IF ( SIZE(chem,1)*SIZE(chem,3) .GT. 1 ) THEN
  CALL RSL_LITE_PACK ( local_communicator,&
chem ( grid%sm31,grid%sm32,grid%sm33,itrace),3,&
rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p, &
rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p, &
4, 0, 1, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
 ENDIF
ENDDO
ENDDO
CALL rsl_comm_iter_init(3,ips,ipe)
DO WHILE ( rsl_comm_iter( grid%id , grid%is_intermediate, 3 , &
                         1 , ids,ide,ips,ipe, grid%nids, grid%nide , & 
     rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p,   & 
     rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p    ))
 CALL RSL_LITE_INIT_EXCH ( local_communicator, 3, 1, &
     rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p,   & 
     rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p,   & 
     0  &
   + num_chem   &
     , 0, 4, &
     0, 0, 4, &
     0, 0, 8, &
      0,  0, 4, &
      mytask, ntasks, ntasks_x, ntasks_y,   &
      ips, ipe, jps, jpe, kps, MAX(1,1&
,kpe &
))
DO itrace = PARAM_FIRST_SCALAR, num_chem
 IF ( SIZE(chem,1)*SIZE(chem,3) .GT. 1 ) THEN
  CALL RSL_LITE_PACK ( local_communicator,&
chem ( grid%sm31,grid%sm32,grid%sm33,itrace),3,&
rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p, &
rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p, &
4, 1, 0, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
 ENDIF
ENDDO
   CALL RSL_LITE_EXCH_X ( local_communicator , mytask, ntasks, ntasks_x, ntasks_y, &
                          rsl_sendw_m,  rsl_sendw_p, rsl_recvw_m,  rsl_recvw_p    )
DO itrace = PARAM_FIRST_SCALAR, num_chem
 IF ( SIZE(chem,1)*SIZE(chem,3) .GT. 1 ) THEN
  CALL RSL_LITE_PACK ( local_communicator,&
chem ( grid%sm31,grid%sm32,grid%sm33,itrace),3,&
rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p, &
rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p, &
4, 1, 1, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
 ENDIF
ENDDO
    ENDDO

  
  END SUBROUTINE HALO_EM_CHEM_E_5_sub







SUBROUTINE HALO_EM_CHEM_E_7_sub ( grid, &
  num_chem, &
  chem, &
  local_communicator, &
  mytask, ntasks, ntasks_x, ntasks_y, &
  ids, ide, jds, jde, kds, kde,       &
  ims, ime, jms, jme, kms, kme,       &
  ips, ipe, jps, jpe, kps, kpe )
  USE module_domain, ONLY:domain
  USE module_configure, ONLY:grid_config_rec_type,in_use_for_config
  USE module_state_description, ONLY:PARAM_FIRST_SCALAR
  USE module_driver_constants
  TYPE(domain) ,               INTENT(IN) :: grid
  INTEGER, INTENT(IN) :: num_chem
  real, INTENT(INOUT) :: chem ( grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33,num_chem)
  INTEGER ,                    INTENT(IN) :: local_communicator
  INTEGER ,                    INTENT(IN) :: mytask, ntasks, ntasks_x, ntasks_y
  INTEGER ,                    INTENT(IN) :: ids, ide, jds, jde, kds, kde
  INTEGER ,                    INTENT(IN) :: ims, ime, jms, jme, kms, kme
  INTEGER ,                    INTENT(IN) :: ips, ipe, jps, jpe, kps, kpe
  INTEGER :: itrace
  INTEGER :: rsl_sendw_p, rsl_sendbeg_p, rsl_recvw_p, rsl_recvbeg_p
  INTEGER :: rsl_sendw_m, rsl_sendbeg_m, rsl_recvw_m, rsl_recvbeg_m
  LOGICAL, EXTERNAL :: rsl_comm_iter
  INTEGER :: idim1, idim2, idim3, idim4, idim5, idim6, idim7
  






CALL wrf_debug(2,'calling inc/HALO_EM_CHEM_E_7_inline.inc')
CALL rsl_comm_iter_init(4,jps,jpe)
DO WHILE ( rsl_comm_iter( grid%id , grid%is_intermediate, 4 , &
                         0 , jds,jde,jps,jpe, grid%njds, grid%njde , & 
     rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p,   & 
     rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p    ))
 CALL RSL_LITE_INIT_EXCH ( local_communicator, 4, 0, &
     rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p,   & 
     rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p,   & 
     0  &
   + num_chem   &
     , 0, 4, &
     0, 0, 4, &
     0, 0, 8, &
      0,  0, 4, &
      mytask, ntasks, ntasks_x, ntasks_y,   &
      ips, ipe, jps, jpe, kps, MAX(1,1&
,kpe &
))
DO itrace = PARAM_FIRST_SCALAR, num_chem
 IF ( SIZE(chem,1)*SIZE(chem,3) .GT. 1 ) THEN
  CALL RSL_LITE_PACK ( local_communicator,&
chem ( grid%sm31,grid%sm32,grid%sm33,itrace),4,&
rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p, &
rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p, &
4, 0, 0, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
 ENDIF
ENDDO
   CALL RSL_LITE_EXCH_Y ( local_communicator , mytask, ntasks, ntasks_x, ntasks_y, &
                          rsl_sendw_m,  rsl_sendw_p, rsl_recvw_m,  rsl_recvw_p    )
DO itrace = PARAM_FIRST_SCALAR, num_chem
 IF ( SIZE(chem,1)*SIZE(chem,3) .GT. 1 ) THEN
  CALL RSL_LITE_PACK ( local_communicator,&
chem ( grid%sm31,grid%sm32,grid%sm33,itrace),4,&
rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p, &
rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p, &
4, 0, 1, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
 ENDIF
ENDDO
ENDDO
CALL rsl_comm_iter_init(4,ips,ipe)
DO WHILE ( rsl_comm_iter( grid%id , grid%is_intermediate, 4 , &
                         1 , ids,ide,ips,ipe, grid%nids, grid%nide , & 
     rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p,   & 
     rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p    ))
 CALL RSL_LITE_INIT_EXCH ( local_communicator, 4, 1, &
     rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p,   & 
     rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p,   & 
     0  &
   + num_chem   &
     , 0, 4, &
     0, 0, 4, &
     0, 0, 8, &
      0,  0, 4, &
      mytask, ntasks, ntasks_x, ntasks_y,   &
      ips, ipe, jps, jpe, kps, MAX(1,1&
,kpe &
))
DO itrace = PARAM_FIRST_SCALAR, num_chem
 IF ( SIZE(chem,1)*SIZE(chem,3) .GT. 1 ) THEN
  CALL RSL_LITE_PACK ( local_communicator,&
chem ( grid%sm31,grid%sm32,grid%sm33,itrace),4,&
rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p, &
rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p, &
4, 1, 0, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
 ENDIF
ENDDO
   CALL RSL_LITE_EXCH_X ( local_communicator , mytask, ntasks, ntasks_x, ntasks_y, &
                          rsl_sendw_m,  rsl_sendw_p, rsl_recvw_m,  rsl_recvw_p    )
DO itrace = PARAM_FIRST_SCALAR, num_chem
 IF ( SIZE(chem,1)*SIZE(chem,3) .GT. 1 ) THEN
  CALL RSL_LITE_PACK ( local_communicator,&
chem ( grid%sm31,grid%sm32,grid%sm33,itrace),4,&
rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p, &
rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p, &
4, 1, 1, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
 ENDIF
ENDDO
    ENDDO

  
  END SUBROUTINE HALO_EM_CHEM_E_7_sub







SUBROUTINE HALO_EM_TRACER_E_3_sub ( grid, &
  num_tracer, &
  tracer, &
  local_communicator, &
  mytask, ntasks, ntasks_x, ntasks_y, &
  ids, ide, jds, jde, kds, kde,       &
  ims, ime, jms, jme, kms, kme,       &
  ips, ipe, jps, jpe, kps, kpe )
  USE module_domain, ONLY:domain
  USE module_configure, ONLY:grid_config_rec_type,in_use_for_config
  USE module_state_description, ONLY:PARAM_FIRST_SCALAR
  USE module_driver_constants
  TYPE(domain) ,               INTENT(IN) :: grid
  INTEGER, INTENT(IN) :: num_tracer
  real, INTENT(INOUT) :: tracer ( grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33,num_tracer)
  INTEGER ,                    INTENT(IN) :: local_communicator
  INTEGER ,                    INTENT(IN) :: mytask, ntasks, ntasks_x, ntasks_y
  INTEGER ,                    INTENT(IN) :: ids, ide, jds, jde, kds, kde
  INTEGER ,                    INTENT(IN) :: ims, ime, jms, jme, kms, kme
  INTEGER ,                    INTENT(IN) :: ips, ipe, jps, jpe, kps, kpe
  INTEGER :: itrace
  INTEGER :: rsl_sendw_p, rsl_sendbeg_p, rsl_recvw_p, rsl_recvbeg_p
  INTEGER :: rsl_sendw_m, rsl_sendbeg_m, rsl_recvw_m, rsl_recvbeg_m
  LOGICAL, EXTERNAL :: rsl_comm_iter
  INTEGER :: idim1, idim2, idim3, idim4, idim5, idim6, idim7
  






CALL wrf_debug(2,'calling inc/HALO_EM_TRACER_E_3_inline.inc')
CALL rsl_comm_iter_init(2,jps,jpe)
DO WHILE ( rsl_comm_iter( grid%id , grid%is_intermediate, 2 , &
                         0 , jds,jde,jps,jpe, grid%njds, grid%njde , & 
     rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p,   & 
     rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p    ))
 CALL RSL_LITE_INIT_EXCH ( local_communicator, 2, 0, &
     rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p,   & 
     rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p,   & 
     0  &
   + num_tracer   &
     , 0, 4, &
     0, 0, 4, &
     0, 0, 8, &
      0,  0, 4, &
      mytask, ntasks, ntasks_x, ntasks_y,   &
      ips, ipe, jps, jpe, kps, MAX(1,1&
,kpe &
))
DO itrace = PARAM_FIRST_SCALAR, num_tracer
 IF ( SIZE(tracer,1)*SIZE(tracer,3) .GT. 1 ) THEN
  CALL RSL_LITE_PACK ( local_communicator,&
tracer ( grid%sm31,grid%sm32,grid%sm33,itrace),2,&
rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p, &
rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p, &
4, 0, 0, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
 ENDIF
ENDDO
   CALL RSL_LITE_EXCH_Y ( local_communicator , mytask, ntasks, ntasks_x, ntasks_y, &
                          rsl_sendw_m,  rsl_sendw_p, rsl_recvw_m,  rsl_recvw_p    )
DO itrace = PARAM_FIRST_SCALAR, num_tracer
 IF ( SIZE(tracer,1)*SIZE(tracer,3) .GT. 1 ) THEN
  CALL RSL_LITE_PACK ( local_communicator,&
tracer ( grid%sm31,grid%sm32,grid%sm33,itrace),2,&
rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p, &
rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p, &
4, 0, 1, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
 ENDIF
ENDDO
ENDDO
CALL rsl_comm_iter_init(2,ips,ipe)
DO WHILE ( rsl_comm_iter( grid%id , grid%is_intermediate, 2 , &
                         1 , ids,ide,ips,ipe, grid%nids, grid%nide , & 
     rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p,   & 
     rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p    ))
 CALL RSL_LITE_INIT_EXCH ( local_communicator, 2, 1, &
     rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p,   & 
     rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p,   & 
     0  &
   + num_tracer   &
     , 0, 4, &
     0, 0, 4, &
     0, 0, 8, &
      0,  0, 4, &
      mytask, ntasks, ntasks_x, ntasks_y,   &
      ips, ipe, jps, jpe, kps, MAX(1,1&
,kpe &
))
DO itrace = PARAM_FIRST_SCALAR, num_tracer
 IF ( SIZE(tracer,1)*SIZE(tracer,3) .GT. 1 ) THEN
  CALL RSL_LITE_PACK ( local_communicator,&
tracer ( grid%sm31,grid%sm32,grid%sm33,itrace),2,&
rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p, &
rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p, &
4, 1, 0, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
 ENDIF
ENDDO
   CALL RSL_LITE_EXCH_X ( local_communicator , mytask, ntasks, ntasks_x, ntasks_y, &
                          rsl_sendw_m,  rsl_sendw_p, rsl_recvw_m,  rsl_recvw_p    )
DO itrace = PARAM_FIRST_SCALAR, num_tracer
 IF ( SIZE(tracer,1)*SIZE(tracer,3) .GT. 1 ) THEN
  CALL RSL_LITE_PACK ( local_communicator,&
tracer ( grid%sm31,grid%sm32,grid%sm33,itrace),2,&
rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p, &
rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p, &
4, 1, 1, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
 ENDIF
ENDDO
    ENDDO

  
  END SUBROUTINE HALO_EM_TRACER_E_3_sub







SUBROUTINE HALO_EM_TRACER_E_5_sub ( grid, &
  num_tracer, &
  tracer, &
  local_communicator, &
  mytask, ntasks, ntasks_x, ntasks_y, &
  ids, ide, jds, jde, kds, kde,       &
  ims, ime, jms, jme, kms, kme,       &
  ips, ipe, jps, jpe, kps, kpe )
  USE module_domain, ONLY:domain
  USE module_configure, ONLY:grid_config_rec_type,in_use_for_config
  USE module_state_description, ONLY:PARAM_FIRST_SCALAR
  USE module_driver_constants
  TYPE(domain) ,               INTENT(IN) :: grid
  INTEGER, INTENT(IN) :: num_tracer
  real, INTENT(INOUT) :: tracer ( grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33,num_tracer)
  INTEGER ,                    INTENT(IN) :: local_communicator
  INTEGER ,                    INTENT(IN) :: mytask, ntasks, ntasks_x, ntasks_y
  INTEGER ,                    INTENT(IN) :: ids, ide, jds, jde, kds, kde
  INTEGER ,                    INTENT(IN) :: ims, ime, jms, jme, kms, kme
  INTEGER ,                    INTENT(IN) :: ips, ipe, jps, jpe, kps, kpe
  INTEGER :: itrace
  INTEGER :: rsl_sendw_p, rsl_sendbeg_p, rsl_recvw_p, rsl_recvbeg_p
  INTEGER :: rsl_sendw_m, rsl_sendbeg_m, rsl_recvw_m, rsl_recvbeg_m
  LOGICAL, EXTERNAL :: rsl_comm_iter
  INTEGER :: idim1, idim2, idim3, idim4, idim5, idim6, idim7
  






CALL wrf_debug(2,'calling inc/HALO_EM_TRACER_E_5_inline.inc')
CALL rsl_comm_iter_init(3,jps,jpe)
DO WHILE ( rsl_comm_iter( grid%id , grid%is_intermediate, 3 , &
                         0 , jds,jde,jps,jpe, grid%njds, grid%njde , & 
     rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p,   & 
     rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p    ))
 CALL RSL_LITE_INIT_EXCH ( local_communicator, 3, 0, &
     rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p,   & 
     rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p,   & 
     0  &
   + num_tracer   &
     , 0, 4, &
     0, 0, 4, &
     0, 0, 8, &
      0,  0, 4, &
      mytask, ntasks, ntasks_x, ntasks_y,   &
      ips, ipe, jps, jpe, kps, MAX(1,1&
,kpe &
))
DO itrace = PARAM_FIRST_SCALAR, num_tracer
 IF ( SIZE(tracer,1)*SIZE(tracer,3) .GT. 1 ) THEN
  CALL RSL_LITE_PACK ( local_communicator,&
tracer ( grid%sm31,grid%sm32,grid%sm33,itrace),3,&
rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p, &
rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p, &
4, 0, 0, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
 ENDIF
ENDDO
   CALL RSL_LITE_EXCH_Y ( local_communicator , mytask, ntasks, ntasks_x, ntasks_y, &
                          rsl_sendw_m,  rsl_sendw_p, rsl_recvw_m,  rsl_recvw_p    )
DO itrace = PARAM_FIRST_SCALAR, num_tracer
 IF ( SIZE(tracer,1)*SIZE(tracer,3) .GT. 1 ) THEN
  CALL RSL_LITE_PACK ( local_communicator,&
tracer ( grid%sm31,grid%sm32,grid%sm33,itrace),3,&
rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p, &
rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p, &
4, 0, 1, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
 ENDIF
ENDDO
ENDDO
CALL rsl_comm_iter_init(3,ips,ipe)
DO WHILE ( rsl_comm_iter( grid%id , grid%is_intermediate, 3 , &
                         1 , ids,ide,ips,ipe, grid%nids, grid%nide , & 
     rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p,   & 
     rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p    ))
 CALL RSL_LITE_INIT_EXCH ( local_communicator, 3, 1, &
     rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p,   & 
     rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p,   & 
     0  &
   + num_tracer   &
     , 0, 4, &
     0, 0, 4, &
     0, 0, 8, &
      0,  0, 4, &
      mytask, ntasks, ntasks_x, ntasks_y,   &
      ips, ipe, jps, jpe, kps, MAX(1,1&
,kpe &
))
DO itrace = PARAM_FIRST_SCALAR, num_tracer
 IF ( SIZE(tracer,1)*SIZE(tracer,3) .GT. 1 ) THEN
  CALL RSL_LITE_PACK ( local_communicator,&
tracer ( grid%sm31,grid%sm32,grid%sm33,itrace),3,&
rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p, &
rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p, &
4, 1, 0, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
 ENDIF
ENDDO
   CALL RSL_LITE_EXCH_X ( local_communicator , mytask, ntasks, ntasks_x, ntasks_y, &
                          rsl_sendw_m,  rsl_sendw_p, rsl_recvw_m,  rsl_recvw_p    )
DO itrace = PARAM_FIRST_SCALAR, num_tracer
 IF ( SIZE(tracer,1)*SIZE(tracer,3) .GT. 1 ) THEN
  CALL RSL_LITE_PACK ( local_communicator,&
tracer ( grid%sm31,grid%sm32,grid%sm33,itrace),3,&
rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p, &
rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p, &
4, 1, 1, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
 ENDIF
ENDDO
    ENDDO

  
  END SUBROUTINE HALO_EM_TRACER_E_5_sub







SUBROUTINE HALO_EM_TRACER_E_7_sub ( grid, &
  num_tracer, &
  tracer, &
  local_communicator, &
  mytask, ntasks, ntasks_x, ntasks_y, &
  ids, ide, jds, jde, kds, kde,       &
  ims, ime, jms, jme, kms, kme,       &
  ips, ipe, jps, jpe, kps, kpe )
  USE module_domain, ONLY:domain
  USE module_configure, ONLY:grid_config_rec_type,in_use_for_config
  USE module_state_description, ONLY:PARAM_FIRST_SCALAR
  USE module_driver_constants
  TYPE(domain) ,               INTENT(IN) :: grid
  INTEGER, INTENT(IN) :: num_tracer
  real, INTENT(INOUT) :: tracer ( grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33,num_tracer)
  INTEGER ,                    INTENT(IN) :: local_communicator
  INTEGER ,                    INTENT(IN) :: mytask, ntasks, ntasks_x, ntasks_y
  INTEGER ,                    INTENT(IN) :: ids, ide, jds, jde, kds, kde
  INTEGER ,                    INTENT(IN) :: ims, ime, jms, jme, kms, kme
  INTEGER ,                    INTENT(IN) :: ips, ipe, jps, jpe, kps, kpe
  INTEGER :: itrace
  INTEGER :: rsl_sendw_p, rsl_sendbeg_p, rsl_recvw_p, rsl_recvbeg_p
  INTEGER :: rsl_sendw_m, rsl_sendbeg_m, rsl_recvw_m, rsl_recvbeg_m
  LOGICAL, EXTERNAL :: rsl_comm_iter
  INTEGER :: idim1, idim2, idim3, idim4, idim5, idim6, idim7
  






CALL wrf_debug(2,'calling inc/HALO_EM_TRACER_E_7_inline.inc')
CALL rsl_comm_iter_init(4,jps,jpe)
DO WHILE ( rsl_comm_iter( grid%id , grid%is_intermediate, 4 , &
                         0 , jds,jde,jps,jpe, grid%njds, grid%njde , & 
     rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p,   & 
     rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p    ))
 CALL RSL_LITE_INIT_EXCH ( local_communicator, 4, 0, &
     rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p,   & 
     rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p,   & 
     0  &
   + num_tracer   &
     , 0, 4, &
     0, 0, 4, &
     0, 0, 8, &
      0,  0, 4, &
      mytask, ntasks, ntasks_x, ntasks_y,   &
      ips, ipe, jps, jpe, kps, MAX(1,1&
,kpe &
))
DO itrace = PARAM_FIRST_SCALAR, num_tracer
 IF ( SIZE(tracer,1)*SIZE(tracer,3) .GT. 1 ) THEN
  CALL RSL_LITE_PACK ( local_communicator,&
tracer ( grid%sm31,grid%sm32,grid%sm33,itrace),4,&
rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p, &
rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p, &
4, 0, 0, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
 ENDIF
ENDDO
   CALL RSL_LITE_EXCH_Y ( local_communicator , mytask, ntasks, ntasks_x, ntasks_y, &
                          rsl_sendw_m,  rsl_sendw_p, rsl_recvw_m,  rsl_recvw_p    )
DO itrace = PARAM_FIRST_SCALAR, num_tracer
 IF ( SIZE(tracer,1)*SIZE(tracer,3) .GT. 1 ) THEN
  CALL RSL_LITE_PACK ( local_communicator,&
tracer ( grid%sm31,grid%sm32,grid%sm33,itrace),4,&
rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p, &
rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p, &
4, 0, 1, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
 ENDIF
ENDDO
ENDDO
CALL rsl_comm_iter_init(4,ips,ipe)
DO WHILE ( rsl_comm_iter( grid%id , grid%is_intermediate, 4 , &
                         1 , ids,ide,ips,ipe, grid%nids, grid%nide , & 
     rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p,   & 
     rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p    ))
 CALL RSL_LITE_INIT_EXCH ( local_communicator, 4, 1, &
     rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p,   & 
     rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p,   & 
     0  &
   + num_tracer   &
     , 0, 4, &
     0, 0, 4, &
     0, 0, 8, &
      0,  0, 4, &
      mytask, ntasks, ntasks_x, ntasks_y,   &
      ips, ipe, jps, jpe, kps, MAX(1,1&
,kpe &
))
DO itrace = PARAM_FIRST_SCALAR, num_tracer
 IF ( SIZE(tracer,1)*SIZE(tracer,3) .GT. 1 ) THEN
  CALL RSL_LITE_PACK ( local_communicator,&
tracer ( grid%sm31,grid%sm32,grid%sm33,itrace),4,&
rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p, &
rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p, &
4, 1, 0, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
 ENDIF
ENDDO
   CALL RSL_LITE_EXCH_X ( local_communicator , mytask, ntasks, ntasks_x, ntasks_y, &
                          rsl_sendw_m,  rsl_sendw_p, rsl_recvw_m,  rsl_recvw_p    )
DO itrace = PARAM_FIRST_SCALAR, num_tracer
 IF ( SIZE(tracer,1)*SIZE(tracer,3) .GT. 1 ) THEN
  CALL RSL_LITE_PACK ( local_communicator,&
tracer ( grid%sm31,grid%sm32,grid%sm33,itrace),4,&
rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p, &
rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p, &
4, 1, 1, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
 ENDIF
ENDDO
    ENDDO

  
  END SUBROUTINE HALO_EM_TRACER_E_7_sub







SUBROUTINE HALO_EM_SCALAR_E_3_sub ( grid, &
  num_scalar, &
  scalar, &
  local_communicator, &
  mytask, ntasks, ntasks_x, ntasks_y, &
  ids, ide, jds, jde, kds, kde,       &
  ims, ime, jms, jme, kms, kme,       &
  ips, ipe, jps, jpe, kps, kpe )
  USE module_domain, ONLY:domain
  USE module_configure, ONLY:grid_config_rec_type,in_use_for_config
  USE module_state_description, ONLY:PARAM_FIRST_SCALAR
  USE module_driver_constants
  TYPE(domain) ,               INTENT(IN) :: grid
  INTEGER, INTENT(IN) :: num_scalar
  real, INTENT(INOUT) :: scalar ( grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33,num_scalar)
  INTEGER ,                    INTENT(IN) :: local_communicator
  INTEGER ,                    INTENT(IN) :: mytask, ntasks, ntasks_x, ntasks_y
  INTEGER ,                    INTENT(IN) :: ids, ide, jds, jde, kds, kde
  INTEGER ,                    INTENT(IN) :: ims, ime, jms, jme, kms, kme
  INTEGER ,                    INTENT(IN) :: ips, ipe, jps, jpe, kps, kpe
  INTEGER :: itrace
  INTEGER :: rsl_sendw_p, rsl_sendbeg_p, rsl_recvw_p, rsl_recvbeg_p
  INTEGER :: rsl_sendw_m, rsl_sendbeg_m, rsl_recvw_m, rsl_recvbeg_m
  LOGICAL, EXTERNAL :: rsl_comm_iter
  INTEGER :: idim1, idim2, idim3, idim4, idim5, idim6, idim7
  






CALL wrf_debug(2,'calling inc/HALO_EM_SCALAR_E_3_inline.inc')
CALL rsl_comm_iter_init(2,jps,jpe)
DO WHILE ( rsl_comm_iter( grid%id , grid%is_intermediate, 2 , &
                         0 , jds,jde,jps,jpe, grid%njds, grid%njde , & 
     rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p,   & 
     rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p    ))
 CALL RSL_LITE_INIT_EXCH ( local_communicator, 2, 0, &
     rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p,   & 
     rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p,   & 
     0  &
   + num_scalar   &
     , 0, 4, &
     0, 0, 4, &
     0, 0, 8, &
      0,  0, 4, &
      mytask, ntasks, ntasks_x, ntasks_y,   &
      ips, ipe, jps, jpe, kps, MAX(1,1&
,kpe &
))
DO itrace = PARAM_FIRST_SCALAR, num_scalar
 IF ( SIZE(scalar,1)*SIZE(scalar,3) .GT. 1 ) THEN
  CALL RSL_LITE_PACK ( local_communicator,&
scalar ( grid%sm31,grid%sm32,grid%sm33,itrace),2,&
rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p, &
rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p, &
4, 0, 0, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
 ENDIF
ENDDO
   CALL RSL_LITE_EXCH_Y ( local_communicator , mytask, ntasks, ntasks_x, ntasks_y, &
                          rsl_sendw_m,  rsl_sendw_p, rsl_recvw_m,  rsl_recvw_p    )
DO itrace = PARAM_FIRST_SCALAR, num_scalar
 IF ( SIZE(scalar,1)*SIZE(scalar,3) .GT. 1 ) THEN
  CALL RSL_LITE_PACK ( local_communicator,&
scalar ( grid%sm31,grid%sm32,grid%sm33,itrace),2,&
rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p, &
rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p, &
4, 0, 1, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
 ENDIF
ENDDO
ENDDO
CALL rsl_comm_iter_init(2,ips,ipe)
DO WHILE ( rsl_comm_iter( grid%id , grid%is_intermediate, 2 , &
                         1 , ids,ide,ips,ipe, grid%nids, grid%nide , & 
     rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p,   & 
     rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p    ))
 CALL RSL_LITE_INIT_EXCH ( local_communicator, 2, 1, &
     rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p,   & 
     rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p,   & 
     0  &
   + num_scalar   &
     , 0, 4, &
     0, 0, 4, &
     0, 0, 8, &
      0,  0, 4, &
      mytask, ntasks, ntasks_x, ntasks_y,   &
      ips, ipe, jps, jpe, kps, MAX(1,1&
,kpe &
))
DO itrace = PARAM_FIRST_SCALAR, num_scalar
 IF ( SIZE(scalar,1)*SIZE(scalar,3) .GT. 1 ) THEN
  CALL RSL_LITE_PACK ( local_communicator,&
scalar ( grid%sm31,grid%sm32,grid%sm33,itrace),2,&
rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p, &
rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p, &
4, 1, 0, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
 ENDIF
ENDDO
   CALL RSL_LITE_EXCH_X ( local_communicator , mytask, ntasks, ntasks_x, ntasks_y, &
                          rsl_sendw_m,  rsl_sendw_p, rsl_recvw_m,  rsl_recvw_p    )
DO itrace = PARAM_FIRST_SCALAR, num_scalar
 IF ( SIZE(scalar,1)*SIZE(scalar,3) .GT. 1 ) THEN
  CALL RSL_LITE_PACK ( local_communicator,&
scalar ( grid%sm31,grid%sm32,grid%sm33,itrace),2,&
rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p, &
rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p, &
4, 1, 1, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
 ENDIF
ENDDO
    ENDDO

  
  END SUBROUTINE HALO_EM_SCALAR_E_3_sub







SUBROUTINE HALO_EM_SCALAR_E_5_sub ( grid, &
  num_scalar, &
  scalar, &
  local_communicator, &
  mytask, ntasks, ntasks_x, ntasks_y, &
  ids, ide, jds, jde, kds, kde,       &
  ims, ime, jms, jme, kms, kme,       &
  ips, ipe, jps, jpe, kps, kpe )
  USE module_domain, ONLY:domain
  USE module_configure, ONLY:grid_config_rec_type,in_use_for_config
  USE module_state_description, ONLY:PARAM_FIRST_SCALAR
  USE module_driver_constants
  TYPE(domain) ,               INTENT(IN) :: grid
  INTEGER, INTENT(IN) :: num_scalar
  real, INTENT(INOUT) :: scalar ( grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33,num_scalar)
  INTEGER ,                    INTENT(IN) :: local_communicator
  INTEGER ,                    INTENT(IN) :: mytask, ntasks, ntasks_x, ntasks_y
  INTEGER ,                    INTENT(IN) :: ids, ide, jds, jde, kds, kde
  INTEGER ,                    INTENT(IN) :: ims, ime, jms, jme, kms, kme
  INTEGER ,                    INTENT(IN) :: ips, ipe, jps, jpe, kps, kpe
  INTEGER :: itrace
  INTEGER :: rsl_sendw_p, rsl_sendbeg_p, rsl_recvw_p, rsl_recvbeg_p
  INTEGER :: rsl_sendw_m, rsl_sendbeg_m, rsl_recvw_m, rsl_recvbeg_m
  LOGICAL, EXTERNAL :: rsl_comm_iter
  INTEGER :: idim1, idim2, idim3, idim4, idim5, idim6, idim7
  






CALL wrf_debug(2,'calling inc/HALO_EM_SCALAR_E_5_inline.inc')
CALL rsl_comm_iter_init(3,jps,jpe)
DO WHILE ( rsl_comm_iter( grid%id , grid%is_intermediate, 3 , &
                         0 , jds,jde,jps,jpe, grid%njds, grid%njde , & 
     rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p,   & 
     rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p    ))
 CALL RSL_LITE_INIT_EXCH ( local_communicator, 3, 0, &
     rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p,   & 
     rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p,   & 
     0  &
   + num_scalar   &
     , 0, 4, &
     0, 0, 4, &
     0, 0, 8, &
      0,  0, 4, &
      mytask, ntasks, ntasks_x, ntasks_y,   &
      ips, ipe, jps, jpe, kps, MAX(1,1&
,kpe &
))
DO itrace = PARAM_FIRST_SCALAR, num_scalar
 IF ( SIZE(scalar,1)*SIZE(scalar,3) .GT. 1 ) THEN
  CALL RSL_LITE_PACK ( local_communicator,&
scalar ( grid%sm31,grid%sm32,grid%sm33,itrace),3,&
rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p, &
rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p, &
4, 0, 0, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
 ENDIF
ENDDO
   CALL RSL_LITE_EXCH_Y ( local_communicator , mytask, ntasks, ntasks_x, ntasks_y, &
                          rsl_sendw_m,  rsl_sendw_p, rsl_recvw_m,  rsl_recvw_p    )
DO itrace = PARAM_FIRST_SCALAR, num_scalar
 IF ( SIZE(scalar,1)*SIZE(scalar,3) .GT. 1 ) THEN
  CALL RSL_LITE_PACK ( local_communicator,&
scalar ( grid%sm31,grid%sm32,grid%sm33,itrace),3,&
rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p, &
rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p, &
4, 0, 1, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
 ENDIF
ENDDO
ENDDO
CALL rsl_comm_iter_init(3,ips,ipe)
DO WHILE ( rsl_comm_iter( grid%id , grid%is_intermediate, 3 , &
                         1 , ids,ide,ips,ipe, grid%nids, grid%nide , & 
     rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p,   & 
     rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p    ))
 CALL RSL_LITE_INIT_EXCH ( local_communicator, 3, 1, &
     rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p,   & 
     rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p,   & 
     0  &
   + num_scalar   &
     , 0, 4, &
     0, 0, 4, &
     0, 0, 8, &
      0,  0, 4, &
      mytask, ntasks, ntasks_x, ntasks_y,   &
      ips, ipe, jps, jpe, kps, MAX(1,1&
,kpe &
))
DO itrace = PARAM_FIRST_SCALAR, num_scalar
 IF ( SIZE(scalar,1)*SIZE(scalar,3) .GT. 1 ) THEN
  CALL RSL_LITE_PACK ( local_communicator,&
scalar ( grid%sm31,grid%sm32,grid%sm33,itrace),3,&
rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p, &
rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p, &
4, 1, 0, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
 ENDIF
ENDDO
   CALL RSL_LITE_EXCH_X ( local_communicator , mytask, ntasks, ntasks_x, ntasks_y, &
                          rsl_sendw_m,  rsl_sendw_p, rsl_recvw_m,  rsl_recvw_p    )
DO itrace = PARAM_FIRST_SCALAR, num_scalar
 IF ( SIZE(scalar,1)*SIZE(scalar,3) .GT. 1 ) THEN
  CALL RSL_LITE_PACK ( local_communicator,&
scalar ( grid%sm31,grid%sm32,grid%sm33,itrace),3,&
rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p, &
rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p, &
4, 1, 1, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
 ENDIF
ENDDO
    ENDDO

  
  END SUBROUTINE HALO_EM_SCALAR_E_5_sub







SUBROUTINE HALO_EM_SCALAR_E_7_sub ( grid, &
  num_scalar, &
  scalar, &
  local_communicator, &
  mytask, ntasks, ntasks_x, ntasks_y, &
  ids, ide, jds, jde, kds, kde,       &
  ims, ime, jms, jme, kms, kme,       &
  ips, ipe, jps, jpe, kps, kpe )
  USE module_domain, ONLY:domain
  USE module_configure, ONLY:grid_config_rec_type,in_use_for_config
  USE module_state_description, ONLY:PARAM_FIRST_SCALAR
  USE module_driver_constants
  TYPE(domain) ,               INTENT(IN) :: grid
  INTEGER, INTENT(IN) :: num_scalar
  real, INTENT(INOUT) :: scalar ( grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33,num_scalar)
  INTEGER ,                    INTENT(IN) :: local_communicator
  INTEGER ,                    INTENT(IN) :: mytask, ntasks, ntasks_x, ntasks_y
  INTEGER ,                    INTENT(IN) :: ids, ide, jds, jde, kds, kde
  INTEGER ,                    INTENT(IN) :: ims, ime, jms, jme, kms, kme
  INTEGER ,                    INTENT(IN) :: ips, ipe, jps, jpe, kps, kpe
  INTEGER :: itrace
  INTEGER :: rsl_sendw_p, rsl_sendbeg_p, rsl_recvw_p, rsl_recvbeg_p
  INTEGER :: rsl_sendw_m, rsl_sendbeg_m, rsl_recvw_m, rsl_recvbeg_m
  LOGICAL, EXTERNAL :: rsl_comm_iter
  INTEGER :: idim1, idim2, idim3, idim4, idim5, idim6, idim7
  






CALL wrf_debug(2,'calling inc/HALO_EM_SCALAR_E_7_inline.inc')
CALL rsl_comm_iter_init(4,jps,jpe)
DO WHILE ( rsl_comm_iter( grid%id , grid%is_intermediate, 4 , &
                         0 , jds,jde,jps,jpe, grid%njds, grid%njde , & 
     rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p,   & 
     rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p    ))
 CALL RSL_LITE_INIT_EXCH ( local_communicator, 4, 0, &
     rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p,   & 
     rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p,   & 
     0  &
   + num_scalar   &
     , 0, 4, &
     0, 0, 4, &
     0, 0, 8, &
      0,  0, 4, &
      mytask, ntasks, ntasks_x, ntasks_y,   &
      ips, ipe, jps, jpe, kps, MAX(1,1&
,kpe &
))
DO itrace = PARAM_FIRST_SCALAR, num_scalar
 IF ( SIZE(scalar,1)*SIZE(scalar,3) .GT. 1 ) THEN
  CALL RSL_LITE_PACK ( local_communicator,&
scalar ( grid%sm31,grid%sm32,grid%sm33,itrace),4,&
rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p, &
rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p, &
4, 0, 0, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
 ENDIF
ENDDO
   CALL RSL_LITE_EXCH_Y ( local_communicator , mytask, ntasks, ntasks_x, ntasks_y, &
                          rsl_sendw_m,  rsl_sendw_p, rsl_recvw_m,  rsl_recvw_p    )
DO itrace = PARAM_FIRST_SCALAR, num_scalar
 IF ( SIZE(scalar,1)*SIZE(scalar,3) .GT. 1 ) THEN
  CALL RSL_LITE_PACK ( local_communicator,&
scalar ( grid%sm31,grid%sm32,grid%sm33,itrace),4,&
rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p, &
rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p, &
4, 0, 1, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
 ENDIF
ENDDO
ENDDO
CALL rsl_comm_iter_init(4,ips,ipe)
DO WHILE ( rsl_comm_iter( grid%id , grid%is_intermediate, 4 , &
                         1 , ids,ide,ips,ipe, grid%nids, grid%nide , & 
     rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p,   & 
     rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p    ))
 CALL RSL_LITE_INIT_EXCH ( local_communicator, 4, 1, &
     rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p,   & 
     rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p,   & 
     0  &
   + num_scalar   &
     , 0, 4, &
     0, 0, 4, &
     0, 0, 8, &
      0,  0, 4, &
      mytask, ntasks, ntasks_x, ntasks_y,   &
      ips, ipe, jps, jpe, kps, MAX(1,1&
,kpe &
))
DO itrace = PARAM_FIRST_SCALAR, num_scalar
 IF ( SIZE(scalar,1)*SIZE(scalar,3) .GT. 1 ) THEN
  CALL RSL_LITE_PACK ( local_communicator,&
scalar ( grid%sm31,grid%sm32,grid%sm33,itrace),4,&
rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p, &
rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p, &
4, 1, 0, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
 ENDIF
ENDDO
   CALL RSL_LITE_EXCH_X ( local_communicator , mytask, ntasks, ntasks_x, ntasks_y, &
                          rsl_sendw_m,  rsl_sendw_p, rsl_recvw_m,  rsl_recvw_p    )
DO itrace = PARAM_FIRST_SCALAR, num_scalar
 IF ( SIZE(scalar,1)*SIZE(scalar,3) .GT. 1 ) THEN
  CALL RSL_LITE_PACK ( local_communicator,&
scalar ( grid%sm31,grid%sm32,grid%sm33,itrace),4,&
rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p, &
rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p, &
4, 1, 1, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
 ENDIF
ENDDO
    ENDDO

  
  END SUBROUTINE HALO_EM_SCALAR_E_7_sub







SUBROUTINE HALO_TOPOSHAD_sub ( grid, &
  local_communicator, &
  mytask, ntasks, ntasks_x, ntasks_y, &
  ids, ide, jds, jde, kds, kde,       &
  ims, ime, jms, jme, kms, kme,       &
  ips, ipe, jps, jpe, kps, kpe )
  USE module_domain, ONLY:domain
  USE module_configure, ONLY:grid_config_rec_type,in_use_for_config
  USE module_state_description, ONLY:PARAM_FIRST_SCALAR
  USE module_driver_constants
  TYPE(domain) ,               INTENT(IN) :: grid
  INTEGER ,                    INTENT(IN) :: local_communicator
  INTEGER ,                    INTENT(IN) :: mytask, ntasks, ntasks_x, ntasks_y
  INTEGER ,                    INTENT(IN) :: ids, ide, jds, jde, kds, kde
  INTEGER ,                    INTENT(IN) :: ims, ime, jms, jme, kms, kme
  INTEGER ,                    INTENT(IN) :: ips, ipe, jps, jpe, kps, kpe
  INTEGER :: itrace
  INTEGER :: rsl_sendw_p, rsl_sendbeg_p, rsl_recvw_p, rsl_recvbeg_p
  INTEGER :: rsl_sendw_m, rsl_sendbeg_m, rsl_recvw_m, rsl_recvbeg_m
  LOGICAL, EXTERNAL :: rsl_comm_iter
  INTEGER :: idim1, idim2, idim3, idim4, idim5, idim6, idim7
  






CALL wrf_debug(2,'calling inc/HALO_TOPOSHAD_inline.inc')
CALL rsl_comm_iter_init(2,jps,jpe)
DO WHILE ( rsl_comm_iter( grid%id , grid%is_intermediate, 2 , &
                         0 , jds,jde,jps,jpe, grid%njds, grid%njde , & 
     rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p,   & 
     rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p    ))
 CALL RSL_LITE_INIT_EXCH ( local_communicator, 2, 0, &
     rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p,   & 
     rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p,   & 
     0, 1, 4, &
     0, 0, 4, &
     0, 0, 8, &
      0,  0, 4, &
      mytask, ntasks, ntasks_x, ntasks_y,   &
      ips, ipe, jps, jpe, kps, MAX(1,1&
))
IF ( SIZE(grid%ht_shad,1)*SIZE(grid%ht_shad,2) .GT. 1 ) THEN
CALL RSL_LITE_PACK ( local_communicator,&
 grid%ht_shad, 2,&
rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p, &
rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p, &
4, 0, 0, DATA_ORDER_XY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
ENDIF
   CALL RSL_LITE_EXCH_Y ( local_communicator , mytask, ntasks, ntasks_x, ntasks_y, &
                          rsl_sendw_m,  rsl_sendw_p, rsl_recvw_m,  rsl_recvw_p    )
IF ( SIZE(grid%ht_shad,1)*SIZE(grid%ht_shad,2) .GT. 1 ) THEN
CALL RSL_LITE_PACK ( local_communicator,&
 grid%ht_shad, 2,&
rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p, &
rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p, &
4, 0, 1, DATA_ORDER_XY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
ENDIF
ENDDO
CALL rsl_comm_iter_init(2,ips,ipe)
DO WHILE ( rsl_comm_iter( grid%id , grid%is_intermediate, 2 , &
                         1 , ids,ide,ips,ipe, grid%nids, grid%nide , & 
     rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p,   & 
     rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p    ))
 CALL RSL_LITE_INIT_EXCH ( local_communicator, 2, 1, &
     rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p,   & 
     rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p,   & 
     0, 1, 4, &
     0, 0, 4, &
     0, 0, 8, &
      0,  0, 4, &
      mytask, ntasks, ntasks_x, ntasks_y,   &
      ips, ipe, jps, jpe, kps, MAX(1,1&
))
IF ( SIZE(grid%ht_shad,1)*SIZE(grid%ht_shad,2) .GT. 1 ) THEN
CALL RSL_LITE_PACK ( local_communicator,&
 grid%ht_shad, 2,&
rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p, &
rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p, &
4, 1, 0, DATA_ORDER_XY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
ENDIF
   CALL RSL_LITE_EXCH_X ( local_communicator , mytask, ntasks, ntasks_x, ntasks_y, &
                          rsl_sendw_m,  rsl_sendw_p, rsl_recvw_m,  rsl_recvw_p    )
IF ( SIZE(grid%ht_shad,1)*SIZE(grid%ht_shad,2) .GT. 1 ) THEN
CALL RSL_LITE_PACK ( local_communicator,&
 grid%ht_shad, 2,&
rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p, &
rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p, &
4, 1, 1, DATA_ORDER_XY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
ENDIF
    ENDDO

  
  END SUBROUTINE HALO_TOPOSHAD_sub







SUBROUTINE HALO_EM_HORIZ_INTERP_sub ( grid, &
  local_communicator, &
  mytask, ntasks, ntasks_x, ntasks_y, &
  ids, ide, jds, jde, kds, kde,       &
  ims, ime, jms, jme, kms, kme,       &
  ips, ipe, jps, jpe, kps, kpe )
  USE module_domain, ONLY:domain
  USE module_configure, ONLY:grid_config_rec_type,in_use_for_config
  USE module_state_description, ONLY:PARAM_FIRST_SCALAR
  USE module_driver_constants
  TYPE(domain) ,               INTENT(IN) :: grid
  INTEGER ,                    INTENT(IN) :: local_communicator
  INTEGER ,                    INTENT(IN) :: mytask, ntasks, ntasks_x, ntasks_y
  INTEGER ,                    INTENT(IN) :: ids, ide, jds, jde, kds, kde
  INTEGER ,                    INTENT(IN) :: ims, ime, jms, jme, kms, kme
  INTEGER ,                    INTENT(IN) :: ips, ipe, jps, jpe, kps, kpe
  INTEGER :: itrace
  INTEGER :: rsl_sendw_p, rsl_sendbeg_p, rsl_recvw_p, rsl_recvbeg_p
  INTEGER :: rsl_sendw_m, rsl_sendbeg_m, rsl_recvw_m, rsl_recvbeg_m
  LOGICAL, EXTERNAL :: rsl_comm_iter
  INTEGER :: idim1, idim2, idim3, idim4, idim5, idim6, idim7
  






CALL wrf_debug(2,'calling inc/HALO_EM_HORIZ_INTERP_inline.inc')
CALL rsl_comm_iter_init(2,jps,jpe)
DO WHILE ( rsl_comm_iter( grid%id , grid%is_intermediate, 2 , &
                         0 , jds,jde,jps,jpe, grid%njds, grid%njde , & 
     rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p,   & 
     rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p    ))
 CALL RSL_LITE_INIT_EXCH ( local_communicator, 2, 0, &
     rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p,   & 
     rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p,   & 
     2, 7, 4, &
     0, 0, 4, &
     0, 0, 8, &
      0,  0, 4, &
      mytask, ntasks, ntasks_x, ntasks_y,   &
      ips, ipe, jps, jpe, kps, MAX(1,1&
,kpe &
))
IF ( SIZE(grid%t_2,1)*SIZE(grid%t_2,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK ( local_communicator,&
 grid%t_2, 2,&
rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p, &
rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p, &
4, 0, 0, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%ph_2,1)*SIZE(grid%ph_2,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK ( local_communicator,&
 grid%ph_2, 2,&
rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p, &
rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p, &
4, 0, 0, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%ht,1)*SIZE(grid%ht,2) .GT. 1 ) THEN
CALL RSL_LITE_PACK ( local_communicator,&
 grid%ht, 2,&
rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p, &
rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p, &
4, 0, 0, DATA_ORDER_XY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
ENDIF
IF ( SIZE(grid%t_max_p,1)*SIZE(grid%t_max_p,2) .GT. 1 ) THEN
CALL RSL_LITE_PACK ( local_communicator,&
 grid%t_max_p, 2,&
rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p, &
rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p, &
4, 0, 0, DATA_ORDER_XY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
ENDIF
IF ( SIZE(grid%ght_max_p,1)*SIZE(grid%ght_max_p,2) .GT. 1 ) THEN
CALL RSL_LITE_PACK ( local_communicator,&
 grid%ght_max_p, 2,&
rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p, &
rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p, &
4, 0, 0, DATA_ORDER_XY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
ENDIF
IF ( SIZE(grid%max_p,1)*SIZE(grid%max_p,2) .GT. 1 ) THEN
CALL RSL_LITE_PACK ( local_communicator,&
 grid%max_p, 2,&
rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p, &
rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p, &
4, 0, 0, DATA_ORDER_XY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
ENDIF
IF ( SIZE(grid%t_min_p,1)*SIZE(grid%t_min_p,2) .GT. 1 ) THEN
CALL RSL_LITE_PACK ( local_communicator,&
 grid%t_min_p, 2,&
rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p, &
rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p, &
4, 0, 0, DATA_ORDER_XY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
ENDIF
IF ( SIZE(grid%ght_min_p,1)*SIZE(grid%ght_min_p,2) .GT. 1 ) THEN
CALL RSL_LITE_PACK ( local_communicator,&
 grid%ght_min_p, 2,&
rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p, &
rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p, &
4, 0, 0, DATA_ORDER_XY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
ENDIF
IF ( SIZE(grid%min_p,1)*SIZE(grid%min_p,2) .GT. 1 ) THEN
CALL RSL_LITE_PACK ( local_communicator,&
 grid%min_p, 2,&
rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p, &
rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p, &
4, 0, 0, DATA_ORDER_XY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
ENDIF
   CALL RSL_LITE_EXCH_Y ( local_communicator , mytask, ntasks, ntasks_x, ntasks_y, &
                          rsl_sendw_m,  rsl_sendw_p, rsl_recvw_m,  rsl_recvw_p    )
IF ( SIZE(grid%t_2,1)*SIZE(grid%t_2,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK ( local_communicator,&
 grid%t_2, 2,&
rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p, &
rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p, &
4, 0, 1, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%ph_2,1)*SIZE(grid%ph_2,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK ( local_communicator,&
 grid%ph_2, 2,&
rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p, &
rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p, &
4, 0, 1, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%ht,1)*SIZE(grid%ht,2) .GT. 1 ) THEN
CALL RSL_LITE_PACK ( local_communicator,&
 grid%ht, 2,&
rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p, &
rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p, &
4, 0, 1, DATA_ORDER_XY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
ENDIF
IF ( SIZE(grid%t_max_p,1)*SIZE(grid%t_max_p,2) .GT. 1 ) THEN
CALL RSL_LITE_PACK ( local_communicator,&
 grid%t_max_p, 2,&
rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p, &
rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p, &
4, 0, 1, DATA_ORDER_XY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
ENDIF
IF ( SIZE(grid%ght_max_p,1)*SIZE(grid%ght_max_p,2) .GT. 1 ) THEN
CALL RSL_LITE_PACK ( local_communicator,&
 grid%ght_max_p, 2,&
rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p, &
rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p, &
4, 0, 1, DATA_ORDER_XY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
ENDIF
IF ( SIZE(grid%max_p,1)*SIZE(grid%max_p,2) .GT. 1 ) THEN
CALL RSL_LITE_PACK ( local_communicator,&
 grid%max_p, 2,&
rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p, &
rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p, &
4, 0, 1, DATA_ORDER_XY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
ENDIF
IF ( SIZE(grid%t_min_p,1)*SIZE(grid%t_min_p,2) .GT. 1 ) THEN
CALL RSL_LITE_PACK ( local_communicator,&
 grid%t_min_p, 2,&
rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p, &
rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p, &
4, 0, 1, DATA_ORDER_XY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
ENDIF
IF ( SIZE(grid%ght_min_p,1)*SIZE(grid%ght_min_p,2) .GT. 1 ) THEN
CALL RSL_LITE_PACK ( local_communicator,&
 grid%ght_min_p, 2,&
rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p, &
rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p, &
4, 0, 1, DATA_ORDER_XY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
ENDIF
IF ( SIZE(grid%min_p,1)*SIZE(grid%min_p,2) .GT. 1 ) THEN
CALL RSL_LITE_PACK ( local_communicator,&
 grid%min_p, 2,&
rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p, &
rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p, &
4, 0, 1, DATA_ORDER_XY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
ENDIF
ENDDO
CALL rsl_comm_iter_init(2,ips,ipe)
DO WHILE ( rsl_comm_iter( grid%id , grid%is_intermediate, 2 , &
                         1 , ids,ide,ips,ipe, grid%nids, grid%nide , & 
     rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p,   & 
     rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p    ))
 CALL RSL_LITE_INIT_EXCH ( local_communicator, 2, 1, &
     rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p,   & 
     rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p,   & 
     2, 7, 4, &
     0, 0, 4, &
     0, 0, 8, &
      0,  0, 4, &
      mytask, ntasks, ntasks_x, ntasks_y,   &
      ips, ipe, jps, jpe, kps, MAX(1,1&
,kpe &
))
IF ( SIZE(grid%t_2,1)*SIZE(grid%t_2,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK ( local_communicator,&
 grid%t_2, 2,&
rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p, &
rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p, &
4, 1, 0, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%ph_2,1)*SIZE(grid%ph_2,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK ( local_communicator,&
 grid%ph_2, 2,&
rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p, &
rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p, &
4, 1, 0, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%ht,1)*SIZE(grid%ht,2) .GT. 1 ) THEN
CALL RSL_LITE_PACK ( local_communicator,&
 grid%ht, 2,&
rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p, &
rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p, &
4, 1, 0, DATA_ORDER_XY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
ENDIF
IF ( SIZE(grid%t_max_p,1)*SIZE(grid%t_max_p,2) .GT. 1 ) THEN
CALL RSL_LITE_PACK ( local_communicator,&
 grid%t_max_p, 2,&
rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p, &
rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p, &
4, 1, 0, DATA_ORDER_XY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
ENDIF
IF ( SIZE(grid%ght_max_p,1)*SIZE(grid%ght_max_p,2) .GT. 1 ) THEN
CALL RSL_LITE_PACK ( local_communicator,&
 grid%ght_max_p, 2,&
rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p, &
rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p, &
4, 1, 0, DATA_ORDER_XY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
ENDIF
IF ( SIZE(grid%max_p,1)*SIZE(grid%max_p,2) .GT. 1 ) THEN
CALL RSL_LITE_PACK ( local_communicator,&
 grid%max_p, 2,&
rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p, &
rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p, &
4, 1, 0, DATA_ORDER_XY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
ENDIF
IF ( SIZE(grid%t_min_p,1)*SIZE(grid%t_min_p,2) .GT. 1 ) THEN
CALL RSL_LITE_PACK ( local_communicator,&
 grid%t_min_p, 2,&
rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p, &
rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p, &
4, 1, 0, DATA_ORDER_XY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
ENDIF
IF ( SIZE(grid%ght_min_p,1)*SIZE(grid%ght_min_p,2) .GT. 1 ) THEN
CALL RSL_LITE_PACK ( local_communicator,&
 grid%ght_min_p, 2,&
rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p, &
rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p, &
4, 1, 0, DATA_ORDER_XY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
ENDIF
IF ( SIZE(grid%min_p,1)*SIZE(grid%min_p,2) .GT. 1 ) THEN
CALL RSL_LITE_PACK ( local_communicator,&
 grid%min_p, 2,&
rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p, &
rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p, &
4, 1, 0, DATA_ORDER_XY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
ENDIF
   CALL RSL_LITE_EXCH_X ( local_communicator , mytask, ntasks, ntasks_x, ntasks_y, &
                          rsl_sendw_m,  rsl_sendw_p, rsl_recvw_m,  rsl_recvw_p    )
IF ( SIZE(grid%t_2,1)*SIZE(grid%t_2,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK ( local_communicator,&
 grid%t_2, 2,&
rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p, &
rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p, &
4, 1, 1, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%ph_2,1)*SIZE(grid%ph_2,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK ( local_communicator,&
 grid%ph_2, 2,&
rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p, &
rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p, &
4, 1, 1, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%ht,1)*SIZE(grid%ht,2) .GT. 1 ) THEN
CALL RSL_LITE_PACK ( local_communicator,&
 grid%ht, 2,&
rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p, &
rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p, &
4, 1, 1, DATA_ORDER_XY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
ENDIF
IF ( SIZE(grid%t_max_p,1)*SIZE(grid%t_max_p,2) .GT. 1 ) THEN
CALL RSL_LITE_PACK ( local_communicator,&
 grid%t_max_p, 2,&
rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p, &
rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p, &
4, 1, 1, DATA_ORDER_XY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
ENDIF
IF ( SIZE(grid%ght_max_p,1)*SIZE(grid%ght_max_p,2) .GT. 1 ) THEN
CALL RSL_LITE_PACK ( local_communicator,&
 grid%ght_max_p, 2,&
rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p, &
rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p, &
4, 1, 1, DATA_ORDER_XY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
ENDIF
IF ( SIZE(grid%max_p,1)*SIZE(grid%max_p,2) .GT. 1 ) THEN
CALL RSL_LITE_PACK ( local_communicator,&
 grid%max_p, 2,&
rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p, &
rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p, &
4, 1, 1, DATA_ORDER_XY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
ENDIF
IF ( SIZE(grid%t_min_p,1)*SIZE(grid%t_min_p,2) .GT. 1 ) THEN
CALL RSL_LITE_PACK ( local_communicator,&
 grid%t_min_p, 2,&
rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p, &
rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p, &
4, 1, 1, DATA_ORDER_XY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
ENDIF
IF ( SIZE(grid%ght_min_p,1)*SIZE(grid%ght_min_p,2) .GT. 1 ) THEN
CALL RSL_LITE_PACK ( local_communicator,&
 grid%ght_min_p, 2,&
rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p, &
rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p, &
4, 1, 1, DATA_ORDER_XY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
ENDIF
IF ( SIZE(grid%min_p,1)*SIZE(grid%min_p,2) .GT. 1 ) THEN
CALL RSL_LITE_PACK ( local_communicator,&
 grid%min_p, 2,&
rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p, &
rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p, &
4, 1, 1, DATA_ORDER_XY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
ENDIF
    ENDDO

  
  END SUBROUTINE HALO_EM_HORIZ_INTERP_sub







SUBROUTINE HALO_EM_MOIST_OLD_E_3_sub ( grid, &
  num_moist, &
  moist_old, &
  local_communicator, &
  mytask, ntasks, ntasks_x, ntasks_y, &
  ids, ide, jds, jde, kds, kde,       &
  ims, ime, jms, jme, kms, kme,       &
  ips, ipe, jps, jpe, kps, kpe )
  USE module_domain, ONLY:domain
  USE module_configure, ONLY:grid_config_rec_type,in_use_for_config
  USE module_state_description, ONLY:PARAM_FIRST_SCALAR
  USE module_driver_constants
  TYPE(domain) ,               INTENT(IN) :: grid
  INTEGER, INTENT(IN) :: num_moist
  real, INTENT(INOUT) :: moist_old ( grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33,num_moist)
  INTEGER ,                    INTENT(IN) :: local_communicator
  INTEGER ,                    INTENT(IN) :: mytask, ntasks, ntasks_x, ntasks_y
  INTEGER ,                    INTENT(IN) :: ids, ide, jds, jde, kds, kde
  INTEGER ,                    INTENT(IN) :: ims, ime, jms, jme, kms, kme
  INTEGER ,                    INTENT(IN) :: ips, ipe, jps, jpe, kps, kpe
  INTEGER :: itrace
  INTEGER :: rsl_sendw_p, rsl_sendbeg_p, rsl_recvw_p, rsl_recvbeg_p
  INTEGER :: rsl_sendw_m, rsl_sendbeg_m, rsl_recvw_m, rsl_recvbeg_m
  LOGICAL, EXTERNAL :: rsl_comm_iter
  INTEGER :: idim1, idim2, idim3, idim4, idim5, idim6, idim7
  






CALL wrf_debug(2,'calling inc/HALO_EM_MOIST_OLD_E_3_inline.inc')
CALL rsl_comm_iter_init(2,jps,jpe)
DO WHILE ( rsl_comm_iter( grid%id , grid%is_intermediate, 2 , &
                         0 , jds,jde,jps,jpe, grid%njds, grid%njde , & 
     rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p,   & 
     rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p    ))
 CALL RSL_LITE_INIT_EXCH ( local_communicator, 2, 0, &
     rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p,   & 
     rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p,   & 
     0  &
   + num_moist   &
     , 0, 4, &
     0, 0, 4, &
     0, 0, 8, &
      0,  0, 4, &
      mytask, ntasks, ntasks_x, ntasks_y,   &
      ips, ipe, jps, jpe, kps, MAX(1,1&
,kpe &
))
DO itrace = PARAM_FIRST_SCALAR, num_moist
 IF ( SIZE(moist_old,1)*SIZE(moist_old,3) .GT. 1 ) THEN
  CALL RSL_LITE_PACK ( local_communicator,&
moist_old ( grid%sm31,grid%sm32,grid%sm33,itrace),2,&
rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p, &
rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p, &
4, 0, 0, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
 ENDIF
ENDDO
   CALL RSL_LITE_EXCH_Y ( local_communicator , mytask, ntasks, ntasks_x, ntasks_y, &
                          rsl_sendw_m,  rsl_sendw_p, rsl_recvw_m,  rsl_recvw_p    )
DO itrace = PARAM_FIRST_SCALAR, num_moist
 IF ( SIZE(moist_old,1)*SIZE(moist_old,3) .GT. 1 ) THEN
  CALL RSL_LITE_PACK ( local_communicator,&
moist_old ( grid%sm31,grid%sm32,grid%sm33,itrace),2,&
rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p, &
rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p, &
4, 0, 1, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
 ENDIF
ENDDO
ENDDO
CALL rsl_comm_iter_init(2,ips,ipe)
DO WHILE ( rsl_comm_iter( grid%id , grid%is_intermediate, 2 , &
                         1 , ids,ide,ips,ipe, grid%nids, grid%nide , & 
     rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p,   & 
     rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p    ))
 CALL RSL_LITE_INIT_EXCH ( local_communicator, 2, 1, &
     rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p,   & 
     rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p,   & 
     0  &
   + num_moist   &
     , 0, 4, &
     0, 0, 4, &
     0, 0, 8, &
      0,  0, 4, &
      mytask, ntasks, ntasks_x, ntasks_y,   &
      ips, ipe, jps, jpe, kps, MAX(1,1&
,kpe &
))
DO itrace = PARAM_FIRST_SCALAR, num_moist
 IF ( SIZE(moist_old,1)*SIZE(moist_old,3) .GT. 1 ) THEN
  CALL RSL_LITE_PACK ( local_communicator,&
moist_old ( grid%sm31,grid%sm32,grid%sm33,itrace),2,&
rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p, &
rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p, &
4, 1, 0, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
 ENDIF
ENDDO
   CALL RSL_LITE_EXCH_X ( local_communicator , mytask, ntasks, ntasks_x, ntasks_y, &
                          rsl_sendw_m,  rsl_sendw_p, rsl_recvw_m,  rsl_recvw_p    )
DO itrace = PARAM_FIRST_SCALAR, num_moist
 IF ( SIZE(moist_old,1)*SIZE(moist_old,3) .GT. 1 ) THEN
  CALL RSL_LITE_PACK ( local_communicator,&
moist_old ( grid%sm31,grid%sm32,grid%sm33,itrace),2,&
rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p, &
rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p, &
4, 1, 1, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
 ENDIF
ENDDO
    ENDDO

  
  END SUBROUTINE HALO_EM_MOIST_OLD_E_3_sub







SUBROUTINE HALO_EM_MOIST_OLD_E_5_sub ( grid, &
  num_moist, &
  moist_old, &
  local_communicator, &
  mytask, ntasks, ntasks_x, ntasks_y, &
  ids, ide, jds, jde, kds, kde,       &
  ims, ime, jms, jme, kms, kme,       &
  ips, ipe, jps, jpe, kps, kpe )
  USE module_domain, ONLY:domain
  USE module_configure, ONLY:grid_config_rec_type,in_use_for_config
  USE module_state_description, ONLY:PARAM_FIRST_SCALAR
  USE module_driver_constants
  TYPE(domain) ,               INTENT(IN) :: grid
  INTEGER, INTENT(IN) :: num_moist
  real, INTENT(INOUT) :: moist_old ( grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33,num_moist)
  INTEGER ,                    INTENT(IN) :: local_communicator
  INTEGER ,                    INTENT(IN) :: mytask, ntasks, ntasks_x, ntasks_y
  INTEGER ,                    INTENT(IN) :: ids, ide, jds, jde, kds, kde
  INTEGER ,                    INTENT(IN) :: ims, ime, jms, jme, kms, kme
  INTEGER ,                    INTENT(IN) :: ips, ipe, jps, jpe, kps, kpe
  INTEGER :: itrace
  INTEGER :: rsl_sendw_p, rsl_sendbeg_p, rsl_recvw_p, rsl_recvbeg_p
  INTEGER :: rsl_sendw_m, rsl_sendbeg_m, rsl_recvw_m, rsl_recvbeg_m
  LOGICAL, EXTERNAL :: rsl_comm_iter
  INTEGER :: idim1, idim2, idim3, idim4, idim5, idim6, idim7
  






CALL wrf_debug(2,'calling inc/HALO_EM_MOIST_OLD_E_5_inline.inc')
CALL rsl_comm_iter_init(3,jps,jpe)
DO WHILE ( rsl_comm_iter( grid%id , grid%is_intermediate, 3 , &
                         0 , jds,jde,jps,jpe, grid%njds, grid%njde , & 
     rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p,   & 
     rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p    ))
 CALL RSL_LITE_INIT_EXCH ( local_communicator, 3, 0, &
     rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p,   & 
     rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p,   & 
     0  &
   + num_moist   &
     , 0, 4, &
     0, 0, 4, &
     0, 0, 8, &
      0,  0, 4, &
      mytask, ntasks, ntasks_x, ntasks_y,   &
      ips, ipe, jps, jpe, kps, MAX(1,1&
,kpe &
))
DO itrace = PARAM_FIRST_SCALAR, num_moist
 IF ( SIZE(moist_old,1)*SIZE(moist_old,3) .GT. 1 ) THEN
  CALL RSL_LITE_PACK ( local_communicator,&
moist_old ( grid%sm31,grid%sm32,grid%sm33,itrace),3,&
rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p, &
rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p, &
4, 0, 0, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
 ENDIF
ENDDO
   CALL RSL_LITE_EXCH_Y ( local_communicator , mytask, ntasks, ntasks_x, ntasks_y, &
                          rsl_sendw_m,  rsl_sendw_p, rsl_recvw_m,  rsl_recvw_p    )
DO itrace = PARAM_FIRST_SCALAR, num_moist
 IF ( SIZE(moist_old,1)*SIZE(moist_old,3) .GT. 1 ) THEN
  CALL RSL_LITE_PACK ( local_communicator,&
moist_old ( grid%sm31,grid%sm32,grid%sm33,itrace),3,&
rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p, &
rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p, &
4, 0, 1, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
 ENDIF
ENDDO
ENDDO
CALL rsl_comm_iter_init(3,ips,ipe)
DO WHILE ( rsl_comm_iter( grid%id , grid%is_intermediate, 3 , &
                         1 , ids,ide,ips,ipe, grid%nids, grid%nide , & 
     rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p,   & 
     rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p    ))
 CALL RSL_LITE_INIT_EXCH ( local_communicator, 3, 1, &
     rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p,   & 
     rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p,   & 
     0  &
   + num_moist   &
     , 0, 4, &
     0, 0, 4, &
     0, 0, 8, &
      0,  0, 4, &
      mytask, ntasks, ntasks_x, ntasks_y,   &
      ips, ipe, jps, jpe, kps, MAX(1,1&
,kpe &
))
DO itrace = PARAM_FIRST_SCALAR, num_moist
 IF ( SIZE(moist_old,1)*SIZE(moist_old,3) .GT. 1 ) THEN
  CALL RSL_LITE_PACK ( local_communicator,&
moist_old ( grid%sm31,grid%sm32,grid%sm33,itrace),3,&
rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p, &
rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p, &
4, 1, 0, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
 ENDIF
ENDDO
   CALL RSL_LITE_EXCH_X ( local_communicator , mytask, ntasks, ntasks_x, ntasks_y, &
                          rsl_sendw_m,  rsl_sendw_p, rsl_recvw_m,  rsl_recvw_p    )
DO itrace = PARAM_FIRST_SCALAR, num_moist
 IF ( SIZE(moist_old,1)*SIZE(moist_old,3) .GT. 1 ) THEN
  CALL RSL_LITE_PACK ( local_communicator,&
moist_old ( grid%sm31,grid%sm32,grid%sm33,itrace),3,&
rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p, &
rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p, &
4, 1, 1, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
 ENDIF
ENDDO
    ENDDO

  
  END SUBROUTINE HALO_EM_MOIST_OLD_E_5_sub







SUBROUTINE HALO_EM_MOIST_OLD_E_7_sub ( grid, &
  num_moist, &
  moist_old, &
  local_communicator, &
  mytask, ntasks, ntasks_x, ntasks_y, &
  ids, ide, jds, jde, kds, kde,       &
  ims, ime, jms, jme, kms, kme,       &
  ips, ipe, jps, jpe, kps, kpe )
  USE module_domain, ONLY:domain
  USE module_configure, ONLY:grid_config_rec_type,in_use_for_config
  USE module_state_description, ONLY:PARAM_FIRST_SCALAR
  USE module_driver_constants
  TYPE(domain) ,               INTENT(IN) :: grid
  INTEGER, INTENT(IN) :: num_moist
  real, INTENT(INOUT) :: moist_old ( grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33,num_moist)
  INTEGER ,                    INTENT(IN) :: local_communicator
  INTEGER ,                    INTENT(IN) :: mytask, ntasks, ntasks_x, ntasks_y
  INTEGER ,                    INTENT(IN) :: ids, ide, jds, jde, kds, kde
  INTEGER ,                    INTENT(IN) :: ims, ime, jms, jme, kms, kme
  INTEGER ,                    INTENT(IN) :: ips, ipe, jps, jpe, kps, kpe
  INTEGER :: itrace
  INTEGER :: rsl_sendw_p, rsl_sendbeg_p, rsl_recvw_p, rsl_recvbeg_p
  INTEGER :: rsl_sendw_m, rsl_sendbeg_m, rsl_recvw_m, rsl_recvbeg_m
  LOGICAL, EXTERNAL :: rsl_comm_iter
  INTEGER :: idim1, idim2, idim3, idim4, idim5, idim6, idim7
  






CALL wrf_debug(2,'calling inc/HALO_EM_MOIST_OLD_E_7_inline.inc')
CALL rsl_comm_iter_init(4,jps,jpe)
DO WHILE ( rsl_comm_iter( grid%id , grid%is_intermediate, 4 , &
                         0 , jds,jde,jps,jpe, grid%njds, grid%njde , & 
     rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p,   & 
     rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p    ))
 CALL RSL_LITE_INIT_EXCH ( local_communicator, 4, 0, &
     rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p,   & 
     rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p,   & 
     0  &
   + num_moist   &
     , 0, 4, &
     0, 0, 4, &
     0, 0, 8, &
      0,  0, 4, &
      mytask, ntasks, ntasks_x, ntasks_y,   &
      ips, ipe, jps, jpe, kps, MAX(1,1&
,kpe &
))
DO itrace = PARAM_FIRST_SCALAR, num_moist
 IF ( SIZE(moist_old,1)*SIZE(moist_old,3) .GT. 1 ) THEN
  CALL RSL_LITE_PACK ( local_communicator,&
moist_old ( grid%sm31,grid%sm32,grid%sm33,itrace),4,&
rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p, &
rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p, &
4, 0, 0, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
 ENDIF
ENDDO
   CALL RSL_LITE_EXCH_Y ( local_communicator , mytask, ntasks, ntasks_x, ntasks_y, &
                          rsl_sendw_m,  rsl_sendw_p, rsl_recvw_m,  rsl_recvw_p    )
DO itrace = PARAM_FIRST_SCALAR, num_moist
 IF ( SIZE(moist_old,1)*SIZE(moist_old,3) .GT. 1 ) THEN
  CALL RSL_LITE_PACK ( local_communicator,&
moist_old ( grid%sm31,grid%sm32,grid%sm33,itrace),4,&
rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p, &
rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p, &
4, 0, 1, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
 ENDIF
ENDDO
ENDDO
CALL rsl_comm_iter_init(4,ips,ipe)
DO WHILE ( rsl_comm_iter( grid%id , grid%is_intermediate, 4 , &
                         1 , ids,ide,ips,ipe, grid%nids, grid%nide , & 
     rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p,   & 
     rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p    ))
 CALL RSL_LITE_INIT_EXCH ( local_communicator, 4, 1, &
     rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p,   & 
     rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p,   & 
     0  &
   + num_moist   &
     , 0, 4, &
     0, 0, 4, &
     0, 0, 8, &
      0,  0, 4, &
      mytask, ntasks, ntasks_x, ntasks_y,   &
      ips, ipe, jps, jpe, kps, MAX(1,1&
,kpe &
))
DO itrace = PARAM_FIRST_SCALAR, num_moist
 IF ( SIZE(moist_old,1)*SIZE(moist_old,3) .GT. 1 ) THEN
  CALL RSL_LITE_PACK ( local_communicator,&
moist_old ( grid%sm31,grid%sm32,grid%sm33,itrace),4,&
rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p, &
rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p, &
4, 1, 0, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
 ENDIF
ENDDO
   CALL RSL_LITE_EXCH_X ( local_communicator , mytask, ntasks, ntasks_x, ntasks_y, &
                          rsl_sendw_m,  rsl_sendw_p, rsl_recvw_m,  rsl_recvw_p    )
DO itrace = PARAM_FIRST_SCALAR, num_moist
 IF ( SIZE(moist_old,1)*SIZE(moist_old,3) .GT. 1 ) THEN
  CALL RSL_LITE_PACK ( local_communicator,&
moist_old ( grid%sm31,grid%sm32,grid%sm33,itrace),4,&
rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p, &
rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p, &
4, 1, 1, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
 ENDIF
ENDDO
    ENDDO

  
  END SUBROUTINE HALO_EM_MOIST_OLD_E_7_sub







SUBROUTINE HALO_EM_CHEM_OLD_E_3_sub ( grid, &
  num_chem, &
  chem_old, &
  local_communicator, &
  mytask, ntasks, ntasks_x, ntasks_y, &
  ids, ide, jds, jde, kds, kde,       &
  ims, ime, jms, jme, kms, kme,       &
  ips, ipe, jps, jpe, kps, kpe )
  USE module_domain, ONLY:domain
  USE module_configure, ONLY:grid_config_rec_type,in_use_for_config
  USE module_state_description, ONLY:PARAM_FIRST_SCALAR
  USE module_driver_constants
  TYPE(domain) ,               INTENT(IN) :: grid
  INTEGER, INTENT(IN) :: num_chem
  real, INTENT(INOUT) :: chem_old ( grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33,num_chem)
  INTEGER ,                    INTENT(IN) :: local_communicator
  INTEGER ,                    INTENT(IN) :: mytask, ntasks, ntasks_x, ntasks_y
  INTEGER ,                    INTENT(IN) :: ids, ide, jds, jde, kds, kde
  INTEGER ,                    INTENT(IN) :: ims, ime, jms, jme, kms, kme
  INTEGER ,                    INTENT(IN) :: ips, ipe, jps, jpe, kps, kpe
  INTEGER :: itrace
  INTEGER :: rsl_sendw_p, rsl_sendbeg_p, rsl_recvw_p, rsl_recvbeg_p
  INTEGER :: rsl_sendw_m, rsl_sendbeg_m, rsl_recvw_m, rsl_recvbeg_m
  LOGICAL, EXTERNAL :: rsl_comm_iter
  INTEGER :: idim1, idim2, idim3, idim4, idim5, idim6, idim7
  






CALL wrf_debug(2,'calling inc/HALO_EM_CHEM_OLD_E_3_inline.inc')
CALL rsl_comm_iter_init(2,jps,jpe)
DO WHILE ( rsl_comm_iter( grid%id , grid%is_intermediate, 2 , &
                         0 , jds,jde,jps,jpe, grid%njds, grid%njde , & 
     rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p,   & 
     rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p    ))
 CALL RSL_LITE_INIT_EXCH ( local_communicator, 2, 0, &
     rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p,   & 
     rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p,   & 
     0  &
   + num_chem   &
     , 0, 4, &
     0, 0, 4, &
     0, 0, 8, &
      0,  0, 4, &
      mytask, ntasks, ntasks_x, ntasks_y,   &
      ips, ipe, jps, jpe, kps, MAX(1,1&
,kpe &
))
DO itrace = PARAM_FIRST_SCALAR, num_chem
 IF ( SIZE(chem_old,1)*SIZE(chem_old,3) .GT. 1 ) THEN
  CALL RSL_LITE_PACK ( local_communicator,&
chem_old ( grid%sm31,grid%sm32,grid%sm33,itrace),2,&
rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p, &
rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p, &
4, 0, 0, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
 ENDIF
ENDDO
   CALL RSL_LITE_EXCH_Y ( local_communicator , mytask, ntasks, ntasks_x, ntasks_y, &
                          rsl_sendw_m,  rsl_sendw_p, rsl_recvw_m,  rsl_recvw_p    )
DO itrace = PARAM_FIRST_SCALAR, num_chem
 IF ( SIZE(chem_old,1)*SIZE(chem_old,3) .GT. 1 ) THEN
  CALL RSL_LITE_PACK ( local_communicator,&
chem_old ( grid%sm31,grid%sm32,grid%sm33,itrace),2,&
rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p, &
rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p, &
4, 0, 1, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
 ENDIF
ENDDO
ENDDO
CALL rsl_comm_iter_init(2,ips,ipe)
DO WHILE ( rsl_comm_iter( grid%id , grid%is_intermediate, 2 , &
                         1 , ids,ide,ips,ipe, grid%nids, grid%nide , & 
     rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p,   & 
     rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p    ))
 CALL RSL_LITE_INIT_EXCH ( local_communicator, 2, 1, &
     rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p,   & 
     rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p,   & 
     0  &
   + num_chem   &
     , 0, 4, &
     0, 0, 4, &
     0, 0, 8, &
      0,  0, 4, &
      mytask, ntasks, ntasks_x, ntasks_y,   &
      ips, ipe, jps, jpe, kps, MAX(1,1&
,kpe &
))
DO itrace = PARAM_FIRST_SCALAR, num_chem
 IF ( SIZE(chem_old,1)*SIZE(chem_old,3) .GT. 1 ) THEN
  CALL RSL_LITE_PACK ( local_communicator,&
chem_old ( grid%sm31,grid%sm32,grid%sm33,itrace),2,&
rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p, &
rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p, &
4, 1, 0, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
 ENDIF
ENDDO
   CALL RSL_LITE_EXCH_X ( local_communicator , mytask, ntasks, ntasks_x, ntasks_y, &
                          rsl_sendw_m,  rsl_sendw_p, rsl_recvw_m,  rsl_recvw_p    )
DO itrace = PARAM_FIRST_SCALAR, num_chem
 IF ( SIZE(chem_old,1)*SIZE(chem_old,3) .GT. 1 ) THEN
  CALL RSL_LITE_PACK ( local_communicator,&
chem_old ( grid%sm31,grid%sm32,grid%sm33,itrace),2,&
rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p, &
rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p, &
4, 1, 1, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
 ENDIF
ENDDO
    ENDDO

  
  END SUBROUTINE HALO_EM_CHEM_OLD_E_3_sub







SUBROUTINE HALO_EM_CHEM_OLD_E_5_sub ( grid, &
  num_chem, &
  chem_old, &
  local_communicator, &
  mytask, ntasks, ntasks_x, ntasks_y, &
  ids, ide, jds, jde, kds, kde,       &
  ims, ime, jms, jme, kms, kme,       &
  ips, ipe, jps, jpe, kps, kpe )
  USE module_domain, ONLY:domain
  USE module_configure, ONLY:grid_config_rec_type,in_use_for_config
  USE module_state_description, ONLY:PARAM_FIRST_SCALAR
  USE module_driver_constants
  TYPE(domain) ,               INTENT(IN) :: grid
  INTEGER, INTENT(IN) :: num_chem
  real, INTENT(INOUT) :: chem_old ( grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33,num_chem)
  INTEGER ,                    INTENT(IN) :: local_communicator
  INTEGER ,                    INTENT(IN) :: mytask, ntasks, ntasks_x, ntasks_y
  INTEGER ,                    INTENT(IN) :: ids, ide, jds, jde, kds, kde
  INTEGER ,                    INTENT(IN) :: ims, ime, jms, jme, kms, kme
  INTEGER ,                    INTENT(IN) :: ips, ipe, jps, jpe, kps, kpe
  INTEGER :: itrace
  INTEGER :: rsl_sendw_p, rsl_sendbeg_p, rsl_recvw_p, rsl_recvbeg_p
  INTEGER :: rsl_sendw_m, rsl_sendbeg_m, rsl_recvw_m, rsl_recvbeg_m
  LOGICAL, EXTERNAL :: rsl_comm_iter
  INTEGER :: idim1, idim2, idim3, idim4, idim5, idim6, idim7
  






CALL wrf_debug(2,'calling inc/HALO_EM_CHEM_OLD_E_5_inline.inc')
CALL rsl_comm_iter_init(3,jps,jpe)
DO WHILE ( rsl_comm_iter( grid%id , grid%is_intermediate, 3 , &
                         0 , jds,jde,jps,jpe, grid%njds, grid%njde , & 
     rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p,   & 
     rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p    ))
 CALL RSL_LITE_INIT_EXCH ( local_communicator, 3, 0, &
     rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p,   & 
     rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p,   & 
     0  &
   + num_chem   &
     , 0, 4, &
     0, 0, 4, &
     0, 0, 8, &
      0,  0, 4, &
      mytask, ntasks, ntasks_x, ntasks_y,   &
      ips, ipe, jps, jpe, kps, MAX(1,1&
,kpe &
))
DO itrace = PARAM_FIRST_SCALAR, num_chem
 IF ( SIZE(chem_old,1)*SIZE(chem_old,3) .GT. 1 ) THEN
  CALL RSL_LITE_PACK ( local_communicator,&
chem_old ( grid%sm31,grid%sm32,grid%sm33,itrace),3,&
rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p, &
rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p, &
4, 0, 0, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
 ENDIF
ENDDO
   CALL RSL_LITE_EXCH_Y ( local_communicator , mytask, ntasks, ntasks_x, ntasks_y, &
                          rsl_sendw_m,  rsl_sendw_p, rsl_recvw_m,  rsl_recvw_p    )
DO itrace = PARAM_FIRST_SCALAR, num_chem
 IF ( SIZE(chem_old,1)*SIZE(chem_old,3) .GT. 1 ) THEN
  CALL RSL_LITE_PACK ( local_communicator,&
chem_old ( grid%sm31,grid%sm32,grid%sm33,itrace),3,&
rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p, &
rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p, &
4, 0, 1, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
 ENDIF
ENDDO
ENDDO
CALL rsl_comm_iter_init(3,ips,ipe)
DO WHILE ( rsl_comm_iter( grid%id , grid%is_intermediate, 3 , &
                         1 , ids,ide,ips,ipe, grid%nids, grid%nide , & 
     rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p,   & 
     rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p    ))
 CALL RSL_LITE_INIT_EXCH ( local_communicator, 3, 1, &
     rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p,   & 
     rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p,   & 
     0  &
   + num_chem   &
     , 0, 4, &
     0, 0, 4, &
     0, 0, 8, &
      0,  0, 4, &
      mytask, ntasks, ntasks_x, ntasks_y,   &
      ips, ipe, jps, jpe, kps, MAX(1,1&
,kpe &
))
DO itrace = PARAM_FIRST_SCALAR, num_chem
 IF ( SIZE(chem_old,1)*SIZE(chem_old,3) .GT. 1 ) THEN
  CALL RSL_LITE_PACK ( local_communicator,&
chem_old ( grid%sm31,grid%sm32,grid%sm33,itrace),3,&
rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p, &
rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p, &
4, 1, 0, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
 ENDIF
ENDDO
   CALL RSL_LITE_EXCH_X ( local_communicator , mytask, ntasks, ntasks_x, ntasks_y, &
                          rsl_sendw_m,  rsl_sendw_p, rsl_recvw_m,  rsl_recvw_p    )
DO itrace = PARAM_FIRST_SCALAR, num_chem
 IF ( SIZE(chem_old,1)*SIZE(chem_old,3) .GT. 1 ) THEN
  CALL RSL_LITE_PACK ( local_communicator,&
chem_old ( grid%sm31,grid%sm32,grid%sm33,itrace),3,&
rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p, &
rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p, &
4, 1, 1, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
 ENDIF
ENDDO
    ENDDO

  
  END SUBROUTINE HALO_EM_CHEM_OLD_E_5_sub







SUBROUTINE HALO_EM_CHEM_OLD_E_7_sub ( grid, &
  num_chem, &
  chem_old, &
  local_communicator, &
  mytask, ntasks, ntasks_x, ntasks_y, &
  ids, ide, jds, jde, kds, kde,       &
  ims, ime, jms, jme, kms, kme,       &
  ips, ipe, jps, jpe, kps, kpe )
  USE module_domain, ONLY:domain
  USE module_configure, ONLY:grid_config_rec_type,in_use_for_config
  USE module_state_description, ONLY:PARAM_FIRST_SCALAR
  USE module_driver_constants
  TYPE(domain) ,               INTENT(IN) :: grid
  INTEGER, INTENT(IN) :: num_chem
  real, INTENT(INOUT) :: chem_old ( grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33,num_chem)
  INTEGER ,                    INTENT(IN) :: local_communicator
  INTEGER ,                    INTENT(IN) :: mytask, ntasks, ntasks_x, ntasks_y
  INTEGER ,                    INTENT(IN) :: ids, ide, jds, jde, kds, kde
  INTEGER ,                    INTENT(IN) :: ims, ime, jms, jme, kms, kme
  INTEGER ,                    INTENT(IN) :: ips, ipe, jps, jpe, kps, kpe
  INTEGER :: itrace
  INTEGER :: rsl_sendw_p, rsl_sendbeg_p, rsl_recvw_p, rsl_recvbeg_p
  INTEGER :: rsl_sendw_m, rsl_sendbeg_m, rsl_recvw_m, rsl_recvbeg_m
  LOGICAL, EXTERNAL :: rsl_comm_iter
  INTEGER :: idim1, idim2, idim3, idim4, idim5, idim6, idim7
  






CALL wrf_debug(2,'calling inc/HALO_EM_CHEM_OLD_E_7_inline.inc')
CALL rsl_comm_iter_init(4,jps,jpe)
DO WHILE ( rsl_comm_iter( grid%id , grid%is_intermediate, 4 , &
                         0 , jds,jde,jps,jpe, grid%njds, grid%njde , & 
     rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p,   & 
     rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p    ))
 CALL RSL_LITE_INIT_EXCH ( local_communicator, 4, 0, &
     rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p,   & 
     rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p,   & 
     0  &
   + num_chem   &
     , 0, 4, &
     0, 0, 4, &
     0, 0, 8, &
      0,  0, 4, &
      mytask, ntasks, ntasks_x, ntasks_y,   &
      ips, ipe, jps, jpe, kps, MAX(1,1&
,kpe &
))
DO itrace = PARAM_FIRST_SCALAR, num_chem
 IF ( SIZE(chem_old,1)*SIZE(chem_old,3) .GT. 1 ) THEN
  CALL RSL_LITE_PACK ( local_communicator,&
chem_old ( grid%sm31,grid%sm32,grid%sm33,itrace),4,&
rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p, &
rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p, &
4, 0, 0, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
 ENDIF
ENDDO
   CALL RSL_LITE_EXCH_Y ( local_communicator , mytask, ntasks, ntasks_x, ntasks_y, &
                          rsl_sendw_m,  rsl_sendw_p, rsl_recvw_m,  rsl_recvw_p    )
DO itrace = PARAM_FIRST_SCALAR, num_chem
 IF ( SIZE(chem_old,1)*SIZE(chem_old,3) .GT. 1 ) THEN
  CALL RSL_LITE_PACK ( local_communicator,&
chem_old ( grid%sm31,grid%sm32,grid%sm33,itrace),4,&
rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p, &
rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p, &
4, 0, 1, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
 ENDIF
ENDDO
ENDDO
CALL rsl_comm_iter_init(4,ips,ipe)
DO WHILE ( rsl_comm_iter( grid%id , grid%is_intermediate, 4 , &
                         1 , ids,ide,ips,ipe, grid%nids, grid%nide , & 
     rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p,   & 
     rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p    ))
 CALL RSL_LITE_INIT_EXCH ( local_communicator, 4, 1, &
     rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p,   & 
     rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p,   & 
     0  &
   + num_chem   &
     , 0, 4, &
     0, 0, 4, &
     0, 0, 8, &
      0,  0, 4, &
      mytask, ntasks, ntasks_x, ntasks_y,   &
      ips, ipe, jps, jpe, kps, MAX(1,1&
,kpe &
))
DO itrace = PARAM_FIRST_SCALAR, num_chem
 IF ( SIZE(chem_old,1)*SIZE(chem_old,3) .GT. 1 ) THEN
  CALL RSL_LITE_PACK ( local_communicator,&
chem_old ( grid%sm31,grid%sm32,grid%sm33,itrace),4,&
rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p, &
rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p, &
4, 1, 0, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
 ENDIF
ENDDO
   CALL RSL_LITE_EXCH_X ( local_communicator , mytask, ntasks, ntasks_x, ntasks_y, &
                          rsl_sendw_m,  rsl_sendw_p, rsl_recvw_m,  rsl_recvw_p    )
DO itrace = PARAM_FIRST_SCALAR, num_chem
 IF ( SIZE(chem_old,1)*SIZE(chem_old,3) .GT. 1 ) THEN
  CALL RSL_LITE_PACK ( local_communicator,&
chem_old ( grid%sm31,grid%sm32,grid%sm33,itrace),4,&
rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p, &
rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p, &
4, 1, 1, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
 ENDIF
ENDDO
    ENDDO

  
  END SUBROUTINE HALO_EM_CHEM_OLD_E_7_sub







SUBROUTINE HALO_EM_TRACER_OLD_E_3_sub ( grid, &
  num_tracer, &
  tracer_old, &
  local_communicator, &
  mytask, ntasks, ntasks_x, ntasks_y, &
  ids, ide, jds, jde, kds, kde,       &
  ims, ime, jms, jme, kms, kme,       &
  ips, ipe, jps, jpe, kps, kpe )
  USE module_domain, ONLY:domain
  USE module_configure, ONLY:grid_config_rec_type,in_use_for_config
  USE module_state_description, ONLY:PARAM_FIRST_SCALAR
  USE module_driver_constants
  TYPE(domain) ,               INTENT(IN) :: grid
  INTEGER, INTENT(IN) :: num_tracer
  real, INTENT(INOUT) :: tracer_old ( grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33,num_tracer)
  INTEGER ,                    INTENT(IN) :: local_communicator
  INTEGER ,                    INTENT(IN) :: mytask, ntasks, ntasks_x, ntasks_y
  INTEGER ,                    INTENT(IN) :: ids, ide, jds, jde, kds, kde
  INTEGER ,                    INTENT(IN) :: ims, ime, jms, jme, kms, kme
  INTEGER ,                    INTENT(IN) :: ips, ipe, jps, jpe, kps, kpe
  INTEGER :: itrace
  INTEGER :: rsl_sendw_p, rsl_sendbeg_p, rsl_recvw_p, rsl_recvbeg_p
  INTEGER :: rsl_sendw_m, rsl_sendbeg_m, rsl_recvw_m, rsl_recvbeg_m
  LOGICAL, EXTERNAL :: rsl_comm_iter
  INTEGER :: idim1, idim2, idim3, idim4, idim5, idim6, idim7
  






CALL wrf_debug(2,'calling inc/HALO_EM_TRACER_OLD_E_3_inline.inc')
CALL rsl_comm_iter_init(2,jps,jpe)
DO WHILE ( rsl_comm_iter( grid%id , grid%is_intermediate, 2 , &
                         0 , jds,jde,jps,jpe, grid%njds, grid%njde , & 
     rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p,   & 
     rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p    ))
 CALL RSL_LITE_INIT_EXCH ( local_communicator, 2, 0, &
     rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p,   & 
     rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p,   & 
     0  &
   + num_tracer   &
     , 0, 4, &
     0, 0, 4, &
     0, 0, 8, &
      0,  0, 4, &
      mytask, ntasks, ntasks_x, ntasks_y,   &
      ips, ipe, jps, jpe, kps, MAX(1,1&
,kpe &
))
DO itrace = PARAM_FIRST_SCALAR, num_tracer
 IF ( SIZE(tracer_old,1)*SIZE(tracer_old,3) .GT. 1 ) THEN
  CALL RSL_LITE_PACK ( local_communicator,&
tracer_old ( grid%sm31,grid%sm32,grid%sm33,itrace),2,&
rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p, &
rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p, &
4, 0, 0, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
 ENDIF
ENDDO
   CALL RSL_LITE_EXCH_Y ( local_communicator , mytask, ntasks, ntasks_x, ntasks_y, &
                          rsl_sendw_m,  rsl_sendw_p, rsl_recvw_m,  rsl_recvw_p    )
DO itrace = PARAM_FIRST_SCALAR, num_tracer
 IF ( SIZE(tracer_old,1)*SIZE(tracer_old,3) .GT. 1 ) THEN
  CALL RSL_LITE_PACK ( local_communicator,&
tracer_old ( grid%sm31,grid%sm32,grid%sm33,itrace),2,&
rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p, &
rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p, &
4, 0, 1, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
 ENDIF
ENDDO
ENDDO
CALL rsl_comm_iter_init(2,ips,ipe)
DO WHILE ( rsl_comm_iter( grid%id , grid%is_intermediate, 2 , &
                         1 , ids,ide,ips,ipe, grid%nids, grid%nide , & 
     rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p,   & 
     rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p    ))
 CALL RSL_LITE_INIT_EXCH ( local_communicator, 2, 1, &
     rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p,   & 
     rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p,   & 
     0  &
   + num_tracer   &
     , 0, 4, &
     0, 0, 4, &
     0, 0, 8, &
      0,  0, 4, &
      mytask, ntasks, ntasks_x, ntasks_y,   &
      ips, ipe, jps, jpe, kps, MAX(1,1&
,kpe &
))
DO itrace = PARAM_FIRST_SCALAR, num_tracer
 IF ( SIZE(tracer_old,1)*SIZE(tracer_old,3) .GT. 1 ) THEN
  CALL RSL_LITE_PACK ( local_communicator,&
tracer_old ( grid%sm31,grid%sm32,grid%sm33,itrace),2,&
rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p, &
rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p, &
4, 1, 0, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
 ENDIF
ENDDO
   CALL RSL_LITE_EXCH_X ( local_communicator , mytask, ntasks, ntasks_x, ntasks_y, &
                          rsl_sendw_m,  rsl_sendw_p, rsl_recvw_m,  rsl_recvw_p    )
DO itrace = PARAM_FIRST_SCALAR, num_tracer
 IF ( SIZE(tracer_old,1)*SIZE(tracer_old,3) .GT. 1 ) THEN
  CALL RSL_LITE_PACK ( local_communicator,&
tracer_old ( grid%sm31,grid%sm32,grid%sm33,itrace),2,&
rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p, &
rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p, &
4, 1, 1, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
 ENDIF
ENDDO
    ENDDO

  
  END SUBROUTINE HALO_EM_TRACER_OLD_E_3_sub







SUBROUTINE HALO_EM_TRACER_OLD_E_5_sub ( grid, &
  num_tracer, &
  tracer_old, &
  local_communicator, &
  mytask, ntasks, ntasks_x, ntasks_y, &
  ids, ide, jds, jde, kds, kde,       &
  ims, ime, jms, jme, kms, kme,       &
  ips, ipe, jps, jpe, kps, kpe )
  USE module_domain, ONLY:domain
  USE module_configure, ONLY:grid_config_rec_type,in_use_for_config
  USE module_state_description, ONLY:PARAM_FIRST_SCALAR
  USE module_driver_constants
  TYPE(domain) ,               INTENT(IN) :: grid
  INTEGER, INTENT(IN) :: num_tracer
  real, INTENT(INOUT) :: tracer_old ( grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33,num_tracer)
  INTEGER ,                    INTENT(IN) :: local_communicator
  INTEGER ,                    INTENT(IN) :: mytask, ntasks, ntasks_x, ntasks_y
  INTEGER ,                    INTENT(IN) :: ids, ide, jds, jde, kds, kde
  INTEGER ,                    INTENT(IN) :: ims, ime, jms, jme, kms, kme
  INTEGER ,                    INTENT(IN) :: ips, ipe, jps, jpe, kps, kpe
  INTEGER :: itrace
  INTEGER :: rsl_sendw_p, rsl_sendbeg_p, rsl_recvw_p, rsl_recvbeg_p
  INTEGER :: rsl_sendw_m, rsl_sendbeg_m, rsl_recvw_m, rsl_recvbeg_m
  LOGICAL, EXTERNAL :: rsl_comm_iter
  INTEGER :: idim1, idim2, idim3, idim4, idim5, idim6, idim7
  






CALL wrf_debug(2,'calling inc/HALO_EM_TRACER_OLD_E_5_inline.inc')
CALL rsl_comm_iter_init(3,jps,jpe)
DO WHILE ( rsl_comm_iter( grid%id , grid%is_intermediate, 3 , &
                         0 , jds,jde,jps,jpe, grid%njds, grid%njde , & 
     rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p,   & 
     rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p    ))
 CALL RSL_LITE_INIT_EXCH ( local_communicator, 3, 0, &
     rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p,   & 
     rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p,   & 
     0  &
   + num_tracer   &
     , 0, 4, &
     0, 0, 4, &
     0, 0, 8, &
      0,  0, 4, &
      mytask, ntasks, ntasks_x, ntasks_y,   &
      ips, ipe, jps, jpe, kps, MAX(1,1&
,kpe &
))
DO itrace = PARAM_FIRST_SCALAR, num_tracer
 IF ( SIZE(tracer_old,1)*SIZE(tracer_old,3) .GT. 1 ) THEN
  CALL RSL_LITE_PACK ( local_communicator,&
tracer_old ( grid%sm31,grid%sm32,grid%sm33,itrace),3,&
rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p, &
rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p, &
4, 0, 0, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
 ENDIF
ENDDO
   CALL RSL_LITE_EXCH_Y ( local_communicator , mytask, ntasks, ntasks_x, ntasks_y, &
                          rsl_sendw_m,  rsl_sendw_p, rsl_recvw_m,  rsl_recvw_p    )
DO itrace = PARAM_FIRST_SCALAR, num_tracer
 IF ( SIZE(tracer_old,1)*SIZE(tracer_old,3) .GT. 1 ) THEN
  CALL RSL_LITE_PACK ( local_communicator,&
tracer_old ( grid%sm31,grid%sm32,grid%sm33,itrace),3,&
rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p, &
rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p, &
4, 0, 1, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
 ENDIF
ENDDO
ENDDO
CALL rsl_comm_iter_init(3,ips,ipe)
DO WHILE ( rsl_comm_iter( grid%id , grid%is_intermediate, 3 , &
                         1 , ids,ide,ips,ipe, grid%nids, grid%nide , & 
     rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p,   & 
     rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p    ))
 CALL RSL_LITE_INIT_EXCH ( local_communicator, 3, 1, &
     rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p,   & 
     rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p,   & 
     0  &
   + num_tracer   &
     , 0, 4, &
     0, 0, 4, &
     0, 0, 8, &
      0,  0, 4, &
      mytask, ntasks, ntasks_x, ntasks_y,   &
      ips, ipe, jps, jpe, kps, MAX(1,1&
,kpe &
))
DO itrace = PARAM_FIRST_SCALAR, num_tracer
 IF ( SIZE(tracer_old,1)*SIZE(tracer_old,3) .GT. 1 ) THEN
  CALL RSL_LITE_PACK ( local_communicator,&
tracer_old ( grid%sm31,grid%sm32,grid%sm33,itrace),3,&
rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p, &
rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p, &
4, 1, 0, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
 ENDIF
ENDDO
   CALL RSL_LITE_EXCH_X ( local_communicator , mytask, ntasks, ntasks_x, ntasks_y, &
                          rsl_sendw_m,  rsl_sendw_p, rsl_recvw_m,  rsl_recvw_p    )
DO itrace = PARAM_FIRST_SCALAR, num_tracer
 IF ( SIZE(tracer_old,1)*SIZE(tracer_old,3) .GT. 1 ) THEN
  CALL RSL_LITE_PACK ( local_communicator,&
tracer_old ( grid%sm31,grid%sm32,grid%sm33,itrace),3,&
rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p, &
rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p, &
4, 1, 1, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
 ENDIF
ENDDO
    ENDDO

  
  END SUBROUTINE HALO_EM_TRACER_OLD_E_5_sub







SUBROUTINE HALO_EM_TRACER_OLD_E_7_sub ( grid, &
  num_tracer, &
  tracer_old, &
  local_communicator, &
  mytask, ntasks, ntasks_x, ntasks_y, &
  ids, ide, jds, jde, kds, kde,       &
  ims, ime, jms, jme, kms, kme,       &
  ips, ipe, jps, jpe, kps, kpe )
  USE module_domain, ONLY:domain
  USE module_configure, ONLY:grid_config_rec_type,in_use_for_config
  USE module_state_description, ONLY:PARAM_FIRST_SCALAR
  USE module_driver_constants
  TYPE(domain) ,               INTENT(IN) :: grid
  INTEGER, INTENT(IN) :: num_tracer
  real, INTENT(INOUT) :: tracer_old ( grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33,num_tracer)
  INTEGER ,                    INTENT(IN) :: local_communicator
  INTEGER ,                    INTENT(IN) :: mytask, ntasks, ntasks_x, ntasks_y
  INTEGER ,                    INTENT(IN) :: ids, ide, jds, jde, kds, kde
  INTEGER ,                    INTENT(IN) :: ims, ime, jms, jme, kms, kme
  INTEGER ,                    INTENT(IN) :: ips, ipe, jps, jpe, kps, kpe
  INTEGER :: itrace
  INTEGER :: rsl_sendw_p, rsl_sendbeg_p, rsl_recvw_p, rsl_recvbeg_p
  INTEGER :: rsl_sendw_m, rsl_sendbeg_m, rsl_recvw_m, rsl_recvbeg_m
  LOGICAL, EXTERNAL :: rsl_comm_iter
  INTEGER :: idim1, idim2, idim3, idim4, idim5, idim6, idim7
  






CALL wrf_debug(2,'calling inc/HALO_EM_TRACER_OLD_E_7_inline.inc')
CALL rsl_comm_iter_init(4,jps,jpe)
DO WHILE ( rsl_comm_iter( grid%id , grid%is_intermediate, 4 , &
                         0 , jds,jde,jps,jpe, grid%njds, grid%njde , & 
     rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p,   & 
     rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p    ))
 CALL RSL_LITE_INIT_EXCH ( local_communicator, 4, 0, &
     rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p,   & 
     rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p,   & 
     0  &
   + num_tracer   &
     , 0, 4, &
     0, 0, 4, &
     0, 0, 8, &
      0,  0, 4, &
      mytask, ntasks, ntasks_x, ntasks_y,   &
      ips, ipe, jps, jpe, kps, MAX(1,1&
,kpe &
))
DO itrace = PARAM_FIRST_SCALAR, num_tracer
 IF ( SIZE(tracer_old,1)*SIZE(tracer_old,3) .GT. 1 ) THEN
  CALL RSL_LITE_PACK ( local_communicator,&
tracer_old ( grid%sm31,grid%sm32,grid%sm33,itrace),4,&
rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p, &
rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p, &
4, 0, 0, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
 ENDIF
ENDDO
   CALL RSL_LITE_EXCH_Y ( local_communicator , mytask, ntasks, ntasks_x, ntasks_y, &
                          rsl_sendw_m,  rsl_sendw_p, rsl_recvw_m,  rsl_recvw_p    )
DO itrace = PARAM_FIRST_SCALAR, num_tracer
 IF ( SIZE(tracer_old,1)*SIZE(tracer_old,3) .GT. 1 ) THEN
  CALL RSL_LITE_PACK ( local_communicator,&
tracer_old ( grid%sm31,grid%sm32,grid%sm33,itrace),4,&
rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p, &
rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p, &
4, 0, 1, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
 ENDIF
ENDDO
ENDDO
CALL rsl_comm_iter_init(4,ips,ipe)
DO WHILE ( rsl_comm_iter( grid%id , grid%is_intermediate, 4 , &
                         1 , ids,ide,ips,ipe, grid%nids, grid%nide , & 
     rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p,   & 
     rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p    ))
 CALL RSL_LITE_INIT_EXCH ( local_communicator, 4, 1, &
     rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p,   & 
     rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p,   & 
     0  &
   + num_tracer   &
     , 0, 4, &
     0, 0, 4, &
     0, 0, 8, &
      0,  0, 4, &
      mytask, ntasks, ntasks_x, ntasks_y,   &
      ips, ipe, jps, jpe, kps, MAX(1,1&
,kpe &
))
DO itrace = PARAM_FIRST_SCALAR, num_tracer
 IF ( SIZE(tracer_old,1)*SIZE(tracer_old,3) .GT. 1 ) THEN
  CALL RSL_LITE_PACK ( local_communicator,&
tracer_old ( grid%sm31,grid%sm32,grid%sm33,itrace),4,&
rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p, &
rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p, &
4, 1, 0, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
 ENDIF
ENDDO
   CALL RSL_LITE_EXCH_X ( local_communicator , mytask, ntasks, ntasks_x, ntasks_y, &
                          rsl_sendw_m,  rsl_sendw_p, rsl_recvw_m,  rsl_recvw_p    )
DO itrace = PARAM_FIRST_SCALAR, num_tracer
 IF ( SIZE(tracer_old,1)*SIZE(tracer_old,3) .GT. 1 ) THEN
  CALL RSL_LITE_PACK ( local_communicator,&
tracer_old ( grid%sm31,grid%sm32,grid%sm33,itrace),4,&
rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p, &
rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p, &
4, 1, 1, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
 ENDIF
ENDDO
    ENDDO

  
  END SUBROUTINE HALO_EM_TRACER_OLD_E_7_sub







SUBROUTINE HALO_EM_SCALAR_OLD_E_3_sub ( grid, &
  num_scalar, &
  scalar_old, &
  local_communicator, &
  mytask, ntasks, ntasks_x, ntasks_y, &
  ids, ide, jds, jde, kds, kde,       &
  ims, ime, jms, jme, kms, kme,       &
  ips, ipe, jps, jpe, kps, kpe )
  USE module_domain, ONLY:domain
  USE module_configure, ONLY:grid_config_rec_type,in_use_for_config
  USE module_state_description, ONLY:PARAM_FIRST_SCALAR
  USE module_driver_constants
  TYPE(domain) ,               INTENT(IN) :: grid
  INTEGER, INTENT(IN) :: num_scalar
  real, INTENT(INOUT) :: scalar_old ( grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33,num_scalar)
  INTEGER ,                    INTENT(IN) :: local_communicator
  INTEGER ,                    INTENT(IN) :: mytask, ntasks, ntasks_x, ntasks_y
  INTEGER ,                    INTENT(IN) :: ids, ide, jds, jde, kds, kde
  INTEGER ,                    INTENT(IN) :: ims, ime, jms, jme, kms, kme
  INTEGER ,                    INTENT(IN) :: ips, ipe, jps, jpe, kps, kpe
  INTEGER :: itrace
  INTEGER :: rsl_sendw_p, rsl_sendbeg_p, rsl_recvw_p, rsl_recvbeg_p
  INTEGER :: rsl_sendw_m, rsl_sendbeg_m, rsl_recvw_m, rsl_recvbeg_m
  LOGICAL, EXTERNAL :: rsl_comm_iter
  INTEGER :: idim1, idim2, idim3, idim4, idim5, idim6, idim7
  






CALL wrf_debug(2,'calling inc/HALO_EM_SCALAR_OLD_E_3_inline.inc')
CALL rsl_comm_iter_init(2,jps,jpe)
DO WHILE ( rsl_comm_iter( grid%id , grid%is_intermediate, 2 , &
                         0 , jds,jde,jps,jpe, grid%njds, grid%njde , & 
     rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p,   & 
     rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p    ))
 CALL RSL_LITE_INIT_EXCH ( local_communicator, 2, 0, &
     rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p,   & 
     rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p,   & 
     0  &
   + num_scalar   &
     , 0, 4, &
     0, 0, 4, &
     0, 0, 8, &
      0,  0, 4, &
      mytask, ntasks, ntasks_x, ntasks_y,   &
      ips, ipe, jps, jpe, kps, MAX(1,1&
,kpe &
))
DO itrace = PARAM_FIRST_SCALAR, num_scalar
 IF ( SIZE(scalar_old,1)*SIZE(scalar_old,3) .GT. 1 ) THEN
  CALL RSL_LITE_PACK ( local_communicator,&
scalar_old ( grid%sm31,grid%sm32,grid%sm33,itrace),2,&
rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p, &
rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p, &
4, 0, 0, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
 ENDIF
ENDDO
   CALL RSL_LITE_EXCH_Y ( local_communicator , mytask, ntasks, ntasks_x, ntasks_y, &
                          rsl_sendw_m,  rsl_sendw_p, rsl_recvw_m,  rsl_recvw_p    )
DO itrace = PARAM_FIRST_SCALAR, num_scalar
 IF ( SIZE(scalar_old,1)*SIZE(scalar_old,3) .GT. 1 ) THEN
  CALL RSL_LITE_PACK ( local_communicator,&
scalar_old ( grid%sm31,grid%sm32,grid%sm33,itrace),2,&
rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p, &
rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p, &
4, 0, 1, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
 ENDIF
ENDDO
ENDDO
CALL rsl_comm_iter_init(2,ips,ipe)
DO WHILE ( rsl_comm_iter( grid%id , grid%is_intermediate, 2 , &
                         1 , ids,ide,ips,ipe, grid%nids, grid%nide , & 
     rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p,   & 
     rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p    ))
 CALL RSL_LITE_INIT_EXCH ( local_communicator, 2, 1, &
     rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p,   & 
     rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p,   & 
     0  &
   + num_scalar   &
     , 0, 4, &
     0, 0, 4, &
     0, 0, 8, &
      0,  0, 4, &
      mytask, ntasks, ntasks_x, ntasks_y,   &
      ips, ipe, jps, jpe, kps, MAX(1,1&
,kpe &
))
DO itrace = PARAM_FIRST_SCALAR, num_scalar
 IF ( SIZE(scalar_old,1)*SIZE(scalar_old,3) .GT. 1 ) THEN
  CALL RSL_LITE_PACK ( local_communicator,&
scalar_old ( grid%sm31,grid%sm32,grid%sm33,itrace),2,&
rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p, &
rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p, &
4, 1, 0, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
 ENDIF
ENDDO
   CALL RSL_LITE_EXCH_X ( local_communicator , mytask, ntasks, ntasks_x, ntasks_y, &
                          rsl_sendw_m,  rsl_sendw_p, rsl_recvw_m,  rsl_recvw_p    )
DO itrace = PARAM_FIRST_SCALAR, num_scalar
 IF ( SIZE(scalar_old,1)*SIZE(scalar_old,3) .GT. 1 ) THEN
  CALL RSL_LITE_PACK ( local_communicator,&
scalar_old ( grid%sm31,grid%sm32,grid%sm33,itrace),2,&
rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p, &
rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p, &
4, 1, 1, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
 ENDIF
ENDDO
    ENDDO

  
  END SUBROUTINE HALO_EM_SCALAR_OLD_E_3_sub







SUBROUTINE HALO_EM_SCALAR_OLD_E_5_sub ( grid, &
  num_scalar, &
  scalar_old, &
  local_communicator, &
  mytask, ntasks, ntasks_x, ntasks_y, &
  ids, ide, jds, jde, kds, kde,       &
  ims, ime, jms, jme, kms, kme,       &
  ips, ipe, jps, jpe, kps, kpe )
  USE module_domain, ONLY:domain
  USE module_configure, ONLY:grid_config_rec_type,in_use_for_config
  USE module_state_description, ONLY:PARAM_FIRST_SCALAR
  USE module_driver_constants
  TYPE(domain) ,               INTENT(IN) :: grid
  INTEGER, INTENT(IN) :: num_scalar
  real, INTENT(INOUT) :: scalar_old ( grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33,num_scalar)
  INTEGER ,                    INTENT(IN) :: local_communicator
  INTEGER ,                    INTENT(IN) :: mytask, ntasks, ntasks_x, ntasks_y
  INTEGER ,                    INTENT(IN) :: ids, ide, jds, jde, kds, kde
  INTEGER ,                    INTENT(IN) :: ims, ime, jms, jme, kms, kme
  INTEGER ,                    INTENT(IN) :: ips, ipe, jps, jpe, kps, kpe
  INTEGER :: itrace
  INTEGER :: rsl_sendw_p, rsl_sendbeg_p, rsl_recvw_p, rsl_recvbeg_p
  INTEGER :: rsl_sendw_m, rsl_sendbeg_m, rsl_recvw_m, rsl_recvbeg_m
  LOGICAL, EXTERNAL :: rsl_comm_iter
  INTEGER :: idim1, idim2, idim3, idim4, idim5, idim6, idim7
  






CALL wrf_debug(2,'calling inc/HALO_EM_SCALAR_OLD_E_5_inline.inc')
CALL rsl_comm_iter_init(3,jps,jpe)
DO WHILE ( rsl_comm_iter( grid%id , grid%is_intermediate, 3 , &
                         0 , jds,jde,jps,jpe, grid%njds, grid%njde , & 
     rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p,   & 
     rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p    ))
 CALL RSL_LITE_INIT_EXCH ( local_communicator, 3, 0, &
     rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p,   & 
     rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p,   & 
     0  &
   + num_scalar   &
     , 0, 4, &
     0, 0, 4, &
     0, 0, 8, &
      0,  0, 4, &
      mytask, ntasks, ntasks_x, ntasks_y,   &
      ips, ipe, jps, jpe, kps, MAX(1,1&
,kpe &
))
DO itrace = PARAM_FIRST_SCALAR, num_scalar
 IF ( SIZE(scalar_old,1)*SIZE(scalar_old,3) .GT. 1 ) THEN
  CALL RSL_LITE_PACK ( local_communicator,&
scalar_old ( grid%sm31,grid%sm32,grid%sm33,itrace),3,&
rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p, &
rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p, &
4, 0, 0, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
 ENDIF
ENDDO
   CALL RSL_LITE_EXCH_Y ( local_communicator , mytask, ntasks, ntasks_x, ntasks_y, &
                          rsl_sendw_m,  rsl_sendw_p, rsl_recvw_m,  rsl_recvw_p    )
DO itrace = PARAM_FIRST_SCALAR, num_scalar
 IF ( SIZE(scalar_old,1)*SIZE(scalar_old,3) .GT. 1 ) THEN
  CALL RSL_LITE_PACK ( local_communicator,&
scalar_old ( grid%sm31,grid%sm32,grid%sm33,itrace),3,&
rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p, &
rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p, &
4, 0, 1, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
 ENDIF
ENDDO
ENDDO
CALL rsl_comm_iter_init(3,ips,ipe)
DO WHILE ( rsl_comm_iter( grid%id , grid%is_intermediate, 3 , &
                         1 , ids,ide,ips,ipe, grid%nids, grid%nide , & 
     rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p,   & 
     rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p    ))
 CALL RSL_LITE_INIT_EXCH ( local_communicator, 3, 1, &
     rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p,   & 
     rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p,   & 
     0  &
   + num_scalar   &
     , 0, 4, &
     0, 0, 4, &
     0, 0, 8, &
      0,  0, 4, &
      mytask, ntasks, ntasks_x, ntasks_y,   &
      ips, ipe, jps, jpe, kps, MAX(1,1&
,kpe &
))
DO itrace = PARAM_FIRST_SCALAR, num_scalar
 IF ( SIZE(scalar_old,1)*SIZE(scalar_old,3) .GT. 1 ) THEN
  CALL RSL_LITE_PACK ( local_communicator,&
scalar_old ( grid%sm31,grid%sm32,grid%sm33,itrace),3,&
rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p, &
rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p, &
4, 1, 0, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
 ENDIF
ENDDO
   CALL RSL_LITE_EXCH_X ( local_communicator , mytask, ntasks, ntasks_x, ntasks_y, &
                          rsl_sendw_m,  rsl_sendw_p, rsl_recvw_m,  rsl_recvw_p    )
DO itrace = PARAM_FIRST_SCALAR, num_scalar
 IF ( SIZE(scalar_old,1)*SIZE(scalar_old,3) .GT. 1 ) THEN
  CALL RSL_LITE_PACK ( local_communicator,&
scalar_old ( grid%sm31,grid%sm32,grid%sm33,itrace),3,&
rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p, &
rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p, &
4, 1, 1, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
 ENDIF
ENDDO
    ENDDO

  
  END SUBROUTINE HALO_EM_SCALAR_OLD_E_5_sub







SUBROUTINE HALO_EM_SCALAR_OLD_E_7_sub ( grid, &
  num_scalar, &
  scalar_old, &
  local_communicator, &
  mytask, ntasks, ntasks_x, ntasks_y, &
  ids, ide, jds, jde, kds, kde,       &
  ims, ime, jms, jme, kms, kme,       &
  ips, ipe, jps, jpe, kps, kpe )
  USE module_domain, ONLY:domain
  USE module_configure, ONLY:grid_config_rec_type,in_use_for_config
  USE module_state_description, ONLY:PARAM_FIRST_SCALAR
  USE module_driver_constants
  TYPE(domain) ,               INTENT(IN) :: grid
  INTEGER, INTENT(IN) :: num_scalar
  real, INTENT(INOUT) :: scalar_old ( grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33,num_scalar)
  INTEGER ,                    INTENT(IN) :: local_communicator
  INTEGER ,                    INTENT(IN) :: mytask, ntasks, ntasks_x, ntasks_y
  INTEGER ,                    INTENT(IN) :: ids, ide, jds, jde, kds, kde
  INTEGER ,                    INTENT(IN) :: ims, ime, jms, jme, kms, kme
  INTEGER ,                    INTENT(IN) :: ips, ipe, jps, jpe, kps, kpe
  INTEGER :: itrace
  INTEGER :: rsl_sendw_p, rsl_sendbeg_p, rsl_recvw_p, rsl_recvbeg_p
  INTEGER :: rsl_sendw_m, rsl_sendbeg_m, rsl_recvw_m, rsl_recvbeg_m
  LOGICAL, EXTERNAL :: rsl_comm_iter
  INTEGER :: idim1, idim2, idim3, idim4, idim5, idim6, idim7
  






CALL wrf_debug(2,'calling inc/HALO_EM_SCALAR_OLD_E_7_inline.inc')
CALL rsl_comm_iter_init(4,jps,jpe)
DO WHILE ( rsl_comm_iter( grid%id , grid%is_intermediate, 4 , &
                         0 , jds,jde,jps,jpe, grid%njds, grid%njde , & 
     rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p,   & 
     rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p    ))
 CALL RSL_LITE_INIT_EXCH ( local_communicator, 4, 0, &
     rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p,   & 
     rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p,   & 
     0  &
   + num_scalar   &
     , 0, 4, &
     0, 0, 4, &
     0, 0, 8, &
      0,  0, 4, &
      mytask, ntasks, ntasks_x, ntasks_y,   &
      ips, ipe, jps, jpe, kps, MAX(1,1&
,kpe &
))
DO itrace = PARAM_FIRST_SCALAR, num_scalar
 IF ( SIZE(scalar_old,1)*SIZE(scalar_old,3) .GT. 1 ) THEN
  CALL RSL_LITE_PACK ( local_communicator,&
scalar_old ( grid%sm31,grid%sm32,grid%sm33,itrace),4,&
rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p, &
rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p, &
4, 0, 0, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
 ENDIF
ENDDO
   CALL RSL_LITE_EXCH_Y ( local_communicator , mytask, ntasks, ntasks_x, ntasks_y, &
                          rsl_sendw_m,  rsl_sendw_p, rsl_recvw_m,  rsl_recvw_p    )
DO itrace = PARAM_FIRST_SCALAR, num_scalar
 IF ( SIZE(scalar_old,1)*SIZE(scalar_old,3) .GT. 1 ) THEN
  CALL RSL_LITE_PACK ( local_communicator,&
scalar_old ( grid%sm31,grid%sm32,grid%sm33,itrace),4,&
rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p, &
rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p, &
4, 0, 1, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
 ENDIF
ENDDO
ENDDO
CALL rsl_comm_iter_init(4,ips,ipe)
DO WHILE ( rsl_comm_iter( grid%id , grid%is_intermediate, 4 , &
                         1 , ids,ide,ips,ipe, grid%nids, grid%nide , & 
     rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p,   & 
     rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p    ))
 CALL RSL_LITE_INIT_EXCH ( local_communicator, 4, 1, &
     rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p,   & 
     rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p,   & 
     0  &
   + num_scalar   &
     , 0, 4, &
     0, 0, 4, &
     0, 0, 8, &
      0,  0, 4, &
      mytask, ntasks, ntasks_x, ntasks_y,   &
      ips, ipe, jps, jpe, kps, MAX(1,1&
,kpe &
))
DO itrace = PARAM_FIRST_SCALAR, num_scalar
 IF ( SIZE(scalar_old,1)*SIZE(scalar_old,3) .GT. 1 ) THEN
  CALL RSL_LITE_PACK ( local_communicator,&
scalar_old ( grid%sm31,grid%sm32,grid%sm33,itrace),4,&
rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p, &
rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p, &
4, 1, 0, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
 ENDIF
ENDDO
   CALL RSL_LITE_EXCH_X ( local_communicator , mytask, ntasks, ntasks_x, ntasks_y, &
                          rsl_sendw_m,  rsl_sendw_p, rsl_recvw_m,  rsl_recvw_p    )
DO itrace = PARAM_FIRST_SCALAR, num_scalar
 IF ( SIZE(scalar_old,1)*SIZE(scalar_old,3) .GT. 1 ) THEN
  CALL RSL_LITE_PACK ( local_communicator,&
scalar_old ( grid%sm31,grid%sm32,grid%sm33,itrace),4,&
rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p, &
rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p, &
4, 1, 1, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
 ENDIF
ENDDO
    ENDDO

  
  END SUBROUTINE HALO_EM_SCALAR_OLD_E_7_sub







SUBROUTINE HALO_EM_FEEDBACK_sub ( grid, &
  local_communicator, &
  mytask, ntasks, ntasks_x, ntasks_y, &
  ids, ide, jds, jde, kds, kde,       &
  ims, ime, jms, jme, kms, kme,       &
  ips, ipe, jps, jpe, kps, kpe )
  USE module_domain, ONLY:domain
  USE module_configure, ONLY:grid_config_rec_type,in_use_for_config
  USE module_state_description, ONLY:PARAM_FIRST_SCALAR
  USE module_driver_constants
  TYPE(domain) ,               INTENT(IN) :: grid
  INTEGER ,                    INTENT(IN) :: local_communicator
  INTEGER ,                    INTENT(IN) :: mytask, ntasks, ntasks_x, ntasks_y
  INTEGER ,                    INTENT(IN) :: ids, ide, jds, jde, kds, kde
  INTEGER ,                    INTENT(IN) :: ims, ime, jms, jme, kms, kme
  INTEGER ,                    INTENT(IN) :: ips, ipe, jps, jpe, kps, kpe
  INTEGER :: itrace
  INTEGER :: rsl_sendw_p, rsl_sendbeg_p, rsl_recvw_p, rsl_recvbeg_p
  INTEGER :: rsl_sendw_m, rsl_sendbeg_m, rsl_recvw_m, rsl_recvbeg_m
  LOGICAL, EXTERNAL :: rsl_comm_iter
  INTEGER :: idim1, idim2, idim3, idim4, idim5, idim6, idim7
  






CALL wrf_debug(2,'calling inc/HALO_EM_FEEDBACK_inline.inc')
CALL rsl_comm_iter_init(3,jps,jpe)
DO WHILE ( rsl_comm_iter( grid%id , grid%is_intermediate, 3 , &
                         0 , jds,jde,jps,jpe, grid%njds, grid%njde , & 
     rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p,   & 
     rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p    ))
 CALL RSL_LITE_INIT_EXCH ( local_communicator, 3, 0, &
     rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p,   & 
     rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p,   & 
     0, 1, 4, &
     0, 0, 4, &
     0, 0, 8, &
      0,  0, 4, &
      mytask, ntasks, ntasks_x, ntasks_y,   &
      ips, ipe, jps, jpe, kps, MAX(1,1&
))
IF ( SIZE(grid%ht,1)*SIZE(grid%ht,2) .GT. 1 ) THEN
CALL RSL_LITE_PACK ( local_communicator,&
 grid%ht, 3,&
rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p, &
rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p, &
4, 0, 0, DATA_ORDER_XY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
ENDIF
   CALL RSL_LITE_EXCH_Y ( local_communicator , mytask, ntasks, ntasks_x, ntasks_y, &
                          rsl_sendw_m,  rsl_sendw_p, rsl_recvw_m,  rsl_recvw_p    )
IF ( SIZE(grid%ht,1)*SIZE(grid%ht,2) .GT. 1 ) THEN
CALL RSL_LITE_PACK ( local_communicator,&
 grid%ht, 3,&
rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p, &
rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p, &
4, 0, 1, DATA_ORDER_XY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
ENDIF
ENDDO
CALL rsl_comm_iter_init(3,ips,ipe)
DO WHILE ( rsl_comm_iter( grid%id , grid%is_intermediate, 3 , &
                         1 , ids,ide,ips,ipe, grid%nids, grid%nide , & 
     rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p,   & 
     rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p    ))
 CALL RSL_LITE_INIT_EXCH ( local_communicator, 3, 1, &
     rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p,   & 
     rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p,   & 
     0, 1, 4, &
     0, 0, 4, &
     0, 0, 8, &
      0,  0, 4, &
      mytask, ntasks, ntasks_x, ntasks_y,   &
      ips, ipe, jps, jpe, kps, MAX(1,1&
))
IF ( SIZE(grid%ht,1)*SIZE(grid%ht,2) .GT. 1 ) THEN
CALL RSL_LITE_PACK ( local_communicator,&
 grid%ht, 3,&
rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p, &
rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p, &
4, 1, 0, DATA_ORDER_XY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
ENDIF
   CALL RSL_LITE_EXCH_X ( local_communicator , mytask, ntasks, ntasks_x, ntasks_y, &
                          rsl_sendw_m,  rsl_sendw_p, rsl_recvw_m,  rsl_recvw_p    )
IF ( SIZE(grid%ht,1)*SIZE(grid%ht,2) .GT. 1 ) THEN
CALL RSL_LITE_PACK ( local_communicator,&
 grid%ht, 3,&
rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p, &
rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p, &
4, 1, 1, DATA_ORDER_XY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
ENDIF
    ENDDO

  
  END SUBROUTINE HALO_EM_FEEDBACK_sub


END MODULE module_comm_dm_2

