
MODULE module_comm_dm_4_

   IMPLICIT NONE

   PRIVATE module_comm_dm_dummy_4


   INTEGER, PRIVATE :: rsl_sendw_p, rsl_sendbeg_p, rsl_recvw_p, rsl_recvbeg_p
   INTEGER, PRIVATE :: rsl_sendw_m, rsl_sendbeg_m, rsl_recvw_m, rsl_recvbeg_m
   LOGICAL, EXTERNAL :: rsl_comm_iter


   INTEGER, PRIVATE :: idim1, idim2, idim3, idim4, idim5, idim6, idim7


CONTAINS

   
   SUBROUTINE module_comm_dm_dummy_4
     USE module_domain, ONLY:domain
     USE module_configure, ONLY:grid_config_rec_type,in_use_for_config
     USE module_state_description, ONLY:PARAM_FIRST_SCALAR
     USE module_driver_constants
     RETURN
   END SUBROUTINE module_comm_dm_dummy_4








SUBROUTINE PERIOD_EM_COUPLE_A_sub ( grid, &
  config_flags, &
  local_communicator_periodic, &
  mytask, ntasks, ntasks_x, ntasks_y, &
  ids, ide, jds, jde, kds, kde,       &
  ims, ime, jms, jme, kms, kme,       &
  ips, ipe, jps, jpe, kps, kpe )
  USE module_domain, ONLY:domain
  USE module_configure, ONLY:grid_config_rec_type,in_use_for_config
  USE module_state_description, ONLY:PARAM_FIRST_SCALAR
  USE module_driver_constants
  TYPE(domain) ,               INTENT(IN) :: grid
  TYPE(grid_config_rec_type) , INTENT(IN) :: config_flags
  INTEGER ,                    INTENT(IN) :: local_communicator_periodic
  INTEGER ,                    INTENT(IN) :: mytask, ntasks, ntasks_x, ntasks_y
  INTEGER ,                    INTENT(IN) :: ids, ide, jds, jde, kds, kde
  INTEGER ,                    INTENT(IN) :: ims, ime, jms, jme, kms, kme
  INTEGER ,                    INTENT(IN) :: ips, ipe, jps, jpe, kps, kpe
  INTEGER :: itrace
  INTEGER :: rsl_sendw_p, rsl_sendbeg_p, rsl_recvw_p, rsl_recvbeg_p
  INTEGER :: rsl_sendw_m, rsl_sendbeg_m, rsl_recvw_m, rsl_recvbeg_m
  LOGICAL, EXTERNAL :: rsl_comm_iter
  INTEGER :: idim1, idim2, idim3, idim4, idim5, idim6, idim7
  






CALL wrf_debug(2,'calling inc/PERIOD_EM_COUPLE_A_inline.inc')
IF ( config_flags%periodic_x ) THEN
CALL RSL_LITE_INIT_PERIOD ( local_communicator_periodic, 2 , &
     0, 3, 4, &
     0, 0, 4, &
     0, 0, 8, &
      0,  0, 4, &
      mytask, ntasks, ntasks_x, ntasks_y,   &
      ips, ipe, jps, jpe, kps, kpe    )
IF ( SIZE(grid%mub,1)*SIZE(grid%mub,2) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%mub, 2, 4, 1, 0, DATA_ORDER_XY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
ENDIF
IF ( SIZE(grid%mu_1,1)*SIZE(grid%mu_1,2) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%mu_1, 2, 4, 1, 0, DATA_ORDER_XY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
ENDIF
IF ( SIZE(grid%mu_2,1)*SIZE(grid%mu_2,2) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%mu_2, 2, 4, 1, 0, DATA_ORDER_XY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
ENDIF
   CALL RSL_LITE_EXCH_PERIOD_X ( local_communicator_periodic , mytask, ntasks, ntasks_x, ntasks_y )
IF ( SIZE(grid%mub,1)*SIZE(grid%mub,2) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%mub, 2, 4, 1, 1, DATA_ORDER_XY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
ENDIF
IF ( SIZE(grid%mu_1,1)*SIZE(grid%mu_1,2) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%mu_1, 2, 4, 1, 1, DATA_ORDER_XY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
ENDIF
IF ( SIZE(grid%mu_2,1)*SIZE(grid%mu_2,2) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%mu_2, 2, 4, 1, 1, DATA_ORDER_XY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
ENDIF
END IF
IF ( config_flags%periodic_y ) THEN
CALL RSL_LITE_INIT_PERIOD ( local_communicator_periodic, 2 , &
     0, 3, 4, &
     0, 0, 4, &
     0, 0, 8, &
      0,  0, 4, &
      mytask, ntasks, ntasks_x, ntasks_y,   &
      ips, ipe, jps, jpe, kps, kpe    )
IF ( SIZE(grid%mub,1)*SIZE(grid%mub,2) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%mub, 2, 4, 0, 0, DATA_ORDER_XY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
ENDIF
IF ( SIZE(grid%mu_1,1)*SIZE(grid%mu_1,2) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%mu_1, 2, 4, 0, 0, DATA_ORDER_XY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
ENDIF
IF ( SIZE(grid%mu_2,1)*SIZE(grid%mu_2,2) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%mu_2, 2, 4, 0, 0, DATA_ORDER_XY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
ENDIF
   CALL RSL_LITE_EXCH_PERIOD_Y ( local_communicator_periodic , mytask, ntasks, ntasks_x, ntasks_y )
IF ( SIZE(grid%mub,1)*SIZE(grid%mub,2) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%mub, 2, 4, 0, 1, DATA_ORDER_XY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
ENDIF
IF ( SIZE(grid%mu_1,1)*SIZE(grid%mu_1,2) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%mu_1, 2, 4, 0, 1, DATA_ORDER_XY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
ENDIF
IF ( SIZE(grid%mu_2,1)*SIZE(grid%mu_2,2) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%mu_2, 2, 4, 0, 1, DATA_ORDER_XY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
ENDIF
END IF

  
  END SUBROUTINE PERIOD_EM_COUPLE_A_sub

SUBROUTINE PERIOD_EM_COUPLE_B_sub ( grid, &
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
  USE module_domain, ONLY:domain
  USE module_configure, ONLY:grid_config_rec_type,in_use_for_config
  USE module_state_description, ONLY:PARAM_FIRST_SCALAR
  USE module_driver_constants
  TYPE(domain) ,               INTENT(IN) :: grid
  TYPE(grid_config_rec_type) , INTENT(IN) :: config_flags
  INTEGER, INTENT(IN) :: num_moist
  real, INTENT(INOUT) :: moist ( grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33,num_moist)
  INTEGER, INTENT(IN) :: num_chem
  real, INTENT(INOUT) :: chem ( grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33,num_chem)
  INTEGER, INTENT(IN) :: num_tracer
  real, INTENT(INOUT) :: tracer ( grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33,num_tracer)
  INTEGER, INTENT(IN) :: num_scalar
  real, INTENT(INOUT) :: scalar ( grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33,num_scalar)
  INTEGER ,                    INTENT(IN) :: local_communicator_periodic
  INTEGER ,                    INTENT(IN) :: mytask, ntasks, ntasks_x, ntasks_y
  INTEGER ,                    INTENT(IN) :: ids, ide, jds, jde, kds, kde
  INTEGER ,                    INTENT(IN) :: ims, ime, jms, jme, kms, kme
  INTEGER ,                    INTENT(IN) :: ips, ipe, jps, jpe, kps, kpe
  INTEGER :: itrace
  INTEGER :: rsl_sendw_p, rsl_sendbeg_p, rsl_recvw_p, rsl_recvbeg_p
  INTEGER :: rsl_sendw_m, rsl_sendbeg_m, rsl_recvw_m, rsl_recvbeg_m
  LOGICAL, EXTERNAL :: rsl_comm_iter
  INTEGER :: idim1, idim2, idim3, idim4, idim5, idim6, idim7
  






CALL wrf_debug(2,'calling inc/PERIOD_EM_COUPLE_B_inline.inc')
IF ( config_flags%periodic_x ) THEN
CALL RSL_LITE_INIT_PERIOD ( local_communicator_periodic, 3 , &
     10  &
   + num_moist   &
   + num_chem   &
   + num_tracer   &
   + num_scalar   &
     , 0, 4, &
     0, 0, 4, &
     0, 0, 8, &
      0,  0, 4, &
      mytask, ntasks, ntasks_x, ntasks_y,   &
      ips, ipe, jps, jpe, kps, kpe    )
IF ( SIZE(grid%ph_1,1)*SIZE(grid%ph_1,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%ph_1, 3, 4, 1, 0, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%ph_2,1)*SIZE(grid%ph_2,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%ph_2, 3, 4, 1, 0, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%w_1,1)*SIZE(grid%w_1,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%w_1, 3, 4, 1, 0, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%w_2,1)*SIZE(grid%w_2,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%w_2, 3, 4, 1, 0, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%t_1,1)*SIZE(grid%t_1,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%t_1, 3, 4, 1, 0, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%t_2,1)*SIZE(grid%t_2,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%t_2, 3, 4, 1, 0, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%u_1,1)*SIZE(grid%u_1,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%u_1, 3, 4, 1, 0, DATA_ORDER_XZY, 1, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%u_2,1)*SIZE(grid%u_2,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%u_2, 3, 4, 1, 0, DATA_ORDER_XZY, 1, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%v_1,1)*SIZE(grid%v_1,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%v_1, 3, 4, 1, 0, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%v_2,1)*SIZE(grid%v_2,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%v_2, 3, 4, 1, 0, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
DO itrace = PARAM_FIRST_SCALAR, num_moist
 IF ( SIZE(moist,1)*SIZE(moist,3) .GT. 1 ) THEN
  CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
moist ( grid%sm31,grid%sm32,grid%sm33,itrace), 3, 4, 1, 0, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
 ENDIF
ENDDO
DO itrace = PARAM_FIRST_SCALAR, num_chem
 IF ( SIZE(chem,1)*SIZE(chem,3) .GT. 1 ) THEN
  CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
chem ( grid%sm31,grid%sm32,grid%sm33,itrace), 3, 4, 1, 0, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
 ENDIF
ENDDO
DO itrace = PARAM_FIRST_SCALAR, num_tracer
 IF ( SIZE(tracer,1)*SIZE(tracer,3) .GT. 1 ) THEN
  CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
tracer ( grid%sm31,grid%sm32,grid%sm33,itrace), 3, 4, 1, 0, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
 ENDIF
ENDDO
DO itrace = PARAM_FIRST_SCALAR, num_scalar
 IF ( SIZE(scalar,1)*SIZE(scalar,3) .GT. 1 ) THEN
  CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
scalar ( grid%sm31,grid%sm32,grid%sm33,itrace), 3, 4, 1, 0, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
 ENDIF
ENDDO
   CALL RSL_LITE_EXCH_PERIOD_X ( local_communicator_periodic , mytask, ntasks, ntasks_x, ntasks_y )
IF ( SIZE(grid%ph_1,1)*SIZE(grid%ph_1,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%ph_1, 3, 4, 1, 1, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%ph_2,1)*SIZE(grid%ph_2,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%ph_2, 3, 4, 1, 1, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%w_1,1)*SIZE(grid%w_1,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%w_1, 3, 4, 1, 1, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%w_2,1)*SIZE(grid%w_2,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%w_2, 3, 4, 1, 1, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%t_1,1)*SIZE(grid%t_1,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%t_1, 3, 4, 1, 1, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%t_2,1)*SIZE(grid%t_2,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%t_2, 3, 4, 1, 1, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%u_1,1)*SIZE(grid%u_1,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%u_1, 3, 4, 1, 1, DATA_ORDER_XZY, 1, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%u_2,1)*SIZE(grid%u_2,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%u_2, 3, 4, 1, 1, DATA_ORDER_XZY, 1, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%v_1,1)*SIZE(grid%v_1,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%v_1, 3, 4, 1, 1, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%v_2,1)*SIZE(grid%v_2,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%v_2, 3, 4, 1, 1, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
DO itrace = PARAM_FIRST_SCALAR, num_moist
 IF ( SIZE(moist,1)*SIZE(moist,3) .GT. 1 ) THEN
  CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
moist ( grid%sm31,grid%sm32,grid%sm33,itrace), 3, 4, 1, 1, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
 ENDIF
ENDDO
DO itrace = PARAM_FIRST_SCALAR, num_chem
 IF ( SIZE(chem,1)*SIZE(chem,3) .GT. 1 ) THEN
  CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
chem ( grid%sm31,grid%sm32,grid%sm33,itrace), 3, 4, 1, 1, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
 ENDIF
ENDDO
DO itrace = PARAM_FIRST_SCALAR, num_tracer
 IF ( SIZE(tracer,1)*SIZE(tracer,3) .GT. 1 ) THEN
  CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
tracer ( grid%sm31,grid%sm32,grid%sm33,itrace), 3, 4, 1, 1, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
 ENDIF
ENDDO
DO itrace = PARAM_FIRST_SCALAR, num_scalar
 IF ( SIZE(scalar,1)*SIZE(scalar,3) .GT. 1 ) THEN
  CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
scalar ( grid%sm31,grid%sm32,grid%sm33,itrace), 3, 4, 1, 1, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
 ENDIF
ENDDO
END IF
IF ( config_flags%periodic_y ) THEN
CALL RSL_LITE_INIT_PERIOD ( local_communicator_periodic, 3 , &
     10  &
   + num_moist   &
   + num_chem   &
   + num_tracer   &
   + num_scalar   &
     , 0, 4, &
     0, 0, 4, &
     0, 0, 8, &
      0,  0, 4, &
      mytask, ntasks, ntasks_x, ntasks_y,   &
      ips, ipe, jps, jpe, kps, kpe    )
IF ( SIZE(grid%ph_1,1)*SIZE(grid%ph_1,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%ph_1, 3, 4, 0, 0, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%ph_2,1)*SIZE(grid%ph_2,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%ph_2, 3, 4, 0, 0, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%w_1,1)*SIZE(grid%w_1,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%w_1, 3, 4, 0, 0, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%w_2,1)*SIZE(grid%w_2,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%w_2, 3, 4, 0, 0, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%t_1,1)*SIZE(grid%t_1,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%t_1, 3, 4, 0, 0, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%t_2,1)*SIZE(grid%t_2,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%t_2, 3, 4, 0, 0, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%u_1,1)*SIZE(grid%u_1,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%u_1, 3, 4, 0, 0, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%u_2,1)*SIZE(grid%u_2,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%u_2, 3, 4, 0, 0, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%v_1,1)*SIZE(grid%v_1,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%v_1, 3, 4, 0, 0, DATA_ORDER_XZY, 1, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%v_2,1)*SIZE(grid%v_2,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%v_2, 3, 4, 0, 0, DATA_ORDER_XZY, 1, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
DO itrace = PARAM_FIRST_SCALAR, num_moist
 IF ( SIZE(moist,1)*SIZE(moist,3) .GT. 1 ) THEN
  CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
moist ( grid%sm31,grid%sm32,grid%sm33,itrace), 3, 4, 0, 0, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
 ENDIF
ENDDO
DO itrace = PARAM_FIRST_SCALAR, num_chem
 IF ( SIZE(chem,1)*SIZE(chem,3) .GT. 1 ) THEN
  CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
chem ( grid%sm31,grid%sm32,grid%sm33,itrace), 3, 4, 0, 0, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
 ENDIF
ENDDO
DO itrace = PARAM_FIRST_SCALAR, num_tracer
 IF ( SIZE(tracer,1)*SIZE(tracer,3) .GT. 1 ) THEN
  CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
tracer ( grid%sm31,grid%sm32,grid%sm33,itrace), 3, 4, 0, 0, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
 ENDIF
ENDDO
DO itrace = PARAM_FIRST_SCALAR, num_scalar
 IF ( SIZE(scalar,1)*SIZE(scalar,3) .GT. 1 ) THEN
  CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
scalar ( grid%sm31,grid%sm32,grid%sm33,itrace), 3, 4, 0, 0, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
 ENDIF
ENDDO
   CALL RSL_LITE_EXCH_PERIOD_Y ( local_communicator_periodic , mytask, ntasks, ntasks_x, ntasks_y )
IF ( SIZE(grid%ph_1,1)*SIZE(grid%ph_1,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%ph_1, 3, 4, 0, 1, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%ph_2,1)*SIZE(grid%ph_2,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%ph_2, 3, 4, 0, 1, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%w_1,1)*SIZE(grid%w_1,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%w_1, 3, 4, 0, 1, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%w_2,1)*SIZE(grid%w_2,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%w_2, 3, 4, 0, 1, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%t_1,1)*SIZE(grid%t_1,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%t_1, 3, 4, 0, 1, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%t_2,1)*SIZE(grid%t_2,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%t_2, 3, 4, 0, 1, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%u_1,1)*SIZE(grid%u_1,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%u_1, 3, 4, 0, 1, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%u_2,1)*SIZE(grid%u_2,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%u_2, 3, 4, 0, 1, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%v_1,1)*SIZE(grid%v_1,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%v_1, 3, 4, 0, 1, DATA_ORDER_XZY, 1, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%v_2,1)*SIZE(grid%v_2,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%v_2, 3, 4, 0, 1, DATA_ORDER_XZY, 1, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
DO itrace = PARAM_FIRST_SCALAR, num_moist
 IF ( SIZE(moist,1)*SIZE(moist,3) .GT. 1 ) THEN
  CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
moist ( grid%sm31,grid%sm32,grid%sm33,itrace), 3, 4, 0, 1, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
 ENDIF
ENDDO
DO itrace = PARAM_FIRST_SCALAR, num_chem
 IF ( SIZE(chem,1)*SIZE(chem,3) .GT. 1 ) THEN
  CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
chem ( grid%sm31,grid%sm32,grid%sm33,itrace), 3, 4, 0, 1, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
 ENDIF
ENDDO
DO itrace = PARAM_FIRST_SCALAR, num_tracer
 IF ( SIZE(tracer,1)*SIZE(tracer,3) .GT. 1 ) THEN
  CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
tracer ( grid%sm31,grid%sm32,grid%sm33,itrace), 3, 4, 0, 1, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
 ENDIF
ENDDO
DO itrace = PARAM_FIRST_SCALAR, num_scalar
 IF ( SIZE(scalar,1)*SIZE(scalar,3) .GT. 1 ) THEN
  CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
scalar ( grid%sm31,grid%sm32,grid%sm33,itrace), 3, 4, 0, 1, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
 ENDIF
ENDDO
END IF

  
  END SUBROUTINE PERIOD_EM_COUPLE_B_sub

SUBROUTINE PERIOD_BDY_EM_INIT_sub ( grid, &
  config_flags, &
  local_communicator_periodic, &
  mytask, ntasks, ntasks_x, ntasks_y, &
  ids, ide, jds, jde, kds, kde,       &
  ims, ime, jms, jme, kms, kme,       &
  ips, ipe, jps, jpe, kps, kpe )
  USE module_domain, ONLY:domain
  USE module_configure, ONLY:grid_config_rec_type,in_use_for_config
  USE module_state_description, ONLY:PARAM_FIRST_SCALAR
  USE module_driver_constants
  TYPE(domain) ,               INTENT(IN) :: grid
  TYPE(grid_config_rec_type) , INTENT(IN) :: config_flags
  INTEGER ,                    INTENT(IN) :: local_communicator_periodic
  INTEGER ,                    INTENT(IN) :: mytask, ntasks, ntasks_x, ntasks_y
  INTEGER ,                    INTENT(IN) :: ids, ide, jds, jde, kds, kde
  INTEGER ,                    INTENT(IN) :: ims, ime, jms, jme, kms, kme
  INTEGER ,                    INTENT(IN) :: ips, ipe, jps, jpe, kps, kpe
  INTEGER :: itrace
  INTEGER :: rsl_sendw_p, rsl_sendbeg_p, rsl_recvw_p, rsl_recvbeg_p
  INTEGER :: rsl_sendw_m, rsl_sendbeg_m, rsl_recvw_m, rsl_recvbeg_m
  LOGICAL, EXTERNAL :: rsl_comm_iter
  INTEGER :: idim1, idim2, idim3, idim4, idim5, idim6, idim7
  






CALL wrf_debug(2,'calling inc/PERIOD_BDY_EM_INIT_inline.inc')
IF ( config_flags%periodic_x ) THEN
CALL RSL_LITE_INIT_PERIOD ( local_communicator_periodic, 3 , &
     18, 16, 4, &
     0, 0, 4, &
     0, 0, 8, &
      0,  0, 4, &
      mytask, ntasks, ntasks_x, ntasks_y,   &
      ips, ipe, jps, jpe, kps, kpe    )
IF ( SIZE(grid%u_1,1)*SIZE(grid%u_1,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%u_1, 3, 4, 1, 0, DATA_ORDER_XZY, 1, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%u_2,1)*SIZE(grid%u_2,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%u_2, 3, 4, 1, 0, DATA_ORDER_XZY, 1, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%v_1,1)*SIZE(grid%v_1,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%v_1, 3, 4, 1, 0, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%v_2,1)*SIZE(grid%v_2,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%v_2, 3, 4, 1, 0, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%w_1,1)*SIZE(grid%w_1,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%w_1, 3, 4, 1, 0, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%w_2,1)*SIZE(grid%w_2,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%w_2, 3, 4, 1, 0, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%t_1,1)*SIZE(grid%t_1,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%t_1, 3, 4, 1, 0, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%t_2,1)*SIZE(grid%t_2,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%t_2, 3, 4, 1, 0, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%ph_1,1)*SIZE(grid%ph_1,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%ph_1, 3, 4, 1, 0, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%ph_2,1)*SIZE(grid%ph_2,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%ph_2, 3, 4, 1, 0, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%t_init,1)*SIZE(grid%t_init,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%t_init, 3, 4, 1, 0, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%phb,1)*SIZE(grid%phb,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%phb, 3, 4, 1, 0, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%ph0,1)*SIZE(grid%ph0,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%ph0, 3, 4, 1, 0, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%php,1)*SIZE(grid%php,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%php, 3, 4, 1, 0, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%pb,1)*SIZE(grid%pb,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%pb, 3, 4, 1, 0, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%al,1)*SIZE(grid%al,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%al, 3, 4, 1, 0, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%alt,1)*SIZE(grid%alt,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%alt, 3, 4, 1, 0, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%alb,1)*SIZE(grid%alb,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%alb, 3, 4, 1, 0, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%mu_1,1)*SIZE(grid%mu_1,2) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%mu_1, 3, 4, 1, 0, DATA_ORDER_XY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
ENDIF
IF ( SIZE(grid%mu_2,1)*SIZE(grid%mu_2,2) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%mu_2, 3, 4, 1, 0, DATA_ORDER_XY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
ENDIF
IF ( SIZE(grid%mub,1)*SIZE(grid%mub,2) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%mub, 3, 4, 1, 0, DATA_ORDER_XY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
ENDIF
IF ( SIZE(grid%mu0,1)*SIZE(grid%mu0,2) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%mu0, 3, 4, 1, 0, DATA_ORDER_XY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
ENDIF
IF ( SIZE(grid%ht,1)*SIZE(grid%ht,2) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%ht, 3, 4, 1, 0, DATA_ORDER_XY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
ENDIF
IF ( SIZE(grid%msftx,1)*SIZE(grid%msftx,2) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%msftx, 3, 4, 1, 0, DATA_ORDER_XY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
ENDIF
IF ( SIZE(grid%msfty,1)*SIZE(grid%msfty,2) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%msfty, 3, 4, 1, 0, DATA_ORDER_XY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
ENDIF
IF ( SIZE(grid%msfux,1)*SIZE(grid%msfux,2) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%msfux, 3, 4, 1, 0, DATA_ORDER_XY, 1, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
ENDIF
IF ( SIZE(grid%msfuy,1)*SIZE(grid%msfuy,2) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%msfuy, 3, 4, 1, 0, DATA_ORDER_XY, 1, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
ENDIF
IF ( SIZE(grid%msfvx,1)*SIZE(grid%msfvx,2) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%msfvx, 3, 4, 1, 0, DATA_ORDER_XY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
ENDIF
IF ( SIZE(grid%msfvy,1)*SIZE(grid%msfvy,2) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%msfvy, 3, 4, 1, 0, DATA_ORDER_XY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
ENDIF
IF ( SIZE(grid%msfvx_inv,1)*SIZE(grid%msfvx_inv,2) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%msfvx_inv, 3, 4, 1, 0, DATA_ORDER_XY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
ENDIF
IF ( SIZE(grid%sina,1)*SIZE(grid%sina,2) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%sina, 3, 4, 1, 0, DATA_ORDER_XY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
ENDIF
IF ( SIZE(grid%cosa,1)*SIZE(grid%cosa,2) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%cosa, 3, 4, 1, 0, DATA_ORDER_XY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
ENDIF
IF ( SIZE(grid%e,1)*SIZE(grid%e,2) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%e, 3, 4, 1, 0, DATA_ORDER_XY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
ENDIF
IF ( SIZE(grid%f,1)*SIZE(grid%f,2) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%f, 3, 4, 1, 0, DATA_ORDER_XY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
ENDIF
   CALL RSL_LITE_EXCH_PERIOD_X ( local_communicator_periodic , mytask, ntasks, ntasks_x, ntasks_y )
IF ( SIZE(grid%u_1,1)*SIZE(grid%u_1,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%u_1, 3, 4, 1, 1, DATA_ORDER_XZY, 1, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%u_2,1)*SIZE(grid%u_2,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%u_2, 3, 4, 1, 1, DATA_ORDER_XZY, 1, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%v_1,1)*SIZE(grid%v_1,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%v_1, 3, 4, 1, 1, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%v_2,1)*SIZE(grid%v_2,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%v_2, 3, 4, 1, 1, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%w_1,1)*SIZE(grid%w_1,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%w_1, 3, 4, 1, 1, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%w_2,1)*SIZE(grid%w_2,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%w_2, 3, 4, 1, 1, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%t_1,1)*SIZE(grid%t_1,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%t_1, 3, 4, 1, 1, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%t_2,1)*SIZE(grid%t_2,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%t_2, 3, 4, 1, 1, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%ph_1,1)*SIZE(grid%ph_1,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%ph_1, 3, 4, 1, 1, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%ph_2,1)*SIZE(grid%ph_2,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%ph_2, 3, 4, 1, 1, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%t_init,1)*SIZE(grid%t_init,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%t_init, 3, 4, 1, 1, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%phb,1)*SIZE(grid%phb,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%phb, 3, 4, 1, 1, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%ph0,1)*SIZE(grid%ph0,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%ph0, 3, 4, 1, 1, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%php,1)*SIZE(grid%php,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%php, 3, 4, 1, 1, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%pb,1)*SIZE(grid%pb,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%pb, 3, 4, 1, 1, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%al,1)*SIZE(grid%al,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%al, 3, 4, 1, 1, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%alt,1)*SIZE(grid%alt,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%alt, 3, 4, 1, 1, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%alb,1)*SIZE(grid%alb,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%alb, 3, 4, 1, 1, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%mu_1,1)*SIZE(grid%mu_1,2) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%mu_1, 3, 4, 1, 1, DATA_ORDER_XY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
ENDIF
IF ( SIZE(grid%mu_2,1)*SIZE(grid%mu_2,2) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%mu_2, 3, 4, 1, 1, DATA_ORDER_XY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
ENDIF
IF ( SIZE(grid%mub,1)*SIZE(grid%mub,2) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%mub, 3, 4, 1, 1, DATA_ORDER_XY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
ENDIF
IF ( SIZE(grid%mu0,1)*SIZE(grid%mu0,2) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%mu0, 3, 4, 1, 1, DATA_ORDER_XY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
ENDIF
IF ( SIZE(grid%ht,1)*SIZE(grid%ht,2) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%ht, 3, 4, 1, 1, DATA_ORDER_XY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
ENDIF
IF ( SIZE(grid%msftx,1)*SIZE(grid%msftx,2) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%msftx, 3, 4, 1, 1, DATA_ORDER_XY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
ENDIF
IF ( SIZE(grid%msfty,1)*SIZE(grid%msfty,2) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%msfty, 3, 4, 1, 1, DATA_ORDER_XY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
ENDIF
IF ( SIZE(grid%msfux,1)*SIZE(grid%msfux,2) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%msfux, 3, 4, 1, 1, DATA_ORDER_XY, 1, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
ENDIF
IF ( SIZE(grid%msfuy,1)*SIZE(grid%msfuy,2) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%msfuy, 3, 4, 1, 1, DATA_ORDER_XY, 1, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
ENDIF
IF ( SIZE(grid%msfvx,1)*SIZE(grid%msfvx,2) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%msfvx, 3, 4, 1, 1, DATA_ORDER_XY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
ENDIF
IF ( SIZE(grid%msfvy,1)*SIZE(grid%msfvy,2) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%msfvy, 3, 4, 1, 1, DATA_ORDER_XY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
ENDIF
IF ( SIZE(grid%msfvx_inv,1)*SIZE(grid%msfvx_inv,2) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%msfvx_inv, 3, 4, 1, 1, DATA_ORDER_XY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
ENDIF
IF ( SIZE(grid%sina,1)*SIZE(grid%sina,2) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%sina, 3, 4, 1, 1, DATA_ORDER_XY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
ENDIF
IF ( SIZE(grid%cosa,1)*SIZE(grid%cosa,2) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%cosa, 3, 4, 1, 1, DATA_ORDER_XY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
ENDIF
IF ( SIZE(grid%e,1)*SIZE(grid%e,2) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%e, 3, 4, 1, 1, DATA_ORDER_XY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
ENDIF
IF ( SIZE(grid%f,1)*SIZE(grid%f,2) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%f, 3, 4, 1, 1, DATA_ORDER_XY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
ENDIF
END IF
IF ( config_flags%periodic_y ) THEN
CALL RSL_LITE_INIT_PERIOD ( local_communicator_periodic, 3 , &
     18, 16, 4, &
     0, 0, 4, &
     0, 0, 8, &
      0,  0, 4, &
      mytask, ntasks, ntasks_x, ntasks_y,   &
      ips, ipe, jps, jpe, kps, kpe    )
IF ( SIZE(grid%u_1,1)*SIZE(grid%u_1,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%u_1, 3, 4, 0, 0, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%u_2,1)*SIZE(grid%u_2,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%u_2, 3, 4, 0, 0, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%v_1,1)*SIZE(grid%v_1,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%v_1, 3, 4, 0, 0, DATA_ORDER_XZY, 1, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%v_2,1)*SIZE(grid%v_2,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%v_2, 3, 4, 0, 0, DATA_ORDER_XZY, 1, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%w_1,1)*SIZE(grid%w_1,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%w_1, 3, 4, 0, 0, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%w_2,1)*SIZE(grid%w_2,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%w_2, 3, 4, 0, 0, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%t_1,1)*SIZE(grid%t_1,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%t_1, 3, 4, 0, 0, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%t_2,1)*SIZE(grid%t_2,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%t_2, 3, 4, 0, 0, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%ph_1,1)*SIZE(grid%ph_1,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%ph_1, 3, 4, 0, 0, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%ph_2,1)*SIZE(grid%ph_2,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%ph_2, 3, 4, 0, 0, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%t_init,1)*SIZE(grid%t_init,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%t_init, 3, 4, 0, 0, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%phb,1)*SIZE(grid%phb,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%phb, 3, 4, 0, 0, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%ph0,1)*SIZE(grid%ph0,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%ph0, 3, 4, 0, 0, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%php,1)*SIZE(grid%php,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%php, 3, 4, 0, 0, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%pb,1)*SIZE(grid%pb,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%pb, 3, 4, 0, 0, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%al,1)*SIZE(grid%al,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%al, 3, 4, 0, 0, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%alt,1)*SIZE(grid%alt,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%alt, 3, 4, 0, 0, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%alb,1)*SIZE(grid%alb,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%alb, 3, 4, 0, 0, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%mu_1,1)*SIZE(grid%mu_1,2) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%mu_1, 3, 4, 0, 0, DATA_ORDER_XY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
ENDIF
IF ( SIZE(grid%mu_2,1)*SIZE(grid%mu_2,2) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%mu_2, 3, 4, 0, 0, DATA_ORDER_XY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
ENDIF
IF ( SIZE(grid%mub,1)*SIZE(grid%mub,2) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%mub, 3, 4, 0, 0, DATA_ORDER_XY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
ENDIF
IF ( SIZE(grid%mu0,1)*SIZE(grid%mu0,2) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%mu0, 3, 4, 0, 0, DATA_ORDER_XY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
ENDIF
IF ( SIZE(grid%ht,1)*SIZE(grid%ht,2) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%ht, 3, 4, 0, 0, DATA_ORDER_XY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
ENDIF
IF ( SIZE(grid%msftx,1)*SIZE(grid%msftx,2) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%msftx, 3, 4, 0, 0, DATA_ORDER_XY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
ENDIF
IF ( SIZE(grid%msfty,1)*SIZE(grid%msfty,2) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%msfty, 3, 4, 0, 0, DATA_ORDER_XY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
ENDIF
IF ( SIZE(grid%msfux,1)*SIZE(grid%msfux,2) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%msfux, 3, 4, 0, 0, DATA_ORDER_XY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
ENDIF
IF ( SIZE(grid%msfuy,1)*SIZE(grid%msfuy,2) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%msfuy, 3, 4, 0, 0, DATA_ORDER_XY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
ENDIF
IF ( SIZE(grid%msfvx,1)*SIZE(grid%msfvx,2) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%msfvx, 3, 4, 0, 0, DATA_ORDER_XY, 1, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
ENDIF
IF ( SIZE(grid%msfvy,1)*SIZE(grid%msfvy,2) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%msfvy, 3, 4, 0, 0, DATA_ORDER_XY, 1, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
ENDIF
IF ( SIZE(grid%msfvx_inv,1)*SIZE(grid%msfvx_inv,2) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%msfvx_inv, 3, 4, 0, 0, DATA_ORDER_XY, 1, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
ENDIF
IF ( SIZE(grid%sina,1)*SIZE(grid%sina,2) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%sina, 3, 4, 0, 0, DATA_ORDER_XY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
ENDIF
IF ( SIZE(grid%cosa,1)*SIZE(grid%cosa,2) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%cosa, 3, 4, 0, 0, DATA_ORDER_XY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
ENDIF
IF ( SIZE(grid%e,1)*SIZE(grid%e,2) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%e, 3, 4, 0, 0, DATA_ORDER_XY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
ENDIF
IF ( SIZE(grid%f,1)*SIZE(grid%f,2) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%f, 3, 4, 0, 0, DATA_ORDER_XY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
ENDIF
   CALL RSL_LITE_EXCH_PERIOD_Y ( local_communicator_periodic , mytask, ntasks, ntasks_x, ntasks_y )
IF ( SIZE(grid%u_1,1)*SIZE(grid%u_1,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%u_1, 3, 4, 0, 1, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%u_2,1)*SIZE(grid%u_2,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%u_2, 3, 4, 0, 1, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%v_1,1)*SIZE(grid%v_1,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%v_1, 3, 4, 0, 1, DATA_ORDER_XZY, 1, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%v_2,1)*SIZE(grid%v_2,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%v_2, 3, 4, 0, 1, DATA_ORDER_XZY, 1, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%w_1,1)*SIZE(grid%w_1,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%w_1, 3, 4, 0, 1, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%w_2,1)*SIZE(grid%w_2,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%w_2, 3, 4, 0, 1, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%t_1,1)*SIZE(grid%t_1,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%t_1, 3, 4, 0, 1, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%t_2,1)*SIZE(grid%t_2,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%t_2, 3, 4, 0, 1, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%ph_1,1)*SIZE(grid%ph_1,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%ph_1, 3, 4, 0, 1, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%ph_2,1)*SIZE(grid%ph_2,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%ph_2, 3, 4, 0, 1, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%t_init,1)*SIZE(grid%t_init,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%t_init, 3, 4, 0, 1, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%phb,1)*SIZE(grid%phb,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%phb, 3, 4, 0, 1, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%ph0,1)*SIZE(grid%ph0,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%ph0, 3, 4, 0, 1, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%php,1)*SIZE(grid%php,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%php, 3, 4, 0, 1, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%pb,1)*SIZE(grid%pb,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%pb, 3, 4, 0, 1, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%al,1)*SIZE(grid%al,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%al, 3, 4, 0, 1, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%alt,1)*SIZE(grid%alt,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%alt, 3, 4, 0, 1, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%alb,1)*SIZE(grid%alb,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%alb, 3, 4, 0, 1, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%mu_1,1)*SIZE(grid%mu_1,2) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%mu_1, 3, 4, 0, 1, DATA_ORDER_XY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
ENDIF
IF ( SIZE(grid%mu_2,1)*SIZE(grid%mu_2,2) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%mu_2, 3, 4, 0, 1, DATA_ORDER_XY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
ENDIF
IF ( SIZE(grid%mub,1)*SIZE(grid%mub,2) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%mub, 3, 4, 0, 1, DATA_ORDER_XY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
ENDIF
IF ( SIZE(grid%mu0,1)*SIZE(grid%mu0,2) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%mu0, 3, 4, 0, 1, DATA_ORDER_XY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
ENDIF
IF ( SIZE(grid%ht,1)*SIZE(grid%ht,2) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%ht, 3, 4, 0, 1, DATA_ORDER_XY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
ENDIF
IF ( SIZE(grid%msftx,1)*SIZE(grid%msftx,2) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%msftx, 3, 4, 0, 1, DATA_ORDER_XY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
ENDIF
IF ( SIZE(grid%msfty,1)*SIZE(grid%msfty,2) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%msfty, 3, 4, 0, 1, DATA_ORDER_XY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
ENDIF
IF ( SIZE(grid%msfux,1)*SIZE(grid%msfux,2) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%msfux, 3, 4, 0, 1, DATA_ORDER_XY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
ENDIF
IF ( SIZE(grid%msfuy,1)*SIZE(grid%msfuy,2) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%msfuy, 3, 4, 0, 1, DATA_ORDER_XY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
ENDIF
IF ( SIZE(grid%msfvx,1)*SIZE(grid%msfvx,2) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%msfvx, 3, 4, 0, 1, DATA_ORDER_XY, 1, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
ENDIF
IF ( SIZE(grid%msfvy,1)*SIZE(grid%msfvy,2) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%msfvy, 3, 4, 0, 1, DATA_ORDER_XY, 1, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
ENDIF
IF ( SIZE(grid%msfvx_inv,1)*SIZE(grid%msfvx_inv,2) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%msfvx_inv, 3, 4, 0, 1, DATA_ORDER_XY, 1, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
ENDIF
IF ( SIZE(grid%sina,1)*SIZE(grid%sina,2) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%sina, 3, 4, 0, 1, DATA_ORDER_XY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
ENDIF
IF ( SIZE(grid%cosa,1)*SIZE(grid%cosa,2) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%cosa, 3, 4, 0, 1, DATA_ORDER_XY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
ENDIF
IF ( SIZE(grid%e,1)*SIZE(grid%e,2) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%e, 3, 4, 0, 1, DATA_ORDER_XY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
ENDIF
IF ( SIZE(grid%f,1)*SIZE(grid%f,2) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%f, 3, 4, 0, 1, DATA_ORDER_XY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
ENDIF
END IF

  
  END SUBROUTINE PERIOD_BDY_EM_INIT_sub

SUBROUTINE PERIOD_BDY_EM_MOIST_sub ( grid, &
  config_flags, &
  num_moist, &
  moist, &
  local_communicator_periodic, &
  mytask, ntasks, ntasks_x, ntasks_y, &
  ids, ide, jds, jde, kds, kde,       &
  ims, ime, jms, jme, kms, kme,       &
  ips, ipe, jps, jpe, kps, kpe )
  USE module_domain, ONLY:domain
  USE module_configure, ONLY:grid_config_rec_type,in_use_for_config
  USE module_state_description, ONLY:PARAM_FIRST_SCALAR
  USE module_driver_constants
  TYPE(domain) ,               INTENT(IN) :: grid
  TYPE(grid_config_rec_type) , INTENT(IN) :: config_flags
  INTEGER, INTENT(IN) :: num_moist
  real, INTENT(INOUT) :: moist ( grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33,num_moist)
  INTEGER ,                    INTENT(IN) :: local_communicator_periodic
  INTEGER ,                    INTENT(IN) :: mytask, ntasks, ntasks_x, ntasks_y
  INTEGER ,                    INTENT(IN) :: ids, ide, jds, jde, kds, kde
  INTEGER ,                    INTENT(IN) :: ims, ime, jms, jme, kms, kme
  INTEGER ,                    INTENT(IN) :: ips, ipe, jps, jpe, kps, kpe
  INTEGER :: itrace
  INTEGER :: rsl_sendw_p, rsl_sendbeg_p, rsl_recvw_p, rsl_recvbeg_p
  INTEGER :: rsl_sendw_m, rsl_sendbeg_m, rsl_recvw_m, rsl_recvbeg_m
  LOGICAL, EXTERNAL :: rsl_comm_iter
  INTEGER :: idim1, idim2, idim3, idim4, idim5, idim6, idim7
  






CALL wrf_debug(2,'calling inc/PERIOD_BDY_EM_MOIST_inline.inc')
IF ( config_flags%periodic_x ) THEN
CALL RSL_LITE_INIT_PERIOD ( local_communicator_periodic, 4 , &
     0  &
   + num_moist   &
     , 0, 4, &
     0, 0, 4, &
     0, 0, 8, &
      0,  0, 4, &
      mytask, ntasks, ntasks_x, ntasks_y,   &
      ips, ipe, jps, jpe, kps, kpe    )
DO itrace = PARAM_FIRST_SCALAR, num_moist
 IF ( SIZE(moist,1)*SIZE(moist,3) .GT. 1 ) THEN
  CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
moist ( grid%sm31,grid%sm32,grid%sm33,itrace), 4, 4, 1, 0, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
 ENDIF
ENDDO
   CALL RSL_LITE_EXCH_PERIOD_X ( local_communicator_periodic , mytask, ntasks, ntasks_x, ntasks_y )
DO itrace = PARAM_FIRST_SCALAR, num_moist
 IF ( SIZE(moist,1)*SIZE(moist,3) .GT. 1 ) THEN
  CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
moist ( grid%sm31,grid%sm32,grid%sm33,itrace), 4, 4, 1, 1, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
 ENDIF
ENDDO
END IF
IF ( config_flags%periodic_y ) THEN
CALL RSL_LITE_INIT_PERIOD ( local_communicator_periodic, 4 , &
     0  &
   + num_moist   &
     , 0, 4, &
     0, 0, 4, &
     0, 0, 8, &
      0,  0, 4, &
      mytask, ntasks, ntasks_x, ntasks_y,   &
      ips, ipe, jps, jpe, kps, kpe    )
DO itrace = PARAM_FIRST_SCALAR, num_moist
 IF ( SIZE(moist,1)*SIZE(moist,3) .GT. 1 ) THEN
  CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
moist ( grid%sm31,grid%sm32,grid%sm33,itrace), 4, 4, 0, 0, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
 ENDIF
ENDDO
   CALL RSL_LITE_EXCH_PERIOD_Y ( local_communicator_periodic , mytask, ntasks, ntasks_x, ntasks_y )
DO itrace = PARAM_FIRST_SCALAR, num_moist
 IF ( SIZE(moist,1)*SIZE(moist,3) .GT. 1 ) THEN
  CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
moist ( grid%sm31,grid%sm32,grid%sm33,itrace), 4, 4, 0, 1, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
 ENDIF
ENDDO
END IF

  
  END SUBROUTINE PERIOD_BDY_EM_MOIST_sub

SUBROUTINE PERIOD_BDY_EM_CHEM_sub ( grid, &
  config_flags, &
  num_chem, &
  chem, &
  local_communicator_periodic, &
  mytask, ntasks, ntasks_x, ntasks_y, &
  ids, ide, jds, jde, kds, kde,       &
  ims, ime, jms, jme, kms, kme,       &
  ips, ipe, jps, jpe, kps, kpe )
  USE module_domain, ONLY:domain
  USE module_configure, ONLY:grid_config_rec_type,in_use_for_config
  USE module_state_description, ONLY:PARAM_FIRST_SCALAR
  USE module_driver_constants
  TYPE(domain) ,               INTENT(IN) :: grid
  TYPE(grid_config_rec_type) , INTENT(IN) :: config_flags
  INTEGER, INTENT(IN) :: num_chem
  real, INTENT(INOUT) :: chem ( grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33,num_chem)
  INTEGER ,                    INTENT(IN) :: local_communicator_periodic
  INTEGER ,                    INTENT(IN) :: mytask, ntasks, ntasks_x, ntasks_y
  INTEGER ,                    INTENT(IN) :: ids, ide, jds, jde, kds, kde
  INTEGER ,                    INTENT(IN) :: ims, ime, jms, jme, kms, kme
  INTEGER ,                    INTENT(IN) :: ips, ipe, jps, jpe, kps, kpe
  INTEGER :: itrace
  INTEGER :: rsl_sendw_p, rsl_sendbeg_p, rsl_recvw_p, rsl_recvbeg_p
  INTEGER :: rsl_sendw_m, rsl_sendbeg_m, rsl_recvw_m, rsl_recvbeg_m
  LOGICAL, EXTERNAL :: rsl_comm_iter
  INTEGER :: idim1, idim2, idim3, idim4, idim5, idim6, idim7
  






CALL wrf_debug(2,'calling inc/PERIOD_BDY_EM_CHEM_inline.inc')
IF ( config_flags%periodic_x ) THEN
CALL RSL_LITE_INIT_PERIOD ( local_communicator_periodic, 4 , &
     0  &
   + num_chem   &
     , 0, 4, &
     0, 0, 4, &
     0, 0, 8, &
      0,  0, 4, &
      mytask, ntasks, ntasks_x, ntasks_y,   &
      ips, ipe, jps, jpe, kps, kpe    )
DO itrace = PARAM_FIRST_SCALAR, num_chem
 IF ( SIZE(chem,1)*SIZE(chem,3) .GT. 1 ) THEN
  CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
chem ( grid%sm31,grid%sm32,grid%sm33,itrace), 4, 4, 1, 0, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
 ENDIF
ENDDO
   CALL RSL_LITE_EXCH_PERIOD_X ( local_communicator_periodic , mytask, ntasks, ntasks_x, ntasks_y )
DO itrace = PARAM_FIRST_SCALAR, num_chem
 IF ( SIZE(chem,1)*SIZE(chem,3) .GT. 1 ) THEN
  CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
chem ( grid%sm31,grid%sm32,grid%sm33,itrace), 4, 4, 1, 1, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
 ENDIF
ENDDO
END IF
IF ( config_flags%periodic_y ) THEN
CALL RSL_LITE_INIT_PERIOD ( local_communicator_periodic, 4 , &
     0  &
   + num_chem   &
     , 0, 4, &
     0, 0, 4, &
     0, 0, 8, &
      0,  0, 4, &
      mytask, ntasks, ntasks_x, ntasks_y,   &
      ips, ipe, jps, jpe, kps, kpe    )
DO itrace = PARAM_FIRST_SCALAR, num_chem
 IF ( SIZE(chem,1)*SIZE(chem,3) .GT. 1 ) THEN
  CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
chem ( grid%sm31,grid%sm32,grid%sm33,itrace), 4, 4, 0, 0, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
 ENDIF
ENDDO
   CALL RSL_LITE_EXCH_PERIOD_Y ( local_communicator_periodic , mytask, ntasks, ntasks_x, ntasks_y )
DO itrace = PARAM_FIRST_SCALAR, num_chem
 IF ( SIZE(chem,1)*SIZE(chem,3) .GT. 1 ) THEN
  CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
chem ( grid%sm31,grid%sm32,grid%sm33,itrace), 4, 4, 0, 1, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
 ENDIF
ENDDO
END IF

  
  END SUBROUTINE PERIOD_BDY_EM_CHEM_sub

SUBROUTINE PERIOD_BDY_EM_TRACER_sub ( grid, &
  config_flags, &
  num_tracer, &
  tracer, &
  local_communicator_periodic, &
  mytask, ntasks, ntasks_x, ntasks_y, &
  ids, ide, jds, jde, kds, kde,       &
  ims, ime, jms, jme, kms, kme,       &
  ips, ipe, jps, jpe, kps, kpe )
  USE module_domain, ONLY:domain
  USE module_configure, ONLY:grid_config_rec_type,in_use_for_config
  USE module_state_description, ONLY:PARAM_FIRST_SCALAR
  USE module_driver_constants
  TYPE(domain) ,               INTENT(IN) :: grid
  TYPE(grid_config_rec_type) , INTENT(IN) :: config_flags
  INTEGER, INTENT(IN) :: num_tracer
  real, INTENT(INOUT) :: tracer ( grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33,num_tracer)
  INTEGER ,                    INTENT(IN) :: local_communicator_periodic
  INTEGER ,                    INTENT(IN) :: mytask, ntasks, ntasks_x, ntasks_y
  INTEGER ,                    INTENT(IN) :: ids, ide, jds, jde, kds, kde
  INTEGER ,                    INTENT(IN) :: ims, ime, jms, jme, kms, kme
  INTEGER ,                    INTENT(IN) :: ips, ipe, jps, jpe, kps, kpe
  INTEGER :: itrace
  INTEGER :: rsl_sendw_p, rsl_sendbeg_p, rsl_recvw_p, rsl_recvbeg_p
  INTEGER :: rsl_sendw_m, rsl_sendbeg_m, rsl_recvw_m, rsl_recvbeg_m
  LOGICAL, EXTERNAL :: rsl_comm_iter
  INTEGER :: idim1, idim2, idim3, idim4, idim5, idim6, idim7
  






CALL wrf_debug(2,'calling inc/PERIOD_BDY_EM_TRACER_inline.inc')
IF ( config_flags%periodic_x ) THEN
CALL RSL_LITE_INIT_PERIOD ( local_communicator_periodic, 4 , &
     0  &
   + num_tracer   &
     , 0, 4, &
     0, 0, 4, &
     0, 0, 8, &
      0,  0, 4, &
      mytask, ntasks, ntasks_x, ntasks_y,   &
      ips, ipe, jps, jpe, kps, kpe    )
DO itrace = PARAM_FIRST_SCALAR, num_tracer
 IF ( SIZE(tracer,1)*SIZE(tracer,3) .GT. 1 ) THEN
  CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
tracer ( grid%sm31,grid%sm32,grid%sm33,itrace), 4, 4, 1, 0, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
 ENDIF
ENDDO
   CALL RSL_LITE_EXCH_PERIOD_X ( local_communicator_periodic , mytask, ntasks, ntasks_x, ntasks_y )
DO itrace = PARAM_FIRST_SCALAR, num_tracer
 IF ( SIZE(tracer,1)*SIZE(tracer,3) .GT. 1 ) THEN
  CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
tracer ( grid%sm31,grid%sm32,grid%sm33,itrace), 4, 4, 1, 1, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
 ENDIF
ENDDO
END IF
IF ( config_flags%periodic_y ) THEN
CALL RSL_LITE_INIT_PERIOD ( local_communicator_periodic, 4 , &
     0  &
   + num_tracer   &
     , 0, 4, &
     0, 0, 4, &
     0, 0, 8, &
      0,  0, 4, &
      mytask, ntasks, ntasks_x, ntasks_y,   &
      ips, ipe, jps, jpe, kps, kpe    )
DO itrace = PARAM_FIRST_SCALAR, num_tracer
 IF ( SIZE(tracer,1)*SIZE(tracer,3) .GT. 1 ) THEN
  CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
tracer ( grid%sm31,grid%sm32,grid%sm33,itrace), 4, 4, 0, 0, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
 ENDIF
ENDDO
   CALL RSL_LITE_EXCH_PERIOD_Y ( local_communicator_periodic , mytask, ntasks, ntasks_x, ntasks_y )
DO itrace = PARAM_FIRST_SCALAR, num_tracer
 IF ( SIZE(tracer,1)*SIZE(tracer,3) .GT. 1 ) THEN
  CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
tracer ( grid%sm31,grid%sm32,grid%sm33,itrace), 4, 4, 0, 1, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
 ENDIF
ENDDO
END IF

  
  END SUBROUTINE PERIOD_BDY_EM_TRACER_sub

SUBROUTINE PERIOD_BDY_EM_SCALAR_sub ( grid, &
  config_flags, &
  num_scalar, &
  scalar, &
  local_communicator_periodic, &
  mytask, ntasks, ntasks_x, ntasks_y, &
  ids, ide, jds, jde, kds, kde,       &
  ims, ime, jms, jme, kms, kme,       &
  ips, ipe, jps, jpe, kps, kpe )
  USE module_domain, ONLY:domain
  USE module_configure, ONLY:grid_config_rec_type,in_use_for_config
  USE module_state_description, ONLY:PARAM_FIRST_SCALAR
  USE module_driver_constants
  TYPE(domain) ,               INTENT(IN) :: grid
  TYPE(grid_config_rec_type) , INTENT(IN) :: config_flags
  INTEGER, INTENT(IN) :: num_scalar
  real, INTENT(INOUT) :: scalar ( grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33,num_scalar)
  INTEGER ,                    INTENT(IN) :: local_communicator_periodic
  INTEGER ,                    INTENT(IN) :: mytask, ntasks, ntasks_x, ntasks_y
  INTEGER ,                    INTENT(IN) :: ids, ide, jds, jde, kds, kde
  INTEGER ,                    INTENT(IN) :: ims, ime, jms, jme, kms, kme
  INTEGER ,                    INTENT(IN) :: ips, ipe, jps, jpe, kps, kpe
  INTEGER :: itrace
  INTEGER :: rsl_sendw_p, rsl_sendbeg_p, rsl_recvw_p, rsl_recvbeg_p
  INTEGER :: rsl_sendw_m, rsl_sendbeg_m, rsl_recvw_m, rsl_recvbeg_m
  LOGICAL, EXTERNAL :: rsl_comm_iter
  INTEGER :: idim1, idim2, idim3, idim4, idim5, idim6, idim7
  






CALL wrf_debug(2,'calling inc/PERIOD_BDY_EM_SCALAR_inline.inc')
IF ( config_flags%periodic_x ) THEN
CALL RSL_LITE_INIT_PERIOD ( local_communicator_periodic, 4 , &
     0  &
   + num_scalar   &
     , 0, 4, &
     0, 0, 4, &
     0, 0, 8, &
      0,  0, 4, &
      mytask, ntasks, ntasks_x, ntasks_y,   &
      ips, ipe, jps, jpe, kps, kpe    )
DO itrace = PARAM_FIRST_SCALAR, num_scalar
 IF ( SIZE(scalar,1)*SIZE(scalar,3) .GT. 1 ) THEN
  CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
scalar ( grid%sm31,grid%sm32,grid%sm33,itrace), 4, 4, 1, 0, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
 ENDIF
ENDDO
   CALL RSL_LITE_EXCH_PERIOD_X ( local_communicator_periodic , mytask, ntasks, ntasks_x, ntasks_y )
DO itrace = PARAM_FIRST_SCALAR, num_scalar
 IF ( SIZE(scalar,1)*SIZE(scalar,3) .GT. 1 ) THEN
  CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
scalar ( grid%sm31,grid%sm32,grid%sm33,itrace), 4, 4, 1, 1, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
 ENDIF
ENDDO
END IF
IF ( config_flags%periodic_y ) THEN
CALL RSL_LITE_INIT_PERIOD ( local_communicator_periodic, 4 , &
     0  &
   + num_scalar   &
     , 0, 4, &
     0, 0, 4, &
     0, 0, 8, &
      0,  0, 4, &
      mytask, ntasks, ntasks_x, ntasks_y,   &
      ips, ipe, jps, jpe, kps, kpe    )
DO itrace = PARAM_FIRST_SCALAR, num_scalar
 IF ( SIZE(scalar,1)*SIZE(scalar,3) .GT. 1 ) THEN
  CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
scalar ( grid%sm31,grid%sm32,grid%sm33,itrace), 4, 4, 0, 0, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
 ENDIF
ENDDO
   CALL RSL_LITE_EXCH_PERIOD_Y ( local_communicator_periodic , mytask, ntasks, ntasks_x, ntasks_y )
DO itrace = PARAM_FIRST_SCALAR, num_scalar
 IF ( SIZE(scalar,1)*SIZE(scalar,3) .GT. 1 ) THEN
  CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
scalar ( grid%sm31,grid%sm32,grid%sm33,itrace), 4, 4, 0, 1, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
 ENDIF
ENDDO
END IF

  
  END SUBROUTINE PERIOD_BDY_EM_SCALAR_sub

SUBROUTINE PERIOD_BDY_EM_TKE_sub ( grid, &
  config_flags, &
  local_communicator_periodic, &
  mytask, ntasks, ntasks_x, ntasks_y, &
  ids, ide, jds, jde, kds, kde,       &
  ims, ime, jms, jme, kms, kme,       &
  ips, ipe, jps, jpe, kps, kpe )
  USE module_domain, ONLY:domain
  USE module_configure, ONLY:grid_config_rec_type,in_use_for_config
  USE module_state_description, ONLY:PARAM_FIRST_SCALAR
  USE module_driver_constants
  TYPE(domain) ,               INTENT(IN) :: grid
  TYPE(grid_config_rec_type) , INTENT(IN) :: config_flags
  INTEGER ,                    INTENT(IN) :: local_communicator_periodic
  INTEGER ,                    INTENT(IN) :: mytask, ntasks, ntasks_x, ntasks_y
  INTEGER ,                    INTENT(IN) :: ids, ide, jds, jde, kds, kde
  INTEGER ,                    INTENT(IN) :: ims, ime, jms, jme, kms, kme
  INTEGER ,                    INTENT(IN) :: ips, ipe, jps, jpe, kps, kpe
  INTEGER :: itrace
  INTEGER :: rsl_sendw_p, rsl_sendbeg_p, rsl_recvw_p, rsl_recvbeg_p
  INTEGER :: rsl_sendw_m, rsl_sendbeg_m, rsl_recvw_m, rsl_recvbeg_m
  LOGICAL, EXTERNAL :: rsl_comm_iter
  INTEGER :: idim1, idim2, idim3, idim4, idim5, idim6, idim7
  






CALL wrf_debug(2,'calling inc/PERIOD_BDY_EM_TKE_inline.inc')
IF ( config_flags%periodic_x ) THEN
CALL RSL_LITE_INIT_PERIOD ( local_communicator_periodic, 4 , &
     1, 0, 4, &
     0, 0, 4, &
     0, 0, 8, &
      0,  0, 4, &
      mytask, ntasks, ntasks_x, ntasks_y,   &
      ips, ipe, jps, jpe, kps, kpe    )
IF ( SIZE(grid%tke_2,1)*SIZE(grid%tke_2,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%tke_2, 4, 4, 1, 0, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
   CALL RSL_LITE_EXCH_PERIOD_X ( local_communicator_periodic , mytask, ntasks, ntasks_x, ntasks_y )
IF ( SIZE(grid%tke_2,1)*SIZE(grid%tke_2,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%tke_2, 4, 4, 1, 1, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
END IF
IF ( config_flags%periodic_y ) THEN
CALL RSL_LITE_INIT_PERIOD ( local_communicator_periodic, 4 , &
     1, 0, 4, &
     0, 0, 4, &
     0, 0, 8, &
      0,  0, 4, &
      mytask, ntasks, ntasks_x, ntasks_y,   &
      ips, ipe, jps, jpe, kps, kpe    )
IF ( SIZE(grid%tke_2,1)*SIZE(grid%tke_2,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%tke_2, 4, 4, 0, 0, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
   CALL RSL_LITE_EXCH_PERIOD_Y ( local_communicator_periodic , mytask, ntasks, ntasks_x, ntasks_y )
IF ( SIZE(grid%tke_2,1)*SIZE(grid%tke_2,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%tke_2, 4, 4, 0, 1, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
END IF

  
  END SUBROUTINE PERIOD_BDY_EM_TKE_sub

SUBROUTINE PERIOD_BDY_EM_MOIST2_sub ( grid, &
  config_flags, &
  num_moist, &
  moist, &
  local_communicator_periodic, &
  mytask, ntasks, ntasks_x, ntasks_y, &
  ids, ide, jds, jde, kds, kde,       &
  ims, ime, jms, jme, kms, kme,       &
  ips, ipe, jps, jpe, kps, kpe )
  USE module_domain, ONLY:domain
  USE module_configure, ONLY:grid_config_rec_type,in_use_for_config
  USE module_state_description, ONLY:PARAM_FIRST_SCALAR
  USE module_driver_constants
  TYPE(domain) ,               INTENT(IN) :: grid
  TYPE(grid_config_rec_type) , INTENT(IN) :: config_flags
  INTEGER, INTENT(IN) :: num_moist
  real, INTENT(INOUT) :: moist ( grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33,num_moist)
  INTEGER ,                    INTENT(IN) :: local_communicator_periodic
  INTEGER ,                    INTENT(IN) :: mytask, ntasks, ntasks_x, ntasks_y
  INTEGER ,                    INTENT(IN) :: ids, ide, jds, jde, kds, kde
  INTEGER ,                    INTENT(IN) :: ims, ime, jms, jme, kms, kme
  INTEGER ,                    INTENT(IN) :: ips, ipe, jps, jpe, kps, kpe
  INTEGER :: itrace
  INTEGER :: rsl_sendw_p, rsl_sendbeg_p, rsl_recvw_p, rsl_recvbeg_p
  INTEGER :: rsl_sendw_m, rsl_sendbeg_m, rsl_recvw_m, rsl_recvbeg_m
  LOGICAL, EXTERNAL :: rsl_comm_iter
  INTEGER :: idim1, idim2, idim3, idim4, idim5, idim6, idim7
  






CALL wrf_debug(2,'calling inc/PERIOD_BDY_EM_MOIST2_inline.inc')
IF ( config_flags%periodic_x ) THEN
CALL RSL_LITE_INIT_PERIOD ( local_communicator_periodic, 4 , &
     0  &
   + num_moist   &
     , 0, 4, &
     0, 0, 4, &
     0, 0, 8, &
      0,  0, 4, &
      mytask, ntasks, ntasks_x, ntasks_y,   &
      ips, ipe, jps, jpe, kps, kpe    )
DO itrace = PARAM_FIRST_SCALAR, num_moist
 IF ( SIZE(moist,1)*SIZE(moist,3) .GT. 1 ) THEN
  CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
moist ( grid%sm31,grid%sm32,grid%sm33,itrace), 4, 4, 1, 0, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
 ENDIF
ENDDO
   CALL RSL_LITE_EXCH_PERIOD_X ( local_communicator_periodic , mytask, ntasks, ntasks_x, ntasks_y )
DO itrace = PARAM_FIRST_SCALAR, num_moist
 IF ( SIZE(moist,1)*SIZE(moist,3) .GT. 1 ) THEN
  CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
moist ( grid%sm31,grid%sm32,grid%sm33,itrace), 4, 4, 1, 1, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
 ENDIF
ENDDO
END IF
IF ( config_flags%periodic_y ) THEN
CALL RSL_LITE_INIT_PERIOD ( local_communicator_periodic, 4 , &
     0  &
   + num_moist   &
     , 0, 4, &
     0, 0, 4, &
     0, 0, 8, &
      0,  0, 4, &
      mytask, ntasks, ntasks_x, ntasks_y,   &
      ips, ipe, jps, jpe, kps, kpe    )
DO itrace = PARAM_FIRST_SCALAR, num_moist
 IF ( SIZE(moist,1)*SIZE(moist,3) .GT. 1 ) THEN
  CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
moist ( grid%sm31,grid%sm32,grid%sm33,itrace), 4, 4, 0, 0, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
 ENDIF
ENDDO
   CALL RSL_LITE_EXCH_PERIOD_Y ( local_communicator_periodic , mytask, ntasks, ntasks_x, ntasks_y )
DO itrace = PARAM_FIRST_SCALAR, num_moist
 IF ( SIZE(moist,1)*SIZE(moist,3) .GT. 1 ) THEN
  CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
moist ( grid%sm31,grid%sm32,grid%sm33,itrace), 4, 4, 0, 1, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
 ENDIF
ENDDO
END IF

  
  END SUBROUTINE PERIOD_BDY_EM_MOIST2_sub

SUBROUTINE PERIOD_BDY_EM_CHEM2_sub ( grid, &
  config_flags, &
  num_chem, &
  chem, &
  local_communicator_periodic, &
  mytask, ntasks, ntasks_x, ntasks_y, &
  ids, ide, jds, jde, kds, kde,       &
  ims, ime, jms, jme, kms, kme,       &
  ips, ipe, jps, jpe, kps, kpe )
  USE module_domain, ONLY:domain
  USE module_configure, ONLY:grid_config_rec_type,in_use_for_config
  USE module_state_description, ONLY:PARAM_FIRST_SCALAR
  USE module_driver_constants
  TYPE(domain) ,               INTENT(IN) :: grid
  TYPE(grid_config_rec_type) , INTENT(IN) :: config_flags
  INTEGER, INTENT(IN) :: num_chem
  real, INTENT(INOUT) :: chem ( grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33,num_chem)
  INTEGER ,                    INTENT(IN) :: local_communicator_periodic
  INTEGER ,                    INTENT(IN) :: mytask, ntasks, ntasks_x, ntasks_y
  INTEGER ,                    INTENT(IN) :: ids, ide, jds, jde, kds, kde
  INTEGER ,                    INTENT(IN) :: ims, ime, jms, jme, kms, kme
  INTEGER ,                    INTENT(IN) :: ips, ipe, jps, jpe, kps, kpe
  INTEGER :: itrace
  INTEGER :: rsl_sendw_p, rsl_sendbeg_p, rsl_recvw_p, rsl_recvbeg_p
  INTEGER :: rsl_sendw_m, rsl_sendbeg_m, rsl_recvw_m, rsl_recvbeg_m
  LOGICAL, EXTERNAL :: rsl_comm_iter
  INTEGER :: idim1, idim2, idim3, idim4, idim5, idim6, idim7
  






CALL wrf_debug(2,'calling inc/PERIOD_BDY_EM_CHEM2_inline.inc')
IF ( config_flags%periodic_x ) THEN
CALL RSL_LITE_INIT_PERIOD ( local_communicator_periodic, 4 , &
     0  &
   + num_chem   &
     , 0, 4, &
     0, 0, 4, &
     0, 0, 8, &
      0,  0, 4, &
      mytask, ntasks, ntasks_x, ntasks_y,   &
      ips, ipe, jps, jpe, kps, kpe    )
DO itrace = PARAM_FIRST_SCALAR, num_chem
 IF ( SIZE(chem,1)*SIZE(chem,3) .GT. 1 ) THEN
  CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
chem ( grid%sm31,grid%sm32,grid%sm33,itrace), 4, 4, 1, 0, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
 ENDIF
ENDDO
   CALL RSL_LITE_EXCH_PERIOD_X ( local_communicator_periodic , mytask, ntasks, ntasks_x, ntasks_y )
DO itrace = PARAM_FIRST_SCALAR, num_chem
 IF ( SIZE(chem,1)*SIZE(chem,3) .GT. 1 ) THEN
  CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
chem ( grid%sm31,grid%sm32,grid%sm33,itrace), 4, 4, 1, 1, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
 ENDIF
ENDDO
END IF
IF ( config_flags%periodic_y ) THEN
CALL RSL_LITE_INIT_PERIOD ( local_communicator_periodic, 4 , &
     0  &
   + num_chem   &
     , 0, 4, &
     0, 0, 4, &
     0, 0, 8, &
      0,  0, 4, &
      mytask, ntasks, ntasks_x, ntasks_y,   &
      ips, ipe, jps, jpe, kps, kpe    )
DO itrace = PARAM_FIRST_SCALAR, num_chem
 IF ( SIZE(chem,1)*SIZE(chem,3) .GT. 1 ) THEN
  CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
chem ( grid%sm31,grid%sm32,grid%sm33,itrace), 4, 4, 0, 0, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
 ENDIF
ENDDO
   CALL RSL_LITE_EXCH_PERIOD_Y ( local_communicator_periodic , mytask, ntasks, ntasks_x, ntasks_y )
DO itrace = PARAM_FIRST_SCALAR, num_chem
 IF ( SIZE(chem,1)*SIZE(chem,3) .GT. 1 ) THEN
  CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
chem ( grid%sm31,grid%sm32,grid%sm33,itrace), 4, 4, 0, 1, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
 ENDIF
ENDDO
END IF

  
  END SUBROUTINE PERIOD_BDY_EM_CHEM2_sub

SUBROUTINE PERIOD_BDY_EM_TRACER2_sub ( grid, &
  config_flags, &
  num_tracer, &
  tracer, &
  local_communicator_periodic, &
  mytask, ntasks, ntasks_x, ntasks_y, &
  ids, ide, jds, jde, kds, kde,       &
  ims, ime, jms, jme, kms, kme,       &
  ips, ipe, jps, jpe, kps, kpe )
  USE module_domain, ONLY:domain
  USE module_configure, ONLY:grid_config_rec_type,in_use_for_config
  USE module_state_description, ONLY:PARAM_FIRST_SCALAR
  USE module_driver_constants
  TYPE(domain) ,               INTENT(IN) :: grid
  TYPE(grid_config_rec_type) , INTENT(IN) :: config_flags
  INTEGER, INTENT(IN) :: num_tracer
  real, INTENT(INOUT) :: tracer ( grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33,num_tracer)
  INTEGER ,                    INTENT(IN) :: local_communicator_periodic
  INTEGER ,                    INTENT(IN) :: mytask, ntasks, ntasks_x, ntasks_y
  INTEGER ,                    INTENT(IN) :: ids, ide, jds, jde, kds, kde
  INTEGER ,                    INTENT(IN) :: ims, ime, jms, jme, kms, kme
  INTEGER ,                    INTENT(IN) :: ips, ipe, jps, jpe, kps, kpe
  INTEGER :: itrace
  INTEGER :: rsl_sendw_p, rsl_sendbeg_p, rsl_recvw_p, rsl_recvbeg_p
  INTEGER :: rsl_sendw_m, rsl_sendbeg_m, rsl_recvw_m, rsl_recvbeg_m
  LOGICAL, EXTERNAL :: rsl_comm_iter
  INTEGER :: idim1, idim2, idim3, idim4, idim5, idim6, idim7
  






CALL wrf_debug(2,'calling inc/PERIOD_BDY_EM_TRACER2_inline.inc')
IF ( config_flags%periodic_x ) THEN
CALL RSL_LITE_INIT_PERIOD ( local_communicator_periodic, 4 , &
     0  &
   + num_tracer   &
     , 0, 4, &
     0, 0, 4, &
     0, 0, 8, &
      0,  0, 4, &
      mytask, ntasks, ntasks_x, ntasks_y,   &
      ips, ipe, jps, jpe, kps, kpe    )
DO itrace = PARAM_FIRST_SCALAR, num_tracer
 IF ( SIZE(tracer,1)*SIZE(tracer,3) .GT. 1 ) THEN
  CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
tracer ( grid%sm31,grid%sm32,grid%sm33,itrace), 4, 4, 1, 0, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
 ENDIF
ENDDO
   CALL RSL_LITE_EXCH_PERIOD_X ( local_communicator_periodic , mytask, ntasks, ntasks_x, ntasks_y )
DO itrace = PARAM_FIRST_SCALAR, num_tracer
 IF ( SIZE(tracer,1)*SIZE(tracer,3) .GT. 1 ) THEN
  CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
tracer ( grid%sm31,grid%sm32,grid%sm33,itrace), 4, 4, 1, 1, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
 ENDIF
ENDDO
END IF
IF ( config_flags%periodic_y ) THEN
CALL RSL_LITE_INIT_PERIOD ( local_communicator_periodic, 4 , &
     0  &
   + num_tracer   &
     , 0, 4, &
     0, 0, 4, &
     0, 0, 8, &
      0,  0, 4, &
      mytask, ntasks, ntasks_x, ntasks_y,   &
      ips, ipe, jps, jpe, kps, kpe    )
DO itrace = PARAM_FIRST_SCALAR, num_tracer
 IF ( SIZE(tracer,1)*SIZE(tracer,3) .GT. 1 ) THEN
  CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
tracer ( grid%sm31,grid%sm32,grid%sm33,itrace), 4, 4, 0, 0, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
 ENDIF
ENDDO
   CALL RSL_LITE_EXCH_PERIOD_Y ( local_communicator_periodic , mytask, ntasks, ntasks_x, ntasks_y )
DO itrace = PARAM_FIRST_SCALAR, num_tracer
 IF ( SIZE(tracer,1)*SIZE(tracer,3) .GT. 1 ) THEN
  CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
tracer ( grid%sm31,grid%sm32,grid%sm33,itrace), 4, 4, 0, 1, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
 ENDIF
ENDDO
END IF

  
  END SUBROUTINE PERIOD_BDY_EM_TRACER2_sub

SUBROUTINE PERIOD_BDY_EM_SCALAR2_sub ( grid, &
  config_flags, &
  num_scalar, &
  scalar, &
  local_communicator_periodic, &
  mytask, ntasks, ntasks_x, ntasks_y, &
  ids, ide, jds, jde, kds, kde,       &
  ims, ime, jms, jme, kms, kme,       &
  ips, ipe, jps, jpe, kps, kpe )
  USE module_domain, ONLY:domain
  USE module_configure, ONLY:grid_config_rec_type,in_use_for_config
  USE module_state_description, ONLY:PARAM_FIRST_SCALAR
  USE module_driver_constants
  TYPE(domain) ,               INTENT(IN) :: grid
  TYPE(grid_config_rec_type) , INTENT(IN) :: config_flags
  INTEGER, INTENT(IN) :: num_scalar
  real, INTENT(INOUT) :: scalar ( grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33,num_scalar)
  INTEGER ,                    INTENT(IN) :: local_communicator_periodic
  INTEGER ,                    INTENT(IN) :: mytask, ntasks, ntasks_x, ntasks_y
  INTEGER ,                    INTENT(IN) :: ids, ide, jds, jde, kds, kde
  INTEGER ,                    INTENT(IN) :: ims, ime, jms, jme, kms, kme
  INTEGER ,                    INTENT(IN) :: ips, ipe, jps, jpe, kps, kpe
  INTEGER :: itrace
  INTEGER :: rsl_sendw_p, rsl_sendbeg_p, rsl_recvw_p, rsl_recvbeg_p
  INTEGER :: rsl_sendw_m, rsl_sendbeg_m, rsl_recvw_m, rsl_recvbeg_m
  LOGICAL, EXTERNAL :: rsl_comm_iter
  INTEGER :: idim1, idim2, idim3, idim4, idim5, idim6, idim7
  






CALL wrf_debug(2,'calling inc/PERIOD_BDY_EM_SCALAR2_inline.inc')
IF ( config_flags%periodic_x ) THEN
CALL RSL_LITE_INIT_PERIOD ( local_communicator_periodic, 4 , &
     0  &
   + num_scalar   &
     , 0, 4, &
     0, 0, 4, &
     0, 0, 8, &
      0,  0, 4, &
      mytask, ntasks, ntasks_x, ntasks_y,   &
      ips, ipe, jps, jpe, kps, kpe    )
DO itrace = PARAM_FIRST_SCALAR, num_scalar
 IF ( SIZE(scalar,1)*SIZE(scalar,3) .GT. 1 ) THEN
  CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
scalar ( grid%sm31,grid%sm32,grid%sm33,itrace), 4, 4, 1, 0, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
 ENDIF
ENDDO
   CALL RSL_LITE_EXCH_PERIOD_X ( local_communicator_periodic , mytask, ntasks, ntasks_x, ntasks_y )
DO itrace = PARAM_FIRST_SCALAR, num_scalar
 IF ( SIZE(scalar,1)*SIZE(scalar,3) .GT. 1 ) THEN
  CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
scalar ( grid%sm31,grid%sm32,grid%sm33,itrace), 4, 4, 1, 1, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
 ENDIF
ENDDO
END IF
IF ( config_flags%periodic_y ) THEN
CALL RSL_LITE_INIT_PERIOD ( local_communicator_periodic, 4 , &
     0  &
   + num_scalar   &
     , 0, 4, &
     0, 0, 4, &
     0, 0, 8, &
      0,  0, 4, &
      mytask, ntasks, ntasks_x, ntasks_y,   &
      ips, ipe, jps, jpe, kps, kpe    )
DO itrace = PARAM_FIRST_SCALAR, num_scalar
 IF ( SIZE(scalar,1)*SIZE(scalar,3) .GT. 1 ) THEN
  CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
scalar ( grid%sm31,grid%sm32,grid%sm33,itrace), 4, 4, 0, 0, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
 ENDIF
ENDDO
   CALL RSL_LITE_EXCH_PERIOD_Y ( local_communicator_periodic , mytask, ntasks, ntasks_x, ntasks_y )
DO itrace = PARAM_FIRST_SCALAR, num_scalar
 IF ( SIZE(scalar,1)*SIZE(scalar,3) .GT. 1 ) THEN
  CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
scalar ( grid%sm31,grid%sm32,grid%sm33,itrace), 4, 4, 0, 1, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
 ENDIF
ENDDO
END IF

  
  END SUBROUTINE PERIOD_BDY_EM_SCALAR2_sub

SUBROUTINE PERIOD_BDY_EM_MOIST_OLD_sub ( grid, &
  config_flags, &
  num_moist, &
  moist_old, &
  local_communicator_periodic, &
  mytask, ntasks, ntasks_x, ntasks_y, &
  ids, ide, jds, jde, kds, kde,       &
  ims, ime, jms, jme, kms, kme,       &
  ips, ipe, jps, jpe, kps, kpe )
  USE module_domain, ONLY:domain
  USE module_configure, ONLY:grid_config_rec_type,in_use_for_config
  USE module_state_description, ONLY:PARAM_FIRST_SCALAR
  USE module_driver_constants
  TYPE(domain) ,               INTENT(IN) :: grid
  TYPE(grid_config_rec_type) , INTENT(IN) :: config_flags
  INTEGER, INTENT(IN) :: num_moist
  real, INTENT(INOUT) :: moist_old ( grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33,num_moist)
  INTEGER ,                    INTENT(IN) :: local_communicator_periodic
  INTEGER ,                    INTENT(IN) :: mytask, ntasks, ntasks_x, ntasks_y
  INTEGER ,                    INTENT(IN) :: ids, ide, jds, jde, kds, kde
  INTEGER ,                    INTENT(IN) :: ims, ime, jms, jme, kms, kme
  INTEGER ,                    INTENT(IN) :: ips, ipe, jps, jpe, kps, kpe
  INTEGER :: itrace
  INTEGER :: rsl_sendw_p, rsl_sendbeg_p, rsl_recvw_p, rsl_recvbeg_p
  INTEGER :: rsl_sendw_m, rsl_sendbeg_m, rsl_recvw_m, rsl_recvbeg_m
  LOGICAL, EXTERNAL :: rsl_comm_iter
  INTEGER :: idim1, idim2, idim3, idim4, idim5, idim6, idim7
  






CALL wrf_debug(2,'calling inc/PERIOD_BDY_EM_MOIST_OLD_inline.inc')
IF ( config_flags%periodic_x ) THEN
CALL RSL_LITE_INIT_PERIOD ( local_communicator_periodic, 4 , &
     0  &
   + num_moist   &
     , 0, 4, &
     0, 0, 4, &
     0, 0, 8, &
      0,  0, 4, &
      mytask, ntasks, ntasks_x, ntasks_y,   &
      ips, ipe, jps, jpe, kps, kpe    )
DO itrace = PARAM_FIRST_SCALAR, num_moist
 IF ( SIZE(moist_old,1)*SIZE(moist_old,3) .GT. 1 ) THEN
  CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
moist_old ( grid%sm31,grid%sm32,grid%sm33,itrace), 4, 4, 1, 0, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
 ENDIF
ENDDO
   CALL RSL_LITE_EXCH_PERIOD_X ( local_communicator_periodic , mytask, ntasks, ntasks_x, ntasks_y )
DO itrace = PARAM_FIRST_SCALAR, num_moist
 IF ( SIZE(moist_old,1)*SIZE(moist_old,3) .GT. 1 ) THEN
  CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
moist_old ( grid%sm31,grid%sm32,grid%sm33,itrace), 4, 4, 1, 1, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
 ENDIF
ENDDO
END IF
IF ( config_flags%periodic_y ) THEN
CALL RSL_LITE_INIT_PERIOD ( local_communicator_periodic, 4 , &
     0  &
   + num_moist   &
     , 0, 4, &
     0, 0, 4, &
     0, 0, 8, &
      0,  0, 4, &
      mytask, ntasks, ntasks_x, ntasks_y,   &
      ips, ipe, jps, jpe, kps, kpe    )
DO itrace = PARAM_FIRST_SCALAR, num_moist
 IF ( SIZE(moist_old,1)*SIZE(moist_old,3) .GT. 1 ) THEN
  CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
moist_old ( grid%sm31,grid%sm32,grid%sm33,itrace), 4, 4, 0, 0, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
 ENDIF
ENDDO
   CALL RSL_LITE_EXCH_PERIOD_Y ( local_communicator_periodic , mytask, ntasks, ntasks_x, ntasks_y )
DO itrace = PARAM_FIRST_SCALAR, num_moist
 IF ( SIZE(moist_old,1)*SIZE(moist_old,3) .GT. 1 ) THEN
  CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
moist_old ( grid%sm31,grid%sm32,grid%sm33,itrace), 4, 4, 0, 1, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
 ENDIF
ENDDO
END IF

  
  END SUBROUTINE PERIOD_BDY_EM_MOIST_OLD_sub

SUBROUTINE PERIOD_BDY_EM_CHEM_OLD_sub ( grid, &
  config_flags, &
  num_chem, &
  chem_old, &
  local_communicator_periodic, &
  mytask, ntasks, ntasks_x, ntasks_y, &
  ids, ide, jds, jde, kds, kde,       &
  ims, ime, jms, jme, kms, kme,       &
  ips, ipe, jps, jpe, kps, kpe )
  USE module_domain, ONLY:domain
  USE module_configure, ONLY:grid_config_rec_type,in_use_for_config
  USE module_state_description, ONLY:PARAM_FIRST_SCALAR
  USE module_driver_constants
  TYPE(domain) ,               INTENT(IN) :: grid
  TYPE(grid_config_rec_type) , INTENT(IN) :: config_flags
  INTEGER, INTENT(IN) :: num_chem
  real, INTENT(INOUT) :: chem_old ( grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33,num_chem)
  INTEGER ,                    INTENT(IN) :: local_communicator_periodic
  INTEGER ,                    INTENT(IN) :: mytask, ntasks, ntasks_x, ntasks_y
  INTEGER ,                    INTENT(IN) :: ids, ide, jds, jde, kds, kde
  INTEGER ,                    INTENT(IN) :: ims, ime, jms, jme, kms, kme
  INTEGER ,                    INTENT(IN) :: ips, ipe, jps, jpe, kps, kpe
  INTEGER :: itrace
  INTEGER :: rsl_sendw_p, rsl_sendbeg_p, rsl_recvw_p, rsl_recvbeg_p
  INTEGER :: rsl_sendw_m, rsl_sendbeg_m, rsl_recvw_m, rsl_recvbeg_m
  LOGICAL, EXTERNAL :: rsl_comm_iter
  INTEGER :: idim1, idim2, idim3, idim4, idim5, idim6, idim7
  






CALL wrf_debug(2,'calling inc/PERIOD_BDY_EM_CHEM_OLD_inline.inc')
IF ( config_flags%periodic_x ) THEN
CALL RSL_LITE_INIT_PERIOD ( local_communicator_periodic, 4 , &
     0  &
   + num_chem   &
     , 0, 4, &
     0, 0, 4, &
     0, 0, 8, &
      0,  0, 4, &
      mytask, ntasks, ntasks_x, ntasks_y,   &
      ips, ipe, jps, jpe, kps, kpe    )
DO itrace = PARAM_FIRST_SCALAR, num_chem
 IF ( SIZE(chem_old,1)*SIZE(chem_old,3) .GT. 1 ) THEN
  CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
chem_old ( grid%sm31,grid%sm32,grid%sm33,itrace), 4, 4, 1, 0, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
 ENDIF
ENDDO
   CALL RSL_LITE_EXCH_PERIOD_X ( local_communicator_periodic , mytask, ntasks, ntasks_x, ntasks_y )
DO itrace = PARAM_FIRST_SCALAR, num_chem
 IF ( SIZE(chem_old,1)*SIZE(chem_old,3) .GT. 1 ) THEN
  CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
chem_old ( grid%sm31,grid%sm32,grid%sm33,itrace), 4, 4, 1, 1, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
 ENDIF
ENDDO
END IF
IF ( config_flags%periodic_y ) THEN
CALL RSL_LITE_INIT_PERIOD ( local_communicator_periodic, 4 , &
     0  &
   + num_chem   &
     , 0, 4, &
     0, 0, 4, &
     0, 0, 8, &
      0,  0, 4, &
      mytask, ntasks, ntasks_x, ntasks_y,   &
      ips, ipe, jps, jpe, kps, kpe    )
DO itrace = PARAM_FIRST_SCALAR, num_chem
 IF ( SIZE(chem_old,1)*SIZE(chem_old,3) .GT. 1 ) THEN
  CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
chem_old ( grid%sm31,grid%sm32,grid%sm33,itrace), 4, 4, 0, 0, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
 ENDIF
ENDDO
   CALL RSL_LITE_EXCH_PERIOD_Y ( local_communicator_periodic , mytask, ntasks, ntasks_x, ntasks_y )
DO itrace = PARAM_FIRST_SCALAR, num_chem
 IF ( SIZE(chem_old,1)*SIZE(chem_old,3) .GT. 1 ) THEN
  CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
chem_old ( grid%sm31,grid%sm32,grid%sm33,itrace), 4, 4, 0, 1, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
 ENDIF
ENDDO
END IF

  
  END SUBROUTINE PERIOD_BDY_EM_CHEM_OLD_sub

SUBROUTINE PERIOD_BDY_EM_TRACER_OLD_sub ( grid, &
  config_flags, &
  num_tracer, &
  tracer_old, &
  local_communicator_periodic, &
  mytask, ntasks, ntasks_x, ntasks_y, &
  ids, ide, jds, jde, kds, kde,       &
  ims, ime, jms, jme, kms, kme,       &
  ips, ipe, jps, jpe, kps, kpe )
  USE module_domain, ONLY:domain
  USE module_configure, ONLY:grid_config_rec_type,in_use_for_config
  USE module_state_description, ONLY:PARAM_FIRST_SCALAR
  USE module_driver_constants
  TYPE(domain) ,               INTENT(IN) :: grid
  TYPE(grid_config_rec_type) , INTENT(IN) :: config_flags
  INTEGER, INTENT(IN) :: num_tracer
  real, INTENT(INOUT) :: tracer_old ( grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33,num_tracer)
  INTEGER ,                    INTENT(IN) :: local_communicator_periodic
  INTEGER ,                    INTENT(IN) :: mytask, ntasks, ntasks_x, ntasks_y
  INTEGER ,                    INTENT(IN) :: ids, ide, jds, jde, kds, kde
  INTEGER ,                    INTENT(IN) :: ims, ime, jms, jme, kms, kme
  INTEGER ,                    INTENT(IN) :: ips, ipe, jps, jpe, kps, kpe
  INTEGER :: itrace
  INTEGER :: rsl_sendw_p, rsl_sendbeg_p, rsl_recvw_p, rsl_recvbeg_p
  INTEGER :: rsl_sendw_m, rsl_sendbeg_m, rsl_recvw_m, rsl_recvbeg_m
  LOGICAL, EXTERNAL :: rsl_comm_iter
  INTEGER :: idim1, idim2, idim3, idim4, idim5, idim6, idim7
  






CALL wrf_debug(2,'calling inc/PERIOD_BDY_EM_TRACER_OLD_inline.inc')
IF ( config_flags%periodic_x ) THEN
CALL RSL_LITE_INIT_PERIOD ( local_communicator_periodic, 4 , &
     0  &
   + num_tracer   &
     , 0, 4, &
     0, 0, 4, &
     0, 0, 8, &
      0,  0, 4, &
      mytask, ntasks, ntasks_x, ntasks_y,   &
      ips, ipe, jps, jpe, kps, kpe    )
DO itrace = PARAM_FIRST_SCALAR, num_tracer
 IF ( SIZE(tracer_old,1)*SIZE(tracer_old,3) .GT. 1 ) THEN
  CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
tracer_old ( grid%sm31,grid%sm32,grid%sm33,itrace), 4, 4, 1, 0, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
 ENDIF
ENDDO
   CALL RSL_LITE_EXCH_PERIOD_X ( local_communicator_periodic , mytask, ntasks, ntasks_x, ntasks_y )
DO itrace = PARAM_FIRST_SCALAR, num_tracer
 IF ( SIZE(tracer_old,1)*SIZE(tracer_old,3) .GT. 1 ) THEN
  CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
tracer_old ( grid%sm31,grid%sm32,grid%sm33,itrace), 4, 4, 1, 1, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
 ENDIF
ENDDO
END IF
IF ( config_flags%periodic_y ) THEN
CALL RSL_LITE_INIT_PERIOD ( local_communicator_periodic, 4 , &
     0  &
   + num_tracer   &
     , 0, 4, &
     0, 0, 4, &
     0, 0, 8, &
      0,  0, 4, &
      mytask, ntasks, ntasks_x, ntasks_y,   &
      ips, ipe, jps, jpe, kps, kpe    )
DO itrace = PARAM_FIRST_SCALAR, num_tracer
 IF ( SIZE(tracer_old,1)*SIZE(tracer_old,3) .GT. 1 ) THEN
  CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
tracer_old ( grid%sm31,grid%sm32,grid%sm33,itrace), 4, 4, 0, 0, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
 ENDIF
ENDDO
   CALL RSL_LITE_EXCH_PERIOD_Y ( local_communicator_periodic , mytask, ntasks, ntasks_x, ntasks_y )
DO itrace = PARAM_FIRST_SCALAR, num_tracer
 IF ( SIZE(tracer_old,1)*SIZE(tracer_old,3) .GT. 1 ) THEN
  CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
tracer_old ( grid%sm31,grid%sm32,grid%sm33,itrace), 4, 4, 0, 1, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
 ENDIF
ENDDO
END IF

  
  END SUBROUTINE PERIOD_BDY_EM_TRACER_OLD_sub

SUBROUTINE PERIOD_BDY_EM_SCALAR_OLD_sub ( grid, &
  config_flags, &
  num_scalar, &
  scalar_old, &
  local_communicator_periodic, &
  mytask, ntasks, ntasks_x, ntasks_y, &
  ids, ide, jds, jde, kds, kde,       &
  ims, ime, jms, jme, kms, kme,       &
  ips, ipe, jps, jpe, kps, kpe )
  USE module_domain, ONLY:domain
  USE module_configure, ONLY:grid_config_rec_type,in_use_for_config
  USE module_state_description, ONLY:PARAM_FIRST_SCALAR
  USE module_driver_constants
  TYPE(domain) ,               INTENT(IN) :: grid
  TYPE(grid_config_rec_type) , INTENT(IN) :: config_flags
  INTEGER, INTENT(IN) :: num_scalar
  real, INTENT(INOUT) :: scalar_old ( grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33,num_scalar)
  INTEGER ,                    INTENT(IN) :: local_communicator_periodic
  INTEGER ,                    INTENT(IN) :: mytask, ntasks, ntasks_x, ntasks_y
  INTEGER ,                    INTENT(IN) :: ids, ide, jds, jde, kds, kde
  INTEGER ,                    INTENT(IN) :: ims, ime, jms, jme, kms, kme
  INTEGER ,                    INTENT(IN) :: ips, ipe, jps, jpe, kps, kpe
  INTEGER :: itrace
  INTEGER :: rsl_sendw_p, rsl_sendbeg_p, rsl_recvw_p, rsl_recvbeg_p
  INTEGER :: rsl_sendw_m, rsl_sendbeg_m, rsl_recvw_m, rsl_recvbeg_m
  LOGICAL, EXTERNAL :: rsl_comm_iter
  INTEGER :: idim1, idim2, idim3, idim4, idim5, idim6, idim7
  






CALL wrf_debug(2,'calling inc/PERIOD_BDY_EM_SCALAR_OLD_inline.inc')
IF ( config_flags%periodic_x ) THEN
CALL RSL_LITE_INIT_PERIOD ( local_communicator_periodic, 4 , &
     0  &
   + num_scalar   &
     , 0, 4, &
     0, 0, 4, &
     0, 0, 8, &
      0,  0, 4, &
      mytask, ntasks, ntasks_x, ntasks_y,   &
      ips, ipe, jps, jpe, kps, kpe    )
DO itrace = PARAM_FIRST_SCALAR, num_scalar
 IF ( SIZE(scalar_old,1)*SIZE(scalar_old,3) .GT. 1 ) THEN
  CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
scalar_old ( grid%sm31,grid%sm32,grid%sm33,itrace), 4, 4, 1, 0, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
 ENDIF
ENDDO
   CALL RSL_LITE_EXCH_PERIOD_X ( local_communicator_periodic , mytask, ntasks, ntasks_x, ntasks_y )
DO itrace = PARAM_FIRST_SCALAR, num_scalar
 IF ( SIZE(scalar_old,1)*SIZE(scalar_old,3) .GT. 1 ) THEN
  CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
scalar_old ( grid%sm31,grid%sm32,grid%sm33,itrace), 4, 4, 1, 1, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
 ENDIF
ENDDO
END IF
IF ( config_flags%periodic_y ) THEN
CALL RSL_LITE_INIT_PERIOD ( local_communicator_periodic, 4 , &
     0  &
   + num_scalar   &
     , 0, 4, &
     0, 0, 4, &
     0, 0, 8, &
      0,  0, 4, &
      mytask, ntasks, ntasks_x, ntasks_y,   &
      ips, ipe, jps, jpe, kps, kpe    )
DO itrace = PARAM_FIRST_SCALAR, num_scalar
 IF ( SIZE(scalar_old,1)*SIZE(scalar_old,3) .GT. 1 ) THEN
  CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
scalar_old ( grid%sm31,grid%sm32,grid%sm33,itrace), 4, 4, 0, 0, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
 ENDIF
ENDDO
   CALL RSL_LITE_EXCH_PERIOD_Y ( local_communicator_periodic , mytask, ntasks, ntasks_x, ntasks_y )
DO itrace = PARAM_FIRST_SCALAR, num_scalar
 IF ( SIZE(scalar_old,1)*SIZE(scalar_old,3) .GT. 1 ) THEN
  CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
scalar_old ( grid%sm31,grid%sm32,grid%sm33,itrace), 4, 4, 0, 1, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
 ENDIF
ENDDO
END IF

  
  END SUBROUTINE PERIOD_BDY_EM_SCALAR_OLD_sub

SUBROUTINE PERIOD_BDY_EM_TKE_OLD_sub ( grid, &
  config_flags, &
  local_communicator_periodic, &
  mytask, ntasks, ntasks_x, ntasks_y, &
  ids, ide, jds, jde, kds, kde,       &
  ims, ime, jms, jme, kms, kme,       &
  ips, ipe, jps, jpe, kps, kpe )
  USE module_domain, ONLY:domain
  USE module_configure, ONLY:grid_config_rec_type,in_use_for_config
  USE module_state_description, ONLY:PARAM_FIRST_SCALAR
  USE module_driver_constants
  TYPE(domain) ,               INTENT(IN) :: grid
  TYPE(grid_config_rec_type) , INTENT(IN) :: config_flags
  INTEGER ,                    INTENT(IN) :: local_communicator_periodic
  INTEGER ,                    INTENT(IN) :: mytask, ntasks, ntasks_x, ntasks_y
  INTEGER ,                    INTENT(IN) :: ids, ide, jds, jde, kds, kde
  INTEGER ,                    INTENT(IN) :: ims, ime, jms, jme, kms, kme
  INTEGER ,                    INTENT(IN) :: ips, ipe, jps, jpe, kps, kpe
  INTEGER :: itrace
  INTEGER :: rsl_sendw_p, rsl_sendbeg_p, rsl_recvw_p, rsl_recvbeg_p
  INTEGER :: rsl_sendw_m, rsl_sendbeg_m, rsl_recvw_m, rsl_recvbeg_m
  LOGICAL, EXTERNAL :: rsl_comm_iter
  INTEGER :: idim1, idim2, idim3, idim4, idim5, idim6, idim7
  






CALL wrf_debug(2,'calling inc/PERIOD_BDY_EM_TKE_OLD_inline.inc')
IF ( config_flags%periodic_x ) THEN
CALL RSL_LITE_INIT_PERIOD ( local_communicator_periodic, 4 , &
     1, 0, 4, &
     0, 0, 4, &
     0, 0, 8, &
      0,  0, 4, &
      mytask, ntasks, ntasks_x, ntasks_y,   &
      ips, ipe, jps, jpe, kps, kpe    )
IF ( SIZE(grid%tke_1,1)*SIZE(grid%tke_1,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%tke_1, 4, 4, 1, 0, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
   CALL RSL_LITE_EXCH_PERIOD_X ( local_communicator_periodic , mytask, ntasks, ntasks_x, ntasks_y )
IF ( SIZE(grid%tke_1,1)*SIZE(grid%tke_1,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%tke_1, 4, 4, 1, 1, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
END IF
IF ( config_flags%periodic_y ) THEN
CALL RSL_LITE_INIT_PERIOD ( local_communicator_periodic, 4 , &
     1, 0, 4, &
     0, 0, 4, &
     0, 0, 8, &
      0,  0, 4, &
      mytask, ntasks, ntasks_x, ntasks_y,   &
      ips, ipe, jps, jpe, kps, kpe    )
IF ( SIZE(grid%tke_1,1)*SIZE(grid%tke_1,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%tke_1, 4, 4, 0, 0, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
   CALL RSL_LITE_EXCH_PERIOD_Y ( local_communicator_periodic , mytask, ntasks, ntasks_x, ntasks_y )
IF ( SIZE(grid%tke_1,1)*SIZE(grid%tke_1,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%tke_1, 4, 4, 0, 1, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
END IF

  
  END SUBROUTINE PERIOD_BDY_EM_TKE_OLD_sub

SUBROUTINE PERIOD_BDY_EM_E_sub ( grid, &
  config_flags, &
  local_communicator_periodic, &
  mytask, ntasks, ntasks_x, ntasks_y, &
  ids, ide, jds, jde, kds, kde,       &
  ims, ime, jms, jme, kms, kme,       &
  ips, ipe, jps, jpe, kps, kpe )
  USE module_domain, ONLY:domain
  USE module_configure, ONLY:grid_config_rec_type,in_use_for_config
  USE module_state_description, ONLY:PARAM_FIRST_SCALAR
  USE module_driver_constants
  TYPE(domain) ,               INTENT(IN) :: grid
  TYPE(grid_config_rec_type) , INTENT(IN) :: config_flags
  INTEGER ,                    INTENT(IN) :: local_communicator_periodic
  INTEGER ,                    INTENT(IN) :: mytask, ntasks, ntasks_x, ntasks_y
  INTEGER ,                    INTENT(IN) :: ids, ide, jds, jde, kds, kde
  INTEGER ,                    INTENT(IN) :: ims, ime, jms, jme, kms, kme
  INTEGER ,                    INTENT(IN) :: ips, ipe, jps, jpe, kps, kpe
  INTEGER :: itrace
  INTEGER :: rsl_sendw_p, rsl_sendbeg_p, rsl_recvw_p, rsl_recvbeg_p
  INTEGER :: rsl_sendw_m, rsl_sendbeg_m, rsl_recvw_m, rsl_recvbeg_m
  LOGICAL, EXTERNAL :: rsl_comm_iter
  INTEGER :: idim1, idim2, idim3, idim4, idim5, idim6, idim7
  






CALL wrf_debug(2,'calling inc/PERIOD_BDY_EM_E_inline.inc')
IF ( config_flags%periodic_x ) THEN
CALL RSL_LITE_INIT_PERIOD ( local_communicator_periodic, 2 , &
     2, 1, 4, &
     0, 0, 4, &
     0, 0, 8, &
      0,  0, 4, &
      mytask, ntasks, ntasks_x, ntasks_y,   &
      ips, ipe, jps, jpe, kps, kpe    )
IF ( SIZE(grid%u_2,1)*SIZE(grid%u_2,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%u_2, 2, 4, 1, 0, DATA_ORDER_XZY, 1, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%v_2,1)*SIZE(grid%v_2,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%v_2, 2, 4, 1, 0, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%ht,1)*SIZE(grid%ht,2) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%ht, 2, 4, 1, 0, DATA_ORDER_XY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
ENDIF
   CALL RSL_LITE_EXCH_PERIOD_X ( local_communicator_periodic , mytask, ntasks, ntasks_x, ntasks_y )
IF ( SIZE(grid%u_2,1)*SIZE(grid%u_2,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%u_2, 2, 4, 1, 1, DATA_ORDER_XZY, 1, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%v_2,1)*SIZE(grid%v_2,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%v_2, 2, 4, 1, 1, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%ht,1)*SIZE(grid%ht,2) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%ht, 2, 4, 1, 1, DATA_ORDER_XY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
ENDIF
END IF
IF ( config_flags%periodic_y ) THEN
CALL RSL_LITE_INIT_PERIOD ( local_communicator_periodic, 2 , &
     2, 1, 4, &
     0, 0, 4, &
     0, 0, 8, &
      0,  0, 4, &
      mytask, ntasks, ntasks_x, ntasks_y,   &
      ips, ipe, jps, jpe, kps, kpe    )
IF ( SIZE(grid%u_2,1)*SIZE(grid%u_2,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%u_2, 2, 4, 0, 0, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%v_2,1)*SIZE(grid%v_2,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%v_2, 2, 4, 0, 0, DATA_ORDER_XZY, 1, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%ht,1)*SIZE(grid%ht,2) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%ht, 2, 4, 0, 0, DATA_ORDER_XY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
ENDIF
   CALL RSL_LITE_EXCH_PERIOD_Y ( local_communicator_periodic , mytask, ntasks, ntasks_x, ntasks_y )
IF ( SIZE(grid%u_2,1)*SIZE(grid%u_2,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%u_2, 2, 4, 0, 1, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%v_2,1)*SIZE(grid%v_2,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%v_2, 2, 4, 0, 1, DATA_ORDER_XZY, 1, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%ht,1)*SIZE(grid%ht,2) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%ht, 2, 4, 0, 1, DATA_ORDER_XY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
ENDIF
END IF

  
  END SUBROUTINE PERIOD_BDY_EM_E_sub

SUBROUTINE PERIOD_EM_HYDRO_UV_sub ( grid, &
  config_flags, &
  local_communicator_periodic, &
  mytask, ntasks, ntasks_x, ntasks_y, &
  ids, ide, jds, jde, kds, kde,       &
  ims, ime, jms, jme, kms, kme,       &
  ips, ipe, jps, jpe, kps, kpe )
  USE module_domain, ONLY:domain
  USE module_configure, ONLY:grid_config_rec_type,in_use_for_config
  USE module_state_description, ONLY:PARAM_FIRST_SCALAR
  USE module_driver_constants
  TYPE(domain) ,               INTENT(IN) :: grid
  TYPE(grid_config_rec_type) , INTENT(IN) :: config_flags
  INTEGER ,                    INTENT(IN) :: local_communicator_periodic
  INTEGER ,                    INTENT(IN) :: mytask, ntasks, ntasks_x, ntasks_y
  INTEGER ,                    INTENT(IN) :: ids, ide, jds, jde, kds, kde
  INTEGER ,                    INTENT(IN) :: ims, ime, jms, jme, kms, kme
  INTEGER ,                    INTENT(IN) :: ips, ipe, jps, jpe, kps, kpe
  INTEGER :: itrace
  INTEGER :: rsl_sendw_p, rsl_sendbeg_p, rsl_recvw_p, rsl_recvbeg_p
  INTEGER :: rsl_sendw_m, rsl_sendbeg_m, rsl_recvw_m, rsl_recvbeg_m
  LOGICAL, EXTERNAL :: rsl_comm_iter
  INTEGER :: idim1, idim2, idim3, idim4, idim5, idim6, idim7
  






CALL wrf_debug(2,'calling inc/PERIOD_EM_HYDRO_UV_inline.inc')
IF ( config_flags%periodic_x ) THEN
CALL RSL_LITE_INIT_PERIOD ( local_communicator_periodic, 1 , &
     2, 0, 4, &
     0, 0, 4, &
     0, 0, 8, &
      0,  0, 4, &
      mytask, ntasks, ntasks_x, ntasks_y,   &
      ips, ipe, jps, jpe, kps, kpe    )
IF ( SIZE(grid%u_2,1)*SIZE(grid%u_2,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%u_2, 1, 4, 1, 0, DATA_ORDER_XZY, 1, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%v_2,1)*SIZE(grid%v_2,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%v_2, 1, 4, 1, 0, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
   CALL RSL_LITE_EXCH_PERIOD_X ( local_communicator_periodic , mytask, ntasks, ntasks_x, ntasks_y )
IF ( SIZE(grid%u_2,1)*SIZE(grid%u_2,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%u_2, 1, 4, 1, 1, DATA_ORDER_XZY, 1, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%v_2,1)*SIZE(grid%v_2,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%v_2, 1, 4, 1, 1, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
END IF
IF ( config_flags%periodic_y ) THEN
CALL RSL_LITE_INIT_PERIOD ( local_communicator_periodic, 1 , &
     2, 0, 4, &
     0, 0, 4, &
     0, 0, 8, &
      0,  0, 4, &
      mytask, ntasks, ntasks_x, ntasks_y,   &
      ips, ipe, jps, jpe, kps, kpe    )
IF ( SIZE(grid%u_2,1)*SIZE(grid%u_2,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%u_2, 1, 4, 0, 0, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%v_2,1)*SIZE(grid%v_2,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%v_2, 1, 4, 0, 0, DATA_ORDER_XZY, 1, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
   CALL RSL_LITE_EXCH_PERIOD_Y ( local_communicator_periodic , mytask, ntasks, ntasks_x, ntasks_y )
IF ( SIZE(grid%u_2,1)*SIZE(grid%u_2,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%u_2, 1, 4, 0, 1, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%v_2,1)*SIZE(grid%v_2,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%v_2, 1, 4, 0, 1, DATA_ORDER_XZY, 1, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
END IF

  
  END SUBROUTINE PERIOD_EM_HYDRO_UV_sub

SUBROUTINE PERIOD_BDY_EM_A_sub ( grid, &
  config_flags, &
  local_communicator_periodic, &
  mytask, ntasks, ntasks_x, ntasks_y, &
  ids, ide, jds, jde, kds, kde,       &
  ims, ime, jms, jme, kms, kme,       &
  ips, ipe, jps, jpe, kps, kpe )
  USE module_domain, ONLY:domain
  USE module_configure, ONLY:grid_config_rec_type,in_use_for_config
  USE module_state_description, ONLY:PARAM_FIRST_SCALAR
  USE module_driver_constants
  TYPE(domain) ,               INTENT(IN) :: grid
  TYPE(grid_config_rec_type) , INTENT(IN) :: config_flags
  INTEGER ,                    INTENT(IN) :: local_communicator_periodic
  INTEGER ,                    INTENT(IN) :: mytask, ntasks, ntasks_x, ntasks_y
  INTEGER ,                    INTENT(IN) :: ids, ide, jds, jde, kds, kde
  INTEGER ,                    INTENT(IN) :: ims, ime, jms, jme, kms, kme
  INTEGER ,                    INTENT(IN) :: ips, ipe, jps, jpe, kps, kpe
  INTEGER :: itrace
  INTEGER :: rsl_sendw_p, rsl_sendbeg_p, rsl_recvw_p, rsl_recvbeg_p
  INTEGER :: rsl_sendw_m, rsl_sendbeg_m, rsl_recvw_m, rsl_recvbeg_m
  LOGICAL, EXTERNAL :: rsl_comm_iter
  INTEGER :: idim1, idim2, idim3, idim4, idim5, idim6, idim7
  






CALL wrf_debug(2,'calling inc/PERIOD_BDY_EM_A_inline.inc')
IF ( config_flags%periodic_x ) THEN
CALL RSL_LITE_INIT_PERIOD ( local_communicator_periodic, 2 , &
     9, 3, 4, &
     0, 0, 4, &
     0, 0, 8, &
      0,  0, 4, &
      mytask, ntasks, ntasks_x, ntasks_y,   &
      ips, ipe, jps, jpe, kps, kpe    )
IF ( SIZE(grid%ru,1)*SIZE(grid%ru,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%ru, 2, 4, 1, 0, DATA_ORDER_XZY, 1, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%rv,1)*SIZE(grid%rv,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%rv, 2, 4, 1, 0, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%rw,1)*SIZE(grid%rw,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%rw, 2, 4, 1, 0, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%ww,1)*SIZE(grid%ww,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%ww, 2, 4, 1, 0, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%php,1)*SIZE(grid%php,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%php, 2, 4, 1, 0, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%alt,1)*SIZE(grid%alt,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%alt, 2, 4, 1, 0, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%p,1)*SIZE(grid%p,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%p, 2, 4, 1, 0, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%muu,1)*SIZE(grid%muu,2) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%muu, 2, 4, 1, 0, DATA_ORDER_XY, 1, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
ENDIF
IF ( SIZE(grid%muv,1)*SIZE(grid%muv,2) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%muv, 2, 4, 1, 0, DATA_ORDER_XY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
ENDIF
IF ( SIZE(grid%mut,1)*SIZE(grid%mut,2) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%mut, 2, 4, 1, 0, DATA_ORDER_XY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
ENDIF
IF ( SIZE(grid%ph_2,1)*SIZE(grid%ph_2,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%ph_2, 2, 4, 1, 0, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%al,1)*SIZE(grid%al,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%al, 2, 4, 1, 0, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
   CALL RSL_LITE_EXCH_PERIOD_X ( local_communicator_periodic , mytask, ntasks, ntasks_x, ntasks_y )
IF ( SIZE(grid%ru,1)*SIZE(grid%ru,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%ru, 2, 4, 1, 1, DATA_ORDER_XZY, 1, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%rv,1)*SIZE(grid%rv,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%rv, 2, 4, 1, 1, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%rw,1)*SIZE(grid%rw,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%rw, 2, 4, 1, 1, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%ww,1)*SIZE(grid%ww,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%ww, 2, 4, 1, 1, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%php,1)*SIZE(grid%php,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%php, 2, 4, 1, 1, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%alt,1)*SIZE(grid%alt,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%alt, 2, 4, 1, 1, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%p,1)*SIZE(grid%p,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%p, 2, 4, 1, 1, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%muu,1)*SIZE(grid%muu,2) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%muu, 2, 4, 1, 1, DATA_ORDER_XY, 1, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
ENDIF
IF ( SIZE(grid%muv,1)*SIZE(grid%muv,2) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%muv, 2, 4, 1, 1, DATA_ORDER_XY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
ENDIF
IF ( SIZE(grid%mut,1)*SIZE(grid%mut,2) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%mut, 2, 4, 1, 1, DATA_ORDER_XY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
ENDIF
IF ( SIZE(grid%ph_2,1)*SIZE(grid%ph_2,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%ph_2, 2, 4, 1, 1, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%al,1)*SIZE(grid%al,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%al, 2, 4, 1, 1, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
END IF
IF ( config_flags%periodic_y ) THEN
CALL RSL_LITE_INIT_PERIOD ( local_communicator_periodic, 2 , &
     9, 3, 4, &
     0, 0, 4, &
     0, 0, 8, &
      0,  0, 4, &
      mytask, ntasks, ntasks_x, ntasks_y,   &
      ips, ipe, jps, jpe, kps, kpe    )
IF ( SIZE(grid%ru,1)*SIZE(grid%ru,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%ru, 2, 4, 0, 0, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%rv,1)*SIZE(grid%rv,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%rv, 2, 4, 0, 0, DATA_ORDER_XZY, 1, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%rw,1)*SIZE(grid%rw,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%rw, 2, 4, 0, 0, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%ww,1)*SIZE(grid%ww,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%ww, 2, 4, 0, 0, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%php,1)*SIZE(grid%php,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%php, 2, 4, 0, 0, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%alt,1)*SIZE(grid%alt,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%alt, 2, 4, 0, 0, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%p,1)*SIZE(grid%p,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%p, 2, 4, 0, 0, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%muu,1)*SIZE(grid%muu,2) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%muu, 2, 4, 0, 0, DATA_ORDER_XY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
ENDIF
IF ( SIZE(grid%muv,1)*SIZE(grid%muv,2) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%muv, 2, 4, 0, 0, DATA_ORDER_XY, 1, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
ENDIF
IF ( SIZE(grid%mut,1)*SIZE(grid%mut,2) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%mut, 2, 4, 0, 0, DATA_ORDER_XY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
ENDIF
IF ( SIZE(grid%ph_2,1)*SIZE(grid%ph_2,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%ph_2, 2, 4, 0, 0, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%al,1)*SIZE(grid%al,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%al, 2, 4, 0, 0, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
   CALL RSL_LITE_EXCH_PERIOD_Y ( local_communicator_periodic , mytask, ntasks, ntasks_x, ntasks_y )
IF ( SIZE(grid%ru,1)*SIZE(grid%ru,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%ru, 2, 4, 0, 1, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%rv,1)*SIZE(grid%rv,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%rv, 2, 4, 0, 1, DATA_ORDER_XZY, 1, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%rw,1)*SIZE(grid%rw,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%rw, 2, 4, 0, 1, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%ww,1)*SIZE(grid%ww,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%ww, 2, 4, 0, 1, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%php,1)*SIZE(grid%php,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%php, 2, 4, 0, 1, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%alt,1)*SIZE(grid%alt,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%alt, 2, 4, 0, 1, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%p,1)*SIZE(grid%p,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%p, 2, 4, 0, 1, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%muu,1)*SIZE(grid%muu,2) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%muu, 2, 4, 0, 1, DATA_ORDER_XY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
ENDIF
IF ( SIZE(grid%muv,1)*SIZE(grid%muv,2) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%muv, 2, 4, 0, 1, DATA_ORDER_XY, 1, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
ENDIF
IF ( SIZE(grid%mut,1)*SIZE(grid%mut,2) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%mut, 2, 4, 0, 1, DATA_ORDER_XY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
ENDIF
IF ( SIZE(grid%ph_2,1)*SIZE(grid%ph_2,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%ph_2, 2, 4, 0, 1, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%al,1)*SIZE(grid%al,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%al, 2, 4, 0, 1, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
END IF

  
  END SUBROUTINE PERIOD_BDY_EM_A_sub

SUBROUTINE PERIOD_BDY_EM_A1_sub ( grid, &
  config_flags, &
  local_communicator_periodic, &
  mytask, ntasks, ntasks_x, ntasks_y, &
  ids, ide, jds, jde, kds, kde,       &
  ims, ime, jms, jme, kms, kme,       &
  ips, ipe, jps, jpe, kps, kpe )
  USE module_domain, ONLY:domain
  USE module_configure, ONLY:grid_config_rec_type,in_use_for_config
  USE module_state_description, ONLY:PARAM_FIRST_SCALAR
  USE module_driver_constants
  TYPE(domain) ,               INTENT(IN) :: grid
  TYPE(grid_config_rec_type) , INTENT(IN) :: config_flags
  INTEGER ,                    INTENT(IN) :: local_communicator_periodic
  INTEGER ,                    INTENT(IN) :: mytask, ntasks, ntasks_x, ntasks_y
  INTEGER ,                    INTENT(IN) :: ids, ide, jds, jde, kds, kde
  INTEGER ,                    INTENT(IN) :: ims, ime, jms, jme, kms, kme
  INTEGER ,                    INTENT(IN) :: ips, ipe, jps, jpe, kps, kpe
  INTEGER :: itrace
  INTEGER :: rsl_sendw_p, rsl_sendbeg_p, rsl_recvw_p, rsl_recvbeg_p
  INTEGER :: rsl_sendw_m, rsl_sendbeg_m, rsl_recvw_m, rsl_recvbeg_m
  LOGICAL, EXTERNAL :: rsl_comm_iter
  INTEGER :: idim1, idim2, idim3, idim4, idim5, idim6, idim7
  






CALL wrf_debug(2,'calling inc/PERIOD_BDY_EM_A1_inline.inc')
IF ( config_flags%periodic_x ) THEN
CALL RSL_LITE_INIT_PERIOD ( local_communicator_periodic, 3 , &
     5, 1, 4, &
     0, 0, 4, &
     0, 0, 8, &
      0,  0, 4, &
      mytask, ntasks, ntasks_x, ntasks_y,   &
      ips, ipe, jps, jpe, kps, kpe    )
IF ( SIZE(grid%rdzw,1)*SIZE(grid%rdzw,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%rdzw, 3, 4, 1, 0, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%rdz,1)*SIZE(grid%rdz,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%rdz, 3, 4, 1, 0, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%z,1)*SIZE(grid%z,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%z, 3, 4, 1, 0, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%zx,1)*SIZE(grid%zx,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%zx, 3, 4, 1, 0, DATA_ORDER_XZY, 1, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%zy,1)*SIZE(grid%zy,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%zy, 3, 4, 1, 0, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%ustm,1)*SIZE(grid%ustm,2) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%ustm, 3, 4, 1, 0, DATA_ORDER_XY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
ENDIF
   CALL RSL_LITE_EXCH_PERIOD_X ( local_communicator_periodic , mytask, ntasks, ntasks_x, ntasks_y )
IF ( SIZE(grid%rdzw,1)*SIZE(grid%rdzw,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%rdzw, 3, 4, 1, 1, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%rdz,1)*SIZE(grid%rdz,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%rdz, 3, 4, 1, 1, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%z,1)*SIZE(grid%z,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%z, 3, 4, 1, 1, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%zx,1)*SIZE(grid%zx,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%zx, 3, 4, 1, 1, DATA_ORDER_XZY, 1, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%zy,1)*SIZE(grid%zy,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%zy, 3, 4, 1, 1, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%ustm,1)*SIZE(grid%ustm,2) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%ustm, 3, 4, 1, 1, DATA_ORDER_XY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
ENDIF
END IF
IF ( config_flags%periodic_y ) THEN
CALL RSL_LITE_INIT_PERIOD ( local_communicator_periodic, 3 , &
     5, 1, 4, &
     0, 0, 4, &
     0, 0, 8, &
      0,  0, 4, &
      mytask, ntasks, ntasks_x, ntasks_y,   &
      ips, ipe, jps, jpe, kps, kpe    )
IF ( SIZE(grid%rdzw,1)*SIZE(grid%rdzw,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%rdzw, 3, 4, 0, 0, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%rdz,1)*SIZE(grid%rdz,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%rdz, 3, 4, 0, 0, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%z,1)*SIZE(grid%z,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%z, 3, 4, 0, 0, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%zx,1)*SIZE(grid%zx,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%zx, 3, 4, 0, 0, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%zy,1)*SIZE(grid%zy,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%zy, 3, 4, 0, 0, DATA_ORDER_XZY, 1, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%ustm,1)*SIZE(grid%ustm,2) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%ustm, 3, 4, 0, 0, DATA_ORDER_XY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
ENDIF
   CALL RSL_LITE_EXCH_PERIOD_Y ( local_communicator_periodic , mytask, ntasks, ntasks_x, ntasks_y )
IF ( SIZE(grid%rdzw,1)*SIZE(grid%rdzw,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%rdzw, 3, 4, 0, 1, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%rdz,1)*SIZE(grid%rdz,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%rdz, 3, 4, 0, 1, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%z,1)*SIZE(grid%z,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%z, 3, 4, 0, 1, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%zx,1)*SIZE(grid%zx,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%zx, 3, 4, 0, 1, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%zy,1)*SIZE(grid%zy,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%zy, 3, 4, 0, 1, DATA_ORDER_XZY, 1, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%ustm,1)*SIZE(grid%ustm,2) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%ustm, 3, 4, 0, 1, DATA_ORDER_XY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
ENDIF
END IF

  
  END SUBROUTINE PERIOD_BDY_EM_A1_sub

SUBROUTINE PERIOD_BDY_EM_PHY_BC_sub ( grid, &
  config_flags, &
  local_communicator_periodic, &
  mytask, ntasks, ntasks_x, ntasks_y, &
  ids, ide, jds, jde, kds, kde,       &
  ims, ime, jms, jme, kms, kme,       &
  ips, ipe, jps, jpe, kps, kpe )
  USE module_domain, ONLY:domain
  USE module_configure, ONLY:grid_config_rec_type,in_use_for_config
  USE module_state_description, ONLY:PARAM_FIRST_SCALAR
  USE module_driver_constants
  TYPE(domain) ,               INTENT(IN) :: grid
  TYPE(grid_config_rec_type) , INTENT(IN) :: config_flags
  INTEGER ,                    INTENT(IN) :: local_communicator_periodic
  INTEGER ,                    INTENT(IN) :: mytask, ntasks, ntasks_x, ntasks_y
  INTEGER ,                    INTENT(IN) :: ids, ide, jds, jde, kds, kde
  INTEGER ,                    INTENT(IN) :: ims, ime, jms, jme, kms, kme
  INTEGER ,                    INTENT(IN) :: ips, ipe, jps, jpe, kps, kpe
  INTEGER :: itrace
  INTEGER :: rsl_sendw_p, rsl_sendbeg_p, rsl_recvw_p, rsl_recvbeg_p
  INTEGER :: rsl_sendw_m, rsl_sendbeg_m, rsl_recvw_m, rsl_recvbeg_m
  LOGICAL, EXTERNAL :: rsl_comm_iter
  INTEGER :: idim1, idim2, idim3, idim4, idim5, idim6, idim7
  






CALL wrf_debug(2,'calling inc/PERIOD_BDY_EM_PHY_BC_inline.inc')
IF ( config_flags%periodic_x ) THEN
CALL RSL_LITE_INIT_PERIOD ( local_communicator_periodic, 2 , &
     16, 0, 4, &
     0, 0, 4, &
     0, 0, 8, &
      0,  0, 4, &
      mytask, ntasks, ntasks_x, ntasks_y,   &
      ips, ipe, jps, jpe, kps, kpe    )
IF ( SIZE(grid%rublten,1)*SIZE(grid%rublten,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%rublten, 2, 4, 1, 0, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%rvblten,1)*SIZE(grid%rvblten,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%rvblten, 2, 4, 1, 0, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%rucuten,1)*SIZE(grid%rucuten,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%rucuten, 2, 4, 1, 0, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%rvcuten,1)*SIZE(grid%rvcuten,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%rvcuten, 2, 4, 1, 0, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%xkmh,1)*SIZE(grid%xkmh,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%xkmh, 2, 4, 1, 0, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%xkmv,1)*SIZE(grid%xkmv,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%xkmv, 2, 4, 1, 0, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%xkhh,1)*SIZE(grid%xkhh,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%xkhh, 2, 4, 1, 0, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%xkhv,1)*SIZE(grid%xkhv,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%xkhv, 2, 4, 1, 0, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%div,1)*SIZE(grid%div,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%div, 2, 4, 1, 0, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%defor11,1)*SIZE(grid%defor11,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%defor11, 2, 4, 1, 0, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%defor22,1)*SIZE(grid%defor22,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%defor22, 2, 4, 1, 0, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%defor12,1)*SIZE(grid%defor12,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%defor12, 2, 4, 1, 0, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%defor13,1)*SIZE(grid%defor13,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%defor13, 2, 4, 1, 0, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%defor23,1)*SIZE(grid%defor23,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%defor23, 2, 4, 1, 0, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%defor33,1)*SIZE(grid%defor33,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%defor33, 2, 4, 1, 0, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%tke_2,1)*SIZE(grid%tke_2,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%tke_2, 2, 4, 1, 0, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
   CALL RSL_LITE_EXCH_PERIOD_X ( local_communicator_periodic , mytask, ntasks, ntasks_x, ntasks_y )
IF ( SIZE(grid%rublten,1)*SIZE(grid%rublten,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%rublten, 2, 4, 1, 1, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%rvblten,1)*SIZE(grid%rvblten,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%rvblten, 2, 4, 1, 1, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%rucuten,1)*SIZE(grid%rucuten,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%rucuten, 2, 4, 1, 1, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%rvcuten,1)*SIZE(grid%rvcuten,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%rvcuten, 2, 4, 1, 1, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%xkmh,1)*SIZE(grid%xkmh,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%xkmh, 2, 4, 1, 1, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%xkmv,1)*SIZE(grid%xkmv,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%xkmv, 2, 4, 1, 1, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%xkhh,1)*SIZE(grid%xkhh,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%xkhh, 2, 4, 1, 1, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%xkhv,1)*SIZE(grid%xkhv,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%xkhv, 2, 4, 1, 1, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%div,1)*SIZE(grid%div,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%div, 2, 4, 1, 1, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%defor11,1)*SIZE(grid%defor11,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%defor11, 2, 4, 1, 1, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%defor22,1)*SIZE(grid%defor22,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%defor22, 2, 4, 1, 1, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%defor12,1)*SIZE(grid%defor12,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%defor12, 2, 4, 1, 1, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%defor13,1)*SIZE(grid%defor13,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%defor13, 2, 4, 1, 1, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%defor23,1)*SIZE(grid%defor23,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%defor23, 2, 4, 1, 1, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%defor33,1)*SIZE(grid%defor33,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%defor33, 2, 4, 1, 1, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%tke_2,1)*SIZE(grid%tke_2,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%tke_2, 2, 4, 1, 1, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
END IF
IF ( config_flags%periodic_y ) THEN
CALL RSL_LITE_INIT_PERIOD ( local_communicator_periodic, 2 , &
     16, 0, 4, &
     0, 0, 4, &
     0, 0, 8, &
      0,  0, 4, &
      mytask, ntasks, ntasks_x, ntasks_y,   &
      ips, ipe, jps, jpe, kps, kpe    )
IF ( SIZE(grid%rublten,1)*SIZE(grid%rublten,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%rublten, 2, 4, 0, 0, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%rvblten,1)*SIZE(grid%rvblten,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%rvblten, 2, 4, 0, 0, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%rucuten,1)*SIZE(grid%rucuten,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%rucuten, 2, 4, 0, 0, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%rvcuten,1)*SIZE(grid%rvcuten,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%rvcuten, 2, 4, 0, 0, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%xkmh,1)*SIZE(grid%xkmh,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%xkmh, 2, 4, 0, 0, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%xkmv,1)*SIZE(grid%xkmv,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%xkmv, 2, 4, 0, 0, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%xkhh,1)*SIZE(grid%xkhh,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%xkhh, 2, 4, 0, 0, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%xkhv,1)*SIZE(grid%xkhv,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%xkhv, 2, 4, 0, 0, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%div,1)*SIZE(grid%div,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%div, 2, 4, 0, 0, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%defor11,1)*SIZE(grid%defor11,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%defor11, 2, 4, 0, 0, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%defor22,1)*SIZE(grid%defor22,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%defor22, 2, 4, 0, 0, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%defor12,1)*SIZE(grid%defor12,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%defor12, 2, 4, 0, 0, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%defor13,1)*SIZE(grid%defor13,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%defor13, 2, 4, 0, 0, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%defor23,1)*SIZE(grid%defor23,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%defor23, 2, 4, 0, 0, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%defor33,1)*SIZE(grid%defor33,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%defor33, 2, 4, 0, 0, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%tke_2,1)*SIZE(grid%tke_2,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%tke_2, 2, 4, 0, 0, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
   CALL RSL_LITE_EXCH_PERIOD_Y ( local_communicator_periodic , mytask, ntasks, ntasks_x, ntasks_y )
IF ( SIZE(grid%rublten,1)*SIZE(grid%rublten,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%rublten, 2, 4, 0, 1, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%rvblten,1)*SIZE(grid%rvblten,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%rvblten, 2, 4, 0, 1, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%rucuten,1)*SIZE(grid%rucuten,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%rucuten, 2, 4, 0, 1, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%rvcuten,1)*SIZE(grid%rvcuten,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%rvcuten, 2, 4, 0, 1, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%xkmh,1)*SIZE(grid%xkmh,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%xkmh, 2, 4, 0, 1, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%xkmv,1)*SIZE(grid%xkmv,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%xkmv, 2, 4, 0, 1, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%xkhh,1)*SIZE(grid%xkhh,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%xkhh, 2, 4, 0, 1, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%xkhv,1)*SIZE(grid%xkhv,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%xkhv, 2, 4, 0, 1, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%div,1)*SIZE(grid%div,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%div, 2, 4, 0, 1, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%defor11,1)*SIZE(grid%defor11,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%defor11, 2, 4, 0, 1, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%defor22,1)*SIZE(grid%defor22,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%defor22, 2, 4, 0, 1, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%defor12,1)*SIZE(grid%defor12,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%defor12, 2, 4, 0, 1, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%defor13,1)*SIZE(grid%defor13,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%defor13, 2, 4, 0, 1, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%defor23,1)*SIZE(grid%defor23,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%defor23, 2, 4, 0, 1, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%defor33,1)*SIZE(grid%defor33,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%defor33, 2, 4, 0, 1, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%tke_2,1)*SIZE(grid%tke_2,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%tke_2, 2, 4, 0, 1, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
END IF

  
  END SUBROUTINE PERIOD_BDY_EM_PHY_BC_sub

SUBROUTINE PERIOD_BDY_EM_FDDA_BC_sub ( grid, &
  config_flags, &
  local_communicator_periodic, &
  mytask, ntasks, ntasks_x, ntasks_y, &
  ids, ide, jds, jde, kds, kde,       &
  ims, ime, jms, jme, kms, kme,       &
  ips, ipe, jps, jpe, kps, kpe )
  USE module_domain, ONLY:domain
  USE module_configure, ONLY:grid_config_rec_type,in_use_for_config
  USE module_state_description, ONLY:PARAM_FIRST_SCALAR
  USE module_driver_constants
  TYPE(domain) ,               INTENT(IN) :: grid
  TYPE(grid_config_rec_type) , INTENT(IN) :: config_flags
  INTEGER ,                    INTENT(IN) :: local_communicator_periodic
  INTEGER ,                    INTENT(IN) :: mytask, ntasks, ntasks_x, ntasks_y
  INTEGER ,                    INTENT(IN) :: ids, ide, jds, jde, kds, kde
  INTEGER ,                    INTENT(IN) :: ims, ime, jms, jme, kms, kme
  INTEGER ,                    INTENT(IN) :: ips, ipe, jps, jpe, kps, kpe
  INTEGER :: itrace
  INTEGER :: rsl_sendw_p, rsl_sendbeg_p, rsl_recvw_p, rsl_recvbeg_p
  INTEGER :: rsl_sendw_m, rsl_sendbeg_m, rsl_recvw_m, rsl_recvbeg_m
  LOGICAL, EXTERNAL :: rsl_comm_iter
  INTEGER :: idim1, idim2, idim3, idim4, idim5, idim6, idim7
  






CALL wrf_debug(2,'calling inc/PERIOD_BDY_EM_FDDA_BC_inline.inc')
IF ( config_flags%periodic_x ) THEN
CALL RSL_LITE_INIT_PERIOD ( local_communicator_periodic, 2 , &
     2, 0, 4, &
     0, 0, 4, &
     0, 0, 8, &
      0,  0, 4, &
      mytask, ntasks, ntasks_x, ntasks_y,   &
      ips, ipe, jps, jpe, kps, kpe    )
IF ( SIZE(grid%rundgdten,1)*SIZE(grid%rundgdten,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%rundgdten, 2, 4, 1, 0, DATA_ORDER_XZY, 1, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%rvndgdten,1)*SIZE(grid%rvndgdten,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%rvndgdten, 2, 4, 1, 0, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
   CALL RSL_LITE_EXCH_PERIOD_X ( local_communicator_periodic , mytask, ntasks, ntasks_x, ntasks_y )
IF ( SIZE(grid%rundgdten,1)*SIZE(grid%rundgdten,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%rundgdten, 2, 4, 1, 1, DATA_ORDER_XZY, 1, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%rvndgdten,1)*SIZE(grid%rvndgdten,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%rvndgdten, 2, 4, 1, 1, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
END IF
IF ( config_flags%periodic_y ) THEN
CALL RSL_LITE_INIT_PERIOD ( local_communicator_periodic, 2 , &
     2, 0, 4, &
     0, 0, 4, &
     0, 0, 8, &
      0,  0, 4, &
      mytask, ntasks, ntasks_x, ntasks_y,   &
      ips, ipe, jps, jpe, kps, kpe    )
IF ( SIZE(grid%rundgdten,1)*SIZE(grid%rundgdten,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%rundgdten, 2, 4, 0, 0, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%rvndgdten,1)*SIZE(grid%rvndgdten,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%rvndgdten, 2, 4, 0, 0, DATA_ORDER_XZY, 1, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
   CALL RSL_LITE_EXCH_PERIOD_Y ( local_communicator_periodic , mytask, ntasks, ntasks_x, ntasks_y )
IF ( SIZE(grid%rundgdten,1)*SIZE(grid%rundgdten,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%rundgdten, 2, 4, 0, 1, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%rvndgdten,1)*SIZE(grid%rvndgdten,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%rvndgdten, 2, 4, 0, 1, DATA_ORDER_XZY, 1, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
END IF

  
  END SUBROUTINE PERIOD_BDY_EM_FDDA_BC_sub

SUBROUTINE PERIOD_BDY_EM_B_sub ( grid, &
  config_flags, &
  local_communicator_periodic, &
  mytask, ntasks, ntasks_x, ntasks_y, &
  ids, ide, jds, jde, kds, kde,       &
  ims, ime, jms, jme, kms, kme,       &
  ips, ipe, jps, jpe, kps, kpe )
  USE module_domain, ONLY:domain
  USE module_configure, ONLY:grid_config_rec_type,in_use_for_config
  USE module_state_description, ONLY:PARAM_FIRST_SCALAR
  USE module_driver_constants
  TYPE(domain) ,               INTENT(IN) :: grid
  TYPE(grid_config_rec_type) , INTENT(IN) :: config_flags
  INTEGER ,                    INTENT(IN) :: local_communicator_periodic
  INTEGER ,                    INTENT(IN) :: mytask, ntasks, ntasks_x, ntasks_y
  INTEGER ,                    INTENT(IN) :: ids, ide, jds, jde, kds, kde
  INTEGER ,                    INTENT(IN) :: ims, ime, jms, jme, kms, kme
  INTEGER ,                    INTENT(IN) :: ips, ipe, jps, jpe, kps, kpe
  INTEGER :: itrace
  INTEGER :: rsl_sendw_p, rsl_sendbeg_p, rsl_recvw_p, rsl_recvbeg_p
  INTEGER :: rsl_sendw_m, rsl_sendbeg_m, rsl_recvw_m, rsl_recvbeg_m
  LOGICAL, EXTERNAL :: rsl_comm_iter
  INTEGER :: idim1, idim2, idim3, idim4, idim5, idim6, idim7
  






CALL wrf_debug(2,'calling inc/PERIOD_BDY_EM_B_inline.inc')
IF ( config_flags%periodic_x ) THEN
CALL RSL_LITE_INIT_PERIOD ( local_communicator_periodic, 2 , &
     12, 3, 4, &
     0, 0, 4, &
     0, 0, 8, &
      0,  0, 4, &
      mytask, ntasks, ntasks_x, ntasks_y,   &
      ips, ipe, jps, jpe, kps, kpe    )
IF ( SIZE(grid%ru_tend,1)*SIZE(grid%ru_tend,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%ru_tend, 2, 4, 1, 0, DATA_ORDER_XZY, 1, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%rv_tend,1)*SIZE(grid%rv_tend,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%rv_tend, 2, 4, 1, 0, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%ph_2,1)*SIZE(grid%ph_2,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%ph_2, 2, 4, 1, 0, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%al,1)*SIZE(grid%al,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%al, 2, 4, 1, 0, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%p,1)*SIZE(grid%p,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%p, 2, 4, 1, 0, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%t_1,1)*SIZE(grid%t_1,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%t_1, 2, 4, 1, 0, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%t_save,1)*SIZE(grid%t_save,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%t_save, 2, 4, 1, 0, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%u_save,1)*SIZE(grid%u_save,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%u_save, 2, 4, 1, 0, DATA_ORDER_XZY, 1, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%v_save,1)*SIZE(grid%v_save,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%v_save, 2, 4, 1, 0, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%mu_1,1)*SIZE(grid%mu_1,2) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%mu_1, 2, 4, 1, 0, DATA_ORDER_XY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
ENDIF
IF ( SIZE(grid%mu_2,1)*SIZE(grid%mu_2,2) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%mu_2, 2, 4, 1, 0, DATA_ORDER_XY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
ENDIF
IF ( SIZE(grid%mudf,1)*SIZE(grid%mudf,2) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%mudf, 2, 4, 1, 0, DATA_ORDER_XY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
ENDIF
IF ( SIZE(grid%php,1)*SIZE(grid%php,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%php, 2, 4, 1, 0, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%alt,1)*SIZE(grid%alt,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%alt, 2, 4, 1, 0, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%pb,1)*SIZE(grid%pb,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%pb, 2, 4, 1, 0, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
   CALL RSL_LITE_EXCH_PERIOD_X ( local_communicator_periodic , mytask, ntasks, ntasks_x, ntasks_y )
IF ( SIZE(grid%ru_tend,1)*SIZE(grid%ru_tend,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%ru_tend, 2, 4, 1, 1, DATA_ORDER_XZY, 1, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%rv_tend,1)*SIZE(grid%rv_tend,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%rv_tend, 2, 4, 1, 1, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%ph_2,1)*SIZE(grid%ph_2,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%ph_2, 2, 4, 1, 1, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%al,1)*SIZE(grid%al,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%al, 2, 4, 1, 1, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%p,1)*SIZE(grid%p,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%p, 2, 4, 1, 1, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%t_1,1)*SIZE(grid%t_1,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%t_1, 2, 4, 1, 1, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%t_save,1)*SIZE(grid%t_save,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%t_save, 2, 4, 1, 1, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%u_save,1)*SIZE(grid%u_save,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%u_save, 2, 4, 1, 1, DATA_ORDER_XZY, 1, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%v_save,1)*SIZE(grid%v_save,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%v_save, 2, 4, 1, 1, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%mu_1,1)*SIZE(grid%mu_1,2) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%mu_1, 2, 4, 1, 1, DATA_ORDER_XY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
ENDIF
IF ( SIZE(grid%mu_2,1)*SIZE(grid%mu_2,2) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%mu_2, 2, 4, 1, 1, DATA_ORDER_XY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
ENDIF
IF ( SIZE(grid%mudf,1)*SIZE(grid%mudf,2) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%mudf, 2, 4, 1, 1, DATA_ORDER_XY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
ENDIF
IF ( SIZE(grid%php,1)*SIZE(grid%php,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%php, 2, 4, 1, 1, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%alt,1)*SIZE(grid%alt,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%alt, 2, 4, 1, 1, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%pb,1)*SIZE(grid%pb,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%pb, 2, 4, 1, 1, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
END IF
IF ( config_flags%periodic_y ) THEN
CALL RSL_LITE_INIT_PERIOD ( local_communicator_periodic, 2 , &
     12, 3, 4, &
     0, 0, 4, &
     0, 0, 8, &
      0,  0, 4, &
      mytask, ntasks, ntasks_x, ntasks_y,   &
      ips, ipe, jps, jpe, kps, kpe    )
IF ( SIZE(grid%ru_tend,1)*SIZE(grid%ru_tend,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%ru_tend, 2, 4, 0, 0, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%rv_tend,1)*SIZE(grid%rv_tend,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%rv_tend, 2, 4, 0, 0, DATA_ORDER_XZY, 1, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%ph_2,1)*SIZE(grid%ph_2,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%ph_2, 2, 4, 0, 0, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%al,1)*SIZE(grid%al,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%al, 2, 4, 0, 0, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%p,1)*SIZE(grid%p,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%p, 2, 4, 0, 0, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%t_1,1)*SIZE(grid%t_1,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%t_1, 2, 4, 0, 0, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%t_save,1)*SIZE(grid%t_save,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%t_save, 2, 4, 0, 0, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%u_save,1)*SIZE(grid%u_save,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%u_save, 2, 4, 0, 0, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%v_save,1)*SIZE(grid%v_save,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%v_save, 2, 4, 0, 0, DATA_ORDER_XZY, 1, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%mu_1,1)*SIZE(grid%mu_1,2) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%mu_1, 2, 4, 0, 0, DATA_ORDER_XY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
ENDIF
IF ( SIZE(grid%mu_2,1)*SIZE(grid%mu_2,2) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%mu_2, 2, 4, 0, 0, DATA_ORDER_XY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
ENDIF
IF ( SIZE(grid%mudf,1)*SIZE(grid%mudf,2) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%mudf, 2, 4, 0, 0, DATA_ORDER_XY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
ENDIF
IF ( SIZE(grid%php,1)*SIZE(grid%php,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%php, 2, 4, 0, 0, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%alt,1)*SIZE(grid%alt,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%alt, 2, 4, 0, 0, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%pb,1)*SIZE(grid%pb,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%pb, 2, 4, 0, 0, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
   CALL RSL_LITE_EXCH_PERIOD_Y ( local_communicator_periodic , mytask, ntasks, ntasks_x, ntasks_y )
IF ( SIZE(grid%ru_tend,1)*SIZE(grid%ru_tend,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%ru_tend, 2, 4, 0, 1, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%rv_tend,1)*SIZE(grid%rv_tend,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%rv_tend, 2, 4, 0, 1, DATA_ORDER_XZY, 1, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%ph_2,1)*SIZE(grid%ph_2,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%ph_2, 2, 4, 0, 1, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%al,1)*SIZE(grid%al,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%al, 2, 4, 0, 1, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%p,1)*SIZE(grid%p,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%p, 2, 4, 0, 1, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%t_1,1)*SIZE(grid%t_1,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%t_1, 2, 4, 0, 1, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%t_save,1)*SIZE(grid%t_save,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%t_save, 2, 4, 0, 1, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%u_save,1)*SIZE(grid%u_save,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%u_save, 2, 4, 0, 1, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%v_save,1)*SIZE(grid%v_save,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%v_save, 2, 4, 0, 1, DATA_ORDER_XZY, 1, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%mu_1,1)*SIZE(grid%mu_1,2) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%mu_1, 2, 4, 0, 1, DATA_ORDER_XY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
ENDIF
IF ( SIZE(grid%mu_2,1)*SIZE(grid%mu_2,2) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%mu_2, 2, 4, 0, 1, DATA_ORDER_XY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
ENDIF
IF ( SIZE(grid%mudf,1)*SIZE(grid%mudf,2) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%mudf, 2, 4, 0, 1, DATA_ORDER_XY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
ENDIF
IF ( SIZE(grid%php,1)*SIZE(grid%php,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%php, 2, 4, 0, 1, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%alt,1)*SIZE(grid%alt,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%alt, 2, 4, 0, 1, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%pb,1)*SIZE(grid%pb,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%pb, 2, 4, 0, 1, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
END IF

  
  END SUBROUTINE PERIOD_BDY_EM_B_sub

SUBROUTINE PERIOD_BDY_EM_B3_sub ( grid, &
  config_flags, &
  local_communicator_periodic, &
  mytask, ntasks, ntasks_x, ntasks_y, &
  ids, ide, jds, jde, kds, kde,       &
  ims, ime, jms, jme, kms, kme,       &
  ips, ipe, jps, jpe, kps, kpe )
  USE module_domain, ONLY:domain
  USE module_configure, ONLY:grid_config_rec_type,in_use_for_config
  USE module_state_description, ONLY:PARAM_FIRST_SCALAR
  USE module_driver_constants
  TYPE(domain) ,               INTENT(IN) :: grid
  TYPE(grid_config_rec_type) , INTENT(IN) :: config_flags
  INTEGER ,                    INTENT(IN) :: local_communicator_periodic
  INTEGER ,                    INTENT(IN) :: mytask, ntasks, ntasks_x, ntasks_y
  INTEGER ,                    INTENT(IN) :: ids, ide, jds, jde, kds, kde
  INTEGER ,                    INTENT(IN) :: ims, ime, jms, jme, kms, kme
  INTEGER ,                    INTENT(IN) :: ips, ipe, jps, jpe, kps, kpe
  INTEGER :: itrace
  INTEGER :: rsl_sendw_p, rsl_sendbeg_p, rsl_recvw_p, rsl_recvbeg_p
  INTEGER :: rsl_sendw_m, rsl_sendbeg_m, rsl_recvw_m, rsl_recvbeg_m
  LOGICAL, EXTERNAL :: rsl_comm_iter
  INTEGER :: idim1, idim2, idim3, idim4, idim5, idim6, idim7
  






CALL wrf_debug(2,'calling inc/PERIOD_BDY_EM_B3_inline.inc')
IF ( config_flags%periodic_x ) THEN
CALL RSL_LITE_INIT_PERIOD ( local_communicator_periodic, 2 , &
     3, 3, 4, &
     0, 0, 4, &
     0, 0, 8, &
      0,  0, 4, &
      mytask, ntasks, ntasks_x, ntasks_y,   &
      ips, ipe, jps, jpe, kps, kpe    )
IF ( SIZE(grid%ph_2,1)*SIZE(grid%ph_2,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%ph_2, 2, 4, 1, 0, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%al,1)*SIZE(grid%al,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%al, 2, 4, 1, 0, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%p,1)*SIZE(grid%p,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%p, 2, 4, 1, 0, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%mu_2,1)*SIZE(grid%mu_2,2) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%mu_2, 2, 4, 1, 0, DATA_ORDER_XY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
ENDIF
IF ( SIZE(grid%muts,1)*SIZE(grid%muts,2) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%muts, 2, 4, 1, 0, DATA_ORDER_XY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
ENDIF
IF ( SIZE(grid%mudf,1)*SIZE(grid%mudf,2) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%mudf, 2, 4, 1, 0, DATA_ORDER_XY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
ENDIF
   CALL RSL_LITE_EXCH_PERIOD_X ( local_communicator_periodic , mytask, ntasks, ntasks_x, ntasks_y )
IF ( SIZE(grid%ph_2,1)*SIZE(grid%ph_2,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%ph_2, 2, 4, 1, 1, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%al,1)*SIZE(grid%al,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%al, 2, 4, 1, 1, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%p,1)*SIZE(grid%p,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%p, 2, 4, 1, 1, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%mu_2,1)*SIZE(grid%mu_2,2) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%mu_2, 2, 4, 1, 1, DATA_ORDER_XY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
ENDIF
IF ( SIZE(grid%muts,1)*SIZE(grid%muts,2) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%muts, 2, 4, 1, 1, DATA_ORDER_XY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
ENDIF
IF ( SIZE(grid%mudf,1)*SIZE(grid%mudf,2) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%mudf, 2, 4, 1, 1, DATA_ORDER_XY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
ENDIF
END IF
IF ( config_flags%periodic_y ) THEN
CALL RSL_LITE_INIT_PERIOD ( local_communicator_periodic, 2 , &
     3, 3, 4, &
     0, 0, 4, &
     0, 0, 8, &
      0,  0, 4, &
      mytask, ntasks, ntasks_x, ntasks_y,   &
      ips, ipe, jps, jpe, kps, kpe    )
IF ( SIZE(grid%ph_2,1)*SIZE(grid%ph_2,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%ph_2, 2, 4, 0, 0, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%al,1)*SIZE(grid%al,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%al, 2, 4, 0, 0, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%p,1)*SIZE(grid%p,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%p, 2, 4, 0, 0, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%mu_2,1)*SIZE(grid%mu_2,2) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%mu_2, 2, 4, 0, 0, DATA_ORDER_XY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
ENDIF
IF ( SIZE(grid%muts,1)*SIZE(grid%muts,2) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%muts, 2, 4, 0, 0, DATA_ORDER_XY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
ENDIF
IF ( SIZE(grid%mudf,1)*SIZE(grid%mudf,2) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%mudf, 2, 4, 0, 0, DATA_ORDER_XY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
ENDIF
   CALL RSL_LITE_EXCH_PERIOD_Y ( local_communicator_periodic , mytask, ntasks, ntasks_x, ntasks_y )
IF ( SIZE(grid%ph_2,1)*SIZE(grid%ph_2,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%ph_2, 2, 4, 0, 1, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%al,1)*SIZE(grid%al,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%al, 2, 4, 0, 1, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%p,1)*SIZE(grid%p,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%p, 2, 4, 0, 1, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%mu_2,1)*SIZE(grid%mu_2,2) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%mu_2, 2, 4, 0, 1, DATA_ORDER_XY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
ENDIF
IF ( SIZE(grid%muts,1)*SIZE(grid%muts,2) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%muts, 2, 4, 0, 1, DATA_ORDER_XY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
ENDIF
IF ( SIZE(grid%mudf,1)*SIZE(grid%mudf,2) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%mudf, 2, 4, 0, 1, DATA_ORDER_XY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
ENDIF
END IF

  
  END SUBROUTINE PERIOD_BDY_EM_B3_sub

SUBROUTINE PERIOD_BDY_EM_B2_sub ( grid, &
  config_flags, &
  local_communicator_periodic, &
  mytask, ntasks, ntasks_x, ntasks_y, &
  ids, ide, jds, jde, kds, kde,       &
  ims, ime, jms, jme, kms, kme,       &
  ips, ipe, jps, jpe, kps, kpe )
  USE module_domain, ONLY:domain
  USE module_configure, ONLY:grid_config_rec_type,in_use_for_config
  USE module_state_description, ONLY:PARAM_FIRST_SCALAR
  USE module_driver_constants
  TYPE(domain) ,               INTENT(IN) :: grid
  TYPE(grid_config_rec_type) , INTENT(IN) :: config_flags
  INTEGER ,                    INTENT(IN) :: local_communicator_periodic
  INTEGER ,                    INTENT(IN) :: mytask, ntasks, ntasks_x, ntasks_y
  INTEGER ,                    INTENT(IN) :: ids, ide, jds, jde, kds, kde
  INTEGER ,                    INTENT(IN) :: ims, ime, jms, jme, kms, kme
  INTEGER ,                    INTENT(IN) :: ips, ipe, jps, jpe, kps, kpe
  INTEGER :: itrace
  INTEGER :: rsl_sendw_p, rsl_sendbeg_p, rsl_recvw_p, rsl_recvbeg_p
  INTEGER :: rsl_sendw_m, rsl_sendbeg_m, rsl_recvw_m, rsl_recvbeg_m
  LOGICAL, EXTERNAL :: rsl_comm_iter
  INTEGER :: idim1, idim2, idim3, idim4, idim5, idim6, idim7
  






CALL wrf_debug(2,'calling inc/PERIOD_BDY_EM_B2_inline.inc')
IF ( config_flags%periodic_x ) THEN
CALL RSL_LITE_INIT_PERIOD ( local_communicator_periodic, 2 , &
     2, 0, 4, &
     0, 0, 4, &
     0, 0, 8, &
      0,  0, 4, &
      mytask, ntasks, ntasks_x, ntasks_y,   &
      ips, ipe, jps, jpe, kps, kpe    )
IF ( SIZE(grid%ru_tend,1)*SIZE(grid%ru_tend,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%ru_tend, 2, 4, 1, 0, DATA_ORDER_XZY, 1, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%rv_tend,1)*SIZE(grid%rv_tend,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%rv_tend, 2, 4, 1, 0, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
   CALL RSL_LITE_EXCH_PERIOD_X ( local_communicator_periodic , mytask, ntasks, ntasks_x, ntasks_y )
IF ( SIZE(grid%ru_tend,1)*SIZE(grid%ru_tend,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%ru_tend, 2, 4, 1, 1, DATA_ORDER_XZY, 1, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%rv_tend,1)*SIZE(grid%rv_tend,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%rv_tend, 2, 4, 1, 1, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
END IF
IF ( config_flags%periodic_y ) THEN
CALL RSL_LITE_INIT_PERIOD ( local_communicator_periodic, 2 , &
     2, 0, 4, &
     0, 0, 4, &
     0, 0, 8, &
      0,  0, 4, &
      mytask, ntasks, ntasks_x, ntasks_y,   &
      ips, ipe, jps, jpe, kps, kpe    )
IF ( SIZE(grid%ru_tend,1)*SIZE(grid%ru_tend,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%ru_tend, 2, 4, 0, 0, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%rv_tend,1)*SIZE(grid%rv_tend,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%rv_tend, 2, 4, 0, 0, DATA_ORDER_XZY, 1, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
   CALL RSL_LITE_EXCH_PERIOD_Y ( local_communicator_periodic , mytask, ntasks, ntasks_x, ntasks_y )
IF ( SIZE(grid%ru_tend,1)*SIZE(grid%ru_tend,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%ru_tend, 2, 4, 0, 1, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%rv_tend,1)*SIZE(grid%rv_tend,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%rv_tend, 2, 4, 0, 1, DATA_ORDER_XZY, 1, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
END IF

  
  END SUBROUTINE PERIOD_BDY_EM_B2_sub

SUBROUTINE PERIOD_BDY_EM_C_sub ( grid, &
  config_flags, &
  local_communicator_periodic, &
  mytask, ntasks, ntasks_x, ntasks_y, &
  ids, ide, jds, jde, kds, kde,       &
  ims, ime, jms, jme, kms, kme,       &
  ips, ipe, jps, jpe, kps, kpe )
  USE module_domain, ONLY:domain
  USE module_configure, ONLY:grid_config_rec_type,in_use_for_config
  USE module_state_description, ONLY:PARAM_FIRST_SCALAR
  USE module_driver_constants
  TYPE(domain) ,               INTENT(IN) :: grid
  TYPE(grid_config_rec_type) , INTENT(IN) :: config_flags
  INTEGER ,                    INTENT(IN) :: local_communicator_periodic
  INTEGER ,                    INTENT(IN) :: mytask, ntasks, ntasks_x, ntasks_y
  INTEGER ,                    INTENT(IN) :: ids, ide, jds, jde, kds, kde
  INTEGER ,                    INTENT(IN) :: ims, ime, jms, jme, kms, kme
  INTEGER ,                    INTENT(IN) :: ips, ipe, jps, jpe, kps, kpe
  INTEGER :: itrace
  INTEGER :: rsl_sendw_p, rsl_sendbeg_p, rsl_recvw_p, rsl_recvbeg_p
  INTEGER :: rsl_sendw_m, rsl_sendbeg_m, rsl_recvw_m, rsl_recvbeg_m
  LOGICAL, EXTERNAL :: rsl_comm_iter
  INTEGER :: idim1, idim2, idim3, idim4, idim5, idim6, idim7
  






CALL wrf_debug(2,'calling inc/PERIOD_BDY_EM_C_inline.inc')
IF ( config_flags%periodic_x ) THEN
CALL RSL_LITE_INIT_PERIOD ( local_communicator_periodic, 2 , &
     6, 7, 4, &
     0, 0, 4, &
     0, 0, 8, &
      0,  0, 4, &
      mytask, ntasks, ntasks_x, ntasks_y,   &
      ips, ipe, jps, jpe, kps, kpe    )
IF ( SIZE(grid%u_2,1)*SIZE(grid%u_2,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%u_2, 2, 4, 1, 0, DATA_ORDER_XZY, 1, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%u_save,1)*SIZE(grid%u_save,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%u_save, 2, 4, 1, 0, DATA_ORDER_XZY, 1, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%v_2,1)*SIZE(grid%v_2,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%v_2, 2, 4, 1, 0, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%v_save,1)*SIZE(grid%v_save,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%v_save, 2, 4, 1, 0, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%t_2,1)*SIZE(grid%t_2,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%t_2, 2, 4, 1, 0, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%t_save,1)*SIZE(grid%t_save,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%t_save, 2, 4, 1, 0, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%muv,1)*SIZE(grid%muv,2) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%muv, 2, 4, 1, 0, DATA_ORDER_XY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
ENDIF
IF ( SIZE(grid%msfvx,1)*SIZE(grid%msfvx,2) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%msfvx, 2, 4, 1, 0, DATA_ORDER_XY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
ENDIF
IF ( SIZE(grid%msfvy,1)*SIZE(grid%msfvy,2) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%msfvy, 2, 4, 1, 0, DATA_ORDER_XY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
ENDIF
IF ( SIZE(grid%muu,1)*SIZE(grid%muu,2) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%muu, 2, 4, 1, 0, DATA_ORDER_XY, 1, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
ENDIF
IF ( SIZE(grid%msfux,1)*SIZE(grid%msfux,2) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%msfux, 2, 4, 1, 0, DATA_ORDER_XY, 1, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
ENDIF
IF ( SIZE(grid%msfuy,1)*SIZE(grid%msfuy,2) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%msfuy, 2, 4, 1, 0, DATA_ORDER_XY, 1, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
ENDIF
IF ( SIZE(grid%msfvx_inv,1)*SIZE(grid%msfvx_inv,2) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%msfvx_inv, 2, 4, 1, 0, DATA_ORDER_XY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
ENDIF
   CALL RSL_LITE_EXCH_PERIOD_X ( local_communicator_periodic , mytask, ntasks, ntasks_x, ntasks_y )
IF ( SIZE(grid%u_2,1)*SIZE(grid%u_2,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%u_2, 2, 4, 1, 1, DATA_ORDER_XZY, 1, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%u_save,1)*SIZE(grid%u_save,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%u_save, 2, 4, 1, 1, DATA_ORDER_XZY, 1, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%v_2,1)*SIZE(grid%v_2,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%v_2, 2, 4, 1, 1, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%v_save,1)*SIZE(grid%v_save,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%v_save, 2, 4, 1, 1, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%t_2,1)*SIZE(grid%t_2,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%t_2, 2, 4, 1, 1, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%t_save,1)*SIZE(grid%t_save,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%t_save, 2, 4, 1, 1, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%muv,1)*SIZE(grid%muv,2) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%muv, 2, 4, 1, 1, DATA_ORDER_XY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
ENDIF
IF ( SIZE(grid%msfvx,1)*SIZE(grid%msfvx,2) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%msfvx, 2, 4, 1, 1, DATA_ORDER_XY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
ENDIF
IF ( SIZE(grid%msfvy,1)*SIZE(grid%msfvy,2) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%msfvy, 2, 4, 1, 1, DATA_ORDER_XY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
ENDIF
IF ( SIZE(grid%muu,1)*SIZE(grid%muu,2) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%muu, 2, 4, 1, 1, DATA_ORDER_XY, 1, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
ENDIF
IF ( SIZE(grid%msfux,1)*SIZE(grid%msfux,2) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%msfux, 2, 4, 1, 1, DATA_ORDER_XY, 1, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
ENDIF
IF ( SIZE(grid%msfuy,1)*SIZE(grid%msfuy,2) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%msfuy, 2, 4, 1, 1, DATA_ORDER_XY, 1, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
ENDIF
IF ( SIZE(grid%msfvx_inv,1)*SIZE(grid%msfvx_inv,2) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%msfvx_inv, 2, 4, 1, 1, DATA_ORDER_XY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
ENDIF
END IF
IF ( config_flags%periodic_y ) THEN
CALL RSL_LITE_INIT_PERIOD ( local_communicator_periodic, 2 , &
     6, 7, 4, &
     0, 0, 4, &
     0, 0, 8, &
      0,  0, 4, &
      mytask, ntasks, ntasks_x, ntasks_y,   &
      ips, ipe, jps, jpe, kps, kpe    )
IF ( SIZE(grid%u_2,1)*SIZE(grid%u_2,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%u_2, 2, 4, 0, 0, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%u_save,1)*SIZE(grid%u_save,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%u_save, 2, 4, 0, 0, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%v_2,1)*SIZE(grid%v_2,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%v_2, 2, 4, 0, 0, DATA_ORDER_XZY, 1, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%v_save,1)*SIZE(grid%v_save,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%v_save, 2, 4, 0, 0, DATA_ORDER_XZY, 1, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%t_2,1)*SIZE(grid%t_2,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%t_2, 2, 4, 0, 0, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%t_save,1)*SIZE(grid%t_save,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%t_save, 2, 4, 0, 0, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%muv,1)*SIZE(grid%muv,2) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%muv, 2, 4, 0, 0, DATA_ORDER_XY, 1, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
ENDIF
IF ( SIZE(grid%msfvx,1)*SIZE(grid%msfvx,2) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%msfvx, 2, 4, 0, 0, DATA_ORDER_XY, 1, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
ENDIF
IF ( SIZE(grid%msfvy,1)*SIZE(grid%msfvy,2) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%msfvy, 2, 4, 0, 0, DATA_ORDER_XY, 1, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
ENDIF
IF ( SIZE(grid%muu,1)*SIZE(grid%muu,2) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%muu, 2, 4, 0, 0, DATA_ORDER_XY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
ENDIF
IF ( SIZE(grid%msfux,1)*SIZE(grid%msfux,2) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%msfux, 2, 4, 0, 0, DATA_ORDER_XY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
ENDIF
IF ( SIZE(grid%msfuy,1)*SIZE(grid%msfuy,2) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%msfuy, 2, 4, 0, 0, DATA_ORDER_XY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
ENDIF
IF ( SIZE(grid%msfvx_inv,1)*SIZE(grid%msfvx_inv,2) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%msfvx_inv, 2, 4, 0, 0, DATA_ORDER_XY, 1, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
ENDIF
   CALL RSL_LITE_EXCH_PERIOD_Y ( local_communicator_periodic , mytask, ntasks, ntasks_x, ntasks_y )
IF ( SIZE(grid%u_2,1)*SIZE(grid%u_2,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%u_2, 2, 4, 0, 1, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%u_save,1)*SIZE(grid%u_save,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%u_save, 2, 4, 0, 1, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%v_2,1)*SIZE(grid%v_2,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%v_2, 2, 4, 0, 1, DATA_ORDER_XZY, 1, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%v_save,1)*SIZE(grid%v_save,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%v_save, 2, 4, 0, 1, DATA_ORDER_XZY, 1, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%t_2,1)*SIZE(grid%t_2,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%t_2, 2, 4, 0, 1, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%t_save,1)*SIZE(grid%t_save,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%t_save, 2, 4, 0, 1, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%muv,1)*SIZE(grid%muv,2) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%muv, 2, 4, 0, 1, DATA_ORDER_XY, 1, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
ENDIF
IF ( SIZE(grid%msfvx,1)*SIZE(grid%msfvx,2) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%msfvx, 2, 4, 0, 1, DATA_ORDER_XY, 1, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
ENDIF
IF ( SIZE(grid%msfvy,1)*SIZE(grid%msfvy,2) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%msfvy, 2, 4, 0, 1, DATA_ORDER_XY, 1, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
ENDIF
IF ( SIZE(grid%muu,1)*SIZE(grid%muu,2) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%muu, 2, 4, 0, 1, DATA_ORDER_XY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
ENDIF
IF ( SIZE(grid%msfux,1)*SIZE(grid%msfux,2) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%msfux, 2, 4, 0, 1, DATA_ORDER_XY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
ENDIF
IF ( SIZE(grid%msfuy,1)*SIZE(grid%msfuy,2) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%msfuy, 2, 4, 0, 1, DATA_ORDER_XY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
ENDIF
IF ( SIZE(grid%msfvx_inv,1)*SIZE(grid%msfvx_inv,2) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%msfvx_inv, 2, 4, 0, 1, DATA_ORDER_XY, 1, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
ENDIF
END IF

  
  END SUBROUTINE PERIOD_BDY_EM_C_sub

SUBROUTINE PERIOD_BDY_EM_D_sub ( grid, &
  config_flags, &
  local_communicator_periodic, &
  mytask, ntasks, ntasks_x, ntasks_y, &
  ids, ide, jds, jde, kds, kde,       &
  ims, ime, jms, jme, kms, kme,       &
  ips, ipe, jps, jpe, kps, kpe )
  USE module_domain, ONLY:domain
  USE module_configure, ONLY:grid_config_rec_type,in_use_for_config
  USE module_state_description, ONLY:PARAM_FIRST_SCALAR
  USE module_driver_constants
  TYPE(domain) ,               INTENT(IN) :: grid
  TYPE(grid_config_rec_type) , INTENT(IN) :: config_flags
  INTEGER ,                    INTENT(IN) :: local_communicator_periodic
  INTEGER ,                    INTENT(IN) :: mytask, ntasks, ntasks_x, ntasks_y
  INTEGER ,                    INTENT(IN) :: ids, ide, jds, jde, kds, kde
  INTEGER ,                    INTENT(IN) :: ims, ime, jms, jme, kms, kme
  INTEGER ,                    INTENT(IN) :: ips, ipe, jps, jpe, kps, kpe
  INTEGER :: itrace
  INTEGER :: rsl_sendw_p, rsl_sendbeg_p, rsl_recvw_p, rsl_recvbeg_p
  INTEGER :: rsl_sendw_m, rsl_sendbeg_m, rsl_recvw_m, rsl_recvbeg_m
  LOGICAL, EXTERNAL :: rsl_comm_iter
  INTEGER :: idim1, idim2, idim3, idim4, idim5, idim6, idim7
  






CALL wrf_debug(2,'calling inc/PERIOD_BDY_EM_D_inline.inc')
IF ( config_flags%periodic_x ) THEN
CALL RSL_LITE_INIT_PERIOD ( local_communicator_periodic, 3 , &
     6, 1, 4, &
     0, 0, 4, &
     0, 0, 8, &
      0,  0, 4, &
      mytask, ntasks, ntasks_x, ntasks_y,   &
      ips, ipe, jps, jpe, kps, kpe    )
IF ( SIZE(grid%u_2,1)*SIZE(grid%u_2,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%u_2, 3, 4, 1, 0, DATA_ORDER_XZY, 1, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%v_2,1)*SIZE(grid%v_2,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%v_2, 3, 4, 1, 0, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%w_2,1)*SIZE(grid%w_2,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%w_2, 3, 4, 1, 0, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%t_2,1)*SIZE(grid%t_2,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%t_2, 3, 4, 1, 0, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%ph_2,1)*SIZE(grid%ph_2,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%ph_2, 3, 4, 1, 0, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%mu_2,1)*SIZE(grid%mu_2,2) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%mu_2, 3, 4, 1, 0, DATA_ORDER_XY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
ENDIF
IF ( SIZE(grid%tke_2,1)*SIZE(grid%tke_2,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%tke_2, 3, 4, 1, 0, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
   CALL RSL_LITE_EXCH_PERIOD_X ( local_communicator_periodic , mytask, ntasks, ntasks_x, ntasks_y )
IF ( SIZE(grid%u_2,1)*SIZE(grid%u_2,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%u_2, 3, 4, 1, 1, DATA_ORDER_XZY, 1, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%v_2,1)*SIZE(grid%v_2,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%v_2, 3, 4, 1, 1, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%w_2,1)*SIZE(grid%w_2,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%w_2, 3, 4, 1, 1, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%t_2,1)*SIZE(grid%t_2,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%t_2, 3, 4, 1, 1, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%ph_2,1)*SIZE(grid%ph_2,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%ph_2, 3, 4, 1, 1, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%mu_2,1)*SIZE(grid%mu_2,2) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%mu_2, 3, 4, 1, 1, DATA_ORDER_XY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
ENDIF
IF ( SIZE(grid%tke_2,1)*SIZE(grid%tke_2,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%tke_2, 3, 4, 1, 1, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
END IF
IF ( config_flags%periodic_y ) THEN
CALL RSL_LITE_INIT_PERIOD ( local_communicator_periodic, 3 , &
     6, 1, 4, &
     0, 0, 4, &
     0, 0, 8, &
      0,  0, 4, &
      mytask, ntasks, ntasks_x, ntasks_y,   &
      ips, ipe, jps, jpe, kps, kpe    )
IF ( SIZE(grid%u_2,1)*SIZE(grid%u_2,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%u_2, 3, 4, 0, 0, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%v_2,1)*SIZE(grid%v_2,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%v_2, 3, 4, 0, 0, DATA_ORDER_XZY, 1, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%w_2,1)*SIZE(grid%w_2,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%w_2, 3, 4, 0, 0, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%t_2,1)*SIZE(grid%t_2,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%t_2, 3, 4, 0, 0, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%ph_2,1)*SIZE(grid%ph_2,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%ph_2, 3, 4, 0, 0, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%mu_2,1)*SIZE(grid%mu_2,2) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%mu_2, 3, 4, 0, 0, DATA_ORDER_XY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
ENDIF
IF ( SIZE(grid%tke_2,1)*SIZE(grid%tke_2,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%tke_2, 3, 4, 0, 0, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
   CALL RSL_LITE_EXCH_PERIOD_Y ( local_communicator_periodic , mytask, ntasks, ntasks_x, ntasks_y )
IF ( SIZE(grid%u_2,1)*SIZE(grid%u_2,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%u_2, 3, 4, 0, 1, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%v_2,1)*SIZE(grid%v_2,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%v_2, 3, 4, 0, 1, DATA_ORDER_XZY, 1, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%w_2,1)*SIZE(grid%w_2,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%w_2, 3, 4, 0, 1, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%t_2,1)*SIZE(grid%t_2,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%t_2, 3, 4, 0, 1, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%ph_2,1)*SIZE(grid%ph_2,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%ph_2, 3, 4, 0, 1, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%mu_2,1)*SIZE(grid%mu_2,2) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%mu_2, 3, 4, 0, 1, DATA_ORDER_XY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
ENDIF
IF ( SIZE(grid%tke_2,1)*SIZE(grid%tke_2,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%tke_2, 3, 4, 0, 1, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
END IF

  
  END SUBROUTINE PERIOD_BDY_EM_D_sub

SUBROUTINE PERIOD_BDY_EM_D3_sub ( grid, &
  config_flags, &
  local_communicator_periodic, &
  mytask, ntasks, ntasks_x, ntasks_y, &
  ids, ide, jds, jde, kds, kde,       &
  ims, ime, jms, jme, kms, kme,       &
  ips, ipe, jps, jpe, kps, kpe )
  USE module_domain, ONLY:domain
  USE module_configure, ONLY:grid_config_rec_type,in_use_for_config
  USE module_state_description, ONLY:PARAM_FIRST_SCALAR
  USE module_driver_constants
  TYPE(domain) ,               INTENT(IN) :: grid
  TYPE(grid_config_rec_type) , INTENT(IN) :: config_flags
  INTEGER ,                    INTENT(IN) :: local_communicator_periodic
  INTEGER ,                    INTENT(IN) :: mytask, ntasks, ntasks_x, ntasks_y
  INTEGER ,                    INTENT(IN) :: ids, ide, jds, jde, kds, kde
  INTEGER ,                    INTENT(IN) :: ims, ime, jms, jme, kms, kme
  INTEGER ,                    INTENT(IN) :: ips, ipe, jps, jpe, kps, kpe
  INTEGER :: itrace
  INTEGER :: rsl_sendw_p, rsl_sendbeg_p, rsl_recvw_p, rsl_recvbeg_p
  INTEGER :: rsl_sendw_m, rsl_sendbeg_m, rsl_recvw_m, rsl_recvbeg_m
  LOGICAL, EXTERNAL :: rsl_comm_iter
  INTEGER :: idim1, idim2, idim3, idim4, idim5, idim6, idim7
  






CALL wrf_debug(2,'calling inc/PERIOD_BDY_EM_D3_inline.inc')
IF ( config_flags%periodic_x ) THEN
CALL RSL_LITE_INIT_PERIOD ( local_communicator_periodic, 3 , &
     12, 2, 4, &
     0, 0, 4, &
     0, 0, 8, &
      0,  0, 4, &
      mytask, ntasks, ntasks_x, ntasks_y,   &
      ips, ipe, jps, jpe, kps, kpe    )
IF ( SIZE(grid%u_1,1)*SIZE(grid%u_1,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%u_1, 3, 4, 1, 0, DATA_ORDER_XZY, 1, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%u_2,1)*SIZE(grid%u_2,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%u_2, 3, 4, 1, 0, DATA_ORDER_XZY, 1, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%v_1,1)*SIZE(grid%v_1,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%v_1, 3, 4, 1, 0, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%v_2,1)*SIZE(grid%v_2,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%v_2, 3, 4, 1, 0, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%w_1,1)*SIZE(grid%w_1,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%w_1, 3, 4, 1, 0, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%w_2,1)*SIZE(grid%w_2,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%w_2, 3, 4, 1, 0, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%t_1,1)*SIZE(grid%t_1,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%t_1, 3, 4, 1, 0, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%t_2,1)*SIZE(grid%t_2,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%t_2, 3, 4, 1, 0, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%ph_1,1)*SIZE(grid%ph_1,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%ph_1, 3, 4, 1, 0, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%ph_2,1)*SIZE(grid%ph_2,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%ph_2, 3, 4, 1, 0, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%tke_1,1)*SIZE(grid%tke_1,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%tke_1, 3, 4, 1, 0, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%tke_2,1)*SIZE(grid%tke_2,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%tke_2, 3, 4, 1, 0, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%mu_1,1)*SIZE(grid%mu_1,2) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%mu_1, 3, 4, 1, 0, DATA_ORDER_XY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
ENDIF
IF ( SIZE(grid%mu_2,1)*SIZE(grid%mu_2,2) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%mu_2, 3, 4, 1, 0, DATA_ORDER_XY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
ENDIF
   CALL RSL_LITE_EXCH_PERIOD_X ( local_communicator_periodic , mytask, ntasks, ntasks_x, ntasks_y )
IF ( SIZE(grid%u_1,1)*SIZE(grid%u_1,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%u_1, 3, 4, 1, 1, DATA_ORDER_XZY, 1, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%u_2,1)*SIZE(grid%u_2,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%u_2, 3, 4, 1, 1, DATA_ORDER_XZY, 1, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%v_1,1)*SIZE(grid%v_1,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%v_1, 3, 4, 1, 1, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%v_2,1)*SIZE(grid%v_2,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%v_2, 3, 4, 1, 1, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%w_1,1)*SIZE(grid%w_1,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%w_1, 3, 4, 1, 1, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%w_2,1)*SIZE(grid%w_2,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%w_2, 3, 4, 1, 1, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%t_1,1)*SIZE(grid%t_1,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%t_1, 3, 4, 1, 1, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%t_2,1)*SIZE(grid%t_2,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%t_2, 3, 4, 1, 1, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%ph_1,1)*SIZE(grid%ph_1,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%ph_1, 3, 4, 1, 1, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%ph_2,1)*SIZE(grid%ph_2,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%ph_2, 3, 4, 1, 1, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%tke_1,1)*SIZE(grid%tke_1,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%tke_1, 3, 4, 1, 1, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%tke_2,1)*SIZE(grid%tke_2,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%tke_2, 3, 4, 1, 1, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%mu_1,1)*SIZE(grid%mu_1,2) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%mu_1, 3, 4, 1, 1, DATA_ORDER_XY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
ENDIF
IF ( SIZE(grid%mu_2,1)*SIZE(grid%mu_2,2) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%mu_2, 3, 4, 1, 1, DATA_ORDER_XY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
ENDIF
END IF
IF ( config_flags%periodic_y ) THEN
CALL RSL_LITE_INIT_PERIOD ( local_communicator_periodic, 3 , &
     12, 2, 4, &
     0, 0, 4, &
     0, 0, 8, &
      0,  0, 4, &
      mytask, ntasks, ntasks_x, ntasks_y,   &
      ips, ipe, jps, jpe, kps, kpe    )
IF ( SIZE(grid%u_1,1)*SIZE(grid%u_1,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%u_1, 3, 4, 0, 0, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%u_2,1)*SIZE(grid%u_2,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%u_2, 3, 4, 0, 0, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%v_1,1)*SIZE(grid%v_1,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%v_1, 3, 4, 0, 0, DATA_ORDER_XZY, 1, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%v_2,1)*SIZE(grid%v_2,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%v_2, 3, 4, 0, 0, DATA_ORDER_XZY, 1, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%w_1,1)*SIZE(grid%w_1,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%w_1, 3, 4, 0, 0, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%w_2,1)*SIZE(grid%w_2,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%w_2, 3, 4, 0, 0, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%t_1,1)*SIZE(grid%t_1,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%t_1, 3, 4, 0, 0, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%t_2,1)*SIZE(grid%t_2,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%t_2, 3, 4, 0, 0, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%ph_1,1)*SIZE(grid%ph_1,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%ph_1, 3, 4, 0, 0, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%ph_2,1)*SIZE(grid%ph_2,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%ph_2, 3, 4, 0, 0, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%tke_1,1)*SIZE(grid%tke_1,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%tke_1, 3, 4, 0, 0, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%tke_2,1)*SIZE(grid%tke_2,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%tke_2, 3, 4, 0, 0, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%mu_1,1)*SIZE(grid%mu_1,2) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%mu_1, 3, 4, 0, 0, DATA_ORDER_XY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
ENDIF
IF ( SIZE(grid%mu_2,1)*SIZE(grid%mu_2,2) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%mu_2, 3, 4, 0, 0, DATA_ORDER_XY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
ENDIF
   CALL RSL_LITE_EXCH_PERIOD_Y ( local_communicator_periodic , mytask, ntasks, ntasks_x, ntasks_y )
IF ( SIZE(grid%u_1,1)*SIZE(grid%u_1,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%u_1, 3, 4, 0, 1, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%u_2,1)*SIZE(grid%u_2,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%u_2, 3, 4, 0, 1, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%v_1,1)*SIZE(grid%v_1,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%v_1, 3, 4, 0, 1, DATA_ORDER_XZY, 1, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%v_2,1)*SIZE(grid%v_2,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%v_2, 3, 4, 0, 1, DATA_ORDER_XZY, 1, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%w_1,1)*SIZE(grid%w_1,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%w_1, 3, 4, 0, 1, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%w_2,1)*SIZE(grid%w_2,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%w_2, 3, 4, 0, 1, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%t_1,1)*SIZE(grid%t_1,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%t_1, 3, 4, 0, 1, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%t_2,1)*SIZE(grid%t_2,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%t_2, 3, 4, 0, 1, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%ph_1,1)*SIZE(grid%ph_1,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%ph_1, 3, 4, 0, 1, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%ph_2,1)*SIZE(grid%ph_2,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%ph_2, 3, 4, 0, 1, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%tke_1,1)*SIZE(grid%tke_1,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%tke_1, 3, 4, 0, 1, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%tke_2,1)*SIZE(grid%tke_2,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%tke_2, 3, 4, 0, 1, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%mu_1,1)*SIZE(grid%mu_1,2) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%mu_1, 3, 4, 0, 1, DATA_ORDER_XY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
ENDIF
IF ( SIZE(grid%mu_2,1)*SIZE(grid%mu_2,2) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%mu_2, 3, 4, 0, 1, DATA_ORDER_XY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
ENDIF
END IF

  
  END SUBROUTINE PERIOD_BDY_EM_D3_sub

SUBROUTINE PERIOD_EM_DA_sub ( grid, &
  config_flags, &
  local_communicator_periodic, &
  mytask, ntasks, ntasks_x, ntasks_y, &
  ids, ide, jds, jde, kds, kde,       &
  ims, ime, jms, jme, kms, kme,       &
  ips, ipe, jps, jpe, kps, kpe )
  USE module_domain, ONLY:domain
  USE module_configure, ONLY:grid_config_rec_type,in_use_for_config
  USE module_state_description, ONLY:PARAM_FIRST_SCALAR
  USE module_driver_constants
  TYPE(domain) ,               INTENT(IN) :: grid
  TYPE(grid_config_rec_type) , INTENT(IN) :: config_flags
  INTEGER ,                    INTENT(IN) :: local_communicator_periodic
  INTEGER ,                    INTENT(IN) :: mytask, ntasks, ntasks_x, ntasks_y
  INTEGER ,                    INTENT(IN) :: ids, ide, jds, jde, kds, kde
  INTEGER ,                    INTENT(IN) :: ims, ime, jms, jme, kms, kme
  INTEGER ,                    INTENT(IN) :: ips, ipe, jps, jpe, kps, kpe
  INTEGER :: itrace
  INTEGER :: rsl_sendw_p, rsl_sendbeg_p, rsl_recvw_p, rsl_recvbeg_p
  INTEGER :: rsl_sendw_m, rsl_sendbeg_m, rsl_recvw_m, rsl_recvbeg_m
  LOGICAL, EXTERNAL :: rsl_comm_iter
  INTEGER :: idim1, idim2, idim3, idim4, idim5, idim6, idim7
  






CALL wrf_debug(2,'calling inc/PERIOD_EM_DA_inline.inc')
IF ( config_flags%periodic_x ) THEN
CALL RSL_LITE_INIT_PERIOD ( local_communicator_periodic, 2 , &
     3, 2, 4, &
     0, 0, 4, &
     0, 0, 8, &
      0,  0, 4, &
      mytask, ntasks, ntasks_x, ntasks_y,   &
      ips, ipe, jps, jpe, kps, kpe    )
IF ( SIZE(grid%ru_m,1)*SIZE(grid%ru_m,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%ru_m, 2, 4, 1, 0, DATA_ORDER_XZY, 1, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%rv_m,1)*SIZE(grid%rv_m,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%rv_m, 2, 4, 1, 0, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%ww_m,1)*SIZE(grid%ww_m,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%ww_m, 2, 4, 1, 0, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%mut,1)*SIZE(grid%mut,2) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%mut, 2, 4, 1, 0, DATA_ORDER_XY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
ENDIF
IF ( SIZE(grid%muts,1)*SIZE(grid%muts,2) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%muts, 2, 4, 1, 0, DATA_ORDER_XY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
ENDIF
   CALL RSL_LITE_EXCH_PERIOD_X ( local_communicator_periodic , mytask, ntasks, ntasks_x, ntasks_y )
IF ( SIZE(grid%ru_m,1)*SIZE(grid%ru_m,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%ru_m, 2, 4, 1, 1, DATA_ORDER_XZY, 1, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%rv_m,1)*SIZE(grid%rv_m,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%rv_m, 2, 4, 1, 1, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%ww_m,1)*SIZE(grid%ww_m,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%ww_m, 2, 4, 1, 1, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%mut,1)*SIZE(grid%mut,2) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%mut, 2, 4, 1, 1, DATA_ORDER_XY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
ENDIF
IF ( SIZE(grid%muts,1)*SIZE(grid%muts,2) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%muts, 2, 4, 1, 1, DATA_ORDER_XY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
ENDIF
END IF
IF ( config_flags%periodic_y ) THEN
CALL RSL_LITE_INIT_PERIOD ( local_communicator_periodic, 2 , &
     3, 2, 4, &
     0, 0, 4, &
     0, 0, 8, &
      0,  0, 4, &
      mytask, ntasks, ntasks_x, ntasks_y,   &
      ips, ipe, jps, jpe, kps, kpe    )
IF ( SIZE(grid%ru_m,1)*SIZE(grid%ru_m,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%ru_m, 2, 4, 0, 0, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%rv_m,1)*SIZE(grid%rv_m,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%rv_m, 2, 4, 0, 0, DATA_ORDER_XZY, 1, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%ww_m,1)*SIZE(grid%ww_m,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%ww_m, 2, 4, 0, 0, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%mut,1)*SIZE(grid%mut,2) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%mut, 2, 4, 0, 0, DATA_ORDER_XY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
ENDIF
IF ( SIZE(grid%muts,1)*SIZE(grid%muts,2) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%muts, 2, 4, 0, 0, DATA_ORDER_XY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
ENDIF
   CALL RSL_LITE_EXCH_PERIOD_Y ( local_communicator_periodic , mytask, ntasks, ntasks_x, ntasks_y )
IF ( SIZE(grid%ru_m,1)*SIZE(grid%ru_m,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%ru_m, 2, 4, 0, 1, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%rv_m,1)*SIZE(grid%rv_m,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%rv_m, 2, 4, 0, 1, DATA_ORDER_XZY, 1, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%ww_m,1)*SIZE(grid%ww_m,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%ww_m, 2, 4, 0, 1, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%mut,1)*SIZE(grid%mut,2) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%mut, 2, 4, 0, 1, DATA_ORDER_XY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
ENDIF
IF ( SIZE(grid%muts,1)*SIZE(grid%muts,2) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%muts, 2, 4, 0, 1, DATA_ORDER_XY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
ENDIF
END IF

  
  END SUBROUTINE PERIOD_EM_DA_sub

SUBROUTINE PERIOD_EM_NBA_MIJ_sub ( grid, &
  config_flags, &
  num_nba_mij, &
  nba_mij, &
  local_communicator_periodic, &
  mytask, ntasks, ntasks_x, ntasks_y, &
  ids, ide, jds, jde, kds, kde,       &
  ims, ime, jms, jme, kms, kme,       &
  ips, ipe, jps, jpe, kps, kpe )
  USE module_domain, ONLY:domain
  USE module_configure, ONLY:grid_config_rec_type,in_use_for_config
  USE module_state_description, ONLY:PARAM_FIRST_SCALAR
  USE module_driver_constants
  TYPE(domain) ,               INTENT(IN) :: grid
  TYPE(grid_config_rec_type) , INTENT(IN) :: config_flags
  INTEGER, INTENT(IN) :: num_nba_mij
  real, INTENT(INOUT) :: nba_mij ( grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33,num_nba_mij)
  INTEGER ,                    INTENT(IN) :: local_communicator_periodic
  INTEGER ,                    INTENT(IN) :: mytask, ntasks, ntasks_x, ntasks_y
  INTEGER ,                    INTENT(IN) :: ids, ide, jds, jde, kds, kde
  INTEGER ,                    INTENT(IN) :: ims, ime, jms, jme, kms, kme
  INTEGER ,                    INTENT(IN) :: ips, ipe, jps, jpe, kps, kpe
  INTEGER :: itrace
  INTEGER :: rsl_sendw_p, rsl_sendbeg_p, rsl_recvw_p, rsl_recvbeg_p
  INTEGER :: rsl_sendw_m, rsl_sendbeg_m, rsl_recvw_m, rsl_recvbeg_m
  LOGICAL, EXTERNAL :: rsl_comm_iter
  INTEGER :: idim1, idim2, idim3, idim4, idim5, idim6, idim7
  






CALL wrf_debug(2,'calling inc/PERIOD_EM_NBA_MIJ_inline.inc')
IF ( config_flags%periodic_x ) THEN
CALL RSL_LITE_INIT_PERIOD ( local_communicator_periodic, 3 , &
     0  &
   + num_nba_mij   &
     , 0, 4, &
     0, 0, 4, &
     0, 0, 8, &
      0,  0, 4, &
      mytask, ntasks, ntasks_x, ntasks_y,   &
      ips, ipe, jps, jpe, kps, kpe    )
DO itrace = PARAM_FIRST_SCALAR, num_nba_mij
 IF ( SIZE(nba_mij,1)*SIZE(nba_mij,3) .GT. 1 ) THEN
  CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
nba_mij ( grid%sm31,grid%sm32,grid%sm33,itrace), 3, 4, 1, 0, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
 ENDIF
ENDDO
   CALL RSL_LITE_EXCH_PERIOD_X ( local_communicator_periodic , mytask, ntasks, ntasks_x, ntasks_y )
DO itrace = PARAM_FIRST_SCALAR, num_nba_mij
 IF ( SIZE(nba_mij,1)*SIZE(nba_mij,3) .GT. 1 ) THEN
  CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
nba_mij ( grid%sm31,grid%sm32,grid%sm33,itrace), 3, 4, 1, 1, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
 ENDIF
ENDDO
END IF
IF ( config_flags%periodic_y ) THEN
CALL RSL_LITE_INIT_PERIOD ( local_communicator_periodic, 3 , &
     0  &
   + num_nba_mij   &
     , 0, 4, &
     0, 0, 4, &
     0, 0, 8, &
      0,  0, 4, &
      mytask, ntasks, ntasks_x, ntasks_y,   &
      ips, ipe, jps, jpe, kps, kpe    )
DO itrace = PARAM_FIRST_SCALAR, num_nba_mij
 IF ( SIZE(nba_mij,1)*SIZE(nba_mij,3) .GT. 1 ) THEN
  CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
nba_mij ( grid%sm31,grid%sm32,grid%sm33,itrace), 3, 4, 0, 0, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
 ENDIF
ENDDO
   CALL RSL_LITE_EXCH_PERIOD_Y ( local_communicator_periodic , mytask, ntasks, ntasks_x, ntasks_y )
DO itrace = PARAM_FIRST_SCALAR, num_nba_mij
 IF ( SIZE(nba_mij,1)*SIZE(nba_mij,3) .GT. 1 ) THEN
  CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
nba_mij ( grid%sm31,grid%sm32,grid%sm33,itrace), 3, 4, 0, 1, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
 ENDIF
ENDDO
END IF

  
  END SUBROUTINE PERIOD_EM_NBA_MIJ_sub

SUBROUTINE PERIOD_EM_NBA_RIJ_sub ( grid, &
  config_flags, &
  num_nba_rij, &
  nba_rij, &
  local_communicator_periodic, &
  mytask, ntasks, ntasks_x, ntasks_y, &
  ids, ide, jds, jde, kds, kde,       &
  ims, ime, jms, jme, kms, kme,       &
  ips, ipe, jps, jpe, kps, kpe )
  USE module_domain, ONLY:domain
  USE module_configure, ONLY:grid_config_rec_type,in_use_for_config
  USE module_state_description, ONLY:PARAM_FIRST_SCALAR
  USE module_driver_constants
  TYPE(domain) ,               INTENT(IN) :: grid
  TYPE(grid_config_rec_type) , INTENT(IN) :: config_flags
  INTEGER, INTENT(IN) :: num_nba_rij
  real, INTENT(INOUT) :: nba_rij ( grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33,num_nba_rij)
  INTEGER ,                    INTENT(IN) :: local_communicator_periodic
  INTEGER ,                    INTENT(IN) :: mytask, ntasks, ntasks_x, ntasks_y
  INTEGER ,                    INTENT(IN) :: ids, ide, jds, jde, kds, kde
  INTEGER ,                    INTENT(IN) :: ims, ime, jms, jme, kms, kme
  INTEGER ,                    INTENT(IN) :: ips, ipe, jps, jpe, kps, kpe
  INTEGER :: itrace
  INTEGER :: rsl_sendw_p, rsl_sendbeg_p, rsl_recvw_p, rsl_recvbeg_p
  INTEGER :: rsl_sendw_m, rsl_sendbeg_m, rsl_recvw_m, rsl_recvbeg_m
  LOGICAL, EXTERNAL :: rsl_comm_iter
  INTEGER :: idim1, idim2, idim3, idim4, idim5, idim6, idim7
  






CALL wrf_debug(2,'calling inc/PERIOD_EM_NBA_RIJ_inline.inc')
IF ( config_flags%periodic_x ) THEN
CALL RSL_LITE_INIT_PERIOD ( local_communicator_periodic, 3 , &
     0  &
   + num_nba_rij   &
     , 0, 4, &
     0, 0, 4, &
     0, 0, 8, &
      0,  0, 4, &
      mytask, ntasks, ntasks_x, ntasks_y,   &
      ips, ipe, jps, jpe, kps, kpe    )
DO itrace = PARAM_FIRST_SCALAR, num_nba_rij
 IF ( SIZE(nba_rij,1)*SIZE(nba_rij,3) .GT. 1 ) THEN
  CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
nba_rij ( grid%sm31,grid%sm32,grid%sm33,itrace), 3, 4, 1, 0, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
 ENDIF
ENDDO
   CALL RSL_LITE_EXCH_PERIOD_X ( local_communicator_periodic , mytask, ntasks, ntasks_x, ntasks_y )
DO itrace = PARAM_FIRST_SCALAR, num_nba_rij
 IF ( SIZE(nba_rij,1)*SIZE(nba_rij,3) .GT. 1 ) THEN
  CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
nba_rij ( grid%sm31,grid%sm32,grid%sm33,itrace), 3, 4, 1, 1, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
 ENDIF
ENDDO
END IF
IF ( config_flags%periodic_y ) THEN
CALL RSL_LITE_INIT_PERIOD ( local_communicator_periodic, 3 , &
     0  &
   + num_nba_rij   &
     , 0, 4, &
     0, 0, 4, &
     0, 0, 8, &
      0,  0, 4, &
      mytask, ntasks, ntasks_x, ntasks_y,   &
      ips, ipe, jps, jpe, kps, kpe    )
DO itrace = PARAM_FIRST_SCALAR, num_nba_rij
 IF ( SIZE(nba_rij,1)*SIZE(nba_rij,3) .GT. 1 ) THEN
  CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
nba_rij ( grid%sm31,grid%sm32,grid%sm33,itrace), 3, 4, 0, 0, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
 ENDIF
ENDDO
   CALL RSL_LITE_EXCH_PERIOD_Y ( local_communicator_periodic , mytask, ntasks, ntasks_x, ntasks_y )
DO itrace = PARAM_FIRST_SCALAR, num_nba_rij
 IF ( SIZE(nba_rij,1)*SIZE(nba_rij,3) .GT. 1 ) THEN
  CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
nba_rij ( grid%sm31,grid%sm32,grid%sm33,itrace), 3, 4, 0, 1, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
 ENDIF
ENDDO
END IF

  
  END SUBROUTINE PERIOD_EM_NBA_RIJ_sub


END MODULE module_comm_dm_4_

