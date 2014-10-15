!STARTOFREGISTRYGENERATEDINCLUDE 'inc/PERIOD_BDY_EM_C_inline.inc'
!
! WARNING This file is generated automatically by use_registry
! using the data base in the file named Registry.
! Do not edit.  Your changes to this file will be lost.
!
CALL wrf_debug(2,'calling inc/PERIOD_BDY_EM_C_inline.inc')
IF ( config_flags%periodic_x ) THEN
CALL RSL_LITE_INIT_PERIOD ( local_communicator_periodic, 2 , &
     6, 7, RWORDSIZE, &
     0, 0, IWORDSIZE, &
     0, 0, DWORDSIZE, &
      0,  0, LWORDSIZE, &
      mytask, ntasks, ntasks_x, ntasks_y,   &
      ips, ipe, jps, jpe, kps, kpe    )
IF ( SIZE(grid%u_2,1)*SIZE(grid%u_2,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%u_2, 2, RWORDSIZE, 1, 0, DATA_ORDER_XZY, 1, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%u_save,1)*SIZE(grid%u_save,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%u_save, 2, RWORDSIZE, 1, 0, DATA_ORDER_XZY, 1, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%v_2,1)*SIZE(grid%v_2,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%v_2, 2, RWORDSIZE, 1, 0, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%v_save,1)*SIZE(grid%v_save,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%v_save, 2, RWORDSIZE, 1, 0, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%t_2,1)*SIZE(grid%t_2,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%t_2, 2, RWORDSIZE, 1, 0, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%t_save,1)*SIZE(grid%t_save,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%t_save, 2, RWORDSIZE, 1, 0, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%muv,1)*SIZE(grid%muv,2) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%muv, 2, RWORDSIZE, 1, 0, DATA_ORDER_XY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
ENDIF
IF ( SIZE(grid%msfvx,1)*SIZE(grid%msfvx,2) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%msfvx, 2, RWORDSIZE, 1, 0, DATA_ORDER_XY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
ENDIF
IF ( SIZE(grid%msfvy,1)*SIZE(grid%msfvy,2) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%msfvy, 2, RWORDSIZE, 1, 0, DATA_ORDER_XY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
ENDIF
IF ( SIZE(grid%muu,1)*SIZE(grid%muu,2) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%muu, 2, RWORDSIZE, 1, 0, DATA_ORDER_XY, 1, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
ENDIF
IF ( SIZE(grid%msfux,1)*SIZE(grid%msfux,2) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%msfux, 2, RWORDSIZE, 1, 0, DATA_ORDER_XY, 1, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
ENDIF
IF ( SIZE(grid%msfuy,1)*SIZE(grid%msfuy,2) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%msfuy, 2, RWORDSIZE, 1, 0, DATA_ORDER_XY, 1, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
ENDIF
IF ( SIZE(grid%msfvx_inv,1)*SIZE(grid%msfvx_inv,2) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%msfvx_inv, 2, RWORDSIZE, 1, 0, DATA_ORDER_XY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
ENDIF
   CALL RSL_LITE_EXCH_PERIOD_X ( local_communicator_periodic , mytask, ntasks, ntasks_x, ntasks_y )
IF ( SIZE(grid%u_2,1)*SIZE(grid%u_2,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%u_2, 2, RWORDSIZE, 1, 1, DATA_ORDER_XZY, 1, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%u_save,1)*SIZE(grid%u_save,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%u_save, 2, RWORDSIZE, 1, 1, DATA_ORDER_XZY, 1, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%v_2,1)*SIZE(grid%v_2,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%v_2, 2, RWORDSIZE, 1, 1, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%v_save,1)*SIZE(grid%v_save,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%v_save, 2, RWORDSIZE, 1, 1, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%t_2,1)*SIZE(grid%t_2,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%t_2, 2, RWORDSIZE, 1, 1, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%t_save,1)*SIZE(grid%t_save,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%t_save, 2, RWORDSIZE, 1, 1, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%muv,1)*SIZE(grid%muv,2) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%muv, 2, RWORDSIZE, 1, 1, DATA_ORDER_XY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
ENDIF
IF ( SIZE(grid%msfvx,1)*SIZE(grid%msfvx,2) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%msfvx, 2, RWORDSIZE, 1, 1, DATA_ORDER_XY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
ENDIF
IF ( SIZE(grid%msfvy,1)*SIZE(grid%msfvy,2) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%msfvy, 2, RWORDSIZE, 1, 1, DATA_ORDER_XY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
ENDIF
IF ( SIZE(grid%muu,1)*SIZE(grid%muu,2) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%muu, 2, RWORDSIZE, 1, 1, DATA_ORDER_XY, 1, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
ENDIF
IF ( SIZE(grid%msfux,1)*SIZE(grid%msfux,2) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%msfux, 2, RWORDSIZE, 1, 1, DATA_ORDER_XY, 1, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
ENDIF
IF ( SIZE(grid%msfuy,1)*SIZE(grid%msfuy,2) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%msfuy, 2, RWORDSIZE, 1, 1, DATA_ORDER_XY, 1, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
ENDIF
IF ( SIZE(grid%msfvx_inv,1)*SIZE(grid%msfvx_inv,2) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%msfvx_inv, 2, RWORDSIZE, 1, 1, DATA_ORDER_XY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
ENDIF
END IF
IF ( config_flags%periodic_y ) THEN
CALL RSL_LITE_INIT_PERIOD ( local_communicator_periodic, 2 , &
     6, 7, RWORDSIZE, &
     0, 0, IWORDSIZE, &
     0, 0, DWORDSIZE, &
      0,  0, LWORDSIZE, &
      mytask, ntasks, ntasks_x, ntasks_y,   &
      ips, ipe, jps, jpe, kps, kpe    )
IF ( SIZE(grid%u_2,1)*SIZE(grid%u_2,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%u_2, 2, RWORDSIZE, 0, 0, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%u_save,1)*SIZE(grid%u_save,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%u_save, 2, RWORDSIZE, 0, 0, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%v_2,1)*SIZE(grid%v_2,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%v_2, 2, RWORDSIZE, 0, 0, DATA_ORDER_XZY, 1, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%v_save,1)*SIZE(grid%v_save,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%v_save, 2, RWORDSIZE, 0, 0, DATA_ORDER_XZY, 1, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%t_2,1)*SIZE(grid%t_2,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%t_2, 2, RWORDSIZE, 0, 0, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%t_save,1)*SIZE(grid%t_save,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%t_save, 2, RWORDSIZE, 0, 0, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%muv,1)*SIZE(grid%muv,2) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%muv, 2, RWORDSIZE, 0, 0, DATA_ORDER_XY, 1, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
ENDIF
IF ( SIZE(grid%msfvx,1)*SIZE(grid%msfvx,2) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%msfvx, 2, RWORDSIZE, 0, 0, DATA_ORDER_XY, 1, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
ENDIF
IF ( SIZE(grid%msfvy,1)*SIZE(grid%msfvy,2) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%msfvy, 2, RWORDSIZE, 0, 0, DATA_ORDER_XY, 1, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
ENDIF
IF ( SIZE(grid%muu,1)*SIZE(grid%muu,2) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%muu, 2, RWORDSIZE, 0, 0, DATA_ORDER_XY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
ENDIF
IF ( SIZE(grid%msfux,1)*SIZE(grid%msfux,2) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%msfux, 2, RWORDSIZE, 0, 0, DATA_ORDER_XY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
ENDIF
IF ( SIZE(grid%msfuy,1)*SIZE(grid%msfuy,2) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%msfuy, 2, RWORDSIZE, 0, 0, DATA_ORDER_XY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
ENDIF
IF ( SIZE(grid%msfvx_inv,1)*SIZE(grid%msfvx_inv,2) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%msfvx_inv, 2, RWORDSIZE, 0, 0, DATA_ORDER_XY, 1, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
ENDIF
   CALL RSL_LITE_EXCH_PERIOD_Y ( local_communicator_periodic , mytask, ntasks, ntasks_x, ntasks_y )
IF ( SIZE(grid%u_2,1)*SIZE(grid%u_2,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%u_2, 2, RWORDSIZE, 0, 1, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%u_save,1)*SIZE(grid%u_save,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%u_save, 2, RWORDSIZE, 0, 1, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%v_2,1)*SIZE(grid%v_2,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%v_2, 2, RWORDSIZE, 0, 1, DATA_ORDER_XZY, 1, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%v_save,1)*SIZE(grid%v_save,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%v_save, 2, RWORDSIZE, 0, 1, DATA_ORDER_XZY, 1, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%t_2,1)*SIZE(grid%t_2,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%t_2, 2, RWORDSIZE, 0, 1, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%t_save,1)*SIZE(grid%t_save,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%t_save, 2, RWORDSIZE, 0, 1, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%muv,1)*SIZE(grid%muv,2) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%muv, 2, RWORDSIZE, 0, 1, DATA_ORDER_XY, 1, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
ENDIF
IF ( SIZE(grid%msfvx,1)*SIZE(grid%msfvx,2) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%msfvx, 2, RWORDSIZE, 0, 1, DATA_ORDER_XY, 1, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
ENDIF
IF ( SIZE(grid%msfvy,1)*SIZE(grid%msfvy,2) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%msfvy, 2, RWORDSIZE, 0, 1, DATA_ORDER_XY, 1, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
ENDIF
IF ( SIZE(grid%muu,1)*SIZE(grid%muu,2) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%muu, 2, RWORDSIZE, 0, 1, DATA_ORDER_XY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
ENDIF
IF ( SIZE(grid%msfux,1)*SIZE(grid%msfux,2) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%msfux, 2, RWORDSIZE, 0, 1, DATA_ORDER_XY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
ENDIF
IF ( SIZE(grid%msfuy,1)*SIZE(grid%msfuy,2) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%msfuy, 2, RWORDSIZE, 0, 1, DATA_ORDER_XY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
ENDIF
IF ( SIZE(grid%msfvx_inv,1)*SIZE(grid%msfvx_inv,2) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%msfvx_inv, 2, RWORDSIZE, 0, 1, DATA_ORDER_XY, 1, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
ENDIF
END IF
!ENDOFREGISTRYGENERATEDINCLUDE
