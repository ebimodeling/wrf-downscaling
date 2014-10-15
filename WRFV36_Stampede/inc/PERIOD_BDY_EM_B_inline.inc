!STARTOFREGISTRYGENERATEDINCLUDE 'inc/PERIOD_BDY_EM_B_inline.inc'
!
! WARNING This file is generated automatically by use_registry
! using the data base in the file named Registry.
! Do not edit.  Your changes to this file will be lost.
!
CALL wrf_debug(2,'calling inc/PERIOD_BDY_EM_B_inline.inc')
IF ( config_flags%periodic_x ) THEN
CALL RSL_LITE_INIT_PERIOD ( local_communicator_periodic, 2 , &
     12, 3, RWORDSIZE, &
     0, 0, IWORDSIZE, &
     0, 0, DWORDSIZE, &
      0,  0, LWORDSIZE, &
      mytask, ntasks, ntasks_x, ntasks_y,   &
      ips, ipe, jps, jpe, kps, kpe    )
IF ( SIZE(grid%ru_tend,1)*SIZE(grid%ru_tend,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%ru_tend, 2, RWORDSIZE, 1, 0, DATA_ORDER_XZY, 1, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%rv_tend,1)*SIZE(grid%rv_tend,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%rv_tend, 2, RWORDSIZE, 1, 0, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%ph_2,1)*SIZE(grid%ph_2,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%ph_2, 2, RWORDSIZE, 1, 0, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%al,1)*SIZE(grid%al,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%al, 2, RWORDSIZE, 1, 0, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%p,1)*SIZE(grid%p,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%p, 2, RWORDSIZE, 1, 0, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%t_1,1)*SIZE(grid%t_1,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%t_1, 2, RWORDSIZE, 1, 0, DATA_ORDER_XZY, 0, &
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
IF ( SIZE(grid%u_save,1)*SIZE(grid%u_save,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%u_save, 2, RWORDSIZE, 1, 0, DATA_ORDER_XZY, 1, &
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
IF ( SIZE(grid%mu_1,1)*SIZE(grid%mu_1,2) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%mu_1, 2, RWORDSIZE, 1, 0, DATA_ORDER_XY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
ENDIF
IF ( SIZE(grid%mu_2,1)*SIZE(grid%mu_2,2) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%mu_2, 2, RWORDSIZE, 1, 0, DATA_ORDER_XY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
ENDIF
IF ( SIZE(grid%mudf,1)*SIZE(grid%mudf,2) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%mudf, 2, RWORDSIZE, 1, 0, DATA_ORDER_XY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
ENDIF
IF ( SIZE(grid%php,1)*SIZE(grid%php,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%php, 2, RWORDSIZE, 1, 0, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%alt,1)*SIZE(grid%alt,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%alt, 2, RWORDSIZE, 1, 0, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%pb,1)*SIZE(grid%pb,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%pb, 2, RWORDSIZE, 1, 0, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
   CALL RSL_LITE_EXCH_PERIOD_X ( local_communicator_periodic , mytask, ntasks, ntasks_x, ntasks_y )
IF ( SIZE(grid%ru_tend,1)*SIZE(grid%ru_tend,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%ru_tend, 2, RWORDSIZE, 1, 1, DATA_ORDER_XZY, 1, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%rv_tend,1)*SIZE(grid%rv_tend,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%rv_tend, 2, RWORDSIZE, 1, 1, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%ph_2,1)*SIZE(grid%ph_2,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%ph_2, 2, RWORDSIZE, 1, 1, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%al,1)*SIZE(grid%al,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%al, 2, RWORDSIZE, 1, 1, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%p,1)*SIZE(grid%p,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%p, 2, RWORDSIZE, 1, 1, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%t_1,1)*SIZE(grid%t_1,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%t_1, 2, RWORDSIZE, 1, 1, DATA_ORDER_XZY, 0, &
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
IF ( SIZE(grid%u_save,1)*SIZE(grid%u_save,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%u_save, 2, RWORDSIZE, 1, 1, DATA_ORDER_XZY, 1, &
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
IF ( SIZE(grid%mu_1,1)*SIZE(grid%mu_1,2) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%mu_1, 2, RWORDSIZE, 1, 1, DATA_ORDER_XY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
ENDIF
IF ( SIZE(grid%mu_2,1)*SIZE(grid%mu_2,2) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%mu_2, 2, RWORDSIZE, 1, 1, DATA_ORDER_XY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
ENDIF
IF ( SIZE(grid%mudf,1)*SIZE(grid%mudf,2) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%mudf, 2, RWORDSIZE, 1, 1, DATA_ORDER_XY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
ENDIF
IF ( SIZE(grid%php,1)*SIZE(grid%php,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%php, 2, RWORDSIZE, 1, 1, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%alt,1)*SIZE(grid%alt,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%alt, 2, RWORDSIZE, 1, 1, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%pb,1)*SIZE(grid%pb,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%pb, 2, RWORDSIZE, 1, 1, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
END IF
IF ( config_flags%periodic_y ) THEN
CALL RSL_LITE_INIT_PERIOD ( local_communicator_periodic, 2 , &
     12, 3, RWORDSIZE, &
     0, 0, IWORDSIZE, &
     0, 0, DWORDSIZE, &
      0,  0, LWORDSIZE, &
      mytask, ntasks, ntasks_x, ntasks_y,   &
      ips, ipe, jps, jpe, kps, kpe    )
IF ( SIZE(grid%ru_tend,1)*SIZE(grid%ru_tend,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%ru_tend, 2, RWORDSIZE, 0, 0, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%rv_tend,1)*SIZE(grid%rv_tend,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%rv_tend, 2, RWORDSIZE, 0, 0, DATA_ORDER_XZY, 1, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%ph_2,1)*SIZE(grid%ph_2,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%ph_2, 2, RWORDSIZE, 0, 0, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%al,1)*SIZE(grid%al,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%al, 2, RWORDSIZE, 0, 0, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%p,1)*SIZE(grid%p,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%p, 2, RWORDSIZE, 0, 0, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%t_1,1)*SIZE(grid%t_1,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%t_1, 2, RWORDSIZE, 0, 0, DATA_ORDER_XZY, 0, &
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
IF ( SIZE(grid%u_save,1)*SIZE(grid%u_save,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%u_save, 2, RWORDSIZE, 0, 0, DATA_ORDER_XZY, 0, &
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
IF ( SIZE(grid%mu_1,1)*SIZE(grid%mu_1,2) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%mu_1, 2, RWORDSIZE, 0, 0, DATA_ORDER_XY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
ENDIF
IF ( SIZE(grid%mu_2,1)*SIZE(grid%mu_2,2) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%mu_2, 2, RWORDSIZE, 0, 0, DATA_ORDER_XY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
ENDIF
IF ( SIZE(grid%mudf,1)*SIZE(grid%mudf,2) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%mudf, 2, RWORDSIZE, 0, 0, DATA_ORDER_XY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
ENDIF
IF ( SIZE(grid%php,1)*SIZE(grid%php,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%php, 2, RWORDSIZE, 0, 0, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%alt,1)*SIZE(grid%alt,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%alt, 2, RWORDSIZE, 0, 0, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%pb,1)*SIZE(grid%pb,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%pb, 2, RWORDSIZE, 0, 0, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
   CALL RSL_LITE_EXCH_PERIOD_Y ( local_communicator_periodic , mytask, ntasks, ntasks_x, ntasks_y )
IF ( SIZE(grid%ru_tend,1)*SIZE(grid%ru_tend,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%ru_tend, 2, RWORDSIZE, 0, 1, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%rv_tend,1)*SIZE(grid%rv_tend,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%rv_tend, 2, RWORDSIZE, 0, 1, DATA_ORDER_XZY, 1, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%ph_2,1)*SIZE(grid%ph_2,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%ph_2, 2, RWORDSIZE, 0, 1, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%al,1)*SIZE(grid%al,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%al, 2, RWORDSIZE, 0, 1, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%p,1)*SIZE(grid%p,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%p, 2, RWORDSIZE, 0, 1, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%t_1,1)*SIZE(grid%t_1,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%t_1, 2, RWORDSIZE, 0, 1, DATA_ORDER_XZY, 0, &
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
IF ( SIZE(grid%u_save,1)*SIZE(grid%u_save,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%u_save, 2, RWORDSIZE, 0, 1, DATA_ORDER_XZY, 0, &
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
IF ( SIZE(grid%mu_1,1)*SIZE(grid%mu_1,2) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%mu_1, 2, RWORDSIZE, 0, 1, DATA_ORDER_XY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
ENDIF
IF ( SIZE(grid%mu_2,1)*SIZE(grid%mu_2,2) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%mu_2, 2, RWORDSIZE, 0, 1, DATA_ORDER_XY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
ENDIF
IF ( SIZE(grid%mudf,1)*SIZE(grid%mudf,2) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%mudf, 2, RWORDSIZE, 0, 1, DATA_ORDER_XY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
ENDIF
IF ( SIZE(grid%php,1)*SIZE(grid%php,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%php, 2, RWORDSIZE, 0, 1, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%alt,1)*SIZE(grid%alt,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%alt, 2, RWORDSIZE, 0, 1, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%pb,1)*SIZE(grid%pb,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%pb, 2, RWORDSIZE, 0, 1, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
END IF
!ENDOFREGISTRYGENERATEDINCLUDE
