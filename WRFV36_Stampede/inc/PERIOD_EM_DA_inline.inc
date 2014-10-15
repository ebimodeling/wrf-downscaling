!STARTOFREGISTRYGENERATEDINCLUDE 'inc/PERIOD_EM_DA_inline.inc'
!
! WARNING This file is generated automatically by use_registry
! using the data base in the file named Registry.
! Do not edit.  Your changes to this file will be lost.
!
CALL wrf_debug(2,'calling inc/PERIOD_EM_DA_inline.inc')
IF ( config_flags%periodic_x ) THEN
CALL RSL_LITE_INIT_PERIOD ( local_communicator_periodic, 2 , &
     3, 2, RWORDSIZE, &
     0, 0, IWORDSIZE, &
     0, 0, DWORDSIZE, &
      0,  0, LWORDSIZE, &
      mytask, ntasks, ntasks_x, ntasks_y,   &
      ips, ipe, jps, jpe, kps, kpe    )
IF ( SIZE(grid%ru_m,1)*SIZE(grid%ru_m,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%ru_m, 2, RWORDSIZE, 1, 0, DATA_ORDER_XZY, 1, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%rv_m,1)*SIZE(grid%rv_m,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%rv_m, 2, RWORDSIZE, 1, 0, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%ww_m,1)*SIZE(grid%ww_m,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%ww_m, 2, RWORDSIZE, 1, 0, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%mut,1)*SIZE(grid%mut,2) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%mut, 2, RWORDSIZE, 1, 0, DATA_ORDER_XY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
ENDIF
IF ( SIZE(grid%muts,1)*SIZE(grid%muts,2) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%muts, 2, RWORDSIZE, 1, 0, DATA_ORDER_XY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
ENDIF
   CALL RSL_LITE_EXCH_PERIOD_X ( local_communicator_periodic , mytask, ntasks, ntasks_x, ntasks_y )
IF ( SIZE(grid%ru_m,1)*SIZE(grid%ru_m,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%ru_m, 2, RWORDSIZE, 1, 1, DATA_ORDER_XZY, 1, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%rv_m,1)*SIZE(grid%rv_m,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%rv_m, 2, RWORDSIZE, 1, 1, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%ww_m,1)*SIZE(grid%ww_m,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%ww_m, 2, RWORDSIZE, 1, 1, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%mut,1)*SIZE(grid%mut,2) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%mut, 2, RWORDSIZE, 1, 1, DATA_ORDER_XY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
ENDIF
IF ( SIZE(grid%muts,1)*SIZE(grid%muts,2) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%muts, 2, RWORDSIZE, 1, 1, DATA_ORDER_XY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
ENDIF
END IF
IF ( config_flags%periodic_y ) THEN
CALL RSL_LITE_INIT_PERIOD ( local_communicator_periodic, 2 , &
     3, 2, RWORDSIZE, &
     0, 0, IWORDSIZE, &
     0, 0, DWORDSIZE, &
      0,  0, LWORDSIZE, &
      mytask, ntasks, ntasks_x, ntasks_y,   &
      ips, ipe, jps, jpe, kps, kpe    )
IF ( SIZE(grid%ru_m,1)*SIZE(grid%ru_m,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%ru_m, 2, RWORDSIZE, 0, 0, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%rv_m,1)*SIZE(grid%rv_m,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%rv_m, 2, RWORDSIZE, 0, 0, DATA_ORDER_XZY, 1, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%ww_m,1)*SIZE(grid%ww_m,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%ww_m, 2, RWORDSIZE, 0, 0, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%mut,1)*SIZE(grid%mut,2) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%mut, 2, RWORDSIZE, 0, 0, DATA_ORDER_XY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
ENDIF
IF ( SIZE(grid%muts,1)*SIZE(grid%muts,2) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%muts, 2, RWORDSIZE, 0, 0, DATA_ORDER_XY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
ENDIF
   CALL RSL_LITE_EXCH_PERIOD_Y ( local_communicator_periodic , mytask, ntasks, ntasks_x, ntasks_y )
IF ( SIZE(grid%ru_m,1)*SIZE(grid%ru_m,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%ru_m, 2, RWORDSIZE, 0, 1, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%rv_m,1)*SIZE(grid%rv_m,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%rv_m, 2, RWORDSIZE, 0, 1, DATA_ORDER_XZY, 1, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%ww_m,1)*SIZE(grid%ww_m,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%ww_m, 2, RWORDSIZE, 0, 1, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%mut,1)*SIZE(grid%mut,2) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%mut, 2, RWORDSIZE, 0, 1, DATA_ORDER_XY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
ENDIF
IF ( SIZE(grid%muts,1)*SIZE(grid%muts,2) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%muts, 2, RWORDSIZE, 0, 1, DATA_ORDER_XY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
ENDIF
END IF
!ENDOFREGISTRYGENERATEDINCLUDE
