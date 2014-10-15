!STARTOFREGISTRYGENERATEDINCLUDE 'inc/PERIOD_BDY_EM_PHY_BC_inline.inc'
!
! WARNING This file is generated automatically by use_registry
! using the data base in the file named Registry.
! Do not edit.  Your changes to this file will be lost.
!
CALL wrf_debug(2,'calling inc/PERIOD_BDY_EM_PHY_BC_inline.inc')
IF ( config_flags%periodic_x ) THEN
CALL RSL_LITE_INIT_PERIOD ( local_communicator_periodic, 2 , &
     16, 0, RWORDSIZE, &
     0, 0, IWORDSIZE, &
     0, 0, DWORDSIZE, &
      0,  0, LWORDSIZE, &
      mytask, ntasks, ntasks_x, ntasks_y,   &
      ips, ipe, jps, jpe, kps, kpe    )
IF ( SIZE(grid%rublten,1)*SIZE(grid%rublten,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%rublten, 2, RWORDSIZE, 1, 0, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%rvblten,1)*SIZE(grid%rvblten,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%rvblten, 2, RWORDSIZE, 1, 0, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%rucuten,1)*SIZE(grid%rucuten,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%rucuten, 2, RWORDSIZE, 1, 0, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%rvcuten,1)*SIZE(grid%rvcuten,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%rvcuten, 2, RWORDSIZE, 1, 0, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%xkmh,1)*SIZE(grid%xkmh,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%xkmh, 2, RWORDSIZE, 1, 0, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%xkmv,1)*SIZE(grid%xkmv,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%xkmv, 2, RWORDSIZE, 1, 0, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%xkhh,1)*SIZE(grid%xkhh,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%xkhh, 2, RWORDSIZE, 1, 0, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%xkhv,1)*SIZE(grid%xkhv,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%xkhv, 2, RWORDSIZE, 1, 0, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%div,1)*SIZE(grid%div,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%div, 2, RWORDSIZE, 1, 0, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%defor11,1)*SIZE(grid%defor11,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%defor11, 2, RWORDSIZE, 1, 0, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%defor22,1)*SIZE(grid%defor22,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%defor22, 2, RWORDSIZE, 1, 0, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%defor12,1)*SIZE(grid%defor12,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%defor12, 2, RWORDSIZE, 1, 0, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%defor13,1)*SIZE(grid%defor13,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%defor13, 2, RWORDSIZE, 1, 0, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%defor23,1)*SIZE(grid%defor23,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%defor23, 2, RWORDSIZE, 1, 0, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%defor33,1)*SIZE(grid%defor33,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%defor33, 2, RWORDSIZE, 1, 0, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%tke_2,1)*SIZE(grid%tke_2,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%tke_2, 2, RWORDSIZE, 1, 0, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
   CALL RSL_LITE_EXCH_PERIOD_X ( local_communicator_periodic , mytask, ntasks, ntasks_x, ntasks_y )
IF ( SIZE(grid%rublten,1)*SIZE(grid%rublten,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%rublten, 2, RWORDSIZE, 1, 1, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%rvblten,1)*SIZE(grid%rvblten,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%rvblten, 2, RWORDSIZE, 1, 1, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%rucuten,1)*SIZE(grid%rucuten,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%rucuten, 2, RWORDSIZE, 1, 1, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%rvcuten,1)*SIZE(grid%rvcuten,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%rvcuten, 2, RWORDSIZE, 1, 1, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%xkmh,1)*SIZE(grid%xkmh,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%xkmh, 2, RWORDSIZE, 1, 1, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%xkmv,1)*SIZE(grid%xkmv,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%xkmv, 2, RWORDSIZE, 1, 1, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%xkhh,1)*SIZE(grid%xkhh,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%xkhh, 2, RWORDSIZE, 1, 1, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%xkhv,1)*SIZE(grid%xkhv,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%xkhv, 2, RWORDSIZE, 1, 1, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%div,1)*SIZE(grid%div,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%div, 2, RWORDSIZE, 1, 1, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%defor11,1)*SIZE(grid%defor11,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%defor11, 2, RWORDSIZE, 1, 1, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%defor22,1)*SIZE(grid%defor22,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%defor22, 2, RWORDSIZE, 1, 1, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%defor12,1)*SIZE(grid%defor12,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%defor12, 2, RWORDSIZE, 1, 1, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%defor13,1)*SIZE(grid%defor13,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%defor13, 2, RWORDSIZE, 1, 1, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%defor23,1)*SIZE(grid%defor23,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%defor23, 2, RWORDSIZE, 1, 1, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%defor33,1)*SIZE(grid%defor33,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%defor33, 2, RWORDSIZE, 1, 1, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%tke_2,1)*SIZE(grid%tke_2,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%tke_2, 2, RWORDSIZE, 1, 1, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
END IF
IF ( config_flags%periodic_y ) THEN
CALL RSL_LITE_INIT_PERIOD ( local_communicator_periodic, 2 , &
     16, 0, RWORDSIZE, &
     0, 0, IWORDSIZE, &
     0, 0, DWORDSIZE, &
      0,  0, LWORDSIZE, &
      mytask, ntasks, ntasks_x, ntasks_y,   &
      ips, ipe, jps, jpe, kps, kpe    )
IF ( SIZE(grid%rublten,1)*SIZE(grid%rublten,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%rublten, 2, RWORDSIZE, 0, 0, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%rvblten,1)*SIZE(grid%rvblten,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%rvblten, 2, RWORDSIZE, 0, 0, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%rucuten,1)*SIZE(grid%rucuten,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%rucuten, 2, RWORDSIZE, 0, 0, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%rvcuten,1)*SIZE(grid%rvcuten,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%rvcuten, 2, RWORDSIZE, 0, 0, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%xkmh,1)*SIZE(grid%xkmh,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%xkmh, 2, RWORDSIZE, 0, 0, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%xkmv,1)*SIZE(grid%xkmv,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%xkmv, 2, RWORDSIZE, 0, 0, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%xkhh,1)*SIZE(grid%xkhh,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%xkhh, 2, RWORDSIZE, 0, 0, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%xkhv,1)*SIZE(grid%xkhv,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%xkhv, 2, RWORDSIZE, 0, 0, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%div,1)*SIZE(grid%div,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%div, 2, RWORDSIZE, 0, 0, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%defor11,1)*SIZE(grid%defor11,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%defor11, 2, RWORDSIZE, 0, 0, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%defor22,1)*SIZE(grid%defor22,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%defor22, 2, RWORDSIZE, 0, 0, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%defor12,1)*SIZE(grid%defor12,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%defor12, 2, RWORDSIZE, 0, 0, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%defor13,1)*SIZE(grid%defor13,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%defor13, 2, RWORDSIZE, 0, 0, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%defor23,1)*SIZE(grid%defor23,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%defor23, 2, RWORDSIZE, 0, 0, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%defor33,1)*SIZE(grid%defor33,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%defor33, 2, RWORDSIZE, 0, 0, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%tke_2,1)*SIZE(grid%tke_2,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%tke_2, 2, RWORDSIZE, 0, 0, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
   CALL RSL_LITE_EXCH_PERIOD_Y ( local_communicator_periodic , mytask, ntasks, ntasks_x, ntasks_y )
IF ( SIZE(grid%rublten,1)*SIZE(grid%rublten,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%rublten, 2, RWORDSIZE, 0, 1, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%rvblten,1)*SIZE(grid%rvblten,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%rvblten, 2, RWORDSIZE, 0, 1, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%rucuten,1)*SIZE(grid%rucuten,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%rucuten, 2, RWORDSIZE, 0, 1, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%rvcuten,1)*SIZE(grid%rvcuten,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%rvcuten, 2, RWORDSIZE, 0, 1, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%xkmh,1)*SIZE(grid%xkmh,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%xkmh, 2, RWORDSIZE, 0, 1, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%xkmv,1)*SIZE(grid%xkmv,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%xkmv, 2, RWORDSIZE, 0, 1, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%xkhh,1)*SIZE(grid%xkhh,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%xkhh, 2, RWORDSIZE, 0, 1, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%xkhv,1)*SIZE(grid%xkhv,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%xkhv, 2, RWORDSIZE, 0, 1, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%div,1)*SIZE(grid%div,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%div, 2, RWORDSIZE, 0, 1, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%defor11,1)*SIZE(grid%defor11,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%defor11, 2, RWORDSIZE, 0, 1, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%defor22,1)*SIZE(grid%defor22,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%defor22, 2, RWORDSIZE, 0, 1, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%defor12,1)*SIZE(grid%defor12,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%defor12, 2, RWORDSIZE, 0, 1, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%defor13,1)*SIZE(grid%defor13,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%defor13, 2, RWORDSIZE, 0, 1, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%defor23,1)*SIZE(grid%defor23,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%defor23, 2, RWORDSIZE, 0, 1, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%defor33,1)*SIZE(grid%defor33,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%defor33, 2, RWORDSIZE, 0, 1, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%tke_2,1)*SIZE(grid%tke_2,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
 grid%tke_2, 2, RWORDSIZE, 0, 1, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
END IF
!ENDOFREGISTRYGENERATEDINCLUDE
