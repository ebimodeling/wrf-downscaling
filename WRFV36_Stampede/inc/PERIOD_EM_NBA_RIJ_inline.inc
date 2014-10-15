!STARTOFREGISTRYGENERATEDINCLUDE 'inc/PERIOD_EM_NBA_RIJ_inline.inc'
!
! WARNING This file is generated automatically by use_registry
! using the data base in the file named Registry.
! Do not edit.  Your changes to this file will be lost.
!
CALL wrf_debug(2,'calling inc/PERIOD_EM_NBA_RIJ_inline.inc')
IF ( config_flags%periodic_x ) THEN
CALL RSL_LITE_INIT_PERIOD ( local_communicator_periodic, 3 , &
     0  &
   + num_nba_rij   &
     , 0, RWORDSIZE, &
     0, 0, IWORDSIZE, &
     0, 0, DWORDSIZE, &
      0,  0, LWORDSIZE, &
      mytask, ntasks, ntasks_x, ntasks_y,   &
      ips, ipe, jps, jpe, kps, kpe    )
DO itrace = PARAM_FIRST_SCALAR, num_nba_rij
 IF ( SIZE(nba_rij,1)*SIZE(nba_rij,3) .GT. 1 ) THEN
  CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
nba_rij ( grid%sm31,grid%sm32,grid%sm33,itrace), 3, RWORDSIZE, 1, 0, DATA_ORDER_XZY, 0, &
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
nba_rij ( grid%sm31,grid%sm32,grid%sm33,itrace), 3, RWORDSIZE, 1, 1, DATA_ORDER_XZY, 0, &
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
     , 0, RWORDSIZE, &
     0, 0, IWORDSIZE, &
     0, 0, DWORDSIZE, &
      0,  0, LWORDSIZE, &
      mytask, ntasks, ntasks_x, ntasks_y,   &
      ips, ipe, jps, jpe, kps, kpe    )
DO itrace = PARAM_FIRST_SCALAR, num_nba_rij
 IF ( SIZE(nba_rij,1)*SIZE(nba_rij,3) .GT. 1 ) THEN
  CALL RSL_LITE_PACK_PERIOD ( local_communicator_periodic,&
nba_rij ( grid%sm31,grid%sm32,grid%sm33,itrace), 3, RWORDSIZE, 0, 0, DATA_ORDER_XZY, 0, &
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
nba_rij ( grid%sm31,grid%sm32,grid%sm33,itrace), 3, RWORDSIZE, 0, 1, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
 ENDIF
ENDDO
END IF
!ENDOFREGISTRYGENERATEDINCLUDE
