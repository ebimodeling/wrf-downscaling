!STARTOFREGISTRYGENERATEDINCLUDE 'inc/HALO_EM_PHYS_SHCU_inline.inc'
!
! WARNING This file is generated automatically by use_registry
! using the data base in the file named Registry.
! Do not edit.  Your changes to this file will be lost.
!
CALL wrf_debug(2,'calling inc/HALO_EM_PHYS_SHCU_inline.inc')
CALL rsl_comm_iter_init(1,jps,jpe)
DO WHILE ( rsl_comm_iter( grid%id , grid%is_intermediate, 1 , &
                         0 , jds,jde,jps,jpe, grid%njds, grid%njde , & 
     rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p,   & 
     rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p    ))
 CALL RSL_LITE_INIT_EXCH ( local_communicator, 1, 0, &
     rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p,   & 
     rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p,   & 
     2, 0, RWORDSIZE, &
     0, 0, IWORDSIZE, &
     0, 0, DWORDSIZE, &
      0,  0, LWORDSIZE, &
      mytask, ntasks, ntasks_x, ntasks_y,   &
      ips, ipe, jps, jpe, kps, MAX(1,1&
,kpe &
))
IF ( SIZE(grid%rushten,1)*SIZE(grid%rushten,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK ( local_communicator,&
 grid%rushten, 1,&
rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p, &
rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p, &
RWORDSIZE, 0, 0, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%rvshten,1)*SIZE(grid%rvshten,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK ( local_communicator,&
 grid%rvshten, 1,&
rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p, &
rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p, &
RWORDSIZE, 0, 0, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
   CALL RSL_LITE_EXCH_Y ( local_communicator , mytask, ntasks, ntasks_x, ntasks_y, &
                          rsl_sendw_m,  rsl_sendw_p, rsl_recvw_m,  rsl_recvw_p    )
IF ( SIZE(grid%rushten,1)*SIZE(grid%rushten,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK ( local_communicator,&
 grid%rushten, 1,&
rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p, &
rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p, &
RWORDSIZE, 0, 1, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%rvshten,1)*SIZE(grid%rvshten,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK ( local_communicator,&
 grid%rvshten, 1,&
rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p, &
rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p, &
RWORDSIZE, 0, 1, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
ENDDO
CALL rsl_comm_iter_init(1,ips,ipe)
DO WHILE ( rsl_comm_iter( grid%id , grid%is_intermediate, 1 , &
                         1 , ids,ide,ips,ipe, grid%nids, grid%nide , & 
     rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p,   & 
     rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p    ))
 CALL RSL_LITE_INIT_EXCH ( local_communicator, 1, 1, &
     rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p,   & 
     rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p,   & 
     2, 0, RWORDSIZE, &
     0, 0, IWORDSIZE, &
     0, 0, DWORDSIZE, &
      0,  0, LWORDSIZE, &
      mytask, ntasks, ntasks_x, ntasks_y,   &
      ips, ipe, jps, jpe, kps, MAX(1,1&
,kpe &
))
IF ( SIZE(grid%rushten,1)*SIZE(grid%rushten,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK ( local_communicator,&
 grid%rushten, 1,&
rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p, &
rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p, &
RWORDSIZE, 1, 0, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%rvshten,1)*SIZE(grid%rvshten,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK ( local_communicator,&
 grid%rvshten, 1,&
rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p, &
rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p, &
RWORDSIZE, 1, 0, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
   CALL RSL_LITE_EXCH_X ( local_communicator , mytask, ntasks, ntasks_x, ntasks_y, &
                          rsl_sendw_m,  rsl_sendw_p, rsl_recvw_m,  rsl_recvw_p    )
IF ( SIZE(grid%rushten,1)*SIZE(grid%rushten,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK ( local_communicator,&
 grid%rushten, 1,&
rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p, &
rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p, &
RWORDSIZE, 1, 1, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%rvshten,1)*SIZE(grid%rvshten,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK ( local_communicator,&
 grid%rvshten, 1,&
rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p, &
rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p, &
RWORDSIZE, 1, 1, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
    ENDDO
!ENDOFREGISTRYGENERATEDINCLUDE
