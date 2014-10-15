!STARTOFREGISTRYGENERATEDINCLUDE 'inc/HALO_FIRE_FUEL_inline.inc'
!
! WARNING This file is generated automatically by use_registry
! using the data base in the file named Registry.
! Do not edit.  Your changes to this file will be lost.
!
CALL wrf_debug(2,'calling inc/HALO_FIRE_FUEL_inline.inc')
IF ( grid%sr_y .GT. 0 ) THEN
CALL rsl_comm_iter_init(1,jps,jpe)
DO WHILE ( rsl_comm_iter( grid%id , grid%is_intermediate, 1 , &
                         0 , jds,jde,jps,jpe, grid%njds, grid%njde , & 
     rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p,   & 
     rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p    ))
 CALL RSL_LITE_INIT_EXCH ( local_communicator, 1, 0, &
     rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p,   & 
     rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p,   & 
     0, 11, RWORDSIZE, &
     0, 0, IWORDSIZE, &
     0, 0, DWORDSIZE, &
      0,  0, LWORDSIZE, &
      mytask, ntasks, ntasks_x, ntasks_y,   &
(ips-1)*grid%sr_x+1,ipe*grid%sr_x,(jps-1)*grid%sr_y+1,jpe*grid%sr_y,kps,kpe)
IF ( SIZE(grid%fuel_frac,1)*SIZE(grid%fuel_frac,2) .GT. 1 ) THEN
CALL RSL_LITE_PACK ( local_communicator,&
 grid%fuel_frac, 1,&
rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p, &
rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p, &
RWORDSIZE, 0, 0, DATA_ORDER_XY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide*grid%sr_x, jds, jde*grid%sr_y, kds, kde, &
(ims-1)*grid%sr_x+1,ime*grid%sr_x,(jms-1)*grid%sr_y+1,jme*grid%sr_y,1,1,&
(ips-1)*grid%sr_x+1,ipe*grid%sr_x,(jps-1)*grid%sr_y+1,jpe*grid%sr_y,1,1)
ENDIF
IF ( SIZE(grid%fuel_time,1)*SIZE(grid%fuel_time,2) .GT. 1 ) THEN
CALL RSL_LITE_PACK ( local_communicator,&
 grid%fuel_time, 1,&
rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p, &
rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p, &
RWORDSIZE, 0, 0, DATA_ORDER_XY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide*grid%sr_x, jds, jde*grid%sr_y, kds, kde, &
(ims-1)*grid%sr_x+1,ime*grid%sr_x,(jms-1)*grid%sr_y+1,jme*grid%sr_y,1,1,&
(ips-1)*grid%sr_x+1,ipe*grid%sr_x,(jps-1)*grid%sr_y+1,jpe*grid%sr_y,1,1)
ENDIF
IF ( SIZE(grid%bbb,1)*SIZE(grid%bbb,2) .GT. 1 ) THEN
CALL RSL_LITE_PACK ( local_communicator,&
 grid%bbb, 1,&
rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p, &
rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p, &
RWORDSIZE, 0, 0, DATA_ORDER_XY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide*grid%sr_x, jds, jde*grid%sr_y, kds, kde, &
(ims-1)*grid%sr_x+1,ime*grid%sr_x,(jms-1)*grid%sr_y+1,jme*grid%sr_y,1,1,&
(ips-1)*grid%sr_x+1,ipe*grid%sr_x,(jps-1)*grid%sr_y+1,jpe*grid%sr_y,1,1)
ENDIF
IF ( SIZE(grid%betafl,1)*SIZE(grid%betafl,2) .GT. 1 ) THEN
CALL RSL_LITE_PACK ( local_communicator,&
 grid%betafl, 1,&
rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p, &
rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p, &
RWORDSIZE, 0, 0, DATA_ORDER_XY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide*grid%sr_x, jds, jde*grid%sr_y, kds, kde, &
(ims-1)*grid%sr_x+1,ime*grid%sr_x,(jms-1)*grid%sr_y+1,jme*grid%sr_y,1,1,&
(ips-1)*grid%sr_x+1,ipe*grid%sr_x,(jps-1)*grid%sr_y+1,jpe*grid%sr_y,1,1)
ENDIF
IF ( SIZE(grid%phiwc,1)*SIZE(grid%phiwc,2) .GT. 1 ) THEN
CALL RSL_LITE_PACK ( local_communicator,&
 grid%phiwc, 1,&
rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p, &
rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p, &
RWORDSIZE, 0, 0, DATA_ORDER_XY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide*grid%sr_x, jds, jde*grid%sr_y, kds, kde, &
(ims-1)*grid%sr_x+1,ime*grid%sr_x,(jms-1)*grid%sr_y+1,jme*grid%sr_y,1,1,&
(ips-1)*grid%sr_x+1,ipe*grid%sr_x,(jps-1)*grid%sr_y+1,jpe*grid%sr_y,1,1)
ENDIF
IF ( SIZE(grid%r_0,1)*SIZE(grid%r_0,2) .GT. 1 ) THEN
CALL RSL_LITE_PACK ( local_communicator,&
 grid%r_0, 1,&
rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p, &
rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p, &
RWORDSIZE, 0, 0, DATA_ORDER_XY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide*grid%sr_x, jds, jde*grid%sr_y, kds, kde, &
(ims-1)*grid%sr_x+1,ime*grid%sr_x,(jms-1)*grid%sr_y+1,jme*grid%sr_y,1,1,&
(ips-1)*grid%sr_x+1,ipe*grid%sr_x,(jps-1)*grid%sr_y+1,jpe*grid%sr_y,1,1)
ENDIF
IF ( SIZE(grid%fgip,1)*SIZE(grid%fgip,2) .GT. 1 ) THEN
CALL RSL_LITE_PACK ( local_communicator,&
 grid%fgip, 1,&
rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p, &
rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p, &
RWORDSIZE, 0, 0, DATA_ORDER_XY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide*grid%sr_x, jds, jde*grid%sr_y, kds, kde, &
(ims-1)*grid%sr_x+1,ime*grid%sr_x,(jms-1)*grid%sr_y+1,jme*grid%sr_y,1,1,&
(ips-1)*grid%sr_x+1,ipe*grid%sr_x,(jps-1)*grid%sr_y+1,jpe*grid%sr_y,1,1)
ENDIF
IF ( SIZE(grid%ischap,1)*SIZE(grid%ischap,2) .GT. 1 ) THEN
CALL RSL_LITE_PACK ( local_communicator,&
 grid%ischap, 1,&
rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p, &
rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p, &
RWORDSIZE, 0, 0, DATA_ORDER_XY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide*grid%sr_x, jds, jde*grid%sr_y, kds, kde, &
(ims-1)*grid%sr_x+1,ime*grid%sr_x,(jms-1)*grid%sr_y+1,jme*grid%sr_y,1,1,&
(ips-1)*grid%sr_x+1,ipe*grid%sr_x,(jps-1)*grid%sr_y+1,jpe*grid%sr_y,1,1)
ENDIF
IF ( SIZE(grid%nfuel_cat,1)*SIZE(grid%nfuel_cat,2) .GT. 1 ) THEN
CALL RSL_LITE_PACK ( local_communicator,&
 grid%nfuel_cat, 1,&
rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p, &
rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p, &
RWORDSIZE, 0, 0, DATA_ORDER_XY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide*grid%sr_x, jds, jde*grid%sr_y, kds, kde, &
(ims-1)*grid%sr_x+1,ime*grid%sr_x,(jms-1)*grid%sr_y+1,jme*grid%sr_y,1,1,&
(ips-1)*grid%sr_x+1,ipe*grid%sr_x,(jps-1)*grid%sr_y+1,jpe*grid%sr_y,1,1)
ENDIF
IF ( SIZE(grid%dzdxf,1)*SIZE(grid%dzdxf,2) .GT. 1 ) THEN
CALL RSL_LITE_PACK ( local_communicator,&
 grid%dzdxf, 1,&
rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p, &
rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p, &
RWORDSIZE, 0, 0, DATA_ORDER_XY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide*grid%sr_x, jds, jde*grid%sr_y, kds, kde, &
(ims-1)*grid%sr_x+1,ime*grid%sr_x,(jms-1)*grid%sr_y+1,jme*grid%sr_y,1,1,&
(ips-1)*grid%sr_x+1,ipe*grid%sr_x,(jps-1)*grid%sr_y+1,jpe*grid%sr_y,1,1)
ENDIF
IF ( SIZE(grid%dzdyf,1)*SIZE(grid%dzdyf,2) .GT. 1 ) THEN
CALL RSL_LITE_PACK ( local_communicator,&
 grid%dzdyf, 1,&
rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p, &
rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p, &
RWORDSIZE, 0, 0, DATA_ORDER_XY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide*grid%sr_x, jds, jde*grid%sr_y, kds, kde, &
(ims-1)*grid%sr_x+1,ime*grid%sr_x,(jms-1)*grid%sr_y+1,jme*grid%sr_y,1,1,&
(ips-1)*grid%sr_x+1,ipe*grid%sr_x,(jps-1)*grid%sr_y+1,jpe*grid%sr_y,1,1)
ENDIF
   CALL RSL_LITE_EXCH_Y ( local_communicator , mytask, ntasks, ntasks_x, ntasks_y, &
                          rsl_sendw_m,  rsl_sendw_p, rsl_recvw_m,  rsl_recvw_p    )
IF ( SIZE(grid%fuel_frac,1)*SIZE(grid%fuel_frac,2) .GT. 1 ) THEN
CALL RSL_LITE_PACK ( local_communicator,&
 grid%fuel_frac, 1,&
rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p, &
rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p, &
RWORDSIZE, 0, 1, DATA_ORDER_XY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide*grid%sr_x, jds, jde*grid%sr_y, kds, kde, &
(ims-1)*grid%sr_x+1,ime*grid%sr_x,(jms-1)*grid%sr_y+1,jme*grid%sr_y,1,1,&
(ips-1)*grid%sr_x+1,ipe*grid%sr_x,(jps-1)*grid%sr_y+1,jpe*grid%sr_y,1,1)
ENDIF
IF ( SIZE(grid%fuel_time,1)*SIZE(grid%fuel_time,2) .GT. 1 ) THEN
CALL RSL_LITE_PACK ( local_communicator,&
 grid%fuel_time, 1,&
rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p, &
rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p, &
RWORDSIZE, 0, 1, DATA_ORDER_XY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide*grid%sr_x, jds, jde*grid%sr_y, kds, kde, &
(ims-1)*grid%sr_x+1,ime*grid%sr_x,(jms-1)*grid%sr_y+1,jme*grid%sr_y,1,1,&
(ips-1)*grid%sr_x+1,ipe*grid%sr_x,(jps-1)*grid%sr_y+1,jpe*grid%sr_y,1,1)
ENDIF
IF ( SIZE(grid%bbb,1)*SIZE(grid%bbb,2) .GT. 1 ) THEN
CALL RSL_LITE_PACK ( local_communicator,&
 grid%bbb, 1,&
rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p, &
rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p, &
RWORDSIZE, 0, 1, DATA_ORDER_XY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide*grid%sr_x, jds, jde*grid%sr_y, kds, kde, &
(ims-1)*grid%sr_x+1,ime*grid%sr_x,(jms-1)*grid%sr_y+1,jme*grid%sr_y,1,1,&
(ips-1)*grid%sr_x+1,ipe*grid%sr_x,(jps-1)*grid%sr_y+1,jpe*grid%sr_y,1,1)
ENDIF
IF ( SIZE(grid%betafl,1)*SIZE(grid%betafl,2) .GT. 1 ) THEN
CALL RSL_LITE_PACK ( local_communicator,&
 grid%betafl, 1,&
rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p, &
rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p, &
RWORDSIZE, 0, 1, DATA_ORDER_XY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide*grid%sr_x, jds, jde*grid%sr_y, kds, kde, &
(ims-1)*grid%sr_x+1,ime*grid%sr_x,(jms-1)*grid%sr_y+1,jme*grid%sr_y,1,1,&
(ips-1)*grid%sr_x+1,ipe*grid%sr_x,(jps-1)*grid%sr_y+1,jpe*grid%sr_y,1,1)
ENDIF
IF ( SIZE(grid%phiwc,1)*SIZE(grid%phiwc,2) .GT. 1 ) THEN
CALL RSL_LITE_PACK ( local_communicator,&
 grid%phiwc, 1,&
rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p, &
rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p, &
RWORDSIZE, 0, 1, DATA_ORDER_XY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide*grid%sr_x, jds, jde*grid%sr_y, kds, kde, &
(ims-1)*grid%sr_x+1,ime*grid%sr_x,(jms-1)*grid%sr_y+1,jme*grid%sr_y,1,1,&
(ips-1)*grid%sr_x+1,ipe*grid%sr_x,(jps-1)*grid%sr_y+1,jpe*grid%sr_y,1,1)
ENDIF
IF ( SIZE(grid%r_0,1)*SIZE(grid%r_0,2) .GT. 1 ) THEN
CALL RSL_LITE_PACK ( local_communicator,&
 grid%r_0, 1,&
rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p, &
rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p, &
RWORDSIZE, 0, 1, DATA_ORDER_XY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide*grid%sr_x, jds, jde*grid%sr_y, kds, kde, &
(ims-1)*grid%sr_x+1,ime*grid%sr_x,(jms-1)*grid%sr_y+1,jme*grid%sr_y,1,1,&
(ips-1)*grid%sr_x+1,ipe*grid%sr_x,(jps-1)*grid%sr_y+1,jpe*grid%sr_y,1,1)
ENDIF
IF ( SIZE(grid%fgip,1)*SIZE(grid%fgip,2) .GT. 1 ) THEN
CALL RSL_LITE_PACK ( local_communicator,&
 grid%fgip, 1,&
rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p, &
rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p, &
RWORDSIZE, 0, 1, DATA_ORDER_XY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide*grid%sr_x, jds, jde*grid%sr_y, kds, kde, &
(ims-1)*grid%sr_x+1,ime*grid%sr_x,(jms-1)*grid%sr_y+1,jme*grid%sr_y,1,1,&
(ips-1)*grid%sr_x+1,ipe*grid%sr_x,(jps-1)*grid%sr_y+1,jpe*grid%sr_y,1,1)
ENDIF
IF ( SIZE(grid%ischap,1)*SIZE(grid%ischap,2) .GT. 1 ) THEN
CALL RSL_LITE_PACK ( local_communicator,&
 grid%ischap, 1,&
rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p, &
rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p, &
RWORDSIZE, 0, 1, DATA_ORDER_XY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide*grid%sr_x, jds, jde*grid%sr_y, kds, kde, &
(ims-1)*grid%sr_x+1,ime*grid%sr_x,(jms-1)*grid%sr_y+1,jme*grid%sr_y,1,1,&
(ips-1)*grid%sr_x+1,ipe*grid%sr_x,(jps-1)*grid%sr_y+1,jpe*grid%sr_y,1,1)
ENDIF
IF ( SIZE(grid%nfuel_cat,1)*SIZE(grid%nfuel_cat,2) .GT. 1 ) THEN
CALL RSL_LITE_PACK ( local_communicator,&
 grid%nfuel_cat, 1,&
rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p, &
rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p, &
RWORDSIZE, 0, 1, DATA_ORDER_XY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide*grid%sr_x, jds, jde*grid%sr_y, kds, kde, &
(ims-1)*grid%sr_x+1,ime*grid%sr_x,(jms-1)*grid%sr_y+1,jme*grid%sr_y,1,1,&
(ips-1)*grid%sr_x+1,ipe*grid%sr_x,(jps-1)*grid%sr_y+1,jpe*grid%sr_y,1,1)
ENDIF
IF ( SIZE(grid%dzdxf,1)*SIZE(grid%dzdxf,2) .GT. 1 ) THEN
CALL RSL_LITE_PACK ( local_communicator,&
 grid%dzdxf, 1,&
rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p, &
rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p, &
RWORDSIZE, 0, 1, DATA_ORDER_XY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide*grid%sr_x, jds, jde*grid%sr_y, kds, kde, &
(ims-1)*grid%sr_x+1,ime*grid%sr_x,(jms-1)*grid%sr_y+1,jme*grid%sr_y,1,1,&
(ips-1)*grid%sr_x+1,ipe*grid%sr_x,(jps-1)*grid%sr_y+1,jpe*grid%sr_y,1,1)
ENDIF
IF ( SIZE(grid%dzdyf,1)*SIZE(grid%dzdyf,2) .GT. 1 ) THEN
CALL RSL_LITE_PACK ( local_communicator,&
 grid%dzdyf, 1,&
rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p, &
rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p, &
RWORDSIZE, 0, 1, DATA_ORDER_XY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide*grid%sr_x, jds, jde*grid%sr_y, kds, kde, &
(ims-1)*grid%sr_x+1,ime*grid%sr_x,(jms-1)*grid%sr_y+1,jme*grid%sr_y,1,1,&
(ips-1)*grid%sr_x+1,ipe*grid%sr_x,(jps-1)*grid%sr_y+1,jpe*grid%sr_y,1,1)
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
     0, 11, RWORDSIZE, &
     0, 0, IWORDSIZE, &
     0, 0, DWORDSIZE, &
      0,  0, LWORDSIZE, &
      mytask, ntasks, ntasks_x, ntasks_y,   &
(ips-1)*grid%sr_x+1,ipe*grid%sr_x,(jps-1)*grid%sr_y+1,jpe*grid%sr_y,kps,kpe)
IF ( SIZE(grid%fuel_frac,1)*SIZE(grid%fuel_frac,2) .GT. 1 ) THEN
CALL RSL_LITE_PACK ( local_communicator,&
 grid%fuel_frac, 1,&
rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p, &
rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p, &
RWORDSIZE, 1, 0, DATA_ORDER_XY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide*grid%sr_x, jds, jde*grid%sr_y, kds, kde, &
(ims-1)*grid%sr_x+1,ime*grid%sr_x,(jms-1)*grid%sr_y+1,jme*grid%sr_y,1,1,&
(ips-1)*grid%sr_x+1,ipe*grid%sr_x,(jps-1)*grid%sr_y+1,jpe*grid%sr_y,1,1)
ENDIF
IF ( SIZE(grid%fuel_time,1)*SIZE(grid%fuel_time,2) .GT. 1 ) THEN
CALL RSL_LITE_PACK ( local_communicator,&
 grid%fuel_time, 1,&
rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p, &
rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p, &
RWORDSIZE, 1, 0, DATA_ORDER_XY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide*grid%sr_x, jds, jde*grid%sr_y, kds, kde, &
(ims-1)*grid%sr_x+1,ime*grid%sr_x,(jms-1)*grid%sr_y+1,jme*grid%sr_y,1,1,&
(ips-1)*grid%sr_x+1,ipe*grid%sr_x,(jps-1)*grid%sr_y+1,jpe*grid%sr_y,1,1)
ENDIF
IF ( SIZE(grid%bbb,1)*SIZE(grid%bbb,2) .GT. 1 ) THEN
CALL RSL_LITE_PACK ( local_communicator,&
 grid%bbb, 1,&
rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p, &
rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p, &
RWORDSIZE, 1, 0, DATA_ORDER_XY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide*grid%sr_x, jds, jde*grid%sr_y, kds, kde, &
(ims-1)*grid%sr_x+1,ime*grid%sr_x,(jms-1)*grid%sr_y+1,jme*grid%sr_y,1,1,&
(ips-1)*grid%sr_x+1,ipe*grid%sr_x,(jps-1)*grid%sr_y+1,jpe*grid%sr_y,1,1)
ENDIF
IF ( SIZE(grid%betafl,1)*SIZE(grid%betafl,2) .GT. 1 ) THEN
CALL RSL_LITE_PACK ( local_communicator,&
 grid%betafl, 1,&
rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p, &
rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p, &
RWORDSIZE, 1, 0, DATA_ORDER_XY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide*grid%sr_x, jds, jde*grid%sr_y, kds, kde, &
(ims-1)*grid%sr_x+1,ime*grid%sr_x,(jms-1)*grid%sr_y+1,jme*grid%sr_y,1,1,&
(ips-1)*grid%sr_x+1,ipe*grid%sr_x,(jps-1)*grid%sr_y+1,jpe*grid%sr_y,1,1)
ENDIF
IF ( SIZE(grid%phiwc,1)*SIZE(grid%phiwc,2) .GT. 1 ) THEN
CALL RSL_LITE_PACK ( local_communicator,&
 grid%phiwc, 1,&
rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p, &
rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p, &
RWORDSIZE, 1, 0, DATA_ORDER_XY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide*grid%sr_x, jds, jde*grid%sr_y, kds, kde, &
(ims-1)*grid%sr_x+1,ime*grid%sr_x,(jms-1)*grid%sr_y+1,jme*grid%sr_y,1,1,&
(ips-1)*grid%sr_x+1,ipe*grid%sr_x,(jps-1)*grid%sr_y+1,jpe*grid%sr_y,1,1)
ENDIF
IF ( SIZE(grid%r_0,1)*SIZE(grid%r_0,2) .GT. 1 ) THEN
CALL RSL_LITE_PACK ( local_communicator,&
 grid%r_0, 1,&
rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p, &
rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p, &
RWORDSIZE, 1, 0, DATA_ORDER_XY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide*grid%sr_x, jds, jde*grid%sr_y, kds, kde, &
(ims-1)*grid%sr_x+1,ime*grid%sr_x,(jms-1)*grid%sr_y+1,jme*grid%sr_y,1,1,&
(ips-1)*grid%sr_x+1,ipe*grid%sr_x,(jps-1)*grid%sr_y+1,jpe*grid%sr_y,1,1)
ENDIF
IF ( SIZE(grid%fgip,1)*SIZE(grid%fgip,2) .GT. 1 ) THEN
CALL RSL_LITE_PACK ( local_communicator,&
 grid%fgip, 1,&
rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p, &
rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p, &
RWORDSIZE, 1, 0, DATA_ORDER_XY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide*grid%sr_x, jds, jde*grid%sr_y, kds, kde, &
(ims-1)*grid%sr_x+1,ime*grid%sr_x,(jms-1)*grid%sr_y+1,jme*grid%sr_y,1,1,&
(ips-1)*grid%sr_x+1,ipe*grid%sr_x,(jps-1)*grid%sr_y+1,jpe*grid%sr_y,1,1)
ENDIF
IF ( SIZE(grid%ischap,1)*SIZE(grid%ischap,2) .GT. 1 ) THEN
CALL RSL_LITE_PACK ( local_communicator,&
 grid%ischap, 1,&
rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p, &
rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p, &
RWORDSIZE, 1, 0, DATA_ORDER_XY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide*grid%sr_x, jds, jde*grid%sr_y, kds, kde, &
(ims-1)*grid%sr_x+1,ime*grid%sr_x,(jms-1)*grid%sr_y+1,jme*grid%sr_y,1,1,&
(ips-1)*grid%sr_x+1,ipe*grid%sr_x,(jps-1)*grid%sr_y+1,jpe*grid%sr_y,1,1)
ENDIF
IF ( SIZE(grid%nfuel_cat,1)*SIZE(grid%nfuel_cat,2) .GT. 1 ) THEN
CALL RSL_LITE_PACK ( local_communicator,&
 grid%nfuel_cat, 1,&
rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p, &
rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p, &
RWORDSIZE, 1, 0, DATA_ORDER_XY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide*grid%sr_x, jds, jde*grid%sr_y, kds, kde, &
(ims-1)*grid%sr_x+1,ime*grid%sr_x,(jms-1)*grid%sr_y+1,jme*grid%sr_y,1,1,&
(ips-1)*grid%sr_x+1,ipe*grid%sr_x,(jps-1)*grid%sr_y+1,jpe*grid%sr_y,1,1)
ENDIF
IF ( SIZE(grid%dzdxf,1)*SIZE(grid%dzdxf,2) .GT. 1 ) THEN
CALL RSL_LITE_PACK ( local_communicator,&
 grid%dzdxf, 1,&
rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p, &
rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p, &
RWORDSIZE, 1, 0, DATA_ORDER_XY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide*grid%sr_x, jds, jde*grid%sr_y, kds, kde, &
(ims-1)*grid%sr_x+1,ime*grid%sr_x,(jms-1)*grid%sr_y+1,jme*grid%sr_y,1,1,&
(ips-1)*grid%sr_x+1,ipe*grid%sr_x,(jps-1)*grid%sr_y+1,jpe*grid%sr_y,1,1)
ENDIF
IF ( SIZE(grid%dzdyf,1)*SIZE(grid%dzdyf,2) .GT. 1 ) THEN
CALL RSL_LITE_PACK ( local_communicator,&
 grid%dzdyf, 1,&
rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p, &
rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p, &
RWORDSIZE, 1, 0, DATA_ORDER_XY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide*grid%sr_x, jds, jde*grid%sr_y, kds, kde, &
(ims-1)*grid%sr_x+1,ime*grid%sr_x,(jms-1)*grid%sr_y+1,jme*grid%sr_y,1,1,&
(ips-1)*grid%sr_x+1,ipe*grid%sr_x,(jps-1)*grid%sr_y+1,jpe*grid%sr_y,1,1)
ENDIF
   CALL RSL_LITE_EXCH_X ( local_communicator , mytask, ntasks, ntasks_x, ntasks_y, &
                          rsl_sendw_m,  rsl_sendw_p, rsl_recvw_m,  rsl_recvw_p    )
IF ( SIZE(grid%fuel_frac,1)*SIZE(grid%fuel_frac,2) .GT. 1 ) THEN
CALL RSL_LITE_PACK ( local_communicator,&
 grid%fuel_frac, 1,&
rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p, &
rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p, &
RWORDSIZE, 1, 1, DATA_ORDER_XY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide*grid%sr_x, jds, jde*grid%sr_y, kds, kde, &
(ims-1)*grid%sr_x+1,ime*grid%sr_x,(jms-1)*grid%sr_y+1,jme*grid%sr_y,1,1,&
(ips-1)*grid%sr_x+1,ipe*grid%sr_x,(jps-1)*grid%sr_y+1,jpe*grid%sr_y,1,1)
ENDIF
IF ( SIZE(grid%fuel_time,1)*SIZE(grid%fuel_time,2) .GT. 1 ) THEN
CALL RSL_LITE_PACK ( local_communicator,&
 grid%fuel_time, 1,&
rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p, &
rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p, &
RWORDSIZE, 1, 1, DATA_ORDER_XY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide*grid%sr_x, jds, jde*grid%sr_y, kds, kde, &
(ims-1)*grid%sr_x+1,ime*grid%sr_x,(jms-1)*grid%sr_y+1,jme*grid%sr_y,1,1,&
(ips-1)*grid%sr_x+1,ipe*grid%sr_x,(jps-1)*grid%sr_y+1,jpe*grid%sr_y,1,1)
ENDIF
IF ( SIZE(grid%bbb,1)*SIZE(grid%bbb,2) .GT. 1 ) THEN
CALL RSL_LITE_PACK ( local_communicator,&
 grid%bbb, 1,&
rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p, &
rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p, &
RWORDSIZE, 1, 1, DATA_ORDER_XY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide*grid%sr_x, jds, jde*grid%sr_y, kds, kde, &
(ims-1)*grid%sr_x+1,ime*grid%sr_x,(jms-1)*grid%sr_y+1,jme*grid%sr_y,1,1,&
(ips-1)*grid%sr_x+1,ipe*grid%sr_x,(jps-1)*grid%sr_y+1,jpe*grid%sr_y,1,1)
ENDIF
IF ( SIZE(grid%betafl,1)*SIZE(grid%betafl,2) .GT. 1 ) THEN
CALL RSL_LITE_PACK ( local_communicator,&
 grid%betafl, 1,&
rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p, &
rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p, &
RWORDSIZE, 1, 1, DATA_ORDER_XY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide*grid%sr_x, jds, jde*grid%sr_y, kds, kde, &
(ims-1)*grid%sr_x+1,ime*grid%sr_x,(jms-1)*grid%sr_y+1,jme*grid%sr_y,1,1,&
(ips-1)*grid%sr_x+1,ipe*grid%sr_x,(jps-1)*grid%sr_y+1,jpe*grid%sr_y,1,1)
ENDIF
IF ( SIZE(grid%phiwc,1)*SIZE(grid%phiwc,2) .GT. 1 ) THEN
CALL RSL_LITE_PACK ( local_communicator,&
 grid%phiwc, 1,&
rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p, &
rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p, &
RWORDSIZE, 1, 1, DATA_ORDER_XY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide*grid%sr_x, jds, jde*grid%sr_y, kds, kde, &
(ims-1)*grid%sr_x+1,ime*grid%sr_x,(jms-1)*grid%sr_y+1,jme*grid%sr_y,1,1,&
(ips-1)*grid%sr_x+1,ipe*grid%sr_x,(jps-1)*grid%sr_y+1,jpe*grid%sr_y,1,1)
ENDIF
IF ( SIZE(grid%r_0,1)*SIZE(grid%r_0,2) .GT. 1 ) THEN
CALL RSL_LITE_PACK ( local_communicator,&
 grid%r_0, 1,&
rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p, &
rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p, &
RWORDSIZE, 1, 1, DATA_ORDER_XY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide*grid%sr_x, jds, jde*grid%sr_y, kds, kde, &
(ims-1)*grid%sr_x+1,ime*grid%sr_x,(jms-1)*grid%sr_y+1,jme*grid%sr_y,1,1,&
(ips-1)*grid%sr_x+1,ipe*grid%sr_x,(jps-1)*grid%sr_y+1,jpe*grid%sr_y,1,1)
ENDIF
IF ( SIZE(grid%fgip,1)*SIZE(grid%fgip,2) .GT. 1 ) THEN
CALL RSL_LITE_PACK ( local_communicator,&
 grid%fgip, 1,&
rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p, &
rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p, &
RWORDSIZE, 1, 1, DATA_ORDER_XY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide*grid%sr_x, jds, jde*grid%sr_y, kds, kde, &
(ims-1)*grid%sr_x+1,ime*grid%sr_x,(jms-1)*grid%sr_y+1,jme*grid%sr_y,1,1,&
(ips-1)*grid%sr_x+1,ipe*grid%sr_x,(jps-1)*grid%sr_y+1,jpe*grid%sr_y,1,1)
ENDIF
IF ( SIZE(grid%ischap,1)*SIZE(grid%ischap,2) .GT. 1 ) THEN
CALL RSL_LITE_PACK ( local_communicator,&
 grid%ischap, 1,&
rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p, &
rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p, &
RWORDSIZE, 1, 1, DATA_ORDER_XY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide*grid%sr_x, jds, jde*grid%sr_y, kds, kde, &
(ims-1)*grid%sr_x+1,ime*grid%sr_x,(jms-1)*grid%sr_y+1,jme*grid%sr_y,1,1,&
(ips-1)*grid%sr_x+1,ipe*grid%sr_x,(jps-1)*grid%sr_y+1,jpe*grid%sr_y,1,1)
ENDIF
IF ( SIZE(grid%nfuel_cat,1)*SIZE(grid%nfuel_cat,2) .GT. 1 ) THEN
CALL RSL_LITE_PACK ( local_communicator,&
 grid%nfuel_cat, 1,&
rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p, &
rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p, &
RWORDSIZE, 1, 1, DATA_ORDER_XY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide*grid%sr_x, jds, jde*grid%sr_y, kds, kde, &
(ims-1)*grid%sr_x+1,ime*grid%sr_x,(jms-1)*grid%sr_y+1,jme*grid%sr_y,1,1,&
(ips-1)*grid%sr_x+1,ipe*grid%sr_x,(jps-1)*grid%sr_y+1,jpe*grid%sr_y,1,1)
ENDIF
IF ( SIZE(grid%dzdxf,1)*SIZE(grid%dzdxf,2) .GT. 1 ) THEN
CALL RSL_LITE_PACK ( local_communicator,&
 grid%dzdxf, 1,&
rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p, &
rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p, &
RWORDSIZE, 1, 1, DATA_ORDER_XY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide*grid%sr_x, jds, jde*grid%sr_y, kds, kde, &
(ims-1)*grid%sr_x+1,ime*grid%sr_x,(jms-1)*grid%sr_y+1,jme*grid%sr_y,1,1,&
(ips-1)*grid%sr_x+1,ipe*grid%sr_x,(jps-1)*grid%sr_y+1,jpe*grid%sr_y,1,1)
ENDIF
IF ( SIZE(grid%dzdyf,1)*SIZE(grid%dzdyf,2) .GT. 1 ) THEN
CALL RSL_LITE_PACK ( local_communicator,&
 grid%dzdyf, 1,&
rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p, &
rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p, &
RWORDSIZE, 1, 1, DATA_ORDER_XY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide*grid%sr_x, jds, jde*grid%sr_y, kds, kde, &
(ims-1)*grid%sr_x+1,ime*grid%sr_x,(jms-1)*grid%sr_y+1,jme*grid%sr_y,1,1,&
(ips-1)*grid%sr_x+1,ipe*grid%sr_x,(jps-1)*grid%sr_y+1,jpe*grid%sr_y,1,1)
ENDIF
    ENDDO
ENDIF
!ENDOFREGISTRYGENERATEDINCLUDE
