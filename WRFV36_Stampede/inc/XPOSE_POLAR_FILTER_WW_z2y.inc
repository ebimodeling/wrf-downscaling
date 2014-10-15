!STARTOFREGISTRYGENERATEDINCLUDE 'inc/XPOSE_POLAR_FILTER_WW_z2y.inc'
!
! WARNING This file is generated automatically by use_registry
! using the data base in the file named Registry.
! Do not edit.  Your changes to this file will be lost.
!
  call trans_z2x ( ntasks_x, local_communicator_x, 1, RWORDSIZE, IWORDSIZE, DATA_ORDER_XZY , &
                   grid%ww_m, &  ! variable in Z decomp
                   grid%sd31, grid%ed31, grid%sd32, grid%ed32, grid%sd33, grid%ed33, &
                   grid%sp31, grid%ep31, grid%sp32, grid%ep32, grid%sp33, grid%ep33, &
                   grid%sm31, grid%em31, grid%sm32, grid%em32, grid%sm33, grid%em33, &
                   grid%ww_xxx, &  ! variable in X decomp
                   grid%sp31x, grid%ep31x, grid%sp32x, grid%ep32x, grid%sp33x, grid%ep33x, &
                   grid%sm31x, grid%em31x, grid%sm32x, grid%em32x, grid%sm33x, grid%em33x )
  call trans_x2y ( ntasks_y, local_communicator_y, 1, RWORDSIZE, IWORDSIZE, DATA_ORDER_XZY , &
                   grid%ww_xxx, &  ! variable in X decomp
                   grid%sd31, grid%ed31, grid%sd32, grid%ed32, grid%sd33, grid%ed33, &
                   grid%sp31x, grid%ep31x, grid%sp32x, grid%ep32x, grid%sp33x, grid%ep33x, &
                   grid%sm31x, grid%em31x, grid%sm32x, grid%em32x, grid%sm33x, grid%em33x, &
                   grid%dum_yyy, &  ! variable in Y decomp
                   grid%sp31y, grid%ep31y, grid%sp32y, grid%ep32y, grid%sp33y, grid%ep33y, &
                   grid%sm31y, grid%em31y, grid%sm32y, grid%em32y, grid%sm33y, grid%em33y ) 
!ENDOFREGISTRYGENERATEDINCLUDE
