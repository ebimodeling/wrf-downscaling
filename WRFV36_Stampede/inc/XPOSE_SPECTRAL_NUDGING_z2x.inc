!STARTOFREGISTRYGENERATEDINCLUDE 'inc/XPOSE_SPECTRAL_NUDGING_z2x.inc'
!
! WARNING This file is generated automatically by use_registry
! using the data base in the file named Registry.
! Do not edit.  Your changes to this file will be lost.
!
  call trans_z2x ( ntasks_x, local_communicator_x, 1, RWORDSIZE, IWORDSIZE, DATA_ORDER_XZY , &
                   grid%dif_analysis, &  ! variable in Z decomp
                   grid%sd31, grid%ed31, grid%sd32, grid%ed32, grid%sd33, grid%ed33, &
                   grid%sp31, grid%ep31, grid%sp32, grid%ep32, grid%sp33, grid%ep33, &
                   grid%sm31, grid%em31, grid%sm32, grid%em32, grid%sm33, grid%em33, &
                   grid%dif_xxx, &  ! variable in X decomp
                   grid%sp31x, grid%ep31x, grid%sp32x, grid%ep32x, grid%sp33x, grid%ep33x, &
                   grid%sm31x, grid%em31x, grid%sm32x, grid%em32x, grid%sm33x, grid%em33x ) 
!ENDOFREGISTRYGENERATEDINCLUDE
