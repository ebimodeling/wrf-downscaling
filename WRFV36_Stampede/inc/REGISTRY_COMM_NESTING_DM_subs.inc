!STARTOFREGISTRYGENERATEDINCLUDE 'inc/REGISTRY_COMM_NESTING_DM_subs.inc'
!
! WARNING This file is generated automatically by use_registry
! using the data base in the file named Registry.
! Do not edit.  Your changes to this file will be lost.
!
SUBROUTINE HALO_FORCE_DOWN_sub ( grid, &
  config_flags, &
  num_moist, &
  moist, &
  num_dfi_moist, &
  dfi_moist, &
  num_scalar, &
  scalar, &
  num_dfi_scalar, &
  dfi_scalar, &
  num_aerod, &
  aerod, &
  num_tracer, &
  tracer, &
  local_communicator, &
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
  INTEGER, INTENT(IN) :: num_dfi_moist
  real, INTENT(INOUT) :: dfi_moist ( grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33,num_dfi_moist)
  INTEGER, INTENT(IN) :: num_scalar
  real, INTENT(INOUT) :: scalar ( grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33,num_scalar)
  INTEGER, INTENT(IN) :: num_dfi_scalar
  real, INTENT(INOUT) :: dfi_scalar ( grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33,num_dfi_scalar)
  INTEGER, INTENT(IN) :: num_aerod
  real, INTENT(INOUT) :: aerod ( grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33,num_aerod)
  INTEGER, INTENT(IN) :: num_tracer
  real, INTENT(INOUT) :: tracer ( grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33,num_tracer)
  INTEGER ,                    INTENT(IN) :: local_communicator
  INTEGER ,                    INTENT(IN) :: mytask, ntasks, ntasks_x, ntasks_y
  INTEGER ,                    INTENT(IN) :: ids, ide, jds, jde, kds, kde
  INTEGER ,                    INTENT(IN) :: ims, ime, jms, jme, kms, kme
  INTEGER ,                    INTENT(IN) :: ips, ipe, jps, jpe, kps, kpe
  INTEGER :: itrace
  INTEGER :: rsl_sendw_p, rsl_sendbeg_p, rsl_recvw_p, rsl_recvbeg_p
  INTEGER :: rsl_sendw_m, rsl_sendbeg_m, rsl_recvw_m, rsl_recvbeg_m
  LOGICAL, EXTERNAL :: rsl_comm_iter
  INTEGER :: idim1, idim2, idim3, idim4, idim5, idim6, idim7
  
#ifdef DM_PARALLEL
#include "HALO_FORCE_DOWN_inline.inc"
#endif
  
  END SUBROUTINE HALO_FORCE_DOWN_sub
!ENDOFREGISTRYGENERATEDINCLUDE
!STARTOFREGISTRYGENERATEDINCLUDE 'inc/REGISTRY_COMM_NESTING_DM_subs.inc'
!
! WARNING This file is generated automatically by use_registry
! using the data base in the file named Registry.
! Do not edit.  Your changes to this file will be lost.
!
SUBROUTINE HALO_INTERP_DOWN_sub ( grid, &
  config_flags, &
  num_moist, &
  moist, &
  num_dfi_moist, &
  dfi_moist, &
  num_scalar, &
  scalar, &
  num_dfi_scalar, &
  dfi_scalar, &
  num_aerod, &
  aerod, &
  num_tracer, &
  tracer, &
  local_communicator, &
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
  INTEGER, INTENT(IN) :: num_dfi_moist
  real, INTENT(INOUT) :: dfi_moist ( grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33,num_dfi_moist)
  INTEGER, INTENT(IN) :: num_scalar
  real, INTENT(INOUT) :: scalar ( grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33,num_scalar)
  INTEGER, INTENT(IN) :: num_dfi_scalar
  real, INTENT(INOUT) :: dfi_scalar ( grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33,num_dfi_scalar)
  INTEGER, INTENT(IN) :: num_aerod
  real, INTENT(INOUT) :: aerod ( grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33,num_aerod)
  INTEGER, INTENT(IN) :: num_tracer
  real, INTENT(INOUT) :: tracer ( grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33,num_tracer)
  INTEGER ,                    INTENT(IN) :: local_communicator
  INTEGER ,                    INTENT(IN) :: mytask, ntasks, ntasks_x, ntasks_y
  INTEGER ,                    INTENT(IN) :: ids, ide, jds, jde, kds, kde
  INTEGER ,                    INTENT(IN) :: ims, ime, jms, jme, kms, kme
  INTEGER ,                    INTENT(IN) :: ips, ipe, jps, jpe, kps, kpe
  INTEGER :: itrace
  INTEGER :: rsl_sendw_p, rsl_sendbeg_p, rsl_recvw_p, rsl_recvbeg_p
  INTEGER :: rsl_sendw_m, rsl_sendbeg_m, rsl_recvw_m, rsl_recvbeg_m
  LOGICAL, EXTERNAL :: rsl_comm_iter
  INTEGER :: idim1, idim2, idim3, idim4, idim5, idim6, idim7
  
#ifdef DM_PARALLEL
#include "HALO_INTERP_DOWN_inline.inc"
#endif
  
  END SUBROUTINE HALO_INTERP_DOWN_sub
!ENDOFREGISTRYGENERATEDINCLUDE
!STARTOFREGISTRYGENERATEDINCLUDE 'inc/REGISTRY_COMM_NESTING_DM_subs.inc'
!
! WARNING This file is generated automatically by use_registry
! using the data base in the file named Registry.
! Do not edit.  Your changes to this file will be lost.
!
SUBROUTINE HALO_INTERP_UP_sub ( grid, &
  config_flags, &
  num_moist, &
  moist, &
  num_dfi_moist, &
  dfi_moist, &
  num_scalar, &
  scalar, &
  num_dfi_scalar, &
  dfi_scalar, &
  num_tracer, &
  tracer, &
  local_communicator, &
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
  INTEGER, INTENT(IN) :: num_dfi_moist
  real, INTENT(INOUT) :: dfi_moist ( grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33,num_dfi_moist)
  INTEGER, INTENT(IN) :: num_scalar
  real, INTENT(INOUT) :: scalar ( grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33,num_scalar)
  INTEGER, INTENT(IN) :: num_dfi_scalar
  real, INTENT(INOUT) :: dfi_scalar ( grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33,num_dfi_scalar)
  INTEGER, INTENT(IN) :: num_tracer
  real, INTENT(INOUT) :: tracer ( grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33,num_tracer)
  INTEGER ,                    INTENT(IN) :: local_communicator
  INTEGER ,                    INTENT(IN) :: mytask, ntasks, ntasks_x, ntasks_y
  INTEGER ,                    INTENT(IN) :: ids, ide, jds, jde, kds, kde
  INTEGER ,                    INTENT(IN) :: ims, ime, jms, jme, kms, kme
  INTEGER ,                    INTENT(IN) :: ips, ipe, jps, jpe, kps, kpe
  INTEGER :: itrace
  INTEGER :: rsl_sendw_p, rsl_sendbeg_p, rsl_recvw_p, rsl_recvbeg_p
  INTEGER :: rsl_sendw_m, rsl_sendbeg_m, rsl_recvw_m, rsl_recvbeg_m
  LOGICAL, EXTERNAL :: rsl_comm_iter
  INTEGER :: idim1, idim2, idim3, idim4, idim5, idim6, idim7
  
#ifdef DM_PARALLEL
#include "HALO_INTERP_UP_inline.inc"
#endif
  
  END SUBROUTINE HALO_INTERP_UP_sub
!ENDOFREGISTRYGENERATEDINCLUDE
!STARTOFREGISTRYGENERATEDINCLUDE 'inc/REGISTRY_COMM_NESTING_DM_subs.inc'
!
! WARNING This file is generated automatically by use_registry
! using the data base in the file named Registry.
! Do not edit.  Your changes to this file will be lost.
!
SUBROUTINE HALO_INTERP_SMOOTH_sub ( grid, &
  num_moist, &
  moist, &
  num_dfi_moist, &
  dfi_moist, &
  num_scalar, &
  scalar, &
  num_dfi_scalar, &
  dfi_scalar, &
  num_tracer, &
  tracer, &
  local_communicator, &
  mytask, ntasks, ntasks_x, ntasks_y, &
  ids, ide, jds, jde, kds, kde,       &
  ims, ime, jms, jme, kms, kme,       &
  ips, ipe, jps, jpe, kps, kpe )
  USE module_domain, ONLY:domain
  USE module_configure, ONLY:grid_config_rec_type,in_use_for_config
  USE module_state_description, ONLY:PARAM_FIRST_SCALAR
  USE module_driver_constants
  TYPE(domain) ,               INTENT(IN) :: grid
  INTEGER, INTENT(IN) :: num_moist
  real, INTENT(INOUT) :: moist ( grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33,num_moist)
  INTEGER, INTENT(IN) :: num_dfi_moist
  real, INTENT(INOUT) :: dfi_moist ( grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33,num_dfi_moist)
  INTEGER, INTENT(IN) :: num_scalar
  real, INTENT(INOUT) :: scalar ( grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33,num_scalar)
  INTEGER, INTENT(IN) :: num_dfi_scalar
  real, INTENT(INOUT) :: dfi_scalar ( grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33,num_dfi_scalar)
  INTEGER, INTENT(IN) :: num_tracer
  real, INTENT(INOUT) :: tracer ( grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33,num_tracer)
  INTEGER ,                    INTENT(IN) :: local_communicator
  INTEGER ,                    INTENT(IN) :: mytask, ntasks, ntasks_x, ntasks_y
  INTEGER ,                    INTENT(IN) :: ids, ide, jds, jde, kds, kde
  INTEGER ,                    INTENT(IN) :: ims, ime, jms, jme, kms, kme
  INTEGER ,                    INTENT(IN) :: ips, ipe, jps, jpe, kps, kpe
  INTEGER :: itrace
  INTEGER :: rsl_sendw_p, rsl_sendbeg_p, rsl_recvw_p, rsl_recvbeg_p
  INTEGER :: rsl_sendw_m, rsl_sendbeg_m, rsl_recvw_m, rsl_recvbeg_m
  LOGICAL, EXTERNAL :: rsl_comm_iter
  INTEGER :: idim1, idim2, idim3, idim4, idim5, idim6, idim7
  
#ifdef DM_PARALLEL
#include "HALO_INTERP_SMOOTH_inline.inc"
#endif
  
  END SUBROUTINE HALO_INTERP_SMOOTH_sub
!ENDOFREGISTRYGENERATEDINCLUDE
