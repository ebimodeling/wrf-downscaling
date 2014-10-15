!STARTOFREGISTRYGENERATEDINCLUDE 'inc/i1_decl.inc'
!
! WARNING This file is generated automatically by use_registry
! using the data base in the file named Registry.
! Do not edit.  Your changes to this file will be lost.
!
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: ru_tendf
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: rv_tendf
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: ww1
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: wwp
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: rw_tend
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: rw_tendf
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: w_save
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: ph_tend
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: ph_tendf
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: ph_save
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: t_tend
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: t_tendf
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: t_2save
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: muus
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: muvs
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: muave
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: mu_save
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: mu_tend
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: mu_tendf
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: tke_tend
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: advect_tend
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: alpha
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: a
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: gamma
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: c2a
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: phm
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: cqu
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: cqv
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: cqw
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: pm1
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: psim
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: psih
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: wspd
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: gz1oz0
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: br
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: chklowq
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: ht_loc
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: th_phy
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: pi_phy
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: p_phy
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: dz8w
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: p8w
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: t8w
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: rho_phy
logical   ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: cu_act_flag
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: hol
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33,num_moist)           :: moist_tend
#ifndef NO_I1_OLD
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33,num_moist)           :: moist_old
#endif
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33,num_dfi_moist)           :: dfi_moist_tend
#ifndef NO_I1_OLD
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33,num_dfi_moist)           :: dfi_moist_old
#endif
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33,num_scalar)           :: scalar_tend
#ifndef NO_I1_OLD
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33,num_scalar)           :: scalar_old
#endif
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33,num_dfi_scalar)           :: dfi_scalar_tend
#ifndef NO_I1_OLD
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33,num_dfi_scalar)           :: dfi_scalar_old
#endif
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33,num_chem)           :: chem_tend
#ifndef NO_I1_OLD
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33,num_chem)           :: chem_old
#endif
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33,num_tracer)           :: tracer_tend
#ifndef NO_I1_OLD
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33,num_tracer)           :: tracer_old
#endif
!ENDOFREGISTRYGENERATEDINCLUDE
