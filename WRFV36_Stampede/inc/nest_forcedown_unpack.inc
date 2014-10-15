!STARTOFREGISTRYGENERATEDINCLUDE 'inc/nest_forcedown_unpack.inc'
!
! WARNING This file is generated automatically by use_registry
! using the data base in the file named Registry.
! Do not edit.  Your changes to this file will be lost.
!
CALL rsl_lite_from_parent_info(pig,pjg,retval)
DO while ( retval .eq. 1 )
IF ( SIZE(grid%u_2) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_parent_msg((((ckde-1))-(ckds)+1)*RWORDSIZE,xv)
DO k = ckds,(ckde-1)
grid%u_2(pig,k,pjg) = xv(k)
ENDDO
ENDIF
IF ( SIZE(grid%v_2) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_parent_msg((((ckde-1))-(ckds)+1)*RWORDSIZE,xv)
DO k = ckds,(ckde-1)
grid%v_2(pig,k,pjg) = xv(k)
ENDDO
ENDIF
IF ( SIZE(grid%w_2) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_parent_msg(((ckde)-(ckds)+1)*RWORDSIZE,xv)
DO k = ckds,ckde
grid%w_2(pig,k,pjg) = xv(k)
ENDDO
ENDIF
IF ( SIZE(grid%ph_2) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_parent_msg(((ckde)-(ckds)+1)*RWORDSIZE,xv)
DO k = ckds,ckde
grid%ph_2(pig,k,pjg) = xv(k)
ENDDO
ENDIF
IF ( SIZE(grid%t_2) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_parent_msg((((ckde-1))-(ckds)+1)*RWORDSIZE,xv)
DO k = ckds,(ckde-1)
grid%t_2(pig,k,pjg) = xv(k)
ENDDO
ENDIF
IF ( SIZE(grid%mu_2) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_parent_msg(RWORDSIZE,xv)
grid%mu_2(pig,pjg) = xv(1)
ENDIF
DO itrace =  PARAM_FIRST_SCALAR, num_moist
CALL rsl_lite_from_parent_msg((((ckde-1))-(ckds)+1)*RWORDSIZE,xv)
DO k = ckds,(ckde-1)
moist(pig,k,pjg,itrace) = xv(k)
ENDDO
ENDDO
DO itrace =  PARAM_FIRST_SCALAR, num_dfi_moist
CALL rsl_lite_from_parent_msg((((ckde-1))-(ckds)+1)*RWORDSIZE,xv)
DO k = ckds,(ckde-1)
dfi_moist(pig,k,pjg,itrace) = xv(k)
ENDDO
ENDDO
DO itrace =  PARAM_FIRST_SCALAR, num_scalar
CALL rsl_lite_from_parent_msg((((ckde-1))-(ckds)+1)*RWORDSIZE,xv)
DO k = ckds,(ckde-1)
scalar(pig,k,pjg,itrace) = xv(k)
ENDDO
ENDDO
DO itrace =  PARAM_FIRST_SCALAR, num_dfi_scalar
CALL rsl_lite_from_parent_msg((((ckde-1))-(ckds)+1)*RWORDSIZE,xv)
DO k = ckds,(ckde-1)
dfi_scalar(pig,k,pjg,itrace) = xv(k)
ENDDO
ENDDO
IF ( SIZE(grid%o3rad) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_parent_msg((((ckde-1))-(ckds)+1)*RWORDSIZE,xv)
DO k = ckds,(ckde-1)
grid%o3rad(pig,k,pjg) = xv(k)
ENDDO
ENDIF
DO itrace =  PARAM_FIRST_SCALAR, num_aerod
CALL rsl_lite_from_parent_msg((((ckde-1))-(ckds)+1)*RWORDSIZE,xv)
DO k = ckds,(ckde-1)
aerod(pig,k,pjg,itrace) = xv(k)
ENDDO
ENDDO
IF ( SIZE(grid%om_tmp) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_parent_msg(((config_flags%ocean_levels)-(1)+1)*RWORDSIZE,xv)
DO k = 1,config_flags%ocean_levels
grid%om_tmp(pig,k,pjg) = xv(k)
ENDDO
ENDIF
IF ( SIZE(grid%om_s) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_parent_msg(((config_flags%ocean_levels)-(1)+1)*RWORDSIZE,xv)
DO k = 1,config_flags%ocean_levels
grid%om_s(pig,k,pjg) = xv(k)
ENDDO
ENDIF
IF ( SIZE(grid%om_u) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_parent_msg(((config_flags%ocean_levels)-(1)+1)*RWORDSIZE,xv)
DO k = 1,config_flags%ocean_levels
grid%om_u(pig,k,pjg) = xv(k)
ENDDO
ENDIF
IF ( SIZE(grid%om_v) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_parent_msg(((config_flags%ocean_levels)-(1)+1)*RWORDSIZE,xv)
DO k = 1,config_flags%ocean_levels
grid%om_v(pig,k,pjg) = xv(k)
ENDDO
ENDIF
IF ( SIZE(grid%om_ml) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_parent_msg(RWORDSIZE,xv)
grid%om_ml(pig,pjg) = xv(1)
ENDIF
IF ( SIZE(grid%ht_shad) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_parent_msg(RWORDSIZE,xv)
grid%ht_shad(pig,pjg) = xv(1)
ENDIF
IF ( SIZE(grid%ru_tendf_stoch) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_parent_msg((((ckde-1))-(ckds)+1)*RWORDSIZE,xv)
DO k = ckds,(ckde-1)
grid%ru_tendf_stoch(pig,k,pjg) = xv(k)
ENDDO
ENDIF
IF ( SIZE(grid%rv_tendf_stoch) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_parent_msg((((ckde-1))-(ckds)+1)*RWORDSIZE,xv)
DO k = ckds,(ckde-1)
grid%rv_tendf_stoch(pig,k,pjg) = xv(k)
ENDDO
ENDIF
IF ( SIZE(grid%rt_tendf_stoch) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_parent_msg((((ckde-1))-(ckds)+1)*RWORDSIZE,xv)
DO k = ckds,(ckde-1)
grid%rt_tendf_stoch(pig,k,pjg) = xv(k)
ENDDO
ENDIF
IF ( SIZE(grid%field_u_tend_perturb) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_parent_msg((((ckde-1))-(ckds)+1)*RWORDSIZE,xv)
DO k = ckds,(ckde-1)
grid%field_u_tend_perturb(pig,k,pjg) = xv(k)
ENDDO
ENDIF
IF ( SIZE(grid%field_v_tend_perturb) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_parent_msg((((ckde-1))-(ckds)+1)*RWORDSIZE,xv)
DO k = ckds,(ckde-1)
grid%field_v_tend_perturb(pig,k,pjg) = xv(k)
ENDDO
ENDIF
IF ( SIZE(grid%field_t_tend_perturb) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_parent_msg((((ckde-1))-(ckds)+1)*RWORDSIZE,xv)
DO k = ckds,(ckde-1)
grid%field_t_tend_perturb(pig,k,pjg) = xv(k)
ENDDO
ENDIF
DO itrace =  PARAM_FIRST_SCALAR, num_tracer
CALL rsl_lite_from_parent_msg((((ckde-1))-(ckds)+1)*RWORDSIZE,xv)
DO k = ckds,(ckde-1)
tracer(pig,k,pjg,itrace) = xv(k)
ENDDO
ENDDO
CALL rsl_lite_from_parent_info(pig,pjg,retval)
ENDDO
!ENDOFREGISTRYGENERATEDINCLUDE
