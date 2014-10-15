!STARTOFREGISTRYGENERATEDINCLUDE 'inc/nest_forcedown_pack.inc'
!
! WARNING This file is generated automatically by use_registry
! using the data base in the file named Registry.
! Do not edit.  Your changes to this file will be lost.
!
msize = (16 + ((num_moist - PARAM_FIRST_SCALAR + 1)) & 
 + ((num_dfi_moist - PARAM_FIRST_SCALAR + 1)) & 
 + ((num_scalar - PARAM_FIRST_SCALAR + 1)) & 
 + ((num_dfi_scalar - PARAM_FIRST_SCALAR + 1)) & 
 + ((num_aerod - PARAM_FIRST_SCALAR + 1)) & 
 + ((num_ozmixm - PARAM_FIRST_SCALAR + 1)) & 
 + ((num_aerosolc - PARAM_FIRST_SCALAR + 1)) & 
 + ((num_fdda3d - PARAM_FIRST_SCALAR + 1)) & 
 + ((num_fdda2d - PARAM_FIRST_SCALAR + 1)) & 
 + ((num_advh_t - PARAM_FIRST_SCALAR + 1)) & 
 + ((num_advz_t - PARAM_FIRST_SCALAR + 1)) & 
 + ((num_nba_mij - PARAM_FIRST_SCALAR + 1)) & 
 + ((num_nba_rij - PARAM_FIRST_SCALAR + 1)) & 
 + ((num_chem - PARAM_FIRST_SCALAR + 1)) & 
 + ((num_tracer - PARAM_FIRST_SCALAR + 1)) )* nlev + 3
CALL rsl_lite_to_child_info( local_communicator, msize*RWORDSIZE                               &
                        ,cips,cipe,cjps,cjpe                               &
                        ,iids,iide,ijds,ijde                               &
                        ,nids,nide,njds,njde                               &
                        ,pgr , sw                                          &
                        ,ntasks_x,ntasks_y                                 &
                        ,thisdomain_max_halo_width                                  &
                        ,icoord,jcoord                                     &
                        ,idim_cd,jdim_cd                                   &
                        ,pig,pjg,retval )
DO while ( retval .eq. 1 )
IF ( SIZE(grid%u_2) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
DO k = ckds,(ckde-1)
xv(k)= grid%u_2(pig,k,pjg)
ENDDO
CALL rsl_lite_to_child_msg((((ckde-1))-(ckds)+1)*RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%v_2) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
DO k = ckds,(ckde-1)
xv(k)= grid%v_2(pig,k,pjg)
ENDDO
CALL rsl_lite_to_child_msg((((ckde-1))-(ckds)+1)*RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%w_2) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
DO k = ckds,ckde
xv(k)= grid%w_2(pig,k,pjg)
ENDDO
CALL rsl_lite_to_child_msg(((ckde)-(ckds)+1)*RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%ph_2) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
DO k = ckds,ckde
xv(k)= grid%ph_2(pig,k,pjg)
ENDDO
CALL rsl_lite_to_child_msg(((ckde)-(ckds)+1)*RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%t_2) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
DO k = ckds,(ckde-1)
xv(k)= grid%t_2(pig,k,pjg)
ENDDO
CALL rsl_lite_to_child_msg((((ckde-1))-(ckds)+1)*RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%mu_2) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
xv(1)=grid%mu_2(pig,pjg)
CALL rsl_lite_to_child_msg(RWORDSIZE,xv)
ENDIF
DO itrace =  PARAM_FIRST_SCALAR, num_moist
DO k = ckds,(ckde-1)
xv(k)= moist(pig,k,pjg,itrace)
ENDDO
CALL rsl_lite_to_child_msg((((ckde-1))-(ckds)+1)*RWORDSIZE,xv)
ENDDO
DO itrace =  PARAM_FIRST_SCALAR, num_dfi_moist
DO k = ckds,(ckde-1)
xv(k)= dfi_moist(pig,k,pjg,itrace)
ENDDO
CALL rsl_lite_to_child_msg((((ckde-1))-(ckds)+1)*RWORDSIZE,xv)
ENDDO
DO itrace =  PARAM_FIRST_SCALAR, num_scalar
DO k = ckds,(ckde-1)
xv(k)= scalar(pig,k,pjg,itrace)
ENDDO
CALL rsl_lite_to_child_msg((((ckde-1))-(ckds)+1)*RWORDSIZE,xv)
ENDDO
DO itrace =  PARAM_FIRST_SCALAR, num_dfi_scalar
DO k = ckds,(ckde-1)
xv(k)= dfi_scalar(pig,k,pjg,itrace)
ENDDO
CALL rsl_lite_to_child_msg((((ckde-1))-(ckds)+1)*RWORDSIZE,xv)
ENDDO
IF ( SIZE(grid%o3rad) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
DO k = ckds,(ckde-1)
xv(k)= grid%o3rad(pig,k,pjg)
ENDDO
CALL rsl_lite_to_child_msg((((ckde-1))-(ckds)+1)*RWORDSIZE,xv)
ENDIF
DO itrace =  PARAM_FIRST_SCALAR, num_aerod
DO k = ckds,(ckde-1)
xv(k)= aerod(pig,k,pjg,itrace)
ENDDO
CALL rsl_lite_to_child_msg((((ckde-1))-(ckds)+1)*RWORDSIZE,xv)
ENDDO
IF ( SIZE(grid%om_tmp) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
DO k = 1,config_flags%ocean_levels
xv(k)= grid%om_tmp(pig,k,pjg)
ENDDO
CALL rsl_lite_to_child_msg(((config_flags%ocean_levels)-(1)+1)*RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%om_s) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
DO k = 1,config_flags%ocean_levels
xv(k)= grid%om_s(pig,k,pjg)
ENDDO
CALL rsl_lite_to_child_msg(((config_flags%ocean_levels)-(1)+1)*RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%om_u) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
DO k = 1,config_flags%ocean_levels
xv(k)= grid%om_u(pig,k,pjg)
ENDDO
CALL rsl_lite_to_child_msg(((config_flags%ocean_levels)-(1)+1)*RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%om_v) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
DO k = 1,config_flags%ocean_levels
xv(k)= grid%om_v(pig,k,pjg)
ENDDO
CALL rsl_lite_to_child_msg(((config_flags%ocean_levels)-(1)+1)*RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%om_ml) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
xv(1)=grid%om_ml(pig,pjg)
CALL rsl_lite_to_child_msg(RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%ht_shad) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
xv(1)=grid%ht_shad(pig,pjg)
CALL rsl_lite_to_child_msg(RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%ru_tendf_stoch) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
DO k = ckds,(ckde-1)
xv(k)= grid%ru_tendf_stoch(pig,k,pjg)
ENDDO
CALL rsl_lite_to_child_msg((((ckde-1))-(ckds)+1)*RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%rv_tendf_stoch) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
DO k = ckds,(ckde-1)
xv(k)= grid%rv_tendf_stoch(pig,k,pjg)
ENDDO
CALL rsl_lite_to_child_msg((((ckde-1))-(ckds)+1)*RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%rt_tendf_stoch) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
DO k = ckds,(ckde-1)
xv(k)= grid%rt_tendf_stoch(pig,k,pjg)
ENDDO
CALL rsl_lite_to_child_msg((((ckde-1))-(ckds)+1)*RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%field_u_tend_perturb) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
DO k = ckds,(ckde-1)
xv(k)= grid%field_u_tend_perturb(pig,k,pjg)
ENDDO
CALL rsl_lite_to_child_msg((((ckde-1))-(ckds)+1)*RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%field_v_tend_perturb) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
DO k = ckds,(ckde-1)
xv(k)= grid%field_v_tend_perturb(pig,k,pjg)
ENDDO
CALL rsl_lite_to_child_msg((((ckde-1))-(ckds)+1)*RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%field_t_tend_perturb) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
DO k = ckds,(ckde-1)
xv(k)= grid%field_t_tend_perturb(pig,k,pjg)
ENDDO
CALL rsl_lite_to_child_msg((((ckde-1))-(ckds)+1)*RWORDSIZE,xv)
ENDIF
DO itrace =  PARAM_FIRST_SCALAR, num_tracer
DO k = ckds,(ckde-1)
xv(k)= tracer(pig,k,pjg,itrace)
ENDDO
CALL rsl_lite_to_child_msg((((ckde-1))-(ckds)+1)*RWORDSIZE,xv)
ENDDO
CALL rsl_lite_to_child_info( local_communicator, msize*RWORDSIZE                               &
                        ,cips,cipe,cjps,cjpe                               &
                        ,iids,iide,ijds,ijde                               &
                        ,nids,nide,njds,njde                               &
                        ,pgr , sw                                          &
                        ,ntasks_x,ntasks_y                                 &
                        ,thisdomain_max_halo_width                                  &
                        ,icoord,jcoord                                     &
                        ,idim_cd,jdim_cd                                   &
                        ,pig,pjg,retval )
ENDDO
!ENDOFREGISTRYGENERATEDINCLUDE
