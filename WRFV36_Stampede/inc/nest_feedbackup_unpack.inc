!STARTOFREGISTRYGENERATEDINCLUDE 'inc/nest_feedbackup_unpack.inc'
!
! WARNING This file is generated automatically by use_registry
! using the data base in the file named Registry.
! Do not edit.  Your changes to this file will be lost.
!
CALL rsl_lite_from_child_info(pig,pjg,retval)
DO while ( retval .eq. 1 )
IF ( SIZE(grid%xlat) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_child_msg(RWORDSIZE,xv)
IF ( cd_feedback_mask( pig, ips_save, ipe_save , pjg, jps_save, jpe_save, .FALSE., .FALSE. ) ) THEN
NEST_INFLUENCE(grid%xlat(pig,pjg),xv(1))
ENDIF
ENDIF
IF ( SIZE(grid%xlong) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_child_msg(RWORDSIZE,xv)
IF ( cd_feedback_mask( pig, ips_save, ipe_save , pjg, jps_save, jpe_save, .FALSE., .FALSE. ) ) THEN
NEST_INFLUENCE(grid%xlong(pig,pjg),xv(1))
ENDIF
ENDIF
IF ( SIZE(grid%lu_index) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_child_msg(RWORDSIZE,xv)
IF ( cd_feedback_mask( pig, ips_save, ipe_save , pjg, jps_save, jpe_save, .FALSE., .FALSE. ) ) THEN
NEST_INFLUENCE(grid%lu_index(pig,pjg),xv(1))
ENDIF
ENDIF
IF ( SIZE(grid%u_2) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_child_msg((((ckde-1))-(ckds)+1)*RWORDSIZE,xv) ;
IF ( cd_feedback_mask( pig, ips_save, ipe_save , pjg, jps_save, jpe_save, .TRUE., .FALSE. ) ) THEN
DO k = ckds,(ckde-1)
NEST_INFLUENCE(grid%u_2(pig,k,pjg),xv(k))
ENDDO
ENDIF
ENDIF
IF ( SIZE(grid%v_2) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_child_msg((((ckde-1))-(ckds)+1)*RWORDSIZE,xv) ;
IF ( cd_feedback_mask( pig, ips_save, ipe_save , pjg, jps_save, jpe_save, .FALSE., .TRUE. ) ) THEN
DO k = ckds,(ckde-1)
NEST_INFLUENCE(grid%v_2(pig,k,pjg),xv(k))
ENDDO
ENDIF
ENDIF
IF ( SIZE(grid%w_2) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_child_msg(((ckde)-(ckds)+1)*RWORDSIZE,xv) ;
IF ( cd_feedback_mask( pig, ips_save, ipe_save , pjg, jps_save, jpe_save, .FALSE., .FALSE. ) ) THEN
DO k = ckds,ckde
NEST_INFLUENCE(grid%w_2(pig,k,pjg),xv(k))
ENDDO
ENDIF
ENDIF
IF ( SIZE(grid%ph_2) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_child_msg(((ckde)-(ckds)+1)*RWORDSIZE,xv) ;
IF ( cd_feedback_mask( pig, ips_save, ipe_save , pjg, jps_save, jpe_save, .FALSE., .FALSE. ) ) THEN
DO k = ckds,ckde
NEST_INFLUENCE(grid%ph_2(pig,k,pjg),xv(k))
ENDDO
ENDIF
ENDIF
IF ( SIZE(grid%phb) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_child_msg(((ckde)-(ckds)+1)*RWORDSIZE,xv) ;
IF ( cd_feedback_mask( pig, ips_save, ipe_save , pjg, jps_save, jpe_save, .FALSE., .FALSE. ) ) THEN
DO k = ckds,ckde
NEST_INFLUENCE(grid%phb(pig,k,pjg),xv(k))
ENDDO
ENDIF
ENDIF
IF ( SIZE(grid%t_2) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_child_msg((((ckde-1))-(ckds)+1)*RWORDSIZE,xv) ;
IF ( cd_feedback_mask( pig, ips_save, ipe_save , pjg, jps_save, jpe_save, .FALSE., .FALSE. ) ) THEN
DO k = ckds,(ckde-1)
NEST_INFLUENCE(grid%t_2(pig,k,pjg),xv(k))
ENDDO
ENDIF
ENDIF
IF ( SIZE(grid%mu_2) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_child_msg(RWORDSIZE,xv)
IF ( cd_feedback_mask( pig, ips_save, ipe_save , pjg, jps_save, jpe_save, .FALSE., .FALSE. ) ) THEN
NEST_INFLUENCE(grid%mu_2(pig,pjg),xv(1))
ENDIF
ENDIF
IF ( SIZE(grid%mub) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_child_msg(RWORDSIZE,xv)
IF ( cd_feedback_mask( pig, ips_save, ipe_save , pjg, jps_save, jpe_save, .FALSE., .FALSE. ) ) THEN
NEST_INFLUENCE(grid%mub(pig,pjg),xv(1))
ENDIF
ENDIF
IF ( SIZE(grid%nest_pos) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_child_msg(RWORDSIZE,xv)
IF ( cd_feedback_mask( pig, ips_save, ipe_save , pjg, jps_save, jpe_save, .FALSE., .FALSE. ) ) THEN
NEST_INFLUENCE(grid%nest_pos(pig,pjg),xv(1))
ENDIF
ENDIF
IF ( SIZE(grid%nest_mask) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_child_msg(RWORDSIZE,xv)
IF ( cd_feedback_mask( pig, ips_save, ipe_save , pjg, jps_save, jpe_save, .FALSE., .FALSE. ) ) THEN
NEST_INFLUENCE(grid%nest_mask(pig,pjg),xv(1))
ENDIF
ENDIF
IF ( SIZE(grid%alb) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_child_msg((((ckde-1))-(ckds)+1)*RWORDSIZE,xv) ;
IF ( cd_feedback_mask( pig, ips_save, ipe_save , pjg, jps_save, jpe_save, .FALSE., .FALSE. ) ) THEN
DO k = ckds,(ckde-1)
NEST_INFLUENCE(grid%alb(pig,k,pjg),xv(k))
ENDDO
ENDIF
ENDIF
IF ( SIZE(grid%pb) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_child_msg((((ckde-1))-(ckds)+1)*RWORDSIZE,xv) ;
IF ( cd_feedback_mask( pig, ips_save, ipe_save , pjg, jps_save, jpe_save, .FALSE., .FALSE. ) ) THEN
DO k = ckds,(ckde-1)
NEST_INFLUENCE(grid%pb(pig,k,pjg),xv(k))
ENDDO
ENDIF
ENDIF
IF ( SIZE(grid%q2) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_child_msg(RWORDSIZE,xv)
IF ( cd_feedback_mask( pig, ips_save, ipe_save , pjg, jps_save, jpe_save, .FALSE., .FALSE. ) ) THEN
NEST_INFLUENCE(grid%q2(pig,pjg),xv(1))
ENDIF
ENDIF
IF ( SIZE(grid%t2) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_child_msg(RWORDSIZE,xv)
IF ( cd_feedback_mask( pig, ips_save, ipe_save , pjg, jps_save, jpe_save, .FALSE., .FALSE. ) ) THEN
NEST_INFLUENCE(grid%t2(pig,pjg),xv(1))
ENDIF
ENDIF
IF ( SIZE(grid%th2) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_child_msg(RWORDSIZE,xv)
IF ( cd_feedback_mask( pig, ips_save, ipe_save , pjg, jps_save, jpe_save, .FALSE., .FALSE. ) ) THEN
NEST_INFLUENCE(grid%th2(pig,pjg),xv(1))
ENDIF
ENDIF
IF ( SIZE(grid%psfc) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_child_msg(RWORDSIZE,xv)
IF ( cd_feedback_mask( pig, ips_save, ipe_save , pjg, jps_save, jpe_save, .FALSE., .FALSE. ) ) THEN
NEST_INFLUENCE(grid%psfc(pig,pjg),xv(1))
ENDIF
ENDIF
IF ( SIZE(grid%u10) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_child_msg(RWORDSIZE,xv)
IF ( cd_feedback_mask( pig, ips_save, ipe_save , pjg, jps_save, jpe_save, .FALSE., .FALSE. ) ) THEN
NEST_INFLUENCE(grid%u10(pig,pjg),xv(1))
ENDIF
ENDIF
IF ( SIZE(grid%v10) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_child_msg(RWORDSIZE,xv)
IF ( cd_feedback_mask( pig, ips_save, ipe_save , pjg, jps_save, jpe_save, .FALSE., .FALSE. ) ) THEN
NEST_INFLUENCE(grid%v10(pig,pjg),xv(1))
ENDIF
ENDIF
DO itrace =  PARAM_FIRST_SCALAR, num_moist
CALL rsl_lite_from_child_msg((((ckde-1))-(ckds)+1)*RWORDSIZE,xv) ;
IF ( cd_feedback_mask( pig, ips_save, ipe_save , pjg, jps_save, jpe_save, .FALSE., .FALSE. ) ) THEN
DO k = ckds,(ckde-1)
NEST_INFLUENCE(moist(pig,k,pjg,itrace),xv(k))
ENDDO
ENDIF
ENDDO
DO itrace =  PARAM_FIRST_SCALAR, num_dfi_moist
CALL rsl_lite_from_child_msg((((ckde-1))-(ckds)+1)*RWORDSIZE,xv) ;
IF ( cd_feedback_mask( pig, ips_save, ipe_save , pjg, jps_save, jpe_save, .FALSE., .FALSE. ) ) THEN
DO k = ckds,(ckde-1)
NEST_INFLUENCE(dfi_moist(pig,k,pjg,itrace),xv(k))
ENDDO
ENDIF
ENDDO
IF ( SIZE(grid%qnwfa2d) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_child_msg(RWORDSIZE,xv)
IF ( cd_feedback_mask( pig, ips_save, ipe_save , pjg, jps_save, jpe_save, .FALSE., .FALSE. ) ) THEN
NEST_INFLUENCE(grid%qnwfa2d(pig,pjg),xv(1))
ENDIF
ENDIF
DO itrace =  PARAM_FIRST_SCALAR, num_scalar
CALL rsl_lite_from_child_msg((((ckde-1))-(ckds)+1)*RWORDSIZE,xv) ;
IF ( cd_feedback_mask( pig, ips_save, ipe_save , pjg, jps_save, jpe_save, .FALSE., .FALSE. ) ) THEN
DO k = ckds,(ckde-1)
NEST_INFLUENCE(scalar(pig,k,pjg,itrace),xv(k))
ENDDO
ENDIF
ENDDO
DO itrace =  PARAM_FIRST_SCALAR, num_dfi_scalar
CALL rsl_lite_from_child_msg((((ckde-1))-(ckds)+1)*RWORDSIZE,xv) ;
IF ( cd_feedback_mask( pig, ips_save, ipe_save , pjg, jps_save, jpe_save, .FALSE., .FALSE. ) ) THEN
DO k = ckds,(ckde-1)
NEST_INFLUENCE(dfi_scalar(pig,k,pjg,itrace),xv(k))
ENDDO
ENDIF
ENDDO
IF ( SIZE(grid%toposlpx) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_child_msg(RWORDSIZE,xv)
IF ( cd_feedback_mask( pig, ips_save, ipe_save , pjg, jps_save, jpe_save, .FALSE., .FALSE. ) ) THEN
NEST_INFLUENCE(grid%toposlpx(pig,pjg),xv(1))
ENDIF
ENDIF
IF ( SIZE(grid%toposlpy) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_child_msg(RWORDSIZE,xv)
IF ( cd_feedback_mask( pig, ips_save, ipe_save , pjg, jps_save, jpe_save, .FALSE., .FALSE. ) ) THEN
NEST_INFLUENCE(grid%toposlpy(pig,pjg),xv(1))
ENDIF
ENDIF
IF ( SIZE(grid%slope) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_child_msg(RWORDSIZE,xv)
IF ( cd_feedback_mask( pig, ips_save, ipe_save , pjg, jps_save, jpe_save, .FALSE., .FALSE. ) ) THEN
NEST_INFLUENCE(grid%slope(pig,pjg),xv(1))
ENDIF
ENDIF
IF ( SIZE(grid%slp_azi) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_child_msg(RWORDSIZE,xv)
IF ( cd_feedback_mask( pig, ips_save, ipe_save , pjg, jps_save, jpe_save, .FALSE., .FALSE. ) ) THEN
NEST_INFLUENCE(grid%slp_azi(pig,pjg),xv(1))
ENDIF
ENDIF
IF ( SIZE(grid%shdmax) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_child_msg(RWORDSIZE,xv)
IF ( cd_feedback_mask( pig, ips_save, ipe_save , pjg, jps_save, jpe_save, .FALSE., .FALSE. ) ) THEN
NEST_INFLUENCE(grid%shdmax(pig,pjg),xv(1))
ENDIF
ENDIF
IF ( SIZE(grid%shdmin) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_child_msg(RWORDSIZE,xv)
IF ( cd_feedback_mask( pig, ips_save, ipe_save , pjg, jps_save, jpe_save, .FALSE., .FALSE. ) ) THEN
NEST_INFLUENCE(grid%shdmin(pig,pjg),xv(1))
ENDIF
ENDIF
IF ( SIZE(grid%tslb) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_child_msg(((config_flags%num_soil_layers)-(1)+1)*RWORDSIZE,xv) ;
IF ( cd_feedback_mask( pig, ips_save, ipe_save , pjg, jps_save, jpe_save, .FALSE., .FALSE. ) ) THEN
DO k = 1,config_flags%num_soil_layers
NEST_INFLUENCE(grid%tslb(pig,k,pjg),xv(k))
ENDDO
ENDIF
ENDIF
IF ( SIZE(grid%urb_param) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_child_msg(((132)-(1)+1)*RWORDSIZE,xv) ;
IF ( cd_feedback_mask( pig, ips_save, ipe_save , pjg, jps_save, jpe_save, .FALSE., .FALSE. ) ) THEN
DO k = 1,132
NEST_INFLUENCE(grid%urb_param(pig,k,pjg),xv(k))
ENDDO
ENDIF
ENDIF
IF ( SIZE(grid%smois) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_child_msg(((config_flags%num_soil_layers)-(1)+1)*RWORDSIZE,xv) ;
IF ( cd_feedback_mask( pig, ips_save, ipe_save , pjg, jps_save, jpe_save, .FALSE., .FALSE. ) ) THEN
DO k = 1,config_flags%num_soil_layers
NEST_INFLUENCE(grid%smois(pig,k,pjg),xv(k))
ENDDO
ENDIF
ENDIF
IF ( SIZE(grid%sh2o) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_child_msg(((config_flags%num_soil_layers)-(1)+1)*RWORDSIZE,xv) ;
IF ( cd_feedback_mask( pig, ips_save, ipe_save , pjg, jps_save, jpe_save, .FALSE., .FALSE. ) ) THEN
DO k = 1,config_flags%num_soil_layers
NEST_INFLUENCE(grid%sh2o(pig,k,pjg),xv(k))
ENDDO
ENDIF
ENDIF
IF ( SIZE(grid%smcrel) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_child_msg(((config_flags%num_soil_layers)-(1)+1)*RWORDSIZE,xv) ;
IF ( cd_feedback_mask( pig, ips_save, ipe_save , pjg, jps_save, jpe_save, .FALSE., .FALSE. ) ) THEN
DO k = 1,config_flags%num_soil_layers
NEST_INFLUENCE(grid%smcrel(pig,k,pjg),xv(k))
ENDDO
ENDIF
ENDIF
IF ( SIZE(grid%xice) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_child_msg(RWORDSIZE,xv)
IF ( cd_feedback_mask( pig, ips_save, ipe_save , pjg, jps_save, jpe_save, .FALSE., .FALSE. ) ) THEN
NEST_INFLUENCE(grid%xice(pig,pjg),xv(1))
ENDIF
ENDIF
IF ( SIZE(grid%icedepth) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_child_msg(RWORDSIZE,xv)
IF ( cd_feedback_mask( pig, ips_save, ipe_save , pjg, jps_save, jpe_save, .FALSE., .FALSE. ) ) THEN
NEST_INFLUENCE(grid%icedepth(pig,pjg),xv(1))
ENDIF
ENDIF
IF ( SIZE(grid%xicem) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_child_msg(RWORDSIZE,xv)
IF ( cd_feedback_mask( pig, ips_save, ipe_save , pjg, jps_save, jpe_save, .FALSE., .FALSE. ) ) THEN
NEST_INFLUENCE(grid%xicem(pig,pjg),xv(1))
ENDIF
ENDIF
IF ( SIZE(grid%albsi) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_child_msg(RWORDSIZE,xv)
IF ( cd_feedback_mask( pig, ips_save, ipe_save , pjg, jps_save, jpe_save, .FALSE., .FALSE. ) ) THEN
NEST_INFLUENCE(grid%albsi(pig,pjg),xv(1))
ENDIF
ENDIF
IF ( SIZE(grid%snowsi) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_child_msg(RWORDSIZE,xv)
IF ( cd_feedback_mask( pig, ips_save, ipe_save , pjg, jps_save, jpe_save, .FALSE., .FALSE. ) ) THEN
NEST_INFLUENCE(grid%snowsi(pig,pjg),xv(1))
ENDIF
ENDIF
IF ( SIZE(grid%ivgtyp) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_child_msg(RWORDSIZE,xv)
IF ( cd_feedback_mask( pig, ips_save, ipe_save , pjg, jps_save, jpe_save, .FALSE., .FALSE. ) ) THEN
NEST_INFLUENCE(grid%ivgtyp(pig,pjg),xv(1))
ENDIF
ENDIF
IF ( SIZE(grid%isltyp) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_child_msg(RWORDSIZE,xv)
IF ( cd_feedback_mask( pig, ips_save, ipe_save , pjg, jps_save, jpe_save, .FALSE., .FALSE. ) ) THEN
NEST_INFLUENCE(grid%isltyp(pig,pjg),xv(1))
ENDIF
ENDIF
IF ( SIZE(grid%vegfra) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_child_msg(RWORDSIZE,xv)
IF ( cd_feedback_mask( pig, ips_save, ipe_save , pjg, jps_save, jpe_save, .FALSE., .FALSE. ) ) THEN
NEST_INFLUENCE(grid%vegfra(pig,pjg),xv(1))
ENDIF
ENDIF
IF ( SIZE(grid%acgrdflx) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_child_msg(RWORDSIZE,xv)
IF ( cd_feedback_mask( pig, ips_save, ipe_save , pjg, jps_save, jpe_save, .FALSE., .FALSE. ) ) THEN
NEST_INFLUENCE(grid%acgrdflx(pig,pjg),xv(1))
ENDIF
ENDIF
IF ( SIZE(grid%acsnow) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_child_msg(RWORDSIZE,xv)
IF ( cd_feedback_mask( pig, ips_save, ipe_save , pjg, jps_save, jpe_save, .FALSE., .FALSE. ) ) THEN
NEST_INFLUENCE(grid%acsnow(pig,pjg),xv(1))
ENDIF
ENDIF
IF ( SIZE(grid%acsnom) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_child_msg(RWORDSIZE,xv)
IF ( cd_feedback_mask( pig, ips_save, ipe_save , pjg, jps_save, jpe_save, .FALSE., .FALSE. ) ) THEN
NEST_INFLUENCE(grid%acsnom(pig,pjg),xv(1))
ENDIF
ENDIF
IF ( SIZE(grid%snow) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_child_msg(RWORDSIZE,xv)
IF ( cd_feedback_mask( pig, ips_save, ipe_save , pjg, jps_save, jpe_save, .FALSE., .FALSE. ) ) THEN
NEST_INFLUENCE(grid%snow(pig,pjg),xv(1))
ENDIF
ENDIF
IF ( SIZE(grid%snowh) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_child_msg(RWORDSIZE,xv)
IF ( cd_feedback_mask( pig, ips_save, ipe_save , pjg, jps_save, jpe_save, .FALSE., .FALSE. ) ) THEN
NEST_INFLUENCE(grid%snowh(pig,pjg),xv(1))
ENDIF
ENDIF
IF ( SIZE(grid%canwat) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_child_msg(RWORDSIZE,xv)
IF ( cd_feedback_mask( pig, ips_save, ipe_save , pjg, jps_save, jpe_save, .FALSE., .FALSE. ) ) THEN
NEST_INFLUENCE(grid%canwat(pig,pjg),xv(1))
ENDIF
ENDIF
IF ( SIZE(grid%tr_urb2d) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_child_msg(RWORDSIZE,xv)
IF ( cd_feedback_mask( pig, ips_save, ipe_save , pjg, jps_save, jpe_save, .FALSE., .FALSE. ) ) THEN
NEST_INFLUENCE(grid%tr_urb2d(pig,pjg),xv(1))
ENDIF
ENDIF
IF ( SIZE(grid%tb_urb2d) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_child_msg(RWORDSIZE,xv)
IF ( cd_feedback_mask( pig, ips_save, ipe_save , pjg, jps_save, jpe_save, .FALSE., .FALSE. ) ) THEN
NEST_INFLUENCE(grid%tb_urb2d(pig,pjg),xv(1))
ENDIF
ENDIF
IF ( SIZE(grid%tg_urb2d) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_child_msg(RWORDSIZE,xv)
IF ( cd_feedback_mask( pig, ips_save, ipe_save , pjg, jps_save, jpe_save, .FALSE., .FALSE. ) ) THEN
NEST_INFLUENCE(grid%tg_urb2d(pig,pjg),xv(1))
ENDIF
ENDIF
IF ( SIZE(grid%tc_urb2d) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_child_msg(RWORDSIZE,xv)
IF ( cd_feedback_mask( pig, ips_save, ipe_save , pjg, jps_save, jpe_save, .FALSE., .FALSE. ) ) THEN
NEST_INFLUENCE(grid%tc_urb2d(pig,pjg),xv(1))
ENDIF
ENDIF
IF ( SIZE(grid%qc_urb2d) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_child_msg(RWORDSIZE,xv)
IF ( cd_feedback_mask( pig, ips_save, ipe_save , pjg, jps_save, jpe_save, .FALSE., .FALSE. ) ) THEN
NEST_INFLUENCE(grid%qc_urb2d(pig,pjg),xv(1))
ENDIF
ENDIF
IF ( SIZE(grid%uc_urb2d) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_child_msg(RWORDSIZE,xv)
IF ( cd_feedback_mask( pig, ips_save, ipe_save , pjg, jps_save, jpe_save, .FALSE., .FALSE. ) ) THEN
NEST_INFLUENCE(grid%uc_urb2d(pig,pjg),xv(1))
ENDIF
ENDIF
IF ( SIZE(grid%xxxr_urb2d) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_child_msg(RWORDSIZE,xv)
IF ( cd_feedback_mask( pig, ips_save, ipe_save , pjg, jps_save, jpe_save, .FALSE., .FALSE. ) ) THEN
NEST_INFLUENCE(grid%xxxr_urb2d(pig,pjg),xv(1))
ENDIF
ENDIF
IF ( SIZE(grid%xxxb_urb2d) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_child_msg(RWORDSIZE,xv)
IF ( cd_feedback_mask( pig, ips_save, ipe_save , pjg, jps_save, jpe_save, .FALSE., .FALSE. ) ) THEN
NEST_INFLUENCE(grid%xxxb_urb2d(pig,pjg),xv(1))
ENDIF
ENDIF
IF ( SIZE(grid%xxxg_urb2d) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_child_msg(RWORDSIZE,xv)
IF ( cd_feedback_mask( pig, ips_save, ipe_save , pjg, jps_save, jpe_save, .FALSE., .FALSE. ) ) THEN
NEST_INFLUENCE(grid%xxxg_urb2d(pig,pjg),xv(1))
ENDIF
ENDIF
IF ( SIZE(grid%xxxc_urb2d) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_child_msg(RWORDSIZE,xv)
IF ( cd_feedback_mask( pig, ips_save, ipe_save , pjg, jps_save, jpe_save, .FALSE., .FALSE. ) ) THEN
NEST_INFLUENCE(grid%xxxc_urb2d(pig,pjg),xv(1))
ENDIF
ENDIF
IF ( SIZE(grid%trl_urb3d) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_child_msg(((config_flags%num_soil_layers)-(1)+1)*RWORDSIZE,xv) ;
IF ( cd_feedback_mask( pig, ips_save, ipe_save , pjg, jps_save, jpe_save, .FALSE., .FALSE. ) ) THEN
DO k = 1,config_flags%num_soil_layers
NEST_INFLUENCE(grid%trl_urb3d(pig,k,pjg),xv(k))
ENDDO
ENDIF
ENDIF
IF ( SIZE(grid%tbl_urb3d) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_child_msg(((config_flags%num_soil_layers)-(1)+1)*RWORDSIZE,xv) ;
IF ( cd_feedback_mask( pig, ips_save, ipe_save , pjg, jps_save, jpe_save, .FALSE., .FALSE. ) ) THEN
DO k = 1,config_flags%num_soil_layers
NEST_INFLUENCE(grid%tbl_urb3d(pig,k,pjg),xv(k))
ENDDO
ENDIF
ENDIF
IF ( SIZE(grid%tgl_urb3d) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_child_msg(((config_flags%num_soil_layers)-(1)+1)*RWORDSIZE,xv) ;
IF ( cd_feedback_mask( pig, ips_save, ipe_save , pjg, jps_save, jpe_save, .FALSE., .FALSE. ) ) THEN
DO k = 1,config_flags%num_soil_layers
NEST_INFLUENCE(grid%tgl_urb3d(pig,k,pjg),xv(k))
ENDDO
ENDIF
ENDIF
IF ( SIZE(grid%frc_urb2d) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_child_msg(RWORDSIZE,xv)
IF ( cd_feedback_mask( pig, ips_save, ipe_save , pjg, jps_save, jpe_save, .FALSE., .FALSE. ) ) THEN
NEST_INFLUENCE(grid%frc_urb2d(pig,pjg),xv(1))
ENDIF
ENDIF
IF ( SIZE(grid%utype_urb2d) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_child_msg(RWORDSIZE,xv)
IF ( cd_feedback_mask( pig, ips_save, ipe_save , pjg, jps_save, jpe_save, .FALSE., .FALSE. ) ) THEN
NEST_INFLUENCE(grid%utype_urb2d(pig,pjg),xv(1))
ENDIF
ENDIF
IF ( SIZE(grid%var2d) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_child_msg(RWORDSIZE,xv)
IF ( cd_feedback_mask( pig, ips_save, ipe_save , pjg, jps_save, jpe_save, .FALSE., .FALSE. ) ) THEN
NEST_INFLUENCE(grid%var2d(pig,pjg),xv(1))
ENDIF
ENDIF
IF ( SIZE(grid%oc12d) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_child_msg(RWORDSIZE,xv)
IF ( cd_feedback_mask( pig, ips_save, ipe_save , pjg, jps_save, jpe_save, .FALSE., .FALSE. ) ) THEN
NEST_INFLUENCE(grid%oc12d(pig,pjg),xv(1))
ENDIF
ENDIF
IF ( SIZE(grid%oa1) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_child_msg(RWORDSIZE,xv)
IF ( cd_feedback_mask( pig, ips_save, ipe_save , pjg, jps_save, jpe_save, .FALSE., .FALSE. ) ) THEN
NEST_INFLUENCE(grid%oa1(pig,pjg),xv(1))
ENDIF
ENDIF
IF ( SIZE(grid%oa2) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_child_msg(RWORDSIZE,xv)
IF ( cd_feedback_mask( pig, ips_save, ipe_save , pjg, jps_save, jpe_save, .FALSE., .FALSE. ) ) THEN
NEST_INFLUENCE(grid%oa2(pig,pjg),xv(1))
ENDIF
ENDIF
IF ( SIZE(grid%oa3) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_child_msg(RWORDSIZE,xv)
IF ( cd_feedback_mask( pig, ips_save, ipe_save , pjg, jps_save, jpe_save, .FALSE., .FALSE. ) ) THEN
NEST_INFLUENCE(grid%oa3(pig,pjg),xv(1))
ENDIF
ENDIF
IF ( SIZE(grid%oa4) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_child_msg(RWORDSIZE,xv)
IF ( cd_feedback_mask( pig, ips_save, ipe_save , pjg, jps_save, jpe_save, .FALSE., .FALSE. ) ) THEN
NEST_INFLUENCE(grid%oa4(pig,pjg),xv(1))
ENDIF
ENDIF
IF ( SIZE(grid%ol1) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_child_msg(RWORDSIZE,xv)
IF ( cd_feedback_mask( pig, ips_save, ipe_save , pjg, jps_save, jpe_save, .FALSE., .FALSE. ) ) THEN
NEST_INFLUENCE(grid%ol1(pig,pjg),xv(1))
ENDIF
ENDIF
IF ( SIZE(grid%ol2) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_child_msg(RWORDSIZE,xv)
IF ( cd_feedback_mask( pig, ips_save, ipe_save , pjg, jps_save, jpe_save, .FALSE., .FALSE. ) ) THEN
NEST_INFLUENCE(grid%ol2(pig,pjg),xv(1))
ENDIF
ENDIF
IF ( SIZE(grid%ol3) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_child_msg(RWORDSIZE,xv)
IF ( cd_feedback_mask( pig, ips_save, ipe_save , pjg, jps_save, jpe_save, .FALSE., .FALSE. ) ) THEN
NEST_INFLUENCE(grid%ol3(pig,pjg),xv(1))
ENDIF
ENDIF
IF ( SIZE(grid%ol4) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_child_msg(RWORDSIZE,xv)
IF ( cd_feedback_mask( pig, ips_save, ipe_save , pjg, jps_save, jpe_save, .FALSE., .FALSE. ) ) THEN
NEST_INFLUENCE(grid%ol4(pig,pjg),xv(1))
ENDIF
ENDIF
IF ( SIZE(grid%ctopo) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_child_msg(RWORDSIZE,xv)
IF ( cd_feedback_mask( pig, ips_save, ipe_save , pjg, jps_save, jpe_save, .FALSE., .FALSE. ) ) THEN
NEST_INFLUENCE(grid%ctopo(pig,pjg),xv(1))
ENDIF
ENDIF
IF ( SIZE(grid%ctopo2) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_child_msg(RWORDSIZE,xv)
IF ( cd_feedback_mask( pig, ips_save, ipe_save , pjg, jps_save, jpe_save, .FALSE., .FALSE. ) ) THEN
NEST_INFLUENCE(grid%ctopo2(pig,pjg),xv(1))
ENDIF
ENDIF
IF ( SIZE(grid%f_ice_phy) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_child_msg((((ckde-1))-(ckds)+1)*RWORDSIZE,xv) ;
IF ( cd_feedback_mask( pig, ips_save, ipe_save , pjg, jps_save, jpe_save, .FALSE., .FALSE. ) ) THEN
DO k = ckds,(ckde-1)
NEST_INFLUENCE(grid%f_ice_phy(pig,k,pjg),xv(k))
ENDDO
ENDIF
ENDIF
IF ( SIZE(grid%f_rain_phy) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_child_msg((((ckde-1))-(ckds)+1)*RWORDSIZE,xv) ;
IF ( cd_feedback_mask( pig, ips_save, ipe_save , pjg, jps_save, jpe_save, .FALSE., .FALSE. ) ) THEN
DO k = ckds,(ckde-1)
NEST_INFLUENCE(grid%f_rain_phy(pig,k,pjg),xv(k))
ENDDO
ENDIF
ENDIF
IF ( SIZE(grid%f_rimef_phy) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_child_msg((((ckde-1))-(ckds)+1)*RWORDSIZE,xv) ;
IF ( cd_feedback_mask( pig, ips_save, ipe_save , pjg, jps_save, jpe_save, .FALSE., .FALSE. ) ) THEN
DO k = ckds,(ckde-1)
NEST_INFLUENCE(grid%f_rimef_phy(pig,k,pjg),xv(k))
ENDDO
ENDIF
ENDIF
IF ( SIZE(grid%om_tmp) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_child_msg(((config_flags%ocean_levels)-(1)+1)*RWORDSIZE,xv) ;
IF ( cd_feedback_mask( pig, ips_save, ipe_save , pjg, jps_save, jpe_save, .FALSE., .FALSE. ) ) THEN
DO k = 1,config_flags%ocean_levels
NEST_INFLUENCE(grid%om_tmp(pig,k,pjg),xv(k))
ENDDO
ENDIF
ENDIF
IF ( SIZE(grid%om_s) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_child_msg(((config_flags%ocean_levels)-(1)+1)*RWORDSIZE,xv) ;
IF ( cd_feedback_mask( pig, ips_save, ipe_save , pjg, jps_save, jpe_save, .FALSE., .FALSE. ) ) THEN
DO k = 1,config_flags%ocean_levels
NEST_INFLUENCE(grid%om_s(pig,k,pjg),xv(k))
ENDDO
ENDIF
ENDIF
IF ( SIZE(grid%om_u) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_child_msg(((config_flags%ocean_levels)-(1)+1)*RWORDSIZE,xv) ;
IF ( cd_feedback_mask( pig, ips_save, ipe_save , pjg, jps_save, jpe_save, .FALSE., .FALSE. ) ) THEN
DO k = 1,config_flags%ocean_levels
NEST_INFLUENCE(grid%om_u(pig,k,pjg),xv(k))
ENDDO
ENDIF
ENDIF
IF ( SIZE(grid%om_v) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_child_msg(((config_flags%ocean_levels)-(1)+1)*RWORDSIZE,xv) ;
IF ( cd_feedback_mask( pig, ips_save, ipe_save , pjg, jps_save, jpe_save, .FALSE., .FALSE. ) ) THEN
DO k = 1,config_flags%ocean_levels
NEST_INFLUENCE(grid%om_v(pig,k,pjg),xv(k))
ENDDO
ENDIF
ENDIF
IF ( SIZE(grid%om_ml) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_child_msg(RWORDSIZE,xv)
IF ( cd_feedback_mask( pig, ips_save, ipe_save , pjg, jps_save, jpe_save, .FALSE., .FALSE. ) ) THEN
NEST_INFLUENCE(grid%om_ml(pig,pjg),xv(1))
ENDIF
ENDIF
IF ( SIZE(grid%h_diabatic) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_child_msg((((ckde-1))-(ckds)+1)*RWORDSIZE,xv) ;
IF ( cd_feedback_mask( pig, ips_save, ipe_save , pjg, jps_save, jpe_save, .FALSE., .FALSE. ) ) THEN
DO k = ckds,(ckde-1)
NEST_INFLUENCE(grid%h_diabatic(pig,k,pjg),xv(k))
ENDDO
ENDIF
ENDIF
IF ( SIZE(grid%msft) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_child_msg(RWORDSIZE,xv)
IF ( cd_feedback_mask( pig, ips_save, ipe_save , pjg, jps_save, jpe_save, .FALSE., .FALSE. ) ) THEN
NEST_INFLUENCE(grid%msft(pig,pjg),xv(1))
ENDIF
ENDIF
IF ( SIZE(grid%msfu) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_child_msg(RWORDSIZE,xv)
IF ( cd_feedback_mask( pig, ips_save, ipe_save , pjg, jps_save, jpe_save, .TRUE., .FALSE. ) ) THEN
NEST_INFLUENCE(grid%msfu(pig,pjg),xv(1))
ENDIF
ENDIF
IF ( SIZE(grid%msfv) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_child_msg(RWORDSIZE,xv)
IF ( cd_feedback_mask( pig, ips_save, ipe_save , pjg, jps_save, jpe_save, .FALSE., .TRUE. ) ) THEN
NEST_INFLUENCE(grid%msfv(pig,pjg),xv(1))
ENDIF
ENDIF
IF ( SIZE(grid%msftx) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_child_msg(RWORDSIZE,xv)
IF ( cd_feedback_mask( pig, ips_save, ipe_save , pjg, jps_save, jpe_save, .FALSE., .FALSE. ) ) THEN
NEST_INFLUENCE(grid%msftx(pig,pjg),xv(1))
ENDIF
ENDIF
IF ( SIZE(grid%msfty) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_child_msg(RWORDSIZE,xv)
IF ( cd_feedback_mask( pig, ips_save, ipe_save , pjg, jps_save, jpe_save, .FALSE., .FALSE. ) ) THEN
NEST_INFLUENCE(grid%msfty(pig,pjg),xv(1))
ENDIF
ENDIF
IF ( SIZE(grid%msfux) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_child_msg(RWORDSIZE,xv)
IF ( cd_feedback_mask( pig, ips_save, ipe_save , pjg, jps_save, jpe_save, .TRUE., .FALSE. ) ) THEN
NEST_INFLUENCE(grid%msfux(pig,pjg),xv(1))
ENDIF
ENDIF
IF ( SIZE(grid%msfuy) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_child_msg(RWORDSIZE,xv)
IF ( cd_feedback_mask( pig, ips_save, ipe_save , pjg, jps_save, jpe_save, .TRUE., .FALSE. ) ) THEN
NEST_INFLUENCE(grid%msfuy(pig,pjg),xv(1))
ENDIF
ENDIF
IF ( SIZE(grid%msfvx) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_child_msg(RWORDSIZE,xv)
IF ( cd_feedback_mask( pig, ips_save, ipe_save , pjg, jps_save, jpe_save, .FALSE., .TRUE. ) ) THEN
NEST_INFLUENCE(grid%msfvx(pig,pjg),xv(1))
ENDIF
ENDIF
IF ( SIZE(grid%msfvx_inv) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_child_msg(RWORDSIZE,xv)
IF ( cd_feedback_mask( pig, ips_save, ipe_save , pjg, jps_save, jpe_save, .FALSE., .TRUE. ) ) THEN
NEST_INFLUENCE(grid%msfvx_inv(pig,pjg),xv(1))
ENDIF
ENDIF
IF ( SIZE(grid%msfvy) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_child_msg(RWORDSIZE,xv)
IF ( cd_feedback_mask( pig, ips_save, ipe_save , pjg, jps_save, jpe_save, .FALSE., .TRUE. ) ) THEN
NEST_INFLUENCE(grid%msfvy(pig,pjg),xv(1))
ENDIF
ENDIF
IF ( SIZE(grid%f) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_child_msg(RWORDSIZE,xv)
IF ( cd_feedback_mask( pig, ips_save, ipe_save , pjg, jps_save, jpe_save, .FALSE., .FALSE. ) ) THEN
NEST_INFLUENCE(grid%f(pig,pjg),xv(1))
ENDIF
ENDIF
IF ( SIZE(grid%e) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_child_msg(RWORDSIZE,xv)
IF ( cd_feedback_mask( pig, ips_save, ipe_save , pjg, jps_save, jpe_save, .FALSE., .FALSE. ) ) THEN
NEST_INFLUENCE(grid%e(pig,pjg),xv(1))
ENDIF
ENDIF
IF ( SIZE(grid%sina) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_child_msg(RWORDSIZE,xv)
IF ( cd_feedback_mask( pig, ips_save, ipe_save , pjg, jps_save, jpe_save, .FALSE., .FALSE. ) ) THEN
NEST_INFLUENCE(grid%sina(pig,pjg),xv(1))
ENDIF
ENDIF
IF ( SIZE(grid%cosa) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_child_msg(RWORDSIZE,xv)
IF ( cd_feedback_mask( pig, ips_save, ipe_save , pjg, jps_save, jpe_save, .FALSE., .FALSE. ) ) THEN
NEST_INFLUENCE(grid%cosa(pig,pjg),xv(1))
ENDIF
ENDIF
IF ( SIZE(grid%ht) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_child_msg(RWORDSIZE,xv)
IF ( cd_feedback_mask( pig, ips_save, ipe_save , pjg, jps_save, jpe_save, .FALSE., .FALSE. ) ) THEN
NEST_INFLUENCE(grid%ht(pig,pjg),xv(1))
ENDIF
ENDIF
IF ( SIZE(grid%tsk) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_child_msg(RWORDSIZE,xv)
IF ( cd_feedback_mask( pig, ips_save, ipe_save , pjg, jps_save, jpe_save, .FALSE., .FALSE. ) ) THEN
NEST_INFLUENCE(grid%tsk(pig,pjg),xv(1))
ENDIF
ENDIF
IF ( SIZE(grid%rainc) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_child_msg(RWORDSIZE,xv)
IF ( cd_feedback_mask( pig, ips_save, ipe_save , pjg, jps_save, jpe_save, .FALSE., .FALSE. ) ) THEN
NEST_INFLUENCE(grid%rainc(pig,pjg),xv(1))
ENDIF
ENDIF
IF ( SIZE(grid%rainsh) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_child_msg(RWORDSIZE,xv)
IF ( cd_feedback_mask( pig, ips_save, ipe_save , pjg, jps_save, jpe_save, .FALSE., .FALSE. ) ) THEN
NEST_INFLUENCE(grid%rainsh(pig,pjg),xv(1))
ENDIF
ENDIF
IF ( SIZE(grid%rainnc) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_child_msg(RWORDSIZE,xv)
IF ( cd_feedback_mask( pig, ips_save, ipe_save , pjg, jps_save, jpe_save, .FALSE., .FALSE. ) ) THEN
NEST_INFLUENCE(grid%rainnc(pig,pjg),xv(1))
ENDIF
ENDIF
IF ( SIZE(grid%i_rainc) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_child_msg(RWORDSIZE,xv)
IF ( cd_feedback_mask( pig, ips_save, ipe_save , pjg, jps_save, jpe_save, .FALSE., .FALSE. ) ) THEN
NEST_INFLUENCE(grid%i_rainc(pig,pjg),xv(1))
ENDIF
ENDIF
IF ( SIZE(grid%i_rainnc) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_child_msg(RWORDSIZE,xv)
IF ( cd_feedback_mask( pig, ips_save, ipe_save , pjg, jps_save, jpe_save, .FALSE., .FALSE. ) ) THEN
NEST_INFLUENCE(grid%i_rainnc(pig,pjg),xv(1))
ENDIF
ENDIF
IF ( SIZE(grid%snownc) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_child_msg(RWORDSIZE,xv)
IF ( cd_feedback_mask( pig, ips_save, ipe_save , pjg, jps_save, jpe_save, .FALSE., .FALSE. ) ) THEN
NEST_INFLUENCE(grid%snownc(pig,pjg),xv(1))
ENDIF
ENDIF
IF ( SIZE(grid%graupelnc) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_child_msg(RWORDSIZE,xv)
IF ( cd_feedback_mask( pig, ips_save, ipe_save , pjg, jps_save, jpe_save, .FALSE., .FALSE. ) ) THEN
NEST_INFLUENCE(grid%graupelnc(pig,pjg),xv(1))
ENDIF
ENDIF
IF ( SIZE(grid%hailnc) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_child_msg(RWORDSIZE,xv)
IF ( cd_feedback_mask( pig, ips_save, ipe_save , pjg, jps_save, jpe_save, .FALSE., .FALSE. ) ) THEN
NEST_INFLUENCE(grid%hailnc(pig,pjg),xv(1))
ENDIF
ENDIF
IF ( SIZE(grid%refl_10cm) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_child_msg((((ckde-1))-(ckds)+1)*RWORDSIZE,xv) ;
IF ( cd_feedback_mask( pig, ips_save, ipe_save , pjg, jps_save, jpe_save, .FALSE., .FALSE. ) ) THEN
DO k = ckds,(ckde-1)
NEST_INFLUENCE(grid%refl_10cm(pig,k,pjg),xv(k))
ENDDO
ENDIF
ENDIF
IF ( SIZE(grid%acswupt) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_child_msg(RWORDSIZE,xv)
IF ( cd_feedback_mask( pig, ips_save, ipe_save , pjg, jps_save, jpe_save, .FALSE., .FALSE. ) ) THEN
NEST_INFLUENCE(grid%acswupt(pig,pjg),xv(1))
ENDIF
ENDIF
IF ( SIZE(grid%acswuptc) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_child_msg(RWORDSIZE,xv)
IF ( cd_feedback_mask( pig, ips_save, ipe_save , pjg, jps_save, jpe_save, .FALSE., .FALSE. ) ) THEN
NEST_INFLUENCE(grid%acswuptc(pig,pjg),xv(1))
ENDIF
ENDIF
IF ( SIZE(grid%acswdnt) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_child_msg(RWORDSIZE,xv)
IF ( cd_feedback_mask( pig, ips_save, ipe_save , pjg, jps_save, jpe_save, .FALSE., .FALSE. ) ) THEN
NEST_INFLUENCE(grid%acswdnt(pig,pjg),xv(1))
ENDIF
ENDIF
IF ( SIZE(grid%acswdntc) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_child_msg(RWORDSIZE,xv)
IF ( cd_feedback_mask( pig, ips_save, ipe_save , pjg, jps_save, jpe_save, .FALSE., .FALSE. ) ) THEN
NEST_INFLUENCE(grid%acswdntc(pig,pjg),xv(1))
ENDIF
ENDIF
IF ( SIZE(grid%acswupb) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_child_msg(RWORDSIZE,xv)
IF ( cd_feedback_mask( pig, ips_save, ipe_save , pjg, jps_save, jpe_save, .FALSE., .FALSE. ) ) THEN
NEST_INFLUENCE(grid%acswupb(pig,pjg),xv(1))
ENDIF
ENDIF
IF ( SIZE(grid%acswupbc) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_child_msg(RWORDSIZE,xv)
IF ( cd_feedback_mask( pig, ips_save, ipe_save , pjg, jps_save, jpe_save, .FALSE., .FALSE. ) ) THEN
NEST_INFLUENCE(grid%acswupbc(pig,pjg),xv(1))
ENDIF
ENDIF
IF ( SIZE(grid%acswdnb) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_child_msg(RWORDSIZE,xv)
IF ( cd_feedback_mask( pig, ips_save, ipe_save , pjg, jps_save, jpe_save, .FALSE., .FALSE. ) ) THEN
NEST_INFLUENCE(grid%acswdnb(pig,pjg),xv(1))
ENDIF
ENDIF
IF ( SIZE(grid%acswdnbc) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_child_msg(RWORDSIZE,xv)
IF ( cd_feedback_mask( pig, ips_save, ipe_save , pjg, jps_save, jpe_save, .FALSE., .FALSE. ) ) THEN
NEST_INFLUENCE(grid%acswdnbc(pig,pjg),xv(1))
ENDIF
ENDIF
IF ( SIZE(grid%aclwupt) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_child_msg(RWORDSIZE,xv)
IF ( cd_feedback_mask( pig, ips_save, ipe_save , pjg, jps_save, jpe_save, .FALSE., .FALSE. ) ) THEN
NEST_INFLUENCE(grid%aclwupt(pig,pjg),xv(1))
ENDIF
ENDIF
IF ( SIZE(grid%aclwuptc) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_child_msg(RWORDSIZE,xv)
IF ( cd_feedback_mask( pig, ips_save, ipe_save , pjg, jps_save, jpe_save, .FALSE., .FALSE. ) ) THEN
NEST_INFLUENCE(grid%aclwuptc(pig,pjg),xv(1))
ENDIF
ENDIF
IF ( SIZE(grid%aclwdnt) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_child_msg(RWORDSIZE,xv)
IF ( cd_feedback_mask( pig, ips_save, ipe_save , pjg, jps_save, jpe_save, .FALSE., .FALSE. ) ) THEN
NEST_INFLUENCE(grid%aclwdnt(pig,pjg),xv(1))
ENDIF
ENDIF
IF ( SIZE(grid%aclwdntc) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_child_msg(RWORDSIZE,xv)
IF ( cd_feedback_mask( pig, ips_save, ipe_save , pjg, jps_save, jpe_save, .FALSE., .FALSE. ) ) THEN
NEST_INFLUENCE(grid%aclwdntc(pig,pjg),xv(1))
ENDIF
ENDIF
IF ( SIZE(grid%aclwupb) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_child_msg(RWORDSIZE,xv)
IF ( cd_feedback_mask( pig, ips_save, ipe_save , pjg, jps_save, jpe_save, .FALSE., .FALSE. ) ) THEN
NEST_INFLUENCE(grid%aclwupb(pig,pjg),xv(1))
ENDIF
ENDIF
IF ( SIZE(grid%aclwupbc) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_child_msg(RWORDSIZE,xv)
IF ( cd_feedback_mask( pig, ips_save, ipe_save , pjg, jps_save, jpe_save, .FALSE., .FALSE. ) ) THEN
NEST_INFLUENCE(grid%aclwupbc(pig,pjg),xv(1))
ENDIF
ENDIF
IF ( SIZE(grid%aclwdnb) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_child_msg(RWORDSIZE,xv)
IF ( cd_feedback_mask( pig, ips_save, ipe_save , pjg, jps_save, jpe_save, .FALSE., .FALSE. ) ) THEN
NEST_INFLUENCE(grid%aclwdnb(pig,pjg),xv(1))
ENDIF
ENDIF
IF ( SIZE(grid%aclwdnbc) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_child_msg(RWORDSIZE,xv)
IF ( cd_feedback_mask( pig, ips_save, ipe_save , pjg, jps_save, jpe_save, .FALSE., .FALSE. ) ) THEN
NEST_INFLUENCE(grid%aclwdnbc(pig,pjg),xv(1))
ENDIF
ENDIF
IF ( SIZE(grid%i_acswupt) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_child_msg(RWORDSIZE,xv)
IF ( cd_feedback_mask( pig, ips_save, ipe_save , pjg, jps_save, jpe_save, .FALSE., .FALSE. ) ) THEN
NEST_INFLUENCE(grid%i_acswupt(pig,pjg),xv(1))
ENDIF
ENDIF
IF ( SIZE(grid%i_acswuptc) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_child_msg(RWORDSIZE,xv)
IF ( cd_feedback_mask( pig, ips_save, ipe_save , pjg, jps_save, jpe_save, .FALSE., .FALSE. ) ) THEN
NEST_INFLUENCE(grid%i_acswuptc(pig,pjg),xv(1))
ENDIF
ENDIF
IF ( SIZE(grid%i_acswdnt) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_child_msg(RWORDSIZE,xv)
IF ( cd_feedback_mask( pig, ips_save, ipe_save , pjg, jps_save, jpe_save, .FALSE., .FALSE. ) ) THEN
NEST_INFLUENCE(grid%i_acswdnt(pig,pjg),xv(1))
ENDIF
ENDIF
IF ( SIZE(grid%i_acswdntc) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_child_msg(RWORDSIZE,xv)
IF ( cd_feedback_mask( pig, ips_save, ipe_save , pjg, jps_save, jpe_save, .FALSE., .FALSE. ) ) THEN
NEST_INFLUENCE(grid%i_acswdntc(pig,pjg),xv(1))
ENDIF
ENDIF
IF ( SIZE(grid%i_acswupb) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_child_msg(RWORDSIZE,xv)
IF ( cd_feedback_mask( pig, ips_save, ipe_save , pjg, jps_save, jpe_save, .FALSE., .FALSE. ) ) THEN
NEST_INFLUENCE(grid%i_acswupb(pig,pjg),xv(1))
ENDIF
ENDIF
IF ( SIZE(grid%i_acswupbc) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_child_msg(RWORDSIZE,xv)
IF ( cd_feedback_mask( pig, ips_save, ipe_save , pjg, jps_save, jpe_save, .FALSE., .FALSE. ) ) THEN
NEST_INFLUENCE(grid%i_acswupbc(pig,pjg),xv(1))
ENDIF
ENDIF
IF ( SIZE(grid%i_acswdnb) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_child_msg(RWORDSIZE,xv)
IF ( cd_feedback_mask( pig, ips_save, ipe_save , pjg, jps_save, jpe_save, .FALSE., .FALSE. ) ) THEN
NEST_INFLUENCE(grid%i_acswdnb(pig,pjg),xv(1))
ENDIF
ENDIF
IF ( SIZE(grid%i_acswdnbc) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_child_msg(RWORDSIZE,xv)
IF ( cd_feedback_mask( pig, ips_save, ipe_save , pjg, jps_save, jpe_save, .FALSE., .FALSE. ) ) THEN
NEST_INFLUENCE(grid%i_acswdnbc(pig,pjg),xv(1))
ENDIF
ENDIF
IF ( SIZE(grid%i_aclwupt) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_child_msg(RWORDSIZE,xv)
IF ( cd_feedback_mask( pig, ips_save, ipe_save , pjg, jps_save, jpe_save, .FALSE., .FALSE. ) ) THEN
NEST_INFLUENCE(grid%i_aclwupt(pig,pjg),xv(1))
ENDIF
ENDIF
IF ( SIZE(grid%i_aclwuptc) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_child_msg(RWORDSIZE,xv)
IF ( cd_feedback_mask( pig, ips_save, ipe_save , pjg, jps_save, jpe_save, .FALSE., .FALSE. ) ) THEN
NEST_INFLUENCE(grid%i_aclwuptc(pig,pjg),xv(1))
ENDIF
ENDIF
IF ( SIZE(grid%i_aclwdnt) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_child_msg(RWORDSIZE,xv)
IF ( cd_feedback_mask( pig, ips_save, ipe_save , pjg, jps_save, jpe_save, .FALSE., .FALSE. ) ) THEN
NEST_INFLUENCE(grid%i_aclwdnt(pig,pjg),xv(1))
ENDIF
ENDIF
IF ( SIZE(grid%i_aclwdntc) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_child_msg(RWORDSIZE,xv)
IF ( cd_feedback_mask( pig, ips_save, ipe_save , pjg, jps_save, jpe_save, .FALSE., .FALSE. ) ) THEN
NEST_INFLUENCE(grid%i_aclwdntc(pig,pjg),xv(1))
ENDIF
ENDIF
IF ( SIZE(grid%i_aclwupb) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_child_msg(RWORDSIZE,xv)
IF ( cd_feedback_mask( pig, ips_save, ipe_save , pjg, jps_save, jpe_save, .FALSE., .FALSE. ) ) THEN
NEST_INFLUENCE(grid%i_aclwupb(pig,pjg),xv(1))
ENDIF
ENDIF
IF ( SIZE(grid%i_aclwupbc) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_child_msg(RWORDSIZE,xv)
IF ( cd_feedback_mask( pig, ips_save, ipe_save , pjg, jps_save, jpe_save, .FALSE., .FALSE. ) ) THEN
NEST_INFLUENCE(grid%i_aclwupbc(pig,pjg),xv(1))
ENDIF
ENDIF
IF ( SIZE(grid%i_aclwdnb) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_child_msg(RWORDSIZE,xv)
IF ( cd_feedback_mask( pig, ips_save, ipe_save , pjg, jps_save, jpe_save, .FALSE., .FALSE. ) ) THEN
NEST_INFLUENCE(grid%i_aclwdnb(pig,pjg),xv(1))
ENDIF
ENDIF
IF ( SIZE(grid%i_aclwdnbc) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_child_msg(RWORDSIZE,xv)
IF ( cd_feedback_mask( pig, ips_save, ipe_save , pjg, jps_save, jpe_save, .FALSE., .FALSE. ) ) THEN
NEST_INFLUENCE(grid%i_aclwdnbc(pig,pjg),xv(1))
ENDIF
ENDIF
IF ( SIZE(grid%swupt) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_child_msg(RWORDSIZE,xv)
IF ( cd_feedback_mask( pig, ips_save, ipe_save , pjg, jps_save, jpe_save, .FALSE., .FALSE. ) ) THEN
NEST_INFLUENCE(grid%swupt(pig,pjg),xv(1))
ENDIF
ENDIF
IF ( SIZE(grid%swuptc) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_child_msg(RWORDSIZE,xv)
IF ( cd_feedback_mask( pig, ips_save, ipe_save , pjg, jps_save, jpe_save, .FALSE., .FALSE. ) ) THEN
NEST_INFLUENCE(grid%swuptc(pig,pjg),xv(1))
ENDIF
ENDIF
IF ( SIZE(grid%swdnt) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_child_msg(RWORDSIZE,xv)
IF ( cd_feedback_mask( pig, ips_save, ipe_save , pjg, jps_save, jpe_save, .FALSE., .FALSE. ) ) THEN
NEST_INFLUENCE(grid%swdnt(pig,pjg),xv(1))
ENDIF
ENDIF
IF ( SIZE(grid%swdntc) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_child_msg(RWORDSIZE,xv)
IF ( cd_feedback_mask( pig, ips_save, ipe_save , pjg, jps_save, jpe_save, .FALSE., .FALSE. ) ) THEN
NEST_INFLUENCE(grid%swdntc(pig,pjg),xv(1))
ENDIF
ENDIF
IF ( SIZE(grid%swupb) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_child_msg(RWORDSIZE,xv)
IF ( cd_feedback_mask( pig, ips_save, ipe_save , pjg, jps_save, jpe_save, .FALSE., .FALSE. ) ) THEN
NEST_INFLUENCE(grid%swupb(pig,pjg),xv(1))
ENDIF
ENDIF
IF ( SIZE(grid%swupbc) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_child_msg(RWORDSIZE,xv)
IF ( cd_feedback_mask( pig, ips_save, ipe_save , pjg, jps_save, jpe_save, .FALSE., .FALSE. ) ) THEN
NEST_INFLUENCE(grid%swupbc(pig,pjg),xv(1))
ENDIF
ENDIF
IF ( SIZE(grid%swdnb) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_child_msg(RWORDSIZE,xv)
IF ( cd_feedback_mask( pig, ips_save, ipe_save , pjg, jps_save, jpe_save, .FALSE., .FALSE. ) ) THEN
NEST_INFLUENCE(grid%swdnb(pig,pjg),xv(1))
ENDIF
ENDIF
IF ( SIZE(grid%swdnbc) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_child_msg(RWORDSIZE,xv)
IF ( cd_feedback_mask( pig, ips_save, ipe_save , pjg, jps_save, jpe_save, .FALSE., .FALSE. ) ) THEN
NEST_INFLUENCE(grid%swdnbc(pig,pjg),xv(1))
ENDIF
ENDIF
IF ( SIZE(grid%lwupt) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_child_msg(RWORDSIZE,xv)
IF ( cd_feedback_mask( pig, ips_save, ipe_save , pjg, jps_save, jpe_save, .FALSE., .FALSE. ) ) THEN
NEST_INFLUENCE(grid%lwupt(pig,pjg),xv(1))
ENDIF
ENDIF
IF ( SIZE(grid%lwuptc) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_child_msg(RWORDSIZE,xv)
IF ( cd_feedback_mask( pig, ips_save, ipe_save , pjg, jps_save, jpe_save, .FALSE., .FALSE. ) ) THEN
NEST_INFLUENCE(grid%lwuptc(pig,pjg),xv(1))
ENDIF
ENDIF
IF ( SIZE(grid%lwdnt) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_child_msg(RWORDSIZE,xv)
IF ( cd_feedback_mask( pig, ips_save, ipe_save , pjg, jps_save, jpe_save, .FALSE., .FALSE. ) ) THEN
NEST_INFLUENCE(grid%lwdnt(pig,pjg),xv(1))
ENDIF
ENDIF
IF ( SIZE(grid%lwdntc) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_child_msg(RWORDSIZE,xv)
IF ( cd_feedback_mask( pig, ips_save, ipe_save , pjg, jps_save, jpe_save, .FALSE., .FALSE. ) ) THEN
NEST_INFLUENCE(grid%lwdntc(pig,pjg),xv(1))
ENDIF
ENDIF
IF ( SIZE(grid%lwupb) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_child_msg(RWORDSIZE,xv)
IF ( cd_feedback_mask( pig, ips_save, ipe_save , pjg, jps_save, jpe_save, .FALSE., .FALSE. ) ) THEN
NEST_INFLUENCE(grid%lwupb(pig,pjg),xv(1))
ENDIF
ENDIF
IF ( SIZE(grid%lwupbc) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_child_msg(RWORDSIZE,xv)
IF ( cd_feedback_mask( pig, ips_save, ipe_save , pjg, jps_save, jpe_save, .FALSE., .FALSE. ) ) THEN
NEST_INFLUENCE(grid%lwupbc(pig,pjg),xv(1))
ENDIF
ENDIF
IF ( SIZE(grid%lwdnb) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_child_msg(RWORDSIZE,xv)
IF ( cd_feedback_mask( pig, ips_save, ipe_save , pjg, jps_save, jpe_save, .FALSE., .FALSE. ) ) THEN
NEST_INFLUENCE(grid%lwdnb(pig,pjg),xv(1))
ENDIF
ENDIF
IF ( SIZE(grid%lwdnbc) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_child_msg(RWORDSIZE,xv)
IF ( cd_feedback_mask( pig, ips_save, ipe_save , pjg, jps_save, jpe_save, .FALSE., .FALSE. ) ) THEN
NEST_INFLUENCE(grid%lwdnbc(pig,pjg),xv(1))
ENDIF
ENDIF
IF ( SIZE(grid%xlat_u) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_child_msg(RWORDSIZE,xv)
IF ( cd_feedback_mask( pig, ips_save, ipe_save , pjg, jps_save, jpe_save, .TRUE., .FALSE. ) ) THEN
NEST_INFLUENCE(grid%xlat_u(pig,pjg),xv(1))
ENDIF
ENDIF
IF ( SIZE(grid%xlong_u) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_child_msg(RWORDSIZE,xv)
IF ( cd_feedback_mask( pig, ips_save, ipe_save , pjg, jps_save, jpe_save, .TRUE., .FALSE. ) ) THEN
NEST_INFLUENCE(grid%xlong_u(pig,pjg),xv(1))
ENDIF
ENDIF
IF ( SIZE(grid%xlat_v) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_child_msg(RWORDSIZE,xv)
IF ( cd_feedback_mask( pig, ips_save, ipe_save , pjg, jps_save, jpe_save, .FALSE., .TRUE. ) ) THEN
NEST_INFLUENCE(grid%xlat_v(pig,pjg),xv(1))
ENDIF
ENDIF
IF ( SIZE(grid%xlong_v) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_child_msg(RWORDSIZE,xv)
IF ( cd_feedback_mask( pig, ips_save, ipe_save , pjg, jps_save, jpe_save, .FALSE., .TRUE. ) ) THEN
NEST_INFLUENCE(grid%xlong_v(pig,pjg),xv(1))
ENDIF
ENDIF
IF ( SIZE(grid%clat) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_child_msg(RWORDSIZE,xv)
IF ( cd_feedback_mask( pig, ips_save, ipe_save , pjg, jps_save, jpe_save, .FALSE., .FALSE. ) ) THEN
NEST_INFLUENCE(grid%clat(pig,pjg),xv(1))
ENDIF
ENDIF
IF ( SIZE(grid%isnowxy) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_child_msg(RWORDSIZE,xv)
IF ( cd_feedback_mask( pig, ips_save, ipe_save , pjg, jps_save, jpe_save, .FALSE., .FALSE. ) ) THEN
NEST_INFLUENCE(grid%isnowxy(pig,pjg),xv(1))
ENDIF
ENDIF
IF ( SIZE(grid%tvxy) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_child_msg(RWORDSIZE,xv)
IF ( cd_feedback_mask( pig, ips_save, ipe_save , pjg, jps_save, jpe_save, .FALSE., .FALSE. ) ) THEN
NEST_INFLUENCE(grid%tvxy(pig,pjg),xv(1))
ENDIF
ENDIF
IF ( SIZE(grid%tgxy) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_child_msg(RWORDSIZE,xv)
IF ( cd_feedback_mask( pig, ips_save, ipe_save , pjg, jps_save, jpe_save, .FALSE., .FALSE. ) ) THEN
NEST_INFLUENCE(grid%tgxy(pig,pjg),xv(1))
ENDIF
ENDIF
IF ( SIZE(grid%canicexy) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_child_msg(RWORDSIZE,xv)
IF ( cd_feedback_mask( pig, ips_save, ipe_save , pjg, jps_save, jpe_save, .FALSE., .FALSE. ) ) THEN
NEST_INFLUENCE(grid%canicexy(pig,pjg),xv(1))
ENDIF
ENDIF
IF ( SIZE(grid%canliqxy) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_child_msg(RWORDSIZE,xv)
IF ( cd_feedback_mask( pig, ips_save, ipe_save , pjg, jps_save, jpe_save, .FALSE., .FALSE. ) ) THEN
NEST_INFLUENCE(grid%canliqxy(pig,pjg),xv(1))
ENDIF
ENDIF
IF ( SIZE(grid%eahxy) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_child_msg(RWORDSIZE,xv)
IF ( cd_feedback_mask( pig, ips_save, ipe_save , pjg, jps_save, jpe_save, .FALSE., .FALSE. ) ) THEN
NEST_INFLUENCE(grid%eahxy(pig,pjg),xv(1))
ENDIF
ENDIF
IF ( SIZE(grid%tahxy) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_child_msg(RWORDSIZE,xv)
IF ( cd_feedback_mask( pig, ips_save, ipe_save , pjg, jps_save, jpe_save, .FALSE., .FALSE. ) ) THEN
NEST_INFLUENCE(grid%tahxy(pig,pjg),xv(1))
ENDIF
ENDIF
IF ( SIZE(grid%cmxy) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_child_msg(RWORDSIZE,xv)
IF ( cd_feedback_mask( pig, ips_save, ipe_save , pjg, jps_save, jpe_save, .FALSE., .FALSE. ) ) THEN
NEST_INFLUENCE(grid%cmxy(pig,pjg),xv(1))
ENDIF
ENDIF
IF ( SIZE(grid%chxy) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_child_msg(RWORDSIZE,xv)
IF ( cd_feedback_mask( pig, ips_save, ipe_save , pjg, jps_save, jpe_save, .FALSE., .FALSE. ) ) THEN
NEST_INFLUENCE(grid%chxy(pig,pjg),xv(1))
ENDIF
ENDIF
IF ( SIZE(grid%fwetxy) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_child_msg(RWORDSIZE,xv)
IF ( cd_feedback_mask( pig, ips_save, ipe_save , pjg, jps_save, jpe_save, .FALSE., .FALSE. ) ) THEN
NEST_INFLUENCE(grid%fwetxy(pig,pjg),xv(1))
ENDIF
ENDIF
IF ( SIZE(grid%sneqvoxy) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_child_msg(RWORDSIZE,xv)
IF ( cd_feedback_mask( pig, ips_save, ipe_save , pjg, jps_save, jpe_save, .FALSE., .FALSE. ) ) THEN
NEST_INFLUENCE(grid%sneqvoxy(pig,pjg),xv(1))
ENDIF
ENDIF
IF ( SIZE(grid%alboldxy) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_child_msg(RWORDSIZE,xv)
IF ( cd_feedback_mask( pig, ips_save, ipe_save , pjg, jps_save, jpe_save, .FALSE., .FALSE. ) ) THEN
NEST_INFLUENCE(grid%alboldxy(pig,pjg),xv(1))
ENDIF
ENDIF
IF ( SIZE(grid%qsnowxy) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_child_msg(RWORDSIZE,xv)
IF ( cd_feedback_mask( pig, ips_save, ipe_save , pjg, jps_save, jpe_save, .FALSE., .FALSE. ) ) THEN
NEST_INFLUENCE(grid%qsnowxy(pig,pjg),xv(1))
ENDIF
ENDIF
IF ( SIZE(grid%wslakexy) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_child_msg(RWORDSIZE,xv)
IF ( cd_feedback_mask( pig, ips_save, ipe_save , pjg, jps_save, jpe_save, .FALSE., .FALSE. ) ) THEN
NEST_INFLUENCE(grid%wslakexy(pig,pjg),xv(1))
ENDIF
ENDIF
IF ( SIZE(grid%zwtxy) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_child_msg(RWORDSIZE,xv)
IF ( cd_feedback_mask( pig, ips_save, ipe_save , pjg, jps_save, jpe_save, .FALSE., .FALSE. ) ) THEN
NEST_INFLUENCE(grid%zwtxy(pig,pjg),xv(1))
ENDIF
ENDIF
IF ( SIZE(grid%waxy) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_child_msg(RWORDSIZE,xv)
IF ( cd_feedback_mask( pig, ips_save, ipe_save , pjg, jps_save, jpe_save, .FALSE., .FALSE. ) ) THEN
NEST_INFLUENCE(grid%waxy(pig,pjg),xv(1))
ENDIF
ENDIF
IF ( SIZE(grid%wtxy) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_child_msg(RWORDSIZE,xv)
IF ( cd_feedback_mask( pig, ips_save, ipe_save , pjg, jps_save, jpe_save, .FALSE., .FALSE. ) ) THEN
NEST_INFLUENCE(grid%wtxy(pig,pjg),xv(1))
ENDIF
ENDIF
IF ( SIZE(grid%tsnoxy) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_child_msg(((config_flags%num_snow_layers)-(1)+1)*RWORDSIZE,xv) ;
IF ( cd_feedback_mask( pig, ips_save, ipe_save , pjg, jps_save, jpe_save, .FALSE., .FALSE. ) ) THEN
DO k = 1,config_flags%num_snow_layers
NEST_INFLUENCE(grid%tsnoxy(pig,k,pjg),xv(k))
ENDDO
ENDIF
ENDIF
IF ( SIZE(grid%zsnsoxy) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_child_msg(((config_flags%num_snso_layers)-(1)+1)*RWORDSIZE,xv) ;
IF ( cd_feedback_mask( pig, ips_save, ipe_save , pjg, jps_save, jpe_save, .FALSE., .FALSE. ) ) THEN
DO k = 1,config_flags%num_snso_layers
NEST_INFLUENCE(grid%zsnsoxy(pig,k,pjg),xv(k))
ENDDO
ENDIF
ENDIF
IF ( SIZE(grid%snicexy) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_child_msg(((config_flags%num_snow_layers)-(1)+1)*RWORDSIZE,xv) ;
IF ( cd_feedback_mask( pig, ips_save, ipe_save , pjg, jps_save, jpe_save, .FALSE., .FALSE. ) ) THEN
DO k = 1,config_flags%num_snow_layers
NEST_INFLUENCE(grid%snicexy(pig,k,pjg),xv(k))
ENDDO
ENDIF
ENDIF
IF ( SIZE(grid%snliqxy) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_child_msg(((config_flags%num_snow_layers)-(1)+1)*RWORDSIZE,xv) ;
IF ( cd_feedback_mask( pig, ips_save, ipe_save , pjg, jps_save, jpe_save, .FALSE., .FALSE. ) ) THEN
DO k = 1,config_flags%num_snow_layers
NEST_INFLUENCE(grid%snliqxy(pig,k,pjg),xv(k))
ENDDO
ENDIF
ENDIF
IF ( SIZE(grid%lfmassxy) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_child_msg(RWORDSIZE,xv)
IF ( cd_feedback_mask( pig, ips_save, ipe_save , pjg, jps_save, jpe_save, .FALSE., .FALSE. ) ) THEN
NEST_INFLUENCE(grid%lfmassxy(pig,pjg),xv(1))
ENDIF
ENDIF
IF ( SIZE(grid%rtmassxy) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_child_msg(RWORDSIZE,xv)
IF ( cd_feedback_mask( pig, ips_save, ipe_save , pjg, jps_save, jpe_save, .FALSE., .FALSE. ) ) THEN
NEST_INFLUENCE(grid%rtmassxy(pig,pjg),xv(1))
ENDIF
ENDIF
IF ( SIZE(grid%stmassxy) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_child_msg(RWORDSIZE,xv)
IF ( cd_feedback_mask( pig, ips_save, ipe_save , pjg, jps_save, jpe_save, .FALSE., .FALSE. ) ) THEN
NEST_INFLUENCE(grid%stmassxy(pig,pjg),xv(1))
ENDIF
ENDIF
IF ( SIZE(grid%woodxy) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_child_msg(RWORDSIZE,xv)
IF ( cd_feedback_mask( pig, ips_save, ipe_save , pjg, jps_save, jpe_save, .FALSE., .FALSE. ) ) THEN
NEST_INFLUENCE(grid%woodxy(pig,pjg),xv(1))
ENDIF
ENDIF
IF ( SIZE(grid%stblcpxy) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_child_msg(RWORDSIZE,xv)
IF ( cd_feedback_mask( pig, ips_save, ipe_save , pjg, jps_save, jpe_save, .FALSE., .FALSE. ) ) THEN
NEST_INFLUENCE(grid%stblcpxy(pig,pjg),xv(1))
ENDIF
ENDIF
IF ( SIZE(grid%fastcpxy) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_child_msg(RWORDSIZE,xv)
IF ( cd_feedback_mask( pig, ips_save, ipe_save , pjg, jps_save, jpe_save, .FALSE., .FALSE. ) ) THEN
NEST_INFLUENCE(grid%fastcpxy(pig,pjg),xv(1))
ENDIF
ENDIF
IF ( SIZE(grid%xsaixy) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_child_msg(RWORDSIZE,xv)
IF ( cd_feedback_mask( pig, ips_save, ipe_save , pjg, jps_save, jpe_save, .FALSE., .FALSE. ) ) THEN
NEST_INFLUENCE(grid%xsaixy(pig,pjg),xv(1))
ENDIF
ENDIF
IF ( SIZE(grid%t2mvxy) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_child_msg(RWORDSIZE,xv)
IF ( cd_feedback_mask( pig, ips_save, ipe_save , pjg, jps_save, jpe_save, .FALSE., .FALSE. ) ) THEN
NEST_INFLUENCE(grid%t2mvxy(pig,pjg),xv(1))
ENDIF
ENDIF
IF ( SIZE(grid%t2mbxy) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_child_msg(RWORDSIZE,xv)
IF ( cd_feedback_mask( pig, ips_save, ipe_save , pjg, jps_save, jpe_save, .FALSE., .FALSE. ) ) THEN
NEST_INFLUENCE(grid%t2mbxy(pig,pjg),xv(1))
ENDIF
ENDIF
IF ( SIZE(grid%q2mvxy) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_child_msg(RWORDSIZE,xv)
IF ( cd_feedback_mask( pig, ips_save, ipe_save , pjg, jps_save, jpe_save, .FALSE., .FALSE. ) ) THEN
NEST_INFLUENCE(grid%q2mvxy(pig,pjg),xv(1))
ENDIF
ENDIF
IF ( SIZE(grid%q2mbxy) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_child_msg(RWORDSIZE,xv)
IF ( cd_feedback_mask( pig, ips_save, ipe_save , pjg, jps_save, jpe_save, .FALSE., .FALSE. ) ) THEN
NEST_INFLUENCE(grid%q2mbxy(pig,pjg),xv(1))
ENDIF
ENDIF
IF ( SIZE(grid%tradxy) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_child_msg(RWORDSIZE,xv)
IF ( cd_feedback_mask( pig, ips_save, ipe_save , pjg, jps_save, jpe_save, .FALSE., .FALSE. ) ) THEN
NEST_INFLUENCE(grid%tradxy(pig,pjg),xv(1))
ENDIF
ENDIF
IF ( SIZE(grid%neexy) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_child_msg(RWORDSIZE,xv)
IF ( cd_feedback_mask( pig, ips_save, ipe_save , pjg, jps_save, jpe_save, .FALSE., .FALSE. ) ) THEN
NEST_INFLUENCE(grid%neexy(pig,pjg),xv(1))
ENDIF
ENDIF
IF ( SIZE(grid%gppxy) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_child_msg(RWORDSIZE,xv)
IF ( cd_feedback_mask( pig, ips_save, ipe_save , pjg, jps_save, jpe_save, .FALSE., .FALSE. ) ) THEN
NEST_INFLUENCE(grid%gppxy(pig,pjg),xv(1))
ENDIF
ENDIF
IF ( SIZE(grid%nppxy) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_child_msg(RWORDSIZE,xv)
IF ( cd_feedback_mask( pig, ips_save, ipe_save , pjg, jps_save, jpe_save, .FALSE., .FALSE. ) ) THEN
NEST_INFLUENCE(grid%nppxy(pig,pjg),xv(1))
ENDIF
ENDIF
IF ( SIZE(grid%fvegxy) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_child_msg(RWORDSIZE,xv)
IF ( cd_feedback_mask( pig, ips_save, ipe_save , pjg, jps_save, jpe_save, .FALSE., .FALSE. ) ) THEN
NEST_INFLUENCE(grid%fvegxy(pig,pjg),xv(1))
ENDIF
ENDIF
IF ( SIZE(grid%qinxy) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_child_msg(RWORDSIZE,xv)
IF ( cd_feedback_mask( pig, ips_save, ipe_save , pjg, jps_save, jpe_save, .FALSE., .FALSE. ) ) THEN
NEST_INFLUENCE(grid%qinxy(pig,pjg),xv(1))
ENDIF
ENDIF
IF ( SIZE(grid%runsfxy) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_child_msg(RWORDSIZE,xv)
IF ( cd_feedback_mask( pig, ips_save, ipe_save , pjg, jps_save, jpe_save, .FALSE., .FALSE. ) ) THEN
NEST_INFLUENCE(grid%runsfxy(pig,pjg),xv(1))
ENDIF
ENDIF
IF ( SIZE(grid%runsbxy) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_child_msg(RWORDSIZE,xv)
IF ( cd_feedback_mask( pig, ips_save, ipe_save , pjg, jps_save, jpe_save, .FALSE., .FALSE. ) ) THEN
NEST_INFLUENCE(grid%runsbxy(pig,pjg),xv(1))
ENDIF
ENDIF
IF ( SIZE(grid%ecanxy) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_child_msg(RWORDSIZE,xv)
IF ( cd_feedback_mask( pig, ips_save, ipe_save , pjg, jps_save, jpe_save, .FALSE., .FALSE. ) ) THEN
NEST_INFLUENCE(grid%ecanxy(pig,pjg),xv(1))
ENDIF
ENDIF
IF ( SIZE(grid%edirxy) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_child_msg(RWORDSIZE,xv)
IF ( cd_feedback_mask( pig, ips_save, ipe_save , pjg, jps_save, jpe_save, .FALSE., .FALSE. ) ) THEN
NEST_INFLUENCE(grid%edirxy(pig,pjg),xv(1))
ENDIF
ENDIF
IF ( SIZE(grid%etranxy) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_child_msg(RWORDSIZE,xv)
IF ( cd_feedback_mask( pig, ips_save, ipe_save , pjg, jps_save, jpe_save, .FALSE., .FALSE. ) ) THEN
NEST_INFLUENCE(grid%etranxy(pig,pjg),xv(1))
ENDIF
ENDIF
IF ( SIZE(grid%fsaxy) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_child_msg(RWORDSIZE,xv)
IF ( cd_feedback_mask( pig, ips_save, ipe_save , pjg, jps_save, jpe_save, .FALSE., .FALSE. ) ) THEN
NEST_INFLUENCE(grid%fsaxy(pig,pjg),xv(1))
ENDIF
ENDIF
IF ( SIZE(grid%firaxy) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_child_msg(RWORDSIZE,xv)
IF ( cd_feedback_mask( pig, ips_save, ipe_save , pjg, jps_save, jpe_save, .FALSE., .FALSE. ) ) THEN
NEST_INFLUENCE(grid%firaxy(pig,pjg),xv(1))
ENDIF
ENDIF
IF ( SIZE(grid%aparxy) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_child_msg(RWORDSIZE,xv)
IF ( cd_feedback_mask( pig, ips_save, ipe_save , pjg, jps_save, jpe_save, .FALSE., .FALSE. ) ) THEN
NEST_INFLUENCE(grid%aparxy(pig,pjg),xv(1))
ENDIF
ENDIF
IF ( SIZE(grid%psnxy) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_child_msg(RWORDSIZE,xv)
IF ( cd_feedback_mask( pig, ips_save, ipe_save , pjg, jps_save, jpe_save, .FALSE., .FALSE. ) ) THEN
NEST_INFLUENCE(grid%psnxy(pig,pjg),xv(1))
ENDIF
ENDIF
IF ( SIZE(grid%savxy) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_child_msg(RWORDSIZE,xv)
IF ( cd_feedback_mask( pig, ips_save, ipe_save , pjg, jps_save, jpe_save, .FALSE., .FALSE. ) ) THEN
NEST_INFLUENCE(grid%savxy(pig,pjg),xv(1))
ENDIF
ENDIF
IF ( SIZE(grid%sagxy) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_child_msg(RWORDSIZE,xv)
IF ( cd_feedback_mask( pig, ips_save, ipe_save , pjg, jps_save, jpe_save, .FALSE., .FALSE. ) ) THEN
NEST_INFLUENCE(grid%sagxy(pig,pjg),xv(1))
ENDIF
ENDIF
IF ( SIZE(grid%rssunxy) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_child_msg(RWORDSIZE,xv)
IF ( cd_feedback_mask( pig, ips_save, ipe_save , pjg, jps_save, jpe_save, .FALSE., .FALSE. ) ) THEN
NEST_INFLUENCE(grid%rssunxy(pig,pjg),xv(1))
ENDIF
ENDIF
IF ( SIZE(grid%rsshaxy) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_child_msg(RWORDSIZE,xv)
IF ( cd_feedback_mask( pig, ips_save, ipe_save , pjg, jps_save, jpe_save, .FALSE., .FALSE. ) ) THEN
NEST_INFLUENCE(grid%rsshaxy(pig,pjg),xv(1))
ENDIF
ENDIF
IF ( SIZE(grid%bgapxy) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_child_msg(RWORDSIZE,xv)
IF ( cd_feedback_mask( pig, ips_save, ipe_save , pjg, jps_save, jpe_save, .FALSE., .FALSE. ) ) THEN
NEST_INFLUENCE(grid%bgapxy(pig,pjg),xv(1))
ENDIF
ENDIF
IF ( SIZE(grid%wgapxy) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_child_msg(RWORDSIZE,xv)
IF ( cd_feedback_mask( pig, ips_save, ipe_save , pjg, jps_save, jpe_save, .FALSE., .FALSE. ) ) THEN
NEST_INFLUENCE(grid%wgapxy(pig,pjg),xv(1))
ENDIF
ENDIF
IF ( SIZE(grid%tgvxy) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_child_msg(RWORDSIZE,xv)
IF ( cd_feedback_mask( pig, ips_save, ipe_save , pjg, jps_save, jpe_save, .FALSE., .FALSE. ) ) THEN
NEST_INFLUENCE(grid%tgvxy(pig,pjg),xv(1))
ENDIF
ENDIF
IF ( SIZE(grid%tgbxy) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_child_msg(RWORDSIZE,xv)
IF ( cd_feedback_mask( pig, ips_save, ipe_save , pjg, jps_save, jpe_save, .FALSE., .FALSE. ) ) THEN
NEST_INFLUENCE(grid%tgbxy(pig,pjg),xv(1))
ENDIF
ENDIF
IF ( SIZE(grid%chvxy) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_child_msg(RWORDSIZE,xv)
IF ( cd_feedback_mask( pig, ips_save, ipe_save , pjg, jps_save, jpe_save, .FALSE., .FALSE. ) ) THEN
NEST_INFLUENCE(grid%chvxy(pig,pjg),xv(1))
ENDIF
ENDIF
IF ( SIZE(grid%chbxy) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_child_msg(RWORDSIZE,xv)
IF ( cd_feedback_mask( pig, ips_save, ipe_save , pjg, jps_save, jpe_save, .FALSE., .FALSE. ) ) THEN
NEST_INFLUENCE(grid%chbxy(pig,pjg),xv(1))
ENDIF
ENDIF
IF ( SIZE(grid%shgxy) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_child_msg(RWORDSIZE,xv)
IF ( cd_feedback_mask( pig, ips_save, ipe_save , pjg, jps_save, jpe_save, .FALSE., .FALSE. ) ) THEN
NEST_INFLUENCE(grid%shgxy(pig,pjg),xv(1))
ENDIF
ENDIF
IF ( SIZE(grid%shcxy) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_child_msg(RWORDSIZE,xv)
IF ( cd_feedback_mask( pig, ips_save, ipe_save , pjg, jps_save, jpe_save, .FALSE., .FALSE. ) ) THEN
NEST_INFLUENCE(grid%shcxy(pig,pjg),xv(1))
ENDIF
ENDIF
IF ( SIZE(grid%shbxy) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_child_msg(RWORDSIZE,xv)
IF ( cd_feedback_mask( pig, ips_save, ipe_save , pjg, jps_save, jpe_save, .FALSE., .FALSE. ) ) THEN
NEST_INFLUENCE(grid%shbxy(pig,pjg),xv(1))
ENDIF
ENDIF
IF ( SIZE(grid%evgxy) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_child_msg(RWORDSIZE,xv)
IF ( cd_feedback_mask( pig, ips_save, ipe_save , pjg, jps_save, jpe_save, .FALSE., .FALSE. ) ) THEN
NEST_INFLUENCE(grid%evgxy(pig,pjg),xv(1))
ENDIF
ENDIF
IF ( SIZE(grid%evbxy) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_child_msg(RWORDSIZE,xv)
IF ( cd_feedback_mask( pig, ips_save, ipe_save , pjg, jps_save, jpe_save, .FALSE., .FALSE. ) ) THEN
NEST_INFLUENCE(grid%evbxy(pig,pjg),xv(1))
ENDIF
ENDIF
IF ( SIZE(grid%ghvxy) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_child_msg(RWORDSIZE,xv)
IF ( cd_feedback_mask( pig, ips_save, ipe_save , pjg, jps_save, jpe_save, .FALSE., .FALSE. ) ) THEN
NEST_INFLUENCE(grid%ghvxy(pig,pjg),xv(1))
ENDIF
ENDIF
IF ( SIZE(grid%ghbxy) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_child_msg(RWORDSIZE,xv)
IF ( cd_feedback_mask( pig, ips_save, ipe_save , pjg, jps_save, jpe_save, .FALSE., .FALSE. ) ) THEN
NEST_INFLUENCE(grid%ghbxy(pig,pjg),xv(1))
ENDIF
ENDIF
IF ( SIZE(grid%irgxy) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_child_msg(RWORDSIZE,xv)
IF ( cd_feedback_mask( pig, ips_save, ipe_save , pjg, jps_save, jpe_save, .FALSE., .FALSE. ) ) THEN
NEST_INFLUENCE(grid%irgxy(pig,pjg),xv(1))
ENDIF
ENDIF
IF ( SIZE(grid%ircxy) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_child_msg(RWORDSIZE,xv)
IF ( cd_feedback_mask( pig, ips_save, ipe_save , pjg, jps_save, jpe_save, .FALSE., .FALSE. ) ) THEN
NEST_INFLUENCE(grid%ircxy(pig,pjg),xv(1))
ENDIF
ENDIF
IF ( SIZE(grid%irbxy) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_child_msg(RWORDSIZE,xv)
IF ( cd_feedback_mask( pig, ips_save, ipe_save , pjg, jps_save, jpe_save, .FALSE., .FALSE. ) ) THEN
NEST_INFLUENCE(grid%irbxy(pig,pjg),xv(1))
ENDIF
ENDIF
IF ( SIZE(grid%trxy) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_child_msg(RWORDSIZE,xv)
IF ( cd_feedback_mask( pig, ips_save, ipe_save , pjg, jps_save, jpe_save, .FALSE., .FALSE. ) ) THEN
NEST_INFLUENCE(grid%trxy(pig,pjg),xv(1))
ENDIF
ENDIF
IF ( SIZE(grid%evcxy) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_child_msg(RWORDSIZE,xv)
IF ( cd_feedback_mask( pig, ips_save, ipe_save , pjg, jps_save, jpe_save, .FALSE., .FALSE. ) ) THEN
NEST_INFLUENCE(grid%evcxy(pig,pjg),xv(1))
ENDIF
ENDIF
IF ( SIZE(grid%chleafxy) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_child_msg(RWORDSIZE,xv)
IF ( cd_feedback_mask( pig, ips_save, ipe_save , pjg, jps_save, jpe_save, .FALSE., .FALSE. ) ) THEN
NEST_INFLUENCE(grid%chleafxy(pig,pjg),xv(1))
ENDIF
ENDIF
IF ( SIZE(grid%chucxy) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_child_msg(RWORDSIZE,xv)
IF ( cd_feedback_mask( pig, ips_save, ipe_save , pjg, jps_save, jpe_save, .FALSE., .FALSE. ) ) THEN
NEST_INFLUENCE(grid%chucxy(pig,pjg),xv(1))
ENDIF
ENDIF
IF ( SIZE(grid%chv2xy) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_child_msg(RWORDSIZE,xv)
IF ( cd_feedback_mask( pig, ips_save, ipe_save , pjg, jps_save, jpe_save, .FALSE., .FALSE. ) ) THEN
NEST_INFLUENCE(grid%chv2xy(pig,pjg),xv(1))
ENDIF
ENDIF
IF ( SIZE(grid%chb2xy) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_child_msg(RWORDSIZE,xv)
IF ( cd_feedback_mask( pig, ips_save, ipe_save , pjg, jps_save, jpe_save, .FALSE., .FALSE. ) ) THEN
NEST_INFLUENCE(grid%chb2xy(pig,pjg),xv(1))
ENDIF
ENDIF
IF ( SIZE(grid%chstarxy) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_child_msg(RWORDSIZE,xv)
IF ( cd_feedback_mask( pig, ips_save, ipe_save , pjg, jps_save, jpe_save, .FALSE., .FALSE. ) ) THEN
NEST_INFLUENCE(grid%chstarxy(pig,pjg),xv(1))
ENDIF
ENDIF
IF ( SIZE(grid%tsk_mosaic) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_child_msg(((config_flags%mosaic_cat)-(1)+1)*RWORDSIZE,xv) ;
IF ( cd_feedback_mask( pig, ips_save, ipe_save , pjg, jps_save, jpe_save, .FALSE., .FALSE. ) ) THEN
DO k = 1,config_flags%mosaic_cat
NEST_INFLUENCE(grid%tsk_mosaic(pig,k,pjg),xv(k))
ENDDO
ENDIF
ENDIF
IF ( SIZE(grid%qsfc_mosaic) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_child_msg(((config_flags%mosaic_cat)-(1)+1)*RWORDSIZE,xv) ;
IF ( cd_feedback_mask( pig, ips_save, ipe_save , pjg, jps_save, jpe_save, .FALSE., .FALSE. ) ) THEN
DO k = 1,config_flags%mosaic_cat
NEST_INFLUENCE(grid%qsfc_mosaic(pig,k,pjg),xv(k))
ENDDO
ENDIF
ENDIF
IF ( SIZE(grid%tslb_mosaic) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_child_msg(((config_flags%mosaic_cat_soil)-(1)+1)*RWORDSIZE,xv) ;
IF ( cd_feedback_mask( pig, ips_save, ipe_save , pjg, jps_save, jpe_save, .FALSE., .FALSE. ) ) THEN
DO k = 1,config_flags%mosaic_cat_soil
NEST_INFLUENCE(grid%tslb_mosaic(pig,k,pjg),xv(k))
ENDDO
ENDIF
ENDIF
IF ( SIZE(grid%smois_mosaic) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_child_msg(((config_flags%mosaic_cat_soil)-(1)+1)*RWORDSIZE,xv) ;
IF ( cd_feedback_mask( pig, ips_save, ipe_save , pjg, jps_save, jpe_save, .FALSE., .FALSE. ) ) THEN
DO k = 1,config_flags%mosaic_cat_soil
NEST_INFLUENCE(grid%smois_mosaic(pig,k,pjg),xv(k))
ENDDO
ENDIF
ENDIF
IF ( SIZE(grid%sh2o_mosaic) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_child_msg(((config_flags%mosaic_cat_soil)-(1)+1)*RWORDSIZE,xv) ;
IF ( cd_feedback_mask( pig, ips_save, ipe_save , pjg, jps_save, jpe_save, .FALSE., .FALSE. ) ) THEN
DO k = 1,config_flags%mosaic_cat_soil
NEST_INFLUENCE(grid%sh2o_mosaic(pig,k,pjg),xv(k))
ENDDO
ENDIF
ENDIF
IF ( SIZE(grid%canwat_mosaic) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_child_msg(((config_flags%mosaic_cat)-(1)+1)*RWORDSIZE,xv) ;
IF ( cd_feedback_mask( pig, ips_save, ipe_save , pjg, jps_save, jpe_save, .FALSE., .FALSE. ) ) THEN
DO k = 1,config_flags%mosaic_cat
NEST_INFLUENCE(grid%canwat_mosaic(pig,k,pjg),xv(k))
ENDDO
ENDIF
ENDIF
IF ( SIZE(grid%snow_mosaic) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_child_msg(((config_flags%mosaic_cat)-(1)+1)*RWORDSIZE,xv) ;
IF ( cd_feedback_mask( pig, ips_save, ipe_save , pjg, jps_save, jpe_save, .FALSE., .FALSE. ) ) THEN
DO k = 1,config_flags%mosaic_cat
NEST_INFLUENCE(grid%snow_mosaic(pig,k,pjg),xv(k))
ENDDO
ENDIF
ENDIF
IF ( SIZE(grid%snowh_mosaic) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_child_msg(((config_flags%mosaic_cat)-(1)+1)*RWORDSIZE,xv) ;
IF ( cd_feedback_mask( pig, ips_save, ipe_save , pjg, jps_save, jpe_save, .FALSE., .FALSE. ) ) THEN
DO k = 1,config_flags%mosaic_cat
NEST_INFLUENCE(grid%snowh_mosaic(pig,k,pjg),xv(k))
ENDDO
ENDIF
ENDIF
IF ( SIZE(grid%snowc_mosaic) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_child_msg(((config_flags%mosaic_cat)-(1)+1)*RWORDSIZE,xv) ;
IF ( cd_feedback_mask( pig, ips_save, ipe_save , pjg, jps_save, jpe_save, .FALSE., .FALSE. ) ) THEN
DO k = 1,config_flags%mosaic_cat
NEST_INFLUENCE(grid%snowc_mosaic(pig,k,pjg),xv(k))
ENDDO
ENDIF
ENDIF
IF ( SIZE(grid%tr_urb2d_mosaic) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_child_msg(((config_flags%mosaic_cat)-(1)+1)*RWORDSIZE,xv) ;
IF ( cd_feedback_mask( pig, ips_save, ipe_save , pjg, jps_save, jpe_save, .FALSE., .FALSE. ) ) THEN
DO k = 1,config_flags%mosaic_cat
NEST_INFLUENCE(grid%tr_urb2d_mosaic(pig,k,pjg),xv(k))
ENDDO
ENDIF
ENDIF
IF ( SIZE(grid%tb_urb2d_mosaic) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_child_msg(((config_flags%mosaic_cat)-(1)+1)*RWORDSIZE,xv) ;
IF ( cd_feedback_mask( pig, ips_save, ipe_save , pjg, jps_save, jpe_save, .FALSE., .FALSE. ) ) THEN
DO k = 1,config_flags%mosaic_cat
NEST_INFLUENCE(grid%tb_urb2d_mosaic(pig,k,pjg),xv(k))
ENDDO
ENDIF
ENDIF
IF ( SIZE(grid%tg_urb2d_mosaic) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_child_msg(((config_flags%mosaic_cat)-(1)+1)*RWORDSIZE,xv) ;
IF ( cd_feedback_mask( pig, ips_save, ipe_save , pjg, jps_save, jpe_save, .FALSE., .FALSE. ) ) THEN
DO k = 1,config_flags%mosaic_cat
NEST_INFLUENCE(grid%tg_urb2d_mosaic(pig,k,pjg),xv(k))
ENDDO
ENDIF
ENDIF
IF ( SIZE(grid%tc_urb2d_mosaic) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_child_msg(((config_flags%mosaic_cat)-(1)+1)*RWORDSIZE,xv) ;
IF ( cd_feedback_mask( pig, ips_save, ipe_save , pjg, jps_save, jpe_save, .FALSE., .FALSE. ) ) THEN
DO k = 1,config_flags%mosaic_cat
NEST_INFLUENCE(grid%tc_urb2d_mosaic(pig,k,pjg),xv(k))
ENDDO
ENDIF
ENDIF
IF ( SIZE(grid%ts_urb2d_mosaic) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_child_msg(((config_flags%mosaic_cat)-(1)+1)*RWORDSIZE,xv) ;
IF ( cd_feedback_mask( pig, ips_save, ipe_save , pjg, jps_save, jpe_save, .FALSE., .FALSE. ) ) THEN
DO k = 1,config_flags%mosaic_cat
NEST_INFLUENCE(grid%ts_urb2d_mosaic(pig,k,pjg),xv(k))
ENDDO
ENDIF
ENDIF
IF ( SIZE(grid%ts_rul2d_mosaic) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_child_msg(((config_flags%mosaic_cat)-(1)+1)*RWORDSIZE,xv) ;
IF ( cd_feedback_mask( pig, ips_save, ipe_save , pjg, jps_save, jpe_save, .FALSE., .FALSE. ) ) THEN
DO k = 1,config_flags%mosaic_cat
NEST_INFLUENCE(grid%ts_rul2d_mosaic(pig,k,pjg),xv(k))
ENDDO
ENDIF
ENDIF
IF ( SIZE(grid%qc_urb2d_mosaic) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_child_msg(((config_flags%mosaic_cat)-(1)+1)*RWORDSIZE,xv) ;
IF ( cd_feedback_mask( pig, ips_save, ipe_save , pjg, jps_save, jpe_save, .FALSE., .FALSE. ) ) THEN
DO k = 1,config_flags%mosaic_cat
NEST_INFLUENCE(grid%qc_urb2d_mosaic(pig,k,pjg),xv(k))
ENDDO
ENDIF
ENDIF
IF ( SIZE(grid%uc_urb2d_mosaic) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_child_msg(((config_flags%mosaic_cat)-(1)+1)*RWORDSIZE,xv) ;
IF ( cd_feedback_mask( pig, ips_save, ipe_save , pjg, jps_save, jpe_save, .FALSE., .FALSE. ) ) THEN
DO k = 1,config_flags%mosaic_cat
NEST_INFLUENCE(grid%uc_urb2d_mosaic(pig,k,pjg),xv(k))
ENDDO
ENDIF
ENDIF
IF ( SIZE(grid%trl_urb3d_mosaic) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_child_msg(((config_flags%mosaic_cat_soil)-(1)+1)*RWORDSIZE,xv) ;
IF ( cd_feedback_mask( pig, ips_save, ipe_save , pjg, jps_save, jpe_save, .FALSE., .FALSE. ) ) THEN
DO k = 1,config_flags%mosaic_cat_soil
NEST_INFLUENCE(grid%trl_urb3d_mosaic(pig,k,pjg),xv(k))
ENDDO
ENDIF
ENDIF
IF ( SIZE(grid%tbl_urb3d_mosaic) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_child_msg(((config_flags%mosaic_cat_soil)-(1)+1)*RWORDSIZE,xv) ;
IF ( cd_feedback_mask( pig, ips_save, ipe_save , pjg, jps_save, jpe_save, .FALSE., .FALSE. ) ) THEN
DO k = 1,config_flags%mosaic_cat_soil
NEST_INFLUENCE(grid%tbl_urb3d_mosaic(pig,k,pjg),xv(k))
ENDDO
ENDIF
ENDIF
IF ( SIZE(grid%tgl_urb3d_mosaic) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_child_msg(((config_flags%mosaic_cat_soil)-(1)+1)*RWORDSIZE,xv) ;
IF ( cd_feedback_mask( pig, ips_save, ipe_save , pjg, jps_save, jpe_save, .FALSE., .FALSE. ) ) THEN
DO k = 1,config_flags%mosaic_cat_soil
NEST_INFLUENCE(grid%tgl_urb3d_mosaic(pig,k,pjg),xv(k))
ENDDO
ENDIF
ENDIF
IF ( SIZE(grid%mosaic_cat_index) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_child_msg(((config_flags%num_land_cat)-(1)+1)*RWORDSIZE,xv) ;
IF ( cd_feedback_mask( pig, ips_save, ipe_save , pjg, jps_save, jpe_save, .FALSE., .FALSE. ) ) THEN
DO k = 1,config_flags%num_land_cat
NEST_INFLUENCE(grid%mosaic_cat_index(pig,k,pjg),xv(k))
ENDDO
ENDIF
ENDIF
IF ( SIZE(grid%landusef2) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_child_msg(((config_flags%num_land_cat)-(1)+1)*RWORDSIZE,xv) ;
IF ( cd_feedback_mask( pig, ips_save, ipe_save , pjg, jps_save, jpe_save, .FALSE., .FALSE. ) ) THEN
DO k = 1,config_flags%num_land_cat
NEST_INFLUENCE(grid%landusef2(pig,k,pjg),xv(k))
ENDDO
ENDIF
ENDIF
IF ( SIZE(grid%tmn) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_child_msg(RWORDSIZE,xv)
IF ( cd_feedback_mask( pig, ips_save, ipe_save , pjg, jps_save, jpe_save, .FALSE., .FALSE. ) ) THEN
NEST_INFLUENCE(grid%tmn(pig,pjg),xv(1))
ENDIF
ENDIF
IF ( SIZE(grid%tyr) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_child_msg(RWORDSIZE,xv)
IF ( cd_feedback_mask( pig, ips_save, ipe_save , pjg, jps_save, jpe_save, .FALSE., .FALSE. ) ) THEN
NEST_INFLUENCE(grid%tyr(pig,pjg),xv(1))
ENDIF
ENDIF
IF ( SIZE(grid%tyra) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_child_msg(RWORDSIZE,xv)
IF ( cd_feedback_mask( pig, ips_save, ipe_save , pjg, jps_save, jpe_save, .FALSE., .FALSE. ) ) THEN
NEST_INFLUENCE(grid%tyra(pig,pjg),xv(1))
ENDIF
ENDIF
IF ( SIZE(grid%tdly) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_child_msg(RWORDSIZE,xv)
IF ( cd_feedback_mask( pig, ips_save, ipe_save , pjg, jps_save, jpe_save, .FALSE., .FALSE. ) ) THEN
NEST_INFLUENCE(grid%tdly(pig,pjg),xv(1))
ENDIF
ENDIF
IF ( SIZE(grid%tlag) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_child_msg(((config_flags%lagday)-(1)+1)*RWORDSIZE,xv) ;
IF ( cd_feedback_mask( pig, ips_save, ipe_save , pjg, jps_save, jpe_save, .FALSE., .FALSE. ) ) THEN
DO k = 1,config_flags%lagday
NEST_INFLUENCE(grid%tlag(pig,k,pjg),xv(k))
ENDDO
ENDIF
ENDIF
IF ( SIZE(grid%xland) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_child_msg(RWORDSIZE,xv)
IF ( cd_feedback_mask( pig, ips_save, ipe_save , pjg, jps_save, jpe_save, .FALSE., .FALSE. ) ) THEN
NEST_INFLUENCE(grid%xland(pig,pjg),xv(1))
ENDIF
ENDIF
IF ( SIZE(grid%achfx) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_child_msg(RWORDSIZE,xv)
IF ( cd_feedback_mask( pig, ips_save, ipe_save , pjg, jps_save, jpe_save, .FALSE., .FALSE. ) ) THEN
NEST_INFLUENCE(grid%achfx(pig,pjg),xv(1))
ENDIF
ENDIF
IF ( SIZE(grid%aclhf) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_child_msg(RWORDSIZE,xv)
IF ( cd_feedback_mask( pig, ips_save, ipe_save , pjg, jps_save, jpe_save, .FALSE., .FALSE. ) ) THEN
NEST_INFLUENCE(grid%aclhf(pig,pjg),xv(1))
ENDIF
ENDIF
IF ( SIZE(grid%snowc) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_child_msg(RWORDSIZE,xv)
IF ( cd_feedback_mask( pig, ips_save, ipe_save , pjg, jps_save, jpe_save, .FALSE., .FALSE. ) ) THEN
NEST_INFLUENCE(grid%snowc(pig,pjg),xv(1))
ENDIF
ENDIF
IF ( SIZE(grid%tml) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_child_msg(RWORDSIZE,xv)
IF ( cd_feedback_mask( pig, ips_save, ipe_save , pjg, jps_save, jpe_save, .FALSE., .FALSE. ) ) THEN
NEST_INFLUENCE(grid%tml(pig,pjg),xv(1))
ENDIF
ENDIF
IF ( SIZE(grid%t0ml) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_child_msg(RWORDSIZE,xv)
IF ( cd_feedback_mask( pig, ips_save, ipe_save , pjg, jps_save, jpe_save, .FALSE., .FALSE. ) ) THEN
NEST_INFLUENCE(grid%t0ml(pig,pjg),xv(1))
ENDIF
ENDIF
IF ( SIZE(grid%hml) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_child_msg(RWORDSIZE,xv)
IF ( cd_feedback_mask( pig, ips_save, ipe_save , pjg, jps_save, jpe_save, .FALSE., .FALSE. ) ) THEN
NEST_INFLUENCE(grid%hml(pig,pjg),xv(1))
ENDIF
ENDIF
IF ( SIZE(grid%h0ml) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_child_msg(RWORDSIZE,xv)
IF ( cd_feedback_mask( pig, ips_save, ipe_save , pjg, jps_save, jpe_save, .FALSE., .FALSE. ) ) THEN
NEST_INFLUENCE(grid%h0ml(pig,pjg),xv(1))
ENDIF
ENDIF
IF ( SIZE(grid%huml) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_child_msg(RWORDSIZE,xv)
IF ( cd_feedback_mask( pig, ips_save, ipe_save , pjg, jps_save, jpe_save, .FALSE., .FALSE. ) ) THEN
NEST_INFLUENCE(grid%huml(pig,pjg),xv(1))
ENDIF
ENDIF
IF ( SIZE(grid%hvml) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_child_msg(RWORDSIZE,xv)
IF ( cd_feedback_mask( pig, ips_save, ipe_save , pjg, jps_save, jpe_save, .FALSE., .FALSE. ) ) THEN
NEST_INFLUENCE(grid%hvml(pig,pjg),xv(1))
ENDIF
ENDIF
IF ( SIZE(grid%tmoml) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_child_msg(RWORDSIZE,xv)
IF ( cd_feedback_mask( pig, ips_save, ipe_save , pjg, jps_save, jpe_save, .FALSE., .FALSE. ) ) THEN
NEST_INFLUENCE(grid%tmoml(pig,pjg),xv(1))
ENDIF
ENDIF
IF ( SIZE(grid%numc) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_child_msg(RWORDSIZE,xv)
IF ( cd_feedback_mask( pig, ips_save, ipe_save , pjg, jps_save, jpe_save, .FALSE., .FALSE. ) ) THEN
NEST_INFLUENCE(grid%numc(pig,pjg),xv(1))
ENDIF
ENDIF
IF ( SIZE(grid%nump) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_child_msg(RWORDSIZE,xv)
IF ( cd_feedback_mask( pig, ips_save, ipe_save , pjg, jps_save, jpe_save, .FALSE., .FALSE. ) ) THEN
NEST_INFLUENCE(grid%nump(pig,pjg),xv(1))
ENDIF
ENDIF
IF ( SIZE(grid%snl) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_child_msg(((config_flags%maxpatch)-(1)+1)*RWORDSIZE,xv) ;
IF ( cd_feedback_mask( pig, ips_save, ipe_save , pjg, jps_save, jpe_save, .FALSE., .FALSE. ) ) THEN
DO k = 1,config_flags%maxpatch
NEST_INFLUENCE(grid%snl(pig,k,pjg),xv(k))
ENDDO
ENDIF
ENDIF
IF ( SIZE(grid%snowdp) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_child_msg(((config_flags%maxpatch)-(1)+1)*RWORDSIZE,xv) ;
IF ( cd_feedback_mask( pig, ips_save, ipe_save , pjg, jps_save, jpe_save, .FALSE., .FALSE. ) ) THEN
DO k = 1,config_flags%maxpatch
NEST_INFLUENCE(grid%snowdp(pig,k,pjg),xv(k))
ENDDO
ENDIF
ENDIF
IF ( SIZE(grid%wtc) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_child_msg(((config_flags%maxpatch)-(1)+1)*RWORDSIZE,xv) ;
IF ( cd_feedback_mask( pig, ips_save, ipe_save , pjg, jps_save, jpe_save, .FALSE., .FALSE. ) ) THEN
DO k = 1,config_flags%maxpatch
NEST_INFLUENCE(grid%wtc(pig,k,pjg),xv(k))
ENDDO
ENDIF
ENDIF
IF ( SIZE(grid%wtp) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_child_msg(((config_flags%maxpatch)-(1)+1)*RWORDSIZE,xv) ;
IF ( cd_feedback_mask( pig, ips_save, ipe_save , pjg, jps_save, jpe_save, .FALSE., .FALSE. ) ) THEN
DO k = 1,config_flags%maxpatch
NEST_INFLUENCE(grid%wtp(pig,k,pjg),xv(k))
ENDDO
ENDIF
ENDIF
IF ( SIZE(grid%h2osno) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_child_msg(((config_flags%maxpatch)-(1)+1)*RWORDSIZE,xv) ;
IF ( cd_feedback_mask( pig, ips_save, ipe_save , pjg, jps_save, jpe_save, .FALSE., .FALSE. ) ) THEN
DO k = 1,config_flags%maxpatch
NEST_INFLUENCE(grid%h2osno(pig,k,pjg),xv(k))
ENDDO
ENDIF
ENDIF
IF ( SIZE(grid%t_grnd) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_child_msg(((config_flags%maxpatch)-(1)+1)*RWORDSIZE,xv) ;
IF ( cd_feedback_mask( pig, ips_save, ipe_save , pjg, jps_save, jpe_save, .FALSE., .FALSE. ) ) THEN
DO k = 1,config_flags%maxpatch
NEST_INFLUENCE(grid%t_grnd(pig,k,pjg),xv(k))
ENDDO
ENDIF
ENDIF
IF ( SIZE(grid%t_veg) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_child_msg(((config_flags%maxpatch)-(1)+1)*RWORDSIZE,xv) ;
IF ( cd_feedback_mask( pig, ips_save, ipe_save , pjg, jps_save, jpe_save, .FALSE., .FALSE. ) ) THEN
DO k = 1,config_flags%maxpatch
NEST_INFLUENCE(grid%t_veg(pig,k,pjg),xv(k))
ENDDO
ENDIF
ENDIF
IF ( SIZE(grid%h2ocan) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_child_msg(((config_flags%maxpatch)-(1)+1)*RWORDSIZE,xv) ;
IF ( cd_feedback_mask( pig, ips_save, ipe_save , pjg, jps_save, jpe_save, .FALSE., .FALSE. ) ) THEN
DO k = 1,config_flags%maxpatch
NEST_INFLUENCE(grid%h2ocan(pig,k,pjg),xv(k))
ENDDO
ENDIF
ENDIF
IF ( SIZE(grid%h2ocan_col) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_child_msg(((config_flags%maxpatch)-(1)+1)*RWORDSIZE,xv) ;
IF ( cd_feedback_mask( pig, ips_save, ipe_save , pjg, jps_save, jpe_save, .FALSE., .FALSE. ) ) THEN
DO k = 1,config_flags%maxpatch
NEST_INFLUENCE(grid%h2ocan_col(pig,k,pjg),xv(k))
ENDDO
ENDIF
ENDIF
IF ( SIZE(grid%t2m_max) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_child_msg(RWORDSIZE,xv)
IF ( cd_feedback_mask( pig, ips_save, ipe_save , pjg, jps_save, jpe_save, .FALSE., .FALSE. ) ) THEN
NEST_INFLUENCE(grid%t2m_max(pig,pjg),xv(1))
ENDIF
ENDIF
IF ( SIZE(grid%t2m_min) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_child_msg(RWORDSIZE,xv)
IF ( cd_feedback_mask( pig, ips_save, ipe_save , pjg, jps_save, jpe_save, .FALSE., .FALSE. ) ) THEN
NEST_INFLUENCE(grid%t2m_min(pig,pjg),xv(1))
ENDIF
ENDIF
IF ( SIZE(grid%t2clm) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_child_msg(RWORDSIZE,xv)
IF ( cd_feedback_mask( pig, ips_save, ipe_save , pjg, jps_save, jpe_save, .FALSE., .FALSE. ) ) THEN
NEST_INFLUENCE(grid%t2clm(pig,pjg),xv(1))
ENDIF
ENDIF
IF ( SIZE(grid%t_ref2m) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_child_msg(((config_flags%maxpatch)-(1)+1)*RWORDSIZE,xv) ;
IF ( cd_feedback_mask( pig, ips_save, ipe_save , pjg, jps_save, jpe_save, .FALSE., .FALSE. ) ) THEN
DO k = 1,config_flags%maxpatch
NEST_INFLUENCE(grid%t_ref2m(pig,k,pjg),xv(k))
ENDDO
ENDIF
ENDIF
IF ( SIZE(grid%h2osoi_liq_s1) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_child_msg(((config_flags%maxpatch)-(1)+1)*RWORDSIZE,xv) ;
IF ( cd_feedback_mask( pig, ips_save, ipe_save , pjg, jps_save, jpe_save, .FALSE., .FALSE. ) ) THEN
DO k = 1,config_flags%maxpatch
NEST_INFLUENCE(grid%h2osoi_liq_s1(pig,k,pjg),xv(k))
ENDDO
ENDIF
ENDIF
IF ( SIZE(grid%h2osoi_liq_s2) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_child_msg(((config_flags%maxpatch)-(1)+1)*RWORDSIZE,xv) ;
IF ( cd_feedback_mask( pig, ips_save, ipe_save , pjg, jps_save, jpe_save, .FALSE., .FALSE. ) ) THEN
DO k = 1,config_flags%maxpatch
NEST_INFLUENCE(grid%h2osoi_liq_s2(pig,k,pjg),xv(k))
ENDDO
ENDIF
ENDIF
IF ( SIZE(grid%h2osoi_liq_s3) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_child_msg(((config_flags%maxpatch)-(1)+1)*RWORDSIZE,xv) ;
IF ( cd_feedback_mask( pig, ips_save, ipe_save , pjg, jps_save, jpe_save, .FALSE., .FALSE. ) ) THEN
DO k = 1,config_flags%maxpatch
NEST_INFLUENCE(grid%h2osoi_liq_s3(pig,k,pjg),xv(k))
ENDDO
ENDIF
ENDIF
IF ( SIZE(grid%h2osoi_liq_s4) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_child_msg(((config_flags%maxpatch)-(1)+1)*RWORDSIZE,xv) ;
IF ( cd_feedback_mask( pig, ips_save, ipe_save , pjg, jps_save, jpe_save, .FALSE., .FALSE. ) ) THEN
DO k = 1,config_flags%maxpatch
NEST_INFLUENCE(grid%h2osoi_liq_s4(pig,k,pjg),xv(k))
ENDDO
ENDIF
ENDIF
IF ( SIZE(grid%h2osoi_liq_s5) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_child_msg(((config_flags%maxpatch)-(1)+1)*RWORDSIZE,xv) ;
IF ( cd_feedback_mask( pig, ips_save, ipe_save , pjg, jps_save, jpe_save, .FALSE., .FALSE. ) ) THEN
DO k = 1,config_flags%maxpatch
NEST_INFLUENCE(grid%h2osoi_liq_s5(pig,k,pjg),xv(k))
ENDDO
ENDIF
ENDIF
IF ( SIZE(grid%h2osoi_liq1) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_child_msg(((config_flags%maxpatch)-(1)+1)*RWORDSIZE,xv) ;
IF ( cd_feedback_mask( pig, ips_save, ipe_save , pjg, jps_save, jpe_save, .FALSE., .FALSE. ) ) THEN
DO k = 1,config_flags%maxpatch
NEST_INFLUENCE(grid%h2osoi_liq1(pig,k,pjg),xv(k))
ENDDO
ENDIF
ENDIF
IF ( SIZE(grid%h2osoi_liq2) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_child_msg(((config_flags%maxpatch)-(1)+1)*RWORDSIZE,xv) ;
IF ( cd_feedback_mask( pig, ips_save, ipe_save , pjg, jps_save, jpe_save, .FALSE., .FALSE. ) ) THEN
DO k = 1,config_flags%maxpatch
NEST_INFLUENCE(grid%h2osoi_liq2(pig,k,pjg),xv(k))
ENDDO
ENDIF
ENDIF
IF ( SIZE(grid%h2osoi_liq3) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_child_msg(((config_flags%maxpatch)-(1)+1)*RWORDSIZE,xv) ;
IF ( cd_feedback_mask( pig, ips_save, ipe_save , pjg, jps_save, jpe_save, .FALSE., .FALSE. ) ) THEN
DO k = 1,config_flags%maxpatch
NEST_INFLUENCE(grid%h2osoi_liq3(pig,k,pjg),xv(k))
ENDDO
ENDIF
ENDIF
IF ( SIZE(grid%h2osoi_liq4) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_child_msg(((config_flags%maxpatch)-(1)+1)*RWORDSIZE,xv) ;
IF ( cd_feedback_mask( pig, ips_save, ipe_save , pjg, jps_save, jpe_save, .FALSE., .FALSE. ) ) THEN
DO k = 1,config_flags%maxpatch
NEST_INFLUENCE(grid%h2osoi_liq4(pig,k,pjg),xv(k))
ENDDO
ENDIF
ENDIF
IF ( SIZE(grid%h2osoi_liq5) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_child_msg(((config_flags%maxpatch)-(1)+1)*RWORDSIZE,xv) ;
IF ( cd_feedback_mask( pig, ips_save, ipe_save , pjg, jps_save, jpe_save, .FALSE., .FALSE. ) ) THEN
DO k = 1,config_flags%maxpatch
NEST_INFLUENCE(grid%h2osoi_liq5(pig,k,pjg),xv(k))
ENDDO
ENDIF
ENDIF
IF ( SIZE(grid%h2osoi_liq6) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_child_msg(((config_flags%maxpatch)-(1)+1)*RWORDSIZE,xv) ;
IF ( cd_feedback_mask( pig, ips_save, ipe_save , pjg, jps_save, jpe_save, .FALSE., .FALSE. ) ) THEN
DO k = 1,config_flags%maxpatch
NEST_INFLUENCE(grid%h2osoi_liq6(pig,k,pjg),xv(k))
ENDDO
ENDIF
ENDIF
IF ( SIZE(grid%h2osoi_liq7) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_child_msg(((config_flags%maxpatch)-(1)+1)*RWORDSIZE,xv) ;
IF ( cd_feedback_mask( pig, ips_save, ipe_save , pjg, jps_save, jpe_save, .FALSE., .FALSE. ) ) THEN
DO k = 1,config_flags%maxpatch
NEST_INFLUENCE(grid%h2osoi_liq7(pig,k,pjg),xv(k))
ENDDO
ENDIF
ENDIF
IF ( SIZE(grid%h2osoi_liq8) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_child_msg(((config_flags%maxpatch)-(1)+1)*RWORDSIZE,xv) ;
IF ( cd_feedback_mask( pig, ips_save, ipe_save , pjg, jps_save, jpe_save, .FALSE., .FALSE. ) ) THEN
DO k = 1,config_flags%maxpatch
NEST_INFLUENCE(grid%h2osoi_liq8(pig,k,pjg),xv(k))
ENDDO
ENDIF
ENDIF
IF ( SIZE(grid%h2osoi_liq9) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_child_msg(((config_flags%maxpatch)-(1)+1)*RWORDSIZE,xv) ;
IF ( cd_feedback_mask( pig, ips_save, ipe_save , pjg, jps_save, jpe_save, .FALSE., .FALSE. ) ) THEN
DO k = 1,config_flags%maxpatch
NEST_INFLUENCE(grid%h2osoi_liq9(pig,k,pjg),xv(k))
ENDDO
ENDIF
ENDIF
IF ( SIZE(grid%h2osoi_liq10) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_child_msg(((config_flags%maxpatch)-(1)+1)*RWORDSIZE,xv) ;
IF ( cd_feedback_mask( pig, ips_save, ipe_save , pjg, jps_save, jpe_save, .FALSE., .FALSE. ) ) THEN
DO k = 1,config_flags%maxpatch
NEST_INFLUENCE(grid%h2osoi_liq10(pig,k,pjg),xv(k))
ENDDO
ENDIF
ENDIF
IF ( SIZE(grid%h2osoi_ice_s1) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_child_msg(((config_flags%maxpatch)-(1)+1)*RWORDSIZE,xv) ;
IF ( cd_feedback_mask( pig, ips_save, ipe_save , pjg, jps_save, jpe_save, .FALSE., .FALSE. ) ) THEN
DO k = 1,config_flags%maxpatch
NEST_INFLUENCE(grid%h2osoi_ice_s1(pig,k,pjg),xv(k))
ENDDO
ENDIF
ENDIF
IF ( SIZE(grid%h2osoi_ice_s2) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_child_msg(((config_flags%maxpatch)-(1)+1)*RWORDSIZE,xv) ;
IF ( cd_feedback_mask( pig, ips_save, ipe_save , pjg, jps_save, jpe_save, .FALSE., .FALSE. ) ) THEN
DO k = 1,config_flags%maxpatch
NEST_INFLUENCE(grid%h2osoi_ice_s2(pig,k,pjg),xv(k))
ENDDO
ENDIF
ENDIF
IF ( SIZE(grid%h2osoi_ice_s3) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_child_msg(((config_flags%maxpatch)-(1)+1)*RWORDSIZE,xv) ;
IF ( cd_feedback_mask( pig, ips_save, ipe_save , pjg, jps_save, jpe_save, .FALSE., .FALSE. ) ) THEN
DO k = 1,config_flags%maxpatch
NEST_INFLUENCE(grid%h2osoi_ice_s3(pig,k,pjg),xv(k))
ENDDO
ENDIF
ENDIF
IF ( SIZE(grid%h2osoi_ice_s4) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_child_msg(((config_flags%maxpatch)-(1)+1)*RWORDSIZE,xv) ;
IF ( cd_feedback_mask( pig, ips_save, ipe_save , pjg, jps_save, jpe_save, .FALSE., .FALSE. ) ) THEN
DO k = 1,config_flags%maxpatch
NEST_INFLUENCE(grid%h2osoi_ice_s4(pig,k,pjg),xv(k))
ENDDO
ENDIF
ENDIF
IF ( SIZE(grid%h2osoi_ice_s5) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_child_msg(((config_flags%maxpatch)-(1)+1)*RWORDSIZE,xv) ;
IF ( cd_feedback_mask( pig, ips_save, ipe_save , pjg, jps_save, jpe_save, .FALSE., .FALSE. ) ) THEN
DO k = 1,config_flags%maxpatch
NEST_INFLUENCE(grid%h2osoi_ice_s5(pig,k,pjg),xv(k))
ENDDO
ENDIF
ENDIF
IF ( SIZE(grid%h2osoi_ice1) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_child_msg(((config_flags%maxpatch)-(1)+1)*RWORDSIZE,xv) ;
IF ( cd_feedback_mask( pig, ips_save, ipe_save , pjg, jps_save, jpe_save, .FALSE., .FALSE. ) ) THEN
DO k = 1,config_flags%maxpatch
NEST_INFLUENCE(grid%h2osoi_ice1(pig,k,pjg),xv(k))
ENDDO
ENDIF
ENDIF
IF ( SIZE(grid%h2osoi_ice2) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_child_msg(((config_flags%maxpatch)-(1)+1)*RWORDSIZE,xv) ;
IF ( cd_feedback_mask( pig, ips_save, ipe_save , pjg, jps_save, jpe_save, .FALSE., .FALSE. ) ) THEN
DO k = 1,config_flags%maxpatch
NEST_INFLUENCE(grid%h2osoi_ice2(pig,k,pjg),xv(k))
ENDDO
ENDIF
ENDIF
IF ( SIZE(grid%h2osoi_ice3) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_child_msg(((config_flags%maxpatch)-(1)+1)*RWORDSIZE,xv) ;
IF ( cd_feedback_mask( pig, ips_save, ipe_save , pjg, jps_save, jpe_save, .FALSE., .FALSE. ) ) THEN
DO k = 1,config_flags%maxpatch
NEST_INFLUENCE(grid%h2osoi_ice3(pig,k,pjg),xv(k))
ENDDO
ENDIF
ENDIF
IF ( SIZE(grid%h2osoi_ice4) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_child_msg(((config_flags%maxpatch)-(1)+1)*RWORDSIZE,xv) ;
IF ( cd_feedback_mask( pig, ips_save, ipe_save , pjg, jps_save, jpe_save, .FALSE., .FALSE. ) ) THEN
DO k = 1,config_flags%maxpatch
NEST_INFLUENCE(grid%h2osoi_ice4(pig,k,pjg),xv(k))
ENDDO
ENDIF
ENDIF
IF ( SIZE(grid%h2osoi_ice5) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_child_msg(((config_flags%maxpatch)-(1)+1)*RWORDSIZE,xv) ;
IF ( cd_feedback_mask( pig, ips_save, ipe_save , pjg, jps_save, jpe_save, .FALSE., .FALSE. ) ) THEN
DO k = 1,config_flags%maxpatch
NEST_INFLUENCE(grid%h2osoi_ice5(pig,k,pjg),xv(k))
ENDDO
ENDIF
ENDIF
IF ( SIZE(grid%h2osoi_ice6) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_child_msg(((config_flags%maxpatch)-(1)+1)*RWORDSIZE,xv) ;
IF ( cd_feedback_mask( pig, ips_save, ipe_save , pjg, jps_save, jpe_save, .FALSE., .FALSE. ) ) THEN
DO k = 1,config_flags%maxpatch
NEST_INFLUENCE(grid%h2osoi_ice6(pig,k,pjg),xv(k))
ENDDO
ENDIF
ENDIF
IF ( SIZE(grid%h2osoi_ice7) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_child_msg(((config_flags%maxpatch)-(1)+1)*RWORDSIZE,xv) ;
IF ( cd_feedback_mask( pig, ips_save, ipe_save , pjg, jps_save, jpe_save, .FALSE., .FALSE. ) ) THEN
DO k = 1,config_flags%maxpatch
NEST_INFLUENCE(grid%h2osoi_ice7(pig,k,pjg),xv(k))
ENDDO
ENDIF
ENDIF
IF ( SIZE(grid%h2osoi_ice8) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_child_msg(((config_flags%maxpatch)-(1)+1)*RWORDSIZE,xv) ;
IF ( cd_feedback_mask( pig, ips_save, ipe_save , pjg, jps_save, jpe_save, .FALSE., .FALSE. ) ) THEN
DO k = 1,config_flags%maxpatch
NEST_INFLUENCE(grid%h2osoi_ice8(pig,k,pjg),xv(k))
ENDDO
ENDIF
ENDIF
IF ( SIZE(grid%h2osoi_ice9) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_child_msg(((config_flags%maxpatch)-(1)+1)*RWORDSIZE,xv) ;
IF ( cd_feedback_mask( pig, ips_save, ipe_save , pjg, jps_save, jpe_save, .FALSE., .FALSE. ) ) THEN
DO k = 1,config_flags%maxpatch
NEST_INFLUENCE(grid%h2osoi_ice9(pig,k,pjg),xv(k))
ENDDO
ENDIF
ENDIF
IF ( SIZE(grid%h2osoi_ice10) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_child_msg(((config_flags%maxpatch)-(1)+1)*RWORDSIZE,xv) ;
IF ( cd_feedback_mask( pig, ips_save, ipe_save , pjg, jps_save, jpe_save, .FALSE., .FALSE. ) ) THEN
DO k = 1,config_flags%maxpatch
NEST_INFLUENCE(grid%h2osoi_ice10(pig,k,pjg),xv(k))
ENDDO
ENDIF
ENDIF
IF ( SIZE(grid%t_soisno_s1) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_child_msg(((config_flags%maxpatch)-(1)+1)*RWORDSIZE,xv) ;
IF ( cd_feedback_mask( pig, ips_save, ipe_save , pjg, jps_save, jpe_save, .FALSE., .FALSE. ) ) THEN
DO k = 1,config_flags%maxpatch
NEST_INFLUENCE(grid%t_soisno_s1(pig,k,pjg),xv(k))
ENDDO
ENDIF
ENDIF
IF ( SIZE(grid%t_soisno_s2) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_child_msg(((config_flags%maxpatch)-(1)+1)*RWORDSIZE,xv) ;
IF ( cd_feedback_mask( pig, ips_save, ipe_save , pjg, jps_save, jpe_save, .FALSE., .FALSE. ) ) THEN
DO k = 1,config_flags%maxpatch
NEST_INFLUENCE(grid%t_soisno_s2(pig,k,pjg),xv(k))
ENDDO
ENDIF
ENDIF
IF ( SIZE(grid%t_soisno_s3) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_child_msg(((config_flags%maxpatch)-(1)+1)*RWORDSIZE,xv) ;
IF ( cd_feedback_mask( pig, ips_save, ipe_save , pjg, jps_save, jpe_save, .FALSE., .FALSE. ) ) THEN
DO k = 1,config_flags%maxpatch
NEST_INFLUENCE(grid%t_soisno_s3(pig,k,pjg),xv(k))
ENDDO
ENDIF
ENDIF
IF ( SIZE(grid%t_soisno_s4) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_child_msg(((config_flags%maxpatch)-(1)+1)*RWORDSIZE,xv) ;
IF ( cd_feedback_mask( pig, ips_save, ipe_save , pjg, jps_save, jpe_save, .FALSE., .FALSE. ) ) THEN
DO k = 1,config_flags%maxpatch
NEST_INFLUENCE(grid%t_soisno_s4(pig,k,pjg),xv(k))
ENDDO
ENDIF
ENDIF
IF ( SIZE(grid%t_soisno_s5) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_child_msg(((config_flags%maxpatch)-(1)+1)*RWORDSIZE,xv) ;
IF ( cd_feedback_mask( pig, ips_save, ipe_save , pjg, jps_save, jpe_save, .FALSE., .FALSE. ) ) THEN
DO k = 1,config_flags%maxpatch
NEST_INFLUENCE(grid%t_soisno_s5(pig,k,pjg),xv(k))
ENDDO
ENDIF
ENDIF
IF ( SIZE(grid%t_soisno1) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_child_msg(((config_flags%maxpatch)-(1)+1)*RWORDSIZE,xv) ;
IF ( cd_feedback_mask( pig, ips_save, ipe_save , pjg, jps_save, jpe_save, .FALSE., .FALSE. ) ) THEN
DO k = 1,config_flags%maxpatch
NEST_INFLUENCE(grid%t_soisno1(pig,k,pjg),xv(k))
ENDDO
ENDIF
ENDIF
IF ( SIZE(grid%t_soisno2) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_child_msg(((config_flags%maxpatch)-(1)+1)*RWORDSIZE,xv) ;
IF ( cd_feedback_mask( pig, ips_save, ipe_save , pjg, jps_save, jpe_save, .FALSE., .FALSE. ) ) THEN
DO k = 1,config_flags%maxpatch
NEST_INFLUENCE(grid%t_soisno2(pig,k,pjg),xv(k))
ENDDO
ENDIF
ENDIF
IF ( SIZE(grid%t_soisno3) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_child_msg(((config_flags%maxpatch)-(1)+1)*RWORDSIZE,xv) ;
IF ( cd_feedback_mask( pig, ips_save, ipe_save , pjg, jps_save, jpe_save, .FALSE., .FALSE. ) ) THEN
DO k = 1,config_flags%maxpatch
NEST_INFLUENCE(grid%t_soisno3(pig,k,pjg),xv(k))
ENDDO
ENDIF
ENDIF
IF ( SIZE(grid%t_soisno4) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_child_msg(((config_flags%maxpatch)-(1)+1)*RWORDSIZE,xv) ;
IF ( cd_feedback_mask( pig, ips_save, ipe_save , pjg, jps_save, jpe_save, .FALSE., .FALSE. ) ) THEN
DO k = 1,config_flags%maxpatch
NEST_INFLUENCE(grid%t_soisno4(pig,k,pjg),xv(k))
ENDDO
ENDIF
ENDIF
IF ( SIZE(grid%t_soisno5) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_child_msg(((config_flags%maxpatch)-(1)+1)*RWORDSIZE,xv) ;
IF ( cd_feedback_mask( pig, ips_save, ipe_save , pjg, jps_save, jpe_save, .FALSE., .FALSE. ) ) THEN
DO k = 1,config_flags%maxpatch
NEST_INFLUENCE(grid%t_soisno5(pig,k,pjg),xv(k))
ENDDO
ENDIF
ENDIF
IF ( SIZE(grid%t_soisno6) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_child_msg(((config_flags%maxpatch)-(1)+1)*RWORDSIZE,xv) ;
IF ( cd_feedback_mask( pig, ips_save, ipe_save , pjg, jps_save, jpe_save, .FALSE., .FALSE. ) ) THEN
DO k = 1,config_flags%maxpatch
NEST_INFLUENCE(grid%t_soisno6(pig,k,pjg),xv(k))
ENDDO
ENDIF
ENDIF
IF ( SIZE(grid%t_soisno7) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_child_msg(((config_flags%maxpatch)-(1)+1)*RWORDSIZE,xv) ;
IF ( cd_feedback_mask( pig, ips_save, ipe_save , pjg, jps_save, jpe_save, .FALSE., .FALSE. ) ) THEN
DO k = 1,config_flags%maxpatch
NEST_INFLUENCE(grid%t_soisno7(pig,k,pjg),xv(k))
ENDDO
ENDIF
ENDIF
IF ( SIZE(grid%t_soisno8) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_child_msg(((config_flags%maxpatch)-(1)+1)*RWORDSIZE,xv) ;
IF ( cd_feedback_mask( pig, ips_save, ipe_save , pjg, jps_save, jpe_save, .FALSE., .FALSE. ) ) THEN
DO k = 1,config_flags%maxpatch
NEST_INFLUENCE(grid%t_soisno8(pig,k,pjg),xv(k))
ENDDO
ENDIF
ENDIF
IF ( SIZE(grid%t_soisno9) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_child_msg(((config_flags%maxpatch)-(1)+1)*RWORDSIZE,xv) ;
IF ( cd_feedback_mask( pig, ips_save, ipe_save , pjg, jps_save, jpe_save, .FALSE., .FALSE. ) ) THEN
DO k = 1,config_flags%maxpatch
NEST_INFLUENCE(grid%t_soisno9(pig,k,pjg),xv(k))
ENDDO
ENDIF
ENDIF
IF ( SIZE(grid%t_soisno10) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_child_msg(((config_flags%maxpatch)-(1)+1)*RWORDSIZE,xv) ;
IF ( cd_feedback_mask( pig, ips_save, ipe_save , pjg, jps_save, jpe_save, .FALSE., .FALSE. ) ) THEN
DO k = 1,config_flags%maxpatch
NEST_INFLUENCE(grid%t_soisno10(pig,k,pjg),xv(k))
ENDDO
ENDIF
ENDIF
IF ( SIZE(grid%dzsnow1) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_child_msg(((config_flags%maxpatch)-(1)+1)*RWORDSIZE,xv) ;
IF ( cd_feedback_mask( pig, ips_save, ipe_save , pjg, jps_save, jpe_save, .FALSE., .FALSE. ) ) THEN
DO k = 1,config_flags%maxpatch
NEST_INFLUENCE(grid%dzsnow1(pig,k,pjg),xv(k))
ENDDO
ENDIF
ENDIF
IF ( SIZE(grid%dzsnow2) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_child_msg(((config_flags%maxpatch)-(1)+1)*RWORDSIZE,xv) ;
IF ( cd_feedback_mask( pig, ips_save, ipe_save , pjg, jps_save, jpe_save, .FALSE., .FALSE. ) ) THEN
DO k = 1,config_flags%maxpatch
NEST_INFLUENCE(grid%dzsnow2(pig,k,pjg),xv(k))
ENDDO
ENDIF
ENDIF
IF ( SIZE(grid%dzsnow3) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_child_msg(((config_flags%maxpatch)-(1)+1)*RWORDSIZE,xv) ;
IF ( cd_feedback_mask( pig, ips_save, ipe_save , pjg, jps_save, jpe_save, .FALSE., .FALSE. ) ) THEN
DO k = 1,config_flags%maxpatch
NEST_INFLUENCE(grid%dzsnow3(pig,k,pjg),xv(k))
ENDDO
ENDIF
ENDIF
IF ( SIZE(grid%dzsnow4) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_child_msg(((config_flags%maxpatch)-(1)+1)*RWORDSIZE,xv) ;
IF ( cd_feedback_mask( pig, ips_save, ipe_save , pjg, jps_save, jpe_save, .FALSE., .FALSE. ) ) THEN
DO k = 1,config_flags%maxpatch
NEST_INFLUENCE(grid%dzsnow4(pig,k,pjg),xv(k))
ENDDO
ENDIF
ENDIF
IF ( SIZE(grid%dzsnow5) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_child_msg(((config_flags%maxpatch)-(1)+1)*RWORDSIZE,xv) ;
IF ( cd_feedback_mask( pig, ips_save, ipe_save , pjg, jps_save, jpe_save, .FALSE., .FALSE. ) ) THEN
DO k = 1,config_flags%maxpatch
NEST_INFLUENCE(grid%dzsnow5(pig,k,pjg),xv(k))
ENDDO
ENDIF
ENDIF
IF ( SIZE(grid%snowrds1) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_child_msg(((config_flags%maxpatch)-(1)+1)*RWORDSIZE,xv) ;
IF ( cd_feedback_mask( pig, ips_save, ipe_save , pjg, jps_save, jpe_save, .FALSE., .FALSE. ) ) THEN
DO k = 1,config_flags%maxpatch
NEST_INFLUENCE(grid%snowrds1(pig,k,pjg),xv(k))
ENDDO
ENDIF
ENDIF
IF ( SIZE(grid%snowrds2) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_child_msg(((config_flags%maxpatch)-(1)+1)*RWORDSIZE,xv) ;
IF ( cd_feedback_mask( pig, ips_save, ipe_save , pjg, jps_save, jpe_save, .FALSE., .FALSE. ) ) THEN
DO k = 1,config_flags%maxpatch
NEST_INFLUENCE(grid%snowrds2(pig,k,pjg),xv(k))
ENDDO
ENDIF
ENDIF
IF ( SIZE(grid%snowrds3) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_child_msg(((config_flags%maxpatch)-(1)+1)*RWORDSIZE,xv) ;
IF ( cd_feedback_mask( pig, ips_save, ipe_save , pjg, jps_save, jpe_save, .FALSE., .FALSE. ) ) THEN
DO k = 1,config_flags%maxpatch
NEST_INFLUENCE(grid%snowrds3(pig,k,pjg),xv(k))
ENDDO
ENDIF
ENDIF
IF ( SIZE(grid%snowrds4) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_child_msg(((config_flags%maxpatch)-(1)+1)*RWORDSIZE,xv) ;
IF ( cd_feedback_mask( pig, ips_save, ipe_save , pjg, jps_save, jpe_save, .FALSE., .FALSE. ) ) THEN
DO k = 1,config_flags%maxpatch
NEST_INFLUENCE(grid%snowrds4(pig,k,pjg),xv(k))
ENDDO
ENDIF
ENDIF
IF ( SIZE(grid%snowrds5) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_child_msg(((config_flags%maxpatch)-(1)+1)*RWORDSIZE,xv) ;
IF ( cd_feedback_mask( pig, ips_save, ipe_save , pjg, jps_save, jpe_save, .FALSE., .FALSE. ) ) THEN
DO k = 1,config_flags%maxpatch
NEST_INFLUENCE(grid%snowrds5(pig,k,pjg),xv(k))
ENDDO
ENDIF
ENDIF
IF ( SIZE(grid%t_lake1) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_child_msg(((config_flags%maxpatch)-(1)+1)*RWORDSIZE,xv) ;
IF ( cd_feedback_mask( pig, ips_save, ipe_save , pjg, jps_save, jpe_save, .FALSE., .FALSE. ) ) THEN
DO k = 1,config_flags%maxpatch
NEST_INFLUENCE(grid%t_lake1(pig,k,pjg),xv(k))
ENDDO
ENDIF
ENDIF
IF ( SIZE(grid%t_lake2) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_child_msg(((config_flags%maxpatch)-(1)+1)*RWORDSIZE,xv) ;
IF ( cd_feedback_mask( pig, ips_save, ipe_save , pjg, jps_save, jpe_save, .FALSE., .FALSE. ) ) THEN
DO k = 1,config_flags%maxpatch
NEST_INFLUENCE(grid%t_lake2(pig,k,pjg),xv(k))
ENDDO
ENDIF
ENDIF
IF ( SIZE(grid%t_lake3) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_child_msg(((config_flags%maxpatch)-(1)+1)*RWORDSIZE,xv) ;
IF ( cd_feedback_mask( pig, ips_save, ipe_save , pjg, jps_save, jpe_save, .FALSE., .FALSE. ) ) THEN
DO k = 1,config_flags%maxpatch
NEST_INFLUENCE(grid%t_lake3(pig,k,pjg),xv(k))
ENDDO
ENDIF
ENDIF
IF ( SIZE(grid%t_lake4) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_child_msg(((config_flags%maxpatch)-(1)+1)*RWORDSIZE,xv) ;
IF ( cd_feedback_mask( pig, ips_save, ipe_save , pjg, jps_save, jpe_save, .FALSE., .FALSE. ) ) THEN
DO k = 1,config_flags%maxpatch
NEST_INFLUENCE(grid%t_lake4(pig,k,pjg),xv(k))
ENDDO
ENDIF
ENDIF
IF ( SIZE(grid%t_lake5) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_child_msg(((config_flags%maxpatch)-(1)+1)*RWORDSIZE,xv) ;
IF ( cd_feedback_mask( pig, ips_save, ipe_save , pjg, jps_save, jpe_save, .FALSE., .FALSE. ) ) THEN
DO k = 1,config_flags%maxpatch
NEST_INFLUENCE(grid%t_lake5(pig,k,pjg),xv(k))
ENDDO
ENDIF
ENDIF
IF ( SIZE(grid%t_lake6) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_child_msg(((config_flags%maxpatch)-(1)+1)*RWORDSIZE,xv) ;
IF ( cd_feedback_mask( pig, ips_save, ipe_save , pjg, jps_save, jpe_save, .FALSE., .FALSE. ) ) THEN
DO k = 1,config_flags%maxpatch
NEST_INFLUENCE(grid%t_lake6(pig,k,pjg),xv(k))
ENDDO
ENDIF
ENDIF
IF ( SIZE(grid%t_lake7) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_child_msg(((config_flags%maxpatch)-(1)+1)*RWORDSIZE,xv) ;
IF ( cd_feedback_mask( pig, ips_save, ipe_save , pjg, jps_save, jpe_save, .FALSE., .FALSE. ) ) THEN
DO k = 1,config_flags%maxpatch
NEST_INFLUENCE(grid%t_lake7(pig,k,pjg),xv(k))
ENDDO
ENDIF
ENDIF
IF ( SIZE(grid%t_lake8) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_child_msg(((config_flags%maxpatch)-(1)+1)*RWORDSIZE,xv) ;
IF ( cd_feedback_mask( pig, ips_save, ipe_save , pjg, jps_save, jpe_save, .FALSE., .FALSE. ) ) THEN
DO k = 1,config_flags%maxpatch
NEST_INFLUENCE(grid%t_lake8(pig,k,pjg),xv(k))
ENDDO
ENDIF
ENDIF
IF ( SIZE(grid%t_lake9) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_child_msg(((config_flags%maxpatch)-(1)+1)*RWORDSIZE,xv) ;
IF ( cd_feedback_mask( pig, ips_save, ipe_save , pjg, jps_save, jpe_save, .FALSE., .FALSE. ) ) THEN
DO k = 1,config_flags%maxpatch
NEST_INFLUENCE(grid%t_lake9(pig,k,pjg),xv(k))
ENDDO
ENDIF
ENDIF
IF ( SIZE(grid%t_lake10) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_child_msg(((config_flags%maxpatch)-(1)+1)*RWORDSIZE,xv) ;
IF ( cd_feedback_mask( pig, ips_save, ipe_save , pjg, jps_save, jpe_save, .FALSE., .FALSE. ) ) THEN
DO k = 1,config_flags%maxpatch
NEST_INFLUENCE(grid%t_lake10(pig,k,pjg),xv(k))
ENDDO
ENDIF
ENDIF
IF ( SIZE(grid%h2osoi_vol1) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_child_msg(((config_flags%maxpatch)-(1)+1)*RWORDSIZE,xv) ;
IF ( cd_feedback_mask( pig, ips_save, ipe_save , pjg, jps_save, jpe_save, .FALSE., .FALSE. ) ) THEN
DO k = 1,config_flags%maxpatch
NEST_INFLUENCE(grid%h2osoi_vol1(pig,k,pjg),xv(k))
ENDDO
ENDIF
ENDIF
IF ( SIZE(grid%h2osoi_vol2) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_child_msg(((config_flags%maxpatch)-(1)+1)*RWORDSIZE,xv) ;
IF ( cd_feedback_mask( pig, ips_save, ipe_save , pjg, jps_save, jpe_save, .FALSE., .FALSE. ) ) THEN
DO k = 1,config_flags%maxpatch
NEST_INFLUENCE(grid%h2osoi_vol2(pig,k,pjg),xv(k))
ENDDO
ENDIF
ENDIF
IF ( SIZE(grid%h2osoi_vol3) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_child_msg(((config_flags%maxpatch)-(1)+1)*RWORDSIZE,xv) ;
IF ( cd_feedback_mask( pig, ips_save, ipe_save , pjg, jps_save, jpe_save, .FALSE., .FALSE. ) ) THEN
DO k = 1,config_flags%maxpatch
NEST_INFLUENCE(grid%h2osoi_vol3(pig,k,pjg),xv(k))
ENDDO
ENDIF
ENDIF
IF ( SIZE(grid%h2osoi_vol4) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_child_msg(((config_flags%maxpatch)-(1)+1)*RWORDSIZE,xv) ;
IF ( cd_feedback_mask( pig, ips_save, ipe_save , pjg, jps_save, jpe_save, .FALSE., .FALSE. ) ) THEN
DO k = 1,config_flags%maxpatch
NEST_INFLUENCE(grid%h2osoi_vol4(pig,k,pjg),xv(k))
ENDDO
ENDIF
ENDIF
IF ( SIZE(grid%h2osoi_vol5) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_child_msg(((config_flags%maxpatch)-(1)+1)*RWORDSIZE,xv) ;
IF ( cd_feedback_mask( pig, ips_save, ipe_save , pjg, jps_save, jpe_save, .FALSE., .FALSE. ) ) THEN
DO k = 1,config_flags%maxpatch
NEST_INFLUENCE(grid%h2osoi_vol5(pig,k,pjg),xv(k))
ENDDO
ENDIF
ENDIF
IF ( SIZE(grid%h2osoi_vol6) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_child_msg(((config_flags%maxpatch)-(1)+1)*RWORDSIZE,xv) ;
IF ( cd_feedback_mask( pig, ips_save, ipe_save , pjg, jps_save, jpe_save, .FALSE., .FALSE. ) ) THEN
DO k = 1,config_flags%maxpatch
NEST_INFLUENCE(grid%h2osoi_vol6(pig,k,pjg),xv(k))
ENDDO
ENDIF
ENDIF
IF ( SIZE(grid%h2osoi_vol7) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_child_msg(((config_flags%maxpatch)-(1)+1)*RWORDSIZE,xv) ;
IF ( cd_feedback_mask( pig, ips_save, ipe_save , pjg, jps_save, jpe_save, .FALSE., .FALSE. ) ) THEN
DO k = 1,config_flags%maxpatch
NEST_INFLUENCE(grid%h2osoi_vol7(pig,k,pjg),xv(k))
ENDDO
ENDIF
ENDIF
IF ( SIZE(grid%h2osoi_vol8) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_child_msg(((config_flags%maxpatch)-(1)+1)*RWORDSIZE,xv) ;
IF ( cd_feedback_mask( pig, ips_save, ipe_save , pjg, jps_save, jpe_save, .FALSE., .FALSE. ) ) THEN
DO k = 1,config_flags%maxpatch
NEST_INFLUENCE(grid%h2osoi_vol8(pig,k,pjg),xv(k))
ENDDO
ENDIF
ENDIF
IF ( SIZE(grid%h2osoi_vol9) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_child_msg(((config_flags%maxpatch)-(1)+1)*RWORDSIZE,xv) ;
IF ( cd_feedback_mask( pig, ips_save, ipe_save , pjg, jps_save, jpe_save, .FALSE., .FALSE. ) ) THEN
DO k = 1,config_flags%maxpatch
NEST_INFLUENCE(grid%h2osoi_vol9(pig,k,pjg),xv(k))
ENDDO
ENDIF
ENDIF
IF ( SIZE(grid%h2osoi_vol10) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_child_msg(((config_flags%maxpatch)-(1)+1)*RWORDSIZE,xv) ;
IF ( cd_feedback_mask( pig, ips_save, ipe_save , pjg, jps_save, jpe_save, .FALSE., .FALSE. ) ) THEN
DO k = 1,config_flags%maxpatch
NEST_INFLUENCE(grid%h2osoi_vol10(pig,k,pjg),xv(k))
ENDDO
ENDIF
ENDIF
IF ( SIZE(grid%albedosubgrid) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_child_msg(((config_flags%maxpatch)-(1)+1)*RWORDSIZE,xv) ;
IF ( cd_feedback_mask( pig, ips_save, ipe_save , pjg, jps_save, jpe_save, .FALSE., .FALSE. ) ) THEN
DO k = 1,config_flags%maxpatch
NEST_INFLUENCE(grid%albedosubgrid(pig,k,pjg),xv(k))
ENDDO
ENDIF
ENDIF
IF ( SIZE(grid%lhsubgrid) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_child_msg(((config_flags%maxpatch)-(1)+1)*RWORDSIZE,xv) ;
IF ( cd_feedback_mask( pig, ips_save, ipe_save , pjg, jps_save, jpe_save, .FALSE., .FALSE. ) ) THEN
DO k = 1,config_flags%maxpatch
NEST_INFLUENCE(grid%lhsubgrid(pig,k,pjg),xv(k))
ENDDO
ENDIF
ENDIF
IF ( SIZE(grid%hfxsubgrid) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_child_msg(((config_flags%maxpatch)-(1)+1)*RWORDSIZE,xv) ;
IF ( cd_feedback_mask( pig, ips_save, ipe_save , pjg, jps_save, jpe_save, .FALSE., .FALSE. ) ) THEN
DO k = 1,config_flags%maxpatch
NEST_INFLUENCE(grid%hfxsubgrid(pig,k,pjg),xv(k))
ENDDO
ENDIF
ENDIF
IF ( SIZE(grid%lwupsubgrid) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_child_msg(((config_flags%maxpatch)-(1)+1)*RWORDSIZE,xv) ;
IF ( cd_feedback_mask( pig, ips_save, ipe_save , pjg, jps_save, jpe_save, .FALSE., .FALSE. ) ) THEN
DO k = 1,config_flags%maxpatch
NEST_INFLUENCE(grid%lwupsubgrid(pig,k,pjg),xv(k))
ENDDO
ENDIF
ENDIF
IF ( SIZE(grid%q2subgrid) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_child_msg(((config_flags%maxpatch)-(1)+1)*RWORDSIZE,xv) ;
IF ( cd_feedback_mask( pig, ips_save, ipe_save , pjg, jps_save, jpe_save, .FALSE., .FALSE. ) ) THEN
DO k = 1,config_flags%maxpatch
NEST_INFLUENCE(grid%q2subgrid(pig,k,pjg),xv(k))
ENDDO
ENDIF
ENDIF
IF ( SIZE(grid%sabvsubgrid) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_child_msg(((config_flags%maxpatch)-(1)+1)*RWORDSIZE,xv) ;
IF ( cd_feedback_mask( pig, ips_save, ipe_save , pjg, jps_save, jpe_save, .FALSE., .FALSE. ) ) THEN
DO k = 1,config_flags%maxpatch
NEST_INFLUENCE(grid%sabvsubgrid(pig,k,pjg),xv(k))
ENDDO
ENDIF
ENDIF
IF ( SIZE(grid%sabgsubgrid) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_child_msg(((config_flags%maxpatch)-(1)+1)*RWORDSIZE,xv) ;
IF ( cd_feedback_mask( pig, ips_save, ipe_save , pjg, jps_save, jpe_save, .FALSE., .FALSE. ) ) THEN
DO k = 1,config_flags%maxpatch
NEST_INFLUENCE(grid%sabgsubgrid(pig,k,pjg),xv(k))
ENDDO
ENDIF
ENDIF
IF ( SIZE(grid%nrasubgrid) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_child_msg(((config_flags%maxpatch)-(1)+1)*RWORDSIZE,xv) ;
IF ( cd_feedback_mask( pig, ips_save, ipe_save , pjg, jps_save, jpe_save, .FALSE., .FALSE. ) ) THEN
DO k = 1,config_flags%maxpatch
NEST_INFLUENCE(grid%nrasubgrid(pig,k,pjg),xv(k))
ENDDO
ENDIF
ENDIF
IF ( SIZE(grid%swupsubgrid) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_child_msg(((config_flags%maxpatch)-(1)+1)*RWORDSIZE,xv) ;
IF ( cd_feedback_mask( pig, ips_save, ipe_save , pjg, jps_save, jpe_save, .FALSE., .FALSE. ) ) THEN
DO k = 1,config_flags%maxpatch
NEST_INFLUENCE(grid%swupsubgrid(pig,k,pjg),xv(k))
ENDDO
ENDIF
ENDIF
IF ( SIZE(grid%th_old) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_child_msg((((ckde-1))-(ckds)+1)*RWORDSIZE,xv) ;
IF ( cd_feedback_mask( pig, ips_save, ipe_save , pjg, jps_save, jpe_save, .FALSE., .FALSE. ) ) THEN
DO k = ckds,(ckde-1)
NEST_INFLUENCE(grid%th_old(pig,k,pjg),xv(k))
ENDDO
ENDIF
ENDIF
IF ( SIZE(grid%qv_old) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_child_msg((((ckde-1))-(ckds)+1)*RWORDSIZE,xv) ;
IF ( cd_feedback_mask( pig, ips_save, ipe_save , pjg, jps_save, jpe_save, .FALSE., .FALSE. ) ) THEN
DO k = ckds,(ckde-1)
NEST_INFLUENCE(grid%qv_old(pig,k,pjg),xv(k))
ENDDO
ENDIF
ENDIF
IF ( SIZE(grid%landmask) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_child_msg(RWORDSIZE,xv)
IF ( cd_feedback_mask( pig, ips_save, ipe_save , pjg, jps_save, jpe_save, .FALSE., .FALSE. ) ) THEN
NEST_INFLUENCE(grid%landmask(pig,pjg),xv(1))
ENDIF
ENDIF
IF ( SIZE(grid%lakemask) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_child_msg(RWORDSIZE,xv)
IF ( cd_feedback_mask( pig, ips_save, ipe_save , pjg, jps_save, jpe_save, .FALSE., .FALSE. ) ) THEN
NEST_INFLUENCE(grid%lakemask(pig,pjg),xv(1))
ENDIF
ENDIF
DO itrace =  PARAM_FIRST_SCALAR, num_tracer
CALL rsl_lite_from_child_msg((((ckde-1))-(ckds)+1)*RWORDSIZE,xv) ;
IF ( cd_feedback_mask( pig, ips_save, ipe_save , pjg, jps_save, jpe_save, .FALSE., .FALSE. ) ) THEN
DO k = ckds,(ckde-1)
NEST_INFLUENCE(tracer(pig,k,pjg,itrace),xv(k))
ENDDO
ENDIF
ENDDO
CALL rsl_lite_from_child_info(pig,pjg,retval)
ENDDO
!ENDOFREGISTRYGENERATEDINCLUDE
