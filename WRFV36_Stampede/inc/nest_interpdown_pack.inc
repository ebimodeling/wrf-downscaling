!STARTOFREGISTRYGENERATEDINCLUDE 'inc/nest_interpdown_pack.inc'
!
! WARNING This file is generated automatically by use_registry
! using the data base in the file named Registry.
! Do not edit.  Your changes to this file will be lost.
!
msize = (161 + ((num_moist - PARAM_FIRST_SCALAR + 1)) & 
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
 + ((num_tracer - PARAM_FIRST_SCALAR + 1)) )* nlev + 254
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
IF ( SIZE(grid%xlat) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
xv(1)=grid%xlat(pig,pjg)
CALL rsl_lite_to_child_msg(RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%xlong) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
xv(1)=grid%xlong(pig,pjg)
CALL rsl_lite_to_child_msg(RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%lu_index) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
xv(1)=grid%lu_index(pig,pjg)
CALL rsl_lite_to_child_msg(RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%t_max_p) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
xv(1)=grid%t_max_p(pig,pjg)
CALL rsl_lite_to_child_msg(RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%ght_max_p) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
xv(1)=grid%ght_max_p(pig,pjg)
CALL rsl_lite_to_child_msg(RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%max_p) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
xv(1)=grid%max_p(pig,pjg)
CALL rsl_lite_to_child_msg(RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%t_min_p) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
xv(1)=grid%t_min_p(pig,pjg)
CALL rsl_lite_to_child_msg(RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%ght_min_p) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
xv(1)=grid%ght_min_p(pig,pjg)
CALL rsl_lite_to_child_msg(RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%min_p) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
xv(1)=grid%min_p(pig,pjg)
CALL rsl_lite_to_child_msg(RWORDSIZE,xv)
ENDIF
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
IF ( SIZE(grid%phb) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
DO k = ckds,ckde
xv(k)= grid%phb(pig,k,pjg)
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
IF ( SIZE(grid%mub) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
xv(1)=grid%mub(pig,pjg)
CALL rsl_lite_to_child_msg(RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%alb) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
DO k = ckds,(ckde-1)
xv(k)= grid%alb(pig,k,pjg)
ENDDO
CALL rsl_lite_to_child_msg((((ckde-1))-(ckds)+1)*RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%pb) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
DO k = ckds,(ckde-1)
xv(k)= grid%pb(pig,k,pjg)
ENDDO
CALL rsl_lite_to_child_msg((((ckde-1))-(ckds)+1)*RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%q2) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
xv(1)=grid%q2(pig,pjg)
CALL rsl_lite_to_child_msg(RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%t2) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
xv(1)=grid%t2(pig,pjg)
CALL rsl_lite_to_child_msg(RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%th2) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
xv(1)=grid%th2(pig,pjg)
CALL rsl_lite_to_child_msg(RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%psfc) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
xv(1)=grid%psfc(pig,pjg)
CALL rsl_lite_to_child_msg(RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%u10) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
xv(1)=grid%u10(pig,pjg)
CALL rsl_lite_to_child_msg(RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%v10) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
xv(1)=grid%v10(pig,pjg)
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
IF ( SIZE(grid%qnwfa2d) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
xv(1)=grid%qnwfa2d(pig,pjg)
CALL rsl_lite_to_child_msg(RWORDSIZE,xv)
ENDIF
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
IF ( SIZE(grid%toposlpx) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
xv(1)=grid%toposlpx(pig,pjg)
CALL rsl_lite_to_child_msg(RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%toposlpy) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
xv(1)=grid%toposlpy(pig,pjg)
CALL rsl_lite_to_child_msg(RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%slope) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
xv(1)=grid%slope(pig,pjg)
CALL rsl_lite_to_child_msg(RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%slp_azi) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
xv(1)=grid%slp_azi(pig,pjg)
CALL rsl_lite_to_child_msg(RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%shdmax) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
xv(1)=grid%shdmax(pig,pjg)
CALL rsl_lite_to_child_msg(RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%shdmin) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
xv(1)=grid%shdmin(pig,pjg)
CALL rsl_lite_to_child_msg(RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%tslb) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
DO k = 1,config_flags%num_soil_layers
xv(k)= grid%tslb(pig,k,pjg)
ENDDO
CALL rsl_lite_to_child_msg(((config_flags%num_soil_layers)-(1)+1)*RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%urb_param) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
DO k = 1,132
xv(k)= grid%urb_param(pig,k,pjg)
ENDDO
CALL rsl_lite_to_child_msg(((132)-(1)+1)*RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%smois) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
DO k = 1,config_flags%num_soil_layers
xv(k)= grid%smois(pig,k,pjg)
ENDDO
CALL rsl_lite_to_child_msg(((config_flags%num_soil_layers)-(1)+1)*RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%sh2o) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
DO k = 1,config_flags%num_soil_layers
xv(k)= grid%sh2o(pig,k,pjg)
ENDDO
CALL rsl_lite_to_child_msg(((config_flags%num_soil_layers)-(1)+1)*RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%smcrel) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
DO k = 1,config_flags%num_soil_layers
xv(k)= grid%smcrel(pig,k,pjg)
ENDDO
CALL rsl_lite_to_child_msg(((config_flags%num_soil_layers)-(1)+1)*RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%xice) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
xv(1)=grid%xice(pig,pjg)
CALL rsl_lite_to_child_msg(RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%icedepth) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
xv(1)=grid%icedepth(pig,pjg)
CALL rsl_lite_to_child_msg(RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%xicem) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
xv(1)=grid%xicem(pig,pjg)
CALL rsl_lite_to_child_msg(RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%albsi) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
xv(1)=grid%albsi(pig,pjg)
CALL rsl_lite_to_child_msg(RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%snowsi) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
xv(1)=grid%snowsi(pig,pjg)
CALL rsl_lite_to_child_msg(RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%smstav) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
xv(1)=grid%smstav(pig,pjg)
CALL rsl_lite_to_child_msg(RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%sfcrunoff) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
xv(1)=grid%sfcrunoff(pig,pjg)
CALL rsl_lite_to_child_msg(RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%udrunoff) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
xv(1)=grid%udrunoff(pig,pjg)
CALL rsl_lite_to_child_msg(RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%ivgtyp) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
xv(1)=grid%ivgtyp(pig,pjg)
CALL rsl_lite_to_child_msg(RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%isltyp) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
xv(1)=grid%isltyp(pig,pjg)
CALL rsl_lite_to_child_msg(RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%vegfra) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
xv(1)=grid%vegfra(pig,pjg)
CALL rsl_lite_to_child_msg(RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%acgrdflx) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
xv(1)=grid%acgrdflx(pig,pjg)
CALL rsl_lite_to_child_msg(RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%acsnow) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
xv(1)=grid%acsnow(pig,pjg)
CALL rsl_lite_to_child_msg(RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%acsnom) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
xv(1)=grid%acsnom(pig,pjg)
CALL rsl_lite_to_child_msg(RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%snow) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
xv(1)=grid%snow(pig,pjg)
CALL rsl_lite_to_child_msg(RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%snowh) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
xv(1)=grid%snowh(pig,pjg)
CALL rsl_lite_to_child_msg(RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%canwat) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
xv(1)=grid%canwat(pig,pjg)
CALL rsl_lite_to_child_msg(RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%sstsk) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
xv(1)=grid%sstsk(pig,pjg)
CALL rsl_lite_to_child_msg(RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%lake_depth) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
xv(1)=grid%lake_depth(pig,pjg)
CALL rsl_lite_to_child_msg(RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%uoce) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
xv(1)=grid%uoce(pig,pjg)
CALL rsl_lite_to_child_msg(RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%voce) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
xv(1)=grid%voce(pig,pjg)
CALL rsl_lite_to_child_msg(RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%tr_urb2d) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
xv(1)=grid%tr_urb2d(pig,pjg)
CALL rsl_lite_to_child_msg(RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%tb_urb2d) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
xv(1)=grid%tb_urb2d(pig,pjg)
CALL rsl_lite_to_child_msg(RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%tg_urb2d) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
xv(1)=grid%tg_urb2d(pig,pjg)
CALL rsl_lite_to_child_msg(RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%tc_urb2d) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
xv(1)=grid%tc_urb2d(pig,pjg)
CALL rsl_lite_to_child_msg(RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%qc_urb2d) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
xv(1)=grid%qc_urb2d(pig,pjg)
CALL rsl_lite_to_child_msg(RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%uc_urb2d) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
xv(1)=grid%uc_urb2d(pig,pjg)
CALL rsl_lite_to_child_msg(RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%xxxr_urb2d) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
xv(1)=grid%xxxr_urb2d(pig,pjg)
CALL rsl_lite_to_child_msg(RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%xxxb_urb2d) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
xv(1)=grid%xxxb_urb2d(pig,pjg)
CALL rsl_lite_to_child_msg(RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%xxxg_urb2d) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
xv(1)=grid%xxxg_urb2d(pig,pjg)
CALL rsl_lite_to_child_msg(RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%xxxc_urb2d) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
xv(1)=grid%xxxc_urb2d(pig,pjg)
CALL rsl_lite_to_child_msg(RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%trl_urb3d) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
DO k = 1,config_flags%num_soil_layers
xv(k)= grid%trl_urb3d(pig,k,pjg)
ENDDO
CALL rsl_lite_to_child_msg(((config_flags%num_soil_layers)-(1)+1)*RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%tbl_urb3d) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
DO k = 1,config_flags%num_soil_layers
xv(k)= grid%tbl_urb3d(pig,k,pjg)
ENDDO
CALL rsl_lite_to_child_msg(((config_flags%num_soil_layers)-(1)+1)*RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%tgl_urb3d) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
DO k = 1,config_flags%num_soil_layers
xv(k)= grid%tgl_urb3d(pig,k,pjg)
ENDDO
CALL rsl_lite_to_child_msg(((config_flags%num_soil_layers)-(1)+1)*RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%frc_urb2d) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
xv(1)=grid%frc_urb2d(pig,pjg)
CALL rsl_lite_to_child_msg(RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%utype_urb2d) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
xv(1)=grid%utype_urb2d(pig,pjg)
CALL rsl_lite_to_child_msg(RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%var2d) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
xv(1)=grid%var2d(pig,pjg)
CALL rsl_lite_to_child_msg(RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%oc12d) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
xv(1)=grid%oc12d(pig,pjg)
CALL rsl_lite_to_child_msg(RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%oa1) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
xv(1)=grid%oa1(pig,pjg)
CALL rsl_lite_to_child_msg(RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%oa2) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
xv(1)=grid%oa2(pig,pjg)
CALL rsl_lite_to_child_msg(RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%oa3) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
xv(1)=grid%oa3(pig,pjg)
CALL rsl_lite_to_child_msg(RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%oa4) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
xv(1)=grid%oa4(pig,pjg)
CALL rsl_lite_to_child_msg(RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%ol1) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
xv(1)=grid%ol1(pig,pjg)
CALL rsl_lite_to_child_msg(RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%ol2) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
xv(1)=grid%ol2(pig,pjg)
CALL rsl_lite_to_child_msg(RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%ol3) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
xv(1)=grid%ol3(pig,pjg)
CALL rsl_lite_to_child_msg(RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%ol4) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
xv(1)=grid%ol4(pig,pjg)
CALL rsl_lite_to_child_msg(RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%ctopo) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
xv(1)=grid%ctopo(pig,pjg)
CALL rsl_lite_to_child_msg(RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%ctopo2) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
xv(1)=grid%ctopo2(pig,pjg)
CALL rsl_lite_to_child_msg(RWORDSIZE,xv)
ENDIF
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
IF ( SIZE(grid%f_ice_phy) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
DO k = ckds,(ckde-1)
xv(k)= grid%f_ice_phy(pig,k,pjg)
ENDDO
CALL rsl_lite_to_child_msg((((ckde-1))-(ckds)+1)*RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%f_rain_phy) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
DO k = ckds,(ckde-1)
xv(k)= grid%f_rain_phy(pig,k,pjg)
ENDDO
CALL rsl_lite_to_child_msg((((ckde-1))-(ckds)+1)*RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%f_rimef_phy) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
DO k = ckds,(ckde-1)
xv(k)= grid%f_rimef_phy(pig,k,pjg)
ENDDO
CALL rsl_lite_to_child_msg((((ckde-1))-(ckds)+1)*RWORDSIZE,xv)
ENDIF
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
IF ( SIZE(grid%om_depth) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
DO k = 1,config_flags%ocean_levels
xv(k)= grid%om_depth(pig,k,pjg)
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
IF ( SIZE(grid%om_lat) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
xv(1)=grid%om_lat(pig,pjg)
CALL rsl_lite_to_child_msg(RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%om_lon) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
xv(1)=grid%om_lon(pig,pjg)
CALL rsl_lite_to_child_msg(RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%om_ml) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
xv(1)=grid%om_ml(pig,pjg)
CALL rsl_lite_to_child_msg(RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%om_tini) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
DO k = 1,config_flags%ocean_levels
xv(k)= grid%om_tini(pig,k,pjg)
ENDDO
CALL rsl_lite_to_child_msg(((config_flags%ocean_levels)-(1)+1)*RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%om_sini) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
DO k = 1,config_flags%ocean_levels
xv(k)= grid%om_sini(pig,k,pjg)
ENDDO
CALL rsl_lite_to_child_msg(((config_flags%ocean_levels)-(1)+1)*RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%h_diabatic) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
DO k = ckds,(ckde-1)
xv(k)= grid%h_diabatic(pig,k,pjg)
ENDDO
CALL rsl_lite_to_child_msg((((ckde-1))-(ckds)+1)*RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%msft) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
xv(1)=grid%msft(pig,pjg)
CALL rsl_lite_to_child_msg(RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%msfu) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
xv(1)=grid%msfu(pig,pjg)
CALL rsl_lite_to_child_msg(RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%msfv) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
xv(1)=grid%msfv(pig,pjg)
CALL rsl_lite_to_child_msg(RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%msftx) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
xv(1)=grid%msftx(pig,pjg)
CALL rsl_lite_to_child_msg(RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%msfty) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
xv(1)=grid%msfty(pig,pjg)
CALL rsl_lite_to_child_msg(RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%msfux) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
xv(1)=grid%msfux(pig,pjg)
CALL rsl_lite_to_child_msg(RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%msfuy) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
xv(1)=grid%msfuy(pig,pjg)
CALL rsl_lite_to_child_msg(RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%msfvx) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
xv(1)=grid%msfvx(pig,pjg)
CALL rsl_lite_to_child_msg(RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%msfvx_inv) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
xv(1)=grid%msfvx_inv(pig,pjg)
CALL rsl_lite_to_child_msg(RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%msfvy) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
xv(1)=grid%msfvy(pig,pjg)
CALL rsl_lite_to_child_msg(RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%f) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
xv(1)=grid%f(pig,pjg)
CALL rsl_lite_to_child_msg(RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%e) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
xv(1)=grid%e(pig,pjg)
CALL rsl_lite_to_child_msg(RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%sina) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
xv(1)=grid%sina(pig,pjg)
CALL rsl_lite_to_child_msg(RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%cosa) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
xv(1)=grid%cosa(pig,pjg)
CALL rsl_lite_to_child_msg(RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%ht) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
xv(1)=grid%ht(pig,pjg)
CALL rsl_lite_to_child_msg(RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%ht_shad) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
xv(1)=grid%ht_shad(pig,pjg)
CALL rsl_lite_to_child_msg(RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%tsk) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
xv(1)=grid%tsk(pig,pjg)
CALL rsl_lite_to_child_msg(RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%rainc) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
xv(1)=grid%rainc(pig,pjg)
CALL rsl_lite_to_child_msg(RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%rainsh) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
xv(1)=grid%rainsh(pig,pjg)
CALL rsl_lite_to_child_msg(RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%rainnc) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
xv(1)=grid%rainnc(pig,pjg)
CALL rsl_lite_to_child_msg(RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%i_rainc) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
xv(1)=grid%i_rainc(pig,pjg)
CALL rsl_lite_to_child_msg(RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%i_rainnc) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
xv(1)=grid%i_rainnc(pig,pjg)
CALL rsl_lite_to_child_msg(RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%snownc) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
xv(1)=grid%snownc(pig,pjg)
CALL rsl_lite_to_child_msg(RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%graupelnc) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
xv(1)=grid%graupelnc(pig,pjg)
CALL rsl_lite_to_child_msg(RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%hailnc) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
xv(1)=grid%hailnc(pig,pjg)
CALL rsl_lite_to_child_msg(RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%refl_10cm) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
DO k = ckds,(ckde-1)
xv(k)= grid%refl_10cm(pig,k,pjg)
ENDDO
CALL rsl_lite_to_child_msg((((ckde-1))-(ckds)+1)*RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%rthraten) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
DO k = ckds,(ckde-1)
xv(k)= grid%rthraten(pig,k,pjg)
ENDDO
CALL rsl_lite_to_child_msg((((ckde-1))-(ckds)+1)*RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%swdown) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
xv(1)=grid%swdown(pig,pjg)
CALL rsl_lite_to_child_msg(RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%gsw) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
xv(1)=grid%gsw(pig,pjg)
CALL rsl_lite_to_child_msg(RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%glw) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
xv(1)=grid%glw(pig,pjg)
CALL rsl_lite_to_child_msg(RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%swnorm) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
xv(1)=grid%swnorm(pig,pjg)
CALL rsl_lite_to_child_msg(RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%swddir) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
xv(1)=grid%swddir(pig,pjg)
CALL rsl_lite_to_child_msg(RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%swddni) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
xv(1)=grid%swddni(pig,pjg)
CALL rsl_lite_to_child_msg(RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%swddif) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
xv(1)=grid%swddif(pig,pjg)
CALL rsl_lite_to_child_msg(RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%gx) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
xv(1)=grid%gx(pig,pjg)
CALL rsl_lite_to_child_msg(RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%bx) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
xv(1)=grid%bx(pig,pjg)
CALL rsl_lite_to_child_msg(RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%gg) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
xv(1)=grid%gg(pig,pjg)
CALL rsl_lite_to_child_msg(RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%bb) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
xv(1)=grid%bb(pig,pjg)
CALL rsl_lite_to_child_msg(RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%acswupt) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
xv(1)=grid%acswupt(pig,pjg)
CALL rsl_lite_to_child_msg(RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%acswuptc) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
xv(1)=grid%acswuptc(pig,pjg)
CALL rsl_lite_to_child_msg(RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%acswdnt) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
xv(1)=grid%acswdnt(pig,pjg)
CALL rsl_lite_to_child_msg(RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%acswdntc) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
xv(1)=grid%acswdntc(pig,pjg)
CALL rsl_lite_to_child_msg(RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%acswupb) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
xv(1)=grid%acswupb(pig,pjg)
CALL rsl_lite_to_child_msg(RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%acswupbc) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
xv(1)=grid%acswupbc(pig,pjg)
CALL rsl_lite_to_child_msg(RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%acswdnb) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
xv(1)=grid%acswdnb(pig,pjg)
CALL rsl_lite_to_child_msg(RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%acswdnbc) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
xv(1)=grid%acswdnbc(pig,pjg)
CALL rsl_lite_to_child_msg(RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%aclwupt) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
xv(1)=grid%aclwupt(pig,pjg)
CALL rsl_lite_to_child_msg(RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%aclwuptc) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
xv(1)=grid%aclwuptc(pig,pjg)
CALL rsl_lite_to_child_msg(RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%aclwdnt) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
xv(1)=grid%aclwdnt(pig,pjg)
CALL rsl_lite_to_child_msg(RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%aclwdntc) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
xv(1)=grid%aclwdntc(pig,pjg)
CALL rsl_lite_to_child_msg(RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%aclwupb) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
xv(1)=grid%aclwupb(pig,pjg)
CALL rsl_lite_to_child_msg(RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%aclwupbc) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
xv(1)=grid%aclwupbc(pig,pjg)
CALL rsl_lite_to_child_msg(RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%aclwdnb) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
xv(1)=grid%aclwdnb(pig,pjg)
CALL rsl_lite_to_child_msg(RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%aclwdnbc) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
xv(1)=grid%aclwdnbc(pig,pjg)
CALL rsl_lite_to_child_msg(RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%i_acswupt) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
xv(1)=grid%i_acswupt(pig,pjg)
CALL rsl_lite_to_child_msg(RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%i_acswuptc) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
xv(1)=grid%i_acswuptc(pig,pjg)
CALL rsl_lite_to_child_msg(RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%i_acswdnt) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
xv(1)=grid%i_acswdnt(pig,pjg)
CALL rsl_lite_to_child_msg(RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%i_acswdntc) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
xv(1)=grid%i_acswdntc(pig,pjg)
CALL rsl_lite_to_child_msg(RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%i_acswupb) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
xv(1)=grid%i_acswupb(pig,pjg)
CALL rsl_lite_to_child_msg(RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%i_acswupbc) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
xv(1)=grid%i_acswupbc(pig,pjg)
CALL rsl_lite_to_child_msg(RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%i_acswdnb) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
xv(1)=grid%i_acswdnb(pig,pjg)
CALL rsl_lite_to_child_msg(RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%i_acswdnbc) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
xv(1)=grid%i_acswdnbc(pig,pjg)
CALL rsl_lite_to_child_msg(RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%i_aclwupt) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
xv(1)=grid%i_aclwupt(pig,pjg)
CALL rsl_lite_to_child_msg(RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%i_aclwuptc) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
xv(1)=grid%i_aclwuptc(pig,pjg)
CALL rsl_lite_to_child_msg(RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%i_aclwdnt) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
xv(1)=grid%i_aclwdnt(pig,pjg)
CALL rsl_lite_to_child_msg(RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%i_aclwdntc) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
xv(1)=grid%i_aclwdntc(pig,pjg)
CALL rsl_lite_to_child_msg(RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%i_aclwupb) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
xv(1)=grid%i_aclwupb(pig,pjg)
CALL rsl_lite_to_child_msg(RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%i_aclwupbc) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
xv(1)=grid%i_aclwupbc(pig,pjg)
CALL rsl_lite_to_child_msg(RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%i_aclwdnb) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
xv(1)=grid%i_aclwdnb(pig,pjg)
CALL rsl_lite_to_child_msg(RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%i_aclwdnbc) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
xv(1)=grid%i_aclwdnbc(pig,pjg)
CALL rsl_lite_to_child_msg(RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%swupt) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
xv(1)=grid%swupt(pig,pjg)
CALL rsl_lite_to_child_msg(RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%swuptc) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
xv(1)=grid%swuptc(pig,pjg)
CALL rsl_lite_to_child_msg(RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%swdnt) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
xv(1)=grid%swdnt(pig,pjg)
CALL rsl_lite_to_child_msg(RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%swdntc) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
xv(1)=grid%swdntc(pig,pjg)
CALL rsl_lite_to_child_msg(RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%swupb) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
xv(1)=grid%swupb(pig,pjg)
CALL rsl_lite_to_child_msg(RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%swupbc) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
xv(1)=grid%swupbc(pig,pjg)
CALL rsl_lite_to_child_msg(RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%swdnb) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
xv(1)=grid%swdnb(pig,pjg)
CALL rsl_lite_to_child_msg(RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%swdnbc) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
xv(1)=grid%swdnbc(pig,pjg)
CALL rsl_lite_to_child_msg(RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%lwupt) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
xv(1)=grid%lwupt(pig,pjg)
CALL rsl_lite_to_child_msg(RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%lwuptc) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
xv(1)=grid%lwuptc(pig,pjg)
CALL rsl_lite_to_child_msg(RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%lwdnt) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
xv(1)=grid%lwdnt(pig,pjg)
CALL rsl_lite_to_child_msg(RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%lwdntc) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
xv(1)=grid%lwdntc(pig,pjg)
CALL rsl_lite_to_child_msg(RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%lwupb) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
xv(1)=grid%lwupb(pig,pjg)
CALL rsl_lite_to_child_msg(RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%lwupbc) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
xv(1)=grid%lwupbc(pig,pjg)
CALL rsl_lite_to_child_msg(RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%lwdnb) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
xv(1)=grid%lwdnb(pig,pjg)
CALL rsl_lite_to_child_msg(RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%lwdnbc) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
xv(1)=grid%lwdnbc(pig,pjg)
CALL rsl_lite_to_child_msg(RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%xlat_u) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
xv(1)=grid%xlat_u(pig,pjg)
CALL rsl_lite_to_child_msg(RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%xlong_u) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
xv(1)=grid%xlong_u(pig,pjg)
CALL rsl_lite_to_child_msg(RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%xlat_v) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
xv(1)=grid%xlat_v(pig,pjg)
CALL rsl_lite_to_child_msg(RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%xlong_v) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
xv(1)=grid%xlong_v(pig,pjg)
CALL rsl_lite_to_child_msg(RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%clat) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
xv(1)=grid%clat(pig,pjg)
CALL rsl_lite_to_child_msg(RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%isnowxy) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
xv(1)=grid%isnowxy(pig,pjg)
CALL rsl_lite_to_child_msg(RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%tvxy) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
xv(1)=grid%tvxy(pig,pjg)
CALL rsl_lite_to_child_msg(RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%tgxy) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
xv(1)=grid%tgxy(pig,pjg)
CALL rsl_lite_to_child_msg(RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%canicexy) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
xv(1)=grid%canicexy(pig,pjg)
CALL rsl_lite_to_child_msg(RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%canliqxy) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
xv(1)=grid%canliqxy(pig,pjg)
CALL rsl_lite_to_child_msg(RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%eahxy) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
xv(1)=grid%eahxy(pig,pjg)
CALL rsl_lite_to_child_msg(RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%tahxy) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
xv(1)=grid%tahxy(pig,pjg)
CALL rsl_lite_to_child_msg(RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%cmxy) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
xv(1)=grid%cmxy(pig,pjg)
CALL rsl_lite_to_child_msg(RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%chxy) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
xv(1)=grid%chxy(pig,pjg)
CALL rsl_lite_to_child_msg(RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%fwetxy) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
xv(1)=grid%fwetxy(pig,pjg)
CALL rsl_lite_to_child_msg(RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%sneqvoxy) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
xv(1)=grid%sneqvoxy(pig,pjg)
CALL rsl_lite_to_child_msg(RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%alboldxy) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
xv(1)=grid%alboldxy(pig,pjg)
CALL rsl_lite_to_child_msg(RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%qsnowxy) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
xv(1)=grid%qsnowxy(pig,pjg)
CALL rsl_lite_to_child_msg(RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%wslakexy) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
xv(1)=grid%wslakexy(pig,pjg)
CALL rsl_lite_to_child_msg(RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%zwtxy) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
xv(1)=grid%zwtxy(pig,pjg)
CALL rsl_lite_to_child_msg(RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%waxy) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
xv(1)=grid%waxy(pig,pjg)
CALL rsl_lite_to_child_msg(RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%wtxy) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
xv(1)=grid%wtxy(pig,pjg)
CALL rsl_lite_to_child_msg(RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%tsnoxy) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
DO k = 1,config_flags%num_snow_layers
xv(k)= grid%tsnoxy(pig,k,pjg)
ENDDO
CALL rsl_lite_to_child_msg(((config_flags%num_snow_layers)-(1)+1)*RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%zsnsoxy) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
DO k = 1,config_flags%num_snso_layers
xv(k)= grid%zsnsoxy(pig,k,pjg)
ENDDO
CALL rsl_lite_to_child_msg(((config_flags%num_snso_layers)-(1)+1)*RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%snicexy) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
DO k = 1,config_flags%num_snow_layers
xv(k)= grid%snicexy(pig,k,pjg)
ENDDO
CALL rsl_lite_to_child_msg(((config_flags%num_snow_layers)-(1)+1)*RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%snliqxy) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
DO k = 1,config_flags%num_snow_layers
xv(k)= grid%snliqxy(pig,k,pjg)
ENDDO
CALL rsl_lite_to_child_msg(((config_flags%num_snow_layers)-(1)+1)*RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%lfmassxy) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
xv(1)=grid%lfmassxy(pig,pjg)
CALL rsl_lite_to_child_msg(RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%rtmassxy) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
xv(1)=grid%rtmassxy(pig,pjg)
CALL rsl_lite_to_child_msg(RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%stmassxy) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
xv(1)=grid%stmassxy(pig,pjg)
CALL rsl_lite_to_child_msg(RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%woodxy) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
xv(1)=grid%woodxy(pig,pjg)
CALL rsl_lite_to_child_msg(RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%stblcpxy) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
xv(1)=grid%stblcpxy(pig,pjg)
CALL rsl_lite_to_child_msg(RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%fastcpxy) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
xv(1)=grid%fastcpxy(pig,pjg)
CALL rsl_lite_to_child_msg(RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%xsaixy) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
xv(1)=grid%xsaixy(pig,pjg)
CALL rsl_lite_to_child_msg(RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%t2mvxy) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
xv(1)=grid%t2mvxy(pig,pjg)
CALL rsl_lite_to_child_msg(RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%t2mbxy) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
xv(1)=grid%t2mbxy(pig,pjg)
CALL rsl_lite_to_child_msg(RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%q2mvxy) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
xv(1)=grid%q2mvxy(pig,pjg)
CALL rsl_lite_to_child_msg(RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%q2mbxy) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
xv(1)=grid%q2mbxy(pig,pjg)
CALL rsl_lite_to_child_msg(RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%tradxy) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
xv(1)=grid%tradxy(pig,pjg)
CALL rsl_lite_to_child_msg(RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%neexy) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
xv(1)=grid%neexy(pig,pjg)
CALL rsl_lite_to_child_msg(RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%gppxy) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
xv(1)=grid%gppxy(pig,pjg)
CALL rsl_lite_to_child_msg(RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%nppxy) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
xv(1)=grid%nppxy(pig,pjg)
CALL rsl_lite_to_child_msg(RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%fvegxy) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
xv(1)=grid%fvegxy(pig,pjg)
CALL rsl_lite_to_child_msg(RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%qinxy) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
xv(1)=grid%qinxy(pig,pjg)
CALL rsl_lite_to_child_msg(RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%runsfxy) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
xv(1)=grid%runsfxy(pig,pjg)
CALL rsl_lite_to_child_msg(RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%runsbxy) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
xv(1)=grid%runsbxy(pig,pjg)
CALL rsl_lite_to_child_msg(RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%ecanxy) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
xv(1)=grid%ecanxy(pig,pjg)
CALL rsl_lite_to_child_msg(RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%edirxy) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
xv(1)=grid%edirxy(pig,pjg)
CALL rsl_lite_to_child_msg(RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%etranxy) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
xv(1)=grid%etranxy(pig,pjg)
CALL rsl_lite_to_child_msg(RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%fsaxy) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
xv(1)=grid%fsaxy(pig,pjg)
CALL rsl_lite_to_child_msg(RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%firaxy) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
xv(1)=grid%firaxy(pig,pjg)
CALL rsl_lite_to_child_msg(RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%aparxy) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
xv(1)=grid%aparxy(pig,pjg)
CALL rsl_lite_to_child_msg(RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%psnxy) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
xv(1)=grid%psnxy(pig,pjg)
CALL rsl_lite_to_child_msg(RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%savxy) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
xv(1)=grid%savxy(pig,pjg)
CALL rsl_lite_to_child_msg(RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%sagxy) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
xv(1)=grid%sagxy(pig,pjg)
CALL rsl_lite_to_child_msg(RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%rssunxy) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
xv(1)=grid%rssunxy(pig,pjg)
CALL rsl_lite_to_child_msg(RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%rsshaxy) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
xv(1)=grid%rsshaxy(pig,pjg)
CALL rsl_lite_to_child_msg(RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%bgapxy) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
xv(1)=grid%bgapxy(pig,pjg)
CALL rsl_lite_to_child_msg(RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%wgapxy) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
xv(1)=grid%wgapxy(pig,pjg)
CALL rsl_lite_to_child_msg(RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%tgvxy) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
xv(1)=grid%tgvxy(pig,pjg)
CALL rsl_lite_to_child_msg(RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%tgbxy) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
xv(1)=grid%tgbxy(pig,pjg)
CALL rsl_lite_to_child_msg(RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%chvxy) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
xv(1)=grid%chvxy(pig,pjg)
CALL rsl_lite_to_child_msg(RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%chbxy) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
xv(1)=grid%chbxy(pig,pjg)
CALL rsl_lite_to_child_msg(RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%shgxy) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
xv(1)=grid%shgxy(pig,pjg)
CALL rsl_lite_to_child_msg(RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%shcxy) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
xv(1)=grid%shcxy(pig,pjg)
CALL rsl_lite_to_child_msg(RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%shbxy) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
xv(1)=grid%shbxy(pig,pjg)
CALL rsl_lite_to_child_msg(RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%evgxy) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
xv(1)=grid%evgxy(pig,pjg)
CALL rsl_lite_to_child_msg(RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%evbxy) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
xv(1)=grid%evbxy(pig,pjg)
CALL rsl_lite_to_child_msg(RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%ghvxy) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
xv(1)=grid%ghvxy(pig,pjg)
CALL rsl_lite_to_child_msg(RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%ghbxy) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
xv(1)=grid%ghbxy(pig,pjg)
CALL rsl_lite_to_child_msg(RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%irgxy) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
xv(1)=grid%irgxy(pig,pjg)
CALL rsl_lite_to_child_msg(RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%ircxy) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
xv(1)=grid%ircxy(pig,pjg)
CALL rsl_lite_to_child_msg(RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%irbxy) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
xv(1)=grid%irbxy(pig,pjg)
CALL rsl_lite_to_child_msg(RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%trxy) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
xv(1)=grid%trxy(pig,pjg)
CALL rsl_lite_to_child_msg(RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%evcxy) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
xv(1)=grid%evcxy(pig,pjg)
CALL rsl_lite_to_child_msg(RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%chleafxy) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
xv(1)=grid%chleafxy(pig,pjg)
CALL rsl_lite_to_child_msg(RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%chucxy) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
xv(1)=grid%chucxy(pig,pjg)
CALL rsl_lite_to_child_msg(RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%chv2xy) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
xv(1)=grid%chv2xy(pig,pjg)
CALL rsl_lite_to_child_msg(RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%chb2xy) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
xv(1)=grid%chb2xy(pig,pjg)
CALL rsl_lite_to_child_msg(RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%chstarxy) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
xv(1)=grid%chstarxy(pig,pjg)
CALL rsl_lite_to_child_msg(RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%tsk_mosaic) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
DO k = 1,config_flags%mosaic_cat
xv(k)= grid%tsk_mosaic(pig,k,pjg)
ENDDO
CALL rsl_lite_to_child_msg(((config_flags%mosaic_cat)-(1)+1)*RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%qsfc_mosaic) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
DO k = 1,config_flags%mosaic_cat
xv(k)= grid%qsfc_mosaic(pig,k,pjg)
ENDDO
CALL rsl_lite_to_child_msg(((config_flags%mosaic_cat)-(1)+1)*RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%tslb_mosaic) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
DO k = 1,config_flags%mosaic_cat_soil
xv(k)= grid%tslb_mosaic(pig,k,pjg)
ENDDO
CALL rsl_lite_to_child_msg(((config_flags%mosaic_cat_soil)-(1)+1)*RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%smois_mosaic) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
DO k = 1,config_flags%mosaic_cat_soil
xv(k)= grid%smois_mosaic(pig,k,pjg)
ENDDO
CALL rsl_lite_to_child_msg(((config_flags%mosaic_cat_soil)-(1)+1)*RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%sh2o_mosaic) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
DO k = 1,config_flags%mosaic_cat_soil
xv(k)= grid%sh2o_mosaic(pig,k,pjg)
ENDDO
CALL rsl_lite_to_child_msg(((config_flags%mosaic_cat_soil)-(1)+1)*RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%canwat_mosaic) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
DO k = 1,config_flags%mosaic_cat
xv(k)= grid%canwat_mosaic(pig,k,pjg)
ENDDO
CALL rsl_lite_to_child_msg(((config_flags%mosaic_cat)-(1)+1)*RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%snow_mosaic) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
DO k = 1,config_flags%mosaic_cat
xv(k)= grid%snow_mosaic(pig,k,pjg)
ENDDO
CALL rsl_lite_to_child_msg(((config_flags%mosaic_cat)-(1)+1)*RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%snowh_mosaic) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
DO k = 1,config_flags%mosaic_cat
xv(k)= grid%snowh_mosaic(pig,k,pjg)
ENDDO
CALL rsl_lite_to_child_msg(((config_flags%mosaic_cat)-(1)+1)*RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%snowc_mosaic) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
DO k = 1,config_flags%mosaic_cat
xv(k)= grid%snowc_mosaic(pig,k,pjg)
ENDDO
CALL rsl_lite_to_child_msg(((config_flags%mosaic_cat)-(1)+1)*RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%tr_urb2d_mosaic) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
DO k = 1,config_flags%mosaic_cat
xv(k)= grid%tr_urb2d_mosaic(pig,k,pjg)
ENDDO
CALL rsl_lite_to_child_msg(((config_flags%mosaic_cat)-(1)+1)*RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%tb_urb2d_mosaic) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
DO k = 1,config_flags%mosaic_cat
xv(k)= grid%tb_urb2d_mosaic(pig,k,pjg)
ENDDO
CALL rsl_lite_to_child_msg(((config_flags%mosaic_cat)-(1)+1)*RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%tg_urb2d_mosaic) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
DO k = 1,config_flags%mosaic_cat
xv(k)= grid%tg_urb2d_mosaic(pig,k,pjg)
ENDDO
CALL rsl_lite_to_child_msg(((config_flags%mosaic_cat)-(1)+1)*RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%tc_urb2d_mosaic) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
DO k = 1,config_flags%mosaic_cat
xv(k)= grid%tc_urb2d_mosaic(pig,k,pjg)
ENDDO
CALL rsl_lite_to_child_msg(((config_flags%mosaic_cat)-(1)+1)*RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%ts_urb2d_mosaic) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
DO k = 1,config_flags%mosaic_cat
xv(k)= grid%ts_urb2d_mosaic(pig,k,pjg)
ENDDO
CALL rsl_lite_to_child_msg(((config_flags%mosaic_cat)-(1)+1)*RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%ts_rul2d_mosaic) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
DO k = 1,config_flags%mosaic_cat
xv(k)= grid%ts_rul2d_mosaic(pig,k,pjg)
ENDDO
CALL rsl_lite_to_child_msg(((config_flags%mosaic_cat)-(1)+1)*RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%qc_urb2d_mosaic) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
DO k = 1,config_flags%mosaic_cat
xv(k)= grid%qc_urb2d_mosaic(pig,k,pjg)
ENDDO
CALL rsl_lite_to_child_msg(((config_flags%mosaic_cat)-(1)+1)*RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%uc_urb2d_mosaic) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
DO k = 1,config_flags%mosaic_cat
xv(k)= grid%uc_urb2d_mosaic(pig,k,pjg)
ENDDO
CALL rsl_lite_to_child_msg(((config_flags%mosaic_cat)-(1)+1)*RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%trl_urb3d_mosaic) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
DO k = 1,config_flags%mosaic_cat_soil
xv(k)= grid%trl_urb3d_mosaic(pig,k,pjg)
ENDDO
CALL rsl_lite_to_child_msg(((config_flags%mosaic_cat_soil)-(1)+1)*RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%tbl_urb3d_mosaic) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
DO k = 1,config_flags%mosaic_cat_soil
xv(k)= grid%tbl_urb3d_mosaic(pig,k,pjg)
ENDDO
CALL rsl_lite_to_child_msg(((config_flags%mosaic_cat_soil)-(1)+1)*RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%tgl_urb3d_mosaic) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
DO k = 1,config_flags%mosaic_cat_soil
xv(k)= grid%tgl_urb3d_mosaic(pig,k,pjg)
ENDDO
CALL rsl_lite_to_child_msg(((config_flags%mosaic_cat_soil)-(1)+1)*RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%mosaic_cat_index) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
DO k = 1,config_flags%num_land_cat
xv(k)= grid%mosaic_cat_index(pig,k,pjg)
ENDDO
CALL rsl_lite_to_child_msg(((config_flags%num_land_cat)-(1)+1)*RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%landusef2) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
DO k = 1,config_flags%num_land_cat
xv(k)= grid%landusef2(pig,k,pjg)
ENDDO
CALL rsl_lite_to_child_msg(((config_flags%num_land_cat)-(1)+1)*RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%tmn) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
xv(1)=grid%tmn(pig,pjg)
CALL rsl_lite_to_child_msg(RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%tyr) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
xv(1)=grid%tyr(pig,pjg)
CALL rsl_lite_to_child_msg(RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%tyra) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
xv(1)=grid%tyra(pig,pjg)
CALL rsl_lite_to_child_msg(RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%tdly) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
xv(1)=grid%tdly(pig,pjg)
CALL rsl_lite_to_child_msg(RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%tlag) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
DO k = 1,config_flags%lagday
xv(k)= grid%tlag(pig,k,pjg)
ENDDO
CALL rsl_lite_to_child_msg(((config_flags%lagday)-(1)+1)*RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%xland) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
xv(1)=grid%xland(pig,pjg)
CALL rsl_lite_to_child_msg(RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%achfx) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
xv(1)=grid%achfx(pig,pjg)
CALL rsl_lite_to_child_msg(RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%aclhf) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
xv(1)=grid%aclhf(pig,pjg)
CALL rsl_lite_to_child_msg(RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%snowc) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
xv(1)=grid%snowc(pig,pjg)
CALL rsl_lite_to_child_msg(RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%tml) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
xv(1)=grid%tml(pig,pjg)
CALL rsl_lite_to_child_msg(RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%t0ml) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
xv(1)=grid%t0ml(pig,pjg)
CALL rsl_lite_to_child_msg(RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%hml) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
xv(1)=grid%hml(pig,pjg)
CALL rsl_lite_to_child_msg(RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%h0ml) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
xv(1)=grid%h0ml(pig,pjg)
CALL rsl_lite_to_child_msg(RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%huml) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
xv(1)=grid%huml(pig,pjg)
CALL rsl_lite_to_child_msg(RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%hvml) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
xv(1)=grid%hvml(pig,pjg)
CALL rsl_lite_to_child_msg(RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%tmoml) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
xv(1)=grid%tmoml(pig,pjg)
CALL rsl_lite_to_child_msg(RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%vertstrucc) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
DO k = ckds,(ckde-1)
xv(k)= grid%vertstrucc(pig,k,pjg)
ENDDO
CALL rsl_lite_to_child_msg((((ckde-1))-(ckds)+1)*RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%vertstrucs) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
DO k = ckds,(ckde-1)
xv(k)= grid%vertstrucs(pig,k,pjg)
ENDDO
CALL rsl_lite_to_child_msg((((ckde-1))-(ckds)+1)*RWORDSIZE,xv)
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
IF ( SIZE(grid%numc) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
xv(1)=grid%numc(pig,pjg)
CALL rsl_lite_to_child_msg(RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%nump) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
xv(1)=grid%nump(pig,pjg)
CALL rsl_lite_to_child_msg(RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%snl) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
DO k = 1,config_flags%maxpatch
xv(k)= grid%snl(pig,k,pjg)
ENDDO
CALL rsl_lite_to_child_msg(((config_flags%maxpatch)-(1)+1)*RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%snowdp) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
DO k = 1,config_flags%maxpatch
xv(k)= grid%snowdp(pig,k,pjg)
ENDDO
CALL rsl_lite_to_child_msg(((config_flags%maxpatch)-(1)+1)*RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%wtc) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
DO k = 1,config_flags%maxpatch
xv(k)= grid%wtc(pig,k,pjg)
ENDDO
CALL rsl_lite_to_child_msg(((config_flags%maxpatch)-(1)+1)*RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%wtp) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
DO k = 1,config_flags%maxpatch
xv(k)= grid%wtp(pig,k,pjg)
ENDDO
CALL rsl_lite_to_child_msg(((config_flags%maxpatch)-(1)+1)*RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%h2osno) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
DO k = 1,config_flags%maxpatch
xv(k)= grid%h2osno(pig,k,pjg)
ENDDO
CALL rsl_lite_to_child_msg(((config_flags%maxpatch)-(1)+1)*RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%t_grnd) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
DO k = 1,config_flags%maxpatch
xv(k)= grid%t_grnd(pig,k,pjg)
ENDDO
CALL rsl_lite_to_child_msg(((config_flags%maxpatch)-(1)+1)*RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%t_veg) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
DO k = 1,config_flags%maxpatch
xv(k)= grid%t_veg(pig,k,pjg)
ENDDO
CALL rsl_lite_to_child_msg(((config_flags%maxpatch)-(1)+1)*RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%h2ocan) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
DO k = 1,config_flags%maxpatch
xv(k)= grid%h2ocan(pig,k,pjg)
ENDDO
CALL rsl_lite_to_child_msg(((config_flags%maxpatch)-(1)+1)*RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%h2ocan_col) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
DO k = 1,config_flags%maxpatch
xv(k)= grid%h2ocan_col(pig,k,pjg)
ENDDO
CALL rsl_lite_to_child_msg(((config_flags%maxpatch)-(1)+1)*RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%t2m_max) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
xv(1)=grid%t2m_max(pig,pjg)
CALL rsl_lite_to_child_msg(RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%t2m_min) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
xv(1)=grid%t2m_min(pig,pjg)
CALL rsl_lite_to_child_msg(RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%t2clm) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
xv(1)=grid%t2clm(pig,pjg)
CALL rsl_lite_to_child_msg(RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%t_ref2m) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
DO k = 1,config_flags%maxpatch
xv(k)= grid%t_ref2m(pig,k,pjg)
ENDDO
CALL rsl_lite_to_child_msg(((config_flags%maxpatch)-(1)+1)*RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%h2osoi_liq_s1) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
DO k = 1,config_flags%maxpatch
xv(k)= grid%h2osoi_liq_s1(pig,k,pjg)
ENDDO
CALL rsl_lite_to_child_msg(((config_flags%maxpatch)-(1)+1)*RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%h2osoi_liq_s2) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
DO k = 1,config_flags%maxpatch
xv(k)= grid%h2osoi_liq_s2(pig,k,pjg)
ENDDO
CALL rsl_lite_to_child_msg(((config_flags%maxpatch)-(1)+1)*RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%h2osoi_liq_s3) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
DO k = 1,config_flags%maxpatch
xv(k)= grid%h2osoi_liq_s3(pig,k,pjg)
ENDDO
CALL rsl_lite_to_child_msg(((config_flags%maxpatch)-(1)+1)*RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%h2osoi_liq_s4) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
DO k = 1,config_flags%maxpatch
xv(k)= grid%h2osoi_liq_s4(pig,k,pjg)
ENDDO
CALL rsl_lite_to_child_msg(((config_flags%maxpatch)-(1)+1)*RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%h2osoi_liq_s5) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
DO k = 1,config_flags%maxpatch
xv(k)= grid%h2osoi_liq_s5(pig,k,pjg)
ENDDO
CALL rsl_lite_to_child_msg(((config_flags%maxpatch)-(1)+1)*RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%h2osoi_liq1) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
DO k = 1,config_flags%maxpatch
xv(k)= grid%h2osoi_liq1(pig,k,pjg)
ENDDO
CALL rsl_lite_to_child_msg(((config_flags%maxpatch)-(1)+1)*RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%h2osoi_liq2) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
DO k = 1,config_flags%maxpatch
xv(k)= grid%h2osoi_liq2(pig,k,pjg)
ENDDO
CALL rsl_lite_to_child_msg(((config_flags%maxpatch)-(1)+1)*RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%h2osoi_liq3) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
DO k = 1,config_flags%maxpatch
xv(k)= grid%h2osoi_liq3(pig,k,pjg)
ENDDO
CALL rsl_lite_to_child_msg(((config_flags%maxpatch)-(1)+1)*RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%h2osoi_liq4) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
DO k = 1,config_flags%maxpatch
xv(k)= grid%h2osoi_liq4(pig,k,pjg)
ENDDO
CALL rsl_lite_to_child_msg(((config_flags%maxpatch)-(1)+1)*RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%h2osoi_liq5) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
DO k = 1,config_flags%maxpatch
xv(k)= grid%h2osoi_liq5(pig,k,pjg)
ENDDO
CALL rsl_lite_to_child_msg(((config_flags%maxpatch)-(1)+1)*RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%h2osoi_liq6) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
DO k = 1,config_flags%maxpatch
xv(k)= grid%h2osoi_liq6(pig,k,pjg)
ENDDO
CALL rsl_lite_to_child_msg(((config_flags%maxpatch)-(1)+1)*RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%h2osoi_liq7) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
DO k = 1,config_flags%maxpatch
xv(k)= grid%h2osoi_liq7(pig,k,pjg)
ENDDO
CALL rsl_lite_to_child_msg(((config_flags%maxpatch)-(1)+1)*RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%h2osoi_liq8) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
DO k = 1,config_flags%maxpatch
xv(k)= grid%h2osoi_liq8(pig,k,pjg)
ENDDO
CALL rsl_lite_to_child_msg(((config_flags%maxpatch)-(1)+1)*RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%h2osoi_liq9) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
DO k = 1,config_flags%maxpatch
xv(k)= grid%h2osoi_liq9(pig,k,pjg)
ENDDO
CALL rsl_lite_to_child_msg(((config_flags%maxpatch)-(1)+1)*RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%h2osoi_liq10) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
DO k = 1,config_flags%maxpatch
xv(k)= grid%h2osoi_liq10(pig,k,pjg)
ENDDO
CALL rsl_lite_to_child_msg(((config_flags%maxpatch)-(1)+1)*RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%h2osoi_ice_s1) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
DO k = 1,config_flags%maxpatch
xv(k)= grid%h2osoi_ice_s1(pig,k,pjg)
ENDDO
CALL rsl_lite_to_child_msg(((config_flags%maxpatch)-(1)+1)*RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%h2osoi_ice_s2) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
DO k = 1,config_flags%maxpatch
xv(k)= grid%h2osoi_ice_s2(pig,k,pjg)
ENDDO
CALL rsl_lite_to_child_msg(((config_flags%maxpatch)-(1)+1)*RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%h2osoi_ice_s3) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
DO k = 1,config_flags%maxpatch
xv(k)= grid%h2osoi_ice_s3(pig,k,pjg)
ENDDO
CALL rsl_lite_to_child_msg(((config_flags%maxpatch)-(1)+1)*RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%h2osoi_ice_s4) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
DO k = 1,config_flags%maxpatch
xv(k)= grid%h2osoi_ice_s4(pig,k,pjg)
ENDDO
CALL rsl_lite_to_child_msg(((config_flags%maxpatch)-(1)+1)*RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%h2osoi_ice_s5) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
DO k = 1,config_flags%maxpatch
xv(k)= grid%h2osoi_ice_s5(pig,k,pjg)
ENDDO
CALL rsl_lite_to_child_msg(((config_flags%maxpatch)-(1)+1)*RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%h2osoi_ice1) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
DO k = 1,config_flags%maxpatch
xv(k)= grid%h2osoi_ice1(pig,k,pjg)
ENDDO
CALL rsl_lite_to_child_msg(((config_flags%maxpatch)-(1)+1)*RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%h2osoi_ice2) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
DO k = 1,config_flags%maxpatch
xv(k)= grid%h2osoi_ice2(pig,k,pjg)
ENDDO
CALL rsl_lite_to_child_msg(((config_flags%maxpatch)-(1)+1)*RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%h2osoi_ice3) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
DO k = 1,config_flags%maxpatch
xv(k)= grid%h2osoi_ice3(pig,k,pjg)
ENDDO
CALL rsl_lite_to_child_msg(((config_flags%maxpatch)-(1)+1)*RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%h2osoi_ice4) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
DO k = 1,config_flags%maxpatch
xv(k)= grid%h2osoi_ice4(pig,k,pjg)
ENDDO
CALL rsl_lite_to_child_msg(((config_flags%maxpatch)-(1)+1)*RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%h2osoi_ice5) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
DO k = 1,config_flags%maxpatch
xv(k)= grid%h2osoi_ice5(pig,k,pjg)
ENDDO
CALL rsl_lite_to_child_msg(((config_flags%maxpatch)-(1)+1)*RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%h2osoi_ice6) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
DO k = 1,config_flags%maxpatch
xv(k)= grid%h2osoi_ice6(pig,k,pjg)
ENDDO
CALL rsl_lite_to_child_msg(((config_flags%maxpatch)-(1)+1)*RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%h2osoi_ice7) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
DO k = 1,config_flags%maxpatch
xv(k)= grid%h2osoi_ice7(pig,k,pjg)
ENDDO
CALL rsl_lite_to_child_msg(((config_flags%maxpatch)-(1)+1)*RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%h2osoi_ice8) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
DO k = 1,config_flags%maxpatch
xv(k)= grid%h2osoi_ice8(pig,k,pjg)
ENDDO
CALL rsl_lite_to_child_msg(((config_flags%maxpatch)-(1)+1)*RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%h2osoi_ice9) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
DO k = 1,config_flags%maxpatch
xv(k)= grid%h2osoi_ice9(pig,k,pjg)
ENDDO
CALL rsl_lite_to_child_msg(((config_flags%maxpatch)-(1)+1)*RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%h2osoi_ice10) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
DO k = 1,config_flags%maxpatch
xv(k)= grid%h2osoi_ice10(pig,k,pjg)
ENDDO
CALL rsl_lite_to_child_msg(((config_flags%maxpatch)-(1)+1)*RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%t_soisno_s1) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
DO k = 1,config_flags%maxpatch
xv(k)= grid%t_soisno_s1(pig,k,pjg)
ENDDO
CALL rsl_lite_to_child_msg(((config_flags%maxpatch)-(1)+1)*RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%t_soisno_s2) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
DO k = 1,config_flags%maxpatch
xv(k)= grid%t_soisno_s2(pig,k,pjg)
ENDDO
CALL rsl_lite_to_child_msg(((config_flags%maxpatch)-(1)+1)*RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%t_soisno_s3) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
DO k = 1,config_flags%maxpatch
xv(k)= grid%t_soisno_s3(pig,k,pjg)
ENDDO
CALL rsl_lite_to_child_msg(((config_flags%maxpatch)-(1)+1)*RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%t_soisno_s4) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
DO k = 1,config_flags%maxpatch
xv(k)= grid%t_soisno_s4(pig,k,pjg)
ENDDO
CALL rsl_lite_to_child_msg(((config_flags%maxpatch)-(1)+1)*RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%t_soisno_s5) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
DO k = 1,config_flags%maxpatch
xv(k)= grid%t_soisno_s5(pig,k,pjg)
ENDDO
CALL rsl_lite_to_child_msg(((config_flags%maxpatch)-(1)+1)*RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%t_soisno1) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
DO k = 1,config_flags%maxpatch
xv(k)= grid%t_soisno1(pig,k,pjg)
ENDDO
CALL rsl_lite_to_child_msg(((config_flags%maxpatch)-(1)+1)*RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%t_soisno2) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
DO k = 1,config_flags%maxpatch
xv(k)= grid%t_soisno2(pig,k,pjg)
ENDDO
CALL rsl_lite_to_child_msg(((config_flags%maxpatch)-(1)+1)*RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%t_soisno3) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
DO k = 1,config_flags%maxpatch
xv(k)= grid%t_soisno3(pig,k,pjg)
ENDDO
CALL rsl_lite_to_child_msg(((config_flags%maxpatch)-(1)+1)*RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%t_soisno4) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
DO k = 1,config_flags%maxpatch
xv(k)= grid%t_soisno4(pig,k,pjg)
ENDDO
CALL rsl_lite_to_child_msg(((config_flags%maxpatch)-(1)+1)*RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%t_soisno5) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
DO k = 1,config_flags%maxpatch
xv(k)= grid%t_soisno5(pig,k,pjg)
ENDDO
CALL rsl_lite_to_child_msg(((config_flags%maxpatch)-(1)+1)*RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%t_soisno6) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
DO k = 1,config_flags%maxpatch
xv(k)= grid%t_soisno6(pig,k,pjg)
ENDDO
CALL rsl_lite_to_child_msg(((config_flags%maxpatch)-(1)+1)*RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%t_soisno7) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
DO k = 1,config_flags%maxpatch
xv(k)= grid%t_soisno7(pig,k,pjg)
ENDDO
CALL rsl_lite_to_child_msg(((config_flags%maxpatch)-(1)+1)*RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%t_soisno8) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
DO k = 1,config_flags%maxpatch
xv(k)= grid%t_soisno8(pig,k,pjg)
ENDDO
CALL rsl_lite_to_child_msg(((config_flags%maxpatch)-(1)+1)*RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%t_soisno9) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
DO k = 1,config_flags%maxpatch
xv(k)= grid%t_soisno9(pig,k,pjg)
ENDDO
CALL rsl_lite_to_child_msg(((config_flags%maxpatch)-(1)+1)*RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%t_soisno10) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
DO k = 1,config_flags%maxpatch
xv(k)= grid%t_soisno10(pig,k,pjg)
ENDDO
CALL rsl_lite_to_child_msg(((config_flags%maxpatch)-(1)+1)*RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%dzsnow1) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
DO k = 1,config_flags%maxpatch
xv(k)= grid%dzsnow1(pig,k,pjg)
ENDDO
CALL rsl_lite_to_child_msg(((config_flags%maxpatch)-(1)+1)*RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%dzsnow2) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
DO k = 1,config_flags%maxpatch
xv(k)= grid%dzsnow2(pig,k,pjg)
ENDDO
CALL rsl_lite_to_child_msg(((config_flags%maxpatch)-(1)+1)*RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%dzsnow3) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
DO k = 1,config_flags%maxpatch
xv(k)= grid%dzsnow3(pig,k,pjg)
ENDDO
CALL rsl_lite_to_child_msg(((config_flags%maxpatch)-(1)+1)*RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%dzsnow4) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
DO k = 1,config_flags%maxpatch
xv(k)= grid%dzsnow4(pig,k,pjg)
ENDDO
CALL rsl_lite_to_child_msg(((config_flags%maxpatch)-(1)+1)*RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%dzsnow5) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
DO k = 1,config_flags%maxpatch
xv(k)= grid%dzsnow5(pig,k,pjg)
ENDDO
CALL rsl_lite_to_child_msg(((config_flags%maxpatch)-(1)+1)*RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%snowrds1) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
DO k = 1,config_flags%maxpatch
xv(k)= grid%snowrds1(pig,k,pjg)
ENDDO
CALL rsl_lite_to_child_msg(((config_flags%maxpatch)-(1)+1)*RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%snowrds2) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
DO k = 1,config_flags%maxpatch
xv(k)= grid%snowrds2(pig,k,pjg)
ENDDO
CALL rsl_lite_to_child_msg(((config_flags%maxpatch)-(1)+1)*RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%snowrds3) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
DO k = 1,config_flags%maxpatch
xv(k)= grid%snowrds3(pig,k,pjg)
ENDDO
CALL rsl_lite_to_child_msg(((config_flags%maxpatch)-(1)+1)*RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%snowrds4) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
DO k = 1,config_flags%maxpatch
xv(k)= grid%snowrds4(pig,k,pjg)
ENDDO
CALL rsl_lite_to_child_msg(((config_flags%maxpatch)-(1)+1)*RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%snowrds5) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
DO k = 1,config_flags%maxpatch
xv(k)= grid%snowrds5(pig,k,pjg)
ENDDO
CALL rsl_lite_to_child_msg(((config_flags%maxpatch)-(1)+1)*RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%t_lake1) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
DO k = 1,config_flags%maxpatch
xv(k)= grid%t_lake1(pig,k,pjg)
ENDDO
CALL rsl_lite_to_child_msg(((config_flags%maxpatch)-(1)+1)*RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%t_lake2) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
DO k = 1,config_flags%maxpatch
xv(k)= grid%t_lake2(pig,k,pjg)
ENDDO
CALL rsl_lite_to_child_msg(((config_flags%maxpatch)-(1)+1)*RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%t_lake3) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
DO k = 1,config_flags%maxpatch
xv(k)= grid%t_lake3(pig,k,pjg)
ENDDO
CALL rsl_lite_to_child_msg(((config_flags%maxpatch)-(1)+1)*RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%t_lake4) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
DO k = 1,config_flags%maxpatch
xv(k)= grid%t_lake4(pig,k,pjg)
ENDDO
CALL rsl_lite_to_child_msg(((config_flags%maxpatch)-(1)+1)*RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%t_lake5) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
DO k = 1,config_flags%maxpatch
xv(k)= grid%t_lake5(pig,k,pjg)
ENDDO
CALL rsl_lite_to_child_msg(((config_flags%maxpatch)-(1)+1)*RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%t_lake6) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
DO k = 1,config_flags%maxpatch
xv(k)= grid%t_lake6(pig,k,pjg)
ENDDO
CALL rsl_lite_to_child_msg(((config_flags%maxpatch)-(1)+1)*RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%t_lake7) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
DO k = 1,config_flags%maxpatch
xv(k)= grid%t_lake7(pig,k,pjg)
ENDDO
CALL rsl_lite_to_child_msg(((config_flags%maxpatch)-(1)+1)*RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%t_lake8) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
DO k = 1,config_flags%maxpatch
xv(k)= grid%t_lake8(pig,k,pjg)
ENDDO
CALL rsl_lite_to_child_msg(((config_flags%maxpatch)-(1)+1)*RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%t_lake9) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
DO k = 1,config_flags%maxpatch
xv(k)= grid%t_lake9(pig,k,pjg)
ENDDO
CALL rsl_lite_to_child_msg(((config_flags%maxpatch)-(1)+1)*RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%t_lake10) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
DO k = 1,config_flags%maxpatch
xv(k)= grid%t_lake10(pig,k,pjg)
ENDDO
CALL rsl_lite_to_child_msg(((config_flags%maxpatch)-(1)+1)*RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%h2osoi_vol1) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
DO k = 1,config_flags%maxpatch
xv(k)= grid%h2osoi_vol1(pig,k,pjg)
ENDDO
CALL rsl_lite_to_child_msg(((config_flags%maxpatch)-(1)+1)*RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%h2osoi_vol2) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
DO k = 1,config_flags%maxpatch
xv(k)= grid%h2osoi_vol2(pig,k,pjg)
ENDDO
CALL rsl_lite_to_child_msg(((config_flags%maxpatch)-(1)+1)*RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%h2osoi_vol3) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
DO k = 1,config_flags%maxpatch
xv(k)= grid%h2osoi_vol3(pig,k,pjg)
ENDDO
CALL rsl_lite_to_child_msg(((config_flags%maxpatch)-(1)+1)*RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%h2osoi_vol4) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
DO k = 1,config_flags%maxpatch
xv(k)= grid%h2osoi_vol4(pig,k,pjg)
ENDDO
CALL rsl_lite_to_child_msg(((config_flags%maxpatch)-(1)+1)*RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%h2osoi_vol5) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
DO k = 1,config_flags%maxpatch
xv(k)= grid%h2osoi_vol5(pig,k,pjg)
ENDDO
CALL rsl_lite_to_child_msg(((config_flags%maxpatch)-(1)+1)*RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%h2osoi_vol6) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
DO k = 1,config_flags%maxpatch
xv(k)= grid%h2osoi_vol6(pig,k,pjg)
ENDDO
CALL rsl_lite_to_child_msg(((config_flags%maxpatch)-(1)+1)*RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%h2osoi_vol7) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
DO k = 1,config_flags%maxpatch
xv(k)= grid%h2osoi_vol7(pig,k,pjg)
ENDDO
CALL rsl_lite_to_child_msg(((config_flags%maxpatch)-(1)+1)*RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%h2osoi_vol8) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
DO k = 1,config_flags%maxpatch
xv(k)= grid%h2osoi_vol8(pig,k,pjg)
ENDDO
CALL rsl_lite_to_child_msg(((config_flags%maxpatch)-(1)+1)*RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%h2osoi_vol9) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
DO k = 1,config_flags%maxpatch
xv(k)= grid%h2osoi_vol9(pig,k,pjg)
ENDDO
CALL rsl_lite_to_child_msg(((config_flags%maxpatch)-(1)+1)*RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%h2osoi_vol10) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
DO k = 1,config_flags%maxpatch
xv(k)= grid%h2osoi_vol10(pig,k,pjg)
ENDDO
CALL rsl_lite_to_child_msg(((config_flags%maxpatch)-(1)+1)*RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%albedosubgrid) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
DO k = 1,config_flags%maxpatch
xv(k)= grid%albedosubgrid(pig,k,pjg)
ENDDO
CALL rsl_lite_to_child_msg(((config_flags%maxpatch)-(1)+1)*RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%lhsubgrid) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
DO k = 1,config_flags%maxpatch
xv(k)= grid%lhsubgrid(pig,k,pjg)
ENDDO
CALL rsl_lite_to_child_msg(((config_flags%maxpatch)-(1)+1)*RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%hfxsubgrid) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
DO k = 1,config_flags%maxpatch
xv(k)= grid%hfxsubgrid(pig,k,pjg)
ENDDO
CALL rsl_lite_to_child_msg(((config_flags%maxpatch)-(1)+1)*RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%lwupsubgrid) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
DO k = 1,config_flags%maxpatch
xv(k)= grid%lwupsubgrid(pig,k,pjg)
ENDDO
CALL rsl_lite_to_child_msg(((config_flags%maxpatch)-(1)+1)*RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%q2subgrid) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
DO k = 1,config_flags%maxpatch
xv(k)= grid%q2subgrid(pig,k,pjg)
ENDDO
CALL rsl_lite_to_child_msg(((config_flags%maxpatch)-(1)+1)*RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%sabvsubgrid) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
DO k = 1,config_flags%maxpatch
xv(k)= grid%sabvsubgrid(pig,k,pjg)
ENDDO
CALL rsl_lite_to_child_msg(((config_flags%maxpatch)-(1)+1)*RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%sabgsubgrid) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
DO k = 1,config_flags%maxpatch
xv(k)= grid%sabgsubgrid(pig,k,pjg)
ENDDO
CALL rsl_lite_to_child_msg(((config_flags%maxpatch)-(1)+1)*RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%nrasubgrid) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
DO k = 1,config_flags%maxpatch
xv(k)= grid%nrasubgrid(pig,k,pjg)
ENDDO
CALL rsl_lite_to_child_msg(((config_flags%maxpatch)-(1)+1)*RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%swupsubgrid) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
DO k = 1,config_flags%maxpatch
xv(k)= grid%swupsubgrid(pig,k,pjg)
ENDDO
CALL rsl_lite_to_child_msg(((config_flags%maxpatch)-(1)+1)*RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%th_old) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
DO k = ckds,(ckde-1)
xv(k)= grid%th_old(pig,k,pjg)
ENDDO
CALL rsl_lite_to_child_msg((((ckde-1))-(ckds)+1)*RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%qv_old) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
DO k = ckds,(ckde-1)
xv(k)= grid%qv_old(pig,k,pjg)
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
IF ( SIZE(grid%landmask) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
xv(1)=grid%landmask(pig,pjg)
CALL rsl_lite_to_child_msg(RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%lakemask) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
xv(1)=grid%lakemask(pig,pjg)
CALL rsl_lite_to_child_msg(RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%sst) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
xv(1)=grid%sst(pig,pjg)
CALL rsl_lite_to_child_msg(RWORDSIZE,xv)
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
