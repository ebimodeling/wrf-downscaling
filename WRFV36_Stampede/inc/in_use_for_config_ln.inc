!STARTOFREGISTRYGENERATEDINCLUDE 'inc/in_use_for_config_ln.inc'
!
! WARNING This file is generated automatically by use_registry
! using the data base in the file named Registry.
! Do not edit.  Your changes to this file will be lost.
!
IF(TRIM(vname).EQ.'lcd_old_mp')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%mp_physics(id).EQ.11
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%mp_physics(id).EQ.11
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'lradius')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%mp_physics(id).EQ.11
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%mp_physics(id).EQ.11
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'lwupt')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%ra_lw_physics(id).EQ.3
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%ra_lw_physics(id).EQ.3
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'lwuptc')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%ra_lw_physics(id).EQ.3
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%ra_lw_physics(id).EQ.3
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'lwdnt')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%ra_lw_physics(id).EQ.3
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%ra_lw_physics(id).EQ.3
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'lwdntc')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%ra_lw_physics(id).EQ.3
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%ra_lw_physics(id).EQ.3
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'lwupb')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%ra_lw_physics(id).EQ.3
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%ra_lw_physics(id).EQ.3
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'lwupbc')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%ra_lw_physics(id).EQ.3
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%ra_lw_physics(id).EQ.3
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'lwdnb')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%ra_lw_physics(id).EQ.3
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%ra_lw_physics(id).EQ.3
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'lwdnbc')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%ra_lw_physics(id).EQ.3
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%ra_lw_physics(id).EQ.3
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'lwupt')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%ra_lw_physics(id).EQ.4
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%ra_lw_physics(id).EQ.4
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'lwuptc')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%ra_lw_physics(id).EQ.4
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%ra_lw_physics(id).EQ.4
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'lwdnt')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%ra_lw_physics(id).EQ.4
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%ra_lw_physics(id).EQ.4
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'lwdntc')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%ra_lw_physics(id).EQ.4
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%ra_lw_physics(id).EQ.4
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'lwupb')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%ra_lw_physics(id).EQ.4
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%ra_lw_physics(id).EQ.4
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'lwupbc')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%ra_lw_physics(id).EQ.4
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%ra_lw_physics(id).EQ.4
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'lwdnb')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%ra_lw_physics(id).EQ.4
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%ra_lw_physics(id).EQ.4
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'lwdnbc')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%ra_lw_physics(id).EQ.4
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%ra_lw_physics(id).EQ.4
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'lwupt')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%ra_sw_physics(id).EQ.3
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%ra_sw_physics(id).EQ.3
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'lwuptc')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%ra_sw_physics(id).EQ.3
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%ra_sw_physics(id).EQ.3
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'lwdnt')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%ra_sw_physics(id).EQ.3
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%ra_sw_physics(id).EQ.3
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'lwdntc')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%ra_sw_physics(id).EQ.3
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%ra_sw_physics(id).EQ.3
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'lwupb')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%ra_sw_physics(id).EQ.3
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%ra_sw_physics(id).EQ.3
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'lwupbc')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%ra_sw_physics(id).EQ.3
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%ra_sw_physics(id).EQ.3
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'lwdnb')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%ra_sw_physics(id).EQ.3
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%ra_sw_physics(id).EQ.3
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'lwdnbc')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%ra_sw_physics(id).EQ.3
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%ra_sw_physics(id).EQ.3
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'mh_urb2d')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_urban_physics(id).EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_urban_physics(id).EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'lf_urb2d')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_urban_physics(id).EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_urban_physics(id).EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'lp_urb2d')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_urban_physics(id).EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_urban_physics(id).EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'lb_urb2d')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_urban_physics(id).EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_urban_physics(id).EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'lp_urb2d')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_urban_physics(id).EQ.2
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_urban_physics(id).EQ.2
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'lb_urb2d')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_urban_physics(id).EQ.2
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_urban_physics(id).EQ.2
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'lf_ac_urb3d')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_urban_physics(id).EQ.3
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_urban_physics(id).EQ.3
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'lfvent_urb3d')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_urban_physics(id).EQ.3
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_urban_physics(id).EQ.3
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'lp_urb2d')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_urban_physics(id).EQ.3
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_urban_physics(id).EQ.3
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'lb_urb2d')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_urban_physics(id).EQ.3
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_urban_physics(id).EQ.3
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'lfmassxy')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_surface_physics(id).EQ.4
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_surface_physics(id).EQ.4
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'neexy')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_surface_physics(id).EQ.4
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_surface_physics(id).EQ.4
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'nppxy')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_surface_physics(id).EQ.4
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_surface_physics(id).EQ.4
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'numc')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_surface_physics(id).EQ.5
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_surface_physics(id).EQ.5
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'nump')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_surface_physics(id).EQ.5
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_surface_physics(id).EQ.5
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'lwup')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_surface_physics(id).EQ.5
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_surface_physics(id).EQ.5
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'lhsoi')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_surface_physics(id).EQ.5
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_surface_physics(id).EQ.5
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'lhveg')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_surface_physics(id).EQ.5
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_surface_physics(id).EQ.5
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'lhtran')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_surface_physics(id).EQ.5
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_surface_physics(id).EQ.5
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'lhsubgrid')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_surface_physics(id).EQ.5
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_surface_physics(id).EQ.5
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'lwupsubgrid')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_surface_physics(id).EQ.5
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_surface_physics(id).EQ.5
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'nrasubgrid')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_surface_physics(id).EQ.5
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_surface_physics(id).EQ.5
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'lh_mosaic')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_surface_mosaic.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_surface_mosaic.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'lh_urb2d_mosaic')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_surface_mosaic.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_surface_mosaic.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'mosaic_cat_index')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_surface_mosaic.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_surface_mosaic.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'landusef2')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_surface_mosaic.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_surface_mosaic.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'massflux_edkf')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%bl_pbl_physics(id).EQ.4
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%bl_pbl_physics(id).EQ.4
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'mf_temf')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%bl_pbl_physics(id).EQ.10
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%bl_pbl_physics(id).EQ.10
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'lcl_temf')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%bl_pbl_physics(id).EQ.10
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%bl_pbl_physics(id).EQ.10
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'md3d')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%cu_physics(id).EQ.7
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%cu_physics(id).EQ.7
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'mu3d')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%cu_physics(id).EQ.7
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%cu_physics(id).EQ.7
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'maxg2d')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%cu_physics(id).EQ.7
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%cu_physics(id).EQ.7
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'lengath2d')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%cu_physics(id).EQ.7
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%cu_physics(id).EQ.7
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'ntraprd_cu')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%shcu_physics(id).EQ.2
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%shcu_physics(id).EQ.2
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'ntsnprd_cu')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%shcu_physics(id).EQ.2
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%shcu_physics(id).EQ.2
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'limit_shcu_cu')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%shcu_physics(id).EQ.2
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%shcu_physics(id).EQ.2
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'limit_negcon_cu')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%shcu_physics(id).EQ.2
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%shcu_physics(id).EQ.2
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'limit_ufrc_cu')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%shcu_physics(id).EQ.2
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%shcu_physics(id).EQ.2
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'limit_ppen_cu')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%shcu_physics(id).EQ.2
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%shcu_physics(id).EQ.2
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'limit_emf_cu')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%shcu_physics(id).EQ.2
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%shcu_physics(id).EQ.2
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'limit_cinlcl_cu')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%shcu_physics(id).EQ.2
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%shcu_physics(id).EQ.2
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'limit_cin_cu')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%shcu_physics(id).EQ.2
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%shcu_physics(id).EQ.2
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'limit_cbmf_cu')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%shcu_physics(id).EQ.2
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%shcu_physics(id).EQ.2
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'limit_rei_cu')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%shcu_physics(id).EQ.2
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%shcu_physics(id).EQ.2
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'max_p')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%nest_interp_coord.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%nest_interp_coord.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'min_p')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%nest_interp_coord.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%nest_interp_coord.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'nfuel_cat')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%ifire(id).EQ.2
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%ifire(id).EQ.2
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'lfn')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%ifire(id).EQ.2
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%ifire(id).EQ.2
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'lfn_hist')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%ifire(id).EQ.2
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%ifire(id).EQ.2
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'lfn_time')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%ifire(id).EQ.2
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%ifire(id).EQ.2
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'lake2d')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_lake_physics(id).EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_lake_physics(id).EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'lakedepth2d')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_lake_physics(id).EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_lake_physics(id).EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'lake_icefrac3d')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_lake_physics(id).EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_lake_physics(id).EQ.1
  ENDIF
ENDIF
!ENDOFREGISTRYGENERATEDINCLUDE
