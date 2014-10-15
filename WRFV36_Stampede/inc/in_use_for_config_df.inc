!STARTOFREGISTRYGENERATEDINCLUDE 'inc/in_use_for_config_df.inc'
!
! WARNING This file is generated automatically by use_registry
! using the data base in the file named Registry.
! Do not edit.  Your changes to this file will be lost.
!
IF(TRIM(vname).EQ.'f_ice_phy')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%mp_physics(id).EQ.5
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%mp_physics(id).EQ.5
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'f_rain_phy')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%mp_physics(id).EQ.5
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%mp_physics(id).EQ.5
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'f_rimef_phy')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%mp_physics(id).EQ.5
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%mp_physics(id).EQ.5
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'dp3d')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%mp_physics(id).EQ.11
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%mp_physics(id).EQ.11
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'dlf')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%mp_physics(id).EQ.11
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%mp_physics(id).EQ.11
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'dlf2')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%mp_physics(id).EQ.11
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%mp_physics(id).EQ.11
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'f_ice_phy')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%mp_physics(id).EQ.95
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%mp_physics(id).EQ.95
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'f_rain_phy')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%mp_physics(id).EQ.95
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%mp_physics(id).EQ.95
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'f_rimef_phy')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%mp_physics(id).EQ.95
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%mp_physics(id).EQ.95
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'dfi_re_cloud')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%mp_physics_dfi(id).EQ.8
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%mp_physics_dfi(id).EQ.8
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'dfi_re_ice')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%mp_physics_dfi(id).EQ.8
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%mp_physics_dfi(id).EQ.8
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'dfi_re_snow')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%mp_physics_dfi(id).EQ.8
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%mp_physics_dfi(id).EQ.8
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'dfi_re_cloud')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%mp_physics_dfi(id).EQ.28
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%mp_physics_dfi(id).EQ.28
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'dfi_re_ice')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%mp_physics_dfi(id).EQ.28
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%mp_physics_dfi(id).EQ.28
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'dfi_re_snow')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%mp_physics_dfi(id).EQ.28
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%mp_physics_dfi(id).EQ.28
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'dlg_bep')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_urban_physics(id).EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_urban_physics(id).EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'dl_u_bep')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_urban_physics(id).EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_urban_physics(id).EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'dlg_bep')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_urban_physics(id).EQ.2
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_urban_physics(id).EQ.2
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'dl_u_bep')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_urban_physics(id).EQ.2
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_urban_physics(id).EQ.2
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'dlg_bep')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_urban_physics(id).EQ.3
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_urban_physics(id).EQ.3
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'dl_u_bep')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_urban_physics(id).EQ.3
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_urban_physics(id).EQ.3
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'flx4')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_surface_physics(id).EQ.2
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_surface_physics(id).EQ.2
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'fvb')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_surface_physics(id).EQ.2
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_surface_physics(id).EQ.2
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'fbur')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_surface_physics(id).EQ.2
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_surface_physics(id).EQ.2
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'fgsn')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_surface_physics(id).EQ.2
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_surface_physics(id).EQ.2
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'eahxy')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_surface_physics(id).EQ.4
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_surface_physics(id).EQ.4
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'fwetxy')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_surface_physics(id).EQ.4
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_surface_physics(id).EQ.4
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'fastcpxy')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_surface_physics(id).EQ.4
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_surface_physics(id).EQ.4
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'fvegxy')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_surface_physics(id).EQ.4
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_surface_physics(id).EQ.4
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'ecanxy')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_surface_physics(id).EQ.4
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_surface_physics(id).EQ.4
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'edirxy')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_surface_physics(id).EQ.4
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_surface_physics(id).EQ.4
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'etranxy')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_surface_physics(id).EQ.4
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_surface_physics(id).EQ.4
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'fsaxy')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_surface_physics(id).EQ.4
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_surface_physics(id).EQ.4
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'firaxy')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_surface_physics(id).EQ.4
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_surface_physics(id).EQ.4
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'evgxy')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_surface_physics(id).EQ.4
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_surface_physics(id).EQ.4
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'evbxy')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_surface_physics(id).EQ.4
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_surface_physics(id).EQ.4
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'evcxy')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_surface_physics(id).EQ.4
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_surface_physics(id).EQ.4
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'deeprechxy')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_surface_physics(id).EQ.4
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_surface_physics(id).EQ.4
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'fdepthxy')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_surface_physics(id).EQ.4
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_surface_physics(id).EQ.4
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'eqzwt')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_surface_physics(id).EQ.4
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_surface_physics(id).EQ.4
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'dzsnow1')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_surface_physics(id).EQ.5
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_surface_physics(id).EQ.5
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'dzsnow2')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_surface_physics(id).EQ.5
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_surface_physics(id).EQ.5
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'dzsnow3')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_surface_physics(id).EQ.5
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_surface_physics(id).EQ.5
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'dzsnow4')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_surface_physics(id).EQ.5
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_surface_physics(id).EQ.5
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'dzsnow5')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_surface_physics(id).EQ.5
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_surface_physics(id).EQ.5
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'dzo1')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_surface_physics(id).EQ.8
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_surface_physics(id).EQ.8
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'fio1')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_surface_physics(id).EQ.8
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_surface_physics(id).EQ.8
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'flo1')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_surface_physics(id).EQ.8
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_surface_physics(id).EQ.8
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'dzo2')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_surface_physics(id).EQ.8
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_surface_physics(id).EQ.8
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'fio2')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_surface_physics(id).EQ.8
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_surface_physics(id).EQ.8
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'flo2')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_surface_physics(id).EQ.8
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_surface_physics(id).EQ.8
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'dzo3')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_surface_physics(id).EQ.8
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_surface_physics(id).EQ.8
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'fio3')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_surface_physics(id).EQ.8
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_surface_physics(id).EQ.8
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'flo3')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_surface_physics(id).EQ.8
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_surface_physics(id).EQ.8
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'dzo4')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_surface_physics(id).EQ.8
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_surface_physics(id).EQ.8
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'fio4')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_surface_physics(id).EQ.8
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_surface_physics(id).EQ.8
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'flo4')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_surface_physics(id).EQ.8
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_surface_physics(id).EQ.8
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'emiss_mosaic')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_surface_mosaic.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_surface_mosaic.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'embck_mosaic')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_surface_mosaic.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_surface_mosaic.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'el_pbl')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%bl_pbl_physics(id).EQ.2
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%bl_pbl_physics(id).EQ.2
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'el_pbl')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%bl_pbl_physics(id).EQ.4
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%bl_pbl_physics(id).EQ.4
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'entr_edkf')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%bl_pbl_physics(id).EQ.4
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%bl_pbl_physics(id).EQ.4
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'detr_edkf')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%bl_pbl_physics(id).EQ.4
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%bl_pbl_physics(id).EQ.4
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'frac_up')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%bl_pbl_physics(id).EQ.4
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%bl_pbl_physics(id).EQ.4
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'el_pbl')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%bl_pbl_physics(id).EQ.94
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%bl_pbl_physics(id).EQ.94
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'el_pbl')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%bl_pbl_physics(id).EQ.5
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%bl_pbl_physics(id).EQ.5
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'el_pbl')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%bl_pbl_physics(id).EQ.6
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%bl_pbl_physics(id).EQ.6
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'dqke')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%bl_mynn_tkebudget(id).EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%bl_mynn_tkebudget(id).EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'el_pbl')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%bl_pbl_physics(id).EQ.8
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%bl_pbl_physics(id).EQ.8
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'exch_tke')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%bl_pbl_physics(id).EQ.12
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%bl_pbl_physics(id).EQ.12
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'el_pbl')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%bl_pbl_physics(id).EQ.12
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%bl_pbl_physics(id).EQ.12
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'evaptzm')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%cu_physics(id).EQ.7
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%cu_physics(id).EQ.7
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'fzsntzm')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%cu_physics(id).EQ.7
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%cu_physics(id).EQ.7
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'evsntzm')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%cu_physics(id).EQ.7
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%cu_physics(id).EQ.7
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'evapqzm')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%cu_physics(id).EQ.7
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%cu_physics(id).EQ.7
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'evapcdp3d')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%cu_physics(id).EQ.7
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%cu_physics(id).EQ.7
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'dp3d')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%cu_physics(id).EQ.7
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%cu_physics(id).EQ.7
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'du3d')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%cu_physics(id).EQ.7
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%cu_physics(id).EQ.7
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'ed3d')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%cu_physics(id).EQ.7
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%cu_physics(id).EQ.7
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'eu3d')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%cu_physics(id).EQ.7
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%cu_physics(id).EQ.7
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'dsubcld2d')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%cu_physics(id).EQ.7
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%cu_physics(id).EQ.7
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'dlf')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%cu_physics(id).EQ.7
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%cu_physics(id).EQ.7
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'dlf')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%shcu_physics(id).EQ.2
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%shcu_physics(id).EQ.2
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'dlf2')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%shcu_physics(id).EQ.2
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%shcu_physics(id).EQ.2
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'emkfbup_cu')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%shcu_physics(id).EQ.2
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%shcu_physics(id).EQ.2
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'fer_cu')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%shcu_physics(id).EQ.2
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%shcu_physics(id).EQ.2
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'fdr_cu')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%shcu_physics(id).EQ.2
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%shcu_physics(id).EQ.2
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'dwten_cu')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%shcu_physics(id).EQ.2
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%shcu_physics(id).EQ.2
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'diten_cu')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%shcu_physics(id).EQ.2
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%shcu_physics(id).EQ.2
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'flxrain_cu')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%shcu_physics(id).EQ.2
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%shcu_physics(id).EQ.2
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'flxsnow_cu')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%shcu_physics(id).EQ.2
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%shcu_physics(id).EQ.2
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'excessu_cu')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%shcu_physics(id).EQ.2
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%shcu_physics(id).EQ.2
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'excessu0_cu')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%shcu_physics(id).EQ.2
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%shcu_physics(id).EQ.2
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'exit_uwcu_cu')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%shcu_physics(id).EQ.2
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%shcu_physics(id).EQ.2
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'exit_conden_cu')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%shcu_physics(id).EQ.2
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%shcu_physics(id).EQ.2
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'exit_klclmkx_cu')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%shcu_physics(id).EQ.2
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%shcu_physics(id).EQ.2
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'exit_klfcmkx_cu')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%shcu_physics(id).EQ.2
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%shcu_physics(id).EQ.2
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'exit_ufrc_cu')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%shcu_physics(id).EQ.2
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%shcu_physics(id).EQ.2
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'exit_wtw_cu')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%shcu_physics(id).EQ.2
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%shcu_physics(id).EQ.2
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'exit_drycore_cu')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%shcu_physics(id).EQ.2
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%shcu_physics(id).EQ.2
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'exit_wu_cu')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%shcu_physics(id).EQ.2
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%shcu_physics(id).EQ.2
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'exit_cufliter_cu')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%shcu_physics(id).EQ.2
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%shcu_physics(id).EQ.2
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'exit_kinv1_cu')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%shcu_physics(id).EQ.2
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%shcu_physics(id).EQ.2
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'exit_rei_cu')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%shcu_physics(id).EQ.2
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%shcu_physics(id).EQ.2
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'evapcsh')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%shcu_physics(id).EQ.2
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%shcu_physics(id).EQ.2
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'fgdp')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%grav_settling(id).EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%grav_settling(id).EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'dfgdp')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%grav_settling(id).EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%grav_settling(id).EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'fgdp')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%grav_settling(id).EQ.2
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%grav_settling(id).EQ.2
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'dfgdp')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%grav_settling(id).EQ.2
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%grav_settling(id).EQ.2
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'dtaux3d')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%gwd_opt.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%gwd_opt.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'dtauy3d')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%gwd_opt.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%gwd_opt.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'dusfcg')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%gwd_opt.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%gwd_opt.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'dvsfcg')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%gwd_opt.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%gwd_opt.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'echotop')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%afwa_radar_opt(id).EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%afwa_radar_opt(id).EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'fzlev')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%afwa_icing_opt(id).EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%afwa_icing_opt(id).EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'dfi_u')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%dfi_opt.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%dfi_opt.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'dfi_v')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%dfi_opt.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%dfi_opt.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'dfi_w')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%dfi_opt.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%dfi_opt.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'dfi_ph')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%dfi_opt.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%dfi_opt.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'dfi_phb')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%dfi_opt.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%dfi_opt.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'dfi_ph0')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%dfi_opt.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%dfi_opt.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'dfi_php')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%dfi_opt.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%dfi_opt.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'dfi_t')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%dfi_opt.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%dfi_opt.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'dfi_p')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%dfi_opt.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%dfi_opt.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'dfi_ww')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%dfi_opt.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%dfi_opt.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'dfi_mu')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%dfi_opt.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%dfi_opt.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'dfi_tke')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%dfi_opt.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%dfi_opt.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'dfi_pb')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%dfi_opt.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%dfi_opt.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'dfi_al')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%dfi_opt.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%dfi_opt.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'dfi_alt')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%dfi_opt.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%dfi_opt.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'dfi_tslb')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%dfi_opt.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%dfi_opt.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'dfi_smois')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%dfi_opt.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%dfi_opt.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'dfi_snow')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%dfi_opt.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%dfi_opt.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'dfi_snowh')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%dfi_opt.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%dfi_opt.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'dfi_canwat')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%dfi_opt.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%dfi_opt.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'dfi_smfr3d')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%dfi_opt.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%dfi_opt.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'dfi_keepfr3dflag')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%dfi_opt.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%dfi_opt.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'dfi_tsk')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%dfi_opt.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%dfi_opt.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'dfi_soilt1')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%dfi_opt.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%dfi_opt.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'dfi_tsnav')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%dfi_opt.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%dfi_opt.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'dfi_snowc')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%dfi_opt.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%dfi_opt.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'dfi_qvg')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%dfi_opt.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%dfi_opt.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'dfi_rh')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%dfi_opt.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%dfi_opt.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'dfi_tten_rad')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%dfi_opt.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%dfi_opt.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'dfi_u')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%dfi_opt.EQ.2
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%dfi_opt.EQ.2
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'dfi_v')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%dfi_opt.EQ.2
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%dfi_opt.EQ.2
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'dfi_w')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%dfi_opt.EQ.2
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%dfi_opt.EQ.2
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'dfi_ph')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%dfi_opt.EQ.2
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%dfi_opt.EQ.2
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'dfi_phb')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%dfi_opt.EQ.2
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%dfi_opt.EQ.2
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'dfi_ph0')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%dfi_opt.EQ.2
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%dfi_opt.EQ.2
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'dfi_php')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%dfi_opt.EQ.2
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%dfi_opt.EQ.2
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'dfi_t')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%dfi_opt.EQ.2
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%dfi_opt.EQ.2
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'dfi_p')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%dfi_opt.EQ.2
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%dfi_opt.EQ.2
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'dfi_ww')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%dfi_opt.EQ.2
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%dfi_opt.EQ.2
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'dfi_mu')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%dfi_opt.EQ.2
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%dfi_opt.EQ.2
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'dfi_tke')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%dfi_opt.EQ.2
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%dfi_opt.EQ.2
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'dfi_pb')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%dfi_opt.EQ.2
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%dfi_opt.EQ.2
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'dfi_al')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%dfi_opt.EQ.2
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%dfi_opt.EQ.2
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'dfi_alt')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%dfi_opt.EQ.2
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%dfi_opt.EQ.2
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'dfi_tslb')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%dfi_opt.EQ.2
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%dfi_opt.EQ.2
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'dfi_smois')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%dfi_opt.EQ.2
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%dfi_opt.EQ.2
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'dfi_snow')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%dfi_opt.EQ.2
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%dfi_opt.EQ.2
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'dfi_snowh')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%dfi_opt.EQ.2
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%dfi_opt.EQ.2
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'dfi_canwat')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%dfi_opt.EQ.2
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%dfi_opt.EQ.2
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'dfi_smfr3d')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%dfi_opt.EQ.2
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%dfi_opt.EQ.2
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'dfi_keepfr3dflag')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%dfi_opt.EQ.2
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%dfi_opt.EQ.2
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'dfi_tsk')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%dfi_opt.EQ.2
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%dfi_opt.EQ.2
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'dfi_soilt1')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%dfi_opt.EQ.2
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%dfi_opt.EQ.2
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'dfi_tsnav')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%dfi_opt.EQ.2
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%dfi_opt.EQ.2
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'dfi_snowc')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%dfi_opt.EQ.2
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%dfi_opt.EQ.2
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'dfi_qvg')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%dfi_opt.EQ.2
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%dfi_opt.EQ.2
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'dfi_rh')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%dfi_opt.EQ.2
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%dfi_opt.EQ.2
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'dfi_tten_rad')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%dfi_opt.EQ.2
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%dfi_opt.EQ.2
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'dfi_u')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%dfi_opt.EQ.3
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%dfi_opt.EQ.3
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'dfi_v')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%dfi_opt.EQ.3
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%dfi_opt.EQ.3
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'dfi_w')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%dfi_opt.EQ.3
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%dfi_opt.EQ.3
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'dfi_ph')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%dfi_opt.EQ.3
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%dfi_opt.EQ.3
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'dfi_phb')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%dfi_opt.EQ.3
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%dfi_opt.EQ.3
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'dfi_ph0')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%dfi_opt.EQ.3
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%dfi_opt.EQ.3
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'dfi_php')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%dfi_opt.EQ.3
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%dfi_opt.EQ.3
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'dfi_t')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%dfi_opt.EQ.3
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%dfi_opt.EQ.3
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'dfi_p')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%dfi_opt.EQ.3
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%dfi_opt.EQ.3
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'dfi_ww')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%dfi_opt.EQ.3
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%dfi_opt.EQ.3
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'dfi_mu')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%dfi_opt.EQ.3
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%dfi_opt.EQ.3
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'dfi_tke')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%dfi_opt.EQ.3
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%dfi_opt.EQ.3
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'dfi_pb')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%dfi_opt.EQ.3
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%dfi_opt.EQ.3
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'dfi_al')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%dfi_opt.EQ.3
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%dfi_opt.EQ.3
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'dfi_alt')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%dfi_opt.EQ.3
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%dfi_opt.EQ.3
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'dfi_tslb')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%dfi_opt.EQ.3
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%dfi_opt.EQ.3
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'dfi_smois')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%dfi_opt.EQ.3
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%dfi_opt.EQ.3
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'dfi_snow')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%dfi_opt.EQ.3
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%dfi_opt.EQ.3
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'dfi_snowh')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%dfi_opt.EQ.3
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%dfi_opt.EQ.3
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'dfi_canwat')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%dfi_opt.EQ.3
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%dfi_opt.EQ.3
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'dfi_smfr3d')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%dfi_opt.EQ.3
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%dfi_opt.EQ.3
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'dfi_keepfr3dflag')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%dfi_opt.EQ.3
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%dfi_opt.EQ.3
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'dfi_tsk')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%dfi_opt.EQ.3
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%dfi_opt.EQ.3
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'dfi_soilt1')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%dfi_opt.EQ.3
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%dfi_opt.EQ.3
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'dfi_tsnav')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%dfi_opt.EQ.3
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%dfi_opt.EQ.3
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'dfi_snowc')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%dfi_opt.EQ.3
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%dfi_opt.EQ.3
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'dfi_qvg')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%dfi_opt.EQ.3
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%dfi_opt.EQ.3
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'dfi_rh')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%dfi_opt.EQ.3
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%dfi_opt.EQ.3
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'dfi_tten_rad')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%dfi_opt.EQ.3
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%dfi_opt.EQ.3
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'fad0_urb2d')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%use_wps_input.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%use_wps_input.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'fad135_urb2d')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%use_wps_input.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%use_wps_input.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'fad45_urb2d')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%use_wps_input.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%use_wps_input.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'fad90_urb2d')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%use_wps_input.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%use_wps_input.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'fuel_frac')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%ifire(id).EQ.2
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%ifire(id).EQ.2
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'fire_area')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%ifire(id).EQ.2
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%ifire(id).EQ.2
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'fgrnhfx')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%ifire(id).EQ.2
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%ifire(id).EQ.2
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'fgrnqfx')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%ifire(id).EQ.2
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%ifire(id).EQ.2
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'fcanhfx')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%ifire(id).EQ.2
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%ifire(id).EQ.2
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'fcanhfx')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%ifire(id).EQ.2
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%ifire(id).EQ.2
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'fcanqfx')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%ifire(id).EQ.2
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%ifire(id).EQ.2
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'fxlong')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%ifire(id).EQ.2
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%ifire(id).EQ.2
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'fxlat')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%ifire(id).EQ.2
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%ifire(id).EQ.2
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'fuel_time')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%ifire(id).EQ.2
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%ifire(id).EQ.2
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'fgip')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%ifire(id).EQ.2
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%ifire(id).EQ.2
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'dfu1')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%do_avgflx_cugd(id).EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%do_avgflx_cugd(id).EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'efu1')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%do_avgflx_cugd(id).EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%do_avgflx_cugd(id).EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'dfd1')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%do_avgflx_cugd(id).EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%do_avgflx_cugd(id).EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'efd1')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%do_avgflx_cugd(id).EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%do_avgflx_cugd(id).EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'dz_lake3d')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_lake_physics(id).EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_lake_physics(id).EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'dz3d')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_lake_physics(id).EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_lake_physics(id).EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'field_u_tend_perturb')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%perturb_bdy.EQ.2
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%perturb_bdy.EQ.2
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'field_v_tend_perturb')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%perturb_bdy.EQ.2
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%perturb_bdy.EQ.2
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'field_t_tend_perturb')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%perturb_bdy.EQ.2
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%perturb_bdy.EQ.2
  ENDIF
ENDIF
!ENDOFREGISTRYGENERATEDINCLUDE
