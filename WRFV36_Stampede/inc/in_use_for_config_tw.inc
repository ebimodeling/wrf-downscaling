!STARTOFREGISTRYGENERATEDINCLUDE 'inc/in_use_for_config_tw.inc'
!
! WARNING This file is generated automatically by use_registry
! using the data base in the file named Registry.
! Do not edit.  Your changes to this file will be lost.
!
IF(TRIM(vname).EQ.'turbtype3d')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%mp_physics(id).EQ.11
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%mp_physics(id).EQ.11
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'wsedl3d')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%mp_physics(id).EQ.11
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%mp_physics(id).EQ.11
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'tke_pbl')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%mp_physics(id).EQ.11
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%mp_physics(id).EQ.11
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'tlwdn')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%ra_lw_physics(id).EQ.5
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%ra_lw_physics(id).EQ.5
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'tlwup')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%ra_lw_physics(id).EQ.5
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%ra_lw_physics(id).EQ.5
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'tswdn')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%ra_sw_physics(id).EQ.5
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%ra_sw_physics(id).EQ.5
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'tswup')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%ra_sw_physics(id).EQ.5
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%ra_sw_physics(id).EQ.5
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'tke_pbl')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_sfclay_physics(id).EQ.2
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_sfclay_physics(id).EQ.2
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'tsq')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_sfclay_physics(id).EQ.5
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_sfclay_physics(id).EQ.5
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'wm_temf')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_sfclay_physics(id).EQ.10
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_sfclay_physics(id).EQ.10
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'trb_urb4d')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_urban_physics(id).EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_urban_physics(id).EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'tw1_urb4d')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_urban_physics(id).EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_urban_physics(id).EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'tw2_urb4d')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_urban_physics(id).EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_urban_physics(id).EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'tgb_urb4d')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_urban_physics(id).EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_urban_physics(id).EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'vl_bep')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_urban_physics(id).EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_urban_physics(id).EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'vl_bep')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_urban_physics(id).EQ.2
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_urban_physics(id).EQ.2
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'trb_urb4d')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_urban_physics(id).EQ.2
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_urban_physics(id).EQ.2
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'tw1_urb4d')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_urban_physics(id).EQ.2
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_urban_physics(id).EQ.2
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'tw2_urb4d')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_urban_physics(id).EQ.2
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_urban_physics(id).EQ.2
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'tgb_urb4d')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_urban_physics(id).EQ.2
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_urban_physics(id).EQ.2
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'vl_bep')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_urban_physics(id).EQ.3
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_urban_physics(id).EQ.3
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'trb_urb4d')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_urban_physics(id).EQ.3
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_urban_physics(id).EQ.3
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'tw1_urb4d')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_urban_physics(id).EQ.3
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_urban_physics(id).EQ.3
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'tw2_urb4d')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_urban_physics(id).EQ.3
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_urban_physics(id).EQ.3
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'tgb_urb4d')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_urban_physics(id).EQ.3
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_urban_physics(id).EQ.3
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'tlev_urb3d')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_urban_physics(id).EQ.3
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_urban_physics(id).EQ.3
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'tw1lev_urb3d')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_urban_physics(id).EQ.3
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_urban_physics(id).EQ.3
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'tw2lev_urb3d')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_urban_physics(id).EQ.3
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_urban_physics(id).EQ.3
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'tglev_urb3d')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_urban_physics(id).EQ.3
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_urban_physics(id).EQ.3
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'tflev_urb3d')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_urban_physics(id).EQ.3
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_urban_physics(id).EQ.3
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'tvxy')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_surface_physics(id).EQ.4
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_surface_physics(id).EQ.4
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'tgxy')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_surface_physics(id).EQ.4
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_surface_physics(id).EQ.4
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'tahxy')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_surface_physics(id).EQ.4
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_surface_physics(id).EQ.4
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'wslakexy')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_surface_physics(id).EQ.4
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_surface_physics(id).EQ.4
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'waxy')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_surface_physics(id).EQ.4
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_surface_physics(id).EQ.4
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'wtxy')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_surface_physics(id).EQ.4
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_surface_physics(id).EQ.4
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'tsnoxy')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_surface_physics(id).EQ.4
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_surface_physics(id).EQ.4
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'woodxy')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_surface_physics(id).EQ.4
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_surface_physics(id).EQ.4
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'taussxy')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_surface_physics(id).EQ.4
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_surface_physics(id).EQ.4
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'t2mvxy')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_surface_physics(id).EQ.4
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_surface_physics(id).EQ.4
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'t2mbxy')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_surface_physics(id).EQ.4
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_surface_physics(id).EQ.4
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'tradxy')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_surface_physics(id).EQ.4
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_surface_physics(id).EQ.4
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'wgapxy')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_surface_physics(id).EQ.4
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_surface_physics(id).EQ.4
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'tgvxy')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_surface_physics(id).EQ.4
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_surface_physics(id).EQ.4
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'tgbxy')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_surface_physics(id).EQ.4
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_surface_physics(id).EQ.4
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'trxy')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_surface_physics(id).EQ.4
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_surface_physics(id).EQ.4
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'wtc')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_surface_physics(id).EQ.5
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_surface_physics(id).EQ.5
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'wtp')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_surface_physics(id).EQ.5
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_surface_physics(id).EQ.5
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'t_grnd')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_surface_physics(id).EQ.5
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_surface_physics(id).EQ.5
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'t_veg')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_surface_physics(id).EQ.5
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_surface_physics(id).EQ.5
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'t2m_max')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_surface_physics(id).EQ.5
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_surface_physics(id).EQ.5
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'t2m_min')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_surface_physics(id).EQ.5
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_surface_physics(id).EQ.5
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'t2clm')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_surface_physics(id).EQ.5
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_surface_physics(id).EQ.5
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'t_ref2m')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_surface_physics(id).EQ.5
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_surface_physics(id).EQ.5
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'t_soisno_s1')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_surface_physics(id).EQ.5
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_surface_physics(id).EQ.5
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'t_soisno_s2')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_surface_physics(id).EQ.5
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_surface_physics(id).EQ.5
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'t_soisno_s3')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_surface_physics(id).EQ.5
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_surface_physics(id).EQ.5
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'t_soisno_s4')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_surface_physics(id).EQ.5
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_surface_physics(id).EQ.5
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'t_soisno_s5')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_surface_physics(id).EQ.5
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_surface_physics(id).EQ.5
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'t_soisno1')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_surface_physics(id).EQ.5
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_surface_physics(id).EQ.5
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'t_soisno2')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_surface_physics(id).EQ.5
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_surface_physics(id).EQ.5
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'t_soisno3')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_surface_physics(id).EQ.5
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_surface_physics(id).EQ.5
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'t_soisno4')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_surface_physics(id).EQ.5
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_surface_physics(id).EQ.5
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'t_soisno5')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_surface_physics(id).EQ.5
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_surface_physics(id).EQ.5
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'t_soisno6')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_surface_physics(id).EQ.5
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_surface_physics(id).EQ.5
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'t_soisno7')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_surface_physics(id).EQ.5
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_surface_physics(id).EQ.5
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'t_soisno8')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_surface_physics(id).EQ.5
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_surface_physics(id).EQ.5
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'t_soisno9')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_surface_physics(id).EQ.5
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_surface_physics(id).EQ.5
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'t_soisno10')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_surface_physics(id).EQ.5
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_surface_physics(id).EQ.5
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'t_lake1')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_surface_physics(id).EQ.5
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_surface_physics(id).EQ.5
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'t_lake2')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_surface_physics(id).EQ.5
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_surface_physics(id).EQ.5
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'t_lake3')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_surface_physics(id).EQ.5
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_surface_physics(id).EQ.5
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'t_lake4')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_surface_physics(id).EQ.5
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_surface_physics(id).EQ.5
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'t_lake5')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_surface_physics(id).EQ.5
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_surface_physics(id).EQ.5
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'t_lake6')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_surface_physics(id).EQ.5
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_surface_physics(id).EQ.5
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'t_lake7')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_surface_physics(id).EQ.5
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_surface_physics(id).EQ.5
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'t_lake8')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_surface_physics(id).EQ.5
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_surface_physics(id).EQ.5
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'t_lake9')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_surface_physics(id).EQ.5
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_surface_physics(id).EQ.5
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'t_lake10')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_surface_physics(id).EQ.5
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_surface_physics(id).EQ.5
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'t2_ndg_new')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_surface_physics(id).EQ.7
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_surface_physics(id).EQ.7
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'t2_ndg_old')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_surface_physics(id).EQ.7
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_surface_physics(id).EQ.7
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'tkair')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_surface_physics(id).EQ.8
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_surface_physics(id).EQ.8
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'wo1')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_surface_physics(id).EQ.8
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_surface_physics(id).EQ.8
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'tssn1')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_surface_physics(id).EQ.8
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_surface_physics(id).EQ.8
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'tssno1')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_surface_physics(id).EQ.8
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_surface_physics(id).EQ.8
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'wo2')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_surface_physics(id).EQ.8
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_surface_physics(id).EQ.8
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'tssn2')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_surface_physics(id).EQ.8
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_surface_physics(id).EQ.8
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'tssno2')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_surface_physics(id).EQ.8
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_surface_physics(id).EQ.8
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'wo3')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_surface_physics(id).EQ.8
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_surface_physics(id).EQ.8
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'tssn3')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_surface_physics(id).EQ.8
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_surface_physics(id).EQ.8
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'tssno3')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_surface_physics(id).EQ.8
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_surface_physics(id).EQ.8
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'wo4')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_surface_physics(id).EQ.8
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_surface_physics(id).EQ.8
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'tssn4')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_surface_physics(id).EQ.8
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_surface_physics(id).EQ.8
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'tssno4')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_surface_physics(id).EQ.8
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_surface_physics(id).EQ.8
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'tsk_mosaic')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_surface_mosaic.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_surface_mosaic.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'tslb_mosaic')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_surface_mosaic.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_surface_mosaic.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'tr_urb2d_mosaic')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_surface_mosaic.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_surface_mosaic.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'tb_urb2d_mosaic')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_surface_mosaic.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_surface_mosaic.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'tg_urb2d_mosaic')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_surface_mosaic.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_surface_mosaic.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'tc_urb2d_mosaic')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_surface_mosaic.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_surface_mosaic.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'ts_urb2d_mosaic')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_surface_mosaic.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_surface_mosaic.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'ts_rul2d_mosaic')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_surface_mosaic.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_surface_mosaic.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'uc_urb2d_mosaic')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_surface_mosaic.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_surface_mosaic.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'trl_urb3d_mosaic')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_surface_mosaic.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_surface_mosaic.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'tbl_urb3d_mosaic')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_surface_mosaic.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_surface_mosaic.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'tgl_urb3d_mosaic')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_surface_mosaic.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_surface_mosaic.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'tke_pbl')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%bl_pbl_physics(id).EQ.2
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%bl_pbl_physics(id).EQ.2
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'tke_pbl')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%bl_pbl_physics(id).EQ.4
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%bl_pbl_physics(id).EQ.4
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'thl_up')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%bl_pbl_physics(id).EQ.4
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%bl_pbl_physics(id).EQ.4
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'thv_up')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%bl_pbl_physics(id).EQ.4
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%bl_pbl_physics(id).EQ.4
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'u_up')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%bl_pbl_physics(id).EQ.4
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%bl_pbl_physics(id).EQ.4
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'v_up')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%bl_pbl_physics(id).EQ.4
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%bl_pbl_physics(id).EQ.4
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'tke_pbl')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%bl_pbl_physics(id).EQ.94
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%bl_pbl_physics(id).EQ.94
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'tke_pbl')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%bl_pbl_physics(id).EQ.5
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%bl_pbl_physics(id).EQ.5
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'tsq')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%bl_pbl_physics(id).EQ.5
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%bl_pbl_physics(id).EQ.5
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'tke_pbl')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%bl_pbl_physics(id).EQ.6
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%bl_pbl_physics(id).EQ.6
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'tsq')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%bl_pbl_physics(id).EQ.6
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%bl_pbl_physics(id).EQ.6
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'tke_pbl')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%bl_pbl_physics(id).EQ.8
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%bl_pbl_physics(id).EQ.8
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'wu_tur')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%bl_pbl_physics(id).EQ.8
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%bl_pbl_physics(id).EQ.8
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'wv_tur')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%bl_pbl_physics(id).EQ.8
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%bl_pbl_physics(id).EQ.8
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'wt_tur')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%bl_pbl_physics(id).EQ.8
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%bl_pbl_physics(id).EQ.8
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'wq_tur')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%bl_pbl_physics(id).EQ.8
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%bl_pbl_physics(id).EQ.8
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'tauresx2d')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%bl_pbl_physics(id).EQ.9
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%bl_pbl_physics(id).EQ.9
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'tauresy2d')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%bl_pbl_physics(id).EQ.9
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%bl_pbl_physics(id).EQ.9
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'tpert2d')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%bl_pbl_physics(id).EQ.9
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%bl_pbl_physics(id).EQ.9
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'wpert2d')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%bl_pbl_physics(id).EQ.9
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%bl_pbl_physics(id).EQ.9
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'tke_pbl')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%bl_pbl_physics(id).EQ.9
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%bl_pbl_physics(id).EQ.9
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'wsedl3d')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%bl_pbl_physics(id).EQ.9
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%bl_pbl_physics(id).EQ.9
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'turbtype3d')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%bl_pbl_physics(id).EQ.9
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%bl_pbl_physics(id).EQ.9
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'te_temf')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%bl_pbl_physics(id).EQ.10
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%bl_pbl_physics(id).EQ.10
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'uw_temf')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%bl_pbl_physics(id).EQ.10
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%bl_pbl_physics(id).EQ.10
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'vw_temf')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%bl_pbl_physics(id).EQ.10
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%bl_pbl_physics(id).EQ.10
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'wupd_temf')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%bl_pbl_physics(id).EQ.10
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%bl_pbl_physics(id).EQ.10
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'thup_temf')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%bl_pbl_physics(id).EQ.10
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%bl_pbl_physics(id).EQ.10
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'tke_pbl')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%bl_pbl_physics(id).EQ.12
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%bl_pbl_physics(id).EQ.12
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'tpert2d')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%cu_physics(id).EQ.7
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%cu_physics(id).EQ.7
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'uflx_cu')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%shcu_physics(id).EQ.2
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%shcu_physics(id).EQ.2
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'vflx_cu')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%shcu_physics(id).EQ.2
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%shcu_physics(id).EQ.2
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'uten_cu')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%shcu_physics(id).EQ.2
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%shcu_physics(id).EQ.2
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'vten_cu')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%shcu_physics(id).EQ.2
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%shcu_physics(id).EQ.2
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'ufrcinvbase_cu')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%shcu_physics(id).EQ.2
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%shcu_physics(id).EQ.2
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'ufrclcl_cu')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%shcu_physics(id).EQ.2
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%shcu_physics(id).EQ.2
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'winvbase_cu')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%shcu_physics(id).EQ.2
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%shcu_physics(id).EQ.2
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'wlcl_cu')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%shcu_physics(id).EQ.2
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%shcu_physics(id).EQ.2
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'thlsrc_cu')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%shcu_physics(id).EQ.2
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%shcu_physics(id).EQ.2
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'thvlsrc_cu')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%shcu_physics(id).EQ.2
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%shcu_physics(id).EQ.2
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'tkeavg_cu')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%shcu_physics(id).EQ.2
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%shcu_physics(id).EQ.2
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'tophgt_cu')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%shcu_physics(id).EQ.2
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%shcu_physics(id).EQ.2
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'wu_cu')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%shcu_physics(id).EQ.2
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%shcu_physics(id).EQ.2
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'ufrc_cu')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%shcu_physics(id).EQ.2
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%shcu_physics(id).EQ.2
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'thlu_cu')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%shcu_physics(id).EQ.2
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%shcu_physics(id).EQ.2
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'thvu_cu')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%shcu_physics(id).EQ.2
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%shcu_physics(id).EQ.2
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'uu_cu')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%shcu_physics(id).EQ.2
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%shcu_physics(id).EQ.2
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'vu_cu')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%shcu_physics(id).EQ.2
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%shcu_physics(id).EQ.2
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'thlu_emf_cu')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%shcu_physics(id).EQ.2
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%shcu_physics(id).EQ.2
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'uu_emf_cu')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%shcu_physics(id).EQ.2
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%shcu_physics(id).EQ.2
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'vu_emf_cu')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%shcu_physics(id).EQ.2
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%shcu_physics(id).EQ.2
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'umf_cu')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%shcu_physics(id).EQ.2
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%shcu_physics(id).EQ.2
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'uemf_cu')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%shcu_physics(id).EQ.2
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%shcu_physics(id).EQ.2
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'vdfg')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%grav_settling(id).EQ.0
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%grav_settling(id).EQ.0
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'vdfg')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%grav_settling(id).EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%grav_settling(id).EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'vdfg')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%grav_settling(id).EQ.2
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%grav_settling(id).EQ.2
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'u10_ndg_old')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%grid_sfdda(id).EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%grid_sfdda(id).EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'v10_ndg_old')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%grid_sfdda(id).EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%grid_sfdda(id).EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'t2_ndg_old')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%grid_sfdda(id).EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%grid_sfdda(id).EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'th2_ndg_old')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%grid_sfdda(id).EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%grid_sfdda(id).EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'u10_ndg_new')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%grid_sfdda(id).EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%grid_sfdda(id).EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'v10_ndg_new')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%grid_sfdda(id).EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%grid_sfdda(id).EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'t2_ndg_new')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%grid_sfdda(id).EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%grid_sfdda(id).EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'th2_ndg_new')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%grid_sfdda(id).EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%grid_sfdda(id).EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'tob_ndg_old')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%grid_sfdda(id).EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%grid_sfdda(id).EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'tob_ndg_new')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%grid_sfdda(id).EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%grid_sfdda(id).EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'tml')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_ocean_physics.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_ocean_physics.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'t0ml')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_ocean_physics.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_ocean_physics.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'tmoml')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_ocean_physics.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_ocean_physics.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'tml')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_ocean_physics.EQ.2
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_ocean_physics.EQ.2
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'t0ml')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_ocean_physics.EQ.2
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_ocean_physics.EQ.2
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'tmoml')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_ocean_physics.EQ.2
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_ocean_physics.EQ.2
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'u_g')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%scm_force.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%scm_force.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'u_g_tend')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%scm_force.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%scm_force.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'v_g')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%scm_force.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%scm_force.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'v_g_tend')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%scm_force.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%scm_force.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'w_subs')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%scm_force.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%scm_force.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'w_subs_tend')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%scm_force.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%scm_force.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'th_upstream_x')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%scm_force.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%scm_force.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'th_upstream_x_tend')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%scm_force.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%scm_force.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'th_upstream_y')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%scm_force.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%scm_force.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'th_upstream_y_tend')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%scm_force.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%scm_force.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'u_upstream_x')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%scm_force.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%scm_force.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'u_upstream_x_tend')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%scm_force.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%scm_force.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'u_upstream_y')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%scm_force.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%scm_force.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'u_upstream_y_tend')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%scm_force.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%scm_force.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'v_upstream_x')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%scm_force.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%scm_force.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'v_upstream_x_tend')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%scm_force.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%scm_force.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'v_upstream_y')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%scm_force.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%scm_force.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'v_upstream_y_tend')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%scm_force.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%scm_force.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'tau_x')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%scm_force.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%scm_force.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'tau_x_tend')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%scm_force.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%scm_force.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'tau_y')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%scm_force.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%scm_force.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'tau_y_tend')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%scm_force.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%scm_force.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'th_largescale')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%scm_force.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%scm_force.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'th_largescale_tend')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%scm_force.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%scm_force.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'u_largescale')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%scm_force.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%scm_force.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'u_largescale_tend')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%scm_force.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%scm_force.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'v_largescale')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%scm_force.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%scm_force.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'v_largescale_tend')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%scm_force.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%scm_force.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'tau_largescale')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%scm_force.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%scm_force.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'tau_largescale_tend')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%scm_force.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%scm_force.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'t_soil_forcing_val')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%scm_force.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%scm_force.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'t_soil_forcing_tend')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%scm_force.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%scm_force.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'tau_soil')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%scm_force.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%scm_force.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'th_t_tend')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%scm_force.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%scm_force.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'t2min')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%output_diagnostics.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%output_diagnostics.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'t2max')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%output_diagnostics.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%output_diagnostics.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'tt2min')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%output_diagnostics.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%output_diagnostics.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'tt2max')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%output_diagnostics.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%output_diagnostics.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'t2mean')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%output_diagnostics.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%output_diagnostics.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'t2std')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%output_diagnostics.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%output_diagnostics.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'tq2min')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%output_diagnostics.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%output_diagnostics.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'tq2max')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%output_diagnostics.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%output_diagnostics.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'tskintempmin')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%output_diagnostics.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%output_diagnostics.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'tskintempmax')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%output_diagnostics.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%output_diagnostics.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'u10max')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%output_diagnostics.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%output_diagnostics.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'v10max')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%output_diagnostics.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%output_diagnostics.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'tspduv10max')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%output_diagnostics.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%output_diagnostics.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'u10mean')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%output_diagnostics.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%output_diagnostics.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'v10mean')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%output_diagnostics.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%output_diagnostics.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'u10std')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%output_diagnostics.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%output_diagnostics.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'v10std')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%output_diagnostics.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%output_diagnostics.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'traincvmax')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%output_diagnostics.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%output_diagnostics.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'trainncvmax')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%output_diagnostics.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%output_diagnostics.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'wspd10max')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%nwp_diagnostics.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%nwp_diagnostics.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'w_up_max')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%nwp_diagnostics.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%nwp_diagnostics.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'w_dn_max')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%nwp_diagnostics.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%nwp_diagnostics.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'up_heli_max')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%nwp_diagnostics.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%nwp_diagnostics.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'w_mean')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%nwp_diagnostics.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%nwp_diagnostics.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'vil')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%afwa_vil_opt(id).EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%afwa_vil_opt(id).EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'wspd10max')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%afwa_severe_opt(id).EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%afwa_severe_opt(id).EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'w_up_max')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%afwa_severe_opt(id).EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%afwa_severe_opt(id).EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'w_dn_max')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%afwa_severe_opt(id).EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%afwa_severe_opt(id).EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'tcoli_max')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%afwa_severe_opt(id).EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%afwa_severe_opt(id).EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'u_gc')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%use_wps_input.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%use_wps_input.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'v_gc')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%use_wps_input.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%use_wps_input.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'t_gc')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%use_wps_input.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%use_wps_input.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'tsk_gc')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%use_wps_input.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%use_wps_input.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'tavgsfc')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%use_wps_input.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%use_wps_input.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'tmn_gc')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%use_wps_input.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%use_wps_input.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'u_gc')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%use_wps_input.EQ.2
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%use_wps_input.EQ.2
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'v_gc')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%use_wps_input.EQ.2
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%use_wps_input.EQ.2
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'t_gc')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%use_wps_input.EQ.2
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%use_wps_input.EQ.2
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'tsk_gc')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%use_wps_input.EQ.2
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%use_wps_input.EQ.2
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'tavgsfc')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%use_wps_input.EQ.2
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%use_wps_input.EQ.2
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'tmn_gc')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%use_wps_input.EQ.2
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%use_wps_input.EQ.2
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'t_max_p')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%nest_interp_coord.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%nest_interp_coord.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'t_min_p')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%nest_interp_coord.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%nest_interp_coord.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'traj_i')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%traj_opt.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%traj_opt.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'traj_j')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%traj_opt.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%traj_opt.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'traj_k')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%traj_opt.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%traj_opt.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'traj_lat')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%traj_opt.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%traj_opt.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'traj_long')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%traj_opt.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%traj_opt.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'ts_hour')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%process_time_series.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%process_time_series.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'ts_u')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%process_time_series.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%process_time_series.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'ts_v')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%process_time_series.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%process_time_series.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'ts_q')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%process_time_series.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%process_time_series.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'ts_t')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%process_time_series.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%process_time_series.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'ts_psfc')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%process_time_series.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%process_time_series.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'ts_glw')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%process_time_series.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%process_time_series.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'ts_gsw')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%process_time_series.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%process_time_series.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'ts_hfx')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%process_time_series.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%process_time_series.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'ts_lh')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%process_time_series.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%process_time_series.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'ts_tsk')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%process_time_series.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%process_time_series.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'ts_tslb')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%process_time_series.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%process_time_series.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'ts_clw')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%process_time_series.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%process_time_series.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'ts_rainc')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%process_time_series.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%process_time_series.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'ts_rainnc')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%process_time_series.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%process_time_series.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'ts_u_profile')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%process_time_series.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%process_time_series.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'ts_v_profile')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%process_time_series.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%process_time_series.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'ts_gph_profile')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%process_time_series.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%process_time_series.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'ts_th_profile')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%process_time_series.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%process_time_series.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'tign_g')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%ifire(id).EQ.2
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%ifire(id).EQ.2
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'uf')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%ifire(id).EQ.2
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%ifire(id).EQ.2
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'vf')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%ifire(id).EQ.2
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%ifire(id).EQ.2
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'uah')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%ifire(id).EQ.2
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%ifire(id).EQ.2
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'vah')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%ifire(id).EQ.2
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%ifire(id).EQ.2
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'vertstrucc')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%stoch_force_opt(id).EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%stoch_force_opt(id).EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'vertstrucs')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%stoch_force_opt(id).EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%stoch_force_opt(id).EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'vertampt')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%stoch_force_opt(id).EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%stoch_force_opt(id).EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'vertampuv')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%stoch_force_opt(id).EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%stoch_force_opt(id).EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'vertstrucc')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%stoch_force_opt(id).EQ.2
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%stoch_force_opt(id).EQ.2
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'vertstrucs')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%stoch_force_opt(id).EQ.2
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%stoch_force_opt(id).EQ.2
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'vertampt')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%stoch_force_opt(id).EQ.2
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%stoch_force_opt(id).EQ.2
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'vertampuv')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%stoch_force_opt(id).EQ.2
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%stoch_force_opt(id).EQ.2
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'t_grnd2d')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_lake_physics(id).EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_lake_physics(id).EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'t_lake3d')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_lake_physics(id).EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_lake_physics(id).EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'t_soisno3d')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_lake_physics(id).EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_lake_physics(id).EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'watsat3d')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_lake_physics(id).EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_lake_physics(id).EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'tkmg3d')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_lake_physics(id).EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_lake_physics(id).EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'tkdry3d')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_lake_physics(id).EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_lake_physics(id).EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'tksatu3d')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_lake_physics(id).EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_lake_physics(id).EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'u_pl')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%p_lev_diags.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%p_lev_diags.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'v_pl')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%p_lev_diags.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%p_lev_diags.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'t_pl')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%p_lev_diags.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%p_lev_diags.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'td_pl')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%p_lev_diags.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%p_lev_diags.EQ.1
  ENDIF
ENDIF
!ENDOFREGISTRYGENERATEDINCLUDE
