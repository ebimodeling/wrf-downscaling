!STARTOFREGISTRYGENERATEDINCLUDE 'inc/in_use_for_config_ac.inc'
!
! WARNING This file is generated automatically by use_registry
! using the data base in the file named Registry.
! Do not edit.  Your changes to this file will be lost.
!
IF(TRIM(vname).EQ.'cldfra_old_mp')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%mp_physics(id).EQ.11
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%mp_physics(id).EQ.11
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'cldfra_mp')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%mp_physics(id).EQ.11
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%mp_physics(id).EQ.11
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'cldfra_mp_all')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%mp_physics(id).EQ.11
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%mp_physics(id).EQ.11
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'cldfra_conv')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%mp_physics(id).EQ.11
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%mp_physics(id).EQ.11
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'cldfrai')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%mp_physics(id).EQ.11
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%mp_physics(id).EQ.11
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'cldfral')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%mp_physics(id).EQ.11
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%mp_physics(id).EQ.11
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'acswupt')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%ra_lw_physics(id).EQ.3
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%ra_lw_physics(id).EQ.3
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'acswuptc')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%ra_lw_physics(id).EQ.3
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%ra_lw_physics(id).EQ.3
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'acswdnt')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%ra_lw_physics(id).EQ.3
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%ra_lw_physics(id).EQ.3
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'acswdntc')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%ra_lw_physics(id).EQ.3
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%ra_lw_physics(id).EQ.3
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'acswupb')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%ra_lw_physics(id).EQ.3
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%ra_lw_physics(id).EQ.3
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'acswupbc')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%ra_lw_physics(id).EQ.3
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%ra_lw_physics(id).EQ.3
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'acswdnb')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%ra_lw_physics(id).EQ.3
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%ra_lw_physics(id).EQ.3
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'acswdnbc')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%ra_lw_physics(id).EQ.3
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%ra_lw_physics(id).EQ.3
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'aclwupt')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%ra_lw_physics(id).EQ.3
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%ra_lw_physics(id).EQ.3
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'aclwuptc')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%ra_lw_physics(id).EQ.3
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%ra_lw_physics(id).EQ.3
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'aclwdnt')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%ra_lw_physics(id).EQ.3
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%ra_lw_physics(id).EQ.3
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'aclwdntc')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%ra_lw_physics(id).EQ.3
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%ra_lw_physics(id).EQ.3
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'aclwupb')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%ra_lw_physics(id).EQ.3
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%ra_lw_physics(id).EQ.3
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'aclwupbc')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%ra_lw_physics(id).EQ.3
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%ra_lw_physics(id).EQ.3
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'aclwdnb')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%ra_lw_physics(id).EQ.3
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%ra_lw_physics(id).EQ.3
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'aclwdnbc')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%ra_lw_physics(id).EQ.3
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%ra_lw_physics(id).EQ.3
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'aclwupt')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%ra_lw_physics(id).EQ.4
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%ra_lw_physics(id).EQ.4
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'aclwuptc')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%ra_lw_physics(id).EQ.4
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%ra_lw_physics(id).EQ.4
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'aclwdnt')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%ra_lw_physics(id).EQ.4
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%ra_lw_physics(id).EQ.4
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'aclwdntc')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%ra_lw_physics(id).EQ.4
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%ra_lw_physics(id).EQ.4
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'aclwupb')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%ra_lw_physics(id).EQ.4
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%ra_lw_physics(id).EQ.4
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'aclwupbc')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%ra_lw_physics(id).EQ.4
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%ra_lw_physics(id).EQ.4
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'aclwdnb')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%ra_lw_physics(id).EQ.4
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%ra_lw_physics(id).EQ.4
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'aclwdnbc')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%ra_lw_physics(id).EQ.4
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%ra_lw_physics(id).EQ.4
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'acswupt')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%ra_sw_physics(id).EQ.3
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%ra_sw_physics(id).EQ.3
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'acswuptc')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%ra_sw_physics(id).EQ.3
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%ra_sw_physics(id).EQ.3
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'acswdnt')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%ra_sw_physics(id).EQ.3
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%ra_sw_physics(id).EQ.3
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'acswdntc')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%ra_sw_physics(id).EQ.3
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%ra_sw_physics(id).EQ.3
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'acswupb')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%ra_sw_physics(id).EQ.3
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%ra_sw_physics(id).EQ.3
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'acswupbc')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%ra_sw_physics(id).EQ.3
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%ra_sw_physics(id).EQ.3
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'acswdnb')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%ra_sw_physics(id).EQ.3
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%ra_sw_physics(id).EQ.3
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'acswdnbc')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%ra_sw_physics(id).EQ.3
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%ra_sw_physics(id).EQ.3
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'aclwupt')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%ra_sw_physics(id).EQ.3
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%ra_sw_physics(id).EQ.3
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'aclwuptc')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%ra_sw_physics(id).EQ.3
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%ra_sw_physics(id).EQ.3
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'aclwdnt')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%ra_sw_physics(id).EQ.3
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%ra_sw_physics(id).EQ.3
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'aclwdntc')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%ra_sw_physics(id).EQ.3
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%ra_sw_physics(id).EQ.3
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'aclwupb')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%ra_sw_physics(id).EQ.3
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%ra_sw_physics(id).EQ.3
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'aclwupbc')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%ra_sw_physics(id).EQ.3
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%ra_sw_physics(id).EQ.3
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'aclwdnb')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%ra_sw_physics(id).EQ.3
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%ra_sw_physics(id).EQ.3
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'aclwdnbc')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%ra_sw_physics(id).EQ.3
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%ra_sw_physics(id).EQ.3
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'acswupt')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%ra_sw_physics(id).EQ.4
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%ra_sw_physics(id).EQ.4
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'acswuptc')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%ra_sw_physics(id).EQ.4
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%ra_sw_physics(id).EQ.4
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'acswdnt')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%ra_sw_physics(id).EQ.4
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%ra_sw_physics(id).EQ.4
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'acswdntc')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%ra_sw_physics(id).EQ.4
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%ra_sw_physics(id).EQ.4
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'acswupb')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%ra_sw_physics(id).EQ.4
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%ra_sw_physics(id).EQ.4
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'acswupbc')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%ra_sw_physics(id).EQ.4
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%ra_sw_physics(id).EQ.4
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'acswdnb')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%ra_sw_physics(id).EQ.4
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%ra_sw_physics(id).EQ.4
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'acswdnbc')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%ra_sw_physics(id).EQ.4
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%ra_sw_physics(id).EQ.4
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'cov')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_sfclay_physics(id).EQ.5
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_sfclay_physics(id).EQ.5
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'a_u_bep')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_urban_physics(id).EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_urban_physics(id).EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'a_v_bep')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_urban_physics(id).EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_urban_physics(id).EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'a_t_bep')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_urban_physics(id).EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_urban_physics(id).EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'a_q_bep')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_urban_physics(id).EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_urban_physics(id).EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'a_e_bep')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_urban_physics(id).EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_urban_physics(id).EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'b_u_bep')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_urban_physics(id).EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_urban_physics(id).EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'b_v_bep')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_urban_physics(id).EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_urban_physics(id).EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'b_t_bep')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_urban_physics(id).EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_urban_physics(id).EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'b_q_bep')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_urban_physics(id).EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_urban_physics(id).EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'b_e_bep')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_urban_physics(id).EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_urban_physics(id).EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'a_u_bep')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_urban_physics(id).EQ.2
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_urban_physics(id).EQ.2
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'a_v_bep')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_urban_physics(id).EQ.2
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_urban_physics(id).EQ.2
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'a_t_bep')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_urban_physics(id).EQ.2
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_urban_physics(id).EQ.2
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'a_q_bep')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_urban_physics(id).EQ.2
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_urban_physics(id).EQ.2
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'a_e_bep')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_urban_physics(id).EQ.2
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_urban_physics(id).EQ.2
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'b_u_bep')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_urban_physics(id).EQ.2
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_urban_physics(id).EQ.2
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'b_v_bep')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_urban_physics(id).EQ.2
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_urban_physics(id).EQ.2
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'b_t_bep')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_urban_physics(id).EQ.2
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_urban_physics(id).EQ.2
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'b_q_bep')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_urban_physics(id).EQ.2
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_urban_physics(id).EQ.2
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'b_e_bep')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_urban_physics(id).EQ.2
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_urban_physics(id).EQ.2
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'a_u_bep')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_urban_physics(id).EQ.3
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_urban_physics(id).EQ.3
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'a_v_bep')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_urban_physics(id).EQ.3
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_urban_physics(id).EQ.3
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'a_t_bep')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_urban_physics(id).EQ.3
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_urban_physics(id).EQ.3
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'a_q_bep')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_urban_physics(id).EQ.3
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_urban_physics(id).EQ.3
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'a_e_bep')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_urban_physics(id).EQ.3
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_urban_physics(id).EQ.3
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'b_u_bep')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_urban_physics(id).EQ.3
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_urban_physics(id).EQ.3
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'b_v_bep')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_urban_physics(id).EQ.3
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_urban_physics(id).EQ.3
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'b_t_bep')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_urban_physics(id).EQ.3
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_urban_physics(id).EQ.3
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'b_q_bep')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_urban_physics(id).EQ.3
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_urban_physics(id).EQ.3
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'b_e_bep')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_urban_physics(id).EQ.3
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_urban_physics(id).EQ.3
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'cm_ac_urb3d')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_urban_physics(id).EQ.3
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_urban_physics(id).EQ.3
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'canliqxy')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_surface_physics(id).EQ.4
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_surface_physics(id).EQ.4
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'canicexy')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_surface_physics(id).EQ.4
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_surface_physics(id).EQ.4
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'cmxy')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_surface_physics(id).EQ.4
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_surface_physics(id).EQ.4
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'chxy')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_surface_physics(id).EQ.4
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_surface_physics(id).EQ.4
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'alboldxy')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_surface_physics(id).EQ.4
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_surface_physics(id).EQ.4
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'aparxy')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_surface_physics(id).EQ.4
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_surface_physics(id).EQ.4
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'bgapxy')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_surface_physics(id).EQ.4
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_surface_physics(id).EQ.4
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'chvxy')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_surface_physics(id).EQ.4
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_surface_physics(id).EQ.4
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'chbxy')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_surface_physics(id).EQ.4
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_surface_physics(id).EQ.4
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'chleafxy')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_surface_physics(id).EQ.4
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_surface_physics(id).EQ.4
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'chucxy')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_surface_physics(id).EQ.4
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_surface_physics(id).EQ.4
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'chv2xy')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_surface_physics(id).EQ.4
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_surface_physics(id).EQ.4
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'chb2xy')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_surface_physics(id).EQ.4
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_surface_physics(id).EQ.4
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'chstarxy')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_surface_physics(id).EQ.4
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_surface_physics(id).EQ.4
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'areaxy')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_surface_physics(id).EQ.4
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_surface_physics(id).EQ.4
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'albedosubgrid')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_surface_physics(id).EQ.5
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_surface_physics(id).EQ.5
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'bwo1')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_surface_physics(id).EQ.8
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_surface_physics(id).EQ.8
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'bto1')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_surface_physics(id).EQ.8
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_surface_physics(id).EQ.8
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'cto1')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_surface_physics(id).EQ.8
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_surface_physics(id).EQ.8
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'bio1')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_surface_physics(id).EQ.8
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_surface_physics(id).EQ.8
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'blo1')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_surface_physics(id).EQ.8
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_surface_physics(id).EQ.8
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'bwo2')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_surface_physics(id).EQ.8
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_surface_physics(id).EQ.8
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'bto2')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_surface_physics(id).EQ.8
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_surface_physics(id).EQ.8
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'cto2')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_surface_physics(id).EQ.8
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_surface_physics(id).EQ.8
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'bio2')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_surface_physics(id).EQ.8
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_surface_physics(id).EQ.8
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'blo2')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_surface_physics(id).EQ.8
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_surface_physics(id).EQ.8
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'bwo3')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_surface_physics(id).EQ.8
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_surface_physics(id).EQ.8
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'bto3')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_surface_physics(id).EQ.8
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_surface_physics(id).EQ.8
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'cto3')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_surface_physics(id).EQ.8
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_surface_physics(id).EQ.8
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'bio3')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_surface_physics(id).EQ.8
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_surface_physics(id).EQ.8
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'blo3')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_surface_physics(id).EQ.8
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_surface_physics(id).EQ.8
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'bwo4')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_surface_physics(id).EQ.8
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_surface_physics(id).EQ.8
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'bto4')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_surface_physics(id).EQ.8
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_surface_physics(id).EQ.8
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'cto4')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_surface_physics(id).EQ.8
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_surface_physics(id).EQ.8
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'bio4')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_surface_physics(id).EQ.8
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_surface_physics(id).EQ.8
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'blo4')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_surface_physics(id).EQ.8
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_surface_physics(id).EQ.8
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'canwat_mosaic')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_surface_mosaic.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_surface_mosaic.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'albedo_mosaic')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_surface_mosaic.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_surface_mosaic.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'albbck_mosaic')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_surface_mosaic.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_surface_mosaic.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'cov')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%bl_pbl_physics(id).EQ.5
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%bl_pbl_physics(id).EQ.5
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'cov')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%bl_pbl_physics(id).EQ.6
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%bl_pbl_physics(id).EQ.6
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'cf3d_temf')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%bl_pbl_physics(id).EQ.10
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%bl_pbl_physics(id).EQ.10
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'cfm_temf')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%bl_pbl_physics(id).EQ.10
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%bl_pbl_physics(id).EQ.10
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'cugd_qvten')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%cu_physics(id).EQ.5
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%cu_physics(id).EQ.5
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'cugd_tten')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%cu_physics(id).EQ.5
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%cu_physics(id).EQ.5
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'cugd_qvtens')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%cu_physics(id).EQ.5
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%cu_physics(id).EQ.5
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'cugd_ttens')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%cu_physics(id).EQ.5
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%cu_physics(id).EQ.5
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'cugd_qcten')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%cu_physics(id).EQ.5
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%cu_physics(id).EQ.5
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'cugd_qvten')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%cu_physics(id).EQ.3
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%cu_physics(id).EQ.3
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'cugd_tten')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%cu_physics(id).EQ.3
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%cu_physics(id).EQ.3
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'cugd_qvtens')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%cu_physics(id).EQ.3
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%cu_physics(id).EQ.3
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'cugd_ttens')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%cu_physics(id).EQ.3
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%cu_physics(id).EQ.3
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'cugd_qcten')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%cu_physics(id).EQ.3
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%cu_physics(id).EQ.3
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'cmfmc')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%cu_physics(id).EQ.7
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%cu_physics(id).EQ.7
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'cmfmcdzm')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%cu_physics(id).EQ.7
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%cu_physics(id).EQ.7
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'cape')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%cu_physics(id).EQ.7
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%cu_physics(id).EQ.7
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'cmfmc')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%shcu_physics(id).EQ.2
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%shcu_physics(id).EQ.2
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'cmfmc2')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%shcu_physics(id).EQ.2
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%shcu_physics(id).EQ.2
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'cbmf_cu')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%shcu_physics(id).EQ.2
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%shcu_physics(id).EQ.2
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'cin_cu')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%shcu_physics(id).EQ.2
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%shcu_physics(id).EQ.2
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'cinlcl_cu')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%shcu_physics(id).EQ.2
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%shcu_physics(id).EQ.2
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'cbmflimit_cu')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%shcu_physics(id).EQ.2
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%shcu_physics(id).EQ.2
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'cufrc_cu')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%shcu_physics(id).EQ.2
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%shcu_physics(id).EQ.2
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'aquad_cu')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%shcu_physics(id).EQ.2
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%shcu_physics(id).EQ.2
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'bquad_cu')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%shcu_physics(id).EQ.2
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%shcu_physics(id).EQ.2
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'cquad_cu')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%shcu_physics(id).EQ.2
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%shcu_physics(id).EQ.2
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'bogbot_cu')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%shcu_physics(id).EQ.2
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%shcu_physics(id).EQ.2
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'bogtop_cu')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%shcu_physics(id).EQ.2
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%shcu_physics(id).EQ.2
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'cmfsl')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%shcu_physics(id).EQ.2
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%shcu_physics(id).EQ.2
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'cmflq')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%shcu_physics(id).EQ.2
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%shcu_physics(id).EQ.2
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'cldfrash')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%shcu_physics(id).EQ.2
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%shcu_physics(id).EQ.2
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'cush')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%shcu_physics(id).EQ.2
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%shcu_physics(id).EQ.2
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'afwa_precip')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%afwa_ptype_opt(id).EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%afwa_ptype_opt(id).EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'afwa_rain')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%afwa_ptype_opt(id).EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%afwa_ptype_opt(id).EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'afwa_snow')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%afwa_ptype_opt(id).EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%afwa_ptype_opt(id).EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'afwa_ice')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%afwa_ptype_opt(id).EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%afwa_ptype_opt(id).EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'afwa_fzra')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%afwa_ptype_opt(id).EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%afwa_ptype_opt(id).EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'afwa_snowfall')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%afwa_ptype_opt(id).EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%afwa_ptype_opt(id).EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'afwa_cloud')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%afwa_cloud_opt(id).EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%afwa_cloud_opt(id).EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'afwa_vis')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%afwa_vis_opt(id).EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%afwa_vis_opt(id).EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'afwa_vis_dust')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%afwa_vis_opt(id).EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%afwa_vis_opt(id).EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'albedo12m')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%use_wps_input.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%use_wps_input.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'car_urb2d')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%use_wps_input.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%use_wps_input.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'albedo12m')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%use_wps_input.EQ.2
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%use_wps_input.EQ.2
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'albsi')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%seaice_albedo_opt.EQ.2
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%seaice_albedo_opt.EQ.2
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'cg_flashcount')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%lightning_option(id).EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%lightning_option(id).EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'cg_flashrate')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%lightning_option(id).EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%lightning_option(id).EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'cg_flashcount')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%lightning_option(id).EQ.2
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%lightning_option(id).EQ.2
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'cg_flashrate')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%lightning_option(id).EQ.2
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%lightning_option(id).EQ.2
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'cg_flashcount')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%lightning_option(id).EQ.11
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%lightning_option(id).EQ.11
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'cg_flashrate')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%lightning_option(id).EQ.11
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%lightning_option(id).EQ.11
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'canhfx')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%ifire(id).EQ.2
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%ifire(id).EQ.2
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'canqfx')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%ifire(id).EQ.2
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%ifire(id).EQ.2
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'bbb')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%ifire(id).EQ.2
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%ifire(id).EQ.2
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'betafl')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%ifire(id).EQ.2
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%ifire(id).EQ.2
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'avg_fuel_frac')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%ifire(id).EQ.2
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%ifire(id).EQ.2
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'avgflx_rum')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%do_avgflx_em(id).EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%do_avgflx_em(id).EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'avgflx_rvm')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%do_avgflx_em(id).EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%do_avgflx_em(id).EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'avgflx_wwm')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%do_avgflx_em(id).EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%do_avgflx_em(id).EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'avgflx_cfu1')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%do_avgflx_cugd(id).EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%do_avgflx_cugd(id).EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'avgflx_cfd1')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%do_avgflx_cugd(id).EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%do_avgflx_cugd(id).EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'avgflx_dfu1')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%do_avgflx_cugd(id).EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%do_avgflx_cugd(id).EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'avgflx_efu1')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%do_avgflx_cugd(id).EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%do_avgflx_cugd(id).EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'avgflx_dfd1')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%do_avgflx_cugd(id).EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%do_avgflx_cugd(id).EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'avgflx_efd1')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%do_avgflx_cugd(id).EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%do_avgflx_cugd(id).EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'cfu1')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%do_avgflx_cugd(id).EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%do_avgflx_cugd(id).EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'cfd1')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%do_avgflx_cugd(id).EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%do_avgflx_cugd(id).EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'csol3d')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_lake_physics(id).EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_lake_physics(id).EQ.1
  ENDIF
ENDIF
!ENDOFREGISTRYGENERATEDINCLUDE
