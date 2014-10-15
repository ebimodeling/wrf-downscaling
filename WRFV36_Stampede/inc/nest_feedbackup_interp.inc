!STARTOFREGISTRYGENERATEDINCLUDE 'inc/nest_feedbackup_interp.inc'
!
! WARNING This file is generated automatically by use_registry
! using the data base in the file named Registry.
! Do not edit.  Your changes to this file will be lost.
!
IF ( SIZE( grid%xlat, 1 ) * SIZE( grid%xlat, 2 ) .GT. 1 ) THEN 
CALL copy_fcnm (  &         
                  grid%xlat,   &       ! CD field
                 cids, cide, 1, 1, cjds, cjde,   &         ! CD dims
                 cims, cime, 1, 1, cjms, cjme,   &         ! CD dims
                 cips, cipe, 1, 1, cjps, cjpe,   &         ! CD dims
                  ngrid%xlat,  &   ! ND field
                 nids, nide, 1, 1, njds, njde,   &         ! ND dims
                 nims, nime, 1, 1, njms, njme,   &         ! ND dims
                 nips, nipe, 1, 1, njps, njpe,   &         ! ND dims
                  config_flags%shw, ngrid%imask_nostag,         &         ! stencil half width
                  .FALSE., .FALSE.,                                                &         ! xstag, ystag
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%xlong, 1 ) * SIZE( grid%xlong, 2 ) .GT. 1 ) THEN 
CALL copy_fcnm (  &         
                  grid%xlong,   &       ! CD field
                 cids, cide, 1, 1, cjds, cjde,   &         ! CD dims
                 cims, cime, 1, 1, cjms, cjme,   &         ! CD dims
                 cips, cipe, 1, 1, cjps, cjpe,   &         ! CD dims
                  ngrid%xlong,  &   ! ND field
                 nids, nide, 1, 1, njds, njde,   &         ! ND dims
                 nims, nime, 1, 1, njms, njme,   &         ! ND dims
                 nips, nipe, 1, 1, njps, njpe,   &         ! ND dims
                  config_flags%shw, ngrid%imask_nostag,         &         ! stencil half width
                  .FALSE., .FALSE.,                                                &         ! xstag, ystag
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%lu_index, 1 ) * SIZE( grid%lu_index, 2 ) .GT. 1 ) THEN 
CALL copy_fcnm (  &         
                  grid%lu_index,   &       ! CD field
                 cids, cide, 1, 1, cjds, cjde,   &         ! CD dims
                 cims, cime, 1, 1, cjms, cjme,   &         ! CD dims
                 cips, cipe, 1, 1, cjps, cjpe,   &         ! CD dims
                  ngrid%lu_index,  &   ! ND field
                 nids, nide, 1, 1, njds, njde,   &         ! ND dims
                 nims, nime, 1, 1, njms, njme,   &         ! ND dims
                 nips, nipe, 1, 1, njps, njpe,   &         ! ND dims
                  config_flags%shw, ngrid%imask_nostag,         &         ! stencil half width
                  .FALSE., .FALSE.,                                                &         ! xstag, ystag
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%u_2, 1 ) * SIZE( grid%u_2, 3 ) .GT. 1 ) THEN 
CALL copy_fcn (  &         
                  grid%u_2,   &       ! CD field
                 cids, cide, ckds, ckde, cjds, cjde,   &         ! CD dims
                 cims, cime, ckms, ckme, cjms, cjme,   &         ! CD dims
                 cips, cipe, ckps, MIN( (ckde-1), ckpe ), cjps, cjpe,   &         ! CD dims
                  ngrid%u_2,  &   ! ND field
                 nids, nide, nkds, nkde, njds, njde,   &         ! ND dims
                 nims, nime, nkms, nkme, njms, njme,   &         ! ND dims
                 nips, nipe, nkps, MIN( (nkde-1), nkpe ), njps, njpe,   &         ! ND dims
                  config_flags%shw, ngrid%imask_xstag,         &         ! stencil half width
                  .TRUE., .FALSE.,                                                &         ! xstag, ystag
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%v_2, 1 ) * SIZE( grid%v_2, 3 ) .GT. 1 ) THEN 
CALL copy_fcn (  &         
                  grid%v_2,   &       ! CD field
                 cids, cide, ckds, ckde, cjds, cjde,   &         ! CD dims
                 cims, cime, ckms, ckme, cjms, cjme,   &         ! CD dims
                 cips, cipe, ckps, MIN( (ckde-1), ckpe ), cjps, cjpe,   &         ! CD dims
                  ngrid%v_2,  &   ! ND field
                 nids, nide, nkds, nkde, njds, njde,   &         ! ND dims
                 nims, nime, nkms, nkme, njms, njme,   &         ! ND dims
                 nips, nipe, nkps, MIN( (nkde-1), nkpe ), njps, njpe,   &         ! ND dims
                  config_flags%shw, ngrid%imask_ystag,         &         ! stencil half width
                  .FALSE., .TRUE.,                                                &         ! xstag, ystag
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%w_2, 1 ) * SIZE( grid%w_2, 3 ) .GT. 1 ) THEN 
CALL copy_fcn (  &         
                  grid%w_2,   &       ! CD field
                 cids, cide, ckds, ckde, cjds, cjde,   &         ! CD dims
                 cims, cime, ckms, ckme, cjms, cjme,   &         ! CD dims
                 cips, cipe, ckps, MIN( ckde, ckpe ), cjps, cjpe,   &         ! CD dims
                  ngrid%w_2,  &   ! ND field
                 nids, nide, nkds, nkde, njds, njde,   &         ! ND dims
                 nims, nime, nkms, nkme, njms, njme,   &         ! ND dims
                 nips, nipe, nkps, MIN( nkde, nkpe ), njps, njpe,   &         ! ND dims
                  config_flags%shw, ngrid%imask_nostag,         &         ! stencil half width
                  .FALSE., .FALSE.,                                                &         ! xstag, ystag
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%ph_2, 1 ) * SIZE( grid%ph_2, 3 ) .GT. 1 ) THEN 
CALL copy_fcn (  &         
                  grid%ph_2,   &       ! CD field
                 cids, cide, ckds, ckde, cjds, cjde,   &         ! CD dims
                 cims, cime, ckms, ckme, cjms, cjme,   &         ! CD dims
                 cips, cipe, ckps, MIN( ckde, ckpe ), cjps, cjpe,   &         ! CD dims
                  ngrid%ph_2,  &   ! ND field
                 nids, nide, nkds, nkde, njds, njde,   &         ! ND dims
                 nims, nime, nkms, nkme, njms, njme,   &         ! ND dims
                 nips, nipe, nkps, MIN( nkde, nkpe ), njps, njpe,   &         ! ND dims
                  config_flags%shw, ngrid%imask_nostag,         &         ! stencil half width
                  .FALSE., .FALSE.,                                                &         ! xstag, ystag
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%phb, 1 ) * SIZE( grid%phb, 3 ) .GT. 1 ) THEN 
CALL copy_fcn (  &         
                  grid%phb,   &       ! CD field
                 cids, cide, ckds, ckde, cjds, cjde,   &         ! CD dims
                 cims, cime, ckms, ckme, cjms, cjme,   &         ! CD dims
                 cips, cipe, ckps, MIN( ckde, ckpe ), cjps, cjpe,   &         ! CD dims
                  ngrid%phb,  &   ! ND field
                 nids, nide, nkds, nkde, njds, njde,   &         ! ND dims
                 nims, nime, nkms, nkme, njms, njme,   &         ! ND dims
                 nips, nipe, nkps, MIN( nkde, nkpe ), njps, njpe,   &         ! ND dims
                  config_flags%shw, ngrid%imask_nostag,         &         ! stencil half width
                  .FALSE., .FALSE.,                                                &         ! xstag, ystag
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%t_2, 1 ) * SIZE( grid%t_2, 3 ) .GT. 1 ) THEN 
CALL copy_fcn (  &         
                  grid%t_2,   &       ! CD field
                 cids, cide, ckds, ckde, cjds, cjde,   &         ! CD dims
                 cims, cime, ckms, ckme, cjms, cjme,   &         ! CD dims
                 cips, cipe, ckps, MIN( (ckde-1), ckpe ), cjps, cjpe,   &         ! CD dims
                  ngrid%t_2,  &   ! ND field
                 nids, nide, nkds, nkde, njds, njde,   &         ! ND dims
                 nims, nime, nkms, nkme, njms, njme,   &         ! ND dims
                 nips, nipe, nkps, MIN( (nkde-1), nkpe ), njps, njpe,   &         ! ND dims
                  config_flags%shw, ngrid%imask_nostag,         &         ! stencil half width
                  .FALSE., .FALSE.,                                                &         ! xstag, ystag
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%mu_2, 1 ) * SIZE( grid%mu_2, 2 ) .GT. 1 ) THEN 
CALL copy_fcn (  &         
                  grid%mu_2,   &       ! CD field
                 cids, cide, 1, 1, cjds, cjde,   &         ! CD dims
                 cims, cime, 1, 1, cjms, cjme,   &         ! CD dims
                 cips, cipe, 1, 1, cjps, cjpe,   &         ! CD dims
                  ngrid%mu_2,  &   ! ND field
                 nids, nide, 1, 1, njds, njde,   &         ! ND dims
                 nims, nime, 1, 1, njms, njme,   &         ! ND dims
                 nips, nipe, 1, 1, njps, njpe,   &         ! ND dims
                  config_flags%shw, ngrid%imask_nostag,         &         ! stencil half width
                  .FALSE., .FALSE.,                                                &         ! xstag, ystag
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%mub, 1 ) * SIZE( grid%mub, 2 ) .GT. 1 ) THEN 
CALL copy_fcn (  &         
                  grid%mub,   &       ! CD field
                 cids, cide, 1, 1, cjds, cjde,   &         ! CD dims
                 cims, cime, 1, 1, cjms, cjme,   &         ! CD dims
                 cips, cipe, 1, 1, cjps, cjpe,   &         ! CD dims
                  ngrid%mub,  &   ! ND field
                 nids, nide, 1, 1, njds, njde,   &         ! ND dims
                 nims, nime, 1, 1, njms, njme,   &         ! ND dims
                 nips, nipe, 1, 1, njps, njpe,   &         ! ND dims
                  config_flags%shw, ngrid%imask_nostag,         &         ! stencil half width
                  .FALSE., .FALSE.,                                                &         ! xstag, ystag
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%nest_pos, 1 ) * SIZE( grid%nest_pos, 2 ) .GT. 1 ) THEN 
CALL mark_domain (  &         
                  grid%nest_pos,   &       ! CD field
                 cids, cide, 1, 1, cjds, cjde,   &         ! CD dims
                 cims, cime, 1, 1, cjms, cjme,   &         ! CD dims
                 cips, cipe, 1, 1, cjps, cjpe,   &         ! CD dims
                  ngrid%nest_pos,  &   ! ND field
                 nids, nide, 1, 1, njds, njde,   &         ! ND dims
                 nims, nime, 1, 1, njms, njme,   &         ! ND dims
                 nips, nipe, 1, 1, njps, njpe,   &         ! ND dims
                  config_flags%shw, ngrid%imask_nostag,         &         ! stencil half width
                  .FALSE., .FALSE.,                                                &         ! xstag, ystag
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%nest_mask, 1 ) * SIZE( grid%nest_mask, 2 ) .GT. 1 ) THEN 
CALL mark_domain (  &         
                  grid%nest_mask,   &       ! CD field
                 cids, cide, 1, 1, cjds, cjde,   &         ! CD dims
                 cims, cime, 1, 1, cjms, cjme,   &         ! CD dims
                 cips, cipe, 1, 1, cjps, cjpe,   &         ! CD dims
                  ngrid%nest_mask,  &   ! ND field
                 nids, nide, 1, 1, njds, njde,   &         ! ND dims
                 nims, nime, 1, 1, njms, njme,   &         ! ND dims
                 nips, nipe, 1, 1, njps, njpe,   &         ! ND dims
                  config_flags%shw, ngrid%imask_nostag,         &         ! stencil half width
                  .FALSE., .FALSE.,                                                &         ! xstag, ystag
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%alb, 1 ) * SIZE( grid%alb, 3 ) .GT. 1 ) THEN 
CALL copy_fcn (  &         
                  grid%alb,   &       ! CD field
                 cids, cide, ckds, ckde, cjds, cjde,   &         ! CD dims
                 cims, cime, ckms, ckme, cjms, cjme,   &         ! CD dims
                 cips, cipe, ckps, MIN( (ckde-1), ckpe ), cjps, cjpe,   &         ! CD dims
                  ngrid%alb,  &   ! ND field
                 nids, nide, nkds, nkde, njds, njde,   &         ! ND dims
                 nims, nime, nkms, nkme, njms, njme,   &         ! ND dims
                 nips, nipe, nkps, MIN( (nkde-1), nkpe ), njps, njpe,   &         ! ND dims
                  config_flags%shw, ngrid%imask_nostag,         &         ! stencil half width
                  .FALSE., .FALSE.,                                                &         ! xstag, ystag
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%pb, 1 ) * SIZE( grid%pb, 3 ) .GT. 1 ) THEN 
CALL copy_fcn (  &         
                  grid%pb,   &       ! CD field
                 cids, cide, ckds, ckde, cjds, cjde,   &         ! CD dims
                 cims, cime, ckms, ckme, cjms, cjme,   &         ! CD dims
                 cips, cipe, ckps, MIN( (ckde-1), ckpe ), cjps, cjpe,   &         ! CD dims
                  ngrid%pb,  &   ! ND field
                 nids, nide, nkds, nkde, njds, njde,   &         ! ND dims
                 nims, nime, nkms, nkme, njms, njme,   &         ! ND dims
                 nips, nipe, nkps, MIN( (nkde-1), nkpe ), njps, njpe,   &         ! ND dims
                  config_flags%shw, ngrid%imask_nostag,         &         ! stencil half width
                  .FALSE., .FALSE.,                                                &         ! xstag, ystag
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%q2, 1 ) * SIZE( grid%q2, 2 ) .GT. 1 ) THEN 
CALL copy_fcn (  &         
                  grid%q2,   &       ! CD field
                 cids, cide, 1, 1, cjds, cjde,   &         ! CD dims
                 cims, cime, 1, 1, cjms, cjme,   &         ! CD dims
                 cips, cipe, 1, 1, cjps, cjpe,   &         ! CD dims
                  ngrid%q2,  &   ! ND field
                 nids, nide, 1, 1, njds, njde,   &         ! ND dims
                 nims, nime, 1, 1, njms, njme,   &         ! ND dims
                 nips, nipe, 1, 1, njps, njpe,   &         ! ND dims
                  config_flags%shw, ngrid%imask_nostag,         &         ! stencil half width
                  .FALSE., .FALSE.,                                                &         ! xstag, ystag
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%t2, 1 ) * SIZE( grid%t2, 2 ) .GT. 1 ) THEN 
CALL copy_fcn (  &         
                  grid%t2,   &       ! CD field
                 cids, cide, 1, 1, cjds, cjde,   &         ! CD dims
                 cims, cime, 1, 1, cjms, cjme,   &         ! CD dims
                 cips, cipe, 1, 1, cjps, cjpe,   &         ! CD dims
                  ngrid%t2,  &   ! ND field
                 nids, nide, 1, 1, njds, njde,   &         ! ND dims
                 nims, nime, 1, 1, njms, njme,   &         ! ND dims
                 nips, nipe, 1, 1, njps, njpe,   &         ! ND dims
                  config_flags%shw, ngrid%imask_nostag,         &         ! stencil half width
                  .FALSE., .FALSE.,                                                &         ! xstag, ystag
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%th2, 1 ) * SIZE( grid%th2, 2 ) .GT. 1 ) THEN 
CALL copy_fcn (  &         
                  grid%th2,   &       ! CD field
                 cids, cide, 1, 1, cjds, cjde,   &         ! CD dims
                 cims, cime, 1, 1, cjms, cjme,   &         ! CD dims
                 cips, cipe, 1, 1, cjps, cjpe,   &         ! CD dims
                  ngrid%th2,  &   ! ND field
                 nids, nide, 1, 1, njds, njde,   &         ! ND dims
                 nims, nime, 1, 1, njms, njme,   &         ! ND dims
                 nips, nipe, 1, 1, njps, njpe,   &         ! ND dims
                  config_flags%shw, ngrid%imask_nostag,         &         ! stencil half width
                  .FALSE., .FALSE.,                                                &         ! xstag, ystag
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%psfc, 1 ) * SIZE( grid%psfc, 2 ) .GT. 1 ) THEN 
CALL copy_fcn (  &         
                  grid%psfc,   &       ! CD field
                 cids, cide, 1, 1, cjds, cjde,   &         ! CD dims
                 cims, cime, 1, 1, cjms, cjme,   &         ! CD dims
                 cips, cipe, 1, 1, cjps, cjpe,   &         ! CD dims
                  ngrid%psfc,  &   ! ND field
                 nids, nide, 1, 1, njds, njde,   &         ! ND dims
                 nims, nime, 1, 1, njms, njme,   &         ! ND dims
                 nips, nipe, 1, 1, njps, njpe,   &         ! ND dims
                  config_flags%shw, ngrid%imask_nostag,         &         ! stencil half width
                  .FALSE., .FALSE.,                                                &         ! xstag, ystag
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%u10, 1 ) * SIZE( grid%u10, 2 ) .GT. 1 ) THEN 
CALL copy_fcn (  &         
                  grid%u10,   &       ! CD field
                 cids, cide, 1, 1, cjds, cjde,   &         ! CD dims
                 cims, cime, 1, 1, cjms, cjme,   &         ! CD dims
                 cips, cipe, 1, 1, cjps, cjpe,   &         ! CD dims
                  ngrid%u10,  &   ! ND field
                 nids, nide, 1, 1, njds, njde,   &         ! ND dims
                 nims, nime, 1, 1, njms, njme,   &         ! ND dims
                 nips, nipe, 1, 1, njps, njpe,   &         ! ND dims
                  config_flags%shw, ngrid%imask_nostag,         &         ! stencil half width
                  .FALSE., .FALSE.,                                                &         ! xstag, ystag
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%v10, 1 ) * SIZE( grid%v10, 2 ) .GT. 1 ) THEN 
CALL copy_fcn (  &         
                  grid%v10,   &       ! CD field
                 cids, cide, 1, 1, cjds, cjde,   &         ! CD dims
                 cims, cime, 1, 1, cjms, cjme,   &         ! CD dims
                 cips, cipe, 1, 1, cjps, cjpe,   &         ! CD dims
                  ngrid%v10,  &   ! ND field
                 nids, nide, 1, 1, njds, njde,   &         ! ND dims
                 nims, nime, 1, 1, njms, njme,   &         ! ND dims
                 nips, nipe, 1, 1, njps, njpe,   &         ! ND dims
                  config_flags%shw, ngrid%imask_nostag,         &         ! stencil half width
                  .FALSE., .FALSE.,                                                &         ! xstag, ystag
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
DO itrace = PARAM_FIRST_SCALAR, num_moist
IF ( SIZE( moist, 1 ) * SIZE( moist, 3 ) .GT. 1 ) THEN 
CALL copy_fcn (  &         
                  moist(grid%sm31,grid%sm32,grid%sm33,itrace),   &       ! CD field
                 cids, cide, ckds, ckde, cjds, cjde,   &         ! CD dims
                 cims, cime, ckms, ckme, cjms, cjme,   &         ! CD dims
                 cips, cipe, ckps, MIN( (ckde-1), ckpe ), cjps, cjpe,   &         ! CD dims
                  ngrid%moist(ngrid%sm31,ngrid%sm32,ngrid%sm33,itrace),  &   ! ND field
                 nids, nide, nkds, nkde, njds, njde,   &         ! ND dims
                 nims, nime, nkms, nkme, njms, njme,   &         ! ND dims
                 nips, nipe, nkps, MIN( (nkde-1), nkpe ), njps, njpe,   &         ! ND dims
                  config_flags%shw, ngrid%imask_nostag,         &         ! stencil half width
                  .FALSE., .FALSE.,                                                &         ! xstag, ystag
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
ENDDO
DO itrace = PARAM_FIRST_SCALAR, num_dfi_moist
IF ( SIZE( dfi_moist, 1 ) * SIZE( dfi_moist, 3 ) .GT. 1 ) THEN 
CALL copy_fcn (  &         
                  dfi_moist(grid%sm31,grid%sm32,grid%sm33,itrace),   &       ! CD field
                 cids, cide, ckds, ckde, cjds, cjde,   &         ! CD dims
                 cims, cime, ckms, ckme, cjms, cjme,   &         ! CD dims
                 cips, cipe, ckps, MIN( (ckde-1), ckpe ), cjps, cjpe,   &         ! CD dims
                  ngrid%dfi_moist(ngrid%sm31,ngrid%sm32,ngrid%sm33,itrace),  &   ! ND field
                 nids, nide, nkds, nkde, njds, njde,   &         ! ND dims
                 nims, nime, nkms, nkme, njms, njme,   &         ! ND dims
                 nips, nipe, nkps, MIN( (nkde-1), nkpe ), njps, njpe,   &         ! ND dims
                  config_flags%shw, ngrid%imask_nostag,         &         ! stencil half width
                  .FALSE., .FALSE.,                                                &         ! xstag, ystag
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
ENDDO
IF ( SIZE( grid%qnwfa2d, 1 ) * SIZE( grid%qnwfa2d, 2 ) .GT. 1 ) THEN 
CALL copy_fcn (  &         
                  grid%qnwfa2d,   &       ! CD field
                 cids, cide, 1, 1, cjds, cjde,   &         ! CD dims
                 cims, cime, 1, 1, cjms, cjme,   &         ! CD dims
                 cips, cipe, 1, 1, cjps, cjpe,   &         ! CD dims
                  ngrid%qnwfa2d,  &   ! ND field
                 nids, nide, 1, 1, njds, njde,   &         ! ND dims
                 nims, nime, 1, 1, njms, njme,   &         ! ND dims
                 nips, nipe, 1, 1, njps, njpe,   &         ! ND dims
                  config_flags%shw, ngrid%imask_nostag,         &         ! stencil half width
                  .FALSE., .FALSE.,                                                &         ! xstag, ystag
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
DO itrace = PARAM_FIRST_SCALAR, num_scalar
IF ( SIZE( scalar, 1 ) * SIZE( scalar, 3 ) .GT. 1 ) THEN 
CALL copy_fcn (  &         
                  scalar(grid%sm31,grid%sm32,grid%sm33,itrace),   &       ! CD field
                 cids, cide, ckds, ckde, cjds, cjde,   &         ! CD dims
                 cims, cime, ckms, ckme, cjms, cjme,   &         ! CD dims
                 cips, cipe, ckps, MIN( (ckde-1), ckpe ), cjps, cjpe,   &         ! CD dims
                  ngrid%scalar(ngrid%sm31,ngrid%sm32,ngrid%sm33,itrace),  &   ! ND field
                 nids, nide, nkds, nkde, njds, njde,   &         ! ND dims
                 nims, nime, nkms, nkme, njms, njme,   &         ! ND dims
                 nips, nipe, nkps, MIN( (nkde-1), nkpe ), njps, njpe,   &         ! ND dims
                  config_flags%shw, ngrid%imask_nostag,         &         ! stencil half width
                  .FALSE., .FALSE.,                                                &         ! xstag, ystag
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
ENDDO
DO itrace = PARAM_FIRST_SCALAR, num_dfi_scalar
IF ( SIZE( dfi_scalar, 1 ) * SIZE( dfi_scalar, 3 ) .GT. 1 ) THEN 
CALL copy_fcn (  &         
                  dfi_scalar(grid%sm31,grid%sm32,grid%sm33,itrace),   &       ! CD field
                 cids, cide, ckds, ckde, cjds, cjde,   &         ! CD dims
                 cims, cime, ckms, ckme, cjms, cjme,   &         ! CD dims
                 cips, cipe, ckps, MIN( (ckde-1), ckpe ), cjps, cjpe,   &         ! CD dims
                  ngrid%dfi_scalar(ngrid%sm31,ngrid%sm32,ngrid%sm33,itrace),  &   ! ND field
                 nids, nide, nkds, nkde, njds, njde,   &         ! ND dims
                 nims, nime, nkms, nkme, njms, njme,   &         ! ND dims
                 nips, nipe, nkps, MIN( (nkde-1), nkpe ), njps, njpe,   &         ! ND dims
                  config_flags%shw, ngrid%imask_nostag,         &         ! stencil half width
                  .FALSE., .FALSE.,                                                &         ! xstag, ystag
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
ENDDO
IF ( SIZE( grid%toposlpx, 1 ) * SIZE( grid%toposlpx, 2 ) .GT. 1 ) THEN 
CALL copy_fcn (  &         
                  grid%toposlpx,   &       ! CD field
                 cids, cide, 1, 1, cjds, cjde,   &         ! CD dims
                 cims, cime, 1, 1, cjms, cjme,   &         ! CD dims
                 cips, cipe, 1, 1, cjps, cjpe,   &         ! CD dims
                  ngrid%toposlpx,  &   ! ND field
                 nids, nide, 1, 1, njds, njde,   &         ! ND dims
                 nims, nime, 1, 1, njms, njme,   &         ! ND dims
                 nips, nipe, 1, 1, njps, njpe,   &         ! ND dims
                  config_flags%shw, ngrid%imask_nostag,         &         ! stencil half width
                  .FALSE., .FALSE.,                                                &         ! xstag, ystag
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%toposlpy, 1 ) * SIZE( grid%toposlpy, 2 ) .GT. 1 ) THEN 
CALL copy_fcn (  &         
                  grid%toposlpy,   &       ! CD field
                 cids, cide, 1, 1, cjds, cjde,   &         ! CD dims
                 cims, cime, 1, 1, cjms, cjme,   &         ! CD dims
                 cips, cipe, 1, 1, cjps, cjpe,   &         ! CD dims
                  ngrid%toposlpy,  &   ! ND field
                 nids, nide, 1, 1, njds, njde,   &         ! ND dims
                 nims, nime, 1, 1, njms, njme,   &         ! ND dims
                 nips, nipe, 1, 1, njps, njpe,   &         ! ND dims
                  config_flags%shw, ngrid%imask_nostag,         &         ! stencil half width
                  .FALSE., .FALSE.,                                                &         ! xstag, ystag
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%slope, 1 ) * SIZE( grid%slope, 2 ) .GT. 1 ) THEN 
CALL copy_fcn (  &         
                  grid%slope,   &       ! CD field
                 cids, cide, 1, 1, cjds, cjde,   &         ! CD dims
                 cims, cime, 1, 1, cjms, cjme,   &         ! CD dims
                 cips, cipe, 1, 1, cjps, cjpe,   &         ! CD dims
                  ngrid%slope,  &   ! ND field
                 nids, nide, 1, 1, njds, njde,   &         ! ND dims
                 nims, nime, 1, 1, njms, njme,   &         ! ND dims
                 nips, nipe, 1, 1, njps, njpe,   &         ! ND dims
                  config_flags%shw, ngrid%imask_nostag,         &         ! stencil half width
                  .FALSE., .FALSE.,                                                &         ! xstag, ystag
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%slp_azi, 1 ) * SIZE( grid%slp_azi, 2 ) .GT. 1 ) THEN 
CALL copy_fcn (  &         
                  grid%slp_azi,   &       ! CD field
                 cids, cide, 1, 1, cjds, cjde,   &         ! CD dims
                 cims, cime, 1, 1, cjms, cjme,   &         ! CD dims
                 cips, cipe, 1, 1, cjps, cjpe,   &         ! CD dims
                  ngrid%slp_azi,  &   ! ND field
                 nids, nide, 1, 1, njds, njde,   &         ! ND dims
                 nims, nime, 1, 1, njms, njme,   &         ! ND dims
                 nips, nipe, 1, 1, njps, njpe,   &         ! ND dims
                  config_flags%shw, ngrid%imask_nostag,         &         ! stencil half width
                  .FALSE., .FALSE.,                                                &         ! xstag, ystag
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%shdmax, 1 ) * SIZE( grid%shdmax, 2 ) .GT. 1 ) THEN 
CALL copy_fcnm (  &         
                  grid%shdmax,   &       ! CD field
                 cids, cide, 1, 1, cjds, cjde,   &         ! CD dims
                 cims, cime, 1, 1, cjms, cjme,   &         ! CD dims
                 cips, cipe, 1, 1, cjps, cjpe,   &         ! CD dims
                  ngrid%shdmax,  &   ! ND field
                 nids, nide, 1, 1, njds, njde,   &         ! ND dims
                 nims, nime, 1, 1, njms, njme,   &         ! ND dims
                 nips, nipe, 1, 1, njps, njpe,   &         ! ND dims
                  config_flags%shw, ngrid%imask_nostag,         &         ! stencil half width
                  .FALSE., .FALSE.,                                                &         ! xstag, ystag
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%shdmin, 1 ) * SIZE( grid%shdmin, 2 ) .GT. 1 ) THEN 
CALL copy_fcnm (  &         
                  grid%shdmin,   &       ! CD field
                 cids, cide, 1, 1, cjds, cjde,   &         ! CD dims
                 cims, cime, 1, 1, cjms, cjme,   &         ! CD dims
                 cips, cipe, 1, 1, cjps, cjpe,   &         ! CD dims
                  ngrid%shdmin,  &   ! ND field
                 nids, nide, 1, 1, njds, njde,   &         ! ND dims
                 nims, nime, 1, 1, njms, njme,   &         ! ND dims
                 nips, nipe, 1, 1, njps, njpe,   &         ! ND dims
                  config_flags%shw, ngrid%imask_nostag,         &         ! stencil half width
                  .FALSE., .FALSE.,                                                &         ! xstag, ystag
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%tslb, 1 ) * SIZE( grid%tslb, 3 ) .GT. 1 ) THEN 
CALL copy_fcnm (  &         
                  grid%tslb,   &       ! CD field
                 cids, cide, 1, config_flags%num_soil_layers, cjds, cjde,   &         ! CD dims
                 cims, cime, 1, config_flags%num_soil_layers, cjms, cjme,   &         ! CD dims
                 cips, cipe, 1, config_flags%num_soil_layers, cjps, cjpe,   &         ! CD dims
                  ngrid%tslb,  &   ! ND field
                 nids, nide, 1, config_flags%num_soil_layers, njds, njde,   &         ! ND dims
                 nims, nime, 1, config_flags%num_soil_layers, njms, njme,   &         ! ND dims
                 nips, nipe, 1, config_flags%num_soil_layers, njps, njpe,   &         ! ND dims
                  config_flags%shw, ngrid%imask_nostag,         &         ! stencil half width
                  .FALSE., .FALSE.,                                                &         ! xstag, ystag
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%urb_param, 1 ) * SIZE( grid%urb_param, 3 ) .GT. 1 ) THEN 
CALL copy_fcni (  &         
                  grid%urb_param,   &       ! CD field
                 cids, cide, 1, 132, cjds, cjde,   &         ! CD dims
                 cims, cime, 1, 132, cjms, cjme,   &         ! CD dims
                 cips, cipe, 1, 132, cjps, cjpe,   &         ! CD dims
                  ngrid%urb_param,  &   ! ND field
                 nids, nide, 1, 132, njds, njde,   &         ! ND dims
                 nims, nime, 1, 132, njms, njme,   &         ! ND dims
                 nips, nipe, 1, 132, njps, njpe,   &         ! ND dims
                  config_flags%shw, ngrid%imask_nostag,         &         ! stencil half width
                  .FALSE., .FALSE.,                                                &         ! xstag, ystag
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%smois, 1 ) * SIZE( grid%smois, 3 ) .GT. 1 ) THEN 
CALL copy_fcnm (  &         
                  grid%smois,   &       ! CD field
                 cids, cide, 1, config_flags%num_soil_layers, cjds, cjde,   &         ! CD dims
                 cims, cime, 1, config_flags%num_soil_layers, cjms, cjme,   &         ! CD dims
                 cips, cipe, 1, config_flags%num_soil_layers, cjps, cjpe,   &         ! CD dims
                  ngrid%smois,  &   ! ND field
                 nids, nide, 1, config_flags%num_soil_layers, njds, njde,   &         ! ND dims
                 nims, nime, 1, config_flags%num_soil_layers, njms, njme,   &         ! ND dims
                 nips, nipe, 1, config_flags%num_soil_layers, njps, njpe,   &         ! ND dims
                  config_flags%shw, ngrid%imask_nostag,         &         ! stencil half width
                  .FALSE., .FALSE.,                                                &         ! xstag, ystag
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%sh2o, 1 ) * SIZE( grid%sh2o, 3 ) .GT. 1 ) THEN 
CALL copy_fcnm (  &         
                  grid%sh2o,   &       ! CD field
                 cids, cide, 1, config_flags%num_soil_layers, cjds, cjde,   &         ! CD dims
                 cims, cime, 1, config_flags%num_soil_layers, cjms, cjme,   &         ! CD dims
                 cips, cipe, 1, config_flags%num_soil_layers, cjps, cjpe,   &         ! CD dims
                  ngrid%sh2o,  &   ! ND field
                 nids, nide, 1, config_flags%num_soil_layers, njds, njde,   &         ! ND dims
                 nims, nime, 1, config_flags%num_soil_layers, njms, njme,   &         ! ND dims
                 nips, nipe, 1, config_flags%num_soil_layers, njps, njpe,   &         ! ND dims
                  config_flags%shw, ngrid%imask_nostag,         &         ! stencil half width
                  .FALSE., .FALSE.,                                                &         ! xstag, ystag
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%smcrel, 1 ) * SIZE( grid%smcrel, 3 ) .GT. 1 ) THEN 
CALL copy_fcnm (  &         
                  grid%smcrel,   &       ! CD field
                 cids, cide, 1, config_flags%num_soil_layers, cjds, cjde,   &         ! CD dims
                 cims, cime, 1, config_flags%num_soil_layers, cjms, cjme,   &         ! CD dims
                 cips, cipe, 1, config_flags%num_soil_layers, cjps, cjpe,   &         ! CD dims
                  ngrid%smcrel,  &   ! ND field
                 nids, nide, 1, config_flags%num_soil_layers, njds, njde,   &         ! ND dims
                 nims, nime, 1, config_flags%num_soil_layers, njms, njme,   &         ! ND dims
                 nips, nipe, 1, config_flags%num_soil_layers, njps, njpe,   &         ! ND dims
                  config_flags%shw, ngrid%imask_nostag,         &         ! stencil half width
                  .FALSE., .FALSE.,                                                &         ! xstag, ystag
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%xice, 1 ) * SIZE( grid%xice, 2 ) .GT. 1 ) THEN 
CALL copy_fcnm (  &         
                  grid%xice,   &       ! CD field
                 cids, cide, 1, 1, cjds, cjde,   &         ! CD dims
                 cims, cime, 1, 1, cjms, cjme,   &         ! CD dims
                 cips, cipe, 1, 1, cjps, cjpe,   &         ! CD dims
                  ngrid%xice,  &   ! ND field
                 nids, nide, 1, 1, njds, njde,   &         ! ND dims
                 nims, nime, 1, 1, njms, njme,   &         ! ND dims
                 nips, nipe, 1, 1, njps, njpe,   &         ! ND dims
                  config_flags%shw, ngrid%imask_nostag,         &         ! stencil half width
                  .FALSE., .FALSE.,                                                &         ! xstag, ystag
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%icedepth, 1 ) * SIZE( grid%icedepth, 2 ) .GT. 1 ) THEN 
CALL copy_fcnm (  &         
                  grid%icedepth,   &       ! CD field
                 cids, cide, 1, 1, cjds, cjde,   &         ! CD dims
                 cims, cime, 1, 1, cjms, cjme,   &         ! CD dims
                 cips, cipe, 1, 1, cjps, cjpe,   &         ! CD dims
                  ngrid%icedepth,  &   ! ND field
                 nids, nide, 1, 1, njds, njde,   &         ! ND dims
                 nims, nime, 1, 1, njms, njme,   &         ! ND dims
                 nips, nipe, 1, 1, njps, njpe,   &         ! ND dims
                  config_flags%shw, ngrid%imask_nostag,         &         ! stencil half width
                  .FALSE., .FALSE.,                                                &         ! xstag, ystag
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%xicem, 1 ) * SIZE( grid%xicem, 2 ) .GT. 1 ) THEN 
CALL copy_fcnm (  &         
                  grid%xicem,   &       ! CD field
                 cids, cide, 1, 1, cjds, cjde,   &         ! CD dims
                 cims, cime, 1, 1, cjms, cjme,   &         ! CD dims
                 cips, cipe, 1, 1, cjps, cjpe,   &         ! CD dims
                  ngrid%xicem,  &   ! ND field
                 nids, nide, 1, 1, njds, njde,   &         ! ND dims
                 nims, nime, 1, 1, njms, njme,   &         ! ND dims
                 nips, nipe, 1, 1, njps, njpe,   &         ! ND dims
                  config_flags%shw, ngrid%imask_nostag,         &         ! stencil half width
                  .FALSE., .FALSE.,                                                &         ! xstag, ystag
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%albsi, 1 ) * SIZE( grid%albsi, 2 ) .GT. 1 ) THEN 
CALL copy_fcnm (  &         
                  grid%albsi,   &       ! CD field
                 cids, cide, 1, 1, cjds, cjde,   &         ! CD dims
                 cims, cime, 1, 1, cjms, cjme,   &         ! CD dims
                 cips, cipe, 1, 1, cjps, cjpe,   &         ! CD dims
                  ngrid%albsi,  &   ! ND field
                 nids, nide, 1, 1, njds, njde,   &         ! ND dims
                 nims, nime, 1, 1, njms, njme,   &         ! ND dims
                 nips, nipe, 1, 1, njps, njpe,   &         ! ND dims
                  config_flags%shw, ngrid%imask_nostag,         &         ! stencil half width
                  .FALSE., .FALSE.,                                                &         ! xstag, ystag
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%snowsi, 1 ) * SIZE( grid%snowsi, 2 ) .GT. 1 ) THEN 
CALL copy_fcnm (  &         
                  grid%snowsi,   &       ! CD field
                 cids, cide, 1, 1, cjds, cjde,   &         ! CD dims
                 cims, cime, 1, 1, cjms, cjme,   &         ! CD dims
                 cips, cipe, 1, 1, cjps, cjpe,   &         ! CD dims
                  ngrid%snowsi,  &   ! ND field
                 nids, nide, 1, 1, njds, njde,   &         ! ND dims
                 nims, nime, 1, 1, njms, njme,   &         ! ND dims
                 nips, nipe, 1, 1, njps, njpe,   &         ! ND dims
                  config_flags%shw, ngrid%imask_nostag,         &         ! stencil half width
                  .FALSE., .FALSE.,                                                &         ! xstag, ystag
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%ivgtyp, 1 ) * SIZE( grid%ivgtyp, 2 ) .GT. 1 ) THEN 
CALL copy_fcni (  &         
                  grid%ivgtyp,   &       ! CD field
                 cids, cide, 1, 1, cjds, cjde,   &         ! CD dims
                 cims, cime, 1, 1, cjms, cjme,   &         ! CD dims
                 cips, cipe, 1, 1, cjps, cjpe,   &         ! CD dims
                  ngrid%ivgtyp,  &   ! ND field
                 nids, nide, 1, 1, njds, njde,   &         ! ND dims
                 nims, nime, 1, 1, njms, njme,   &         ! ND dims
                 nips, nipe, 1, 1, njps, njpe,   &         ! ND dims
                  config_flags%shw, ngrid%imask_nostag,         &         ! stencil half width
                  .FALSE., .FALSE.,                                                &         ! xstag, ystag
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%isltyp, 1 ) * SIZE( grid%isltyp, 2 ) .GT. 1 ) THEN 
CALL copy_fcni (  &         
                  grid%isltyp,   &       ! CD field
                 cids, cide, 1, 1, cjds, cjde,   &         ! CD dims
                 cims, cime, 1, 1, cjms, cjme,   &         ! CD dims
                 cips, cipe, 1, 1, cjps, cjpe,   &         ! CD dims
                  ngrid%isltyp,  &   ! ND field
                 nids, nide, 1, 1, njds, njde,   &         ! ND dims
                 nims, nime, 1, 1, njms, njme,   &         ! ND dims
                 nips, nipe, 1, 1, njps, njpe,   &         ! ND dims
                  config_flags%shw, ngrid%imask_nostag,         &         ! stencil half width
                  .FALSE., .FALSE.,                                                &         ! xstag, ystag
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%vegfra, 1 ) * SIZE( grid%vegfra, 2 ) .GT. 1 ) THEN 
CALL copy_fcnm (  &         
                  grid%vegfra,   &       ! CD field
                 cids, cide, 1, 1, cjds, cjde,   &         ! CD dims
                 cims, cime, 1, 1, cjms, cjme,   &         ! CD dims
                 cips, cipe, 1, 1, cjps, cjpe,   &         ! CD dims
                  ngrid%vegfra,  &   ! ND field
                 nids, nide, 1, 1, njds, njde,   &         ! ND dims
                 nims, nime, 1, 1, njms, njme,   &         ! ND dims
                 nips, nipe, 1, 1, njps, njpe,   &         ! ND dims
                  config_flags%shw, ngrid%imask_nostag,         &         ! stencil half width
                  .FALSE., .FALSE.,                                                &         ! xstag, ystag
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%acgrdflx, 1 ) * SIZE( grid%acgrdflx, 2 ) .GT. 1 ) THEN 
CALL copy_fcn (  &         
                  grid%acgrdflx,   &       ! CD field
                 cids, cide, 1, 1, cjds, cjde,   &         ! CD dims
                 cims, cime, 1, 1, cjms, cjme,   &         ! CD dims
                 cips, cipe, 1, 1, cjps, cjpe,   &         ! CD dims
                  ngrid%acgrdflx,  &   ! ND field
                 nids, nide, 1, 1, njds, njde,   &         ! ND dims
                 nims, nime, 1, 1, njms, njme,   &         ! ND dims
                 nips, nipe, 1, 1, njps, njpe,   &         ! ND dims
                  config_flags%shw, ngrid%imask_nostag,         &         ! stencil half width
                  .FALSE., .FALSE.,                                                &         ! xstag, ystag
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%acsnow, 1 ) * SIZE( grid%acsnow, 2 ) .GT. 1 ) THEN 
CALL copy_fcnm (  &         
                  grid%acsnow,   &       ! CD field
                 cids, cide, 1, 1, cjds, cjde,   &         ! CD dims
                 cims, cime, 1, 1, cjms, cjme,   &         ! CD dims
                 cips, cipe, 1, 1, cjps, cjpe,   &         ! CD dims
                  ngrid%acsnow,  &   ! ND field
                 nids, nide, 1, 1, njds, njde,   &         ! ND dims
                 nims, nime, 1, 1, njms, njme,   &         ! ND dims
                 nips, nipe, 1, 1, njps, njpe,   &         ! ND dims
                  config_flags%shw, ngrid%imask_nostag,         &         ! stencil half width
                  .FALSE., .FALSE.,                                                &         ! xstag, ystag
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%acsnom, 1 ) * SIZE( grid%acsnom, 2 ) .GT. 1 ) THEN 
CALL copy_fcnm (  &         
                  grid%acsnom,   &       ! CD field
                 cids, cide, 1, 1, cjds, cjde,   &         ! CD dims
                 cims, cime, 1, 1, cjms, cjme,   &         ! CD dims
                 cips, cipe, 1, 1, cjps, cjpe,   &         ! CD dims
                  ngrid%acsnom,  &   ! ND field
                 nids, nide, 1, 1, njds, njde,   &         ! ND dims
                 nims, nime, 1, 1, njms, njme,   &         ! ND dims
                 nips, nipe, 1, 1, njps, njpe,   &         ! ND dims
                  config_flags%shw, ngrid%imask_nostag,         &         ! stencil half width
                  .FALSE., .FALSE.,                                                &         ! xstag, ystag
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%snow, 1 ) * SIZE( grid%snow, 2 ) .GT. 1 ) THEN 
CALL copy_fcnm (  &         
                  grid%snow,   &       ! CD field
                 cids, cide, 1, 1, cjds, cjde,   &         ! CD dims
                 cims, cime, 1, 1, cjms, cjme,   &         ! CD dims
                 cips, cipe, 1, 1, cjps, cjpe,   &         ! CD dims
                  ngrid%snow,  &   ! ND field
                 nids, nide, 1, 1, njds, njde,   &         ! ND dims
                 nims, nime, 1, 1, njms, njme,   &         ! ND dims
                 nips, nipe, 1, 1, njps, njpe,   &         ! ND dims
                  config_flags%shw, ngrid%imask_nostag,         &         ! stencil half width
                  .FALSE., .FALSE.,                                                &         ! xstag, ystag
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%snowh, 1 ) * SIZE( grid%snowh, 2 ) .GT. 1 ) THEN 
CALL copy_fcnm (  &         
                  grid%snowh,   &       ! CD field
                 cids, cide, 1, 1, cjds, cjde,   &         ! CD dims
                 cims, cime, 1, 1, cjms, cjme,   &         ! CD dims
                 cips, cipe, 1, 1, cjps, cjpe,   &         ! CD dims
                  ngrid%snowh,  &   ! ND field
                 nids, nide, 1, 1, njds, njde,   &         ! ND dims
                 nims, nime, 1, 1, njms, njme,   &         ! ND dims
                 nips, nipe, 1, 1, njps, njpe,   &         ! ND dims
                  config_flags%shw, ngrid%imask_nostag,         &         ! stencil half width
                  .FALSE., .FALSE.,                                                &         ! xstag, ystag
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%canwat, 1 ) * SIZE( grid%canwat, 2 ) .GT. 1 ) THEN 
CALL copy_fcnm (  &         
                  grid%canwat,   &       ! CD field
                 cids, cide, 1, 1, cjds, cjde,   &         ! CD dims
                 cims, cime, 1, 1, cjms, cjme,   &         ! CD dims
                 cips, cipe, 1, 1, cjps, cjpe,   &         ! CD dims
                  ngrid%canwat,  &   ! ND field
                 nids, nide, 1, 1, njds, njde,   &         ! ND dims
                 nims, nime, 1, 1, njms, njme,   &         ! ND dims
                 nips, nipe, 1, 1, njps, njpe,   &         ! ND dims
                  config_flags%shw, ngrid%imask_nostag,         &         ! stencil half width
                  .FALSE., .FALSE.,                                                &         ! xstag, ystag
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%tr_urb2d, 1 ) * SIZE( grid%tr_urb2d, 2 ) .GT. 1 ) THEN 
CALL copy_fcnm (  &         
                  grid%tr_urb2d,   &       ! CD field
                 cids, cide, 1, 1, cjds, cjde,   &         ! CD dims
                 cims, cime, 1, 1, cjms, cjme,   &         ! CD dims
                 cips, cipe, 1, 1, cjps, cjpe,   &         ! CD dims
                  ngrid%tr_urb2d,  &   ! ND field
                 nids, nide, 1, 1, njds, njde,   &         ! ND dims
                 nims, nime, 1, 1, njms, njme,   &         ! ND dims
                 nips, nipe, 1, 1, njps, njpe,   &         ! ND dims
                  config_flags%shw, ngrid%imask_nostag,         &         ! stencil half width
                  .FALSE., .FALSE.,                                                &         ! xstag, ystag
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%tb_urb2d, 1 ) * SIZE( grid%tb_urb2d, 2 ) .GT. 1 ) THEN 
CALL copy_fcnm (  &         
                  grid%tb_urb2d,   &       ! CD field
                 cids, cide, 1, 1, cjds, cjde,   &         ! CD dims
                 cims, cime, 1, 1, cjms, cjme,   &         ! CD dims
                 cips, cipe, 1, 1, cjps, cjpe,   &         ! CD dims
                  ngrid%tb_urb2d,  &   ! ND field
                 nids, nide, 1, 1, njds, njde,   &         ! ND dims
                 nims, nime, 1, 1, njms, njme,   &         ! ND dims
                 nips, nipe, 1, 1, njps, njpe,   &         ! ND dims
                  config_flags%shw, ngrid%imask_nostag,         &         ! stencil half width
                  .FALSE., .FALSE.,                                                &         ! xstag, ystag
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%tg_urb2d, 1 ) * SIZE( grid%tg_urb2d, 2 ) .GT. 1 ) THEN 
CALL copy_fcnm (  &         
                  grid%tg_urb2d,   &       ! CD field
                 cids, cide, 1, 1, cjds, cjde,   &         ! CD dims
                 cims, cime, 1, 1, cjms, cjme,   &         ! CD dims
                 cips, cipe, 1, 1, cjps, cjpe,   &         ! CD dims
                  ngrid%tg_urb2d,  &   ! ND field
                 nids, nide, 1, 1, njds, njde,   &         ! ND dims
                 nims, nime, 1, 1, njms, njme,   &         ! ND dims
                 nips, nipe, 1, 1, njps, njpe,   &         ! ND dims
                  config_flags%shw, ngrid%imask_nostag,         &         ! stencil half width
                  .FALSE., .FALSE.,                                                &         ! xstag, ystag
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%tc_urb2d, 1 ) * SIZE( grid%tc_urb2d, 2 ) .GT. 1 ) THEN 
CALL copy_fcnm (  &         
                  grid%tc_urb2d,   &       ! CD field
                 cids, cide, 1, 1, cjds, cjde,   &         ! CD dims
                 cims, cime, 1, 1, cjms, cjme,   &         ! CD dims
                 cips, cipe, 1, 1, cjps, cjpe,   &         ! CD dims
                  ngrid%tc_urb2d,  &   ! ND field
                 nids, nide, 1, 1, njds, njde,   &         ! ND dims
                 nims, nime, 1, 1, njms, njme,   &         ! ND dims
                 nips, nipe, 1, 1, njps, njpe,   &         ! ND dims
                  config_flags%shw, ngrid%imask_nostag,         &         ! stencil half width
                  .FALSE., .FALSE.,                                                &         ! xstag, ystag
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%qc_urb2d, 1 ) * SIZE( grid%qc_urb2d, 2 ) .GT. 1 ) THEN 
CALL copy_fcnm (  &         
                  grid%qc_urb2d,   &       ! CD field
                 cids, cide, 1, 1, cjds, cjde,   &         ! CD dims
                 cims, cime, 1, 1, cjms, cjme,   &         ! CD dims
                 cips, cipe, 1, 1, cjps, cjpe,   &         ! CD dims
                  ngrid%qc_urb2d,  &   ! ND field
                 nids, nide, 1, 1, njds, njde,   &         ! ND dims
                 nims, nime, 1, 1, njms, njme,   &         ! ND dims
                 nips, nipe, 1, 1, njps, njpe,   &         ! ND dims
                  config_flags%shw, ngrid%imask_nostag,         &         ! stencil half width
                  .FALSE., .FALSE.,                                                &         ! xstag, ystag
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%uc_urb2d, 1 ) * SIZE( grid%uc_urb2d, 2 ) .GT. 1 ) THEN 
CALL copy_fcnm (  &         
                  grid%uc_urb2d,   &       ! CD field
                 cids, cide, 1, 1, cjds, cjde,   &         ! CD dims
                 cims, cime, 1, 1, cjms, cjme,   &         ! CD dims
                 cips, cipe, 1, 1, cjps, cjpe,   &         ! CD dims
                  ngrid%uc_urb2d,  &   ! ND field
                 nids, nide, 1, 1, njds, njde,   &         ! ND dims
                 nims, nime, 1, 1, njms, njme,   &         ! ND dims
                 nips, nipe, 1, 1, njps, njpe,   &         ! ND dims
                  config_flags%shw, ngrid%imask_nostag,         &         ! stencil half width
                  .FALSE., .FALSE.,                                                &         ! xstag, ystag
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%xxxr_urb2d, 1 ) * SIZE( grid%xxxr_urb2d, 2 ) .GT. 1 ) THEN 
CALL copy_fcnm (  &         
                  grid%xxxr_urb2d,   &       ! CD field
                 cids, cide, 1, 1, cjds, cjde,   &         ! CD dims
                 cims, cime, 1, 1, cjms, cjme,   &         ! CD dims
                 cips, cipe, 1, 1, cjps, cjpe,   &         ! CD dims
                  ngrid%xxxr_urb2d,  &   ! ND field
                 nids, nide, 1, 1, njds, njde,   &         ! ND dims
                 nims, nime, 1, 1, njms, njme,   &         ! ND dims
                 nips, nipe, 1, 1, njps, njpe,   &         ! ND dims
                  config_flags%shw, ngrid%imask_nostag,         &         ! stencil half width
                  .FALSE., .FALSE.,                                                &         ! xstag, ystag
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%xxxb_urb2d, 1 ) * SIZE( grid%xxxb_urb2d, 2 ) .GT. 1 ) THEN 
CALL copy_fcnm (  &         
                  grid%xxxb_urb2d,   &       ! CD field
                 cids, cide, 1, 1, cjds, cjde,   &         ! CD dims
                 cims, cime, 1, 1, cjms, cjme,   &         ! CD dims
                 cips, cipe, 1, 1, cjps, cjpe,   &         ! CD dims
                  ngrid%xxxb_urb2d,  &   ! ND field
                 nids, nide, 1, 1, njds, njde,   &         ! ND dims
                 nims, nime, 1, 1, njms, njme,   &         ! ND dims
                 nips, nipe, 1, 1, njps, njpe,   &         ! ND dims
                  config_flags%shw, ngrid%imask_nostag,         &         ! stencil half width
                  .FALSE., .FALSE.,                                                &         ! xstag, ystag
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%xxxg_urb2d, 1 ) * SIZE( grid%xxxg_urb2d, 2 ) .GT. 1 ) THEN 
CALL copy_fcnm (  &         
                  grid%xxxg_urb2d,   &       ! CD field
                 cids, cide, 1, 1, cjds, cjde,   &         ! CD dims
                 cims, cime, 1, 1, cjms, cjme,   &         ! CD dims
                 cips, cipe, 1, 1, cjps, cjpe,   &         ! CD dims
                  ngrid%xxxg_urb2d,  &   ! ND field
                 nids, nide, 1, 1, njds, njde,   &         ! ND dims
                 nims, nime, 1, 1, njms, njme,   &         ! ND dims
                 nips, nipe, 1, 1, njps, njpe,   &         ! ND dims
                  config_flags%shw, ngrid%imask_nostag,         &         ! stencil half width
                  .FALSE., .FALSE.,                                                &         ! xstag, ystag
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%xxxc_urb2d, 1 ) * SIZE( grid%xxxc_urb2d, 2 ) .GT. 1 ) THEN 
CALL copy_fcnm (  &         
                  grid%xxxc_urb2d,   &       ! CD field
                 cids, cide, 1, 1, cjds, cjde,   &         ! CD dims
                 cims, cime, 1, 1, cjms, cjme,   &         ! CD dims
                 cips, cipe, 1, 1, cjps, cjpe,   &         ! CD dims
                  ngrid%xxxc_urb2d,  &   ! ND field
                 nids, nide, 1, 1, njds, njde,   &         ! ND dims
                 nims, nime, 1, 1, njms, njme,   &         ! ND dims
                 nips, nipe, 1, 1, njps, njpe,   &         ! ND dims
                  config_flags%shw, ngrid%imask_nostag,         &         ! stencil half width
                  .FALSE., .FALSE.,                                                &         ! xstag, ystag
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%trl_urb3d, 1 ) * SIZE( grid%trl_urb3d, 3 ) .GT. 1 ) THEN 
CALL copy_fcnm (  &         
                  grid%trl_urb3d,   &       ! CD field
                 cids, cide, 1, config_flags%num_soil_layers, cjds, cjde,   &         ! CD dims
                 cims, cime, 1, config_flags%num_soil_layers, cjms, cjme,   &         ! CD dims
                 cips, cipe, 1, config_flags%num_soil_layers, cjps, cjpe,   &         ! CD dims
                  ngrid%trl_urb3d,  &   ! ND field
                 nids, nide, 1, config_flags%num_soil_layers, njds, njde,   &         ! ND dims
                 nims, nime, 1, config_flags%num_soil_layers, njms, njme,   &         ! ND dims
                 nips, nipe, 1, config_flags%num_soil_layers, njps, njpe,   &         ! ND dims
                  config_flags%shw, ngrid%imask_nostag,         &         ! stencil half width
                  .FALSE., .FALSE.,                                                &         ! xstag, ystag
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%tbl_urb3d, 1 ) * SIZE( grid%tbl_urb3d, 3 ) .GT. 1 ) THEN 
CALL copy_fcnm (  &         
                  grid%tbl_urb3d,   &       ! CD field
                 cids, cide, 1, config_flags%num_soil_layers, cjds, cjde,   &         ! CD dims
                 cims, cime, 1, config_flags%num_soil_layers, cjms, cjme,   &         ! CD dims
                 cips, cipe, 1, config_flags%num_soil_layers, cjps, cjpe,   &         ! CD dims
                  ngrid%tbl_urb3d,  &   ! ND field
                 nids, nide, 1, config_flags%num_soil_layers, njds, njde,   &         ! ND dims
                 nims, nime, 1, config_flags%num_soil_layers, njms, njme,   &         ! ND dims
                 nips, nipe, 1, config_flags%num_soil_layers, njps, njpe,   &         ! ND dims
                  config_flags%shw, ngrid%imask_nostag,         &         ! stencil half width
                  .FALSE., .FALSE.,                                                &         ! xstag, ystag
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%tgl_urb3d, 1 ) * SIZE( grid%tgl_urb3d, 3 ) .GT. 1 ) THEN 
CALL copy_fcnm (  &         
                  grid%tgl_urb3d,   &       ! CD field
                 cids, cide, 1, config_flags%num_soil_layers, cjds, cjde,   &         ! CD dims
                 cims, cime, 1, config_flags%num_soil_layers, cjms, cjme,   &         ! CD dims
                 cips, cipe, 1, config_flags%num_soil_layers, cjps, cjpe,   &         ! CD dims
                  ngrid%tgl_urb3d,  &   ! ND field
                 nids, nide, 1, config_flags%num_soil_layers, njds, njde,   &         ! ND dims
                 nims, nime, 1, config_flags%num_soil_layers, njms, njme,   &         ! ND dims
                 nips, nipe, 1, config_flags%num_soil_layers, njps, njpe,   &         ! ND dims
                  config_flags%shw, ngrid%imask_nostag,         &         ! stencil half width
                  .FALSE., .FALSE.,                                                &         ! xstag, ystag
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%frc_urb2d, 1 ) * SIZE( grid%frc_urb2d, 2 ) .GT. 1 ) THEN 
CALL copy_fcnm (  &         
                  grid%frc_urb2d,   &       ! CD field
                 cids, cide, 1, 1, cjds, cjde,   &         ! CD dims
                 cims, cime, 1, 1, cjms, cjme,   &         ! CD dims
                 cips, cipe, 1, 1, cjps, cjpe,   &         ! CD dims
                  ngrid%frc_urb2d,  &   ! ND field
                 nids, nide, 1, 1, njds, njde,   &         ! ND dims
                 nims, nime, 1, 1, njms, njme,   &         ! ND dims
                 nips, nipe, 1, 1, njps, njpe,   &         ! ND dims
                  config_flags%shw, ngrid%imask_nostag,         &         ! stencil half width
                  .FALSE., .FALSE.,                                                &         ! xstag, ystag
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%utype_urb2d, 1 ) * SIZE( grid%utype_urb2d, 2 ) .GT. 1 ) THEN 
CALL copy_fcnm (  &         
                  grid%utype_urb2d,   &       ! CD field
                 cids, cide, 1, 1, cjds, cjde,   &         ! CD dims
                 cims, cime, 1, 1, cjms, cjme,   &         ! CD dims
                 cips, cipe, 1, 1, cjps, cjpe,   &         ! CD dims
                  ngrid%utype_urb2d,  &   ! ND field
                 nids, nide, 1, 1, njds, njde,   &         ! ND dims
                 nims, nime, 1, 1, njms, njme,   &         ! ND dims
                 nips, nipe, 1, 1, njps, njpe,   &         ! ND dims
                  config_flags%shw, ngrid%imask_nostag,         &         ! stencil half width
                  .FALSE., .FALSE.,                                                &         ! xstag, ystag
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%var2d, 1 ) * SIZE( grid%var2d, 2 ) .GT. 1 ) THEN 
CALL copy_fcn (  &         
                  grid%var2d,   &       ! CD field
                 cids, cide, 1, 1, cjds, cjde,   &         ! CD dims
                 cims, cime, 1, 1, cjms, cjme,   &         ! CD dims
                 cips, cipe, 1, 1, cjps, cjpe,   &         ! CD dims
                  ngrid%var2d,  &   ! ND field
                 nids, nide, 1, 1, njds, njde,   &         ! ND dims
                 nims, nime, 1, 1, njms, njme,   &         ! ND dims
                 nips, nipe, 1, 1, njps, njpe,   &         ! ND dims
                  config_flags%shw, ngrid%imask_nostag,         &         ! stencil half width
                  .FALSE., .FALSE.,                                                &         ! xstag, ystag
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%oc12d, 1 ) * SIZE( grid%oc12d, 2 ) .GT. 1 ) THEN 
CALL copy_fcn (  &         
                  grid%oc12d,   &       ! CD field
                 cids, cide, 1, 1, cjds, cjde,   &         ! CD dims
                 cims, cime, 1, 1, cjms, cjme,   &         ! CD dims
                 cips, cipe, 1, 1, cjps, cjpe,   &         ! CD dims
                  ngrid%oc12d,  &   ! ND field
                 nids, nide, 1, 1, njds, njde,   &         ! ND dims
                 nims, nime, 1, 1, njms, njme,   &         ! ND dims
                 nips, nipe, 1, 1, njps, njpe,   &         ! ND dims
                  config_flags%shw, ngrid%imask_nostag,         &         ! stencil half width
                  .FALSE., .FALSE.,                                                &         ! xstag, ystag
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%oa1, 1 ) * SIZE( grid%oa1, 2 ) .GT. 1 ) THEN 
CALL copy_fcn (  &         
                  grid%oa1,   &       ! CD field
                 cids, cide, 1, 1, cjds, cjde,   &         ! CD dims
                 cims, cime, 1, 1, cjms, cjme,   &         ! CD dims
                 cips, cipe, 1, 1, cjps, cjpe,   &         ! CD dims
                  ngrid%oa1,  &   ! ND field
                 nids, nide, 1, 1, njds, njde,   &         ! ND dims
                 nims, nime, 1, 1, njms, njme,   &         ! ND dims
                 nips, nipe, 1, 1, njps, njpe,   &         ! ND dims
                  config_flags%shw, ngrid%imask_nostag,         &         ! stencil half width
                  .FALSE., .FALSE.,                                                &         ! xstag, ystag
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%oa2, 1 ) * SIZE( grid%oa2, 2 ) .GT. 1 ) THEN 
CALL copy_fcn (  &         
                  grid%oa2,   &       ! CD field
                 cids, cide, 1, 1, cjds, cjde,   &         ! CD dims
                 cims, cime, 1, 1, cjms, cjme,   &         ! CD dims
                 cips, cipe, 1, 1, cjps, cjpe,   &         ! CD dims
                  ngrid%oa2,  &   ! ND field
                 nids, nide, 1, 1, njds, njde,   &         ! ND dims
                 nims, nime, 1, 1, njms, njme,   &         ! ND dims
                 nips, nipe, 1, 1, njps, njpe,   &         ! ND dims
                  config_flags%shw, ngrid%imask_nostag,         &         ! stencil half width
                  .FALSE., .FALSE.,                                                &         ! xstag, ystag
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%oa3, 1 ) * SIZE( grid%oa3, 2 ) .GT. 1 ) THEN 
CALL copy_fcn (  &         
                  grid%oa3,   &       ! CD field
                 cids, cide, 1, 1, cjds, cjde,   &         ! CD dims
                 cims, cime, 1, 1, cjms, cjme,   &         ! CD dims
                 cips, cipe, 1, 1, cjps, cjpe,   &         ! CD dims
                  ngrid%oa3,  &   ! ND field
                 nids, nide, 1, 1, njds, njde,   &         ! ND dims
                 nims, nime, 1, 1, njms, njme,   &         ! ND dims
                 nips, nipe, 1, 1, njps, njpe,   &         ! ND dims
                  config_flags%shw, ngrid%imask_nostag,         &         ! stencil half width
                  .FALSE., .FALSE.,                                                &         ! xstag, ystag
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%oa4, 1 ) * SIZE( grid%oa4, 2 ) .GT. 1 ) THEN 
CALL copy_fcn (  &         
                  grid%oa4,   &       ! CD field
                 cids, cide, 1, 1, cjds, cjde,   &         ! CD dims
                 cims, cime, 1, 1, cjms, cjme,   &         ! CD dims
                 cips, cipe, 1, 1, cjps, cjpe,   &         ! CD dims
                  ngrid%oa4,  &   ! ND field
                 nids, nide, 1, 1, njds, njde,   &         ! ND dims
                 nims, nime, 1, 1, njms, njme,   &         ! ND dims
                 nips, nipe, 1, 1, njps, njpe,   &         ! ND dims
                  config_flags%shw, ngrid%imask_nostag,         &         ! stencil half width
                  .FALSE., .FALSE.,                                                &         ! xstag, ystag
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%ol1, 1 ) * SIZE( grid%ol1, 2 ) .GT. 1 ) THEN 
CALL copy_fcn (  &         
                  grid%ol1,   &       ! CD field
                 cids, cide, 1, 1, cjds, cjde,   &         ! CD dims
                 cims, cime, 1, 1, cjms, cjme,   &         ! CD dims
                 cips, cipe, 1, 1, cjps, cjpe,   &         ! CD dims
                  ngrid%ol1,  &   ! ND field
                 nids, nide, 1, 1, njds, njde,   &         ! ND dims
                 nims, nime, 1, 1, njms, njme,   &         ! ND dims
                 nips, nipe, 1, 1, njps, njpe,   &         ! ND dims
                  config_flags%shw, ngrid%imask_nostag,         &         ! stencil half width
                  .FALSE., .FALSE.,                                                &         ! xstag, ystag
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%ol2, 1 ) * SIZE( grid%ol2, 2 ) .GT. 1 ) THEN 
CALL copy_fcn (  &         
                  grid%ol2,   &       ! CD field
                 cids, cide, 1, 1, cjds, cjde,   &         ! CD dims
                 cims, cime, 1, 1, cjms, cjme,   &         ! CD dims
                 cips, cipe, 1, 1, cjps, cjpe,   &         ! CD dims
                  ngrid%ol2,  &   ! ND field
                 nids, nide, 1, 1, njds, njde,   &         ! ND dims
                 nims, nime, 1, 1, njms, njme,   &         ! ND dims
                 nips, nipe, 1, 1, njps, njpe,   &         ! ND dims
                  config_flags%shw, ngrid%imask_nostag,         &         ! stencil half width
                  .FALSE., .FALSE.,                                                &         ! xstag, ystag
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%ol3, 1 ) * SIZE( grid%ol3, 2 ) .GT. 1 ) THEN 
CALL copy_fcn (  &         
                  grid%ol3,   &       ! CD field
                 cids, cide, 1, 1, cjds, cjde,   &         ! CD dims
                 cims, cime, 1, 1, cjms, cjme,   &         ! CD dims
                 cips, cipe, 1, 1, cjps, cjpe,   &         ! CD dims
                  ngrid%ol3,  &   ! ND field
                 nids, nide, 1, 1, njds, njde,   &         ! ND dims
                 nims, nime, 1, 1, njms, njme,   &         ! ND dims
                 nips, nipe, 1, 1, njps, njpe,   &         ! ND dims
                  config_flags%shw, ngrid%imask_nostag,         &         ! stencil half width
                  .FALSE., .FALSE.,                                                &         ! xstag, ystag
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%ol4, 1 ) * SIZE( grid%ol4, 2 ) .GT. 1 ) THEN 
CALL copy_fcn (  &         
                  grid%ol4,   &       ! CD field
                 cids, cide, 1, 1, cjds, cjde,   &         ! CD dims
                 cims, cime, 1, 1, cjms, cjme,   &         ! CD dims
                 cips, cipe, 1, 1, cjps, cjpe,   &         ! CD dims
                  ngrid%ol4,  &   ! ND field
                 nids, nide, 1, 1, njds, njde,   &         ! ND dims
                 nims, nime, 1, 1, njms, njme,   &         ! ND dims
                 nips, nipe, 1, 1, njps, njpe,   &         ! ND dims
                  config_flags%shw, ngrid%imask_nostag,         &         ! stencil half width
                  .FALSE., .FALSE.,                                                &         ! xstag, ystag
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%ctopo, 1 ) * SIZE( grid%ctopo, 2 ) .GT. 1 ) THEN 
CALL copy_fcn (  &         
                  grid%ctopo,   &       ! CD field
                 cids, cide, 1, 1, cjds, cjde,   &         ! CD dims
                 cims, cime, 1, 1, cjms, cjme,   &         ! CD dims
                 cips, cipe, 1, 1, cjps, cjpe,   &         ! CD dims
                  ngrid%ctopo,  &   ! ND field
                 nids, nide, 1, 1, njds, njde,   &         ! ND dims
                 nims, nime, 1, 1, njms, njme,   &         ! ND dims
                 nips, nipe, 1, 1, njps, njpe,   &         ! ND dims
                  config_flags%shw, ngrid%imask_nostag,         &         ! stencil half width
                  .FALSE., .FALSE.,                                                &         ! xstag, ystag
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%ctopo2, 1 ) * SIZE( grid%ctopo2, 2 ) .GT. 1 ) THEN 
CALL copy_fcn (  &         
                  grid%ctopo2,   &       ! CD field
                 cids, cide, 1, 1, cjds, cjde,   &         ! CD dims
                 cims, cime, 1, 1, cjms, cjme,   &         ! CD dims
                 cips, cipe, 1, 1, cjps, cjpe,   &         ! CD dims
                  ngrid%ctopo2,  &   ! ND field
                 nids, nide, 1, 1, njds, njde,   &         ! ND dims
                 nims, nime, 1, 1, njms, njme,   &         ! ND dims
                 nips, nipe, 1, 1, njps, njpe,   &         ! ND dims
                  config_flags%shw, ngrid%imask_nostag,         &         ! stencil half width
                  .FALSE., .FALSE.,                                                &         ! xstag, ystag
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%f_ice_phy, 1 ) * SIZE( grid%f_ice_phy, 3 ) .GT. 1 ) THEN 
CALL copy_fcn (  &         
                  grid%f_ice_phy,   &       ! CD field
                 cids, cide, ckds, ckde, cjds, cjde,   &         ! CD dims
                 cims, cime, ckms, ckme, cjms, cjme,   &         ! CD dims
                 cips, cipe, ckps, MIN( (ckde-1), ckpe ), cjps, cjpe,   &         ! CD dims
                  ngrid%f_ice_phy,  &   ! ND field
                 nids, nide, nkds, nkde, njds, njde,   &         ! ND dims
                 nims, nime, nkms, nkme, njms, njme,   &         ! ND dims
                 nips, nipe, nkps, MIN( (nkde-1), nkpe ), njps, njpe,   &         ! ND dims
                  config_flags%shw, ngrid%imask_nostag,         &         ! stencil half width
                  .FALSE., .FALSE.,                                                &         ! xstag, ystag
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%f_rain_phy, 1 ) * SIZE( grid%f_rain_phy, 3 ) .GT. 1 ) THEN 
CALL copy_fcn (  &         
                  grid%f_rain_phy,   &       ! CD field
                 cids, cide, ckds, ckde, cjds, cjde,   &         ! CD dims
                 cims, cime, ckms, ckme, cjms, cjme,   &         ! CD dims
                 cips, cipe, ckps, MIN( (ckde-1), ckpe ), cjps, cjpe,   &         ! CD dims
                  ngrid%f_rain_phy,  &   ! ND field
                 nids, nide, nkds, nkde, njds, njde,   &         ! ND dims
                 nims, nime, nkms, nkme, njms, njme,   &         ! ND dims
                 nips, nipe, nkps, MIN( (nkde-1), nkpe ), njps, njpe,   &         ! ND dims
                  config_flags%shw, ngrid%imask_nostag,         &         ! stencil half width
                  .FALSE., .FALSE.,                                                &         ! xstag, ystag
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%f_rimef_phy, 1 ) * SIZE( grid%f_rimef_phy, 3 ) .GT. 1 ) THEN 
CALL copy_fcn (  &         
                  grid%f_rimef_phy,   &       ! CD field
                 cids, cide, ckds, ckde, cjds, cjde,   &         ! CD dims
                 cims, cime, ckms, ckme, cjms, cjme,   &         ! CD dims
                 cips, cipe, ckps, MIN( (ckde-1), ckpe ), cjps, cjpe,   &         ! CD dims
                  ngrid%f_rimef_phy,  &   ! ND field
                 nids, nide, nkds, nkde, njds, njde,   &         ! ND dims
                 nims, nime, nkms, nkme, njms, njme,   &         ! ND dims
                 nips, nipe, nkps, MIN( (nkde-1), nkpe ), njps, njpe,   &         ! ND dims
                  config_flags%shw, ngrid%imask_nostag,         &         ! stencil half width
                  .FALSE., .FALSE.,                                                &         ! xstag, ystag
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%om_tmp, 1 ) * SIZE( grid%om_tmp, 3 ) .GT. 1 ) THEN 
CALL copy_fcnm (  &         
                  grid%om_tmp,   &       ! CD field
                 cids, cide, 1, config_flags%ocean_levels, cjds, cjde,   &         ! CD dims
                 cims, cime, 1, config_flags%ocean_levels, cjms, cjme,   &         ! CD dims
                 cips, cipe, 1, config_flags%ocean_levels, cjps, cjpe,   &         ! CD dims
                  ngrid%om_tmp,  &   ! ND field
                 nids, nide, 1, config_flags%ocean_levels, njds, njde,   &         ! ND dims
                 nims, nime, 1, config_flags%ocean_levels, njms, njme,   &         ! ND dims
                 nips, nipe, 1, config_flags%ocean_levels, njps, njpe,   &         ! ND dims
                  config_flags%shw, ngrid%imask_nostag,         &         ! stencil half width
                  .FALSE., .FALSE.,                                                &         ! xstag, ystag
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%om_s, 1 ) * SIZE( grid%om_s, 3 ) .GT. 1 ) THEN 
CALL copy_fcnm (  &         
                  grid%om_s,   &       ! CD field
                 cids, cide, 1, config_flags%ocean_levels, cjds, cjde,   &         ! CD dims
                 cims, cime, 1, config_flags%ocean_levels, cjms, cjme,   &         ! CD dims
                 cips, cipe, 1, config_flags%ocean_levels, cjps, cjpe,   &         ! CD dims
                  ngrid%om_s,  &   ! ND field
                 nids, nide, 1, config_flags%ocean_levels, njds, njde,   &         ! ND dims
                 nims, nime, 1, config_flags%ocean_levels, njms, njme,   &         ! ND dims
                 nips, nipe, 1, config_flags%ocean_levels, njps, njpe,   &         ! ND dims
                  config_flags%shw, ngrid%imask_nostag,         &         ! stencil half width
                  .FALSE., .FALSE.,                                                &         ! xstag, ystag
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%om_u, 1 ) * SIZE( grid%om_u, 3 ) .GT. 1 ) THEN 
CALL copy_fcnm (  &         
                  grid%om_u,   &       ! CD field
                 cids, cide, 1, config_flags%ocean_levels, cjds, cjde,   &         ! CD dims
                 cims, cime, 1, config_flags%ocean_levels, cjms, cjme,   &         ! CD dims
                 cips, cipe, 1, config_flags%ocean_levels, cjps, cjpe,   &         ! CD dims
                  ngrid%om_u,  &   ! ND field
                 nids, nide, 1, config_flags%ocean_levels, njds, njde,   &         ! ND dims
                 nims, nime, 1, config_flags%ocean_levels, njms, njme,   &         ! ND dims
                 nips, nipe, 1, config_flags%ocean_levels, njps, njpe,   &         ! ND dims
                  config_flags%shw, ngrid%imask_nostag,         &         ! stencil half width
                  .FALSE., .FALSE.,                                                &         ! xstag, ystag
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%om_v, 1 ) * SIZE( grid%om_v, 3 ) .GT. 1 ) THEN 
CALL copy_fcnm (  &         
                  grid%om_v,   &       ! CD field
                 cids, cide, 1, config_flags%ocean_levels, cjds, cjde,   &         ! CD dims
                 cims, cime, 1, config_flags%ocean_levels, cjms, cjme,   &         ! CD dims
                 cips, cipe, 1, config_flags%ocean_levels, cjps, cjpe,   &         ! CD dims
                  ngrid%om_v,  &   ! ND field
                 nids, nide, 1, config_flags%ocean_levels, njds, njde,   &         ! ND dims
                 nims, nime, 1, config_flags%ocean_levels, njms, njme,   &         ! ND dims
                 nips, nipe, 1, config_flags%ocean_levels, njps, njpe,   &         ! ND dims
                  config_flags%shw, ngrid%imask_nostag,         &         ! stencil half width
                  .FALSE., .FALSE.,                                                &         ! xstag, ystag
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%om_ml, 1 ) * SIZE( grid%om_ml, 2 ) .GT. 1 ) THEN 
CALL copy_fcnm (  &         
                  grid%om_ml,   &       ! CD field
                 cids, cide, 1, 1, cjds, cjde,   &         ! CD dims
                 cims, cime, 1, 1, cjms, cjme,   &         ! CD dims
                 cips, cipe, 1, 1, cjps, cjpe,   &         ! CD dims
                  ngrid%om_ml,  &   ! ND field
                 nids, nide, 1, 1, njds, njde,   &         ! ND dims
                 nims, nime, 1, 1, njms, njme,   &         ! ND dims
                 nips, nipe, 1, 1, njps, njpe,   &         ! ND dims
                  config_flags%shw, ngrid%imask_nostag,         &         ! stencil half width
                  .FALSE., .FALSE.,                                                &         ! xstag, ystag
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%h_diabatic, 1 ) * SIZE( grid%h_diabatic, 3 ) .GT. 1 ) THEN 
CALL copy_fcn (  &         
                  grid%h_diabatic,   &       ! CD field
                 cids, cide, ckds, ckde, cjds, cjde,   &         ! CD dims
                 cims, cime, ckms, ckme, cjms, cjme,   &         ! CD dims
                 cips, cipe, ckps, MIN( (ckde-1), ckpe ), cjps, cjpe,   &         ! CD dims
                  ngrid%h_diabatic,  &   ! ND field
                 nids, nide, nkds, nkde, njds, njde,   &         ! ND dims
                 nims, nime, nkms, nkme, njms, njme,   &         ! ND dims
                 nips, nipe, nkps, MIN( (nkde-1), nkpe ), njps, njpe,   &         ! ND dims
                  config_flags%shw, ngrid%imask_nostag,         &         ! stencil half width
                  .FALSE., .FALSE.,                                                &         ! xstag, ystag
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%msft, 1 ) * SIZE( grid%msft, 2 ) .GT. 1 ) THEN 
CALL copy_fcnm (  &         
                  grid%msft,   &       ! CD field
                 cids, cide, 1, 1, cjds, cjde,   &         ! CD dims
                 cims, cime, 1, 1, cjms, cjme,   &         ! CD dims
                 cips, cipe, 1, 1, cjps, cjpe,   &         ! CD dims
                  ngrid%msft,  &   ! ND field
                 nids, nide, 1, 1, njds, njde,   &         ! ND dims
                 nims, nime, 1, 1, njms, njme,   &         ! ND dims
                 nips, nipe, 1, 1, njps, njpe,   &         ! ND dims
                  config_flags%shw, ngrid%imask_nostag,         &         ! stencil half width
                  .FALSE., .FALSE.,                                                &         ! xstag, ystag
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%msfu, 1 ) * SIZE( grid%msfu, 2 ) .GT. 1 ) THEN 
CALL copy_fcnm (  &         
                  grid%msfu,   &       ! CD field
                 cids, cide, 1, 1, cjds, cjde,   &         ! CD dims
                 cims, cime, 1, 1, cjms, cjme,   &         ! CD dims
                 cips, cipe, 1, 1, cjps, cjpe,   &         ! CD dims
                  ngrid%msfu,  &   ! ND field
                 nids, nide, 1, 1, njds, njde,   &         ! ND dims
                 nims, nime, 1, 1, njms, njme,   &         ! ND dims
                 nips, nipe, 1, 1, njps, njpe,   &         ! ND dims
                  config_flags%shw, ngrid%imask_xstag,         &         ! stencil half width
                  .TRUE., .FALSE.,                                                &         ! xstag, ystag
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%msfv, 1 ) * SIZE( grid%msfv, 2 ) .GT. 1 ) THEN 
CALL copy_fcnm (  &         
                  grid%msfv,   &       ! CD field
                 cids, cide, 1, 1, cjds, cjde,   &         ! CD dims
                 cims, cime, 1, 1, cjms, cjme,   &         ! CD dims
                 cips, cipe, 1, 1, cjps, cjpe,   &         ! CD dims
                  ngrid%msfv,  &   ! ND field
                 nids, nide, 1, 1, njds, njde,   &         ! ND dims
                 nims, nime, 1, 1, njms, njme,   &         ! ND dims
                 nips, nipe, 1, 1, njps, njpe,   &         ! ND dims
                  config_flags%shw, ngrid%imask_ystag,         &         ! stencil half width
                  .FALSE., .TRUE.,                                                &         ! xstag, ystag
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%msftx, 1 ) * SIZE( grid%msftx, 2 ) .GT. 1 ) THEN 
CALL copy_fcnm (  &         
                  grid%msftx,   &       ! CD field
                 cids, cide, 1, 1, cjds, cjde,   &         ! CD dims
                 cims, cime, 1, 1, cjms, cjme,   &         ! CD dims
                 cips, cipe, 1, 1, cjps, cjpe,   &         ! CD dims
                  ngrid%msftx,  &   ! ND field
                 nids, nide, 1, 1, njds, njde,   &         ! ND dims
                 nims, nime, 1, 1, njms, njme,   &         ! ND dims
                 nips, nipe, 1, 1, njps, njpe,   &         ! ND dims
                  config_flags%shw, ngrid%imask_nostag,         &         ! stencil half width
                  .FALSE., .FALSE.,                                                &         ! xstag, ystag
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%msfty, 1 ) * SIZE( grid%msfty, 2 ) .GT. 1 ) THEN 
CALL copy_fcnm (  &         
                  grid%msfty,   &       ! CD field
                 cids, cide, 1, 1, cjds, cjde,   &         ! CD dims
                 cims, cime, 1, 1, cjms, cjme,   &         ! CD dims
                 cips, cipe, 1, 1, cjps, cjpe,   &         ! CD dims
                  ngrid%msfty,  &   ! ND field
                 nids, nide, 1, 1, njds, njde,   &         ! ND dims
                 nims, nime, 1, 1, njms, njme,   &         ! ND dims
                 nips, nipe, 1, 1, njps, njpe,   &         ! ND dims
                  config_flags%shw, ngrid%imask_nostag,         &         ! stencil half width
                  .FALSE., .FALSE.,                                                &         ! xstag, ystag
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%msfux, 1 ) * SIZE( grid%msfux, 2 ) .GT. 1 ) THEN 
CALL copy_fcnm (  &         
                  grid%msfux,   &       ! CD field
                 cids, cide, 1, 1, cjds, cjde,   &         ! CD dims
                 cims, cime, 1, 1, cjms, cjme,   &         ! CD dims
                 cips, cipe, 1, 1, cjps, cjpe,   &         ! CD dims
                  ngrid%msfux,  &   ! ND field
                 nids, nide, 1, 1, njds, njde,   &         ! ND dims
                 nims, nime, 1, 1, njms, njme,   &         ! ND dims
                 nips, nipe, 1, 1, njps, njpe,   &         ! ND dims
                  config_flags%shw, ngrid%imask_xstag,         &         ! stencil half width
                  .TRUE., .FALSE.,                                                &         ! xstag, ystag
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%msfuy, 1 ) * SIZE( grid%msfuy, 2 ) .GT. 1 ) THEN 
CALL copy_fcnm (  &         
                  grid%msfuy,   &       ! CD field
                 cids, cide, 1, 1, cjds, cjde,   &         ! CD dims
                 cims, cime, 1, 1, cjms, cjme,   &         ! CD dims
                 cips, cipe, 1, 1, cjps, cjpe,   &         ! CD dims
                  ngrid%msfuy,  &   ! ND field
                 nids, nide, 1, 1, njds, njde,   &         ! ND dims
                 nims, nime, 1, 1, njms, njme,   &         ! ND dims
                 nips, nipe, 1, 1, njps, njpe,   &         ! ND dims
                  config_flags%shw, ngrid%imask_xstag,         &         ! stencil half width
                  .TRUE., .FALSE.,                                                &         ! xstag, ystag
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%msfvx, 1 ) * SIZE( grid%msfvx, 2 ) .GT. 1 ) THEN 
CALL copy_fcnm (  &         
                  grid%msfvx,   &       ! CD field
                 cids, cide, 1, 1, cjds, cjde,   &         ! CD dims
                 cims, cime, 1, 1, cjms, cjme,   &         ! CD dims
                 cips, cipe, 1, 1, cjps, cjpe,   &         ! CD dims
                  ngrid%msfvx,  &   ! ND field
                 nids, nide, 1, 1, njds, njde,   &         ! ND dims
                 nims, nime, 1, 1, njms, njme,   &         ! ND dims
                 nips, nipe, 1, 1, njps, njpe,   &         ! ND dims
                  config_flags%shw, ngrid%imask_ystag,         &         ! stencil half width
                  .FALSE., .TRUE.,                                                &         ! xstag, ystag
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%msfvx_inv, 1 ) * SIZE( grid%msfvx_inv, 2 ) .GT. 1 ) THEN 
CALL copy_fcnm (  &         
                  grid%msfvx_inv,   &       ! CD field
                 cids, cide, 1, 1, cjds, cjde,   &         ! CD dims
                 cims, cime, 1, 1, cjms, cjme,   &         ! CD dims
                 cips, cipe, 1, 1, cjps, cjpe,   &         ! CD dims
                  ngrid%msfvx_inv,  &   ! ND field
                 nids, nide, 1, 1, njds, njde,   &         ! ND dims
                 nims, nime, 1, 1, njms, njme,   &         ! ND dims
                 nips, nipe, 1, 1, njps, njpe,   &         ! ND dims
                  config_flags%shw, ngrid%imask_ystag,         &         ! stencil half width
                  .FALSE., .TRUE.,                                                &         ! xstag, ystag
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%msfvy, 1 ) * SIZE( grid%msfvy, 2 ) .GT. 1 ) THEN 
CALL copy_fcnm (  &         
                  grid%msfvy,   &       ! CD field
                 cids, cide, 1, 1, cjds, cjde,   &         ! CD dims
                 cims, cime, 1, 1, cjms, cjme,   &         ! CD dims
                 cips, cipe, 1, 1, cjps, cjpe,   &         ! CD dims
                  ngrid%msfvy,  &   ! ND field
                 nids, nide, 1, 1, njds, njde,   &         ! ND dims
                 nims, nime, 1, 1, njms, njme,   &         ! ND dims
                 nips, nipe, 1, 1, njps, njpe,   &         ! ND dims
                  config_flags%shw, ngrid%imask_ystag,         &         ! stencil half width
                  .FALSE., .TRUE.,                                                &         ! xstag, ystag
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%f, 1 ) * SIZE( grid%f, 2 ) .GT. 1 ) THEN 
CALL copy_fcnm (  &         
                  grid%f,   &       ! CD field
                 cids, cide, 1, 1, cjds, cjde,   &         ! CD dims
                 cims, cime, 1, 1, cjms, cjme,   &         ! CD dims
                 cips, cipe, 1, 1, cjps, cjpe,   &         ! CD dims
                  ngrid%f,  &   ! ND field
                 nids, nide, 1, 1, njds, njde,   &         ! ND dims
                 nims, nime, 1, 1, njms, njme,   &         ! ND dims
                 nips, nipe, 1, 1, njps, njpe,   &         ! ND dims
                  config_flags%shw, ngrid%imask_nostag,         &         ! stencil half width
                  .FALSE., .FALSE.,                                                &         ! xstag, ystag
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%e, 1 ) * SIZE( grid%e, 2 ) .GT. 1 ) THEN 
CALL copy_fcnm (  &         
                  grid%e,   &       ! CD field
                 cids, cide, 1, 1, cjds, cjde,   &         ! CD dims
                 cims, cime, 1, 1, cjms, cjme,   &         ! CD dims
                 cips, cipe, 1, 1, cjps, cjpe,   &         ! CD dims
                  ngrid%e,  &   ! ND field
                 nids, nide, 1, 1, njds, njde,   &         ! ND dims
                 nims, nime, 1, 1, njms, njme,   &         ! ND dims
                 nips, nipe, 1, 1, njps, njpe,   &         ! ND dims
                  config_flags%shw, ngrid%imask_nostag,         &         ! stencil half width
                  .FALSE., .FALSE.,                                                &         ! xstag, ystag
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%sina, 1 ) * SIZE( grid%sina, 2 ) .GT. 1 ) THEN 
CALL copy_fcnm (  &         
                  grid%sina,   &       ! CD field
                 cids, cide, 1, 1, cjds, cjde,   &         ! CD dims
                 cims, cime, 1, 1, cjms, cjme,   &         ! CD dims
                 cips, cipe, 1, 1, cjps, cjpe,   &         ! CD dims
                  ngrid%sina,  &   ! ND field
                 nids, nide, 1, 1, njds, njde,   &         ! ND dims
                 nims, nime, 1, 1, njms, njme,   &         ! ND dims
                 nips, nipe, 1, 1, njps, njpe,   &         ! ND dims
                  config_flags%shw, ngrid%imask_nostag,         &         ! stencil half width
                  .FALSE., .FALSE.,                                                &         ! xstag, ystag
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%cosa, 1 ) * SIZE( grid%cosa, 2 ) .GT. 1 ) THEN 
CALL copy_fcnm (  &         
                  grid%cosa,   &       ! CD field
                 cids, cide, 1, 1, cjds, cjde,   &         ! CD dims
                 cims, cime, 1, 1, cjms, cjme,   &         ! CD dims
                 cips, cipe, 1, 1, cjps, cjpe,   &         ! CD dims
                  ngrid%cosa,  &   ! ND field
                 nids, nide, 1, 1, njds, njde,   &         ! ND dims
                 nims, nime, 1, 1, njms, njme,   &         ! ND dims
                 nips, nipe, 1, 1, njps, njpe,   &         ! ND dims
                  config_flags%shw, ngrid%imask_nostag,         &         ! stencil half width
                  .FALSE., .FALSE.,                                                &         ! xstag, ystag
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%ht, 1 ) * SIZE( grid%ht, 2 ) .GT. 1 ) THEN 
CALL copy_fcn (  &         
                  grid%ht,   &       ! CD field
                 cids, cide, 1, 1, cjds, cjde,   &         ! CD dims
                 cims, cime, 1, 1, cjms, cjme,   &         ! CD dims
                 cips, cipe, 1, 1, cjps, cjpe,   &         ! CD dims
                  ngrid%ht,  &   ! ND field
                 nids, nide, 1, 1, njds, njde,   &         ! ND dims
                 nims, nime, 1, 1, njms, njme,   &         ! ND dims
                 nips, nipe, 1, 1, njps, njpe,   &         ! ND dims
                  config_flags%shw, ngrid%imask_nostag,         &         ! stencil half width
                  .FALSE., .FALSE.,                                                &         ! xstag, ystag
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%tsk, 1 ) * SIZE( grid%tsk, 2 ) .GT. 1 ) THEN 
CALL copy_fcnm (  &         
                  grid%tsk,   &       ! CD field
                 cids, cide, 1, 1, cjds, cjde,   &         ! CD dims
                 cims, cime, 1, 1, cjms, cjme,   &         ! CD dims
                 cips, cipe, 1, 1, cjps, cjpe,   &         ! CD dims
                  ngrid%tsk,  &   ! ND field
                 nids, nide, 1, 1, njds, njde,   &         ! ND dims
                 nims, nime, 1, 1, njms, njme,   &         ! ND dims
                 nips, nipe, 1, 1, njps, njpe,   &         ! ND dims
                  config_flags%shw, ngrid%imask_nostag,         &         ! stencil half width
                  .FALSE., .FALSE.,                                                &         ! xstag, ystag
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%rainc, 1 ) * SIZE( grid%rainc, 2 ) .GT. 1 ) THEN 
CALL copy_fcn (  &         
                  grid%rainc,   &       ! CD field
                 cids, cide, 1, 1, cjds, cjde,   &         ! CD dims
                 cims, cime, 1, 1, cjms, cjme,   &         ! CD dims
                 cips, cipe, 1, 1, cjps, cjpe,   &         ! CD dims
                  ngrid%rainc,  &   ! ND field
                 nids, nide, 1, 1, njds, njde,   &         ! ND dims
                 nims, nime, 1, 1, njms, njme,   &         ! ND dims
                 nips, nipe, 1, 1, njps, njpe,   &         ! ND dims
                  config_flags%shw, ngrid%imask_nostag,         &         ! stencil half width
                  .FALSE., .FALSE.,                                                &         ! xstag, ystag
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%rainsh, 1 ) * SIZE( grid%rainsh, 2 ) .GT. 1 ) THEN 
CALL copy_fcn (  &         
                  grid%rainsh,   &       ! CD field
                 cids, cide, 1, 1, cjds, cjde,   &         ! CD dims
                 cims, cime, 1, 1, cjms, cjme,   &         ! CD dims
                 cips, cipe, 1, 1, cjps, cjpe,   &         ! CD dims
                  ngrid%rainsh,  &   ! ND field
                 nids, nide, 1, 1, njds, njde,   &         ! ND dims
                 nims, nime, 1, 1, njms, njme,   &         ! ND dims
                 nips, nipe, 1, 1, njps, njpe,   &         ! ND dims
                  config_flags%shw, ngrid%imask_nostag,         &         ! stencil half width
                  .FALSE., .FALSE.,                                                &         ! xstag, ystag
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%rainnc, 1 ) * SIZE( grid%rainnc, 2 ) .GT. 1 ) THEN 
CALL copy_fcn (  &         
                  grid%rainnc,   &       ! CD field
                 cids, cide, 1, 1, cjds, cjde,   &         ! CD dims
                 cims, cime, 1, 1, cjms, cjme,   &         ! CD dims
                 cips, cipe, 1, 1, cjps, cjpe,   &         ! CD dims
                  ngrid%rainnc,  &   ! ND field
                 nids, nide, 1, 1, njds, njde,   &         ! ND dims
                 nims, nime, 1, 1, njms, njme,   &         ! ND dims
                 nips, nipe, 1, 1, njps, njpe,   &         ! ND dims
                  config_flags%shw, ngrid%imask_nostag,         &         ! stencil half width
                  .FALSE., .FALSE.,                                                &         ! xstag, ystag
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%i_rainc, 1 ) * SIZE( grid%i_rainc, 2 ) .GT. 1 ) THEN 
CALL copy_fcni (  &         
                  grid%i_rainc,   &       ! CD field
                 cids, cide, 1, 1, cjds, cjde,   &         ! CD dims
                 cims, cime, 1, 1, cjms, cjme,   &         ! CD dims
                 cips, cipe, 1, 1, cjps, cjpe,   &         ! CD dims
                  ngrid%i_rainc,  &   ! ND field
                 nids, nide, 1, 1, njds, njde,   &         ! ND dims
                 nims, nime, 1, 1, njms, njme,   &         ! ND dims
                 nips, nipe, 1, 1, njps, njpe,   &         ! ND dims
                  config_flags%shw, ngrid%imask_nostag,         &         ! stencil half width
                  .FALSE., .FALSE.,                                                &         ! xstag, ystag
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%i_rainnc, 1 ) * SIZE( grid%i_rainnc, 2 ) .GT. 1 ) THEN 
CALL copy_fcni (  &         
                  grid%i_rainnc,   &       ! CD field
                 cids, cide, 1, 1, cjds, cjde,   &         ! CD dims
                 cims, cime, 1, 1, cjms, cjme,   &         ! CD dims
                 cips, cipe, 1, 1, cjps, cjpe,   &         ! CD dims
                  ngrid%i_rainnc,  &   ! ND field
                 nids, nide, 1, 1, njds, njde,   &         ! ND dims
                 nims, nime, 1, 1, njms, njme,   &         ! ND dims
                 nips, nipe, 1, 1, njps, njpe,   &         ! ND dims
                  config_flags%shw, ngrid%imask_nostag,         &         ! stencil half width
                  .FALSE., .FALSE.,                                                &         ! xstag, ystag
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%snownc, 1 ) * SIZE( grid%snownc, 2 ) .GT. 1 ) THEN 
CALL copy_fcn (  &         
                  grid%snownc,   &       ! CD field
                 cids, cide, 1, 1, cjds, cjde,   &         ! CD dims
                 cims, cime, 1, 1, cjms, cjme,   &         ! CD dims
                 cips, cipe, 1, 1, cjps, cjpe,   &         ! CD dims
                  ngrid%snownc,  &   ! ND field
                 nids, nide, 1, 1, njds, njde,   &         ! ND dims
                 nims, nime, 1, 1, njms, njme,   &         ! ND dims
                 nips, nipe, 1, 1, njps, njpe,   &         ! ND dims
                  config_flags%shw, ngrid%imask_nostag,         &         ! stencil half width
                  .FALSE., .FALSE.,                                                &         ! xstag, ystag
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%graupelnc, 1 ) * SIZE( grid%graupelnc, 2 ) .GT. 1 ) THEN 
CALL copy_fcn (  &         
                  grid%graupelnc,   &       ! CD field
                 cids, cide, 1, 1, cjds, cjde,   &         ! CD dims
                 cims, cime, 1, 1, cjms, cjme,   &         ! CD dims
                 cips, cipe, 1, 1, cjps, cjpe,   &         ! CD dims
                  ngrid%graupelnc,  &   ! ND field
                 nids, nide, 1, 1, njds, njde,   &         ! ND dims
                 nims, nime, 1, 1, njms, njme,   &         ! ND dims
                 nips, nipe, 1, 1, njps, njpe,   &         ! ND dims
                  config_flags%shw, ngrid%imask_nostag,         &         ! stencil half width
                  .FALSE., .FALSE.,                                                &         ! xstag, ystag
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%hailnc, 1 ) * SIZE( grid%hailnc, 2 ) .GT. 1 ) THEN 
CALL copy_fcn (  &         
                  grid%hailnc,   &       ! CD field
                 cids, cide, 1, 1, cjds, cjde,   &         ! CD dims
                 cims, cime, 1, 1, cjms, cjme,   &         ! CD dims
                 cips, cipe, 1, 1, cjps, cjpe,   &         ! CD dims
                  ngrid%hailnc,  &   ! ND field
                 nids, nide, 1, 1, njds, njde,   &         ! ND dims
                 nims, nime, 1, 1, njms, njme,   &         ! ND dims
                 nips, nipe, 1, 1, njps, njpe,   &         ! ND dims
                  config_flags%shw, ngrid%imask_nostag,         &         ! stencil half width
                  .FALSE., .FALSE.,                                                &         ! xstag, ystag
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%refl_10cm, 1 ) * SIZE( grid%refl_10cm, 3 ) .GT. 1 ) THEN 
CALL copy_fcn (  &         
                  grid%refl_10cm,   &       ! CD field
                 cids, cide, ckds, ckde, cjds, cjde,   &         ! CD dims
                 cims, cime, ckms, ckme, cjms, cjme,   &         ! CD dims
                 cips, cipe, ckps, MIN( (ckde-1), ckpe ), cjps, cjpe,   &         ! CD dims
                  ngrid%refl_10cm,  &   ! ND field
                 nids, nide, nkds, nkde, njds, njde,   &         ! ND dims
                 nims, nime, nkms, nkme, njms, njme,   &         ! ND dims
                 nips, nipe, nkps, MIN( (nkde-1), nkpe ), njps, njpe,   &         ! ND dims
                  config_flags%shw, ngrid%imask_nostag,         &         ! stencil half width
                  .FALSE., .FALSE.,                                                &         ! xstag, ystag
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%acswupt, 1 ) * SIZE( grid%acswupt, 2 ) .GT. 1 ) THEN 
CALL copy_fcn (  &         
                  grid%acswupt,   &       ! CD field
                 cids, cide, 1, 1, cjds, cjde,   &         ! CD dims
                 cims, cime, 1, 1, cjms, cjme,   &         ! CD dims
                 cips, cipe, 1, 1, cjps, cjpe,   &         ! CD dims
                  ngrid%acswupt,  &   ! ND field
                 nids, nide, 1, 1, njds, njde,   &         ! ND dims
                 nims, nime, 1, 1, njms, njme,   &         ! ND dims
                 nips, nipe, 1, 1, njps, njpe,   &         ! ND dims
                  config_flags%shw, ngrid%imask_nostag,         &         ! stencil half width
                  .FALSE., .FALSE.,                                                &         ! xstag, ystag
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%acswuptc, 1 ) * SIZE( grid%acswuptc, 2 ) .GT. 1 ) THEN 
CALL copy_fcn (  &         
                  grid%acswuptc,   &       ! CD field
                 cids, cide, 1, 1, cjds, cjde,   &         ! CD dims
                 cims, cime, 1, 1, cjms, cjme,   &         ! CD dims
                 cips, cipe, 1, 1, cjps, cjpe,   &         ! CD dims
                  ngrid%acswuptc,  &   ! ND field
                 nids, nide, 1, 1, njds, njde,   &         ! ND dims
                 nims, nime, 1, 1, njms, njme,   &         ! ND dims
                 nips, nipe, 1, 1, njps, njpe,   &         ! ND dims
                  config_flags%shw, ngrid%imask_nostag,         &         ! stencil half width
                  .FALSE., .FALSE.,                                                &         ! xstag, ystag
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%acswdnt, 1 ) * SIZE( grid%acswdnt, 2 ) .GT. 1 ) THEN 
CALL copy_fcn (  &         
                  grid%acswdnt,   &       ! CD field
                 cids, cide, 1, 1, cjds, cjde,   &         ! CD dims
                 cims, cime, 1, 1, cjms, cjme,   &         ! CD dims
                 cips, cipe, 1, 1, cjps, cjpe,   &         ! CD dims
                  ngrid%acswdnt,  &   ! ND field
                 nids, nide, 1, 1, njds, njde,   &         ! ND dims
                 nims, nime, 1, 1, njms, njme,   &         ! ND dims
                 nips, nipe, 1, 1, njps, njpe,   &         ! ND dims
                  config_flags%shw, ngrid%imask_nostag,         &         ! stencil half width
                  .FALSE., .FALSE.,                                                &         ! xstag, ystag
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%acswdntc, 1 ) * SIZE( grid%acswdntc, 2 ) .GT. 1 ) THEN 
CALL copy_fcn (  &         
                  grid%acswdntc,   &       ! CD field
                 cids, cide, 1, 1, cjds, cjde,   &         ! CD dims
                 cims, cime, 1, 1, cjms, cjme,   &         ! CD dims
                 cips, cipe, 1, 1, cjps, cjpe,   &         ! CD dims
                  ngrid%acswdntc,  &   ! ND field
                 nids, nide, 1, 1, njds, njde,   &         ! ND dims
                 nims, nime, 1, 1, njms, njme,   &         ! ND dims
                 nips, nipe, 1, 1, njps, njpe,   &         ! ND dims
                  config_flags%shw, ngrid%imask_nostag,         &         ! stencil half width
                  .FALSE., .FALSE.,                                                &         ! xstag, ystag
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%acswupb, 1 ) * SIZE( grid%acswupb, 2 ) .GT. 1 ) THEN 
CALL copy_fcn (  &         
                  grid%acswupb,   &       ! CD field
                 cids, cide, 1, 1, cjds, cjde,   &         ! CD dims
                 cims, cime, 1, 1, cjms, cjme,   &         ! CD dims
                 cips, cipe, 1, 1, cjps, cjpe,   &         ! CD dims
                  ngrid%acswupb,  &   ! ND field
                 nids, nide, 1, 1, njds, njde,   &         ! ND dims
                 nims, nime, 1, 1, njms, njme,   &         ! ND dims
                 nips, nipe, 1, 1, njps, njpe,   &         ! ND dims
                  config_flags%shw, ngrid%imask_nostag,         &         ! stencil half width
                  .FALSE., .FALSE.,                                                &         ! xstag, ystag
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%acswupbc, 1 ) * SIZE( grid%acswupbc, 2 ) .GT. 1 ) THEN 
CALL copy_fcn (  &         
                  grid%acswupbc,   &       ! CD field
                 cids, cide, 1, 1, cjds, cjde,   &         ! CD dims
                 cims, cime, 1, 1, cjms, cjme,   &         ! CD dims
                 cips, cipe, 1, 1, cjps, cjpe,   &         ! CD dims
                  ngrid%acswupbc,  &   ! ND field
                 nids, nide, 1, 1, njds, njde,   &         ! ND dims
                 nims, nime, 1, 1, njms, njme,   &         ! ND dims
                 nips, nipe, 1, 1, njps, njpe,   &         ! ND dims
                  config_flags%shw, ngrid%imask_nostag,         &         ! stencil half width
                  .FALSE., .FALSE.,                                                &         ! xstag, ystag
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%acswdnb, 1 ) * SIZE( grid%acswdnb, 2 ) .GT. 1 ) THEN 
CALL copy_fcn (  &         
                  grid%acswdnb,   &       ! CD field
                 cids, cide, 1, 1, cjds, cjde,   &         ! CD dims
                 cims, cime, 1, 1, cjms, cjme,   &         ! CD dims
                 cips, cipe, 1, 1, cjps, cjpe,   &         ! CD dims
                  ngrid%acswdnb,  &   ! ND field
                 nids, nide, 1, 1, njds, njde,   &         ! ND dims
                 nims, nime, 1, 1, njms, njme,   &         ! ND dims
                 nips, nipe, 1, 1, njps, njpe,   &         ! ND dims
                  config_flags%shw, ngrid%imask_nostag,         &         ! stencil half width
                  .FALSE., .FALSE.,                                                &         ! xstag, ystag
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%acswdnbc, 1 ) * SIZE( grid%acswdnbc, 2 ) .GT. 1 ) THEN 
CALL copy_fcn (  &         
                  grid%acswdnbc,   &       ! CD field
                 cids, cide, 1, 1, cjds, cjde,   &         ! CD dims
                 cims, cime, 1, 1, cjms, cjme,   &         ! CD dims
                 cips, cipe, 1, 1, cjps, cjpe,   &         ! CD dims
                  ngrid%acswdnbc,  &   ! ND field
                 nids, nide, 1, 1, njds, njde,   &         ! ND dims
                 nims, nime, 1, 1, njms, njme,   &         ! ND dims
                 nips, nipe, 1, 1, njps, njpe,   &         ! ND dims
                  config_flags%shw, ngrid%imask_nostag,         &         ! stencil half width
                  .FALSE., .FALSE.,                                                &         ! xstag, ystag
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%aclwupt, 1 ) * SIZE( grid%aclwupt, 2 ) .GT. 1 ) THEN 
CALL copy_fcn (  &         
                  grid%aclwupt,   &       ! CD field
                 cids, cide, 1, 1, cjds, cjde,   &         ! CD dims
                 cims, cime, 1, 1, cjms, cjme,   &         ! CD dims
                 cips, cipe, 1, 1, cjps, cjpe,   &         ! CD dims
                  ngrid%aclwupt,  &   ! ND field
                 nids, nide, 1, 1, njds, njde,   &         ! ND dims
                 nims, nime, 1, 1, njms, njme,   &         ! ND dims
                 nips, nipe, 1, 1, njps, njpe,   &         ! ND dims
                  config_flags%shw, ngrid%imask_nostag,         &         ! stencil half width
                  .FALSE., .FALSE.,                                                &         ! xstag, ystag
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%aclwuptc, 1 ) * SIZE( grid%aclwuptc, 2 ) .GT. 1 ) THEN 
CALL copy_fcn (  &         
                  grid%aclwuptc,   &       ! CD field
                 cids, cide, 1, 1, cjds, cjde,   &         ! CD dims
                 cims, cime, 1, 1, cjms, cjme,   &         ! CD dims
                 cips, cipe, 1, 1, cjps, cjpe,   &         ! CD dims
                  ngrid%aclwuptc,  &   ! ND field
                 nids, nide, 1, 1, njds, njde,   &         ! ND dims
                 nims, nime, 1, 1, njms, njme,   &         ! ND dims
                 nips, nipe, 1, 1, njps, njpe,   &         ! ND dims
                  config_flags%shw, ngrid%imask_nostag,         &         ! stencil half width
                  .FALSE., .FALSE.,                                                &         ! xstag, ystag
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%aclwdnt, 1 ) * SIZE( grid%aclwdnt, 2 ) .GT. 1 ) THEN 
CALL copy_fcn (  &         
                  grid%aclwdnt,   &       ! CD field
                 cids, cide, 1, 1, cjds, cjde,   &         ! CD dims
                 cims, cime, 1, 1, cjms, cjme,   &         ! CD dims
                 cips, cipe, 1, 1, cjps, cjpe,   &         ! CD dims
                  ngrid%aclwdnt,  &   ! ND field
                 nids, nide, 1, 1, njds, njde,   &         ! ND dims
                 nims, nime, 1, 1, njms, njme,   &         ! ND dims
                 nips, nipe, 1, 1, njps, njpe,   &         ! ND dims
                  config_flags%shw, ngrid%imask_nostag,         &         ! stencil half width
                  .FALSE., .FALSE.,                                                &         ! xstag, ystag
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%aclwdntc, 1 ) * SIZE( grid%aclwdntc, 2 ) .GT. 1 ) THEN 
CALL copy_fcn (  &         
                  grid%aclwdntc,   &       ! CD field
                 cids, cide, 1, 1, cjds, cjde,   &         ! CD dims
                 cims, cime, 1, 1, cjms, cjme,   &         ! CD dims
                 cips, cipe, 1, 1, cjps, cjpe,   &         ! CD dims
                  ngrid%aclwdntc,  &   ! ND field
                 nids, nide, 1, 1, njds, njde,   &         ! ND dims
                 nims, nime, 1, 1, njms, njme,   &         ! ND dims
                 nips, nipe, 1, 1, njps, njpe,   &         ! ND dims
                  config_flags%shw, ngrid%imask_nostag,         &         ! stencil half width
                  .FALSE., .FALSE.,                                                &         ! xstag, ystag
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%aclwupb, 1 ) * SIZE( grid%aclwupb, 2 ) .GT. 1 ) THEN 
CALL copy_fcn (  &         
                  grid%aclwupb,   &       ! CD field
                 cids, cide, 1, 1, cjds, cjde,   &         ! CD dims
                 cims, cime, 1, 1, cjms, cjme,   &         ! CD dims
                 cips, cipe, 1, 1, cjps, cjpe,   &         ! CD dims
                  ngrid%aclwupb,  &   ! ND field
                 nids, nide, 1, 1, njds, njde,   &         ! ND dims
                 nims, nime, 1, 1, njms, njme,   &         ! ND dims
                 nips, nipe, 1, 1, njps, njpe,   &         ! ND dims
                  config_flags%shw, ngrid%imask_nostag,         &         ! stencil half width
                  .FALSE., .FALSE.,                                                &         ! xstag, ystag
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%aclwupbc, 1 ) * SIZE( grid%aclwupbc, 2 ) .GT. 1 ) THEN 
CALL copy_fcn (  &         
                  grid%aclwupbc,   &       ! CD field
                 cids, cide, 1, 1, cjds, cjde,   &         ! CD dims
                 cims, cime, 1, 1, cjms, cjme,   &         ! CD dims
                 cips, cipe, 1, 1, cjps, cjpe,   &         ! CD dims
                  ngrid%aclwupbc,  &   ! ND field
                 nids, nide, 1, 1, njds, njde,   &         ! ND dims
                 nims, nime, 1, 1, njms, njme,   &         ! ND dims
                 nips, nipe, 1, 1, njps, njpe,   &         ! ND dims
                  config_flags%shw, ngrid%imask_nostag,         &         ! stencil half width
                  .FALSE., .FALSE.,                                                &         ! xstag, ystag
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%aclwdnb, 1 ) * SIZE( grid%aclwdnb, 2 ) .GT. 1 ) THEN 
CALL copy_fcn (  &         
                  grid%aclwdnb,   &       ! CD field
                 cids, cide, 1, 1, cjds, cjde,   &         ! CD dims
                 cims, cime, 1, 1, cjms, cjme,   &         ! CD dims
                 cips, cipe, 1, 1, cjps, cjpe,   &         ! CD dims
                  ngrid%aclwdnb,  &   ! ND field
                 nids, nide, 1, 1, njds, njde,   &         ! ND dims
                 nims, nime, 1, 1, njms, njme,   &         ! ND dims
                 nips, nipe, 1, 1, njps, njpe,   &         ! ND dims
                  config_flags%shw, ngrid%imask_nostag,         &         ! stencil half width
                  .FALSE., .FALSE.,                                                &         ! xstag, ystag
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%aclwdnbc, 1 ) * SIZE( grid%aclwdnbc, 2 ) .GT. 1 ) THEN 
CALL copy_fcn (  &         
                  grid%aclwdnbc,   &       ! CD field
                 cids, cide, 1, 1, cjds, cjde,   &         ! CD dims
                 cims, cime, 1, 1, cjms, cjme,   &         ! CD dims
                 cips, cipe, 1, 1, cjps, cjpe,   &         ! CD dims
                  ngrid%aclwdnbc,  &   ! ND field
                 nids, nide, 1, 1, njds, njde,   &         ! ND dims
                 nims, nime, 1, 1, njms, njme,   &         ! ND dims
                 nips, nipe, 1, 1, njps, njpe,   &         ! ND dims
                  config_flags%shw, ngrid%imask_nostag,         &         ! stencil half width
                  .FALSE., .FALSE.,                                                &         ! xstag, ystag
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%i_acswupt, 1 ) * SIZE( grid%i_acswupt, 2 ) .GT. 1 ) THEN 
CALL copy_fcni (  &         
                  grid%i_acswupt,   &       ! CD field
                 cids, cide, 1, 1, cjds, cjde,   &         ! CD dims
                 cims, cime, 1, 1, cjms, cjme,   &         ! CD dims
                 cips, cipe, 1, 1, cjps, cjpe,   &         ! CD dims
                  ngrid%i_acswupt,  &   ! ND field
                 nids, nide, 1, 1, njds, njde,   &         ! ND dims
                 nims, nime, 1, 1, njms, njme,   &         ! ND dims
                 nips, nipe, 1, 1, njps, njpe,   &         ! ND dims
                  config_flags%shw, ngrid%imask_nostag,         &         ! stencil half width
                  .FALSE., .FALSE.,                                                &         ! xstag, ystag
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%i_acswuptc, 1 ) * SIZE( grid%i_acswuptc, 2 ) .GT. 1 ) THEN 
CALL copy_fcni (  &         
                  grid%i_acswuptc,   &       ! CD field
                 cids, cide, 1, 1, cjds, cjde,   &         ! CD dims
                 cims, cime, 1, 1, cjms, cjme,   &         ! CD dims
                 cips, cipe, 1, 1, cjps, cjpe,   &         ! CD dims
                  ngrid%i_acswuptc,  &   ! ND field
                 nids, nide, 1, 1, njds, njde,   &         ! ND dims
                 nims, nime, 1, 1, njms, njme,   &         ! ND dims
                 nips, nipe, 1, 1, njps, njpe,   &         ! ND dims
                  config_flags%shw, ngrid%imask_nostag,         &         ! stencil half width
                  .FALSE., .FALSE.,                                                &         ! xstag, ystag
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%i_acswdnt, 1 ) * SIZE( grid%i_acswdnt, 2 ) .GT. 1 ) THEN 
CALL copy_fcni (  &         
                  grid%i_acswdnt,   &       ! CD field
                 cids, cide, 1, 1, cjds, cjde,   &         ! CD dims
                 cims, cime, 1, 1, cjms, cjme,   &         ! CD dims
                 cips, cipe, 1, 1, cjps, cjpe,   &         ! CD dims
                  ngrid%i_acswdnt,  &   ! ND field
                 nids, nide, 1, 1, njds, njde,   &         ! ND dims
                 nims, nime, 1, 1, njms, njme,   &         ! ND dims
                 nips, nipe, 1, 1, njps, njpe,   &         ! ND dims
                  config_flags%shw, ngrid%imask_nostag,         &         ! stencil half width
                  .FALSE., .FALSE.,                                                &         ! xstag, ystag
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%i_acswdntc, 1 ) * SIZE( grid%i_acswdntc, 2 ) .GT. 1 ) THEN 
CALL copy_fcni (  &         
                  grid%i_acswdntc,   &       ! CD field
                 cids, cide, 1, 1, cjds, cjde,   &         ! CD dims
                 cims, cime, 1, 1, cjms, cjme,   &         ! CD dims
                 cips, cipe, 1, 1, cjps, cjpe,   &         ! CD dims
                  ngrid%i_acswdntc,  &   ! ND field
                 nids, nide, 1, 1, njds, njde,   &         ! ND dims
                 nims, nime, 1, 1, njms, njme,   &         ! ND dims
                 nips, nipe, 1, 1, njps, njpe,   &         ! ND dims
                  config_flags%shw, ngrid%imask_nostag,         &         ! stencil half width
                  .FALSE., .FALSE.,                                                &         ! xstag, ystag
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%i_acswupb, 1 ) * SIZE( grid%i_acswupb, 2 ) .GT. 1 ) THEN 
CALL copy_fcni (  &         
                  grid%i_acswupb,   &       ! CD field
                 cids, cide, 1, 1, cjds, cjde,   &         ! CD dims
                 cims, cime, 1, 1, cjms, cjme,   &         ! CD dims
                 cips, cipe, 1, 1, cjps, cjpe,   &         ! CD dims
                  ngrid%i_acswupb,  &   ! ND field
                 nids, nide, 1, 1, njds, njde,   &         ! ND dims
                 nims, nime, 1, 1, njms, njme,   &         ! ND dims
                 nips, nipe, 1, 1, njps, njpe,   &         ! ND dims
                  config_flags%shw, ngrid%imask_nostag,         &         ! stencil half width
                  .FALSE., .FALSE.,                                                &         ! xstag, ystag
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%i_acswupbc, 1 ) * SIZE( grid%i_acswupbc, 2 ) .GT. 1 ) THEN 
CALL copy_fcni (  &         
                  grid%i_acswupbc,   &       ! CD field
                 cids, cide, 1, 1, cjds, cjde,   &         ! CD dims
                 cims, cime, 1, 1, cjms, cjme,   &         ! CD dims
                 cips, cipe, 1, 1, cjps, cjpe,   &         ! CD dims
                  ngrid%i_acswupbc,  &   ! ND field
                 nids, nide, 1, 1, njds, njde,   &         ! ND dims
                 nims, nime, 1, 1, njms, njme,   &         ! ND dims
                 nips, nipe, 1, 1, njps, njpe,   &         ! ND dims
                  config_flags%shw, ngrid%imask_nostag,         &         ! stencil half width
                  .FALSE., .FALSE.,                                                &         ! xstag, ystag
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%i_acswdnb, 1 ) * SIZE( grid%i_acswdnb, 2 ) .GT. 1 ) THEN 
CALL copy_fcni (  &         
                  grid%i_acswdnb,   &       ! CD field
                 cids, cide, 1, 1, cjds, cjde,   &         ! CD dims
                 cims, cime, 1, 1, cjms, cjme,   &         ! CD dims
                 cips, cipe, 1, 1, cjps, cjpe,   &         ! CD dims
                  ngrid%i_acswdnb,  &   ! ND field
                 nids, nide, 1, 1, njds, njde,   &         ! ND dims
                 nims, nime, 1, 1, njms, njme,   &         ! ND dims
                 nips, nipe, 1, 1, njps, njpe,   &         ! ND dims
                  config_flags%shw, ngrid%imask_nostag,         &         ! stencil half width
                  .FALSE., .FALSE.,                                                &         ! xstag, ystag
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%i_acswdnbc, 1 ) * SIZE( grid%i_acswdnbc, 2 ) .GT. 1 ) THEN 
CALL copy_fcni (  &         
                  grid%i_acswdnbc,   &       ! CD field
                 cids, cide, 1, 1, cjds, cjde,   &         ! CD dims
                 cims, cime, 1, 1, cjms, cjme,   &         ! CD dims
                 cips, cipe, 1, 1, cjps, cjpe,   &         ! CD dims
                  ngrid%i_acswdnbc,  &   ! ND field
                 nids, nide, 1, 1, njds, njde,   &         ! ND dims
                 nims, nime, 1, 1, njms, njme,   &         ! ND dims
                 nips, nipe, 1, 1, njps, njpe,   &         ! ND dims
                  config_flags%shw, ngrid%imask_nostag,         &         ! stencil half width
                  .FALSE., .FALSE.,                                                &         ! xstag, ystag
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%i_aclwupt, 1 ) * SIZE( grid%i_aclwupt, 2 ) .GT. 1 ) THEN 
CALL copy_fcni (  &         
                  grid%i_aclwupt,   &       ! CD field
                 cids, cide, 1, 1, cjds, cjde,   &         ! CD dims
                 cims, cime, 1, 1, cjms, cjme,   &         ! CD dims
                 cips, cipe, 1, 1, cjps, cjpe,   &         ! CD dims
                  ngrid%i_aclwupt,  &   ! ND field
                 nids, nide, 1, 1, njds, njde,   &         ! ND dims
                 nims, nime, 1, 1, njms, njme,   &         ! ND dims
                 nips, nipe, 1, 1, njps, njpe,   &         ! ND dims
                  config_flags%shw, ngrid%imask_nostag,         &         ! stencil half width
                  .FALSE., .FALSE.,                                                &         ! xstag, ystag
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%i_aclwuptc, 1 ) * SIZE( grid%i_aclwuptc, 2 ) .GT. 1 ) THEN 
CALL copy_fcni (  &         
                  grid%i_aclwuptc,   &       ! CD field
                 cids, cide, 1, 1, cjds, cjde,   &         ! CD dims
                 cims, cime, 1, 1, cjms, cjme,   &         ! CD dims
                 cips, cipe, 1, 1, cjps, cjpe,   &         ! CD dims
                  ngrid%i_aclwuptc,  &   ! ND field
                 nids, nide, 1, 1, njds, njde,   &         ! ND dims
                 nims, nime, 1, 1, njms, njme,   &         ! ND dims
                 nips, nipe, 1, 1, njps, njpe,   &         ! ND dims
                  config_flags%shw, ngrid%imask_nostag,         &         ! stencil half width
                  .FALSE., .FALSE.,                                                &         ! xstag, ystag
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%i_aclwdnt, 1 ) * SIZE( grid%i_aclwdnt, 2 ) .GT. 1 ) THEN 
CALL copy_fcni (  &         
                  grid%i_aclwdnt,   &       ! CD field
                 cids, cide, 1, 1, cjds, cjde,   &         ! CD dims
                 cims, cime, 1, 1, cjms, cjme,   &         ! CD dims
                 cips, cipe, 1, 1, cjps, cjpe,   &         ! CD dims
                  ngrid%i_aclwdnt,  &   ! ND field
                 nids, nide, 1, 1, njds, njde,   &         ! ND dims
                 nims, nime, 1, 1, njms, njme,   &         ! ND dims
                 nips, nipe, 1, 1, njps, njpe,   &         ! ND dims
                  config_flags%shw, ngrid%imask_nostag,         &         ! stencil half width
                  .FALSE., .FALSE.,                                                &         ! xstag, ystag
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%i_aclwdntc, 1 ) * SIZE( grid%i_aclwdntc, 2 ) .GT. 1 ) THEN 
CALL copy_fcni (  &         
                  grid%i_aclwdntc,   &       ! CD field
                 cids, cide, 1, 1, cjds, cjde,   &         ! CD dims
                 cims, cime, 1, 1, cjms, cjme,   &         ! CD dims
                 cips, cipe, 1, 1, cjps, cjpe,   &         ! CD dims
                  ngrid%i_aclwdntc,  &   ! ND field
                 nids, nide, 1, 1, njds, njde,   &         ! ND dims
                 nims, nime, 1, 1, njms, njme,   &         ! ND dims
                 nips, nipe, 1, 1, njps, njpe,   &         ! ND dims
                  config_flags%shw, ngrid%imask_nostag,         &         ! stencil half width
                  .FALSE., .FALSE.,                                                &         ! xstag, ystag
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%i_aclwupb, 1 ) * SIZE( grid%i_aclwupb, 2 ) .GT. 1 ) THEN 
CALL copy_fcni (  &         
                  grid%i_aclwupb,   &       ! CD field
                 cids, cide, 1, 1, cjds, cjde,   &         ! CD dims
                 cims, cime, 1, 1, cjms, cjme,   &         ! CD dims
                 cips, cipe, 1, 1, cjps, cjpe,   &         ! CD dims
                  ngrid%i_aclwupb,  &   ! ND field
                 nids, nide, 1, 1, njds, njde,   &         ! ND dims
                 nims, nime, 1, 1, njms, njme,   &         ! ND dims
                 nips, nipe, 1, 1, njps, njpe,   &         ! ND dims
                  config_flags%shw, ngrid%imask_nostag,         &         ! stencil half width
                  .FALSE., .FALSE.,                                                &         ! xstag, ystag
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%i_aclwupbc, 1 ) * SIZE( grid%i_aclwupbc, 2 ) .GT. 1 ) THEN 
CALL copy_fcni (  &         
                  grid%i_aclwupbc,   &       ! CD field
                 cids, cide, 1, 1, cjds, cjde,   &         ! CD dims
                 cims, cime, 1, 1, cjms, cjme,   &         ! CD dims
                 cips, cipe, 1, 1, cjps, cjpe,   &         ! CD dims
                  ngrid%i_aclwupbc,  &   ! ND field
                 nids, nide, 1, 1, njds, njde,   &         ! ND dims
                 nims, nime, 1, 1, njms, njme,   &         ! ND dims
                 nips, nipe, 1, 1, njps, njpe,   &         ! ND dims
                  config_flags%shw, ngrid%imask_nostag,         &         ! stencil half width
                  .FALSE., .FALSE.,                                                &         ! xstag, ystag
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%i_aclwdnb, 1 ) * SIZE( grid%i_aclwdnb, 2 ) .GT. 1 ) THEN 
CALL copy_fcni (  &         
                  grid%i_aclwdnb,   &       ! CD field
                 cids, cide, 1, 1, cjds, cjde,   &         ! CD dims
                 cims, cime, 1, 1, cjms, cjme,   &         ! CD dims
                 cips, cipe, 1, 1, cjps, cjpe,   &         ! CD dims
                  ngrid%i_aclwdnb,  &   ! ND field
                 nids, nide, 1, 1, njds, njde,   &         ! ND dims
                 nims, nime, 1, 1, njms, njme,   &         ! ND dims
                 nips, nipe, 1, 1, njps, njpe,   &         ! ND dims
                  config_flags%shw, ngrid%imask_nostag,         &         ! stencil half width
                  .FALSE., .FALSE.,                                                &         ! xstag, ystag
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%i_aclwdnbc, 1 ) * SIZE( grid%i_aclwdnbc, 2 ) .GT. 1 ) THEN 
CALL copy_fcni (  &         
                  grid%i_aclwdnbc,   &       ! CD field
                 cids, cide, 1, 1, cjds, cjde,   &         ! CD dims
                 cims, cime, 1, 1, cjms, cjme,   &         ! CD dims
                 cips, cipe, 1, 1, cjps, cjpe,   &         ! CD dims
                  ngrid%i_aclwdnbc,  &   ! ND field
                 nids, nide, 1, 1, njds, njde,   &         ! ND dims
                 nims, nime, 1, 1, njms, njme,   &         ! ND dims
                 nips, nipe, 1, 1, njps, njpe,   &         ! ND dims
                  config_flags%shw, ngrid%imask_nostag,         &         ! stencil half width
                  .FALSE., .FALSE.,                                                &         ! xstag, ystag
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%swupt, 1 ) * SIZE( grid%swupt, 2 ) .GT. 1 ) THEN 
CALL copy_fcn (  &         
                  grid%swupt,   &       ! CD field
                 cids, cide, 1, 1, cjds, cjde,   &         ! CD dims
                 cims, cime, 1, 1, cjms, cjme,   &         ! CD dims
                 cips, cipe, 1, 1, cjps, cjpe,   &         ! CD dims
                  ngrid%swupt,  &   ! ND field
                 nids, nide, 1, 1, njds, njde,   &         ! ND dims
                 nims, nime, 1, 1, njms, njme,   &         ! ND dims
                 nips, nipe, 1, 1, njps, njpe,   &         ! ND dims
                  config_flags%shw, ngrid%imask_nostag,         &         ! stencil half width
                  .FALSE., .FALSE.,                                                &         ! xstag, ystag
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%swuptc, 1 ) * SIZE( grid%swuptc, 2 ) .GT. 1 ) THEN 
CALL copy_fcn (  &         
                  grid%swuptc,   &       ! CD field
                 cids, cide, 1, 1, cjds, cjde,   &         ! CD dims
                 cims, cime, 1, 1, cjms, cjme,   &         ! CD dims
                 cips, cipe, 1, 1, cjps, cjpe,   &         ! CD dims
                  ngrid%swuptc,  &   ! ND field
                 nids, nide, 1, 1, njds, njde,   &         ! ND dims
                 nims, nime, 1, 1, njms, njme,   &         ! ND dims
                 nips, nipe, 1, 1, njps, njpe,   &         ! ND dims
                  config_flags%shw, ngrid%imask_nostag,         &         ! stencil half width
                  .FALSE., .FALSE.,                                                &         ! xstag, ystag
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%swdnt, 1 ) * SIZE( grid%swdnt, 2 ) .GT. 1 ) THEN 
CALL copy_fcn (  &         
                  grid%swdnt,   &       ! CD field
                 cids, cide, 1, 1, cjds, cjde,   &         ! CD dims
                 cims, cime, 1, 1, cjms, cjme,   &         ! CD dims
                 cips, cipe, 1, 1, cjps, cjpe,   &         ! CD dims
                  ngrid%swdnt,  &   ! ND field
                 nids, nide, 1, 1, njds, njde,   &         ! ND dims
                 nims, nime, 1, 1, njms, njme,   &         ! ND dims
                 nips, nipe, 1, 1, njps, njpe,   &         ! ND dims
                  config_flags%shw, ngrid%imask_nostag,         &         ! stencil half width
                  .FALSE., .FALSE.,                                                &         ! xstag, ystag
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%swdntc, 1 ) * SIZE( grid%swdntc, 2 ) .GT. 1 ) THEN 
CALL copy_fcn (  &         
                  grid%swdntc,   &       ! CD field
                 cids, cide, 1, 1, cjds, cjde,   &         ! CD dims
                 cims, cime, 1, 1, cjms, cjme,   &         ! CD dims
                 cips, cipe, 1, 1, cjps, cjpe,   &         ! CD dims
                  ngrid%swdntc,  &   ! ND field
                 nids, nide, 1, 1, njds, njde,   &         ! ND dims
                 nims, nime, 1, 1, njms, njme,   &         ! ND dims
                 nips, nipe, 1, 1, njps, njpe,   &         ! ND dims
                  config_flags%shw, ngrid%imask_nostag,         &         ! stencil half width
                  .FALSE., .FALSE.,                                                &         ! xstag, ystag
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%swupb, 1 ) * SIZE( grid%swupb, 2 ) .GT. 1 ) THEN 
CALL copy_fcn (  &         
                  grid%swupb,   &       ! CD field
                 cids, cide, 1, 1, cjds, cjde,   &         ! CD dims
                 cims, cime, 1, 1, cjms, cjme,   &         ! CD dims
                 cips, cipe, 1, 1, cjps, cjpe,   &         ! CD dims
                  ngrid%swupb,  &   ! ND field
                 nids, nide, 1, 1, njds, njde,   &         ! ND dims
                 nims, nime, 1, 1, njms, njme,   &         ! ND dims
                 nips, nipe, 1, 1, njps, njpe,   &         ! ND dims
                  config_flags%shw, ngrid%imask_nostag,         &         ! stencil half width
                  .FALSE., .FALSE.,                                                &         ! xstag, ystag
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%swupbc, 1 ) * SIZE( grid%swupbc, 2 ) .GT. 1 ) THEN 
CALL copy_fcn (  &         
                  grid%swupbc,   &       ! CD field
                 cids, cide, 1, 1, cjds, cjde,   &         ! CD dims
                 cims, cime, 1, 1, cjms, cjme,   &         ! CD dims
                 cips, cipe, 1, 1, cjps, cjpe,   &         ! CD dims
                  ngrid%swupbc,  &   ! ND field
                 nids, nide, 1, 1, njds, njde,   &         ! ND dims
                 nims, nime, 1, 1, njms, njme,   &         ! ND dims
                 nips, nipe, 1, 1, njps, njpe,   &         ! ND dims
                  config_flags%shw, ngrid%imask_nostag,         &         ! stencil half width
                  .FALSE., .FALSE.,                                                &         ! xstag, ystag
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%swdnb, 1 ) * SIZE( grid%swdnb, 2 ) .GT. 1 ) THEN 
CALL copy_fcn (  &         
                  grid%swdnb,   &       ! CD field
                 cids, cide, 1, 1, cjds, cjde,   &         ! CD dims
                 cims, cime, 1, 1, cjms, cjme,   &         ! CD dims
                 cips, cipe, 1, 1, cjps, cjpe,   &         ! CD dims
                  ngrid%swdnb,  &   ! ND field
                 nids, nide, 1, 1, njds, njde,   &         ! ND dims
                 nims, nime, 1, 1, njms, njme,   &         ! ND dims
                 nips, nipe, 1, 1, njps, njpe,   &         ! ND dims
                  config_flags%shw, ngrid%imask_nostag,         &         ! stencil half width
                  .FALSE., .FALSE.,                                                &         ! xstag, ystag
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%swdnbc, 1 ) * SIZE( grid%swdnbc, 2 ) .GT. 1 ) THEN 
CALL copy_fcn (  &         
                  grid%swdnbc,   &       ! CD field
                 cids, cide, 1, 1, cjds, cjde,   &         ! CD dims
                 cims, cime, 1, 1, cjms, cjme,   &         ! CD dims
                 cips, cipe, 1, 1, cjps, cjpe,   &         ! CD dims
                  ngrid%swdnbc,  &   ! ND field
                 nids, nide, 1, 1, njds, njde,   &         ! ND dims
                 nims, nime, 1, 1, njms, njme,   &         ! ND dims
                 nips, nipe, 1, 1, njps, njpe,   &         ! ND dims
                  config_flags%shw, ngrid%imask_nostag,         &         ! stencil half width
                  .FALSE., .FALSE.,                                                &         ! xstag, ystag
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%lwupt, 1 ) * SIZE( grid%lwupt, 2 ) .GT. 1 ) THEN 
CALL copy_fcn (  &         
                  grid%lwupt,   &       ! CD field
                 cids, cide, 1, 1, cjds, cjde,   &         ! CD dims
                 cims, cime, 1, 1, cjms, cjme,   &         ! CD dims
                 cips, cipe, 1, 1, cjps, cjpe,   &         ! CD dims
                  ngrid%lwupt,  &   ! ND field
                 nids, nide, 1, 1, njds, njde,   &         ! ND dims
                 nims, nime, 1, 1, njms, njme,   &         ! ND dims
                 nips, nipe, 1, 1, njps, njpe,   &         ! ND dims
                  config_flags%shw, ngrid%imask_nostag,         &         ! stencil half width
                  .FALSE., .FALSE.,                                                &         ! xstag, ystag
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%lwuptc, 1 ) * SIZE( grid%lwuptc, 2 ) .GT. 1 ) THEN 
CALL copy_fcn (  &         
                  grid%lwuptc,   &       ! CD field
                 cids, cide, 1, 1, cjds, cjde,   &         ! CD dims
                 cims, cime, 1, 1, cjms, cjme,   &         ! CD dims
                 cips, cipe, 1, 1, cjps, cjpe,   &         ! CD dims
                  ngrid%lwuptc,  &   ! ND field
                 nids, nide, 1, 1, njds, njde,   &         ! ND dims
                 nims, nime, 1, 1, njms, njme,   &         ! ND dims
                 nips, nipe, 1, 1, njps, njpe,   &         ! ND dims
                  config_flags%shw, ngrid%imask_nostag,         &         ! stencil half width
                  .FALSE., .FALSE.,                                                &         ! xstag, ystag
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%lwdnt, 1 ) * SIZE( grid%lwdnt, 2 ) .GT. 1 ) THEN 
CALL copy_fcn (  &         
                  grid%lwdnt,   &       ! CD field
                 cids, cide, 1, 1, cjds, cjde,   &         ! CD dims
                 cims, cime, 1, 1, cjms, cjme,   &         ! CD dims
                 cips, cipe, 1, 1, cjps, cjpe,   &         ! CD dims
                  ngrid%lwdnt,  &   ! ND field
                 nids, nide, 1, 1, njds, njde,   &         ! ND dims
                 nims, nime, 1, 1, njms, njme,   &         ! ND dims
                 nips, nipe, 1, 1, njps, njpe,   &         ! ND dims
                  config_flags%shw, ngrid%imask_nostag,         &         ! stencil half width
                  .FALSE., .FALSE.,                                                &         ! xstag, ystag
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%lwdntc, 1 ) * SIZE( grid%lwdntc, 2 ) .GT. 1 ) THEN 
CALL copy_fcn (  &         
                  grid%lwdntc,   &       ! CD field
                 cids, cide, 1, 1, cjds, cjde,   &         ! CD dims
                 cims, cime, 1, 1, cjms, cjme,   &         ! CD dims
                 cips, cipe, 1, 1, cjps, cjpe,   &         ! CD dims
                  ngrid%lwdntc,  &   ! ND field
                 nids, nide, 1, 1, njds, njde,   &         ! ND dims
                 nims, nime, 1, 1, njms, njme,   &         ! ND dims
                 nips, nipe, 1, 1, njps, njpe,   &         ! ND dims
                  config_flags%shw, ngrid%imask_nostag,         &         ! stencil half width
                  .FALSE., .FALSE.,                                                &         ! xstag, ystag
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%lwupb, 1 ) * SIZE( grid%lwupb, 2 ) .GT. 1 ) THEN 
CALL copy_fcn (  &         
                  grid%lwupb,   &       ! CD field
                 cids, cide, 1, 1, cjds, cjde,   &         ! CD dims
                 cims, cime, 1, 1, cjms, cjme,   &         ! CD dims
                 cips, cipe, 1, 1, cjps, cjpe,   &         ! CD dims
                  ngrid%lwupb,  &   ! ND field
                 nids, nide, 1, 1, njds, njde,   &         ! ND dims
                 nims, nime, 1, 1, njms, njme,   &         ! ND dims
                 nips, nipe, 1, 1, njps, njpe,   &         ! ND dims
                  config_flags%shw, ngrid%imask_nostag,         &         ! stencil half width
                  .FALSE., .FALSE.,                                                &         ! xstag, ystag
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%lwupbc, 1 ) * SIZE( grid%lwupbc, 2 ) .GT. 1 ) THEN 
CALL copy_fcn (  &         
                  grid%lwupbc,   &       ! CD field
                 cids, cide, 1, 1, cjds, cjde,   &         ! CD dims
                 cims, cime, 1, 1, cjms, cjme,   &         ! CD dims
                 cips, cipe, 1, 1, cjps, cjpe,   &         ! CD dims
                  ngrid%lwupbc,  &   ! ND field
                 nids, nide, 1, 1, njds, njde,   &         ! ND dims
                 nims, nime, 1, 1, njms, njme,   &         ! ND dims
                 nips, nipe, 1, 1, njps, njpe,   &         ! ND dims
                  config_flags%shw, ngrid%imask_nostag,         &         ! stencil half width
                  .FALSE., .FALSE.,                                                &         ! xstag, ystag
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%lwdnb, 1 ) * SIZE( grid%lwdnb, 2 ) .GT. 1 ) THEN 
CALL copy_fcn (  &         
                  grid%lwdnb,   &       ! CD field
                 cids, cide, 1, 1, cjds, cjde,   &         ! CD dims
                 cims, cime, 1, 1, cjms, cjme,   &         ! CD dims
                 cips, cipe, 1, 1, cjps, cjpe,   &         ! CD dims
                  ngrid%lwdnb,  &   ! ND field
                 nids, nide, 1, 1, njds, njde,   &         ! ND dims
                 nims, nime, 1, 1, njms, njme,   &         ! ND dims
                 nips, nipe, 1, 1, njps, njpe,   &         ! ND dims
                  config_flags%shw, ngrid%imask_nostag,         &         ! stencil half width
                  .FALSE., .FALSE.,                                                &         ! xstag, ystag
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%lwdnbc, 1 ) * SIZE( grid%lwdnbc, 2 ) .GT. 1 ) THEN 
CALL copy_fcn (  &         
                  grid%lwdnbc,   &       ! CD field
                 cids, cide, 1, 1, cjds, cjde,   &         ! CD dims
                 cims, cime, 1, 1, cjms, cjme,   &         ! CD dims
                 cips, cipe, 1, 1, cjps, cjpe,   &         ! CD dims
                  ngrid%lwdnbc,  &   ! ND field
                 nids, nide, 1, 1, njds, njde,   &         ! ND dims
                 nims, nime, 1, 1, njms, njme,   &         ! ND dims
                 nips, nipe, 1, 1, njps, njpe,   &         ! ND dims
                  config_flags%shw, ngrid%imask_nostag,         &         ! stencil half width
                  .FALSE., .FALSE.,                                                &         ! xstag, ystag
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%xlat_u, 1 ) * SIZE( grid%xlat_u, 2 ) .GT. 1 ) THEN 
CALL copy_fcnm (  &         
                  grid%xlat_u,   &       ! CD field
                 cids, cide, 1, 1, cjds, cjde,   &         ! CD dims
                 cims, cime, 1, 1, cjms, cjme,   &         ! CD dims
                 cips, cipe, 1, 1, cjps, cjpe,   &         ! CD dims
                  ngrid%xlat_u,  &   ! ND field
                 nids, nide, 1, 1, njds, njde,   &         ! ND dims
                 nims, nime, 1, 1, njms, njme,   &         ! ND dims
                 nips, nipe, 1, 1, njps, njpe,   &         ! ND dims
                  config_flags%shw, ngrid%imask_xstag,         &         ! stencil half width
                  .TRUE., .FALSE.,                                                &         ! xstag, ystag
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%xlong_u, 1 ) * SIZE( grid%xlong_u, 2 ) .GT. 1 ) THEN 
CALL copy_fcnm (  &         
                  grid%xlong_u,   &       ! CD field
                 cids, cide, 1, 1, cjds, cjde,   &         ! CD dims
                 cims, cime, 1, 1, cjms, cjme,   &         ! CD dims
                 cips, cipe, 1, 1, cjps, cjpe,   &         ! CD dims
                  ngrid%xlong_u,  &   ! ND field
                 nids, nide, 1, 1, njds, njde,   &         ! ND dims
                 nims, nime, 1, 1, njms, njme,   &         ! ND dims
                 nips, nipe, 1, 1, njps, njpe,   &         ! ND dims
                  config_flags%shw, ngrid%imask_xstag,         &         ! stencil half width
                  .TRUE., .FALSE.,                                                &         ! xstag, ystag
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%xlat_v, 1 ) * SIZE( grid%xlat_v, 2 ) .GT. 1 ) THEN 
CALL copy_fcnm (  &         
                  grid%xlat_v,   &       ! CD field
                 cids, cide, 1, 1, cjds, cjde,   &         ! CD dims
                 cims, cime, 1, 1, cjms, cjme,   &         ! CD dims
                 cips, cipe, 1, 1, cjps, cjpe,   &         ! CD dims
                  ngrid%xlat_v,  &   ! ND field
                 nids, nide, 1, 1, njds, njde,   &         ! ND dims
                 nims, nime, 1, 1, njms, njme,   &         ! ND dims
                 nips, nipe, 1, 1, njps, njpe,   &         ! ND dims
                  config_flags%shw, ngrid%imask_ystag,         &         ! stencil half width
                  .FALSE., .TRUE.,                                                &         ! xstag, ystag
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%xlong_v, 1 ) * SIZE( grid%xlong_v, 2 ) .GT. 1 ) THEN 
CALL copy_fcnm (  &         
                  grid%xlong_v,   &       ! CD field
                 cids, cide, 1, 1, cjds, cjde,   &         ! CD dims
                 cims, cime, 1, 1, cjms, cjme,   &         ! CD dims
                 cips, cipe, 1, 1, cjps, cjpe,   &         ! CD dims
                  ngrid%xlong_v,  &   ! ND field
                 nids, nide, 1, 1, njds, njde,   &         ! ND dims
                 nims, nime, 1, 1, njms, njme,   &         ! ND dims
                 nips, nipe, 1, 1, njps, njpe,   &         ! ND dims
                  config_flags%shw, ngrid%imask_ystag,         &         ! stencil half width
                  .FALSE., .TRUE.,                                                &         ! xstag, ystag
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%clat, 1 ) * SIZE( grid%clat, 2 ) .GT. 1 ) THEN 
CALL copy_fcnm (  &         
                  grid%clat,   &       ! CD field
                 cids, cide, 1, 1, cjds, cjde,   &         ! CD dims
                 cims, cime, 1, 1, cjms, cjme,   &         ! CD dims
                 cips, cipe, 1, 1, cjps, cjpe,   &         ! CD dims
                  ngrid%clat,  &   ! ND field
                 nids, nide, 1, 1, njds, njde,   &         ! ND dims
                 nims, nime, 1, 1, njms, njme,   &         ! ND dims
                 nips, nipe, 1, 1, njps, njpe,   &         ! ND dims
                  config_flags%shw, ngrid%imask_nostag,         &         ! stencil half width
                  .FALSE., .FALSE.,                                                &         ! xstag, ystag
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%isnowxy, 1 ) * SIZE( grid%isnowxy, 2 ) .GT. 1 ) THEN 
CALL copy_fcnm (  &         
                  grid%isnowxy,   &       ! CD field
                 cids, cide, 1, 1, cjds, cjde,   &         ! CD dims
                 cims, cime, 1, 1, cjms, cjme,   &         ! CD dims
                 cips, cipe, 1, 1, cjps, cjpe,   &         ! CD dims
                  ngrid%isnowxy,  &   ! ND field
                 nids, nide, 1, 1, njds, njde,   &         ! ND dims
                 nims, nime, 1, 1, njms, njme,   &         ! ND dims
                 nips, nipe, 1, 1, njps, njpe,   &         ! ND dims
                  config_flags%shw, ngrid%imask_nostag,         &         ! stencil half width
                  .FALSE., .FALSE.,                                                &         ! xstag, ystag
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%tvxy, 1 ) * SIZE( grid%tvxy, 2 ) .GT. 1 ) THEN 
CALL copy_fcnm (  &         
                  grid%tvxy,   &       ! CD field
                 cids, cide, 1, 1, cjds, cjde,   &         ! CD dims
                 cims, cime, 1, 1, cjms, cjme,   &         ! CD dims
                 cips, cipe, 1, 1, cjps, cjpe,   &         ! CD dims
                  ngrid%tvxy,  &   ! ND field
                 nids, nide, 1, 1, njds, njde,   &         ! ND dims
                 nims, nime, 1, 1, njms, njme,   &         ! ND dims
                 nips, nipe, 1, 1, njps, njpe,   &         ! ND dims
                  config_flags%shw, ngrid%imask_nostag,         &         ! stencil half width
                  .FALSE., .FALSE.,                                                &         ! xstag, ystag
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%tgxy, 1 ) * SIZE( grid%tgxy, 2 ) .GT. 1 ) THEN 
CALL copy_fcnm (  &         
                  grid%tgxy,   &       ! CD field
                 cids, cide, 1, 1, cjds, cjde,   &         ! CD dims
                 cims, cime, 1, 1, cjms, cjme,   &         ! CD dims
                 cips, cipe, 1, 1, cjps, cjpe,   &         ! CD dims
                  ngrid%tgxy,  &   ! ND field
                 nids, nide, 1, 1, njds, njde,   &         ! ND dims
                 nims, nime, 1, 1, njms, njme,   &         ! ND dims
                 nips, nipe, 1, 1, njps, njpe,   &         ! ND dims
                  config_flags%shw, ngrid%imask_nostag,         &         ! stencil half width
                  .FALSE., .FALSE.,                                                &         ! xstag, ystag
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%canicexy, 1 ) * SIZE( grid%canicexy, 2 ) .GT. 1 ) THEN 
CALL copy_fcnm (  &         
                  grid%canicexy,   &       ! CD field
                 cids, cide, 1, 1, cjds, cjde,   &         ! CD dims
                 cims, cime, 1, 1, cjms, cjme,   &         ! CD dims
                 cips, cipe, 1, 1, cjps, cjpe,   &         ! CD dims
                  ngrid%canicexy,  &   ! ND field
                 nids, nide, 1, 1, njds, njde,   &         ! ND dims
                 nims, nime, 1, 1, njms, njme,   &         ! ND dims
                 nips, nipe, 1, 1, njps, njpe,   &         ! ND dims
                  config_flags%shw, ngrid%imask_nostag,         &         ! stencil half width
                  .FALSE., .FALSE.,                                                &         ! xstag, ystag
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%canliqxy, 1 ) * SIZE( grid%canliqxy, 2 ) .GT. 1 ) THEN 
CALL copy_fcnm (  &         
                  grid%canliqxy,   &       ! CD field
                 cids, cide, 1, 1, cjds, cjde,   &         ! CD dims
                 cims, cime, 1, 1, cjms, cjme,   &         ! CD dims
                 cips, cipe, 1, 1, cjps, cjpe,   &         ! CD dims
                  ngrid%canliqxy,  &   ! ND field
                 nids, nide, 1, 1, njds, njde,   &         ! ND dims
                 nims, nime, 1, 1, njms, njme,   &         ! ND dims
                 nips, nipe, 1, 1, njps, njpe,   &         ! ND dims
                  config_flags%shw, ngrid%imask_nostag,         &         ! stencil half width
                  .FALSE., .FALSE.,                                                &         ! xstag, ystag
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%eahxy, 1 ) * SIZE( grid%eahxy, 2 ) .GT. 1 ) THEN 
CALL copy_fcnm (  &         
                  grid%eahxy,   &       ! CD field
                 cids, cide, 1, 1, cjds, cjde,   &         ! CD dims
                 cims, cime, 1, 1, cjms, cjme,   &         ! CD dims
                 cips, cipe, 1, 1, cjps, cjpe,   &         ! CD dims
                  ngrid%eahxy,  &   ! ND field
                 nids, nide, 1, 1, njds, njde,   &         ! ND dims
                 nims, nime, 1, 1, njms, njme,   &         ! ND dims
                 nips, nipe, 1, 1, njps, njpe,   &         ! ND dims
                  config_flags%shw, ngrid%imask_nostag,         &         ! stencil half width
                  .FALSE., .FALSE.,                                                &         ! xstag, ystag
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%tahxy, 1 ) * SIZE( grid%tahxy, 2 ) .GT. 1 ) THEN 
CALL copy_fcnm (  &         
                  grid%tahxy,   &       ! CD field
                 cids, cide, 1, 1, cjds, cjde,   &         ! CD dims
                 cims, cime, 1, 1, cjms, cjme,   &         ! CD dims
                 cips, cipe, 1, 1, cjps, cjpe,   &         ! CD dims
                  ngrid%tahxy,  &   ! ND field
                 nids, nide, 1, 1, njds, njde,   &         ! ND dims
                 nims, nime, 1, 1, njms, njme,   &         ! ND dims
                 nips, nipe, 1, 1, njps, njpe,   &         ! ND dims
                  config_flags%shw, ngrid%imask_nostag,         &         ! stencil half width
                  .FALSE., .FALSE.,                                                &         ! xstag, ystag
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%cmxy, 1 ) * SIZE( grid%cmxy, 2 ) .GT. 1 ) THEN 
CALL copy_fcnm (  &         
                  grid%cmxy,   &       ! CD field
                 cids, cide, 1, 1, cjds, cjde,   &         ! CD dims
                 cims, cime, 1, 1, cjms, cjme,   &         ! CD dims
                 cips, cipe, 1, 1, cjps, cjpe,   &         ! CD dims
                  ngrid%cmxy,  &   ! ND field
                 nids, nide, 1, 1, njds, njde,   &         ! ND dims
                 nims, nime, 1, 1, njms, njme,   &         ! ND dims
                 nips, nipe, 1, 1, njps, njpe,   &         ! ND dims
                  config_flags%shw, ngrid%imask_nostag,         &         ! stencil half width
                  .FALSE., .FALSE.,                                                &         ! xstag, ystag
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%chxy, 1 ) * SIZE( grid%chxy, 2 ) .GT. 1 ) THEN 
CALL copy_fcnm (  &         
                  grid%chxy,   &       ! CD field
                 cids, cide, 1, 1, cjds, cjde,   &         ! CD dims
                 cims, cime, 1, 1, cjms, cjme,   &         ! CD dims
                 cips, cipe, 1, 1, cjps, cjpe,   &         ! CD dims
                  ngrid%chxy,  &   ! ND field
                 nids, nide, 1, 1, njds, njde,   &         ! ND dims
                 nims, nime, 1, 1, njms, njme,   &         ! ND dims
                 nips, nipe, 1, 1, njps, njpe,   &         ! ND dims
                  config_flags%shw, ngrid%imask_nostag,         &         ! stencil half width
                  .FALSE., .FALSE.,                                                &         ! xstag, ystag
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%fwetxy, 1 ) * SIZE( grid%fwetxy, 2 ) .GT. 1 ) THEN 
CALL copy_fcnm (  &         
                  grid%fwetxy,   &       ! CD field
                 cids, cide, 1, 1, cjds, cjde,   &         ! CD dims
                 cims, cime, 1, 1, cjms, cjme,   &         ! CD dims
                 cips, cipe, 1, 1, cjps, cjpe,   &         ! CD dims
                  ngrid%fwetxy,  &   ! ND field
                 nids, nide, 1, 1, njds, njde,   &         ! ND dims
                 nims, nime, 1, 1, njms, njme,   &         ! ND dims
                 nips, nipe, 1, 1, njps, njpe,   &         ! ND dims
                  config_flags%shw, ngrid%imask_nostag,         &         ! stencil half width
                  .FALSE., .FALSE.,                                                &         ! xstag, ystag
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%sneqvoxy, 1 ) * SIZE( grid%sneqvoxy, 2 ) .GT. 1 ) THEN 
CALL copy_fcnm (  &         
                  grid%sneqvoxy,   &       ! CD field
                 cids, cide, 1, 1, cjds, cjde,   &         ! CD dims
                 cims, cime, 1, 1, cjms, cjme,   &         ! CD dims
                 cips, cipe, 1, 1, cjps, cjpe,   &         ! CD dims
                  ngrid%sneqvoxy,  &   ! ND field
                 nids, nide, 1, 1, njds, njde,   &         ! ND dims
                 nims, nime, 1, 1, njms, njme,   &         ! ND dims
                 nips, nipe, 1, 1, njps, njpe,   &         ! ND dims
                  config_flags%shw, ngrid%imask_nostag,         &         ! stencil half width
                  .FALSE., .FALSE.,                                                &         ! xstag, ystag
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%alboldxy, 1 ) * SIZE( grid%alboldxy, 2 ) .GT. 1 ) THEN 
CALL copy_fcnm (  &         
                  grid%alboldxy,   &       ! CD field
                 cids, cide, 1, 1, cjds, cjde,   &         ! CD dims
                 cims, cime, 1, 1, cjms, cjme,   &         ! CD dims
                 cips, cipe, 1, 1, cjps, cjpe,   &         ! CD dims
                  ngrid%alboldxy,  &   ! ND field
                 nids, nide, 1, 1, njds, njde,   &         ! ND dims
                 nims, nime, 1, 1, njms, njme,   &         ! ND dims
                 nips, nipe, 1, 1, njps, njpe,   &         ! ND dims
                  config_flags%shw, ngrid%imask_nostag,         &         ! stencil half width
                  .FALSE., .FALSE.,                                                &         ! xstag, ystag
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%qsnowxy, 1 ) * SIZE( grid%qsnowxy, 2 ) .GT. 1 ) THEN 
CALL copy_fcnm (  &         
                  grid%qsnowxy,   &       ! CD field
                 cids, cide, 1, 1, cjds, cjde,   &         ! CD dims
                 cims, cime, 1, 1, cjms, cjme,   &         ! CD dims
                 cips, cipe, 1, 1, cjps, cjpe,   &         ! CD dims
                  ngrid%qsnowxy,  &   ! ND field
                 nids, nide, 1, 1, njds, njde,   &         ! ND dims
                 nims, nime, 1, 1, njms, njme,   &         ! ND dims
                 nips, nipe, 1, 1, njps, njpe,   &         ! ND dims
                  config_flags%shw, ngrid%imask_nostag,         &         ! stencil half width
                  .FALSE., .FALSE.,                                                &         ! xstag, ystag
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%wslakexy, 1 ) * SIZE( grid%wslakexy, 2 ) .GT. 1 ) THEN 
CALL copy_fcnm (  &         
                  grid%wslakexy,   &       ! CD field
                 cids, cide, 1, 1, cjds, cjde,   &         ! CD dims
                 cims, cime, 1, 1, cjms, cjme,   &         ! CD dims
                 cips, cipe, 1, 1, cjps, cjpe,   &         ! CD dims
                  ngrid%wslakexy,  &   ! ND field
                 nids, nide, 1, 1, njds, njde,   &         ! ND dims
                 nims, nime, 1, 1, njms, njme,   &         ! ND dims
                 nips, nipe, 1, 1, njps, njpe,   &         ! ND dims
                  config_flags%shw, ngrid%imask_nostag,         &         ! stencil half width
                  .FALSE., .FALSE.,                                                &         ! xstag, ystag
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%zwtxy, 1 ) * SIZE( grid%zwtxy, 2 ) .GT. 1 ) THEN 
CALL copy_fcnm (  &         
                  grid%zwtxy,   &       ! CD field
                 cids, cide, 1, 1, cjds, cjde,   &         ! CD dims
                 cims, cime, 1, 1, cjms, cjme,   &         ! CD dims
                 cips, cipe, 1, 1, cjps, cjpe,   &         ! CD dims
                  ngrid%zwtxy,  &   ! ND field
                 nids, nide, 1, 1, njds, njde,   &         ! ND dims
                 nims, nime, 1, 1, njms, njme,   &         ! ND dims
                 nips, nipe, 1, 1, njps, njpe,   &         ! ND dims
                  config_flags%shw, ngrid%imask_nostag,         &         ! stencil half width
                  .FALSE., .FALSE.,                                                &         ! xstag, ystag
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%waxy, 1 ) * SIZE( grid%waxy, 2 ) .GT. 1 ) THEN 
CALL copy_fcnm (  &         
                  grid%waxy,   &       ! CD field
                 cids, cide, 1, 1, cjds, cjde,   &         ! CD dims
                 cims, cime, 1, 1, cjms, cjme,   &         ! CD dims
                 cips, cipe, 1, 1, cjps, cjpe,   &         ! CD dims
                  ngrid%waxy,  &   ! ND field
                 nids, nide, 1, 1, njds, njde,   &         ! ND dims
                 nims, nime, 1, 1, njms, njme,   &         ! ND dims
                 nips, nipe, 1, 1, njps, njpe,   &         ! ND dims
                  config_flags%shw, ngrid%imask_nostag,         &         ! stencil half width
                  .FALSE., .FALSE.,                                                &         ! xstag, ystag
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%wtxy, 1 ) * SIZE( grid%wtxy, 2 ) .GT. 1 ) THEN 
CALL copy_fcnm (  &         
                  grid%wtxy,   &       ! CD field
                 cids, cide, 1, 1, cjds, cjde,   &         ! CD dims
                 cims, cime, 1, 1, cjms, cjme,   &         ! CD dims
                 cips, cipe, 1, 1, cjps, cjpe,   &         ! CD dims
                  ngrid%wtxy,  &   ! ND field
                 nids, nide, 1, 1, njds, njde,   &         ! ND dims
                 nims, nime, 1, 1, njms, njme,   &         ! ND dims
                 nips, nipe, 1, 1, njps, njpe,   &         ! ND dims
                  config_flags%shw, ngrid%imask_nostag,         &         ! stencil half width
                  .FALSE., .FALSE.,                                                &         ! xstag, ystag
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%tsnoxy, 1 ) * SIZE( grid%tsnoxy, 3 ) .GT. 1 ) THEN 
CALL copy_fcnm (  &         
                  grid%tsnoxy,   &       ! CD field
                 cids, cide, 1, config_flags%num_snow_layers, cjds, cjde,   &         ! CD dims
                 cims, cime, 1, config_flags%num_snow_layers, cjms, cjme,   &         ! CD dims
                 cips, cipe, 1, config_flags%num_snow_layers, cjps, cjpe,   &         ! CD dims
                  ngrid%tsnoxy,  &   ! ND field
                 nids, nide, 1, config_flags%num_snow_layers, njds, njde,   &         ! ND dims
                 nims, nime, 1, config_flags%num_snow_layers, njms, njme,   &         ! ND dims
                 nips, nipe, 1, config_flags%num_snow_layers, njps, njpe,   &         ! ND dims
                  config_flags%shw, ngrid%imask_nostag,         &         ! stencil half width
                  .FALSE., .FALSE.,                                                &         ! xstag, ystag
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%zsnsoxy, 1 ) * SIZE( grid%zsnsoxy, 3 ) .GT. 1 ) THEN 
CALL copy_fcnm (  &         
                  grid%zsnsoxy,   &       ! CD field
                 cids, cide, 1, config_flags%num_snso_layers, cjds, cjde,   &         ! CD dims
                 cims, cime, 1, config_flags%num_snso_layers, cjms, cjme,   &         ! CD dims
                 cips, cipe, 1, config_flags%num_snso_layers, cjps, cjpe,   &         ! CD dims
                  ngrid%zsnsoxy,  &   ! ND field
                 nids, nide, 1, config_flags%num_snso_layers, njds, njde,   &         ! ND dims
                 nims, nime, 1, config_flags%num_snso_layers, njms, njme,   &         ! ND dims
                 nips, nipe, 1, config_flags%num_snso_layers, njps, njpe,   &         ! ND dims
                  config_flags%shw, ngrid%imask_nostag,         &         ! stencil half width
                  .FALSE., .FALSE.,                                                &         ! xstag, ystag
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%snicexy, 1 ) * SIZE( grid%snicexy, 3 ) .GT. 1 ) THEN 
CALL copy_fcnm (  &         
                  grid%snicexy,   &       ! CD field
                 cids, cide, 1, config_flags%num_snow_layers, cjds, cjde,   &         ! CD dims
                 cims, cime, 1, config_flags%num_snow_layers, cjms, cjme,   &         ! CD dims
                 cips, cipe, 1, config_flags%num_snow_layers, cjps, cjpe,   &         ! CD dims
                  ngrid%snicexy,  &   ! ND field
                 nids, nide, 1, config_flags%num_snow_layers, njds, njde,   &         ! ND dims
                 nims, nime, 1, config_flags%num_snow_layers, njms, njme,   &         ! ND dims
                 nips, nipe, 1, config_flags%num_snow_layers, njps, njpe,   &         ! ND dims
                  config_flags%shw, ngrid%imask_nostag,         &         ! stencil half width
                  .FALSE., .FALSE.,                                                &         ! xstag, ystag
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%snliqxy, 1 ) * SIZE( grid%snliqxy, 3 ) .GT. 1 ) THEN 
CALL copy_fcnm (  &         
                  grid%snliqxy,   &       ! CD field
                 cids, cide, 1, config_flags%num_snow_layers, cjds, cjde,   &         ! CD dims
                 cims, cime, 1, config_flags%num_snow_layers, cjms, cjme,   &         ! CD dims
                 cips, cipe, 1, config_flags%num_snow_layers, cjps, cjpe,   &         ! CD dims
                  ngrid%snliqxy,  &   ! ND field
                 nids, nide, 1, config_flags%num_snow_layers, njds, njde,   &         ! ND dims
                 nims, nime, 1, config_flags%num_snow_layers, njms, njme,   &         ! ND dims
                 nips, nipe, 1, config_flags%num_snow_layers, njps, njpe,   &         ! ND dims
                  config_flags%shw, ngrid%imask_nostag,         &         ! stencil half width
                  .FALSE., .FALSE.,                                                &         ! xstag, ystag
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%lfmassxy, 1 ) * SIZE( grid%lfmassxy, 2 ) .GT. 1 ) THEN 
CALL copy_fcnm (  &         
                  grid%lfmassxy,   &       ! CD field
                 cids, cide, 1, 1, cjds, cjde,   &         ! CD dims
                 cims, cime, 1, 1, cjms, cjme,   &         ! CD dims
                 cips, cipe, 1, 1, cjps, cjpe,   &         ! CD dims
                  ngrid%lfmassxy,  &   ! ND field
                 nids, nide, 1, 1, njds, njde,   &         ! ND dims
                 nims, nime, 1, 1, njms, njme,   &         ! ND dims
                 nips, nipe, 1, 1, njps, njpe,   &         ! ND dims
                  config_flags%shw, ngrid%imask_nostag,         &         ! stencil half width
                  .FALSE., .FALSE.,                                                &         ! xstag, ystag
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%rtmassxy, 1 ) * SIZE( grid%rtmassxy, 2 ) .GT. 1 ) THEN 
CALL copy_fcnm (  &         
                  grid%rtmassxy,   &       ! CD field
                 cids, cide, 1, 1, cjds, cjde,   &         ! CD dims
                 cims, cime, 1, 1, cjms, cjme,   &         ! CD dims
                 cips, cipe, 1, 1, cjps, cjpe,   &         ! CD dims
                  ngrid%rtmassxy,  &   ! ND field
                 nids, nide, 1, 1, njds, njde,   &         ! ND dims
                 nims, nime, 1, 1, njms, njme,   &         ! ND dims
                 nips, nipe, 1, 1, njps, njpe,   &         ! ND dims
                  config_flags%shw, ngrid%imask_nostag,         &         ! stencil half width
                  .FALSE., .FALSE.,                                                &         ! xstag, ystag
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%stmassxy, 1 ) * SIZE( grid%stmassxy, 2 ) .GT. 1 ) THEN 
CALL copy_fcnm (  &         
                  grid%stmassxy,   &       ! CD field
                 cids, cide, 1, 1, cjds, cjde,   &         ! CD dims
                 cims, cime, 1, 1, cjms, cjme,   &         ! CD dims
                 cips, cipe, 1, 1, cjps, cjpe,   &         ! CD dims
                  ngrid%stmassxy,  &   ! ND field
                 nids, nide, 1, 1, njds, njde,   &         ! ND dims
                 nims, nime, 1, 1, njms, njme,   &         ! ND dims
                 nips, nipe, 1, 1, njps, njpe,   &         ! ND dims
                  config_flags%shw, ngrid%imask_nostag,         &         ! stencil half width
                  .FALSE., .FALSE.,                                                &         ! xstag, ystag
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%woodxy, 1 ) * SIZE( grid%woodxy, 2 ) .GT. 1 ) THEN 
CALL copy_fcnm (  &         
                  grid%woodxy,   &       ! CD field
                 cids, cide, 1, 1, cjds, cjde,   &         ! CD dims
                 cims, cime, 1, 1, cjms, cjme,   &         ! CD dims
                 cips, cipe, 1, 1, cjps, cjpe,   &         ! CD dims
                  ngrid%woodxy,  &   ! ND field
                 nids, nide, 1, 1, njds, njde,   &         ! ND dims
                 nims, nime, 1, 1, njms, njme,   &         ! ND dims
                 nips, nipe, 1, 1, njps, njpe,   &         ! ND dims
                  config_flags%shw, ngrid%imask_nostag,         &         ! stencil half width
                  .FALSE., .FALSE.,                                                &         ! xstag, ystag
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%stblcpxy, 1 ) * SIZE( grid%stblcpxy, 2 ) .GT. 1 ) THEN 
CALL copy_fcnm (  &         
                  grid%stblcpxy,   &       ! CD field
                 cids, cide, 1, 1, cjds, cjde,   &         ! CD dims
                 cims, cime, 1, 1, cjms, cjme,   &         ! CD dims
                 cips, cipe, 1, 1, cjps, cjpe,   &         ! CD dims
                  ngrid%stblcpxy,  &   ! ND field
                 nids, nide, 1, 1, njds, njde,   &         ! ND dims
                 nims, nime, 1, 1, njms, njme,   &         ! ND dims
                 nips, nipe, 1, 1, njps, njpe,   &         ! ND dims
                  config_flags%shw, ngrid%imask_nostag,         &         ! stencil half width
                  .FALSE., .FALSE.,                                                &         ! xstag, ystag
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%fastcpxy, 1 ) * SIZE( grid%fastcpxy, 2 ) .GT. 1 ) THEN 
CALL copy_fcnm (  &         
                  grid%fastcpxy,   &       ! CD field
                 cids, cide, 1, 1, cjds, cjde,   &         ! CD dims
                 cims, cime, 1, 1, cjms, cjme,   &         ! CD dims
                 cips, cipe, 1, 1, cjps, cjpe,   &         ! CD dims
                  ngrid%fastcpxy,  &   ! ND field
                 nids, nide, 1, 1, njds, njde,   &         ! ND dims
                 nims, nime, 1, 1, njms, njme,   &         ! ND dims
                 nips, nipe, 1, 1, njps, njpe,   &         ! ND dims
                  config_flags%shw, ngrid%imask_nostag,         &         ! stencil half width
                  .FALSE., .FALSE.,                                                &         ! xstag, ystag
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%xsaixy, 1 ) * SIZE( grid%xsaixy, 2 ) .GT. 1 ) THEN 
CALL copy_fcnm (  &         
                  grid%xsaixy,   &       ! CD field
                 cids, cide, 1, 1, cjds, cjde,   &         ! CD dims
                 cims, cime, 1, 1, cjms, cjme,   &         ! CD dims
                 cips, cipe, 1, 1, cjps, cjpe,   &         ! CD dims
                  ngrid%xsaixy,  &   ! ND field
                 nids, nide, 1, 1, njds, njde,   &         ! ND dims
                 nims, nime, 1, 1, njms, njme,   &         ! ND dims
                 nips, nipe, 1, 1, njps, njpe,   &         ! ND dims
                  config_flags%shw, ngrid%imask_nostag,         &         ! stencil half width
                  .FALSE., .FALSE.,                                                &         ! xstag, ystag
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%t2mvxy, 1 ) * SIZE( grid%t2mvxy, 2 ) .GT. 1 ) THEN 
CALL copy_fcnm (  &         
                  grid%t2mvxy,   &       ! CD field
                 cids, cide, 1, 1, cjds, cjde,   &         ! CD dims
                 cims, cime, 1, 1, cjms, cjme,   &         ! CD dims
                 cips, cipe, 1, 1, cjps, cjpe,   &         ! CD dims
                  ngrid%t2mvxy,  &   ! ND field
                 nids, nide, 1, 1, njds, njde,   &         ! ND dims
                 nims, nime, 1, 1, njms, njme,   &         ! ND dims
                 nips, nipe, 1, 1, njps, njpe,   &         ! ND dims
                  config_flags%shw, ngrid%imask_nostag,         &         ! stencil half width
                  .FALSE., .FALSE.,                                                &         ! xstag, ystag
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%t2mbxy, 1 ) * SIZE( grid%t2mbxy, 2 ) .GT. 1 ) THEN 
CALL copy_fcnm (  &         
                  grid%t2mbxy,   &       ! CD field
                 cids, cide, 1, 1, cjds, cjde,   &         ! CD dims
                 cims, cime, 1, 1, cjms, cjme,   &         ! CD dims
                 cips, cipe, 1, 1, cjps, cjpe,   &         ! CD dims
                  ngrid%t2mbxy,  &   ! ND field
                 nids, nide, 1, 1, njds, njde,   &         ! ND dims
                 nims, nime, 1, 1, njms, njme,   &         ! ND dims
                 nips, nipe, 1, 1, njps, njpe,   &         ! ND dims
                  config_flags%shw, ngrid%imask_nostag,         &         ! stencil half width
                  .FALSE., .FALSE.,                                                &         ! xstag, ystag
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%q2mvxy, 1 ) * SIZE( grid%q2mvxy, 2 ) .GT. 1 ) THEN 
CALL copy_fcnm (  &         
                  grid%q2mvxy,   &       ! CD field
                 cids, cide, 1, 1, cjds, cjde,   &         ! CD dims
                 cims, cime, 1, 1, cjms, cjme,   &         ! CD dims
                 cips, cipe, 1, 1, cjps, cjpe,   &         ! CD dims
                  ngrid%q2mvxy,  &   ! ND field
                 nids, nide, 1, 1, njds, njde,   &         ! ND dims
                 nims, nime, 1, 1, njms, njme,   &         ! ND dims
                 nips, nipe, 1, 1, njps, njpe,   &         ! ND dims
                  config_flags%shw, ngrid%imask_nostag,         &         ! stencil half width
                  .FALSE., .FALSE.,                                                &         ! xstag, ystag
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%q2mbxy, 1 ) * SIZE( grid%q2mbxy, 2 ) .GT. 1 ) THEN 
CALL copy_fcnm (  &         
                  grid%q2mbxy,   &       ! CD field
                 cids, cide, 1, 1, cjds, cjde,   &         ! CD dims
                 cims, cime, 1, 1, cjms, cjme,   &         ! CD dims
                 cips, cipe, 1, 1, cjps, cjpe,   &         ! CD dims
                  ngrid%q2mbxy,  &   ! ND field
                 nids, nide, 1, 1, njds, njde,   &         ! ND dims
                 nims, nime, 1, 1, njms, njme,   &         ! ND dims
                 nips, nipe, 1, 1, njps, njpe,   &         ! ND dims
                  config_flags%shw, ngrid%imask_nostag,         &         ! stencil half width
                  .FALSE., .FALSE.,                                                &         ! xstag, ystag
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%tradxy, 1 ) * SIZE( grid%tradxy, 2 ) .GT. 1 ) THEN 
CALL copy_fcnm (  &         
                  grid%tradxy,   &       ! CD field
                 cids, cide, 1, 1, cjds, cjde,   &         ! CD dims
                 cims, cime, 1, 1, cjms, cjme,   &         ! CD dims
                 cips, cipe, 1, 1, cjps, cjpe,   &         ! CD dims
                  ngrid%tradxy,  &   ! ND field
                 nids, nide, 1, 1, njds, njde,   &         ! ND dims
                 nims, nime, 1, 1, njms, njme,   &         ! ND dims
                 nips, nipe, 1, 1, njps, njpe,   &         ! ND dims
                  config_flags%shw, ngrid%imask_nostag,         &         ! stencil half width
                  .FALSE., .FALSE.,                                                &         ! xstag, ystag
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%neexy, 1 ) * SIZE( grid%neexy, 2 ) .GT. 1 ) THEN 
CALL copy_fcnm (  &         
                  grid%neexy,   &       ! CD field
                 cids, cide, 1, 1, cjds, cjde,   &         ! CD dims
                 cims, cime, 1, 1, cjms, cjme,   &         ! CD dims
                 cips, cipe, 1, 1, cjps, cjpe,   &         ! CD dims
                  ngrid%neexy,  &   ! ND field
                 nids, nide, 1, 1, njds, njde,   &         ! ND dims
                 nims, nime, 1, 1, njms, njme,   &         ! ND dims
                 nips, nipe, 1, 1, njps, njpe,   &         ! ND dims
                  config_flags%shw, ngrid%imask_nostag,         &         ! stencil half width
                  .FALSE., .FALSE.,                                                &         ! xstag, ystag
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%gppxy, 1 ) * SIZE( grid%gppxy, 2 ) .GT. 1 ) THEN 
CALL copy_fcnm (  &         
                  grid%gppxy,   &       ! CD field
                 cids, cide, 1, 1, cjds, cjde,   &         ! CD dims
                 cims, cime, 1, 1, cjms, cjme,   &         ! CD dims
                 cips, cipe, 1, 1, cjps, cjpe,   &         ! CD dims
                  ngrid%gppxy,  &   ! ND field
                 nids, nide, 1, 1, njds, njde,   &         ! ND dims
                 nims, nime, 1, 1, njms, njme,   &         ! ND dims
                 nips, nipe, 1, 1, njps, njpe,   &         ! ND dims
                  config_flags%shw, ngrid%imask_nostag,         &         ! stencil half width
                  .FALSE., .FALSE.,                                                &         ! xstag, ystag
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%nppxy, 1 ) * SIZE( grid%nppxy, 2 ) .GT. 1 ) THEN 
CALL copy_fcnm (  &         
                  grid%nppxy,   &       ! CD field
                 cids, cide, 1, 1, cjds, cjde,   &         ! CD dims
                 cims, cime, 1, 1, cjms, cjme,   &         ! CD dims
                 cips, cipe, 1, 1, cjps, cjpe,   &         ! CD dims
                  ngrid%nppxy,  &   ! ND field
                 nids, nide, 1, 1, njds, njde,   &         ! ND dims
                 nims, nime, 1, 1, njms, njme,   &         ! ND dims
                 nips, nipe, 1, 1, njps, njpe,   &         ! ND dims
                  config_flags%shw, ngrid%imask_nostag,         &         ! stencil half width
                  .FALSE., .FALSE.,                                                &         ! xstag, ystag
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%fvegxy, 1 ) * SIZE( grid%fvegxy, 2 ) .GT. 1 ) THEN 
CALL copy_fcnm (  &         
                  grid%fvegxy,   &       ! CD field
                 cids, cide, 1, 1, cjds, cjde,   &         ! CD dims
                 cims, cime, 1, 1, cjms, cjme,   &         ! CD dims
                 cips, cipe, 1, 1, cjps, cjpe,   &         ! CD dims
                  ngrid%fvegxy,  &   ! ND field
                 nids, nide, 1, 1, njds, njde,   &         ! ND dims
                 nims, nime, 1, 1, njms, njme,   &         ! ND dims
                 nips, nipe, 1, 1, njps, njpe,   &         ! ND dims
                  config_flags%shw, ngrid%imask_nostag,         &         ! stencil half width
                  .FALSE., .FALSE.,                                                &         ! xstag, ystag
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%qinxy, 1 ) * SIZE( grid%qinxy, 2 ) .GT. 1 ) THEN 
CALL copy_fcnm (  &         
                  grid%qinxy,   &       ! CD field
                 cids, cide, 1, 1, cjds, cjde,   &         ! CD dims
                 cims, cime, 1, 1, cjms, cjme,   &         ! CD dims
                 cips, cipe, 1, 1, cjps, cjpe,   &         ! CD dims
                  ngrid%qinxy,  &   ! ND field
                 nids, nide, 1, 1, njds, njde,   &         ! ND dims
                 nims, nime, 1, 1, njms, njme,   &         ! ND dims
                 nips, nipe, 1, 1, njps, njpe,   &         ! ND dims
                  config_flags%shw, ngrid%imask_nostag,         &         ! stencil half width
                  .FALSE., .FALSE.,                                                &         ! xstag, ystag
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%runsfxy, 1 ) * SIZE( grid%runsfxy, 2 ) .GT. 1 ) THEN 
CALL copy_fcnm (  &         
                  grid%runsfxy,   &       ! CD field
                 cids, cide, 1, 1, cjds, cjde,   &         ! CD dims
                 cims, cime, 1, 1, cjms, cjme,   &         ! CD dims
                 cips, cipe, 1, 1, cjps, cjpe,   &         ! CD dims
                  ngrid%runsfxy,  &   ! ND field
                 nids, nide, 1, 1, njds, njde,   &         ! ND dims
                 nims, nime, 1, 1, njms, njme,   &         ! ND dims
                 nips, nipe, 1, 1, njps, njpe,   &         ! ND dims
                  config_flags%shw, ngrid%imask_nostag,         &         ! stencil half width
                  .FALSE., .FALSE.,                                                &         ! xstag, ystag
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%runsbxy, 1 ) * SIZE( grid%runsbxy, 2 ) .GT. 1 ) THEN 
CALL copy_fcnm (  &         
                  grid%runsbxy,   &       ! CD field
                 cids, cide, 1, 1, cjds, cjde,   &         ! CD dims
                 cims, cime, 1, 1, cjms, cjme,   &         ! CD dims
                 cips, cipe, 1, 1, cjps, cjpe,   &         ! CD dims
                  ngrid%runsbxy,  &   ! ND field
                 nids, nide, 1, 1, njds, njde,   &         ! ND dims
                 nims, nime, 1, 1, njms, njme,   &         ! ND dims
                 nips, nipe, 1, 1, njps, njpe,   &         ! ND dims
                  config_flags%shw, ngrid%imask_nostag,         &         ! stencil half width
                  .FALSE., .FALSE.,                                                &         ! xstag, ystag
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%ecanxy, 1 ) * SIZE( grid%ecanxy, 2 ) .GT. 1 ) THEN 
CALL copy_fcnm (  &         
                  grid%ecanxy,   &       ! CD field
                 cids, cide, 1, 1, cjds, cjde,   &         ! CD dims
                 cims, cime, 1, 1, cjms, cjme,   &         ! CD dims
                 cips, cipe, 1, 1, cjps, cjpe,   &         ! CD dims
                  ngrid%ecanxy,  &   ! ND field
                 nids, nide, 1, 1, njds, njde,   &         ! ND dims
                 nims, nime, 1, 1, njms, njme,   &         ! ND dims
                 nips, nipe, 1, 1, njps, njpe,   &         ! ND dims
                  config_flags%shw, ngrid%imask_nostag,         &         ! stencil half width
                  .FALSE., .FALSE.,                                                &         ! xstag, ystag
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%edirxy, 1 ) * SIZE( grid%edirxy, 2 ) .GT. 1 ) THEN 
CALL copy_fcnm (  &         
                  grid%edirxy,   &       ! CD field
                 cids, cide, 1, 1, cjds, cjde,   &         ! CD dims
                 cims, cime, 1, 1, cjms, cjme,   &         ! CD dims
                 cips, cipe, 1, 1, cjps, cjpe,   &         ! CD dims
                  ngrid%edirxy,  &   ! ND field
                 nids, nide, 1, 1, njds, njde,   &         ! ND dims
                 nims, nime, 1, 1, njms, njme,   &         ! ND dims
                 nips, nipe, 1, 1, njps, njpe,   &         ! ND dims
                  config_flags%shw, ngrid%imask_nostag,         &         ! stencil half width
                  .FALSE., .FALSE.,                                                &         ! xstag, ystag
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%etranxy, 1 ) * SIZE( grid%etranxy, 2 ) .GT. 1 ) THEN 
CALL copy_fcnm (  &         
                  grid%etranxy,   &       ! CD field
                 cids, cide, 1, 1, cjds, cjde,   &         ! CD dims
                 cims, cime, 1, 1, cjms, cjme,   &         ! CD dims
                 cips, cipe, 1, 1, cjps, cjpe,   &         ! CD dims
                  ngrid%etranxy,  &   ! ND field
                 nids, nide, 1, 1, njds, njde,   &         ! ND dims
                 nims, nime, 1, 1, njms, njme,   &         ! ND dims
                 nips, nipe, 1, 1, njps, njpe,   &         ! ND dims
                  config_flags%shw, ngrid%imask_nostag,         &         ! stencil half width
                  .FALSE., .FALSE.,                                                &         ! xstag, ystag
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%fsaxy, 1 ) * SIZE( grid%fsaxy, 2 ) .GT. 1 ) THEN 
CALL copy_fcnm (  &         
                  grid%fsaxy,   &       ! CD field
                 cids, cide, 1, 1, cjds, cjde,   &         ! CD dims
                 cims, cime, 1, 1, cjms, cjme,   &         ! CD dims
                 cips, cipe, 1, 1, cjps, cjpe,   &         ! CD dims
                  ngrid%fsaxy,  &   ! ND field
                 nids, nide, 1, 1, njds, njde,   &         ! ND dims
                 nims, nime, 1, 1, njms, njme,   &         ! ND dims
                 nips, nipe, 1, 1, njps, njpe,   &         ! ND dims
                  config_flags%shw, ngrid%imask_nostag,         &         ! stencil half width
                  .FALSE., .FALSE.,                                                &         ! xstag, ystag
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%firaxy, 1 ) * SIZE( grid%firaxy, 2 ) .GT. 1 ) THEN 
CALL copy_fcnm (  &         
                  grid%firaxy,   &       ! CD field
                 cids, cide, 1, 1, cjds, cjde,   &         ! CD dims
                 cims, cime, 1, 1, cjms, cjme,   &         ! CD dims
                 cips, cipe, 1, 1, cjps, cjpe,   &         ! CD dims
                  ngrid%firaxy,  &   ! ND field
                 nids, nide, 1, 1, njds, njde,   &         ! ND dims
                 nims, nime, 1, 1, njms, njme,   &         ! ND dims
                 nips, nipe, 1, 1, njps, njpe,   &         ! ND dims
                  config_flags%shw, ngrid%imask_nostag,         &         ! stencil half width
                  .FALSE., .FALSE.,                                                &         ! xstag, ystag
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%aparxy, 1 ) * SIZE( grid%aparxy, 2 ) .GT. 1 ) THEN 
CALL copy_fcnm (  &         
                  grid%aparxy,   &       ! CD field
                 cids, cide, 1, 1, cjds, cjde,   &         ! CD dims
                 cims, cime, 1, 1, cjms, cjme,   &         ! CD dims
                 cips, cipe, 1, 1, cjps, cjpe,   &         ! CD dims
                  ngrid%aparxy,  &   ! ND field
                 nids, nide, 1, 1, njds, njde,   &         ! ND dims
                 nims, nime, 1, 1, njms, njme,   &         ! ND dims
                 nips, nipe, 1, 1, njps, njpe,   &         ! ND dims
                  config_flags%shw, ngrid%imask_nostag,         &         ! stencil half width
                  .FALSE., .FALSE.,                                                &         ! xstag, ystag
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%psnxy, 1 ) * SIZE( grid%psnxy, 2 ) .GT. 1 ) THEN 
CALL copy_fcnm (  &         
                  grid%psnxy,   &       ! CD field
                 cids, cide, 1, 1, cjds, cjde,   &         ! CD dims
                 cims, cime, 1, 1, cjms, cjme,   &         ! CD dims
                 cips, cipe, 1, 1, cjps, cjpe,   &         ! CD dims
                  ngrid%psnxy,  &   ! ND field
                 nids, nide, 1, 1, njds, njde,   &         ! ND dims
                 nims, nime, 1, 1, njms, njme,   &         ! ND dims
                 nips, nipe, 1, 1, njps, njpe,   &         ! ND dims
                  config_flags%shw, ngrid%imask_nostag,         &         ! stencil half width
                  .FALSE., .FALSE.,                                                &         ! xstag, ystag
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%savxy, 1 ) * SIZE( grid%savxy, 2 ) .GT. 1 ) THEN 
CALL copy_fcnm (  &         
                  grid%savxy,   &       ! CD field
                 cids, cide, 1, 1, cjds, cjde,   &         ! CD dims
                 cims, cime, 1, 1, cjms, cjme,   &         ! CD dims
                 cips, cipe, 1, 1, cjps, cjpe,   &         ! CD dims
                  ngrid%savxy,  &   ! ND field
                 nids, nide, 1, 1, njds, njde,   &         ! ND dims
                 nims, nime, 1, 1, njms, njme,   &         ! ND dims
                 nips, nipe, 1, 1, njps, njpe,   &         ! ND dims
                  config_flags%shw, ngrid%imask_nostag,         &         ! stencil half width
                  .FALSE., .FALSE.,                                                &         ! xstag, ystag
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%sagxy, 1 ) * SIZE( grid%sagxy, 2 ) .GT. 1 ) THEN 
CALL copy_fcnm (  &         
                  grid%sagxy,   &       ! CD field
                 cids, cide, 1, 1, cjds, cjde,   &         ! CD dims
                 cims, cime, 1, 1, cjms, cjme,   &         ! CD dims
                 cips, cipe, 1, 1, cjps, cjpe,   &         ! CD dims
                  ngrid%sagxy,  &   ! ND field
                 nids, nide, 1, 1, njds, njde,   &         ! ND dims
                 nims, nime, 1, 1, njms, njme,   &         ! ND dims
                 nips, nipe, 1, 1, njps, njpe,   &         ! ND dims
                  config_flags%shw, ngrid%imask_nostag,         &         ! stencil half width
                  .FALSE., .FALSE.,                                                &         ! xstag, ystag
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%rssunxy, 1 ) * SIZE( grid%rssunxy, 2 ) .GT. 1 ) THEN 
CALL copy_fcnm (  &         
                  grid%rssunxy,   &       ! CD field
                 cids, cide, 1, 1, cjds, cjde,   &         ! CD dims
                 cims, cime, 1, 1, cjms, cjme,   &         ! CD dims
                 cips, cipe, 1, 1, cjps, cjpe,   &         ! CD dims
                  ngrid%rssunxy,  &   ! ND field
                 nids, nide, 1, 1, njds, njde,   &         ! ND dims
                 nims, nime, 1, 1, njms, njme,   &         ! ND dims
                 nips, nipe, 1, 1, njps, njpe,   &         ! ND dims
                  config_flags%shw, ngrid%imask_nostag,         &         ! stencil half width
                  .FALSE., .FALSE.,                                                &         ! xstag, ystag
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%rsshaxy, 1 ) * SIZE( grid%rsshaxy, 2 ) .GT. 1 ) THEN 
CALL copy_fcnm (  &         
                  grid%rsshaxy,   &       ! CD field
                 cids, cide, 1, 1, cjds, cjde,   &         ! CD dims
                 cims, cime, 1, 1, cjms, cjme,   &         ! CD dims
                 cips, cipe, 1, 1, cjps, cjpe,   &         ! CD dims
                  ngrid%rsshaxy,  &   ! ND field
                 nids, nide, 1, 1, njds, njde,   &         ! ND dims
                 nims, nime, 1, 1, njms, njme,   &         ! ND dims
                 nips, nipe, 1, 1, njps, njpe,   &         ! ND dims
                  config_flags%shw, ngrid%imask_nostag,         &         ! stencil half width
                  .FALSE., .FALSE.,                                                &         ! xstag, ystag
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%bgapxy, 1 ) * SIZE( grid%bgapxy, 2 ) .GT. 1 ) THEN 
CALL copy_fcnm (  &         
                  grid%bgapxy,   &       ! CD field
                 cids, cide, 1, 1, cjds, cjde,   &         ! CD dims
                 cims, cime, 1, 1, cjms, cjme,   &         ! CD dims
                 cips, cipe, 1, 1, cjps, cjpe,   &         ! CD dims
                  ngrid%bgapxy,  &   ! ND field
                 nids, nide, 1, 1, njds, njde,   &         ! ND dims
                 nims, nime, 1, 1, njms, njme,   &         ! ND dims
                 nips, nipe, 1, 1, njps, njpe,   &         ! ND dims
                  config_flags%shw, ngrid%imask_nostag,         &         ! stencil half width
                  .FALSE., .FALSE.,                                                &         ! xstag, ystag
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%wgapxy, 1 ) * SIZE( grid%wgapxy, 2 ) .GT. 1 ) THEN 
CALL copy_fcnm (  &         
                  grid%wgapxy,   &       ! CD field
                 cids, cide, 1, 1, cjds, cjde,   &         ! CD dims
                 cims, cime, 1, 1, cjms, cjme,   &         ! CD dims
                 cips, cipe, 1, 1, cjps, cjpe,   &         ! CD dims
                  ngrid%wgapxy,  &   ! ND field
                 nids, nide, 1, 1, njds, njde,   &         ! ND dims
                 nims, nime, 1, 1, njms, njme,   &         ! ND dims
                 nips, nipe, 1, 1, njps, njpe,   &         ! ND dims
                  config_flags%shw, ngrid%imask_nostag,         &         ! stencil half width
                  .FALSE., .FALSE.,                                                &         ! xstag, ystag
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%tgvxy, 1 ) * SIZE( grid%tgvxy, 2 ) .GT. 1 ) THEN 
CALL copy_fcnm (  &         
                  grid%tgvxy,   &       ! CD field
                 cids, cide, 1, 1, cjds, cjde,   &         ! CD dims
                 cims, cime, 1, 1, cjms, cjme,   &         ! CD dims
                 cips, cipe, 1, 1, cjps, cjpe,   &         ! CD dims
                  ngrid%tgvxy,  &   ! ND field
                 nids, nide, 1, 1, njds, njde,   &         ! ND dims
                 nims, nime, 1, 1, njms, njme,   &         ! ND dims
                 nips, nipe, 1, 1, njps, njpe,   &         ! ND dims
                  config_flags%shw, ngrid%imask_nostag,         &         ! stencil half width
                  .FALSE., .FALSE.,                                                &         ! xstag, ystag
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%tgbxy, 1 ) * SIZE( grid%tgbxy, 2 ) .GT. 1 ) THEN 
CALL copy_fcnm (  &         
                  grid%tgbxy,   &       ! CD field
                 cids, cide, 1, 1, cjds, cjde,   &         ! CD dims
                 cims, cime, 1, 1, cjms, cjme,   &         ! CD dims
                 cips, cipe, 1, 1, cjps, cjpe,   &         ! CD dims
                  ngrid%tgbxy,  &   ! ND field
                 nids, nide, 1, 1, njds, njde,   &         ! ND dims
                 nims, nime, 1, 1, njms, njme,   &         ! ND dims
                 nips, nipe, 1, 1, njps, njpe,   &         ! ND dims
                  config_flags%shw, ngrid%imask_nostag,         &         ! stencil half width
                  .FALSE., .FALSE.,                                                &         ! xstag, ystag
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%chvxy, 1 ) * SIZE( grid%chvxy, 2 ) .GT. 1 ) THEN 
CALL copy_fcnm (  &         
                  grid%chvxy,   &       ! CD field
                 cids, cide, 1, 1, cjds, cjde,   &         ! CD dims
                 cims, cime, 1, 1, cjms, cjme,   &         ! CD dims
                 cips, cipe, 1, 1, cjps, cjpe,   &         ! CD dims
                  ngrid%chvxy,  &   ! ND field
                 nids, nide, 1, 1, njds, njde,   &         ! ND dims
                 nims, nime, 1, 1, njms, njme,   &         ! ND dims
                 nips, nipe, 1, 1, njps, njpe,   &         ! ND dims
                  config_flags%shw, ngrid%imask_nostag,         &         ! stencil half width
                  .FALSE., .FALSE.,                                                &         ! xstag, ystag
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%chbxy, 1 ) * SIZE( grid%chbxy, 2 ) .GT. 1 ) THEN 
CALL copy_fcnm (  &         
                  grid%chbxy,   &       ! CD field
                 cids, cide, 1, 1, cjds, cjde,   &         ! CD dims
                 cims, cime, 1, 1, cjms, cjme,   &         ! CD dims
                 cips, cipe, 1, 1, cjps, cjpe,   &         ! CD dims
                  ngrid%chbxy,  &   ! ND field
                 nids, nide, 1, 1, njds, njde,   &         ! ND dims
                 nims, nime, 1, 1, njms, njme,   &         ! ND dims
                 nips, nipe, 1, 1, njps, njpe,   &         ! ND dims
                  config_flags%shw, ngrid%imask_nostag,         &         ! stencil half width
                  .FALSE., .FALSE.,                                                &         ! xstag, ystag
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%shgxy, 1 ) * SIZE( grid%shgxy, 2 ) .GT. 1 ) THEN 
CALL copy_fcnm (  &         
                  grid%shgxy,   &       ! CD field
                 cids, cide, 1, 1, cjds, cjde,   &         ! CD dims
                 cims, cime, 1, 1, cjms, cjme,   &         ! CD dims
                 cips, cipe, 1, 1, cjps, cjpe,   &         ! CD dims
                  ngrid%shgxy,  &   ! ND field
                 nids, nide, 1, 1, njds, njde,   &         ! ND dims
                 nims, nime, 1, 1, njms, njme,   &         ! ND dims
                 nips, nipe, 1, 1, njps, njpe,   &         ! ND dims
                  config_flags%shw, ngrid%imask_nostag,         &         ! stencil half width
                  .FALSE., .FALSE.,                                                &         ! xstag, ystag
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%shcxy, 1 ) * SIZE( grid%shcxy, 2 ) .GT. 1 ) THEN 
CALL copy_fcnm (  &         
                  grid%shcxy,   &       ! CD field
                 cids, cide, 1, 1, cjds, cjde,   &         ! CD dims
                 cims, cime, 1, 1, cjms, cjme,   &         ! CD dims
                 cips, cipe, 1, 1, cjps, cjpe,   &         ! CD dims
                  ngrid%shcxy,  &   ! ND field
                 nids, nide, 1, 1, njds, njde,   &         ! ND dims
                 nims, nime, 1, 1, njms, njme,   &         ! ND dims
                 nips, nipe, 1, 1, njps, njpe,   &         ! ND dims
                  config_flags%shw, ngrid%imask_nostag,         &         ! stencil half width
                  .FALSE., .FALSE.,                                                &         ! xstag, ystag
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%shbxy, 1 ) * SIZE( grid%shbxy, 2 ) .GT. 1 ) THEN 
CALL copy_fcnm (  &         
                  grid%shbxy,   &       ! CD field
                 cids, cide, 1, 1, cjds, cjde,   &         ! CD dims
                 cims, cime, 1, 1, cjms, cjme,   &         ! CD dims
                 cips, cipe, 1, 1, cjps, cjpe,   &         ! CD dims
                  ngrid%shbxy,  &   ! ND field
                 nids, nide, 1, 1, njds, njde,   &         ! ND dims
                 nims, nime, 1, 1, njms, njme,   &         ! ND dims
                 nips, nipe, 1, 1, njps, njpe,   &         ! ND dims
                  config_flags%shw, ngrid%imask_nostag,         &         ! stencil half width
                  .FALSE., .FALSE.,                                                &         ! xstag, ystag
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%evgxy, 1 ) * SIZE( grid%evgxy, 2 ) .GT. 1 ) THEN 
CALL copy_fcnm (  &         
                  grid%evgxy,   &       ! CD field
                 cids, cide, 1, 1, cjds, cjde,   &         ! CD dims
                 cims, cime, 1, 1, cjms, cjme,   &         ! CD dims
                 cips, cipe, 1, 1, cjps, cjpe,   &         ! CD dims
                  ngrid%evgxy,  &   ! ND field
                 nids, nide, 1, 1, njds, njde,   &         ! ND dims
                 nims, nime, 1, 1, njms, njme,   &         ! ND dims
                 nips, nipe, 1, 1, njps, njpe,   &         ! ND dims
                  config_flags%shw, ngrid%imask_nostag,         &         ! stencil half width
                  .FALSE., .FALSE.,                                                &         ! xstag, ystag
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%evbxy, 1 ) * SIZE( grid%evbxy, 2 ) .GT. 1 ) THEN 
CALL copy_fcnm (  &         
                  grid%evbxy,   &       ! CD field
                 cids, cide, 1, 1, cjds, cjde,   &         ! CD dims
                 cims, cime, 1, 1, cjms, cjme,   &         ! CD dims
                 cips, cipe, 1, 1, cjps, cjpe,   &         ! CD dims
                  ngrid%evbxy,  &   ! ND field
                 nids, nide, 1, 1, njds, njde,   &         ! ND dims
                 nims, nime, 1, 1, njms, njme,   &         ! ND dims
                 nips, nipe, 1, 1, njps, njpe,   &         ! ND dims
                  config_flags%shw, ngrid%imask_nostag,         &         ! stencil half width
                  .FALSE., .FALSE.,                                                &         ! xstag, ystag
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%ghvxy, 1 ) * SIZE( grid%ghvxy, 2 ) .GT. 1 ) THEN 
CALL copy_fcnm (  &         
                  grid%ghvxy,   &       ! CD field
                 cids, cide, 1, 1, cjds, cjde,   &         ! CD dims
                 cims, cime, 1, 1, cjms, cjme,   &         ! CD dims
                 cips, cipe, 1, 1, cjps, cjpe,   &         ! CD dims
                  ngrid%ghvxy,  &   ! ND field
                 nids, nide, 1, 1, njds, njde,   &         ! ND dims
                 nims, nime, 1, 1, njms, njme,   &         ! ND dims
                 nips, nipe, 1, 1, njps, njpe,   &         ! ND dims
                  config_flags%shw, ngrid%imask_nostag,         &         ! stencil half width
                  .FALSE., .FALSE.,                                                &         ! xstag, ystag
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%ghbxy, 1 ) * SIZE( grid%ghbxy, 2 ) .GT. 1 ) THEN 
CALL copy_fcnm (  &         
                  grid%ghbxy,   &       ! CD field
                 cids, cide, 1, 1, cjds, cjde,   &         ! CD dims
                 cims, cime, 1, 1, cjms, cjme,   &         ! CD dims
                 cips, cipe, 1, 1, cjps, cjpe,   &         ! CD dims
                  ngrid%ghbxy,  &   ! ND field
                 nids, nide, 1, 1, njds, njde,   &         ! ND dims
                 nims, nime, 1, 1, njms, njme,   &         ! ND dims
                 nips, nipe, 1, 1, njps, njpe,   &         ! ND dims
                  config_flags%shw, ngrid%imask_nostag,         &         ! stencil half width
                  .FALSE., .FALSE.,                                                &         ! xstag, ystag
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%irgxy, 1 ) * SIZE( grid%irgxy, 2 ) .GT. 1 ) THEN 
CALL copy_fcnm (  &         
                  grid%irgxy,   &       ! CD field
                 cids, cide, 1, 1, cjds, cjde,   &         ! CD dims
                 cims, cime, 1, 1, cjms, cjme,   &         ! CD dims
                 cips, cipe, 1, 1, cjps, cjpe,   &         ! CD dims
                  ngrid%irgxy,  &   ! ND field
                 nids, nide, 1, 1, njds, njde,   &         ! ND dims
                 nims, nime, 1, 1, njms, njme,   &         ! ND dims
                 nips, nipe, 1, 1, njps, njpe,   &         ! ND dims
                  config_flags%shw, ngrid%imask_nostag,         &         ! stencil half width
                  .FALSE., .FALSE.,                                                &         ! xstag, ystag
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%ircxy, 1 ) * SIZE( grid%ircxy, 2 ) .GT. 1 ) THEN 
CALL copy_fcnm (  &         
                  grid%ircxy,   &       ! CD field
                 cids, cide, 1, 1, cjds, cjde,   &         ! CD dims
                 cims, cime, 1, 1, cjms, cjme,   &         ! CD dims
                 cips, cipe, 1, 1, cjps, cjpe,   &         ! CD dims
                  ngrid%ircxy,  &   ! ND field
                 nids, nide, 1, 1, njds, njde,   &         ! ND dims
                 nims, nime, 1, 1, njms, njme,   &         ! ND dims
                 nips, nipe, 1, 1, njps, njpe,   &         ! ND dims
                  config_flags%shw, ngrid%imask_nostag,         &         ! stencil half width
                  .FALSE., .FALSE.,                                                &         ! xstag, ystag
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%irbxy, 1 ) * SIZE( grid%irbxy, 2 ) .GT. 1 ) THEN 
CALL copy_fcnm (  &         
                  grid%irbxy,   &       ! CD field
                 cids, cide, 1, 1, cjds, cjde,   &         ! CD dims
                 cims, cime, 1, 1, cjms, cjme,   &         ! CD dims
                 cips, cipe, 1, 1, cjps, cjpe,   &         ! CD dims
                  ngrid%irbxy,  &   ! ND field
                 nids, nide, 1, 1, njds, njde,   &         ! ND dims
                 nims, nime, 1, 1, njms, njme,   &         ! ND dims
                 nips, nipe, 1, 1, njps, njpe,   &         ! ND dims
                  config_flags%shw, ngrid%imask_nostag,         &         ! stencil half width
                  .FALSE., .FALSE.,                                                &         ! xstag, ystag
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%trxy, 1 ) * SIZE( grid%trxy, 2 ) .GT. 1 ) THEN 
CALL copy_fcnm (  &         
                  grid%trxy,   &       ! CD field
                 cids, cide, 1, 1, cjds, cjde,   &         ! CD dims
                 cims, cime, 1, 1, cjms, cjme,   &         ! CD dims
                 cips, cipe, 1, 1, cjps, cjpe,   &         ! CD dims
                  ngrid%trxy,  &   ! ND field
                 nids, nide, 1, 1, njds, njde,   &         ! ND dims
                 nims, nime, 1, 1, njms, njme,   &         ! ND dims
                 nips, nipe, 1, 1, njps, njpe,   &         ! ND dims
                  config_flags%shw, ngrid%imask_nostag,         &         ! stencil half width
                  .FALSE., .FALSE.,                                                &         ! xstag, ystag
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%evcxy, 1 ) * SIZE( grid%evcxy, 2 ) .GT. 1 ) THEN 
CALL copy_fcnm (  &         
                  grid%evcxy,   &       ! CD field
                 cids, cide, 1, 1, cjds, cjde,   &         ! CD dims
                 cims, cime, 1, 1, cjms, cjme,   &         ! CD dims
                 cips, cipe, 1, 1, cjps, cjpe,   &         ! CD dims
                  ngrid%evcxy,  &   ! ND field
                 nids, nide, 1, 1, njds, njde,   &         ! ND dims
                 nims, nime, 1, 1, njms, njme,   &         ! ND dims
                 nips, nipe, 1, 1, njps, njpe,   &         ! ND dims
                  config_flags%shw, ngrid%imask_nostag,         &         ! stencil half width
                  .FALSE., .FALSE.,                                                &         ! xstag, ystag
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%chleafxy, 1 ) * SIZE( grid%chleafxy, 2 ) .GT. 1 ) THEN 
CALL copy_fcnm (  &         
                  grid%chleafxy,   &       ! CD field
                 cids, cide, 1, 1, cjds, cjde,   &         ! CD dims
                 cims, cime, 1, 1, cjms, cjme,   &         ! CD dims
                 cips, cipe, 1, 1, cjps, cjpe,   &         ! CD dims
                  ngrid%chleafxy,  &   ! ND field
                 nids, nide, 1, 1, njds, njde,   &         ! ND dims
                 nims, nime, 1, 1, njms, njme,   &         ! ND dims
                 nips, nipe, 1, 1, njps, njpe,   &         ! ND dims
                  config_flags%shw, ngrid%imask_nostag,         &         ! stencil half width
                  .FALSE., .FALSE.,                                                &         ! xstag, ystag
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%chucxy, 1 ) * SIZE( grid%chucxy, 2 ) .GT. 1 ) THEN 
CALL copy_fcnm (  &         
                  grid%chucxy,   &       ! CD field
                 cids, cide, 1, 1, cjds, cjde,   &         ! CD dims
                 cims, cime, 1, 1, cjms, cjme,   &         ! CD dims
                 cips, cipe, 1, 1, cjps, cjpe,   &         ! CD dims
                  ngrid%chucxy,  &   ! ND field
                 nids, nide, 1, 1, njds, njde,   &         ! ND dims
                 nims, nime, 1, 1, njms, njme,   &         ! ND dims
                 nips, nipe, 1, 1, njps, njpe,   &         ! ND dims
                  config_flags%shw, ngrid%imask_nostag,         &         ! stencil half width
                  .FALSE., .FALSE.,                                                &         ! xstag, ystag
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%chv2xy, 1 ) * SIZE( grid%chv2xy, 2 ) .GT. 1 ) THEN 
CALL copy_fcnm (  &         
                  grid%chv2xy,   &       ! CD field
                 cids, cide, 1, 1, cjds, cjde,   &         ! CD dims
                 cims, cime, 1, 1, cjms, cjme,   &         ! CD dims
                 cips, cipe, 1, 1, cjps, cjpe,   &         ! CD dims
                  ngrid%chv2xy,  &   ! ND field
                 nids, nide, 1, 1, njds, njde,   &         ! ND dims
                 nims, nime, 1, 1, njms, njme,   &         ! ND dims
                 nips, nipe, 1, 1, njps, njpe,   &         ! ND dims
                  config_flags%shw, ngrid%imask_nostag,         &         ! stencil half width
                  .FALSE., .FALSE.,                                                &         ! xstag, ystag
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%chb2xy, 1 ) * SIZE( grid%chb2xy, 2 ) .GT. 1 ) THEN 
CALL copy_fcnm (  &         
                  grid%chb2xy,   &       ! CD field
                 cids, cide, 1, 1, cjds, cjde,   &         ! CD dims
                 cims, cime, 1, 1, cjms, cjme,   &         ! CD dims
                 cips, cipe, 1, 1, cjps, cjpe,   &         ! CD dims
                  ngrid%chb2xy,  &   ! ND field
                 nids, nide, 1, 1, njds, njde,   &         ! ND dims
                 nims, nime, 1, 1, njms, njme,   &         ! ND dims
                 nips, nipe, 1, 1, njps, njpe,   &         ! ND dims
                  config_flags%shw, ngrid%imask_nostag,         &         ! stencil half width
                  .FALSE., .FALSE.,                                                &         ! xstag, ystag
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%chstarxy, 1 ) * SIZE( grid%chstarxy, 2 ) .GT. 1 ) THEN 
CALL copy_fcnm (  &         
                  grid%chstarxy,   &       ! CD field
                 cids, cide, 1, 1, cjds, cjde,   &         ! CD dims
                 cims, cime, 1, 1, cjms, cjme,   &         ! CD dims
                 cips, cipe, 1, 1, cjps, cjpe,   &         ! CD dims
                  ngrid%chstarxy,  &   ! ND field
                 nids, nide, 1, 1, njds, njde,   &         ! ND dims
                 nims, nime, 1, 1, njms, njme,   &         ! ND dims
                 nips, nipe, 1, 1, njps, njpe,   &         ! ND dims
                  config_flags%shw, ngrid%imask_nostag,         &         ! stencil half width
                  .FALSE., .FALSE.,                                                &         ! xstag, ystag
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%tsk_mosaic, 1 ) * SIZE( grid%tsk_mosaic, 3 ) .GT. 1 ) THEN 
CALL copy_fcnm (  &         
                  grid%tsk_mosaic,   &       ! CD field
                 cids, cide, 1, config_flags%mosaic_cat, cjds, cjde,   &         ! CD dims
                 cims, cime, 1, config_flags%mosaic_cat, cjms, cjme,   &         ! CD dims
                 cips, cipe, 1, config_flags%mosaic_cat, cjps, cjpe,   &         ! CD dims
                  ngrid%tsk_mosaic,  &   ! ND field
                 nids, nide, 1, config_flags%mosaic_cat, njds, njde,   &         ! ND dims
                 nims, nime, 1, config_flags%mosaic_cat, njms, njme,   &         ! ND dims
                 nips, nipe, 1, config_flags%mosaic_cat, njps, njpe,   &         ! ND dims
                  config_flags%shw, ngrid%imask_nostag,         &         ! stencil half width
                  .FALSE., .FALSE.,                                                &         ! xstag, ystag
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%qsfc_mosaic, 1 ) * SIZE( grid%qsfc_mosaic, 3 ) .GT. 1 ) THEN 
CALL copy_fcnm (  &         
                  grid%qsfc_mosaic,   &       ! CD field
                 cids, cide, 1, config_flags%mosaic_cat, cjds, cjde,   &         ! CD dims
                 cims, cime, 1, config_flags%mosaic_cat, cjms, cjme,   &         ! CD dims
                 cips, cipe, 1, config_flags%mosaic_cat, cjps, cjpe,   &         ! CD dims
                  ngrid%qsfc_mosaic,  &   ! ND field
                 nids, nide, 1, config_flags%mosaic_cat, njds, njde,   &         ! ND dims
                 nims, nime, 1, config_flags%mosaic_cat, njms, njme,   &         ! ND dims
                 nips, nipe, 1, config_flags%mosaic_cat, njps, njpe,   &         ! ND dims
                  config_flags%shw, ngrid%imask_nostag,         &         ! stencil half width
                  .FALSE., .FALSE.,                                                &         ! xstag, ystag
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%tslb_mosaic, 1 ) * SIZE( grid%tslb_mosaic, 3 ) .GT. 1 ) THEN 
CALL copy_fcnm (  &         
                  grid%tslb_mosaic,   &       ! CD field
                 cids, cide, 1, config_flags%mosaic_cat_soil, cjds, cjde,   &         ! CD dims
                 cims, cime, 1, config_flags%mosaic_cat_soil, cjms, cjme,   &         ! CD dims
                 cips, cipe, 1, config_flags%mosaic_cat_soil, cjps, cjpe,   &         ! CD dims
                  ngrid%tslb_mosaic,  &   ! ND field
                 nids, nide, 1, config_flags%mosaic_cat_soil, njds, njde,   &         ! ND dims
                 nims, nime, 1, config_flags%mosaic_cat_soil, njms, njme,   &         ! ND dims
                 nips, nipe, 1, config_flags%mosaic_cat_soil, njps, njpe,   &         ! ND dims
                  config_flags%shw, ngrid%imask_nostag,         &         ! stencil half width
                  .FALSE., .FALSE.,                                                &         ! xstag, ystag
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%smois_mosaic, 1 ) * SIZE( grid%smois_mosaic, 3 ) .GT. 1 ) THEN 
CALL copy_fcnm (  &         
                  grid%smois_mosaic,   &       ! CD field
                 cids, cide, 1, config_flags%mosaic_cat_soil, cjds, cjde,   &         ! CD dims
                 cims, cime, 1, config_flags%mosaic_cat_soil, cjms, cjme,   &         ! CD dims
                 cips, cipe, 1, config_flags%mosaic_cat_soil, cjps, cjpe,   &         ! CD dims
                  ngrid%smois_mosaic,  &   ! ND field
                 nids, nide, 1, config_flags%mosaic_cat_soil, njds, njde,   &         ! ND dims
                 nims, nime, 1, config_flags%mosaic_cat_soil, njms, njme,   &         ! ND dims
                 nips, nipe, 1, config_flags%mosaic_cat_soil, njps, njpe,   &         ! ND dims
                  config_flags%shw, ngrid%imask_nostag,         &         ! stencil half width
                  .FALSE., .FALSE.,                                                &         ! xstag, ystag
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%sh2o_mosaic, 1 ) * SIZE( grid%sh2o_mosaic, 3 ) .GT. 1 ) THEN 
CALL copy_fcnm (  &         
                  grid%sh2o_mosaic,   &       ! CD field
                 cids, cide, 1, config_flags%mosaic_cat_soil, cjds, cjde,   &         ! CD dims
                 cims, cime, 1, config_flags%mosaic_cat_soil, cjms, cjme,   &         ! CD dims
                 cips, cipe, 1, config_flags%mosaic_cat_soil, cjps, cjpe,   &         ! CD dims
                  ngrid%sh2o_mosaic,  &   ! ND field
                 nids, nide, 1, config_flags%mosaic_cat_soil, njds, njde,   &         ! ND dims
                 nims, nime, 1, config_flags%mosaic_cat_soil, njms, njme,   &         ! ND dims
                 nips, nipe, 1, config_flags%mosaic_cat_soil, njps, njpe,   &         ! ND dims
                  config_flags%shw, ngrid%imask_nostag,         &         ! stencil half width
                  .FALSE., .FALSE.,                                                &         ! xstag, ystag
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%canwat_mosaic, 1 ) * SIZE( grid%canwat_mosaic, 3 ) .GT. 1 ) THEN 
CALL copy_fcnm (  &         
                  grid%canwat_mosaic,   &       ! CD field
                 cids, cide, 1, config_flags%mosaic_cat, cjds, cjde,   &         ! CD dims
                 cims, cime, 1, config_flags%mosaic_cat, cjms, cjme,   &         ! CD dims
                 cips, cipe, 1, config_flags%mosaic_cat, cjps, cjpe,   &         ! CD dims
                  ngrid%canwat_mosaic,  &   ! ND field
                 nids, nide, 1, config_flags%mosaic_cat, njds, njde,   &         ! ND dims
                 nims, nime, 1, config_flags%mosaic_cat, njms, njme,   &         ! ND dims
                 nips, nipe, 1, config_flags%mosaic_cat, njps, njpe,   &         ! ND dims
                  config_flags%shw, ngrid%imask_nostag,         &         ! stencil half width
                  .FALSE., .FALSE.,                                                &         ! xstag, ystag
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%snow_mosaic, 1 ) * SIZE( grid%snow_mosaic, 3 ) .GT. 1 ) THEN 
CALL copy_fcnm (  &         
                  grid%snow_mosaic,   &       ! CD field
                 cids, cide, 1, config_flags%mosaic_cat, cjds, cjde,   &         ! CD dims
                 cims, cime, 1, config_flags%mosaic_cat, cjms, cjme,   &         ! CD dims
                 cips, cipe, 1, config_flags%mosaic_cat, cjps, cjpe,   &         ! CD dims
                  ngrid%snow_mosaic,  &   ! ND field
                 nids, nide, 1, config_flags%mosaic_cat, njds, njde,   &         ! ND dims
                 nims, nime, 1, config_flags%mosaic_cat, njms, njme,   &         ! ND dims
                 nips, nipe, 1, config_flags%mosaic_cat, njps, njpe,   &         ! ND dims
                  config_flags%shw, ngrid%imask_nostag,         &         ! stencil half width
                  .FALSE., .FALSE.,                                                &         ! xstag, ystag
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%snowh_mosaic, 1 ) * SIZE( grid%snowh_mosaic, 3 ) .GT. 1 ) THEN 
CALL copy_fcnm (  &         
                  grid%snowh_mosaic,   &       ! CD field
                 cids, cide, 1, config_flags%mosaic_cat, cjds, cjde,   &         ! CD dims
                 cims, cime, 1, config_flags%mosaic_cat, cjms, cjme,   &         ! CD dims
                 cips, cipe, 1, config_flags%mosaic_cat, cjps, cjpe,   &         ! CD dims
                  ngrid%snowh_mosaic,  &   ! ND field
                 nids, nide, 1, config_flags%mosaic_cat, njds, njde,   &         ! ND dims
                 nims, nime, 1, config_flags%mosaic_cat, njms, njme,   &         ! ND dims
                 nips, nipe, 1, config_flags%mosaic_cat, njps, njpe,   &         ! ND dims
                  config_flags%shw, ngrid%imask_nostag,         &         ! stencil half width
                  .FALSE., .FALSE.,                                                &         ! xstag, ystag
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%snowc_mosaic, 1 ) * SIZE( grid%snowc_mosaic, 3 ) .GT. 1 ) THEN 
CALL copy_fcnm (  &         
                  grid%snowc_mosaic,   &       ! CD field
                 cids, cide, 1, config_flags%mosaic_cat, cjds, cjde,   &         ! CD dims
                 cims, cime, 1, config_flags%mosaic_cat, cjms, cjme,   &         ! CD dims
                 cips, cipe, 1, config_flags%mosaic_cat, cjps, cjpe,   &         ! CD dims
                  ngrid%snowc_mosaic,  &   ! ND field
                 nids, nide, 1, config_flags%mosaic_cat, njds, njde,   &         ! ND dims
                 nims, nime, 1, config_flags%mosaic_cat, njms, njme,   &         ! ND dims
                 nips, nipe, 1, config_flags%mosaic_cat, njps, njpe,   &         ! ND dims
                  config_flags%shw, ngrid%imask_nostag,         &         ! stencil half width
                  .FALSE., .FALSE.,                                                &         ! xstag, ystag
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%tr_urb2d_mosaic, 1 ) * SIZE( grid%tr_urb2d_mosaic, 3 ) .GT. 1 ) THEN 
CALL copy_fcnm (  &         
                  grid%tr_urb2d_mosaic,   &       ! CD field
                 cids, cide, 1, config_flags%mosaic_cat, cjds, cjde,   &         ! CD dims
                 cims, cime, 1, config_flags%mosaic_cat, cjms, cjme,   &         ! CD dims
                 cips, cipe, 1, config_flags%mosaic_cat, cjps, cjpe,   &         ! CD dims
                  ngrid%tr_urb2d_mosaic,  &   ! ND field
                 nids, nide, 1, config_flags%mosaic_cat, njds, njde,   &         ! ND dims
                 nims, nime, 1, config_flags%mosaic_cat, njms, njme,   &         ! ND dims
                 nips, nipe, 1, config_flags%mosaic_cat, njps, njpe,   &         ! ND dims
                  config_flags%shw, ngrid%imask_nostag,         &         ! stencil half width
                  .FALSE., .FALSE.,                                                &         ! xstag, ystag
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%tb_urb2d_mosaic, 1 ) * SIZE( grid%tb_urb2d_mosaic, 3 ) .GT. 1 ) THEN 
CALL copy_fcnm (  &         
                  grid%tb_urb2d_mosaic,   &       ! CD field
                 cids, cide, 1, config_flags%mosaic_cat, cjds, cjde,   &         ! CD dims
                 cims, cime, 1, config_flags%mosaic_cat, cjms, cjme,   &         ! CD dims
                 cips, cipe, 1, config_flags%mosaic_cat, cjps, cjpe,   &         ! CD dims
                  ngrid%tb_urb2d_mosaic,  &   ! ND field
                 nids, nide, 1, config_flags%mosaic_cat, njds, njde,   &         ! ND dims
                 nims, nime, 1, config_flags%mosaic_cat, njms, njme,   &         ! ND dims
                 nips, nipe, 1, config_flags%mosaic_cat, njps, njpe,   &         ! ND dims
                  config_flags%shw, ngrid%imask_nostag,         &         ! stencil half width
                  .FALSE., .FALSE.,                                                &         ! xstag, ystag
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%tg_urb2d_mosaic, 1 ) * SIZE( grid%tg_urb2d_mosaic, 3 ) .GT. 1 ) THEN 
CALL copy_fcnm (  &         
                  grid%tg_urb2d_mosaic,   &       ! CD field
                 cids, cide, 1, config_flags%mosaic_cat, cjds, cjde,   &         ! CD dims
                 cims, cime, 1, config_flags%mosaic_cat, cjms, cjme,   &         ! CD dims
                 cips, cipe, 1, config_flags%mosaic_cat, cjps, cjpe,   &         ! CD dims
                  ngrid%tg_urb2d_mosaic,  &   ! ND field
                 nids, nide, 1, config_flags%mosaic_cat, njds, njde,   &         ! ND dims
                 nims, nime, 1, config_flags%mosaic_cat, njms, njme,   &         ! ND dims
                 nips, nipe, 1, config_flags%mosaic_cat, njps, njpe,   &         ! ND dims
                  config_flags%shw, ngrid%imask_nostag,         &         ! stencil half width
                  .FALSE., .FALSE.,                                                &         ! xstag, ystag
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%tc_urb2d_mosaic, 1 ) * SIZE( grid%tc_urb2d_mosaic, 3 ) .GT. 1 ) THEN 
CALL copy_fcnm (  &         
                  grid%tc_urb2d_mosaic,   &       ! CD field
                 cids, cide, 1, config_flags%mosaic_cat, cjds, cjde,   &         ! CD dims
                 cims, cime, 1, config_flags%mosaic_cat, cjms, cjme,   &         ! CD dims
                 cips, cipe, 1, config_flags%mosaic_cat, cjps, cjpe,   &         ! CD dims
                  ngrid%tc_urb2d_mosaic,  &   ! ND field
                 nids, nide, 1, config_flags%mosaic_cat, njds, njde,   &         ! ND dims
                 nims, nime, 1, config_flags%mosaic_cat, njms, njme,   &         ! ND dims
                 nips, nipe, 1, config_flags%mosaic_cat, njps, njpe,   &         ! ND dims
                  config_flags%shw, ngrid%imask_nostag,         &         ! stencil half width
                  .FALSE., .FALSE.,                                                &         ! xstag, ystag
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%ts_urb2d_mosaic, 1 ) * SIZE( grid%ts_urb2d_mosaic, 3 ) .GT. 1 ) THEN 
CALL copy_fcnm (  &         
                  grid%ts_urb2d_mosaic,   &       ! CD field
                 cids, cide, 1, config_flags%mosaic_cat, cjds, cjde,   &         ! CD dims
                 cims, cime, 1, config_flags%mosaic_cat, cjms, cjme,   &         ! CD dims
                 cips, cipe, 1, config_flags%mosaic_cat, cjps, cjpe,   &         ! CD dims
                  ngrid%ts_urb2d_mosaic,  &   ! ND field
                 nids, nide, 1, config_flags%mosaic_cat, njds, njde,   &         ! ND dims
                 nims, nime, 1, config_flags%mosaic_cat, njms, njme,   &         ! ND dims
                 nips, nipe, 1, config_flags%mosaic_cat, njps, njpe,   &         ! ND dims
                  config_flags%shw, ngrid%imask_nostag,         &         ! stencil half width
                  .FALSE., .FALSE.,                                                &         ! xstag, ystag
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%ts_rul2d_mosaic, 1 ) * SIZE( grid%ts_rul2d_mosaic, 3 ) .GT. 1 ) THEN 
CALL copy_fcnm (  &         
                  grid%ts_rul2d_mosaic,   &       ! CD field
                 cids, cide, 1, config_flags%mosaic_cat, cjds, cjde,   &         ! CD dims
                 cims, cime, 1, config_flags%mosaic_cat, cjms, cjme,   &         ! CD dims
                 cips, cipe, 1, config_flags%mosaic_cat, cjps, cjpe,   &         ! CD dims
                  ngrid%ts_rul2d_mosaic,  &   ! ND field
                 nids, nide, 1, config_flags%mosaic_cat, njds, njde,   &         ! ND dims
                 nims, nime, 1, config_flags%mosaic_cat, njms, njme,   &         ! ND dims
                 nips, nipe, 1, config_flags%mosaic_cat, njps, njpe,   &         ! ND dims
                  config_flags%shw, ngrid%imask_nostag,         &         ! stencil half width
                  .FALSE., .FALSE.,                                                &         ! xstag, ystag
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%qc_urb2d_mosaic, 1 ) * SIZE( grid%qc_urb2d_mosaic, 3 ) .GT. 1 ) THEN 
CALL copy_fcnm (  &         
                  grid%qc_urb2d_mosaic,   &       ! CD field
                 cids, cide, 1, config_flags%mosaic_cat, cjds, cjde,   &         ! CD dims
                 cims, cime, 1, config_flags%mosaic_cat, cjms, cjme,   &         ! CD dims
                 cips, cipe, 1, config_flags%mosaic_cat, cjps, cjpe,   &         ! CD dims
                  ngrid%qc_urb2d_mosaic,  &   ! ND field
                 nids, nide, 1, config_flags%mosaic_cat, njds, njde,   &         ! ND dims
                 nims, nime, 1, config_flags%mosaic_cat, njms, njme,   &         ! ND dims
                 nips, nipe, 1, config_flags%mosaic_cat, njps, njpe,   &         ! ND dims
                  config_flags%shw, ngrid%imask_nostag,         &         ! stencil half width
                  .FALSE., .FALSE.,                                                &         ! xstag, ystag
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%uc_urb2d_mosaic, 1 ) * SIZE( grid%uc_urb2d_mosaic, 3 ) .GT. 1 ) THEN 
CALL copy_fcnm (  &         
                  grid%uc_urb2d_mosaic,   &       ! CD field
                 cids, cide, 1, config_flags%mosaic_cat, cjds, cjde,   &         ! CD dims
                 cims, cime, 1, config_flags%mosaic_cat, cjms, cjme,   &         ! CD dims
                 cips, cipe, 1, config_flags%mosaic_cat, cjps, cjpe,   &         ! CD dims
                  ngrid%uc_urb2d_mosaic,  &   ! ND field
                 nids, nide, 1, config_flags%mosaic_cat, njds, njde,   &         ! ND dims
                 nims, nime, 1, config_flags%mosaic_cat, njms, njme,   &         ! ND dims
                 nips, nipe, 1, config_flags%mosaic_cat, njps, njpe,   &         ! ND dims
                  config_flags%shw, ngrid%imask_nostag,         &         ! stencil half width
                  .FALSE., .FALSE.,                                                &         ! xstag, ystag
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%trl_urb3d_mosaic, 1 ) * SIZE( grid%trl_urb3d_mosaic, 3 ) .GT. 1 ) THEN 
CALL copy_fcnm (  &         
                  grid%trl_urb3d_mosaic,   &       ! CD field
                 cids, cide, 1, config_flags%mosaic_cat_soil, cjds, cjde,   &         ! CD dims
                 cims, cime, 1, config_flags%mosaic_cat_soil, cjms, cjme,   &         ! CD dims
                 cips, cipe, 1, config_flags%mosaic_cat_soil, cjps, cjpe,   &         ! CD dims
                  ngrid%trl_urb3d_mosaic,  &   ! ND field
                 nids, nide, 1, config_flags%mosaic_cat_soil, njds, njde,   &         ! ND dims
                 nims, nime, 1, config_flags%mosaic_cat_soil, njms, njme,   &         ! ND dims
                 nips, nipe, 1, config_flags%mosaic_cat_soil, njps, njpe,   &         ! ND dims
                  config_flags%shw, ngrid%imask_nostag,         &         ! stencil half width
                  .FALSE., .FALSE.,                                                &         ! xstag, ystag
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%tbl_urb3d_mosaic, 1 ) * SIZE( grid%tbl_urb3d_mosaic, 3 ) .GT. 1 ) THEN 
CALL copy_fcnm (  &         
                  grid%tbl_urb3d_mosaic,   &       ! CD field
                 cids, cide, 1, config_flags%mosaic_cat_soil, cjds, cjde,   &         ! CD dims
                 cims, cime, 1, config_flags%mosaic_cat_soil, cjms, cjme,   &         ! CD dims
                 cips, cipe, 1, config_flags%mosaic_cat_soil, cjps, cjpe,   &         ! CD dims
                  ngrid%tbl_urb3d_mosaic,  &   ! ND field
                 nids, nide, 1, config_flags%mosaic_cat_soil, njds, njde,   &         ! ND dims
                 nims, nime, 1, config_flags%mosaic_cat_soil, njms, njme,   &         ! ND dims
                 nips, nipe, 1, config_flags%mosaic_cat_soil, njps, njpe,   &         ! ND dims
                  config_flags%shw, ngrid%imask_nostag,         &         ! stencil half width
                  .FALSE., .FALSE.,                                                &         ! xstag, ystag
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%tgl_urb3d_mosaic, 1 ) * SIZE( grid%tgl_urb3d_mosaic, 3 ) .GT. 1 ) THEN 
CALL copy_fcnm (  &         
                  grid%tgl_urb3d_mosaic,   &       ! CD field
                 cids, cide, 1, config_flags%mosaic_cat_soil, cjds, cjde,   &         ! CD dims
                 cims, cime, 1, config_flags%mosaic_cat_soil, cjms, cjme,   &         ! CD dims
                 cips, cipe, 1, config_flags%mosaic_cat_soil, cjps, cjpe,   &         ! CD dims
                  ngrid%tgl_urb3d_mosaic,  &   ! ND field
                 nids, nide, 1, config_flags%mosaic_cat_soil, njds, njde,   &         ! ND dims
                 nims, nime, 1, config_flags%mosaic_cat_soil, njms, njme,   &         ! ND dims
                 nips, nipe, 1, config_flags%mosaic_cat_soil, njps, njpe,   &         ! ND dims
                  config_flags%shw, ngrid%imask_nostag,         &         ! stencil half width
                  .FALSE., .FALSE.,                                                &         ! xstag, ystag
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%mosaic_cat_index, 1 ) * SIZE( grid%mosaic_cat_index, 3 ) .GT. 1 ) THEN 
CALL copy_fcnm (  &         
                  grid%mosaic_cat_index,   &       ! CD field
                 cids, cide, 1, config_flags%num_land_cat, cjds, cjde,   &         ! CD dims
                 cims, cime, 1, config_flags%num_land_cat, cjms, cjme,   &         ! CD dims
                 cips, cipe, 1, config_flags%num_land_cat, cjps, cjpe,   &         ! CD dims
                  ngrid%mosaic_cat_index,  &   ! ND field
                 nids, nide, 1, config_flags%num_land_cat, njds, njde,   &         ! ND dims
                 nims, nime, 1, config_flags%num_land_cat, njms, njme,   &         ! ND dims
                 nips, nipe, 1, config_flags%num_land_cat, njps, njpe,   &         ! ND dims
                  config_flags%shw, ngrid%imask_nostag,         &         ! stencil half width
                  .FALSE., .FALSE.,                                                &         ! xstag, ystag
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%landusef2, 1 ) * SIZE( grid%landusef2, 3 ) .GT. 1 ) THEN 
CALL copy_fcnm (  &         
                  grid%landusef2,   &       ! CD field
                 cids, cide, 1, config_flags%num_land_cat, cjds, cjde,   &         ! CD dims
                 cims, cime, 1, config_flags%num_land_cat, cjms, cjme,   &         ! CD dims
                 cips, cipe, 1, config_flags%num_land_cat, cjps, cjpe,   &         ! CD dims
                  ngrid%landusef2,  &   ! ND field
                 nids, nide, 1, config_flags%num_land_cat, njds, njde,   &         ! ND dims
                 nims, nime, 1, config_flags%num_land_cat, njms, njme,   &         ! ND dims
                 nips, nipe, 1, config_flags%num_land_cat, njps, njpe,   &         ! ND dims
                  config_flags%shw, ngrid%imask_nostag,         &         ! stencil half width
                  .FALSE., .FALSE.,                                                &         ! xstag, ystag
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%tmn, 1 ) * SIZE( grid%tmn, 2 ) .GT. 1 ) THEN 
CALL copy_fcnm (  &         
                  grid%tmn,   &       ! CD field
                 cids, cide, 1, 1, cjds, cjde,   &         ! CD dims
                 cims, cime, 1, 1, cjms, cjme,   &         ! CD dims
                 cips, cipe, 1, 1, cjps, cjpe,   &         ! CD dims
                  ngrid%tmn,  &   ! ND field
                 nids, nide, 1, 1, njds, njde,   &         ! ND dims
                 nims, nime, 1, 1, njms, njme,   &         ! ND dims
                 nips, nipe, 1, 1, njps, njpe,   &         ! ND dims
                  config_flags%shw, ngrid%imask_nostag,         &         ! stencil half width
                  .FALSE., .FALSE.,                                                &         ! xstag, ystag
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%tyr, 1 ) * SIZE( grid%tyr, 2 ) .GT. 1 ) THEN 
CALL copy_fcnm (  &         
                  grid%tyr,   &       ! CD field
                 cids, cide, 1, 1, cjds, cjde,   &         ! CD dims
                 cims, cime, 1, 1, cjms, cjme,   &         ! CD dims
                 cips, cipe, 1, 1, cjps, cjpe,   &         ! CD dims
                  ngrid%tyr,  &   ! ND field
                 nids, nide, 1, 1, njds, njde,   &         ! ND dims
                 nims, nime, 1, 1, njms, njme,   &         ! ND dims
                 nips, nipe, 1, 1, njps, njpe,   &         ! ND dims
                  config_flags%shw, ngrid%imask_nostag,         &         ! stencil half width
                  .FALSE., .FALSE.,                                                &         ! xstag, ystag
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%tyra, 1 ) * SIZE( grid%tyra, 2 ) .GT. 1 ) THEN 
CALL copy_fcnm (  &         
                  grid%tyra,   &       ! CD field
                 cids, cide, 1, 1, cjds, cjde,   &         ! CD dims
                 cims, cime, 1, 1, cjms, cjme,   &         ! CD dims
                 cips, cipe, 1, 1, cjps, cjpe,   &         ! CD dims
                  ngrid%tyra,  &   ! ND field
                 nids, nide, 1, 1, njds, njde,   &         ! ND dims
                 nims, nime, 1, 1, njms, njme,   &         ! ND dims
                 nips, nipe, 1, 1, njps, njpe,   &         ! ND dims
                  config_flags%shw, ngrid%imask_nostag,         &         ! stencil half width
                  .FALSE., .FALSE.,                                                &         ! xstag, ystag
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%tdly, 1 ) * SIZE( grid%tdly, 2 ) .GT. 1 ) THEN 
CALL copy_fcnm (  &         
                  grid%tdly,   &       ! CD field
                 cids, cide, 1, 1, cjds, cjde,   &         ! CD dims
                 cims, cime, 1, 1, cjms, cjme,   &         ! CD dims
                 cips, cipe, 1, 1, cjps, cjpe,   &         ! CD dims
                  ngrid%tdly,  &   ! ND field
                 nids, nide, 1, 1, njds, njde,   &         ! ND dims
                 nims, nime, 1, 1, njms, njme,   &         ! ND dims
                 nips, nipe, 1, 1, njps, njpe,   &         ! ND dims
                  config_flags%shw, ngrid%imask_nostag,         &         ! stencil half width
                  .FALSE., .FALSE.,                                                &         ! xstag, ystag
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%tlag, 1 ) * SIZE( grid%tlag, 3 ) .GT. 1 ) THEN 
CALL copy_fcnm (  &         
                  grid%tlag,   &       ! CD field
                 cids, cide, 1, config_flags%lagday, cjds, cjde,   &         ! CD dims
                 cims, cime, 1, config_flags%lagday, cjms, cjme,   &         ! CD dims
                 cips, cipe, 1, config_flags%lagday, cjps, cjpe,   &         ! CD dims
                  ngrid%tlag,  &   ! ND field
                 nids, nide, 1, config_flags%lagday, njds, njde,   &         ! ND dims
                 nims, nime, 1, config_flags%lagday, njms, njme,   &         ! ND dims
                 nips, nipe, 1, config_flags%lagday, njps, njpe,   &         ! ND dims
                  config_flags%shw, ngrid%imask_nostag,         &         ! stencil half width
                  .FALSE., .FALSE.,                                                &         ! xstag, ystag
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%xland, 1 ) * SIZE( grid%xland, 2 ) .GT. 1 ) THEN 
CALL copy_fcnm (  &         
                  grid%xland,   &       ! CD field
                 cids, cide, 1, 1, cjds, cjde,   &         ! CD dims
                 cims, cime, 1, 1, cjms, cjme,   &         ! CD dims
                 cips, cipe, 1, 1, cjps, cjpe,   &         ! CD dims
                  ngrid%xland,  &   ! ND field
                 nids, nide, 1, 1, njds, njde,   &         ! ND dims
                 nims, nime, 1, 1, njms, njme,   &         ! ND dims
                 nips, nipe, 1, 1, njps, njpe,   &         ! ND dims
                  config_flags%shw, ngrid%imask_nostag,         &         ! stencil half width
                  .FALSE., .FALSE.,                                                &         ! xstag, ystag
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%achfx, 1 ) * SIZE( grid%achfx, 2 ) .GT. 1 ) THEN 
CALL copy_fcn (  &         
                  grid%achfx,   &       ! CD field
                 cids, cide, 1, 1, cjds, cjde,   &         ! CD dims
                 cims, cime, 1, 1, cjms, cjme,   &         ! CD dims
                 cips, cipe, 1, 1, cjps, cjpe,   &         ! CD dims
                  ngrid%achfx,  &   ! ND field
                 nids, nide, 1, 1, njds, njde,   &         ! ND dims
                 nims, nime, 1, 1, njms, njme,   &         ! ND dims
                 nips, nipe, 1, 1, njps, njpe,   &         ! ND dims
                  config_flags%shw, ngrid%imask_nostag,         &         ! stencil half width
                  .FALSE., .FALSE.,                                                &         ! xstag, ystag
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%aclhf, 1 ) * SIZE( grid%aclhf, 2 ) .GT. 1 ) THEN 
CALL copy_fcn (  &         
                  grid%aclhf,   &       ! CD field
                 cids, cide, 1, 1, cjds, cjde,   &         ! CD dims
                 cims, cime, 1, 1, cjms, cjme,   &         ! CD dims
                 cips, cipe, 1, 1, cjps, cjpe,   &         ! CD dims
                  ngrid%aclhf,  &   ! ND field
                 nids, nide, 1, 1, njds, njde,   &         ! ND dims
                 nims, nime, 1, 1, njms, njme,   &         ! ND dims
                 nips, nipe, 1, 1, njps, njpe,   &         ! ND dims
                  config_flags%shw, ngrid%imask_nostag,         &         ! stencil half width
                  .FALSE., .FALSE.,                                                &         ! xstag, ystag
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%snowc, 1 ) * SIZE( grid%snowc, 2 ) .GT. 1 ) THEN 
CALL copy_fcnm (  &         
                  grid%snowc,   &       ! CD field
                 cids, cide, 1, 1, cjds, cjde,   &         ! CD dims
                 cims, cime, 1, 1, cjms, cjme,   &         ! CD dims
                 cips, cipe, 1, 1, cjps, cjpe,   &         ! CD dims
                  ngrid%snowc,  &   ! ND field
                 nids, nide, 1, 1, njds, njde,   &         ! ND dims
                 nims, nime, 1, 1, njms, njme,   &         ! ND dims
                 nips, nipe, 1, 1, njps, njpe,   &         ! ND dims
                  config_flags%shw, ngrid%imask_nostag,         &         ! stencil half width
                  .FALSE., .FALSE.,                                                &         ! xstag, ystag
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%tml, 1 ) * SIZE( grid%tml, 2 ) .GT. 1 ) THEN 
CALL copy_fcnm (  &         
                  grid%tml,   &       ! CD field
                 cids, cide, 1, 1, cjds, cjde,   &         ! CD dims
                 cims, cime, 1, 1, cjms, cjme,   &         ! CD dims
                 cips, cipe, 1, 1, cjps, cjpe,   &         ! CD dims
                  ngrid%tml,  &   ! ND field
                 nids, nide, 1, 1, njds, njde,   &         ! ND dims
                 nims, nime, 1, 1, njms, njme,   &         ! ND dims
                 nips, nipe, 1, 1, njps, njpe,   &         ! ND dims
                  config_flags%shw, ngrid%imask_nostag,         &         ! stencil half width
                  .FALSE., .FALSE.,                                                &         ! xstag, ystag
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%t0ml, 1 ) * SIZE( grid%t0ml, 2 ) .GT. 1 ) THEN 
CALL copy_fcnm (  &         
                  grid%t0ml,   &       ! CD field
                 cids, cide, 1, 1, cjds, cjde,   &         ! CD dims
                 cims, cime, 1, 1, cjms, cjme,   &         ! CD dims
                 cips, cipe, 1, 1, cjps, cjpe,   &         ! CD dims
                  ngrid%t0ml,  &   ! ND field
                 nids, nide, 1, 1, njds, njde,   &         ! ND dims
                 nims, nime, 1, 1, njms, njme,   &         ! ND dims
                 nips, nipe, 1, 1, njps, njpe,   &         ! ND dims
                  config_flags%shw, ngrid%imask_nostag,         &         ! stencil half width
                  .FALSE., .FALSE.,                                                &         ! xstag, ystag
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%hml, 1 ) * SIZE( grid%hml, 2 ) .GT. 1 ) THEN 
CALL copy_fcnm (  &         
                  grid%hml,   &       ! CD field
                 cids, cide, 1, 1, cjds, cjde,   &         ! CD dims
                 cims, cime, 1, 1, cjms, cjme,   &         ! CD dims
                 cips, cipe, 1, 1, cjps, cjpe,   &         ! CD dims
                  ngrid%hml,  &   ! ND field
                 nids, nide, 1, 1, njds, njde,   &         ! ND dims
                 nims, nime, 1, 1, njms, njme,   &         ! ND dims
                 nips, nipe, 1, 1, njps, njpe,   &         ! ND dims
                  config_flags%shw, ngrid%imask_nostag,         &         ! stencil half width
                  .FALSE., .FALSE.,                                                &         ! xstag, ystag
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%h0ml, 1 ) * SIZE( grid%h0ml, 2 ) .GT. 1 ) THEN 
CALL copy_fcnm (  &         
                  grid%h0ml,   &       ! CD field
                 cids, cide, 1, 1, cjds, cjde,   &         ! CD dims
                 cims, cime, 1, 1, cjms, cjme,   &         ! CD dims
                 cips, cipe, 1, 1, cjps, cjpe,   &         ! CD dims
                  ngrid%h0ml,  &   ! ND field
                 nids, nide, 1, 1, njds, njde,   &         ! ND dims
                 nims, nime, 1, 1, njms, njme,   &         ! ND dims
                 nips, nipe, 1, 1, njps, njpe,   &         ! ND dims
                  config_flags%shw, ngrid%imask_nostag,         &         ! stencil half width
                  .FALSE., .FALSE.,                                                &         ! xstag, ystag
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%huml, 1 ) * SIZE( grid%huml, 2 ) .GT. 1 ) THEN 
CALL copy_fcnm (  &         
                  grid%huml,   &       ! CD field
                 cids, cide, 1, 1, cjds, cjde,   &         ! CD dims
                 cims, cime, 1, 1, cjms, cjme,   &         ! CD dims
                 cips, cipe, 1, 1, cjps, cjpe,   &         ! CD dims
                  ngrid%huml,  &   ! ND field
                 nids, nide, 1, 1, njds, njde,   &         ! ND dims
                 nims, nime, 1, 1, njms, njme,   &         ! ND dims
                 nips, nipe, 1, 1, njps, njpe,   &         ! ND dims
                  config_flags%shw, ngrid%imask_nostag,         &         ! stencil half width
                  .FALSE., .FALSE.,                                                &         ! xstag, ystag
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%hvml, 1 ) * SIZE( grid%hvml, 2 ) .GT. 1 ) THEN 
CALL copy_fcnm (  &         
                  grid%hvml,   &       ! CD field
                 cids, cide, 1, 1, cjds, cjde,   &         ! CD dims
                 cims, cime, 1, 1, cjms, cjme,   &         ! CD dims
                 cips, cipe, 1, 1, cjps, cjpe,   &         ! CD dims
                  ngrid%hvml,  &   ! ND field
                 nids, nide, 1, 1, njds, njde,   &         ! ND dims
                 nims, nime, 1, 1, njms, njme,   &         ! ND dims
                 nips, nipe, 1, 1, njps, njpe,   &         ! ND dims
                  config_flags%shw, ngrid%imask_nostag,         &         ! stencil half width
                  .FALSE., .FALSE.,                                                &         ! xstag, ystag
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%tmoml, 1 ) * SIZE( grid%tmoml, 2 ) .GT. 1 ) THEN 
CALL copy_fcnm (  &         
                  grid%tmoml,   &       ! CD field
                 cids, cide, 1, 1, cjds, cjde,   &         ! CD dims
                 cims, cime, 1, 1, cjms, cjme,   &         ! CD dims
                 cips, cipe, 1, 1, cjps, cjpe,   &         ! CD dims
                  ngrid%tmoml,  &   ! ND field
                 nids, nide, 1, 1, njds, njde,   &         ! ND dims
                 nims, nime, 1, 1, njms, njme,   &         ! ND dims
                 nips, nipe, 1, 1, njps, njpe,   &         ! ND dims
                  config_flags%shw, ngrid%imask_nostag,         &         ! stencil half width
                  .FALSE., .FALSE.,                                                &         ! xstag, ystag
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%numc, 1 ) * SIZE( grid%numc, 2 ) .GT. 1 ) THEN 
CALL copy_fcnm (  &         
                  grid%numc,   &       ! CD field
                 cids, cide, 1, 1, cjds, cjde,   &         ! CD dims
                 cims, cime, 1, 1, cjms, cjme,   &         ! CD dims
                 cips, cipe, 1, 1, cjps, cjpe,   &         ! CD dims
                  ngrid%numc,  &   ! ND field
                 nids, nide, 1, 1, njds, njde,   &         ! ND dims
                 nims, nime, 1, 1, njms, njme,   &         ! ND dims
                 nips, nipe, 1, 1, njps, njpe,   &         ! ND dims
                  config_flags%shw, ngrid%imask_nostag,         &         ! stencil half width
                  .FALSE., .FALSE.,                                                &         ! xstag, ystag
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%nump, 1 ) * SIZE( grid%nump, 2 ) .GT. 1 ) THEN 
CALL copy_fcnm (  &         
                  grid%nump,   &       ! CD field
                 cids, cide, 1, 1, cjds, cjde,   &         ! CD dims
                 cims, cime, 1, 1, cjms, cjme,   &         ! CD dims
                 cips, cipe, 1, 1, cjps, cjpe,   &         ! CD dims
                  ngrid%nump,  &   ! ND field
                 nids, nide, 1, 1, njds, njde,   &         ! ND dims
                 nims, nime, 1, 1, njms, njme,   &         ! ND dims
                 nips, nipe, 1, 1, njps, njpe,   &         ! ND dims
                  config_flags%shw, ngrid%imask_nostag,         &         ! stencil half width
                  .FALSE., .FALSE.,                                                &         ! xstag, ystag
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%snl, 1 ) * SIZE( grid%snl, 3 ) .GT. 1 ) THEN 
CALL copy_fcnm (  &         
                  grid%snl,   &       ! CD field
                 cids, cide, 1, config_flags%maxpatch, cjds, cjde,   &         ! CD dims
                 cims, cime, 1, config_flags%maxpatch, cjms, cjme,   &         ! CD dims
                 cips, cipe, 1, config_flags%maxpatch, cjps, cjpe,   &         ! CD dims
                  ngrid%snl,  &   ! ND field
                 nids, nide, 1, config_flags%maxpatch, njds, njde,   &         ! ND dims
                 nims, nime, 1, config_flags%maxpatch, njms, njme,   &         ! ND dims
                 nips, nipe, 1, config_flags%maxpatch, njps, njpe,   &         ! ND dims
                  config_flags%shw, ngrid%imask_nostag,         &         ! stencil half width
                  .FALSE., .FALSE.,                                                &         ! xstag, ystag
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%snowdp, 1 ) * SIZE( grid%snowdp, 3 ) .GT. 1 ) THEN 
CALL copy_fcnm (  &         
                  grid%snowdp,   &       ! CD field
                 cids, cide, 1, config_flags%maxpatch, cjds, cjde,   &         ! CD dims
                 cims, cime, 1, config_flags%maxpatch, cjms, cjme,   &         ! CD dims
                 cips, cipe, 1, config_flags%maxpatch, cjps, cjpe,   &         ! CD dims
                  ngrid%snowdp,  &   ! ND field
                 nids, nide, 1, config_flags%maxpatch, njds, njde,   &         ! ND dims
                 nims, nime, 1, config_flags%maxpatch, njms, njme,   &         ! ND dims
                 nips, nipe, 1, config_flags%maxpatch, njps, njpe,   &         ! ND dims
                  config_flags%shw, ngrid%imask_nostag,         &         ! stencil half width
                  .FALSE., .FALSE.,                                                &         ! xstag, ystag
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%wtc, 1 ) * SIZE( grid%wtc, 3 ) .GT. 1 ) THEN 
CALL copy_fcnm (  &         
                  grid%wtc,   &       ! CD field
                 cids, cide, 1, config_flags%maxpatch, cjds, cjde,   &         ! CD dims
                 cims, cime, 1, config_flags%maxpatch, cjms, cjme,   &         ! CD dims
                 cips, cipe, 1, config_flags%maxpatch, cjps, cjpe,   &         ! CD dims
                  ngrid%wtc,  &   ! ND field
                 nids, nide, 1, config_flags%maxpatch, njds, njde,   &         ! ND dims
                 nims, nime, 1, config_flags%maxpatch, njms, njme,   &         ! ND dims
                 nips, nipe, 1, config_flags%maxpatch, njps, njpe,   &         ! ND dims
                  config_flags%shw, ngrid%imask_nostag,         &         ! stencil half width
                  .FALSE., .FALSE.,                                                &         ! xstag, ystag
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%wtp, 1 ) * SIZE( grid%wtp, 3 ) .GT. 1 ) THEN 
CALL copy_fcnm (  &         
                  grid%wtp,   &       ! CD field
                 cids, cide, 1, config_flags%maxpatch, cjds, cjde,   &         ! CD dims
                 cims, cime, 1, config_flags%maxpatch, cjms, cjme,   &         ! CD dims
                 cips, cipe, 1, config_flags%maxpatch, cjps, cjpe,   &         ! CD dims
                  ngrid%wtp,  &   ! ND field
                 nids, nide, 1, config_flags%maxpatch, njds, njde,   &         ! ND dims
                 nims, nime, 1, config_flags%maxpatch, njms, njme,   &         ! ND dims
                 nips, nipe, 1, config_flags%maxpatch, njps, njpe,   &         ! ND dims
                  config_flags%shw, ngrid%imask_nostag,         &         ! stencil half width
                  .FALSE., .FALSE.,                                                &         ! xstag, ystag
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%h2osno, 1 ) * SIZE( grid%h2osno, 3 ) .GT. 1 ) THEN 
CALL copy_fcnm (  &         
                  grid%h2osno,   &       ! CD field
                 cids, cide, 1, config_flags%maxpatch, cjds, cjde,   &         ! CD dims
                 cims, cime, 1, config_flags%maxpatch, cjms, cjme,   &         ! CD dims
                 cips, cipe, 1, config_flags%maxpatch, cjps, cjpe,   &         ! CD dims
                  ngrid%h2osno,  &   ! ND field
                 nids, nide, 1, config_flags%maxpatch, njds, njde,   &         ! ND dims
                 nims, nime, 1, config_flags%maxpatch, njms, njme,   &         ! ND dims
                 nips, nipe, 1, config_flags%maxpatch, njps, njpe,   &         ! ND dims
                  config_flags%shw, ngrid%imask_nostag,         &         ! stencil half width
                  .FALSE., .FALSE.,                                                &         ! xstag, ystag
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%t_grnd, 1 ) * SIZE( grid%t_grnd, 3 ) .GT. 1 ) THEN 
CALL copy_fcnm (  &         
                  grid%t_grnd,   &       ! CD field
                 cids, cide, 1, config_flags%maxpatch, cjds, cjde,   &         ! CD dims
                 cims, cime, 1, config_flags%maxpatch, cjms, cjme,   &         ! CD dims
                 cips, cipe, 1, config_flags%maxpatch, cjps, cjpe,   &         ! CD dims
                  ngrid%t_grnd,  &   ! ND field
                 nids, nide, 1, config_flags%maxpatch, njds, njde,   &         ! ND dims
                 nims, nime, 1, config_flags%maxpatch, njms, njme,   &         ! ND dims
                 nips, nipe, 1, config_flags%maxpatch, njps, njpe,   &         ! ND dims
                  config_flags%shw, ngrid%imask_nostag,         &         ! stencil half width
                  .FALSE., .FALSE.,                                                &         ! xstag, ystag
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%t_veg, 1 ) * SIZE( grid%t_veg, 3 ) .GT. 1 ) THEN 
CALL copy_fcnm (  &         
                  grid%t_veg,   &       ! CD field
                 cids, cide, 1, config_flags%maxpatch, cjds, cjde,   &         ! CD dims
                 cims, cime, 1, config_flags%maxpatch, cjms, cjme,   &         ! CD dims
                 cips, cipe, 1, config_flags%maxpatch, cjps, cjpe,   &         ! CD dims
                  ngrid%t_veg,  &   ! ND field
                 nids, nide, 1, config_flags%maxpatch, njds, njde,   &         ! ND dims
                 nims, nime, 1, config_flags%maxpatch, njms, njme,   &         ! ND dims
                 nips, nipe, 1, config_flags%maxpatch, njps, njpe,   &         ! ND dims
                  config_flags%shw, ngrid%imask_nostag,         &         ! stencil half width
                  .FALSE., .FALSE.,                                                &         ! xstag, ystag
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%h2ocan, 1 ) * SIZE( grid%h2ocan, 3 ) .GT. 1 ) THEN 
CALL copy_fcnm (  &         
                  grid%h2ocan,   &       ! CD field
                 cids, cide, 1, config_flags%maxpatch, cjds, cjde,   &         ! CD dims
                 cims, cime, 1, config_flags%maxpatch, cjms, cjme,   &         ! CD dims
                 cips, cipe, 1, config_flags%maxpatch, cjps, cjpe,   &         ! CD dims
                  ngrid%h2ocan,  &   ! ND field
                 nids, nide, 1, config_flags%maxpatch, njds, njde,   &         ! ND dims
                 nims, nime, 1, config_flags%maxpatch, njms, njme,   &         ! ND dims
                 nips, nipe, 1, config_flags%maxpatch, njps, njpe,   &         ! ND dims
                  config_flags%shw, ngrid%imask_nostag,         &         ! stencil half width
                  .FALSE., .FALSE.,                                                &         ! xstag, ystag
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%h2ocan_col, 1 ) * SIZE( grid%h2ocan_col, 3 ) .GT. 1 ) THEN 
CALL copy_fcnm (  &         
                  grid%h2ocan_col,   &       ! CD field
                 cids, cide, 1, config_flags%maxpatch, cjds, cjde,   &         ! CD dims
                 cims, cime, 1, config_flags%maxpatch, cjms, cjme,   &         ! CD dims
                 cips, cipe, 1, config_flags%maxpatch, cjps, cjpe,   &         ! CD dims
                  ngrid%h2ocan_col,  &   ! ND field
                 nids, nide, 1, config_flags%maxpatch, njds, njde,   &         ! ND dims
                 nims, nime, 1, config_flags%maxpatch, njms, njme,   &         ! ND dims
                 nips, nipe, 1, config_flags%maxpatch, njps, njpe,   &         ! ND dims
                  config_flags%shw, ngrid%imask_nostag,         &         ! stencil half width
                  .FALSE., .FALSE.,                                                &         ! xstag, ystag
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%t2m_max, 1 ) * SIZE( grid%t2m_max, 2 ) .GT. 1 ) THEN 
CALL copy_fcnm (  &         
                  grid%t2m_max,   &       ! CD field
                 cids, cide, 1, 1, cjds, cjde,   &         ! CD dims
                 cims, cime, 1, 1, cjms, cjme,   &         ! CD dims
                 cips, cipe, 1, 1, cjps, cjpe,   &         ! CD dims
                  ngrid%t2m_max,  &   ! ND field
                 nids, nide, 1, 1, njds, njde,   &         ! ND dims
                 nims, nime, 1, 1, njms, njme,   &         ! ND dims
                 nips, nipe, 1, 1, njps, njpe,   &         ! ND dims
                  config_flags%shw, ngrid%imask_nostag,         &         ! stencil half width
                  .FALSE., .FALSE.,                                                &         ! xstag, ystag
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%t2m_min, 1 ) * SIZE( grid%t2m_min, 2 ) .GT. 1 ) THEN 
CALL copy_fcnm (  &         
                  grid%t2m_min,   &       ! CD field
                 cids, cide, 1, 1, cjds, cjde,   &         ! CD dims
                 cims, cime, 1, 1, cjms, cjme,   &         ! CD dims
                 cips, cipe, 1, 1, cjps, cjpe,   &         ! CD dims
                  ngrid%t2m_min,  &   ! ND field
                 nids, nide, 1, 1, njds, njde,   &         ! ND dims
                 nims, nime, 1, 1, njms, njme,   &         ! ND dims
                 nips, nipe, 1, 1, njps, njpe,   &         ! ND dims
                  config_flags%shw, ngrid%imask_nostag,         &         ! stencil half width
                  .FALSE., .FALSE.,                                                &         ! xstag, ystag
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%t2clm, 1 ) * SIZE( grid%t2clm, 2 ) .GT. 1 ) THEN 
CALL copy_fcnm (  &         
                  grid%t2clm,   &       ! CD field
                 cids, cide, 1, 1, cjds, cjde,   &         ! CD dims
                 cims, cime, 1, 1, cjms, cjme,   &         ! CD dims
                 cips, cipe, 1, 1, cjps, cjpe,   &         ! CD dims
                  ngrid%t2clm,  &   ! ND field
                 nids, nide, 1, 1, njds, njde,   &         ! ND dims
                 nims, nime, 1, 1, njms, njme,   &         ! ND dims
                 nips, nipe, 1, 1, njps, njpe,   &         ! ND dims
                  config_flags%shw, ngrid%imask_nostag,         &         ! stencil half width
                  .FALSE., .FALSE.,                                                &         ! xstag, ystag
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%t_ref2m, 1 ) * SIZE( grid%t_ref2m, 3 ) .GT. 1 ) THEN 
CALL copy_fcnm (  &         
                  grid%t_ref2m,   &       ! CD field
                 cids, cide, 1, config_flags%maxpatch, cjds, cjde,   &         ! CD dims
                 cims, cime, 1, config_flags%maxpatch, cjms, cjme,   &         ! CD dims
                 cips, cipe, 1, config_flags%maxpatch, cjps, cjpe,   &         ! CD dims
                  ngrid%t_ref2m,  &   ! ND field
                 nids, nide, 1, config_flags%maxpatch, njds, njde,   &         ! ND dims
                 nims, nime, 1, config_flags%maxpatch, njms, njme,   &         ! ND dims
                 nips, nipe, 1, config_flags%maxpatch, njps, njpe,   &         ! ND dims
                  config_flags%shw, ngrid%imask_nostag,         &         ! stencil half width
                  .FALSE., .FALSE.,                                                &         ! xstag, ystag
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%h2osoi_liq_s1, 1 ) * SIZE( grid%h2osoi_liq_s1, 3 ) .GT. 1 ) THEN 
CALL copy_fcnm (  &         
                  grid%h2osoi_liq_s1,   &       ! CD field
                 cids, cide, 1, config_flags%maxpatch, cjds, cjde,   &         ! CD dims
                 cims, cime, 1, config_flags%maxpatch, cjms, cjme,   &         ! CD dims
                 cips, cipe, 1, config_flags%maxpatch, cjps, cjpe,   &         ! CD dims
                  ngrid%h2osoi_liq_s1,  &   ! ND field
                 nids, nide, 1, config_flags%maxpatch, njds, njde,   &         ! ND dims
                 nims, nime, 1, config_flags%maxpatch, njms, njme,   &         ! ND dims
                 nips, nipe, 1, config_flags%maxpatch, njps, njpe,   &         ! ND dims
                  config_flags%shw, ngrid%imask_nostag,         &         ! stencil half width
                  .FALSE., .FALSE.,                                                &         ! xstag, ystag
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%h2osoi_liq_s2, 1 ) * SIZE( grid%h2osoi_liq_s2, 3 ) .GT. 1 ) THEN 
CALL copy_fcnm (  &         
                  grid%h2osoi_liq_s2,   &       ! CD field
                 cids, cide, 1, config_flags%maxpatch, cjds, cjde,   &         ! CD dims
                 cims, cime, 1, config_flags%maxpatch, cjms, cjme,   &         ! CD dims
                 cips, cipe, 1, config_flags%maxpatch, cjps, cjpe,   &         ! CD dims
                  ngrid%h2osoi_liq_s2,  &   ! ND field
                 nids, nide, 1, config_flags%maxpatch, njds, njde,   &         ! ND dims
                 nims, nime, 1, config_flags%maxpatch, njms, njme,   &         ! ND dims
                 nips, nipe, 1, config_flags%maxpatch, njps, njpe,   &         ! ND dims
                  config_flags%shw, ngrid%imask_nostag,         &         ! stencil half width
                  .FALSE., .FALSE.,                                                &         ! xstag, ystag
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%h2osoi_liq_s3, 1 ) * SIZE( grid%h2osoi_liq_s3, 3 ) .GT. 1 ) THEN 
CALL copy_fcnm (  &         
                  grid%h2osoi_liq_s3,   &       ! CD field
                 cids, cide, 1, config_flags%maxpatch, cjds, cjde,   &         ! CD dims
                 cims, cime, 1, config_flags%maxpatch, cjms, cjme,   &         ! CD dims
                 cips, cipe, 1, config_flags%maxpatch, cjps, cjpe,   &         ! CD dims
                  ngrid%h2osoi_liq_s3,  &   ! ND field
                 nids, nide, 1, config_flags%maxpatch, njds, njde,   &         ! ND dims
                 nims, nime, 1, config_flags%maxpatch, njms, njme,   &         ! ND dims
                 nips, nipe, 1, config_flags%maxpatch, njps, njpe,   &         ! ND dims
                  config_flags%shw, ngrid%imask_nostag,         &         ! stencil half width
                  .FALSE., .FALSE.,                                                &         ! xstag, ystag
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%h2osoi_liq_s4, 1 ) * SIZE( grid%h2osoi_liq_s4, 3 ) .GT. 1 ) THEN 
CALL copy_fcnm (  &         
                  grid%h2osoi_liq_s4,   &       ! CD field
                 cids, cide, 1, config_flags%maxpatch, cjds, cjde,   &         ! CD dims
                 cims, cime, 1, config_flags%maxpatch, cjms, cjme,   &         ! CD dims
                 cips, cipe, 1, config_flags%maxpatch, cjps, cjpe,   &         ! CD dims
                  ngrid%h2osoi_liq_s4,  &   ! ND field
                 nids, nide, 1, config_flags%maxpatch, njds, njde,   &         ! ND dims
                 nims, nime, 1, config_flags%maxpatch, njms, njme,   &         ! ND dims
                 nips, nipe, 1, config_flags%maxpatch, njps, njpe,   &         ! ND dims
                  config_flags%shw, ngrid%imask_nostag,         &         ! stencil half width
                  .FALSE., .FALSE.,                                                &         ! xstag, ystag
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%h2osoi_liq_s5, 1 ) * SIZE( grid%h2osoi_liq_s5, 3 ) .GT. 1 ) THEN 
CALL copy_fcnm (  &         
                  grid%h2osoi_liq_s5,   &       ! CD field
                 cids, cide, 1, config_flags%maxpatch, cjds, cjde,   &         ! CD dims
                 cims, cime, 1, config_flags%maxpatch, cjms, cjme,   &         ! CD dims
                 cips, cipe, 1, config_flags%maxpatch, cjps, cjpe,   &         ! CD dims
                  ngrid%h2osoi_liq_s5,  &   ! ND field
                 nids, nide, 1, config_flags%maxpatch, njds, njde,   &         ! ND dims
                 nims, nime, 1, config_flags%maxpatch, njms, njme,   &         ! ND dims
                 nips, nipe, 1, config_flags%maxpatch, njps, njpe,   &         ! ND dims
                  config_flags%shw, ngrid%imask_nostag,         &         ! stencil half width
                  .FALSE., .FALSE.,                                                &         ! xstag, ystag
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%h2osoi_liq1, 1 ) * SIZE( grid%h2osoi_liq1, 3 ) .GT. 1 ) THEN 
CALL copy_fcnm (  &         
                  grid%h2osoi_liq1,   &       ! CD field
                 cids, cide, 1, config_flags%maxpatch, cjds, cjde,   &         ! CD dims
                 cims, cime, 1, config_flags%maxpatch, cjms, cjme,   &         ! CD dims
                 cips, cipe, 1, config_flags%maxpatch, cjps, cjpe,   &         ! CD dims
                  ngrid%h2osoi_liq1,  &   ! ND field
                 nids, nide, 1, config_flags%maxpatch, njds, njde,   &         ! ND dims
                 nims, nime, 1, config_flags%maxpatch, njms, njme,   &         ! ND dims
                 nips, nipe, 1, config_flags%maxpatch, njps, njpe,   &         ! ND dims
                  config_flags%shw, ngrid%imask_nostag,         &         ! stencil half width
                  .FALSE., .FALSE.,                                                &         ! xstag, ystag
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%h2osoi_liq2, 1 ) * SIZE( grid%h2osoi_liq2, 3 ) .GT. 1 ) THEN 
CALL copy_fcnm (  &         
                  grid%h2osoi_liq2,   &       ! CD field
                 cids, cide, 1, config_flags%maxpatch, cjds, cjde,   &         ! CD dims
                 cims, cime, 1, config_flags%maxpatch, cjms, cjme,   &         ! CD dims
                 cips, cipe, 1, config_flags%maxpatch, cjps, cjpe,   &         ! CD dims
                  ngrid%h2osoi_liq2,  &   ! ND field
                 nids, nide, 1, config_flags%maxpatch, njds, njde,   &         ! ND dims
                 nims, nime, 1, config_flags%maxpatch, njms, njme,   &         ! ND dims
                 nips, nipe, 1, config_flags%maxpatch, njps, njpe,   &         ! ND dims
                  config_flags%shw, ngrid%imask_nostag,         &         ! stencil half width
                  .FALSE., .FALSE.,                                                &         ! xstag, ystag
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%h2osoi_liq3, 1 ) * SIZE( grid%h2osoi_liq3, 3 ) .GT. 1 ) THEN 
CALL copy_fcnm (  &         
                  grid%h2osoi_liq3,   &       ! CD field
                 cids, cide, 1, config_flags%maxpatch, cjds, cjde,   &         ! CD dims
                 cims, cime, 1, config_flags%maxpatch, cjms, cjme,   &         ! CD dims
                 cips, cipe, 1, config_flags%maxpatch, cjps, cjpe,   &         ! CD dims
                  ngrid%h2osoi_liq3,  &   ! ND field
                 nids, nide, 1, config_flags%maxpatch, njds, njde,   &         ! ND dims
                 nims, nime, 1, config_flags%maxpatch, njms, njme,   &         ! ND dims
                 nips, nipe, 1, config_flags%maxpatch, njps, njpe,   &         ! ND dims
                  config_flags%shw, ngrid%imask_nostag,         &         ! stencil half width
                  .FALSE., .FALSE.,                                                &         ! xstag, ystag
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%h2osoi_liq4, 1 ) * SIZE( grid%h2osoi_liq4, 3 ) .GT. 1 ) THEN 
CALL copy_fcnm (  &         
                  grid%h2osoi_liq4,   &       ! CD field
                 cids, cide, 1, config_flags%maxpatch, cjds, cjde,   &         ! CD dims
                 cims, cime, 1, config_flags%maxpatch, cjms, cjme,   &         ! CD dims
                 cips, cipe, 1, config_flags%maxpatch, cjps, cjpe,   &         ! CD dims
                  ngrid%h2osoi_liq4,  &   ! ND field
                 nids, nide, 1, config_flags%maxpatch, njds, njde,   &         ! ND dims
                 nims, nime, 1, config_flags%maxpatch, njms, njme,   &         ! ND dims
                 nips, nipe, 1, config_flags%maxpatch, njps, njpe,   &         ! ND dims
                  config_flags%shw, ngrid%imask_nostag,         &         ! stencil half width
                  .FALSE., .FALSE.,                                                &         ! xstag, ystag
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%h2osoi_liq5, 1 ) * SIZE( grid%h2osoi_liq5, 3 ) .GT. 1 ) THEN 
CALL copy_fcnm (  &         
                  grid%h2osoi_liq5,   &       ! CD field
                 cids, cide, 1, config_flags%maxpatch, cjds, cjde,   &         ! CD dims
                 cims, cime, 1, config_flags%maxpatch, cjms, cjme,   &         ! CD dims
                 cips, cipe, 1, config_flags%maxpatch, cjps, cjpe,   &         ! CD dims
                  ngrid%h2osoi_liq5,  &   ! ND field
                 nids, nide, 1, config_flags%maxpatch, njds, njde,   &         ! ND dims
                 nims, nime, 1, config_flags%maxpatch, njms, njme,   &         ! ND dims
                 nips, nipe, 1, config_flags%maxpatch, njps, njpe,   &         ! ND dims
                  config_flags%shw, ngrid%imask_nostag,         &         ! stencil half width
                  .FALSE., .FALSE.,                                                &         ! xstag, ystag
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%h2osoi_liq6, 1 ) * SIZE( grid%h2osoi_liq6, 3 ) .GT. 1 ) THEN 
CALL copy_fcnm (  &         
                  grid%h2osoi_liq6,   &       ! CD field
                 cids, cide, 1, config_flags%maxpatch, cjds, cjde,   &         ! CD dims
                 cims, cime, 1, config_flags%maxpatch, cjms, cjme,   &         ! CD dims
                 cips, cipe, 1, config_flags%maxpatch, cjps, cjpe,   &         ! CD dims
                  ngrid%h2osoi_liq6,  &   ! ND field
                 nids, nide, 1, config_flags%maxpatch, njds, njde,   &         ! ND dims
                 nims, nime, 1, config_flags%maxpatch, njms, njme,   &         ! ND dims
                 nips, nipe, 1, config_flags%maxpatch, njps, njpe,   &         ! ND dims
                  config_flags%shw, ngrid%imask_nostag,         &         ! stencil half width
                  .FALSE., .FALSE.,                                                &         ! xstag, ystag
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%h2osoi_liq7, 1 ) * SIZE( grid%h2osoi_liq7, 3 ) .GT. 1 ) THEN 
CALL copy_fcnm (  &         
                  grid%h2osoi_liq7,   &       ! CD field
                 cids, cide, 1, config_flags%maxpatch, cjds, cjde,   &         ! CD dims
                 cims, cime, 1, config_flags%maxpatch, cjms, cjme,   &         ! CD dims
                 cips, cipe, 1, config_flags%maxpatch, cjps, cjpe,   &         ! CD dims
                  ngrid%h2osoi_liq7,  &   ! ND field
                 nids, nide, 1, config_flags%maxpatch, njds, njde,   &         ! ND dims
                 nims, nime, 1, config_flags%maxpatch, njms, njme,   &         ! ND dims
                 nips, nipe, 1, config_flags%maxpatch, njps, njpe,   &         ! ND dims
                  config_flags%shw, ngrid%imask_nostag,         &         ! stencil half width
                  .FALSE., .FALSE.,                                                &         ! xstag, ystag
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%h2osoi_liq8, 1 ) * SIZE( grid%h2osoi_liq8, 3 ) .GT. 1 ) THEN 
CALL copy_fcnm (  &         
                  grid%h2osoi_liq8,   &       ! CD field
                 cids, cide, 1, config_flags%maxpatch, cjds, cjde,   &         ! CD dims
                 cims, cime, 1, config_flags%maxpatch, cjms, cjme,   &         ! CD dims
                 cips, cipe, 1, config_flags%maxpatch, cjps, cjpe,   &         ! CD dims
                  ngrid%h2osoi_liq8,  &   ! ND field
                 nids, nide, 1, config_flags%maxpatch, njds, njde,   &         ! ND dims
                 nims, nime, 1, config_flags%maxpatch, njms, njme,   &         ! ND dims
                 nips, nipe, 1, config_flags%maxpatch, njps, njpe,   &         ! ND dims
                  config_flags%shw, ngrid%imask_nostag,         &         ! stencil half width
                  .FALSE., .FALSE.,                                                &         ! xstag, ystag
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%h2osoi_liq9, 1 ) * SIZE( grid%h2osoi_liq9, 3 ) .GT. 1 ) THEN 
CALL copy_fcnm (  &         
                  grid%h2osoi_liq9,   &       ! CD field
                 cids, cide, 1, config_flags%maxpatch, cjds, cjde,   &         ! CD dims
                 cims, cime, 1, config_flags%maxpatch, cjms, cjme,   &         ! CD dims
                 cips, cipe, 1, config_flags%maxpatch, cjps, cjpe,   &         ! CD dims
                  ngrid%h2osoi_liq9,  &   ! ND field
                 nids, nide, 1, config_flags%maxpatch, njds, njde,   &         ! ND dims
                 nims, nime, 1, config_flags%maxpatch, njms, njme,   &         ! ND dims
                 nips, nipe, 1, config_flags%maxpatch, njps, njpe,   &         ! ND dims
                  config_flags%shw, ngrid%imask_nostag,         &         ! stencil half width
                  .FALSE., .FALSE.,                                                &         ! xstag, ystag
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%h2osoi_liq10, 1 ) * SIZE( grid%h2osoi_liq10, 3 ) .GT. 1 ) THEN 
CALL copy_fcnm (  &         
                  grid%h2osoi_liq10,   &       ! CD field
                 cids, cide, 1, config_flags%maxpatch, cjds, cjde,   &         ! CD dims
                 cims, cime, 1, config_flags%maxpatch, cjms, cjme,   &         ! CD dims
                 cips, cipe, 1, config_flags%maxpatch, cjps, cjpe,   &         ! CD dims
                  ngrid%h2osoi_liq10,  &   ! ND field
                 nids, nide, 1, config_flags%maxpatch, njds, njde,   &         ! ND dims
                 nims, nime, 1, config_flags%maxpatch, njms, njme,   &         ! ND dims
                 nips, nipe, 1, config_flags%maxpatch, njps, njpe,   &         ! ND dims
                  config_flags%shw, ngrid%imask_nostag,         &         ! stencil half width
                  .FALSE., .FALSE.,                                                &         ! xstag, ystag
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%h2osoi_ice_s1, 1 ) * SIZE( grid%h2osoi_ice_s1, 3 ) .GT. 1 ) THEN 
CALL copy_fcnm (  &         
                  grid%h2osoi_ice_s1,   &       ! CD field
                 cids, cide, 1, config_flags%maxpatch, cjds, cjde,   &         ! CD dims
                 cims, cime, 1, config_flags%maxpatch, cjms, cjme,   &         ! CD dims
                 cips, cipe, 1, config_flags%maxpatch, cjps, cjpe,   &         ! CD dims
                  ngrid%h2osoi_ice_s1,  &   ! ND field
                 nids, nide, 1, config_flags%maxpatch, njds, njde,   &         ! ND dims
                 nims, nime, 1, config_flags%maxpatch, njms, njme,   &         ! ND dims
                 nips, nipe, 1, config_flags%maxpatch, njps, njpe,   &         ! ND dims
                  config_flags%shw, ngrid%imask_nostag,         &         ! stencil half width
                  .FALSE., .FALSE.,                                                &         ! xstag, ystag
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%h2osoi_ice_s2, 1 ) * SIZE( grid%h2osoi_ice_s2, 3 ) .GT. 1 ) THEN 
CALL copy_fcnm (  &         
                  grid%h2osoi_ice_s2,   &       ! CD field
                 cids, cide, 1, config_flags%maxpatch, cjds, cjde,   &         ! CD dims
                 cims, cime, 1, config_flags%maxpatch, cjms, cjme,   &         ! CD dims
                 cips, cipe, 1, config_flags%maxpatch, cjps, cjpe,   &         ! CD dims
                  ngrid%h2osoi_ice_s2,  &   ! ND field
                 nids, nide, 1, config_flags%maxpatch, njds, njde,   &         ! ND dims
                 nims, nime, 1, config_flags%maxpatch, njms, njme,   &         ! ND dims
                 nips, nipe, 1, config_flags%maxpatch, njps, njpe,   &         ! ND dims
                  config_flags%shw, ngrid%imask_nostag,         &         ! stencil half width
                  .FALSE., .FALSE.,                                                &         ! xstag, ystag
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%h2osoi_ice_s3, 1 ) * SIZE( grid%h2osoi_ice_s3, 3 ) .GT. 1 ) THEN 
CALL copy_fcnm (  &         
                  grid%h2osoi_ice_s3,   &       ! CD field
                 cids, cide, 1, config_flags%maxpatch, cjds, cjde,   &         ! CD dims
                 cims, cime, 1, config_flags%maxpatch, cjms, cjme,   &         ! CD dims
                 cips, cipe, 1, config_flags%maxpatch, cjps, cjpe,   &         ! CD dims
                  ngrid%h2osoi_ice_s3,  &   ! ND field
                 nids, nide, 1, config_flags%maxpatch, njds, njde,   &         ! ND dims
                 nims, nime, 1, config_flags%maxpatch, njms, njme,   &         ! ND dims
                 nips, nipe, 1, config_flags%maxpatch, njps, njpe,   &         ! ND dims
                  config_flags%shw, ngrid%imask_nostag,         &         ! stencil half width
                  .FALSE., .FALSE.,                                                &         ! xstag, ystag
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%h2osoi_ice_s4, 1 ) * SIZE( grid%h2osoi_ice_s4, 3 ) .GT. 1 ) THEN 
CALL copy_fcnm (  &         
                  grid%h2osoi_ice_s4,   &       ! CD field
                 cids, cide, 1, config_flags%maxpatch, cjds, cjde,   &         ! CD dims
                 cims, cime, 1, config_flags%maxpatch, cjms, cjme,   &         ! CD dims
                 cips, cipe, 1, config_flags%maxpatch, cjps, cjpe,   &         ! CD dims
                  ngrid%h2osoi_ice_s4,  &   ! ND field
                 nids, nide, 1, config_flags%maxpatch, njds, njde,   &         ! ND dims
                 nims, nime, 1, config_flags%maxpatch, njms, njme,   &         ! ND dims
                 nips, nipe, 1, config_flags%maxpatch, njps, njpe,   &         ! ND dims
                  config_flags%shw, ngrid%imask_nostag,         &         ! stencil half width
                  .FALSE., .FALSE.,                                                &         ! xstag, ystag
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%h2osoi_ice_s5, 1 ) * SIZE( grid%h2osoi_ice_s5, 3 ) .GT. 1 ) THEN 
CALL copy_fcnm (  &         
                  grid%h2osoi_ice_s5,   &       ! CD field
                 cids, cide, 1, config_flags%maxpatch, cjds, cjde,   &         ! CD dims
                 cims, cime, 1, config_flags%maxpatch, cjms, cjme,   &         ! CD dims
                 cips, cipe, 1, config_flags%maxpatch, cjps, cjpe,   &         ! CD dims
                  ngrid%h2osoi_ice_s5,  &   ! ND field
                 nids, nide, 1, config_flags%maxpatch, njds, njde,   &         ! ND dims
                 nims, nime, 1, config_flags%maxpatch, njms, njme,   &         ! ND dims
                 nips, nipe, 1, config_flags%maxpatch, njps, njpe,   &         ! ND dims
                  config_flags%shw, ngrid%imask_nostag,         &         ! stencil half width
                  .FALSE., .FALSE.,                                                &         ! xstag, ystag
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%h2osoi_ice1, 1 ) * SIZE( grid%h2osoi_ice1, 3 ) .GT. 1 ) THEN 
CALL copy_fcnm (  &         
                  grid%h2osoi_ice1,   &       ! CD field
                 cids, cide, 1, config_flags%maxpatch, cjds, cjde,   &         ! CD dims
                 cims, cime, 1, config_flags%maxpatch, cjms, cjme,   &         ! CD dims
                 cips, cipe, 1, config_flags%maxpatch, cjps, cjpe,   &         ! CD dims
                  ngrid%h2osoi_ice1,  &   ! ND field
                 nids, nide, 1, config_flags%maxpatch, njds, njde,   &         ! ND dims
                 nims, nime, 1, config_flags%maxpatch, njms, njme,   &         ! ND dims
                 nips, nipe, 1, config_flags%maxpatch, njps, njpe,   &         ! ND dims
                  config_flags%shw, ngrid%imask_nostag,         &         ! stencil half width
                  .FALSE., .FALSE.,                                                &         ! xstag, ystag
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%h2osoi_ice2, 1 ) * SIZE( grid%h2osoi_ice2, 3 ) .GT. 1 ) THEN 
CALL copy_fcnm (  &         
                  grid%h2osoi_ice2,   &       ! CD field
                 cids, cide, 1, config_flags%maxpatch, cjds, cjde,   &         ! CD dims
                 cims, cime, 1, config_flags%maxpatch, cjms, cjme,   &         ! CD dims
                 cips, cipe, 1, config_flags%maxpatch, cjps, cjpe,   &         ! CD dims
                  ngrid%h2osoi_ice2,  &   ! ND field
                 nids, nide, 1, config_flags%maxpatch, njds, njde,   &         ! ND dims
                 nims, nime, 1, config_flags%maxpatch, njms, njme,   &         ! ND dims
                 nips, nipe, 1, config_flags%maxpatch, njps, njpe,   &         ! ND dims
                  config_flags%shw, ngrid%imask_nostag,         &         ! stencil half width
                  .FALSE., .FALSE.,                                                &         ! xstag, ystag
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%h2osoi_ice3, 1 ) * SIZE( grid%h2osoi_ice3, 3 ) .GT. 1 ) THEN 
CALL copy_fcnm (  &         
                  grid%h2osoi_ice3,   &       ! CD field
                 cids, cide, 1, config_flags%maxpatch, cjds, cjde,   &         ! CD dims
                 cims, cime, 1, config_flags%maxpatch, cjms, cjme,   &         ! CD dims
                 cips, cipe, 1, config_flags%maxpatch, cjps, cjpe,   &         ! CD dims
                  ngrid%h2osoi_ice3,  &   ! ND field
                 nids, nide, 1, config_flags%maxpatch, njds, njde,   &         ! ND dims
                 nims, nime, 1, config_flags%maxpatch, njms, njme,   &         ! ND dims
                 nips, nipe, 1, config_flags%maxpatch, njps, njpe,   &         ! ND dims
                  config_flags%shw, ngrid%imask_nostag,         &         ! stencil half width
                  .FALSE., .FALSE.,                                                &         ! xstag, ystag
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%h2osoi_ice4, 1 ) * SIZE( grid%h2osoi_ice4, 3 ) .GT. 1 ) THEN 
CALL copy_fcnm (  &         
                  grid%h2osoi_ice4,   &       ! CD field
                 cids, cide, 1, config_flags%maxpatch, cjds, cjde,   &         ! CD dims
                 cims, cime, 1, config_flags%maxpatch, cjms, cjme,   &         ! CD dims
                 cips, cipe, 1, config_flags%maxpatch, cjps, cjpe,   &         ! CD dims
                  ngrid%h2osoi_ice4,  &   ! ND field
                 nids, nide, 1, config_flags%maxpatch, njds, njde,   &         ! ND dims
                 nims, nime, 1, config_flags%maxpatch, njms, njme,   &         ! ND dims
                 nips, nipe, 1, config_flags%maxpatch, njps, njpe,   &         ! ND dims
                  config_flags%shw, ngrid%imask_nostag,         &         ! stencil half width
                  .FALSE., .FALSE.,                                                &         ! xstag, ystag
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%h2osoi_ice5, 1 ) * SIZE( grid%h2osoi_ice5, 3 ) .GT. 1 ) THEN 
CALL copy_fcnm (  &         
                  grid%h2osoi_ice5,   &       ! CD field
                 cids, cide, 1, config_flags%maxpatch, cjds, cjde,   &         ! CD dims
                 cims, cime, 1, config_flags%maxpatch, cjms, cjme,   &         ! CD dims
                 cips, cipe, 1, config_flags%maxpatch, cjps, cjpe,   &         ! CD dims
                  ngrid%h2osoi_ice5,  &   ! ND field
                 nids, nide, 1, config_flags%maxpatch, njds, njde,   &         ! ND dims
                 nims, nime, 1, config_flags%maxpatch, njms, njme,   &         ! ND dims
                 nips, nipe, 1, config_flags%maxpatch, njps, njpe,   &         ! ND dims
                  config_flags%shw, ngrid%imask_nostag,         &         ! stencil half width
                  .FALSE., .FALSE.,                                                &         ! xstag, ystag
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%h2osoi_ice6, 1 ) * SIZE( grid%h2osoi_ice6, 3 ) .GT. 1 ) THEN 
CALL copy_fcnm (  &         
                  grid%h2osoi_ice6,   &       ! CD field
                 cids, cide, 1, config_flags%maxpatch, cjds, cjde,   &         ! CD dims
                 cims, cime, 1, config_flags%maxpatch, cjms, cjme,   &         ! CD dims
                 cips, cipe, 1, config_flags%maxpatch, cjps, cjpe,   &         ! CD dims
                  ngrid%h2osoi_ice6,  &   ! ND field
                 nids, nide, 1, config_flags%maxpatch, njds, njde,   &         ! ND dims
                 nims, nime, 1, config_flags%maxpatch, njms, njme,   &         ! ND dims
                 nips, nipe, 1, config_flags%maxpatch, njps, njpe,   &         ! ND dims
                  config_flags%shw, ngrid%imask_nostag,         &         ! stencil half width
                  .FALSE., .FALSE.,                                                &         ! xstag, ystag
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%h2osoi_ice7, 1 ) * SIZE( grid%h2osoi_ice7, 3 ) .GT. 1 ) THEN 
CALL copy_fcnm (  &         
                  grid%h2osoi_ice7,   &       ! CD field
                 cids, cide, 1, config_flags%maxpatch, cjds, cjde,   &         ! CD dims
                 cims, cime, 1, config_flags%maxpatch, cjms, cjme,   &         ! CD dims
                 cips, cipe, 1, config_flags%maxpatch, cjps, cjpe,   &         ! CD dims
                  ngrid%h2osoi_ice7,  &   ! ND field
                 nids, nide, 1, config_flags%maxpatch, njds, njde,   &         ! ND dims
                 nims, nime, 1, config_flags%maxpatch, njms, njme,   &         ! ND dims
                 nips, nipe, 1, config_flags%maxpatch, njps, njpe,   &         ! ND dims
                  config_flags%shw, ngrid%imask_nostag,         &         ! stencil half width
                  .FALSE., .FALSE.,                                                &         ! xstag, ystag
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%h2osoi_ice8, 1 ) * SIZE( grid%h2osoi_ice8, 3 ) .GT. 1 ) THEN 
CALL copy_fcnm (  &         
                  grid%h2osoi_ice8,   &       ! CD field
                 cids, cide, 1, config_flags%maxpatch, cjds, cjde,   &         ! CD dims
                 cims, cime, 1, config_flags%maxpatch, cjms, cjme,   &         ! CD dims
                 cips, cipe, 1, config_flags%maxpatch, cjps, cjpe,   &         ! CD dims
                  ngrid%h2osoi_ice8,  &   ! ND field
                 nids, nide, 1, config_flags%maxpatch, njds, njde,   &         ! ND dims
                 nims, nime, 1, config_flags%maxpatch, njms, njme,   &         ! ND dims
                 nips, nipe, 1, config_flags%maxpatch, njps, njpe,   &         ! ND dims
                  config_flags%shw, ngrid%imask_nostag,         &         ! stencil half width
                  .FALSE., .FALSE.,                                                &         ! xstag, ystag
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%h2osoi_ice9, 1 ) * SIZE( grid%h2osoi_ice9, 3 ) .GT. 1 ) THEN 
CALL copy_fcnm (  &         
                  grid%h2osoi_ice9,   &       ! CD field
                 cids, cide, 1, config_flags%maxpatch, cjds, cjde,   &         ! CD dims
                 cims, cime, 1, config_flags%maxpatch, cjms, cjme,   &         ! CD dims
                 cips, cipe, 1, config_flags%maxpatch, cjps, cjpe,   &         ! CD dims
                  ngrid%h2osoi_ice9,  &   ! ND field
                 nids, nide, 1, config_flags%maxpatch, njds, njde,   &         ! ND dims
                 nims, nime, 1, config_flags%maxpatch, njms, njme,   &         ! ND dims
                 nips, nipe, 1, config_flags%maxpatch, njps, njpe,   &         ! ND dims
                  config_flags%shw, ngrid%imask_nostag,         &         ! stencil half width
                  .FALSE., .FALSE.,                                                &         ! xstag, ystag
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%h2osoi_ice10, 1 ) * SIZE( grid%h2osoi_ice10, 3 ) .GT. 1 ) THEN 
CALL copy_fcnm (  &         
                  grid%h2osoi_ice10,   &       ! CD field
                 cids, cide, 1, config_flags%maxpatch, cjds, cjde,   &         ! CD dims
                 cims, cime, 1, config_flags%maxpatch, cjms, cjme,   &         ! CD dims
                 cips, cipe, 1, config_flags%maxpatch, cjps, cjpe,   &         ! CD dims
                  ngrid%h2osoi_ice10,  &   ! ND field
                 nids, nide, 1, config_flags%maxpatch, njds, njde,   &         ! ND dims
                 nims, nime, 1, config_flags%maxpatch, njms, njme,   &         ! ND dims
                 nips, nipe, 1, config_flags%maxpatch, njps, njpe,   &         ! ND dims
                  config_flags%shw, ngrid%imask_nostag,         &         ! stencil half width
                  .FALSE., .FALSE.,                                                &         ! xstag, ystag
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%t_soisno_s1, 1 ) * SIZE( grid%t_soisno_s1, 3 ) .GT. 1 ) THEN 
CALL copy_fcnm (  &         
                  grid%t_soisno_s1,   &       ! CD field
                 cids, cide, 1, config_flags%maxpatch, cjds, cjde,   &         ! CD dims
                 cims, cime, 1, config_flags%maxpatch, cjms, cjme,   &         ! CD dims
                 cips, cipe, 1, config_flags%maxpatch, cjps, cjpe,   &         ! CD dims
                  ngrid%t_soisno_s1,  &   ! ND field
                 nids, nide, 1, config_flags%maxpatch, njds, njde,   &         ! ND dims
                 nims, nime, 1, config_flags%maxpatch, njms, njme,   &         ! ND dims
                 nips, nipe, 1, config_flags%maxpatch, njps, njpe,   &         ! ND dims
                  config_flags%shw, ngrid%imask_nostag,         &         ! stencil half width
                  .FALSE., .FALSE.,                                                &         ! xstag, ystag
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%t_soisno_s2, 1 ) * SIZE( grid%t_soisno_s2, 3 ) .GT. 1 ) THEN 
CALL copy_fcnm (  &         
                  grid%t_soisno_s2,   &       ! CD field
                 cids, cide, 1, config_flags%maxpatch, cjds, cjde,   &         ! CD dims
                 cims, cime, 1, config_flags%maxpatch, cjms, cjme,   &         ! CD dims
                 cips, cipe, 1, config_flags%maxpatch, cjps, cjpe,   &         ! CD dims
                  ngrid%t_soisno_s2,  &   ! ND field
                 nids, nide, 1, config_flags%maxpatch, njds, njde,   &         ! ND dims
                 nims, nime, 1, config_flags%maxpatch, njms, njme,   &         ! ND dims
                 nips, nipe, 1, config_flags%maxpatch, njps, njpe,   &         ! ND dims
                  config_flags%shw, ngrid%imask_nostag,         &         ! stencil half width
                  .FALSE., .FALSE.,                                                &         ! xstag, ystag
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%t_soisno_s3, 1 ) * SIZE( grid%t_soisno_s3, 3 ) .GT. 1 ) THEN 
CALL copy_fcnm (  &         
                  grid%t_soisno_s3,   &       ! CD field
                 cids, cide, 1, config_flags%maxpatch, cjds, cjde,   &         ! CD dims
                 cims, cime, 1, config_flags%maxpatch, cjms, cjme,   &         ! CD dims
                 cips, cipe, 1, config_flags%maxpatch, cjps, cjpe,   &         ! CD dims
                  ngrid%t_soisno_s3,  &   ! ND field
                 nids, nide, 1, config_flags%maxpatch, njds, njde,   &         ! ND dims
                 nims, nime, 1, config_flags%maxpatch, njms, njme,   &         ! ND dims
                 nips, nipe, 1, config_flags%maxpatch, njps, njpe,   &         ! ND dims
                  config_flags%shw, ngrid%imask_nostag,         &         ! stencil half width
                  .FALSE., .FALSE.,                                                &         ! xstag, ystag
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%t_soisno_s4, 1 ) * SIZE( grid%t_soisno_s4, 3 ) .GT. 1 ) THEN 
CALL copy_fcnm (  &         
                  grid%t_soisno_s4,   &       ! CD field
                 cids, cide, 1, config_flags%maxpatch, cjds, cjde,   &         ! CD dims
                 cims, cime, 1, config_flags%maxpatch, cjms, cjme,   &         ! CD dims
                 cips, cipe, 1, config_flags%maxpatch, cjps, cjpe,   &         ! CD dims
                  ngrid%t_soisno_s4,  &   ! ND field
                 nids, nide, 1, config_flags%maxpatch, njds, njde,   &         ! ND dims
                 nims, nime, 1, config_flags%maxpatch, njms, njme,   &         ! ND dims
                 nips, nipe, 1, config_flags%maxpatch, njps, njpe,   &         ! ND dims
                  config_flags%shw, ngrid%imask_nostag,         &         ! stencil half width
                  .FALSE., .FALSE.,                                                &         ! xstag, ystag
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%t_soisno_s5, 1 ) * SIZE( grid%t_soisno_s5, 3 ) .GT. 1 ) THEN 
CALL copy_fcnm (  &         
                  grid%t_soisno_s5,   &       ! CD field
                 cids, cide, 1, config_flags%maxpatch, cjds, cjde,   &         ! CD dims
                 cims, cime, 1, config_flags%maxpatch, cjms, cjme,   &         ! CD dims
                 cips, cipe, 1, config_flags%maxpatch, cjps, cjpe,   &         ! CD dims
                  ngrid%t_soisno_s5,  &   ! ND field
                 nids, nide, 1, config_flags%maxpatch, njds, njde,   &         ! ND dims
                 nims, nime, 1, config_flags%maxpatch, njms, njme,   &         ! ND dims
                 nips, nipe, 1, config_flags%maxpatch, njps, njpe,   &         ! ND dims
                  config_flags%shw, ngrid%imask_nostag,         &         ! stencil half width
                  .FALSE., .FALSE.,                                                &         ! xstag, ystag
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%t_soisno1, 1 ) * SIZE( grid%t_soisno1, 3 ) .GT. 1 ) THEN 
CALL copy_fcnm (  &         
                  grid%t_soisno1,   &       ! CD field
                 cids, cide, 1, config_flags%maxpatch, cjds, cjde,   &         ! CD dims
                 cims, cime, 1, config_flags%maxpatch, cjms, cjme,   &         ! CD dims
                 cips, cipe, 1, config_flags%maxpatch, cjps, cjpe,   &         ! CD dims
                  ngrid%t_soisno1,  &   ! ND field
                 nids, nide, 1, config_flags%maxpatch, njds, njde,   &         ! ND dims
                 nims, nime, 1, config_flags%maxpatch, njms, njme,   &         ! ND dims
                 nips, nipe, 1, config_flags%maxpatch, njps, njpe,   &         ! ND dims
                  config_flags%shw, ngrid%imask_nostag,         &         ! stencil half width
                  .FALSE., .FALSE.,                                                &         ! xstag, ystag
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%t_soisno2, 1 ) * SIZE( grid%t_soisno2, 3 ) .GT. 1 ) THEN 
CALL copy_fcnm (  &         
                  grid%t_soisno2,   &       ! CD field
                 cids, cide, 1, config_flags%maxpatch, cjds, cjde,   &         ! CD dims
                 cims, cime, 1, config_flags%maxpatch, cjms, cjme,   &         ! CD dims
                 cips, cipe, 1, config_flags%maxpatch, cjps, cjpe,   &         ! CD dims
                  ngrid%t_soisno2,  &   ! ND field
                 nids, nide, 1, config_flags%maxpatch, njds, njde,   &         ! ND dims
                 nims, nime, 1, config_flags%maxpatch, njms, njme,   &         ! ND dims
                 nips, nipe, 1, config_flags%maxpatch, njps, njpe,   &         ! ND dims
                  config_flags%shw, ngrid%imask_nostag,         &         ! stencil half width
                  .FALSE., .FALSE.,                                                &         ! xstag, ystag
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%t_soisno3, 1 ) * SIZE( grid%t_soisno3, 3 ) .GT. 1 ) THEN 
CALL copy_fcnm (  &         
                  grid%t_soisno3,   &       ! CD field
                 cids, cide, 1, config_flags%maxpatch, cjds, cjde,   &         ! CD dims
                 cims, cime, 1, config_flags%maxpatch, cjms, cjme,   &         ! CD dims
                 cips, cipe, 1, config_flags%maxpatch, cjps, cjpe,   &         ! CD dims
                  ngrid%t_soisno3,  &   ! ND field
                 nids, nide, 1, config_flags%maxpatch, njds, njde,   &         ! ND dims
                 nims, nime, 1, config_flags%maxpatch, njms, njme,   &         ! ND dims
                 nips, nipe, 1, config_flags%maxpatch, njps, njpe,   &         ! ND dims
                  config_flags%shw, ngrid%imask_nostag,         &         ! stencil half width
                  .FALSE., .FALSE.,                                                &         ! xstag, ystag
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%t_soisno4, 1 ) * SIZE( grid%t_soisno4, 3 ) .GT. 1 ) THEN 
CALL copy_fcnm (  &         
                  grid%t_soisno4,   &       ! CD field
                 cids, cide, 1, config_flags%maxpatch, cjds, cjde,   &         ! CD dims
                 cims, cime, 1, config_flags%maxpatch, cjms, cjme,   &         ! CD dims
                 cips, cipe, 1, config_flags%maxpatch, cjps, cjpe,   &         ! CD dims
                  ngrid%t_soisno4,  &   ! ND field
                 nids, nide, 1, config_flags%maxpatch, njds, njde,   &         ! ND dims
                 nims, nime, 1, config_flags%maxpatch, njms, njme,   &         ! ND dims
                 nips, nipe, 1, config_flags%maxpatch, njps, njpe,   &         ! ND dims
                  config_flags%shw, ngrid%imask_nostag,         &         ! stencil half width
                  .FALSE., .FALSE.,                                                &         ! xstag, ystag
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%t_soisno5, 1 ) * SIZE( grid%t_soisno5, 3 ) .GT. 1 ) THEN 
CALL copy_fcnm (  &         
                  grid%t_soisno5,   &       ! CD field
                 cids, cide, 1, config_flags%maxpatch, cjds, cjde,   &         ! CD dims
                 cims, cime, 1, config_flags%maxpatch, cjms, cjme,   &         ! CD dims
                 cips, cipe, 1, config_flags%maxpatch, cjps, cjpe,   &         ! CD dims
                  ngrid%t_soisno5,  &   ! ND field
                 nids, nide, 1, config_flags%maxpatch, njds, njde,   &         ! ND dims
                 nims, nime, 1, config_flags%maxpatch, njms, njme,   &         ! ND dims
                 nips, nipe, 1, config_flags%maxpatch, njps, njpe,   &         ! ND dims
                  config_flags%shw, ngrid%imask_nostag,         &         ! stencil half width
                  .FALSE., .FALSE.,                                                &         ! xstag, ystag
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%t_soisno6, 1 ) * SIZE( grid%t_soisno6, 3 ) .GT. 1 ) THEN 
CALL copy_fcnm (  &         
                  grid%t_soisno6,   &       ! CD field
                 cids, cide, 1, config_flags%maxpatch, cjds, cjde,   &         ! CD dims
                 cims, cime, 1, config_flags%maxpatch, cjms, cjme,   &         ! CD dims
                 cips, cipe, 1, config_flags%maxpatch, cjps, cjpe,   &         ! CD dims
                  ngrid%t_soisno6,  &   ! ND field
                 nids, nide, 1, config_flags%maxpatch, njds, njde,   &         ! ND dims
                 nims, nime, 1, config_flags%maxpatch, njms, njme,   &         ! ND dims
                 nips, nipe, 1, config_flags%maxpatch, njps, njpe,   &         ! ND dims
                  config_flags%shw, ngrid%imask_nostag,         &         ! stencil half width
                  .FALSE., .FALSE.,                                                &         ! xstag, ystag
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%t_soisno7, 1 ) * SIZE( grid%t_soisno7, 3 ) .GT. 1 ) THEN 
CALL copy_fcnm (  &         
                  grid%t_soisno7,   &       ! CD field
                 cids, cide, 1, config_flags%maxpatch, cjds, cjde,   &         ! CD dims
                 cims, cime, 1, config_flags%maxpatch, cjms, cjme,   &         ! CD dims
                 cips, cipe, 1, config_flags%maxpatch, cjps, cjpe,   &         ! CD dims
                  ngrid%t_soisno7,  &   ! ND field
                 nids, nide, 1, config_flags%maxpatch, njds, njde,   &         ! ND dims
                 nims, nime, 1, config_flags%maxpatch, njms, njme,   &         ! ND dims
                 nips, nipe, 1, config_flags%maxpatch, njps, njpe,   &         ! ND dims
                  config_flags%shw, ngrid%imask_nostag,         &         ! stencil half width
                  .FALSE., .FALSE.,                                                &         ! xstag, ystag
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%t_soisno8, 1 ) * SIZE( grid%t_soisno8, 3 ) .GT. 1 ) THEN 
CALL copy_fcnm (  &         
                  grid%t_soisno8,   &       ! CD field
                 cids, cide, 1, config_flags%maxpatch, cjds, cjde,   &         ! CD dims
                 cims, cime, 1, config_flags%maxpatch, cjms, cjme,   &         ! CD dims
                 cips, cipe, 1, config_flags%maxpatch, cjps, cjpe,   &         ! CD dims
                  ngrid%t_soisno8,  &   ! ND field
                 nids, nide, 1, config_flags%maxpatch, njds, njde,   &         ! ND dims
                 nims, nime, 1, config_flags%maxpatch, njms, njme,   &         ! ND dims
                 nips, nipe, 1, config_flags%maxpatch, njps, njpe,   &         ! ND dims
                  config_flags%shw, ngrid%imask_nostag,         &         ! stencil half width
                  .FALSE., .FALSE.,                                                &         ! xstag, ystag
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%t_soisno9, 1 ) * SIZE( grid%t_soisno9, 3 ) .GT. 1 ) THEN 
CALL copy_fcnm (  &         
                  grid%t_soisno9,   &       ! CD field
                 cids, cide, 1, config_flags%maxpatch, cjds, cjde,   &         ! CD dims
                 cims, cime, 1, config_flags%maxpatch, cjms, cjme,   &         ! CD dims
                 cips, cipe, 1, config_flags%maxpatch, cjps, cjpe,   &         ! CD dims
                  ngrid%t_soisno9,  &   ! ND field
                 nids, nide, 1, config_flags%maxpatch, njds, njde,   &         ! ND dims
                 nims, nime, 1, config_flags%maxpatch, njms, njme,   &         ! ND dims
                 nips, nipe, 1, config_flags%maxpatch, njps, njpe,   &         ! ND dims
                  config_flags%shw, ngrid%imask_nostag,         &         ! stencil half width
                  .FALSE., .FALSE.,                                                &         ! xstag, ystag
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%t_soisno10, 1 ) * SIZE( grid%t_soisno10, 3 ) .GT. 1 ) THEN 
CALL copy_fcnm (  &         
                  grid%t_soisno10,   &       ! CD field
                 cids, cide, 1, config_flags%maxpatch, cjds, cjde,   &         ! CD dims
                 cims, cime, 1, config_flags%maxpatch, cjms, cjme,   &         ! CD dims
                 cips, cipe, 1, config_flags%maxpatch, cjps, cjpe,   &         ! CD dims
                  ngrid%t_soisno10,  &   ! ND field
                 nids, nide, 1, config_flags%maxpatch, njds, njde,   &         ! ND dims
                 nims, nime, 1, config_flags%maxpatch, njms, njme,   &         ! ND dims
                 nips, nipe, 1, config_flags%maxpatch, njps, njpe,   &         ! ND dims
                  config_flags%shw, ngrid%imask_nostag,         &         ! stencil half width
                  .FALSE., .FALSE.,                                                &         ! xstag, ystag
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%dzsnow1, 1 ) * SIZE( grid%dzsnow1, 3 ) .GT. 1 ) THEN 
CALL copy_fcnm (  &         
                  grid%dzsnow1,   &       ! CD field
                 cids, cide, 1, config_flags%maxpatch, cjds, cjde,   &         ! CD dims
                 cims, cime, 1, config_flags%maxpatch, cjms, cjme,   &         ! CD dims
                 cips, cipe, 1, config_flags%maxpatch, cjps, cjpe,   &         ! CD dims
                  ngrid%dzsnow1,  &   ! ND field
                 nids, nide, 1, config_flags%maxpatch, njds, njde,   &         ! ND dims
                 nims, nime, 1, config_flags%maxpatch, njms, njme,   &         ! ND dims
                 nips, nipe, 1, config_flags%maxpatch, njps, njpe,   &         ! ND dims
                  config_flags%shw, ngrid%imask_nostag,         &         ! stencil half width
                  .FALSE., .FALSE.,                                                &         ! xstag, ystag
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%dzsnow2, 1 ) * SIZE( grid%dzsnow2, 3 ) .GT. 1 ) THEN 
CALL copy_fcnm (  &         
                  grid%dzsnow2,   &       ! CD field
                 cids, cide, 1, config_flags%maxpatch, cjds, cjde,   &         ! CD dims
                 cims, cime, 1, config_flags%maxpatch, cjms, cjme,   &         ! CD dims
                 cips, cipe, 1, config_flags%maxpatch, cjps, cjpe,   &         ! CD dims
                  ngrid%dzsnow2,  &   ! ND field
                 nids, nide, 1, config_flags%maxpatch, njds, njde,   &         ! ND dims
                 nims, nime, 1, config_flags%maxpatch, njms, njme,   &         ! ND dims
                 nips, nipe, 1, config_flags%maxpatch, njps, njpe,   &         ! ND dims
                  config_flags%shw, ngrid%imask_nostag,         &         ! stencil half width
                  .FALSE., .FALSE.,                                                &         ! xstag, ystag
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%dzsnow3, 1 ) * SIZE( grid%dzsnow3, 3 ) .GT. 1 ) THEN 
CALL copy_fcnm (  &         
                  grid%dzsnow3,   &       ! CD field
                 cids, cide, 1, config_flags%maxpatch, cjds, cjde,   &         ! CD dims
                 cims, cime, 1, config_flags%maxpatch, cjms, cjme,   &         ! CD dims
                 cips, cipe, 1, config_flags%maxpatch, cjps, cjpe,   &         ! CD dims
                  ngrid%dzsnow3,  &   ! ND field
                 nids, nide, 1, config_flags%maxpatch, njds, njde,   &         ! ND dims
                 nims, nime, 1, config_flags%maxpatch, njms, njme,   &         ! ND dims
                 nips, nipe, 1, config_flags%maxpatch, njps, njpe,   &         ! ND dims
                  config_flags%shw, ngrid%imask_nostag,         &         ! stencil half width
                  .FALSE., .FALSE.,                                                &         ! xstag, ystag
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%dzsnow4, 1 ) * SIZE( grid%dzsnow4, 3 ) .GT. 1 ) THEN 
CALL copy_fcnm (  &         
                  grid%dzsnow4,   &       ! CD field
                 cids, cide, 1, config_flags%maxpatch, cjds, cjde,   &         ! CD dims
                 cims, cime, 1, config_flags%maxpatch, cjms, cjme,   &         ! CD dims
                 cips, cipe, 1, config_flags%maxpatch, cjps, cjpe,   &         ! CD dims
                  ngrid%dzsnow4,  &   ! ND field
                 nids, nide, 1, config_flags%maxpatch, njds, njde,   &         ! ND dims
                 nims, nime, 1, config_flags%maxpatch, njms, njme,   &         ! ND dims
                 nips, nipe, 1, config_flags%maxpatch, njps, njpe,   &         ! ND dims
                  config_flags%shw, ngrid%imask_nostag,         &         ! stencil half width
                  .FALSE., .FALSE.,                                                &         ! xstag, ystag
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%dzsnow5, 1 ) * SIZE( grid%dzsnow5, 3 ) .GT. 1 ) THEN 
CALL copy_fcnm (  &         
                  grid%dzsnow5,   &       ! CD field
                 cids, cide, 1, config_flags%maxpatch, cjds, cjde,   &         ! CD dims
                 cims, cime, 1, config_flags%maxpatch, cjms, cjme,   &         ! CD dims
                 cips, cipe, 1, config_flags%maxpatch, cjps, cjpe,   &         ! CD dims
                  ngrid%dzsnow5,  &   ! ND field
                 nids, nide, 1, config_flags%maxpatch, njds, njde,   &         ! ND dims
                 nims, nime, 1, config_flags%maxpatch, njms, njme,   &         ! ND dims
                 nips, nipe, 1, config_flags%maxpatch, njps, njpe,   &         ! ND dims
                  config_flags%shw, ngrid%imask_nostag,         &         ! stencil half width
                  .FALSE., .FALSE.,                                                &         ! xstag, ystag
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%snowrds1, 1 ) * SIZE( grid%snowrds1, 3 ) .GT. 1 ) THEN 
CALL copy_fcnm (  &         
                  grid%snowrds1,   &       ! CD field
                 cids, cide, 1, config_flags%maxpatch, cjds, cjde,   &         ! CD dims
                 cims, cime, 1, config_flags%maxpatch, cjms, cjme,   &         ! CD dims
                 cips, cipe, 1, config_flags%maxpatch, cjps, cjpe,   &         ! CD dims
                  ngrid%snowrds1,  &   ! ND field
                 nids, nide, 1, config_flags%maxpatch, njds, njde,   &         ! ND dims
                 nims, nime, 1, config_flags%maxpatch, njms, njme,   &         ! ND dims
                 nips, nipe, 1, config_flags%maxpatch, njps, njpe,   &         ! ND dims
                  config_flags%shw, ngrid%imask_nostag,         &         ! stencil half width
                  .FALSE., .FALSE.,                                                &         ! xstag, ystag
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%snowrds2, 1 ) * SIZE( grid%snowrds2, 3 ) .GT. 1 ) THEN 
CALL copy_fcnm (  &         
                  grid%snowrds2,   &       ! CD field
                 cids, cide, 1, config_flags%maxpatch, cjds, cjde,   &         ! CD dims
                 cims, cime, 1, config_flags%maxpatch, cjms, cjme,   &         ! CD dims
                 cips, cipe, 1, config_flags%maxpatch, cjps, cjpe,   &         ! CD dims
                  ngrid%snowrds2,  &   ! ND field
                 nids, nide, 1, config_flags%maxpatch, njds, njde,   &         ! ND dims
                 nims, nime, 1, config_flags%maxpatch, njms, njme,   &         ! ND dims
                 nips, nipe, 1, config_flags%maxpatch, njps, njpe,   &         ! ND dims
                  config_flags%shw, ngrid%imask_nostag,         &         ! stencil half width
                  .FALSE., .FALSE.,                                                &         ! xstag, ystag
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%snowrds3, 1 ) * SIZE( grid%snowrds3, 3 ) .GT. 1 ) THEN 
CALL copy_fcnm (  &         
                  grid%snowrds3,   &       ! CD field
                 cids, cide, 1, config_flags%maxpatch, cjds, cjde,   &         ! CD dims
                 cims, cime, 1, config_flags%maxpatch, cjms, cjme,   &         ! CD dims
                 cips, cipe, 1, config_flags%maxpatch, cjps, cjpe,   &         ! CD dims
                  ngrid%snowrds3,  &   ! ND field
                 nids, nide, 1, config_flags%maxpatch, njds, njde,   &         ! ND dims
                 nims, nime, 1, config_flags%maxpatch, njms, njme,   &         ! ND dims
                 nips, nipe, 1, config_flags%maxpatch, njps, njpe,   &         ! ND dims
                  config_flags%shw, ngrid%imask_nostag,         &         ! stencil half width
                  .FALSE., .FALSE.,                                                &         ! xstag, ystag
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%snowrds4, 1 ) * SIZE( grid%snowrds4, 3 ) .GT. 1 ) THEN 
CALL copy_fcnm (  &         
                  grid%snowrds4,   &       ! CD field
                 cids, cide, 1, config_flags%maxpatch, cjds, cjde,   &         ! CD dims
                 cims, cime, 1, config_flags%maxpatch, cjms, cjme,   &         ! CD dims
                 cips, cipe, 1, config_flags%maxpatch, cjps, cjpe,   &         ! CD dims
                  ngrid%snowrds4,  &   ! ND field
                 nids, nide, 1, config_flags%maxpatch, njds, njde,   &         ! ND dims
                 nims, nime, 1, config_flags%maxpatch, njms, njme,   &         ! ND dims
                 nips, nipe, 1, config_flags%maxpatch, njps, njpe,   &         ! ND dims
                  config_flags%shw, ngrid%imask_nostag,         &         ! stencil half width
                  .FALSE., .FALSE.,                                                &         ! xstag, ystag
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%snowrds5, 1 ) * SIZE( grid%snowrds5, 3 ) .GT. 1 ) THEN 
CALL copy_fcnm (  &         
                  grid%snowrds5,   &       ! CD field
                 cids, cide, 1, config_flags%maxpatch, cjds, cjde,   &         ! CD dims
                 cims, cime, 1, config_flags%maxpatch, cjms, cjme,   &         ! CD dims
                 cips, cipe, 1, config_flags%maxpatch, cjps, cjpe,   &         ! CD dims
                  ngrid%snowrds5,  &   ! ND field
                 nids, nide, 1, config_flags%maxpatch, njds, njde,   &         ! ND dims
                 nims, nime, 1, config_flags%maxpatch, njms, njme,   &         ! ND dims
                 nips, nipe, 1, config_flags%maxpatch, njps, njpe,   &         ! ND dims
                  config_flags%shw, ngrid%imask_nostag,         &         ! stencil half width
                  .FALSE., .FALSE.,                                                &         ! xstag, ystag
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%t_lake1, 1 ) * SIZE( grid%t_lake1, 3 ) .GT. 1 ) THEN 
CALL copy_fcnm (  &         
                  grid%t_lake1,   &       ! CD field
                 cids, cide, 1, config_flags%maxpatch, cjds, cjde,   &         ! CD dims
                 cims, cime, 1, config_flags%maxpatch, cjms, cjme,   &         ! CD dims
                 cips, cipe, 1, config_flags%maxpatch, cjps, cjpe,   &         ! CD dims
                  ngrid%t_lake1,  &   ! ND field
                 nids, nide, 1, config_flags%maxpatch, njds, njde,   &         ! ND dims
                 nims, nime, 1, config_flags%maxpatch, njms, njme,   &         ! ND dims
                 nips, nipe, 1, config_flags%maxpatch, njps, njpe,   &         ! ND dims
                  config_flags%shw, ngrid%imask_nostag,         &         ! stencil half width
                  .FALSE., .FALSE.,                                                &         ! xstag, ystag
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%t_lake2, 1 ) * SIZE( grid%t_lake2, 3 ) .GT. 1 ) THEN 
CALL copy_fcnm (  &         
                  grid%t_lake2,   &       ! CD field
                 cids, cide, 1, config_flags%maxpatch, cjds, cjde,   &         ! CD dims
                 cims, cime, 1, config_flags%maxpatch, cjms, cjme,   &         ! CD dims
                 cips, cipe, 1, config_flags%maxpatch, cjps, cjpe,   &         ! CD dims
                  ngrid%t_lake2,  &   ! ND field
                 nids, nide, 1, config_flags%maxpatch, njds, njde,   &         ! ND dims
                 nims, nime, 1, config_flags%maxpatch, njms, njme,   &         ! ND dims
                 nips, nipe, 1, config_flags%maxpatch, njps, njpe,   &         ! ND dims
                  config_flags%shw, ngrid%imask_nostag,         &         ! stencil half width
                  .FALSE., .FALSE.,                                                &         ! xstag, ystag
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%t_lake3, 1 ) * SIZE( grid%t_lake3, 3 ) .GT. 1 ) THEN 
CALL copy_fcnm (  &         
                  grid%t_lake3,   &       ! CD field
                 cids, cide, 1, config_flags%maxpatch, cjds, cjde,   &         ! CD dims
                 cims, cime, 1, config_flags%maxpatch, cjms, cjme,   &         ! CD dims
                 cips, cipe, 1, config_flags%maxpatch, cjps, cjpe,   &         ! CD dims
                  ngrid%t_lake3,  &   ! ND field
                 nids, nide, 1, config_flags%maxpatch, njds, njde,   &         ! ND dims
                 nims, nime, 1, config_flags%maxpatch, njms, njme,   &         ! ND dims
                 nips, nipe, 1, config_flags%maxpatch, njps, njpe,   &         ! ND dims
                  config_flags%shw, ngrid%imask_nostag,         &         ! stencil half width
                  .FALSE., .FALSE.,                                                &         ! xstag, ystag
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%t_lake4, 1 ) * SIZE( grid%t_lake4, 3 ) .GT. 1 ) THEN 
CALL copy_fcnm (  &         
                  grid%t_lake4,   &       ! CD field
                 cids, cide, 1, config_flags%maxpatch, cjds, cjde,   &         ! CD dims
                 cims, cime, 1, config_flags%maxpatch, cjms, cjme,   &         ! CD dims
                 cips, cipe, 1, config_flags%maxpatch, cjps, cjpe,   &         ! CD dims
                  ngrid%t_lake4,  &   ! ND field
                 nids, nide, 1, config_flags%maxpatch, njds, njde,   &         ! ND dims
                 nims, nime, 1, config_flags%maxpatch, njms, njme,   &         ! ND dims
                 nips, nipe, 1, config_flags%maxpatch, njps, njpe,   &         ! ND dims
                  config_flags%shw, ngrid%imask_nostag,         &         ! stencil half width
                  .FALSE., .FALSE.,                                                &         ! xstag, ystag
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%t_lake5, 1 ) * SIZE( grid%t_lake5, 3 ) .GT. 1 ) THEN 
CALL copy_fcnm (  &         
                  grid%t_lake5,   &       ! CD field
                 cids, cide, 1, config_flags%maxpatch, cjds, cjde,   &         ! CD dims
                 cims, cime, 1, config_flags%maxpatch, cjms, cjme,   &         ! CD dims
                 cips, cipe, 1, config_flags%maxpatch, cjps, cjpe,   &         ! CD dims
                  ngrid%t_lake5,  &   ! ND field
                 nids, nide, 1, config_flags%maxpatch, njds, njde,   &         ! ND dims
                 nims, nime, 1, config_flags%maxpatch, njms, njme,   &         ! ND dims
                 nips, nipe, 1, config_flags%maxpatch, njps, njpe,   &         ! ND dims
                  config_flags%shw, ngrid%imask_nostag,         &         ! stencil half width
                  .FALSE., .FALSE.,                                                &         ! xstag, ystag
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%t_lake6, 1 ) * SIZE( grid%t_lake6, 3 ) .GT. 1 ) THEN 
CALL copy_fcnm (  &         
                  grid%t_lake6,   &       ! CD field
                 cids, cide, 1, config_flags%maxpatch, cjds, cjde,   &         ! CD dims
                 cims, cime, 1, config_flags%maxpatch, cjms, cjme,   &         ! CD dims
                 cips, cipe, 1, config_flags%maxpatch, cjps, cjpe,   &         ! CD dims
                  ngrid%t_lake6,  &   ! ND field
                 nids, nide, 1, config_flags%maxpatch, njds, njde,   &         ! ND dims
                 nims, nime, 1, config_flags%maxpatch, njms, njme,   &         ! ND dims
                 nips, nipe, 1, config_flags%maxpatch, njps, njpe,   &         ! ND dims
                  config_flags%shw, ngrid%imask_nostag,         &         ! stencil half width
                  .FALSE., .FALSE.,                                                &         ! xstag, ystag
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%t_lake7, 1 ) * SIZE( grid%t_lake7, 3 ) .GT. 1 ) THEN 
CALL copy_fcnm (  &         
                  grid%t_lake7,   &       ! CD field
                 cids, cide, 1, config_flags%maxpatch, cjds, cjde,   &         ! CD dims
                 cims, cime, 1, config_flags%maxpatch, cjms, cjme,   &         ! CD dims
                 cips, cipe, 1, config_flags%maxpatch, cjps, cjpe,   &         ! CD dims
                  ngrid%t_lake7,  &   ! ND field
                 nids, nide, 1, config_flags%maxpatch, njds, njde,   &         ! ND dims
                 nims, nime, 1, config_flags%maxpatch, njms, njme,   &         ! ND dims
                 nips, nipe, 1, config_flags%maxpatch, njps, njpe,   &         ! ND dims
                  config_flags%shw, ngrid%imask_nostag,         &         ! stencil half width
                  .FALSE., .FALSE.,                                                &         ! xstag, ystag
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%t_lake8, 1 ) * SIZE( grid%t_lake8, 3 ) .GT. 1 ) THEN 
CALL copy_fcnm (  &         
                  grid%t_lake8,   &       ! CD field
                 cids, cide, 1, config_flags%maxpatch, cjds, cjde,   &         ! CD dims
                 cims, cime, 1, config_flags%maxpatch, cjms, cjme,   &         ! CD dims
                 cips, cipe, 1, config_flags%maxpatch, cjps, cjpe,   &         ! CD dims
                  ngrid%t_lake8,  &   ! ND field
                 nids, nide, 1, config_flags%maxpatch, njds, njde,   &         ! ND dims
                 nims, nime, 1, config_flags%maxpatch, njms, njme,   &         ! ND dims
                 nips, nipe, 1, config_flags%maxpatch, njps, njpe,   &         ! ND dims
                  config_flags%shw, ngrid%imask_nostag,         &         ! stencil half width
                  .FALSE., .FALSE.,                                                &         ! xstag, ystag
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%t_lake9, 1 ) * SIZE( grid%t_lake9, 3 ) .GT. 1 ) THEN 
CALL copy_fcnm (  &         
                  grid%t_lake9,   &       ! CD field
                 cids, cide, 1, config_flags%maxpatch, cjds, cjde,   &         ! CD dims
                 cims, cime, 1, config_flags%maxpatch, cjms, cjme,   &         ! CD dims
                 cips, cipe, 1, config_flags%maxpatch, cjps, cjpe,   &         ! CD dims
                  ngrid%t_lake9,  &   ! ND field
                 nids, nide, 1, config_flags%maxpatch, njds, njde,   &         ! ND dims
                 nims, nime, 1, config_flags%maxpatch, njms, njme,   &         ! ND dims
                 nips, nipe, 1, config_flags%maxpatch, njps, njpe,   &         ! ND dims
                  config_flags%shw, ngrid%imask_nostag,         &         ! stencil half width
                  .FALSE., .FALSE.,                                                &         ! xstag, ystag
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%t_lake10, 1 ) * SIZE( grid%t_lake10, 3 ) .GT. 1 ) THEN 
CALL copy_fcnm (  &         
                  grid%t_lake10,   &       ! CD field
                 cids, cide, 1, config_flags%maxpatch, cjds, cjde,   &         ! CD dims
                 cims, cime, 1, config_flags%maxpatch, cjms, cjme,   &         ! CD dims
                 cips, cipe, 1, config_flags%maxpatch, cjps, cjpe,   &         ! CD dims
                  ngrid%t_lake10,  &   ! ND field
                 nids, nide, 1, config_flags%maxpatch, njds, njde,   &         ! ND dims
                 nims, nime, 1, config_flags%maxpatch, njms, njme,   &         ! ND dims
                 nips, nipe, 1, config_flags%maxpatch, njps, njpe,   &         ! ND dims
                  config_flags%shw, ngrid%imask_nostag,         &         ! stencil half width
                  .FALSE., .FALSE.,                                                &         ! xstag, ystag
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%h2osoi_vol1, 1 ) * SIZE( grid%h2osoi_vol1, 3 ) .GT. 1 ) THEN 
CALL copy_fcnm (  &         
                  grid%h2osoi_vol1,   &       ! CD field
                 cids, cide, 1, config_flags%maxpatch, cjds, cjde,   &         ! CD dims
                 cims, cime, 1, config_flags%maxpatch, cjms, cjme,   &         ! CD dims
                 cips, cipe, 1, config_flags%maxpatch, cjps, cjpe,   &         ! CD dims
                  ngrid%h2osoi_vol1,  &   ! ND field
                 nids, nide, 1, config_flags%maxpatch, njds, njde,   &         ! ND dims
                 nims, nime, 1, config_flags%maxpatch, njms, njme,   &         ! ND dims
                 nips, nipe, 1, config_flags%maxpatch, njps, njpe,   &         ! ND dims
                  config_flags%shw, ngrid%imask_nostag,         &         ! stencil half width
                  .FALSE., .FALSE.,                                                &         ! xstag, ystag
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%h2osoi_vol2, 1 ) * SIZE( grid%h2osoi_vol2, 3 ) .GT. 1 ) THEN 
CALL copy_fcnm (  &         
                  grid%h2osoi_vol2,   &       ! CD field
                 cids, cide, 1, config_flags%maxpatch, cjds, cjde,   &         ! CD dims
                 cims, cime, 1, config_flags%maxpatch, cjms, cjme,   &         ! CD dims
                 cips, cipe, 1, config_flags%maxpatch, cjps, cjpe,   &         ! CD dims
                  ngrid%h2osoi_vol2,  &   ! ND field
                 nids, nide, 1, config_flags%maxpatch, njds, njde,   &         ! ND dims
                 nims, nime, 1, config_flags%maxpatch, njms, njme,   &         ! ND dims
                 nips, nipe, 1, config_flags%maxpatch, njps, njpe,   &         ! ND dims
                  config_flags%shw, ngrid%imask_nostag,         &         ! stencil half width
                  .FALSE., .FALSE.,                                                &         ! xstag, ystag
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%h2osoi_vol3, 1 ) * SIZE( grid%h2osoi_vol3, 3 ) .GT. 1 ) THEN 
CALL copy_fcnm (  &         
                  grid%h2osoi_vol3,   &       ! CD field
                 cids, cide, 1, config_flags%maxpatch, cjds, cjde,   &         ! CD dims
                 cims, cime, 1, config_flags%maxpatch, cjms, cjme,   &         ! CD dims
                 cips, cipe, 1, config_flags%maxpatch, cjps, cjpe,   &         ! CD dims
                  ngrid%h2osoi_vol3,  &   ! ND field
                 nids, nide, 1, config_flags%maxpatch, njds, njde,   &         ! ND dims
                 nims, nime, 1, config_flags%maxpatch, njms, njme,   &         ! ND dims
                 nips, nipe, 1, config_flags%maxpatch, njps, njpe,   &         ! ND dims
                  config_flags%shw, ngrid%imask_nostag,         &         ! stencil half width
                  .FALSE., .FALSE.,                                                &         ! xstag, ystag
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%h2osoi_vol4, 1 ) * SIZE( grid%h2osoi_vol4, 3 ) .GT. 1 ) THEN 
CALL copy_fcnm (  &         
                  grid%h2osoi_vol4,   &       ! CD field
                 cids, cide, 1, config_flags%maxpatch, cjds, cjde,   &         ! CD dims
                 cims, cime, 1, config_flags%maxpatch, cjms, cjme,   &         ! CD dims
                 cips, cipe, 1, config_flags%maxpatch, cjps, cjpe,   &         ! CD dims
                  ngrid%h2osoi_vol4,  &   ! ND field
                 nids, nide, 1, config_flags%maxpatch, njds, njde,   &         ! ND dims
                 nims, nime, 1, config_flags%maxpatch, njms, njme,   &         ! ND dims
                 nips, nipe, 1, config_flags%maxpatch, njps, njpe,   &         ! ND dims
                  config_flags%shw, ngrid%imask_nostag,         &         ! stencil half width
                  .FALSE., .FALSE.,                                                &         ! xstag, ystag
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%h2osoi_vol5, 1 ) * SIZE( grid%h2osoi_vol5, 3 ) .GT. 1 ) THEN 
CALL copy_fcnm (  &         
                  grid%h2osoi_vol5,   &       ! CD field
                 cids, cide, 1, config_flags%maxpatch, cjds, cjde,   &         ! CD dims
                 cims, cime, 1, config_flags%maxpatch, cjms, cjme,   &         ! CD dims
                 cips, cipe, 1, config_flags%maxpatch, cjps, cjpe,   &         ! CD dims
                  ngrid%h2osoi_vol5,  &   ! ND field
                 nids, nide, 1, config_flags%maxpatch, njds, njde,   &         ! ND dims
                 nims, nime, 1, config_flags%maxpatch, njms, njme,   &         ! ND dims
                 nips, nipe, 1, config_flags%maxpatch, njps, njpe,   &         ! ND dims
                  config_flags%shw, ngrid%imask_nostag,         &         ! stencil half width
                  .FALSE., .FALSE.,                                                &         ! xstag, ystag
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%h2osoi_vol6, 1 ) * SIZE( grid%h2osoi_vol6, 3 ) .GT. 1 ) THEN 
CALL copy_fcnm (  &         
                  grid%h2osoi_vol6,   &       ! CD field
                 cids, cide, 1, config_flags%maxpatch, cjds, cjde,   &         ! CD dims
                 cims, cime, 1, config_flags%maxpatch, cjms, cjme,   &         ! CD dims
                 cips, cipe, 1, config_flags%maxpatch, cjps, cjpe,   &         ! CD dims
                  ngrid%h2osoi_vol6,  &   ! ND field
                 nids, nide, 1, config_flags%maxpatch, njds, njde,   &         ! ND dims
                 nims, nime, 1, config_flags%maxpatch, njms, njme,   &         ! ND dims
                 nips, nipe, 1, config_flags%maxpatch, njps, njpe,   &         ! ND dims
                  config_flags%shw, ngrid%imask_nostag,         &         ! stencil half width
                  .FALSE., .FALSE.,                                                &         ! xstag, ystag
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%h2osoi_vol7, 1 ) * SIZE( grid%h2osoi_vol7, 3 ) .GT. 1 ) THEN 
CALL copy_fcnm (  &         
                  grid%h2osoi_vol7,   &       ! CD field
                 cids, cide, 1, config_flags%maxpatch, cjds, cjde,   &         ! CD dims
                 cims, cime, 1, config_flags%maxpatch, cjms, cjme,   &         ! CD dims
                 cips, cipe, 1, config_flags%maxpatch, cjps, cjpe,   &         ! CD dims
                  ngrid%h2osoi_vol7,  &   ! ND field
                 nids, nide, 1, config_flags%maxpatch, njds, njde,   &         ! ND dims
                 nims, nime, 1, config_flags%maxpatch, njms, njme,   &         ! ND dims
                 nips, nipe, 1, config_flags%maxpatch, njps, njpe,   &         ! ND dims
                  config_flags%shw, ngrid%imask_nostag,         &         ! stencil half width
                  .FALSE., .FALSE.,                                                &         ! xstag, ystag
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%h2osoi_vol8, 1 ) * SIZE( grid%h2osoi_vol8, 3 ) .GT. 1 ) THEN 
CALL copy_fcnm (  &         
                  grid%h2osoi_vol8,   &       ! CD field
                 cids, cide, 1, config_flags%maxpatch, cjds, cjde,   &         ! CD dims
                 cims, cime, 1, config_flags%maxpatch, cjms, cjme,   &         ! CD dims
                 cips, cipe, 1, config_flags%maxpatch, cjps, cjpe,   &         ! CD dims
                  ngrid%h2osoi_vol8,  &   ! ND field
                 nids, nide, 1, config_flags%maxpatch, njds, njde,   &         ! ND dims
                 nims, nime, 1, config_flags%maxpatch, njms, njme,   &         ! ND dims
                 nips, nipe, 1, config_flags%maxpatch, njps, njpe,   &         ! ND dims
                  config_flags%shw, ngrid%imask_nostag,         &         ! stencil half width
                  .FALSE., .FALSE.,                                                &         ! xstag, ystag
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%h2osoi_vol9, 1 ) * SIZE( grid%h2osoi_vol9, 3 ) .GT. 1 ) THEN 
CALL copy_fcnm (  &         
                  grid%h2osoi_vol9,   &       ! CD field
                 cids, cide, 1, config_flags%maxpatch, cjds, cjde,   &         ! CD dims
                 cims, cime, 1, config_flags%maxpatch, cjms, cjme,   &         ! CD dims
                 cips, cipe, 1, config_flags%maxpatch, cjps, cjpe,   &         ! CD dims
                  ngrid%h2osoi_vol9,  &   ! ND field
                 nids, nide, 1, config_flags%maxpatch, njds, njde,   &         ! ND dims
                 nims, nime, 1, config_flags%maxpatch, njms, njme,   &         ! ND dims
                 nips, nipe, 1, config_flags%maxpatch, njps, njpe,   &         ! ND dims
                  config_flags%shw, ngrid%imask_nostag,         &         ! stencil half width
                  .FALSE., .FALSE.,                                                &         ! xstag, ystag
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%h2osoi_vol10, 1 ) * SIZE( grid%h2osoi_vol10, 3 ) .GT. 1 ) THEN 
CALL copy_fcnm (  &         
                  grid%h2osoi_vol10,   &       ! CD field
                 cids, cide, 1, config_flags%maxpatch, cjds, cjde,   &         ! CD dims
                 cims, cime, 1, config_flags%maxpatch, cjms, cjme,   &         ! CD dims
                 cips, cipe, 1, config_flags%maxpatch, cjps, cjpe,   &         ! CD dims
                  ngrid%h2osoi_vol10,  &   ! ND field
                 nids, nide, 1, config_flags%maxpatch, njds, njde,   &         ! ND dims
                 nims, nime, 1, config_flags%maxpatch, njms, njme,   &         ! ND dims
                 nips, nipe, 1, config_flags%maxpatch, njps, njpe,   &         ! ND dims
                  config_flags%shw, ngrid%imask_nostag,         &         ! stencil half width
                  .FALSE., .FALSE.,                                                &         ! xstag, ystag
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%albedosubgrid, 1 ) * SIZE( grid%albedosubgrid, 3 ) .GT. 1 ) THEN 
CALL copy_fcnm (  &         
                  grid%albedosubgrid,   &       ! CD field
                 cids, cide, 1, config_flags%maxpatch, cjds, cjde,   &         ! CD dims
                 cims, cime, 1, config_flags%maxpatch, cjms, cjme,   &         ! CD dims
                 cips, cipe, 1, config_flags%maxpatch, cjps, cjpe,   &         ! CD dims
                  ngrid%albedosubgrid,  &   ! ND field
                 nids, nide, 1, config_flags%maxpatch, njds, njde,   &         ! ND dims
                 nims, nime, 1, config_flags%maxpatch, njms, njme,   &         ! ND dims
                 nips, nipe, 1, config_flags%maxpatch, njps, njpe,   &         ! ND dims
                  config_flags%shw, ngrid%imask_nostag,         &         ! stencil half width
                  .FALSE., .FALSE.,                                                &         ! xstag, ystag
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%lhsubgrid, 1 ) * SIZE( grid%lhsubgrid, 3 ) .GT. 1 ) THEN 
CALL copy_fcnm (  &         
                  grid%lhsubgrid,   &       ! CD field
                 cids, cide, 1, config_flags%maxpatch, cjds, cjde,   &         ! CD dims
                 cims, cime, 1, config_flags%maxpatch, cjms, cjme,   &         ! CD dims
                 cips, cipe, 1, config_flags%maxpatch, cjps, cjpe,   &         ! CD dims
                  ngrid%lhsubgrid,  &   ! ND field
                 nids, nide, 1, config_flags%maxpatch, njds, njde,   &         ! ND dims
                 nims, nime, 1, config_flags%maxpatch, njms, njme,   &         ! ND dims
                 nips, nipe, 1, config_flags%maxpatch, njps, njpe,   &         ! ND dims
                  config_flags%shw, ngrid%imask_nostag,         &         ! stencil half width
                  .FALSE., .FALSE.,                                                &         ! xstag, ystag
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%hfxsubgrid, 1 ) * SIZE( grid%hfxsubgrid, 3 ) .GT. 1 ) THEN 
CALL copy_fcnm (  &         
                  grid%hfxsubgrid,   &       ! CD field
                 cids, cide, 1, config_flags%maxpatch, cjds, cjde,   &         ! CD dims
                 cims, cime, 1, config_flags%maxpatch, cjms, cjme,   &         ! CD dims
                 cips, cipe, 1, config_flags%maxpatch, cjps, cjpe,   &         ! CD dims
                  ngrid%hfxsubgrid,  &   ! ND field
                 nids, nide, 1, config_flags%maxpatch, njds, njde,   &         ! ND dims
                 nims, nime, 1, config_flags%maxpatch, njms, njme,   &         ! ND dims
                 nips, nipe, 1, config_flags%maxpatch, njps, njpe,   &         ! ND dims
                  config_flags%shw, ngrid%imask_nostag,         &         ! stencil half width
                  .FALSE., .FALSE.,                                                &         ! xstag, ystag
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%lwupsubgrid, 1 ) * SIZE( grid%lwupsubgrid, 3 ) .GT. 1 ) THEN 
CALL copy_fcnm (  &         
                  grid%lwupsubgrid,   &       ! CD field
                 cids, cide, 1, config_flags%maxpatch, cjds, cjde,   &         ! CD dims
                 cims, cime, 1, config_flags%maxpatch, cjms, cjme,   &         ! CD dims
                 cips, cipe, 1, config_flags%maxpatch, cjps, cjpe,   &         ! CD dims
                  ngrid%lwupsubgrid,  &   ! ND field
                 nids, nide, 1, config_flags%maxpatch, njds, njde,   &         ! ND dims
                 nims, nime, 1, config_flags%maxpatch, njms, njme,   &         ! ND dims
                 nips, nipe, 1, config_flags%maxpatch, njps, njpe,   &         ! ND dims
                  config_flags%shw, ngrid%imask_nostag,         &         ! stencil half width
                  .FALSE., .FALSE.,                                                &         ! xstag, ystag
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%q2subgrid, 1 ) * SIZE( grid%q2subgrid, 3 ) .GT. 1 ) THEN 
CALL copy_fcnm (  &         
                  grid%q2subgrid,   &       ! CD field
                 cids, cide, 1, config_flags%maxpatch, cjds, cjde,   &         ! CD dims
                 cims, cime, 1, config_flags%maxpatch, cjms, cjme,   &         ! CD dims
                 cips, cipe, 1, config_flags%maxpatch, cjps, cjpe,   &         ! CD dims
                  ngrid%q2subgrid,  &   ! ND field
                 nids, nide, 1, config_flags%maxpatch, njds, njde,   &         ! ND dims
                 nims, nime, 1, config_flags%maxpatch, njms, njme,   &         ! ND dims
                 nips, nipe, 1, config_flags%maxpatch, njps, njpe,   &         ! ND dims
                  config_flags%shw, ngrid%imask_nostag,         &         ! stencil half width
                  .FALSE., .FALSE.,                                                &         ! xstag, ystag
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%sabvsubgrid, 1 ) * SIZE( grid%sabvsubgrid, 3 ) .GT. 1 ) THEN 
CALL copy_fcnm (  &         
                  grid%sabvsubgrid,   &       ! CD field
                 cids, cide, 1, config_flags%maxpatch, cjds, cjde,   &         ! CD dims
                 cims, cime, 1, config_flags%maxpatch, cjms, cjme,   &         ! CD dims
                 cips, cipe, 1, config_flags%maxpatch, cjps, cjpe,   &         ! CD dims
                  ngrid%sabvsubgrid,  &   ! ND field
                 nids, nide, 1, config_flags%maxpatch, njds, njde,   &         ! ND dims
                 nims, nime, 1, config_flags%maxpatch, njms, njme,   &         ! ND dims
                 nips, nipe, 1, config_flags%maxpatch, njps, njpe,   &         ! ND dims
                  config_flags%shw, ngrid%imask_nostag,         &         ! stencil half width
                  .FALSE., .FALSE.,                                                &         ! xstag, ystag
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%sabgsubgrid, 1 ) * SIZE( grid%sabgsubgrid, 3 ) .GT. 1 ) THEN 
CALL copy_fcnm (  &         
                  grid%sabgsubgrid,   &       ! CD field
                 cids, cide, 1, config_flags%maxpatch, cjds, cjde,   &         ! CD dims
                 cims, cime, 1, config_flags%maxpatch, cjms, cjme,   &         ! CD dims
                 cips, cipe, 1, config_flags%maxpatch, cjps, cjpe,   &         ! CD dims
                  ngrid%sabgsubgrid,  &   ! ND field
                 nids, nide, 1, config_flags%maxpatch, njds, njde,   &         ! ND dims
                 nims, nime, 1, config_flags%maxpatch, njms, njme,   &         ! ND dims
                 nips, nipe, 1, config_flags%maxpatch, njps, njpe,   &         ! ND dims
                  config_flags%shw, ngrid%imask_nostag,         &         ! stencil half width
                  .FALSE., .FALSE.,                                                &         ! xstag, ystag
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%nrasubgrid, 1 ) * SIZE( grid%nrasubgrid, 3 ) .GT. 1 ) THEN 
CALL copy_fcnm (  &         
                  grid%nrasubgrid,   &       ! CD field
                 cids, cide, 1, config_flags%maxpatch, cjds, cjde,   &         ! CD dims
                 cims, cime, 1, config_flags%maxpatch, cjms, cjme,   &         ! CD dims
                 cips, cipe, 1, config_flags%maxpatch, cjps, cjpe,   &         ! CD dims
                  ngrid%nrasubgrid,  &   ! ND field
                 nids, nide, 1, config_flags%maxpatch, njds, njde,   &         ! ND dims
                 nims, nime, 1, config_flags%maxpatch, njms, njme,   &         ! ND dims
                 nips, nipe, 1, config_flags%maxpatch, njps, njpe,   &         ! ND dims
                  config_flags%shw, ngrid%imask_nostag,         &         ! stencil half width
                  .FALSE., .FALSE.,                                                &         ! xstag, ystag
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%swupsubgrid, 1 ) * SIZE( grid%swupsubgrid, 3 ) .GT. 1 ) THEN 
CALL copy_fcnm (  &         
                  grid%swupsubgrid,   &       ! CD field
                 cids, cide, 1, config_flags%maxpatch, cjds, cjde,   &         ! CD dims
                 cims, cime, 1, config_flags%maxpatch, cjms, cjme,   &         ! CD dims
                 cips, cipe, 1, config_flags%maxpatch, cjps, cjpe,   &         ! CD dims
                  ngrid%swupsubgrid,  &   ! ND field
                 nids, nide, 1, config_flags%maxpatch, njds, njde,   &         ! ND dims
                 nims, nime, 1, config_flags%maxpatch, njms, njme,   &         ! ND dims
                 nips, nipe, 1, config_flags%maxpatch, njps, njpe,   &         ! ND dims
                  config_flags%shw, ngrid%imask_nostag,         &         ! stencil half width
                  .FALSE., .FALSE.,                                                &         ! xstag, ystag
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%th_old, 1 ) * SIZE( grid%th_old, 3 ) .GT. 1 ) THEN 
CALL copy_fcn (  &         
                  grid%th_old,   &       ! CD field
                 cids, cide, ckds, ckde, cjds, cjde,   &         ! CD dims
                 cims, cime, ckms, ckme, cjms, cjme,   &         ! CD dims
                 cips, cipe, ckps, MIN( (ckde-1), ckpe ), cjps, cjpe,   &         ! CD dims
                  ngrid%th_old,  &   ! ND field
                 nids, nide, nkds, nkde, njds, njde,   &         ! ND dims
                 nims, nime, nkms, nkme, njms, njme,   &         ! ND dims
                 nips, nipe, nkps, MIN( (nkde-1), nkpe ), njps, njpe,   &         ! ND dims
                  config_flags%shw, ngrid%imask_nostag,         &         ! stencil half width
                  .FALSE., .FALSE.,                                                &         ! xstag, ystag
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%qv_old, 1 ) * SIZE( grid%qv_old, 3 ) .GT. 1 ) THEN 
CALL copy_fcn (  &         
                  grid%qv_old,   &       ! CD field
                 cids, cide, ckds, ckde, cjds, cjde,   &         ! CD dims
                 cims, cime, ckms, ckme, cjms, cjme,   &         ! CD dims
                 cips, cipe, ckps, MIN( (ckde-1), ckpe ), cjps, cjpe,   &         ! CD dims
                  ngrid%qv_old,  &   ! ND field
                 nids, nide, nkds, nkde, njds, njde,   &         ! ND dims
                 nims, nime, nkms, nkme, njms, njme,   &         ! ND dims
                 nips, nipe, nkps, MIN( (nkde-1), nkpe ), njps, njpe,   &         ! ND dims
                  config_flags%shw, ngrid%imask_nostag,         &         ! stencil half width
                  .FALSE., .FALSE.,                                                &         ! xstag, ystag
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%landmask, 1 ) * SIZE( grid%landmask, 2 ) .GT. 1 ) THEN 
CALL copy_fcnm (  &         
                  grid%landmask,   &       ! CD field
                 cids, cide, 1, 1, cjds, cjde,   &         ! CD dims
                 cims, cime, 1, 1, cjms, cjme,   &         ! CD dims
                 cips, cipe, 1, 1, cjps, cjpe,   &         ! CD dims
                  ngrid%landmask,  &   ! ND field
                 nids, nide, 1, 1, njds, njde,   &         ! ND dims
                 nims, nime, 1, 1, njms, njme,   &         ! ND dims
                 nips, nipe, 1, 1, njps, njpe,   &         ! ND dims
                  config_flags%shw, ngrid%imask_nostag,         &         ! stencil half width
                  .FALSE., .FALSE.,                                                &         ! xstag, ystag
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%lakemask, 1 ) * SIZE( grid%lakemask, 2 ) .GT. 1 ) THEN 
CALL copy_fcnm (  &         
                  grid%lakemask,   &       ! CD field
                 cids, cide, 1, 1, cjds, cjde,   &         ! CD dims
                 cims, cime, 1, 1, cjms, cjme,   &         ! CD dims
                 cips, cipe, 1, 1, cjps, cjpe,   &         ! CD dims
                  ngrid%lakemask,  &   ! ND field
                 nids, nide, 1, 1, njds, njde,   &         ! ND dims
                 nims, nime, 1, 1, njms, njme,   &         ! ND dims
                 nips, nipe, 1, 1, njps, njpe,   &         ! ND dims
                  config_flags%shw, ngrid%imask_nostag,         &         ! stencil half width
                  .FALSE., .FALSE.,                                                &         ! xstag, ystag
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
DO itrace = PARAM_FIRST_SCALAR, num_tracer
IF ( SIZE( tracer, 1 ) * SIZE( tracer, 3 ) .GT. 1 ) THEN 
CALL copy_fcn (  &         
                  tracer(grid%sm31,grid%sm32,grid%sm33,itrace),   &       ! CD field
                 cids, cide, ckds, ckde, cjds, cjde,   &         ! CD dims
                 cims, cime, ckms, ckme, cjms, cjme,   &         ! CD dims
                 cips, cipe, ckps, MIN( (ckde-1), ckpe ), cjps, cjpe,   &         ! CD dims
                  ngrid%tracer(ngrid%sm31,ngrid%sm32,ngrid%sm33,itrace),  &   ! ND field
                 nids, nide, nkds, nkde, njds, njde,   &         ! ND dims
                 nims, nime, nkms, nkme, njms, njme,   &         ! ND dims
                 nips, nipe, nkps, MIN( (nkde-1), nkpe ), njps, njpe,   &         ! ND dims
                  config_flags%shw, ngrid%imask_nostag,         &         ! stencil half width
                  .FALSE., .FALSE.,                                                &         ! xstag, ystag
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
ENDDO
!ENDOFREGISTRYGENERATEDINCLUDE
