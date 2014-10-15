!STARTOFREGISTRYGENERATEDINCLUDE 'inc/wrf_bdyout.inc'
!
! WARNING This file is generated automatically by use_registry
! using the data base in the file named Registry.
! Do not edit.  Your changes to this file will be lost.
!
CALL wrf_ext_write_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'U_BXS'               , &  ! Data Name 
                       grid%u_bxs(jms,kds,1)     , &  ! Field 
                       WRF_FLOAT          , &  ! FieldType 
                       grid%communicator , &  ! Comm
                       grid%iocommunicator , &  ! Comm
                       grid%domdesc      , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       dryrun             , &  ! flag
                       'XSZ'               , &  ! MemoryOrder
                       'X'               , &  ! Stagger
                       'south_north'               , &  ! Dimname 1 
                       'bottom_top'               , &  ! Dimname 2 
                       'bdy_width'               , &  ! Dimname 3 
                       'bdy x-wind component'               , &  ! Desc  
                       'm s-1'               , &  ! Units 
'inc/wrf_bdyout.inc ext_write_field U_BXS memorder XSZ' , & ! Debug message
1, (jde-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
jms, jme, kds, kde, 1, config_flags%spec_bdy_width, &
jps, MIN( (jde-1), jpe ), kds, (kde-1), 1, config_flags%spec_bdy_width, &
                       ierr )
CALL wrf_ext_write_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'U_BXE'               , &  ! Data Name 
                       grid%u_bxe(jms,kds,1)     , &  ! Field 
                       WRF_FLOAT          , &  ! FieldType 
                       grid%communicator , &  ! Comm
                       grid%iocommunicator , &  ! Comm
                       grid%domdesc      , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       dryrun             , &  ! flag
                       'XEZ'               , &  ! MemoryOrder
                       'X'               , &  ! Stagger
                       'south_north'               , &  ! Dimname 1 
                       'bottom_top'               , &  ! Dimname 2 
                       'bdy_width'               , &  ! Dimname 3 
                       'bdy x-wind component'               , &  ! Desc  
                       'm s-1'               , &  ! Units 
'inc/wrf_bdyout.inc ext_write_field U_BXE memorder XEZ' , & ! Debug message
1, (jde-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
jms, jme, kds, kde, 1, config_flags%spec_bdy_width, &
jps, MIN( (jde-1), jpe ), kds, (kde-1), 1, config_flags%spec_bdy_width, &
                       ierr )
CALL wrf_ext_write_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'U_BYS'               , &  ! Data Name 
                       grid%u_bys(ims,kds,1)     , &  ! Field 
                       WRF_FLOAT          , &  ! FieldType 
                       grid%communicator , &  ! Comm
                       grid%iocommunicator , &  ! Comm
                       grid%domdesc      , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       dryrun             , &  ! flag
                       'YSZ'               , &  ! MemoryOrder
                       'X'               , &  ! Stagger
                       'west_east_stag'               , &  ! Dimname 1 
                       'bottom_top'               , &  ! Dimname 2 
                       'bdy_width'               , &  ! Dimname 3 
                       'bdy x-wind component'               , &  ! Desc  
                       'm s-1'               , &  ! Units 
'inc/wrf_bdyout.inc ext_write_field U_BYS memorder YSZ' , & ! Debug message
1, ide, kds, (kde-1), 1, config_flags%spec_bdy_width, &
ims, ime, kds, kde, 1, config_flags%spec_bdy_width, &
ips, MIN( ide, ipe ), kds, (kde-1), 1, config_flags%spec_bdy_width, &
                       ierr )
CALL wrf_ext_write_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'U_BYE'               , &  ! Data Name 
                       grid%u_bye(ims,kds,1)     , &  ! Field 
                       WRF_FLOAT          , &  ! FieldType 
                       grid%communicator , &  ! Comm
                       grid%iocommunicator , &  ! Comm
                       grid%domdesc      , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       dryrun             , &  ! flag
                       'YEZ'               , &  ! MemoryOrder
                       'X'               , &  ! Stagger
                       'west_east_stag'               , &  ! Dimname 1 
                       'bottom_top'               , &  ! Dimname 2 
                       'bdy_width'               , &  ! Dimname 3 
                       'bdy x-wind component'               , &  ! Desc  
                       'm s-1'               , &  ! Units 
'inc/wrf_bdyout.inc ext_write_field U_BYE memorder YEZ' , & ! Debug message
1, ide, kds, (kde-1), 1, config_flags%spec_bdy_width, &
ims, ime, kds, kde, 1, config_flags%spec_bdy_width, &
ips, MIN( ide, ipe ), kds, (kde-1), 1, config_flags%spec_bdy_width, &
                       ierr )
CALL wrf_ext_write_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'U_BTXS'               , &  ! Data Name 
                       grid%u_btxs(jms,kds,1)     , &  ! Field 
                       WRF_FLOAT          , &  ! FieldType 
                       grid%communicator , &  ! Comm
                       grid%iocommunicator , &  ! Comm
                       grid%domdesc      , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       dryrun             , &  ! flag
                       'XSZ'               , &  ! MemoryOrder
                       'X'               , &  ! Stagger
                       'south_north'               , &  ! Dimname 1 
                       'bottom_top'               , &  ! Dimname 2 
                       'bdy_width'               , &  ! Dimname 3 
                       'bdy tend x-wind component'               , &  ! Desc  
                       '(m s-1)/dt'               , &  ! Units 
'inc/wrf_bdyout.inc ext_write_field U_BTXS memorder XSZ' , & ! Debug message
1, (jde-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
jms, jme, kds, kde, 1, config_flags%spec_bdy_width, &
jps, MIN( (jde-1), jpe ), kds, (kde-1), 1, config_flags%spec_bdy_width, &
                       ierr )
CALL wrf_ext_write_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'U_BTXE'               , &  ! Data Name 
                       grid%u_btxe(jms,kds,1)     , &  ! Field 
                       WRF_FLOAT          , &  ! FieldType 
                       grid%communicator , &  ! Comm
                       grid%iocommunicator , &  ! Comm
                       grid%domdesc      , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       dryrun             , &  ! flag
                       'XEZ'               , &  ! MemoryOrder
                       'X'               , &  ! Stagger
                       'south_north'               , &  ! Dimname 1 
                       'bottom_top'               , &  ! Dimname 2 
                       'bdy_width'               , &  ! Dimname 3 
                       'bdy tend x-wind component'               , &  ! Desc  
                       '(m s-1)/dt'               , &  ! Units 
'inc/wrf_bdyout.inc ext_write_field U_BTXE memorder XEZ' , & ! Debug message
1, (jde-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
jms, jme, kds, kde, 1, config_flags%spec_bdy_width, &
jps, MIN( (jde-1), jpe ), kds, (kde-1), 1, config_flags%spec_bdy_width, &
                       ierr )
CALL wrf_ext_write_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'U_BTYS'               , &  ! Data Name 
                       grid%u_btys(ims,kds,1)     , &  ! Field 
                       WRF_FLOAT          , &  ! FieldType 
                       grid%communicator , &  ! Comm
                       grid%iocommunicator , &  ! Comm
                       grid%domdesc      , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       dryrun             , &  ! flag
                       'YSZ'               , &  ! MemoryOrder
                       'X'               , &  ! Stagger
                       'west_east_stag'               , &  ! Dimname 1 
                       'bottom_top'               , &  ! Dimname 2 
                       'bdy_width'               , &  ! Dimname 3 
                       'bdy tend x-wind component'               , &  ! Desc  
                       '(m s-1)/dt'               , &  ! Units 
'inc/wrf_bdyout.inc ext_write_field U_BTYS memorder YSZ' , & ! Debug message
1, ide, kds, (kde-1), 1, config_flags%spec_bdy_width, &
ims, ime, kds, kde, 1, config_flags%spec_bdy_width, &
ips, MIN( ide, ipe ), kds, (kde-1), 1, config_flags%spec_bdy_width, &
                       ierr )
CALL wrf_ext_write_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'U_BTYE'               , &  ! Data Name 
                       grid%u_btye(ims,kds,1)     , &  ! Field 
                       WRF_FLOAT          , &  ! FieldType 
                       grid%communicator , &  ! Comm
                       grid%iocommunicator , &  ! Comm
                       grid%domdesc      , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       dryrun             , &  ! flag
                       'YEZ'               , &  ! MemoryOrder
                       'X'               , &  ! Stagger
                       'west_east_stag'               , &  ! Dimname 1 
                       'bottom_top'               , &  ! Dimname 2 
                       'bdy_width'               , &  ! Dimname 3 
                       'bdy tend x-wind component'               , &  ! Desc  
                       '(m s-1)/dt'               , &  ! Units 
'inc/wrf_bdyout.inc ext_write_field U_BTYE memorder YEZ' , & ! Debug message
1, ide, kds, (kde-1), 1, config_flags%spec_bdy_width, &
ims, ime, kds, kde, 1, config_flags%spec_bdy_width, &
ips, MIN( ide, ipe ), kds, (kde-1), 1, config_flags%spec_bdy_width, &
                       ierr )
CALL wrf_ext_write_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'V_BXS'               , &  ! Data Name 
                       grid%v_bxs(jms,kds,1)     , &  ! Field 
                       WRF_FLOAT          , &  ! FieldType 
                       grid%communicator , &  ! Comm
                       grid%iocommunicator , &  ! Comm
                       grid%domdesc      , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       dryrun             , &  ! flag
                       'XSZ'               , &  ! MemoryOrder
                       'Y'               , &  ! Stagger
                       'south_north_stag'               , &  ! Dimname 1 
                       'bottom_top'               , &  ! Dimname 2 
                       'bdy_width'               , &  ! Dimname 3 
                       'bdy y-wind component'               , &  ! Desc  
                       'm s-1'               , &  ! Units 
'inc/wrf_bdyout.inc ext_write_field V_BXS memorder XSZ' , & ! Debug message
1, jde, kds, (kde-1), 1, config_flags%spec_bdy_width, &
jms, jme, kds, kde, 1, config_flags%spec_bdy_width, &
jps, MIN( jde, jpe ), kds, (kde-1), 1, config_flags%spec_bdy_width, &
                       ierr )
CALL wrf_ext_write_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'V_BXE'               , &  ! Data Name 
                       grid%v_bxe(jms,kds,1)     , &  ! Field 
                       WRF_FLOAT          , &  ! FieldType 
                       grid%communicator , &  ! Comm
                       grid%iocommunicator , &  ! Comm
                       grid%domdesc      , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       dryrun             , &  ! flag
                       'XEZ'               , &  ! MemoryOrder
                       'Y'               , &  ! Stagger
                       'south_north_stag'               , &  ! Dimname 1 
                       'bottom_top'               , &  ! Dimname 2 
                       'bdy_width'               , &  ! Dimname 3 
                       'bdy y-wind component'               , &  ! Desc  
                       'm s-1'               , &  ! Units 
'inc/wrf_bdyout.inc ext_write_field V_BXE memorder XEZ' , & ! Debug message
1, jde, kds, (kde-1), 1, config_flags%spec_bdy_width, &
jms, jme, kds, kde, 1, config_flags%spec_bdy_width, &
jps, MIN( jde, jpe ), kds, (kde-1), 1, config_flags%spec_bdy_width, &
                       ierr )
CALL wrf_ext_write_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'V_BYS'               , &  ! Data Name 
                       grid%v_bys(ims,kds,1)     , &  ! Field 
                       WRF_FLOAT          , &  ! FieldType 
                       grid%communicator , &  ! Comm
                       grid%iocommunicator , &  ! Comm
                       grid%domdesc      , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       dryrun             , &  ! flag
                       'YSZ'               , &  ! MemoryOrder
                       'Y'               , &  ! Stagger
                       'west_east'               , &  ! Dimname 1 
                       'bottom_top'               , &  ! Dimname 2 
                       'bdy_width'               , &  ! Dimname 3 
                       'bdy y-wind component'               , &  ! Desc  
                       'm s-1'               , &  ! Units 
'inc/wrf_bdyout.inc ext_write_field V_BYS memorder YSZ' , & ! Debug message
1, (ide-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
ims, ime, kds, kde, 1, config_flags%spec_bdy_width, &
ips, MIN( (ide-1), ipe ), kds, (kde-1), 1, config_flags%spec_bdy_width, &
                       ierr )
CALL wrf_ext_write_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'V_BYE'               , &  ! Data Name 
                       grid%v_bye(ims,kds,1)     , &  ! Field 
                       WRF_FLOAT          , &  ! FieldType 
                       grid%communicator , &  ! Comm
                       grid%iocommunicator , &  ! Comm
                       grid%domdesc      , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       dryrun             , &  ! flag
                       'YEZ'               , &  ! MemoryOrder
                       'Y'               , &  ! Stagger
                       'west_east'               , &  ! Dimname 1 
                       'bottom_top'               , &  ! Dimname 2 
                       'bdy_width'               , &  ! Dimname 3 
                       'bdy y-wind component'               , &  ! Desc  
                       'm s-1'               , &  ! Units 
'inc/wrf_bdyout.inc ext_write_field V_BYE memorder YEZ' , & ! Debug message
1, (ide-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
ims, ime, kds, kde, 1, config_flags%spec_bdy_width, &
ips, MIN( (ide-1), ipe ), kds, (kde-1), 1, config_flags%spec_bdy_width, &
                       ierr )
CALL wrf_ext_write_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'V_BTXS'               , &  ! Data Name 
                       grid%v_btxs(jms,kds,1)     , &  ! Field 
                       WRF_FLOAT          , &  ! FieldType 
                       grid%communicator , &  ! Comm
                       grid%iocommunicator , &  ! Comm
                       grid%domdesc      , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       dryrun             , &  ! flag
                       'XSZ'               , &  ! MemoryOrder
                       'Y'               , &  ! Stagger
                       'south_north_stag'               , &  ! Dimname 1 
                       'bottom_top'               , &  ! Dimname 2 
                       'bdy_width'               , &  ! Dimname 3 
                       'bdy tend y-wind component'               , &  ! Desc  
                       '(m s-1)/dt'               , &  ! Units 
'inc/wrf_bdyout.inc ext_write_field V_BTXS memorder XSZ' , & ! Debug message
1, jde, kds, (kde-1), 1, config_flags%spec_bdy_width, &
jms, jme, kds, kde, 1, config_flags%spec_bdy_width, &
jps, MIN( jde, jpe ), kds, (kde-1), 1, config_flags%spec_bdy_width, &
                       ierr )
CALL wrf_ext_write_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'V_BTXE'               , &  ! Data Name 
                       grid%v_btxe(jms,kds,1)     , &  ! Field 
                       WRF_FLOAT          , &  ! FieldType 
                       grid%communicator , &  ! Comm
                       grid%iocommunicator , &  ! Comm
                       grid%domdesc      , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       dryrun             , &  ! flag
                       'XEZ'               , &  ! MemoryOrder
                       'Y'               , &  ! Stagger
                       'south_north_stag'               , &  ! Dimname 1 
                       'bottom_top'               , &  ! Dimname 2 
                       'bdy_width'               , &  ! Dimname 3 
                       'bdy tend y-wind component'               , &  ! Desc  
                       '(m s-1)/dt'               , &  ! Units 
'inc/wrf_bdyout.inc ext_write_field V_BTXE memorder XEZ' , & ! Debug message
1, jde, kds, (kde-1), 1, config_flags%spec_bdy_width, &
jms, jme, kds, kde, 1, config_flags%spec_bdy_width, &
jps, MIN( jde, jpe ), kds, (kde-1), 1, config_flags%spec_bdy_width, &
                       ierr )
CALL wrf_ext_write_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'V_BTYS'               , &  ! Data Name 
                       grid%v_btys(ims,kds,1)     , &  ! Field 
                       WRF_FLOAT          , &  ! FieldType 
                       grid%communicator , &  ! Comm
                       grid%iocommunicator , &  ! Comm
                       grid%domdesc      , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       dryrun             , &  ! flag
                       'YSZ'               , &  ! MemoryOrder
                       'Y'               , &  ! Stagger
                       'west_east'               , &  ! Dimname 1 
                       'bottom_top'               , &  ! Dimname 2 
                       'bdy_width'               , &  ! Dimname 3 
                       'bdy tend y-wind component'               , &  ! Desc  
                       '(m s-1)/dt'               , &  ! Units 
'inc/wrf_bdyout.inc ext_write_field V_BTYS memorder YSZ' , & ! Debug message
1, (ide-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
ims, ime, kds, kde, 1, config_flags%spec_bdy_width, &
ips, MIN( (ide-1), ipe ), kds, (kde-1), 1, config_flags%spec_bdy_width, &
                       ierr )
CALL wrf_ext_write_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'V_BTYE'               , &  ! Data Name 
                       grid%v_btye(ims,kds,1)     , &  ! Field 
                       WRF_FLOAT          , &  ! FieldType 
                       grid%communicator , &  ! Comm
                       grid%iocommunicator , &  ! Comm
                       grid%domdesc      , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       dryrun             , &  ! flag
                       'YEZ'               , &  ! MemoryOrder
                       'Y'               , &  ! Stagger
                       'west_east'               , &  ! Dimname 1 
                       'bottom_top'               , &  ! Dimname 2 
                       'bdy_width'               , &  ! Dimname 3 
                       'bdy tend y-wind component'               , &  ! Desc  
                       '(m s-1)/dt'               , &  ! Units 
'inc/wrf_bdyout.inc ext_write_field V_BTYE memorder YEZ' , & ! Debug message
1, (ide-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
ims, ime, kds, kde, 1, config_flags%spec_bdy_width, &
ips, MIN( (ide-1), ipe ), kds, (kde-1), 1, config_flags%spec_bdy_width, &
                       ierr )
CALL wrf_ext_write_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'W_BXS'               , &  ! Data Name 
                       grid%w_bxs(jms,kds,1)     , &  ! Field 
                       WRF_FLOAT          , &  ! FieldType 
                       grid%communicator , &  ! Comm
                       grid%iocommunicator , &  ! Comm
                       grid%domdesc      , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       dryrun             , &  ! flag
                       'XSZ'               , &  ! MemoryOrder
                       'Z'               , &  ! Stagger
                       'south_north'               , &  ! Dimname 1 
                       'bottom_top_stag'               , &  ! Dimname 2 
                       'bdy_width'               , &  ! Dimname 3 
                       'bdy z-wind component'               , &  ! Desc  
                       'm s-1'               , &  ! Units 
'inc/wrf_bdyout.inc ext_write_field W_BXS memorder XSZ' , & ! Debug message
1, (jde-1), kds, kde, 1, config_flags%spec_bdy_width, &
jms, jme, kds, kde, 1, config_flags%spec_bdy_width, &
jps, MIN( (jde-1), jpe ), kds, kde, 1, config_flags%spec_bdy_width, &
                       ierr )
CALL wrf_ext_write_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'W_BXE'               , &  ! Data Name 
                       grid%w_bxe(jms,kds,1)     , &  ! Field 
                       WRF_FLOAT          , &  ! FieldType 
                       grid%communicator , &  ! Comm
                       grid%iocommunicator , &  ! Comm
                       grid%domdesc      , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       dryrun             , &  ! flag
                       'XEZ'               , &  ! MemoryOrder
                       'Z'               , &  ! Stagger
                       'south_north'               , &  ! Dimname 1 
                       'bottom_top_stag'               , &  ! Dimname 2 
                       'bdy_width'               , &  ! Dimname 3 
                       'bdy z-wind component'               , &  ! Desc  
                       'm s-1'               , &  ! Units 
'inc/wrf_bdyout.inc ext_write_field W_BXE memorder XEZ' , & ! Debug message
1, (jde-1), kds, kde, 1, config_flags%spec_bdy_width, &
jms, jme, kds, kde, 1, config_flags%spec_bdy_width, &
jps, MIN( (jde-1), jpe ), kds, kde, 1, config_flags%spec_bdy_width, &
                       ierr )
CALL wrf_ext_write_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'W_BYS'               , &  ! Data Name 
                       grid%w_bys(ims,kds,1)     , &  ! Field 
                       WRF_FLOAT          , &  ! FieldType 
                       grid%communicator , &  ! Comm
                       grid%iocommunicator , &  ! Comm
                       grid%domdesc      , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       dryrun             , &  ! flag
                       'YSZ'               , &  ! MemoryOrder
                       'Z'               , &  ! Stagger
                       'west_east'               , &  ! Dimname 1 
                       'bottom_top_stag'               , &  ! Dimname 2 
                       'bdy_width'               , &  ! Dimname 3 
                       'bdy z-wind component'               , &  ! Desc  
                       'm s-1'               , &  ! Units 
'inc/wrf_bdyout.inc ext_write_field W_BYS memorder YSZ' , & ! Debug message
1, (ide-1), kds, kde, 1, config_flags%spec_bdy_width, &
ims, ime, kds, kde, 1, config_flags%spec_bdy_width, &
ips, MIN( (ide-1), ipe ), kds, kde, 1, config_flags%spec_bdy_width, &
                       ierr )
CALL wrf_ext_write_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'W_BYE'               , &  ! Data Name 
                       grid%w_bye(ims,kds,1)     , &  ! Field 
                       WRF_FLOAT          , &  ! FieldType 
                       grid%communicator , &  ! Comm
                       grid%iocommunicator , &  ! Comm
                       grid%domdesc      , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       dryrun             , &  ! flag
                       'YEZ'               , &  ! MemoryOrder
                       'Z'               , &  ! Stagger
                       'west_east'               , &  ! Dimname 1 
                       'bottom_top_stag'               , &  ! Dimname 2 
                       'bdy_width'               , &  ! Dimname 3 
                       'bdy z-wind component'               , &  ! Desc  
                       'm s-1'               , &  ! Units 
'inc/wrf_bdyout.inc ext_write_field W_BYE memorder YEZ' , & ! Debug message
1, (ide-1), kds, kde, 1, config_flags%spec_bdy_width, &
ims, ime, kds, kde, 1, config_flags%spec_bdy_width, &
ips, MIN( (ide-1), ipe ), kds, kde, 1, config_flags%spec_bdy_width, &
                       ierr )
CALL wrf_ext_write_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'W_BTXS'               , &  ! Data Name 
                       grid%w_btxs(jms,kds,1)     , &  ! Field 
                       WRF_FLOAT          , &  ! FieldType 
                       grid%communicator , &  ! Comm
                       grid%iocommunicator , &  ! Comm
                       grid%domdesc      , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       dryrun             , &  ! flag
                       'XSZ'               , &  ! MemoryOrder
                       'Z'               , &  ! Stagger
                       'south_north'               , &  ! Dimname 1 
                       'bottom_top_stag'               , &  ! Dimname 2 
                       'bdy_width'               , &  ! Dimname 3 
                       'bdy tend z-wind component'               , &  ! Desc  
                       '(m s-1)/dt'               , &  ! Units 
'inc/wrf_bdyout.inc ext_write_field W_BTXS memorder XSZ' , & ! Debug message
1, (jde-1), kds, kde, 1, config_flags%spec_bdy_width, &
jms, jme, kds, kde, 1, config_flags%spec_bdy_width, &
jps, MIN( (jde-1), jpe ), kds, kde, 1, config_flags%spec_bdy_width, &
                       ierr )
CALL wrf_ext_write_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'W_BTXE'               , &  ! Data Name 
                       grid%w_btxe(jms,kds,1)     , &  ! Field 
                       WRF_FLOAT          , &  ! FieldType 
                       grid%communicator , &  ! Comm
                       grid%iocommunicator , &  ! Comm
                       grid%domdesc      , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       dryrun             , &  ! flag
                       'XEZ'               , &  ! MemoryOrder
                       'Z'               , &  ! Stagger
                       'south_north'               , &  ! Dimname 1 
                       'bottom_top_stag'               , &  ! Dimname 2 
                       'bdy_width'               , &  ! Dimname 3 
                       'bdy tend z-wind component'               , &  ! Desc  
                       '(m s-1)/dt'               , &  ! Units 
'inc/wrf_bdyout.inc ext_write_field W_BTXE memorder XEZ' , & ! Debug message
1, (jde-1), kds, kde, 1, config_flags%spec_bdy_width, &
jms, jme, kds, kde, 1, config_flags%spec_bdy_width, &
jps, MIN( (jde-1), jpe ), kds, kde, 1, config_flags%spec_bdy_width, &
                       ierr )
CALL wrf_ext_write_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'W_BTYS'               , &  ! Data Name 
                       grid%w_btys(ims,kds,1)     , &  ! Field 
                       WRF_FLOAT          , &  ! FieldType 
                       grid%communicator , &  ! Comm
                       grid%iocommunicator , &  ! Comm
                       grid%domdesc      , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       dryrun             , &  ! flag
                       'YSZ'               , &  ! MemoryOrder
                       'Z'               , &  ! Stagger
                       'west_east'               , &  ! Dimname 1 
                       'bottom_top_stag'               , &  ! Dimname 2 
                       'bdy_width'               , &  ! Dimname 3 
                       'bdy tend z-wind component'               , &  ! Desc  
                       '(m s-1)/dt'               , &  ! Units 
'inc/wrf_bdyout.inc ext_write_field W_BTYS memorder YSZ' , & ! Debug message
1, (ide-1), kds, kde, 1, config_flags%spec_bdy_width, &
ims, ime, kds, kde, 1, config_flags%spec_bdy_width, &
ips, MIN( (ide-1), ipe ), kds, kde, 1, config_flags%spec_bdy_width, &
                       ierr )
CALL wrf_ext_write_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'W_BTYE'               , &  ! Data Name 
                       grid%w_btye(ims,kds,1)     , &  ! Field 
                       WRF_FLOAT          , &  ! FieldType 
                       grid%communicator , &  ! Comm
                       grid%iocommunicator , &  ! Comm
                       grid%domdesc      , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       dryrun             , &  ! flag
                       'YEZ'               , &  ! MemoryOrder
                       'Z'               , &  ! Stagger
                       'west_east'               , &  ! Dimname 1 
                       'bottom_top_stag'               , &  ! Dimname 2 
                       'bdy_width'               , &  ! Dimname 3 
                       'bdy tend z-wind component'               , &  ! Desc  
                       '(m s-1)/dt'               , &  ! Units 
'inc/wrf_bdyout.inc ext_write_field W_BTYE memorder YEZ' , & ! Debug message
1, (ide-1), kds, kde, 1, config_flags%spec_bdy_width, &
ims, ime, kds, kde, 1, config_flags%spec_bdy_width, &
ips, MIN( (ide-1), ipe ), kds, kde, 1, config_flags%spec_bdy_width, &
                       ierr )
CALL wrf_ext_write_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'PH_BXS'               , &  ! Data Name 
                       grid%ph_bxs(jms,kds,1)     , &  ! Field 
                       WRF_FLOAT          , &  ! FieldType 
                       grid%communicator , &  ! Comm
                       grid%iocommunicator , &  ! Comm
                       grid%domdesc      , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       dryrun             , &  ! flag
                       'XSZ'               , &  ! MemoryOrder
                       'Z'               , &  ! Stagger
                       'south_north'               , &  ! Dimname 1 
                       'bottom_top_stag'               , &  ! Dimname 2 
                       'bdy_width'               , &  ! Dimname 3 
                       'bdy perturbation geopotential'               , &  ! Desc  
                       'm2 s-2'               , &  ! Units 
'inc/wrf_bdyout.inc ext_write_field PH_BXS memorder XSZ' , & ! Debug message
1, (jde-1), kds, kde, 1, config_flags%spec_bdy_width, &
jms, jme, kds, kde, 1, config_flags%spec_bdy_width, &
jps, MIN( (jde-1), jpe ), kds, kde, 1, config_flags%spec_bdy_width, &
                       ierr )
CALL wrf_ext_write_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'PH_BXE'               , &  ! Data Name 
                       grid%ph_bxe(jms,kds,1)     , &  ! Field 
                       WRF_FLOAT          , &  ! FieldType 
                       grid%communicator , &  ! Comm
                       grid%iocommunicator , &  ! Comm
                       grid%domdesc      , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       dryrun             , &  ! flag
                       'XEZ'               , &  ! MemoryOrder
                       'Z'               , &  ! Stagger
                       'south_north'               , &  ! Dimname 1 
                       'bottom_top_stag'               , &  ! Dimname 2 
                       'bdy_width'               , &  ! Dimname 3 
                       'bdy perturbation geopotential'               , &  ! Desc  
                       'm2 s-2'               , &  ! Units 
'inc/wrf_bdyout.inc ext_write_field PH_BXE memorder XEZ' , & ! Debug message
1, (jde-1), kds, kde, 1, config_flags%spec_bdy_width, &
jms, jme, kds, kde, 1, config_flags%spec_bdy_width, &
jps, MIN( (jde-1), jpe ), kds, kde, 1, config_flags%spec_bdy_width, &
                       ierr )
CALL wrf_ext_write_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'PH_BYS'               , &  ! Data Name 
                       grid%ph_bys(ims,kds,1)     , &  ! Field 
                       WRF_FLOAT          , &  ! FieldType 
                       grid%communicator , &  ! Comm
                       grid%iocommunicator , &  ! Comm
                       grid%domdesc      , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       dryrun             , &  ! flag
                       'YSZ'               , &  ! MemoryOrder
                       'Z'               , &  ! Stagger
                       'west_east'               , &  ! Dimname 1 
                       'bottom_top_stag'               , &  ! Dimname 2 
                       'bdy_width'               , &  ! Dimname 3 
                       'bdy perturbation geopotential'               , &  ! Desc  
                       'm2 s-2'               , &  ! Units 
'inc/wrf_bdyout.inc ext_write_field PH_BYS memorder YSZ' , & ! Debug message
1, (ide-1), kds, kde, 1, config_flags%spec_bdy_width, &
ims, ime, kds, kde, 1, config_flags%spec_bdy_width, &
ips, MIN( (ide-1), ipe ), kds, kde, 1, config_flags%spec_bdy_width, &
                       ierr )
CALL wrf_ext_write_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'PH_BYE'               , &  ! Data Name 
                       grid%ph_bye(ims,kds,1)     , &  ! Field 
                       WRF_FLOAT          , &  ! FieldType 
                       grid%communicator , &  ! Comm
                       grid%iocommunicator , &  ! Comm
                       grid%domdesc      , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       dryrun             , &  ! flag
                       'YEZ'               , &  ! MemoryOrder
                       'Z'               , &  ! Stagger
                       'west_east'               , &  ! Dimname 1 
                       'bottom_top_stag'               , &  ! Dimname 2 
                       'bdy_width'               , &  ! Dimname 3 
                       'bdy perturbation geopotential'               , &  ! Desc  
                       'm2 s-2'               , &  ! Units 
'inc/wrf_bdyout.inc ext_write_field PH_BYE memorder YEZ' , & ! Debug message
1, (ide-1), kds, kde, 1, config_flags%spec_bdy_width, &
ims, ime, kds, kde, 1, config_flags%spec_bdy_width, &
ips, MIN( (ide-1), ipe ), kds, kde, 1, config_flags%spec_bdy_width, &
                       ierr )
CALL wrf_ext_write_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'PH_BTXS'               , &  ! Data Name 
                       grid%ph_btxs(jms,kds,1)     , &  ! Field 
                       WRF_FLOAT          , &  ! FieldType 
                       grid%communicator , &  ! Comm
                       grid%iocommunicator , &  ! Comm
                       grid%domdesc      , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       dryrun             , &  ! flag
                       'XSZ'               , &  ! MemoryOrder
                       'Z'               , &  ! Stagger
                       'south_north'               , &  ! Dimname 1 
                       'bottom_top_stag'               , &  ! Dimname 2 
                       'bdy_width'               , &  ! Dimname 3 
                       'bdy tend perturbation geopotential'               , &  ! Desc  
                       '(m2 s-2)/dt'               , &  ! Units 
'inc/wrf_bdyout.inc ext_write_field PH_BTXS memorder XSZ' , & ! Debug message
1, (jde-1), kds, kde, 1, config_flags%spec_bdy_width, &
jms, jme, kds, kde, 1, config_flags%spec_bdy_width, &
jps, MIN( (jde-1), jpe ), kds, kde, 1, config_flags%spec_bdy_width, &
                       ierr )
CALL wrf_ext_write_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'PH_BTXE'               , &  ! Data Name 
                       grid%ph_btxe(jms,kds,1)     , &  ! Field 
                       WRF_FLOAT          , &  ! FieldType 
                       grid%communicator , &  ! Comm
                       grid%iocommunicator , &  ! Comm
                       grid%domdesc      , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       dryrun             , &  ! flag
                       'XEZ'               , &  ! MemoryOrder
                       'Z'               , &  ! Stagger
                       'south_north'               , &  ! Dimname 1 
                       'bottom_top_stag'               , &  ! Dimname 2 
                       'bdy_width'               , &  ! Dimname 3 
                       'bdy tend perturbation geopotential'               , &  ! Desc  
                       '(m2 s-2)/dt'               , &  ! Units 
'inc/wrf_bdyout.inc ext_write_field PH_BTXE memorder XEZ' , & ! Debug message
1, (jde-1), kds, kde, 1, config_flags%spec_bdy_width, &
jms, jme, kds, kde, 1, config_flags%spec_bdy_width, &
jps, MIN( (jde-1), jpe ), kds, kde, 1, config_flags%spec_bdy_width, &
                       ierr )
CALL wrf_ext_write_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'PH_BTYS'               , &  ! Data Name 
                       grid%ph_btys(ims,kds,1)     , &  ! Field 
                       WRF_FLOAT          , &  ! FieldType 
                       grid%communicator , &  ! Comm
                       grid%iocommunicator , &  ! Comm
                       grid%domdesc      , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       dryrun             , &  ! flag
                       'YSZ'               , &  ! MemoryOrder
                       'Z'               , &  ! Stagger
                       'west_east'               , &  ! Dimname 1 
                       'bottom_top_stag'               , &  ! Dimname 2 
                       'bdy_width'               , &  ! Dimname 3 
                       'bdy tend perturbation geopotential'               , &  ! Desc  
                       '(m2 s-2)/dt'               , &  ! Units 
'inc/wrf_bdyout.inc ext_write_field PH_BTYS memorder YSZ' , & ! Debug message
1, (ide-1), kds, kde, 1, config_flags%spec_bdy_width, &
ims, ime, kds, kde, 1, config_flags%spec_bdy_width, &
ips, MIN( (ide-1), ipe ), kds, kde, 1, config_flags%spec_bdy_width, &
                       ierr )
CALL wrf_ext_write_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'PH_BTYE'               , &  ! Data Name 
                       grid%ph_btye(ims,kds,1)     , &  ! Field 
                       WRF_FLOAT          , &  ! FieldType 
                       grid%communicator , &  ! Comm
                       grid%iocommunicator , &  ! Comm
                       grid%domdesc      , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       dryrun             , &  ! flag
                       'YEZ'               , &  ! MemoryOrder
                       'Z'               , &  ! Stagger
                       'west_east'               , &  ! Dimname 1 
                       'bottom_top_stag'               , &  ! Dimname 2 
                       'bdy_width'               , &  ! Dimname 3 
                       'bdy tend perturbation geopotential'               , &  ! Desc  
                       '(m2 s-2)/dt'               , &  ! Units 
'inc/wrf_bdyout.inc ext_write_field PH_BTYE memorder YEZ' , & ! Debug message
1, (ide-1), kds, kde, 1, config_flags%spec_bdy_width, &
ims, ime, kds, kde, 1, config_flags%spec_bdy_width, &
ips, MIN( (ide-1), ipe ), kds, kde, 1, config_flags%spec_bdy_width, &
                       ierr )
CALL wrf_ext_write_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'T_BXS'               , &  ! Data Name 
                       grid%t_bxs(jms,kds,1)     , &  ! Field 
                       WRF_FLOAT          , &  ! FieldType 
                       grid%communicator , &  ! Comm
                       grid%iocommunicator , &  ! Comm
                       grid%domdesc      , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       dryrun             , &  ! flag
                       'XSZ'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
                       'south_north'               , &  ! Dimname 1 
                       'bottom_top'               , &  ! Dimname 2 
                       'bdy_width'               , &  ! Dimname 3 
                       'bdy perturbation potential temperature (theta-t0)'               , &  ! Desc  
                       'K'               , &  ! Units 
'inc/wrf_bdyout.inc ext_write_field T_BXS memorder XSZ' , & ! Debug message
1, (jde-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
jms, jme, kds, kde, 1, config_flags%spec_bdy_width, &
jps, MIN( (jde-1), jpe ), kds, (kde-1), 1, config_flags%spec_bdy_width, &
                       ierr )
CALL wrf_ext_write_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'T_BXE'               , &  ! Data Name 
                       grid%t_bxe(jms,kds,1)     , &  ! Field 
                       WRF_FLOAT          , &  ! FieldType 
                       grid%communicator , &  ! Comm
                       grid%iocommunicator , &  ! Comm
                       grid%domdesc      , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       dryrun             , &  ! flag
                       'XEZ'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
                       'south_north'               , &  ! Dimname 1 
                       'bottom_top'               , &  ! Dimname 2 
                       'bdy_width'               , &  ! Dimname 3 
                       'bdy perturbation potential temperature (theta-t0)'               , &  ! Desc  
                       'K'               , &  ! Units 
'inc/wrf_bdyout.inc ext_write_field T_BXE memorder XEZ' , & ! Debug message
1, (jde-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
jms, jme, kds, kde, 1, config_flags%spec_bdy_width, &
jps, MIN( (jde-1), jpe ), kds, (kde-1), 1, config_flags%spec_bdy_width, &
                       ierr )
CALL wrf_ext_write_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'T_BYS'               , &  ! Data Name 
                       grid%t_bys(ims,kds,1)     , &  ! Field 
                       WRF_FLOAT          , &  ! FieldType 
                       grid%communicator , &  ! Comm
                       grid%iocommunicator , &  ! Comm
                       grid%domdesc      , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       dryrun             , &  ! flag
                       'YSZ'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
                       'west_east'               , &  ! Dimname 1 
                       'bottom_top'               , &  ! Dimname 2 
                       'bdy_width'               , &  ! Dimname 3 
                       'bdy perturbation potential temperature (theta-t0)'               , &  ! Desc  
                       'K'               , &  ! Units 
'inc/wrf_bdyout.inc ext_write_field T_BYS memorder YSZ' , & ! Debug message
1, (ide-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
ims, ime, kds, kde, 1, config_flags%spec_bdy_width, &
ips, MIN( (ide-1), ipe ), kds, (kde-1), 1, config_flags%spec_bdy_width, &
                       ierr )
CALL wrf_ext_write_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'T_BYE'               , &  ! Data Name 
                       grid%t_bye(ims,kds,1)     , &  ! Field 
                       WRF_FLOAT          , &  ! FieldType 
                       grid%communicator , &  ! Comm
                       grid%iocommunicator , &  ! Comm
                       grid%domdesc      , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       dryrun             , &  ! flag
                       'YEZ'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
                       'west_east'               , &  ! Dimname 1 
                       'bottom_top'               , &  ! Dimname 2 
                       'bdy_width'               , &  ! Dimname 3 
                       'bdy perturbation potential temperature (theta-t0)'               , &  ! Desc  
                       'K'               , &  ! Units 
'inc/wrf_bdyout.inc ext_write_field T_BYE memorder YEZ' , & ! Debug message
1, (ide-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
ims, ime, kds, kde, 1, config_flags%spec_bdy_width, &
ips, MIN( (ide-1), ipe ), kds, (kde-1), 1, config_flags%spec_bdy_width, &
                       ierr )
CALL wrf_ext_write_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'T_BTXS'               , &  ! Data Name 
                       grid%t_btxs(jms,kds,1)     , &  ! Field 
                       WRF_FLOAT          , &  ! FieldType 
                       grid%communicator , &  ! Comm
                       grid%iocommunicator , &  ! Comm
                       grid%domdesc      , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       dryrun             , &  ! flag
                       'XSZ'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
                       'south_north'               , &  ! Dimname 1 
                       'bottom_top'               , &  ! Dimname 2 
                       'bdy_width'               , &  ! Dimname 3 
                       'bdy tend perturbation potential temperature (theta-t0)'               , &  ! Desc  
                       '(K)/dt'               , &  ! Units 
'inc/wrf_bdyout.inc ext_write_field T_BTXS memorder XSZ' , & ! Debug message
1, (jde-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
jms, jme, kds, kde, 1, config_flags%spec_bdy_width, &
jps, MIN( (jde-1), jpe ), kds, (kde-1), 1, config_flags%spec_bdy_width, &
                       ierr )
CALL wrf_ext_write_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'T_BTXE'               , &  ! Data Name 
                       grid%t_btxe(jms,kds,1)     , &  ! Field 
                       WRF_FLOAT          , &  ! FieldType 
                       grid%communicator , &  ! Comm
                       grid%iocommunicator , &  ! Comm
                       grid%domdesc      , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       dryrun             , &  ! flag
                       'XEZ'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
                       'south_north'               , &  ! Dimname 1 
                       'bottom_top'               , &  ! Dimname 2 
                       'bdy_width'               , &  ! Dimname 3 
                       'bdy tend perturbation potential temperature (theta-t0)'               , &  ! Desc  
                       '(K)/dt'               , &  ! Units 
'inc/wrf_bdyout.inc ext_write_field T_BTXE memorder XEZ' , & ! Debug message
1, (jde-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
jms, jme, kds, kde, 1, config_flags%spec_bdy_width, &
jps, MIN( (jde-1), jpe ), kds, (kde-1), 1, config_flags%spec_bdy_width, &
                       ierr )
CALL wrf_ext_write_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'T_BTYS'               , &  ! Data Name 
                       grid%t_btys(ims,kds,1)     , &  ! Field 
                       WRF_FLOAT          , &  ! FieldType 
                       grid%communicator , &  ! Comm
                       grid%iocommunicator , &  ! Comm
                       grid%domdesc      , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       dryrun             , &  ! flag
                       'YSZ'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
                       'west_east'               , &  ! Dimname 1 
                       'bottom_top'               , &  ! Dimname 2 
                       'bdy_width'               , &  ! Dimname 3 
                       'bdy tend perturbation potential temperature (theta-t0)'               , &  ! Desc  
                       '(K)/dt'               , &  ! Units 
'inc/wrf_bdyout.inc ext_write_field T_BTYS memorder YSZ' , & ! Debug message
1, (ide-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
ims, ime, kds, kde, 1, config_flags%spec_bdy_width, &
ips, MIN( (ide-1), ipe ), kds, (kde-1), 1, config_flags%spec_bdy_width, &
                       ierr )
CALL wrf_ext_write_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'T_BTYE'               , &  ! Data Name 
                       grid%t_btye(ims,kds,1)     , &  ! Field 
                       WRF_FLOAT          , &  ! FieldType 
                       grid%communicator , &  ! Comm
                       grid%iocommunicator , &  ! Comm
                       grid%domdesc      , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       dryrun             , &  ! flag
                       'YEZ'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
                       'west_east'               , &  ! Dimname 1 
                       'bottom_top'               , &  ! Dimname 2 
                       'bdy_width'               , &  ! Dimname 3 
                       'bdy tend perturbation potential temperature (theta-t0)'               , &  ! Desc  
                       '(K)/dt'               , &  ! Units 
'inc/wrf_bdyout.inc ext_write_field T_BTYE memorder YEZ' , & ! Debug message
1, (ide-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
ims, ime, kds, kde, 1, config_flags%spec_bdy_width, &
ips, MIN( (ide-1), ipe ), kds, (kde-1), 1, config_flags%spec_bdy_width, &
                       ierr )
CALL wrf_ext_write_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'MU_BXS'               , &  ! Data Name 
                       grid%mu_bxs(jms,kds,1)     , &  ! Field 
                       WRF_FLOAT          , &  ! FieldType 
                       grid%communicator , &  ! Comm
                       grid%iocommunicator , &  ! Comm
                       grid%domdesc      , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       dryrun             , &  ! flag
                       'XS'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
                       'south_north'               , &  ! Dimname 1 
                       'bdy_width'               , &  ! Dimname 2 
                       'one_element'               , &  ! Dimname 3 
                       'bdy perturbation dry air mass in column'               , &  ! Desc  
                       'Pa'               , &  ! Units 
'inc/wrf_bdyout.inc ext_write_field MU_BXS memorder XS' , & ! Debug message
1, (jde-1), 1, config_flags%spec_bdy_width, 1, 1, &
jms, jme, 1, config_flags%spec_bdy_width, 1, 1, &
jps, MIN( (jde-1), jpe ), 1, config_flags%spec_bdy_width, 1, 1, &
                       ierr )
CALL wrf_ext_write_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'MU_BXE'               , &  ! Data Name 
                       grid%mu_bxe(jms,kds,1)     , &  ! Field 
                       WRF_FLOAT          , &  ! FieldType 
                       grid%communicator , &  ! Comm
                       grid%iocommunicator , &  ! Comm
                       grid%domdesc      , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       dryrun             , &  ! flag
                       'XE'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
                       'south_north'               , &  ! Dimname 1 
                       'bdy_width'               , &  ! Dimname 2 
                       'one_element'               , &  ! Dimname 3 
                       'bdy perturbation dry air mass in column'               , &  ! Desc  
                       'Pa'               , &  ! Units 
'inc/wrf_bdyout.inc ext_write_field MU_BXE memorder XE' , & ! Debug message
1, (jde-1), 1, config_flags%spec_bdy_width, 1, 1, &
jms, jme, 1, config_flags%spec_bdy_width, 1, 1, &
jps, MIN( (jde-1), jpe ), 1, config_flags%spec_bdy_width, 1, 1, &
                       ierr )
CALL wrf_ext_write_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'MU_BYS'               , &  ! Data Name 
                       grid%mu_bys(ims,kds,1)     , &  ! Field 
                       WRF_FLOAT          , &  ! FieldType 
                       grid%communicator , &  ! Comm
                       grid%iocommunicator , &  ! Comm
                       grid%domdesc      , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       dryrun             , &  ! flag
                       'YS'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
                       'west_east'               , &  ! Dimname 1 
                       'bdy_width'               , &  ! Dimname 2 
                       'one_element'               , &  ! Dimname 3 
                       'bdy perturbation dry air mass in column'               , &  ! Desc  
                       'Pa'               , &  ! Units 
'inc/wrf_bdyout.inc ext_write_field MU_BYS memorder YS' , & ! Debug message
1, (ide-1), 1, config_flags%spec_bdy_width, 1, 1, &
ims, ime, 1, config_flags%spec_bdy_width, 1, 1, &
ips, MIN( (ide-1), ipe ), 1, config_flags%spec_bdy_width, 1, 1, &
                       ierr )
CALL wrf_ext_write_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'MU_BYE'               , &  ! Data Name 
                       grid%mu_bye(ims,kds,1)     , &  ! Field 
                       WRF_FLOAT          , &  ! FieldType 
                       grid%communicator , &  ! Comm
                       grid%iocommunicator , &  ! Comm
                       grid%domdesc      , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       dryrun             , &  ! flag
                       'YE'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
                       'west_east'               , &  ! Dimname 1 
                       'bdy_width'               , &  ! Dimname 2 
                       'one_element'               , &  ! Dimname 3 
                       'bdy perturbation dry air mass in column'               , &  ! Desc  
                       'Pa'               , &  ! Units 
'inc/wrf_bdyout.inc ext_write_field MU_BYE memorder YE' , & ! Debug message
1, (ide-1), 1, config_flags%spec_bdy_width, 1, 1, &
ims, ime, 1, config_flags%spec_bdy_width, 1, 1, &
ips, MIN( (ide-1), ipe ), 1, config_flags%spec_bdy_width, 1, 1, &
                       ierr )
CALL wrf_ext_write_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'MU_BTXS'               , &  ! Data Name 
                       grid%mu_btxs(jms,kds,1)     , &  ! Field 
                       WRF_FLOAT          , &  ! FieldType 
                       grid%communicator , &  ! Comm
                       grid%iocommunicator , &  ! Comm
                       grid%domdesc      , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       dryrun             , &  ! flag
                       'XS'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
                       'south_north'               , &  ! Dimname 1 
                       'bdy_width'               , &  ! Dimname 2 
                       'one_element'               , &  ! Dimname 3 
                       'bdy tend perturbation dry air mass in column'               , &  ! Desc  
                       '(Pa)/dt'               , &  ! Units 
'inc/wrf_bdyout.inc ext_write_field MU_BTXS memorder XS' , & ! Debug message
1, (jde-1), 1, config_flags%spec_bdy_width, 1, 1, &
jms, jme, 1, config_flags%spec_bdy_width, 1, 1, &
jps, MIN( (jde-1), jpe ), 1, config_flags%spec_bdy_width, 1, 1, &
                       ierr )
CALL wrf_ext_write_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'MU_BTXE'               , &  ! Data Name 
                       grid%mu_btxe(jms,kds,1)     , &  ! Field 
                       WRF_FLOAT          , &  ! FieldType 
                       grid%communicator , &  ! Comm
                       grid%iocommunicator , &  ! Comm
                       grid%domdesc      , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       dryrun             , &  ! flag
                       'XE'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
                       'south_north'               , &  ! Dimname 1 
                       'bdy_width'               , &  ! Dimname 2 
                       'one_element'               , &  ! Dimname 3 
                       'bdy tend perturbation dry air mass in column'               , &  ! Desc  
                       '(Pa)/dt'               , &  ! Units 
'inc/wrf_bdyout.inc ext_write_field MU_BTXE memorder XE' , & ! Debug message
1, (jde-1), 1, config_flags%spec_bdy_width, 1, 1, &
jms, jme, 1, config_flags%spec_bdy_width, 1, 1, &
jps, MIN( (jde-1), jpe ), 1, config_flags%spec_bdy_width, 1, 1, &
                       ierr )
CALL wrf_ext_write_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'MU_BTYS'               , &  ! Data Name 
                       grid%mu_btys(ims,kds,1)     , &  ! Field 
                       WRF_FLOAT          , &  ! FieldType 
                       grid%communicator , &  ! Comm
                       grid%iocommunicator , &  ! Comm
                       grid%domdesc      , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       dryrun             , &  ! flag
                       'YS'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
                       'west_east'               , &  ! Dimname 1 
                       'bdy_width'               , &  ! Dimname 2 
                       'one_element'               , &  ! Dimname 3 
                       'bdy tend perturbation dry air mass in column'               , &  ! Desc  
                       '(Pa)/dt'               , &  ! Units 
'inc/wrf_bdyout.inc ext_write_field MU_BTYS memorder YS' , & ! Debug message
1, (ide-1), 1, config_flags%spec_bdy_width, 1, 1, &
ims, ime, 1, config_flags%spec_bdy_width, 1, 1, &
ips, MIN( (ide-1), ipe ), 1, config_flags%spec_bdy_width, 1, 1, &
                       ierr )
CALL wrf_ext_write_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'MU_BTYE'               , &  ! Data Name 
                       grid%mu_btye(ims,kds,1)     , &  ! Field 
                       WRF_FLOAT          , &  ! FieldType 
                       grid%communicator , &  ! Comm
                       grid%iocommunicator , &  ! Comm
                       grid%domdesc      , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       dryrun             , &  ! flag
                       'YE'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
                       'west_east'               , &  ! Dimname 1 
                       'bdy_width'               , &  ! Dimname 2 
                       'one_element'               , &  ! Dimname 3 
                       'bdy tend perturbation dry air mass in column'               , &  ! Desc  
                       '(Pa)/dt'               , &  ! Units 
'inc/wrf_bdyout.inc ext_write_field MU_BTYE memorder YE' , & ! Debug message
1, (ide-1), 1, config_flags%spec_bdy_width, 1, 1, &
ims, ime, 1, config_flags%spec_bdy_width, 1, 1, &
ips, MIN( (ide-1), ipe ), 1, config_flags%spec_bdy_width, 1, 1, &
                       ierr )
DO itrace = PARAM_FIRST_SCALAR , num_moist
  IF ( moist_boundary_table(grid%id, itrace ) ) THEN
    CALL wrf_ext_write_field (  &
          fid                             , &  ! DataHandle
          current_date(1:19)              , &  ! DateStr
          TRIM(moist_dname_table( grid%id, itrace )) // '_BXS', & !data name
          grid%moist_BXS(jms,kds,1,itrace)  , &  ! Field
                       WRF_FLOAT             , &  ! FieldType 
          grid%communicator  , &  ! Comm
          grid%iocommunicator  , &  ! Comm
          grid%domdesc       , &  ! Comm
          grid%bdy_mask       , &  ! bdy_mask
          dryrun             , &  ! flag
          'XSZ'               , &  ! MemoryOrder
          ''                , &  ! Stagger
                       'south_north'               , &  ! Dimname 1 
                       'bottom_top'               , &  ! Dimname 2 
                       'bdy_width'               , &  ! Dimname 3 
          moist_desc_table( grid%id, itrace  ), & ! Desc
          moist_units_table( grid%id, itrace  ), & ! Units
'inc/wrf_bdyout.inc ext_write_field '//TRIM(moist_dname_table( grid%id, itrace ))//' memorder XSZ' , & ! Debug message
1, (jde-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
jms, jme, kds, kde, 1, config_flags%spec_bdy_width, &
jps, MIN( (jde-1), jpe ), kds, (kde-1), 1, config_flags%spec_bdy_width, &
                         ierr )
    CALL wrf_ext_write_field (  &
          fid                             , &  ! DataHandle
          current_date(1:19)              , &  ! DateStr
          TRIM(moist_dname_table( grid%id, itrace )) // '_BXE', & !data name
          grid%moist_BXE(jms,kds,1,itrace)  , &  ! Field
                       WRF_FLOAT             , &  ! FieldType 
          grid%communicator  , &  ! Comm
          grid%iocommunicator  , &  ! Comm
          grid%domdesc       , &  ! Comm
          grid%bdy_mask       , &  ! bdy_mask
          dryrun             , &  ! flag
          'XEZ'               , &  ! MemoryOrder
          ''                , &  ! Stagger
                       'south_north'               , &  ! Dimname 1 
                       'bottom_top'               , &  ! Dimname 2 
                       'bdy_width'               , &  ! Dimname 3 
          moist_desc_table( grid%id, itrace  ), & ! Desc
          moist_units_table( grid%id, itrace  ), & ! Units
'inc/wrf_bdyout.inc ext_write_field '//TRIM(moist_dname_table( grid%id, itrace ))//' memorder XEZ' , & ! Debug message
1, (jde-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
jms, jme, kds, kde, 1, config_flags%spec_bdy_width, &
jps, MIN( (jde-1), jpe ), kds, (kde-1), 1, config_flags%spec_bdy_width, &
                         ierr )
    CALL wrf_ext_write_field (  &
          fid                             , &  ! DataHandle
          current_date(1:19)              , &  ! DateStr
          TRIM(moist_dname_table( grid%id, itrace )) // '_BYS', & !data name
          grid%moist_BYS(ims,kds,1,itrace)  , &  ! Field
                       WRF_FLOAT             , &  ! FieldType 
          grid%communicator  , &  ! Comm
          grid%iocommunicator  , &  ! Comm
          grid%domdesc       , &  ! Comm
          grid%bdy_mask       , &  ! bdy_mask
          dryrun             , &  ! flag
          'YSZ'               , &  ! MemoryOrder
          ''                , &  ! Stagger
                       'west_east'               , &  ! Dimname 1 
                       'bottom_top'               , &  ! Dimname 2 
                       'bdy_width'               , &  ! Dimname 3 
          moist_desc_table( grid%id, itrace  ), & ! Desc
          moist_units_table( grid%id, itrace  ), & ! Units
'inc/wrf_bdyout.inc ext_write_field '//TRIM(moist_dname_table( grid%id, itrace ))//' memorder YSZ' , & ! Debug message
1, (ide-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
ims, ime, kds, kde, 1, config_flags%spec_bdy_width, &
ips, MIN( (ide-1), ipe ), kds, (kde-1), 1, config_flags%spec_bdy_width, &
                         ierr )
    CALL wrf_ext_write_field (  &
          fid                             , &  ! DataHandle
          current_date(1:19)              , &  ! DateStr
          TRIM(moist_dname_table( grid%id, itrace )) // '_BYE', & !data name
          grid%moist_BYE(ims,kds,1,itrace)  , &  ! Field
                       WRF_FLOAT             , &  ! FieldType 
          grid%communicator  , &  ! Comm
          grid%iocommunicator  , &  ! Comm
          grid%domdesc       , &  ! Comm
          grid%bdy_mask       , &  ! bdy_mask
          dryrun             , &  ! flag
          'YEZ'               , &  ! MemoryOrder
          ''                , &  ! Stagger
                       'west_east'               , &  ! Dimname 1 
                       'bottom_top'               , &  ! Dimname 2 
                       'bdy_width'               , &  ! Dimname 3 
          moist_desc_table( grid%id, itrace  ), & ! Desc
          moist_units_table( grid%id, itrace  ), & ! Units
'inc/wrf_bdyout.inc ext_write_field '//TRIM(moist_dname_table( grid%id, itrace ))//' memorder YEZ' , & ! Debug message
1, (ide-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
ims, ime, kds, kde, 1, config_flags%spec_bdy_width, &
ips, MIN( (ide-1), ipe ), kds, (kde-1), 1, config_flags%spec_bdy_width, &
                         ierr )
  ENDIF
ENDDO
DO itrace = PARAM_FIRST_SCALAR , num_moist
  IF ( moist_boundary_table(grid%id, itrace ) ) THEN
    CALL wrf_ext_write_field (  &
          fid                             , &  ! DataHandle
          current_date(1:19)              , &  ! DateStr
          TRIM(moist_dname_table( grid%id, itrace )) // '_BTXS', & !data name
          grid%moist_BTXS(jms,kds,1,itrace)  , &  ! Field
                       WRF_FLOAT             , &  ! FieldType 
          grid%communicator  , &  ! Comm
          grid%iocommunicator  , &  ! Comm
          grid%domdesc       , &  ! Comm
          grid%bdy_mask       , &  ! bdy_mask
          dryrun             , &  ! flag
          'XSZ'               , &  ! MemoryOrder
          ''                , &  ! Stagger
                       'south_north'               , &  ! Dimname 1 
                       'bottom_top'               , &  ! Dimname 2 
                       'bdy_width'               , &  ! Dimname 3 
          moist_desc_table( grid%id, itrace  ), & ! Desc
          moist_units_table( grid%id, itrace  ), & ! Units
'inc/wrf_bdyout.inc ext_write_field '//TRIM(moist_dname_table( grid%id, itrace ))//' memorder XSZ' , & ! Debug message
1, (jde-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
jms, jme, kds, kde, 1, config_flags%spec_bdy_width, &
jps, MIN( (jde-1), jpe ), kds, (kde-1), 1, config_flags%spec_bdy_width, &
                         ierr )
    CALL wrf_ext_write_field (  &
          fid                             , &  ! DataHandle
          current_date(1:19)              , &  ! DateStr
          TRIM(moist_dname_table( grid%id, itrace )) // '_BTXE', & !data name
          grid%moist_BTXE(jms,kds,1,itrace)  , &  ! Field
                       WRF_FLOAT             , &  ! FieldType 
          grid%communicator  , &  ! Comm
          grid%iocommunicator  , &  ! Comm
          grid%domdesc       , &  ! Comm
          grid%bdy_mask       , &  ! bdy_mask
          dryrun             , &  ! flag
          'XEZ'               , &  ! MemoryOrder
          ''                , &  ! Stagger
                       'south_north'               , &  ! Dimname 1 
                       'bottom_top'               , &  ! Dimname 2 
                       'bdy_width'               , &  ! Dimname 3 
          moist_desc_table( grid%id, itrace  ), & ! Desc
          moist_units_table( grid%id, itrace  ), & ! Units
'inc/wrf_bdyout.inc ext_write_field '//TRIM(moist_dname_table( grid%id, itrace ))//' memorder XEZ' , & ! Debug message
1, (jde-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
jms, jme, kds, kde, 1, config_flags%spec_bdy_width, &
jps, MIN( (jde-1), jpe ), kds, (kde-1), 1, config_flags%spec_bdy_width, &
                         ierr )
    CALL wrf_ext_write_field (  &
          fid                             , &  ! DataHandle
          current_date(1:19)              , &  ! DateStr
          TRIM(moist_dname_table( grid%id, itrace )) // '_BTYS', & !data name
          grid%moist_BTYS(ims,kds,1,itrace)  , &  ! Field
                       WRF_FLOAT             , &  ! FieldType 
          grid%communicator  , &  ! Comm
          grid%iocommunicator  , &  ! Comm
          grid%domdesc       , &  ! Comm
          grid%bdy_mask       , &  ! bdy_mask
          dryrun             , &  ! flag
          'YSZ'               , &  ! MemoryOrder
          ''                , &  ! Stagger
                       'west_east'               , &  ! Dimname 1 
                       'bottom_top'               , &  ! Dimname 2 
                       'bdy_width'               , &  ! Dimname 3 
          moist_desc_table( grid%id, itrace  ), & ! Desc
          moist_units_table( grid%id, itrace  ), & ! Units
'inc/wrf_bdyout.inc ext_write_field '//TRIM(moist_dname_table( grid%id, itrace ))//' memorder YSZ' , & ! Debug message
1, (ide-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
ims, ime, kds, kde, 1, config_flags%spec_bdy_width, &
ips, MIN( (ide-1), ipe ), kds, (kde-1), 1, config_flags%spec_bdy_width, &
                         ierr )
    CALL wrf_ext_write_field (  &
          fid                             , &  ! DataHandle
          current_date(1:19)              , &  ! DateStr
          TRIM(moist_dname_table( grid%id, itrace )) // '_BTYE', & !data name
          grid%moist_BTYE(ims,kds,1,itrace)  , &  ! Field
                       WRF_FLOAT             , &  ! FieldType 
          grid%communicator  , &  ! Comm
          grid%iocommunicator  , &  ! Comm
          grid%domdesc       , &  ! Comm
          grid%bdy_mask       , &  ! bdy_mask
          dryrun             , &  ! flag
          'YEZ'               , &  ! MemoryOrder
          ''                , &  ! Stagger
                       'west_east'               , &  ! Dimname 1 
                       'bottom_top'               , &  ! Dimname 2 
                       'bdy_width'               , &  ! Dimname 3 
          moist_desc_table( grid%id, itrace  ), & ! Desc
          moist_units_table( grid%id, itrace  ), & ! Units
'inc/wrf_bdyout.inc ext_write_field '//TRIM(moist_dname_table( grid%id, itrace ))//' memorder YEZ' , & ! Debug message
1, (ide-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
ims, ime, kds, kde, 1, config_flags%spec_bdy_width, &
ips, MIN( (ide-1), ipe ), kds, (kde-1), 1, config_flags%spec_bdy_width, &
                         ierr )
  ENDIF
ENDDO
DO itrace = PARAM_FIRST_SCALAR , num_dfi_moist
  IF ( dfi_moist_boundary_table(grid%id, itrace ) ) THEN
    CALL wrf_ext_write_field (  &
          fid                             , &  ! DataHandle
          current_date(1:19)              , &  ! DateStr
          TRIM(dfi_moist_dname_table( grid%id, itrace )) // '_BXS', & !data name
          grid%dfi_moist_BXS(jms,kds,1,itrace)  , &  ! Field
                       WRF_FLOAT             , &  ! FieldType 
          grid%communicator  , &  ! Comm
          grid%iocommunicator  , &  ! Comm
          grid%domdesc       , &  ! Comm
          grid%bdy_mask       , &  ! bdy_mask
          dryrun             , &  ! flag
          'XSZ'               , &  ! MemoryOrder
          ''                , &  ! Stagger
                       'south_north'               , &  ! Dimname 1 
                       'bottom_top'               , &  ! Dimname 2 
                       'bdy_width'               , &  ! Dimname 3 
          dfi_moist_desc_table( grid%id, itrace  ), & ! Desc
          dfi_moist_units_table( grid%id, itrace  ), & ! Units
'inc/wrf_bdyout.inc ext_write_field '//TRIM(dfi_moist_dname_table( grid%id, itrace ))//' memorder XSZ' , & ! Debug message
1, (jde-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
jms, jme, kds, kde, 1, config_flags%spec_bdy_width, &
jps, MIN( (jde-1), jpe ), kds, (kde-1), 1, config_flags%spec_bdy_width, &
                         ierr )
    CALL wrf_ext_write_field (  &
          fid                             , &  ! DataHandle
          current_date(1:19)              , &  ! DateStr
          TRIM(dfi_moist_dname_table( grid%id, itrace )) // '_BXE', & !data name
          grid%dfi_moist_BXE(jms,kds,1,itrace)  , &  ! Field
                       WRF_FLOAT             , &  ! FieldType 
          grid%communicator  , &  ! Comm
          grid%iocommunicator  , &  ! Comm
          grid%domdesc       , &  ! Comm
          grid%bdy_mask       , &  ! bdy_mask
          dryrun             , &  ! flag
          'XEZ'               , &  ! MemoryOrder
          ''                , &  ! Stagger
                       'south_north'               , &  ! Dimname 1 
                       'bottom_top'               , &  ! Dimname 2 
                       'bdy_width'               , &  ! Dimname 3 
          dfi_moist_desc_table( grid%id, itrace  ), & ! Desc
          dfi_moist_units_table( grid%id, itrace  ), & ! Units
'inc/wrf_bdyout.inc ext_write_field '//TRIM(dfi_moist_dname_table( grid%id, itrace ))//' memorder XEZ' , & ! Debug message
1, (jde-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
jms, jme, kds, kde, 1, config_flags%spec_bdy_width, &
jps, MIN( (jde-1), jpe ), kds, (kde-1), 1, config_flags%spec_bdy_width, &
                         ierr )
    CALL wrf_ext_write_field (  &
          fid                             , &  ! DataHandle
          current_date(1:19)              , &  ! DateStr
          TRIM(dfi_moist_dname_table( grid%id, itrace )) // '_BYS', & !data name
          grid%dfi_moist_BYS(ims,kds,1,itrace)  , &  ! Field
                       WRF_FLOAT             , &  ! FieldType 
          grid%communicator  , &  ! Comm
          grid%iocommunicator  , &  ! Comm
          grid%domdesc       , &  ! Comm
          grid%bdy_mask       , &  ! bdy_mask
          dryrun             , &  ! flag
          'YSZ'               , &  ! MemoryOrder
          ''                , &  ! Stagger
                       'west_east'               , &  ! Dimname 1 
                       'bottom_top'               , &  ! Dimname 2 
                       'bdy_width'               , &  ! Dimname 3 
          dfi_moist_desc_table( grid%id, itrace  ), & ! Desc
          dfi_moist_units_table( grid%id, itrace  ), & ! Units
'inc/wrf_bdyout.inc ext_write_field '//TRIM(dfi_moist_dname_table( grid%id, itrace ))//' memorder YSZ' , & ! Debug message
1, (ide-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
ims, ime, kds, kde, 1, config_flags%spec_bdy_width, &
ips, MIN( (ide-1), ipe ), kds, (kde-1), 1, config_flags%spec_bdy_width, &
                         ierr )
    CALL wrf_ext_write_field (  &
          fid                             , &  ! DataHandle
          current_date(1:19)              , &  ! DateStr
          TRIM(dfi_moist_dname_table( grid%id, itrace )) // '_BYE', & !data name
          grid%dfi_moist_BYE(ims,kds,1,itrace)  , &  ! Field
                       WRF_FLOAT             , &  ! FieldType 
          grid%communicator  , &  ! Comm
          grid%iocommunicator  , &  ! Comm
          grid%domdesc       , &  ! Comm
          grid%bdy_mask       , &  ! bdy_mask
          dryrun             , &  ! flag
          'YEZ'               , &  ! MemoryOrder
          ''                , &  ! Stagger
                       'west_east'               , &  ! Dimname 1 
                       'bottom_top'               , &  ! Dimname 2 
                       'bdy_width'               , &  ! Dimname 3 
          dfi_moist_desc_table( grid%id, itrace  ), & ! Desc
          dfi_moist_units_table( grid%id, itrace  ), & ! Units
'inc/wrf_bdyout.inc ext_write_field '//TRIM(dfi_moist_dname_table( grid%id, itrace ))//' memorder YEZ' , & ! Debug message
1, (ide-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
ims, ime, kds, kde, 1, config_flags%spec_bdy_width, &
ips, MIN( (ide-1), ipe ), kds, (kde-1), 1, config_flags%spec_bdy_width, &
                         ierr )
  ENDIF
ENDDO
DO itrace = PARAM_FIRST_SCALAR , num_dfi_moist
  IF ( dfi_moist_boundary_table(grid%id, itrace ) ) THEN
    CALL wrf_ext_write_field (  &
          fid                             , &  ! DataHandle
          current_date(1:19)              , &  ! DateStr
          TRIM(dfi_moist_dname_table( grid%id, itrace )) // '_BTXS', & !data name
          grid%dfi_moist_BTXS(jms,kds,1,itrace)  , &  ! Field
                       WRF_FLOAT             , &  ! FieldType 
          grid%communicator  , &  ! Comm
          grid%iocommunicator  , &  ! Comm
          grid%domdesc       , &  ! Comm
          grid%bdy_mask       , &  ! bdy_mask
          dryrun             , &  ! flag
          'XSZ'               , &  ! MemoryOrder
          ''                , &  ! Stagger
                       'south_north'               , &  ! Dimname 1 
                       'bottom_top'               , &  ! Dimname 2 
                       'bdy_width'               , &  ! Dimname 3 
          dfi_moist_desc_table( grid%id, itrace  ), & ! Desc
          dfi_moist_units_table( grid%id, itrace  ), & ! Units
'inc/wrf_bdyout.inc ext_write_field '//TRIM(dfi_moist_dname_table( grid%id, itrace ))//' memorder XSZ' , & ! Debug message
1, (jde-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
jms, jme, kds, kde, 1, config_flags%spec_bdy_width, &
jps, MIN( (jde-1), jpe ), kds, (kde-1), 1, config_flags%spec_bdy_width, &
                         ierr )
    CALL wrf_ext_write_field (  &
          fid                             , &  ! DataHandle
          current_date(1:19)              , &  ! DateStr
          TRIM(dfi_moist_dname_table( grid%id, itrace )) // '_BTXE', & !data name
          grid%dfi_moist_BTXE(jms,kds,1,itrace)  , &  ! Field
                       WRF_FLOAT             , &  ! FieldType 
          grid%communicator  , &  ! Comm
          grid%iocommunicator  , &  ! Comm
          grid%domdesc       , &  ! Comm
          grid%bdy_mask       , &  ! bdy_mask
          dryrun             , &  ! flag
          'XEZ'               , &  ! MemoryOrder
          ''                , &  ! Stagger
                       'south_north'               , &  ! Dimname 1 
                       'bottom_top'               , &  ! Dimname 2 
                       'bdy_width'               , &  ! Dimname 3 
          dfi_moist_desc_table( grid%id, itrace  ), & ! Desc
          dfi_moist_units_table( grid%id, itrace  ), & ! Units
'inc/wrf_bdyout.inc ext_write_field '//TRIM(dfi_moist_dname_table( grid%id, itrace ))//' memorder XEZ' , & ! Debug message
1, (jde-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
jms, jme, kds, kde, 1, config_flags%spec_bdy_width, &
jps, MIN( (jde-1), jpe ), kds, (kde-1), 1, config_flags%spec_bdy_width, &
                         ierr )
    CALL wrf_ext_write_field (  &
          fid                             , &  ! DataHandle
          current_date(1:19)              , &  ! DateStr
          TRIM(dfi_moist_dname_table( grid%id, itrace )) // '_BTYS', & !data name
          grid%dfi_moist_BTYS(ims,kds,1,itrace)  , &  ! Field
                       WRF_FLOAT             , &  ! FieldType 
          grid%communicator  , &  ! Comm
          grid%iocommunicator  , &  ! Comm
          grid%domdesc       , &  ! Comm
          grid%bdy_mask       , &  ! bdy_mask
          dryrun             , &  ! flag
          'YSZ'               , &  ! MemoryOrder
          ''                , &  ! Stagger
                       'west_east'               , &  ! Dimname 1 
                       'bottom_top'               , &  ! Dimname 2 
                       'bdy_width'               , &  ! Dimname 3 
          dfi_moist_desc_table( grid%id, itrace  ), & ! Desc
          dfi_moist_units_table( grid%id, itrace  ), & ! Units
'inc/wrf_bdyout.inc ext_write_field '//TRIM(dfi_moist_dname_table( grid%id, itrace ))//' memorder YSZ' , & ! Debug message
1, (ide-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
ims, ime, kds, kde, 1, config_flags%spec_bdy_width, &
ips, MIN( (ide-1), ipe ), kds, (kde-1), 1, config_flags%spec_bdy_width, &
                         ierr )
    CALL wrf_ext_write_field (  &
          fid                             , &  ! DataHandle
          current_date(1:19)              , &  ! DateStr
          TRIM(dfi_moist_dname_table( grid%id, itrace )) // '_BTYE', & !data name
          grid%dfi_moist_BTYE(ims,kds,1,itrace)  , &  ! Field
                       WRF_FLOAT             , &  ! FieldType 
          grid%communicator  , &  ! Comm
          grid%iocommunicator  , &  ! Comm
          grid%domdesc       , &  ! Comm
          grid%bdy_mask       , &  ! bdy_mask
          dryrun             , &  ! flag
          'YEZ'               , &  ! MemoryOrder
          ''                , &  ! Stagger
                       'west_east'               , &  ! Dimname 1 
                       'bottom_top'               , &  ! Dimname 2 
                       'bdy_width'               , &  ! Dimname 3 
          dfi_moist_desc_table( grid%id, itrace  ), & ! Desc
          dfi_moist_units_table( grid%id, itrace  ), & ! Units
'inc/wrf_bdyout.inc ext_write_field '//TRIM(dfi_moist_dname_table( grid%id, itrace ))//' memorder YEZ' , & ! Debug message
1, (ide-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
ims, ime, kds, kde, 1, config_flags%spec_bdy_width, &
ips, MIN( (ide-1), ipe ), kds, (kde-1), 1, config_flags%spec_bdy_width, &
                         ierr )
  ENDIF
ENDDO
DO itrace = PARAM_FIRST_SCALAR , num_scalar
  IF ( scalar_boundary_table(grid%id, itrace ) ) THEN
    CALL wrf_ext_write_field (  &
          fid                             , &  ! DataHandle
          current_date(1:19)              , &  ! DateStr
          TRIM(scalar_dname_table( grid%id, itrace )) // '_BXS', & !data name
          grid%scalar_BXS(jms,kds,1,itrace)  , &  ! Field
                       WRF_FLOAT             , &  ! FieldType 
          grid%communicator  , &  ! Comm
          grid%iocommunicator  , &  ! Comm
          grid%domdesc       , &  ! Comm
          grid%bdy_mask       , &  ! bdy_mask
          dryrun             , &  ! flag
          'XSZ'               , &  ! MemoryOrder
          ''                , &  ! Stagger
                       'south_north'               , &  ! Dimname 1 
                       'bottom_top'               , &  ! Dimname 2 
                       'bdy_width'               , &  ! Dimname 3 
          scalar_desc_table( grid%id, itrace  ), & ! Desc
          scalar_units_table( grid%id, itrace  ), & ! Units
'inc/wrf_bdyout.inc ext_write_field '//TRIM(scalar_dname_table( grid%id, itrace ))//' memorder XSZ' , & ! Debug message
1, (jde-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
jms, jme, kds, kde, 1, config_flags%spec_bdy_width, &
jps, MIN( (jde-1), jpe ), kds, (kde-1), 1, config_flags%spec_bdy_width, &
                         ierr )
    CALL wrf_ext_write_field (  &
          fid                             , &  ! DataHandle
          current_date(1:19)              , &  ! DateStr
          TRIM(scalar_dname_table( grid%id, itrace )) // '_BXE', & !data name
          grid%scalar_BXE(jms,kds,1,itrace)  , &  ! Field
                       WRF_FLOAT             , &  ! FieldType 
          grid%communicator  , &  ! Comm
          grid%iocommunicator  , &  ! Comm
          grid%domdesc       , &  ! Comm
          grid%bdy_mask       , &  ! bdy_mask
          dryrun             , &  ! flag
          'XEZ'               , &  ! MemoryOrder
          ''                , &  ! Stagger
                       'south_north'               , &  ! Dimname 1 
                       'bottom_top'               , &  ! Dimname 2 
                       'bdy_width'               , &  ! Dimname 3 
          scalar_desc_table( grid%id, itrace  ), & ! Desc
          scalar_units_table( grid%id, itrace  ), & ! Units
'inc/wrf_bdyout.inc ext_write_field '//TRIM(scalar_dname_table( grid%id, itrace ))//' memorder XEZ' , & ! Debug message
1, (jde-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
jms, jme, kds, kde, 1, config_flags%spec_bdy_width, &
jps, MIN( (jde-1), jpe ), kds, (kde-1), 1, config_flags%spec_bdy_width, &
                         ierr )
    CALL wrf_ext_write_field (  &
          fid                             , &  ! DataHandle
          current_date(1:19)              , &  ! DateStr
          TRIM(scalar_dname_table( grid%id, itrace )) // '_BYS', & !data name
          grid%scalar_BYS(ims,kds,1,itrace)  , &  ! Field
                       WRF_FLOAT             , &  ! FieldType 
          grid%communicator  , &  ! Comm
          grid%iocommunicator  , &  ! Comm
          grid%domdesc       , &  ! Comm
          grid%bdy_mask       , &  ! bdy_mask
          dryrun             , &  ! flag
          'YSZ'               , &  ! MemoryOrder
          ''                , &  ! Stagger
                       'west_east'               , &  ! Dimname 1 
                       'bottom_top'               , &  ! Dimname 2 
                       'bdy_width'               , &  ! Dimname 3 
          scalar_desc_table( grid%id, itrace  ), & ! Desc
          scalar_units_table( grid%id, itrace  ), & ! Units
'inc/wrf_bdyout.inc ext_write_field '//TRIM(scalar_dname_table( grid%id, itrace ))//' memorder YSZ' , & ! Debug message
1, (ide-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
ims, ime, kds, kde, 1, config_flags%spec_bdy_width, &
ips, MIN( (ide-1), ipe ), kds, (kde-1), 1, config_flags%spec_bdy_width, &
                         ierr )
    CALL wrf_ext_write_field (  &
          fid                             , &  ! DataHandle
          current_date(1:19)              , &  ! DateStr
          TRIM(scalar_dname_table( grid%id, itrace )) // '_BYE', & !data name
          grid%scalar_BYE(ims,kds,1,itrace)  , &  ! Field
                       WRF_FLOAT             , &  ! FieldType 
          grid%communicator  , &  ! Comm
          grid%iocommunicator  , &  ! Comm
          grid%domdesc       , &  ! Comm
          grid%bdy_mask       , &  ! bdy_mask
          dryrun             , &  ! flag
          'YEZ'               , &  ! MemoryOrder
          ''                , &  ! Stagger
                       'west_east'               , &  ! Dimname 1 
                       'bottom_top'               , &  ! Dimname 2 
                       'bdy_width'               , &  ! Dimname 3 
          scalar_desc_table( grid%id, itrace  ), & ! Desc
          scalar_units_table( grid%id, itrace  ), & ! Units
'inc/wrf_bdyout.inc ext_write_field '//TRIM(scalar_dname_table( grid%id, itrace ))//' memorder YEZ' , & ! Debug message
1, (ide-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
ims, ime, kds, kde, 1, config_flags%spec_bdy_width, &
ips, MIN( (ide-1), ipe ), kds, (kde-1), 1, config_flags%spec_bdy_width, &
                         ierr )
  ENDIF
ENDDO
DO itrace = PARAM_FIRST_SCALAR , num_scalar
  IF ( scalar_boundary_table(grid%id, itrace ) ) THEN
    CALL wrf_ext_write_field (  &
          fid                             , &  ! DataHandle
          current_date(1:19)              , &  ! DateStr
          TRIM(scalar_dname_table( grid%id, itrace )) // '_BTXS', & !data name
          grid%scalar_BTXS(jms,kds,1,itrace)  , &  ! Field
                       WRF_FLOAT             , &  ! FieldType 
          grid%communicator  , &  ! Comm
          grid%iocommunicator  , &  ! Comm
          grid%domdesc       , &  ! Comm
          grid%bdy_mask       , &  ! bdy_mask
          dryrun             , &  ! flag
          'XSZ'               , &  ! MemoryOrder
          ''                , &  ! Stagger
                       'south_north'               , &  ! Dimname 1 
                       'bottom_top'               , &  ! Dimname 2 
                       'bdy_width'               , &  ! Dimname 3 
          scalar_desc_table( grid%id, itrace  ), & ! Desc
          scalar_units_table( grid%id, itrace  ), & ! Units
'inc/wrf_bdyout.inc ext_write_field '//TRIM(scalar_dname_table( grid%id, itrace ))//' memorder XSZ' , & ! Debug message
1, (jde-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
jms, jme, kds, kde, 1, config_flags%spec_bdy_width, &
jps, MIN( (jde-1), jpe ), kds, (kde-1), 1, config_flags%spec_bdy_width, &
                         ierr )
    CALL wrf_ext_write_field (  &
          fid                             , &  ! DataHandle
          current_date(1:19)              , &  ! DateStr
          TRIM(scalar_dname_table( grid%id, itrace )) // '_BTXE', & !data name
          grid%scalar_BTXE(jms,kds,1,itrace)  , &  ! Field
                       WRF_FLOAT             , &  ! FieldType 
          grid%communicator  , &  ! Comm
          grid%iocommunicator  , &  ! Comm
          grid%domdesc       , &  ! Comm
          grid%bdy_mask       , &  ! bdy_mask
          dryrun             , &  ! flag
          'XEZ'               , &  ! MemoryOrder
          ''                , &  ! Stagger
                       'south_north'               , &  ! Dimname 1 
                       'bottom_top'               , &  ! Dimname 2 
                       'bdy_width'               , &  ! Dimname 3 
          scalar_desc_table( grid%id, itrace  ), & ! Desc
          scalar_units_table( grid%id, itrace  ), & ! Units
'inc/wrf_bdyout.inc ext_write_field '//TRIM(scalar_dname_table( grid%id, itrace ))//' memorder XEZ' , & ! Debug message
1, (jde-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
jms, jme, kds, kde, 1, config_flags%spec_bdy_width, &
jps, MIN( (jde-1), jpe ), kds, (kde-1), 1, config_flags%spec_bdy_width, &
                         ierr )
    CALL wrf_ext_write_field (  &
          fid                             , &  ! DataHandle
          current_date(1:19)              , &  ! DateStr
          TRIM(scalar_dname_table( grid%id, itrace )) // '_BTYS', & !data name
          grid%scalar_BTYS(ims,kds,1,itrace)  , &  ! Field
                       WRF_FLOAT             , &  ! FieldType 
          grid%communicator  , &  ! Comm
          grid%iocommunicator  , &  ! Comm
          grid%domdesc       , &  ! Comm
          grid%bdy_mask       , &  ! bdy_mask
          dryrun             , &  ! flag
          'YSZ'               , &  ! MemoryOrder
          ''                , &  ! Stagger
                       'west_east'               , &  ! Dimname 1 
                       'bottom_top'               , &  ! Dimname 2 
                       'bdy_width'               , &  ! Dimname 3 
          scalar_desc_table( grid%id, itrace  ), & ! Desc
          scalar_units_table( grid%id, itrace  ), & ! Units
'inc/wrf_bdyout.inc ext_write_field '//TRIM(scalar_dname_table( grid%id, itrace ))//' memorder YSZ' , & ! Debug message
1, (ide-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
ims, ime, kds, kde, 1, config_flags%spec_bdy_width, &
ips, MIN( (ide-1), ipe ), kds, (kde-1), 1, config_flags%spec_bdy_width, &
                         ierr )
    CALL wrf_ext_write_field (  &
          fid                             , &  ! DataHandle
          current_date(1:19)              , &  ! DateStr
          TRIM(scalar_dname_table( grid%id, itrace )) // '_BTYE', & !data name
          grid%scalar_BTYE(ims,kds,1,itrace)  , &  ! Field
                       WRF_FLOAT             , &  ! FieldType 
          grid%communicator  , &  ! Comm
          grid%iocommunicator  , &  ! Comm
          grid%domdesc       , &  ! Comm
          grid%bdy_mask       , &  ! bdy_mask
          dryrun             , &  ! flag
          'YEZ'               , &  ! MemoryOrder
          ''                , &  ! Stagger
                       'west_east'               , &  ! Dimname 1 
                       'bottom_top'               , &  ! Dimname 2 
                       'bdy_width'               , &  ! Dimname 3 
          scalar_desc_table( grid%id, itrace  ), & ! Desc
          scalar_units_table( grid%id, itrace  ), & ! Units
'inc/wrf_bdyout.inc ext_write_field '//TRIM(scalar_dname_table( grid%id, itrace ))//' memorder YEZ' , & ! Debug message
1, (ide-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
ims, ime, kds, kde, 1, config_flags%spec_bdy_width, &
ips, MIN( (ide-1), ipe ), kds, (kde-1), 1, config_flags%spec_bdy_width, &
                         ierr )
  ENDIF
ENDDO
DO itrace = PARAM_FIRST_SCALAR , num_dfi_scalar
  IF ( dfi_scalar_boundary_table(grid%id, itrace ) ) THEN
    CALL wrf_ext_write_field (  &
          fid                             , &  ! DataHandle
          current_date(1:19)              , &  ! DateStr
          TRIM(dfi_scalar_dname_table( grid%id, itrace )) // '_BXS', & !data name
          grid%dfi_scalar_BXS(jms,kds,1,itrace)  , &  ! Field
                       WRF_FLOAT             , &  ! FieldType 
          grid%communicator  , &  ! Comm
          grid%iocommunicator  , &  ! Comm
          grid%domdesc       , &  ! Comm
          grid%bdy_mask       , &  ! bdy_mask
          dryrun             , &  ! flag
          'XSZ'               , &  ! MemoryOrder
          ''                , &  ! Stagger
                       'south_north'               , &  ! Dimname 1 
                       'bottom_top'               , &  ! Dimname 2 
                       'bdy_width'               , &  ! Dimname 3 
          dfi_scalar_desc_table( grid%id, itrace  ), & ! Desc
          dfi_scalar_units_table( grid%id, itrace  ), & ! Units
'inc/wrf_bdyout.inc ext_write_field '//TRIM(dfi_scalar_dname_table( grid%id, itrace ))//' memorder XSZ' , & ! Debug message
1, (jde-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
jms, jme, kds, kde, 1, config_flags%spec_bdy_width, &
jps, MIN( (jde-1), jpe ), kds, (kde-1), 1, config_flags%spec_bdy_width, &
                         ierr )
    CALL wrf_ext_write_field (  &
          fid                             , &  ! DataHandle
          current_date(1:19)              , &  ! DateStr
          TRIM(dfi_scalar_dname_table( grid%id, itrace )) // '_BXE', & !data name
          grid%dfi_scalar_BXE(jms,kds,1,itrace)  , &  ! Field
                       WRF_FLOAT             , &  ! FieldType 
          grid%communicator  , &  ! Comm
          grid%iocommunicator  , &  ! Comm
          grid%domdesc       , &  ! Comm
          grid%bdy_mask       , &  ! bdy_mask
          dryrun             , &  ! flag
          'XEZ'               , &  ! MemoryOrder
          ''                , &  ! Stagger
                       'south_north'               , &  ! Dimname 1 
                       'bottom_top'               , &  ! Dimname 2 
                       'bdy_width'               , &  ! Dimname 3 
          dfi_scalar_desc_table( grid%id, itrace  ), & ! Desc
          dfi_scalar_units_table( grid%id, itrace  ), & ! Units
'inc/wrf_bdyout.inc ext_write_field '//TRIM(dfi_scalar_dname_table( grid%id, itrace ))//' memorder XEZ' , & ! Debug message
1, (jde-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
jms, jme, kds, kde, 1, config_flags%spec_bdy_width, &
jps, MIN( (jde-1), jpe ), kds, (kde-1), 1, config_flags%spec_bdy_width, &
                         ierr )
    CALL wrf_ext_write_field (  &
          fid                             , &  ! DataHandle
          current_date(1:19)              , &  ! DateStr
          TRIM(dfi_scalar_dname_table( grid%id, itrace )) // '_BYS', & !data name
          grid%dfi_scalar_BYS(ims,kds,1,itrace)  , &  ! Field
                       WRF_FLOAT             , &  ! FieldType 
          grid%communicator  , &  ! Comm
          grid%iocommunicator  , &  ! Comm
          grid%domdesc       , &  ! Comm
          grid%bdy_mask       , &  ! bdy_mask
          dryrun             , &  ! flag
          'YSZ'               , &  ! MemoryOrder
          ''                , &  ! Stagger
                       'west_east'               , &  ! Dimname 1 
                       'bottom_top'               , &  ! Dimname 2 
                       'bdy_width'               , &  ! Dimname 3 
          dfi_scalar_desc_table( grid%id, itrace  ), & ! Desc
          dfi_scalar_units_table( grid%id, itrace  ), & ! Units
'inc/wrf_bdyout.inc ext_write_field '//TRIM(dfi_scalar_dname_table( grid%id, itrace ))//' memorder YSZ' , & ! Debug message
1, (ide-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
ims, ime, kds, kde, 1, config_flags%spec_bdy_width, &
ips, MIN( (ide-1), ipe ), kds, (kde-1), 1, config_flags%spec_bdy_width, &
                         ierr )
    CALL wrf_ext_write_field (  &
          fid                             , &  ! DataHandle
          current_date(1:19)              , &  ! DateStr
          TRIM(dfi_scalar_dname_table( grid%id, itrace )) // '_BYE', & !data name
          grid%dfi_scalar_BYE(ims,kds,1,itrace)  , &  ! Field
                       WRF_FLOAT             , &  ! FieldType 
          grid%communicator  , &  ! Comm
          grid%iocommunicator  , &  ! Comm
          grid%domdesc       , &  ! Comm
          grid%bdy_mask       , &  ! bdy_mask
          dryrun             , &  ! flag
          'YEZ'               , &  ! MemoryOrder
          ''                , &  ! Stagger
                       'west_east'               , &  ! Dimname 1 
                       'bottom_top'               , &  ! Dimname 2 
                       'bdy_width'               , &  ! Dimname 3 
          dfi_scalar_desc_table( grid%id, itrace  ), & ! Desc
          dfi_scalar_units_table( grid%id, itrace  ), & ! Units
'inc/wrf_bdyout.inc ext_write_field '//TRIM(dfi_scalar_dname_table( grid%id, itrace ))//' memorder YEZ' , & ! Debug message
1, (ide-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
ims, ime, kds, kde, 1, config_flags%spec_bdy_width, &
ips, MIN( (ide-1), ipe ), kds, (kde-1), 1, config_flags%spec_bdy_width, &
                         ierr )
  ENDIF
ENDDO
DO itrace = PARAM_FIRST_SCALAR , num_dfi_scalar
  IF ( dfi_scalar_boundary_table(grid%id, itrace ) ) THEN
    CALL wrf_ext_write_field (  &
          fid                             , &  ! DataHandle
          current_date(1:19)              , &  ! DateStr
          TRIM(dfi_scalar_dname_table( grid%id, itrace )) // '_BTXS', & !data name
          grid%dfi_scalar_BTXS(jms,kds,1,itrace)  , &  ! Field
                       WRF_FLOAT             , &  ! FieldType 
          grid%communicator  , &  ! Comm
          grid%iocommunicator  , &  ! Comm
          grid%domdesc       , &  ! Comm
          grid%bdy_mask       , &  ! bdy_mask
          dryrun             , &  ! flag
          'XSZ'               , &  ! MemoryOrder
          ''                , &  ! Stagger
                       'south_north'               , &  ! Dimname 1 
                       'bottom_top'               , &  ! Dimname 2 
                       'bdy_width'               , &  ! Dimname 3 
          dfi_scalar_desc_table( grid%id, itrace  ), & ! Desc
          dfi_scalar_units_table( grid%id, itrace  ), & ! Units
'inc/wrf_bdyout.inc ext_write_field '//TRIM(dfi_scalar_dname_table( grid%id, itrace ))//' memorder XSZ' , & ! Debug message
1, (jde-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
jms, jme, kds, kde, 1, config_flags%spec_bdy_width, &
jps, MIN( (jde-1), jpe ), kds, (kde-1), 1, config_flags%spec_bdy_width, &
                         ierr )
    CALL wrf_ext_write_field (  &
          fid                             , &  ! DataHandle
          current_date(1:19)              , &  ! DateStr
          TRIM(dfi_scalar_dname_table( grid%id, itrace )) // '_BTXE', & !data name
          grid%dfi_scalar_BTXE(jms,kds,1,itrace)  , &  ! Field
                       WRF_FLOAT             , &  ! FieldType 
          grid%communicator  , &  ! Comm
          grid%iocommunicator  , &  ! Comm
          grid%domdesc       , &  ! Comm
          grid%bdy_mask       , &  ! bdy_mask
          dryrun             , &  ! flag
          'XEZ'               , &  ! MemoryOrder
          ''                , &  ! Stagger
                       'south_north'               , &  ! Dimname 1 
                       'bottom_top'               , &  ! Dimname 2 
                       'bdy_width'               , &  ! Dimname 3 
          dfi_scalar_desc_table( grid%id, itrace  ), & ! Desc
          dfi_scalar_units_table( grid%id, itrace  ), & ! Units
'inc/wrf_bdyout.inc ext_write_field '//TRIM(dfi_scalar_dname_table( grid%id, itrace ))//' memorder XEZ' , & ! Debug message
1, (jde-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
jms, jme, kds, kde, 1, config_flags%spec_bdy_width, &
jps, MIN( (jde-1), jpe ), kds, (kde-1), 1, config_flags%spec_bdy_width, &
                         ierr )
    CALL wrf_ext_write_field (  &
          fid                             , &  ! DataHandle
          current_date(1:19)              , &  ! DateStr
          TRIM(dfi_scalar_dname_table( grid%id, itrace )) // '_BTYS', & !data name
          grid%dfi_scalar_BTYS(ims,kds,1,itrace)  , &  ! Field
                       WRF_FLOAT             , &  ! FieldType 
          grid%communicator  , &  ! Comm
          grid%iocommunicator  , &  ! Comm
          grid%domdesc       , &  ! Comm
          grid%bdy_mask       , &  ! bdy_mask
          dryrun             , &  ! flag
          'YSZ'               , &  ! MemoryOrder
          ''                , &  ! Stagger
                       'west_east'               , &  ! Dimname 1 
                       'bottom_top'               , &  ! Dimname 2 
                       'bdy_width'               , &  ! Dimname 3 
          dfi_scalar_desc_table( grid%id, itrace  ), & ! Desc
          dfi_scalar_units_table( grid%id, itrace  ), & ! Units
'inc/wrf_bdyout.inc ext_write_field '//TRIM(dfi_scalar_dname_table( grid%id, itrace ))//' memorder YSZ' , & ! Debug message
1, (ide-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
ims, ime, kds, kde, 1, config_flags%spec_bdy_width, &
ips, MIN( (ide-1), ipe ), kds, (kde-1), 1, config_flags%spec_bdy_width, &
                         ierr )
    CALL wrf_ext_write_field (  &
          fid                             , &  ! DataHandle
          current_date(1:19)              , &  ! DateStr
          TRIM(dfi_scalar_dname_table( grid%id, itrace )) // '_BTYE', & !data name
          grid%dfi_scalar_BTYE(ims,kds,1,itrace)  , &  ! Field
                       WRF_FLOAT             , &  ! FieldType 
          grid%communicator  , &  ! Comm
          grid%iocommunicator  , &  ! Comm
          grid%domdesc       , &  ! Comm
          grid%bdy_mask       , &  ! bdy_mask
          dryrun             , &  ! flag
          'YEZ'               , &  ! MemoryOrder
          ''                , &  ! Stagger
                       'west_east'               , &  ! Dimname 1 
                       'bottom_top'               , &  ! Dimname 2 
                       'bdy_width'               , &  ! Dimname 3 
          dfi_scalar_desc_table( grid%id, itrace  ), & ! Desc
          dfi_scalar_units_table( grid%id, itrace  ), & ! Units
'inc/wrf_bdyout.inc ext_write_field '//TRIM(dfi_scalar_dname_table( grid%id, itrace ))//' memorder YEZ' , & ! Debug message
1, (ide-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
ims, ime, kds, kde, 1, config_flags%spec_bdy_width, &
ips, MIN( (ide-1), ipe ), kds, (kde-1), 1, config_flags%spec_bdy_width, &
                         ierr )
  ENDIF
ENDDO
CALL wrf_ext_write_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'HT_SHAD_BXS'               , &  ! Data Name 
                       grid%ht_shad_bxs(jms,kds,1)     , &  ! Field 
                       WRF_FLOAT          , &  ! FieldType 
                       grid%communicator , &  ! Comm
                       grid%iocommunicator , &  ! Comm
                       grid%domdesc      , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       dryrun             , &  ! flag
                       'XS'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
                       'south_north'               , &  ! Dimname 1 
                       'bdy_width'               , &  ! Dimname 2 
                       'one_element'               , &  ! Dimname 3 
                       'bdy Height of orographic shadow'               , &  ! Desc  
                       'm'               , &  ! Units 
'inc/wrf_bdyout.inc ext_write_field HT_SHAD_BXS memorder XS' , & ! Debug message
1, (jde-1), 1, config_flags%spec_bdy_width, 1, 1, &
jms, jme, 1, config_flags%spec_bdy_width, 1, 1, &
jps, MIN( (jde-1), jpe ), 1, config_flags%spec_bdy_width, 1, 1, &
                       ierr )
CALL wrf_ext_write_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'HT_SHAD_BXE'               , &  ! Data Name 
                       grid%ht_shad_bxe(jms,kds,1)     , &  ! Field 
                       WRF_FLOAT          , &  ! FieldType 
                       grid%communicator , &  ! Comm
                       grid%iocommunicator , &  ! Comm
                       grid%domdesc      , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       dryrun             , &  ! flag
                       'XE'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
                       'south_north'               , &  ! Dimname 1 
                       'bdy_width'               , &  ! Dimname 2 
                       'one_element'               , &  ! Dimname 3 
                       'bdy Height of orographic shadow'               , &  ! Desc  
                       'm'               , &  ! Units 
'inc/wrf_bdyout.inc ext_write_field HT_SHAD_BXE memorder XE' , & ! Debug message
1, (jde-1), 1, config_flags%spec_bdy_width, 1, 1, &
jms, jme, 1, config_flags%spec_bdy_width, 1, 1, &
jps, MIN( (jde-1), jpe ), 1, config_flags%spec_bdy_width, 1, 1, &
                       ierr )
CALL wrf_ext_write_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'HT_SHAD_BYS'               , &  ! Data Name 
                       grid%ht_shad_bys(ims,kds,1)     , &  ! Field 
                       WRF_FLOAT          , &  ! FieldType 
                       grid%communicator , &  ! Comm
                       grid%iocommunicator , &  ! Comm
                       grid%domdesc      , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       dryrun             , &  ! flag
                       'YS'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
                       'west_east'               , &  ! Dimname 1 
                       'bdy_width'               , &  ! Dimname 2 
                       'one_element'               , &  ! Dimname 3 
                       'bdy Height of orographic shadow'               , &  ! Desc  
                       'm'               , &  ! Units 
'inc/wrf_bdyout.inc ext_write_field HT_SHAD_BYS memorder YS' , & ! Debug message
1, (ide-1), 1, config_flags%spec_bdy_width, 1, 1, &
ims, ime, 1, config_flags%spec_bdy_width, 1, 1, &
ips, MIN( (ide-1), ipe ), 1, config_flags%spec_bdy_width, 1, 1, &
                       ierr )
CALL wrf_ext_write_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'HT_SHAD_BYE'               , &  ! Data Name 
                       grid%ht_shad_bye(ims,kds,1)     , &  ! Field 
                       WRF_FLOAT          , &  ! FieldType 
                       grid%communicator , &  ! Comm
                       grid%iocommunicator , &  ! Comm
                       grid%domdesc      , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       dryrun             , &  ! flag
                       'YE'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
                       'west_east'               , &  ! Dimname 1 
                       'bdy_width'               , &  ! Dimname 2 
                       'one_element'               , &  ! Dimname 3 
                       'bdy Height of orographic shadow'               , &  ! Desc  
                       'm'               , &  ! Units 
'inc/wrf_bdyout.inc ext_write_field HT_SHAD_BYE memorder YE' , & ! Debug message
1, (ide-1), 1, config_flags%spec_bdy_width, 1, 1, &
ims, ime, 1, config_flags%spec_bdy_width, 1, 1, &
ips, MIN( (ide-1), ipe ), 1, config_flags%spec_bdy_width, 1, 1, &
                       ierr )
CALL wrf_ext_write_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'HT_SHAD_BTXS'               , &  ! Data Name 
                       grid%ht_shad_btxs(jms,kds,1)     , &  ! Field 
                       WRF_FLOAT          , &  ! FieldType 
                       grid%communicator , &  ! Comm
                       grid%iocommunicator , &  ! Comm
                       grid%domdesc      , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       dryrun             , &  ! flag
                       'XS'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
                       'south_north'               , &  ! Dimname 1 
                       'bdy_width'               , &  ! Dimname 2 
                       'one_element'               , &  ! Dimname 3 
                       'bdy tend Height of orographic shadow'               , &  ! Desc  
                       '(m)/dt'               , &  ! Units 
'inc/wrf_bdyout.inc ext_write_field HT_SHAD_BTXS memorder XS' , & ! Debug message
1, (jde-1), 1, config_flags%spec_bdy_width, 1, 1, &
jms, jme, 1, config_flags%spec_bdy_width, 1, 1, &
jps, MIN( (jde-1), jpe ), 1, config_flags%spec_bdy_width, 1, 1, &
                       ierr )
CALL wrf_ext_write_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'HT_SHAD_BTXE'               , &  ! Data Name 
                       grid%ht_shad_btxe(jms,kds,1)     , &  ! Field 
                       WRF_FLOAT          , &  ! FieldType 
                       grid%communicator , &  ! Comm
                       grid%iocommunicator , &  ! Comm
                       grid%domdesc      , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       dryrun             , &  ! flag
                       'XE'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
                       'south_north'               , &  ! Dimname 1 
                       'bdy_width'               , &  ! Dimname 2 
                       'one_element'               , &  ! Dimname 3 
                       'bdy tend Height of orographic shadow'               , &  ! Desc  
                       '(m)/dt'               , &  ! Units 
'inc/wrf_bdyout.inc ext_write_field HT_SHAD_BTXE memorder XE' , & ! Debug message
1, (jde-1), 1, config_flags%spec_bdy_width, 1, 1, &
jms, jme, 1, config_flags%spec_bdy_width, 1, 1, &
jps, MIN( (jde-1), jpe ), 1, config_flags%spec_bdy_width, 1, 1, &
                       ierr )
CALL wrf_ext_write_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'HT_SHAD_BTYS'               , &  ! Data Name 
                       grid%ht_shad_btys(ims,kds,1)     , &  ! Field 
                       WRF_FLOAT          , &  ! FieldType 
                       grid%communicator , &  ! Comm
                       grid%iocommunicator , &  ! Comm
                       grid%domdesc      , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       dryrun             , &  ! flag
                       'YS'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
                       'west_east'               , &  ! Dimname 1 
                       'bdy_width'               , &  ! Dimname 2 
                       'one_element'               , &  ! Dimname 3 
                       'bdy tend Height of orographic shadow'               , &  ! Desc  
                       '(m)/dt'               , &  ! Units 
'inc/wrf_bdyout.inc ext_write_field HT_SHAD_BTYS memorder YS' , & ! Debug message
1, (ide-1), 1, config_flags%spec_bdy_width, 1, 1, &
ims, ime, 1, config_flags%spec_bdy_width, 1, 1, &
ips, MIN( (ide-1), ipe ), 1, config_flags%spec_bdy_width, 1, 1, &
                       ierr )
CALL wrf_ext_write_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'HT_SHAD_BTYE'               , &  ! Data Name 
                       grid%ht_shad_btye(ims,kds,1)     , &  ! Field 
                       WRF_FLOAT          , &  ! FieldType 
                       grid%communicator , &  ! Comm
                       grid%iocommunicator , &  ! Comm
                       grid%domdesc      , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       dryrun             , &  ! flag
                       'YE'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
                       'west_east'               , &  ! Dimname 1 
                       'bdy_width'               , &  ! Dimname 2 
                       'one_element'               , &  ! Dimname 3 
                       'bdy tend Height of orographic shadow'               , &  ! Desc  
                       '(m)/dt'               , &  ! Units 
'inc/wrf_bdyout.inc ext_write_field HT_SHAD_BTYE memorder YE' , & ! Debug message
1, (ide-1), 1, config_flags%spec_bdy_width, 1, 1, &
ims, ime, 1, config_flags%spec_bdy_width, 1, 1, &
ips, MIN( (ide-1), ipe ), 1, config_flags%spec_bdy_width, 1, 1, &
                       ierr )
DO itrace = PARAM_FIRST_SCALAR , num_tracer
  IF ( tracer_boundary_table(grid%id, itrace ) ) THEN
    CALL wrf_ext_write_field (  &
          fid                             , &  ! DataHandle
          current_date(1:19)              , &  ! DateStr
          TRIM(tracer_dname_table( grid%id, itrace )) // '_BXS', & !data name
          grid%tracer_BXS(jms,kds,1,itrace)  , &  ! Field
                       WRF_FLOAT             , &  ! FieldType 
          grid%communicator  , &  ! Comm
          grid%iocommunicator  , &  ! Comm
          grid%domdesc       , &  ! Comm
          grid%bdy_mask       , &  ! bdy_mask
          dryrun             , &  ! flag
          'XSZ'               , &  ! MemoryOrder
          ''                , &  ! Stagger
                       'south_north'               , &  ! Dimname 1 
                       'bottom_top'               , &  ! Dimname 2 
                       'bdy_width'               , &  ! Dimname 3 
          tracer_desc_table( grid%id, itrace  ), & ! Desc
          tracer_units_table( grid%id, itrace  ), & ! Units
'inc/wrf_bdyout.inc ext_write_field '//TRIM(tracer_dname_table( grid%id, itrace ))//' memorder XSZ' , & ! Debug message
1, (jde-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
jms, jme, kds, kde, 1, config_flags%spec_bdy_width, &
jps, MIN( (jde-1), jpe ), kds, (kde-1), 1, config_flags%spec_bdy_width, &
                         ierr )
    CALL wrf_ext_write_field (  &
          fid                             , &  ! DataHandle
          current_date(1:19)              , &  ! DateStr
          TRIM(tracer_dname_table( grid%id, itrace )) // '_BXE', & !data name
          grid%tracer_BXE(jms,kds,1,itrace)  , &  ! Field
                       WRF_FLOAT             , &  ! FieldType 
          grid%communicator  , &  ! Comm
          grid%iocommunicator  , &  ! Comm
          grid%domdesc       , &  ! Comm
          grid%bdy_mask       , &  ! bdy_mask
          dryrun             , &  ! flag
          'XEZ'               , &  ! MemoryOrder
          ''                , &  ! Stagger
                       'south_north'               , &  ! Dimname 1 
                       'bottom_top'               , &  ! Dimname 2 
                       'bdy_width'               , &  ! Dimname 3 
          tracer_desc_table( grid%id, itrace  ), & ! Desc
          tracer_units_table( grid%id, itrace  ), & ! Units
'inc/wrf_bdyout.inc ext_write_field '//TRIM(tracer_dname_table( grid%id, itrace ))//' memorder XEZ' , & ! Debug message
1, (jde-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
jms, jme, kds, kde, 1, config_flags%spec_bdy_width, &
jps, MIN( (jde-1), jpe ), kds, (kde-1), 1, config_flags%spec_bdy_width, &
                         ierr )
    CALL wrf_ext_write_field (  &
          fid                             , &  ! DataHandle
          current_date(1:19)              , &  ! DateStr
          TRIM(tracer_dname_table( grid%id, itrace )) // '_BYS', & !data name
          grid%tracer_BYS(ims,kds,1,itrace)  , &  ! Field
                       WRF_FLOAT             , &  ! FieldType 
          grid%communicator  , &  ! Comm
          grid%iocommunicator  , &  ! Comm
          grid%domdesc       , &  ! Comm
          grid%bdy_mask       , &  ! bdy_mask
          dryrun             , &  ! flag
          'YSZ'               , &  ! MemoryOrder
          ''                , &  ! Stagger
                       'west_east'               , &  ! Dimname 1 
                       'bottom_top'               , &  ! Dimname 2 
                       'bdy_width'               , &  ! Dimname 3 
          tracer_desc_table( grid%id, itrace  ), & ! Desc
          tracer_units_table( grid%id, itrace  ), & ! Units
'inc/wrf_bdyout.inc ext_write_field '//TRIM(tracer_dname_table( grid%id, itrace ))//' memorder YSZ' , & ! Debug message
1, (ide-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
ims, ime, kds, kde, 1, config_flags%spec_bdy_width, &
ips, MIN( (ide-1), ipe ), kds, (kde-1), 1, config_flags%spec_bdy_width, &
                         ierr )
    CALL wrf_ext_write_field (  &
          fid                             , &  ! DataHandle
          current_date(1:19)              , &  ! DateStr
          TRIM(tracer_dname_table( grid%id, itrace )) // '_BYE', & !data name
          grid%tracer_BYE(ims,kds,1,itrace)  , &  ! Field
                       WRF_FLOAT             , &  ! FieldType 
          grid%communicator  , &  ! Comm
          grid%iocommunicator  , &  ! Comm
          grid%domdesc       , &  ! Comm
          grid%bdy_mask       , &  ! bdy_mask
          dryrun             , &  ! flag
          'YEZ'               , &  ! MemoryOrder
          ''                , &  ! Stagger
                       'west_east'               , &  ! Dimname 1 
                       'bottom_top'               , &  ! Dimname 2 
                       'bdy_width'               , &  ! Dimname 3 
          tracer_desc_table( grid%id, itrace  ), & ! Desc
          tracer_units_table( grid%id, itrace  ), & ! Units
'inc/wrf_bdyout.inc ext_write_field '//TRIM(tracer_dname_table( grid%id, itrace ))//' memorder YEZ' , & ! Debug message
1, (ide-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
ims, ime, kds, kde, 1, config_flags%spec_bdy_width, &
ips, MIN( (ide-1), ipe ), kds, (kde-1), 1, config_flags%spec_bdy_width, &
                         ierr )
  ENDIF
ENDDO
DO itrace = PARAM_FIRST_SCALAR , num_tracer
  IF ( tracer_boundary_table(grid%id, itrace ) ) THEN
    CALL wrf_ext_write_field (  &
          fid                             , &  ! DataHandle
          current_date(1:19)              , &  ! DateStr
          TRIM(tracer_dname_table( grid%id, itrace )) // '_BTXS', & !data name
          grid%tracer_BTXS(jms,kds,1,itrace)  , &  ! Field
                       WRF_FLOAT             , &  ! FieldType 
          grid%communicator  , &  ! Comm
          grid%iocommunicator  , &  ! Comm
          grid%domdesc       , &  ! Comm
          grid%bdy_mask       , &  ! bdy_mask
          dryrun             , &  ! flag
          'XSZ'               , &  ! MemoryOrder
          ''                , &  ! Stagger
                       'south_north'               , &  ! Dimname 1 
                       'bottom_top'               , &  ! Dimname 2 
                       'bdy_width'               , &  ! Dimname 3 
          tracer_desc_table( grid%id, itrace  ), & ! Desc
          tracer_units_table( grid%id, itrace  ), & ! Units
'inc/wrf_bdyout.inc ext_write_field '//TRIM(tracer_dname_table( grid%id, itrace ))//' memorder XSZ' , & ! Debug message
1, (jde-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
jms, jme, kds, kde, 1, config_flags%spec_bdy_width, &
jps, MIN( (jde-1), jpe ), kds, (kde-1), 1, config_flags%spec_bdy_width, &
                         ierr )
    CALL wrf_ext_write_field (  &
          fid                             , &  ! DataHandle
          current_date(1:19)              , &  ! DateStr
          TRIM(tracer_dname_table( grid%id, itrace )) // '_BTXE', & !data name
          grid%tracer_BTXE(jms,kds,1,itrace)  , &  ! Field
                       WRF_FLOAT             , &  ! FieldType 
          grid%communicator  , &  ! Comm
          grid%iocommunicator  , &  ! Comm
          grid%domdesc       , &  ! Comm
          grid%bdy_mask       , &  ! bdy_mask
          dryrun             , &  ! flag
          'XEZ'               , &  ! MemoryOrder
          ''                , &  ! Stagger
                       'south_north'               , &  ! Dimname 1 
                       'bottom_top'               , &  ! Dimname 2 
                       'bdy_width'               , &  ! Dimname 3 
          tracer_desc_table( grid%id, itrace  ), & ! Desc
          tracer_units_table( grid%id, itrace  ), & ! Units
'inc/wrf_bdyout.inc ext_write_field '//TRIM(tracer_dname_table( grid%id, itrace ))//' memorder XEZ' , & ! Debug message
1, (jde-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
jms, jme, kds, kde, 1, config_flags%spec_bdy_width, &
jps, MIN( (jde-1), jpe ), kds, (kde-1), 1, config_flags%spec_bdy_width, &
                         ierr )
    CALL wrf_ext_write_field (  &
          fid                             , &  ! DataHandle
          current_date(1:19)              , &  ! DateStr
          TRIM(tracer_dname_table( grid%id, itrace )) // '_BTYS', & !data name
          grid%tracer_BTYS(ims,kds,1,itrace)  , &  ! Field
                       WRF_FLOAT             , &  ! FieldType 
          grid%communicator  , &  ! Comm
          grid%iocommunicator  , &  ! Comm
          grid%domdesc       , &  ! Comm
          grid%bdy_mask       , &  ! bdy_mask
          dryrun             , &  ! flag
          'YSZ'               , &  ! MemoryOrder
          ''                , &  ! Stagger
                       'west_east'               , &  ! Dimname 1 
                       'bottom_top'               , &  ! Dimname 2 
                       'bdy_width'               , &  ! Dimname 3 
          tracer_desc_table( grid%id, itrace  ), & ! Desc
          tracer_units_table( grid%id, itrace  ), & ! Units
'inc/wrf_bdyout.inc ext_write_field '//TRIM(tracer_dname_table( grid%id, itrace ))//' memorder YSZ' , & ! Debug message
1, (ide-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
ims, ime, kds, kde, 1, config_flags%spec_bdy_width, &
ips, MIN( (ide-1), ipe ), kds, (kde-1), 1, config_flags%spec_bdy_width, &
                         ierr )
    CALL wrf_ext_write_field (  &
          fid                             , &  ! DataHandle
          current_date(1:19)              , &  ! DateStr
          TRIM(tracer_dname_table( grid%id, itrace )) // '_BTYE', & !data name
          grid%tracer_BTYE(ims,kds,1,itrace)  , &  ! Field
                       WRF_FLOAT             , &  ! FieldType 
          grid%communicator  , &  ! Comm
          grid%iocommunicator  , &  ! Comm
          grid%domdesc       , &  ! Comm
          grid%bdy_mask       , &  ! bdy_mask
          dryrun             , &  ! flag
          'YEZ'               , &  ! MemoryOrder
          ''                , &  ! Stagger
                       'west_east'               , &  ! Dimname 1 
                       'bottom_top'               , &  ! Dimname 2 
                       'bdy_width'               , &  ! Dimname 3 
          tracer_desc_table( grid%id, itrace  ), & ! Desc
          tracer_units_table( grid%id, itrace  ), & ! Units
'inc/wrf_bdyout.inc ext_write_field '//TRIM(tracer_dname_table( grid%id, itrace ))//' memorder YEZ' , & ! Debug message
1, (ide-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
ims, ime, kds, kde, 1, config_flags%spec_bdy_width, &
ips, MIN( (ide-1), ipe ), kds, (kde-1), 1, config_flags%spec_bdy_width, &
                         ierr )
  ENDIF
ENDDO
!ENDOFREGISTRYGENERATEDINCLUDE
