!STARTOFREGISTRYGENERATEDINCLUDE 'inc/wrf_bdyin.inc'
!
! WARNING This file is generated automatically by use_registry
! using the data base in the file named Registry.
! Do not edit.  Your changes to this file will be lost.
!
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'U_BXS'               , &  ! Data Name 
                       grid%u_bxs(jms,kds,1)     , &  ! Field 
                       WRF_FLOAT             , &  ! FieldType 
                       grid%communicator , &  ! Comm
                       grid%iocommunicator , &  ! Comm
                       grid%domdesc      , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XSZ'               , &  ! MemoryOrder
                       'X'               , &  ! Stagger
'inc/wrf_bdyin.inc ext_read_field U_BXS memorder XSZ' , & ! Debug message
1, (jde-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
jms, jme, kds, kde, 1, config_flags%spec_bdy_width, &
MAX(jms,jds), MIN(jme,(jde-1)), kds, (kde-1), 1, config_flags%spec_bdy_width, &
                       ierr )
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'U_BXE'               , &  ! Data Name 
                       grid%u_bxe(jms,kds,1)     , &  ! Field 
                       WRF_FLOAT             , &  ! FieldType 
                       grid%communicator , &  ! Comm
                       grid%iocommunicator , &  ! Comm
                       grid%domdesc      , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XEZ'               , &  ! MemoryOrder
                       'X'               , &  ! Stagger
'inc/wrf_bdyin.inc ext_read_field U_BXE memorder XEZ' , & ! Debug message
1, (jde-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
jms, jme, kds, kde, 1, config_flags%spec_bdy_width, &
MAX(jms,jds), MIN(jme,(jde-1)), kds, (kde-1), 1, config_flags%spec_bdy_width, &
                       ierr )
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'U_BYS'               , &  ! Data Name 
                       grid%u_bys(ims,kds,1)     , &  ! Field 
                       WRF_FLOAT             , &  ! FieldType 
                       grid%communicator , &  ! Comm
                       grid%iocommunicator , &  ! Comm
                       grid%domdesc      , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'YSZ'               , &  ! MemoryOrder
                       'X'               , &  ! Stagger
'inc/wrf_bdyin.inc ext_read_field U_BYS memorder YSZ' , & ! Debug message
1, ide, kds, (kde-1), 1, config_flags%spec_bdy_width, &
ims, ime, kds, kde, 1, config_flags%spec_bdy_width, &
MAX(ims,ids), MIN(ime,ide), kds, (kde-1), 1, config_flags%spec_bdy_width, &
                       ierr )
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'U_BYE'               , &  ! Data Name 
                       grid%u_bye(ims,kds,1)     , &  ! Field 
                       WRF_FLOAT             , &  ! FieldType 
                       grid%communicator , &  ! Comm
                       grid%iocommunicator , &  ! Comm
                       grid%domdesc      , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'YEZ'               , &  ! MemoryOrder
                       'X'               , &  ! Stagger
'inc/wrf_bdyin.inc ext_read_field U_BYE memorder YEZ' , & ! Debug message
1, ide, kds, (kde-1), 1, config_flags%spec_bdy_width, &
ims, ime, kds, kde, 1, config_flags%spec_bdy_width, &
MAX(ims,ids), MIN(ime,ide), kds, (kde-1), 1, config_flags%spec_bdy_width, &
                       ierr )
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'U_BTXS'               , &  ! Data Name 
                       grid%u_btxs(jms,kds,1)     , &  ! Field 
                       WRF_FLOAT             , &  ! FieldType 
                       grid%communicator , &  ! Comm
                       grid%iocommunicator , &  ! Comm
                       grid%domdesc      , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XSZ'               , &  ! MemoryOrder
                       'X'               , &  ! Stagger
'inc/wrf_bdyin.inc ext_read_field U_BTXS memorder XSZ' , & ! Debug message
1, (jde-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
jms, jme, kds, kde, 1, config_flags%spec_bdy_width, &
MAX(jms,jds), MIN(jme,(jde-1)), kds, (kde-1), 1, config_flags%spec_bdy_width, &
                       ierr )
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'U_BTXE'               , &  ! Data Name 
                       grid%u_btxe(jms,kds,1)     , &  ! Field 
                       WRF_FLOAT             , &  ! FieldType 
                       grid%communicator , &  ! Comm
                       grid%iocommunicator , &  ! Comm
                       grid%domdesc      , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XEZ'               , &  ! MemoryOrder
                       'X'               , &  ! Stagger
'inc/wrf_bdyin.inc ext_read_field U_BTXE memorder XEZ' , & ! Debug message
1, (jde-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
jms, jme, kds, kde, 1, config_flags%spec_bdy_width, &
MAX(jms,jds), MIN(jme,(jde-1)), kds, (kde-1), 1, config_flags%spec_bdy_width, &
                       ierr )
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'U_BTYS'               , &  ! Data Name 
                       grid%u_btys(ims,kds,1)     , &  ! Field 
                       WRF_FLOAT             , &  ! FieldType 
                       grid%communicator , &  ! Comm
                       grid%iocommunicator , &  ! Comm
                       grid%domdesc      , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'YSZ'               , &  ! MemoryOrder
                       'X'               , &  ! Stagger
'inc/wrf_bdyin.inc ext_read_field U_BTYS memorder YSZ' , & ! Debug message
1, ide, kds, (kde-1), 1, config_flags%spec_bdy_width, &
ims, ime, kds, kde, 1, config_flags%spec_bdy_width, &
MAX(ims,ids), MIN(ime,ide), kds, (kde-1), 1, config_flags%spec_bdy_width, &
                       ierr )
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'U_BTYE'               , &  ! Data Name 
                       grid%u_btye(ims,kds,1)     , &  ! Field 
                       WRF_FLOAT             , &  ! FieldType 
                       grid%communicator , &  ! Comm
                       grid%iocommunicator , &  ! Comm
                       grid%domdesc      , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'YEZ'               , &  ! MemoryOrder
                       'X'               , &  ! Stagger
'inc/wrf_bdyin.inc ext_read_field U_BTYE memorder YEZ' , & ! Debug message
1, ide, kds, (kde-1), 1, config_flags%spec_bdy_width, &
ims, ime, kds, kde, 1, config_flags%spec_bdy_width, &
MAX(ims,ids), MIN(ime,ide), kds, (kde-1), 1, config_flags%spec_bdy_width, &
                       ierr )
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'V_BXS'               , &  ! Data Name 
                       grid%v_bxs(jms,kds,1)     , &  ! Field 
                       WRF_FLOAT             , &  ! FieldType 
                       grid%communicator , &  ! Comm
                       grid%iocommunicator , &  ! Comm
                       grid%domdesc      , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XSZ'               , &  ! MemoryOrder
                       'Y'               , &  ! Stagger
'inc/wrf_bdyin.inc ext_read_field V_BXS memorder XSZ' , & ! Debug message
1, jde, kds, (kde-1), 1, config_flags%spec_bdy_width, &
jms, jme, kds, kde, 1, config_flags%spec_bdy_width, &
MAX(jms,jds), MIN(jme,jde), kds, (kde-1), 1, config_flags%spec_bdy_width, &
                       ierr )
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'V_BXE'               , &  ! Data Name 
                       grid%v_bxe(jms,kds,1)     , &  ! Field 
                       WRF_FLOAT             , &  ! FieldType 
                       grid%communicator , &  ! Comm
                       grid%iocommunicator , &  ! Comm
                       grid%domdesc      , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XEZ'               , &  ! MemoryOrder
                       'Y'               , &  ! Stagger
'inc/wrf_bdyin.inc ext_read_field V_BXE memorder XEZ' , & ! Debug message
1, jde, kds, (kde-1), 1, config_flags%spec_bdy_width, &
jms, jme, kds, kde, 1, config_flags%spec_bdy_width, &
MAX(jms,jds), MIN(jme,jde), kds, (kde-1), 1, config_flags%spec_bdy_width, &
                       ierr )
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'V_BYS'               , &  ! Data Name 
                       grid%v_bys(ims,kds,1)     , &  ! Field 
                       WRF_FLOAT             , &  ! FieldType 
                       grid%communicator , &  ! Comm
                       grid%iocommunicator , &  ! Comm
                       grid%domdesc      , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'YSZ'               , &  ! MemoryOrder
                       'Y'               , &  ! Stagger
'inc/wrf_bdyin.inc ext_read_field V_BYS memorder YSZ' , & ! Debug message
1, (ide-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
ims, ime, kds, kde, 1, config_flags%spec_bdy_width, &
MAX(ims,ids), MIN(ime,(ide-1)), kds, (kde-1), 1, config_flags%spec_bdy_width, &
                       ierr )
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'V_BYE'               , &  ! Data Name 
                       grid%v_bye(ims,kds,1)     , &  ! Field 
                       WRF_FLOAT             , &  ! FieldType 
                       grid%communicator , &  ! Comm
                       grid%iocommunicator , &  ! Comm
                       grid%domdesc      , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'YEZ'               , &  ! MemoryOrder
                       'Y'               , &  ! Stagger
'inc/wrf_bdyin.inc ext_read_field V_BYE memorder YEZ' , & ! Debug message
1, (ide-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
ims, ime, kds, kde, 1, config_flags%spec_bdy_width, &
MAX(ims,ids), MIN(ime,(ide-1)), kds, (kde-1), 1, config_flags%spec_bdy_width, &
                       ierr )
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'V_BTXS'               , &  ! Data Name 
                       grid%v_btxs(jms,kds,1)     , &  ! Field 
                       WRF_FLOAT             , &  ! FieldType 
                       grid%communicator , &  ! Comm
                       grid%iocommunicator , &  ! Comm
                       grid%domdesc      , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XSZ'               , &  ! MemoryOrder
                       'Y'               , &  ! Stagger
'inc/wrf_bdyin.inc ext_read_field V_BTXS memorder XSZ' , & ! Debug message
1, jde, kds, (kde-1), 1, config_flags%spec_bdy_width, &
jms, jme, kds, kde, 1, config_flags%spec_bdy_width, &
MAX(jms,jds), MIN(jme,jde), kds, (kde-1), 1, config_flags%spec_bdy_width, &
                       ierr )
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'V_BTXE'               , &  ! Data Name 
                       grid%v_btxe(jms,kds,1)     , &  ! Field 
                       WRF_FLOAT             , &  ! FieldType 
                       grid%communicator , &  ! Comm
                       grid%iocommunicator , &  ! Comm
                       grid%domdesc      , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XEZ'               , &  ! MemoryOrder
                       'Y'               , &  ! Stagger
'inc/wrf_bdyin.inc ext_read_field V_BTXE memorder XEZ' , & ! Debug message
1, jde, kds, (kde-1), 1, config_flags%spec_bdy_width, &
jms, jme, kds, kde, 1, config_flags%spec_bdy_width, &
MAX(jms,jds), MIN(jme,jde), kds, (kde-1), 1, config_flags%spec_bdy_width, &
                       ierr )
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'V_BTYS'               , &  ! Data Name 
                       grid%v_btys(ims,kds,1)     , &  ! Field 
                       WRF_FLOAT             , &  ! FieldType 
                       grid%communicator , &  ! Comm
                       grid%iocommunicator , &  ! Comm
                       grid%domdesc      , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'YSZ'               , &  ! MemoryOrder
                       'Y'               , &  ! Stagger
'inc/wrf_bdyin.inc ext_read_field V_BTYS memorder YSZ' , & ! Debug message
1, (ide-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
ims, ime, kds, kde, 1, config_flags%spec_bdy_width, &
MAX(ims,ids), MIN(ime,(ide-1)), kds, (kde-1), 1, config_flags%spec_bdy_width, &
                       ierr )
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'V_BTYE'               , &  ! Data Name 
                       grid%v_btye(ims,kds,1)     , &  ! Field 
                       WRF_FLOAT             , &  ! FieldType 
                       grid%communicator , &  ! Comm
                       grid%iocommunicator , &  ! Comm
                       grid%domdesc      , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'YEZ'               , &  ! MemoryOrder
                       'Y'               , &  ! Stagger
'inc/wrf_bdyin.inc ext_read_field V_BTYE memorder YEZ' , & ! Debug message
1, (ide-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
ims, ime, kds, kde, 1, config_flags%spec_bdy_width, &
MAX(ims,ids), MIN(ime,(ide-1)), kds, (kde-1), 1, config_flags%spec_bdy_width, &
                       ierr )
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'W_BXS'               , &  ! Data Name 
                       grid%w_bxs(jms,kds,1)     , &  ! Field 
                       WRF_FLOAT             , &  ! FieldType 
                       grid%communicator , &  ! Comm
                       grid%iocommunicator , &  ! Comm
                       grid%domdesc      , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XSZ'               , &  ! MemoryOrder
                       'Z'               , &  ! Stagger
'inc/wrf_bdyin.inc ext_read_field W_BXS memorder XSZ' , & ! Debug message
1, (jde-1), kds, kde, 1, config_flags%spec_bdy_width, &
jms, jme, kds, kde, 1, config_flags%spec_bdy_width, &
MAX(jms,jds), MIN(jme,(jde-1)), kds, kde, 1, config_flags%spec_bdy_width, &
                       ierr )
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'W_BXE'               , &  ! Data Name 
                       grid%w_bxe(jms,kds,1)     , &  ! Field 
                       WRF_FLOAT             , &  ! FieldType 
                       grid%communicator , &  ! Comm
                       grid%iocommunicator , &  ! Comm
                       grid%domdesc      , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XEZ'               , &  ! MemoryOrder
                       'Z'               , &  ! Stagger
'inc/wrf_bdyin.inc ext_read_field W_BXE memorder XEZ' , & ! Debug message
1, (jde-1), kds, kde, 1, config_flags%spec_bdy_width, &
jms, jme, kds, kde, 1, config_flags%spec_bdy_width, &
MAX(jms,jds), MIN(jme,(jde-1)), kds, kde, 1, config_flags%spec_bdy_width, &
                       ierr )
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'W_BYS'               , &  ! Data Name 
                       grid%w_bys(ims,kds,1)     , &  ! Field 
                       WRF_FLOAT             , &  ! FieldType 
                       grid%communicator , &  ! Comm
                       grid%iocommunicator , &  ! Comm
                       grid%domdesc      , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'YSZ'               , &  ! MemoryOrder
                       'Z'               , &  ! Stagger
'inc/wrf_bdyin.inc ext_read_field W_BYS memorder YSZ' , & ! Debug message
1, (ide-1), kds, kde, 1, config_flags%spec_bdy_width, &
ims, ime, kds, kde, 1, config_flags%spec_bdy_width, &
MAX(ims,ids), MIN(ime,(ide-1)), kds, kde, 1, config_flags%spec_bdy_width, &
                       ierr )
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'W_BYE'               , &  ! Data Name 
                       grid%w_bye(ims,kds,1)     , &  ! Field 
                       WRF_FLOAT             , &  ! FieldType 
                       grid%communicator , &  ! Comm
                       grid%iocommunicator , &  ! Comm
                       grid%domdesc      , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'YEZ'               , &  ! MemoryOrder
                       'Z'               , &  ! Stagger
'inc/wrf_bdyin.inc ext_read_field W_BYE memorder YEZ' , & ! Debug message
1, (ide-1), kds, kde, 1, config_flags%spec_bdy_width, &
ims, ime, kds, kde, 1, config_flags%spec_bdy_width, &
MAX(ims,ids), MIN(ime,(ide-1)), kds, kde, 1, config_flags%spec_bdy_width, &
                       ierr )
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'W_BTXS'               , &  ! Data Name 
                       grid%w_btxs(jms,kds,1)     , &  ! Field 
                       WRF_FLOAT             , &  ! FieldType 
                       grid%communicator , &  ! Comm
                       grid%iocommunicator , &  ! Comm
                       grid%domdesc      , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XSZ'               , &  ! MemoryOrder
                       'Z'               , &  ! Stagger
'inc/wrf_bdyin.inc ext_read_field W_BTXS memorder XSZ' , & ! Debug message
1, (jde-1), kds, kde, 1, config_flags%spec_bdy_width, &
jms, jme, kds, kde, 1, config_flags%spec_bdy_width, &
MAX(jms,jds), MIN(jme,(jde-1)), kds, kde, 1, config_flags%spec_bdy_width, &
                       ierr )
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'W_BTXE'               , &  ! Data Name 
                       grid%w_btxe(jms,kds,1)     , &  ! Field 
                       WRF_FLOAT             , &  ! FieldType 
                       grid%communicator , &  ! Comm
                       grid%iocommunicator , &  ! Comm
                       grid%domdesc      , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XEZ'               , &  ! MemoryOrder
                       'Z'               , &  ! Stagger
'inc/wrf_bdyin.inc ext_read_field W_BTXE memorder XEZ' , & ! Debug message
1, (jde-1), kds, kde, 1, config_flags%spec_bdy_width, &
jms, jme, kds, kde, 1, config_flags%spec_bdy_width, &
MAX(jms,jds), MIN(jme,(jde-1)), kds, kde, 1, config_flags%spec_bdy_width, &
                       ierr )
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'W_BTYS'               , &  ! Data Name 
                       grid%w_btys(ims,kds,1)     , &  ! Field 
                       WRF_FLOAT             , &  ! FieldType 
                       grid%communicator , &  ! Comm
                       grid%iocommunicator , &  ! Comm
                       grid%domdesc      , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'YSZ'               , &  ! MemoryOrder
                       'Z'               , &  ! Stagger
'inc/wrf_bdyin.inc ext_read_field W_BTYS memorder YSZ' , & ! Debug message
1, (ide-1), kds, kde, 1, config_flags%spec_bdy_width, &
ims, ime, kds, kde, 1, config_flags%spec_bdy_width, &
MAX(ims,ids), MIN(ime,(ide-1)), kds, kde, 1, config_flags%spec_bdy_width, &
                       ierr )
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'W_BTYE'               , &  ! Data Name 
                       grid%w_btye(ims,kds,1)     , &  ! Field 
                       WRF_FLOAT             , &  ! FieldType 
                       grid%communicator , &  ! Comm
                       grid%iocommunicator , &  ! Comm
                       grid%domdesc      , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'YEZ'               , &  ! MemoryOrder
                       'Z'               , &  ! Stagger
'inc/wrf_bdyin.inc ext_read_field W_BTYE memorder YEZ' , & ! Debug message
1, (ide-1), kds, kde, 1, config_flags%spec_bdy_width, &
ims, ime, kds, kde, 1, config_flags%spec_bdy_width, &
MAX(ims,ids), MIN(ime,(ide-1)), kds, kde, 1, config_flags%spec_bdy_width, &
                       ierr )
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'PH_BXS'               , &  ! Data Name 
                       grid%ph_bxs(jms,kds,1)     , &  ! Field 
                       WRF_FLOAT             , &  ! FieldType 
                       grid%communicator , &  ! Comm
                       grid%iocommunicator , &  ! Comm
                       grid%domdesc      , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XSZ'               , &  ! MemoryOrder
                       'Z'               , &  ! Stagger
'inc/wrf_bdyin.inc ext_read_field PH_BXS memorder XSZ' , & ! Debug message
1, (jde-1), kds, kde, 1, config_flags%spec_bdy_width, &
jms, jme, kds, kde, 1, config_flags%spec_bdy_width, &
MAX(jms,jds), MIN(jme,(jde-1)), kds, kde, 1, config_flags%spec_bdy_width, &
                       ierr )
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'PH_BXE'               , &  ! Data Name 
                       grid%ph_bxe(jms,kds,1)     , &  ! Field 
                       WRF_FLOAT             , &  ! FieldType 
                       grid%communicator , &  ! Comm
                       grid%iocommunicator , &  ! Comm
                       grid%domdesc      , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XEZ'               , &  ! MemoryOrder
                       'Z'               , &  ! Stagger
'inc/wrf_bdyin.inc ext_read_field PH_BXE memorder XEZ' , & ! Debug message
1, (jde-1), kds, kde, 1, config_flags%spec_bdy_width, &
jms, jme, kds, kde, 1, config_flags%spec_bdy_width, &
MAX(jms,jds), MIN(jme,(jde-1)), kds, kde, 1, config_flags%spec_bdy_width, &
                       ierr )
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'PH_BYS'               , &  ! Data Name 
                       grid%ph_bys(ims,kds,1)     , &  ! Field 
                       WRF_FLOAT             , &  ! FieldType 
                       grid%communicator , &  ! Comm
                       grid%iocommunicator , &  ! Comm
                       grid%domdesc      , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'YSZ'               , &  ! MemoryOrder
                       'Z'               , &  ! Stagger
'inc/wrf_bdyin.inc ext_read_field PH_BYS memorder YSZ' , & ! Debug message
1, (ide-1), kds, kde, 1, config_flags%spec_bdy_width, &
ims, ime, kds, kde, 1, config_flags%spec_bdy_width, &
MAX(ims,ids), MIN(ime,(ide-1)), kds, kde, 1, config_flags%spec_bdy_width, &
                       ierr )
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'PH_BYE'               , &  ! Data Name 
                       grid%ph_bye(ims,kds,1)     , &  ! Field 
                       WRF_FLOAT             , &  ! FieldType 
                       grid%communicator , &  ! Comm
                       grid%iocommunicator , &  ! Comm
                       grid%domdesc      , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'YEZ'               , &  ! MemoryOrder
                       'Z'               , &  ! Stagger
'inc/wrf_bdyin.inc ext_read_field PH_BYE memorder YEZ' , & ! Debug message
1, (ide-1), kds, kde, 1, config_flags%spec_bdy_width, &
ims, ime, kds, kde, 1, config_flags%spec_bdy_width, &
MAX(ims,ids), MIN(ime,(ide-1)), kds, kde, 1, config_flags%spec_bdy_width, &
                       ierr )
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'PH_BTXS'               , &  ! Data Name 
                       grid%ph_btxs(jms,kds,1)     , &  ! Field 
                       WRF_FLOAT             , &  ! FieldType 
                       grid%communicator , &  ! Comm
                       grid%iocommunicator , &  ! Comm
                       grid%domdesc      , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XSZ'               , &  ! MemoryOrder
                       'Z'               , &  ! Stagger
'inc/wrf_bdyin.inc ext_read_field PH_BTXS memorder XSZ' , & ! Debug message
1, (jde-1), kds, kde, 1, config_flags%spec_bdy_width, &
jms, jme, kds, kde, 1, config_flags%spec_bdy_width, &
MAX(jms,jds), MIN(jme,(jde-1)), kds, kde, 1, config_flags%spec_bdy_width, &
                       ierr )
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'PH_BTXE'               , &  ! Data Name 
                       grid%ph_btxe(jms,kds,1)     , &  ! Field 
                       WRF_FLOAT             , &  ! FieldType 
                       grid%communicator , &  ! Comm
                       grid%iocommunicator , &  ! Comm
                       grid%domdesc      , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XEZ'               , &  ! MemoryOrder
                       'Z'               , &  ! Stagger
'inc/wrf_bdyin.inc ext_read_field PH_BTXE memorder XEZ' , & ! Debug message
1, (jde-1), kds, kde, 1, config_flags%spec_bdy_width, &
jms, jme, kds, kde, 1, config_flags%spec_bdy_width, &
MAX(jms,jds), MIN(jme,(jde-1)), kds, kde, 1, config_flags%spec_bdy_width, &
                       ierr )
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'PH_BTYS'               , &  ! Data Name 
                       grid%ph_btys(ims,kds,1)     , &  ! Field 
                       WRF_FLOAT             , &  ! FieldType 
                       grid%communicator , &  ! Comm
                       grid%iocommunicator , &  ! Comm
                       grid%domdesc      , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'YSZ'               , &  ! MemoryOrder
                       'Z'               , &  ! Stagger
'inc/wrf_bdyin.inc ext_read_field PH_BTYS memorder YSZ' , & ! Debug message
1, (ide-1), kds, kde, 1, config_flags%spec_bdy_width, &
ims, ime, kds, kde, 1, config_flags%spec_bdy_width, &
MAX(ims,ids), MIN(ime,(ide-1)), kds, kde, 1, config_flags%spec_bdy_width, &
                       ierr )
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'PH_BTYE'               , &  ! Data Name 
                       grid%ph_btye(ims,kds,1)     , &  ! Field 
                       WRF_FLOAT             , &  ! FieldType 
                       grid%communicator , &  ! Comm
                       grid%iocommunicator , &  ! Comm
                       grid%domdesc      , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'YEZ'               , &  ! MemoryOrder
                       'Z'               , &  ! Stagger
'inc/wrf_bdyin.inc ext_read_field PH_BTYE memorder YEZ' , & ! Debug message
1, (ide-1), kds, kde, 1, config_flags%spec_bdy_width, &
ims, ime, kds, kde, 1, config_flags%spec_bdy_width, &
MAX(ims,ids), MIN(ime,(ide-1)), kds, kde, 1, config_flags%spec_bdy_width, &
                       ierr )
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'T_BXS'               , &  ! Data Name 
                       grid%t_bxs(jms,kds,1)     , &  ! Field 
                       WRF_FLOAT             , &  ! FieldType 
                       grid%communicator , &  ! Comm
                       grid%iocommunicator , &  ! Comm
                       grid%domdesc      , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XSZ'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_bdyin.inc ext_read_field T_BXS memorder XSZ' , & ! Debug message
1, (jde-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
jms, jme, kds, kde, 1, config_flags%spec_bdy_width, &
MAX(jms,jds), MIN(jme,(jde-1)), kds, (kde-1), 1, config_flags%spec_bdy_width, &
                       ierr )
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'T_BXE'               , &  ! Data Name 
                       grid%t_bxe(jms,kds,1)     , &  ! Field 
                       WRF_FLOAT             , &  ! FieldType 
                       grid%communicator , &  ! Comm
                       grid%iocommunicator , &  ! Comm
                       grid%domdesc      , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XEZ'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_bdyin.inc ext_read_field T_BXE memorder XEZ' , & ! Debug message
1, (jde-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
jms, jme, kds, kde, 1, config_flags%spec_bdy_width, &
MAX(jms,jds), MIN(jme,(jde-1)), kds, (kde-1), 1, config_flags%spec_bdy_width, &
                       ierr )
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'T_BYS'               , &  ! Data Name 
                       grid%t_bys(ims,kds,1)     , &  ! Field 
                       WRF_FLOAT             , &  ! FieldType 
                       grid%communicator , &  ! Comm
                       grid%iocommunicator , &  ! Comm
                       grid%domdesc      , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'YSZ'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_bdyin.inc ext_read_field T_BYS memorder YSZ' , & ! Debug message
1, (ide-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
ims, ime, kds, kde, 1, config_flags%spec_bdy_width, &
MAX(ims,ids), MIN(ime,(ide-1)), kds, (kde-1), 1, config_flags%spec_bdy_width, &
                       ierr )
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'T_BYE'               , &  ! Data Name 
                       grid%t_bye(ims,kds,1)     , &  ! Field 
                       WRF_FLOAT             , &  ! FieldType 
                       grid%communicator , &  ! Comm
                       grid%iocommunicator , &  ! Comm
                       grid%domdesc      , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'YEZ'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_bdyin.inc ext_read_field T_BYE memorder YEZ' , & ! Debug message
1, (ide-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
ims, ime, kds, kde, 1, config_flags%spec_bdy_width, &
MAX(ims,ids), MIN(ime,(ide-1)), kds, (kde-1), 1, config_flags%spec_bdy_width, &
                       ierr )
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'T_BTXS'               , &  ! Data Name 
                       grid%t_btxs(jms,kds,1)     , &  ! Field 
                       WRF_FLOAT             , &  ! FieldType 
                       grid%communicator , &  ! Comm
                       grid%iocommunicator , &  ! Comm
                       grid%domdesc      , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XSZ'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_bdyin.inc ext_read_field T_BTXS memorder XSZ' , & ! Debug message
1, (jde-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
jms, jme, kds, kde, 1, config_flags%spec_bdy_width, &
MAX(jms,jds), MIN(jme,(jde-1)), kds, (kde-1), 1, config_flags%spec_bdy_width, &
                       ierr )
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'T_BTXE'               , &  ! Data Name 
                       grid%t_btxe(jms,kds,1)     , &  ! Field 
                       WRF_FLOAT             , &  ! FieldType 
                       grid%communicator , &  ! Comm
                       grid%iocommunicator , &  ! Comm
                       grid%domdesc      , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XEZ'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_bdyin.inc ext_read_field T_BTXE memorder XEZ' , & ! Debug message
1, (jde-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
jms, jme, kds, kde, 1, config_flags%spec_bdy_width, &
MAX(jms,jds), MIN(jme,(jde-1)), kds, (kde-1), 1, config_flags%spec_bdy_width, &
                       ierr )
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'T_BTYS'               , &  ! Data Name 
                       grid%t_btys(ims,kds,1)     , &  ! Field 
                       WRF_FLOAT             , &  ! FieldType 
                       grid%communicator , &  ! Comm
                       grid%iocommunicator , &  ! Comm
                       grid%domdesc      , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'YSZ'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_bdyin.inc ext_read_field T_BTYS memorder YSZ' , & ! Debug message
1, (ide-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
ims, ime, kds, kde, 1, config_flags%spec_bdy_width, &
MAX(ims,ids), MIN(ime,(ide-1)), kds, (kde-1), 1, config_flags%spec_bdy_width, &
                       ierr )
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'T_BTYE'               , &  ! Data Name 
                       grid%t_btye(ims,kds,1)     , &  ! Field 
                       WRF_FLOAT             , &  ! FieldType 
                       grid%communicator , &  ! Comm
                       grid%iocommunicator , &  ! Comm
                       grid%domdesc      , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'YEZ'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_bdyin.inc ext_read_field T_BTYE memorder YEZ' , & ! Debug message
1, (ide-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
ims, ime, kds, kde, 1, config_flags%spec_bdy_width, &
MAX(ims,ids), MIN(ime,(ide-1)), kds, (kde-1), 1, config_flags%spec_bdy_width, &
                       ierr )
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'MU_BXS'               , &  ! Data Name 
                       grid%mu_bxs(jms,kds,1)     , &  ! Field 
                       WRF_FLOAT             , &  ! FieldType 
                       grid%communicator , &  ! Comm
                       grid%iocommunicator , &  ! Comm
                       grid%domdesc      , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XS'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_bdyin.inc ext_read_field MU_BXS memorder XS' , & ! Debug message
1, (jde-1), 1, config_flags%spec_bdy_width, 1, 1, &
jms, jme, 1, config_flags%spec_bdy_width, 1, 1, &
MAX(jms,jds), MIN(jme,(jde-1)), 1, config_flags%spec_bdy_width, 1, 1, &
                       ierr )
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'MU_BXE'               , &  ! Data Name 
                       grid%mu_bxe(jms,kds,1)     , &  ! Field 
                       WRF_FLOAT             , &  ! FieldType 
                       grid%communicator , &  ! Comm
                       grid%iocommunicator , &  ! Comm
                       grid%domdesc      , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XE'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_bdyin.inc ext_read_field MU_BXE memorder XE' , & ! Debug message
1, (jde-1), 1, config_flags%spec_bdy_width, 1, 1, &
jms, jme, 1, config_flags%spec_bdy_width, 1, 1, &
MAX(jms,jds), MIN(jme,(jde-1)), 1, config_flags%spec_bdy_width, 1, 1, &
                       ierr )
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'MU_BYS'               , &  ! Data Name 
                       grid%mu_bys(ims,kds,1)     , &  ! Field 
                       WRF_FLOAT             , &  ! FieldType 
                       grid%communicator , &  ! Comm
                       grid%iocommunicator , &  ! Comm
                       grid%domdesc      , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'YS'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_bdyin.inc ext_read_field MU_BYS memorder YS' , & ! Debug message
1, (ide-1), 1, config_flags%spec_bdy_width, 1, 1, &
ims, ime, 1, config_flags%spec_bdy_width, 1, 1, &
MAX(ims,ids), MIN(ime,(ide-1)), 1, config_flags%spec_bdy_width, 1, 1, &
                       ierr )
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'MU_BYE'               , &  ! Data Name 
                       grid%mu_bye(ims,kds,1)     , &  ! Field 
                       WRF_FLOAT             , &  ! FieldType 
                       grid%communicator , &  ! Comm
                       grid%iocommunicator , &  ! Comm
                       grid%domdesc      , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'YE'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_bdyin.inc ext_read_field MU_BYE memorder YE' , & ! Debug message
1, (ide-1), 1, config_flags%spec_bdy_width, 1, 1, &
ims, ime, 1, config_flags%spec_bdy_width, 1, 1, &
MAX(ims,ids), MIN(ime,(ide-1)), 1, config_flags%spec_bdy_width, 1, 1, &
                       ierr )
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'MU_BTXS'               , &  ! Data Name 
                       grid%mu_btxs(jms,kds,1)     , &  ! Field 
                       WRF_FLOAT             , &  ! FieldType 
                       grid%communicator , &  ! Comm
                       grid%iocommunicator , &  ! Comm
                       grid%domdesc      , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XS'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_bdyin.inc ext_read_field MU_BTXS memorder XS' , & ! Debug message
1, (jde-1), 1, config_flags%spec_bdy_width, 1, 1, &
jms, jme, 1, config_flags%spec_bdy_width, 1, 1, &
MAX(jms,jds), MIN(jme,(jde-1)), 1, config_flags%spec_bdy_width, 1, 1, &
                       ierr )
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'MU_BTXE'               , &  ! Data Name 
                       grid%mu_btxe(jms,kds,1)     , &  ! Field 
                       WRF_FLOAT             , &  ! FieldType 
                       grid%communicator , &  ! Comm
                       grid%iocommunicator , &  ! Comm
                       grid%domdesc      , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XE'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_bdyin.inc ext_read_field MU_BTXE memorder XE' , & ! Debug message
1, (jde-1), 1, config_flags%spec_bdy_width, 1, 1, &
jms, jme, 1, config_flags%spec_bdy_width, 1, 1, &
MAX(jms,jds), MIN(jme,(jde-1)), 1, config_flags%spec_bdy_width, 1, 1, &
                       ierr )
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'MU_BTYS'               , &  ! Data Name 
                       grid%mu_btys(ims,kds,1)     , &  ! Field 
                       WRF_FLOAT             , &  ! FieldType 
                       grid%communicator , &  ! Comm
                       grid%iocommunicator , &  ! Comm
                       grid%domdesc      , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'YS'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_bdyin.inc ext_read_field MU_BTYS memorder YS' , & ! Debug message
1, (ide-1), 1, config_flags%spec_bdy_width, 1, 1, &
ims, ime, 1, config_flags%spec_bdy_width, 1, 1, &
MAX(ims,ids), MIN(ime,(ide-1)), 1, config_flags%spec_bdy_width, 1, 1, &
                       ierr )
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'MU_BTYE'               , &  ! Data Name 
                       grid%mu_btye(ims,kds,1)     , &  ! Field 
                       WRF_FLOAT             , &  ! FieldType 
                       grid%communicator , &  ! Comm
                       grid%iocommunicator , &  ! Comm
                       grid%domdesc      , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'YE'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_bdyin.inc ext_read_field MU_BTYE memorder YE' , & ! Debug message
1, (ide-1), 1, config_flags%spec_bdy_width, 1, 1, &
ims, ime, 1, config_flags%spec_bdy_width, 1, 1, &
MAX(ims,ids), MIN(ime,(ide-1)), 1, config_flags%spec_bdy_width, 1, 1, &
                       ierr )
DO itrace = PARAM_FIRST_SCALAR , num_moist
  IF ( moist_boundary_table(grid%id, itrace ) ) THEN
    CALL wrf_ext_read_field (  &
          fid                             , &  ! DataHandle
          current_date(1:19)              , &  ! DateStr
          TRIM(moist_dname_table( grid%id, itrace )) // '_BXS', & !data name
          grid%moist_BXS(jms,kds,1,itrace)  , &  ! Field
                       WRF_FLOAT             , &  ! FieldType 
          grid%communicator  , &  ! Comm
          grid%iocommunicator  , &  ! Comm
          grid%domdesc       , &  ! Comm
          grid%bdy_mask       , &  ! bdy_mask
          'XSZ'               , &  ! MemoryOrder
          ''                , &  ! Stagger
'inc/wrf_bdyin.inc ext_write_field '//TRIM(moist_dname_table( grid%id, itrace ))//' memorder XSZ' , & ! Debug message
1, (jde-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
jms, jme, kds, kde, 1, config_flags%spec_bdy_width, &
MAX(jms,jds), MIN(jme,(jde-1)), kds, (kde-1), 1, config_flags%spec_bdy_width, &
                         ierr )
    CALL wrf_ext_read_field (  &
          fid                             , &  ! DataHandle
          current_date(1:19)              , &  ! DateStr
          TRIM(moist_dname_table( grid%id, itrace )) // '_BXE', & !data name
          grid%moist_BXE(jms,kds,1,itrace)  , &  ! Field
                       WRF_FLOAT             , &  ! FieldType 
          grid%communicator  , &  ! Comm
          grid%iocommunicator  , &  ! Comm
          grid%domdesc       , &  ! Comm
          grid%bdy_mask       , &  ! bdy_mask
          'XEZ'               , &  ! MemoryOrder
          ''                , &  ! Stagger
'inc/wrf_bdyin.inc ext_write_field '//TRIM(moist_dname_table( grid%id, itrace ))//' memorder XEZ' , & ! Debug message
1, (jde-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
jms, jme, kds, kde, 1, config_flags%spec_bdy_width, &
MAX(jms,jds), MIN(jme,(jde-1)), kds, (kde-1), 1, config_flags%spec_bdy_width, &
                         ierr )
    CALL wrf_ext_read_field (  &
          fid                             , &  ! DataHandle
          current_date(1:19)              , &  ! DateStr
          TRIM(moist_dname_table( grid%id, itrace )) // '_BYS', & !data name
          grid%moist_BYS(ims,kds,1,itrace)  , &  ! Field
                       WRF_FLOAT             , &  ! FieldType 
          grid%communicator  , &  ! Comm
          grid%iocommunicator  , &  ! Comm
          grid%domdesc       , &  ! Comm
          grid%bdy_mask       , &  ! bdy_mask
          'YSZ'               , &  ! MemoryOrder
          ''                , &  ! Stagger
'inc/wrf_bdyin.inc ext_write_field '//TRIM(moist_dname_table( grid%id, itrace ))//' memorder YSZ' , & ! Debug message
1, (ide-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
ims, ime, kds, kde, 1, config_flags%spec_bdy_width, &
MAX(ims,ids), MIN(ime,(ide-1)), kds, (kde-1), 1, config_flags%spec_bdy_width, &
                         ierr )
    CALL wrf_ext_read_field (  &
          fid                             , &  ! DataHandle
          current_date(1:19)              , &  ! DateStr
          TRIM(moist_dname_table( grid%id, itrace )) // '_BYE', & !data name
          grid%moist_BYE(ims,kds,1,itrace)  , &  ! Field
                       WRF_FLOAT             , &  ! FieldType 
          grid%communicator  , &  ! Comm
          grid%iocommunicator  , &  ! Comm
          grid%domdesc       , &  ! Comm
          grid%bdy_mask       , &  ! bdy_mask
          'YEZ'               , &  ! MemoryOrder
          ''                , &  ! Stagger
'inc/wrf_bdyin.inc ext_write_field '//TRIM(moist_dname_table( grid%id, itrace ))//' memorder YEZ' , & ! Debug message
1, (ide-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
ims, ime, kds, kde, 1, config_flags%spec_bdy_width, &
MAX(ims,ids), MIN(ime,(ide-1)), kds, (kde-1), 1, config_flags%spec_bdy_width, &
                         ierr )
  ENDIF
ENDDO
DO itrace = PARAM_FIRST_SCALAR , num_moist
  IF ( moist_boundary_table(grid%id, itrace ) ) THEN
    CALL wrf_ext_read_field (  &
          fid                             , &  ! DataHandle
          current_date(1:19)              , &  ! DateStr
          TRIM(moist_dname_table( grid%id, itrace )) // '_BTXS', & !data name
          grid%moist_BTXS(jms,kds,1,itrace)  , &  ! Field
                       WRF_FLOAT             , &  ! FieldType 
          grid%communicator  , &  ! Comm
          grid%iocommunicator  , &  ! Comm
          grid%domdesc       , &  ! Comm
          grid%bdy_mask       , &  ! bdy_mask
          'XSZ'               , &  ! MemoryOrder
          ''                , &  ! Stagger
'inc/wrf_bdyin.inc ext_write_field '//TRIM(moist_dname_table( grid%id, itrace ))//' memorder XSZ' , & ! Debug message
1, (jde-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
jms, jme, kds, kde, 1, config_flags%spec_bdy_width, &
MAX(jms,jds), MIN(jme,(jde-1)), kds, (kde-1), 1, config_flags%spec_bdy_width, &
                         ierr )
    CALL wrf_ext_read_field (  &
          fid                             , &  ! DataHandle
          current_date(1:19)              , &  ! DateStr
          TRIM(moist_dname_table( grid%id, itrace )) // '_BTXE', & !data name
          grid%moist_BTXE(jms,kds,1,itrace)  , &  ! Field
                       WRF_FLOAT             , &  ! FieldType 
          grid%communicator  , &  ! Comm
          grid%iocommunicator  , &  ! Comm
          grid%domdesc       , &  ! Comm
          grid%bdy_mask       , &  ! bdy_mask
          'XEZ'               , &  ! MemoryOrder
          ''                , &  ! Stagger
'inc/wrf_bdyin.inc ext_write_field '//TRIM(moist_dname_table( grid%id, itrace ))//' memorder XEZ' , & ! Debug message
1, (jde-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
jms, jme, kds, kde, 1, config_flags%spec_bdy_width, &
MAX(jms,jds), MIN(jme,(jde-1)), kds, (kde-1), 1, config_flags%spec_bdy_width, &
                         ierr )
    CALL wrf_ext_read_field (  &
          fid                             , &  ! DataHandle
          current_date(1:19)              , &  ! DateStr
          TRIM(moist_dname_table( grid%id, itrace )) // '_BTYS', & !data name
          grid%moist_BTYS(ims,kds,1,itrace)  , &  ! Field
                       WRF_FLOAT             , &  ! FieldType 
          grid%communicator  , &  ! Comm
          grid%iocommunicator  , &  ! Comm
          grid%domdesc       , &  ! Comm
          grid%bdy_mask       , &  ! bdy_mask
          'YSZ'               , &  ! MemoryOrder
          ''                , &  ! Stagger
'inc/wrf_bdyin.inc ext_write_field '//TRIM(moist_dname_table( grid%id, itrace ))//' memorder YSZ' , & ! Debug message
1, (ide-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
ims, ime, kds, kde, 1, config_flags%spec_bdy_width, &
MAX(ims,ids), MIN(ime,(ide-1)), kds, (kde-1), 1, config_flags%spec_bdy_width, &
                         ierr )
    CALL wrf_ext_read_field (  &
          fid                             , &  ! DataHandle
          current_date(1:19)              , &  ! DateStr
          TRIM(moist_dname_table( grid%id, itrace )) // '_BTYE', & !data name
          grid%moist_BTYE(ims,kds,1,itrace)  , &  ! Field
                       WRF_FLOAT             , &  ! FieldType 
          grid%communicator  , &  ! Comm
          grid%iocommunicator  , &  ! Comm
          grid%domdesc       , &  ! Comm
          grid%bdy_mask       , &  ! bdy_mask
          'YEZ'               , &  ! MemoryOrder
          ''                , &  ! Stagger
'inc/wrf_bdyin.inc ext_write_field '//TRIM(moist_dname_table( grid%id, itrace ))//' memorder YEZ' , & ! Debug message
1, (ide-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
ims, ime, kds, kde, 1, config_flags%spec_bdy_width, &
MAX(ims,ids), MIN(ime,(ide-1)), kds, (kde-1), 1, config_flags%spec_bdy_width, &
                         ierr )
  ENDIF
ENDDO
DO itrace = PARAM_FIRST_SCALAR , num_dfi_moist
  IF ( dfi_moist_boundary_table(grid%id, itrace ) ) THEN
    CALL wrf_ext_read_field (  &
          fid                             , &  ! DataHandle
          current_date(1:19)              , &  ! DateStr
          TRIM(dfi_moist_dname_table( grid%id, itrace )) // '_BXS', & !data name
          grid%dfi_moist_BXS(jms,kds,1,itrace)  , &  ! Field
                       WRF_FLOAT             , &  ! FieldType 
          grid%communicator  , &  ! Comm
          grid%iocommunicator  , &  ! Comm
          grid%domdesc       , &  ! Comm
          grid%bdy_mask       , &  ! bdy_mask
          'XSZ'               , &  ! MemoryOrder
          ''                , &  ! Stagger
'inc/wrf_bdyin.inc ext_write_field '//TRIM(dfi_moist_dname_table( grid%id, itrace ))//' memorder XSZ' , & ! Debug message
1, (jde-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
jms, jme, kds, kde, 1, config_flags%spec_bdy_width, &
MAX(jms,jds), MIN(jme,(jde-1)), kds, (kde-1), 1, config_flags%spec_bdy_width, &
                         ierr )
    CALL wrf_ext_read_field (  &
          fid                             , &  ! DataHandle
          current_date(1:19)              , &  ! DateStr
          TRIM(dfi_moist_dname_table( grid%id, itrace )) // '_BXE', & !data name
          grid%dfi_moist_BXE(jms,kds,1,itrace)  , &  ! Field
                       WRF_FLOAT             , &  ! FieldType 
          grid%communicator  , &  ! Comm
          grid%iocommunicator  , &  ! Comm
          grid%domdesc       , &  ! Comm
          grid%bdy_mask       , &  ! bdy_mask
          'XEZ'               , &  ! MemoryOrder
          ''                , &  ! Stagger
'inc/wrf_bdyin.inc ext_write_field '//TRIM(dfi_moist_dname_table( grid%id, itrace ))//' memorder XEZ' , & ! Debug message
1, (jde-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
jms, jme, kds, kde, 1, config_flags%spec_bdy_width, &
MAX(jms,jds), MIN(jme,(jde-1)), kds, (kde-1), 1, config_flags%spec_bdy_width, &
                         ierr )
    CALL wrf_ext_read_field (  &
          fid                             , &  ! DataHandle
          current_date(1:19)              , &  ! DateStr
          TRIM(dfi_moist_dname_table( grid%id, itrace )) // '_BYS', & !data name
          grid%dfi_moist_BYS(ims,kds,1,itrace)  , &  ! Field
                       WRF_FLOAT             , &  ! FieldType 
          grid%communicator  , &  ! Comm
          grid%iocommunicator  , &  ! Comm
          grid%domdesc       , &  ! Comm
          grid%bdy_mask       , &  ! bdy_mask
          'YSZ'               , &  ! MemoryOrder
          ''                , &  ! Stagger
'inc/wrf_bdyin.inc ext_write_field '//TRIM(dfi_moist_dname_table( grid%id, itrace ))//' memorder YSZ' , & ! Debug message
1, (ide-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
ims, ime, kds, kde, 1, config_flags%spec_bdy_width, &
MAX(ims,ids), MIN(ime,(ide-1)), kds, (kde-1), 1, config_flags%spec_bdy_width, &
                         ierr )
    CALL wrf_ext_read_field (  &
          fid                             , &  ! DataHandle
          current_date(1:19)              , &  ! DateStr
          TRIM(dfi_moist_dname_table( grid%id, itrace )) // '_BYE', & !data name
          grid%dfi_moist_BYE(ims,kds,1,itrace)  , &  ! Field
                       WRF_FLOAT             , &  ! FieldType 
          grid%communicator  , &  ! Comm
          grid%iocommunicator  , &  ! Comm
          grid%domdesc       , &  ! Comm
          grid%bdy_mask       , &  ! bdy_mask
          'YEZ'               , &  ! MemoryOrder
          ''                , &  ! Stagger
'inc/wrf_bdyin.inc ext_write_field '//TRIM(dfi_moist_dname_table( grid%id, itrace ))//' memorder YEZ' , & ! Debug message
1, (ide-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
ims, ime, kds, kde, 1, config_flags%spec_bdy_width, &
MAX(ims,ids), MIN(ime,(ide-1)), kds, (kde-1), 1, config_flags%spec_bdy_width, &
                         ierr )
  ENDIF
ENDDO
DO itrace = PARAM_FIRST_SCALAR , num_dfi_moist
  IF ( dfi_moist_boundary_table(grid%id, itrace ) ) THEN
    CALL wrf_ext_read_field (  &
          fid                             , &  ! DataHandle
          current_date(1:19)              , &  ! DateStr
          TRIM(dfi_moist_dname_table( grid%id, itrace )) // '_BTXS', & !data name
          grid%dfi_moist_BTXS(jms,kds,1,itrace)  , &  ! Field
                       WRF_FLOAT             , &  ! FieldType 
          grid%communicator  , &  ! Comm
          grid%iocommunicator  , &  ! Comm
          grid%domdesc       , &  ! Comm
          grid%bdy_mask       , &  ! bdy_mask
          'XSZ'               , &  ! MemoryOrder
          ''                , &  ! Stagger
'inc/wrf_bdyin.inc ext_write_field '//TRIM(dfi_moist_dname_table( grid%id, itrace ))//' memorder XSZ' , & ! Debug message
1, (jde-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
jms, jme, kds, kde, 1, config_flags%spec_bdy_width, &
MAX(jms,jds), MIN(jme,(jde-1)), kds, (kde-1), 1, config_flags%spec_bdy_width, &
                         ierr )
    CALL wrf_ext_read_field (  &
          fid                             , &  ! DataHandle
          current_date(1:19)              , &  ! DateStr
          TRIM(dfi_moist_dname_table( grid%id, itrace )) // '_BTXE', & !data name
          grid%dfi_moist_BTXE(jms,kds,1,itrace)  , &  ! Field
                       WRF_FLOAT             , &  ! FieldType 
          grid%communicator  , &  ! Comm
          grid%iocommunicator  , &  ! Comm
          grid%domdesc       , &  ! Comm
          grid%bdy_mask       , &  ! bdy_mask
          'XEZ'               , &  ! MemoryOrder
          ''                , &  ! Stagger
'inc/wrf_bdyin.inc ext_write_field '//TRIM(dfi_moist_dname_table( grid%id, itrace ))//' memorder XEZ' , & ! Debug message
1, (jde-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
jms, jme, kds, kde, 1, config_flags%spec_bdy_width, &
MAX(jms,jds), MIN(jme,(jde-1)), kds, (kde-1), 1, config_flags%spec_bdy_width, &
                         ierr )
    CALL wrf_ext_read_field (  &
          fid                             , &  ! DataHandle
          current_date(1:19)              , &  ! DateStr
          TRIM(dfi_moist_dname_table( grid%id, itrace )) // '_BTYS', & !data name
          grid%dfi_moist_BTYS(ims,kds,1,itrace)  , &  ! Field
                       WRF_FLOAT             , &  ! FieldType 
          grid%communicator  , &  ! Comm
          grid%iocommunicator  , &  ! Comm
          grid%domdesc       , &  ! Comm
          grid%bdy_mask       , &  ! bdy_mask
          'YSZ'               , &  ! MemoryOrder
          ''                , &  ! Stagger
'inc/wrf_bdyin.inc ext_write_field '//TRIM(dfi_moist_dname_table( grid%id, itrace ))//' memorder YSZ' , & ! Debug message
1, (ide-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
ims, ime, kds, kde, 1, config_flags%spec_bdy_width, &
MAX(ims,ids), MIN(ime,(ide-1)), kds, (kde-1), 1, config_flags%spec_bdy_width, &
                         ierr )
    CALL wrf_ext_read_field (  &
          fid                             , &  ! DataHandle
          current_date(1:19)              , &  ! DateStr
          TRIM(dfi_moist_dname_table( grid%id, itrace )) // '_BTYE', & !data name
          grid%dfi_moist_BTYE(ims,kds,1,itrace)  , &  ! Field
                       WRF_FLOAT             , &  ! FieldType 
          grid%communicator  , &  ! Comm
          grid%iocommunicator  , &  ! Comm
          grid%domdesc       , &  ! Comm
          grid%bdy_mask       , &  ! bdy_mask
          'YEZ'               , &  ! MemoryOrder
          ''                , &  ! Stagger
'inc/wrf_bdyin.inc ext_write_field '//TRIM(dfi_moist_dname_table( grid%id, itrace ))//' memorder YEZ' , & ! Debug message
1, (ide-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
ims, ime, kds, kde, 1, config_flags%spec_bdy_width, &
MAX(ims,ids), MIN(ime,(ide-1)), kds, (kde-1), 1, config_flags%spec_bdy_width, &
                         ierr )
  ENDIF
ENDDO
DO itrace = PARAM_FIRST_SCALAR , num_scalar
  IF ( scalar_boundary_table(grid%id, itrace ) ) THEN
    CALL wrf_ext_read_field (  &
          fid                             , &  ! DataHandle
          current_date(1:19)              , &  ! DateStr
          TRIM(scalar_dname_table( grid%id, itrace )) // '_BXS', & !data name
          grid%scalar_BXS(jms,kds,1,itrace)  , &  ! Field
                       WRF_FLOAT             , &  ! FieldType 
          grid%communicator  , &  ! Comm
          grid%iocommunicator  , &  ! Comm
          grid%domdesc       , &  ! Comm
          grid%bdy_mask       , &  ! bdy_mask
          'XSZ'               , &  ! MemoryOrder
          ''                , &  ! Stagger
'inc/wrf_bdyin.inc ext_write_field '//TRIM(scalar_dname_table( grid%id, itrace ))//' memorder XSZ' , & ! Debug message
1, (jde-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
jms, jme, kds, kde, 1, config_flags%spec_bdy_width, &
MAX(jms,jds), MIN(jme,(jde-1)), kds, (kde-1), 1, config_flags%spec_bdy_width, &
                         ierr )
    CALL wrf_ext_read_field (  &
          fid                             , &  ! DataHandle
          current_date(1:19)              , &  ! DateStr
          TRIM(scalar_dname_table( grid%id, itrace )) // '_BXE', & !data name
          grid%scalar_BXE(jms,kds,1,itrace)  , &  ! Field
                       WRF_FLOAT             , &  ! FieldType 
          grid%communicator  , &  ! Comm
          grid%iocommunicator  , &  ! Comm
          grid%domdesc       , &  ! Comm
          grid%bdy_mask       , &  ! bdy_mask
          'XEZ'               , &  ! MemoryOrder
          ''                , &  ! Stagger
'inc/wrf_bdyin.inc ext_write_field '//TRIM(scalar_dname_table( grid%id, itrace ))//' memorder XEZ' , & ! Debug message
1, (jde-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
jms, jme, kds, kde, 1, config_flags%spec_bdy_width, &
MAX(jms,jds), MIN(jme,(jde-1)), kds, (kde-1), 1, config_flags%spec_bdy_width, &
                         ierr )
    CALL wrf_ext_read_field (  &
          fid                             , &  ! DataHandle
          current_date(1:19)              , &  ! DateStr
          TRIM(scalar_dname_table( grid%id, itrace )) // '_BYS', & !data name
          grid%scalar_BYS(ims,kds,1,itrace)  , &  ! Field
                       WRF_FLOAT             , &  ! FieldType 
          grid%communicator  , &  ! Comm
          grid%iocommunicator  , &  ! Comm
          grid%domdesc       , &  ! Comm
          grid%bdy_mask       , &  ! bdy_mask
          'YSZ'               , &  ! MemoryOrder
          ''                , &  ! Stagger
'inc/wrf_bdyin.inc ext_write_field '//TRIM(scalar_dname_table( grid%id, itrace ))//' memorder YSZ' , & ! Debug message
1, (ide-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
ims, ime, kds, kde, 1, config_flags%spec_bdy_width, &
MAX(ims,ids), MIN(ime,(ide-1)), kds, (kde-1), 1, config_flags%spec_bdy_width, &
                         ierr )
    CALL wrf_ext_read_field (  &
          fid                             , &  ! DataHandle
          current_date(1:19)              , &  ! DateStr
          TRIM(scalar_dname_table( grid%id, itrace )) // '_BYE', & !data name
          grid%scalar_BYE(ims,kds,1,itrace)  , &  ! Field
                       WRF_FLOAT             , &  ! FieldType 
          grid%communicator  , &  ! Comm
          grid%iocommunicator  , &  ! Comm
          grid%domdesc       , &  ! Comm
          grid%bdy_mask       , &  ! bdy_mask
          'YEZ'               , &  ! MemoryOrder
          ''                , &  ! Stagger
'inc/wrf_bdyin.inc ext_write_field '//TRIM(scalar_dname_table( grid%id, itrace ))//' memorder YEZ' , & ! Debug message
1, (ide-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
ims, ime, kds, kde, 1, config_flags%spec_bdy_width, &
MAX(ims,ids), MIN(ime,(ide-1)), kds, (kde-1), 1, config_flags%spec_bdy_width, &
                         ierr )
  ENDIF
ENDDO
DO itrace = PARAM_FIRST_SCALAR , num_scalar
  IF ( scalar_boundary_table(grid%id, itrace ) ) THEN
    CALL wrf_ext_read_field (  &
          fid                             , &  ! DataHandle
          current_date(1:19)              , &  ! DateStr
          TRIM(scalar_dname_table( grid%id, itrace )) // '_BTXS', & !data name
          grid%scalar_BTXS(jms,kds,1,itrace)  , &  ! Field
                       WRF_FLOAT             , &  ! FieldType 
          grid%communicator  , &  ! Comm
          grid%iocommunicator  , &  ! Comm
          grid%domdesc       , &  ! Comm
          grid%bdy_mask       , &  ! bdy_mask
          'XSZ'               , &  ! MemoryOrder
          ''                , &  ! Stagger
'inc/wrf_bdyin.inc ext_write_field '//TRIM(scalar_dname_table( grid%id, itrace ))//' memorder XSZ' , & ! Debug message
1, (jde-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
jms, jme, kds, kde, 1, config_flags%spec_bdy_width, &
MAX(jms,jds), MIN(jme,(jde-1)), kds, (kde-1), 1, config_flags%spec_bdy_width, &
                         ierr )
    CALL wrf_ext_read_field (  &
          fid                             , &  ! DataHandle
          current_date(1:19)              , &  ! DateStr
          TRIM(scalar_dname_table( grid%id, itrace )) // '_BTXE', & !data name
          grid%scalar_BTXE(jms,kds,1,itrace)  , &  ! Field
                       WRF_FLOAT             , &  ! FieldType 
          grid%communicator  , &  ! Comm
          grid%iocommunicator  , &  ! Comm
          grid%domdesc       , &  ! Comm
          grid%bdy_mask       , &  ! bdy_mask
          'XEZ'               , &  ! MemoryOrder
          ''                , &  ! Stagger
'inc/wrf_bdyin.inc ext_write_field '//TRIM(scalar_dname_table( grid%id, itrace ))//' memorder XEZ' , & ! Debug message
1, (jde-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
jms, jme, kds, kde, 1, config_flags%spec_bdy_width, &
MAX(jms,jds), MIN(jme,(jde-1)), kds, (kde-1), 1, config_flags%spec_bdy_width, &
                         ierr )
    CALL wrf_ext_read_field (  &
          fid                             , &  ! DataHandle
          current_date(1:19)              , &  ! DateStr
          TRIM(scalar_dname_table( grid%id, itrace )) // '_BTYS', & !data name
          grid%scalar_BTYS(ims,kds,1,itrace)  , &  ! Field
                       WRF_FLOAT             , &  ! FieldType 
          grid%communicator  , &  ! Comm
          grid%iocommunicator  , &  ! Comm
          grid%domdesc       , &  ! Comm
          grid%bdy_mask       , &  ! bdy_mask
          'YSZ'               , &  ! MemoryOrder
          ''                , &  ! Stagger
'inc/wrf_bdyin.inc ext_write_field '//TRIM(scalar_dname_table( grid%id, itrace ))//' memorder YSZ' , & ! Debug message
1, (ide-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
ims, ime, kds, kde, 1, config_flags%spec_bdy_width, &
MAX(ims,ids), MIN(ime,(ide-1)), kds, (kde-1), 1, config_flags%spec_bdy_width, &
                         ierr )
    CALL wrf_ext_read_field (  &
          fid                             , &  ! DataHandle
          current_date(1:19)              , &  ! DateStr
          TRIM(scalar_dname_table( grid%id, itrace )) // '_BTYE', & !data name
          grid%scalar_BTYE(ims,kds,1,itrace)  , &  ! Field
                       WRF_FLOAT             , &  ! FieldType 
          grid%communicator  , &  ! Comm
          grid%iocommunicator  , &  ! Comm
          grid%domdesc       , &  ! Comm
          grid%bdy_mask       , &  ! bdy_mask
          'YEZ'               , &  ! MemoryOrder
          ''                , &  ! Stagger
'inc/wrf_bdyin.inc ext_write_field '//TRIM(scalar_dname_table( grid%id, itrace ))//' memorder YEZ' , & ! Debug message
1, (ide-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
ims, ime, kds, kde, 1, config_flags%spec_bdy_width, &
MAX(ims,ids), MIN(ime,(ide-1)), kds, (kde-1), 1, config_flags%spec_bdy_width, &
                         ierr )
  ENDIF
ENDDO
DO itrace = PARAM_FIRST_SCALAR , num_dfi_scalar
  IF ( dfi_scalar_boundary_table(grid%id, itrace ) ) THEN
    CALL wrf_ext_read_field (  &
          fid                             , &  ! DataHandle
          current_date(1:19)              , &  ! DateStr
          TRIM(dfi_scalar_dname_table( grid%id, itrace )) // '_BXS', & !data name
          grid%dfi_scalar_BXS(jms,kds,1,itrace)  , &  ! Field
                       WRF_FLOAT             , &  ! FieldType 
          grid%communicator  , &  ! Comm
          grid%iocommunicator  , &  ! Comm
          grid%domdesc       , &  ! Comm
          grid%bdy_mask       , &  ! bdy_mask
          'XSZ'               , &  ! MemoryOrder
          ''                , &  ! Stagger
'inc/wrf_bdyin.inc ext_write_field '//TRIM(dfi_scalar_dname_table( grid%id, itrace ))//' memorder XSZ' , & ! Debug message
1, (jde-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
jms, jme, kds, kde, 1, config_flags%spec_bdy_width, &
MAX(jms,jds), MIN(jme,(jde-1)), kds, (kde-1), 1, config_flags%spec_bdy_width, &
                         ierr )
    CALL wrf_ext_read_field (  &
          fid                             , &  ! DataHandle
          current_date(1:19)              , &  ! DateStr
          TRIM(dfi_scalar_dname_table( grid%id, itrace )) // '_BXE', & !data name
          grid%dfi_scalar_BXE(jms,kds,1,itrace)  , &  ! Field
                       WRF_FLOAT             , &  ! FieldType 
          grid%communicator  , &  ! Comm
          grid%iocommunicator  , &  ! Comm
          grid%domdesc       , &  ! Comm
          grid%bdy_mask       , &  ! bdy_mask
          'XEZ'               , &  ! MemoryOrder
          ''                , &  ! Stagger
'inc/wrf_bdyin.inc ext_write_field '//TRIM(dfi_scalar_dname_table( grid%id, itrace ))//' memorder XEZ' , & ! Debug message
1, (jde-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
jms, jme, kds, kde, 1, config_flags%spec_bdy_width, &
MAX(jms,jds), MIN(jme,(jde-1)), kds, (kde-1), 1, config_flags%spec_bdy_width, &
                         ierr )
    CALL wrf_ext_read_field (  &
          fid                             , &  ! DataHandle
          current_date(1:19)              , &  ! DateStr
          TRIM(dfi_scalar_dname_table( grid%id, itrace )) // '_BYS', & !data name
          grid%dfi_scalar_BYS(ims,kds,1,itrace)  , &  ! Field
                       WRF_FLOAT             , &  ! FieldType 
          grid%communicator  , &  ! Comm
          grid%iocommunicator  , &  ! Comm
          grid%domdesc       , &  ! Comm
          grid%bdy_mask       , &  ! bdy_mask
          'YSZ'               , &  ! MemoryOrder
          ''                , &  ! Stagger
'inc/wrf_bdyin.inc ext_write_field '//TRIM(dfi_scalar_dname_table( grid%id, itrace ))//' memorder YSZ' , & ! Debug message
1, (ide-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
ims, ime, kds, kde, 1, config_flags%spec_bdy_width, &
MAX(ims,ids), MIN(ime,(ide-1)), kds, (kde-1), 1, config_flags%spec_bdy_width, &
                         ierr )
    CALL wrf_ext_read_field (  &
          fid                             , &  ! DataHandle
          current_date(1:19)              , &  ! DateStr
          TRIM(dfi_scalar_dname_table( grid%id, itrace )) // '_BYE', & !data name
          grid%dfi_scalar_BYE(ims,kds,1,itrace)  , &  ! Field
                       WRF_FLOAT             , &  ! FieldType 
          grid%communicator  , &  ! Comm
          grid%iocommunicator  , &  ! Comm
          grid%domdesc       , &  ! Comm
          grid%bdy_mask       , &  ! bdy_mask
          'YEZ'               , &  ! MemoryOrder
          ''                , &  ! Stagger
'inc/wrf_bdyin.inc ext_write_field '//TRIM(dfi_scalar_dname_table( grid%id, itrace ))//' memorder YEZ' , & ! Debug message
1, (ide-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
ims, ime, kds, kde, 1, config_flags%spec_bdy_width, &
MAX(ims,ids), MIN(ime,(ide-1)), kds, (kde-1), 1, config_flags%spec_bdy_width, &
                         ierr )
  ENDIF
ENDDO
DO itrace = PARAM_FIRST_SCALAR , num_dfi_scalar
  IF ( dfi_scalar_boundary_table(grid%id, itrace ) ) THEN
    CALL wrf_ext_read_field (  &
          fid                             , &  ! DataHandle
          current_date(1:19)              , &  ! DateStr
          TRIM(dfi_scalar_dname_table( grid%id, itrace )) // '_BTXS', & !data name
          grid%dfi_scalar_BTXS(jms,kds,1,itrace)  , &  ! Field
                       WRF_FLOAT             , &  ! FieldType 
          grid%communicator  , &  ! Comm
          grid%iocommunicator  , &  ! Comm
          grid%domdesc       , &  ! Comm
          grid%bdy_mask       , &  ! bdy_mask
          'XSZ'               , &  ! MemoryOrder
          ''                , &  ! Stagger
'inc/wrf_bdyin.inc ext_write_field '//TRIM(dfi_scalar_dname_table( grid%id, itrace ))//' memorder XSZ' , & ! Debug message
1, (jde-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
jms, jme, kds, kde, 1, config_flags%spec_bdy_width, &
MAX(jms,jds), MIN(jme,(jde-1)), kds, (kde-1), 1, config_flags%spec_bdy_width, &
                         ierr )
    CALL wrf_ext_read_field (  &
          fid                             , &  ! DataHandle
          current_date(1:19)              , &  ! DateStr
          TRIM(dfi_scalar_dname_table( grid%id, itrace )) // '_BTXE', & !data name
          grid%dfi_scalar_BTXE(jms,kds,1,itrace)  , &  ! Field
                       WRF_FLOAT             , &  ! FieldType 
          grid%communicator  , &  ! Comm
          grid%iocommunicator  , &  ! Comm
          grid%domdesc       , &  ! Comm
          grid%bdy_mask       , &  ! bdy_mask
          'XEZ'               , &  ! MemoryOrder
          ''                , &  ! Stagger
'inc/wrf_bdyin.inc ext_write_field '//TRIM(dfi_scalar_dname_table( grid%id, itrace ))//' memorder XEZ' , & ! Debug message
1, (jde-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
jms, jme, kds, kde, 1, config_flags%spec_bdy_width, &
MAX(jms,jds), MIN(jme,(jde-1)), kds, (kde-1), 1, config_flags%spec_bdy_width, &
                         ierr )
    CALL wrf_ext_read_field (  &
          fid                             , &  ! DataHandle
          current_date(1:19)              , &  ! DateStr
          TRIM(dfi_scalar_dname_table( grid%id, itrace )) // '_BTYS', & !data name
          grid%dfi_scalar_BTYS(ims,kds,1,itrace)  , &  ! Field
                       WRF_FLOAT             , &  ! FieldType 
          grid%communicator  , &  ! Comm
          grid%iocommunicator  , &  ! Comm
          grid%domdesc       , &  ! Comm
          grid%bdy_mask       , &  ! bdy_mask
          'YSZ'               , &  ! MemoryOrder
          ''                , &  ! Stagger
'inc/wrf_bdyin.inc ext_write_field '//TRIM(dfi_scalar_dname_table( grid%id, itrace ))//' memorder YSZ' , & ! Debug message
1, (ide-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
ims, ime, kds, kde, 1, config_flags%spec_bdy_width, &
MAX(ims,ids), MIN(ime,(ide-1)), kds, (kde-1), 1, config_flags%spec_bdy_width, &
                         ierr )
    CALL wrf_ext_read_field (  &
          fid                             , &  ! DataHandle
          current_date(1:19)              , &  ! DateStr
          TRIM(dfi_scalar_dname_table( grid%id, itrace )) // '_BTYE', & !data name
          grid%dfi_scalar_BTYE(ims,kds,1,itrace)  , &  ! Field
                       WRF_FLOAT             , &  ! FieldType 
          grid%communicator  , &  ! Comm
          grid%iocommunicator  , &  ! Comm
          grid%domdesc       , &  ! Comm
          grid%bdy_mask       , &  ! bdy_mask
          'YEZ'               , &  ! MemoryOrder
          ''                , &  ! Stagger
'inc/wrf_bdyin.inc ext_write_field '//TRIM(dfi_scalar_dname_table( grid%id, itrace ))//' memorder YEZ' , & ! Debug message
1, (ide-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
ims, ime, kds, kde, 1, config_flags%spec_bdy_width, &
MAX(ims,ids), MIN(ime,(ide-1)), kds, (kde-1), 1, config_flags%spec_bdy_width, &
                         ierr )
  ENDIF
ENDDO
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'HT_SHAD_BXS'               , &  ! Data Name 
                       grid%ht_shad_bxs(jms,kds,1)     , &  ! Field 
                       WRF_FLOAT             , &  ! FieldType 
                       grid%communicator , &  ! Comm
                       grid%iocommunicator , &  ! Comm
                       grid%domdesc      , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XS'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_bdyin.inc ext_read_field HT_SHAD_BXS memorder XS' , & ! Debug message
1, (jde-1), 1, config_flags%spec_bdy_width, 1, 1, &
jms, jme, 1, config_flags%spec_bdy_width, 1, 1, &
MAX(jms,jds), MIN(jme,(jde-1)), 1, config_flags%spec_bdy_width, 1, 1, &
                       ierr )
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'HT_SHAD_BXE'               , &  ! Data Name 
                       grid%ht_shad_bxe(jms,kds,1)     , &  ! Field 
                       WRF_FLOAT             , &  ! FieldType 
                       grid%communicator , &  ! Comm
                       grid%iocommunicator , &  ! Comm
                       grid%domdesc      , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XE'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_bdyin.inc ext_read_field HT_SHAD_BXE memorder XE' , & ! Debug message
1, (jde-1), 1, config_flags%spec_bdy_width, 1, 1, &
jms, jme, 1, config_flags%spec_bdy_width, 1, 1, &
MAX(jms,jds), MIN(jme,(jde-1)), 1, config_flags%spec_bdy_width, 1, 1, &
                       ierr )
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'HT_SHAD_BYS'               , &  ! Data Name 
                       grid%ht_shad_bys(ims,kds,1)     , &  ! Field 
                       WRF_FLOAT             , &  ! FieldType 
                       grid%communicator , &  ! Comm
                       grid%iocommunicator , &  ! Comm
                       grid%domdesc      , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'YS'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_bdyin.inc ext_read_field HT_SHAD_BYS memorder YS' , & ! Debug message
1, (ide-1), 1, config_flags%spec_bdy_width, 1, 1, &
ims, ime, 1, config_flags%spec_bdy_width, 1, 1, &
MAX(ims,ids), MIN(ime,(ide-1)), 1, config_flags%spec_bdy_width, 1, 1, &
                       ierr )
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'HT_SHAD_BYE'               , &  ! Data Name 
                       grid%ht_shad_bye(ims,kds,1)     , &  ! Field 
                       WRF_FLOAT             , &  ! FieldType 
                       grid%communicator , &  ! Comm
                       grid%iocommunicator , &  ! Comm
                       grid%domdesc      , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'YE'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_bdyin.inc ext_read_field HT_SHAD_BYE memorder YE' , & ! Debug message
1, (ide-1), 1, config_flags%spec_bdy_width, 1, 1, &
ims, ime, 1, config_flags%spec_bdy_width, 1, 1, &
MAX(ims,ids), MIN(ime,(ide-1)), 1, config_flags%spec_bdy_width, 1, 1, &
                       ierr )
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'HT_SHAD_BTXS'               , &  ! Data Name 
                       grid%ht_shad_btxs(jms,kds,1)     , &  ! Field 
                       WRF_FLOAT             , &  ! FieldType 
                       grid%communicator , &  ! Comm
                       grid%iocommunicator , &  ! Comm
                       grid%domdesc      , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XS'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_bdyin.inc ext_read_field HT_SHAD_BTXS memorder XS' , & ! Debug message
1, (jde-1), 1, config_flags%spec_bdy_width, 1, 1, &
jms, jme, 1, config_flags%spec_bdy_width, 1, 1, &
MAX(jms,jds), MIN(jme,(jde-1)), 1, config_flags%spec_bdy_width, 1, 1, &
                       ierr )
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'HT_SHAD_BTXE'               , &  ! Data Name 
                       grid%ht_shad_btxe(jms,kds,1)     , &  ! Field 
                       WRF_FLOAT             , &  ! FieldType 
                       grid%communicator , &  ! Comm
                       grid%iocommunicator , &  ! Comm
                       grid%domdesc      , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XE'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_bdyin.inc ext_read_field HT_SHAD_BTXE memorder XE' , & ! Debug message
1, (jde-1), 1, config_flags%spec_bdy_width, 1, 1, &
jms, jme, 1, config_flags%spec_bdy_width, 1, 1, &
MAX(jms,jds), MIN(jme,(jde-1)), 1, config_flags%spec_bdy_width, 1, 1, &
                       ierr )
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'HT_SHAD_BTYS'               , &  ! Data Name 
                       grid%ht_shad_btys(ims,kds,1)     , &  ! Field 
                       WRF_FLOAT             , &  ! FieldType 
                       grid%communicator , &  ! Comm
                       grid%iocommunicator , &  ! Comm
                       grid%domdesc      , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'YS'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_bdyin.inc ext_read_field HT_SHAD_BTYS memorder YS' , & ! Debug message
1, (ide-1), 1, config_flags%spec_bdy_width, 1, 1, &
ims, ime, 1, config_flags%spec_bdy_width, 1, 1, &
MAX(ims,ids), MIN(ime,(ide-1)), 1, config_flags%spec_bdy_width, 1, 1, &
                       ierr )
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'HT_SHAD_BTYE'               , &  ! Data Name 
                       grid%ht_shad_btye(ims,kds,1)     , &  ! Field 
                       WRF_FLOAT             , &  ! FieldType 
                       grid%communicator , &  ! Comm
                       grid%iocommunicator , &  ! Comm
                       grid%domdesc      , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'YE'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_bdyin.inc ext_read_field HT_SHAD_BTYE memorder YE' , & ! Debug message
1, (ide-1), 1, config_flags%spec_bdy_width, 1, 1, &
ims, ime, 1, config_flags%spec_bdy_width, 1, 1, &
MAX(ims,ids), MIN(ime,(ide-1)), 1, config_flags%spec_bdy_width, 1, 1, &
                       ierr )
DO itrace = PARAM_FIRST_SCALAR , num_tracer
  IF ( tracer_boundary_table(grid%id, itrace ) ) THEN
    CALL wrf_ext_read_field (  &
          fid                             , &  ! DataHandle
          current_date(1:19)              , &  ! DateStr
          TRIM(tracer_dname_table( grid%id, itrace )) // '_BXS', & !data name
          grid%tracer_BXS(jms,kds,1,itrace)  , &  ! Field
                       WRF_FLOAT             , &  ! FieldType 
          grid%communicator  , &  ! Comm
          grid%iocommunicator  , &  ! Comm
          grid%domdesc       , &  ! Comm
          grid%bdy_mask       , &  ! bdy_mask
          'XSZ'               , &  ! MemoryOrder
          ''                , &  ! Stagger
'inc/wrf_bdyin.inc ext_write_field '//TRIM(tracer_dname_table( grid%id, itrace ))//' memorder XSZ' , & ! Debug message
1, (jde-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
jms, jme, kds, kde, 1, config_flags%spec_bdy_width, &
MAX(jms,jds), MIN(jme,(jde-1)), kds, (kde-1), 1, config_flags%spec_bdy_width, &
                         ierr )
    CALL wrf_ext_read_field (  &
          fid                             , &  ! DataHandle
          current_date(1:19)              , &  ! DateStr
          TRIM(tracer_dname_table( grid%id, itrace )) // '_BXE', & !data name
          grid%tracer_BXE(jms,kds,1,itrace)  , &  ! Field
                       WRF_FLOAT             , &  ! FieldType 
          grid%communicator  , &  ! Comm
          grid%iocommunicator  , &  ! Comm
          grid%domdesc       , &  ! Comm
          grid%bdy_mask       , &  ! bdy_mask
          'XEZ'               , &  ! MemoryOrder
          ''                , &  ! Stagger
'inc/wrf_bdyin.inc ext_write_field '//TRIM(tracer_dname_table( grid%id, itrace ))//' memorder XEZ' , & ! Debug message
1, (jde-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
jms, jme, kds, kde, 1, config_flags%spec_bdy_width, &
MAX(jms,jds), MIN(jme,(jde-1)), kds, (kde-1), 1, config_flags%spec_bdy_width, &
                         ierr )
    CALL wrf_ext_read_field (  &
          fid                             , &  ! DataHandle
          current_date(1:19)              , &  ! DateStr
          TRIM(tracer_dname_table( grid%id, itrace )) // '_BYS', & !data name
          grid%tracer_BYS(ims,kds,1,itrace)  , &  ! Field
                       WRF_FLOAT             , &  ! FieldType 
          grid%communicator  , &  ! Comm
          grid%iocommunicator  , &  ! Comm
          grid%domdesc       , &  ! Comm
          grid%bdy_mask       , &  ! bdy_mask
          'YSZ'               , &  ! MemoryOrder
          ''                , &  ! Stagger
'inc/wrf_bdyin.inc ext_write_field '//TRIM(tracer_dname_table( grid%id, itrace ))//' memorder YSZ' , & ! Debug message
1, (ide-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
ims, ime, kds, kde, 1, config_flags%spec_bdy_width, &
MAX(ims,ids), MIN(ime,(ide-1)), kds, (kde-1), 1, config_flags%spec_bdy_width, &
                         ierr )
    CALL wrf_ext_read_field (  &
          fid                             , &  ! DataHandle
          current_date(1:19)              , &  ! DateStr
          TRIM(tracer_dname_table( grid%id, itrace )) // '_BYE', & !data name
          grid%tracer_BYE(ims,kds,1,itrace)  , &  ! Field
                       WRF_FLOAT             , &  ! FieldType 
          grid%communicator  , &  ! Comm
          grid%iocommunicator  , &  ! Comm
          grid%domdesc       , &  ! Comm
          grid%bdy_mask       , &  ! bdy_mask
          'YEZ'               , &  ! MemoryOrder
          ''                , &  ! Stagger
'inc/wrf_bdyin.inc ext_write_field '//TRIM(tracer_dname_table( grid%id, itrace ))//' memorder YEZ' , & ! Debug message
1, (ide-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
ims, ime, kds, kde, 1, config_flags%spec_bdy_width, &
MAX(ims,ids), MIN(ime,(ide-1)), kds, (kde-1), 1, config_flags%spec_bdy_width, &
                         ierr )
  ENDIF
ENDDO
DO itrace = PARAM_FIRST_SCALAR , num_tracer
  IF ( tracer_boundary_table(grid%id, itrace ) ) THEN
    CALL wrf_ext_read_field (  &
          fid                             , &  ! DataHandle
          current_date(1:19)              , &  ! DateStr
          TRIM(tracer_dname_table( grid%id, itrace )) // '_BTXS', & !data name
          grid%tracer_BTXS(jms,kds,1,itrace)  , &  ! Field
                       WRF_FLOAT             , &  ! FieldType 
          grid%communicator  , &  ! Comm
          grid%iocommunicator  , &  ! Comm
          grid%domdesc       , &  ! Comm
          grid%bdy_mask       , &  ! bdy_mask
          'XSZ'               , &  ! MemoryOrder
          ''                , &  ! Stagger
'inc/wrf_bdyin.inc ext_write_field '//TRIM(tracer_dname_table( grid%id, itrace ))//' memorder XSZ' , & ! Debug message
1, (jde-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
jms, jme, kds, kde, 1, config_flags%spec_bdy_width, &
MAX(jms,jds), MIN(jme,(jde-1)), kds, (kde-1), 1, config_flags%spec_bdy_width, &
                         ierr )
    CALL wrf_ext_read_field (  &
          fid                             , &  ! DataHandle
          current_date(1:19)              , &  ! DateStr
          TRIM(tracer_dname_table( grid%id, itrace )) // '_BTXE', & !data name
          grid%tracer_BTXE(jms,kds,1,itrace)  , &  ! Field
                       WRF_FLOAT             , &  ! FieldType 
          grid%communicator  , &  ! Comm
          grid%iocommunicator  , &  ! Comm
          grid%domdesc       , &  ! Comm
          grid%bdy_mask       , &  ! bdy_mask
          'XEZ'               , &  ! MemoryOrder
          ''                , &  ! Stagger
'inc/wrf_bdyin.inc ext_write_field '//TRIM(tracer_dname_table( grid%id, itrace ))//' memorder XEZ' , & ! Debug message
1, (jde-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
jms, jme, kds, kde, 1, config_flags%spec_bdy_width, &
MAX(jms,jds), MIN(jme,(jde-1)), kds, (kde-1), 1, config_flags%spec_bdy_width, &
                         ierr )
    CALL wrf_ext_read_field (  &
          fid                             , &  ! DataHandle
          current_date(1:19)              , &  ! DateStr
          TRIM(tracer_dname_table( grid%id, itrace )) // '_BTYS', & !data name
          grid%tracer_BTYS(ims,kds,1,itrace)  , &  ! Field
                       WRF_FLOAT             , &  ! FieldType 
          grid%communicator  , &  ! Comm
          grid%iocommunicator  , &  ! Comm
          grid%domdesc       , &  ! Comm
          grid%bdy_mask       , &  ! bdy_mask
          'YSZ'               , &  ! MemoryOrder
          ''                , &  ! Stagger
'inc/wrf_bdyin.inc ext_write_field '//TRIM(tracer_dname_table( grid%id, itrace ))//' memorder YSZ' , & ! Debug message
1, (ide-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
ims, ime, kds, kde, 1, config_flags%spec_bdy_width, &
MAX(ims,ids), MIN(ime,(ide-1)), kds, (kde-1), 1, config_flags%spec_bdy_width, &
                         ierr )
    CALL wrf_ext_read_field (  &
          fid                             , &  ! DataHandle
          current_date(1:19)              , &  ! DateStr
          TRIM(tracer_dname_table( grid%id, itrace )) // '_BTYE', & !data name
          grid%tracer_BTYE(ims,kds,1,itrace)  , &  ! Field
                       WRF_FLOAT             , &  ! FieldType 
          grid%communicator  , &  ! Comm
          grid%iocommunicator  , &  ! Comm
          grid%domdesc       , &  ! Comm
          grid%bdy_mask       , &  ! bdy_mask
          'YEZ'               , &  ! MemoryOrder
          ''                , &  ! Stagger
'inc/wrf_bdyin.inc ext_write_field '//TRIM(tracer_dname_table( grid%id, itrace ))//' memorder YEZ' , & ! Debug message
1, (ide-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
ims, ime, kds, kde, 1, config_flags%spec_bdy_width, &
MAX(ims,ids), MIN(ime,(ide-1)), kds, (kde-1), 1, config_flags%spec_bdy_width, &
                         ierr )
  ENDIF
ENDDO
!ENDOFREGISTRYGENERATEDINCLUDE
