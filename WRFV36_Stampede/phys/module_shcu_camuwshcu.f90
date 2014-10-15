


  module uwshcu



  use module_cam_support, only: outfld, addfld, phys_decomp



  use module_state_description, only: CAMUWPBLSCHEME, MYJPBLSCHEME, &
      MYNNPBLSCHEME2, MYNNPBLSCHEME3


  use error_function, only: erfc






  use module_cam_support, only: iulog, pcols, pver, pverp, endrun, masterproc



  implicit none
  private
  save

  public &
     uwshcu_readnl,      &
     init_uwshcu,        &
     compute_uwshcu,     &
     compute_uwshcu_inv
  
  integer , parameter :: r8 = selected_real_kind(12)    
  real(r8), parameter :: unset_r8 = huge(1.0_r8)
  real(r8)            :: xlv                            
  real(r8)            :: xlf                            
  real(r8)            :: xls                            
  real(r8)            :: cp                             
  real(r8)            :: zvir                           
  real(r8)            :: r                              
  real(r8)            :: g                              
  real(r8)            :: ep2                            
  real(r8)            :: p00                            
  real(r8)            :: rovcp                          

  
  real(r8) :: rpen          


contains

  
  real(r8) function exnf(pressure)
           real(r8), intent(in)              :: pressure
           exnf = (pressure/p00)**rovcp
           return
  end function exnf



subroutine uwshcu_readnl(nlfile)






   character(len=*), intent(in) :: nlfile  

   
   
   rpen= 10.0_r8

end subroutine uwshcu_readnl



  subroutine init_uwshcu( kind, xlv_in, cp_in, xlf_in, zvir_in, r_in, g_in, ep2_in &
       , rushten, rvshten, rthshten, rqvshten,                   &
       rqcshten, rqrshten, rqishten, rqsshten, rqgshten,         &
       p_qc, p_qr, p_qi, p_qs, p_qg,                             &
       bl_pbl_physics, param_first_scalar, restart,              &
       ids, ide, jds, jde, kds, kde,                             &
       ims, ime, jms, jme, kms, kme,                             &
       its, ite, jts, jte, kts, kte                              )


    
    
    
    
    use module_cam_support, only: outfld, addfld, phys_decomp, pcols, pver, pverp
    implicit none
    integer , intent(in) :: kind       
    real(r8), intent(in) :: xlv_in     
    real(r8), intent(in) :: xlf_in     
    real(r8), intent(in) :: cp_in      
    real(r8), intent(in) :: zvir_in    
    real(r8), intent(in) :: r_in       
    real(r8), intent(in) :: g_in       
    real(r8), intent(in) :: ep2_in     

    character(len=*), parameter :: subname = 'init_uwshcu'
    LOGICAL , INTENT(IN)           ::   restart
    INTEGER , INTENT(IN)           ::   ids, ide, jds, jde, kds, kde, &
                                        ims, ime, jms, jme, kms, kme, &
                                        its, ite, jts, jte, kts, kte, &
                                        p_qc, p_qr, p_qi, p_qs, p_qg, &
                                        param_first_scalar,           &
                                        bl_pbl_physics

    REAL, DIMENSION( ims:ime , kms:kme , jms:jme ), INTENT(INOUT) :: &
                                                             rushten, &
                                                             rvshten, &
                                                            rthshten, &
                                                            rqvshten, &
                                                            rqcshten, &
                                                            rqrshten, &
                                                            rqishten, &
                                                            rqsshten, &
                                                            rqgshten

  integer :: i, itf, j, jtf, k, ktf

  jtf = min(jte,jde-1)
  ktf = min(kte,kde-1)
  itf = min(ite,ide-1)

  
  call uwshcu_readnl('dummyString')









  select case(bl_pbl_physics)
  case (CAMUWPBLSCHEME, MYJPBLSCHEME, MYNNPBLSCHEME2, MYNNPBLSCHEME3)
     
  case default
     call wrf_error_fatal3("<stdin>",148,&
"The CAMUWSHCU scheme requires CAMUWPBLSCHEME, MYJPBLSCHEME or MYNN.")
  end select







  pver  = ktf-kts+1
  pverp = pver+1
    
    
    

    call addfld( 'qtflx_Cu'       , 'kg/m2/s' , pverp , 'A' , 'Convective qt flux'                                  , phys_decomp )
    call addfld( 'slflx_Cu'       , 'J/m2/s'  , pverp , 'A' , 'Convective sl flux'                                  , phys_decomp )
    call addfld( 'uflx_Cu'        , 'kg/m/s2' , pverp , 'A' , 'Convective  u flux'                                  , phys_decomp )
    call addfld( 'vflx_Cu'        , 'kg/m/s2' , pverp , 'A' , 'Convective  v flux'                                  , phys_decomp )

    call addfld( 'qtten_Cu'       , 'kg/kg/s' , pver  , 'A' , 'qt tendency by convection'                           , phys_decomp )
    call addfld( 'slten_Cu'       , 'J/kg/s'  , pver  , 'A' , 'sl tendency by convection'                           , phys_decomp )
    call addfld( 'uten_Cu'        , 'm/s2'    , pver  , 'A' , ' u tendency by convection'                           , phys_decomp )
    call addfld( 'vten_Cu'        , 'm/s2'    , pver  , 'A' , ' v tendency by convection'                           , phys_decomp )
    call addfld( 'qvten_Cu'       , 'kg/kg/s' , pver  , 'A' , 'qv tendency by convection'                           , phys_decomp )
    call addfld( 'qlten_Cu'       , 'kg/kg/s' , pver  , 'A' , 'ql tendency by convection'                           , phys_decomp )
    call addfld( 'qiten_Cu'       , 'kg/kg/s' , pver  , 'A' , 'qi tendency by convection'                           , phys_decomp )

    call addfld( 'cbmf_Cu'        , 'kg/m2/s' , 1     , 'A' , 'Cumulus base mass flux'                              , phys_decomp )
    call addfld( 'ufrcinvbase_Cu' , 'fraction', 1     , 'A' , 'Cumulus fraction at PBL top'                         , phys_decomp ) 
    call addfld( 'ufrclcl_Cu'     , 'fraction', 1     , 'A' , 'Cumulus fraction at LCL'                             , phys_decomp )
    call addfld( 'winvbase_Cu'    , 'm/s'     , 1     , 'A' , 'Cumulus vertical velocity at PBL top'                , phys_decomp )
    call addfld( 'wlcl_Cu'        , 'm/s'     , 1     , 'A' , 'Cumulus vertical velocity at LCL'                    , phys_decomp )
    call addfld( 'plcl_Cu'        , 'Pa'      , 1     , 'A' , 'LCL of source air'                                   , phys_decomp )
    call addfld( 'pinv_Cu'        , 'Pa'      , 1     , 'A' , 'PBL top pressure'                                    , phys_decomp )
    call addfld( 'plfc_Cu'        , 'Pa'      , 1     , 'A' , 'LFC of source air'                                   , phys_decomp )
    call addfld( 'pbup_Cu'        , 'Pa'      , 1     , 'A' , 'Highest interface level of positive cumulus buoyancy', phys_decomp )
    call addfld( 'ppen_Cu'        , 'Pa'      , 1     , 'A' , 'Highest level where cumulus w is 0'                  , phys_decomp )
    call addfld( 'qtsrc_Cu'       , 'kg/kg'   , 1     , 'A' , 'Cumulus source air qt'                               , phys_decomp )
    call addfld( 'thlsrc_Cu'      , 'K'       , 1     , 'A' , 'Cumulus source air thl'                              , phys_decomp )
    call addfld( 'thvlsrc_Cu'     , 'K'       , 1     , 'A' , 'Cumulus source air thvl'                             , phys_decomp )
    call addfld( 'emfkbup_Cu'     , 'kg/m2/s' , 1     , 'A' , 'Penetrative mass flux at kbup'                       , phys_decomp )
    call addfld( 'cin_Cu'         , 'J/kg'    , 1     , 'A' , 'CIN upto LFC'                                        , phys_decomp )
    call addfld( 'cinlcl_Cu'      , 'J/kg'    , 1     , 'A' , 'CIN upto LCL'                                        , phys_decomp )
    call addfld( 'cbmflimit_Cu'   , 'kg/m2/s' , 1     , 'A' , 'cbmflimiter'                                         , phys_decomp ) 
    call addfld( 'tkeavg_Cu'      , 'm2/s2'   , 1     , 'A' , 'Average tke within PBL for convection scheme'        , phys_decomp ) 
    call addfld( 'zinv_Cu'        , 'm'       , 1     , 'A' , 'PBL top height'                                      , phys_decomp )
    call addfld( 'rcwp_Cu'        , 'kg/m2'   , 1     , 'A' , 'Cumulus LWP+IWP'                                     , phys_decomp )
    call addfld( 'rlwp_Cu'        , 'kg/m2'   , 1     , 'A' , 'Cumulus LWP'                                         , phys_decomp )
    call addfld( 'riwp_Cu'        , 'kg/m2'   , 1     , 'A' , 'Cumulus IWP'                                         , phys_decomp )
    call addfld( 'tophgt_Cu'      , 'm'       , 1     , 'A' , 'Cumulus top height'                                  , phys_decomp )

    call addfld( 'wu_Cu'          , 'm/s'     , pverp , 'A' , 'Convective updraft vertical velocity'                , phys_decomp )
    call addfld( 'ufrc_Cu'        , 'fraction', pverp , 'A' , 'Convective updraft fractional area'                  , phys_decomp )
    call addfld( 'qtu_Cu'         , 'kg/kg'   , pverp , 'A' , 'Cumulus updraft qt'                                  , phys_decomp )
    call addfld( 'thlu_Cu'        , 'K'       , pverp , 'A' , 'Cumulus updraft thl'                                 , phys_decomp )
    call addfld( 'thvu_Cu'        , 'K'       , pverp , 'A' , 'Cumulus updraft thv'                                 , phys_decomp )
    call addfld( 'uu_Cu'          , 'm/s'     , pverp , 'A' , 'Cumulus updraft uwnd'                                , phys_decomp )
    call addfld( 'vu_Cu'          , 'm/s'     , pverp , 'A' , 'Cumulus updraft vwnd'                                , phys_decomp )
    call addfld( 'qtu_emf_Cu'     , 'kg/kg'   , pverp , 'A' , 'qt of penatratively entrained air'                   , phys_decomp )
    call addfld( 'thlu_emf_Cu'    , 'K'       , pverp , 'A' , 'thl of penatratively entrained air'                  , phys_decomp )
    call addfld( 'uu_emf_Cu'      , 'm/s'     , pverp , 'A' , 'uwnd of penatratively entrained air'                 , phys_decomp )
    call addfld( 'vu_emf_Cu'      , 'm/s'     , pverp , 'A' , 'vwnd of penatratively entrained air'                 , phys_decomp )
    call addfld( 'umf_Cu'         , 'kg/m2/s' , pverp , 'A' , 'Cumulus updraft mass flux'                           , phys_decomp )
    call addfld( 'uemf_Cu'        , 'kg/m2/s' , pverp , 'A' , 'Cumulus net ( updraft + entrainment ) mass flux'     , phys_decomp )
    call addfld( 'qcu_Cu'         , 'kg/kg'   , pver  , 'A' , 'Cumulus updraft LWC+IWC'                             , phys_decomp )
    call addfld( 'qlu_Cu'         , 'kg/kg'   , pver  , 'A' , 'Cumulus updraft LWC'                                 , phys_decomp )
    call addfld( 'qiu_Cu'         , 'kg/kg'   , pver  , 'A' , 'Cumulus updraft IWC'                                 , phys_decomp )
    call addfld( 'cufrc_Cu'       , 'fraction', pver  , 'A' , 'Cumulus cloud fraction'                              , phys_decomp )
    call addfld( 'fer_Cu'         , '1/m'     , pver  , 'A' , 'Cumulus lateral fractional entrainment rate'         , phys_decomp )
    call addfld( 'fdr_Cu'         , '1/m'     , pver  , 'A' , 'Cumulus lateral fractional detrainment Rate'         , phys_decomp )

    call addfld( 'dwten_Cu'       , 'kg/kg/s' , pver  , 'A' , 'Expellsion rate of cumulus cloud water to env.'      , phys_decomp )
    call addfld( 'diten_Cu'       , 'kg/kg/s' , pver  , 'A' , 'Expellsion rate of cumulus ice water to env.'        , phys_decomp )
    call addfld( 'qrten_Cu'       , 'kg/kg/s' , pver  , 'A' , 'Production rate of rain by cumulus'                  , phys_decomp )
    call addfld( 'qsten_Cu'       , 'kg/kg/s' , pver  , 'A' , 'Production rate of snow by cumulus'                  , phys_decomp )
    call addfld( 'flxrain_Cu'     , 'kg/m2/s' , pverp , 'A' , 'Rain flux induced by Cumulus'                        , phys_decomp )
    call addfld( 'flxsnow_Cu'     , 'kg/m2/s' , pverp , 'A' , 'Snow flux induced by Cumulus'                        , phys_decomp )
    call addfld( 'ntraprd_Cu'     , 'kg/kg/s' , pver  , 'A' , 'Net production rate of rain by Cumulus'              , phys_decomp )
    call addfld( 'ntsnprd_Cu'     , 'kg/kg/s' , pver  , 'A' , 'Net production rate of snow by Cumulus'              , phys_decomp )

    call addfld( 'excessu_Cu'     , 'no'      , pver  , 'A' , 'Updraft saturation excess'                           , phys_decomp )
    call addfld( 'excess0_Cu'     , 'no'      , pver  , 'A' , 'Environmental saturation excess'                     , phys_decomp )
    call addfld( 'xc_Cu'          , 'no'      , pver  , 'A' , 'Critical mixing ratio'                               , phys_decomp )
    call addfld( 'aquad_Cu'       , 'no'      , pver  , 'A' , 'aquad'                                               , phys_decomp )
    call addfld( 'bquad_Cu'       , 'no'      , pver  , 'A' , 'bquad'                                               , phys_decomp )
    call addfld( 'cquad_Cu'       , 'no'      , pver  , 'A' , 'cquad'                                               , phys_decomp )
    call addfld( 'bogbot_Cu'      , 'no'      , pver  , 'A' , 'Cloud buoyancy at the bottom interface'              , phys_decomp )
    call addfld( 'bogtop_Cu'      , 'no'      , pver  , 'A' , 'Cloud buoyancy at the top interface'                 , phys_decomp )

    call addfld('exit_UWCu_Cu'    , 'no'      , 1     , 'A' , 'exit_UWCu'                                           , phys_decomp ) 
    call addfld('exit_conden_Cu'  , 'no'      , 1     , 'A' , 'exit_conden'                                         , phys_decomp ) 
    call addfld('exit_klclmkx_Cu' , 'no'      , 1     , 'A' , 'exit_klclmkx'                                        , phys_decomp ) 
    call addfld('exit_klfcmkx_Cu' , 'no'      , 1     , 'A' , 'exit_klfcmkx'                                        , phys_decomp ) 
    call addfld('exit_ufrc_Cu'    , 'no'      , 1     , 'A' , 'exit_ufrc'                                           , phys_decomp ) 
    call addfld('exit_wtw_Cu'     , 'no'      , 1     , 'A' , 'exit_wtw'                                            , phys_decomp ) 
    call addfld('exit_drycore_Cu' , 'no'      , 1     , 'A' , 'exit_drycore'                                        , phys_decomp ) 
    call addfld('exit_wu_Cu'      , 'no'      , 1     , 'A' , 'exit_wu'                                             , phys_decomp ) 
    call addfld('exit_cufilter_Cu', 'no'      , 1     , 'A' , 'exit_cufilter'                                       , phys_decomp ) 
    call addfld('exit_kinv1_Cu'   , 'no'      , 1     , 'A' , 'exit_kinv1'                                          , phys_decomp ) 
    call addfld('exit_rei_Cu'     , 'no'      , 1     , 'A' , 'exit_rei'                                            , phys_decomp ) 

    call addfld('limit_shcu_Cu'   , 'no'      , 1     , 'A' , 'limit_shcu'                                          , phys_decomp ) 
    call addfld('limit_negcon_Cu' , 'no'      , 1     , 'A' , 'limit_negcon'                                        , phys_decomp ) 
    call addfld('limit_ufrc_Cu'   , 'no'      , 1     , 'A' , 'limit_ufrc'                                          , phys_decomp ) 
    call addfld('limit_ppen_Cu'   , 'no'      , 1     , 'A' , 'limit_ppen'                                          , phys_decomp ) 
    call addfld('limit_emf_Cu'    , 'no'      , 1     , 'A' , 'limit_emf'                                           , phys_decomp ) 
    call addfld('limit_cinlcl_Cu' , 'no'      , 1     , 'A' , 'limit_cinlcl'                                        , phys_decomp ) 
    call addfld('limit_cin_Cu'    , 'no'      , 1     , 'A' , 'limit_cin'                                           , phys_decomp ) 
    call addfld('limit_cbmf_Cu'   , 'no'      , 1     , 'A' , 'limit_cbmf'                                          , phys_decomp ) 
    call addfld('limit_rei_Cu'    , 'no'      , 1     , 'A' , 'limit_rei'                                           , phys_decomp ) 
    call addfld('ind_delcin_Cu'   , 'no'      , 1     , 'A' , 'ind_delcin'                                          , phys_decomp ) 

    if( kind .ne. r8 ) then
        write(iulog,*) subname//': ERROR -- real KIND does not match internal specification.'
        call wrf_message(iulog)
        call endrun(subname//': ERROR -- real KIND does not match internal specification.')
    endif

    xlv   = xlv_in
    xlf   = xlf_in
    xls   = xlv + xlf
    cp    = cp_in
    zvir  = zvir_in
    r     = r_in
    g     = g_in
    ep2   = ep2_in
    p00   = 1.e5_r8
    rovcp = r/cp

    if (rpen == unset_r8) then
       call endrun(subname//': uwshcu_rpen must be set in the namelist')
    end if

    if ( masterproc ) then 
       write(iulog,*) subname//': tuning parameters: rpen=',rpen
       call wrf_debug(1,iulog)
    endif

    
    
    
    if(.not.restart)then
       do j=jts,jtf
          do k=kts,ktf
             do i=its,itf
                rushten(i,k,j)  = 0.
                rvshten(i,k,j)  = 0.
                rthshten(i,k,j) = 0.
                rqvshten(i,k,j) = 0.
                if( p_qc > param_first_scalar ) rqcshten(i,k,j) = 0.
                if( p_qr > param_first_scalar ) rqrshten(i,k,j) = 0.
                if( p_qi > param_first_scalar ) rqishten(i,k,j) = 0.
                if( p_qs > param_first_scalar ) rqsshten(i,k,j) = 0.
                if( p_qg > param_first_scalar ) rqgshten(i,k,j) = 0.
             enddo
          enddo
       enddo
    end if   

  end subroutine init_uwshcu

  subroutine compute_uwshcu_inv( mix      , mkx        , iend          , ncnst     , dt       ,  & 
                                 ps0_inv  , zs0_inv    , p0_inv        , z0_inv    , dp0_inv  ,  &
                                 u0_inv   , v0_inv     , qv0_inv       , ql0_inv   , qi0_inv  ,  &
                                 t0_inv   , s0_inv     , tr0_inv       ,                         &
                                 tke_inv  , cldfrct_inv, concldfrct_inv, pblh      , cush     ,  & 
                                 umf_inv  , slflx_inv  , qtflx_inv     ,                         & 
                                 flxprc1_inv, flxsnow1_inv,     				 &
                                 qvten_inv, qlten_inv  , qiten_inv     ,                         &
                                 sten_inv , uten_inv   , vten_inv      , trten_inv ,             &  
                                 qrten_inv, qsten_inv  , precip        , snow      , evapc_inv,  &
                                 cufrc_inv, qcu_inv    , qlu_inv       , qiu_inv   ,             &   
                                 cbmf     , qc_inv     , rliq          ,                         &
                                 cnt_inv  , cnb_inv    , qsat          , lchnk     , dpdry0_inv )

    implicit none
    integer , intent(in)    :: lchnk     
    integer , intent(in)    :: mix
    integer , intent(in)    :: mkx
    integer , intent(in)    :: iend
    integer , intent(in)    :: ncnst
    real(r8), intent(in)    :: dt                       
    real(r8), intent(in)    :: ps0_inv(mix,mkx+1)       
    real(r8), intent(in)    :: zs0_inv(mix,mkx+1)       
    real(r8), intent(in)    :: p0_inv(mix,mkx)          
    real(r8), intent(in)    :: z0_inv(mix,mkx)          
    real(r8), intent(in)    :: dp0_inv(mix,mkx)         
    real(r8), intent(in)    :: dpdry0_inv(mix,mkx)      
    real(r8), intent(in)    :: u0_inv(mix,mkx)          
    real(r8), intent(in)    :: v0_inv(mix,mkx)          
    real(r8), intent(in)    :: qv0_inv(mix,mkx)         
    real(r8), intent(in)    :: ql0_inv(mix,mkx)         
    real(r8), intent(in)    :: qi0_inv(mix,mkx)         
    real(r8), intent(in)    :: t0_inv(mix,mkx)          
    real(r8), intent(in)    :: s0_inv(mix,mkx)          
    real(r8), intent(in)    :: tr0_inv(mix,mkx,ncnst)   
    real(r8), intent(in)    :: tke_inv(mix,mkx+1)       
    real(r8), intent(in)    :: cldfrct_inv(mix,mkx)     
    real(r8), intent(in)    :: concldfrct_inv(mix,mkx)  
    real(r8), intent(in)    :: pblh(mix)                
    real(r8), intent(inout) :: cush(mix)                
    real(r8), intent(out)   :: umf_inv(mix,mkx+1)       
    real(r8), intent(out)   :: qvten_inv(mix,mkx)       
    real(r8), intent(out)   :: qlten_inv(mix,mkx)       
    real(r8), intent(out)   :: qiten_inv(mix,mkx)       
    real(r8), intent(out)   :: sten_inv(mix,mkx)        
    real(r8), intent(out)   :: uten_inv(mix,mkx)        
    real(r8), intent(out)   :: vten_inv(mix,mkx)        
    real(r8), intent(out)   :: trten_inv(mix,mkx,ncnst) 
    real(r8), intent(out)   :: qrten_inv(mix,mkx)       
    real(r8), intent(out)   :: qsten_inv(mix,mkx)       
    real(r8), intent(out)   :: precip(mix)              
    real(r8), intent(out)   :: snow(mix)                
    real(r8), intent(out)   :: evapc_inv(mix,mkx)       
    real(r8), intent(out)   :: rliq(mix)                
    real(r8), intent(out)   :: slflx_inv(mix,mkx+1)     
    real(r8), intent(out)   :: qtflx_inv(mix,mkx+1)     
    real(r8), intent(out)   :: flxprc1_inv(mix,mkx+1)   
    real(r8), intent(out)   :: flxsnow1_inv(mix,mkx+1)  

    real(r8), intent(out)   :: cufrc_inv(mix,mkx)       
    real(r8), intent(out)   :: qcu_inv(mix,mkx)         
    real(r8), intent(out)   :: qlu_inv(mix,mkx)         
    real(r8), intent(out)   :: qiu_inv(mix,mkx)         
    real(r8), intent(out)   :: qc_inv(mix,mkx)          
    real(r8), intent(out)   :: cbmf(mix)                
    real(r8), intent(out)   :: cnt_inv(mix)             
    real(r8), intent(out)   :: cnb_inv(mix)             
    integer , external      :: qsat                     

    real(r8)                :: ps0(mix,0:mkx)           
    real(r8)                :: zs0(mix,0:mkx)           
    real(r8)                :: p0(mix,mkx)              
    real(r8)                :: z0(mix,mkx)              
    real(r8)                :: dp0(mix,mkx)             
    real(r8)                :: dpdry0(mix,mkx)          
    real(r8)                :: u0(mix,mkx)              
    real(r8)                :: v0(mix,mkx)              
    real(r8)                :: tke(mix,0:mkx)           
    real(r8)                :: cldfrct(mix,mkx)         
    real(r8)                :: concldfrct(mix,mkx)      
    real(r8)                :: qv0(mix,mkx)             
    real(r8)                :: ql0(mix,mkx)             
    real(r8)                :: qi0(mix,mkx)             
    real(r8)                :: t0(mix,mkx)              
    real(r8)                :: s0(mix,mkx)              
    real(r8)                :: tr0(mix,mkx,ncnst)       
    real(r8)                :: umf(mix,0:mkx)           
    real(r8)                :: qvten(mix,mkx)           
    real(r8)                :: qlten(mix,mkx)           
    real(r8)                :: qiten(mix,mkx)           
    real(r8)                :: sten(mix,mkx)            
    real(r8)                :: uten(mix,mkx)            
    real(r8)                :: vten(mix,mkx)            
    real(r8)                :: trten(mix,mkx,ncnst)     
    real(r8)                :: qrten(mix,mkx)           
    real(r8)                :: qsten(mix,mkx)           
    real(r8)                :: evapc(mix,mkx)           
    real(r8)                :: slflx(mix,0:mkx)         
    real(r8)                :: qtflx(mix,0:mkx)         
    real(r8)                :: flxprc1(mix,0:mkx)       
    real(r8)                :: flxsnow1(mix,0:mkx)      
    real(r8)                :: cufrc(mix,mkx)           
    real(r8)                :: qcu(mix,mkx)             
    real(r8)                :: qlu(mix,mkx)             
    real(r8)                :: qiu(mix,mkx)             
    real(r8)                :: qc(mix,mkx)              
    real(r8)                :: cnt(mix)                 
    real(r8)                :: cnb(mix)                 
    integer                 :: k                        
    integer                 :: k_inv                    
    integer                 :: m                        

    do k = 1, mkx
       k_inv               = mkx + 1 - k
       p0(:iend,k)         = p0_inv(:iend,k_inv)
       u0(:iend,k)         = u0_inv(:iend,k_inv)
       v0(:iend,k)         = v0_inv(:iend,k_inv)
       z0(:iend,k)         = z0_inv(:iend,k_inv)
       dp0(:iend,k)        = dp0_inv(:iend,k_inv)
       dpdry0(:iend,k)     = dpdry0_inv(:iend,k_inv)
       qv0(:iend,k)        = qv0_inv(:iend,k_inv)
       ql0(:iend,k)        = ql0_inv(:iend,k_inv)
       qi0(:iend,k)        = qi0_inv(:iend,k_inv)
       t0(:iend,k)         = t0_inv(:iend,k_inv)
       s0(:iend,k)         = s0_inv(:iend,k_inv)
       cldfrct(:iend,k)    = cldfrct_inv(:iend,k_inv)
       concldfrct(:iend,k) = concldfrct_inv(:iend,k_inv)
       do m = 1, ncnst
          tr0(:iend,k,m)   = tr0_inv(:iend,k_inv,m)
       enddo
    enddo
    
    do k = 0, mkx
       k_inv               = mkx + 1 - k
       ps0(:iend,k)        = ps0_inv(:iend,k_inv)
       zs0(:iend,k)        = zs0_inv(:iend,k_inv)
       tke(:iend,k)        = tke_inv(:iend,k_inv)
    end do

    call compute_uwshcu( mix  , mkx    , iend      , ncnst , dt   , &
                         ps0  , zs0    , p0        , z0    , dp0  , &
                         u0   , v0     , qv0       , ql0   , qi0  , & 
                         t0   , s0     , tr0       ,                & 
                         tke  , cldfrct, concldfrct, pblh  , cush , & 
                         umf  , slflx  , qtflx     ,                &  
                         flxprc1  , flxsnow1  ,		            &
                         qvten, qlten  , qiten     ,                & 
                         sten , uten   , vten      , trten ,        &
                         qrten, qsten  , precip    , snow  , evapc, &
                         cufrc, qcu    , qlu       , qiu   ,        &
                         cbmf , qc     , rliq      ,                &
                         cnt  , cnb    , qsat      , lchnk , dpdry0 )

    

       cnt_inv(:iend) = mkx + 1 - cnt(:iend)
       cnb_inv(:iend) = mkx + 1 - cnb(:iend)

    do k = 0, mkx
       k_inv                  = mkx + 1 - k
       umf_inv(:iend,k_inv)   = umf(:iend,k)       
       slflx_inv(:iend,k_inv) = slflx(:iend,k)     
       qtflx_inv(:iend,k_inv) = qtflx(:iend,k)
       flxprc1_inv(:iend,k_inv) = flxprc1(:iend,k)     
       flxsnow1_inv(:iend,k_inv) = flxsnow1(:iend,k)   
    end do

    do k = 1, mkx
       k_inv                         = mkx + 1 - k
       qvten_inv(:iend,k_inv)        = qvten(:iend,k)   
       qlten_inv(:iend,k_inv)        = qlten(:iend,k)   
       qiten_inv(:iend,k_inv)        = qiten(:iend,k)   
       sten_inv(:iend,k_inv)         = sten(:iend,k)    
       uten_inv(:iend,k_inv)         = uten(:iend,k)    
       vten_inv(:iend,k_inv)         = vten(:iend,k)    
       qrten_inv(:iend,k_inv)        = qrten(:iend,k)   
       qsten_inv(:iend,k_inv)        = qsten(:iend,k)   
       evapc_inv(:iend,k_inv)        = evapc(:iend,k)
       cufrc_inv(:iend,k_inv)        = cufrc(:iend,k)   
       qcu_inv(:iend,k_inv)          = qcu(:iend,k)     
       qlu_inv(:iend,k_inv)          = qlu(:iend,k)     
       qiu_inv(:iend,k_inv)          = qiu(:iend,k)     
       qc_inv(:iend,k_inv)           = qc(:iend,k)      
       do m = 1, ncnst
          trten_inv(:iend,k_inv,m)   = trten(:iend,k,m) 
       enddo

    enddo

  end subroutine compute_uwshcu_inv

  subroutine compute_uwshcu( mix      , mkx       , iend         , ncnst    , dt        , &
                             ps0_in   , zs0_in    , p0_in        , z0_in    , dp0_in    , &
                             u0_in    , v0_in     , qv0_in       , ql0_in   , qi0_in    , &
                             t0_in    , s0_in     , tr0_in       ,                        &
                             tke_in   , cldfrct_in, concldfrct_in,  pblh_in , cush_inout, & 
                             umf_out  , slflx_out , qtflx_out    ,                        &
                             flxprc1_out  , flxsnow1_out  , 			 	  &
                             qvten_out, qlten_out , qiten_out    ,                        & 
                             sten_out , uten_out  , vten_out     , trten_out,             &
                             qrten_out, qsten_out , precip_out   , snow_out , evapc_out , &
                             cufrc_out, qcu_out   , qlu_out      , qiu_out  ,             &
                             cbmf_out , qc_out    , rliq_out     ,                        &
                             cnt_out  , cnb_out   , qsat         , lchnk    , dpdry0_in )

    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    use module_cam_support, only : outfld, addfld, phys_decomp
    use constituents,    only : qmin, cnst_get_type_byind, cnst_get_ind
    use modal_aero_data, only : ntot_amode, numptr_amode

    implicit none

    
    
    

    integer , intent(in)    :: lchnk
    integer , intent(in)    :: mix
    integer , intent(in)    :: mkx
    integer , intent(in)    :: iend
    integer , intent(in)    :: ncnst
    real(r8), intent(in)    :: dt                             
    real(r8), intent(in)    :: ps0_in(mix,0:mkx)              
    real(r8), intent(in)    :: zs0_in(mix,0:mkx)              
    real(r8), intent(in)    :: p0_in(mix,mkx)                 
    real(r8), intent(in)    :: z0_in(mix,mkx)                 
    real(r8), intent(in)    :: dp0_in(mix,mkx)                
    real(r8), intent(in)    :: dpdry0_in(mix,mkx)             
    real(r8), intent(in)    :: u0_in(mix,mkx)                 
    real(r8), intent(in)    :: v0_in(mix,mkx)                 
    real(r8), intent(in)    :: qv0_in(mix,mkx)                
    real(r8), intent(in)    :: ql0_in(mix,mkx)                
    real(r8), intent(in)    :: qi0_in(mix,mkx)                
    real(r8), intent(in)    :: t0_in(mix,mkx)                 
    real(r8), intent(in)    :: s0_in(mix,mkx)                 
    real(r8), intent(in)    :: tr0_in(mix,mkx,ncnst)          
    real(r8), intent(in)    :: tke_in(mix,0:mkx)              
    real(r8), intent(in)    :: cldfrct_in(mix,mkx)            
    real(r8), intent(in)    :: concldfrct_in(mix,mkx)         
    real(r8), intent(in)    :: pblh_in(mix)                   
    real(r8), intent(inout) :: cush_inout(mix)                

    real(r8)                   tw0_in(mix,mkx)                
    real(r8)                   qw0_in(mix,mkx)                

    real(r8), intent(out)   :: umf_out(mix,0:mkx)             
    real(r8), intent(out)   :: qvten_out(mix,mkx)             
    real(r8), intent(out)   :: qlten_out(mix,mkx)             
    real(r8), intent(out)   :: qiten_out(mix,mkx)             
    real(r8), intent(out)   :: sten_out(mix,mkx)              
    real(r8), intent(out)   :: uten_out(mix,mkx)              
    real(r8), intent(out)   :: vten_out(mix,mkx)              
    real(r8), intent(out)   :: trten_out(mix,mkx,ncnst)       
    real(r8), intent(out)   :: qrten_out(mix,mkx)             
    real(r8), intent(out)   :: qsten_out(mix,mkx)             
    real(r8), intent(out)   :: precip_out(mix)                
    real(r8), intent(out)   :: snow_out(mix)                  
    real(r8), intent(out)   :: evapc_out(mix,mkx)             
    real(r8), intent(out)   :: slflx_out(mix,0:mkx)           
    real(r8), intent(out)   :: qtflx_out(mix,0:mkx)           
    real(r8), intent(out)   :: flxprc1_out(mix,0:mkx)         
    real(r8), intent(out)   :: flxsnow1_out(mix,0:mkx)        
    real(r8), intent(out)   :: cufrc_out(mix,mkx)             
    real(r8), intent(out)   :: qcu_out(mix,mkx)               
    real(r8), intent(out)   :: qlu_out(mix,mkx)               
    real(r8), intent(out)   :: qiu_out(mix,mkx)               
    real(r8), intent(out)   :: cbmf_out(mix)                  
    real(r8), intent(out)   :: qc_out(mix,mkx)                
    real(r8), intent(out)   :: rliq_out(mix)                  
    real(r8), intent(out)   :: cnt_out(mix)                   
    real(r8), intent(out)   :: cnb_out(mix)                   

    
    
    

    integer , external      :: qsat 
    real(r8)                   qtten_out(mix,mkx)             
    real(r8)                   slten_out(mix,mkx)             
    real(r8)                   ufrc_out(mix,0:mkx)            
    real(r8)                   uflx_out(mix,0:mkx)            
    real(r8)                   vflx_out(mix,0:mkx)            
    real(r8)                   fer_out(mix,mkx)               
    real(r8)                   fdr_out(mix,mkx)               
    real(r8)                   cinh_out(mix)                  
    real(r8)                   trflx_out(mix,0:mkx,ncnst)     
   
    
    
    

    

    real(r8)    ps0(0:mkx)                                    
    real(r8)    zs0(0:mkx)                                    
    real(r8)    p0(mkx)                                       
    real(r8)    z0(mkx)                                       
    real(r8)    dp0(mkx)                                      
    real(r8)    dpdry0(mkx)                                   
    real(r8)    u0(mkx)                                       
    real(r8)    v0(mkx)                                       
    real(r8)    tke(0:mkx)                                    
    real(r8)    cldfrct(mkx)                                  
    real(r8)    concldfrct(mkx)                               
    real(r8)    qv0(mkx)                                      
    real(r8)    ql0(mkx)                                      
    real(r8)    qi0(mkx)                                      
    real(r8)    t0(mkx)                                       
    real(r8)    s0(mkx)                                       
    real(r8)    pblh                                          
    real(r8)    cush                                          
    real(r8)    tr0(mkx,ncnst)                                

    

    real(r8)    qt0(mkx)                                      
    real(r8)    thl0(mkx)                                     
    real(r8)    thvl0(mkx)                                    
    real(r8)    ssqt0(mkx)                                    
    real(r8)    ssthl0(mkx)                                   
    real(r8)    ssu0(mkx)                                     
    real(r8)    ssv0(mkx)                                     
    real(r8)    thv0bot(mkx)                                  
    real(r8)    thv0top(mkx)                                  
    real(r8)    thvl0bot(mkx)                                 
    real(r8)    thvl0top(mkx)                                 
    real(r8)    exn0(mkx)                                     
    real(r8)    exns0(0:mkx)                                  
    real(r8)    sstr0(mkx,ncnst)                              

   

    real(r8)    qv0_star(mkx)                                 
    real(r8)    ql0_star(mkx)                                 
    real(r8)    qi0_star(mkx)                                 
    real(r8)    t0_star(mkx)                                  
    real(r8)    s0_star(mkx)                                  

   

    real(r8)    umf(0:mkx)                                    
    real(r8)    emf(0:mkx)                                    
    real(r8)    qvten(mkx)                                    
    real(r8)    qlten(mkx)                                    
    real(r8)    qiten(mkx)                                    
    real(r8)    sten(mkx)                                     
    real(r8)    uten(mkx)                                     
    real(r8)    vten(mkx)                                     
    real(r8)    qrten(mkx)                                    
    real(r8)    qsten(mkx)                                    
    real(r8)    precip                                        
    real(r8)    snow                                          
    real(r8)    evapc(mkx)                                    
    real(r8)    slflx(0:mkx)                                  
    real(r8)    qtflx(0:mkx)                                  
    real(r8)    uflx(0:mkx)                                   
    real(r8)    vflx(0:mkx)                                   
    real(r8)    cufrc(mkx)                                    
    real(r8)    qcu(mkx)                                      
    real(r8)    qlu(mkx)                                      
    real(r8)    qiu(mkx)                                      
    real(r8)    dwten(mkx)                                    
    real(r8)    diten(mkx)                                    
    real(r8)    fer(mkx)                                      
    real(r8)    fdr(mkx)                                      
    real(r8)    uf(mkx)                                       
    real(r8)    vf(mkx)                                       
    real(r8)    qc(mkx)                                       
    real(r8)    qc_l(mkx)                                     
    real(r8)    qc_i(mkx)                                     
    real(r8)    qc_lm
    real(r8)    qc_im
    real(r8)    nc_lm
    real(r8)    nc_im
    real(r8)    ql_emf_kbup
    real(r8)    qi_emf_kbup
    real(r8)    nl_emf_kbup
    real(r8)    ni_emf_kbup
    real(r8)    qlten_det
    real(r8)    qiten_det
    real(r8)    rliq                                          
    real(r8)    cnt                                           
    real(r8)    cnb                                           
    real(r8)    qtten(mkx)                                    
    real(r8)    slten(mkx)                                    
    real(r8)    ufrc(0:mkx)                                   
    real(r8)    trten(mkx,ncnst)                              
    real(r8)    trflx(0:mkx,ncnst)                            
    real(r8)    trflx_d(0:mkx)                                
    real(r8)    trflx_u(0:mkx)                                
    real(r8)    trmin                                         
    real(r8)    pdelx, dum 
    
    
    

    real(r8)    uemf(0:mkx)                                   
    real(r8)    comsub(mkx)                                   
    real(r8)    qlten_sink(mkx)                               
    real(r8)    qiten_sink(mkx)                               
    real(r8)    nlten_sink(mkx)                               
    real(r8)    niten_sink(mkx)                               
    real(r8)    thlten_sub, qtten_sub                         
    real(r8)    qlten_sub, qiten_sub                          
    real(r8)    nlten_sub, niten_sub                          
    real(r8)    thl_prog, qt_prog                             

    

    real(r8)    wu(0:mkx)                                     
    real(r8)    thlu(0:mkx)                                   
    real(r8)    qtu(0:mkx)                                    
    real(r8)    uu(0:mkx)                                     
    real(r8)    vu(0:mkx)                                     
    real(r8)    thvu(0:mkx)                                   
    real(r8)    rei(mkx)                                      
    real(r8)    tru(0:mkx,ncnst)                              

    
    
    
    

    real(r8)    thlu_emf(0:mkx)                               
    real(r8)    qtu_emf(0:mkx)                                
    real(r8)    uu_emf(0:mkx)                                 
    real(r8)    vu_emf(0:mkx)                                 
    real(r8)    tru_emf(0:mkx,ncnst)                          

    

    real(r8)    flxrain(0:mkx)                                
    real(r8)    flxsnow(0:mkx)                                
    real(r8)    ntraprd(mkx)                                  
    real(r8)    ntsnprd(mkx)                                  
    real(r8)    flxsntm                                       
    real(r8)    snowmlt                                       
    real(r8)    subsat                                        
    real(r8)    evprain                                       
    real(r8)    evpsnow                                       
    real(r8)    evplimit                                      
    real(r8)    evplimit_rain                                 
    real(r8)    evplimit_snow                                 
    real(r8)    evpint_rain                                   
    real(r8)    evpint_snow                                   
    real(r8)    kevp                                          

    

    integer     kk, mm, k, i, m, kp1, km1
    integer     iter_scaleh, iter_xc
    integer     id_check, status
    integer     klcl                                          
    integer     kinv                                          
    integer     krel                                          
    integer     klfc                                          
    integer     kbup                                          
    integer     kpen                                          
    logical     id_exit   
    logical     forcedCu                                      
    real(r8)    thlsrc, qtsrc, usrc, vsrc, thvlsrc            
    real(r8)    PGFc, uplus, vplus
    real(r8)    trsrc(ncnst), tre(ncnst)
    real(r8)    plcl, plfc, prel, wrel
    real(r8)    frc_rasn
    real(r8)    ee2, ud2, wtw, wtwb, wtwh
    real(r8)    xc, xc_2                                       
    real(r8)    cldhgt, scaleh, tscaleh, cridis, rle, rkm
    real(r8)    rkfre, sigmaw, epsvarw, tkeavg, dpsum, dpi, thvlmin
    real(r8)    thlxsat, qtxsat, thvxsat, x_cu, x_en, thv_x0, thv_x1
    real(r8)    thj, qvj, qlj, qij, thvj, tj, thv0j, rho0j, rhos0j, qse 
    real(r8)    cin, cinlcl
    real(r8)    pe, dpe, exne, thvebot, thle, qte, ue, ve, thlue, qtue, wue
    real(r8)    mu, mumin0, mumin1, mumin2, mulcl, mulclstar
    real(r8)    cbmf, wcrit, winv, wlcl, ufrcinv, ufrclcl, rmaxfrac
    real(r8)    criqc, exql, exqi, ppen
    real(r8)    thl0top, thl0bot, qt0bot, qt0top, thvubot, thvutop
    real(r8)    thlu_top, qtu_top, qlu_top, qiu_top, qlu_mid, qiu_mid, exntop
    real(r8)    thl0lcl, qt0lcl, thv0lcl, thv0rel, rho0inv, autodet
    real(r8)    aquad, bquad, cquad, xc1, xc2, excessu, excess0, xsat, xs1, xs2
    real(r8)    bogbot, bogtop, delbog, drage, expfac, rbuoy, rdrag
    real(r8)    rcwp, rlwp, riwp, qcubelow, qlubelow, qiubelow
    real(r8)    rainflx, snowflx                     
    real(r8)    es(1)                               
    real(r8)    qs(1)                               
    real(r8)    gam(1)                                        
    real(r8)    qsat_arg             
    real(r8)    xsrc, xmean, xtop, xbot, xflx(0:mkx)
    real(r8)    tmp1, tmp2

    

    real(r8)  ufrcinvbase_out(mix)                            
    real(r8)  ufrclcl_out(mix)                                
    real(r8)  winvbase_out(mix)                               
    real(r8)  wlcl_out(mix)                                   
    real(r8)  plcl_out(mix)                                   
    real(r8)  pinv_out(mix)                                   
    real(r8)  plfc_out(mix)                                   
    real(r8)  pbup_out(mix)                                   
    real(r8)  ppen_out(mix)                                   
    real(r8)  qtsrc_out(mix)                                  
    real(r8)  thlsrc_out(mix)                                 
    real(r8)  thvlsrc_out(mix)                                
    real(r8)  emfkbup_out(mix)                                
    real(r8)  cinlclh_out(mix)                                
    real(r8)  tkeavg_out(mix)                                 
    real(r8)  cbmflimit_out(mix)                              
    real(r8)  zinv_out(mix)                                   
    real(r8)  rcwp_out(mix)                                   
    real(r8)  rlwp_out(mix)                                   
    real(r8)  riwp_out(mix)                                   
    real(r8)  wu_out(mix,0:mkx)                               
    real(r8)  qtu_out(mix,0:mkx)                              
    real(r8)  thlu_out(mix,0:mkx)                             
    real(r8)  thvu_out(mix,0:mkx)                             
    real(r8)  uu_out(mix,0:mkx)                               
    real(r8)  vu_out(mix,0:mkx)                               
    real(r8)  qtu_emf_out(mix,0:mkx)                          
    real(r8)  thlu_emf_out(mix,0:mkx)                         
    real(r8)  uu_emf_out(mix,0:mkx)                           
    real(r8)  vu_emf_out(mix,0:mkx)                           
    real(r8)  uemf_out(mix,0:mkx)                             
    real(r8)  tru_out(mix,0:mkx,ncnst)                        
    real(r8)  tru_emf_out(mix,0:mkx,ncnst)                    

    real(r8)  wu_s(0:mkx)                                     
    real(r8)  qtu_s(0:mkx)
    real(r8)  thlu_s(0:mkx)
    real(r8)  thvu_s(0:mkx)
    real(r8)  uu_s(0:mkx)
    real(r8)  vu_s(0:mkx)
    real(r8)  qtu_emf_s(0:mkx) 
    real(r8)  thlu_emf_s(0:mkx)  
    real(r8)  uu_emf_s(0:mkx)   
    real(r8)  vu_emf_s(0:mkx)
    real(r8)  uemf_s(0:mkx)   
    real(r8)  tru_s(0:mkx,ncnst)
    real(r8)  tru_emf_s(0:mkx,ncnst)   

    real(r8)  dwten_out(mix,mkx)
    real(r8)  diten_out(mix,mkx)
    real(r8)  flxrain_out(mix,0:mkx)  
    real(r8)  flxsnow_out(mix,0:mkx)  
    real(r8)  ntraprd_out(mix,mkx)    
    real(r8)  ntsnprd_out(mix,mkx)    

    real(r8)  dwten_s(mkx)
    real(r8)  diten_s(mkx)
    real(r8)  flxrain_s(0:mkx)  
    real(r8)  flxsnow_s(0:mkx)  
    real(r8)  ntraprd_s(mkx)    
    real(r8)  ntsnprd_s(mkx)    

    real(r8)  excessu_arr_out(mix,mkx)
    real(r8)  excessu_arr(mkx) 
    real(r8)  excessu_arr_s(mkx)
    real(r8)  excess0_arr_out(mix,mkx)
    real(r8)  excess0_arr(mkx)
    real(r8)  excess0_arr_s(mkx)
    real(r8)  xc_arr_out(mix,mkx)
    real(r8)  xc_arr(mkx)
    real(r8)  xc_arr_s(mkx)
    real(r8)  aquad_arr_out(mix,mkx)
    real(r8)  aquad_arr(mkx)
    real(r8)  aquad_arr_s(mkx)
    real(r8)  bquad_arr_out(mix,mkx)
    real(r8)  bquad_arr(mkx)
    real(r8)  bquad_arr_s(mkx)
    real(r8)  cquad_arr_out(mix,mkx) 
    real(r8)  cquad_arr(mkx)
    real(r8)  cquad_arr_s(mkx)
    real(r8)  bogbot_arr_out(mix,mkx)
    real(r8)  bogbot_arr(mkx)
    real(r8)  bogbot_arr_s(mkx)
    real(r8)  bogtop_arr_out(mix,mkx)
    real(r8)  bogtop_arr(mkx)
    real(r8)  bogtop_arr_s(mkx)

    real(r8)  exit_UWCu(mix)
    real(r8)  exit_conden(mix)
    real(r8)  exit_klclmkx(mix)
    real(r8)  exit_klfcmkx(mix)
    real(r8)  exit_ufrc(mix)
    real(r8)  exit_wtw(mix)
    real(r8)  exit_drycore(mix)
    real(r8)  exit_wu(mix)
    real(r8)  exit_cufilter(mix)
    real(r8)  exit_kinv1(mix)
    real(r8)  exit_rei(mix)

    real(r8)  limit_shcu(mix)
    real(r8)  limit_negcon(mix)
    real(r8)  limit_ufrc(mix)
    real(r8)  limit_ppen(mix)
    real(r8)  limit_emf(mix)
    real(r8)  limit_cinlcl(mix)
    real(r8)  limit_cin(mix)
    real(r8)  limit_cbmf(mix)
    real(r8)  limit_rei(mix)
    real(r8)  ind_delcin(mix)

    real(r8) :: ufrcinvbase_s, ufrclcl_s, winvbase_s, wlcl_s, plcl_s, pinv_s, plfc_s, &
                qtsrc_s, thlsrc_s, thvlsrc_s, emfkbup_s, cinlcl_s, pbup_s, ppen_s, cbmflimit_s, &
                tkeavg_s, zinv_s, rcwp_s, rlwp_s, riwp_s 
    real(r8) :: ufrcinvbase, winvbase, pinv, zinv, emfkbup, cbmflimit, rho0rel  

    

    real(r8), dimension(mkx)         :: qv0_s  , ql0_s   , qi0_s   , s0_s    , u0_s    ,           & 
                                        v0_s   , t0_s    , qt0_s   , thl0_s  , thvl0_s , qvten_s , &
                                        qlten_s, qiten_s , qrten_s , qsten_s , sten_s  , evapc_s , &
                                        uten_s , vten_s  , cufrc_s , qcu_s   , qlu_s   , qiu_s   , &
                                        fer_s  , fdr_s   , qc_s    , qtten_s , slten_s 
    real(r8), dimension(0:mkx)       :: umf_s  , slflx_s , qtflx_s , ufrc_s  , uflx_s , vflx_s
    real(r8)                         :: cush_s , precip_s, snow_s  , cin_s   , rliq_s, cbmf_s, cnt_s, cnb_s
    real(r8)                         :: cin_i,cin_f,del_CIN,ke,alpha,thlj
    real(r8)                         :: cinlcl_i,cinlcl_f,del_cinlcl
    integer                          :: iter

    real(r8), dimension(mkx,ncnst)   :: tr0_s, trten_s
    real(r8), dimension(0:mkx,ncnst) :: trflx_s

    

    real(r8), dimension(mkx)         :: qv0_o, ql0_o, qi0_o, t0_o, s0_o, u0_o, v0_o
    real(r8), dimension(mkx)         :: qt0_o    , thl0_o   , thvl0_o   ,                         &
                                        qvten_o  , qlten_o  , qiten_o   , qrten_o   , qsten_o ,   &
                                        sten_o   , uten_o   , vten_o    , qcu_o     , qlu_o   ,   & 
                                        qiu_o    , cufrc_o  , evapc_o   ,                         &
                                        thv0bot_o, thv0top_o, thvl0bot_o, thvl0top_o,             &
                                        ssthl0_o , ssqt0_o  , ssu0_o    , ssv0_o    , qc_o    ,   &
                                        qtten_o  , slten_o  
    real(r8), dimension(0:mkx)       :: umf_o    , slflx_o  , qtflx_o   , ufrc_o 
    real(r8), dimension(mix)         :: cush_o   , precip_o , snow_o    , rliq_o, cbmf_o, cnt_o, cnb_o
    real(r8), dimension(0:mkx)       :: uflx_o   , vflx_o
    real(r8)                         :: tkeavg_o , thvlmin_o, qtsrc_o  , thvlsrc_o, thlsrc_o ,    &
                                        usrc_o   , vsrc_o   , plcl_o   , plfc_o   ,               &
                                        thv0lcl_o, cinlcl_o 
    integer                          :: kinv_o   , klcl_o   , klfc_o  

    real(r8), dimension(mkx,ncnst)   :: tr0_o
    real(r8), dimension(mkx,ncnst)   :: trten_o, sstr0_o  
    real(r8), dimension(0:mkx,ncnst) :: trflx_o
    real(r8), dimension(ncnst)       :: trsrc_o
    integer                          :: ixnumliq, ixnumice

    
    
    
    
    

    
    
    

    integer , parameter              :: niter_xc = 2

    
    
    

    logical , parameter              :: use_CINcin = .true.

    
    
    
    
    
    
    
    
    
    
    
    
    
    
    

    integer , parameter              :: iter_cin = 2

    
    
    
    

    logical , parameter              :: use_self_detrain = .false.
    
    
    
    

    logical , parameter              :: use_momenflx = .true.

    
    
    
    

    logical , parameter              :: use_cumpenent = .true.

    
    
    
    
    

    logical , parameter              :: use_expconten = .true.

    
    
    
    
    

    logical , parameter              :: use_unicondet = .false.

    
    
    

    parameter (rle = 0.1_r8)         

    parameter (rkm = 14.0_r8)        

    parameter (rkfre = 1.0_r8)       
    parameter (rmaxfrac = 0.10_r8)   
    parameter (mumin1 = 0.906_r8)    
                                     
                                     
    parameter (rbuoy = 1.0_r8)       
    parameter (rdrag = 1.0_r8)       

    parameter (epsvarw = 5.e-4_r8)   
    parameter (PGFc = 0.7_r8)        
                                     

    
    
    
    
    
    
    
    
    
    
    
    

    parameter ( criqc    = 0.7e-3_r8 ) 
    parameter ( frc_rasn = 1.0_r8    )
    parameter ( kevp     = 2.e-6_r8  )
    logical, parameter :: noevap_krelkpen = .false.

    
    
    
    
    

    call cnst_get_ind( 'NUMLIQ', ixnumliq )
    call cnst_get_ind( 'NUMICE', ixnumice )

    
    
    

    umf_out(:iend,0:mkx)         = 0.0_r8
    slflx_out(:iend,0:mkx)       = 0.0_r8
    qtflx_out(:iend,0:mkx)       = 0.0_r8
    flxprc1_out(:iend,0:mkx)     = 0.0_r8
    flxsnow1_out(:iend,0:mkx)    = 0.0_r8
    qvten_out(:iend,:mkx)        = 0.0_r8
    qlten_out(:iend,:mkx)        = 0.0_r8
    qiten_out(:iend,:mkx)        = 0.0_r8
    sten_out(:iend,:mkx)         = 0.0_r8
    uten_out(:iend,:mkx)         = 0.0_r8
    vten_out(:iend,:mkx)         = 0.0_r8
    qrten_out(:iend,:mkx)        = 0.0_r8
    qsten_out(:iend,:mkx)        = 0.0_r8
    precip_out(:iend)            = 0.0_r8
    snow_out(:iend)              = 0.0_r8
    evapc_out(:iend,:mkx)        = 0.0_r8
    cufrc_out(:iend,:mkx)        = 0.0_r8
    qcu_out(:iend,:mkx)          = 0.0_r8
    qlu_out(:iend,:mkx)          = 0.0_r8
    qiu_out(:iend,:mkx)          = 0.0_r8
    fer_out(:iend,:mkx)          = 0.0_r8
    fdr_out(:iend,:mkx)          = 0.0_r8
    cinh_out(:iend)              = -1.0_r8
    cinlclh_out(:iend)           = -1.0_r8
    cbmf_out(:iend)              = 0.0_r8
    qc_out(:iend,:mkx)           = 0.0_r8
    rliq_out(:iend)              = 0.0_r8
    cnt_out(:iend)               = real(mkx, r8)
    cnb_out(:iend)               = 0.0_r8
    qtten_out(:iend,:mkx)        = 0.0_r8
    slten_out(:iend,:mkx)        = 0.0_r8
    ufrc_out(:iend,0:mkx)        = 0.0_r8

    uflx_out(:iend,0:mkx)        = 0.0_r8
    vflx_out(:iend,0:mkx)        = 0.0_r8

    trten_out(:iend,:mkx,:ncnst) = 0.0_r8
    trflx_out(:iend,0:mkx,:ncnst)= 0.0_r8
    
    ufrcinvbase_out(:iend)       = 0.0_r8
    ufrclcl_out(:iend)           = 0.0_r8
    winvbase_out(:iend)          = 0.0_r8
    wlcl_out(:iend)              = 0.0_r8
    plcl_out(:iend)              = 0.0_r8
    pinv_out(:iend)              = 0.0_r8
    plfc_out(:iend)              = 0.0_r8
    pbup_out(:iend)              = 0.0_r8
    ppen_out(:iend)              = 0.0_r8
    qtsrc_out(:iend)             = 0.0_r8
    thlsrc_out(:iend)            = 0.0_r8
    thvlsrc_out(:iend)           = 0.0_r8
    emfkbup_out(:iend)           = 0.0_r8
    cbmflimit_out(:iend)         = 0.0_r8
    tkeavg_out(:iend)            = 0.0_r8
    zinv_out(:iend)              = 0.0_r8
    rcwp_out(:iend)              = 0.0_r8
    rlwp_out(:iend)              = 0.0_r8
    riwp_out(:iend)              = 0.0_r8

    wu_out(:iend,0:mkx)          = 0.0_r8
    qtu_out(:iend,0:mkx)         = 0.0_r8
    thlu_out(:iend,0:mkx)        = 0.0_r8
    thvu_out(:iend,0:mkx)        = 0.0_r8
    uu_out(:iend,0:mkx)          = 0.0_r8
    vu_out(:iend,0:mkx)          = 0.0_r8
    qtu_emf_out(:iend,0:mkx)     = 0.0_r8
    thlu_emf_out(:iend,0:mkx)    = 0.0_r8
    uu_emf_out(:iend,0:mkx)      = 0.0_r8
    vu_emf_out(:iend,0:mkx)      = 0.0_r8
    uemf_out(:iend,0:mkx)        = 0.0_r8

    tru_out(:iend,0:mkx,:ncnst)     = 0.0_r8
    tru_emf_out(:iend,0:mkx,:ncnst) = 0.0_r8

    dwten_out(:iend,:mkx)        = 0.0_r8
    diten_out(:iend,:mkx)        = 0.0_r8
    flxrain_out(:iend,0:mkx)     = 0.0_r8  
    flxsnow_out(:iend,0:mkx)     = 0.0_r8
    ntraprd_out(:iend,mkx)       = 0.0_r8
    ntsnprd_out(:iend,mkx)       = 0.0_r8

    excessu_arr_out(:iend,:mkx)  = 0.0_r8
    excess0_arr_out(:iend,:mkx)  = 0.0_r8
    xc_arr_out(:iend,:mkx)       = 0.0_r8
    aquad_arr_out(:iend,:mkx)    = 0.0_r8
    bquad_arr_out(:iend,:mkx)    = 0.0_r8
    cquad_arr_out(:iend,:mkx)    = 0.0_r8
    bogbot_arr_out(:iend,:mkx)   = 0.0_r8
    bogtop_arr_out(:iend,:mkx)   = 0.0_r8

    exit_UWCu(:iend)             = 0.0_r8 
    exit_conden(:iend)           = 0.0_r8 
    exit_klclmkx(:iend)          = 0.0_r8 
    exit_klfcmkx(:iend)          = 0.0_r8 
    exit_ufrc(:iend)             = 0.0_r8 
    exit_wtw(:iend)              = 0.0_r8 
    exit_drycore(:iend)          = 0.0_r8 
    exit_wu(:iend)               = 0.0_r8 
    exit_cufilter(:iend)         = 0.0_r8 
    exit_kinv1(:iend)            = 0.0_r8 
    exit_rei(:iend)              = 0.0_r8 

    limit_shcu(:iend)            = 0.0_r8 
    limit_negcon(:iend)          = 0.0_r8 
    limit_ufrc(:iend)            = 0.0_r8
    limit_ppen(:iend)            = 0.0_r8
    limit_emf(:iend)             = 0.0_r8
    limit_cinlcl(:iend)          = 0.0_r8
    limit_cin(:iend)             = 0.0_r8
    limit_cbmf(:iend)            = 0.0_r8
    limit_rei(:iend)             = 0.0_r8

    ind_delcin(:iend)            = 0.0_r8

    
    
    
    
    

    
    

    call findsp( lchnk, iend, qv0_in, t0_in, p0_in, tw0_in, qw0_in )

    do i = 1, iend                                      

      id_exit = .false.

      
      
      

      ps0(0:mkx)       = ps0_in(i,0:mkx)
      zs0(0:mkx)       = zs0_in(i,0:mkx)
      p0(:mkx)         = p0_in(i,:mkx)
      z0(:mkx)         = z0_in(i,:mkx)
      dp0(:mkx)        = dp0_in(i,:mkx)
      dpdry0(:mkx)     = dpdry0_in(i,:mkx)
      u0(:mkx)         = u0_in(i,:mkx)
      v0(:mkx)         = v0_in(i,:mkx)
      qv0(:mkx)        = qv0_in(i,:mkx)
      ql0(:mkx)        = ql0_in(i,:mkx)
      qi0(:mkx)        = qi0_in(i,:mkx)
      t0(:mkx)         = t0_in(i,:mkx)
      s0(:mkx)         = s0_in(i,:mkx)
      tke(0:mkx)       = tke_in(i,0:mkx)
      cldfrct(:mkx)    = cldfrct_in(i,:mkx)
      concldfrct(:mkx) = concldfrct_in(i,:mkx)
      pblh             = pblh_in(i)
      cush             = cush_inout(i)
      do m = 1, ncnst
         tr0(:mkx,m)   = tr0_in(i,:mkx,m)
      enddo

      
      
      
      

      
      
      exn0(:mkx)   = (p0(:mkx)/p00)**rovcp
      exns0(0:mkx) = (ps0(0:mkx)/p00)**rovcp
      qt0(:mkx)    = (qv0(:mkx) + ql0(:mkx) + qi0(:mkx))
      thl0(:mkx)   = (t0(:mkx) - xlv*ql0(:mkx)/cp - xls*qi0(:mkx)/cp)/exn0(:mkx)
      thvl0(:mkx)  = (1._r8 + zvir*qt0(:mkx))*thl0(:mkx)

      
      

      ssthl0       = slope(mkx,thl0,p0) 
      ssqt0        = slope(mkx,qt0 ,p0)
      ssu0         = slope(mkx,u0  ,p0)
      ssv0         = slope(mkx,v0  ,p0)
      do m = 1, ncnst
         sstr0(:mkx,m) = slope(mkx,tr0(:mkx,m),p0)
      enddo     
 
      
      

      do k = 1, mkx

         thl0bot = thl0(k) + ssthl0(k)*(ps0(k-1) - p0(k))
         qt0bot  = qt0(k)  + ssqt0(k) *(ps0(k-1) - p0(k))
         call conden(ps0(k-1),thl0bot,qt0bot,thj,qvj,qlj,qij,qse,id_check,qsat)
         if( id_check .eq. 1 ) then
             exit_conden(i) = 1._r8
             id_exit = .true.
             go to 333
         end if
         thv0bot(k)  = thj*(1._r8 + zvir*qvj - qlj - qij)
         thvl0bot(k) = thl0bot*(1._r8 + zvir*qt0bot)
          
         thl0top = thl0(k) + ssthl0(k)*(ps0(k) - p0(k))
         qt0top  =  qt0(k) + ssqt0(k) *(ps0(k) - p0(k))
         call conden(ps0(k),thl0top,qt0top,thj,qvj,qlj,qij,qse,id_check,qsat)
         if( id_check .eq. 1 ) then
             exit_conden(i) = 1._r8
             id_exit = .true.
             go to 333
         end if 
         thv0top(k)  = thj*(1._r8 + zvir*qvj - qlj - qij)
         thvl0top(k) = thl0top*(1._r8 + zvir*qt0top)

      end do

      
      
      
      

      qv0_o(:mkx)          = qv0(:mkx)
      ql0_o(:mkx)          = ql0(:mkx)
      qi0_o(:mkx)          = qi0(:mkx)
      t0_o(:mkx)           = t0(:mkx)
      s0_o(:mkx)           = s0(:mkx)
      u0_o(:mkx)           = u0(:mkx)
      v0_o(:mkx)           = v0(:mkx)
      qt0_o(:mkx)          = qt0(:mkx)
      thl0_o(:mkx)         = thl0(:mkx)
      thvl0_o(:mkx)        = thvl0(:mkx)
      ssthl0_o(:mkx)       = ssthl0(:mkx)
      ssqt0_o(:mkx)        = ssqt0(:mkx)
      thv0bot_o(:mkx)      = thv0bot(:mkx)
      thv0top_o(:mkx)      = thv0top(:mkx)
      thvl0bot_o(:mkx)     = thvl0bot(:mkx)
      thvl0top_o(:mkx)     = thvl0top(:mkx)
      ssu0_o(:mkx)         = ssu0(:mkx) 
      ssv0_o(:mkx)         = ssv0(:mkx) 
      do m = 1, ncnst
         tr0_o(:mkx,m)     = tr0(:mkx,m)
         sstr0_o(:mkx,m)   = sstr0(:mkx,m)
      enddo 

      
      
      

      umf(0:mkx)          = 0.0_r8
      emf(0:mkx)          = 0.0_r8
      slflx(0:mkx)        = 0.0_r8
      qtflx(0:mkx)        = 0.0_r8
      uflx(0:mkx)         = 0.0_r8
      vflx(0:mkx)         = 0.0_r8
      qvten(:mkx)         = 0.0_r8
      qlten(:mkx)         = 0.0_r8
      qiten(:mkx)         = 0.0_r8
      sten(:mkx)          = 0.0_r8
      uten(:mkx)          = 0.0_r8
      vten(:mkx)          = 0.0_r8
      qrten(:mkx)         = 0.0_r8
      qsten(:mkx)         = 0.0_r8
      dwten(:mkx)         = 0.0_r8
      diten(:mkx)         = 0.0_r8
      precip              = 0.0_r8
      snow                = 0.0_r8
      evapc(:mkx)         = 0.0_r8
      cufrc(:mkx)         = 0.0_r8
      qcu(:mkx)           = 0.0_r8
      qlu(:mkx)           = 0.0_r8
      qiu(:mkx)           = 0.0_r8
      fer(:mkx)           = 0.0_r8
      fdr(:mkx)           = 0.0_r8
      cin                 = 0.0_r8
      cbmf                = 0.0_r8
      qc(:mkx)            = 0.0_r8
      qc_l(:mkx)          = 0.0_r8
      qc_i(:mkx)          = 0.0_r8
      rliq                = 0.0_r8
      cnt                 = real(mkx, r8)
      cnb                 = 0.0_r8
      qtten(:mkx)         = 0.0_r8
      slten(:mkx)         = 0.0_r8   
      ufrc(0:mkx)         = 0.0_r8  

      thlu(0:mkx)         = 0.0_r8
      qtu(0:mkx)          = 0.0_r8
      uu(0:mkx)           = 0.0_r8
      vu(0:mkx)           = 0.0_r8
      wu(0:mkx)           = 0.0_r8
      thvu(0:mkx)         = 0.0_r8
      thlu_emf(0:mkx)     = 0.0_r8
      qtu_emf(0:mkx)      = 0.0_r8
      uu_emf(0:mkx)       = 0.0_r8
      vu_emf(0:mkx)       = 0.0_r8
      
      ufrcinvbase         = 0.0_r8
      ufrclcl             = 0.0_r8
      winvbase            = 0.0_r8
      wlcl                = 0.0_r8
      emfkbup             = 0.0_r8 
      cbmflimit           = 0.0_r8
      excessu_arr(:mkx)   = 0.0_r8
      excess0_arr(:mkx)   = 0.0_r8
      xc_arr(:mkx)        = 0.0_r8
      aquad_arr(:mkx)     = 0.0_r8
      bquad_arr(:mkx)     = 0.0_r8
      cquad_arr(:mkx)     = 0.0_r8
      bogbot_arr(:mkx)    = 0.0_r8
      bogtop_arr(:mkx)    = 0.0_r8

      uemf(0:mkx)         = 0.0_r8
      comsub(:mkx)        = 0.0_r8
      qlten_sink(:mkx)    = 0.0_r8
      qiten_sink(:mkx)    = 0.0_r8 
      nlten_sink(:mkx)    = 0.0_r8
      niten_sink(:mkx)    = 0.0_r8 

      do m = 1, ncnst
         trflx(0:mkx,m)   = 0.0_r8
         trten(:mkx,m)    = 0.0_r8
         tru(0:mkx,m)     = 0.0_r8
         tru_emf(0:mkx,m) = 0.0_r8
      enddo

    
    
    

    
    
    
    
    
    

    do iter = 1, iter_cin

       
       
       
       
       
       
       

       tscaleh = cush                        
       cush    = -1._r8

       
       
       
       
       
       
       
       
       
       
       
       
       
       
       
       
       
       
       
       
       
       do k = mkx - 1, 1, -1 
          if( (pblh + 5._r8 - zs0(k))*(pblh + 5._r8 - zs0(k+1)) .lt. 0._r8 ) then
               kinv = k + 1 
               go to 15
          endif 
       end do
       kinv = 1
15     continue    

       if( kinv .le. 1 ) then          
           exit_kinv1(i) = 1._r8
           id_exit = .true.
           go to 333
       endif
       

       
       
       
       
       
       
       
       
       
       
       
       
       
       dpsum    = 0._r8
       tkeavg   = 0._r8
       thvlmin  = 1000._r8
       do k = 0, kinv - 1   
          if( k .eq. 0 ) then
              dpi = ps0(0) - p0(1)
          elseif( k .eq. (kinv-1) ) then 
              dpi = p0(kinv-1) - ps0(kinv-1)
          else
              dpi = p0(k) - p0(k+1)
          endif 
          dpsum  = dpsum  + dpi  
          tkeavg = tkeavg + dpi*tke(k) 
          if( k .ne. 0 ) thvlmin = min(thvlmin,min(thvl0bot(k),thvl0top(k)))
       end do
       tkeavg  = tkeavg/dpsum

       
       
       
       
       
       
       

       qtsrc   = qt0(1)                     
       thvlsrc = thvlmin 
       thlsrc  = thvlsrc / ( 1._r8 + zvir * qtsrc )  
       usrc    = u0(kinv-1) + ssu0(kinv-1) * ( ps0(kinv-1) - p0(kinv-1) )             
       vsrc    = v0(kinv-1) + ssv0(kinv-1) * ( ps0(kinv-1) - p0(kinv-1) )             
       do m = 1, ncnst
          trsrc(m) = tr0(1,m)
       enddo 

       
       
       
       
       
       
       
       
       
       
       
       
       
       

       plcl = qsinvert(qtsrc,thlsrc,ps0(0),qsat)
       do k = 0, mkx
          if( ps0(k) .lt. plcl ) then
              klcl = k
              go to 25
          endif           
       end do
       klcl = mkx
25     continue
       klcl = max(1,klcl)
     
       if( plcl .lt. 30000._r8 ) then               
     
           exit_klclmkx(i) = 1._r8
           id_exit = .true.
           go to 333
       endif

       
       
       
       
       
       
       

       thl0lcl = thl0(klcl) + ssthl0(klcl) * ( plcl - p0(klcl) )
       qt0lcl  = qt0(klcl)  + ssqt0(klcl)  * ( plcl - p0(klcl) )
       call conden(plcl,thl0lcl,qt0lcl,thj,qvj,qlj,qij,qse,id_check,qsat)
       if( id_check .eq. 1 ) then
           exit_conden(i) = 1._r8
           id_exit = .true.
           go to 333
       end if
       thv0lcl = thj * ( 1._r8 + zvir * qvj - qlj - qij )

       
       
       
       
       
       
       
       
       
       
       
       
       
       
       
       
       
       
       
       
       
       

        cin    = 0._r8
        cinlcl = 0._r8
        plfc   = 0._r8
        klfc   = mkx

        
        
        

        if( klcl .ge. kinv ) then

            do k = kinv, mkx - 1
               if( k .lt. klcl ) then
                   thvubot = thvlsrc
                   thvutop = thvlsrc  
                   cin     = cin + single_cin(ps0(k-1),thv0bot(k),ps0(k),thv0top(k),thvubot,thvutop)
               elseif( k .eq. klcl ) then
                   
                   thvubot = thvlsrc
                   thvutop = thvlsrc
                   cin     = cin + single_cin(ps0(k-1),thv0bot(k),plcl,thv0lcl,thvubot,thvutop)
                   if( cin .lt. 0._r8 ) limit_cinlcl(i) = 1._r8
                   cinlcl  = max(cin,0._r8)
                   cin     = cinlcl
                   
                   thvubot = thvlsrc
                   call conden(ps0(k),thlsrc,qtsrc,thj,qvj,qlj,qij,qse,id_check,qsat)
                   if( id_check .eq. 1 ) then
                       exit_conden(i) = 1._r8
                       id_exit = .true.
                       go to 333
                   end if
                   thvutop = thj * ( 1._r8 + zvir*qvj - qlj - qij )
                   call getbuoy(plcl,thv0lcl,ps0(k),thv0top(k),thvubot,thvutop,plfc,cin)
                   if( plfc .gt. 0._r8 ) then 
                       klfc = k 
                       go to 35
                   end if
               else
                   thvubot = thvutop
                   call conden(ps0(k),thlsrc,qtsrc,thj,qvj,qlj,qij,qse,id_check,qsat)
                   if( id_check .eq. 1 ) then
                       exit_conden(i) = 1._r8
                       id_exit = .true.
                       go to 333
                   end if
                   thvutop = thj * ( 1._r8 + zvir*qvj - qlj - qij )
                   call getbuoy(ps0(k-1),thv0bot(k),ps0(k),thv0top(k),thvubot,thvutop,plfc,cin)
                   if( plfc .gt. 0._r8 ) then 
                       klfc = k
                       go to 35
                   end if 
               endif
            end do        

       
       
       

       else
          cinlcl = 0._r8 
          do k = kinv, mkx - 1
             call conden(ps0(k-1),thlsrc,qtsrc,thj,qvj,qlj,qij,qse,id_check,qsat)
             if( id_check .eq. 1 ) then
                 exit_conden(i) = 1._r8
                 id_exit = .true.
                 go to 333
             end if
             thvubot = thj * ( 1._r8 + zvir*qvj - qlj - qij )
             call conden(ps0(k),thlsrc,qtsrc,thj,qvj,qlj,qij,qse,id_check,qsat)
             if( id_check .eq. 1 ) then
                 exit_conden(i) = 1._r8
                 id_exit = .true.
                 go to 333
             end if
             thvutop = thj * ( 1._r8 + zvir*qvj - qlj - qij )
             call getbuoy(ps0(k-1),thv0bot(k),ps0(k),thv0top(k),thvubot,thvutop,plfc,cin)
             if( plfc .gt. 0._r8 ) then 
                 klfc = k
                 go to 35
             end if 
          end do
       endif  

 35    continue
       if( cin .lt. 0._r8 ) limit_cin(i) = 1._r8
       cin = max(0._r8,cin)
       if( klfc .ge. mkx ) then
           klfc = mkx
         
           exit_klfcmkx(i) = 1._r8
           id_exit = .true.
           go to 333
       endif

       
       
       
       
       

       if( iter .eq. 1 ) then 
           cin_i       = cin
           cinlcl_i    = cinlcl
           ke          = rbuoy / ( rkfre * tkeavg + epsvarw ) 
           kinv_o      = kinv     
           klcl_o      = klcl     
           klfc_o      = klfc    
           plcl_o      = plcl    
           plfc_o      = plfc     
           tkeavg_o    = tkeavg   
           thvlmin_o   = thvlmin
           qtsrc_o     = qtsrc    
           thvlsrc_o   = thvlsrc  
           thlsrc_o    = thlsrc
           usrc_o      = usrc     
           vsrc_o      = vsrc     
           thv0lcl_o   = thv0lcl  
           do m = 1, ncnst
              trsrc_o(m) = trsrc(m)
           enddo
       endif   

     
     
     
     
     
     
     
     
     

       
       
       
       
       
       
       
       
       
       

       if( iter .ne. 1 ) then

           cin_f = cin
           cinlcl_f = cinlcl
           if( use_CINcin ) then
               del_CIN = cin_f - cin_i
           else
               del_CIN = cinlcl_f - cinlcl_i
           endif

           if( del_CIN .gt. 0._r8 ) then

               
               
               
               
               
               
               
               
               
               
         
               alpha = compute_alpha( del_CIN, ke ) 
               cin   = cin_i + alpha * del_CIN
               if( use_CINcin ) then
                   cinlcl = cinlcl_i                 
               else
                   cinlcl = cinlcl_i + alpha * del_cinlcl   
               endif

               
               
               
               

               kinv      = kinv_o     
               klcl      = klcl_o     
               klfc      = klfc_o    
               plcl      = plcl_o    
               plfc      = plfc_o     
               tkeavg    = tkeavg_o   
               thvlmin   = thvlmin_o
               qtsrc     = qtsrc_o    
               thvlsrc   = thvlsrc_o  
               thlsrc    = thlsrc_o
               usrc      = usrc_o     
               vsrc      = vsrc_o     
               thv0lcl   = thv0lcl_o  
               do m = 1, ncnst
                  trsrc(m) = trsrc_o(m)
               enddo

               qv0(:mkx)            = qv0_o(:mkx)
               ql0(:mkx)            = ql0_o(:mkx)
               qi0(:mkx)            = qi0_o(:mkx)
               t0(:mkx)             = t0_o(:mkx)
               s0(:mkx)             = s0_o(:mkx)
               u0(:mkx)             = u0_o(:mkx)
               v0(:mkx)             = v0_o(:mkx)
               qt0(:mkx)            = qt0_o(:mkx)
               thl0(:mkx)           = thl0_o(:mkx)
               thvl0(:mkx)          = thvl0_o(:mkx)
               ssthl0(:mkx)         = ssthl0_o(:mkx)
               ssqt0(:mkx)          = ssqt0_o(:mkx)
               thv0bot(:mkx)        = thv0bot_o(:mkx)
               thv0top(:mkx)        = thv0top_o(:mkx)
               thvl0bot(:mkx)       = thvl0bot_o(:mkx)
               thvl0top(:mkx)       = thvl0top_o(:mkx)
               ssu0(:mkx)           = ssu0_o(:mkx) 
               ssv0(:mkx)           = ssv0_o(:mkx) 
               do m = 1, ncnst
                  tr0(:mkx,m)   = tr0_o(:mkx,m)
                  sstr0(:mkx,m) = sstr0_o(:mkx,m)
               enddo

               
               
               
               

               umf(0:mkx)          = 0.0_r8
               emf(0:mkx)          = 0.0_r8
               slflx(0:mkx)        = 0.0_r8
               qtflx(0:mkx)        = 0.0_r8
               uflx(0:mkx)         = 0.0_r8
               vflx(0:mkx)         = 0.0_r8
               qvten(:mkx)         = 0.0_r8
               qlten(:mkx)         = 0.0_r8
               qiten(:mkx)         = 0.0_r8
               sten(:mkx)          = 0.0_r8
               uten(:mkx)          = 0.0_r8
               vten(:mkx)          = 0.0_r8
               qrten(:mkx)         = 0.0_r8
               qsten(:mkx)         = 0.0_r8
               dwten(:mkx)         = 0.0_r8
               diten(:mkx)         = 0.0_r8
               precip              = 0.0_r8
               snow                = 0.0_r8
               evapc(:mkx)         = 0.0_r8
               cufrc(:mkx)         = 0.0_r8
               qcu(:mkx)           = 0.0_r8
               qlu(:mkx)           = 0.0_r8
               qiu(:mkx)           = 0.0_r8
               fer(:mkx)           = 0.0_r8
               fdr(:mkx)           = 0.0_r8
               qc(:mkx)            = 0.0_r8
               qc_l(:mkx)          = 0.0_r8
               qc_i(:mkx)          = 0.0_r8
               rliq                = 0.0_r8
               cbmf                = 0.0_r8
               cnt                 = real(mkx, r8)
               cnb                 = 0.0_r8
               qtten(:mkx)         = 0.0_r8
               slten(:mkx)         = 0.0_r8
               ufrc(0:mkx)         = 0.0_r8

               thlu(0:mkx)         = 0.0_r8
               qtu(0:mkx)          = 0.0_r8
               uu(0:mkx)           = 0.0_r8
               vu(0:mkx)           = 0.0_r8
               wu(0:mkx)           = 0.0_r8
               thvu(0:mkx)         = 0.0_r8
               thlu_emf(0:mkx)     = 0.0_r8
               qtu_emf(0:mkx)      = 0.0_r8
               uu_emf(0:mkx)       = 0.0_r8
               vu_emf(0:mkx)       = 0.0_r8
             
               do m = 1, ncnst
                  trflx(0:mkx,m)   = 0.0_r8
                  trten(:mkx,m)    = 0.0_r8
                  tru(0:mkx,m)     = 0.0_r8
                  tru_emf(0:mkx,m) = 0.0_r8
               enddo

               
               
               
               

               ufrcinvbase         = 0.0_r8
               ufrclcl             = 0.0_r8
               winvbase            = 0.0_r8
               wlcl                = 0.0_r8
               emfkbup             = 0.0_r8 
               cbmflimit           = 0.0_r8
               excessu_arr(:mkx)   = 0.0_r8
               excess0_arr(:mkx)   = 0.0_r8
               xc_arr(:mkx)        = 0.0_r8
               aquad_arr(:mkx)     = 0.0_r8
               bquad_arr(:mkx)     = 0.0_r8
               cquad_arr(:mkx)     = 0.0_r8
               bogbot_arr(:mkx)    = 0.0_r8
               bogtop_arr(:mkx)    = 0.0_r8

          else 
           
               
               
               

               ind_delcin(i) = 1._r8             
   
               
               
               

               umf_out(i,0:mkx)         = umf_s(0:mkx)
               qvten_out(i,:mkx)        = qvten_s(:mkx)
               qlten_out(i,:mkx)        = qlten_s(:mkx)  
               qiten_out(i,:mkx)        = qiten_s(:mkx)
               sten_out(i,:mkx)         = sten_s(:mkx)
               uten_out(i,:mkx)         = uten_s(:mkx)  
               vten_out(i,:mkx)         = vten_s(:mkx)
               qrten_out(i,:mkx)        = qrten_s(:mkx)
               qsten_out(i,:mkx)        = qsten_s(:mkx)  
               precip_out(i)            = precip_s
               snow_out(i)              = snow_s
               evapc_out(i,:mkx)        = evapc_s(:mkx)
               cush_inout(i)            = cush_s
               cufrc_out(i,:mkx)        = cufrc_s(:mkx)  
               slflx_out(i,0:mkx)       = slflx_s(0:mkx)  
               qtflx_out(i,0:mkx)       = qtflx_s(0:mkx)
               qcu_out(i,:mkx)          = qcu_s(:mkx)    
               qlu_out(i,:mkx)          = qlu_s(:mkx)  
               qiu_out(i,:mkx)          = qiu_s(:mkx)  
               cbmf_out(i)              = cbmf_s
               qc_out(i,:mkx)           = qc_s(:mkx)  
               rliq_out(i)              = rliq_s
               cnt_out(i)               = cnt_s
               cnb_out(i)               = cnb_s
               do m = 1, ncnst
                  trten_out(i,:mkx,m)   = trten_s(:mkx,m)
               enddo  
             
               
               
               
               

               fer_out(i,mkx:1:-1)      = fer_s(:mkx)  
               fdr_out(i,mkx:1:-1)      = fdr_s(:mkx)  
               cinh_out(i)              = cin_s
               cinlclh_out(i)           = cinlcl_s
               qtten_out(i,mkx:1:-1)    = qtten_s(:mkx)
               slten_out(i,mkx:1:-1)    = slten_s(:mkx)
               ufrc_out(i,mkx:0:-1)     = ufrc_s(0:mkx)
               uflx_out(i,mkx:0:-1)     = uflx_s(0:mkx)  
               vflx_out(i,mkx:0:-1)     = vflx_s(0:mkx)  

               ufrcinvbase_out(i)       = ufrcinvbase_s
               ufrclcl_out(i)           = ufrclcl_s 
               winvbase_out(i)          = winvbase_s
               wlcl_out(i)              = wlcl_s
               plcl_out(i)              = plcl_s
               pinv_out(i)              = pinv_s    
               plfc_out(i)              = plfc_s    
               pbup_out(i)              = pbup_s
               ppen_out(i)              = ppen_s    
               qtsrc_out(i)             = qtsrc_s
               thlsrc_out(i)            = thlsrc_s
               thvlsrc_out(i)           = thvlsrc_s
               emfkbup_out(i)           = emfkbup_s
               cbmflimit_out(i)         = cbmflimit_s
               tkeavg_out(i)            = tkeavg_s
               zinv_out(i)              = zinv_s
               rcwp_out(i)              = rcwp_s
               rlwp_out(i)              = rlwp_s
               riwp_out(i)              = riwp_s

               wu_out(i,mkx:0:-1)       = wu_s(0:mkx)
               qtu_out(i,mkx:0:-1)      = qtu_s(0:mkx)
               thlu_out(i,mkx:0:-1)     = thlu_s(0:mkx)
               thvu_out(i,mkx:0:-1)     = thvu_s(0:mkx)
               uu_out(i,mkx:0:-1)       = uu_s(0:mkx)
               vu_out(i,mkx:0:-1)       = vu_s(0:mkx)
               qtu_emf_out(i,mkx:0:-1)  = qtu_emf_s(0:mkx)
               thlu_emf_out(i,mkx:0:-1) = thlu_emf_s(0:mkx)
               uu_emf_out(i,mkx:0:-1)   = uu_emf_s(0:mkx)
               vu_emf_out(i,mkx:0:-1)   = vu_emf_s(0:mkx)
               uemf_out(i,mkx:0:-1)     = uemf_s(0:mkx)

               dwten_out(i,mkx:1:-1)    = dwten_s(:mkx)
               diten_out(i,mkx:1:-1)    = diten_s(:mkx)
               flxrain_out(i,mkx:0:-1)  = flxrain_s(0:mkx)
               flxsnow_out(i,mkx:0:-1)  = flxsnow_s(0:mkx)
               ntraprd_out(i,mkx:1:-1)  = ntraprd_s(:mkx)
               ntsnprd_out(i,mkx:1:-1)  = ntsnprd_s(:mkx)

               excessu_arr_out(i,mkx:1:-1)  = excessu_arr_s(:mkx)
               excess0_arr_out(i,mkx:1:-1)  = excess0_arr_s(:mkx)
               xc_arr_out(i,mkx:1:-1)       = xc_arr_s(:mkx)
               aquad_arr_out(i,mkx:1:-1)    = aquad_arr_s(:mkx)
               bquad_arr_out(i,mkx:1:-1)    = bquad_arr_s(:mkx)
               cquad_arr_out(i,mkx:1:-1)    = cquad_arr_s(:mkx)
               bogbot_arr_out(i,mkx:1:-1)   = bogbot_arr_s(:mkx)
               bogtop_arr_out(i,mkx:1:-1)   = bogtop_arr_s(:mkx)

               do m = 1, ncnst
                  trflx_out(i,mkx:0:-1,m)   = trflx_s(0:mkx,m)  
                  tru_out(i,mkx:0:-1,m)     = tru_s(0:mkx,m)
                  tru_emf_out(i,mkx:0:-1,m) = tru_emf_s(0:mkx,m)
               enddo

               id_exit = .false.
               go to 333

          endif

       endif    

       
       
       
       
       
       
       
       
       
       
       
       
       
       
       
       
       
       

       if( klcl .lt. kinv ) then
           krel    = kinv
           prel    = ps0(krel-1)
           thv0rel = thv0bot(krel) 
       else
           krel    = klcl
           prel    = plcl 
           thv0rel = thv0lcl
       endif  

       
       
       
       
       
       
       
       
       
       
       
       
       
       
       
       
       
       
       
       
       
       
       
       
       
       
       
       
       
       
       
       
       
       
       
       
       
       
       
       
       
       
       
       
       
       
   
       if( use_CINcin ) then       
           wcrit = sqrt( 2._r8 * cin * rbuoy )      
       else
           wcrit = sqrt( 2._r8 * cinlcl * rbuoy )   
       endif
       sigmaw = sqrt( rkfre * tkeavg + epsvarw )
       mu = wcrit/sigmaw/1.4142_r8                  
       if( mu .ge. 3._r8 ) then
         
           id_exit = .true.
           go to 333
       endif
       rho0inv = ps0(kinv-1)/(r*thv0top(kinv-1)*exns0(kinv-1))
       cbmf = (rho0inv*sigmaw/2.5066_r8)*exp(-mu**2)
       
       cbmflimit = 0.9_r8*dp0(kinv-1)/g/dt
       mumin0 = 0._r8
       if( cbmf .gt. cbmflimit ) mumin0 = sqrt(-log(2.5066_r8*cbmflimit/rho0inv/sigmaw))
       
       mu = max(max(mu,mumin0),mumin1)
       
       mulcl = sqrt(2._r8*cinlcl*rbuoy)/1.4142_r8/sigmaw
       mulclstar = sqrt(max(0._r8,2._r8*(exp(-mu**2)/2.5066_r8)**2*(1._r8/erfc(mu)**2-0.25_r8/rmaxfrac**2)))
       if( mulcl .gt. 1.e-8_r8 .and. mulcl .gt. mulclstar ) then
           mumin2 = compute_mumin2(mulcl,rmaxfrac,mu)
           if( mu .gt. mumin2 ) then
               write(iulog,*) 'Critical error in mu calculation in UW_ShCu'
               call wrf_message(iulog)
               call endrun
           endif
           mu = max(mu,mumin2)
           if( mu .eq. mumin2 ) limit_ufrc(i) = 1._r8
       endif
       if( mu .eq. mumin0 ) limit_cbmf(i) = 1._r8
       if( mu .eq. mumin1 ) limit_ufrc(i) = 1._r8

       
       
       
       
       

       cbmf = (rho0inv*sigmaw/2.5066_r8)*exp(-mu**2)                       
       winv = sigmaw*(2._r8/2.5066_r8)*exp(-mu**2)/erfc(mu)
       ufrcinv = cbmf/winv/rho0inv

       
       
       
       
       
       
       
       
       
       

       wtw = winv * winv - 2._r8 * cinlcl * rbuoy
       if( wtw .le. 0._r8 ) then
         
           exit_wtw(i) = 1._r8
           id_exit = .true.
           go to 333
       endif
       wlcl = sqrt(wtw)
       ufrclcl = cbmf/wlcl/rho0inv
       wrel = wlcl
       if( ufrclcl .le. 0.0001_r8 ) then
         
           exit_ufrc(i) = 1._r8
           id_exit = .true.
           go to 333
       endif
       ufrc(krel-1) = ufrclcl

       
       
       

       ufrcinvbase        = ufrcinv
       winvbase           = winv
       umf(kinv-1:krel-1) = cbmf   
       wu(kinv-1:krel-1)  = winv   

       
       
       
       
       
       
       
       
       
       
       
       
       
       
       
       
       
       

       emf(krel-1)  = 0._r8
       umf(krel-1)  = cbmf
       wu(krel-1)   = wrel
       thlu(krel-1) = thlsrc
       qtu(krel-1)  = qtsrc
       call conden(prel,thlsrc,qtsrc,thj,qvj,qlj,qij,qse,id_check,qsat)
       if( id_check .eq. 1 ) then
           exit_conden(i) = 1._r8
           id_exit = .true.
           go to 333
       endif
       thvu(krel-1) = thj * ( 1._r8 + zvir*qvj - qlj - qij )       

       uplus = 0._r8
       vplus = 0._r8
       if( krel .eq. kinv ) then
           uplus = PGFc * ssu0(kinv) * ( prel - ps0(kinv-1) )
           vplus = PGFc * ssv0(kinv) * ( prel - ps0(kinv-1) )
       else
           do k = kinv, max(krel-1,kinv)
              uplus = uplus + PGFc * ssu0(k) * ( ps0(k) - ps0(k-1) )
              vplus = vplus + PGFc * ssv0(k) * ( ps0(k) - ps0(k-1) )
           end do
           uplus = uplus + PGFc * ssu0(krel) * ( prel - ps0(krel-1) )
           vplus = vplus + PGFc * ssv0(krel) * ( prel - ps0(krel-1) )
       end if
       uu(krel-1) = usrc + uplus
       vu(krel-1) = vsrc + vplus      

       do m = 1, ncnst
          tru(krel-1,m)  = trsrc(m)
       enddo

       
       
       
       
       
       

       pe      = 0.5_r8 * ( prel + ps0(krel) )
       dpe     = prel - ps0(krel)
       exne    = exnf(pe)
       thvebot = thv0rel
       thle    = thl0(krel) + ssthl0(krel) * ( pe - p0(krel) )
       qte     = qt0(krel)  + ssqt0(krel)  * ( pe - p0(krel) )
       ue      = u0(krel)   + ssu0(krel)   * ( pe - p0(krel) )
       ve      = v0(krel)   + ssv0(krel)   * ( pe - p0(krel) )
       do m = 1, ncnst
          tre(m) = tr0(krel,m)  + sstr0(krel,m) * ( pe - p0(krel) )
       enddo

       
       
       
       
       
       
       
       
       
       
       
       
       
       
       
       
       
       
       
       
       
       
       
       
       
       
       
       
       
       
       
       
       
       
       
       
       
       

       scaleh = tscaleh
       if( tscaleh .lt. 0.0_r8 ) scaleh = 1000._r8 

     
     
     
     
     
     

       do iter_scaleh = 1, 3

       
       
       
       
       
       
       
       
       
       
       
       
       
       
       
       
       
       
       
       
       
       
       
       
       
       
       
       
       
       
       
       
       
       
       
       
       
       
       
       
       
       
       
       
       
       
       
       

       kbup    = krel
       kpen    = krel
       
       
       
       
       
       
       

       wtw     = wlcl * wlcl
       pe      = 0.5_r8 * ( prel + ps0(krel) )
       dpe     = prel - ps0(krel)
       exne    = exnf(pe)
       thvebot = thv0rel
       thle    = thl0(krel) + ssthl0(krel) * ( pe - p0(krel) )
       qte     = qt0(krel)  + ssqt0(krel)  * ( pe - p0(krel) )
       ue      = u0(krel)   + ssu0(krel)   * ( pe - p0(krel) )
       ve      = v0(krel)   + ssv0(krel)   * ( pe - p0(krel) )
       do m = 1, ncnst
          tre(m) = tr0(krel,m)  + sstr0(krel,m)  * ( pe - p0(krel) )
       enddo

       
       
       
       
       
       
       
       
       
       
       
       
       
       
       
       
       
       
       
       do k = krel, mkx - 1 

          km1 = k - 1

          thlue = thlu(km1)
          qtue  = qtu(km1)    
          wue   = wu(km1)
          wtwb  = wtw  

       do iter_xc = 1, niter_xc
          
          wtw = wu(km1) * wu(km1)

          
          
          
          
          
          

          call conden(pe,thle,qte,thj,qvj,qlj,qij,qse,id_check,qsat)
          if( id_check .eq. 1 ) then
              exit_conden(i) = 1._r8
              id_exit = .true.
              go to 333
          end if
          thv0j    = thj * ( 1._r8 + zvir*qvj - qlj - qij )
          rho0j    = pe / ( r * thv0j * exne )
          qsat_arg = thle*exne     
          status   = qsat(qsat_arg,pe,es(1),qs(1),gam(1),1)
          excess0  = qte - qs(1)

          call conden(pe,thlue,qtue,thj,qvj,qlj,qij,qse,id_check,qsat)
          if( id_check .eq. 1 ) then
              exit_conden(i) = 1._r8
              id_exit = .true.
              go to 333
          end if
          
          
          
          
          
          
          
          
          
          if( (qlj + qij) .gt. criqc ) then
               exql  = ( ( qlj + qij ) - criqc ) * qlj / ( qlj + qij )
               exqi  = ( ( qlj + qij ) - criqc ) * qij / ( qlj + qij )
               qtue  = qtue - exql - exqi
               thlue = thlue + (xlv/cp/exne)*exql + (xls/cp/exne)*exqi 
          endif
          call conden(pe,thlue,qtue,thj,qvj,qlj,qij,qse,id_check,qsat)
          if( id_check .eq. 1 ) then
              exit_conden(i) = 1._r8
              id_exit = .true.
              go to 333
          end if
          thvj     = thj * ( 1._r8 + zvir * qvj - qlj - qij )
          tj       = thj * exne 
          qsat_arg = thlue*exne
          status   = qsat(qsat_arg,pe,es(1),qs(1),gam(1),1)
          excessu  = qtue - qs(1)

          
          
          
          
          
          
          
          
          
          
          

          
          
          
          

            cridis = rle*scaleh                 
          
 
          
          
          

          
          
          

          if( ( excessu .le. 0._r8 .and. excess0 .le. 0._r8 ) .or. ( excessu .ge. 0._r8 .and. excess0 .ge. 0._r8 ) ) then
                xc = min(1._r8,max(0._r8,1._r8-2._r8*rbuoy*g*cridis/wue**2._r8*(1._r8-thvj/thv0j)))
              
              
                aquad = 0._r8
                bquad = 0._r8
                cquad = 0._r8
          else
          
          
          
              xsat    = excessu / ( excessu - excess0 );
              thlxsat = thlue + xsat * ( thle - thlue );
              qtxsat  = qtue  + xsat * ( qte - qtue );
              call conden(pe,thlxsat,qtxsat,thj,qvj,qlj,qij,qse,id_check,qsat)
              if( id_check .eq. 1 ) then
                  exit_conden(i) = 1._r8
                  id_exit = .true.
                  go to 333
              end if
              thvxsat = thj * ( 1._r8 + zvir * qvj - qlj - qij )               
              
              
              
              do kk = 1, 2 
                   if( kk .eq. 1 ) then
                       thv_x0 = thvj;
                       thv_x1 = ( 1._r8 - 1._r8/xsat ) * thvj + ( 1._r8/xsat ) * thvxsat;
                   else
                       thv_x1 = thv0j;
                       thv_x0 = ( xsat / ( xsat - 1._r8 ) ) * thv0j + ( 1._r8/( 1._r8 - xsat ) ) * thvxsat;
                   endif
                   aquad =  wue**2;
                   bquad =  2._r8*rbuoy*g*cridis*(thv_x1 - thv_x0)/thv0j - 2._r8*wue**2;
                   cquad =  2._r8*rbuoy*g*cridis*(thv_x0 -  thv0j)/thv0j +       wue**2;
                   if( kk .eq. 1 ) then
                       if( ( bquad**2-4._r8*aquad*cquad ) .ge. 0._r8 ) then
                             call roots(aquad,bquad,cquad,xs1,xs2,status)
                             x_cu = min(1._r8,max(0._r8,min(xsat,min(xs1,xs2))))
                       else
                             x_cu = xsat;
                       endif
                   else 
                       if( ( bquad**2-4._r8*aquad*cquad) .ge. 0._r8 ) then
                             call roots(aquad,bquad,cquad,xs1,xs2,status)
                             x_en = min(1._r8,max(0._r8,max(xsat,min(xs1,xs2))))
                       else
                             x_en = 1._r8;
                       endif
                   endif
              enddo
              if( x_cu .eq. xsat ) then
                  xc = max(x_cu, x_en);
              else
                  xc = x_cu;
              endif
          endif

          
          
          
          
          
          
          
          
          
          
          
          
          
          ee2    = xc**2
          ud2    = 1._r8 - 2._r8*xc + xc**2
        
          rei(k) = ( 0.5_r8 * rkm / z0(k) / g /rho0j ) 
          if( xc .gt. 0.5_r8 ) rei(k) = min(rei(k),0.9_r8*log(dp0(k)/g/dt/umf(km1) + 1._r8)/dpe/(2._r8*xc-1._r8))
          fer(k) = rei(k) * ee2
          fdr(k) = rei(k) * ud2

          
          
          

          
          
          
          
          
          

          umf(k) = umf(km1) * exp( dpe * ( fer(k) - fdr(k) ) )
          emf(k) = 0._r8    

          
          
          
          

          if( fer(k)*dpe .lt. 1.e-4_r8 ) then
              thlu(k) = thlu(km1) + ( thle + ssthl0(k) * dpe / 2._r8 - thlu(km1) ) * fer(k) * dpe
              qtu(k)  =  qtu(km1) + ( qte  +  ssqt0(k) * dpe / 2._r8 -  qtu(km1) ) * fer(k) * dpe
              uu(k)   =   uu(km1) + ( ue   +   ssu0(k) * dpe / 2._r8 -   uu(km1) ) * fer(k) * dpe - PGFc * ssu0(k) * dpe
              vu(k)   =   vu(km1) + ( ve   +   ssv0(k) * dpe / 2._r8 -   vu(km1) ) * fer(k) * dpe - PGFc * ssv0(k) * dpe
              do m = 1, ncnst
                 tru(k,m)  =  tru(km1,m) + ( tre(m)  + sstr0(k,m) * dpe / 2._r8  -  tru(km1,m) ) * fer(k) * dpe
              enddo
          else
              thlu(k) = ( thle + ssthl0(k) / fer(k) - ssthl0(k) * dpe / 2._r8 ) -          &
                        ( thle + ssthl0(k) * dpe / 2._r8 - thlu(km1) + ssthl0(k) / fer(k) ) * exp(-fer(k) * dpe)
              qtu(k)  = ( qte  +  ssqt0(k) / fer(k) -  ssqt0(k) * dpe / 2._r8 ) -          &  
                        ( qte  +  ssqt0(k) * dpe / 2._r8 -  qtu(km1) +  ssqt0(k) / fer(k) ) * exp(-fer(k) * dpe)
              uu(k) =   ( ue + ( 1._r8 - PGFc ) * ssu0(k) / fer(k) - ssu0(k) * dpe / 2._r8 ) - &
                        ( ue +     ssu0(k) * dpe / 2._r8 -   uu(km1) + ( 1._r8 - PGFc ) * ssu0(k) / fer(k) ) * exp(-fer(k) * dpe)
              vu(k) =   ( ve + ( 1._r8 - PGFc ) * ssv0(k) / fer(k) - ssv0(k) * dpe / 2._r8 ) - &
                        ( ve +     ssv0(k) * dpe / 2._r8 -   vu(km1) + ( 1._r8 - PGFc ) * ssv0(k) / fer(k) ) * exp(-fer(k) * dpe)
              do m = 1, ncnst
                 tru(k,m)  = ( tre(m)  + sstr0(k,m) / fer(k) - sstr0(k,m) * dpe / 2._r8 ) - &  
                             ( tre(m)  + sstr0(k,m) * dpe / 2._r8 - tru(km1,m) + sstr0(k,m) / fer(k) ) * exp(-fer(k) * dpe)
              enddo
          end if

          
          
          
          
          
          
          
          
          
          
          
          
          
          
          
          
          
          
          
          
          
          
          

          call conden(ps0(k),thlu(k),qtu(k),thj,qvj,qlj,qij,qse,id_check,qsat)
          if( id_check .eq. 1 ) then
              exit_conden(i) = 1._r8
              id_exit = .true.
              go to 333
          end if
          if( (qlj + qij) .gt. criqc ) then
               exql    = ( ( qlj + qij ) - criqc ) * qlj / ( qlj + qij )
               exqi    = ( ( qlj + qij ) - criqc ) * qij / ( qlj + qij )
               
               
               
               
               
               
               
               qtu(k)  = qtu(k) - exql - exqi
               thlu(k) = thlu(k) + (xlv/cp/exns0(k))*exql + (xls/cp/exns0(k))*exqi 
               
               
               
               
               
               
               
               
               
               dwten(k) = exql   
               diten(k) = exqi
          else
               dwten(k) = 0._r8
               diten(k) = 0._r8
          endif
          
          
          
          call conden(ps0(k),thlu(k),qtu(k),thj,qvj,qlj,qij,qse,id_check,qsat)
          if( id_check .eq. 1 ) then
              exit_conden(i) = 1._r8
              id_exit = .true.
              go to 333
          end if  
          thvu(k) = thj * ( 1._r8 + zvir * qvj - qlj - qij )

          
          
          
          
          
          

          bogbot = rbuoy * ( thvu(km1) / thvebot  - 1._r8 ) 
          bogtop = rbuoy * ( thvu(k) / thv0top(k) - 1._r8 ) 

          delbog = bogtop - bogbot
          drage  = fer(k) * ( 1._r8 + rdrag )
          expfac = exp(-2._r8*drage*dpe)

          wtwb = wtw
          if( drage*dpe .gt. 1.e-3_r8 ) then
              wtw = wtw*expfac + (delbog + (1._r8-expfac)*(bogbot + delbog/(-2._r8*drage*dpe)))/(rho0j*drage)
          else
              wtw = wtw + dpe * ( bogbot + bogtop ) / rho0j
          endif

        
        

        
        
        
         
          
          
          
          
          
          if( wtw .gt. 0._r8 ) then   
              thlue = 0.5_r8 * ( thlu(km1) + thlu(k) )
              qtue  = 0.5_r8 * ( qtu(km1)  +  qtu(k) )         
              wue   = 0.5_r8 *   sqrt( max( wtwb + wtw, 0._r8 ) )
          else
              go to 111
          endif 

       enddo 

   111 continue

          
          
          
          
          
          
          
          
          
          
          
          
          
          
          
          
          
     
          if( use_self_detrain ) then
              autodet = min( 0.5_r8*g*(bogbot+bogtop)/(max(wtw,0._r8)+1.e-4_r8), 0._r8 ) 
              umf(k)  = umf(k) * exp( 0.637_r8*(dpe/rho0j/g) * autodet )   
          end if      
          if( umf(k) .eq. 0._r8 ) wtw = -1._r8

          
          
          

          excessu_arr(k) = excessu
          excess0_arr(k) = excess0
          xc_arr(k)      = xc
          aquad_arr(k)   = aquad
          bquad_arr(k)   = bquad
          cquad_arr(K)   = cquad
          bogbot_arr(k)  = bogbot
          bogtop_arr(k)  = bogtop

          
          
          
          
          
          
          
          
          
          
          
          
        
        
          if( bogtop .gt. 0._r8 .and. wtw .gt. 0._r8 ) then 
              kbup = k
          end if

          if( wtw .le. 0._r8 ) then
              kpen = k
              go to 45
          end if

          wu(k) = sqrt(wtw)
          if( wu(k) .gt. 100._r8 ) then
              exit_wu(i) = 1._r8
              id_exit = .true.
              go to 333
          endif

          
          
          

          
          
          
          
          
          
          
          
          
          
          
          
          
          
          
            
          rhos0j  = ps0(k) / ( r * 0.5_r8 * ( thv0bot(k+1) + thv0top(k) ) * exns0(k) )
          ufrc(k) = umf(k) / ( rhos0j * wu(k) )
          if( ufrc(k) .gt. rmaxfrac ) then
              limit_ufrc(i) = 1._r8 
              ufrc(k) = rmaxfrac
              umf(k)  = rmaxfrac * rhos0j * wu(k)
              fdr(k)  = fer(k) - log( umf(k) / umf(km1) ) / dpe
          endif

          
          
          
          

          pe      = p0(k+1)
          dpe     = dp0(k+1)
          exne    = exn0(k+1)
          thvebot = thv0bot(k+1)
          thle    = thl0(k+1)
          qte     = qt0(k+1)
          ue      = u0(k+1)
          ve      = v0(k+1) 
          do m = 1, ncnst
             tre(m)  = tr0(k+1,m)
          enddo

       end do   
       
       
       
       
       
       
       
       
       
       
       
       
       
       
       
       
       
       
       
       
       
       
       
       
       
       
       
       
       
       
       
       
 45    continue

       
       
       
       
       
       
       
            
       if( drage .eq. 0._r8 ) then
           aquad =  ( bogtop - bogbot ) / ( ps0(kpen) - ps0(kpen-1) )
           bquad =  2._r8 * bogbot
           cquad = -wu(kpen-1)**2 * rho0j
           call roots(aquad,bquad,cquad,xc1,xc2,status)
           if( status .eq. 0 ) then
               if( xc1 .le. 0._r8 .and. xc2 .le. 0._r8 ) then
                   ppen = max( xc1, xc2 )
                   ppen = min( 0._r8,max( -dp0(kpen), ppen ) )  
               elseif( xc1 .gt. 0._r8 .and. xc2 .gt. 0._r8 ) then
                   ppen = -dp0(kpen)
                   write(iulog,*) 'Warning : UW-Cumulus penetrates upto kpen interface'
                   call wrf_message(iulog)
               else
                   ppen = min( xc1, xc2 )
                   ppen = min( 0._r8,max( -dp0(kpen), ppen ) )  
               endif
           else
               ppen = -dp0(kpen)
               write(iulog,*) 'Warning : UW-Cumulus penetrates upto kpen interface'
               call wrf_message(iulog)
           endif       
       else 
           ppen = compute_ppen(wtwb,drage,bogbot,bogtop,rho0j,dp0(kpen))
       endif
       if( ppen .eq. -dp0(kpen) .or. ppen .eq. 0._r8 ) limit_ppen(i) = 1._r8

       
       
       
       
       
       
       

       if( fer(kpen)*(-ppen) .lt. 1.e-4_r8 ) then
           thlu_top = thlu(kpen-1) + ( thl0(kpen) + ssthl0(kpen) * (-ppen) / 2._r8 - thlu(kpen-1) ) * fer(kpen) * (-ppen)
           qtu_top  =  qtu(kpen-1) + (  qt0(kpen) +  ssqt0(kpen) * (-ppen) / 2._r8  - qtu(kpen-1) ) * fer(kpen) * (-ppen)
       else
           thlu_top = ( thl0(kpen) + ssthl0(kpen) / fer(kpen) - ssthl0(kpen) * (-ppen) / 2._r8 ) - &
                      ( thl0(kpen) + ssthl0(kpen) * (-ppen) / 2._r8 - thlu(kpen-1) + ssthl0(kpen) / fer(kpen) ) * &
                        exp(-fer(kpen) * (-ppen))
           qtu_top  = ( qt0(kpen)  +  ssqt0(kpen) / fer(kpen) -  ssqt0(kpen) * (-ppen) / 2._r8 ) - &  
                      ( qt0(kpen)  +  ssqt0(kpen) * (-ppen) / 2._r8 -  qtu(kpen-1) +  ssqt0(kpen) / fer(kpen) ) * &
                        exp(-fer(kpen) * (-ppen))
       end if

       call conden(ps0(kpen-1)+ppen,thlu_top,qtu_top,thj,qvj,qlj,qij,qse,id_check,qsat)
       if( id_check .eq. 1 ) then
           exit_conden(i) = 1._r8
           id_exit = .true.
           go to 333
       end if
       exntop = ((ps0(kpen-1)+ppen)/p00)**rovcp
       if( (qlj + qij) .gt. criqc ) then
            dwten(kpen) = ( ( qlj + qij ) - criqc ) * qlj / ( qlj + qij )
            diten(kpen) = ( ( qlj + qij ) - criqc ) * qij / ( qlj + qij )
            qtu_top  = qtu_top - dwten(kpen) - diten(kpen)
            thlu_top = thlu_top + (xlv/cp/exntop)*dwten(kpen) + (xls/cp/exntop)*diten(kpen) 
       else
            dwten(kpen) = 0._r8
            diten(kpen) = 0._r8
       endif      
 
       
       
       
       
       rhos0j = ps0(kpen-1)/(r*0.5_r8*(thv0bot(kpen)+thv0top(kpen-1))*exns0(kpen-1))  
       cush   = zs0(kpen-1) - ppen/rhos0j/g
       scaleh = cush 

    end do   

       
       
       
       
       
       
       
       
       
 
       if( kbup .eq. krel ) then 
           forcedCu = .true.
           limit_shcu(i) = 1._r8
       else
           forcedCu = .false.
           limit_shcu(i) = 0._r8
       endif  
       
       
       
       
       
       
       
       
       
       
       
       
       
       

       cldhgt = ps0(kpen-1) + ppen
       if( forcedCu ) then
           
           exit_cufilter(i) = 1._r8
           id_exit = .true.
           go to 333
       end if
       
       
       
       
       
       
       
       
       
       
       
       
       
       
       
       

       umf(kpen:mkx)     = 0._r8
       emf(kpen:mkx)     = 0._r8
       ufrc(kpen:mkx)    = 0._r8
       dwten(kpen+1:mkx) = 0._r8
       diten(kpen+1:mkx) = 0._r8
       fer(kpen+1:mkx)   = 0._r8
       fdr(kpen+1:mkx)   = 0._r8
       
       
       
       
       
       
       
       
       
       
       
       
       
       
       
       
       
       
       
       
       
       
       
       
       
       
       
       
       
       
       
       
       
       
       
       
       
 
       do k = 0, mkx
          thlu_emf(k) = thlu(k)
          qtu_emf(k)  = qtu(k)
          uu_emf(k)   = uu(k)
          vu_emf(k)   = vu(k)
          do m = 1, ncnst
             tru_emf(k,m)  = tru(k,m)
          enddo
       end do

       do k = kpen - 1, kbup, -1  
                                  
                                  
          rhos0j = ps0(k) / ( r * 0.5_r8 * ( thv0bot(k+1) + thv0top(k) ) * exns0(k) )

          if( k .eq. kpen - 1 ) then

             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             if( ( umf(k)*ppen*rei(kpen)*rpen ) .lt. -0.1_r8*rhos0j )         limit_emf(i) = 1._r8
             if( ( umf(k)*ppen*rei(kpen)*rpen ) .lt. -0.9_r8*dp0(kpen)/g/dt ) limit_emf(i) = 1._r8             

             emf(k) = max( max( umf(k)*ppen*rei(kpen)*rpen, -0.1_r8*rhos0j), -0.9_r8*dp0(kpen)/g/dt)
             thlu_emf(k) = thl0(kpen) + ssthl0(kpen) * ( ps0(k) - p0(kpen) )
             qtu_emf(k)  = qt0(kpen)  + ssqt0(kpen)  * ( ps0(k) - p0(kpen) )
             uu_emf(k)   = u0(kpen)   + ssu0(kpen)   * ( ps0(k) - p0(kpen) )     
             vu_emf(k)   = v0(kpen)   + ssv0(kpen)   * ( ps0(k) - p0(kpen) )   
             do m = 1, ncnst
                tru_emf(k,m)  = tr0(kpen,m)  + sstr0(kpen,m)  * ( ps0(k) - p0(kpen) )
             enddo

          else 
              
             
             
             
             
             
             

             if( use_cumpenent ) then  

                 if( ( emf(k+1)-umf(k)*dp0(k+1)*rei(k+1)*rpen ) .lt. -0.1_r8*rhos0j )        limit_emf(i) = 1
                 if( ( emf(k+1)-umf(k)*dp0(k+1)*rei(k+1)*rpen ) .lt. -0.9_r8*dp0(k+1)/g/dt ) limit_emf(i) = 1         
                 emf(k) = max(max(emf(k+1)-umf(k)*dp0(k+1)*rei(k+1)*rpen, -0.1_r8*rhos0j), -0.9_r8*dp0(k+1)/g/dt )    
                 if( abs(emf(k)) .gt. abs(emf(k+1)) ) then
                     thlu_emf(k) = ( thlu_emf(k+1) * emf(k+1) + thl0(k+1) * ( emf(k) - emf(k+1) ) ) / emf(k)
                     qtu_emf(k)  = ( qtu_emf(k+1)  * emf(k+1) + qt0(k+1)  * ( emf(k) - emf(k+1) ) ) / emf(k)
                     uu_emf(k)   = ( uu_emf(k+1)   * emf(k+1) + u0(k+1)   * ( emf(k) - emf(k+1) ) ) / emf(k)
                     vu_emf(k)   = ( vu_emf(k+1)   * emf(k+1) + v0(k+1)   * ( emf(k) - emf(k+1) ) ) / emf(k)
                     do m = 1, ncnst
                        tru_emf(k,m)  = ( tru_emf(k+1,m)  * emf(k+1) + tr0(k+1,m)  * ( emf(k) - emf(k+1) ) ) / emf(k)
                     enddo
                 else   
                     thlu_emf(k) = thl0(k+1)
                     qtu_emf(k)  =  qt0(k+1)
                     uu_emf(k)   =   u0(k+1)
                     vu_emf(k)   =   v0(k+1)
                     do m = 1, ncnst
                        tru_emf(k,m)  =  tr0(k+1,m)
                     enddo
                 endif   
                     
             else 

                 if( ( -umf(k)*dp0(k+1)*rei(k+1)*rpen ) .lt. -0.1_r8*rhos0j )        limit_emf(i) = 1
                 if( ( -umf(k)*dp0(k+1)*rei(k+1)*rpen ) .lt. -0.9_r8*dp0(k+1)/g/dt ) limit_emf(i) = 1         
                 emf(k) = max(max(-umf(k)*dp0(k+1)*rei(k+1)*rpen, -0.1_r8*rhos0j), -0.9_r8*dp0(k+1)/g/dt )    
                 thlu_emf(k) = thl0(k+1)
                 qtu_emf(k)  =  qt0(k+1)
                 uu_emf(k)   =   u0(k+1)
                 vu_emf(k)   =   v0(k+1)
                 do m = 1, ncnst
                    tru_emf(k,m)  =  tr0(k+1,m)
                 enddo

             endif

          endif

          
          
          
          
          
          
          
          
          
          
          
          
          
          
          
          
          
          
          
          
          
          
          
          
          
          
       end do

       
       
       
       
       
       
       
       
       
       
       
       
       
       
       
       
       
       
       
       
       
       
       
       
       
       
       
       
       
       
       
       
       
       
       
       
       
       
       
       
       
       
       
       
       
       
       
       
       
       
       
       
       
       
       
       
       
       
       
       
       
       
       

       
       
       
       
       

       xsrc  = qtsrc
       xmean = qt0(kinv)
       xtop  = qt0(kinv+1) + ssqt0(kinv+1) * ( ps0(kinv)   - p0(kinv+1) )
       xbot  = qt0(kinv-1) + ssqt0(kinv-1) * ( ps0(kinv-1) - p0(kinv-1) )        
       call fluxbelowinv( cbmf, ps0(0:mkx), mkx, kinv, dt, xsrc, xmean, xtop, xbot, xflx )
       qtflx(0:kinv-1) = xflx(0:kinv-1)

       xsrc  = thlsrc
       xmean = thl0(kinv)
       xtop  = thl0(kinv+1) + ssthl0(kinv+1) * ( ps0(kinv)   - p0(kinv+1) )
       xbot  = thl0(kinv-1) + ssthl0(kinv-1) * ( ps0(kinv-1) - p0(kinv-1) )        
       call fluxbelowinv( cbmf, ps0(0:mkx), mkx, kinv, dt, xsrc, xmean, xtop, xbot, xflx )
       slflx(0:kinv-1) = cp * exns0(0:kinv-1) * xflx(0:kinv-1)

       xsrc  = usrc
       xmean = u0(kinv)
       xtop  = u0(kinv+1) + ssu0(kinv+1) * ( ps0(kinv)   - p0(kinv+1) )
       xbot  = u0(kinv-1) + ssu0(kinv-1) * ( ps0(kinv-1) - p0(kinv-1) )
       call fluxbelowinv( cbmf, ps0(0:mkx), mkx, kinv, dt, xsrc, xmean, xtop, xbot, xflx )
       uflx(0:kinv-1) = xflx(0:kinv-1)

       xsrc  = vsrc
       xmean = v0(kinv)
       xtop  = v0(kinv+1) + ssv0(kinv+1) * ( ps0(kinv)   - p0(kinv+1) )
       xbot  = v0(kinv-1) + ssv0(kinv-1) * ( ps0(kinv-1) - p0(kinv-1) )
       call fluxbelowinv( cbmf, ps0(0:mkx), mkx, kinv, dt, xsrc, xmean, xtop, xbot, xflx )
       vflx(0:kinv-1) = xflx(0:kinv-1)

       do m = 1, ncnst
          xsrc  = trsrc(m)
          xmean = tr0(kinv,m)
          xtop  = tr0(kinv+1,m) + sstr0(kinv+1,m) * ( ps0(kinv)   - p0(kinv+1) )
          xbot  = tr0(kinv-1,m) + sstr0(kinv-1,m) * ( ps0(kinv-1) - p0(kinv-1) )        
          call fluxbelowinv( cbmf, ps0(0:mkx), mkx, kinv, dt, xsrc, xmean, xtop, xbot, xflx )
          trflx(0:kinv-1,m) = xflx(0:kinv-1)
       enddo

       
       
       
       
       
       
       

       uplus = 0._r8
       vplus = 0._r8
       do k = kinv, krel - 1
          kp1 = k + 1
          qtflx(k) = cbmf * ( qtsrc  - (  qt0(kp1) +  ssqt0(kp1) * ( ps0(k) - p0(kp1) ) ) )          
          slflx(k) = cbmf * ( thlsrc - ( thl0(kp1) + ssthl0(kp1) * ( ps0(k) - p0(kp1) ) ) ) * cp * exns0(k)
          uplus    = uplus + PGFc * ssu0(k) * ( ps0(k) - ps0(k-1) )
          vplus    = vplus + PGFc * ssv0(k) * ( ps0(k) - ps0(k-1) )
          uflx(k)  = cbmf * ( usrc + uplus -  (  u0(kp1)  +   ssu0(kp1) * ( ps0(k) - p0(kp1) ) ) ) 
          vflx(k)  = cbmf * ( vsrc + vplus -  (  v0(kp1)  +   ssv0(kp1) * ( ps0(k) - p0(kp1) ) ) )
          do m = 1, ncnst
             trflx(k,m) = cbmf * ( trsrc(m)  - (  tr0(kp1,m) +  sstr0(kp1,m) * ( ps0(k) - p0(kp1) ) ) )
          enddo          
       end do

       
       
       
       
       
       

       do k = krel, kbup - 1      
          kp1 = k + 1
          slflx(k) = cp * exns0(k) * umf(k) * ( thlu(k) - ( thl0(kp1) + ssthl0(kp1) * ( ps0(k) - p0(kp1) ) ) )
          qtflx(k) = umf(k) * ( qtu(k) - ( qt0(kp1) + ssqt0(kp1) * ( ps0(k) - p0(kp1) ) ) )
          uflx(k)  = umf(k) * ( uu(k) - ( u0(kp1) + ssu0(kp1) * ( ps0(k) - p0(kp1) ) ) )
          vflx(k)  = umf(k) * ( vu(k) - ( v0(kp1) + ssv0(kp1) * ( ps0(k) - p0(kp1) ) ) )
          do m = 1, ncnst
             trflx(k,m) = umf(k) * ( tru(k,m) - ( tr0(kp1,m) + sstr0(kp1,m) * ( ps0(k) - p0(kp1) ) ) )
          enddo
       end do

       
       
       
       
       
       
       
       
       
       
       
       
       
       
       
       
       
       
       
       
       
       
       
       
       
       
       
       
       
       
       
       
       
       
       
       
       
       
       
       
       
       
       
       
       
       
       
       
       
       
       
       
       
       
       
       
       
       

       do k = kbup, kpen - 1      
          kp1 = k + 1
          slflx(k) = cp * exns0(k) * emf(k) * ( thlu_emf(k) - ( thl0(k) + ssthl0(k) * ( ps0(k) - p0(k) ) ) )
          qtflx(k) =                 emf(k) * (  qtu_emf(k) - (  qt0(k) +  ssqt0(k) * ( ps0(k) - p0(k) ) ) ) 
          uflx(k)  =                 emf(k) * (   uu_emf(k) - (   u0(k) +   ssu0(k) * ( ps0(k) - p0(k) ) ) ) 
          vflx(k)  =                 emf(k) * (   vu_emf(k) - (   v0(k) +   ssv0(k) * ( ps0(k) - p0(k) ) ) )
          do m = 1, ncnst
             trflx(k,m) = emf(k) * ( tru_emf(k,m) - ( tr0(k,m) + sstr0(k,m) * ( ps0(k) - p0(k) ) ) ) 
          enddo
       end do

       
       
       

       if( .not. use_momenflx ) then
           uflx(0:mkx) = 0._r8
           vflx(0:mkx) = 0._r8
       endif       

       
       
       
       
       uemf(0:mkx)         = 0._r8
       do k = 0, kinv - 2  
          uemf(k) = cbmf * ( ps0(0) - ps0(k) ) / ( ps0(0) - ps0(kinv-1) ) 
       end do
       uemf(kinv-1:krel-1) = cbmf
       uemf(krel:kbup-1)   = umf(krel:kbup-1)
       uemf(kbup:kpen-1)   = emf(kbup:kpen-1) 

       comsub(1:mkx) = 0._r8
       do k = 1, kpen
          comsub(k)  = 0.5_r8 * ( uemf(k) + uemf(k-1) ) 
       end do    

       do k = 1, kpen
          if( comsub(k) .ge. 0._r8 ) then
              if( k .eq. mkx ) then
                  thlten_sub = 0._r8
                  qtten_sub  = 0._r8
                  qlten_sub  = 0._r8
                  qiten_sub  = 0._r8
                  nlten_sub  = 0._r8
                  niten_sub  = 0._r8
              else
                  thlten_sub = g * comsub(k) * ( thl0(k+1) - thl0(k) ) / ( p0(k) - p0(k+1) )
                  qtten_sub  = g * comsub(k) * (  qt0(k+1) -  qt0(k) ) / ( p0(k) - p0(k+1) )
                  qlten_sub  = g * comsub(k) * (  ql0(k+1) -  ql0(k) ) / ( p0(k) - p0(k+1) )
                  qiten_sub  = g * comsub(k) * (  qi0(k+1) -  qi0(k) ) / ( p0(k) - p0(k+1) )
                  nlten_sub  = g * comsub(k) * (  tr0(k+1,ixnumliq) -  tr0(k,ixnumliq) ) / ( p0(k) - p0(k+1) )
                  niten_sub  = g * comsub(k) * (  tr0(k+1,ixnumice) -  tr0(k,ixnumice) ) / ( p0(k) - p0(k+1) )
              endif
          else
              if( k .eq. 1 ) then
                  thlten_sub = 0._r8
                  qtten_sub  = 0._r8
                  qlten_sub  = 0._r8
                  qiten_sub  = 0._r8
                  nlten_sub  = 0._r8
                  niten_sub  = 0._r8
              else
                  thlten_sub = g * comsub(k) * ( thl0(k) - thl0(k-1) ) / ( p0(k-1) - p0(k) )
                  qtten_sub  = g * comsub(k) * (  qt0(k) -  qt0(k-1) ) / ( p0(k-1) - p0(k) )
                  qlten_sub  = g * comsub(k) * (  ql0(k) -  ql0(k-1) ) / ( p0(k-1) - p0(k) )
                  qiten_sub  = g * comsub(k) * (  qi0(k) -  qi0(k-1) ) / ( p0(k-1) - p0(k) )
                  nlten_sub  = g * comsub(k) * (  tr0(k,ixnumliq) -  tr0(k-1,ixnumliq) ) / ( p0(k-1) - p0(k) )
                  niten_sub  = g * comsub(k) * (  tr0(k,ixnumice) -  tr0(k-1,ixnumice) ) / ( p0(k-1) - p0(k) )
              endif
          endif
          thl_prog = thl0(k) + thlten_sub * dt
          qt_prog  = max( qt0(k) + qtten_sub * dt, 1.e-12_r8 )
          call conden(p0(k),thl_prog,qt_prog,thj,qvj,qlj,qij,qse,id_check,qsat)
          if( id_check .eq. 1 ) then
              id_exit = .true.
              go to 333
          endif
        
        
          qlten_sink(k) = max( qlten_sub, - ql0(k) / dt ) 
          qiten_sink(k) = max( qiten_sub, - qi0(k) / dt ) 
          nlten_sink(k) = max( nlten_sub, - tr0(k,ixnumliq) / dt ) 
          niten_sink(k) = max( niten_sub, - tr0(k,ixnumice) / dt )
       end do

       
       
       
       
       
       
       
       
       
       
       do k = 1, kpen
          km1 = k - 1 
          uten(k) = ( uflx(km1) - uflx(k) ) * g / dp0(k)
          vten(k) = ( vflx(km1) - vflx(k) ) * g / dp0(k) 
          uf(k)   = u0(k) + uten(k) * dt
          vf(k)   = v0(k) + vten(k) * dt
        
        
        
        
        
        
        
       end do        

       
       
       
       
       
       
       
       
       rliq    = 0._r8
       rainflx = 0._r8
       snowflx = 0._r8

       do k = 1, kpen

          km1 = k - 1

          
          
          
          
          
          
          
          
          
          
          
          
          
          
          
          

          
          
          
          
          
          
          
          
          
          
          
          
          
          
          
          
          
          

          dwten(k) = dwten(k) * 0.5_r8 * ( umf(k-1) + umf(k) ) * g / dp0(k) 
          diten(k) = diten(k) * 0.5_r8 * ( umf(k-1) + umf(k) ) * g / dp0(k) 

          
          

          
          
          
          
          
          
          
          
          
          
          
          
          
          
          

          qrten(k) = frc_rasn * dwten(k)
          qsten(k) = frc_rasn * diten(k) 
 
          
          
          
          
          
          
          
          
          

          rainflx = rainflx + qrten(k) * dp0(k) / g
          snowflx = snowflx + qsten(k) * dp0(k) / g

          
          
          
          
          
          
          
          
          
          
          
          
          
                   
          slten(k) = ( slflx(km1) - slflx(k) ) * g / dp0(k)
          if( k .eq. 1 ) then
              slten(k) = slten(k) - g / 4._r8 / dp0(k) * (                            &
                                    uflx(k)*(uf(k+1) - uf(k) + u0(k+1) - u0(k)) +     & 
                                    vflx(k)*(vf(k+1) - vf(k) + v0(k+1) - v0(k)))
          elseif( k .ge. 2 .and. k .le. kpen-1 ) then
              slten(k) = slten(k) - g / 4._r8 / dp0(k) * (                            &
                                    uflx(k)*(uf(k+1) - uf(k) + u0(k+1) - u0(k)) +     &
                                    uflx(k-1)*(uf(k) - uf(k-1) + u0(k) - u0(k-1)) +   &
                                    vflx(k)*(vf(k+1) - vf(k) + v0(k+1) - v0(k)) +     &
                                    vflx(k-1)*(vf(k) - vf(k-1) + v0(k) - v0(k-1)))
          elseif( k .eq. kpen ) then
              slten(k) = slten(k) - g / 4._r8 / dp0(k) * (                            &
                                    uflx(k-1)*(uf(k) - uf(k-1) + u0(k) - u0(k-1)) +   &
                                    vflx(k-1)*(vf(k) - vf(k-1) + v0(k) - v0(k-1)))
          endif
          qtten(k) = ( qtflx(km1) - qtflx(k) ) * g / dp0(k)

          
          
          
          
          
          
          
  
          

          if( k .lt. krel .or. k .gt. kpen ) then
              qlu_mid = 0._r8
              qiu_mid = 0._r8
              qlj     = 0._r8
              qij     = 0._r8
          elseif( k .eq. krel ) then 
              call conden(prel,thlu(krel-1),qtu(krel-1),thj,qvj,qlj,qij,qse,id_check,qsat)
              if( id_check .eq. 1 ) then
                  exit_conden(i) = 1._r8
                  id_exit = .true.
                  go to 333
              endif
              qlubelow = qlj       
              qiubelow = qij       
              call conden(ps0(k),thlu(k),qtu(k),thj,qvj,qlj,qij,qse,id_check,qsat)
              if( id_check .eq. 1 ) then
                  exit_conden(i) = 1._r8
                  id_exit = .true.
                  go to 333
              end if
              qlu_mid = 0.5_r8 * ( qlubelow + qlj ) * ( prel - ps0(k) )/( ps0(k-1) - ps0(k) )
              qiu_mid = 0.5_r8 * ( qiubelow + qij ) * ( prel - ps0(k) )/( ps0(k-1) - ps0(k) )
          elseif( k .eq. kpen ) then 
              call conden(ps0(k-1)+ppen,thlu_top,qtu_top,thj,qvj,qlj,qij,qse,id_check,qsat)
              if( id_check .eq. 1 ) then
                  exit_conden(i) = 1._r8
                  id_exit = .true.
                  go to 333
              end if
              qlu_mid = 0.5_r8 * ( qlubelow + qlj ) * ( -ppen )        /( ps0(k-1) - ps0(k) )
              qiu_mid = 0.5_r8 * ( qiubelow + qij ) * ( -ppen )        /( ps0(k-1) - ps0(k) )
              qlu_top = qlj
              qiu_top = qij
          else
              call conden(ps0(k),thlu(k),qtu(k),thj,qvj,qlj,qij,qse,id_check,qsat)
              if( id_check .eq. 1 ) then
                  exit_conden(i) = 1._r8
                  id_exit = .true.
                  go to 333
              end if
              qlu_mid = 0.5_r8 * ( qlubelow + qlj )
              qiu_mid = 0.5_r8 * ( qiubelow + qij )
          endif
          qlubelow = qlj       
          qiubelow = qij       

          

          qc_l(k) = ( 1._r8 - frc_rasn ) * dwten(k) 
          qc_i(k) = ( 1._r8 - frc_rasn ) * diten(k) 

          

          if( k .le. kbup ) then 
              qc_l(k) = qc_l(k) + g * 0.5_r8 * ( umf(k-1) + umf(k) ) * fdr(k) * qlu_mid 
              qc_i(k) = qc_i(k) + g * 0.5_r8 * ( umf(k-1) + umf(k) ) * fdr(k) * qiu_mid 
              qc_lm   =         - g * 0.5_r8 * ( umf(k-1) + umf(k) ) * fdr(k) * ql0(k)  
              qc_im   =         - g * 0.5_r8 * ( umf(k-1) + umf(k) ) * fdr(k) * qi0(k)
            
              nc_lm   =         - g * 0.5_r8 * ( umf(k-1) + umf(k) ) * fdr(k) * tr0(k,ixnumliq)  
              nc_im   =         - g * 0.5_r8 * ( umf(k-1) + umf(k) ) * fdr(k) * tr0(k,ixnumice)
          else
              qc_lm   = 0._r8
              qc_im   = 0._r8
              nc_lm   = 0._r8
              nc_im   = 0._r8
          endif

          

          if( k .eq. kbup ) then
              qc_l(k) = qc_l(k) + g * umf(k) * qlj     / ( ps0(k-1) - ps0(k) ) 
              qc_i(k) = qc_i(k) + g * umf(k) * qij     / ( ps0(k-1) - ps0(k) ) 
              qc_lm   = qc_lm   - g * umf(k) * ql0(k)  / ( ps0(k-1) - ps0(k) ) 
              qc_im   = qc_im   - g * umf(k) * qi0(k)  / ( ps0(k-1) - ps0(k) ) 
              nc_lm   = nc_lm   - g * umf(k) * tr0(k,ixnumliq)  / ( ps0(k-1) - ps0(k) ) 
              nc_im   = nc_im   - g * umf(k) * tr0(k,ixnumice)  / ( ps0(k-1) - ps0(k) ) 
          endif 

          
          

          if( k .eq. kbup ) then
              call conden(p0(k),thlu_emf(k),qtu_emf(k),thj,qvj,ql_emf_kbup,qi_emf_kbup,qse,id_check,qsat)
              if( id_check .eq. 1 ) then
                  id_exit = .true.
                  go to 333
              endif
              if( ql_emf_kbup .gt. 0._r8 ) then
                  nl_emf_kbup = tru_emf(k,ixnumliq)
              else
                  nl_emf_kbup = 0._r8
              endif
              if( qi_emf_kbup .gt. 0._r8 ) then
                  ni_emf_kbup = tru_emf(k,ixnumice)
              else
                  ni_emf_kbup = 0._r8
              endif
              qc_lm   = qc_lm   - g * emf(k) * ( ql_emf_kbup - ql0(k) ) / ( ps0(k-1) - ps0(k) ) 
              qc_im   = qc_im   - g * emf(k) * ( qi_emf_kbup - qi0(k) ) / ( ps0(k-1) - ps0(k) ) 
              nc_lm   = nc_lm   - g * emf(k) * ( nl_emf_kbup - tr0(k,ixnumliq) ) / ( ps0(k-1) - ps0(k) ) 
              nc_im   = nc_im   - g * emf(k) * ( ni_emf_kbup - tr0(k,ixnumice) ) / ( ps0(k-1) - ps0(k) ) 
          endif 

          qlten_det   = qc_l(k) + qc_lm
          qiten_det   = qc_i(k) + qc_im

          
          
          
          
          

          if( use_expconten ) then
              if( use_unicondet ) then
                  qc_l(k) = 0._r8
                  qc_i(k) = 0._r8 
                  qlten(k) = frc_rasn * dwten(k) + qlten_sink(k) + qlten_det
                  qiten(k) = frc_rasn * diten(k) + qiten_sink(k) + qiten_det
              else 
                  qlten(k) = qc_l(k) + frc_rasn * dwten(k) + ( max( 0._r8, ql0(k) + ( qc_lm + qlten_sink(k) ) * dt ) - ql0(k) ) / dt
                  qiten(k) = qc_i(k) + frc_rasn * diten(k) + ( max( 0._r8, qi0(k) + ( qc_im + qiten_sink(k) ) * dt ) - qi0(k) ) / dt
                  trten(k,ixnumliq) = max( nc_lm + nlten_sink(k), - tr0(k,ixnumliq) / dt )
                  trten(k,ixnumice) = max( nc_im + niten_sink(k), - tr0(k,ixnumice) / dt )
              endif
          else
              if( use_unicondet ) then
                  qc_l(k) = 0._r8
                  qc_i(k) = 0._r8 
              endif                      
              qlten(k) = dwten(k) + ( qtten(k) - dwten(k) - diten(k) ) * ( ql0(k) / qt0(k) )
              qiten(k) = diten(k) + ( qtten(k) - dwten(k) - diten(k) ) * ( qi0(k) / qt0(k) )
          endif

          qvten(k) = qtten(k) - qlten(k) - qiten(k)
          sten(k)  = slten(k) + xlv * qlten(k) + xls * qiten(k)

          
          
          
          
          
          
          
          
          
          
          
          
          
          
          

          qc(k)  =  qc_l(k) +  qc_i(k)   
          rliq   =  rliq    + qc(k) * dp0(k) / g / 1000._r8    

       end do

          precip  =  rainflx + snowflx                       
          snow    =  snowflx                                 

       
       
       
       
       
       
       
       
       
       
       

       evpint_rain    = 0._r8 
       evpint_snow    = 0._r8
       flxrain(0:mkx) = 0._r8
       flxsnow(0:mkx) = 0._r8
       ntraprd(:mkx)  = 0._r8
       ntsnprd(:mkx)  = 0._r8

       do k = mkx, 1, -1  
          
          
          
          
          
          

          if( t0(k) .gt. 273.16_r8 ) then
              snowmlt = max( 0._r8, flxsnow(k) * g / dp0(k) ) 
          else
              snowmlt = 0._r8
          endif

          
          
          
          
          
          
          
          
          
          
          
          

          status = qsat(t0(k),p0(k),es(1),qs(1),gam(1), 1)          
          subsat = max( ( 1._r8 - qv0(k)/qs(1) ), 0._r8 )
          if( noevap_krelkpen ) then
              if( k .ge. krel ) subsat = 0._r8
          endif

          evprain  = kevp * subsat * sqrt(flxrain(k)+snowmlt*dp0(k)/g) 
          evpsnow  = kevp * subsat * sqrt(max(flxsnow(k)-snowmlt*dp0(k)/g,0._r8))

          evplimit = max( 0._r8, ( qw0_in(i,k) - qv0(k) ) / dt ) 

          evplimit_rain = min( evplimit,      ( flxrain(k) + snowmlt * dp0(k) / g ) * g / dp0(k) )
          evplimit_rain = min( evplimit_rain, ( rainflx - evpint_rain ) * g / dp0(k) )
          evprain = max(0._r8,min( evplimit_rain, evprain ))

          evplimit_snow = min( evplimit,   max( flxsnow(k) - snowmlt * dp0(k) / g , 0._r8 ) * g / dp0(k) )
          evplimit_snow = min( evplimit_snow, ( snowflx - evpint_snow ) * g / dp0(k) )
          evpsnow = max(0._r8,min( evplimit_snow, evpsnow ))

          if( ( evprain + evpsnow ) .gt. evplimit ) then
                tmp1 = evprain * evplimit / ( evprain + evpsnow )
                tmp2 = evpsnow * evplimit / ( evprain + evpsnow )
                evprain = tmp1
                evpsnow = tmp2
          endif

          evapc(k) = evprain + evpsnow

          
          
          

          evpint_rain = evpint_rain + evprain * dp0(k) / g
          evpint_snow = evpint_snow + evpsnow * dp0(k) / g

          
          
          

          ntraprd(k) = qrten(k) - evprain + snowmlt
          ntsnprd(k) = qsten(k) - evpsnow - snowmlt
         
          
          
          
          

          flxrain(k-1) = flxrain(k) + ntraprd(k) * dp0(k) / g
          flxsnow(k-1) = flxsnow(k) + ntsnprd(k) * dp0(k) / g
          flxrain(k-1) = max( flxrain(k-1), 0._r8 )
          if( flxrain(k-1) .eq. 0._r8 ) ntraprd(k) = -flxrain(k) * g / dp0(k)
          flxsnow(k-1) = max( flxsnow(k-1), 0._r8 )         
          if( flxsnow(k-1) .eq. 0._r8 ) ntsnprd(k) = -flxsnow(k) * g / dp0(k)

          
          
          
          
          
          
          
          
          
          
          
          
          
          

          qlten(k) = qlten(k) - qrten(k)
          qiten(k) = qiten(k) - qsten(k)
          qvten(k) = qvten(k) + evprain  + evpsnow
          qtten(k) = qlten(k) + qiten(k) + qvten(k)
          if( ( qv0(k) + qvten(k)*dt ) .lt. qmin(1) .or. &
              ( ql0(k) + qlten(k)*dt ) .lt. qmin(2) .or. &
              ( qi0(k) + qiten(k)*dt ) .lt. qmin(3) ) then
               limit_negcon(i) = 1._r8
          end if
          sten(k)  = sten(k) - xlv*evprain  - xls*evpsnow - (xls-xlv)*snowmlt
          slten(k) = sten(k) - xlv*qlten(k) - xls*qiten(k)

        
        

       end do

       
       
       
       

       precip  = ( flxrain(0) + flxsnow(0) ) / 1000._r8
       snow    =   flxsnow(0) / 1000._r8       

       
       
       
       
       
       
       
       
       
       
       
       
       
       
       
       
       
       
       
       
       
       
       
       
       
       
       
       
       

       do k = 1, kpen       
          qtten(k) = qtten(k) - qc(k)
          qlten(k) = qlten(k) - qc_l(k)
          qiten(k) = qiten(k) - qc_i(k)
          slten(k) = slten(k) + ( xlv * qc_l(k) + xls * qc_i(k) )
          
          
          
          
          
          
          
          
          
          sten(k)  = sten(k)  - ( xls - xlv ) * qc_i(k)
       end do

       
       
       
       
       

       
       
       
       
       

        qv0_star(:mkx) = qv0(:mkx) + qvten(:mkx) * dt
        ql0_star(:mkx) = ql0(:mkx) + qlten(:mkx) * dt
        qi0_star(:mkx) = qi0(:mkx) + qiten(:mkx) * dt
        s0_star(:mkx)  =  s0(:mkx) +  sten(:mkx) * dt
        call positive_moisture_single( xlv, xls, mkx, dt, qmin(1), qmin(2), qmin(3), dp0, &
                                       qv0_star, ql0_star, qi0_star, s0_star, qvten, qlten, qiten, sten )
        qtten(:mkx)    = qvten(:mkx) + qlten(:mkx) + qiten(:mkx)
        slten(:mkx)    = sten(:mkx)  - xlv * qlten(:mkx) - xls * qiten(:mkx)

       
       
       

       do m = 4, ncnst

       if( m .ne. ixnumliq .and. m .ne. ixnumice ) then

          trmin = qmin(m)
          do mm = 1, ntot_amode
             if( m .eq. numptr_amode(mm) ) then
                 trmin = 1.e-5_r8
                 goto 55
             endif              
          enddo
       55 continue
          trflx_d(0:mkx) = 0._r8
          trflx_u(0:mkx) = 0._r8           
          do k = 1, mkx-1
             if( cnst_get_type_byind(m) .eq. 'wet' ) then
                 pdelx = dp0(k)
             else
                 pdelx = dpdry0(k)
             endif
             km1 = k - 1
             dum = ( tr0(k,m) - trmin ) *  pdelx / g / dt + trflx(km1,m) - trflx(k,m) + trflx_d(km1)
             trflx_d(k) = min( 0._r8, dum )
          enddo
          do k = mkx, 2, -1
             if( cnst_get_type_byind(m) .eq. 'wet' ) then
                 pdelx = dp0(k)
             else
                 pdelx = dpdry0(k)
             endif
             km1 = k - 1
             dum = ( tr0(k,m) - trmin ) * pdelx / g / dt + trflx(km1,m) - trflx(k,m) + &
                                                           trflx_d(km1) - trflx_d(k) - trflx_u(k) 
             trflx_u(km1) = max( 0._r8, -dum ) 
          enddo
          do k = 1, mkx
             if( cnst_get_type_byind(m) .eq. 'wet' ) then
                 pdelx = dp0(k)
             else
                 pdelx = dpdry0(k)
             endif
             km1 = k - 1
           
           
             trten(k,m) = ( trflx(km1,m) - trflx(k,m) + & 
                            trflx_d(km1) - trflx_d(k) + &
                            trflx_u(km1) - trflx_u(k) ) * g / pdelx
          enddo

       endif

       enddo

       
       
       
       
       
       
       
 
       call conden(prel,thlu(krel-1),qtu(krel-1),thj,qvj,qlj,qij,qse,id_check,qsat)
       if( id_check .eq. 1 ) then
           exit_conden(i) = 1._r8
           id_exit = .true.
           go to 333
       end if
       qcubelow = qlj + qij
       qlubelow = qlj       
       qiubelow = qij       
       rcwp     = 0._r8
       rlwp     = 0._r8
       riwp     = 0._r8

       
       
       
       
       do k = krel, kpen 
          
          
          
          
          
          if( k .eq. kpen ) then 
              call conden(ps0(k-1)+ppen,thlu_top,qtu_top,thj,qvj,qlj,qij,qse,id_check,qsat)
          else
              call conden(ps0(k),thlu(k),qtu(k),thj,qvj,qlj,qij,qse,id_check,qsat)
          endif
          if( id_check .eq. 1 ) then
              exit_conden(i) = 1._r8
              id_exit = .true.
              go to 333
          end if
          
          
          
          
          
          
          
          
          qcu(k)   = 0.5_r8 * ( qcubelow + qlj + qij )
          qlu(k)   = 0.5_r8 * ( qlubelow + qlj )
          qiu(k)   = 0.5_r8 * ( qiubelow + qij )
          cufrc(k) = ( ufrc(k-1) + ufrc(k) )
          if( k .eq. krel ) then
              cufrc(k) = ( ufrclcl + ufrc(k) )*( prel - ps0(k) )/( ps0(k-1) - ps0(k) )
          else if( k .eq. kpen ) then
              cufrc(k) = ( ufrc(k-1) + 0._r8 )*( -ppen )        /( ps0(k-1) - ps0(k) )
              if( (qlj + qij) .gt. criqc ) then           
                   qcu(k) = 0.5_r8 * ( qcubelow + criqc )
                   qlu(k) = 0.5_r8 * ( qlubelow + criqc * qlj / ( qlj + qij ) )
                   qiu(k) = 0.5_r8 * ( qiubelow + criqc * qij / ( qlj + qij ) )
              endif
          endif  
          rcwp = rcwp + ( qlu(k) + qiu(k) ) * ( ps0(k-1) - ps0(k) ) / g * cufrc(k)
          rlwp = rlwp +   qlu(k)            * ( ps0(k-1) - ps0(k) ) / g * cufrc(k)
          riwp = riwp +   qiu(k)            * ( ps0(k-1) - ps0(k) ) / g * cufrc(k)
          qcubelow = qlj + qij
          qlubelow = qlj
          qiubelow = qij
       end do
       
       
       
       cnt = real( kpen, r8 )
       cnb = real( krel - 1, r8 )

       
       
       
       
       
       
       
       

       if( iter .ne. iter_cin ) then 

          
          
          
          
          

          qv0_s(:mkx)           = qv0(:mkx) + qvten(:mkx) * dt
          ql0_s(:mkx)           = ql0(:mkx) + qlten(:mkx) * dt
          qi0_s(:mkx)           = qi0(:mkx) + qiten(:mkx) * dt
          s0_s(:mkx)            = s0(:mkx)  +  sten(:mkx) * dt 
          u0_s(:mkx)            = u0(:mkx)  +  uten(:mkx) * dt
          v0_s(:mkx)            = v0(:mkx)  +  vten(:mkx) * dt 
          qt0_s(:mkx)           = qv0_s(:mkx) + ql0_s(:mkx) + qi0_s(:mkx)
          t0_s(:mkx)            = t0(:mkx)  +  sten(:mkx) * dt / cp
          do m = 1, ncnst
             tr0_s(:mkx,m)      = tr0(:mkx,m) + trten(:mkx,m) * dt
          enddo

          umf_s(0:mkx)          = umf(0:mkx)
          qvten_s(:mkx)         = qvten(:mkx)
          qlten_s(:mkx)         = qlten(:mkx)  
          qiten_s(:mkx)         = qiten(:mkx)
          sten_s(:mkx)          = sten(:mkx)
          uten_s(:mkx)          = uten(:mkx)  
          vten_s(:mkx)          = vten(:mkx)
          qrten_s(:mkx)         = qrten(:mkx)
          qsten_s(:mkx)         = qsten(:mkx)  
          precip_s              = precip
          snow_s                = snow
          evapc_s(:mkx)         = evapc(:mkx)
          cush_s                = cush
          cufrc_s(:mkx)         = cufrc(:mkx)  
          slflx_s(0:mkx)        = slflx(0:mkx)  
          qtflx_s(0:mkx)        = qtflx(0:mkx)  
          qcu_s(:mkx)           = qcu(:mkx)  
          qlu_s(:mkx)           = qlu(:mkx)  
          qiu_s(:mkx)           = qiu(:mkx)  
          fer_s(:mkx)           = fer(:mkx)  
          fdr_s(:mkx)           = fdr(:mkx)  
          cin_s                 = cin
          cinlcl_s              = cinlcl
          cbmf_s                = cbmf
          rliq_s                = rliq
          qc_s(:mkx)            = qc(:mkx)
          cnt_s                 = cnt
          cnb_s                 = cnb
          qtten_s(:mkx)         = qtten(:mkx)
          slten_s(:mkx)         = slten(:mkx)
          ufrc_s(0:mkx)         = ufrc(0:mkx) 

          uflx_s(0:mkx)         = uflx(0:mkx)  
          vflx_s(0:mkx)         = vflx(0:mkx)  
           
          ufrcinvbase_s         = ufrcinvbase
          ufrclcl_s             = ufrclcl 
          winvbase_s            = winvbase
          wlcl_s                = wlcl
          plcl_s                = plcl
          pinv_s                = ps0(kinv-1)
          plfc_s                = plfc        
          pbup_s                = ps0(kbup)
          ppen_s                = ps0(kpen-1) + ppen        
          qtsrc_s               = qtsrc
          thlsrc_s              = thlsrc
          thvlsrc_s             = thvlsrc
          emfkbup_s             = emf(kbup)
          cbmflimit_s           = cbmflimit
          tkeavg_s              = tkeavg
          zinv_s                = zs0(kinv-1)
          rcwp_s                = rcwp
          rlwp_s                = rlwp
          riwp_s                = riwp

          wu_s(0:mkx)           = wu(0:mkx)
          qtu_s(0:mkx)          = qtu(0:mkx)
          thlu_s(0:mkx)         = thlu(0:mkx)
          thvu_s(0:mkx)         = thvu(0:mkx)
          uu_s(0:mkx)           = uu(0:mkx)
          vu_s(0:mkx)           = vu(0:mkx)
          qtu_emf_s(0:mkx)      = qtu_emf(0:mkx)
          thlu_emf_s(0:mkx)     = thlu_emf(0:mkx)
          uu_emf_s(0:mkx)       = uu_emf(0:mkx)
          vu_emf_s(0:mkx)       = vu_emf(0:mkx)
          uemf_s(0:mkx)         = uemf(0:mkx)

          dwten_s(:mkx)         = dwten(:mkx)
          diten_s(:mkx)         = diten(:mkx)
          flxrain_s(0:mkx)      = flxrain(0:mkx)
          flxsnow_s(0:mkx)      = flxsnow(0:mkx)
          ntraprd_s(:mkx)       = ntraprd(:mkx)
          ntsnprd_s(:mkx)       = ntsnprd(:mkx)

          excessu_arr_s(:mkx)   = excessu_arr(:mkx)
          excess0_arr_s(:mkx)   = excess0_arr(:mkx)
          xc_arr_s(:mkx)        = xc_arr(:mkx)
          aquad_arr_s(:mkx)     = aquad_arr(:mkx)
          bquad_arr_s(:mkx)     = bquad_arr(:mkx)
          cquad_arr_s(:mkx)     = cquad_arr(:mkx)
          bogbot_arr_s(:mkx)    = bogbot_arr(:mkx)
          bogtop_arr_s(:mkx)    = bogtop_arr(:mkx)

          do m = 1, ncnst
             trten_s(:mkx,m)    = trten(:mkx,m)
             trflx_s(0:mkx,m)   = trflx(0:mkx,m)
             tru_s(0:mkx,m)     = tru(0:mkx,m)
             tru_emf_s(0:mkx,m) = tru_emf(0:mkx,m)
          enddo

          
          
          
          
          
          
          qv0(:mkx)   = qv0_s(:mkx)
          ql0(:mkx)   = ql0_s(:mkx)
          qi0(:mkx)   = qi0_s(:mkx)
          s0(:mkx)    = s0_s(:mkx)
          t0(:mkx)    = t0_s(:mkx)
      
          qt0(:mkx)   = (qv0(:mkx) + ql0(:mkx) + qi0(:mkx))
          thl0(:mkx)  = (t0(:mkx) - xlv*ql0(:mkx)/cp - xls*qi0(:mkx)/cp)/exn0(:mkx)
          thvl0(:mkx) = (1._r8 + zvir*qt0(:mkx))*thl0(:mkx)

          ssthl0      = slope(mkx,thl0,p0) 
          ssqt0       = slope(mkx,qt0 ,p0)
          ssu0        = slope(mkx,u0  ,p0)
          ssv0        = slope(mkx,v0  ,p0)
          do m = 1, ncnst
             sstr0(:mkx,m) = slope(mkx,tr0(:mkx,m),p0)
          enddo

          do k = 1, mkx

             thl0bot = thl0(k) + ssthl0(k) * ( ps0(k-1) - p0(k) )
             qt0bot  = qt0(k)  + ssqt0(k)  * ( ps0(k-1) - p0(k) )
             call conden(ps0(k-1),thl0bot,qt0bot,thj,qvj,qlj,qij,qse,id_check,qsat)
             if( id_check .eq. 1 ) then
                 exit_conden(i) = 1._r8
                 id_exit = .true.
                 go to 333
             end if
             thv0bot(k)  = thj * ( 1._r8 + zvir*qvj - qlj - qij )
             thvl0bot(k) = thl0bot * ( 1._r8 + zvir*qt0bot )
          
             thl0top = thl0(k) + ssthl0(k) * ( ps0(k) - p0(k) )
             qt0top  =  qt0(k) + ssqt0(k)  * ( ps0(k) - p0(k) )
             call conden(ps0(k),thl0top,qt0top,thj,qvj,qlj,qij,qse,id_check,qsat)
             if( id_check .eq. 1 ) then
                 exit_conden(i) = 1._r8
                 id_exit = .true.
                 go to 333
             end if
             thv0top(k)  = thj * ( 1._r8 + zvir*qvj - qlj - qij )
             thvl0top(k) = thl0top * ( 1._r8 + zvir*qt0top )

          end do

       endif               

     end do                

     
     
     

     umf_out(i,0:mkx)             = umf(0:mkx)
     slflx_out(i,0:mkx)           = slflx(0:mkx)
     qtflx_out(i,0:mkx)           = qtflx(0:mkx)

     flxprc1_out(i,0:mkx)         = flxrain(0:mkx) + flxsnow(0:mkx)
     flxsnow1_out(i,0:mkx)        = flxsnow(0:mkx)
     qvten_out(i,:mkx)            = qvten(:mkx)
     qlten_out(i,:mkx)            = qlten(:mkx)
     qiten_out(i,:mkx)            = qiten(:mkx)
     sten_out(i,:mkx)             = sten(:mkx)
     uten_out(i,:mkx)             = uten(:mkx)
     vten_out(i,:mkx)             = vten(:mkx)
     qrten_out(i,:mkx)            = qrten(:mkx)
     qsten_out(i,:mkx)            = qsten(:mkx)
     precip_out(i)                = precip
     snow_out(i)                  = snow
     evapc_out(i,:mkx)            = evapc(:mkx)
     cufrc_out(i,:mkx)            = cufrc(:mkx)
     qcu_out(i,:mkx)              = qcu(:mkx)
     qlu_out(i,:mkx)              = qlu(:mkx)
     qiu_out(i,:mkx)              = qiu(:mkx)
     cush_inout(i)                = cush
     cbmf_out(i)                  = cbmf
     rliq_out(i)                  = rliq
     qc_out(i,:mkx)               = qc(:mkx)
     cnt_out(i)                   = cnt
     cnb_out(i)                   = cnb

     do m = 1, ncnst
        trten_out(i,:mkx,m)       = trten(:mkx,m)
     enddo
  
     
     
     
     

     fer_out(i,mkx:1:-1)          = fer(:mkx)  
     fdr_out(i,mkx:1:-1)          = fdr(:mkx)  
     cinh_out(i)                  = cin
     cinlclh_out(i)               = cinlcl
     qtten_out(i,mkx:1:-1)        = qtten(:mkx)
     slten_out(i,mkx:1:-1)        = slten(:mkx)
     ufrc_out(i,mkx:0:-1)         = ufrc(0:mkx)
     uflx_out(i,mkx:0:-1)         = uflx(0:mkx)  
     vflx_out(i,mkx:0:-1)         = vflx(0:mkx)  
     
     ufrcinvbase_out(i)           = ufrcinvbase
     ufrclcl_out(i)               = ufrclcl 
     winvbase_out(i)              = winvbase
     wlcl_out(i)                  = wlcl
     plcl_out(i)                  = plcl
     pinv_out(i)                  = ps0(kinv-1)
     plfc_out(i)                  = plfc    
     pbup_out(i)                  = ps0(kbup)        
     ppen_out(i)                  = ps0(kpen-1) + ppen            
     qtsrc_out(i)                 = qtsrc
     thlsrc_out(i)                = thlsrc
     thvlsrc_out(i)               = thvlsrc
     emfkbup_out(i)               = emf(kbup)
     cbmflimit_out(i)             = cbmflimit
     tkeavg_out(i)                = tkeavg
     zinv_out(i)                  = zs0(kinv-1)
     rcwp_out(i)                  = rcwp
     rlwp_out(i)                  = rlwp
     riwp_out(i)                  = riwp

     wu_out(i,mkx:0:-1)           = wu(0:mkx)
     qtu_out(i,mkx:0:-1)          = qtu(0:mkx)
     thlu_out(i,mkx:0:-1)         = thlu(0:mkx)
     thvu_out(i,mkx:0:-1)         = thvu(0:mkx)
     uu_out(i,mkx:0:-1)           = uu(0:mkx)
     vu_out(i,mkx:0:-1)           = vu(0:mkx)
     qtu_emf_out(i,mkx:0:-1)      = qtu_emf(0:mkx)
     thlu_emf_out(i,mkx:0:-1)     = thlu_emf(0:mkx)
     uu_emf_out(i,mkx:0:-1)       = uu_emf(0:mkx)
     vu_emf_out(i,mkx:0:-1)       = vu_emf(0:mkx)
     uemf_out(i,mkx:0:-1)         = uemf(0:mkx)

     dwten_out(i,mkx:1:-1)        = dwten(:mkx)
     diten_out(i,mkx:1:-1)        = diten(:mkx)
     flxrain_out(i,mkx:0:-1)      = flxrain(0:mkx)
     flxsnow_out(i,mkx:0:-1)      = flxsnow(0:mkx)
     ntraprd_out(i,mkx:1:-1)      = ntraprd(:mkx)
     ntsnprd_out(i,mkx:1:-1)      = ntsnprd(:mkx)

     excessu_arr_out(i,mkx:1:-1)  = excessu_arr(:mkx)
     excess0_arr_out(i,mkx:1:-1)  = excess0_arr(:mkx)
     xc_arr_out(i,mkx:1:-1)       = xc_arr(:mkx)
     aquad_arr_out(i,mkx:1:-1)    = aquad_arr(:mkx)
     bquad_arr_out(i,mkx:1:-1)    = bquad_arr(:mkx)
     cquad_arr_out(i,mkx:1:-1)    = cquad_arr(:mkx)
     bogbot_arr_out(i,mkx:1:-1)   = bogbot_arr(:mkx)
     bogtop_arr_out(i,mkx:1:-1)   = bogtop_arr(:mkx)

     do m = 1, ncnst
        trflx_out(i,mkx:0:-1,m)   = trflx(0:mkx,m)  
        tru_out(i,mkx:0:-1,m)     = tru(0:mkx,m)
        tru_emf_out(i,mkx:0:-1,m) = tru_emf(0:mkx,m)
     enddo

 333 if(id_exit) then 

     exit_UWCu(i) = 1._r8

     
     
     
     
     umf_out(i,0:mkx)             = 0._r8   
     slflx_out(i,0:mkx)           = 0._r8
     qtflx_out(i,0:mkx)           = 0._r8
     qvten_out(i,:mkx)            = 0._r8
     qlten_out(i,:mkx)            = 0._r8
     qiten_out(i,:mkx)            = 0._r8
     sten_out(i,:mkx)             = 0._r8
     uten_out(i,:mkx)             = 0._r8
     vten_out(i,:mkx)             = 0._r8
     qrten_out(i,:mkx)            = 0._r8
     qsten_out(i,:mkx)            = 0._r8
     precip_out(i)                = 0._r8
     snow_out(i)                  = 0._r8
     evapc_out(i,:mkx)            = 0._r8
     cufrc_out(i,:mkx)            = 0._r8
     qcu_out(i,:mkx)              = 0._r8
     qlu_out(i,:mkx)              = 0._r8
     qiu_out(i,:mkx)              = 0._r8
     cush_inout(i)                = -1._r8
     cbmf_out(i)                  = 0._r8   
     rliq_out(i)                  = 0._r8
     qc_out(i,:mkx)               = 0._r8
     cnt_out(i)                   = 1._r8
     cnb_out(i)                   = real(mkx, r8)

     fer_out(i,mkx:1:-1)          = 0._r8  
     fdr_out(i,mkx:1:-1)          = 0._r8  
     cinh_out(i)                  = -1._r8 
     cinlclh_out(i)               = -1._r8 
     qtten_out(i,mkx:1:-1)        = 0._r8
     slten_out(i,mkx:1:-1)        = 0._r8
     ufrc_out(i,mkx:0:-1)         = 0._r8
     uflx_out(i,mkx:0:-1)         = 0._r8  
     vflx_out(i,mkx:0:-1)         = 0._r8  

     ufrcinvbase_out(i)           = 0._r8 
     ufrclcl_out(i)               = 0._r8 
     winvbase_out(i)              = 0._r8    
     wlcl_out(i)                  = 0._r8    
     plcl_out(i)                  = 0._r8    
     pinv_out(i)                  = 0._r8     
     plfc_out(i)                  = 0._r8     
     pbup_out(i)                  = 0._r8    
     ppen_out(i)                  = 0._r8    
     qtsrc_out(i)                 = 0._r8    
     thlsrc_out(i)                = 0._r8    
     thvlsrc_out(i)               = 0._r8    
     emfkbup_out(i)               = 0._r8
     cbmflimit_out(i)             = 0._r8    
     tkeavg_out(i)                = 0._r8    
     zinv_out(i)                  = 0._r8    
     rcwp_out(i)                  = 0._r8    
     rlwp_out(i)                  = 0._r8    
     riwp_out(i)                  = 0._r8    

     wu_out(i,mkx:0:-1)           = 0._r8    
     qtu_out(i,mkx:0:-1)          = 0._r8        
     thlu_out(i,mkx:0:-1)         = 0._r8         
     thvu_out(i,mkx:0:-1)         = 0._r8         
     uu_out(i,mkx:0:-1)           = 0._r8        
     vu_out(i,mkx:0:-1)           = 0._r8        
     qtu_emf_out(i,mkx:0:-1)      = 0._r8         
     thlu_emf_out(i,mkx:0:-1)     = 0._r8         
     uu_emf_out(i,mkx:0:-1)       = 0._r8          
     vu_emf_out(i,mkx:0:-1)       = 0._r8    
     uemf_out(i,mkx:0:-1)         = 0._r8    
   
     dwten_out(i,mkx:1:-1)        = 0._r8    
     diten_out(i,mkx:1:-1)        = 0._r8    
     flxrain_out(i,mkx:0:-1)      = 0._r8     
     flxsnow_out(i,mkx:0:-1)      = 0._r8    
     ntraprd_out(i,mkx:1:-1)      = 0._r8    
     ntsnprd_out(i,mkx:1:-1)      = 0._r8    

     excessu_arr_out(i,mkx:1:-1)  = 0._r8    
     excess0_arr_out(i,mkx:1:-1)  = 0._r8    
     xc_arr_out(i,mkx:1:-1)       = 0._r8    
     aquad_arr_out(i,mkx:1:-1)    = 0._r8    
     bquad_arr_out(i,mkx:1:-1)    = 0._r8    
     cquad_arr_out(i,mkx:1:-1)    = 0._r8    
     bogbot_arr_out(i,mkx:1:-1)   = 0._r8    
     bogtop_arr_out(i,mkx:1:-1)   = 0._r8    

     do m = 1, ncnst
        trten_out(i,:mkx,m)       = 0._r8
        trflx_out(i,mkx:0:-1,m)   = 0._r8  
        tru_out(i,mkx:0:-1,m)     = 0._r8
        tru_emf_out(i,mkx:0:-1,m) = 0._r8
     enddo

     end if

     end do                  

     
     
     

     call outfld( 'qtflx_Cu'        , qtflx_out(:,mkx:0:-1),    mix,    lchnk ) 
     call outfld( 'slflx_Cu'        , slflx_out(:,mkx:0:-1),    mix,    lchnk ) 
     call outfld( 'uflx_Cu'         , uflx_out,                 mix,    lchnk ) 
     call outfld( 'vflx_Cu'         , vflx_out,                 mix,    lchnk ) 

     call outfld( 'qtten_Cu'        , qtten_out,                mix,    lchnk ) 
     call outfld( 'slten_Cu'        , slten_out,                mix,    lchnk ) 
     call outfld( 'uten_Cu'         , uten_out(:,mkx:1:-1),     mix,    lchnk ) 
     call outfld( 'vten_Cu'         , vten_out(:,mkx:1:-1),     mix,    lchnk ) 
     call outfld( 'qvten_Cu'        , qvten_out(:,mkx:1:-1),    mix,    lchnk ) 
     call outfld( 'qlten_Cu'        , qlten_out(:,mkx:1:-1),    mix,    lchnk )
     call outfld( 'qiten_Cu'        , qiten_out(:,mkx:1:-1),    mix,    lchnk )   

     call outfld( 'cbmf_Cu'         , cbmf_out,                 mix,    lchnk ) 
     call outfld( 'ufrcinvbase_Cu'  , ufrcinvbase_out,          mix,    lchnk ) 
     call outfld( 'ufrclcl_Cu'      , ufrclcl_out,              mix,    lchnk ) 
     call outfld( 'winvbase_Cu'     , winvbase_out,             mix,    lchnk ) 
     call outfld( 'wlcl_Cu'         , wlcl_out,                 mix,    lchnk ) 
     call outfld( 'plcl_Cu'         , plcl_out,                 mix,    lchnk ) 
     call outfld( 'pinv_Cu'         , pinv_out,                 mix,    lchnk ) 
     call outfld( 'plfc_Cu'         , plfc_out,                 mix,    lchnk ) 
     call outfld( 'pbup_Cu'         , pbup_out,                 mix,    lchnk ) 
     call outfld( 'ppen_Cu'         , ppen_out,                 mix,    lchnk ) 
     call outfld( 'qtsrc_Cu'        , qtsrc_out,                mix,    lchnk ) 
     call outfld( 'thlsrc_Cu'       , thlsrc_out,               mix,    lchnk ) 
     call outfld( 'thvlsrc_Cu'      , thvlsrc_out,              mix,    lchnk ) 
     call outfld( 'emfkbup_Cu'      , emfkbup_out,              mix,    lchnk )
     call outfld( 'cin_Cu'          , cinh_out,                 mix,    lchnk )  
     call outfld( 'cinlcl_Cu'       , cinlclh_out,              mix,    lchnk ) 
     call outfld( 'cbmflimit_Cu'    , cbmflimit_out,            mix,    lchnk ) 
     call outfld( 'tkeavg_Cu'       , tkeavg_out,               mix,    lchnk )
     call outfld( 'zinv_Cu'         , zinv_out,                 mix,    lchnk )  
     call outfld( 'rcwp_Cu'         , rcwp_out,                 mix,    lchnk )
     call outfld( 'rlwp_Cu'         , rlwp_out,                 mix,    lchnk )
     call outfld( 'riwp_Cu'         , riwp_out,                 mix,    lchnk )
     call outfld( 'tophgt_Cu'       , cush_inout,               mix,    lchnk )   

     call outfld( 'wu_Cu'           , wu_out,                   mix,    lchnk )
     call outfld( 'ufrc_Cu'         , ufrc_out,                 mix,    lchnk )
     call outfld( 'qtu_Cu'          , qtu_out,                  mix,    lchnk )
     call outfld( 'thlu_Cu'         , thlu_out,                 mix,    lchnk )
     call outfld( 'thvu_Cu'         , thvu_out,                 mix,    lchnk )
     call outfld( 'uu_Cu'           , uu_out,                   mix,    lchnk )
     call outfld( 'vu_Cu'           , vu_out,                   mix,    lchnk )
     call outfld( 'qtu_emf_Cu'      , qtu_emf_out,              mix,    lchnk )
     call outfld( 'thlu_emf_Cu'     , thlu_emf_out,             mix,    lchnk )
     call outfld( 'uu_emf_Cu'       , uu_emf_out,               mix,    lchnk )
     call outfld( 'vu_emf_Cu'       , vu_emf_out,               mix,    lchnk )
     call outfld( 'umf_Cu'          , umf_out(:,mkx:0:-1),      mix,    lchnk )
     call outfld( 'uemf_Cu'         , uemf_out,                 mix,    lchnk )
     call outfld( 'qcu_Cu'          , qcu_out(:,mkx:1:-1),      mix,    lchnk )
     call outfld( 'qlu_Cu'          , qlu_out(:,mkx:1:-1),      mix,    lchnk )
     call outfld( 'qiu_Cu'          , qiu_out(:,mkx:1:-1),      mix,    lchnk )
     call outfld( 'cufrc_Cu'        , cufrc_out(:,mkx:1:-1),    mix,    lchnk )  
     call outfld( 'fer_Cu'          , fer_out,                  mix,    lchnk )  
     call outfld( 'fdr_Cu'          , fdr_out,                  mix,    lchnk )  

     call outfld( 'dwten_Cu'        , dwten_out,                mix,    lchnk )
     call outfld( 'diten_Cu'        , diten_out,                mix,    lchnk )
     call outfld( 'qrten_Cu'        , qrten_out(:,mkx:1:-1),    mix,    lchnk )
     call outfld( 'qsten_Cu'        , qsten_out(:,mkx:1:-1),    mix,    lchnk )
     call outfld( 'flxrain_Cu'      , flxrain_out,              mix,    lchnk )
     call outfld( 'flxsnow_Cu'      , flxsnow_out,              mix,    lchnk )
     call outfld( 'ntraprd_Cu'      , ntraprd_out,              mix,    lchnk )
     call outfld( 'ntsnprd_Cu'      , ntsnprd_out,              mix,    lchnk )

     call outfld( 'excessu_Cu'      , excessu_arr_out,          mix,    lchnk )
     call outfld( 'excess0_Cu'      , excess0_arr_out,          mix,    lchnk )
     call outfld( 'xc_Cu'           , xc_arr_out,               mix,    lchnk )
     call outfld( 'aquad_Cu'        , aquad_arr_out,            mix,    lchnk )
     call outfld( 'bquad_Cu'        , bquad_arr_out,            mix,    lchnk )
     call outfld( 'cquad_Cu'        , cquad_arr_out,            mix,    lchnk )
     call outfld( 'bogbot_Cu'       , bogbot_arr_out,           mix,    lchnk )
     call outfld( 'bogtop_Cu'       , bogtop_arr_out,           mix,    lchnk )

     call outfld( 'exit_UWCu_Cu'    , exit_UWCu,                mix,    lchnk ) 
     call outfld( 'exit_conden_Cu'  , exit_conden,              mix,    lchnk ) 
     call outfld( 'exit_klclmkx_Cu' , exit_klclmkx,             mix,    lchnk ) 
     call outfld( 'exit_klfcmkx_Cu' , exit_klfcmkx,             mix,    lchnk ) 
     call outfld( 'exit_ufrc_Cu'    , exit_ufrc,                mix,    lchnk ) 
     call outfld( 'exit_wtw_Cu'     , exit_wtw,                 mix,    lchnk ) 
     call outfld( 'exit_drycore_Cu' , exit_drycore,             mix,    lchnk ) 
     call outfld( 'exit_wu_Cu'      , exit_wu,                  mix,    lchnk ) 
     call outfld( 'exit_cufilter_Cu', exit_cufilter,            mix,    lchnk ) 
     call outfld( 'exit_kinv1_Cu'   , exit_kinv1,               mix,    lchnk ) 
     call outfld( 'exit_rei_Cu'     , exit_rei,                 mix,    lchnk ) 

     call outfld( 'limit_shcu_Cu'   , limit_shcu,               mix,    lchnk ) 
     call outfld( 'limit_negcon_Cu' , limit_negcon,             mix,    lchnk ) 
     call outfld( 'limit_ufrc_Cu'   , limit_ufrc,               mix,    lchnk ) 
     call outfld( 'limit_ppen_Cu'   , limit_ppen,               mix,    lchnk ) 
     call outfld( 'limit_emf_Cu'    , limit_emf,                mix,    lchnk ) 
     call outfld( 'limit_cinlcl_Cu' , limit_cinlcl,             mix,    lchnk ) 
     call outfld( 'limit_cin_Cu'    , limit_cin,                mix,    lchnk ) 
     call outfld( 'limit_cbmf_Cu'   , limit_cbmf,               mix,    lchnk ) 
     call outfld( 'limit_rei_Cu'    , limit_rei,                mix,    lchnk ) 
     call outfld( 'ind_delcin_Cu'   , ind_delcin,               mix,    lchnk ) 

    return

  end subroutine compute_uwshcu

  
  
  
  
  

  subroutine getbuoy(pbot,thv0bot,ptop,thv0top,thvubot,thvutop,plfc,cin)
  
  
  
  
  
  
  
  
  
    real(r8) pbot,thv0bot,ptop,thv0top,thvubot,thvutop,plfc,cin,frc

    if( thvubot .gt. thv0bot .and. thvutop .gt. thv0top ) then
        plfc = pbot
        return
    elseif( thvubot .le. thv0bot .and. thvutop .le. thv0top ) then 
        cin  = cin - ( (thvubot/thv0bot - 1._r8) + (thvutop/thv0top - 1._r8)) * (pbot - ptop) /        &
                     ( pbot/(r*thv0bot*exnf(pbot)) + ptop/(r*thv0top*exnf(ptop)) )
    elseif( thvubot .gt. thv0bot .and. thvutop .le. thv0top ) then 
        frc  = ( thvutop/thv0top - 1._r8 ) / ( (thvutop/thv0top - 1._r8) - (thvubot/thv0bot - 1._r8) )
        cin  = cin - ( thvutop/thv0top - 1._r8 ) * ( (ptop + frc*(pbot - ptop)) - ptop ) /             &
                     ( pbot/(r*thv0bot*exnf(pbot)) + ptop/(r*thv0top*exnf(ptop)) )
    else            
        frc  = ( thvubot/thv0bot - 1._r8 ) / ( (thvubot/thv0bot - 1._r8) - (thvutop/thv0top - 1._r8) )
        plfc = pbot - frc * ( pbot - ptop )
        cin  = cin - ( thvubot/thv0bot - 1._r8)*(pbot - plfc)/                                         & 
                     ( pbot/(r*thv0bot*exnf(pbot)) + ptop/(r*thv0top * exnf(ptop)))
    endif

    return
  end subroutine getbuoy

  function single_cin(pbot,thv0bot,ptop,thv0top,thvubot,thvutop)
  
  
  
  
    real(r8) :: single_cin
    real(r8)    pbot,thv0bot,ptop,thv0top,thvubot,thvutop 

    single_cin = ( (1._r8 - thvubot/thv0bot) + (1._r8 - thvutop/thv0top)) * ( pbot - ptop ) / &
                 ( pbot/(r*thv0bot*exnf(pbot)) + ptop/(r*thv0top*exnf(ptop)) )
    return
  end function single_cin   


  subroutine conden(p,thl,qt,th,qv,ql,qi,rvls,id_check,qsat)
  
  
  
    implicit none
    real(r8), intent(in)  :: p
    real(r8), intent(in)  :: thl
    real(r8), intent(in)  :: qt
    real(r8), intent(out) :: th
    real(r8), intent(out) :: qv
    real(r8), intent(out) :: ql
    real(r8), intent(out) :: qi
    real(r8), intent(out) :: rvls
    integer , intent(out) :: id_check
    integer , external    :: qsat
    real(r8)              :: tc,temps,t
    real(r8)              :: leff, nu, qc
    integer               :: iteration
    real(r8)              :: es(1)              
    real(r8)              :: qs(1)              
    real(r8)              :: gam(1)             
    integer               :: status             

    tc   = thl*exnf(p)
  
  
  
  
    nu   = max(min((268._r8 - tc)/20._r8,1.0_r8),0.0_r8)  
    leff = (1._r8 - nu)*xlv + nu*xls                      

    
    
    
    
    

    temps  = tc
    status = qsat(temps,p,es(1),qs(1),gam(1), 1)
    rvls   = qs(1)

    if( qs(1) .ge. qt ) then  
        id_check = 0
        qv = qt
        qc = 0._r8
        ql = 0._r8
        qi = 0._r8
        th = tc/exnf(p)
    else 
        do iteration = 1, 10
           temps  = temps + ( (tc-temps)*cp/leff + qt - rvls )/( cp/leff + ep2*leff*rvls/r/temps/temps )
           status = qsat(temps,p,es(1),qs(1),gam(1),1)
           rvls   = qs(1)
        end do
        qc = max(qt - qs(1),0._r8)
        qv = qt - qc
        ql = qc*(1._r8 - nu)
        qi = nu*qc
        th = temps/exnf(p)
        if( abs((temps-(leff/cp)*qc)-tc) .ge. 1._r8 ) then
            id_check = 1
        else
            id_check = 0
        end if
    end if

    return
  end subroutine conden

  subroutine roots(a,b,c,r1,r2,status)
  
  
  
  
    real(r8), intent(in)  :: a
    real(r8), intent(in)  :: b
    real(r8), intent(in)  :: c
    real(r8), intent(out) :: r1
    real(r8), intent(out) :: r2
    integer , intent(out) :: status
    real(r8)              :: q

    status = 0

    if( a .eq. 0._r8 ) then                            
        if( b .eq. 0._r8 ) then                        
            status = 1
        else                                           
            r1 = -c/b
        endif
        r2 = r1
    else
        if( b .eq. 0._r8 ) then                        
            if( a*c .gt. 0._r8 ) then                  
                status = 2  
            else                                       
                r1 = sqrt(-c/a)
            endif
            r2 = -r1
       else                                            
            if( (b**2 - 4._r8*a*c) .lt. 0._r8 ) then   
                 status = 3
            else
                 q  = -0.5_r8*(b + sign(1.0_r8,b)*sqrt(b**2 - 4._r8*a*c))
                 r1 =  q/a
                 r2 =  c/q
            endif
       endif
    endif

    return
  end subroutine roots
  
  function slope(mkx,field,p0)
  
  
  
  
  
  
  
    real(r8)             :: slope(mkx)
    integer,  intent(in) :: mkx
    real(r8), intent(in) :: field(mkx)
    real(r8), intent(in) :: p0(mkx)
    
    real(r8)             :: below
    real(r8)             :: above
    integer              :: k

    below = ( field(2) - field(1) ) / ( p0(2) - p0(1) )
    do k = 2, mkx
       above = ( field(k) - field(k-1) ) / ( p0(k) - p0(k-1) )
       if( above .gt. 0._r8 ) then
           slope(k-1) = max(0._r8,min(above,below))
       else 
           slope(k-1) = min(0._r8,max(above,below))
       end if
       below = above
    end do
    slope(mkx) = slope(mkx-1)

    return
  end function slope

  function qsinvert(qt,thl,psfc,qsat)
  
  
  
  
  
  
    real(r8)          :: qsinvert    
    real(r8)             qt, thl, psfc
    real(r8)             ps, Pis, Ts, err, dlnqsdT, dTdPis
    real(r8)             dPisdps, dlnqsdps, derrdps, dps 
    real(r8)             Ti, rhi, TLCL, PiLCL, psmin, dpsmax
    integer              i
    integer, external :: qsat
    real(r8)          :: es(1)                     
    real(r8)          :: qs(1)                     
    real(r8)          :: gam(1)                    
    integer           :: status                    
    real(r8)          :: leff, nu

    psmin  = 100._r8*100._r8 
    dpsmax = 1._r8           

    
    
    

    Ti       =  thl*(psfc/p00)**rovcp
    status   =  qsat(Ti,psfc,es(1),qs(1),gam(1),1)
    rhi      =  qt/qs(1)      
    if( rhi .le. 0.01_r8 ) then
        write(iulog,*) 'Source air is too dry and pLCL is set to psmin in uwshcu.F90' 
        call wrf_message(iulog)
        qsinvert = psmin
        return
    end if
    TLCL     =  55._r8 + 1._r8/(1._r8/(Ti-55._r8)-log(rhi)/2840._r8); 
    PiLCL    =  TLCL/thl
    ps       =  p00*(PiLCL)**(1._r8/rovcp)

    do i = 1, 10
       Pis      =  (ps/p00)**rovcp
       Ts       =  thl*Pis
       status   =  qsat(Ts,ps,es(1),qs(1),gam(1),1)
       err      =  qt - qs(1)
       nu       =  max(min((268._r8 - Ts)/20._r8,1.0_r8),0.0_r8)        
       leff     =  (1._r8 - nu)*xlv + nu*xls                   
       dlnqsdT  =  gam(1)*(cp/leff)/qs(1)
       dTdPis   =  thl
       dPisdps  =  rovcp*Pis/ps 
       dlnqsdps = -1._r8/(ps - (1._r8 - ep2)*es(1))
       derrdps  = -qs(1)*(dlnqsdT * dTdPis * dPisdps + dlnqsdps)
       dps      = -err/derrdps
       ps       =  ps + dps
       if( ps .lt. 0._r8 ) then
           write(iulog,*) 'pLCL iteration is negative and set to psmin in uwshcu.F90', qt, thl, psfc 
           call wrf_message(iulog)
           qsinvert = psmin
           return    
       end if
       if( abs(dps) .le. dpsmax ) then
           qsinvert = ps
           return
       end if
    end do
    write(iulog,*) 'pLCL does not converge and is set to psmin in uwshcu.F90', qt, thl, psfc 
           call wrf_message(iulog)
    qsinvert = psmin
    return
  end function qsinvert

  real(r8) function compute_alpha(del_CIN,ke)
  
  
  
  
    real(r8) :: del_CIN, ke
    real(r8) :: x0, x1

    integer  :: iteration

    x0 = 0._r8
    do iteration = 1, 10
       x1 = x0 - (exp(-x0*ke*del_CIN) - x0)/(-ke*del_CIN*exp(-x0*ke*del_CIN) - 1._r8)
       x0 = x1
    end do
    compute_alpha = x0

    return

  end function compute_alpha

  real(r8) function compute_mumin2(mulcl,rmaxfrac,mulow)
  
  
  
  
    real(r8) :: mulcl, rmaxfrac, mulow
    real(r8) :: x0, x1, ex, ef, exf, f, fs
    integer  :: iteration

    x0 = mulow
    do iteration = 1, 10
       ex = exp(-x0**2)
       ef = erfc(x0)
       
       
       
       
       exf = ex/ef
       f  = 0.5_r8*exf**2 - 0.5_r8*(ex/2._r8/rmaxfrac)**2 - (mulcl*2.5066_r8/2._r8)**2
       fs = (2._r8*exf**2)*(exf/sqrt(3.141592_r8)-x0) + (0.5_r8*x0*ex**2)/(rmaxfrac**2)
       x1 = x0 - f/fs     
       x0 = x1
    end do
    compute_mumin2 = x0

 20 return

  end function compute_mumin2

  real(r8) function compute_ppen(wtwb,D,bogbot,bogtop,rho0j,dpen)
  
  
  
  
  
  
    real(r8) :: wtwb, D, bogbot, bogtop, rho0j, dpen
    real(r8) :: x0, x1, f, fs, SB, s00
    integer  :: iteration

    
      SB = ( bogtop - bogbot ) / dpen
    
    
      s00 = bogbot / rho0j - D * wtwb

    if( D*dpen .lt. 1.e-8 ) then
        if( s00 .ge. 0._r8 ) then
            x0 = dpen       
        else
            x0 = max(0._r8,min(dpen,-0.5_r8*wtwb/s00))
        endif
    else
        if( s00 .ge. 0._r8 ) then
            x0 = dpen
        else 
            x0 = 0._r8
        endif
        do iteration = 1, 5
           f  = exp(-2._r8*D*x0)*(wtwb-(bogbot-SB/(2._r8*D))/(D*rho0j)) + &
                                 (SB*x0+bogbot-SB/(2._r8*D))/(D*rho0j)
           fs = -2._r8*D*exp(-2._r8*D*x0)*(wtwb-(bogbot-SB/(2._r8*D))/(D*rho0j)) + &
                                 (SB)/(D*rho0j)
           if( fs .ge. 0._r8 ) then
		fs = max(fs, 1.e-10_r8)
           else
           	fs = min(fs,-1.e-10_r8)
           endif
           x1 = x0 - f/fs     
           x0 = x1
      end do

    endif    

    compute_ppen = -max(0._r8,min(dpen,x0))

  end function compute_ppen

  subroutine fluxbelowinv(cbmf,ps0,mkx,kinv,dt,xsrc,xmean,xtopin,xbotin,xflx)   
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
    integer,  intent(in)                     :: mkx, kinv 
    real(r8), intent(in)                     :: cbmf, dt, xsrc, xmean, xtopin, xbotin
    real(r8), intent(in),  dimension(0:mkx)  :: ps0
    real(r8), intent(out), dimension(0:mkx)  :: xflx  
    integer k
    real(r8) rcbmf, rpeff, dp, rr, pinv_eff, xtop, xbot, pinv, xtop_ori, xbot_ori

    xflx(0:mkx) = 0._r8
    dp = ps0(kinv-1) - ps0(kinv) 
    xbot = xbotin
    xtop = xtopin
    
    
    
    xtop_ori = xtop
    xbot_ori = xbot
    rcbmf = ( cbmf * g * dt ) / dp                  
    rpeff = ( xmean - xtop ) / max(1.e-13_r8, (xbot - xtop) ) 
    rpeff = abs(rpeff)
    rpeff = min( max(0._r8,rpeff), 1._r8 )          
    if( rpeff .eq. 0._r8 .or. rpeff .eq. 1._r8 ) then
        xbot = xmean
        xtop = xmean
    endif
    
    
    
    rr       = rpeff / rcbmf
    pinv     = ps0(kinv-1) - rpeff * dp             
    pinv_eff = ps0(kinv-1) + ( rcbmf - rpeff ) * dp 
    
    
    
    
    do k = 0, kinv - 1
       xflx(k) = cbmf * ( xsrc - xbot ) * ( ps0(0) - ps0(k) ) / ( ps0(0) - pinv )
    end do
    if( rr .le. 1._r8 ) then
        xflx(kinv-1) =  xflx(kinv-1) - ( 1._r8 - rr ) * cbmf * ( xtop_ori - xbot_ori )
    endif

    return
  end subroutine fluxbelowinv

  subroutine positive_moisture_single( xlv, xls, mkx, dt, qvmin, qlmin, qimin, dp, qv, ql, qi, s, qvten, qlten, qiten, sten )
  
  
  
  
  
  
  
  
  
  
    implicit none
    integer,  intent(in)     :: mkx
    real(r8), intent(in)     :: xlv, xls
    real(r8), intent(in)     :: dt, qvmin, qlmin, qimin
    real(r8), intent(in)     :: dp(mkx)
    real(r8), intent(inout)  :: qv(mkx), ql(mkx), qi(mkx), s(mkx)
    real(r8), intent(inout)  :: qvten(mkx), qlten(mkx), qiten(mkx), sten(mkx)
    integer   k
    real(r8)  dql, dqi, dqv, sum, aa, dum 

    do k = mkx, 1, -1        
       dql = max(0._r8,1._r8*qlmin-ql(k))
       dqi = max(0._r8,1._r8*qimin-qi(k))
       qlten(k) = qlten(k) +  dql/dt
       qiten(k) = qiten(k) +  dqi/dt
       qvten(k) = qvten(k) - (dql+dqi)/dt
       sten(k)  = sten(k)  + xlv * (dql/dt) + xls * (dqi/dt)
       ql(k)    = ql(k) +  dql
       qi(k)    = qi(k) +  dqi
       qv(k)    = qv(k) -  dql - dqi
       s(k)     = s(k)  +  xlv * dql + xls * dqi
       dqv      = max(0._r8,1._r8*qvmin-qv(k))
       qvten(k) = qvten(k) + dqv/dt
       qv(k)    = qv(k)   + dqv
       if( k .ne. 1 ) then 
           qv(k-1)    = qv(k-1)    - dqv*dp(k)/dp(k-1)
           qvten(k-1) = qvten(k-1) - dqv*dp(k)/dp(k-1)/dt
       endif
       qv(k) = max(qv(k),qvmin)
       ql(k) = max(ql(k),qlmin)
       qi(k) = max(qi(k),qimin)
    end do
    
    
    
    if( dqv .gt. 1.e-20_r8 ) then
        sum = 0._r8
        do k = 1, mkx
           if( qv(k) .gt. 2._r8*qvmin ) sum = sum + qv(k)*dp(k)
        enddo
        aa = dqv*dp(1)/max(1.e-20_r8,sum)
        if( aa .lt. 0.5_r8 ) then
            do k = 1, mkx
               if( qv(k) .gt. 2._r8*qvmin ) then
                   dum      = aa*qv(k)
                   qv(k)    = qv(k) - dum
                   qvten(k) = qvten(k) - dum/dt
               endif
            enddo 
        else 
            write(iulog,*) 'Full positive_moisture is impossible in uwshcu'
            call wrf_message(iulog)
        endif
    endif 

    return
  end subroutine positive_moisture_single

  subroutine findsp (lchnk, ncol, q, t, p, tsp, qsp)

  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  

  use wv_saturation, only: estblf, hlatv, tmin, hlatf, rgasv, pcf, &
                           cp, epsqs, ttrice

  
  
  
   integer, intent(in) :: lchnk                 
   integer, intent(in) :: ncol                  

   real(r8), intent(in) :: q(pcols,pver)        
   real(r8), intent(in) :: t(pcols,pver)        
   real(r8), intent(in) :: p(pcols,pver)        



   real(r8), intent(out) :: tsp(pcols,pver)      
   real(r8), intent(out) :: qsp(pcols,pver)      



   integer i                 
   integer k                 
   logical lflg              
   integer iter              
   integer l                 
   logical :: error_found

   real(r8) omeps                
   real(r8) trinv                
   real(r8) es                   
   real(r8) desdt                

   real(r8) dqsdt                
   real(r8) dgdt                 
   real(r8) g                    
   real(r8) weight(pcols)        
   real(r8) hlatsb               
   real(r8) hlatvp               
   real(r8) hltalt(pcols,pver)   
   real(r8) tterm                
   real(r8) qs                   
   real(r8) tc                   
   real(r8) tt0 


   real(r8) t1, q1, dt, dq
   real(r8) dtm, dqm
   real(r8) qvd, a1, tmp
   real(r8) rair
   real(r8) r1b, c1, c2, c3
   real(r8) denom
   real(r8) dttol
   real(r8) dqtol
   integer doit(pcols) 
   real(r8) enin(pcols), enout(pcols)
   real(r8) tlim(pcols)

   omeps = 1.0_r8 - epsqs
   trinv = 1.0_r8/ttrice
   a1 = 7.5_r8*log(10._r8)
   rair =  287.04_r8
   c3 = rair*a1/cp
   dtm = 0._r8    
   dqm = 0._r8    
   dttol = 1.e-4_r8 
   dqtol = 1.e-4_r8 
   tt0 = 273.15_r8  



   iter = 8

   do k = 1,pver




      do i = 1,ncol


         tlim(i) = min(max(t(i,k),173._r8),373._r8)
         es = estblf(tlim(i))
         denom = p(i,k) - omeps*es
         qs = epsqs*es/denom
         doit(i) = 0
         enout(i) = 1._r8

         if (p(i,k) > 5._r8*es .and. qs > 0._r8 .and. qs < 0.5_r8) then



             qs = min(epsqs*es/denom,1._r8)











             tc     = tlim(i) - tt0
             lflg   = (tc >= -ttrice .and. tc < 0.0_r8)
             weight(i) = min(-tc*trinv,1.0_r8)
             hlatsb = hlatv + weight(i)*hlatf
             hlatvp = hlatv - 2369.0_r8*tc
             if (tlim(i) < tt0) then
                hltalt(i,k) = hlatsb
             else
                hltalt(i,k) = hlatvp
             end if
             enin(i) = cp*tlim(i) + hltalt(i,k)*q(i,k)


             tmp =  q(i,k) - qs
             c1 = hltalt(i,k)*c3
             c2 = (tlim(i) + 36._r8)**2
             r1b    = c2/(c2 + c1*qs)
             qvd   = r1b*tmp
             tsp(i,k) = tlim(i) + ((hltalt(i,k)/cp)*qvd)
             es = estblf(tsp(i,k))
             qsp(i,k) = min(epsqs*es/(p(i,k) - omeps*es),1._r8)
          else
             doit(i) = 1
             tsp(i,k) = tlim(i)
             qsp(i,k) = q(i,k)
             enin(i) = 1._r8
          endif
       end do   



      do l = 1, iter
         dtm = 0
         dqm = 0
         do i = 1,ncol
            if (doit(i) == 0) then
               es = estblf(tsp(i,k))



               qs = min(epsqs*es/(p(i,k) - omeps*es),1._r8)











               tc     = tsp(i,k) - tt0
               lflg   = (tc >= -ttrice .and. tc < 0.0_r8)
               weight(i) = min(-tc*trinv,1.0_r8)
               hlatsb = hlatv + weight(i)*hlatf
               hlatvp = hlatv - 2369.0_r8*tc
               if (tsp(i,k) < tt0) then
                  hltalt(i,k) = hlatsb
               else
                  hltalt(i,k) = hlatvp
               end if
               if (lflg) then
                  tterm = pcf(1) + tc*(pcf(2) + tc*(pcf(3)+tc*(pcf(4) + tc*pcf(5))))
               else
                  tterm = 0.0_r8
               end if
               desdt = hltalt(i,k)*es/(rgasv*tsp(i,k)*tsp(i,k)) + tterm*trinv
               dqsdt = (epsqs + omeps*qs)/(p(i,k) - omeps*es)*desdt

               g = enin(i) - (cp*tsp(i,k) + hltalt(i,k)*qsp(i,k))
               dgdt = -(cp + hltalt(i,k)*dqsdt)
               t1 = tsp(i,k) - g/dgdt
               dt = abs(t1 - tsp(i,k))/t1
               tsp(i,k) = max(t1,tmin)
               es = estblf(tsp(i,k))
               q1 = min(epsqs*es/(p(i,k) - omeps*es),1._r8)
               dq = abs(q1 - qsp(i,k))/max(q1,1.e-12_r8)
               qsp(i,k) = q1
               dtm = max(dtm,dt)
               dqm = max(dqm,dq)

               if (dt < dttol .and. dq < dqtol) then
                  doit(i) = 2
               endif
               enout(i) = cp*tsp(i,k) + hltalt(i,k)*qsp(i,k)

               if (tsp(i,k) < 174.16_r8) then
                  doit(i) = 4
               endif
            else
            endif
         end do              

         if (dtm < dttol .and. dqm < dqtol) then
            go to 10
         endif

      end do                 
10    continue

      error_found = .false.
      if (dtm > dttol .or. dqm > dqtol) then
         do i = 1,ncol
            if (doit(i) == 0) error_found = .true.
         end do
         if (error_found) then
            do i = 1,ncol
               if (doit(i) == 0) then
                  write(iulog,*) ' findsp not converging at point i, k ', i, k
                  call wrf_message(iulog)
                  write(iulog,*) ' t, q, p, enin ', t(i,k), q(i,k), p(i,k), enin(i)
                  call wrf_message(iulog)
                  write(iulog,*) ' tsp, qsp, enout ', tsp(i,k), qsp(i,k), enout(i)
                  call wrf_message(iulog)
                  call endrun ('FINDSP')
               endif
            end do
         endif
      endif
      do i = 1,ncol
         if (doit(i) == 2 .and. abs((enin(i)-enout(i))/(enin(i)+enout(i))) > 1.e-4_r8) then
            error_found = .true.
         endif
      end do
      if (error_found) then
         do i = 1,ncol
            if (doit(i) == 2 .and. abs((enin(i)-enout(i))/(enin(i)+enout(i))) > 1.e-4_r8) then
               write(iulog,*) ' the enthalpy is not conserved for point ', &
                  i, k, enin(i), enout(i)
               call wrf_message(iulog)
               write(iulog,*) ' t, q, p, enin ', t(i,k), q(i,k), p(i,k), enin(i)
               call wrf_message(iulog)
               write(iulog,*) ' tsp, qsp, enout ', tsp(i,k), qsp(i,k), enout(i)
               call wrf_message(iulog)
               call endrun ('FINDSP')
            endif
         end do
      endif
      
   end do                    

   return
   end subroutine findsp

  
  
  
  
  

  end module uwshcu

