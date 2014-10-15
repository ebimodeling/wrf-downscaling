



  module eddy_diff

  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  

  use diffusion_solver, only : vdiff_selector





  use module_cam_support,   only: iulog, pver,outfld, addfld, phys_decomp  


  implicit none
  private
  save

  public init_eddy_diff
  public compute_eddy_diff

  type(vdiff_selector)        :: fieldlist_wet                  
  type(vdiff_selector)        :: fieldlist_dry                  
  integer,          parameter :: r8 = selected_real_kind(12)    

  
  
  

  character,        parameter :: sftype         = 'l'           

  character(len=4), parameter :: choice_evhc    = 'maxi'        
  character(len=6), parameter :: choice_radf    = 'maxi'        
  character(len=6), parameter :: choice_SRCL    = 'nonamb'      
 
  character(len=6), parameter :: choice_tunl    = 'rampcl'      
  real(r8),         parameter :: ctunl          =  2._r8        
  character(len=6), parameter :: choice_leng    = 'origin'      
  real(r8),         parameter :: cleng          =  3._r8        
  character(len=6), parameter :: choice_tkes    = 'ibprod'      

  
  

  logical,          parameter :: id_sedfact     = .false.
  real(r8),         parameter :: ased           =  9._r8        

  
  
  
  
  

  real(r8),         parameter :: a1l            =   0.10_r8     
                                                                

  real(r8),         parameter :: a1i            =   0.2_r8      
  real(r8),         parameter :: ccrit          =   0.5_r8      
  real(r8),         parameter :: wstar3factcrit =   0.5_r8      

  real(r8),         parameter :: a2l            =   30._r8      
  real(r8),         parameter :: a3l            =   0.8_r8      

  real(r8),         parameter :: jbumin         =   .001_r8     
  real(r8),         parameter :: evhcmax        =   10._r8      

  real(r8),         parameter :: ustar_min      =   0.01_r8     
  real(r8),         parameter :: onet           =   1._r8/3._r8 




  integer                     :: ncvmax  

  real(r8),         parameter :: qmin           =   1.e-5_r8    
  real(r8),         parameter :: ntzero         =   1.e-12_r8   
  real(r8),         parameter :: b1             =   5.8_r8      
  real(r8)                    :: b123                           
  real(r8),         parameter :: tunl           =   0.085_r8    
  real(r8),         parameter :: alph1          =   0.5562_r8   
  real(r8),         parameter :: alph2          =  -4.3640_r8   
  real(r8),         parameter :: alph3          = -34.6764_r8   
  real(r8),         parameter :: alph4          =  -6.1272_r8   
  real(r8),         parameter :: alph5          =   0.6986_r8   
  real(r8),         parameter :: ricrit         =   0.19_r8     
  real(r8),         parameter :: ae             =   1._r8       
  real(r8),         parameter :: rinc           =  -0.04_r8     
  real(r8),         parameter :: wpertmin       =   1.e-6_r8    
  real(r8),         parameter :: wfac           =   1._r8       
  real(r8),         parameter :: tfac           =   1._r8       
  real(r8),         parameter :: fak            =   8.5_r8      
  real(r8),         parameter :: rcapmin        =   0.1_r8      
  real(r8),         parameter :: rcapmax        =   2.0_r8      
  real(r8),         parameter :: tkemax         =  20._r8       
  real(r8),         parameter :: lambda         =   0.5_r8      

  logical,          parameter :: use_kvf        =  .false.      
  logical,          parameter :: use_dw_surf    =  .true.       ! Used in 'zisocl'. Default is 'true'
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                

  logical,          parameter :: set_qrlzero    =  .false.      

  
  
  

  real(r8),         parameter :: pblmaxp        =  4.e4_r8      
  real(r8),         parameter :: zkmin          =  0.01_r8      
  real(r8),         parameter :: betam          = 15.0_r8       
  real(r8),         parameter :: betas          =  5.0_r8       
  real(r8),         parameter :: betah          = 15.0_r8       
  real(r8),         parameter :: fakn           =  7.2_r8       
  real(r8),         parameter :: ricr           =  0.3_r8       
  real(r8),         parameter :: sffrac         =  0.1_r8       
  real(r8),         parameter :: binm           =  betam*sffrac 
  real(r8),         parameter :: binh           =  betah*sffrac 

  
  
  

  real(r8)                    :: cpair                          
  real(r8)                    :: rair                           
  real(r8)                    :: zvir                           
  real(r8)                    :: latvap                         
  real(r8)                    :: latice                         
  real(r8)                    :: latsub                         
  real(r8)                    :: g                              
  real(r8)                    :: vk                             
  real(r8)                    :: ccon                           

  integer                     :: ntop_turb                      
  integer                     :: nbot_turb                      

  real(r8), allocatable       :: ml2(:)                         

  CONTAINS

  
  
  
  
  subroutine init_eddy_diff( kind, pver, gravx, cpairx, rairx, zvirx, & 
                             latvapx, laticex, ntop_eddy, nbot_eddy, hypm, vkx )
    
    
    
    
    use diffusion_solver, only: init_vdiff, vdiff_select



    use module_cam_support,   only: outfld, addfld, phys_decomp  

    implicit none
    
    
    
    integer,  intent(in) :: kind       
    integer,  intent(in) :: pver       
    integer,  intent(in) :: ntop_eddy  
    integer,  intent(in) :: nbot_eddy  
    real(r8), intent(in) :: gravx      
    real(r8), intent(in) :: cpairx     
    real(r8), intent(in) :: rairx      
    real(r8), intent(in) :: zvirx      
    real(r8), intent(in) :: latvapx    
    real(r8), intent(in) :: laticex    
    real(r8), intent(in) :: hypm(pver) 
    real(r8), intent(in) :: vkx        

    character(128)       :: errstring  
    integer              :: k          

    
    ncvmax = pver        

    if( kind .ne. r8 ) then
        write(iulog,*) 'wrong KIND of reals passed to init_diffusvity -- exiting.'

        call wrf_message(iulog)

        stop 'init_eddy_diff'
    endif

    
    
    

    cpair     = cpairx
    rair      = rairx
    g         = gravx
    zvir      = zvirx
    latvap    = latvapx
    latice    = laticex
    latsub    = latvap + latice
    vk        = vkx
    ccon      = fak*sffrac*vk
    ntop_turb = ntop_eddy
    nbot_turb = nbot_eddy
    b123      = b1**(2._r8/3._r8)

    
    
    
    

    call init_vdiff(r8, 1, rair, g, fieldlist_wet, fieldlist_dry, errstring)

    

    if(vdiff_select(fieldlist_wet,'s').ne.'')   write(iulog,*) 'error: ', vdiff_select(fieldlist_wet,'s')
        call wrf_message(iulog)
    if(vdiff_select(fieldlist_wet,'q',1).ne.'') write(iulog,*) 'error: ', vdiff_select(fieldlist_wet,'q',1)
        call wrf_message(iulog)
    if(vdiff_select(fieldlist_wet,'u').ne.'')   write(iulog,*) 'error: ', vdiff_select(fieldlist_wet,'u')
        call wrf_message(iulog)
    if(vdiff_select(fieldlist_wet,'v').ne.'')   write(iulog,*) 'error: ', vdiff_select(fieldlist_wet,'v')
        call wrf_message(iulog)

    
    
    

    call addfld('UW_errorPBL',      'm2/s',    1,      'A',  'Error function of UW PBL',                              phys_decomp )
    call addfld('UW_n2',            's-2',     pver,   'A',  'Buoyancy Frequency, LI',                                phys_decomp )
    call addfld('UW_s2',            's-2',     pver,   'A',  'Shear Frequency, LI',                                   phys_decomp )
    call addfld('UW_ri',            'no',      pver,   'A',  'Interface Richardson Number, I',                        phys_decomp )
    call addfld('UW_sfuh',          'no',      pver,   'A',  'Upper-Half Saturation Fraction, L',                     phys_decomp )
    call addfld('UW_sflh',          'no',      pver,   'A',  'Lower-Half Saturation Fraction, L',                     phys_decomp )
    call addfld('UW_sfi',           'no',      pver+1, 'A',  'Interface Saturation Fraction, I',                      phys_decomp )
    call addfld('UW_cldn',          'no',      pver,   'A',  'Cloud Fraction, L',                                     phys_decomp )
    call addfld('UW_qrl',           'g*W/m2',  pver,   'A',  'LW cooling rate, L',                                    phys_decomp )
    call addfld('UW_ql',            'kg/kg',   pver,   'A',  'ql(LWC), L',                                            phys_decomp )
    call addfld('UW_chu',           'g*kg/J',  pver+1, 'A',  'Buoyancy Coefficient, chu, I',                          phys_decomp )
    call addfld('UW_chs',           'g*kg/J',  pver+1, 'A',  'Buoyancy Coefficient, chs, I',                          phys_decomp )
    call addfld('UW_cmu',           'g/kg/kg', pver+1, 'A',  'Buoyancy Coefficient, cmu, I',                          phys_decomp )
    call addfld('UW_cms',           'g/kg/kg', pver+1, 'A',  'Buoyancy Coefficient, cms, I',                          phys_decomp )    
    call addfld('UW_tke',           'm2/s2',   pver+1, 'A',  'TKE, I',                                                phys_decomp )
    call addfld('UW_wcap',          'm2/s2',   pver+1, 'A',  'Wcap, I',                                               phys_decomp )        
    call addfld('UW_bprod',         'm2/s3',   pver+1, 'A',  'Buoyancy production, I',                                phys_decomp )
    call addfld('UW_sprod',         'm2/s3',   pver+1, 'A',  'Shear production, I',                                   phys_decomp )    
    call addfld('UW_kvh',           'm2/s',    pver+1, 'A',  'Eddy diffusivity of heat, I',                           phys_decomp )
    call addfld('UW_kvm',           'm2/s',    pver+1, 'A',  'Eddy diffusivity of uv, I',                             phys_decomp )
    call addfld('UW_pblh',          'm',       1,      'A',  'PBLH, 1',                                               phys_decomp )
    call addfld('UW_pblhp',         'Pa',      1,      'A',  'PBLH pressure, 1',                                      phys_decomp )
    call addfld('UW_tpert',         'K',       1,      'A',  'Convective T excess, 1',                                phys_decomp )
    call addfld('UW_qpert',         'kg/kg',   1,      'A',  'Convective qt excess, I',                               phys_decomp )
    call addfld('UW_wpert',         'm/s',     1,      'A',  'Convective W excess, I',                                phys_decomp )
    call addfld('UW_ustar',         'm/s',     1,      'A',  'Surface Frictional Velocity, 1',                        phys_decomp )
    call addfld('UW_tkes',          'm2/s2',   1,      'A',  'Surface TKE, 1',                                        phys_decomp )
    call addfld('UW_minpblh',       'm',       1,      'A',  'Minimum PBLH, 1',                                       phys_decomp )
    call addfld('UW_turbtype',      'no',      pver+1, 'A',  'Interface Turbulence Type, I',                          phys_decomp )    
    call addfld('UW_kbase_o',       'no',      ncvmax, 'A',  'Initial CL Base Exterbal Interface Index, CL',          phys_decomp )
    call addfld('UW_ktop_o',        'no',      ncvmax, 'A',  'Initial Top Exterbal Interface Index, CL',              phys_decomp )
    call addfld('UW_ncvfin_o',      '#',       1,      'A',  'Initial Total Number of CL regimes, CL',                phys_decomp )
    call addfld('UW_kbase_mg',      'no',      ncvmax, 'A',  'kbase after merging, CL',                               phys_decomp )
    call addfld('UW_ktop_mg',       'no',      ncvmax, 'A',  'ktop after merging, CL',                                phys_decomp )
    call addfld('UW_ncvfin_mg',     '#',       1,      'A',  'ncvfin after merging, CL',                              phys_decomp )
    call addfld('UW_kbase_f',       'no',      ncvmax, 'A',  'Final kbase with SRCL, CL',                             phys_decomp )
    call addfld('UW_ktop_f',        'no',      ncvmax, 'A',  'Final ktop with SRCL, CL',                              phys_decomp )
    call addfld('UW_ncvfin_f',      '#',       1,      'A',  'Final ncvfin with SRCL, CL',                            phys_decomp )
    call addfld('UW_wet',           'm/s',     ncvmax, 'A',  'Entrainment rate at CL top, CL',                        phys_decomp )
    call addfld('UW_web',           'm/s',     ncvmax, 'A',  'Entrainment rate at CL base, CL',                       phys_decomp )
    call addfld('UW_jtbu',          'm/s2',    ncvmax, 'A',  'Buoyancy jump across CL top, CL',                       phys_decomp )
    call addfld('UW_jbbu',          'm/s2',    ncvmax, 'A',  'Buoyancy jump across CL base, CL',                      phys_decomp )
    call addfld('UW_evhc',          'no',      ncvmax, 'A',  'Evaporative enhancement factor, CL',                    phys_decomp )
    call addfld('UW_jt2slv',        'J/kg',    ncvmax, 'A',  'slv jump for evhc, CL',                                 phys_decomp )
    call addfld('UW_n2ht',          's-2',     ncvmax, 'A',  'n2 at just below CL top interface, CL',                 phys_decomp )
    call addfld('UW_n2hb',          's-2',     ncvmax, 'A',  'n2 at just above CL base interface',                    phys_decomp )
    call addfld('UW_lwp',           'kg/m2',   ncvmax, 'A',  'LWP in the CL top layer, CL',                           phys_decomp )
    call addfld('UW_optdepth',      'no',      ncvmax, 'A',  'Optical depth of the CL top layer, CL',                 phys_decomp )
    call addfld('UW_radfrac',       'no',      ncvmax, 'A',  'Fraction of radiative cooling confined in the CL top',  phys_decomp )
    call addfld('UW_radf',          'm2/s3',   ncvmax, 'A',  'Buoyancy production at the CL top by radf, I',          phys_decomp )        
    call addfld('UW_wstar',         'm/s',     ncvmax, 'A',  'Convective velocity, Wstar, CL',                        phys_decomp )
    call addfld('UW_wstar3fact',    'no',      ncvmax, 'A',  'Enhancement of wstar3 due to entrainment, CL',          phys_decomp )
    call addfld('UW_ebrk',          'm2/s2',   ncvmax, 'A',  'CL-averaged TKE, CL',                                   phys_decomp )
    call addfld('UW_wbrk',          'm2/s2',   ncvmax, 'A',  'CL-averaged W, CL',                                     phys_decomp )
    call addfld('UW_lbrk',          'm',       ncvmax, 'A',  'CL internal thickness, CL',                             phys_decomp )
    call addfld('UW_ricl',          'no',      ncvmax, 'A',  'CL-averaged Ri, CL',                                    phys_decomp )
    call addfld('UW_ghcl',          'no',      ncvmax, 'A',  'CL-averaged gh, CL',                                    phys_decomp )
    call addfld('UW_shcl',          'no',      ncvmax, 'A',  'CL-averaged sh, CL',                                    phys_decomp )
    call addfld('UW_smcl',          'no',      ncvmax, 'A',  'CL-averaged sm, CL',                                    phys_decomp )
    call addfld('UW_gh',            'no',      pver+1, 'A',  'gh at all interfaces, I',                               phys_decomp )
    call addfld('UW_sh',            'no',      pver+1, 'A',  'sh at all interfaces, I',                               phys_decomp )
    call addfld('UW_sm',            'no',      pver+1, 'A',  'sm at all interfaces, I',                               phys_decomp )
    call addfld('UW_ria',           'no',      pver+1, 'A',  'ri at all interfaces, I',                               phys_decomp )
    call addfld('UW_leng',          'm/s',     pver+1, 'A',  'Turbulence length scale, I',                            phys_decomp )

  return

  end subroutine init_eddy_diff

  
  
  
  
  subroutine compute_eddy_diff( lchnk  ,                                                            &
                                pcols  , pver   , ncol     , t       , qv       , ztodt   ,         &
                                ql     , qi     , s        , rpdel   , cldn     , qrl     , wsedl , &
                                z      , zi     , pmid     , pi      , u        , v       ,         &
                                taux   , tauy   , shflx    , qflx    , wstarent , nturb   ,         &
                                ustar  , pblh   , kvm_in   , kvh_in  , kvm_out  , kvh_out , kvq   , & 
                                cgh    , cgs    , tpert    , qpert   , wpert    , tke     , bprod , &
                                sprod  , sfi    , qsat     , kvinit  ,                              &
                                tauresx, tauresy, ksrftms  ,                                        &
                                ipbl   , kpblh  , wstarPBL , turbtype, sm_aw )
       
    
    
    
    
    
    
    
    use diffusion_solver, only: compute_vdiff
    use module_cam_support,   only: outfld, addfld, phys_decomp 

    implicit none

  

    
    
    

    integer,  intent(in)    :: lchnk   
    integer,  intent(in)    :: pcols                     
    integer,  intent(in)    :: pver                      
    integer,  intent(in)    :: ncol                      
    integer,  intent(in)    :: nturb                     
    logical,  intent(in)    :: wstarent                  
    logical,  intent(in)    :: kvinit                    
    real(r8), intent(in)    :: ztodt                     
    real(r8), intent(in)    :: t(pcols,pver)             
    real(r8), intent(in)    :: qv(pcols,pver)            
    real(r8), intent(in)    :: ql(pcols,pver)            
    real(r8), intent(in)    :: qi(pcols,pver)            
    real(r8), intent(in)    :: s(pcols,pver)             
    real(r8), intent(in)    :: rpdel(pcols,pver)         
    real(r8), intent(in)    :: cldn(pcols,pver)          
    real(r8), intent(in)    :: qrl(pcols,pver)           
    real(r8), intent(in)    :: wsedl(pcols,pver)         
    real(r8), intent(in)    :: z(pcols,pver)             
    real(r8), intent(in)    :: zi(pcols,pver+1)          
    real(r8), intent(in)    :: pmid(pcols,pver)          
    real(r8), intent(in)    :: pi(pcols,pver+1)          
    real(r8), intent(in)    :: u(pcols,pver)             
    real(r8), intent(in)    :: v(pcols,pver)             
    real(r8), intent(in)    :: taux(pcols)               
    real(r8), intent(in)    :: tauy(pcols)               
    real(r8), intent(in)    :: shflx(pcols)              
    real(r8), intent(in)    :: qflx(pcols)               
    real(r8), intent(in)    :: kvm_in(pcols,pver+1)      
    real(r8), intent(in)    :: kvh_in(pcols,pver+1)      
    real(r8), intent(in)    :: ksrftms(pcols)            

    
    
    

    real(r8), intent(out)   :: kvm_out(pcols,pver+1)     
    real(r8), intent(out)   :: kvh_out(pcols,pver+1)     
    real(r8), intent(out)   :: kvq(pcols,pver+1)         
    real(r8), intent(out)   :: ustar(pcols)              
    real(r8), intent(out)   :: pblh(pcols)               
    real(r8), intent(out)   :: cgh(pcols,pver+1)         
    real(r8), intent(out)   :: cgs(pcols,pver+1)         
    real(r8), intent(out)   :: tpert(pcols)              
    real(r8), intent(out)   :: qpert(pcols)              
    real(r8), intent(out)   :: wpert(pcols)              
    real(r8), intent(out)   :: tke(pcols,pver+1)         
    real(r8), intent(out)   :: bprod(pcols,pver+1)       
    real(r8), intent(out)   :: sprod(pcols,pver+1)       
    real(r8), intent(out)   :: sfi(pcols,pver+1)         
    real(r8), intent(out)   :: turbtype(pcols,pver+1)    
    real(r8), intent(out)   :: sm_aw(pcols,pver+1)       
                                                         
    real(r8), intent(out)   :: ipbl(pcols)               
    real(r8), intent(out)   :: kpblh(pcols)              
    real(r8), intent(out)   :: wstarPBL(pcols)           

    
    
    

    real(r8), intent(inout) :: tauresx(pcols)            
    real(r8), intent(inout) :: tauresy(pcols)            

    
    
    

    integer                    icol
    integer                    i, k, iturb, status
    integer,  external      :: qsat
    character(128)          :: errstring                 

    real(r8)                :: tautotx(pcols)            
    real(r8)                :: tautoty(pcols)            
    real(r8)                :: kvf(pcols,pver+1)         
    real(r8)                :: kvm(pcols,pver+1)         
    real(r8)                :: kvh(pcols,pver+1)         
    real(r8)                :: kvm_preo(pcols,pver+1)    
    real(r8)                :: kvh_preo(pcols,pver+1)    
    real(r8)                :: kvm_pre(pcols,pver+1)     
    real(r8)                :: kvh_pre(pcols,pver+1)     
    real(r8)                :: errorPBL(pcols)           
    real(r8)                :: s2(pcols,pver)            
    real(r8)                :: n2(pcols,pver)            
    real(r8)                :: ri(pcols,pver)            
    real(r8)                :: pblhp(pcols)              
    real(r8)                :: minpblh(pcols)            

    real(r8)                :: qt(pcols,pver)            
    real(r8)                :: sfuh(pcols,pver)          
    real(r8)                :: sflh(pcols,pver)          
    real(r8)                :: sl(pcols,pver)            
    real(r8)                :: slv(pcols,pver)           
    real(r8)                :: slslope(pcols,pver)       
    real(r8)                :: qtslope(pcols,pver)       
    real(r8)                :: rrho(pcols)               
    real(r8)                :: qvfd(pcols,pver)          
    real(r8)                :: tfd(pcols,pver)           
    real(r8)                :: slfd(pcols,pver)          
    real(r8)                :: qtfd(pcols,pver)          
    real(r8)                :: qlfd(pcols,pver)          
    real(r8)                :: ufd(pcols,pver)           
    real(r8)                :: vfd(pcols,pver)           

    

    real(r8)                :: chu(pcols,pver+1)         
    real(r8)                :: chs(pcols,pver+1)         
    real(r8)                :: cmu(pcols,pver+1)         
    real(r8)                :: cms(pcols,pver+1)         

    real(r8)                :: jnk1d(pcols)
    real(r8)                :: jnk2d(pcols,pver+1)  
    real(r8)                :: zero(pcols)
    real(r8)                :: zero2d(pcols,pver+1)
    real(r8)                :: es(1)                     
    real(r8)                :: qs(1)                     
    real(r8)                :: gam(1)                    
    real(r8)                :: ep2, templ, temps

    
    
    

    real(r8)                :: tkes(pcols)               
    real(r8)                :: kbase_o(pcols,ncvmax)     
    real(r8)                :: ktop_o(pcols,ncvmax)      
    real(r8)                :: ncvfin_o(pcols)           
    real(r8)                :: kbase_mg(pcols,ncvmax)    ! 'kbase' after extending-merging from 'zisocl'
    real(r8)                :: ktop_mg(pcols,ncvmax)     ! 'ktop' after extending-merging from 'zisocl'
    real(r8)                :: ncvfin_mg(pcols)          ! 'ncvfin' after extending-merging from 'zisocl'
    real(r8)                :: kbase_f(pcols,ncvmax)     
    real(r8)                :: ktop_f(pcols,ncvmax)      
    real(r8)                :: ncvfin_f(pcols)           
    real(r8)                :: wet(pcols,ncvmax)         
    real(r8)                :: web(pcols,ncvmax)         
    real(r8)                :: jtbu(pcols,ncvmax)        
    real(r8)                :: jbbu(pcols,ncvmax)        
    real(r8)                :: evhc(pcols,ncvmax)        
    real(r8)                :: jt2slv(pcols,ncvmax)      
    real(r8)                :: n2ht(pcols,ncvmax)        
    real(r8)                :: n2hb(pcols,ncvmax)        
    real(r8)                :: lwp(pcols,ncvmax)         
    real(r8)                :: opt_depth(pcols,ncvmax)   
    real(r8)                :: radinvfrac(pcols,ncvmax)  
    real(r8)                :: radf(pcols,ncvmax)        
    real(r8)                :: wstar(pcols,ncvmax)       
    real(r8)                :: wstar3fact(pcols,ncvmax)  
    real(r8)                :: ebrk(pcols,ncvmax)        
    real(r8)                :: wbrk(pcols,ncvmax)        
    real(r8)                :: lbrk(pcols,ncvmax)        
    real(r8)                :: ricl(pcols,ncvmax)        
    real(r8)                :: ghcl(pcols,ncvmax)        
    real(r8)                :: shcl(pcols,ncvmax)        
    real(r8)                :: smcl(pcols,ncvmax)        
    real(r8)                :: ghi(pcols,pver+1)         
    real(r8)                :: shi(pcols,pver+1)         
    real(r8)                :: smi(pcols,pver+1)         
    real(r8)                :: rii(pcols,pver+1)         
    real(r8)                :: lengi(pcols,pver+1)       
    real(r8)                :: wcap(pcols,pver+1)        

    
    
    

    zero(:)     = 0._r8
    zero2d(:,:) = 0._r8

    
    
    

    ufd(:ncol,:)  = u(:ncol,:)
    vfd(:ncol,:)  = v(:ncol,:)
    tfd(:ncol,:)  = t(:ncol,:)
    qvfd(:ncol,:) = qv(:ncol,:)
    qlfd(:ncol,:) = ql(:ncol,:)
    
    do iturb = 1, nturb

     
     
     
     
     

       tautotx(:ncol) = taux(:ncol) - ksrftms(:ncol) * ufd(:ncol,pver)
       tautoty(:ncol) = tauy(:ncol) - ksrftms(:ncol) * vfd(:ncol,pver)

     

       call trbintd( &
                     pcols    , pver    , ncol  , z       , ufd     , vfd     , tfd   , pmid    , &
                     tautotx  , tautoty , ustar , rrho    , s2      , n2      , ri    , zi      , &
                     pi       , cldn    , qtfd  , qvfd    , qlfd    , qi      , sfi   , sfuh    , &
                     sflh     , slfd    , slv   , slslope , qtslope , chs     , chu   , cms     , &
                     cmu      , minpblh , qsat )

     
     

       if( iturb .eq. 1 ) then
           qt(:ncol,:) = qtfd(:ncol,:)
           sl(:ncol,:) = slfd(:ncol,:)
       endif

     
       if(use_kvf)call austausch_atm( pcols, pver, ncol, ri, s2, kvf )

     
     

       if( iturb .eq. 1 ) then
           if( kvinit ) then
           
             if( use_kvf ) then
                 kvh(:ncol,:) = kvf(:ncol,:)
                 kvm(:ncol,:) = kvf(:ncol,:)
             else
                 kvh(:ncol,:) = 0._r8
                 kvm(:ncol,:) = 0._r8
             endif
           else
             
             kvh(:ncol,:) = kvh_in(:ncol,:)
             kvm(:ncol,:) = kvm_in(:ncol,:)
           endif
       else
        
          kvh(:ncol,:) = kvh_out(:ncol,:)
          kvm(:ncol,:) = kvm_out(:ncol,:)
       endif

     
     
     
     

       call caleddy( pcols     , pver      , ncol      ,                     &
                     slfd      , qtfd      , qlfd      , slv      ,ufd     , &
                     vfd       , pi        , z         , zi       ,          &
                     qflx      , shflx     , slslope   , qtslope  ,          &
                     chu       , chs       , cmu       , cms      ,sfuh    , &
                     sflh      , n2        , s2        , ri       ,rrho    , &
                     pblh      , ustar     ,                                 &
                     kvh       , kvm       , kvh_out   , kvm_out  ,          &
                     tpert     , qpert     , qrl       , kvf      , tke    , &
                     wstarent  , bprod     , sprod     , minpblh  , wpert  , &
                     tkes      , turbtype  , sm_aw     ,                     & 
                     kbase_o   , ktop_o    , ncvfin_o  ,                     &
                     kbase_mg  , ktop_mg   , ncvfin_mg ,                     &                  
                     kbase_f   , ktop_f    , ncvfin_f  ,                     &                  
                     wet       , web       , jtbu      , jbbu     ,          &
                     evhc      , jt2slv    , n2ht      , n2hb     ,          & 
                     lwp       , opt_depth , radinvfrac, radf     ,          &
                     wstar     , wstar3fact,                                 &
                     ebrk      , wbrk      , lbrk      , ricl     , ghcl   , & 
                     shcl      , smcl      , ghi       , shi      , smi    , &
                     rii       , lengi     , wcap      , pblhp    , cldn   , &
                     ipbl      , kpblh     , wsedl )

     

       if( iturb .eq. nturb ) then
           do i = 1, ncol
              errorPBL(i) = 0._r8 
              do k = 1, pver
                 errorPBL(i) = errorPBL(i) + ( kvh(i,k) - kvh_out(i,k) )**2 
              end do 
              errorPBL(i) = sqrt(errorPBL(i)/pver)
           end do
       end if

     
     

       if( iturb .gt. 1 .and. iturb .lt. nturb ) then
           kvm_out(:ncol,:) = lambda * kvm_out(:ncol,:) + ( 1._r8 - lambda ) * kvm(:ncol,:)
           kvh_out(:ncol,:) = lambda * kvh_out(:ncol,:) + ( 1._r8 - lambda ) * kvh(:ncol,:)
       endif

     

       cgh(:ncol,:) = 0._r8
       cgs(:ncol,:) = 0._r8      

       if( iturb .lt. nturb ) then

         

           slfd(:ncol,:)  = sl(:ncol,:)
           qtfd(:ncol,:)  = qt(:ncol,:)
           ufd(:ncol,:)   = u(:ncol,:)
           vfd(:ncol,:)   = v(:ncol,:)

         
         

           call compute_vdiff( lchnk   ,                                                  &
                               pcols   , pver     , 1        , ncol         , pmid      , &
                               pi      , rpdel    , t        , ztodt        , taux      , &
                               tauy    , shflx    , qflx     , ntop_turb    , nbot_turb , &
                               kvh_out , kvm_out  , kvh_out  , cgs          , cgh       , &
                               zi      , ksrftms  , zero     , fieldlist_wet,             &
                               ufd     , vfd      , qtfd     , slfd         ,             &
                               jnk1d   , jnk1d    , jnk2d    , jnk1d        , errstring , &
                               tauresx , tauresy  , 0        , .false. )

         
         
          
          do k = 1, pver
             do i = 1, ncol
              
              
              
              
              
              
              
                templ     = ( slfd(i,k) - g*z(i,k) ) / cpair
                status    =   qsat( templ, pmid(i,k), es(1), qs(1), gam(1), 1 )
                ep2       =  .622_r8 
                temps     =   templ + ( qtfd(i,k) - qs(1) ) / ( cpair / latvap + latvap * qs(1) / ( rair * templ**2 ) )
                status    =   qsat( temps, pmid(i,k), es(1), qs(1), gam(1), 1 )
                qlfd(i,k) =   max( qtfd(i,k) - qi(i,k) - qs(1) ,0._r8 )
              
              
              
              
              
              
                qvfd(i,k) = max( 0._r8, qtfd(i,k) - qi(i,k) - qlfd(i,k) )
                tfd(i,k)  = ( slfd(i,k) + latvap * qlfd(i,k) + latsub * qi(i,k) - g*z(i,k)) / cpair
             end do
          end do
       endif

     
     
     
     
     
     
     
     
     
     
     

    end do  

    kvq(:ncol,:) = kvh_out(:ncol,:)

  

    do i = 1, ncol
       if( ipbl(i) .eq. 1._r8 ) then 
           wstarPBL(i) = max( 0._r8, wstar(i,1) )
       else
           wstarPBL(i) = 0._r8
       endif
    end do

    
    
    

    call outfld( 'UW_errorPBL',    errorPBL,   pcols,   lchnk )

    call outfld( 'UW_n2',          n2,         pcols,   lchnk )
    call outfld( 'UW_s2',          s2,         pcols,   lchnk )
    call outfld( 'UW_ri',          ri,         pcols,   lchnk )

    call outfld( 'UW_sfuh',        sfuh,       pcols,   lchnk )
    call outfld( 'UW_sflh',        sflh,       pcols,   lchnk )
    call outfld( 'UW_sfi',         sfi,        pcols,   lchnk )

    call outfld( 'UW_cldn',        cldn,       pcols,   lchnk )
    call outfld( 'UW_qrl',         qrl,        pcols,   lchnk )
    call outfld( 'UW_ql',          qlfd,       pcols,   lchnk )

    call outfld( 'UW_chu',         chu,        pcols,   lchnk )
    call outfld( 'UW_chs',         chs,        pcols,   lchnk )
    call outfld( 'UW_cmu',         cmu,        pcols,   lchnk )
    call outfld( 'UW_cms',         cms,        pcols,   lchnk )

    call outfld( 'UW_tke',         tke,        pcols,   lchnk )
    call outfld( 'UW_wcap',        wcap,       pcols,   lchnk )
    call outfld( 'UW_bprod',       bprod,      pcols,   lchnk )
    call outfld( 'UW_sprod',       sprod,      pcols,   lchnk )

    call outfld( 'UW_kvh',         kvh_out,    pcols,   lchnk )
    call outfld( 'UW_kvm',         kvm_out,    pcols,   lchnk )

    call outfld( 'UW_pblh',        pblh,       pcols,   lchnk )
    call outfld( 'UW_pblhp',       pblhp,      pcols,   lchnk )
    call outfld( 'UW_tpert',       tpert,      pcols,   lchnk )
    call outfld( 'UW_qpert',       qpert,      pcols,   lchnk )
    call outfld( 'UW_wpert',       wpert,      pcols,   lchnk )

    call outfld( 'UW_ustar',       ustar,      pcols,   lchnk )
    call outfld( 'UW_tkes',        tkes,       pcols,   lchnk )
    call outfld( 'UW_minpblh',     minpblh,    pcols,   lchnk )

    call outfld( 'UW_turbtype',    turbtype,   pcols,   lchnk )

    call outfld( 'UW_kbase_o',     kbase_o,    pcols,   lchnk )
    call outfld( 'UW_ktop_o',      ktop_o,     pcols,   lchnk )
    call outfld( 'UW_ncvfin_o',    ncvfin_o,   pcols,   lchnk )

    call outfld( 'UW_kbase_mg',    kbase_mg,   pcols,   lchnk )
    call outfld( 'UW_ktop_mg',     ktop_mg,    pcols,   lchnk )
    call outfld( 'UW_ncvfin_mg',   ncvfin_mg,  pcols,   lchnk )

    call outfld( 'UW_kbase_f',     kbase_f,    pcols,   lchnk )
    call outfld( 'UW_ktop_f',      ktop_f,     pcols,   lchnk )
    call outfld( 'UW_ncvfin_f',    ncvfin_f,   pcols,   lchnk ) 

    call outfld( 'UW_wet',         wet,        pcols,   lchnk )
    call outfld( 'UW_web',         web,        pcols,   lchnk )
    call outfld( 'UW_jtbu',        jtbu,       pcols,   lchnk )
    call outfld( 'UW_jbbu',        jbbu,       pcols,   lchnk )
    call outfld( 'UW_evhc',        evhc,       pcols,   lchnk )
    call outfld( 'UW_jt2slv',      jt2slv,     pcols,   lchnk )
    call outfld( 'UW_n2ht',        n2ht,       pcols,   lchnk )
    call outfld( 'UW_n2hb',        n2hb,       pcols,   lchnk )
    call outfld( 'UW_lwp',         lwp,        pcols,   lchnk )
    call outfld( 'UW_optdepth',    opt_depth,  pcols,   lchnk )
    call outfld( 'UW_radfrac',     radinvfrac, pcols,   lchnk )
    call outfld( 'UW_radf',        radf,       pcols,   lchnk )
    call outfld( 'UW_wstar',       wstar,      pcols,   lchnk )
    call outfld( 'UW_wstar3fact',  wstar3fact, pcols,   lchnk )
    call outfld( 'UW_ebrk',        ebrk,       pcols,   lchnk )
    call outfld( 'UW_wbrk',        wbrk,       pcols,   lchnk )
    call outfld( 'UW_lbrk',        lbrk,       pcols,   lchnk )
    call outfld( 'UW_ricl',        ricl,       pcols,   lchnk )
    call outfld( 'UW_ghcl',        ghcl,       pcols,   lchnk )
    call outfld( 'UW_shcl',        shcl,       pcols,   lchnk )
    call outfld( 'UW_smcl',        smcl,       pcols,   lchnk )

    call outfld( 'UW_gh',          ghi,        pcols,   lchnk )
    call outfld( 'UW_sh',          shi,        pcols,   lchnk )
    call outfld( 'UW_sm',          smi,        pcols,   lchnk )
    call outfld( 'UW_ria',         rii,        pcols,   lchnk )
    call outfld( 'UW_leng',        lengi,      pcols,   lchnk )

    return
    
  end subroutine compute_eddy_diff

  
  
  
  
  subroutine sfdiag( pcols   , pver    , ncol    , qt      , ql      , sl      , &
                     pi      , pm      , zi      , cld     , sfi     , sfuh    , &
                     sflh    , slslope , qtslope , qsat )
    
    
    
    
    
    
    
    
    
    
    
    
    
    

    implicit none       

    
    
    

    integer,  external    :: qsat

    integer,  intent(in)  :: pcols               
    integer,  intent(in)  :: pver                
    integer,  intent(in)  :: ncol                

    real(r8), intent(in)  :: sl(pcols,pver)      
    real(r8), intent(in)  :: qt(pcols,pver)      
    real(r8), intent(in)  :: ql(pcols,pver)      
    real(r8), intent(in)  :: pi(pcols,pver+1)    
    real(r8), intent(in)  :: pm(pcols,pver)      
    real(r8), intent(in)  :: zi(pcols,pver+1)    
    real(r8), intent(in)  :: cld(pcols,pver)     
    real(r8), intent(in)  :: slslope(pcols,pver) 
    real(r8), intent(in)  :: qtslope(pcols,pver) 

    
    
    

    real(r8), intent(out) :: sfi(pcols,pver+1)   
    real(r8), intent(out) :: sfuh(pcols,pver)    
    real(r8), intent(out) :: sflh(pcols,pver)    

    
    
    

    integer               :: i                   
    integer               :: k                   
    integer               :: km1                 
    integer               :: status              
    real(r8)              :: sltop, slbot        
    real(r8)              :: qttop, qtbot        
    real(r8)              :: tltop(1), tlbot(1)  
    real(r8)              :: qxtop, qxbot        
    real(r8)              :: qxm                 
    real(r8)              :: es(1)               
    real(r8)              :: qs(1)               
    real(r8)              :: gam(1)              
    real(r8)              :: cldeff(pcols,pver)  

    
    
    

    sfi(1:ncol,:)    = 0._r8
    sfuh(1:ncol,:)   = 0._r8
    sflh(1:ncol,:)   = 0._r8
    cldeff(1:ncol,:) = 0._r8

    select case (sftype)
    case ('d')
       
       
       
       do k = ntop_turb + 1, nbot_turb
          km1 = k - 1
          do i = 1, ncol
             sfuh(i,k) = cld(i,k)
             sflh(i,k) = cld(i,k)
             sfi(i,k)  = 0.5_r8 * ( sflh(i,km1) + min( sflh(i,km1), sfuh(i,k) ) )
          end do
       end do
       do i = 1, ncol
          sfi(i,pver+1) = sflh(i,pver) 
       end do
    case ('l')
       
       
       
       do k = ntop_turb + 1, nbot_turb
          km1 = k - 1
          do i = 1, ncol
             cldeff(i,k) = cld(i,k)
             sfuh(i,k)   = cld(i,k)
             sflh(i,k)   = cld(i,k)
             if( ql(i,k) .lt. qmin ) then
                 sfuh(i,k) = 0._r8
                 sflh(i,k) = 0._r8
             end if
           
             if( choice_evhc .eq. 'ramp' .or. choice_radf .eq. 'ramp' ) then 
                 cldeff(i,k) = cld(i,k) * min( ql(i,k) / qmin, 1._r8 )
                 sfuh(i,k)   = cldeff(i,k)
                 sflh(i,k)   = cldeff(i,k)
             elseif( choice_evhc .eq. 'maxi' .or. choice_radf .eq. 'maxi' ) then 
                 cldeff(i,k) = cld(i,k)
                 sfuh(i,k)   = cldeff(i,k)
                 sflh(i,k)   = cldeff(i,k)
             endif
           
             sfi(i,k) = 0.5_r8 * ( sflh(i,km1) + min( sfuh(i,k), sflh(i,km1) ) )
           
           
           
           
           
           
           
           
          end do
       end do
       do i = 1, ncol
          sfi(i,pver+1) = sflh(i,pver)
       end do
    case ('u')
       
       
       
       
    case ('z')
       
       
       
       
       
       
       
       
       
       
       
       
       
       
       
       do k = ntop_turb + 1, nbot_turb
          km1 = k - 1
          do i = 1, ncol
           
             sltop    = sl(i,k) + slslope(i,k) * ( pi(i,k) - pm(i,k) )      
             qttop    = qt(i,k) + qtslope(i,k) * ( pi(i,k) - pm(i,k) )
             tltop(1) = ( sltop - g * zi(i,k) ) / cpair 
             status   = qsat( tltop(1), pi(i,k), es(1), qs(1), gam(1), 1 )
             qxtop    = qttop - qs(1) 
             slbot    = sl(i,k) + slslope(i,k) * ( pi(i,k+1) - pm(i,k) )      
             qtbot    = qt(i,k) + qtslope(i,k) * ( pi(i,k+1) - pm(i,k) )
             tlbot(1) = ( slbot - g * zi(i,k+1) ) / cpair 
             status   = qsat( tlbot(1), pi(i,k+1), es(1), qs(1), gam(1), 1 )
             qxbot    = qtbot - qs(1) 
             qxm      = qxtop + ( qxbot - qxtop ) * ( pm(i,k) - pi(i,k) ) / ( pi(i,k+1) - pi(i,k) )
           
             if( ( qxtop .lt. 0._r8 ) .and. ( qxm .lt. 0._r8 ) ) then
                   sfuh(i,k) = 0._r8 
             else if( ( qxtop .gt. 0._r8 ) .and. ( qxm .gt. 0._r8 ) ) then
                   sfuh(i,k) = 1._r8  
             else 
                   sfuh(i,k) = max( qxtop, qxm ) / abs( qxtop - qxm )
             end if
           
             sfi(i,k) = 0.5_r8 * ( sflh(i,k-1) + min( sflh(i,k-1), sfuh(i,k) ) )
           
             if( ( qxbot .lt. 0._r8 ) .and. ( qxm .lt. 0._r8 ) ) then
                   sflh(i,k) = 0._r8 
             else if( ( qxbot .gt. 0._r8 ) .and. ( qxm .gt. 0._r8 ) ) then
                   sflh(i,k) = 1._r8 
             else 
                   sflh(i,k) = max( qxbot, qxm ) / abs( qxbot - qxm )
             end if
          end do  
       end do 
       do i = 1, ncol
          sfi(i,pver+1) = sflh(i,pver)  
       end do
    end select

  return
  end subroutine sfdiag
  
  
  
  
 
  subroutine trbintd( pcols   , pver    , ncol    ,                               &
                      z       , u       , v       ,                               &
                      t       , pmid    , taux    ,                               &
                      tauy    , ustar   , rrho    ,                               &
                      s2      , n2      , ri      ,                               &
                      zi      , pi      , cld     ,                               &
                      qt      , qv      , ql      , qi      , sfi     , sfuh    , &
                      sflh    , sl      , slv     , slslope , qtslope ,           &
                      chs     , chu     , cms     , cmu     , minpblh , qsat )
    
    
    
    
    
    
    
    
    

    implicit none

    
    
    

    integer,  intent(in)  :: pcols                            
    integer,  intent(in)  :: pver                             
    integer,  intent(in)  :: ncol                             
    real(r8), intent(in)  :: z(pcols,pver)                    
    real(r8), intent(in)  :: u(pcols,pver)                    
    real(r8), intent(in)  :: v(pcols,pver)                    
    real(r8), intent(in)  :: t(pcols,pver)                    
    real(r8), intent(in)  :: pmid(pcols,pver)                 
    real(r8), intent(in)  :: taux(pcols)                      
    real(r8), intent(in)  :: tauy(pcols)                      
    real(r8), intent(in)  :: zi(pcols,pver+1)                 
    real(r8), intent(in)  :: pi(pcols,pver+1)                 
    real(r8), intent(in)  :: cld(pcols,pver)                  
    real(r8), intent(in)  :: qv(pcols,pver)                   
    real(r8), intent(in)  :: ql(pcols,pver)                   
    real(r8), intent(in)  :: qi(pcols,pver)                   
    integer,  external    :: qsat

    
    
    

    real(r8), intent(out) :: ustar(pcols)                     
    real(r8), intent(out) :: s2(pcols,pver)                   
    real(r8), intent(out) :: n2(pcols,pver)                   
    real(r8), intent(out) :: ri(pcols,pver)                   
 
    real(r8), intent(out) :: qt(pcols,pver)                   
    real(r8), intent(out) :: sfi(pcols,pver+1)                
    real(r8), intent(out) :: sfuh(pcols,pver)                 
    real(r8), intent(out) :: sflh(pcols,pver)                 
    real(r8), intent(out) :: sl(pcols,pver)                   
    real(r8), intent(out) :: slv(pcols,pver)                  
   
    real(r8), intent(out) :: chu(pcols,pver+1)                
    real(r8), intent(out) :: chs(pcols,pver+1)                
    real(r8), intent(out) :: cmu(pcols,pver+1)                
    real(r8), intent(out) :: cms(pcols,pver+1)                
    real(r8), intent(out) :: slslope(pcols,pver)              
    real(r8), intent(out) :: qtslope(pcols,pver)              
    real(r8), intent(out) :: rrho(pcols)                      
    real(r8), intent(out) :: minpblh(pcols)                   
 
    
    
    

    integer               :: i                                
    integer               :: k, km1                           
    integer               :: status                           

    real(r8)              :: qs(pcols,pver)                   
    real(r8)              :: es(pcols,pver)                   
    real(r8)              :: gam(pcols,pver)                  
    real(r8)              :: rdz                              
    real(r8)              :: dsldz                            
    real(r8)              :: dqtdz                            
    real(r8)              :: ch                               
    real(r8)              :: cm                               
    real(r8)              :: bfact                            
    real(r8)              :: product                          
    real(r8)              :: dsldp_a, dqtdp_a                 
    real(r8)              :: dsldp_b(pcols), dqtdp_b(pcols)   

    
    
    

    

    do i = 1, ncol
       rrho(i)    = rair * t(i,pver) / pmid(i,pver)
       ustar(i)   = max( sqrt( sqrt( taux(i)**2 + tauy(i)**2 ) * rrho(i) ), ustar_min )
       minpblh(i) = 100.0_r8 * ustar(i)                       
    end do

    
    

    do k = ntop_turb, nbot_turb
       status = qsat( t(1,k), pmid(1,k), es(1,k), qs(1,k), gam(1,k), ncol )
       do i = 1, ncol
          qt(i,k)  = qv(i,k) + ql(i,k) + qi(i,k) 
          sl(i,k)  = cpair * t(i,k) + g * z(i,k) - latvap * ql(i,k) - latsub * qi(i,k)
          slv(i,k) = sl(i,k) * ( 1._r8 + zvir * qt(i,k) )
        
        
        
        
          bfact    = g / ( t(i,k) * ( 1._r8 + zvir * qv(i,k) - ql(i,k) - qi(i,k) ) )
          chu(i,k) = ( 1._r8 + zvir * qt(i,k) ) * bfact / cpair
          chs(i,k) = ( ( 1._r8 + ( 1._r8 + zvir ) * gam(i,k) * cpair * t(i,k) / latvap ) / ( 1._r8 + gam(i,k) ) ) * bfact / cpair
          cmu(i,k) = zvir * bfact * t(i,k)
          cms(i,k) = latvap * chs(i,k)  -  bfact * t(i,k)
       end do
    end do

    do i = 1, ncol
       chu(i,pver+1) = chu(i,pver)
       chs(i,pver+1) = chs(i,pver)
       cmu(i,pver+1) = cmu(i,pver)
       cms(i,pver+1) = cms(i,pver)
    end do

    
    
    
    
    
    
    
    

    do i = 1, ncol
     
       slslope(i,pver) = ( sl(i,pver) - sl(i,pver-1) ) / ( pmid(i,pver) - pmid(i,pver-1) )
       qtslope(i,pver) = ( qt(i,pver) - qt(i,pver-1) ) / ( pmid(i,pver) - pmid(i,pver-1) )
       slslope(i,1)    = ( sl(i,2) - sl(i,1) ) / ( pmid(i,2) - pmid(i,1) )
       qtslope(i,1)    = ( qt(i,2) - qt(i,1) ) / ( pmid(i,2) - pmid(i,1) )
       dsldp_b(i)      = slslope(i,1)
       dqtdp_b(i)      = qtslope(i,1)
    end do

    do k = 2, pver - 1
       do i = 1, ncol
          dsldp_a    = dsldp_b(i)
          dqtdp_a    = dqtdp_b(i)
          dsldp_b(i) = ( sl(i,k+1) - sl(i,k) ) / ( pmid(i,k+1) - pmid(i,k) )
          dqtdp_b(i) = ( qt(i,k+1) - qt(i,k) ) / ( pmid(i,k+1) - pmid(i,k) )
          product    = dsldp_a * dsldp_b(i)
          if( product .le. 0._r8 ) then 
              slslope(i,k) = 0._r8
          else if( product .gt. 0._r8 .and. dsldp_a .lt. 0._r8 ) then 
              slslope(i,k) = max( dsldp_a, dsldp_b(i) )
          else if( product .gt. 0._r8 .and. dsldp_a .gt. 0._r8 ) then 
              slslope(i,k) = min( dsldp_a, dsldp_b(i) )
          end if
          product = dqtdp_a*dqtdp_b(i)
          if( product .le. 0._r8 ) then 
              qtslope(i,k) = 0._r8
          else if( product .gt. 0._r8 .and. dqtdp_a .lt. 0._r8 ) then 
              qtslope(i,k) = max( dqtdp_a, dqtdp_b(i) )
          else if( product .gt. 0._r8 .and. dqtdp_a .gt. 0._r8 ) then 
              qtslope(i,k) = min( dqtdp_a, dqtdp_b(i) )
          end if
       end do 
    end do 

    
    

    call sfdiag( pcols  , pver    , ncol    , qt      , ql      , sl      , & 
                 pi     , pmid    , zi      , cld     , sfi     , sfuh    , &
                 sflh   , slslope , qtslope , qsat )

    
    
    
    
    
    
    

    do k = nbot_turb, ntop_turb + 1, -1
       km1 = k - 1
       do i = 1, ncol
          rdz      = 1._r8 / ( z(i,km1) - z(i,k) )
          dsldz    = ( sl(i,km1) - sl(i,k) ) * rdz
          dqtdz    = ( qt(i,km1) - qt(i,k) ) * rdz 
          chu(i,k) = ( chu(i,km1) + chu(i,k) ) * 0.5_r8
          chs(i,k) = ( chs(i,km1) + chs(i,k) ) * 0.5_r8
          cmu(i,k) = ( cmu(i,km1) + cmu(i,k) ) * 0.5_r8
          cms(i,k) = ( cms(i,km1) + cms(i,k) ) * 0.5_r8
          ch       = chu(i,k) * ( 1._r8 - sfi(i,k) ) + chs(i,k) * sfi(i,k)
          cm       = cmu(i,k) * ( 1._r8 - sfi(i,k) ) + cms(i,k) * sfi(i,k)
          n2(i,k)  = ch * dsldz +  cm * dqtdz
          s2(i,k)  = ( ( u(i,km1) - u(i,k) )**2 + ( v(i,km1) - v(i,k) )**2) * rdz**2
          s2(i,k)  = max( ntzero, s2(i,k) )
          ri(i,k)  = n2(i,k) / s2(i,k)
       end do
    end do 
    do i = 1, ncol
       n2(i,1) = n2(i,2)
       s2(i,1) = s2(i,2)
       ri(i,1) = ri(i,2)
    end do

  return

  end subroutine trbintd

  
  
  
  
  subroutine austausch_atm( pcols, pver, ncol, ri, s2, kvf )

    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    implicit none
    
    
    
    

    integer,  intent(in)  :: pcols                
    integer,  intent(in)  :: pver                 
    integer,  intent(in)  :: ncol                 

    real(r8), intent(in)  :: s2(pcols,pver)       
    real(r8), intent(in)  :: ri(pcols,pver)       

    
    
    

    real(r8), intent(out) :: kvf(pcols,pver+1)    

    
    
    

    real(r8)              :: fofri                
    real(r8)              :: kvn                  

    integer               :: i                    
    integer               :: k                    

    
    
    

    kvf(:ncol,:)           = 0.0_r8
    kvf(:ncol,pver+1)      = 0.0_r8
    kvf(:ncol,1:ntop_turb) = 0.0_r8

    

    do k = ntop_turb + 1, nbot_turb
       do i = 1, ncol
          if( ri(i,k) < 0.0_r8 ) then
              fofri = sqrt( max( 1._r8 - 18._r8 * ri(i,k), 0._r8 ) )
          else 
              fofri = 1.0_r8 / ( 1.0_r8 + 10.0_r8 * ri(i,k) * ( 1.0_r8 + 8.0_r8 * ri(i,k) ) )    
          end if
          kvn = ml2(k) * sqrt(s2(i,k))
          kvf(i,k) = max( zkmin, kvn * fofri )
       end do
    end do

    return

    end subroutine austausch_atm

    
    
    
    
    
    
    
    

    subroutine caleddy( pcols        , pver         , ncol        ,                             &
                        sl           , qt           , ql          , slv        , u            , &
                        v            , pi           , z           , zi         ,                &
                        qflx         , shflx        , slslope     , qtslope    ,                &
                        chu          , chs          , cmu         , cms        , sfuh         , &
                        sflh         , n2           , s2          , ri         , rrho         , &
                        pblh         , ustar        ,                                           &
                        kvh_in       , kvm_in       , kvh         , kvm        ,                &
                        tpert        , qpert        , qrlin       , kvf        , tke          , & 
                        wstarent     , bprod        , sprod       , minpblh    , wpert        , &
                        tkes         , turbtype_f   , sm_aw       ,                             &
                        kbase_o      , ktop_o       , ncvfin_o    ,                             & 
                        kbase_mg     , ktop_mg      , ncvfin_mg   ,                             & 
                        kbase_f      , ktop_f       , ncvfin_f    ,                             & 
                        wet_CL       , web_CL       , jtbu_CL     , jbbu_CL    ,                &
                        evhc_CL      , jt2slv_CL    , n2ht_CL     , n2hb_CL    , lwp_CL       , &
                        opt_depth_CL , radinvfrac_CL, radf_CL     , wstar_CL   , wstar3fact_CL, &
                        ebrk         , wbrk         , lbrk        , ricl       , ghcl         , & 
                        shcl         , smcl         ,                                           &
                        gh_a         , sh_a         , sm_a        , ri_a       , leng         , & 
                        wcap         , pblhp        , cld         , ipbl       , kpblh        , &
                        wsedl        )

    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    

    
    
    

    implicit none

    integer,  intent(in) :: pcols                     
    integer,  intent(in) :: pver                      
    integer,  intent(in) :: ncol                      
    real(r8), intent(in) :: u(pcols,pver)             
    real(r8), intent(in) :: v(pcols,pver)             
    real(r8), intent(in) :: sl(pcols,pver)            
    real(r8), intent(in) :: slv(pcols,pver)           
    real(r8), intent(in) :: qt(pcols,pver)            
    real(r8), intent(in) :: ql(pcols,pver)            
    real(r8), intent(in) :: pi(pcols,pver+1)          
    real(r8), intent(in) :: z(pcols,pver)             
    real(r8), intent(in) :: zi(pcols,pver+1)          
    real(r8), intent(in) :: chu(pcols,pver+1)         
    real(r8), intent(in) :: chs(pcols,pver+1)         
    real(r8), intent(in) :: cmu(pcols,pver+1)         
    real(r8), intent(in) :: cms(pcols,pver+1)         
    real(r8), intent(in) :: sfuh(pcols,pver)          
    real(r8), intent(in) :: sflh(pcols,pver)          
    real(r8), intent(in) :: n2(pcols,pver)            
    real(r8), intent(in) :: s2(pcols,pver)            
    real(r8), intent(in) :: ri(pcols,pver)            
    real(r8), intent(in) :: qflx(pcols)               
    real(r8), intent(in) :: shflx(pcols)              
    real(r8), intent(in) :: slslope(pcols,pver)       
    real(r8), intent(in) :: qtslope(pcols,pver)       
    real(r8), intent(in) :: qrlin(pcols,pver)         
    real(r8), intent(in) :: wsedl(pcols,pver)         
    real(r8), intent(in) :: ustar(pcols)              
    real(r8), intent(in) :: rrho(pcols)               
    real(r8), intent(in) :: kvf(pcols,pver+1)         
    logical,  intent(in) :: wstarent                  
    real(r8), intent(in) :: minpblh(pcols)            
    real(r8), intent(in) :: kvh_in(pcols,pver+1)      
    real(r8), intent(in) :: kvm_in(pcols,pver+1)      
    real(r8), intent(in) :: cld(pcols,pver)           

    
    
    

    real(r8), intent(out) :: kvh(pcols,pver+1)        
    real(r8), intent(out) :: kvm(pcols,pver+1)        
    real(r8), intent(out) :: pblh(pcols)              
    real(r8), intent(out) :: pblhp(pcols)             
    real(r8), intent(out) :: tpert(pcols)             
    real(r8), intent(out) :: qpert(pcols)             
    real(r8), intent(out) :: wpert(pcols)             
    real(r8), intent(out) :: tke(pcols,pver+1)        
    real(r8), intent(out) :: bprod(pcols,pver+1)      
    real(r8), intent(out) :: sprod(pcols,pver+1)      
    real(r8), intent(out) :: turbtype_f(pcols,pver+1) 
                                                      
                                                      
                                                      
                                                      
                                                      
                                                      
    real(r8), intent(out) :: sm_aw(pcols,pver+1)      
    real(r8), intent(out) :: ipbl(pcols)              
    real(r8), intent(out) :: kpblh(pcols)             

    
    
    

    real(r8) :: tkes(pcols)                           
    real(r8) :: kbase_o(pcols,ncvmax)                 
    real(r8) :: ktop_o(pcols,ncvmax)                  
    real(r8) :: ncvfin_o(pcols)                       
    real(r8) :: kbase_mg(pcols,ncvmax)                ! kbase  just after extending-merging (after 'zisocl') but without SRCL
    real(r8) :: ktop_mg(pcols,ncvmax)                 ! ktop   just after extending-merging (after 'zisocl') but without SRCL
    real(r8) :: ncvfin_mg(pcols)                      ! ncvfin just after extending-merging (after 'zisocl') but without SRCL
    real(r8) :: kbase_f(pcols,ncvmax)                 
    real(r8) :: ktop_f(pcols,ncvmax)                  
    real(r8) :: ncvfin_f(pcols)                       
    real(r8) :: wet_CL(pcols,ncvmax)                  
    real(r8) :: web_CL(pcols,ncvmax)                  
    real(r8) :: jtbu_CL(pcols,ncvmax)                 
    real(r8) :: jbbu_CL(pcols,ncvmax)                 
    real(r8) :: evhc_CL(pcols,ncvmax)                 
    real(r8) :: jt2slv_CL(pcols,ncvmax)               
    real(r8) :: n2ht_CL(pcols,ncvmax)                 
    real(r8) :: n2hb_CL(pcols,ncvmax)                 
    real(r8) :: lwp_CL(pcols,ncvmax)                  
    real(r8) :: opt_depth_CL(pcols,ncvmax)            
    real(r8) :: radinvfrac_CL(pcols,ncvmax)           
    real(r8) :: radf_CL(pcols,ncvmax)                 
    real(r8) :: wstar_CL(pcols,ncvmax)                
    real(r8) :: wstar3fact_CL(pcols,ncvmax)           

    real(r8) :: gh_a(pcols,pver+1)                    
    real(r8) :: sh_a(pcols,pver+1)                    
    real(r8) :: sm_a(pcols,pver+1)                    
    real(r8) :: ri_a(pcols,pver+1)                    

    real(r8) :: ebrk(pcols,ncvmax)                    
    real(r8) :: wbrk(pcols,ncvmax)                    
    real(r8) :: lbrk(pcols,ncvmax)                    
    real(r8) :: ricl(pcols,ncvmax)                    
    real(r8) :: ghcl(pcols,ncvmax)                    
    real(r8) :: shcl(pcols,ncvmax)                    
    real(r8) :: smcl(pcols,ncvmax)                    

    real(r8) :: leng(pcols,pver+1)                    
    real(r8) :: wcap(pcols,pver+1)                    
                                                      
    
    
    

    logical :: belongcv(pcols,pver+1)                 
    logical :: belongst(pcols,pver+1)                 
    logical :: in_CL                                  
    logical :: extend                                 ! True when CL is extended in zisocl
    logical :: extend_up                              ! True when CL is extended upward in zisocl
    logical :: extend_dn                              ! True when CL is extended downward in zisocl

    integer :: i                                      
    integer :: k                                      
    integer :: ks                                     
    integer :: ncvfin(pcols)                          
    integer :: ncvf                                   
    integer :: ncv                                    
    integer :: ncvnew                                 ! Index of added SRCL appended after regular CLs from 'zisocl'
    integer :: ncvsurf                                
    integer :: kbase(pcols,ncvmax)                    
    integer :: ktop(pcols,ncvmax)                     
    integer :: kb, kt                                 
    integer :: ktblw                                  
    integer :: turbtype(pcols,pver+1)                 
                                                      
                                                      
                                                      
                                                      
                                                      
                                                      
    integer  :: ktopbl(pcols)                         
    real(r8) :: bflxs(pcols)                          
    real(r8) :: rcap                                  
    real(r8) :: jtzm                                  
    real(r8) :: jtsl                                  
    real(r8) :: jtqt                                  
    real(r8) :: jtbu                                  
    real(r8) :: jtu                                   
    real(r8) :: jtv                                   
    real(r8) :: jt2slv                                
    real(r8) :: radf                                  
    real(r8) :: jbzm                                  
    real(r8) :: jbsl                                  
    real(r8) :: jbqt                                  
    real(r8) :: jbbu                                  
    real(r8) :: jbu                                   
    real(r8) :: jbv                                   
    real(r8) :: ch                                    
    real(r8) :: cm                                    
                                                      
    real(r8) :: n2ht                                  
    real(r8) :: n2hb                                  
    real(r8) :: n2htSRCL                              
                                                      
    real(r8) :: gh                                    
    real(r8) :: sh                                    
    real(r8) :: sm                                    
    real(r8) :: lbulk                                 
    real(r8) :: dzht                                  
    real(r8) :: dzhb                                  
    real(r8) :: rootp                                 
    real(r8) :: evhc                                  
    real(r8) :: kentr                                 
    real(r8) :: lwp                                   
    real(r8) :: opt_depth                             
    real(r8) :: radinvfrac                            
    real(r8) :: wet                                   
    real(r8) :: web                                   
    real(r8) :: vyt                                   
    real(r8) :: vyb                                   
    real(r8) :: vut                                   
    real(r8) :: vub                                   
    real(r8) :: fact                                  
    real(r8) :: trma                                  
    real(r8) :: trmb                                  
    real(r8) :: trmc                                  
    real(r8) :: trmp                                  
    real(r8) :: trmq                                  
    real(r8) :: qq                                    
    real(r8) :: det                                   
    real(r8) :: gg                                    
                                                      
    real(r8) :: dzhb5                                 
    real(r8) :: dzht5                                 
    real(r8) :: qrlw(pcols,pver)                      

    real(r8) :: cldeff(pcols,pver)                    
    real(r8) :: qleff                                 
    real(r8) :: tunlramp                              
    real(r8) :: leng_imsi                             
    real(r8) :: tke_imsi                              
    real(r8) :: kvh_imsi                              
    real(r8) :: kvm_imsi                              
    real(r8) :: alph4exs                              
    real(r8) :: ghmin                                 

    real(r8) :: sedfact                               

    

    real(r8) :: cet                                   
    real(r8) :: ceb                                   
    real(r8) :: wstar                                 
    real(r8) :: wstar3                                
    real(r8) :: wstar3fact                            
    real(r8) :: rmin                                  
    real(r8) :: fmin                                  
    real(r8) :: rcrit                                 
    real(r8) :: fcrit                                 
    logical     noroot                                

    
    
    
    
    
    
    

    if( set_qrlzero ) then
        qrlw(:,:) = 0._r8
    else
        qrlw(:ncol,:pver) = qrlin(:ncol,:pver)
    endif

    
    
    
    

    do k = 1, pver
       do i = 1, ncol
          if( choice_evhc .eq. 'ramp' .or. choice_radf .eq. 'ramp' ) then 
              cldeff(i,k) = cld(i,k) * min( ql(i,k) / qmin, 1._r8 )
          else
              cldeff(i,k) = cld(i,k)
          endif
       end do
    end do

    
    

    if( ricrit .eq. 0.19_r8 ) then
        alph4exs = alph4
        ghmin    = -3.5334_r8
    elseif( ricrit .gt. 0.19_r8 ) then
        alph4exs = -2._r8 * b1 * alph2 / ( alph3 - 2._r8 * b1 * alph5 ) / ricrit
        ghmin    = -1.e10_r8
    else
        write(iulog,*) 'Error : ricrit should be larger than 0.19 in UW PBL'       
        call wrf_message(iulog)
        stop
    endif

    
    
    

    do i = 1, ncol
       wet_CL(i,:ncvmax)        = 0._r8
       web_CL(i,:ncvmax)        = 0._r8
       jtbu_CL(i,:ncvmax)       = 0._r8
       jbbu_CL(i,:ncvmax)       = 0._r8
       evhc_CL(i,:ncvmax)       = 0._r8
       jt2slv_CL(i,:ncvmax)     = 0._r8
       n2ht_CL(i,:ncvmax)       = 0._r8
       n2hb_CL(i,:ncvmax)       = 0._r8                    
       lwp_CL(i,:ncvmax)        = 0._r8
       opt_depth_CL(i,:ncvmax)  = 0._r8
       radinvfrac_CL(i,:ncvmax) = 0._r8
       radf_CL(i,:ncvmax)       = 0._r8
       wstar_CL(i,:ncvmax)      = 0._r8          
       wstar3fact_CL(i,:ncvmax) = 0._r8
       ricl(i,:ncvmax)          = 0._r8
       ghcl(i,:ncvmax)          = 0._r8
       shcl(i,:ncvmax)          = 0._r8
       smcl(i,:ncvmax)          = 0._r8
       ebrk(i,:ncvmax)          = 0._r8
       wbrk(i,:ncvmax)          = 0._r8
       lbrk(i,:ncvmax)          = 0._r8
       gh_a(i,:pver+1)          = 0._r8
       sh_a(i,:pver+1)          = 0._r8
       sm_a(i,:pver+1)          = 0._r8
       ri_a(i,:pver+1)          = 0._r8
       sm_aw(i,:pver+1)         = 0._r8
       ipbl(i)                  = 0._r8
       kpblh(i)                 = real(pver,r8)
    end do  

    
    
    
    
    

    do k = 1, pver + 1
       do i = 1, ncol
        
          if( use_kvf ) then
              kvh(i,k) = kvf(i,k)
              kvm(i,k) = kvf(i,k)
          else
              kvh(i,k) = 0._r8
              kvm(i,k) = 0._r8
          end if
        
          wcap(i,k) = 0._r8
          leng(i,k) = 0._r8
          tke(i,k)  = 0._r8
          turbtype(i,k) = 0
       end do
    end do

    
    
    
    
    
    
    

    do k = 2, pver
       do i = 1, ncol
            bprod(i,k) = -kvh_in(i,k) * n2(i,k)
            sprod(i,k) =  kvm_in(i,k) * s2(i,k)
       end do
    end do

    
    
    
    
    
    
    
    
    
    
    ! in 'zisocl', I should use 'l2n2 = - wint / sh'  for similar treatment
    
    
    

    do i = 1, ncol
       bprod(i,1) = 0._r8 
       sprod(i,1) = 0._r8 
       ch = chu(i,pver+1) * ( 1._r8 - sflh(i,pver) ) + chs(i,pver+1) * sflh(i,pver)   
       cm = cmu(i,pver+1) * ( 1._r8 - sflh(i,pver) ) + cms(i,pver+1) * sflh(i,pver)   
       bflxs(i) = ch * shflx(i) * rrho(i) + cm * qflx(i) * rrho(i)
       if( choice_tkes .eq. 'ibprod' ) then
           bprod(i,pver+1) = bflxs(i)
       else
           bprod(i,pver+1) = 0._r8
       endif
       sprod(i,pver+1) = (ustar(i)**3)/(vk*z(i,pver))
    end do

    
    
    
    
    
    
    
    
    
    
    

    call exacol( pcols, pver, ncol, ri, bflxs, minpblh, zi, ktop, kbase, ncvfin )

    
    ! of CL regimes in 'zisocl'
    do i = 1, ncol
    do k = 1, ncvmax
       kbase_o(i,k) = real(kbase(i,k),r8)
       ktop_o(i,k)  = real(ktop(i,k),r8) 
       ncvfin_o(i)  = real(ncvfin(i),r8)
    end do
    end do 

    
    
    

    do i = 1, ncol

       
       
       
       
       
       
       
       
       
       
       
       
       
       
       
  
     
       tkes(i) = max(b1*vk*z(i,pver)*(bprod(i,pver+1)+sprod(i,pver+1)), 1.e-7_r8)**(2._r8/3._r8)
       tkes(i) = min(tkes(i), tkemax)
       tke(i,pver+1)  = tkes(i)
       wcap(i,pver+1) = tkes(i)/b1

       
       ! CL internal mean energetics and stability functions in 'zisocl'. 
       
       ! with height. The following outputs are from 'zisocl'. Here, the dimension
       
       
       
       
       
       
       
       
       
       
       
       
       
       
       
       
       
       
       
       
       
       ! in 'zisocl' but before adding SRCLs), if any newly identified SRCL (e.g., 
       
       

       ncvsurf = 0
       if( ncvfin(i) .gt. 0 ) then 
           call zisocl( pcols  , pver     , i        ,           &
                        z      , zi       , n2       , s2      , & 
                        bprod  , sprod    , bflxs    , tkes    , &
                        ncvfin , kbase    , ktop     , belongcv, &
                        ricl   , ghcl     , shcl     , smcl    , & 
                        lbrk   , wbrk     , ebrk     ,           & 
                        extend , extend_up, extend_dn )
           if( kbase(i,1) .eq. pver + 1 ) ncvsurf = 1
       else
           belongcv(i,:) = .false.
       endif

       ! Diagnostic output after finishing extending-merging process in 'zisocl'
       

       do k = 1, ncvmax
          kbase_mg(i,k) = real(kbase(i,k))
          ktop_mg(i,k)  = real(ktop(i,k)) 
          ncvfin_mg(i)  = real(ncvfin(i))
       end do 

       
       
       

     
     
     
     
     

       
       
       
       
       
       
       
       
       
       
       
       
       
       
       
       
       
       
       
       
       
       
       
       
       
       
       
       
       
       ! with height similar to the regular CLs indices identified from 'zisocl'.       !
       

       ncv  = 1
       ncvf = ncvfin(i)

       if( choice_SRCL .eq. 'remove' ) goto 222 

       do k = nbot_turb, ntop_turb + 1, -1 

          if( ql(i,k) .gt. qmin .and. ql(i,k-1) .lt. qmin .and. qrlw(i,k) .lt. 0._r8 &
                                .and. ri(i,k) .ge. ricrit ) then

              
              
              
              
              
              
 
              if( choice_SRCL .eq. 'nonamb' .and. belongcv(i,k+1) ) then
                  go to 220 
              endif

              ch = ( 1._r8 - sfuh(i,k) ) * chu(i,k) + sfuh(i,k) * chs(i,k)
              cm = ( 1._r8 - sfuh(i,k) ) * cmu(i,k) + sfuh(i,k) * cms(i,k)

              n2htSRCL = ch * slslope(i,k) + cm * qtslope(i,k)

              if( n2htSRCL .le. 0._r8 ) then

                  
                  
                  ! calculation makes use of 'ncv set' obtained from 'zisocl'. The 
                  
                  

                  in_CL = .false.

                  do while ( ncv .le. ncvf )
                     if( ktop(i,ncv) .le. k ) then
                        if( kbase(i,ncv) .gt. k ) then 
                            in_CL = .true.
                        endif
                        exit             
                     else
                        ncv = ncv + 1    
                     end if
                  end do 

                  if( .not. in_CL ) then 

                     

                     ncvfin(i)       =  ncvfin(i) + 1
                     ncvnew          =  ncvfin(i)
                     ktop(i,ncvnew)  =  k
                     kbase(i,ncvnew) =  k+1
                     belongcv(i,k)   = .true.
                     belongcv(i,k+1) = .true.

                     
                     
                     
                     
                     
                     
                     
                     
                     
                     
                     
                     
                     
                     

                     if( k .lt. pver ) then

                         wbrk(i,ncvnew) = 0._r8
                         ebrk(i,ncvnew) = 0._r8
                         lbrk(i,ncvnew) = 0._r8
                         ghcl(i,ncvnew) = 0._r8
                         shcl(i,ncvnew) = 0._r8
                         smcl(i,ncvnew) = 0._r8
                         ricl(i,ncvnew) = 0._r8

                     else 

                         if( bflxs(i) .gt. 0._r8 ) then    
                                                           
                                                           
                                                           ! have been identified as SBCL in 'zisocl' ahead. 
                             ebrk(i,ncvnew) = tkes(i)
                             lbrk(i,ncvnew) = z(i,pver)
                             wbrk(i,ncvnew) = tkes(i) / b1    
        
                             write(iulog,*) 'Major mistake in SRCL: bflxs > 0 for surface-based SRCL'
                             call wrf_message(iulog)
                             write(iulog,*) 'bflxs = ', bflxs(i)
                             call wrf_message(iulog)
                             write(iulog,*) 'ncvfin_o = ', ncvfin_o(i)
                             call wrf_message(iulog)
                             write(iulog,*) 'ncvfin_mg = ', ncvfin_mg(i)
                             call wrf_message(iulog)
                             do ks = 1, ncvmax
                                write(iulog,*) 'ncv =', ks, ' ', kbase_o(i,ks), ktop_o(i,ks), kbase_mg(i,ks), ktop_mg(i,ks)
                                call wrf_message(iulog)
                             end do
                             stop

                         else                              

                             ebrk(i,ncvnew) = 0._r8
                             lbrk(i,ncvnew) = 0._r8
                             wbrk(i,ncvnew) = 0._r8

                         endif

                         
                         
                         
                         
                         
                         
                         
                         
                         
                         
                         gg = 0.5_r8 * vk * z(i,pver) * bprod(i,pver+1) / ( tkes(i)**(3._r8/2._r8) )
                         if( abs(alph5-gg*alph3) .le. 1.e-7_r8 ) then
                           
                           
                             gh = ghmin
                         else    
                             gh = gg / ( alph5 - gg * alph3 )
                         end if 
                       
                       
                         gh = min(max(gh,ghmin),0.0233_r8)
                         ghcl(i,ncvnew) =  gh
                         shcl(i,ncvnew) =  max(0._r8,alph5/(1._r8+alph3*gh))
                         smcl(i,ncvnew) =  max(0._r8,(alph1 + alph2*gh)/(1._r8+alph3*gh)/(1._r8+alph4exs*gh))
                         ricl(i,ncvnew) = -(smcl(i,ncvnew)/shcl(i,ncvnew))*(bprod(i,pver+1)/sprod(i,pver+1))

                       
                       
    
                         ncvsurf = ncvnew

                      end if

                  end if

              end if

          end if

   220 continue    

       end do 

   222 continue

       
       
       
       
       
       
       
       ! '1-4' were identified from 'zisocl' while '5' were identified separately   !
       ! after performing 'zisocl'. CL regime index of '1-4' increases with height  !
       
       
       ! 'zisocl'. However, CL regime indices of SRCLs itself increases with height !
       ! when there are multiple SRCLs, similar to the regular CLs from 'zisocl'.   !
       

       
       
       do k = 1, ncvmax
          kbase_f(i,k) = real(kbase(i,k))
          ktop_f(i,k)  = real(ktop(i,k)) 
          ncvfin_f(i)  = real(ncvfin(i))
       end do 

       
       
       
       
       
       
       
       
       
       
       
       
       
       
       
       
       
       
       

       ktblw = 0
       do ncv = 1, ncvfin(i)

          kt = ktop(i,ncv)
          kb = kbase(i,ncv)
          
          if( kb .eq. (pver+1) .and. bflxs(i) .le. 0._r8 ) then
              lbulk = zi(i,kt) - z(i,pver)
          else
              lbulk = zi(i,kt) - zi(i,kb)
          end if

          
          
          
          
          

          do k = min(kb,pver), kt, -1 
             if( choice_tunl .eq. 'rampcl' ) then
               
               
               
               
                 tunlramp = ctunl*tunl*(1._r8-(1._r8-1._r8/ctunl)*exp(min(0._r8,ricl(i,ncv))))
                 tunlramp = min(max(tunlramp,tunl),ctunl*tunl)
             elseif( choice_tunl .eq. 'rampsl' ) then
                 tunlramp = ctunl*tunl
               
             else
                 tunlramp = tunl
             endif
             if( choice_leng .eq. 'origin' ) then
                 leng(i,k) = ( (vk*zi(i,k))**(-cleng) + (tunlramp*lbulk)**(-cleng) )**(-1._r8/cleng)
               
             else
                 leng(i,k) = min( vk*zi(i,k), tunlramp*lbulk )              
             endif
             wcap(i,k) = (leng(i,k)**2) * (-shcl(i,ncv)*n2(i,k)+smcl(i,ncv)*s2(i,k))
          end do 

          
          

          if( kb .lt. pver+1 ) then 

              jbzm = z(i,kb-1) - z(i,kb)                                      
              jbsl = sl(i,kb-1) - sl(i,kb)                                    
              jbqt = qt(i,kb-1) - qt(i,kb)                                    
              jbbu = n2(i,kb) * jbzm                                          
              jbbu = max(jbbu,jbumin)                                         
              jbu  = u(i,kb-1) - u(i,kb)                                      
              jbv  = v(i,kb-1) - v(i,kb)                                      
              ch   = (1._r8 -sflh(i,kb-1))*chu(i,kb) + sflh(i,kb-1)*chs(i,kb) 
              cm   = (1._r8 -sflh(i,kb-1))*cmu(i,kb) + sflh(i,kb-1)*cms(i,kb) 
              n2hb = (ch*jbsl + cm*jbqt)/jbzm                                 
              vyb  = n2hb*jbzm/jbbu                                           
              vub  = min(1._r8,(jbu**2+jbv**2)/(jbbu*jbzm) )                  

          else 

            
              jbbu = 0._r8
              n2hb = 0._r8
              vyb  = 0._r8
              vub  = 0._r8
              web  = 0._r8

          end if

          
          
          

          jtzm = z(i,kt-1) - z(i,kt)
          jtsl = sl(i,kt-1) - sl(i,kt)
          jtqt = qt(i,kt-1) - qt(i,kt)
          jtbu = n2(i,kt)*jtzm                                                
          jtbu = max(jtbu,jbumin)                                             
          jtu  = u(i,kt-1) - u(i,kt)
          jtv  = v(i,kt-1) - v(i,kt)
          ch   = (1._r8 -sfuh(i,kt))*chu(i,kt) + sfuh(i,kt)*chs(i,kt) 
          cm   = (1._r8 -sfuh(i,kt))*cmu(i,kt) + sfuh(i,kt)*cms(i,kt) 
          n2ht = (ch*jtsl + cm*jtqt)/jtzm                       
          vyt  = n2ht*jtzm/jtbu                                  
          vut  = min(1._r8,(jtu**2+jtv**2)/(jtbu*jtzm))             

          
          
          
          
          
          
          

          evhc   = 1._r8
          jt2slv = 0._r8

        
        

          if( choice_evhc .eq. 'orig' ) then

              if( ql(i,kt) .gt. qmin .and. ql(i,kt-1) .lt. qmin ) then 
                  jt2slv = slv(i,max(kt-2,1)) - slv(i,kt)
                  jt2slv = max( jt2slv, jbumin*slv(i,kt-1)/g )
                  evhc   = 1._r8 + a2l * a3l * latvap * ql(i,kt) / jt2slv
                  evhc   = min( evhc, evhcmax )
              end if

          elseif( choice_evhc .eq. 'ramp' ) then

              jt2slv = slv(i,max(kt-2,1)) - slv(i,kt)
              jt2slv = max( jt2slv, jbumin*slv(i,kt-1)/g )
              evhc   = 1._r8 + max(cldeff(i,kt)-cldeff(i,kt-1),0._r8) * a2l * a3l * latvap * ql(i,kt) / jt2slv
              evhc   = min( evhc, evhcmax )

          elseif( choice_evhc .eq. 'maxi' ) then

              qleff  = max( ql(i,kt-1), ql(i,kt) ) 
              jt2slv = slv(i,max(kt-2,1)) - slv(i,kt)
              jt2slv = max( jt2slv, jbumin*slv(i,kt-1)/g )
              evhc   = 1._r8 + a2l * a3l * latvap * qleff / jt2slv
              evhc   = min( evhc, evhcmax )

          endif

          
          
          
          
          
          

          lwp        = 0._r8
          opt_depth  = 0._r8
          radinvfrac = 0._r8 
          radf       = 0._r8

          if( choice_radf .eq. 'orig' ) then

              if( ql(i,kt) .gt. qmin .and. ql(i,kt-1) .lt. qmin ) then 

                  lwp       = ql(i,kt) * ( pi(i,kt+1) - pi(i,kt) ) / g
                  opt_depth = 156._r8 * lwp  

                  
                  

                  radinvfrac  = opt_depth * ( 4._r8 + opt_depth ) / ( 6._r8 * ( 4._r8 + opt_depth ) + opt_depth**2 )
                  radf        = qrlw(i,kt) / ( pi(i,kt) - pi(i,kt+1) ) 
                  radf        = max( radinvfrac * radf * ( zi(i,kt) - zi(i,kt+1) ), 0._r8 ) * chs(i,kt)
                
                

              end if

          elseif( choice_radf .eq. 'ramp' ) then

                  lwp         = ql(i,kt) * ( pi(i,kt+1) - pi(i,kt) ) / g
                  opt_depth   = 156._r8 * lwp  
                  radinvfrac  = opt_depth * ( 4._r8 + opt_depth ) / ( 6._r8 * ( 4._r8 + opt_depth ) + opt_depth**2 )
                  radinvfrac  = max(cldeff(i,kt)-cldeff(i,kt-1),0._r8) * radinvfrac 
                  radf        = qrlw(i,kt) / ( pi(i,kt) - pi(i,kt+1) ) 
                  radf        = max( radinvfrac * radf * ( zi(i,kt) - zi(i,kt+1) ), 0._r8 ) * chs(i,kt)

          elseif( choice_radf .eq. 'maxi' ) then

                
                
                  lwp         = ql(i,kt) * ( pi(i,kt+1) - pi(i,kt) ) / g
                  opt_depth   = 156._r8 * lwp  
                  radinvfrac  = opt_depth * ( 4._r8 + opt_depth ) / ( 6._r8 * ( 4._r8 + opt_depth ) + opt_depth**2 )
                  radf        = max( radinvfrac * qrlw(i,kt) / ( pi(i,kt) - pi(i,kt+1) ) * ( zi(i,kt) - zi(i,kt+1) ), 0._r8 )
                
                  lwp         = ql(i,kt-1) * ( pi(i,kt) - pi(i,kt-1) ) / g
                  opt_depth   = 156._r8 * lwp  
                  radinvfrac  = opt_depth * ( 4._r8 + opt_depth ) / ( 6._r8 * ( 4._r8 + opt_depth) + opt_depth**2 )
                  radf        = radf + max( radinvfrac * qrlw(i,kt-1) / ( pi(i,kt-1) - pi(i,kt) ) * ( zi(i,kt-1) - zi(i,kt) ), &
                                            0._r8 )
                  radf        = max( radf, 0._r8 ) * chs(i,kt) 

          endif

          
          
          
          
          
          
          
          
          
          
          
          
          
          
          
          
          
          
          
          

          dzht   = zi(i,kt)  - z(i,kt)     
          dzhb   = z(i,kb-1) - zi(i,kb)    
          wstar3 = radf * dzht
          do k = kt + 1, kb - 1 
               wstar3 =  wstar3 + bprod(i,k) * ( z(i,k-1) - z(i,k) )
             
             
             
             
             
             
          end do      
          if( kb .eq. (pver+1) .and. bflxs(i) .gt. 0._r8 ) then
             wstar3 = wstar3 + bflxs(i) * dzhb
           
          end if   
          wstar3 = max( 2.5_r8 * wstar3, 0._r8 )
   
          
          
          

          if( id_sedfact ) then
            
              sedfact = exp(-ased*wsedl(i,kt)/(wstar3**(1._r8/3._r8)+1.e-6))
              if( choice_evhc .eq. 'orig' ) then
                  if (ql(i,kt).gt.qmin .and. ql(i,kt-1).lt.qmin) then
                      jt2slv = slv(i,max(kt-2,1)) - slv(i,kt)
                      jt2slv = max(jt2slv, jbumin*slv(i,kt-1)/g)
                      evhc = 1._r8+sedfact*a2l*a3l*latvap*ql(i,kt) / jt2slv
                      evhc = min(evhc,evhcmax)
                  end if
              elseif( choice_evhc .eq. 'ramp' ) then
                  jt2slv = slv(i,max(kt-2,1)) - slv(i,kt)
                  jt2slv = max(jt2slv, jbumin*slv(i,kt-1)/g)
                  evhc = 1._r8+max(cldeff(i,kt)-cldeff(i,kt-1),0._r8)*sedfact*a2l*a3l*latvap*ql(i,kt) / jt2slv
                  evhc = min(evhc,evhcmax)
              elseif( choice_evhc .eq. 'maxi' ) then
                  qleff  = max(ql(i,kt-1),ql(i,kt))
                  jt2slv = slv(i,max(kt-2,1)) - slv(i,kt)
                  jt2slv = max(jt2slv, jbumin*slv(i,kt-1)/g)
                  evhc = 1._r8+sedfact*a2l*a3l*latvap*qleff / jt2slv
                  evhc = min(evhc,evhcmax)
              endif
          endif

          
          
          
          
          
          
          
          
          
          
          
          
          
          
          
          
          
          
          
          
          
          
          
          if( wstar3 .gt. 0._r8 ) then
              cet = a1i * evhc / ( jtbu * lbulk )
              if( kb .eq. pver + 1 ) then 
                  wstar3fact = max( 1._r8 + 2.5_r8 * cet * n2ht * jtzm * dzht, wstar3factcrit )
              else    
                  ceb = a1i / ( jbbu * lbulk )
                  wstar3fact = max( 1._r8 + 2.5_r8 * cet * n2ht * jtzm * dzht &
                                          + 2.5_r8 * ceb * n2hb * jbzm * dzhb, wstar3factcrit )
              end if
              wstar3 = wstar3 / wstar3fact       
          else 
              wstar3fact = 0._r8 
              cet        = 0._r8
              ceb        = 0._r8
          end if 

          
          
          
          
          
          
          
          
          
          
          
          
          
          
          
          
          
          
          
          
          
          
          
          
          
          
          
          
          
          
          
          
          
          
          
          
          
          
          
          

          fact = ( evhc * ( -vyt + vut ) * dzht + ( -vyb + vub ) * dzhb * leng(i,kb) / leng(i,kt) ) / lbulk

          if( wstarent ) then

              
              
              
              
              

              trma = 1._r8          
              trmp = ebrk(i,ncv) * ( lbrk(i,ncv) / lbulk ) / 3._r8 + ntzero
              trmq = 0.5_r8 * b1 * ( leng(i,kt)  / lbulk ) * ( radf * dzht + a1i * fact * wstar3 )

              
              
              

              rmin  = sqrt(trmp)
              fmin  = rmin * ( rmin * rmin - 3._r8 * trmp ) - 2._r8 * trmq
              wstar = wstar3**onet
              rcrit = ccrit * wstar
              fcrit = rcrit * ( rcrit * rcrit - 3._r8 * trmp ) - 2._r8 * trmq

              
              
              
              
              
              
              
              
              

              noroot = ( ( rmin .lt. rcrit ) .and. ( fcrit .gt. 0._r8 ) ) &
                  .or. ( ( rmin .ge. rcrit ) .and. ( fmin  .gt. 0._r8 ) )
              if( noroot ) then 
                  trma = 1._r8 - b1 * ( leng(i,kt) / lbulk ) * a1i * fact / ccrit**3
                  trma = max( trma, 0.5_r8 )  
                  trmp = trmp / trma 
                  trmq = 0.5_r8 * b1 * ( leng(i,kt) / lbulk ) * radf * dzht / trma
              end if   

              

              qq = trmq**2 - trmp**3
              if( qq .ge. 0._r8 ) then 
                  rootp = ( trmq + sqrt(qq) )**(1._r8/3._r8) + ( max( trmq - sqrt(qq), 0._r8 ) )**(1._r8/3._r8)
              else
                  rootp = 2._r8 * sqrt(trmp) * cos( acos( trmq / sqrt(trmp**3) ) / 3._r8 )
              end if
 
              
              

              if( noroot )  wstar3 = ( rootp / ccrit )**3     
              wet = cet * wstar3                              
              if( kb .lt. pver + 1 ) web = ceb * wstar3       

          else 

              
              
              
             
              trma = 1._r8 - b1 * a1l * fact
              trma = max( trma, 0.5_r8 )  
              trmp = ebrk(i,ncv) * ( lbrk(i,ncv) / lbulk ) / ( 3._r8 * trma )
              trmq = 0.5_r8 * b1 * ( leng(i,kt)  / lbulk ) * radf * dzht / trma

              qq = trmq**2 - trmp**3
              if( qq .ge. 0._r8 ) then 
                  rootp = ( trmq + sqrt(qq) )**(1._r8/3._r8) + ( max( trmq - sqrt(qq), 0._r8 ) )**(1._r8/3._r8)
              else 
                  rootp = 2._r8 * sqrt(trmp) * cos( acos( trmq / sqrt(trmp**3) ) / 3._r8 )
              end if   

             

              wet = a1l * rootp * min( evhc * rootp**2 / ( leng(i,kt) * jtbu ), 1._r8 )   
              if( kb .lt. pver + 1 ) web = a1l * rootp * min( evhc * rootp**2 / ( leng(i,kb) * jbbu ), 1._r8 )

          end if 

          
          
          

          ebrk(i,ncv) = rootp**2
          ebrk(i,ncv) = min(ebrk(i,ncv),tkemax) 
          wbrk(i,ncv) = ebrk(i,ncv)/b1  
        
          
          
          
          
          
          
          
          
          

          if( ebrk(i,ncv) .le. 0._r8 ) then
              write(iulog,*) 'CALEDDY: Warning, CL with zero TKE, i, kt, kb ', i, kt, kb
              call wrf_message(iulog)
              belongcv(i,kt) = .false.
              belongcv(i,kb) = .false. 
          end if
          
          
          
          
          
          
          
          
          
          
          
          
          
          

          
          do k = kb - 1, kt + 1, -1
             rcap = ( b1 * ae + wcap(i,k) / wbrk(i,ncv) ) / ( b1 * ae + 1._r8 )
             rcap = min( max(rcap,rcapmin), rcapmax )
             tke(i,k) = ebrk(i,ncv) * rcap
             tke(i,k) = min( tke(i,k), tkemax )
             kvh(i,k) = leng(i,k) * sqrt(tke(i,k)) * shcl(i,ncv)
             kvm(i,k) = leng(i,k) * sqrt(tke(i,k)) * smcl(i,ncv)
             bprod(i,k) = -kvh(i,k) * n2(i,k)
             sprod(i,k) =  kvm(i,k) * s2(i,k)
             turbtype(i,k) = 2                     
             sm_aw(i,k) = smcl(i,ncv)/alph1        
          end do

          
          kentr = wet * jtzm
          kvh(i,kt) = kentr
          kvm(i,kt) = kentr
          bprod(i,kt) = -kentr * n2ht + radf       
          sprod(i,kt) =  kentr * s2(i,kt)
          turbtype(i,kt) = 4                       
          trmp = -b1 * ae / ( 1._r8 + b1 * ae )
          trmq = -(bprod(i,kt)+sprod(i,kt))*b1*leng(i,kt)/(1._r8+b1*ae)/(ebrk(i,ncv)**(3._r8/2._r8))
          rcap = compute_cubic(0._r8,trmp,trmq)**2._r8
          rcap = min( max(rcap,rcapmin), rcapmax )
          tke(i,kt)  = ebrk(i,ncv) * rcap
          tke(i,kt)  = min( tke(i,kt), tkemax )
          sm_aw(i,kt) = smcl(i,ncv) / alph1        

          
          
          
          
          

          if( kb .lt. pver + 1 ) then 

              kentr = web * jbzm

              if( kb .ne. ktblw ) then

                  kvh(i,kb) = kentr
                  kvm(i,kb) = kentr
                  bprod(i,kb) = -kvh(i,kb)*n2hb     
                  sprod(i,kb) =  kvm(i,kb)*s2(i,kb)
                  turbtype(i,kb) = 3                
                  trmp = -b1*ae/(1._r8+b1*ae)
                  trmq = -(bprod(i,kb)+sprod(i,kb))*b1*leng(i,kb)/(1._r8+b1*ae)/(ebrk(i,ncv)**(3._r8/2._r8))
                  rcap = compute_cubic(0._r8,trmp,trmq)**2._r8
                  rcap = min( max(rcap,rcapmin), rcapmax )
                  tke(i,kb)  = ebrk(i,ncv) * rcap
                  tke(i,kb)  = min( tke(i,kb),tkemax )

              else
                  
                  kvh(i,kb) = kvh(i,kb) + kentr 
                  kvm(i,kb) = kvm(i,kb) + kentr
                
                
                  dzhb5 = z(i,kb-1) - zi(i,kb)
                  dzht5 = zi(i,kb) - z(i,kb)
                  bprod(i,kb) = ( dzht5*bprod(i,kb) - dzhb5*kentr*n2hb )     / ( dzhb5 + dzht5 )
                  sprod(i,kb) = ( dzht5*sprod(i,kb) + dzhb5*kentr*s2(i,kb) ) / ( dzhb5 + dzht5 )
                  trmp = -b1*ae/(1._r8+b1*ae)
                  trmq = -kentr*(s2(i,kb)-n2hb)*b1*leng(i,kb)/(1._r8+b1*ae)/(ebrk(i,ncv)**(3._r8/2._r8))
                  rcap = compute_cubic(0._r8,trmp,trmq)**2._r8
                  rcap = min( max(rcap,rcapmin), rcapmax )
                  tke_imsi = ebrk(i,ncv) * rcap
                  tke_imsi = min( tke_imsi, tkemax )
                  tke(i,kb)  = ( dzht5*tke(i,kb) + dzhb5*tke_imsi ) / ( dzhb5 + dzht5 )               
                  tke(i,kb)  = min(tke(i,kb),tkemax)
                  turbtype(i,kb) = 5                
                 
              end if

           else

             
             
             
 
             rcap = (b1*ae + wcap(i,kb)/wbrk(i,ncv))/(b1*ae + 1._r8)
             rcap = min( max(rcap,rcapmin), rcapmax )
             tke(i,kb) = ebrk(i,ncv) * rcap
             tke(i,kb) = min( tke(i,kb),tkemax )

          end if

          
          
          

          sm_aw(i,kb) = smcl(i,ncv)/alph1             

          
          
          
          
          
          
          
          wcap(i,kt) = (bprod(i,kt)+sprod(i,kt))*leng(i,kt)/sqrt(max(tke(i,kt),1.e-6_r8))
          if( kb .lt. pver + 1 ) then
              wcap(i,kb) = (bprod(i,kb)+sprod(i,kb))*leng(i,kb)/sqrt(max(tke(i,kb),1.e-6_r8))
          end if

          
          
          

          ktblw = kt 

          

          wet_CL(i,ncv)        = wet
          web_CL(i,ncv)        = web
          jtbu_CL(i,ncv)       = jtbu
          jbbu_CL(i,ncv)       = jbbu
          evhc_CL(i,ncv)       = evhc
          jt2slv_CL(i,ncv)     = jt2slv
          n2ht_CL(i,ncv)       = n2ht
          n2hb_CL(i,ncv)       = n2hb          
          lwp_CL(i,ncv)        = lwp
          opt_depth_CL(i,ncv)  = opt_depth
          radinvfrac_CL(i,ncv) = radinvfrac
          radf_CL(i,ncv)       = radf
          wstar_CL(i,ncv)      = wstar          
          wstar3fact_CL(i,ncv) = wstar3fact          

       end do        
 
       
       
       
       
       
       

       if( ncvsurf .gt. 0 ) then

           ktopbl(i) = ktop(i,ncvsurf)
           pblh(i)   = zi(i, ktopbl(i))
           pblhp(i)  = pi(i, ktopbl(i))
           wpert(i)  = max(wfac*sqrt(ebrk(i,ncvsurf)),wpertmin)
           tpert(i)  = max(abs(shflx(i)*rrho(i)/cpair)*tfac/wpert(i),0._r8)
           qpert(i)  = max(abs(qflx(i)*rrho(i))*tfac/wpert(i),0._r8)

           if( bflxs(i) .gt. 0._r8 ) then
               turbtype(i,pver+1) = 2 
           else
               turbtype(i,pver+1) = 3 
           endif

           ipbl(i)  = 1._r8
           kpblh(i) = ktopbl(i) - 1._r8

       end if 

       
       
       

       
       

       belongst(i,1) = .false.   
       do k = 2, pver            
          belongst(i,k) = ( ri(i,k) .lt. ricrit ) .and. ( .not. belongcv(i,k) )
          if( belongst(i,k) .and. ( .not. belongst(i,k-1) ) ) then
              kt = k             
          elseif( .not. belongst(i,k) .and. belongst(i,k-1) ) then
              kb = k - 1         
              lbulk = z(i,kt-1) - z(i,kb)
              do ks = kt, kb
                 if( choice_tunl .eq. 'rampcl' ) then
                     tunlramp = tunl
                 elseif( choice_tunl .eq. 'rampsl' ) then
                    tunlramp = max( 1.e-3_r8, ctunl * tunl * exp(-log(ctunl)*ri(i,ks)/ricrit) )
                  
                 else
                    tunlramp = tunl
                 endif
                 if( choice_leng .eq. 'origin' ) then
                     leng(i,ks) = ( (vk*zi(i,ks))**(-cleng) + (tunlramp*lbulk)**(-cleng) )**(-1._r8/cleng)
                   
                 else
                     leng(i,ks) = min( vk*zi(i,ks), tunlramp*lbulk )              
                 endif
              end do
          end if
       end do 

       
       
       
       
       
       
       belongst(i,pver+1) = .not. belongcv(i,pver+1)

       if( belongst(i,pver+1) ) then     

           turbtype(i,pver+1) = 1        
          
           if( belongst(i,pver) ) then   
             
               lbulk = z(i,kt-1)          
           else                          
               kt = pver+1
               lbulk = z(i,kt-1)
           end if

           
           
           
           

           ktopbl(i) = kt - 1
           pblh(i)   = z(i,ktopbl(i))
           pblhp(i)  = 0.5_r8 * ( pi(i,ktopbl(i)) + pi(i,ktopbl(i)+1) )          

           
           

           do ks = kt, pver
              if( choice_tunl .eq. 'rampcl' ) then
                  tunlramp = tunl
              elseif( choice_tunl .eq. 'rampsl' ) then
                  tunlramp = max(1.e-3_r8,ctunl*tunl*exp(-log(ctunl)*ri(i,ks)/ricrit))
                
              else
                  tunlramp = tunl
              endif
              if( choice_leng .eq. 'origin' ) then
                  leng(i,ks) = ( (vk*zi(i,ks))**(-cleng) + (tunlramp*lbulk)**(-cleng) )**(-1._r8/cleng)
                
              else
                  leng(i,ks) = min( vk*zi(i,ks), tunlramp*lbulk )              
              endif
           end do 

           
           

           wpert(i) = 0._r8 
           tpert(i) = max(shflx(i)*rrho(i)/cpair*fak/ustar(i),0._r8) 
           qpert(i) = max(qflx(i)*rrho(i)*fak/ustar(i),0._r8)

           ipbl(i)  = 0._r8
           kpblh(i) = ktopbl(i)

       end if

       
       
       
       
       
       
           
       do k = 2, pver

          if( belongst(i,k) ) then

              turbtype(i,k) = 1    
              trma = alph3*alph4exs*ri(i,k) + 2._r8*b1*(alph2-alph4exs*alph5*ri(i,k))
              trmb = (alph3+alph4exs)*ri(i,k) + 2._r8*b1*(-alph5*ri(i,k)+alph1)
              trmc = ri(i,k)
              det = max(trmb*trmb-4._r8*trma*trmc,0._r8)
              
              if( det .lt. 0._r8 ) then
                  write(iulog,*) 'The det < 0. for the STL in UW eddy_diff'             
                  call wrf_message(iulog)
                  stop
              end if                  
              gh = (-trmb + sqrt(det))/(2._r8*trma)
            
            
              gh = min(max(gh,ghmin),0.0233_r8)
              sh = max(0._r8,alph5/(1._r8+alph3*gh))
              sm = max(0._r8,(alph1 + alph2*gh)/(1._r8+alph3*gh)/(1._r8+alph4exs*gh))

              tke(i,k)   = b1*(leng(i,k)**2)*(-sh*n2(i,k)+sm*s2(i,k))
              tke(i,k)   = min(tke(i,k),tkemax)
              wcap(i,k)  = tke(i,k)/b1
              kvh(i,k)   = leng(i,k) * sqrt(tke(i,k)) * sh
              kvm(i,k)   = leng(i,k) * sqrt(tke(i,k)) * sm
              bprod(i,k) = -kvh(i,k) * n2(i,k)
              sprod(i,k) =  kvm(i,k) * s2(i,k)

              sm_aw(i,k) = sm/alph1     

          end if

       end do  

       
       
       

       
       
       
       
       
       
       
       
       

       

         do k = 2, pver

         if( ( turbtype(i,k) .eq. 3 ) .or. ( turbtype(i,k) .eq. 4 ) .or. &
             ( turbtype(i,k) .eq. 5 ) ) then

             trma = alph3*alph4exs*ri(i,k) + 2._r8*b1*(alph2-alph4exs*alph5*ri(i,k))
             trmb = (alph3+alph4exs)*ri(i,k) + 2._r8*b1*(-alph5*ri(i,k)+alph1)
             trmc = ri(i,k)
             det  = max(trmb*trmb-4._r8*trma*trmc,0._r8)
             gh   = (-trmb + sqrt(det))/(2._r8*trma)
           
           
             gh   = min(max(gh,ghmin),0.0233_r8)
             sh   = max(0._r8,alph5/(1._r8+alph3*gh))
             sm   = max(0._r8,(alph1 + alph2*gh)/(1._r8+alph3*gh)/(1._r8+alph4exs*gh))

             lbulk = z(i,k-1) - z(i,k)

             if( choice_tunl .eq. 'rampcl' ) then
                 tunlramp = tunl
             elseif( choice_tunl .eq. 'rampsl' ) then
                 tunlramp = max(1.e-3_r8,ctunl*tunl*exp(-log(ctunl)*ri(i,k)/ricrit))
               
             else
                 tunlramp = tunl
             endif
             if( choice_leng .eq. 'origin' ) then
                 leng_imsi = ( (vk*zi(i,k))**(-cleng) + (tunlramp*lbulk)**(-cleng) )**(-1._r8/cleng)
               
             else
                 leng_imsi = min( vk*zi(i,k), tunlramp*lbulk )              
             endif

             tke_imsi = b1*(leng_imsi**2)*(-sh*n2(i,k)+sm*s2(i,k))
             tke_imsi = min(max(tke_imsi,0._r8),tkemax)
             kvh_imsi = leng_imsi * sqrt(tke_imsi) * sh
             kvm_imsi = leng_imsi * sqrt(tke_imsi) * sm

             if( kvh(i,k) .lt. kvh_imsi ) then 
                 kvh(i,k)   =  kvh_imsi
                 kvm(i,k)   =  kvm_imsi
                 leng(i,k)  = leng_imsi
                 tke(i,k)   =  tke_imsi
                 wcap(i,k)  =  tke_imsi / b1
                 bprod(i,k) = -kvh_imsi * n2(i,k)
                 sprod(i,k) =  kvm_imsi * s2(i,k)
                 sm_aw(i,k) =  sm/alph1     
                 turbtype(i,k) = 1          
             endif

         end if

         end do

 

       
       
       

       

       
       
       
       
 
       
       
       
       
       
       ! starting from tkes(i) similar to the case of SRCL and SBCL in zisocl. !
       
       
       
       
       
       
       
       
       

       bprod(i,pver+1) = bflxs(i)
              
       gg = 0.5_r8*vk*z(i,pver)*bprod(i,pver+1)/(tkes(i)**(3._r8/2._r8))
       if( abs(alph5-gg*alph3) .le. 1.e-7_r8 ) then
         
           if( bprod(i,pver+1) .gt. 0._r8 ) then
               gh = -3.5334_r8
           else
               gh = ghmin
           endif
       else    
           gh = gg/(alph5-gg*alph3)
       end if 

     
       if( bprod(i,pver+1) .gt. 0._r8 ) then
           gh = min(max(gh,-3.5334_r8),0.0233_r8)
       else
           gh = min(max(gh,ghmin),0.0233_r8)
       endif

       gh_a(i,pver+1) = gh     
       sh_a(i,pver+1) = max(0._r8,alph5/(1._r8+alph3*gh))
       if( bprod(i,pver+1) .gt. 0._r8 ) then       
           sm_a(i,pver+1) = max(0._r8,(alph1+alph2*gh)/(1._r8+alph3*gh)/(1._r8+alph4*gh))
       else
           sm_a(i,pver+1) = max(0._r8,(alph1+alph2*gh)/(1._r8+alph3*gh)/(1._r8+alph4exs*gh))
       endif
       sm_aw(i,pver+1) = sm_a(i,pver+1)/alph1
       ri_a(i,pver+1)  = -(sm_a(i,pver+1)/sh_a(i,pver+1))*(bprod(i,pver+1)/sprod(i,pver+1))

       do k = 1, pver
          if( ri(i,k) .lt. 0._r8 ) then
              trma = alph3*alph4*ri(i,k) + 2._r8*b1*(alph2-alph4*alph5*ri(i,k))
              trmb = (alph3+alph4)*ri(i,k) + 2._r8*b1*(-alph5*ri(i,k)+alph1)
              trmc = ri(i,k)
              det  = max(trmb*trmb-4._r8*trma*trmc,0._r8)
              gh   = (-trmb + sqrt(det))/(2._r8*trma)
              gh   = min(max(gh,-3.5334_r8),0.0233_r8)
              gh_a(i,k) = gh
              sh_a(i,k) = max(0._r8,alph5/(1._r8+alph3*gh))
              sm_a(i,k) = max(0._r8,(alph1+alph2*gh)/(1._r8+alph3*gh)/(1._r8+alph4*gh))
              ri_a(i,k) = ri(i,k)
          else
              if( ri(i,k) .gt. ricrit ) then
                  gh_a(i,k) = ghmin
                  sh_a(i,k) = 0._r8
                  sm_a(i,k) = 0._r8
                  ri_a(i,k) = ri(i,k)
              else
                  trma = alph3*alph4exs*ri(i,k) + 2._r8*b1*(alph2-alph4exs*alph5*ri(i,k))
                  trmb = (alph3+alph4exs)*ri(i,k) + 2._r8*b1*(-alph5*ri(i,k)+alph1)
                  trmc = ri(i,k)
                  det  = max(trmb*trmb-4._r8*trma*trmc,0._r8)
                  gh   = (-trmb + sqrt(det))/(2._r8*trma)
                  gh   = min(max(gh,ghmin),0.0233_r8)
                  gh_a(i,k) = gh
                  sh_a(i,k) = max(0._r8,alph5/(1._r8+alph3*gh))
                  sm_a(i,k) = max(0._r8,(alph1+alph2*gh)/(1._r8+alph3*gh)/(1._r8+alph4exs*gh))
                  ri_a(i,k) = ri(i,k)
              endif
          endif

       end do

       do k = 1, pver + 1
          turbtype_f(i,k) = real(turbtype(i,k))
       end do

    end do   

    return

    end subroutine caleddy

    
    
    

    subroutine exacol( pcols, pver, ncol, ri, bflxs, minpblh, zi, ktop, kbase, ncvfin ) 

    
    
    
    
    
    
    

    implicit none

    
    
    

    integer,  intent(in) :: pcols                  
    integer,  intent(in) :: pver                   
    integer,  intent(in) :: ncol                   

    real(r8), intent(in) :: ri(pcols,pver)         
    real(r8), intent(in) :: bflxs(pcols)           
    real(r8), intent(in) :: minpblh(pcols)         
    real(r8), intent(in) :: zi(pcols,pver+1)       

    
    
    

    integer, intent(out) :: kbase(pcols,ncvmax)    
    integer, intent(out) :: ktop(pcols,ncvmax)     
    integer, intent(out) :: ncvfin(pcols)          

    
    
    

    integer              :: i
    integer              :: k
    integer              :: ncv
    real(r8)             :: rimaxentr
    real(r8)             :: riex(pver+1)           

    
    
    

    do i = 1, ncol
       ncvfin(i) = 0
       do ncv = 1, ncvmax
          ktop(i,ncv)  = 0
          kbase(i,ncv) = 0
       end do
    end do

    
    
    
    
    rimaxentr = 0._r8   
    
    do i = 1, ncol

       riex(2:pver) = ri(i,2:pver)

       
       

       riex(pver+1) = rimaxentr - bflxs(i) 

       ncv = 0
       k   = pver + 1 

       do while ( k .gt. ntop_turb + 1 )

        
        
       
          if( riex(k) .lt. rimaxentr ) then 

              

              ncv = ncv + 1

              
              

              kbase(i,ncv) = min(k+1,pver+1)

              

              do while( riex(k) .lt. rimaxentr .and. k .gt. ntop_turb + 1 )
                 k = k - 1
              end do

              
              

              ktop(i,ncv) = k
             
          else

              

              k = k - 1

          end if

       end do 

       ncvfin(i) = ncv    

    end do  

    return 

    end subroutine exacol

    
    
    
    
    subroutine zisocl( pcols  , pver  , long ,                                 & 
                       z      , zi    , n2   ,  s2      ,                      & 
                       bprod  , sprod , bflxs,  tkes    ,                      & 
                       ncvfin , kbase , ktop ,  belongcv,                      & 
                       ricl   , ghcl  , shcl ,  smcl    ,                      &
                       lbrk   , wbrk  , ebrk ,  extend  , extend_up, extend_dn )

    
    ! Object : This 'zisocl' vertically extends original CLs identified from  !
    
    
    
    
    
    
    
    
    
    
    

    implicit none

    
    
    

    integer,  intent(in)   :: long                    
    integer,  intent(in)   :: pcols                   
    integer,  intent(in)   :: pver                    
    real(r8), intent(in)   :: z(pcols, pver)          
    real(r8), intent(in)   :: zi(pcols, pver+1)       
    real(r8), intent(in)   :: n2(pcols, pver)         
    real(r8), intent(in)   :: s2(pcols, pver)         
    real(r8), intent(in)   :: bprod(pcols,pver+1)     
    real(r8), intent(in)   :: sprod(pcols,pver+1)     
    real(r8), intent(in)   :: bflxs(pcols)            
    real(r8), intent(in)   :: tkes(pcols)             

    
    
    

    integer, intent(inout) :: kbase(pcols,ncvmax)     
    integer, intent(inout) :: ktop(pcols,ncvmax)      
    integer, intent(inout) :: ncvfin(pcols)           

    
    
    

    logical,  intent(out) :: belongcv(pcols,pver+1)   
    real(r8), intent(out) :: ricl(pcols,ncvmax)       
    real(r8), intent(out) :: ghcl(pcols,ncvmax)       
    real(r8), intent(out) :: shcl(pcols,ncvmax)       
    real(r8), intent(out) :: smcl(pcols,ncvmax)       
    real(r8), intent(out) :: lbrk(pcols,ncvmax)       
    real(r8), intent(out) :: wbrk(pcols,ncvmax)       
    real(r8), intent(out) :: ebrk(pcols,ncvmax)       

    
    
    

    logical               :: extend                   ! True when CL is extended in zisocl
    logical               :: extend_up                ! True when CL is extended upward in zisocl
    logical               :: extend_dn                ! True when CL is extended downward in zisocl
    logical               :: bottom                   

    integer               :: i                        
    integer               :: ncv                      
    integer               :: incv
    integer               :: k
    integer               :: kb                       
    integer               :: kt                       
    integer               :: ncvinit                  
    integer               :: cntu                     
    integer               :: cntd                     
    integer               :: kbinc                    
    integer               :: ktinc                    

    real(r8)              :: wint                     
    real(r8)              :: dwinc                    
    real(r8)              :: dw_surf                  
    real(r8)              :: dzinc
    real(r8)              :: gh
    real(r8)              :: sh
    real(r8)              :: sm
    real(r8)              :: gh_surf                  
    real(r8)              :: sh_surf                  
    real(r8)              :: sm_surf                  
    real(r8)              :: l2n2                     
    real(r8)              :: l2s2                     
    real(r8)              :: dl2n2                    
    real(r8)              :: dl2s2                    
    real(r8)              :: dl2n2_surf               
    real(r8)              :: dl2s2_surf               
    real(r8)              :: lint                     
    real(r8)              :: dlint                    
    real(r8)              :: dlint_surf               
    real(r8)              :: lbulk                    
    real(r8)              :: lz                       
    real(r8)              :: ricll                    
    real(r8)              :: trma
    real(r8)              :: trmb
    real(r8)              :: trmc
    real(r8)              :: det
    real(r8)              :: zbot                     
    real(r8)              :: l2rat                    
    real(r8)              :: gg                       
    real(r8)              :: tunlramp                 

    
    
    

    i = long

    
    
    do k = 1, ncvmax
       ricl(i,k) = 0._r8
       ghcl(i,k) = 0._r8
       shcl(i,k) = 0._r8
       smcl(i,k) = 0._r8
       lbrk(i,k) = 0._r8
       wbrk(i,k) = 0._r8
       ebrk(i,k) = 0._r8
    end do
    extend    = .false.
    extend_up = .false.
    extend_dn = .false.

    
    
    

    ncv = 1

    do while( ncv .le. ncvfin(i) )

       ncvinit = ncv
       cntu    = 0
       cntd    = 0
       kb      = kbase(i,ncv) 
       kt      = ktop(i,ncv)
       
       
       
       
       
       
       
       
       
       
       
       
       
       
       
       
       
       
       
       
       
       
       
       

       lbulk      = zi(i,kt) - zi(i,kb)
       dlint_surf = 0._r8
       dl2n2_surf = 0._r8
       dl2s2_surf = 0._r8
       dw_surf    = 0._r8
       if( kb .eq. pver+1 ) then

           if( bflxs(i) .gt. 0._r8 ) then

               
               
               
               

               gg    = 0.5_r8*vk*z(i,pver)*bprod(i,pver+1)/(tkes(i)**(3._r8/2._r8))
               gh    = gg/(alph5-gg*alph3)
             
               gh    = min(max(gh,-3.5334_r8),0.0233_r8)
               sh    = alph5/(1._r8+alph3*gh)
               sm    = (alph1 + alph2*gh)/(1._r8+alph3*gh)/(1._r8+alph4*gh)
               ricll = min(-(sm/sh)*(bprod(i,pver+1)/sprod(i,pver+1)),ricrit)

               
               
               
               
               

               dlint_surf = z(i,pver)
               dl2n2_surf = -vk*(z(i,pver)**2)*bprod(i,pver+1)/(sh*sqrt(tkes(i)))
               dl2s2_surf =  vk*(z(i,pver)**2)*sprod(i,pver+1)/(sm*sqrt(tkes(i)))
               dw_surf    = (tkes(i)/b1)*z(i,pver) 

           else

               
               
               lbulk = zi(i,kt) - z(i,pver)

           end if

       end if
           
       
       
       
       
       lint = dlint_surf
       l2n2 = dl2n2_surf
       l2s2 = dl2s2_surf          
       wint = dw_surf
       if( use_dw_surf ) then
           l2n2 = 0._r8
           l2s2 = 0._r8
       else
           wint = 0._r8
       end if    
       
       
       
       
       
       if( kt .lt. kb - 1 ) then 
                              
           do k = kb - 1, kt + 1, -1       
              if( choice_tunl .eq. 'rampcl' ) then
                
                  tunlramp = 0.5_r8*(1._r8+ctunl)*tunl
              elseif( choice_tunl .eq. 'rampsl' ) then
                  tunlramp = ctunl*tunl
                
              else
                  tunlramp = tunl
              endif
              if( choice_leng .eq. 'origin' ) then
                  lz = ( (vk*zi(i,k))**(-cleng) + (tunlramp*lbulk)**(-cleng) )**(-1._r8/cleng)
                
              else
                  lz = min( vk*zi(i,k), tunlramp*lbulk )              
              endif
              dzinc = z(i,k-1) - z(i,k)
              l2n2  = l2n2 + lz*lz*n2(i,k)*dzinc
              l2s2  = l2s2 + lz*lz*s2(i,k)*dzinc
              lint  = lint + dzinc
           end do

           
           

         
         

           ricll = min(l2n2/max(l2s2,ntzero),ricrit) 
           trma  = alph3*alph4*ricll+2._r8*b1*(alph2-alph4*alph5*ricll)
           trmb  = ricll*(alph3+alph4)+2._r8*b1*(-alph5*ricll+alph1)
           trmc  = ricll
           det   = max(trmb*trmb-4._r8*trma*trmc,0._r8)
           gh    = (-trmb + sqrt(det))/2._r8/trma
         
           gh    = min(max(gh,-3.5334_r8),0.0233_r8)
           sh    = alph5/(1._r8+alph3*gh)
           sm    = (alph1 + alph2*gh)/(1._r8+alph3*gh)/(1._r8+alph4*gh)
           wint  = wint - sh*l2n2 + sm*l2s2 

       else 
 
           
           
           
           
           
           
       
           lint = dlint_surf
           l2n2 = dl2n2_surf
           l2s2 = dl2s2_surf 
           wint = dw_surf

           
           
           
           
           
           
           
           
           
           
           
           

           if( choice_tkes .eq. 'ebprod' ) then
               l2n2 = - wint / sh 
           endif
           
       endif
           
       
       

       l2n2 = -min(-l2n2, tkemax*lint/(b1*sh))
       l2s2 =  min( l2s2, tkemax*lint/(b1*sm))
       
       
       
       
       
       
       
       
       
       
       
       
       
       
       
       
       
       
       
       
       
       
       
       
       
       
       
       
       
       extend = .false.    

       

       if( choice_tunl .eq. 'rampcl' ) then
           tunlramp = 0.5_r8*(1._r8+ctunl)*tunl
       elseif( choice_tunl .eq. 'rampsl' ) then
           tunlramp = ctunl*tunl
         
       else
           tunlramp = tunl
       endif
       if( choice_leng .eq. 'origin' ) then
           lz = ( (vk*zi(i,kt))**(-cleng) + (tunlramp*lbulk)**(-cleng) )**(-1._r8/cleng)
         
       else
           lz = min( vk*zi(i,kt), tunlramp*lbulk )              
       endif

       dzinc = z(i,kt-1)-z(i,kt)
       dl2n2 = lz*lz*n2(i,kt)*dzinc
       dl2s2 = lz*lz*s2(i,kt)*dzinc
       dwinc = -sh*dl2n2 + sm*dl2s2

       
       
       
 
     
     
       do while ( -dl2n2 .gt. (-rinc*l2n2/(1._r8-rinc)) .and. kt-1 .gt. ntop_turb )                     

          
          
          
          
          
          
          

          lint = lint + dzinc
          l2n2 = l2n2 + dl2n2
          l2n2 = -min(-l2n2, tkemax*lint/(b1*sh))
          l2s2 = l2s2 + dl2s2
          wint = wint + dwinc

          

          kt        = kt - 1
          extend    = .true.
          extend_up = .true.
          if( kt .eq. ntop_turb ) then
              write(iulog,*) 'zisocl: Error: Tried to extend CL to the model top'
              call wrf_message(iulog)
              stop
          end if

          
          
          
          
          
 
          ktinc = kbase(i,ncv+cntu+1) - 1  

          if( kt .eq. ktinc ) then

              do k = kbase(i,ncv+cntu+1) - 1, ktop(i,ncv+cntu+1) + 1, -1

                 if( choice_tunl .eq. 'rampcl' ) then
                     tunlramp = 0.5_r8*(1._r8+ctunl)*tunl
                 elseif( choice_tunl .eq. 'rampsl' ) then
                     tunlramp = ctunl*tunl
                   
                 else
                     tunlramp = tunl
                 endif
                 if( choice_leng .eq. 'origin' ) then
                     lz = ( (vk*zi(i,k))**(-cleng) + (tunlramp*lbulk)**(-cleng) )**(-1._r8/cleng)
                   
                 else
                     lz = min( vk*zi(i,k), tunlramp*lbulk )              
                 endif

                 dzinc = z(i,k-1)-z(i,k)
                 dl2n2 = lz*lz*n2(i,k)*dzinc
                 dl2s2 = lz*lz*s2(i,k)*dzinc
                 dwinc = -sh*dl2n2 + sm*dl2s2

                 lint = lint + dzinc
                 l2n2 = l2n2 + dl2n2
                 l2n2 = -min(-l2n2, tkemax*lint/(b1*sh))
                 l2s2 = l2s2 + dl2s2
                 wint = wint + dwinc

              end do 

              kt        = ktop(i,ncv+cntu+1) 
              ncvfin(i) = ncvfin(i) - 1
              cntu      = cntu + 1
        
          end if

          
          

          if( choice_tunl .eq. 'rampcl' ) then
              tunlramp = 0.5_r8*(1._r8+ctunl)*tunl
          elseif( choice_tunl .eq. 'rampsl' ) then
              tunlramp = ctunl*tunl
            
          else
              tunlramp = tunl
          endif
          if( choice_leng .eq. 'origin' ) then
              lz = ( (vk*zi(i,kt))**(-cleng) + (tunlramp*lbulk)**(-cleng) )**(-1._r8/cleng)
            
          else
              lz = min( vk*zi(i,kt), tunlramp*lbulk )              
          endif

          dzinc = z(i,kt-1)-z(i,kt)
          dl2n2 = lz*lz*n2(i,kt)*dzinc
          dl2s2 = lz*lz*s2(i,kt)*dzinc
          dwinc = -sh*dl2n2 + sm*dl2s2

       end do   

       
       
       
       
       

       if( cntu .gt. 0 ) then
           do incv = 1, ncvfin(i) - ncv
              kbase(i,ncv+incv) = kbase(i,ncv+cntu+incv)
              ktop(i,ncv+incv)  = ktop(i,ncv+cntu+incv)
           end do
       end if

       
       
       
       
       if( kb .ne. pver + 1 ) then

           

           if( choice_tunl .eq. 'rampcl' ) then
               tunlramp = 0.5_r8*(1._r8+ctunl)*tunl
           elseif( choice_tunl .eq. 'rampsl' ) then
               tunlramp = ctunl*tunl
             
           else
               tunlramp = tunl
           endif
           if( choice_leng .eq. 'origin' ) then
               lz = ( (vk*zi(i,kb))**(-cleng) + (tunlramp*lbulk)**(-cleng) )**(-1._r8/cleng)
             
           else
               lz = min( vk*zi(i,kb), tunlramp*lbulk )              
           endif

           dzinc = z(i,kb-1)-z(i,kb)
           dl2n2 = lz*lz*n2(i,kb)*dzinc
           dl2s2 = lz*lz*s2(i,kb)*dzinc
           dwinc = -sh*dl2n2 + sm*dl2s2

           
           
           

           
           
           

         
         
         
           do while( ( -dl2n2 .gt. (-rinc*l2n2/(1._r8-rinc)) ) &                     
                       .and.(kb.ne.pver+1))

              

              lint = lint + dzinc
              l2n2 = l2n2 + dl2n2
              l2n2 = -min(-l2n2, tkemax*lint/(b1*sh))
              l2s2 = l2s2 + dl2s2
              wint = wint + dwinc

              

              kb        =  kb + 1
              extend    = .true.
              extend_dn = .true.

              
              
              
              
              
              
              
              
              
              

              kbinc = 0
              if( ncv .gt. 1 ) kbinc = ktop(i,ncv-1) + 1
              if( kb .eq. kbinc ) then

                  do k =  ktop(i,ncv-1) + 1, kbase(i,ncv-1) - 1

                     if( choice_tunl .eq. 'rampcl' ) then
                         tunlramp = 0.5_r8*(1._r8+ctunl)*tunl
                     elseif( choice_tunl .eq. 'rampsl' ) then
                         tunlramp = ctunl*tunl
                       
                     else
                         tunlramp = tunl
                     endif
                     if( choice_leng .eq. 'origin' ) then
                         lz = ( (vk*zi(i,k))**(-cleng) + (tunlramp*lbulk)**(-cleng) )**(-1._r8/cleng)
                       
                     else
                         lz = min( vk*zi(i,k), tunlramp*lbulk )              
                     endif

                     dzinc = z(i,k-1)-z(i,k)
                     dl2n2 = lz*lz*n2(i,k)*dzinc
                     dl2s2 = lz*lz*s2(i,k)*dzinc
                     dwinc = -sh*dl2n2 + sm*dl2s2

                     lint = lint + dzinc
                     l2n2 = l2n2 + dl2n2
                     l2n2 = -min(-l2n2, tkemax*lint/(b1*sh))
                     l2s2 = l2s2 + dl2s2
                     wint = wint + dwinc

                  end do 

                  
                  

                  kb        = kbase(i,ncv-1)
                  ncv       = ncv - 1
                  ncvfin(i) = ncvfin(i) -1
                  cntd      = cntd + 1

              end if

              
              
              
             
              if( kb .eq. pver + 1 ) then 

                  if( bflxs(i) .gt. 0._r8 ) then 
                      
                      gg = 0.5_r8*vk*z(i,pver)*bprod(i,pver+1)/(tkes(i)**(3._r8/2._r8))
                      gh_surf = gg/(alph5-gg*alph3)
                    
                      gh_surf = min(max(gh_surf,-3.5334_r8),0.0233_r8)
                      sh_surf = alph5/(1._r8+alph3*gh_surf)
                      sm_surf = (alph1 + alph2*gh_surf)/(1._r8+alph3*gh_surf)/(1._r8+alph4*gh_surf)
                      
                      
                      dlint_surf = z(i,pver)
                      dl2n2_surf = -vk*(z(i,pver)**2._r8)*bprod(i,pver+1)/(sh_surf*sqrt(tkes(i)))
                      dl2s2_surf =  vk*(z(i,pver)**2._r8)*sprod(i,pver+1)/(sm_surf*sqrt(tkes(i)))
                      dw_surf = (tkes(i)/b1)*z(i,pver) 
                  else
                      dlint_surf = 0._r8
                      dl2n2_surf = 0._r8
                      dl2s2_surf = 0._r8
                      dw_surf = 0._r8
                  end if
                  
                  
                  
                  
                  
                  
                  
                  lint = lint + dlint_surf
                  l2n2 = l2n2 + dl2n2_surf
                  l2n2 = -min(-l2n2, tkemax*lint/(b1*sh))
                  l2s2 = l2s2 + dl2s2_surf 
                  wint = wint + dw_surf                
                
              else

                  if( choice_tunl .eq. 'rampcl' ) then
                      tunlramp = 0.5_r8*(1._r8+ctunl)*tunl
                  elseif( choice_tunl .eq. 'rampsl' ) then
                      tunlramp = ctunl*tunl
                    
                  else
                      tunlramp = tunl
                  endif
                  if( choice_leng .eq. 'origin' ) then
                      lz = ( (vk*zi(i,kb))**(-cleng) + (tunlramp*lbulk)**(-cleng) )**(-1._r8/cleng)
                    
                  else
                      lz = min( vk*zi(i,kb), tunlramp*lbulk )              
                  endif

                  dzinc = z(i,kb-1)-z(i,kb)
                  dl2n2 = lz*lz*n2(i,kb)*dzinc
                  dl2s2 = lz*lz*s2(i,kb)*dzinc
                  dwinc = -sh*dl2n2 + sm*dl2s2

              end if

          end do 

          if( (kb.eq.pver+1) .and. (ncv.ne.1) ) then 
               write(iulog,*) 'Major mistake zisocl: the CL based at surface is not indexed 1'
               call wrf_message(iulog)
               stop
          end if

       end if   

       
       
       
       
       

       if( cntd .gt. 0 ) then
           do incv = 1, ncvfin(i) - ncv
              kbase(i,ncv+incv) = kbase(i,ncvinit+incv)
              ktop(i,ncv+incv)  = ktop(i,ncvinit+incv)
           end do
       end if

       

       if( wint .lt. 0.01_r8 ) then
           wint = 0.01_r8
       end if

       
       
       
       
       
       
       
       
       

       if( extend ) then

           ktop(i,ncv)  = kt
           kbase(i,ncv) = kb

           
           
           
          
           lbulk      = zi(i,kt) - zi(i,kb)
           dlint_surf = 0._r8
           dl2n2_surf = 0._r8
           dl2s2_surf = 0._r8
           dw_surf    = 0._r8
           if( kb .eq. pver + 1 ) then
               if( bflxs(i) .gt. 0._r8 ) then
                   
                   gg = 0.5_r8*vk*z(i,pver)*bprod(i,pver+1)/(tkes(i)**(3._r8/2._r8))
                   gh = gg/(alph5-gg*alph3)
                 
                   gh = min(max(gh,-3.5334_r8),0.0233_r8)
                   sh = alph5/(1._r8+alph3*gh)
                   sm = (alph1 + alph2*gh)/(1._r8+alph3*gh)/(1._r8+alph4*gh)
                   
                   
                   dlint_surf = z(i,pver)
                   dl2n2_surf = -vk*(z(i,pver)**2._r8)*bprod(i,pver+1)/(sh*sqrt(tkes(i)))
                   dl2s2_surf =  vk*(z(i,pver)**2._r8)*sprod(i,pver+1)/(sm*sqrt(tkes(i)))
                   dw_surf    = (tkes(i)/b1)*z(i,pver) 
               else
                   lbulk = zi(i,kt) - z(i,pver)
               end if
           end if
           lint = dlint_surf
           l2n2 = dl2n2_surf
           l2s2 = dl2s2_surf
           wint = dw_surf
           if( use_dw_surf ) then
               l2n2 = 0._r8
               l2s2 = 0._r8
           else
               wint = 0._r8
           end if   
       
           
           
           
          
           do k = kt + 1, kb - 1
              if( choice_tunl .eq. 'rampcl' ) then
                  tunlramp = 0.5_r8*(1._r8+ctunl)*tunl
              elseif( choice_tunl .eq. 'rampsl' ) then
                  tunlramp = ctunl*tunl
                
              else
                  tunlramp = tunl
              endif
              if( choice_leng .eq. 'origin' ) then
                  lz = ( (vk*zi(i,k))**(-cleng) + (tunlramp*lbulk)**(-cleng) )**(-1._r8/cleng)
                
              else
                  lz = min( vk*zi(i,k), tunlramp*lbulk )              
              endif
              dzinc = z(i,k-1) - z(i,k)
              lint = lint + dzinc
              l2n2 = l2n2 + lz*lz*n2(i,k)*dzinc
              l2s2 = l2s2 + lz*lz*s2(i,k)*dzinc
           end do

           ricll = min(l2n2/max(l2s2,ntzero),ricrit)
           trma = alph3*alph4*ricll+2._r8*b1*(alph2-alph4*alph5*ricll)
           trmb = ricll*(alph3+alph4)+2._r8*b1*(-alph5*ricll+alph1)
           trmc = ricll
           det = max(trmb*trmb-4._r8*trma*trmc,0._r8)
           gh = (-trmb + sqrt(det))/2._r8/trma
         
           gh = min(max(gh,-3.5334_r8),0.0233_r8)
           sh = alph5 / (1._r8+alph3*gh)
           sm = (alph1 + alph2*gh)/(1._r8+alph3*gh)/(1._r8+alph4*gh)
           
           
           
           wint = max( wint - sh*l2n2 + sm*l2s2, 0.01_r8 )

       end if

       
       
       

       lbrk(i,ncv) = lint
       wbrk(i,ncv) = wint/lint
       ebrk(i,ncv) = b1*wbrk(i,ncv)
       ebrk(i,ncv) = min(ebrk(i,ncv),tkemax)
       ricl(i,ncv) = ricll 
       ghcl(i,ncv) = gh 
       shcl(i,ncv) = sh
       smcl(i,ncv) = sm

       
       
       
       

       ncv = ncv + 1

    end do                   

    
    
    

    do ncv = ncvfin(i) + 1, ncvmax
       ktop(i,ncv)  = 0
       kbase(i,ncv) = 0
    end do

    
    
    
    

    do k = 1, pver + 1
       belongcv(i,k) = .false.
    end do

    do ncv = 1, ncvfin(i)
       do k = ktop(i,ncv), kbase(i,ncv)
          belongcv(i,k) = .true.
       end do
    end do

    return

    end subroutine zisocl

    real(r8) function compute_cubic(a,b,c)
    
    
    
    
    implicit none
    real(r8), intent(in)     :: a, b, c
    real(r8)  qq, rr, dd, theta, aa, bb, x1, x2, x3
    real(r8), parameter      :: xmin = 1.e-2_r8
    
    qq = (a**2-3._r8*b)/9._r8 
    rr = (2._r8*a**3 - 9._r8*a*b + 27._r8*c)/54._r8
    
    dd = rr**2 - qq**3
    if( dd .le. 0._r8 ) then
        theta = acos(rr/qq**(3._r8/2._r8))
        x1 = -2._r8*sqrt(qq)*cos(theta/3._r8) - a/3._r8
        x2 = -2._r8*sqrt(qq)*cos((theta+2._r8*3.141592)/3._r8) - a/3._r8
        x3 = -2._r8*sqrt(qq)*cos((theta-2._r8*3.141592)/3._r8) - a/3._r8
        compute_cubic = max(max(max(x1,x2),x3),xmin)        
        return
    else
        if( rr .ge. 0._r8 ) then
            aa = -(sqrt(rr**2-qq**3)+rr)**(1._r8/3._r8)
        else
            aa =  (sqrt(rr**2-qq**3)-rr)**(1._r8/3._r8)
        endif
        if( aa .eq. 0._r8 ) then
            bb = 0._r8
        else
            bb = qq/aa
        endif
        compute_cubic = max((aa+bb)-a/3._r8,xmin) 
        return
    endif

    return
    end function compute_cubic

END MODULE eddy_diff
