





















module module_mp_cammgmp_driver
  
  
  
  
  
  
  
  
  
  
  
  
  use shr_kind_mod,  only: r8=>shr_kind_r8
  use physconst,     only: gravit  
  use module_cam_support,  only: pcnst =>pcnst_mp, pcols, pver, pverp



  use constituents,  only: cnst_get_ind, cnst_name, qmin







  
  implicit none
  private
  save
  
  public :: CAMMGMP_INIT
  public :: CAMMGMP




  
  
  
  
  
  
  
  
  
  
  character(len=5), private, parameter :: micro_treatment = 'inter' 
  
  logical, private :: sub_column = .false. 
  
  
  
  
  
  
  integer :: &
       ixcldliq,     &
       ixcldice,     &
       ixnumliq,     &
       ixnumice       
  integer  :: chem_opt, ptend_top_level, ptend_bot_level

  real(r8), parameter :: qthresh_mass = 1.0e-11 
  real(r8), parameter :: qthresh_numl = 5.0e2   
  real(r8), parameter :: qthresh_numi = 1.0e-1  

  
  real(r8), parameter :: dp1 = 0.10_r8 
  real(r8), parameter :: dp2 = 500.0_r8

contains
  
  subroutine CAMMGMP(itimestep, dt, p8w, p_hyd   &
       , t_phy, pi_phy, z_at_w, qfx              &
       , tke_pbl, turbtype3d, smaw3d             &
       , dlf3d, dlf2_3d, rliq2d, z_sea_level     &
       , kvh3d, ht, alt, accum_mode              &
       , aitken_mode, coarse_mode, icwmrsh3d     &
       , icwmrdp3d, shfrc3d, cmfmc3d, cmfmc2_3d  &
       , config_flags, f_ice_phy, f_rain_phy     &



       , ids, ide,  jds, jde,  kds, kde          &
       , ims, ime,  jms, jme,  kms, kme          &
       , its, ite,  jts, jte,  kts, kte          &
       
       , th, cldfra_old_mp, cldfra_mp            &
       ,cldfra_mp_all, lradius, iradius, cldfrai &
       , cldfral, cldfra_conv,wsedl3d, rainnc    &
       , rainncv, snownc, snowncv,sr             &
       , qv_curr, qc_curr, qi_curr,qs_curr       &
       , qr_curr, nc3d, ni3d,ns3d,nr3d,qndrop    &
       , rh_old_mp,lcd_old_mp                    & 





       , xland,snowh                            )
    
    
    
    
    
    
    
    
    
    
    
    
    
    use shr_kind_mod,             only: r8 => shr_kind_r8
    use cldwat2m_micro,           only: mmicro_pcond
    use microp_aero,              only: microp_aero_ts 
    use physconst,                only: cpair, tmelt 
    

    use modal_aero_data,          only: modeptr_accum => modeptr_accum_mp, &
         numptr_amode   => numptr_amode_mp  , lspectype_amode => lspectype_amode_mp, &
         specdens_amode => specdens_amode_mp, voltonumb_amode => voltonumb_amode_mp, &
         lmassptr_amode => lmassptr_amode_mp, modeptr_aitken  => modeptr_aitken_mp,  &
         modeptr_coarse => modeptr_coarse_mp, ntot_amode      => ntot_amode_mp,      &
         nspec_amode    => nspec_amode_mp

    



    
    use wv_saturation,             only: epsqs, polysvp 
    use conv_water,                only: conv_water_4rad
    use cldwat,                    only: cldwat_fice
    use module_state_description,  only: CAMZMSCHEME, CAMUWSHCUSCHEME,F_QV, F_QC, F_QI, F_QS
    use module_configure,          only: grid_config_rec_type
    
    implicit none    
    
    
    
    TYPE(grid_config_rec_type),  INTENT(IN   )    :: config_flags
    integer, intent(in) :: itimestep       
    
    integer, intent(in) :: ids,ide, jds,jde, kds,kde
    integer, intent(in) :: ims,ime, jms,jme, kms,kme
    integer, intent(in) :: its,ite, jts,jte, kts,kte
    
    real,    intent(in) :: dt              
    
    real,    intent(in) :: accum_mode, aitken_mode, coarse_mode     
    
    real, dimension( ims:ime, jms:jme ), intent(in) :: ht     
    real, dimension( ims:ime, jms:jme ), intent(in) :: qfx    
    real, dimension( ims:ime, jms:jme ), intent(in) :: rliq2d 
    real, dimension( ims:ime, jms:jme ), intent(in) :: xland
    real, dimension( ims:ime, jms:jme ), intent(in) :: snowh
    
    real, dimension( ims:ime, kms:kme, jms:jme ), intent(in)    :: dlf3d       
    real, dimension( ims:ime, kms:kme, jms:jme ), intent(in)    :: dlf2_3d     
    real, dimension( ims:ime, kms:kme, jms:jme ), intent(in)    :: p8w         
    real, dimension( ims:ime, kms:kme, jms:jme ), intent(in)    :: p_hyd       
    real, dimension( ims:ime, kms:kme, jms:jme ), intent(in)    :: z_sea_level 
    real, dimension( ims:ime, kms:kme, jms:jme ), intent(in)    :: t_phy       
    real, dimension( ims:ime, kms:kme, jms:jme ), intent(in)    :: z_at_w      
    real, dimension( ims:ime, kms:kme, jms:jme ), intent(in)    :: pi_phy      
    real, dimension( ims:ime, kms:kme, jms:jme ), intent(in)    :: tke_pbl     
    real, dimension( ims:ime, kms:kme, jms:jme ), intent(in)    :: kvh3d       
    real, dimension( ims:ime, kms:kme, jms:jme ), intent(in)    :: turbtype3d  
    real, dimension( ims:ime, kms:kme, jms:jme ), intent(in)    :: smaw3d      
    real, dimension( ims:ime, kms:kme, jms:jme ), intent(in)    :: alt         
    real, dimension( ims:ime, kms:kme, jms:jme ), intent(in)    :: F_ICE_PHY   
    real, dimension( ims:ime, kms:kme, jms:jme ), intent(in)    :: F_RAIN_PHY  
    
    real, dimension( ims:ime, kms:kme, jms:jme ), intent(in) :: shfrc3d        
    real, dimension( ims:ime, kms:kme, jms:jme ), intent(in) :: cmfmc3d        
    real, dimension( ims:ime, kms:kme, jms:jme ), intent(in) :: cmfmc2_3d      
    real, dimension( ims:ime, kms:kme, jms:jme ), intent(in) :: icwmrsh3d      
    real, dimension( ims:ime, kms:kme, jms:jme ), intent(in) :: icwmrdp3d      




    
    real, dimension( ims:ime , jms:jme ), intent(inout) :: rainnc              
    real, dimension( ims:ime , jms:jme ), intent(inout) :: rainncv             
    real, dimension( ims:ime , jms:jme ), intent(inout) :: snownc              
    real, dimension( ims:ime , jms:jme ), intent(inout) :: snowncv             
    
    
    real, dimension( ims:ime, kms:kme, jms:jme ), intent(inout) :: cldfra_old_mp  
    real, dimension( ims:ime, kms:kme, jms:jme ), intent(inout) :: rh_old_mp      
    real, dimension( ims:ime, kms:kme, jms:jme ), intent(inout) :: lcd_old_mp     
    real, dimension( ims:ime, kms:kme, jms:jme ), intent(inout) :: qv_curr        
    real, dimension( ims:ime, kms:kme, jms:jme ), intent(inout) :: qc_curr        
    real, dimension( ims:ime, kms:kme, jms:jme ), intent(inout) :: qi_curr        
    real, dimension( ims:ime, kms:kme, jms:jme ), intent(inout) :: qs_curr        
    real, dimension( ims:ime, kms:kme, jms:jme ), intent(inout) :: qr_curr        
    real, dimension( ims:ime, kms:kme, jms:jme ), intent(inout) :: ni3d           
    real, dimension( ims:ime, kms:kme, jms:jme ), intent(inout) :: nc3d           
    real, dimension( ims:ime, kms:kme, jms:jme ), intent(inout) :: ns3d           
    real, dimension( ims:ime, kms:kme, jms:jme ), intent(inout) :: nr3d           
    real, dimension( ims:ime, kms:kme, jms:jme ), intent(inout) :: qndrop         
    real, dimension( ims:ime, kms:kme, jms:jme ), intent(inout) :: th             
    real, dimension( ims:ime, kms:kme, jms:jme ), intent(inout) :: wsedl3d        
    real, dimension( ims:ime, kms:kme, jms:jme ), intent(inout) :: cldfra_mp      
    real, dimension( ims:ime, kms:kme, jms:jme ), intent(inout) :: cldfra_conv    
    real, dimension( ims:ime, kms:kme, jms:jme ), intent(inout) :: cldfra_mp_all  
    real, dimension( ims:ime, kms:kme, jms:jme ), intent(inout) :: lradius
    real, dimension( ims:ime, kms:kme, jms:jme ), intent(inout) :: iradius
    real, dimension( ims:ime, kms:kme, jms:jme ), intent(inout) :: cldfrai
    real, dimension( ims:ime, kms:kme, jms:jme ), intent(inout) :: cldfral











    
    real, dimension( ims:ime , jms:jme ), intent(  out) :: sr                     
    
    
    
    
    
    
    
    character*250 :: wrfmessage
    logical  :: is_first_step                             
    logical  :: ptend_loc_ls, ptend_all_ls                
    logical  :: ptend_loc_lq(pcnst),ptend_all_lq(pcnst)   
    integer   :: i,k,m,n,iw,jw,kw,imode,itsm1,itile_len,ktep1,kflip,ips,kcam



    integer   :: lchnk                                    
    integer   :: ncol                                     
    integer   :: conv_water_in_rad                        
    real (r8) :: multFrc
    real(r8)  :: dtime                                    
    real(r8)  :: dp

    real(r8) :: phis(pcols)                               
    real(r8) :: state_t(pcols,kte)
    real(r8) :: state_q(pcols,kte,pcnst)
    real(r8) :: state_s(pcols,kte)
    real(r8) :: state_pmid(pcols,kte)
    real(r8) :: state_pdel(pcols,kte)
    real(r8) :: state_rpdel(pcols,kte)
    real(r8) :: state_zm(pcols,kte)
    real(r8) :: state_omega(pcols,kte) 
    
    real(r8) :: state_pint(pcols,kte+1)
    
    real(r8) :: state1_t(pcols,kte)
    real(r8) :: state1_q(pcols,kte,pcnst)
    real(r8) :: state1_s(pcols,kte)
    real(r8) :: state1_pmid(pcols,kte)
    real(r8) :: state1_pdel(pcols,kte)
    real(r8) :: state1_rpdel(pcols,kte)
    real(r8) :: state1_zm(pcols,kte)
    real(r8) :: state1_omega(pcols,kte) 
    
    real(r8) :: state1_pint(pcols,kte+1)
    
    character*24 :: ptend_loc_name 
    real(r8) :: ptend_loc_s(pcols,kte)
    real(r8) :: ptend_loc_q(pcols,kte,pcnst)
    
    character*24 :: ptend_all_name 
    real(r8) :: ptend_all_s(pcols,kte)
    real(r8) :: ptend_all_q(pcols,kte,pcnst)

    real(r8) :: tmpnaer,tmpmaer
    real(r8) :: cmeliq(pcols,kte)                 
    real(r8) :: nrout(pcols,kte)                  
    real(r8) :: nsout(pcols,kte)                  
    real(r8) :: sh_icwmr(pcols,kte)               
    real(r8) :: dp_icwmr(pcols,kte)               
    real(r8) :: fice(pcols,kte)                   
    real(r8) :: fsnow(pcols,kte)                  
    real(r8) :: sh_frac(pcols,kte)                
    real(r8) :: dp_frac(pcols,kte)                
    real(r8) :: cmfmc(pcols,kte+1)
    real(r8) :: cmfmc2(pcols,kte+1)
   
    real(r8), dimension(pcols,kte) :: esl         
    real(r8), dimension(pcols,kte) :: esi         
    real(r8), dimension(pcols,kte) :: qvs         
    real(r8), dimension(pcols,kte) :: qvi         


    
    real(r8), dimension(pcols,kte) :: ttt         
    real(r8), dimension(pcols,kte) :: rh_old      
    real(r8), dimension(pcols,kte) :: qi_mac      


    


    
    real(r8)   :: rliq(pcols)              
 
    
    real(r8)   :: prec_str(pcols)          
    real(r8)   :: snow_str(pcols)          
    real(r8)   :: prec_sed(pcols)          
    real(r8)   :: snow_sed(pcols)          
    real(r8)   :: prec_pcw(pcols)          
    real(r8)   :: snow_pcw(pcols)          
    
    real(r8)   :: cflx(pcols,pcnst)        
    real(r8)   :: dlf(pcols,kte)           
    real(r8)   :: dlf2(pcols,kte)          
    real(r8)   :: rate1cld(pcols,kte)                    
    
    

    real(r8), dimension(pcols,kte)   :: cld          
    real(r8), dimension(pcols,kte)   :: ast          
    real(r8), dimension(pcols,kte)   :: aist         
    real(r8), dimension(pcols,kte)   :: alst         
    real(r8), dimension(pcols,kte)   :: concld       
    real(r8), dimension(pcols,kte)   :: qme
    real(r8), dimension(pcols,kte)   :: prain        
    real(r8), dimension(pcols,kte)   :: nevapr       
    real(r8), dimension(pcols,kte)   :: rel          
    real(r8), dimension(pcols,kte)   :: rei          
    real(r8), dimension(pcols,kte)   :: rel2         
    real(r8), dimension(pcols,kte)   :: rei2         
    real(r8), dimension(pcols,kte)   :: cldo         
    real(r8), dimension(pcols,kte)   :: wsedl        
    real(r8), dimension(pcols,kte)   :: rel_fn       
    real(r8), dimension(pcols,kte+1) :: kkvh         

    real(r8), target, dimension(pcols,kte,pcnst) :: qqcw                
    real(r8), dimension(pcols,kte,ntot_amode)    :: dgnumwet            
    real(r8), dimension(pcols,kte,ntot_amode)    :: dgnum               
    real(r8), dimension(pcols,kte)               :: rate1ord_cw2pr_st   
    

    
    
    
    real(r8), dimension(pcols,kte+1) :: mgflxprc     
    real(r8), dimension(pcols,kte+1) :: mgflxsnw     
    real(r8), dimension(pcols,kte)   :: mgmrprc      
    real(r8), dimension(pcols,kte)   :: mgmrsnw      
    real(r8), dimension(pcols,kte)   :: mgreffrain   
    real(r8), dimension(pcols,kte)   :: mgreffsnow   
    real(r8), dimension(pcols,kte)   :: cvreffliq    
    real(r8), dimension(pcols,kte)   :: cvreffice    
    
    
    
    real(r8), dimension(pcols,kte) :: dei          
    real(r8), dimension(pcols,kte) :: mu           
    real(r8), dimension(pcols,kte) :: lambdac      
    real(r8), dimension(pcols,kte) :: iciwp        
    real(r8), dimension(pcols,kte) :: iclwp        
    
    
    
    real(r8) :: mucon                                 
    real(r8) :: dcon                                  
    real(r8) :: lamcon                                
    real(r8) :: deicon                                
    
    
    
    real(r8), dimension(pcols,kte) :: deiconv      
    real(r8), dimension(pcols,kte) :: muconv       
    real(r8), dimension(pcols,kte) :: lambdaconv   
    real(r8), dimension(pcols,kte) :: iciwpst      
    real(r8), dimension(pcols,kte) :: iclwpst      
    real(r8), dimension(pcols,kte) :: iciwpconv    
    real(r8), dimension(pcols,kte) :: iclwpconv    
    
    real(r8), dimension(pcols,kte+1) :: tke          
    real(r8), dimension(pcols,kte+1) :: turbtype     
    real(r8), dimension(pcols,kte+1) :: smaw         
    
    
    
    real(r8)  allcld_ice (pcols,kte)                 
    real(r8)  allcld_liq (pcols,kte)                 
    
    
    
    real(r8), dimension(pcols,kte) :: cldfsnow     
    real(r8), dimension(pcols,kte) :: icswp        
    real(r8), dimension(pcols,kte) :: des          
    real(r8)  qsout(pcols,kte)                       
    
    real(r8) :: qrout(pcols,kte)                     
    real(r8) :: rflx(pcols,kte+1)                   
    real(r8) :: sflx(pcols,kte+1)                   
    real(r8) :: reff_rain(pcols,kte)                
    real(r8) :: reff_snow(pcols,kte)                
    
    real(r8)  icecldf(pcols,kte)                     
    real(r8)  liqcldf(pcols,kte)                     
    real(r8)  icecldf_out(pcols,kte)                 
    real(r8)  liqcldf_out(pcols,kte)                 
    
    
    
    real(r8)  rdtime                                  
    real(r8)  qtend(pcols,kte)                       
    real(r8)  ttend(pcols,kte)                       
    real(r8)  ltend(pcols,kte)                       
    real(r8)  evapsnow(pcols,kte)                    
    real(r8)  prodsnow(pcols,kte)                    
    real(r8)  icimr(pcols,kte)                       
    real(r8)  icwmr(pcols,kte)                       
    real(r8)  icimrst(pcols,kte)                     
    real(r8)  icwmrst(pcols,kte)                     
    real(r8)  icimrst_out(pcols,kte)                 
    real(r8)  icwmrst_out(pcols,kte)                 
    real(r8)  cmeice(pcols,kte)                      
    real(r8)  temp(pcols)
    real(r8)  res(pcols,kte)
    
    
    
    real(r8)  qcsevap(pcols,kte)                     
    real(r8)  qisevap(pcols,kte)                     
    real(r8)  qvres(pcols,kte)                       
    real(r8)  cmeiout(pcols,kte)                     
    real(r8)  vtrmc(pcols,kte)                       
    real(r8)  vtrmi(pcols,kte)                       
    real(r8)  qcsedten(pcols,kte)                    
    real(r8)  qisedten(pcols,kte)                    
    
    real(r8)  prao(pcols,kte)  
    real(r8)  prco(pcols,kte)  
    real(r8)  mnuccco(pcols,kte)  
    real(r8)  mnuccto(pcols,kte)  
    real(r8)  mnuccdo(pcols,kte)
    real(r8)  mnuccdohet(pcols,kte)
    real(r8)  msacwio(pcols,kte)  
    real(r8)  psacwso(pcols,kte)  
    real(r8)  bergso(pcols,kte)  
    real(r8)  bergo(pcols,kte)  
    real(r8)  melto(pcols,kte)  
    real(r8)  homoo(pcols,kte)  
    real(r8)  qcreso(pcols,kte)  
    real(r8)  prcio(pcols,kte)  
    real(r8)  praio(pcols,kte)  
    real(r8)  qireso(pcols,kte)
    real(r8)  ftem(pcols,kte)
    real(r8)  mnuccro(pcols,kte) 
    real(r8)  pracso (pcols,kte) 
    real(r8)  meltsdt(pcols,kte) 
    real(r8)  frzrdt (pcols,kte) 
    real(r8)  dpdlfliq(pcols,kte)
    real(r8)  dpdlfice(pcols,kte)
    real(r8)  shdlfliq(pcols,kte)
    real(r8)  shdlfice(pcols,kte)
    real(r8)  dpdlft  (pcols,kte)
    real(r8)  shdlft  (pcols,kte)
    

    integer l, lnum, lnumcw, lmass, lmasscw

    
    
    
    real(r8)  dum1,dum2
    real(r8)  qc(pcols,kte)
    real(r8)  qi(pcols,kte)
    real(r8)  nc(pcols,kte)
    real(r8)  ni(pcols,kte)
    real(r8)  icinc(pcols,kte)                       
    real(r8)  cdnumc(pcols)                          
    real(r8)  icwnc(pcols,kte)                       
    real(r8)  iwc(pcols,kte)                         
    real(r8)  lwc(pcols,kte)                         
    real(r8)  effliq(pcols,kte)                      
    real(r8)  effice(pcols,kte)                      
    real(r8)  effliq_fn(pcols,kte)                   
    real(r8)  wsub(pcols,kte)                        
    real(r8)  wsubi(pcols,kte)                       
    
    
    
    real(r8)  tlat(pcols,kte)
    real(r8)  qvlat(pcols,kte)
    real(r8)  qcten(pcols,kte)
    real(r8)  qiten(pcols,kte)
    real(r8)  ncten(pcols,kte)
    real(r8)  niten(pcols,kte)
    real(r8)  effc(pcols,kte)
    real(r8)  effc_fn(pcols,kte)                     
    real(r8)  effi(pcols,kte)
    real(r8)  prect(pcols)
    real(r8)  preci(pcols)
    
    
    real(r8)  naai(pcols,kte)      
    real(r8)  naai_hom(pcols,kte)  
    real(r8)  npccn(pcols,kte)     
    real(r8)  rndst(pcols,kte,4)
    real(r8)  nacon(pcols,kte,4)
    
    
    
    real(r8)  efiout(pcols,kte)
    real(r8)  efcout(pcols,kte)
    real(r8)  ncout(pcols,kte)
    real(r8)  niout(pcols,kte)
    
    real(r8)  freqi(pcols,kte)
    real(r8)  freql(pcols,kte)
    
    
    
    real(r8)  ctrel(pcols)
    real(r8)  ctrei(pcols)
    real(r8)  ctnl(pcols)
    real(r8)  ctni(pcols)
    real(r8)  fcti(pcols)
    real(r8)  fctl(pcols)
    
    
    
    integer               :: naer_all
    real(r8)     :: aermmr1(pcols,kte)
    real(r8), allocatable :: aer_mmr(:,:,:)           
    
    real(r8)  zeros(pcols,kte)
    
    real(r8)  alst_mic(pcols,kte)
    real(r8)  aist_mic(pcols,kte)
    
    real(r8), pointer :: fldcw(:,:)
    real(r8) :: xland_pt
    real(r8) :: snowh_pt
    
    
    
    
    
    cmeliq(:,:)=0.0_r8
    
    
    conv_water_in_rad = 1
    



    
    

    dgnumwet(:,:,:)              = 0.1e-6_r8      
    dgnumwet(:,:,modeptr_aitken) = 0.02e-6
    dgnumwet(:,:,modeptr_coarse) = 1.0e-6 







    
    
    dgnum(:,:,:) = dgnumwet(:,:,:) 
    
    
    dtime = dt

    
    
    lradius(:,:,:) = 10._r8
    iradius(:,:,:) = 25._r8
  
    
    is_first_step  = .false.
    if(itimestep == 1) then
       is_first_step          = .true.
       cldfra_old_mp(:,:,:)   = 0.0_r8
       rate1ord_cw2pr_st(:,:) = 0.0_r8 
       cldfrai(:,:,:)       = 0._r8
       cldfral(:,:,:)       = 0._r8
       cldfra_mp(:,:,:)     = 0._r8
       cldfra_mp_all(:,:,:) = 0._r8
       cldfra_conv(:,:,:)   = 0._r8

       if(config_flags%shcu_physics .NE. CAMUWSHCUSCHEME) call wrf_message('WARNING: sh_icwmr,cmfmc,cmfmc2 and sh_frac are set to zero in CAMMGMP')
       if(config_flags%cu_physics   .NE. CAMZMSCHEME)     call wrf_message('WARNING: dp_icwmr is set to zero in CAMMGMP')
    endif
    
    ncol = pcols    
    
    if(ncol .NE. 1) then
       call wrf_error_fatal3("<stdin>",626,&
'Number of CAM Columns (NCOL) in CAMMGMP scheme must be 1')
    endif
    
    
    concld(:,:)      =0.0_r8  
                              
    
    state_omega(:,:) = 0.0_r8 
                              
    
    
    
    
    
    itsm1     = its - 1 
    itile_len = ite - itsm1
    do jw     = jts , jte 
       do iw  = its , ite 
          
          xland_pt = xland(iw,jw)
          snowh_pt = snowh(iw,jw)

          lchnk   = (jw - jts) * itile_len + (iw - itsm1)          
          phis(1) = ht(iw,jw)  * gravit   
          ktep1   = kte + 1
          
          
          do kw  = kts, kte
             kflip                = ktep1 - kw
             state_pmid(1,kflip)  = p_hyd(iw,kw,jw)                   
             dp                   = p8w(iw,kw,jw) - p8w(iw,kw+1,jw)   
             state_pdel(1,kflip)  = dp
             state_rpdel(1,kflip) = 1.0_r8/dp                         
             state_zm(1,kflip)    = z_sea_level(iw,kw,jw) - ht(iw,jw) 
             state_t(1,kflip)     = t_phy(iw,kw,jw)                   
             state_s(1,kflip)     = cpair *th(iw,kw,jw)*pi_phy(iw,kw,jw) + gravit*state_zm(1,kflip) + phis(1) 
             
             
             
             multFrc              = 1._r8/(1._r8 + qv_curr(iw,kw,jw))
             state_q(1,kflip,1)   = qv_curr(iw,kw,jw)*multFrc      
             state_q(1,kflip,ixcldliq) = qc_curr(iw,kw,jw)*multFrc 
             state_q(1,kflip,ixcldice) = qi_curr(iw,kw,jw)*multFrc 
             state_q(1,kflip,ixnumliq) = nc3d(iw,kw,jw)*multFrc
             state_q(1,kflip,ixnumice) = ni3d(iw,kw,jw)*multFrc

             qi_mac(1,kflip)           = qi_curr(iw,kw,jw)*multFrc

             
             
             if(abs(state_q(1,kflip,1))        < qthresh_mass .and. state_q(1,kflip,1)        < qmin(1))        state_q(1,kflip,1)        = qmin(1)
             if(abs(state_q(1,kflip,ixcldliq)) < qthresh_mass .and. state_q(1,kflip,ixcldliq) < qmin(ixcldliq)) state_q(1,kflip,ixcldliq) = qmin(ixcldliq)
             if(abs(state_q(1,kflip,ixcldice)) < qthresh_mass .and. state_q(1,kflip,ixcldice) < qmin(ixcldice)) state_q(1,kflip,ixcldice) = qmin(ixcldice)

             
             if(abs(state_q(1,kflip,ixnumliq)) < qthresh_numl .and. state_q(1,kflip,ixnumliq) < qmin(ixnumliq)) state_q(1,kflip,ixnumliq) = qmin(ixnumliq)
             if(abs(state_q(1,kflip,ixnumice)) < qthresh_numi .and. state_q(1,kflip,ixnumice) < qmin(ixnumice)) state_q(1,kflip,ixnumice) = qmin(ixnumice)


             
             if(state_q(1,kflip,1) < 0.0_r8)then
                
                
                state_q(1,kflip,1)        = qmin(1)
             endif
             
             if(state_q(1,kflip,ixcldliq) < 0.0_r8) then
                
                
                state_q(1,kflip,ixcldliq) = qmin(ixcldliq)
             endif

             if(state_q(1,kflip,ixcldice) < 0.0_r8) then
                
                
                state_q(1,kflip,ixcldice) = qmin(ixcldice)
             endif

             if(state_q(1,kflip,ixnumliq) < 0.0_r8) then
                
                
                state_q(1,kflip,ixnumliq) = qmin(ixnumliq)
             endif
             
             if(state_q(1,kflip,ixnumice) < 0.0_r8) then
                
                
                state_q(1,kflip,ixnumice) = qmin(ixnumice)
             endif
             
             qqcw(1,kflip,:)                  = 1.e-38_r8      

             cldo(1,kflip)        = cldfra_mp_all(iw,kw,jw)
             
             
             rh_old(1,kflip)      = rh_old_mp(iw,kw,jw)
             dlf(1,kflip)         = 0.0_r8
             dlf2(1,kflip)        = 0.0_r8

             sh_icwmr(1,kflip)    = 0.0_r8
             sh_frac(1,kflip)     = 0.0_r8
             dp_frac(1,kflip)     = 0._r8
             if(config_flags%shcu_physics==CAMUWSHCUSCHEME) then
                sh_icwmr(1,kflip)  = icwmrsh3d(iw,kw,jw)      
                sh_frac(1,kflip)   = shfrc3d(iw,kw,jw)        
                dlf2(1,kflip)      = dlf2_3d(iw,kw,jw)        
             endif
             dp_icwmr(1,kflip)     = 0.0_r8
             if(config_flags%cu_physics==CAMZMSCHEME)then
                dp_icwmr(1,kflip)  = icwmrdp3d(iw,kw,jw)      
                dlf(1,kflip)       = dlf3d(iw,kw,jw)          
             endif
             
          enddo
          
          do kw = kts, kte+1
             kflip = kte - kw + 2
             
             state_pint(1,kflip) = p8w(iw,kw,jw)        
             tke(1,kflip)        = tke_pbl(iw,kw,jw)    
             kkvh(1,kflip)       = kvh3d(iw,kw,jw)      
             turbtype(1,kflip)   = turbtype3d(iw,kw,jw) 
             smaw(1,kflip)       = smaw3d(iw,kw,jw)     

             cmfmc(1,kflip)      = 0.0_r8
             cmfmc2(1,kflip)     = 0.0_r8
             if(config_flags%shcu_physics==CAMUWSHCUSCHEME  .or. config_flags%cu_physics   == CAMZMSCHEME ) then 
                cmfmc(1,kflip)   = cmfmc3d(iw,kw,jw)    
             endif
             
             if(config_flags%shcu_physics==CAMUWSHCUSCHEME ) then
                cmfmc2(1,kflip)  = cmfmc2_3d(iw,kw,jw)  
             endif
             
          end do
          do kcam = 1, kte
             
             dp_frac(1,kcam)         = max(0.0_r8,min(dp1*log(1.0_r8+dp2*(cmfmc(1,kcam+1)-cmfmc2(1,kcam+1))),0.60_r8)) 
          end do


          
          cflx(:pcols,:) = 0.0_r8        
          cflx(:pcols,1) = qfx(iw,jw)    
          rliq(:pcols)   = rliq2d(iw,jw) 
          

          
          state1_pmid(:,:)  = state_pmid(:,:)
          state1_pdel(:,:)  = state_pdel(:,:)
          state1_rpdel(:,:) = state_rpdel(:,:)
          state1_zm(:,:)    = state_zm(:,:)
          state1_t(:,:)     = state_t(:,:)
          state1_s(:,:)     = state_s(:,:)
          state1_pint(:,:)  = state_pint(:,:)
          state1_q(:,:,:)   = state_q(:,:,:)
          state1_omega(:,:) = state_omega(:,:) 
          
          call physics_ptend_init(ptend_loc_name,ptend_loc_q,ptend_loc_s,ptend_loc_lq,ptend_loc_ls,pcnst)  
          call physics_ptend_init(ptend_all_name,ptend_all_q,ptend_all_s,ptend_all_lq,ptend_all_ls,pcnst)  
          
          if( is_first_step) then
             do i=1,pcnst
                fldcw => qqcw(:,:,i)
                if(associated(fldcw)) then
                   fldcw(1:pcols,1:pver)          = 1.e-38_r8
                end if
             end do
          endif
          
          
          
          if( is_first_step) then
             kkvh(:,:)     = 0._r8
             tke(:,:)      = 0._r8
             
             
             
             turbtype(:,:) = 0._r8
             smaw(:,:)     = 0._r8
             dlf(:,:)      = 0._r8
             dlf2(:,:)     = 0._r8
          endif
          
          
          
          
          dcon   = 25.e-6_r8
          mucon  = 5.3_r8
          deicon = 50._r8
          
          muconv(:,:)     = mucon
          lambdaconv(:,:) = (mucon + 1._r8)/dcon
          deiconv(:,:)    = deicon
          
          
          call Macrophysics_simple(is_first_step, pcnst, pcols, kte, ixcldliq, ixnumliq,         &
               lchnk, dtime, state1_t, state_pmid, state_q, dp_icwmr, sh_icwmr, dp_frac,      &
               sh_frac, ast,aist,alst,qi_mac,xland_pt,snowh_pt,                               &
               
               state1_s, state1_q, rh_old, cldo, esl, qvs, ptend_loc_name, ptend_loc_ls,      &
               ptend_loc_lq, ptend_loc_s,ptend_loc_q, ptend_all_name, ptend_all_ls,           &
               ptend_all_lq,ptend_all_s,ptend_all_q                                           )          
          
          do kw = kts , kte
             kflip                   = kte-kw+1

             cldfral(iw,kw,jw)       = alst(1,kflip)
             cldfrai(iw,kw,jw)       = aist(1,kflip)
             CLDFRA_MP(iw,kw,jw)     = ast(1,kflip)  
             cld(1,kflip)            = ast(1,kflip)
             CLDFRA_MP_all(iw,kw,jw) = min(ast(1,kflip)+min(0.8_r8,dp_frac(1,kflip)+sh_frac(1,kflip)),1._r8)
             CLDFRA_CONV(iw,kw,jw)   = max(min(0.8_r8,dp_frac(1,kflip)+sh_frac(1,kflip)),0._r8)
             concld(1,kflip)         = CLDFRA_CONV(iw,kw,jw) 
             cldo(1,kflip)           = cldfra_old_mp(iw,kw,jw)
          end do

          
          
          

          
          
          
          

          ptend_loc_name         = 'microp'
          ptend_loc_ls           = .true.
          ptend_loc_lq(1)        = .true.
          ptend_loc_lq(ixcldliq) = .true.
          ptend_loc_lq(ixcldice) = .true.
          ptend_loc_lq(ixnumliq) = .true.
          ptend_loc_lq(ixnumice) = .true.
          
          
          
          zeros(:ncol,:pver)  = 0._r8
          qc(:ncol,:pver) = state1_q(:ncol,:pver,ixcldliq)
          qi(:ncol,:pver) = state1_q(:ncol,:pver,ixcldice)
          nc(:ncol,:pver) = state1_q(:ncol,:pver,ixnumliq)
          ni(:ncol,:pver) = state1_q(:ncol,:pver,ixnumice)
          
          if( micro_treatment .eq. 'inter' ) then
             alst_mic(:ncol,:pver) = ast(:ncol,:pver)
             aist_mic(:ncol,:pver) = ast(:ncol,:pver)
          elseif( micro_treatment .eq. 'compl' ) then
             alst_mic(:ncol,:pver) = alst(:ncol,:pver)
             aist_mic(:ncol,:pver) = aist(:ncol,:pver)
          endif
          
          
          
          
          
          
          n = modeptr_accum    
          
          tmpnaer = accum_mode 
          l = numptr_amode(n)  
          state1_q(:,:,l) = tmpnaer
          
          m = 1                
          tmpmaer = specdens_amode(lspectype_amode(m,n)) * tmpnaer / voltonumb_amode(n)
          l = lmassptr_amode(m,n)
          state1_q(:,:,l) = tmpmaer
          
          n = modeptr_aitken   
          
          tmpnaer = aitken_mode 
          l = numptr_amode(n)
          state1_q(:,:,l) = tmpnaer
          
          m = 1                
          tmpmaer = specdens_amode(lspectype_amode(m,n)) * tmpnaer / voltonumb_amode(n)
          l = lmassptr_amode(m,n)
          state1_q(:,:,l) = tmpmaer
          
          n = modeptr_coarse   
          
          tmpnaer = coarse_mode 
          l = numptr_amode(n)
          state1_q(:,:,l) = tmpnaer
          
          tmpmaer = specdens_amode(lspectype_amode(m,n)) * tmpnaer / voltonumb_amode(n)
          m = 1                
          l = lmassptr_amode(m,n)
          state1_q(:,:,l) = tmpmaer*0.5
          
          m = 2                
          l = lmassptr_amode(m,n)
          state1_q(:,:,l) = tmpmaer*0.5
          
          
          
          
          
          
          call microp_aero_ts ( lchnk, ncol, dtime, state1_t, zeros,      & 
               state1_q(1,1,1), qc, qi,                                   &
               nc, ni, state1_pmid, state1_pdel, ast,                     &
               alst_mic, aist_mic,                                        &
               cldo, state1_pint, state1_rpdel, state1_zm, state1_omega,  &
               state1_q, cflx, ptend_loc_q, dgnumwet, dgnum,              &
               kkvh, tke, turbtype, smaw, wsub, wsubi,                    &
               naai,naai_hom, npccn, rndst,nacon,qqcw)
          
          
          
          
          
          
          
          
          
          
          
          
          
          
          call mmicro_pcond( sub_column, lchnk, ncol, dtime, state1_t,    &
               state1_q(1,1,1), qc, qi,                                   &
               nc, ni, state1_pmid, state1_pdel, ast,                     &
               alst_mic, aist_mic,                                        &
               cldo,                                                      &
               rate1cld,                                                  & 
               naai, npccn, rndst,nacon,                                  &
               tlat, qvlat,                                               &
               qcten, qiten, ncten, niten, effc,                          &
               effc_fn, effi, prect, preci,                               & 
               nevapr, evapsnow,                                          &
               prain, prodsnow, cmeice, dei, mu,                          &
               lambdac, qsout, des,                                       &
               rflx,sflx, qrout,reff_rain,reff_snow,                      &
               qcsevap, qisevap, qvres, cmeiout,                          &
               vtrmc, vtrmi, qcsedten, qisedten,                          &
               prao, prco, mnuccco, mnuccto, msacwio, psacwso,            &
               bergso, bergo, melto, homoo, qcreso, prcio, praio, qireso, &
               mnuccro, pracso, meltsdt, frzrdt , mnuccdo, nsout, nrout )
          
          do k=1,pver
             do i=1,ncol
                if (naai(i,k) .gt. 0._r8) then
                   mnuccdohet(i,k) = mnuccdo(i,k) - (naai_hom(i,k)/naai(i,k))*mnuccdo(i,k)
                else
                   mnuccdohet(i,k) = 0._r8
                end if
             end do
          end do
          
          mgflxprc(:ncol,:pverp) = rflx(:ncol,:pverp) + sflx(:ncol,:pverp)
          mgflxsnw(:ncol,:pverp) = sflx(:ncol,:pverp)
          
          mgmrprc(:ncol,:pver) = qrout(:ncol,:pver) + qsout(:ncol,:pver)
          mgmrsnw(:ncol,:pver) = qsout(:ncol,:pver)
          
          
          mgreffrain(:ncol,:pver) = reff_rain(:ncol,:pver)
          mgreffsnow(:ncol,:pver) = reff_snow(:ncol,:pver)
          
          
          cvreffliq(:ncol,:pver) = 9.0_r8  
          cvreffice(:ncol,:pver) = 37.0_r8 
          
          
          rate1ord_cw2pr_st(1:ncol,1:pver)=rate1cld(1:ncol,1:pver)
          
          
          wsedl(:ncol,:pver) = vtrmc(:ncol,:pver)
          
          
          
          
          do k = 1, pver
             do i = 1, ncol 
                des(i,k) = des(i,k) * 1.e6_r8
                if( ast(i,k) .lt. 1.e-4_r8 ) then
                   mu(i,k) = mucon
                   lambdac(i,k) = (mucon + 1._r8)/dcon
                   dei(i,k) = deicon
                endif
             end do
          end do
          
          
          
          qme(:ncol,:pver) = cmeliq(:ncol,:pver) + cmeiout(:ncol,:pver) 
          
          
          
          do k = 1, pver
             do i = 1, ncol
                ptend_loc_s(i,k)          =  tlat(i,k)
                ptend_loc_q(i,k,1)        = qvlat(i,k)
                ptend_loc_q(i,k,ixcldliq) = qcten(i,k)
                ptend_loc_q(i,k,ixcldice) = qiten(i,k)
                ptend_loc_q(i,k,ixnumliq) = ncten(i,k)
                ptend_loc_q(i,k,ixnumice) = niten(i,k)
             enddo
          enddo
          
          
          
          prec_pcw(:ncol) = prect(:ncol)
          snow_pcw(:ncol) = preci(:ncol)
          prec_sed(:ncol) = 0._r8
          snow_sed(:ncol) = 0._r8
          prec_str(:ncol) = prec_pcw(:ncol) + prec_sed(:ncol) - rliq(:ncol)
          snow_str(:ncol) = snow_pcw(:ncol) + snow_sed(:ncol) 
          
          
          
          
          
          call physics_ptend_sum(ptend_loc_ls,ptend_loc_lq,ptend_loc_s,ptend_loc_q,ptend_all_ls,ptend_all_lq,ptend_all_s,ptend_all_q,pcnst)
          ptend_all_name = 'cldwat'
          call physics_update( lchnk,dtime,state1_q,ptend_loc_q,state1_s,ptend_loc_s,ptend_loc_name,ptend_loc_lq,ptend_loc_ls,pcnst)
          call physics_ptend_init( ptend_loc_name,ptend_loc_q,ptend_loc_s, ptend_loc_lq,ptend_loc_ls,pcnst)
          
          if( micro_treatment .eq. 'inter' ) then
             icecldf(:ncol,:pver) = ast(:ncol,:pver)
             liqcldf(:ncol,:pver) = ast(:ncol,:pver)
          elseif( micro_treatment .eq. 'compl' ) then
             icecldf(:ncol,:pver) = aist(:ncol,:pver)
             liqcldf(:ncol,:pver) = alst(:ncol,:pver)
          endif
          
          
          rel(:ncol,:pver)        = effc(:ncol,:pver)
          rel_fn(:ncol,:pver)     = effc_fn(:ncol,:pver)	
          rei(:ncol,:pver)        = effi(:ncol,:pver)
          rel2(:ncol,:pver)       = rel(:ncol,:pver) * 0.9071_r8 
          rei2(:ncol,:pver)       = rei(:ncol,:pver) * 0.6057_r8 
          
          
          
          
          
          allcld_ice(:ncol,:pver) = 0._r8 
          allcld_liq(:ncol,:pver) = 0._r8 
          if( conv_water_in_rad /= 0 ) then
             
             call cldwat_fice( ncol, state1_t, fice, fsnow )

             
             call conv_water_4rad( lchnk, ncol, ast, sh_icwmr, dp_icwmr,       &
                  fice, sh_frac, dp_frac, conv_water_in_rad, rei, state1_pdel, &
                  state1_q(:,:,ixcldliq), state1_q(:,:,ixcldice),              &
                  allcld_liq, allcld_ice )
          else
             allcld_liq(:ncol,:) = state1_q(:ncol,:,ixcldliq)  
             allcld_ice(:ncol,:) = state1_q(:ncol,:,ixcldice)  
          end if
          
          
          
          
          do k = 1, pver
             do i = 1, ncol
                
                
                icimr(i,k)     = min( allcld_ice(i,k) / max(0.0001_r8,cld(i,k)),0.005_r8 )
                icwmr(i,k)     = min( allcld_liq(i,k) / max(0.0001_r8,cld(i,k)),0.005_r8 )
                icimrst(i,k)   = min( state1_q(i,k,ixcldice) / max(0.0001_r8,icecldf(i,k)),0.005_r8 )
                icwmrst(i,k)   = min( state1_q(i,k,ixcldliq) / max(0.0001_r8,liqcldf(i,k)),0.005_r8 )
                icinc(i,k)     = state1_q(i,k,ixnumice) / max(0.0001_r8,icecldf(i,k)) * state1_pmid(i,k) / (287.15_r8*state1_t(i,k))
                icwnc(i,k)     = state1_q(i,k,ixnumliq) / max(0.0001_r8,liqcldf(i,k)) * state1_pmid(i,k) / (287.15_r8*state1_t(i,k))
                iwc(i,k)       = allcld_ice(i,k) * state1_pmid(i,k) / (287.15_r8*state1_t(i,k))
                lwc(i,k)       = allcld_liq(i,k) * state1_pmid(i,k) / (287.15_r8*state1_t(i,k))
                effliq(i,k)    = effc(i,k)
                effliq_fn(i,k) = effc_fn(i,k)
                effice(i,k)    = effi(i,k)
                
                iciwp(i,k)     = icimr(i,k) * state1_pdel(i,k) / gravit
                iclwp(i,k)     = icwmr(i,k) * state1_pdel(i,k) / gravit
                
                
                iciwpst(i,k)   = min(state1_q(i,k,ixcldice)/max(0.0001_r8,ast(i,k)),0.005_r8) * state1_pdel(i,k) / gravit
                iclwpst(i,k)   = min(state1_q(i,k,ixcldliq)/max(0.0001_r8,ast(i,k)),0.005_r8) * state1_pdel(i,k) / gravit
                
                
                
                
                
                
                
                cldfsnow(i,k) = cld(i,k)
                
                if( ( cldfsnow(i,k) .gt. 1.e-4_r8 ) .and. & 
                     ( concld(i,k)   .lt. 1.e-4_r8 ) .and. & 
                     ( state1_q(i,k,ixcldliq) .lt. 1.e-10_r8 ) ) then
                   
                   
                endif
                
                if( ( cldfsnow(i,k) .lt. 1.e-4_r8 ) .and. ( qsout(i,k) .gt. 1.e-6_r8 ) ) then 
                   
                   
                endif
                
                
                
             enddo
          enddo
          
          
          
          
          
          
          
          do i = 1, ncol
             cdnumc(i) = 0._r8
             do k = 1, pver
                cdnumc(i) = cdnumc(i) + state1_q(i,k,ixnumliq)*state1_pdel(i,k)/gravit
             end do
          end do
          
          
          
          efcout(:,:)      = 10._r8 
          efiout(:,:)      = 25._r8 
          ncout(:,:)       = 0._r8
          niout(:,:)       = 0._r8	
          freql(:,:)       = 0._r8
          freqi(:,:)       = 0._r8
          liqcldf_out(:,:) = 0._r8
          icecldf_out(:,:) = 0._r8
          icwmrst_out(:,:) = 0._r8
          icimrst_out(:,:) = 0._r8
          do k = 1, pver
             do i = 1, ncol
                if( liqcldf(i,k) .gt. 0.01_r8 .and. icwmrst(i,k) .gt. 5.e-5_r8 ) then
                   efcout(i,k) = effc(i,k)
                   ncout(i,k)  = icwnc(i,k)
                   freql(i,k)  = 1._r8
                   liqcldf_out(i,k) = liqcldf(i,k)
                   icwmrst_out(i,k) = icwmrst(i,k)
                endif
                if( icecldf(i,k) .gt. 0.01_r8 .and. icimrst(i,k) .gt. 1.e-6_r8 ) then
                   efiout(i,k) = effi(i,k)
                   niout(i,k)  = icinc(i,k)
                   freqi(i,k)  = 1._r8
                   icecldf_out(i,k) = icecldf(i,k)
                   icimrst_out(i,k) = icimrst(i,k)
                endif
             end do
          end do
          
          
          
          fcti(:)  = 0._r8
          fctl(:)  = 0._r8
          ctrel(:) = 0._r8
          ctrei(:) = 0._r8
          ctnl(:)  = 0._r8
          ctni(:)  = 0._r8
          do i = 1, ncol
             do k = 1, pver
                if( liqcldf(i,k) .gt. 0.01_r8 .and. icwmrst(i,k) .gt. 1.e-7_r8 ) then
                   ctrel(i) = effc(i,k)
                   ctnl(i)  = icwnc(i,k)
                   fctl(i)  = 1._r8
                   exit
                endif
                if( icecldf(i,k) .gt. 0.01_r8 .and. icimrst(i,k) .gt. 1.e-7_r8 ) then
                   ctrei(i) = effi(i,k)
                   ctni(i)  = icinc(i,k)
                   fcti(i)  = 1._r8
                   exit
                endif
             enddo
          enddo
          
          
          call physics_update( lchnk,dtime,state_q,ptend_all_q,state_s,ptend_all_s,ptend_all_name,ptend_all_lq,ptend_all_ls,pcnst)
          
          
          do kw=kts,kte
             
             kflip = kte-kw+1
             
             qv_curr(iw,kw,jw)       = state_q(1,kflip,1) / (1.0_r8 - state_q(1,kflip,1)) 
             multFrc                 = 1._r8 + qv_curr(iw,kw,jw)
             
             qc_curr(iw,kw,jw)       = state_q(1,kflip,2) * multFrc
             qi_curr(iw,kw,jw)       = state_q(1,kflip,3) * multFrc 
             qs_curr(iw,kw,jw)       = qsout(1,kflip)     * multFrc 
             qr_curr(iw,kw,jw)       = qrout(1,kflip)     * multFrc 
             
             nc3d(iw,kw,jw)          = state_q(1,kflip,4) * multFrc 
             qndrop(iw,kw,jw)        =  nc3d(iw,kw,jw)              
             ni3d(iw,kw,jw)          = state_q(1,kflip,5) * multFrc
             
             
             ns3d(iw,kw,jw)          = nsout(1,kflip) * alt(iw,kw,jw) * multFrc  
             nr3d(iw,kw,jw)          = nrout(1,kflip) * alt(iw,kw,jw) * multFrc  
             
             th(iw,kw,jw)            = (state_s(1,kflip)-gravit*z_sea_level(iw,kw,jw))/(cpair*pi_phy(iw,kw,jw))
             wsedl3d(iw,kw,jw)       = wsedl(1,kflip)
             cldfra_old_mp(iw,kw,jw) = ast(1,kflip)

             
             ttt(1,kflip)     = (state_s(1,kflip)-gravit*z_sea_level(iw,kw,jw))/cpair
             esl(1,kflip)     = polysvp(ttt(1,kflip),0)
             qvs(1,kflip)     = max(1.e-30_r8,epsqs*esl(1,kflip)/(state_pmid(1,kflip)-(1._r8-epsqs)*esl(1,kflip)))
             rh_old_mp(iw,kw,jw)     = max(0._r8,state_q(1,kflip,1) / qvs(1,kflip))
             lcd_old_mp(iw,kw,jw)    = alst(1,kflip)
             lradius(iw,kw,jw)    = efcout(1,kflip)
             iradius(iw,kw,jw)    = efiout(1,kflip)
             
          end do
          
          
          
          
          
          
          
          
          rainncv(iw,jw) = (prec_sed(1) + prec_pcw(1))*dtime*1.0e3_r8 
          rainnc (iw,jw) = rainnc(iw,jw)+ rainncv(iw,jw)

          
          snowncv(iw,jw) = (snow_sed(1)  + snow_pcw(1))*dtime*1.0e3_r8 
          snownc (iw,jw) = snownc(iw,jw) + snowncv(iw,jw)

          sr(iw,jw) = snowncv(iw,jw)/(rainncv(iw,jw) + 1.E-12_r8)

        
       enddo 
    enddo 
  end subroutine CAMMGMP

  
  
  
  
  subroutine physics_update(state_lchnk,dt,state_q,ptend_q,state_s,ptend_s,ptend_name,ptend_lq,ptend_ls,pcnst_in)
  
  
  
  
  

    implicit none
    integer , intent(in) :: state_lchnk, pcnst_in
    real(r8), intent(in) :: dt
    
    character*24, intent(inout) :: ptend_name    
    
    logical , intent(inout) :: ptend_ls
    logical , intent(inout) :: ptend_lq(pcnst_in)
    real(r8), intent(inout) :: ptend_s(pcols,pver)
    real(r8), intent(inout) :: ptend_q(pcols,pver,pcnst_in)
    
    real(r8), intent(inout) :: state_q(pcols,pver,pcnst_in)
    real(r8), intent(inout) :: state_s(pcols,pver)
    
    character*40            :: name    
    character(len=16)       :: microp_scheme
    integer                 :: m,i,k,ncol
    integer                 :: ixcldice, ixcldliq, ixnumice, ixnumliq


    microp_scheme   = 'MG' 
    ptend_top_level = 1
    ptend_bot_level = pver
    ncol = pcols

    
    if(ptend_ls) then
       do k = ptend_top_level, ptend_bot_level
          do i = 1, ncol
             state_s(i,k)   = state_s(i,k)   + ptend_s(i,k) * dt
          end do
       end do
    end if

    
    call cnst_get_ind('CLDICE', ixcldice, abort=.false.)
    call cnst_get_ind('CLDLIQ', ixcldliq, abort=.false.)
    
    
    call cnst_get_ind('NUMICE', ixnumice, abort=.false.)
    call cnst_get_ind('NUMLIQ', ixnumliq, abort=.false.)
 
    do m = 1, pcnst_in
       if(ptend_lq(m)) then
          do k = ptend_top_level, ptend_bot_level
             do i = 1,ncol
                state_q(i,k,m) = state_q(i,k,m) + ptend_q(i,k,m) * dt
             end do
          end do

          
          
          if (m .ne. ixnumice  .and.  m .ne. ixnumliq) then
             name = trim(ptend_name) // '/' // trim(cnst_name(m))
             call qneg3(trim(name), state_lchnk, ncol, pcols, pver, m, m, qmin(m), state_q(1,1,m))
          else
             do k = ptend_top_level, ptend_bot_level
                do i = 1,ncol
                   
                   state_q(i,k,m) = max(1.e-12_r8,state_q(i,k,m))
                   state_q(i,k,m) = min(1.e10_r8,state_q(i,k,m))
                end do
             end do
          end if

       end if
    end do

    
    if (ixcldliq > 1) then
       if(ptend_lq(ixcldliq)) then
          if (ptend_name == 'stratiform' .or. ptend_name == 'cldwat'  ) then

          else if (ptend_name == 'convect_deep') then
             where (state_q(:ncol,:pver,ixcldliq) < 1.e-36_r8)
                state_q(:ncol,:pver,ixcldliq) = 0._r8
             end where
             
             if ( microp_scheme .eq. 'MG' ) then
                where (state_q(:ncol,:pver,ixcldliq) < 1.e-36_r8)
                   state_q(:ncol,:pver,ixnumliq) = 0._r8
                end where
             end if
          end if
       end if
    end if

    
    if (ixcldice > 1) then
       if(ptend_lq(ixcldice)) then
          if (ptend_name == 'stratiform' .or. ptend_name == 'cldwat'  ) then

          else if (ptend_name == 'convect_deep') then
             where (state_q(:ncol,:pver,ixcldice) < 1.e-36_r8)
                state_q(:ncol,:pver,ixcldice) = 0._r8
             end where
             
             if ( microp_scheme .eq. 'MG' ) then
                where (state_q(:ncol,:pver,ixcldice) < 1.e-36_r8)
                   state_q(:ncol,:pver,ixnumice) = 0._r8
                end where
             end if
          end if
       end if
    end if

    
    
    
    
    
    
    

    
    call physics_ptend_reset(ptend_name,ptend_q,ptend_s,ptend_lq,ptend_ls,pcnst_in)
    
  end subroutine physics_update
  
  
  
  subroutine physics_ptend_init(ptend_name,ptend_q,ptend_s,ptend_lq,ptend_ls,pcnst_in)
  
  
  
  
    implicit none
    integer, intent(in)    :: pcnst_in

    character*24, intent(inout) :: ptend_name

    logical , intent(inout):: ptend_ls 
    logical , intent(inout):: ptend_lq(pcnst_in)
    real(r8), intent(inout):: ptend_s(pcols,pver)
    real(r8), intent(inout):: ptend_q(pcols,pver,pcnst_in)
    
    ptend_name  = "none"
    ptend_lq(:) = .TRUE.
    ptend_ls    = .TRUE.
    call physics_ptend_reset(ptend_name,ptend_q,ptend_s,ptend_lq,ptend_ls,pcnst_in)
    return
  end subroutine physics_ptend_init
  
  

  subroutine physics_ptend_reset(ptend_name,ptend_q,ptend_s,ptend_lq,ptend_ls,pcnst_in)




    implicit none
    integer, intent(in)    :: pcnst_in

    character*24, intent(inout) :: ptend_name
    
    logical , intent(inout):: ptend_ls 
    logical , intent(inout):: ptend_lq(pcnst_in)
    real(r8), intent(inout):: ptend_s(pcols,pver)
    real(r8), intent(inout):: ptend_q(pcols,pver,pcnst_in)
    integer :: m
    
    if(ptend_ls) then
       ptend_s = 0._r8
    endif
    do m = 1, pcnst_in
       if(ptend_lq(m)) then
          ptend_q(:,:,m) = 0._r8
       endif
    end do

    ptend_name  = "none"
    ptend_lq(:) = .FALSE.
    ptend_ls    = .FALSE.

    ptend_top_level = 1
    ptend_bot_level = pver

    return
  end subroutine physics_ptend_reset
  


  subroutine physics_ptend_sum(ptend_ls,ptend_lq,ptend_s,ptend_q,ptend_sum_ls,ptend_sum_lq,ptend_sum_s,ptend_sum_q, pcnst_in)











    integer,  intent(in) :: pcnst_in
    logical,  intent(in) :: ptend_ls                  
    logical,  intent(in) :: ptend_lq(pcnst_in)           
    real(r8), intent(in) :: ptend_s(pcols,pver)       
    real(r8), intent(in) :: ptend_q(pcols,pver,pcnst_in) 
    
    logical,  intent(inout) :: ptend_sum_ls                  
    logical,  intent(inout) :: ptend_sum_lq(pcnst_in)           
    real(r8), intent(inout) :: ptend_sum_s(pcols,pver)       
    real(r8), intent(inout) :: ptend_sum_q(pcols,pver,pcnst_in) 
    


    integer :: k,m                              
    integer :: ncol,i
    

    ptend_top_level = 1
    ptend_bot_level = pver

    ncol = pcols
    
    if(ptend_ls) then
       ptend_sum_ls = .true.
       do i = 1, ncol
          do k = ptend_top_level, ptend_bot_level
             ptend_sum_s(i,k) = ptend_sum_s(i,k) + ptend_s(i,k)
          end do
       end do
    end if


    do m = 1, pcnst_in
       if(ptend_lq(m)) then
          ptend_sum_lq(m) = .true.
          do i = 1,ncol
             do k = ptend_top_level, ptend_bot_level
                ptend_sum_q(i,k,m) = ptend_sum_q(i,k,m) + ptend_q(i,k,m)
             end do
          end do
       end if
    end do

  end subroutine physics_ptend_sum



  subroutine cal_cldfra_mp_1d(cldl,cldi,qqv, qqc, qqi,tt,pp,xland_pt,snowh_pt)
    
    


    use wv_saturation,      only: epsqs, polysvp
    use shr_kind_mod,  only: r8=>shr_kind_r8
    use physconst,          only: tmelt
    
    real(r8), intent(in) :: qqv
    real(r8), intent(in) :: qqc
    real(r8), intent(in) :: qqi
    real(r8), intent(in) :: tt
    real(r8), intent(in) :: pp
    real(r8), intent(in) :: xland_pt
    real(r8), intent(in) :: snowh_pt
    real(r8), intent(inout) :: cldl,cldi
    REAL(r8):: rhl,dV,esl,omeps,cldrh,qvl
    real(r8):: cldcr,esi,qvi,rhi,rhdif,icimr,qv,qc,qi
    
    qv=max(qqv,0._r8)
    qc=max(qqc,0._r8)
    qi=max(qqi,0._r8)
    cldi= 0._r8
    cldl= 0._r8
    
    
    cldrh=1._r8
    
    if (pp >= 70000._r8) then
       if (xland_pt < 1.1_r8 .and. snowh_pt < 0.001_r8) then    
          cldcr = 0.7875_r8
       else
          cldcr = 0.8875_r8
       endif
    elseif (pp <= 40000._r8) then
       cldcr = 0.8_r8
    else
       cldcr = (0.8875_r8 - 0.8_r8)/30000._r8*(pp-40000._r8)+0.8_r8
    end if
    
    cldcr=min(max(0._r8,cldcr),1._r8)
    
    dV=cldrh-cldcr
    omeps = 1._r8 - 0.622_r8
    rhdif = 0._r8
    icimr = 0._r8
    
    esl  = polysvp(tt,0)
    qvl  = max(1.e-12_r8,epsqs*esl/(pp-(1._r8-epsqs)*esl))
    rhl  = (qv+qc)/qvl
    rhl  = min(1.1_r8,max(0._r8,rhl))
    if( rhl .ge. 1._r8 ) then
       cldl  = 1._r8
    elseif( rhl .gt. (cldrh-dV/6._r8) .and. rhl .lt. 1._r8 ) then
       cldl  = 1._r8 - (-3._r8/sqrt(2._r8)*(rhl-cldrh)/dV)**(2._r8/3._r8)
    elseif( rhl .gt. (cldrh-dV) .and. rhl .le. (cldrh-dV/6._r8) ) then
       cldl  = 4._r8*(cos((1._r8/3._r8)*(acos((3._r8/2._r8/sqrt(2._r8))* &
            (1._r8+(rhl-cldrh)/dV))-2._r8*3.14159_r8)))**2._r8
    elseif( rhl .le. (cldrh-dV) ) then
       cldl  = 0._r8
    endif
    
    esi  = polysvp(tt,1)
    if (tt.gt.tmelt) esi=esl
    qvi  = max(1.e-12_r8,epsqs*esi/(pp-(1._r8-epsqs)*esi))
    rhi  = (qv+qi)/qvi
    rhi  = min(1.1_r8,max(0._r8,rhi))
    
    if (tt < 273.15_r8) then
       rhdif = (rhi-0.8_r8) / 0.3_r8   
       cldi  = min(1._r8, max(rhdif,0._r8)**2._r8)
       if (qi.lt.1.e-12_r8) then
          cldi=0._r8
       else
          cldi=max(1.e-4_r8,cldi)
       endif
       
       if (qi.ge.1.e-12_r8) then
          icimr=qi/(max(cldi,1.e-20_r8))
          if (icimr.lt.1.e-7_r8) then
             cldi = max(0._r8,min(1._r8,qi/1.e-7_r8))
          endif
          if (icimr.gt.5.e-3_r8) then
             cldi = max(0._r8,min(1._r8,qi/5.e-3_r8))
          endif
       endif
    endif
    
    cldl=min(max(0._r8,cldl),1._r8)
    cldi=min(max(0._r8,cldi),1._r8)
    
  end subroutine cal_cldfra_mp_1d


  
  subroutine Macrophysics_simple(is_first_step, pcnst, pcols, kte, ixcldliq, ixnumliq, &
       lchnk, dtime, state1_t, state_pmid, state_q, dp_icwmr, sh_icwmr, dp_frac,    &
       sh_frac, ast,aist,alst,qi_mac,xland_pt,snowh_pt,                             &
       
       state1_s, state1_q, rh_old, cldo, esl, qvs, ptend_loc_name, ptend_loc_ls,    &
       ptend_loc_lq, ptend_loc_s,ptend_loc_q, ptend_all_name, ptend_all_ls,         &
       ptend_all_lq,ptend_all_s,ptend_all_q )

    
    
    
    
    

    
    
    
    
    
    
    
    
    
    
    
    
    
    
    

    use wv_saturation, only: epsqs, polysvp
    use physconst,     only: latvap,cpair 

    

    
    logical,  intent(in) :: is_first_step
    integer,  intent(in) :: pcnst, pcols, kte, ixcldliq, ixnumliq, lchnk
    real(r8), intent(in) :: dtime,xland_pt,snowh_pt

    real(r8), dimension(pcols,kte), intent(in) :: state_pmid
    real(r8), dimension(pcols,kte), intent(in) :: dp_icwmr
    real(r8), dimension(pcols,kte), intent(in) :: sh_icwmr
    real(r8), dimension(pcols,kte), intent(in) :: dp_frac
    real(r8), dimension(pcols,kte), intent(in) :: sh_frac
    real(r8), dimension(pcols,kte), intent(in) :: qi_mac

    real(r8), dimension(pcols,kte,pcnst), intent(in) :: state_q

    
    character*24, intent(inout) :: ptend_loc_name, ptend_all_name  

    logical, intent(inout) :: ptend_loc_ls, ptend_all_ls    
    logical, dimension(pcnst),intent(inout) :: ptend_loc_lq 
    logical, dimension(pcnst),intent(inout) :: ptend_all_lq 

    real(r8), dimension(pcols,kte),intent(inout) :: ptend_loc_s
    real(r8), dimension(pcols,kte),intent(inout) :: ptend_all_s
    real(r8), dimension(pcols,kte),intent(inout) :: rh_old  
    real(r8), dimension(pcols,kte),intent(inout) :: ast
    real(r8), dimension(pcols,kte),intent(inout) :: alst
    real(r8), dimension(pcols,kte),intent(inout) :: aist
    real(r8), dimension(pcols,kte),intent(inout) :: cldo    
    real(r8), dimension(pcols,kte),intent(inout) :: esl     
    real(r8), dimension(pcols,kte),intent(inout) :: qvs     
    real(r8), dimension(pcols,kte),intent(inout) :: state1_s
    real(r8), dimension(pcols,kte),intent(inout) :: state1_t

    real(r8), dimension(pcols,kte,pcnst), intent(inout) :: ptend_loc_q
    real(r8), dimension(pcols,kte,pcnst), intent(inout) :: ptend_all_q
    real(r8), dimension(pcols,kte,pcnst), intent(inout) :: state1_q


    
    integer  :: k
    real(r8), dimension(pcols,kte) :: cv_frac     
    real(r8), dimension(pcols,kte) :: st_frac     
    real(r8), dimension(pcols,kte) :: ttt         
    real(r8), dimension(pcols,kte) :: rh_curr     
    real(r8), dimension(pcols,kte) :: del_rh      
    real(r8), dimension(pcols,kte) :: qc_nc       
    real(r8), dimension(pcols,kte) :: del_qq      
    real(r8), dimension(pcols,kte) :: del_cf      
    real(r8), dimension(pcols,kte) :: rh_temp     

    real(r8), dimension(pcols) :: qsll        
    real(r8), dimension(pcols) :: tsp         
    real(r8) :: cldcr
    real(r8) :: factcc,fact_adj


    character*250 :: wrfmessage

   
    
    
    

    ptend_loc_name         = 'Macro1'
    ptend_loc_ls           = .true.
    ptend_loc_lq(1)        = .true.
    ptend_loc_lq(ixcldliq) = .true.
    ptend_loc_lq(ixnumliq) = .true.

    factcc   = 0.1_r8
    fact_adj = 0.35_r8
    cldcr    = 0.8875_r8 
    
    
    
    do k=1,pver
      if (state_pmid(1,k) >= 70000._r8) then
         if (xland_pt < 1.1_r8 .and. snowh_pt < 0.001_r8) then    
          cldcr = 0.7875_r8
         else
          cldcr = 0.8875_r8
         endif
      elseif (state_pmid(1,k)<= 40000._r8) then
        cldcr = 0.8_r8
      else   
        cldcr = (0.8875_r8 - 0.8_r8)/30000._r8*(state_pmid(1,k)-40000._r8)+0.8_r8
      end if 

       cldcr=min(max(0._r8,cldcr),1._r8)

       call cal_cldfra_mp_1d(alst(1,k),aist(1,k),state1_q(1,k,1), state1_q(1,k,ixcldliq), qi_mac(1,k),state1_t(1,k),state_pmid(1,k),xland_pt,snowh_pt)
       qsll         = 0._r8
       tsp          = 0._r8
       del_qq(1,k)  = 0._r8
       rh_temp(1,k) = 0._r8
       cv_frac(1,k) = max(0._r8,min(0.8_r8,dp_frac(1,k)+sh_frac(1,k)))
       alst(1,k)    = (1._r8-cv_frac(1,k))*alst(1,k)
       aist(1,k)    = (1._r8-cv_frac(1,k))*aist(1,k)
       ast(1,k)     = max(alst(1,k),aist(1,k))
       st_frac(1,k) = min(1._r8,ast(1,k)+cv_frac(1,k))
       esl(1,k)     = polysvp(state1_t(1,k),0)
       qvs(1,k)     = max(1.e-30_r8,epsqs*esl(1,k)/(state_pmid(1,k)-(1._r8-epsqs)*esl(1,k)))
       rh_curr(1,k) = max(0._r8,state1_q(1,k,1) / qvs(1,k))
       qc_nc(1,k)   = state1_q(1,k,ixcldliq)
             
       if( is_first_step ) then
          rh_old(1,k) = rh_curr(1,k)
          cldo(1,k)   = min(1._r8,ast(1,k)+cv_frac(1,k))
       endif
       
       
       
       
       
       if (rh_curr(1,k) > 1._r8) then
          call findsp_water(lchnk,pcols,state1_q(1,k,1),state1_t(1,k),state_pmid(1,k),tsp,qsll)
          del_qq(1,k)               = min(3.e-3_r8,max(0._r8,(state1_q(1,k,1)-qsll(1))*0.999_r8))  
          ptend_loc_q(1,k,ixcldliq) =  del_qq(1,k) /dtime
          ptend_loc_s(1,k)          =  del_qq(1,k) * latvap/dtime 
          ptend_loc_q(1,k,1)        = -del_qq(1,k)/dtime

          
          
          
          
       else if (rh_curr(1,k) > cldcr .and. rh_curr(1,k) <= 1._r8 ) then
          del_cf(1,k) = st_frac(1,k)-cldo(1,k)  
          if( del_cf(1,k) < 0._r8 ) then
             del_qq(1,k)               = qc_nc(1,k)*del_cf(1,k)/cldo(1,k)*factcc
             ptend_loc_q(1,k,ixcldliq) = del_qq(1,k)  /dtime
             ptend_loc_s(1,k)          = del_qq(1,k)  * latvap/dtime 
             ptend_loc_q(1,k,1)        = -del_qq(1,k) /dtime
             ptend_loc_q(1,k,ixnumliq) = ptend_loc_q(1,k,ixcldliq)*state1_q(1,k,ixnumliq)/max(1.e-30_r8,state1_q(1,k,ixcldliq))
             ptend_loc_q(1,k,ixnumliq) = min(0._r8,max(ptend_loc_q(1,k,ixnumliq),-state1_q(1,k,ixnumliq)/dtime))
          else if( del_cf(1,k) > 0._r8 .and. cldo(1,k) > 1.e-8_r8) then
             del_qq(1,k)               = min(state1_q(1,k,1)*0.1_r8,min(3.e-3_r8*st_frac(1,k),qc_nc(1,k)*del_cf(1,k)/cldo(1,k)))*factcc
             ptend_loc_q(1,k,ixcldliq) = del_qq(1,k)  /dtime
             ptend_loc_s(1,k)          = del_qq(1,k)  * latvap/dtime 
             ptend_loc_q(1,k,1)        = -del_qq(1,k) /dtime
             ptend_loc_q(1,k,ixnumliq) = 0._r8
          else if( del_cf(1,k) > 0._r8 .and. cldo(1,k) < 1.e-8_r8) then
             del_qq(1,k)               = min(state1_q(1,k,1)*0.1_r8,2.e-5_r8*st_frac(1,k))*factcc
             ptend_loc_q(1,k,ixcldliq) = del_qq(1,k)  /dtime
             ptend_loc_s(1,k)          = del_qq(1,k)  * latvap/dtime 
             ptend_loc_q(1,k,1)        = -del_qq(1,k) /dtime
             ptend_loc_q(1,k,ixnumliq) = 0._r8
          else
             del_qq(1,k)               = 0._r8
             ptend_loc_q(1,k,ixcldliq) = 0._r8
             ptend_loc_s(1,k)          = 0._r8
             ptend_loc_q(1,k,1)        = 0._r8
             ptend_loc_q(1,k,ixnumliq) = 0._r8
          end if
       else if (rh_curr(1,k) < cldcr  .and. cldo(1,k) <1.e-2_r8 .and. qc_nc(1,k) >1.e-12_r8) then 
          call findsp_water(lchnk,pcols,state1_q(1,k,1),state1_t(1,k),state_pmid(1,k),tsp,qsll)                      
             del_qq(1,k)= max(min(qc_nc(1,k), (qsll(1)-state1_q(1,k,1))),0._r8)*0.99_r8
             ptend_loc_q(1,k,ixcldliq) = -del_qq(1,k) /dtime
             ptend_loc_s(1,k)          = -del_qq(1,k) * latvap/dtime 
             ptend_loc_q(1,k,1)        =  del_qq(1,k)/dtime
             ptend_loc_q(1,k,ixnumliq) = ptend_loc_q(1,k,ixcldliq)*state1_q(1,k,ixnumliq)/max(1.e-30_r8,state1_q(1,k,ixcldliq))
             ptend_loc_q(1,k,ixnumliq) = min(0._r8,max(ptend_loc_q(1,k,ixnumliq),-state1_q(1,k,ixnumliq)/dtime))
       else if (rh_curr(1,k) < cldcr  .and. cldo(1,k) >=1.e-2_r8 .and. qc_nc(1,k) >1.e-12_r8) then 
          call findsp_water(lchnk,pcols,state1_q(1,k,1),state1_t(1,k),state_pmid(1,k),tsp,qsll)
             del_qq(1,k)= max(min(qc_nc(1,k), qsll(1)-state1_q(1,k,1)),0._r8)*factcc
             ptend_loc_q(1,k,ixcldliq) = -del_qq(1,k) /dtime
             ptend_loc_s(1,k)          = -del_qq(1,k) * latvap/dtime 
             ptend_loc_q(1,k,1)        =  del_qq(1,k)/dtime
             ptend_loc_q(1,k,ixnumliq) = ptend_loc_q(1,k,ixcldliq)*state1_q(1,k,ixnumliq)/max(1.e-30_r8,state1_q(1,k,ixcldliq))

             ptend_loc_q(1,k,ixnumliq) = min(0._r8,max(ptend_loc_q(1,k,ixnumliq),-state1_q(1,k,ixnumliq)/dtime))
       else
             ptend_loc_q(1,k,ixcldliq) = 0._r8
             ptend_loc_s(1,k)          = 0._r8
             ptend_loc_q(1,k,1)        = 0._r8
             ptend_loc_q(1,k,ixnumliq) = 0._r8
       end if
       state1_t(1,k)=state1_t(1,k)+ptend_loc_s(1,k)*dtime/cpair
    end do  
    
    
    
    
    
    call physics_ptend_sum(ptend_loc_ls,ptend_loc_lq,ptend_loc_s,ptend_loc_q,ptend_all_ls,ptend_all_lq,ptend_all_s,ptend_all_q,pcnst)
    ptend_all_name = 'cldwat'
    call physics_update( lchnk,dtime,state1_q,ptend_loc_q,state1_s,ptend_loc_s,ptend_loc_name,ptend_loc_lq,ptend_loc_ls,pcnst)
    call physics_ptend_init( ptend_loc_name,ptend_loc_q,ptend_loc_s, ptend_loc_lq,ptend_loc_ls,pcnst)

    
    
    
    ptend_loc_name         = 'Macro2'
    ptend_loc_ls           = .true.
    ptend_loc_lq(1)        = .true.
    ptend_loc_lq(ixcldliq) = .true.
    ptend_loc_lq(ixnumliq) = .true.
    
    
    
    
    
    do k = 1 , pver
       del_qq(1,k)=0._r8
       call cal_cldfra_mp_1d(alst(1,k),aist(1,k),state1_q(1,k,1), state1_q(1,k,ixcldliq), qi_mac(1,k),state1_t(1,k),state_pmid(1,k),xland_pt,snowh_pt)
       alst(1,k) = (1._r8-cv_frac(1,k))*alst(1,k)
       aist(1,k) = (1._r8-cv_frac(1,k))*aist(1,k)
       ast(1,k)  = max(alst(1,k),aist(1,k))
       st_frac(1,k) = alst(1,k)

       qc_nc(1,k) = max(0._r8, state1_q(1,k,ixcldliq) - &
            max(0._r8,(dp_frac(1,k)*dp_icwmr(1,k) +sh_frac(1,k)*sh_icwmr(1,k))))
       
       
       
       
       
       
       
       
       
       
       
       
       
          
       if (st_frac(1,k) > 1.e-5_r8 ) then
          
          
          
          
          if (qc_nc(1,k) > 3.e-3_r8 * st_frac(1,k)) then
             del_qq(1,k)               = qc_nc(1,k)-3.e-3_r8* st_frac(1,k)*fact_adj
             ptend_loc_q(1,k,ixcldliq) = -del_qq(1,k)/dtime
             ptend_loc_q(1,k,1)        =  del_qq(1,k)/dtime
             ptend_loc_s(1,k)          = -del_qq(1,k) * latvap /dtime  
             ptend_loc_q(1,k,ixnumliq) = ptend_loc_q(1,k,ixcldliq)*state1_q(1,k,ixnumliq)/max(1.e-20_r8,state1_q(1,k,ixcldliq))
             ptend_loc_q(1,k,ixnumliq) = min(0._r8,max(ptend_loc_q(1,k,ixnumliq),-state1_q(1,k,ixnumliq)/dtime))
             
             
             
             
          else if (qc_nc(1,k) < 2.e-5_r8 * st_frac(1,k)) then
             del_qq(1,k)               = 2.e-5_r8 *st_frac(1,k)-qc_nc(1,k)
             del_qq(1,k)               = max(0._r8,min(del_qq(1,k), state1_q(1,k,1)*0.1_r8))*fact_adj
             ptend_loc_q(1,k,ixcldliq) = del_qq(1,k)/dtime
             ptend_loc_q(1,k,1)        = -del_qq(1,k)/dtime
             ptend_loc_s(1,k)          = del_qq(1,k)* latvap / dtime  
          end if
       end if
       state1_t(1,k)=state1_t(1,k)+ptend_loc_s(1,k)*dtime/cpair
    end do
    
    
    
    call physics_ptend_sum(ptend_loc_ls,ptend_loc_lq,ptend_loc_s,ptend_loc_q,ptend_all_ls,ptend_all_lq,ptend_all_s,ptend_all_q,pcnst)
    ptend_all_name = 'cldwat'
    call physics_update( lchnk,dtime,state1_q,ptend_loc_q,state1_s,ptend_loc_s,ptend_loc_name,ptend_loc_lq,ptend_loc_ls,pcnst)
    call physics_ptend_init( ptend_loc_name,ptend_loc_q,ptend_loc_s, ptend_loc_lq,ptend_loc_ls,pcnst)

 go to 2000
    
    
    
    
    ptend_loc_name         = 'Macro3'
    ptend_loc_ls           = .true.
    ptend_loc_lq(1)        = .true.
    ptend_loc_lq(ixcldliq) = .true.
    
    
    
    
    do k=1,pver
       qsll = 0._r8
       tsp  = 0._r8
       del_qq(1,k)  = 0._r8
       esl(1,k)     = polysvp(state1_t(1,k),0)
       qvs(1,k)     = max(1.e-30_r8,epsqs*esl(1,k)/(state_pmid(1,k)-(1._r8-epsqs)*esl(1,k)))
       rh_curr(1,k) = state1_q(1,k,1) / qvs(1,k)
             
       if (rh_curr(1,k) > 1._r8) then
          call findsp_water(lchnk,pcols,state1_q(1,k,1),state1_t(1,k),state_pmid(1,k),tsp,qsll)
          del_qq(1,k)=min(2.e-2_r8,max(0._r8,(state1_q(1,k,1)-qsll(1))*0.999_r8))  
          ptend_loc_q(1,k,ixcldliq) =  del_qq(1,k) /dtime
          ptend_loc_s(1,k)          =  del_qq(1,k) * latvap/dtime 
          ptend_loc_q(1,k,1)        = -del_qq(1,k)/dtime
       end if
       state1_t(1,k)=state1_t(1,k)+ptend_loc_s(1,k)*dtime/cpair
    end do

    
    
    call physics_ptend_sum(ptend_loc_ls,ptend_loc_lq,ptend_loc_s,ptend_loc_q,ptend_all_ls,ptend_all_lq,ptend_all_s,ptend_all_q,pcnst)
    ptend_all_name = 'cldwat'
    call physics_update( lchnk,dtime,state1_q,ptend_loc_q,state1_s,ptend_loc_s,ptend_loc_name,ptend_loc_lq,ptend_loc_ls,pcnst)
    call physics_ptend_init( ptend_loc_name,ptend_loc_q,ptend_loc_s, ptend_loc_lq,ptend_loc_ls,pcnst)

    
    
    
2000 continue
    
    do k = 1 , pver
       state1_q(1,k,1)        = max(qmin(1),state1_q(1,k,1))
       state1_q(1,k,ixcldliq) = max(qmin(ixcldliq),state1_q(1,k,ixcldliq))
       state1_q(1,k,ixcldice) = max(qmin(ixcldice),state1_q(1,k,ixcldice))
       state1_q(1,k,ixnumliq) = max(qmin(ixnumliq),state1_q(1,k,ixnumliq))
       state1_q(1,k,ixnumice) = max(qmin(ixnumice),state1_q(1,k,ixnumice))
       call  cal_cldfra_mp_1d(alst(1,k),aist(1,k),state1_q(1,k,1), state1_q(1,k,ixcldliq), qi_mac(1,k),state1_t(1,k),state_pmid(1,k),xland_pt,snowh_pt)

       aist(1,k) = (1._r8-cv_frac(1,k))*aist(1,k)
       alst(1,k) = (1._r8-cv_frac(1,k))*alst(1,k)
       ast(1,k)=max(alst(1,k),aist(1,k))
    end do
    
    
    
  end subroutine Macrophysics_simple
  
  subroutine CAMMGMP_INIT(ixcldliq_in,ixcldice_in &
       ,ixnumliq_in,ixnumice_in,chem_opt_in       &    
       ,ids, ide, jds, jde, kds, kde              &
       ,ims, ime, jms, jme, kms, kme              &
       ,its, ite, jts, jte, kts, kte              )        
    
    
    
    
    
    
    use microp_aero,     only: ini_microp_aero
    use cldwat2m_micro,  only: ini_micro
    use ndrop,           only: activate_init
    
    
    implicit none
    integer, intent(in) :: ixcldliq_in, ixcldice_in, ixnumliq_in, ixnumice_in
    integer, intent(in) :: chem_opt_in
    integer, intent(in) :: ids, ide, jds, jde, kds, kde &
         ,ims, ime, jms, jme, kms, kme                  &
         ,its, ite, jts, jte, kts, kte  
    
    
    character(len=1000) :: msg
    integer :: jtf,ktf,itf
    
    chem_opt = chem_opt_in

    jtf   = min(jte,jde-1)
    ktf   = min(kte,kde-1)
    itf   = min(ite,ide-1)
    
    
    pver  = ktf - kts + 1
    pverp = pver + 1
    
    
    ixcldliq = ixcldliq_in
    ixcldice = ixcldice_in
    ixnumliq = ixnumliq_in
    ixnumice = ixnumice_in
    
    
    call ini_micro
    call ini_microp_aero
    
    
    call activate_init
    return
  end subroutine CAMMGMP_INIT
subroutine findsp_water (lchnk, ncol, q, t, p, tsp, qsp)























  use wv_saturation, only: estblf, hlatv, tmin, hlatf, rgasv, pcf, &
                           cp, epsqs, ttrice,polysvp




   integer, intent(in) :: lchnk                 
   integer, intent(in) :: ncol                  

   real(r8), intent(in) :: q(pcols)        
   real(r8), intent(in) :: t(pcols)        
   real(r8), intent(in) :: p(pcols)        



   real(r8), intent(out) :: tsp(pcols)      
   real(r8), intent(out) :: qsp(pcols)      



   character*250 :: iulog
   integer i                 

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
   real(r8) hltalt(pcols)   
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

   a1 = 7.5_r8*log(10._r8)
   rair =  287.04_r8
   c3 = rair*a1/cp
   dtm = 0._r8    
   dqm = 0._r8    
   dttol = 1.e-4_r8 
   dqtol = 1.e-4_r8 

   tt0  = 273.15_r8


   iter = 8








      do i = 1,ncol











         tlim(i) = min(max(t(i),128._r8),373._r8)


         es = polysvp(tlim(i),0)  
         denom = p(i) - omeps*es 
         qs = epsqs*es/denom     
         doit(i) = 0
         enout(i) = 1._r8

         if (p(i) > 5._r8*es .and. qs > 0._r8 .and. qs < 0.5_r8) then



             qs = min(epsqs*es/denom,1._r8)

























             hlatvp = hlatv - 2369.0*(tlim(i)-tt0)
             hlatsb = hlatv
             if (tlim(i) < tt0) then
               hltalt(i) = hlatsb
             else
               hltalt(i) = hlatvp
             end if

             enin(i) = cp*tlim(i) + hltalt(i)*q(i)
         

             tmp =  q(i) - qs
             c1 = hltalt(i)*c3
             c2 = (tlim(i) + 36._r8)**2
             r1b    = c2/(c2 + c1*qs)
             qvd   = r1b*tmp
             tsp(i) = tlim(i) + ((hltalt(i)/cp)*qvd)







             es = polysvp(tsp(i),0)  
             qsp(i) = min(epsqs*es/(p(i) - omeps*es),1._r8)
          else
             doit(i) = 1
             tsp(i) = tlim(i)
             qsp(i) = q(i)
             enin(i) = 1._r8
          endif 
       end do   



      do l = 1, iter
         dtm = 0
         dqm = 0
         do i = 1,ncol
            if (doit(i) == 0) then

               es = polysvp(tsp(i),0)  



               qs = min(epsqs*es/(p(i) - omeps*es),1._r8)































               hlatvp = hlatv - 2369.0*(tsp(i)-tt0)
               hlatsb = hlatv
               if (tsp(i) < tt0) then
                 hltalt(i) = hlatsb
               else
                 hltalt(i) = hlatvp
               end if
               desdt = hltalt(i)*es/(rgasv*tsp(i)*tsp(i))

               dqsdt = (epsqs + omeps*qs)/(p(i) - omeps*es)*desdt

               g = enin(i) - (cp*tsp(i) + hltalt(i)*qsp(i))
               dgdt = -(cp + hltalt(i)*dqsdt)
               t1 = tsp(i) - g/dgdt
               dt = abs(t1 - tsp(i))/t1
               tsp(i) = max(t1,tmin)

               es = polysvp(tsp(i),0)  
               q1 = min(epsqs*es/(p(i) - omeps*es),1._r8)
               dq = abs(q1 - qsp(i))/max(q1,1.e-12_r8)
               qsp(i) = q1





               dtm = max(dtm,dt)
               dqm = max(dqm,dq)

               if (dt < dttol .and. dq < dqtol) then
                  doit(i) = 2
               endif
               enout(i) = cp*tsp(i) + hltalt(i)*qsp(i)


               if (tsp(i) < 174.16_r8) then



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
                  write (iulog,*) ' findsp not converging at point i ', i
                  write (iulog,*) ' t, q, p, enin ', t(i), q(i), p(i), enin(i)
                  write (iulog,*) ' tsp, qsp, enout ', tsp(i), qsp(i), enout(i)
                  call  wrf_error_fatal3("<stdin>",2309,&
'FINDSP')
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
               write (iulog,*) ' the enthalpy is not conserved for point ', &
                  i, enin(i), enout(i)
               write (iulog,*) ' t, q, p, enin ', t(i), q(i), p(i), enin(i)
               write (iulog,*) ' tsp, qsp, enout ', tsp(i), qsp(i), enout(i)
               call wrf_error_fatal3("<stdin>",2327,&
'FINDSP')
            endif
         end do
      endif

 

   return
end subroutine findsp_water
end module module_mp_cammgmp_driver
