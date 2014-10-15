
MODULE module_bl_camuwpbl_driver
  
  

  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  


  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  

  use shr_kind_mod,       only : r8 => shr_kind_r8
  use module_cam_support, only : pcols, pver, pverp, endrun, iulog,fieldname_len,pcnst =>pcnst_runtime
  use constituents,       only : qmin
  use diffusion_solver,   only : vdiff_selector
  use physconst,          only :          &
       cpair  , &     
       gravit , &     
       rair   , &     
       zvir   , &     
       latvap , &     
       latice , &     
       karman , &     
       mwdry  , &     
       avogad , &     
       boltz          
  
  implicit none
  private      
  save
  
  
  
  
  
  public camuwpblinit   
  public camuwpbl       
  public vd_register    
  
  
  
  
  
  character(len=16)    :: eddy_scheme                  
  
  
  
  integer, parameter   :: nturb = 5                    
  logical, parameter   :: wstarent = .true.            
  logical              :: do_pseudocon_diff = .false.  

  
  character(len=16)    :: shallow_scheme               
  
  
  character(len=16)    :: microp_scheme                
  
  logical              :: do_molec_diff = .false.      
  logical              :: do_tms                       
  real(r8)             :: tms_orocnst                  
  real(r8)             :: tms_z0fac                    
  
  type(vdiff_selector) :: fieldlist_wet                
  type(vdiff_selector) :: fieldlist_dry                
  integer              :: ntop                         
  integer              :: nbot                         
  integer              :: tke_idx, kvh_idx, kvm_idx    
  integer              :: turbtype_idx, smaw_idx       
  integer              :: tauresx_idx, tauresy_idx     
 
  integer              :: ixcldice, ixcldliq           
  integer              :: ixnumice, ixnumliq

  integer              :: ixndrop

  integer              :: wgustd_index   
CONTAINS
  
  subroutine camuwpbl(dt,u_phy,v_phy,th_phy,rho,qv_curr,hfx,qfx,ustar,p8w &
       ,p_phy,z,t_phy,qc_curr,qi_curr,z_at_w,cldfra_old_mp,cldfra,ht      &
       ,rthratenlw,exner,is_CAMMGMP_used                                  &
       ,itimestep,qnc_curr,qni_curr,wsedl3d                               &
       ,ids,ide, jds,jde, kds,kde                                         &
       ,ims,ime, jms,jme, kms,kme                                         &
       ,its,ite, jts,jte, kts,kte                                         &

       ,tauresx2d,tauresy2d                                               &
       ,rublten,rvblten,rthblten,rqiblten,rqniblten,rqvblten,rqcblten     &
       ,kvm3d,kvh3d                                                       &

       ,tpert2d,qpert2d,wpert2d,smaw3d,turbtype3d                         &
       ,tke_pbl,pblh2d,kpbl2d                                             )  

    
    
    
    
    
    
    
    use module_cam_support,    only : pcols
    use trb_mtn_stress,        only : compute_tms
    use eddy_diff,             only : compute_eddy_diff
    use wv_saturation,         only : fqsatd, aqsat
    use molec_diff,            only : compute_molec_diff, vd_lu_qdecomp
    use constituents,          only : qmincg, qmin
    use diffusion_solver 

    use modal_aero_data

    
    implicit none   
    
    
    
    
    logical, intent(in) :: is_CAMMGMP_used
    integer, intent(in) :: ids,ide, jds,jde, kds,kde
    integer, intent(in) :: ims,ime, jms,jme, kms,kme
    integer, intent(in) :: its,ite, jts,jte, kts,kte
    integer, intent(in) :: itimestep

    real, intent(in) :: dt                                                
    
    real, dimension( ims:ime,jms:jme ), intent(in) :: hfx                   
    real, dimension( ims:ime,jms:jme ), intent(in) :: qfx                   
    real, dimension( ims:ime,jms:jme ), intent(in) :: ustar                 
    real, dimension( ims:ime,jms:jme ), intent(in) :: ht                    

    real, dimension( ims:ime, kms:kme, jms:jme ), intent(in) :: v_phy       
    real, dimension( ims:ime, kms:kme, jms:jme ), intent(in) :: u_phy       
    real, dimension( ims:ime, kms:kme, jms:jme ), intent(in) :: th_phy      
    real, dimension( ims:ime, kms:kme, jms:jme ), intent(in) :: rho         
    real, dimension( ims:ime, kms:kme, jms:jme ), intent(in) :: qv_curr     
    real, dimension( ims:ime, kms:kme, jms:jme ), intent(in) :: qc_curr     
    real, dimension( ims:ime, kms:kme, jms:jme ), intent(in) :: qi_curr     
    real, dimension( ims:ime, kms:kme, jms:jme ), intent(in) :: qnc_curr    
    real, dimension( ims:ime, kms:kme, jms:jme ), intent(in) :: qni_curr    
    real, dimension( ims:ime, kms:kme, jms:jme ), intent(in) :: p_phy       
    real, dimension( ims:ime, kms:kme, jms:jme ), intent(in) :: p8w         
    real, dimension( ims:ime, kms:kme, jms:jme ), intent(in) :: z           
    real, dimension( ims:ime, kms:kme, jms:jme ), intent(in) :: t_phy       
    real, dimension( ims:ime, kms:kme, jms:jme ), intent(in) :: z_at_w      
    real, dimension( ims:ime, kms:kme, jms:jme ), intent(in) :: cldfra_old_mp 
    real, dimension( ims:ime, kms:kme, jms:jme ), intent(in) :: cldfra      
    real, dimension( ims:ime, kms:kme, jms:jme ), intent(in) :: exner       
    real, dimension( ims:ime, kms:kme, jms:jme ), intent(in) :: rthratenlw  
    real, dimension( ims:ime, kms:kme, jms:jme ), intent(in) :: wsedl3d     

    
    
    
    
    
    real, dimension( ims:ime, kms:kme, jms:jme ), intent(inout) :: rublten     
    real, dimension( ims:ime, kms:kme, jms:jme ), intent(inout) :: rvblten     
    real, dimension( ims:ime, kms:kme, jms:jme ), intent(inout) :: rthblten    
    real, dimension( ims:ime, kms:kme, jms:jme ), intent(inout) :: rqvblten    
    real, dimension( ims:ime, kms:kme, jms:jme ), intent(inout) :: rqcblten    
    real, dimension( ims:ime, kms:kme, jms:jme ), intent(inout) :: rqiblten    
    real, dimension( ims:ime, kms:kme, jms:jme ), intent(inout) :: rqniblten   

    integer, dimension( ims:ime,jms:jme ), intent(out) :: kpbl2d 
    real,    dimension( ims:ime,jms:jme ), intent(out) :: pblh2d 

    real, dimension( ims:ime, kms:kme, jms:jme ), intent(out) :: tke_pbl     
    real, dimension( ims:ime, kms:kme, jms:jme ), intent(out) :: turbtype3d  
    real, dimension( ims:ime, kms:kme, jms:jme ), intent(out) :: smaw3d      
        

    
    
    
    
    
    real, dimension( ims:ime, jms:jme )         , intent(inout ):: tauresx2d,tauresy2d 
    real, dimension( ims:ime, jms:jme )         , intent(out)   :: tpert2d             
    real, dimension( ims:ime, jms:jme )         , intent(out)   :: qpert2d             
    real, dimension( ims:ime, jms:jme )         , intent(out)   :: wpert2d             

    real, dimension( ims:ime, kms:kme, jms:jme ), intent(inout) :: kvm3d,kvh3d         


    
    
    

    character(128) :: errstring                              
    logical        :: kvinit                                 
    logical        :: is_first_step                          
    integer        :: i,j,k,itsp1,itile_len,ktep1,kflip,ncol,ips,icnst,m,kp1
    integer        :: lchnk                                  
    real(r8)       :: tauFac, uMean, dp, multFrc
    real(r8)       :: ztodt                                  
    real(r8)       :: rztodt                                 
	real(r8),parameter :: invalidVal = -999888777.0_r8

    real(r8) :: topflx(  pcols)                              
    real(r8) :: wpert(   pcols)                              
    real(r8) :: tauresx( pcols)                              
    real(r8) :: tauresy( pcols)                              
    real(r8) :: ipbl(    pcols)                              
    real(r8) :: kpblh(   pcols)                              
    real(r8) :: wstarPBL(pcols)                              
    real(r8) :: sgh(     pcols)                              
    real(r8) :: landfrac(pcols)                              
    real(r8) :: taux(    pcols)                              
    real(r8) :: tauy(    pcols)                              
    real(r8) :: tautotx( pcols)                              
    real(r8) :: tautoty( pcols)                              
    real(r8) :: ksrftms( pcols)                              
    real(r8) :: tautmsx( pcols)                              
    real(r8) :: tautmsy( pcols)                              
    real(r8) :: ustar8(  pcols)                              
    real(r8) :: pblh(    pcols)                              
    real(r8) :: tpert(   pcols)                              
    real(r8) :: qpert(   pcols)                              
    real(r8) :: shflx(   pcols)                              
    real(r8) :: phis(    pcols)                              

    real(r8) :: cldn8(      pcols,kte)                       
    real(r8) :: qrl8(       pcols,kte)                       
    real(r8) :: wsedl8(     pcols,kte)                       
    real(r8) :: dtk(        pcols,kte)                       
    real(r8) :: qt(         pcols,kte)                       
    real(r8) :: sl_prePBL(  pcols,kte)                       
    real(r8) :: qt_prePBL(  pcols,kte)                       
    real(r8) :: slten(      pcols,kte)                       
    real(r8) :: qtten(      pcols,kte)                       
    real(r8) :: sl(         pcols,kte)                       
    real(r8) :: ftem(       pcols,kte)                       
    real(r8) :: ftem_prePBL(pcols,kte)                       
    real(r8) :: ftem_aftPBL(pcols,kte)                       
    real(r8) :: tem2(       pcols,kte)                       
    real(r8) :: t_aftPBL(   pcols,kte)                       
    real(r8) :: tten(       pcols,kte)                       
    real(r8) :: rhten(      pcols,kte)                       
    real(r8) :: qv_aft_PBL( pcols,kte)                       
    real(r8) :: ql_aft_PBL( pcols,kte)                       
    real(r8) :: qi_aft_PBL( pcols,kte)                       
    real(r8) :: s_aft_PBL(  pcols,kte)                       
    real(r8) :: u_aft_PBL(  pcols,kte)                       
    real(r8) :: v_aft_PBL(  pcols,kte)                       
    real(r8) :: qv_pro(     pcols,kte)                       
    real(r8) :: ql_pro(     pcols,kte)                       
    real(r8) :: qi_pro(     pcols,kte)                       
    real(r8) :: s_pro(      pcols,kte)                       
    real(r8) :: t_pro(      pcols,kte)                       
    real(r8) :: u8(         pcols,kte)                       
    real(r8) :: v8(         pcols,kte)                       
    real(r8) :: t8(         pcols,kte)                       
    real(r8) :: pmid8(      pcols,kte)                       
    real(r8) :: pmiddry8(   pcols,kte)                       
    real(r8) :: zm8(        pcols,kte)                       
    real(r8) :: exner8(     pcols,kte)                       
    real(r8) :: s8(         pcols,kte)                       
    real(r8) :: rpdel8(     pcols,kte)                       
    real(r8) :: pdel8(      pcols,kte)                       
    real(r8) :: rpdeldry8(  pcols,kte)                       
    real(r8) :: pdeldry8(   pcols,kte)                       
    REAL(r8) :: stnd(       pcols,kte)                       
    
    real(r8) :: tke8(     pcols,kte+1)                       
    real(r8) :: turbtype( pcols,kte+1)                       
    real(r8) :: smaw(     pcols,kte+1)                       
                                                             
    real(r8) :: cgs(      pcols,kte+1)                       
    real(r8) :: cgh(      pcols,kte+1)                       
    real(r8) :: kvh(      pcols,kte+1)                       
    real(r8) :: kvm(      pcols,kte+1)                       
    real(r8) :: kvq(      pcols,kte+1)                       
    real(r8) :: kvh_in(   pcols,kte+1)                       
    real(r8) :: kvm_in(   pcols,kte+1)                       
    real(r8) :: bprod(    pcols,kte+1)                       
    real(r8) :: sprod(    pcols,kte+1)                       
    real(r8) :: sfi(      pcols,kte+1)                       
    real(r8) :: pint8(    pcols,kte+1)                       
    real(r8) :: pintdry8( pcols,kte+1)                       
    real(r8) :: zi8(      pcols,kte+1)                       

    real(r8) :: cloud(     pcols,kte,pcnst)                      
    real(r8) :: cloudtnd(  pcols,kte,pcnst)                      
    real(r8) :: wind_tends(pcols,kte,2)                      

    real(r8) :: cflx(pcols,pcnst)                            
    

    real(r8) :: tmp1(pcols)                                         
    integer  :: l, lspec

    
    
    

    do icnst = 1 , pcnst
       
       cflx(:pcols,icnst)  = 0.0_r8
    enddo

    is_first_step  = .false.
    if(itimestep == 1) then
       is_first_step = .true.
    endif
    ncol   = pcols
    ztodt  = DT
    rztodt = 1.0_r8 / ztodt

    
    if(ncol .NE. 1) then
       call wrf_error_fatal3("<stdin>",355,&
'Number of CAM Columns (NCOL) in CAMUWPBL scheme must be 1')
    endif

    
    errstring = ''

    
    
    
    
    
    itsp1     = its - 1 
    itile_len = ite - itsp1
    do j    = jts , jte
       do i = its , ite

          lchnk   = (j - jts) * itile_len + (i - itsp1)          
          phis(1) = ht(i,j) * gravit                             

          
          ktep1 = kte + 1
          do k  = kts,kte
             kflip               = ktep1 - k
             
             
             do icnst = 1 , pcnst
                
                
                cloud(1,kflip,icnst)    = invalidVal
                cloudtnd(1,kflip,icnst) = invalidVal
             enddo

             u8(      1,kflip)   = u_phy(i,k,j)  
             v8(      1,kflip)   = v_phy(i,k,j)  

             pmid8(   1,kflip)   = p_phy(i,k,j)  
             
             dp                  = p8w(i,k,j) - p8w(i,k+1,j) 
             pdel8(  1,kflip)    = dp
             rpdel8(  1,kflip)   = 1.0_r8/dp     
             zm8(     1,kflip)   = z(i,k,j)-ht(i,j)      
             t8(      1,kflip)   = t_phy(i,k,j)  
             
             s8(      1,kflip)   = cpair *t8(1,kflip) + gravit*zm8(1,kflip) + phis(1) 
             qrl8(    1,kflip)   = rthratenlw(i,k,j)* cpair * dp 

             wsedl8(  1,kflip)   = wsedl3d(i,k,j)               
             
             
             
             multFrc                  =  1._r8/(1._r8 + qv_curr(i,k,j))
             cloud( 1,kflip,1)        = max( qv_curr(i,k,j) * multFrc, 1.0e-30_r8 ) 
             cloud( 1,kflip,ixcldliq) = qc_curr(i,k,j)  * multFrc              
             cloud( 1,kflip,ixcldice) = qi_curr(i,k,j)  * multFrc              
             cloud( 1,kflip,ixnumliq) = qnc_curr(i,k,j) * multFrc 
             cloud( 1,kflip,ixnumice) = qni_curr(i,k,j) * multFrc 

             exner8(1,kflip)   = exner(i,k,j)   
             if(is_CAMMGMP_used) then
                cldn8( 1,kflip)   = cldfra_old_mp(i,k,j)  
             else
                cldn8( 1,kflip)   = cldfra(i,k,j)  
             endif

             
             pdeldry8(1,kflip)    =  pdel8(1,kflip)*(1._r8-cloud(1,kflip,1))            
             rpdeldry8(1,kflip)   =  1._r8/pdeldry8(1,kflip)

          enddo
          
          do k = kts,kte+1
             kflip = kte - k + 2

             pint8(   1,kflip) = p8w(   i,k,j) 

             zi8(     1,kflip) = z_at_w(i,k,j) -ht(i,j) 

             
             kvq(1,kflip)   = 0.0_r8        
             cgh(1,kflip)   = 0.0_r8        
             cgs(1,kflip)   = 0.0_r8        
             if( is_first_step ) then
                kvm3d(i,k,j) = 0.0_r8     
                kvh3d(i,k,j) = 0.0_r8     
             endif
             kvh(1,kflip)   = kvh3d(i,k,j)  
             kvm(1,kflip)   = kvm3d(i,k,j)  
          end do

          
          
          pintdry8(1,1) = pint8(1,1)          
          do k = 1, pver
             pintdry8(1,k+1) =  pintdry8(1,k)  + pdeldry8(1,k)
             pmiddry8(1,k)   = (pintdry8(1,k+1)+ pintdry8(1,k))*0.5_r8
          end do

          shflx(   ncol) = hfx(i,j)  

          
          sgh(     ncol) = 0.0_r8    
          landfrac(ncol) = 0.0_r8    

          uMean      = sqrt( u_phy(i,kts,j) * u_phy(i,kts,j) +  v_phy(i,kts,j) * v_phy(i,kts,j) ) 
          tauFac     = rho(i,kts,j) * ustar(i,j) *ustar(i,j)/uMean

          taux(ncol) = -tauFac * u_phy(i,kts,j)  
          tauy(ncol) = -tauFac * v_phy(i,kts,j)  

          
          if( is_first_step ) then
             tauresx2d(i,j) = 0._r8
             tauresy2d(i,j) = 0._r8
          endif
          tauresx(:ncol) = tauresx2d(i,j)
          tauresy(:ncol) = tauresy2d(i,j)
          
          
          
          
          
          
          
          
          
          

          if( do_tms ) then
             call compute_tms( pcols   , pver    , ncol  , &
                  u8      , v8      , t8       , pmid8   , & 
                  exner8  , zm8     , sgh      , ksrftms , & 
                  tautmsx , tautmsy , landfrac )
             
             
             
             
             
             
             tautotx(:ncol) = taux(:ncol) + tautmsx(:ncol)
             tautoty(:ncol) = tauy(:ncol) + tautmsy(:ncol)
          else
             ksrftms(:ncol) = 0.0_r8
             tautotx(:ncol) = taux(:ncol)
             tautoty(:ncol) = tauy(:ncol)
          endif
          
          
          
          
          cflx(:pcols,1)   = qfx(i,j) 
          
          
          ustar8(  :pcols) = 0.0_r8   
          pblh(    :pcols) = 0.0_r8   
          ipbl(    :pcols) = 0.0_r8   
          kpblh(   :pcols) = 0.0_r8   
          wstarPBL(:pcols) = 0.0_r8   


          
          
          
          
          select case (eddy_scheme)
          case ( 'diag_TKE' ) 
             
             
             
             
             
             
             kvinit = .false.
             if(is_first_step) then
                kvinit = .true.
             endif
             
             
             
             
             
             
             
             kvm_in(:ncol,:) = kvm(:ncol,:) 
             kvh_in(:ncol,:) = kvh(:ncol,:) 
             call compute_eddy_diff( lchnk  , pcols  , pver   , ncol   , t8      , cloud(:,:,1)   , ztodt    ,           &
                  cloud(:,:,2), cloud(:,:,3), s8     , rpdel8 , cldn8  , qrl8    , wsedl8         , zm8      , zi8     , &
                  pmid8       , pint8       , u8     , v8     , taux   , tauy    , shflx          , cflx(:,1), wstarent, &
                  nturb       , ustar8      , pblh   , kvm_in , kvh_in , kvm     , kvh            , kvq      , cgh     , &                                                     
                  cgs         , tpert       , qpert  , wpert  , tke8   , bprod   , sprod          , sfi      , fqsatd  , &
                  kvinit      , tauresx     , tauresy, ksrftms, ipbl(:), kpblh(:), wstarPBL(:)    , turbtype , smaw      )

             
             
             
             
             
             do k = kts,kte+1
                kflip          = kte - k + 2
                
                kvh3d(i,k,j)   = kvh(1,kflip)
                kvm3d(i,k,j)   = kvm(1,kflip)  
             end do
             
             
          end select
          
          
          
          
          cloudtnd(  :ncol,:,:) = cloud(:ncol,:,:)
          stnd(      :ncol,:  ) = s8(   :ncol,:  )
          wind_tends(:ncol,:,1) = u8(   :ncol,:  )
          wind_tends(:ncol,:,2) = v8(   :ncol,:  )

          
          
          
          
          sl_prePBL(:ncol,:pver)  = stnd(:ncol,:pver) - latvap * cloudtnd(:ncol,:pver,ixcldliq) &
               - ( latvap + latice) * cloudtnd(:ncol,:pver,ixcldice)
          qt_prePBL(:ncol,:pver)  = cloudtnd(:ncol,:pver,1) + cloudtnd(:ncol,:pver,ixcldliq) &
               + cloudtnd(:ncol,:pver,ixcldice)

          call aqsat( t8, pmid8, tem2, ftem, pcols, ncol, pver, 1, pver )
          ftem_prePBL(:ncol,:) = cloud(:ncol,:,1)/ftem(:ncol,:)*100._r8

          
          
          
          
          
          
          
          
          
          

          if( any(fieldlist_wet) ) then
             call compute_vdiff( lchnk, pcols, pver, pcnst, ncol, pmid8, pint8, rpdel8, t8, ztodt, &
                  taux, tauy, shflx, cflx, ntop, nbot, kvh, kvm, kvq, cgs, cgh, zi8, ksrftms, qmincg, &
                  fieldlist_wet, wind_tends(:,:,1), wind_tends(:,:,2), cloudtnd, stnd, tautmsx,       &
                  tautmsy, dtk, topflx, errstring, tauresx, tauresy, 1, do_molec_diff,                &
                  compute_molec_diff, vd_lu_qdecomp)
          end if

          if( errstring .ne. '' ) then
             call wrf_error_fatal3("<stdin>",602,&
errstring)
          endif
          
          if( any( fieldlist_dry ) ) then
             if( do_molec_diff ) then
                errstring = "Design flaw: dry vdiff not currently supported with molecular diffusion"
                call wrf_error_fatal3("<stdin>",609,&
errstring)
             end if
             
             call compute_vdiff( lchnk, pcols, pver, pcnst, ncol, pmiddry8, pintdry8, rpdeldry8, t8, &
                  ztodt, taux, tauy, shflx, cflx, ntop, nbot, kvh, kvm, kvq, cgs, cgh, zi8, ksrftms,    &
                  qmincg, fieldlist_dry, wind_tends(:,:,1), wind_tends(:,:,2), cloudtnd, stnd, tautmsx, &
                  tautmsy, dtk, topflx, errstring, tauresx, tauresy, 1, do_molec_diff,                  &
                  compute_molec_diff, vd_lu_qdecomp)

             if( errstring .ne. '' ) call wrf_error_fatal3("<stdin>",619,&
errstring)

          end if
          
          
          tauresx2d(i,j) = tauresx(ncol)
          tauresy2d(i,j) = tauresy(ncol)


          
          
          
          

          
          
          
          
          
          
          
          
          
          
          

          
          
          
          sl(:ncol,:pver)  = stnd(:ncol,:pver) -   latvap           * cloudtnd(:ncol,:pver,ixcldliq) &
               - ( latvap + latice) * cloudtnd(:ncol,:pver,ixcldice)
          qt(:ncol,:pver)  = cloudtnd(:ncol,:pver,1) + cloudtnd(:ncol,:pver,ixcldliq) &
               + cloudtnd(:ncol,:pver,ixcldice)
          
          
          
          
          
          
          

          slten(:ncol,:)          = ( sl(:ncol,:)             - sl_prePBL(:ncol,:) )   * rztodt 
          qtten(:ncol,:)          = ( qt(:ncol,:)             - qt_prePBL(:ncol,:) )   * rztodt 
          stnd(:ncol,:)           = ( stnd(:ncol,:)           - s8(:ncol,:) )          * rztodt
          wind_tends(:ncol,:,1)   = ( wind_tends(:ncol,:,1)   - u8(:ncol,:) )          * rztodt
          wind_tends(:ncol,:,2)   = ( wind_tends(:ncol,:,2)   - v8(:ncol,:) )          * rztodt
          cloudtnd(:ncol,:pver,:) = ( cloudtnd(:ncol,:pver,:) - cloud(:ncol,:pver,:) ) * rztodt

          
          
          
          
          
          
          
          
          
          
          
          if( eddy_scheme .eq. 'diag_TKE' .and. do_pseudocon_diff ) then
             cloudtnd(:ncol,:pver,1) = qtten(:ncol,:pver)
             stnd(:ncol,:pver)       = slten(:ncol,:pver)
             cloudtnd(:ncol,:pver,ixcldliq) = 0._r8         
             cloudtnd(:ncol,:pver,ixcldice) = 0._r8         
             cloudtnd(:ncol,:pver,ixnumliq) = 0._r8         
             cloudtnd(:ncol,:pver,ixnumice) = 0._r8         
             
             do ips = 1, ncol
                do k = 1, pver
                   qv_pro(ips,k) = cloud(ips,k,1)        + cloudtnd(ips,k,1)             * ztodt       
                   ql_pro(ips,k) = cloud(ips,k,ixcldliq) + cloudtnd(ips,k,ixcldliq)      * ztodt
                   qi_pro(ips,k) = cloud(ips,k,ixcldice) + cloudtnd(ips,k,ixcldice)      * ztodt              
                   s_pro(ips,k)  = s8(ips,k)             + stnd(ips,k)                   * ztodt
                   t_pro(ips,k)  = t8(ips,k)             + (1._r8/cpair)*stnd(ips,k) * ztodt

                end do
             end do
             call positive_moisture( cpair, latvap, latvap+latice, ncol, pver, ztodt, qmin(1), qmin(2), qmin(3),    &
                  pdel8(:ncol,pver:1:-1), qv_pro(:ncol,pver:1:-1), ql_pro(:ncol,pver:1:-1), &
                  qi_pro(:ncol,pver:1:-1), t_pro(:ncol,pver:1:-1), s_pro(:ncol,pver:1:-1),       &
                  cloudtnd(:ncol,pver:1:-1,1), cloudtnd(:ncol,pver:1:-1,ixcldliq),                 &
                  cloudtnd(:ncol,pver:1:-1,ixcldice), stnd(:ncol,pver:1:-1) )
             
          end if
          
          
          
          
          qv_aft_PBL(:ncol,:pver)  =   cloud(:ncol,:pver,1)         + cloudtnd(:ncol,:pver,1)        * ztodt
          ql_aft_PBL(:ncol,:pver)  =   cloud(:ncol,:pver,ixcldliq)  + cloudtnd(:ncol,:pver,ixcldliq) * ztodt
          qi_aft_PBL(:ncol,:pver)  =   cloud(:ncol,:pver,ixcldice)  + cloudtnd(:ncol,:pver,ixcldice) * ztodt

          s_aft_PBL(:ncol,:pver)   =   s8(:ncol,:pver)           + stnd(:ncol,:pver)          * ztodt
          t_aftPBL(:ncol,:pver)    = ( s_aft_PBL(:ncol,:pver) - gravit*zm8(:ncol,:pver) ) / cpair 
          
          u_aft_PBL(:ncol,:pver)   =  u8(:ncol,:pver)          + wind_tends(:ncol,:pver,1)            * ztodt
          v_aft_PBL(:ncol,:pver)   =  v8(:ncol,:pver)          + wind_tends(:ncol,:pver,2)            * ztodt
          
          call aqsat( t_aftPBL, pmid8, tem2, ftem, pcols, ncol, pver, 1, pver )
          ftem_aftPBL(:ncol,:pver) = qv_aft_PBL(:ncol,:pver) / ftem(:ncol,:pver) * 100._r8
          
          tten(:ncol,:pver)        = ( t_aftPBL(:ncol,:pver)    - t8(:ncol,:pver) )              * rztodt     
          rhten(:ncol,:pver)       = ( ftem_aftPBL(:ncol,:pver) - ftem_prePBL(:ncol,:pver) )          * rztodt 


          
          do k=kts,kte
          
             kflip = kte-k+1
             
             rublten(i,k,j)    = wind_tends(1,kflip,1)
             rvblten(i,k,j)    = wind_tends(1,kflip,2)
             rthblten(i,k,j)   = stnd(1,kflip)/cpair/exner8(1,kflip)
             
             multFrc           =  1._r8/(1._r8 - qv_curr(i,k,j))

             rqvblten(i,k,j)   = cloudtnd(1,kflip,1       ) * multFrc
             rqcblten(i,k,j)   = cloudtnd(1,kflip,ixcldliq) * multFrc
             rqiblten(i,k,j)   = cloudtnd(1,kflip,ixcldice) * multFrc
             
             rqniblten(i,k,j)  = cloudtnd(1,kflip,ixnumice) * multFrc

             
             kp1 = k + 1
          end do

          do k = kts,kte+1
             kflip             = kte - k + 2
             
             tke_pbl(i,k,j)    = tke8(1,kflip)    
             turbtype3d(i,k,j) = turbtype(1,kflip)
             smaw3d(i,k,j)     = smaw(1,kflip)
          end do

          

          kpbl2d(i,j)  = kte - int(kpblh(1)) + 1  
          pblh2d(i,j)  = pblh( 1)
          tpert2d(i,j) = tpert(1)
          qpert2d(i,j) = qpert(1)
          wpert2d(i,j) = wpert(1)
          
          
       enddo 
    enddo 
    return
  end subroutine camuwpbl





  subroutine positive_moisture( cp, xlv, xls, ncol, mkx, dt, qvmin, qlmin, qimin, & 
       dp, qv, ql, qi, t, s, qvten, qlten, qiten, sten )
    
    
    
    
    
    
    
    
    
    
    

    implicit none
    integer,  intent(in)     :: ncol, mkx
    real(r8), intent(in)     :: cp, xlv, xls
    real(r8), intent(in)     :: dt, qvmin, qlmin, qimin
    real(r8), intent(in)     :: dp(ncol,mkx)
    real(r8), intent(inout)  :: qv(ncol,mkx), ql(ncol,mkx), qi(ncol,mkx), t(ncol,mkx), s(ncol,mkx)
    real(r8), intent(inout)  :: qvten(ncol,mkx), qlten(ncol,mkx), qiten(ncol,mkx), sten(ncol,mkx)
    integer   i, k
    real(r8)  dql, dqi, dqv, sum, aa, dum 
    
    
    
    
    do i = 1, ncol
       do k = mkx, 1, -1    
          dql        = max(0._r8,1._r8*qlmin-ql(i,k))
          dqi        = max(0._r8,1._r8*qimin-qi(i,k))
          qlten(i,k) = qlten(i,k) +  dql/dt
          qiten(i,k) = qiten(i,k) +  dqi/dt
          qvten(i,k) = qvten(i,k) - (dql+dqi)/dt
          sten(i,k)  = sten(i,k)  + xlv * (dql/dt) + xls * (dqi/dt)
          ql(i,k)    = ql(i,k) +  dql
          qi(i,k)    = qi(i,k) +  dqi
          qv(i,k)    = qv(i,k) -  dql - dqi
          s(i,k)     = s(i,k)  +  xlv * dql + xls * dqi
          t(i,k)     = t(i,k)  + (xlv * dql + xls * dqi)/cp
          dqv        = max(0._r8,1._r8*qvmin-qv(i,k))
          qvten(i,k) = qvten(i,k) + dqv/dt
          qv(i,k)    = qv(i,k)    + dqv
          if( k .ne. 1 ) then 
             qv(i,k-1)    = qv(i,k-1)    - dqv*dp(i,k)/dp(i,k-1)
             qvten(i,k-1) = qvten(i,k-1) - dqv*dp(i,k)/dp(i,k-1)/dt
          endif
          qv(i,k) = max(qv(i,k),qvmin)
          ql(i,k) = max(ql(i,k),qlmin)
          qi(i,k) = max(qi(i,k),qimin)
       end do
       
       
       
       if( dqv .gt. 1.e-20_r8 ) then
          sum = 0._r8
          do k = 1, mkx
             if( qv(i,k) .gt. 2._r8*qvmin ) sum = sum + qv(i,k)*dp(i,k)
          enddo
          aa = dqv*dp(i,1)/max(1.e-20_r8,sum)
          if( aa .lt. 0.5_r8 ) then
             do k = 1, mkx
                if( qv(i,k) .gt. 2._r8*qvmin ) then
                   dum        = aa*qv(i,k)
                   qv(i,k)    = qv(i,k) - dum
                   qvten(i,k) = qvten(i,k) - dum/dt
                endif
             enddo
          else 
             write(iulog,*) 'Full positive_moisture is impossible in vertical_diffusion'
             call wrf_message(iulog)
          endif
       endif
    end do
    return
    
  end subroutine positive_moisture
  
  
 
 
  subroutine camuwpblinit(rublten,rvblten,rthblten,rqvblten, &
       restart,tke_pbl,is_CAMMGMP_used,                      &
       ids,ide,jds,jde,kds,kde,                              &
       ims,ime,jms,jme,kms,kme,                              &
       its,ite,jts,jte,kts,kte)
    
    
    
    

    
    
    
    
    


    use eddy_diff,              only : init_eddy_diff
    use molec_diff,             only : init_molec_diff
    use trb_mtn_stress,         only : init_tms
    use diffusion_solver,       only : init_vdiff, vdiff_select
    use constituents,           only : cnst_get_ind, cnst_get_type_byind, cnst_name
    use module_cam_support,     only : masterproc
    use module_model_constants, only : epsq2

    use modal_aero_data

    
    implicit none

    
    
    
    logical,intent(in) :: restart,is_CAMMGMP_used 
    integer,intent(in) :: ids,ide,jds,jde,kds,kde
    integer,intent(in) :: ims,ime,jms,jme,kms,kme
    integer,intent(in) :: its,ite,jts,jte,kts,kte
    
    real,dimension(ims:ime,kms:kme,jms:jme),intent(inout) :: &
         rublten, rvblten, rthblten, rqvblten
    real,dimension(ims:ime,kms:kme,jms:jme),intent(out) :: TKE_PBL

    
    
    
    integer        :: i,j,k,itf,jtf,ktf 
    integer        :: ntop_eddy         
    integer        :: nbot_eddy         
    integer        :: ntop_molec        
    integer        :: nbot_molec        
    character(128) :: errstring         
    real(r8)       :: hypm(kte)         

    integer        :: m, l


    
    jtf   = min(jte,jde-1)
    ktf   = min(kte,kde-1)
    itf   = min(ite,ide-1)
    
    
    pver  = ktf - kts + 1
    pverp = pver + 1
    
    
    call vd_register()  
    
    
    
    
    
    call cnst_get_ind( 'CLDLIQ', ixcldliq )
    call cnst_get_ind( 'CLDICE', ixcldice )
    if( microp_scheme .eq. 'MG' ) then
       call cnst_get_ind( 'NUMLIQ', ixnumliq )
       call cnst_get_ind( 'NUMICE', ixnumice )
    endif
    
    if (masterproc) then
       write(iulog,*)'Initializing vertical diffusion (vertical_diffusion_init)'
       call wrf_debug(1,iulog)
    end if
    
    
    
    
    
    
    
    
    
    ntop_molec = 1       
    nbot_molec = 0       
    
    
    
    
    
    ntop_eddy  = 1       
    nbot_eddy  = pver    
    
    if( masterproc ) write(iulog,fmt='(a,i3,5x,a,i3)') 'NTOP_EDDY  =', ntop_eddy, 'NBOT_EDDY  =', nbot_eddy
    call wrf_debug(1,iulog)
    
    select case ( eddy_scheme )
    case ( 'diag_TKE' ) 
       if( masterproc ) write(iulog,*) 'vertical_diffusion_init: eddy_diffusivity scheme'
       call wrf_debug(1,iulog)
       if( masterproc ) write(iulog,*) 'UW Moist Turbulence Scheme by Bretherton and Park'
       call wrf_debug(1,iulog)
       
       if( shallow_scheme .ne. 'UW' ) then
          write(iulog,*) 'ERROR: shallow convection scheme ', shallow_scheme,' is incompatible with eddy scheme ', eddy_scheme
          call wrf_message(iulog)
          call wrf_error_fatal3("<stdin>",968,&
'convect_shallow_init: shallow_scheme and eddy_scheme are incompatible' )
       endif
       
       call init_eddy_diff( r8, pver, gravit, cpair, rair, zvir, latvap, latice, &
            ntop_eddy, nbot_eddy, hypm, karman )
       
       if( masterproc ) write(iulog,*) 'vertical_diffusion: nturb, ntop_eddy, nbot_eddy ', nturb, ntop_eddy, nbot_eddy
       call wrf_debug(1,iulog)
    end select
    
    
    
    
    
    
    ntop = min(ntop_molec,ntop_eddy)
    nbot = max(nbot_molec,nbot_eddy)
    
    
    
    
    
    if( do_tms ) then
       call init_tms( r8, tms_orocnst, tms_z0fac, karman, gravit, rair )
       if (masterproc) then
          write(iulog,*)'Using turbulent mountain stress module'
          call wrf_message(iulog)
          write(iulog,*)'  tms_orocnst = ',tms_orocnst
          call wrf_message(iulog)
       end if
    endif
    
    
    
    
    
    call init_vdiff( r8, pcnst, rair, gravit, fieldlist_wet, fieldlist_dry, errstring )
    if( errstring .ne. '' ) call wrf_error_fatal3("<stdin>",1006,&
errstring )
    
    
    
    
    
    
    if( vdiff_select( fieldlist_wet, 'u' ) .ne. '' ) call wrf_error_fatal3("<stdin>",1014,&
vdiff_select( fieldlist_wet, 'u' ) )
    if( vdiff_select( fieldlist_wet, 'v' ) .ne. '' ) call wrf_error_fatal3("<stdin>",1016,&
vdiff_select( fieldlist_wet, 'v' ) )
    if( vdiff_select( fieldlist_wet, 's' ) .ne. '' ) call wrf_error_fatal3("<stdin>",1018,&
vdiff_select( fieldlist_wet, 's' ) )


    
    call cnst_get_ind( 'NUMLIQ', ixndrop )


    do k = 1, pcnst
       

       

       
       

       if(is_CAMMGMP_used) then
          if( k == ixndrop ) cycle 
       endif
       
       
       
       if( k > 5 ) cycle


       

       
       
       
          
       
       
             
       
          
       

       
       if( cnst_get_type_byind(k) .eq. 'wet' ) then
          if( vdiff_select( fieldlist_wet, 'q', k ) .ne. '' ) call wrf_error_fatal3("<stdin>",1058,&
vdiff_select( fieldlist_wet, 'q', k ) )
       else
          
          
       endif
    enddo
    
    
    jtf=min0(jte,jde-1)
    ktf=min0(kte,kde-1)
    itf=min0(ite,ide-1)
    
    if(.not.restart)then
       do j = jts , jtf
          do k = kts , ktf
             do i = its , itf
                tke_pbl(i,k,j)  = epsq2
                rublten(i,k,j)  = 0.
                rvblten(i,k,j)  = 0.
                rthblten(i,k,j) = 0.
                rqvblten(i,k,j) = 0.
             enddo
          enddo
       enddo
    endif
  end subroutine camuwpblinit




  subroutine vd_register()



    
    
    microp_scheme  = 'MG'       
    eddy_scheme    = 'diag_TKE' 

    
    shallow_scheme = 'UW'       

    do_tms         = .false.    
    tms_orocnst    = 1._r8      
    
  end subroutine vd_register
  
end module module_bl_camuwpbl_driver

