


  module diffusion_solver

  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  



  use module_cam_support,   only : iulog


  implicit none
  private       
  save

  integer, parameter :: r8 = selected_real_kind(12)      

  
  
  

  public init_vdiff                                      
  public compute_vdiff                                   
  public vd_lu_solve                                     
  public vd_lu_decomp                                    
  public vdiff_selector                                  
  public vdiff_select                                    
  public operator(.not.)                                 
  public any                                             

  integer, public   :: nbot_molec                        
 
  

  type vdiff_selector 
       private
       logical, pointer, dimension(:) :: fields
  end type vdiff_selector

  

  interface operator(.not.)
       module procedure not
  end interface

  

  interface any                           
       module procedure my_any
  end interface

  
  
  

  real(r8), private   :: cpair                           
  real(r8), private   :: gravit                          
  real(r8), private   :: rair                            
  real(r8), private   :: zvir                            
  real(r8), private   :: latvap                          
  real(r8), private   :: karman                          

  

  real(r8), parameter :: z0fac   = 0.025_r8              
  real(r8), parameter :: z0max   = 100._r8               
  real(r8), parameter :: horomin = 10._r8                
  real(r8), parameter :: dv2min  = 0.01_r8               
  real(r8), private   :: oroconst                        

  contains

  
  
  

  subroutine init_vdiff( kind, ncnst, rair_in, gravit_in, fieldlist_wet, fieldlist_dry, errstring )

    integer,              intent(in)  :: kind            
    integer,              intent(in)  :: ncnst           
    real(r8),             intent(in)  :: rair_in         
    real(r8),             intent(in)  :: gravit_in       
    type(vdiff_selector), intent(out) :: fieldlist_wet   
    type(vdiff_selector), intent(out) :: fieldlist_dry   
    character(128),       intent(out) :: errstring       
    
    errstring = ''
    if( kind .ne. r8 ) then
        write(iulog,*) 'KIND of reals passed to init_vdiff -- exiting.'

        call wrf_message(iulog)

        errstring = 'init_vdiff'
        return
    endif

    rair   = rair_in     
    gravit = gravit_in 

    allocate( fieldlist_wet%fields( 3 + ncnst ) )
    fieldlist_wet%fields(:) = .false.

    allocate( fieldlist_dry%fields( 3 + ncnst ) )
    fieldlist_dry%fields(:) = .false.

  end subroutine init_vdiff

  
  
  

  subroutine compute_vdiff( lchnk           ,                                                                   &
                            pcols           , pver               , ncnst         , ncol         , pmid        , &
                            pint            , rpdel              , t             , ztodt        , taux        , &
                            tauy            , shflx              , cflx          , ntop         , nbot        , &
                            kvh             , kvm                , kvq           , cgs          , cgh         , &
                            zi              , ksrftms            , qmincg        , fieldlist    ,               &
                            u               , v                  , q             , dse          ,               &
                            tautmsx         , tautmsy            , dtk           , topflx       , errstring   , &
                            tauresx         , tauresy            , itaures       ,                              &
                            do_molec_diff   , compute_molec_diff , vd_lu_qdecomp )

    
    
    
    
    
    
    





  
  
  
  

    
    
    

    integer,  intent(in)    :: lchnk                   
    integer,  intent(in)    :: pcols
    integer,  intent(in)    :: pver
    integer,  intent(in)    :: ncnst
    integer,  intent(in)    :: ncol                      
    integer,  intent(in)    :: ntop                      
    integer,  intent(in)    :: nbot                      
    integer,  intent(in)    :: itaures                   

    real(r8), intent(in)    :: pmid(pcols,pver)          
    real(r8), intent(in)    :: pint(pcols,pver+1)        
    real(r8), intent(in)    :: rpdel(pcols,pver)         
    real(r8), intent(in)    :: t(pcols,pver)             
    real(r8), intent(in)    :: ztodt                     
    real(r8), intent(in)    :: taux(pcols)               
    real(r8), intent(in)    :: tauy(pcols)               
    real(r8), intent(in)    :: shflx(pcols)              
    real(r8), intent(in)    :: cflx(pcols,ncnst)         
    real(r8), intent(in)    :: zi(pcols,pver+1)          
    real(r8), intent(in)    :: ksrftms(pcols)            
    real(r8), intent(in)    :: qmincg(ncnst)             

    logical,  intent(in)         :: do_molec_diff        
    integer,  external, optional :: compute_molec_diff   
    integer,  external, optional :: vd_lu_qdecomp        
    type(vdiff_selector), intent(in) :: fieldlist        

    
    
    

    real(r8), intent(inout) :: kvh(pcols,pver+1)         
    real(r8), intent(inout) :: kvm(pcols,pver+1)         
    real(r8), intent(inout) :: kvq(pcols,pver+1)         
    real(r8), intent(inout) :: cgs(pcols,pver+1)         
    real(r8), intent(inout) :: cgh(pcols,pver+1)         

    real(r8), intent(inout) :: u(pcols,pver)             
    real(r8), intent(inout) :: v(pcols,pver)             
    real(r8), intent(inout) :: q(pcols,pver,ncnst)       
    real(r8), intent(inout) :: dse(pcols,pver)           

    real(r8), intent(inout) :: tauresx(pcols)            
    real(r8), intent(inout) :: tauresy(pcols)            

    
    
    

    real(r8), intent(out)   :: dtk(pcols,pver)           
    real(r8), intent(out)   :: tautmsx(pcols)            
    real(r8), intent(out)   :: tautmsy(pcols)            
    real(r8), intent(out)   :: topflx(pcols)             
    character(128), intent(out) :: errstring             

    
    
    

    integer  :: i, k, m, icol                            
    integer  :: status                                   
    integer  :: ntop_molec                               
    logical  :: lqtst(pcols)                             
    logical  :: need_decomp                              
    logical  :: cnst_fixed_ubc(ncnst)                    
    logical  :: do_iss                                   

    real(r8) :: tmpm(pcols,pver)                         
    real(r8) :: ca(pcols,pver)                           
    real(r8) :: cc(pcols,pver)                           
    real(r8) :: dnom(pcols,pver)                         

    real(r8) :: tmp1(pcols)                              
    real(r8) :: tmpi1(pcols,pver+1)                      
    real(r8) :: tint(pcols,pver+1)                       
    real(r8) :: rhoi(pcols,pver+1)                       
    real(r8) :: tmpi2(pcols,pver+1)                      
    real(r8) :: rrho(pcols)                              

    real(r8) :: zero(pcols)                              
    real(r8) :: tautotx(pcols)                           
    real(r8) :: tautoty(pcols)                           

    real(r8) :: dinp_u(pcols,pver+1)                     
    real(r8) :: dinp_v(pcols,pver+1)                     
    real(r8) :: dout_u                                   
    real(r8) :: dout_v                                   
    real(r8) :: dse_top(pcols)                           
    real(r8) :: cc_top(pcols)                            
    real(r8) :: cd_top(pcols)                            
    real(r8) :: rghd(pcols,pver+1)                       

    real(r8) :: qtm(pcols,pver)                          
    real(r8) :: kq_scal(pcols,pver+1)                    
    real(r8) :: mw_fac(ncnst)                            
    real(r8) :: cnst_mw(ncnst)                           
    real(r8) :: ubc_mmr(pcols,ncnst)                     
    real(r8) :: ubc_t(pcols)                             

    real(r8) :: ws(pcols)                                
    real(r8) :: tau(pcols)                               
    real(r8) :: ksrfturb(pcols)                          
    real(r8) :: ksrf(pcols)                              
    real(r8) :: usum_in(pcols)                           
    real(r8) :: vsum_in(pcols)                           
    real(r8) :: usum_mid(pcols)                          
    real(r8) :: vsum_mid(pcols)                          
    real(r8) :: usum_out(pcols)                          
    real(r8) :: vsum_out(pcols)                          
    real(r8) :: tauimpx(pcols)                           
    real(r8) :: tauimpy(pcols)                           
    real(r8) :: wsmin                                    
    real(r8) :: ksrfmin                                  
    real(r8) :: timeres                                  
    real(r8) :: ramda                                    
    real(r8) :: psum
    real(r8) :: u_in, u_res, tauresx_in
    real(r8) :: v_in, v_res, tauresy_in  

    
    
    

    wsmin    = 1._r8                                     
    ksrfmin  = 1.e-4_r8                                  
    timeres  = 7200._r8                                  



    do_iss = .true. 


    
    
    

    errstring = ''
    if( ( diffuse(fieldlist,'u') .or. diffuse(fieldlist,'v') ) .and. .not. diffuse(fieldlist,'s') ) then
          errstring = 'diffusion_solver.compute_vdiff: must diffuse s if diffusing u or v'
          return
    end if
    zero(:) = 0._r8

    

    tint(:ncol,1) = t(:ncol,1)
    rhoi(:ncol,1) = pint(:ncol,1) / (rair*tint(:ncol,1))
    do k = 2, pver
       do i = 1, ncol
          tint(i,k)  = 0.5_r8 * ( t(i,k) + t(i,k-1) )
          rhoi(i,k)  = pint(i,k) / (rair*tint(i,k))
          tmpi2(i,k) = ztodt * ( gravit*rhoi(i,k) )**2 / ( pmid(i,k) - pmid(i,k-1) )
       end do
    end do
    tint(:ncol,pver+1) = t(:ncol,pver)
    rhoi(:ncol,pver+1) = pint(:ncol,pver+1) / ( rair*tint(:ncol,pver+1) )

    rrho(:ncol) = rair  * t(:ncol,pver) / pmid(:ncol,pver)
    tmp1(:ncol) = ztodt * gravit * rpdel(:ncol,pver)

    
    
    

  

    if( do_molec_diff ) then

        if( (.not.present(compute_molec_diff)) .or. (.not.present(vd_lu_qdecomp)) ) then
              errstring = 'compute_vdiff: do_molec_diff true but compute_molec_diff or vd_lu_qdecomp missing'
              return
        endif

      
      
      
      

        status = compute_molec_diff( lchnk          ,                                                                &
                                     pcols          , pver    , ncnst      , ncol      , t      , pmid   , pint    , &
                                     zi             , ztodt   , kvh        , kvm       , tint   , rhoi   , tmpi2   , &
                                     kq_scal        , ubc_t   , ubc_mmr    , dse_top   , cc_top , cd_top , cnst_mw , &
                                     cnst_fixed_ubc , mw_fac  , ntop_molec , nbot_molec )

    else

        kq_scal(:,:) = 0._r8
        cd_top(:)    = 0._r8
        cc_top(:)    = 0._r8

    endif

    
    
    

    if( diffuse(fieldlist,'u') .or. diffuse(fieldlist,'v') ) then

        
        
        
        

        do i = 1, ncol
           dinp_u(i,1) = 0._r8
           dinp_v(i,1) = 0._r8
           dinp_u(i,pver+1) = -u(i,pver)
           dinp_v(i,pver+1) = -v(i,pver)
        end do
        do k = 2, pver
           do i = 1, ncol
              dinp_u(i,k) = u(i,k) - u(i,k-1)
              dinp_v(i,k) = v(i,k) - v(i,k-1)
           end do
        end do

       
       
       
       

       if( do_iss ) then

         
         

           do i = 1, ncol
              ws(i)       = max( sqrt( u(i,pver)**2._r8 + v(i,pver)**2._r8 ), wsmin )
              tau(i)      = sqrt( taux(i)**2._r8 + tauy(i)**2._r8 )
              ksrfturb(i) = max( tau(i) / ws(i), ksrfmin )
           end do              
           ksrf(:ncol) = ksrfturb(:ncol) + ksrftms(:ncol)  

         
         
         
         

           do i = 1, ncol
              usum_in(i) = 0._r8
              vsum_in(i) = 0._r8
              do k = 1, pver
                 usum_in(i) = usum_in(i) + (1._r8/gravit)*u(i,k)/rpdel(i,k)
                 vsum_in(i) = vsum_in(i) + (1._r8/gravit)*v(i,k)/rpdel(i,k)
              end do
           end do              

         
         

           ramda         = ztodt / timeres
           u(:ncol,pver) = u(:ncol,pver) + tmp1(:ncol)*tauresx(:ncol)*ramda
           v(:ncol,pver) = v(:ncol,pver) + tmp1(:ncol)*tauresy(:ncol)*ramda

         
         

           do i = 1, ncol
              usum_mid(i) = 0._r8
              vsum_mid(i) = 0._r8
              do k = 1, pver
                 usum_mid(i) = usum_mid(i) + (1._r8/gravit)*u(i,k)/rpdel(i,k)
                 vsum_mid(i) = vsum_mid(i) + (1._r8/gravit)*v(i,k)/rpdel(i,k)
              end do
           end do              

         
         
         
         
         
         
         
         
         
         
         

       else

         
         
         
         

         

           ksrf(:ncol) = ksrftms(:ncol) 

         

           u(:ncol,pver) = u(:ncol,pver) + tmp1(:ncol)*taux(:ncol)
           v(:ncol,pver) = v(:ncol,pver) + tmp1(:ncol)*tauy(:ncol)

       end if  

       
       
       
       
       
       
       
       
       

       call vd_lu_decomp( pcols , pver , ncol  ,                        &
                          ksrf  , kvm  , tmpi2 , rpdel , ztodt , zero , &
                          ca    , cc   , dnom  , tmpm  , ntop  , nbot )

       call vd_lu_solve(  pcols , pver , ncol  ,                        &
                          u     , ca   , tmpm  , dnom  , ntop  , nbot , zero )

       call vd_lu_solve(  pcols , pver , ncol  ,                        &
                          v     , ca   , tmpm  , dnom  , ntop  , nbot , zero )

       
       
       
       
       

       do i = 1, ncol

          
          
          
          

          tautmsx(i) = -ksrftms(i)*u(i,pver)
          tautmsy(i) = -ksrftms(i)*v(i,pver)

          if( do_iss ) then

            

              usum_out(i) = 0._r8
              vsum_out(i) = 0._r8
              do k = 1, pver
                 usum_out(i) = usum_out(i) + (1._r8/gravit)*u(i,k)/rpdel(i,k)
                 vsum_out(i) = vsum_out(i) + (1._r8/gravit)*v(i,k)/rpdel(i,k)
              end do

            
            
            
            
            

              tauimpx(i) = ( usum_out(i) - usum_in(i) ) / ztodt
              tauimpy(i) = ( vsum_out(i) - vsum_in(i) ) / ztodt

              tautotx(i) = tauimpx(i) 
              tautoty(i) = tauimpy(i) 

            
            
            

              if( itaures .eq. 1 ) then
                  tauresx(i) = taux(i) + tautmsx(i) + tauresx(i) - tauimpx(i)
                  tauresy(i) = tauy(i) + tautmsy(i) + tauresy(i) - tauimpy(i)
              endif

          else

              tautotx(i) = tautmsx(i) + taux(i)
              tautoty(i) = tautmsy(i) + tauy(i)
              tauresx(i) = 0._r8
              tauresy(i) = 0._r8

          end if  

       end do 

     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     

       
       
       

     
     

       
       
       
       

     
     

       k = pver + 1
       do i = 1, ncol
          tmpi1(i,1) = 0._r8
          tmpi1(i,k) = 0.5_r8 * ztodt * gravit * &
                       ( (-u(i,k-1) + dinp_u(i,k))*tautotx(i) + (-v(i,k-1) + dinp_v(i,k))*tautoty(i) )
       end do

       do k = 2, pver
          do i = 1, ncol
             dout_u = u(i,k) - u(i,k-1)
             dout_v = v(i,k) - v(i,k-1)
             tmpi1(i,k) = 0.25_r8 * tmpi2(i,k) * kvm(i,k) * &
                          ( dout_u**2 + dout_v**2 + dout_u*dinp_u(i,k) + dout_v*dinp_v(i,k) )
          end do
       end do

       

       do k = 1, pver
          do i = 1, ncol
             dtk(i,k) = ( tmpi1(i,k+1) + tmpi1(i,k) ) * rpdel(i,k)
             dse(i,k) = dse(i,k) + dtk(i,k)
          end do
       end do

    end if 

    
    
    

  
  

    if( diffuse(fieldlist,'s') ) then

      

        do k = 1, pver
           dse(:ncol,k) = dse(:ncol,k) + ztodt * rpdel(:ncol,k) * gravit  *                &
                                       ( rhoi(:ncol,k+1) * kvh(:ncol,k+1) * cgh(:ncol,k+1) &
                                       - rhoi(:ncol,k  ) * kvh(:ncol,k  ) * cgh(:ncol,k  ) )
       end do

     

       dse(:ncol,pver) = dse(:ncol,pver) + tmp1(:ncol) * shflx(:ncol)

     

       call vd_lu_decomp( pcols , pver , ncol  ,                         &
                          zero  , kvh  , tmpi2 , rpdel , ztodt , cc_top, &
                          ca    , cc   , dnom  , tmpm  , ntop  , nbot    )

       call vd_lu_solve(  pcols , pver , ncol  ,                         &
                          dse   , ca   , tmpm  , dnom  , ntop  , nbot  , cd_top )

     
     
     

       if( do_molec_diff ) then
           topflx(:ncol) =  - kvh(:ncol,ntop_molec) * tmpi2(:ncol,ntop_molec) / (ztodt*gravit) * &
                            ( dse(:ncol,ntop_molec) - dse_top(:ncol) )
       end if

    endif

    
    
    

  
  

    

    need_decomp = .true.

    do m = 1, ncnst

       if( diffuse(fieldlist,'q',m) ) then

           
           
           
           
           

           qtm(:ncol,:pver) = q(:ncol,:pver,m)

           do k = 1, pver
              q(:ncol,k,m) = q(:ncol,k,m) + &
                             ztodt * rpdel(:ncol,k) * gravit  * ( cflx(:ncol,m) * rrho(:ncol) ) * &
                           ( rhoi(:ncol,k+1) * kvh(:ncol,k+1) * cgs(:ncol,k+1)                    &
                           - rhoi(:ncol,k  ) * kvh(:ncol,k  ) * cgs(:ncol,k  ) )
           end do
           lqtst(:ncol) = all(q(:ncol,1:pver,m) >= qmincg(m), 2)
           do k = 1, pver
              q(:ncol,k,m) = merge( q(:ncol,k,m), qtm(:ncol,k), lqtst(:ncol) )
           end do

           

           q(:ncol,pver,m) = q(:ncol,pver,m) + tmp1(:ncol) * cflx(:ncol,m)

           

           if( need_decomp ) then

               call vd_lu_decomp( pcols , pver , ncol  ,                         &
                                  zero  , kvq  , tmpi2 , rpdel , ztodt , zero  , &
                                  ca    , cc   , dnom  , tmpm  , ntop  , nbot )

               if( do_molec_diff ) then

                 

                   status = vd_lu_qdecomp( pcols , pver   , ncol      , cnst_fixed_ubc(m), cnst_mw(m), ubc_mmr(:,m), &
                                           kvq   , kq_scal, mw_fac(m) , tmpi2            , rpdel     ,               &
                                           ca    , cc     , dnom      , tmpm             , rhoi      ,               &
                                           tint  , ztodt  , ntop_molec, nbot_molec       , cd_top )
               else
                   need_decomp =  .false.
               endif
           end if

           call vd_lu_solve(  pcols , pver , ncol  ,                         &
                              q(1,1,m) , ca, tmpm  , dnom  , ntop  , nbot  , cd_top )
       end if
    end do

    return
  end subroutine compute_vdiff

  
  
  

  subroutine vd_lu_decomp( pcols, pver, ncol ,                        &
                           ksrf , kv  , tmpi , rpdel, ztodt , cc_top, &
                           ca   , cc  , dnom , ze   , ntop  , nbot    )
    
    
    
    
    
    

    
    
    

    integer,  intent(in)  :: pcols                 
    integer,  intent(in)  :: pver                  
    integer,  intent(in)  :: ncol                  
    integer,  intent(in)  :: ntop                  
    integer,  intent(in)  :: nbot                  
    real(r8), intent(in)  :: ksrf(pcols)           
    real(r8), intent(in)  :: kv(pcols,pver+1)      
    real(r8), intent(in)  :: tmpi(pcols,pver+1)    
    real(r8), intent(in)  :: rpdel(pcols,pver)     
    real(r8), intent(in)  :: ztodt                 
    real(r8), intent(in)  :: cc_top(pcols)         

    real(r8), intent(out) :: ca(pcols,pver)        
    real(r8), intent(out) :: cc(pcols,pver)        
    real(r8), intent(out) :: dnom(pcols,pver)      
    real(r8), intent(out) :: ze(pcols,pver)        

    
    
    

    integer :: i                                   
    integer :: k                                   

    
    
    

    
    
    

    do k = nbot - 1, ntop, -1
       do i = 1, ncol
          ca(i,k  ) = kv(i,k+1) * tmpi(i,k+1) * rpdel(i,k  )
          cc(i,k+1) = kv(i,k+1) * tmpi(i,k+1) * rpdel(i,k+1)
       end do
    end do

    
    

    do i = 1, ncol
       ca(i,nbot) = 0._r8
    end do

    
    

    do i = 1, ncol
       dnom(i,nbot) = 1._r8/(1._r8 + cc(i,nbot) + ksrf(i)*ztodt*gravit*rpdel(i,nbot))
       ze(i,nbot)   = cc(i,nbot)*dnom(i,nbot)
    end do

    do k = nbot - 1, ntop + 1, -1
       do i = 1, ncol
          dnom(i,k) = 1._r8/(1._r8 + ca(i,k) + cc(i,k) - ca(i,k)*ze(i,k+1))
          ze(i,k)   = cc(i,k)*dnom(i,k)
       end do
    end do

    do i = 1, ncol
       dnom(i,ntop) = 1._r8/(1._r8 + ca(i,ntop) + cc_top(i) - ca(i,ntop)*ze(i,ntop+1))
    end do

    return
  end subroutine vd_lu_decomp

  
  
  

  subroutine vd_lu_solve( pcols , pver , ncol , &
                          q     , ca   , ze   , dnom , ntop , nbot , cd_top )
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    

    
    
    

    integer,  intent(in)    :: pcols                  
    integer,  intent(in)    :: pver                   
    integer,  intent(in)    :: ncol                   
    integer,  intent(in)    :: ntop                   
    integer,  intent(in)    :: nbot                   
    real(r8), intent(in)    :: ca(pcols,pver)         
    real(r8), intent(in)    :: ze(pcols,pver)         
    real(r8), intent(in)    :: dnom(pcols,pver)       
    real(r8), intent(in)    :: cd_top(pcols)          

    real(r8), intent(inout) :: q(pcols,pver)          

    
    
    

    real(r8)                :: zf(pcols,pver)         
    integer                    i, k                   

    
    
    

    
    
    

    do i = 1, ncol
       zf(i,nbot) = q(i,nbot)*dnom(i,nbot)
    end do

    do k = nbot - 1, ntop + 1, -1
       do i = 1, ncol
          zf(i,k) = (q(i,k) + ca(i,k)*zf(i,k+1))*dnom(i,k)
       end do
    end do

    

    k = ntop
    do i = 1, ncol
       zf(i,k) = (q(i,k) + cd_top(i) + ca(i,k)*zf(i,k+1))*dnom(i,k)
    end do

    

    do i = 1, ncol
       q(i,ntop) = zf(i,ntop)
    end do

    do k = ntop + 1, nbot, +1
       do i = 1, ncol
          q(i,k) = zf(i,k) + ze(i,k)*q(i,k-1)
       end do
    end do

    return
  end subroutine vd_lu_solve

  
  
  
  
  character(128) function vdiff_select( fieldlist, name, qindex )
    
    
    
    type(vdiff_selector), intent(inout)        :: fieldlist
    character(*),         intent(in)           :: name
    integer,              intent(in), optional :: qindex
    
    vdiff_select = ''
    select case (name)
    case ('u','U')
       fieldlist%fields(1) = .true.
    case ('v','V')
       fieldlist%fields(2) = .true.
    case ('s','S')
       fieldlist%fields(3) = .true.
    case ('q','Q')
       if( present(qindex) ) then
           fieldlist%fields(3 + qindex) = .true.
       else
           fieldlist%fields(4) = .true.
       endif
    case default
       write(vdiff_select,*) 'Bad argument to vdiff_index: ', name
    end select
    return
    
  end function vdiff_select

  type(vdiff_selector) function not(a)
    
    
    
    type(vdiff_selector), intent(in)  :: a
    allocate(not%fields(size(a%fields)))
    not%fields(:) = .not. a%fields(:)
  end function not

  logical function my_any(a)
    
    
    
    
    type(vdiff_selector), intent(in) :: a
    my_any = any(a%fields)
  end function my_any

  logical function diffuse(fieldlist,name,qindex)
    
    
    
    type(vdiff_selector), intent(in)           :: fieldlist
    character(*),         intent(in)           :: name
    integer,              intent(in), optional :: qindex
    
    select case (name)
    case ('u','U')
       diffuse = fieldlist%fields(1)
    case ('v','V')
       diffuse = fieldlist%fields(2)
    case ('s','S')
       diffuse = fieldlist%fields(3)
    case ('q','Q')
       if( present(qindex) ) then
           diffuse = fieldlist%fields(3 + qindex)
       else
           diffuse = fieldlist%fields(4)
       endif
    case default
       diffuse = .false.
    end select
    return
  end function diffuse

end module diffusion_solver
