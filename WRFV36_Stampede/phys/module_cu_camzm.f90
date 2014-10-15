


module module_cu_camzm













  use shr_kind_mod,    only: r8 => shr_kind_r8




  use module_cam_support, only: masterproc, pcols, pver, pverp 

  use cldwat,          only: cldwat_fice
  use physconst,       only: cpair, epsilo, gravit, latice, latvap, tmelt, rair, &
                             cpwv, cpliq, rh2o




  use module_cam_support,only: endrun, iulog


  implicit none

  save
  private                         



  public zmconv_readnl            
  public zm_convi                 
  public zm_convr                 
  public zm_conv_evap             
  public convtran                 
  public momtran                  




   real(r8), parameter :: unset_r8 = huge(1.0_r8)
   real(r8) :: zmconv_c0_lnd = unset_r8    
   real(r8) :: zmconv_c0_ocn = unset_r8    
   real(r8) :: zmconv_ke     = unset_r8    

   real(r8) rl         
   real(r8) cpres      
   real(r8), parameter :: capelmt = 70._r8  
   real(r8) :: ke           
   real(r8) :: c0_lnd       
   real(r8) :: c0_ocn       
   real(r8) tau   
   real(r8),parameter :: a = 21.656_r8
   real(r8),parameter :: b = 5418._r8
   real(r8),parameter :: c1 = 6.112_r8
   real(r8),parameter :: c2 = 17.67_r8
   real(r8),parameter :: c3 = 243.5_r8
   real(r8) :: tfreez
   real(r8) :: eps1
      

   logical :: no_deep_pbl 
                          
   


   real(r8) :: rgrav       
   real(r8) :: rgas        
   real(r8) :: grav        
   real(r8) :: cp          
   
   integer  limcnv       

   real(r8),parameter ::  tiedke_add = 0.5_r8   

contains

subroutine zmconv_readnl(nlfile)






   character(len=*), intent(in) :: nlfile  




   c0_lnd = 0.0059D0 
   c0_ocn = 0.0450D0 
   ke     = 1.0E-6 
end subroutine zmconv_readnl


   subroutine zm_convi(DT,DX,limcnv_in, no_deep_pbl_in)


   integer, intent(in)           :: limcnv_in       
   logical, intent(in), optional :: no_deep_pbl_in  
   REAL ,    INTENT(IN)        :: DT, DX
   real(r8) :: DTT



   
   character(len=32)   :: hgrid           

   real(r8) :: deltax     
   real(r8) :: ref_dx     
   real(r8) :: taumin     
   real(r8) :: taumax     

   
   limcnv = limcnv_in
   tfreez = tmelt
   eps1   = epsilo
   rl     = latvap
   cpres  = cpair
   rgrav  = 1.0_r8/gravit
   rgas   = rair
   grav   = gravit
   cp     = cpres
   
   ref_dx = 275000._r8
   taumin = 600._r8
   taumax = 3600._r8
   deltax=dx

   if ( present(no_deep_pbl_in) )  then
      no_deep_pbl = no_deep_pbl_in
   else
      no_deep_pbl = .true.
   endif

   DTT=DT

   
   


















   tau = max(taumin, taumax*min(1._r8,deltax/ref_dx))

   if ( masterproc ) then
      write(iulog,*) 'delta X =',deltax
      call wrf_debug(1,iulog)
      write(iulog,*) 'Convective relaxation time scale (tau) is a tunable parameter in CAM and is a function of spatial resolution.'
      write(iulog,*) 'Users are encouraged to consult with the PNNL WRF-CAM5 development team if they want to change tau.'
      call wrf_debug(1,iulog)
      write(iulog,*) 'tuning parameters zm_convi: tau',tau
      call wrf_debug(1,iulog)
      write(iulog,*) 'tuning parameters zm_convi: c0_lnd',c0_lnd, ', c0_ocn', c0_ocn 
      call wrf_debug(1,iulog)
      write(iulog,*) 'tuning parameters zm_convi: ke',ke
      call wrf_debug(1,iulog)
      write(iulog,*) 'tuning parameters zm_convi: no_deep_pbl',no_deep_pbl
      call wrf_debug(1,iulog)
   endif

   if (masterproc) write(iulog,*)'**** ZM: DILUTE Buoyancy Calculation ****'
      call wrf_debug(1,iulog)

end subroutine zm_convi



subroutine zm_convr(lchnk   ,ncol    , &
                    t       ,qh      ,prec    ,jctop   ,jcbot   , &
                    pblh    ,zm      ,geos    ,zi      ,qtnd    , &
                    heat    ,pap     ,paph    ,dpp     , &
                    delt    ,mcon    ,cme     ,cape    , &
                    tpert   ,dlf     ,pflx    ,zdu     ,rprd    , &
                    mu      ,md      ,du      ,eu      ,ed      , &
                    dp      ,dsubcld ,jt      ,maxg    ,ideep   , &
                    lengath ,ql      ,rliq    ,landfrac)


















  use module_cam_support, only: pcnst =>pcnst_runtime


































































































   integer, intent(in) :: lchnk                   
   integer, intent(in) :: ncol                    

   real(r8), intent(in) :: t(pcols,pver)          
   real(r8), intent(in) :: qh(pcols,pver,pcnst)   
   real(r8), intent(in) :: pap(pcols,pver)     
   real(r8), intent(in) :: paph(pcols,pver+1)
   real(r8), intent(in) :: dpp(pcols,pver)        
   real(r8), intent(in) :: zm(pcols,pver)
   real(r8), intent(in) :: geos(pcols)
   real(r8), intent(in) :: zi(pcols,pver+1)
   real(r8), intent(in) :: pblh(pcols)
   real(r8), intent(in) :: tpert(pcols)
   real(r8), intent(in) :: landfrac(pcols) 



   real(r8), intent(out) :: qtnd(pcols,pver)           
   real(r8), intent(out) :: heat(pcols,pver)           
   real(r8), intent(out) :: mcon(pcols,pverp)
   real(r8), intent(out) :: dlf(pcols,pver)    
   real(r8), intent(out) :: pflx(pcols,pverp)  
   real(r8), intent(out) :: cme(pcols,pver)
   real(r8), intent(out) :: cape(pcols)        
   real(r8), intent(out) :: zdu(pcols,pver)
   real(r8), intent(out) :: rprd(pcols,pver)     


   real(r8), intent(out) :: mu(pcols,pver)
   real(r8), intent(out) :: eu(pcols,pver)
   real(r8), intent(out) :: du(pcols,pver)
   real(r8), intent(out) :: md(pcols,pver)
   real(r8), intent(out) :: ed(pcols,pver)
   real(r8), intent(out) :: dp(pcols,pver)       
   real(r8), intent(out) :: dsubcld(pcols)       
   real(r8), intent(out) :: jctop(pcols)  
   real(r8), intent(out) :: jcbot(pcols)  
   real(r8), intent(out) :: prec(pcols)
   real(r8), intent(out) :: rliq(pcols) 

   real(r8) zs(pcols)
   real(r8) dlg(pcols,pver)    
   real(r8) pflxg(pcols,pverp) 
   real(r8) cug(pcols,pver)    
   real(r8) evpg(pcols,pver)   
   real(r8) mumax(pcols)
   integer jt(pcols)                          
   integer maxg(pcols)                        
   integer ideep(pcols)                       
   integer lengath

   real(r8) ql(pcols,pver)                    

   real(r8) pblt(pcols)           









   real(r8) q(pcols,pver)              
   real(r8) p(pcols,pver)              
   real(r8) z(pcols,pver)              
   real(r8) s(pcols,pver)              
   real(r8) tp(pcols,pver)             
   real(r8) zf(pcols,pver+1)           
   real(r8) pf(pcols,pver+1)           
   real(r8) qstp(pcols,pver)           

   real(r8) tl(pcols)                  

   integer lcl(pcols)                  
   integer lel(pcols)                  
   integer lon(pcols)                  
   integer maxi(pcols)                 
   integer index(pcols)
   real(r8) precip



   real(r8) qg(pcols,pver)             
   real(r8) tg(pcols,pver)             
   real(r8) pg(pcols,pver)             
   real(r8) zg(pcols,pver)             
   real(r8) sg(pcols,pver)             
   real(r8) tpg(pcols,pver)            
   real(r8) zfg(pcols,pver+1)          
   real(r8) qstpg(pcols,pver)          
   real(r8) ug(pcols,pver)             
   real(r8) vg(pcols,pver)             
   real(r8) cmeg(pcols,pver)

   real(r8) rprdg(pcols,pver)           
   real(r8) capeg(pcols)               
   real(r8) tlg(pcols)                 
   real(r8) landfracg(pcols)            

   integer lclg(pcols)       
   integer lelg(pcols)



   real(r8) dqdt(pcols,pver)           
   real(r8) dsdt(pcols,pver)           

   real(r8) sd(pcols,pver)             
   real(r8) qd(pcols,pver)             
   real(r8) mc(pcols,pver)             
   real(r8) qhat(pcols,pver)           
   real(r8) qu(pcols,pver)             
   real(r8) su(pcols,pver)             
   real(r8) qs(pcols,pver)             
   real(r8) shat(pcols,pver)           
   real(r8) hmn(pcols,pver)            
   real(r8) hsat(pcols,pver)           
   real(r8) qlg(pcols,pver)
   real(r8) dudt(pcols,pver)           
   real(r8) dvdt(pcols,pver)           



   real(r8) mb(pcols)                  

   integer jlcl(pcols)
   integer j0(pcols)                 
   integer jd(pcols)                 

   real(r8) delt                     

   integer i
   integer ii
   integer k
   integer msg                      
   real(r8) qdifr
   real(r8) sdifr






   msg = limcnv - 1




   qtnd(:,:) = 0._r8
   heat(:,:) = 0._r8
   mcon(:,:) = 0._r8
   rliq(:ncol)   = 0._r8



   prec(:ncol) = 0._r8
   do k = 1,pver
      do i = 1,ncol
         dqdt(i,k)  = 0._r8
         dsdt(i,k)  = 0._r8
         dudt(i,k)  = 0._r8
         dvdt(i,k)  = 0._r8
         pflx(i,k)  = 0._r8
         pflxg(i,k) = 0._r8
         cme(i,k)   = 0._r8
         rprd(i,k)  = 0._r8
         zdu(i,k)   = 0._r8
         ql(i,k)    = 0._r8
         qlg(i,k)   = 0._r8
         dlf(i,k)   = 0._r8
         dlg(i,k)   = 0._r8
      end do
   end do
   do i = 1,ncol
      pflx(i,pverp) = 0
      pflxg(i,pverp) = 0
   end do

   do i = 1,ncol
      pblt(i) = pver
      dsubcld(i) = 0._r8

      jctop(i) = pver
      jcbot(i) = 1

   end do




   do i = 1,ncol
      zs(i) = geos(i)*rgrav
      pf(i,pver+1) = paph(i,pver+1)*0.01_r8
      zf(i,pver+1) = zi(i,pver+1) + zs(i)
   end do
   do k = 1,pver
      do i = 1,ncol
         p(i,k) = pap(i,k)*0.01_r8
         pf(i,k) = paph(i,k)*0.01_r8
         z(i,k) = zm(i,k) + zs(i)
         zf(i,k) = zi(i,k) + zs(i)
      end do
   end do

   do k = pver - 1,msg + 1,-1
      do i = 1,ncol
         if (abs(z(i,k)-zs(i)-pblh(i)) < (zf(i,k)-zf(i,k+1))*0.5_r8) pblt(i) = k
      end do
   end do





   do k = 1,pver
      do i = 1,ncol
         q(i,k) = qh(i,k,1)
         s(i,k) = t(i,k) + (grav/cpres)*z(i,k)
         tp(i,k)=0.0_r8
         shat(i,k) = s(i,k)
         qhat(i,k) = q(i,k)
      end do
   end do

   do i = 1,ncol
      capeg(i) = 0._r8
      lclg(i) = 1
      lelg(i) = pver
      maxg(i) = 1
      tlg(i) = 400._r8
      dsubcld(i) = 0._r8
   end do

      
      

      call buoyan_dilute(lchnk   ,ncol    , &
                  q       ,t       ,p       ,z       ,pf       , &
                  tp      ,qstp    ,tl      ,rl      ,cape     , &
                  pblt    ,lcl     ,lel     ,lon     ,maxi     , &
                  rgas    ,grav    ,cpres   ,msg     , &
                  tpert   )






   lengath = 0
   do i=1,ncol
      if (cape(i) > capelmt) then
         lengath = lengath + 1
         index(lengath) = i
      end if
   end do

   if (lengath.eq.0) return
   do ii=1,lengath
      i=index(ii)
      ideep(ii)=i
   end do



   do k = 1,pver
      do i = 1,lengath
         dp(i,k) = 0.01_r8*dpp(ideep(i),k)
         qg(i,k) = q(ideep(i),k)
         tg(i,k) = t(ideep(i),k)
         pg(i,k) = p(ideep(i),k)
         zg(i,k) = z(ideep(i),k)
         sg(i,k) = s(ideep(i),k)
         tpg(i,k) = tp(ideep(i),k)
         zfg(i,k) = zf(ideep(i),k)
         qstpg(i,k) = qstp(ideep(i),k)
         ug(i,k) = 0._r8
         vg(i,k) = 0._r8
      end do
   end do

   do i = 1,lengath
      zfg(i,pver+1) = zf(ideep(i),pver+1)
   end do
   do i = 1,lengath
      capeg(i) = cape(ideep(i))
      lclg(i) = lcl(ideep(i))
      lelg(i) = lel(ideep(i))
      maxg(i) = maxi(ideep(i))
      tlg(i) = tl(ideep(i))
      landfracg(i) = landfrac(ideep(i))
   end do




   do k = msg + 1,pver
      do i = 1,lengath
         if (k >= maxg(i)) then
            dsubcld(i) = dsubcld(i) + dp(i,k)
         end if
      end do
   end do





   do k = msg + 2,pver
      do i = 1,lengath

         sdifr = 0._r8
         qdifr = 0._r8
         if (sg(i,k) > 0._r8 .or. sg(i,k-1) > 0._r8) &
            sdifr = abs((sg(i,k)-sg(i,k-1))/max(sg(i,k-1),sg(i,k)))
         if (qg(i,k) > 0._r8 .or. qg(i,k-1) > 0._r8) &
            qdifr = abs((qg(i,k)-qg(i,k-1))/max(qg(i,k-1),qg(i,k)))
         if (sdifr > 1.E-6_r8) then
            shat(i,k) = log(sg(i,k-1)/sg(i,k))*sg(i,k-1)*sg(i,k)/(sg(i,k-1)-sg(i,k))
         else
            shat(i,k) = 0.5_r8* (sg(i,k)+sg(i,k-1))
         end if
         if (qdifr > 1.E-6_r8) then
            qhat(i,k) = log(qg(i,k-1)/qg(i,k))*qg(i,k-1)*qg(i,k)/(qg(i,k-1)-qg(i,k))
         else
            qhat(i,k) = 0.5_r8* (qg(i,k)+qg(i,k-1))
         end if
      end do
   end do




   call cldprp(lchnk   , &
               qg      ,tg      ,ug      ,vg      ,pg      , &
               zg      ,sg      ,mu      ,eu      ,du      , &
               md      ,ed      ,sd      ,qd      ,mc      , &
               qu      ,su      ,zfg     ,qs      ,hmn     , &
               hsat    ,shat    ,qlg     , &
               cmeg    ,maxg    ,lelg    ,jt      ,jlcl    , &
               maxg    ,j0      ,jd      ,rl      ,lengath , &
               rgas    ,grav    ,cpres   ,msg     , &
               pflxg   ,evpg    ,cug     ,rprdg   ,limcnv  ,landfracg)



   do k = msg + 1,pver
      do i = 1,lengath
         du   (i,k) = du   (i,k)* (zfg(i,k)-zfg(i,k+1))/dp(i,k)
         eu   (i,k) = eu   (i,k)* (zfg(i,k)-zfg(i,k+1))/dp(i,k)
         ed   (i,k) = ed   (i,k)* (zfg(i,k)-zfg(i,k+1))/dp(i,k)
         cug  (i,k) = cug  (i,k)* (zfg(i,k)-zfg(i,k+1))/dp(i,k)
         cmeg (i,k) = cmeg (i,k)* (zfg(i,k)-zfg(i,k+1))/dp(i,k)
         rprdg(i,k) = rprdg(i,k)* (zfg(i,k)-zfg(i,k+1))/dp(i,k)
         evpg (i,k) = evpg (i,k)* (zfg(i,k)-zfg(i,k+1))/dp(i,k)
      end do
   end do

   call closure(lchnk   , &
                qg      ,tg      ,pg      ,zg      ,sg      , &
                tpg     ,qs      ,qu      ,su      ,mc      , &
                du      ,mu      ,md      ,qd      ,sd      , &
                qhat    ,shat    ,dp      ,qstpg   ,zfg     , &
                qlg     ,dsubcld ,mb      ,capeg   ,tlg     , &
                lclg    ,lelg    ,jt      ,maxg    ,1       , &
                lengath ,rgas    ,grav    ,cpres   ,rl      , &
                msg     ,capelmt    )



   do i=1,lengath
      mumax(i) = 0
   end do
   do k=msg + 2,pver
      do i=1,lengath
        mumax(i) = max(mumax(i), mu(i,k)/dp(i,k))
      end do
   end do

   do i=1,lengath
      if (mumax(i) > 0._r8) then
         mb(i) = min(mb(i),0.5_r8/(delt*mumax(i)))
      else
         mb(i) = 0._r8
      endif
   end do
   
   

   if (no_deep_pbl) then
      do i=1,lengath
         if (zm(ideep(i),jt(i)) < pblh(ideep(i))) mb(i) = 0
      end do
   end if


   do k=msg+1,pver
      do i=1,lengath
         mu   (i,k)  = mu   (i,k)*mb(i)
         md   (i,k)  = md   (i,k)*mb(i)
         mc   (i,k)  = mc   (i,k)*mb(i)
         du   (i,k)  = du   (i,k)*mb(i)
         eu   (i,k)  = eu   (i,k)*mb(i)
         ed   (i,k)  = ed   (i,k)*mb(i)
         cmeg (i,k)  = cmeg (i,k)*mb(i)
         rprdg(i,k)  = rprdg(i,k)*mb(i)
         cug  (i,k)  = cug  (i,k)*mb(i)
         evpg (i,k)  = evpg (i,k)*mb(i)
         pflxg(i,k+1)= pflxg(i,k+1)*mb(i)*100._r8/grav
      end do
   end do



   call q1q2_pjr(lchnk   , &
                 dqdt    ,dsdt    ,qg      ,qs      ,qu      , &
                 su      ,du      ,qhat    ,shat    ,dp      , &
                 mu      ,md      ,sd      ,qd      ,qlg     , &
                 dsubcld ,jt      ,maxg    ,1       ,lengath , &
                 cpres   ,rl      ,msg     ,          &
                 dlg     ,evpg    ,cug     )



   do k = msg + 1,pver
!DIR$ CONCURRENT
      do i = 1,lengath



         q(ideep(i),k) = qh(ideep(i),k,1) + 2._r8*delt*dqdt(i,k)
         qtnd(ideep(i),k) = dqdt (i,k)
         cme (ideep(i),k) = cmeg (i,k)
         rprd(ideep(i),k) = rprdg(i,k)
         zdu (ideep(i),k) = du   (i,k)
         mcon(ideep(i),k) = mc   (i,k)
         heat(ideep(i),k) = dsdt (i,k)*cpres
         dlf (ideep(i),k) = dlg  (i,k)
         pflx(ideep(i),k) = pflxg(i,k)
         ql  (ideep(i),k) = qlg  (i,k)
      end do
   end do

!DIR$ CONCURRENT
   do i = 1,lengath
      jctop(ideep(i)) = jt(i)

      jcbot(ideep(i)) = maxg(i)

      pflx(ideep(i),pverp) = pflxg(i,pverp)
   end do


   do k = pver,msg + 1,-1
      do i = 1,ncol
         prec(i) = prec(i) - dpp(i,k)* (q(i,k)-qh(i,k,1)) - dpp(i,k)*dlf(i,k)*2._r8*delt
      end do
   end do


   do i = 1,ncol
      prec(i) = rgrav*max(prec(i),0._r8)/ (2._r8*delt)/1000._r8
   end do



   do k = 1, pver
      do i = 1, ncol
         rliq(i) = rliq(i) + dlf(i,k)*dpp(i,k)/gravit
      end do
   end do
   rliq(:ncol) = rliq(:ncol) /1000._r8

   return
end subroutine zm_convr


subroutine zm_conv_evap(ncol,lchnk, &
     t,pmid,pdel,q, &
     tend_s, tend_s_snwprd, tend_s_snwevmlt, tend_q, &
     prdprec, cldfrc, deltat,  &
     prec, snow, ntprprd, ntsnprd, flxprec, flxsnow )










    use wv_saturation,  only: aqsat


    integer,intent(in) :: ncol, lchnk             
    real(r8),intent(in), dimension(pcols,pver) :: t          
    real(r8),intent(in), dimension(pcols,pver) :: pmid       
    real(r8),intent(in), dimension(pcols,pver) :: pdel       
    real(r8),intent(in), dimension(pcols,pver) :: q          
    real(r8),intent(inout), dimension(pcols,pver) :: tend_s     
    real(r8),intent(inout), dimension(pcols,pver) :: tend_q     
    real(r8),intent(out  ), dimension(pcols,pver) :: tend_s_snwprd 
    real(r8),intent(out  ), dimension(pcols,pver) :: tend_s_snwevmlt 
    


    real(r8), intent(in   ) :: prdprec(pcols,pver)
    real(r8), intent(in   ) :: cldfrc(pcols,pver) 
    real(r8), intent(in   ) :: deltat             

    real(r8), intent(inout) :: prec(pcols)        
    real(r8), intent(out)   :: snow(pcols)        



    real(r8) :: est    (pcols,pver)    
    real(r8) :: fice   (pcols,pver)    
    real(r8) :: fsnow_conv(pcols,pver) 
    real(r8) :: qsat   (pcols,pver)    
    real(r8),intent(out) :: flxprec(pcols,pverp)   
    real(r8),intent(out) :: flxsnow(pcols,pverp)   
    real(r8),intent(out) :: ntprprd(pcols,pver)    
    real(r8),intent(out) :: ntsnprd(pcols,pver)    
    real(r8) :: work1                  
    real(r8) :: work2                  

    real(r8) :: evpvint(pcols)         
    real(r8) :: evpprec(pcols)         
    real(r8) :: evpsnow(pcols)         
    real(r8) :: snowmlt(pcols)         
    real(r8) :: flxsntm(pcols)         

    real(r8) :: evplimit               
    real(r8) :: rlat(pcols)

    integer :: i,k                     





    prec(:ncol) = prec(:ncol)*1000._r8


    call aqsat (t    ,pmid  ,est    ,qsat    ,pcols   , &
         ncol ,pver  ,1       ,pver    )


    call cldwat_fice(ncol, t, fice, fsnow_conv)


    flxprec(:ncol,1) = 0._r8
    flxsnow(:ncol,1) = 0._r8
    evpvint(:ncol)   = 0._r8

    do k = 1, pver
       do i = 1, ncol


          if (t(i,k) > tmelt) then
             flxsntm(i) = 0._r8
             snowmlt(i) = flxsnow(i,k) * gravit/ pdel(i,k)
          else
             flxsntm(i) = flxsnow(i,k)
             snowmlt(i) = 0._r8
          end if


          evplimit = max(1._r8 - q(i,k)/qsat(i,k), 0._r8)



          evpprec(i) = ke * (1._r8 - cldfrc(i,k)) * evplimit * sqrt(flxprec(i,k))






          evplimit   = max(0._r8, (qsat(i,k)-q(i,k)) / deltat)





          evplimit   = min(evplimit, flxprec(i,k) * gravit / pdel(i,k))


          evplimit   = min(evplimit, (prec(i) - evpvint(i)) * gravit / pdel(i,k))

          evpprec(i) = min(evplimit, evpprec(i))


          if (flxprec(i,k) > 0._r8) then


             work1 = min(max(0._r8,flxsntm(i)/flxprec(i,k)),1._r8)
             evpsnow(i) = evpprec(i) * work1
          else
             evpsnow(i) = 0._r8
          end if


          evpvint(i) = evpvint(i) + evpprec(i) * pdel(i,k)/gravit


          ntprprd(i,k) = prdprec(i,k) - evpprec(i)






          if (flxprec(i,k).gt.0._r8) then
             work1 = min(max(0._r8,flxsnow(i,k)/flxprec(i,k)),1._r8)
          else
             work1 = 0._r8
          endif
          work2 = max(fsnow_conv(i,k), work1)
          if (snowmlt(i).gt.0._r8) work2 = 0._r8

          ntsnprd(i,k) = prdprec(i,k)*work2 - evpsnow(i) - snowmlt(i)
          tend_s_snwprd  (i,k) = prdprec(i,k)*work2*latice
          tend_s_snwevmlt(i,k) = - ( evpsnow(i) + snowmlt(i) )*latice


          flxprec(i,k+1) = flxprec(i,k) + ntprprd(i,k) * pdel(i,k)/gravit
          flxsnow(i,k+1) = flxsnow(i,k) + ntsnprd(i,k) * pdel(i,k)/gravit


          flxprec(i,k+1) = max(flxprec(i,k+1), 0._r8)
          flxsnow(i,k+1) = max(flxsnow(i,k+1), 0._r8)






          tend_s(i,k)   =-evpprec(i)*latvap + ntsnprd(i,k)*latice
          tend_q(i,k) = evpprec(i)
       end do
    end do


    prec(:ncol) = flxprec(:ncol,pver+1) / 1000._r8
    snow(:ncol) = flxsnow(:ncol,pver+1) / 1000._r8





  end subroutine zm_conv_evap



subroutine convtran(lchnk   , &
                    doconvtran,q       ,ncnst   ,mu      ,md      , &
                    du      ,eu      ,ed      ,dp      ,dsubcld , &
                    jt      ,mx      ,ideep   ,il1g    ,il2g    , &
                    nstep   ,fracis  ,dqdt    ,dpdry   )














   use shr_kind_mod, only: r8 => shr_kind_r8
   use constituents,    only: cnst_get_type_byind

   implicit none




   integer, intent(in) :: lchnk                 
   integer, intent(in) :: ncnst                 
   logical, intent(in) :: doconvtran(ncnst)     
   real(r8), intent(in) :: q(pcols,pver,ncnst)  
   real(r8), intent(in) :: mu(pcols,pver)       
   real(r8), intent(in) :: md(pcols,pver)       
   real(r8), intent(in) :: du(pcols,pver)       
   real(r8), intent(in) :: eu(pcols,pver)       
   real(r8), intent(in) :: ed(pcols,pver)       
   real(r8), intent(in) :: dp(pcols,pver)       
   real(r8), intent(in) :: dsubcld(pcols)       
   real(r8), intent(in) :: fracis(pcols,pver,ncnst) 

   integer, intent(in) :: jt(pcols)         
   integer, intent(in) :: mx(pcols)         
   integer, intent(in) :: ideep(pcols)      
   integer, intent(in) :: il1g              
   integer, intent(in) :: il2g              
   integer, intent(in) :: nstep             

   real(r8), intent(in) :: dpdry(pcols,pver)       




   real(r8), intent(out) :: dqdt(pcols,pver,ncnst)  



   integer i                 
   integer k                 
   integer kbm               
   integer kk                
   integer kkp1              
   integer km1               
   integer kp1               
   integer ktm               
   integer m                 

   real(r8) cabv                 
   real(r8) cbel                 
   real(r8) cdifr                
   real(r8) chat(pcols,pver)     
   real(r8) cond(pcols,pver)     
   real(r8) const(pcols,pver)    
   real(r8) fisg(pcols,pver)     
   real(r8) conu(pcols,pver)     
   real(r8) dcondt(pcols,pver)   
   real(r8) small                
   real(r8) mbsth                
   real(r8) mupdudp              
   real(r8) minc                 
   real(r8) maxc                 
   real(r8) fluxin               
   real(r8) fluxout              
   real(r8) netflux              

   real(r8) dutmp(pcols,pver)       
   real(r8) eutmp(pcols,pver)       
   real(r8) edtmp(pcols,pver)       
   real(r8) dptmp(pcols,pver)    


   small = 1.e-36_r8

   mbsth = 1.e-15_r8


   ktm = pver
   kbm = pver
   do i = il1g, il2g
      ktm = min(ktm,jt(i))
      kbm = min(kbm,mx(i))
   end do


   do m = 2, ncnst
      if (doconvtran(m)) then

         if (cnst_get_type_byind(m).eq.'dry') then
            do k = 1,pver
               do i =il1g,il2g
                  dptmp(i,k) = dpdry(i,k)
                  dutmp(i,k) = du(i,k)*dp(i,k)/dpdry(i,k)
                  eutmp(i,k) = eu(i,k)*dp(i,k)/dpdry(i,k)
                  edtmp(i,k) = ed(i,k)*dp(i,k)/dpdry(i,k)
               end do
            end do
         else
            do k = 1,pver
               do i =il1g,il2g
                  dptmp(i,k) = dp(i,k)
                  dutmp(i,k) = du(i,k)
                  eutmp(i,k) = eu(i,k)
                  edtmp(i,k) = ed(i,k)
               end do
            end do
         endif



         do k = 1,pver
            do i =il1g,il2g
               const(i,k) = q(ideep(i),k,m)
               fisg(i,k) = fracis(ideep(i),k,m)
            end do
         end do




         do k = 1,pver
            km1 = max(1,k-1)
            do i = il1g, il2g
               minc = min(const(i,km1),const(i,k))
               maxc = max(const(i,km1),const(i,k))
               if (minc < 0) then
                  cdifr = 0._r8
               else
                  cdifr = abs(const(i,k)-const(i,km1))/max(maxc,small)
               endif



               if (cdifr > 1.E-6_r8) then
                  cabv = max(const(i,km1),maxc*1.e-12_r8)
                  cbel = max(const(i,k),maxc*1.e-12_r8)
                  chat(i,k) = log(cabv/cbel)/(cabv-cbel)*cabv*cbel

               else             
                  chat(i,k) = 0.5_r8* (const(i,k)+const(i,km1))
               end if


               conu(i,k) = chat(i,k)
               cond(i,k) = chat(i,k)


               dcondt(i,k) = 0._r8

            end do
         end do


         k = 2
         km1 = 1
         kk = pver
         do i = il1g,il2g
            mupdudp = mu(i,kk) + dutmp(i,kk)*dptmp(i,kk)
            if (mupdudp > mbsth) then
               conu(i,kk) = (+eutmp(i,kk)*fisg(i,kk)*const(i,kk)*dptmp(i,kk))/mupdudp
            endif
            if (md(i,k) < -mbsth) then
               cond(i,k) =  (-edtmp(i,km1)*fisg(i,km1)*const(i,km1)*dptmp(i,km1))/md(i,k)
            endif
         end do


         do kk = pver-1,1,-1
            kkp1 = min(pver,kk+1)
            do i = il1g,il2g
               mupdudp = mu(i,kk) + dutmp(i,kk)*dptmp(i,kk)
               if (mupdudp > mbsth) then
                  conu(i,kk) = (  mu(i,kkp1)*conu(i,kkp1)+eutmp(i,kk)*fisg(i,kk)* &
                                  const(i,kk)*dptmp(i,kk) )/mupdudp
               endif
            end do
         end do


         do k = 3,pver
            km1 = max(1,k-1)
            do i = il1g,il2g
               if (md(i,k) < -mbsth) then
                  cond(i,k) =  (  md(i,km1)*cond(i,km1)-edtmp(i,km1)*fisg(i,km1)*const(i,km1) &
                                  *dptmp(i,km1) )/md(i,k)
               endif
            end do
         end do


         do k = ktm,pver
            km1 = max(1,k-1)
            kp1 = min(pver,k+1)
            do i = il1g,il2g


















               fluxin =  mu(i,kp1)*conu(i,kp1)+ mu(i,k)*min(chat(i,k),const(i,km1)) &
                         -(md(i,k)  *cond(i,k) + md(i,kp1)*min(chat(i,kp1),const(i,kp1)))
               fluxout = mu(i,k)*conu(i,k) + mu(i,kp1)*min(chat(i,kp1),const(i,k)) &
                         -(md(i,kp1)*cond(i,kp1) + md(i,k)*min(chat(i,k),const(i,k)))

               netflux = fluxin - fluxout
               if (abs(netflux) < max(fluxin,fluxout)*1.e-12_r8) then
                  netflux = 0._r8
               endif
               dcondt(i,k) = netflux/dptmp(i,k)
            end do
         end do


!DIR$ NOINTERCHANGE
         do k = kbm,pver
            km1 = max(1,k-1)
            do i = il1g,il2g
               if (k == mx(i)) then











                  fluxin =  mu(i,k)*min(chat(i,k),const(i,km1)) - md(i,k)*cond(i,k)
                  fluxout = mu(i,k)*conu(i,k) - md(i,k)*min(chat(i,k),const(i,k))

                  netflux = fluxin - fluxout
                  if (abs(netflux) < max(fluxin,fluxout)*1.e-12_r8) then
                     netflux = 0._r8
                  endif

                  dcondt(i,k) = netflux/dptmp(i,k)
               else if (k > mx(i)) then

                  dcondt(i,k) = 0._r8
               end if
            end do
         end do


         dqdt(:,:,m) = 0._r8
         do k = 1,pver
            kp1 = min(pver,k+1)
!DIR$ CONCURRENT
            do i = il1g,il2g
               dqdt(ideep(i),k,m) = dcondt(i,k)
            end do
         end do

      end if      

   end do

   return
end subroutine convtran



subroutine momtran(lchnk, ncol, &
                    domomtran,q       ,ncnst   ,mu      ,md    , &
                    du      ,eu      ,ed      ,dp      ,dsubcld , &
                    jt      ,mx      ,ideep   ,il1g    ,il2g    , &
                    nstep   ,dqdt    ,pguall     ,pgdall, icwu, icwd, dt, seten    )














   use shr_kind_mod, only: r8 => shr_kind_r8

   implicit none




   integer, intent(in) :: lchnk                 
   integer, intent(in) :: ncol                  
   integer, intent(in) :: ncnst                 
   logical, intent(in) :: domomtran(ncnst)      
   real(r8), intent(in) :: q(pcols,pver,ncnst)  
   real(r8), intent(in) :: mu(pcols,pver)       
   real(r8), intent(in) :: md(pcols,pver)       
   real(r8), intent(in) :: du(pcols,pver)       
   real(r8), intent(in) :: eu(pcols,pver)       
   real(r8), intent(in) :: ed(pcols,pver)       
   real(r8), intent(in) :: dp(pcols,pver)       
   real(r8), intent(in) :: dsubcld(pcols)       
   real(r8), intent(in)    :: dt                       

   integer, intent(in) :: jt(pcols)         
   integer, intent(in) :: mx(pcols)         
   integer, intent(in) :: ideep(pcols)      
   integer, intent(in) :: il1g              
   integer, intent(in) :: il2g              
   integer, intent(in) :: nstep             





   real(r8), intent(out) :: dqdt(pcols,pver,ncnst)  



   integer i                 
   integer k                 
   integer kbm               
   integer kk                
   integer kkp1              
   integer kkm1              
   integer km1               
   integer kp1               
   integer ktm               
   integer m                 
   integer ii                 

   real(r8) cabv                 
   real(r8) cbel                 
   real(r8) cdifr                
   real(r8) chat(pcols,pver)     
   real(r8) cond(pcols,pver)     
   real(r8) const(pcols,pver)    
   real(r8) conu(pcols,pver)     
   real(r8) dcondt(pcols,pver)   
   real(r8) small                
   real(r8) mbsth                
   real(r8) mupdudp              
   real(r8) minc                 
   real(r8) maxc                 
   real(r8) fluxin               
   real(r8) fluxout              
   real(r8) netflux              

   real(r8) momcu                
   real(r8) momcd                
   real(r8) sum                  
   real(r8) sum2                  
 
   real(r8) mududp(pcols,pver) 
   real(r8) mddudp(pcols,pver)     

   real(r8) pgu(pcols,pver)      
   real(r8) pgd(pcols,pver)      

   real(r8),intent(out) ::  pguall(pcols,pver,ncnst)      
   real(r8),intent(out) ::  pgdall(pcols,pver,ncnst)      

   real(r8),intent(out) ::  icwu(pcols,pver,ncnst)      
   real(r8),intent(out) ::  icwd(pcols,pver,ncnst)      

   real(r8),intent(out) ::  seten(pcols,pver) 
   real(r8)                 gseten(pcols,pver) 

   real(r8)  mflux(pcols,pverp,ncnst)   

   real(r8)  wind0(pcols,pver,ncnst)       
   real(r8)  windf(pcols,pver,ncnst)       
   real(r8) fkeb, fket, ketend_cons, ketend, utop, ubot, vtop, vbot, gset2
   





   pguall(:,:,:)     = 0.0_r8
   pgdall(:,:,:)     = 0.0_r8

   icwu(:ncol,:,:)       = q(:ncol,:,:)
   icwd(:ncol,:,:)       = q(:ncol,:,:)


   mflux(:,:,:)       = 0.0_r8
   wind0(:,:,:)         = 0.0_r8
   windf(:,:,:)         = 0.0_r8



   seten(:,:)         = 0.0_r8
   gseten(:,:)         = 0.0_r8



   momcu = 0.4_r8
   momcd = 0.4_r8

   small = 1.e-36_r8

   mbsth = 1.e-15_r8


   ktm = pver
   kbm = pver
   do i = il1g, il2g
      ktm = min(ktm,jt(i))
      kbm = min(kbm,mx(i))
   end do


   do m = 1, ncnst                    
      if (domomtran(m)) then


         do k = 1,pver
            do i =il1g,il2g
               const(i,k) = q(ideep(i),k,m)
                wind0(i,k,m) = const(i,k)
            end do
         end do






         do k = 1,pver
            km1 = max(1,k-1)
            do i = il1g, il2g

               
               chat(i,k) = 0.5_r8* (const(i,k)+const(i,km1))


               conu(i,k) = chat(i,k)
               cond(i,k) = chat(i,k)


               dcondt(i,k) = 0._r8

            end do
         end do






      

         k=1
         pgu(:il2g,k) = 0.0_r8
         pgd(:il2g,k) = 0.0_r8

         do k=2,pver-1
            km1 = max(1,k-1)
            kp1 = min(pver,k+1)
            do i = il1g,il2g
            
               

               mududp(i,k) =  ( mu(i,k) * (const(i,k)- const(i,km1))/dp(i,km1) &
                           +  mu(i,kp1) * (const(i,kp1) - const(i,k))/dp(i,k))

               pgu(i,k) = - momcu * 0.5_r8 * mududp(i,k)
                           

               mddudp(i,k) =  ( md(i,k) * (const(i,k)- const(i,km1))/dp(i,km1) &
                           +  md(i,kp1) * (const(i,kp1) - const(i,k))/dp(i,k))

               pgd(i,k) = - momcd * 0.5_r8 * mddudp(i,k)


            end do
         end do

       
       k = pver
       km1 = max(1,k-1)
       do i=il1g,il2g

          mududp(i,k) =   mu(i,k) * (const(i,k)- const(i,km1))/dp(i,km1)
          pgu(i,k) = - momcu *  mududp(i,k)
          
          mddudp(i,k) =   md(i,k) * (const(i,k)- const(i,km1))/dp(i,km1) 

          pgd(i,k) = - momcd * mddudp(i,k)
          
       end do
       






         k = 2
         km1 = 1
         kk = pver
         kkm1 = max(1,kk-1)
         do i = il1g,il2g
            mupdudp = mu(i,kk) + du(i,kk)*dp(i,kk)
            if (mupdudp > mbsth) then
                 
               conu(i,kk) = (+eu(i,kk)*const(i,kk)*dp(i,kk)+pgu(i,kk)*dp(i,kk))/mupdudp
            endif
            if (md(i,k) < -mbsth) then
               cond(i,k) =  (-ed(i,km1)*const(i,km1)*dp(i,km1))-pgd(i,km1)*dp(i,km1)/md(i,k)
            endif

                        
         end do




         do kk = pver-1,1,-1
            kkm1 = max(1,kk-1)
            kkp1 = min(pver,kk+1)
            do i = il1g,il2g
               mupdudp = mu(i,kk) + du(i,kk)*dp(i,kk)
               if (mupdudp > mbsth) then
            
                  conu(i,kk) = (  mu(i,kkp1)*conu(i,kkp1)+eu(i,kk)* &
                                  const(i,kk)*dp(i,kk)+pgu(i,kk)*dp(i,kk))/mupdudp
               endif
            end do

         end do



         do k = 3,pver
            km1 = max(1,k-1)
            do i = il1g,il2g
               if (md(i,k) < -mbsth) then
                            
                  cond(i,k) =  (  md(i,km1)*cond(i,km1)-ed(i,km1)*const(i,km1) &
                                  *dp(i,km1)-pgd(i,km1)*dp(i,km1) )/md(i,k)

               endif
            end do
         end do


         sum = 0._r8
         sum2 = 0._r8


         do k = ktm,pver
            km1 = max(1,k-1)
            kp1 = min(pver,k+1)
            do i = il1g,il2g
               ii = ideep(i)
	

               dcondt(i,k) =  &
                           +(mu(i,kp1)* (conu(i,kp1)-chat(i,kp1)) &
                           -mu(i,k)*   (conu(i,k)-chat(i,k))      &
                           +md(i,kp1)* (cond(i,kp1)-chat(i,kp1)) &
                           -md(i,k)*   (cond(i,k)-chat(i,k)) &
                          )/dp(i,k)

            end do
         end do

  
          
          !DIR$ NOINTERCHANGE
          do k = kbm,pver
             km1 = max(1,k-1)
             do i = il1g,il2g
                if (k == mx(i)) then

                   
                   dcondt(i,k) = (1./dp(i,k))*   &  
                        (-mu(i,k)*(conu(i,k)-chat(i,k)) &
                        -md(i,k)*(cond(i,k)-chat(i,k)) &
                        )
                end if
             end do
          end do


         dqdt(:,:,m) = 0._r8

         do k = 1,pver
            do i = il1g,il2g
               ii = ideep(i)
               dqdt(ii,k,m) = dcondt(i,k)
    
               pguall(ii,k,m) = -pgu(i,k)
               pgdall(ii,k,m) = -pgd(i,k)
               icwu(ii,k,m)   =  conu(i,k)
               icwd(ii,k,m)   =  cond(i,k)
            end do
         end do

          

          do k = ktm,pver
             do i = il1g,il2g
                ii = ideep(i)
                mflux(i,k,m) = &
                     -mu(i,k)*   (conu(i,k)-chat(i,k))      &
                     -md(i,k)*   (cond(i,k)-chat(i,k))
             end do
          end do


          

          do k = ktm,pver
             do i = il1g,il2g
                ii = ideep(i)
                km1 = max(1,k-1)
                kp1 = k+1
                windf(i,k,m) = const(i,k)    -   (mflux(i,kp1,m) - mflux(i,k,m)) * dt /dp(i,k)

             end do
          end do

       end if      
   end do

 
    
    

    do k = ktm,pver
       km1 = max(1,k-1)
       kp1 = min(pver,k+1)
       do i = il1g,il2g

          ii = ideep(i)

          
          
          utop = (wind0(i,k,1)+wind0(i,km1,1))/2.
          vtop = (wind0(i,k,2)+wind0(i,km1,2))/2.
          ubot = (wind0(i,kp1,1)+wind0(i,k,1))/2.
          vbot = (wind0(i,kp1,2)+wind0(i,k,2))/2.
          fket = utop*mflux(i,k,1)   + vtop*mflux(i,k,2)    
          fkeb = ubot*mflux(i,k+1,1) + vbot*mflux(i,k+1,2)  

          
          ketend_cons = (fket-fkeb)/dp(i,k)

          
          ketend = ((windf(i,k,1)**2 + windf(i,k,2)**2) - (wind0(i,k,1)**2 + wind0(i,k,2)**2))*0.5/dt

          
          gset2 = ketend_cons - ketend
          gseten(i,k) = gset2

       end do

    end do

    
    do k = 1,pver
       do i = il1g,il2g
          ii = ideep(i)
          seten(ii,k) = gseten(i,k)

       end do
    end do

   return
end subroutine momtran



subroutine buoyan(lchnk   ,ncol    , &
                  q       ,t       ,p       ,z       ,pf      , &
                  tp      ,qstp    ,tl      ,rl      ,cape    , &
                  pblt    ,lcl     ,lel     ,lon     ,mx      , &
                  rd      ,grav    ,cp      ,msg     , &
                  tpert   )















   implicit none




   integer, intent(in) :: lchnk                 
   integer, intent(in) :: ncol                  

   real(r8), intent(in) :: q(pcols,pver)        
   real(r8), intent(in) :: t(pcols,pver)        
   real(r8), intent(in) :: p(pcols,pver)        
   real(r8), intent(in) :: z(pcols,pver)        
   real(r8), intent(in) :: pf(pcols,pver+1)     
   real(r8), intent(in) :: pblt(pcols)          
   real(r8), intent(in) :: tpert(pcols)         




   real(r8), intent(out) :: tp(pcols,pver)       
   real(r8), intent(out) :: qstp(pcols,pver)     
   real(r8), intent(out) :: tl(pcols)            
   real(r8), intent(out) :: cape(pcols)          
   integer lcl(pcols)        
   integer lel(pcols)        
   integer lon(pcols)        
   integer mx(pcols)         



   real(r8) capeten(pcols,5)     
   real(r8) tv(pcols,pver)       
   real(r8) tpv(pcols,pver)      
   real(r8) buoy(pcols,pver)

   real(r8) a1(pcols)
   real(r8) a2(pcols)
   real(r8) estp(pcols)
   real(r8) pl(pcols)
   real(r8) plexp(pcols)
   real(r8) hmax(pcols)
   real(r8) hmn(pcols)
   real(r8) y(pcols)

   logical plge600(pcols)
   integer knt(pcols)
   integer lelten(pcols,5)

   real(r8) cp
   real(r8) e
   real(r8) grav

   integer i
   integer k
   integer msg
   integer n

   real(r8) rd
   real(r8) rl



   do n = 1,5
      do i = 1,ncol
         lelten(i,n) = pver
         capeten(i,n) = 0._r8
      end do
   end do

   do i = 1,ncol
      lon(i) = pver
      knt(i) = 0
      lel(i) = pver
      mx(i) = lon(i)
      cape(i) = 0._r8
      hmax(i) = 0._r8
   end do

   tp(:ncol,:) = t(:ncol,:)
   qstp(:ncol,:) = q(:ncol,:)



   tv(:ncol,:) = t(:ncol,:) *(1._r8+1.608*q(:ncol,:))/ (1._r8+q(:ncol,:))
   tpv(:ncol,:) = tv(:ncol,:)
   buoy(:ncol,:) = 0._r8





   do k = pver,msg + 1,-1
      do i = 1,ncol
         hmn(i) = cp*t(i,k) + grav*z(i,k) + rl*q(i,k)
         if (k >= nint(pblt(i)) .and. k <= lon(i) .and. hmn(i) > hmax(i)) then
            hmax(i) = hmn(i)
            mx(i) = k
         end if
      end do
   end do

   do i = 1,ncol
      lcl(i) = mx(i)
      e = p(i,mx(i))*q(i,mx(i))/ (eps1+q(i,mx(i)))
      tl(i) = 2840._r8/ (3.5_r8*log(t(i,mx(i)))-log(e)-4.805_r8) + 55._r8
      if (tl(i) < t(i,mx(i))) then
         plexp(i) = (1._r8/ (0.2854_r8* (1._r8-0.28_r8*q(i,mx(i)))))
         pl(i) = p(i,mx(i))* (tl(i)/t(i,mx(i)))**plexp(i)
      else
         tl(i) = t(i,mx(i))
         pl(i) = p(i,mx(i))
      end if
   end do




   do k = pver,msg + 2,-1
      do i = 1,ncol
         if (k <= mx(i) .and. (p(i,k) > pl(i) .and. p(i,k-1) <= pl(i))) then
            lcl(i) = k - 1
         end if
      end do
   end do





   do i = 1,ncol
      plge600(i) = pl(i).ge.600._r8
   end do



   do k = pver,msg + 1,-1
      do i=1,ncol
         if (k > lcl(i) .and. k <= mx(i) .and. plge600(i)) then
            tv(i,k) = t(i,k)* (1._r8+1.608_r8*q(i,k))/ (1._r8+q(i,k))
            qstp(i,k) = q(i,mx(i))
            tp(i,k) = t(i,mx(i))* (p(i,k)/p(i,mx(i)))**(0.2854_r8* (1._r8-0.28_r8*q(i,mx(i))))





            tpv(i,k) = (tp(i,k)+tpert(i))*(1._r8+1.608_r8*q(i,mx(i)))/ (1._r8+q(i,mx(i)))
            buoy(i,k) = tpv(i,k) - tv(i,k) + tiedke_add
         end if
      end do
   end do




   do k = pver,msg + 1,-1
      do i=1,ncol
         if (k == lcl(i) .and. plge600(i)) then
            tv(i,k) = t(i,k)* (1._r8+1.608_r8*q(i,k))/ (1._r8+q(i,k))
            qstp(i,k) = q(i,mx(i))
            tp(i,k) = tl(i)* (p(i,k)/pl(i))**(0.2854_r8* (1._r8-0.28_r8*qstp(i,k)))





            estp(i) = c1*exp((c2* (tp(i,k)-tfreez))/((tp(i,k)-tfreez)+c3))

            qstp(i,k) = eps1*estp(i)/ (p(i,k)-estp(i))
            a1(i) = cp / rl + qstp(i,k) * (1._r8+ qstp(i,k) / eps1) * rl * eps1 / &
                    (rd * tp(i,k) ** 2)
            a2(i) = .5_r8* (qstp(i,k)* (1._r8+2._r8/eps1*qstp(i,k))* &
                    (1._r8+qstp(i,k)/eps1)*eps1**2*rl*rl/ &
                    (rd**2*tp(i,k)**4)-qstp(i,k)* &
                    (1._r8+qstp(i,k)/eps1)*2._r8*eps1*rl/ &
                    (rd*tp(i,k)**3))
            a1(i) = 1._r8/a1(i)
            a2(i) = -a2(i)*a1(i)**3
            y(i) = q(i,mx(i)) - qstp(i,k)
            tp(i,k) = tp(i,k) + a1(i)*y(i) + a2(i)*y(i)**2

            estp(i) = c1*exp((c2* (tp(i,k)-tfreez))/ ((tp(i,k)-tfreez)+c3))

            qstp(i,k) = eps1*estp(i) / (p(i,k)-estp(i))





            tpv(i,k) = (tp(i,k)+tpert(i))* (1._r8+1.608_r8*qstp(i,k)) / (1._r8+q(i,mx(i)))
            buoy(i,k) = tpv(i,k) - tv(i,k) + tiedke_add
         end if
      end do
   end do



   do k = pver - 1,msg + 1,-1
      do i=1,ncol
         if (k < lcl(i) .and. plge600(i)) then
            tv(i,k) = t(i,k)* (1._r8+1.608_r8*q(i,k))/ (1._r8+q(i,k))
            qstp(i,k) = qstp(i,k+1)
            tp(i,k) = tp(i,k+1)* (p(i,k)/p(i,k+1))**(0.2854_r8* (1._r8-0.28_r8*qstp(i,k)))

            estp(i) = c1*exp((c2* (tp(i,k)-tfreez))/((tp(i,k)-tfreez)+c3))

            qstp(i,k) = eps1*estp(i)/ (p(i,k)-estp(i))
            a1(i) = cp/rl + qstp(i,k)* (1._r8+qstp(i,k)/eps1)*rl*eps1/ (rd*tp(i,k)**2)
            a2(i) = .5_r8* (qstp(i,k)* (1._r8+2._r8/eps1*qstp(i,k))* &
                    (1._r8+qstp(i,k)/eps1)*eps1**2*rl*rl/ &
                    (rd**2*tp(i,k)**4)-qstp(i,k)* &
                    (1._r8+qstp(i,k)/eps1)*2._r8*eps1*rl/ &
                    (rd*tp(i,k)**3))
            a1(i) = 1._r8/a1(i)
            a2(i) = -a2(i)*a1(i)**3
            y(i) = qstp(i,k+1) - qstp(i,k)
            tp(i,k) = tp(i,k) + a1(i)*y(i) + a2(i)*y(i)**2

            estp(i) = c1*exp((c2* (tp(i,k)-tfreez))/ ((tp(i,k)-tfreez)+c3))

            qstp(i,k) = eps1*estp(i)/ (p(i,k)-estp(i))


            tpv(i,k) = (tp(i,k)+tpert(i))* (1._r8+1.608_r8*qstp(i,k))/(1._r8+q(i,mx(i)))
            buoy(i,k) = tpv(i,k) - tv(i,k) + tiedke_add
         end if
      end do
   end do


   do k = msg + 2,pver
      do i = 1,ncol
         if (k < lcl(i) .and. plge600(i)) then
            if (buoy(i,k+1) > 0._r8 .and. buoy(i,k) <= 0._r8) then
               knt(i) = min(5,knt(i) + 1)
               lelten(i,knt(i)) = k
            end if
         end if
      end do
   end do



   do n = 1,5
      do k = msg + 1,pver
         do i = 1,ncol
            if (plge600(i) .and. k <= mx(i) .and. k > lelten(i,n)) then
               capeten(i,n) = capeten(i,n) + rd*buoy(i,k)*log(pf(i,k+1)/pf(i,k))
            end if
         end do
      end do
   end do





   do n = 1,5
      do i = 1,ncol
         if (capeten(i,n) > cape(i)) then
            cape(i) = capeten(i,n)
            lel(i) = lelten(i,n)
         end if
      end do
   end do



   do i = 1,ncol
      cape(i) = max(cape(i), 0._r8)
   end do

   return
end subroutine buoyan

subroutine cldprp(lchnk   , &
                  q       ,t       ,u       ,v       ,p       , &
                  z       ,s       ,mu      ,eu      ,du      , &
                  md      ,ed      ,sd      ,qd      ,mc      , &
                  qu      ,su      ,zf      ,qst     ,hmn     , &
                  hsat    ,shat    ,ql      , &
                  cmeg    ,jb      ,lel     ,jt      ,jlcl    , &
                  mx      ,j0      ,jd      ,rl      ,il2g    , &
                  rd      ,grav    ,cp      ,msg     , &
                  pflx    ,evp     ,cu      ,rprd    ,limcnv  ,landfrac)





















   implicit none





   integer, intent(in) :: lchnk                  

   real(r8), intent(in) :: q(pcols,pver)         
   real(r8), intent(in) :: t(pcols,pver)         
   real(r8), intent(in) :: p(pcols,pver)         
   real(r8), intent(in) :: z(pcols,pver)         
   real(r8), intent(in) :: s(pcols,pver)         
   real(r8), intent(in) :: zf(pcols,pverp)       
   real(r8), intent(in) :: u(pcols,pver)         
   real(r8), intent(in) :: v(pcols,pver)         

   real(r8), intent(in) :: landfrac(pcols) 

   integer, intent(in) :: jb(pcols)              
   integer, intent(in) :: lel(pcols)             
   integer, intent(out) :: jt(pcols)              
   integer, intent(out) :: jlcl(pcols)            
   integer, intent(in) :: mx(pcols)              
   integer, intent(out) :: j0(pcols)              
   integer, intent(out) :: jd(pcols)              
   integer, intent(in) :: limcnv                 
   integer, intent(in) :: il2g                   
   integer, intent(in) :: msg                    
   real(r8), intent(in) :: rl                    
   real(r8), intent(in) :: shat(pcols,pver)      



   real(r8), intent(out) :: rprd(pcols,pver)     
   real(r8), intent(out) :: du(pcols,pver)       
   real(r8), intent(out) :: ed(pcols,pver)       
   real(r8), intent(out) :: eu(pcols,pver)       
   real(r8), intent(out) :: hmn(pcols,pver)      
   real(r8), intent(out) :: hsat(pcols,pver)     
   real(r8), intent(out) :: mc(pcols,pver)       
   real(r8), intent(out) :: md(pcols,pver)       
   real(r8), intent(out) :: mu(pcols,pver)       
   real(r8), intent(out) :: pflx(pcols,pverp)    
   real(r8), intent(out) :: qd(pcols,pver)       
   real(r8), intent(out) :: ql(pcols,pver)       
   real(r8), intent(out) :: qst(pcols,pver)      
   real(r8), intent(out) :: qu(pcols,pver)       
   real(r8), intent(out) :: sd(pcols,pver)       
   real(r8), intent(out) :: su(pcols,pver)       


   real(r8) rd                   
   real(r8) grav                 
   real(r8) cp                   




   real(r8) gamma(pcols,pver)
   real(r8) dz(pcols,pver)
   real(r8) iprm(pcols,pver)
   real(r8) hu(pcols,pver)
   real(r8) hd(pcols,pver)
   real(r8) eps(pcols,pver)
   real(r8) f(pcols,pver)
   real(r8) k1(pcols,pver)
   real(r8) i2(pcols,pver)
   real(r8) ihat(pcols,pver)
   real(r8) i3(pcols,pver)
   real(r8) idag(pcols,pver)
   real(r8) i4(pcols,pver)
   real(r8) qsthat(pcols,pver)
   real(r8) hsthat(pcols,pver)
   real(r8) gamhat(pcols,pver)
   real(r8) cu(pcols,pver)
   real(r8) evp(pcols,pver)
   real(r8) cmeg(pcols,pver)
   real(r8) qds(pcols,pver)

   real(r8) c0mask(pcols)

   real(r8) hmin(pcols)
   real(r8) expdif(pcols)
   real(r8) expnum(pcols)
   real(r8) ftemp(pcols)
   real(r8) eps0(pcols)
   real(r8) rmue(pcols)
   real(r8) zuef(pcols)
   real(r8) zdef(pcols)
   real(r8) epsm(pcols)
   real(r8) ratmjb(pcols)
   real(r8) est(pcols)
   real(r8) totpcp(pcols)
   real(r8) totevp(pcols)
   real(r8) alfa(pcols)
   real(r8) ql1
   real(r8) tu
   real(r8) estu
   real(r8) qstu

   real(r8) small
   real(r8) mdt

   integer khighest
   integer klowest
   integer kount
   integer i,k

   logical doit(pcols)
   logical done(pcols)



   do i = 1,il2g
      ftemp(i) = 0._r8
      expnum(i) = 0._r8
      expdif(i) = 0._r8
      c0mask(i)  = c0_ocn * (1._r8-landfrac(i)) +   c0_lnd * landfrac(i) 
   end do



   do k = 1,pver
      do i = 1,il2g
         dz(i,k) = zf(i,k) - zf(i,k+1)
      end do
   end do




   pflx(:il2g,1) = 0

   do k = 1,pver
      do i = 1,il2g
         k1(i,k) = 0._r8
         i2(i,k) = 0._r8
         i3(i,k) = 0._r8
         i4(i,k) = 0._r8
         mu(i,k) = 0._r8
         f(i,k) = 0._r8
         eps(i,k) = 0._r8
         eu(i,k) = 0._r8
         du(i,k) = 0._r8
         ql(i,k) = 0._r8
         cu(i,k) = 0._r8
         evp(i,k) = 0._r8
         cmeg(i,k) = 0._r8
         qds(i,k) = q(i,k)
         md(i,k) = 0._r8
         ed(i,k) = 0._r8
         sd(i,k) = s(i,k)
         qd(i,k) = q(i,k)
         mc(i,k) = 0._r8
         qu(i,k) = q(i,k)
         su(i,k) = s(i,k)

         est(i) = c1*exp((c2* (t(i,k)-tfreez))/((t(i,k)-tfreez)+c3))

         if ( p(i,k)-est(i) > 0._r8 ) then
            qst(i,k) = eps1*est(i)/ (p(i,k)-est(i))
         else
            qst(i,k) = 1.0_r8
         end if

         gamma(i,k) = qst(i,k)*(1._r8 + qst(i,k)/eps1)*eps1*rl/(rd*t(i,k)**2)*rl/cp
         hmn(i,k) = cp*t(i,k) + grav*z(i,k) + rl*q(i,k)
         hsat(i,k) = cp*t(i,k) + grav*z(i,k) + rl*qst(i,k)
         hu(i,k) = hmn(i,k)
         hd(i,k) = hmn(i,k)
         rprd(i,k) = 0._r8
      end do
   end do



   do k=1,msg
      do i=1,il2g
         rprd(i,k) = 0._r8
      end do
   end do




   do i = 1,il2g
      hsthat(i,msg+1) = hsat(i,msg+1)
      qsthat(i,msg+1) = qst(i,msg+1)
      gamhat(i,msg+1) = gamma(i,msg+1)
      totpcp(i) = 0._r8
      totevp(i) = 0._r8
   end do
   do k = msg + 2,pver
      do i = 1,il2g
         if (abs(qst(i,k-1)-qst(i,k)) > 1.E-6_r8) then
            qsthat(i,k) = log(qst(i,k-1)/qst(i,k))*qst(i,k-1)*qst(i,k)/ (qst(i,k-1)-qst(i,k))
         else
            qsthat(i,k) = qst(i,k)
         end if
         hsthat(i,k) = cp*shat(i,k) + rl*qsthat(i,k)
         if (abs(gamma(i,k-1)-gamma(i,k)) > 1.E-6_r8) then
            gamhat(i,k) = log(gamma(i,k-1)/gamma(i,k))*gamma(i,k-1)*gamma(i,k)/ &
                                (gamma(i,k-1)-gamma(i,k))
         else
            gamhat(i,k) = gamma(i,k)
         end if
      end do
   end do




   jt(:) = pver
   do i = 1,il2g
      jt(i) = max(lel(i),limcnv+1)
      jt(i) = min(jt(i),pver)
      jd(i) = pver
      jlcl(i) = lel(i)
      hmin(i) = 1.E6_r8
   end do




   do k = msg + 1,pver
      do i = 1,il2g
         if (hsat(i,k) <= hmin(i) .and. k >= jt(i) .and. k <= jb(i)) then
            hmin(i) = hsat(i,k)
            j0(i) = k
         end if
      end do
   end do
   do i = 1,il2g
      j0(i) = min(j0(i),jb(i)-2)
      j0(i) = max(j0(i),jt(i)+2)



      j0(i) = min(j0(i),pver)
   end do



   do k = msg + 1,pver
      do i = 1,il2g
         if (k >= jt(i) .and. k <= jb(i)) then
            hu(i,k) = hmn(i,mx(i)) + cp*tiedke_add
            su(i,k) = s(i,mx(i)) + tiedke_add
         end if
      end do
   end do





   do k = pver - 1,msg + 1,-1
      do i = 1,il2g
         if (k < jb(i) .and. k >= jt(i)) then
            k1(i,k) = k1(i,k+1) + (hmn(i,mx(i))-hmn(i,k))*dz(i,k)
            ihat(i,k) = 0.5_r8* (k1(i,k+1)+k1(i,k))
            i2(i,k) = i2(i,k+1) + ihat(i,k)*dz(i,k)
            idag(i,k) = 0.5_r8* (i2(i,k+1)+i2(i,k))
            i3(i,k) = i3(i,k+1) + idag(i,k)*dz(i,k)
            iprm(i,k) = 0.5_r8* (i3(i,k+1)+i3(i,k))
            i4(i,k) = i4(i,k+1) + iprm(i,k)*dz(i,k)
         end if
      end do
   end do



   do i = 1,il2g
      hmin(i) = 1.E6_r8
   end do
   do k = msg + 1,pver
      do i = 1,il2g
         if (k >= j0(i) .and. k <= jb(i) .and. hmn(i,k) <= hmin(i)) then
            hmin(i) = hmn(i,k)
            expdif(i) = hmn(i,mx(i)) - hmin(i)
         end if
      end do
   end do





   do k = msg + 2,pver
      do i = 1,il2g
         expnum(i) = 0._r8
         ftemp(i) = 0._r8
         if (k < jt(i) .or. k >= jb(i)) then
            k1(i,k) = 0._r8
            expnum(i) = 0._r8
         else
            expnum(i) = hmn(i,mx(i)) - (hsat(i,k-1)*(zf(i,k)-z(i,k)) + &
                        hsat(i,k)* (z(i,k-1)-zf(i,k)))/(z(i,k-1)-z(i,k))
         end if
         if ((expdif(i) > 100._r8 .and. expnum(i) > 0._r8) .and. &
	     k1(i,k) > expnum(i)*dz(i,k)) then
            ftemp(i) = expnum(i)/k1(i,k)
            f(i,k) = ftemp(i) + i2(i,k)/k1(i,k)*ftemp(i)**2 + &
                     (2._r8*i2(i,k)**2-k1(i,k)*i3(i,k))/k1(i,k)**2* &
                     ftemp(i)**3 + (-5._r8*k1(i,k)*i2(i,k)*i3(i,k)+ &
                     5._r8*i2(i,k)**3+k1(i,k)**2*i4(i,k))/ &
                     k1(i,k)**3*ftemp(i)**4
            f(i,k) = max(f(i,k),0._r8)
            f(i,k) = min(f(i,k),0.0002_r8)
         end if
      end do
   end do
   do i = 1,il2g
      if (j0(i) < jb(i)) then
         if (f(i,j0(i)) < 1.E-6_r8 .and. f(i,j0(i)+1) > f(i,j0(i))) j0(i) = j0(i) + 1
      end if
   end do
   do k = msg + 2,pver
      do i = 1,il2g
         if (k >= jt(i) .and. k <= j0(i)) then
            f(i,k) = max(f(i,k),f(i,k-1))
         end if
      end do
   end do
   do i = 1,il2g
      eps0(i) = f(i,j0(i))
      eps(i,jb(i)) = eps0(i)
   end do



   do k = pver,msg + 1,-1
      do i = 1,il2g
         if (k >= j0(i) .and. k <= jb(i)) then
            eps(i,k) = f(i,j0(i))
         end if
      end do
   end do
   do k = pver,msg + 1,-1
      do i = 1,il2g
         if (k < j0(i) .and. k >= jt(i)) eps(i,k) = f(i,k)
      end do
   end do





   do i = 1,il2g
      if (eps0(i) > 0._r8) then
         mu(i,jb(i)) = 1._r8
         eu(i,jb(i)) = mu(i,jb(i))/dz(i,jb(i))
      end if
   end do
   do k = pver,msg + 1,-1
      do i = 1,il2g
         if (eps0(i) > 0._r8 .and. (k >= jt(i) .and. k < jb(i))) then
            zuef(i) = zf(i,k) - zf(i,jb(i))
            rmue(i) = (1._r8/eps0(i))* (exp(eps(i,k+1)*zuef(i))-1._r8)/zuef(i)
            mu(i,k) = (1._r8/eps0(i))* (exp(eps(i,k  )*zuef(i))-1._r8)/zuef(i)
            eu(i,k) = (rmue(i)-mu(i,k+1))/dz(i,k)
            du(i,k) = (rmue(i)-mu(i,k))/dz(i,k)
         end if
      end do
   end do

   khighest = pverp
   klowest = 1
   do i=1,il2g
      khighest = min(khighest,lel(i))
      klowest = max(klowest,jb(i))
   end do
   do k = klowest-1,khighest,-1
!cdir$ ivdep
      do i = 1,il2g
         if (k <= jb(i)-1 .and. k >= lel(i) .and. eps0(i) > 0._r8) then
            if (mu(i,k) < 0.01_r8) then
               hu(i,k) = hu(i,jb(i))
               mu(i,k) = 0._r8
               eu(i,k) = 0._r8
               du(i,k) = mu(i,k+1)/dz(i,k)
            else
               hu(i,k) = mu(i,k+1)/mu(i,k)*hu(i,k+1) + &
                         dz(i,k)/mu(i,k)* (eu(i,k)*hmn(i,k)- du(i,k)*hsat(i,k))
            end if
         end if
      end do
   end do




   do i=1,il2g
      doit(i) = .true.
   end do
   do k=klowest-2,khighest-1,-1
      do i=1,il2g
         if (doit(i) .and. k <= jb(i)-2 .and. k >= lel(i)-1) then
  	   if (hu(i,k) <= hsthat(i,k) .and. hu(i,k+1) > hsthat(i,k+1) &
	       .and. mu(i,k) >= 0.02_r8) then
               if (hu(i,k)-hsthat(i,k) < -2000._r8) then
                  jt(i) = k + 1
                  doit(i) = .false.
               else
                  jt(i) = k
                  if (eps0(i) <= 0._r8) doit(i) = .false.
               end if
            else if (hu(i,k) > hu(i,jb(i)) .or. mu(i,k) < 0.01_r8) then
               jt(i) = k + 1
               doit(i) = .false.
            end if
         end if
      end do
   end do
   do k = pver,msg + 1,-1
!cdir$ ivdep
      do i = 1,il2g
         if (k >= lel(i) .and. k <= jt(i) .and. eps0(i) > 0._r8) then
            mu(i,k) = 0._r8
            eu(i,k) = 0._r8
            du(i,k) = 0._r8
            hu(i,k) = hu(i,jb(i))
         end if
         if (k == jt(i) .and. eps0(i) > 0._r8) then
            du(i,k) = mu(i,k+1)/dz(i,k)
            eu(i,k) = 0._r8
            mu(i,k) = 0._r8
         end if
      end do
   end do





   do i = 1,il2g



      alfa(i) = 0.1_r8
      jt(i) = min(jt(i),jb(i)-1)
      jd(i) = max(j0(i),jt(i)+1)
      jd(i) = min(jd(i),jb(i))
      hd(i,jd(i)) = hmn(i,jd(i)-1)
      if (jd(i) < jb(i) .and. eps0(i) > 0._r8) then
         epsm(i) = eps0(i)
         md(i,jd(i)) = -alfa(i)*epsm(i)/eps0(i)
      end if
   end do
   do k = msg + 1,pver
      do i = 1,il2g
         if ((k > jd(i) .and. k <= jb(i)) .and. eps0(i) > 0._r8) then
            zdef(i) = zf(i,jd(i)) - zf(i,k)
            md(i,k) = -alfa(i)/ (2._r8*eps0(i))*(exp(2._r8*epsm(i)*zdef(i))-1._r8)/zdef(i)
         end if
      end do
   end do
   do k = msg + 1,pver
      do i = 1,il2g
         if ((k >= jt(i) .and. k <= jb(i)) .and. eps0(i) > 0._r8 .and. jd(i) < jb(i)) then
            ratmjb(i) = min(abs(mu(i,jb(i))/md(i,jb(i))),1._r8)
            md(i,k) = md(i,k)*ratmjb(i)
         end if
      end do
   end do

   small = 1.e-20_r8
   do k = msg + 1,pver
      do i = 1,il2g
         if ((k >= jt(i) .and. k <= pver) .and. eps0(i) > 0._r8) then
            ed(i,k-1) = (md(i,k-1)-md(i,k))/dz(i,k-1)
            mdt = min(md(i,k),-small)
            hd(i,k) = (md(i,k-1)*hd(i,k-1) - dz(i,k-1)*ed(i,k-1)*hmn(i,k-1))/mdt
         end if
      end do
   end do



   do k = msg + 2,pver
      do i = 1,il2g
         if ((k >= jd(i) .and. k <= jb(i)) .and. eps0(i) > 0._r8 .and. jd(i) < jb(i)) then
            qds(i,k) = qsthat(i,k) + gamhat(i,k)*(hd(i,k)-hsthat(i,k))/ &
               (rl*(1._r8 + gamhat(i,k)))
         end if
      end do
   end do

   do i = 1,il2g
      done(i) = .false.
   end do
   kount = 0
   do k = pver,msg + 2,-1
      do i = 1,il2g
         if (( .not. done(i) .and. k > jt(i) .and. k < jb(i)) .and. eps0(i) > 0._r8) then
            su(i,k) = mu(i,k+1)/mu(i,k)*su(i,k+1) + &
                      dz(i,k)/mu(i,k)* (eu(i,k)-du(i,k))*s(i,k)
            qu(i,k) = mu(i,k+1)/mu(i,k)*qu(i,k+1) + dz(i,k)/mu(i,k)* (eu(i,k)*q(i,k)- &
                            du(i,k)*qst(i,k))
            tu = su(i,k) - grav/cp*zf(i,k)
            estu = c1*exp((c2* (tu-tfreez))/ ((tu-tfreez)+c3))
            qstu = eps1*estu/ ((p(i,k)+p(i,k-1))/2._r8-estu)
            if (qu(i,k) >= qstu) then
               jlcl(i) = k
               kount = kount + 1
               done(i) = .true.
            end if
         end if
      end do
      if (kount >= il2g) goto 690
   end do
690 continue
   do k = msg + 2,pver
      do i = 1,il2g
         if (k == jb(i) .and. eps0(i) > 0._r8) then
            qu(i,k) = q(i,mx(i))
            su(i,k) = (hu(i,k)-rl*qu(i,k))/cp
         end if
         if ((k > jt(i) .and. k <= jlcl(i)) .and. eps0(i) > 0._r8) then
            su(i,k) = shat(i,k) + (hu(i,k)-hsthat(i,k))/(cp* (1._r8+gamhat(i,k)))
            qu(i,k) = qsthat(i,k) + gamhat(i,k)*(hu(i,k)-hsthat(i,k))/ &
                     (rl* (1._r8+gamhat(i,k)))
         end if
      end do
   end do


   do k = pver,msg + 2,-1
      do i = 1,il2g
         if (k >= jt(i) .and. k < jb(i) .and. eps0(i) > 0._r8) then
            cu(i,k) = ((mu(i,k)*su(i,k)-mu(i,k+1)*su(i,k+1))/ &
                      dz(i,k)- (eu(i,k)-du(i,k))*s(i,k))/(rl/cp)
            if (k == jt(i)) cu(i,k) = 0._r8
            cu(i,k) = max(0._r8,cu(i,k))
         end if
      end do
   end do








   do k = pver,msg + 2,-1
      do i = 1,il2g
         rprd(i,k) = 0._r8
         if (k >= jt(i) .and. k < jb(i) .and. eps0(i) > 0._r8 .and. mu(i,k) >= 0.0_r8) then
            if (mu(i,k) > 0._r8) then
               ql1 = 1._r8/mu(i,k)* (mu(i,k+1)*ql(i,k+1)- &
                     dz(i,k)*du(i,k)*ql(i,k+1)+dz(i,k)*cu(i,k))
               ql(i,k) = ql1/ (1._r8+dz(i,k)*c0mask(i))
            else
               ql(i,k) = 0._r8
            end if
            totpcp(i) = totpcp(i) + dz(i,k)*(cu(i,k)-du(i,k)*ql(i,k+1))
            rprd(i,k) = c0mask(i)*mu(i,k)*ql(i,k)
         end if
      end do
   end do

   do i = 1,il2g
      qd(i,jd(i)) = qds(i,jd(i))
      sd(i,jd(i)) = (hd(i,jd(i)) - rl*qd(i,jd(i)))/cp
   end do

   do k = msg + 2,pver
      do i = 1,il2g
         if (k >= jd(i) .and. k < jb(i) .and. eps0(i) > 0._r8) then
            qd(i,k+1) = qds(i,k+1)
            evp(i,k) = -ed(i,k)*q(i,k) + (md(i,k)*qd(i,k)-md(i,k+1)*qd(i,k+1))/dz(i,k)
            evp(i,k) = max(evp(i,k),0._r8)
            mdt = min(md(i,k+1),-small)
            sd(i,k+1) = ((rl/cp*evp(i,k)-ed(i,k)*s(i,k))*dz(i,k) + md(i,k)*sd(i,k))/mdt
            totevp(i) = totevp(i) - dz(i,k)*ed(i,k)*q(i,k)
         end if
      end do
   end do
   do i = 1,il2g

      totevp(i) = totevp(i) + md(i,jd(i))*qd(i,jd(i)) - md(i,jb(i))*qd(i,jb(i))
   end do

   if (.false.) then
      do i = 1,il2g
         k = jb(i)
         if (eps0(i) > 0._r8) then
            evp(i,k) = -ed(i,k)*q(i,k) + (md(i,k)*qd(i,k))/dz(i,k)
            evp(i,k) = max(evp(i,k),0._r8)
            totevp(i) = totevp(i) - dz(i,k)*ed(i,k)*q(i,k)
         end if
      end do
   endif

   do i = 1,il2g
      totpcp(i) = max(totpcp(i),0._r8)
      totevp(i) = max(totevp(i),0._r8)
   end do

   do k = msg + 2,pver
      do i = 1,il2g
         if (totevp(i) > 0._r8 .and. totpcp(i) > 0._r8) then
            md(i,k)  = md (i,k)*min(1._r8, totpcp(i)/(totevp(i)+totpcp(i)))
            ed(i,k)  = ed (i,k)*min(1._r8, totpcp(i)/(totevp(i)+totpcp(i)))
            evp(i,k) = evp(i,k)*min(1._r8, totpcp(i)/(totevp(i)+totpcp(i)))
         else
            md(i,k) = 0._r8
            ed(i,k) = 0._r8
            evp(i,k) = 0._r8
         end if


         cmeg(i,k) = cu(i,k) - evp(i,k)
         rprd(i,k) = rprd(i,k)-evp(i,k)
      end do
   end do


   pflx(:il2g,1) = 0._r8
   do k = 2,pverp
      do i = 1,il2g
         pflx(i,k) = pflx(i,k-1) + rprd(i,k-1)*dz(i,k-1)
      end do
   end do

   do k = msg + 1,pver
      do i = 1,il2g
         mc(i,k) = mu(i,k) + md(i,k)
      end do
   end do

   return
end subroutine cldprp

subroutine closure(lchnk   , &
                   q       ,t       ,p       ,z       ,s       , &
                   tp      ,qs      ,qu      ,su      ,mc      , &
                   du      ,mu      ,md      ,qd      ,sd      , &
                   qhat    ,shat    ,dp      ,qstp    ,zf      , &
                   ql      ,dsubcld ,mb      ,cape    ,tl      , &
                   lcl     ,lel     ,jt      ,mx      ,il1g    , &
                   il2g    ,rd      ,grav    ,cp      ,rl      , &
                   msg     ,capelmt )



















   implicit none




   integer, intent(in) :: lchnk                 

   real(r8), intent(inout) :: q(pcols,pver)        
   real(r8), intent(inout) :: t(pcols,pver)        
   real(r8), intent(inout) :: p(pcols,pver)        
   real(r8), intent(inout) :: mb(pcols)            
   real(r8), intent(in) :: z(pcols,pver)        
   real(r8), intent(in) :: s(pcols,pver)        
   real(r8), intent(in) :: tp(pcols,pver)       
   real(r8), intent(in) :: qs(pcols,pver)       
   real(r8), intent(in) :: qu(pcols,pver)       
   real(r8), intent(in) :: su(pcols,pver)       
   real(r8), intent(in) :: mc(pcols,pver)       
   real(r8), intent(in) :: du(pcols,pver)       
   real(r8), intent(in) :: mu(pcols,pver)       
   real(r8), intent(in) :: md(pcols,pver)       
   real(r8), intent(in) :: qd(pcols,pver)       
   real(r8), intent(in) :: sd(pcols,pver)       
   real(r8), intent(in) :: qhat(pcols,pver)     
   real(r8), intent(in) :: shat(pcols,pver)     
   real(r8), intent(in) :: dp(pcols,pver)       
   real(r8), intent(in) :: qstp(pcols,pver)     
   real(r8), intent(in) :: zf(pcols,pver+1)     
   real(r8), intent(in) :: ql(pcols,pver)       

   real(r8), intent(in) :: cape(pcols)          
   real(r8), intent(in) :: tl(pcols)
   real(r8), intent(in) :: dsubcld(pcols)       

   integer, intent(in) :: lcl(pcols)        
   integer, intent(in) :: lel(pcols)        
   integer, intent(in) :: jt(pcols)         
   integer, intent(in) :: mx(pcols)         



   real(r8) dtpdt(pcols,pver)
   real(r8) dqsdtp(pcols,pver)
   real(r8) dtmdt(pcols,pver)
   real(r8) dqmdt(pcols,pver)
   real(r8) dboydt(pcols,pver)
   real(r8) thetavp(pcols,pver)
   real(r8) thetavm(pcols,pver)

   real(r8) dtbdt(pcols),dqbdt(pcols),dtldt(pcols)
   real(r8) beta
   real(r8) capelmt
   real(r8) cp
   real(r8) dadt(pcols)
   real(r8) debdt
   real(r8) dltaa
   real(r8) eb
   real(r8) grav

   integer i
   integer il1g
   integer il2g
   integer k, kmin, kmax
   integer msg

   real(r8) rd
   real(r8) rl








   do i = il1g,il2g
      mb(i) = 0._r8
      eb = p(i,mx(i))*q(i,mx(i))/ (eps1+q(i,mx(i)))
      dtbdt(i) = (1._r8/dsubcld(i))* (mu(i,mx(i))*(shat(i,mx(i))-su(i,mx(i)))+ &
                  md(i,mx(i))* (shat(i,mx(i))-sd(i,mx(i))))
      dqbdt(i) = (1._r8/dsubcld(i))* (mu(i,mx(i))*(qhat(i,mx(i))-qu(i,mx(i)))+ &
                 md(i,mx(i))* (qhat(i,mx(i))-qd(i,mx(i))))
      debdt = eps1*p(i,mx(i))/ (eps1+q(i,mx(i)))**2*dqbdt(i)
      dtldt(i) = -2840._r8* (3.5_r8/t(i,mx(i))*dtbdt(i)-debdt/eb)/ &
                 (3.5_r8*log(t(i,mx(i)))-log(eb)-4.805_r8)**2
   end do



   do k = msg + 1,pver
      do i = il1g,il2g
         dtmdt(i,k) = 0._r8
         dqmdt(i,k) = 0._r8
      end do
   end do

   do k = msg + 1,pver - 1
      do i = il1g,il2g
         if (k == jt(i)) then
            dtmdt(i,k) = (1._r8/dp(i,k))*(mu(i,k+1)* (su(i,k+1)-shat(i,k+1)- &
                          rl/cp*ql(i,k+1))+md(i,k+1)* (sd(i,k+1)-shat(i,k+1)))
            dqmdt(i,k) = (1._r8/dp(i,k))*(mu(i,k+1)* (qu(i,k+1)- &
                         qhat(i,k+1)+ql(i,k+1))+md(i,k+1)*(qd(i,k+1)-qhat(i,k+1)))
         end if
      end do
   end do

   beta = 0._r8
   do k = msg + 1,pver - 1
      do i = il1g,il2g
         if (k > jt(i) .and. k < mx(i)) then
            dtmdt(i,k) = (mc(i,k)* (shat(i,k)-s(i,k))+mc(i,k+1)* (s(i,k)-shat(i,k+1)))/ &
                         dp(i,k) - rl/cp*du(i,k)*(beta*ql(i,k)+ (1-beta)*ql(i,k+1))





            dqmdt(i,k) = (mu(i,k+1)* (qu(i,k+1)-qhat(i,k+1)+cp/rl* (su(i,k+1)-s(i,k)))- &
                          mu(i,k)* (qu(i,k)-qhat(i,k)+cp/rl*(su(i,k)-s(i,k)))+md(i,k+1)* &
                         (qd(i,k+1)-qhat(i,k+1)+cp/rl*(sd(i,k+1)-s(i,k)))-md(i,k)* &
                         (qd(i,k)-qhat(i,k)+cp/rl*(sd(i,k)-s(i,k))))/dp(i,k) + &
                          du(i,k)* (beta*ql(i,k)+(1-beta)*ql(i,k+1))
         end if
      end do
   end do

   do k = msg + 1,pver
      do i = il1g,il2g
         if (k >= lel(i) .and. k <= lcl(i)) then
            thetavp(i,k) = tp(i,k)* (1000._r8/p(i,k))** (rd/cp)*(1._r8+1.608_r8*qstp(i,k)-q(i,mx(i)))
            thetavm(i,k) = t(i,k)* (1000._r8/p(i,k))** (rd/cp)*(1._r8+0.608_r8*q(i,k))
            dqsdtp(i,k) = qstp(i,k)* (1._r8+qstp(i,k)/eps1)*eps1*rl/(rd*tp(i,k)**2)




            dtpdt(i,k) = tp(i,k)/ (1._r8+rl/cp* (dqsdtp(i,k)-qstp(i,k)/tp(i,k)))* &
                        (dtbdt(i)/t(i,mx(i))+rl/cp* (dqbdt(i)/tl(i)-q(i,mx(i))/ &
                         tl(i)**2*dtldt(i)))



            dboydt(i,k) = ((dtpdt(i,k)/tp(i,k)+1._r8/(1._r8+1.608_r8*qstp(i,k)-q(i,mx(i)))* &
                          (1.608_r8 * dqsdtp(i,k) * dtpdt(i,k) -dqbdt(i))) - (dtmdt(i,k)/t(i,k)+0.608_r8/ &
                          (1._r8+0.608_r8*q(i,k))*dqmdt(i,k)))*grav*thetavp(i,k)/thetavm(i,k)
         end if
      end do
   end do

   do k = msg + 1,pver
      do i = il1g,il2g
         if (k > lcl(i) .and. k < mx(i)) then
            thetavp(i,k) = tp(i,k)* (1000._r8/p(i,k))** (rd/cp)*(1._r8+0.608_r8*q(i,mx(i)))
            thetavm(i,k) = t(i,k)* (1000._r8/p(i,k))** (rd/cp)*(1._r8+0.608_r8*q(i,k))



            dboydt(i,k) = (dtbdt(i)/t(i,mx(i))+0.608_r8/ (1._r8+0.608_r8*q(i,mx(i)))*dqbdt(i)- &
                          dtmdt(i,k)/t(i,k)-0.608_r8/ (1._r8+0.608_r8*q(i,k))*dqmdt(i,k))* &
                          grav*thetavp(i,k)/thetavm(i,k)
         end if
      end do
   end do




   dadt(il1g:il2g)  = 0._r8
   kmin = minval(lel(il1g:il2g))
   kmax = maxval(mx(il1g:il2g)) - 1
   do k = kmin, kmax
      do i = il1g,il2g
         if ( k >= lel(i) .and. k <= mx(i) - 1) then
            dadt(i) = dadt(i) + dboydt(i,k)* (zf(i,k)-zf(i,k+1))
         endif
      end do
   end do
   do i = il1g,il2g
      dltaa = -1._r8* (cape(i)-capelmt)
      if (dadt(i) /= 0._r8) mb(i) = max(dltaa/tau/dadt(i),0._r8)
   end do

   return
end subroutine closure

subroutine q1q2_pjr(lchnk   , &
                    dqdt    ,dsdt    ,q       ,qs      ,qu      , &
                    su      ,du      ,qhat    ,shat    ,dp      , &
                    mu      ,md      ,sd      ,qd      ,ql      , &
                    dsubcld ,jt      ,mx      ,il1g    ,il2g    , &
                    cp      ,rl      ,msg     ,          &
                    dl      ,evp     ,cu      )


   implicit none















   real(r8), intent(in) :: cp

   integer, intent(in) :: lchnk             
   integer, intent(in) :: il1g
   integer, intent(in) :: il2g
   integer, intent(in) :: msg

   real(r8), intent(in) :: q(pcols,pver)
   real(r8), intent(in) :: qs(pcols,pver)
   real(r8), intent(in) :: qu(pcols,pver)
   real(r8), intent(in) :: su(pcols,pver)
   real(r8), intent(in) :: du(pcols,pver)
   real(r8), intent(in) :: qhat(pcols,pver)
   real(r8), intent(in) :: shat(pcols,pver)
   real(r8), intent(in) :: dp(pcols,pver)
   real(r8), intent(in) :: mu(pcols,pver)
   real(r8), intent(in) :: md(pcols,pver)
   real(r8), intent(in) :: sd(pcols,pver)
   real(r8), intent(in) :: qd(pcols,pver)
   real(r8), intent(in) :: ql(pcols,pver)
   real(r8), intent(in) :: evp(pcols,pver)
   real(r8), intent(in) :: cu(pcols,pver)
   real(r8), intent(in) :: dsubcld(pcols)

   real(r8),intent(out) :: dqdt(pcols,pver),dsdt(pcols,pver)
   real(r8),intent(out) :: dl(pcols,pver)
   integer kbm
   integer ktm
   integer jt(pcols)
   integer mx(pcols)



   integer i
   integer k

   real(r8) emc
   real(r8) rl

   do k = msg + 1,pver
      do i = il1g,il2g
         dsdt(i,k) = 0._r8
         dqdt(i,k) = 0._r8
         dl(i,k) = 0._r8
      end do
   end do



   ktm = pver
   kbm = pver
   do i = il1g, il2g
      ktm = min(ktm,jt(i))
      kbm = min(kbm,mx(i))
   end do

   do k = ktm,pver-1
      do i = il1g,il2g
         emc = -cu (i,k)               &         
               +evp(i,k)                         

         dsdt(i,k) = -rl/cp*emc &
                     + (+mu(i,k+1)* (su(i,k+1)-shat(i,k+1)) &
                        -mu(i,k)*   (su(i,k)-shat(i,k)) &
                        +md(i,k+1)* (sd(i,k+1)-shat(i,k+1)) &
                        -md(i,k)*   (sd(i,k)-shat(i,k)) &
                       )/dp(i,k)

         dqdt(i,k) = emc + &
                    (+mu(i,k+1)* (qu(i,k+1)-qhat(i,k+1)) &
                     -mu(i,k)*   (qu(i,k)-qhat(i,k)) &
                     +md(i,k+1)* (qd(i,k+1)-qhat(i,k+1)) &
                     -md(i,k)*   (qd(i,k)-qhat(i,k)) &
                    )/dp(i,k)

         dl(i,k) = du(i,k)*ql(i,k+1)

      end do
   end do


!DIR$ NOINTERCHANGE!
   do k = kbm,pver
      do i = il1g,il2g
         if (k == mx(i)) then
            dsdt(i,k) = (1._r8/dsubcld(i))* &
                        (-mu(i,k)* (su(i,k)-shat(i,k)) &
                         -md(i,k)* (sd(i,k)-shat(i,k)) &
                        )
            dqdt(i,k) = (1._r8/dsubcld(i))* &
                        (-mu(i,k)*(qu(i,k)-qhat(i,k)) &
                         -md(i,k)*(qd(i,k)-qhat(i,k)) &
                        )
         else if (k > mx(i)) then
            dsdt(i,k) = dsdt(i,k-1)
            dqdt(i,k) = dqdt(i,k-1)
         end if
      end do
   end do

   return
end subroutine q1q2_pjr

subroutine buoyan_dilute(lchnk   ,ncol    , &
                  q       ,t       ,p       ,z       ,pf      , &
                  tp      ,qstp    ,tl      ,rl      ,cape    , &
                  pblt    ,lcl     ,lel     ,lon     ,mx      , &
                  rd      ,grav    ,cp      ,msg     , &
                  tpert   )

























   implicit none




   integer, intent(in) :: lchnk                 
   integer, intent(in) :: ncol                  

   real(r8), intent(in) :: q(pcols,pver)        
   real(r8), intent(in) :: t(pcols,pver)        
   real(r8), intent(in) :: p(pcols,pver)        
   real(r8), intent(in) :: z(pcols,pver)        
   real(r8), intent(in) :: pf(pcols,pver+1)     
   real(r8), intent(in) :: pblt(pcols)          
   real(r8), intent(in) :: tpert(pcols)         




   real(r8), intent(out) :: tp(pcols,pver)       
   real(r8), intent(out) :: qstp(pcols,pver)     
   real(r8), intent(out) :: tl(pcols)            
   real(r8), intent(out) :: cape(pcols)          
   integer lcl(pcols)        
   integer lel(pcols)        
   integer lon(pcols)        
   integer mx(pcols)         



   real(r8) capeten(pcols,5)     
   real(r8) tv(pcols,pver)       
   real(r8) tpv(pcols,pver)      
   real(r8) buoy(pcols,pver)

   real(r8) a1(pcols)
   real(r8) a2(pcols)
   real(r8) estp(pcols)
   real(r8) pl(pcols)
   real(r8) plexp(pcols)
   real(r8) hmax(pcols)
   real(r8) hmn(pcols)
   real(r8) y(pcols)

   logical plge600(pcols)
   integer knt(pcols)
   integer lelten(pcols,5)

   real(r8) cp
   real(r8) e
   real(r8) grav

   integer i
   integer k
   integer msg
   integer n

   real(r8) rd
   real(r8) rl



   do n = 1,5
      do i = 1,ncol
         lelten(i,n) = pver
         capeten(i,n) = 0._r8
      end do
   end do

   do i = 1,ncol
      lon(i) = pver
      knt(i) = 0
      lel(i) = pver
      mx(i) = lon(i)
      cape(i) = 0._r8
      hmax(i) = 0._r8
   end do

   tp(:ncol,:) = t(:ncol,:)
   qstp(:ncol,:) = q(:ncol,:)



   tv(:ncol,:) = t(:ncol,:) *(1._r8+1.608_r8*q(:ncol,:))/ (1._r8+q(:ncol,:))
   tpv(:ncol,:) = tv(:ncol,:)
   buoy(:ncol,:) = 0._r8





   do k = pver,msg + 1,-1
      do i = 1,ncol
         hmn(i) = cp*t(i,k) + grav*z(i,k) + rl*q(i,k)
         if (k >= nint(pblt(i)) .and. k <= lon(i) .and. hmn(i) > hmax(i)) then
            hmax(i) = hmn(i)
            mx(i) = k
         end if
      end do
   end do






   do i = 1,ncol 
      lcl(i) = mx(i)
      tl(i) = t(i,mx(i))
      pl(i) = p(i,mx(i))
   end do








   call parcel_dilute(lchnk, ncol, msg, mx, p, t, q, tpert, tp, tpv, qstp, pl, tl, lcl)






   do i = 1,ncol
      plge600(i) = pl(i).ge.600._r8 
   end do




   do k = pver,msg + 1,-1
      do i=1,ncol
         if (k <= mx(i) .and. plge600(i)) then   
            tv(i,k) = t(i,k)* (1._r8+1.608_r8*q(i,k))/ (1._r8+q(i,k))
            buoy(i,k) = tpv(i,k) - tv(i,k) + tiedke_add  
         else
            qstp(i,k) = q(i,k)
            tp(i,k)   = t(i,k)            
            tpv(i,k)  = tv(i,k)
         endif
      end do
   end do










   do k = msg + 2,pver
      do i = 1,ncol
         if (k < lcl(i) .and. plge600(i)) then
            if (buoy(i,k+1) > 0. .and. buoy(i,k) <= 0._r8) then
               knt(i) = min(5,knt(i) + 1)
               lelten(i,knt(i)) = k
            end if
         end if
      end do
   end do



   do n = 1,5
      do k = msg + 1,pver
         do i = 1,ncol
            if (plge600(i) .and. k <= mx(i) .and. k > lelten(i,n)) then
               capeten(i,n) = capeten(i,n) + rd*buoy(i,k)*log(pf(i,k+1)/pf(i,k))
            end if
         end do
      end do
   end do





   do n = 1,5
      do i = 1,ncol
         if (capeten(i,n) > cape(i)) then
            cape(i) = capeten(i,n)
            lel(i) = lelten(i,n)
         end if
      end do
   end do



   do i = 1,ncol
      cape(i) = max(cape(i), 0._r8)
   end do

   return
end subroutine buoyan_dilute

subroutine parcel_dilute (lchnk, ncol, msg, klaunch, p, t, q, tpert, tp, tpv, qstp, pl, tl, lcl)






implicit none


integer, intent(in) :: lchnk
integer, intent(in) :: ncol
integer, intent(in) :: msg

integer, intent(in), dimension(pcols) :: klaunch(pcols)

real(r8), intent(in), dimension(pcols,pver) :: p
real(r8), intent(in), dimension(pcols,pver) :: t
real(r8), intent(in), dimension(pcols,pver) :: q
real(r8), intent(in), dimension(pcols) :: tpert 

real(r8), intent(inout), dimension(pcols,pver) :: tp    
real(r8), intent(inout), dimension(pcols,pver) :: qstp  
real(r8), intent(inout), dimension(pcols) :: tl         
real(r8), intent(inout), dimension(pcols) :: pl          

integer, intent(inout), dimension(pcols) :: lcl 

real(r8), intent(out), dimension(pcols,pver) :: tpv   










real(r8) tmix(pcols,pver)        
real(r8) qtmix(pcols,pver)       
real(r8) qsmix(pcols,pver)       
real(r8) smix(pcols,pver)        
real(r8) xsh2o(pcols,pver)       
real(r8) ds_xsh2o(pcols,pver)    
real(r8) ds_freeze(pcols,pver)   

real(r8) mp(pcols)    
real(r8) qtp(pcols)   
real(r8) sp(pcols)    

real(r8) sp0(pcols)    
real(r8) qtp0(pcols)   
real(r8) mp0(pcols)    

real(r8) lwmax      
real(r8) dmpdp      

real(r8) dmpdz      
real(r8) dpdz,dzdp  
real(r8) senv       
real(r8) qtenv      
real(r8) penv       
real(r8) tenv       
real(r8) new_s      
real(r8) new_q      
real(r8) dp         
real(r8) tfguess    
real(r8) tscool     

real(r8) qxsk, qxskp1        
real(r8) dsdp, dqtdp, dqxsdp 
real(r8) slcl,qtlcl,qslcl    

integer rcall       
integer nit_lheat     
integer i,k,ii   














nit_lheat = 2 
dmpdz=-1.e-3_r8        

lwmax = 1.e-3_r8    
tscool = 0.0_r8   

qtmix=0._r8
smix=0._r8

qtenv = 0._r8
senv = 0._r8
tenv = 0._r8
penv = 0._r8

qtp0 = 0._r8
sp0  = 0._r8
mp0 = 0._r8

qtp = 0._r8
sp = 0._r8
mp = 0._r8

new_q = 0._r8
new_s = 0._r8



do k = pver, msg+1, -1
   do i=1,ncol 



      if (k == klaunch(i)) then 
         qtp0(i) = q(i,k)   
         sp0(i)  = entropy(t(i,k),p(i,k),qtp0(i))  
         mp0(i)  = 1._r8       
         smix(i,k)  = sp0(i)
         qtmix(i,k) = qtp0(i)
         tfguess = t(i,k)
         rcall = 1
         call ientropy (rcall,i,lchnk,smix(i,k),p(i,k),qtmix(i,k),tmix(i,k),qsmix(i,k),tfguess)
      end if


      
      if (k < klaunch(i)) then 


         
         dp = (p(i,k)-p(i,k+1)) 
         qtenv = 0.5_r8*(q(i,k)+q(i,k+1))         
         tenv  = 0.5_r8*(t(i,k)+t(i,k+1)) 
         penv  = 0.5_r8*(p(i,k)+p(i,k+1))

         senv  = entropy(tenv,penv,qtenv)  



         dpdz = -(penv*grav)/(rgas*tenv) 
         dzdp = 1._r8/dpdz                  
         dmpdp = dmpdz*dzdp              





         sp(i)  = sp(i)  - dmpdp*dp*senv 
         qtp(i) = qtp(i) - dmpdp*dp*qtenv 
         mp(i)  = mp(i)  - dmpdp*dp
            


         smix(i,k)  = (sp0(i)  +  sp(i)) / (mp0(i) + mp(i))
         qtmix(i,k) = (qtp0(i) + qtp(i)) / (mp0(i) + mp(i))




         tfguess = tmix(i,k+1)
         rcall = 2
         call ientropy(rcall,i,lchnk,smix(i,k),p(i,k),qtmix(i,k),tmix(i,k),qsmix(i,k),tfguess)   





         if (qsmix(i,k) <= qtmix(i,k) .and. qsmix(i,k+1) > qtmix(i,k+1)) then
            lcl(i) = k
            qxsk   = qtmix(i,k) - qsmix(i,k)
            qxskp1 = qtmix(i,k+1) - qsmix(i,k+1)
            dqxsdp = (qxsk - qxskp1)/dp
            pl(i)  = p(i,k+1) - qxskp1/dqxsdp    
            dsdp   = (smix(i,k)  - smix(i,k+1))/dp
            dqtdp  = (qtmix(i,k) - qtmix(i,k+1))/dp
            slcl   = smix(i,k+1)  +  dsdp* (pl(i)-p(i,k+1))  
            qtlcl  = qtmix(i,k+1) +  dqtdp*(pl(i)-p(i,k+1))

            tfguess = tmix(i,k)
            rcall = 3
            call ientropy (rcall,i,lchnk,slcl,pl(i),qtlcl,tl(i),qslcl,tfguess)








         endif

      end if 

 
   end do 
end do 















 

xsh2o = 0._r8
ds_xsh2o = 0._r8
ds_freeze = 0._r8






do k = pver, msg+1, -1
   do i=1,ncol    
      

      
      if (k == klaunch(i)) then



         tp(i,k)    = tmix(i,k)
         qstp(i,k)  = q(i,k) 
         tpv(i,k)   =  (tp(i,k) + tpert(i)) * (1._r8+1.608_r8*qstp(i,k)) / (1._r8+qstp(i,k))
         
      end if

      if (k < klaunch(i)) then
            




         do ii=0,nit_lheat-1            



            xsh2o(i,k) = max (0._r8, qtmix(i,k) - qsmix(i,k) - lwmax)


                     
            ds_xsh2o(i,k) = ds_xsh2o(i,k+1) - cpliq * log (tmix(i,k)/tfreez) * max(0._r8,(xsh2o(i,k)-xsh2o(i,k+1)))



 
            if (tmix(i,k) <= tfreez+tscool .and. ds_freeze(i,k+1) == 0._r8) then 
               ds_freeze(i,k) = (latice/tmix(i,k)) * max(0._r8,qtmix(i,k)-qsmix(i,k)-xsh2o(i,k)) 
            end if
            
            if (tmix(i,k) <= tfreez+tscool .and. ds_freeze(i,k+1) /= 0._r8) then 
               ds_freeze(i,k) = ds_freeze(i,k+1)+(latice/tmix(i,k)) * max(0._r8,(qsmix(i,k+1)-qsmix(i,k)))
            end if
            


            new_s = smix(i,k) + ds_xsh2o(i,k) + ds_freeze(i,k) 



            new_q = qtmix(i,k) - xsh2o(i,k)



            tfguess = tmix(i,k)
            rcall =4
            call ientropy (rcall,i,lchnk,new_s, p(i,k), new_q, tmix(i,k), qsmix(i,k), tfguess)
            
         end do  




         tp(i,k)    = tmix(i,k)



         if (new_q > qsmix(i,k)) then  
            qstp(i,k) = qsmix(i,k)
         else                          
            qstp(i,k) = new_q
         end if

         tpv(i,k) = (tp(i,k)+tpert(i))* (1._r8+1.608_r8*qstp(i,k)) / (1._r8+ new_q) 

      end if 
      
   end do 
   
end do  


return
end subroutine parcel_dilute


real(r8) function entropy(TK,p,qtot)





     real(r8), intent(in) :: p,qtot,TK
     real(r8) :: qv,qsat,e,esat,L,eref,pref

pref = 1000.0_r8           
eref = 6.106_r8            

L = rl - (cpliq - cpwv)*(TK-tfreez)         



esat = c1*exp(c2*(TK-tfreez)/(c3+TK-tfreez))       
qsat=eps1*esat/(p-esat)                      

qv = min(qtot,qsat)                         
e = qv*p / (eps1 +qv)

entropy = (cpres + qtot*cpliq)*log( TK/tfreez) - rgas*log( (p-e)/pref ) + &
        L*qv/TK - qv*rh2o*log(qv/qsat)

return
end FUNCTION entropy



   SUBROUTINE ientropy (rcall,icol,lchnk,s,p,qt,T,qsat,Tfg)







     integer, intent(in) :: icol, lchnk, rcall
     real(r8), intent(in)  :: s, p, Tfg, qt
     real(r8), intent(out) :: qsat, T
     real(r8) :: qv,Ts,dTs,fs1,fs2,esat     
     real(r8) :: pref,eref,L,e
     real(r8) :: this_lat,this_lon
     integer :: LOOPMAX,i

LOOPMAX = 100                   


pref = 1000.0_r8           
eref = 6.106_r8           



Ts = Tfg                  

converge: do i=0, LOOPMAX

   L = rl - (cpliq - cpwv)*(Ts-tfreez) 

   esat = c1*exp(c2*(Ts-tfreez)/(c3+Ts-tfreez)) 
   qsat = eps1*esat/(p-esat)     
   qv = min(qt,qsat) 
   e = qv*p / (eps1 +qv)  
   fs1 = (cpres + qt*cpliq)*log( Ts/tfreez ) - rgas*log( (p-e)/pref ) + &
        L*qv/Ts - qv*rh2o*log(qv/qsat) - s
   
   L = rl - (cpliq - cpwv)*(Ts-1._r8-tfreez)         

   esat = c1*exp(c2*(Ts-1._r8-tfreez)/(c3+Ts-1._r8-tfreez))
   qsat = eps1*esat/(p-esat)  
   qv = min(qt,qsat) 
   e = qv*p / (eps1 +qv)
   fs2 = (cpres + qt*cpliq)*log( (Ts-1._r8)/tfreez ) - rgas*log( (p-e)/pref ) + &
        L*qv/(Ts-1._r8) - qv*rh2o*log(qv/qsat) - s 
   
   dTs = fs1/(fs2 - fs1)
   Ts  = Ts+dTs
   if (abs(dTs).lt.0.001_r8) exit converge
   if (i .eq. LOOPMAX - 1) then

      this_lat = 0.
      this_lon = 0.
      write(iulog,*) '*** ZM_CONV: IENTROPY: Failed and about to exit, info follows ****'
      call wrf_debug(1,iulog)
      write(iulog,100) 'ZM_CONV: IENTROPY. Details: call#,lchnk,icol= ',rcall,lchnk,icol, &
       ' lat: ',this_lat,' lon: ',this_lon, &
       ' P(mb)= ', p, ' Tfg(K)= ', Tfg, ' qt(g/kg) = ', 1000._r8*qt, &
       ' qsat(g/kg) = ', 1000._r8*qsat,', s(J/kg) = ',s
      call wrf_debug(1,iulog)
      write(iulog,*) '*** Please report this crash to Po-Lun.Ma@pnnl.gov ***'
      call wrf_debug(1,iulog)
      call endrun('**** ZM_CONV IENTROPY: Tmix did not converge ****')
   end if
enddo converge



esat = c1*exp(c2*(Ts-tfreez)/(c3+Ts-tfreez))
qsat=eps1*esat/(p-esat)

qv = min(qt,qsat)                             
T = Ts 

 100    format (A,I1,I4,I4,7(A,F6.2))

return
end SUBROUTINE ientropy

end module module_cu_camzm
