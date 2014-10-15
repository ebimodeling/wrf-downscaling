















module my_fncs_mod





   implicit none

   private
   public  :: NccnFNC,SxFNC,gamma,gser,gammln,gammp,cfg,gamminc,polysvp,qsat

   contains



 REAL FUNCTION NccnFNC(Win,Tin,Pin,CCNtype)







  IMPLICIT NONE


  real,    intent(in) :: Win, Tin, Pin
  integer, intent(in) :: CCNtype


  real :: T,p,x,y,a,b,c,d,e,f,g,h,T2,T3,T4,x2,x3,x4,p2

  x= log10(Win*100.);   x2= x*x;  x3= x2*x;  x4= x2*x2
  T= Tin - 273.15;      T2= T*T;  T3= T2*T;  T4= T2*T2
  p= Pin*0.01;          p2= p*p

  if (CCNtype==1) then  

     a= 1.47e-9*T4 -6.944e-8*T3 -9.933e-7*T2 +2.7278e-4*T -6.6853e-4
     b=-1.41e-8*T4 +6.662e-7*T3 +4.483e-6*T2 -2.0479e-3*T +4.0823e-2
     c= 5.12e-8*T4 -2.375e-6*T3 +4.268e-6*T2 +3.9681e-3*T -3.2356e-1
     d=-8.25e-8*T4 +3.629e-6*T3 -4.044e-5*T2 +2.1846e-3*T +9.1227e-1
     e= 5.02e-8*T4 -1.973e-6*T3 +3.944e-5*T2 -9.0734e-3*T +1.1256e0
     f= -1.424e-6*p2 +3.631e-3*p -1.986
     g= -0.0212*x4 +0.1765*x3 -0.3770*x2 -0.2200*x +1.0081
     h= 2.47e-6*T3 -3.654e-5*T2 +2.3327e-3*T +0.1938
     y= a*x4 + b*x3 + c*x2 + d*x + e + f*g*h
     NccnFNC= 10.**min(2.,max(0.,y)) *1.e6                

  else if (CCNtype==2) then  

     a= 0.
     b= 0.
     c=-2.112e-9*T4 +3.9836e-8*T3 +2.3703e-6*T2 -1.4542e-4*T -0.0698
     d=-4.210e-8*T4 +5.5745e-7*T3 +1.8460e-5*T2 +9.6078e-4*T +0.7120
     e= 1.434e-7*T4 -1.6455e-6*T3 -4.3334e-5*T2 -7.6720e-3*T +1.0056
     f= 1.340e-6*p2 -3.5114e-3*p  +1.9453
     g= 4.226e-3*x4 -5.6012e-3*x3 -8.7846e-2*x2 +2.7435e-2*x +0.9932
     h= 5.811e-9*T4 +1.5589e-7*T3 -3.8623e-5*T2 +1.4471e-3*T +0.1496
     y= a*x4 +b*x3 +c*x2 + d*x + e + (f*g*h)
     NccnFNC= 10.**max(0.,y) *1.e6

  else

    print*, '*** STOPPED in MODULE ### NccnFNC  *** '
    print*, '    Parameter CCNtype incorrectly specified'
    stop

  endif

 END FUNCTION NccnFNC


   real FUNCTION SxFNC(Win,Tin,Pin,Qsw,Qsi,CCNtype,WRT)








 IMPLICIT NONE


  integer, intent(IN) :: WRT
  integer, intent(IN) :: CCNtype
  real,    intent(IN) :: Win, Tin, Pin, Qsw, Qsi


  real   ::  Si,Sw,Qv,T,p,x,a,b,c,d,f,g,h,Pcorr,T2corr,T2,T3,T4,x2,x3,x4,p2
  real, parameter :: TRPL= 273.15

  x= log10(max(Win,1.e-20)*100.);   x2= x*x;  x3= x2*x;  x4= x2*x2
  T= Tin;                           T2= T*T;  T3= T2*T;  T4= T2*T2
  p= Pin*0.01;                      p2= p*p

  if (CCNtype==1) then  

     a= -5.109e-7*T4 -3.996e-5*T3 -1.066e-3*T2 -1.273e-2*T +0.0659
     b=  2.014e-6*T4 +1.583e-4*T3 +4.356e-3*T2 +4.943e-2*T -0.1538
     c= -2.037e-6*T4 -1.625e-4*T3 -4.541e-3*T2 -5.118e-2*T +0.1428
     d=  3.812e-7*T4 +3.065e-5*T3 +8.795e-4*T2 +9.440e-3*T +6.14e-3
     f= -2.012e-6*p2 + 4.1913e-3*p    - 1.785e0
     g=  2.832e-1*x3 -5.6990e-1*x2 +5.1105e-1*x -4.1747e-4
     h=  1.173e-6*T3 +3.2174e-5*T2 -6.8832e-4*T +6.7888e-2
     Pcorr= f*g*h
     T2corr= 0.9581-4.449e-3*T-2.016e-4*T2-3.307e-6*T3-1.725e-8*T4

  else if (CCNtype==2) then  

     a=  3.80e-5*T2 +1.65e-4*T +9.88e-2
     b= -7.38e-5*T2 -2.53e-3*T -3.23e-1
     c=  8.39e-5*T2 +3.96e-3*T +3.50e-1
     d= -1.88e-6*T2 -1.33e-3*T -3.73e-2
     f= -1.9761e-6*p2 + 4.1473e-3*p - 1.771e0
     g=  0.1539*x4 -0.5575*x3 +0.9262*x2 -0.3498*x -0.1293
     h=-8.035e-9*T4+3.162e-7*T3+1.029e-5*T2-5.931e-4*T+5.62e-2
     Pcorr= f*g*h
     T2corr= 0.98888-5.0525e-4*T-1.7598e-5*T2-8.3308e-8*T3

  else

    print*, '*** STOPPED in MODULE ### SxFNC  *** '
    print*, '    Parameter CCNtype incorrectly specified'
    stop

  endif

  Sw= (a*x3 + b*x2 +c*x + d) + Pcorr
  Sw= 1. + 0.01*Sw
  Qv= Qsw*Sw
  Si= Qv/Qsi
  Si= Si*T2corr
  if (WRT.eq.1) then
     SxFNC= Sw
  else
     SxFNC= Si
  endif
  if (Win.le.0.) SxFNC= 1.

 END function SxFNC


 real FUNCTION gamma(xx)



  IMPLICIT NONE


  real, intent(IN) :: xx


  integer  :: j
  real*8   :: ser,stp,tmp,x,y,cof(6),gammadp


  SAVE cof,stp
  DATA cof,stp/76.18009172947146d0,-86.50532032941677d0,               &
       24.01409824083091d0,-1.231739572450155d0,.1208650973866179d-2,  &
       -.5395239384953d-5,2.5066282746310005d0/
  x=dble(xx)
  y=x
  tmp=x+5.5d0
  tmp=(x+0.5d0)*log(tmp)-tmp
  ser=1.000000000190015d0

  do j=1,4

     y=y+1.d0
     ser=ser+cof(j)/y
  enddo
  gammadp=tmp+log(stp*ser/x)
  gammadp= exp(gammadp)




  gamma  = sngl(gammadp)




 END FUNCTION gamma


 SUBROUTINE gser(gamser,a,x,gln)






 implicit none

 integer :: itmax
 real    :: a,gamser,gln,x,eps
 parameter (itmax=100, eps=3.e-7)
 integer :: n
 real :: ap,de1,summ

 gln=gammln(a)
 if(x.le.0.)then
    if(x.lt.0.) call wrf_error_fatal3("<stdin>",221,&
'WARNING: x <0 in gser' )
    gamser=0.
    return
 endif
 ap=a
 summ=1./a
 de1=summ
 do n=1,itmax
    ap=ap+1.
    de1=de1*x/ap
    summ=summ+de1
    if(abs(de1).lt.abs(summ)*eps) goto 1
 enddo
 call wrf_error_fatal3("<stdin>",235,&
'Warning: a too large, itmax too small in gser')
1 gamser=summ*exp(-x+a*log(x)-gln)
 return

END SUBROUTINE gser


 real FUNCTION gammln(xx)




  IMPLICIT NONE


  real, intent(IN) :: xx


  integer  :: j
  real*8   :: ser,stp,tmp,x,y,cof(6)

  SAVE cof,stp
  DATA cof,stp/76.18009172947146d0,-86.50532032941677d0,               &
       24.01409824083091d0,-1.231739572450155d0,.1208650973866179d-2,  &
       -.5395239384953d-5,2.5066282746310005d0/
  x=dble(xx)
  y=x
  tmp=x+5.5d0
  tmp=(x+0.5d0)*log(tmp)-tmp
  ser=1.000000000190015d0
  do j=1,6   

     y=y+1.d0
     ser=ser+cof(j)/y
  enddo



  gammln= sngl( tmp+log(stp*ser/x)  )




 END FUNCTION gammln


 real FUNCTION gammp(a,x)





 implicit none

 real :: a,x,gammcf,gamser,gln

 if(x.lt.0..or.a.le.0.) call wrf_error_fatal3("<stdin>",292,&
'warning : bad arguments in gammq' )
 if(x.lt.a+1.)then
    call gser(gamser,a,x,gln)
    gammp=gamser
 else
    call cfg(gammcf,a,x,gln)
    gammp=1.-gammcf
 endif
 return

 END FUNCTION gammp


 SUBROUTINE cfg(gammcf,a,x,gln)








 implicit none

 integer :: i,itmax
 real    :: a,gammcf,gln,x,eps,fpmin
 real    :: an,b,c,d,de1,h
 parameter (itmax=100,eps=3.e-7)

 gln=gammln(a)
 b=x+1.-a
 c=1./fpmin
 d=1./b
 h=d
 do i= 1,itmax
   an=-i*(i-a)
   b=b+2.
   d=an*d+b
   if(abs(d).lt.fpmin)d=fpmin
   c=b+an/c
 if(abs(c).lt.fpmin) c=fpmin
   d=1./d
   de1=d*c
   h=h*de1
   if(abs(de1-1.).lt.eps) goto 1
 enddo
 call wrf_error_fatal3("<stdin>",339,&
'Warning: a too large, itmax too small in gcf')
1 gammcf=exp(-x+a*log(x)-gln)*h
 return

END SUBROUTINE cfg


 real FUNCTION gamminc(p,xmax)



 real :: p,xmax
 gamminc= gammp(p,xmax)*exp(gammln(p))

 end FUNCTION gamminc


 real function polysvp(T,TYPE)














      IMPLICIT NONE

      REAL DUM
      REAL T
      INTEGER TYPE

      real a0i,a1i,a2i,a3i,a4i,a5i,a6i,a7i,a8i
      data a0i,a1i,a2i,a3i,a4i,a5i,a6i,a7i,a8i /&
	6.11147274, 0.503160820, 0.188439774e-1, &
        0.420895665e-3, 0.615021634e-5,0.602588177e-7, &
        0.385852041e-9, 0.146898966e-11, 0.252751365e-14/


      real a0,a1,a2,a3,a4,a5,a6,a7,a8


      data a0,a1,a2,a3,a4,a5,a6,a7,a8 /&
	6.11239921, 0.443987641, 0.142986287e-1, &
        0.264847430e-3, 0.302950461e-5, 0.206739458e-7, &
        0.640689451e-10,-0.952447341e-13,-0.976195544e-15/
      real dt



      IF (TYPE.EQ.1) THEN






      dt = max(-80.,t-273.16)
      polysvp = a0i + dt*(a1i+dt*(a2i+dt*(a3i+dt*(a4i+dt*(a5i+dt*(a6i+dt*(a7i+a8i*dt)))))))
      polysvp = polysvp*100.

      END IF



      IF (TYPE.EQ.0) THEN

       dt = max(-80.,t-273.16)
       polysvp = a0 + dt*(a1+dt*(a2+dt*(a3+dt*(a4+dt*(a5+dt*(a6+dt*(a7+a8*dt)))))))
       polysvp = polysvp*100.







         END IF

 end function polysvp


 real function qsat(temp,pres,wtype)









  implicit none

 
  real, intent(in)    :: temp     
  real, intent(in)    :: pres     
  integer, intent(in) :: wtype    

 
  real :: tmp1

  tmp1 = polysvp(temp,wtype)       
  qsat = 0.622*tmp1/(pres-tmp1)

  end function qsat



end module my_fncs_mod



module my_sedi_mod





  implicit none

  private
  public :: compute_sublevels,sedi_wrapper,sedi_1D,count_columns

  contains


 SUBROUTINE compute_sublevels(ktop,kbot,kdir,nk,nk_skip,nk_sub,kskip,kfull,iint)








implicit none























 integer,                       intent(in)  :: ktop,kbot,kdir,nk,nk_skip,nk_sub
 integer, dimension(nk_skip),   intent(in)  :: kskip
 integer, dimension(nk_sub),    intent(out) :: kfull
 integer, dimension(nk_skip,3), intent(out) :: iint

 
 logical :: skip_this_one
 integer :: k1,k2,k3


 
 kfull = 0
 k3 = 1
 do k1=1,nk
    skip_this_one = .false.
    do k2 = 1,nk_skip
      if (k1==kskip(k2)) then     

         skip_this_one = .true.
         exit
      endif
    enddo
    if (.not. skip_this_one) then
       kfull(k3) = k1
       k3 = k3 + 1
    endif
 enddo

 
 do k1 = 1,nk_skip
    iint(k1,1) = kskip(k1)
    do k2 = 1,nk_sub
       if (kfull(k2)>kskip(k1)) exit
    enddo
    iint(k1,2) = kfull(k2-1)
    iint(k1,3) = kfull(k2)
 enddo

END SUBROUTINE compute_sublevels


 SUBROUTINE sedi_wrapper(QX,NX,cat,epsQ,epsQ_sedi,epsN,dmx,ni,nk_sub,VxMax,DxMax,dt,     &
                massFlux_bot,kdir,kbot,ktop_sedi,GRAV,nk_skip,kfull,iint,DE_sub,iDE_sub, &
                iDP_sub,DZ_sub,iDZ_sub,gamfact_sub,zheight,nk,DE,iDE,iDP,DZ,iDZ,gamfact, &
                kskip1,kount,afx_in,bfx_in,cmx_in,ckQx1_in,ckQx2_in,ckQx4_in,BX,epsB)








  use my_fncs_mod   

  implicit none


  real, dimension(:,:), intent(inout),optional :: BX
  real, dimension(:,:), intent(inout) :: QX,NX
  real, dimension(:),   intent(out)   :: massFlux_bot
  real, dimension(:,:), intent(in)    :: DE_sub,iDE_sub,iDP_sub,DZ_sub,iDZ_sub,          &
                                         gamfact_sub,zheight, DE,iDE,iDP,DZ,iDZ,gamfact
  real, intent(in)                    :: epsQ,epsQ_sedi,epsN,VxMax,dmx,DxMax,dt,GRAV
  real, intent(in), optional          :: afx_in,bfx_in,cmx_in,ckQx1_in,ckQx2_in,         &
                                         ckQx4_in,epsB
  integer, dimension(:), intent(in)   :: ktop_sedi,kfull
  integer, intent(in)                 :: ni,nk_sub,cat,kbot,kdir,nk_skip,nk,kskip1,kount
  integer, dimension(:,:), intent(in) :: iint


  real, dimension(:,:), allocatable   :: QX_sub,NX_sub,BX_sub
  real                                :: i_Zrun
  integer, dimension(size(QX,dim=1))  :: activeColumn,ktop
  integer                             :: counter
  integer                             :: status
  integer                             :: a,i,k,i_sub,k_sub
  logical                             :: sediOnFull,found_blank,found_Q

real :: tmp1,tmp2




















































   massFlux_bot = 0.

  
  
  
  
   ktop = ktop_sedi  
   call count_columns(QX,ni,epsQ_sedi,counter,activeColumn,kdir,kbot,ktop)


   DO a = 1,counter
      i= activeColumn(a)
     

     sediOnFull = .true. 





























         call sedi_1D(QX(i,:),NX(i,:),cat,DE(i,:),iDE(i,:),iDP(i,:),gamfact(i,:),epsQ,   &

             epsN,dmx,VxMax,DxMax,dt,DZ(i,:),iDZ(i,:),massFlux_bot(i),kdir,kbot,ktop(i), &
             GRAV,afx_in=afx_in,bfx_in=bfx_in,cmx_in=cmx_in,ckQx1_in=ckQx1_in,ckQx2_in=  &
             ckQx2_in,ckQx4_in=ckQx4_in)












































































   ENDDO  

 END SUBROUTINE sedi_wrapper


 SUBROUTINE sedi_1D(QX1d,NX1d,cat,DE1d,iDE1d,iDP1d,gamfact1d,epsQ,epsN,dmx,VxMax,DxMax,  &
                    dt,DZ1d,iDZ1d,massFlux_bot,kdir,kbot,ktop,GRAV,afx_in,bfx_in,cmx_in, &
                    ckQx1_in,ckQx2_in,ckQx4_in,BX1d,epsB)



















































  use my_fncs_mod   

  implicit none


  real, dimension(:),  intent(inout), optional :: BX1d
  real, dimension(:),  intent(inout) :: QX1d,NX1d
  real, dimension(:),  intent(in)    :: gamfact1d
  real,                intent(out)   :: massFlux_bot
  real, dimension(:),  intent(in)    :: DE1d,iDE1d,iDP1d,DZ1d,iDZ1d
  real,                intent(in)    :: epsQ,epsN,VxMax,dmx,DxMax,dt,GRAV
  real, optional,      intent(in)    :: afx_in,bfx_in,cmx_in,ckQx1_in,ckQx2_in,          &
                                        ckQx4_in,epsB
  integer,             intent(in)    :: cat,kbot,kdir
  integer,             intent(in)    :: ktop


  integer                            :: npassx
  real, dimension(size(QX1d,dim=1))  :: VVQ,VVN
  real                               :: dzMIN,dtx,VxMaxx
  logical                            :: firstPass,QxPresent,BX_present
  integer                            :: nnn,i,k,l,km1,kp1,idzmin,kk
  real                               :: VqMax,VnMax,iLAMx,iLAMxB0,tmp1,tmp2,tmp3,Dx,     &
                                        iDxMax,icmx,VincFact,ratio_Vn2Vq,zmax_Q,zmax_N,  &
                                        idmx
  real                               :: alpha_x,afx,bfx,cmx,ckQx1,ckQx2,ckQx4

  real, parameter :: thrd    = 1./3.
  real, parameter :: sxth    = 1./6.

  real, parameter :: CoMAX   = 0.8
  real, parameter :: PIov6   = 3.14159265*sxth


   BX_present = present(BX1d)

  
   if (.not. (cat==4 .and. BX_present)) then
      afx   = afx_in
      bfx   = bfx_in
      cmx   = cmx_in
      icmx  = 1./cmx
      ckQx1 = ckQx1_in
      ckQx2 = ckQx2_in
      ckQx4 = ckQx4_in
      ratio_Vn2Vq  = ckQx2/ckQx1
   endif

   massFlux_bot = 0.
   iDxMax = 1./DxMax
   idmx   = 1./dmx
   VVQ    = 0.
   VVN    = 0.
   VqMax  = 0.
   VnMax  = 0.
   VVQ(:) = 0.















      do k= kbot,ktop,kdir
         QxPresent =  (QX1d(k)>epsQ .and. NX1d(k)>epsN)
         if (QxPresent) VVQ(k)= VV_Q()
      enddo


   Vxmaxx= min( VxMax, maxval(VVQ(:)))
   if (kdir==1) then
      dzMIN = minval(DZ1d(ktop-kdir:kbot))  
   else
      dzMIN = minval(DZ1d(ktop:kbot+kdir))  
   endif
   npassx= max(1, nint( dt*Vxmaxx/(CoMAX*dzMIN) ))






   dtx   = dt/float(npassx)


   DO nnn= 1,npassx

      firstPass = (nnn==1)







































      do k= kbot,ktop,kdir
         QxPresent  = (QX1d(k)>epsQ .and. NX1d(k)>epsN)
         if (QxPresent) then
            if (firstPass) then     
               VVQ(k)= -VVQ(k)
            else
               VVQ(k)= -VV_Q()
            endif









            VVN(k) = VVQ(k)*ratio_Vn2Vq
            VqMax  = max(VxMAX,-VVQ(k))
            VnMax  = max(VxMAX,-VVN(k))
         else
            VVQ(k) = 0.
            VVN(k) = 0.
            VqMax  = 0.
            VnMax  = 0.
         endif
      enddo  



      
      massFlux_bot= massFlux_bot - VVQ(kbot)*DE1d(kbot)*QX1d(kbot)
     
     







    
      do k= kbot,ktop,kdir
         QX1d(k)= QX1d(k) + dtx*iDE1d(k)*(-DE1d(k+kdir)*QX1d(k+kdir)*VVQ(k+kdir) +       &
                            DE1d(k)*QX1d(k)*VVQ(k))*iDZ1d(k+kdir)
         NX1d(k)= NX1d(k) + dtx*(-NX1d(k+kdir)*VVN(k+kdir) + NX1d(k)*VVN(k))*iDZ1d(k+kdir)
         QX1d(k) = max( QX1d(k), 0.)
         NX1d(k) = max( NX1d(k), 0.)
      enddo
     
      if (BX_present) then
       do k= kbot,ktop,kdir
         BX1d(k)= BX1d(k) + dtx*iDE1d(k)*(-DE1d(k+kdir)*BX1d(k+kdir)*VVQ(k+kdir) +       &
                            DE1d(k)*BX1d(k)*VVQ(k))*iDZ1d(k+kdir)
         BX1d(k) = max( BX1d(k), 0.)
       enddo
      endif
     

      do k= kbot,ktop,kdir

        
        if (QX1d(k)>epsQ .and. NX1d(k)<epsN) then
           
           do kk = k+kdir,ktop,kdir
              
              
              if (NX1d(kk)>=epsN) exit
           enddo
          
          
          
           NX1d(k) = max(epsN,NX1d(kk))
        endif

        
        if (QX1d(k)>epsQ .and. NX1d(k)>epsN) then
           Dx= (DE1d(k)*QX1d(k)/(NX1d(k)*cmx))**idmx
           if (cat==1 .and. Dx>3.e-3) then
              NX1d(k)= NX1d(k)*max((1.+2.e4*(Dx-3.e-3)**2),(Dx*iDxMAX)**3)
           else
              NX1d(k)= NX1d(k)*(max(Dx,DxMAX)*iDxMAX)**dmx   
           endif
        endif

      enddo

   ENDDO  

  
  
   massFlux_bot= massFlux_bot/float(npassx)



   CONTAINS

   real function VV_Q()
   
      iLAMx   = ((QX1d(k)*DE1d(k)/NX1d(k))*ckQx4)**idmx
      iLAMxB0 = iLAMx**bfx
      VV_Q    = gamfact1d(k)*iLAMxB0*ckQx1
   end function VV_Q

   real function VV_Qg()
   

      iLAMxB0 = iLAMx**bfx
      VV_Qg   = gamfact1d(k)*iLAMxB0*ckQx1
   end function VV_Qg

 END SUBROUTINE sedi_1D


 SUBROUTINE count_columns(QX,ni,minQX,counter,activeColumn,kdir,kbot,ktop)

 
 
 
 
 
 

  implicit none


  real,    dimension(:,:),intent(in)   :: QX            
  real,    intent(in)                  :: minQX         
  integer, intent(in)                  :: ni            
  integer, intent(in)                  :: kbot          
  integer, intent(in)                  :: kdir          
  integer, dimension(:),  intent(inout):: ktop          
  integer,                intent(out)  :: counter       
  integer, dimension(:),  intent(out)  :: activeColumn  


  integer                              :: i
  integer, dimension(size(QX,dim=1))   :: k

   counter       = 0
   activeColumn  = 0

 

   do i=1,ni
      k(i)= ktop(i)
      do
         k(i)=k(i)-kdir               
         if (QX(i,k(i))>minQX) then
            counter=counter+1
            activeColumn(counter)=i
            ktop(i)= k(i)             
            exit
         else
            if (k(i)==kbot) then
               ktop(i) = kbot
               exit
            endif
         endif
      enddo
   enddo

 END SUBROUTINE count_columns


end module my_sedi_mod



module my_dmom_mod

  implicit none

  private
  public :: mp_milbrandt2mom_main

  contains



 SUBROUTINE mp_milbrandt2mom_main(WZ,T,Q,QC,QR,QI,QN,QG,QH,NC,NR,NY,NN,NG,NH,PS,          &
     sigma,RT_rn1,RT_rn2,RT_fr1,RT_fr2,RT_sn1,RT_sn2,RT_sn3,RT_pe1,RT_pe2,RT_peL,RT_snd,  &
     dt,NI,NK,J,KOUNT,CCNtype,precipDiag_ON,sedi_ON,warmphase_ON,autoconv_ON,icephase_ON, &
     snow_ON,Dm_c,Dm_r,Dm_i,Dm_s,Dm_g,Dm_h,ZET,ZEC,SS,nk_bottom)


  use my_fncs_mod
  use my_sedi_mod

    use module_model_constants, ONLY: CPD => cp, CPV => cpv, RGASD => r_d, RGASV => r_v, &
        EPS1 => EP_2, DELTA => EP_1, CAPPA => rcp, GRAV => g, CHLC => XLV, CHLF => XLF


  implicit none


  integer,               intent(in)    :: NI,NK,J,KOUNT,CCNtype
  real,                  intent(in)    :: dt
  real, dimension(:),    intent(in)    :: PS
  real, dimension(:),    intent(out)   :: RT_rn1,RT_rn2,RT_fr1,RT_fr2,RT_sn1,RT_sn2,     &
                                          RT_sn3,RT_pe1,RT_pe2,RT_peL,ZEC,RT_snd
  real, dimension(:,:),  intent(in)    :: WZ,sigma
  real, dimension(:,:),  intent(inout) :: T,Q,QC,QR,QI,QN,QG,QH,NC,NR,NY,NN,NG,NH
  real, dimension(:,:),  intent(out)   :: ZET,Dm_c,Dm_r,Dm_i,Dm_s,Dm_g,Dm_h
  real, dimension(:,:,:),intent(out)   :: SS
  logical,               intent(in)    :: precipDiag_ON,sedi_ON,icephase_ON,snow_ON,     &
                                          warmphase_ON,autoconv_ON,nk_BOTTOM





























































































































 
  logical :: log1,log2,log3,log4,doneK,rainPresent,calcDiag,CB_found,ML_found,      &
             SN_found
  logical, dimension(size(QC,dim=1),size(QC,dim=2)) :: activePoint
  integer, dimension(size(QC,dim=1)) :: ktop_sedi
  integer :: i,k,niter,ll,start,kskip_1,ktop,kbot,kdir

  real    :: tmp1,tmp2,tmp3,tmp4,tmp5,tmp6,tmp7,tmp8,tmp9,tmp10,                    &
       VDmax,NNUmax,X,D,DEL,QREVP,NuDEPSOR,NuCONTA,NuCONTB,NuCONTC,iMUkin,Ecg,Erg,  &
       NuCONT,GG,Na,Tcc,F1,F2,Kdiff,PSIa,Kn,source,sink,sour,ratio,qvs0,Kstoke,     &
       DELqvs,ft,esi,Si,Simax,Vq,Vn,Vz,LAMr,No_r_DM,No_i,No_s,No_g,No_h,D_sll,      &
       iABi,ABw,VENTr,VENTs,VENTg,VENTi,VENTh,Cdiff,Ka,MUdyn,MUkin,Ng_tail,         &
       gam,ScTHRD,Tc,mi,ff,Ec,Ntr,Dho,DMrain,Ech,DMice,DMsnow,DMgrpl,DMhail,        &
       ssat,Swmax,dey,Esh,Eii,Eis,Ess,Eig,Eih,FRAC,JJ,Dirg,Dirh,Dsrs,Dsrg,Dsrh,     &
       Dgrg,Dgrh,SIGc,L,TAU,DrAUT,DrINIT,Di,Ds,Dg,Dh,qFact,nFact,Ki,Rz,NgCNgh,      &
       vr0,vi0,vs0,vg0,vh0,Dc,Dr,QCLcs,QCLrs,QCLis,QCLcg,QCLrg,QCLig,NhCNgh,        &
       QCLch,QCLrh,QCLsh,QMLir,QMLsr,QMLgr,QMLhr,QCLih,QVDvg,QVDvh,QSHhr,           &
       QFZci,QNUvi,QVDvi,QCNis,QCNis1,QCNis2,QCLir,QCLri,QCNsg,QCLsr,QCNgh,         &
       QCLgr,QHwet,QVDvs,QFZrh,QIMsi,QIMgi,NMLhr,NVDvh,NCLir,NCLri,NCLrh,           &
       NCLch,NCLsr,NCLirg,NCLirh,NrFZrh,NhFZrh,NCLsrs,NCLsrg,NCLsrh,NCLgrg,         &
       NCLgrh,NVDvg,NMLgr,NiCNis,NsCNis,NVDvs,NMLsr,NCLsh,NCLss,NNUvi,NFZci,NVDvi,  &
       NCLis,NCLig,NCLih,NMLir,NCLrs,NCNsg,NCLcs,NCLcg,NIMsi,NIMgi,NCLgr,NCLrg,     &
       NSHhr,RCAUTR,RCACCR,CCACCR,CCSCOC,CCAUTR,CRSCOR,ALFx,des_pmlt,Ecs,des,ides,  &
       LAMx,iLAMx,iLAMxB0,Dx,ffx,iLAMc,iNC,iNR,iNY,iNN,iNG,iLAMs_D3,                &
       iLAMg,iLAMg2,iLAMgB0,iLAMgB1,iLAMgB2,iLAMh,iLAMhB0,iLAMhB1,iLAMhB2,iNH,      &
       iLAMi,iLAMi2,iLAMi3,iLAMi4,iLAMi5,iLAMiB0,iLAMiB1,iLAMiB2,iLAMr6,iLAMh2,     &
       iLAMs,iLAMs2,iLAMsB0,iLAMsB1,iLAMsB2,iLAMr,iLAMr2,iLAMr3,iLAMr4,iLAMr5,      &
       iLAMc2,iLAMc3,iLAMc4,iLAMc5,iLAMc6,iQC,iQR,iQI,iQN,iQG,iQH,iEih,iEsh,        &
       N_c,N_r,N_i,N_s,N_g,N_h,fluxV_i,fluxV_g,fluxV_s,rhos_mlt,fracLiq

 
  real, save :: idt,iMUc,cmr,cmi,cms,cmg,cmh,icmr,icmi,icmg,icms,icmh,idew,idei,    &
       ideh,ideg,GC1,imso,icexc9,cexr1,cexr2,cexr3,No_s_SM,No_r,idms,imgo,icexs2,   &
       cexr4,cexr5,cexr6,cexr9,icexr9,ckQr1,ckQr2,ckQr3,ckQi1,ckQi2,ckQi3,ckQi4,    &
       icexi9,ckQs1,ckQs2,cexs1,cexs2,ckQg1,ckQg2,ckQg4,ckQh1,ckQh2,ckQh4,GR37,dms, &
       LCP,LFP,LSP,ck5,ck6,PI2,PIov4,PIov6,CHLS,iCHLF,cxr,cxi,Gzr,Gzi,Gzs,Gzg,Gzh,  &
       N_c_SM,iGC1,GC2,GC3,GC4,GC5,iGC5,GC6,GC7,GC8,GC11,GC12,GC13,GC14,iGR34,mso,  &
       GC15,GR1,GR3,GR13,GR14,GR15,GR17,GR31,iGR31,GR32,GR33,GR34,GR35,GR36,GI4,    &
       GI6,GI20,GI21,GI22,GI31,GI32,GI33,GI34,GI35,iGI31,GI11,GI36,GI37,GI40,iGG34, &
       GS09,GS11,GS12,GS13,iGS20,GS31,iGS31,GS32,GS33,GS34,GS35,GS36,GS40,iGS40,    &
       GS50,GG09,GG11,GG12,GG13,GG31,iGG31,GG32,GG33,GG34,GG35,GG36,GG40,iGG99,GH09,&
       GH11,GH12,GH13,GH31,GH32,GH33,GH40,GR50,GG50,iGH34,GH50,iGH99,iGH31,iGS34,   &
       iGS20_D3,GS40_D3,cms_D3,eds,fds,rfact_FvFm


  real, parameter :: MUc      =  3.    
  real, parameter :: alpha_c  =  1.    
  real, parameter :: alpha_r  =  0.    
  real, parameter :: alpha_i  =  0.    
  real, parameter :: alpha_s  =  0.    
  real, parameter :: alpha_g  =  0.    
  real, parameter :: alpha_h  =  0.    
  real, parameter :: No_s_max =  1.e+8 
  real, parameter :: lamdas_min= 500.  

 
  real, parameter :: No_r_SM  =  1.e+7  
  real, parameter :: No_g_SM  =  4.e+6  
  real, parameter :: No_h_SM  =  1.e+5  
  
  
  
  
  
  
  
  
  
  
  

  
  real, parameter :: afr=  149.100,  bfr= 0.5000   
  real, parameter :: afi=   71.340,  bfi= 0.6635   
  real, parameter :: afs=   11.720,  bfs= 0.4100   
  real, parameter :: afg=   19.300,  bfg= 0.3700   
  real, parameter :: afh=  206.890,  bfh= 0.6384   
 
 
 

  real, parameter :: epsQ  = 1.e-14   
  real, parameter :: epsN  = 1.e-3    
  real, parameter :: epsQ2 = 1.e-6    
  real, parameter :: epsVIS= 1.       

  real, parameter :: iLAMmin1= 1.e-6  
  real, parameter :: iLAMmin2= 1.e-10 
  real, parameter :: eps   = 1.e-32
  real, parameter :: k1    = 0.001
  real, parameter :: k2    = 0.0005
  real, parameter :: k3    = 2.54
  real, parameter :: CPW   = 4218., CPI=2093.

  real, parameter :: deg   =  400., mgo= 1.6e-10
  real, parameter :: deh   =  900.
  real, parameter :: dei   =  500., mio=1.e-12, Nti0=1.e3
  real, parameter :: dew   = 1000.
  real, parameter :: desFix=  100.  
  real, parameter :: desMax=  500.
  real, parameter :: Dso   =  125.e-6  
  real, parameter :: dmr   = 3., dmi= 3., dmg= 3., dmh= 3.

  
  
  
  real, parameter :: DrMax=  5.e-3,   VrMax= 16.,   epsQr_sedi= 1.e-8
  real, parameter :: DiMax=  5.e-3,   ViMax=  2.,   epsQi_sedi= 1.e-10
  real, parameter :: DsMax=  5.e-3,   VsMax=  4.,   epsQs_sedi= 1.e-8
  real, parameter :: DgMax=  5.e-3,   VgMax=  6.,   epsQg_sedi= 1.e-8
  real, parameter :: DhMax= 80.e-3,   VhMax= 25.,   epsQh_sedi= 1.e-10

  real, parameter :: DEo     = 1.225              
  real, parameter :: thrd    = 1./3.
  real, parameter :: sixth   = 0.5*thrd
  real, parameter :: Ers     = 1., Eci= 1.        
  real, parameter :: Eri     = 1., Erh= 1.
  real, parameter :: Xdisp   = 0.25               
  real, parameter :: aa11    = 9.44e15, aa22= 5.78e3, Rh= 41.e-6
  real, parameter :: Avx     = 0.78, Bvx= 0.30    
  real, parameter :: Abigg   = 0.66, Bbigg= 100.  
  real, parameter :: fdielec     = 4.464          
  real, parameter :: zfact       = 1.e+18         
  real, parameter :: minZET      = -99.           
  real, parameter :: maxVIS      = 99.e+3         
  real, parameter :: Drshed      = 0.001          
  real, parameter :: SIGcTHRS    = 15.e-6         
  real, parameter :: KK1         = 3.03e3         
  real, parameter :: KK2         = 2.59e15        
  real, parameter :: Dhh         = 82.e-6         
  real, parameter :: zMax_sedi   = 20000.         
  real, parameter :: Dr_large    = 200.e-6        
  real, parameter :: Ds_large    = 200.e-6        
  real, parameter :: Dh_large    = 1.0e-2         
  real, parameter :: Dh_min      = 1.0e-3         
  real, parameter :: Dr_3cmpThrs = 2.5e-3         
  real, parameter :: w_CNgh      = 3.             
  real, parameter :: Ngh_crit    = 1.e+0          
  real, parameter :: Tc_FZrh     = -10.           
  real, parameter :: CNsgThres   = 1.0            
  real, parameter :: capFact_i   = 0.5            
  real, parameter :: capFact_s   = 0.5            
  real, parameter :: Fv_Dsmin    = 125.e-6        
  real, parameter :: Fv_Dsmax    = 0.008          
  real, parameter :: Ni_max      = 1.e+7          











  real, parameter :: TRPL     =.27316e+3          
  real, parameter :: TCDK     =.27315e+3          
  real, parameter :: RAUW     =.1e+4              

  real, parameter :: EPS2     =.3780199778986     


  real, parameter :: TGL      =.27316e+3          
  real, parameter :: CONSOL   =.1367e+4           

  real, parameter :: RAYT     =.637122e+7         
  real, parameter :: STEFAN   =.566948e-7         
  real, parameter :: PI       =.314159265359e+1   
  real, parameter :: OMEGA    =.7292e-4           
  real, parameter :: KNAMS    =.514791            
  real, parameter :: STLO     =.6628486583943e-3  
  real, parameter :: KARMAN   =.35                
  real, parameter :: RIC      =.2                 




  
  real, parameter :: LAMa0  = 6.6e-8     
  real, parameter :: T0     = 293.15     
  real, parameter :: p0     = 101325.    
  real, parameter :: Ra     = 1.e-6      
  real, parameter :: kBoltz = 1.381e-23  
  real, parameter :: KAPa   = 5.39e5     

 
  logical, parameter :: iceDep_ON     = .true.  
  logical, parameter :: grpl_ON       = .true.  
  logical, parameter :: hail_ON       = .true.  
  logical, parameter :: rainAccr_ON   = .true.  
  logical, parameter :: snowSpherical = .false. 
  integer, parameter :: primIceNucl   = 1       
  real,    parameter :: outfreq       =  60.    

  real, dimension(size(QC,dim=1),size(QC,dim=2)) :: DE,iDE,iDP,QSW,QSI,DZ,iDZ,zz,VQQ,    &
        gamfact,pres,zheight,QC_in,QR_in,NC_in,NR_in
  real, dimension(size(QC,dim=1))                :: fluxM_r,fluxM_i,fluxM_s,fluxM_g,     &
        fluxM_h,dum
  integer, dimension(size(QC,dim=1))             :: activeColumn
  integer                                        :: k_sub,nk_sub,nk_skip
  integer                                        :: status  
  integer, allocatable, dimension(:)             :: kfull,kskip
  integer, allocatable, dimension(:,:)           :: iint
  real, dimension(:,:), allocatable              :: DE_sub,iDE_sub,iDP_sub,pres_sub,     &
        DZ_sub,zheight_sub,iDZ_sub,gamfact_sub


  

  
  
  

  if (nk_BOTTOM) then

     ktop  = 1          
     kbot  = nk         
     kdir  = -1         
  else
   
     ktop  = nk         
     kbot  = 1          
     kdir  = 1          
  endif




     nk_skip = 0
     allocate ( kskip(nk_skip), STAT=status  )
     kskip   = 0







  do k= kbot,ktop,kdir
     pres(:,k)= PS(:)*sigma(:,k)               
     do i=1,ni
        QSW(i,k) = qsat(T(i,k),pres(i,k),0)    
        QSI(i,k) = qsat(T(i,k),pres(i,k),1)    
     enddo
  enddo

 
  DE  = pres/(RGASD*T)
  iDE = 1./DE

 
  NC = NC*DE
  NR = NR*DE
  NY = NY*DE
  NN = NN*DE
  NG = NG*DE
  NH = NH*DE

  
  
  
  
  
  
  SS= 0.

 
 
  calcDiag = .true.  











  if (.TRUE.) then

   PI2    = PI*2.
   PIov4  = 0.25*PI
   PIov6  = PI*sixth
   CHLS   = CHLC+CHLF  
   LCP    = CHLC/CPD
   LFP    = CHLF/CPD
   iCHLF  = 1./CHLF
   LSP    = LCP+LFP
   ck5    = 4098.170*LCP
   ck6    = 5806.485*LSP
   idt    = 1./dt
   imgo   = 1./mgo
   idew   = 1./dew
   idei   = 1./dei
   ideg   = 1./deg
   ideh   = 1./deh

   

   
   cmr    = PIov6*dew;  icmr= 1./cmr
   cmi    = 440.;       icmi= 1./cmi
   cmg    = PIov6*deg;  icmg= 1./cmg
   cmh    = PIov6*deh;  icmh= 1./cmh

   cms_D3 = PIov6*desFix 
   if (snowSpherical) then
      cms = cms_D3
      dms = 3.
   else

      cms = 0.1597;  dms = 2.078   
   endif
   icms   = 1./cms
   idms   = 1./dms
   mso    = cms*Dso**dms
   imso   = 1./mso
  
  
  
  
   eds    = cms/PIov6
   fds    = dms-3.
   if (fds/=-1. .and..not.snowSpherical) GS50= gamma(1.+fds+alpha_s)

   
   iMUc   =  1./MUc
   GC1    =  gamma(alpha_c+1.0)
   iGC1   = 1./GC1
   GC2    =  gamma(alpha_c+1.+3.0*iMUc)  
   GC3    =  gamma(alpha_c+1.+6.0*iMUc)  
   GC4    =  gamma(alpha_c+1.+9.0*iMUc)  
   GC11   =  gamma(1.0*iMUc+1.0+alpha_c)
   GC12   =  gamma(2.0*iMUc+1.0+alpha_c)
   GC5    =  gamma(1.0+alpha_c)
   iGC5   = 1./GC5
   GC6    =  gamma(1.0+alpha_c+1.0*iMUc)
   GC7    =  gamma(1.0+alpha_c+2.0*iMUc)
   GC8    =  gamma(1.0+alpha_c+3.0*iMUc)
   GC13   =  gamma(3.0*iMUc+1.0+alpha_c)
   GC14   =  gamma(4.0*iMUc+1.0+alpha_c)
   GC15   =  gamma(5.0*iMUc+1.0+alpha_c)
   icexc9 =  1./(GC2*iGC1*PIov6*dew)
  
   if     (CCNtype==1) then
      N_c_SM =  0.8e+8          
   elseif (CCNtype==2) then
      N_c_SM =  2.0e+8          
   elseif (CCNtype==3) then
      N_c_SM =  5.0e+8          
   else
      N_c_SM =  2.0e+8          
   endif

   
   cexr1  = 1.+alpha_r+dmr+bfr
   cexr2  = 1.+alpha_r+dmr
   GR17   = gamma(2.5+alpha_r+0.5*bfr)
   GR31   = gamma(1.+alpha_r)
   iGR31  = 1./GR31
   GR32   = gamma(2.+alpha_r)
   GR33   = gamma(3.+alpha_r)
   GR34   = gamma(4.+alpha_r)
   iGR34  = 1./GR34
   GR35   = gamma(5.+alpha_r)
   GR36   = gamma(6.+alpha_r)
   GR37   = gamma(7.+alpha_r)
   GR50   = (No_r_SM*GR31)**0.75  
   cexr5  = 2.+alpha_r
   cexr6  = 2.5+alpha_r+0.5*bfr
   cexr9  = cmr*GR34*iGR31;    icexr9= 1./cexr9
   cexr3  = 1.+bfr+alpha_r
   cexr4  = 1.+alpha_r
   ckQr1  = afr*gamma(1.+alpha_r+dmr+bfr)/gamma(1.+alpha_r+dmr)
   ckQr2  = afr*gamma(1.+alpha_r+bfr)*GR31
   ckQr3  = afr*gamma(7.+alpha_r+bfr)/GR37

   
   GI4    = gamma(alpha_i+dmi+bfi)
   GI6    = gamma(2.5+bfi*0.5+alpha_i)
   GI11   = gamma(1.+bfi+alpha_i)
   GI20   = gamma(0.+bfi+1.+alpha_i)
   GI21   = gamma(1.+bfi+1.+alpha_i)
   GI22   = gamma(2.+bfi+1.+alpha_i)
   GI31   = gamma(1.+alpha_i)
   iGI31  = 1./GI31
   GI32   = gamma(2.+alpha_i)
   GI33   = gamma(3.+alpha_i)
   GI34   = gamma(4.+alpha_i)
   GI35   = gamma(5.+alpha_i)
   GI36   = gamma(6.+alpha_i)
   GI40   = gamma(1.+alpha_i+dmi)
   icexi9 = 1./(cmi*gamma(1.+alpha_i+dmi)*iGI31)
   ckQi1  = afi*gamma(1.+alpha_i+dmi+bfi)/GI40
   ckQi2  = afi*GI11*iGI31
   ckQi4  = 1./(cmi*GI40*iGI31)

   
   cexs1  = 2.5+0.5*bfs+alpha_s
   cexs2  = 1.+alpha_s+dms
   icexs2 = 1./cexs2
   GS09   = gamma(2.5+bfs*0.5+alpha_s)
   GS11   = gamma(1.+bfs+alpha_s)
   GS12   = gamma(2.+bfs+alpha_s)
   GS13   = gamma(3.+bfs+alpha_s)
   GS31   = gamma(1.+alpha_s)
   iGS31  = 1./GS31
   GS32   = gamma(2.+alpha_s)
   GS33   = gamma(3.+alpha_s)
   GS34   = gamma(4.+alpha_s)
   iGS34  = 1./GS34
   GS35   = gamma(5.+alpha_s)
   GS36   = gamma(6.+alpha_s)
   GS40   = gamma(1.+alpha_s+dms)
   iGS40  = 1./GS40
   iGS20  = 1./(GS40*iGS31*cms)
   ckQs1  = afs*gamma(1.+alpha_s+dms+bfs)*iGS40
   ckQs2  = afs*GS11*iGS31
   GS40_D3 = gamma(1.+alpha_s+3.)
   iGS20_D3= 1./(GS40_D3*iGS31*cms_D3)
   rfact_FvFm= PIov6*icms*gamma(4.+bfs+alpha_s)/gamma(1.+dms+bfs+alpha_s)

   
   GG09   = gamma(2.5+0.5*bfg+alpha_g)
   GG11   = gamma(1.+bfg+alpha_g)
   GG12   = gamma(2.+bfg+alpha_g)
   GG13   = gamma(3.+bfg+alpha_g)
   GG31   = gamma(1.+alpha_g)
   iGG31  = 1./GG31
   GG32   = gamma(2.+alpha_g)
   GG33   = gamma(3.+alpha_g)
   GG34   = gamma(4.+alpha_g)
   iGG34  = 1./GG34
   GG35   = gamma(5.+alpha_g)
   GG36   = gamma(6.+alpha_g)
   GG40   = gamma(1.+alpha_g+dmg)
   iGG99  = 1./(GG40*iGG31*cmg)
   GG50   = (No_g_SM*GG31)**0.75     
   ckQg1  = afg*gamma(1.+alpha_g+dmg+bfg)/GG40
   ckQg2  = afg*GG11*iGG31
   ckQg4  = 1./(cmg*GG40*iGG31)

   
   GH09   = gamma(2.5+bfh*0.5+alpha_h)
   GH11   = gamma(1.+bfh+alpha_h)
   GH12   = gamma(2.+bfh+alpha_h)
   GH13   = gamma(3.+bfh+alpha_h)
   GH31   = gamma(1.+alpha_h)
   iGH31  = 1./GH31
   GH32   = gamma(2.+alpha_h)
   GH33   = gamma(3.+alpha_h)
   iGH34  = 1./gamma(4.+alpha_h)
   GH40   = gamma(1.+alpha_h+dmh)
   iGH99  = 1./(GH40*iGH31*cmh)
   GH50   = (No_h_SM*GH31)**0.75     
   ckQh1  = afh*gamma(1.+alpha_h+dmh+bfh)/GH40
   ckQh2  = afh*GH11*iGH31
   ckQh4  = 1./(cmh*GH40*iGH31)

  endif  





  do k= kbot,ktop,kdir
     do i= 1,ni

        tmp1 = QSW(i,k)/max(Q(i,k),1.e-20)   
        tmp2 = QSI(i,k)/max(Q(i,k),1.e-20)   
       
       
       
       
       

       
        if (QC(i,k)>epsQ .and. NC(i,k)<epsN) then
           NC(i,k) = N_c_SM
        elseif (QC(i,k)<=epsQ .or. (QC(i,k)<epsQ2 .and. tmp1<0.90)) then
           Q(i,k)  = Q(i,k) + QC(i,k)
           T(i,k)  = T(i,k) - LCP*QC(i,k)
           QC(i,k) = 0.
           NC(i,k) = 0.
        endif

       
        if (QR(i,k)>epsQ .and. NR(i,k)<epsN) then
           NR(i,k) = (No_r_SM*GR31)**(3./(4.+alpha_r))*(GR31*iGR34*DE(i,k)*QR(i,k)*      &
                     icmr)**((1.+alpha_r)/(4.+alpha_r))
        elseif (QR(i,k)<=epsQ .or. (QR(i,k)<epsQ2 .and. tmp1<0.90)) then
           Q(i,k)  = Q(i,k) + QR(i,k)
           T(i,k)  = T(i,k) - LCP*QR(i,k)
           QR(i,k) = 0.
           NR(i,k) = 0.
        endif

        
        if (QI(i,k)>epsQ .and. NY(i,k)<epsN) then
           NY(i,k) = N_Cooper(TRPL,T(i,k))
        elseif (QI(i,k)<=epsQ .or. (QI(i,k)<epsQ2 .and. tmp2<0.80)) then
           Q(i,k)  = Q(i,k) + QI(i,k)
           T(i,k)  = T(i,k) - LSP*QI(i,k)
           QI(i,k) = 0.
           NY(i,k) = 0.
        endif

       
        if (QN(i,k)>epsQ .and. NN(i,k)<epsN) then
           No_s    = Nos_Thompson(TRPL,T(i,k))
           NN(i,k) = (No_s*GS31)**(dms*icexs2)*(GS31*iGS40*icms*DE(i,k)*QN(i,k))**       &
                     ((1.+alpha_s)*icexs2)
        elseif (QN(i,k)<=epsQ .or. (QN(i,k)<epsQ2 .and. tmp2<0.80)) then
           Q(i,k)  = Q(i,k) + QN(i,k)
           T(i,k)  = T(i,k) - LSP*QN(i,k)
           QN(i,k) = 0.
           NN(i,k) = 0.
        endif

       
      if (QG(i,k)>epsQ .and. NG(i,k)<epsN) then
           NG(i,k) = (No_g_SM*GG31)**(3./(4.+alpha_g))*(GG31*iGG34*DE(i,k)*QG(i,k)*      &
                     icmg)**((1.+alpha_g)/(4.+alpha_g))
        elseif (QG(i,k)<=epsQ .or. (QG(i,k)<epsQ2 .and. tmp2<0.80)) then
           Q(i,k)  = Q(i,k) + QG(i,k)
           T(i,k)  = T(i,k) - LSP*QG(i,k)
           QG(i,k) = 0.
           NG(i,k) = 0.
        endif

       
        if (QH(i,k)>epsQ .and. NH(i,k)<epsN) then
           NH(i,k) = (No_h_SM*GH31)**(3./(4.+alpha_h))*(GH31*iGH34*DE(i,k)*QH(i,k)*      &
                     icmh)**((1.+alpha_h)/(4.+alpha_h))
        elseif (QH(i,k)<=epsQ .or. (QH(i,k)<epsQ2 .and. tmp2<0.80)) then
           Q(i,k)  = Q(i,k) + QH(i,k)
           T(i,k)  = T(i,k) - LSP*QH(i,k)
           QH(i,k) = 0.
           NH(i,k) = 0.
        endif

     enddo 
  enddo    




  QC_in = QC
  QR_in = QR
  NC_in = NC
  NR_in = NR


 
  do i= 1,ni
     gamfact(i,:)  = sqrt(DEo/(DE(i,:)))
  enddo

 
  iDP(:,kbot) = 1./(PS(:)-pres(:,kbot))
  do k = kbot+kdir,ktop,kdir
     iDP(:,k) = 1./(pres(:,k-kdir)-pres(:,k))
  enddo

 
 
  iDZ = DE*GRAV*iDP
  DZ  = 1./iDZ

 
  zheight(:,kbot)= DZ(:,kbot)
  zz(:,kbot)= 0.       
  do k = kbot+kdir,ktop,kdir
     zheight(:,k) = zheight(:,k-kdir) + DZ(:,k)
     zz(:,k)      = zz(:,k-kdir)      + DZ(:,k)
  enddo

 
 
  ktop_sedi= 0
  do i=1,ni
     do k= ktop,kbot,-kdir
       ktop_sedi(i)= k
       if (zheight(i,k)<zMax_sedi) exit
     enddo
  enddo

  

  
  
  

  
  
  


  activePoint = .false.
  DO k= ktop-kdir,kbot,-kdir
     DO i=1,ni
        log1= ((QI(i,k)+QG(i,k)+QN(i,k)+QH(i,k))<epsQ)     
        log2= ((QC(i,k)+QR(i,k))                  <epsQ)   
        log3= ((T(i,k)>TRPL) .and. log1)                   
        log4= log1.and.log2.and.(Q(i,k)<QSI(i,k))          
        if (.not.( log3 .or. log4 ) .and. icephase_ON) then
          activePoint(i,k)= .true.
        endif
     ENDDO
  ENDDO

    
    
    
    
    

  DO k= ktop-kdir,kbot,-kdir
    DO i= 1,ni
      IF (activePoint(i,k)) THEN

       Tc= T(i,k)-TRPL
       if (Tc<-120. .or. Tc>50.)   &
        print*, '***WARNING*** -- In MICROPHYSICS --  Ambient Temp.(C):',Tc

       Cdiff = (2.2157e-5+0.0155e-5*Tc)*1.e5/pres(i,k)
       MUdyn = 1.72e-5*(393./(T(i,k)+120.))*(T(i,k)/TRPL)**1.5 
       MUkin = MUdyn*iDE(i,k)
       iMUkin= 1./MUkin
       ScTHRD= (MUkin/Cdiff)**thrd       
       Ka    = 2.3971e-2 + 0.0078e-2*Tc                                   
       Kdiff = (9.1018e-11*T(i,k)*T(i,k)+8.8197e-8*T(i,k)-(1.0654e-5)) 
       gam   = gamfact(i,k)

      
       Eis   = min(0.05*exp(0.1*Tc),1.)     
       Eig   = min(0.01*exp(0.1*Tc),1.)     
       Eii   = 0.1*Eis
       Ess   = Eis;   Eih = Eig;   Esh = Eig
       iEih  = 1./Eih
       iEsh  = 1./Esh
       
       
       
       
       

       qvs0   = qsat(TRPL,pres(i,k),0)      
       DELqvs = qvs0-(Q(i,k))

    
       if (QC(i,k)>epsQ) then
          iQC   = 1./QC(i,k)
          iNC   = 1./NC(i,k)
          Dc     = Dm_x(DE(i,k),QC(i,k),iNC,icmr,thrd)

          iLAMc  = iLAMDA_x(DE(i,k),QC(i,k),iNC,icexc9,thrd)
          iLAMc2 = iLAMc *iLAMc
          iLAMc3 = iLAMc2*iLAMc
          iLAMc4 = iLAMc2*iLAMc2
          iLAMc5 = iLAMc3*iLAMc2
       else
          Dc     = 0.;   iLAMc3= 0.
          iLAMc  = 0.;   iLAMc4= 0.
          iLAMc2 = 0.;   iLAMc5= 0.
       endif

    
       if (QR(i,k)>epsQ) then
          iQR   = 1./QR(i,k)
          iNR   = 1./NR(i,k)
          Dr     = Dm_x(DE(i,k),QR(i,k),iNR,icmr,thrd)
          iLAMr  = max( iLAMmin1, iLAMDA_x(DE(i,k),QR(i,k),iNR,icexr9,thrd) )
          tmp1   = 1./iLAMr
          iLAMr2 = iLAMr**2
          iLAMr3 = iLAMr**3
          iLAMr4 = iLAMr**4
          iLAMr5 = iLAMr**5
          if (Dr>40.e-6) then
             vr0 = gamfact(i,k)*ckQr1*iLAMr**bfr
          else
             vr0 = 0.
          endif
       else
          iLAMr  = 0.;  Dr    = 0.;  vr0   = 0.
          iLAMr2 = 0.;  iLAMr3= 0.;  iLAMr4= 0.;  iLAMr5 = 0.
       endif

    
       if (QI(i,k)>epsQ) then
          iQI   = 1./QI(i,k)
          iNY   = 1./NY(i,k)
          iLAMi  = max( iLAMmin2, iLAMDA_x(DE(i,k),QI(i,k),iNY,icexi9,thrd) )
          iLAMi2 = iLAMi**2
          iLAMi3 = iLAMi**3
          iLAMi4 = iLAMi**4
          iLAMi5 = iLAMi**5
          iLAMiB0= iLAMi**(bfi)
          iLAMiB1= iLAMi**(bfi+1.)
          iLAMiB2= iLAMi**(bfi+2.)
          vi0    = gamfact(i,k)*ckQi1*iLAMiB0
          Di     = Dm_x(DE(i,k),QI(i,k),iNY,icmi,thrd)
       else
          iLAMi  = 0.;  vi0    = 0.;  Di     = 0.
          iLAMi2 = 0.;  iLAMi3 = 0.;  iLAMi4 = 0.;  iLAMi5= 0.
          iLAMiB0= 0.;  iLAMiB1= 0.;  iLAMiB2= 0.
       endif

    
       if (QN(i,k)>epsQ) then
          iQN   = 1./QN(i,k)
          iNN   = 1./NN(i,k)
          iLAMs  = max( iLAMmin2, iLAMDA_x(DE(i,k),QN(i,k),iNN,iGS20,idms) )
          iLAMs_D3= max(iLAMmin2, iLAMDA_x(DE(i,k),QN(i,k),iNN,iGS20_D3,thrd) )
          iLAMs2 = iLAMs**2
          iLAMsB0= iLAMs**(bfs)
          iLAMsB1= iLAMs**(bfs+1.)
          iLAMsB2= iLAMs**(bfs+2.)
          vs0    = gamfact(i,k)*ckQs1*iLAMsB0
          Ds     = min(DsMax, Dm_x(DE(i,k),QN(i,k),iNN,icms,idms))
          if (snowSpherical) then
             des = desFix
          else
             des = des_OF_Ds(Ds,desMax,eds,fds)
          endif
         
         
         
         
         
         
         
         
        
         No_s= NN(i,k)*iGS31/iLAMs_D3  
         VENTs= Avx*GS32*iLAMs_D3**2. + Bvx*ScTHRD*sqrt(gamfact(i,k)*afs*iMUkin)*GS09*   &
                iLAMs_D3**cexs1
       else
          iLAMs  = 0.;  vs0    = 0.;  Ds     = 0.;  iLAMs2= 0.
          iLAMsB0= 0.;  iLAMsB1= 0.;  iLAMsB1= 0.
          des    = desFix 
       endif
       ides  = 1./des


    
       if (QG(i,k)>epsQ) then
          iQG    = 1./QG(i,k)
          iNG    = 1./NG(i,k)
          iLAMg  = max( iLAMmin1, iLAMDA_x(DE(i,k),QG(i,k),iNG,iGG99,thrd) )
          iLAMg2 = iLAMg**2
          iLAMgB0= iLAMg**(bfg)
          iLAMgB1= iLAMg**(bfg+1.)
          iLAMgB2= iLAMg**(bfg+2.)
         
          No_g= NG(i,k)*iGG31/iLAMg     
          vg0    = gamfact(i,k)*ckQg1*iLAMgB0
          Dg     = Dm_x(DE(i,k),QG(i,k),iNG,icmg,thrd)
       else
          iLAMg  = 0.;  vg0    = 0.;  Dg     = 0.;  No_g   = 0.
          iLAMg2 = 0.;  iLAMgB0= 0.;  iLAMgB1= 0.;  iLAMgB1= 0.
       endif

    
       if (QH(i,k)>epsQ) then
          iQH    = 1./QH(i,k)
          iNH    = 1./NH(i,k)
          iLAMh  = max( iLAMmin1, iLAMDA_x(DE(i,k),QH(i,k),iNH,iGH99,thrd) )
          iLAMh2 = iLAMh**2
          iLAMhB0= iLAMh**(bfh)
          iLAMhB1= iLAMh**(bfh+1.)
          iLAMhB2= iLAMh**(bfh+2.)
          No_h= NH(i,k)*iGH31/iLAMh**(1.+alpha_h)
          vh0    = gamfact(i,k)*ckQh1*iLAMhB0
          Dh     = Dm_x(DE(i,k),QH(i,k),iNH,icmh,thrd)
       else
          iLAMh  = 0.;  vh0    = 0.;  Dh     = 0.;  No_h= 0.
          iLAMhB0= 0.;  iLAMhB1= 0.;  iLAMhB1= 0.
       endif


 

 
       QNUvi=0.;  QVDvi=0.;  QVDvs=0.;  QVDvg=0.;  QVDvh=0.
       QCLcs=0.;  QCLcg=0.;  QCLch=0.;  QFZci=0.;  QCLri=0.;   QMLsr=0.
       QCLrs=0.;  QCLrg=0.;  QMLgr=0.;  QCLrh=0.;  QMLhr=0.;   QFZrh=0.
       QMLir=0.;  QCLsr=0.;  QCLsh=0.;  QCLgr=0.;  QCNgh=0.
       QCNis=0.;  QCLir=0.;  QCLis=0.;  QCLih=0.
       QIMsi=0.;  QIMgi=0.;  QCNsg=0.;  QHwet=0.

       NCLcs= 0.; NCLcg=0.;  NCLch=0.;  NFZci=0.;  NMLhr=0.;   NhCNgh=0.
       NCLri= 0.; NCLrs=0.;  NCLrg=0.;  NCLrh=0.;  NMLsr=0.;   NMLgr=0.
       NMLir= 0.; NSHhr=0.;  NNUvi=0.;  NVDvi=0.;  NVDvh=0.;   QCLig=0.
       NCLir= 0.; NCLis=0.;  NCLig=0.;  NCLih=0.;  NIMsi=0.;   NIMgi=0.
       NiCNis=0.; NsCNis=0.; NVDvs=0.;  NCNsg=0.;  NCLgr=0.;   NCLsrh=0.
       NCLss= 0.; NCLsr=0.;  NCLsh=0.;  NCLsrs=0.; NCLgrg=0.;  NgCNgh=0.
       NVDvg= 0.; NCLirg=0.; NCLsrg=0.; NCLgrh=0.; NrFZrh=0.;  NhFZrh=0.
       NCLirh=0.

       Dirg=0.; Dirh=0.; Dsrs= 0.; Dsrg= 0.; Dsrh= 0.; Dgrg=0.; Dgrh=0.

   

       Si    = Q(i,k)/QSI(i,k)
       iABi  = 1./( CHLS*CHLS/(Ka*RGASV*T(i,k)**2) + 1./(DE(i,k)*(QSI(i,k))*Cdiff) )

           
           

           
       if (QN(i,k)>epsQ) then
          
          if (QC(i,k)>epsQ) then

            
             Ecs= min(Dc,30.e-6)*3.333e+4*sqrt(min(Ds,1.e-3)*1.e+3)
             QCLcs= dt*gam*afs*cmr*Ecs*PIov4*iDE(i,k)*(NC(i,k)*NN(i,k))*iGC5*iGS31*    &
                    (GC13*GS13*iLAMc3*iLAMsB2+2.*GC14*GS12*iLAMc4*iLAMsB1+GC15*GS11*     &
                    iLAMc5*iLAMsB0)

             NCLcs= dt*gam*afs*PIov4*Ecs*(NC(i,k)*NN(i,k))*iGC5*iGS31*(GC5*GS13*       &
                    iLAMsB2+2.*GC11*GS12*iLAMc*iLAMsB1+GC12*GS11*iLAMc2*iLAMsB0)

            
            
            

            
            
            
            
             if (.not. snowSpherical) then
                tmp1 = 0.6366      
                QCLcs= tmp1*QCLcs
                NCLcs= tmp1*NCLcs
             endif

             QCLcs= min(QCLcs, QC(i,k))
             NCLcs= min(NCLcs, NC(i,k))
          else
             QCLcs= 0.;   NCLcs= 0.
          endif

          
          if (QI(i,k)>epsQ) then
             tmp1= vs0-vi0
             tmp3= sqrt(tmp1*tmp1+0.04*vs0*vi0)

             QCLis= dt*cmi*iDE(i,k)*PI*6.*Eis*(NY(i,k)*NN(i,k))*tmp3*iGI31*iGS31*(0.5* &
                    iLAMs2*iLAMi3+2.*iLAMs*iLAMi4+5.*iLAMi5)

             NCLis= dt*PIov4*Eis*(NY(i,k)*NN(i,k))*GI31*GS31*tmp3*(GI33*GS31*iLAMi2+   &
                    2.*GI32*GS32*iLAMi*iLAMs+GI31*GS33*iLAMs2)

             QCLis= min(QCLis, (QI(i,k)))
             NCLis= min(QCLis*(NY(i,k)*iQI), NCLis)
          else
             QCLis= 0.;   NCLis= 0.
          endif

          
          NCLss= dt*0.93952*Ess*(DE(i,k)*(QN(i,k)))**((2.+bfs)*thrd)*(NN(i,k))**    &
                   ((4.-bfs)*thrd)
            
            
            
            
           NCLss= min(NCLss, 0.5*(NN(i,k)))

       else
          QCLcs= 0.;   NCLcs= 0.;   QCLis= 0.;   NCLis= 0.;  NCLss= 0.
       endif

       
       if (QG(i,k)>epsQ) then

          
          if (QC(i,k)>epsQ) then

            
             Kstoke = dew*vg0*Dc*Dc/(9.*MUdyn*Dg)
             Kstoke = max(1.5,min(10.,Kstoke))
             Ecg    = 0.55*log10(2.51*Kstoke)

             QCLcg= dt*gam*afg*cmr*Ecg*PIov4*iDE(i,k)*(NC(i,k)*NG(i,k))*iGC5*iGG31*    &
                    (GC13*GG13*iLAMc3*iLAMgB2+ 2.*GC14*GG12*iLAMc4*iLAMgB1+GC15*GG11*    &
                    iLAMc5*iLAMgB0)

             NCLcg= dt*gam*afg*PIov4*Ecg*(NC(i,k)*NG(i,k))*iGC5*iGG31*(GC5*GG13*       &
                    iLAMgB2+2.*GC11*GG12*iLAMc*iLAMgB1+GC12*GG11*iLAMc2*iLAMgB0)

             QCLcg= min(QCLcg, (QC(i,k)))
             NCLcg= min(NCLcg, (NC(i,k)))
          else
             QCLcg= 0.;   NCLcg= 0.
          endif

          
          if (QI(i,k)>epsQ) then
             tmp1= vg0-vi0
             tmp3= sqrt(tmp1*tmp1+0.04*vg0*vi0)

             QCLig= dt*cmi*iDE(i,k)*PI*6.*Eig*(NY(i,k)*NG(i,k))*tmp3*iGI31*iGG31*(0.5* &
                    iLAMg2*iLAMi3+2.*iLAMg*iLAMi4+5.*iLAMi5)
             NCLig= dt*PIov4*Eig*(NY(i,k)*NG(i,k))*GI31*GG31*tmp3*(GI33*GG31*iLAMi2+   &
                    2.*GI32*GG32*iLAMi*iLAMg+GI31*GG33*iLAMg2)

             QCLig= min(QCLig, (QI(i,k)))
             NCLig= min(QCLig*(NY(i,k)*iQI), NCLig)
          else
             QCLig= 0.;   NCLig= 0.
          endif

         
          VENTg= Avx*GG32*iLAMg*iLAMg+Bvx*ScTHRD*sqrt(gam*afg*iMUkin)*GG09*iLAMg**       &
                 (2.5+0.5*bfg+alpha_g)


          QVDvg = dt*iDE(i,k)*iABi*(PI2*(Si-1.)*No_g*VENTg)   
          
          VDmax = (Q(i,k)-QSI(i,k))/(1.+ck6*QSI(i,k)/(T(i,k)-7.66)**2)  
          if(Si>=1.) then
             QVDvg= min(max(QVDvg,0.),VDmax)
          else
             if (VDmax<0.) QVDvg= max(QVDvg,VDmax)
             
          endif
         
          NVDvg = 0.                            

       else
          QCLcg= 0.;   QCLrg= 0.;   QCLig= 0.
          NCLcg= 0.;   NCLrg= 0.;   NCLig= 0.
       endif

       
       if (QH(i,k)>epsQ) then

         
          if (QC(i,k)>epsQ) then
             Ech  = exp(-8.68e-7*Dc**(-1.6)*Dh)    

             QCLch= dt*gam*afh*cmr*Ech*PIov4*iDE(i,k)*(NC(i,k)*NH(i,k))*iGC5*iGH31*    &
                    (GC13*GH13*iLAMc3*iLAMhB2+2.*GC14*GH12*iLAMc4*iLAMhB1+GC15*GH11*     &
                    iLAMc5*iLAMhB0)

             NCLch= dt*gam*afh*PIov4*Ech*(NC(i,k)*NH(i,k))*iGC5*iGH31*(GC5*GH13*       &
                    iLAMhB2+2.*GC11*GH12*iLAMc*iLAMhB1+GC12*GH11*iLAMc2*iLAMhB0)

             QCLch= min(QCLch, QC(i,k))
             NCLch= min(NCLch, NC(i,k))
          else
             QCLch= 0.;   NCLch= 0.
          endif

          
          if (QR(i,k)>epsQ) then
             tmp1= vh0-vr0
             tmp3= sqrt(tmp1*tmp1+0.04*vh0*vr0)
             QCLrh= dt*cmr*Erh*PIov4*iDE(i,k)*(NH(i,k)*NR(i,k))*iGR31*iGH31*tmp3*      &
                    (GR36*GH31*iLAMr5+2.*GR35*GH32*iLAMr4*iLAMh+GR34*GH33*iLAMr3*iLAMh2)

             NCLrh= dt*PIov4*Erh*(NH(i,k)*NR(i,k))*iGR31*iGH31*tmp3*(GR33*GH31*        &
                    iLAMr2+2.*GR32*GH32*iLAMr*iLAMh+GR31*GH33*iLAMh2)

             QCLrh= min(QCLrh, QR(i,k))
             NCLrh= min(NCLrh, QCLrh*(NR(i,k)*iQR))
          else
             QCLrh= 0.;   NCLrh= 0.
          endif

          
          if (QI(i,k)>epsQ) then
             tmp1 = vh0-vi0
             tmp3 = sqrt(tmp1*tmp1+0.04*vh0*vi0)

             QCLih= dt*cmi*iDE(i,k)*PI*6.*Eih*(NY(i,k)*NH(i,k))*tmp3*iGI31*iGH31*(0.5* &
                    iLAMh2*iLAMi3+2.*iLAMh*iLAMi4+5.*iLAMi5)

             NCLih= dt*PIov4*Eih*(NY(i,k)*NH(i,k))*GI31*GH31*tmp3*(GI33*GH31*iLAMi2+   &
                    2.*GI32*GH32*iLAMi*iLAMh+GI31*GH33*iLAMh2)

             QCLih= min(QCLih, QI(i,k))
             NCLih= min(QCLih*(NY(i,k)*iQI), NCLih)
          else
             QCLih= 0.;   NCLih= 0.
          endif

          
          if (QN(i,k)>epsQ) then
             tmp1 = vh0-vs0
             tmp3 = sqrt(tmp1*tmp1+0.04*vh0*vs0)
             tmp4 = iLAMs2*iLAMs2

             if (snowSpherical) then
               
                QCLsh= dt*cms*iDE(i,k)*PI*6.*Esh*(NN(i,k)*NH(i,k))*tmp3*iGS31*iGH31*  &
                       (0.5*iLAMh2*iLAMs2*iLAMs+2.*iLAMh*tmp4+5.*tmp4*iLAMs)
             else
               
                QCLsh= dt*cms*iDE(i,k)*PI*0.25*Esh*tmp3*NN(i,k)*NH(i,k)*iGS31*iGH31*  &
                       (GH33*GS33*iLAMh**2.*iLAMs**2. + 2.*GH32*GS34*iLAMh*iLAMs**3. +  &
                        GH31*GS35*iLAMs**4.)
             endif

             NCLsh= dt*PIov4*Esh*(NN(i,k)*NH(i,k))*GS31*GH31*tmp3*(GS33*GH31*iLAMs2+  &
                    2.*GS32*GH32*iLAMs*iLAMh+GS31*GH33*iLAMh2)

             QCLsh= min(QCLsh, (QN(i,k)))
             NCLsh= min((NN(i,k)*iQN)*QCLsh, NCLsh, (NN(i,k)))
          else
             QCLsh= 0.;   NCLsh= 0.
          endif

         
          VENTh= Avx*GH32*iLAMh**(2.+alpha_h) + Bvx*ScTHRD*sqrt(gam*afh*iMUkin)*GH09*    &
                 iLAMh**(2.5+0.5*bfh+alpha_h)
          QHwet= max(0., dt*PI2*(DE(i,k)*CHLC*Cdiff*DELqvs-Ka*Tc)*No_h*iDE(i,k)/(CHLF+   &
                 CPW*Tc)*VENTh+(QCLih*iEih+QCLsh*iEsh)*(1.-CPI*Tc/(CHLF+CPW*Tc)) )

         


          QVDvh = dt*iDE(i,k)*iABi*(PI2*(Si-1.)*No_h*VENTh)   
          
          VDmax = (Q(i,k)-QSI(i,k))/(1.+ck6*(QSI(i,k))/(T(i,k)-7.66)**2)  
          if(Si>=1.) then
             QVDvh= min(max(QVDvh,0.),VDmax)
          else
             if (VDmax<0.) QVDvh= max(QVDvh,VDmax)  
          endif

          NVDvh= 0.                            

       else
          QCLch= 0.;   QCLrh= 0.;   QCLih= 0.;   QCLsh= 0.;   QHwet= 0.
          NCLch= 0.;   NCLrh= 0.;   NCLsh= 0.;   NCLih= 0.
       endif

       IF (T(i,k)>TRPL .and. warmphase_ON) THEN
          
          
          

          
          
          QMLir   = QI(i,k)  
          QI(i,k)= 0.
          NMLir   = NY(i,k)

          
          if (QN(i,k)>epsQ) then
             QMLsr= dt*(PI2*iDE(i,k)*iCHLF*No_s*VENTs*(Ka*Tc-CHLC*Cdiff*DELqvs) + CPW*   &
                    iCHLF*Tc*(QCLcs+QCLrs)*idt)
             QMLsr= min(max(QMLsr,0.), QN(i,k))
             NMLsr= NN(i,k)*iQN*QMLsr
          else
             QMLsr= 0.;   NMLsr= 0.
          endif

          
          if (QG(i,k)>epsQ) then
             QMLgr= dt*(PI2*iDE(i,k)*iCHLF*No_g*VENTg*(Ka*Tc-CHLC*Cdiff*DELqvs) + CPW*   &
                    iCHLF*Tc*(QCLcg+QCLrg)*idt)
             QMLgr= min(max(QMLgr,0.), QG(i,k))
             NMLgr= NG(i,k)*iQG*QMLgr
          else
             QMLgr= 0.;   NMLgr= 0.
          endif

          
          if (QH(i,k)>epsQ.and.Tc>5.) then
             VENTh= Avx*GH32*iLAMh**(2.+alpha_h) + Bvx*ScTHRD*sqrt(gam*afh*iMUkin)*GH09* &
                    iLAMh**(2.5+0.5*bfh+alpha_h)
             QMLhr= dt*(PI2*iDE(i,k)*iCHLF*No_h*VENTh*(Ka*Tc-CHLC*Cdiff*DELqvs) + CPW/   &
                    CHLF*Tc*(QCLch+QCLrh)*idt)
             QMLhr= min(max(QMLhr,0.), QH(i,k))
             NMLhr= NH(i,k)*iQH*QMLhr
             if(QCLrh>0.) NMLhr= NMLhr*0.1   
          else
             QMLhr= 0.;   NMLhr= 0.
          endif

         
          QNUvi= 0.;   QFZci= 0.;   QVDvi= 0.;   QVDvs= 0.
          QCLis= 0.;   QCNis1=0.;   QCNis2=0.;   QCLri= 0.
          QCNgh= 0.;   QIMsi= 0.;   QIMgi= 0.;   QCLir= 0.
          QCLrs= 0.;   QCLgr= 0.;   QCLrg= 0.;   QCNis= 0.
          QCNsg= 0.;   QCLsr= 0.

          NNUvi= 0.;   NFZci= 0.;   NCLgr= 0.;   NCLrg= 0.;   NgCNgh= 0.
          NCLis= 0.;   NVDvi= 0.;   NVDvs= 0.;   NCLri= 0.;   NCLsr= 0.
          NCNsg= 0.;   NhCNgh= 0.;  NiCNis=0.;   NsCNis=0.
          NIMsi= 0.;   NIMgi= 0.;   NCLir= 0.;   NCLrs= 0.

       ELSE  
             
             

          
          QMLir= 0.;   QMLsr= 0.;   QMLgr= 0.;   QMLhr= 0.
          NMLir= 0.;   NMLsr= 0.;   NMLgr= 0.;   NMLhr= 0.

          
          if (Tc<Tc_FZrh .and. QR(i,k)>epsQ .and. hail_ON) then
             
             
             
             
             NrFZrh= -dt*Bbigg*(exp(Abigg*Tc)-1.)*DE(i,k)*QR(i,k)*idew
             Rz= 1.  
           
           
           
           
           
           
             NhFZrh= Rz*NrFZrh
             QFZrh = NrFZrh*(QR(i,k)*iNR)
          else
             QFZrh= 0.;   NrFZrh= 0.;  NhFZrh= 0.
          endif

          
          
          
          
          if (QC(i,k)>epsQ) then
             tmp2  = Tc*Tc; tmp3= tmp2*Tc; tmp4= tmp2*tmp2
             JJ    = (10.**max(-20.,(-606.3952-52.6611*Tc-1.7439*tmp2-0.0265*tmp3-    &
                      1.536e-4*tmp4)))
             tmp1  = 1.e6*(DE(i,k)*(QC(i,k)*iNC)*icmr) 
             FRAC  = 1.-exp(-JJ*PIov6*tmp1*dt)
             if (Tc>-30.) FRAC= 0.
             if (Tc<-50.) FRAC= 1.
             QFZci= FRAC*QC(i,k)
             NFZci= FRAC*NC(i,k)
          else
             QFZci= 0.;   NFZci= 0.
          endif

          
          NNUvi= 0.;   QNUvi= 0.
          if (primIceNucl==1) then

             NuDEPSOR= 0.;   NuCONT= 0.
             Simax   = min(Si, SxFNC(WZ(i,k),Tc,pres(i,k),QSW(i,k),QSI(i,k),CCNtype,2))
             tmp1    = T(i,k)-7.66
             NNUmax  = max(0., DE(i,k)/mio*(Q(i,k)-QSI(i,k))/(1.+ck6*(QSI(i,k)/(tmp1*    &
                       tmp1))))
             
             if (Tc<-5. .and. Si>1.) then
                NuDEPSOR= max(0., 1.e3*exp(12.96*(Simax-1.)-0.639)-(NY(i,k))) 
             endif
             
             if (QC(i,k)>epsQ .and. Tc<-2.) then
                GG     =  1.*idew/(RGASV*(T(i,k))/((QSW(i,k)*pres(i,k))/EPS1)/          &
                            Cdiff+CHLC/Ka/(T(i,k))*(CHLC/RGASV/(T(i,k))-1.))  
                Swmax  =  SxFNC(WZ(i,k),Tc,pres(i,k),QSW(i,k),QSI(i,k),CCNtype,1)
                ssat   =  min((Q(i,k)/QSW(i,k)), Swmax) -1.
                Tcc    =  Tc + GG*ssat*CHLC/Kdiff                            
                Na     =  exp(4.11-0.262*Tcc)                                
                Kn     =  LAMa0*(T(i,k))*p0/(T0*pres(i,k)*Ra)               
                PSIa   =  -kBoltz*Tcc/(6.*pi*Ra*MUdyn)*(1.+Kn)               
                ft     =  0.4*(1.+1.45*Kn+0.4*Kn*exp(-1./Kn))*(Ka+2.5*Kn*KAPa)/          &
                         (1.+3.*Kn)/(2.*Ka+5.*KAPa*Kn+KAPa)                  
                Dc     =  (DE(i,k)*(QC(i,k)*iNC)*icmr)**thrd
                F1     =  PI2*Dc*Na*(NC(i,k))                               
                F2     =  Ka/pres(i,k)*(Tc-Tcc)                              
                NuCONTA= -F1*F2*RGASV*(T(i,k))/CHLC*iDE(i,k)                
                NuCONTB=  F1*F2*ft*iDE(i,k)                                  
                NuCONTC=  F1*PSIa                                            
                NuCONT =  max(0.,(NuCONTA+NuCONTB+NuCONTC)*dt)
             endif
             
             if (icephase_ON) then
                NNUvi= min(NNUmax, NuDEPSOR + NuCONT )
                QNUvi= mio*iDE(i,k)*NNUvi
                QNUvi= min(QNUvi,(Q(i,k)))
             endif

          elseif (primIceNucl==2) then
             if (Tc<-5. .and. Si>1.08) then 
                NNUvi= max(N_Cooper(TRPL,T(i,k))-NY(i,k),0.)
                QNUvi= min(mio*iDE(i,k)*NNUvi, Q(i,k))
             endif
         
         
         
         
          endif 



          IF (QI(i,k)>epsQ) THEN

             



             No_i  = NY(i,k)*iGI31/iLAMi    
             VENTi= Avx*GI32*iLAMi*iLAMi+Bvx*ScTHRD*sqrt(gam*afi*iMUkin)*GI6*iLAMi**     &
                    (2.5+0.5*bfi+alpha_i)
            

             QVDvi = dt*iDE(i,k)*capFact_i*iABi*(PI2*(Si-1.)*No_i*VENTi)

             
             VDmax = (Q(i,k)-QSI(i,k))/(1.+ck6*(QSI(i,k))/(T(i,k)-7.66)**2)
             if(Si>=1.) then
                QVDvi= min(max(QVDvi,0.),VDmax)
             else
                if (VDmax<0.) QVDvi= max(QVDvi,VDmax)
               
             endif
             if (.not. iceDep_ON) QVDvi= 0. 
             NVDvi= min(0., (NY(i,k)*iQI)*QVDvi) 

             
             if (QI(i,k)+QVDvi>epsQ .and. NY(i,k)+NVDvi>epsN) then
                tmp5   = iLAMi 
                tmp6   = No_i  
               
                tmp1   = QI(i,k) + QVDvi
                tmp2   = NY(i,k) + NVDvi
                iLAMi  = max( iLAMmin2, iLAMDA_x(DE(i,k),tmp1,1./tmp2,icexi9,thrd) )
                No_i   = tmp2*iGI31/iLAMi    
               
               
                tmp4   = exp(-Dso/iLAMi)
                NiCNis = No_i*iLAMi*tmp4
                NsCNis = NiCNis
                QCNis  = cmi*No_i*tmp4*(Dso**3*iLAMi + 3.*Dso**2*iLAMi**2 + 6.*Dso*      &
                         iLAMi**3 + 6.*iLAMi**4)
                iLAMi  = tmp5  
                No_i   = tmp6  
             endif

             if (.not.(snow_ON)) then  
                QCNis  = 0.
                NiCNis = 0.
                NsCNis = 0.
             endif

             
             if (QR(i,k)>epsQ .and. QI(i,k)>epsQ) then
                tmp1 = vr0-vi0
                tmp3 = sqrt(tmp1*tmp1+0.04*vr0*vi0)

                QCLir= dt*cmi*Eri*PIov4*iDE(i,k)*(NR(i,k)*NY(i,k))*iGI31*iGR31*tmp3*   &
                       (GI36*GR31*iLAMi5+2.*GI35*GR32*iLAMi4*iLAMr+GI34*GR33*iLAMi3*     &
                       iLAMr2)

                NCLri= dt*PIov4*Eri*(NR(i,k)*NY(i,k))*iGI31*iGR31*tmp3*(GI33*GR31*     &
                       iLAMi2+2.*GI32*GR32*iLAMi*iLAMr+GI31*GR33*iLAMr2)

                QCLri= dt*cmr*Eri*PIov4*iDE(i,k)*(NY(i,k)*NR(i,k))*iGR31*iGI31*tmp3*   &
                       (GR36*GI31 *iLAMr5+2.*GR35*GI32*iLAMr4*iLAMi+GR34*GI33*iLAMr3*    &
                       iLAMi2)

               
                NCLir= min(QCLir*(NY(i,k)*iQI), NCLri)
                QCLri= min(QCLri, (QR(i,k)));  QCLir= min(QCLir, (QI(i,k)))
                NCLri= min(NCLri, (NR(i,k)));  NCLir= min(NCLir, (NY(i,k)))

                
                tmp1= max(Di,Dr)
                dey= (dei*Di*Di*Di+dew*Dr*Dr*Dr)/(tmp1*tmp1*tmp1)
                if (dey>0.5*(deg+deh) .and. Dr>Dr_3cmpThrs .and. hail_ON) then
                   Dirg= 0.;  Dirh= 1.
                else
                   Dirg= 1.;  Dirh= 0.
                endif
                if (.not. grpl_ON) Dirg= 0.

             else
                QCLir= 0.;  NCLir= 0.;  QCLri= 0.
                NCLri= 0.;  Dirh = 0.;  Dirg= 0.
             endif

             
             ff= 0.
             if(Tc>=-8..and.Tc<=-5.) ff= 3.5e8*(Tc +8.)*thrd
             if(Tc> -5..and.Tc< -3.) ff= 3.5e8*(-3.-Tc)*0.5
             NIMsi= DE(i,k)*ff*QCLcs
             NIMgi= DE(i,k)*ff*QCLcg
             QIMsi= mio*iDE(i,k)*NIMsi
             QIMgi= mio*iDE(i,k)*NIMgi

          ELSE

             QVDvi= 0.;  QCNis= 0.
             QIMsi= 0.;  QIMgi= 0.;   QCLri= 0.;   QCLir= 0.
             NVDvi= 0.;  NCLir= 0.;   NIMsi= 0.
             NiCNis=0.;  NsCNis=0.;   NIMgi= 0.;   NCLri= 0.

          ENDIF
          
          
          
          IF (QN(i,k)>epsQ) THEN

            
             
             


             QVDvs = dt*iDE(i,k)*capFact_s*iABi*(PI2*(Si-1.)*No_s*VENTs - CHLS*CHLF/(Ka* &
                     RGASV*T(i,k)**2)*QCLcs*idt)

             
             VDmax = (Q(i,k)-QSI(i,k))/(1.+ck6*(QSI(i,k))/(T(i,k)-7.66)**2)
             if(Si>=1.) then
                QVDvs= min(max(QVDvs,0.),VDmax)
             else
                if (VDmax<0.) QVDvs= max(QVDvs,VDmax)
                
             endif
             NVDvs= -min(0.,(NN(i,k)*iQN)*QVDvs)  

             
             if (QCLcs>0. .and. QCLcs>CNsgThres*QVDvs .and. grpl_ON) then
                tmp1 = 100.  
                             
                             
                QCNsg = min( QN(i,k)+QCLcs, QCLcs*(tmp1*QCLcs/QN(i,k)) )
               
               
               
               
                NCNsg = DE(i,k)*QCNsg/(QN(i,k)+QCLcs)
             else
                QCNsg = 0.
                NCNsg = 0.
             endif

             
              if (QR(i,k)>epsQ .and. QN(i,k)>epsQ .and. Tc<-5.) then
                tmp1 = vs0-vr0
                tmp2 = sqrt(tmp1*tmp1+0.04*vs0*vr0)
                tmp6 = iLAMs2*iLAMs2*iLAMs

                QCLrs= dt*cmr*Ers*PIov4*iDE(i,k)*NN(i,k)*NR(i,k)*iGR31*iGS31*tmp2*     &
                       (GR36*GS31*iLAMr5+2.*GR35*GS32*iLAMr4*iLAMs+GR34*GS33*iLAMr3*     &
                       iLAMs2)

                NCLrs= dt*0.25e0*PI*Ers*(NN(i,k)*NR(i,k))*iGR31*iGS31*tmp2*(GR33*      &
                       GS31*iLAMr2+2.*GR32*GS32*iLAMr*iLAMs+GR31*GS33*iLAMs2)

                if (snowSpherical) then
                  
                   QCLsr= dt*cms*Ers*PIov4*iDE(i,k)*(NR(i,k)*NN(i,k))*iGS31*iGR31*     &
                          tmp2*(GS36*GR31*tmp6+2.*GS35*GR32*iLAMs2*iLAMs2*iLAMr+GS34*    &
                          GR33*iLAMs2*iLAMs*iLAMr2)
                else
                  
                   QCLsr= dt*cms*iDE(i,k)*PI*0.25*ERS*tmp2*NN(i,k)*NR(i,k)*iGS31*      &
                          iGR31*(GR33*GS33*iLAMr**2.*iLAMs**2. + 2.*GR32*GS34*iLAMr*     &
                          iLAMs**3. +GR31*GS35*iLAMs**4.)
                endif

               
                NCLsr= min(QCLsr*(NN(i,k)*iQN), NCLrs)
                QCLrs= min(QCLrs, QR(i,k));  QCLsr= min(QCLsr, QN(i,k))
                NCLrs= min(NCLrs, NR(i,k));  NCLsr= min(NCLsr, NN(i,k))

                
                Dsrs= 0.;   Dsrg= 0.;    Dsrh= 0.
                tmp1= max(Ds,Dr)
                tmp2= tmp1*tmp1*tmp1
                dey = (des*Ds*Ds*Ds + dew*Dr*Dr*Dr)/tmp2
                if (dey<=0.5*(des+deg)                        ) Dsrs= 1.  
                if (dey >0.5*(des+deg) .and. dey<0.5*(deg+deh)) Dsrg= 1.  
                if (dey>=0.5*(deg+deh)) then
                   Dsrh= 1.                                               
                   if (.not.hail_ON .or. Dr<Dr_3cmpThrs) then
                      Dsrg= 1.;   Dsrh= 0.                                
                   endif
                endif
                if (.not. grpl_ON) Dsrg=0.
             else
                QCLrs= 0.;   QCLsr= 0.;   NCLrs= 0.;   NCLsr= 0.
             endif

          ELSE

             QVDvs= 0.;  QCLcs= 0.;  QCNsg= 0.;  QCLsr= 0.;  QCLrs= 0.
             NVDvs= 0.;  NCLcs= 0.;  NCLsr= 0.;  NCLrs= 0.;  NCNsg= 0.

          ENDIF
          
          
          
          IF (QG(i,k)>epsQ) THEN

           
             if ( (QCLcg+QCLrg)>0. .and. hail_ON ) then


                D_sll = 2.*0.01*(exp(min(20.,-Tc/(1.1e4*DE(i,k)*(QC(i,k)+QR(i,k)) + 1.)))-1.)
                D_sll = min(1., max(0.0001,D_sll))    
                tmp1  = iLAMg 
                tmp2  = No_g  
               
                tmp3  = QG(i,k) + QCLcg + QCLrg
                iLAMg = exp(thrd*log(DE(i,k)*tmp3/(NG(i,k)*6*cmg)))
                No_g  = NG(i,k)/iLAMg
                tmp4  = exp(-D_sll/iLAMg)
                Ng_tail = No_g*iLAMg*tmp4
                if (Ng_tail > Ngh_crit) then
                   NgCNgh= min(NG(i,k), Ng_tail)
                   QCNgh = min(QG(i,k), cmg*No_g*tmp4*(D_sll**3*iLAMg + 3.*D_sll**2*    &
                                         iLAMg**2 + 6.*D_sll*iLAMg**3 + 6.*iLAMg**4) )
                   Rz= 1.
                   
                   
                   
                   NhCNgh = Rz*NgCNgh
                else
                   QCNgh  = 0.
                   NgCNgh = 0.
                   NhCNgh = 0.
                endif
                iLAMg = tmp1  
                No_g  = tmp2  
             endif

          
             if (QR(i,k)>epsQ) then
                tmp1 = vg0-vr0
                tmp2 = sqrt(tmp1*tmp1 + 0.04*vg0*vr0)
                tmp8 = iLAMg2*iLAMg      
                tmp9 = tmp8*iLAMg        
                tmp10= tmp9*iLAMg        

               
                Kstoke = dew*abs(vg0-vr0)*Dr*Dr/(9.*MUdyn*Dg)
                Kstoke = max(1.5,min(10.,Kstoke))
                Erg    = 0.55*log10(2.51*Kstoke)

                QCLrg= dt*cmr*Erg*PIov4*iDE(i,k)*(NG(i,k)*NR(i,k))*iGR31*iGG31*tmp2*   &
                       (GR36*GG31*iLAMr5+2.*GR35*GG32*iLAMr4*iLAMg+GR34*GG33*iLAMr3*     &
                       iLAMg2)

                NCLrg= dt*PIov4*Erg*(NG(i,k)*NR(i,k))*iGR31*iGG31*tmp2*(GR33*GG31*     &
                       iLAMr2+2.*GR32*GG32*iLAMr*iLAMg+GR31*GG33*iLAMg2)

                QCLgr= dt*cmg*Erg*PIov4*iDE(i,k)*(NR(i,k)*NG(i,k))*iGG31*iGR31*tmp2*   &
                       (GG36*GR31*tmp10+2.*GG35*GR32*tmp9*iLAMr+GG34*GR33*tmp8*iLAMr2)

               
                NCLgr= min(NCLrg, QCLgr*(NG(i,k)*iQG))
                QCLrg= min(QCLrg, QR(i,k));  QCLgr= min(QCLgr, QG(i,k))
                NCLrg= min(NCLrg, NR(i,k));  NCLgr= min(NCLgr, NG(i,k))

               
                tmp1= max(Dg,Dr)
                tmp2= tmp1*tmp1*tmp1
                dey = (deg*Dg*Dg*Dg + dew*Dr*Dr*Dr)/tmp2
                if (dey>0.5*(deg+deh) .and. Dr>Dr_3cmpThrs .and. hail_ON) then
                   Dgrg= 0.;  Dgrh= 1.
                else
                   Dgrg= 1.;  Dgrh= 0.
                endif
             else
                QCLgr= 0.;  QCLrg= 0.;  NCLgr= 0.;  NCLrg= 0.
             endif

          ELSE

             QCNgh= 0.;  QCLgr= 0.;  QCLrg= 0.;  NgCNgh= 0.
             NhCNgh= 0.; NCLgr= 0.;  NCLrg= 0.

          ENDIF
          
          
          
          IF (QH(i,k)>epsQ) THEN

            
             if (QHwet<(QCLch+QCLrh+QCLih+QCLsh) .and. Tc>-40.) then
                QCLih= min(QCLih*iEih, QI(i,k))  
                NCLih= min(NCLih*iEih, NY(i,k))  
                QCLsh= min(QCLsh*iEsh, QN(i,k))  
                NCLsh= min(NCLsh*iEsh, NN(i,k))  
                tmp3 = QCLrh
                QCLrh= QHwet-(QCLch+QCLih+QCLsh)  
                QSHhr= tmp3-QCLrh                 
                NSHhr= DE(i,k)*QSHhr/(cmr*Drshed*Drshed*Drshed)
             else
                NSHhr= 0.
             endif
          ELSE
             NSHhr= 0.
          ENDIF

       ENDIF  

     

     
       do niter= 1,2

          
          source= Q(i,k) +dim(-QVDvi,0.)+dim(-QVDvs,0.)+dim(-QVDvg,0.)+dim(-QVDvh,0.)
          sink  = QNUvi+dim(QVDvi,0.)+dim(QVDvs,0.)
          sour  = max(source,0.)
          if(sink>sour) then
             ratio= sour/sink
             QNUvi= ratio*QNUvi;   NNUvi= ratio*NNUvi
             if(QVDvi>0.) then
               QVDvi= ratio*QVDvi; NVDvi= ratio*NVDvi
             endif
             if(QVDvs>0.) then
               QVDvs=ratio*QVDvs;  NVDvs=ratio*NVDvs
             endif
             QVDvg= ratio*QVDvg;   NVDvg= ratio*NVDvg
             QVDvh= ratio*QVDvh;   NVDvh= ratio*NVDvh
          endif

          
          source= QC(i,k)
          sink  = QCLcs+QCLcg+QCLch+QFZci
          sour  = max(source,0.)
          if(sink>sour) then
             ratio= sour/sink
             QFZci= ratio*QFZci;   NFZci= ratio*NFZci
             QCLcs= ratio*QCLcs;   NCLcs= ratio*NCLcs
             QCLcg= ratio*QCLcg;   NCLcg= ratio*NCLcg
             QCLch= ratio*QCLch;   NCLch= ratio*NCLch
          endif

          
          source= QR(i,k)+QMLsr+QMLgr+QMLhr+QMLir
          sink  = QCLri+QCLrs+QCLrg+QCLrh+QFZrh
          sour  = max(source,0.)
          if(sink>sour) then
             ratio= sour/sink
             QCLrg= ratio*QCLrg;   QCLri= ratio*QCLri;   NCLri= ratio*NCLri
             QCLrs= ratio*QCLrs;   NCLrs= ratio*NCLrs;   QCLrg= ratio*QCLrg
             NCLrg= ratio*NCLrg;   QCLrh= ratio*QCLrh;   NCLrh= ratio*NCLrh
             QFZrh= ratio*QFZrh;   NrFZrh=ratio*NrFZrh;  NhFZrh=ratio*NhFZrh
             if (ratio==0.) then
                Dirg= 0.; Dirh= 0.; Dgrg= 0.; Dgrh= 0.
                Dsrs= 0.; Dsrg= 0.; Dsrh= 0.
              endif
          endif

          
          source= QI(i,k)+QNUvi+dim(QVDvi,0.)+QFZci
          sink  = QCNis+QCLir+dim(-QVDvi,0.)+QCLis+QCLig+QCLih+QMLir
          sour  = max(source,0.)
          if(sink>sour) then
             ratio= sour/sink
             QMLir= ratio*QMLir;    NMLir= ratio*NMLir
             if (QVDvi<0.) then
                QVDvi= ratio*QVDvi; NVDvi= ratio*NVDvi
             endif
             QCNis=  ratio*QCNis;   NiCNis= ratio*NiCNis;   NsCNis= ratio*NsCNis
             QCLir=  ratio*QCLir;   NCLir=  ratio*NCLir;    QCLig=  ratio*QCLig
             QCLis=  ratio*QCLis;   NCLis=  ratio*NCLis
             QCLih=  ratio*QCLih;   NCLih=  ratio*NCLih
             if (ratio==0.) then
                Dirg= 0.; Dirh= 0.
             endif
          endif

          
          source= QN(i,k)+QCNis+dim(QVDvs,0.)+QCLis+Dsrs*(QCLrs+QCLsr)+QCLcs
          sink  = dim(-QVDvs,0.)+QCNsg+QMLsr+QCLsr+QCLsh
          sour  = max(source,0.)
          if(sink>sour) then
             ratio= sour/sink
             if(QVDvs<=0.) then
                QVDvs= ratio*QVDvs;   NVDvs= ratio*NVDvs
             endif
             QCNsg= ratio*QCNsg;   NCNsg= ratio*NCNsg;   QMLsr= ratio*QMLsr
             NMLsr= ratio*NMLsr;   QCLsr= ratio*QCLsr;   NCLsr= ratio*NCLsr
             QCLsh= ratio*QCLsh;   NCLsh= ratio*NCLsh
             if (ratio==0.) then
                Dsrs= 0.; Dsrg= 0.; Dsrh= 0.
             endif
          endif

          
          source= QG(i,k)+QCNsg+dim(QVDvg,0.)+Dirg*(QCLri+QCLir)+Dgrg*(QCLrg+QCLgr)+     &
                  QCLcg+Dsrg*(QCLrs+QCLsr)+QCLig
          sink  = dim(-QVDvg,0.)+QMLgr+QCNgh+QCLgr
          sour  = max(source,0.)
          if(sink>sour) then
             ratio= sour/sink
             QVDvg= ratio*QVDvg;   NVDvg= ratio*NVDvg;   QMLgr = ratio*QMLgr
             NMLgr= ratio*NMLgr;   QCNgh= ratio*QCNgh;   NgCNgh= ratio*NgCNgh
             QCLgr= ratio*QCLgr;   NCLgr= ratio*NCLgr;   NhCNgh= ratio*NhCNgh
             if (ratio==0.) then
                Dgrg= 0.; Dgrh= 0.
             endif
          endif

          
          source= QH(i,k)+dim(QVDvh,0.)+QCLch+QCLrh+Dirh*(QCLri+QCLir)+QCLih+QCLsh+    &
                  Dsrh*(QCLrs+QCLsr)+QCNgh+Dgrh*(QCLrg+QCLgr)+QFZrh
          sink  = dim(-QVDvh,0.)+QMLhr
          sour  = max(source,0.)
          if(sink>sour) then
             ratio= sour/sink
             QVDvh= ratio*QVDvh;   NVDvh= ratio*NVDvh
             QMLhr= ratio*QMLhr;   NMLhr= ratio*NMLhr
          endif

       enddo
       

      
       NCLirg= 0.;  NCLirh= 0.;  NCLsrs= 0.;  NCLsrg= 0.
       NCLsrh= 0.;  NCLgrg= 0.;  NCLgrh= 0.

       if (QCLir+QCLri>0.) then
          tmp1  = max(Dr,Di)
          tmp2  = tmp1*tmp1*tmp1*PIov6
          NCLirg= Dirg*DE(i,k)*(QCLir+QCLri)/(deg*tmp2)
          NCLirh= Dirh*DE(i,k)*(QCLir+QCLri)/(deh*tmp2)
       endif

       if (QCLsr+QCLrs>0.) then
          tmp1  = max(Dr,Ds)
          tmp2  = tmp1*tmp1*tmp1*PIov6
          NCLsrs= Dsrs*DE(i,k)*(QCLsr+QCLrs)/(des*tmp2)
          NCLsrg= Dsrg*DE(i,k)*(QCLsr+QCLrs)/(deg*tmp2)
          NCLsrh= Dsrh*DE(i,k)*(QCLsr+QCLrs)/(deh*tmp2)
       endif

       if (QCLgr+QCLrg>0.) then
          tmp1  = max(Dr,Dg)
          tmp2  = tmp1*tmp1*tmp1*PIov6
          NCLgrg= Dgrg*DE(i,k)*(QCLgr+QCLrg)/(deg*tmp2)
          NCLgrh= Dgrh*DE(i,k)*(QCLgr+QCLrg)/(deh*tmp2)
       endif

       
       
       

      
      

       
       Q(i,k) = Q(i,k)  -QNUvi -QVDvi -QVDvs -QVDvg -QVDvh
       QC(i,k)= QC(i,k) -QCLcs -QCLcg -QCLch -QFZci
       QR(i,k)= QR(i,k) -QCLri +QMLsr -QCLrs -QCLrg +QMLgr -QCLrh +QMLhr -QFZrh +QMLir
       QI(i,k)= QI(i,k) +QNUvi +QVDvi +QFZci -QCNis -QCLir -QCLis -QCLig                 &
                        -QMLir -QCLih +QIMsi +QIMgi
       QG(i,k)= QG(i,k) +QCNsg +QVDvg +QCLcg -QCLgr-QMLgr -QCNgh -QIMgi +QCLig           &
                        +Dirg*(QCLri+QCLir) +Dgrg*(QCLrg+QCLgr) +Dsrg*(QCLrs+QCLsr)
       QN(i,k)= QN(i,k) +QCNis +QVDvs +QCLcs -QCNsg -QMLsr -QIMsi -QCLsr +QCLis -QCLsh   &
                        +Dsrs*(QCLrs+QCLsr)
       QH(i,k)= QH(i,k) +Dirh*(QCLri+QCLir) -QMLhr +QVDvh +QCLch +Dsrh*(QCLrs+QCLsr)     &
                        +QCLih +QCLsh +QFZrh +QCLrh +QCNgh +Dgrh*(QCLrg+QCLgr)

       
       NC(i,k)= NC(i,k) -NCLcs -NCLcg -NCLch -NFZci
       NR(i,k)= NR(i,k) -NCLri -NCLrs -NCLrg -NCLrh +NMLsr +NMLgr +NMLhr -NrFZrh +NMLir  &
                        +NSHhr
       NY(i,k)= NY(i,k) +NNUvi +NVDvi +NFZci -NCLir -NCLis -NCLig -NCLih -NMLir +NIMsi   &
                        +NIMgi -NiCNis
       NN(i,k)= NN(i,k) +NsCNis -NVDvs -NCNsg -NMLsr -NCLss -NCLsr -NCLsh +NCLsrs
       NG(i,k)= NG(i,k) +NCNsg -NCLgr -NVDvg -NMLgr +NCLirg +NCLsrg +NCLgrg -NgCNgh
       NH(i,k)= NH(i,k) +NhFZrh +NhCNgh -NMLhr -NVDvh +NCLirh +NCLsrh +NCLgrh

       T(i,k)= T(i,k)   +LFP*(QCLri+QCLcs+QCLrs+QFZci-QMLsr+QCLcg+QCLrg-QMLir-QMLgr      &
                        -QMLhr+QCLch+QCLrh+QFZrh) +LSP*(QNUvi+QVDvi+QVDvs+QVDvg+QVDvh)


         tmp1= pres(i,k)/(RGASD*T(i,k))    
         
         

        
         if (QC(i,k)>epsQ .and. NC(i,k)<epsN) then
             NC(i,k)= N_c_SM
         elseif (QC(i,k)<=epsQ) then
            Q(i,k)  = Q(i,k) + QC(i,k)
            T(i,k)  = T(i,k) - LCP*QC(i,k)
            QC(i,k) = 0.
            NC(i,k) = 0.
         endif

        
         if (QR(i,k)>epsQ .and. NR(i,k)<epsN) then
            NR(i,k)= (No_r_SM*GR31)**(3./(4.+alpha_r))*(GR31*iGR34*tmp1*QR(i,k)*         &
                     icmr)**((1.+alpha_r)/(4.+alpha_r))
         elseif (QR(i,k)<=epsQ) then
            Q(i,k)  = Q(i,k) + QR(i,k)
            T(i,k)  = T(i,k) - LCP*QR(i,k)
            QR(i,k) = 0.
            NR(i,k) = 0.
         endif

        
         if (QI(i,k)>epsQ .and. NY(i,k)<epsN) then
            NY(i,k)= N_Cooper(TRPL,T(i,k))
         elseif (QI(i,k)<=epsQ) then
            Q(i,k)  = Q(i,k) + QI(i,k)
            T(i,k)  = T(i,k) - LSP*QI(i,k)
            QI(i,k) = 0.
            NY(i,k) = 0.
         endif

        
         if (QN(i,k)>epsQ .and. NN(i,k)<epsN) then
            No_s= Nos_Thompson(TRPL,T(i,k))
            NN(i,k)= (No_s*GS31)**(dms*icexs2)*(GS31*iGS40*icms*tmp1*QN(i,k))**((1.+     &
                     alpha_s)*icexs2)
         elseif (QN(i,k)<=epsQ) then
            Q(i,k)  = Q(i,k) + QN(i,k)
            T(i,k)  = T(i,k) - LSP*QN(i,k)
            QN(i,k) = 0.
            NN(i,k) = 0.
         endif

        
         if (QG(i,k)>epsQ .and. NG(i,k)<epsN) then
            NG(i,k) = (No_g_SM*GG31)**(3./(4.+alpha_g))*(GG31*iGG34*tmp1*QG(i,k)*        &
                      icmg)**((1.+alpha_g)/(4.+alpha_g))
         elseif (QG(i,k)<=epsQ) then
            Q(i,k)  = Q(i,k) + QG(i,k)
            T(i,k)  = T(i,k) - LSP*QG(i,k)
            QG(i,k) = 0.
            NG(i,k) = 0.
         endif

        
         if (QH(i,k)>epsQ .and. NH(i,k)<epsN) then
            NH(i,k)= (No_h_SM*GH31)**(3./(4.+alpha_h))*(GH31*iGH34*tmp1*QH(i,k)*         &
                     icmh)**((1.+alpha_h)/(4.+alpha_h))
         elseif (QH(i,k)<=epsQ) then
            Q(i,k)  = Q(i,k) + QH(i,k)
            T(i,k)  = T(i,k) - LSP*QH(i,k)
            QH(i,k) = 0.
            NH(i,k) = 0.
         endif
         if (QH(i,k)>epsQ .and. NH(i,k)>epsN) then
          
            Dh = Dm_x(DE(i,k),QH(i,k),1./NH(i,k),icmh,thrd)
            if (Dh < Dh_min) then
               QG(i,k) = QG(i,k) + QH(i,k)
               NG(i,k) = NG(i,k) + NH(i,k)
               QH(i,k) = 0.
               NH(i,k) = 0.
            endif
         endif

         Q(i,k)= max(Q(i,k),0.)
         NY(i,k)= min(NY(i,k), Ni_max)

      ENDIF  
    ENDDO
  ENDDO

  
  
  

  
  
  
  
  
  
  

  

 IF (warmphase_ON) THEN

  DO k= ktop-kdir,kbot,-kdir
     DO i= 1,ni

        RCAUTR= 0.;  CCACCR= 0.;  Dc= 0.;  iLAMc= 0.;  L  = 0.
        RCACCR= 0.;  CCSCOC= 0.;  Dr= 0.;  iLAMr= 0.;  TAU= 0.
        CCAUTR= 0.;  CRSCOR= 0.;  SIGc= 0.;  DrINIT= 0.
        iLAMc3= 0.;  iLAMc6= 0.;  iLAMr3= 0.;  iLAMr6= 0.

        rainPresent= (QR_in(i,k)>epsQ .and. NR_in(i,k)>epsN)

        if (QC_in(i,k)>epsQ .and. NC_in(i,k)>epsN) then
           iLAMc = iLAMDA_x(DE(i,k),QC_in(i,k),1./NC_in(i,k),icexc9,thrd)
           iLAMc3= iLAMc*iLAMc*iLAMc
           iLAMc6= iLAMc3*iLAMc3
           Dc    = iLAMc*(GC2*iGC1)**thrd
           SIGc  = iLAMc*( GC3*iGC1- (GC2*iGC1)*(GC2*iGC1) )**sixth
           L     = 0.027*DE(i,k)*QC_in(i,k)*(6.25e18*SIGc*SIGc*SIGc*Dc-0.4)
           if (SIGc>SIGcTHRS) TAU= 3.7/(DE(i,k)*(QC_in(i,k))*(0.5e6*SIGc-7.5))
        endif

        if (rainPresent) then
           Dr = Dm_x(DE(i,k),QR_in(i,k),1./NR_in(i,k),icmr,thrd)
          
            if (Dr>3.e-3) then
              tmp1    = (Dr-3.e-3);  tmp2= (Dr/DrMAX); tmp3= tmp2*tmp2*tmp2
              NR_in(i,k)= NR_in(i,k)*max((1.+2.e4*tmp1*tmp1),tmp3)
              tmp1    = DE(i,k)*QR_in(i,k)*icmr
              Dr      = (tmp1/NR_in(i,k))**thrd
           endif
           iLAMr = iLAMDA_x(DE(i,k),QR(i,k),1./NR(i,k),icexr9,thrd)
           iLAMr3= iLAMr*iLAMr*iLAMr
           iLAMr6= iLAMr3*iLAMr3
        endif

        
        if (QC_in(i,k)>epsQ .and. SIGc>SIGcTHRS .and. autoconv_ON) then
           RCAUTR= min( max(L/TAU,0.), QC(i,k)*idt )
           DrINIT= max(83.e-6, 12.6e-4/(0.5e6*SIGc-3.5))  
           DrAUT = max(DrINIT, Dr)                     
           CCAUTR= RCAUTR*DE(i,k)/(cmr*DrAUT*DrAUT*DrAUT)

           
           
           
           
           
           
           
           
           
           
           

           
           CCSCOC= min(KK2*NC_in(i,k)*NC_in(i,k)*GC3*iGC1*iLAMc6, NC_in(i,k)*idt)  
        endif

        
        if (((QR_in(i,k))>1.2*max(L,0.)*iDE(i,k).or.Dr>max(5.e-6,DrINIT)).and.rainAccr_ON  &
             .and. rainPresent) then

           
           if (QC_in(i,k)>epsQ.and.L>0.) then
              if (Dr.ge.100.e-6) then
                 CCACCR = KK1*(NC_in(i,k)*NR_in(i,k))*(GC2*iGC1*iLAMc3+GR34*iGR31*iLAMr3)
                 RCACCR = cmr*iDE(i,k)*KK1*(NC_in(i,k)*NR_in(i,k))*iLAMc3*(GC3*iGC1*iLAMc3+  &
                          GC2*iGC1*GR34*iGR31*iLAMr3)
              else
                 CCACCR = KK2*(NC_in(i,k)*NR_in(i,k))*(GC3*iGC1*iLAMc6+GR37*iGR31*iLAMr6)




                 tmp1   = cmr*iDE(i,k)
                 tmp2   = KK2*(NC_in(i,k)*NR_in(i,k))*iLAMc3
                 RCACCR = tmp1 * tmp2
                 tmp1   = GC4*iGR31
                 tmp1   = (tmp1)*iLAMc6
                 tmp2   = GC2*iGC1
                 tmp2   = tmp2*GR37*iGR31
                 tmp2   = (tmp2)*iLAMr6
                 RCACCR = RCACCR * (tmp1 + tmp2)

              endif
              CCACCR = min(CCACCR,(NC(i,k))*idt)
              RCACCR = min(RCACCR,(QC(i,k))*idt)
            endif

         
           tmp1= NR_in(i,k)*NR_in(i,k)
           if (Dr.ge.100.e-6) then
              CRSCOR= KK1*tmp1*GR34*iGR31*iLAMr3                        
           else
              CRSCOR= KK2*tmp1*GR37*iGR31*iLAMr6                        
           endif
         
           Ec= 1.

           if (iLAMr > 300.e-6) Ec = 2.-exp(2300.*(iLAMr-300.e-6))  

           CRSCOR= min(Ec*CRSCOR,(0.5*NR(i,k))*idt) 

        endif  

        
        source= QC(i,k)
        sink  = (RCAUTR+RCACCR)*dt
        if (sink>source) then
           ratio = source/sink
           RCAUTR= ratio*RCAUTR
           RCACCR= ratio*RCACCR
           CCACCR= ratio*CCACCR
        endif

        
        QC(i,k)= max(0., QC(i,k)+(-RCAUTR-RCACCR)*dt )
        QR(i,k)= max(0., QR(i,k)+( RCAUTR+RCACCR)*dt )
        NC(i,k)= max(0., NC(i,k)+(-CCACCR-CCSCOC)*dt )
        NR(i,k)= max(0., NR(i,k)+( CCAUTR-CRSCOR)*dt )

        if (QR(i,k)>epsQ .and. NR(i,k)>epsN) then
           Dr = Dm_x(DE(i,k),QR(i,k),1./NR(i,k),icmr,thrd)
           if (Dr>3.e-3) then
              tmp1= (Dr-3.e-3);   tmp2= tmp1*tmp1
              tmp3= (Dr/DrMAX);   tmp4= tmp3*tmp3*tmp3
              NR(i,k)= NR(i,k)*(max((1.+2.e4*tmp2),tmp4))
           elseif (Dr<Dhh) then
           
              QC(i,k)= QC(i,k) + QR(i,k)
              NC(i,k)= NC(i,k) + NR(i,k)
              QR(i,k)= 0.;   NR(i,k)= 0.
           endif
        else
           QR(i,k)= 0.;   NR(i,k)= 0.
        endif  

  

        QSW(i,k) = qsat(T(i,k),pres(i,k),0)              
        ssat    = Q(i,k)/QSW(i,k)-1.                     
        X       = Q(i,k)-QSW(i,k)                        
        

        X       = X / ( 1.+ ((3.1484e6-2370.*T(i,k))**2 * QSW(i,k))/( (1005.*(1.+       & 
                        0.887*Q(i,k))) *461.5*T(i,k)**2 ) )
        X       = max(X, -QC(i,k))                       
        QC(i,k) = QC(i,k) + X
        Q(i,k)  = Q(i,k)  - X
        T(i,k)  = T(i,k)  + LCP*X
        if (ssat>0. .and. WZ(i,k)>0.001) then
           
           
           
          
           NC(i,k) = max(NC(i,k), NccnFNC(WZ(i,k),T(i,k),pres(i,k),CCNtype))
        else
           NC(i,k) = max(0., NC(i,k) + X*NC(i,k)/max(QC(i,k),epsQ) ) 
        endif
        
        if (QC(i,k)>epsQ .and. NC(i,k)<epsN) NC(i,k)= N_c_SM


       

       
        QSW(i,k) = qsat(T(i,k),pres(i,k),0)              
        if (Q(i,k)<QSW(i,k) .and. QR(i,k)>epsQ) then

           ssat     = Q(i,k)/QSW(i,k)-1.
           Tc      = T(i,k)-TRPL
           Cdiff   = max(1.62e-5, (2.2157e-5 + 0.0155e-5*Tc)) *1.e5/pres(i,k)
          
           MUdyn   = max(1.51e-5, (1.7153e-5 + 0.0050e-5*Tc))
           Ka      = max(2.07e-2, (2.3971e-2 + 0.0078e-2*Tc))
           MUkin   = MUdyn*iDE(i,k)
           iMUkin  = 1./MUkin
           ScTHRD  = (MUkin/Cdiff)**thrd
           X       = QSW(i,k) - Q(i,k)                      
           

           X       = X / ( 1.+ ((3.1484e6-2370.*T(i,k))**2 * QSW(i,k))/( (1005.*(1.+     &  
                     0.887*Q(i,k))) *461.5*T(i,k)**2 ) )
           DE(i,k)  = pres(i,k)/(RGASD*T(i,k))        
           iDE(i,k) = 1./DE(i,k)
           gam      = sqrt(DEo*iDE(i,k))
           iLAMr    = iLAMDA_x(DE(i,k),QR(i,k),1./NR(i,k),icexr9,thrd)
           LAMr     = 1./iLAMr
          
          
           No_r     = sngl(dble(NR(i,k))*dble(LAMr)**dble(1.+alpha_r))*iGR31
          
           VENTr    = Avx*GR32*iLAMr**cexr5 + Bvx*ScTHRD*sqrt(gam*afr*iMUkin)*GR17*iLAMr**cexr6
           ABw      = CHLC**2/(Ka*RGASV*T(i,k)**2)+1./(DE(i,k)*(QSW(i,k))*Cdiff)
           QREVP    = min( QR(i,k), -dt*(iDE(i,k)*PI2*ssat*No_r*VENTr/ABw) )
           tmp1     = QR(i,k)   
           T(i,k)   = T(i,k)  - LCP*QREVP
           Q(i,k)   = Q(i,k)  + QREVP
           QR(i,k)  = QR(i,k) - QREVP
           NR(i,k)  = max(0., NR(i,k) - QREVP*NR(i,k)/tmp1)
          
           if (QR(i,k)<epsQ .or. NR(i,k)<epsN)  then
              Q(i,k)  = Q(i,k) + QR(i,k)
              T(i,k)  = T(i,k) - QR(i,k)*LCP
              QR(i,k) = 0.
              NR(i,k) = 0.
           endif

         endif

       
        Tc = T(i,k) - TRPL
        if (QC(i,k)>epsQ .and. Tc<-30. .and. icephase_ON) then

         







         
           if (Tc<-35.) then
              FRAC = 1.
           else
              FRAC = 0.
           endif
          
           QFZci   = FRAC*QC(i,k)
           NFZci   = FRAC*NC(i,k)
           QC(i,k) = QC(i,k) - QFZci
           NC(i,k) = NC(i,k) - NFZci
           QI(i,k) = QI(i,k) + QFZci
           NY(i,k) = NY(i,k) + NFZci
           T(i,k)  = T(i,k)  + LFP*QFZci

           if (QC(i,k)>epsQ .and. NC(i,k)<epsN) then
               NC(i,k)= N_c_SM
           elseif (QC(i,k)<=epsQ) then
              Q(i,k)  = Q(i,k) + QC(i,k)
              T(i,k)  = T(i,k) - LCP*QC(i,k)
              QC(i,k) = 0.
              NC(i,k) = 0.
           endif

           if (QI(i,k)>epsQ .and. NY(i,k)<epsN) then
              NY(i,k)= N_Cooper(TRPL,T(i,k))
           elseif (QI(i,k)<=epsQ) then
              Q(i,k)  = Q(i,k) + QI(i,k)
              T(i,k)  = T(i,k) - LSP*QI(i,k)
              QI(i,k) = 0.
              NY(i,k) = 0.
           endif

        endif



     ENDDO
  ENDDO    

 ENDIF  

  
  
  

  
  
  

 IF (sedi_ON) THEN

   fluxM_r= 0.;  fluxM_i= 0.;  fluxM_s= 0.;  fluxM_g= 0.;  fluxM_h= 0.
   RT_rn1 = 0.;  RT_rn2 = 0.;  RT_fr1 = 0.;  RT_fr2 = 0.;  RT_sn1 = 0.
   RT_sn2 = 0.;  RT_sn3 = 0.;  RT_pe1 = 0.;  RT_pe2 = 0.;  RT_peL = 0.

    call sedi_wrapper(QR,NR,1,epsQ,epsQr_sedi,epsN,dmr,ni,nk_sub,VrMax,DrMax,dt,fluxM_r, &
                      kdir,kbot,ktop_sedi,GRAV,nk_skip,kfull,iint,DE_sub,iDE_sub,iDP_sub,&
                      DZ_sub,iDZ_sub,gamfact_sub,zheight,nk,DE,iDE,iDP,DZ,iDZ,gamfact,   &
                      kskip_1,kount,afr,bfr,cmr,ckQr1,ckQr2,icexr9)

    call sedi_wrapper(QI,NY,2,epsQ,epsQi_sedi,epsN,dmi,ni,nk_sub,ViMax,DiMax,dt,fluxM_i, &
                      kdir,kbot,ktop_sedi,GRAV,nk_skip,kfull,iint,DE_sub,iDE_sub,iDP_sub,&
                      DZ_sub,iDZ_sub,gamfact_sub,zheight,nk,DE,iDE,iDP,DZ,iDZ,gamfact,   &
                      kskip_1,kount,afi,bfi,cmi,ckQi1,ckQi2,ckQi4)

    call sedi_wrapper(QN,NN,3,epsQ,epsQs_sedi,epsN,dms,ni,nk_sub,VsMax,DsMax,dt,fluxM_s, &
                      kdir,kbot,ktop_sedi,GRAV,nk_skip,kfull,iint,DE_sub,iDE_sub,iDP_sub,&
                      DZ_sub,iDZ_sub,gamfact_sub,zheight,nk,DE,iDE,iDP,DZ,iDZ,gamfact,   &
                      kskip_1,kount,afs,bfs,cms,ckQs1,ckQs2,iGS20)

    call sedi_wrapper(QG,NG,4,epsQ,epsQg_sedi,epsN,dmg,ni,nk_sub,VgMax,DgMax,dt,fluxM_g, &
                      kdir,kbot,ktop_sedi,GRAV,nk_skip,kfull,iint,DE_sub,iDE_sub,iDP_sub,&
                      DZ_sub,iDZ_sub,gamfact_sub,zheight,nk,DE,iDE,iDP,DZ,iDZ,gamfact,   &
                      kskip_1,kount,afg,bfg,cmg,ckQg1,ckQg2,ckQg4)

    call sedi_wrapper(QH,NH,5,epsQ,epsQh_sedi,epsN,dmh,ni,nk_sub,VhMax,DhMax,dt,fluxM_h, &
                      kdir,kbot,ktop_sedi,GRAV,nk_skip,kfull,iint,DE_sub,iDE_sub,iDP_sub,&
                      DZ_sub,iDZ_sub,gamfact_sub,zheight,nk,DE,iDE,iDP,DZ,iDZ,gamfact,   &
                      kskip_1,kount,afh,bfh,cmh,ckQh1,ckQh2,ckQh4)


   do k= ktop,kbot,-kdir
      do i= 1,ni

       
         if (QN(i,k)>epsQ .and. NN(i,k)>epsN) then

         
            iLAMs  = max( iLAMmin2, iLAMDA_x(DE(i,k),QN(i,k), 1./NN(i,k),iGS20,idms) )
            tmp1   = min(NN(i,k)/iLAMs,No_s_max)                 
            NN(i,k)= tmp1**(dms/(1.+dms))*(iGS20*DE(i,k)*QN(i,k))**(1./(1.+dms)) 

         
            iLAMs  = max( iLAMmin2, iLAMDA_x(DE(i,k),QN(i,k),1./NN(i,k),iGS20,idms) )
            tmp2   = 1./iLAMs                                   
           
           
           
            tmp4   = 0.6*lamdas_min
            tmp5   = 2.*tmp4
            tmp3   = tmp2 + tmp4*(max(0.,tmp5-tmp2)/tmp5)**2.   
            tmp3   = max(tmp3, lamdas_min)                      
            NN(i,k)= NN(i,k)*(tmp3*iLAMs)**dms                  
         endif

      enddo 
   enddo 


  
  
   RT_rn1 = fluxM_r *idew
   RT_sn1 = fluxM_i *idew
   RT_sn2 = fluxM_s *idew
   RT_sn3 = fluxM_g *idew
   RT_pe1 = fluxM_h *idew


  
  

  
  
  
  
  
  

   do i= 1,ni

      fluxV_i= fluxM_i(i)*idei
      fluxV_g= fluxM_g(i)*ideg
     
         
         
         
         
         
      if (QN(i,nk)>epsQ .and. NN(i,nk)>epsN .and. fluxM_s(i)>0.) then
         tmp1= 1./iLAMDA_x(DE(i,nk),QN(i,nk),1./NN(i,nk),iGS20,idms) 
         fluxV_s= fluxM_s(i)*rfact_FvFm*tmp1**(dms-3.)
      else
         fluxV_s=0.
      endif

     
      tmp1= fluxV_i + fluxV_g + fluxV_s

     
     
     
     
      tmp2= QR(i,nk) + QI(i,nk) + QN(i,nk) + QG(i,nk)
      if (T(i,nk)>TRPL .and. tmp2>epsQ) then
         fracLiq= QR(i,nk)/tmp2
      else
         fracLiq= 0.
      endif

     
      tmp3= RT_sn1(i) + RT_sn2(i) + RT_sn3(i)      
      RT_snd(i)= (1.-fracLiq)*tmp1 + fracLiq*tmp3  
      
      

   enddo  


 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 



   IF (precipDiag_ON) THEN
      DO i= 1,ni

         DE(i,kbot)= pres(i,kbot)/(RGASD*T(i,kbot))

      
         N_r= NR(i,nk)
         if (QR(i,kbot)>epsQ .and. N_r>epsN) then
            Dm_r(i,kbot)= (DE(i,nk)*icmr*QR(i,kbot)/N_r)**thrd
            if (Dm_r(i,kbot)>Dr_large) then  
               RT_rn2(i)= RT_rn1(i);   RT_rn1(i)= 0.
            endif
         endif

      
         if (T(i,nk)<TRPL) then
            RT_fr1(i)= RT_rn1(i);   RT_rn1(i)= 0.
            RT_fr2(i)= RT_rn2(i);   RT_rn2(i)= 0.
         endif

      
         if (T(i,nk)>(TRPL+5.0)) then
         
         
         
            RT_pe2(i)= RT_pe1(i);   RT_pe1(i)= 0.
         endif

      
         if (QH(i,kbot)>epsQ) then
            N_h= NH(i,kbot)
            Dm_h(i,kbot)= Dm_x(DE(i,kbot),QH(i,kbot),1./N_h,icmh,thrd)
            if (DM_h(i,kbot)>Dh_large) RT_peL(i)= RT_pe2(i)
            
         endif

      ENDDO
   ENDIF  
 
 

 ENDIF  

 where (Q<0.) Q= 0.

 
 
 


 
 
 

 
 

  IF (calcDiag) THEN

   
     ZEC= minZET
     cxr=            icmr*icmr   
     cxi= 1./fdielec*icmr*icmr   
     Gzr= (6.+alpha_r)*(5.+alpha_r)*(4.+alpha_r)/((3.+alpha_r)*(2.+alpha_r)*(1.+alpha_r))
     Gzi= (6.+alpha_i)*(5.+alpha_i)*(4.+alpha_i)/((3.+alpha_i)*(2.+alpha_i)*(1.+alpha_i))
     if (snowSpherical) then  
        Gzs= (6.+alpha_s)*(5.+alpha_s)*(4.+alpha_s)/((3.+alpha_s)*(2.+alpha_s)*          &
             (1.+alpha_s))
     else                     
        Gzs= (4.+alpha_s)*(3.+alpha_s)/((2.+alpha_s)*(1.+alpha_s))
     endif
     Gzg= (6.+alpha_g)*(5.+alpha_g)*(4.+alpha_g)/((3.+alpha_g)*(2.+alpha_g)*(1.+alpha_g))
     Gzh= (6.+alpha_h)*(5.+alpha_h)*(4.+alpha_h)/((3.+alpha_h)*(2.+alpha_h)*(1.+alpha_h))

     do k= ktop,kbot,-kdir
       do i= 1,ni
           DE(i,k)= pres(i,k)/(RGASD*T(i,k))
           tmp9= DE(i,k)*DE(i,k)

           N_c= NC(i,k)
           N_r= NR(i,k)
           N_i= NY(i,k)
           N_s= NN(i,k)
           N_g= NG(i,k)
           N_h= NH(i,k)

        
           tmp1= 0.;  tmp2= 0.;  tmp3= 0.;  tmp4= 0.;  tmp5= 0.
           if (QR(i,k)>epsQ .and. N_r>epsN) tmp1 = cxr*Gzr*tmp9*QR(i,k)*QR(i,k)/N_r
           if (QI(i,k)>epsQ .and. N_i>epsN) tmp2 = cxi*Gzi*tmp9*QI(i,k)*QI(i,k)/N_i
           if (QN(i,k)>epsQ .and. N_s>epsN) tmp3 = cxi*Gzs*tmp9*QN(i,k)*QN(i,k)/N_s
           if (QG(i,k)>epsQ .and. N_g>epsN) tmp4 = cxi*Gzg*tmp9*QG(i,k)*QG(i,k)/N_g
           if (QH(i,k)>epsQ .and. N_h>epsN) tmp5 = cxi*Gzh*tmp9*QH(i,k)*QH(i,k)/N_h
          
           if ( T(i,k)>TRPL) then
             tmp2= tmp2*fdielec
             tmp3= tmp3*fdielec
             tmp4= tmp4*fdielec
             tmp5= tmp5*fdielec
           endif
           ZET(i,k) = tmp1 + tmp2 + tmp3 + tmp4 + tmp5   
           if (ZET(i,k)>0.) then
              ZET(i,k)= 10.*log10((ZET(i,k)*Zfact))      
           else
              ZET(i,k)= minZET
           endif
           ZET(i,k)= max(ZET(i,k),minZET)
           ZEC(i)= max(ZEC(i),ZET(i,k))  

         
           Dm_c(i,k)= 0.;   Dm_r(i,k)= 0.;   Dm_i(i,k)= 0.
           Dm_s(i,k)= 0.;   Dm_g(i,k)= 0.;   Dm_h(i,k)= 0.
           if(QC(i,k)>epsQ.and.N_c>epsN) Dm_c(i,k)=Dm_x(DE(i,k),QC(i,k),1./N_c,icmr,thrd)
           if(QR(i,k)>epsQ.and.N_r>epsN) Dm_r(i,k)=Dm_x(DE(i,k),QR(i,k),1./N_r,icmr,thrd)
           if(QI(i,k)>epsQ.and.N_i>epsN) Dm_i(i,k)=Dm_x(DE(i,k),QI(i,k),1./N_i,icmi,thrd)
           if(QN(i,k)>epsQ.and.N_s>epsN) Dm_s(i,k)=Dm_x(DE(i,k),QN(i,k),1./N_s,icms,idms)
           if(QG(i,k)>epsQ.and.N_g>epsN) Dm_g(i,k)=Dm_x(DE(i,k),QG(i,k),1./N_g,icmg,thrd)
           if(QH(i,k)>epsQ.and.N_h>epsN) Dm_h(i,k)=Dm_x(DE(i,k),QH(i,k),1./N_h,icmh,thrd)

        enddo  
     enddo     

  ENDIF


  iDE = (RGASD*T)/pres
  NC  = NC*iDE
  NR  = NR*iDE
  NY  = NY*iDE
  NN  = NN*iDE
  NG  = NG*iDE
  NH  = NH*iDE









 
END SUBROUTINE mp_milbrandt2mom_main
 

   real function des_OF_Ds(Ds_local,desMax_local,eds_local,fds_local)
   
      real :: Ds_local,desMax_local,eds_local,fds_local

      des_OF_Ds= min(desMax_local, eds_local*exp(fds_local*log(Ds_local)))   
   end function des_OF_Ds


   real function Dm_x(DE_local,QX_local,iNX_local,icmx_local,idmx_local)
   
      real :: DE_local,QX_local,iNX_local,icmx_local,idmx_local
     
      Dm_x = exp(idmx_local*log(DE_local*QX_local*iNX_local*icmx_local))     
   end function Dm_x


   real function iLAMDA_x(DE_local,QX_local,iNX_local,icex_local,idmx_local)
   
      real :: DE_local,QX_local,iNX_local,icex_local,idmx_local
     
      iLAMDA_x = exp(idmx_local*log(DE_local*QX_local*iNX_local*icex_local)) 
   end function


   real function N_Cooper(TRPL_local,T_local)
   
   
      real :: TRPL_local,T_local
      N_Cooper= 5.*exp(0.304*(TRPL_local-max(233.,T_local)))
   end function N_Cooper

   real function Nos_Thompson(TRPL_local,T_local)
   
   
      real :: TRPL_local,T_local
      Nos_Thompson= min(2.e+8, 2.e+6*exp(-0.12*min(-0.001,T_local-TRPL_local)))
   end function Nos_Thompson



END MODULE my_dmom_mod



 MODULE module_mp_milbrandt2mom

      use module_wrf_error
      use my_dmom_mod

      implicit none




      CONTAINS


      SUBROUTINE milbrandt2mom_init



      END SUBROUTINE milbrandt2mom_init



 
 
 


      SUBROUTINE mp_milbrandt2mom_driver(qv, qc, qr, qi, qs, qg, qh, nc, nr, ni, ns, ng,  &
                              nh, th, pii, p, w, dz, dt_in, itimestep,                    &
                              RAINNC, RAINNCV, SNOWNC, SNOWNCV, GRPLNC, GRPLNCV,          &
                              HAILNC, HAILNCV, SR, Zet,                                   &
                              ids,ide, jds,jde, kds,kde,                                  &  
                              ims,ime, jms,jme, kms,kme,                                  &  
                              its,ite, jts,jte, kts,kte)                                     

      implicit none

 
      integer, intent(in):: ids,ide, jds,jde, kds,kde,                                   &
                            ims,ime, jms,jme, kms,kme,                                   &
                            its,ite, jts,jte, kts,kte
      real, dimension(ims:ime, kms:kme, jms:jme), intent(inout)::                        &
                            qv,qc,qr,qi,qs,qg,qh,nc,nr,ni,ns,ng,nh,th,Zet
      real, dimension(ims:ime, kms:kme, jms:jme), intent(in)::                           &
                            pii,p,w,dz
      real, dimension(ims:ime, jms:jme), intent(inout)::                                 &
                            RAINNC,RAINNCV,SNOWNC,SNOWNCV,GRPLNC,GRPLNCV,HAILNC,HAILNCV, &
                            SR
      real, intent(in)::    dt_in
      integer, intent(in):: itimestep 

 
      real, dimension(1:ite-its+1,1:kte-kts+1) :: t2d,p2d,sigma2d

     
      real, dimension(1:ite-its+1,1:kte-kts+1)      :: Dm_c,Dm_r,Dm_i,Dm_s,Dm_g,Dm_h
      real, dimension(1:ite-its+1,1:kte-kts+1,1:20) :: SS
      
      

      real, dimension(1:ite-its+1) :: rt_rn1,rt_rn2,rt_fr1,rt_fr2,rt_sn1,rt_sn2,rt_sn3,  &
            rt_pe1,rt_pe2,rt_peL,rt_snd,ZEC,p_sfc
      real, dimension(ims:ime, kms:kme, jms:jme) :: t3d

      real    :: dt,ms2mmstp
      integer :: i,j,k,i2d,k2d,i2d_max,k2d_max
      integer :: i_start, j_start, i_end, j_end, CCNtype
      logical :: precipDiag_ON,sedi_ON,warmphase_ON,autoconv_ON,icephase_ON,snow_ON

      real, parameter    :: ms2mmh    = 3.6e+6   
      logical, parameter :: nk_BOTTOM = .false.  


      i2d_max  = ite-its+1
      k2d_max  = kte-kts+1
      dt       = dt_in
      ms2mmstp = 1.e+3*dt    

   

      CCNtype       = 2.  

      precipDiag_ON = .true.
      sedi_ON       = .true.
      warmphase_ON  = .true.
      autoconv_ON   = .true.
      icephase_ON   = .true.
      snow_ON       = .true.
   

      RAINNCV(its:ite,jts:jte) = 0.
      SNOWNCV(its:ite,jts:jte) = 0.
      GRPLNCV(its:ite,jts:jte) = 0.
      HAILNCV(its:ite,jts:jte) = 0.
      SR(its:ite,jts:jte)      = 0.

      do j = jts, jte

         t2d(:,:) = th(its:ite,kts:kte,j)*pii(its:ite,kts:kte,j)
         p2d(:,:) = p(its:ite,kts:kte,j)
         p_sfc(:) = p2d(:,k2d_max)

         do i = its, ite
            i2d = i-its+1
            sigma2d(i2d,:) = p2d(i2d,:)/p_sfc(i2d)
         enddo

         call mp_milbrandt2mom_main(w(its:ite,kts:kte,j),t2d,qv(its:ite,kts:kte,j), &
               qc(its:ite,kts:kte,j),qr(its:ite,kts:kte,j),qi(its:ite,kts:kte,j),   &
               qs(its:ite,kts:kte,j),qg(its:ite,kts:kte,j),qh(its:ite,kts:kte,j),   &
               nc(its:ite,kts:kte,j),nr(its:ite,kts:kte,j),ni(its:ite,kts:kte,j),   &
               ns(its:ite,kts:kte,j),ng(its:ite,kts:kte,j),nh(its:ite,kts:kte,j),   &
               p_sfc,sigma2d,rt_rn1,rt_rn2,rt_fr1,rt_fr2,rt_sn1,rt_sn2,rt_sn3,      &
               rt_pe1,rt_pe2,rt_peL,rt_snd,dt,i2d_max,k2d_max,j,itimestep,CCNtype,  &
               precipDiag_ON,sedi_ON,warmphase_ON,autoconv_ON,icephase_ON,snow_ON,  &
               Dm_c,Dm_r,Dm_i,Dm_s,Dm_g,Dm_h,Zet(its:ite,kts:kte,j),ZEC,SS,nk_BOTTOM)

         th(its:ite,kts:kte,j) = t2d(:,:)/pii(its:ite,kts:kte,j)

         
         RAINNCV(its:ite,j) = ( rt_rn1(:)+rt_rn2(:)+rt_fr1(:)+rt_fr2(:)+rt_sn1(:)+rt_sn2(:)+ &
                                rt_sn3(:)+rt_pe1(:)+rt_pe2(:) )*ms2mmstp
         SNOWNCV(its:ite,j) = (rt_sn1(:) + rt_sn2(:))*ms2mmstp
         HAILNCV(its:ite,j) = (rt_pe1(:) + rt_pe2(:))*ms2mmstp
         GRPLNCV(its:ite,j) = rt_sn3(:)*ms2mmstp


      enddo 

      END SUBROUTINE mp_milbrandt2mom_driver




END MODULE module_mp_milbrandt2mom
