



























MODULE module_bl_mynn

  USE module_model_constants, only: &
       &karman, g, p1000mb, &
       &cp, r_d, rcp, xlv, xlf,&
       &svp1, svp2, svp3, svpt0, ep_1, ep_2

  USE module_state_description, only: param_first_scalar, &
       &p_qc, p_qr, p_qi, p_qs, p_qg, p_qnc, p_qni

  IMPLICIT NONE



  REAL, PARAMETER :: cphm_st=5.0, cphm_unst=16.0, &
                     cphh_st=5.0, cphh_unst=16.0

  REAL, PARAMETER :: xlvcp=xlv/cp, xlscp=(xlv+xlf)/cp, ev=xlv, rd=r_d, &
       &rk=cp/rd, svp11=svp1*1.e3, p608=ep_1, ep_3=1.-ep_2

  REAL, PARAMETER :: tref=300.0    
  REAL, PARAMETER :: tv0=p608*tref, tv1=(1.+p608)*tref, gtr=g/tref


  REAL, PARAMETER :: &
       &vk  = karman, &
       &pr  =  0.74, &
       &g1  =  0.229, &  
       &b1  = 24.0, &
       &b2  = 15.0, &    
       &c2  =  0.729, &  
       &c3  =  0.340, &  
       &c4  =  0.0, &
       &c5  =  0.2, &
       &a1  = b1*( 1.0-3.0*g1 )/6.0, &

       &c1  = g1 -1.0/( 3.0*a1*2.88449914061481660), &
       &a2  = a1*( g1-c1 )/( g1*pr ), &
       &g2  = b2/b1*( 1.0-c3 ) +2.0*a1/b1*( 3.0-2.0*c2 )

  REAL, PARAMETER :: &
       &cc2 =  1.0-c2, &
       &cc3 =  1.0-c3, &
       &e1c =  3.0*a2*b2*cc3, &
       &e2c =  9.0*a1*a2*cc2, &
       &e3c =  9.0*a2*a2*cc2*( 1.0-c5 ), &
       &e4c = 12.0*a1*a2*cc2, &
       &e5c =  6.0*a1*a1







  REAL, PARAMETER :: qmin=0.0, zmax=1.0, cns=2.1, &
            &alp1=0.23, alp2=0.65, alp3=3.0, alp4=20.0, &
            &alp5=1.0, Sqfac=2.0



  REAL, PARAMETER :: gno=1.0  
  REAL, PARAMETER :: gpw=5./3., qcgmin=1.e-8, qkemin=1.e-12



  REAL, PARAMETER :: rr2=0.7071068, rrp=0.3989423


  
  
  
  
  
  
  
  
  REAL, PARAMETER :: CKmod=1.

  
  
  REAL, PARAMETER :: BLmod=1.

  
  REAL, PARAMETER :: Cloudmix=0.


  INTEGER :: mynn_level

  INTEGER, PARAMETER :: kdebug=27

CONTAINS


































































































































  SUBROUTINE  mym_initialize ( kts,kte,&
       &            dz, zw,  &
       &            u, v, thl, qw, &


       &        zi,theta,&
       &        sh,&

       &            ust, rmo, el,&
       &            Qke, Tsq, Qsq, Cov)


    
    INTEGER, INTENT(IN)   :: kts,kte

    REAL, INTENT(IN)   :: ust, rmo
    REAL, DIMENSION(kts:kte), INTENT(in) :: dz
    REAL, DIMENSION(kts:kte+1), INTENT(in) :: zw
    REAL, DIMENSION(kts:kte), INTENT(in) :: u,v,thl,qw

    REAL, DIMENSION(kts:kte), INTENT(out) :: tsq,qsq,cov
    REAL, DIMENSION(kts:kte), INTENT(inout) :: el,qke

    REAL, DIMENSION(kts:kte) :: &
         &ql,pdk,pdt,pdq,pdc,dtl,dqw,dtv,&
         &gm,gh,sm,sh,qkw,vt,vq
    INTEGER :: k,l,lmax
    REAL :: phm,vkz,elq,elv,b1l,b2l,pmz=1.,phh=1.,flt=0.,flq=0.,tmpq

    REAL :: zi
    REAL, DIMENSION(kts:kte) :: theta




    DO k = kts,kte
       ql(k) = 0.0
       vt(k) = 0.0
       vq(k) = 0.0
    END DO

    CALL mym_level2 ( kts,kte,&
         &            dz,  &
         &            u, v, thl, qw, &
         &            ql, vt, vq, &
         &            dtl, dqw, dtv, gm, gh, sm, sh )



    el (kts) = 0.0
    qke(kts) = ust**2 * ( b1*pmz )**(2.0/3.0)

    phm      = phh*b2 / ( b1*pmz )**(1.0/3.0)
    tsq(kts) = phm*( flt/ust )**2
    qsq(kts) = phm*( flq/ust )**2
    cov(kts) = phm*( flt/ust )*( flq/ust )

    DO k = kts+1,kte
       vkz = vk*zw(k)
       el (k) = vkz/( 1.0 + vkz/100.0 )
       qke(k) = 0.0

       tsq(k) = 0.0
       qsq(k) = 0.0
       cov(k) = 0.0
    END DO



    lmax = 5 

    DO l = 1,lmax

       CALL mym_length ( kts,kte,&
            &            dz, zw, &
            &            rmo, flt, flq, &
            &            vt, vq, &
            &            qke, &
            &            dtv, &
            &            el, &

            &            zi,theta,&

            &            qkw)

       DO k = kts+1,kte
          elq = el(k)*qkw(k)
          pdk(k) = elq*( sm(k)*gm (k)+&
               &sh(k)*gh (k) )
          pdt(k) = elq*  sh(k)*dtl(k)**2
          pdq(k) = elq*  sh(k)*dqw(k)**2
          pdc(k) = elq*  sh(k)*dtl(k)*dqw(k)
       END DO


       vkz = vk*0.5*dz(kts)

       elv = 0.5*( el(kts+1)+el(kts) ) /  vkz 
       qke(kts) = ust**2 * ( b1*pmz*elv    )**(2.0/3.0)

       phm      = phh*b2 / ( b1*pmz/elv**2 )**(1.0/3.0)
       tsq(kts) = phm*( flt/ust )**2
       qsq(kts) = phm*( flq/ust )**2
       cov(kts) = phm*( flt/ust )*( flq/ust )

       DO k = kts+1,kte-1
          b1l = b1*0.25*( el(k+1)+el(k) )
          tmpq=MAX(b1l*( pdk(k+1)+pdk(k) ),qkemin)

          qke(k) = tmpq**(2.0/3.0)


          IF ( qke(k) .LE. 0.0 ) THEN
             b2l = 0.0
          ELSE
             b2l = b2*( b1l/b1 ) / SQRT( qke(k) )
          END IF

          tsq(k) = b2l*( pdt(k+1)+pdt(k) )
          qsq(k) = b2l*( pdq(k+1)+pdq(k) )
          cov(k) = b2l*( pdc(k+1)+pdc(k) )
       END DO


    END DO






    qke(kte)=qke(kte-1)
    tsq(kte)=tsq(kte-1)
    qsq(kte)=qsq(kte-1)
    cov(kte)=cov(kte-1)




  END SUBROUTINE mym_initialize
  

















  SUBROUTINE  mym_level2 (kts,kte,&
       &            dz, &
       &            u, v, thl, qw, &
       &            ql, vt, vq, &
       &            dtl, dqw, dtv, gm, gh, sm, sh )



    INTEGER, INTENT(IN)   :: kts,kte
    REAL, DIMENSION(kts:kte), INTENT(in) :: dz
    REAL, DIMENSION(kts:kte), INTENT(in) :: u,v,thl,qw,ql,vt,vq

    REAL, DIMENSION(kts:kte), INTENT(out) :: &
         &dtl,dqw,dtv,gm,gh,sm,sh

    INTEGER :: k

    REAL :: rfc,f1,f2,rf1,rf2,smc,shc,&
         &ri1,ri2,ri3,ri4,duz,dtz,dqz,vtt,vqq,dtq,dzk,afk,abk,ri,rf


    REAL ::   a2den







    rfc = g1/( g1+g2 )
    f1  = b1*( g1-c1 ) +3.0*a2*( 1.0    -c2 )*( 1.0-c5 ) &
    &                   +2.0*a1*( 3.0-2.0*c2 )
    f2  = b1*( g1+g2 ) -3.0*a1*( 1.0    -c2 )
    rf1 = b1*( g1-c1 )/f1
    rf2 = b1*  g1     /f2
    smc = a1 /a2*  f1/f2
    shc = 3.0*a2*( g1+g2 )

    ri1 = 0.5/smc
    ri2 = rf1*smc
    ri3 = 4.0*rf2*smc -2.0*ri2
    ri4 = ri2**2

    DO k = kts+1,kte
       dzk = 0.5  *( dz(k)+dz(k-1) )
       afk = dz(k)/( dz(k)+dz(k-1) )
       abk = 1.0 -afk
       duz = ( u(k)-u(k-1) )**2 +( v(k)-v(k-1) )**2
       duz =   duz                    /dzk**2
       dtz = ( thl(k)-thl(k-1) )/( dzk )
       dqz = ( qw(k)-qw(k-1) )/( dzk )

       vtt =  1.0 +vt(k)*abk +vt(k-1)*afk
       vqq =  tv0 +vq(k)*abk +vq(k-1)*afk
       dtq =  vtt*dtz +vqq*dqz

       dtl(k) =  dtz
       dqw(k) =  dqz
       dtv(k) =  dtq




       gm (k) =  duz
       gh (k) = -dtq*gtr


       ri = -gh(k)/MAX( duz, 1.0e-10 )


    IF (CKmod .eq. 1) THEN
       a2den = 1. + MAX(ri,0.0)
    ELSE
       a2den = 1. + 0.0
    ENDIF

       rfc = g1/( g1+g2 )
       f1  = b1*( g1-c1 ) +3.0*(a2/a2den)*( 1.0    -c2 )*( 1.0-c5 ) &
    &                     +2.0*a1*( 3.0-2.0*c2 )
       f2  = b1*( g1+g2 ) -3.0*a1*( 1.0    -c2 )
       rf1 = b1*( g1-c1 )/f1
       rf2 = b1*  g1     /f2
       smc = a1 /(a2/a2den)*  f1/f2
       shc = 3.0*(a2/a2den)*( g1+g2 )

       ri1 = 0.5/smc
       ri2 = rf1*smc
       ri3 = 4.0*rf2*smc -2.0*ri2
       ri4 = ri2**2



       rf = MIN( ri1*( ri+ri2-SQRT(ri**2-ri3*ri+ri4) ), rfc )

       sh (k) = shc*( rfc-rf )/( 1.0-rf )
       sm (k) = smc*( rf1-rf )/( rf2-rf ) * sh(k)
    END DO

    RETURN

  END SUBROUTINE mym_level2
















  SUBROUTINE  mym_length ( kts,kte,&
    &            dz, zw, &
    &            rmo, flt, flq, &
    &            vt, vq, &
    &            qke, &
    &            dtv, &
    &            el, &
    &            zi,theta,&       
    &            qkw)
    


    INTEGER, INTENT(IN)   :: kts,kte
    REAL, DIMENSION(kts:kte), INTENT(in)   :: dz
    REAL, DIMENSION(kts:kte+1), INTENT(in) :: zw
    REAL, INTENT(in) :: rmo,flt,flq
    REAL, DIMENSION(kts:kte), INTENT(IN)   :: qke,vt,vq

    REAL, DIMENSION(kts:kte), INTENT(out)  :: qkw, el
    REAL, DIMENSION(kts:kte), INTENT(in)   :: dtv

    REAL :: elt,vsc

    REAL, DIMENSION(kts:kte), INTENT(IN) :: theta
    REAL, DIMENSION(kts:kte) :: qtke,elBLmin,elBLavg,thetaw
    REAL :: wt,zi,zi2,h1,h2

    
    
    
    
    REAL, PARAMETER :: minzi = 300.  
    REAL, PARAMETER :: maxdz = 750.  
                                     
                                     
    REAL, PARAMETER :: mindz = 300.  

    
    REAL, PARAMETER :: ZSLH = 100. 
    REAL, PARAMETER :: CSL = 2.    
    REAL :: z_m



    INTEGER :: i,j,k
    REAL :: afk,abk,zwk,dzk,qdz,vflx,bv,elb,els,elf






    IF ( BLmod .EQ. 0. ) THEN
       zi2=5000.  
    ELSE
       zi2=MAX(zi,minzi)
    ENDIF
    h1=MAX(0.3*zi2,mindz)
    h1=MIN(h1,maxdz)         
    h2=h1/2.0                

    qtke(kts)=MAX(qke(kts)/2.,0.01) 
    thetaw(kts)=theta(kts)          

    qkw(kts) = SQRT(MAX(qke(kts),1.0e-10))

    DO k = kts+1,kte
       afk = dz(k)/( dz(k)+dz(k-1) )
       abk = 1.0 -afk
       qkw(k) = SQRT(MAX(qke(k)*abk+qke(k-1)*afk,1.0e-3))


       qtke(k) = (qkw(k)**2.)/2.    
       thetaw(k)= theta(k)*abk + theta(k-1)*afk


    END DO

    elt = 1.0e-5
    vsc = 1.0e-5






     k = kts+1
     zwk = zw(k)
     DO WHILE (zwk .LE. MIN((zi2+h1), 4000.)) 
       dzk = 0.5*( dz(k)+dz(k-1) )
       qdz = MAX( qkw(k)-qmin, 0.03 )*dzk
             elt = elt +qdz*zwk
             vsc = vsc +qdz
       k   = k+1
       zwk = zw(k)
    END DO

    elt =  alp1*elt/vsc
    vflx = ( vt(kts)+1.0 )*flt +( vq(kts)+tv0 )*flq
    vsc = ( gtr*elt*MAX( vflx, 0.0 ) )**(1.0/3.0)


    el(kts) = 0.0


    IF ( BLmod .GT. 0. ) THEN
       
       CALL boulac_length(kts,kte,zw,dz,qtke,thetaw,elBLmin,elBLavg)
    ENDIF


    DO k = kts+1,kte
       zwk = zw(k)              


       IF ( dtv(k) .GT. 0.0 ) THEN
          bv  = SQRT( gtr*dtv(k) )
          elb = alp2*qkw(k) / bv &
               &       *( 1.0 + alp3/alp2*&
               &SQRT( vsc/( bv*elt ) ) )

          elf = alp2 * qkw(k)/bv
       ELSE
          elb = 1.0e10
          elf = elb
       END IF

       z_m = MAX(ZSLH,CSL*zwk*rmo)


       IF ( rmo .GT. 0.0 ) THEN
       
             els = vk*zwk/(1.0+cns*MIN( zwk*rmo, zmax ))
             
       
       
       
       
       ELSE
          els =  vk*zwk*( 1.0 - alp4* zwk*rmo )**0.2
       END IF





       IF ( BLmod .EQ. 0. ) THEN
          el(k) = MIN(elb/( elb/elt+elb/els+1.0 ),elf)
       ELSE
          
          
          el(k) = MIN(elb/( elb/elt+elb/els+1.0 ),elf)
          wt=.5*TANH((zwk - (zi2+h1))/h2) + .5
          el(k) = el(k)*(1.-wt) + alp5*elBLmin(k)*wt
       ENDIF


       
       
       
    END DO

    RETURN

  END SUBROUTINE mym_length



  SUBROUTINE boulac_length(kts,kte,zw,dz,qtke,theta,lb1,lb2)

















     INTEGER, INTENT(IN) :: kts,kte
     REAL, DIMENSION(kts:kte), INTENT(IN) :: qtke,dz,theta
     REAL, DIMENSION(kts:kte), INTENT(OUT) :: lb1,lb2
     REAL, DIMENSION(kts:kte+1), INTENT(IN) :: zw

     
     INTEGER :: iz, izz, found
     REAL, DIMENSION(kts:kte) :: dlu,dld
     REAL, PARAMETER :: Lmax=2000.  
     REAL :: dzt, zup, beta, zup_inf, bbb, tl, zdo, zdo_sup, zzz

     

     do iz=kts,kte

        
        
        
        zup=0.
        dlu(iz)=zw(kte+1)-zw(iz)-dz(iz)/2.
        zzz=0.
        zup_inf=0.
        beta=g/theta(iz)           

        

        if (iz .lt. kte) then      

          found = 0
          izz=iz       
          DO WHILE (found .EQ. 0) 

            if (izz .lt. kte) then
              dzt=dz(izz)                    
              zup=zup-beta*theta(iz)*dzt     
              
              zup=zup+beta*(theta(izz+1)+theta(izz))*dzt/2. 
              zzz=zzz+dzt                   
              
              if (qtke(iz).lt.zup .and. qtke(iz).ge.zup_inf) then
                 bbb=(theta(izz+1)-theta(izz))/dzt
                 if (bbb .ne. 0.) then
                    
                    tl=(-beta*(theta(izz)-theta(iz)) + &
                      & sqrt( max(0.,(beta*(theta(izz)-theta(iz)))**2. + &
                      &       2.*bbb*beta*(qtke(iz)-zup_inf))))/bbb/beta
                 else
                    if (theta(izz) .ne. theta(iz))then
                       tl=(qtke(iz)-zup_inf)/(beta*(theta(izz)-theta(iz)))
                    else
                       tl=0.
                    endif
                 endif            
                 dlu(iz)=zzz-dzt+tl
                 
                 found =1
              endif
              zup_inf=zup
              izz=izz+1
             ELSE
              found = 1
            ENDIF

          ENDDO

        endif
                   
        
        
        
        zdo=0.
        zdo_sup=0.
        dld(iz)=zw(iz)
        zzz=0.

        
        if (iz .gt. kts) then  

          found = 0
          izz=iz       
          DO WHILE (found .EQ. 0) 

            if (izz .gt. kts) then
              dzt=dz(izz-1)
              zdo=zdo+beta*theta(iz)*dzt
              
              zdo=zdo-beta*(theta(izz-1)+theta(izz))*dzt/2.
              zzz=zzz+dzt
              
              if (qtke(iz).lt.zdo .and. qtke(iz).ge.zdo_sup) then
                 bbb=(theta(izz)-theta(izz-1))/dzt
                 if (bbb .ne. 0.) then
                    tl=(beta*(theta(izz)-theta(iz))+ &
                      & sqrt( max(0.,(beta*(theta(izz)-theta(iz)))**2. + &
                      &       2.*bbb*beta*(qtke(iz)-zdo_sup))))/bbb/beta
                 else
                    if (theta(izz) .ne. theta(iz)) then
                       tl=(qtke(iz)-zdo_sup)/(beta*(theta(izz)-theta(iz)))
                    else
                       tl=0.
                    endif
                 endif            
                 dld(iz)=zzz-dzt+tl
                 
                 found = 1
              endif
              zdo_sup=zdo
              izz=izz-1
            ELSE
              found = 1
            ENDIF
          ENDDO

        endif

        
        
        
        
        
        dld(iz) = min(dld(iz),zw(iz+1))
        lb1(iz) = min(dlu(iz),dld(iz))     
        lb2(iz) = sqrt(dlu(iz)*dld(iz))    
        

        
        lb1(iz) = lb1(iz)/(1. + (lb1(iz)/Lmax))
        lb2(iz) = lb2(iz)/(1. + (lb2(iz)/Lmax))
 
        if (iz .eq. kte) then
           lb1(kte) = lb1(kte-1)
           lb2(kte) = lb2(kte-1)
        endif
        
        

     ENDDO
                   
  END SUBROUTINE boulac_length



































  SUBROUTINE  mym_turbulence ( kts,kte,&
    &            levflag, &
    &            dz, zw, &
    &            u, v, thl, ql, qw, &
    &            qke, tsq, qsq, cov, &
    &            vt, vq,&
    &            rmo, flt, flq, &

    &            zi,theta,&
    &            sh,&

    &            El,&
    &            Dfm, Dfh, Dfq, Tcd, Qcd, Pdk, Pdt, Pdq, Pdc &

    &		 ,qWT1D,qSHEAR1D,qBUOY1D,qDISS1D &
    &            ,bl_mynn_tkebudget &

    &)



    INTEGER, INTENT(IN)   :: kts,kte
    INTEGER, INTENT(IN)   :: levflag
    REAL, DIMENSION(kts:kte), INTENT(in) :: dz
    REAL, DIMENSION(kts:kte+1), INTENT(in) :: zw
    REAL, INTENT(in) :: rmo,flt,flq   
    REAL, DIMENSION(kts:kte), INTENT(in) :: u,v,thl,qw,& 
         &ql,vt,vq,qke,tsq,qsq,cov

    REAL, DIMENSION(kts:kte), INTENT(out) :: dfm,dfh,dfq,&
         &pdk,pdt,pdq,pdc,tcd,qcd,el


    REAL, DIMENSION(kts:kte), INTENT(inout) :: &
         qWT1D,qSHEAR1D,qBUOY1D,qDISS1D
    REAL :: q3sq_old,dlsq1,qWTP_old,qWTP_new
    REAL :: dudz,dvdz,dTdz,&
            upwp,vpwp,Tpwp
    INTEGER, INTENT(in) :: bl_mynn_tkebudget


    REAL, DIMENSION(kts:kte) :: qkw,dtl,dqw,dtv,gm,gh,sm,sh

    INTEGER :: k

    REAL :: e6c,dzk,afk,abk,vtt,vqq,&
         &cw25,clow,cupp,gamt,gamq,smd,gamv,elq,elh


    REAL :: zi
    REAL, DIMENSION(kts:kte), INTENT(in) :: theta


    REAL ::  a2den, duz, ri, HLmod  

    DOUBLE PRECISION  q2sq, t2sq, r2sq, c2sq, elsq, gmel, ghel
    DOUBLE PRECISION  q3sq, t3sq, r3sq, c3sq, dlsq, qdiv
    DOUBLE PRECISION  e1, e2, e3, e4, enum, eden, wden













    CALL mym_level2 (kts,kte,&
    &            dz, &
    &            u, v, thl, qw, &
    &            ql, vt, vq, &
    &            dtl, dqw, dtv, gm, gh, sm, sh )

    CALL mym_length (kts,kte, &
    &            dz, zw, &
    &            rmo, flt, flq, &
    &            vt, vq, &
    &            qke, &
    &            dtv, &
    &            el, &
    &            zi,theta,&  
    &            qkw)


    DO k = kts+1,kte
       dzk = 0.5  *( dz(k)+dz(k-1) )
       afk = dz(k)/( dz(k)+dz(k-1) )
       abk = 1.0 -afk
       elsq = el (k)**2
       q2sq = b1*elsq*( sm(k)*gm(k)+sh(k)*gh(k) )
       q3sq = qkw(k)**2


       duz = ( u(k)-u(k-1) )**2 +( v(k)-v(k-1) )**2
       duz =   duz                    /dzk**2
       
       ri = -gh(k)/MAX( duz, 1.0e-10 )
       IF (CKmod .eq. 1) THEN
          a2den = 1. + MAX(ri,0.0)
       ELSE
          a2den = 1. + 0.0
       ENDIF



       gmel = gm (k)*elsq
       ghel = gh (k)*elsq



       IF (sh(k)<0.0 .OR. sm(k)<0.0) THEN
          PRINT*,"MYM_TURBULENCE2.0: k=",k," sh=",sh(k)
          PRINT*," gm=",gm(k)," gh=",gh(k)," sm=",sm(k)
          PRINT*," q2sq=",q2sq," q3sq=",q3sq," q3/q2=",q3sq/q2sq
          PRINT*," qke=",qke(k)," el=",el(k)," ri=",ri
          PRINT*," PBLH=",zi," u=",u(k)," v=",v(k)
       ENDIF


       IF (CKmod .eq. 1) THEN
          HLmod = q2sq -1.
       ELSE
          HLmod = q3sq
       ENDIF

       IF ( q3sq .LT. q2sq ) THEN


          qdiv = SQRT( q3sq/q2sq )   
          sm(k) = sm(k) * qdiv
          sh(k) = sh(k) * qdiv






          e1   = q3sq - e1c*ghel/a2den * qdiv**2
          e2   = q3sq - e2c*ghel/a2den * qdiv**2
          e3   = e1   + e3c*ghel/(a2den**2) * qdiv**2
          e4   = e1   - e4c*ghel/a2den * qdiv**2

          eden = e2*e4 + e3*e5c*gmel * qdiv**2
          eden = MAX( eden, 1.0d-20 )
       ELSE





          e1   = q3sq - e1c*ghel/a2den
          e2   = q3sq - e2c*ghel/a2den
          e3   = e1   + e3c*ghel/(a2den**2)
          e4   = e1   - e4c*ghel/a2den

          eden = e2*e4 + e3*e5c*gmel
          eden = MAX( eden, 1.0d-20 )

          qdiv = 1.0
          sm(k) = q3sq*a1*( e3-3.0*c1*e4       )/eden


          sh(k) = q3sq*(a2/a2den)*( e2+3.0*c1*e5c*gmel )/eden

       END IF


       IF (sh(k)<0.0 .OR. sm(k)<0.0 .OR. &
           sh(k) > 0.76*b2 .or. (sm(k)**2*gm(k) .gt. .44**2)) THEN
          PRINT*,"MYM_TURBULENCE2.5: k=",k," sh=",sh(k)
          PRINT*," gm=",gm(k)," gh=",gh(k)," sm=",sm(k)
          PRINT*," q2sq=",q2sq," q3sq=",q3sq," q3/q2=",q3sq/q2sq
          PRINT*," qke=",qke(k)," el=",el(k)," ri=",ri
          PRINT*," PBLH=",zi," u=",u(k)," v=",v(k)
       ENDIF


       IF ( levflag .EQ. 3 ) THEN
          t2sq = qdiv*b2*elsq*sh(k)*dtl(k)**2
          r2sq = qdiv*b2*elsq*sh(k)*dqw(k)**2
          c2sq = qdiv*b2*elsq*sh(k)*dtl(k)*dqw(k)
          t3sq = MAX( tsq(k)*abk+tsq(k-1)*afk, 0.0 )
          r3sq = MAX( qsq(k)*abk+qsq(k-1)*afk, 0.0 )
          c3sq =      cov(k)*abk+cov(k-1)*afk


          c3sq = SIGN( MIN( ABS(c3sq), SQRT(t3sq*r3sq) ), c3sq )

          vtt  = 1.0 +vt(k)*abk +vt(k-1)*afk
          vqq  = tv0 +vq(k)*abk +vq(k-1)*afk
          t2sq = vtt*t2sq +vqq*c2sq
          r2sq = vtt*c2sq +vqq*r2sq
          c2sq = MAX( vtt*t2sq+vqq*r2sq, 0.0d0 )
          t3sq = vtt*t3sq +vqq*c3sq
          r3sq = vtt*c3sq +vqq*r3sq
          c3sq = MAX( vtt*t3sq+vqq*r3sq, 0.0d0 )

          cw25 = e1*( e2 + 3.0*c1*e5c*gmel*qdiv**2 )/( 3.0*eden )


          dlsq =  elsq
          IF ( q3sq/dlsq .LT. -gh(k) ) q3sq = -dlsq*gh(k)






          e2   = q3sq - e2c*ghel/a2den * qdiv**2
          e3   = q3sq + e3c*ghel/(a2den**2) * qdiv**2
          e4   = q3sq - e4c*ghel/a2den * qdiv**2

          eden = e2*e4  + e3 *e5c*gmel * qdiv**2




          wden = cc3*gtr**2 * dlsq**2/elsq * qdiv**2 &
               &        *( e2*e4c/a2den - e3c*e5c*gmel/(a2den**2) * qdiv**2 )


          IF ( wden .NE. 0.0 ) THEN
             clow = q3sq*( 0.12-cw25 )*eden/wden
             cupp = q3sq*( 0.76-cw25 )*eden/wden

             IF ( wden .GT. 0.0 ) THEN
                c3sq  = MIN( MAX( c3sq, c2sq+clow ), c2sq+cupp )
             ELSE
                c3sq  = MAX( MIN( c3sq, c2sq+clow ), c2sq+cupp )
             END IF
          END IF

          e1   = e2 + e5c*gmel * qdiv**2
          eden = MAX( eden, 1.0d-20 )




          e6c  = 3.0*(a2/a2den)*cc3*gtr * dlsq/elsq




          IF ( t2sq .GE. 0.0 ) THEN
             enum = MAX( qdiv*e6c*( t3sq-t2sq ), 0.0d0 )
          ELSE
             enum = MIN( qdiv*e6c*( t3sq-t2sq ), 0.0d0 )
          ENDIF

          gamt =-e1  *enum    /eden



          IF ( r2sq .GE. 0.0 ) THEN
             enum = MAX( qdiv*e6c*( r3sq-r2sq ), 0.0d0 )
          ELSE
             enum = MIN( qdiv*e6c*( r3sq-r2sq ), 0.0d0 )
          ENDIF

          gamq =-e1  *enum    /eden



          enum = MAX( qdiv*e6c*( c3sq-c2sq ), 0.0d0)



          smd  = dlsq*enum*gtr/eden * qdiv**2 * (e3c/(a2den**2) + &
               & e4c/a2den)*a1/(a2/a2den)

          gamv = e1  *enum*gtr/eden

          sm(k) = sm(k) +smd


          qdiv = 1.0


          IF (sh(k)<0.0 .OR. sm(k)<0.0) THEN
            PRINT*,"MYM_TURBULENCE3.0: k=",k," sh=",sh(k)
            PRINT*," gm=",gm(k)," gh=",gh(k)," sm=",sm(k)
            PRINT*," q2sq=",q2sq," q3sq=",q3sq," q3/q2=",q3sq/q2sq
            PRINT*," qke=",qke(k)," el=",el(k)," ri=",ri
            PRINT*," PBLH=",zi," u=",u(k)," v=",v(k)
          ENDIF

       ELSE

          gamt = 0.0
          gamq = 0.0
          gamv = 0.0
       END IF

       elq = el(k)*qkw(k)
       elh = elq*qdiv

       pdk(k) = elq*( sm(k)*gm(k) &
            &                    +sh(k)*gh(k)+gamv )
       pdt(k) = elh*( sh(k)*dtl(k)+gamt )*dtl(k)
       pdq(k) = elh*( sh(k)*dqw(k)+gamq )*dqw(k)
       pdc(k) = elh*( sh(k)*dtl(k)+gamt )&
            &*dqw(k)*0.5 &
                  &+elh*( sh(k)*dqw(k)+gamq )*dtl(k)*0.5

       tcd(k) = elq*gamt
       qcd(k) = elq*gamq

       dfm(k) = elq*sm (k) / dzk
       dfh(k) = elq*sh (k) / dzk



       dfq(k) =     dfm(k)


   IF ( bl_mynn_tkebudget == 1) THEN
       
       dudz = ( u(k)-u(k-1) )/dzk
       dvdz = ( v(k)-v(k-1) )/dzk
       dTdz = ( thl(k)-thl(k-1) )/dzk

       upwp = -elq*sm(k)*dudz
       vpwp = -elq*sm(k)*dvdz
       Tpwp = -elq*sh(k)*dTdz
       Tpwp = SIGN(MAX(ABS(Tpwp),1.E-6),Tpwp)

       IF ( k .EQ. kts+1 ) THEN
          qWT1D(kts)=0.
          q3sq_old =0.
          qWTP_old =0.
          
          dlsq1 = MAX(el(kts)**2,1.0)
          IF ( q3sq_old/dlsq1 .LT. -gh(k) ) q3sq_old = -dlsq1*gh(k)
       ENDIF

       
       qWTP_new = elq*Sqfac*sm(k)*(q3sq - q3sq_old)/dzk
       qWT1D(k) = 0.5*(qWTP_new - qWTP_old)/dzk
       qWTP_old = elq*Sqfac*sm(k)*(q3sq - q3sq_old)/dzk
       q3sq_old = q3sq

       
       
       qSHEAR1D(k) = elq*sm(k)*gm(k)

       
       
       
       qBUOY1D(k) = elq*(sh(k)*(-dTdz*g/thl(k)) + gamv)

       
       qDISS1D(k) = (q3sq**(3./2.))/(b1*MAX(el(k),1.))
    ENDIF

    END DO


    dfm(kts) = 0.0
    dfh(kts) = 0.0
    dfq(kts) = 0.0
    tcd(kts) = 0.0
    qcd(kts) = 0.0

    tcd(kte) = 0.0
    qcd(kte) = 0.0


    DO k = kts,kte-1
       dzk = dz(k)
       tcd(k) = ( tcd(k+1)-tcd(k) )/( dzk )
       qcd(k) = ( qcd(k+1)-qcd(k) )/( dzk )
    END DO


   IF ( bl_mynn_tkebudget == 1) THEN
      
      qWT1D(kts)=0.
      qSHEAR1D(kts)=qSHEAR1D(kts+1)
      qBUOY1D(kts)=qBUOY1D(kts+1)
      qDISS1D(kts)=qDISS1D(kts+1)
   ENDIF

    RETURN

  END SUBROUTINE mym_turbulence













































  SUBROUTINE  mym_predict (kts,kte,&
       &            levflag,  &
       &            delt,&
       &            dz, &
       &            ust, flt, flq, pmz, phh, &
       &            el, dfq, &
       &            pdk, pdt, pdq, pdc,&
       &            qke, tsq, qsq, cov &
       &)


    INTEGER, INTENT(IN)   :: kts,kte    
    INTEGER, INTENT(IN) :: levflag
    REAL, INTENT(IN) :: delt
    REAL, DIMENSION(kts:kte), INTENT(IN) :: dz, dfq,el
    REAL, DIMENSION(kts:kte), INTENT(INOUT) :: pdk, pdt, pdq, pdc
    REAL, INTENT(IN) ::  flt, flq, ust, pmz, phh
    REAL, DIMENSION(kts:kte), INTENT(INOUT) :: qke,tsq, qsq, cov

    INTEGER :: k,nz
    REAL, DIMENSION(kts:kte) :: qkw, bp, rp, df3q
    REAL :: vkz,pdk1,phm,pdt1,pdq1,pdc1,b1l,b2l
    REAL, DIMENSION(kts:kte) :: dtz
    REAL, DIMENSION(1:kte-kts+1) :: a,b,c,d

    nz=kte-kts+1


    vkz = vk*0.5*dz(kts)






    DO k = kts,kte

       qkw(k) = SQRT( MAX( qke(k), 0.0 ) )
       
       df3q(k)=Sqfac*dfq(k)
       dtz(k)=delt/dz(k)
    END DO

    pdk1 = 2.0*ust**3*pmz/( vkz )
    phm  = 2.0/ust   *phh/( vkz )
    pdt1 = phm*flt**2
    pdq1 = phm*flq**2
    pdc1 = phm*flt*flq


    pdk(kts) = pdk1 -pdk(kts+1)




    pdt(kts) = pdt(kts+1)
    pdq(kts) = pdq(kts+1)
    pdc(kts) = pdc(kts+1)



    DO k = kts,kte-1
       b1l = b1*0.5*( el(k+1)+el(k) )
       bp(k) = 2.*qkw(k) / b1l
       rp(k) = pdk(k+1) + pdk(k) 
    END DO
    






    DO k=kts,kte-1
       a(k-kts+1)=-dtz(k)*df3q(k)
       b(k-kts+1)=1.+dtz(k)*(df3q(k)+df3q(k+1))+bp(k)*delt
       c(k-kts+1)=-dtz(k)*df3q(k+1)
       d(k-kts+1)=rp(k)*delt + qke(k)
    ENDDO








    a(nz)=-1. 
    b(nz)=1.
    c(nz)=0.
    d(nz)=0.

    CALL tridiag(nz,a,b,c,d)

    DO k=kts,kte   
       qke(k)=d(k-kts+1)
    ENDDO
      

    IF ( levflag .EQ. 3 ) THEN








       DO k = kts,kte-1
          b2l = b2*0.5*( el(k+1)+el(k) )
          bp(k) = 2.*qkw(k) / b2l
          rp(k) = pdt(k+1) + pdt(k) 
       END DO
       

       






       DO k=kts,kte-1
          a(k-kts+1)=-dtz(k)*dfq(k)
          b(k-kts+1)=1.+dtz(k)*(dfq(k)+dfq(k+1))+bp(k)*delt
          c(k-kts+1)=-dtz(k)*dfq(k+1)
          d(k-kts+1)=rp(k)*delt + tsq(k)
       ENDDO








       a(nz)=-1. 
       b(nz)=1.
       c(nz)=0.
       d(nz)=0.
       
       CALL tridiag(nz,a,b,c,d)
       
       DO k=kts,kte
          tsq(k)=d(k-kts+1)
       ENDDO
       


       DO k = kts,kte-1
          b2l = b2*0.5*( el(k+1)+el(k) )
          bp(k) = 2.*qkw(k) / b2l
          rp(k) = pdq(k+1) +pdq(k) 
       END DO
       

       






       DO k=kts,kte-1
          a(k-kts+1)=-dtz(k)*dfq(k)
          b(k-kts+1)=1.+dtz(k)*(dfq(k)+dfq(k+1))+bp(k)*delt
          c(k-kts+1)=-dtz(k)*dfq(k+1)
          d(k-kts+1)=rp(k)*delt + qsq(k)
       ENDDO








       a(nz)=-1. 
       b(nz)=1.
       c(nz)=0.
       d(nz)=0.
       
       CALL tridiag(nz,a,b,c,d)
       
       DO k=kts,kte
          qsq(k)=d(k-kts+1)
       ENDDO
       


       DO k = kts,kte-1
          b2l = b2*0.5*( el(k+1)+el(k) )
          bp(k) = 2.*qkw(k) / b2l
          rp(k) = pdc(k+1) + pdc(k) 
       END DO
       

       






       DO k=kts,kte-1
          a(k-kts+1)=-dtz(k)*dfq(k)
          b(k-kts+1)=1.+dtz(k)*(dfq(k)+dfq(k+1))+bp(k)*delt
          c(k-kts+1)=-dtz(k)*dfq(k+1)
          d(k-kts+1)=rp(k)*delt + cov(k)
       ENDDO








       a(nz)=-1. 
       b(nz)=1.
       c(nz)=0.
       d(nz)=0.
       
       CALL tridiag(nz,a,b,c,d)
       
       DO k=kts,kte
          cov(k)=d(k-kts+1)
       ENDDO
       
    ELSE

       DO k = kts,kte-1
          IF ( qkw(k) .LE. 0.0 ) THEN
             b2l = 0.0
          ELSE
             b2l = b2*0.25*( el(k+1)+el(k) )/qkw(k)
          END IF

          tsq(k) = b2l*( pdt(k+1)+pdt(k) )
          qsq(k) = b2l*( pdq(k+1)+pdq(k) )
          cov(k) = b2l*( pdc(k+1)+pdc(k) )
       END DO
       




       tsq(kte)=tsq(kte-1)
       qsq(kte)=qsq(kte-1)
       cov(kte)=cov(kte-1)
      
    END IF

  END SUBROUTINE mym_predict
  





























  SUBROUTINE  mym_condensation (kts,kte, &
    &            dz, &
    &            thl, qw, &
    &            p,exner, &
    &            tsq, qsq, cov, &
    &            Sh, el, bl_mynn_cloudpdf,&  
    &            Vt, Vq)


    INTEGER, INTENT(IN)   :: kts,kte, bl_mynn_cloudpdf

    REAL, DIMENSION(kts:kte), INTENT(IN) :: dz
    REAL, DIMENSION(kts:kte), INTENT(IN) :: p,exner, thl, qw, &
         &tsq, qsq, cov

    REAL, DIMENSION(kts:kte), INTENT(OUT) :: vt,vq

    REAL, DIMENSION(kts:kte) :: qmq,alp,bet,sgm,ql,cld

    DOUBLE PRECISION :: t3sq, r3sq, c3sq


    REAL :: p2a,t,esl,qsl,dqsl,q1,cld0,eq1,qll,&
         &q2p,pt,rac,qt
    INTEGER :: i,j,k

    REAL :: erf

    
    REAL::dth,dqw,dzk
    REAL, DIMENSION(kts:kte), INTENT(IN) :: Sh,el



    DO k = kts,kte-1
       p2a = exner(k)
       t  = thl(k)*p2a 










       
       esl=svp11*EXP(svp2*(t-svpt0)/(t-svp3))
       
       qsl=ep_2*esl/(p(k)-ep_3*esl)
       
       dqsl = qsl*ep_2*ev/( rd*t**2 )
       
       qmq(k) = qw(k) -qsl

       alp(k) = 1.0/( 1.0+dqsl*xlvcp )
       bet(k) = dqsl*p2a

       t3sq = MAX( tsq(k), 0.0 )
       r3sq = MAX( qsq(k), 0.0 )
       c3sq =      cov(k)
       c3sq = SIGN( MIN( ABS(c3sq), SQRT(t3sq*r3sq) ), c3sq )

       r3sq = r3sq +bet(k)**2*t3sq -2.0*bet(k)*c3sq
       IF (bl_mynn_cloudpdf == 0) THEN
          
          sgm(k) = SQRT( MAX( r3sq, 1.0d-10 ))
       ELSE
          
          
          if (k .eq. kts) then 
             dzk = 0.5*dz(k)
          else
             dzk = 0.5*( dz(k) + dz(k-1) )
          end if
          dth = 0.5*(thl(k+1)+thl(k)) - 0.5*(thl(k)+thl(MAX(k-1,kts)))
          dqw = 0.5*(qw(k+1) + qw(k)) - 0.5*(qw(k) + qw(MAX(k-1,kts)))
          sgm(k) = SQRT( MAX( (alp(k)**2 * MAX(el(k)**2,1.) * &
                             b2 * MAX(Sh(k),0.03))/4. * &
                      (dqw/dzk - bet(k)*(dth/dzk ))**2 , 1.0e-10) )
       ENDIF
    END DO

    DO k = kts,kte-1
       
       q1   = qmq(k) / sgm(k)
       
       cld(k) = 0.5*( 1.0+erf( q1*rr2 ) )







       
       
       eq1  = rrp*EXP( -0.5*q1*q1 )
       qll  = MAX( cld(k)*q1 + eq1, 0.0 )
       
       ql (k) = alp(k)*sgm(k)*qll

       q2p  = xlvcp/exner(k) 
       
       pt   = thl(k) +q2p*ql(k)
       
       qt   = 1.0 +p608*qw(k) -(1.+p608)*ql(k)
       rac  = alp(k)*( cld(k)-qll*eq1 )*( q2p*qt-(1.+p608)*pt )

       
       
       
       vt (k) =      qt-1.0 -rac*bet(k)
       vq (k) = p608*pt-tv0 +rac
    END DO


    cld(kte) = cld(kte-1)
    ql(kte) = ql(kte-1)
    vt(kte) = vt(kte-1)
    vq(kte) = vq(kte-1)

    RETURN

  END SUBROUTINE mym_condensation


  SUBROUTINE mynn_tendencies(kts,kte,&
       &levflag,grav_settling,&
       &delt,&
       &dz,&
       &u,v,th,qv,qc,qi,qni,& 
       &p,exner,&
       &thl,sqv,sqc,sqi,sqw,&
       &ust,flt,flq,flqv,flqc,wspd,qcg,&
       &uoce,voce,&
       &tsq,qsq,cov,&
       &tcd,qcd,&
       &dfm,dfh,dfq,&
       &Du,Dv,Dth,Dqv,Dqc,Dqi,Dqni&
       &,vdfg1&               
       &,FLAG_QI,FLAG_QNI,FLAG_QC,FLAG_QNC &
       &)


    INTEGER, INTENT(in) :: kts,kte
    INTEGER, INTENT(in) :: grav_settling,levflag
    LOGICAL, INTENT(IN) :: FLAG_QI,FLAG_QNI,FLAG_QC,FLAG_QNC









    REAL, DIMENSION(kts:kte), INTENT(in) :: u,v,th,qv,qc,qi,qni,&
         &p,exner,dfm,dfh,dfq,dz,tsq,qsq,cov,tcd,qcd
    REAL, DIMENSION(kts:kte), INTENT(inout) :: thl,sqw,sqv,sqc,sqi
    REAL, DIMENSION(kts:kte), INTENT(inout) :: du,dv,dth,dqv,dqc,dqi,&
         &dqni
    REAL, INTENT(IN) :: delt,ust,flt,flq,flqv,flqc,wspd,uoce,voce,qcg






    REAL, DIMENSION(kts:kte) :: dtz,vt,vq,qni2

    REAL, DIMENSION(1:kte-kts+1) :: a,b,c,d

    REAL :: rhs,gfluxm,gfluxp,dztop

    REAL :: grav_settling2,vdfg1    

    INTEGER :: k,kk,nz

    nz=kte-kts+1

    dztop=.5*(dz(kte)+dz(kte-1))

    DO k=kts,kte
       dtz(k)=delt/dz(k)
    ENDDO




   
    k=kts

    a(1)=0.
    b(1)=1.+dtz(k)*(dfm(k+1)+ust**2/wspd)
    c(1)=-dtz(k)*dfm(k+1)

    d(1)=u(k)+dtz(k)*uoce*ust**2/wspd





    
    DO k=kts+1,kte-1
       kk=k-kts+1
       a(kk)=-dtz(k)*dfm(k)
       b(kk)=1.+dtz(k)*(dfm(k)+dfm(k+1))
       c(kk)=-dtz(k)*dfm(k+1)
       d(kk)=u(k)
    ENDDO

















    a(nz)=0
    b(nz)=1.
    c(nz)=0.
    d(nz)=u(kte)

    CALL tridiag(nz,a,b,c,d)
    
    DO k=kts,kte
       du(k)=(d(k-kts+1)-u(k))/delt
    ENDDO





    k=kts

    a(1)=0.
    b(1)=1.+dtz(k)*(dfm(k+1)+ust**2/wspd)
    c(1)=-dtz(k)*dfm(k+1)

    d(1)=v(k)+dtz(k)*voce*ust**2/wspd






    DO k=kts+1,kte-1
       kk=k-kts+1
       a(kk)=-dtz(k)*dfm(k)
       b(kk)=1.+dtz(k)*(dfm(k)+dfm(k+1))
       c(kk)=-dtz(k)*dfm(k+1)
       d(kk)=v(k)
    ENDDO


















    a(nz)=0
    b(nz)=1.
    c(nz)=0.
    d(nz)=v(kte)

    CALL tridiag(nz,a,b,c,d)
    
    DO k=kts,kte
       dv(k)=(d(k-kts+1)-v(k))/delt
    ENDDO





    k=kts

    a(1)=0.
    b(1)=1.+dtz(k)*dfh(k+1)
    c(1)=-dtz(k)*dfh(k+1)
    






























    rhs= tcd(k) 


    d(1)=thl(k) + dtz(k)*flt + rhs*delt
    
    DO k=kts+1,kte-1
       kk=k-kts+1
       a(kk)=-dtz(k)*dfh(k)
       b(kk)=1.+dtz(k)*(dfh(k)+dfh(k+1)) 
       c(kk)=-dtz(k)*dfh(k+1)













       rhs= tcd(k) 


       d(kk)=thl(k) + rhs*delt
    ENDDO







 











    a(nz)=0.
    b(nz)=1.
    c(nz)=0.
    d(nz)=thl(kte)

    CALL tridiag(nz,a,b,c,d)
    
    DO k=kts,kte
       thl(k)=d(k-kts+1)
    ENDDO







































































 
























IF (Cloudmix > 0.5 .AND. FLAG_QC) THEN

    k=kts

    a(1)=0.
    b(1)=1.+dtz(k)*dfh(k+1)
    c(1)=-dtz(k)*dfh(k+1)

    rhs   =  qcd(k)
    d(1)=sqc(k) + dtz(k)*flqc + rhs*delt

    DO k=kts+1,kte-1
       kk=k-kts+1
       a(kk)=-dtz(k)*dfh(k)
       b(kk)=1.+dtz(k)*(dfh(k)+dfh(k+1))
       c(kk)=-dtz(k)*dfh(k+1)

       rhs = qcd(k)
       d(kk)=sqc(k) + rhs*delt
    ENDDO


    a(nz)=0.
    b(nz)=1.
    c(nz)=0.
    d(nz)=sqc(kte)

    CALL tridiag(nz,a,b,c,d)

    DO k=kts,kte
       sqc(k)=d(k-kts+1)
    ENDDO

ENDIF













































    k=kts

    a(1)=0.
    b(1)=1.+dtz(k)*dfh(k+1)
    c(1)=-dtz(k)*dfh(k+1)
    d(1)=sqv(k) + dtz(k)*flqv + qcd(k)*delt

    DO k=kts+1,kte-1
       kk=k-kts+1
       a(kk)=-dtz(k)*dfh(k)
       b(kk)=1.+dtz(k)*(dfh(k)+dfh(k+1))
       c(kk)=-dtz(k)*dfh(k+1)
       d(kk)=sqv(k) + qcd(k)*delt
    ENDDO















    a(nz)=0.
    b(nz)=1.
    c(nz)=0.
    d(nz)=sqv(kte)

    CALL tridiag(nz,a,b,c,d)

    DO k=kts,kte
       sqv(k)=d(k-kts+1)
    ENDDO




IF (Cloudmix > 0.5 .AND. FLAG_QI) THEN

    k=kts

    a(1)=0.
    b(1)=1.+dtz(k)*dfh(k+1)
    c(1)=-dtz(k)*dfh(k+1)
    d(1)=sqi(k) + qcd(k)*delt 

    DO k=kts+1,kte-1
       kk=k-kts+1
       a(kk)=-dtz(k)*dfh(k)
       b(kk)=1.+dtz(k)*(dfh(k)+dfh(k+1))
       c(kk)=-dtz(k)*dfh(k+1)
       d(kk)=sqi(k) + qcd(k)*delt
    ENDDO















    a(nz)=0.
    b(nz)=1.
    c(nz)=0.
    d(nz)=sqi(kte)

    CALL tridiag(nz,a,b,c,d)

    DO k=kts,kte
       sqi(k)=d(k-kts+1)
    ENDDO

ENDIF




IF (Cloudmix > 0.5 .AND. FLAG_QNI) THEN

    k=kts

    a(1)=0.
    b(1)=1.+dtz(k)*dfh(k+1)
    c(1)=-dtz(k)*dfh(k+1)

    rhs = qcd(k)               

    d(1)=qni(k) 

    DO k=kts+1,kte-1
       kk=k-kts+1
       a(kk)=-dtz(k)*dfh(k)
       b(kk)=1.+dtz(k)*(dfh(k)+dfh(k+1))
       c(kk)=-dtz(k)*dfh(k+1)

       rhs = qcd(k)
       d(kk)=qni(k) + rhs*delt

    ENDDO


    a(nz)=0.
    b(nz)=1.
    c(nz)=0.
    d(nz)=qni(kte)

    CALL tridiag(nz,a,b,c,d)

    DO k=kts,kte
       qni2(k)=d(k-kts+1)
    ENDDO
ELSE
    qni2=qni
ENDIF






    DO k=kts,kte
       
       Dqv(k)=(sqv(k)/(1.-sqv(k))-qv(k))/delt
       
       
       Dqc(k)=(sqc(k)/(1.-sqc(k))-qc(k))/delt
       Dqi(k)=(sqi(k)/(1.-sqi(k))-qi(k))/delt
 
       Dqni(k)=(qni2(k)-qni(k))/delt
       Dth(k)=(thl(k) + xlvcp/exner(k)*sqc(k) &
         &            + xlscp/exner(k)*sqi(k) &
         &            - th(k))/delt
       
    ENDDO

  END SUBROUTINE mynn_tendencies


  SUBROUTINE retrieve_exchange_coeffs(kts,kte,&
       &dfm,dfh,dfq,dz,&
       &K_m,K_h,K_q)



    INTEGER , INTENT(in) :: kts,kte

    REAL, DIMENSION(KtS:KtE), INTENT(in) :: dz,dfm,dfh,dfq

    REAL, DIMENSION(KtS:KtE), INTENT(out) :: &
         &K_m, K_h, K_q


    INTEGER :: k
    REAL :: dzk

    K_m(kts)=0.
    K_h(kts)=0.
    K_q(kts)=0.

    DO k=kts+1,kte
       dzk = 0.5  *( dz(k)+dz(k-1) )
       K_m(k)=dfm(k)*dzk
       K_h(k)=dfh(k)*dzk
       K_q(k)=dfq(k)*dzk
    ENDDO

  END SUBROUTINE retrieve_exchange_coeffs


  SUBROUTINE tridiag(n,a,b,c,d)






    


    INTEGER, INTENT(in):: n
    REAL, DIMENSION(n), INTENT(in) :: a,b
    REAL, DIMENSION(n), INTENT(inout) :: c,d
    
    INTEGER :: i
    REAL :: p
    REAL, DIMENSION(n) :: q
    
    c(n)=0.
    q(1)=-c(1)/b(1)
    d(1)=d(1)/b(1)
    
    DO i=2,n
       p=1./(b(i)+a(i)*q(i-1))
       q(i)=-c(i)*p
       d(i)=(d(i)-a(i)*d(i-1))*p
    ENDDO
    
    DO i=n-1,1,-1
       d(i)=d(i)+q(i)*d(i+1)
    ENDDO

  END SUBROUTINE tridiag


  SUBROUTINE mynn_bl_driver(&
       &initflag,&
       &grav_settling,&
       &delt,&
       &dz,&
       &u,v,th,qv,qc,qi,qni,&
       &p,exner,rho,&
       &xland,ts,qsfc,qcg,ps,&
       &ust,ch,hfx,qfx,rmol,wspd,&
       &uoce,voce,&                     
       &vdfg,&                          
       &Qke,tke_pbl,&                   
       &qke_adv,bl_mynn_tkeadvect,&     
       &Tsq,Qsq,Cov,&
       &Du,Dv,Dth,&
       &Dqv,Dqc,Dqi,Dqni,& 

       &K_h,k_m,&
       &Pblh,kpbl&                      
       &,el_pbl&
       &,dqke,qWT,qSHEAR,qBUOY,qDISS    & 
       &,wstar,delta                    & 
       &,bl_mynn_tkebudget              & 
       &,bl_mynn_cloudpdf,Sh3D          & 
       
       &,FLAG_QI,FLAG_QNI,FLAG_QC,FLAG_QNC &
       &,IDS,IDE,JDS,JDE,KDS,KDE        &
       &,IMS,IME,JMS,JME,KMS,KME        &
       &,ITS,ITE,JTS,JTE,KTS,KTE)
    


    INTEGER, INTENT(in) :: initflag
    
    INTEGER, INTENT(in) :: grav_settling
    INTEGER, INTENT(in) :: bl_mynn_tkebudget
    INTEGER, INTENT(in) :: bl_mynn_cloudpdf
    LOGICAL, INTENT(IN) :: bl_mynn_tkeadvect

    LOGICAL, INTENT(IN) :: FLAG_QI,FLAG_QNI,FLAG_QC,FLAG_QNC
    
    INTEGER,INTENT(IN) :: &
         & IDS,IDE,JDS,JDE,KDS,KDE &
         &,IMS,IME,JMS,JME,KMS,KME &
         &,ITS,ITE,JTS,JTE,KTS,KTE
    







    
    REAL, INTENT(in) :: delt
    REAL, DIMENSION(IMS:IME,KMS:KME,JMS:JME), INTENT(in) :: dz,&
         &u,v,th,qv,qc,p,exner,rho
    REAL, DIMENSION(IMS:IME,KMS:KME,JMS:JME), OPTIONAL, INTENT(in)::&
         &qi,qni
    REAL, DIMENSION(IMS:IME,JMS:JME), INTENT(in) :: xland,ust,&


         &ch,rmol,ts,qsfc,qcg,ps,hfx,qfx, wspd,uoce,voce, vdfg


    REAL, DIMENSION(IMS:IME,KMS:KME,JMS:JME), INTENT(inout) :: &
         &Qke,Tsq,Qsq,Cov, &
         &tke_pbl, & 
         &qke_adv    

    REAL, DIMENSION(IMS:IME,KMS:KME,JMS:JME), INTENT(inout) :: &
         &Du,Dv,Dth,Dqv,Dqc,Dqi,Dqni

    REAL, DIMENSION(IMS:IME,KMS:KME,JMS:JME), INTENT(out) :: &
         &K_h,K_m

    REAL, DIMENSION(IMS:IME,JMS:JME), INTENT(inout) :: &
         &Pblh,wstar,delta  
    INTEGER,DIMENSION(IMS:IME,JMS:JME),INTENT(INOUT) :: & 
         &KPBL
    
    REAL, DIMENSION(IMS:IME,KMS:KME,JMS:JME), INTENT(inout) :: &
         &el_pbl


    REAL, DIMENSION(IMS:IME,KMS:KME,JMS:JME), INTENT(out) :: &
         &qWT,qSHEAR,qBUOY,qDISS,dqke
    
    
    REAL, DIMENSION(KTS:KTE) :: qWT1,qSHEAR1,qBUOY1,qDISS1,dqke1

    REAL, DIMENSION(IMS:IME,KMS:KME,JMS:JME) :: K_q,Sh3D


    INTEGER :: ITF,JTF,KTF, IMD,JMD
    INTEGER :: i,j,k
    REAL, DIMENSION(KTS:KTE) :: thl,sqv,sqc,sqi,sqw,&
         &El, Dfm, Dfh, Dfq, Tcd, Qcd, Pdk, Pdt, Pdq, Pdc, Vt, Vq

    REAL, DIMENSION(KTS:KTE) :: thetav,sh,u1,v1,p1,ex1,dz1,th1,qke1, &
           & tsq1,qsq1,cov1,qv1,qi1,qc1,du1,dv1,dth1,dqv1,dqc1,dqi1, &
           & k_m1,k_h1,k_q1,qni1,dqni1

    REAL, DIMENSION(KTS:KTE+1) :: zw
    
      REAL :: cpm,sqcg,flt,flq,flqv,flqc,pmz,phh,exnerg,zet,& 
              &afk,abk

   real,parameter    ::  d1 = 0.02, d2 = 0.05, d3 = 0.001
   real,parameter    ::  h1 = 0.33333335, h2 = 0.6666667
   REAL :: govrth, sflux, bfx0, wstar3, wm2, wm3, delb

    INTEGER, SAVE :: levflag


    IMD=(IMS+IME)/2
    JMD=(JMS+JME)/2


    JTF=MIN0(JTE,JDE-1)
    ITF=MIN0(ITE,IDE-1)
    KTF=MIN0(KTE,KDE-1)

    levflag=mynn_level

    IF (initflag > 0) THEN
 
       Sh3D(its:ite,kts:kte,jts:jte)=0.
       el_pbl(its:ite,kts:kte,jts:jte)=0.
       tsq(its:ite,kts:kte,jts:jte)=0.
       qsq(its:ite,kts:kte,jts:jte)=0.
       cov(its:ite,kts:kte,jts:jte)=0.

       DO j=JTS,JTF
          DO i=ITS,ITF
             DO k=KTS,KTF
                dz1(k)=dz(i,k,j)
                u1(k) = u(i,k,j)
                v1(k) = v(i,k,j)
                th1(k)=th(i,k,j)
                sqc(k)=qc(i,k,j)/(1.+qc(i,k,j))
                sqv(k)=qv(i,k,j)/(1.+qv(i,k,j))
                thetav(k)=th(i,k,j)*(1.+0.61*sqv(k))
                IF (PRESENT(qi) .AND. FLAG_QI ) THEN
                   sqi(k)=qi(i,k,j)/(1.+qi(i,k,j))
                   sqw(k)=sqv(k)+sqc(k)+sqi(k)
                   thl(k)=th(i,k,j)- xlvcp/exner(i,k,j)*sqc(k) &
                       &           - xlscp/exner(i,k,j)*sqi(k)
                ELSE
                   sqi(k)=0.0
                   sqw(k)=sqv(k)+sqc(k)
                   thl(k)=th(i,k,j)-xlvcp/exner(i,k,j)*sqc(k)
                ENDIF

                IF (k==kts) THEN
                   zw(k)=0.
                ELSE
                   zw(k)=zw(k-1)+dz(i,k-1,j)
                ENDIF

                k_m(i,k,j)=0.
                k_h(i,k,j)=0.
                k_q(i,k,j)=0.
                qke(i,k,j)=0.1-MIN(zw(k)*0.001, 0.0)
                qke1(k)=qke(i,k,j)
                el(k)=el_pbl(i,k,j)
                sh(k)=Sh3D(i,k,j)
                tsq1(k)=tsq(i,k,j)
                qsq1(k)=qsq(i,k,j)
                cov1(k)=cov(i,k,j)

                IF ( bl_mynn_tkebudget == 1) THEN
                   
                   qWT(i,k,j)=0.
                   qSHEAR(i,k,j)=0.
                   qBUOY(i,k,j)=0.
                   qDISS(i,k,j)=0.
                   dqke(i,k,j)=0.
                ENDIF
             ENDDO

             zw(kte+1)=zw(kte)+dz(i,kte,j)
             
             CALL GET_PBLH(KTS,KTE,PBLH(i,j),thetav,&
               &  Qke1,zw,dz1,xland(i,j),KPBL(i,j))

             CALL mym_initialize ( kts,kte,&
                  &dz1, zw, u1, v1, thl, sqv,&
                  &PBLH(i,j),th1,&                      
                  &sh,&                                 
                  &ust(i,j), rmol(i,j),&
                  &el, Qke1, Tsq1, Qsq1, Cov1)

             
             DO k=KTS,KTE 
                el_pbl(i,k,j)=el(k)
                sh3d(i,k,j)=sh(k)
                qke(i,k,j)=qke1(k)
                tsq(i,k,j)=tsq1(k)
                qsq(i,k,j)=qsq1(k)
                cov(i,k,j)=cov1(k)

                IF (bl_mynn_tkeadvect) THEN
                   qke_adv(i,k,j)=qke1(k)
                ENDIF

             ENDDO












          ENDDO
       ENDDO

    ENDIF 


    IF (bl_mynn_tkeadvect) THEN
       qke=qke_adv
    ENDIF


    DO j=JTS,JTF
       DO i=ITS,ITF
          DO k=KTS,KTF
             
             IF ( bl_mynn_tkebudget == 1) THEN
                dqke(i,k,j)=qke(i,k,j)
             END IF
             dz1(k)= dz(i,k,j)
             u1(k) = u(i,k,j)
             v1(k) = v(i,k,j)
             th1(k)= th(i,k,j)
             qv1(k)= qv(i,k,j)
             qc1(k)= qc(i,k,j)
             sqv(k)= qv(i,k,j)/(1.+qv(i,k,j))
             sqc(k)= qc(i,k,j)/(1.+qc(i,k,j))
             IF(PRESENT(qi) .AND. FLAG_QI)THEN
                qi1(k)= qi(i,k,j)
                sqi(k)= qi(i,k,j)/(1.+qi(i,k,j))
                sqw(k)= sqv(k)+sqc(k)+sqi(k)
                thl(k)= th(i,k,j) - xlvcp/exner(i,k,j)*sqc(k) &
                     &            - xlscp/exner(i,k,j)*sqi(k)
                
             ELSE
                qi1(k)=0.0
                sqi(k)=0.0
                sqw(k)= sqv(k)+sqc(k)
                thl(k)= th(i,k,j)-xlvcp/exner(i,k,j)*sqc(k)
             ENDIF
             IF (PRESENT(qni) .AND. FLAG_QNI ) THEN
                qni1(k)=qni(i,k,j)
                
             ELSE
                qni1(k)=0.0
             ENDIF
             
             
             
             
             
             
             thetav(k)=th(i,k,j)*(1.+0.608*sqv(k))
             p1(k) = p(i,k,j)
             ex1(k)= exner(i,k,j)
             el(k) = el_pbl(i,k,j)
             qke1(k)=qke(i,k,j)
             sh(k) = sh3d(i,k,j)
             tsq1(k)=tsq(i,k,j)
             qsq1(k)=qsq(i,k,j)
             cov1(k)=cov(i,k,j)

             IF (k==kts) THEN
                zw(k)=0.
             ELSE
                zw(k)=zw(k-1)+dz(i,k-1,j)
             ENDIF
          ENDDO

          zw(kte+1)=zw(kte)+dz(i,kte,j)          
          
          CALL GET_PBLH(KTS,KTE,PBLH(i,j),thetav,&
          & Qke1,zw,dz1,xland(i,j),KPBL(i,j))
          
          sqcg= 0.0   
          cpm=cp*(1.+0.84*qv(i,kts,j))
          exnerg=(ps(i,j)/p1000mb)**rcp

          
          
          
          
          
          
          
          
          
          flt = hfx(i,j)/( rho(i,kts,j)*cpm ) &
            & +xlvcp*vdfg(i,j)*(sqc(kts)/exner(i,kts,j)- sqcg/exnerg)
          flq = qfx(i,j)/  rho(i,kts,j)       &
            & -vdfg(i,j)*(sqc(kts) - sqcg )
          flqv = qfx(i,j)/rho(i,kts,j)
          flqc = -vdfg(i,j)*(sqc(kts) - sqcg )

          zet = 0.5*dz(i,kts,j)*rmol(i,j)
          if ( zet >= 0.0 ) then
            pmz = 1.0 + (cphm_st-1.0) * zet
            phh = 1.0 +  cphh_st      * zet
          else
            pmz = 1.0/    (1.0-cphm_unst*zet)**0.25 - zet
            phh = 1.0/SQRT(1.0-cphh_unst*zet)
          end if


          govrth = g/th1(kts)
          sflux = hfx(i,j)/rho(i,kts,j)/cpm + &
                  qfx(i,j)/rho(i,kts,j)*ep_1*th1(kts)
          bfx0 = max(sflux,0.)
          wstar3     = (govrth*bfx0*pblh(i,j))
          wstar(i,j) = wstar3**h1
          wm3        = wstar3 + 5.*ust(i,j)**3.
          wm2        = wm3**h2
          delb       = govrth*d3*pblh(i,j)
          delta(i,j) = min(d1*pblh(i,j) + d2*wm2/delb, 100.)


          CALL  mym_condensation ( kts,kte,&
               &dz1,thl,sqw,p1,ex1, &
               &tsq1, qsq1, cov1, &
               &Sh,el,bl_mynn_cloudpdf, & 
               &Vt, Vq)

          CALL mym_turbulence ( kts,kte,levflag, &
               &dz1, zw, u1, v1, thl, sqc, sqw, &
               &qke1, tsq1, qsq1, cov1, &
               &vt, vq,&
               &rmol(i,j), flt, flq, &
               &PBLH(i,j),th1,&                  
               &Sh,&                             
               &el,&  
               &Dfm,Dfh,Dfq, &
               &Tcd,Qcd,Pdk, &
               &Pdt,Pdq,Pdc &
               &,qWT1,qSHEAR1,qBUOY1,qDISS1  &   
               &,bl_mynn_tkebudget           &   
               &)

          CALL mym_predict (kts,kte,levflag,  &
               &delt, dz1, &
               &ust(i,j), flt, flq, pmz, phh, &
               &el, dfq, pdk, pdt, pdq, pdc, &
               &Qke1, Tsq1, Qsq1, Cov1)

          CALL mynn_tendencies(kts,kte,&
               &levflag,grav_settling,&
               &delt, dz1,&
               &u1, v1, th1, qv1, qc1, qi1, qni1,&
               &p1, ex1, thl, sqv, sqc, sqi, sqw,&
               &ust(i,j),flt,flq,flqv,flqc,wspd(i,j),qcg(i,j),&
               &uoce(i,j),voce(i,j),&
               &tsq1, qsq1, cov1,&
               &tcd, qcd, &
               &dfm, dfh, dfq,&
               &Du1, Dv1, Dth1, Dqv1, Dqc1, Dqi1, Dqni1& 
               &,vdfg(i,j)&                 
               &,FLAG_QI,FLAG_QNI,FLAG_QC,FLAG_QNC &
               &)

                
                

          CALL retrieve_exchange_coeffs(kts,kte,&
               &dfm, dfh, dfq, dz1,&
               &K_m1, K_h1, K_q1)

          
          DO k=KTS,KTF
             K_m(i,k,j)=K_m1(k)
             K_h(i,k,j)=K_h1(k)
             K_q(i,k,j)=K_q1(k)
             du(i,k,j)=du1(k)
             dv(i,k,j)=dv1(k)
             dth(i,k,j)=dth1(k)
             dqv(i,k,j)=dqv1(k)
             dqc(i,k,j)=dqc1(k)
             IF (PRESENT(qi) .AND. FLAG_QI) dqi(i,k,j)=dqi1(k)
             
             IF (PRESENT(qni) .AND. FLAG_QNI) dqni(i,k,j)=dqni1(k)
             el_pbl(i,k,j)=el(k)
             qke(i,k,j)=qke1(k)
             tsq(i,k,j)=tsq1(k)
             qsq(i,k,j)=qsq1(k)
             cov(i,k,j)=cov1(k)
             sh3d(i,k,j)=sh(k)
             IF ( bl_mynn_tkebudget == 1) THEN
                dqke(i,k,j)  = (qke1(k)-dqke(i,k,j))*0.5  
                qWT(i,k,j)   = qWT1(k)*delt
                qSHEAR(i,k,j)= qSHEAR1(k)*delt
                qBUOY(i,k,j) = qBUOY1(k)*delt
                qDISS(i,k,j) = qDISS1(k)*delt
             ENDIF
             













             
          ENDDO


          tke_pbl(i,kts,j) = 0.5*MAX(qke(i,kts,j),1.0e-10)
          DO k = kts+1,kte
             afk = dz1(k)/( dz1(k)+dz1(k-1) )
             abk = 1.0 -afk
             tke_pbl(i,k,j) = 0.5*MAX(qke(i,k,j)*abk+qke(i,k-1,j)*afk,1.0e-3)
          ENDDO















       ENDDO
    ENDDO


    IF (bl_mynn_tkeadvect) THEN
       qke_adv=qke
    ENDIF

    
  END SUBROUTINE mynn_bl_driver


  SUBROUTINE mynn_bl_init_driver(&
       &Du,Dv,Dth,Dqv,Dqc,Dqi                       &
       
       &,QKE,TKE_PBL,EXCH_H                         &
       &,RESTART,ALLOWED_TO_READ,LEVEL              &
       &,IDS,IDE,JDS,JDE,KDS,KDE                    &
       &,IMS,IME,JMS,JME,KMS,KME                    &
       &,ITS,ITE,JTS,JTE,KTS,KTE)

    
    LOGICAL,INTENT(IN) :: ALLOWED_TO_READ,RESTART
    INTEGER,INTENT(IN) :: LEVEL

    INTEGER,INTENT(IN) :: IDS,IDE,JDS,JDE,KDS,KDE,                    &
         &                IMS,IME,JMS,JME,KMS,KME,                    &
         &                ITS,ITE,JTS,JTE,KTS,KTE
    
    
    REAL,DIMENSION(IMS:IME,KMS:KME,JMS:JME),INTENT(INOUT) :: &
         &Du,Dv,Dth,Dqv,Dqc,Dqi, & 
         &QKE,TKE_PBL,EXCH_H

    INTEGER :: I,J,K,ITF,JTF,KTF
    
    JTF=MIN0(JTE,JDE-1)
    KTF=MIN0(KTE,KDE-1)
    ITF=MIN0(ITE,IDE-1)
    
    IF(.NOT.RESTART)THEN
       DO J=JTS,JTF
          DO K=KTS,KTF
             DO I=ITS,ITF
                Du(i,k,j)=0.
                Dv(i,k,j)=0.
                Dth(i,k,j)=0.
                Dqv(i,k,j)=0.
                if( p_qc >= param_first_scalar ) Dqc(i,k,j)=0.
                if( p_qi >= param_first_scalar ) Dqi(i,k,j)=0.
                
                
                QKE(i,k,j)=0.
                TKE_PBL(i,k,j)=0.
                EXCH_H(i,k,j)=0.
             ENDDO
          ENDDO
       ENDDO
    ENDIF

    mynn_level=level

  END SUBROUTINE mynn_bl_init_driver



  SUBROUTINE GET_PBLH(KTS,KTE,zi,thetav1D,qke1D,zw1D,dz1D,landsea,kzi)

    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    

    INTEGER,INTENT(IN) :: KTS,KTE
    REAL, INTENT(OUT) :: zi
    REAL, INTENT(IN) :: landsea
    REAL, DIMENSION(KTS:KTE), INTENT(IN) :: thetav1D, qke1D, dz1D
    REAL, DIMENSION(KTS:KTE+1), INTENT(IN) :: zw1D
    
    REAL ::  PBLH_TKE,qtke,qtkem1,wt,maxqke,TKEeps,minthv
    REAL :: delt_thv   
    REAL, PARAMETER :: sbl_lim  = 200. 
    REAL, PARAMETER :: sbl_damp = 400. 
    INTEGER :: I,J,K,kthv,ktke,kzi,kzi2

    
    
    kzi = 1
    kzi2= 1

    
    k = kts+1
    kthv = 1
    ktke = 1
    maxqke = 0.
    minthv = 9.E9
    DO WHILE (zw1D(k) .LE. 500.)
       qtke  =MAX(Qke1D(k),0.)   
       IF (maxqke < qtke) then
           maxqke = qtke
           ktke = k
       ENDIF
       IF (minthv > thetav1D(k)) then
           minthv = thetav1D(k)
           kthv = k
       ENDIF
       k = k+1
    ENDDO
    
    TKEeps = maxqke/40. 
    TKEeps = MAX(TKEeps,0.025)

    
    zi=0.
    k = kthv+1
    IF((landsea-1.5).GE.0)THEN                                            
        
        delt_thv = 0.75
    ELSE         
        
        delt_thv = 1.25
    ENDIF

    zi=0.
    k = kthv+1
    DO WHILE (zi .EQ. 0.) 
       IF (thetav1D(k) .GE. (minthv + delt_thv))THEN
          kzi = MAX(k-1,1)
          zi = zw1D(k) - dz1D(k-1)* &
             & MIN((thetav1D(k)-(minthv + delt_thv))/ &
             & MAX(thetav1D(k)-thetav1D(k-1),1E-6),1.0)
       ENDIF
       k = k+1
       IF (k .EQ. kte-1) zi = zw1D(kts+1) 
    ENDDO
    

    
    
    
    

    PBLH_TKE=0.
    k = ktke+1
    DO WHILE (PBLH_TKE .EQ. 0.) 
       
       qtke  =MAX(Qke1D(k)/2.,0.)      
       qtkem1=MAX(Qke1D(k-1)/2.,0.)
       IF (qtke .LE. TKEeps) THEN
           kzi2 = MAX(k-1,1)
           PBLH_TKE = zw1D(k) - dz1D(k-1)* &
             & MIN((TKEeps-qtke)/MAX(qtkem1-qtke, 1E-6), 1.0)
           
           PBLH_TKE = MAX(PBLH_TKE,zw1D(kts+1))
           
       ENDIF
       k = k+1
       IF (k .EQ. kte-1) PBLH_TKE = zw1D(kts+1) 
    ENDDO

    
    
    
    
    
    
    PBLH_TKE = MIN(PBLH_TKE,zi+500.)

    
    wt=.5*TANH((zi - sbl_lim)/sbl_damp) + .5
    zi=PBLH_TKE*(1.-wt) + zi*wt

    
     kzi = INT(kzi2*(1.-wt) + kzi*wt)

  END SUBROUTINE GET_PBLH
  


END MODULE module_bl_mynn
