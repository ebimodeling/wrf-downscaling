




























MODULE module_bl_gbmpbl

   USE module_model_constants, ONLY: cp, g, rcp, r_d,            &
                                     r_v, svp1, svp2, svp3,      &
                                     svpt0, ep_1, ep_2, xlv,     &
                                     karman
public gbmpbl
public gbmpblinit
private

CONTAINS

  SUBROUTINE GBMPBL(U3D,V3D,TH3D,T3D,QV3D,QC3D,QI3D,P3D,PI3D,     &
       RUBLTEN,RVBLTEN,RTHBLTEN,                                  &
       RQVBLTEN,RQCBLTEN,RQIBLTEN,                                & 
       KZM_GBM,KTH_GBM,KETHL_GBM,EL_GBM,                          &
       dz8w,z,PSFC,TKE_PBL,RTHRATEN,                              &
       ZNT,UST,ZOL,HOL,PBL,KPBL2D,REGIME,PSIM,PSIH,               & 
       XLAND,HFX,QFX,TSK,GZ1OZ0,WSPD,BR,                          &
       DT,DTMIN,                                                  &
       ids,ide, jds,jde, kds,kde,                                 &
       ims,ime, jms,jme, kms,kme,                                 &
       its,ite, jts,jte, kts,kte                        )
    
    IMPLICIT NONE
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    

    INTEGER,  INTENT(IN   )   ::      ids,ide, jds,jde, kds,kde, &
         ims,ime, jms,jme, kms,kme, &
         its,ite, jts,jte, kts,kte

    REAL,     INTENT(IN   )   ::      DT,DTMIN

    REAL,     DIMENSION( ims:ime, kms:kme, jms:jme )           , &
         INTENT(IN   )   ::                           QV3D, &
         QC3D, &
         QI3D, &
         P3D, &
         PI3D, &
         TH3D, &
         T3D, &
         dz8w, &
         z   

    
    REAL,     DIMENSION( ims:ime, kms:kme, jms:jme )           , &
         INTENT(INOUT)   ::                          RUBLTEN, &
         RVBLTEN, &
         RTHBLTEN, &
         RQVBLTEN, &
         RQCBLTEN, &
         RQIBLTEN

    REAL,     DIMENSION( ims:ime, kms:kme, jms:jme )           , &
         INTENT(OUT)   ::   KZM_GBM,KTH_GBM,KETHL_GBM, EL_GBM

    REAL,     DIMENSION( ims:ime, jms:jme )                    , &
         INTENT(IN   )   ::                          XLAND, &
         HFX, &
         QFX, &
         REGIME

    REAL,     DIMENSION( ims:ime, jms:jme )                    , &
         INTENT(INOUT)   ::                            HOL, &
         UST, &
         PBL, &
         ZNT
    INTEGER,  DIMENSION( ims:ime, jms:jme )                    , &
         INTENT(INOUT)   ::                            KPBL2D

    


     REAL,     DIMENSION( ims:ime, jms:jme ), INTENT(IN   )  ::    &
                                                             PSIM, &
                                                             PSIH

     REAL,     DIMENSION( ims:ime, jms:jme ), INTENT(INOUT)   ::   &
                                                             WSPD

     REAL,     DIMENSION( ims:ime, jms:jme ), INTENT(IN   )   ::   &
                                                           GZ1OZ0, &
                                                               BR

      REAL,     DIMENSION( ims:ime, jms:jme )                    , &
                INTENT(IN   )               ::               PSFC

      REAL,     DIMENSION( ims:ime, jms:jme )                    , &
                INTENT(IN   )   ::                            TSK

      REAL,     DIMENSION( ims:ime, jms:jme )                    , &
                INTENT(INOUT)   ::                            ZOL
                                                                      
      REAL,     DIMENSION( ims:ime, kms:kme, jms:jme )           , &
                INTENT(IN   )   ::                            U3D, &
                                                              V3D

    REAL,DIMENSION(IMS:IME,KMS:KME,JMS:JME)                          &
         ,INTENT(INOUT) ::                    TKE_PBL
    REAL,DIMENSION(IMS:IME,KMS:KME,JMS:JME)                          &
         ,INTENT(IN) ::                    RTHRATEN

    

    REAL,       DIMENSION( its:ite, kts:kte )          ::   dz8w2d, & 
         z2d,t2dten_ra

    INTEGER ::  I,J,K,NK,pass
    CHARACTER(LEN=200)        :: string
    REAL,DIMENSION(IMS:IME,KMS:KME)   :: TKE2d_1,TKE2d_2 ,          &
                                         u2dblten_1,u2dblten_2,     &
                                         v2dblten_1,v2dblten_2

    tke2d_1 = 0.0 
    tke2d_2 = 0.0 
    u2dblten_1 = 0.0 
    u2dblten_2 = 0.0 
    v2dblten_1 = 0.0 
    v2dblten_2 = 0.0 
    t2dten_ra  = 0.0 
    
    DO J=jts,jte
       DO k=kts,kte
          NK=kme-k
          DO i=its,ite
             dz8w2d(I,K) = dz8w(i,K,j)
             z2d(I,K) = z(i,NK,j)
             t2dten_ra(i,k) = RTHRATEN(i,k,j)*PI3D(I,K,J)
             


             tke2d_2(i,k)= TKE_PBL(i,k,j) 
          ENDDO
       ENDDO

    do pass=1,2
       CALL GBM2D(J,U3D(ims,kms,j),V3D(ims,kms,j),T3D(ims,kms,j),&
            QV3D(ims,kms,j),QC3D(ims,kms,j),QI3D(ims,kms,j),     &
            P3D(ims,kms,j),u2dblten_2(ims,kms),v2dblten_2(ims,kms),&
            RTHBLTEN(ims,kms,j),RQVBLTEN(ims,kms,j),             &
            RQCBLTEN(ims,kms,j),RQIBLTEN(ims,kms,j),             &
            KZM_GBM(ims,kms,j),KTH_GBM(ims,kms,j),               &
            KETHL_GBM(ims,kms,j),EL_GBM(ims,kms,j),              &
            TKE2d_2(ims,kms),t2dten_ra,                          &
            dz8w2d,z2d,                                          &
            PSFC(ims,j),ZNT(ims,j),UST(ims,j),ZOL(ims,j),        &
            HOL(ims,j),PBL(ims,j),KPBL2D(ims,j),PSIM(ims,j),     &
            PSIH(ims,j),XLAND(ims,j),HFX(ims,j),QFX(ims,j),      &
            TSK(ims,j),GZ1OZ0(ims,j),WSPD(ims,j),BR(ims,j),      &
            DT,DTMIN,                                            &
            ids,ide, jds,jde, kds,kde,                           &
            ims,ime, jms,jme, kms,kme,                           &
            its,ite, jts,jte, kts,kte                            )
      if(pass==1)then
        TKE2d_1=tke2d_2 
        u2dblten_1=u2dblten_2
        v2dblten_1=v2dblten_2
      end if
      if(pass==2)then

        TKE_PBL(:,:,j)=tke2d_1 
        rublten(:,:,j)=0.5*(u2dblten_2+u2dblten_1)
        rvblten(:,:,j)=0.5*(v2dblten_2+v2dblten_1)
      end if
    end do


       DO k=kts,kte
          DO i=its,ite
             RTHBLTEN(I,K,J)=RTHBLTEN(I,K,J)/PI3D(I,K,J)
          ENDDO
       ENDDO
    ENDDO

  END SUBROUTINE GBMPBL

  SUBROUTINE GBM2D(J,U2D,V2D,T2D,QV2D,QC2D,QI2D,P2D,    &
       U2DTEN,V2DTEN,T2DTEN,                            &
       QV2DTEN,QC2DTEN,QI2DTEN,                         & 
       KZM2D,KTH2D,KETHL2D,EL2D,                        &
       TKE2D,T2DTEN_RA,                                 &
       dz8w2d,z2d,PSFCPA,                               &
       ZNT,UST,ZOL,HOL,PBL,kpbl1d,PSIM,PSIH,            &
       XLAND,HFX,QFX,TSK,GZ1OZ0,WSPD,BR,                &
       DT,DTMIN,                                        &
       ids,ide, jds,jde, kds,kde,                       &
       ims,ime, jms,jme, kms,kme,                       &
       its,ite, jts,jte, kts,kte                        )
    
    implicit none
    
    INTEGER,  INTENT(IN   )   ::      ids,ide, jds,jde, kds,kde, &
         ims,ime, jms,jme, kms,kme, &
         its,ite, jts,jte, kts,kte,J
    
    REAL,     INTENT(IN   )   ::      DT,DTMIN
    REAL                      ::     SVP1PA

    REAL,     DIMENSION( ims:ime, kms:kme )                    , &
         INTENT(IN   )   ::                           QV2D, &
         QC2D, &
         QI2D, &
         P2D, &
         T2D

    
    REAL,     DIMENSION( ims:ime, kms:kme )                    , &
         INTENT(INOUT)   ::                         U2DTEN, &
         V2DTEN, &
         T2DTEN, &
         QV2DTEN, &
         QC2DTEN, &
         QI2DTEN, &
         tke2d
    
    REAL,     DIMENSION( ims:ime, kms:kme )                    , &
         INTENT(OUT)   ::    KZM2d,KTH2d,KETHL2d,EL2D

    REAL,     DIMENSION( ims:ime )                             , &
         INTENT(INOUT)   ::                            HOL, &
         UST, &
         PBL, &
         ZNT

    INTEGER,  DIMENSION( ims:ime )                             , &
         INTENT(INOUT)   ::                            kpbl1d

    REAL,     DIMENSION( ims:ime )                             , &
         INTENT(IN   )   ::                          XLAND, &
         HFX, &
         QFX
    
    REAL,     DIMENSION( ims:ime ), INTENT(IN   )   ::      PSIM, &
         PSIH

    REAL,     DIMENSION( ims:ime ), INTENT(INOUT)   ::      WSPD

    REAL,     DIMENSION( ims:ime ), INTENT(IN   )   ::    GZ1OZ0, &
         BR

    REAL,     DIMENSION( ims:ime )                             , &
         INTENT(IN   )               ::             PSFCPA

    REAL,     DIMENSION( ims:ime )                             , &
         INTENT(IN   )   ::                            TSK

    REAL,     DIMENSION( ims:ime )                             , &
         INTENT(INOUT)   ::                            ZOL

    
    REAL,     DIMENSION( its:ite, kts:kte ) ,                    &
         INTENT(IN)      ::                         dz8w2d, &
         z2d,t2dten_ra
    
    REAL,     DIMENSION( ims:ime, kms:kme )                    , &
         INTENT(IN   )   ::                            U2D, &
         V2D
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    real zqx(kts:kte+2)
    REAL KTH(KTS:KTE+1),KZM(KTS:KTE+1),RHOXFL(KTS:KTE+1),tke(kts:kte+1),tkes(kts:kte+1), &
         rrhoxfl(kts:kte+1),BBLS(KTS:KTE+1),NSQUAR(KTS:KTE+1),BOUYAN(KTS:KTE+1), &
         DQWDZ(kts:kte+1),rdza(kts:kte+1),dza(kts:kte+1),SVS(KTS:KTE+1),presfl(kts:kte+1), &
         exnerfl(kts:kte+1),SHEAR(KTS:KTE+1),rexnerfl(kts:kte+1),rcldb(kts:kte+1), &
         epop(kts:kte+1),DTLDZ(KTS:KTE+1)
    
    REAL UX(KTS:KTE),VX(KTS:KTE),THX(KTS:KTE),QX(KTS:KTE),THVX(KTS:KTE),zax(kts:kte),qix(kts:kte), &
         KETHL(KTS:KTE),THLX(KTS:KTE),THXS(KTS:KTE),tx(kts:kte),tvx(kts:kte),rttenx(kts:kte), &
         PRESHL(KTS:KTE),QCX(KTS:KTE),QWX(KTS:KTE),dzq(kts:kte),rRHOXHL(KTS:KTE),UXS(KTS:KTE), &
         QXS(KTS:KTE),RHOXHL(KTS:KTE),exnerhl(kts:kte),rexnerhl(kts:kte),rdzq(kts:kte), &
         VXS(KTS:KTE),qixs(kts:kte),qcxs(kts:kte)
    REAL,     DIMENSION( its:ite ) :: wspd1
    REAL UFLXP,VFLXP,RHOXSF,Q0S, &
         RDT,dt2, &
         aone,atwo,czero,tskx, &
         tvcon,fracz,dudz,dvdz,rvls,thv0,dthv,xfr, &
         cpoxlv,r1cp,rczero, &
         templ,temps,gamcrt,gamcrq,cell, &
         rwspd,cfac, &
         thgb,pblx,gothv,capcd,capch,capcq, &
         ustx,ustxm,qfxx,hfxx,rlwp,tkeavg,wstar,xlvocp,wso,phih, &
         rstbl,vk,tbbls,pref
    integer i,k,l,iconv,ilay, &
         ktop(kts:kte),kpbl2dx,kmix2dx, &
         iteration,pass,kr,&
	 ktop_save(kts:kte) 
    
    real aimp(kts:kte),bimp(kts:kte),cimp(kts:kte)
    real uimp1(kts:kte),rimp1(kts:kte)
    real uimp2(kts:kte),rimp2(kts:kte)
    
    real  THX_t,THVX_t,DTHV_t
    
    parameter(XFR=0.1)        

    parameter(aone=1.9*xfr,atwo=10.,cfac=7.8) 
    parameter(czero=5.869)    
    parameter(rstbl=1.5)      
    PARAMETER (GAMCRT=3.,GAMCRQ=2.E-3)
    parameter(vk=0.4)
    parameter(pref = 100000.)
    
    
    
    
    RDT=1./DT 
    dt2=dt 
    SVP1PA=svp1*10. 
    cpoxlv = cp/xlv
    xlvocp = xlv/cp
    r1cp = 1./cp
    rczero = 1./czero

    
    
    
    
    
    DO I=its,ite
       
       tskx = tsk(i)
       pblx = pbl(i)
       qfxx = qfx(i)
       hfxx = hfx(i)
       zqx(kte+1)=0.0
       zqx(kte+1+1)=0.0
       tke(kte+1) = 0.0
       DO K = kte,kts,-1
          KR = kte+1-K
          zqx(k)= zqx(k+1) + dz8w2d(i,kr)
          zax(k)=0.5*(zqx(k)+zqx(k+1))
          tke(k) = tke2d(i,kr)
          
          UX(K)=U2D(I,KR)  
          VX(K)=V2D(I,KR)  
          TX(K)=T2D(I,KR)
          QX(K)=QV2D(I,KR)
          QCX(K)=QC2D(I,KR)
          qix(k)=qi2D(i,kr)
          rttenx(k)=t2dten_ra(i,kr)
       ENDDO
       
       tkes(kte+1) = tke(kte+1)
       do k = kts,kte
          kr = kte+1-k
          
          PRESHL(K)=p2d(i,kr)
          DZQ(K)=ZQX(K)-ZQX(K+1)
          rdzq(k)=1./dzq(k)
          exnerhl(k)=(preshl(k)/pref)**rcp
          rexnerhl(k)=1./exnerhl(k)
          THX(K)=TX(K)*rexnerhl(k)
          QWX(K) = QX(K) + QCX(K)
          TVCON=(1.+ep_1*QX(K)-qcx(k))
          TVX(K)=TX(K)*TVCON  
          THVX(K)=THX(K)*TVCON
          THLX(K) = THX(K) - XLVOCP * rexnerhl(K) * QCX(K)
          
          tkes(k) = tke(k)
          UXS(K)  = UX(K)
          VXS(K)  = VX(K)
          THXS(K) = THX(K)
          QXS(K) = QX(K)
          qixs(k) = qix(k)
          qcxs(k) = qcx(k)
          
          RHOXHL(K)=PRESHL(K)/(r_d*TVX(K))
          rrhoxhl(k)=1./rhoxhl(k)
       enddo
       
       presfl(1)=0.
       DO K = 2, kte
          kr = kte+1-k
          
          PRESFL(K)=exp(0.5*(log(p2d(i,kr+1))+log(p2d(i,kr))))
          epop(k)=ep_2/presfl(k)
          DZA(K)=ZAX(K-1)-ZAX(K)
          rdza(k)=1./dza(k)
          exnerfl(k)=(presfl(k)/pref)**rcp
          rexnerfl(k)=1./exnerfl(k)
          FRACZ=(ZQX(K)-ZAX(K))*RDZA(K)
          RHOXFL(K)=RHOXHL(K)+(RHOXHL(K-1)-RHOXHL(K))*FRACZ
          RRHOXFL(K)=1./RHOXFL(K)
          DUDZ =  (UX(K-1)  - UX(K))  *RDZA(K)
          DVDZ =  (VX(K-1)  - VX(K))  *RDZA(K)
          SVS(K)= DUDZ*DUDZ + DVDZ*DVDZ
          DQWDZ(K) =  (QWX(K-1)  - QWX(K))  *RDZA(K)
          DTLDZ(K) =  (THLX(K-1)  - THLX(K))  *RDZA(K)
       ENDDO


       
       PRESFL(KTE+1)=psfcpa(i)
       rexnerfl(kte+1)=(pref/presfl(kte+1))**rcp
       exnerfl(kte+1)=1./rexnerfl(kte+1)
       
       q0s=ep_2/(PRESFL(KTE+1)/(100.*SVP1PA*EXP(svp2*(tskx-svpt0)/(tskx-svp3)))-1.)
       
       THGB = TSKX * rexnerfl(kte+1)
       
       RHOXSF=(PRESFL(KTE+1))/(r_d*TVX(kte))
       
       WSPD1(i)=SQRT(UX(KTE)*UX(KTE)+VX(KTE)*VX(KTE))+1.e-4
       THV0=THGB*(1.+ep_1*Q0S)
       DTHV=(THVX(KTE)-THV0)
       gothv = g/thvx(kte)
       
       ustx = wspd1(i)*vk/(gz1oz0(i)-psim(i))
       ustxm = amax1(ustx,0.1)
       if ((xland(i)-1.5).lt.0)  ustx = ustxm   
       
       
       
       
       
       call n2(thlx,exnerfl,epop,qwx,cpoxlv,rdza,            &
            rexnerfl,kts,kte,nsquar,rcldb,xlvocp,svp1pa)
       nsquar(kte+1)=  g/thvx(kte) * dthv / zax(kte)
       
       
       
       
       call pblhgt( &
            
       zqx,kts,kte, &
            nsquar,tke,presfl,rhoxfl,rcldb,exnerfl, &
            rttenx,thvx,thlx,thx,qwx,rexnerhl,qcx, &
            xfr,xlvocp,aone,atwo,rstbl,            &
            
       bbls,pblx, &
            ktop,iconv,kmix2dx,kpbl2dx)
       
       
       
       
       do pass = 1,2
          call my( &
               
          bbls,nsquar,tke,iconv,ktop,thlx,thx,qwx,     &
               xlvocp,rcldb,rexnerhl,aone,atwo,kts,kte, &
               
          kzm,kth,kethl )
          
          
          
          
          
          do k = 1, kte
             if(k.eq.1)then
                aimp(k)=0.
             else
                aimp(k)=-(rhoxfl(k)*rrhoxhl(k))* &
                     kth(k) * dt2 *rdzq(k)*rdza(k)
             endif
             if(k.eq.kte)then
                cimp(k)=0.
             else
                cimp(k)=-(rhoxfl(k+1)*rrhoxhl(k))* &
                     kth(k+1) * dt2 *rdzq(k)*rdza(k+1)
             endif
             bimp(k) = 1 - aimp(k) - cimp(k)
             
             
             if(k.eq.kte)then  
                
                rimp2(k) = qwx(k) + dt2 * qfxx*rrhoxhl(k)*rdzq(kte)
                
                rimp1(k) = thlx(k) + &
                     dt2 * hfxx*rrhoxhl(k)*r1cp*rdzq(kte)*rexnerhl(kte)
             else
                rimp2(k) = qwx(k)
                rimp1(k) = thlx(k)
             endif
          enddo
          call tridag(aimp,bimp,cimp,rimp1,uimp1,kte)
          call tridag(aimp,bimp,cimp,rimp2,uimp2,kte)
          
          
          
          
          call n2(uimp1,exnerfl,epop,uimp2,cpoxlv,rdza,       &
	       rexnerfl, kts,kte,nsquar,rcldb,xlvocp,svp1pa)
	       
               
               THX_t = uimp1(KTE) + XLVOCP * rexnerhl(KTE) * QCX(KTE)
               THVX_t=THX_t*TVCON
               DTHV_t=(THVX_t-THV0)
               nsquar(kte+1)=  g/thvx_t * dthv_t / zax(kte)
	       
          
       
       
       
       
       









       
       enddo
       
       
       
       
       
       do  k = 1, kte
          thlx(k) = uimp1(k)
          qwx(k) = uimp2(k)
          if(k.ge.2) DQWDZ(K) = (QWX(K-1) - QWX(K)) *RDZA(K)
          if(k.ge.2) DTLDZ(K) = (THLX(K-1) - THLX(K)) *RDZA(K)
          templ = thlx(k)*exnerhl(k)
          temps = templ
          rvls = ep_2/ &
               (preshl(k)/(100.*svp1pa*EXP(svp2*(temps-svpt0)/(temps-svp3)))-1.)
          do iteration = 1, 3
             temps = temps  + ((templ-temps)*cp/xlv + qwx(k)-rvls)/ &
                  (cp/xlv+ep_2*xlv*rvls/r_d/temps/temps)
             rvls = ep_2/ &
                  (preshl(k)/(100.*svp1pa*EXP(svp2*(temps-svpt0)/(temps-svp3)))-1.)
          enddo
          qcx(k)=max(qwx(k)-rvls,0.)
          qx(k) = qwx(k) - qcx(k)
          thx(k) = (templ+qcx(k)*xlv/cp) / exnerhl(k) 
          THVX(K)=THX(K)*(1.+ep_1*QX(K)-QCX(K)) 


       ENDDO
       
       
       
       
       
       do k = 1, kte
          if(k.eq.1)then
             aimp(k)=0.
          else
             aimp(k)=-(rhoxfl(k)*rrhoxhl(k))* &
                  kzm(k) * dt2 *rdzq(k)*rdza(k)
          endif
          if(k.eq.kte)then
             cimp(k)=0.
          else
             cimp(k)=-(rhoxfl(k+1)*rrhoxhl(k))* &
                  kzm(k+1) * dt2 *rdzq(k)*rdza(k+1)
          endif
          bimp(k) = 1 - aimp(k) - cimp(k)
          
          
          if(k.eq.kte)then
             
             UFLXP=-USTX*USTX*UX(KTE)/WSPD1(i)
             VFLXP=-USTX*USTX*VX(KTE)/WSPD1(i)
             
             rimp1(k) = ux(k) + dt2 *  &
                  uflxp * (rhoxsf*rrhoxhl(k)) *rdzq(kte)
             rimp2(k) = vx(k) + dt2 *  &
                  vflxp * (rhoxsf*rrhoxhl(k)) *rdzq(kte)
          else
             rimp1(k) = ux(k)
             rimp2(k) = vx(k)
          endif
       enddo
       call tridag(aimp,bimp,cimp,rimp1,uimp1,kte)
       call tridag(aimp,bimp,cimp,rimp2,uimp2,kte)
       
       do  k = 1, kte
          ux(k) = uimp1(k)
          vx(k) = uimp2(k)
          if(k.ge.2)then
             DUDZ =  (UX(K-1)  - UX(K))  *RDZA(K)
             DVDZ =  (VX(K-1)  - VX(K))  *RDZA(K)
             SVS(K)= DUDZ*DUDZ + DVDZ*DVDZ
          endif
       enddo
       
       
       
       
       
       do k = 1, kte
          if(k.eq.1)then
             aimp(k)=0.
          else
             aimp(k)=-(rhoxfl(k)*rrhoxhl(k))* &
                  kth(k) * dt2 *rdzq(k)*rdza(k)
          endif
          if(k.eq.kte)then
             cimp(k)=0.
          else
             cimp(k)=-(rhoxfl(k+1)*rrhoxhl(k))* &
                  kth(k+1) * dt2 *rdzq(k)*rdza(k+1)
          endif
          bimp(k) = 1 - aimp(k) - cimp(k)
          
          
          
          rimp1(k) = qix(k)
          
       enddo
       call tridag(aimp,bimp,cimp,rimp1,qix,kte)
       
       
       
       
       
       
       
       
       
       
       
       
       DO K=2,KTE
          
          BOUYAN(K)=-KTH(K)*nsquar(K)
          
          SHEAR(K)  = KZM(K) * SVS(K)
       ENDDO
       do ilay = 1,iconv
          k = ktop(ilay)
          if(qcx(k).gt.1e-8.and.k.gt.1) bouyan(k) = bouyan(k) - & 
               rttenx(k)*(presfl(k+1)-presfl(k)) * rrhoxfl(k) &
               * rexnerfl(k) / thvx(k) 
       enddo
       
       tke(1)=0.
       bbls(1)= 0.0
       
       tke(kte+1)=3.25 * ustx * ustx 
       
       
       
       do k = 2, kte
          if(k.eq.2)then
             aimp(k-1)=0.
          else
             aimp(k-1)=-(rhoxhl(k-1)*rrhoxfl(k))* &
                  kethl(k-1)*dt2*rdzq(k-1)*rdza(k)
          endif
          if(k.eq.kte)then
             cimp(k-1)=0.
             
             if(bbls(k).gt.0)then
                bimp(k-1) = 1 - aimp(k-1) - cimp(k-1) + &
                     dt2 * ( sqrt(tke(k))*rczero/bbls(k) + &
                     rhoxhl(k)*rrhoxfl(k)*kethl(k)*rdzq(k)*rdza(k) )
             else
                bimp(k-1) = 1 - aimp(k-1) - cimp(k-1) + &
                     dt2 * rhoxhl(k)*rrhoxfl(k)* kethl(k)*rdzq(k)*rdza(k)
             endif

          else
             cimp(k-1)=-(rhoxhl(k)*rrhoxfl(k))* &
                  kethl(k)*dt2*rdzq(k)*rdza(k)
             tbbls = max(bbls(k),bbls(k+1))
             if(tbbls.gt.0)then
                bimp(k-1)= 1 - aimp(k-1) - cimp(k-1) + dt2 * sqrt(tke(k))*rczero/tbbls
             else
                bimp(k-1)= 1 - aimp(k-1) - cimp(k-1)
             endif
          endif
          
          if(k.eq.kte)then
             
             rimp1(k-1) = tke(k) + dt2 * ( SHEAR(K)+BOUYAN(K) + &
                  tke(kte+1)*(rhoxhl(k)*rrhoxfl(k))*kethl(k)*rdzq(k)*rdza(k) )
          else
             rimp1(k-1) = tke(k) + dt2 * (SHEAR(K)+BOUYAN(K))
          endif
       enddo

       call tridag(aimp,bimp,cimp,rimp1,uimp1,kte-1)
       
       do  k = 2, kte
          tke(k) = max(uimp1(k-1),0.001) 
       enddo
       
       
       
       

       DO K=KTS,KTE
          kr = kte+1-k
          U2DTEN(I,KR)= (UX(K)-UXS(K))*RDT
          V2DTEN(I,KR)=(VX(K)-VXS(K))*RDT
          T2DTEN(I,KR)=(THX(K)-THXS(K))*EXNERHL(K)*RDT
          QV2DTEN(I,KR) = (QX(K)-QXS(K))*RDT
          qi2Dten(i,kr) = (qix(k)-qixs(k))*rdt
          qc2Dten(i,kr) = (qcx(k)-qcxs(k))*rdt
          tke2d(i,kr)=tke(k)
          kzm2d(i,kr)=kzm(k)
          kth2d(i,kr)=kth(k)
          kethl2d(i,kr)=kethl(k)
          EL2D(i,kr) = bbls(k)
       ENDDO
       
       pbl(i)=pblx
       ust(i)=ustx
       kpbl1d(i)=kte+1-kpbl2dx
    end do
    
    RETURN
  END SUBROUTINE GBM2D
  
  
  subroutine tridag(a,b,c,r,u,n)
    
    
    implicit none
    integer n,nmax
    real a(n),b(n),c(n),r(n),u(n)
    parameter (nmax=100)
    integer j
    real rbet,gam(nmax)
    rbet=1./b(1)
    u(1)=r(1)*rbet
    do j=2,n
       gam(j)=c(j-1)*rbet
       rbet=1./(b(j)-a(j)*gam(j))
       u(j)=(r(j)-a(j)*u(j-1))*rbet
    end do
    do  j=n-1,1,-1
       u(j)=u(j)-gam(j+1)*u(j+1)
    end do
    return
  end subroutine tridag
      
  subroutine pblhgt( &
       
       zqx,kts,kte, &
       nsquar,tke,presfl,rhoxfl,rcldb,exnerfl, &
       rttenx,thvx,thlx,thx,qwx,rexnerhl,qcx, &
       xfr,xlvocp,aone,atwo,rstbl,            &
       
       bbls,pblx, &
       ktop,iconv,kmix2dx,kpbl2dx &
       )
    
    implicit none
    
    real zqx(kts:kte+2)
    real nsquar(kts:kte+1),tke(kts:kte+1),presfl(kts:kte+1),rhoxfl(kts:kte+1), &
         rcldb(kts:kte+1),exnerfl(kts:kte+1)
    real rttenx(kts:kte),thvx(kts:kte),thlx(kts:kte),thx(kts:kte),qwx(kts:kte), &
         rexnerhl(kts:kte),qcx(kts:kte)
    real xfr,xlvocp,aone,atwo,rstbl
    
    real bbls(kts:kte+1), pblx
    integer ktop(kts:kte+1), iconv,kmix2dx,kpbl2dx, &
            ktop_save(kts:kte+1)

    
    integer kbot(kts:kte+1),kts,kte,kstart
    integer istabl,ibeg,ilay,nlev,k,itemp
    real blinf,rnnll,tkeavg,trnnll,radnnll,delthvl,elambda, &
         bige,biga,entnnll,tbbls

    
    iconv = 0
    istabl = 1
    do k=2,kte+1   
       if(nsquar(k).le.0)then   
          if(istabl.eq.1)then
             iconv = iconv + 1
             ktop(iconv)=k
          endif
          istabl=0
          kbot(iconv)=k
       else
          istabl=1
          BBLS(K) = MIN(rstbl*SQRT(TKE(K)/NSQUAR(K)),karman*ZQX(K))
       endif
    enddo
    



    
	      
    

   ktop_save=ktop

  IF(iconv.ge.1)THEN

    ibeg = 1
2745 do ilay = ibeg, iconv
       blinf = xfr*(zqx(ktop(ilay)-1) - zqx(kbot(ilay)+1))
       
       rnnll = 0.
       tkeavg = 0.
       nlev = kbot(ilay)-ktop(ilay)+1
       do k = ktop(ilay), kbot(ilay)
          bbls(k) = min(blinf,karman*ZQX(K))
          rnnll = rnnll + nsquar(k)*bbls(k)*bbls(k)   
          tkeavg = tkeavg + tke(k) / nlev
       enddo
       
       kstart=ktop(ilay)-1

       do k = kstart,2,-1
          ktop(ilay)=k        
          bbls(k) = min(karman * ZQX(K),blinf)
          trnnll = nsquar(k)*bbls(k)*bbls(k)
          if(trnnll*nlev.ge.-0.5*rnnll) goto 2746 




                                           
                                           
                                           
          if(ilay.gt.1) then
             if(ktop(ilay).eq.kbot(ilay-1))then 
                ibeg = ilay - 1
   
                                        
                                        
                ktop(ibeg)=ktop_save(ibeg) 
                kbot(ibeg)=kbot(ibeg+1)
                iconv = iconv - 1
                do itemp = ibeg+1,iconv 
                   ktop(itemp)=ktop(itemp+1)
                   kbot(itemp)=kbot(itemp+1)
                   ktop_save(itemp)=ktop_save(itemp+1) 
                enddo
                goto 2745        
             endif
          endif
          rnnll = rnnll + trnnll
          nlev = nlev + 1 
       enddo
       
2746   k = ktop(ilay) 
       radnnll = 0.
       if(qcx(k).gt.1e-8) radnnll =  &
            rttenx(k)*(presfl(k+1)-presfl(k))/ &
            (rhoxfl(k)*thvx(k)*exnerfl(k))
       entnnll = 0.
       if(k.ge.3)then
          delthvl = (thlx(k-2)+thx(k-2)*ep_1*qwx(k-2)) &
               - (thlx(k) + thx(k)*ep_1*qwx(k))
          elambda = xlvocp*rcldb(k)*rexnerhl(k)/max(delthvl,0.1)
          bige = 0.8 * elambda
          biga = aone * (1 + atwo * bige)
          entnnll = biga * sqrt(tkeavg**3) / bbls(k)
       endif
       if(tkeavg.gt.0.) rnnll = rnnll + min(0.,bbls(k)/sqrt(tkeavg) * (radnnll + entnnll) )
       
       do k = kbot(ilay)+1,kte+1
          tbbls = min(karman * ZQX(K),blinf)
          trnnll = nsquar(k)*tbbls*tbbls
          if(trnnll*nlev.ge.-0.5*rnnll)goto 2747 
          kbot(ilay)=k
          if(ilay.lt.iconv.and.kbot(ilay).eq.ktop(ilay+1))then 

             ktop(ilay)=ktop_save(ilay)
             kbot(ilay)=kbot(ilay+1)
             iconv = iconv - 1
             do itemp = ilay+1,iconv
                ktop(itemp)=ktop(itemp+1)
                kbot(itemp)=kbot(itemp+1)
                ktop_save(itemp)=ktop_save(itemp+1)
             enddo
             goto 2745        
          endif
          rnnll = rnnll + trnnll
          bbls(k)=tbbls
          nlev = nlev + 1
       enddo
2747   continue
    enddo 
    do ilay = 1, iconv
       blinf = xfr*(zqx(ktop(ilay)-1) - zqx(kbot(ilay)+1))
       do k = ktop(ilay),kbot(ilay)
          bbls(k) = min(karman * ZQX(K),blinf)
       enddo
    enddo

 ENDIF  

    
    
    
    
    
			
    if(iconv.gt.0)then
       if(kbot(iconv).eq.kte+1)then
          kmix2dx = ktop(iconv)
          if(kpbl2dx.ge.0)then
             if(iconv.gt.1)then
                kpbl2dx = ktop(iconv-1)
             else
                kpbl2dx = kmix2dx
             endif
          else
             kpbl2dx=-kpbl2dx
          endif
       else
          kmix2dx = kte
          if(kpbl2dx.ge.0)then
             kpbl2dx = ktop(iconv)
          else
             kpbl2dx = -kpbl2dx
          endif
       endif
    else
       kmix2dx = kte
       if(kpbl2dx.ge.0)then
          kpbl2dx = kmix2dx
       else
          kpbl2dx = -kpbl2dx
       endif
    endif
    pblx=ZQX(KMIX2DX)
    return
  end subroutine pblhgt


  subroutine roots(a,b,c,r1,r2)
    implicit none
    real a,b,c,r1,r2,q
    if(a.eq.0)then            
       if(b.eq.0)then         
          r1 = -9.99e33
       else                   
          r1 = -c / b
       endif
       r2 = r1
    else
       if(b.eq.0.)then        
          if(a*c.gt.0.)then   
             r1 =  -9.99e33
          else                
             r1 = sqrt(-c/a)
          endif
          r2 = -r1
       else                   
          if((b**2 - 4*a*c).lt.0.)then 
             r1 =  -9.99e33
             r2 = -r1
          else
             q = - 0.5 * ( b + sign(1.0,b) * &
                  sqrt(b**2 - 4*a*c) )
             r1 = q/a
             r2 = c/q
          endif
       endif
    endif
    return
  end subroutine roots

  subroutine n2(thlx,exnerfl,epop,qwx,cpoxlv,rdza,        &
       rexnerfl, kts,kte, nsquar,rcldb,xlvocp, svp1pa)
    implicit none
    
    real thlx(kts:kte),exnerfl(kts:kte+1),epop(kts:kte+1),qwx(kts:kte), &
         rexnerfl(kts:kte+1),rdza(kts:kte+1),nsquar(kts:kte+1),rcldb(kts:kte+1)
    real cpoxlv,xlvocp,svp1pa
    
    real templ,rvls,temps,tempv,tvbl,rcld,tvab,thvxfl,dtvdz
    integer k,kts,kte

    DO K = 2, KTE
       
       
       templ = thlx(k)*exnerfl(k)
       rvls  = 100.*svp1pa*EXP(svp2*(templ-svpt0)/(templ-svp3))*epop(k)
       temps=templ + (qwx(k)-rvls)/(cpoxlv + &
            ep_2*xlv*rvls/(r_d*templ**2))
       rvls  = 100.*svp1pa*EXP(svp2*(temps-svpt0)/(temps-svp3))*epop(k)
       rcldb(k)=max(qwx(k)-rvls,0.)
       tempv = (templ + xlvocp*rcldb(k)) * &
            (1 + ep_1*(qwx(k)-rcldb(k)) - rcldb(k))
       tvbl = tempv*rexnerfl(k) 
       
       templ = thlx(k-1)*exnerfl(k)
       rvls  = 100.*svp1pa*EXP(svp2*(templ-svpt0)/(templ-svp3))*epop(k)
       temps=templ+(qwx(k-1)-rvls)/(cpoxlv+ &
            ep_2*xlv*rvls/(r_d*templ**2))
       rvls  = 100.*svp1pa*EXP(svp2*(temps-svpt0)/(temps-svp3))*epop(k)
       rcld=max(qwx(k-1)-rvls,0.)
       tempv = (templ + xlvocp*rcld) * &
            (1 + ep_1*(qwx(k-1)-rcld) - rcld)
       tvab = tempv*rexnerfl(k) 
       
       thvxfl= .5 * (tvab+tvbl)
       dtvdz = (tvab - tvbl) *rdza(k)
       nsquar(k) = g/thvxfl * dtvdz
    ENDDO
    nsquar(1)=nsquar(2)
    return
  end subroutine n2

  subroutine my( &
       
    bbls,nsquar,tke,iconv,ktop,thlx,thx,qwx,     &
         xlvocp,rcldb,rexnerhl,aone,atwo,kts,kte, &
         
         kzm,kth,kethl)
    implicit none
    real bbls(kts:kte+1),nsquar(kts:kte+1),tke(kts:kte+1),thlx(kts:kte),thx(kts:kte), &
         qwx(kts:kte),kzm(kts:kte+1),kth(kts:kte+1),kethl(kts:kte)
    real xlvocp,rcldb(kts:kte+1),rexnerhl(kts:kte),aone,atwo
    integer iconv,ktop(kts:kte)
    
    real gh,a1,b1,c1,a2,b2,a1ob1,nuk,delthvl,elambda,bige,biga,kmax,n2min
    real sm(kts:kte+1),sh(kts:kte+1)
    integer k,ilay,kts,kte
    parameter(A1=0.92,B1=16.6,C1=0.08,A2=0.74,B2=10.1)
    parameter(nuk=5.0)        

    parameter(kmax=1000.)      
    parameter(n2min=1.e-7)     
    a1ob1 = a1/b1

    
    
    
    kzm(kte+1)=0.
    kth(kte+1)=0.
    kzm(1)=0.
    kth(1)=0.
    sm(kte+1)=1.
    sh(kte+1)=1.
    sm(1) = 1. 
    sh(1) = 1. 
    DO K = KTE, 2, -1




       gh =-bbls(k)*bbls(k)*nsquar(k)/(2*tke(k)+1e-9) 
       gh = min(gh,0.0233)
       gh = max(gh,-0.28)  
       sm(k) = a1 * (1. - 3.*c1 - 6.*a1ob1 - 3.*a2*gh* &
            ((b2-3.*a2)*(1.-6.*a1ob1) - 3.*c1*(b2+6.*a1))) / &
            ((1. - 3.*a2*gh*(6.*a1 + b2)) * (1. - 9.*a1*a2*gh))
       sh(k) = a2 * (1. - 6.*a1ob1) / (1. - 3.*a2*gh*(6.*a1+b2))


       kzm(k) = min(bbls(k)*sqrt(2*tke(k))*sm(k),kmax) 
       kth(k) = min(bbls(k)*sqrt(2*tke(k))*sh(k),kmax)

       kethl(k)=nuk*sqrt(kzm(k)*kzm(k+1)) 
       kethl(k)=min(kethl(k),kmax) 
    ENDDO
    
    
     do ilay = 1,iconv
      k = ktop(ilay)
      IF(nsquar(k).ge.n2min)THEN 
      kethl(k  )=nuk*kzm(k+1)
      if(k.ge.3)then 
         kethl(k-1)=0.0   
         delthvl = (thlx(k-2)+thx(k-2)*ep_1*qwx(k-2)) - &   
              (thlx(k) + thx(k) * ep_1 * qwx(k))
         elambda = xlvocp*rcldb(k)*rexnerhl(k)/max(delthvl,0.1)
         bige = 0.8 * elambda
         biga = aone * (1 + atwo * bige)
         kth(k) = min(kth(k), biga*sqrt(TKE(k)**3)/NSQUAR(k)/ &
              max(bbls(k),bbls(k+1)))
         kzm(k) = min(kzm(k), kth(k) / sh(k+1) * sm(k+1)) 
        endif
    ENDIF 
   enddo
     
     KETHL(1) = kethl(2)
     
     kethl(kte) = nuk*0.5*kzm(kte)
    return
  end subroutine my

  SUBROUTINE gbmpblinit(RUBLTEN,RVBLTEN,RTHBLTEN,RQVBLTEN,        &
       RQCBLTEN,RQIBLTEN,P_QI,P_FIRST_SCALAR,       &
       TKE_PBL,KTH_GBM,                             &
       restart,allowed_to_read,                     &
       ids, ide, jds, jde, kds, kde,                &
       ims, ime, jms, jme, kms, kme,                &
       its, ite, jts, jte, kts, kte                 )
    
    IMPLICIT NONE
    
    LOGICAL , INTENT(IN)          :: restart,allowed_to_read
    INTEGER , INTENT(IN)          ::  ids, ide, jds, jde, kds, kde, &
         ims, ime, jms, jme, kms, kme, &
         its, ite, jts, jte, kts, kte
    INTEGER , INTENT(IN)          ::  P_QI,P_FIRST_SCALAR

    REAL , DIMENSION( ims:ime , kms:kme , jms:jme ) , INTENT(OUT) ::         &
         RUBLTEN, &
         RVBLTEN, &
         RTHBLTEN, &
         RQVBLTEN, &
         RQCBLTEN, & 
         RQIBLTEN, &
         TKE_PBL,  &
         KTH_GBM
    INTEGER :: i, j, k, itf, jtf, ktf

    jtf=min0(jte,jde-1)
    ktf=min0(kte,kde-1)
    itf=min0(ite,ide-1)

    IF(.not.restart)THEN
       DO j=jts,jtf
          DO k=kts,ktf
             DO i=its,itf
                RUBLTEN(i,k,j)=0.
                RVBLTEN(i,k,j)=0.
                RTHBLTEN(i,k,j)=0.
                RQVBLTEN(i,k,j)=0.
                RQCBLTEN(i,k,j)=0.
                TKE_PBL(i,k,j)=0.001 
                KTH_GBM(i,k,j)=0.
             ENDDO
          ENDDO
       ENDDO
    ENDIF

    IF (P_QI .ge. P_FIRST_SCALAR .and. .not.restart) THEN
       DO j=jts,jtf
          DO k=kts,ktf
             DO i=its,itf
                RQIBLTEN(i,k,j)=0.
             ENDDO
          ENDDO
       ENDDO
    ENDIF

  END SUBROUTINE gbmpblinit


END MODULE module_bl_gbmpbl

