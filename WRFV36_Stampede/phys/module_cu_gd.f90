


MODULE module_cu_gd

CONTAINS


   SUBROUTINE GRELLDRV(                                         &
               DT,itimestep,DX                                  &
              ,rho,RAINCV,PRATEC                                &
              ,U,V,t,W,q,p,pi                                   &
              ,dz8w,p8w,XLV,CP,G,r_v                            &
              ,htop,hbot,ktop_deep                              &
              ,CU_ACT_FLAG,warm_rain                            &
              ,APR_GR,APR_W,APR_MC,APR_ST,APR_AS                &
              ,APR_CAPMA,APR_CAPME,APR_CAPMI                    &
              ,MASS_FLUX,XF_ENS,PR_ENS,HT,XLAND,gsw             &
              ,GDC,GDC2                                         &
              ,ensdim,maxiens,maxens,maxens2,maxens3            &
              ,ids,ide, jds,jde, kds,kde                        &
              ,ims,ime, jms,jme, kms,kme                        &
              ,its,ite, jts,jte, kts,kte                        &
              ,periodic_x,periodic_y                            &
              ,RQVCUTEN,RQCCUTEN,RQICUTEN                       &
              ,RQVFTEN,RQVBLTEN                                 &
              ,RTHFTEN,RTHCUTEN,RTHRATEN,RTHBLTEN               &
              ,F_QV    ,F_QC    ,F_QR    ,F_QI    ,F_QS         &
              ,CFU1,CFD1,DFU1,EFU1,DFD1,EFD1,f_flux             )


   IMPLICIT NONE

   INTEGER,      INTENT(IN   ) ::                               &
                                  ids,ide, jds,jde, kds,kde,    & 
                                  ims,ime, jms,jme, kms,kme,    & 
                                  its,ite, jts,jte, kts,kte
   LOGICAL periodic_x,periodic_y

   integer, intent (in   )              ::                      &
                       ensdim,maxiens,maxens,maxens2,maxens3
  
   INTEGER,      INTENT(IN   ) :: ITIMESTEP
   LOGICAL,      INTENT(IN   ) :: warm_rain

   REAL,         INTENT(IN   ) :: XLV, R_v
   REAL,         INTENT(IN   ) :: CP,G

   REAL,  DIMENSION( ims:ime , kms:kme , jms:jme )         ,    &
          INTENT(IN   ) ::                                      &
                                                          U,    &
                                                          V,    &
                                                          W,    &
                                                         pi,    &
                                                          t,    &
                                                          q,    &
                                                          p,    &
                                                       dz8w,    &
                                                       p8w,    &
                                                        rho
   REAL,  DIMENSION( ims:ime , kms:kme , jms:jme )         ,    &
          OPTIONAL                                         ,    &
          INTENT(INOUT   ) ::                                   &
               GDC,GDC2


   REAL, DIMENSION( ims:ime , jms:jme ),INTENT(IN) :: GSW,HT,XLAND

   REAL, INTENT(IN   ) :: DT, DX


   REAL, DIMENSION( ims:ime , jms:jme ),                        &
         INTENT(INOUT) ::         RAINCV, PRATEC, MASS_FLUX,    &
                          APR_GR,APR_W,APR_MC,APR_ST,APR_AS,    &
                              APR_CAPMA,APR_CAPME,APR_CAPMI,htop,hbot






   LOGICAL, DIMENSION( ims:ime , jms:jme ),                     &
         INTENT(INOUT) ::                       CU_ACT_FLAG




   INTEGER, DIMENSION( ims:ime,         jms:jme ),              &
         OPTIONAL,                                              &
         INTENT(  OUT) ::                           ktop_deep

   REAL, DIMENSION( ims:ime , kms:kme , jms:jme ),              &
         OPTIONAL,                                              &
         INTENT(INOUT) ::                           RTHFTEN,    &
                                                    RQVFTEN

   REAL, DIMENSION( ims:ime , kms:kme , jms:jme ),              &
         OPTIONAL,                                              &
         INTENT(IN   ) ::                                       &
                                                   RTHRATEN,    &
                                                   RTHBLTEN,    &
                                                   RQVBLTEN
   REAL, DIMENSION( ims:ime , kms:kme , jms:jme ),              &
         OPTIONAL,                                              &
         INTENT(INOUT) ::                                       &
                                                   RTHCUTEN,    &
                                                   RQVCUTEN,    &
                                                   RQCCUTEN,    &
                                                   RQICUTEN
   REAL, DIMENSION( ims:ime , kms:kme , jms:jme ),              &
         OPTIONAL,                                              &
         INTENT(INOUT) ::                                       &
                                                   CFU1,        &
                                                   CFD1,        &
                                                   DFU1,        &
                                                   EFU1,        &
                                                   DFD1,        &
                                                   EFD1







   LOGICAL, OPTIONAL ::                                      &
                                                   F_QV      &
                                                  ,F_QC      &
                                                  ,F_QR      &
                                                  ,F_QI      &
                                                  ,F_QS
   LOGICAL, intent(in), OPTIONAL ::                f_flux




     real,    dimension ( ims:ime , jms:jme , 1:ensdim) ::      &
        massfln,xf_ens,pr_ens
     real,    dimension (its:ite,kts:kte+1) ::                    &
        OUTT,OUTQ,OUTQC,phh,cupclw,     &
        outCFU1,outCFD1,outDFU1,outEFU1,outDFD1,outEFD1
     logical :: l_flux
     real,    dimension (its:ite)         ::                    &
        pret, ter11, aa0, fp

     integer, dimension (its:ite) ::                            &
        kbcon, ktop

     integer, dimension (its:ite,jts:jte) ::                    &
        iact_old_gr
     integer :: ichoice,iens,ibeg,iend,jbeg,jend






     real,    dimension (its:ite,kts:kte+1) ::                    &
        T2d,TN,q2d,qo,PO,P2d,US,VS,omeg
     real, dimension (its:ite)            ::                    &
        Z1,PSUR,AAEQ,direction,mconv,cuten,umean,vmean,pmean

   INTEGER :: i,j,k,ICLDCK,ipr,jpr
   REAL    :: tcrit,dp,dq
   INTEGER :: itf,jtf,ktf
   REAL    :: rkbcon,rktop        

   l_flux=.FALSE.
   if (present(f_flux)) l_flux=f_flux
   if (l_flux) then
      l_flux = l_flux .and. present(cfu1) .and. present(cfd1)   &
               .and. present(dfu1) .and. present(efu1)          &
               .and. present(dfd1) .and. present(efd1)
   endif

   ichoice=0
   iens=1
     ipr=0
     jpr=0

   IF ( periodic_x ) THEN
      ibeg=max(its,ids)
      iend=min(ite,ide-1)
   ELSE
      ibeg=max(its,ids+4)
      iend=min(ite,ide-5)
   END IF
   IF ( periodic_y ) THEN
      jbeg=max(jts,jds)
      jend=min(jte,jde-1)
   ELSE
      jbeg=max(jts,jds+4)
      jend=min(jte,jde-5)
   END IF


   tcrit=258.

   itf=MIN(ite,ide-1)
   ktf=MIN(kte,kde-1)
   jtf=MIN(jte,jde-1)

     DO 100 J = jts,jtf  
     DO I= its,itf
        cuten(i)=0.
        iact_old_gr(i,j)=0
        mass_flux(i,j)=0.
        pratec(i,j) = 0.
        raincv(i,j)=0.
        CU_ACT_FLAG(i,j) = .true.
        ktop_deep(i,j) = 0
     ENDDO
     DO k=1,ensdim
     DO I= its,itf
        massfln(i,j,k)=0.
     ENDDO
     ENDDO

     DO k= kts,ktf
     DO I= its,itf
        RTHFTEN(i,k,j)=(RTHFTEN(i,k,j)+RTHRATEN(i,k,j)+RTHBLTEN(i,k,j))*pi(i,k,j)
        RQVFTEN(i,k,j)=RQVFTEN(i,k,j)+RQVBLTEN(i,k,j)
     ENDDO
     ENDDO

     
     DO K=kts,ktf
     DO I=ITS,ITF
         phh(i,k) = p(i,k,j)
     ENDDO
     ENDDO
     DO I=ITS,ITF
         PSUR(I)=p8w(I,1,J)*.01
         TER11(I)=HT(i,j)
         mconv(i)=0.
         aaeq(i)=0.
         direction(i)=0.
         pret(i)=0.
         umean(i)=0.
         vmean(i)=0.
         pmean(i)=0.
     ENDDO
     DO K=kts,ktf
     DO I=ITS,ITF
         omeg(i,k)=0.

         po(i,k)=phh(i,k)*.01
         P2d(I,K)=PO(i,k)
         US(I,K) =u(i,k,j)
         VS(I,K) =v(i,k,j)
         T2d(I,K)=t(i,k,j)
         q2d(I,K)=q(i,k,j)
         omeg(I,K)= -g*rho(i,k,j)*w(i,k,j)
         TN(I,K)=t2d(i,k)+RTHFTEN(i,k,j)*dt
         IF(TN(I,K).LT.200.)TN(I,K)=T2d(I,K)
         QO(I,K)=q2d(i,k)+RQVFTEN(i,k,j)*dt
         IF(Q2d(I,K).LT.1.E-08)Q2d(I,K)=1.E-08
         IF(QO(I,K).LT.1.E-08)QO(I,K)=1.E-08
         OUTT(I,K)=0.
         OUTQ(I,K)=0.
         OUTQC(I,K)=0.


     ENDDO
     ENDDO
      do k=  kts+1,ktf-1
      DO I = its,itf
         if((p2d(i,1)-p2d(i,k)).gt.150.and.p2d(i,k).gt.300)then
            dp=-.5*(p2d(i,k+1)-p2d(i,k-1))
            umean(i)=umean(i)+us(i,k)*dp
            vmean(i)=vmean(i)+vs(i,k)*dp
            pmean(i)=pmean(i)+dp
         endif
      enddo
      enddo
      DO I = its,itf
         if(pmean(i).gt.0)then
            umean(i)=umean(i)/pmean(i)
            vmean(i)=vmean(i)/pmean(i)
            direction(i)=(atan2(umean(i),vmean(i))+3.1415926)*57.29578
            if(direction(i).gt.360.)direction(i)=direction(i)-360.
         endif
      ENDDO
      DO K=kts,ktf-1
      DO I = its,itf
        dq=(q2d(i,k+1)-q2d(i,k))
        mconv(i)=mconv(i)+omeg(i,k)*dq/g
      ENDDO
      ENDDO
      DO I = its,itf
        if(mconv(i).lt.0.)mconv(i)=0.
      ENDDO



      CALL CUP_enss(outqc,j,AAEQ,T2d,Q2d,TER11,TN,QO,PO,PRET,     &
           P2d,OUTT,OUTQ,DT,PSUR,US,VS,tcrit,iens,                &
           mconv,massfln,iact_old_gr,omeg,direction,MASS_FLUX,    &
           maxiens,maxens,maxens2,maxens3,ensdim,                 &
           APR_GR,APR_W,APR_MC,APR_ST,APR_AS,                     &
           APR_CAPMA,APR_CAPME,APR_CAPMI,kbcon,ktop,              &
           xf_ens,pr_ens,XLAND,gsw,cupclw,                        &
           xlv,r_v,cp,g,ichoice,ipr,jpr,                          &
           outCFU1,outCFD1,outDFU1,outEFU1,outDFD1,outEFD1,l_flux,&
           ids,ide, jds,jde, kds,kde,                             &
           ims,ime, jms,jme, kms,kme,                             &
           its,ite, jts,jte, kts,kte                             )

      CALL neg_check(dt,q2d,outq,outt,outqc,pret,its,ite,kts,kte,itf,ktf)
      if(j.ge.jbeg.and.j.le.jend)then

            DO I=its,itf

              ktop_deep(i,j) = ktop(i)
              if(i.ge.ibeg.and.i.le.iend)then
              if(pret(i).gt.0.)then
                 pratec(i,j)=pret(i)
                 raincv(i,j)=pret(i)*dt
                 cuten(i)=1.
                 rkbcon = kte+kts - kbcon(i)
                 rktop  = kte+kts -  ktop(i)
                 if (ktop(i)  > HTOP(i,j)) HTOP(i,j) = ktop(i)+.001
                 if (kbcon(i) < HBOT(i,j)) HBOT(i,j) = kbcon(i)+.001
              endif
              else
               pret(i)=0.
              endif
            ENDDO
            DO K=kts,ktf
            DO I=its,itf
               RTHCUTEN(I,K,J)=outt(i,k)*cuten(i)/pi(i,k,j)
               RQVCUTEN(I,K,J)=outq(i,k)*cuten(i)
            ENDDO
            ENDDO

            IF(PRESENT(RQCCUTEN)) THEN
              IF ( F_QC ) THEN
                DO K=kts,ktf
                DO I=its,itf
                   RQCCUTEN(I,K,J)=outqc(I,K)*cuten(i)
                   IF ( PRESENT( GDC ) ) GDC(I,K,J)=CUPCLW(I,K)*cuten(i)
                   IF ( PRESENT( GDC2 ) ) GDC2(I,K,J)=0.
                ENDDO
                ENDDO
              ENDIF
            ENDIF



            IF(PRESENT(RQICUTEN).AND.PRESENT(RQCCUTEN))THEN
              IF (F_QI) THEN
                DO K=kts,ktf
                DO I=its,itf
                   if(t2d(i,k).lt.258.)then
                      RQICUTEN(I,K,J)=outqc(I,K)*cuten(i)
                      RQCCUTEN(I,K,J)=0.
                      IF ( PRESENT( GDC2 ) ) GDC2(I,K,J)=CUPCLW(I,K)*cuten(i)
                   else
                      RQICUTEN(I,K,J)=0.
                      RQCCUTEN(I,K,J)=outqc(I,K)*cuten(i)
                      IF ( PRESENT( GDC ) ) GDC(I,K,J)=CUPCLW(I,K)*cuten(i)
                   endif
                ENDDO
                ENDDO
              ENDIF
            ENDIF

            if (l_flux) then
               DO K=kts,ktf
               DO I=its,itf
                  cfu1(i,k,j)=outcfu1(i,k)*cuten(i)
                  cfd1(i,k,j)=outcfd1(i,k)*cuten(i)
                  dfu1(i,k,j)=outdfu1(i,k)*cuten(i)
                  efu1(i,k,j)=outefu1(i,k)*cuten(i)
                  dfd1(i,k,j)=outdfd1(i,k)*cuten(i)
                  efd1(i,k,j)=outefd1(i,k)*cuten(i)
               enddo
               enddo
            endif
        endif 

 100    continue

   END SUBROUTINE GRELLDRV


   SUBROUTINE CUP_enss(OUTQC,J,AAEQ,T,Q,Z1,                            &
              TN,QO,PO,PRE,P,OUTT,OUTQ,DTIME,PSUR,US,VS,               &
              TCRIT,iens,mconv,massfln,iact,                           &
              omeg,direction,massflx,maxiens,                          &
              maxens,maxens2,maxens3,ensdim,                           &
              APR_GR,APR_W,APR_MC,APR_ST,APR_AS,                       &
              APR_CAPMA,APR_CAPME,APR_CAPMI,kbcon,ktop,                &   
              xf_ens,pr_ens,xland,gsw,cupclw,                          &
              xl,rv,cp,g,ichoice,ipr,jpr,                              &
              outCFU1,outCFD1,outDFU1,outEFU1,outDFD1,outEFD1,l_flux,  &
              ids,ide, jds,jde, kds,kde,                               &
              ims,ime, jms,jme, kms,kme,                               &
              its,ite, jts,jte, kts,kte                               )

   IMPLICIT NONE

     integer                                                           &
        ,intent (in   )                   ::                           &
        ids,ide, jds,jde, kds,kde,                                     &
        ims,ime, jms,jme, kms,kme,                                     &
        its,ite, jts,jte, kts,kte,ipr,jpr
     integer, intent (in   )              ::                           &
        j,ensdim,maxiens,maxens,maxens2,maxens3,ichoice,iens
  
  
  
     real,    dimension (ims:ime,jms:jme,1:ensdim)                     &
        ,intent (inout)                   ::                           &
        massfln,xf_ens,pr_ens
     real,    dimension (ims:ime,jms:jme)                              &
        ,intent (inout )                  ::                           &
               APR_GR,APR_W,APR_MC,APR_ST,APR_AS,APR_CAPMA,     &
               APR_CAPME,APR_CAPMI,massflx
     real,    dimension (ims:ime,jms:jme)                              &
        ,intent (in   )                   ::                           &
               xland,gsw
     integer, dimension (its:ite,jts:jte)                              &
        ,intent (in   )                   ::                           &
        iact
  
  
  
  
     real,    dimension (its:ite,kts:kte)                              &
        ,intent (out  )                   ::                           &
        OUTT,OUTQ,OUTQC,CUPCLW,                                        &
        outCFU1,outCFD1,outDFU1,outEFU1,outDFD1,outEFD1
     logical, intent(in) :: l_flux
     real,    dimension (its:ite)                                      &
        ,intent (out  )                   ::                           &
        pre

     integer,    dimension (its:ite)                                   &
        ,intent (out  )                   ::                           &
        kbcon,ktop

  
  
  
  
  
     real,    dimension (its:ite,kts:kte)                              &
        ,intent (in   )                   ::                           &
        T,TN,PO,P,US,VS,omeg
     real,    dimension (its:ite,kts:kte)                              &
        ,intent (inout)                   ::                           &
         Q,QO
     real, dimension (its:ite)                                         &
        ,intent (in   )                   ::                           &
        Z1,PSUR,AAEQ,direction,mconv

       
       real                                                            &
        ,intent (in   )                   ::                           &
        dtime,tcrit,xl,cp,rv,g





     real,    dimension (its:ite,1:maxens)  ::                         &
        xaa0_ens
     real,    dimension (1:maxens)  ::                                 &
        mbdt_ens
     real,    dimension (1:maxens2) ::                                 &
        edt_ens
     real,    dimension (its:ite,1:maxens2) ::                         &
        edtc
     real,    dimension (its:ite,kts:kte,1:maxens2) ::                 &
        dellat_ens,dellaqc_ens,dellaq_ens,pwo_ens
     real,    dimension (its:ite,kts:kte,1:maxens2) ::                 &
        CFU1_ens,CFD1_ens,DFU1_ens,EFU1_ens,DFD1_ens,EFD1_ens










  
  
  
  
  
  
  
  
  
  
  
  
  
  
  


  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  

     real,    dimension (its:ite,kts:kte) ::                           &
        he,hes,qes,z,                                                  &
        heo,heso,qeso,zo,                                              &
        xhe,xhes,xqes,xz,xt,xq,                                        &

        qes_cup,q_cup,he_cup,hes_cup,z_cup,p_cup,gamma_cup,t_cup,      &
        qeso_cup,qo_cup,heo_cup,heso_cup,zo_cup,po_cup,gammao_cup,     &
        tn_cup,                                                        &
        xqes_cup,xq_cup,xhe_cup,xhes_cup,xz_cup,xp_cup,xgamma_cup,     &
        xt_cup,                                                        &

        dby,qc,qrcd,pwd,pw,hcd,qcd,dbyd,hc,qrc,zu,zd,clw_all,          &
        dbyo,qco,qrcdo,pwdo,pwo,hcdo,qcdo,dbydo,hco,qrco,zuo,zdo,      &
        xdby,xqc,xqrcd,xpwd,xpw,xhcd,xqcd,xhc,xqrc,xzu,xzd,            &

  
  
  
  
  

        cd,cdd,scr1,DELLAH,DELLAQ,DELLAT,DELLAQC,                      &
        CFU1,CFD1,DFU1,EFU1,DFD1,EFD1

  
  
  
  
  
  
     real,    dimension (its:ite) ::                                   &
       edt,edto,edtx,AA1,AA0,XAA0,HKB,HKBO,aad,XHKB,QKB,QKBO,          &
       XMB,XPWAV,XPWEV,PWAV,PWEV,PWAVO,PWEVO,BU,BUO,cap_max,xland1,     &
       cap_max_increment,closure_n
     integer,    dimension (its:ite) ::                                &
       kzdown,KDET,K22,KB,JMIN,kstabi,kstabm,K22x,                     &   
       KBCONx,KBx,KTOPx,ierr,ierr2,ierr3,KBMAX 

     integer                              ::                           &
       nall,iedt,nens,nens3,ki,I,K,KK,iresult
     real                                 ::                           &
      day,dz,mbdt,entr_rate,radius,entrd_rate,mentr_rate,mentrd_rate,  &
      zcutdown,edtmax,edtmin,depth_min,zkbmax,z_detr,zktop,            &
      massfld,dh,cap_maxs

     integer :: itf,jtf,ktf
     integer :: jmini
     logical :: keep_going

     itf=MIN(ite,ide-1)
     ktf=MIN(kte,kde-1)
     jtf=MIN(jte,jde-1)


      day=86400.
      do i=its,itf
        closure_n(i)=16.
        xland1(i)=1.
        if(xland(i,j).gt.1.5)xland1(i)=0.
        cap_max_increment(i)=25.
      enddo



      if(iens.le.4)then
      radius=14000.-float(iens)*2000.
      else
      radius=12000.
      endif




      entr_rate=.2/radius



      mentrd_rate=0.
      mentr_rate=entr_rate



      do k=kts,ktf
      do i=its,itf
        cupclw(i,k)=0.
        cd(i,k)=0.1*entr_rate
        cdd(i,k)=0.
      enddo
      enddo




      edtmax=.8
      edtmin=.2



      depth_min=500.




      cap_maxs=75.

      DO 7 i=its,itf
        kbmax(i)=1
        aa0(i)=0.
        aa1(i)=0.
        aad(i)=0.
        edt(i)=0.
        kstabm(i)=ktf-1
        IERR(i)=0
        IERR2(i)=0
        IERR3(i)=0
        if(aaeq(i).lt.-1.)then
           ierr(i)=20
        endif
 7    CONTINUE



      do i=its,itf
          cap_max(i)=cap_maxs

          if(gsw(i,j).lt.1.)cap_max(i)=25.

        iresult=0







       if(iresult.eq.1)then
          cap_max(i)=cap_maxs+20.
       endif

      enddo



      zkbmax=4000.



      zcutdown=3000.



      z_detr=1250.

      do nens=1,maxens
         mbdt_ens(nens)=(float(nens)-3.)*dtime*1.e-3+dtime*5.E-03
      enddo
      do nens=1,maxens2
         edt_ens(nens)=.95-float(nens)*.01
      enddo








      do i=its,itf
         if(ierr(i).ne.20)then
            do k=1,maxens*maxens2*maxens3
               xf_ens(i,j,(iens-1)*maxens*maxens2*maxens3+k)=0.
               pr_ens(i,j,(iens-1)*maxens*maxens2*maxens3+k)=0.
            enddo
         endif
      enddo



      call cup_env(z,qes,he,hes,t,q,p,z1, &
           psur,ierr,tcrit,0,xl,cp,   &
           ids,ide, jds,jde, kds,kde, &
           ims,ime, jms,jme, kms,kme, &
           its,ite, jts,jte, kts,kte)
      call cup_env(zo,qeso,heo,heso,tn,qo,po,z1, &
           psur,ierr,tcrit,0,xl,cp,   &
           ids,ide, jds,jde, kds,kde, &
           ims,ime, jms,jme, kms,kme, &
           its,ite, jts,jte, kts,kte)



      call cup_env_clev(t,qes,q,he,hes,z,p,qes_cup,q_cup,he_cup, &
           hes_cup,z_cup,p_cup,gamma_cup,t_cup,psur, &
           ierr,z1,xl,rv,cp,          &
           ids,ide, jds,jde, kds,kde, &
           ims,ime, jms,jme, kms,kme, &
           its,ite, jts,jte, kts,kte)
      call cup_env_clev(tn,qeso,qo,heo,heso,zo,po,qeso_cup,qo_cup, &
           heo_cup,heso_cup,zo_cup,po_cup,gammao_cup,tn_cup,psur,  &
           ierr,z1,xl,rv,cp,          &
           ids,ide, jds,jde, kds,kde, &
           ims,ime, jms,jme, kms,kme, &
           its,ite, jts,jte, kts,kte)
      do i=its,itf
      if(ierr(i).eq.0)then

      do k=kts,ktf-2
        if(zo_cup(i,k).gt.zkbmax+z1(i))then
          kbmax(i)=k
          go to 25
        endif
      enddo
 25   continue




      do k=kts,ktf
        if(zo_cup(i,k).gt.z_detr+z1(i))then
          kdet(i)=k
          go to 26
        endif
      enddo
 26   continue

      endif
      enddo





      CALL cup_MAXIMI(HEO_CUP,3,KBMAX,K22,ierr, &
           ids,ide, jds,jde, kds,kde, &
           ims,ime, jms,jme, kms,kme, &
           its,ite, jts,jte, kts,kte)
       DO 36 i=its,itf
         IF(ierr(I).eq.0.)THEN
         IF(K22(I).GE.KBMAX(i))ierr(i)=2
         endif
 36   CONTINUE



      call cup_kbcon(cap_max_increment,1,k22,kbcon,heo_cup,heso_cup, &
           ierr,kbmax,po_cup,cap_max, &
           ids,ide, jds,jde, kds,kde, &
           ims,ime, jms,jme, kms,kme, &
           its,ite, jts,jte, kts,kte)








      CALL cup_minimi(HEso_cup,Kbcon,kstabm,kstabi,ierr,  &
           ids,ide, jds,jde, kds,kde, &
           ims,ime, jms,jme, kms,kme, &
           its,ite, jts,jte, kts,kte)
      do i=its,itf
      IF(ierr(I).eq.0.)THEN
        if(kstabm(i)-1.gt.kstabi(i))then
           do k=kstabi(i),kstabm(i)-1
             cd(i,k)=cd(i,k-1)+1.5*entr_rate
             if(cd(i,k).gt.10.0*entr_rate)cd(i,k)=10.0*entr_rate
           enddo
        ENDIF
      ENDIF
      ENDDO



      call cup_up_he(k22,hkb,z_cup,cd,mentr_rate,he_cup,hc, &
           kbcon,ierr,dby,he,hes_cup, &
           ids,ide, jds,jde, kds,kde, &
           ims,ime, jms,jme, kms,kme, &
           its,ite, jts,jte, kts,kte)
      call cup_up_he(k22,hkbo,zo_cup,cd,mentr_rate,heo_cup,hco, &
           kbcon,ierr,dbyo,heo,heso_cup, &
           ids,ide, jds,jde, kds,kde, &
           ims,ime, jms,jme, kms,kme, &
           its,ite, jts,jte, kts,kte)



      call cup_ktop(1,dbyo,kbcon,ktop,ierr, &
           ids,ide, jds,jde, kds,kde, &
           ims,ime, jms,jme, kms,kme, &
           its,ite, jts,jte, kts,kte)
      DO 37 i=its,itf
         kzdown(i)=0
         if(ierr(i).eq.0)then
            zktop=(zo_cup(i,ktop(i))-z1(i))*.6
            zktop=min(zktop+z1(i),zcutdown+z1(i))
            do k=kts,kte
              if(zo_cup(i,k).gt.zktop)then
                 kzdown(i)=k
                 go to 37
              endif
              enddo
         endif
 37   CONTINUE



      call cup_minimi(HEso_cup,K22,kzdown,JMIN,ierr, &
           ids,ide, jds,jde, kds,kde, &
           ims,ime, jms,jme, kms,kme, &
           its,ite, jts,jte, kts,kte)
      DO 100 i=its,ite
        IF(ierr(I).eq.0.)THEN







        jmini = jmin(i)
        keep_going = .TRUE.
        DO WHILE ( keep_going )
          keep_going = .FALSE.
          if ( jmini - 1 .lt. kdet(i)   ) kdet(i) = jmini-1
          if ( jmini     .ge. ktop(i)-1 ) jmini = ktop(i) - 2
          ki = jmini
          hcdo(i,ki)=heso_cup(i,ki)
          DZ=Zo_cup(i,Ki+1)-Zo_cup(i,Ki)
          dh=0.
          DO k=ki-1,1,-1
            hcdo(i,k)=heso_cup(i,jmini)
            DZ=Zo_cup(i,K+1)-Zo_cup(i,K)
            dh=dh+dz*(HCDo(i,K)-heso_cup(i,k))
            IF(dh.gt.0.)THEN
              jmini=jmini-1
              IF ( jmini .gt. 3 ) THEN
                keep_going = .TRUE.
              ELSE
                ierr(i) = 9
                EXIT
              ENDIF
            ENDIF
          ENDDO
        ENDDO
        jmin(i) = jmini
        IF ( jmini .le. 3 ) THEN
          ierr(i)=4
        ENDIF

      ENDIF
100   CONTINUE




      do i=its,itf
      IF(ierr(I).eq.0.)THEN
      IF(-zo_cup(I,KBCON(I))+zo_cup(I,KTOP(I)).LT.depth_min)then
            ierr(i)=6
      endif
      endif
      enddo




      call cup_up_nms(zu,z_cup,mentr_rate,cd,kbcon,ktop,ierr,k22, &
           ids,ide, jds,jde, kds,kde, &
           ims,ime, jms,jme, kms,kme, &
           its,ite, jts,jte, kts,kte)
      call cup_up_nms(zuo,zo_cup,mentr_rate,cd,kbcon,ktop,ierr,k22, &
           ids,ide, jds,jde, kds,kde, &
           ims,ime, jms,jme, kms,kme, &
           its,ite, jts,jte, kts,kte)




      call cup_dd_nms(zd,z_cup,cdd,mentrd_rate,jmin,ierr, &
           0,kdet,z1,                 &
           ids,ide, jds,jde, kds,kde, &
           ims,ime, jms,jme, kms,kme, &
           its,ite, jts,jte, kts,kte)
      call cup_dd_nms(zdo,zo_cup,cdd,mentrd_rate,jmin,ierr, &
           1,kdet,z1,                 &
           ids,ide, jds,jde, kds,kde, &
           ims,ime, jms,jme, kms,kme, &
           its,ite, jts,jte, kts,kte)



      call cup_dd_he(hes_cup,zd,hcd,z_cup,cdd,mentrd_rate, &
           jmin,ierr,he,dbyd,he_cup,  &
           ids,ide, jds,jde, kds,kde, &
           ims,ime, jms,jme, kms,kme, &
           its,ite, jts,jte, kts,kte)
      call cup_dd_he(heso_cup,zdo,hcdo,zo_cup,cdd,mentrd_rate, &
           jmin,ierr,heo,dbydo,he_cup,&
           ids,ide, jds,jde, kds,kde, &
           ims,ime, jms,jme, kms,kme, &
           its,ite, jts,jte, kts,kte)



      call cup_dd_moisture(zd,hcd,hes_cup,qcd,qes_cup, &
           pwd,q_cup,z_cup,cdd,mentrd_rate,jmin,ierr,gamma_cup, &
           pwev,bu,qrcd,q,he,t_cup,2,xl, &
           ids,ide, jds,jde, kds,kde, &
           ims,ime, jms,jme, kms,kme, &
           its,ite, jts,jte, kts,kte)
      call cup_dd_moisture(zdo,hcdo,heso_cup,qcdo,qeso_cup, &
           pwdo,qo_cup,zo_cup,cdd,mentrd_rate,jmin,ierr,gammao_cup, &
           pwevo,bu,qrcdo,qo,heo,tn_cup,1,xl, &
           ids,ide, jds,jde, kds,kde, &
           ims,ime, jms,jme, kms,kme, &
           its,ite, jts,jte, kts,kte)



      call cup_up_moisture(ierr,z_cup,qc,qrc,pw,pwav, &
           kbcon,ktop,cd,dby,mentr_rate,clw_all,      &
           q,GAMMA_cup,zu,qes_cup,k22,q_cup,xl, &
           ids,ide, jds,jde, kds,kde, &
           ims,ime, jms,jme, kms,kme, &
           its,ite, jts,jte, kts,kte)
      do k=kts,ktf
      do i=its,itf
         cupclw(i,k)=qrc(i,k)
      enddo
      enddo
      call cup_up_moisture(ierr,zo_cup,qco,qrco,pwo,pwavo, &
           kbcon,ktop,cd,dbyo,mentr_rate,clw_all, &
           qo,GAMMAo_cup,zuo,qeso_cup,k22,qo_cup,xl,&
           ids,ide, jds,jde, kds,kde, &
           ims,ime, jms,jme, kms,kme, &
           its,ite, jts,jte, kts,kte)



      call cup_up_aa0(aa0,z,zu,dby,GAMMA_CUP,t_cup, &
           kbcon,ktop,ierr,           &
           ids,ide, jds,jde, kds,kde, &
           ims,ime, jms,jme, kms,kme, &
           its,ite, jts,jte, kts,kte)
      call cup_up_aa0(aa1,zo,zuo,dbyo,GAMMAo_CUP,tn_cup, &
           kbcon,ktop,ierr,           &
           ids,ide, jds,jde, kds,kde, &
           ims,ime, jms,jme, kms,kme, &
           its,ite, jts,jte, kts,kte)
      do i=its,itf
         if(ierr(i).eq.0)then
           if(aa1(i).eq.0.)then
               ierr(i)=17
           endif
         endif
      enddo



      call cup_dd_edt(ierr,us,vs,zo,ktop,kbcon,edt,po,pwavo, &
           pwevo,edtmax,edtmin,maxens2,edtc, &
           ids,ide, jds,jde, kds,kde, &
           ims,ime, jms,jme, kms,kme, &
           its,ite, jts,jte, kts,kte)
      do 250 iedt=1,maxens2
        do i=its,itf
         if(ierr(i).eq.0)then
         edt(i)=edtc(i,iedt)
         edto(i)=edtc(i,iedt)
         edtx(i)=edtc(i,iedt)
         endif
        enddo
        do k=kts,ktf
        do i=its,itf
           dellat_ens(i,k,iedt)=0.
           dellaq_ens(i,k,iedt)=0.
           dellaqc_ens(i,k,iedt)=0.
           pwo_ens(i,k,iedt)=0.
        enddo
        enddo
        if (l_flux) then
           do k=kts,ktf
              do i=its,itf
                 cfu1_ens(i,k,iedt)=0.
                 cfd1_ens(i,k,iedt)=0.
                 dfu1_ens(i,k,iedt)=0.
                 efu1_ens(i,k,iedt)=0.
                 dfd1_ens(i,k,iedt)=0.
                 efd1_ens(i,k,iedt)=0.
              enddo
           enddo
        endif

      do i=its,itf
        aad(i)=0.
      enddo


























      call cup_dellabot(ipr,jpr,heo_cup,ierr,zo_cup,po,hcdo,edto, &
           zuo,zdo,cdd,heo,dellah,j,mentrd_rate,zo,g, &
           CFU1,CFD1,DFU1,EFU1,DFD1,EFD1,l_flux,               &
           ids,ide, jds,jde, kds,kde, &
           ims,ime, jms,jme, kms,kme, &
           its,ite, jts,jte, kts,kte)
      call cup_dellabot(ipr,jpr,qo_cup,ierr,zo_cup,po,qrcdo,edto, &
           zuo,zdo,cdd,qo,dellaq,j,mentrd_rate,zo,g,&
           CFU1,CFD1,DFU1,EFU1,DFD1,EFD1,.FALSE.,               &
           ids,ide, jds,jde, kds,kde, &
           ims,ime, jms,jme, kms,kme, &
           its,ite, jts,jte, kts,kte)



      call cup_dellas(ierr,zo_cup,po_cup,hcdo,edto,zdo,cdd,    &
           heo,dellah,j,mentrd_rate,zuo,g,                     &
           cd,hco,ktop,k22,kbcon,mentr_rate,jmin,heo_cup,kdet, &
           k22,ipr,jpr,'deep',                                 &
           CFU1,CFD1,DFU1,EFU1,DFD1,EFD1,l_flux,               &
           ids,ide, jds,jde, kds,kde, &
           ims,ime, jms,jme, kms,kme, &
           its,ite, jts,jte, kts,kte)




      do k=kts,ktf-1
      do i=its,itf
       scr1(i,k)=0.
       dellaqc(i,k)=0.
       if(ierr(i).eq.0)then

         scr1(i,k)=qco(i,k)-qrco(i,k)
         if(k.eq.ktop(i)-0)dellaqc(i,k)= &
                      .01*zuo(i,ktop(i))*qrco(i,ktop(i))* &
                      9.81/(po_cup(i,k)-po_cup(i,k+1))
         if(k.lt.ktop(i).and.k.gt.kbcon(i))then
           dz=zo_cup(i,k+1)-zo_cup(i,k)
           dellaqc(i,k)=.01*9.81*cd(i,k)*dz*zuo(i,k) &
                        *.5*(qrco(i,k)+qrco(i,k+1))/ &
                        (po_cup(i,k)-po_cup(i,k+1))
         endif
       endif
      enddo
      enddo
      call cup_dellas(ierr,zo_cup,po_cup,qrcdo,edto,zdo,cdd, &
           qo,dellaq,j,mentrd_rate,zuo,g, &
           cd,scr1,ktop,k22,kbcon,mentr_rate,jmin,qo_cup,kdet, &
           k22,ipr,jpr,'deep',               &
           CFU1,CFD1,DFU1,EFU1,DFD1,EFD1,.FALSE.,            &
              ids,ide, jds,jde, kds,kde,                     &
              ims,ime, jms,jme, kms,kme,                     &
              its,ite, jts,jte, kts,kte    )




      mbdt=mbdt_ens(2)
      do i=its,itf
      xaa0_ens(i,1)=0.
      xaa0_ens(i,2)=0.
      xaa0_ens(i,3)=0.
      enddo





      do k=kts,ktf
      do i=its,itf
         dellat(i,k)=0.
         if(ierr(i).eq.0)then
            XHE(I,K)=DELLAH(I,K)*MBDT+HEO(I,K)
            XQ(I,K)=DELLAQ(I,K)*MBDT+QO(I,K)
            DELLAT(I,K)=(1./cp)*(DELLAH(I,K)-xl*DELLAQ(I,K))
            XT(I,K)= DELLAT(I,K)*MBDT+TN(I,K)
            IF(XQ(I,K).LE.0.)XQ(I,K)=1.E-08



         ENDIF
      enddo
      enddo
      do i=its,itf
      if(ierr(i).eq.0)then
      XHE(I,ktf)=HEO(I,ktf)
      XQ(I,ktf)=QO(I,ktf)
      XT(I,ktf)=TN(I,ktf)
      IF(XQ(I,ktf).LE.0.)XQ(I,ktf)=1.E-08
      endif
      enddo



      call cup_env(xz,xqes,xhe,xhes,xt,xq,po,z1, &
           psur,ierr,tcrit,2,xl,cp,   &
           ids,ide, jds,jde, kds,kde, &
           ims,ime, jms,jme, kms,kme, &
           its,ite, jts,jte, kts,kte)



      call cup_env_clev(xt,xqes,xq,xhe,xhes,xz,po,xqes_cup,xq_cup, &
           xhe_cup,xhes_cup,xz_cup,po_cup,gamma_cup,xt_cup,psur,   &
           ierr,z1,xl,rv,cp,          &
           ids,ide, jds,jde, kds,kde, &
           ims,ime, jms,jme, kms,kme, &
           its,ite, jts,jte, kts,kte)






      do i=its,itf
        if(ierr(i).eq.0)then
          xhkb(i)=xhe(i,k22(i))
        endif
      enddo
      call cup_up_he(k22,xhkb,xz_cup,cd,mentr_rate,xhe_cup,xhc, &
           kbcon,ierr,xdby,xhe,xhes_cup, &
           ids,ide, jds,jde, kds,kde, &
           ims,ime, jms,jme, kms,kme, &
           its,ite, jts,jte, kts,kte)



      call cup_up_nms(xzu,xz_cup,mentr_rate,cd,kbcon,ktop,ierr,k22, &
           ids,ide, jds,jde, kds,kde, &
           ims,ime, jms,jme, kms,kme, &
           its,ite, jts,jte, kts,kte)



      call cup_dd_nms(xzd,xz_cup,cdd,mentrd_rate,jmin,ierr, &
           1,kdet,z1,                 &
           ids,ide, jds,jde, kds,kde, &
           ims,ime, jms,jme, kms,kme, &
           its,ite, jts,jte, kts,kte)
      call cup_dd_he(xhes_cup,xzd,xhcd,xz_cup,cdd,mentrd_rate, &
           jmin,ierr,xhe,dbyd,xhe_cup,&
           ids,ide, jds,jde, kds,kde, &
           ims,ime, jms,jme, kms,kme, &
           its,ite, jts,jte, kts,kte)
      call cup_dd_moisture(xzd,xhcd,xhes_cup,xqcd,xqes_cup, &
           xpwd,xq_cup,xz_cup,cdd,mentrd_rate,jmin,ierr,gamma_cup, &
           xpwev,bu,xqrcd,xq,xhe,xt_cup,3,xl, &
           ids,ide, jds,jde, kds,kde, &
           ims,ime, jms,jme, kms,kme, &
           its,ite, jts,jte, kts,kte)




      call cup_up_moisture(ierr,xz_cup,xqc,xqrc,xpw,xpwav, &
           kbcon,ktop,cd,xdby,mentr_rate,clw_all, &
           xq,GAMMA_cup,xzu,xqes_cup,k22,xq_cup,xl, &
           ids,ide, jds,jde, kds,kde, &
           ims,ime, jms,jme, kms,kme, &
           its,ite, jts,jte, kts,kte)



      call cup_up_aa0(xaa0,xz,xzu,xdby,GAMMA_CUP,xt_cup, &
           kbcon,ktop,ierr,           &
           ids,ide, jds,jde, kds,kde, &
           ims,ime, jms,jme, kms,kme, &
           its,ite, jts,jte, kts,kte)









      do 200 nens=1,maxens
      do i=its,itf 
         if(ierr(i).eq.0)then
           xaa0_ens(i,nens)=xaa0(i)
           nall=(iens-1)*maxens3*maxens*maxens2 &
                +(iedt-1)*maxens*maxens3 &
                +(nens-1)*maxens3
           do k=kts,ktf
              if(k.le.ktop(i))then
                 do nens3=1,maxens3
                 if(nens3.eq.7)then

                 pr_ens(i,j,nall+nens3)=pr_ens(i,j,nall+nens3)+ &
                                    pwo(i,k) 


                 else if(nens3.eq.8)then
                 pr_ens(i,j,nall+nens3)=pr_ens(i,j,nall+nens3)+ &
                                    pwo(i,k)

                 else if(nens3.eq.9)then
                 pr_ens(i,j,nall+nens3)=pr_ens(i,j,nall+nens3)+ &
                                    pwo(i,k)

                 else
                 pr_ens(i,j,nall+nens3)=pr_ens(i,j,nall+nens3)+ &
                                    pwo(i,k)+edto(i)*pwdo(i,k)
                 endif
                 enddo
              endif
           enddo
         if(pr_ens(i,j,nall+7).lt.1.e-6)then
            ierr(i)=18
            do nens3=1,maxens3
               pr_ens(i,j,nall+nens3)=0.
            enddo
         endif
         do nens3=1,maxens3
           if(pr_ens(i,j,nall+nens3).lt.1.e-4)then
            pr_ens(i,j,nall+nens3)=0.
           endif
         enddo
         endif
      enddo
 200  continue







      CALL cup_MAXIMI(HEO_CUP,3,KBMAX,K22x,ierr, &
           ids,ide, jds,jde, kds,kde, &
           ims,ime, jms,jme, kms,kme, &
           its,ite, jts,jte, kts,kte)
      do i=its,itf
         ierr2(i)=ierr(i)
         ierr3(i)=ierr(i)
      enddo
      call cup_kbcon(cap_max_increment,2,k22x,kbconx,heo_cup, &
           heso_cup,ierr2,kbmax,po_cup,cap_max, &
           ids,ide, jds,jde, kds,kde, &
           ims,ime, jms,jme, kms,kme, &
           its,ite, jts,jte, kts,kte)
      call cup_kbcon(cap_max_increment,3,k22x,kbconx,heo_cup, &
           heso_cup,ierr3,kbmax,po_cup,cap_max, &
           ids,ide, jds,jde, kds,kde, &
           ims,ime, jms,jme, kms,kme, &
           its,ite, jts,jte, kts,kte)



      call cup_forcing_ens(closure_n,xland1,aa0,aa1,xaa0_ens,mbdt_ens,dtime,   &
           ierr,ierr2,ierr3,xf_ens,j,'deeps',                 &
           maxens,iens,iedt,maxens2,maxens3,mconv,            &
           po_cup,ktop,omeg,zdo,k22,zuo,pr_ens,edto,kbcon,    &
           massflx,iact,direction,ensdim,massfln,ichoice,     &
           ids,ide, jds,jde, kds,kde, &
           ims,ime, jms,jme, kms,kme, &
           its,ite, jts,jte, kts,kte)

      do k=kts,ktf
      do i=its,itf
        if(ierr(i).eq.0)then
           dellat_ens(i,k,iedt)=dellat(i,k)
           dellaq_ens(i,k,iedt)=dellaq(i,k)
           dellaqc_ens(i,k,iedt)=dellaqc(i,k)
           pwo_ens(i,k,iedt)=pwo(i,k)+edt(i)*pwdo(i,k)
        else 
           dellat_ens(i,k,iedt)=0.
           dellaq_ens(i,k,iedt)=0.
           dellaqc_ens(i,k,iedt)=0.
           pwo_ens(i,k,iedt)=0.
        endif




      enddo
      enddo
      if (l_flux) then
         do k=kts,ktf
            do i=its,itf
               if(ierr(i).eq.0)then
                 cfu1_ens(i,k,iedt)=cfu1(i,k)
                 cfd1_ens(i,k,iedt)=cfd1(i,k)
                 dfu1_ens(i,k,iedt)=dfu1(i,k)
                 efu1_ens(i,k,iedt)=efu1(i,k)
                 dfd1_ens(i,k,iedt)=dfd1(i,k)
                 efd1_ens(i,k,iedt)=efd1(i,k)
              else
                 cfu1_ens(i,k,iedt)=0.
                 cfd1_ens(i,k,iedt)=0.
                 dfu1_ens(i,k,iedt)=0.
                 efu1_ens(i,k,iedt)=0.
                 dfd1_ens(i,k,iedt)=0.
                 efd1_ens(i,k,iedt)=0.
              end if
           end do
         end do
      end if

 250  continue



      call cup_output_ens(xf_ens,ierr,dellat_ens,dellaq_ens, &
           dellaqc_ens,outt,outq,outqc,pre,pwo_ens,xmb,ktop, &
           j,'deep',maxens2,maxens,iens,ierr2,ierr3,         &
           pr_ens,maxens3,ensdim,massfln,                    &
           APR_GR,APR_W,APR_MC,APR_ST,APR_AS,                &
           APR_CAPMA,APR_CAPME,APR_CAPMI,closure_n,xland1,   &
           outCFU1,outCFD1,outDFU1,outEFU1,outDFD1,outEFD1,  &
           CFU1_ens,CFD1_ens,DFU1_ens,EFU1_ens,DFD1_ens,EFD1_ens,  &
           l_flux,                                           &
           ids,ide, jds,jde, kds,kde, &
           ims,ime, jms,jme, kms,kme, &
           its,ite, jts,jte, kts,kte)
      do i=its,itf
           PRE(I)=MAX(PRE(I),0.)
      enddo




   END SUBROUTINE CUP_enss


   SUBROUTINE cup_dd_aa0(edt,ierr,aa0,jmin,gamma_cup,t_cup, &
              hcd,hes_cup,z,zd,                             &
              ids,ide, jds,jde, kds,kde,                    &
              ims,ime, jms,jme, kms,kme,                    &
              its,ite, jts,jte, kts,kte                    )

   IMPLICIT NONE




   

     integer                                                           &
        ,intent (in   )                   ::                           &
        ids,ide, jds,jde, kds,kde,                                     &
        ims,ime, jms,jme, kms,kme,                                     &
        its,ite, jts,jte, kts,kte
  
  
  
  
  
  
  
  
  
  
     real,    dimension (its:ite,kts:kte)                              &
        ,intent (in   )                   ::                           &
        z,zd,gamma_cup,t_cup,hes_cup,hcd
     real,    dimension (its:ite)                                      &
        ,intent (in   )                   ::                           &
        edt
     integer, dimension (its:ite)                                      &
        ,intent (in   )                   ::                           &
        jmin





     integer, dimension (its:ite)                                      &
        ,intent (inout)                   ::                           &
        ierr
     real,    dimension (its:ite)                                      &
        ,intent (out  )                   ::                           &
        aa0




     integer                              ::                           &
        i,k,kk
     real                                 ::                           &
        dz

     integer :: itf, ktf

     itf=MIN(ite,ide-1)
     ktf=MIN(kte,kde-1)


       DO k=kts,ktf-1
       do i=its,itf
         IF(ierr(I).eq.0.and.k.lt.jmin(i))then
         KK=JMIN(I)-K



         DZ=(Z(I,KK)-Z(I,KK+1))
         AA0(I)=AA0(I)+zd(i,kk)*EDT(I)*DZ*(9.81/(1004.*T_cup(I,KK))) &
            *((hcd(i,kk)-hes_cup(i,kk))/(1.+GAMMA_cup(i,kk)))
         endif
      enddo
      enddo

   END SUBROUTINE CUP_dd_aa0


   SUBROUTINE cup_dd_edt(ierr,us,vs,z,ktop,kbcon,edt,p,pwav, &
              pwev,edtmax,edtmin,maxens2,edtc,               &
              ids,ide, jds,jde, kds,kde,                     &
              ims,ime, jms,jme, kms,kme,                     &
              its,ite, jts,jte, kts,kte                     )

   IMPLICIT NONE

     integer                                                           &
        ,intent (in   )                   ::                           &
        ids,ide, jds,jde, kds,kde,           &
        ims,ime, jms,jme, kms,kme,           &
        its,ite, jts,jte, kts,kte
     integer, intent (in   )              ::                           &
        maxens2
  
  
  
     real,    dimension (its:ite,kts:kte)                              &
        ,intent (in   )                   ::                           &
        us,vs,z,p
     real,    dimension (its:ite,1:maxens2)                            &
        ,intent (out  )                   ::                           &
        edtc
     real,    dimension (its:ite)                                      &
        ,intent (out  )                   ::                           &
        edt
     real,    dimension (its:ite)                                      &
        ,intent (in   )                   ::                           &
        pwav,pwev
     real                                                              &
        ,intent (in   )                   ::                           &
        edtmax,edtmin
     integer, dimension (its:ite)                                      &
        ,intent (in   )                   ::                           &
        ktop,kbcon
     integer, dimension (its:ite)                                      &
        ,intent (inout)                   ::                           &
        ierr




     integer i,k,kk
     real    einc,pef,pefb,prezk,zkbc
     real,    dimension (its:ite)         ::                           &
      vshear,sdp,vws

     integer :: itf, ktf

     itf=MIN(ite,ide-1)
     ktf=MIN(kte,kde-1)





       do i=its,itf
        edt(i)=0.
        vws(i)=0.
        sdp(i)=0.
        vshear(i)=0.
       enddo
       do kk = kts,ktf-1
         do 62 i=its,itf
          IF(ierr(i).ne.0)GO TO 62
          if (kk .le. min0(ktop(i),ktf) .and. kk .ge. kbcon(i)) then
             vws(i) = vws(i)+ &
              (abs((us(i,kk+1)-us(i,kk))/(z(i,kk+1)-z(i,kk))) &
          +   abs((vs(i,kk+1)-vs(i,kk))/(z(i,kk+1)-z(i,kk)))) * &
              (p(i,kk) - p(i,kk+1))
            sdp(i) = sdp(i) + p(i,kk) - p(i,kk+1)
          endif
          if (kk .eq. ktf-1)vshear(i) = 1.e3 * vws(i) / sdp(i)
   62   continue
       end do
      do i=its,itf
         IF(ierr(i).eq.0)then
            pef=(1.591-.639*VSHEAR(I)+.0953*(VSHEAR(I)**2) &
               -.00496*(VSHEAR(I)**3))
            if(pef.gt.edtmax)pef=edtmax
            if(pef.lt.edtmin)pef=edtmin



            zkbc=z(i,kbcon(i))*3.281e-3
            prezk=.02
            if(zkbc.gt.3.)then
               prezk=.96729352+zkbc*(-.70034167+zkbc*(.162179896+zkbc &
               *(- 1.2569798E-2+zkbc*(4.2772E-4-zkbc*5.44E-6))))
            endif
            if(zkbc.gt.25)then
               prezk=2.4
            endif
            pefb=1./(1.+prezk)
            if(pefb.gt.edtmax)pefb=edtmax
            if(pefb.lt.edtmin)pefb=edtmin
            EDT(I)=1.-.5*(pefb+pef)




            einc=.2*edt(i)
            do k=1,maxens2
                edtc(i,k)=edt(i)+float(k-2)*einc
            enddo
         endif
      enddo
      do i=its,itf
         IF(ierr(i).eq.0)then
            do k=1,maxens2
               EDTC(I,K)=-EDTC(I,K)*PWAV(I)/PWEV(I)
               IF(EDTC(I,K).GT.edtmax)EDTC(I,K)=edtmax
               IF(EDTC(I,K).LT.edtmin)EDTC(I,K)=edtmin
            enddo
         endif
      enddo

   END SUBROUTINE cup_dd_edt


   SUBROUTINE cup_dd_he(hes_cup,zd,hcd,z_cup,cdd,entr,       &
              jmin,ierr,he,dby,he_cup,                       &
              ids,ide, jds,jde, kds,kde,                     &
              ims,ime, jms,jme, kms,kme,                     &
              its,ite, jts,jte, kts,kte                     )

   IMPLICIT NONE




   

     integer                                                           &
        ,intent (in   )                   ::                           &
                                  ids,ide, jds,jde, kds,kde,           &
                                  ims,ime, jms,jme, kms,kme,           &
                                  its,ite, jts,jte, kts,kte
  
  
  
  
  
  
  
  
  
  
     real,    dimension (its:ite,kts:kte)                              &
        ,intent (in   )                   ::                           &
        he,he_cup,hes_cup,z_cup,cdd,zd
  
     real                                                              &
        ,intent (in   )                   ::                           &
        entr
     integer, dimension (its:ite)                                      &
        ,intent (in   )                   ::                           &
        jmin




   

     integer, dimension (its:ite)                                      &
        ,intent (inout)                   ::                           &
        ierr

     real,    dimension (its:ite,kts:kte)                              &
        ,intent (out  )                   ::                           &
        hcd,dby




     integer                              ::                           &
        i,k,ki
     real                                 ::                           &
        dz

     integer :: itf, ktf

     itf=MIN(ite,ide-1)
     ktf=MIN(kte,kde-1)

      do k=kts+1,ktf
      do i=its,itf
      dby(i,k)=0.
      IF(ierr(I).eq.0)then
         hcd(i,k)=hes_cup(i,k)
      endif
      enddo
      enddo

      do 100 i=its,itf
      IF(ierr(I).eq.0)then
      k=jmin(i)
      hcd(i,k)=hes_cup(i,k)
      dby(i,k)=hcd(i,jmin(i))-hes_cup(i,k)

      do ki=jmin(i)-1,1,-1
         DZ=Z_cup(i,Ki+1)-Z_cup(i,Ki)
         HCD(i,Ki)=(HCD(i,Ki+1)*(1.-.5*CDD(i,Ki)*DZ) &
                  +entr*DZ*HE(i,Ki) &
                  )/(1.+entr*DZ-.5*CDD(i,Ki)*DZ)
         dby(i,ki)=HCD(i,Ki)-hes_cup(i,ki)
      enddo

      endif

100    continue


   END SUBROUTINE cup_dd_he


   SUBROUTINE cup_dd_moisture(zd,hcd,hes_cup,qcd,qes_cup,    &
              pwd,q_cup,z_cup,cdd,entr,jmin,ierr,            &
              gamma_cup,pwev,bu,qrcd,                        &
              q,he,t_cup,iloop,xl,                           &
              ids,ide, jds,jde, kds,kde,                     &
              ims,ime, jms,jme, kms,kme,                     &
              its,ite, jts,jte, kts,kte                     )

   IMPLICIT NONE

     integer                                                           &
        ,intent (in   )                   ::                           &
                                  ids,ide, jds,jde, kds,kde,           &
                                  ims,ime, jms,jme, kms,kme,           &
                                  its,ite, jts,jte, kts,kte
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
     real,    dimension (its:ite,kts:kte)                              &
        ,intent (in   )                   ::                           &
        zd,t_cup,hes_cup,hcd,qes_cup,q_cup,z_cup,cdd,gamma_cup,q,he 
     real                                                              &
        ,intent (in   )                   ::                           &
        entr,xl
     integer                                                           &
        ,intent (in   )                   ::                           &
        iloop
     integer, dimension (its:ite)                                      &
        ,intent (in   )                   ::                           &
        jmin
     integer, dimension (its:ite)                                      &
        ,intent (inout)                   ::                           &
        ierr
     real,    dimension (its:ite,kts:kte)                              &
        ,intent (out  )                   ::                           &
        qcd,qrcd,pwd
     real,    dimension (its:ite)                                      &
        ,intent (out  )                   ::                           &
        pwev,bu




     integer                              ::                           &
        i,k,ki
     real                                 ::                           &
        dh,dz,dqeva

     integer :: itf, ktf

     itf=MIN(ite,ide-1)
     ktf=MIN(kte,kde-1)

      do i=its,itf
         bu(i)=0.
         pwev(i)=0.
      enddo
      do k=kts,ktf
      do i=its,itf
         qcd(i,k)=0.
         qrcd(i,k)=0.
         pwd(i,k)=0.
      enddo
      enddo



      do 100 i=its,itf
      IF(ierr(I).eq.0)then
      k=jmin(i)
      DZ=Z_cup(i,K+1)-Z_cup(i,K)
      qcd(i,k)=q_cup(i,k)

      qrcd(i,k)=qes_cup(i,k)
      pwd(i,jmin(i))=min(0.,qcd(i,k)-qrcd(i,k))
      pwev(i)=pwev(i)+pwd(i,jmin(i))
      qcd(i,k)=qes_cup(i,k)

      DH=HCD(I,k)-HES_cup(I,K)
      bu(i)=dz*dh
      do ki=jmin(i)-1,1,-1
         DZ=Z_cup(i,Ki+1)-Z_cup(i,Ki)
         QCD(i,Ki)=(qCD(i,Ki+1)*(1.-.5*CDD(i,Ki)*DZ) &
                  +entr*DZ*q(i,Ki) &
                  )/(1.+entr*DZ-.5*CDD(i,Ki)*DZ)



         DH=HCD(I,ki)-HES_cup(I,Ki)
         bu(i)=bu(i)+dz*dh
         QRCD(I,Ki)=qes_cup(i,ki)+(1./XL)*(GAMMA_cup(i,ki) &
                  /(1.+GAMMA_cup(i,ki)))*DH
         dqeva=qcd(i,ki)-qrcd(i,ki)
         if(dqeva.gt.0.)dqeva=0.
         pwd(i,ki)=zd(i,ki)*dqeva
         qcd(i,ki)=qrcd(i,ki)
         pwev(i)=pwev(i)+pwd(i,ki)



      enddo


       if(pwev(I).eq.0.and.iloop.eq.1)then

         ierr(i)=7
       endif
       if(BU(I).GE.0.and.iloop.eq.1)then

         ierr(i)=7
       endif
      endif
100    continue

   END SUBROUTINE cup_dd_moisture


   SUBROUTINE cup_dd_nms(zd,z_cup,cdd,entr,jmin,ierr,        &
              itest,kdet,z1,                                 &
              ids,ide, jds,jde, kds,kde,                     &
              ims,ime, jms,jme, kms,kme,                     &
              its,ite, jts,jte, kts,kte                     )

   IMPLICIT NONE




   

     integer                                                           &
        ,intent (in   )                   ::                           &
                                  ids,ide, jds,jde, kds,kde,           &
                                  ims,ime, jms,jme, kms,kme,           &
                                  its,ite, jts,jte, kts,kte
  
  
  
  
  
  
  
     real,    dimension (its:ite,kts:kte)                              &
        ,intent (in   )                   ::                           &
        z_cup
     real,    dimension (its:ite)                                      &
        ,intent (in   )                   ::                           &
        z1 
     real                                                              &
        ,intent (in   )                   ::                           &
        entr 
     integer, dimension (its:ite)                                      &
        ,intent (in   )                   ::                           &
        jmin,kdet
     integer                                                           &
        ,intent (in   )                   ::                           &
        itest




   

     integer, dimension (its:ite)                                      &
        ,intent (inout)                   ::                           &
                                                                 ierr
   
   

     real,    dimension (its:ite,kts:kte)                              &
        ,intent (out  )                   ::                           &
                                                             zd
     real,    dimension (its:ite,kts:kte)                              &
        ,intent (inout)                   ::                           &
                                                             cdd




     integer                              ::                           &
                                                  i,k,ki
     real                                 ::                           &
                                            a,perc,dz

     integer :: itf, ktf

     itf=MIN(ite,ide-1)
     ktf=MIN(kte,kde-1)



      perc=.03

      do k=kts,ktf
      do i=its,itf
         zd(i,k)=0.
         if(itest.eq.0)cdd(i,k)=0.
      enddo
      enddo
      a=1.-perc



      do 100 i=its,itf
      IF(ierr(I).eq.0)then
      zd(i,jmin(i))=1.



      do ki=jmin(i)-1,1,-1
         DZ=Z_cup(i,Ki+1)-Z_cup(i,Ki)
         if(ki.le.kdet(i).and.itest.eq.0)then
           cdd(i,ki)=entr+(1.- (a*(z_cup(i,ki)-z1(i)) &
                     +perc*(z_cup(i,kdet(i))-z1(i)) ) &
                         /(a*(z_cup(i,ki+1)-z1(i)) &
                      +perc*(z_cup(i,kdet(i))-z1(i))))/dz
         endif
         zd(i,ki)=zd(i,ki+1)*(1.+(entr-cdd(i,ki))*dz)
      enddo

      endif

100    continue

   END SUBROUTINE cup_dd_nms


   SUBROUTINE cup_dellabot(ipr,jpr,he_cup,ierr,z_cup,p_cup,  &
              hcd,edt,zu,zd,cdd,he,della,j,mentrd_rate,z,g,     &
              CFU1,CFD1,DFU1,EFU1,DFD1,EFD1,l_flux,  &
              ids,ide, jds,jde, kds,kde,                     &
              ims,ime, jms,jme, kms,kme,                     &
              its,ite, jts,jte, kts,kte                     )

   IMPLICIT NONE

     integer                                                           &
        ,intent (in   )                   ::                           &
        ids,ide, jds,jde, kds,kde,           &
        ims,ime, jms,jme, kms,kme,           &
        its,ite, jts,jte, kts,kte
     integer, intent (in   )              ::                           &
        j,ipr,jpr
  
  
  
     real,    dimension (its:ite,kts:kte)                              &
        ,intent (out  )                   ::                           &
        della
     real,    dimension (its:ite,kts:kte)                              &
        ,intent (in  )                   ::                           &
        z_cup,p_cup,hcd,zu,zd,cdd,he,z,he_cup
     real,    dimension (its:ite)                                      &
        ,intent (in   )                   ::                           &
        edt
     real                                                              &
        ,intent (in   )                   ::                           &
        g,mentrd_rate
     integer, dimension (its:ite)                                      &
        ,intent (inout)                   ::                           &
        ierr
     real,    dimension (its:ite,kts:kte)                              &
        ,intent (inout  )                 ::                           &
        CFU1,CFD1,DFU1,EFU1,DFD1,EFD1
     logical, intent(in) :: l_flux




      integer i
      real detdo,detdo1,detdo2,entdo,dp,dz,subin,                      &
      totmas

     integer :: itf, ktf

     itf=MIN(ite,ide-1)
     ktf=MIN(kte,kde-1)



      do 100 i=its,itf
      if (l_flux) then
         cfu1(i,1)=0.
         cfd1(i,1)=0.
         cfu1(i,2)=0.
         cfd1(i,2)=0.
         dfu1(i,1)=0.
         efu1(i,1)=0.
         dfd1(i,1)=0.
         efd1(i,1)=0.
      endif
      della(i,1)=0.
      if(ierr(i).ne.0)go to 100
      dz=z_cup(i,2)-z_cup(i,1)
      DP=100.*(p_cup(i,1)-P_cup(i,2))
      detdo1=edt(i)*zd(i,2)*CDD(i,1)*DZ
      detdo2=edt(i)*zd(i,1)
      entdo=edt(i)*zd(i,2)*mentrd_rate*dz
      subin=-EDT(I)*zd(i,2)
      detdo=detdo1+detdo2-entdo+subin
      DELLA(I,1)=(detdo1*.5*(HCD(i,1)+HCD(i,2)) &
                 +detdo2*hcd(i,1) &
                 +subin*he_cup(i,2) &
                 -entdo*he(i,1))*g/dp
      if (l_flux) then
         cfd1(i,2)   = -edt(i)*zd(i,2)  
         dfd1(i,1)   =  detdo1+detdo2
         efd1(i,1)   = -entdo
      endif
 100  CONTINUE

   END SUBROUTINE cup_dellabot


   SUBROUTINE cup_dellas(ierr,z_cup,p_cup,hcd,edt,zd,cdd,              &
              he,della,j,mentrd_rate,zu,g,                             &
              cd,hc,ktop,k22,kbcon,mentr_rate,jmin,he_cup,kdet,kpbl,   &
              ipr,jpr,name,                                            &
              CFU1,CFD1,DFU1,EFU1,DFD1,EFD1,l_flux,  &
              ids,ide, jds,jde, kds,kde,                               &
              ims,ime, jms,jme, kms,kme,                               &
              its,ite, jts,jte, kts,kte                               )

   IMPLICIT NONE

     integer                                                           &
        ,intent (in   )                   ::                           &
        ids,ide, jds,jde, kds,kde,           &
        ims,ime, jms,jme, kms,kme,           &
        its,ite, jts,jte, kts,kte
     integer, intent (in   )              ::                           &
        j,ipr,jpr
  
  
  
     real,    dimension (its:ite,kts:kte)                              &
        ,intent (out  )                   ::                           &
        della
     real,    dimension (its:ite,kts:kte)                              &
        ,intent (in  )                   ::                           &
        z_cup,p_cup,hcd,zd,cdd,he,hc,cd,zu,he_cup
     real,    dimension (its:ite)                                      &
        ,intent (in   )                   ::                           &
        edt
     real                                                              &
        ,intent (in   )                   ::                           &
        g,mentrd_rate,mentr_rate
     integer, dimension (its:ite)                                      &
        ,intent (in   )                   ::                           &
        kbcon,ktop,k22,jmin,kdet,kpbl
     integer, dimension (its:ite)                                      &
        ,intent (inout)                   ::                           &
        ierr
      character *(*), intent (in)        ::                           &
       name
     real,    dimension (its:ite,kts:kte)                              &
        ,intent (inout  )                 ::                           &
        CFU1,CFD1,DFU1,EFU1,DFD1,EFD1
     logical, intent(in) :: l_flux




      integer i,k
      real detdo1,detdo2,entdo,dp,dz,subin,detdo,entup,                &
      detup,subdown,entdoj,entupk,detupk,totmas

     integer :: itf, ktf

     itf=MIN(ite,ide-1)
     ktf=MIN(kte,kde-1)


      i=ipr




       DO K=kts+1,ktf
       do i=its,itf
          della(i,k)=0.
       enddo
       enddo
       if (l_flux) then
          DO K=kts+1,ktf-1
             do i=its,itf
                cfu1(i,k+1)=0.
                cfd1(i,k+1)=0.
             enddo
          enddo
          DO K=kts+1,ktf
             do i=its,itf
                dfu1(i,k)=0.
                efu1(i,k)=0.
                dfd1(i,k)=0.
                efd1(i,k)=0.
             enddo
          enddo
       endif

       DO 100 k=kts+1,ktf-1
       DO 100 i=its,ite
         IF(ierr(i).ne.0)GO TO 100
         IF(K.Gt.KTOP(I))GO TO 100




         DZ=Z_cup(I,K+1)-Z_cup(I,K)
         detdo=edt(i)*CDD(i,K)*DZ*ZD(i,k+1)
         entdo=edt(i)*mentrd_rate*dz*zd(i,k+1)
         subin=zu(i,k+1)-zd(i,k+1)*edt(i)
         entup=0.
         detup=0.
         if(k.ge.kbcon(i).and.k.lt.ktop(i))then
            entup=mentr_rate*dz*zu(i,k)
            detup=CD(i,K+1)*DZ*ZU(i,k)
         endif
         subdown=(zu(i,k)-zd(i,k)*edt(i))
         entdoj=0.
         entupk=0.
         detupk=0.

         if(k.eq.jmin(i))then
         entdoj=edt(i)*zd(i,k)
         endif

         if(k.eq.k22(i)-1)then
         entupk=zu(i,kpbl(i))
         endif

         if(k.gt.kdet(i))then
            detdo=0.
         endif

         if(k.eq.ktop(i)-0)then
         detupk=zu(i,ktop(i))
         subin=0.
         endif
         if(k.lt.kbcon(i))then
            detup=0.
         endif
         if (l_flux) then







            cfu1(i,k+1)   =  zu(i,k+1)
            cfd1(i,k+1)   = -edt(i)*zd(i,k+1)

            dfu1(i,k) =   detup+detupk
            efu1(i,k) = -(entup+entupk)
            dfd1(i,k) =   detdo
            efd1(i,k) = -(entdo+entdoj)
         endif



         totmas=subin-subdown+detup-entup-entdo+ &
                 detdo-entupk-entdoj+detupk






         if(abs(totmas).gt.1.e-6)then







         endif
         dp=100.*(p_cup(i,k-1)-p_cup(i,k))
         della(i,k)=(subin*he_cup(i,k+1) &
                    -subdown*he_cup(i,k) &
                    +detup*.5*(HC(i,K+1)+HC(i,K)) &
                    +detdo*.5*(HCD(i,K+1)+HCD(i,K)) &
                    -entup*he(i,k) &
                    -entdo*he(i,k) &
                    -entupk*he_cup(i,k22(i)) &
                    -entdoj*he_cup(i,jmin(i)) &
                    +detupk*hc(i,ktop(i)) &
                     )*g/dp








 100  CONTINUE

   END SUBROUTINE cup_dellas


   SUBROUTINE cup_direction2(i,j,dir,id,massflx,             &
              iresult,imass,massfld,                         &
              ids,ide, jds,jde, kds,kde,                     &
              ims,ime, jms,jme, kms,kme,                     &
              its,ite, jts,jte, kts,kte                     )

   IMPLICIT NONE

     integer                                                           &
        ,intent (in   )                   ::                           &
        ids,ide, jds,jde, kds,kde,           &
        ims,ime, jms,jme, kms,kme,           &
        its,ite, jts,jte, kts,kte
     integer, intent (in   )              ::                           &
        i,j,imass
     integer, intent (out  )              ::                           &
        iresult
  
  
  
     integer,    dimension (ims:ime,jms:jme)                           &
        ,intent (in   )                   ::                           &
        id
     real,    dimension (ims:ime,jms:jme)                              &
        ,intent (in   )                   ::                           &
        massflx
     real,    dimension (its:ite)                                      &
        ,intent (inout)                   ::                           &
        dir
     real                                                              &
        ,intent (out  )                   ::                           &
        massfld




       integer k,ia,ja,ib,jb
       real diff



       if(imass.eq.1)then
           massfld=massflx(i,j)
       endif
       iresult=0

       diff=22.5
       if(dir(i).lt.22.5)dir(i)=360.+dir(i)
       if(id(i,j).eq.1)iresult=1




       ja=j-1
       ia=i-1
       jb=j+1
       ib=i+1
        if(dir(i).gt.90.-diff.and.dir(i).le.90.+diff)then

          if(id(ib,j).eq.1)then
            iresult=1
            if(imass.eq.1)then
               massfld=max(massflx(ib,j),massflx(i,j))
            endif
            return
          endif
        else if(dir(i).gt.135.-diff.and.dir(i).le.135.+diff)then

          if(id(ib,ja).eq.1)then
            iresult=1
            if(imass.eq.1)then
               massfld=max(massflx(ib,ja),massflx(i,j))
            endif
            return
          endif

        else if(dir(i).gt.180.-diff.and.dir(i).le.180.+diff)then
          if(id(i,ja).eq.1)then
            iresult=1
            if(imass.eq.1)then
               massfld=max(massflx(i,ja),massflx(i,j))
            endif
            return
          endif

        else if(dir(i).gt.225.-diff.and.dir(i).le.225.+diff)then
          if(id(ia,ja).eq.1)then
            iresult=1
            if(imass.eq.1)then
               massfld=max(massflx(ia,ja),massflx(i,j))
            endif
            return
          endif

        else if(dir(i).gt.270.-diff.and.dir(i).le.270.+diff)then
          if(id(ia,j).eq.1)then
            iresult=1
            if(imass.eq.1)then
               massfld=max(massflx(ia,j),massflx(i,j))
            endif
            return
          endif

        else if(dir(i).gt.305.-diff.and.dir(i).le.305.+diff)then
          if(id(ia,jb).eq.1)then
            iresult=1
            if(imass.eq.1)then
               massfld=max(massflx(ia,jb),massflx(i,j))
            endif
            return
          endif

        else if(dir(i).gt.360.-diff.and.dir(i).le.360.+diff)then
          if(id(i,jb).eq.1)then
            iresult=1
            if(imass.eq.1)then
               massfld=max(massflx(i,jb),massflx(i,j))
            endif
            return
          endif

        else if(dir(i).gt.45.-diff.and.dir(i).le.45.+diff)then
          if(id(ib,jb).eq.1)then
            iresult=1
            if(imass.eq.1)then
               massfld=max(massflx(ib,jb),massflx(i,j))
            endif
            return
          endif
        endif

   END SUBROUTINE cup_direction2


   SUBROUTINE cup_env(z,qes,he,hes,t,q,p,z1,                 &
              psur,ierr,tcrit,itest,xl,cp,                   &
              ids,ide, jds,jde, kds,kde,                     &
              ims,ime, jms,jme, kms,kme,                     &
              its,ite, jts,jte, kts,kte                     )

   IMPLICIT NONE

     integer                                                           &
        ,intent (in   )                   ::                           &
        ids,ide, jds,jde, kds,kde,           &
        ims,ime, jms,jme, kms,kme,           &
        its,ite, jts,jte, kts,kte
  
  
  
  
  
  
  
  
  
  
  
  
  
  
     real,    dimension (its:ite,kts:kte)                              &
        ,intent (in   )                   ::                           &
        p,t
     real,    dimension (its:ite,kts:kte)                              &
        ,intent (out  )                   ::                           &
        he,hes,qes
     real,    dimension (its:ite,kts:kte)                              &
        ,intent (inout)                   ::                           &
        z,q
     real,    dimension (its:ite)                                      &
        ,intent (in   )                   ::                           &
        psur,z1
     real                                                              &
        ,intent (in   )                   ::                           &
        xl,cp
     integer, dimension (its:ite)                                      &
        ,intent (inout)                   ::                           &
        ierr
     integer                                                           &
        ,intent (in   )                   ::                           &
        itest




     integer                              ::                           &
       i,k,iph
      real, dimension (1:2) :: AE,BE,HT
      real, dimension (its:ite,kts:kte) :: tv
      real :: tcrit,e,tvbar

     integer :: itf, ktf

     itf=MIN(ite,ide-1)
     ktf=MIN(kte,kde-1)

      HT(1)=XL/CP
      HT(2)=2.834E6/CP
      BE(1)=.622*HT(1)/.286
      AE(1)=BE(1)/273.+ALOG(610.71)
      BE(2)=.622*HT(2)/.286
      AE(2)=BE(2)/273.+ALOG(610.71)

      DO k=kts,ktf
      do i=its,itf
        if(ierr(i).eq.0)then

        IPH=1
        IF(T(I,K).LE.TCRIT)IPH=2

        E=EXP(AE(IPH)-BE(IPH)/T(I,K))

        QES(I,K)=.622*E/(100.*P(I,K)-E)
        IF(QES(I,K).LE.1.E-08)QES(I,K)=1.E-08
        IF(Q(I,K).GT.QES(I,K))Q(I,K)=QES(I,K)
        TV(I,K)=T(I,K)+.608*Q(I,K)*T(I,K)
        endif
      enddo
      enddo




      if(itest.ne.2)then
         do i=its,itf
           if(ierr(i).eq.0)then
             Z(I,1)=max(0.,Z1(I))-(ALOG(P(I,1))- &
                 ALOG(PSUR(I)))*287.*TV(I,1)/9.81
           endif
         enddo


         DO K=kts+1,ktf
         do i=its,itf
           if(ierr(i).eq.0)then
              TVBAR=.5*TV(I,K)+.5*TV(I,K-1)
              Z(I,K)=Z(I,K-1)-(ALOG(P(I,K))- &
               ALOG(P(I,K-1)))*287.*TVBAR/9.81
           endif
         enddo
         enddo
      else
         do k=kts,ktf
         do i=its,itf
           if(ierr(i).eq.0)then
             z(i,k)=(he(i,k)-1004.*t(i,k)-2.5e6*q(i,k))/9.81
             z(i,k)=max(1.e-3,z(i,k))
           endif
         enddo
         enddo
      endif




       DO k=kts,ktf
       do i=its,itf
         if(ierr(i).eq.0)then
         if(itest.eq.0)HE(I,K)=9.81*Z(I,K)+1004.*T(I,K)+2.5E06*Q(I,K)
         HES(I,K)=9.81*Z(I,K)+1004.*T(I,K)+2.5E06*QES(I,K)
         IF(HE(I,K).GE.HES(I,K))HE(I,K)=HES(I,K)



         endif
      enddo
      enddo

   END SUBROUTINE cup_env


   SUBROUTINE cup_env_clev(t,qes,q,he,hes,z,p,qes_cup,q_cup,   &
              he_cup,hes_cup,z_cup,p_cup,gamma_cup,t_cup,psur, &
              ierr,z1,xl,rv,cp,                                &
              ids,ide, jds,jde, kds,kde,                       &
              ims,ime, jms,jme, kms,kme,                       &
              its,ite, jts,jte, kts,kte                       )

   IMPLICIT NONE

     integer                                                           &
        ,intent (in   )                   ::                           &
        ids,ide, jds,jde, kds,kde,           &
        ims,ime, jms,jme, kms,kme,           &
        its,ite, jts,jte, kts,kte
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
     real,    dimension (its:ite,kts:kte)                              &
        ,intent (in   )                   ::                           &
        qes,q,he,hes,z,p,t
     real,    dimension (its:ite,kts:kte)                              &
        ,intent (out  )                   ::                           &
        qes_cup,q_cup,he_cup,hes_cup,z_cup,p_cup,gamma_cup,t_cup
     real,    dimension (its:ite)                                      &
        ,intent (in   )                   ::                           &
        psur,z1
     real                                                              &
        ,intent (in   )                   ::                           &
        xl,rv,cp
     integer, dimension (its:ite)                                      &
        ,intent (inout)                   ::                           &
        ierr




     integer                              ::                           &
       i,k

     integer :: itf, ktf

     itf=MIN(ite,ide-1)
     ktf=MIN(kte,kde-1)

      do k=kts+1,ktf
      do i=its,itf
        if(ierr(i).eq.0)then
        qes_cup(i,k)=.5*(qes(i,k-1)+qes(i,k))
        q_cup(i,k)=.5*(q(i,k-1)+q(i,k))
        hes_cup(i,k)=.5*(hes(i,k-1)+hes(i,k))
        he_cup(i,k)=.5*(he(i,k-1)+he(i,k))
        if(he_cup(i,k).gt.hes_cup(i,k))he_cup(i,k)=hes_cup(i,k)
        z_cup(i,k)=.5*(z(i,k-1)+z(i,k))
        p_cup(i,k)=.5*(p(i,k-1)+p(i,k))
        t_cup(i,k)=.5*(t(i,k-1)+t(i,k))
        gamma_cup(i,k)=(xl/cp)*(xl/(rv*t_cup(i,k) &
                       *t_cup(i,k)))*qes_cup(i,k)
        endif
      enddo
      enddo
      do i=its,itf
        if(ierr(i).eq.0)then
        qes_cup(i,1)=qes(i,1)
        q_cup(i,1)=q(i,1)
        hes_cup(i,1)=hes(i,1)
        he_cup(i,1)=he(i,1)
        z_cup(i,1)=.5*(z(i,1)+z1(i))
        p_cup(i,1)=.5*(p(i,1)+psur(i))
        t_cup(i,1)=t(i,1)
        gamma_cup(i,1)=xl/cp*(xl/(rv*t_cup(i,1) &
                       *t_cup(i,1)))*qes_cup(i,1)
        endif
      enddo

   END SUBROUTINE cup_env_clev


   SUBROUTINE cup_forcing_ens(closure_n,xland,aa0,aa1,xaa0,mbdt,dtime,ierr,ierr2,ierr3,&
              xf_ens,j,name,maxens,iens,iedt,maxens2,maxens3,mconv,    &
              p_cup,ktop,omeg,zd,k22,zu,pr_ens,edt,kbcon,massflx,      &
              iact_old_gr,dir,ensdim,massfln,icoic,                    &
              ids,ide, jds,jde, kds,kde,                               &
              ims,ime, jms,jme, kms,kme,                               &
              its,ite, jts,jte, kts,kte                               )

   IMPLICIT NONE

     integer                                                           &
        ,intent (in   )                   ::                           &
        ids,ide, jds,jde, kds,kde,           &
        ims,ime, jms,jme, kms,kme,           &
        its,ite, jts,jte, kts,kte
     integer, intent (in   )              ::                           &
        j,ensdim,maxens,iens,iedt,maxens2,maxens3
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
     real,    dimension (ims:ime,jms:jme,1:ensdim)                     &
        ,intent (inout)                   ::                           &
        pr_ens
     real,    dimension (ims:ime,jms:jme,1:ensdim)                     &
        ,intent (out  )                   ::                           &
        xf_ens,massfln
     real,    dimension (ims:ime,jms:jme)                              &
        ,intent (in   )                   ::                           &
        massflx
     real,    dimension (its:ite,kts:kte)                              &
        ,intent (in   )                   ::                           &
        omeg,zd,zu,p_cup
     real,    dimension (its:ite,1:maxens)                             &
        ,intent (in   )                   ::                           &
        xaa0
     real,    dimension (its:ite)                                      &
        ,intent (in   )                   ::                           &
        aa1,edt,dir,mconv,xland
     real,    dimension (its:ite)                                      &
        ,intent (inout)                   ::                           &
        aa0,closure_n
     real,    dimension (1:maxens)                                     &
        ,intent (in   )                   ::                           &
        mbdt
     real                                                              &
        ,intent (in   )                   ::                           &
        dtime
     integer, dimension (its:ite,jts:jte)                              &
        ,intent (in   )                   ::                           &
        iact_old_gr
     integer, dimension (its:ite)                                      &
        ,intent (in   )                   ::                           &
        k22,kbcon,ktop
     integer, dimension (its:ite)                                      &
        ,intent (inout)                   ::                           &
        ierr,ierr2,ierr3
     integer                                                           &
        ,intent (in   )                   ::                           &
        icoic
      character *(*), intent (in)         ::                           &
       name




     real,    dimension (1:maxens3)       ::                           &
       xff_ens3
     real,    dimension (1:maxens)        ::                           &
       xk
     integer                              ::                           &
       i,k,nall,n,ne,nens,nens3,iresult,iresultd,iresulte,mkxcrt,kclim
     parameter (mkxcrt=15)
     real                                 ::                           &
       a1,massfld,xff0,xomg,aclim1,aclim2,aclim3,aclim4
     real,    dimension(1:mkxcrt)         ::                           &
       pcrit,acrit,acritt

     integer :: itf,nall2

     itf=MIN(ite,ide-1)

      DATA PCRIT/850.,800.,750.,700.,650.,600.,550.,500.,450.,400.,    &
                 350.,300.,250.,200.,150./
      DATA ACRIT/.0633,.0445,.0553,.0664,.075,.1082,.1521,.2216,       &
                 .3151,.3677,.41,.5255,.7663,1.1686,1.6851/

      DATA ACRITT/.203,.515,.521,.566,.625,.665,.659,.688,             &
                  .743,.813,.886,.947,1.138,1.377,1.896/

       nens=0



       DO 100 i=its,itf

          if(name.eq.'deeps'.and.ierr(i).gt.995)then

           aa0(i)=0.
           ierr(i)=0
          endif
          IF(ierr(i).eq.0)then

           do k=mkxcrt,1,-1
             if(p_cup(i,ktop(i)).lt.pcrit(k))then
               kclim=k
               go to 9
             endif
           enddo
           if(p_cup(i,ktop(i)).ge.pcrit(1))kclim=1
 9         continue
           kclim=max(kclim,1)
           k=max(kclim-1,1)
           aclim1=acrit(kclim)*1.e3
           aclim2=acrit(k)*1.e3
           aclim3=acritt(kclim)*1.e3
           aclim4=acritt(k)*1.e3









             if(name.eq.'deeps')then

                xff0= (AA1(I)-AA0(I))/DTIME
                xff_ens3(1)=(AA1(I)-AA0(I))/dtime
                xff_ens3(2)=.9*xff_ens3(1)
                xff_ens3(3)=1.1*xff_ens3(1)







                xff_ens3(4)=-omeg(i,k22(i))/9.81
                xff_ens3(5)=-omeg(i,kbcon(i))/9.81
                xff_ens3(6)=-omeg(i,1)/9.81
                do k=2,kbcon(i)-1
                  xomg=-omeg(i,k)/9.81
                  if(xomg.gt.xff_ens3(6))xff_ens3(6)=xomg
                enddo



                xff_ens3(7)=mconv(i)
                xff_ens3(8)=mconv(i)
                xff_ens3(9)=mconv(i)



                xff_ens3(10)=AA1(I)/(60.*20.)
                xff_ens3(11)=AA1(I)/(60.*30.)
                xff_ens3(12)=AA1(I)/(60.*40.)



                xff_ens3(13)=max(0.,(AA1(I)-aclim1)/dtime)
                xff_ens3(14)=max(0.,(AA1(I)-aclim2)/dtime)
                xff_ens3(15)=max(0.,(AA1(I)-aclim3)/dtime)
                xff_ens3(16)=max(0.,(AA1(I)-aclim4)/dtime)










                do nens=1,maxens
                   XK(nens)=(XAA0(I,nens)-AA1(I))/MBDT(2)
                   if(xk(nens).le.0.and.xk(nens).gt.-1.e-6) &
                           xk(nens)=-1.e-6
                   if(xk(nens).gt.0.and.xk(nens).lt.1.e-6) &
                           xk(nens)=1.e-6
                enddo



                do 350 ne=1,maxens











                   iresult=0
                   iresultd=0
                   iresulte=0
                   nall=(iens-1)*maxens3*maxens*maxens2 &
                        +(iedt-1)*maxens*maxens3 &
                        +(ne-1)*maxens3



                if(xland(i).lt.0.1)then
                 if(ierr2(i).gt.0.or.ierr3(i).gt.0)then



                      xff_ens3(1) =0.
                      massfln(i,j,nall+1)=0.
                      xff_ens3(2) =0.
                      massfln(i,j,nall+2)=0.
                      xff_ens3(3) =0.
                      massfln(i,j,nall+3)=0.
                      closure_n(i)=closure_n(i)-1.

                      xff_ens3(7) =0.
                      massfln(i,j,nall+7)=0.
                      xff_ens3(8) =0.
                      massfln(i,j,nall+8)=0.
                      xff_ens3(9) =0.

                      closure_n(i)=closure_n(i)-1.
                endif



                      xff_ens3(4) =0.
                      massfln(i,j,nall+4)=0.
                      xff_ens3(5) =0.
                      massfln(i,j,nall+5)=0.
                      xff_ens3(6) =0.
                      massfln(i,j,nall+6)=0.
                      closure_n(i)=closure_n(i)-3.

                      xff_ens3(10)=0.
                      massfln(i,j,nall+10)=0.
                      xff_ens3(11)=0.
                      massfln(i,j,nall+11)=0.
                      xff_ens3(12)=0.
                      massfln(i,j,nall+12)=0.
                      if(ne.eq.1)closure_n(i)=closure_n(i)-3
                      xff_ens3(13)=0.
                      massfln(i,j,nall+13)=0.
                      xff_ens3(14)=0.
                      massfln(i,j,nall+14)=0.
                      xff_ens3(15)=0.
                      massfln(i,j,nall+15)=0.
                      massfln(i,j,nall+16)=0.
                      if(ne.eq.1)closure_n(i)=closure_n(i)-4

                endif





                   massfld=0.













                   IF(XK(ne).lt.0.and.xff0.gt.0.)iresultd=1
                   iresulte=max(iresult,iresultd)
                   iresulte=1
                   if(iresulte.eq.1)then




                      if(xff0.gt.0.)then
                         xf_ens(i,j,nall+1)=max(0.,-xff_ens3(1)/xk(ne)) &
                                        +massfld
                         xf_ens(i,j,nall+2)=max(0.,-xff_ens3(2)/xk(ne)) &
                                        +massfld
                         xf_ens(i,j,nall+3)=max(0.,-xff_ens3(3)/xk(ne)) &
                                        +massfld
                         xf_ens(i,j,nall+13)=max(0.,-xff_ens3(13)/xk(ne)) &
                                        +massfld
                         xf_ens(i,j,nall+14)=max(0.,-xff_ens3(14)/xk(ne)) &
                                        +massfld
                         xf_ens(i,j,nall+15)=max(0.,-xff_ens3(15)/xk(ne)) &
                                        +massfld
                         xf_ens(i,j,nall+16)=max(0.,-xff_ens3(16)/xk(ne)) &
                                        +massfld
                      else
                         xf_ens(i,j,nall+1)=massfld
                         xf_ens(i,j,nall+2)=massfld
                         xf_ens(i,j,nall+3)=massfld
                         xf_ens(i,j,nall+13)=massfld
                         xf_ens(i,j,nall+14)=massfld
                         xf_ens(i,j,nall+15)=massfld
                         xf_ens(i,j,nall+16)=massfld
                      endif



                         xf_ens(i,j,nall+4)=max(0.,xff_ens3(4) &
                            +massfld)
                         xf_ens(i,j,nall+5)=max(0.,xff_ens3(5) &
                                        +massfld)
                         xf_ens(i,j,nall+6)=max(0.,xff_ens3(6) &
                                        +massfld)
                         a1=max(1.e-3,pr_ens(i,j,nall+7))
                         xf_ens(i,j,nall+7)=max(0.,xff_ens3(7) &
                                     /a1)
                         a1=max(1.e-3,pr_ens(i,j,nall+8))
                         xf_ens(i,j,nall+8)=max(0.,xff_ens3(8) &
                                     /a1)
                         a1=max(1.e-3,pr_ens(i,j,nall+9))
                         xf_ens(i,j,nall+9)=max(0.,xff_ens3(9) &
                                     /a1)
                         if(XK(ne).lt.0.)then
                            xf_ens(i,j,nall+10)=max(0., &
                                        -xff_ens3(10)/xk(ne)) &
                                        +massfld
                            xf_ens(i,j,nall+11)=max(0., &
                                        -xff_ens3(11)/xk(ne)) &
                                        +massfld
                            xf_ens(i,j,nall+12)=max(0., &
                                        -xff_ens3(12)/xk(ne)) &
                                        +massfld
                         else
                            xf_ens(i,j,nall+10)=massfld
                            xf_ens(i,j,nall+11)=massfld
                            xf_ens(i,j,nall+12)=massfld
                         endif
                      if(icoic.ge.1)then
                      closure_n(i)=0.
                      xf_ens(i,j,nall+1)=xf_ens(i,j,nall+icoic)
                      xf_ens(i,j,nall+2)=xf_ens(i,j,nall+icoic)
                      xf_ens(i,j,nall+3)=xf_ens(i,j,nall+icoic)
                      xf_ens(i,j,nall+4)=xf_ens(i,j,nall+icoic)
                      xf_ens(i,j,nall+5)=xf_ens(i,j,nall+icoic)
                      xf_ens(i,j,nall+6)=xf_ens(i,j,nall+icoic)
                      xf_ens(i,j,nall+7)=xf_ens(i,j,nall+icoic)
                      xf_ens(i,j,nall+8)=xf_ens(i,j,nall+icoic)
                      xf_ens(i,j,nall+9)=xf_ens(i,j,nall+icoic)
                      xf_ens(i,j,nall+10)=xf_ens(i,j,nall+icoic)
                      xf_ens(i,j,nall+11)=xf_ens(i,j,nall+icoic)
                      xf_ens(i,j,nall+12)=xf_ens(i,j,nall+icoic)
                      xf_ens(i,j,nall+13)=xf_ens(i,j,nall+icoic)
                      xf_ens(i,j,nall+14)=xf_ens(i,j,nall+icoic)
                      xf_ens(i,j,nall+15)=xf_ens(i,j,nall+icoic)
                      xf_ens(i,j,nall+16)=xf_ens(i,j,nall+icoic)
                      endif





                      if(icoic.eq.0)xf_ens(i,j,nall+14)=xf_ens(i,j,nall+13)








                      do nens3=1,maxens3
                        massfln(i,j,nall+nens3)=edt(i) &
                                                *xf_ens(i,j,nall+nens3)
                        massfln(i,j,nall+nens3)=max(0., &
                                              massfln(i,j,nall+nens3))
                      enddo








                if(ne.eq.2.and.ierr2(i).gt.0)then
                      xf_ens(i,j,nall+1) =0.
                      xf_ens(i,j,nall+2) =0.
                      xf_ens(i,j,nall+3) =0.
                      xf_ens(i,j,nall+4) =0.
                      xf_ens(i,j,nall+5) =0.
                      xf_ens(i,j,nall+6) =0.
                      xf_ens(i,j,nall+7) =0.
                      xf_ens(i,j,nall+8) =0.
                      xf_ens(i,j,nall+9) =0.
                      xf_ens(i,j,nall+10)=0.
                      xf_ens(i,j,nall+11)=0.
                      xf_ens(i,j,nall+12)=0.
                      xf_ens(i,j,nall+13)=0.
                      xf_ens(i,j,nall+14)=0.
                      xf_ens(i,j,nall+15)=0.
                      xf_ens(i,j,nall+16)=0.
                      massfln(i,j,nall+1)=0.
                      massfln(i,j,nall+2)=0.
                      massfln(i,j,nall+3)=0.
                      massfln(i,j,nall+4)=0.
                      massfln(i,j,nall+5)=0.
                      massfln(i,j,nall+6)=0.
                      massfln(i,j,nall+7)=0.
                      massfln(i,j,nall+8)=0.
                      massfln(i,j,nall+9)=0.
                      massfln(i,j,nall+10)=0.
                      massfln(i,j,nall+11)=0.
                      massfln(i,j,nall+12)=0.
                      massfln(i,j,nall+13)=0.
                      massfln(i,j,nall+14)=0.
                      massfln(i,j,nall+15)=0.
                      massfln(i,j,nall+16)=0.
                endif
                if(ne.eq.3.and.ierr3(i).gt.0)then
                      xf_ens(i,j,nall+1) =0.
                      xf_ens(i,j,nall+2) =0.
                      xf_ens(i,j,nall+3) =0.
                      xf_ens(i,j,nall+4) =0.
                      xf_ens(i,j,nall+5) =0.
                      xf_ens(i,j,nall+6) =0.
                      xf_ens(i,j,nall+7) =0.
                      xf_ens(i,j,nall+8) =0.
                      xf_ens(i,j,nall+9) =0.
                      xf_ens(i,j,nall+10)=0.
                      xf_ens(i,j,nall+11)=0.
                      xf_ens(i,j,nall+12)=0.
                      xf_ens(i,j,nall+13)=0.
                      xf_ens(i,j,nall+14)=0.
                      xf_ens(i,j,nall+15)=0.
                      xf_ens(i,j,nall+16)=0.
                      massfln(i,j,nall+1)=0.
                      massfln(i,j,nall+2)=0.
                      massfln(i,j,nall+3)=0.
                      massfln(i,j,nall+4)=0.
                      massfln(i,j,nall+5)=0.
                      massfln(i,j,nall+6)=0.
                      massfln(i,j,nall+7)=0.
                      massfln(i,j,nall+8)=0.
                      massfln(i,j,nall+9)=0.
                      massfln(i,j,nall+10)=0.
                      massfln(i,j,nall+11)=0.
                      massfln(i,j,nall+12)=0.
                      massfln(i,j,nall+13)=0.
                      massfln(i,j,nall+14)=0.
                      massfln(i,j,nall+15)=0.
                      massfln(i,j,nall+16)=0.
                endif

                   endif
 350            continue


                   nall=(iens-1)*maxens3*maxens*maxens2 &
                        +(iedt-1)*maxens*maxens3


                   nall2=(iens-1)*maxens3*maxens*maxens2 &
                        +(iedt-1)*maxens*maxens3 &
                        +(2-1)*maxens3
                      xf_ens(i,j,nall+4) = xf_ens(i,j,nall2+4)
                      xf_ens(i,j,nall+5) =xf_ens(i,j,nall2+5)
                      xf_ens(i,j,nall+6) =xf_ens(i,j,nall2+6)
                      xf_ens(i,j,nall+7) =xf_ens(i,j,nall2+7)
                      xf_ens(i,j,nall+8) =xf_ens(i,j,nall2+8)
                      xf_ens(i,j,nall+9) =xf_ens(i,j,nall2+9)
                      xf_ens(i,j,nall+10)=xf_ens(i,j,nall2+10)
                      xf_ens(i,j,nall+11)=xf_ens(i,j,nall2+11)
                      xf_ens(i,j,nall+12)=xf_ens(i,j,nall2+12)
                go to 100
             endif
          elseif(ierr(i).ne.20.and.ierr(i).ne.0)then
             do n=1,ensdim
               xf_ens(i,j,n)=0.
               massfln(i,j,n)=0.
             enddo
          endif
 100   continue

   END SUBROUTINE cup_forcing_ens


   SUBROUTINE cup_kbcon(cap_inc,iloop,k22,kbcon,he_cup,hes_cup, &
              ierr,kbmax,p_cup,cap_max,                         &
              ids,ide, jds,jde, kds,kde,                        &
              ims,ime, jms,jme, kms,kme,                        &
              its,ite, jts,jte, kts,kte                        )

   IMPLICIT NONE


   

     integer                                                           &
        ,intent (in   )                   ::                           &
        ids,ide, jds,jde, kds,kde,           &
        ims,ime, jms,jme, kms,kme,           &
        its,ite, jts,jte, kts,kte
  
  
  
  
  
     real,    dimension (its:ite,kts:kte)                              &
        ,intent (in   )                   ::                           &
        he_cup,hes_cup,p_cup
     real,    dimension (its:ite)                                      &
        ,intent (in   )                   ::                           &
        cap_max,cap_inc
     integer, dimension (its:ite)                                      &
        ,intent (in   )                   ::                           &
        kbmax
     integer, dimension (its:ite)                                      &
        ,intent (inout)                   ::                           &
        kbcon,k22,ierr
     integer                                                           &
        ,intent (in   )                   ::                           &
        iloop




     integer                              ::                           &
        i
     real                                 ::                           &
        pbcdif,plus
     integer :: itf

     itf=MIN(ite,ide-1)



       DO 27 i=its,itf
      kbcon(i)=1
      IF(ierr(I).ne.0)GO TO 27
      KBCON(I)=K22(I)
      GO TO 32
 31   CONTINUE
      KBCON(I)=KBCON(I)+1
      IF(KBCON(I).GT.KBMAX(i)+2)THEN
         if(iloop.lt.4)ierr(i)=3

        GO TO 27
      ENDIF
 32   CONTINUE
      IF(HE_cup(I,K22(I)).LT.HES_cup(I,KBCON(I)))GO TO 31



      if(KBCON(I)-K22(I).eq.1)go to 27
      PBCDIF=-P_cup(I,KBCON(I))+P_cup(I,K22(I))
      plus=max(25.,cap_max(i)-float(iloop-1)*cap_inc(i))
      if(iloop.eq.4)plus=cap_max(i)
      IF(PBCDIF.GT.plus)THEN
        K22(I)=K22(I)+1
        KBCON(I)=K22(I)
        GO TO 32
      ENDIF
 27   CONTINUE

   END SUBROUTINE cup_kbcon


   SUBROUTINE cup_kbcon_cin(iloop,k22,kbcon,he_cup,hes_cup,  &
              z,tmean,qes,ierr,kbmax,p_cup,cap_max,xl,cp,    &
              ids,ide, jds,jde, kds,kde,                     &
              ims,ime, jms,jme, kms,kme,                     &
              its,ite, jts,jte, kts,kte                     )

   IMPLICIT NONE


   

     integer                                                           &
        ,intent (in   )                   ::                           &
        ids,ide, jds,jde, kds,kde,           &
        ims,ime, jms,jme, kms,kme,           &
        its,ite, jts,jte, kts,kte
  
  
  
  
  
     real,    dimension (its:ite,kts:kte)                              &
        ,intent (in   )                   ::                           &
        he_cup,hes_cup,p_cup,z,tmean,qes
     real,    dimension (its:ite)                                      &
        ,intent (in   )                   ::                           &
        cap_max
     real                                                              &
        ,intent (in   )                   ::                           &
        xl,cp
     integer, dimension (its:ite)                                      &
        ,intent (in   )                   ::                           &
        kbmax
     integer, dimension (its:ite)                                      &
        ,intent (inout)                   ::                           &
        kbcon,k22,ierr
     integer                                                           &
        ,intent (in   )                   ::                           &
        iloop




     integer                              ::                           &
        i,k
     real                                 ::                           &
        cin,cin_max,dh,tprim,gamma

     integer :: itf

     itf=MIN(ite,ide-1)


    



       DO 27 i=its,itf
      cin_max=-cap_max(i)
      kbcon(i)=1
      cin = 0.
      IF(ierr(I).ne.0)GO TO 27
      KBCON(I)=K22(I)
      GO TO 32
 31   CONTINUE
      KBCON(I)=KBCON(I)+1
      IF(KBCON(I).GT.KBMAX(i)+2)THEN
         if(iloop.eq.1)ierr(i)=3

        GO TO 27
      ENDIF
 32   CONTINUE
      dh      = HE_cup(I,K22(I)) - HES_cup(I,KBCON(I))
      if (dh.lt. 0.) then
        GAMMA=(xl/cp)*(xl/(461.525*(Tmean(I,K22(i))**2)))*QES(I,K22(i))
        tprim = dh/(cp*(1.+gamma))

        cin = cin + 9.8066 * tprim &
              *(z(i,k22(i))-z(i,k22(i)-1)) / tmean(i,k22(i))
        go to 31
      end if







      IF(cin.lT.cin_max)THEN

        K22(I)=K22(I)+1
        KBCON(I)=K22(I)
        GO TO 32
      ENDIF
 27   CONTINUE

   END SUBROUTINE cup_kbcon_cin


   SUBROUTINE cup_ktop(ilo,dby,kbcon,ktop,ierr,              &
              ids,ide, jds,jde, kds,kde,                     &
              ims,ime, jms,jme, kms,kme,                     &
              its,ite, jts,jte, kts,kte                     )

   IMPLICIT NONE




   

     integer                                                           &
        ,intent (in   )                   ::                           &
        ids,ide, jds,jde, kds,kde,           &
        ims,ime, jms,jme, kms,kme,           &
        its,ite, jts,jte, kts,kte
  
  
  
  
  
     real,    dimension (its:ite,kts:kte)                              &
        ,intent (inout)                   ::                           &
        dby
     integer, dimension (its:ite)                                      &
        ,intent (in   )                   ::                           &
        kbcon
     integer                                                           &
        ,intent (in   )                   ::                           &
        ilo
     integer, dimension (its:ite)                                      &
        ,intent (out  )                   ::                           &
        ktop
     integer, dimension (its:ite)                                      &
        ,intent (inout)                   ::                           &
        ierr




     integer                              ::                           &
        i,k

     integer :: itf, ktf

     itf=MIN(ite,ide-1)
     ktf=MIN(kte,kde-1)


        DO 42 i=its,itf
        ktop(i)=1
         IF(ierr(I).EQ.0)then
          DO 40 K=KBCON(I)+1,ktf-1
            IF(DBY(I,K).LE.0.)THEN
                KTOP(I)=K-1
                GO TO 41
             ENDIF
  40      CONTINUE
          if(ilo.eq.1)ierr(i)=5

          GO TO 42
  41     CONTINUE
         do k=ktop(i)+1,ktf
           dby(i,k)=0.
         enddo
         endif
  42     CONTINUE

   END SUBROUTINE cup_ktop


   SUBROUTINE cup_MAXIMI(ARRAY,KS,KE,MAXX,ierr,              &
              ids,ide, jds,jde, kds,kde,                     &
              ims,ime, jms,jme, kms,kme,                     &
              its,ite, jts,jte, kts,kte                     )

   IMPLICIT NONE




   

     integer                                                           &
        ,intent (in   )                   ::                           &
         ids,ide, jds,jde, kds,kde,                                    &
         ims,ime, jms,jme, kms,kme,                                    &
         its,ite, jts,jte, kts,kte
  
  
  
  
     real,    dimension (its:ite,kts:kte)                              &
        ,intent (in   )                   ::                           &
         array
     integer, dimension (its:ite)                                      &
        ,intent (in   )                   ::                           &
         ierr,ke
     integer                                                           &
        ,intent (in   )                   ::                           &
         ks
     integer, dimension (its:ite)                                      &
        ,intent (out  )                   ::                           &
         maxx
     real,    dimension (its:ite)         ::                           &
         x
     real                                 ::                           &
         xar
     integer                              ::                           &
         i,k
     integer :: itf

     itf=MIN(ite,ide-1)

       DO 200 i=its,itf
       MAXX(I)=KS
       if(ierr(i).eq.0)then
      X(I)=ARRAY(I,KS)

       DO 100 K=KS,KE(i)
         XAR=ARRAY(I,K)
         IF(XAR.GE.X(I)) THEN
            X(I)=XAR
            MAXX(I)=K
         ENDIF
 100  CONTINUE
      endif
 200  CONTINUE

   END SUBROUTINE cup_MAXIMI


   SUBROUTINE cup_minimi(ARRAY,KS,KEND,KT,ierr,              &
              ids,ide, jds,jde, kds,kde,                     &
              ims,ime, jms,jme, kms,kme,                     &
              its,ite, jts,jte, kts,kte                     )

   IMPLICIT NONE




   

     integer                                                           &
        ,intent (in   )                   ::                           &
         ids,ide, jds,jde, kds,kde,                                    &
         ims,ime, jms,jme, kms,kme,                                    &
         its,ite, jts,jte, kts,kte
  
  
  
  
     real,    dimension (its:ite,kts:kte)                              &
        ,intent (in   )                   ::                           &
         array
     integer, dimension (its:ite)                                      &
        ,intent (in   )                   ::                           &
         ierr,ks,kend
     integer, dimension (its:ite)                                      &
        ,intent (out  )                   ::                           &
         kt
     real,    dimension (its:ite)         ::                           &
         x
     integer                              ::                           &
         i,k,kstop

     integer :: itf

     itf=MIN(ite,ide-1)

       DO 200 i=its,itf
      KT(I)=KS(I)
      if(ierr(i).eq.0)then
      X(I)=ARRAY(I,KS(I))
       KSTOP=MAX(KS(I)+1,KEND(I))

       DO 100 K=KS(I)+1,KSTOP
         IF(ARRAY(I,K).LT.X(I)) THEN
              X(I)=ARRAY(I,K)
              KT(I)=K
         ENDIF
 100  CONTINUE
      endif
 200  CONTINUE

   END SUBROUTINE cup_MINIMI


   SUBROUTINE cup_output_ens(xf_ens,ierr,dellat,dellaq,dellaqc,  &
              outtem,outq,outqc,pre,pw,xmb,ktop,                 &
              j,name,nx,nx2,iens,ierr2,ierr3,pr_ens,             &
              maxens3,ensdim,massfln,                            &
              APR_GR,APR_W,APR_MC,APR_ST,APR_AS,                 &
              APR_CAPMA,APR_CAPME,APR_CAPMI,closure_n,xland1,    &
              outCFU1,outCFD1,outDFU1,outEFU1,outDFD1,outEFD1,  &
              CFU1_ens,CFD1_ens,DFU1_ens,EFU1_ens,DFD1_ens,EFD1_ens,  &
              l_flux,                                           &
              ids,ide, jds,jde, kds,kde, &
              ims,ime, jms,jme, kms,kme, &
              its,ite, jts,jte, kts,kte)

   IMPLICIT NONE




   

     integer                                                           &
        ,intent (in   )                   ::                           &
        ids,ide, jds,jde, kds,kde,           &
        ims,ime, jms,jme, kms,kme,           &
        its,ite, jts,jte, kts,kte
     integer, intent (in   )              ::                           &
        j,ensdim,nx,nx2,iens,maxens3
  
  
  
  
  
  
  
  
  
  
  
  
  
  
     real,    dimension (ims:ime,jms:jme,1:ensdim)                     &
        ,intent (inout)                   ::                           &
       xf_ens,pr_ens,massfln
     real,    dimension (ims:ime,jms:jme)                              &
        ,intent (inout)                   ::                           &
               APR_GR,APR_W,APR_MC,APR_ST,APR_AS,APR_CAPMA,            &
               APR_CAPME,APR_CAPMI 

     real,    dimension (its:ite,kts:kte)                              &
        ,intent (out  )                   ::                           &
        outtem,outq,outqc
     real,    dimension (its:ite)                                      &
        ,intent (out  )                   ::                           &
        pre,xmb
     real,    dimension (its:ite)                                      &
        ,intent (inout  )                   ::                           &
        closure_n,xland1
     real,    dimension (its:ite,kts:kte,1:nx)                         &
        ,intent (in   )                   ::                           &
       dellat,dellaqc,dellaq,pw
     integer, dimension (its:ite)                                      &
        ,intent (in   )                   ::                           &
        ktop
     integer, dimension (its:ite)                                      &
        ,intent (inout)                   ::                           &
        ierr,ierr2,ierr3
     real,    dimension (its:ite,kts:kte,1:ensdim)                     &
        ,intent (in   )                   ::                           &
       CFU1_ens,CFD1_ens,DFU1_ens,EFU1_ens,DFD1_ens,EFD1_ens
     real,    dimension (its:ite,kts:kte)                     &
        ,intent (out)                   ::                           &
           outCFU1,outCFD1,outDFU1,outEFU1,outDFD1,outEFD1
     logical, intent(in) :: l_flux





     integer                              ::                           &
        i,k,n,ncount
     real                                 ::                           &
        outtes,ddtes,dtt,dtq,dtqc,dtpw,tuning,prerate,clos_wei
     real,    dimension (its:ite)         ::                           &
       xfac1
     real,    dimension (its:ite)::                           &
       xmb_ske,xmb_ave,xmb_std,xmb_cur,xmbweight
     real,    dimension (its:ite)::                           &
       pr_ske,pr_ave,pr_std,pr_cur
     real,    dimension (its:ite,jts:jte)::                           &
               pr_gr,pr_w,pr_mc,pr_st,pr_as,pr_capma,     &
               pr_capme,pr_capmi
     integer :: iedt, kk


      character *(*), intent (in)        ::                           &
       name

     integer :: itf, ktf

     itf=MIN(ite,ide-1)
     ktf=MIN(kte,kde-1)
     tuning=0.


      DO k=kts,ktf
      do i=its,itf
        outtem(i,k)=0.
        outq(i,k)=0.
        outqc(i,k)=0.
      enddo
      enddo
      do i=its,itf
        pre(i)=0.
        xmb(i)=0.
         xfac1(i)=1.
        xmbweight(i)=1.
      enddo
      do i=its,itf
        IF(ierr(i).eq.0)then
        do n=(iens-1)*nx*nx2*maxens3+1,iens*nx*nx2*maxens3
           if(pr_ens(i,j,n).le.0.)then
             xf_ens(i,j,n)=0.
           endif
        enddo
        endif
      enddo



       call massflx_stats(xf_ens,ensdim,nx2,nx,maxens3,      &
            xmb_ave,xmb_std,xmb_cur,xmb_ske,j,ierr,1,    &
            APR_GR,APR_W,APR_MC,APR_ST,APR_AS,           &
            APR_CAPMA,APR_CAPME,APR_CAPMI,               &
            pr_gr,pr_w,pr_mc,pr_st,pr_as,                &
            pr_capma,pr_capme,pr_capmi,                  &
            ids,ide, jds,jde, kds,kde,                   &
            ims,ime, jms,jme, kms,kme,                   &
            its,ite, jts,jte, kts,kte                   )
       call massflx_stats(pr_ens,ensdim,nx2,nx,maxens3,  &
            pr_ave,pr_std,pr_cur,pr_ske,j,ierr,2,        &
            APR_GR,APR_W,APR_MC,APR_ST,APR_AS,           &
            APR_CAPMA,APR_CAPME,APR_CAPMI,               &
            pr_gr,pr_w,pr_mc,pr_st,pr_as,                &
            pr_capma,pr_capme,pr_capmi,                  &
            ids,ide, jds,jde, kds,kde,                   &
            ims,ime, jms,jme, kms,kme,                   &
            its,ite, jts,jte, kts,kte                   )



      ddtes=200.

      do i=its,itf
        if(ierr(i).eq.0)then
         if(xmb_ave(i).le.0.)then
              ierr(i)=13
              xmb_ave(i)=0.
         endif

         xmb(i)=max(.1*xmb_ave(i),xmb_ave(i)-tuning*xmb_std(i))



           clos_wei=16./max(1.,closure_n(i))
           if (xland1(i).lt.0.5)xmb(i)=xmb(i)*clos_wei
           if(xmb(i).eq.0.)then
              ierr(i)=19
           endif
           if(xmb(i).gt.100.)then
              ierr(i)=19
           endif
           xfac1(i)=xmb(i)

        endif
        xfac1(i)=xmb_ave(i)
      ENDDO
      DO k=kts,ktf
      do i=its,itf
            dtt=0.
            dtq=0.
            dtqc=0.
            dtpw=0.
        IF(ierr(i).eq.0.and.k.le.ktop(i))then
           do n=1,nx
              dtt=dtt+dellat(i,k,n)
              dtq=dtq+dellaq(i,k,n)
              dtqc=dtqc+dellaqc(i,k,n)
              dtpw=dtpw+pw(i,k,n)
           enddo
           outtes=dtt*XMB(I)*86400./float(nx)
           IF((OUTTES.GT.2.*ddtes.and.k.gt.2))THEN
             XMB(I)= 2.*ddtes/outtes * xmb(i)
             outtes=1.*ddtes
           endif
           if (outtes .lt. -ddtes) then
             XMB(I)= -ddtes/outtes * xmb(i)
             outtes=-ddtes
           endif
           if (outtes .gt. .5*ddtes.and.k.le.2) then
             XMB(I)= ddtes/outtes * xmb(i)
             outtes=.5*ddtes
           endif
           OUTTEM(I,K)=XMB(I)*dtt/float(nx)
           OUTQ(I,K)=XMB(I)*dtq/float(nx)
           OUTQC(I,K)=XMB(I)*dtqc/float(nx)
           PRE(I)=PRE(I)+XMB(I)*dtpw/float(nx)
        endif
      enddo
      enddo
      do i=its,itf
        if(ierr(i).eq.0)then
           prerate=pre(i)*3600.
           if(prerate.lt.0.1)then
              if(ierr2(i).gt.0.or.ierr3(i).gt.0)then
                 pre(i)=0.
                 ierr(i)=221
                 do k=kts,ktf
                    outtem(i,k)=0.
                    outq(i,k)=0.
                    outqc(i,k)=0.
                 enddo
                 do k=(iens-1)*nx*nx2*maxens3+1,iens*nx*nx2*maxens3
                   massfln(i,j,k)=0.
                   xf_ens(i,j,k)=0.
                 enddo
               endif
            endif

        endif
      ENDDO

      do i=its,itf
        if(ierr(i).eq.0)then
        xfac1(i)=xmb(i)/xfac1(i)
        do k=(iens-1)*nx*nx2*maxens3+1,iens*nx*nx2*maxens3
          massfln(i,j,k)=massfln(i,j,k)*xfac1(i)
          xf_ens(i,j,k)=xf_ens(i,j,k)*xfac1(i)
        enddo
        endif
      ENDDO
      if (l_flux) then
         if (iens .eq. 1) then  
            do k=kts,ktf
               do i=its,itf
                  outcfu1(i,k)=0.
                  outcfd1(i,k)=0.
                  outdfu1(i,k)=0.
                  outefu1(i,k)=0.
                  outdfd1(i,k)=0.
                  outefd1(i,k)=0.
                  if (ierr(i) .eq. 0) then
                     do iedt=1,nx
                        do kk=1,nx2*maxens3
                           n=(iens-1)*nx*nx2*maxens3 + &
                                (iedt-1)*nx2*maxens3 + kk
                           outcfu1(i,k)=outcfu1(i,k)+cfu1_ens(i,k,iedt)*xf_ens(i,j,n)
                           outcfd1(i,k)=outcfd1(i,k)+cfd1_ens(i,k,iedt)*xf_ens(i,j,n)
                           outdfu1(i,k)=outdfu1(i,k)+dfu1_ens(i,k,iedt)*xf_ens(i,j,n)
                           outefu1(i,k)=outefu1(i,k)+efu1_ens(i,k,iedt)*xf_ens(i,j,n)
                           outdfd1(i,k)=outdfd1(i,k)+dfd1_ens(i,k,iedt)*xf_ens(i,j,n)
                           outefd1(i,k)=outefd1(i,k)+efd1_ens(i,k,iedt)*xf_ens(i,j,n)
                        end do
                     end do
                     outcfu1(i,k)=outcfu1(i,k)/ensdim
                     outcfd1(i,k)=outcfd1(i,k)/ensdim
                     outdfu1(i,k)=outdfu1(i,k)/ensdim
                     outefu1(i,k)=outefu1(i,k)/ensdim
                     outdfd1(i,k)=outdfd1(i,k)/ensdim
                     outefd1(i,k)=outefd1(i,k)/ensdim
                  end if 
               end do 
            end do 
         end if 
      end if 

   END SUBROUTINE cup_output_ens


   SUBROUTINE cup_up_aa0(aa0,z,zu,dby,GAMMA_CUP,t_cup,       &
              kbcon,ktop,ierr,                               &
              ids,ide, jds,jde, kds,kde,                     &
              ims,ime, jms,jme, kms,kme,                     &
              its,ite, jts,jte, kts,kte                     )

   IMPLICIT NONE




   

     integer                                                           &
        ,intent (in   )                   ::                           &
        ids,ide, jds,jde, kds,kde,                                     &
        ims,ime, jms,jme, kms,kme,                                     &
        its,ite, jts,jte, kts,kte
  
  
  
  
  
  
  
  
     real,    dimension (its:ite,kts:kte)                              &
        ,intent (in   )                   ::                           &
        z,zu,gamma_cup,t_cup,dby
     integer, dimension (its:ite)                                      &
        ,intent (in   )                   ::                           &
        kbcon,ktop





     integer, dimension (its:ite)                                      &
        ,intent (inout)                   ::                           &
        ierr
     real,    dimension (its:ite)                                      &
        ,intent (out  )                   ::                           &
        aa0




     integer                              ::                           &
        i,k
     real                                 ::                           &
        dz,da

     integer :: itf, ktf

     itf = MIN(ite,ide-1)
     ktf = MIN(kte,kde-1)

        do i=its,itf
         aa0(i)=0.
        enddo
        DO 100 k=kts+1,ktf
        DO 100 i=its,itf
         IF(ierr(i).ne.0)GO TO 100
         IF(K.LE.KBCON(I))GO TO 100
         IF(K.Gt.KTOP(I))GO TO 100
         DZ=Z(I,K)-Z(I,K-1)
         da=zu(i,k)*DZ*(9.81/(1004.*( &
                (T_cup(I,K)))))*DBY(I,K-1)/ &
             (1.+GAMMA_CUP(I,K))
         IF(K.eq.KTOP(I).and.da.le.0.)go to 100
         AA0(I)=AA0(I)+da
         if(aa0(i).lt.0.)aa0(i)=0.
100     continue

   END SUBROUTINE cup_up_aa0


   SUBROUTINE cup_up_he(k22,hkb,z_cup,cd,entr,he_cup,hc,     &
              kbcon,ierr,dby,he,hes_cup,                     &
              ids,ide, jds,jde, kds,kde,                     &
              ims,ime, jms,jme, kms,kme,                     &
              its,ite, jts,jte, kts,kte                     )

   IMPLICIT NONE




   

     integer                                                           &
        ,intent (in   )                   ::                           &
                                  ids,ide, jds,jde, kds,kde,           &
                                  ims,ime, jms,jme, kms,kme,           &
                                  its,ite, jts,jte, kts,kte
  
  
  
  
  
  
  
  
  
  
     real,    dimension (its:ite,kts:kte)                              &
        ,intent (in   )                   ::                           &
        he,he_cup,hes_cup,z_cup,cd
  
     real                                                              &
        ,intent (in   )                   ::                           &
        entr
     integer, dimension (its:ite)                                      &
        ,intent (in   )                   ::                           &
        kbcon,k22




   

     integer, dimension (its:ite)                                      &
        ,intent (inout)                   ::                           &
        ierr

     real,    dimension (its:ite,kts:kte)                              &
        ,intent (out  )                   ::                           &
        hc,dby
     real,    dimension (its:ite)                                      &
        ,intent (out  )                   ::                           &
        hkb




     integer                              ::                           &
        i,k
     real                                 ::                           &
        dz

     integer :: itf, ktf

     itf = MIN(ite,ide-1)
     ktf = MIN(kte,kde-1)



      do i=its,itf
      if(ierr(i).eq.0.)then
      hkb(i)=he_cup(i,k22(i))
      do k=1,k22(i)
        hc(i,k)=he_cup(i,k)
        DBY(I,K)=0.
      enddo
      do k=k22(i),kbcon(i)-1
        hc(i,k)=hkb(i)
        DBY(I,K)=0.
      enddo
        k=kbcon(i)
        hc(i,k)=hkb(i)
        DBY(I,Kbcon(i))=Hkb(I)-HES_cup(I,K)
      endif
      enddo
      do k=kts+1,ktf
      do i=its,itf
        if(k.gt.kbcon(i).and.ierr(i).eq.0.)then
           DZ=Z_cup(i,K)-Z_cup(i,K-1)
           HC(i,K)=(HC(i,K-1)*(1.-.5*CD(i,K)*DZ)+entr* &
                DZ*HE(i,K-1))/(1.+entr*DZ-.5*cd(i,k)*dz)
           DBY(I,K)=HC(I,K)-HES_cup(I,K)
        endif
      enddo

      enddo

   END SUBROUTINE cup_up_he


   SUBROUTINE cup_up_moisture(ierr,z_cup,qc,qrc,pw,pwav,     &
              kbcon,ktop,cd,dby,mentr_rate,clw_all,          &
              q,GAMMA_cup,zu,qes_cup,k22,qe_cup,xl,          &
              ids,ide, jds,jde, kds,kde,                     &
              ims,ime, jms,jme, kms,kme,                     &
              its,ite, jts,jte, kts,kte                     )

   IMPLICIT NONE




   

     integer                                                           &
        ,intent (in   )                   ::                           &
                                  ids,ide, jds,jde, kds,kde,           &
                                  ims,ime, jms,jme, kms,kme,           &
                                  its,ite, jts,jte, kts,kte
  
  
  
  
  
  
  
  
  
  
     real,    dimension (its:ite,kts:kte)                              &
        ,intent (in   )                   ::                           &
        q,zu,gamma_cup,qe_cup,dby,qes_cup,z_cup,cd
  
     real                                                              &
        ,intent (in   )                   ::                           &
        mentr_rate,xl
     integer, dimension (its:ite)                                      &
        ,intent (in   )                   ::                           &
        kbcon,ktop,k22




   

     integer, dimension (its:ite)                                      &
        ,intent (inout)                   ::                           &
        ierr
   
   
   
   
   
   

     real,    dimension (its:ite,kts:kte)                              &
        ,intent (out  )                   ::                           &
        qc,qrc,pw,clw_all
     real,    dimension (its:ite)                                      &
        ,intent (out  )                   ::                           &
        pwav




     integer                              ::                           &
        iall,i,k
     real                                 ::                           &
        dh,qrch,c0,dz,radius

     integer :: itf, ktf

     itf = MIN(ite,ide-1)
     ktf = MIN(kte,kde-1)

        iall=0
        c0=.002



        if(mentr_rate.gt.0.)then
          radius=.2/mentr_rate
          if(radius.lt.900.)c0=0.

        endif
        do i=its,itf
          pwav(i)=0.
        enddo
        do k=kts,ktf
        do i=its,itf
          pw(i,k)=0.
          if(ierr(i).eq.0)qc(i,k)=qes_cup(i,k)
          clw_all(i,k)=0.
          qrc(i,k)=0.
        enddo
        enddo
      do i=its,itf
      if(ierr(i).eq.0.)then
      do k=k22(i),kbcon(i)-1
        qc(i,k)=qe_cup(i,k22(i))
      enddo
      endif
      enddo

        DO 100 k=kts+1,ktf
        DO 100 i=its,itf
         IF(ierr(i).ne.0)GO TO 100
         IF(K.Lt.KBCON(I))GO TO 100
         IF(K.Gt.KTOP(I))GO TO 100
         DZ=Z_cup(i,K)-Z_cup(i,K-1)





        QC(i,K)=(QC(i,K-1)*(1.-.5*CD(i,K)*DZ)+mentr_rate* &
                DZ*Q(i,K-1))/(1.+mentr_rate*DZ-.5*cd(i,k)*dz)



         QRCH=QES_cup(I,K)+(1./XL)*(GAMMA_cup(i,k) &
              /(1.+GAMMA_cup(i,k)))*DBY(I,K)



        clw_all(i,k)=QC(I,K)-QRCH
        QRC(I,K)=(QC(I,K)-QRCH)/(1.+C0*DZ*zu(i,k))
        if(qrc(i,k).lt.0.)then
          qrc(i,k)=0.
        endif



         PW(i,k)=c0*dz*QRC(I,K)*zu(i,k)
        if(iall.eq.1)then
          qrc(i,k)=0.
          pw(i,k)=(QC(I,K)-QRCH)*zu(i,k)
          if(pw(i,k).lt.0.)pw(i,k)=0.
        endif



         QC(I,K)=QRC(I,K)+qrch



         PWAV(I)=PWAV(I)+PW(I,K)
 100     CONTINUE

   END SUBROUTINE cup_up_moisture


   SUBROUTINE cup_up_nms(zu,z_cup,entr,cd,kbcon,ktop,ierr,k22,  &
              ids,ide, jds,jde, kds,kde,                        &
              ims,ime, jms,jme, kms,kme,                        &
              its,ite, jts,jte, kts,kte                        )

   IMPLICIT NONE





   

     integer                                                           &
        ,intent (in   )                   ::                           &
         ids,ide, jds,jde, kds,kde,                                    &
         ims,ime, jms,jme, kms,kme,                                    &
         its,ite, jts,jte, kts,kte
  
     real,    dimension (its:ite,kts:kte)                              &
        ,intent (in   )                   ::                           &
         z_cup,cd
  
     real                                                              &
        ,intent (in   )                   ::                           &
         entr
     integer, dimension (its:ite)                                      &
        ,intent (in   )                   ::                           &
         kbcon,ktop,k22




   

     integer, dimension (its:ite)                                      &
        ,intent (inout)                   ::                           &
         ierr
   

     real,    dimension (its:ite,kts:kte)                              &
        ,intent (out  )                   ::                           &
         zu




     integer                              ::                           &
         i,k
     real                                 ::                           &
         dz
     integer :: itf, ktf

     itf = MIN(ite,ide-1)
     ktf = MIN(kte,kde-1)



       do k=kts,ktf
       do i=its,itf
         zu(i,k)=0.
       enddo
       enddo



       do i=its,itf
          IF(ierr(I).eq.0)then
             do k=k22(i),kbcon(i)
               zu(i,k)=1.
             enddo
             DO K=KBcon(i)+1,KTOP(i)
               DZ=Z_cup(i,K)-Z_cup(i,K-1)
               ZU(i,K)=ZU(i,K-1)*(1.+(entr-cd(i,k))*DZ)
             enddo
          endif
       enddo

   END SUBROUTINE cup_up_nms


   SUBROUTINE gdinit(RTHCUTEN,RQVCUTEN,RQCCUTEN,RQICUTEN,           &
                        MASS_FLUX,cp,restart,                       &
                        P_QC,P_QI,P_FIRST_SCALAR,                   &
                        RTHFTEN, RQVFTEN,                           &
                        APR_GR,APR_W,APR_MC,APR_ST,APR_AS,          &
                        APR_CAPMA,APR_CAPME,APR_CAPMI,              &
                        allowed_to_read,                            &
                        ids, ide, jds, jde, kds, kde,               &
                        ims, ime, jms, jme, kms, kme,               &
                        its, ite, jts, jte, kts, kte               )

   IMPLICIT NONE

   LOGICAL , INTENT(IN)           ::  restart,allowed_to_read
   INTEGER , INTENT(IN)           ::  ids, ide, jds, jde, kds, kde, &
                                      ims, ime, jms, jme, kms, kme, &
                                      its, ite, jts, jte, kts, kte
   INTEGER , INTENT(IN)           ::  P_FIRST_SCALAR, P_QI, P_QC
   REAL,     INTENT(IN)           ::  cp

   REAL,     DIMENSION( ims:ime , kms:kme , jms:jme ) , INTENT(OUT) ::       &
                                                          RTHCUTEN, &
                                                          RQVCUTEN, &
                                                          RQCCUTEN, &
                                                          RQICUTEN   

   REAL,     DIMENSION( ims:ime , kms:kme , jms:jme ) , INTENT(OUT) ::       &
                                                          RTHFTEN,  &
                                                          RQVFTEN

   REAL,     DIMENSION( ims:ime , jms:jme ) , INTENT(OUT) ::        &
                                APR_GR,APR_W,APR_MC,APR_ST,APR_AS,  &
                                APR_CAPMA,APR_CAPME,APR_CAPMI,      &
                                MASS_FLUX

   INTEGER :: i, j, k, itf, jtf, ktf
 
   jtf=min0(jte,jde-1)
   ktf=min0(kte,kde-1)
   itf=min0(ite,ide-1)
 
   IF(.not.restart)THEN
     DO j=jts,jtf
     DO k=kts,ktf
     DO i=its,itf
        RTHCUTEN(i,k,j)=0.
        RQVCUTEN(i,k,j)=0.
     ENDDO
     ENDDO
     ENDDO

     DO j=jts,jtf
     DO k=kts,ktf
     DO i=its,itf
        RTHFTEN(i,k,j)=0.
        RQVFTEN(i,k,j)=0.
     ENDDO
     ENDDO
     ENDDO

     IF (P_QC .ge. P_FIRST_SCALAR) THEN
        DO j=jts,jtf
        DO k=kts,ktf
        DO i=its,itf
           RQCCUTEN(i,k,j)=0.
        ENDDO
        ENDDO
        ENDDO
     ENDIF

     IF (P_QI .ge. P_FIRST_SCALAR) THEN
        DO j=jts,jtf
        DO k=kts,ktf
        DO i=its,itf
           RQICUTEN(i,k,j)=0.
        ENDDO
        ENDDO
        ENDDO
     ENDIF

     DO j=jts,jtf
     DO i=its,itf
        mass_flux(i,j)=0.
     ENDDO
     ENDDO

   ENDIF
     DO j=jts,jtf
     DO i=its,itf
        APR_GR(i,j)=0.
        APR_ST(i,j)=0.
        APR_W(i,j)=0.
        APR_MC(i,j)=0.
        APR_AS(i,j)=0.
        APR_CAPMA(i,j)=0.
        APR_CAPME(i,j)=0.
        APR_CAPMI(i,j)=0.
     ENDDO
     ENDDO

   END SUBROUTINE gdinit


   SUBROUTINE massflx_stats(xf_ens,ensdim,maxens,maxens2,maxens3, &
              xt_ave,xt_std,xt_cur,xt_ske,j,ierr,itest,           &
              APR_GR,APR_W,APR_MC,APR_ST,APR_AS,                  &
              APR_CAPMA,APR_CAPME,APR_CAPMI,                      &
              pr_gr,pr_w,pr_mc,pr_st,pr_as,                       &
              pr_capma,pr_capme,pr_capmi,                         &
              ids,ide, jds,jde, kds,kde,  &
              ims,ime, jms,jme, kms,kme,  &
              its,ite, jts,jte, kts,kte)

   IMPLICIT NONE

   integer, intent (in   )              ::                                    &
                     j,ensdim,maxens3,maxens,maxens2,itest
   INTEGER,      INTENT(IN   ) ::                                             &
                                  ids,ide, jds,jde, kds,kde,                  &
                                  ims,ime, jms,jme, kms,kme,                  &
                                  its,ite, jts,jte, kts,kte


     real, dimension (its:ite)                                                &
         , intent(inout) ::                                                   &
           xt_ave,xt_cur,xt_std,xt_ske
     integer, dimension (its:ite), intent (in) ::                             &
           ierr
     real, dimension (ims:ime,jms:jme,1:ensdim)                               &
         , intent(in   ) ::                                                   &
           xf_ens
     real, dimension (ims:ime,jms:jme)                                        &
         , intent(inout) ::                                                   &
           APR_GR,APR_W,APR_MC,APR_ST,APR_AS,                                 &
           APR_CAPMA,APR_CAPME,APR_CAPMI
     real, dimension (its:ite,jts:jte)                                        &
         , intent(inout) ::                                                   &
           pr_gr,pr_w,pr_mc,pr_st,pr_as,                                      &
           pr_capma,pr_capme,pr_capmi




     real, dimension (its:ite , 1:maxens3 )       ::                          &
           x_ave,x_cur,x_std,x_ske
     real, dimension (its:ite , 1:maxens  )       ::                          &
           x_ave_cap


      integer, dimension (1:maxens3) :: nc1
      integer :: i,k
      integer :: num,kk,num2,iedt
      real :: a3,a4

      num=ensdim/maxens3
      num2=ensdim/maxens
      if(itest.eq.1)then
      do i=its,ite
       pr_gr(i,j) =  0.
       pr_w(i,j) =  0.
       pr_mc(i,j) = 0.
       pr_st(i,j) = 0.
       pr_as(i,j) = 0.
       pr_capma(i,j) =  0.
       pr_capme(i,j) = 0.
       pr_capmi(i,j) = 0.
      enddo
      endif

      do k=1,maxens
      do i=its,ite
        x_ave_cap(i,k)=0.
      enddo
      enddo
      do k=1,maxens3
      do i=its,ite
        x_ave(i,k)=0.
        x_std(i,k)=0.
        x_ske(i,k)=0.
        x_cur(i,k)=0.
      enddo
      enddo
      do i=its,ite
        xt_ave(i)=0.
        xt_std(i)=0.
        xt_ske(i)=0.
        xt_cur(i)=0.
      enddo
      do kk=1,num
      do k=1,maxens3
      do i=its,ite
        if(ierr(i).eq.0)then
        x_ave(i,k)=x_ave(i,k)+xf_ens(i,j,maxens3*(kk-1)+k)
        endif
      enddo
      enddo
      enddo
      do iedt=1,maxens2
      do k=1,maxens
      do kk=1,maxens3
      do i=its,ite
        if(ierr(i).eq.0)then
        x_ave_cap(i,k)=x_ave_cap(i,k)                               &
            +xf_ens(i,j,maxens3*(k-1)+(iedt-1)*maxens*maxens3+kk)
        endif
      enddo
      enddo
      enddo
      enddo
      do k=1,maxens
      do i=its,ite
        if(ierr(i).eq.0)then
        x_ave_cap(i,k)=x_ave_cap(i,k)/float(num2)
        endif
      enddo
      enddo

      do k=1,maxens3
      do i=its,ite
        if(ierr(i).eq.0)then
        x_ave(i,k)=x_ave(i,k)/float(num)
        endif
      enddo
      enddo
      do k=1,maxens3
      do i=its,ite
        if(ierr(i).eq.0)then
        xt_ave(i)=xt_ave(i)+x_ave(i,k)
        endif
      enddo
      enddo
      do i=its,ite
        if(ierr(i).eq.0)then
        xt_ave(i)=xt_ave(i)/float(maxens3)
        endif
      enddo



      do kk=1,num
      do k=1,maxens3
      do i=its,ite
        if(ierr(i).eq.0.and.x_ave(i,k).gt.0.)then

        x_std(i,k)=x_std(i,k)+(xf_ens(i,j,maxens3*(kk-1)+k)-x_ave(i,k))**2
        x_ske(i,k)=x_ske(i,k)+(xf_ens(i,j,maxens3*(kk-1)+k)-x_ave(i,k))**3
        x_cur(i,k)=x_cur(i,k)+(xf_ens(i,j,maxens3*(kk-1)+k)-x_ave(i,k))**4
        endif
      enddo
      enddo
      enddo
      do k=1,maxens3
      do i=its,ite
        if(ierr(i).eq.0.and.xt_ave(i).gt.0.)then
        xt_std(i)=xt_std(i)+(x_ave(i,k)-xt_ave(i))**2
        xt_ske(i)=xt_ske(i)+(x_ave(i,k)-xt_ave(i))**3
        xt_cur(i)=xt_cur(i)+(x_ave(i,k)-xt_ave(i))**4
        endif
      enddo
      enddo
      do k=1,maxens3
      do i=its,ite
        if(ierr(i).eq.0.and.x_std(i,k).gt.0.)then
           x_std(i,k)=x_std(i,k)/float(num)
           a3=max(1.e-6,x_std(i,k))
           x_std(i,k)=sqrt(a3)
           a3=max(1.e-6,x_std(i,k)**3)
           a4=max(1.e-6,x_std(i,k)**4)
           x_ske(i,k)=x_ske(i,k)/float(num)/a3
           x_cur(i,k)=x_cur(i,k)/float(num)/a4
        endif







      enddo
      enddo
      do i=its,ite
        if(ierr(i).eq.0.and.xt_std(i).gt.0.)then
           xt_std(i)=xt_std(i)/float(maxens3)
           a3=max(1.e-6,xt_std(i))
           xt_std(i)=sqrt(a3)
           a3=max(1.e-6,xt_std(i)**3)
           a4=max(1.e-6,xt_std(i)**4)
           xt_ske(i)=xt_ske(i)/float(maxens3)/a3
           xt_cur(i)=xt_cur(i)/float(maxens3)/a4








      if(itest.eq.1)then
       pr_gr(i,j) = .333*(x_ave(i,1)+x_ave(i,2)+x_ave(i,3))
       pr_w(i,j) = .333*(x_ave(i,4)+x_ave(i,5)+x_ave(i,6))
       pr_mc(i,j) = .333*(x_ave(i,7)+x_ave(i,8)+x_ave(i,9))
       pr_st(i,j) = .333*(x_ave(i,10)+x_ave(i,11)+x_ave(i,12))
       pr_as(i,j) = .25*(x_ave(i,13)+x_ave(i,14)+x_ave(i,15) &
                     + x_ave(i,16))
       pr_capma(i,j) = x_ave_cap(i,1)
       pr_capme(i,j) = x_ave_cap(i,2)
       pr_capmi(i,j) = x_ave_cap(i,3)



        else if (itest.eq.2)then
       APR_GR(i,j)=.333*(x_ave(i,1)+x_ave(i,2)+x_ave(i,3))*      &
                  3600.*pr_gr(i,j) +APR_GR(i,j)
       APR_W(i,j)=.333*(x_ave(i,4)+x_ave(i,5)+x_ave(i,6))*       &
                  3600.*pr_w(i,j) +APR_W(i,j)
       APR_MC(i,j)=.333*(x_ave(i,7)+x_ave(i,8)+x_ave(i,9))*      &
                  3600.*pr_mc(i,j) +APR_MC(i,j)
       APR_ST(i,j)=.333*(x_ave(i,10)+x_ave(i,11)+x_ave(i,12))*   &
                  3600.*pr_st(i,j) +APR_ST(i,j)
       APR_AS(i,j)=.25*(x_ave(i,13)+x_ave(i,14)+x_ave(i,15)      &
                           + x_ave(i,16))*                       &
                  3600.*pr_as(i,j) +APR_AS(i,j)
       APR_CAPMA(i,j) = x_ave_cap(i,1)*                          &
                  3600.*pr_capma(i,j) +APR_CAPMA(i,j)
       APR_CAPME(i,j) = x_ave_cap(i,2)*                          &
                  3600.*pr_capme(i,j) +APR_CAPME(i,j)
       APR_CAPMI(i,j) = x_ave_cap(i,3)*                          &
                  3600.*pr_capmi(i,j) +APR_CAPMI(i,j)
        endif
        endif
      enddo

   END SUBROUTINE massflx_stats


   SUBROUTINE neg_check(dt,q,outq,outt,outqc,pret,its,ite,kts,kte,itf,ktf)

   INTEGER,      INTENT(IN   ) ::            its,ite,kts,kte,itf,ktf

     real, dimension (its:ite,kts:kte  )                    ,                 &
      intent(inout   ) ::                                                     &
       q,outq,outt,outqc
     real, dimension (its:ite  )                            ,                 &
      intent(inout   ) ::                                                     &
       pret
     real                                                                     &
        ,intent (in  )                   ::                                   &
        dt
     real :: thresh,qmem,qmemf,qmem2,qtest,qmem1



      thresh=200.01
      do i=its,itf
      qmemf=1.
      qmem=0.
      do k=kts,ktf
         qmem=outt(i,k)*86400.
         if(qmem.gt.2.*thresh)then
           qmem2=2.*thresh/qmem
           qmemf=min(qmemf,qmem2)



         endif
         if(qmem.lt.-thresh)then
           qmem2=-thresh/qmem
           qmemf=min(qmemf,qmem2)



         endif
      enddo
      do k=kts,ktf
         outq(i,k)=outq(i,k)*qmemf
         outt(i,k)=outt(i,k)*qmemf
         outqc(i,k)=outqc(i,k)*qmemf
      enddo
      pret(i)=pret(i)*qmemf 
      enddo






      thresh=1.e-10
      do i=its,itf
      qmemf=1.
      do k=kts,ktf
         qmem=outq(i,k)
         if(abs(qmem).gt.0.)then
         qtest=q(i,k)+outq(i,k)*dt
         if(qtest.lt.thresh)then



           qmem1=outq(i,k)
           qmem2=(thresh-q(i,k))/dt
           qmemf=min(qmemf,qmem2/qmem1)


         endif
         endif
      enddo
      do k=kts,ktf
         outq(i,k)=outq(i,k)*qmemf
         outt(i,k)=outt(i,k)*qmemf
         outqc(i,k)=outqc(i,k)*qmemf
      enddo
      pret(i)=pret(i)*qmemf 
      enddo

   END SUBROUTINE neg_check



END MODULE module_cu_gd
