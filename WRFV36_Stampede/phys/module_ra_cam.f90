MODULE module_ra_cam
  use module_ra_cam_support
  use module_cam_support, only: endrun

  implicit none





   real(r8) abarl(4)         
   real(r8) bbarl(4)         
   real(r8) cbarl(4)         
   real(r8) dbarl(4)         
   real(r8) ebarl(4)         
   real(r8) fbarl(4)         

   save abarl, bbarl, cbarl, dbarl, ebarl, fbarl

   data abarl/ 2.817e-02, 2.682e-02,2.264e-02,1.281e-02/
   data bbarl/ 1.305    , 1.346    ,1.454    ,1.641    /
   data cbarl/-5.62e-08 ,-6.94e-06 ,4.64e-04 ,0.201    /
   data dbarl/ 1.63e-07 , 2.35e-05 ,1.24e-03 ,7.56e-03 /
   data ebarl/ 0.829    , 0.794    ,0.754    ,0.826    /
   data fbarl/ 2.482e-03, 4.226e-03,6.560e-03,4.353e-03/







   real(r8) abari(4)         
   real(r8) bbari(4)         
   real(r8) cbari(4)         
   real(r8) dbari(4)         
   real(r8) ebari(4)         
   real(r8) fbari(4)         

   save abari, bbari, cbari, dbari, ebari, fbari

   data abari/ 3.448e-03, 3.448e-03,3.448e-03,3.448e-03/
   data bbari/ 2.431    , 2.431    ,2.431    ,2.431    /
   data cbari/ 1.00e-05 , 1.10e-04 ,1.861e-02,.46658   /
   data dbari/ 0.0      , 1.405e-05,8.328e-04,2.05e-05 /
   data ebari/ 0.7661   , 0.7730   ,0.794    ,0.9595   /
   data fbari/ 5.851e-04, 5.665e-04,7.267e-04,1.076e-04/


   real(r8) delta            
   real(r8) o2mmr            

   save delta, o2mmr




   data delta / 0.0014257179260883 /



   data o2mmr / .23143 /



   real(r8) frcsol(nspint)   
   real(r8) wavmin(nspint)   
   real(r8) wavmax(nspint)   
   real(r8) raytau(nspint)   
   real(r8) abh2o(nspint)    
   real(r8) abo3 (nspint)    
   real(r8) abco2(nspint)    
   real(r8) abo2 (nspint)    
   real(r8) ph2o(nspint)     
   real(r8) pco2(nspint)     
   real(r8) po2 (nspint)     
   real(r8) nirwgt(nspint)   
   save frcsol ,wavmin ,wavmax ,raytau ,abh2o ,abo3 , &
        abco2  ,abo2   ,ph2o   ,pco2   ,po2   ,nirwgt

   data frcsol / .001488, .001389, .001290, .001686, .002877, &
                 .003869, .026336, .360739, .065392, .526861, &
                 .526861, .526861, .526861, .526861, .526861, &
                 .526861, .006239, .001834, .001834/



   data nirwgt /  0.0,   0.0,   0.0,      0.0,   0.0, &
                  0.0,   0.0,   0.0, 0.320518,   1.0,  1.0, &
                  1.0,   1.0,   1.0,      1.0,   1.0, &
                  1.0,   1.0,   1.0 /

   data wavmin / .200,  .245,  .265,  .275,  .285, &
                 .295,  .305,  .350,  .640,  .700,  .701, &
                 .701,  .701,  .701,  .702,  .702, &
                 2.630, 4.160, 4.160/

   data wavmax / .245,  .265,  .275,  .285,  .295, &
                 .305,  .350,  .640,  .700, 5.000, 5.000, &
                 5.000, 5.000, 5.000, 5.000, 5.000, &
                 2.860, 4.550, 4.550/




   real(r8) v_raytau_35
   real(r8) v_raytau_64
   real(r8) v_abo3_35
   real(r8) v_abo3_64
   parameter( &
        v_raytau_35 = 0.155208, &
        v_raytau_64 = 0.0392, &
        v_abo3_35 = 2.4058030e+01, &  
        v_abo3_64 = 2.210e+01 &
        )

   data raytau / 4.020, 2.180, 1.700, 1.450, 1.250, &
                  1.085, 0.730, v_raytau_35, v_raytau_64, &
                  0.02899756, 0.01356763, 0.00537341, &
                  0.00228515, 0.00105028, 0.00046631, &
                  0.00025734, &
                 .0001, .0001, .0001/










   data abh2o /    .000,     .000,    .000,    .000,    .000, &
                   .000,     .000,    .000,    .000,    &
                   0.00256608,  0.06310504,   0.42287445, 2.45397941, &
                  11.20070807, 47.66091389, 240.19010243, &
                   .000,    .000,    .000/




   data abo3  /5.370e+04, 13.080e+04,  9.292e+04, 4.530e+04, 1.616e+04, &
               4.441e+03,  1.775e+02, v_abo3_35, v_abo3_64,      .000, &
               .000,   .000    ,   .000   ,   .000   ,      .000, &
               .000,   .000    ,   .000   ,   .000    /

   data abco2  /   .000,     .000,    .000,    .000,    .000, &
                   .000,     .000,    .000,    .000,    .000, &
                   .000,     .000,    .000,    .000,    .000, &
                   .000,     .094,    .196,   1.963/

   data abo2  /    .000,     .000,    .000,    .000,    .000, &
                   .000,     .000,    .000,1.11e-05,6.69e-05, &
                   .000,     .000,    .000,    .000,    .000, &  
                   .000,     .000,    .000,    .000/



   data ph2o  /    .000,     .000,    .000,    .000,    .000, &
        .000,     .000,    .000,    .000,    .505,     &
        .210,     .120,    .070,    .048,    .029,     &
        .018,     .000,    .000,    .000/

   data pco2  /    .000,     .000,    .000,    .000,    .000, &
        .000,     .000,    .000,    .000,    .000,     &
        .000,     .000,    .000,    .000,    .000,     &
        .000,    1.000,    .640,    .360/

   data po2   /    .000,     .000,    .000,    .000,    .000, &
        .000,     .000,    .000,   1.000,   1.000,     &
        .000,     .000,    .000,    .000,    .000,     &
        .000,     .000,    .000,    .000/

   real(r8) amo                 
   save     amo

   data amo   /  48.0000   /

contains
subroutine camrad(RTHRATENLW,RTHRATENSW,                           &
                     dolw,dosw,                                    &
                     SWUPT,SWUPTC,SWDNT,SWDNTC,                    &
                     LWUPT,LWUPTC,LWDNT,LWDNTC,                    &
                     SWUPB,SWUPBC,SWDNB,SWDNBC,                    &
                     LWUPB,LWUPBC,LWDNB,LWDNBC,                    &
                     swcf,lwcf,olr,cemiss,taucldc,taucldi,coszr,   &
                     GSW,GLW,XLAT,XLONG,                           &
                     ALBEDO,t_phy,TSK,EMISS,                       &
                     QV3D,QC3D,QR3D,QI3D,QS3D,QG3D,                &
                     ALSWVISDIR,ALSWVISDIF,                        & 
                     ALSWNIRDIR,ALSWNIRDIF,                        & 
                     SWVISDIR,SWVISDIF,                            & 
                     SWNIRDIR,SWNIRDIF,                            & 
                     sf_surface_physics,                           & 
                     F_QV,F_QC,F_QR,F_QI,F_QS,F_QG,                &
                     f_ice_phy,f_rain_phy,                         &
                     p_phy,p8w,z,pi_phy,rho_phy,dz8w,               &
                     CLDFRA,XLAND,XICE,SNOW,                        &
                     ozmixm,pin0,levsiz,num_months,                 &
                     m_psp,m_psn,aerosolcp,aerosolcn,m_hybi0,       &
                     cam_abs_dim1, cam_abs_dim2,                    &
                     paerlev,naer_c,                                &
                     GMT,JULDAY,JULIAN,YR,DT,XTIME,DECLIN,SOLCON,         &
                     RADT,DEGRAD,n_cldadv,                                  &
                     abstot_3d, absnxt_3d, emstot_3d,              &
                     doabsems,                                     &
                     ids,ide, jds,jde, kds,kde,                    &
                     ims,ime, jms,jme, kms,kme,                    &
                     its,ite, jts,jte, kts,kte,                    &
                     coszen                                        )


   USE MODULE_RA_CLWRF_SUPPORT, ONLY : read_CAMgases
   USE module_wrf_error
   USE module_state_description, ONLY : SSIBSCHEME, CLMSCHEME          


   IMPLICIT NONE


   INTEGER,    INTENT(IN   ) ::        ids,ide, jds,jde, kds,kde, &
                                       ims,ime, jms,jme, kms,kme, &
                                       its,ite, jts,jte, kts,kte
   LOGICAL,    INTENT(IN   ) ::        F_QV,F_QC,F_QR,F_QI,F_QS,F_QG
   LOGICAL,    INTENT(INout) ::        doabsems
   LOGICAL,    INTENT(IN   ) ::        dolw,dosw

   INTEGER,    INTENT(IN  )  ::        n_cldadv
   INTEGER,    INTENT(IN  )  ::        JULDAY
   REAL,       INTENT(IN  )  ::        JULIAN
   INTEGER,    INTENT(IN  )  ::        YR
   REAL,       INTENT(IN  )  ::        DT
   INTEGER,      INTENT(IN   )    ::   levsiz, num_months
   INTEGER,      INTENT(IN   )    ::   paerlev, naer_c
   INTEGER,      INTENT(IN   )    ::   cam_abs_dim1, cam_abs_dim2


   REAL, INTENT(IN    )      ::        RADT,DEGRAD,             &
                                       XTIME,DECLIN,SOLCON,GMT


   REAL, DIMENSION( ims:ime, kms:kme, jms:jme ),                  &
         INTENT(IN    ) ::                                   P_PHY, &
                                                           P8W, &
                                                             Z, &
                                                            pi_PHY, &
                                                           rho_PHY, &
                                                              dz8w, &
                                                             T_PHY, &
                                                            QV3D, &
                                                            QC3D, &
                                                            QR3D, &
                                                            QI3D, &
                                                            QS3D, &
                                                            QG3D, &
                                                        CLDFRA

   REAL, DIMENSION( ims:ime, kms:kme, jms:jme ),                  &
         INTENT(INOUT)  ::                              RTHRATENLW, &
                                                        RTHRATENSW

   REAL, DIMENSION( ims:ime, jms:jme ),                           &
         INTENT(IN   )  ::                                  XLAT, &
                                                           XLONG, &
                                                           XLAND, &
                                                           XICE, &
                                                           SNOW, &
                                                           EMISS, &
                                                             TSK, &
                                                             ALBEDO

   REAL,  DIMENSION( ims:ime, levsiz, jms:jme, num_months ),      &
          INTENT(IN   ) ::                                  OZMIXM

   REAL,  DIMENSION(levsiz), INTENT(IN )  ::                   PIN0

   REAL,  DIMENSION(ims:ime,jms:jme), INTENT(IN )  ::      m_psp,m_psn
   REAL,  DIMENSION(paerlev), intent(in)             ::      m_hybi0
   REAL,  DIMENSION( ims:ime, paerlev, jms:jme, naer_c ),      &
          INTENT(IN   ) ::                    aerosolcp, aerosolcn


   REAL, DIMENSION( ims:ime, jms:jme ),                           &
         INTENT(INOUT)  ::                                   GSW, GLW


   REAL, DIMENSION( ims:ime, jms:jme ),                           &
         INTENT(IN)     ::                            ALSWVISDIR, &
                                                      ALSWVISDIF, &
                                                      ALSWNIRDIR, &
                                                      ALSWNIRDIF

   REAL, DIMENSION( ims:ime, jms:jme ),                           &
         INTENT(OUT)    ::                              SWVISDIR, &
                                                        SWVISDIF, &
                                                        SWNIRDIR, &
                                                        SWNIRDIF
   INTEGER, INTENT(IN) :: sf_surface_physics



   REAL, DIMENSION( ims:ime, kms:kme, cam_abs_dim2 , jms:jme ),           &
         INTENT(INOUT)  ::                                  abstot_3d
   REAL, DIMENSION( ims:ime, kms:kme, cam_abs_dim1 , jms:jme ),           &
         INTENT(INOUT)  ::                                  absnxt_3d
   REAL, DIMENSION( ims:ime, kms:kme, jms:jme ),           &
         INTENT(INOUT)  ::                                  emstot_3d
















   REAL, DIMENSION( ims:ime, jms:jme ), OPTIONAL, INTENT(INOUT) ::&
                    SWUPT,SWUPTC,SWDNT,SWDNTC,                    &
                    LWUPT,LWUPTC,LWDNT,LWDNTC,                    &
                    SWUPB,SWUPBC,SWDNB,SWDNBC,                    &
                    LWUPB,LWUPBC,LWDNB,LWDNBC

   REAL, DIMENSION( ims:ime, jms:jme ),                           &
         INTENT(INOUT)  ::                                  swcf, &
                                                            lwcf, &
                                                             olr, &
                                                            coszr    
   REAL, DIMENSION( ims:ime, kms:kme, jms:jme )                 , &
         INTENT(OUT   )  ::                               cemiss, &        
                                                         taucldc, &        
                                                         taucldi           


   REAL, DIMENSION( ims:ime, kms:kme, jms:jme ),                     &
         INTENT(IN   ) ::                                            &
                                                          F_ICE_PHY, &
                                                         F_RAIN_PHY

  real, dimension(ims:ime,jms:jme), optional, intent(in) :: coszen


 
   INTEGER :: lchnk, ncol, pcols, pver, pverp, pverr, pverrp
   INTEGER :: pcnst, pnats, ppcnst, i, j, k, ii, kk, kk1, m, n
   integer :: begchunk, endchunk
   integer :: nyrm, nyrp
   real(r8) doymodel, doydatam, doydatap, deltat, fact1, fact2

   REAL :: XT24, TLOCTM, HRANG, XXLAT, oldXT24
 
   real(r8), DIMENSION( 1:ite-its+1 ) :: coszrs, landfrac, landm, snowh, icefrac, lwups
   real(r8), DIMENSION( 1:ite-its+1 ) :: asdir, asdif, aldir, aldif, ps
   real(r8), DIMENSION( 1:ite-its+1, 1:kte-kts+1 ) :: cld, pmid, lnpmid, pdel, zm, t
   real(r8), DIMENSION( 1:ite-its+1, 1:kte-kts+2 ) ::  pint, lnpint
   real(r8), DIMENSION( 1:ite-its+1, 1:kte-kts+1, n_cldadv) :: q


    real(r8), dimension(  1:ite-its+1, 1:kte-kts+1 ) :: cicewp      
    real(r8), dimension(  1:ite-its+1, 1:kte-kts+1 ) :: cliqwp      
    real(r8), dimension(  1:ite-its+1, 0:kte-kts+1 ) :: tauxcl      
    real(r8), dimension(  1:ite-its+1, 0:kte-kts+1 ) :: tauxci      
    real(r8), dimension(  1:ite-its+1, 1:kte-kts+1 ) :: emis        
    real(r8), dimension(  1:ite-its+1, 1:kte-kts+1 ) :: rel         
    real(r8), dimension(  1:ite-its+1, 1:kte-kts+1 ) :: rei         
    real(r8), dimension(  1:ite-its+1, 1:kte-kts+2 ) :: pmxrgn      
    integer , dimension(  1:ite-its+1 ) :: nmxrgn               

   real(r8), dimension(  1:ite-its+1 ) :: fsns          
   real(r8), dimension(  1:ite-its+1 ) :: fsnt          
   real(r8), dimension(  1:ite-its+1 ) :: flns          
   real(r8), dimension(  1:ite-its+1 ) :: flnt          

   real(r8), dimension(  1:ite-its+1, 1:kte-kts+2 )  :: fsup        
   real(r8), dimension(  1:ite-its+1, 1:kte-kts+2 )  :: fsupc       
   real(r8), dimension(  1:ite-its+1, 1:kte-kts+2 )  :: fsdn        
   real(r8), dimension(  1:ite-its+1, 1:kte-kts+2 )  :: fsdnc       
   real(r8), dimension(  1:ite-its+1, 1:kte-kts+2 )  :: flup        
   real(r8), dimension(  1:ite-its+1, 1:kte-kts+2 )  :: flupc       
   real(r8), dimension(  1:ite-its+1, 1:kte-kts+2 )  :: fldn        
   real(r8), dimension(  1:ite-its+1, 1:kte-kts+2 )  :: fldnc       
   real(r8), dimension(  1:ite-its+1 ) :: swcftoa                 
   real(r8), dimension(  1:ite-its+1 ) :: lwcftoa                 
   real(r8), dimension(  1:ite-its+1 ) :: olrtoa                  

   real(r8), dimension(  1:ite-its+1 ) :: sols          
   real(r8), dimension(  1:ite-its+1 ) :: soll          
   real(r8), dimension(  1:ite-its+1 ) :: solsd         
   real(r8), dimension(  1:ite-its+1 ) :: solld         
   real(r8), dimension(  1:ite-its+1, 1:kte-kts+1 ) :: qrs      
   real(r8), dimension(  1:ite-its+1 ) :: fsds          
   real(r8), dimension(  1:ite-its+1, 1:kte-kts+1 ) :: qrl      
   real(r8), dimension(  1:ite-its+1 ) :: flwds          
   real(r8), dimension(  1:ite-its+1, levsiz, num_months ) :: ozmixmj        
   real(r8), dimension(  1:ite-its+1, levsiz ) :: ozmix          
   real(r8), dimension(levsiz)         :: pin            
   real(r8), dimension(1:ite-its+1)    :: m_psjp,m_psjn          
   real(r8), dimension(  1:ite-its+1, paerlev, naer_c ) :: aerosoljp        
   real(r8), dimension(  1:ite-its+1, paerlev, naer_c ) :: aerosoljn        
   real(r8), dimension(paerlev)                           :: m_hybi
   real(r8), dimension(1:ite-its+1 )          :: clat           
   real(r8), dimension(its:ite,kts:kte+1,kts:kte+1) :: abstot 
   real(r8), dimension(its:ite,kts:kte,4)           :: absnxt 
   real(r8), dimension(its:ite,kts:kte+1)           :: emstot 
   CHARACTER(LEN=256) :: msgstr


   LOGICAL, EXTERNAL                                :: wrf_dm_on_monitor
   CHARACTER(LEN=256)                               :: message



   lchnk = 1
   begchunk = ims
   endchunk = ime
   ncol = ite - its + 1
   pcols= ite - its + 1
   pver = kte - kts + 1
   pverp= pver + 1
   pverr = kte - kts + 1
   pverrp= pverr + 1

   ppcnst = n_cldadv

   pnats = 0
   pcnst = ppcnst-pnats





   if(naer_c.ne.naer_all) then
             WRITE( wrf_err_message , * ) 'naer_c-1 ne naer_all ', naer_c, naer_all
             CALL wrf_error_fatal3("<stdin>",440,&
wrf_err_message )
   endif 


  






   nyrm     = yr - yrdata(1) + 1
   nyrp     = nyrm + 1
   doymodel = yr*365.    + julian
   doydatam = yrdata(nyrm)*365. + 1.
   doydatap = yrdata(nyrp)*365. + 1.
   deltat   = doydatap - doydatam
   fact1    = (doydatap - doymodel)/deltat
   fact2    = (doymodel - doydatam)/deltat
   co2vmr = (co2(nyrm)*fact1 + co2(nyrp)*fact2)*1.e-06

   IF ( wrf_dm_on_monitor() ) THEN
     WRITE(message,*)'CAM interpolated values_____ year:',yr,' julian day:',julian
     call wrf_debug( 100, message)
     WRITE(message,*)'CAM co2vmr: ',co2vmr,' n2ovmr:',n2ovmr,' ch4vmr:',ch4vmr,' cfc11:',f11vmr,&
       ' cfc12:',f12vmr
     call wrf_debug( 100, message)
   ENDIF


   co2mmr=co2vmr*mwco2/mwdry





      do k=1,levsiz
      pin(k)=pin0(k)
      enddo

      do k=1,paerlev
      m_hybi(k)=m_hybi0(k)
      enddo


      if(abstot_3d(its,kts,kts,jts) .eq. 0.0 .and. .not.doabsems .and. dolw)then
        CALL wrf_debug(0, 'camrad lw: CAUTION: re-calculating abstot, absnxt, emstot on restart')
        doabsems = .true.
      endif

   do j =jts,jte







      if (present(coszen)) then
         do i=its,ite
            ii=i-its+1
            clat(ii)=XLAT(I,J)*DEGRAD
            coszrs(ii)=coszen(i,j)
         enddo
      else
         do i = its,ite
            ii = i - its + 1
            
            
            
            
            XT24=MOD(XTIME+RADT*0.5,1440.)
            TLOCTM=GMT+XT24/60.+XLONG(I,J)/15.
            HRANG=15.*(TLOCTM-12.)*DEGRAD
            XXLAT=XLAT(I,J)*DEGRAD
            clat(ii)=xxlat
            coszrs(II)=SIN(XXLAT)*SIN(DECLIN)+COS(XXLAT)*COS(DECLIN)*COS(HRANG)
         enddo
      end if



      do k = kts,kte
      kk = kte - k + kts 
      do i = its,ite
      ii = i - its + 1

      q(ii,kk,1) = max(1.e-10,qv3d(i,k,j)/(1.+qv3d(i,k,j)))
     IF ( F_QI .and. F_QC .and. F_QS ) THEN
      q(ii,kk,ixcldliq) = max(0.,qc3d(i,k,j)/(1.+qv3d(i,k,j)))
      q(ii,kk,ixcldice) = max(0.,(qi3d(i,k,j)+qs3d(i,k,j))/(1.+qv3d(i,k,j)))
     ELSE IF ( F_QC .and. F_QR ) THEN

      q(ii,kk,ixcldliq) = 0.
      q(ii,kk,ixcldice) = 0.
      if(t_phy(i,k,j).gt.273.15)q(ii,kk,ixcldliq) = max(0.,qc3d(i,k,j)/(1.+qv3d(i,k,j)))
      if(t_phy(i,k,j).le.273.15)q(ii,kk,ixcldice) = max(0.,qc3d(i,k,j)/(1.+qv3d(i,k,j)))
     ELSE IF ( F_QC .and. F_QS ) THEN

      q(ii,kk,ixcldice) = max(0.,qc3d(i,k,j)/(1.+qv3d(i,k,j))*f_ice_phy(i,k,j))
      q(ii,kk,ixcldliq) = max(0.,qc3d(i,k,j)/(1.+qv3d(i,k,j))*(1.-f_ice_phy(i,k,j))*(1.-f_rain_phy(i,k,j)))
     ELSE
      q(ii,kk,ixcldliq) = 0.
      q(ii,kk,ixcldice) = 0.
     ENDIF
      cld(ii,kk) = CLDFRA(I,K,J)
      enddo
      enddo

      do i = its,ite
      ii = i - its + 1
      landfrac(ii) = 2.-XLAND(I,J)
      landm(ii) = landfrac(ii)
      snowh(ii) = 0.001*SNOW(I,J)
      icefrac(ii) = XICE(I,J)
      enddo

      do m=1,num_months-1
      do k=1,levsiz
      do i = its,ite
      ii = i - its + 1
      ozmixmj(ii,k,m) = ozmixm(i,k,j,m+1)
      enddo
      enddo
      enddo

      do i = its,ite
      ii = i - its + 1
      m_psjp(ii) = m_psp(i,j)
      m_psjn(ii) = m_psn(i,j)
      enddo

      do n=1,naer_c
      do k=1,paerlev
      do i = its,ite
      ii = i - its + 1
      aerosoljp(ii,k,n) = aerosolcp(i,k,j,n)
      aerosoljn(ii,k,n) = aerosolcn(i,k,j,n)
      enddo
      enddo
      enddo




      do i = its,ite
      ii = i - its + 1
      lwups(ii) = stebol*EMISS(I,J)*TSK(I,J)**4
      enddo

      do k = kts,kte+1
      kk = kte - k + kts + 1
      do i = its,ite
      ii = i - its + 1
      pint(ii,kk) = p8w(i,k,j)
      if(k.eq.kts)ps(ii)=pint(ii,kk)
      lnpint(ii,kk) = log(pint(ii,kk))
      enddo
      enddo

      if(.not.doabsems .and. dolw)then

      do kk = 1,cam_abs_dim2
        do kk1 = kts,kte+1
          do i = its,ite
            abstot(i,kk1,kk) = abstot_3d(i,kk1,kk,j)
          enddo
        enddo
      enddo

      do kk = 1,cam_abs_dim1
        do kk1 = kts,kte
          do i = its,ite
            absnxt(i,kk1,kk) = absnxt_3d(i,kk1,kk,j)
          enddo
        enddo
      enddo
      do kk = kts,kte+1
          do i = its,ite
            emstot(i,kk) = emstot_3d(i,kk,j)
          enddo
      enddo
      endif

      do k = kts,kte
      kk = kte - k + kts 
      do i = its,ite
      ii = i - its + 1
      pmid(ii,kk) = p_phy(i,k,j)
      lnpmid(ii,kk) = log(pmid(ii,kk))
      lnpint(ii,kk) = log(pint(ii,kk))
      pdel(ii,kk) = pint(ii,kk+1) - pint(ii,kk)
      t(ii,kk) = t_phy(i,k,j)
      zm(ii,kk) = z(i,k,j)
      enddo
      enddo




      call param_cldoptics_calc(ncol, pcols, pver, pverp, pverr, pverrp, ppcnst, q, cld, landfrac, landm,icefrac, &
                                pdel, t, ps, pmid, pint, cicewp, cliqwp, emis, rel, rei, pmxrgn, nmxrgn, snowh)


   SELECT CASE(sf_surface_physics)
   CASE (SSIBSCHEME)
   if (xtime .gt. 1.0) then

      do i = its,ite
         ii = i - its + 1
         if (xland(i,j).lt.1.5) then   
           asdir(ii) = ALSWVISDIR(i,j) 
           asdif(ii) = ALSWVISDIF(i,j) 
           aldir(ii) = ALSWNIRDIR(i,j) 
           aldif(ii) = ALSWNIRDIF(i,j) 
         else
           asdir(ii) = albedo(i,j)
           asdif(ii) = albedo(i,j)
           aldir(ii) = albedo(i,j)
           aldif(ii) = albedo(i,j)
         endif
      enddo
   else
      do i = its,ite
         ii = i - its + 1
         asdir(ii) = albedo(i,j)
         asdif(ii) = albedo(i,j)
         aldir(ii) = albedo(i,j)
         aldif(ii) = albedo(i,j)
      enddo
   endif
   CASE (CLMSCHEME)
   if (xtime .gt. 1.0) then
      do i = its,ite
         ii = i - its + 1
         if (xland(i,j).lt.1.5) then   
           asdir(ii) = ALSWVISDIR(i,j) 
           asdif(ii) = ALSWVISDIF(i,j) 
           aldir(ii) = ALSWNIRDIR(i,j) 
           aldif(ii) = ALSWNIRDIF(i,j) 
         else
           asdir(ii) = albedo(i,j)
           asdif(ii) = albedo(i,j)
           aldir(ii) = albedo(i,j)
           aldif(ii) = albedo(i,j)
         endif
      enddo
   else
      do i = its,ite
         ii = i - its + 1
         asdir(ii) = albedo(i,j)
         asdif(ii) = albedo(i,j)
         aldir(ii) = albedo(i,j)
         aldif(ii) = albedo(i,j)
      enddo
   endif

   CASE DEFAULT
      do i = its,ite
      ii = i - its + 1


      asdir(ii) = albedo(i,j)
      asdif(ii) = albedo(i,j)
      aldir(ii) = albedo(i,j)
      aldif(ii) = albedo(i,j)
      enddo
   END SELECT





      call radctl (j,lchnk, ncol, pcols, pver, pverp, pverr, pverrp, ppcnst, pcnst, lwups, emis, pmid,             &
                   pint, lnpmid, lnpint, pdel, t, q,   &
                   cld, cicewp, cliqwp, tauxcl, tauxci, coszrs, clat, asdir, asdif,               &
                   aldir, aldif, solcon, GMT,JULDAY,JULIAN,DT,XTIME,   &
                   pin, ozmixmj, ozmix, levsiz, num_months,  & 
                   m_psjp,m_psjn, aerosoljp, aerosoljn,  m_hybi, paerlev, naer_c, pmxrgn, nmxrgn, &
                   dolw, dosw, doabsems, abstot, absnxt, emstot, &
                   fsup, fsupc, fsdn, fsdnc, flup, flupc, fldn, fldnc, swcftoa, lwcftoa, olrtoa,  &
                   fsns, fsnt    ,flns    ,flnt    , &
                   qrs, qrl, flwds, rel, rei,                       &
                   sols, soll, solsd, solld,                  &


                   landfrac, zm, fsds)

      do k = kts,kte
      kk = kte - k + kts 
      do i = its,ite
      ii = i - its + 1
      if(dolw)RTHRATENLW(I,K,J) = 1.e4*qrl(ii,kk)/(cpair*pi_phy(i,k,j))
      if(dosw)RTHRATENSW(I,K,J) = 1.e4*qrs(ii,kk)/(cpair*pi_phy(i,k,j))
      cemiss(i,k,j)     = emis(ii,kk)
      taucldc(i,k,j)    = tauxcl(ii,kk)
      taucldi(i,k,j)    = tauxci(ii,kk)
      enddo
      enddo

      if(doabsems .and. dolw)then

      do kk = 1,cam_abs_dim2
        do kk1 = kts,kte+1
          do i = its,ite
            abstot_3d(i,kk1,kk,j) = abstot(i,kk1,kk)
          enddo
        enddo
      enddo

      do kk = 1,cam_abs_dim1
        do kk1 = kts,kte
          do i = its,ite
            absnxt_3d(i,kk1,kk,j) = absnxt(i,kk1,kk)
          enddo
        enddo
      enddo
      do kk = kts,kte+1
          do i = its,ite
            emstot_3d(i,kk,j) = emstot(i,kk)
          enddo
      enddo
      endif

      IF(PRESENT(SWUPT))THEN
      if(dosw)then

      do k = kts,kte+1
      kk = kte +1 - k + kts
      do i = its,ite
      ii = i - its + 1




       if(k.eq.kte+1)then
         swupt(i,j)     = fsup(ii,kk)
         swuptc(i,j)    = fsupc(ii,kk)
         swdnt(i,j)     = fsdn(ii,kk)
         swdntc(i,j)    = fsdnc(ii,kk)
       endif
       if(k.eq.kts)then
         swupb(i,j)     = fsup(ii,kk)
         swupbc(i,j)    = fsupc(ii,kk)
         swdnb(i,j)     = fsdn(ii,kk)
         swdnbc(i,j)    = fsdnc(ii,kk)
       endif




     enddo
      enddo
      endif
      if(dolw)then

      do k = kts,kte+1
      kk = kte +1 - k + kts
      do i = its,ite
      ii = i - its + 1




       if(k.eq.kte+1)then
         lwupt(i,j)     = flup(ii,kk)
         lwuptc(i,j)    = flupc(ii,kk)
         lwdnt(i,j)     = fldn(ii,kk)
         lwdntc(i,j)    = fldnc(ii,kk)
       endif
       if(k.eq.kts)then
         lwupb(i,j)     = flup(ii,kk)
         lwupbc(i,j)    = flupc(ii,kk)
         lwdnb(i,j)     = fldn(ii,kk)
         lwdnbc(i,j)    = fldnc(ii,kk)
       endif




      enddo
      enddo
      endif
      ENDIF

      do i = its,ite
      ii = i - its + 1

      if(dolw)then
        GLW(I,J) = flwds(ii)
        lwcf(i,j) = lwcftoa(ii)
        olr(i,j)  = olrtoa(ii)
      endif
      if(dosw)then
        GSW(I,J) = fsns(ii)
        swcf(i,j) = swcftoa(ii)
        coszr(i,j) = coszrs(ii)
      endif
      enddo

   SELECT CASE(sf_surface_physics)
   CASE (SSIBSCHEME)

      if(dosw)then
      do i = its,ite
        ii = i - its + 1
        SWVISDIR(I,J) = sols(ii)  
        SWVISDIF(I,J) = solsd(ii) 
        SWNIRDIR(I,J) = soll(ii)  
        SWNIRDIF(I,J) = solld(ii) 
      enddo
      endif
  CASE (CLMSCHEME)
      if(dosw)then
      do i = its,ite
        ii = i - its + 1
        SWVISDIR(I,J) = sols(ii)  
        SWVISDIF(I,J) = solsd(ii) 
        SWNIRDIR(I,J) = soll(ii)  
        SWNIRDIF(I,J) = solld(ii) 
      enddo
      endif

   END SELECT


    enddo    


end subroutine camrad

   SUBROUTINE camradinit(                                           &
                         R_D,R_V,CP,G,STBOLT,EP_2,shalf,pptop,               &
                         ozmixm,pin,levsiz,XLAT,num_months,         &
                         m_psp,m_psn,m_hybi,aerosolcp,aerosolcn,    &
                         paerlev,naer_c,                            &
                     ids, ide, jds, jde, kds, kde,                  &
                     ims, ime, jms, jme, kms, kme,                  &
                     its, ite, jts, jte, kts, kte                   )

   USE module_wrf_error
   USE module_state_description
   


   IMPLICIT NONE

   INTEGER , INTENT(IN)           :: ids, ide, jds, jde, kds, kde,  &
                                     ims, ime, jms, jme, kms, kme,  &
                                     its, ite, jts, jte, kts, kte
   REAL, intent(in)               :: pptop
   REAL, INTENT(IN)               :: R_D,R_V,CP,G,STBOLT,EP_2

   REAL,     DIMENSION( kms:kme )  :: shalf

   INTEGER,      INTENT(IN   )    ::   levsiz, num_months
   INTEGER,      INTENT(IN   )    ::   paerlev, naer_c

   REAL, DIMENSION( ims:ime, jms:jme ), INTENT(IN   )  :: XLAT

   REAL,  DIMENSION( ims:ime, levsiz, jms:jme, num_months ),      &
          INTENT(INOUT   ) ::                                  OZMIXM

   REAL,  DIMENSION(levsiz), INTENT(INOUT )  ::                   PIN
   REAL,  DIMENSION(ims:ime, jms:jme), INTENT(INOUT )  ::                  m_psp,m_psn
   REAL,  DIMENSION(paerlev), INTENT(INOUT )  ::               m_hybi
   REAL,  DIMENSION( ims:ime, paerlev, jms:jme, naer_c ),      &
          INTENT(INOUT) ::                             aerosolcp,aerosolcn

   REAL(r8)    :: pstd
   REAL(r8)    :: rh2o, cpair


   IF ( .NOT. ALLOCATED( ksul   ) ) ALLOCATE( ksul( nrh, nspint ) )
   IF ( .NOT. ALLOCATED( wsul   ) ) ALLOCATE( wsul( nrh, nspint ) )
   IF ( .NOT. ALLOCATED( gsul   ) ) ALLOCATE( gsul( nrh, nspint ) )
   IF ( .NOT. ALLOCATED( ksslt  ) ) ALLOCATE( ksslt( nrh, nspint ) )
   IF ( .NOT. ALLOCATED( wsslt  ) ) ALLOCATE( wsslt( nrh, nspint ) )
   IF ( .NOT. ALLOCATED( gsslt  ) ) ALLOCATE( gsslt( nrh, nspint ) )
   IF ( .NOT. ALLOCATED( kcphil ) ) ALLOCATE( kcphil( nrh, nspint ) )
   IF ( .NOT. ALLOCATED( wcphil ) ) ALLOCATE( wcphil( nrh, nspint ) )
   IF ( .NOT. ALLOCATED( gcphil ) ) ALLOCATE( gcphil( nrh, nspint ) )

   IF ( .NOT. ALLOCATED(ah2onw  ) ) ALLOCATE( ah2onw(n_p, n_tp, n_u, n_te, n_rh) )
   IF ( .NOT. ALLOCATED(eh2onw  ) ) ALLOCATE( eh2onw(n_p, n_tp, n_u, n_te, n_rh) )
   IF ( .NOT. ALLOCATED(ah2ow   ) ) ALLOCATE( ah2ow(n_p, n_tp, n_u, n_te, n_rh) )
   IF ( .NOT. ALLOCATED(cn_ah2ow) ) ALLOCATE( cn_ah2ow(n_p, n_tp, n_u, n_te, n_rh) )
   IF ( .NOT. ALLOCATED(cn_eh2ow) ) ALLOCATE( cn_eh2ow(n_p, n_tp, n_u, n_te, n_rh) )
   IF ( .NOT. ALLOCATED(ln_ah2ow) ) ALLOCATE( ln_ah2ow(n_p, n_tp, n_u, n_te, n_rh) )
   IF ( .NOT. ALLOCATED(ln_eh2ow) ) ALLOCATE( ln_eh2ow(n_p, n_tp, n_u, n_te, n_rh) )

   ozncyc = .true.
   indirect = .true.
   ixcldliq = 2
   ixcldice = 3



   idxSUL = P_SUL
   idxSSLT = P_SSLT
   idxDUSTfirst = P_DUST1
   idxOCPHO = P_OCPHO
   idxCARBONfirst = P_OCPHO
   idxBCPHO = P_BCPHO
   idxOCPHI = P_OCPHI
   idxBCPHI = P_BCPHI
   idxBG = P_BG
   idxVOLC = P_VOLC

   pstd = 101325.0

   mwdry = 28.966            
   mwco2 =  44.              
   mwh2o = 18.016            
   mwch4 =  16.              
   mwn2o =  44.              
   mwf11 = 136.              
   mwf12 = 120.              
   cappa = R_D/CP
   rair = R_D
   tmelt = 273.16            
   r_universal = 6.02214e26 * STBOLT   
   latvap = 2.501e6          
   latice = 3.336e5          
   zvir = R_V/R_D - 1.
   rh2o = R_V
   cpair = CP

   epsqs = EP_2

   CALL radini(G, CP, EP_2, STBOLT, pstd*10.0 )
   CALL esinti(epsqs  ,latvap  ,latice  ,rh2o    ,cpair   ,tmelt   )
   CALL oznini(ozmixm,pin,levsiz,num_months,XLAT,                   &
                     ids, ide, jds, jde, kds, kde,                  &
                     ims, ime, jms, jme, kms, kme,                  &
                     its, ite, jts, jte, kts, kte)                   
   CALL aerosol_init(m_psp,m_psn,m_hybi,aerosolcp,aerosolcn,paerlev,naer_c,shalf,pptop,    &
                     ids, ide, jds, jde, kds, kde,                  &
                     ims, ime, jms, jme, kms, kme,                  &
                     its, ite, jts, jte, kts, kte)


   END SUBROUTINE camradinit


subroutine oznint(julday,julian,dt,gmt,xtime,ozmixmj,ozmix,levsiz,num_months,pcols)

      IMPLICIT NONE

   INTEGER,      INTENT(IN   )    ::   levsiz, num_months,pcols

   REAL(r8),  DIMENSION( pcols, levsiz, num_months ),      &
          INTENT(IN   ) ::                                  ozmixmj 

   REAL, INTENT(IN    )      ::        XTIME,GMT
   INTEGER, INTENT(IN )      ::        JULDAY
   REAL,    INTENT(IN )      ::        JULIAN
   REAL,    INTENT(IN )      ::        DT

   REAL(r8),  DIMENSION( pcols, levsiz ),      &
          INTENT(OUT  ) ::                                  ozmix
   
   REAL(r8)  :: intJULIAN
   integer   :: np1,np,nm,m,k,i
   integer   :: IJUL
   integer, dimension(12) ::  date_oz
   data date_oz/16, 45, 75, 105, 136, 166, 197, 228, 258, 289, 319, 350/
   real(r8) :: cdayozp, cdayozm
   real(r8) :: fact1, fact2
   logical  :: finddate
   CHARACTER(LEN=256) :: msgstr

   
   intJULIAN = JULIAN + 1.0_r8    

   IJUL=INT(intJULIAN)


   intJULIAN=intJULIAN-FLOAT(IJUL)
   IJUL=MOD(IJUL,365)
   IF(IJUL.EQ.0)IJUL=365
   intJULIAN=intJULIAN+IJUL
   np1=1
   finddate=.false.

   do m=1,12
   if(date_oz(m).gt.intjulian.and..not.finddate) then
     np1=m
     finddate=.true.
   endif
   enddo
   cdayozp=date_oz(np1)
   if(np1.gt.1) then
   cdayozm=date_oz(np1-1)
   np=np1
   nm=np-1
   else
   cdayozm=date_oz(12)
   np=np1
   nm=12
   endif
   call getfactors(ozncyc,np1, cdayozm, cdayozp,intjulian, &
                    fact1, fact2) 




      do k=1,levsiz
         do i=1,pcols
            ozmix(i,k) = ozmixmj(i,k,nm)*fact1 + ozmixmj(i,k,np)*fact2
         end do
      end do

END subroutine oznint


subroutine get_aerosol(c, julday, julian, dt, gmt, xtime, m_psp, m_psn, aerosoljp, &
  aerosoljn, m_hybi, paerlev, naer_c, pint, pcols, pver, pverp, pverr, pverrp, AEROSOLt, scale)


























   integer, intent(in) :: c                   
   integer, intent(in) :: paerlev, naer_c, pcols, pver, pverp, pverr, pverrp
   real(r8), intent(in) :: pint(pcols,pverp)  
   real(r8), intent(in) :: scale(naer_all)    
   REAL, INTENT(IN    )      ::        XTIME,GMT
   INTEGER, INTENT(IN )      ::        JULDAY
   REAL, INTENT(IN    )      ::        JULIAN
   REAL, INTENT(IN    )      ::        DT
   real(r8), intent(in   )      ::        m_psp(pcols),m_psn(pcols)  
   real(r8), intent(in   )   ::        aerosoljp(pcols,paerlev,naer_c) 
   real(r8), intent(in   )   ::        aerosoljn(pcols,paerlev,naer_c) 
   real(r8), intent(in   )   ::        m_hybi(paerlev)

   real(r8), intent(out) :: AEROSOLt(pcols, pver, naer_all) 



   real(r8) caldayloc                     
   real(r8) fact1, fact2                  

  integer :: nm = 1                
  integer :: np = 2                
  integer :: mo_nxt = bigint       
  integer :: mo_prv                       

  real(r8) :: cdaym = inf          
  real(r8) :: cdayp = inf          
  real(r8) :: Mid(12)              
  data Mid/16.5, 46.0, 75.5, 106.0, 136.5, 167.0, 197.5, 228.5, 259.0, 289.5, 320.0, 350.5 /

   integer i, k, j                        
   integer m                              
   integer lats(pcols),lons(pcols)        
   integer ncol                           
   INTEGER IJUL
   REAL(r8) intJULIAN

   real(r8) speciesmin(naer)              






   real(r8) AEROSOLm(pcols,pver,naer) 



   real(r8) AEROSOLp(pcols,pver,naer) 
   CHARACTER(LEN=256) :: msgstr

   
   intJULIAN = JULIAN + 1.0_r8    

   IJUL=INT(intJULIAN)


   intJULIAN=intJULIAN-FLOAT(IJUL)
   IJUL=MOD(IJUL,365)
   IF(IJUL.EQ.0)IJUL=365
   caldayloc=intJULIAN+IJUL

   if (caldayloc < Mid(1)) then
      mo_prv = 12
      mo_nxt =  1
   else if (caldayloc >= Mid(12)) then
      mo_prv = 12
      mo_nxt =  1
   else
      do i = 2 , 12
         if (caldayloc < Mid(i)) then
            mo_prv = i-1
            mo_nxt = i
            exit
         end if
      end do
   end if



   cdaym = Mid(mo_prv)
   cdayp = Mid(mo_nxt)




   call getfactors (.true., mo_nxt, cdaym, cdayp, caldayloc, &
                    fact1, fact2)






   ncol = pcols




   call vert_interpolate (m_psp, aerosoljp, m_hybi, paerlev, naer_c, pint, nm, AEROSOLm, pcols, pver, pverp, ncol, c)
   call vert_interpolate (m_psn, aerosoljn, m_hybi, paerlev, naer_c, pint, np, AEROSOLp, pcols, pver, pverp, ncol, c)




   do m=1,naer
      do k=1,pver
         do i=1,ncol
            AEROSOLt(i,k,m) = AEROSOLm(i,k,m)*fact1 + AEROSOLp(i,k,m)*fact2
         end do
      end do
   end do







   call background (c, ncol, pint, pcols, pverr, pverrp, AEROSOLt(:, :, idxBG))







    AEROSOLt(:,:,idxVOLC) = 0._r8






   speciesmin(:) = 0. 

   do m=1,naer
      do k=1,pver
         do i=1,ncol
            if (AEROSOLt(i, k, m) < speciesmin(m)) then
               write(6,*) 'AEROSOL_INTERPOLATE: negative mass mixing ratio, exiting'
               write(6,*) 'm, column, pver',m, i, k ,AEROSOLt(i, k, m)


               call endrun ()
            end if
         end do
      end do
   end do



   call scale_aerosols (AEROSOLt, pcols, pver, ncol, c, scale)

   return
end subroutine get_aerosol


subroutine aerosol_indirect(ncol,lchnk,pcols,pver,ppcnst,landfrac,pmid,t,qm1,cld,zm,rel)










  integer, intent(in) :: ncol                  
  integer, intent(in) :: lchnk                 
  integer, intent(in) :: pcols,pver,ppcnst

  real(r8), intent(in) :: landfrac(pcols)      
  real(r8), intent(in) :: pmid(pcols,pver)     
  real(r8), intent(in) :: t(pcols,pver)        
  real(r8), intent(in) :: qm1(pcols,pver,ppcnst) 
  real(r8), intent(in) :: cld(pcols,pver)      
  real(r8), intent(in) :: zm(pcols,pver)       
  real(r8), intent(in) :: rel(pcols,pver)      



  real(r8) locrhoair(pcols,pver)  
  real(r8) lwcwat(pcols,pver)     
  real(r8) sulfmix(pcols,pver)    
  real(r8) so4mass(pcols,pver)    
  real(r8) Aso4(pcols,pver)       
  real(r8) Ntot(pcols,pver)       
  real(r8) relmod(pcols,pver)     

  real(r8) wrel(pcols,pver)       
  real(r8) wlwc(pcols,pver)       
  real(r8) cldfrq(pcols,pver)     

  real(r8) locPi                  
  real(r8) Rdryair                
  real(r8) rhowat                 
  real(r8) Acoef                  

  real(r8) rekappa                
  real(r8) recoef                 
  real(r8) reexp                  
  real(r8) Ntotb                  

  real(r8) Cmarn                  
  real(r8) Cland                  
  real(r8) Hmarn                  
  real(r8) Hland                  
  parameter ( Cmarn = 50.0, Cland = 100.0 )
  parameter ( Hmarn = 1000.0, Hland = 2000.0 )
  real(r8) bgaer                  

  integer i,k     



  logical land    
  land(i) = nint(landfrac(i)).gt.0.5_r8

  if (indirect) then




    sulfmix(1:ncol,1:pver) = 0._r8

    locPi = 3.141592654
    Rdryair = 287.04
    rhowat = 1000.0
    Acoef = 1.2930E14
    recoef = 3.0/(4.0*locPi*rhowat)
    reexp = 1.0/3.0


    do k=pver,1,-1
      do i = 1,ncol
        locrhoair(i,k) = pmid(i,k)/( Rdryair*t(i,k) )
        lwcwat(i,k) = ( qm1(i,k,ixcldliq)/max(0.01_r8,cld(i,k)) )* &
                      locrhoair(i,k)

        so4mass(i,k) = sulfmix(i,k)*locrhoair(i,k)*0.001
        Aso4(i,k) = so4mass(i,k)*Acoef

        if (Aso4(i,k) <= 280.0) then
           Aso4(i,k) = max(36.0_r8,Aso4(i,k))
           Ntot(i,k) = -1.15E-3*Aso4(i,k)**2 + 0.963*Aso4(i,k)+5.30
           rekappa = 0.80
        else
           Aso4(i,k) = min(1500.0_r8,Aso4(i,k))
           Ntot(i,k) = -2.10E-4*Aso4(i,k)**2 + 0.568*Aso4(i,k)-27.9
           rekappa = 0.67
        end if
        if (land(i)) then 
           bgaer = Cland*exp(-(zm(i,k)/Hland))
           Ntot(i,k) = max(bgaer,Ntot(i,k))
        else
           bgaer = Cmarn*exp(-(zm(i,k)/Hmarn))
           Ntot(i,k) = max(bgaer,Ntot(i,k))
        end if

        if (k == pver) then
           Ntotb = Ntot(i,k)
        else
           Ntotb = Ntot(i,k+1)
        end if

        relmod(i,k) = (( (recoef*lwcwat(i,k))/(rekappa*Ntotb))**reexp)*10000.0
        relmod(i,k) = max(4.0_r8,relmod(i,k))
        relmod(i,k) = min(20.0_r8,relmod(i,k))
        if (cld(i,k) >= 0.01) then
           cldfrq(i,k) = 1.0
        else
           cldfrq(i,k) = 0.0
        end if
        wrel(i,k) = relmod(i,k)*cldfrq(i,k)
        wlwc(i,k) = lwcwat(i,k)*cldfrq(i,k)
      end do
    end do






  else
    do k = 1, pver
      do i = 1, ncol
        relmod(i,k) = rel(i,k)
      end do
    end do
  endif



  return
end subroutine aerosol_indirect


      subroutine aer_trn(aer_mpp, aer_trn_ttl, pcols, plev, plevp )









      implicit none





      integer, intent(in)         :: pcols, plev, plevp
      real(r8), intent(in) :: aer_mpp(pcols,plevp)





      real(r8), intent(out) ::  aer_trn_ttl(pcols,plevp,plevp,bnd_nbr_LW)




      integer bnd_idx           
      integer i                 
      integer k1                
      integer k2                
      real(r8) aer_pth_dlt      
                                
      real(r8) odap_aer_ttl     
                                



      if (strat_volcanic) then
        do bnd_idx=1,bnd_nbr_LW
           do i=1,pcols
              aer_trn_ttl(i,1,1,bnd_idx)=1.0
           end do
           do k1=2,plevp
              do i=1,pcols
                 aer_trn_ttl(i,k1,k1,bnd_idx)=1.0

                 aer_pth_dlt  = abs(aer_mpp(i,k1) - aer_mpp(i,1))
                 odap_aer_ttl = abs_cff_mss_aer(bnd_idx) * aer_pth_dlt

                 aer_trn_ttl(i,1,k1,bnd_idx) = exp(-1.66 * odap_aer_ttl)
              end do
           end do

           do k1=2,plev
              do k2=k1+1,plevp
                 do i=1,pcols
                    aer_trn_ttl(i,k1,k2,bnd_idx) = &
                         aer_trn_ttl(i,1,k2,bnd_idx) / &
                         aer_trn_ttl(i,1,k1,bnd_idx)
                 end do
              end do
           end do

           do k1=2,plevp
              do k2=1,k1-1
                 do i=1,pcols
                    aer_trn_ttl(i,k1,k2,bnd_idx)=aer_trn_ttl(i,k2,k1,bnd_idx)
                 end do
              end do
           end do
        end do
      else
        aer_trn_ttl = 1.0
      endif

      return
      end subroutine aer_trn

      subroutine aer_pth(aer_mass, aer_mpp, ncol, pcols, plev, plevp)






      implicit none




      integer, intent(in)        :: pcols, plev, plevp
      real(r8), intent(in):: aer_mass(pcols,plev)  
      integer, intent(in):: ncol


      real(r8), intent(out):: aer_mpp(pcols,plevp) 


      integer i      
      integer k      



      aer_mpp(1:ncol,1) =  0._r8
      do k=2,plevp
          aer_mpp(1:ncol,k) = aer_mpp(1:ncol,k-1) + aer_mass(1:ncol,k-1)
      enddo

      return
      end subroutine aer_pth

subroutine radctl(j, lchnk   ,ncol    , pcols, pver, pverp, pverr, pverrp, ppcnst, pcnst,  &
                  lwups   ,emis    ,          &
                  pmid    ,pint    ,pmln    ,piln    ,pdel    ,t       , &

                  qm1     ,cld     ,cicewp  ,cliqwp  ,tauxcl, tauxci, coszrs,  clat, &
                  asdir   ,asdif   ,aldir   ,aldif   ,solcon, GMT,JULDAY,JULIAN,DT,XTIME,  &
                  pin, ozmixmj, ozmix, levsiz, num_months,      &
                  m_psp, m_psn,  aerosoljp, aerosoljn, m_hybi, paerlev, naer_c, pmxrgn  , &
                  nmxrgn  ,                   &
                  dolw, dosw, doabsems, abstot, absnxt, emstot, &
                  fsup    ,fsupc   ,fsdn    ,fsdnc   , &
                  flup    ,flupc   ,fldn    ,fldnc   , &
                  swcf    ,lwcf    ,flut    ,          &
                  fsns    ,fsnt    ,flns    ,flnt    , &
                  qrs     ,qrl     ,flwds   ,rel     ,rei     , &
                  sols    ,soll    ,solsd   ,solld   , &


                  landfrac,zm      ,fsds     )




























   implicit none




   integer, intent(in) :: lchnk,j                 
   integer, intent(in) :: ncol                  
   integer, intent(in) :: levsiz                
   integer, intent(in) :: num_months            
   integer, intent(in) :: paerlev,naer_c          
   integer, intent(in) :: pcols, pver, pverp, pverr, pverrp, ppcnst, pcnst
   logical, intent(in) :: dolw,dosw,doabsems


   integer nspint            
   integer naer_groups       
   parameter ( nspint = 19 )
   parameter ( naer_groups = 7 )    


   real(r8), intent(in) :: lwups(pcols)         
   real(r8), intent(in) :: emis(pcols,pver)     
   real(r8), intent(in) :: pmid(pcols,pver)     
   real(r8), intent(in) :: pint(pcols,pverp)    
   real(r8), intent(in) :: pmln(pcols,pver)     
   real(r8), intent(in) :: rel(pcols,pver)      
   real(r8), intent(in) :: rei(pcols,pver)      
   real(r8), intent(in) :: piln(pcols,pverp)    
   real(r8), intent(in) :: pdel(pcols,pverp)    
   real(r8), intent(in) :: t(pcols,pver)        
   real(r8), intent(in) :: qm1(pcols,pver,ppcnst) 
   real(r8), intent(in) :: cld(pcols,pver)      
   real(r8), intent(in) :: cicewp(pcols,pver)   
   real(r8), intent(in) :: cliqwp(pcols,pver)   
   real(r8), intent(inout) :: tauxcl(pcols,0:pver) 
   real(r8), intent(inout) :: tauxci(pcols,0:pver) 
   real(r8), intent(in) :: coszrs(pcols)        
   real(r8), intent(in) :: clat(pcols)          
   real(r8), intent(in) :: asdir(pcols)         
   real(r8), intent(in) :: asdif(pcols)         
   real(r8), intent(in) :: aldir(pcols)         
   real(r8), intent(in) :: aldif(pcols)         
   real(r8), intent(in) :: landfrac(pcols)      
   real(r8), intent(in) :: zm(pcols,pver)       
   real(r8), intent(in) :: pin(levsiz)          
   real(r8), intent(in) :: ozmixmj(pcols,levsiz,num_months)  
   real(r8), intent(inout) :: ozmix(pcols,levsiz)  
   real, intent(in) :: solcon               
   REAL, INTENT(IN    )      ::        XTIME,GMT              
   INTEGER, INTENT(IN )      ::        JULDAY
   REAL,    INTENT(IN )      ::        JULIAN
   REAL,    INTENT(IN )      ::        DT
   real(r8), intent(in)     :: m_psp(pcols),m_psn(pcols)       
   real(r8), intent(in)     :: aerosoljp(pcols,paerlev,naer_c)   
   real(r8), intent(in)     :: aerosoljn(pcols,paerlev,naer_c)   
   real(r8), intent(in)     :: m_hybi(paerlev)

   real(r8), intent(inout) :: pmxrgn(pcols,pverp) 




   integer, intent(inout) :: nmxrgn(pcols)     

    real(r8) :: pmxrgnrf(pcols,pverp)             
    integer  :: nmxrgnrf(pcols)     




   real(r8), intent(out) :: fsns(pcols)          
   real(r8), intent(out) :: fsnt(pcols)          
   real(r8), intent(out) :: flns(pcols)          
   real(r8), intent(out) :: flnt(pcols)          
   real(r8), intent(out) :: sols(pcols)          
   real(r8), intent(out) :: soll(pcols)          
   real(r8), intent(out) :: solsd(pcols)         
   real(r8), intent(out) :: solld(pcols)         
   real(r8), intent(out) :: qrs(pcols,pver)      
   real(r8), intent(out) :: fsds(pcols)          

   real(r8), intent(out) :: fsup(pcols,pverp)    
   real(r8), intent(out) :: fsupc(pcols,pverp)   
   real(r8), intent(out) :: fsdn(pcols,pverp)    
   real(r8), intent(out) :: fsdnc(pcols,pverp)   
   real(r8), intent(out) :: flup(pcols,pverp)    
   real(r8), intent(out) :: flupc(pcols,pverp)   
   real(r8), intent(out) :: fldn(pcols,pverp)    
   real(r8), intent(out) :: fldnc(pcols,pverp)   
   real(r8), intent(out) :: swcf(pcols)          
   real(r8), intent(out) :: lwcf(pcols)          
   real(r8), intent(out) :: flut(pcols)          



   real(r8), intent(out) :: qrl(pcols,pver)      
   real(r8), intent(out) :: flwds(pcols)         

   real(r8), intent(inout) :: abstot(pcols,pverp,pverp) 
   real(r8), intent(inout) :: absnxt(pcols,pver,4)      
   real(r8), intent(inout) :: emstot(pcols,pverp)     





   integer i, k              

   integer :: in2o, ich4, if11, if12 

   real(r8) solin(pcols)         

   real(r8) fsntoa(pcols)        
   real(r8) fsntoac(pcols)       
   real(r8) fsnirt(pcols)        
   real(r8) fsnrtc(pcols)        
   real(r8) fsnirtsq(pcols)      
   real(r8) fsntc(pcols)         
   real(r8) fsnsc(pcols)         
   real(r8) fsdsc(pcols)         



   real(r8) flutc(pcols)         
   real(r8) flntc(pcols)         
   real(r8) flnsc(pcols)         
   real(r8) ftem(pcols,pver)     

   real(r8) pbr(pcols,pverr)     
   real(r8) pnm(pcols,pverrp)    
   real(r8) o3vmr(pcols,pverr)   
   real(r8) o3mmr(pcols,pverr)   
   real(r8) eccf                 
   real(r8) n2o(pcols,pver)      
   real(r8) ch4(pcols,pver)      
   real(r8) cfc11(pcols,pver)    
   real(r8) cfc12(pcols,pver)    
   real(r8) rh(pcols,pverr)      
   real(r8) lwupcgs(pcols)       

   real(r8) esat(pcols,pverr)    
   real(r8) qsat(pcols,pverr)    

   real(r8) :: frc_day(pcols) 
   real(r8) :: aertau(pcols,nspint,naer_groups) 
   real(r8) :: aerssa(pcols,nspint,naer_groups) 
   real(r8) :: aerasm(pcols,nspint,naer_groups) 
   real(r8) :: aerfwd(pcols,nspint,naer_groups) 

   real(r8) aerosol(pcols, pver, naer_all) 
   real(r8) scales(naer_all)               


   LOGICAL, EXTERNAL                                :: wrf_dm_on_monitor
   CHARACTER (LEN=256)         ::    message






   call oznint(julday,julian,dt,gmt,xtime,ozmixmj,ozmix,levsiz,num_months,pcols)

   call radozn(lchnk   ,ncol    &
              ,pcols, pver &
              ,pmid    ,pin, levsiz, ozmix, o3vmr   )






   call radinp(lchnk   ,ncol    ,pcols, pver, pverp,      &
               pmid    ,pint    ,o3vmr   , pbr     ,&
               pnm     ,eccf    ,o3mmr   )




   if (dosw) then




      call aqsat(t, pmid, esat, qsat, pcols, &
                 ncol, pver, 1, pver)

      



      rh(1:ncol,1:pver) = qm1(1:ncol,1:pver,1) / qsat(1:ncol,1:pver) * &
         ((1.0 - epsilo) * qsat(1:ncol,1:pver) + epsilo) / &
         ((1.0 - epsilo) * qm1(1:ncol,1:pver,1) + epsilo)

      if (radforce) then

         pmxrgnrf = pmxrgn
         nmxrgnrf = nmxrgn

         call get_rf_scales(scales)

         call get_aerosol(lchnk, julday, julian, dt, gmt, xtime, m_psp, m_psn, aerosoljp, &
           aerosoljn, m_hybi, paerlev, naer, pint, pcols, pver, pverp, pverr, pverrp, aerosol, scales)

         




         call aerosol_indirect(ncol,lchnk,pcols,pver,ppcnst,landfrac,pmid,t,qm1,cld,zm,rel)
   

         call radcswmx(j, lchnk   ,ncol ,pcols, pver, pverp,         &
                    pnm     ,pbr     ,qm1     ,rh      ,o3mmr   , &
                    aerosol ,cld     ,cicewp  ,cliqwp  ,rel     , &

                    rei     ,tauxcl  ,tauxci  ,eccf    ,coszrs  ,scon    ,solin   ,solcon , &
                    asdir   ,asdif   ,aldir   ,aldif   ,nmxrgnrf, &
                    pmxrgnrf,qrs     ,fsnt    ,fsntc   ,fsntoa  , &
                    fsntoac ,fsnirt  ,fsnrtc  ,fsnirtsq,fsns    , &
                    fsnsc   ,fsdsc   ,fsds    ,sols    ,soll    , &
                    solsd   ,solld   ,frc_day ,                   &
                    fsup    ,fsupc   ,fsdn    ,fsdnc   ,          &
                    aertau  ,aerssa  ,aerasm  ,aerfwd             )






            do i = 1, ncol
            solin(i) = solin(i)*1.e-3
            fsnt(i)  = fsnt(i) *1.e-3
            fsns(i)  = fsns(i) *1.e-3
            fsntc(i) = fsntc(i)*1.e-3
            fsnsc(i) = fsnsc(i)*1.e-3
            end do
         ftem(:ncol,:pver) = qrs(:ncol,:pver)/cpair








 
      endif 

      call get_int_scales(scales)

      call get_aerosol(lchnk, julday, julian, dt, gmt, xtime, m_psp, m_psn, aerosoljp, aerosoljn, &
             m_hybi, paerlev, naer, pint, pcols, pver, pverp, pverr, pverrp, aerosol, scales)

      


      call aerosol_indirect(ncol,lchnk,pcols,pver,ppcnst,landfrac,pmid,t,qm1,cld,zm,rel)


      call radcswmx(j, lchnk   ,ncol    ,pcols, pver, pverp,         &
                    pnm     ,pbr     ,qm1     ,rh      ,o3mmr   , &
                    aerosol ,cld     ,cicewp  ,cliqwp  ,rel     , &

                    rei     ,tauxcl  ,tauxci  ,eccf    ,coszrs  ,scon    ,solin   ,solcon , &
                    asdir   ,asdif   ,aldir   ,aldif   ,nmxrgn  , &
                    pmxrgn  ,qrs     ,fsnt    ,fsntc   ,fsntoa  , &
                    fsntoac ,fsnirt  ,fsnrtc  ,fsnirtsq,fsns    , &
                    fsnsc   ,fsdsc   ,fsds    ,sols    ,soll    , &
                    solsd   ,solld   ,frc_day ,                   &
                    fsup    ,fsupc   ,fsdn    ,fsdnc   ,          &
                    aertau  ,aerssa  ,aerasm  ,aerfwd             )






      do i=1,ncol
         solin(i) = solin(i)*1.e-3
         fsds(i)  = fsds(i)*1.e-3
         fsnirt(i)= fsnirt(i)*1.e-3
         fsnrtc(i)= fsnrtc(i)*1.e-3
         fsnirtsq(i)= fsnirtsq(i)*1.e-3
         fsnt(i)  = fsnt(i) *1.e-3
         fsns(i)  = fsns(i) *1.e-3
         fsntc(i) = fsntc(i)*1.e-3
         fsnsc(i) = fsnsc(i)*1.e-3
         fsdsc(i) = fsdsc(i)*1.e-3
         fsntoa(i)=fsntoa(i)*1.e-3
         fsntoac(i)=fsntoac(i)*1.e-3
         swcf(i)  = fsntoa(i) - fsntoac(i)
      end do
      ftem(:ncol,:pver) = qrs(:ncol,:pver)/cpair


         do k = 1, pverp
            do i = 1, ncol
            fsup(i,k)  = fsup(i,k)*1.e-3
            fsupc(i,k) = fsupc(i,k)*1.e-3
            fsdn(i,k)  = fsdn(i,k)*1.e-3
            fsdnc(i,k) = fsdnc(i,k)*1.e-3
            end do
         end do




































   end if



   if (dolw) then

      call get_int_scales(scales)

      call get_aerosol(lchnk, julday, julian, dt, gmt, xtime, m_psp, m_psn, aerosoljp, aerosoljn, &
             m_hybi, paerlev, naer, pint, pcols, pver, pverp, pverr, pverrp, aerosol, scales)




      do i=1,ncol

         lwupcgs(i) = lwups(i)
      end do








      if (trace_gas) then





         call radclwmx(lchnk   ,ncol    ,pcols, pver, pverp ,        & 
                       lwupcgs ,t       ,qm1(1,1,1)       ,o3vmr ,   &
                       pbr     ,pnm     ,pmln    ,piln    ,          &
                       qm1(1,1,in2o)    ,qm1(1,1,ich4)    ,          &
                       qm1(1,1,if11)    ,qm1(1,1,if12)    ,          &
                       cld     ,emis    ,pmxrgn  ,nmxrgn  ,qrl     , &
                       doabsems, abstot, absnxt, emstot,             &
                       flns    ,flnt    ,flnsc   ,flntc   ,flwds   , &
                       flut    ,flutc   ,                            &
                       flup    ,flupc   ,fldn    ,fldnc   ,          &
                       aerosol(:,:,idxVOLC))

      else

         call trcmix(lchnk   ,ncol    ,pcols, pver,  &
                     pmid    ,clat, n2o     ,ch4     ,                     &
                     cfc11   ,cfc12   )
         IF ( wrf_dm_on_monitor() ) THEN
           WRITE(message,*)'CLWRF post_trcmix_values. n2o:', n2o(pcols/2,pver/2), ' ch4:',      &
             ch4(pcols/2,pver/2),' cfc11:', cfc11(pcols/2,pver/2),' cfc12:', cfc12(pcols/2,pver/2) 
           CALL wrf_debug(1, message)
         END IF 


         call radclwmx(lchnk     ,ncol    ,pcols, pver, pverp ,        &
                       lwupcgs   ,t       ,qm1(1,1,1)       ,o3vmr ,   &
                       pbr       ,pnm     ,pmln    ,piln    ,          &
                       n2o       ,ch4     ,cfc11   ,cfc12   ,          &
                       cld       ,emis    ,pmxrgn  ,nmxrgn  ,qrl     , &
                       doabsems, abstot, absnxt, emstot,             &
                       flns      ,flnt    ,flnsc   ,flntc   ,flwds   , &
                       flut      ,flutc   ,                            &
                       flup      ,flupc   ,fldn    ,fldnc   ,          &
                       aerosol(:,:,idxVOLC))

      endif



      do i=1,ncol
         flnt(i)  = flnt(i)*1.e-3
         flut(i)  = flut(i)*1.e-3
         flutc(i) = flutc(i)*1.e-3
         flns(i)  = flns(i)*1.e-3
         flntc(i) = flntc(i)*1.e-3
         flnsc(i) = flnsc(i)*1.e-3
         flwds(i) = flwds(i)*1.e-3
         lwcf(i)  = flutc(i) - flut(i)
      end do


         do k = 1, pverp
            do i = 1, ncol
            flup(i,k)  = flup(i,k)*1.e-3
            flupc(i,k) = flupc(i,k)*1.e-3
            fldn(i,k)  = fldn(i,k)*1.e-3
            fldnc(i,k) = fldnc(i,k)*1.e-3
            end do
         end do













   end if

   return
end subroutine radctl
  subroutine param_cldoptics_calc(ncol, pcols, pver, pverp, pverr, pverrp, ppcnst, &
                                  q, cldn, landfrac, landm,icefrac, &
        pdel,  t, ps, pmid, pint, cicewp, cliqwp, emis, rel, rei, pmxrgn, nmxrgn, snowh )










    implicit none


    integer, intent(in) :: ncol, pcols, pver, pverp, pverr, pverrp, ppcnst
    real(r8), intent(in)  :: q(pcols,pver,ppcnst)     
    real(r8), intent(in)  :: cldn(pcols,pver)        
    real(r8), intent(in)  :: pdel(pcols,pver)        
    real(r8), intent(in)  :: t(pcols,pver)           
    real(r8), intent(in)  :: pmid(pcols,pver)        
    real(r8), intent(in)  :: pint(pcols,pverp)       
    real(r8), intent(in)  :: ps(pcols)               
    real(r8), intent(in)  :: landfrac(pcols)         
    real(r8), intent(in)  :: icefrac(pcols)          
    real(r8), intent(in)  :: landm(pcols)            
    real(r8), intent(in) :: snowh(pcols)         


    real(r8), intent(out) :: cicewp(pcols,pver)      
    real(r8), intent(out) :: cliqwp(pcols,pver)      
    real(r8), intent(out) :: emis  (pcols,pver)      
    real(r8), intent(out) :: rel   (pcols,pver)      
    real(r8), intent(out) :: rei   (pcols,pver)      
    real(r8), intent(out) :: pmxrgn(pcols,pver+1)    
    integer , intent(out) :: nmxrgn(pcols)           


    real(r8) :: cwp   (pcols,pver)      


    real(r8) :: effcld(pcols,pver)                   
    real(r8) :: gicewp(pcols,pver)                   
    real(r8) :: gliqwp(pcols,pver)                   
    real(r8) :: gwp   (pcols,pver)                   
    real(r8) :: hl     (pcols)                       
    real(r8) :: tgicewp(pcols)                       
    real(r8) :: tgliqwp(pcols)                       
    real(r8) :: tgwp   (pcols)                       
    real(r8) :: tpw    (pcols)                       
    real(r8) :: clwpold(pcols,pver)                  
    real(r8) :: ficemr (pcols,pver)                  

    real(r8) :: rgrav                

    integer :: i,k                                   
    integer :: lchnk




    tgicewp(:ncol) = 0.
    tgliqwp(:ncol) = 0.
    do k=1,pver
       do i = 1,ncol
          gicewp(i,k) = q(i,k,ixcldice)*pdel(i,k)/gravmks*1000.0  
          gliqwp(i,k) = q(i,k,ixcldliq)*pdel(i,k)/gravmks*1000.0  

          cicewp(i,k) = gicewp(i,k) / max(0.01_r8,cldn(i,k))                 
          cliqwp(i,k) = gliqwp(i,k) / max(0.01_r8,cldn(i,k))                 

          ficemr(i,k) = q(i,k,ixcldice) /                 &
               max(1.e-10_r8,(q(i,k,ixcldice)+q(i,k,ixcldliq)))
          
          tgicewp(i)  = tgicewp(i) + gicewp(i,k)
          tgliqwp(i)  = tgliqwp(i) + gliqwp(i,k)
       end do
    end do
    tgwp(:ncol) = tgicewp(:ncol) + tgliqwp(:ncol)
    gwp(:ncol,:pver) = gicewp(:ncol,:pver) + gliqwp(:ncol,:pver) 
    cwp(:ncol,:pver) = cicewp(:ncol,:pver) + cliqwp(:ncol,:pver) 


    tpw(:ncol) = 0.0
    rgrav = 1.0/gravmks
    do k=1,pver
       do i=1,ncol
          tpw(i) = tpw(i) + pdel(i,k)*q(i,k,1)*rgrav
       end do
    end do





    call cldefr(lchnk, ncol, pcols, pver, pverp, landfrac, t, rel, rei, ps, pmid, landm, icefrac, snowh)


    call cldems(lchnk, ncol, pcols, pver, pverp, cwp, ficemr, rei, emis)


    do k=1,pver
       do i=1,ncol
          effcld(i,k) = cldn(i,k)*emis(i,k)
       end do
    end do


    call cldovrlap(lchnk, ncol, pcols, pver, pverp, pint, cldn, nmxrgn, pmxrgn)










  end subroutine param_cldoptics_calc

subroutine radabs(lchnk   ,ncol    ,pcols, pver, pverp,   &
   pbr    ,pnm     ,co2em    ,co2eml  ,tplnka  , &
   s2c    ,tcg     ,w        ,h2otr   ,plco2   , &
   plh2o  ,co2t    ,tint     ,tlayr   ,plol    , &
   plos   ,pmln    ,piln     ,ucfc11  ,ucfc12  , &
   un2o0  ,un2o1   ,uch4     ,uco211  ,uco212  , &
   uco213 ,uco221  ,uco222   ,uco223  ,uptype  , &
   bn2o0  ,bn2o1   ,bch4    ,abplnk1  ,abplnk2 , &
   abstot ,absnxt  ,plh2ob  ,wb       , &
   aer_mpp ,aer_trn_ttl)


























































   integer, intent(in) :: lchnk                       
   integer, intent(in) :: ncol                        
   integer, intent(in) :: pcols, pver, pverp

   real(r8), intent(in) :: pbr(pcols,pver)            
   real(r8), intent(in) :: pnm(pcols,pverp)           
   real(r8), intent(in) :: co2em(pcols,pverp)         
   real(r8), intent(in) :: co2eml(pcols,pver)         
   real(r8), intent(in) :: tplnka(pcols,pverp)        
   real(r8), intent(in) :: s2c(pcols,pverp)           
   real(r8), intent(in) :: tcg(pcols,pverp)           
   real(r8), intent(in) :: w(pcols,pverp)             
   real(r8), intent(in) :: h2otr(pcols,pverp)         
   real(r8), intent(in) :: plco2(pcols,pverp)         
   real(r8), intent(in) :: plh2o(pcols,pverp)         
   real(r8), intent(in) :: co2t(pcols,pverp)          
   real(r8), intent(in) :: tint(pcols,pverp)          
   real(r8), intent(in) :: tlayr(pcols,pverp)         
   real(r8), intent(in) :: plol(pcols,pverp)          
   real(r8), intent(in) :: plos(pcols,pverp)          
   real(r8), intent(in) :: pmln(pcols,pver)           
   real(r8), intent(in) :: piln(pcols,pverp)          
   real(r8), intent(in) :: plh2ob(nbands,pcols,pverp) 
                                                      
                                                      
   real(r8), intent(in) :: wb(nbands,pcols,pverp)     
                                                      
                                                      

   real(r8), intent(in) :: aer_mpp(pcols,pverp) 
   real(r8), intent(in) :: aer_trn_ttl(pcols,pverp,pverp,bnd_nbr_LW) 





   real(r8), intent(in) :: ucfc11(pcols,pverp)        
   real(r8), intent(in) :: ucfc12(pcols,pverp)        
   real(r8), intent(in) :: un2o0(pcols,pverp)         
   real(r8), intent(in) :: un2o1(pcols,pverp)         
   real(r8), intent(in) :: uch4(pcols,pverp)          
   real(r8), intent(in) :: uco211(pcols,pverp)        
   real(r8), intent(in) :: uco212(pcols,pverp)        
   real(r8), intent(in) :: uco213(pcols,pverp)        
   real(r8), intent(in) :: uco221(pcols,pverp)        
   real(r8), intent(in) :: uco222(pcols,pverp)        
   real(r8), intent(in) :: uco223(pcols,pverp)        
   real(r8), intent(in) :: uptype(pcols,pverp)        
   real(r8), intent(in) :: bn2o0(pcols,pverp)         
   real(r8), intent(in) :: bn2o1(pcols,pverp)         
   real(r8), intent(in) :: bch4(pcols,pverp)          
   real(r8), intent(in) :: abplnk1(14,pcols,pverp)    
   real(r8), intent(in) :: abplnk2(14,pcols,pverp)    



   real(r8), intent(out) :: abstot(pcols,pverp,pverp) 
   real(r8), intent(out) :: absnxt(pcols,pver,4)      



   integer i                   
   integer k                   
   integer k1                  
   integer k2                  
   integer kn                  
   integer wvl                 

   real(r8) abstrc(pcols)              
   real(r8) bplnk(14,pcols,4)          
   real(r8) pnew(pcols)        
   real(r8) pnewb(nbands)      
                               
                               
   real(r8) u(pcols)           
   real(r8) ub(nbands)         
                               
                               
   real(r8) tbar(pcols,4)      
   real(r8) emm(pcols,4)       
   real(r8) o3emm(pcols,4)     
   real(r8) o3bndi             
   real(r8) temh2o(pcols,4)    
   real(r8) k21                


   real(r8) k22                


   real(r8) uc1(pcols)         
   real(r8) to3h2o(pcols)      
   real(r8) pi                 
   real(r8) sqti(pcols)        
   real(r8) et                 
   real(r8) et2                
   real(r8) et4                
   real(r8) omet               
   real(r8) f1co2              
   real(r8) f2co2(pcols)       
   real(r8) f3co2(pcols)       
   real(r8) t1co2(pcols)       
   real(r8) sqwp               
   real(r8) f1sqwp(pcols)      
   real(r8) oneme              
   real(r8) alphat             
   real(r8) wco2               
   real(r8) posqt              
   real(r8) u7(pcols)          
   real(r8) u8                 
   real(r8) u9                 
   real(r8) u13                
   real(r8) rbeta7(pcols)      
   real(r8) rbeta8             
   real(r8) rbeta9             
   real(r8) rbeta13            
   real(r8) tpatha             
   real(r8) abso(pcols,4)      
   real(r8) dtx(pcols)         
   real(r8) dty(pcols)         
   real(r8) term7(pcols,2)     
   real(r8) term8(pcols,2)     
   real(r8) tr1                
   real(r8) tr10(pcols)        

   real(r8) tr2                
   real(r8) tr5                
   real(r8) tr6                
   real(r8) tr9(pcols)         

   real(r8) sqrtu(pcols)       
   real(r8) fwk(pcols)         
   real(r8) fwku(pcols)        
   real(r8) to3co2(pcols)      
   real(r8) dpnm(pcols)        
   real(r8) pnmsq(pcols,pverp) 
   real(r8) dw(pcols)          
   real(r8) uinpl(pcols,4)     
   real(r8) winpl(pcols,4)     
   real(r8) zinpl(pcols,4)     
   real(r8) pinpl(pcols,4)     
   real(r8) dplh2o(pcols)      
   real(r8) r293               
   real(r8) r250               
   real(r8) r3205              
   real(r8) r300               
   real(r8) rsslp              
   real(r8) r2sslp             
   real(r8) ds2c               
   real(r8)  dplos             
   real(r8) dplol              
   real(r8) tlocal             
   real(r8) beta               

   real(r8) rphat              
   real(r8) tcrfac             
   real(r8) tmp1               
   real(r8) u1                 
   real(r8) realnu             
   real(r8) tmp2               
   real(r8) u2                 
   real(r8) rsqti              
   real(r8) tpath              
   real(r8) tmp3               
   real(r8) rdpnmsq            
   real(r8) rdpnm              
   real(r8) p1                 
   real(r8) p2                 
   real(r8) dtym10             
   real(r8) dplco2             
   real(r8) te                 
   real(r8) denom              
   real(r8) th2o(pcols)        
   real(r8) tco2(pcols)        
   real(r8) to3(pcols)         



   real(r8) trab2(pcols)       
   real(r8) absbnd             
   real(r8) dbvtit(pcols,pverp)
   real(r8) dbvtly(pcols,pver) 














   real(r8) fa               
   real(r8) a_star           
   real(r8) l_star           
   real(r8) c_star           

   real(r8) te1              
   real(r8) te2              
   real(r8) te3              
   real(r8) te4              
   real(r8) te5              

   real(r8) log_u            
   real(r8) log_uc           
   real(r8) log_p            
   real(r8) t_p              
   real(r8) t_e              

   integer iu                
   integer iu1               
   integer iuc               
   integer iuc1              
   integer ip                
   integer ip1               
   integer itp               
   integer itp1              
   integer ite               
   integer ite1              
   integer irh               
   integer irh1              

   real(r8) dvar             
   real(r8) uvar             
   real(r8) uscl             

   real(r8) wu               
   real(r8) wu1              
   real(r8) wuc              
   real(r8) wuc1             
   real(r8) wp               
   real(r8) wp1              
   real(r8) wtp              
   real(r8) wtp1             
   real(r8) wte              
   real(r8) wte1             
   real(r8) wrh              
   real(r8) wrh1             

   real(r8) w_0_0_           
   real(r8) w_0_1_           
   real(r8) w_1_0_           
   real(r8) w_1_1_           

   real(r8) w_0_00           
   real(r8) w_0_01           
   real(r8) w_0_10           
   real(r8) w_0_11           
   real(r8) w_1_00           
   real(r8) w_1_01           
   real(r8) w_1_10           
   real(r8) w_1_11           

   real(r8) w00_00           
   real(r8) w00_01           
   real(r8) w00_10           
   real(r8) w00_11           
   real(r8) w01_00           
   real(r8) w01_01           
   real(r8) w01_10           
   real(r8) w01_11           
   real(r8) w10_00           
   real(r8) w10_01           
   real(r8) w10_10           
   real(r8) w10_11           
   real(r8) w11_00           
   real(r8) w11_01           
   real(r8) w11_10           
   real(r8) w11_11           

   integer ib                
                             
                             


   real(r8) pch2o            
   real(r8) fch2o            
   real(r8) uch2o            

   real(r8) fdif             

   real(r8) sslp_mks         
   real(r8) esx              
   real(r8) qsx              
   real(r8) pnew_mks         
   real(r8) q_path           
   real(r8) rh_path          
   real(r8) omeps            

   integer  iest             

      integer bnd_idx        
      real(r8) aer_pth_dlt   
      real(r8) aer_pth_ngh(pcols)
                             
      real(r8) odap_aer_ttl  
      real(r8) aer_trn_ngh(pcols,bnd_nbr_LW) 
                             
                             



   real(r8) dbvt,t             

   dbvt(t)=(-2.8911366682e-4+(2.3771251896e-6+1.1305188929e-10*t)*t)/ &
      (1.0+(-6.1364820707e-3+1.5550319767e-5*t)*t)






   do k2=1,ntoplw-1
      do k1=1,ntoplw-1
         abstot(:,k1,k2) = inf    
      end do
   end do
   do k2=1,4
      do k1=1,ntoplw-1
         absnxt(:,k1,k2) = inf    
      end do
   end do

   do k=ntoplw,pverp
      abstot(:,k,k) = inf         
   end do

   do k=ntoplw,pver
      do i=1,ncol
         dbvtly(i,k) = dbvt(tlayr(i,k+1))
         dbvtit(i,k) = dbvt(tint(i,k))
      end do
   end do
   do i=1,ncol
      dbvtit(i,pverp) = dbvt(tint(i,pverp))
   end do

   r293    = 1./293.
   r250    = 1./250.
   r3205   = 1./.3205
   r300    = 1./300.
   rsslp   = 1./sslp
   r2sslp  = 1./(2.*sslp)



   fdif       = 1.66
   sslp_mks   = sslp / 10.0
   omeps      = 1.0 - epsilo
















   do k=ntoplw,pverp
      do i=1,ncol
         pnmsq(i,k) = pnm(i,k)**2
         dtx(i) = tplnka(i,k) - 250.
      end do
   end do



   do k1=pverp,ntoplw,-1
      do k2=pverp,ntoplw,-1
         if (k1 == k2) cycle
         do i=1,ncol
            dplh2o(i) = plh2o(i,k1) - plh2o(i,k2)
            u(i)      = abs(dplh2o(i))
            sqrtu(i)  = sqrt(u(i))
            ds2c      = abs(s2c(i,k1) - s2c(i,k2))
            dw(i)     = abs(w(i,k1) - w(i,k2))
            uc1(i)    = (ds2c + 1.7e-3*u(i))*(1. +  2.*ds2c)/(1. + 15.*ds2c)
            pch2o     = ds2c
            pnew(i)   = u(i)/dw(i)
            pnew_mks  = pnew(i) * sslp_mks



            tpatha = abs(tcg(i,k1) - tcg(i,k2))/dw(i)
            t_p = min(max(tpatha, min_tp_h2o), max_tp_h2o)
            iest = floor(t_p) - min_tp_h2o
            esx = estblh2o(iest) + (estblh2o(iest+1)-estblh2o(iest)) * &
                 (t_p - min_tp_h2o - iest)
            qsx = epsilo * esx / (pnew_mks - omeps * esx)



            q_path = dw(i) / abs(pnm(i,k1) - pnm(i,k2)) / rga










            ub(1) = abs(plh2ob(1,i,k1) - plh2ob(1,i,k2)) / psi(t_p,1)
            ub(2) = abs(plh2ob(2,i,k1) - plh2ob(2,i,k2)) / psi(t_p,2)
            
            pnewb(1) = ub(1) / abs(wb(1,i,k1) - wb(1,i,k2)) * phi(t_p,1)
            pnewb(2) = ub(2) / abs(wb(2,i,k1) - wb(2,i,k2)) * phi(t_p,2)

            dtx(i)      = tplnka(i,k2) - 250.
            dty(i)      = tpatha       - 250.

            fwk(i)  = fwcoef + fwc1/(1. + fwc2*u(i))
            fwku(i) = fwk(i)*u(i)






















            te1  = tplnka(i,k2)
            te2  = te1 * te1
            te3  = te2 * te1
            te4  = te3 * te1
            te5  = te4 * te1




            dvar = (t_p - min_tp_h2o) / dtp_h2o
            itp = min(max(int(aint(dvar,r8)) + 1, 1), n_tp - 1)
            itp1 = itp + 1
            wtp = dvar - floor(dvar)
            wtp1 = 1.0 - wtp
            
            t_e = min(max(tplnka(i,k2)-t_p, min_te_h2o), max_te_h2o)
            dvar = (t_e - min_te_h2o) / dte_h2o
            ite = min(max(int(aint(dvar,r8)) + 1, 1), n_te - 1)
            ite1 = ite + 1
            wte = dvar - floor(dvar)
            wte1 = 1.0 - wte
            
            rh_path = min(max(q_path / qsx, min_rh_h2o), max_rh_h2o)
            dvar = (rh_path - min_rh_h2o) / drh_h2o
            irh = min(max(int(aint(dvar,r8)) + 1, 1), n_rh - 1)
            irh1 = irh + 1
            wrh = dvar - floor(dvar)
            wrh1 = 1.0 - wrh

            w_0_0_ = wtp  * wte
            w_0_1_ = wtp  * wte1
            w_1_0_ = wtp1 * wte 
            w_1_1_ = wtp1 * wte1
            
            w_0_00 = w_0_0_ * wrh
            w_0_01 = w_0_0_ * wrh1
            w_0_10 = w_0_1_ * wrh
            w_0_11 = w_0_1_ * wrh1
            w_1_00 = w_1_0_ * wrh
            w_1_01 = w_1_0_ * wrh1
            w_1_10 = w_1_1_ * wrh
            w_1_11 = w_1_1_ * wrh1




















































            fch2o = fh2oself(t_p) 
            uch2o = (pch2o * epsilo) / (q_path * fch2o)




            ib = 1

            uvar = ub(ib) * fdif
            log_u  = min(log10(max(uvar, min_u_h2o)), max_lu_h2o)
            dvar = (log_u - min_lu_h2o) / dlu_h2o
            iu = min(max(int(aint(dvar,r8)) + 1, 1), n_u - 1)
            iu1 = iu + 1
            wu = dvar - floor(dvar)
            wu1 = 1.0 - wu
            
            log_p  = min(log10(max(pnewb(ib), min_p_h2o)), max_lp_h2o)
            dvar = (log_p - min_lp_h2o) / dlp_h2o
            ip = min(max(int(aint(dvar,r8)) + 1, 1), n_p - 1)
            ip1 = ip + 1
            wp = dvar - floor(dvar)
            wp1 = 1.0 - wp
         
            w00_00 = wp  * w_0_00 
            w00_01 = wp  * w_0_01 
            w00_10 = wp  * w_0_10 
            w00_11 = wp  * w_0_11 
            w01_00 = wp  * w_1_00 
            w01_01 = wp  * w_1_01 
            w01_10 = wp  * w_1_10 
            w01_11 = wp  * w_1_11 
            w10_00 = wp1 * w_0_00 
            w10_01 = wp1 * w_0_01 
            w10_10 = wp1 * w_0_10 
            w10_11 = wp1 * w_0_11 
            w11_00 = wp1 * w_1_00 
            w11_01 = wp1 * w_1_01 
            w11_10 = wp1 * w_1_10 
            w11_11 = wp1 * w_1_11 



            fa = fat(1,ib) + &
                 fat(2,ib) * te1 + &
                 fat(3,ib) * te2 + &
                 fat(4,ib) * te3 + &
                 fat(5,ib) * te4 + &
                 fat(6,ib) * te5

            a_star = &
                 ah2onw(ip , itp , iu , ite , irh ) * w11_11 * wu1 + &
                 ah2onw(ip , itp , iu , ite , irh1) * w11_10 * wu1 + &
                 ah2onw(ip , itp , iu , ite1, irh ) * w11_01 * wu1 + &
                 ah2onw(ip , itp , iu , ite1, irh1) * w11_00 * wu1 + &
                 ah2onw(ip , itp , iu1, ite , irh ) * w11_11 * wu  + &
                 ah2onw(ip , itp , iu1, ite , irh1) * w11_10 * wu  + &
                 ah2onw(ip , itp , iu1, ite1, irh ) * w11_01 * wu  + &
                 ah2onw(ip , itp , iu1, ite1, irh1) * w11_00 * wu  + &
                 ah2onw(ip , itp1, iu , ite , irh ) * w10_11 * wu1 + &
                 ah2onw(ip , itp1, iu , ite , irh1) * w10_10 * wu1 + &
                 ah2onw(ip , itp1, iu , ite1, irh ) * w10_01 * wu1 + &
                 ah2onw(ip , itp1, iu , ite1, irh1) * w10_00 * wu1 + &
                 ah2onw(ip , itp1, iu1, ite , irh ) * w10_11 * wu  + &
                 ah2onw(ip , itp1, iu1, ite , irh1) * w10_10 * wu  + &
                 ah2onw(ip , itp1, iu1, ite1, irh ) * w10_01 * wu  + &
                 ah2onw(ip , itp1, iu1, ite1, irh1) * w10_00 * wu  + &
                 ah2onw(ip1, itp , iu , ite , irh ) * w01_11 * wu1 + &
                 ah2onw(ip1, itp , iu , ite , irh1) * w01_10 * wu1 + &
                 ah2onw(ip1, itp , iu , ite1, irh ) * w01_01 * wu1 + &
                 ah2onw(ip1, itp , iu , ite1, irh1) * w01_00 * wu1 + &
                 ah2onw(ip1, itp , iu1, ite , irh ) * w01_11 * wu  + &
                 ah2onw(ip1, itp , iu1, ite , irh1) * w01_10 * wu  + &
                 ah2onw(ip1, itp , iu1, ite1, irh ) * w01_01 * wu  + &
                 ah2onw(ip1, itp , iu1, ite1, irh1) * w01_00 * wu  + &
                 ah2onw(ip1, itp1, iu , ite , irh ) * w00_11 * wu1 + &
                 ah2onw(ip1, itp1, iu , ite , irh1) * w00_10 * wu1 + &
                 ah2onw(ip1, itp1, iu , ite1, irh ) * w00_01 * wu1 + &
                 ah2onw(ip1, itp1, iu , ite1, irh1) * w00_00 * wu1 + &
                 ah2onw(ip1, itp1, iu1, ite , irh ) * w00_11 * wu  + &
                 ah2onw(ip1, itp1, iu1, ite , irh1) * w00_10 * wu  + &
                 ah2onw(ip1, itp1, iu1, ite1, irh ) * w00_01 * wu  + &
                 ah2onw(ip1, itp1, iu1, ite1, irh1) * w00_00 * wu 
            abso(i,ib) = min(max(fa * (1.0 - (1.0 - a_star) * &
                                 aer_trn_ttl(i,k1,k2,ib)), &
                             0.0_r8), 1.0_r8)



            if (uvar < min_u_h2o) then
               uscl = uvar / min_u_h2o
               abso(i,ib) = abso(i,ib) * uscl
            endif
                         



            ib = 2

            uvar = ub(ib) * fdif
            log_u  = min(log10(max(uvar, min_u_h2o)), max_lu_h2o)
            dvar = (log_u - min_lu_h2o) / dlu_h2o
            iu = min(max(int(aint(dvar,r8)) + 1, 1), n_u - 1)
            iu1 = iu + 1
            wu = dvar - floor(dvar)
            wu1 = 1.0 - wu
            
            log_p  = min(log10(max(pnewb(ib), min_p_h2o)), max_lp_h2o)
            dvar = (log_p - min_lp_h2o) / dlp_h2o
            ip = min(max(int(aint(dvar,r8)) + 1, 1), n_p - 1)
            ip1 = ip + 1
            wp = dvar - floor(dvar)
            wp1 = 1.0 - wp
         
            w00_00 = wp  * w_0_00 
            w00_01 = wp  * w_0_01 
            w00_10 = wp  * w_0_10 
            w00_11 = wp  * w_0_11 
            w01_00 = wp  * w_1_00 
            w01_01 = wp  * w_1_01 
            w01_10 = wp  * w_1_10 
            w01_11 = wp  * w_1_11 
            w10_00 = wp1 * w_0_00 
            w10_01 = wp1 * w_0_01 
            w10_10 = wp1 * w_0_10 
            w10_11 = wp1 * w_0_11 
            w11_00 = wp1 * w_1_00 
            w11_01 = wp1 * w_1_01 
            w11_10 = wp1 * w_1_10 
            w11_11 = wp1 * w_1_11 

            log_uc  = min(log10(max(uch2o * fdif, min_u_h2o)), max_lu_h2o)
            dvar = (log_uc - min_lu_h2o) / dlu_h2o
            iuc = min(max(int(aint(dvar,r8)) + 1, 1), n_u - 1)
            iuc1 = iuc + 1
            wuc = dvar - floor(dvar)
            wuc1 = 1.0 - wuc



            fa = fat(1,ib) + &
                 fat(2,ib) * te1 + &
                 fat(3,ib) * te2 + &
                 fat(4,ib) * te3 + &
                 fat(5,ib) * te4 + &
                 fat(6,ib) * te5

            l_star = &
                 ln_ah2ow(ip , itp , iu , ite , irh ) * w11_11 * wu1 + &
                 ln_ah2ow(ip , itp , iu , ite , irh1) * w11_10 * wu1 + &
                 ln_ah2ow(ip , itp , iu , ite1, irh ) * w11_01 * wu1 + &
                 ln_ah2ow(ip , itp , iu , ite1, irh1) * w11_00 * wu1 + &
                 ln_ah2ow(ip , itp , iu1, ite , irh ) * w11_11 * wu  + &
                 ln_ah2ow(ip , itp , iu1, ite , irh1) * w11_10 * wu  + &
                 ln_ah2ow(ip , itp , iu1, ite1, irh ) * w11_01 * wu  + &
                 ln_ah2ow(ip , itp , iu1, ite1, irh1) * w11_00 * wu  + &
                 ln_ah2ow(ip , itp1, iu , ite , irh ) * w10_11 * wu1 + &
                 ln_ah2ow(ip , itp1, iu , ite , irh1) * w10_10 * wu1 + &
                 ln_ah2ow(ip , itp1, iu , ite1, irh ) * w10_01 * wu1 + &
                 ln_ah2ow(ip , itp1, iu , ite1, irh1) * w10_00 * wu1 + &
                 ln_ah2ow(ip , itp1, iu1, ite , irh ) * w10_11 * wu  + &
                 ln_ah2ow(ip , itp1, iu1, ite , irh1) * w10_10 * wu  + &
                 ln_ah2ow(ip , itp1, iu1, ite1, irh ) * w10_01 * wu  + &
                 ln_ah2ow(ip , itp1, iu1, ite1, irh1) * w10_00 * wu  + &
                 ln_ah2ow(ip1, itp , iu , ite , irh ) * w01_11 * wu1 + &
                 ln_ah2ow(ip1, itp , iu , ite , irh1) * w01_10 * wu1 + &
                 ln_ah2ow(ip1, itp , iu , ite1, irh ) * w01_01 * wu1 + &
                 ln_ah2ow(ip1, itp , iu , ite1, irh1) * w01_00 * wu1 + &
                 ln_ah2ow(ip1, itp , iu1, ite , irh ) * w01_11 * wu  + &
                 ln_ah2ow(ip1, itp , iu1, ite , irh1) * w01_10 * wu  + &
                 ln_ah2ow(ip1, itp , iu1, ite1, irh ) * w01_01 * wu  + &
                 ln_ah2ow(ip1, itp , iu1, ite1, irh1) * w01_00 * wu  + &
                 ln_ah2ow(ip1, itp1, iu , ite , irh ) * w00_11 * wu1 + &
                 ln_ah2ow(ip1, itp1, iu , ite , irh1) * w00_10 * wu1 + &
                 ln_ah2ow(ip1, itp1, iu , ite1, irh ) * w00_01 * wu1 + &
                 ln_ah2ow(ip1, itp1, iu , ite1, irh1) * w00_00 * wu1 + &
                 ln_ah2ow(ip1, itp1, iu1, ite , irh ) * w00_11 * wu  + &
                 ln_ah2ow(ip1, itp1, iu1, ite , irh1) * w00_10 * wu  + &
                 ln_ah2ow(ip1, itp1, iu1, ite1, irh ) * w00_01 * wu  + &
                 ln_ah2ow(ip1, itp1, iu1, ite1, irh1) * w00_00 * wu 

            c_star = &
                 cn_ah2ow(ip , itp , iuc , ite , irh ) * w11_11 * wuc1 + &
                 cn_ah2ow(ip , itp , iuc , ite , irh1) * w11_10 * wuc1 + &
                 cn_ah2ow(ip , itp , iuc , ite1, irh ) * w11_01 * wuc1 + &
                 cn_ah2ow(ip , itp , iuc , ite1, irh1) * w11_00 * wuc1 + &
                 cn_ah2ow(ip , itp , iuc1, ite , irh ) * w11_11 * wuc  + &
                 cn_ah2ow(ip , itp , iuc1, ite , irh1) * w11_10 * wuc  + &
                 cn_ah2ow(ip , itp , iuc1, ite1, irh ) * w11_01 * wuc  + &
                 cn_ah2ow(ip , itp , iuc1, ite1, irh1) * w11_00 * wuc  + &
                 cn_ah2ow(ip , itp1, iuc , ite , irh ) * w10_11 * wuc1 + &
                 cn_ah2ow(ip , itp1, iuc , ite , irh1) * w10_10 * wuc1 + &
                 cn_ah2ow(ip , itp1, iuc , ite1, irh ) * w10_01 * wuc1 + &
                 cn_ah2ow(ip , itp1, iuc , ite1, irh1) * w10_00 * wuc1 + &
                 cn_ah2ow(ip , itp1, iuc1, ite , irh ) * w10_11 * wuc  + &
                 cn_ah2ow(ip , itp1, iuc1, ite , irh1) * w10_10 * wuc  + &
                 cn_ah2ow(ip , itp1, iuc1, ite1, irh ) * w10_01 * wuc  + &
                 cn_ah2ow(ip , itp1, iuc1, ite1, irh1) * w10_00 * wuc  + &
                 cn_ah2ow(ip1, itp , iuc , ite , irh ) * w01_11 * wuc1 + &
                 cn_ah2ow(ip1, itp , iuc , ite , irh1) * w01_10 * wuc1 + &
                 cn_ah2ow(ip1, itp , iuc , ite1, irh ) * w01_01 * wuc1 + &
                 cn_ah2ow(ip1, itp , iuc , ite1, irh1) * w01_00 * wuc1 + &
                 cn_ah2ow(ip1, itp , iuc1, ite , irh ) * w01_11 * wuc  + &
                 cn_ah2ow(ip1, itp , iuc1, ite , irh1) * w01_10 * wuc  + &
                 cn_ah2ow(ip1, itp , iuc1, ite1, irh ) * w01_01 * wuc  + &
                 cn_ah2ow(ip1, itp , iuc1, ite1, irh1) * w01_00 * wuc  + &
                 cn_ah2ow(ip1, itp1, iuc , ite , irh ) * w00_11 * wuc1 + &
                 cn_ah2ow(ip1, itp1, iuc , ite , irh1) * w00_10 * wuc1 + &
                 cn_ah2ow(ip1, itp1, iuc , ite1, irh ) * w00_01 * wuc1 + &
                 cn_ah2ow(ip1, itp1, iuc , ite1, irh1) * w00_00 * wuc1 + &
                 cn_ah2ow(ip1, itp1, iuc1, ite , irh ) * w00_11 * wuc  + &
                 cn_ah2ow(ip1, itp1, iuc1, ite , irh1) * w00_10 * wuc  + &
                 cn_ah2ow(ip1, itp1, iuc1, ite1, irh ) * w00_01 * wuc  + &
                 cn_ah2ow(ip1, itp1, iuc1, ite1, irh1) * w00_00 * wuc 
            abso(i,ib) = min(max(fa * (1.0 - l_star * c_star * &
                                 aer_trn_ttl(i,k1,k2,ib)), &
                             0.0_r8), 1.0_r8) 



            if (uvar < min_u_h2o) then
               uscl = uvar / min_u_h2o
               abso(i,ib) = abso(i,ib) * uscl
            endif

         end do



         do i=1,ncol
            term7(i,1) = coefj(1,1) + coefj(2,1)*dty(i)*(1. + c16*dty(i))
            term8(i,1) = coefk(1,1) + coefk(2,1)*dty(i)*(1. + c17*dty(i))
            term7(i,2) = coefj(1,2) + coefj(2,2)*dty(i)*(1. + c26*dty(i))
            term8(i,2) = coefk(1,2) + coefk(2,2)*dty(i)*(1. + c27*dty(i))
         end do



         do i=1,ncol
            k21    = term7(i,1) + term8(i,1)/ &
               (1. + (c30 + c31*(dty(i)-10.)*(dty(i)-10.))*sqrtu(i))
            k22    = term7(i,2) + term8(i,2)/ &
               (1. + (c28 + c29*(dty(i)-10.))*sqrtu(i))
            tr1    = exp(-(k21*(sqrtu(i) + fc1*fwku(i))))
            tr2    = exp(-(k22*(sqrtu(i) + fc1*fwku(i))))
            tr1=tr1*aer_trn_ttl(i,k1,k2,idx_LW_0650_0800) 

            tr2=tr2*aer_trn_ttl(i,k1,k2,idx_LW_0500_0650)

            tr5    = exp(-((coefh(1,3) + coefh(2,3)*dtx(i))*uc1(i)))
            tr6    = exp(-((coefh(1,4) + coefh(2,4)*dtx(i))*uc1(i)))
            tr9(i)   = tr1*tr5
            tr10(i)  = tr2*tr6
            th2o(i) = tr10(i)
            trab2(i) = 0.65*tr9(i) + 0.35*tr10(i)
         end do
         if (k2 < k1) then
            do i=1,ncol
               to3h2o(i) = h2otr(i,k1)/h2otr(i,k2)
            end do
         else
            do i=1,ncol
               to3h2o(i) = h2otr(i,k2)/h2otr(i,k1)
            end do
         end if



         do i=1,ncol
            dpnm(i)  = pnm(i,k1) - pnm(i,k2)
            to3co2(i) = (pnm(i,k1)*co2t(i,k1) - pnm(i,k2)*co2t(i,k2))/dpnm(i)
            te       = (to3co2(i)*r293)**.7
            dplos    = plos(i,k1) - plos(i,k2)
            dplol    = plol(i,k1) - plol(i,k2)
            u1       = 18.29*abs(dplos)/te
            u2       = .5649*abs(dplos)/te
            rphat    = dplol/dplos
            tlocal   = tint(i,k2)
            tcrfac   = sqrt(tlocal*r250)*te
            beta     = r3205*(rphat + dpfo3*tcrfac)
            realnu   = te/beta
            tmp1     = u1/sqrt(4. + u1*(1. + realnu))
            tmp2     = u2/sqrt(4. + u2*(1. + realnu))
            o3bndi    = 74.*te*log(1. + tmp1 + tmp2)
            abso(i,3) = o3bndi*to3h2o(i)*dbvtit(i,k2)
            to3(i)   = 1.0/(1. + 0.1*tmp1 + 0.1*tmp2)
         end do



         do i=1,ncol
            sqwp      = sqrt(abs(plco2(i,k1) - plco2(i,k2)))
            et        = exp(-480./to3co2(i))
            sqti(i)   = sqrt(to3co2(i))
            rsqti     = 1./sqti(i)
            et2       = et*et
            et4       = et2*et2
            omet      = 1. - 1.5*et2
            f1co2     = 899.70*omet*(1. + 1.94774*et + 4.73486*et2)*rsqti
            f1sqwp(i) = f1co2*sqwp
            t1co2(i)  = 1./(1. + (245.18*omet*sqwp*rsqti))
            oneme     = 1. - et2
            alphat    = oneme**3*rsqti
            pi        = abs(dpnm(i))
            wco2      =  2.5221*co2vmr*pi*rga
            u7(i)     =  4.9411e4*alphat*et2*wco2
            u8        =  3.9744e4*alphat*et4*wco2
            u9        =  1.0447e5*alphat*et4*et2*wco2
            u13       = 2.8388e3*alphat*et4*wco2
            tpath     = to3co2(i)
            tlocal    = tint(i,k2)
            tcrfac    = sqrt(tlocal*r250*tpath*r300)
            posqt     = ((pnm(i,k2) + pnm(i,k1))*r2sslp + dpfco2*tcrfac)*rsqti
            rbeta7(i) = 1./(5.3228*posqt)
            rbeta8    = 1./(10.6576*posqt)
            rbeta9    = rbeta7(i)
            rbeta13   = rbeta9
            f2co2(i)  = (u7(i)/sqrt(4. + u7(i)*(1. + rbeta7(i)))) + &
               (u8   /sqrt(4. + u8*(1. + rbeta8))) + &
               (u9   /sqrt(4. + u9*(1. + rbeta9)))
            f3co2(i)  = u13/sqrt(4. + u13*(1. + rbeta13))
         end do
         if (k2 >= k1) then
            do i=1,ncol
               sqti(i) = sqrt(tlayr(i,k2))
            end do
         end if

         do i=1,ncol
            tmp1      = log(1. + f1sqwp(i))
            tmp2      = log(1. + f2co2(i))
            tmp3      = log(1. + f3co2(i))
            absbnd    = (tmp1 + 2.*t1co2(i)*tmp2 + 2.*tmp3)*sqti(i)
            abso(i,4) = trab2(i)*co2em(i,k2)*absbnd
            tco2(i)   = 1./(1.0+10.0*(u7(i)/sqrt(4. + u7(i)*(1. + rbeta7(i)))))
         end do



         call trcab( lchnk   ,ncol    ,pcols, pverp,                   &
            k1      ,k2      ,ucfc11  ,ucfc12  ,un2o0   , &
            un2o1   ,uch4    ,uco211  ,uco212  ,uco213  , &
            uco221  ,uco222  ,uco223  ,bn2o0   ,bn2o1   , &
            bch4    ,to3co2  ,pnm     ,dw      ,pnew    , &
            s2c     ,uptype  ,u       ,abplnk1 ,tco2    , &
            th2o    ,to3     ,abstrc  , &
            aer_trn_ttl)



         do i=1,ncol
            abstot(i,k1,k2) = abso(i,1) + abso(i,2) + &
               abso(i,3) + abso(i,4) + abstrc(i)
         end do
      end do 
   end do 


















   do k2=pver,ntoplw,-1
      do i=1,ncol
         tbar(i,1)   = 0.5*(tint(i,k2+1) + tlayr(i,k2+1))
         emm(i,1)    = 0.5*(co2em(i,k2+1) + co2eml(i,k2))
         tbar(i,2)   = 0.5*(tlayr(i,k2+1) + tint(i,k2))
         emm(i,2)    = 0.5*(co2em(i,k2) + co2eml(i,k2))
         tbar(i,3)   = 0.5*(tbar(i,2) + tbar(i,1))
         emm(i,3)    = emm(i,1)
         tbar(i,4)   = tbar(i,3)
         emm(i,4)    = emm(i,2)
         o3emm(i,1)  = 0.5*(dbvtit(i,k2+1) + dbvtly(i,k2))
         o3emm(i,2)  = 0.5*(dbvtit(i,k2) + dbvtly(i,k2))
         o3emm(i,3)  = o3emm(i,1)
         o3emm(i,4)  = o3emm(i,2)
         temh2o(i,1) = tbar(i,1)
         temh2o(i,2) = tbar(i,2)
         temh2o(i,3) = tbar(i,1)
         temh2o(i,4) = tbar(i,2)
         dpnm(i)     = pnm(i,k2+1) - pnm(i,k2)
      end do



      do wvl = 1,14
         do i = 1,ncol
            bplnk(wvl,i,1) = 0.5*(abplnk1(wvl,i,k2+1) + abplnk2(wvl,i,k2))
            bplnk(wvl,i,2) = 0.5*(abplnk1(wvl,i,k2) + abplnk2(wvl,i,k2))
            bplnk(wvl,i,3) = bplnk(wvl,i,1)
            bplnk(wvl,i,4) = bplnk(wvl,i,2)
         end do
      end do
      
      do i=1,ncol
         rdpnmsq    = 1./(pnmsq(i,k2+1) - pnmsq(i,k2))
         rdpnm      = 1./dpnm(i)
         p1         = .5*(pbr(i,k2) + pnm(i,k2+1))
         p2         = .5*(pbr(i,k2) + pnm(i,k2  ))
         uinpl(i,1) =  (pnmsq(i,k2+1) - p1**2)*rdpnmsq
         uinpl(i,2) = -(pnmsq(i,k2  ) - p2**2)*rdpnmsq
         uinpl(i,3) = -(pnmsq(i,k2  ) - p1**2)*rdpnmsq
         uinpl(i,4) =  (pnmsq(i,k2+1) - p2**2)*rdpnmsq
         winpl(i,1) = (.5*( pnm(i,k2+1) - pbr(i,k2)))*rdpnm
         winpl(i,2) = (.5*(-pnm(i,k2  ) + pbr(i,k2)))*rdpnm
         winpl(i,3) = (.5*( pnm(i,k2+1) + pbr(i,k2)) - pnm(i,k2  ))*rdpnm
         winpl(i,4) = (.5*(-pnm(i,k2  ) - pbr(i,k2)) + pnm(i,k2+1))*rdpnm
         tmp1       = 1./(piln(i,k2+1) - piln(i,k2))
         tmp2       = piln(i,k2+1) - pmln(i,k2)
         tmp3       = piln(i,k2  ) - pmln(i,k2)
         zinpl(i,1) = (.5*tmp2          )*tmp1
         zinpl(i,2) = (        - .5*tmp3)*tmp1
         zinpl(i,3) = (.5*tmp2 -    tmp3)*tmp1
         zinpl(i,4) = (   tmp2 - .5*tmp3)*tmp1
         pinpl(i,1) = 0.5*(p1 + pnm(i,k2+1))
         pinpl(i,2) = 0.5*(p2 + pnm(i,k2  ))
         pinpl(i,3) = 0.5*(p1 + pnm(i,k2  ))
         pinpl(i,4) = 0.5*(p2 + pnm(i,k2+1))
         if(strat_volcanic) then
           aer_pth_ngh(i) = abs(aer_mpp(i,k2)-aer_mpp(i,k2+1))
         endif
      end do
      do kn=1,4
         do i=1,ncol
            u(i)     = uinpl(i,kn)*abs(plh2o(i,k2) - plh2o(i,k2+1))
            sqrtu(i) = sqrt(u(i))
            dw(i)    = abs(w(i,k2) - w(i,k2+1))
            pnew(i)  = u(i)/(winpl(i,kn)*dw(i))
            pnew_mks  = pnew(i) * sslp_mks
            t_p = min(max(tbar(i,kn), min_tp_h2o), max_tp_h2o)
            iest = floor(t_p) - min_tp_h2o
            esx = estblh2o(iest) + (estblh2o(iest+1)-estblh2o(iest)) * &
                 (t_p - min_tp_h2o - iest)
            qsx = epsilo * esx / (pnew_mks - omeps * esx)
            q_path = dw(i) / ABS(dpnm(i)) / rga
            
            ds2c     = abs(s2c(i,k2) - s2c(i,k2+1))
            uc1(i)   = uinpl(i,kn)*ds2c
            pch2o    = uc1(i)
            uc1(i)   = (uc1(i) + 1.7e-3*u(i))*(1. +  2.*uc1(i))/(1. + 15.*uc1(i))
            dtx(i)      = temh2o(i,kn) - 250.
            dty(i)      = tbar(i,kn) - 250.
            
            fwk(i)    = fwcoef + fwc1/(1. + fwc2*u(i))
            fwku(i)   = fwk(i)*u(i)

            if(strat_volcanic) then
              aer_pth_dlt=uinpl(i,kn)*aer_pth_ngh(i)
  
              do bnd_idx=1,bnd_nbr_LW
                 odap_aer_ttl=abs_cff_mss_aer(bnd_idx) * aer_pth_dlt 
                 aer_trn_ngh(i,bnd_idx)=exp(-fdif * odap_aer_ttl)
              end do
            else
              aer_trn_ngh(i,:) = 1.0
            endif























            te1  = temh2o(i,kn)
            te2  = te1 * te1
            te3  = te2 * te1
            te4  = te3 * te1
            te5  = te4 * te1







            uvar = u(i)*fdif
            log_u  = min(log10(max(uvar, min_u_h2o)), max_lu_h2o)
            dvar = (log_u - min_lu_h2o) / dlu_h2o
            iu = min(max(int(aint(dvar,r8)) + 1, 1), n_u - 1)
            iu1 = iu + 1
            wu = dvar - floor(dvar)
            wu1 = 1.0 - wu
            
            log_p  = min(log10(max(pnew(i), min_p_h2o)), max_lp_h2o)
            dvar = (log_p - min_lp_h2o) / dlp_h2o
            ip = min(max(int(aint(dvar,r8)) + 1, 1), n_p - 1)
            ip1 = ip + 1
            wp = dvar - floor(dvar)
            wp1 = 1.0 - wp
            
            dvar = (t_p - min_tp_h2o) / dtp_h2o
            itp = min(max(int(aint(dvar,r8)) + 1, 1), n_tp - 1)
            itp1 = itp + 1
            wtp = dvar - floor(dvar)
            wtp1 = 1.0 - wtp
            
            t_e = min(max(temh2o(i,kn)-t_p,min_te_h2o),max_te_h2o)
            dvar = (t_e - min_te_h2o) / dte_h2o
            ite = min(max(int(aint(dvar,r8)) + 1, 1), n_te - 1)
            ite1 = ite + 1
            wte = dvar - floor(dvar)
            wte1 = 1.0 - wte
            
            rh_path = min(max(q_path / qsx, min_rh_h2o), max_rh_h2o)
            dvar = (rh_path - min_rh_h2o) / drh_h2o
            irh = min(max(int(aint(dvar,r8)) + 1, 1), n_rh - 1)
            irh1 = irh + 1
            wrh = dvar - floor(dvar)
            wrh1 = 1.0 - wrh
            
            w_0_0_ = wtp  * wte
            w_0_1_ = wtp  * wte1
            w_1_0_ = wtp1 * wte 
            w_1_1_ = wtp1 * wte1
            
            w_0_00 = w_0_0_ * wrh
            w_0_01 = w_0_0_ * wrh1
            w_0_10 = w_0_1_ * wrh
            w_0_11 = w_0_1_ * wrh1
            w_1_00 = w_1_0_ * wrh
            w_1_01 = w_1_0_ * wrh1
            w_1_10 = w_1_1_ * wrh
            w_1_11 = w_1_1_ * wrh1
            
            w00_00 = wp  * w_0_00 
            w00_01 = wp  * w_0_01 
            w00_10 = wp  * w_0_10 
            w00_11 = wp  * w_0_11 
            w01_00 = wp  * w_1_00 
            w01_01 = wp  * w_1_01 
            w01_10 = wp  * w_1_10 
            w01_11 = wp  * w_1_11 
            w10_00 = wp1 * w_0_00 
            w10_01 = wp1 * w_0_01 
            w10_10 = wp1 * w_0_10 
            w10_11 = wp1 * w_0_11 
            w11_00 = wp1 * w_1_00 
            w11_01 = wp1 * w_1_01 
            w11_10 = wp1 * w_1_10 
            w11_11 = wp1 * w_1_11 




            ib = 1
            
            fa = fat(1,ib) + &
                 fat(2,ib) * te1 + &
                 fat(3,ib) * te2 + &
                 fat(4,ib) * te3 + &
                 fat(5,ib) * te4 + &
                 fat(6,ib) * te5
            
            a_star = &
                 ah2onw(ip , itp , iu , ite , irh ) * w11_11 * wu1 + &
                 ah2onw(ip , itp , iu , ite , irh1) * w11_10 * wu1 + &
                 ah2onw(ip , itp , iu , ite1, irh ) * w11_01 * wu1 + &
                 ah2onw(ip , itp , iu , ite1, irh1) * w11_00 * wu1 + &
                 ah2onw(ip , itp , iu1, ite , irh ) * w11_11 * wu  + &
                 ah2onw(ip , itp , iu1, ite , irh1) * w11_10 * wu  + &
                 ah2onw(ip , itp , iu1, ite1, irh ) * w11_01 * wu  + &
                 ah2onw(ip , itp , iu1, ite1, irh1) * w11_00 * wu  + &
                 ah2onw(ip , itp1, iu , ite , irh ) * w10_11 * wu1 + &
                 ah2onw(ip , itp1, iu , ite , irh1) * w10_10 * wu1 + &
                 ah2onw(ip , itp1, iu , ite1, irh ) * w10_01 * wu1 + &
                 ah2onw(ip , itp1, iu , ite1, irh1) * w10_00 * wu1 + &
                 ah2onw(ip , itp1, iu1, ite , irh ) * w10_11 * wu  + &
                 ah2onw(ip , itp1, iu1, ite , irh1) * w10_10 * wu  + &
                 ah2onw(ip , itp1, iu1, ite1, irh ) * w10_01 * wu  + &
                 ah2onw(ip , itp1, iu1, ite1, irh1) * w10_00 * wu  + &
                 ah2onw(ip1, itp , iu , ite , irh ) * w01_11 * wu1 + &
                 ah2onw(ip1, itp , iu , ite , irh1) * w01_10 * wu1 + &
                 ah2onw(ip1, itp , iu , ite1, irh ) * w01_01 * wu1 + &
                 ah2onw(ip1, itp , iu , ite1, irh1) * w01_00 * wu1 + &
                 ah2onw(ip1, itp , iu1, ite , irh ) * w01_11 * wu  + &
                 ah2onw(ip1, itp , iu1, ite , irh1) * w01_10 * wu  + &
                 ah2onw(ip1, itp , iu1, ite1, irh ) * w01_01 * wu  + &
                 ah2onw(ip1, itp , iu1, ite1, irh1) * w01_00 * wu  + &
                 ah2onw(ip1, itp1, iu , ite , irh ) * w00_11 * wu1 + &
                 ah2onw(ip1, itp1, iu , ite , irh1) * w00_10 * wu1 + &
                 ah2onw(ip1, itp1, iu , ite1, irh ) * w00_01 * wu1 + &
                 ah2onw(ip1, itp1, iu , ite1, irh1) * w00_00 * wu1 + &
                 ah2onw(ip1, itp1, iu1, ite , irh ) * w00_11 * wu  + &
                 ah2onw(ip1, itp1, iu1, ite , irh1) * w00_10 * wu  + &
                 ah2onw(ip1, itp1, iu1, ite1, irh ) * w00_01 * wu  + &
                 ah2onw(ip1, itp1, iu1, ite1, irh1) * w00_00 * wu
            
            abso(i,ib) = min(max(fa * (1.0 - (1.0 - a_star) * &
                                 aer_trn_ngh(i,ib)), &
                             0.0_r8), 1.0_r8)




            if (uvar < min_u_h2o) then
               uscl = uvar / min_u_h2o
               abso(i,ib) = abso(i,ib) * uscl
            endif
            



            ib = 2
            
            fa = fat(1,ib) + &
                 fat(2,ib) * te1 + &
                 fat(3,ib) * te2 + &
                 fat(4,ib) * te3 + &
                 fat(5,ib) * te4 + &
                 fat(6,ib) * te5
            
            a_star = &
                 ah2ow(ip , itp , iu , ite , irh ) * w11_11 * wu1 + &
                 ah2ow(ip , itp , iu , ite , irh1) * w11_10 * wu1 + &
                 ah2ow(ip , itp , iu , ite1, irh ) * w11_01 * wu1 + &
                 ah2ow(ip , itp , iu , ite1, irh1) * w11_00 * wu1 + &
                 ah2ow(ip , itp , iu1, ite , irh ) * w11_11 * wu  + &
                 ah2ow(ip , itp , iu1, ite , irh1) * w11_10 * wu  + &
                 ah2ow(ip , itp , iu1, ite1, irh ) * w11_01 * wu  + &
                 ah2ow(ip , itp , iu1, ite1, irh1) * w11_00 * wu  + &
                 ah2ow(ip , itp1, iu , ite , irh ) * w10_11 * wu1 + &
                 ah2ow(ip , itp1, iu , ite , irh1) * w10_10 * wu1 + &
                 ah2ow(ip , itp1, iu , ite1, irh ) * w10_01 * wu1 + &
                 ah2ow(ip , itp1, iu , ite1, irh1) * w10_00 * wu1 + &
                 ah2ow(ip , itp1, iu1, ite , irh ) * w10_11 * wu  + &
                 ah2ow(ip , itp1, iu1, ite , irh1) * w10_10 * wu  + &
                 ah2ow(ip , itp1, iu1, ite1, irh ) * w10_01 * wu  + &
                 ah2ow(ip , itp1, iu1, ite1, irh1) * w10_00 * wu  + &
                 ah2ow(ip1, itp , iu , ite , irh ) * w01_11 * wu1 + &
                 ah2ow(ip1, itp , iu , ite , irh1) * w01_10 * wu1 + &
                 ah2ow(ip1, itp , iu , ite1, irh ) * w01_01 * wu1 + &
                 ah2ow(ip1, itp , iu , ite1, irh1) * w01_00 * wu1 + &
                 ah2ow(ip1, itp , iu1, ite , irh ) * w01_11 * wu  + &
                 ah2ow(ip1, itp , iu1, ite , irh1) * w01_10 * wu  + &
                 ah2ow(ip1, itp , iu1, ite1, irh ) * w01_01 * wu  + &
                 ah2ow(ip1, itp , iu1, ite1, irh1) * w01_00 * wu  + &
                 ah2ow(ip1, itp1, iu , ite , irh ) * w00_11 * wu1 + &
                 ah2ow(ip1, itp1, iu , ite , irh1) * w00_10 * wu1 + &
                 ah2ow(ip1, itp1, iu , ite1, irh ) * w00_01 * wu1 + &
                 ah2ow(ip1, itp1, iu , ite1, irh1) * w00_00 * wu1 + &
                 ah2ow(ip1, itp1, iu1, ite , irh ) * w00_11 * wu  + &
                 ah2ow(ip1, itp1, iu1, ite , irh1) * w00_10 * wu  + &
                 ah2ow(ip1, itp1, iu1, ite1, irh ) * w00_01 * wu  + &
                 ah2ow(ip1, itp1, iu1, ite1, irh1) * w00_00 * wu
            
            abso(i,ib) = min(max(fa * (1.0 - (1.0 - a_star) * &
                                 aer_trn_ngh(i,ib)), &
                             0.0_r8), 1.0_r8)




            if (uvar < min_u_h2o) then
               uscl = uvar / min_u_h2o
               abso(i,ib) = abso(i,ib) * uscl
            endif
            
         end do



         do i=1,ncol
            term7(i,1) = coefj(1,1) + coefj(2,1)*dty(i)*(1. + c16*dty(i))
            term8(i,1) = coefk(1,1) + coefk(2,1)*dty(i)*(1. + c17*dty(i))
            term7(i,2) = coefj(1,2) + coefj(2,2)*dty(i)*(1. + c26*dty(i))
            term8(i,2) = coefk(1,2) + coefk(2,2)*dty(i)*(1. + c27*dty(i))
         end do



         do i=1,ncol
            dtym10     = dty(i) - 10.
            denom      = 1. + (c30 + c31*dtym10*dtym10)*sqrtu(i)
            k21        = term7(i,1) + term8(i,1)/denom
            denom      = 1. + (c28 + c29*dtym10       )*sqrtu(i)
            k22        = term7(i,2) + term8(i,2)/denom
            tr1     = exp(-(k21*(sqrtu(i) + fc1*fwku(i))))
            tr2     = exp(-(k22*(sqrtu(i) + fc1*fwku(i))))
            tr1=tr1*aer_trn_ngh(i,idx_LW_0650_0800) 

            tr2=tr2*aer_trn_ngh(i,idx_LW_0500_0650) 

            tr5     = exp(-((coefh(1,3) + coefh(2,3)*dtx(i))*uc1(i)))
            tr6     = exp(-((coefh(1,4) + coefh(2,4)*dtx(i))*uc1(i)))
            tr9(i)  = tr1*tr5
            tr10(i) = tr2*tr6
            trab2(i)= 0.65*tr9(i) + 0.35*tr10(i)
            th2o(i) = tr10(i)
         end do



         do i=1,ncol
            te        = (tbar(i,kn)*r293)**.7
            dplos     = abs(plos(i,k2+1) - plos(i,k2))
            u1        = zinpl(i,kn)*18.29*dplos/te
            u2        = zinpl(i,kn)*.5649*dplos/te
            tlocal    = tbar(i,kn)
            tcrfac    = sqrt(tlocal*r250)*te
            beta      = r3205*(pinpl(i,kn)*rsslp + dpfo3*tcrfac)
            realnu    = te/beta
            tmp1      = u1/sqrt(4. + u1*(1. + realnu))
            tmp2      = u2/sqrt(4. + u2*(1. + realnu))
            o3bndi    = 74.*te*log(1. + tmp1 + tmp2)
            abso(i,3) = o3bndi*o3emm(i,kn)*(h2otr(i,k2+1)/h2otr(i,k2))
            to3(i)    = 1.0/(1. + 0.1*tmp1 + 0.1*tmp2)
         end do



         do i=1,ncol
            dplco2   = plco2(i,k2+1) - plco2(i,k2)
            sqwp     = sqrt(uinpl(i,kn)*dplco2)
            et       = exp(-480./tbar(i,kn))
            sqti(i)  = sqrt(tbar(i,kn))
            rsqti    = 1./sqti(i)
            et2      = et*et
            et4      = et2*et2
            omet     = (1. - 1.5*et2)
            f1co2    = 899.70*omet*(1. + 1.94774*et + 4.73486*et2)*rsqti
            f1sqwp(i)= f1co2*sqwp
            t1co2(i) = 1./(1. + (245.18*omet*sqwp*rsqti))
            oneme    = 1. - et2
            alphat   = oneme**3*rsqti
            pi       = abs(dpnm(i))*winpl(i,kn)
            wco2     = 2.5221*co2vmr*pi*rga
            u7(i)    = 4.9411e4*alphat*et2*wco2
            u8       = 3.9744e4*alphat*et4*wco2
            u9       = 1.0447e5*alphat*et4*et2*wco2
            u13      = 2.8388e3*alphat*et4*wco2
            tpath    = tbar(i,kn)
            tlocal   = tbar(i,kn)
            tcrfac   = sqrt((tlocal*r250)*(tpath*r300))
            posqt    = (pinpl(i,kn)*rsslp + dpfco2*tcrfac)*rsqti
            rbeta7(i)= 1./(5.3228*posqt)
            rbeta8   = 1./(10.6576*posqt)
            rbeta9   = rbeta7(i)
            rbeta13  = rbeta9
            f2co2(i) = u7(i)/sqrt(4. + u7(i)*(1. + rbeta7(i))) + &
                 u8   /sqrt(4. + u8*(1. + rbeta8)) + &
                 u9   /sqrt(4. + u9*(1. + rbeta9))
            f3co2(i) = u13/sqrt(4. + u13*(1. + rbeta13))
            tmp1     = log(1. + f1sqwp(i))
            tmp2     = log(1. + f2co2(i))
            tmp3     = log(1. + f3co2(i))
            absbnd   = (tmp1 + 2.*t1co2(i)*tmp2 + 2.*tmp3)*sqti(i)
            abso(i,4)= trab2(i)*emm(i,kn)*absbnd
            tco2(i)  = 1.0/(1.0+ 10.0*u7(i)/sqrt(4. + u7(i)*(1. + rbeta7(i))))
         end do 



         call trcabn(lchnk   ,ncol    ,pcols, pverp,                   &
              k2      ,kn      ,ucfc11  ,ucfc12  ,un2o0   , &
              un2o1   ,uch4    ,uco211  ,uco212  ,uco213  , &
              uco221  ,uco222  ,uco223  ,tbar    ,bplnk   , &
              winpl   ,pinpl   ,tco2    ,th2o    ,to3     , &
              uptype  ,dw      ,s2c     ,u       ,pnew    , &
              abstrc  ,uinpl   , &
              aer_trn_ngh)



         do i=1,ncol
            absnxt(i,k2,kn) = abso(i,1) + abso(i,2) + &
                 abso(i,3) + abso(i,4) + abstrc(i)
         end do
      end do 
   end do 

   return
end subroutine radabs



subroutine radems(lchnk   ,ncol    ,pcols, pver, pverp,         &
                  s2c     ,tcg     ,w       ,tplnke  ,plh2o   , &
                  pnm     ,plco2   ,tint    ,tint4   ,tlayr   , &
                  tlayr4  ,plol    ,plos    ,ucfc11  ,ucfc12  , &
                  un2o0   ,un2o1   ,uch4    ,uco211 ,uco212   , &
                  uco213  ,uco221  ,uco222  ,uco223  ,uptype  , &
                  bn2o0   ,bn2o1   ,bch4    ,co2em   ,co2eml  , &
                  co2t    ,h2otr   ,abplnk1 ,abplnk2 ,emstot  , &
                  plh2ob  ,wb      , &
                  aer_trn_ttl)
























































   integer, intent(in) :: lchnk                    
   integer, intent(in) :: ncol                     
   integer, intent(in) :: pcols, pver, pverp

   real(r8), intent(in) :: s2c(pcols,pverp)        
   real(r8), intent(in) :: tcg(pcols,pverp)        
   real(r8), intent(in) :: w(pcols,pverp)          
   real(r8), intent(in) :: tplnke(pcols)           
   real(r8), intent(in) :: plh2o(pcols,pverp)      
   real(r8), intent(in) :: pnm(pcols,pverp)        
   real(r8), intent(in) :: plco2(pcols,pverp)      
   real(r8), intent(in) :: tint(pcols,pverp)       
   real(r8), intent(in) :: tint4(pcols,pverp)      
   real(r8), intent(in) :: tlayr(pcols,pverp)      
   real(r8), intent(in) :: tlayr4(pcols,pverp)     
   real(r8), intent(in) :: plol(pcols,pverp)       
   real(r8), intent(in) :: plos(pcols,pverp)       
   real(r8), intent(in) :: plh2ob(nbands,pcols,pverp) 
                                                      
                                                      
   real(r8), intent(in) :: wb(nbands,pcols,pverp)     
                                                      
                                                      

   real(r8), intent(in) :: aer_trn_ttl(pcols,pverp,pverp,bnd_nbr_LW) 






   real(r8), intent(in) :: ucfc11(pcols,pverp)     
   real(r8), intent(in) :: ucfc12(pcols,pverp)     
   real(r8), intent(in) :: un2o0(pcols,pverp)      
   real(r8), intent(in) :: un2o1(pcols,pverp)      
   real(r8), intent(in) :: uch4(pcols,pverp)       
   real(r8), intent(in) :: uco211(pcols,pverp)     
   real(r8), intent(in) :: uco212(pcols,pverp)     
   real(r8), intent(in) :: uco213(pcols,pverp)     
   real(r8), intent(in) :: uco221(pcols,pverp)     
   real(r8), intent(in) :: uco222(pcols,pverp)     
   real(r8), intent(in) :: uco223(pcols,pverp)     
   real(r8), intent(in) :: bn2o0(pcols,pverp)      
   real(r8), intent(in) :: bn2o1(pcols,pverp)      
   real(r8), intent(in) :: bch4(pcols,pverp)       
   real(r8), intent(in) :: uptype(pcols,pverp)     



   real(r8), intent(out) :: emstot(pcols,pverp)     
   real(r8), intent(out) :: co2em(pcols,pverp)      
   real(r8), intent(out) :: co2eml(pcols,pver)      
   real(r8), intent(out) :: co2t(pcols,pverp)       
   real(r8), intent(out) :: h2otr(pcols,pverp)      
   real(r8), intent(out) :: abplnk1(14,pcols,pverp) 
   real(r8), intent(out) :: abplnk2(14,pcols,pverp) 




   integer i                    
   integer k                    
   integer k1                   



   real(r8) h2oems(pcols,pverp)     
   real(r8) tpathe                  
   real(r8) dtx(pcols)              
   real(r8) dty(pcols)              




   real(r8) emis(pcols,2)           



   real(r8) term7(pcols,2)          
   real(r8) term8(pcols,2)          
   real(r8) tr1(pcols)              
   real(r8) tr2(pcols)              
   real(r8) tr3(pcols)              
   real(r8) tr4(pcols)              
   real(r8) tr7(pcols)              

   real(r8) tr8(pcols)              

   real(r8) k21(pcols)              


   real(r8) k22(pcols)              


   real(r8) u(pcols)                
   real(r8) ub(nbands)              
                                    
                                    
   real(r8) pnew                    
   real(r8) pnewb(nbands)           
                                    
                                    
   real(r8) uc1(pcols)              
   real(r8) fwk                     
   real(r8) troco2(pcols,pverp)     
   real(r8) emplnk(14,pcols)        
   real(r8) emstrc(pcols,pverp)     



   real(r8) co2ems(pcols,pverp)      
   real(r8) co2plk(pcols)            
   real(r8) sum(pcols)               
   real(r8) t1i                      
   real(r8) sqti                     
   real(r8) pi                       
   real(r8) et                       
   real(r8) et2                      
   real(r8) et4                      
   real(r8) omet                     
   real(r8) ex                       
   real(r8) f1co2                    
   real(r8) f2co2                    
   real(r8) f3co2                    
   real(r8) t1co2                    
   real(r8) sqwp                     
   real(r8) f1sqwp                   
   real(r8) oneme                    
   real(r8) alphat                   
   real(r8) wco2                     
   real(r8) posqt                    
   real(r8) rbeta7                   
   real(r8) rbeta8                   
   real(r8) rbeta9                   
   real(r8) rbeta13                  
   real(r8) tpath                    
   real(r8) tmp1                     
   real(r8) tmp2                     
   real(r8) tmp3                     
   real(r8) tlayr5                   
   real(r8) rsqti                    
   real(r8) exm1sq                   
   real(r8) u7                       
   real(r8) u8                       
   real(r8) u9                       
   real(r8) u13                      
   real(r8) r250                     
   real(r8) r300                     
   real(r8) rsslp                    



   real(r8) o3ems(pcols,pverp)       
   real(r8) dbvtt(pcols)             
   real(r8) dbvt,fo3,t,ux,vx
   real(r8) te                       
   real(r8) u1                       
   real(r8) u2                       
   real(r8) phat                     
   real(r8) tlocal                   
   real(r8) tcrfac                   
   real(r8) beta                     
   real(r8) realnu                   
   real(r8) o3bndi                   



   real(r8) absbnd                   
   real(r8) tco2(pcols)              
   real(r8) th2o(pcols)              
   real(r8) to3(pcols)               












   real(r8) fe               
   real(r8) e_star           
   real(r8) l_star           
   real(r8) c_star           

   real(r8) te1              
   real(r8) te2              
   real(r8) te3              
   real(r8) te4              
   real(r8) te5              

   real(r8) log_u            
   real(r8) log_uc           
   real(r8) log_p            
   real(r8) t_p              
   real(r8) t_e              

   integer iu                
   integer iu1               
   integer iuc               
   integer iuc1              
   integer ip                
   integer ip1               
   integer itp               
   integer itp1              
   integer ite               
   integer ite1              
   integer irh               
   integer irh1              

   real(r8) dvar             
   real(r8) uvar             
   real(r8) uscl             

   real(r8) wu               
   real(r8) wu1              
   real(r8) wuc              
   real(r8) wuc1             
   real(r8) wp               
   real(r8) wp1              
   real(r8) wtp              
   real(r8) wtp1             
   real(r8) wte              
   real(r8) wte1             
   real(r8) wrh              
   real(r8) wrh1             

   real(r8) w_0_0_           
   real(r8) w_0_1_           
   real(r8) w_1_0_           
   real(r8) w_1_1_           

   real(r8) w_0_00           
   real(r8) w_0_01           
   real(r8) w_0_10           
   real(r8) w_0_11           
   real(r8) w_1_00           
   real(r8) w_1_01           
   real(r8) w_1_10           
   real(r8) w_1_11           

   real(r8) w00_00           
   real(r8) w00_01           
   real(r8) w00_10           
   real(r8) w00_11           
   real(r8) w01_00           
   real(r8) w01_01           
   real(r8) w01_10           
   real(r8) w01_11           
   real(r8) w10_00           
   real(r8) w10_01           
   real(r8) w10_10           
   real(r8) w10_11           
   real(r8) w11_00           
   real(r8) w11_01           
   real(r8) w11_10           
   real(r8) w11_11           

   integer ib                
                             
                             

   real(r8) pch2o            
   real(r8) fch2o            
   real(r8) uch2o            

   real(r8) fdif             

   real(r8) sslp_mks         
   real(r8) esx              
   real(r8) qsx              
   real(r8) pnew_mks         
   real(r8) q_path           
   real(r8) rh_path          
   real(r8) omeps            

   integer  iest             








   dbvt(t)=(-2.8911366682e-4+(2.3771251896e-6+1.1305188929e-10*t)*t)/ &
           (1.0+(-6.1364820707e-3+1.5550319767e-5*t)*t)

   fo3(ux,vx)=ux/sqrt(4.+ux*(1.+vx))







   r250  = 1./250.
   r300  = 1./300.
   rsslp = 1./sslp



   fdif       = 1.66
   sslp_mks   = sslp / 10.0
   omeps      = 1.0 - epsilo



   do i=1,ncol
      ex             = exp(960./tplnke(i))
      co2plk(i)      = 5.e8/((tplnke(i)**4)*(ex - 1.))
      co2t(i,ntoplw) = tplnke(i)
      sum(i)         = co2t(i,ntoplw)*pnm(i,ntoplw)
   end do
   k = ntoplw
   do k1=pverp,ntoplw+1,-1
      k = k + 1
      do i=1,ncol
         sum(i)         = sum(i) + tlayr(i,k)*(pnm(i,k)-pnm(i,k-1))
         ex             = exp(960./tlayr(i,k1))
         tlayr5         = tlayr(i,k1)*tlayr4(i,k1)
         co2eml(i,k1-1) = 1.2e11*ex/(tlayr5*(ex - 1.)**2)
         co2t(i,k)      = sum(i)/pnm(i,k)
      end do
   end do



   do i=1,ncol
      dbvtt(i) = dbvt(tplnke(i))
   end do



   call trcplk(lchnk   ,ncol    ,pcols, pver, pverp,         &
               tint    ,tlayr   ,tplnke  ,emplnk  ,abplnk1 , &
               abplnk2 )



   do k1=ntoplw,pverp














      do i=1,ncol
         u(i)        = plh2o(i,k1)
         pnew        = u(i)/w(i,k1)
         pnew_mks    = pnew * sslp_mks



         uc1(i)      = (s2c(i,k1) + 1.7e-3*plh2o(i,k1))*(1. + 2.*s2c(i,k1))/ &
                       (1. + 15.*s2c(i,k1))
         pch2o       = s2c(i,k1)



         tpathe   = tcg(i,k1)/w(i,k1)
         t_p = min(max(tpathe, min_tp_h2o), max_tp_h2o)
         iest = floor(t_p) - min_tp_h2o
         esx = estblh2o(iest) + (estblh2o(iest+1)-estblh2o(iest)) * &
               (t_p - min_tp_h2o - iest)
         qsx = epsilo * esx / (pnew_mks - omeps * esx)



         q_path = w(i,k1) / pnm(i,k1) / rga










         ub(1) = plh2ob(1,i,k1) / psi(t_p,1)
         ub(2) = plh2ob(2,i,k1) / psi(t_p,2)

         pnewb(1) = ub(1) / wb(1,i,k1) * phi(t_p,1)
         pnewb(2) = ub(2) / wb(2,i,k1) * phi(t_p,2)



         dtx(i) = tplnke(i) - 250.
         dty(i) = tpathe - 250.























         te1  = tplnke(i)
         te2  = te1 * te1
         te3  = te2 * te1
         te4  = te3 * te1
         te5  = te4 * te1



         dvar = (t_p - min_tp_h2o) / dtp_h2o
         itp = min(max(int(aint(dvar,r8)) + 1, 1), n_tp - 1)
         itp1 = itp + 1
         wtp = dvar - floor(dvar)
         wtp1 = 1.0 - wtp

         t_e = min(max(tplnke(i) - t_p, min_te_h2o), max_te_h2o)
         dvar = (t_e - min_te_h2o) / dte_h2o
         ite = min(max(int(aint(dvar,r8)) + 1, 1), n_te - 1)
         ite1 = ite + 1
         wte = dvar - floor(dvar)
         wte1 = 1.0 - wte

         rh_path = min(max(q_path / qsx, min_rh_h2o), max_rh_h2o)
         dvar = (rh_path - min_rh_h2o) / drh_h2o
         irh = min(max(int(aint(dvar,r8)) + 1, 1), n_rh - 1)
         irh1 = irh + 1
         wrh = dvar - floor(dvar)
         wrh1 = 1.0 - wrh

         w_0_0_ = wtp  * wte
         w_0_1_ = wtp  * wte1
         w_1_0_ = wtp1 * wte 
         w_1_1_ = wtp1 * wte1

         w_0_00 = w_0_0_ * wrh
         w_0_01 = w_0_0_ * wrh1
         w_0_10 = w_0_1_ * wrh
         w_0_11 = w_0_1_ * wrh1
         w_1_00 = w_1_0_ * wrh
         w_1_01 = w_1_0_ * wrh1
         w_1_10 = w_1_1_ * wrh
         w_1_11 = w_1_1_ * wrh1



















































         fch2o = fh2oself(t_p)
         uch2o = (pch2o * epsilo) / (q_path * fch2o)




         ib = 1

         uvar = ub(ib) * fdif
         log_u  = min(log10(max(uvar, min_u_h2o)), max_lu_h2o)
         dvar = (log_u - min_lu_h2o) / dlu_h2o
         iu = min(max(int(aint(dvar,r8)) + 1, 1), n_u - 1)
         iu1 = iu + 1
         wu = dvar - floor(dvar)
         wu1 = 1.0 - wu
         
         log_p  = min(log10(max(pnewb(ib), min_p_h2o)), max_lp_h2o)
         dvar = (log_p - min_lp_h2o) / dlp_h2o
         ip = min(max(int(aint(dvar,r8)) + 1, 1), n_p - 1)
         ip1 = ip + 1
         wp = dvar - floor(dvar)
         wp1 = 1.0 - wp

         w00_00 = wp  * w_0_00 
         w00_01 = wp  * w_0_01 
         w00_10 = wp  * w_0_10 
         w00_11 = wp  * w_0_11 
         w01_00 = wp  * w_1_00 
         w01_01 = wp  * w_1_01 
         w01_10 = wp  * w_1_10 
         w01_11 = wp  * w_1_11 
         w10_00 = wp1 * w_0_00 
         w10_01 = wp1 * w_0_01 
         w10_10 = wp1 * w_0_10 
         w10_11 = wp1 * w_0_11 
         w11_00 = wp1 * w_1_00 
         w11_01 = wp1 * w_1_01 
         w11_10 = wp1 * w_1_10 
         w11_11 = wp1 * w_1_11 




         fe = fet(1,ib) + &
              fet(2,ib) * te1 + &
              fet(3,ib) * te2 + &
              fet(4,ib) * te3 + &
              fet(5,ib) * te4 + &
              fet(6,ib) * te5

         e_star = &
              eh2onw(ip , itp , iu , ite , irh ) * w11_11 * wu1 + &
              eh2onw(ip , itp , iu , ite , irh1) * w11_10 * wu1 + &
              eh2onw(ip , itp , iu , ite1, irh ) * w11_01 * wu1 + &
              eh2onw(ip , itp , iu , ite1, irh1) * w11_00 * wu1 + &
              eh2onw(ip , itp , iu1, ite , irh ) * w11_11 * wu  + &
              eh2onw(ip , itp , iu1, ite , irh1) * w11_10 * wu  + &
              eh2onw(ip , itp , iu1, ite1, irh ) * w11_01 * wu  + &
              eh2onw(ip , itp , iu1, ite1, irh1) * w11_00 * wu  + &
              eh2onw(ip , itp1, iu , ite , irh ) * w10_11 * wu1 + &
              eh2onw(ip , itp1, iu , ite , irh1) * w10_10 * wu1 + &
              eh2onw(ip , itp1, iu , ite1, irh ) * w10_01 * wu1 + &
              eh2onw(ip , itp1, iu , ite1, irh1) * w10_00 * wu1 + &
              eh2onw(ip , itp1, iu1, ite , irh ) * w10_11 * wu  + &
              eh2onw(ip , itp1, iu1, ite , irh1) * w10_10 * wu  + &
              eh2onw(ip , itp1, iu1, ite1, irh ) * w10_01 * wu  + &
              eh2onw(ip , itp1, iu1, ite1, irh1) * w10_00 * wu  + &
              eh2onw(ip1, itp , iu , ite , irh ) * w01_11 * wu1 + &
              eh2onw(ip1, itp , iu , ite , irh1) * w01_10 * wu1 + &
              eh2onw(ip1, itp , iu , ite1, irh ) * w01_01 * wu1 + &
              eh2onw(ip1, itp , iu , ite1, irh1) * w01_00 * wu1 + &
              eh2onw(ip1, itp , iu1, ite , irh ) * w01_11 * wu  + &
              eh2onw(ip1, itp , iu1, ite , irh1) * w01_10 * wu  + &
              eh2onw(ip1, itp , iu1, ite1, irh ) * w01_01 * wu  + &
              eh2onw(ip1, itp , iu1, ite1, irh1) * w01_00 * wu  + &
              eh2onw(ip1, itp1, iu , ite , irh ) * w00_11 * wu1 + &
              eh2onw(ip1, itp1, iu , ite , irh1) * w00_10 * wu1 + &
              eh2onw(ip1, itp1, iu , ite1, irh ) * w00_01 * wu1 + &
              eh2onw(ip1, itp1, iu , ite1, irh1) * w00_00 * wu1 + &
              eh2onw(ip1, itp1, iu1, ite , irh ) * w00_11 * wu  + &
              eh2onw(ip1, itp1, iu1, ite , irh1) * w00_10 * wu  + &
              eh2onw(ip1, itp1, iu1, ite1, irh ) * w00_01 * wu  + &
              eh2onw(ip1, itp1, iu1, ite1, irh1) * w00_00 * wu 
         emis(i,ib) = min(max(fe * (1.0 - (1.0 - e_star) * &
                              aer_trn_ttl(i,k1,1,ib)), &
                          0.0_r8), 1.0_r8)



         if (uvar < min_u_h2o) then
            uscl = uvar / min_u_h2o
            emis(i,ib) = emis(i,ib) * uscl
         endif

                      




         ib = 2

         uvar = ub(ib) * fdif
         log_u  = min(log10(max(uvar, min_u_h2o)), max_lu_h2o)
         dvar = (log_u - min_lu_h2o) / dlu_h2o
         iu = min(max(int(aint(dvar,r8)) + 1, 1), n_u - 1)
         iu1 = iu + 1
         wu = dvar - floor(dvar)
         wu1 = 1.0 - wu
         
         log_p  = min(log10(max(pnewb(ib), min_p_h2o)), max_lp_h2o)
         dvar = (log_p - min_lp_h2o) / dlp_h2o
         ip = min(max(int(aint(dvar,r8)) + 1, 1), n_p - 1)
         ip1 = ip + 1
         wp = dvar - floor(dvar)
         wp1 = 1.0 - wp

         w00_00 = wp  * w_0_00 
         w00_01 = wp  * w_0_01 
         w00_10 = wp  * w_0_10 
         w00_11 = wp  * w_0_11 
         w01_00 = wp  * w_1_00 
         w01_01 = wp  * w_1_01 
         w01_10 = wp  * w_1_10 
         w01_11 = wp  * w_1_11 
         w10_00 = wp1 * w_0_00 
         w10_01 = wp1 * w_0_01 
         w10_10 = wp1 * w_0_10 
         w10_11 = wp1 * w_0_11 
         w11_00 = wp1 * w_1_00 
         w11_01 = wp1 * w_1_01 
         w11_10 = wp1 * w_1_10 
         w11_11 = wp1 * w_1_11 

         log_uc  = min(log10(max(uch2o * fdif, min_u_h2o)), max_lu_h2o)
         dvar = (log_uc - min_lu_h2o) / dlu_h2o
         iuc = min(max(int(aint(dvar,r8)) + 1, 1), n_u - 1)
         iuc1 = iuc + 1
         wuc = dvar - floor(dvar)
         wuc1 = 1.0 - wuc



         fe = fet(1,ib) + &
              fet(2,ib) * te1 + &
              fet(3,ib) * te2 + &
              fet(4,ib) * te3 + &
              fet(5,ib) * te4 + &
              fet(6,ib) * te5

         l_star = &
              ln_eh2ow(ip , itp , iu , ite , irh ) * w11_11 * wu1 + &
              ln_eh2ow(ip , itp , iu , ite , irh1) * w11_10 * wu1 + &
              ln_eh2ow(ip , itp , iu , ite1, irh ) * w11_01 * wu1 + &
              ln_eh2ow(ip , itp , iu , ite1, irh1) * w11_00 * wu1 + &
              ln_eh2ow(ip , itp , iu1, ite , irh ) * w11_11 * wu  + &
              ln_eh2ow(ip , itp , iu1, ite , irh1) * w11_10 * wu  + &
              ln_eh2ow(ip , itp , iu1, ite1, irh ) * w11_01 * wu  + &
              ln_eh2ow(ip , itp , iu1, ite1, irh1) * w11_00 * wu  + &
              ln_eh2ow(ip , itp1, iu , ite , irh ) * w10_11 * wu1 + &
              ln_eh2ow(ip , itp1, iu , ite , irh1) * w10_10 * wu1 + &
              ln_eh2ow(ip , itp1, iu , ite1, irh ) * w10_01 * wu1 + &
              ln_eh2ow(ip , itp1, iu , ite1, irh1) * w10_00 * wu1 + &
              ln_eh2ow(ip , itp1, iu1, ite , irh ) * w10_11 * wu  + &
              ln_eh2ow(ip , itp1, iu1, ite , irh1) * w10_10 * wu  + &
              ln_eh2ow(ip , itp1, iu1, ite1, irh ) * w10_01 * wu  + &
              ln_eh2ow(ip , itp1, iu1, ite1, irh1) * w10_00 * wu  + &
              ln_eh2ow(ip1, itp , iu , ite , irh ) * w01_11 * wu1 + &
              ln_eh2ow(ip1, itp , iu , ite , irh1) * w01_10 * wu1 + &
              ln_eh2ow(ip1, itp , iu , ite1, irh ) * w01_01 * wu1 + &
              ln_eh2ow(ip1, itp , iu , ite1, irh1) * w01_00 * wu1 + &
              ln_eh2ow(ip1, itp , iu1, ite , irh ) * w01_11 * wu  + &
              ln_eh2ow(ip1, itp , iu1, ite , irh1) * w01_10 * wu  + &
              ln_eh2ow(ip1, itp , iu1, ite1, irh ) * w01_01 * wu  + &
              ln_eh2ow(ip1, itp , iu1, ite1, irh1) * w01_00 * wu  + &
              ln_eh2ow(ip1, itp1, iu , ite , irh ) * w00_11 * wu1 + &
              ln_eh2ow(ip1, itp1, iu , ite , irh1) * w00_10 * wu1 + &
              ln_eh2ow(ip1, itp1, iu , ite1, irh ) * w00_01 * wu1 + &
              ln_eh2ow(ip1, itp1, iu , ite1, irh1) * w00_00 * wu1 + &
              ln_eh2ow(ip1, itp1, iu1, ite , irh ) * w00_11 * wu  + &
              ln_eh2ow(ip1, itp1, iu1, ite , irh1) * w00_10 * wu  + &
              ln_eh2ow(ip1, itp1, iu1, ite1, irh ) * w00_01 * wu  + &
              ln_eh2ow(ip1, itp1, iu1, ite1, irh1) * w00_00 * wu 

         c_star = &
              cn_eh2ow(ip , itp , iuc , ite , irh ) * w11_11 * wuc1 + &
              cn_eh2ow(ip , itp , iuc , ite , irh1) * w11_10 * wuc1 + &
              cn_eh2ow(ip , itp , iuc , ite1, irh ) * w11_01 * wuc1 + &
              cn_eh2ow(ip , itp , iuc , ite1, irh1) * w11_00 * wuc1 + &
              cn_eh2ow(ip , itp , iuc1, ite , irh ) * w11_11 * wuc  + &
              cn_eh2ow(ip , itp , iuc1, ite , irh1) * w11_10 * wuc  + &
              cn_eh2ow(ip , itp , iuc1, ite1, irh ) * w11_01 * wuc  + &
              cn_eh2ow(ip , itp , iuc1, ite1, irh1) * w11_00 * wuc  + &
              cn_eh2ow(ip , itp1, iuc , ite , irh ) * w10_11 * wuc1 + &
              cn_eh2ow(ip , itp1, iuc , ite , irh1) * w10_10 * wuc1 + &
              cn_eh2ow(ip , itp1, iuc , ite1, irh ) * w10_01 * wuc1 + &
              cn_eh2ow(ip , itp1, iuc , ite1, irh1) * w10_00 * wuc1 + &
              cn_eh2ow(ip , itp1, iuc1, ite , irh ) * w10_11 * wuc  + &
              cn_eh2ow(ip , itp1, iuc1, ite , irh1) * w10_10 * wuc  + &
              cn_eh2ow(ip , itp1, iuc1, ite1, irh ) * w10_01 * wuc  + &
              cn_eh2ow(ip , itp1, iuc1, ite1, irh1) * w10_00 * wuc  + &
              cn_eh2ow(ip1, itp , iuc , ite , irh ) * w01_11 * wuc1 + &
              cn_eh2ow(ip1, itp , iuc , ite , irh1) * w01_10 * wuc1 + &
              cn_eh2ow(ip1, itp , iuc , ite1, irh ) * w01_01 * wuc1 + &
              cn_eh2ow(ip1, itp , iuc , ite1, irh1) * w01_00 * wuc1 + &
              cn_eh2ow(ip1, itp , iuc1, ite , irh ) * w01_11 * wuc  + &
              cn_eh2ow(ip1, itp , iuc1, ite , irh1) * w01_10 * wuc  + &
              cn_eh2ow(ip1, itp , iuc1, ite1, irh ) * w01_01 * wuc  + &
              cn_eh2ow(ip1, itp , iuc1, ite1, irh1) * w01_00 * wuc  + &
              cn_eh2ow(ip1, itp1, iuc , ite , irh ) * w00_11 * wuc1 + &
              cn_eh2ow(ip1, itp1, iuc , ite , irh1) * w00_10 * wuc1 + &
              cn_eh2ow(ip1, itp1, iuc , ite1, irh ) * w00_01 * wuc1 + &
              cn_eh2ow(ip1, itp1, iuc , ite1, irh1) * w00_00 * wuc1 + &
              cn_eh2ow(ip1, itp1, iuc1, ite , irh ) * w00_11 * wuc  + &
              cn_eh2ow(ip1, itp1, iuc1, ite , irh1) * w00_10 * wuc  + &
              cn_eh2ow(ip1, itp1, iuc1, ite1, irh ) * w00_01 * wuc  + &
              cn_eh2ow(ip1, itp1, iuc1, ite1, irh1) * w00_00 * wuc 
         emis(i,ib) = min(max(fe * (1.0 - l_star * c_star * &
                              aer_trn_ttl(i,k1,1,ib)), &
                          0.0_r8), 1.0_r8) 



         if (uvar < min_u_h2o) then
            uscl = uvar / min_u_h2o
            emis(i,ib) = emis(i,ib) * uscl
         endif

                      



         h2oems(i,k1) = emis(i,1)+emis(i,2)

      end do




      do i=1,ncol
         term7(i,1) = coefj(1,1) + coefj(2,1)*dty(i)*(1.+c16*dty(i))
         term8(i,1) = coefk(1,1) + coefk(2,1)*dty(i)*(1.+c17*dty(i))
         term7(i,2) = coefj(1,2) + coefj(2,2)*dty(i)*(1.+c26*dty(i))
         term8(i,2) = coefk(1,2) + coefk(2,2)*dty(i)*(1.+c27*dty(i))
      end do
      do i=1,ncol



         k21(i) = term7(i,1) + term8(i,1)/ &
                 (1. + (c30 + c31*(dty(i)-10.)*(dty(i)-10.))*sqrt(u(i)))
         k22(i) = term7(i,2) + term8(i,2)/ &
                 (1. + (c28 + c29*(dty(i)-10.))*sqrt(u(i)))
         fwk    = fwcoef + fwc1/(1.+fwc2*u(i))
         tr1(i) = exp(-(k21(i)*(sqrt(u(i)) + fc1*fwk*u(i))))
         tr2(i) = exp(-(k22(i)*(sqrt(u(i)) + fc1*fwk*u(i))))
         tr1(i)=tr1(i)*aer_trn_ttl(i,k1,1,idx_LW_0650_0800) 

         tr2(i)=tr2(i)*aer_trn_ttl(i,k1,1,idx_LW_0500_0650) 

         tr3(i) = exp(-((coefh(1,1) + coefh(2,1)*dtx(i))*uc1(i)))
         tr4(i) = exp(-((coefh(1,2) + coefh(2,2)*dtx(i))*uc1(i)))
         tr7(i) = tr1(i)*tr3(i)
         tr8(i) = tr2(i)*tr4(i)
         troco2(i,k1) = 0.65*tr7(i) + 0.35*tr8(i)
         th2o(i) = tr8(i)
      end do



      do i=1,ncol
         t1i    = exp(-480./co2t(i,k1))
         sqti   = sqrt(co2t(i,k1))
         rsqti  = 1./sqti
         et     = t1i
         et2    = et*et
         et4    = et2*et2
         omet   = 1. - 1.5*et2
         f1co2  = 899.70*omet*(1. + 1.94774*et + 4.73486*et2)*rsqti
         sqwp   = sqrt(plco2(i,k1))
         f1sqwp = f1co2*sqwp
         t1co2  = 1./(1. + 245.18*omet*sqwp*rsqti)
         oneme  = 1. - et2
         alphat = oneme**3*rsqti
         wco2   = 2.5221*co2vmr*pnm(i,k1)*rga
         u7     = 4.9411e4*alphat*et2*wco2
         u8     = 3.9744e4*alphat*et4*wco2
         u9     = 1.0447e5*alphat*et4*et2*wco2
         u13    = 2.8388e3*alphat*et4*wco2

         tpath  = co2t(i,k1)
         tlocal = tplnke(i)
         tcrfac = sqrt((tlocal*r250)*(tpath*r300))
         pi     = pnm(i,k1)*rsslp + 2.*dpfco2*tcrfac
         posqt  = pi/(2.*sqti)
         rbeta7 =  1./( 5.3288*posqt)
         rbeta8 = 1./ (10.6576*posqt)
         rbeta9 = rbeta7
         rbeta13= rbeta9
         f2co2  = (u7/sqrt(4. + u7*(1. + rbeta7))) + &
                  (u8/sqrt(4. + u8*(1. + rbeta8))) + &
                  (u9/sqrt(4. + u9*(1. + rbeta9)))
         f3co2  = u13/sqrt(4. + u13*(1. + rbeta13))
         tmp1   = log(1. + f1sqwp)
         tmp2   = log(1. +  f2co2)
         tmp3   = log(1. +  f3co2)
         absbnd = (tmp1 + 2.*t1co2*tmp2 + 2.*tmp3)*sqti
         tco2(i)=1.0/(1.0+10.0*(u7/sqrt(4. + u7*(1. + rbeta7))))
         co2ems(i,k1)  = troco2(i,k1)*absbnd*co2plk(i)
         ex     = exp(960./tint(i,k1))
         exm1sq = (ex - 1.)**2
         co2em(i,k1) = 1.2e11*ex/(tint(i,k1)*tint4(i,k1)*exm1sq)
      end do



      do i=1,ncol
         h2otr(i,k1) = exp(-12.*s2c(i,k1))
          h2otr(i,k1)=h2otr(i,k1)*aer_trn_ttl(i,k1,1,idx_LW_1000_1200)
         te          = (co2t(i,k1)/293.)**.7
         u1          = 18.29*plos(i,k1)/te
         u2          = .5649*plos(i,k1)/te
         phat        = plos(i,k1)/plol(i,k1)
         tlocal      = tplnke(i)
         tcrfac      = sqrt(tlocal*r250)*te
         beta        = (1./.3205)*((1./phat) + (dpfo3*tcrfac))
         realnu      = (1./beta)*te
         o3bndi      = 74.*te*(tplnke(i)/375.)*log(1. + fo3(u1,realnu) + fo3(u2,realnu))
         o3ems(i,k1) = dbvtt(i)*h2otr(i,k1)*o3bndi
         to3(i)=1.0/(1. + 0.1*fo3(u1,realnu) + 0.1*fo3(u2,realnu))
      end do



      call trcems(lchnk   ,ncol    ,pcols, pverp,               &
                  k1      ,co2t    ,pnm     ,ucfc11  ,ucfc12  , &
                  un2o0   ,un2o1   ,bn2o0   ,bn2o1   ,uch4    , &
                  bch4    ,uco211  ,uco212  ,uco213  ,uco221  , &
                  uco222  ,uco223  ,uptype  ,w       ,s2c     , &
                  u       ,emplnk  ,th2o    ,tco2    ,to3     , &
                  emstrc  , &
                  aer_trn_ttl)



      do i=1,ncol
         emstot(i,k1) = h2oems(i,k1) + co2ems(i,k1) + o3ems(i,k1)  &
                        + emstrc(i,k1)
      end do
   end do 

   return
end subroutine radems

subroutine radtpl(lchnk   ,ncol    ,pcols, pver, pverp,                 &
                  tnm     ,lwupcgs ,qnm     ,pnm     ,plco2   ,plh2o   , &
                  tplnka  ,s2c     ,tcg     ,w       ,tplnke  , &
                  tint    ,tint4   ,tlayr   ,tlayr4  ,pmln    , &
                  piln    ,plh2ob  ,wb      )

















   integer, intent(in) :: lchnk                 
   integer, intent(in) :: ncol                  
   integer, intent(in) :: pcols, pver, pverp

   real(r8), intent(in) :: tnm(pcols,pver)      
   real(r8), intent(in) :: lwupcgs(pcols)       
   real(r8), intent(in) :: qnm(pcols,pver)      
   real(r8), intent(in) :: pnm(pcols,pverp)     
   real(r8), intent(in) :: pmln(pcols,pver)     
   real(r8), intent(in) :: piln(pcols,pverp)    



   real(r8), intent(out) :: plco2(pcols,pverp)   
   real(r8), intent(out) :: plh2o(pcols,pverp)   
   real(r8), intent(out) :: tplnka(pcols,pverp)  
   real(r8), intent(out) :: s2c(pcols,pverp)     
   real(r8), intent(out) :: tcg(pcols,pverp)     
   real(r8), intent(out) :: w(pcols,pverp)       
   real(r8), intent(out) :: tplnke(pcols)        
   real(r8), intent(out) :: tint(pcols,pverp)    
   real(r8), intent(out) :: tint4(pcols,pverp)   
   real(r8), intent(out) :: tlayr(pcols,pverp)   
   real(r8), intent(out) :: tlayr4(pcols,pverp)  
   real(r8), intent(out) :: plh2ob(nbands,pcols,pverp)
                                                      
                                                      
   real(r8), intent(out) :: wb(nbands,pcols,pverp)    
                                                      
                                                      




   integer i                 
   integer k                 
   integer kp1               

   real(r8) repsil               
   real(r8) dy                   
   real(r8) dpnm                 
   real(r8) dpnmsq               
   real(r8) dw                   
   real(r8) dplh2o               
   real(r8) cpwpl                



   repsil = 1./epsilo



   cpwpl = amco2/amd * 0.5/(gravit*p0)
   do i=1,ncol
      plh2o(i,ntoplw)  = rgsslp*qnm(i,ntoplw)*pnm(i,ntoplw)*pnm(i,ntoplw)
      plco2(i,ntoplw)  = co2vmr*cpwpl*pnm(i,ntoplw)*pnm(i,ntoplw)
   end do
   do k=ntoplw,pver
      do i=1,ncol
         plh2o(i,k+1)  = plh2o(i,k) + rgsslp* &
                         (pnm(i,k+1)**2 - pnm(i,k)**2)*qnm(i,k)
         plco2(i,k+1)  = co2vmr*cpwpl*pnm(i,k+1)**2
      end do
   end do







   do i=1,ncol
      tint4(i,pverp)   = lwupcgs(i)/stebol
      tint(i,pverp)    = sqrt(sqrt(tint4(i,pverp)))
      tplnka(i,ntoplw) = tnm(i,ntoplw)
      tint(i,ntoplw)   = tplnka(i,ntoplw)
      tlayr4(i,ntoplw) = tplnka(i,ntoplw)**4
      tint4(i,ntoplw)  = tlayr4(i,ntoplw)
   end do




   do k=ntoplw+1,pver
      do i=1,ncol
         dy = (piln(i,k) - pmln(i,k))/(pmln(i,k-1) - pmln(i,k))
         tint(i,k)  = tnm(i,k) - dy*(tnm(i,k)-tnm(i,k-1))
         tint4(i,k) = tint(i,k)**4
      end do
   end do






   do k=ntoplw+1,pverp
      do i=1,ncol
         tlayr(i,k)  = tnm(i,k-1)
         tlayr4(i,k) = tlayr(i,k)**4
         tplnka(i,k) = .5*(tint(i,k) + tint(i,k-1))
      end do
   end do




   do i=1,ncol
      tplnke(i)       = tplnka(i,ntoplw)
      tlayr(i,ntoplw) = tint(i,ntoplw)
   end do



   do i=1,ncol



      tcg(i,ntoplw) = rga*qnm(i,ntoplw)*pnm(i,ntoplw)*tnm(i,ntoplw)
      w(i,ntoplw)   = sslp * (plh2o(i,ntoplw)*2.) / pnm(i,ntoplw)



      wb(1,i,ntoplw) = w(i,ntoplw) * phi(tnm(i,ntoplw),1)
      wb(2,i,ntoplw) = w(i,ntoplw) * phi(tnm(i,ntoplw),2)



      plh2ob(1,i,ntoplw) = plh2o(i,ntoplw) * psi(tnm(i,ntoplw),1)
      plh2ob(2,i,ntoplw) = plh2o(i,ntoplw) * psi(tnm(i,ntoplw),2)

      s2c(i,ntoplw) = plh2o(i,ntoplw)*fh2oself(tnm(i,ntoplw))*qnm(i,ntoplw)*repsil
   end do

   do k=ntoplw,pver
      do i=1,ncol
         dpnm       = pnm(i,k+1) - pnm(i,k)
         dpnmsq     = pnm(i,k+1)**2 - pnm(i,k)**2
         dw         = rga*qnm(i,k)*dpnm
         kp1        = k+1
         w(i,kp1)   = w(i,k) + dw



         wb(1,i,kp1) = wb(1,i,k) + dw * phi(tnm(i,k),1)
         wb(2,i,kp1) = wb(2,i,k) + dw * phi(tnm(i,k),2)



         dplh2o = plh2o(i,kp1) - plh2o(i,k)

         plh2ob(1,i,kp1) = plh2ob(1,i,k) + dplh2o * psi(tnm(i,k),1)
         plh2ob(2,i,kp1) = plh2ob(2,i,k) + dplh2o * psi(tnm(i,k),2)



         tcg(i,kp1) = tcg(i,k) + dw*tnm(i,k)
         s2c(i,kp1) = s2c(i,k) + rgsslp*dpnmsq*qnm(i,k)* &
                      fh2oself(tnm(i,k))*qnm(i,k)*repsil
      end do
   end do

   return
end subroutine radtpl


subroutine radclwmx(lchnk   ,ncol    ,pcols, pver, pverp,         &
                    lwupcgs ,tnm     ,qnm     ,o3vmr   , &
                    pmid    ,pint    ,pmln    ,piln    ,          &
                             n2o     ,ch4     ,cfc11   ,cfc12   , &
                    cld     ,emis    ,pmxrgn  ,nmxrgn  ,qrl     , &
                    doabsems, abstot, absnxt, emstot,             &
                    flns    ,flnt    ,flnsc   ,flntc   ,flwds   , &
                    flut    ,flutc   , &
                    flup    ,flupc   ,fldn    ,fldnc   ,          &
                    aer_mass)


























   implicit none

   integer pverp2,pverp3,pverp4


   real(r8) cldmin
   parameter (cldmin = 1.0d-80)






   integer, intent(in) :: lchnk                 
   integer, intent(in) :: pcols, pver, pverp
   integer, intent(in) :: ncol                  




   integer, intent(in) :: nmxrgn(pcols)         
   logical, intent(in) :: doabsems

   real(r8), intent(in) :: pmxrgn(pcols,pverp)  
   real(r8), intent(in) :: lwupcgs(pcols)       



   real(r8), intent(in) :: tnm(pcols,pver)      
   real(r8), intent(in) :: qnm(pcols,pver)      
   real(r8), intent(in) :: o3vmr(pcols,pver)    
   real(r8), intent(in) :: pmid(pcols,pver)     
   real(r8), intent(in) :: pint(pcols,pverp)    
   real(r8), intent(in) :: pmln(pcols,pver)     
   real(r8), intent(in) :: piln(pcols,pverp)    
   real(r8), intent(in) :: n2o(pcols,pver)      
   real(r8), intent(in) :: ch4(pcols,pver)      
   real(r8), intent(in) :: cfc11(pcols,pver)    
   real(r8), intent(in) :: cfc12(pcols,pver)    
   real(r8), intent(in) :: cld(pcols,pver)      
   real(r8), intent(in) :: emis(pcols,pver)     
   real(r8), intent(in) :: aer_mass(pcols,pver) 




   real(r8), intent(out) :: qrl(pcols,pver)      
   real(r8), intent(out) :: flns(pcols)          
   real(r8), intent(out) :: flnt(pcols)          
   real(r8), intent(out) :: flut(pcols)          
   real(r8), intent(out) :: flnsc(pcols)         
   real(r8), intent(out) :: flntc(pcols)         
   real(r8), intent(out) :: flutc(pcols)         
   real(r8), intent(out) :: flwds(pcols)         

   real(r8), intent(out) :: flup(pcols,pverp)      
   real(r8), intent(out) :: flupc(pcols,pverp)     
   real(r8), intent(out) :: fldn(pcols,pverp)      
   real(r8), intent(out) :: fldnc(pcols,pverp)     

   real(r8), intent(inout) :: abstot(pcols,pverp,pverp) 
   real(r8), intent(inout) :: absnxt(pcols,pver,4)      
   real(r8), intent(inout) :: emstot(pcols,pverp)     



   integer i                 
   integer ilon              
   integer ii                
   integer iimx              
   integer k                 
   integer k1                
   integer k2                
   integer k3                
   integer km                
   integer km1               
   integer km3               
   integer km4               
   integer irgn              
   integer l                 
   integer l1                
   integer n                 


   real(r8) :: plco2(pcols,pverp)   
   real(r8) :: plh2o(pcols,pverp)   
   real(r8) tmp(pcols)           
   real(r8) tmp2(pcols)          
   real(r8) absbt(pcols)         
   real(r8) plol(pcols,pverp)    
   real(r8) plos(pcols,pverp)    
   real(r8) aer_mpp(pcols,pverp) 
   real(r8) co2em(pcols,pverp)   
   real(r8) co2eml(pcols,pver)   
   real(r8) delt(pcols)          
   real(r8) delt1(pcols)         
   real(r8) bk1(pcols)           
   real(r8) bk2(pcols)           
   real(r8) cldp(pcols,pverp)    
   real(r8) ful(pcols,pverp)     
   real(r8) fsul(pcols,pverp)    
   real(r8) fdl(pcols,pverp)     
   real(r8) fsdl(pcols,pverp)    
   real(r8) fclb4(pcols,-1:pver)    
   real(r8) fclt4(pcols,0:pver)    
   real(r8) s(pcols,pverp,pverp) 
   real(r8) tplnka(pcols,pverp)  
   real(r8) s2c(pcols,pverp)     
   real(r8) tcg(pcols,pverp)     
   real(r8) w(pcols,pverp)       
   real(r8) tplnke(pcols)        
   real(r8) h2otr(pcols,pverp)   
   real(r8) co2t(pcols,pverp)    
   real(r8) tint(pcols,pverp)    
   real(r8) tint4(pcols,pverp)   
   real(r8) tlayr(pcols,pverp)   
   real(r8) tlayr4(pcols,pverp)  
   real(r8) plh2ob(nbands,pcols,pverp)
                                      
                                      
   real(r8) wb(nbands,pcols,pverp)    
                                      
                                      

   real(r8) cld0                 
   real(r8) cld1                 
   real(r8) emx(0:pverp)         
   real(r8) emx0                 
   real(r8) trans                
   real(r8) asort(pver)          
   real(r8) atmp                 
   real(r8) maxcld(pcols)        

   integer indx(pcols)       

   integer indxmx(pcols,pverp)

   integer nrgn(pcols)       
   integer npts              
   integer ncolmx(pverp)     
   integer kx1(pcols,pverp)  
   integer kx2(pcols,0:pverp)
   integer kxs(0:pverp,pcols,pverp)

   integer nxs(pcols,pverp)  
   integer nxsk              
   integer ksort(0:pverp)    

   integer ktmp              


  real(r8) aer_trn_ttl(pcols,pverp,pverp,bnd_nbr_LW) 











   real(r8) ucfc11(pcols,pverp)  
   real(r8) ucfc12(pcols,pverp)  
   real(r8) un2o0(pcols,pverp)   
   real(r8) un2o1(pcols,pverp)   
   real(r8) uch4(pcols,pverp)    
   real(r8) uco211(pcols,pverp)  
   real(r8) uco212(pcols,pverp)  
   real(r8) uco213(pcols,pverp)  
   real(r8) uco221(pcols,pverp)  
   real(r8) uco222(pcols,pverp)  
   real(r8) uco223(pcols,pverp)  
   real(r8) bn2o0(pcols,pverp)   
   real(r8) bn2o1(pcols,pverp)   
   real(r8) bch4(pcols,pverp)    
   real(r8) uptype(pcols,pverp)  
   real(r8) abplnk1(14,pcols,pverp)  
   real(r8) abplnk2(14,pcols,pverp)  





   pverp2=pver+2
   pverp3=pver+3
   pverp4=pver+4









  call aer_pth(aer_mass, aer_mpp, ncol, pcols, pver, pverp)





   call radtpl(lchnk   ,ncol    ,pcols, pver, pverp,                  &
               tnm     ,lwupcgs ,qnm     ,pint    ,plco2   ,plh2o   , &
               tplnka  ,s2c     ,tcg     ,w       ,tplnke  , &
               tint    ,tint4   ,tlayr   ,tlayr4  ,pmln    , &
               piln    ,plh2ob  ,wb      )
   if (doabsems) then



      call radoz2(lchnk, ncol, pcols, pver, pverp, o3vmr   ,pint    ,plol    ,plos, ntoplw    )



      call trcpth(lchnk   ,ncol    ,pcols, pver, pverp,         &
                  tnm     ,pint    ,cfc11   ,cfc12   ,n2o     , &
                  ch4     ,qnm     ,ucfc11  ,ucfc12  ,un2o0   , &
                  un2o1   ,uch4    ,uco211  ,uco212  ,uco213  , &
                  uco221  ,uco222  ,uco223  ,bn2o0   ,bn2o1   , &
                  bch4    ,uptype  )


      call aer_trn(aer_mpp, aer_trn_ttl, pcols, pver, pverp)





      call radems(lchnk   ,ncol    ,pcols, pver, pverp,         &
                  s2c     ,tcg     ,w       ,tplnke  ,plh2o   , &
                  pint    ,plco2   ,tint    ,tint4   ,tlayr   , &
                  tlayr4  ,plol    ,plos    ,ucfc11  ,ucfc12  , &
                  un2o0   ,un2o1   ,uch4    ,uco211  ,uco212  , &
                  uco213  ,uco221  ,uco222  ,uco223  ,uptype  , &
                  bn2o0   ,bn2o1   ,bch4    ,co2em   ,co2eml  , &
                  co2t    ,h2otr   ,abplnk1 ,abplnk2 ,emstot  , &
                  plh2ob  ,wb      , &
                  aer_trn_ttl)



      call radabs(lchnk   ,ncol    ,pcols, pver, pverp,         &
                  pmid    ,pint    ,co2em   ,co2eml  ,tplnka  , &
                  s2c     ,tcg     ,w       ,h2otr   ,plco2   , &
                  plh2o   ,co2t    ,tint    ,tlayr   ,plol    , &
                  plos    ,pmln    ,piln    ,ucfc11  ,ucfc12  , &
                  un2o0   ,un2o1   ,uch4    ,uco211  ,uco212  , &
                  uco213  ,uco221  ,uco222  ,uco223  ,uptype  , &
                  bn2o0   ,bn2o1   ,bch4    ,abplnk1 ,abplnk2 , &
                  abstot  ,absnxt  ,plh2ob  ,wb      , &
                  aer_mpp ,aer_trn_ttl)
   end if










   do i=1,ncol
      delt(i) = tint4(i,pver) - tlayr4(i,pverp)
      delt1(i) = tlayr4(i,pverp) - tint4(i,pverp)
      s(i,pverp,pverp) = stebol*(delt1(i)*absnxt(i,pver,1) + delt (i)*absnxt(i,pver,4))
      s(i,pver,pverp)  = stebol*(delt (i)*absnxt(i,pver,2) + delt1(i)*absnxt(i,pver,3))
   end do
   do k=ntoplw,pver-1
      do i=1,ncol
         bk2(i) = (abstot(i,k,pver) + abstot(i,k,pverp))*0.5
         bk1(i) = bk2(i)
         s(i,k,pverp) = stebol*(bk2(i)*delt(i) + bk1(i)*delt1(i))
      end do
   end do



   do km=pver,ntoplw+1,-1
      do i=1,ncol
         delt(i)  = tint4(i,km-1) - tlayr4(i,km)
         delt1(i) = tlayr4(i,km) - tint4(i,km)
      end do
      do k=pverp,ntoplw,-1
         if (k == km) then
            do i=1,ncol
               bk2(i) = absnxt(i,km-1,4)
               bk1(i) = absnxt(i,km-1,1)
            end do
         else if (k == km-1) then
            do i=1,ncol
               bk2(i) = absnxt(i,km-1,2)
               bk1(i) = absnxt(i,km-1,3)
            end do
         else
            do i=1,ncol
               bk2(i) = (abstot(i,k,km-1) + abstot(i,k,km))*0.5
               bk1(i) = bk2(i)
            end do
         end if
         do i=1,ncol
            s(i,k,km) = s(i,k,km+1) + stebol*(bk2(i)*delt(i) + bk1(i)*delt1(i))
         end do
      end do
   end do



   do i=1,ncol
      fsul(i,pverp) = lwupcgs(i)
   end do




   do i=1,ncol
      tmp(i) = fsul(i,pverp) - stebol*tint4(i,pverp)
      fsul(i,ntoplw) = fsul(i,pverp) - abstot(i,ntoplw,pverp)*tmp(i) + s(i,ntoplw,ntoplw+1)
      fsdl(i,ntoplw) = stebol*(tplnke(i)**4)*emstot(i,ntoplw)
   end do



   do k=ntoplw+1,pver
      do i=1,ncol
         fsul(i,k) = fsul(i,pverp) - abstot(i,k,pverp)*tmp(i) + s(i,k,k+1)
         fsdl(i,k) = stebol*(tplnke(i)**4)*emstot(i,k) - (s(i,k,ntoplw+1) - s(i,k,k+1))
      end do
   end do




   do i=1,ncol
      absbt(i) = stebol*(tplnke(i)**4)*emstot(i,pverp)
      fsdl(i,pverp) = absbt(i) - s(i,pverp,ntoplw+1)
   end do























   cldp(:ncol,ntoplw:pver) = cld(:ncol,ntoplw:pver)
   cldp(:ncol,pverp) = 0.0






   maxcld(1:ncol) = maxval(cldp(1:ncol,ntoplw:pver),dim=2)

   npts = 0
   do i=1,ncol
      if (maxcld(i) < cldmin) then
         npts = npts + 1
         indx(npts) = i
      end if
   end do

   do ii = 1, npts
      i = indx(ii)
      do k = ntoplw, pverp
         fdl(i,k) = fsdl(i,k)
         ful(i,k) = fsul(i,k)
      end do
   end do



   npts = 0
   do i=1,ncol
      if (maxcld(i) >= cldmin) then
         npts = npts + 1
         indx(npts) = i
      end if
   end do




   do ii = 1, npts
      i = indx(ii)
      fdl(i,ntoplw) = fsdl(i,ntoplw)
      fdl(i,pverp)  = 0.0
      ful(i,ntoplw) = 0.0
      ful(i,pverp)  = fsul(i,pverp)
      do k = ntoplw+1, pver
         fdl(i,k) = 0.0
         ful(i,k) = 0.0
      end do



      do k = ntoplw, pver
         fclt4(i,k-1) = stebol*tint4(i,k)
         fclb4(i,k-1) = stebol*tint4(i,k+1)
      enddo
      fclb4(i,ntoplw-2) =  stebol*tint4(i,ntoplw)
      fclt4(i,pver)     = stebol*tint4(i,pverp)



      do irgn = 0, nmxrgn(i)
         kx2(i,irgn) = ntoplw-1
      end do
      nrgn(i) = 0
   end do




   do ii = 1, npts
      ilon = indx(ii)




      do irgn = 1, nmxrgn(ilon)



         n = 0
         if (kx2(ilon,irgn-1) < pver) then
            nrgn(ilon) = irgn
            k1 = kx2(ilon,irgn-1)+1
            kx1(ilon,irgn) = k1
            kx2(ilon,irgn) = 0
            do k2 = pver, k1, -1
               if (pmid(ilon,k2) <= pmxrgn(ilon,irgn)) then
                  kx2(ilon,irgn) = k2
                  exit
               end if
            end do



            do k = k1, k2
               if (cldp(ilon,k) >= cldmin) then
                  n = n+1
                  indxmx(n,irgn) = ilon
                  exit
               endif
            end do
         endif
         ncolmx(irgn) = n







         do iimx = 1, ncolmx(irgn)
            i = indxmx(iimx,irgn)



            n = 0
            do k = kx1(i,irgn),kx2(i,irgn)
               if (cldp(i,k) >= cldmin) then
                  n = n+1
                  ksort(n) = k




                  asort(n) = 1.0-cldp(i,k)
               end if
            end do
            nxs(i,irgn) = n





            if (nxs(i,irgn) == 2) then
               if (asort(2) < asort(1)) then
                  ktmp = ksort(1)
                  ksort(1) = ksort(2)
                  ksort(2) = ktmp

                  atmp = asort(1)
                  asort(1) = asort(2)
                  asort(2) = atmp
               endif
            else if (nxs(i,irgn) >= 3) then
               call sortarray(nxs(i,irgn),asort,ksort(1:))
            endif

            do l = 1, nxs(i,irgn)
               kxs(l,i,irgn) = ksort(l)
            end do



         end do



      end do





      do irgn = 1, nmxrgn(ilon)



         iimx = 1
         if (ilon < indxmx(iimx,irgn) .and. irgn <= nrgn(ilon)) then







            k1 = kx1(ilon,irgn)
            do km1 = ntoplw-2, k1-2
               km4 = km1+3
               k2 = k1
               k3 = k2+1
               tmp(ilon) = s(ilon,k2,min(k3,pverp))*min(1,pverp2-k3)
               emx0 = (fdl(ilon,k1)-fsdl(ilon,k1))/ &
                      ((fclb4(ilon,km1)-s(ilon,k2,km4)+tmp(ilon))- fsdl(ilon,k1))
               if (emx0 >= 0.0 .and. emx0 <= 1.0) exit
            end do
            km1 = min(km1,k1-2)
            do k2 = kx1(ilon,irgn)+1, kx2(ilon,irgn)+1
               k3 = k2+1
               tmp(ilon) = s(ilon,k2,min(k3,pverp))*min(1,pverp2-k3)
               fdl(ilon,k2) = (1.0-emx0)*fsdl(ilon,k2) + &
                               emx0*(fclb4(ilon,km1)-s(ilon,k2,km4)+tmp(ilon))
            end do
         else if (ilon==indxmx(iimx,irgn) .and. iimx<=ncolmx(irgn)) then
            iimx = iimx+1
         end if



         do iimx = 1, ncolmx(irgn)
            i = indxmx(iimx,irgn)








            k1 = kx1(i,irgn)
            do km1 = ntoplw-2,k1-2
               km4 = km1+3
               k2 = k1
               k3 = k2 + 1
               tmp(i) = s(i,k2,min(k3,pverp))*min(1,pverp2-k3)
               tmp2(i) = s(i,k2,min(km4,pverp))*min(1,pverp2-km4)
               emx0 = (fdl(i,k1)-fsdl(i,k1))/((fclb4(i,km1)-tmp2(i)+tmp(i))-fsdl(i,k1))
               if (emx0 >= 0.0 .and. emx0 <= 1.0) exit
            end do
            km1 = min(km1,k1-2)
            ksort(0) = km1 + 1



            nxsk = 0
            do k = kx1(i,irgn), kx2(i,irgn)





               if (nxsk < nxs(i,irgn)) then
                  nxsk = 0
                  do l = 1, nxs(i,irgn)
                     k1 = kxs(l,i,irgn)
                     if (k >= k1) then
                        nxsk = nxsk + 1
                        ksort(nxsk) = k1
                     endif
                  end do
               endif



               ksort(nxsk+1) = pverp



               do l = 1, nxsk
                  emx(l) = emis(i,ksort(l))
               end do



               emx(0) = emx0



               cld0 = 1.0



               k2 = k+1
               k3 = k2+1
               tmp(i) = s(i,k2,min(k3,pverp))*min(1,pverp2-k3)



               do l = 1, nxsk+1



                  cld1 = cldp(i,ksort(l))*min(1,nxsk+1-l)
                  if (cld0 /= cld1) then
                     fdl(i,k2) = fdl(i,k2)+(cld0-cld1)*fsdl(i,k2)
                     do l1 = 0, l - 1
                        km1 = ksort(l1)-1
                        km4 = km1+3
                        tmp2(i) = s(i,k2,min(km4,pverp))* min(1,pverp2-km4)
                        fdl(i,k2) = fdl(i,k2)+(cld0-cld1)*emx(l1)*(fclb4(i,km1)-tmp2(i)+tmp(i)- &
                                    fsdl(i,k2))
                     end do
                  endif
                  cld0 = cld1



                  if (l <= nxsk) then
                     k1 = ksort(l)
                     trans = 1.0-emis(i,k1)




                     do l1 = 0, nxsk
                        if (ksort(l1) < k1) then
                           emx(l1) = emx(l1)*trans
                        endif
                     end do
                  end if



               end do



            end do



         end do



      end do






      do irgn = nmxrgn(ilon), 1, -1



         iimx = 1
         if (ilon < indxmx(iimx,irgn) .and. irgn <= nrgn(ilon)) then









            k1 = kx2(ilon,irgn)+1
            if (k1 < pverp) then
               do km1 = pver-1,kx2(ilon,irgn),-1
                  km3 = km1+2
                  k2 = k1
                  k3 = k2+1
                  tmp(ilon) = s(ilon,k2,min(km3,pverp))* min(1,pverp2-km3)
                  emx0 = (ful(ilon,k1)-fsul(ilon,k1))/ &
                         ((fclt4(ilon,km1)+s(ilon,k2,k3)-tmp(ilon))- fsul(ilon,k1))
                  if (emx0 >= 0.0 .and. emx0 <= 1.0) exit
               end do
               km1 = max(km1,kx2(ilon,irgn))
            else
               km1 = k1-1
               km3 = km1+2
               emx0 = 1.0
            endif

            do k2 = kx1(ilon,irgn), kx2(ilon,irgn)
               k3 = k2+1



               tmp(ilon) = s(ilon,k2,min(km3,pverp))* min(1,pverp2-km3)
               ful(ilon,k2) =(1.0-emx0)*fsul(ilon,k2) + emx0* &
                             (fclt4(ilon,km1)+s(ilon,k2,k3)-tmp(ilon))
            end do
         else if (ilon==indxmx(iimx,irgn) .and. iimx<=ncolmx(irgn)) then
            iimx = iimx+1
         end if



         do iimx = 1, ncolmx(irgn)
            i = indxmx(iimx,irgn)










            k1 = kx2(i,irgn)+1
            if (k1 < pverp) then
               do km1 = pver-1,kx2(i,irgn),-1
                  km3 = km1+2
                  k2 = k1
                  k3 = k2+1
                  tmp(i) = s(i,k2,min(km3,pverp))*min(1,pverp2-km3)
                  emx0 = (ful(i,k1)-fsul(i,k1))/((fclt4(i,km1)+s(i,k2,k3)-tmp(i))-fsul(i,k1))
                  if (emx0 >= 0.0 .and. emx0 <= 1.0) exit
               end do
               km1 = max(km1,kx2(i,irgn))
            else
               emx0 = 1.0
               km1 = k1-1
            endif
            ksort(0) = km1 + 1




            nxsk = 0
            do k = kx2(i,irgn), kx1(i,irgn), -1





               if (nxsk < nxs(i,irgn)) then
                  nxsk = 0
                  do l = 1, nxs(i,irgn)
                     k1 = kxs(l,i,irgn)
                     if (k <= k1) then
                        nxsk = nxsk + 1
                        ksort(nxsk) = k1
                     endif
                  end do
               endif



               ksort(nxsk+1) = pverp



               do l = 1, nxsk
                  emx(l) = emis(i,ksort(l))
               end do



               emx(0) = emx0



               cld0 = 1.0



               k2 = k
               k3 = k2+1



               do l = 1, nxsk+1



                  cld1 = cldp(i,ksort(l))*min(1,nxsk+1-l)
                  if (cld0 /= cld1) then
                     ful(i,k2) = ful(i,k2)+(cld0-cld1)*fsul(i,k2)
                     do l1 = 0, l - 1
                        km1 = ksort(l1)-1
                        km3 = km1+2



                        tmp(i) = s(i,k2,min(km3,pverp))* min(1,pverp2-km3)
                        ful(i,k2) = ful(i,k2)+(cld0-cld1)*emx(l1)* &
                                   (fclt4(i,km1)+s(i,k2,k3)-tmp(i)- fsul(i,k2))
                     end do
                  endif
                  cld0 = cld1



                  if (l <= nxsk) then
                     k1 = ksort(l)
                     trans = 1.0-emis(i,k1)




                     do l1 = 0, nxsk
                        if (ksort(l1) > k1) then
                           emx(l1) = emx(l1)*trans
                        endif
                     end do
                  end if



               end do



            end do



         end do



      end do



   end do






   do i=1,ncol
      flwds(i) = fdl (i,pverp )
      flns(i)  = ful (i,pverp ) - fdl (i,pverp )
      flnsc(i) = fsul(i,pverp ) - fsdl(i,pverp )
      flnt(i)  = ful (i,ntoplw) - fdl (i,ntoplw)
      flntc(i) = fsul(i,ntoplw) - fsdl(i,ntoplw)
      flut(i)  = ful (i,ntoplw)
      flutc(i) = fsul(i,ntoplw)
   end do



   do k=ntoplw,pver
      do i=1,ncol
         qrl(i,k) = (ful(i,k) - fdl(i,k) - ful(i,k+1) + fdl(i,k+1))* &
                     1.E-4*gravit/((pint(i,k) - pint(i,k+1)))
      end do
   end do

   if ( ntoplw > 1 )then
      qrl(:ncol,:ntoplw-1) = 0.
   end if



   do k=ntoplw,pverp
      do i=1,ncol
        flup(i,k)  = ful(i,k)
        flupc(i,k) = fsul(i,k)
        fldn(i,k)  = fdl(i,k)
        fldnc(i,k) = fsdl(i,k)
      end do
   end do

   if ( ntoplw > 1 )then
      flup(:ncol,:ntoplw-1) = 0.
      flupc(:ncol,:ntoplw-1) = 0.
      fldn(:ncol,:ntoplw-1) = 0.
      fldnc(:ncol,:ntoplw-1) = 0.
   end if

   return
end subroutine radclwmx

subroutine radcswmx(jj, lchnk   ,ncol    ,pcols, pver, pverp,         &
                    pint    ,pmid    ,h2ommr  ,rh      ,o3mmr   , &
                    aermmr  ,cld     ,cicewp  ,cliqwp  ,rel     , &

                    rei     ,tauxcl  ,tauxci  ,eccf    ,coszrs  ,scon    ,solin   ,solcon,  &
                    asdir   ,asdif   ,aldir   ,aldif   ,nmxrgn  , &
                    pmxrgn  ,qrs     ,fsnt    ,fsntc   ,fsntoa  , &
                    fsntoac ,fsnirtoa,fsnrtoac,fsnrtoaq,fsns    , &
                    fsnsc   ,fsdsc   ,fsds    ,sols    ,soll    , &
                    solsd   ,solld   ,frc_day ,                   &
                    fsup    ,fsupc   ,fsdn    ,fsdnc   ,          &
                    aertau  ,aerssa  ,aerasm  ,aerfwd             )






























































   implicit none

   integer nspint            
   integer naer_groups       

   parameter ( nspint = 19 )
   parameter ( naer_groups = 7 )    



































   real(r8) cldmin
   parameter (cldmin = 1.0e-80_r8)




   real(r8) areamin
   parameter (areamin = 0.01_r8)




   real(r8) cldeps
   parameter (cldeps = 0.0_r8)



   integer nconfgmax
   parameter (nconfgmax = 15)




   integer, intent(in) :: lchnk,jj             
   integer, intent(in) :: pcols, pver, pverp
   integer, intent(in) :: ncol              

   real(r8), intent(in) :: pmid(pcols,pver) 
   real(r8), intent(in) :: pint(pcols,pverp) 
   real(r8), intent(in) :: h2ommr(pcols,pver) 
   real(r8), intent(in) :: o3mmr(pcols,pver) 
   real(r8), intent(in) :: aermmr(pcols,pver,naer_all) 
   real(r8), intent(in) :: rh(pcols,pver)   

   real(r8), intent(in) :: cld(pcols,pver)  
   real(r8), intent(in) :: cicewp(pcols,pver) 
   real(r8), intent(in) :: cliqwp(pcols,pver) 
   real(r8), intent(in) :: rel(pcols,pver)  
   real(r8), intent(in) :: rei(pcols,pver)  

   real(r8), intent(in) :: eccf             
   real, intent(in) :: solcon           
   real(r8), intent(in) :: coszrs(pcols)    
   real(r8), intent(in) :: asdir(pcols)     
   real(r8), intent(in) :: aldir(pcols)     
   real(r8), intent(in) :: asdif(pcols)     
   real(r8), intent(in) :: aldif(pcols)     

   real(r8), intent(in) :: scon             



   real(r8), intent(inout) :: pmxrgn(pcols,pverp) 




   integer, intent(inout) ::  nmxrgn(pcols)    




   real(r8), intent(out) :: solin(pcols)     
   real(r8), intent(out) :: qrs(pcols,pver)  
   real(r8), intent(out) :: fsns(pcols)      
   real(r8), intent(out) :: fsnt(pcols)      
   real(r8), intent(out) :: fsntoa(pcols)    
   real(r8), intent(out) :: fsds(pcols)      

   real(r8), intent(out) :: fsnsc(pcols)     
   real(r8), intent(out) :: fsdsc(pcols)     
   real(r8), intent(out) :: fsntc(pcols)     
   real(r8), intent(out) :: fsntoac(pcols)   
   real(r8), intent(out) :: sols(pcols)      
   real(r8), intent(out) :: soll(pcols)      
   real(r8), intent(out) :: solsd(pcols)     
   real(r8), intent(out) :: solld(pcols)     
   real(r8), intent(out) :: fsnirtoa(pcols)  
   real(r8), intent(out) :: fsnrtoac(pcols)  
   real(r8), intent(out) :: fsnrtoaq(pcols)  
   real(r8), intent(out) :: tauxcl(pcols,0:pver) 
   real(r8), intent(out) :: tauxci(pcols,0:pver) 


   real(r8), intent(out) :: fsup(pcols,pverp)      
   real(r8), intent(out) :: fsupc(pcols,pverp)     
   real(r8), intent(out) :: fsdn(pcols,pverp)      
   real(r8), intent(out) :: fsdnc(pcols,pverp)     

   real(r8) , intent(out) :: frc_day(pcols) 
   real(r8) :: aertau(pcols,nspint,naer_groups) 
   real(r8) :: aerssa(pcols,nspint,naer_groups) 
   real(r8) :: aerasm(pcols,nspint,naer_groups) 
   real(r8) :: aerfwd(pcols,nspint,naer_groups) 









   real(r8) asort(pverp)     
   real(r8) atmp             
   real(r8) cld0             
   real(r8) totwgt           



   real(r8) wgtv(nconfgmax)  

   real(r8) wstr(pverp,pverp) 



   real(r8) xexpt            
   real(r8) xrdnd            
   real(r8) xrupd            
   real(r8) xrups            
   real(r8) xtdnt            

   real(r8) xwgt             

   real(r8) yexpt            
   real(r8) yrdnd            
   real(r8) yrupd            
   real(r8) ytdnd            
   real(r8) ytupd            

   real(r8) zexpt            
   real(r8) zrdnd            
   real(r8) zrupd            
   real(r8) zrups            
   real(r8) ztdnt            

   logical new_term          
   logical region_found      

   integer ccon(0:pverp,nconfgmax)                                




   integer cstr(0:pverp,pverp)                                




   integer icond(0:pverp,nconfgmax)






   integer iconu(0:pverp,nconfgmax)






   integer iconfig           
   integer irgn              
   integer is0               
   integer is1               
   integer isn               
   integer istr(pverp+1)     
   integer istrtd(0:pverp,0:nconfgmax+1)




   integer istrtu(0:pverp,0:nconfgmax+1)




   integer j                 
   integer k1                
   integer k2                
   integer ksort(pverp)      
   integer ktmp              
   integer kx1(0:pverp)      
   integer kx2(0:pverp)      
   integer l                 
   integer l0                
   integer mrgn              
   integer mstr              
   integer n0                
   integer n1                
   integer nconfig           
   integer nconfigm          

   integer npasses           
   integer nrgn              

   integer nstr(pverp)       


   integer nuniq             
   integer nuniqd(0:pverp)   

   integer nuniqu(0:pverp)   

   integer nxs               
   integer ptr0(nconfgmax)   
   integer ptr1(nconfgmax)   
   integer ptrc(nconfgmax)   







   integer ns                
   integer i                 
   integer k                 
   integer km1               
   integer kp1               
   integer n                 
   integer ndayc             
   integer idayc(pcols)      
   integer indxsl            
   integer ksz               
   integer krh               
   integer kaer              
   real(r8) wrh              
   real(r8) :: rhtrunc       
                             
   real(r8) albdir(pcols,nspint) 
   real(r8) albdif(pcols,nspint) 

   real(r8) wgtint           





   real(r8) solflx           
   real(r8) sfltot           
   real(r8) totfld(0:pver)   
   real(r8) fswup(0:pverp)   
   real(r8) fswdn(0:pverp)   
   real(r8) fswupc(0:pverp)  
   real(r8) fswdnc(0:pverp)  





   real(r8) wcl(pcols,0:pver) 
   real(r8) gcl(pcols,0:pver) 
   real(r8) fcl(pcols,0:pver) 
   real(r8) wci(pcols,0:pver) 
   real(r8) gci(pcols,0:pver) 
   real(r8) fci(pcols,0:pver) 



  real(r8) usul(pcols,pver)   
  real(r8) ubg(pcols,pver)    
  real(r8) usslt(pcols,pver)  
  real(r8) ucphil(pcols,pver) 
  real(r8) ucphob(pcols,pver) 
  real(r8) ucb(pcols,pver)    
  real(r8) uvolc(pcols,pver) 
  real(r8) udst(ndstsz,pcols,pver) 




  real(r8) tau_sul             
  real(r8) tau_bg              
  real(r8) tau_sslt            
  real(r8) tau_cphil           
  real(r8) tau_cphob           
  real(r8) tau_cb              
  real(r8) tau_volc            
  real(r8) tau_dst(ndstsz)     
  real(r8) tau_dst_tot         
  real(r8) tau_tot             

  real(r8) tau_w_sul           
  real(r8) tau_w_bg            
  real(r8) tau_w_sslt          
  real(r8) tau_w_cphil         
  real(r8) tau_w_cphob         
  real(r8) tau_w_cb            
  real(r8) tau_w_volc          
  real(r8) tau_w_dst(ndstsz)   
  real(r8) tau_w_dst_tot       
  real(r8) tau_w_tot           

  real(r8) tau_w_g_sul         
  real(r8) tau_w_g_bg          
  real(r8) tau_w_g_sslt        
  real(r8) tau_w_g_cphil       
  real(r8) tau_w_g_cphob       
  real(r8) tau_w_g_cb          
  real(r8) tau_w_g_volc        
  real(r8) tau_w_g_dst(ndstsz) 
  real(r8) tau_w_g_dst_tot     
  real(r8) tau_w_g_tot         

  real(r8) f_sul               
  real(r8) f_bg                
  real(r8) f_sslt              
  real(r8) f_cphil             
  real(r8) f_cphob             
  real(r8) f_cb                
  real(r8) f_volc              
  real(r8) f_dst(ndstsz)       
  real(r8) f_dst_tot           
  real(r8) f_tot               

  real(r8) tau_w_f_sul         
  real(r8) tau_w_f_bg          
  real(r8) tau_w_f_sslt        
  real(r8) tau_w_f_cphil       
  real(r8) tau_w_f_cphob       
  real(r8) tau_w_f_cb          
  real(r8) tau_w_f_volc        
  real(r8) tau_w_f_dst(ndstsz) 
  real(r8) tau_w_f_dst_tot     
  real(r8) tau_w_f_tot         
  real(r8) w_dst_tot           
  real(r8) w_tot               
  real(r8) g_dst_tot           
  real(r8) g_tot               
  real(r8) ksuli               
  real(r8) ksslti              
  real(r8) kcphili             
  real(r8) wsuli               
  real(r8) wsslti              
  real(r8) wcphili             
  real(r8) gsuli               
  real(r8) gsslti              
  real(r8) gcphili             



   real(r8) tauxar(pcols,0:pver) 
   real(r8) wa(pcols,0:pver) 
   real(r8) ga(pcols,0:pver) 
   real(r8) fa(pcols,0:pver) 




   real(r8) pflx(pcols,0:pverp) 
   real(r8) zenfac(pcols)    
   real(r8) sqrco2           
   real(r8) tmp1             
   real(r8) tmp2             
   real(r8) pdel             
   real(r8) path             
   real(r8) ptop             
   real(r8) ptho2            
   real(r8) ptho3            
   real(r8) pthco2           
   real(r8) pthh2o           
   real(r8) h2ostr           
   real(r8) wavmid(nspint)   
   real(r8) trayoslp         
   real(r8) tmp1l            
   real(r8) tmp2l            
   real(r8) tmp3l            
   real(r8) tmp1i            
   real(r8) tmp2i            
   real(r8) tmp3i            
   real(r8) rdenom           
   real(r8) rdirexp          
   real(r8) tdnmexp          
   real(r8) psf(nspint)      




   real(r8) uh2o(pcols,0:pver) 
   real(r8) uo3(pcols,0:pver) 
   real(r8) uco2(pcols,0:pver) 
   real(r8) uo2(pcols,0:pver) 
   real(r8) uaer(pcols,0:pver) 



   real(r8) uth2o(pcols)     
   real(r8) uto3(pcols)      
   real(r8) utco2(pcols)     
   real(r8) uto2(pcols)      




   real(r8) rdir(nspint,pcols,0:pver) 
   real(r8) rdif(nspint,pcols,0:pver) 
   real(r8) tdir(nspint,pcols,0:pver) 
   real(r8) tdif(nspint,pcols,0:pver) 
   real(r8) explay(nspint,pcols,0:pver) 

   real(r8) rdirc(nspint,pcols,0:pver) 
   real(r8) rdifc(nspint,pcols,0:pver) 
   real(r8) tdirc(nspint,pcols,0:pver) 
   real(r8) tdifc(nspint,pcols,0:pver) 
   real(r8) explayc(nspint,pcols,0:pver) 

   real(r8) flxdiv           















   real(r8) exptdn(0:pverp,nconfgmax) 
   real(r8) rdndif(0:pverp,nconfgmax) 
   real(r8) rupdif(0:pverp,nconfgmax) 
   real(r8) rupdir(0:pverp,nconfgmax) 
   real(r8) tdntot(0:pverp,nconfgmax) 



   real(r8) exptdnc(0:pverp) 
   real(r8) rdndifc(0:pverp) 
   real(r8) rupdifc(0:pverp) 
   real(r8) rupdirc(0:pverp) 
   real(r8) tdntotc(0:pverp) 

   real(r8) fluxup(0:pverp)  
   real(r8) fluxdn(0:pverp)  
   real(r8) wexptdn          


   real(r8) abarli           
   real(r8) bbarli           
   real(r8) cbarli           
   real(r8) dbarli           
   real(r8) ebarli           
   real(r8) fbarli           

   real(r8) abarii           
   real(r8) bbarii           
   real(r8) cbarii           
   real(r8) dbarii           
   real(r8) ebarii           
   real(r8) fbarii           









   do i=1, ncol



      fsds(i)     = 0.0_r8

      fsnirtoa(i) = 0.0_r8
      fsnrtoac(i) = 0.0_r8
      fsnrtoaq(i) = 0.0_r8

      fsns(i)     = 0.0_r8
      fsnsc(i)    = 0.0_r8
      fsdsc(i)    = 0.0_r8

      fsnt(i)     = 0.0_r8
      fsntc(i)    = 0.0_r8
      fsntoa(i)   = 0.0_r8
      fsntoac(i)  = 0.0_r8

      solin(i)    = 0.0_r8

      sols(i)     = 0.0_r8
      soll(i)     = 0.0_r8
      solsd(i)    = 0.0_r8
      solld(i)    = 0.0_r8



         do k=1,pverp
            fsup(i,k)  = 0.0_r8
            fsupc(i,k) = 0.0_r8
            fsdn(i,k)  = 0.0_r8
            fsdnc(i,k) = 0.0_r8
            tauxcl(i,k-1) = 0.0_r8
            tauxci(i,k-1) = 0.0_r8
         end do

      do k=1, pver
         qrs(i,k) = 0.0_r8
      end do

      
      
      do kaer = 1, naer_groups
         do ns = 1, nspint
            frc_day(i) = 0.0_r8
            aertau(i,ns,kaer) = 0.0_r8
            aerssa(i,ns,kaer) = 0.0_r8
            aerasm(i,ns,kaer) = 0.0_r8
            aerfwd(i,ns,kaer) = 0.0_r8
         end do
      end do

   end do





   ndayc = 0
   do i=1,ncol
      if (coszrs(i) > 0.0_r8) then
         ndayc = ndayc + 1
         idayc(ndayc) = i
      end if
   end do



   if (ndayc == 0) return



   tmp1   = 0.5_r8/(gravit*sslp)
   tmp2   = delta/gravit
   sqrco2 = sqrt(co2mmr)

   do n=1,ndayc
      i=idayc(n)





         solin(i)  = solcon*coszrs(i)*1000.
         pflx(i,0) = 0._r8
         do k=1,pverp
            pflx(i,k) = pint(i,k)
         end do



         ptop      = pflx(i,1)
         ptho2     = o2mmr * ptop / gravit
         ptho3     = o3mmr(i,1) * ptop / gravit
         pthco2    = sqrco2 * (ptop / gravit)
         h2ostr    = sqrt( 1._r8 / h2ommr(i,1) )
         zenfac(i) = sqrt(coszrs(i))
         pthh2o    = ptop**2*tmp1 + (ptop*rga)* &
                    (h2ostr*zenfac(i)*delta)
         uh2o(i,0) = h2ommr(i,1)*pthh2o
         uco2(i,0) = zenfac(i)*pthco2
         uo2 (i,0) = zenfac(i)*ptho2
         uo3 (i,0) = ptho3
         uaer(i,0) = 0.0_r8
         do k=1,pver
            pdel      = pflx(i,k+1) - pflx(i,k)
            path      = pdel / gravit
            ptho2     = o2mmr * path
            ptho3     = o3mmr(i,k) * path
            pthco2    = sqrco2 * path
            h2ostr    = sqrt(1.0_r8/h2ommr(i,k))
            pthh2o    = (pflx(i,k+1)**2 - pflx(i,k)**2)*tmp1 + pdel*h2ostr*zenfac(i)*tmp2
            uh2o(i,k) = h2ommr(i,k)*pthh2o
            uco2(i,k) = zenfac(i)*pthco2
            uo2 (i,k) = zenfac(i)*ptho2
            uo3 (i,k) = ptho3
            usul(i,k) = aermmr(i,k,idxSUL) * path 
            ubg(i,k) = aermmr(i,k,idxBG) * path 
            usslt(i,k) = aermmr(i,k,idxSSLT) * path
            if (usslt(i,k) .lt. 0.0) then  
              usslt(i,k) = 0.0
            end if
            ucphil(i,k) = aermmr(i,k,idxOCPHI) * path
            ucphob(i,k) = aermmr(i,k,idxOCPHO) * path
            ucb(i,k) = ( aermmr(i,k,idxBCPHO) + aermmr(i,k,idxBCPHI) ) * path
            uvolc(i,k) =  aermmr(i,k,idxVOLC)
            do ksz = 1, ndstsz
              udst(ksz,i,k) = aermmr(i,k,idxDUSTfirst-1+ksz) * path
            end do
         end do



         uth2o(i) = 0.0_r8
         uto3(i)  = 0.0_r8
         utco2(i) = 0.0_r8
         uto2(i)  = 0.0_r8

         do k=1,pver
            uth2o(i) = uth2o(i) + uh2o(i,k)
            uto3(i)  = uto3(i)  + uo3(i,k)
            utco2(i) = utco2(i) + uco2(i,k)
            uto2(i)  = uto2(i)  + uo2(i,k)
         end do





         tauxcl(i,0)  = 0._r8
         wcl(i,0)     = 0.999999_r8
         gcl(i,0)     = 0.85_r8
         fcl(i,0)     = 0.725_r8
         tauxci(i,0)  = 0._r8
         wci(i,0)     = 0.999999_r8
         gci(i,0)     = 0.85_r8
         fci(i,0)     = 0.725_r8



         tauxar(i,0)  = 0._r8
         wa(i,0)      = 0.925_r8
         ga(i,0)      = 0.850_r8
         fa(i,0)      = 0.7225_r8



   end do



   do ns=1,nspint












      if(wavmax(ns) <= 0.7_r8) then
         indxsl = 1
      else if(wavmin(ns) == 0.700_r8) then
         indxsl = 2
      else if(wavmin(ns) == 0.701_r8) then
         indxsl = 3
      else if(wavmin(ns) == 0.702_r8 .or. wavmin(ns) > 2.38_r8) then
         indxsl = 4
      end if




      abarli = abarl(indxsl)
      bbarli = bbarl(indxsl)
      cbarli = cbarl(indxsl)
      dbarli = dbarl(indxsl)
      ebarli = ebarl(indxsl)
      fbarli = fbarl(indxsl)

      abarii = abari(indxsl)
      bbarii = bbari(indxsl)
      cbarii = cbari(indxsl)
      dbarii = dbari(indxsl)
      ebarii = ebari(indxsl)
      fbarii = fbari(indxsl)




      psf(ns) = 1.0_r8
      if(ph2o(ns)/=0._r8) psf(ns) = psf(ns)*ph2o(ns)
      if(pco2(ns)/=0._r8) psf(ns) = psf(ns)*pco2(ns)
      if(po2 (ns)/=0._r8) psf(ns) = psf(ns)*po2 (ns)

      do n=1,ndayc
         i=idayc(n)

         frc_day(i) = 1.0_r8
         do kaer = 1, naer_groups
            aertau(i,ns,kaer) = 0.0
            aerssa(i,ns,kaer) = 0.0
            aerasm(i,ns,kaer) = 0.0
            aerfwd(i,ns,kaer) = 0.0
         end do

            do k=1,pver



               tmp1l = abarli + bbarli/rel(i,k)
               tmp2l = 1._r8 - cbarli - dbarli*rel(i,k)
               tmp3l = fbarli*rel(i,k)



               tmp1i = abarii + bbarii/rei(i,k)
               tmp2i = 1._r8 - cbarii - dbarii*rei(i,k)
               tmp3i = fbarii*rei(i,k)

               if (cld(i,k) >= cldmin .and. cld(i,k) >= cldeps) then
                  tauxcl(i,k) = cliqwp(i,k)*tmp1l
                  tauxci(i,k) = cicewp(i,k)*tmp1i
               else
                  tauxcl(i,k) = 0.0
                  tauxci(i,k) = 0.0
               endif





               wcl(i,k) = min(tmp2l,.999999_r8)
               gcl(i,k) = ebarli + tmp3l
               fcl(i,k) = gcl(i,k)*gcl(i,k)

               wci(i,k) = min(tmp2i,.999999_r8)
               gci(i,k) = ebarii + tmp3i
               fci(i,k) = gci(i,k)*gci(i,k)




               rhtrunc = rh(i,k)
               rhtrunc = min(rh(i,k),1._r8)

               krh = min(floor( rhtrunc * nrh ) + 1, nrh - 1)
               wrh = rhtrunc * nrh - krh

               
               ksuli = ksul(krh + 1, ns) * (wrh + 1) - ksul(krh, ns) * wrh
               ksslti = ksslt(krh + 1, ns) * (wrh + 1) - ksslt(krh, ns) * wrh
               kcphili = kcphil(krh + 1, ns) * (wrh + 1) - kcphil(krh, ns) * wrh
               wsuli = wsul(krh + 1, ns) * (wrh + 1) - wsul(krh, ns) * wrh
               wsslti = wsslt(krh + 1, ns) * (wrh + 1) - wsslt(krh, ns) * wrh
               wcphili = wcphil(krh + 1, ns) * (wrh + 1) - wcphil(krh, ns) * wrh
               gsuli = gsul(krh + 1, ns) * (wrh + 1) - gsul(krh, ns) * wrh
               gsslti = gsslt(krh + 1, ns) * (wrh + 1) - gsslt(krh, ns) * wrh
               gcphili = gcphil(krh + 1, ns) * (wrh + 1) - gcphil(krh, ns) * wrh

               tau_sul = 1.e4 * ksuli * usul(i,k)
               tau_sslt = 1.e4 * ksslti * usslt(i,k)
               tau_cphil = 1.e4 * kcphili * ucphil(i,k)
               tau_cphob = 1.e4 * kcphob(ns) * ucphob(i,k)
               tau_cb = 1.e4 * kcb(ns) * ucb(i,k)
               tau_volc = 1.e3 * kvolc(ns) * uvolc(i,k)
               tau_dst(:) = 1.e4 * kdst(:,ns) * udst(:,i,k)
               tau_bg = 1.e4 * kbg(ns) * ubg(i,k)

               tau_w_sul = tau_sul * wsuli
               tau_w_sslt = tau_sslt * wsslti
               tau_w_cphil = tau_cphil * wcphili
               tau_w_cphob = tau_cphob * wcphob(ns)
               tau_w_cb = tau_cb * wcb(ns)
               tau_w_volc = tau_volc * wvolc(ns)
               tau_w_dst(:) = tau_dst(:) * wdst(:,ns)
               tau_w_bg = tau_bg * wbg(ns)

               tau_w_g_sul = tau_w_sul * gsuli
               tau_w_g_sslt = tau_w_sslt * gsslti
               tau_w_g_cphil = tau_w_cphil * gcphili
               tau_w_g_cphob = tau_w_cphob * gcphob(ns)
               tau_w_g_cb = tau_w_cb * gcb(ns)
               tau_w_g_volc = tau_w_volc * gvolc(ns)
               tau_w_g_dst(:) = tau_w_dst(:) * gdst(:,ns)
               tau_w_g_bg = tau_w_bg * gbg(ns)

               f_sul = gsuli * gsuli
               f_sslt = gsslti * gsslti
               f_cphil = gcphili * gcphili
               f_cphob = gcphob(ns) * gcphob(ns)
               f_cb = gcb(ns) * gcb(ns)
               f_volc = gvolc(ns) * gvolc(ns)
               f_dst(:) = gdst(:,ns) * gdst(:,ns)
               f_bg = gbg(ns) * gbg(ns)

               tau_w_f_sul = tau_w_sul * f_sul
               tau_w_f_bg = tau_w_bg * f_bg
               tau_w_f_sslt = tau_w_sslt * f_sslt
               tau_w_f_cphil = tau_w_cphil * f_cphil
               tau_w_f_cphob = tau_w_cphob * f_cphob
               tau_w_f_cb = tau_w_cb * f_cb
               tau_w_f_volc = tau_w_volc * f_volc
               tau_w_f_dst(:) = tau_w_dst(:) * f_dst(:)





               tau_dst_tot = sum(tau_dst)
               tau_w_dst_tot = sum(tau_w_dst)
               tau_w_g_dst_tot = sum(tau_w_g_dst)
               tau_w_f_dst_tot = sum(tau_w_f_dst)

               if (tau_dst_tot .gt. 0.0) then
                 w_dst_tot = tau_w_dst_tot / tau_dst_tot
               else
                 w_dst_tot = 0.0
               endif

               if (tau_w_dst_tot .gt. 0.0) then
                 g_dst_tot = tau_w_g_dst_tot / tau_w_dst_tot
                 f_dst_tot = tau_w_f_dst_tot / tau_w_dst_tot
               else
                 g_dst_tot = 0.0
                 f_dst_tot = 0.0
               endif



               tau_tot     = tau_sul + tau_sslt &
                           + tau_cphil + tau_cphob + tau_cb + tau_dst_tot
               tau_tot     = tau_tot + tau_bg + tau_volc

               tau_w_tot   = tau_w_sul + tau_w_sslt &
                           + tau_w_cphil + tau_w_cphob + tau_w_cb + tau_w_dst_tot
               tau_w_tot   = tau_w_tot + tau_w_bg + tau_w_volc

               tau_w_g_tot = tau_w_g_sul + tau_w_g_sslt &
                           + tau_w_g_cphil + tau_w_g_cphob + tau_w_g_cb + tau_w_g_dst_tot
               tau_w_g_tot = tau_w_g_tot + tau_w_g_bg + tau_w_g_volc

               tau_w_f_tot = tau_w_f_sul + tau_w_f_sslt &
                           + tau_w_f_cphil + tau_w_f_cphob + tau_w_f_cb + tau_w_f_dst_tot
               tau_w_f_tot = tau_w_f_tot + tau_w_f_bg  + tau_w_f_volc

               if (tau_tot .gt. 0.0) then
                 w_tot = tau_w_tot / tau_tot
               else
                 w_tot = 0.0
               endif

               if (tau_w_tot .gt. 0.0) then
                 g_tot = tau_w_g_tot / tau_w_tot
                 f_tot = tau_w_f_tot / tau_w_tot
               else
                 g_tot = 0.0
                 f_tot = 0.0
               endif

               tauxar(i,k) = tau_tot
               wa(i,k)     = min(w_tot, 0.999999_r8)
               if (g_tot.gt.1._r8) write(6,*) "g_tot > 1"
               if (g_tot.lt.-1._r8) write(6,*) "g_tot < -1"


               ga(i,k)     = g_tot
               if (f_tot.gt.1._r8) write(6,*)"f_tot > 1"
               if (f_tot.lt.0._r8) write(6,*)"f_tot < 0"


               fa(i,k)     = f_tot

               aertau(i,ns,1) = aertau(i,ns,1) + tau_sul
               aertau(i,ns,2) = aertau(i,ns,2) + tau_sslt
               aertau(i,ns,3) = aertau(i,ns,3) + tau_cphil + tau_cphob + tau_cb
               aertau(i,ns,4) = aertau(i,ns,4) + tau_dst_tot
               aertau(i,ns,5) = aertau(i,ns,5) + tau_bg
               aertau(i,ns,6) = aertau(i,ns,6) + tau_volc
               aertau(i,ns,7) = aertau(i,ns,7) + tau_tot

               aerssa(i,ns,1) = aerssa(i,ns,1) + tau_w_sul
               aerssa(i,ns,2) = aerssa(i,ns,2) + tau_w_sslt
               aerssa(i,ns,3) = aerssa(i,ns,3) + tau_w_cphil + tau_w_cphob + tau_w_cb
               aerssa(i,ns,4) = aerssa(i,ns,4) + tau_w_dst_tot
               aerssa(i,ns,5) = aerssa(i,ns,5) + tau_w_bg
               aerssa(i,ns,6) = aerssa(i,ns,6) + tau_w_volc
               aerssa(i,ns,7) = aerssa(i,ns,7) + tau_w_tot

               aerasm(i,ns,1) = aerasm(i,ns,1) + tau_w_g_sul
               aerasm(i,ns,2) = aerasm(i,ns,2) + tau_w_g_sslt
               aerasm(i,ns,3) = aerasm(i,ns,3) + tau_w_g_cphil + tau_w_g_cphob + tau_w_g_cb
               aerasm(i,ns,4) = aerasm(i,ns,4) + tau_w_g_dst_tot
               aerasm(i,ns,5) = aerasm(i,ns,5) + tau_w_g_bg
               aerasm(i,ns,6) = aerasm(i,ns,6) + tau_w_g_volc
               aerasm(i,ns,7) = aerasm(i,ns,7) + tau_w_g_tot

               aerfwd(i,ns,1) = aerfwd(i,ns,1) + tau_w_f_sul
               aerfwd(i,ns,2) = aerfwd(i,ns,2) + tau_w_f_sslt
               aerfwd(i,ns,3) = aerfwd(i,ns,3) + tau_w_f_cphil + tau_w_f_cphob + tau_w_f_cb
               aerfwd(i,ns,4) = aerfwd(i,ns,4) + tau_w_f_dst_tot
               aerfwd(i,ns,5) = aerfwd(i,ns,5) + tau_w_f_bg
               aerfwd(i,ns,6) = aerfwd(i,ns,6) + tau_w_f_volc
               aerfwd(i,ns,7) = aerfwd(i,ns,7) + tau_w_f_tot




            end do

            
            do kaer = 1, naer_groups

               if (aerssa(i,ns,kaer) .gt. 0.0) then   
                  aerasm(i,ns,kaer) = aerasm(i,ns,kaer) / aerssa(i,ns,kaer)
                  aerfwd(i,ns,kaer) = aerfwd(i,ns,kaer) / aerssa(i,ns,kaer)
               else
                  aerasm(i,ns,kaer) = 0.0_r8
                  aerfwd(i,ns,kaer) = 0.0_r8
               end if

               if (aertau(i,ns,kaer) .gt. 0.0) then
                  aerssa(i,ns,kaer) = aerssa(i,ns,kaer) / aertau(i,ns,kaer)
               else
                  aerssa(i,ns,kaer) = 0.0_r8
               end if

            end do





      end do




      wavmid(ns) = 0.5_r8*(wavmin(ns) + wavmax(ns))



      if (wavmid(ns) < 0.7_r8 ) then
         do n=1,ndayc
            i=idayc(n)
               albdir(i,ns) = asdir(i)
               albdif(i,ns) = asdif(i)
         end do



      else
         do n=1,ndayc
            i=idayc(n)
               albdir(i,ns) = aldir(i)
               albdif(i,ns) = aldif(i)
         end do
      end if
      trayoslp = raytau(ns)/sslp





      call raddedmx(pver, pverp, pcols, coszrs   ,ndayc    ,idayc   , &
              abh2o(ns),abo3(ns) ,abco2(ns),abo2(ns) , &
              uh2o     ,uo3      ,uco2     ,uo2      , &
              trayoslp ,pflx     ,ns       , &
              tauxcl   ,wcl      ,gcl      ,fcl      , &
              tauxci   ,wci      ,gci      ,fci      , &
              tauxar   ,wa       ,ga       ,fa       , &
              rdir     ,rdif     ,tdir     ,tdif     ,explay  , &
              rdirc    ,rdifc    ,tdirc    ,tdifc    ,explayc )



   end do


























   do n=1,ndayc
      i=idayc(n)















































         npasses = 0
         do
            do irgn = 0, nmxrgn(i)
               kx2(irgn) = 0
            end do
            mrgn = 0



            do irgn = 1, nmxrgn(i)



               region_found = .false.
               if (kx2(irgn-1) < pver) then
                  k1 = kx2(irgn-1)+1
                  kx1(irgn) = k1
                  kx2(irgn) = k1-1
                  do k2 = pver, k1, -1
                     if (pmid(i,k2) <= pmxrgn(i,irgn)) then
                        kx2(irgn) = k2
                        mrgn = mrgn+1
                        region_found = .true.
                        exit
                     end if
                  end do
               else
                  exit
               endif

               if (region_found) then



                  nxs = 0
                  if (cldeps > 0) then 
                     do k = k1,k2
                        if (cld(i,k) >= cldmin .and. cld(i,k) >= cldeps) then
                           nxs = nxs+1
                           ksort(nxs) = k




                           asort(nxs) = 1.0_r8-(floor(cld(i,k)/cldeps)*cldeps)
                        end if
                     end do
                  else
                     do k = k1,k2
                        if (cld(i,k) >= cldmin) then
                           nxs = nxs+1
                           ksort(nxs) = k




                           asort(nxs) = 1.0_r8-cld(i,k)
                        end if
                     end do
                  endif





                  if (nxs == 2) then
                     if (asort(2) < asort(1)) then
                        ktmp = ksort(1)
                        ksort(1) = ksort(2)
                        ksort(2) = ktmp

                        atmp = asort(1)
                        asort(1) = asort(2)
                        asort(2) = atmp
                     endif
                  else if (nxs >= 3) then
                     call sortarray(nxs,asort,ksort)
                  endif



                  cstr(k1:k2,1:nxs+1) = 0
                  mstr = 1
                  cld0 = 0.0_r8
                  do l = 1, nxs
                     if (asort(l) /= cld0) then
                        wstr(mstr,mrgn) = asort(l) - cld0
                        cld0 = asort(l)
                        mstr = mstr + 1
                     endif
                     cstr(ksort(l),mstr:nxs+1) = 1
                  end do
                  nstr(mrgn) = mstr
                  wstr(mstr,mrgn) = 1.0_r8 - cld0



               endif



            end do
            nrgn = mrgn



            cstr(0,1:nstr(1)) = 0


















            nconfigm = product(nstr(1: nrgn))



            istr(1: nrgn) = 1
            nconfig = 0
            totwgt = 0.0_r8
            new_term = .true.
            do iconfig = 1, nconfigm
               xwgt = 1.0_r8
               do mrgn = 1,  nrgn
                  xwgt = xwgt * wstr(istr(mrgn),mrgn)
               end do
               if (xwgt >= areamin) then
                  nconfig = nconfig + 1
                  if (nconfig <= nconfgmax) then
                     j = nconfig
                     ptrc(nconfig) = nconfig
                  else
                     nconfig = nconfgmax
                     if (new_term) then
                        j = findvalue(1,nconfig,wgtv,ptrc)
                     endif
                     if (wgtv(j) < xwgt) then
                        totwgt = totwgt - wgtv(j)
                        new_term = .true.
                     else
                        new_term = .false.
                     endif
                  endif
                  if (new_term) then
                     wgtv(j) = xwgt
                     totwgt = totwgt + xwgt
                     do mrgn = 1, nrgn
                        ccon(kx1(mrgn):kx2(mrgn),j) = cstr(kx1(mrgn):kx2(mrgn),istr(mrgn))
                     end do
                  endif
               endif

               mrgn =  nrgn
               istr(mrgn) = istr(mrgn) + 1
               do while (istr(mrgn) > nstr(mrgn) .and. mrgn > 1)
                  istr(mrgn) = 1
                  mrgn = mrgn - 1
                  istr(mrgn) = istr(mrgn) + 1
               end do



            end do




            if (totwgt > 0.) then
               exit
            else
               npasses = npasses + 1
               if (npasses >= 2 ) then
                  write(6,*)'RADCSWMX: Maximum overlap of column ','failed'
                  call endrun
               endif
               nmxrgn(i)=1
               pmxrgn(i,1)=1.0e30
            end if



         end do




         ccon(0,:) = 0
         ccon(pverp,:) = 0



         nuniqd(0) = 1
         nuniqu(pverp) = 1

         istrtd(0,1) = 1
         istrtu(pverp,1) = 1

         do j = 1, nconfig
            icond(0,j)=j
            iconu(pverp,j)=j
         end do

         istrtd(0,2) = nconfig+1
         istrtu(pverp,2) = nconfig+1

         do k = 1, pverp
            km1 = k-1
            nuniq = 0
            istrtd(k,1) = 1
            do l0 = 1, nuniqd(km1)
               is0 = istrtd(km1,l0)
               is1 = istrtd(km1,l0+1)-1
               n0 = 0
               n1 = 0
               do isn = is0, is1
                  j = icond(km1,isn)
                  if (ccon(k,j) == 0) then
                     n0 = n0 + 1
                     ptr0(n0) = j
                  endif
                  if (ccon(k,j) == 1) then
                     n1 = n1 + 1
                     ptr1(n1) = j
                  endif
               end do
               if (n0 > 0) then
                  nuniq = nuniq + 1
                  istrtd(k,nuniq+1) = istrtd(k,nuniq)+n0
                  icond(k,istrtd(k,nuniq):istrtd(k,nuniq+1)-1) =  ptr0(1:n0)
               endif
               if (n1 > 0) then
                  nuniq = nuniq + 1
                  istrtd(k,nuniq+1) = istrtd(k,nuniq)+n1
                  icond(k,istrtd(k,nuniq):istrtd(k,nuniq+1)-1) =  ptr1(1:n1)
               endif
            end do
            nuniqd(k) = nuniq
         end do

         do k = pver, 0, -1
            kp1 = k+1
            nuniq = 0
            istrtu(k,1) = 1
            do l0 = 1, nuniqu(kp1)
               is0 = istrtu(kp1,l0)
               is1 = istrtu(kp1,l0+1)-1
               n0 = 0
               n1 = 0
               do isn = is0, is1
                  j = iconu(kp1,isn)
                  if (ccon(k,j) == 0) then
                     n0 = n0 + 1
                     ptr0(n0) = j
                  endif
                  if (ccon(k,j) == 1) then
                     n1 = n1 + 1
                     ptr1(n1) = j
                  endif
               end do
               if (n0 > 0) then
                  nuniq = nuniq + 1
                  istrtu(k,nuniq+1) = istrtu(k,nuniq)+n0
                  iconu(k,istrtu(k,nuniq):istrtu(k,nuniq+1)-1) =  ptr0(1:n0)
               endif
               if (n1 > 0) then
                  nuniq = nuniq + 1
                  istrtu(k,nuniq+1) = istrtu(k,nuniq)+n1
                  iconu(k,istrtu(k,nuniq):istrtu(k,nuniq+1)-1) = ptr1(1:n1)
               endif
            end do
            nuniqu(k) = nuniq
         end do












         do k=0,pver
            totfld(k) = 0.0_r8
            fswup (k) = 0.0_r8
            fswdn (k) = 0.0_r8
            fswupc (k) = 0.0_r8
            fswdnc (k) = 0.0_r8
         end do

         sfltot        = 0.0_r8
         fswup (pverp) = 0.0_r8
         fswdn (pverp) = 0.0_r8
         fswupc (pverp) = 0.0_r8
         fswdnc (pverp) = 0.0_r8



         do ns = 1,nspint
            wgtint = nirwgt(ns)








            rdndif(0,1:nconfig) = 0.0_r8
            exptdn(0,1:nconfig) = 1.0_r8
            tdntot(0,1:nconfig) = 1.0_r8








            do k = 1, pverp
               km1 = k - 1
               do l0 = 1, nuniqd(km1)
                  is0 = istrtd(km1,l0)
                  is1 = istrtd(km1,l0+1)-1

                  j = icond(km1,is0)

                  xexpt   = exptdn(km1,j)
                  xrdnd   = rdndif(km1,j)
                  tdnmexp = tdntot(km1,j) - xexpt

                  if (ccon(km1,j) == 1) then



                     ytdnd = tdif(ns,i,km1)
                     yrdnd = rdif(ns,i,km1)

                     rdenom  = 1._r8/(1._r8-yrdnd*xrdnd)
                     rdirexp = rdir(ns,i,km1)*xexpt

                     zexpt = xexpt * explay(ns,i,km1)
                     zrdnd = yrdnd + xrdnd*(ytdnd**2)*rdenom
                     ztdnt = xexpt*tdir(ns,i,km1) + ytdnd*(tdnmexp + xrdnd*rdirexp)*rdenom
                  else



                     ytdnd = tdifc(ns,i,km1)
                     yrdnd = rdifc(ns,i,km1)

                     rdenom  = 1._r8/(1._r8-yrdnd*xrdnd)
                     rdirexp = rdirc(ns,i,km1)*xexpt

                     zexpt = xexpt * explayc(ns,i,km1)
                     zrdnd = yrdnd + xrdnd*(ytdnd**2)*rdenom
                     ztdnt = xexpt*tdirc(ns,i,km1) + ytdnd* &
                                            (tdnmexp + xrdnd*rdirexp)*rdenom
                  endif






                  do isn = is0, is1
                     j = icond(km1,isn)
                     exptdn(k,j) = zexpt
                     rdndif(k,j) = zrdnd
                     tdntot(k,j) = ztdnt
                  end do



               end do



            end do









            rupdir(pverp,1:nconfig) = albdir(i,ns)
            rupdif(pverp,1:nconfig) = albdif(i,ns)

            do k = pver, 0, -1
               do l0 = 1, nuniqu(k)
                  is0 = istrtu(k,l0)
                  is1 = istrtu(k,l0+1)-1

                  j = iconu(k,is0)

                  xrupd = rupdif(k+1,j)
                  xrups = rupdir(k+1,j)

                  if (ccon(k,j) == 1) then



                     yexpt = explay(ns,i,k)
                     yrupd = rdif(ns,i,k)
                     ytupd = tdif(ns,i,k)

                     rdenom  = 1._r8/( 1._r8 - yrupd*xrupd)
                     tdnmexp = (tdir(ns,i,k)-yexpt)
                     rdirexp = xrups*yexpt

                     zrupd = yrupd + xrupd*(ytupd**2)*rdenom
                     zrups = rdir(ns,i,k) + ytupd*(rdirexp + xrupd*tdnmexp)*rdenom
                  else



                     yexpt = explayc(ns,i,k)
                     yrupd = rdifc(ns,i,k)
                     ytupd = tdifc(ns,i,k)

                     rdenom  = 1._r8/( 1._r8 - yrupd*xrupd)
                     tdnmexp = (tdirc(ns,i,k)-yexpt)
                     rdirexp = xrups*yexpt

                     zrupd = yrupd + xrupd*(ytupd**2)*rdenom
                     zrups = rdirc(ns,i,k) + ytupd*(rdirexp + xrupd*tdnmexp)*rdenom
                  endif






                  do isn = is0, is1
                     j = iconu(k,isn)
                     rupdif(k,j) = zrupd
                     rupdir(k,j) = zrups
                  end do



               end do



            end do













            do k = 0,pverp



               fluxup(k)=0.0_r8
               fluxdn(k)=0.0_r8

               do iconfig = 1, nconfig
                  xwgt = wgtv(iconfig)
                  xexpt = exptdn(k,iconfig)
                  xtdnt = tdntot(k,iconfig)
                  xrdnd = rdndif(k,iconfig)
                  xrupd = rupdif(k,iconfig)
                  xrups = rupdir(k,iconfig)



                  rdenom = 1._r8/(1._r8 - xrdnd * xrupd)

                  fluxup(k) = fluxup(k) + xwgt *  &
                              ((xexpt * xrups + (xtdnt - xexpt) * xrupd) * rdenom)
                  fluxdn(k) = fluxdn(k) + xwgt *  &
                              (xexpt + (xtdnt - xexpt + xexpt * xrups * xrdnd) * rdenom)



               end do




               fluxup(k)=fluxup(k) / totwgt
               fluxdn(k)=fluxdn(k) / totwgt                  



            end do



            wexptdn = 0.0_r8

            do iconfig = 1, nconfig
               wexptdn =  wexptdn + wgtv(iconfig) * exptdn(pverp,iconfig)
            end do

            wexptdn = wexptdn / totwgt



            solflx   = solin(i)*frcsol(ns)*psf(ns)
            fsnt(i)  = fsnt(i) + solflx*(fluxdn(1) - fluxup(1))
            fsntoa(i)= fsntoa(i) + solflx*(fluxdn(0) - fluxup(0))
            fsns(i)  = fsns(i) + solflx*(fluxdn(pverp)-fluxup(pverp))
            sfltot   = sfltot + solflx
            fswup(0) = fswup(0) + solflx*fluxup(0)
            fswdn(0) = fswdn(0) + solflx*fluxdn(0)



            if (wavmid(ns) < 0.7_r8) then
               sols(i)  = sols(i) + wexptdn*solflx*0.001_r8
               solsd(i) = solsd(i)+(fluxdn(pverp)-wexptdn)*solflx*0.001_r8
            else
               soll(i)  = soll(i) + wexptdn*solflx*0.001_r8
               solld(i) = solld(i)+(fluxdn(pverp)-wexptdn)*solflx*0.001_r8
               fsnrtoaq(i) = fsnrtoaq(i) + solflx*(fluxdn(0) - fluxup(0))
            end if
            fsnirtoa(i) = fsnirtoa(i) + wgtint*solflx*(fluxdn(0) - fluxup(0))

            do k=0,pver




               kp1 = k+1
               flxdiv = (fluxdn(k  ) - fluxdn(kp1)) + (fluxup(kp1) - fluxup(k  ))
               totfld(k)  = totfld(k)  + solflx*flxdiv
               fswdn(kp1) = fswdn(kp1) + solflx*fluxdn(kp1)
               fswup(kp1) = fswup(kp1) + solflx*fluxup(kp1)
            end do



            exptdnc(0) =   1.0_r8
            rdndifc(0) =   0.0_r8
            tdntotc(0) =   1.0_r8
            rupdirc(pverp) = albdir(i,ns)
            rupdifc(pverp) = albdif(i,ns)

            do k = 1, pverp
               km1 = k - 1
               xexpt = exptdnc(km1)
               xrdnd = rdndifc(km1)
               yrdnd = rdifc(ns,i,km1)
               ytdnd = tdifc(ns,i,km1)

               exptdnc(k) = xexpt*explayc(ns,i,km1)

               rdenom  = 1._r8/(1._r8 - yrdnd*xrdnd)
               rdirexp = rdirc(ns,i,km1)*xexpt
               tdnmexp = tdntotc(km1) - xexpt

               tdntotc(k) = xexpt*tdirc(ns,i,km1) + ytdnd*(tdnmexp + xrdnd*rdirexp)* &
                                rdenom
               rdndifc(k) = yrdnd + xrdnd*(ytdnd**2)*rdenom
            end do

            do k=pver,0,-1
               xrupd = rupdifc(k+1)
               yexpt = explayc(ns,i,k)
               yrupd = rdifc(ns,i,k)
               ytupd = tdifc(ns,i,k)

               rdenom = 1._r8/( 1._r8 - yrupd*xrupd)

               rupdirc(k) = rdirc(ns,i,k) + ytupd*(rupdirc(k+1)*yexpt + &
                            xrupd*(tdirc(ns,i,k)-yexpt))*rdenom
               rupdifc(k) = yrupd + xrupd*ytupd**2*rdenom
            end do

            do k=0,1
               rdenom    = 1._r8/(1._r8 - rdndifc(k)*rupdifc(k))
               fluxup(k) = (exptdnc(k)*rupdirc(k) + (tdntotc(k)-exptdnc(k))*rupdifc(k))* &
                           rdenom
               fluxdn(k) = exptdnc(k) + &
                           (tdntotc(k) - exptdnc(k) + exptdnc(k)*rupdirc(k)*rdndifc(k))* &
                           rdenom
               fswupc(k) = fswupc(k) + solflx*fluxup(k)
               fswdnc(k) = fswdnc(k) + solflx*fluxdn(k)
            end do

            do k=2,pverp
            rdenom      = 1._r8/(1._r8 - rdndifc(k)*rupdifc(k))
            fluxup(k)   = (exptdnc(k)*rupdirc(k) + (tdntotc(k)-exptdnc(k))*rupdifc(k))* &
                           rdenom
            fluxdn(k)   = exptdnc(k) + (tdntotc(k) - exptdnc(k) + &
                          exptdnc(k)*rupdirc(k)*rdndifc(k))*rdenom
            fswupc(k)   = fswupc(k) + solflx*fluxup(k)
            fswdnc(k)   = fswdnc(k) + solflx*fluxdn(k)
            end do

            fsntc(i)    = fsntc(i)+solflx*(fluxdn(1)-fluxup(1))
            fsntoac(i)  = fsntoac(i)+solflx*(fluxdn(0)-fluxup(0))
            fsnsc(i)    = fsnsc(i)+solflx*(fluxdn(pverp)-fluxup(pverp))
            fsdsc(i)    = fsdsc(i)+solflx*(fluxdn(pverp))
            fsnrtoac(i) = fsnrtoac(i)+wgtint*solflx*(fluxdn(0)-fluxup(0))







         end do



         do k=1,pver
            qrs(i,k) = -1.E-4*gravit*totfld(k)/(pint(i,k) - pint(i,k+1))
         end do



         do k=1,pverp
            fsup(i,k)  = fswup(k)
            fsupc(i,k) = fswupc(k)
            fsdn(i,k)  = fswdn(k)
            fsdnc(i,k) = fswdnc(k)
         end do



         fsds(i) = fswdn(pverp)



   end do



   return
end subroutine radcswmx

subroutine raddedmx(pver, pverp, pcols, coszrs  ,ndayc   ,idayc   ,abh2o   , &
                    abo3    ,abco2   ,abo2    ,uh2o    ,uo3     , &
                    uco2    ,uo2     ,trayoslp,pflx    ,ns      , &
                    tauxcl  ,wcl     ,gcl     ,fcl     ,tauxci  , &
                    wci     ,gci     ,fci     ,tauxar  ,wa      , &
                    ga      ,fa      ,rdir    ,rdif    ,tdir    , &
                    tdif    ,explay  ,rdirc   ,rdifc   ,tdirc   , &
                    tdifc   ,explayc )




















   implicit none

   integer nspint           

   parameter ( nspint = 19 )



   real(r8) trmin                
   real(r8) wray                 
   real(r8) gray                 
   real(r8) fray                 

   parameter (trmin = 1.e-3)
   parameter (wray = 0.999999)
   parameter (gray = 0.0)
   parameter (fray = 0.1)





   integer, intent(in) :: pver, pverp, pcols
   real(r8), intent(in) :: coszrs(pcols)        
   real(r8), intent(in) :: trayoslp             
   real(r8), intent(in) :: pflx(pcols,0:pverp)  
   real(r8), intent(in) :: abh2o                
   real(r8), intent(in) :: abo3                 
   real(r8), intent(in) :: abco2                
   real(r8), intent(in) :: abo2                 
   real(r8), intent(in) :: uh2o(pcols,0:pver)   
   real(r8), intent(in) :: uo3(pcols,0:pver)    
   real(r8), intent(in) :: uco2(pcols,0:pver)   
   real(r8), intent(in) :: uo2(pcols,0:pver)    
   real(r8), intent(in) :: tauxcl(pcols,0:pver) 
   real(r8), intent(in) :: wcl(pcols,0:pver)    
   real(r8), intent(in) :: gcl(pcols,0:pver)    
   real(r8), intent(in) :: fcl(pcols,0:pver)    
   real(r8), intent(in) :: tauxci(pcols,0:pver) 
   real(r8), intent(in) :: wci(pcols,0:pver)    
   real(r8), intent(in) :: gci(pcols,0:pver)    
   real(r8), intent(in) :: fci(pcols,0:pver)    
   real(r8), intent(in) :: tauxar(pcols,0:pver) 
   real(r8), intent(in) :: wa(pcols,0:pver)     
   real(r8), intent(in) :: ga(pcols,0:pver)     
   real(r8), intent(in) :: fa(pcols,0:pver)     

   integer, intent(in) :: ndayc                 
   integer, intent(in) :: idayc(pcols)          
   integer, intent(in) :: ns                    






   real(r8), intent(inout) :: rdir(nspint,pcols,0:pver)   
   real(r8), intent(inout) :: rdif(nspint,pcols,0:pver)   
   real(r8), intent(inout) :: tdir(nspint,pcols,0:pver)   
   real(r8), intent(inout) :: tdif(nspint,pcols,0:pver)   
   real(r8), intent(inout) :: explay(nspint,pcols,0:pver) 



   real(r8), intent(inout) :: rdirc(nspint,pcols,0:pver)  
   real(r8), intent(inout) :: rdifc(nspint,pcols,0:pver)  
   real(r8), intent(inout) :: tdirc(nspint,pcols,0:pver)  
   real(r8), intent(inout) :: tdifc(nspint,pcols,0:pver)  
   real(r8), intent(inout) :: explayc(nspint,pcols,0:pver)



   integer i                 
   integer k                 
   integer nn                

   real(r8) taugab(pcols)        
   real(r8) tauray(pcols)        
   real(r8) taucsc               
   real(r8) tautot               
   real(r8) wtot                 
   real(r8) gtot                 
   real(r8) ftot                 
   real(r8) wtau                 
   real(r8) wt                   
   real(r8) ts                   
   real(r8) ws                   
   real(r8) gs                   





   real(r8) alpha                
   real(r8) gamma                
   real(r8) el                   
   real(r8) taus                 
   real(r8) omgs                 
   real(r8) asys                 
   real(r8) u                    

   real(r8) n                    

   real(r8) lm                   
   real(r8) ne                   
   real(r8) w                    
   real(r8) uu                   
   real(r8) g                    
   real(r8) e                    
   real(r8) f                    
   real(r8) t                    
   real(r8) et                   



   real(r8) alp                  
   real(r8) gam                  
   real(r8) ue                   
   real(r8) arg                  
   real(r8) extins               
   real(r8) amg                  
   real(r8) apg                  

   alpha(w,uu,g,e) = .75_r8*w*uu*((1._r8 + g*(1._r8-w))/(1._r8 - e*e*uu*uu))
   gamma(w,uu,g,e) = .50_r8*w*((3._r8*g*(1._r8-w)*uu*uu + 1._r8)/(1._r8-e*e*uu*uu))
   el(w,g)         = sqrt(3._r8*(1._r8-w)*(1._r8 - w*g))
   taus(w,f,t)     = (1._r8 - w*f)*t
   omgs(w,f)       = (1._r8 - f)*w/(1._r8 - w*f)
   asys(g,f)       = (g - f)/(1._r8 - f)
   u(w,g,e)        = 1.5_r8*(1._r8 - w*g)/e
   n(uu,et)        = ((uu+1._r8)*(uu+1._r8)/et ) - ((uu-1._r8)*(uu-1._r8)*et)










   do k=0,pver
      do nn=1,ndayc
         i=idayc(nn)
            tauray(i) = trayoslp*(pflx(i,k+1)-pflx(i,k))
            taugab(i) = abh2o*uh2o(i,k) + abo3*uo3(i,k) + abco2*uco2(i,k) + abo2*uo2(i,k)
            tautot = tauxcl(i,k) + tauxci(i,k) + tauray(i) + taugab(i) + tauxar(i,k)
            taucsc = tauxcl(i,k)*wcl(i,k) + tauxci(i,k)*wci(i,k) + tauxar(i,k)*wa(i,k)
            wtau   = wray*tauray(i)
            wt     = wtau + taucsc
            wtot   = wt/tautot
            gtot   = (wtau*gray + gcl(i,k)*wcl(i,k)*tauxcl(i,k) &
                     + gci(i,k)*wci(i,k)*tauxci(i,k) + ga(i,k) *wa(i,k) *tauxar(i,k))/wt
            ftot   = (wtau*fray + fcl(i,k)*wcl(i,k)*tauxcl(i,k) &
                     + fci(i,k)*wci(i,k)*tauxci(i,k) + fa(i,k) *wa(i,k) *tauxar(i,k))/wt
            ts   = taus(wtot,ftot,tautot)
            ws   = omgs(wtot,ftot)
            gs   = asys(gtot,ftot)
            lm   = el(ws,gs)
            alp  = alpha(ws,coszrs(i),gs,lm)
            gam  = gamma(ws,coszrs(i),gs,lm)
            ue   = u(ws,gs,lm)



            arg  = min(lm*ts,25._r8)
            extins = exp(-arg)
            ne = n(ue,extins)
            rdif(ns,i,k) = (ue+1._r8)*(ue-1._r8)*(1._r8/extins - extins)/ne
            tdif(ns,i,k)   =   4._r8*ue/ne



            arg       = min(ts/coszrs(i),25._r8)
            explay(ns,i,k) = exp(-arg)
            apg = alp + gam
            amg = alp - gam
            rdir(ns,i,k) = amg*(tdif(ns,i,k)*explay(ns,i,k)-1._r8) + apg*rdif(ns,i,k)
            tdir(ns,i,k) = apg*tdif(ns,i,k) + (amg*rdif(ns,i,k)-(apg-1._r8))*explay(ns,i,k)




            rdir(ns,i,k) = max(rdir(ns,i,k),0.0_r8)
            tdir(ns,i,k) = max(tdir(ns,i,k),0.0_r8)
            rdif(ns,i,k) = max(rdif(ns,i,k),0.0_r8)
            tdif(ns,i,k) = max(tdif(ns,i,k),0.0_r8)



            if (tauxcl(i,k) == 0.0_r8 .and. tauxci(i,k) == 0.0_r8) then

               rdirc(ns,i,k) = rdir(ns,i,k)
               tdirc(ns,i,k) = tdir(ns,i,k)
               rdifc(ns,i,k) = rdif(ns,i,k)
               tdifc(ns,i,k) = tdif(ns,i,k)
               explayc(ns,i,k) = explay(ns,i,k)
            else
               tautot = tauray(i) + taugab(i) + tauxar(i,k)
               taucsc = tauxar(i,k)*wa(i,k)



               wt     = wtau + taucsc
               wtot   = wt/tautot
               gtot   = (wtau*gray + ga(i,k)*wa(i,k)*tauxar(i,k))/wt
               ftot   = (wtau*fray + fa(i,k)*wa(i,k)*tauxar(i,k))/wt
               ts   = taus(wtot,ftot,tautot)
               ws   = omgs(wtot,ftot)
               gs   = asys(gtot,ftot)
               lm   = el(ws,gs)
               alp  = alpha(ws,coszrs(i),gs,lm)
               gam  = gamma(ws,coszrs(i),gs,lm)
               ue   = u(ws,gs,lm)



               arg  = min(lm*ts,25._r8)
               extins = exp(-arg)
               ne = n(ue,extins)
               rdifc(ns,i,k) = (ue+1._r8)*(ue-1._r8)*(1._r8/extins - extins)/ne
               tdifc(ns,i,k)   =   4._r8*ue/ne



               arg       = min(ts/coszrs(i),25._r8)
               explayc(ns,i,k) = exp(-arg)
               apg = alp + gam
               amg = alp - gam
               rdirc(ns,i,k) = amg*(tdifc(ns,i,k)*explayc(ns,i,k)-1._r8)+ &
                               apg*rdifc(ns,i,k)
               tdirc(ns,i,k) = apg*tdifc(ns,i,k) + (amg*rdifc(ns,i,k) - (apg-1._r8))* &
                               explayc(ns,i,k)




               rdirc(ns,i,k) = max(rdirc(ns,i,k),0.0_r8)
               tdirc(ns,i,k) = max(tdirc(ns,i,k),0.0_r8)
               rdifc(ns,i,k) = max(rdifc(ns,i,k),0.0_r8)
               tdifc(ns,i,k) = max(tdifc(ns,i,k),0.0_r8)
            end if
         end do
   end do

   return
end subroutine raddedmx

subroutine radinp(lchnk   ,ncol    , pcols, pver, pverp,     &
                  pmid    ,pint    ,o3vmr   , pmidrd  ,&
                  pintrd  ,eccf    ,o3mmr   )



















   implicit none





   integer, intent(in) :: lchnk                
   integer, intent(in) :: pcols, pver, pverp
   integer, intent(in) :: ncol                 

   real(r8), intent(in) :: pmid(pcols,pver)    
   real(r8), intent(in) :: pint(pcols,pverp)   
   real(r8), intent(in) :: o3vmr(pcols,pver)   



   real(r8), intent(out) :: pmidrd(pcols,pver)  
   real(r8), intent(out) :: pintrd(pcols,pverp) 
   real(r8), intent(out) :: eccf                
   real(r8), intent(out) :: o3mmr(pcols,pver)   




   integer i                
   integer k                

   real(r8) :: calday           
   real(r8) vmmr                
   real(r8) delta               





   eccf = 1. 






   do k=1,pver
      do i=1,ncol
         pmidrd(i,k) = pmid(i,k)*10.0
         pintrd(i,k) = pint(i,k)*10.0
      end do
   end do
   do i=1,ncol
      pintrd(i,pverp) = pint(i,pverp)*10.0
   end do



   vmmr = amo/amd
   do k=1,pver
      do i=1,ncol
         o3mmr(i,k) = vmmr*o3vmr(i,k)
      end do
   end do

   return
end subroutine radinp
subroutine radoz2(lchnk   ,ncol    ,pcols, pver, pverp, o3vmr   ,pint    ,plol    ,plos, ntoplw    )

















   implicit none


   integer, intent(in) :: lchnk                
   integer, intent(in) :: ncol                 
   integer, intent(in) :: pcols, pver, pverp

   real(r8), intent(in) :: o3vmr(pcols,pver)   
   real(r8), intent(in) :: pint(pcols,pverp)   

   integer, intent(in) :: ntoplw               




   real(r8), intent(out) :: plol(pcols,pverp)   
   real(r8), intent(out) :: plos(pcols,pverp)   




   integer i                
   integer k                






   do i=1,ncol
      plos(i,ntoplw) = 0.1 *cplos*o3vmr(i,ntoplw)*pint(i,ntoplw)
      plol(i,ntoplw) = 0.01*cplol*o3vmr(i,ntoplw)*pint(i,ntoplw)*pint(i,ntoplw)
   end do
   do k=ntoplw+1,pverp
      do i=1,ncol
         plos(i,k) = plos(i,k-1) + 0.1*cplos*o3vmr(i,k-1)*(pint(i,k) - pint(i,k-1))
         plol(i,k) = plol(i,k-1) + 0.01*cplol*o3vmr(i,k-1)* &
                    (pint(i,k)*pint(i,k) - pint(i,k-1)*pint(i,k-1))
      end do
   end do

   return
end subroutine radoz2


subroutine radozn (lchnk, ncol, pcols, pver,pmid, pin, levsiz, ozmix, o3vmr)















   implicit none




   integer, intent(in) :: lchnk               
   integer, intent(in) :: pcols, pver
   integer, intent(in) :: ncol                
   integer, intent(in) :: levsiz              

   real(r8), intent(in) :: pmid(pcols,pver)   
   real(r8), intent(in) :: pin(levsiz)        
   real(r8), intent(in) :: ozmix(pcols,levsiz) 

   real(r8), intent(out) :: o3vmr(pcols,pver) 



   integer i                   
   integer k, kk, kkstart      
   integer kupper(pcols)       
   integer kount               
   integer lats(pcols)         
   integer lons(pcols)         

   real(r8) dpu                
   real(r8) dpl                








   do i=1,ncol
      kupper(i) = 1
   end do

   do k=1,pver




      kkstart = levsiz
      do i=1,ncol
         kkstart = min0(kkstart,kupper(i))
      end do
      kount = 0



      do kk=kkstart,levsiz-1
         do i=1,ncol
            if (pin(kk).lt.pmid(i,k) .and. pmid(i,k).le.pin(kk+1)) then
               kupper(i) = kk
               kount = kount + 1
            end if
         end do




         if (kount.eq.ncol) then
            do i=1,ncol
               dpu = pmid(i,k) - pin(kupper(i))
               dpl = pin(kupper(i)+1) - pmid(i,k)
               o3vmr(i,k) = (ozmix(i,kupper(i))*dpl + &
                             ozmix(i,kupper(i)+1)*dpu)/(dpl + dpu)
            end do
            goto 35
         end if
      end do





      do i=1,ncol
         if (pmid(i,k) .lt. pin(1)) then
            o3vmr(i,k) = ozmix(i,1)*pmid(i,k)/pin(1)
         else if (pmid(i,k) .gt. pin(levsiz)) then
            o3vmr(i,k) = ozmix(i,levsiz)
         else
            dpu = pmid(i,k) - pin(kupper(i))
            dpl = pin(kupper(i)+1) - pmid(i,k)
            o3vmr(i,k) = (ozmix(i,kupper(i))*dpl + &
                          ozmix(i,kupper(i)+1)*dpu)/(dpl + dpu)
         end if
      end do

      if (kount.gt.ncol) then
         call endrun ('RADOZN: Bad ozone data: non-monotonicity suspected')
      end if
35    continue
   end do

   return
end subroutine radozn



end MODULE module_ra_cam
