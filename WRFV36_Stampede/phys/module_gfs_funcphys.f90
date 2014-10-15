
module module_gfs_funcphys












































































































































































































































  use module_gfs_machine,only:kind_phys
  use module_gfs_physcons
  implicit none
  private



  integer,public,parameter:: krealfp=kind_phys


  real(krealfp),parameter:: psatb=con_psat*1.e-5
  integer,parameter:: nxpvsl=7501
  real(krealfp) c1xpvsl,c2xpvsl,tbpvsl(nxpvsl)
  integer,parameter:: nxpvsi=7501
  real(krealfp) c1xpvsi,c2xpvsi,tbpvsi(nxpvsi)
  integer,parameter:: nxpvs=7501
  real(krealfp) c1xpvs,c2xpvs,tbpvs(nxpvs)
  integer,parameter:: nxtdpl=5001
  real(krealfp) c1xtdpl,c2xtdpl,tbtdpl(nxtdpl)
  integer,parameter:: nxtdpi=5001
  real(krealfp) c1xtdpi,c2xtdpi,tbtdpi(nxtdpi)
  integer,parameter:: nxtdp=5001
  real(krealfp) c1xtdp,c2xtdp,tbtdp(nxtdp)
  integer,parameter:: nxthe=241,nythe=151
  real(krealfp) c1xthe,c2xthe,c1ythe,c2ythe,tbthe(nxthe,nythe)
  integer,parameter:: nxma=151,nyma=121
  real(krealfp) c1xma,c2xma,c1yma,c2yma,tbtma(nxma,nyma),tbqma(nxma,nyma)

  integer,parameter:: nxpkap=11001
  real(krealfp) c1xpkap,c2xpkap,tbpkap(nxpkap)
  integer,parameter:: nxrkap=5501
  real(krealfp) c1xrkap,c2xrkap,tbrkap(nxrkap)
  integer,parameter:: nxtlcl=151,nytlcl=61
  real(krealfp) c1xtlcl,c2xtlcl,c1ytlcl,c2ytlcl,tbtlcl(nxtlcl,nytlcl)


  public gpvsl,fpvsl,fpvslq,fpvslx
  public gpvsi,fpvsi,fpvsiq,fpvsix
  public gpvs,fpvs,fpvsq,fpvsx
  public gtdpl,ftdpl,ftdplq,ftdplx,ftdplxg
  public gtdpi,ftdpi,ftdpiq,ftdpix,ftdpixg
  public gtdp,ftdp,ftdpq,ftdpx,ftdpxg
  public gthe,fthe,ftheq,fthex
  public gtma,stma,stmaq,stmax,stmaxg
  public gpkap,fpkap,fpkapq,fpkapo,fpkapx
  public grkap,frkap,frkapq,frkapx
  public gtlcl,ftlcl,ftlclq,ftlclo,ftlclx
  public gfuncphys
contains

  subroutine gpvsl

























    implicit none
    integer jx
    real(krealfp) xmin,xmax,xinc,x,t

    xmin=180.0_krealfp
    xmax=330.0_krealfp
    xinc=(xmax-xmin)/(nxpvsl-1)

    c2xpvsl=1./xinc
    c1xpvsl=1.-xmin*c2xpvsl
    do jx=1,nxpvsl
      x=xmin+(jx-1)*xinc
      t=x
      tbpvsl(jx)=fpvslx(t)
    enddo

  end subroutine


  function fpvsl(t)






























    implicit none
    real(krealfp) fpvsl
    real(krealfp),intent(in):: t
    integer jx
    real(krealfp) xj

    xj=min(max(c1xpvsl+c2xpvsl*t,1._krealfp),real(nxpvsl,krealfp))
    jx=min(xj,nxpvsl-1._krealfp)
    fpvsl=tbpvsl(jx)+(xj-jx)*(tbpvsl(jx+1)-tbpvsl(jx))

  end function


  function fpvslq(t)






























    implicit none
    real(krealfp) fpvslq
    real(krealfp),intent(in):: t
    integer jx
    real(krealfp) xj,dxj,fj1,fj2,fj3

    xj=min(max(c1xpvsl+c2xpvsl*t,1._krealfp),real(nxpvsl,krealfp))
    jx=min(max(nint(xj),2),nxpvsl-1)
    dxj=xj-jx
    fj1=tbpvsl(jx-1)
    fj2=tbpvsl(jx)
    fj3=tbpvsl(jx+1)
    fpvslq=(((fj3+fj1)/2-fj2)*dxj+(fj3-fj1)/2)*dxj+fj2

  end function


  function fpvslx(t)

































    implicit none
    real(krealfp) fpvslx
    real(krealfp),intent(in):: t
    real(krealfp),parameter:: dldt=con_cvap-con_cliq
    real(krealfp),parameter:: heat=con_hvap
    real(krealfp),parameter:: xpona=-dldt/con_rv
    real(krealfp),parameter:: xponb=-dldt/con_rv+heat/(con_rv*con_ttp)
    real(krealfp) tr

    tr=con_ttp/t
    fpvslx=con_psat*(tr**xpona)*exp(xponb*(1.-tr))

  end function

  subroutine gpvsi


























    implicit none
    integer jx
    real(krealfp) xmin,xmax,xinc,x,t

    xmin=180.0_krealfp
    xmax=330.0_krealfp
    xinc=(xmax-xmin)/(nxpvsi-1)

    c2xpvsi=1./xinc
    c1xpvsi=1.-xmin*c2xpvsi
    do jx=1,nxpvsi
      x=xmin+(jx-1)*xinc
      t=x
      tbpvsi(jx)=fpvsix(t)
    enddo

  end subroutine


  function fpvsi(t)































    implicit none
    real(krealfp) fpvsi
    real(krealfp),intent(in):: t
    integer jx
    real(krealfp) xj

    xj=min(max(c1xpvsi+c2xpvsi*t,1._krealfp),real(nxpvsi,krealfp))
    jx=min(xj,nxpvsi-1._krealfp)
    fpvsi=tbpvsi(jx)+(xj-jx)*(tbpvsi(jx+1)-tbpvsi(jx))

  end function


  function fpvsiq(t)































    implicit none
    real(krealfp) fpvsiq
    real(krealfp),intent(in):: t
    integer jx
    real(krealfp) xj,dxj,fj1,fj2,fj3

    xj=min(max(c1xpvsi+c2xpvsi*t,1._krealfp),real(nxpvsi,krealfp))
    jx=min(max(nint(xj),2),nxpvsi-1)
    dxj=xj-jx
    fj1=tbpvsi(jx-1)
    fj2=tbpvsi(jx)
    fj3=tbpvsi(jx+1)
    fpvsiq=(((fj3+fj1)/2-fj2)*dxj+(fj3-fj1)/2)*dxj+fj2

  end function


  function fpvsix(t)


































    implicit none
    real(krealfp) fpvsix
    real(krealfp),intent(in):: t
    real(krealfp),parameter:: dldt=con_cvap-con_csol
    real(krealfp),parameter:: heat=con_hvap+con_hfus
    real(krealfp),parameter:: xpona=-dldt/con_rv
    real(krealfp),parameter:: xponb=-dldt/con_rv+heat/(con_rv*con_ttp)
    real(krealfp) tr

    tr=con_ttp/t
    fpvsix=con_psat*(tr**xpona)*exp(xponb*(1.-tr))

  end function

  subroutine gpvs


























    implicit none
    integer jx
    real(krealfp) xmin,xmax,xinc,x,t

    xmin=180.0_krealfp
    xmax=330.0_krealfp
    xinc=(xmax-xmin)/(nxpvs-1)

    c2xpvs=1./xinc
    c1xpvs=1.-xmin*c2xpvs
    do jx=1,nxpvs
      x=xmin+(jx-1)*xinc
      t=x
      tbpvs(jx)=fpvsx(t)
    enddo

  end subroutine


  function fpvs(t)































    implicit none
    real(krealfp) fpvs
    real(krealfp),intent(in):: t
    integer jx
    real(krealfp) xj

    xj=min(max(c1xpvs+c2xpvs*t,1._krealfp),real(nxpvs,krealfp))
    jx=min(xj,nxpvs-1._krealfp)
    fpvs=tbpvs(jx)+(xj-jx)*(tbpvs(jx+1)-tbpvs(jx))

  end function


  function fpvsq(t)































    implicit none
    real(krealfp) fpvsq
    real(krealfp),intent(in):: t
    integer jx
    real(krealfp) xj,dxj,fj1,fj2,fj3

    xj=min(max(c1xpvs+c2xpvs*t,1._krealfp),real(nxpvs,krealfp))
    jx=min(max(nint(xj),2),nxpvs-1)
    dxj=xj-jx
    fj1=tbpvs(jx-1)
    fj2=tbpvs(jx)
    fj3=tbpvs(jx+1)
    fpvsq=(((fj3+fj1)/2-fj2)*dxj+(fj3-fj1)/2)*dxj+fj2

  end function


  function fpvsx(t)







































    implicit none
    real(krealfp) fpvsx
    real(krealfp),intent(in):: t
    real(krealfp),parameter:: tliq=con_ttp
    real(krealfp),parameter:: tice=con_ttp-20.0
    real(krealfp),parameter:: dldtl=con_cvap-con_cliq
    real(krealfp),parameter:: heatl=con_hvap
    real(krealfp),parameter:: xponal=-dldtl/con_rv
    real(krealfp),parameter:: xponbl=-dldtl/con_rv+heatl/(con_rv*con_ttp)
    real(krealfp),parameter:: dldti=con_cvap-con_csol
    real(krealfp),parameter:: heati=con_hvap+con_hfus
    real(krealfp),parameter:: xponai=-dldti/con_rv
    real(krealfp),parameter:: xponbi=-dldti/con_rv+heati/(con_rv*con_ttp)
    real(krealfp) tr,w,pvl,pvi

    tr=con_ttp/t
    if(t.ge.tliq) then
      fpvsx=con_psat*(tr**xponal)*exp(xponbl*(1.-tr))
    elseif(t.lt.tice) then
      fpvsx=con_psat*(tr**xponai)*exp(xponbi*(1.-tr))
    else
      w=(t-tice)/(tliq-tice)
      pvl=con_psat*(tr**xponal)*exp(xponbl*(1.-tr))
      pvi=con_psat*(tr**xponai)*exp(xponbi*(1.-tr))
      fpvsx=w*pvl+(1.-w)*pvi
    endif

  end function

  subroutine gtdpl


























    implicit none
    integer jx
    real(krealfp) xmin,xmax,xinc,t,x,pv

    xmin=1
    xmax=10001
    xinc=(xmax-xmin)/(nxtdpl-1)
    c1xtdpl=1.-xmin/xinc
    c2xtdpl=1./xinc
    t=208.0
    do jx=1,nxtdpl
      x=xmin+(jx-1)*xinc
      pv=x
      t=ftdplxg(t,pv)
      tbtdpl(jx)=t
    enddo

  end subroutine


  function ftdpl(pv)
































    implicit none
    real(krealfp) ftdpl
    real(krealfp),intent(in):: pv
    integer jx
    real(krealfp) xj

    xj=min(max(c1xtdpl+c2xtdpl*pv,1._krealfp),real(nxtdpl,krealfp))
    jx=min(xj,nxtdpl-1._krealfp)
    ftdpl=tbtdpl(jx)+(xj-jx)*(tbtdpl(jx+1)-tbtdpl(jx))

  end function


  function ftdplq(pv)
































    implicit none
    real(krealfp) ftdplq
    real(krealfp),intent(in):: pv
    integer jx
    real(krealfp) xj,dxj,fj1,fj2,fj3

    xj=min(max(c1xtdpl+c2xtdpl*pv,1._krealfp),real(nxtdpl,krealfp))
    jx=min(max(nint(xj),2),nxtdpl-1)
    dxj=xj-jx
    fj1=tbtdpl(jx-1)
    fj2=tbtdpl(jx)
    fj3=tbtdpl(jx+1)
    ftdplq=(((fj3+fj1)/2-fj2)*dxj+(fj3-fj1)/2)*dxj+fj2

  end function


  function ftdplx(pv)































    implicit none
    real(krealfp) ftdplx
    real(krealfp),intent(in):: pv
    real(krealfp) tg

    tg=ftdpl(pv)
    ftdplx=ftdplxg(tg,pv)

  end function


  function ftdplxg(tg,pv)





































    implicit none
    real(krealfp) ftdplxg
    real(krealfp),intent(in):: tg,pv
    real(krealfp),parameter:: terrm=1.e-6
    real(krealfp),parameter:: dldt=con_cvap-con_cliq
    real(krealfp),parameter:: heat=con_hvap
    real(krealfp),parameter:: xpona=-dldt/con_rv
    real(krealfp),parameter:: xponb=-dldt/con_rv+heat/(con_rv*con_ttp)
    real(krealfp) t,tr,pvt,el,dpvt,terr
    integer i

    t=tg
    do i=1,100
      tr=con_ttp/t
      pvt=con_psat*(tr**xpona)*exp(xponb*(1.-tr))
      el=heat+dldt*(t-con_ttp)
      dpvt=el*pvt/(con_rv*t**2)
      terr=(pvt-pv)/dpvt
      t=t-terr
      if(abs(terr).le.terrm) exit
    enddo
    ftdplxg=t

  end function

  subroutine gtdpi



























    implicit none
    integer jx
    real(krealfp) xmin,xmax,xinc,t,x,pv

    xmin=0.1
    xmax=1000.1
    xinc=(xmax-xmin)/(nxtdpi-1)
    c1xtdpi=1.-xmin/xinc
    c2xtdpi=1./xinc
    t=197.0
    do jx=1,nxtdpi
      x=xmin+(jx-1)*xinc
      pv=x
      t=ftdpixg(t,pv)
      tbtdpi(jx)=t
    enddo

  end subroutine


  function ftdpi(pv)

































    implicit none
    real(krealfp) ftdpi
    real(krealfp),intent(in):: pv
    integer jx
    real(krealfp) xj

    xj=min(max(c1xtdpi+c2xtdpi*pv,1._krealfp),real(nxtdpi,krealfp))
    jx=min(xj,nxtdpi-1._krealfp)
    ftdpi=tbtdpi(jx)+(xj-jx)*(tbtdpi(jx+1)-tbtdpi(jx))

  end function


  function ftdpiq(pv)

































    implicit none
    real(krealfp) ftdpiq
    real(krealfp),intent(in):: pv
    integer jx
    real(krealfp) xj,dxj,fj1,fj2,fj3

    xj=min(max(c1xtdpi+c2xtdpi*pv,1._krealfp),real(nxtdpi,krealfp))
    jx=min(max(nint(xj),2),nxtdpi-1)
    dxj=xj-jx
    fj1=tbtdpi(jx-1)
    fj2=tbtdpi(jx)
    fj3=tbtdpi(jx+1)
    ftdpiq=(((fj3+fj1)/2-fj2)*dxj+(fj3-fj1)/2)*dxj+fj2

  end function


  function ftdpix(pv)
































    implicit none
    real(krealfp) ftdpix
    real(krealfp),intent(in):: pv
    real(krealfp) tg

    tg=ftdpi(pv)
    ftdpix=ftdpixg(tg,pv)

  end function


  function ftdpixg(tg,pv)






































    implicit none
    real(krealfp) ftdpixg
    real(krealfp),intent(in):: tg,pv
    real(krealfp),parameter:: terrm=1.e-6
    real(krealfp),parameter:: dldt=con_cvap-con_csol
    real(krealfp),parameter:: heat=con_hvap+con_hfus
    real(krealfp),parameter:: xpona=-dldt/con_rv
    real(krealfp),parameter:: xponb=-dldt/con_rv+heat/(con_rv*con_ttp)
    real(krealfp) t,tr,pvt,el,dpvt,terr
    integer i

    t=tg
    do i=1,100
      tr=con_ttp/t
      pvt=con_psat*(tr**xpona)*exp(xponb*(1.-tr))
      el=heat+dldt*(t-con_ttp)
      dpvt=el*pvt/(con_rv*t**2)
      terr=(pvt-pv)/dpvt
      t=t-terr
      if(abs(terr).le.terrm) exit
    enddo
    ftdpixg=t

  end function

  subroutine gtdp



























    implicit none
    integer jx
    real(krealfp) xmin,xmax,xinc,t,x,pv

    xmin=0.5
    xmax=10000.5
    xinc=(xmax-xmin)/(nxtdp-1)
    c1xtdp=1.-xmin/xinc
    c2xtdp=1./xinc
    t=208.0
    do jx=1,nxtdp
      x=xmin+(jx-1)*xinc
      pv=x
      t=ftdpxg(t,pv)
      tbtdp(jx)=t
    enddo

  end subroutine


  function ftdp(pv)

































    implicit none
    real(krealfp) ftdp
    real(krealfp),intent(in):: pv
    integer jx
    real(krealfp) xj

    xj=min(max(c1xtdp+c2xtdp*pv,1._krealfp),real(nxtdp,krealfp))
    jx=min(xj,nxtdp-1._krealfp)
    ftdp=tbtdp(jx)+(xj-jx)*(tbtdp(jx+1)-tbtdp(jx))

  end function


  function ftdpq(pv)

































    implicit none
    real(krealfp) ftdpq
    real(krealfp),intent(in):: pv
    integer jx
    real(krealfp) xj,dxj,fj1,fj2,fj3

    xj=min(max(c1xtdp+c2xtdp*pv,1._krealfp),real(nxtdp,krealfp))
    jx=min(max(nint(xj),2),nxtdp-1)
    dxj=xj-jx
    fj1=tbtdp(jx-1)
    fj2=tbtdp(jx)
    fj3=tbtdp(jx+1)
    ftdpq=(((fj3+fj1)/2-fj2)*dxj+(fj3-fj1)/2)*dxj+fj2

  end function


  function ftdpx(pv)
































    implicit none
    real(krealfp) ftdpx
    real(krealfp),intent(in):: pv
    real(krealfp) tg

    tg=ftdp(pv)
    ftdpx=ftdpxg(tg,pv)

  end function


  function ftdpxg(tg,pv)











































    implicit none
    real(krealfp) ftdpxg
    real(krealfp),intent(in):: tg,pv
    real(krealfp),parameter:: terrm=1.e-6
    real(krealfp),parameter:: tliq=con_ttp
    real(krealfp),parameter:: tice=con_ttp-20.0
    real(krealfp),parameter:: dldtl=con_cvap-con_cliq
    real(krealfp),parameter:: heatl=con_hvap
    real(krealfp),parameter:: xponal=-dldtl/con_rv
    real(krealfp),parameter:: xponbl=-dldtl/con_rv+heatl/(con_rv*con_ttp)
    real(krealfp),parameter:: dldti=con_cvap-con_csol
    real(krealfp),parameter:: heati=con_hvap+con_hfus
    real(krealfp),parameter:: xponai=-dldti/con_rv
    real(krealfp),parameter:: xponbi=-dldti/con_rv+heati/(con_rv*con_ttp)
    real(krealfp) t,tr,w,pvtl,pvti,pvt,ell,eli,el,dpvt,terr
    integer i

    t=tg
    do i=1,100
      tr=con_ttp/t
      if(t.ge.tliq) then
        pvt=con_psat*(tr**xponal)*exp(xponbl*(1.-tr))
        el=heatl+dldtl*(t-con_ttp)
        dpvt=el*pvt/(con_rv*t**2)
      elseif(t.lt.tice) then
        pvt=con_psat*(tr**xponai)*exp(xponbi*(1.-tr))
        el=heati+dldti*(t-con_ttp)
        dpvt=el*pvt/(con_rv*t**2)
      else
        w=(t-tice)/(tliq-tice)
        pvtl=con_psat*(tr**xponal)*exp(xponbl*(1.-tr))
        pvti=con_psat*(tr**xponai)*exp(xponbi*(1.-tr))
        pvt=w*pvtl+(1.-w)*pvti
        ell=heatl+dldtl*(t-con_ttp)
        eli=heati+dldti*(t-con_ttp)
        dpvt=(w*ell*pvtl+(1.-w)*eli*pvti)/(con_rv*t**2)
      endif
      terr=(pvt-pv)/dpvt
      t=t-terr
      if(abs(terr).le.terrm) exit
    enddo
    ftdpxg=t

  end function

  subroutine gthe




























    implicit none
    integer jx,jy
    real(krealfp) xmin,xmax,ymin,ymax,xinc,yinc,x,y,pk,t

    xmin=con_ttp-90._krealfp
    xmax=con_ttp+30._krealfp
    ymin=0.04_krealfp**con_rocp
    ymax=1.10_krealfp**con_rocp
    xinc=(xmax-xmin)/(nxthe-1)
    c1xthe=1.-xmin/xinc
    c2xthe=1./xinc
    yinc=(ymax-ymin)/(nythe-1)
    c1ythe=1.-ymin/yinc
    c2ythe=1./yinc
    do jy=1,nythe
      y=ymin+(jy-1)*yinc
      pk=y
      do jx=1,nxthe
        x=xmin+(jx-1)*xinc
        t=x
        tbthe(jx,jy)=fthex(t,pk)
      enddo
    enddo

  end subroutine


  function fthe(t,pk)

































    implicit none
    real(krealfp) fthe
    real(krealfp),intent(in):: t,pk
    integer jx,jy
    real(krealfp) xj,yj,ftx1,ftx2

    xj=min(c1xthe+c2xthe*t,real(nxthe,krealfp))
    yj=min(c1ythe+c2ythe*pk,real(nythe,krealfp))
    if(xj.ge.1..and.yj.ge.1.) then
      jx=min(xj,nxthe-1._krealfp)
      jy=min(yj,nythe-1._krealfp)
      ftx1=tbthe(jx,jy)+(xj-jx)*(tbthe(jx+1,jy)-tbthe(jx,jy))
      ftx2=tbthe(jx,jy+1)+(xj-jx)*(tbthe(jx+1,jy+1)-tbthe(jx,jy+1))
      fthe=ftx1+(yj-jy)*(ftx2-ftx1)
    else
      fthe=0.
    endif

  end function


  function ftheq(t,pk)

































    implicit none
    real(krealfp) ftheq
    real(krealfp),intent(in):: t,pk
    integer jx,jy
    real(krealfp) xj,yj,dxj,dyj
    real(krealfp) ft11,ft12,ft13,ft21,ft22,ft23,ft31,ft32,ft33
    real(krealfp) ftx1,ftx2,ftx3

    xj=min(c1xthe+c2xthe*t,real(nxthe,krealfp))
    yj=min(c1ythe+c2ythe*pk,real(nythe,krealfp))
    if(xj.ge.1..and.yj.ge.1.) then
      jx=min(max(nint(xj),2),nxthe-1)
      jy=min(max(nint(yj),2),nythe-1)
      dxj=xj-jx
      dyj=yj-jy
      ft11=tbthe(jx-1,jy-1)
      ft12=tbthe(jx-1,jy)
      ft13=tbthe(jx-1,jy+1)
      ft21=tbthe(jx,jy-1)
      ft22=tbthe(jx,jy)
      ft23=tbthe(jx,jy+1)
      ft31=tbthe(jx+1,jy-1)
      ft32=tbthe(jx+1,jy)
      ft33=tbthe(jx+1,jy+1)
      ftx1=(((ft31+ft11)/2-ft21)*dxj+(ft31-ft11)/2)*dxj+ft21
      ftx2=(((ft32+ft12)/2-ft22)*dxj+(ft32-ft12)/2)*dxj+ft22
      ftx3=(((ft33+ft13)/2-ft23)*dxj+(ft33-ft13)/2)*dxj+ft23
      ftheq=(((ftx3+ftx1)/2-ftx2)*dyj+(ftx3-ftx1)/2)*dyj+ftx2
    else
      ftheq=0.
    endif

  end function


            function fthex(t,pk)





































    implicit none
    real(krealfp) fthex
    real(krealfp),intent(in):: t,pk
    real(krealfp) p,tr,pv,pd,el,expo,expmax

    p=pk**con_cpor
    tr=con_ttp/t
    pv=psatb*(tr**con_xpona)*exp(con_xponb*(1.-tr))
    pd=p-pv
    if(pd.gt.pv) then
      el=con_hvap+con_dldt*(t-con_ttp)
      expo=el*con_eps*pv/(con_cp*t*pd)
      fthex=t*pd**(-con_rocp)*exp(expo)
    else
      fthex=0.
    endif

  end function

  subroutine gtma




























    implicit none
    integer jx,jy
    real(krealfp) xmin,xmax,ymin,ymax,xinc,yinc,x,y,pk,the,t,q,tg

    xmin=200._krealfp
    xmax=500._krealfp
    ymin=0.01_krealfp**con_rocp
    ymax=1.10_krealfp**con_rocp
    xinc=(xmax-xmin)/(nxma-1)
    c1xma=1.-xmin/xinc
    c2xma=1./xinc
    yinc=(ymax-ymin)/(nyma-1)
    c1yma=1.-ymin/yinc
    c2yma=1./yinc
    do jy=1,nyma
      y=ymin+(jy-1)*yinc
      pk=y
      tg=xmin*y
      do jx=1,nxma
        x=xmin+(jx-1)*xinc
        the=x
        call stmaxg(tg,the,pk,t,q)
        tbtma(jx,jy)=t
        tbqma(jx,jy)=q
        tg=t
      enddo
    enddo

  end subroutine


  subroutine stma(the,pk,tma,qma)



































    implicit none
    real(krealfp),intent(in):: the,pk
    real(krealfp),intent(out):: tma,qma
    integer jx,jy
    real(krealfp) xj,yj,ftx1,ftx2,qx1,qx2

    xj=min(max(c1xma+c2xma*the,1._krealfp),real(nxma,krealfp))
    yj=min(max(c1yma+c2yma*pk,1._krealfp),real(nyma,krealfp))
    jx=min(xj,nxma-1._krealfp)
    jy=min(yj,nyma-1._krealfp)
    ftx1=tbtma(jx,jy)+(xj-jx)*(tbtma(jx+1,jy)-tbtma(jx,jy))
    ftx2=tbtma(jx,jy+1)+(xj-jx)*(tbtma(jx+1,jy+1)-tbtma(jx,jy+1))
    tma=ftx1+(yj-jy)*(ftx2-ftx1)
    qx1=tbqma(jx,jy)+(xj-jx)*(tbqma(jx+1,jy)-tbqma(jx,jy))
    qx2=tbqma(jx,jy+1)+(xj-jx)*(tbqma(jx+1,jy+1)-tbqma(jx,jy+1))
    qma=qx1+(yj-jy)*(qx2-qx1)

  end subroutine


  subroutine stmaq(the,pk,tma,qma)



































    implicit none
    real(krealfp),intent(in):: the,pk
    real(krealfp),intent(out):: tma,qma
    integer jx,jy
    real(krealfp) xj,yj,dxj,dyj
    real(krealfp) ft11,ft12,ft13,ft21,ft22,ft23,ft31,ft32,ft33
    real(krealfp) ftx1,ftx2,ftx3
    real(krealfp) q11,q12,q13,q21,q22,q23,q31,q32,q33,qx1,qx2,qx3

    xj=min(max(c1xma+c2xma*the,1._krealfp),real(nxma,krealfp))
    yj=min(max(c1yma+c2yma*pk,1._krealfp),real(nyma,krealfp))
    jx=min(max(nint(xj),2),nxma-1)
    jy=min(max(nint(yj),2),nyma-1)
    dxj=xj-jx
    dyj=yj-jy
    ft11=tbtma(jx-1,jy-1)
    ft12=tbtma(jx-1,jy)
    ft13=tbtma(jx-1,jy+1)
    ft21=tbtma(jx,jy-1)
    ft22=tbtma(jx,jy)
    ft23=tbtma(jx,jy+1)
    ft31=tbtma(jx+1,jy-1)
    ft32=tbtma(jx+1,jy)
    ft33=tbtma(jx+1,jy+1)
    ftx1=(((ft31+ft11)/2-ft21)*dxj+(ft31-ft11)/2)*dxj+ft21
    ftx2=(((ft32+ft12)/2-ft22)*dxj+(ft32-ft12)/2)*dxj+ft22
    ftx3=(((ft33+ft13)/2-ft23)*dxj+(ft33-ft13)/2)*dxj+ft23
    tma=(((ftx3+ftx1)/2-ftx2)*dyj+(ftx3-ftx1)/2)*dyj+ftx2
    q11=tbqma(jx-1,jy-1)
    q12=tbqma(jx-1,jy)
    q13=tbqma(jx-1,jy+1)
    q21=tbqma(jx,jy-1)
    q22=tbqma(jx,jy)
    q23=tbqma(jx,jy+1)
    q31=tbqma(jx+1,jy-1)
    q32=tbqma(jx+1,jy)
    q33=tbqma(jx+1,jy+1)
    qx1=(((q31+q11)/2-q21)*dxj+(q31-q11)/2)*dxj+q21
    qx2=(((q32+q12)/2-q22)*dxj+(q32-q12)/2)*dxj+q22
    qx3=(((q33+q13)/2-q23)*dxj+(q33-q13)/2)*dxj+q23
    qma=(((qx3+qx1)/2-qx2)*dyj+(qx3-qx1)/2)*dyj+qx2

  end subroutine


  subroutine stmax(the,pk,tma,qma)



































    implicit none
    real(krealfp),intent(in):: the,pk
    real(krealfp),intent(out):: tma,qma
    real(krealfp) tg,qg

    call stma(the,pk,tg,qg)
    call stmaxg(tg,the,pk,tma,qma)

  end subroutine


  subroutine stmaxg(tg,the,pk,tma,qma)











































    implicit none
    real(krealfp),intent(in):: tg,the,pk
    real(krealfp),intent(out):: tma,qma
    real(krealfp),parameter:: terrm=1.e-4
    real(krealfp) t,p,tr,pv,pd,el,expo,thet,dthet,terr
    integer i

    t=tg
    p=pk**con_cpor
    do i=1,100
      tr=con_ttp/t
      pv=psatb*(tr**con_xpona)*exp(con_xponb*(1.-tr))
      pd=p-pv
      el=con_hvap+con_dldt*(t-con_ttp)
      expo=el*con_eps*pv/(con_cp*t*pd)
      thet=t*pd**(-con_rocp)*exp(expo)
      dthet=thet/t*(1.+expo*(con_dldt*t/el+el*p/(con_rv*t*pd)))
      terr=(thet-the)/dthet
      t=t-terr
      if(abs(terr).le.terrm) exit
    enddo
    tma=t
    tr=con_ttp/t
    pv=psatb*(tr**con_xpona)*exp(con_xponb*(1.-tr))
    pd=p-pv
    qma=con_eps*pv/(pd+con_eps*pv)

  end subroutine

  subroutine gpkap

























    implicit none
    integer jx
    real(krealfp) xmin,xmax,xinc,x,p

    xmin=0._krealfp
    xmax=110000._krealfp
    xinc=(xmax-xmin)/(nxpkap-1)
    c1xpkap=1.-xmin/xinc
    c2xpkap=1./xinc
    do jx=1,nxpkap
      x=xmin+(jx-1)*xinc
      p=x
      tbpkap(jx)=fpkapx(p)
    enddo

  end subroutine


  function fpkap(p)

































    implicit none
    real(krealfp) fpkap
    real(krealfp),intent(in):: p
    integer jx
    real(krealfp) xj

    xj=min(max(c1xpkap+c2xpkap*p,1._krealfp),real(nxpkap,krealfp))
    jx=min(xj,nxpkap-1._krealfp)
    fpkap=tbpkap(jx)+(xj-jx)*(tbpkap(jx+1)-tbpkap(jx))

  end function


  function fpkapq(p)

































    implicit none
    real(krealfp) fpkapq
    real(krealfp),intent(in):: p
    integer jx
    real(krealfp) xj,dxj,fj1,fj2,fj3

    xj=min(max(c1xpkap+c2xpkap*p,1._krealfp),real(nxpkap,krealfp))
    jx=min(max(nint(xj),2),nxpkap-1)
    dxj=xj-jx
    fj1=tbpkap(jx-1)
    fj2=tbpkap(jx)
    fj3=tbpkap(jx+1)
    fpkapq=(((fj3+fj1)/2-fj2)*dxj+(fj3-fj1)/2)*dxj+fj2

  end function

  function fpkapo(p)
































    implicit none
    real(krealfp) fpkapo
    real(krealfp),intent(in):: p
    integer,parameter:: nnpk=2,ndpk=4
    real(krealfp):: cnpk(0:nnpk)=(/3.13198449e-1,5.78544829e-2,&
                                         8.35491871e-4/)
    real(krealfp):: cdpk(0:ndpk)=(/1.,8.15968401e-2,5.72839518e-4,&
                                         -4.86959812e-7,5.24459889e-10/)
    integer n
    real(krealfp) pkpa,fnpk,fdpk

    pkpa=p*1.e-3_krealfp
    fnpk=cnpk(nnpk)
    do n=nnpk-1,0,-1
      fnpk=pkpa*fnpk+cnpk(n)
    enddo
    fdpk=cdpk(ndpk)
    do n=ndpk-1,0,-1
      fdpk=pkpa*fdpk+cdpk(n)
    enddo
    fpkapo=fnpk/fdpk

  end function


  function fpkapx(p)

























    implicit none
    real(krealfp) fpkapx
    real(krealfp),intent(in):: p

    fpkapx=(p/1.e5_krealfp)**con_rocp

  end function

  subroutine grkap

























    implicit none
    integer jx
    real(krealfp) xmin,xmax,xinc,x,p

    xmin=0._krealfp
    xmax=fpkapx(110000._krealfp)
    xinc=(xmax-xmin)/(nxrkap-1)
    c1xrkap=1.-xmin/xinc
    c2xrkap=1./xinc
    do jx=1,nxrkap
      x=xmin+(jx-1)*xinc
      p=x
      tbrkap(jx)=frkapx(p)
    enddo

  end subroutine


  function frkap(pkap)
































    implicit none
    real(krealfp) frkap
    real(krealfp),intent(in):: pkap
    integer jx
    real(krealfp) xj

    xj=min(max(c1xrkap+c2xrkap*pkap,1._krealfp),real(nxrkap,krealfp))
    jx=min(xj,nxrkap-1._krealfp)
    frkap=tbrkap(jx)+(xj-jx)*(tbrkap(jx+1)-tbrkap(jx))

  end function


  function frkapq(pkap)
































    implicit none
    real(krealfp) frkapq
    real(krealfp),intent(in):: pkap
    integer jx
    real(krealfp) xj,dxj,fj1,fj2,fj3

    xj=min(max(c1xrkap+c2xrkap*pkap,1._krealfp),real(nxrkap,krealfp))
    jx=min(max(nint(xj),2),nxrkap-1)
    dxj=xj-jx
    fj1=tbrkap(jx-1)
    fj2=tbrkap(jx)
    fj3=tbrkap(jx+1)
    frkapq=(((fj3+fj1)/2-fj2)*dxj+(fj3-fj1)/2)*dxj+fj2

  end function


  function frkapx(pkap)

























    implicit none
    real(krealfp) frkapx
    real(krealfp),intent(in):: pkap

    frkapx=pkap**(1/con_rocp)*1.e5_krealfp

  end function

  subroutine gtlcl

























    implicit none
    integer jx,jy
    real(krealfp) xmin,xmax,ymin,ymax,xinc,yinc,x,y,tdpd,t

    xmin=180._krealfp
    xmax=330._krealfp
    ymin=0._krealfp
    ymax=60._krealfp
    xinc=(xmax-xmin)/(nxtlcl-1)
    c1xtlcl=1.-xmin/xinc
    c2xtlcl=1./xinc
    yinc=(ymax-ymin)/(nytlcl-1)
    c1ytlcl=1.-ymin/yinc
    c2ytlcl=1./yinc
    do jy=1,nytlcl
      y=ymin+(jy-1)*yinc
      tdpd=y
      do jx=1,nxtlcl
        x=xmin+(jx-1)*xinc
        t=x
        tbtlcl(jx,jy)=ftlclx(t,tdpd)
      enddo
    enddo

  end subroutine


  function ftlcl(t,tdpd)






























    implicit none
    real(krealfp) ftlcl
    real(krealfp),intent(in):: t,tdpd
    integer jx,jy
    real(krealfp) xj,yj,ftx1,ftx2

    xj=min(max(c1xtlcl+c2xtlcl*t,1._krealfp),real(nxtlcl,krealfp))
    yj=min(max(c1ytlcl+c2ytlcl*tdpd,1._krealfp),real(nytlcl,krealfp))
    jx=min(xj,nxtlcl-1._krealfp)
    jy=min(yj,nytlcl-1._krealfp)
    ftx1=tbtlcl(jx,jy)+(xj-jx)*(tbtlcl(jx+1,jy)-tbtlcl(jx,jy))
    ftx2=tbtlcl(jx,jy+1)+(xj-jx)*(tbtlcl(jx+1,jy+1)-tbtlcl(jx,jy+1))
    ftlcl=ftx1+(yj-jy)*(ftx2-ftx1)

  end function


  function ftlclq(t,tdpd)






























    implicit none
    real(krealfp) ftlclq
    real(krealfp),intent(in):: t,tdpd
    integer jx,jy
    real(krealfp) xj,yj,dxj,dyj
    real(krealfp) ft11,ft12,ft13,ft21,ft22,ft23,ft31,ft32,ft33
    real(krealfp) ftx1,ftx2,ftx3

    xj=min(max(c1xtlcl+c2xtlcl*t,1._krealfp),real(nxtlcl,krealfp))
    yj=min(max(c1ytlcl+c2ytlcl*tdpd,1._krealfp),real(nytlcl,krealfp))
    jx=min(max(nint(xj),2),nxtlcl-1)
    jy=min(max(nint(yj),2),nytlcl-1)
    dxj=xj-jx
    dyj=yj-jy
    ft11=tbtlcl(jx-1,jy-1)
    ft12=tbtlcl(jx-1,jy)
    ft13=tbtlcl(jx-1,jy+1)
    ft21=tbtlcl(jx,jy-1)
    ft22=tbtlcl(jx,jy)
    ft23=tbtlcl(jx,jy+1)
    ft31=tbtlcl(jx+1,jy-1)
    ft32=tbtlcl(jx+1,jy)
    ft33=tbtlcl(jx+1,jy+1)
    ftx1=(((ft31+ft11)/2-ft21)*dxj+(ft31-ft11)/2)*dxj+ft21
    ftx2=(((ft32+ft12)/2-ft22)*dxj+(ft32-ft12)/2)*dxj+ft22
    ftx3=(((ft33+ft13)/2-ft23)*dxj+(ft33-ft13)/2)*dxj+ft23
    ftlclq=(((ftx3+ftx1)/2-ftx2)*dyj+(ftx3-ftx1)/2)*dyj+ftx2

  end function

  function ftlclo(t,tdpd)































    implicit none
    real(krealfp) ftlclo
    real(krealfp),intent(in):: t,tdpd
    real(krealfp),parameter:: clcl1= 0.954442e+0,clcl2= 0.967772e-3,&
                                    clcl3=-0.710321e-3,clcl4=-0.270742e-5

    ftlclo=t-tdpd*(clcl1+clcl2*t+tdpd*(clcl3+clcl4*t))

  end function


  function ftlclx(t,tdpd)









































    implicit none
    real(krealfp) ftlclx
    real(krealfp),intent(in):: t,tdpd
    real(krealfp),parameter:: terrm=1.e-4,tlmin=180.,tlminx=tlmin-5.
    real(krealfp) tr,pvdew,tlcl,ta,pvlcl,el,dpvlcl,terr,terrp
    integer i

    tr=con_ttp/(t-tdpd)
    pvdew=con_psat*(tr**con_xpona)*exp(con_xponb*(1.-tr))
    tlcl=t-tdpd
    do i=1,100
      tr=con_ttp/tlcl
      ta=t/tlcl
      pvlcl=con_psat*(tr**con_xpona)*exp(con_xponb*(1.-tr))*ta**(1/con_rocp)
      el=con_hvap+con_dldt*(tlcl-con_ttp)
      dpvlcl=(el/(con_rv*t**2)+1/(con_rocp*tlcl))*pvlcl
      terr=(pvlcl-pvdew)/dpvlcl
      tlcl=tlcl-terr
      if(abs(terr).le.terrm.or.tlcl.lt.tlminx) exit
    enddo
    ftlclx=max(tlcl,tlmin)

  end function

  subroutine gfuncphys
































    implicit none

    call gpvsl
    call gpvsi
    call gpvs
    call gtdpl
    call gtdpi
    call gtdp
    call gthe
    call gtma
    call gpkap
    call grkap
    call gtlcl

  end subroutine

end module module_gfs_funcphys
