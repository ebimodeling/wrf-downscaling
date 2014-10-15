










MODULE module_mixactivate
PRIVATE
PUBLIC prescribe_aerosol_mixactivate, mixactivate
CONTAINS






      subroutine prescribe_aerosol_mixactivate (                      &
		grid_id, ktau, dtstep, naer,                          &
		rho_phy, th_phy, pi_phy, w, cldfra, cldfra_old,       &
		z, dz8w, p_at_w, t_at_w, exch_h,                      &
        qv, qc, qi, qndrop3d,                                 &
        nsource,                                              &
		ids,ide, jds,jde, kds,kde,                            &
		ims,ime, jms,jme, kms,kme,                            &
		its,ite, jts,jte, kts,kte,                            &
		f_qc, f_qi                                            )





	implicit none


	integer, intent(in) ::                  &
		grid_id, ktau,                  &
		ids, ide, jds, jde, kds, kde,   &
		ims, ime, jms, jme, kms, kme,   &
		its, ite, jts, jte, kts, kte

	real, intent(in) :: dtstep
	real, intent(inout) :: naer 

	real, intent(in),   &
		dimension( ims:ime, kms:kme, jms:jme ) :: &
		rho_phy, th_phy, pi_phy, w,  &
		z, dz8w, p_at_w, t_at_w, exch_h

	real, intent(inout),   &
		dimension( ims:ime, kms:kme, jms:jme ) :: cldfra, cldfra_old

	real, intent(in),   &
		dimension( ims:ime, kms:kme, jms:jme ) :: &
		qv, qc, qi

	real, intent(inout),   &
		dimension( ims:ime, kms:kme, jms:jme ) :: &
		qndrop3d

	real, intent(out),   &
		dimension( ims:ime, kms:kme, jms:jme) :: nsource

    LOGICAL, OPTIONAL :: f_qc, f_qi


	integer maxd_aphase, maxd_atype, maxd_asize, maxd_acomp, max_chem
	parameter (maxd_aphase=2,maxd_atype=1,maxd_asize=1,maxd_acomp=1, max_chem=10)
	real ddvel(its:ite, jts:jte, max_chem) 
	real qsrflx(ims:ime, jms:jme, max_chem) 
	real chem(ims:ime, kms:kme, jms:jme, max_chem) 
	integer i,j,k,l,m,n,p
	real hygro( its:ite, kts:kte, jts:jte, maxd_asize, maxd_atype ) 
	integer ntype_aer, nsize_aer(maxd_atype),ncomp_aer(maxd_atype), nphase_aer
      	integer massptr_aer( maxd_acomp, maxd_asize, maxd_atype, maxd_aphase ),   &
      	  waterptr_aer( maxd_asize, maxd_atype ),   &
      	  numptr_aer( maxd_asize, maxd_atype, maxd_aphase ), &
	  ai_phase, cw_phase
        real dlo_sect( maxd_asize, maxd_atype ),   & 
             dhi_sect( maxd_asize, maxd_atype ),   & 
	     sigmag_aer(maxd_asize, maxd_atype),   & 
	     dgnum_aer(maxd_asize, maxd_atype),    & 
	     dens_aer( maxd_acomp, maxd_atype),    & 
	     mw_aer( maxd_acomp, maxd_atype),      & 
	     dpvolmean_aer(maxd_asize, maxd_atype)   


	real, dimension(ims:ime,kms:kme,jms:jme) :: &
	     ccn1,ccn2,ccn3,ccn4,ccn5,ccn6  
	integer idrydep_onoff
	real, dimension(ims:ime,kms:kme,jms:jme) :: t_phy
	integer msectional


	  integer ptr
	  real maer

      if(naer.lt.1.)then
	     naer=1000.e6 
      endif
	  ai_phase=1
	  cw_phase=2
	  idrydep_onoff = 0
	  msectional = 0

	  t_phy(its:ite,kts:kte,jts:jte)=th_phy(its:ite,kts:kte,jts:jte)*pi_phy(its:ite,kts:kte,jts:jte)

      ntype_aer=maxd_atype
      do n=1,ntype_aer
         nsize_aer(n)=maxd_asize
	 ncomp_aer(n)=maxd_acomp
      end do
      nphase_aer=maxd_aphase


       do n=1,ntype_aer
       do m=1,nsize_aer(n)
          dlo_sect( m,n )=0.01e-4    
          dhi_sect( m,n )=0.5e-4    
	  sigmag_aer(m,n)=2.      
	  dgnum_aer(m,n)=0.1e-4       
	  dpvolmean_aer(m,n) = dgnum_aer(m,n) * exp( 1.5 * (log(sigmag_aer(m,n)))**2 )
	  end do
	  do l=1,ncomp_aer(n)
	     dens_aer( l, n)=1.0   
	     mw_aer( l, n)=132. 
	  end do
      end do
       ptr=0
       do p=1,nphase_aer
       do n=1,ntype_aer
       do m=1,nsize_aer(n)
          ptr=ptr+1
          numptr_aer( m, n, p )=ptr
	  if(p.eq.ai_phase)then
	     chem(its:ite,kts:kte,jts:jte,ptr)=naer
	  else
	     chem(its:ite,kts:kte,jts:jte,ptr)=0.
	  endif
	end do 
	end do 
	end do 
       do p=1,maxd_aphase
       do n=1,ntype_aer
       do m=1,nsize_aer(n)
	  do l=1,ncomp_aer(n)
          ptr=ptr+1
	     if(ptr.gt.max_chem)then
	        write(6,*)'ptr,max_chem=',ptr,max_chem,' in prescribe_aerosol_mixactivate'
	        call wrf_error_fatal3("<stdin>",154,&
"1")
	     endif
	     massptr_aer(l, m, n, p)=ptr


	     maer= 1.0e6 * naer * dens_aer(l,n) * ( (3.1416/6.) *   &
                 (dgnum_aer(m,n)**3) * exp( 4.5*((log(sigmag_aer(m,n)))**2) ) )
	     if(p.eq.ai_phase)then
	        chem(its:ite,kts:kte,jts:jte,ptr)=maer
	     else
	        chem(its:ite,kts:kte,jts:jte,ptr)=0.
	     endif
	  end do
	end do 
	end do 
	end do 
       do n=1,ntype_aer
       do m=1,nsize_aer(n)
          ptr=ptr+1
	  if(ptr.gt.max_chem)then
	     write(6,*)'ptr,max_chem=',ptr,max_chem,' in prescribe_aerosol_mixactivate'
	     call wrf_error_fatal3("<stdin>",176,&
"1")
	  endif

	  waterptr_aer(m, n)=-1
	end do 
	end do 
	ddvel(its:ite,jts:jte,:)=0.
    hygro(its:ite,kts:kte,jts:jte,:,:) = 0.5


      call mixactivate(  msectional,     &
            chem,max_chem,qv,qc,qi,qndrop3d,        &
            t_phy, w, ddvel, idrydep_onoff,  &
            maxd_acomp, maxd_asize, maxd_atype, maxd_aphase,   &
            ncomp_aer, nsize_aer, ntype_aer, nphase_aer,  &
            numptr_aer, massptr_aer, dlo_sect, dhi_sect, sigmag_aer, dpvolmean_aer,  &
            dens_aer, mw_aer,           &
            waterptr_aer, hygro,  ai_phase, cw_phase,                &
            ids,ide, jds,jde, kds,kde,                            &
            ims,ime, jms,jme, kms,kme,                            &
            its,ite, jts,jte, kts,kte,                            &
            rho_phy, z, dz8w, p_at_w, t_at_w, exch_h,      &
            cldfra, cldfra_old, qsrflx,         &
            ccn1, ccn2, ccn3, ccn4, ccn5, ccn6, nsource,       &
            grid_id, ktau, dtstep, &
            F_QC=f_qc, F_QI=f_qi                              )


      end subroutine prescribe_aerosol_mixactivate







subroutine mixactivate(  msectional,            &
           chem, num_chem, qv, qc, qi, qndrop3d,         &
           temp, w, ddvel, idrydep_onoff,  &
           maxd_acomp, maxd_asize, maxd_atype, maxd_aphase,   &
           ncomp_aer, nsize_aer, ntype_aer, nphase_aer,  &
           numptr_aer, massptr_aer, dlo_sect, dhi_sect, sigmag_aer, dpvolmean_aer,  &
           dens_aer, mw_aer,               &
           waterptr_aer, hygro, ai_phase, cw_phase,              &
           ids,ide, jds,jde, kds,kde,                            &
           ims,ime, jms,jme, kms,kme,                            &
           its,ite, jts,jte, kts,kte,                            &
           rho, zm, dz8w, p_at_w, t_at_w, kvh,      &
           cldfra, cldfra_old, qsrflx,          &
           ccn1, ccn2, ccn3, ccn4, ccn5, ccn6, nsource,       &
           grid_id, ktau, dtstep, &
           f_qc, f_qi                       )






  USE module_model_constants, only: g, rhowater, xlv, cp, rvovrd, r_d, r_v, mwdry, ep_2
  USE module_radiation_driver, only: cal_cldfra2

  implicit none



  INTEGER, intent(in) ::         grid_id, ktau
  INTEGER, intent(in) ::         num_chem
  integer, intent(in) ::         ids,ide, jds,jde, kds,kde,    &
                                 ims,ime, jms,jme, kms,kme,    &
                                 its,ite, jts,jte, kts,kte

  integer maxd_aphase, nphase_aer, maxd_atype, ntype_aer
  integer maxd_asize, maxd_acomp, nsize_aer(maxd_atype)
  integer, intent(in) ::   &
       ncomp_aer( maxd_atype  ),   &
       massptr_aer( maxd_acomp, maxd_asize, maxd_atype, maxd_aphase ),   &
       waterptr_aer( maxd_asize, maxd_atype ),   &
       numptr_aer( maxd_asize, maxd_atype, maxd_aphase), &
       ai_phase, cw_phase
  integer, intent(in) :: msectional 
  integer, intent(in) :: idrydep_onoff
  real, intent(in)  ::                       &
       dlo_sect( maxd_asize, maxd_atype ),   & 
       dhi_sect( maxd_asize, maxd_atype ),   & 
       sigmag_aer(maxd_asize, maxd_atype),   & 
       dens_aer( maxd_acomp, maxd_atype),    & 
       mw_aer( maxd_acomp, maxd_atype),      & 
       dpvolmean_aer(maxd_asize, maxd_atype)   




  REAL, intent(inout), DIMENSION( ims:ime, kms:kme, jms:jme, num_chem ) :: &
       chem 

  REAL, intent(in), DIMENSION( ims:ime, kms:kme, jms:jme ) :: &
       qv, qc, qi 

  LOGICAL, OPTIONAL :: f_qc, f_qi

  REAL, intent(inout), DIMENSION( ims:ime, kms:kme, jms:jme ) :: &
       qndrop3d    

  real, intent(in) :: dtstep             
  real, intent(in) :: temp(ims:ime, kms:kme, jms:jme)    
  real, intent(in) :: w(ims:ime, kms:kme, jms:jme)   
  real, intent(in) :: rho(ims:ime, kms:kme, jms:jme)    
  REAL, intent(in) :: ddvel( its:ite, jts:jte, num_chem ) 
  real, intent(in) :: zm(ims:ime, kms:kme, jms:jme)     
  real, intent(in) :: dz8w(ims:ime, kms:kme, jms:jme) 
  real, intent(in) :: p_at_w(ims:ime, kms:kme, jms:jme) 
  real, intent(in) :: t_at_w(ims:ime, kms:kme, jms:jme) 
  real, intent(in) :: kvh(ims:ime, kms:kme, jms:jme)    
  real, intent(inout) :: cldfra_old(ims:ime, kms:kme, jms:jme)
  real, intent(inout) :: cldfra(ims:ime, kms:kme, jms:jme)    
  real, intent(in) :: hygro( its:ite, kts:kte, jts:jte, maxd_asize, maxd_atype ) 

  REAL, intent(out), DIMENSION( ims:ime, jms:jme, num_chem ) ::   qsrflx 
  real, intent(out), dimension(ims:ime,kms:kme,jms:jme) :: nsource, &  
       ccn1,ccn2,ccn3,ccn4,ccn5,ccn6  




  real :: dgnum_aer(maxd_asize, maxd_atype) 
  real :: qndrop(kms:kme)      
  real :: lcldfra(kms:kme)     
  real :: lcldfra_old(kms:kme) 
  real :: wtke(kms:kme)        
  real zn(kms:kme)             
  real zs(kms:kme)             
  real, parameter :: zkmin = 0.01
  real, parameter :: zkmax = 100.
  real cs(kms:kme)             
  real csbot(kms:kme)          
  real csbot_cscen(kms:kme)    
  real dz(kms:kme)             

  real wdiab                   

  real, parameter :: wmixmin = 0.2 

  real :: qndrop_new(kms:kme)  
  real :: ekd(kms:kme)         
  real :: ekk(kms:kme)         
  real :: srcn(kms:kme)        
  real, parameter :: sq2pi = 2.5066282746
  real dtinv

  integer km1,kp1
  real wbar,wmix,wmin,wmax
  real dum
  real tmpa, tmpb, tmpc, tmpc1, tmpc2, tmpd, tmpe, tmpf
  real tmpcourno
  real dact
  real fluxntot         
  real fac_srflx
  real depvel_drop, depvel_tmp
  real, parameter :: depvel_uplimit = 1.0 
  real :: surfrate(num_chem) 
  real surfratemax      
  real surfrate_drop    
  real dtmin,tinv,dtt
  integer nsubmix,nsubmix_bnd
  integer i,j,k,m,n,nsub
  real dtmix
  real alogarg
  real qcld
  real pi
  integer nnew,nsav,ntemp
  real :: overlapp(kms:kme),overlapm(kms:kme) 
  real ::  ekkp(kms:kme),ekkm(kms:kme) 


  integer lnum,lnumcw,l,lmass,lmasscw,lsfc,lsfccw,ltype,lsig,lwater
  integer :: ntype(maxd_asize)

  real ::  naerosol(maxd_asize, maxd_atype)    
  real ::  naerosolcw(maxd_asize, maxd_atype)  
  real ::   maerosol(maxd_acomp,maxd_asize, maxd_atype)   
  real ::   maerosolcw(maxd_acomp,maxd_asize, maxd_atype) 
  real ::   maerosol_tot(maxd_asize, maxd_atype)     
  real ::   maerosol_totcw(maxd_asize, maxd_atype)   
  real ::   vaerosol(maxd_asize, maxd_atype) 
  real ::   vaerosolcw(maxd_asize, maxd_atype) 
  real ::   raercol(kms:kme,num_chem,2) 
  real ::   source(kms:kme) 

  real ::   fn(maxd_asize, maxd_atype)         
  real ::   fs(maxd_asize, maxd_atype)         
  real ::   fm(maxd_asize, maxd_atype)         
  integer ::   ncomp(maxd_atype)

  real ::   fluxn(maxd_asize, maxd_atype)      
  real ::   fluxs(maxd_asize, maxd_atype)      
  real ::   fluxm(maxd_asize, maxd_atype)      
  real ::   flux_fullact(kms:kme)              




  real :: nact(kms:kme,maxd_asize, maxd_atype)  
  real :: mact(kms:kme,maxd_asize, maxd_atype)  
  real :: npv(maxd_asize, maxd_atype) 
  real scale

  real :: hygro_aer(maxd_asize, maxd_atype)  
  real :: exp45logsig     
  real :: alogsig(maxd_asize, maxd_atype) 
  integer, parameter :: psat=6  
  real ccn(kts:kte,psat)        
  real, parameter :: supersat(psat)= &
       (/0.02,0.05,0.1,0.2,0.5,1.0/)
  real super(psat) 
  real, parameter :: surften = 0.076 
  real :: ccnfact(psat,maxd_asize, maxd_atype)
  real :: amcube(maxd_asize, maxd_atype) 
  real :: argfactor(maxd_asize, maxd_atype)
  real aten 
  real t0 
  real sm 
  real arg

  integer,parameter :: icheck_colmass = 0
           
           
  integer :: colmass_worst_ij( 2, 0:maxd_acomp, maxd_asize, maxd_atype )
  integer :: colmass_maxworst_i(3)
  real :: colmass_bgn( 0:maxd_acomp, maxd_asize, maxd_atype, maxd_aphase )
  real :: colmass_end( 0:maxd_acomp, maxd_asize, maxd_atype, maxd_aphase )
  real :: colmass_sfc( 0:maxd_acomp, maxd_asize, maxd_atype, maxd_aphase )
  real :: colmass_worst( 0:maxd_acomp, maxd_asize, maxd_atype )
  real :: colmass_maxworst_r
  real :: rhodz( kts:kte ), rhodzsum











  character*8, parameter :: ccn_name(psat)=(/'CCN1','CCN2','CCN3','CCN4','CCN5','CCN6'/)


  colmass_worst(:,:,:) = 0.0
  colmass_worst_ij(:,:,:,:) = -1


  arg = 1.0
  if (abs(0.8427-ERF_ALT(arg))/0.8427>0.001) then
     write (6,*) 'erf_alt(1.0) = ',ERF_ALT(arg)
     call wrf_error_fatal3("<stdin>",432,&
'dropmixnuc: Error function error')
  endif
  arg = 0.0
  if (ERF_ALT(arg) /= 0.0) then
     write (6,*) 'erf_alt(0.0) = ',ERF_ALT(arg)
     call wrf_error_fatal3("<stdin>",438,&
'dropmixnuc: Error function error')
  endif

  pi = 4.*atan(1.0)
  dtinv=1./dtstep

  depvel_drop =  0.1 
  if (idrydep_onoff .le. 0) depvel_drop =  0.0
  depvel_drop =  min(depvel_drop,depvel_uplimit)

  do n=1,ntype_aer
     do m=1,nsize_aer(n)
        ncomp(n)=ncomp_aer(n)
        alogsig(m,n)=alog(sigmag_aer(m,n))
        dgnum_aer(m,n) = dpvolmean_aer(m,n) * exp( -1.5*alogsig(m,n)*alogsig(m,n) )

        
        npv(m,n)=6./(pi*(0.01*dgnum_aer(m,n))**3*exp(4.5*alogsig(m,n)*alogsig(m,n)))
     end do
  end do
  t0=273.15   
  aten=2.*surften/(r_v*t0*rhowater)
  super(:)=0.01*supersat(:)
  do n=1,ntype_aer
     do m=1,nsize_aer(n)
        exp45logsig=exp(4.5*alogsig(m,n)*alogsig(m,n))
        argfactor(m,n)=2./(3.*sqrt(2.)*alogsig(m,n))
        amcube(m,n)=3./(4.*pi*exp45logsig*npv(m,n))
     enddo
  enddo

  IF( PRESENT(F_QC) .AND. PRESENT ( F_QI ) ) THEN
     CALL cal_cldfra2(CLDFRA,qc,qi,f_qc,f_qi,     &
          ids,ide, jds,jde, kds,kde,              &
          ims,ime, jms,jme, kms,kme,              &
          its,ite, jts,jte, kts,kte               )
  END IF

  qsrflx(its:ite,jts:jte,:) = 0.



OVERALL_MAIN_J_LOOP: do j=jts,jte
OVERALL_MAIN_I_LOOP: do i=its,ite





     do k=kts+1,kte
	    zs(k)=1./(zm(i,k,j)-zm(i,k-1,j))
	 enddo
	 zs(kts)=zs(kts+1)
     zs(kte+1)=0.

     do k=kts,kte



        if(f_qi)then
           qcld=qc(i,k,j)+qi(i,k,j)
        else
           qcld=qc(i,k,j)
        endif
        if(qcld.lt.-1..or.qcld.gt.1.)then
           write(6,'(a,g12.2,a,3i5)')'qcld=',qcld,' for i,k,j=',i,k,j
           call wrf_error_fatal3("<stdin>",505,&
"1")
        endif
        if(qcld.gt.1.e-20)then
           lcldfra(k)=cldfra(i,k,j)*qc(i,k,j)/qcld
           lcldfra_old(k)=cldfra_old(i,k,j)*qc(i,k,j)/qcld
        else
           lcldfra(k)=0.
           lcldfra_old(k)=0.
        endif
        qndrop(k)=qndrop3d(i,k,j)

        cs(k)=rho(i,k,j) 
        dz(k)=dz8w(i,k,j)
        do n=1,ntype_aer
           do m=1,nsize_aer(n)
              nact(k,m,n)=0.
              mact(k,m,n)=0.
           enddo
        enddo
        zn(k)=1./(cs(k)*dz(k))
        if(k>kts)then
           ekd(k)=kvh(i,k,j)
           ekd(k)=max(ekd(k),zkmin)
           ekd(k)=min(ekd(k),zkmax)
        else
           ekd(k)=0
        endif

        if(k.eq.kts)then
           wtke(k)=sq2pi*depvel_drop


        else
           wtke(k)=sq2pi*ekd(k)/dz(k)
        endif
        wtke(k)=max(wtke(k),wmixmin)
        nsource(i,k,j)=0.
     enddo
     nsource(i,kte+1,j) = 0.
     qndrop(kte+1)      = 0.
     zn(kte+1)          = 0.

     do k = kts+1, kte
        tmpa = dz(k-1) ; tmpb = dz(k)
        tmpc = tmpa/(tmpa + tmpb)
        csbot(k) = cs(k-1)*(1.0-tmpc) + cs(k)*tmpc
        csbot_cscen(k) = csbot(k)/cs(k)
     end do
     csbot(kts) = cs(kts)
     csbot_cscen(kts) = 1.0
     csbot(kte+1) = cs(kte)
     csbot_cscen(kte+1) = 1.0

    

     surfratemax = 0.0
     nsav=1
     nnew=2
     surfrate_drop=depvel_drop/dz(kts)
     surfratemax = max( surfratemax, surfrate_drop )
     do n=1,ntype_aer
        do m=1,nsize_aer(n)
           lnum=numptr_aer(m,n,ai_phase)
           lnumcw=numptr_aer(m,n,cw_phase)
           if(lnum>0)then
              depvel_tmp = max( 0.0, min( ddvel(i,j,lnum), depvel_uplimit ) )
              surfrate(lnum)=depvel_tmp/dz(kts)
              surfrate(lnumcw)=surfrate_drop
              surfratemax = max( surfratemax, surfrate(lnum) )

              scale = 1.
              raercol(kts:kte,lnumcw,nsav)=chem(i,kts:kte,j,lnumcw)*scale 
              raercol(kts:kte,lnum,nsav)=chem(i,kts:kte,j,lnum)*scale
           endif
           do l=1,ncomp(n)
              lmass=massptr_aer(l,m,n,ai_phase)
              lmasscw=massptr_aer(l,m,n,cw_phase)

              scale = 1.e-9 
              depvel_tmp = max( 0.0, min( ddvel(i,j,lmass), depvel_uplimit ) )
              surfrate(lmass)=depvel_tmp/dz(kts)
              surfrate(lmasscw)=surfrate_drop
              surfratemax = max( surfratemax, surfrate(lmass) )
              raercol(kts:kte,lmasscw,nsav)=chem(i,kts:kte,j,lmasscw)*scale 
              raercol(kts:kte,lmass,nsav)=chem(i,kts:kte,j,lmass)*scale 
           enddo
           lwater=waterptr_aer(m,n)
           if(lwater>0)then
              depvel_tmp = max( 0.0, min( ddvel(i,j,lwater), depvel_uplimit ) )
              surfrate(lwater)=depvel_tmp/dz(kts)
              surfratemax = max( surfratemax, surfrate(lwater) )
              raercol(kts:kte,lwater,nsav)=chem(i,kts:kte,j,lwater) 
             
           endif
        enddo 
     enddo 



     if (icheck_colmass > 0) then


        colmass_bgn(:,:,:,:) = 0.0
        colmass_end(:,:,:,:) = 0.0
        colmass_sfc(:,:,:,:) = 0.0
        rhodz(kts:kte) = 1.0/zn(kts:kte)
        rhodzsum = sum( rhodz(kts:kte) )
        do n=1,ntype_aer
           do m=1,nsize_aer(n)
              lnum=numptr_aer(m,n,ai_phase)
              lnumcw=numptr_aer(m,n,cw_phase)
              if(lnum>0)then
                 colmass_bgn(0,m,n,1) = sum( chem(i,kts:kte,j,lnum  )*rhodz(kts:kte) )
                 colmass_bgn(0,m,n,2) = sum( chem(i,kts:kte,j,lnumcw)*rhodz(kts:kte) )
              endif
              do l=1,ncomp(n)
                 lmass=massptr_aer(l,m,n,ai_phase)
                 lmasscw=massptr_aer(l,m,n,cw_phase)
                 colmass_bgn(l,m,n,1) = sum( chem(i,kts:kte,j,lmass  )*rhodz(kts:kte) )
                 colmass_bgn(l,m,n,2) = sum( chem(i,kts:kte,j,lmasscw)*rhodz(kts:kte) )
              enddo
           enddo 
        enddo 
     endif 





GROW_SHRINK_MAIN_K_LOOP: do k=kts,kte
        km1=max0(k-1,1)
        kp1=min0(k+1,kde-1)









        tmpc1 = max( (lcldfra(k)-lcldfra_old(k)), 0.0 )
        if (k > kts) then



           tmpcourno = dtstep*max(w(i,k,j),0.0)/dz(k)
           tmpc2 = max( (lcldfra(k)-lcldfra(km1)), 0.0 ) * tmpcourno
           tmpc2 = min( tmpc2, 1.0 )

        else
           tmpc2 = 0.0
        endif

        if ((tmpc1 > 0.001) .or. (tmpc2 > 0.001)) then


           wbar=w(i,k,j)+wtke(k)
           wmix=0.
           wmin=0.

           wmax=50.
           wdiab=0


           do n=1,ntype_aer
              do m=1,nsize_aer(n)
                 call loadaer(raercol(1,1,nsav),k,kms,kme,num_chem,    &
                      cs(k), npv(m,n), dlo_sect(m,n),dhi_sect(m,n),             &
                      maxd_acomp, ncomp(n), &
                      grid_id, ktau, i, j, m, n,   &
                      numptr_aer(m,n,ai_phase),numptr_aer(m,n,cw_phase),  &
                      dens_aer(1,n),    &
                      massptr_aer(1,m,n,ai_phase), massptr_aer(1,m,n,cw_phase),  &
                      maerosol(1,m,n), maerosolcw(1,m,n),          &
                      maerosol_tot(m,n), maerosol_totcw(m,n),      &
                      naerosol(m,n), naerosolcw(m,n),                  &
                      vaerosol(m,n), vaerosolcw(m,n) )

                 hygro_aer(m,n)=hygro(i,k,j,m,n)
              enddo
           enddo


           call activate(wbar,wmix,wdiab,wmin,wmax,temp(i,k,j),cs(k), &
                msectional, maxd_atype, ntype_aer, maxd_asize, nsize_aer,    &
                naerosol, vaerosol,  &
                dlo_sect,dhi_sect,sigmag_aer,hygro_aer,              &
                fn,fs,fm,fluxn,fluxs,fluxm,flux_fullact(k), grid_id, ktau, i, j, k )

           do n = 1,ntype_aer
              do m = 1,nsize_aer(n)
                 lnum   = numptr_aer(m,n,ai_phase)
                 lnumcw = numptr_aer(m,n,cw_phase)
                 if (tmpc1 > 0.0) then
                    dact = tmpc1*fn(m,n)*raercol(k,lnum,nsav) 
                 else
                    dact = 0.0
                 endif
                 if (tmpc2 > 0.0) then
                    dact = dact + tmpc2*fn(m,n)*raercol(km1,lnum,nsav) 
                 endif
                 dact = min( dact, 0.99*raercol(k,lnum,nsav) )
                 raercol(k,lnumcw,nsav) = raercol(k,lnumcw,nsav)+dact
                 raercol(k,lnum,  nsav) = raercol(k,lnum,  nsav)-dact
                 qndrop(k) = qndrop(k)+dact
                 nsource(i,k,j) = nsource(i,k,j)+dact*dtinv
                 do l = 1,ncomp(n)
                    lmass   = massptr_aer(l,m,n,ai_phase)
                    lmasscw = massptr_aer(l,m,n,cw_phase)
                    if (tmpc1 > 0.0) then
                       dact = tmpc1*fm(m,n)*raercol(k,lmass,nsav) 
                    else
                       dact = 0.0
                    endif
                    if (tmpc2 > 0.0) then
                       dact = dact + tmpc2*fm(m,n)*raercol(km1,lmass,nsav) 
                    endif
                    dact = min( dact, 0.99*raercol(k,lmass,nsav) )
                    raercol(k,lmasscw,nsav)  =  raercol(k,lmasscw,nsav)+dact
                    raercol(k,lmass,  nsav)  =  raercol(k,lmass,  nsav)-dact
                 enddo
              enddo
           enddo

        endif   


        if(lcldfra(k) < lcldfra_old(k) .and. lcldfra_old(k) > 1.e-20)then   





           nsource(i,k,j)=nsource(i,k,j)+qndrop(k)*(lcldfra(k)-lcldfra_old(k))*dtinv
           qndrop(k)=qndrop(k)*(1.+lcldfra(k)-lcldfra_old(k))


           tmpc = (lcldfra(k)-lcldfra_old(k))/lcldfra_old(k)
           do n=1,ntype_aer
              do m=1,nsize_aer(n)
                 lnum=numptr_aer(m,n,ai_phase)
                 lnumcw=numptr_aer(m,n,cw_phase)
                 if(lnum.gt.0)then
                    dact=raercol(k,lnumcw,nsav)*tmpc
                    raercol(k,lnumcw,nsav)=raercol(k,lnumcw,nsav)+dact
                    raercol(k,lnum,nsav)=raercol(k,lnum,nsav)-dact
                 endif
                 do l=1,ncomp(n)
                    lmass=massptr_aer(l,m,n,ai_phase)
                    lmasscw=massptr_aer(l,m,n,cw_phase)
                    dact=raercol(k,lmasscw,nsav)*tmpc
                    raercol(k,lmasscw,nsav)=raercol(k,lmasscw,nsav)+dact
                    raercol(k,lmass,nsav)=raercol(k,lmass,nsav)-dact
                 enddo
              enddo
           enddo

        endif

     enddo GROW_SHRINK_MAIN_K_LOOP











OLD_CLOUD_MAIN_K_LOOP: do k=kts,kte
        km1=max0(k-1,kts)
        kp1=min0(k+1,kde-1)
        flux_fullact(k) = 0.0
        if(lcldfra(k).gt.0.01)then



              if(lcldfra(k)-lcldfra(km1).gt.0.01.or.k.eq.kts)then




                 wdiab=0
                 wmix=wtke(k) 
                 wbar=w(i,k,j) 



                 wmax=50.
                 ekd(k)=wtke(k)*dz(k)/sq2pi
                 alogarg=max(1.e-20,1/lcldfra(k)-1.)
                 wmin=wbar+wmix*0.25*sq2pi*alog(alogarg)

                 do n=1,ntype_aer
                    do m=1,nsize_aer(n)
                       call loadaer(raercol(1,1,nsav),km1,kms,kme,num_chem,    &
                            cs(k), npv(m,n),dlo_sect(m,n),dhi_sect(m,n),               &
                            maxd_acomp, ncomp(n), &
                            grid_id, ktau, i, j, m, n,   &
                            numptr_aer(m,n,ai_phase),numptr_aer(m,n,cw_phase),  &
                            dens_aer(1,n),   &
                            massptr_aer(1,m,n,ai_phase), massptr_aer(1,m,n,cw_phase),  &
                            maerosol(1,m,n), maerosolcw(1,m,n),          &
                            maerosol_tot(m,n), maerosol_totcw(m,n),      &
                            naerosol(m,n), naerosolcw(m,n),                  &
                            vaerosol(m,n), vaerosolcw(m,n) )

                       hygro_aer(m,n)=hygro(i,k,j,m,n)

                    enddo
                 enddo


                 call activate(wbar,wmix,wdiab,wmin,wmax,temp(i,k,j),cs(k), &
                      msectional, maxd_atype, ntype_aer, maxd_asize, nsize_aer,    &
                      naerosol, vaerosol,  &
                      dlo_sect,dhi_sect, sigmag_aer,hygro_aer,                    &
                      fn,fs,fm,fluxn,fluxs,fluxm,flux_fullact(k), grid_id, ktau, i, j, k )
                 






                 if (k > kts) then
                    if (flux_fullact(k) > 1.0e-20) then
                       tmpa = ekd(k)*zs(k)
                       tmpf = flux_fullact(k)
                       do n=1,ntype_aer
                       do m=1,nsize_aer(n)
                          tmpb = max( fluxn(m,n), 0.0 ) / max( fluxn(m,n), tmpf )
                          fluxn(m,n) = tmpa*tmpb
                          tmpb = max( fluxm(m,n), 0.0 ) / max( fluxm(m,n), tmpf )
                          fluxm(m,n) = tmpa*tmpb
                       enddo
                       enddo
                    else
                       fluxn(:,:) = 0.0
                       fluxm(:,:) = 0.0
                    endif
                 endif

                 if(k.gt.kts)then
                    tmpc = lcldfra(k)-lcldfra(km1)
                 else
                    tmpc=lcldfra(k)
                 endif







                 tmpe = csbot_cscen(k)/(dz(k))
                 fluxntot=0.
                 do n=1,ntype_aer
                 do m=1,nsize_aer(n)
                    fluxn(m,n)=fluxn(m,n)*tmpc

                    fluxm(m,n)=fluxm(m,n)*tmpc
                    lnum=numptr_aer(m,n,ai_phase)
                    fluxntot=fluxntot+fluxn(m,n)*raercol(km1,lnum,nsav)


                    nact(k,m,n)=nact(k,m,n)+fluxn(m,n)*tmpe
                    mact(k,m,n)=mact(k,m,n)+fluxm(m,n)*tmpe
                 enddo
                 enddo
                 flux_fullact(k) = flux_fullact(k)*tmpe
                 nsource(i,k,j)=nsource(i,k,j)+fluxntot*zs(k)
                 fluxntot=fluxntot*cs(k)
              endif


        else


           if(qndrop(k).gt.10000.e6)then
              print *,'i,k,j,lcldfra,qndrop=',i,k,j,lcldfra(k),qndrop(k)
              print *,'cldfra,ql,qi',cldfra(i,k,j),qc(i,k,j),qi(i,k,j)
           endif
           nsource(i,k,j)=nsource(i,k,j)-qndrop(k)*dtinv
           qndrop(k)=0.

           do n=1,ntype_aer
              do m=1,nsize_aer(n)
                 lnum=numptr_aer(m,n,ai_phase)
                 lnumcw=numptr_aer(m,n,cw_phase)
                 if(lnum.gt.0)then
                    raercol(k,lnum,nsav)=raercol(k,lnum,nsav)+raercol(k,lnumcw,nsav)
                    raercol(k,lnumcw,nsav)=0.
                 endif
                 do l=1,ncomp(n)
                    lmass=massptr_aer(l,m,n,ai_phase)
                    lmasscw=massptr_aer(l,m,n,cw_phase)
                    raercol(k,lmass,nsav)=raercol(k,lmass,nsav)+raercol(k,lmasscw,nsav)
                    raercol(k,lmasscw,nsav)=0.
                 enddo
              enddo
           enddo

        endif

     enddo OLD_CLOUD_MAIN_K_LOOP






     ntemp=nsav
     nsav=nnew
     nnew=ntemp



     dtmin=dtstep
     ekk(kts)=0.0


     do k=kts+1,kte
        ekk(k)=ekd(k)*csbot(k)
     enddo
     ekk(kte+1)=0.0
     do k=kts,kte
        ekkp(k)=zn(k)*ekk(k+1)*zs(k+1)
        ekkm(k)=zn(k)*ekk(k)*zs(k)
        tinv=ekkp(k)+ekkm(k)
        if(k.eq.kts)tinv=tinv+surfratemax
        if(tinv.gt.1.e-6)then
           dtt=1./tinv
           dtmin=min(dtmin,dtt)
        endif
     enddo
     dtmix=0.9*dtmin
     nsubmix=dtstep/dtmix+1
     if(nsubmix>100)then
        nsubmix_bnd=100
     else
        nsubmix_bnd=nsubmix
     endif

     dtmix=dtstep/nsubmix
     fac_srflx = -1.0/(zn(1)*nsubmix)
     
     do k=kts,kte
        kp1=min(k+1,kde-1)
        km1=max(k-1,1)
        if(lcldfra(kp1).gt.0)then
           overlapp(k)=min(lcldfra(k)/lcldfra(kp1),1.)
        else
           overlapp(k)=1.
        endif
        if(lcldfra(km1).gt.0)then
           overlapm(k)=min(lcldfra(k)/lcldfra(km1),1.)
        else
           overlapm(k)=1.
        endif
     enddo





OLD_CLOUD_NSUBMIX_LOOP: do nsub=1,nsubmix
        qndrop_new(kts:kte)=qndrop(kts:kte)

        ntemp=nsav
        nsav=nnew
        nnew=ntemp
        srcn(:)=0.0
        do n=1,ntype_aer
           do m=1,nsize_aer(n)
              lnum=numptr_aer(m,n,ai_phase)



              srcn(kts+1:kte)=srcn(kts+1:kte)+nact(kts+1:kte,m,n)*(raercol(kts:kte-1,lnum,nsav))

              srcn(kts      )=srcn(kts      )+nact(kts      ,m,n)*(raercol(kts      ,lnum,nsav))
           enddo
        enddo
        call explmix(qndrop,srcn,ekkp,ekkm,overlapp,overlapm,   &
             qndrop_new,surfrate_drop,kms,kme,kts,kte,dtmix,.false.)
        do n=1,ntype_aer
           do m=1,nsize_aer(n)
              lnum=numptr_aer(m,n,ai_phase)
              lnumcw=numptr_aer(m,n,cw_phase)
              if(lnum>0)then


                 source(kts+1:kte)= nact(kts+1:kte,m,n)*(raercol(kts:kte-1,lnum,nsav))

                 source(kts      )= nact(kts      ,m,n)*(raercol(kts      ,lnum,nsav))
                 call explmix(raercol(1,lnumcw,nnew),source,ekkp,ekkm,overlapp,overlapm, &
                      raercol(1,lnumcw,nsav),surfrate(lnumcw),kms,kme,kts,kte,dtmix,&
                      .false.)
                 call explmix(raercol(1,lnum,nnew),source,ekkp,ekkm,overlapp,overlapm,  &
                      raercol(1,lnum,nsav),surfrate(lnum),kms,kme,kts,kte,dtmix, &
                      .true.,raercol(1,lnumcw,nsav))
                 qsrflx(i,j,lnum) = qsrflx(i,j,lnum) + fac_srflx*            &
                      raercol(kts,lnum,nsav)*surfrate(lnum)
                 qsrflx(i,j,lnumcw) = qsrflx(i,j,lnumcw) + fac_srflx*        &
                      raercol(kts,lnumcw,nsav)*surfrate(lnumcw)
                 if (icheck_colmass > 0) then
                    tmpf = dtmix*rhodz(kts)
                    colmass_sfc(0,m,n,1) = colmass_sfc(0,m,n,1) &
                          + raercol(kts,lnum  ,nsav)*surfrate(lnum  )*tmpf
                    colmass_sfc(0,m,n,2) = colmass_sfc(0,m,n,2) &
                          + raercol(kts,lnumcw,nsav)*surfrate(lnumcw)*tmpf
                 endif
              endif
              do l=1,ncomp(n)
                 lmass=massptr_aer(l,m,n,ai_phase)
                 lmasscw=massptr_aer(l,m,n,cw_phase)


                 source(kts+1:kte)= mact(kts+1:kte,m,n)*(raercol(kts:kte-1,lmass,nsav))

                 source(kts      )= mact(kts      ,m,n)*(raercol(kts      ,lmass,nsav))
                 call explmix(raercol(1,lmasscw,nnew),source,ekkp,ekkm,overlapp,overlapm, &
                      raercol(1,lmasscw,nsav),surfrate(lmasscw),kms,kme,kts,kte,dtmix,  &
                      .false.)
                 call explmix(raercol(1,lmass,nnew),source,ekkp,ekkm,overlapp,overlapm,  &
                      raercol(1,lmass,nsav),surfrate(lmass),kms,kme,kts,kte,dtmix,  &
                      .true.,raercol(1,lmasscw,nsav))
                 qsrflx(i,j,lmass) = qsrflx(i,j,lmass) + fac_srflx*          &
                      raercol(kts,lmass,nsav)*surfrate(lmass)
                 qsrflx(i,j,lmasscw) = qsrflx(i,j,lmasscw) + fac_srflx*      &
                      raercol(kts,lmasscw,nsav)*surfrate(lmasscw)
                 if (icheck_colmass > 0) then
                    
                    
                    
                    
                    
                    
                    tmpf = dtmix*rhodz(kts)*1.0e9
                    colmass_sfc(l,m,n,1) = colmass_sfc(l,m,n,1) &
                          + raercol(kts,lmass  ,nsav)*surfrate(lmass  )*tmpf
                    colmass_sfc(l,m,n,2) = colmass_sfc(l,m,n,2) &
                          + raercol(kts,lmasscw,nsav)*surfrate(lmasscw)*tmpf
                 endif
              enddo
              lwater=waterptr_aer(m,n)  
              if(lwater>0)then
                 source(:)=0.
                 call explmix(   raercol(1,lwater,nnew),source,ekkp,ekkm,overlapp,overlapm,   &
                      raercol(1,lwater,nsav),surfrate(lwater),kms,kme,kts,kte,dtmix,  &
                      .true.,source)
              endif
           enddo 
        enddo 

     enddo OLD_CLOUD_NSUBMIX_LOOP





     do k=kts,kte
        if(lcldfra(k).eq.0.)then



           qndrop(k)=0.

           do n=1,ntype_aer
              do m=1,nsize_aer(n)
                 lnum=numptr_aer(m,n,ai_phase)
                 lnumcw=numptr_aer(m,n,cw_phase)
                 if(lnum.gt.0)then
                    raercol(k,lnum,nnew)=raercol(k,lnum,nnew)+raercol(k,lnumcw,nnew)
                    raercol(k,lnumcw,nnew)=0.
                 endif
                 do l=1,ncomp(n)
                    lmass=massptr_aer(l,m,n,ai_phase)
                    lmasscw=massptr_aer(l,m,n,cw_phase)
                    raercol(k,lmass,nnew)=raercol(k,lmass,nnew)+raercol(k,lmasscw,nnew)
                    raercol(k,lmasscw,nnew)=0.
                 enddo
              enddo
           enddo
        endif
     enddo





     do k=kts,kte



        if(qndrop(k).lt.-10.e6.or.qndrop(k).gt.1.e12)then
           write(6,'(a,g12.2,a,3i5)')'after qndrop=',qndrop(k),' for i,k,j=',i,k,j
        endif

        qndrop3d(i,k,j) = max(qndrop(k),1.e-6)

        if(qndrop3d(i,k,j).lt.-10.e6.or.qndrop3d(i,k,j).gt.1.E20)then
           write(6,'(a,g12.2,a,3i5)')'after qndrop3d=',qndrop3d(i,k,j),' for i,k,j=',i,k,j
        endif
        if(qc(i,k,j).lt.-1..or.qc(i,k,j).gt.1.)then
           write(6,'(a,g12.2,a,3i5)')'qc=',qc(i,k,j),' for i,k,j=',i,k,j
           call wrf_error_fatal3("<stdin>",1116,&
"1")
        endif
        if(qi(i,k,j).lt.-1..or.qi(i,k,j).gt.1.)then
           write(6,'(a,g12.2,a,3i5)')'qi=',qi(i,k,j),' for i,k,j=',i,k,j
           call wrf_error_fatal3("<stdin>",1121,&
"1")
        endif
        if(qv(i,k,j).lt.-1..or.qv(i,k,j).gt.1.)then
           write(6,'(a,g12.2,a,3i5)')'qv=',qv(i,k,j),' for i,k,j=',i,k,j
           call wrf_error_fatal3("<stdin>",1126,&
"1")
        endif
        cldfra_old(i,k,j) = cldfra(i,k,j)

     enddo





     ccn(:,:) = 0.
     do n=1,ntype_aer
        do m=1,nsize_aer(n)
           lnum=numptr_aer(m,n,ai_phase)
           lnumcw=numptr_aer(m,n,cw_phase)
           if(lnum.gt.0)then
              
              scale = 1.
              chem(i,kts:kte,j,lnumcw)= raercol(kts:kte,lnumcw,nnew)*scale
              chem(i,kts:kte,j,lnum)= raercol(kts:kte,lnum,nnew)*scale
           endif
           do l=1,ncomp(n)
              lmass=massptr_aer(l,m,n,ai_phase)
              lmasscw=massptr_aer(l,m,n,cw_phase)

              scale = 1.e9
              chem(i,kts:kte,j,lmasscw)=raercol(kts:kte,lmasscw,nnew)*scale 
              chem(i,kts:kte,j,lmass)=raercol(kts:kte,lmass,nnew)*scale 
           enddo
           lwater=waterptr_aer(m,n)
           if(lwater>0)chem(i,kts:kte,j,lwater)=raercol(kts:kte,lwater,nnew) 
           do k=kts,kte
              sm=2.*aten*sqrt(aten/(27.*hygro(i,k,j,m,n)*amcube(m,n)))
              do l=1,psat
                 arg=argfactor(m,n)*log(sm/super(l))
                 if(arg<2)then
                    if(arg<-2)then
                       ccnfact(l,m,n)=1.e-6 
                    else
                       ccnfact(l,m,n)=1.e-6*0.5*ERFC_NUM_RECIPES(arg)
                    endif
                 else
                    ccnfact(l,m,n) = 0.
                 endif


                 ccn(k,l)=ccn(k,l)+(raercol(k,lnum,nnew)+raercol(k,lnumcw,nnew))*cs(k)*ccnfact(l,m,n)
              enddo
           enddo
        enddo
     enddo
     do l=1,psat
        
        if(l.eq.1)ccn1(i,kts:kte,j)=ccn(:,l)
        if(l.eq.2)ccn2(i,kts:kte,j)=ccn(:,l)
        if(l.eq.3)ccn3(i,kts:kte,j)=ccn(:,l)
        if(l.eq.4)ccn4(i,kts:kte,j)=ccn(:,l)
        if(l.eq.5)ccn5(i,kts:kte,j)=ccn(:,l)
        if(l.eq.6)ccn6(i,kts:kte,j)=ccn(:,l)
     end do


     if (icheck_colmass > 0) then

        do n=1,ntype_aer
        do m=1,nsize_aer(n)
           lnum=numptr_aer(m,n,ai_phase)
           lnumcw=numptr_aer(m,n,cw_phase)
           if(lnum>0)then
              colmass_end(0,m,n,1) = sum( chem(i,kts:kte,j,lnum  )*rhodz(kts:kte) )
              colmass_end(0,m,n,2) = sum( chem(i,kts:kte,j,lnumcw)*rhodz(kts:kte) )
           endif
           do l=1,ncomp(n)
              lmass=massptr_aer(l,m,n,ai_phase)
              lmasscw=massptr_aer(l,m,n,cw_phase)
              colmass_end(l,m,n,1) = sum( chem(i,kts:kte,j,lmass  )*rhodz(kts:kte) )
              colmass_end(l,m,n,2) = sum( chem(i,kts:kte,j,lmasscw)*rhodz(kts:kte) )
           enddo
        enddo 
        enddo 

        do n=1,ntype_aer
        do m=1,nsize_aer(n)
           do l=0,ncomp(n)
              
              
              
              tmpa = ( colmass_bgn(l,m,n,1) + colmass_bgn(l,m,n,2) )/rhodzsum
              tmpb = ( colmass_end(l,m,n,1) + colmass_end(l,m,n,2) )/rhodzsum
              tmpc = ( colmass_sfc(l,m,n,1) + colmass_sfc(l,m,n,2) )/rhodzsum

              
              
              tmpd = (tmpb + tmpc) - tmpa
              tmpe = max( tmpa, 1.0e-20 )

              
              if (abs(tmpd) < 1.0e5*tmpe) then
                 tmpf = tmpd/tmpe
              else if (tmpf < 0.0) then
                 tmpf = -1.0e5
              else
                 tmpf = 1.0e5
              end if
              if (abs(tmpf) > abs(colmass_worst(l,m,n))) then
                 colmass_worst(l,m,n) = tmpf
                 colmass_worst_ij(1,l,m,n) = i
                 colmass_worst_ij(2,l,m,n) = j
              endif
           enddo
        enddo 
        enddo 
     endif 


     enddo OVERALL_MAIN_I_LOOP 
     enddo OVERALL_MAIN_J_LOOP 



     if (icheck_colmass > 0) then
        if (icheck_colmass >= 100) write(*,'(a)') &
             'mixactivate colmass worst errors bgn - type, size, comp, err, i, j'
        colmass_maxworst_r = 0.0
        colmass_maxworst_i(:) = -1
        do n=1,ntype_aer
        do m=1,nsize_aer(n)
           do l=0,ncomp(n)
              if (icheck_colmass >= 100) &
                 write(*,'(3i3,1p,e10.2,2i4)') n, m, l, &
                 colmass_worst(l,m,n), colmass_worst_ij(1:2,l,m,n) 
              if (abs(colmass_worst(l,m,n)) > abs(colmass_maxworst_r)) then
                 colmass_maxworst_r = colmass_worst(l,m,n) 
                 colmass_maxworst_i(1) = n
                 colmass_maxworst_i(2) = m
                 colmass_maxworst_i(3) = l
              end if
           enddo
        enddo 
        enddo 
        if ((icheck_colmass >= 10) .or. (abs(colmass_maxworst_r) >= 1.0e-6)) &
             write(*,'(a,3i3,1p,e10.2)') 'mixactivate colmass maxworst', &
             colmass_maxworst_i(1:3), colmass_maxworst_r
     endif 


     return
   end subroutine mixactivate




   subroutine explmix( q, src, ekkp, ekkm, overlapp, overlapm, &
                       qold, surfrate, kms, kme, kts, kte, dt, &
                       is_unact, qactold )





   implicit none
   integer, intent(in) :: kms,kme 
   integer, intent(in) :: kts,kte 
   real, intent(inout) :: q(kms:kme) 
   real, intent(in) :: qold(kms:kme) 
   real, intent(in) :: src(kms:kme) 
   real, intent(in) :: ekkp(kms:kme) 
                      
   real, intent(in) :: ekkm(kms:kme) 
                      
   real, intent(in) :: overlapp(kms:kme) 
   real, intent(in) :: overlapm(kms:kme) 
   real, intent(in) :: surfrate 
   real, intent(in) :: dt 
   logical, intent(in) :: is_unact 
   real, intent(in),optional :: qactold(kms:kme)
          
          
          

   integer k,kp1,km1

   if ( is_unact ) then

      do k=kts,kte
         kp1=min(k+1,kte)
         km1=max(k-1,kts)
         q(k) = qold(k) + dt*( - src(k) + ekkp(k)*(qold(kp1) - qold(k) +  &
                           qactold(kp1)*(1.0-overlapp(k)))               &
                                  + ekkm(k)*(qold(km1) - qold(k) +     &
                           qactold(km1)*(1.0-overlapm(k))) )


             q(k)=max(q(k),0.)

      end do

   else
      do k=kts,kte
         kp1=min(k+1,kte)
         km1=max(k-1,kts)
         q(k) = qold(k) + dt*(src(k) + ekkp(k)*(overlapp(k)*qold(kp1)-qold(k)) +  &
                                    ekkm(k)*(overlapm(k)*qold(km1)-qold(k)) )


            q(k)=max(q(k),0.)

      end do
   end if


   q(kts)=q(kts)-surfrate*qold(kts)*dt


      q(kts)=max(q(kts),0.)


   return
   end subroutine explmix




      subroutine activate(wbar, sigw, wdiab, wminf, wmaxf, tair, rhoair,  &
                      msectional, maxd_atype, ntype_aer, maxd_asize, nsize_aer,    &
                      na, volc, dlo_sect,dhi_sect,sigman, hygro, &
                      fn, fs, fm, fluxn, fluxs, fluxm, flux_fullact, &
                      grid_id, ktau, ii, jj, kk )












      USE module_model_constants, only: g,rhowater, xlv, cp, rvovrd, r_d, r_v, &
              mwdry,svp1,svp2,svp3,ep_2

      implicit none




      integer,intent(in) :: maxd_atype      
      integer,intent(in) :: maxd_asize      
      integer,intent(in) :: ntype_aer       
      integer,intent(in) :: nsize_aer(maxd_atype) 
      integer,intent(in) :: msectional      
      integer,intent(in) :: grid_id         
      integer,intent(in) :: ktau            
      integer,intent(in) :: ii, jj, kk      
      real,intent(in) :: wbar          
      real,intent(in) :: sigw          
      real,intent(in) :: wdiab         
      real,intent(in) :: wminf         
      real,intent(in) :: wmaxf         
      real,intent(in) :: tair          
      real,intent(in) :: rhoair        
      real,intent(in) :: na(maxd_asize,maxd_atype)     
      real,intent(in) :: sigman(maxd_asize,maxd_atype) 
      real,intent(in) :: hygro(maxd_asize,maxd_atype)  
      real,intent(in) :: volc(maxd_asize,maxd_atype)   
      real,intent(in) :: dlo_sect( maxd_asize, maxd_atype ), &  
           dhi_sect( maxd_asize, maxd_atype )     



      real,intent(inout) :: fn(maxd_asize,maxd_atype)    
      real,intent(inout) :: fs(maxd_asize,maxd_atype)    
      real,intent(inout) :: fm(maxd_asize,maxd_atype)    
      real,intent(inout) :: fluxn(maxd_asize,maxd_atype) 
      real,intent(inout) :: fluxs(maxd_asize,maxd_atype) 
      real,intent(inout) :: fluxm(maxd_asize,maxd_atype) 
      real,intent(inout) :: flux_fullact                 






      integer, parameter:: nx=200
      integer iquasisect_option, isectional
      real integ,integf
      real, parameter :: surften = 0.076 
      real, parameter :: p0 = 1013.25e2  
      real, parameter :: t0 = 273.15     
      real ylo(maxd_asize,maxd_atype),yhi(maxd_asize,maxd_atype) 
      real ymean(maxd_asize,maxd_atype) 
      real ycut, lnycut, betayy, betayy2, gammayy, phiyy
      real surfc(maxd_asize,maxd_atype) 
      real sign(maxd_asize,maxd_atype)    
      real alnsign(maxd_asize,maxd_atype) 
      real am(maxd_asize,maxd_atype) 
      real lnhygro(maxd_asize,maxd_atype) 
      real f1(maxd_asize,maxd_atype) 
      real pres 
      real path 
      real diff 
      real conduct 
      real diff0,conduct0
      real es 
      real qs 
      real dqsdt 
      real dqsdp 
      real gg 
      real sqrtg 
      real sm(maxd_asize,maxd_atype) 
      real lnsm(maxd_asize,maxd_atype) 
      real zeta, eta(maxd_asize,maxd_atype)
      real lnsmax 
      real alpha
      real gamma
      real beta
      real gaus
      logical :: top        
      real asub(maxd_asize,maxd_atype),bsub(maxd_asize,maxd_atype) 
      real totn(maxd_atype) 
      real aten 
      real gmrad(maxd_atype) 
      real gmradsq(maxd_atype) 
      real gmlnsig(maxd_atype) 
      real gmsm(maxd_atype) 
      real sumflxn(maxd_asize,maxd_atype)
      real sumflxs(maxd_asize,maxd_atype)
      real sumflxm(maxd_asize,maxd_atype)
      real sumflx_fullact
      real sumfn(maxd_asize,maxd_atype)
      real sumfs(maxd_asize,maxd_atype)
      real sumfm(maxd_asize,maxd_atype)
      real sumns(maxd_atype)
      real fnold(maxd_asize,maxd_atype)   
      real fsold(maxd_asize,maxd_atype)   
      real fmold(maxd_asize,maxd_atype)   
      real wold,gold
      real alogten,alog2,alog3,alogaten
      real alogam
      real rlo(maxd_asize,maxd_atype), rhi(maxd_asize,maxd_atype)
      real rmean(maxd_asize,maxd_atype)
                  
                  
      real ccc
      real dumaa,dumbb
      real wmin,wmax,w,dw,dwmax,dwmin,wnuc,dwnew,wb
      real dfmin,dfmax,fnew,fold,fnmin,fnbar,fsbar,fmbar
      real alw,sqrtalw
      real smax
      real x,arg
      real xmincoeff,xcut
      real z,z1,z2,wf1,wf2,zf1,zf2,gf1,gf2,gf
      real etafactor1,etafactor2(maxd_asize,maxd_atype),etafactor2max
      integer m,n,nw,nwmax


      real, parameter :: eps = 0.3
      real, parameter :: fmax = 0.99
      real, parameter :: sds = 3.


      real third, twothird, sixth, zero, one, two, three

      real, parameter :: sq2  = 1.4142135624
      real, parameter :: sqpi = 1.7724538509
      real, parameter :: pi   = 3.1415926536



















      real, parameter :: nsmall = 1.0e-20    
      real, parameter :: vsmall = 1.0e-37    
      logical bin_is_empty(maxd_asize,maxd_atype), all_bins_empty
      logical bin_is_narrow(maxd_asize,maxd_atype)

      integer idiagaa, ipass_nwloop
      integer idiag_dndy_neg, idiag_fnsm_prob





      top = .false.






      idiag_dndy_neg = 1      
                              
      idiag_fnsm_prob = 1     
                              

      iquasisect_option = 2
      if(msectional.gt.0)then
         isectional = iquasisect_option
      else
         isectional = 0
      endif

      do n=1,ntype_aer


        if(ntype_aer.eq.1.and.nsize_aer(n).eq.1.and.na(1,1).lt.1.e-20)then
         fn(1,1)=0.
         fs(1,1)=0.
         fm(1,1)=0.
         fluxn(1,1)=0.
         fluxs(1,1)=0.
         fluxm(1,1)=0.
         flux_fullact=0.
         return
        endif
      enddo

      zero = 0.0
      one = 1.0
      two = 2.0
      three = 3.0
      third = 1.0/3.0
      twothird = 2.0/3.0 
      sixth = 1.0/6.0

      pres=r_d*rhoair*tair
      diff0=0.211e-4*(p0/pres)*(tair/t0)**1.94
      conduct0=(5.69+0.017*(tair-t0))*4.186e2*1.e-5 
      es=1000.*svp1*exp( svp2*(tair-t0)/(tair-svp3) )
      qs=ep_2*es/(pres-es)
      dqsdt=xlv/(r_v*tair*tair)*qs
      alpha=g*(xlv/(cp*r_v*tair*tair)-1./(r_d*tair))
      gamma=(1+xlv/cp*dqsdt)/(rhoair*qs)
      gg=1./(rhowater/(diff0*rhoair*qs)+xlv*rhowater/(conduct0*tair)*(xlv/(r_v*tair)-1.))
      sqrtg=sqrt(gg)
      beta=4.*pi*rhowater*gg*gamma
      aten=2.*surften/(r_v*tair*rhowater)
      alogaten=log(aten)
      alog2=log(two)
      alog3=log(three)
      ccc=4.*pi*third
      etafactor2max=1.e10/(alpha*wmaxf)**1.5 

      all_bins_empty = .true.
      do n=1,ntype_aer
      totn(n)=0.
      gmrad(n)=0.
      gmradsq(n)=0.
      sumns(n)=0.
      do m=1,nsize_aer(n)
         alnsign(m,n)=log(sigman(m,n))


         bin_is_empty(m,n) = .true.
         if (volc(m,n).gt.vsmall .and. na(m,n).gt.nsmall) then
            bin_is_empty(m,n) = .false.
            all_bins_empty = .false.
            lnhygro(m,n)=log(hygro(m,n))


            am(m,n)=exp(-1.5*alnsign(m,n)*alnsign(m,n))*              &
              (3.*volc(m,n)/(4.*pi*na(m,n)))**third

            if (isectional .gt. 0) then



               totn(n)=totn(n)+na(m,n)
               alogam=log(am(m,n))
               gmrad(n)=gmrad(n)+na(m,n)*alogam
               gmradsq(n)=gmradsq(n)+na(m,n)*alogam*alogam
            endif
            etafactor2(m,n)=1./(na(m,n)*beta*sqrtg)

            if(hygro(m,n).gt.1.e-10)then
               sm(m,n)=2.*aten/(3.*am(m,n))*sqrt(aten/(3.*hygro(m,n)*am(m,n)))
            else
               sm(m,n)=100.
            endif

         else
            sm(m,n)=1.
            etafactor2(m,n)=etafactor2max 

         endif
         lnsm(m,n)=log(sm(m,n))
         if ((isectional .eq. 3) .or. (isectional .eq. 4)) then
            sumns(n)=sumns(n)+na(m,n)/sm(m,n)**twothird
         endif

      end do 
      end do 


         if ( all_bins_empty ) then
            do n=1,ntype_aer
            do m=1,nsize_aer(n)
               fluxn(m,n)=0.
               fn(m,n)=0.
               fluxs(m,n)=0.
               fs(m,n)=0.
               fluxm(m,n)=0.
               fm(m,n)=0.
            end do
            end do
            flux_fullact=0.
            return
         endif



         if (isectional .le. 0) then
            
            
            call maxsat_init(maxd_atype, ntype_aer, &
                 maxd_asize, nsize_aer, alnsign, f1)

            goto 30000
         end if

         do n=1,ntype_aer
            
            
            
            gmrad(n)=gmrad(n)/max(totn(n),1e-20)
            gmlnsig=gmradsq(n)/totn(n)-gmrad(n)*gmrad(n)    
            gmlnsig(n)=sqrt( max( 1.e-4, gmlnsig(n) ) )
            gmrad(n)=exp(gmrad(n))
            if ((isectional .eq. 3) .or. (isectional .eq. 4)) then
               gmsm(n)=totn(n)/sumns(n)
               gmsm(n)=gmsm(n)*gmsm(n)*gmsm(n)
               gmsm(n)=sqrt(gmsm(n))
            else

               gmsm(n)=2.*aten/(3.*gmrad(n))*sqrt(aten/(3.*hygro(nsize_aer(n),n)*gmrad(n)))
            endif
         enddo
         
         
         
         call maxsat_init(maxd_atype, ntype_aer, &
              maxd_asize, (/1/), gmlnsig, f1)






















             do 25002 n = 1,ntype_aer
             do 25000 m = 1,nsize_aer(n)


                rlo(m,n) = 0.5*0.01*dlo_sect(m,n)
                rhi(m,n) = 0.5*0.01*dhi_sect(m,n)
                ylo(m,n) = (rlo(m,n)/rhi(m,n))**3
                yhi(m,n) = 1.0



                bin_is_narrow(m,n) = .false.
                if ((rhi(m,n)/rlo(m,n)) .le. 1.01) bin_is_narrow(m,n) = .true.



                if ( bin_is_empty(m,n) ) then
                   rmean(m,n) = sqrt(rlo(m,n)*rhi(m,n)) 
                   ymean(m,n) = (rmean(m,n)/rhi(m,n))**3
                   goto 25000
                end if

                rmean(m,n) = (volc(m,n)/(ccc*na(m,n)))**third
                rmean(m,n) = max( rlo(m,n), min( rhi(m,n), rmean(m,n) ) )
                ymean(m,n) = (rmean(m,n)/rhi(m,n))**3
                if ( bin_is_narrow(m,n) ) goto 25000



                if ((rhi(m,n)/rmean(m,n)) .le. 1.01) then
                   bin_is_narrow(m,n) = .true.
                   rlo(m,n) = min( rmean(m,n), (rhi(m,n)/1.01) )
                   ylo(m,n) = (rlo(m,n)/rhi(m,n))**3
                   goto 25000
                else if ((rmean(m,n)/rlo(m,n)) .le. 1.01) then
                   bin_is_narrow(m,n) = .true.
                   rhi(m,n) = max( rmean(m,n), (rlo(m,n)*1.01) )
                   ylo(m,n) = (rlo(m,n)/rhi(m,n))**3
                   ymean(m,n) = (rmean(m,n)/rhi(m,n))**3
                   goto 25000
                endif







                gammayy = (ymean(m,n)-ylo(m,n)) / (yhi(m,n)-ylo(m,n))
                if (gammayy .lt. 0.34) then
                   dumaa = ylo(m,n) + (yhi(m,n)-ylo(m,n))*(gammayy/0.34)
                   rhi(m,n) = rhi(m,n)*(dumaa**third)
                   ylo(m,n) = (rlo(m,n)/rhi(m,n))**3
                   ymean(m,n) = (rmean(m,n)/rhi(m,n))**3
                else if (gammayy .ge. 0.66) then
                   dumaa = ylo(m,n) + (yhi(m,n)-ylo(m,n))*((gammayy-0.66)/0.34)
                   ylo(m,n) = dumaa
                   rlo(m,n) = rhi(m,n)*(dumaa**third)
                end if
                if ((rhi(m,n)/rlo(m,n)) .le. 1.01) then
                   bin_is_narrow(m,n) = .true.
                   goto 25000
                end if

                betayy = ylo(m,n)/yhi(m,n)
                betayy2 = betayy*betayy
                bsub(m,n) = (12.0*ymean(m,n) - 6.0*(1.0+betayy)) /   &
                   (4.0*(1.0-betayy2*betayy) - 3.0*(1.0-betayy2)*(1.0+betayy))
                asub(m,n) = (1.0 - bsub(m,n)*(1.0-betayy2)*0.5) / (1.0-betayy)

                if ( asub(m,n)+bsub(m,n)*ylo(m,n) .lt. 0. ) then
                  if (idiag_dndy_neg .gt. 0) then
                    print *,'dndy<0 at lower boundary'
                    print *,'n,m=',n,m
                    print *,'na=',na(m,n),' volc=',volc(m,n)
                    print *,'volc/(na*pi*4/3)=', (volc(m,n)/(na(m,n)*ccc))
                    print *,'rlo(m,n),rhi(m,n)=',rlo(m,n),rhi(m,n)
                    print *,'dlo_sect/2,dhi_sect/2=',   &
                             (0.005*dlo_sect(m,n)),(0.005*dhi_sect(m,n))
                    print *,'asub,bsub,ylo,yhi=',asub(m,n),bsub(m,n),ylo(m,n),yhi(m,n)
                    print *,'asub+bsub*ylo=',   &
                             (asub(m,n)+bsub(m,n)*ylo(m,n))
                    print *,'subr activate error 11 - i,j,k =', ii, jj, kk
                  endif
                endif
                if ( asub(m,n)+bsub(m,n)*yhi(m,n) .lt. 0. ) then
                  if (idiag_dndy_neg .gt. 0) then
                    print *,'dndy<0 at upper boundary'
                    print *,'n,m=',n,m
                    print *,'na=',na(m,n),' volc=',volc(m,n)
                    print *,'volc/(na*pi*4/3)=', (volc(m,n)/(na(m,n)*ccc))
                    print *,'rlo(m,n),rhi(m,n)=',rlo(m,n),rhi(m,n)
                    print *,'dlo_sect/2,dhi_sect/2=',   &
                             (0.005*dlo_sect(m,n)),(0.005*dhi_sect(m,n))
                    print *,'asub,bsub,ylo,yhi=',asub(m,n),bsub(m,n),ylo(m,n),yhi(m,n)
                    print *,'asub+bsub*yhi=',   &
                             (asub(m,n)+bsub(m,n)*yhi(m,n))
                    print *,'subr activate error 12 - i,j,k =', ii, jj, kk
                  endif
                endif

25000        continue      
25002        continue      


30000    continue











      if(sigw.le.1.e-5) goto 50000







         ipass_nwloop = 1
         idiagaa = 0




40000    continue
         if(top)then
           wmax=0.
           wmin=min(zero,-wdiab)
         else
           wmax=min(wmaxf,wbar+sds*sigw)
           wmin=max(wminf,-wdiab)
         endif
         wmin=max(wmin,wbar-sds*sigw)
         w=wmin
         dwmax=eps*sigw
         dw=dwmax
         dfmax=0.2
         dfmin=0.1
         if(wmax.le.w)then
            do n=1,ntype_aer
            do m=1,nsize_aer(n)
               fluxn(m,n)=0.
               fn(m,n)=0.
               fluxs(m,n)=0.
               fs(m,n)=0.
               fluxm(m,n)=0.
               fm(m,n)=0.
            end do
            end do
            flux_fullact=0.
            return
         endif
         do n=1,ntype_aer
         do m=1,nsize_aer(n)
            sumflxn(m,n)=0.
            sumfn(m,n)=0.
            fnold(m,n)=0.
            sumflxs(m,n)=0.
            sumfs(m,n)=0.
            fsold(m,n)=0.
            sumflxm(m,n)=0.
            sumfm(m,n)=0.
            fmold(m,n)=0.
         enddo
         enddo
         sumflx_fullact=0.

         fold=0
         gold=0


         wold=w



         nwmax = 200

         dwmin = (wmax - wmin)/(nwmax-1)
         dwmin = min( dwmax, dwmin )
         dwmin = max( 0.01,  dwmin )






         if (idiagaa.gt.0) then
             write(*,94700) ktau, grid_id, ii, jj, kk, nwmax
             write(*,94710) 'wbar,sigw,wdiab=', wbar, sigw, wdiab
             write(*,94710) 'wmin,wmax,dwmin,dwmax=', wmin, wmax, dwmin, dwmax
             write(*,94720) -1, w, wold, dw
         end if
94700    format( / 'activate 47000 - ktau,id,ii,jj,kk,nwmax=', 6i5 )
94710    format( 'activate 47000 - ', a, 6(1x,f11.5) )
94720    format( 'activate 47000 - nw,w,wold,dw=', i5, 3(1x,f11.5) )

         do 47000 nw = 1, nwmax
41000       wnuc=w+wdiab

            if (idiagaa.gt.0) write(*,94720) nw, w, wold, dw


            alw=alpha*wnuc
            sqrtalw=sqrt(alw)
            zeta=2.*sqrtalw*aten/(3.*sqrtg)
            etafactor1=2.*alw*sqrtalw
            if (isectional .gt. 0) then



              do n=1,ntype_aer
                 if(totn(n).gt.1.e-10)then
                    eta(1,n)=etafactor1/(totn(n)*beta*sqrtg)
                 else
                    eta(1,n)=1.e10
                 endif
              enddo
              call maxsat(zeta,eta,maxd_atype,ntype_aer, &
                   maxd_asize,(/1/),gmsm,gmlnsig,f1,smax)
              lnsmax=log(smax)
              x=2*(log(gmsm(1))-lnsmax)/(3*sq2*gmlnsig(1))
              fnew=0.5*(1.-ERF_ALT(x))

            else

              do n=1,ntype_aer
              do m=1,nsize_aer(n)
                 eta(m,n)=etafactor1*etafactor2(m,n)
              enddo
              enddo

              call maxsat(zeta,eta,maxd_atype,ntype_aer, &
                   maxd_asize,nsize_aer,sm,alnsign,f1,smax)


              lnsmax=log(smax)

              x=2*(lnsm(nsize_aer(1),1)-lnsmax)/(3*sq2*alnsign(nsize_aer(1),1))
              fnew=0.5*(1.-ERF_ALT(x))

            endif

            dwnew = dw


            if(fnew-fold.gt.dfmax.and.nw.gt.1)then

               if (dw .gt. 1.01*dwmin) then
                  dw=0.7*dw
                  dw=max(dw,dwmin)
                  w=wold+dw
                  go to 41000
               else
                  dwnew = dwmin
               endif
            endif

            if(fnew-fold.lt.dfmin)then

               dwnew=min(1.5*dw,dwmax)
            endif
            fold=fnew

            z=(w-wbar)/(sigw*sq2)
            gaus=exp(-z*z)
            fnmin=1.
            xmincoeff=alogaten-2.*third*(lnsmax-alog2)-alog3



            do 44002 n=1,ntype_aer
            do 44000 m=1,nsize_aer(n)
               if ( bin_is_empty(m,n) ) then
                   fn(m,n)=0.
                   fs(m,n)=0.
                   fm(m,n)=0.
               else if ((isectional .eq. 2) .or. (isectional .eq. 4)) then


                  xcut=xmincoeff-third*lnhygro(m,n)




                  lnycut = 3.0 * ( xcut - log(rhi(m,n)) )
                  lnycut = min( lnycut, log(yhi(m,n)*1.0e5) )
                  ycut=exp(lnycut)


                  if(ycut.gt.yhi(m,n))then
                     fn(m,n)=0.
                     fs(m,n)=0.
                     fm(m,n)=0.
                  elseif(ycut.lt.ylo(m,n))then
                     fn(m,n)=1.
                     fs(m,n)=1.
                     fm(m,n)=1.
                  elseif ( bin_is_narrow(m,n) ) then


                     if (ycut.gt.ymean(m,n)) then
                        fn(m,n)=0.
                        fs(m,n)=0.
                        fm(m,n)=0.
                     else
                        fn(m,n)=1.
                        fs(m,n)=1.
                        fm(m,n)=1.
                     endif
                  else
                     phiyy=ycut/yhi(m,n)
                     fn(m,n) = asub(m,n)*(1.0-phiyy) + 0.5*bsub(m,n)*(1.0-phiyy*phiyy)
                     if (fn(m,n).lt.zero .or. fn(m,n).gt.one) then
                      if (idiag_fnsm_prob .gt. 0) then
                        print *,'fn(',m,n,')=',fn(m,n),' outside 0,1 - activate err21'
                        print *,'na,volc       =', na(m,n), volc(m,n)
                        print *,'asub,bsub     =', asub(m,n), bsub(m,n)
                        print *,'yhi,ycut      =', yhi(m,n), ycut
                      endif
                     endif

                     if (fn(m,n) .le. zero) then

                        fn(m,n)=zero
                        fs(m,n)=zero
                        fm(m,n)=zero
                     else if (fn(m,n) .ge. one) then

                        fn(m,n)=one
                        fs(m,n)=one
                        fm(m,n)=one
                     else

                        fm(m,n) = (yhi(m,n)/ymean(m,n)) * (0.5*asub(m,n)*(1.0-phiyy*phiyy) +   &
                                  third*bsub(m,n)*(1.0-phiyy*phiyy*phiyy))
                        if (fm(m,n).lt.fn(m,n) .or. fm(m,n).gt.one) then
                         if (idiag_fnsm_prob .gt. 0) then
                           print *,'fm(',m,n,')=',fm(m,n),' outside fn,1 - activate err22'
                           print *,'na,volc,fn    =', na(m,n), volc(m,n), fn(m,n)
                           print *,'asub,bsub     =', asub(m,n), bsub(m,n)
                           print *,'yhi,ycut     =', yhi(m,n), ycut
                         endif
                        endif
                        if (fm(m,n) .le. fn(m,n)) then

                           fm(m,n)=fn(m,n)
                           fs(m,n)=fn(m,n)
                        else if (fm(m,n) .ge. one) then

                           fm(m,n)=one
                           fs(m,n)=one
                           fn(m,n)=one
                        else


                           dumaa = fn(m,n)*(yhi(m,n)/ymean(m,n)) 
                           fm(m,n) = min( fm(m,n), dumaa )
                           dumaa = 1.0 + (fn(m,n)-1.0)*(ylo(m,n)/ymean(m,n)) 
                           fm(m,n) = min( fm(m,n), dumaa )

                           betayy = ylo(m,n)/yhi(m,n)
                           dumaa = phiyy**twothird
                           dumbb = betayy**twothird
                           fs(m,n) =   &
                              (asub(m,n)*(1.0-phiyy*dumaa) +   &
                                  0.625*bsub(m,n)*(1.0-phiyy*phiyy*dumaa)) /   &
                              (asub(m,n)*(1.0-betayy*dumbb) +   &
                                  0.625*bsub(m,n)*(1.0-betayy*betayy*dumbb))
                           fs(m,n)=max(fs(m,n),fn(m,n))
                           fs(m,n)=min(fs(m,n),fm(m,n))
                        endif
                     endif
                  endif

               else

                  x=2*(lnsm(m,n)-lnsmax)/(3*sq2*alnsign(m,n))
                  fn(m,n)=0.5*(1.-ERF_ALT(x))
                  arg=x-sq2*alnsign(m,n)
                  fs(m,n)=0.5*(1.-ERF_ALT(arg))
                  arg=x-1.5*sq2*alnsign(m,n)
                  fm(m,n)=0.5*(1.-ERF_ALT(arg))

               endif




               fnmin=min(fn(m,n),fnmin)


               wb=(w+wold)
               fnbar=(fn(m,n)*gaus+fnold(m,n)*gold)
               fsbar=(fs(m,n)*gaus+fsold(m,n)*gold)
               fmbar=(fm(m,n)*gaus+fmold(m,n)*gold)
               if((top.and.w.lt.0.).or.(.not.top.and.w.gt.0.))then
                  sumflxn(m,n)=sumflxn(m,n)+sixth*(wb*fnbar           &
                      +(fn(m,n)*gaus*w+fnold(m,n)*gold*wold))*dw
                  sumflxs(m,n)=sumflxs(m,n)+sixth*(wb*fsbar           &
                      +(fs(m,n)*gaus*w+fsold(m,n)*gold*wold))*dw
                  sumflxm(m,n)=sumflxm(m,n)+sixth*(wb*fmbar           &
                      +(fm(m,n)*gaus*w+fmold(m,n)*gold*wold))*dw
               endif
               sumfn(m,n)=sumfn(m,n)+0.5*fnbar*dw


               fnold(m,n)=fn(m,n)
               sumfs(m,n)=sumfs(m,n)+0.5*fsbar*dw
               fsold(m,n)=fs(m,n)
               sumfm(m,n)=sumfm(m,n)+0.5*fmbar*dw
               fmold(m,n)=fm(m,n)

44000       continue      
44002       continue      


            sumflx_fullact = sumflx_fullact &
                           + sixth*(wb*(gaus+gold) + (gaus*w + gold*wold))*dw

            gold=gaus
            wold=w
            dw=dwnew

            if(nw.gt.1.and.(w.gt.wmax.or.fnmin.gt.fmax))go to 48000
            w=w+dw

47000    continue      


         print *,'do loop is too short in activate'
         print *,'wmin=',wmin,' w=',w,' wmax=',wmax,' dw=',dw
         print *,'wbar=',wbar,' sigw=',sigw,' wdiab=',wdiab
         print *,'wnuc=',wnuc
         do n=1,ntype_aer
            print *,'ntype=',n
            print *,'na=',(na(m,n),m=1,nsize_aer(n))
            print *,'fn=',(fn(m,n),m=1,nsize_aer(n))
         end do


         print *,'top,wbar,sigw,wdiab,tair,rhoair,ntype_aer='
         print *, top,wbar,sigw,wdiab,tair,rhoair,ntype_aer
         print *,'na='
         print *, na
         print *,'volc='
         print *, volc
         print *,'sigman='
         print *, sigman
         print *,'hygro='
         print *, hygro

         print *,'subr activate error 31 - i,j,k =', ii, jj, kk

         if (ipass_nwloop .eq. 1) then
             ipass_nwloop = 2
             idiagaa = 2
             goto 40000
         end if
         call wrf_error_fatal3("<stdin>",2172,&
"STOP: activate before 48000")

48000    continue



         if(.not.top.and.w.lt.wmaxf)then



            wnuc=w+wdiab

            z1=(w-wbar)/(sigw*sq2)
            z2=(wmaxf-wbar)/(sigw*sq2)
            integ=sigw*0.5*sq2*sqpi*(ERFC_NUM_RECIPES(z1)-ERFC_NUM_RECIPES(z2))

            wf1=max(w,zero)
            zf1=(wf1-wbar)/(sigw*sq2)
            gf1=exp(-zf1*zf1)
            wf2=max(wmaxf,zero)
            zf2=(wf2-wbar)/(sigw*sq2)
            gf2=exp(-zf2*zf2)
            gf=(gf1-gf2)
            integf=wbar*sigw*0.5*sq2*sqpi*(ERFC_NUM_RECIPES(zf1)-ERFC_NUM_RECIPES(zf2))+sigw*sigw*gf

            do n=1,ntype_aer
            do m=1,nsize_aer(n)
               sumflxn(m,n)=sumflxn(m,n)+integf*fn(m,n)
               sumfn(m,n)=sumfn(m,n)+fn(m,n)*integ
               sumflxs(m,n)=sumflxs(m,n)+integf*fs(m,n)
               sumfs(m,n)=sumfs(m,n)+fs(m,n)*integ
               sumflxm(m,n)=sumflxm(m,n)+integf*fm(m,n)
               sumfm(m,n)=sumfm(m,n)+fm(m,n)*integ
            end do
            end do

            sumflx_fullact = sumflx_fullact + integf

         endif


         do n=1,ntype_aer
         do m=1,nsize_aer(n)


            fn(m,n)=sumfn(m,n)/(sq2*sqpi*sigw)
            fluxn(m,n)=sumflxn(m,n)/(sq2*sqpi*sigw)
            if(fn(m,n).gt.1.01)then
             if (idiag_fnsm_prob .gt. 0) then
               print *,'fn=',fn(m,n),' > 1 - activate err41'
               print *,'w,m,n,na,am=',w,m,n,na(m,n),am(m,n)
               print *,'integ,sumfn,sigw=',integ,sumfn(m,n),sigw
               print *,'subr activate error - i,j,k =', ii, jj, kk
             endif
             fluxn(m,n) = fluxn(m,n)/fn(m,n)
            endif

            fs(m,n)=sumfs(m,n)/(sq2*sqpi*sigw)
            fluxs(m,n)=sumflxs(m,n)/(sq2*sqpi*sigw)
            if(fs(m,n).gt.1.01)then
             if (idiag_fnsm_prob .gt. 0) then
               print *,'fs=',fs(m,n),' > 1 - activate err42'
               print *,'m,n,isectional=',m,n,isectional
               print *,'alnsign(m,n)=',alnsign(m,n)
               print *,'rcut,rlo(m,n),rhi(m,n)',exp(xcut),rlo(m,n),rhi(m,n)
               print *,'w,m,na,am=',w,m,na(m,n),am(m,n)
               print *,'integ,sumfs,sigw=',integ,sumfs(m,n),sigw
             endif
             fluxs(m,n) = fluxs(m,n)/fs(m,n)
            endif


            fm(m,n)=sumfm(m,n)/(sq2*sqpi*sigw)
            fluxm(m,n)=sumflxm(m,n)/(sq2*sqpi*sigw)
            if(fm(m,n).gt.1.01)then
             if (idiag_fnsm_prob .gt. 0) then
               print *,'fm(',m,n,')=',fm(m,n),' > 1 - activate err43'
             endif
             fluxm(m,n) = fluxm(m,n)/fm(m,n)
            endif

         end do
         end do

         flux_fullact = sumflx_fullact/(sq2*sqpi*sigw)

      goto 60000













50000 continue

         wnuc=wbar+wdiab



         if(wnuc.le.0.)then
            do n=1,ntype_aer
            do m=1,nsize_aer(n)
               fn(m,n)=0
               fluxn(m,n)=0
               fs(m,n)=0
               fluxs(m,n)=0
               fm(m,n)=0
               fluxm(m,n)=0
            end do
            end do
            flux_fullact=0.
            return
         endif

            w=wbar
            alw=alpha*wnuc
            sqrtalw=sqrt(alw)
            zeta=2.*sqrtalw*aten/(3.*sqrtg)

            if (isectional .gt. 0) then


              do n=1,ntype_aer
              if(totn(n).gt.1.e-10)then
                 eta(1,n)=2*alw*sqrtalw/(totn(n)*beta*sqrtg)
              else
                 eta(1,n)=1.e10
              endif
              end do
               call maxsat(zeta,eta,maxd_atype,ntype_aer, &
                    maxd_asize,(/1/),gmsm,gmlnsig,f1,smax)

            else

              do n=1,ntype_aer
              do m=1,nsize_aer(n)
                 if(na(m,n).gt.1.e-10)then
                    eta(m,n)=2*alw*sqrtalw/(na(m,n)*beta*sqrtg)
                 else
                    eta(m,n)=1.e10
                 endif
              end do
              end do

              call maxsat(zeta,eta,maxd_atype,ntype_aer, &
                   maxd_asize,nsize_aer,sm,alnsign,f1,smax)

            endif

            lnsmax=log(smax)
            xmincoeff=alogaten-2.*third*(lnsmax-alog2)-alog3

            do 55002 n=1,ntype_aer
            do 55000 m=1,nsize_aer(n)


               if ( bin_is_empty(m,n) ) then
                   fn(m,n)=0.
                   fs(m,n)=0.
                   fm(m,n)=0.

               else if ((isectional .eq. 2) .or. (isectional .eq. 4)) then


                  xcut=xmincoeff-third*lnhygro(m,n)




                  lnycut = 3.0 * ( xcut - log(rhi(m,n)) )
                  lnycut = min( lnycut, log(yhi(m,n)*1.0e5) )
                  ycut=exp(lnycut)


                  if(ycut.gt.yhi(m,n))then
                     fn(m,n)=0.
                     fs(m,n)=0.
                     fm(m,n)=0.

                  elseif(ycut.lt.ylo(m,n))then
                     fn(m,n)=1.
                     fs(m,n)=1.
                     fm(m,n)=1.
                  elseif ( bin_is_narrow(m,n) ) then


                     if (ycut.gt.ymean(m,n)) then
                        fn(m,n)=0.
                        fs(m,n)=0.
                        fm(m,n)=0.
                     else
                        fn(m,n)=1.
                        fs(m,n)=1.
                        fm(m,n)=1.
                     endif
                  else
                     phiyy=ycut/yhi(m,n)
                     fn(m,n) = asub(m,n)*(1.0-phiyy) + 0.5*bsub(m,n)*(1.0-phiyy*phiyy)
                     if (fn(m,n).lt.zero .or. fn(m,n).gt.one) then
                      if (idiag_fnsm_prob .gt. 0) then
                        print *,'fn(',m,n,')=',fn(m,n),' outside 0,1 - activate err21'
                        print *,'na,volc       =', na(m,n), volc(m,n)
                        print *,'asub,bsub     =', asub(m,n), bsub(m,n)
                        print *,'yhi,ycut      =', yhi(m,n), ycut
                      endif
                     endif

                     if (fn(m,n) .le. zero) then

                        fn(m,n)=zero
                        fs(m,n)=zero
                        fm(m,n)=zero
                     else if (fn(m,n) .ge. one) then

                        fn(m,n)=one
                        fs(m,n)=one
                        fm(m,n)=one
                     else

                        fm(m,n) = (yhi(m,n)/ymean(m,n)) * (0.5*asub(m,n)*(1.0-phiyy*phiyy) +   &
                                  third*bsub(m,n)*(1.0-phiyy*phiyy*phiyy))
                        if (fm(m,n).lt.fn(m,n) .or. fm(m,n).gt.one) then
                         if (idiag_fnsm_prob .gt. 0) then
                           print *,'fm(',m,n,')=',fm(m,n),' outside fn,1 - activate err22'
                           print *,'na,volc,fn    =', na(m,n), volc(m,n), fn(m,n)
                           print *,'asub,bsub     =', asub(m,n), bsub(m,n)
                           print *,'yhi,ycut      =', yhi(m,n), ycut
                         endif
                        endif
                        if (fm(m,n) .le. fn(m,n)) then

                           fm(m,n)=fn(m,n)
                           fs(m,n)=fn(m,n)
                        else if (fm(m,n) .ge. one) then

                           fm(m,n)=one
                           fs(m,n)=one
                           fn(m,n)=one
                        else


                           dumaa = fn(m,n)*(yhi(m,n)/ymean(m,n)) 
                           fm(m,n) = min( fm(m,n), dumaa )
                           dumaa = 1.0 + (fn(m,n)-1.0)*(ylo(m,n)/ymean(m,n))
                           fm(m,n) = min( fm(m,n), dumaa )

                           betayy = ylo(m,n)/yhi(m,n)
                           dumaa = phiyy**twothird
                           dumbb = betayy**twothird
                           fs(m,n) =   &
                              (asub(m,n)*(1.0-phiyy*dumaa) +   &
                                  0.625*bsub(m,n)*(1.0-phiyy*phiyy*dumaa)) /   &
                              (asub(m,n)*(1.0-betayy*dumbb) +   &
                                  0.625*bsub(m,n)*(1.0-betayy*betayy*dumbb))
                           fs(m,n)=max(fs(m,n),fn(m,n))
                           fs(m,n)=min(fs(m,n),fm(m,n))
                        endif
                     endif

                  endif

               else

                  x=2*(lnsm(m,n)-lnsmax)/(3*sq2*alnsign(m,n))
                  fn(m,n)=0.5*(1.-ERF_ALT(x))
                  arg=x-sq2*alnsign(m,n)
                  fs(m,n)=0.5*(1.-ERF_ALT(arg))
                  arg=x-1.5*sq2*alnsign(m,n)
                  fm(m,n)=0.5*(1.-ERF_ALT(arg))
               endif




                if((top.and.wbar.lt.0.).or.(.not.top.and.wbar.gt.0.))then
                   fluxn(m,n)=fn(m,n)*w
                   fluxs(m,n)=fs(m,n)*w
                   fluxm(m,n)=fm(m,n)*w
                else
                   fluxn(m,n)=0
                   fluxs(m,n)=0
                   fluxm(m,n)=0
               endif

55000       continue      
55002       continue      

            if((top.and.wbar.lt.0.).or.(.not.top.and.wbar.gt.0.))then
               flux_fullact = w
            else
               flux_fullact = 0.0
            endif













60000 continue









      return
      end subroutine activate





      subroutine maxsat(zeta,eta, &
                        maxd_atype,ntype_aer,maxd_asize,nsize_aer, &
                        sm,alnsign,f1,smax)








      implicit none

      integer, intent(in) :: maxd_atype
      integer, intent(in) :: ntype_aer
      integer, intent(in) :: maxd_asize
      integer, intent(in) :: nsize_aer(maxd_atype) 
      real, intent(in) :: sm(maxd_asize,maxd_atype) 
      real, intent(in) :: zeta, eta(maxd_asize,maxd_atype)
      real, intent(in) :: alnsign(maxd_asize,maxd_atype) 
      real, intent(in) :: f1(maxd_asize,maxd_atype)
      real, intent(out) :: smax 

      real :: g1, g2
      real thesum
      integer m 
      integer n 

      do n=1,ntype_aer
      do m=1,nsize_aer(n)
         if(zeta.gt.1.e5*eta(m,n) .or. &
              sm(m,n)*sm(m,n).gt.1.e5*eta(m,n))then

            smax=1.e-20
         else

            go to 1
         endif
      end do
      end do

      return

  1   continue

      thesum=0
      do n=1,ntype_aer
      do m=1,nsize_aer(n)
         if(eta(m,n).gt.1.e-20)then
            g1=sqrt(zeta/eta(m,n))
            g1=g1*g1*g1
            g2=sm(m,n)/sqrt(eta(m,n)+3*zeta)
            g2=sqrt(g2)
            g2=g2*g2*g2
            thesum=thesum + &
                 (f1(m,n)*g1+(1.+0.25*alnsign(m,n))*g2)/(sm(m,n)*sm(m,n))
         else
            thesum=1.e20
         endif
      end do
      end do

      smax=1./sqrt(thesum)

      return
      end subroutine maxsat





      subroutine maxsat_init(maxd_atype, ntype_aer, &
           maxd_asize, nsize_aer, alnsign, f1)






      implicit none

      integer, intent(in)  :: maxd_atype
      integer, intent(in)  :: ntype_aer 
      integer, intent(in)  :: maxd_asize
      integer, intent(in)  :: nsize_aer(maxd_atype) 
      real,    intent(in)  :: alnsign(maxd_asize,maxd_atype) 
      real,    intent(out) :: f1(maxd_asize,maxd_atype)

      integer m 
      integer n 




      do n=1,ntype_aer
         do m=1,nsize_aer(n)
            f1(m,n)=0.5*exp(2.5*alnsign(m,n)*alnsign(m,n))
         end do
      end do

      end subroutine maxsat_init







       subroutine loadaer(chem,k,kmn,kmx,num_chem,cs,npv, &
                          dlo_sect,dhi_sect,maxd_acomp, ncomp,                &
                          grid_id, ktau, i, j, isize, itype,   &
                          numptr_aer, numptrcw_aer, dens_aer,   &
                          massptr_aer, massptrcw_aer,   &
                          maerosol, maerosolcw,                 &
                          maerosol_tot, maerosol_totcw,         &
                          naerosol, naerosolcw,                 &
                          vaerosol, vaerosolcw)

      implicit none





       integer, intent(in) ::  num_chem 
       integer, intent(in) ::  k,kmn,kmx
       real,    intent(in) ::  chem(kmn:kmx,num_chem) 
       real,    intent(in) ::  cs  
       real,    intent(in) ::  npv 
       integer, intent(in) ::  maxd_acomp,ncomp
       integer, intent(in) ::  numptr_aer,numptrcw_aer
       integer, intent(in) ::  massptr_aer(maxd_acomp), massptrcw_aer(maxd_acomp)
       real,    intent(in) ::  dens_aer(maxd_acomp) 
       real,    intent(in) ::  dlo_sect,dhi_sect 
       integer, intent(in) ::  grid_id, ktau, i, j, isize, itype



       real, intent(out) ::  naerosol                
       real, intent(out) ::  naerosolcw              
       real, intent(out) ::  maerosol(maxd_acomp)   
       real, intent(out) ::  maerosolcw(maxd_acomp) 
       real, intent(out) ::  maerosol_tot   
       real, intent(out) ::  maerosol_totcw 
       real, intent(out) ::  vaerosol       
       real, intent(out) ::  vaerosolcw     



       integer lnum,lnumcw,l,ltype,lmass,lmasscw,lsfc,lsfccw
       real num_at_dhi, num_at_dlo
       real npv_at_dhi, npv_at_dlo
       real, parameter :: pi = 3.1415926526
       real specvol 

          lnum=numptr_aer
          lnumcw=numptrcw_aer
          maerosol_tot=0.
          maerosol_totcw=0.
          vaerosol=0.
          vaerosolcw=0.
          do l=1,ncomp
             lmass=massptr_aer(l)
             lmasscw=massptrcw_aer(l)
             maerosol(l)=chem(k,lmass)*cs
             maerosol(l)=max(maerosol(l),0.)
             maerosolcw(l)=chem(k,lmasscw)*cs
             maerosolcw(l)=max(maerosolcw(l),0.)
             maerosol_tot=maerosol_tot+maerosol(l)
             maerosol_totcw=maerosol_totcw+maerosolcw(l)

             specvol=1.0e-3/dens_aer(l)
             vaerosol=vaerosol+maerosol(l)*specvol
             vaerosolcw=vaerosolcw+maerosolcw(l)*specvol

          enddo

          if(lnum.gt.0)then


             npv_at_dhi = 6.0e6/(pi*dhi_sect*dhi_sect*dhi_sect)
             npv_at_dlo = 6.0e6/(pi*dlo_sect*dlo_sect*dlo_sect)

             naerosol=chem(k,lnum)*cs
             naerosolcw=chem(k,lnumcw)*cs
             num_at_dhi = vaerosol*npv_at_dhi
             num_at_dlo = vaerosol*npv_at_dlo
             naerosol = max( num_at_dhi, min( num_at_dlo, naerosol ) )



             num_at_dhi = vaerosolcw*npv_at_dhi
             num_at_dlo = vaerosolcw*npv_at_dlo
             naerosolcw = max( num_at_dhi, min( num_at_dlo, naerosolcw ) )
          else

             naerosol=vaerosol*npv
             naerosol=max(naerosol,0.)
             naerosolcw=vaerosolcw*npv
             naerosolcw=max(naerosolcw,0.)
          endif


       return
       end subroutine loadaer




        real function erfc_num_recipes( x )



        implicit none
        real x
        double precision erfc_dbl, dum, t, zz

        zz = abs(x)
        t = 1.0/(1.0 + 0.5*zz)







        dum =  ( -zz*zz - 1.26551223 + t*(1.00002368 + t*(0.37409196 +   &
          t*(0.09678418 + t*(-0.18628806 + t*(0.27886807 +   &
                                           t*(-1.13520398 +   &
          t*(1.48851587 + t*(-0.82215223 + t*0.17087277 )))))))))

        erfc_dbl = t * exp(dum)
        if (x .lt. 0.0) erfc_dbl = 2.0d0 - erfc_dbl

        erfc_num_recipes = erfc_dbl

        return
        end function erfc_num_recipes     


    real function erf_alt( x )

    implicit none

    real,intent(in) :: x

    erf_alt = 1. - erfc_num_recipes(x)

    end function erf_alt

END MODULE module_mixactivate
