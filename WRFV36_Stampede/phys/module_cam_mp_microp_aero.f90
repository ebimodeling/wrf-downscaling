







module microp_aero
















  use shr_kind_mod,  only: r8=>shr_kind_r8




  use module_cam_support, only: masterproc, pcols, pver, pverp

  use physconst,     only: gravit, rair, tmelt, cpair, rh2o, r_universal, mwh2o, rhoh2o
  use physconst,     only: latvap, latice



   use module_cam_support, only: endrun

  use error_function, only: erf,erfc
  use wv_saturation,  only: estblf, hlatv, tmin, hlatf, rgasv, pcf, cp, epsqs, ttrice, &
                            vqsatd2, vqsatd2_single,polysvp







  use module_cam_support, only: addfld, add_default, phys_decomp, outfld, iulog



 implicit none
 private
 save

 public :: ini_microp_aero, microp_aero_ts



   logical             :: wsubTKE  


      real(r8), private:: rn_dst1, rn_dst2, rn_dst3, rn_dst4 
      real(r8), private:: pi    
      real(r8), private:: tt0   
      real(r8), private:: qsmall 

  real(r8), private::  r              




      integer, private:: psat
      parameter (psat=6) 
      real(r8), private:: aten

      real(r8), allocatable, private:: alogsig(:) 
      real(r8), allocatable, private:: exp45logsig(:)
      real(r8), allocatable, private:: argfactor(:)
      real(r8), allocatable, private:: amcube(:) 
      real(r8), allocatable, private:: smcrit(:) 
      real(r8), allocatable, private:: lnsm(:) 
      real(r8), private:: amcubesulfate(pcols) 
      real(r8), private:: smcritsulfate(pcols) 
      real(r8), allocatable, private:: amcubefactor(:) 
      real(r8), allocatable, private:: smcritfactor(:) 
      real(r8), private:: super(psat)
      real(r8), private:: alogten,alog2,alog3,alogaten
      real(r8), private, parameter :: supersat(psat)= &
               (/0.02,0.05,0.1,0.2,0.5,1.0/)
      real(r8), allocatable, private:: ccnfact(:,:)

      real(r8), allocatable, private:: f1(:),f2(:) 
      real(r8), private:: third, sixth,zero
      real(r8), private:: sq2, sqpi

      integer :: naer_all    
      integer :: idxsul = -1 
      integer :: idxdst1 = -1 
      integer :: idxdst2 = -1 
      integer :: idxdst3 = -1 
      integer :: idxdst4 = -1 

      integer :: idxbcphi = -1 

      
      character(len=20), allocatable :: aername(:)
      real(r8), allocatable :: dryrad_aer(:)
      real(r8), allocatable :: density_aer(:)
      real(r8), allocatable :: hygro_aer(:)
      real(r8), allocatable :: dispersion_aer(:)
      real(r8), allocatable :: num_to_mass_aer(:)

contains



subroutine ini_microp_aero











    use ndrop,           only: activate_init
    use constituents,    only: cnst_name







    use module_cam_support, only: fieldname_len, masterproc
    use modal_aero_data,    only: cnst_name_cw => cnst_name_cw_mp, &
         lmassptr_amode => lmassptr_amode_mp, lmassptrcw_amode => lmassptrcw_amode_mp , &
         nspec_amode    => nspec_amode_mp   , ntot_amode       => ntot_amode_mp,        &
         numptr_amode   => numptr_amode_mp  , numptrcw_amode   => numptrcw_amode_mp

      
    integer                        :: lphase, lspec
    character(len=fieldname_len)   :: tmpname
    character(len=fieldname_len+3) :: fieldname
    character(128)                 :: long_name
    character(8)                   :: unit
    logical                        :: history_aerosol      



   integer k

   integer l,m, iaer
   real(r8) surften       
   real(r8) arg
   real(r8) derf

   character(len=16) :: eddy_scheme = ' '
   logical           :: history_microphysics     







   history_microphysics =.FALSE.
   wsubTKE              =.TRUE.


   
   
   call addfld ('CCN1    ','#/cm3   ',pver, 'A','CCN concentration at S=0.02%',phys_decomp)
   call addfld ('CCN2    ','#/cm3   ',pver, 'A','CCN concentration at S=0.05%',phys_decomp)
   call addfld ('CCN3    ','#/cm3   ',pver, 'A','CCN concentration at S=0.1%',phys_decomp)
   call addfld ('CCN4    ','#/cm3   ',pver, 'A','CCN concentration at S=0.2%',phys_decomp)
   call addfld ('CCN5    ','#/cm3   ',pver, 'A','CCN concentration at S=0.5%',phys_decomp)
   call addfld ('CCN6    ','#/cm3   ',pver, 'A','CCN concentration at S=1.0%',phys_decomp)

   call add_default('CCN3',  1, ' ' )

   call addfld ('NIHF    ','#/m3   ',pver, 'A','Activated Ice Number Concentation due to homogenous freezing',phys_decomp)
   call addfld ('NIDEP    ','#/m3   ',pver, 'A','Activated Ice Number Concentation due to deposition nucleation',phys_decomp)
   call addfld ('NIIMM    ','#/m3   ',pver, 'A','Activated Ice Number Concentation due to immersion freezing',phys_decomp)
   call addfld ('NIMEY    ','#/m3   ',pver, 'A','Activated Ice Number Concentation due to meyers deposition',phys_decomp)




   rn_dst1=0.258e-6_r8
   rn_dst2=0.717e-6_r8
   rn_dst3=1.576e-6_r8
   rn_dst4=3.026e-6_r8



   qsmall = 1.e-18_r8  





      zero=0._r8
      third=1./3._r8
      sixth=1./6._r8
      sq2=sqrt(2._r8)
      pi=4._r8*atan(1.0_r8)
      sqpi=sqrt(pi)

      r= rair        


      tt0=273.15_r8  

      surften=0.076_r8
      aten=2.*mwh2o*surften/(r_universal*tt0*rhoh2o)
      alogaten=log(aten)
      alog2=log(2._r8)
      alog3=log(3._r8)
      super(:)=0.01*supersat(:)
 return
 end subroutine ini_microp_aero





      subroutine microp_aero_ts (   &
   lchnk, ncol, deltatin, t, ttend,        &
   qn, qc, qi,               &
   nc, ni, p, pdel, cldn,                   &
   liqcldf, icecldf,                        &
   cldo, pint, rpdel, zm, omega,            &
   qaer, cflx, qaertend, dgnumwet,dgnum, &
   kkvh, tke, turbtype, smaw, wsub, wsubi, &
   naai, naai_hom, npccn, rndst, nacon     &
   ,qqcw                                   &
                                           )

   use wv_saturation, only: vqsatd, vqsatd_water
   use module_cam_support, only: pcnst => pcnst_mp
   use ndrop,         only: dropmixnuc
   use modal_aero_data, only:numptr_amode => numptr_amode_mp, modeptr_accum => modeptr_accum_mp, &
        modeptr_coarse    => modeptr_coarse_mp   , modeptr_aitken    => modeptr_aitken_mp, &
        lptr_dust_a_amode => lptr_dust_a_amode_mp, lptr_nacl_a_amode => lptr_nacl_a_amode_mp, &
        ntot_amode        => ntot_amode_mp, modeptr_coardust => modeptr_coardust_mp  
     

   
   integer,  intent(in) :: lchnk
   integer,  intent(in) :: ncol
   real(r8), intent(in) :: deltatin             
   real(r8), intent(in) :: t(pcols,pver)       
   real(r8), intent(in) :: ttend(pcols,pver)    
   real(r8), intent(in) :: qn(pcols,pver)       
   
   real(r8), intent(in) :: qc(pcols,pver)    
   real(r8), intent(in) :: qi(pcols,pver)    
   real(r8), intent(in) :: nc(pcols,pver)    
   real(r8), intent(in) :: ni(pcols,pver)    
   real(r8), intent(in) :: p(pcols,pver)        
   real(r8), intent(in) :: pdel(pcols,pver)     
   real(r8), intent(in) :: cldn(pcols,pver)     
   real(r8), intent(in) :: icecldf(pcols,pver)  
   real(r8), intent(in) :: liqcldf(pcols,pver)  
   real(r8), intent(in) :: cldo(pcols,pver)  
   real(r8), intent(in) :: pint(pcols,pverp)    
   real(r8), intent(in) :: rpdel(pcols,pver)    
   real(r8), intent(in) :: zm(pcols,pver)       
   real(r8), intent(in) :: omega(pcols,pver)    
   real(r8), intent(in) :: qaer(pcols,pver,pcnst) 
   real(r8), intent(in) :: cflx(pcols,pcnst)      
   real(r8), intent(inout) :: qaertend(pcols,pver,pcnst) 
   real(r8), intent(inout) :: qqcw(pcols,pver,pcnst) 
   real(r8), intent(in) :: dgnumwet(pcols,pver,ntot_amode) 
   real(r8), intent(in) :: dgnum(pcols,pver,ntot_amode) 
   real(r8), intent(in) :: kkvh(pcols,pver+1) 
   real(r8), intent(in) :: tke(pcols,pver+1)    
   real(r8), intent(in) :: turbtype(pcols,pver+1) 
   real(r8), intent(in) :: smaw(pcols,pver+1)     

   

   real(r8), intent(out) :: wsub(pcols,pver)    
   real(r8), intent(out) :: wsubi(pcols,pver)   
   real(r8), intent(out) :: naai(pcols,pver)    
   real(r8), intent(out) :: naai_hom(pcols,pver)
   real(r8), intent(out) :: npccn(pcols,pver)   
   real(r8), intent(out) :: rndst(pcols,pver,4) 
   real(r8), intent(out) :: nacon(pcols,pver,4) 





   real(r8) :: tkem(pcols,pver)     
   real(r8) :: smm(pcols,pver)      
   real(r8) :: relhum(pcols,pver) 
   real(r8) :: cldm(pcols,pver)   
   real(r8) :: icldm(pcols,pver)   
   real(r8) :: lcldm(pcols,pver)   
   real(r8) :: nfice(pcols,pver) 
   real(r8) :: dumfice
   real(r8) :: qcld          
        real(r8) :: lcldn(pcols,pver) 
        real(r8) :: lcldo(pcols,pver) 
        real(r8) :: nctend_mixnuc(pcols,pver)
   	real(r8) :: deltat        
        real(r8) :: nihf2,niimm2,nidep2,nimey2,dum2 
        real(r8) arg  

	 integer ftrue  
         real(r8) :: dum        

        real(r8) :: q(pcols,pver) 

        real(r8) :: ncloc(pcols,pver) 
        real(r8) :: rho(pcols,pver) 
        real(r8) :: mincld  


        real(r8)  tcnt, viscosity, mfp
        real(r8)  slip1, slip2, slip3, slip4
        real(r8)  dfaer1, dfaer2, dfaer3, dfaer4
        real(r8)  nacon1,nacon2,nacon3,nacon4

        real(r8) dmc,ssmc,dstrn  


        real(r8) :: tsp(pcols,pver)      
        real(r8) :: qsp(pcols,pver)      
        real(r8) :: qsphy(pcols,pver)      
        real(r8) :: qs(pcols)            
        real(r8) :: es(pcols)            
        real(r8) :: esl(pcols,pver)      
        real(r8) :: esi(pcols,pver)      
        real(r8) :: gammas(pcols)        


        real(r8), allocatable :: naermod(:) 
	real(r8), allocatable :: naer2(:,:,:)   

        real(r8), allocatable :: maerosol(:,:)   
	real(r8) naer(pcols)
        real(r8) ccn(pcols,pver,psat)        
        character*8, parameter :: ccn_name(psat)=(/'CCN1','CCN2','CCN3','CCN4','CCN5','CCN6'/)


          real(r8) :: nimey(pcols,pver) 
          real(r8) :: nihf(pcols,pver)  
          real(r8) :: nidep(pcols,pver) 
          real(r8) :: niimm(pcols,pver) 


         integer i,k,nstep,n, l
	 integer ii,kk, m
         integer, allocatable :: ntype(:)




    nimey(1:ncol,1:pver)=0._r8 
    nihf(1:ncol,1:pver)=0._r8  
    nidep(1:ncol,1:pver)=0._r8 
    niimm(1:ncol,1:pver)=0._r8  


    naai(1:ncol,1:pver)=0._r8
    naai_hom(1:ncol,1:pver)=0._r8
    npccn(1:ncol,1:pver)=0._r8  
    nacon(1:ncol,1:pver,:)=0._r8


    rndst(1:ncol,1:pver,1)=rn_dst1
    rndst(1:ncol,1:pver,2)=rn_dst2
    rndst(1:ncol,1:pver,3)=rn_dst3
    rndst(1:ncol,1:pver,4)=rn_dst4
     

    tkem(1:ncol,1:pver)=0._r8    
    smm(1:ncol,1:pver)=0._r8    

    allocate(naermod(naer_all), &
      naer2(pcols,pver,naer_all), &
      maerosol(1,naer_all), &
      ntype(naer_all))


	deltat=deltatin	


        q(1:ncol,1:pver)=qn(1:ncol,1:pver)
        ncloc(1:ncol,1:pver)=nc(1:ncol,1:pver)


	mincld=0.0001_r8



        do k=1,pver
           do i=1,ncol
              rho(i,k)=p(i,k)/(r*t(i,k))
           end do
        end do





        if( wsubTKE ) then

            do i = 1, ncol
            do k = 1, pver
               tkem(i,k) = 0.5_r8 * ( tke(i,k)  +  tke(i,k+1) )
               smm(i,k)  = 0.5_r8 * ( smaw(i,k) + smaw(i,k+1) )
               if( turbtype(i,k) .eq. 3._r8 ) then       
                   tkem(i,k) = 0.5_r8 *  tke(i,k+1)
                   smm(i,k)  = 0.5_r8 * smaw(i,k+1)
               elseif( turbtype(i,k+1) .eq. 4._r8 ) then 
                   tkem(i,k) = 0.5_r8 *  tke(i,k)  
                   smm(i,k)  = 0.5_r8 * smaw(i,k)
               endif
               smm(i,k) = 0.259_r8*smm(i,k)
               smm(i,k) = max(smm(i,k), 0.4743_r8)
	    end do
            end do

        endif

           do i=1,ncol
	      ftrue=0
              do k=1,pver




	      dum=(kkvh(i,k)+kkvh(i,k+1))/2._r8/30._r8

	      dum=min(dum,10._r8)

	      wsub(i,k)=dum
            
              if( wsubTKE ) then
                  wsub(i,k) = sqrt(0.5_r8*(tke(i,k)+tke(i,k+1))*(2._r8/3._r8))
                  wsub(i,k) = min(wsub(i,k),10._r8)
              endif
              wsubi(i,k) = max(0.001_r8,wsub(i,k))
              wsubi(i,k) = min(wsubi(i,k),0.2_r8)
              wsub(i,k)  = max(0.20_r8,wsub(i,k))
           end do
        end do




        do k=1,pver




        call vqsatd_water(t(1,k),p(1,k),es,qs,gammas,ncol) 

        do i=1,ncol

	esl(i,k)=polysvp(t(i,k),0)
	esi(i,k)=polysvp(t(i,k),1)


	if (t(i,k).gt.tmelt)esi(i,k)=esl(i,k)

        relhum(i,k)=q(i,k)/qs(i)



           cldm(i,k)=max(cldn(i,k),mincld)

           icldm(i,k)=max(icecldf(i,k),mincld)
           lcldm(i,k)=max(liqcldf(i,k),mincld)




        nfice(i,k)=0._r8
        dumfice=qc(i,k)+qi(i,k)
        if (dumfice.gt.qsmall .and. qi(i,k).gt.qsmall) then
           nfice(i,k)=qi(i,k)/dumfice
        endif





        if (t(i,k).lt.tmelt - 5._r8) then




           call nucleati(wsubi(i,k),t(i,k),relhum(i,k),icldm(i,k),qc(i,k),nfice(i,k),rho(i,k),  &
                         qaer(i,k,:)*rho(i,k),dgnum(i,k,:),1,naer_all,dum2,nihf2,niimm2,nidep2,nimey2)

           naai(i,k)=dum2
           naai_hom(i,k)=nihf2
           nihf(i,k)=nihf2
           niimm(i,k)=niimm2
           nidep(i,k)=nidep2
           nimey(i,k)=nimey2
        end if






	end do 
	end do 






      do k=1,pver
      do i=1,ncol
         qcld=qc(i,k)+qi(i,k)
         if(qcld.gt.qsmall)then
            lcldn(i,k)=cldn(i,k)*qc(i,k)/qcld
            lcldo(i,k)=cldo(i,k)*qc(i,k)/qcld
         else
            lcldn(i,k)=0._r8
            lcldo(i,k)=0._r8
         endif
      enddo
      enddo

      call dropmixnuc(lchnk, ncol, ncloc, nctend_mixnuc, t, omega,  &
                    p, pint, pdel, rpdel, zm, kkvh, wsub, lcldn, lcldo,     &
                    qaer, cflx, qaertend, deltat                    &
                    , qqcw                                          &
                                                                    )

      npccn(:ncol,:)= nctend_mixnuc(:ncol,:)







      do k=1,pver
      do i=1,ncol

         if (t(i,k).lt.269.15_r8) then





               dmc= qaer(i,k,lptr_dust_a_amode(modeptr_coarse))
               ssmc=qaer(i,k,lptr_nacl_a_amode(modeptr_coarse))
               if (dmc.gt.0.0_r8) then
                  nacon(i,k,3)=dmc/(ssmc+dmc) * qaer(i,k,numptr_amode(modeptr_coarse))*rho(i,k) 
               else 
                  nacon(i,k,3)=0._r8
               endif
               
               
               
               rndst(i,k,3)=0.5_r8*dgnumwet(i,k,modeptr_coarse)


           if (rndst(i,k,3).le.0._r8) then 
              rndst(i,k,3)=rn_dst3
           endif

        endif
      enddo
      enddo






      do l=1,naer_all
         call outfld(aername(l)//'_m3', naer2(1,1,l), pcols, lchnk)
      enddo



  do i = 1,ncol
     do k=1,pver
        nihf(i,k)=nihf(i,k)*rho(i,k)
        niimm(i,k)=niimm(i,k)*rho(i,k)   
        nidep(i,k)=nidep(i,k)*rho(i,k)
        nimey(i,k)=nimey(i,k)*rho(i,k)
     end do
  end do
  call outfld('NIHF',nihf,    pcols,lchnk)
  call outfld('NIIMM',niimm,    pcols,lchnk)
  call outfld('NIDEP',nidep,    pcols,lchnk)
  call outfld('NIMEY',nimey,    pcols,lchnk)

      deallocate( &
         naermod,  &
         naer2,    &
         maerosol, &
         ntype     )

return
end subroutine microp_aero_ts



      subroutine activate(wbar, tair, rhoair,  &
                          na, pmode, nmode, ma, sigman, hygro, rhodry, nact)










      use physconst, only: rair, epsilo, cpair, rh2o, latvap, gravit,   &
                                 rhoh2o, mwh2o, r_universal
      use wv_saturation, only: estblf, epsqs

      implicit none




      integer pmode,ptype 
      real(r8) wbar          
      real(r8) tair          
      real(r8) rhoair        
      real(r8) na(pmode)           
      integer nmode      
      real(r8) ma(pmode)     
      real(r8) rhodry(pmode) 

      real(r8) sigman(pmode)  
      real(r8) hygro(pmode)  




      real(r8) nact      





      real(r8) derf,derfc

      integer, parameter:: nx=200
      integer :: maxmodes
      real(r8) surften       
      data surften/0.076/
      save surften
      real(r8) p0     
      data p0/1013.25e2/
      save p0

      real(r8), allocatable :: volc(:) 
      real(r8) tmass 
      real(r8) rm 
      real(r8) pres 
      real(r8) path 
      real(r8) diff 
      real(r8) conduct 
      real(r8) diff0,conduct0
      real(r8) qs 
      real(r8) dqsdt 
      real(r8) dqsdp 
      real(r8) gloc 
      real(r8) zeta
      real(r8), allocatable :: eta(:)
      real(r8), allocatable :: smc(:)
      real(r8) lnsmax 
      real(r8) alpha
      real(r8) gammaloc
      real(r8) beta
      real(r8) sqrtg
      real(r8) alogam
      real(r8) rlo,rhi,xint1,xint2,xint3,xint4
      real(r8) w,wnuc,wb

      real(r8) alw,sqrtalw
      real(r8) smax
      real(r8) x,arg
      real(r8) xmincoeff,xcut,volcut,surfcut
      real(r8) z,z1,z2,wf1,wf2,zf1,zf2,gf1,gf2,gf
      real(r8) :: etafactor1,etafactor2max
      real(r8),allocatable :: etafactor2(:)
      real(r8) es
      integer m,n

      real(r8),allocatable :: amcubeloc(:)
      real(r8),allocatable :: lnsmloc(:)

      maxmodes = naer_all
      allocate( &
         volc(maxmodes),       &
         eta(maxmodes),        &
         smc(maxmodes),        &
         etafactor2(maxmodes), &
         amcubeloc(maxmodes),  &
         lnsmloc(maxmodes)     )

      if(maxmodes<pmode)then
         write(iulog,*)'maxmodes,pmode in activate =',maxmodes,pmode
         call wrf_message(iulog)
	 call endrun('activate')
      endif

      nact=0._r8

      if(nmode.eq.1.and.na(1).lt.1.e-20)return

      if(wbar.le.0.)return

      pres=rair*rhoair*tair
      diff0=0.211e-4*(p0/pres)*(tair/tt0)**1.94
      conduct0=(5.69+0.017*(tair-tt0))*4.186e2*1.e-5 
      es = estblf(tair)
      qs = epsilo*es/(pres-(1.0_r8 - epsqs)*es)
      dqsdt=latvap/(rh2o*tair*tair)*qs
      alpha=gravit*(latvap/(cpair*rh2o*tair*tair)-1./(rair*tair))
      gammaloc=(1+latvap/cpair*dqsdt)/(rhoair*qs)


      gloc=1./(rhoh2o/(diff0*rhoair*qs)                                    &
          +latvap*rhoh2o/(conduct0*tair)*(latvap/(rh2o*tair)-1.))
      sqrtg=sqrt(gloc)
      beta=4.*pi*rhoh2o*gloc*gammaloc
      etafactor2max=1.e10/(alpha*wbar)**1.5 

      do m=1,nmode

          volc(m)=ma(m)/(rhodry(m)) 


         if(volc(m).gt. TINY(volc) .and.na(m).gt. TINY(na))then
            etafactor2(m)=1./(na(m)*beta*sqrtg)  

            amcubeloc(m)=(3.*volc(m)/(4.*pi*exp45logsig(m)*na(m)))  
	    smc(m)=smcrit(m) 

            if(hygro(m).gt.1.e-10)then   
               smc(m)=2.*aten*sqrt(aten/(27.*hygro(m)*amcubeloc(m))) 
            else
              smc(m)=100.
            endif
         else
            smc(m)=1.
	    etafactor2(m)=etafactor2max 
         endif
         lnsmloc(m)=log(smc(m)) 
      enddo


         wnuc=wbar

            w=wbar
            alw=alpha*wnuc
            sqrtalw=sqrt(alw)
            zeta=2.*sqrtalw*aten/(3.*sqrtg)
	    etafactor1=2.*alw*sqrtalw

            do m=1,nmode
               eta(m)=etafactor1*etafactor2(m)
            enddo

            call maxsat(zeta,eta,nmode,smc,smax)

            lnsmax=log(smax)
            xmincoeff=alogaten-2.*third*(lnsmax-alog2)-alog3

	    nact=0._r8
            do m=1,nmode
	       x=2*(lnsmloc(m)-lnsmax)/(3*sq2*alogsig(m))
               nact=nact+0.5*(1.-erf(x))*na(m)
            enddo
	    nact=nact/rhoair 

      deallocate( &
         volc,       &
         eta,        &
         smc,        &
         etafactor2, &
         amcubeloc,  &
         lnsmloc     )

      return
      end subroutine activate

      subroutine maxsat(zeta,eta,nmode,smc,smax)







      implicit none

    
    
      integer nmode 
      real(r8) :: smc(:) 
      real(r8) zeta
      real(r8) :: eta(:)
      real(r8) smax 
      integer m  
      real(r8) sum, g1, g2

      do m=1,nmode
         if(zeta.gt.1.e5*eta(m).or.smc(m)*smc(m).gt.1.e5*eta(m))then

            smax=1.e-20
         else

            go to 1
         endif
      enddo

      return

  1   continue

      sum=0
      do m=1,nmode
         if(eta(m).gt.1.e-20)then
            g1=sqrt(zeta/eta(m))
            g1=g1*g1*g1
            g2=smc(m)/sqrt(eta(m)+3*zeta)
            g2=sqrt(g2)
            g2=g2*g2*g2
            sum=sum+(f1(m)*g1+f2(m)*g2)/(smc(m)*smc(m))
         else
            sum=1.e20
         endif
      enddo

      smax=1./sqrt(sum)

      return

      end subroutine maxsat


subroutine nucleati(wbar, tair, relhum, cldn, qc, nfice, rhoair, &
       qaerpt, dgnum, ptype, naer_all, nuci, onihf, oniimm, onidep, onimey)
 











      use modal_aero_data, only:numptr_amode => numptr_amode_mp, modeptr_accum => modeptr_accum_mp, &
           modeptr_coarse    => modeptr_coarse_mp   , modeptr_aitken    => modeptr_aitken_mp, &
           ntot_amode        => ntot_amode_mp       , sigmag_amode      => sigmag_amode_mp,   &
           lptr_dust_a_amode => lptr_dust_a_amode_mp, lptr_nacl_a_amode => lptr_nacl_a_amode_mp, &
           modeptr_coardust => modeptr_coardust_mp  
      use module_cam_support, only: pcnst =>pcnst_mp




  integer ptype, naer_all
  real(r8), intent(in) :: wbar                
  real(r8), intent(in) :: tair                
  real(r8), intent(in) :: relhum              
  real(r8), intent(in) :: cldn                
  real(r8), intent(in) :: qc                  
  real(r8), intent(in) :: nfice               
  real(r8), intent(in) :: rhoair              
  real(r8), intent(in) :: qaerpt(pcnst) 
  real(r8), intent(in) :: dgnum(ntot_amode)   





  real(r8), intent(out) :: nuci               
  real(r8), intent(out) :: onihf              
  real(r8), intent(out) :: oniimm             
  real(r8), intent(out) :: onidep             
  real(r8), intent(out) :: onimey             




  real(r8)  so4_num                                      
  real(r8)  soot_num                                     
  real(r8)  dst1_num,dst2_num,dst3_num,dst4_num          
  real(r8)  dst_num                                      
  real(r8)  nihf                                         
  real(r8)  niimm                                        
  real(r8)  nidep                                        
  real(r8)  nimey                                        
  real(r8)  n1,ni                                        
  real(r8)  tc,A,B,C,regm,RHw                            
  real(r8)  esl,esi,deles                                
  real(r8)  dst_scale
  real(r8)  subgrid
  real(r8) dmc,ssmc         

    so4_num=0.0_r8
    soot_num=0.0_r8
    dst_num=0.0_r8
    dst1_num = 0.0_r8
    dst2_num = 0.0_r8
    dst3_num = 0.0_r8
    dst4_num = 0.0_r8     







      soot_num = qaerpt(numptr_amode(modeptr_accum))*1.0e-6_r8 

      dmc= qaerpt(lptr_dust_a_amode(modeptr_coarse))
      ssmc=qaerpt(lptr_nacl_a_amode(modeptr_coarse))
      if (dmc.gt.0._r8) then
         dst_num=dmc/(ssmc+dmc) * qaerpt(numptr_amode(modeptr_coarse))*1.0e-6_r8 
      else 
         dst_num=0.0_r8
      endif

      if (dgnum(modeptr_aitken) .gt. 0._r8  ) then
         so4_num  = qaerpt(numptr_amode(modeptr_aitken))*1.0e-6_r8 & 
               * (0.5_r8 - 0.5_r8*erf(log(0.1e-6_r8/dgnum(modeptr_aitken))/  &
                 (2._r8**0.5_r8*log(sigmag_amode(modeptr_aitken)))))
      else 
         so4_num = 0.0_r8 
      endif
      so4_num = max(0.0_r8,so4_num)


    soot_num=0.0_r8

    ni=0._r8
    tc=tair-273.15_r8

    
    niimm=0._r8
    nidep=0._r8
    nihf=0._r8

    if(so4_num.ge.1.0e-10_r8 .and. (soot_num+dst_num).ge.1.0e-10_r8 .and. cldn.gt.0._r8) then



    A = 0.0073_r8
    B = 1.477_r8
    C = 131.74_r8
    RHw=(A*tc*tc+B*tc+C)*0.01_r8   

    subgrid = 1.2_r8

    if((tc.le.-35.0_r8) .and. ((relhum*polysvp(tair,0)/polysvp(tair,1)*subgrid).ge.1.2_r8)) then 

       A = -1.4938_r8 * log(soot_num+dst_num) + 12.884_r8
       B = -10.41_r8  * log(soot_num+dst_num) - 67.69_r8
       regm = A * log(wbar) + B

       if(tc.gt.regm) then    
         if(tc.lt.-40._r8 .and. wbar.gt.1._r8) then 
           call hf(tc,wbar,relhum,subgrid,so4_num,nihf)
           niimm=0._r8
           nidep=0._r8
           n1=nihf
         else
           call hetero(tc,wbar,soot_num+dst_num,niimm,nidep)
           nihf=0._r8
           n1=niimm+nidep
         endif
       elseif (tc.lt.regm-5._r8) then 
         call hf(tc,wbar,relhum,subgrid,so4_num,nihf)
         niimm=0._r8
         nidep=0._r8
         n1=nihf
       else        
         if(tc.lt.-40._r8 .and. wbar.gt.1._r8) then 
           call hf(tc,wbar,relhum,subgrid,so4_num,nihf)
           niimm=0._r8
           nidep=0._r8
           n1=nihf
         else

           call hf(regm-5._r8,wbar,relhum,subgrid,so4_num,nihf)
           call hetero(regm,wbar,soot_num+dst_num,niimm,nidep)

           if(nihf.le.(niimm+nidep)) then
             n1=nihf
           else
             n1=(niimm+nidep)*((niimm+nidep)/nihf)**((tc-regm)/5._r8)
           endif
         endif
       endif

       ni=n1

    endif
    endif
1100  continue


    if(tc.lt.0._r8 .and. tc.gt.-37._r8 .and. qc.gt.1.e-12_r8) then
      esl = polysvp(tair,0)     
      esi = polysvp(tair,1)     
      deles = (esl - esi)
      nimey=1.e-3_r8*exp(12.96_r8*deles/esi - 0.639_r8) 
    else
      nimey=0._r8
    endif

    nuci=ni+nimey
    if(nuci.gt.9999._r8.or.nuci.lt.0._r8) then
       write(iulog, *) 'Warning: incorrect ice nucleation number (nuci reset =0)'
       call wrf_message(iulog)
       write(iulog, *) ni, tair, relhum, wbar, nihf, niimm, nidep,deles,esi,dst2_num,dst3_num,dst4_num
       call wrf_message(iulog)
       nuci=0._r8
    endif

    nuci=nuci*1.e+6_r8/rhoair    
    onimey=nimey*1.e+6_r8/rhoair
    onidep=nidep*1.e+6_r8/rhoair
    oniimm=niimm*1.e+6_r8/rhoair
    onihf=nihf*1.e+6_r8/rhoair

  return
  end subroutine nucleati

  subroutine hetero(T,ww,Ns,Nis,Nid)

    real(r8), intent(in)  :: T, ww, Ns
    real(r8), intent(out) :: Nis, Nid

    real(r8) A11,A12,A21,A22,B11,B12,B21,B22
    real(r8) A,B,C




      A11 = 0.0263_r8
      A12 = -0.0185_r8
      A21 = 2.758_r8
      A22 = 1.3221_r8
      B11 = -0.008_r8
      B12 = -0.0468_r8
      B21 = -0.2667_r8
      B22 = -1.4588_r8



      B = (A11+B11*log(Ns)) * log(ww) + (A12+B12*log(Ns))
      C =  A21+B21*log(Ns)

      Nis = exp(A22) * Ns**B22 * exp(B*T) * ww**C
      Nis = min(Nis,Ns)

      Nid = 0.0_r8    

      return
  end subroutine hetero


  subroutine hf(T,ww,RH,subgrid,Na,Ni)

      real(r8), intent(in)  :: T, ww, RH, subgrid, Na
      real(r8), intent(out) :: Ni

      real(r8)    A1_fast,A21_fast,A22_fast,B1_fast,B21_fast,B22_fast
      real(r8)    A2_fast,B2_fast
      real(r8)    C1_fast,C2_fast,k1_fast,k2_fast
      real(r8)    A1_slow,A2_slow,B1_slow,B2_slow,B3_slow
      real(r8)    C1_slow,C2_slow,k1_slow,k2_slow
      real(r8)    regm
      real(r8)    A,B,C
      real(r8)    RHw




      A1_fast  =0.0231_r8
      A21_fast =-1.6387_r8  
      A22_fast =-6.045_r8   
      B1_fast  =-0.008_r8
      B21_fast =-0.042_r8   
      B22_fast =-0.112_r8   
      C1_fast  =0.0739_r8
      C2_fast  =1.2372_r8

      A1_slow  =-0.3949_r8
      A2_slow  =1.282_r8
      B1_slow  =-0.0156_r8
      B2_slow  =0.0111_r8
      B3_slow  =0.0217_r8
      C1_slow  =0.120_r8
      C2_slow  =2.312_r8

      Ni = 0.0_r8



      A = 6.0e-4_r8*log(ww)+6.6e-3_r8
      B = 6.0e-2_r8*log(ww)+1.052_r8
      C = 1.68_r8  *log(ww)+129.35_r8
      RHw=(A*T*T+B*T+C)*0.01_r8

      if((T.le.-37.0_r8) .and. ((RH*subgrid).ge.RHw)) then

        regm = 6.07_r8*log(ww)-55.0_r8

        if(T.ge.regm) then    

          if(T.gt.-64.0_r8) then
            A2_fast=A21_fast
            B2_fast=B21_fast
          else
            A2_fast=A22_fast
            B2_fast=B22_fast
          endif

          k1_fast = exp(A2_fast + B2_fast*T + C2_fast*log(ww))
          k2_fast = A1_fast+B1_fast*T+C1_fast*log(ww)

          Ni = k1_fast*Na**(k2_fast)
          Ni = min(Ni,Na)

        else       

          k1_slow = exp(A2_slow + (B2_slow+B3_slow*log(ww))*T + C2_slow*log(ww))
          k2_slow = A1_slow+B1_slow*T+C1_slow*log(ww)

          Ni = k1_slow*Na**(k2_slow)
          Ni = min(Ni,Na)

        endif

      end if

      return
  end subroutine hf








      real(r8) function derf(x)
      implicit real (a - h, o - z)


      real(r8) a,b,x
      dimension a(0 : 64), b(0 : 64)
      integer i,k
      data (a(i), i = 0, 12) / & 
         0.00000000005958930743d0, -0.00000000113739022964d0, & 
         0.00000001466005199839d0, -0.00000016350354461960d0, &
         0.00000164610044809620d0, -0.00001492559551950604d0, &
         0.00012055331122299265d0, -0.00085483269811296660d0, &
         0.00522397762482322257d0, -0.02686617064507733420d0, &
         0.11283791670954881569d0, -0.37612638903183748117d0, &
         1.12837916709551257377d0 / 
      data (a(i), i = 13, 25) / &
         0.00000000002372510631d0, -0.00000000045493253732d0, &
         0.00000000590362766598d0, -0.00000006642090827576d0, &
         0.00000067595634268133d0, -0.00000621188515924000d0, &
         0.00005103883009709690d0, -0.00037015410692956173d0, &
         0.00233307631218880978d0, -0.01254988477182192210d0, &
         0.05657061146827041994d0, -0.21379664776456006580d0, &
         0.84270079294971486929d0 / 
      data (a(i), i = 26, 38) / &
         0.00000000000949905026d0, -0.00000000018310229805d0, &
         0.00000000239463074000d0, -0.00000002721444369609d0, &
         0.00000028045522331686d0, -0.00000261830022482897d0, &
         0.00002195455056768781d0, -0.00016358986921372656d0, &
         0.00107052153564110318d0, -0.00608284718113590151d0, &
         0.02986978465246258244d0, -0.13055593046562267625d0, &
         0.67493323603965504676d0 / 
      data (a(i), i = 39, 51) / &
         0.00000000000382722073d0, -0.00000000007421598602d0, &
         0.00000000097930574080d0, -0.00000001126008898854d0, &
         0.00000011775134830784d0, -0.00000111992758382650d0, &
         0.00000962023443095201d0, -0.00007404402135070773d0, &
         0.00050689993654144881d0, -0.00307553051439272889d0, &
         0.01668977892553165586d0, -0.08548534594781312114d0, &
         0.56909076642393639985d0 / 
      data (a(i), i = 52, 64) / &
         0.00000000000155296588d0, -0.00000000003032205868d0, &
         0.00000000040424830707d0, -0.00000000471135111493d0, &
         0.00000005011915876293d0, -0.00000048722516178974d0, &
         0.00000430683284629395d0, -0.00003445026145385764d0, &
         0.00024879276133931664d0, -0.00162940941748079288d0, &
         0.00988786373932350462d0, -0.05962426839442303805d0, &
         0.49766113250947636708d0 / 
      data (b(i), i = 0, 12) / &
         -0.00000000029734388465d0, 0.00000000269776334046d0, &
         -0.00000000640788827665d0, -0.00000001667820132100d0, &
         -0.00000021854388148686d0, 0.00000266246030457984d0, &
         0.00001612722157047886d0, -0.00025616361025506629d0, &
         0.00015380842432375365d0, 0.00815533022524927908d0, &
         -0.01402283663896319337d0, -0.19746892495383021487d0,& 
         0.71511720328842845913d0 / 
      data (b(i), i = 13, 25) / &
         -0.00000000001951073787d0, -0.00000000032302692214d0, &
         0.00000000522461866919d0, 0.00000000342940918551d0, &
         -0.00000035772874310272d0, 0.00000019999935792654d0, &
         0.00002687044575042908d0, -0.00011843240273775776d0, &
         -0.00080991728956032271d0, 0.00661062970502241174d0, &
         0.00909530922354827295d0, -0.20160072778491013140d0, &
         0.51169696718727644908d0 / 
      data (b(i), i = 26, 38) / &
         0.00000000003147682272d0, -0.00000000048465972408d0, &
         0.00000000063675740242d0, 0.00000003377623323271d0, &
         -0.00000015451139637086d0, -0.00000203340624738438d0,& 
         0.00001947204525295057d0, 0.00002854147231653228d0, &
         -0.00101565063152200272d0, 0.00271187003520095655d0, &
         0.02328095035422810727d0, -0.16725021123116877197d0, &
         0.32490054966649436974d0 / 
      data (b(i), i = 39, 51) / &
         0.00000000002319363370d0, -0.00000000006303206648d0, &
         -0.00000000264888267434d0, 0.00000002050708040581d0, &
         0.00000011371857327578d0, -0.00000211211337219663d0, &
         0.00000368797328322935d0, 0.00009823686253424796d0, &
         -0.00065860243990455368d0, -0.00075285814895230877d0,& 
         0.02585434424202960464d0, -0.11637092784486193258d0, &
         0.18267336775296612024d0 / 
      data (b(i), i = 52, 64) / &
         -0.00000000000367789363d0, 0.00000000020876046746d0, &
         -0.00000000193319027226d0, -0.00000000435953392472d0, &
         0.00000018006992266137d0, -0.00000078441223763969d0, &
         -0.00000675407647949153d0, 0.00008428418334440096d0, &
         -0.00017604388937031815d0, -0.00239729611435071610d0, &
         0.02064129023876022970d0, -0.06905562880005864105d0, &
         0.09084526782065478489d0 / 
      w = abs(x)
      if (w .lt. 2.2d0) then
          t = w * w
          k = int(t)
          t = t - k
          k = k * 13
          y = ((((((((((((a(k) * t + a(k + 1)) * t + &
             a(k + 2)) * t + a(k + 3)) * t + a(k + 4)) * t + &
             a(k + 5)) * t + a(k + 6)) * t + a(k + 7)) * t + &
             a(k + 8)) * t + a(k + 9)) * t + a(k + 10)) * t + &
             a(k + 11)) * t + a(k + 12)) * w
      else if (w .lt. 6.9d0) then
          k = int(w)
          t = w - k
          k = 13 * (k - 2)
          y = (((((((((((b(k) * t + b(k + 1)) * t + &
             b(k + 2)) * t + b(k + 3)) * t + b(k + 4)) * t + &
             b(k + 5)) * t + b(k + 6)) * t + b(k + 7)) * t + &
             b(k + 8)) * t + b(k + 9)) * t + b(k + 10)) * t + &
             b(k + 11)) * t + b(k + 12)
          y = y * y
          y = y * y
          y = y * y
          y = 1 - y * y
      else
          y = 1
      end if
      if (x .lt. 0) y = -y
      derf = y
      end function derf


end module microp_aero

