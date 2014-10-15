


      module ndrop

      use shr_kind_mod,  only: r8 => shr_kind_r8





      use module_cam_support, only: endrun, iulog
      use modal_aero_data, only: ntot_amode



      implicit none

      private
      save

      public activate_init, dropmixnuc

      real(r8) :: npv(ntot_amode) 
      real(r8) :: alogsig(ntot_amode) 
      real(r8) :: exp45logsig(ntot_amode)
      real(r8) :: argfactor(ntot_amode)
      real(r8) :: f1(ntot_amode),f2(ntot_amode)  
      real(r8) :: t0 
      real(r8) :: aten
      real(r8) :: surften       
      real(r8) :: alogten,alog2,alog3,alogaten
      real(r8) :: third, twothird, sixth, zero
      real(r8) :: sq2, sqpi, pi

      type qqcw_type
         real(r8), pointer :: fldcw(:,:)
      end type qqcw_type

contains

      subroutine dropmixnuc(lchnk, ncol, ncldwtr,tendnd, temp,omega,  &
                    pmid,pint,pdel,rpdel,zm,kvh,wsub,cldn,cldo,     &
                    raer, cflx, raertend, dtmicro                   &

                    , qqcw_inout                                    &

                                                                    ) 







      use module_cam_support, only: pcols, pver, pverp

      use physconst,     only: gravit, rhoh2o, latvap, cpair, epsilo, rair, mwh2o, r_universal




      use module_cam_support, only: pcnst => pcnst_mp
      use constituents,  only: cnst_get_ind, cnst_name 

      use error_function, only: erf










      use modal_aero_data,    only: numptrcw_amode => numptrcw_amode_mp, &
           nspec_amode  => nspec_amode_mp, lmassptrcw_amode => lmassptrcw_amode_mp, &
           numptr_amode => numptr_amode_mp  , lmassptr_amode   => lmassptr_amode_mp, &
           cnst_name_cw => cnst_name_cw_mp
      use module_cam_support, only: fieldname_len, outfld


      implicit none



      integer, intent(in) :: lchnk                
      integer, intent(in) :: ncol                 

      real(r8), intent(in) :: dtmicro             
      real(r8), intent(in) :: temp(pcols,pver)    
      real(r8), intent(inout) :: ncldwtr(pcols,pver) 
      real(r8), intent(inout) :: tendnd(pcols,pver) 
      real(r8), intent(in) :: omega(pcols,pver)   
      real(r8), intent(in) :: pmid(pcols,pver)    
      real(r8), intent(in) :: pint(pcols,pverp)   
      real(r8), intent(in) :: pdel(pcols,pver)    
      real(r8), intent(in) :: rpdel(pcols,pver)   
      real(r8), intent(in) :: zm(pcols,pver)      
      real(r8), intent(in) :: kvh(pcols,pverp)    
      real(r8), intent(in) :: wsub(pcols,pver)    
      real(r8), intent(in) :: cldo(pcols,pver)    
      real(r8), intent(in) :: cldn(pcols,pver)    




      real(r8), intent(in) :: raer(pcols,pver,pcnst) 
      real(r8), intent(in) :: cflx(pcols,pcnst) 
      real(r8), intent(out) :: raertend(pcols,pver,pcnst) 

      real(r8), target, intent(inout) :: qqcw_inout(pcols,pver,pcnst) 




      type(qqcw_type) :: QQCW(pcnst)

      real(r8) depvel(pcols,pcnst)
      real(r8) qcld(pver) 
      real(r8) wtke(pcols,pver)     
      real(r8) wtke_cen(pcols,pver) 
      real(r8) zn(pver) 
      real(r8) zs(pver) 
      real(r8), parameter :: zkmin=0.01_r8,zkmax=100._r8
      real(r8) w(pcols,pver)       
      real(r8) cs(pcols,pver)      
      real(r8) dz(pcols,pver)      
      real(r8) zero,flxconv 

      real(r8) wdiab           
      real(r8), parameter :: wmixmin = 0.1 


      real(r8) qncld(pver)     
      real(r8) ekd(pver)       
      real(r8) ekk(0:pver)       
      real(r8) srcn(pver) 
      real(r8), parameter :: sq2pi=2.5066283_r8
      real(r8) dtinv

      logical top        
      integer km1,kp1
      real(r8) wbar,wmix,wmin,wmax
      real(r8) dum,dumc
      real(r8) dact
      real(r8) fluxntot         
      real(r8) fac_srflx
      real(r8) surfrate(pcnst) 
      real(r8) surfratemax      
      real(r8) dtmin,tinv,dtt
      integer nsubmix,nsubmix_bnd
      integer i,k,m,n
      real(r8) tempr4 
      real(r8) dtmix
      real(r8) alogarg
      integer nstep
      real(r8) pi
      integer nnew,nsav,ntemp
      real(r8) overlapp(pver),overlapm(pver) 
      real(r8) ekkp(pver),ekkm(pver) 
      integer count_submix(100)
      save count_submix
      real(r8) kvhmax
      save kvhmax
      real(r8) nsource(pcols,pver)            
      real(r8) ndropmix(pcols,pver)           
      real(r8) ndropcol(pcols)               

      integer lnum, lnumcw, lmass, lmasscw, lphase, &
              lsfc, lsfccw, lsig, lspec, ltype, lwater
      integer ntype(ntot_amode)

      real(r8) na(pcols),va(pcols),hy(pcols)
      real(r8) naermod(ntot_amode) 
      real(r8) sigmag(ntot_amode)  
      real(r8) hygro(ntot_amode)  
      real(r8) vaerosol(ntot_amode) 
      real(r8) raercol(pver,pcnst,2) 
      real(r8) raercol_cw(pver,pcnst,2) 
      real(r8) surfrate_cw(pcnst) 
      real(r8) source(pver) 

      real(r8) fn(ntot_amode)         
      real(r8) fm(ntot_amode)         

      real(r8) fluxn(ntot_amode)      
      real(r8) fluxm(ntot_amode)      
      real(r8) sum,sumcw,flux,fluxcw




      real(r8) nact(pver,ntot_amode)  
      real(r8) mact(pver,ntot_amode)  
      real(r8) sigmag_amode_cur(ntot_amode) 
      save sigmag_amode_cur
      real(r8) :: qqcwtend(pcols,pver,pcnst) 
      real(r8) :: coltend(pcols)    
      real(r8) :: tmpa
      integer :: lc
      character(len=fieldname_len)   :: tmpname
      character(len=fieldname_len+3) :: fieldname
      real(r8) :: csbot(pver)       
      real(r8) :: csbot_cscen(pver) 
      real(r8) :: flux_fullact(pver)     
      real(r8) :: taumix_internal_pver_inv 
      real(r8) dactn,taunuc,damp
      real(r8) :: cldo_tmp, cldn_tmp
      real(r8) :: tau_cld_regenerate

      logical cldinterior

      integer ixndrop, l
      real(r8) naer_tot,naer(pcols)
      integer, parameter :: psat=6 
      real(r8)  :: supersat(psat)= & 
               (/0.02,0.05,0.1,0.2,0.5,1.0/)
      real(r8) ccn(pcols,pver,psat)        
      character(len=8), dimension(psat) :: ccn_name(psat)= &
               (/'CCN1','CCN2','CCN3','CCN4','CCN5','CCN6'/)
      real(r8) arg
      integer phase 



    arg = 1.0_r8
    if (abs(0.8427_r8-erf(arg))/0.8427_r8>0.001_r8) then
       write (iulog,*) 'erf(1.0) = ',ERF(arg)

       call wrf_message(iulog)

       call endrun('dropmixnuc: Error function error')
    endif
    arg = 0.0
    if (erf(arg) /= 0.0) then
       write (iulog,*) 'erf(0.0) = ',erf(arg)

       call wrf_message(iulog)

       write (iulog,*) 'dropmixnuc: Error function error'

       call wrf_message(iulog)

       call endrun('dropmixnuc: Error function error')
    endif
     zero=0._r8


       pi = 4._r8*atan(1.0_r8)
       dtinv=1./dtmicro







       call cnst_get_ind('NUMLIQ', ixndrop)
       depvel(:,:) = 0.0_r8        




       do m=1,ntot_amode



          QQCW(numptrcw_amode(m))%fldcw => qqcw_inout(:,:,numptrcw_amode(m))

          do l=1,nspec_amode(m)



             QQCW(lmassptrcw_amode(l,m))%fldcw => qqcw_inout(:,:,lmassptrcw_amode(l,m))

          end do
       end do



overall_main_i_loop: &
      do i=1,ncol






         do k=1,pver-1
	    zs(k)=1._r8/(zm(i,k)-zm(i,k+1))
	 enddo
	 zs(pver)=zs(pver-1)

	 do k=1,pver
            qcld(k)=ncldwtr(i,k)
            qncld(k)=0._r8
            srcn(k)=0._r8
	    cs(i,k)=pmid(i,k)/(rair*temp(i,k)) 
	    dz(i,k)=1._r8/(cs(i,k)*gravit*rpdel(i,k)) 
	    w(i,k)=-1._r8*omega(i,k)/(cs(i,k)*gravit) 
            do m=1,ntot_amode
	       nact(k,m)=0._r8
	       mact(k,m)=0._r8
            enddo
            zn(k)=gravit*rpdel(i,k)
	    kvhmax=max(kvh(i,k),kvhmax)
	    if(k<pver)then
               ekd(k)=kvh(i,k+1)
               ekd(k)=max(ekd(k),zkmin)
               ekd(k)=min(ekd(k),zkmax)
               csbot(k)=2.0_r8*pint(i,k+1)/(rair*(temp(i,k)+temp(i,k+1)))
               csbot_cscen(k) = csbot(k)/cs(i,k)
	    else
               ekd(k)=0._r8
               csbot(k)=cs(i,k)
               csbot_cscen(k) = 1.0_r8
	    endif

            if(k.eq.pver)then
               wtke(i,k)=sq2pi*depvel(i,ixndrop)

               wtke(i,k)=max(wtke(i,k),wmixmin)
            else
               wtke(i,k)=sq2pi*ekd(k)/dz(i,k)
            endif









            wtke_cen(i,k)=wsub(i,k)
            wtke(i,k)=wsub(i,k)

            wtke_cen(i,k)=max(wtke_cen(i,k),wmixmin)
            wtke(i,k)=max(wtke(i,k),wmixmin)
            nsource(i,k)=0._r8
         enddo

            surfratemax = 0.0_r8
	    nsav=1
	    nnew=2
            surfrate(ixndrop)=depvel(i,ixndrop)/dz(i,pver)
            surfratemax = max( surfratemax, surfrate(ixndrop) )
            do m=1,ntot_amode
               lnum=numptr_amode(m)
               lnumcw=numptrcw_amode(m)
	       if(lnum>0)then
                  surfrate(lnum)=depvel(i,lnum)/dz(i,pver)

                  surfrate_cw(lnumcw)=surfrate(ixndrop)
                  surfratemax = max( surfratemax, surfrate(lnum) )

                  raercol_cw(:,lnumcw,nsav)=qqcw(lnumcw)%fldcw(i,:)   
		  raercol(:,lnum,nsav)=raer(i,:,lnum)
	       endif
               do l=1,nspec_amode(m)
                  lmass=lmassptr_amode(l,m)
                  lmasscw=lmassptrcw_amode(l,m)
                  surfrate(lmass)=depvel(i,lmass)/dz(i,pver)

                  surfrate_cw(lmasscw)=surfrate(ixndrop)
                  surfratemax = max( surfratemax, surfrate(lmass) )

                  raercol_cw(:,lmasscw,nsav)=qqcw(lmasscw)%fldcw(i,:)  
		  raercol(:,lmass,nsav)=raer(i,:,lmass)
               enddo




            enddo






            tau_cld_regenerate = 3600.0_r8 * 3.0_r8 


grow_shrink_main_k_loop: &
         do k=1,pver
            km1=max0(k-1,1)
            kp1=min0(k+1,pver)




              cldo_tmp = cldo(i,k)
              cldn_tmp = cldn(i,k) * exp( -dtmicro/tau_cld_regenerate )



              if(cldn_tmp.lt.cldo_tmp)then


		 nsource(i,k)=nsource(i,k)+qcld(k)*(cldn_tmp-cldo_tmp)/cldo_tmp*dtinv
                 qcld(k)=qcld(k)*(1.+(cldn_tmp-cldo_tmp)/cldo_tmp)




                  dumc=(cldn_tmp-cldo_tmp)/cldo_tmp
                  do m=1,ntot_amode
		     lnum=numptr_amode(m)
		     lnumcw=numptrcw_amode(m)
		     if(lnum.gt.0)then
		        dact=raercol_cw(k,lnumcw,nsav)*dumc

                        raercol_cw(k,lnumcw,nsav)=raercol_cw(k,lnumcw,nsav)+dact   
		        raercol(k,lnum,nsav)=raercol(k,lnum,nsav)-dact
		     endif
		     do l=1,nspec_amode(m)
		        lmass=lmassptr_amode(l,m)
			lmasscw=lmassptrcw_amode(l,m)
			dact=raercol_cw(k,lmasscw,nsav)*dumc

                        raercol_cw(k,lmasscw,nsav)=raercol_cw(k,lmasscw,nsav)+dact  
			raercol(k,lmass,nsav)=raercol(k,lmass,nsav)-dact
                     enddo
                  enddo
               endif




              cldo_tmp = cldn_tmp
              cldn_tmp = cldn(i,k)

              if(cldn_tmp-cldo_tmp.gt.0.01)then



                 wbar=wtke_cen(i,k)
                 wmix=0._r8
                 wmin=0._r8
                 wmax=10._r8
                 wdiab=0


                phase=1 
                 do m=1,ntot_amode
                    call loadaer(raer,qqcw,i,i,k,m,cs,npv(m),phase, &
                         na, va,  hy )
			 naermod(m)=na(i)
			 vaerosol(m)=va(i)
			 hygro(m)=hy(i)
                 end do






                 call activate_modal(wbar,wmix,wdiab,wmin,wmax,temp(i,k),cs(i,k), &
                      naermod, ntot_amode,ntot_amode, vaerosol, sigmag,hygro,       &
                      fn,fm,fluxn,fluxm,flux_fullact(k))





                 dumc=(cldn_tmp-cldo_tmp)
                 do m=1,ntot_amode
                    lnum=numptr_amode(m)
                    lnumcw=numptrcw_amode(m)
		    dact=dumc*fn(m)*raer(i,k,lnum) 
                    qcld(k)=qcld(k)+dact
		    nsource(i,k)=nsource(i,k)+dact*dtinv
		    if(lnum.gt.0)then
                       raercol_cw(k,lnumcw,nsav)=raercol_cw(k,lnumcw,nsav)+dact  
		       raercol(k,lnum,nsav)=raercol(k,lnum,nsav)-dact
		    endif
                    dum=dumc*fm(m)
                    do l=1,nspec_amode(m)
		        lmass=lmassptr_amode(l,m)
			lmasscw=lmassptrcw_amode(l,m)
			dact=dum*(raer(i,k,lmass)) 
                        raercol_cw(k,lmasscw,nsav)=raercol_cw(k,lmasscw,nsav)+dact  
			raercol(k,lmass,nsav)=raercol(k,lmass,nsav)-dact
                    enddo
                 enddo
               endif

         enddo grow_shrink_main_k_loop













old_cloud_main_k_loop: &
         do k=1,pver
            km1=max0(k-1,1)
            kp1=min0(k+1,pver)
            taumix_internal_pver_inv = 0.0

            if(cldn(i,k).gt.0.01)then


               wdiab=0


               wmix=0._r8 
	       wbar=wtke(i,k) 

	       if (k == pver) wbar=wtke_cen(i,k) 
               wmax=10._r8
	       wmin=0._r8

                  if(cldn(i,k)-cldn(i,kp1).gt.0.01.or.k.eq.pver)then


                    
                    ekd(k)=wtke(i,k)*dz(i,k)/sq2pi










                    ekd(k)=wbar/zs(k)

                    alogarg=max(1.e-20_r8,1/cldn(i,k)-1._r8)
                    wmin=wbar+wmix*0.25*sq2pi*log(alogarg)
                    phase=1 
                    do m=1,ntot_amode



                       call loadaer(raer,qqcw,i,i,kp1,m,cs, npv(m),phase, &
                         na, va,  hy )
			 naermod(m)=na(i)
			 vaerosol(m)=va(i)
			 hygro(m)=hy(i)
                    end do





                    call activate_modal(wbar,wmix,wdiab,wmin,wmax,temp(i,k),cs(i,k), &
                      naermod, ntot_amode,ntot_amode, vaerosol, sigmag, hygro,    &
                      fn,fm,fluxn,fluxm,flux_fullact(k))




                      if(k.lt.pver)then
                          dumc = cldn(i,k)-cldn(i,kp1)
                      else
                          dumc=cldn(i,k)
                      endif
                    fluxntot=0







                    dum=csbot_cscen(k)/(dz(i,k))




























                    if (k == pver) then
                       taumix_internal_pver_inv = flux_fullact(k)/dz(i,k)
                    end if
                    do m=1,ntot_amode
                       fluxn(m)=fluxn(m)*dumc
                       fluxm(m)=fluxm(m)*dumc
                       lnum=numptr_amode(m)
                       nact(k,m)=nact(k,m)+fluxn(m)*dum
                       mact(k,m)=mact(k,m)+fluxm(m)*dum
                       if (k > pver) then

                          fluxntot = fluxntot &
                                   +fluxn(m)*raercol(kp1,lnum,nsav)*cs(i,k)
                       else
                          lnumcw=numptrcw_amode(m)
                          tmpa = raercol(kp1,lnum,nsav)*fluxn(m) &
                               + raercol(kp1,lnumcw,nsav)*(fluxn(m) &
                                    -taumix_internal_pver_inv)
                          fluxntot = fluxntot + max(0.0_r8,tmpa)*cs(i,k)
                       end if
                    enddo
                      srcn(k)=srcn(k)+fluxntot/(cs(i,k)*dz(i,k))
		      nsource(i,k)=nsource(i,k)+fluxntot/(cs(i,k)*dz(i,k))

                 endif  




            else

               nsource(i,k)=nsource(i,k)-qcld(k)*dtinv
               qcld(k)=0

               do m=1,ntot_amode
	          lnum=numptr_amode(m)
		  lnumcw=numptrcw_amode(m)
		  if(lnum.gt.0)then
                     raercol(k,lnum,nsav)=raercol(k,lnum,nsav)+raercol_cw(k,lnumcw,nsav)  
                     raercol_cw(k,lnumcw,nsav)=0.
		  endif
		  do l=1,nspec_amode(m)
		     lmass=lmassptr_amode(l,m)
		     lmasscw=lmassptrcw_amode(l,m)
                     raercol(k,lmass,nsav)=raercol(k,lmass,nsav)+raercol_cw(k,lmasscw,nsav) 
                     raercol_cw(k,lmasscw,nsav)=0.
                  enddo
                enddo
            endif
         enddo old_cloud_main_k_loop

 	    ntemp=nsav
 	    nsav=nnew
 	    nnew=ntemp




         dtmin=dtmicro
         ekk(0)=0.0
         ekk(pver)=0.0
         do k=1,pver-1





            ekk(k)=ekd(k)*csbot(k)
         enddo
         do k=1,pver
            km1=max0(k-1,1)
            ekkp(k)=zn(k)*ekk(k)*zs(k)
            ekkm(k)=zn(k)*ekk(k-1)*zs(km1)
            tinv=ekkp(k)+ekkm(k)

            if(k.eq.pver)tinv=tinv+surfratemax









            if(k.eq.pver)tinv=tinv+taumix_internal_pver_inv

            if(tinv.gt.1.e-6)then
               dtt=1./tinv
               dtmin=min(dtmin,dtt)
            endif
         enddo
         dtmix=0.9*dtmin
         nsubmix=dtmicro/dtmix+1
	 if(nsubmix>100)then
	    nsubmix_bnd=100
	 else
	    nsubmix_bnd=nsubmix
	 endif
	 count_submix(nsubmix_bnd)=count_submix(nsubmix_bnd)+1
         dtmix=dtmicro/nsubmix
         fac_srflx = -1.0/(zn(pver)*nsubmix)

	 do k=1,pver
            kp1=min(k+1,pver)
            km1=max(k-1,1)
	    
            if(cldn(i,kp1).gt.1.e-10_r8)then
               overlapp(k)=min(cldn(i,k)/cldn(i,kp1),1._r8)
            else
               overlapp(k)=1.
            endif
            if(cldn(i,km1).gt.1.e-10_r8)then
               overlapm(k)=min(cldn(i,k)/cldn(i,km1),1._r8)
            else
               overlapm(k)=1.
            endif
	 enddo








         do k = 1, pver-1
         do m = 1, ntot_amode
            nact(k,m) = min( nact(k,m), ekkp(k) )
            mact(k,m) = min( mact(k,m), ekkp(k) )
         end do
         end do


old_cloud_nsubmix_loop: &
         do n=1,nsubmix
            qncld(:)=qcld(:)

	    ntemp=nsav
	    nsav=nnew
	    nnew=ntemp
            srcn(:)=0.0
            do m=1,ntot_amode
               lnum=numptr_amode(m)
               lnumcw=numptrcw_amode(m)



               srcn(1:pver-1)=srcn(1:pver-1)+nact(1:pver-1,m)*(raercol(2:pver,lnum,nsav))


               tmpa = raercol(pver,lnum,nsav)*nact(pver,m) &
                    + raercol(pver,lnumcw,nsav)*(nact(pver,m) - taumix_internal_pver_inv)
               srcn(pver)=srcn(pver) + max(0.0_r8,tmpa)
            enddo
            call explmix(qcld,srcn,ekkp,ekkm,overlapp,overlapm,   &
                         qncld,surfrate(ixndrop),zero,pver,dtmix,.false.)








            do m=1,ntot_amode
               lnum=numptr_amode(m)
               lnumcw=numptrcw_amode(m)
               if(lnum>0)then


                  source(1:pver-1)= nact(1:pver-1,m)*(raercol(2:pver,lnum,nsav))


                  tmpa = raercol(pver,lnum,nsav)*nact(pver,m) &
                       + raercol(pver,lnumcw,nsav)*(nact(pver,m) - taumix_internal_pver_inv)
                  source(pver) = max(0.0_r8,tmpa)

                  flxconv=0.
                  call explmix(raercol_cw(1,lnumcw,nnew),source,ekkp,ekkm,overlapp,overlapm, &
                               raercol_cw(1,lnumcw,nsav),surfrate_cw(lnumcw),zero,pver,dtmix,&
                               .false.)
                  call explmix(raercol(1,lnum,nnew),source,ekkp,ekkm,overlapp,overlapm,  &
                               raercol(1,lnum,nsav),surfrate(lnum),flxconv,pver,dtmix, &
                               .true.,raercol_cw(1,lnumcw,nsav))
               endif

               do l=1,nspec_amode(m)
                  lmass=lmassptr_amode(l,m)
                  lmasscw=lmassptrcw_amode(l,m)


                  source(1:pver-1)= mact(1:pver-1,m)*(raercol(2:pver,lmass,nsav))


                  tmpa = raercol(pver,lmass,nsav)*mact(pver,m) &
                       + raercol(pver,lmasscw,nsav)*(mact(pver,m) - taumix_internal_pver_inv)
                  source(pver) = max(0.0_r8,tmpa)

		  flxconv=0.

                  call explmix(raercol_cw(1,lmasscw,nnew),source,ekkp,ekkm,overlapp,overlapm, &
                               raercol_cw(1,lmasscw,nsav),surfrate_cw(lmasscw),zero,pver,dtmix,&
                               .false.)
                  call explmix(raercol(1,lmass,nnew),source,ekkp,ekkm,overlapp,overlapm,  &
                               raercol(1,lmass,nsav),surfrate(lmass),flxconv, pver,dtmix,&
                               .true.,raercol_cw(1,lmasscw,nsav))
	       enddo   





            enddo   

         enddo old_cloud_nsubmix_loop




         do k=1,pver
	    if(cldn(i,k).eq.0.)then

               qcld(k)=0.

               do m=1,ntot_amode
	          lnum=numptr_amode(m)
		  lnumcw=numptrcw_amode(m)
		  if(lnum.gt.0)then
                        raercol(k,lnum,nnew)=raercol(k,lnum,nnew)+raercol_cw(k,lnumcw,nnew)
                        raercol_cw(k,lnumcw,nnew)=0.
		  endif

		  do l=1,nspec_amode(m)
		        lmass=lmassptr_amode(l,m)
			lmasscw=lmassptrcw_amode(l,m)


                        raercol(k,lmass,nnew)=raercol(k,lmass,nnew)+raercol_cw(k,lmasscw,nnew)
                        raercol_cw(k,lmasscw,nnew)=0.
                  enddo
                enddo
            endif
         enddo



         ndropcol(i)=0.
         do k=1,pver
	    ndropmix(i,k)=(qcld(k)-ncldwtr(i,k))*dtinv - nsource(i,k)
	    tendnd(i,k) = (max(qcld(k),1.e-6_r8)-ncldwtr(i,k))*dtinv
	    ndropcol(i) = ndropcol(i) + ncldwtr(i,k)*pdel(i,k)
	 enddo
	 ndropcol(i) = ndropcol(i)/gravit



         do m=1,ntot_amode
            lnum=numptr_amode(m)
            lnumcw=numptrcw_amode(m)
	    if(lnum.gt.0)then
               qqcwtend(i,:,lnumcw)=(raercol_cw(:,lnumcw,nnew)-qqcw(lnumcw)%fldcw(i,:))*dtinv
               qqcw(lnumcw)%fldcw(i,:)=raercol_cw(:,lnumcw,nnew)    
	       raertend(i,:,lnum)= (raercol(:,lnum,nnew)-raer(i,:,lnum))*dtinv
	    endif
	    do l=1,nspec_amode(m)
	       lmass=lmassptr_amode(l,m)
	       lmasscw=lmassptrcw_amode(l,m)
               qqcwtend(i,:,lmasscw)=(raercol_cw(:,lmasscw,nnew)-qqcw(lmasscw)%fldcw(i,:))*dtinv
               qqcw(lmasscw)%fldcw(i,:)=raercol_cw(:,lmasscw,nnew)   
	       raertend(i,:,lmass)=(raercol(:,lmass,nnew)-raer(i,:,lmass))*dtinv
            enddo


         enddo

      enddo overall_main_i_loop


      call outfld('NDROPCOL', ndropcol  , pcols, lchnk   )
      call outfld('NDROPSRC', nsource    , pcols, lchnk   )
      call outfld('NDROPMIX', ndropmix    , pcols, lchnk   )
      call outfld('LCLOUD  ', cldn    , pcols, lchnk   )
      call outfld('WTKE    ', wtke    , pcols, lchnk   )

      call ccncalc(lchnk,ncol,temp,cs,raer,qqcw,ccn,psat,supersat,alogsig,npv)
      do l=1,psat
         call outfld(ccn_name(l), ccn(1,1,l)    , pcols, lchnk   )
      enddo


      do m = 1, ntot_amode
      do lphase = 1, 2
      do lspec = 0, nspec_amode(m)+1   
         if (lspec == 0) then   
            if (lphase == 1) then
               l = numptr_amode(m)
            else
               l = numptrcw_amode(m)
            endif
         else if (lspec <= nspec_amode(m)) then   
            if (lphase == 1) then
               l = lmassptr_amode(lspec,m)
            else
               l = lmassptrcw_amode(lspec,m)
            endif
         else   



               cycle

         end if
         if (l <= 0) cycle



         coltend(:) = 0.0
         if (lphase == 1) then
            tmpname = cnst_name(l)
            do i = 1, ncol
               coltend(i) = sum( pdel(i,:)*raertend(i,:,l) )/gravit

            end do
         else
            tmpname = cnst_name_cw(l)
            do i = 1, ncol
               coltend(i) = sum( pdel(i,:)*qqcwtend(i,:,l) )/gravit
            end do
         end if
         fieldname = trim(tmpname) // '_mixnuc1'
         call outfld( fieldname, coltend, pcols, lchnk)

      end do   
      end do   
      end do   

      return
      end subroutine dropmixnuc

   subroutine explmix( q, src, ekkp, ekkm, overlapp, overlapm, &
                       qold, surfrate, flxconv, pver, dt, is_unact, qactold )



      

   implicit none
   integer, intent(in) :: pver 
   real(r8), intent(out) :: q(pver) 
   real(r8), intent(in) :: qold(pver) 
   real(r8), intent(in) :: src(pver) 
   real(r8), intent(in) :: ekkp(pver) 
                      
   real(r8), intent(in) :: ekkm(pver) 
                      
   real(r8), intent(in) :: overlapp(pver) 
   real(r8), intent(in) :: overlapm(pver) 
   real(r8), intent(in) :: surfrate 
   real(r8), intent(in) :: flxconv 
   real(r8), intent(in) :: dt 
   logical, intent(in) :: is_unact 
   real(r8), intent(in),optional :: qactold(pver)
          
          
          

   integer k,kp1,km1

   if ( is_unact ) then

      do k=1,pver
         kp1=min(k+1,pver)
         km1=max(k-1,1)
         q(k) = qold(k) + dt*( - src(k) + ekkp(k)*(qold(kp1) - qold(k) +       &
                           qactold(kp1)*(1.0-overlapp(k)))               &
                                  + ekkm(k)*(qold(km1) - qold(k) +     &
                           qactold(km1)*(1.0-overlapm(k))) )



            q(k)=max(q(k),0._r8)

      end do

      q(pver)=q(pver)-surfrate*qold(pver)*dt+flxconv*dt



            q(pver)=max(q(pver),0._r8)

   else
      do k=1,pver
         kp1=min(k+1,pver)
         km1=max(k-1,1)
         q(k) = qold(k) + dt*(src(k) + ekkp(k)*(overlapp(k)*qold(kp1)-qold(k)) +      &
                                    ekkm(k)*(overlapm(k)*qold(km1)-qold(k)) )



            q(k)=max(q(k),0._r8)

      end do

      q(pver)=q(pver)-surfrate*qold(pver)*dt+flxconv*dt



            q(pver)=max(q(pver),0._r8)

   end if

   return
   end subroutine explmix

   subroutine activate_init



      use module_cam_support, only: pver





      use modal_aero_data, only:sigmag_amode => sigmag_amode_mp, &
           numptr_amode   => numptr_amode_mp, nspec_amode => nspec_amode_mp, &
           lmassptr_amode => lmassptr_amode_mp, dgnum_amode => dgnum_amode_mp
      use module_cam_support, only: addfld, add_default, phys_decomp

      use physconst, only: rhoh2o, mwh2o, r_universal
      use error_function, only: erf

      implicit none

      integer l,m
      real(r8) arg
      integer lnum, lmass

      character*16 ccn_longname



      zero=0._r8
      third=1./3._r8
      twothird=2.*third
      sixth=1./6._r8
      sq2=sqrt(2._r8)
      pi=4._r8*atan(1.0_r8)
      sqpi=sqrt(pi)

      t0=273.
      surften=0.076_r8
      aten=2.*mwh2o*surften/(r_universal*t0*rhoh2o)
      alogaten=log(aten)
      alog2=log(2._r8)
      alog3=log(3._r8)

      do m=1,ntot_amode

          alogsig(m)=log(sigmag_amode(m))
          exp45logsig(m)=exp(4.5*alogsig(m)*alogsig(m))
	  argfactor(m)=2./(3.*sqrt(2.)*alogsig(m))
          f1(m)=0.5*exp(2.5*alogsig(m)*alogsig(m))
	  f2(m)=1.+0.25*alogsig(m)
          lnum=numptr_amode(m)
          do l=1,nspec_amode(m)
               lmass=lmassptr_amode(l,m)
               if(lmass.le.0)then
                  write(iulog,*)'lmassptr_amode(',l,m,')=',lmass

                  call wrf_message(iulog)

                  call endrun
               endif
          enddo
          npv(m)=6./(pi*dgnum_amode(m)**3*exp45logsig(m))
      enddo
      
      call addfld ('WTKE     ', 'm/s     ', pver, 'A', 'Standard deviation of updraft velocity'                  ,phys_decomp)
      call addfld ('LCLOUD   ', '        ', pver, 'A', 'Liquid cloud fraction'                                   ,phys_decomp)
      call addfld ('NDROPMIX ', '#/kg/s  ', pver, 'A', 'Droplet number mixing'                     ,phys_decomp)
      call addfld ('NDROPSRC ', '#/kg/s  ', pver, 'A', 'Droplet number source'                     ,phys_decomp)
      call addfld ('NDROPSNK ', '#/kg/s  ', pver, 'A', 'Droplet number loss by microphysics'       ,phys_decomp)
      call addfld ('NDROPCOL ', '#/m2    ', 1,    'A', 'Column droplet number'                     ,phys_decomp)
      call add_default ('WTKE    ', 1, ' ')
      call add_default ('LCLOUD  ', 1, ' ')

      return
      end subroutine activate_init

      subroutine activate_modal(wbar, sigw, wdiab, wminf, wmaxf, tair, rhoair,  &
                          na, pmode, nmode, volume, sigman, hygro, &
                          fn, fm, fluxn, fluxm, flux_fullact )











      use physconst, only: rair, epsilo, cpair, rh2o, latvap, gravit,   &
                                 rhoh2o, mwh2o, r_universal
      use wv_saturation, only: estblf, epsqs
      use error_function, only: erf

      implicit none




      integer pmode 
      real(r8) wbar          
      real(r8) sigw          
      real(r8) wdiab         
      real(r8) wminf         
      real(r8) wmaxf         
      real(r8) tair          
      real(r8) rhoair        
      real(r8) na(pmode)           
      integer nmode      
      real(r8) volume(pmode)     
      real(r8) sigman(pmode)  
      real(r8) hygro(pmode)  



      real(r8) fn(pmode)      
      real(r8) fm(pmode)      
      real(r8) fluxn(pmode)   
      real(r8) fluxm(pmode)   
      real(r8) flux_fullact   
                              
                              
                              
                              



      integer, parameter:: nx=200
      integer iquasisect_option, isectional
      real(r8) integ,integf
      real(r8), parameter :: p0 = 1013.25e2_r8    
      real(r8) xmin(ntot_amode),xmax(ntot_amode) 
      real(r8) volmin(ntot_amode),volmax(ntot_amode) 
      real(r8) tmass 
      real(r8) sign(ntot_amode)    
      real(r8) rm 
      real(r8) pres 
      real(r8) path 
      real(r8) diff 
      real(r8) conduct 
      real(r8) diff0,conduct0
      real(r8) es 
      real(r8) qs 
      real(r8) dqsdt 
      real(r8) dqsdp 
      real(r8) g 
      real(r8) zeta(ntot_amode), eta(ntot_amode)
      real(r8) lnsmax 
      real(r8) alpha
      real(r8) gamma
      real(r8) beta
      real(r8) sqrtg(ntot_amode)
      real(r8) :: amcube(ntot_amode) 
      real(r8) :: smcrit(ntot_amode) 
      real(r8) :: lnsm(ntot_amode) 
      real(r8) smc(ntot_amode) 
      real(r8) sumflx_fullact
      real(r8) sumflxn(ntot_amode)
      real(r8) sumflxm(ntot_amode)
      real(r8) sumfn(ntot_amode)
      real(r8) sumfm(ntot_amode)
      real(r8) fnold(ntot_amode)   
      real(r8) fmold(ntot_amode)   
      real(r8) wold,gold
      real(r8) alogam
      real(r8) rlo,rhi,xint1,xint2,xint3,xint4
      real(r8) wmin,wmax,w,dw,dwmax,dwmin,wnuc,dwnew,wb
      real(r8) dfmin,dfmax,fnew,fold,fnmin,fnbar,fsbar,fmbar
      real(r8) alw,sqrtalw
      real(r8) smax
      real(r8) x,arg
      real(r8) xmincoeff,xcut,volcut,surfcut
      real(r8) z,z1,z2,wf1,wf2,zf1,zf2,gf1,gf2,gf
      real(r8) etafactor1,etafactor2(ntot_amode),etafactor2max
      integer m,n

      real(r8), parameter :: eps=0.3_r8,fmax=0.99_r8,sds=3._r8
 
      real(r8), parameter :: namin=1.e6_r8   

      integer ndist(nx)  
      data ndist/nx*0/
      save ndist

      if(ntot_amode<pmode)then
         write(iulog,*)'ntot_amode,pmode in activate =',ntot_amode,pmode

         call wrf_message(iulog)

	 call endrun('activate')
      endif

      fn(:)=0._r8
      fm(:)=0._r8
      fluxn(:)=0._r8
      fluxm(:)=0._r8
      flux_fullact=0._r8

      if(nmode.eq.1.and.na(1).lt.1.e-20_r8)return

      if(sigw.le.1.e-5_r8.and.wbar.le.0.)return

      pres=rair*rhoair*tair
      diff0=0.211e-4_r8*(p0/pres)*(tair/t0)**1.94
      conduct0=(5.69_r8+0.017_r8*(tair-t0))*4.186e2_r8*1.e-5_r8 
      es = estblf(tair)
      qs = epsilo*es/(pres-(1.0_r8 - epsqs)*es)
      dqsdt=latvap/(rh2o*tair*tair)*qs
      alpha=gravit*(latvap/(cpair*rh2o*tair*tair)-1./(rair*tair))
      gamma=(1+latvap/cpair*dqsdt)/(rhoair*qs)
      etafactor2max=1.e10/(alpha*wmaxf)**1.5 

      do m=1,nmode
         if(volume(m).gt.1.e-39_r8.and.na(m).gt.1.e-39_r8)then


            amcube(m)=(3.*volume(m)/(4.*pi*exp45logsig(m)*na(m)))  




            g=1._r8/(rhoh2o/(diff0*rhoair*qs)                                    &
              +latvap*rhoh2o/(conduct0*tair)*(latvap/(rh2o*tair)-1._r8))
            sqrtg(m)=sqrt(g)
            beta=2._r8*pi*rhoh2o*g*gamma
            etafactor2(m)=1._r8/(na(m)*beta*sqrtg(m))
            if(hygro(m).gt.1.e-10)then
               smc(m)=2.*aten*sqrt(aten/(27.*hygro(m)*amcube(m))) 
            else
               smc(m)=100.
            endif

         else
            g=1._r8/(rhoh2o/(diff0*rhoair*qs)                                    &
              +latvap*rhoh2o/(conduct0*tair)*(latvap/(rh2o*tair)-1._r8))
            sqrtg(m)=sqrt(g)
            smc(m)=1._r8
	    etafactor2(m)=etafactor2max 
         endif
         lnsm(m)=log(smc(m)) 


      enddo

      if(sigw.gt.1.e-5_r8)then 

         wmax=min(wmaxf,wbar+sds*sigw)
         wmin=max(wminf,-wdiab)
         wmin=max(wmin,wbar-sds*sigw)
         w=wmin
         dwmax=eps*sigw
         dw=dwmax
         dfmax=0.2_r8
         dfmin=0.1_r8
         if(wmax.le.w)then
            do m=1,nmode
               fluxn(m)=0.
               fn(m)=0.
               fluxm(m)=0.
               fm(m)=0.
            enddo
            flux_fullact=0._r8
            return
         endif
         do m=1,nmode
            sumflxn(m)=0._r8
            sumfn(m)=0._r8
	    fnold(m)=0._r8
            sumflxm(m)=0._r8
            sumfm(m)=0._r8
	    fmold(m)=0._r8
         enddo
         sumflx_fullact=0._r8

         fold=0._r8
         wold=0._r8
         gold=0._r8

         dwmin = min( dwmax, 0.01_r8 )

         do n=1,200
 100        wnuc=w+wdiab

            alw=alpha*wnuc
            sqrtalw=sqrt(alw)
	    etafactor1=alw*sqrtalw

              do m=1,nmode
	         eta(m)=etafactor1*etafactor2(m)
                 zeta(m)=twothird*sqrtalw*aten/sqrtg(m)
              enddo

              call maxsat(zeta,eta,nmode,smc,smax)


              lnsmax=log(smax)

              x=twothird*(lnsm(nmode)-lnsmax)/(sq2*alogsig(nmode))
              fnew=0.5_r8*(1._r8-erf(x))


            dwnew = dw
            if(fnew-fold.gt.dfmax.and.n.gt.1)then

	       if (dw .gt. 1.01*dwmin) then
                  dw=0.7_r8*dw
                  dw=max(dw,dwmin)
                  w=wold+dw
                  go to 100
               else
                  dwnew = dwmin
               endif
            endif

            if(fnew-fold.lt.dfmin)then

	       dwnew=min(1.5_r8*dw,dwmax)
            endif
            fold=fnew

            z=(w-wbar)/(sigw*sq2)
            g=exp(-z*z)
            fnmin=1._r8
            xmincoeff=alogaten-twothird*(lnsmax-alog2)-alog3

            do m=1,nmode

               x=twothird*(lnsm(m)-lnsmax)/(sq2*alogsig(m))
               fn(m)=0.5_r8*(1.-erf(x))
               fnmin=min(fn(m),fnmin)


               fnbar=(fn(m)*g+fnold(m)*gold)
	       arg=x-1.5_r8*sq2*alogsig(m)
               fm(m)=0.5_r8*(1._r8-erf(arg))
               fmbar=(fm(m)*g+fmold(m)*gold)
               wb=(w+wold)
               if(w.gt.0.)then
                  sumflxn(m)=sumflxn(m)+sixth*(wb*fnbar           &
                      +(fn(m)*g*w+fnold(m)*gold*wold))*dw
                  sumflxm(m)=sumflxm(m)+sixth*(wb*fmbar           &
                      +(fm(m)*g*w+fmold(m)*gold*wold))*dw
               endif
               sumfn(m)=sumfn(m)+0.5_r8*fnbar*dw

               fnold(m)=fn(m)
               sumfm(m)=sumfm(m)+0.5_r8*fmbar*dw
               fmold(m)=fm(m)
            enddo

            sumflx_fullact = sumflx_fullact &
                           + sixth*(wb*(g+gold) + (g*w+gold*wold))*dw

            gold=g
            wold=w
            dw=dwnew
            if(n.gt.1.and.(w.gt.wmax.or.fnmin.gt.fmax))go to 20
            w=w+dw
         enddo
         write(iulog,*)'do loop is too short in activate'

         call wrf_message(iulog)

         write(iulog,*)'wmin=',wmin,' w=',w,' wmax=',wmax,' dw=',dw

         call wrf_message(iulog)

         write(iulog,*)'wbar=',wbar,' sigw=',sigw,' wdiab=',wdiab

         call wrf_message(iulog)

         write(iulog,*)'wnuc=',wnuc

         call wrf_message(iulog)

         write(iulog,*)'na=',(na(m),m=1,nmode)

         call wrf_message(iulog)

         write(iulog,*)'fn=',(fn(m),m=1,nmode)

         call wrf_message(iulog)



         write(iulog,*)'wbar,sigw,wdiab,tair,rhoair,nmode='

         call wrf_message(iulog)

         write(iulog,*) wbar,sigw,wdiab,tair,rhoair,nmode

         call wrf_message(iulog)

         write(iulog,*)'na=',na

         call wrf_message(iulog)

         write(iulog,*)'volume=', (volume(m),m=1,nmode)

         call wrf_message(iulog)

         write(iulog,*)'sigman=',sigman

         call wrf_message(iulog)

         write(iulog,*)'hydro='

         call wrf_message(iulog)

         write(iulog,*) hygro

         call wrf_message(iulog)


         call endrun
   20    continue
         ndist(n)=ndist(n)+1
         if(w.lt.wmaxf)then



            wnuc=w+wdiab

            z1=(w-wbar)/(sigw*sq2)
            z2=(wmaxf-wbar)/(sigw*sq2)
            g=exp(-z1*z1)
            integ=sigw*0.5*sq2*sqpi*(erf(z2)-erf(z1))

            wf1=max(w,zero)
            zf1=(wf1-wbar)/(sigw*sq2)
            gf1=exp(-zf1*zf1)
            wf2=max(wmaxf,zero)
            zf2=(wf2-wbar)/(sigw*sq2)
            gf2=exp(-zf2*zf2)
            gf=(gf1-gf2)
            integf=wbar*sigw*0.5*sq2*sqpi*(erf(zf2)-erf(zf1))+sigw*sigw*gf

            do m=1,nmode
               sumflxn(m)=sumflxn(m)+integf*fn(m)
               sumfn(m)=sumfn(m)+fn(m)*integ
              sumflxm(m)=sumflxm(m)+integf*fm(m)
               sumfm(m)=sumfm(m)+fm(m)*integ
            enddo

            sumflx_fullact = sumflx_fullact + integf

         endif


         do m=1,nmode
            fn(m)=sumfn(m)/(sq2*sqpi*sigw)

            if(fn(m).gt.1.01)then
               write(iulog,*)'fn=',fn(m),' > 1 in activate'

               call wrf_message(iulog)

	       write(iulog,*)'w,m,na,amcube=',w,m,na(m),amcube(m)

               call wrf_message(iulog)

	       write(iulog,*)'integ,sumfn,sigw=',integ,sumfn(m),sigw

               call wrf_message(iulog)

	       call endrun('activate')
            endif
            fluxn(m)=sumflxn(m)/(sq2*sqpi*sigw)
           fm(m)=sumfm(m)/(sq2*sqpi*sigw)

            if(fm(m).gt.1.01)then
               write(iulog,*)'fm=',fm(m),' > 1 in activate'

               call wrf_message(iulog)

            endif
            fluxm(m)=sumflxm(m)/(sq2*sqpi*sigw)
         enddo

         flux_fullact = sumflx_fullact/(sq2*sqpi*sigw)

      else


         wnuc=wbar+wdiab

         if(wnuc.gt.0._r8)then

            w=wbar
            alw=alpha*wnuc
            sqrtalw=sqrt(alw)
	    etafactor1=alw*sqrtalw

            do m=1,nmode
	       eta(m)=etafactor1*etafactor2(m)
               zeta(m)=twothird*sqrtalw*aten/sqrtg(m)
            enddo

            call maxsat(zeta,eta,nmode,smc,smax)

            lnsmax=log(smax)
            xmincoeff=alogaten-twothird*(lnsmax-alog2)-alog3


            do m=1,nmode

		  x=twothird*(lnsm(m)-lnsmax)/(sq2*alogsig(m))
                  fn(m)=0.5_r8*(1._r8-erf(x))
		  arg=x-1.5_r8*sq2*alogsig(m)
                  fm(m)=0.5_r8*(1._r8-erf(arg))
                if(wbar.gt.0._r8)then
                   fluxn(m)=fn(m)*w
                   fluxm(m)=fm(m)*w
               endif
            enddo
            flux_fullact = w
         endif

      endif

      return
      end subroutine activate_modal

      subroutine maxsat(zeta,eta,nmode,smc,smax)







      implicit none

      integer nmode 
      real(r8) smc(ntot_amode) 
      real(r8) zeta(ntot_amode), eta(ntot_amode)
      real(r8) smax 
      integer m  
      real(r8) sum, g1, g2, g1sqrt, g2sqrt

      do m=1,nmode
         if(zeta(m).gt.1.e5_r8*eta(m).or.smc(m)*smc(m).gt.1.e5_r8*eta(m))then

            smax=1.e-20_r8
         else

            go to 1
         endif
      enddo

      return

  1   continue

      sum=0
      do m=1,nmode
         if(eta(m).gt.1.e-20_r8)then
            g1=zeta(m)/eta(m)
            g1sqrt=sqrt(g1)
            g1=g1sqrt*g1
            g1=g1sqrt*g1
            g2=smc(m)/sqrt(eta(m)+3._r8*zeta(m))
            g2sqrt=sqrt(g2)
            g2=g2sqrt*g2
            sum=sum+(f1(m)*g1+f2(m)*g2)/(smc(m)*smc(m))
         else
            sum=1.e20_r8
         endif
      enddo

      smax=1._r8/sqrt(sum)

      return

      end subroutine maxsat

      subroutine ccncalc(lchnk,ncol,tair,cs,raer,qqcw,ccn,psat,supersat,alogsig,npv)








      use shr_kind_mod,  only: r8 => shr_kind_r8





      use module_cam_support, only: pcols, pver, pverp, pcnst =>pcnst_mp

      use physconst,     only: rhoh2o, mwh2o, r_universal
      use modal_aero_data, only:
      use error_function, only: erf

      implicit none



      integer lchnk 
      integer, intent(in) :: ncol 
      integer, intent(in) :: psat 
      real(r8), intent(in) :: raer(pcols,pver,pcnst) 
      type(qqcw_type), intent(in) :: QQCW(:)


      real(r8), intent(in) :: tair(pcols,pver)          
      real(r8), intent(in) :: cs(pcols,pver)    
      real(r8), intent(in) :: supersat(psat)
      real(r8), intent(in) :: npv(ntot_amode) 
      real(r8), intent(in) :: alogsig(ntot_amode) 
      real(r8), intent(out) :: ccn(pcols,pver,psat) 



      real(r8) naerosol(pcols) 
      real(r8) vaerosol(pcols) 
      real(r8) exp45logsig(ntot_amode)     
      real(r8) amcube(pcols)
      real(r8) super(psat) 
      real(r8) amcubecoef(ntot_amode)
      real(r8) :: surften       
      real(r8) surften_coef
      real(r8) a(pcols) 
      real(r8) hygro(pcols)  
      real(r8) sm(pcols)  
      real(r8) argfactor(ntot_amode),arg(pcols)

      real(r8) pi
      real(r8) twothird,sq2
      integer l,m,n,i,k
      real(r8) log,cc
      real(r8) smcoefcoef,smcoef(pcols)
      integer phase 

      super(:)=supersat(:)*0.01
      pi = 4.*atan(1.0)
      sq2=sqrt(2._r8)
      twothird=2._r8/3._r8
      surften=0.076_r8
      surften_coef=2._r8*mwh2o*surften/(r_universal*rhoh2o)
      smcoefcoef=2._r8/sqrt(27._r8)

      do m=1,ntot_amode
          exp45logsig(m)=exp(4.5*alogsig(m)*alogsig(m))
	  amcubecoef(m)=3./(4.*pi*exp45logsig(m))
	  argfactor(m)=twothird/(sq2*alogsig(m))
      end do

      do k=1,pver

         do i=1,ncol
            ccn(i,k,:)=0.
            a(i)=surften_coef/tair(i,k)
	    smcoef(i)=smcoefcoef*a(i)*sqrt(a(i))
	 end do

         do m=1,ntot_amode
            phase=3 
            call loadaer(raer,qqcw,1,ncol,k,m,cs,npv(m), phase, &
                         naerosol, vaerosol,  hygro )
            where(naerosol(:ncol)>1.e-3)
               amcube(:ncol)=amcubecoef(m)*vaerosol(:ncol)/naerosol(:ncol)
	       sm(:ncol)=smcoef(:ncol)/sqrt(hygro(:ncol)*amcube(:ncol)) 
	    elsewhere
	       sm(:ncol)=1. 
	    endwhere
            do l=1,psat
               do i=1,ncol
	          arg(i)=argfactor(m)*log(sm(i)/super(l))
                  ccn(i,k,l)=ccn(i,k,l)+naerosol(i)*0.5*(1._r8-erf(arg(i)))
               enddo
             enddo
         enddo
      enddo
      ccn(:ncol,:,:)=ccn(:ncol,:,:)*1.e-6 

      return
      end subroutine ccncalc

      subroutine loadaer(raer,qqcw,istart,istop,k,m,cs,npv1, phase, &
                         naerosol, vaerosol,  hygro )

      use shr_kind_mod,  only: r8 => shr_kind_r8





      use module_cam_support, only: pcols, pver, endrun, pcnst =>pcnst_mp
      use modal_aero_data,   only: nspec_amode => nspec_amode_mp, &
           lmassptr_amode    => lmassptr_amode_mp , lmassptrcw_amode  => lmassptrcw_amode_mp, &
           lspectype_amode   => lspectype_amode_mp, specdens_amode    => specdens_amode_mp,   &
           spechygro         => spechygro_mp      , numptr_amode      => numptr_amode_mp,     &
           numptrcw_amode    => numptrcw_amode_mp , voltonumbhi_amode => voltonumbhi_amode_mp,&
           voltonumblo_amode => voltonumblo_amode_mp 

     

      implicit none

      

      type(qqcw_type), intent(in) :: QQCW(:)

      real(r8), intent(in) :: raer(pcols,pver,pcnst) 
       integer, intent(in) :: istart, istop 
       integer, intent(in) ::  m          
       integer, intent(in) ::  k          
       real(r8), intent(in) :: cs(pcols,pver)  
       integer, intent(in) :: phase 
       real(r8), intent(in) :: npv1 
       real(r8), intent(out) :: naerosol(pcols)                
       real(r8), intent(out) :: vaerosol(pcols)       
       real(r8), intent(out) :: hygro(pcols) 



       real(r8) vol(pcols) 
       integer i,lnum,lnumcw,l,ltype,lmass,lmasscw

          do i=istart,istop
             vaerosol(i)=0._r8
	     hygro(i)=0._r8
	  end do

          do l=1,nspec_amode(m)
             lmass=lmassptr_amode(l,m) 
             lmasscw=lmassptrcw_amode(l,m) 
             ltype=lspectype_amode(l,m)
	     if(phase.eq.3)then
	        do i=istart,istop

                   vol(i)=max(raer(i,k,lmass)+qqcw(lmasscw)%fldcw(i,k),0._r8)/specdens_amode(ltype)
		end do
             elseif(phase.eq.2)then
                do i=istart,istop
                   vol(i)=max(qqcw(lmasscw)%fldcw(i,k),0._r8)/specdens_amode(ltype)
                end do
             elseif(phase.eq.1)then
                do i=istart,istop
                   vol(i)=max(raer(i,k,lmass),0._r8)/specdens_amode(ltype)
		end do
             else
	        write(iulog,*)'phase=',phase,' in loadaer'

               call wrf_message(iulog)

	        call endrun('phase error in loadaer')
	     endif
	     do i=istart,istop
                vaerosol(i)=vaerosol(i)+vol(i)
                hygro(i)=hygro(i)+vol(i)*spechygro(ltype)
	     end do
          enddo
	  do i=istart,istop
             if (vaerosol(i) > 1.0e-30_r8) then   
               hygro(i)=hygro(i)/(vaerosol(i))
               vaerosol(i)=vaerosol(i)*cs(i,k)
             else
               hygro(i)=0.0_r8
               vaerosol(i)=0.0_r8
             endif
	  end do

          lnum=numptr_amode(m)
          lnumcw=numptrcw_amode(m)
          if(lnum.gt.0)then

             if(phase.eq.3)then
	        do i=istart,istop

                   naerosol(i)=(raer(i,k,lnum)+qqcw(lnumcw)%fldcw(i,k))*cs(i,k)
		end do
             elseif(phase.eq.2)then
                do i=istart,istop
                   naerosol(i)=qqcw(lnumcw)%fldcw(i,k)*cs(i,k)
                end do
	     else
	        do i=istart,istop
                   naerosol(i)=raer(i,k,lnum)*cs(i,k)
		end do
	     endif

	     do i=istart,istop
                naerosol(i) = max( naerosol(i), vaerosol(i)*voltonumbhi_amode(m) )
                naerosol(i) = min( naerosol(i), vaerosol(i)*voltonumblo_amode(m) )
	     end do
          else

	     do i=istart,istop
                naerosol(i)=vaerosol(i)*npv1
                naerosol(i)=max(naerosol(i),0._r8)
	     end do
          endif

       return
       end subroutine loadaer

      end module ndrop



