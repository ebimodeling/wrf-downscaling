
module module_fr_fire_phys

use module_model_constants, only: cp,xlv
use module_fr_fire_util

PRIVATE


PUBLIC:: init_fuel_cats,fire_ros,heat_fluxes,set_nfuel_cat,set_fire_params,write_fuels_m

PUBLIC::fire_params


type fire_params
real,pointer,dimension(:,:):: vx,vy                
real,pointer,dimension(:,:):: zsf                  
real,pointer,dimension(:,:):: dzdxf,dzdyf          
real,pointer,dimension(:,:):: bbb,betafl,phiwc,r_0 
real,pointer,dimension(:,:):: fgip                 
real,pointer,dimension(:,:):: ischap               
end type fire_params



























































   REAL, SAVE:: cmbcnst,hfgl,fuelmc_g,fuelmc_c

   REAL, SAVE:: bmst,fuelheat


   DATA cmbcnst  / 17.433e+06/             
   DATA hfgl     / 17.e4 /                
   DATA fuelmc_g / 0.08  /                
   DATA fuelmc_c / 1.00  /                







   INTEGER, PARAMETER :: nf=14              
   INTEGER, SAVE      :: nfuelcats = 13     
   INTEGER, PARAMETER :: mfuelcats = 30     
   INTEGER, PARAMETER :: zf = mfuelcats-nf  
   INTEGER, SAVE      :: no_fuel_cat = 14   
   CHARACTER (len=80), DIMENSION(mfuelcats ), save :: fuel_name
   INTEGER, DIMENSION( mfuelcats ), save :: ichap
   REAL   , DIMENSION( mfuelcats ), save :: windrf,weight,fgi,fci,fci_d,fct,fcbr, &
                                            fueldepthm,fueldens,fuelmce,   &
                                            savr,st,se
   DATA windrf /0.36, 0.36, 0.44,  0.55,  0.42,  0.44,  0.44, &     
                0.36, 0.36, 0.36,  0.36,  0.43,  0.46,  1e-7, zf*0 /
   DATA fgi / 0.166, 0.896, 0.674, 3.591, 0.784, 1.344, 1.091, &
              1.120, 0.780, 2.692, 2.582, 7.749, 13.024, 1.e-7, zf*0.  /
   DATA fueldepthm /0.305,  0.305,  0.762, 1.829, 0.61,  0.762,0.762, &
                    0.0610, 0.0610, 0.305, 0.305, 0.701, 0.914, 0.305,zf*0. /
   DATA savr / 3500., 2784., 1500., 1739., 1683., 1564., 1562.,  &
               1889., 2484., 1764., 1182., 1145., 1159., 3500., zf*0. /
   DATA fuelmce / 0.12, 0.15, 0.25, 0.20, 0.20, 0.25, 0.40,  &
                  0.30, 0.25, 0.25, 0.15, 0.20, 0.25, 0.12 , zf*0. / 
   DATA fueldens / nf * 32., zf*0. /   
   DATA st / nf* 0.0555 , zf*0./
   DATA se / nf* 0.010 , zf*0./



   DATA weight / 7.,  7.,  7., 180., 100., 100., 100.,  &
              900., 900., 900., 900., 900., 900., 7. , zf*0./ 

   DATA fci_d / 0., 0., 0., 1.123, 0., 0., 0.,  &
            1.121, 1.121, 1.121, 1.121, 1.121, 1.121, 0., zf*0./
   DATA fct / 60., 60., 60., 60., 60., 60., 60.,  &
            60., 120., 180., 180., 180., 180. , 60. , zf*0.   /
   DATA ichap / 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 , zf*0/ 


contains

subroutine init_fuel_cats
implicit none


logical, external:: wrf_dm_on_monitor


integer:: i,j,k,ii,iounit
character(len=128):: msg



namelist /fuel_scalars/ cmbcnst,hfgl,fuelmc_g,fuelmc_c,nfuelcats,no_fuel_cat
namelist /fuel_categories/ fuel_name,windrf,fgi,fueldepthm,savr, &
    fuelmce,fueldens,st,se,weight,fci_d,fct,ichap





IF ( wrf_dm_on_monitor() ) THEN
    
    iounit=open_text_file('namelist.fire','read')
    read(iounit,fuel_scalars)
    read(iounit,fuel_categories)
    CLOSE(iounit)
    
    if (nfuelcats>mfuelcats) then
        write(msg,*)'nfuelcats=',nfuelcats,' is too large, increase mfuelcats'
        call crash(msg)
    endif
        if (no_fuel_cat >= 1 .and. no_fuel_cat <= nfuelcats)then
        write(msg,*)'no_fuel_cat=',no_fuel_cat,' may not be between 1 and nfuelcats=',nfuelcats
        call crash(msg)
    endif
ENDIF


call wrf_dm_bcast_real(cmbcnst,1)
call wrf_dm_bcast_real(hfgl,1)
call wrf_dm_bcast_real(fuelmc_g,1)
call wrf_dm_bcast_real(fuelmc_c,1)
call wrf_dm_bcast_integer(nfuelcats,1)
call wrf_dm_bcast_integer(no_fuel_cat,1)
call wrf_dm_bcast_real(windrf,    nfuelcats)
call wrf_dm_bcast_real(fgi,       nfuelcats)
call wrf_dm_bcast_real(fueldepthm,nfuelcats)
call wrf_dm_bcast_real(savr,      nfuelcats)
call wrf_dm_bcast_real(fuelmce,   nfuelcats)
call wrf_dm_bcast_real(fueldens,  nfuelcats)
call wrf_dm_bcast_real(st,        nfuelcats)
call wrf_dm_bcast_real(se,        nfuelcats)
call wrf_dm_bcast_real(weight,    nfuelcats)
call wrf_dm_bcast_real(fci_d,     nfuelcats)
call wrf_dm_bcast_real(fct,       nfuelcats)
call wrf_dm_bcast_integer(ichap,  nfuelcats)



bmst     = fuelmc_g/(1+fuelmc_g)
fuelheat = cmbcnst * 4.30e-04     



DO i = 1,nfuelcats
    fci(i) = (1.+fuelmc_c)*fci_d(i)
    if(fct(i) .ne.  0.)then
        fcbr(i) = fci_d(i)/fct(i) 
    else
        fcbr(i) = 0
    endif
END DO



call message('**********************************************************')
call message('FUEL COEFFICIENTS')
write(msg,8)'cmbcnst    ',cmbcnst
call message(msg)
write(msg,8)'hfgl       ',hfgl
call message(msg)
write(msg,8)'fuelmc_g   ',fuelmc_g
call message(msg)
write(msg,8)'fuelmc_c   ',fuelmc_c
call message(msg)
write(msg,8)'bmst       ',bmst
call message(msg)
write(msg,8)'fuelheat   ',fuelheat
call message(msg)
write(msg,7)'nfuelcats  ',nfuelcats
call message(msg)
write(msg,7)'no_fuel_cat',no_fuel_cat
call message(msg)

j=1
7 format(a,5(1x,i8,4x))
8 format(a,5(1x,g12.5e2))
9 format(a,5(1x,a))
do i=1,nfuelcats,j
    k=min(i+j-1,nfuelcats)
    call message(' ')
    write(msg,7)'CATEGORY  ',(ii,ii=i,k)
    call message(msg)
    write(msg,9)'fuel name ',(fuel_name(ii),ii=i,k)
    call message(msg)


    write(msg,8)'fgi       ',(fgi(ii),ii=i,k)
    call message(msg)
    write(msg,8)'fueldepthm',(fueldepthm(ii),ii=i,k)
    call message(msg)
    write(msg,8)'savr      ',(savr(ii),ii=i,k)
    call message(msg)
    write(msg,8)'fuelmce   ',(fuelmce(ii),ii=i,k)
    call message(msg)
    write(msg,8)'fueldens  ',(fueldens(ii),ii=i,k)
    call message(msg)
    write(msg,8)'st        ',(st(ii),ii=i,k)
    call message(msg)
    write(msg,8)'se        ',(se(ii),ii=i,k)
    call message(msg)
    write(msg,8)'weight    ',(weight(ii),ii=i,k)
    call message(msg)
    write(msg,8)'fci_d     ',(fci_d(ii),ii=i,k)
    call message(msg)
    write(msg,8)'fct       ',(fct(ii),ii=i,k)
    call message(msg)
    write(msg,7)'ichap     ',(ichap(ii),ii=i,k)
    call message(msg)
    write(msg,8)'fci       ',(fci(ii),ii=i,k)
    call message(msg)
    write(msg,8)'fcbr      ',(fcbr(ii),ii=i,k)
    call message(msg)
enddo
call message('**********************************************************')


IF ( wrf_dm_on_monitor() ) THEN
  call write_fuels_m(61,30.,1.)
ENDIF
end subroutine init_fuel_cats


subroutine write_fuels_m(nsteps,maxwind,maxslope)
implicit none
integer, intent(in):: nsteps   
real, intent(in):: maxwind,maxslope 

integer:: iounit,k,j,i
type(fire_params)::fp








real, dimension(1:2,1:nsteps), target::vx,vy,zsf,dzdxf,dzdyf,bbb,betafl,phiwc,r_0,fgip,ischap
real, dimension(1:2,1:nsteps)::nfuel_cat,fuel_time,ros
real::ros_base,ros_wind,ros_slope,propx,propy,r

fp%vx=>vx
fp%vy=>vy
fp%dzdxf=>dzdxf
fp%dzdyf=>dzdyf
fp%bbb=>bbb
fp%betafl=>betafl
fp%phiwc=>phiwc
fp%r_0=>r_0
fp%fgip=>fgip
fp%ischap=>ischap

iounit = open_text_file('fuels.m','write')

10 format('fuel(',i3,').',a,'=',"'",a,"'",';% ',a)
do k=1,nfuelcats
    write(iounit,10)k,'fuel_name',trim(fuel_name(k)),'FUEL MODEL NAME'
    call write_var(k,'windrf',windrf(k),'WIND REDUCTION FACTOR FROM 20ft TO MIDFLAME HEIGHT' )
    call write_var(k,'fgi',fgi(k),'INITIAL TOTAL MASS OF SURFACE FUEL (KG/M**2)' )
    call write_var(k,'fueldepthm',fueldepthm(k),'FUEL DEPTH (M)')
    call write_var(k,'savr',savr(k),'FUEL PARTICLE SURFACE-AREA-TO-VOLUME RATIO, 1/FT')
    call write_var(k,'fuelmce',fuelmce(k),'MOISTURE CONTENT OF EXTINCTION')
    call write_var(k,'fueldens',fueldens(k),'OVENDRY PARTICLE DENSITY, LB/FT^3')
    call write_var(k,'st',st(k),'FUEL PARTICLE TOTAL MINERAL CONTENT')
    call write_var(k,'se',se(k),'FUEL PARTICLE EFFECTIVE MINERAL CONTENT')
    call write_var(k,'weight',weight(k),'WEIGHTING PARAMETER THAT DETERMINES THE SLOPE OF THE MASS LOSS CURVE')
    call write_var(k,'fci_d',fci_d(k),'INITIAL DRY MASS OF CANOPY FUEL')
    call write_var(k,'fct',fct(k),'BURN OUT TIME FOR CANOPY FUEL, AFTER DRY (S)')
    call write_var(k,'ichap',float(ichap(k)),'1 if chaparral, 0 if not')
    call write_var(k,'fci',fci(k),'INITIAL TOTAL MASS OF CANOPY FUEL')
    call write_var(k,'fcbr',fcbr(k),'FUEL CANOPY BURN RATE (KG/M**2/S)')
    call write_var(k,'hfgl',hfgl,'SURFACE FIRE HEAT FLUX THRESHOLD TO IGNITE CANOPY (W/m^2)')
    call write_var(k,'cmbcnst',cmbcnst,'JOULES PER KG OF DRY FUEL')
    call write_var(k,'fuelheat',fuelheat,'FUEL PARTICLE LOW HEAT CONTENT, BTU/LB')
    call write_var(k,'fuelmc_g',fuelmc_g,'FUEL PARTICLE (SURFACE) MOISTURE CONTENT')
    call write_var(k,'fuelmc_c',fuelmc_c,'FUEL PARTICLE (CANOPY) MOISTURE CONTENT')
    
    
    
    
    
    
    
    
    nfuel_cat = k
    call set_fire_params( &
                           1,2,1,nsteps, &
                           1,2,1,nsteps, &
                           1,2,1,nsteps, &
                           0.,0.,k,  &
                           nfuel_cat,fuel_time, &
                           fp ) 
    
    propx=1.
    propy=0.
    do j=1,nsteps
       r=float(j-1)/float(nsteps-1)
       
       vx(1,j)=maxwind*r
       vy(1,j)=0.
       dzdxf(1,j)=0.
       dzdyf(1,j)=0.
       
       vx(2,j)=0.
       vy(2,j)=0.
       dzdxf(2,j)=maxslope*r
       dzdyf(2,j)=0.
    enddo
    do j=1,nsteps
       do i=1,2
          call fire_ros(ros_base,ros_wind,ros_slope, &
             propx,propy,i,j,fp)
          ros(i,j)=ros_base+ros_wind+ros_slope
       enddo
       write(iounit,13)k,'wind',j,vx(1,j),'wind speed'
       write(iounit,13)k,'ros_wind',j,ros(1,j),'rate of spread for the wind speed'
       write(iounit,13)k,'slope',j,dzdxf(2,j),'slope'
       write(iounit,13)k,'ros_slope',j,ros(2,j),'rate of spread for the slope'
    enddo
enddo
13 format('fuel(',i3,').',a,'(',i3,')=',g12.5e2,';% ',a)
 
close(iounit)


contains

subroutine write_var(k,name,value,descr)

integer, intent(in)::k
character(len=*), intent(in)::name,descr
real, intent(in)::value
write(iounit,11)k,name,value
write(iounit,12)k,name,descr
11 format('fuel(',i3,').',a,'=',g12.5e2,  ';')
12 format('fuel(',i3,').',a,"_descr='",a,"';")
end subroutine write_var

end subroutine write_fuels_m





subroutine set_fire_params( &
                           ifds,ifde,jfds,jfde, &
                           ifms,ifme,jfms,jfme, &
                           ifts,ifte,jfts,jfte, &
                           fdx,fdy,nfuel_cat0,  &
                           nfuel_cat,fuel_time, &
                           fp ) 

implicit none




integer, intent(in)::ifds,ifde,jfds,jfde                        
integer, intent(in)::ifts,ifte,jfts,jfte                        
integer, intent(in)::ifms,ifme,jfms,jfme                        
real, intent(in):: fdx,fdy                                      
integer,intent(in)::nfuel_cat0                                  
real, intent(in),dimension(ifms:ifme, jfms:jfme)::nfuel_cat  
real, intent(out), dimension(ifms:ifme, jfms:jfme)::fuel_time   
type(fire_params),intent(inout)::fp



real::  fuelload, fueldepth, rtemp1, rtemp2, &
        qig, epsilon, rhob, wn, betaop, e, c, &
        xifr, etas, etam, a, gammax, gamma, ratio, ir, &
        fuelloadm,fdxinv,fdyinv
integer:: i,j,k
integer::nerr
character(len=128)::msg



nerr=0
do j=jfts,jfte
   do i=ifts,ifte
     
     k=int( nfuel_cat(i,j) )
     if(k.eq.no_fuel_cat)then   
        fp%fgip(i,j)=0.            
        fp%ischap(i,j)=0.
        fp%betafl(i,j)=0.          
        fp%bbb(i,j)=1.             
        fuel_time(i,j)=7./0.85  
        fp%phiwc(i,j)=0.
        fp%r_0(i,j)=0.             
     else
        if(k.eq.0.and.nfuel_cat0.ge.1.and.nfuel_cat0.le.nfuelcats)then
            
            k=nfuel_cat0
            nerr=nerr+1
        endif
   
        if(k.lt.1.or.k.gt.nfuelcats)then
!$OMP CRITICAL(FIRE_PHYS_CRIT)
            write(msg,'(3(a,i5))')'nfuel_cat(', i ,',',j,')=',k
!$OMP END CRITICAL(FIRE_PHYS_CRIT)
            call message(msg)
            call crash('set_fire_params: fuel category out of bounds')
        endif

        fuel_time(i,j)=weight(k)/0.85 
        
        
        
        

        fp%ischap(i,j)=ichap(k)
        fp%fgip(i,j)=fgi(k)

        
        
        
        fuelloadm= (1.-bmst) * fgi(k)  
        fuelload = fuelloadm * (.3048)**2 * 2.205    
        fueldepth = fueldepthm(k)/0.3048               
        fp%betafl(i,j) = fuelload/(fueldepth * fueldens(k))
        betaop = 3.348 * savr(k)**(-0.8189)     
        qig = 250. + 1116.*fuelmc_g            
        epsilon = exp(-138./savr(k) )    
        rhob = fuelload/fueldepth    

        c = 7.47 * exp( -0.133 * savr(k)**0.55)    
        fp%bbb(i,j) = 0.02526 * savr(k)**0.54      
        e = 0.715 * exp( -3.59e-4 * savr(k))       
        fp%phiwc(i,j) = c * (fp%betafl(i,j)/betaop)**(-e)

        rtemp2 = savr(k)**1.5
        gammax = rtemp2/(495. + 0.0594*rtemp2)              
        a = 1./(4.774 * savr(k)**0.1 - 7.27)   
        ratio = fp%betafl(i,j)/betaop
        gamma = gammax *(ratio**a) *exp(a*(1.-ratio)) 

        wn = fuelload/(1 + st(k))       
        rtemp1 = fuelmc_g/fuelmce(k)
        etam = 1.-2.59*rtemp1 +5.11*rtemp1**2 -3.52*rtemp1**3  
        etas = 0.174* se(k)**(-0.19)                
        ir = gamma * wn * fuelheat * etam * etas  
        

        xifr = exp( (0.792 + 0.681*savr(k)**0.5) &
            * (fp%betafl(i,j)+0.1)) /(192. + 0.2595*savr(k)) 


        fp%r_0(i,j) = ir*xifr/(rhob * epsilon *qig)    

     endif
  enddo
enddo

if(nerr.gt.1)then
!$OMP CRITICAL(FIRE_PHYS_CRIT)
    write(msg,'(a,i6,a)')'set_fire_params: WARNING: fuel category 0 replaced in',nerr,' cells'
!$OMP END CRITICAL(FIRE_PHYS_CRIT)
    call message(msg)
endif

end subroutine set_fire_params





subroutine heat_fluxes(dt,                        &
        ifms,ifme,jfms,jfme,                      &  
        ifts,ifte,jfts,jfte,                      &  
        iffs,iffe,jffs,jffe,                      &  
        fgip,fuel_frac_burnt,                     & 
        grnhft,grnqft)                              
implicit none





real, intent(in)::dt          
integer, intent(in)::ifts,ifte,jfts,jfte,ifms,ifme,jfms,jfme,iffs,iffe,jffs,jffe   
real, intent(in),dimension(ifms:ifme,jfms:jfme):: fgip
real, intent(in),dimension(iffs:iffe,jffs:jffe):: fuel_frac_burnt
real, intent(out),dimension(ifms:ifme,jfms:jfme):: grnhft,grnqft


integer::i,j
real:: dmass


do j=jfts,jfte
    do i=ifts,ifte
         dmass =                     &     
             fgip(i,j)               &     
             * fuel_frac_burnt(i,j)        
         grnhft(i,j) = (dmass/dt)*(1.-bmst)*cmbcnst         
         grnqft(i,j) = (bmst+(1.-bmst)*.56)*(dmass/dt)*xlv  
         
    enddo
enddo

end subroutine heat_fluxes






subroutine set_nfuel_cat(   &
    ifms,ifme,jfms,jfme,               &
    ifts,ifte,jfts,jfte,               &
    ifuelread,nfuel_cat0,zsf,nfuel_cat)

implicit none


integer, intent(in)::   ifts,ifte,jfts,jfte,               &
                        ifms,ifme,jfms,jfme               

integer, intent(in)::ifuelread,nfuel_cat0
real, intent(in), dimension(ifms:ifme, jfms:jfme)::zsf
real, intent(out), dimension(ifms:ifme, jfms:jfme)::nfuel_cat




integer:: i,j,iu1
real:: t1
character(len=128)msg

!$OMP CRITICAL(FIRE_PHYS_CRIT)
    write(msg,'(a,i3)')'set_nfuel_cat: ifuelread=',ifuelread 
!$OMP END CRITICAL(FIRE_PHYS_CRIT)
    call message(msg)

if (ifuelread .eq. -1 .or. ifuelread .eq. 2) then
!$OMP CRITICAL(FIRE_PHYS_CRIT)
    call message('set_nfuel_cat: assuming nfuel_cat initialized already') 
    call message(msg)
!$OMP END CRITICAL(FIRE_PHYS_CRIT)
else if (ifuelread .eq. 0) then

    do j=jfts,jfte
        do  i=ifts,ifte
            nfuel_cat(i,j)=real(nfuel_cat0)
        enddo
    enddo
!$OMP CRITICAL(FIRE_PHYS_CRIT)
    write(msg,'(a,i3)')'set_nfuel_cat: fuel initialized with category',nfuel_cat0
!$OMP END CRITICAL(FIRE_PHYS_CRIT)
    call message(msg)
         
else if (ifuelread .eq. 1) then





    do j=jfts,jfte
        do  i=ifts,ifte
            
            
            t1 = zsf(i,j)  
            if(t1.le.1524.)then   
                nfuel_cat(i,j)= 3  
            else if(t1.ge.1524. .and. t1.le.2073.)then  
                nfuel_cat(i,j)= 2  
            else if(t1.ge.2073..and.t1.le.2438.)then  
                nfuel_cat(i,j)= 8  
            else if(t1.gt.2438. .and. t1.le. 3354.) then 

                nfuel_cat(i,j)= 10 
            else if(t1.gt.3354. .and. t1.le. 3658.) then 
                nfuel_cat(i,j)= 1  
            else if(t1.gt.3658. ) then  
                nfuel_cat(i,j)= 14 
            endif
        enddo
    enddo

    call message('set_nfuel_cat: fuel initialized by altitude')
else

    call crash('set_nfuel_cat: bad ifuelread')
endif


end subroutine set_nfuel_cat            





subroutine fire_ros(ros_base,ros_wind,ros_slope, &
propx,propy,i,j,fp)

implicit none




















real, intent(out)::ros_base,ros_wind,ros_slope 
real, intent(in)::propx,propy
integer, intent(in)::i,j         
type(fire_params),intent(in)::fp


real:: speed, tanphi 
real:: umid, phis, phiw, spdms, umidm, excess
real:: ros_back
integer, parameter::ibeh=1
real, parameter::ros_max=6.
character(len=128)msg
real::cor_wind,cor_slope,nvx,nvy,scale






scale=1.
nvx=propx/scale
nvy=propy/scale
if (fire_advection.ne.0) then 
    
    speed =  sqrt(fp%vx(i,j)*fp%vx(i,j)+ fp%vy(i,j)*fp%vy(i,j))+tiny(speed)
    
    tanphi = sqrt(fp%dzdxf(i,j)*fp%dzdxf(i,j) + fp%dzdyf(i,j)*fp%dzdyf(i,j))+tiny(tanphi)
    
    cor_wind =  max(0.,(fp%vx(i,j)*nvx + fp%vy(i,j)*nvy)/speed)
    
    cor_slope = max(0., (fp%dzdxf(i,j)*nvx + fp%dzdyf(i,j)*nvy)/tanphi)
else
    
    speed =  fp%vx(i,j)*nvx + fp%vy(i,j)*nvy
    
    tanphi = fp%dzdxf(i,j)*nvx + fp%dzdyf(i,j)*nvy
    cor_wind=1.
    cor_slope=1.
endif

if (.not. fp%ischap(i,j) > 0.) then      
    if (ibeh .eq. 1) then                

        spdms = max(speed,0.)            
        umidm = min(spdms,30.)           
        umid = umidm * 196.850           
        
        phiw = umid**fp%bbb(i,j) * fp%phiwc(i,j) 
        phis=0.
        if (tanphi .gt. 0.) then
            phis = 5.275 *(fp%betafl(i,j))**(-0.3) *tanphi**2   
        endif
        
        ros_base = fp%r_0(i,j) * .00508
        ros_wind = ros_base*phiw
        ros_slope= ros_base*phis
        

    else                                   
        
        ros_base = 0.18*exp(0.8424)
        ros_wind = 0.18*exp(0.8424*max(speed,0.))
        ros_slope =0.
    endif

else   

    spdms = max(speed,0.)      
    
    
    
    
    

    ros_back=.03333    
    ros_wind = 1.2974 * spdms**1.41       
    ros_wind = max(ros_wind, ros_back)
    ros_slope =0.

endif

ros_wind=ros_wind*cor_wind
ros_slope=ros_slope*cor_slope




excess = ros_base + ros_wind + ros_slope - ros_max

if (excess > 0.)then
    
    ros_wind = ros_wind - excess*ros_wind/(ros_wind+ros_slope)
    ros_slope = ros_slope - excess*ros_slope/(ros_wind+ros_slope)
endif



      return

contains
real function nrm2(u,v)
real, intent(in)::u,v
nrm2=sqrt(u*u+v*v)
end function nrm2

end subroutine fire_ros 

end module module_fr_fire_phys
