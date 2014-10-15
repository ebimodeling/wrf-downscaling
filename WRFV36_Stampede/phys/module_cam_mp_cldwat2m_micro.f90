


module cldwat2m_micro

















  use shr_kind_mod,  only: r8=>shr_kind_r8




  use module_cam_support, only: masterproc, pcols, pver, pverp

  use physconst,     only: gravit, rair, tmelt, cpair, rh2o, rhoh2o
  use physconst,     only: latvap, latice
  use wv_saturation,  only: polysvp, epsqs 







  use module_cam_support, only: addfld, add_default, phys_decomp, outfld, &
       fillvalue, iulog


  implicit none
  private
  save


  
  real(r8), public,  parameter :: rhmini       = 0.80_r8       

  logical, public :: liu_in = .true.   
                                       

   public :: ini_micro, mmicro_pcond,gamma


  real(r8), private::  g              
  real(r8), private::  r              
  real(r8), private::  rv             
  real(r8), private::  cpp            
  real(r8), private::  rhow           
  real(r8), private::  xxlv           
  real(r8), private::  xlf            
  real(r8), private::  xxls           

real(r8), private:: rhosn  
real(r8), private:: rhoi   

real(r8), private:: ac,bc,as,bs,ai,bi,ar,br  
real(r8), private:: ci,di    
real(r8), private:: cs,ds    
real(r8), private:: cr,dr    
real(r8), private:: f1s,f2s  
real(r8), private:: Eii      
real(r8), private:: Ecc      
real(r8), private:: Ecr      
real(r8), private:: f1r,f2r  
real(r8), private:: DCS      
real(r8), private:: qsmall   
real(r8), private:: bimm,aimm 
real(r8), private:: rhosu     
real(r8), private:: mi0       
real(r8), private:: rin       
real(r8), private:: qcvar     
real(r8), private:: pi       



real(r8), private:: cons1
real(r8), private:: cons2
real(r8), private:: cons3
real(r8), private:: cons4
real(r8), private:: cons5
real(r8), private:: cons6
real(r8), private:: cons7
real(r8), private:: cons8
real(r8), private:: cons9
real(r8), private:: cons10
real(r8), private:: cons11
real(r8), private:: cons12
real(r8), private:: cons13
real(r8), private:: cons14
real(r8), private:: cons15
real(r8), private:: cons16
real(r8), private:: cons17
real(r8), private:: cons18
real(r8), private:: cons19
real(r8), private:: cons20
real(r8), private:: cons21
real(r8), private:: cons22
real(r8), private:: cons23
real(r8), private:: cons24
real(r8), private:: cons25
real(r8), private:: cons27
real(r8), private:: cons28

real(r8), private:: lammini
real(r8), private:: lammaxi
real(r8), private:: lamminr
real(r8), private:: lammaxr
real(r8), private:: lammins
real(r8), private:: lammaxs


real(r8), private:: tmax_fsnow 
real(r8), private:: tmin_fsnow 


real(r8), private:: tt0       

real(r8), private:: csmin,csmax,minrefl,mindbz

contains



subroutine ini_micro











   integer k

   integer l,m, iaer
   real(r8) surften       
   real(r8) arg

   character(len=16) :: eddy_scheme = ' '
   logical           :: history_microphysics     





   history_microphysics = .FALSE.

   
   call addfld ('QRAIN   ','kg/kg   ',pver, 'A','Diagnostic grid-mean rain mixing ratio'         ,phys_decomp)	
   call addfld ('QSNOW   ','kg/kg   ',pver, 'A','Diagnostic grid-mean snow mixing ratio'         ,phys_decomp)	
   call addfld ('NRAIN   ','m-3     ',pver, 'A','Diagnostic grid-mean rain number conc'         ,phys_decomp)	
   call addfld ('NSNOW   ','m-3     ',pver, 'A','Diagnostic grid-mean snow number conc'         ,phys_decomp)	

   
   call addfld ('RERCLD   ','m      ',pver, 'A','Diagnostic effective radius of Liquid Cloud and Rain' ,phys_decomp)

   call addfld ('DSNOW   ','m       ',pver, 'A','Diagnostic grid-mean snow diameter'         ,phys_decomp)	

   
   call addfld ('REFL  ','DBz  ',pver, 'A','94 GHz radar reflectivity'       ,phys_decomp)
   call addfld ('AREFL  ','DBz  ',pver, 'A','Average 94 GHz radar reflectivity'       ,phys_decomp)
   call addfld ('FREFL  ','fraction  ',pver, 'A','Fractional occurance of radar reflectivity'       ,phys_decomp)
   
   call addfld ('CSRFL  ','DBz  ',pver, 'A','94 GHz radar reflectivity (CloudSat thresholds)'       ,phys_decomp)
   call addfld ('ACSRFL  ','DBz  ',pver, 'A','Average 94 GHz radar reflectivity (CloudSat thresholds)'       ,phys_decomp)
   call addfld ('FCSRFL  ','fraction  ',pver, 'A','Fractional occurance of radar reflectivity (CloudSat thresholds)' &
	,phys_decomp)
 
   call addfld ('AREFLZ ','mm^6/m^3 ',pver, 'A','Average 94 GHz radar reflectivity'       ,phys_decomp)

   
    call addfld ('NCAL    ','#/m3   ',pver, 'A','Number Concentation Activated for Liquid',phys_decomp)
    call addfld ('NCAI    ','#/m3   ',pver, 'A','Number Concentation Activated for Ice',phys_decomp)


   
   call addfld ('AQRAIN   ','kg/kg   ',pver, 'A','Average rain mixing ratio'         ,phys_decomp)	
   call addfld ('AQSNOW   ','kg/kg   ',pver, 'A','Average snow mixing ratio'         ,phys_decomp)	
   call addfld ('ANRAIN   ','m-3     ',pver, 'A','Average rain number conc'         ,phys_decomp)	
   call addfld ('ANSNOW   ','m-3     ',pver, 'A','Average snow number conc'         ,phys_decomp)
   call addfld ('ADRAIN   ','Micron  ',pver, 'A','Average rain effective Diameter'         ,phys_decomp)	
   call addfld ('ADSNOW   ','Micron  ',pver, 'A','Average snow effective Diameter'         ,phys_decomp)
   call addfld ('FREQR  ','fraction  ',pver, 'A','Fractional occurance of rain'       ,phys_decomp)
   call addfld ('FREQS  ','fraction  ',pver, 'A','Fractional occurance of snow'       ,phys_decomp)

   if ( history_microphysics) then
      call add_default ('AQSNOW   ', 1, ' ')
      call add_default ('FREQR    ', 1, ' ')
      call add_default ('FREQS    ', 1, ' ')
      call add_default ('AQRAIN   ', 1, ' ')
      call add_default ('AQSNOW   ', 1, ' ')
      call add_default ('ANRAIN   ', 1, ' ')
      call add_default ('ANSNOW   ', 1, ' ')
  end if



   g= gravit                  
   r= rair       	      
   rv= rh2o                   
   cpp = cpair                 
   rhow = rhoh2o              



   xxlv = latvap         
   xlf = latice          
   xxls = xxlv + xlf     



   tmax_fsnow = tmelt
   tmin_fsnow = tmelt-5._r8




      rhosn = 250._r8    
      rhoi = 500._r8     
      rhow = 1000._r8    






	ac = 3.e7_r8
	bc = 2._r8


	as = 11.72_r8
	bs = 0.41_r8


	ai = 700._r8
	bi = 1._r8


	ar = 841.99667_r8
	br = 0.8_r8





        pi= 3.1415927_r8



	ci = rhoi*pi/6._r8
	di = 3._r8



	cs = rhosn*pi/6._r8
	ds = 3._r8



	cr = rhow*pi/6._r8
	dr = 3._r8




	f1s = 0.86_r8
	f2s = 0.28_r8



	Eii = 0.1_r8



	Ecr = 1.0_r8



	f1r = 0.78_r8
	f2r = 0.32_r8



       Dcs = 400.e-6_r8



	qsmall = 1.e-18_r8  



	bimm = 100._r8
	aimm = 0.66_r8



	rhosu = 85000._r8/(rair * tmelt)



	mi0 = 4._r8/3._r8*pi*rhoi*(10.e-6_r8)*(10.e-6_r8)*(10.e-6_r8)



        rin = 0.1e-6_r8




	qcvar = 2._r8


	tt0=273.15_r8

      pi=4._r8*atan(1.0_r8)

      
      csmin= -30._r8
      csmax= 26._r8
      mindbz = -99._r8
      
      minrefl = 1.26e-10_r8



	cons1=gamma(1._r8+di)
	cons2=gamma(qcvar+2.47_r8)
	cons3=gamma(qcvar)
	cons4=gamma(1._r8+br)
	cons5=gamma(4._r8+br)
	cons6=gamma(1._r8+ds)
	cons7=gamma(1._r8+bs)     
	cons8=gamma(4._r8+bs)     
	cons9=gamma(qcvar+2._r8)     
	cons10=gamma(qcvar+1._r8)
	cons11=gamma(3._r8+bs)
	cons12=gamma(qcvar+1.15_r8)
	cons13=gamma(5._r8/2._r8+br/2._r8)
	cons14=gamma(5._r8/2._r8+bs/2._r8)
	cons15=gamma(qcvar+bc/3._r8)
	cons16=gamma(1._r8+bi)
	cons17=gamma(4._r8+bi)
	cons18=qcvar**2.47_r8
	cons19=qcvar**2
	cons20=qcvar**1.15_r8
	cons21=qcvar**(bc/3._r8)
	cons22=(4._r8/3._r8*pi*rhow*(25.e-6_r8)**3)	
	cons23=dcs**3
	cons24=dcs**2
	cons25=dcs**bs
	cons27=xxlv**2
	cons28=xxls**2

        lammaxi = 1._r8/10.e-6_r8
        lammini = 1._r8/(2._r8*dcs)
	lammaxr = 1._r8/20.e-6_r8
	lamminr = 1._r8/500.e-6_r8
	lammaxs = 1._r8/10.e-6_r8
	lammins = 1._r8/2000.e-6_r8

   return
 end subroutine ini_micro




subroutine mmicro_pcond ( sub_column,       &
   lchnk, ncol, deltatin, tn,               &
   qn, qc, qi,                              &
   nc, ni, p, pdel, cldn,                   &
   liqcldf, icecldf,                        &
   cldo,                                    &
   rate1ord_cw2pr_st,                       &   
   naai, npccnin, rndst,nacon,              &
   tlat, qvlat,        &
   qctend, qitend, nctend, nitend, effc,    &
   effc_fn, effi, prect, preci,             &  
   nevapr, evapsnow,      &
   prain, prodsnow, cmeout, deffi, pgamrad, &
   lamcrad,qsout,dsout, &
         rflx,sflx, qrout,reff_rain,reff_snow,  &
   qcsevap,qisevap,qvres,cmeiout, &
   vtrmc,vtrmi,qcsedten,qisedten, &
   prao,prco,mnuccco,mnuccto,msacwio,psacwso,&
   bergso,bergo,melto,homoo,qcreso,prcio,praio,qireso,&
   mnuccro,pracso,meltsdt,frzrdt,mnuccdo    &

   , nsout, nrout                           &

                                            )




   use wv_saturation, only: vqsatd_water

   
   logical,  intent(in) :: sub_column  
   integer,  intent(in) :: lchnk
   integer,  intent(in) :: ncol
   real(r8), intent(in) :: deltatin             
   real(r8), intent(in) :: tn(pcols,pver)       
   real(r8), intent(in) :: qn(pcols,pver)       

   
   real(r8), intent(inout) :: qc(pcols,pver)    
   real(r8), intent(inout) :: qi(pcols,pver)    
   real(r8), intent(inout) :: nc(pcols,pver)    
   real(r8), intent(inout) :: ni(pcols,pver)    
   real(r8), intent(in) :: p(pcols,pver)        
   real(r8), intent(in) :: pdel(pcols,pver)     
   real(r8), intent(in) :: cldn(pcols,pver)     
   real(r8), intent(in) :: icecldf(pcols,pver)  
   real(r8), intent(in) :: liqcldf(pcols,pver)  
   real(r8), intent(inout) :: cldo(pcols,pver)  

   real(r8), intent(out) :: rate1ord_cw2pr_st(pcols,pver) 
                                                          

   real(r8), intent(in) :: naai(pcols,pver)      
   real(r8), intent(in) :: npccnin(pcols,pver)   
   real(r8), intent(in) :: rndst(pcols,pver,4)   
   real(r8), intent(in) :: nacon(pcols,pver,4)   

   

   real(r8), intent(out) :: tlat(pcols,pver)    
   real(r8), intent(out) :: qvlat(pcols,pver)   
   real(r8), intent(out) :: qctend(pcols,pver)  
   real(r8), intent(out) :: qitend(pcols,pver)  
   real(r8), intent(out) :: nctend(pcols,pver)  
   real(r8), intent(out) :: nitend(pcols,pver)  
   real(r8), intent(out) :: effc(pcols,pver)    
   real(r8), intent(out) :: effc_fn(pcols,pver) 
   real(r8), intent(out) :: effi(pcols,pver)    
   real(r8), intent(out) :: prect(pcols)        
   real(r8), intent(out) :: preci(pcols)        
   real(r8), intent(out) :: nevapr(pcols,pver)  
   real(r8), intent(out) :: evapsnow(pcols,pver)
   real(r8), intent(out) :: prain(pcols,pver)   
   real(r8), intent(out) :: prodsnow(pcols,pver)
   real(r8), intent(out) :: cmeout(pcols,pver)  
   real(r8), intent(out) :: deffi(pcols,pver)   
   real(r8), intent(out) :: pgamrad(pcols,pver) 
   real(r8), intent(out) :: lamcrad(pcols,pver) 
   real(r8), intent(out) :: rflx(pcols,pver+1)  
   real(r8), intent(out) :: sflx(pcols,pver+1)  
   real(r8), intent(out) :: qcsevap(pcols,pver) 
   real(r8), intent(out) :: qisevap(pcols,pver) 
   real(r8), intent(out) :: qvres(pcols,pver) 
   real(r8), intent(out) :: cmeiout(pcols,pver) 
   real(r8), intent(out) :: vtrmc(pcols,pver) 
   real(r8), intent(out) :: vtrmi(pcols,pver) 
   real(r8), intent(out) :: qcsedten(pcols,pver) 
   real(r8), intent(out) :: qisedten(pcols,pver) 

   real(r8), intent(out) :: prao(pcols,pver) 
   real(r8), intent(out) :: prco(pcols,pver) 
   real(r8), intent(out) :: mnuccco(pcols,pver) 
   real(r8), intent(out) :: mnuccto(pcols,pver) 
   real(r8), intent(out) :: msacwio(pcols,pver) 
   real(r8), intent(out) :: psacwso(pcols,pver) 
   real(r8), intent(out) :: bergso(pcols,pver) 
   real(r8), intent(out) :: bergo(pcols,pver) 
   real(r8), intent(out) :: melto(pcols,pver) 
   real(r8), intent(out) :: homoo(pcols,pver) 
   real(r8), intent(out) :: qcreso(pcols,pver) 
   real(r8), intent(out) :: prcio(pcols,pver) 
   real(r8), intent(out) :: praio(pcols,pver) 
   real(r8), intent(out) :: qireso(pcols,pver) 
   real(r8), intent(out) :: mnuccro(pcols,pver) 
   real(r8), intent(out) :: pracso (pcols,pver) 
   real(r8), intent(out) :: meltsdt(pcols,pver) 
   real(r8), intent(out) :: frzrdt (pcols,pver) 
   real(r8), intent(out) :: mnuccdo(pcols,pver) 





	real(r8) :: t1(pcols,pver)
	real(r8) :: q1(pcols,pver)
	real(r8) :: qc1(pcols,pver)
   	real(r8) :: qi1(pcols,pver)
   	real(r8) :: nc1(pcols,pver)
   	real(r8) :: ni1(pcols,pver)
  	real(r8) :: tlat1(pcols,pver)
   	real(r8) :: qvlat1(pcols,pver)
   	real(r8) :: qctend1(pcols,pver)
   	real(r8) :: qitend1(pcols,pver)
   	real(r8) :: nctend1(pcols,pver)
   	real(r8) :: nitend1(pcols,pver)
   	real(r8) :: prect1(pcols)
   	real(r8) :: preci1(pcols)


   	real(r8) :: deltat        
        real(r8) :: omsm    
        real(r8) :: dto2    
        real(r8) :: mincld  
        real(r8) :: q(pcols,pver) 
        real(r8) :: t(pcols,pver) 
        real(r8) :: rho(pcols,pver) 
        real(r8) :: dv(pcols,pver)  
        real(r8) :: mu(pcols,pver)  
        real(r8) :: sc(pcols,pver)  
        real(r8) :: kap(pcols,pver) 
        real(r8) :: rhof(pcols,pver) 
        real(r8) :: cldmax(pcols,pver) 
        real(r8) :: cldm(pcols,pver)   
        real(r8) :: icldm(pcols,pver)   
        real(r8) :: lcldm(pcols,pver)   
        real(r8) :: icwc(pcols)    
        real(r8) :: calpha(pcols)  
        real(r8) :: cbeta(pcols) 
        real(r8) :: cbetah(pcols) 
        real(r8) :: cgamma(pcols) 
        real(r8) :: cgamah(pcols) 
        real(r8) :: rcgama(pcols) 
        real(r8) :: cmec1(pcols) 
        real(r8) :: cmec2(pcols) 
        real(r8) :: cmec3(pcols) 
        real(r8) :: cmec4(pcols) 
        real(r8) :: qtmp 
        real(r8) :: dum  

        real(r8) :: cme(pcols,pver)  

        real(r8) :: cmei(pcols,pver) 
        real(r8) :: cwml(pcols,pver) 
        real(r8) :: cwmi(pcols,pver) 
        real(r8) :: nnuccd(pver)   
        real(r8) :: mnuccd(pver)   
        real(r8) :: qcld              
        real(r8) :: lcldn(pcols,pver) 
        real(r8) :: lcldo(pcols,pver) 
	real(r8) :: nctend_mixnuc(pcols,pver)
	real(r8) :: arg 

        
        real(r8) :: qcsinksum_rate1ord(pver)   
        real(r8) :: qcsum_rate1ord(pver)    

        real(r8) :: alpha

        real(r8) :: dum1,dum2   

        real(r8) :: npccn(pver)     
        real(r8) :: qcic(pcols,pver) 
        real(r8) :: qiic(pcols,pver) 
        real(r8) :: qniic(pcols,pver) 
        real(r8) :: qric(pcols,pver) 
        real(r8) :: ncic(pcols,pver) 
        real(r8) :: niic(pcols,pver) 
        real(r8) :: nsic(pcols,pver) 
        real(r8) :: nric(pcols,pver) 
        real(r8) :: lami(pver) 
        real(r8) :: n0i(pver) 
        real(r8) :: lamc(pver) 
        real(r8) :: n0c(pver) 
        real(r8) :: lams(pver) 
        real(r8) :: n0s(pver) 
        real(r8) :: lamr(pver) 
        real(r8) :: n0r(pver) 
        real(r8) :: cdist1(pver) 

        real(r8) :: rercld(pcols,pver) 
        real(r8) :: arcld(pcols,pver) 
        real(r8) :: Actmp  
        real(r8) :: Artmp  

        real(r8) :: pgam(pver) 
        real(r8) :: lammax  
        real(r8) :: lammin  
        real(r8) :: nacnt   
        real(r8) :: mnuccc(pver) 
        real(r8) :: nnuccc(pver) 

        real(r8) :: mnucct(pver) 
        real(r8) :: nnucct(pver) 
        real(r8) :: msacwi(pver) 
        real(r8) :: nsacwi(pver) 

        real(r8) :: prc(pver) 
        real(r8) :: nprc(pver) 
        real(r8) :: nprc1(pver) 
        real(r8) :: nsagg(pver) 
        real(r8) :: dc0  
        real(r8) :: ds0  
        real(r8) :: eci  
        real(r8) :: psacws(pver) 
        real(r8) :: npsacws(pver) 
        real(r8) :: uni 
        real(r8) :: umi 
        real(r8) :: uns(pver) 
        real(r8) :: ums(pver) 
        real(r8) :: unr(pver) 
        real(r8) :: umr(pver) 
        real(r8) :: unc 
        real(r8) :: umc 
        real(r8) :: pracs(pver) 
        real(r8) :: npracs(pver) 
        real(r8) :: mnuccr(pver) 
        real(r8) :: nnuccr(pver) 
        real(r8) :: pra(pver) 
        real(r8) :: npra(pver) 
        real(r8) :: nragg(pver) 
        real(r8) :: prci(pver) 
        real(r8) :: nprci(pver) 
        real(r8) :: prai(pver) 
        real(r8) :: nprai(pver) 
        real(r8) :: qvs 
        real(r8) :: qvi 
        real(r8) :: dqsdt 
        real(r8) :: dqsidt 
        real(r8) :: ab 
        real(r8) :: qclr 
        real(r8) :: abi 
        real(r8) :: epss 
        real(r8) :: epsr 
        real(r8) :: pre(pver) 
        real(r8) :: prds(pver) 
        real(r8) :: qce 
        real(r8) :: qie 
        real(r8) :: nce 
        real(r8) :: nie 
        real(r8) :: ratio 
        real(r8) :: dumc(pcols,pver) 
        real(r8) :: dumnc(pcols,pver) 
        real(r8) :: dumi(pcols,pver) 
        real(r8) :: dumni(pcols,pver) 
        real(r8) :: dums(pcols,pver) 
        real(r8) :: dumns(pcols,pver) 
        real(r8) :: dumr(pcols,pver) 
        real(r8) :: dumnr(pcols,pver) 

        real(r8) :: fr(pver)
        real(r8) :: fnr(pver)
        real(r8) :: fc(pver)
        real(r8) :: fnc(pver)
        real(r8) :: fi(pver)
        real(r8) :: fni(pver)
        real(r8) :: fs(pver)
        real(r8) :: fns(pver)
        real(r8) :: faloutr(pver)
        real(r8) :: faloutnr(pver)
        real(r8) :: faloutc(pver)
        real(r8) :: faloutnc(pver)
        real(r8) :: falouti(pver)
        real(r8) :: faloutni(pver)
        real(r8) :: falouts(pver)
        real(r8) :: faloutns(pver)
        real(r8) :: faltndr
        real(r8) :: faltndnr
        real(r8) :: faltndc
        real(r8) :: faltndnc
        real(r8) :: faltndi
        real(r8) :: faltndni
        real(r8) :: faltnds
        real(r8) :: faltndns
        real(r8) :: faltndqie
        real(r8) :: faltndqce

        real(r8) :: relhum(pcols,pver) 
        real(r8) :: csigma(pcols) 
        real(r8) :: rgvm 
        real(r8) :: arn(pcols,pver) 
        real(r8) :: asn(pcols,pver) 
        real(r8) :: acn(pcols,pver) 
        real(r8) :: ain(pcols,pver) 
        real(r8) :: nsubi(pver) 
        real(r8) :: nsubc(pver) 
        real(r8) :: nsubs(pver) 
        real(r8) :: nsubr(pver) 
	real(r8) :: mtime 
	real(r8) :: dz(pcols,pver) 


	real(r8) :: nfice(pcols,pver)

	
	real(r8) :: rflx1(pcols,pver+1)
        real(r8) :: sflx1(pcols,pver+1)


        real(r8) :: tsp(pcols,pver)      
        real(r8) :: qsp(pcols,pver)      
        real(r8) :: qsphy(pcols,pver)      
        real(r8) :: qs(pcols)            
        real(r8) :: es(pcols)            
        real(r8) :: esl(pcols,pver)      
        real(r8) :: esi(pcols,pver)      
        real(r8) :: gammas(pcols)        



        real(r8) :: qnitend(pcols,pver) 
        real(r8) :: nstend(pcols,pver)  
        real(r8) :: qrtend(pcols,pver) 
        real(r8) :: nrtend(pcols,pver)  
	real(r8) :: qrtot 
	real(r8) :: nrtot 
	real(r8) :: qstot 
	real(r8) :: nstot 



	real(r8) :: dumnnuc 
	real(r8) :: ninew  
	real(r8) :: qinew 
	real(r8) :: qvl  
	real(r8) :: epsi 
	real(r8) :: prd 
	real(r8) :: berg(pcols,pver) 
	real(r8) :: bergs(pver) 


        real(r8) :: bergtsf   
        real(r8) :: rhin      




        real(r8), intent(out) :: qrout(pcols,pver)     
	real(r8), intent(out) :: reff_rain(pcols,pver) 
	real(r8), intent(out) :: reff_snow(pcols,pver) 
	real(r8) 	      :: drout(pcols,pver)     




        
        real(r8), intent(out) :: nrout(pcols,pver) 
	real(r8), intent(out) :: nsout(pcols,pver) 

	real(r8), intent(out) :: dsout(pcols,pver) 
	real(r8), intent(out) :: qsout(pcols,pver) 


	real(r8) :: qrout2(pcols,pver)
	real(r8) :: qsout2(pcols,pver)
	real(r8) :: nrout2(pcols,pver)
	real(r8) :: nsout2(pcols,pver)
	real(r8) :: freqs(pcols,pver)
	real(r8) :: freqr(pcols,pver)
	real(r8) :: dumfice
	real(r8) :: drout2(pcols,pver) 
	real(r8) :: dsout2(pcols,pver) 


        real(r8) :: dum2i(pcols,pver) 
        real(r8) :: dum2l(pcols,pver) 
	real(r8) :: ncmax
	real(r8) :: nimax


        real(r8) :: ncai(pcols,pver) 
        real(r8) :: ncal(pcols,pver) 


         integer i,k,nstep,n, l
	 integer ii,kk, m


	 integer iter,it,ltrue(pcols)


        real(r8)  tcnt, viscosity, mfp
        real(r8)  slip1, slip2, slip3, slip4


        real(r8)  ndfaer1, ndfaer2, ndfaer3, ndfaer4
        real(r8)  nslip1, nslip2, nslip3, nslip4


        real(r8)  bbi, cci, ak, iciwc, rvi


        real(r8)  Tk, deles, Aprpr, Bprpr, Cice, qi0, Crate, qidep


        real(r8)  cldmw(pcols,pver)


        real(r8) ni_secp



	real(r8) :: esn
	real(r8) :: qsn
	real(r8) :: ttmp


        real(r8) :: refl(pcols,pver)    
        real(r8) :: rainrt(pcols,pver)  
        real(r8) :: rainrt1(pcols,pver)
 	real(r8) :: csrfl(pcols,pver)	
        real(r8) :: arefl(pcols,pver)  
  	real(r8) :: acsrfl(pcols,pver) 
 	real(r8) :: frefl(pcols,pver)
  	real(r8) :: fcsrfl(pcols,pver)
 	real(r8) :: areflz(pcols,pver)  
	real(r8) :: tmp

        real(r8) dmc,ssmc,dstrn  
  
      real(r8), parameter :: cdnl    = 0.e6_r8    




    ncai(1:ncol,1:pver)=0._r8 
    ncal(1:ncol,1:pver)=0._r8  


    rercld(1:ncol,1:pver)=0._r8
    arcld(1:ncol,1:pver)=0._r8


        pgamrad(1:ncol,1:pver)=0._r8 
        lamcrad(1:ncol,1:pver)=0._r8 
        deffi  (1:ncol,1:pver)=0._r8 


        qcsevap(1:ncol,1:pver)=0._r8 
        qisevap(1:ncol,1:pver)=0._r8 
        qvres  (1:ncol,1:pver)=0._r8 
        cmeiout (1:ncol,1:pver)=0._r8
        vtrmc (1:ncol,1:pver)=0._r8
        vtrmi (1:ncol,1:pver)=0._r8
        qcsedten (1:ncol,1:pver)=0._r8
        qisedten (1:ncol,1:pver)=0._r8    

        prao(1:ncol,1:pver)=0._r8 
        prco(1:ncol,1:pver)=0._r8 
        mnuccco(1:ncol,1:pver)=0._r8 
        mnuccto(1:ncol,1:pver)=0._r8 
        msacwio(1:ncol,1:pver)=0._r8 
        psacwso(1:ncol,1:pver)=0._r8 
        bergso(1:ncol,1:pver)=0._r8 
        bergo(1:ncol,1:pver)=0._r8 
        melto(1:ncol,1:pver)=0._r8 
        homoo(1:ncol,1:pver)=0._r8 
        qcreso(1:ncol,1:pver)=0._r8 
        prcio(1:ncol,1:pver)=0._r8 
        praio(1:ncol,1:pver)=0._r8 
        qireso(1:ncol,1:pver)=0._r8 
        mnuccro(1:ncol,1:pver)=0._r8 
        pracso (1:ncol,1:pver)=0._r8 
        meltsdt(1:ncol,1:pver)=0._r8
        frzrdt (1:ncol,1:pver)=0._r8
        mnuccdo(1:ncol,1:pver)=0._r8


	deltat=deltatin	



        omsm=0.99999_r8
        dto2=0.5_r8*deltat
	mincld=0.0001_r8
 

        q(1:ncol,1:pver)=qn(1:ncol,1:pver)
        t(1:ncol,1:pver)=tn(1:ncol,1:pver)
	


        do k=1,pver
           do i=1,ncol
        rho(i,k)=p(i,k)/(r*t(i,k))
	dv(i,k) = 8.794E-5_r8*t(i,k)**1.81_r8/p(i,k)
	mu(i,k) = 1.496E-6_r8*t(i,k)**1.5_r8/ &
             (t(i,k)+120._r8)	
	sc(i,k) = mu(i,k)/(rho(i,k)*dv(i,k))
	kap(i,k) = 1.414e3_r8*1.496e-6_r8*t(i,k)** &
            1.5_r8/(t(i,k)+120._r8) 





	rhof(i,k)=(rhosu/rho(i,k))**0.54

        arn(i,k)=ar*rhof(i,k)
        asn(i,k)=as*rhof(i,k)
        acn(i,k)=ac*rhof(i,k)
        ain(i,k)=ai*rhof(i,k)




        dz(i,k)= pdel(i,k)/(rho(i,k)*g)

        end do
        end do


        t1(1:ncol,1:pver) = t(1:ncol,1:pver)
	q1(1:ncol,1:pver) = q(1:ncol,1:pver)
	qc1(1:ncol,1:pver) = qc(1:ncol,1:pver)
	qi1(1:ncol,1:pver) = qi(1:ncol,1:pver)
	nc1(1:ncol,1:pver) = nc(1:ncol,1:pver)
	ni1(1:ncol,1:pver) = ni(1:ncol,1:pver)


        tlat1(1:ncol,1:pver)=0._r8
	qvlat1(1:ncol,1:pver)=0._r8
	qctend1(1:ncol,1:pver)=0._r8
	qitend1(1:ncol,1:pver)=0._r8
	nctend1(1:ncol,1:pver)=0._r8
	nitend1(1:ncol,1:pver)=0._r8


	qrout(1:ncol,1:pver)=0._r8
	qsout(1:ncol,1:pver)=0._r8
	nrout(1:ncol,1:pver)=0._r8
	nsout(1:ncol,1:pver)=0._r8
        dsout(1:ncol,1:pver)=0._r8

        drout(1:ncol,1:pver)=0._r8
        
	reff_rain(1:ncol,1:pver)=fillvalue
        reff_snow(1:ncol,1:pver)=fillvalue


	nevapr(1:ncol,1:pver) = 0._r8
	evapsnow(1:ncol,1:pver) = 0._r8
	prain(1:ncol,1:pver) = 0._r8
	prodsnow(1:ncol,1:pver) = 0._r8
	cmeout(1:ncol,1:pver) = 0._r8


        rainrt1(1:ncol,1:pver) = 0._r8


        cldmax(1:ncol,1:pver)=mincld



        dum2l(1:ncol,1:pver)=0._r8
        dum2i(1:ncol,1:pver)=0._r8


	prect1(1:ncol)=0._r8
	preci1(1:ncol)=0._r8




        do k=1,pver




        call vqsatd_water(t(1,k),p(1,k),es,qs,gammas,ncol) 

        do i=1,ncol

	esl(i,k)=polysvp(t(i,k),0)
	esi(i,k)=polysvp(t(i,k),1)


	if (t(i,k).gt.tmelt)esi(i,k)=esl(i,k)

        relhum(i,k)=q(i,k)/qs(i)



           cldm(i,k)=max(cldn(i,k),mincld)
           cldmw(i,k)=max(cldn(i,k),mincld)

           icldm(i,k)=max(icecldf(i,k),mincld)
           lcldm(i,k)=max(liqcldf(i,k),mincld)






           if (sub_column) then

              cldm(i,k)=mincld
              cldmw(i,k)=mincld
              icldm(i,k)=mincld
              lcldm(i,k)=mincld
              
              if (qc(i,k).ge.qsmall) then
                 lcldm(i,k)=1.           
                 cldm(i,k)=1.
                 cldmw(i,k)=1.
              end if
              
              if (qi(i,k).ge.qsmall) then             
                 cldm(i,k)=1.
                 icldm(i,k)=1.
              end if
              
           end if               



        nfice(i,k)=0._r8
        dumfice=qc(i,k)+qi(i,k)
        if (dumfice.gt.qsmall .and. qi(i,k).gt.qsmall) then
           nfice(i,k)=qi(i,k)/dumfice
        endif

	if (t(i,k).lt.tmelt - 5._r8) then
           if (liu_in) then 


              dum2=naai(i,k)

           else

              dum2=0.005_r8*exp(0.304_r8*(273.15_r8-t(i,k)))*1000._r8


              dum2=min(dum,208.9e3_r8)/rho(i,k) 
           endif

           dumnnuc=(dum2-ni(i,k)/icldm(i,k))/deltat*icldm(i,k)
           dumnnuc=max(dumnnuc,0._r8)


           ninew=ni(i,k)+dumnnuc*deltat
           qinew=qi(i,k)+dumnnuc*deltat*mi0

        else
           ninew=ni(i,k)
           qinew=qi(i,k)
        end if



  cme(i,k) = 0._r8
  cmei(i,k)=0._r8






       berg(i,k)=0._r8
       prd = 0._r8




       if (icldm(i,k) .gt. 0._r8) then 
          qiic(i,k)=qinew/icldm(i,k)
          niic(i,k)=ninew/icldm(i,k)
       else
          qiic(i,k)=0._r8
          niic(i,k)=0._r8
       endif


           if (t(i,k).lt.273.15) then

              if (qi(i,k).gt.qsmall) then

                 bergtsf = 0._r8 

                    qvi=epsqs*esi(i,k)/(p(i,k)-(1._r8-epsqs)*esi(i,k))
                    qvl=epsqs*esl(i,k)/(p(i,k)-(1._r8-epsqs)*esl(i,k))

                    dqsidt =  xxls*qvi/(rv*t(i,k)**2)
                    abi = 1._r8+dqsidt*xxls/cpp



                    if (qiic(i,k).ge.qsmall) then
                       lami(k) = (cons1*ci* &
                       niic(i,k)/qiic(i,k))**(1._r8/di)
                       n0i(k) = niic(i,k)*lami(k)



                       if (lami(k).lt.lammini) then

                          lami(k) = lammini
                          n0i(k) = lami(k)**(di+1._r8)*qiic(i,k)/(ci*cons1)
                       else if (lami(k).gt.lammaxi) then
                          lami(k) = lammaxi
                          n0i(k) = lami(k)**(di+1._r8)*qiic(i,k)/(ci*cons1)
                       end if
                    
                       epsi = 2._r8*pi*n0i(k)*rho(i,k)*Dv(i,k) &
                       /(lami(k)*lami(k))


                 if (qc(i,k).gt. qsmall) then 







                       prd = epsi*(qvl-qvi)/abi
                    
                    else
                       prd = 0._r8
                    end if



                    prd = prd*min(icldm(i,k),lcldm(i,k))



                    berg(i,k)=max(0._r8,prd)

                 end if  

                 if (berg(i,k).gt.0._r8) then
                    bergtsf=max(0._r8,(qc(i,k)/berg(i,k))/deltat) 

                    if(bergtsf.lt.1._r8) berg(i,k) = max(0._r8,qc(i,k)/deltat)

                 endif



                 if (bergtsf.lt.1._r8.or.icldm(i,k).gt.lcldm(i,k)) then

			if (qiic(i,k).ge.qsmall) then



		if (qc(i,k).ge.qsmall) then
                       rhin  = (1.0_r8 + relhum(i,k)) / 2._r8
                       if ((rhin*esl(i,k)/esi(i,k)) > 1._r8) then
                          prd = epsi*(rhin*qvl-qvi)/abi


                          prd = prd*min(icldm(i,k),lcldm(i,k))


                          cmei(i,k) = cmei(i,k) + (prd * (1._r8- bergtsf))

	                end if 
	        end if 



		if (qc(i,k).lt.qsmall.or.icldm(i,k).gt.lcldm(i,k)) then




			if (qc(i,k).lt.qsmall) then 
			dum=0._r8 
			else
			dum=lcldm(i,k)
			end if


                       rhin = relhum(i,k)

                if ((rhin*esl(i,k)/esi(i,k)) > 1._r8) then

                       prd = epsi*(rhin*qvl-qvi)/abi



                       prd = prd*max((icldm(i,k)-dum),0._r8)
                       cmei(i,k) = cmei(i,k) + prd

		end if 
		end if 
	end if 
	end if 


                 if(cmei(i,k) > 0.0_r8 .and. (relhum(i,k)*esl(i,k)/esi(i,k)) > 1._r8 ) &
                  cmei(i,k)=min(cmei(i,k),(q(i,k)-qs(i)*esi(i,k)/esl(i,k))/abi/deltat)

              end if            
   


           end if  




           if ((-berg(i,k)).lt.-qc(i,k)/deltat) &
             berg(i,k) = max(qc(i,k)/deltat,0._r8)



            if ((relhum(i,k)*esl(i,k)/esi(i,k)).lt.1._r8 .and. qiic(i,k).ge.qsmall ) then

               qvi=epsqs*esi(i,k)/(p(i,k)-(1._r8-epsqs)*esi(i,k))
               qvl=epsqs*esl(i,k)/(p(i,k)-(1._r8-epsqs)*esl(i,k))
               dqsidt =  xxls*qvi/(rv*t(i,k)**2)
               abi = 1._r8+dqsidt*xxls/cpp



               lami(k) = (cons1*ci* &
               niic(i,k)/qiic(i,k))**(1._r8/di)
               n0i(k) = niic(i,k)*lami(k)



               if (lami(k).lt.lammini) then
                  
                  lami(k) = lammini
                  n0i(k) = lami(k)**(di+1._r8)*qiic(i,k)/(ci*cons1)
               else if (lami(k).gt.lammaxi) then
                  lami(k) = lammaxi
                  n0i(k) = lami(k)**(di+1._r8)*qiic(i,k)/(ci*cons1)
               end if
                    
               epsi = 2._r8*pi*n0i(k)*rho(i,k)*Dv(i,k) &
               /(lami(k)*lami(k))


               prd = epsi*(relhum(i,k)*qvl-qvi)/abi * icldm(i,k)
               cmei(i,k)=min(prd,0._r8)

            endif


           if (cmei(i,k).lt.-qi(i,k)/deltat) &
              cmei(i,k)=-qi(i,k)/deltat


           if(cmei(i,k) < 0.0_r8 .and. (relhum(i,k)*esl(i,k)/esi(i,k)) < 1._r8 ) &
              cmei(i,k)=min(0._r8,max(cmei(i,k),(q(i,k)-qs(i)*esi(i,k)/esl(i,k))/abi/deltat))



           cmei(i,k)=cmei(i,k)*omsm



        if (t(i,k).lt.(tmelt - 5._r8)) then 
          if ( liu_in ) then 




             dum2i(i,k)=naai(i,k)
          else


              dum2i(i,k)=0.005_r8*exp(0.304_r8*(273.15_r8-t(i,k)))*1000._r8


              dum2i(i,k)=min(dum2i(i,k),208.9e3_r8)/rho(i,k) 
           endif
        else
           dum2i(i,k)=0._r8
	end if

	end do 
	end do 

 
     cldo(:ncol,:)=cldn(:ncol,:)


	do i=1,ncol
	   
           rflx1(i,1)=0._r8
           sflx1(i,1)=0._r8
	do k=1,pver

	   
           rflx1(i,k+1)=0._r8
           sflx1(i,k+1)=0._r8
	end do 
	end do 

	do i=1,ncol
	   
           rflx(i,1)=0._r8
           sflx(i,1)=0._r8
	do k=1,pver
	   
           rflx(i,k+1)=0._r8
           sflx(i,k+1)=0._r8
	end do 
	end do 

	do i=1,ncol
	ltrue(i)=0
	do k=1,pver


	if (qc(i,k).ge.qsmall.or.qi(i,k).ge.qsmall.or.cmei(i,k).ge.qsmall) ltrue(i)=1
	end do
	end do



	iter = 2


	deltat=deltat/real(iter)








        mtime=1._r8
        rate1ord_cw2pr_st(:,:)=0._r8 


	do i=1,ncol
	if (ltrue(i).eq.0) then
           tlat(i,1:pver)=0._r8
           qvlat(i,1:pver)=0._r8
           qctend(i,1:pver)=0._r8
           qitend(i,1:pver)=0._r8
           qnitend(i,1:pver)=0._r8
           qrtend(i,1:pver)=0._r8
           nctend(i,1:pver)=0._r8
           nitend(i,1:pver)=0._r8
           nrtend(i,1:pver)=0._r8
           nstend(i,1:pver)=0._r8
           prect(i)=0._r8
           preci(i)=0._r8
           qniic(i,1:pver)=0._r8
           qric(i,1:pver)=0._r8
           nsic(i,1:pver)=0._r8
           nric(i,1:pver)=0._r8
           rainrt(i,1:pver)=0._r8
	goto 300
	end if

        qcsinksum_rate1ord(1:pver)=0._r8 
        qcsum_rate1ord(1:pver)=0._r8 




	do it=1,iter



	tlat(i,1:pver)=0._r8
	qvlat(i,1:pver)=0._r8
	qctend(i,1:pver)=0._r8
	qitend(i,1:pver)=0._r8
	qnitend(i,1:pver)=0._r8
	qrtend(i,1:pver)=0._r8
	nctend(i,1:pver)=0._r8
	nitend(i,1:pver)=0._r8
	nrtend(i,1:pver)=0._r8
	nstend(i,1:pver)=0._r8



        qniic(i,1:pver)=0._r8
        qric(i,1:pver)=0._r8
        nsic(i,1:pver)=0._r8
        nric(i,1:pver)=0._r8
 
        rainrt(i,1:pver)=0._r8






	   qrtot = 0._r8
	   nrtot = 0._r8
	   qstot = 0._r8
	   nstot = 0._r8



	   prect(i)=0._r8
	   preci(i)=0._r8

	   do k=1,pver



	   cwml(i,k)=qc(i,k)
	   cwmi(i,k)=qi(i,k)



	   ums(k)=0._r8 
	   uns(k)=0._r8 
	   umr(k)=0._r8 
	   unr(k)=0._r8







           if (k.eq.1) then
		cldmax(i,k)=cldm(i,k)
	   else


	   if (qric(i,k-1).ge.qsmall.or.qniic(i,k-1).ge.qsmall) then
                cldmax(i,k)=max(cldmax(i,k-1),cldm(i,k))
	   else
	   cldmax(i,k)=cldm(i,k)
	   end if
	   end if





           if (cmei(i,k) < 0._r8 .and. qi(i,k) > qsmall .and. cldm(i,k) > mincld) then
              nsubi(k)=cmei(i,k)/qi(i,k)*ni(i,k)/cldm(i,k)
	   else
              nsubi(k)=0._r8
           end if
           nsubc(k)=0._r8







        if (dum2i(i,k).gt.0._r8.and.t(i,k).lt.(tmelt - 5._r8).and. &
	   relhum(i,k)*esl(i,k)/esi(i,k).gt. rhmini+0.05_r8) then




              nnuccd(k)=(dum2i(i,k)-ni(i,k)/icldm(i,k))/deltat*icldm(i,k)
              nnuccd(k)=max(nnuccd(k),0._r8)
              nimax = dum2i(i,k)*icldm(i,k)




              mnuccd(k) = nnuccd(k) * mi0


              cmei(i,k)= cmei(i,k) + mnuccd(k) * mtime



               qvi=epsqs*esi(i,k)/(p(i,k)-(1._r8-epsqs)*esi(i,k))
               dqsidt =  xxls*qvi/(rv*t(i,k)**2)
               abi = 1._r8+dqsidt*xxls/cpp
              cmei(i,k)=min(cmei(i,k),(q(i,k)-qvi)/abi/deltat)


              cmei(i,k)=cmei(i,k)*omsm

           else
              nnuccd(k)=0._r8
	      nimax = 0._r8
              mnuccd(k) = 0._r8
           end if









           qcic(i,k)=min(cwml(i,k)/lcldm(i,k),5.e-3_r8)
           qiic(i,k)=min(cwmi(i,k)/icldm(i,k),5.e-3_r8)
           ncic(i,k)=max(nc(i,k)/lcldm(i,k),0._r8)
           niic(i,k)=max(ni(i,k)/icldm(i,k),0._r8)

           if (qc(i,k) - berg(i,k)*deltat.lt.qsmall) then
              qcic(i,k)=0._r8
              ncic(i,k)=0._r8
              if (qc(i,k)-berg(i,k)*deltat.lt.0._r8) then
                 berg(i,k)=qc(i,k)/deltat*omsm
              end if
	   end if

	   if (qi(i,k)+(cmei(i,k)+berg(i,k))*deltat.lt.qsmall) then
	   qiic(i,k)=0._r8
	   niic(i,k)=0._r8
	   if (qi(i,k)+(cmei(i,k)+berg(i,k))*deltat.lt.0._r8) then
	   cmei(i,k)=(-qi(i,k)/deltat-berg(i,k))*omsm
	   end if
	   end if



           cmeout(i,k) = cmeout(i,k)+cmei(i,k)










           if (qcic(i,k).ge.qsmall) then   
              npccn(k) = max(0._r8,npccnin(i,k))  
	      dum2l(i,k)=(nc(i,k)+npccn(k)*deltat)/lcldm(i,k)
              dum2l(i,k)=max(dum2l(i,k),cdnl/rho(i,k)) 
	      ncmax = dum2l(i,k)*lcldm(i,k)
           else
              npccn(k)=0._r8
              dum2l(i,k)=0._r8
              ncmax = 0._r8
           end if 









	if (qiic(i,k).ge.qsmall) then


	niic(i,k)=min(niic(i,k),qiic(i,k)*1.e20_r8)

	lami(k) = (cons1*ci* &
              niic(i,k)/qiic(i,k))**(1._r8/di)
	n0i(k) = niic(i,k)*lami(k)




	if (lami(k).lt.lammini) then

	lami(k) = lammini
	n0i(k) = lami(k)**(di+1._r8)*qiic(i,k)/(ci*cons1)
	niic(i,k) = n0i(k)/lami(k)
	else if (lami(k).gt.lammaxi) then
	lami(k) = lammaxi
	n0i(k) = lami(k)**(di+1._r8)*qiic(i,k)/(ci*cons1)
        niic(i,k) = n0i(k)/lami(k)
	end if

	else
	lami(k) = 0._r8
	n0i(k) = 0._r8
	end if

	if (qcic(i,k).ge.qsmall) then


	ncic(i,k)=min(ncic(i,k),qcic(i,k)*1.e20_r8)

        ncic(i,k)=max(ncic(i,k),cdnl/rho(i,k)) 



        pgam(k)=0.0005714_r8*(ncic(i,k)/1.e6_r8*rho(i,k))+0.2714_r8
        pgam(k)=1._r8/(pgam(k)**2)-1._r8
        pgam(k)=max(pgam(k),2._r8)
        pgam(k)=min(pgam(k),15._r8)



	lamc(k) = (pi/6._r8*rhow*ncic(i,k)*gamma(pgam(k)+4._r8)/ &
                 (qcic(i,k)*gamma(pgam(k)+1._r8)))**(1._r8/3._r8)



	lammin = (pgam(k)+1._r8)/50.e-6_r8
	lammax = (pgam(k)+1._r8)/2.e-6_r8

	if (lamc(k).lt.lammin) then
	lamc(k) = lammin
	ncic(i,k) = 6._r8*lamc(k)**3*qcic(i,k)* &
                gamma(pgam(k)+1._r8)/ &
               (pi*rhow*gamma(pgam(k)+4._r8))
	else if (lamc(k).gt.lammax) then
	lamc(k) = lammax
	ncic(i,k) = 6._r8*lamc(k)**3*qcic(i,k)* &
                gamma(pgam(k)+1._r8)/ &
               (pi*rhow*gamma(pgam(k)+4._r8))
	end if



        cdist1(k) = ncic(i,k)/gamma(pgam(k)+1._r8) 

	else
	lamc(k) = 0._r8
        cdist1(k) = 0._r8
	end if









	if (qcic(i,k).ge.1.e-8_r8) then







 
           if (sub_column) then
 
              prc(k) = 1350._r8*qcic(i,k)**2.47_r8* &
              (ncic(i,k)/1.e6_r8*rho(i,k))**(-1.79_r8)
              nprc(k) = prc(k)/(4._r8/3._r8*pi*rhow*(25.e-6_r8)**3)
              nprc1(k) = prc(k)/(qcic(i,k)/ncic(i,k))
 
           else

              prc(k) = cons2/(cons3*cons18)*1350._r8*qcic(i,k)**2.47_r8* &
              (ncic(i,k)/1.e6_r8*rho(i,k))**(-1.79_r8)
              nprc(k) = prc(k)/cons22
              nprc1(k) = prc(k)/(qcic(i,k)/ncic(i,k))
 
           end if               

        else
           prc(k)=0._r8
           nprc(k)=0._r8
	   nprc1(k)=0._r8
	end if
 





	dum=0.45_r8
	dum1=0.45_r8

	if (k.eq.1) then
	qric(i,k)=prc(k)*lcldm(i,k)*dz(i,k)/cldmax(i,k)/dum
	nric(i,k)=nprc(k)*lcldm(i,k)*dz(i,k)/cldmax(i,k)/dum
	else
	if (qric(i,k-1).ge.qsmall) then
	dum=umr(k-1)
	dum1=unr(k-1)
	end if





	if (qric(i,k-1).ge.1.e-9_r8.or.qniic(i,k-1).ge.1.e-9_r8) then
	nprc(k)=0._r8
	end if

        qric(i,k) = (rho(i,k-1)*umr(k-1)*qric(i,k-1)*cldmax(i,k-1)+ &
         (rho(i,k)*dz(i,k)*((pra(k-1)+prc(k))*lcldm(i,k)+(pre(k-1)-pracs(k-1)-mnuccr(k-1))*cldmax(i,k))))&
                   /(dum*rho(i,k)*cldmax(i,k))
	nric(i,k) = (rho(i,k-1)*unr(k-1)*nric(i,k-1)*cldmax(i,k-1)+ &
         (rho(i,k)*dz(i,k)*(nprc(k)*lcldm(i,k)+(nsubr(k-1)-npracs(k-1)-nnuccr(k-1)+nragg(k-1))*cldmax(i,k))))&
                   /(dum1*rho(i,k)*cldmax(i,k))

	end if





	if (t(i,k).le.273.15_r8.and.qiic(i,k).ge.qsmall) then



           nprci(k) = n0i(k)/(lami(k)*180._r8)*exp(-lami(k)*dcs)

           prci(k) = pi*rhoi*n0i(k)/(6._r8*180._r8)* &
              (cons23/lami(k)+3._r8*cons24/lami(k)**2+ &
          6._r8*dcs/lami(k)**3+6._r8/lami(k)**4)*exp(-lami(k)*dcs)          

        else
              prci(k)=0._r8
              nprci(k)=0._r8
	end if




	dum=(asn(i,k)*cons25)
	dum1=(asn(i,k)*cons25)

	if (k.eq.1) then
           qniic(i,k)=prci(k)*icldm(i,k)*dz(i,k)/cldmax(i,k)/dum
           nsic(i,k)=nprci(k)*icldm(i,k)*dz(i,k)/cldmax(i,k)/dum
	else
	if (qniic(i,k-1).ge.qsmall) then
	dum=ums(k-1)
	dum1=uns(k-1)
	end if

        qniic(i,k) = (rho(i,k-1)*ums(k-1)*qniic(i,k-1)*cldmax(i,k-1)+ &
         (rho(i,k)*dz(i,k)*((prci(k)+prai(k-1)+psacws(k-1)+bergs(k-1))*icldm(i,k)+(prds(k-1)+ &
         pracs(k-1)+mnuccr(k-1))*cldmax(i,k))))&
                    /(dum*rho(i,k)*cldmax(i,k))

	nsic(i,k) = (rho(i,k-1)*uns(k-1)*nsic(i,k-1)*cldmax(i,k-1)+ &
         (rho(i,k)*dz(i,k)*(nprci(k)*icldm(i,k)+(nsubs(k-1)+nsagg(k-1)+nnuccr(k-1))*cldmax(i,k))))&
                   /(dum1*rho(i,k)*cldmax(i,k))

	end if



	if (qniic(i,k).lt.qsmall) then
	qniic(i,k)=0._r8
	nsic(i,k)=0._r8
	end if

	if (qric(i,k).lt.qsmall) then
	qric(i,k)=0._r8
	nric(i,k)=0._r8
	end if




	nric(i,k)=max(nric(i,k),0._r8)
	nsic(i,k)=max(nsic(i,k),0._r8)






	if (qric(i,k).ge.qsmall) then
	lamr(k) = (pi*rhow*nric(i,k)/qric(i,k))**(1._r8/3._r8)
	n0r(k) = nric(i,k)*lamr(k)




	if (lamr(k).lt.lamminr) then

	lamr(k) = lamminr

	n0r(k) = lamr(k)**4*qric(i,k)/(pi*rhow)
	nric(i,k) = n0r(k)/lamr(k)
	else if (lamr(k).gt.lammaxr) then
	lamr(k) = lammaxr
	n0r(k) = lamr(k)**4*qric(i,k)/(pi*rhow)
	nric(i,k) = n0r(k)/lamr(k)
	end if



        unr(k) = min(arn(i,k)*cons4/lamr(k)**br,9.1_r8*rhof(i,k))
        umr(k) = min(arn(i,k)*cons5/(6._r8*lamr(k)**br),9.1_r8*rhof(i,k))

	else
	lamr(k) = 0._r8
	n0r(k) = 0._r8
	umr(k) = 0._r8
	unr(k) = 0._r8
	end if




	if (qniic(i,k).ge.qsmall) then
	lams(k) = (cons6*cs*nsic(i,k)/ &
            qniic(i,k))**(1._r8/ds)
	n0s(k) = nsic(i,k)*lams(k)




	if (lams(k).lt.lammins) then
	lams(k) = lammins
	n0s(k) = lams(k)**(ds+1._r8)*qniic(i,k)/(cs*cons6)
	nsic(i,k) = n0s(k)/lams(k)

	else if (lams(k).gt.lammaxs) then
	lams(k) = lammaxs
	n0s(k) = lams(k)**(ds+1._r8)*qniic(i,k)/(cs*cons6)
	nsic(i,k) = n0s(k)/lams(k)
	end if



        ums(k) = min(asn(i,k)*cons8/(6._r8*lams(k)**bs),1.2_r8*rhof(i,k))
        uns(k) = min(asn(i,k)*cons7/lams(k)**bs,1.2_r8*rhof(i,k))

	else
	lams(k) = 0._r8
	n0s(k) = 0._r8
	ums(k) = 0._r8
	uns(k) = 0._r8
	end if





	if (qcic(i,k).ge.qsmall .and. t(i,k).lt.269.15_r8) then



 

           
           if (sub_column) then
 
              mnuccc(k) = &
                       pi*pi/36._r8*rhow* &
                   cdist1(k)*gamma(7._r8+pgam(k))* &
                    bimm*(exp(aimm*(273.15_r8-t(i,k)))-1._r8)/ &
                    lamc(k)**3/lamc(k)**3
 
              nnuccc(k) = &
                    pi/6._r8*cdist1(k)*gamma(pgam(k)+4._r8) &
                    *bimm* &
                    (exp(aimm*(273.15_r8-t(i,k)))-1._r8)/lamc(k)**3
 
           else
 
              mnuccc(k) = cons9/(cons3*cons19)* &
                      pi*pi/36._r8*rhow* &
                  cdist1(k)*gamma(7._r8+pgam(k))* &
                   bimm*(exp(aimm*(273.15_r8-t(i,k)))-1._r8)/ &
                   lamc(k)**3/lamc(k)**3

              nnuccc(k) = cons10/(cons3*qcvar)* &
                  pi/6._r8*cdist1(k)*gamma(pgam(k)+4._r8) &
                  *bimm* &
                  (exp(aimm*(273.15_r8-t(i,k)))-1._r8)/lamc(k)**3
           end if           





           tcnt=(270.16_r8-t(i,k))**1.3_r8
           viscosity=1.8e-5_r8*(t(i,k)/298.0_r8)**0.85_r8    
           mfp=2.0_r8*viscosity/(p(i,k)  &                   
               *sqrt(8.0_r8*28.96e-3_r8/(pi*8.314409_r8*t(i,k))))           

           nslip1=1.0_r8+(mfp/rndst(i,k,1))*(1.257_r8+(0.4_r8*Exp(-(1.1_r8*rndst(i,k,1)/mfp))))
           nslip2=1.0_r8+(mfp/rndst(i,k,2))*(1.257_r8+(0.4_r8*Exp(-(1.1_r8*rndst(i,k,2)/mfp))))
           nslip3=1.0_r8+(mfp/rndst(i,k,3))*(1.257_r8+(0.4_r8*Exp(-(1.1_r8*rndst(i,k,3)/mfp))))
           nslip4=1.0_r8+(mfp/rndst(i,k,4))*(1.257_r8+(0.4_r8*Exp(-(1.1_r8*rndst(i,k,4)/mfp))))

           ndfaer1=1.381e-23_r8*t(i,k)*nslip1/(6._r8*pi*viscosity*rndst(i,k,1))  
           ndfaer2=1.381e-23_r8*t(i,k)*nslip2/(6._r8*pi*viscosity*rndst(i,k,2))
           ndfaer3=1.381e-23_r8*t(i,k)*nslip3/(6._r8*pi*viscosity*rndst(i,k,3))
           ndfaer4=1.381e-23_r8*t(i,k)*nslip4/(6._r8*pi*viscosity*rndst(i,k,4))


           if (sub_column) then
 
               mnucct(k) = &
                        (ndfaer1*(nacon(i,k,1)*tcnt)+ndfaer2*(nacon(i,k,2)*tcnt)+&
                                  ndfaer3*(nacon(i,k,3)*tcnt)+ndfaer4*(nacon(i,k,4)*tcnt))*pi*pi/3._r8*rhow* &
                        cdist1(k)*gamma(pgam(k)+5._r8)/lamc(k)**4
 
               nnucct(k) = (ndfaer1*(nacon(i,k,1)*tcnt)+ndfaer2*(nacon(i,k,2)*tcnt)+&
                                     ndfaer3*(nacon(i,k,3)*tcnt)+ndfaer4*(nacon(i,k,4)*tcnt))*2._r8*pi*  &
                        cdist1(k)*gamma(pgam(k)+2._r8)/lamc(k)
 
           else

               mnucct(k) = gamma(qcvar+4._r8/3._r8)/(cons3*qcvar**(4._r8/3._r8))*  &
                       (ndfaer1*(nacon(i,k,1)*tcnt)+ndfaer2*(nacon(i,k,2)*tcnt)+&
                                 ndfaer3*(nacon(i,k,3)*tcnt)+ndfaer4*(nacon(i,k,4)*tcnt))*pi*pi/3._r8*rhow* &
                       cdist1(k)*gamma(pgam(k)+5._r8)/lamc(k)**4

                nnucct(k) =  gamma(qcvar+1._r8/3._r8)/(cons3*qcvar**(1._r8/3._r8))*  &
                         (ndfaer1*(nacon(i,k,1)*tcnt)+ndfaer2*(nacon(i,k,2)*tcnt)+&
                                   ndfaer3*(nacon(i,k,3)*tcnt)+ndfaer4*(nacon(i,k,4)*tcnt))*2._r8*pi*  &
                       cdist1(k)*gamma(pgam(k)+2._r8)/lamc(k)

           end if      




        if (nnuccc(k)*lcldm(i,k).gt.nnuccd(k)) then
        dum=(nnuccd(k)/(nnuccc(k)*lcldm(i,k)))

        mnuccc(k)=mnuccc(k)*dum
        nnuccc(k)=nnuccd(k)/lcldm(i,k)
        end if

        else
           mnuccc(k)=0._r8
           nnuccc(k)=0._r8
           mnucct(k)=0._r8
           nnucct(k)=0._r8
	end if






	if (qniic(i,k).ge.qsmall .and. t(i,k).le.273.15_r8) then
 	nsagg(k) = -1108._r8*asn(i,k)*Eii* &
             pi**((1._r8-bs)/3._r8)*rhosn**((-2._r8-bs)/3._r8)*rho(i,k)** &
            ((2._r8+bs)/3._r8)*qniic(i,k)**((2._r8+bs)/3._r8)* &
            (nsic(i,k)*rho(i,k))**((4._r8-bs)/3._r8)/ &
            (4._r8*720._r8*rho(i,k))
        else
        nsagg(k)=0._r8
	end if










	if (qniic(i,k).ge.qsmall .and. t(i,k).le.tmelt .and. &
            qcic(i,k).ge.qsmall) then






        dc0 = (pgam(k)+1._r8)/lamc(k)
        ds0 = 1._r8/lams(k)
        dum = dc0*dc0*uns(k)*rhow/(9._r8*mu(i,k)*ds0)
        eci = dum*dum/((dum+0.4_r8)*(dum+0.4_r8))

        eci = max(eci,0._r8)
        eci = min(eci,1._r8)





	psacws(k) = pi/4._r8*asn(i,k)*qcic(i,k)*rho(i,k)* &
                  n0s(k)*Eci*cons11/ &
                  lams(k)**(bs+3._r8)	
	npsacws(k) = pi/4._r8*asn(i,k)*ncic(i,k)*rho(i,k)* &
                  n0s(k)*Eci*cons11/ &
                  lams(k)**(bs+3._r8)
        else
           psacws(k)=0._r8
           npsacws(k)=0._r8
        end if



        if((t(i,k).lt.270.16_r8) .and. (t(i,k).ge.268.16_r8)) then
           ni_secp   = 3.5e8_r8*(270.16_r8-t(i,k))/2.0_r8*psacws(k)
           nsacwi(k) = ni_secp
           msacwi(k) = min(ni_secp*mi0,psacws(k))
        else if((t(i,k).lt.268.16_r8) .and. (t(i,k).ge.265.16_r8)) then
           ni_secp   = 3.5e8_r8*(t(i,k)-265.16_r8)/3.0_r8*psacws(k)
           nsacwi(k) = ni_secp
           msacwi(k) = min(ni_secp*mi0,psacws(k))
        else
           ni_secp   = 0.0_r8
           nsacwi(k) = 0.0_r8
           msacwi(k) = 0.0_r8
        endif
        psacws(k) = max(0.0_r8,psacws(k)-ni_secp*mi0)





	if (qric(i,k).ge.1.e-8_r8 .and. qniic(i,k).ge.1.e-8_r8 .and. & 
              t(i,k).le.273.15_r8) then

	pracs(k) = pi*pi*ecr*(((1.2_r8*umr(k)-0.95_r8*ums(k))**2+ &
                  0.08_r8*ums(k)*umr(k))**0.5_r8*rhow*rho(i,k)* &
                 n0r(k)*n0s(k)* &
                  (5._r8/(lamr(k)**6*lams(k))+ &
                  2._r8/(lamr(k)**5*lams(k)**2)+ &
                  0.5_r8/(lamr(k)**4*lams(k)**3)))

	npracs(k) = pi/2._r8*rho(i,k)*ecr*(1.7_r8*(unr(k)-uns(k))**2+ &
              0.3_r8*unr(k)*uns(k))**0.5_r8*n0r(k)*n0s(k)* &
              (1._r8/(lamr(k)**3*lams(k))+ &
              1._r8/(lamr(k)**2*lams(k)**2)+ &
              1._r8/(lamr(k)*lams(k)**3))

        else
           pracs(k)=0._r8
           npracs(k)=0._r8
	end if





	if (t(i,k).lt.269.15_r8 .and. qric(i,k).ge.qsmall) then

	mnuccr(k) = 20._r8*pi*pi*rhow*nric(i,k)*bimm* &
                  (exp(aimm*(273.15_r8-t(i,k)))-1._r8)/lamr(k)**3 &
                 /lamr(k)**3

	nnuccr(k) = pi*nric(i,k)*bimm* &
                   (exp(aimm*(273.15_r8-t(i,k)))-1._r8)/lamr(k)**3
        else
           mnuccr(k)=0._r8
           nnuccr(k)=0._r8
	end if






	if (qric(i,k).ge.qsmall .and. qcic(i,k).ge.qsmall) then




 
           if (sub_column) then
 
              pra(k) = &
                       67._r8*(qcic(i,k)*qric(i,k))**1.15_r8
              npra(k) = pra(k)/(qcic(i,k)/ncic(i,k))
 
           else

              pra(k) = cons12/(cons3*cons20)* &
                      67._r8*(qcic(i,k)*qric(i,k))**1.15_r8
              npra(k) = pra(k)/(qcic(i,k)/ncic(i,k))

           end if               

         else
           pra(k)=0._r8
           npra(k)=0._r8
	end if





	if (qric(i,k).ge.qsmall) then
	nragg(k) = -8._r8*nric(i,k)*qric(i,k)*rho(i,k)
        else
           nragg(k)=0._r8
	end if






	if (qniic(i,k).ge.qsmall.and.qiic(i,k).ge.qsmall &
            .and.t(i,k).le.273.15_r8) then
	prai(k) = pi/4._r8*asn(i,k)*qiic(i,k)*rho(i,k)* &
                  n0s(k)*Eii*cons11/ &
                  lams(k)**(bs+3._r8)	
	nprai(k) = pi/4._r8*asn(i,k)*niic(i,k)* &
                  rho(i,k)*n0s(k)*Eii*cons11/ &
                  lams(k)**(bs+3._r8)
        else
        prai(k)=0._r8
        nprai(k)=0._r8
	end if








	pre(k)=0._r8
	prds(k)=0._r8




        if (qcic(i,k)+qiic(i,k).lt.1.e-6_r8.or.cldmax(i,k).gt.lcldm(i,k)) then




        if (qcic(i,k)+qiic(i,k).lt.1.e-6_r8) then
	dum=0._r8
	else
	dum=lcldm(i,k)
	end if


        esn=polysvp(t(i,k),0)
	qsn=min(epsqs*esn/(p(i,k)-(1._r8-epsqs)*esn),1._r8)


	esl(i,k)=esn
	esi(i,k)=polysvp(t(i,k),1)

	if (t(i,k).gt.tmelt)esi(i,k)=esl(i,k)


	qclr=(q(i,k)-dum*qsn)/(1._r8-dum)

	if (qric(i,k).ge.qsmall) then

        qvs=epsqs*esl(i,k)/(p(i,k)-(1._r8-epsqs)*esl(i,k))
	dqsdt = xxlv*qvs/(rv*t(i,k)**2)
        ab = 1._r8+dqsdt*xxlv/cpp
        epsr = 2._r8*pi*n0r(k)*rho(i,k)*Dv(i,k)* &
                   (f1r/(lamr(k)*lamr(k))+ &
                    f2r*(arn(i,k)*rho(i,k)/mu(i,k))**0.5_r8* &
                    sc(i,k)**(1._r8/3._r8)*cons13/ &
                (lamr(k)**(5._r8/2._r8+br/2._r8)))

	   pre(k) = epsr*(qclr-qvs)/ab



           pre(k)=min(pre(k)*(cldmax(i,k)-dum),0._r8)
           pre(k)=pre(k)/cldmax(i,k)
	end if


	if (qniic(i,k).ge.qsmall) then
        qvi=epsqs*esi(i,k)/(p(i,k)-(1._r8-epsqs)*esi(i,k))
        dqsidt =  xxls*qvi/(rv*t(i,k)**2)
        abi = 1._r8+dqsidt*xxls/cpp
        epss = 2._r8*pi*n0s(k)*rho(i,k)*Dv(i,k)* &
                   (f1s/(lams(k)*lams(k))+ &
                    f2s*(asn(i,k)*rho(i,k)/mu(i,k))**0.5_r8* &
                    sc(i,k)**(1._r8/3._r8)*cons14/ &
               (lams(k)**(5._r8/2._r8+bs/2._r8)))
	   prds(k) = epss*(qclr-qvi)/abi	


           prds(k)=min(prds(k)*(cldmax(i,k)-dum),0._r8)
	   prds(k)=prds(k)/cldmax(i,k)
	end if




	qtmp=q(i,k)-(cmei(i,k)+(pre(k)+prds(k))*cldmax(i,k))*deltat
	ttmp=t(i,k)+((pre(k)*cldmax(i,k))*xxlv+ &
                (cmei(i,k)+prds(k)*cldmax(i,k))*xxls)*deltat/cpp


        ttmp=max(180._r8,min(ttmp,323._r8))

        esn=polysvp(ttmp,0)  
	qsn=min(epsqs*esn/(p(i,k)-(1._r8-epsqs)*esn),1._r8)


	if (qtmp.gt.qsn) then
	if (pre(k)+prds(k).lt.-1.e-20) then
	dum1=pre(k)/(pre(k)+prds(k))

	qtmp=q(i,k)-(cmei(i,k))*deltat
	ttmp=t(i,k)+(cmei(i,k)*xxls)*deltat/cpp
        esn=polysvp(ttmp,0) 
	qsn=min(epsqs*esn/(p(i,k)-(1._r8-epsqs)*esn),1._r8)
	dum=(qtmp-qsn)/(1._r8 + cons27*qsn/(cpp*rv*ttmp**2))
	dum=min(dum,0._r8)


	pre(k)=dum*dum1/deltat/cldmax(i,k)


        esn=polysvp(ttmp,1) 
	qsn=min(epsqs*esn/(p(i,k)-(1._r8-epsqs)*esn),1._r8)
	dum=(qtmp-qsn)/(1._r8 + cons28*qsn/(cpp*rv*ttmp**2))
	dum=min(dum,0._r8)


	prds(k)=dum*(1._r8-dum1)/deltat/cldmax(i,k)
	end if
	end if

	end if



	if (qniic(i,k).ge.qsmall.and.qcic(i,k).ge.qsmall.and.t(i,k).lt.tmelt) then
        qvi=epsqs*esi(i,k)/(p(i,k)-(1._r8-epsqs)*esi(i,k))
        qvs=epsqs*esl(i,k)/(p(i,k)-(1._r8-epsqs)*esl(i,k))
        dqsidt =  xxls*qvi/(rv*t(i,k)**2)
        abi = 1._r8+dqsidt*xxls/cpp
        epss = 2._r8*pi*n0s(k)*rho(i,k)*Dv(i,k)* &
                   (f1s/(lams(k)*lams(k))+ &
                    f2s*(asn(i,k)*rho(i,k)/mu(i,k))**0.5_r8* &
                    sc(i,k)**(1._r8/3._r8)*cons14/ &
               (lams(k)**(5._r8/2._r8+bs/2._r8)))
	bergs(k)=epss*(qvs-qvi)/abi
	else
	bergs(k)=0._r8
	end if













        qce=(qc(i,k) - berg(i,k)*deltat)
        nce=(nc(i,k)+npccn(k)*deltat*mtime)
        qie=(qi(i,k)+(cmei(i,k)+berg(i,k))*deltat)
        nie=(ni(i,k)+nnuccd(k)*deltat*mtime)



        dum = (prc(k)+pra(k)+mnuccc(k)+mnucct(k)+msacwi(k)+ &

              psacws(k)+bergs(k))*lcldm(i,k)*deltat

	if (dum.gt.qce) then

         if (abs(prc(k)+pra(k)+mnuccc(k)+mnucct(k)+msacwi(k)+psacws(k)+bergs(k)) > 1.e-20_r8) then
          ratio = qce/deltat/lcldm(i,k)/(prc(k)+pra(k)+mnuccc(k)+mnucct(k)+msacwi(k)+psacws(k)+bergs(k))*omsm 
         else
          ratio = 0.5_r8
         end if
	prc(k) = prc(k)*ratio
	pra(k) = pra(k)*ratio
	mnuccc(k) = mnuccc(k)*ratio
        mnucct(k) = mnucct(k)*ratio  
        msacwi(k) = msacwi(k)*ratio  
	psacws(k) = psacws(k)*ratio	
	bergs(k) = bergs(k)*ratio	
        end if



        dum = (nprc1(k)+npra(k)+nnuccc(k)+nnucct(k)+ &
              npsacws(k)-nsubc(k))*lcldm(i,k)*deltat

	if (dum.gt.nce) then

          if (abs(nprc1(k)+npra(k)+nnuccc(k)+nnucct(k)+npsacws(k)-nsubc(k)) > 1.e-20_r8) then
        ratio = nce/deltat/((nprc1(k)+npra(k)+nnuccc(k)+nnucct(k)+&
            npsacws(k)-nsubc(k))*lcldm(i,k))*omsm
          else
        ratio = 0.5_r8
          end if

	nprc1(k) = nprc1(k)*ratio
	npra(k) = npra(k)*ratio
	nnuccc(k) = nnuccc(k)*ratio
        nnucct(k) = nnucct(k)*ratio  
	npsacws(k) = npsacws(k)*ratio	
	nsubc(k)=nsubc(k)*ratio
        end if



        dum = ((-mnuccc(k)-mnucct(k)-msacwi(k))*lcldm(i,k)+(prci(k)+ &
               prai(k))*icldm(i,k))*deltat

	if (dum.gt.qie) then


        if(abs(prci(k)+prai(k)).gt.1.0e-20) then
          ratio = (qie/deltat+(mnuccc(k)+mnucct(k)+msacwi(k))*lcldm(i,k))/((prci(k)+prai(k))*icldm(i,k))*omsm
        else
          ratio=0.5
        endif

        prci(k) = prci(k)*ratio
        prai(k) = prai(k)*ratio
        end if



        dum = ((-nnucct(k)-nsacwi(k))*lcldm(i,k)+(nprci(k)+ &
               nprai(k)-nsubi(k))*icldm(i,k))*deltat

	if (dum.gt.nie) then

           if(abs( ((nprci(k)+nprai(k)-nsubi(k))*icldm(i,k))*omsm )  .gt. 1.0e-20_r8) then
              ratio = (nie/deltat+(nnucct(k)+nsacwi(k))*lcldm(i,k))/ &  
                   ((nprci(k)+nprai(k)-nsubi(k))*icldm(i,k))*omsm
           else
              ratio = 0.5
           endif
        nprci(k) = nprci(k)*ratio
        nprai(k) = nprai(k)*ratio
	nsubi(k) = nsubi(k)*ratio
        end if






	if (((prc(k)+pra(k))*lcldm(i,k)+(-mnuccr(k)+pre(k)-pracs(k))*&
               cldmax(i,k))*dz(i,k)*rho(i,k)+qrtot.lt.0._r8) then

	if (-pre(k)+pracs(k)+mnuccr(k).ge.qsmall) then
	     
	ratio = (qrtot/(dz(i,k)*rho(i,k))+(prc(k)+pra(k))*lcldm(i,k))/&
                ((-pre(k)+pracs(k)+mnuccr(k))*cldmax(i,k))*omsm 

        pre(k) = pre(k)*ratio
        pracs(k) = pracs(k)*ratio
        mnuccr(k) = mnuccr(k)*ratio
	end if
        end if



       nsubr(k)=0._r8

       if ((nprc(k)*lcldm(i,k)+(-nnuccr(k)+nsubr(k)-npracs(k)&
            +nragg(k))*cldmax(i,k))*dz(i,k)*rho(i,k)+nrtot.lt.0._r8) then

	if (-nsubr(k)-nragg(k)+npracs(k)+nnuccr(k).ge.qsmall) then

	ratio = (nrtot/(dz(i,k)*rho(i,k))+nprc(k)*lcldm(i,k))/&
               ((-nsubr(k)-nragg(k)+npracs(k)+nnuccr(k))*cldmax(i,k))*omsm

        nsubr(k) = nsubr(k)*ratio
        npracs(k) = npracs(k)*ratio
        nnuccr(k) = nnuccr(k)*ratio
        nragg(k) = nragg(k)*ratio
	end if
        end if



	if (((bergs(k)+psacws(k))*lcldm(i,k)+(prai(k)+prci(k))*icldm(i,k)+(pracs(k)+&
            mnuccr(k)+prds(k))*cldmax(i,k))*dz(i,k)*rho(i,k)+qstot.lt.0._r8) then

	if (-prds(k).ge.qsmall) then

	ratio = (qstot/(dz(i,k)*rho(i,k))+(bergs(k)+psacws(k))*lcldm(i,k)+(prai(k)+prci(k))*icldm(i,k)+&
                (pracs(k)+mnuccr(k))*cldmax(i,k))/(-prds(k)*cldmax(i,k))*omsm

        prds(k) = prds(k)*ratio
	end if
       end if





       nsubs(k)=0._r8

       if ((nprci(k)*icldm(i,k)+(nnuccr(k)+nsubs(k)+nsagg(k))*cldmax(i,k))*&
            dz(i,k)*rho(i,k)+nstot.lt.0._r8) then

	if (-nsubs(k)-nsagg(k).ge.qsmall) then

	ratio = (nstot/(dz(i,k)*rho(i,k))+nprci(k)*icldm(i,k)+&
                nnuccr(k)*cldmax(i,k))/((-nsubs(k)-nsagg(k))*cldmax(i,k))*omsm

        nsubs(k) = nsubs(k)*ratio
        nsagg(k) = nsagg(k)*ratio
	end if
        end if






	qvlat(i,k) = qvlat(i,k)- &
       (pre(k)+prds(k))*cldmax(i,k)-cmei(i,k) 

	tlat(i,k) = tlat(i,k)+((pre(k)*cldmax(i,k)) &
           *xxlv+(prds(k)*cldmax(i,k)+cmei(i,k))*xxls+ &
           ((bergs(k)+psacws(k)+mnuccc(k)+mnucct(k)+msacwi(k))*lcldm(i,k)+(mnuccr(k)+ &
               pracs(k))*cldmax(i,k)+berg(i,k))*xlf)

	qctend(i,k) = qctend(i,k)+ &
                 (-pra(k)-prc(k)-mnuccc(k)-mnucct(k)-msacwi(k)- & 
                  psacws(k)-bergs(k))*lcldm(i,k)-berg(i,k)

	qitend(i,k) = qitend(i,k)+ &
         (mnuccc(k)+mnucct(k)+msacwi(k))*lcldm(i,k)+(-prci(k)- & 
                  prai(k))*icldm(i,k)+cmei(i,k)+berg(i,k)

	qrtend(i,k) = qrtend(i,k)+ &
                 (pra(k)+prc(k))*lcldm(i,k)+(pre(k)-pracs(k)- &
                  mnuccr(k))*cldmax(i,k)

	qnitend(i,k) = qnitend(i,k)+ &
                (prai(k)+prci(k))*icldm(i,k)+(psacws(k)+bergs(k))*lcldm(i,k)+(prds(k)+ &
                   pracs(k)+mnuccr(k))*cldmax(i,k)


	cmeiout(i,k) = cmeiout(i,k) + cmei(i,k)




	evapsnow(i,k) = evapsnow(i,k)-prds(k)*cldmax(i,k)
	nevapr(i,k) = nevapr(i,k)-pre(k)*cldmax(i,k)



        	prain(i,k) = prain(i,k)+(pra(k)+prc(k))*lcldm(i,k)+(-pracs(k)- &
                          mnuccr(k))*cldmax(i,k)
	prodsnow(i,k) = prodsnow(i,k)+(prai(k)+prci(k))*icldm(i,k)+(psacws(k)+bergs(k))*lcldm(i,k)+(&
                   pracs(k)+mnuccr(k))*cldmax(i,k)









        qcsinksum_rate1ord(k) = qcsinksum_rate1ord(k) + (pra(k)+prc(k)+psacws(k))*lcldm(i,k) 
        qcsum_rate1ord(k) = qcsum_rate1ord(k) + qc(i,k) 


        prao(i,k)=prao(i,k)+pra(k)*lcldm(i,k)
        prco(i,k)=prco(i,k)+prc(k)*lcldm(i,k)
        mnuccco(i,k)=mnuccco(i,k)+mnuccc(k)*lcldm(i,k)
        mnuccto(i,k)=mnuccto(i,k)+mnucct(k)*lcldm(i,k)
        mnuccdo(i,k)=mnuccdo(i,k)+mnuccd(k)*lcldm(i,k)
        msacwio(i,k)=msacwio(i,k)+msacwi(k)*lcldm(i,k)
        psacwso(i,k)=psacwso(i,k)+psacws(k)*lcldm(i,k)
        bergso(i,k)=bergso(i,k)+bergs(k)*lcldm(i,k)
        bergo(i,k)=bergo(i,k)+berg(i,k)
        prcio(i,k)=prcio(i,k)+prci(k)*icldm(i,k)
        praio(i,k)=praio(i,k)+prai(k)*icldm(i,k)
        mnuccro(i,k)=mnuccro(i,k)+mnuccr(k)*cldmax(i,k)
        pracso (i,k)=pracso (i,k)+pracs (k)*cldmax(i,k)



	nctend(i,k) = nctend(i,k)+ npccn(k)*mtime+&
                  (-nnuccc(k)-nnucct(k)-npsacws(k)+nsubc(k) & 
                  -npra(k)-nprc1(k))*lcldm(i,k)      

	nitend(i,k) = nitend(i,k)+ nnuccd(k)*mtime+ & 
                  (nnucct(k)+nsacwi(k))*lcldm(i,k)+(nsubi(k)-nprci(k)- &   
                  nprai(k))*icldm(i,k)

	nstend(i,k) = nstend(i,k)+(nsubs(k)+ &
                  nsagg(k)+nnuccr(k))*cldmax(i,k)+nprci(k)*icldm(i,k)

	nrtend(i,k) = nrtend(i,k)+ &
                  nprc(k)*lcldm(i,k)+(nsubr(k)-npracs(k)-nnuccr(k) &
                   +nragg(k))*cldmax(i,k)





	if (nctend(i,k).gt.0._r8.and.nc(i,k)+nctend(i,k)*deltat.gt.ncmax) then
	nctend(i,k)=max(0._r8,(ncmax-nc(i,k))/deltat)
	end if
	if (nitend(i,k).gt.0._r8.and.ni(i,k)+nitend(i,k)*deltat.gt.nimax) then
	nitend(i,k)=max(0._r8,(nimax-ni(i,k))/deltat)
	end if







        if (qric(i,k).ge.qsmall) then
	if (k.eq.1) then
	qric(i,k)=qrtend(i,k)*dz(i,k)/cldmax(i,k)/umr(k)
	nric(i,k)=nrtend(i,k)*dz(i,k)/cldmax(i,k)/unr(k)
	else
        qric(i,k) = (rho(i,k-1)*umr(k-1)*qric(i,k-1)*cldmax(i,k-1)+ &
         (rho(i,k)*dz(i,k)*qrtend(i,k)))/(umr(k)*rho(i,k)*cldmax(i,k))
	nric(i,k) = (rho(i,k-1)*unr(k-1)*nric(i,k-1)*cldmax(i,k-1)+ &
         (rho(i,k)*dz(i,k)*nrtend(i,k)))/(unr(k)*rho(i,k)*cldmax(i,k))

	end if
	else
	qric(i,k)=0._r8
	nric(i,k)=0._r8
	end if



        if (qniic(i,k).ge.qsmall) then
	if (k.eq.1) then
	qniic(i,k)=qnitend(i,k)*dz(i,k)/cldmax(i,k)/ums(k)	
	nsic(i,k)=nstend(i,k)*dz(i,k)/cldmax(i,k)/uns(k)
	else
        qniic(i,k) = (rho(i,k-1)*ums(k-1)*qniic(i,k-1)*cldmax(i,k-1)+ &
         (rho(i,k)*dz(i,k)*qnitend(i,k)))/(ums(k)*rho(i,k)*cldmax(i,k))
	nsic(i,k) = (rho(i,k-1)*uns(k-1)*nsic(i,k-1)*cldmax(i,k-1)+ &
         (rho(i,k)*dz(i,k)*nstend(i,k)))/(uns(k)*rho(i,k)*cldmax(i,k))
	end if
	else
	qniic(i,k)=0._r8
	nsic(i,k)=0._r8
	end if




	prect(i) = prect(i)+(qrtend(i,k)*dz(i,k)*rho(i,k)+&
                   qnitend(i,k)*dz(i,k)*rho(i,k))/rhow
	preci(i) = preci(i)+qnitend(i,k)*dz(i,k)*rho(i,k)/rhow

        
 
         rainrt(i,k)=qric(i,k)*rho(i,k)*umr(k)/rhow*3600._r8*1000._r8



	qrtot = max(qrtot+qrtend(i,k)*dz(i,k)*rho(i,k),0._r8)
	qstot = max(qstot+qnitend(i,k)*dz(i,k)*rho(i,k),0._r8)
	nrtot = max(nrtot+nrtend(i,k)*dz(i,k)*rho(i,k),0._r8)
	nstot = max(nstot+nstend(i,k)*dz(i,k)*rho(i,k),0._r8)





        if (t(i,k)+tlat(i,k)/cpp*deltat > 275.15_r8) then
           if (qstot > 0._r8) then


	      dum = -xlf/cpp*qstot/(dz(i,k)*rho(i,k))
	      if (t(i,k)+tlat(i,k)/cpp*deltat+dum.lt.275.15_r8) then
	       dum = (t(i,k)+tlat(i,k)/cpp*deltat-275.15_r8)*cpp/xlf
	       dum = dum/(xlf/cpp*qstot/(dz(i,k)*rho(i,k)))
	       dum = max(0._r8,dum)
	       dum = min(1._r8,dum)
	      else
               dum = 1._r8
              end if

	      qric(i,k)=qric(i,k)+dum*qniic(i,k)
	      nric(i,k)=nric(i,k)+dum*nsic(i,k)
	      qniic(i,k)=(1._r8-dum)*qniic(i,k)
	      nsic(i,k)=(1._r8-dum)*nsic(i,k)

              tmp=-xlf*dum*qstot/(dz(i,k)*rho(i,k))
              meltsdt(i,k)=meltsdt(i,k) + tmp

	      tlat(i,k)=tlat(i,k)+tmp
	      qrtot=qrtot+dum*qstot
	      nrtot=nrtot+dum*nstot
	      qstot=(1._r8-dum)*qstot
	      nstot=(1._r8-dum)*nstot
	      preci(i)=(1._r8-dum)*preci(i)
           end if
        end if



        if (t(i,k)+tlat(i,k)/cpp*deltat < (tmelt - 5._r8)) then

           if (qrtot > 0._r8) then


          dum = xlf/cpp*qrtot/(dz(i,k)*rho(i,k))
          if (t(i,k)+tlat(i,k)/cpp*deltat+dum.gt.(tmelt - 5._r8)) then
           dum = -(t(i,k)+tlat(i,k)/cpp*deltat-(tmelt-5._r8))*cpp/xlf
           dum = dum/(xlf/cpp*qrtot/(dz(i,k)*rho(i,k)))
           dum = max(0._r8,dum)
           dum = min(1._r8,dum)
          else
             dum = 1._r8
          end if
            
	      qniic(i,k)=qniic(i,k)+dum*qric(i,k)
	      nsic(i,k)=nsic(i,k)+dum*nric(i,k)
	      qric(i,k)=(1._r8-dum)*qric(i,k)
	      nric(i,k)=(1._r8-dum)*nric(i,k)

	      tmp = xlf*dum*qrtot/(dz(i,k)*rho(i,k))
	      frzrdt(i,k)=frzrdt(i,k) + tmp

	      tlat(i,k)=tlat(i,k)+tmp
	      qstot=qstot+dum*qrtot
	      qrtot=(1._r8-dum)*qrtot
	      nstot=nstot+dum*nrtot
	      nrtot=(1._r8-dum)*nrtot
	      preci(i)=preci(i)+dum*(prect(i)-preci(i))
           end if
        end if



	if (qniic(i,k).lt.qsmall) then
	qniic(i,k)=0._r8
	nsic(i,k)=0._r8
	end if

	if (qric(i,k).lt.qsmall) then
	qric(i,k)=0._r8
	nric(i,k)=0._r8
	end if




	nric(i,k)=max(nric(i,k),0._r8)
	nsic(i,k)=max(nsic(i,k),0._r8)






	if (qric(i,k).ge.qsmall) then
	lamr(k) = (pi*rhow*nric(i,k)/qric(i,k))**(1._r8/3._r8)
	n0r(k) = nric(i,k)*lamr(k)





	if (lamr(k).lt.lamminr) then

	lamr(k) = lamminr

	n0r(k) = lamr(k)**4*qric(i,k)/(pi*rhow)
	nric(i,k) = n0r(k)/lamr(k)
	else if (lamr(k).gt.lammaxr) then
	lamr(k) = lammaxr
	n0r(k) = lamr(k)**4*qric(i,k)/(pi*rhow)
	nric(i,k) = n0r(k)/lamr(k)
	end if




        unr(k) = min(arn(i,k)*cons4/lamr(k)**br,9.1_r8*rhof(i,k))
        umr(k) = min(arn(i,k)*cons5/(6._r8*lamr(k)**br),9.1_r8*rhof(i,k))

	else
	lamr(k) = 0._r8
	n0r(k) = 0._r8
	umr(k)=0._r8
	unr(k)=0._r8
	end if



        if (lamr(k).gt.0._r8) then
           Artmp = n0r(k) * pi / (2 * lamr(k)**3._r8)
        else 
           Artmp = 0._r8
        endif

        if (lamc(k).gt.0._r8) then
           Actmp = cdist1(k) * pi * gamma(pgam(k)+3._r8)/(4._r8 * lamc(k)**2._r8)
        else 
           Actmp = 0._r8
        endif

        if (Actmp.gt.0_r8.or.Artmp.gt.0) then
           rercld(i,k)=rercld(i,k) + 3._r8 *(qric(i,k) + qcic(i,k)) / (4._r8 * rhow * (Actmp + Artmp))
           arcld(i,k)=arcld(i,k)+1._r8
        endif




	if (qniic(i,k).ge.qsmall) then
	lams(k) = (cons6*cs*nsic(i,k)/ &
            qniic(i,k))**(1._r8/ds)
	n0s(k) = nsic(i,k)*lams(k)




	if (lams(k).lt.lammins) then
	lams(k) = lammins
	n0s(k) = lams(k)**(ds+1._r8)*qniic(i,k)/(cs*cons6)
	nsic(i,k) = n0s(k)/lams(k)

	else if (lams(k).gt.lammaxs) then
	lams(k) = lammaxs
	n0s(k) = lams(k)**(ds+1._r8)*qniic(i,k)/(cs*cons6)
	nsic(i,k) = n0s(k)/lams(k)
	end if



        ums(k) = min(asn(i,k)*cons8/(6._r8*lams(k)**bs),1.2_r8*rhof(i,k))
        uns(k) = min(asn(i,k)*cons7/lams(k)**bs,1.2_r8*rhof(i,k))

	else
	lams(k) = 0._r8
	n0s(k) = 0._r8
	ums(k) = 0._r8
	uns(k) = 0._r8
	end if







	qrout(i,k)=qrout(i,k)+qric(i,k)*cldmax(i,k)
	qsout(i,k)=qsout(i,k)+qniic(i,k)*cldmax(i,k)
	nrout(i,k)=nrout(i,k)+nric(i,k)*rho(i,k)*cldmax(i,k)
	nsout(i,k)=nsout(i,k)+nsic(i,k)*rho(i,k)*cldmax(i,k)

	tlat1(i,k)=tlat1(i,k)+tlat(i,k)
	qvlat1(i,k)=qvlat1(i,k)+qvlat(i,k)
	qctend1(i,k)=qctend1(i,k)+qctend(i,k)
	qitend1(i,k)=qitend1(i,k)+qitend(i,k)
	nctend1(i,k)=nctend1(i,k)+nctend(i,k)
	nitend1(i,k)=nitend1(i,k)+nitend(i,k)

	t(i,k)=t(i,k)+tlat(i,k)*deltat/cpp
	q(i,k)=q(i,k)+qvlat(i,k)*deltat
	qc(i,k)=qc(i,k)+qctend(i,k)*deltat
	qi(i,k)=qi(i,k)+qitend(i,k)*deltat
	nc(i,k)=nc(i,k)+nctend(i,k)*deltat
	ni(i,k)=ni(i,k)+nitend(i,k)*deltat

        rainrt1(i,k)=rainrt1(i,k)+rainrt(i,k)


        if (arcld(i,k) .gt. 0._r8) then
           rercld(i,k)=rercld(i,k)/arcld(i,k)
        end if


      
      	rflx(i,1)=0.0_r8
      	sflx(i,1)=0.0_r8
	
      
      	rflx(i,k+1)=qrout(i,k)*rho(i,k)*umr(k)
	sflx(i,k+1)=qsout(i,k)*rho(i,k)*ums(k)
	
      
	rflx1(i,k+1)=rflx1(i,k+1)+rflx(i,k+1)
	sflx1(i,k+1)=sflx1(i,k+1)+sflx(i,k+1)	



	end do 

	prect1(i)=prect1(i)+prect(i)
	preci1(i)=preci1(i)+preci(i)

	end do 

        do k = 1, pver               
        rate1ord_cw2pr_st(i,k) = qcsinksum_rate1ord(k)/max(qcsum_rate1ord(k),1.0e-30_r8) 
        end do                       

 300    continue  
	end do 


	deltat=deltat*real(iter)



        do i=1,ncol


	if (ltrue(i).eq.0) then

	do k=1,pver

	effc(i,k)=10._r8
	effi(i,k)=25._r8
	effc_fn(i,k)=10._r8
        lamcrad(i,k)=0._r8
        pgamrad(i,k)=0._r8
        deffi(i,k)=0._r8
	end do
	goto 500
	end if


	nstep = 1



	prect(i)=prect1(i)/real(iter)
	preci(i)=preci1(i)/real(iter)

       do k=1,pver



	t(i,k)=t1(i,k)
	q(i,k)=q1(i,k)
	qc(i,k)=qc1(i,k)
	qi(i,k)=qi1(i,k)
	nc(i,k)=nc1(i,k)
	ni(i,k)=ni1(i,k)



	tlat(i,k)=tlat1(i,k)/real(iter)
	qvlat(i,k)=qvlat1(i,k)/real(iter)
	qctend(i,k)=qctend1(i,k)/real(iter)
	qitend(i,k)=qitend1(i,k)/real(iter)
	nctend(i,k)=nctend1(i,k)/real(iter)
	nitend(i,k)=nitend1(i,k)/real(iter)
 
        rainrt(i,k)=rainrt1(i,k)/real(iter)


	rflx(i,k+1)=rflx1(i,k+1)/real(iter)
	sflx(i,k+1)=sflx1(i,k+1)/real(iter)



	qrout(i,k)=qrout(i,k)/real(iter)
	qsout(i,k)=qsout(i,k)/real(iter)
	nrout(i,k)=nrout(i,k)/real(iter)
	nsout(i,k)=nsout(i,k)/real(iter)



	nevapr(i,k) = nevapr(i,k)/real(iter)
	evapsnow(i,k) = evapsnow(i,k)/real(iter)
	prain(i,k) = prain(i,k)/real(iter)
	prodsnow(i,k) = prodsnow(i,k)/real(iter)
	cmeout(i,k) = cmeout(i,k)/real(iter)

	cmeiout(i,k) = cmeiout(i,k)/real(iter)
        meltsdt(i,k) = meltsdt(i,k)/real(iter)
        frzrdt (i,k) = frzrdt (i,k)/real(iter)



        prao(i,k)=prao(i,k)/real(iter)
        prco(i,k)=prco(i,k)/real(iter)
        mnuccco(i,k)=mnuccco(i,k)/real(iter)
        mnuccto(i,k)=mnuccto(i,k)/real(iter)
        msacwio(i,k)=msacwio(i,k)/real(iter)
        psacwso(i,k)=psacwso(i,k)/real(iter)
        bergso(i,k)=bergso(i,k)/real(iter)
        bergo(i,k)=bergo(i,k)/real(iter)
        prcio(i,k)=prcio(i,k)/real(iter)
        praio(i,k)=praio(i,k)/real(iter)

        mnuccro(i,k)=mnuccro(i,k)/real(iter)
        pracso (i,k)=pracso (i,k)/real(iter)

        mnuccdo(i,k)=mnuccdo(i,k)/real(iter)


        nevapr(i,k) = nevapr(i,k) + evapsnow(i,k)
        prain(i,k) = prain(i,k) + prodsnow(i,k)









        dumc(i,k) = (qc(i,k)+qctend(i,k)*deltat)/lcldm(i,k)
        dumi(i,k) = (qi(i,k)+qitend(i,k)*deltat)/icldm(i,k)
        dumnc(i,k) = max((nc(i,k)+nctend(i,k)*deltat)/lcldm(i,k),0._r8)
        dumni(i,k) = max((ni(i,k)+nitend(i,k)*deltat)/icldm(i,k),0._r8)



	if (dumi(i,k).ge.qsmall) then

	dumni(i,k)=min(dumni(i,k),dumi(i,k)*1.e20_r8)

	lami(k) = (cons1*ci* &
              dumni(i,k)/dumi(i,k))**(1._r8/di)
        lami(k)=max(lami(k),lammini)
        lami(k)=min(lami(k),lammaxi)
        else
           lami(k)=0._r8
        end if

	if (dumc(i,k).ge.qsmall) then

	dumnc(i,k)=min(dumnc(i,k),dumc(i,k)*1.e20_r8)

         dumnc(i,k)=max(dumnc(i,k),cdnl/rho(i,k)) 
         pgam(k)=0.0005714_r8*(ncic(i,k)/1.e6_r8*rho(i,k))+0.2714_r8
         pgam(k)=1._r8/(pgam(k)**2)-1._r8
         pgam(k)=max(pgam(k),2._r8)
         pgam(k)=min(pgam(k),15._r8)

	lamc(k) = (pi/6._r8*rhow*dumnc(i,k)*gamma(pgam(k)+4._r8)/ &
                 (dumc(i,k)*gamma(pgam(k)+1._r8)))**(1._r8/3._r8)
	lammin = (pgam(k)+1._r8)/50.e-6_r8
	lammax = (pgam(k)+1._r8)/2.e-6_r8
        lamc(k)=max(lamc(k),lammin)
        lamc(k)=min(lamc(k),lammax)
        else
           lamc(k)=0._r8
        end if





	if (dumc(i,k).ge.qsmall) then
	unc = &
              acn(i,k)*gamma(1._r8+bc+pgam(k))/ &
               (lamc(k)**bc*gamma(pgam(k)+1._r8))
	umc = &
              acn(i,k)*gamma(4._r8+bc+pgam(k))/ &
             (lamc(k)**bc*gamma(pgam(k)+4._r8))

	vtrmc(i,k)=umc
	else
	umc = 0._r8
	unc = 0._r8
	end if



	if (dumi(i,k).ge.qsmall) then
	uni =  ain(i,k)*cons16/lami(k)**bi
	umi = ain(i,k)*cons17/(6._r8*lami(k)**bi)
        uni=min(uni,1.2_r8*rhof(i,k))
        umi=min(umi,1.2_r8*rhof(i,k))


	vtrmi(i,k)=umi
	else
	umi = 0._r8
	uni = 0._r8
	end if

	fi(k) = g*rho(i,k)*umi
	fni(k) = g*rho(i,k)*uni
	fc(k) = g*rho(i,k)*umc
	fnc(k) = g*rho(i,k)*unc




	rgvm = max(fi(k),fc(k),fni(k),fnc(k))
	nstep = max(int(rgvm*deltat/pdel(i,k)+1._r8),nstep)




        dumc(i,k) = (qc(i,k)+qctend(i,k)*deltat)
        dumi(i,k) = (qi(i,k)+qitend(i,k)*deltat)
        dumnc(i,k) = max((nc(i,k)+nctend(i,k)*deltat),0._r8)
        dumni(i,k) = max((ni(i,k)+nitend(i,k)*deltat),0._r8)

	if (dumc(i,k).lt.qsmall) dumnc(i,k)=0._r8
	if (dumi(i,k).lt.qsmall) dumni(i,k)=0._r8

	end do       

	do n = 1,nstep  
	
	do k = 1,pver
	falouti(k) = fi(k)*dumi(i,k)
	faloutni(k) = fni(k)*dumni(i,k)
	faloutc(k) = fc(k)*dumc(i,k)
	faloutnc(k) = fnc(k)*dumnc(i,k)
	end do



	k = 1
	faltndi = falouti(k)/pdel(i,k)
	faltndni = faloutni(k)/pdel(i,k)
	faltndc = faloutc(k)/pdel(i,k)
        faltndnc = faloutnc(k)/pdel(i,k)



	qitend(i,k) = qitend(i,k)-faltndi/nstep
	nitend(i,k) = nitend(i,k)-faltndni/nstep
	qctend(i,k) = qctend(i,k)-faltndc/nstep
        nctend(i,k) = nctend(i,k)-faltndnc/nstep


	qcsedten(i,k)=qcsedten(i,k)-faltndc/nstep
	qisedten(i,k)=qisedten(i,k)-faltndi/nstep

	dumi(i,k) = dumi(i,k)-faltndi*deltat/nstep
	dumni(i,k) = dumni(i,k)-faltndni*deltat/nstep
	dumc(i,k) = dumc(i,k)-faltndc*deltat/nstep
        dumnc(i,k) = dumnc(i,k)-faltndnc*deltat/nstep

	do k = 2,pver






        dum=lcldm(i,k)/lcldm(i,k-1)
	dum=min(dum,1._r8)
        dum1=icldm(i,k)/icldm(i,k-1)
	dum1=min(dum1,1._r8)

	faltndqie=(falouti(k)-falouti(k-1))/pdel(i,k)
	faltndi=(falouti(k)-dum1*falouti(k-1))/pdel(i,k)
	faltndni=(faloutni(k)-dum1*faloutni(k-1))/pdel(i,k)
	faltndqce=(faloutc(k)-faloutc(k-1))/pdel(i,k)
	faltndc=(faloutc(k)-dum*faloutc(k-1))/pdel(i,k)
	faltndnc=(faloutnc(k)-dum*faloutnc(k-1))/pdel(i,k)



	qitend(i,k) = qitend(i,k)-faltndi/nstep
	nitend(i,k) = nitend(i,k)-faltndni/nstep
	qctend(i,k) = qctend(i,k)-faltndc/nstep
        nctend(i,k) = nctend(i,k)-faltndnc/nstep


	qcsedten(i,k)=qcsedten(i,k)-faltndc/nstep
	qisedten(i,k)=qisedten(i,k)-faltndi/nstep
	


        qvlat(i,k)=qvlat(i,k)-(faltndqie-faltndi)/nstep

        qisevap(i,k)=qisevap(i,k)-(faltndqie-faltndi)/nstep
        qvlat(i,k)=qvlat(i,k)-(faltndqce-faltndc)/nstep

        qcsevap(i,k)=qcsevap(i,k)-(faltndqce-faltndc)/nstep

	tlat(i,k)=tlat(i,k)+(faltndqie-faltndi)*xxls/nstep
	tlat(i,k)=tlat(i,k)+(faltndqce-faltndc)*xxlv/nstep

	dumi(i,k) = dumi(i,k)-faltndi*deltat/nstep
	dumni(i,k) = dumni(i,k)-faltndni*deltat/nstep
	dumc(i,k) = dumc(i,k)-faltndc*deltat/nstep
        dumnc(i,k) = dumnc(i,k)-faltndnc*deltat/nstep

        Fni(K)=MAX(Fni(K)/pdel(i,K),Fni(K-1)/pdel(i,K-1))*pdel(i,K)
        FI(K)=MAX(FI(K)/pdel(i,K),FI(K-1)/pdel(i,K-1))*pdel(i,K)
        fnc(k)=max(fnc(k)/pdel(i,k),fnc(k-1)/pdel(i,k-1))*pdel(i,k)
        Fc(K)=MAX(Fc(K)/pdel(i,K),Fc(K-1)/pdel(i,K-1))*pdel(i,K)

	end do   






	  prect(i) = prect(i)+(faloutc(pver)+falouti(pver)) &
                     /g/nstep/1000._r8	
	  preci(i) = preci(i)+(falouti(pver)) &
                     /g/nstep/1000._r8

        end do   







  do k=1,pver	

        dumc(i,k) = max(qc(i,k)+qctend(i,k)*deltat,0._r8)
        dumi(i,k) = max(qi(i,k)+qitend(i,k)*deltat,0._r8)
        dumnc(i,k) = max(nc(i,k)+nctend(i,k)*deltat,0._r8)
        dumni(i,k) = max(ni(i,k)+nitend(i,k)*deltat,0._r8)

	if (dumc(i,k).lt.qsmall) dumnc(i,k)=0._r8
	if (dumi(i,k).lt.qsmall) dumni(i,k)=0._r8
	


        if (t(i,k)+tlat(i,k)/cpp*deltat > tmelt) then
           if (dumi(i,k) > 0._r8) then


	      dum = -dumi(i,k)*xlf/cpp
	      if (t(i,k)+tlat(i,k)/cpp*deltat+dum.lt.tmelt) then
	       dum = (t(i,k)+tlat(i,k)/cpp*deltat-tmelt)*cpp/xlf
	       dum = dum/dumi(i,k)*xlf/cpp 
	       dum = max(0._r8,dum)
	       dum = min(1._r8,dum)
	      else
	       dum = 1._r8
	      end if

              qctend(i,k)=qctend(i,k)+dum*dumi(i,k)/deltat


              melto(i,k)=dum*dumi(i,k)/deltat




	      nctend(i,k)=nctend(i,k)+3._r8*dum*dumi(i,k)/deltat/ &
               (4._r8*pi*5.12e-16_r8*rhow)

              qitend(i,k)=((1._r8-dum)*dumi(i,k)-qi(i,k))/deltat
              nitend(i,k)=((1._r8-dum)*dumni(i,k)-ni(i,k))/deltat
              tlat(i,k)=tlat(i,k)-xlf*dum*dumi(i,k)/deltat
           end if
        end if



        if (t(i,k)+tlat(i,k)/cpp*deltat < 233.15_r8) then
           if (dumc(i,k) > 0._r8) then


	      dum = dumc(i,k)*xlf/cpp
	      if (t(i,k)+tlat(i,k)/cpp*deltat+dum.gt.233.15_r8) then
	       dum = -(t(i,k)+tlat(i,k)/cpp*deltat-233.15_r8)*cpp/xlf
	       dum = dum/dumc(i,k)*xlf/cpp
	       dum = max(0._r8,dum)
	       dum = min(1._r8,dum)
	      else
	       dum = 1._r8
	      end if

              qitend(i,k)=qitend(i,k)+dum*dumc(i,k)/deltat

              homoo(i,k)=dum*dumc(i,k)/deltat



	      nitend(i,k)=nitend(i,k)+dum*3._r8*dumc(i,k)/(4._r8*3.14_r8*1.563e-14_r8* &
                 500._r8)/deltat
              qctend(i,k)=((1._r8-dum)*dumc(i,k)-qc(i,k))/deltat
              nctend(i,k)=((1._r8-dum)*dumnc(i,k)-nc(i,k))/deltat
              tlat(i,k)=tlat(i,k)+xlf*dum*dumc(i,k)/deltat
           end if
        end if





	qtmp=q(i,k)+qvlat(i,k)*deltat
	ttmp=t(i,k)+tlat(i,k)/cpp*deltat

        esn = polysvp(ttmp,0)  
	qsn = min(epsqs*esn/(p(i,k)-(1._r8-epsqs)*esn),1._r8)

	if (qtmp > qsn .and. qsn > 0) then

	dum = (qtmp-qsn)/(1._r8+cons27*qsn/(cpp*rv*ttmp**2))/deltat

	cmeout(i,k) = cmeout(i,k)+dum

           if (ttmp > 268.15_r8) then
              dum1=0.0_r8

           else if (ttmp < 238.15_r8) then
              dum1=1.0_r8
           else
              dum1=(268.15_r8-ttmp)/30._r8
	   end if  

	dum = (qtmp-qsn)/(1._r8+(xxls*dum1+xxlv*(1._r8-dum1))**2 &
                     *qsn/(cpp*rv*ttmp**2))/deltat
	qctend(i,k)=qctend(i,k)+dum*(1._r8-dum1)

        qcreso(i,k)=dum*(1._r8-dum1)
	qitend(i,k)=qitend(i,k)+dum*dum1
	qireso(i,k)=dum*dum1
	qvlat(i,k)=qvlat(i,k)-dum

        qvres(i,k)=-dum
	tlat(i,k)=tlat(i,k)+dum*(1._r8-dum1)*xxlv+dum*dum1*xxls
	end if









        dumc(i,k) = max(qc(i,k)+qctend(i,k)*deltat,0._r8)/lcldm(i,k)
        dumi(i,k) = max(qi(i,k)+qitend(i,k)*deltat,0._r8)/icldm(i,k)
        dumnc(i,k) = max(nc(i,k)+nctend(i,k)*deltat,0._r8)/lcldm(i,k)
        dumni(i,k) = max(ni(i,k)+nitend(i,k)*deltat,0._r8)/icldm(i,k)



	dumc(i,k)=min(dumc(i,k),5.e-3_r8)
	dumi(i,k)=min(dumi(i,k),5.e-3_r8)




	if (dumi(i,k).ge.qsmall) then

	dumni(i,k)=min(dumni(i,k),dumi(i,k)*1.e20_r8)
	lami(k) = (cons1*ci* &
              dumni(i,k)/dumi(i,k))**(1._r8/di)

	if (lami(k).lt.lammini) then
	lami(k) = lammini
	n0i(k) = lami(k)**(di+1._r8)*dumi(i,k)/(ci*cons1)
	niic(i,k) = n0i(k)/lami(k)

	nitend(i,k)=(niic(i,k)*icldm(i,k)-ni(i,k))/deltat
	else if (lami(k).gt.lammaxi) then
	lami(k) = lammaxi
	n0i(k) = lami(k)**(di+1._r8)*dumi(i,k)/(ci*cons1)
	niic(i,k) = n0i(k)/lami(k)

	nitend(i,k)=(niic(i,k)*icldm(i,k)-ni(i,k))/deltat
	end if
	   effi(i,k) = 1.5_r8/lami(k)*1.e6_r8

        else
	   effi(i,k) = 25._r8
        end if




	if (dumc(i,k).ge.qsmall) then


           dumnc(i,k)=min(dumnc(i,k),dumc(i,k)*1.e20_r8)





           if (dumnc(i,k).lt.cdnl/rho(i,k)) then   
              nctend(i,k)=(cdnl/rho(i,k)*lcldm(i,k)-nc(i,k))/deltat   
           end if
           dumnc(i,k)=max(dumnc(i,k),cdnl/rho(i,k)) 

           pgam(k)=0.0005714_r8*(ncic(i,k)/1.e6_r8*rho(i,k))+0.2714_r8
           pgam(k)=1._r8/(pgam(k)**2)-1._r8
           pgam(k)=max(pgam(k),2._r8)
           pgam(k)=min(pgam(k),15._r8)
           
           lamc(k) = (pi/6._r8*rhow*dumnc(i,k)*gamma(pgam(k)+4._r8)/ &
                     (dumc(i,k)*gamma(pgam(k)+1._r8)))**(1._r8/3._r8)
           lammin = (pgam(k)+1._r8)/50.e-6_r8
           lammax = (pgam(k)+1._r8)/2.e-6_r8
           if (lamc(k).lt.lammin) then
              lamc(k) = lammin
              ncic(i,k) = 6._r8*lamc(k)**3*dumc(i,k)* &
                   gamma(pgam(k)+1._r8)/ &
                   (pi*rhow*gamma(pgam(k)+4._r8))

              nctend(i,k)=(ncic(i,k)*lcldm(i,k)-nc(i,k))/deltat

           else if (lamc(k).gt.lammax) then
              lamc(k) = lammax
              ncic(i,k) = 6._r8*lamc(k)**3*dumc(i,k)* &
                   gamma(pgam(k)+1._r8)/ &
                   (pi*rhow*gamma(pgam(k)+4._r8))

              nctend(i,k)=(ncic(i,k)*lcldm(i,k)-nc(i,k))/deltat
	end if

        effc(i,k) = &
             gamma(pgam(k)+4._r8)/ &
             gamma(pgam(k)+3._r8)/lamc(k)/2._r8*1.e6_r8

        lamcrad(i,k)=lamc(k)
        pgamrad(i,k)=pgam(k)

        else
           effc(i,k) = 10._r8
           lamcrad(i,k)=0._r8
           pgamrad(i,k)=0._r8
        end if


        deffi(i,k)=effi(i,k)*rhoi/917._r8*2._r8






	dumnc(i,k)=1.e8

	if (dumc(i,k).ge.qsmall) then
         pgam(k)=0.0005714_r8*(ncic(i,k)/1.e6_r8*rho(i,k))+0.2714_r8
         pgam(k)=1._r8/(pgam(k)**2)-1._r8
         pgam(k)=max(pgam(k),2._r8)
         pgam(k)=min(pgam(k),15._r8)

	lamc(k) = (pi/6._r8*rhow*dumnc(i,k)*gamma(pgam(k)+4._r8)/ &
                 (dumc(i,k)*gamma(pgam(k)+1._r8)))**(1._r8/3._r8)
	lammin = (pgam(k)+1._r8)/50.e-6_r8
	lammax = (pgam(k)+1._r8)/2.e-6_r8
	if (lamc(k).lt.lammin) then
	lamc(k) = lammin
	else if (lamc(k).gt.lammax) then
	lamc(k) = lammax
	end if
	effc_fn(i,k) = &
             gamma(pgam(k)+4._r8)/ &
             gamma(pgam(k)+3._r8)/lamc(k)/2._r8*1.e6_r8

        else
	effc_fn(i,k) = 10._r8
        end if




	end do 

 500    continue

	do k=1,pver


        if (qc(i,k)+qctend(i,k)*deltat.lt.qsmall) nctend(i,k)=-nc(i,k)/deltat
        if (qi(i,k)+qitend(i,k)*deltat.lt.qsmall) nitend(i,k)=-ni(i,k)/deltat
	end do

	end do 




      call outfld('QRAIN',qrout,   pcols, lchnk)
      call outfld('QSNOW',qsout,   pcols, lchnk)
      call outfld('NRAIN',nrout,   pcols, lchnk)
      call outfld('NSNOW',nsout,   pcols, lchnk)


      do i = 1,ncol
         do k=1,pver
            if (qsout(i,k).gt.1.e-7_r8.and.nsout(i,k).gt.0._r8) then
               dsout(i,k)=3._r8*rhosn/917._r8*(pi * rhosn * nsout(i,k)/qsout(i,k))**(-1._r8/3._r8)
            endif
         end do
      end do
      call outfld('DSNOW',dsout,   pcols, lchnk)
 

      do i = 1,ncol
         do k=1,pver
	   
            if (qrout(i,k).gt.1.e-7_r8.and.nrout(i,k).gt.0._r8) then
		reff_rain(i,k)=1.5_r8*(pi * rhow * nrout(i,k)/qrout(i,k))**(-1._r8/3._r8)*1.e6_r8 
            endif
	    
            if (qsout(i,k).gt.1.e-7_r8.and.nsout(i,k).gt.0._r8) then
		reff_snow(i,k)=1.5_r8*(pi * rhosn * nsout(i,k)/qsout(i,k))**(-1._r8/3._r8)*1.e6_r8 

            endif
         end do
      end do

      
      
      
      
 
      do i = 1,ncol
         do k=1,pver
            if (qc(i,k)+qctend(i,k)*deltat.ge.qsmall) then
               dum=((qc(i,k)+qctend(i,k)*deltat)/lcldm(i,k)*rho(i,k)*1000._r8)**2 &
                  /(0.109_r8*(nc(i,k)+nctend(i,k)*deltat)/lcldm(i,k)*rho(i,k)/1.e6_r8)*lcldm(i,k)/cldmax(i,k)
            else
               dum=0._r8
            end if
            if (qi(i,k)+qitend(i,k)*deltat.ge.qsmall) then
               dum1=((qi(i,k)+qitend(i,k)*deltat)*rho(i,k)/icldm(i,k)*1000._r8/0.1_r8)**(1._r8/0.63_r8)*icldm(i,k)/cldmax(i,k)
            else 
               dum1=0._r8
            end if
         
            if (qsout(i,k).ge.qsmall) then
               dum1=dum1+(qsout(i,k)*rho(i,k)*1000._r8/0.1_r8)**(1._r8/0.63_r8)
            end if
            
            refl(i,k)=dum+dum1
 
            
            
            
            
 
            if (rainrt(i,k).ge.0.001) then
               dum=log10(rainrt(i,k)**6._r8)+16._r8
 
               
 
               dum = 10._r8**(dum/10._r8)
            else
               
               dum=0.
            end if
 
            
 
            refl(i,k)=refl(i,k)+dum
 
            
            areflz(i,k)=refl(i,k)
 
            
 
            if (refl(i,k).gt.minrefl) then 
               refl(i,k)=10._r8*log10(refl(i,k))
            else	
               refl(i,k)=-9999._r8
            end if
  
            
            if (refl(i,k).gt.mindbz) then 
               arefl(i,k)=refl(i,k)
               frefl(i,k)=1.0_r8  
            else
               arefl(i,k)=0._r8
               areflz(i,k)=0._r8
               frefl(i,k)=0._r8
            end if
 
            
 
            csrfl(i,k)=min(csmax,refl(i,k))
 
            
            if (csrfl(i,k).gt.csmin) then 
               acsrfl(i,k)=refl(i,k)
               fcsrfl(i,k)=1.0_r8  
            else
               acsrfl(i,k)=0._r8
               fcsrfl(i,k)=0._r8
            end if
 
         end do
      end do
       
      call outfld('REFL',refl,   pcols, lchnk)
      call outfld('AREFL',arefl,   pcols, lchnk)
      call outfld('AREFLZ',areflz,   pcols, lchnk)
      call outfld('FREFL',frefl,   pcols, lchnk)
      call outfld('CSRFL',csrfl,   pcols, lchnk)
      call outfld('ACSRFL',acsrfl,   pcols, lchnk)
      call outfld('FCSRFL',fcsrfl,   pcols, lchnk)

      call outfld('RERCLD',rercld,   pcols, lchnk)



  qrout2(:,:)=0._r8
  qsout2(:,:)=0._r8
  nrout2(:,:)=0._r8
  nsout2(:,:)=0._r8	
  drout2(:,:)=0._r8
  dsout2(:,:)=0._r8	
  freqs(:,:)=0._r8
  freqr(:,:)=0._r8
  do i = 1,ncol
     do k=1,pver
	if (qrout(i,k).gt.1.e-7_r8.and.nrout(i,k).gt.0._r8) then
 	   qrout2(i,k)=qrout(i,k)
	   nrout2(i,k)=nrout(i,k)
	   drout2(i,k)=(pi * rhow * nrout(i,k)/qrout(i,k))**(-1._r8/3._r8)
	   freqr(i,k)=1._r8
	endif
	if (qsout(i,k).gt.1.e-7_r8.and.nsout(i,k).gt.0._r8) then
 	   qsout2(i,k)=qsout(i,k)
	   nsout2(i,k)=nsout(i,k)
	   dsout2(i,k)=(pi * rhosn * nsout(i,k)/qsout(i,k))**(-1._r8/3._r8)
	   freqs(i,k)=1._r8
	endif
     end do
  end do


  do i = 1,ncol
     do k=1,pver
        ncai(i,k)=dum2i(i,k)*rho(i,k)
        ncal(i,k)=dum2l(i,k)*rho(i,k)
     end do
  end do
  call outfld('NCAL',ncal,    pcols,lchnk)
  call outfld('NCAI',ncai,    pcols,lchnk)


  call outfld('AQRAIN',qrout2,    pcols,lchnk)
  call outfld('AQSNOW',qsout2,    pcols,lchnk)
  call outfld('ANRAIN',nrout2,    pcols,lchnk)
  call outfld('ANSNOW',nsout2,    pcols,lchnk)
  call outfld('ADRAIN',drout2,    pcols,lchnk)
  call outfld('ADSNOW',dsout2,    pcols,lchnk)
  call outfld('FREQR',freqr,    pcols,lchnk)
  call outfld('FREQS',freqs,    pcols,lchnk)


	nfice(:,:)=0._r8
	do k=1,pver
	   do i=1,ncol
        	dumc(i,k) = (qc(i,k)+qctend(i,k)*deltat)
        	dumi(i,k) = (qi(i,k)+qitend(i,k)*deltat)
		dumfice=qsout(i,k) + qrout(i,k) + dumc(i,k) + dumi(i,k)  

                if (dumfice.gt.qsmall.and.(qsout(i,k)+dumi(i,k).gt.qsmall)) then
                  nfice(i,k)=(qsout(i,k) + dumi(i,k))/dumfice
                endif

                if (nfice(i,k).gt.1._r8) then
                   nfice(i,k)=1._r8
                endif

	   enddo
	enddo

      call outfld('FICE',nfice,   pcols, lchnk)

return
end subroutine mmicro_pcond




      FUNCTION GAMMA(X)





























































































      INTEGER I,N
      LOGICAL PARITY

      real(r8) gamma
      REAL(r8) &

         C,CONV,EPS,FACT,HALF,ONE,P,PI,Q,RES,SQRTPI,SUM,TWELVE, &
         TWO,X,XBIG,XDEN,XINF,XMININ,XNUM,Y,Y1,YSQ,Z,ZERO
      DIMENSION C(7),P(8),Q(8)



      DATA ONE,HALF,TWELVE,TWO,ZERO/1.0E0_r8,0.5E0_r8,12.0E0_r8,2.0E0_r8,0.0E0_r8/, &
          SQRTPI/0.9189385332046727417803297E0_r8/, &
          PI/3.1415926535897932384626434E0_r8/






      DATA XBIG,XMININ,EPS/35.040E0_r8,1.18E-38_r8,1.19E-7_r8/, &
          XINF/3.4E38_r8/






      DATA P/-1.71618513886549492533811E+0_r8,2.47656508055759199108314E+1_r8,&
            -3.79804256470945635097577E+2_r8,6.29331155312818442661052E+2_r8,&
            8.66966202790413211295064E+2_r8,-3.14512729688483675254357E+4_r8,&
            -3.61444134186911729807069E+4_r8,6.64561438202405440627855E+4_r8/
      DATA Q/-3.08402300119738975254353E+1_r8,3.15350626979604161529144E+2_r8,&
           -1.01515636749021914166146E+3_r8,-3.10777167157231109440444E+3_r8,&
             2.25381184209801510330112E+4_r8,4.75584627752788110767815E+3_r8,&
           -1.34659959864969306392456E+5_r8,-1.15132259675553483497211E+5_r8/











      DATA C/-1.910444077728E-03_r8,8.4171387781295E-04_r8, &
          -5.952379913043012E-04_r8,7.93650793500350248E-04_r8,&
          -2.777777777777681622553E-03_r8,8.333333333333333331554247E-02_r8,&
           5.7083835261E-03_r8/







      CONV(I) = REAL(I,r8)

      PARITY=.FALSE.
      FACT=ONE
      N=0
      Y=X
      IF(Y.LE.ZERO)THEN



        Y=-X
        Y1=AINT(Y)
        RES=Y-Y1
        IF(RES.NE.ZERO)THEN
          IF(Y1.NE.AINT(Y1*HALF)*TWO)PARITY=.TRUE.
          FACT=-PI/SIN(PI*RES)
          Y=Y+ONE
        ELSE
          RES=XINF
          GOTO 900
        ENDIF
      ENDIF



      IF(Y.LT.EPS)THEN



        IF(Y.GE.XMININ)THEN
          RES=ONE/Y
        ELSE
          RES=XINF
          GOTO 900
        ENDIF
      ELSEIF(Y.LT.TWELVE)THEN
        Y1=Y
        IF(Y.LT.ONE)THEN



          Z=Y
          Y=Y+ONE
        ELSE



          N=INT(Y)-1
          Y=Y-CONV(N)
          Z=Y-ONE
        ENDIF



        XNUM=ZERO
        XDEN=ONE
        DO 260 I=1,8
          XNUM=(XNUM+P(I))*Z
          XDEN=XDEN*Z+Q(I)
  260   CONTINUE
        RES=XNUM/XDEN+ONE
        IF(Y1.LT.Y)THEN



          RES=RES/Y1
        ELSEIF(Y1.GT.Y)THEN



          DO 290 I=1,N
            RES=RES*Y
            Y=Y+ONE
  290     CONTINUE
        ENDIF
      ELSE



        IF(Y.LE.XBIG)THEN
          YSQ=Y*Y
          SUM=C(7)
          DO 350 I=1,6
            SUM=SUM/YSQ+C(I)
  350     CONTINUE
          SUM=SUM/Y-Y+SQRTPI
          SUM=SUM+(Y-HALF)*LOG(Y)
          RES=EXP(SUM)
        ELSE
          RES=XINF
          GOTO 900
        ENDIF
      ENDIF



      IF(PARITY)RES=-RES
      IF(FACT.NE.ONE)RES=FACT/RES
  900 GAMMA=RES

      RETURN

      END function gamma


end module cldwat2m_micro
