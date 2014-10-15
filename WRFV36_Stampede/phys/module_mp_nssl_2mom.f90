

















































MODULE module_mp_nssl_2mom

  IMPLICIT NONE
  
  public nssl_2mom_driver
  public nssl_2mom_init
  private gamma,gamxinf,GAML02, GAML02d300, GAML02d500, fqvs, fqis
  private delbk, delabk
  private gammadp
  
  logical, public :: cleardiag = .false.
  PRIVATE
  
  integer, private :: eqtset = 1 
   double precision, parameter, public :: zscale = 1.0d0 
   double precision, parameter, public :: zscaleinv = 1.0d0/zscale 

  
  real, parameter :: warmonly = 0.0 
  
  logical, parameter :: lwsm6 = .false. 


  real, parameter  :: dimax = 500.e-6    
  real, parameter  :: roqimax = 2.08e22*dimax**8
  

  integer  :: iuseferrier = 1  
  integer  :: idbzci      = 0
  integer  :: iusewetgraupel = 0 
                                 
  integer  :: iusewethail = 0 



  real, private :: rho_qr = 1000., cnor = 8.0e5  
  real, private :: rho_qs =  100., cnos = 3.0e6  
  real, private :: rho_qh =  500., cnoh = 4.0e5  
  real, private :: rho_qhl=  900., cnohl = 4.0e4 

  real, private :: hldnmn = 500.0  

  real :: cnohmn  = 1.e-2 
  real :: cnohlmn = 1.e-2 
  


  real   , private :: qcmincwrn      = 2.0e-3    
  real   , private :: cwdiap         = 20.0e-6   
  real   , private :: cwdisp         = 0.15      
  real   , private  :: ccn            = 1.5e+09   
  real   , private :: qccn             
  integer, private :: iauttim        = 1         
  real   , private :: auttim         = 300.      
  real   , private :: qcwmntim       = 1.0e-5    






  integer, private :: itfall = 0
  integer, private :: iscfall = 1
  integer, private :: irfall = -1
  integer, private :: infall = 4   
                          
                          
                          
                          
                          
  real    :: rainfallfac = 1.0 
  integer, private :: icdx = 3 
  integer, private :: icdxhl = 3 
  real   , private :: cdhmin = 0.45, cdhmax = 0.8        
  real   , private :: cdhdnmin = 500., cdhdnmax = 800.0  
  real   , private :: cdhlmin = 0.45, cdhlmax = 0.6      
  real   , private :: cdhldnmin = 500., cdhldnmax = 800.0  
  
  integer :: rssflg = 1   
  integer :: sssflg = 1   
  integer :: hssflg = 1   
  integer :: hlssflg = 1  



  integer, private :: ndebug = -1, ncdebug = 0
  integer, private :: ipconc = 5
  integer, private :: ichaff = 0
  integer, private :: ilimit = 0

  real, private :: cimn = 1.0e3, cimx = 1.0e6


  real   , private :: ifrzg = 1.0 
  integer, private :: irwfrz = 1 
  integer, private :: irimtim = 0 


  real   , private :: rimc1 = 300.0, rimc2 = 0.44  
  real   , private :: rimc3 = 170.0                
  real   , private :: rimtim = 120.0               
  real   , private :: eqtot = 1.0e-9               

  integer, private :: ireadmic = 0

  integer, private :: iccwflg = 1     
                             
  integer, private :: issfilt = 0     
  integer, private :: irenuc = 2      
                                      
                             
  integer, private :: irenuc3d = 0      
  real    :: renucfrac = 0.0 
                             
                             
  real   , private :: cck = 0.6       
  real   , private :: xcradmx = 40.0e-6,ciintmx = 1.0e6

  real   , private :: cwccn 
  real   , private :: ccwmx

  integer, private :: idocw = 1, idorw = 1, idoci = 1, idoir = 1, idoip = 1, idosw = 1
  integer, private :: idogl = 1, idogm = 1, idogh = 1, idofw = 1, idohw = 1, idohl = 1




  integer, private :: itype1 = 0, itype2 = 2  
  integer, private :: icenucopt = 2       
  integer, private :: icfn = 2                
  integer, private :: ihrn = 0            
  integer, private :: ibfc = 1            
  integer, private :: iacr = 2            
                                 
  integer, private :: ibfr = 2            
                                 
  integer, private :: ibiggopt = 1        
  integer, private :: iacrsize = 1        
                                 
                                 
                                 
  real   , private :: cimas0 = 6.62e-11   
                                 
  real   , private :: splintermass = 6.88e-13
  real   , private :: cfnfac = 0.1        
  integer, private :: iscni = 4           
  logical, private :: imeyers5 = .false.  
  real   , private :: dmincw = 15.0e-6    
  integer, private :: iehw = 1            
  integer, private :: iehlw = 1           
                                 
  integer, private :: ierw = 1            
  real   , private :: ehw0 = 0.9          
  real   , private :: erw0 = 1.0          
  real   , private :: ehlw0 = 0.9         

  real   , private :: esilfo0 = 1.0       
  real   , private :: ehslfo0 = 1.0       

  integer, private :: ircnw    = 5        
  real   , private :: qminrncw = 2.0e-3   

  integer, private :: iqcinit = 2         
                                 
                                 
                                 

  real   , private :: ssmxinit = 0.4      
                                 

  real   , private :: ewfac = 1.0         
  real   , private :: eii0 = 0.1 ,eii1 = 0.1  
                                     
  real   , private :: eii0hl = 0.2 ,eii1hl = 0.0  
                                     
  real   , private :: eri0 = 1.0   
  real   , private :: ehs0 = 0.1 ,ehs1 = 0.1  
                                     
  real   , private :: ess0 = 1.0 ,ess1 = 0.05 
                                     
  real   , private :: ehsfrac = 1.0           
  real   , private :: ehimin = 0.0 
  real   , private :: ehimax = 1.0 
  real   , private :: ehsmax = 0.5 
  integer, private :: iglcnvi = 1  
  integer, private :: iglcnvs = 2  

  real   , private :: rz          
                         
                         
  real   , private :: rzhl        
                         

  real   , private :: alphahacx = 0.0 

  real   , private :: fconv = 1.0  

  real   , private :: rg0 = 400.0  

  integer, private :: rcond = 2    
                                   
  integer, parameter, private :: icond = 1    
                          
  
  real   , private :: dfrz = 0.15e-3  
                            
  real   , private :: dmlt = 3.0e-3  
  real   , private :: dshd = 1.0e-3  

  integer, private :: ihmlt = 2      
  integer, private :: imltshddmr = 0 
                            
                            
                            
                            
                            
                            


  integer, private :: nsplinter = 0  
  integer, private :: isnwfrac = 0   



  logical, private :: mixedphase = .false.   
  integer, private :: imixedphase = 0
  logical, private :: qsdenmod = .false.     
  logical, private :: qhdenmod = .false.     
  logical, private :: qsvtmod = .false.      
  real   , private :: sheddiam   = 8.0e-03  
  real    :: sheddiamlg = 10.0e-03  
  real    :: sheddiam0  = 20.0e-03  

  real   , private :: fwms = 0.5 
  real   , private :: fwmh = 0.5 
  real   , private :: fwmhl = 0.5 
  real    :: fwmlarge = 0.2 
  integer :: ifwmfall = 0   
                            
  
  logical :: rescale_high_alpha = .false.  
  logical :: rescale_low_alpha = .true.    
  logical :: rescale_low_alphar = .true.    
  
  
  integer, private ::  ihlcnh = 1  
                          
                          
  real   , private :: hldia1 = 20.0e-3  

  integer, parameter :: lqmx = 30
  integer, parameter :: lt = 1
  integer, parameter :: lv = 2
  integer, parameter :: lc = 3
  integer, parameter :: lr = 4
  integer, parameter :: li = 5
  integer, parameter :: ls = 6
  integer, parameter :: lh = 7
  integer :: lhl = 0

  integer, private  :: lccn = 9 
  integer, private :: lccna = 0
  integer, private :: lcina = 0
  integer, private :: lcin = 0
  integer, private :: lnc = 9
  integer, private :: lnr = 10
  integer, private :: lni = 11
  integer, private :: lns = 12
  integer, private :: lnh = 13
  integer, private :: lnhl = 0
  integer :: lvh = 15

  integer, private :: lhab = 8
  integer, private :: lg = 7



  integer :: lvi = 0
  integer :: lvs = 0
  integer :: lvgl = 0
  integer :: lvgm = 0
  integer :: lvgh = 0
  integer :: lvf = 0

  integer :: lvhl = 0


  integer :: lhw = 0
  integer :: lsw = 0
  integer :: lhlw = 0



  integer :: lzr = 0
  integer :: lzi = 0
  integer :: lzs = 0
  integer :: lzgl = 0
  integer :: lzgm = 0
  integer :: lzgh = 0
  integer :: lzf = 0
  integer :: lzh = 0
  integer :: lzhl = 0



  integer :: lscw = 0
  integer :: lscr = 0
  integer :: lsci = 0
  integer :: lscs = 0
  integer :: lsch = 0
  integer :: lschl = 0
  integer :: lscwi = 0
  integer :: lscpi = 0
  integer :: lscni = 0
  integer :: lscpli = 0
  integer :: lscnli = 0
  integer :: lschab = 0

  integer :: lscb = 0
  integer :: lsce = 0
  integer :: lsceq = 0



  integer :: lne = 0 

  real :: cnoh0 = 4.0e+5
  real :: hwdn1 = 700.0

  real    :: alphai  = 0.0 
  real    :: alphas  = 0.0 
  real    :: alphar  = 0.0 
  real, private    :: alphah  = 0.0 
  real, private    :: alphahl = 1.0 

  real    :: dmuh    = 1.0  
  real    :: dmuhl   = 1.0  

  real, parameter :: alpharmax = 8. 
  real, parameter :: alphamax = 15.
  real, parameter :: alphamin = 0.
  real, parameter :: rnumin = -0.8
  real, parameter :: rnumax = 15.0

  
  real            :: cnu = 0.0
  real, parameter :: rnu = -0.8, snu = -0.8, cinu = 0.0

  
  real xnu(lc:lqmx) 
  real xmu(lc:lqmx) 
  real dnu(lc:lqmx) 
  real dmu(lc:lqmx) 
  
  real ax(lc:lqmx)
  real bx(lc:lqmx)
  real fx(lc:lqmx)

      real da0 (lc:lqmx)          
      real dab0(lc:lqmx,lc:lqmx)  
      real dab1(lc:lqmx,lc:lqmx)  
      real da1 (lc:lqmx)          
      real bb  (lc:lqmx)




      real :: xvcmn, xvcmx = 2.89e-13  
      real xvrmn, xvrmx0  
      real xvsmn, xvsmx  
      real xvfmn, xvfmx  
      real xvgmn, xvgmx  
      real xvhmn, xvhmn0, xvhmx, xvhmx0  
      real xvhlmn, xvhlmx  

      real, private :: dhmn = -1., dhmx = -1.
      real, parameter :: dhlmn = 0.3e-3, dhlmx = 40.e-3

      parameter( xvcmn=4.188e-18 )   
      parameter( xvrmn=0.523599*(80.e-6)**3, xvrmx0=0.523599*(6.e-3)**3 ) 
      real     :: xvdmx = -1.0 
      real     :: xvrmx
      parameter( xvsmn=0.523599*(0.1e-3)**3, xvsmx=0.523599*(10.e-3)**3 ) 
      parameter( xvfmn=0.523599*(0.1e-3)**3, xvfmx=0.523599*(10.e-3)**3 )  
      parameter( xvgmn=0.523599*(0.1e-3)**3, xvgmx=0.523599*(10.e-3)**3 )  
      parameter( xvhmn0=0.523599*(0.15e-3)**3, xvhmx0=0.523599*(10.e-3)**3 )  
      parameter( xvhlmn=0.523599*(0.3e-3)**3, xvhlmx=0.523599*(25.e-3)**3 )  

  integer :: ipelec = 0
  integer :: isaund = 0
  logical :: idonic = .false.



      integer ngm0,ngm1,ngm2
      parameter (ngm0=3001,ngm1=500,ngm2=500)
      real, parameter :: dgam = 0.01, dgami = 100.
      real gmoi(0:ngm0) 

      integer, parameter :: nqiacralpha = 15, nqiacrratio = 25
      real,    parameter :: dqiacralpha = 1., dqiacrratio = 1.
      real :: ciacrratio(0:nqiacrratio,0:nqiacralpha)
      real :: qiacrratio(0:nqiacrratio,0:nqiacralpha)

    integer, parameter :: ngdnmm = 9
    real :: mmgraupvt(ngdnmm,3)  

    DATA mmgraupvt(:,1) / 50., 150., 250., 350., 450., 550., 650., 750., 850./
    DATA mmgraupvt(:,2) / 62.923, 94.122, 114.74, 131.21, 145.26, 157.71, 168.98, 179.36, 189.02 /
    DATA mmgraupvt(:,3) / 0.67819, 0.63789, 0.62197, 0.61240, 0.60572, 0.60066, 0.59663, 0.59330, 0.59048 /

      integer lsc(lc:lqmx)
      integer ln(lc:lqmx)
      integer ipc(lc:lqmx)
      integer lvol(lc:lqmx)
      integer lz(lc:lqmx)
      integer lliq(ls:lqmx)
      integer denscale(lc:lqmx) 

      integer ido(lc:lqmx)
      logical ldovol

      real xdn0(lc:lqmx)
      real xdnmx(lc:lqmx), xdnmn(lc:lqmx)
      real cdx(lc:lqmx)
      real cno(lc:lqmx)
      real xvmn(lc:lqmx), xvmx(lc:lqmx)
      real qxmin(lc:lqmx)

      integer nqsat
      parameter (nqsat=1000001) 
      real fqsat,fqsati
      parameter (fqsat=0.002,fqsati=1./fqsat)
      real tabqvs(nqsat),tabqis(nqsat),dtabqvs(nqsat),dtabqis(nqsat)




      real, parameter :: cp608 = 0.608          
      real, parameter :: ar = 841.99666         
      real, parameter :: br = 0.8               
      real, parameter :: aradcw = -0.27544      
      real, parameter :: bradcw = 0.26249e+06   
      real, parameter :: cradcw = -1.8896e+10   
      real, parameter :: dradcw = 4.4626e+14    
      real, parameter :: bta1 = 0.6             
      real, parameter :: cnit = 1.0e-02         
      real, parameter :: dragh = 0.60           
      real, parameter :: dnz00 = 1.225          
      real, parameter :: rho00 = 1.225          



      real, parameter :: cs = 12.42             
      real, parameter :: ds = 0.42              
      real, parameter :: pi = 3.141592653589793
      real, parameter :: piinv = 1./pi
      real, parameter :: pid4 = pi/4.0

      real, parameter :: gr = 9.8




      real eperao
      parameter (eperao  = 8.8592e-12 )

      real ec,eci  
      parameter (ec = 1.602e-19)
      parameter (eci = 1.0/ec)




      real, parameter :: c1f3 = 1.0/3.0

      real, parameter :: cai = 21.87455
      real, parameter :: caw = 17.2693882
      real, parameter :: cbi = 7.66
      real, parameter :: cbw = 35.86

      real, parameter :: tfr = 273.15, tfrh = 233.15

      real, parameter :: cp = 1004.0, rd = 287.04
      real, parameter :: cpi = 1./cp
      real, parameter :: cap = rd/cp, poo = 1.0e+05

      real, parameter :: rw = 461.5              
      real, parameter :: advisc0 = 1.832e-05     
      real, parameter :: advisc1 = 1.718e-05     
      real, parameter :: tka0 = 2.43e-02         
      real, parameter :: tfrcbw = tfr - cbw
      real, parameter :: tfrcbi = tfr - cbi

     

     real, private, parameter ::      cv = 717.0             
     REAL, PRIVATE, parameter ::      cvv = 1408.5
     REAL, PRIVATE, parameter ::      cpl = 4190.0
     REAL, PRIVATE, parameter ::      cpigb = 2106.0
     

      real, parameter ::  bfnu0 = (rnu + 2.0)/(rnu + 1.0) 
      real :: ventr, ventrn, ventc, c1sw

      real :: cwmasn = 5.23e-13   
      real :: cwmasn5 =  5.23e-13
      real :: cwradn = 5.0e-6     
      real :: cwmasx = 5.25e-10   
      real, parameter :: cwc1 = 6.0/(pi*1000.)

      real :: cckm,ccne,ccnefac,cnexp

      integer :: na = 9

      real gf4p5, gf4ds, gf4br
      real gfcinu1, gfcinu1p47, gfcinu2p47

      real :: cwchtmp0 = 1.0
      real :: cwchltmp0 = 1.0

      integer, private :: imurain = 1 
      integer :: iturbenhance = 0 
      integer, private :: isedonly = 0 
      integer, private :: iferwisventr = 2 
      integer, private :: izwisventr   = 2 
      integer :: iresetmoments = 1 
      integer, private :: imaxdiaopt    = 3 
                                   
                                   
      integer, private :: dmrauto       = 0 
                                  
                                  
                                  
                                  
      real    :: cxmin = 1.e-4  
      real    :: zxmin = 1.e-28 
  
      integer :: ithompsoncnoh = 0 
                               
                               

      integer :: ivhmltsoak = 1   
                             
      integer, private :: ioldlimiter = 0 
  
      integer :: ibiggsnow   = 0 
  
      integer :: ixtaltype = 1 
  
      real    :: evapfac     = 1.0 
  
      real, private :: hlcnhdia = 1.e-3 
      real, private :: hlcnhqmin = 0.1e-3 
      integer :: icvhl2h = 0   





 CONTAINS




 REAL FUNCTION fqvs(t)
  implicit none
  real :: t
  fqvs = exp(caw*(t-273.15)/(t-cbw))
 END FUNCTION fqvs

 REAL FUNCTION fqis(t)
  implicit none
  real :: t
  fqis = exp(cai*(t-273.15)/(t-cbi))
 END FUNCTION fqis


 
       SUBROUTINE nssl_2mom_init(  &
     & ims,ime, jms,jme, kms,kme, nssl_params, ipctmp, mixphase,ihvol,idonictmp)

  implicit none
  
   integer, intent(in) :: ims,ime, jms,jme, kms,kme
   real,    intent(in), dimension(20) :: nssl_params
   integer, intent(in) :: ipctmp,mixphase,ihvol
   logical, optional, intent(in) :: idonictmp

     real    :: arg, temq
     integer :: igam
     integer :: i,il,j,l
     integer :: ltmp
     integer :: isub

      real    :: alp,ratio,x,y
     





      ccn = nssl_params(1)
      alphah   = nssl_params(2)
      alphahl  = nssl_params(3)
      cnoh     = nssl_params(4)
      cnohl    = nssl_params(5)
      cnor     = nssl_params(6)
      cnos     = nssl_params(7)
      rho_qh   = nssl_params(8)
      rho_qhl  = nssl_params(9)
      rho_qs   = nssl_params(10)

      cwccn = ccn

      IF ( ipelec > 0 ) idonic = .true.





      do l = 1,nqsat
      temq = 163.15 + (l-1)*fqsat
      tabqvs(l) = exp(caw*(temq-273.15)/(temq-cbw))
      dtabqvs(l) = ((-caw*(-273.15 + temq))/(temq - cbw)**2 + &
     &                 caw/(temq - cbw))*tabqvs(l)
      tabqis(l) = exp(cai*(temq-273.15)/(temq-cbi))
      dtabqis(l) = ((-cai*(-273.15 + temq))/(temq - cbi)**2 + &
     &                 cai/(temq - cbi))*tabqis(l)
      end do


     gmoi(0) = 1.e32
     do igam = 1,ngm0
      arg = dgam*igam
      gmoi(igam) = gamma(arg)
     end do

     
     
     
      
      DO j = 0,nqiacralpha
      alp = float(j)
      y = gamma(1.+alp)
      DO i = 1,nqiacrratio
        ratio = float(i)
        x = gamxinf( 1.+alp, ratio )

        ciacrratio(i,j) = x/y
      ENDDO
      ENDDO
      ciacrratio(0,:) = 1.0

      DO j = 0,nqiacralpha
      alp = float(j)
      y = gamma(4.+alp)
      DO i = 1,nqiacrratio
        ratio = float(i)
        x = gamxinf( 4.+alp, ratio )

        qiacrratio(i,j) = x/y
      ENDDO
      ENDDO
      qiacrratio(0,:) = 1.0


      lhab = 8
      lhl = 8
      IF ( ihvol == -1 ) THEN
        lhab = 7  
        lhl = 0
      ENDIF
      isub = Min( 0, ihvol) 

      lccn = 0
      lccna = 0
      lnc = 0
      lnr = 0
      lni = 0
      lns = 0
      lnh = 0
      lnhl = 0
      lvh = 0
      lvhl = 0
      lzr = 0
      lzh = 0
      lzhl = 0
      lsw = 0
      lhw = 0
      lhlw = 0

      denscale(:) = 0
      


    ipconc = ipctmp

    IF ( ipconc == 0 ) THEN
       IF ( ihvol >= 0 ) THEN
       lvh = 9
       ltmp = 9
       denscale(lvh) = 1
       ELSE 
       ltmp = lhab
       lhl = 0
       ENDIF
    ELSEIF ( ipconc == 5 ) THEN
      lccn = lhab+1 
      lnc = lhab+2 
      lnr = lhab+3 
      lni = lhab+4 
      lns = lhab+5 
      lnh = lhab+6 
      IF ( ihvol >= 0 ) THEN
      lnhl = lhab+7 
      ENDIF
      lvh = lhab+8 + isub 
      ltmp = lvh
      denscale(lccn:lvh) = 1
      IF ( ihvol == 1 ) THEN
       lvhl = ltmp+1
       ltmp = lvhl
       denscale(lvhl) = 1
      ENDIF
      IF ( mixedphase ) THEN
      lsw  = ltmp+1
      lhw  = ltmp+2
      lhlw = ltmp+3
      ltmp = lhlw
      ENDIF
    ELSEIF ( ipconc >= 6 ) THEN
      lccn = 9
      lnc = 10
      lnr = 11
      lni = 12
      lns = 13
      lnh = 14
      IF ( ihvol >= 0 ) THEN
      lnhl = 15
      ENDIF
      IF ( ipconc == 6 ) THEN
      lzh = 16 + isub
      lvh = 17 + isub
      ELSEIF ( ipconc == 7 ) THEN
      lzh = 16
      lzr = 17
      lvh = 18
      ELSEIF ( ipconc == 8 ) THEN
      lzr = 16
      lzh = 17
      lzhl = 18
      lvh = 19
      ENDIF
      ltmp = lvh
      denscale(lccn:lvh) = 1
      IF ( ihvol == 1 ) THEN
       lvhl = ltmp+1
       ltmp = lvhl
       denscale(lvhl) = 1
      ENDIF
      IF ( mixedphase ) THEN
      lsw  = ltmp+1
      lhw  = ltmp+2
      lhlw = ltmp+3
      ltmp = lhlw
      ENDIF
    ELSE
      CALL wrf_error_fatal3("<stdin>",819,&
'nssl_2mom_init: Invalid value of ipctmp' )
    ENDIF


    
      na = ltmp
      
      ln(lc) = lnc
      ln(lr) = lnr
      ln(li) = lni
      ln(ls) = lns
      ln(lh) = lnh
      IF ( lhl .gt. 1 ) ln(lhl) = lnhl

      ipc(lc) = 2
      ipc(lr) = 3
      ipc(li) = 1
      ipc(ls) = 4
      ipc(lh) = 5
      IF ( lhl .gt. 1 ) ipc(lhl) = 5
      
      ldovol = .false.
      lvol(:) = 0
      lvol(li) = lvi
      lvol(ls) = lvs
      lvol(lh) = lvh
      IF ( lhl .gt. 1 .and. lvhl .gt. 1 ) lvol(lhl) = lvhl
      
      lne = Max(lnh,lnhl)
      lne = Max(lne,lvh)
      lne = Max(lne,lvhl)
      lne = Max(lne,na)

      lsc(:) = 0
      lsc(lc) = lscw
      lsc(lr) = lscr
      lsc(li) = lsci
      lsc(ls) = lscs
      lsc(lh) = lsch
      IF ( lhl .gt. 1 ) lsc(lhl) = lschl


      DO il = lc,lhab
        ldovol = ldovol .or. ( lvol(il) .gt. 1 )
      ENDDO



      lz(:) = 0
      lz(lr) = lzr
      lz(li) = lzi
      lz(ls) = lzs
      lz(lh) = lzh
      IF ( lhl .gt. 1 .and. lzhl > 1 ) lz(lhl) = lzhl

      lliq(:) = 0
      lliq(ls) = lsw
      lliq(lh) = lhw
      IF ( lhl .gt. 1 ) lliq(lhl) = lhlw
      IF ( mixedphase ) THEN

      ENDIF

      bx(lr) = 0.85
      ax(lr) = 1647.81
      fx(lr) = 135.477
      
      IF ( icdx > 0 ) THEN
        bx(lh) = 0.5
        ax(lh) = 75.7149
      ELSE
        bx(lh) = 0.37 
        ax(lh) = 19.3
      ENDIF


      IF ( lhl .gt. 1 ) THEN
        IF (icdxhl > 0 ) THEN
         bx(lhl) = 0.5
         ax(lhl) = 75.7149
        ELSE
        ax(lhl) = 206.984
        bx(lhl) = 0.6384
        ENDIF
      ENDIF


      xnu(lc) = 0.0
      xmu(lc) = 1.
      
      IF ( imurain == 3 ) THEN
        xnu(lr) = -0.8
        xmu(lr) = 1.
      ELSEIF ( imurain == 1 ) THEN
        xnu(lr) = (alphar - 2.0)/3.0
        xmu(lr) = 1./3.
      ENDIF

      xnu(li) = 0.0
      xmu(li) = 1.

      dnu(lc) = 3.*xnu(lc) + 2. 
      dmu(lc) = 3.*xmu(lc)

      dnu(lr) = 3.*xnu(lr) + 2. 
      dmu(lr) = 3.*xmu(lr)

      dnu(ls) = -0.4 
      dmu(ls) = 3.

      xnu(ls) = -0.8
      xmu(ls) = 1.

      dnu(lh) = alphah
      dmu(lh) = dmuh

      xnu(lh) = (dnu(lh) - 2.)/3.
      xmu(lh) = dmuh/3.

      rz =  ((4 + alphah)*(5 + alphah)*(6 + alphah)*(1. + xnu(lr)))/ &
     &  ((1 + alphah)*(2 + alphah)*(3 + alphah)*(2. + xnu(lr)))



      rzhl =  ((4 + alphahl)*(5 + alphahl)*(6 + alphahl)*(1. + xnu(lr)))/ &
     &  ((1 + alphahl)*(2 + alphahl)*(3 + alphahl)*(2. + xnu(lr)))




      IF ( ipconc .lt. 4 ) THEN

      dnu(ls) = alphas
      dmu(ls) = 1.

      xnu(ls) = (dnu(ls) - 2.)/3.
      xmu(ls) = 1./3.


      ENDIF

      IF ( lhl .gt. 1 ) THEN

      dnu(lhl) = alphahl
      dmu(lhl) = dmuhl

      xnu(lhl) = (dnu(lhl) - 2.)/3.
      xmu(lhl) = dmuhl/3.

      ENDIF

      cno(lc)  = 1.0e+08
      IF ( li .gt. 1 ) cno(li)  = 1.0e+08
      cno(lr)  = cnor
      IF ( ls .gt. 1 ) cno(ls)  = cnos 
      IF ( lh .gt. 1 ) cno(lh)  = cnoh 
      IF ( lhl .gt. 1 ) cno(lhl)  = cnohl 



      xdnmx(:) = 900.0

      xdnmx(lr) = 1000.0
      xdnmx(lc) = 1000.0
      xdnmx(li) =  917.0
      xdnmx(ls) =  300.0
      xdnmx(lh) =  900.0
      IF ( lhl .gt. 1 ) xdnmx(lhl) = 900.0

      xdnmn(:) = 900.0

      xdnmn(lr) = 1000.0
      xdnmn(lc) = 1000.0
      xdnmn(li) =  100.0
      xdnmn(ls) =  100.0
      xdnmn(lh) =  170.0
      IF ( lhl .gt. 1 ) xdnmn(lhl) = hldnmn

      xdn0(:) = 900.0

      xdn0(lc) = 1000.0
      xdn0(li) = 900.0
      xdn0(lr) = 1000.0
      xdn0(ls) = rho_qs 
      xdn0(lh) = rho_qh 
      IF ( lhl .gt. 1 ) xdn0(lhl) = rho_qhl 





      cdx(lr) = 0.60
      cdx(lh) = 0.8 
      cdx(ls) = 2.00
      IF ( lhl .gt. 1 ) cdx(lhl) = 0.45

      ido(lc) = idocw
      ido(lr) = idorw
      ido(li) = idoci
      ido(ls) = idosw
      ido(lh)  = idohw
      IF ( lhl .gt. 1 ) ido(lhl) = idohl

      IF ( irfall .lt. 0 ) irfall = infall
      IF ( lzr > 0 ) irfall = 0

      qccn = ccn/rho00
      xvcmx = (4./3.)*pi*xcradmx**3


      IF ( xvdmx .gt. 0.0 ) THEN
        xvrmx = 0.523599*(xvdmx)**3
      ELSE
        xvrmx = xvrmx0
      ENDIF

         IF ( dhmn <= 0.0 ) THEN
           xvhmn = xvhmn0

         ELSE
           xvhmn = 0.523599*(dhmn)**3

         ENDIF

         IF ( dhmx <= 0.0 ) THEN
           xvhmx = xvhmx0
         ELSE
           xvhmx = 0.523599*(dhmx)**3
         ENDIF


      xvmn(lc) = xvcmn
      xvmn(lr) = xvrmn
      xvmn(ls) = xvsmn
      xvmn(lh) = xvhmn

      xvmx(lc) = xvcmx
      xvmx(lr) = xvrmx
      xvmx(ls) = xvsmx
      xvmx(lh) = xvhmx

      IF ( lhl .gt. 1 ) THEN
      xvmn(lhl) = xvhlmn
      xvmx(lhl) = xvhlmx
      ENDIF





      cwmasn = 5.23e-13   
      cwmasn5 =  5.23e-13
      cwradn = 5.0e-6     
      cwmasx = 5.25e-10   

      IF ( ipconc .ge. 2 ) THEN
        cwmasn = xvmn(lc)*1000.  
        cwradn = 1.0e-6          
        cwmasx = xvmx(lc)*1000.  
        
      ENDIF



      IF ( lhl < 1 ) ifrzg = 1

      ventr = 1.
      IF ( imurain == 3 ) THEN

        ventr = Gamma(rnu + 4./3.)/((rnu + 1.)**(1./3.)*Gamma(rnu + 1.)) 

        ventrn =  Gamma(rnu + 1.5 + br/6.)/(Gamma(rnu + 1.)*(rnu + 1.)**((1.+br)/6. + 1./3.) ) 



      ELSE 

        ventr = Gamma(2. + alphar)  

        ventrn =  Gamma(alphar + 2.5 + br/2.)/Gamma(alphar + 1.) 

      ENDIF
      ventc   = Gamma(cnu + 4./3.)/(cnu + 1.)**(1./3.)/Gamma(cnu + 1.)
      c1sw = Gamma(snu + 4./3.)*(snu + 1.0)**(-1./3.)/gamma(snu + 1.0) 

  

      qxmin(:) = 1.0e-12

      qxmin(lc) = 1.e-9
      qxmin(lr) = 1.e-7
      IF ( li > 1 ) qxmin(li) = 1.e-12
      IF ( ls > 1 ) qxmin(ls) = 1.e-7
      IF ( lh > 1 ) qxmin(lh) = 1.e-7
      IF ( lhl .gt. 1 ) qxmin(lhl) = 1.e-7

      IF ( lc .gt. 1 .and. lnc .gt. 1 ) qxmin(lc) = 1.0e-13
      IF ( lr .gt. 1 .and. lnr .gt. 1 ) qxmin(lr) = 1.0e-12

      IF ( li .gt. 1 .and. lni .gt. 1 ) qxmin(li ) = 1.0e-13
      IF ( ls .gt. 1 .and. lns .gt. 1 ) qxmin(ls ) = 1.0e-13
      IF ( lh .gt. 1 .and. lnh .gt. 1 ) qxmin(lh ) = 1.0e-12
      IF ( lhl.gt. 1 .and. lnhl.gt. 1 ) qxmin(lhl) = 1.0e-9

  

      cckm = cck-1.
      ccnefac =  (1.63/(cck * beta(3./2., cck/2.)))**(cck/(cck + 2.0))
      cnexp   = (3./2.)*cck/(cck+2.0)


      ccne = ccnefac*1.e6*(1.e-6*Abs(cwccn))**(2./(2.+cck))
      IF ( cwccn .lt. 0.0 ) THEN
      cwccn = Abs(cwccn)
      ccwmx = cwccn
      ELSE
      ccwmx = cwccn 
      ENDIF





      bb(:) = 1.0/3.0
      bb(li) = 0.3429
      DO il = lc,lhab
        da0(il) = delbk(bb(il), xnu(il), xmu(il), 0)
        da1(il) = delbk(bb(il), xnu(il), xmu(il), 1)


      ENDDO

      dab0(:,:) = 0.0
      dab1(:,:) = 0.0

      DO il = lc,lhab
        DO j = lc,lhab
          IF ( il .ne. j ) THEN

            dab0(il,j) = delabk(bb(il), bb(j), xnu(il), xnu(j), xmu(il), xmu(j), 0)
            dab1(il,j) = delabk(bb(il), bb(j), xnu(il), xnu(j), xmu(il), xmu(j), 1)


          ENDIF
        ENDDO
      ENDDO

        gf4br = gamma(4.0+br)
        gf4ds = gamma(4.0+ds)
        gf4p5 = gamma(4.0+0.5)
        gfcinu1 = gamma(cinu + 1.0)
        gfcinu1p47 = gamma(cinu + 1.47167)
        gfcinu2p47 = gamma(cinu + 2.47167)

        IF ( lh  .gt. 1 ) cwchtmp0 = 6.0/pi*gamma( (xnu(lh) + 1.)/xmu(lh) )/gamma( (xnu(lh) + 2.)/xmu(lh) )
        IF ( lhl .gt. 1 ) cwchltmp0 = 6.0/pi*gamma( (xnu(lhl) + 1)/xmu(lhl) )/gamma( (xnu(lhl) + 2)/xmu(lhl) )


  RETURN
END SUBROUTINE nssl_2mom_init




SUBROUTINE nssl_2mom_driver(qv, qc, qr, qi, qs, qh, qhl, ccw, crw, cci, csw, chw, chl,  &
                              cn, vhw, vhl,                                             &
                              zrw, zhw, zhl,                                            &
                              qsw, qhw, qhlw,                                           &
                              th, pii, p, w, dn, dz, dtp, itimestep,                    &
                              RAINNC,RAINNCV, SNOWNC, SNOWNCV, GRPLNC, GRPLNCV,         &
                              SR,HAILNC, HAILNCV,                                       &
                              dx, dy,                                                   &
                              dbz, vzf,compdbz,                                         &
                              rscghis_3d, rscghis_2d,                                   &
                              scr,scw,sci,scs,sch,schl,sctot,noninduc,                  &
                              induc,elec,scion,sciona,                                  &
                              ipelectmp,                                                &
                              diagflag,                                                 &
                              ids,ide, jds,jde, kds,kde,                                &  
                              ims,ime, jms,jme, kms,kme,                                &  
                              its,ite, jts,jte, kts,kte)                                   




      implicit none

      integer :: mytask = 0

 

      integer, intent(in)::                                                             &
                            ids,ide, jds,jde, kds,kde,                                   &
                            ims,ime, jms,jme, kms,kme,                                   &
                            its,ite, jts,jte, kts,kte
      real, dimension(ims:ime, kms:kme, jms:jme), intent(inout)::                        &
                            qv,qc,qr,qi,qs,qh,th
      real, dimension(ims:ime, kms:kme, jms:jme), optional, intent(inout)::                        &
                              zrw, zhw, zhl,                                            &
                              qsw, qhw, qhlw,                                           &
                            qhl,ccw,crw,cci,csw,chw,chl,vhw,vhl
      real, dimension(ims:ime, kms:kme, jms:jme), optional, intent(inout):: dbz, vzf, cn
      real, dimension(ims:ime, jms:jme), optional, intent(inout):: compdbz
      real, dimension(ims:ime, jms:jme), optional, intent(inout):: rscghis_2d
      real, dimension(ims:ime, kms:kme, jms:jme), optional, intent(inout)::rscghis_3d
      real, dimension(ims:ime, kms:kme, jms:jme),  optional, intent(inout)::                   &
                            scr,scw,sci,scs,sch,schl,sciona,sctot,induc,noninduc  
      real, dimension(ims:ime, kms:kme, jms:jme),  optional, intent(in) :: elec 
      real, dimension(ims:ime, kms:kme, jms:jme,2),optional, intent(inout) :: scion  
      real, dimension(ims:ime, kms:kme, jms:jme), intent(in)::                           &
                            pii,p,w,dz,dn
      real, dimension(ims:ime, jms:jme), intent(inout)::                                 &
                            RAINNC,RAINNCV,SNOWNC,SNOWNCV,GRPLNC,GRPLNCV,SR        
      real, dimension(ims:ime, jms:jme), optional, intent(inout)::                                 &
                            HAILNC,HAILNCV 
      real, optional, intent(in) :: dx,dy
      real, intent(in)::    dtp
      integer, intent(in):: itimestep 
      logical, optional, intent(in) :: diagflag
      real, optional, intent(in) :: ipelectmp



     real, dimension(its:ite, 1, kts:kte,2) :: scion2 
     real, dimension(its:ite, 1, kts:kte, na) :: an
     real, dimension(its:ite, 1, kts:kte) :: t0,t1,t2,t3,t4,t5,t6,t7,t8,t9
     real, dimension(its:ite, 1, kts:kte) :: dn1,t00,t77,ssat,pn,wn,dz2d,dz2dinv,dbz2d,vzf2d
     real, dimension(its:ite, 1, na) :: xfall
     integer, parameter :: nor = 0, ng = 0
     integer :: nx,ny,nz
     integer ix,jy,kz,i,j,k,il,n
     integer :: infdo
     real :: ssival, ssifac, t8s, t9s, qvapor
     integer :: ltemq
     double precision :: dp1
     integer :: jye, lnb
     integer :: imx,kmx
     real    :: dbzmx
     integer :: vzflag0 = 0
     logical :: makediag
      real, parameter :: cnin20 = 1.0e3
      real, parameter :: cnin10 = 5.0e1
      real, parameter :: cnin1a = 4.5
      real, parameter :: cnin2a = 12.96
      real, parameter :: cnin2b = 0.639

      real :: tmp,dv

      double precision :: dt1,dt2
      double precision :: timesed,timesed1,timesed2,timesed3, timegs, timenucond, timedbz,zmaxsed
      double precision :: timevtcalc,timesetvt
      




      

      
      
     IF ( present( vzf ) ) vzflag0 = 1
     
     IF ( present( ipelectmp ) ) ipelec = Nint(ipelectmp)










     
     makediag = .true.
     IF ( present( diagflag ) ) THEN
      makediag = diagflag
     ENDIF


     
     
     nx = ite-its+1
     ny = 1         
     nz = kte-kts+1
     

     IF ( itimestep == 1 ) THEN
      IF ( itimestep == 1 .and. present( cn ) ) THEN
        DO jy = jts,jte
         DO kz = kts,kte
           DO ix = its,ite
             cn(ix,kz,jy) = qccn 
           ENDDO
         ENDDO
       ENDDO
       ENDIF

     ENDIF 



      infdo = 2
      
      IF ( infall .ne. 1 .or. iscfall .ge. 2 ) THEN
         infdo = 1
      ELSE
         infdo = 0
      ENDIF

      IF ( infall .ge. 3 .or. ipconc .ge. 6 ) THEN
         infdo = 2
      ENDIF
     

      IF ( present( HAILNCV ) .and. lhl < 1 ) THEN 
        HAILNCV(its:ite,jts:jte) = 0.
      ENDIF

     lnb = Max(lh,lhl)+1 


       jye = jte

     IF ( present( compdbz ) .and. makediag ) THEN
     DO jy = jts,jye
       DO ix = its,ite
        compdbz(ix,jy) = -3.0
       ENDDO
     ENDDO
     ENDIF

      zmaxsed = 0.0d0
      timevtcalc = 0.0d0
      timesetvt = 0.0d0
      timesed = 0.0d0
      timesed1 = 0.0d0
      timesed2 = 0.0d0
      timesed3 = 0.0d0
      timegs = 0.0d0
      timenucond = 0.0d0




     DO jy = jts,jye
     
     xfall(:,:,:) = 0.0



     
   
   
       DO kz = kts,kte
        DO ix = its,ite
        
          an(ix,1,kz,lt)   = th(ix,kz,jy)
          an(ix,1,kz,lv)   = qv(ix,kz,jy)
          an(ix,1,kz,lc)   = qc(ix,kz,jy)
          an(ix,1,kz,lr)   = qr(ix,kz,jy)
          an(ix,1,kz,li)   = qi(ix,kz,jy)
          an(ix,1,kz,ls)   = qs(ix,kz,jy)
          an(ix,1,kz,lh)   = qh(ix,kz,jy)
          IF ( lhl > 1 ) an(ix,1,kz,lhl)  = qhl(ix,kz,jy)
          IF ( lccn > 1 ) THEN
           IF ( present( cn ) ) THEN
            an(ix,1,kz,lccn) = cn(ix,kz,jy)
           ELSE
            an(ix,1,kz,lccn) = qccn
           ENDIF
          ENDIF
          IF ( ipconc >= 5 ) THEN
          an(ix,1,kz,lnc)  = ccw(ix,kz,jy)
          an(ix,1,kz,lnr)  = crw(ix,kz,jy)
          an(ix,1,kz,lni)  = cci(ix,kz,jy)
          an(ix,1,kz,lns)  = csw(ix,kz,jy)
          an(ix,1,kz,lnh)  = chw(ix,kz,jy)
          IF ( lhl > 1 ) an(ix,1,kz,lnhl) = chl(ix,kz,jy)
          ENDIF
          IF ( lvh > 0 ) an(ix,1,kz,lvh)  = vhw(ix,kz,jy)
          IF ( lvhl > 0 .and. present( vhl ) ) an(ix,1,kz,lvhl)  = vhl(ix,kz,jy)

          


          
          t0(ix,1,kz) = th(ix,kz,jy)*pii(ix,kz,jy) 
          t1(ix,1,kz) = 0.0
          t2(ix,1,kz) = 0.0
          t3(ix,1,kz) = 0.0
          t4(ix,1,kz) = 0.0
          t5(ix,1,kz) = 0.0
          t6(ix,1,kz) = 0.0
          t7(ix,1,kz) = 0.0
          t8(ix,1,kz) = 0.0
          t9(ix,1,kz) = 0.0
          t00(ix,1,kz) = 380.0/p(ix,kz,jy)
          t77(ix,1,kz) = pii(ix,kz,jy)
          dbz2d(ix,1,kz) = 0.0
          vzf2d(ix,1,kz) = 0.0

          dn1(ix,1,kz) = dn(ix,kz,jy)
          pn(ix,1,kz) = p(ix,kz,jy)
          wn(ix,1,kz) = w(ix,kz,jy)
          dz2d(ix,1,kz) = dz(ix,kz,jy)
          dz2dinv(ix,1,kz) = 1./dz(ix,kz,jy)
          
         ltemq = Int( (t0(ix,1,kz)-163.15)/fqsat+1.5 )
         ltemq = Min( nqsat, Max(1,ltemq) )



      t8s = t00(ix,1,kz)*tabqvs(ltemq)  
      t9s = t00(ix,1,kz)*tabqis(ltemq)  




      ssival = Min(t8s,max(an(ix,1,kz,lv),0.0))/t9s  

      if ( ssival .gt. 1.0 ) then

      IF ( icenucopt == 1 ) THEN

      if ( t0(ix,1,kz).le.268.15 ) then

       dp1 = cnin20*exp( Min( 57.0 ,(cnin2a*(ssival-1.0)-cnin2b) ) )
       t7(ix,1,kz) = Min(dp1, 1.0d30)
      end if




      IF ( imeyers5 ) THEN
      if ( t0(ix,1,kz).lt.tfr .and. t0(ix,1,kz).gt.268.15 ) then
      qvapor = max(an(ix,1,kz,lv),0.0)
      ssifac = 0.0
      if ( (qvapor-t9s) .gt. 1.0e-5 ) then
      if ( (t8s-t9s) .gt. 1.0e-5 ) then
      ssifac = (qvapor-t9s) /(t8s-t9s)
      ssifac = ssifac**cnin1a
      end if
      end if
      t7(ix,1,kz) = cnin10*ssifac*exp(-(t0(ix,1,kz)-tfr)*bta1)
      end if
      ENDIF

      ELSEIF ( icenucopt == 2 ) THEN 
                                     
                                     
                                     
      
        t7(ix,1,kz) = 0.00446684*exp(0.3108*(273.16 - Max(233.0, t0(ix,1,kz) ) ) )

      
      ELSEIF ( icenucopt == 3 ) THEN 

      if ( t0(ix,1,kz).le.268.15 .and.  t0(ix,1,kz) > 243.15 ) then 
        
       dp1 = 0.06*cnin20*exp( Min( 57.0 ,(cnin2a*(ssival-1.0)-cnin2b) ) )
       t7(ix,1,kz) = Min(dp1, 1.0d30)
      elseif ( t0(ix,1,kz) <= 243.15 ) then 
       dp1 = 1000.*( exp( Min( 57.0 ,cnin2a*(ssival-1.1) ) ) )**0.3
       t7(ix,1,kz) = Min(dp1, 1.0d30)
      
      end if
      
      ENDIF 



      end if 


        ENDDO
       ENDDO


   
     
     DO il = lnb,na
       IF ( denscale(il) == 1 ) THEN
         DO kz = kts,kte
          DO ix = its,ite
           an(ix,1,kz,il) = an(ix,1,kz,il)*dn(ix,kz,jy)
          ENDDO
         ENDDO
       ENDIF
     ENDDO 
        

      xfall(:,:,:) = 0.0
       
      IF ( .true. ) THEN



       IF ( itimestep == 1 .and. ipconc > 0 ) THEN
         call calcnfromq(nx,ny,nz,an,na,nor,nor,dn1)
       ENDIF
      call sediment1d(dtp,nx,ny,nz,an,na,nor,nor,xfall,dn1,dz2d,dz2dinv, &
     &                    t0,t7,infdo,jy,its,jts &
     &   ,timesed1,timesed2,timesed3,zmaxsed,timesetvt)






       DO ix = its,ite
         IF ( lhl > 1 ) THEN
         RAINNCV(ix,jy) = dtp*dn1(ix,1,1)*(xfall(ix,1,lr) + xfall(ix,1,ls)*1000./xdn0(lr) + &
              &            xfall(ix,1,lh)*1000./xdn0(lr) + xfall(ix,1,lhl)*1000./xdn0(lr) )
         ELSE
         RAINNCV(ix,jy) = dtp*dn1(ix,1,1)*(xfall(ix,1,lr) + xfall(ix,1,ls)*1000./xdn0(lr) + &
              &            xfall(ix,1,lh)*1000./xdn0(lr) )
         ENDIF
         SNOWNCV(ix,jy) = dtp*dn1(ix,1,1)*xfall(ix,1,ls)*1000./xdn0(lr)
         GRPLNCV(ix,jy) = dtp*dn1(ix,1,1)*xfall(ix,1,lh)*1000./xdn0(lr)
         RAINNC(ix,jy)  = RAINNC(ix,jy) + RAINNCV(ix,jy)

         SNOWNC(ix,jy)  = SNOWNC(ix,jy) + SNOWNCV(ix,jy)
         IF ( lhl > 1 ) THEN
           IF ( present( HAILNC ) ) THEN
             HAILNCV(ix,jy) = dtp*dn1(ix,1,1)*xfall(ix,1,lhl)*1000./xdn0(lr)
             HAILNC(ix,jy)  = HAILNC(ix,jy) + HAILNCV(ix,jy)
           ELSE
             GRPLNCV(ix,jy) = dtp*dn1(ix,1,1)*xfall(ix,1,lhl)*1000./xdn0(lr)
           ENDIF
         ENDIF
         GRPLNC(ix,jy)  = GRPLNC(ix,jy) + GRPLNCV(ix,jy)
         IF ( present( HAILNC ) ) THEN
           SR(ix,jy)      = (SNOWNCV(ix,jy)+HAILNCV(ix,jy)+GRPLNCV(ix,jy))/(RAINNCV(ix,jy)+1.e-12)
         ELSE
           SR(ix,jy)      = (SNOWNCV(ix,jy)+GRPLNCV(ix,jy))/(RAINNCV(ix,jy)+1.e-12)
         ENDIF
       ENDDO
       
      ENDIF 
 
      IF ( isedonly /= 1 ) THEN
   





      call nssl_2mom_gs   &
     &  (nx,ny,nz,na,jy   &
     &  ,nor,nor          &
     &  ,dtp,dz2d       &
     &  ,t0,t1,t2,t3,t4,t5,t6,t7,t8,t9      &
     &  ,an,dn1,t77                  &
     &  ,pn,wn,0                   &
     &  ,t00,t77,                             &
     &   ventr,ventc,c1sw,1,ido,    &
     &   xdnmx,xdnmn,                  &

     &   cdx,                              &
     &   xdn0,dbz2d,timevtcalc  &
     & )






   ENDIF 
   
 
   CALL NUCOND    &
     &  (nx,ny,nz,na,jy & 
     &  ,nor,nor & 
     &  ,dtp,dz2d & 
     &  ,t0,t1,t2,t3,t4,t5,t6,t7,t8,t9 & 
     &  ,an,dn1,t77 & 
     &  ,pn,wn & 
     &  ,ssat,t00,t77,dbz2d,scion2)




     IF ( present( dbz ) .and. makediag ) THEN
   
      
      IF ( .true. ) THEN
      call radardd02(nx,ny,nz,nor,na,an,t0,         &
     &    dbz2d,dn1,nz,cnoh,rho_qh,ipconc, 0)
      ENDIF 

     
       DO kz = kts,kte
        DO ix = its,ite
         dbz(ix,kz,jy) = dbz2d(ix,1,kz)
         IF ( present( vzf ) ) THEN
           vzf(ix,kz,jy) = vzf2d(ix,1,kz)
         ENDIF
          IF ( present( compdbz ) ) THEN
            compdbz(ix,jy) = Max( compdbz(ix,jy), dbz2d(ix,1,kz) )
          ENDIF
        ENDDO
       ENDDO


       ENDIF
   

     DO il = lnb,na
      IF ( denscale(il) == 1 ) THEN
       DO kz = kts,kte
        DO ix = its,ite
         an(ix,1,kz,il) = an(ix,1,kz,il)/dn(ix,kz,jy)
        ENDDO
       ENDDO
      ENDIF
     ENDDO 
   
   

   
       DO kz = kts,kte
        DO ix = its,ite
        
         th(ix,kz,jy)  = an(ix,1,kz,lt)
         qv(ix,kz,jy)  = an(ix,1,kz,lv)
         qc(ix,kz,jy)  = an(ix,1,kz,lc)
         qr(ix,kz,jy)  = an(ix,1,kz,lr)
         qi(ix,kz,jy)  = an(ix,1,kz,li)
         qs(ix,kz,jy)  = an(ix,1,kz,ls)
         qh(ix,kz,jy)  = an(ix,1,kz,lh)
         IF ( lhl > 1 ) qhl(ix,kz,jy) = an(ix,1,kz,lhl)
         IF ( present( cn ) .and. lccn > 1 ) THEN
           cn(ix,kz,jy) = an(ix,1,kz,lccn)
         ENDIF
         IF ( ipconc >= 5 ) THEN
          ccw(ix,kz,jy) = an(ix,1,kz,lnc)
          crw(ix,kz,jy) = an(ix,1,kz,lnr)
          cci(ix,kz,jy) = an(ix,1,kz,lni)
          csw(ix,kz,jy) = an(ix,1,kz,lns)
          chw(ix,kz,jy) = an(ix,1,kz,lnh)
          IF ( lhl > 1 ) chl(ix,kz,jy) = an(ix,1,kz,lnhl)
         ENDIF




         IF ( lvh > 0 )  vhw(ix,kz,jy) = an(ix,1,kz,lvh)
         IF ( lvhl > 0 .and. present( vhl ) ) vhl(ix,kz,jy) = an(ix,1,kz,lvhl)

        ENDDO
       ENDDO
  
     ENDDO 




  RETURN
END SUBROUTINE nssl_2mom_driver




      REAL FUNCTION GAMMA(xx)

      implicit none
      real xx
      integer j



      real*8 ser,stp,tmp,x,y,cof(6)
      SAVE cof,stp
      DATA cof,stp/76.18009172947146d+0,  &
     &            -86.50532032941677d0,   &
     &             24.01409824083091d0,   &
     &             -1.231739572450155d0,  &
     &              0.1208650973866179d-2,&
     &             -0.5395239384953d-5,   &
     &              2.5066282746310005d0/

      IF ( xx <= 0.0 ) THEN
        write(0,*) 'Argument to gamma must be > 0!! xx = ',xx
        STOP
      ENDIF
      
      x = xx
      y = x
      tmp = x + 5.5d0
      tmp = (x + 0.5d0)*Log(tmp) - tmp
      ser = 1.000000000190015d0
      DO j=1,6
        y = y + 1.0d0
        ser = ser + cof(j)/y
      END DO
      gamma = Exp(tmp + log(stp*ser/x))

      RETURN
      END FUNCTION GAMMA



        real function GAMXINF(A1,X1)











        implicit none
        real :: a1,x1
        double precision :: xam,dlog,s,r,ga,t0,a,x
        integer :: k
        double precision :: gin, gim
        
        a = a1
        x = x1
        XAM=-X+A*DLOG(X)
        IF (XAM.GT.700.0.OR.A.GT.170.0) THEN
           WRITE(*,*)'a and/or x too large'
           STOP
        ENDIF
        IF (X.EQ.0.0) THEN
           GIN=0.0
           GIM = GAMMA(A1)
        ELSE IF (X.LE.1.0+A) THEN
           S=1.0D0/A
           R=S
           DO 10 K=1,60
              R=R*X/(A+K)
              S=S+R
              IF (DABS(R/S).LT.1.0D-15) GO TO 15
10         CONTINUE
15         GIN=DEXP(XAM)*S
           ga = GAMMA(A1)
           GIM=GA-GIN
        ELSE IF (X.GT.1.0+A) THEN
           T0=0.0D0
           DO 20 K=60,1,-1
              T0=(K-A)/(1.0D0+K/(X+T0))
20         CONTINUE
           GIM=DEXP(XAM)/(X+T0)


        ENDIF
        
        gamxinf = GIM
        return
        END function GAMXINF








      real FUNCTION GAML02(x) 
      implicit none
      integer ig, i, ii, n, np
      real x
      integer ng
      parameter(ng=12)
      real gamxg(ng), xg(ng)
      DATA xg/0.01,0.02,0.025,0.04,0.075,0.1,0.25,0.5,0.75,1.,2.,10./ 
      DATA gamxg/  &
     &  7.391019203578037e-8,0.02212726874591478,0.06959352407989682, &
     &  0.2355654024970809,0.46135930387500346,0.545435791452399,     &
     &  0.7371571313308203,                                           &
     &  0.8265676632204345,0.8640182781845841,0.8855756211304151,     &
     &  0.9245079225301251,                                           &
     &  0.9712578342732681/
      IF ( x .ge. xg(ng) ) THEN
        gaml02 = xg(ng)
        RETURN
      ENDIF
      IF ( x .lt. xg(1) ) THEN
        gaml02 = 0.0
        RETURN
      ENDIF
      DO ii = 1,ng-1
        i = ng - ii
        n = i
        np = n + 1
        IF ( x .ge. xg(i) ) THEN

          gaml02 = gamxg(N)+((X-XG(N))/(XG(NP)-XG(N)))* &
     &            ( gamxg(NP) - gamxg(N) ) 
          RETURN
        ENDIF
      ENDDO
      RETURN
      END FUNCTION GAML02






      real FUNCTION GAML02d300(x) 
      implicit none
      integer ig, i, ii, n, np
      real x
      integer ng
      parameter(ng=9)
      real gamxg(ng), xg(ng)
      DATA xg/0.04,0.075,0.1,0.25,0.5,0.75,1.,2.,10./ 
      DATA gamxg/                           &
     &  0.0,                                  &
     &  7.391019203578011e-8,0.0002260640810600053,  &
     &  0.16567071824457152,                         &
     &  0.4231369044918005,0.5454357914523988,       &
     &  0.6170290936864555,                           &
     &  0.7471346054110058,0.9037156157718299 /
      IF ( x .ge. xg(ng) ) THEN
        GAML02d300 = xg(ng)
        RETURN
      ENDIF
      IF ( x .lt. xg(1) ) THEN
        GAML02d300 = 0.0
        RETURN
      ENDIF
      DO ii = 1,ng-1
        i = ng - ii
        n = i
        np = n + 1
        IF ( x .ge. xg(i) ) THEN

          GAML02d300 = gamxg(N)+((X-XG(N))/(XG(NP)-XG(N)))*  &
     &            ( gamxg(NP) - gamxg(N) ) 
          RETURN
        ENDIF
      ENDDO
      RETURN
      END FUNCTION GAML02d300










      real FUNCTION GAML02d500(x) 
      implicit none
      integer ig, i, ii, n, np
      real x
      integer ng
      parameter(ng=9)
      real gamxg(ng), xg(ng)
      DATA xg/0.04,0.075,0.1,0.25,0.5,0.75,1.,2.,10./ 
      DATA gamxg/  &
     &  0.0,0.0,   &
     &  2.2346039e-13, 0.0221272687459,  &
     &  0.23556540,  0.38710348,         &
     &  0.48136183,0.6565833,            &
     &  0.86918315 /
      IF ( x .ge. xg(ng) ) THEN
        GAML02d500 = xg(ng)
        RETURN
      ENDIF
      IF ( x .lt. xg(1) ) THEN
        GAML02d500 = 0.0
        RETURN
      ENDIF
      DO ii = 1,ng-1
        i = ng - ii
        n = i
        np = n + 1
        IF ( x .ge. xg(i) ) THEN

          GAML02d500 = gamxg(N)+((X-XG(N))/(XG(NP)-XG(N)))*  &
     &            ( gamxg(NP) - gamxg(N) ) 
          RETURN
        ENDIF
      ENDDO
      RETURN
      END FUNCTION GAML02d500







        real function BETA(P,Q)










        implicit none
        double precision p1,gp,q1,gq, ppq,gpq
        real p,q
        
        p1 = p
        q1 = q
        CALL GAMMADP(P1,GP)
        CALL GAMMADP(Q1,GQ)
        PPQ=P1+Q1
        CALL GAMMADP(PPQ,GPQ)
        beta=GP*GQ/GPQ
        RETURN
        END function BETA



        SUBROUTINE GAMMADP(X,GA)









        implicit none
        
        double precision, parameter :: PI=3.141592653589793D0
        double precision :: x,ga,z,r,gr
        integer :: k,m1,m
        
        double precision :: G(26)
        
        IF (X.EQ.INT(X)) THEN
           IF (X.GT.0.0D0) THEN
              GA=1.0D0
              M1=X-1
              DO 10 K=2,M1
10               GA=GA*K
           ELSE
              GA=1.0D+300
           ENDIF
        ELSE
           IF (DABS(X).GT.1.0D0) THEN
              Z=DABS(X)
              M=INT(Z)
              R=1.0D0
              DO 15 K=1,M
15               R=R*(Z-K)
              Z=Z-M
           ELSE
              Z=X
           ENDIF
           DATA G/1.0D0,0.5772156649015329D0,                  &
     &          -0.6558780715202538D0, -0.420026350340952D-1,  &
     &          0.1665386113822915D0,-.421977345555443D-1,     &
     &          -.96219715278770D-2, .72189432466630D-2,       &
     &          -.11651675918591D-2, -.2152416741149D-3,       &
     &          .1280502823882D-3, -.201348547807D-4,          &
     &          -.12504934821D-5, .11330272320D-5,             &
     &          -.2056338417D-6, .61160950D-8,                 &
     &          .50020075D-8, -.11812746D-8,                   &
     &          .1043427D-9, .77823D-11,                       &
     &          -.36968D-11, .51D-12,                          &
     &          -.206D-13, -.54D-14, .14D-14, .1D-15/
           GR=G(26)
           DO 20 K=25,1,-1
20            GR=GR*Z+G(K)
           GA=1.0D0/(GR*Z)
           IF (DABS(X).GT.1.0D0) THEN
              GA=GA*R
              IF (X.LT.0.0D0) GA=-PI/(X*GA*DSIN(PI*X))
           ENDIF
        ENDIF
        RETURN
        END SUBROUTINE GAMMADP







      Function delbk(bb,nu,mu,k)














      implicit none
      real delbk, gamma
      real nu, mu, bb
      integer k
      
      real tmp, del
      real x1, x2, x3, x4
      integer i

        tmp = ((1.0 + nu)/mu)
        i = Int(dgami*(tmp))
        del = tmp - dgam*i
        x1 = gmoi(i) + (gmoi(i+1) - gmoi(i))*del*dgami

        tmp = ((2.0 + nu)/mu)
        i = Int(dgami*(tmp))
        del = tmp - dgam*i
        x2 = gmoi(i) + (gmoi(i+1) - gmoi(i))*del*dgami

        tmp = ((1.0 + 2.0*bb + k + nu)/mu)
        i = Int(dgami*(tmp))
        del = tmp - dgam*i
        x3 = gmoi(i) + (gmoi(i+1) - gmoi(i))*del*dgami
      




      delbk =  &
     &  ((x1/x2)**(2.0*bb + k)* &
     &    x3)/x1
      
      RETURN
      END  Function delbk
      





      Function delabk(ba,bb,nua,nub,mua,mub,k)
      
      implicit none
      real delabk, gamma
      real nua, mua, ba
      integer k
      real nub, mub, bb
      
      integer i
      real tmp,del
      
      real g1pnua, g2pnua, g1pbapnua, g1pbbpk, g1pnub, g2pnub
      
        tmp = (1. + nua)/mua
        i = Int(dgami*(tmp))
        del = tmp - dgam*i
        IF ( i+1 > ngm0 ) THEN
          write(0,*) 'delabk: i+1 > ngm0!!!!',i,ngm0,nua,mua,tmp
          STOP
        ENDIF
        g1pnua = gmoi(i) + (gmoi(i+1) - gmoi(i))*del*dgami


        tmp = ((2. + nua)/mua)
        i = Int(dgami*(tmp))
        del = tmp - dgam*i
        g2pnua = gmoi(i) + (gmoi(i+1) - gmoi(i))*del*dgami

        tmp = ((1. + ba + nua)/mua)
        i = Int(dgami*(tmp))
        del = tmp - dgam*i
        g1pbapnua = gmoi(i) + (gmoi(i+1) - gmoi(i))*del*dgami

        tmp = ((1. + nub)/mub)
        i = Int(dgami*(tmp))
        del = tmp - dgam*i
        g1pnub = gmoi(i) + (gmoi(i+1) - gmoi(i))*del*dgami

        tmp = ((2 + nub)/mub)
        i = Int(dgami*(tmp))
        del = tmp - dgam*i
        g2pnub = gmoi(i) + (gmoi(i+1) - gmoi(i))*del*dgami

        tmp = ((1. + bb + k + nub)/mub)
        i = Int(dgami*(tmp))
        del = tmp - dgam*i
        g1pbbpk = gmoi(i) + (gmoi(i+1) - gmoi(i))*del*dgami

      delabk =  &
     &  (2.*(g1pnua/g2pnua)**ba*     &
     &    g1pbapnua*                                               &
     &    (g1pnub/g2pnub)**(bb + k)*                                &
     &    g1pbbpk)/                                                &
     &  (g1pnua*g1pnub)              
      
      RETURN
      END Function delabk
      










      subroutine sediment1d(dtp,nx,ny,nz,an,na,nor,norz,xfall,dn,dz3d,dz3dinv, &
     &                    t0,t7,infdo,jslab,its,jts,  &
     &   timesed1,timesed2,timesed3,zmaxsed,timesetvt) 







      implicit none

      integer nx,ny,nz,nor,norz,ngt,jgs,na,ia
      integer id 
      integer :: its,jts 
      
      integer ng1
      parameter(ng1 = 1)

      real an(-nor+1:nx+nor,-nor+1:ny+nor,-norz+1:nz+norz,na)
      real dn(-nor+1:nx+nor,-nor+1:ny+nor,-norz+1:nz+norz)
      real dz3d(-nor+1:nx+nor,-nor+1:ny+nor,-norz+1:nz+norz)
      real dz3dinv(-nor+1:nx+nor,-nor+1:ny+nor,-norz+1:nz+norz)
      real t0(-nor+1:nx+nor,-nor+1:ny+nor,-norz+1:nz+norz)
      real t7(-nor+1:nx+nor,-nor+1:ny+nor,-norz+1:nz+norz)


      real dtp
      real xfall(nx,ny,na)  
      real xfall0(nx,ny)    
      integer infdo
      integer jslab 
            
      integer ix,jy,kz,ndfall,n,k,il,in
      real tmp, vtmax, dtptmp, dtfrac
      real, parameter :: dz = 200.

      real :: xvt(nz+1,nx,3,lc:lhab) 
      real :: tmpn(-nor+ng1:nx+nor,-nor+ng1:ny+nor,-norz+ng1:nz+norz)
      real :: tmpn2(-nor+ng1:nx+nor,-nor+ng1:ny+nor,-norz+ng1:nz+norz)
      real :: z(-nor+ng1:nx+nor,-norz+ng1:nz+norz,lr:lhab)
      real :: db1(nx,nz+1),dtz1(nz+1,nx,0:1),dz2dinv(nz+1,nx),db1inv(nx,nz+1)
      
      real :: rhovtzx(nz,nx)
      
      double precision :: timesed1,timesed2,timesed3, zmaxsed,timesetvt,dummy
      double precision :: dt1,dt2,dt3,dt4

      integer,parameter :: ngs = 128 
      integer :: ngscnt,mgs,ipconc0
      
      real ::  qx(ngs,lv:lhab) 
      real ::  qxw(ngs,ls:lhab) 
      real ::  cx(ngs,lc:lhab) 
      real ::  xv(ngs,lc:lhab) 
      real ::  vtxbar(ngs,lc:lhab,3) 
      real ::  xmas(ngs,lc:lhab) 
      real ::  xdn(ngs,lc:lhab) 
      real ::  xdia(ngs,lc:lhab,3) 
      real ::  vx(ngs,li:lhab) 
      real ::  alpha(ngs,lr:lhab) 
      real ::  zx(ngs,lr:lhab) 
      logical :: hasmass(nx,lc+1:lhab)

      integer igs(ngs),kgs(ngs)
      
      real rho0(ngs),temcg(ngs)

      real temg(ngs)
      
      real rhovt(ngs)
      
      real cwnc(ngs),cinc(ngs)
      real fadvisc(ngs),cwdia(ngs),cipmas(ngs)
      
      real cimasn,cimasx,cnina(ngs),cimas(ngs)
      
      real cnostmp(ngs)




      integer :: ixb, jyb, kzb
      integer :: ixe, jye, kze
      integer :: plo, phi

      logical :: debug_mpi = .TRUE.






      kzb = 1
      kze = nz

      ixb = 1
      ixe = nx


      jy = 1
      jgs = jy






      xvt(:,:,:,:) = 0.0

      if ( ndebug .gt. 0 ) write(0,*) 'dbg = 3a'


      DO kz = kzb,kze
      DO ix = ixb,ixe
       db1(ix,kz) = dn(ix,jy,kz)
       db1inv(ix,kz) = 1./dn(ix,jy,kz)
       rhovtzx(kz,ix) = Sqrt(rho00*db1inv(ix,kz) )
      ENDDO
      ENDDO

      DO kz = kzb,kze
      DO ix = ixb,ixe
       dtz1(kz,ix,0) = dz3dinv(ix,jy,kz)
       dtz1(kz,ix,1) = dz3dinv(ix,jy,kz)*db1inv(ix,kz) 
       dz2dinv(kz,ix) = dz3dinv(ix,jy,kz)
      ENDDO
      ENDDO

      IF ( lzh .gt. 1 ) THEN
      DO kz = kzb,kze
      DO ix = ixb,ixe
        an(ix,jy,kz,lzh) = Max( 0., an(ix,jy,kz,lzh) )
      ENDDO
      ENDDO
      ENDIF

      
      DO il = lc+1,lhab
       DO ix = ixb,ixe

       ENDDO
      ENDDO




      if (ndebug .gt. 0 ) write(0,*) 'dbg = 3a2'


      DO ix = ixb,ixe
      
      dummy = 0.d0

      
      call ziegfall1d(nx,ny,nz,nor,norz,na,dtp,jgs,ix, & 
     &  xvt, rhovtzx, & 
     &  an,dn,ipconc,t0,t7,cwmasn,cwmasx, & 
     &  cwradn, & 
     &  qxmin,xdnmx,xdnmn,cdx,cno,xdn0,xvmn,xvmx, & 
     &  ngs,qx,qxw,cx,xv,vtxbar,xmas,xdn,xdia,vx,alpha,zx,igs,kgs, &
     &  rho0,temcg,temg,rhovt,cwnc,cinc,fadvisc,cwdia,cipmas,cnina,cimas, &
     &  cnostmp,              &
     &  infdo,0               &
     & )



     DO il = lc+1,lhab







      vtmax = 0.0
      
      do kz = kzb,kze
      
      vtmax = Max(vtmax,xvt(kz,ix,1,il)*dz2dinv(kz,ix))
      vtmax = Max(vtmax,xvt(kz,ix,2,il)*dz2dinv(kz,ix))
      vtmax = Max(vtmax,xvt(kz,ix,3,il)*dz2dinv(kz,ix))










      
      ENDDO
      
      IF ( vtmax == 0.0 ) CYCLE


      
      IF ( dtp*vtmax .lt. 0.7 ) THEN 
        ndfall = 1
      ELSE
       IF ( dtp > 20.0 ) THEN 
         ndfall = Max(2, Int(dtp*vtmax/0.7) + 1)
       ELSE 
         ndfall = 1+Int(dtp*vtmax + 0.301)
       ENDIF
      ENDIF
      
      IF ( ndfall .gt. 1 ) THEN
        dtptmp = dtp/Real(ndfall)


      ELSE
        dtptmp = dtp
      ENDIF
      
      dtfrac = dtptmp/dtp


      DO n = 1,ndfall

      IF ( n .ge. 2 ) THEN



      

      dummy = 0.d0
      call ziegfall1d(nx,ny,nz,nor,norz,na,dtp,jgs,ix, & 
     &  xvt, rhovtzx, & 
     &  an,dn,ipconc,t0,t7,cwmasn,cwmasx, & 
     &  cwradn, & 
     &  qxmin,xdnmx,xdnmn,cdx,cno,xdn0,xvmn,xvmx, & 
     &  ngs,qx,qxw,cx,xv,vtxbar,xmas,xdn,xdia,vx,alpha,zx,igs,kgs, &
     &  rho0,temcg,temg,rhovt,cwnc,cinc,fadvisc,cwdia,cipmas,cnina,cimas, &
     &  cnostmp,             &
     &  infdo,il)


      ENDIF 


        IF ( il >= lr .and. ( infall .eq. 3 .or. infall .eq. 4 ) .and. ln(il) > 0 ) THEN
           IF ( (il .eq. lr .and. irfall .eq. infall .and. lzr < 1) .or. (il .ge. lh .and. lz(il) .lt. 1 ) ) THEN
            call calczgr1d(nx,ny,nz,nor,na,an,ixe,kze, & 
     &         z,db1,jgs,ipconc, dnu(il), il, ln(il), qxmin(il), xvmn(il), xvmx(il), lvol(il), xdn0(il), ix )
           ENDIF
        ENDIF

      if (ndebug .gt. 0 ) write(0,*) 'dbg = 1b'



      call fallout1d(nx,ny,nz,nor,na,dtptmp,dtfrac,jgs,xvt(1,1,1,il), & 
     &             an,db1,il,1,xfall,dtz1,ix)


      if (ndebug .gt. 0 ) write(0,*) 'dbg = 3c'



      IF ( ldovol .and. il >= li ) THEN
        IF ( lvol(il) .gt. 1 ) THEN
         call fallout1d(nx,ny,nz,nor,na,dtptmp,dtfrac,jgs,xvt(1,1,1,il), & 
     &              an,db1,lvol(il),0,xfall,dtz1,ix)
        ENDIF
      ENDIF


      if (ndebug .gt. 0 ) write(0,*) 'dbg = 3d'

      
      IF ( ipconc .gt. 0 ) THEN 
        IF ( ipconc .ge. ipc(il) ) THEN

      IF ( ( infall .ge. 2 .or. (infall .eq. 0 .and. il .lt. lh) ) .and. lz(il) .lt. 1) THEN 





        IF ( ( infall .eq. 3 .or. infall .eq. 4 ) .and. ( il .eq. lh .or. il .eq. lhl .or.  & 
     &      ( il .eq. lr .and. irfall .eq. infall) ) ) THEN

          DO kz = kzb,kze

              tmpn2(ix,jy,kz) = z(ix,kz,il)

          ENDDO
          DO kz = kzb,kze

              tmpn(ix,jy,kz) = an(ix,jy,kz,ln(il))

          ENDDO
        
        ELSE
          
          DO kz = kzb,kze

              tmpn(ix,jy,kz) = an(ix,jy,kz,ln(il))

          ENDDO

        ENDIF

      ENDIF 


      if (ndebug .gt. 0 ) write(0,*) 'dbg = 3f'

       in = 2
       IF ( infall .eq. 1 ) in = 1

         call fallout1d(nx,ny,nz,nor,na,dtptmp,dtfrac,jgs,xvt(1,1,in,il), & 
     &        an,db1,ln(il),0,xfall,dtz1,ix)


         IF ( lz(il) .lt. 1 ) THEN 
         IF ( (infall .ge. 2 .or. infall .eq. 3) .and. .not. (infall .eq. 0 .and. il .ge. lh) & 
     &       .and. ( il .eq. lr .or. (il .ge. li .and. il .le. lhab) )) THEN

           
           xfall0(:,jgs) = 0.0

           IF ( ( infall .eq. 3 .or. infall .eq. 4 ) .and.  & 
     &        ( il .ge. lh .or. (il .eq. lr .and. irfall .eq. infall) ) ) THEN
             call fallout1d(nx,ny,nz,nor,1,dtptmp,dtfrac,jgs,xvt(1,1,3,il), & 
     &         tmpn2,db1,1,0,xfall0,dtz1,ix)
             call fallout1d(nx,ny,nz,nor,1,dtptmp,dtfrac,jgs,xvt(1,1,1,il), & 
     &         tmpn,db1,1,0,xfall0,dtz1,ix)
           ELSE
             call fallout1d(nx,ny,nz,nor,1,dtptmp,dtfrac,jgs,xvt(1,1,1,il), & 
     &         tmpn,db1,1,0,xfall0,dtz1,ix)
           ENDIF

           IF ( ( infall .eq. 3 .or. infall .eq. 4 ) .and. ( (il .eq. lr .and. irfall .eq. infall) & 
     &            .or. il .ge. lh ) ) THEN


             call calcnfromz1d(nx,ny,nz,nor,na,an,tmpn2,ixe,kze, & 
     &       z,db1,jgs,ipconc, dnu(il), il, ln(il), qxmin(il), xvmn(il), xvmx(il),tmpn,  & 
     &       lvol(il), rho_qh, infall, ix)

           ELSEIF ( infall .eq. 5 .and. il .ge. lh .or. ( il == lr .and. irfall == 5 ) ) THEN

             DO kz = kzb,kze

               an(ix,jgs,kz,ln(il)) = Max( an(ix,jgs,kz,ln(il)), 0.5* ( an(ix,jgs,kz,ln(il)) + tmpn(ix,jy,kz) ))
              

             ENDDO           

           ELSEIF ( .not. (il .eq. lr .and. irfall .eq. 0) ) THEN


             DO kz = kzb,kze


               an(ix,jgs,kz,ln(il)) = Max( an(ix,jgs,kz,ln(il)), tmpn(ix,jy,kz) )
              

             ENDDO
           ENDIF 
           ENDIF 
           

         ENDIF
        ENDIF


      ENDIF 


      ENDDO 
      ENDDO 
      
      ENDDO 



      
      RETURN
      END SUBROUTINE SEDIMENT1D













      subroutine fallout1d(nx,ny,nz,nor,na,dtp,dtfrac,jgs,vt,   &
     &  a,db1,ia,id,xfall,dtz1,ixcol)







      implicit none

      integer nx,ny,nz,nor,ngt,jgs,na,ia
      integer id 
      integer ng1
      parameter(ng1 = 1)
      integer :: ixcol



      real a(-nor+ng1:nx+nor,-nor+ng1:ny+nor,-nor+ng1:nz+nor,na) 
      real vt(nz+1,nx)  
      real dtp,dtfrac
      real cmax
      real xfall(nx,ny,na)  
      real db1(nx,nz+1),dtz1(nz+1,nx,0:1)


           
      integer ix,jy,kz,n,k
      integer iv1,iv2
      real tmp
      integer imn,imx,kmn,kmx
      real qtmp1(nz+1)



      integer :: ixb, jyb, kzb
      integer :: ixe, jye, kze

      logical :: debug_mpi = .TRUE.



      jy = 1

      iv1 = 0
      iv2 = 0

      imn = nx
      imx = 1
      kmn = nz
      kmx = 1

      cmax = 0.0

      kzb = 1
      kze = nz

      ixb = ixcol
      ixe = ixcol
      ix  = ixcol

      qtmp1(nz+1) = 0.0
      
      DO kz = kzb,kze


         
         IF ( id == 1 ) THEN
         qtmp1(kz) = a(ix,jgs,kz,ia)*vt(kz,ix)*db1(ix,kz)
         ELSE
         qtmp1(kz) = a(ix,jgs,kz,ia)*vt(kz,ix)
         ENDIF
         
         IF ( a(ix,jgs,kz,ia) .ne. 0.0 ) THEN


           kmn = Min(kz,kmn)
           kmx = Max(kz,kmx)
         ENDIF

      ENDDO
      
      kmn = Max(1,kmn-1)
      




      
      IF ( kmn == 1 ) THEN
      
      kz = 1

         xfall(ix,jy,ia) = xfall(ix,jy,ia) + a(ix,jgs,kz,ia)*vt(kz,ix)*dtfrac

      
      ENDIF

      do kz = 1,nz

        a(ix,jgs,kz,ia) =  a(ix,jgs,kz,ia) + dtp*dtz1(kz,ix,id)*(qtmp1(kz+1) - qtmp1(kz) )

      enddo

      
      RETURN
      END SUBROUTINE FALLOUT1D




      subroutine calczgr1d(nx,ny,nz,nor,na,a,ixe,kze,              &
     &    z,db,jgs,ipconc, alpha, l,ln, qmin, xvmn,xvmx, lvol, rho_qh, ixcol)


      implicit none

      integer nx,ny,nz,nor,na,ngt,jgs
      integer :: ixcol
      integer, parameter :: norz = 3
      real a(-nor+1:nx+nor,-nor+1:ny+nor,-nor+1:nz+nor,na)
      real z(-nor+1:nx+nor,-nor+1:nz+nor,lr:lhab)   
      real db(nx,nz+1)  


      integer ixe,kze
      real    alpha
      real    qmin
      real    xvmn,xvmx
      integer ipconc
      integer l   
      integer ln  
      integer lvol 
      real    rho_qh


      integer ix,jy,kz
      real vr,qr,nrx,rd,xv,g1,zx,chw,xdn
      
      
      jy = jgs
      ix = ixcol
      
      IF ( l .eq. lh .or. l .eq. lhl .or. ( l .eq. lr .and. imurain == 1 )  ) THEN
      
      
      DO kz = 1,kze
          
          
          
          IF (  a(ix,jy,kz,l) .gt. qmin .and. a(ix,jy,kz,ln) .gt. 1.e-15 ) THEN
            
            IF ( lvol .gt. 1 ) THEN
                IF ( a(ix,jy,kz,lvol) .gt. 0.0 ) THEN
                  xdn = db(ix,kz)*a(ix,jy,kz,l)/a(ix,jy,kz,lvol)
                  xdn = Min( 900., Max( 170., xdn ) )
                ELSE
                  xdn = rho_qh
                ENDIF
            ELSE
                xdn = rho_qh
            ENDIF

            IF ( l == lr ) xdn = 1000.

            qr = a(ix,jy,kz,l)
            xv = db(ix,kz)*a(ix,jy,kz,l)/(xdn*a(ix,jy,kz,ln))
            chw = a(ix,jy,kz,ln)

             IF ( xv .lt. xvmn .or. xv .gt. xvmx ) THEN
              xv = Min( xvmx, Max( xvmn,xv ) )
              chw = db(ix,kz)*a(ix,jy,kz,l)/(xv*xdn)
             ENDIF

             g1 = (6.0 + alpha)*(5.0 + alpha)*(4.0 + alpha)/  &
     &            ((3.0 + alpha)*(2.0 + alpha)*(1.0 + alpha))
             zx = g1*db(ix,kz)**2*(a(ix,jy,kz,l))*a(ix,jy,kz,l)/chw

             z(ix,kz,l)  = zx*(6./(pi*1000.))**2





          
          ELSE
           
            z(ix,kz,l) = 0.0
           
          ENDIF
          
      ENDDO
      
      ELSEIF ( l .eq. lr .and. imurain == 3) THEN

      xdn = 1000.
      
      DO kz = 1,kze
          IF (  a(ix,jy,kz,l) .gt. qmin .and. a(ix,jy,kz,ln) .gt. 1.e-15 ) THEN

            vr = db(ix,kz)*a(ix,jy,kz,l)/(xdn*a(ix,jy,kz,ln))

            z(ix,kz,l) = 3.6*(rnu+2.0)*a(ix,jy,kz,ln)*vr**2/(rnu+1.0)


          
          ELSE
           
            z(ix,kz,l) = 0.0
           
          ENDIF
      
          
      ENDDO
      
      ENDIF
      
      RETURN
      
      END subroutine calczgr1d









      subroutine calcnfromz1d(nx,ny,nz,nor,na,a,t0,ixe,kze,    &
     &    z0,db,jgs,ipconc, alpha, l,ln, qmin, xvmn,xvmx,t1, &
     &    lvol, rho_qh, infall, ixcol)

      
      implicit none

      integer nx,ny,nz,nor,na,ngt,jgs,ixcol

      real a(-nor+1:nx+nor,-nor+1:ny+nor,-nor+1:nz+nor,na)  
      real t0(-nor+1:nx+nor,-nor+1:ny+nor,-nor+1:nz+nor)    
      real t1(-nor+1:nx+nor,-nor+1:ny+nor,-nor+1:nz+nor)    

      real z0(-nor+1:nx+nor,-nor+1:nz+nor,lr:lhab)   

      real db(nx,nz+1)  
      
      integer ixe,kze
      real    alpha
      real    qmin
      real    xvmn,xvmx
      integer ipconc
      integer l   
      integer ln  
      integer lvol 
      real    rho_qh
      integer infall
      
      
      integer ix,jy,kz
      double precision vr,qr,nrx,rd,g1,zx,chw,z,znew,zt,zxt
      real xv,xdn
      integer :: ndbz, nmwgt, nnwgt, nwlessthanz
      
      ndbz = 0
      nmwgt = 0
      nnwgt = 0
      nwlessthanz = 0
      

      
      jy = jgs
      ix = ixcol
      
      IF ( l .eq. lh .or. l .eq. lhl .or. ( l == lr .and. imurain == 1 ) ) THEN
      
             g1 = (6.0 + alpha)*(5.0 + alpha)*(4.0 + alpha)/  &
     &            ((3.0 + alpha)*(2.0 + alpha)*(1.0 + alpha))
      
      DO kz = 1,kze

         
          IF (   t0(ix,jy,kz) .gt. 0. ) THEN 
            
            IF ( lvol .gt. 1 ) THEN
               IF ( a(ix,jy,kz,lvol) .gt. 0.0 ) THEN
                 xdn = db(ix,kz)*a(ix,jy,kz,l)/a(ix,jy,kz,lvol)
                 xdn = Min( 900., Max( 170., xdn ) )
               ELSE 
                 xdn = rho_qh
               ENDIF
            ELSE
               xdn = rho_qh
            ENDIF
            
            IF ( l == lr ) xdn = 1000.
          
            qr = a(ix,jy,kz,l)
            xv = db(ix,kz)*a(ix,jy,kz,l)/(xdn*a(ix,jy,kz,ln))
            chw = a(ix,jy,kz,ln)

             IF ( xv .lt. xvmn .or. xv .gt. xvmx ) THEN
              xv = Min( xvmx, Max( xvmn,xv ) )
              chw = db(ix,kz)*a(ix,jy,kz,l)/(xv*xdn)
             ENDIF

             zx = g1*db(ix,kz)**2*( a(ix,jy,kz,l))*a(ix,jy,kz,l)/chw
             z  = zx*(6./(pi*1000.))**2

            
           IF ( (z .gt. t0(ix,jy,kz) .and. z .gt. 0.0 .and.  &
     &           t0(ix,jy,kz) .gt. z0(ix,kz,l) )) THEN 
           
            zx = t0(ix,jy,kz)/((6./(pi*1000.))**2)
            
            nrx =  g1*db(ix,kz)**2*( a(ix,jy,kz,l))*a(ix,jy,kz,l)/zx
            IF ( infall .eq. 3 ) THEN
              IF ( nrx .gt. a(ix,jy,kz,ln) ) THEN
                ndbz = ndbz + 1
                IF ( t1(ix,jy,kz) .lt. ndbz ) nwlessthanz = nwlessthanz + 1
              ELSE
                nnwgt = nnwgt + 1
              ENDIF
              a(ix,jy,kz,ln) = Max( real(nrx), a(ix,jy,kz,ln) )
            ELSE
             IF (  nrx .gt. a(ix,jy,kz,ln) .and. t1(ix,jy,kz) .gt. a(ix,jy,kz,ln) ) THEN
              IF ( nrx .lt. t1(ix,jy,kz)  ) THEN
                ndbz = ndbz + 1
              ELSE
                nmwgt = nmwgt + 1
                IF ( t1(ix,jy,kz) .lt. ndbz ) nwlessthanz = nwlessthanz + 1
              ENDIF
             ELSE
              nnwgt = nnwgt + 1
             ENDIF
              
              a(ix,jy,kz,ln) = Max(Min( real(nrx), t1(ix,jy,kz) ), a(ix,jy,kz,ln) )
            ENDIF

           ELSE 
             IF ( t1(ix,jy,kz) .gt. 0 .and. a(ix,jy,kz,ln) .gt. 0 ) THEN
              IF ( t1(ix,jy,kz) .gt. a(ix,jy,kz,ln) ) THEN
                nmwgt = nmwgt + 1
              ELSE
                nnwgt = nnwgt + 1
              ENDIF
            ENDIF
            a(ix,jy,kz,ln) = Max(t1(ix,jy,kz), a(ix,jy,kz,ln) )
            nrx = a(ix,jy,kz,ln)



           ENDIF 

           
          ELSE 
            IF ( t1(ix,jy,kz) .gt. 0 .and. a(ix,jy,kz,ln) .gt. 0 ) THEN
              IF ( t1(ix,jy,kz) .gt. a(ix,jy,kz,ln) ) THEN
                nmwgt = nmwgt + 1
              ELSE
                nnwgt = nnwgt + 1
              ENDIF
            ENDIF
          ENDIF
          
      ENDDO
      
      
      ELSEIF ( l .eq. lr .and. imurain == 3) THEN

      xdn = 1000.
      
      DO kz = 1,kze
          IF (  t0(ix,jy,kz) .gt. 0. ) THEN

            vr = db(ix,kz)*a(ix,jy,kz,l)/(xdn*a(ix,jy,kz,ln))
            z = 3.6*(rnu+2.0)*a(ix,jy,kz,ln)*vr**2/(rnu+1.0)
          
             IF ( z .gt. t0(ix,jy,kz) .and. z .gt. 0.0 .and.  &
     &          t0(ix,jy,kz) .gt. 0.0                         &
     &          .and. t0(ix,jy,kz) .gt. z0(ix,kz,l) ) THEN

            vr = db(ix,kz)*a(ix,jy,kz,l)/(xdn)
             chw =  a(ix,jy,kz,ln)
            nrx =   3.6*(rnu+2.0)*vr**2/((rnu+1.0)*t0(ix,jy,kz))
             IF ( infall .eq. 3 ) THEN
              a(ix,jy,kz,ln) = Max( real(nrx), a(ix,jy,kz,ln) )
            ELSEIF ( infall .eq. 4 ) THEN
              a(ix,jy,kz,ln) = Max( Min( real(nrx), t1(ix,jy,kz)), a(ix,jy,kz,ln) )
            ENDIF

           ELSE

            a(ix,jy,kz,ln) = Max(t1(ix,jy,kz), a(ix,jy,kz,ln) )

           ENDIF

          ELSE

            a(ix,jy,kz,ln) = Max(t1(ix,jy,kz), a(ix,jy,kz,ln) )

          ENDIF


      ENDDO

      ENDIF

      RETURN

      END subroutine calcnfromz1d









      subroutine calcnfromq(nx,ny,nz,an,na,nor,norz,dn)

      
      implicit none

      integer nx,ny,nz,nor,norz,na,ngt,jgs,ixcol

      real an(-nor+1:nx+nor,-nor+1:ny+nor,-norz+1:nz+norz,na)  

      real dn(nx,nz+1)  
      
      integer ixe,kze
      real    alpha
      real    qmin
      real    xvmn,xvmx
      integer ipconc
      integer lvol 
      real    rho_qh
      integer infall
      
      
      integer ix,jy,kz
      double precision vr,q,nrx,rd,g1h,g1r,g1s,zx,chw,z,znew,zt,zxt,n1,laminv1
      double precision :: zr, zs, zh, dninv
      real, parameter :: xn0s = 3.0e6, xn0r = 8.0e6, xn0h = 4.0e4
      real, parameter :: xdnr = 1000., xdns = 100. ,xdnh = 900.0
      real, parameter :: zhfac = 1./(pi*xdnh*xn0h)
      real, parameter :: zrfac = 1./(pi*xdnr*xn0r)
      real, parameter :: zsfac = 1./(pi*xdns*xn0s)
      real, parameter :: g0 = (6.0)*(5.0)*(4.0)/((3.0)*(2.0)*(1.0))

      real xv,xdn
      integer :: ndbz, nmwgt, nnwgt, nwlessthanz


      
      
      jy = 1
      
      
         g1h = (6.0 + alphah)*(5.0 + alphah)*(4.0 + alphah)/  &
     &        ((3.0 + alphah)*(2.0 + alphah)*(1.0 + alphah))
     
         IF ( imurain == 3 ) THEN
         g1r = (rnu+2.0)/(rnu+1.0)
         ELSE 
         g1r = (6.0 + alphar)*(5.0 + alphar)*(4.0 + alphar)/  &
     &        ((3.0 + alphar)*(2.0 + alphar)*(1.0 + alphar))
         ENDIF

         g1s = (snu+2.0)/(snu+1.0)
      
      DO kz = 1,nz
       DO ix = 1,nx 

         dninv = 1./dn(ix,kz)
         
   
         
         IF ( lnc > 1 ) THEN
           IF ( an(ix,jy,kz,lnc) <= 0.0 .and. an(ix,jy,kz,lc) > qxmin(lc) ) THEN
             an(ix,jy,kz,lnc) = qccn
           ENDIF
         ENDIF

   
         
         IF ( lnr > 1 ) THEN
           IF ( an(ix,jy,kz,lnr) <= 0.0 .and. an(ix,jy,kz,lr) > qxmin(lr) ) THEN

             q = an(ix,jy,kz,lr)
             
             laminv1 = (dn(ix,kz) * q * zrfac)**(0.25)  
             
             n1 = laminv1*xn0h  
             
             nrx =  n1*g1r/g0   

             an(ix,jy,kz,lnr) = nrx 
             
           ENDIF
         ENDIF

  
         IF ( lns > 1 ) THEN
           IF ( an(ix,jy,kz,lns) <= 0.0 .and. an(ix,jy,kz,ls) > qxmin(ls) ) THEN

             q = an(ix,jy,kz,ls)
             
             laminv1 = (dn(ix,kz) * q * zsfac)**(0.25)  
             
             n1 = laminv1*xn0s  
             
             nrx =  n1*g1s/g0   

             an(ix,jy,kz,lns) = nrx 
             
           ENDIF
         ENDIF
         
    

         IF ( lnh > 1 ) THEN
           IF ( an(ix,jy,kz,lnh) <= 0.0 .and. an(ix,jy,kz,lh) > qxmin(lh) ) THEN
             IF ( lvh > 1 ) THEN
               IF ( an(ix,jy,kz,lvh) <= 0.0 ) THEN
                 an(ix,jy,kz,lvh) = an(ix,jy,kz,lh)/xdnh
               ENDIF
             ENDIF

             q = an(ix,jy,kz,lh)
             
             laminv1 = (dn(ix,kz) * q * zhfac)**(0.25)  
             
             n1 = laminv1*xn0h  
             
             nrx =  n1*g1h/g0   

             an(ix,jy,kz,lnh) = nrx 

           ENDIF
         ENDIF
 
      ENDDO 
      ENDDO 
      
      RETURN
      
      END subroutine calcnfromq








   SUBROUTINE NUCOND    &
     &  (nx,ny,nz,na,jyslab & 
     &  ,nor,norz & 
     &  ,dtp,dz3d & 
     &  ,t0,t1,t2,t3,t4,t5,t6,t7,t8,t9 & 
     &  ,an,dn,p2 & 
     &  ,pn,w & 
     &  ,ssfilt,t00,t77,tmp3d,scion)

   implicit none

      integer :: nx,ny,nz,na
      integer :: ng
      integer :: nor,norz, jyslab 
      real    :: dtp  





      real t00(-nor+1:nx+nor,-nor+1:ny+nor,-norz+1:nz+norz)
      real t77(-nor+1:nx+nor,-nor+1:ny+nor,-norz+1:nz+norz)

      real t0(-nor+1:nx+nor,-nor+1:ny+nor,-norz+1:nz+norz)
      real t1(-nor+1:nx+nor,-nor+1:ny+nor,-norz+1:nz+norz)
      real t2(-nor+1:nx+nor,-nor+1:ny+nor,-norz+1:nz+norz)
      real t3(-nor+1:nx+nor,-nor+1:ny+nor,-norz+1:nz+norz)
      real t4(-nor+1:nx+nor,-nor+1:ny+nor,-norz+1:nz+norz)
      real t5(-nor+1:nx+nor,-nor+1:ny+nor,-norz+1:nz+norz)
      real t6(-nor+1:nx+nor,-nor+1:ny+nor,-norz+1:nz+norz)
      real t7(-nor+1:nx+nor,-nor+1:ny+nor,-norz+1:nz+norz)
      real t8(-nor+1:nx+nor,-nor+1:ny+nor,-norz+1:nz+norz)
      real t9(-nor+1:nx+nor,-nor+1:ny+nor,-norz+1:nz+norz)
      

      real p2(-nor+1:nx+nor,-nor+1:ny+nor,-norz+1:nz+norz)  
      real pn(-nor+1:nx+nor,-nor+1:ny+nor,-norz+1:nz+norz)
      real an(-nor+1:nx+nor,-nor+1:ny+nor,-norz+1:nz+norz,na)
      real dn(-nor+1:nx+nor,-nor+1:ny+nor,-norz+1:nz+norz)

      real w(-nor+1:nx+nor,-nor+1:ny+nor,-norz+1:nz+norz)


      real ssfilt(-nor+1:nx+nor,-nor+1:ny+nor,-norz+1:nz+norz)
      
      real dz3d(-nor+1:nx+nor,-nor+1:ny+nor,-norz+1:nz+norz)

      real tmp3d(-nor+1:nx+nor,-nor+1:ny+nor,-norz+1:nz+norz)

      real scion(-nor+1:nx+nor,-nor+1:ny+nor,-norz+1:nz+norz,2)
      
    




      integer nxmpb,nzmpb,nxz
      integer mgs,ngs,numgs,inumgs
      parameter (ngs=500)
      integer ngscnt,igs(ngs),kgs(ngs)
      integer kgsp(ngs),kgsm(ngs)
      integer nsvcnt
      
      integer ix,kz,i,n, kp1
      integer :: jy, jgs
      integer ixb,ixe,jyb,jye,kzb,kze
    
      integer itile,jtile,ktile
      integer ixend,jyend,kzend,kzbeg
      integer nxend,nyend,nzend,nzbeg






      real ccnc(ngs), ccna(ngs), cnuc(ngs), cwnccn(ngs)
      real sscb  
      parameter ( sscb = 2.0 )
      integer idecss  
      parameter ( idecss = 1 )
      integer iba 
                  
                  
      parameter (iba = 1)
      integer ifilt   
      parameter ( ifilt = 0 ) 
      real temp1,temp2 
      real ssmax(ngs)       
      real ssmx
      real dnnet,dqnet


      real ventrx(ngs)
      real ventrxn(ngs)
      real volb, t2s
      real, parameter :: aa1 = 9.44e15, aa2 = 5.78e3  

      real ec0, ex1, ft, rhoinv(ngs)
      
      real chw, g1, rd1

      real ac1,bc, taus, c1,d1,e1,f1,p380,tmp,tmp2 
      real x,y,del,r,alpr
      double precision :: vent1,vent2
      real g1palp
      real bs
      real v1, v2
      real d1r, d1i, d1s, e1i
      integer nc 
      real dtcon,dtcon1,dtcon2 
      real delta
      integer ltemq1,ltemq1m 
      real dqv,qv1,ss1,ss2,qvs1,dqvs,dtemp,dt1   

      real ssi1, ssi2, dqvi, dqvis, dqvii,qis1
      real dqvr, dqc, dqr, dqi, dqs
      real qv1m,qvs1m,ss1m,ssi1m,qis1m
      real cwmastmp 
      real  dcloud,dcloud2 
      real cn(ngs) 

      integer ltemq
      
      integer il

      real  es(ngs) 
      real  eis(ngs)
      real ssf(ngs),ssfkp1(ngs),ssfkm1(ngs),ssat0(ngs)
      real ssfjp1(ngs),ssfjm1(ngs)
      real ssfip1(ngs),ssfim1(ngs)

      real supcb, supmx
      parameter (supcb=0.5,supmx=238.0)
      real r2dxm, r2dym, r2dzm
      real dssdz, dssdy, dssdx

      real epsi,d
      parameter (epsi = 0.622, d = 0.266)
      real r1,qevap 
      
      real vr,nrx,qr,z1,z2,rdi,alp,xnutmp,xnuc
      real ctmp, ccwtmp
      real f5, qvs0  
      real    :: t0p1, t0p3
      real qvex
      

      real dqvcnd(ngs),dqwv(ngs),dqcw(ngs),dqci(ngs)
      real temp(ngs),tempc(ngs)
      real temg(ngs),temcg(ngs),theta(ngs),qvap(ngs) 
      real temgx(ngs),temcgx(ngs)
      real qvs(ngs),qis(ngs),qss(ngs),pqs(ngs)
      real felv(ngs),felf(ngs),fels(ngs)
      real felvcp(ngs)
      real gamw(ngs),gams(ngs)   
      real tsqr(ngs),ssi(ngs),ssw(ngs)
      real cc3(ngs),cqv1(ngs),cqv2(ngs)
      real qcwtmp(ngs),qtmp

      real fvent(ngs) 
      real fwvdf(ngs),ftka(ngs),fthdf(ngs)
      real fadvisc(ngs),fakvisc(ngs)
      real fci(ngs),fcw(ngs)
      real fschm(ngs),fpndl(ngs)

      real pres(ngs)
      real pk(ngs)
      real rho0(ngs),pi0(ngs)
      real rhovt(ngs)
      real thetap(ngs),theta0(ngs),qwvp(ngs),qv0(ngs)
      real thsave(ngs)
      real qss0(ngs)
      real fcqv1(ngs)
      real wvel(ngs),wvelkm1(ngs)

      real wvdf(ngs),tka(ngs)
      real advisc(ngs)

      real rwvent(ngs)
      

      real :: qx(ngs,lv:lhab)
      real :: cx(ngs,lc:lhab)
      real :: xv(ngs,lc:lhab)
      real :: xmas(ngs,lc:lhab)
      real :: xdn(ngs,lc:lhab)
      real :: xdia(ngs,lc:lhab,3)
      real :: alpha(ngs,lr:lhab)
      real :: zx(ngs,lr:lhab)


      logical zerocx(lc:lqmx)

      integer, parameter :: iunit = 0
      
      real :: frac, hwdn, tmpg
      
      real :: cvm


      itile = nx
      jtile = ny
      ktile = nz
      ixend = nx
      jyend = ny
      kzend = nz
      nxend = nx + 1
      nyend = ny + 1
      nzend = nz
      kzbeg = 1
      nzbeg = 1

      jy = 1
      
      IF ( ipconc <= 1 .or. isedonly == 2 ) GOTO 2200





      ssfilt(:,:,:) = 0.0

      do kz = 1,nz
        do ix = 1,nx

         temp1 = an(ix,jy,kz,lt)*t77(ix,jy,kz)
          t0(ix,jy,kz) = temp1
          ltemq = Int( (temp1-163.15)/fqsat+1.5 )
         ltemq = Min( nqsat, Max(1,ltemq) )

          c1 = t00(ix,jy,kz)*tabqvs(ltemq)

          ssfilt(ix,jy,kz) = 100.*(an(ix,jy,kz,lv)/c1 - 1.0)  


        ENDDO
      ENDDO






      jgs = jy




      if ( ndebug .gt. 0 ) write(0,*) 'ICEZVD_DR: Gather stage'

      nxmpb = 1
      nzmpb = 1
      nxz = nx*nz
      numgs = nxz/ngs + 1


      do 2000 inumgs = 1,numgs

      ngscnt = 0


      kzb = nzmpb
      kze = nz
 

      ixb = nxmpb
      ixe = itile


      do kz = kzb,kze
      do ix = nxmpb,nx

      pqs(1) = 380.0/pn(ix,jy,kz)
      theta(1) = an(ix,jy,kz,lt)
      temg(1) = t0(ix,jy,kz)

      temcg(1) = temg(1) - tfr
      ltemq = (temg(1)-163.15)/fqsat+1.5
      ltemq = Min( nqsat, Max(1,ltemq) )
      qvs(1) = pqs(1)*tabqvs(ltemq)
      qis(1) = pqs(1)*tabqis(ltemq)

      qss(1) = qvs(1)


      if ( temg(1) .lt. tfr ) then
      end if

      if ( (temg(1) .gt. tfrh ) .and.  &
     &   ( an(ix,jy,kz,lv)  .gt. qss(1) .or. &
     &     an(ix,jy,kz,lc)  .gt. qxmin(lc)   .or.  &
     &     ( an(ix,jy,kz,lr)  .gt. qxmin(lr) .and. rcond == 2 )  &
     &     )) then
      ngscnt = ngscnt + 1
      igs(ngscnt) = ix
      kgs(ngscnt) = kz
      if ( ngscnt .eq. ngs ) goto 2100
      end if

      end do  

      nxmpb = 1
      end do  

 2100 continue

      if ( ngscnt .eq. 0 ) go to 29998

      if (ndebug .gt. 0 ) write(0,*) 'ICEZVD_DR: dbg = 8'

      
      qx(:,:) = 0.0
      cx(:,:) = 0.0

      xv(:,:) = 0.0
      xmas(:,:) = 0.0

      alpha(:,lr) = xnu(lr)




      DO mgs = 1,ngscnt
      qx(mgs,lv) = an(igs(mgs),jy,kgs(mgs),lv)
       DO il = lc,lhab
        qx(mgs,il) = max(an(igs(mgs),jy,kgs(mgs),il), 0.0)
       ENDDO

       qcwtmp(mgs) = qx(mgs,lc)


      theta0(mgs) = an(igs(mgs),jy,kgs(mgs),lt) 
      thetap(mgs) = 0.0
      theta(mgs) = an(igs(mgs),jy,kgs(mgs),lt)
      qv0(mgs) =  qx(mgs,lv)
      qwvp(mgs) = qx(mgs,lv) - qv0(mgs)

       pres(mgs) = pn(igs(mgs),jy,kgs(mgs))
       rho0(mgs) = dn(igs(mgs),jy,kgs(mgs))
       rhoinv(mgs) = 1.0/rho0(mgs)
       rhovt(mgs) = Sqrt(rho00/rho0(mgs))
       pi0(mgs) = p2(igs(mgs),jy,kgs(mgs))
       temg(mgs) = t0(igs(mgs),jy,kgs(mgs))
       pk(mgs) = t77(igs(mgs),jy,kgs(mgs)) 
       temcg(mgs) = temg(mgs) - tfr
       qss0(mgs) = (380.0)/(pres(mgs))
       pqs(mgs) = (380.0)/(pres(mgs))
       ltemq = (temg(mgs)-163.15)/fqsat+1.5
       ltemq = Min( nqsat, Max(1,ltemq) )
       qvs(mgs) = pqs(mgs)*tabqvs(ltemq)
       qis(mgs) = pqs(mgs)*tabqis(ltemq)

        qvap(mgs) = max( (qwvp(mgs) + qv0(mgs)), 0.0 )
        es(mgs) = 6.1078e2*tabqvs(ltemq)
        qss(mgs) = qvs(mgs)


        temgx(mgs) = min(temg(mgs),313.15)
        temgx(mgs) = max(temgx(mgs),233.15)
        felv(mgs) = 2500837.367 * (273.15/temgx(mgs))**((0.167)+(3.67e-4)*temgx(mgs))

        IF ( eqtset <= 1 ) THEN
          felvcp(mgs) = felv(mgs)*cpi
        ELSE 
          tmp = qx(mgs,li)+qx(mgs,ls)+qx(mgs,lh)
          IF ( lhl > 1 ) tmp = tmp + qx(mgs,lhl)
          cvm = cv+cvv*qx(mgs,lv)+cpl*(qx(mgs,lc)+qx(mgs,lr))   &
                                  +cpigb*(tmp)
          felvcp(mgs) = (felv(mgs)-rw*temg(mgs))/cvm
        ENDIF

        temcgx(mgs) = min(temg(mgs),273.15)
        temcgx(mgs) = max(temcgx(mgs),223.15)
        temcgx(mgs) = temcgx(mgs)-273.15
        felf(mgs) = 333690.6098 + (2030.61425)*temcgx(mgs) - (10.46708312)*temcgx(mgs)**2

        fels(mgs) = felv(mgs) + felf(mgs)
        fcqv1(mgs) = 4098.0258*felv(mgs)*cpi

      wvdf(mgs) = (2.11e-05)*((temg(mgs)/tfr)**1.94)* &
     &  (101325.0/pn(igs(mgs),jgs,kgs(mgs)))                            
      advisc(mgs) = advisc0*(416.16/(temg(mgs)+120.0))* &
     &  (temg(mgs)/296.0)**(1.5)                         
      tka(mgs) = tka0*advisc(mgs)/advisc1                 


      ENDDO






      if ( ipconc .ge. 1 ) then
       do mgs = 1,ngscnt
        cx(mgs,li) = Max(an(igs(mgs),jy,kgs(mgs),lni), 0.0)
       end do
      end if
      if ( ipconc .ge. 2 ) then
       do mgs = 1,ngscnt
        cx(mgs,lc) = Max(an(igs(mgs),jy,kgs(mgs),lnc), 0.0)
        cwnccn(mgs) = cwccn*rho0(mgs)/rho00
        cn(mgs) = 0.0
        IF ( lccn .gt. 1 ) THEN
          ccnc(mgs) = an(igs(mgs),jy,kgs(mgs),lccn)
        ELSE
          ccnc(mgs) = cwnccn(mgs)
        ENDIF
        IF ( lccna > 1 ) THEN
          ccna(mgs) = an(igs(mgs),jy,kgs(mgs),lccna)
        ELSE
          IF ( lccn > 1 ) THEN
            ccna(mgs) = cwnccn(mgs) - ccnc(mgs)
          ELSE
            ccna(mgs) = cx(mgs,lc) 
          ENDIF
        ENDIF
       end do
      end if
      if ( ipconc .ge. 3 ) then
       do mgs = 1,ngscnt
        cx(mgs,lr) = Max(an(igs(mgs),jy,kgs(mgs),lnr), 0.0)
       end do
      end if

        cnuc(1:ngscnt) = cwccn*(1. - renucfrac) + ccnc(1:ngscnt)*renucfrac




      if (ndebug .gt. 0 ) write(0,*) 'ICEZVD_DR: Set density'

      do mgs = 1,ngscnt
        xdn(mgs,lc) = xdn0(lc)
        xdn(mgs,lr) = xdn0(lr)
      end do

      ventrx(:) = ventr
      ventrxn(:) = ventrn
      


      
      DO mgs = 1,ngscnt
      
      kp1 = Min(nz, kgs(mgs)+1 )
      wvel(mgs) = (0.5)*(w(igs(mgs),jgs,kp1) & 
     &                  +w(igs(mgs),jgs,kgs(mgs)))
      wvelkm1(mgs) = (0.5)*(w(igs(mgs),jgs,kgs(mgs)) & 
     &                  +w(igs(mgs),jgs,Max(1,kgs(mgs)-1)))

      ssat0(mgs)  = ssfilt(igs(mgs),jgs,kgs(mgs))
      ssf(mgs)    = ssfilt(igs(mgs),jgs,kgs(mgs))
      
      ssfkp1(mgs) = ssfilt(igs(mgs),jgs,Min(nz-1,kgs(mgs)+1))
      ssfkm1(mgs) = ssfilt(igs(mgs),jgs,Max(1,kgs(mgs)-1))


      ENDDO







      if ( ndebug .gt. 0 )write(0,*) 'ICEZVD_DR: Set cloud water variables'

      do mgs = 1,ngscnt
      xv(mgs,lc) = 0.0
      IF ( ipconc .ge. 2 .and. cx(mgs,lc) .gt. 1.0e6 ) THEN
        xmas(mgs,lc) = &
     &    min( max(qx(mgs,lc)*rho0(mgs)/cx(mgs,lc),cwmasn),cwmasx )
        xv(mgs,lc) = xmas(mgs,lc)/xdn(mgs,lc)
      ELSE
       IF ( qx(mgs,lc) .gt. qxmin(lc) .and. cx(mgs,lc) .gt. 0.01 ) THEN
        xmas(mgs,lc) = &
     &     min( max(qx(mgs,lc)*rho0(mgs)/cx(mgs,lc),xdn(mgs,lc)*xvmn(lc)), &
     &      xdn(mgs,lc)*xvmx(lc) )

        cx(mgs,lc) = qx(mgs,lc)*rho0(mgs)/xmas(mgs,lc)

       ELSEIF ( qx(mgs,lc) .gt. qxmin(lc) .and. cx(mgs,lc) .le. 0.01 ) THEN
        xmas(mgs,lc) = xdn(mgs,lc)*4.*pi/3.*(5.0e-6)**3
        cx(mgs,lc) = rho0(mgs)*qx(mgs,lc)/xmas(mgs,lc)

       ELSE
        xmas(mgs,lc) = cwmasn
       ENDIF
      ENDIF
      xdia(mgs,lc,1) = (xmas(mgs,lc)*cwc1)**c1f3


      end do



      do mgs = 1,ngscnt
      if ( qx(mgs,lr) .gt. qxmin(lr) ) then

      if ( ipconc .ge. 3 ) then
        xv(mgs,lr) = rho0(mgs)*qx(mgs,lr)/(xdn(mgs,lr)*Max(1.0e-9,cx(mgs,lr)))

        IF ( xv(mgs,lr) .gt. xvmx(lr) ) THEN
          xv(mgs,lr) = xvmx(lr)
          cx(mgs,lr) = rho0(mgs)*qx(mgs,lr)/(xvmx(lr)*xdn(mgs,lr))
        ELSEIF ( xv(mgs,lr) .lt. xvmn(lr) ) THEN
          xv(mgs,lr) = xvmn(lr)
          cx(mgs,lr) = rho0(mgs)*qx(mgs,lr)/(xvmn(lr)*xdn(mgs,lr))
        ENDIF

        xmas(mgs,lr) = xv(mgs,lr)*xdn(mgs,lr)
        xdia(mgs,lr,3) = (xmas(mgs,lr)*cwc1)**(1./3.) 
        IF ( imurain == 3 ) THEN

          xdia(mgs,lr,1) = xdia(mgs,lr,3) 
        ELSE 
          xdia(mgs,lr,1) = (6.*piinv*xv(mgs,lr)/((alpha(mgs,lr)+3.)*(alpha(mgs,lr)+2.)*(alpha(mgs,lr)+1.)))**(1./3.)
        ENDIF






      ELSE
        xdia(mgs,lr,1) = &
     &  (qx(mgs,lr)*rho0(mgs)/(pi*xdn(mgs,lr)*cno(lr)))**(0.25)
      end if
      else
        xdia(mgs,lr,1) = 1.e-9

      end if

      end do





      do mgs = 1,ngscnt


      fadvisc(mgs) = advisc0*(416.16/(temg(mgs)+120.0))* & 
     &  (temg(mgs)/296.0)**(1.5)

      fakvisc(mgs) = fadvisc(mgs)*rhoinv(mgs)

      fwvdf(mgs) = (2.11e-05)*((temg(mgs)/tfr)**1.94)* & 
     &  (101325.0/(pres(mgs)))
      
      fschm(mgs) = (fakvisc(mgs)/fwvdf(mgs))

      fvent(mgs) = (fschm(mgs)**(1./3.)) * (fakvisc(mgs)**(-0.5))

      end do








      DO mgs=1,ngscnt
        dcloud = 0.0
        IF ( temg(mgs) .le. tfrh ) THEN


         CYCLE
        ENDIF

      IF( ssat0(mgs) .GT. 0. .OR. ssf(mgs) .GT. 0. ) GO TO 620





      IF ( qx(mgs,lc) .LE. 0. ) GO TO 631


      R1=1./(1. + caw*(273.15 - cbw)*qss(mgs)*felv(mgs)/ &
     &            (cp*(temg(mgs) - cbw)**2))
      QEVAP= Min( qx(mgs,lc), R1*(qss(mgs)-qvap(mgs)) )


      IF ( qx(mgs,lc) .LT. QEVAP ) THEN 
        qwvp(mgs) = qwvp(mgs) + qx(mgs,lc)
        thetap(mgs) = thetap(mgs) - felv(mgs)*qx(mgs,lc)/(cp*pi0(mgs))


        qx(mgs,lc) = 0.
        cx(mgs,lc) = 0.
      ELSE
        qwvp(mgs) = qwvp(mgs) + QEVAP
        qx(mgs,lc) = qx(mgs,lc) - QEVAP
        IF ( qx(mgs,lc) .le. 0. ) cx(mgs,lc) = 0.
        thetap(mgs) = thetap(mgs) - felv(mgs)*QEVAP/(CP*pi0(mgs))


      ENDIF

      GO TO 631


  620 CONTINUE



        IF ( qx(mgs,lc) .GT. qxmin(lc) .and. cx(mgs,lc) .ge. 1. ) THEN






       ac1 =  felv(mgs)**2/(tka(mgs)*rw*temg(mgs)**2)





       bc =   rw*temg(mgs)/(wvdf(mgs)*es(mgs))








      IF ( ssf(mgs) .gt. 0.0 .or. ssat0(mgs) .gt. 0.0 ) THEN
       IF ( ny .le. 2 ) THEN


       ENDIF



       IF ( qx(mgs,lc) .gt. qxmin(lc) ) THEN

         IF ( xdia(mgs,lc,1) .le. 0.0 ) THEN
          xmas(mgs,lc) = cwmasn
          xdia(mgs,lc,1) = (xmas(mgs,lc)*cwc1)**c1f3
         ENDIF
        d1 = (1./(ac1 + bc))*4.0*pi*ventc &
     &        *0.5*xdia(mgs,lc,1)*cx(mgs,lc)*rhoinv(mgs)

       ELSE
         d1 = 0.0
       ENDIF

       IF ( rcond .eq. 2 .and. qx(mgs,lr) .gt. qxmin(lr) .and. cx(mgs,lr) > 1.e-9 ) THEN
          IF ( imurain == 3 ) THEN
           IF ( izwisventr == 1 ) THEN
            rwvent(mgs) = ventrx(mgs)*(1.6 + 124.9*(1.e-3*rho0(mgs)*qx(mgs,lr))**.2046)
           ELSE 

          rwvent(mgs) =   &
     &  (0.78*ventrx(mgs) + 0.308*ventrxn(mgs)*fvent(mgs)   &
     &   *Sqrt((ar*rhovt(mgs)))   &
     &    *(xdia(mgs,lr,1)**((1.0+br)/2.0)) )
           ENDIF

          ELSE 

           IF ( iferwisventr == 1 ) THEN
             alpr = Min(alpharmax,alpha(mgs,lr) )

             x =  1. + alpr

              tmp = 1 + alpr
              i = Int(dgami*(tmp))
              del = tmp - dgam*i
              g1palp = gmoi(i) + (gmoi(i+1) - gmoi(i))*del*dgami

              tmp = 2.5 + alpr + 0.5*bx(lr)
              i = Int(dgami*(tmp))
              del = tmp - dgam*i
              y = (gmoi(i) + (gmoi(i+1) - gmoi(i))*del*dgami)/g1palp 

         vent1 = dble(xdia(mgs,lr,1))**(-2. - alpr)
         vent2 = dble(1./xdia(mgs,lr,1) + 0.5*fx(lr))**dble(2.5+alpr+0.5*bx(lr))
        
        
        rwvent(mgs) =    &
     &    0.78*x +    &
     &    0.308*fvent(mgs)*y*   &
     &            Sqrt(ax(lr)*rhovt(mgs))*(vent1/vent2)

           ELSEIF ( iferwisventr == 2 ) THEN
          

            x =  1. + alpha(mgs,lr)

            rwvent(mgs) =   &
     &        (0.78*x + 0.308*ventrxn(mgs)*fvent(mgs)   &
     &         *Sqrt((ar*rhovt(mgs)))   &
     &         *(xdia(mgs,lr,1)**((1.0+br)/2.0)) )

          
          ENDIF 
          
       ENDIF 

       d1r = (1./(ac1 + bc))*4.0*pi*rwvent(mgs) & 
     &        *0.5*xdia(mgs,lr,1)*cx(mgs,lr)*rhoinv(mgs)
       ELSE
       d1r = 0.0
       ENDIF
       
       
       e1  = felvcp(mgs)/(pi0(mgs))
       f1 = pk(mgs) 




       ltemq = (temg(mgs)-163.15)/fqsat+1.5
       ltemq = Min( nqsat, Max(1,ltemq) )
       ltemq1 = ltemq
       temp1 = temg(mgs)
       p380 = 380.0/pres(mgs)




       ss1 = qx(mgs,lv)/qvs(mgs)
       ss2 = ss1
       temp2 = temp1
       qv1 = qx(mgs,lv)
       qvs1 = qvs(mgs)
       qis1 = qis(mgs)
       dt1 = 0.0





       ltemq1 = ltemq




       IF ( Abs(ss1 - 1.0) .gt. 1.e-5 ) THEN
         delta = 0.5*(qv1-qvs1)/(d1*(ss1 - 1.0))
       ELSE
         delta = 0.1*dtp
       ENDIF



       dtcon1 = Min(0.05,0.2*delta)
       nc = Max(5,2*NInt( (dtp-4.0*dtcon1)/delta))
       dtcon2 = (dtp-4.0*dtcon1)/nc

       n = 1
       dt1 = 0.0
       nc = 0
       dqc = 0.0
       dqr = 0.0
       dqi = 0.0
       dqs = 0.0

       RK2c: DO WHILE ( dt1 .lt. dtp )
          nc = 0
          IF ( n .le. 4 ) THEN
            dtcon = dtcon1
          ELSE
            dtcon = dtcon2
          ENDIF
 609       dqv  = -(ss1 - 1.)*d1*dtcon
           dqvr = -(ss1 - 1.)*d1r*dtcon
            dtemp = -0.5*e1*f1*(dqv + dqvr)


           ltemq1m = ltemq1 + Nint(dtemp*fqsat + 0.5)
           IF ( ltemq1m .lt. 1 .or. ltemq1m .gt. nqsat ) THEN
             write(0,*) 'STOP in icezvd_dr line 3790 '
             write(0,*) ' ltemq1m,icond = ',ltemq1m,icond
             write(0,*) ' dtemp,e1,f1,dqv,dqvr = ', dtemp,e1,f1,dqv,dqvr
             write(0,*) ' d1,d1r,dtcon,ss1 = ',d1,d1r,dtcon,ss1
             write(0,*) ' dqc, dqr = ',dqc,dqr
             write(0,*) ' qv,qc,qr = ',qx(mgs,lv)*1000.,qx(mgs,lc)*1000.,qx(mgs,lr)*1000.
             write(0,*) ' i, j, k = ',igs(mgs),jy,kgs(mgs)
             write(0,*) ' dtcon1,dtcon2,delta = ',dtcon1,dtcon2,delta
             write(0,*) ' nc,dtp = ',nc,dtp
             write(0,*) ' rwvent,xdia,crw = ', rwvent(mgs),xdia(mgs,lr,1),cx(mgs,lr)
             write(0,*) ' fvent,alphar = ',fvent(mgs),alpha(mgs,lr)
             write(0,*) ' xvr,xmasr,xdnr,cwc1 = ',xv(mgs,lr),xmas(mgs,lr),xdn(mgs,lr),cwc1
           ENDIF
            dqvs = dtemp*p380*dtabqvs(ltemq1m)
            qv1m = qv1 + dqv + dqvr


            qvs1m = qvs1 + dqvs
            ss1m = qv1m/qvs1m

    
          IF ( ss1m .lt. 1.  .and. (dqvii + dqvis) .eq. 0.0 ) THEN
            dtcon = (0.5*dtcon)
            IF ( dtcon .ge. dtcon1 ) THEN
             GOTO 609
            ELSE
             EXIT
            ENDIF
          ENDIF

          dqv  = -(ss1m - 1.)*d1*dtcon
          dqvr = -(ss1m - 1.)*d1r*dtcon



          dtemp = -e1*f1*(dqv + dqvr)
          ltemq1 = ltemq1 + Nint(dtemp*fqsat + 0.5)
           IF ( ltemq1 .lt. 1 .or. ltemq1 .gt. nqsat ) THEN
             write(0,*) 'STOP in icezvd_dr line 3856 '
             write(0,*) ' ltemq1m,icond = ',ltemq1m,icond
             write(0,*) ' dtemp,e1,dqv,dqvr = ', dtemp,e1,dqv,dqvr
           ENDIF
          dqvs = dtemp*p380*dtabqvs(ltemq1)

          qv1 = qv1 + dqv + dqvr

          dqc = dqc - dqv
          dqr = dqr - dqvr

          qvs1 = qvs1 + dqvs
          ss1 = qv1/qvs1
          temp1 = temp1 + dtemp
          IF ( temp2 .eq. temp1 .or. ss2 .eq. ss1 .or.  &
     &           ss1 .eq. 1.00 .or.  &
     &      ( n .gt. 10 .and. ss1 .lt. 1.0005 ) ) THEN

           EXIT
          ELSE
           ss2 = ss1
           temp2 = temp1
           dt1 = dt1 + dtcon
           n = n + 1
          ENDIF
       ENDDO RK2c


        dcloud = dqc 
        thetap(mgs) = thetap(mgs) + e1*(DCLOUD + dqr)
        qwvp(mgs) = qwvp(mgs) - (DCLOUD + dqr)
        qx(mgs,lc) = qx(mgs,lc) + DCLOUD
        qx(mgs,lr) = qx(mgs,lr) + dqr




        theta(mgs) = thetap(mgs) + theta0(mgs)
        temg(mgs) = theta(mgs)*f1
        ltemq = (temg(mgs)-163.15)/fqsat+1.5
        ltemq = Min( nqsat, Max(1,ltemq) )
        qvs(mgs) = pqs(mgs)*tabqvs(ltemq)
        es(mgs) = 6.1078e2*tabqvs(ltemq)



      ENDIF  


      ELSE  

        IF ( ssf(mgs) .gt. 0.0 ) THEN 

          IF ( iqcinit == 1 ) THEN

         qvs0   = 380.*exp(17.27*(temg(mgs)-273.)/(temg(mgs)- 36.))/pk(mgs)

         dcloud = Max(0.0, (qx(mgs,lv)-qvs0) / (1.+qvs0*f5/(temg(mgs)-36.)**2) )

          ELSEIF ( iqcinit == 3 ) THEN
              R1=1./(1. + caw*(273.15 - cbw)*qss(mgs)*felvcp(mgs)/ & 
     &             ((temg(mgs) - cbw)**2))
            DCLOUD=R1*(qvap(mgs) - qvs(mgs))  
                              
          
          ELSEIF ( iqcinit == 2 ) THEN



                              
         ssmx = ssmxinit

          IF ( ssf(mgs) > ssmx ) THEN
           CALL QVEXCESS(ngs,mgs,qwvp,qv0,qx(1,lc),pres,thetap,theta0,dcloud, & 
     &      pi0,tabqvs,nqsat,fqsat,cbw,fcqv1,felvcp,ssmx,pk,ngscnt)
          ELSE
            dcloud = 0.0
          ENDIF
         ENDIF
        ELSE
            dcloud = 0.0
        ENDIF

        thetap(mgs) = thetap(mgs) + felvcp(mgs)*DCLOUD/(pi0(mgs))
        qwvp(mgs) = qwvp(mgs) - DCLOUD
        qx(mgs,lc) = qx(mgs,lc) + DCLOUD




        theta(mgs) = thetap(mgs) + theta0(mgs)
        temg(mgs) = theta(mgs)*pk(mgs) 

        ltemq = (temg(mgs)-163.15)/fqsat+1.5
        ltemq = Min( nqsat, Max(1,ltemq) )
        qvs(mgs) = pqs(mgs)*tabqvs(ltemq)
        es(mgs) = 6.1078e2*tabqvs(ltemq)



      cn(mgs) = 0.0
      
      IF ( ncdebug .ge. 1 ) THEN
        write(iunit,*) 'at 613: ',qx(mgs,lc),cx(mgs,lc),wvel(mgs),ssmax(mgs),kgs(mgs)
      ENDIF
      

      IF ( dcloud .gt. qxmin(lc) .and. wvel(mgs) > 0.0) THEN
       CN(mgs) =   CCNE*wvel(mgs)**cnexp 
        IF ( ny .le. 2 .and. cn(mgs) .gt. 0.0    &
     &                    .and. ncdebug .ge. 1 ) THEN 
          write(iunit,*) 'CN: ',cn(mgs)*1.e-6, cx(mgs,lc)*1.e-6, qx(mgs,lc)*1.e3,   &
     &       wvel(mgs), dcloud*1.e3
          IF ( cn(mgs) .gt. 1.0 ) write(iunit,*) 'cwrad = ',   &
     &       1.e6*(rho0(mgs)*qx(mgs,lc)/cn(mgs)*cwc1)**c1f3,   &
     &   igs(mgs),kgs(mgs),temcg(mgs),    &
     &   1.e3*an(igs(mgs),jgs,kgs(mgs)-1,lc)
        ENDIF
        IF ( iccwflg .eq. 1 ) THEN
          cn(mgs) = Min(cwccn, Max(cn(mgs),   &
     &       rho0(mgs)*qx(mgs,lc)/(xdn(mgs,lc)*(4.*pi/3.)*(4.e-6)**3)))
        ENDIF
      ELSE
          cn(mgs) = Min(cwccn,    &
     &       rho0(mgs)*dcloud/(xdn(mgs,lc)*(4.*pi/3.)*(4.e-6)**3) )
      ENDIF

      IF ( cn(mgs) .gt. 0.0 ) THEN
       IF ( cn(mgs) .gt. ccnc(mgs) ) THEN
         cn(mgs) = ccnc(mgs)

       ENDIF

      ccnc(mgs) = Max(0.0, ccnc(mgs) - cn(mgs))
      ccna(mgs) = ccna(mgs) + cn(mgs)
      ENDIF



      IF( CN(mgs) .GT. cx(mgs,lc) ) cx(mgs,lc) = CN(mgs)
      IF( cx(mgs,lc) .GT. 0. .AND. qx(mgs,lc) .le. qxmin(lc) ) THEN
        cx(mgs,lc) = 0.
      ELSE
        cx(mgs,lc) = Min(cx(mgs,lc),rho0(mgs)*Max(0.0,qx(mgs,lc))/cwmasn)
      ENDIF

        END IF 








      IF ( wvel(mgs) .le. 0. ) GO TO 616
      IF ( cx(mgs,lc) .le. 0. )  GO TO 613                             
      IF ( kzbeg-1+kgs(mgs) .GT. 1 .and. qx(mgs,lc) .le. qxmin(lc)) GO TO 613  
      IF ( kzbeg-1+kgs(mgs) .eq. 1 .and. wvel(mgs) .gt. 0. ) GO TO 613         

  616 IF ( ssf(mgs) .LE. SUPCB .AND. wvel(mgs) .GT. 0. ) GO TO 631 
      IF ( kzbeg-1+kgs(mgs) .GT. 1 .AND. kzbeg-1+kgs(mgs) .LT. nzend-1 .AND.  &
     &    (ssfkp1(mgs) .GE. SUPMX .OR. &
     &     ssf(mgs)    .GE. SUPMX .OR. &
     &     ssfkm1(mgs) .GE. SUPMX)) GO TO 631                      
      IF (ssf(mgs) .LT. 1.E-10 .OR. ssf(mgs) .GE. SUPMX) GO TO 631 





      if (ndebug .gt. 0) write(0,*) "ICEZVD_DR: Entered Ziegler Cloud Nucleation" 

      DSSDZ=0.
      r2dzm=0.50/dz3d(igs(mgs),jy,kgs(mgs))
      IF ( irenuc >= 0 ) THEN

      IF ( irenuc /= 2 ) THEN 

        IF ( kzend == nzend ) THEN
          t0p3 = t0(igs(mgs),jgs,Min(kze,kgs(mgs)+3))
          t0p1 = t0(igs(mgs),jgs,Min(kze,kgs(mgs)+1))
        ELSE
          t0p3 = t0(igs(mgs),jgs,kgs(mgs)+3)
          t0p1 = t0(igs(mgs),jgs,kgs(mgs)+1)
        ENDIF

      IF ( ( ssf(mgs) .gt. ssmax(mgs) .or.  irenuc .eq. 1 ) &
     &   .and.  ( ( lccn .lt. 1 .and.  &
     &            cx(mgs,lc) .lt. cwccn*(Min(1.0,rho0(mgs)))) .or. &
     &    ( lccn .gt. 1 .and. ccnc(mgs) .gt. 0. )   ) &
     &    ) THEN
      IF( kzbeg-1+kgs(mgs) .GT. 1 .AND. kzbeg-1+kgs(mgs) .LT. nzend-1 &
     &  .and. ssf(mgs) .gt. 0.0 &
     &  .and. ssfkp1(mgs) .LT. SUPMX .and. ssfkp1(mgs) .ge. 0.0  &
     &  .AND. ssfkm1(mgs) .LT. SUPMX .AND. ssfkm1(mgs) .ge. 0.0  &
     &  .AND. ssfkp1(mgs) .gt. ssfkm1(mgs)  &
     &  .and. t0p3 .gt. 233.2) THEN
          DSSDZ = (ssfkp1(mgs) - ssfkm1(mgs))*R2DZM



        ELSEIF( kzbeg-1+kgs(mgs) .GT. 1 .AND. kzbeg-1+kgs(mgs) .LT. nzend-1 &

     &  .and. ssf(mgs) .gt. 0.0  .and. wvel(mgs) .gt. 0.0 &
     &  .and. ssfkp1(mgs) .gt. 0.0   &
     &  .AND. ssfkm1(mgs) .le. 0.0 .and. wvelkm1(mgs) .gt. 0.0 &
     &  .AND. ssf(mgs) .gt. ssfkm1(mgs)  &
     &  .and. t0p1 .gt. 233.2) THEN
         DSSDZ = 2.*(ssf(mgs) - ssfkm1(mgs))*R2DZM  
        ENDIF

       ENDIF



      c1 = Max(0.0, rho0(mgs)*(qx(mgs,lv) - qss(mgs))/ &
     &        (xdn(mgs,lc)*(4.*pi/3.)*(4.e-6)**3))
      IF ( lccn .lt. 1 ) THEN
       CN(mgs) = cwccn*CCK*ssf(mgs)**CCKM*dtp*   &
     & Max(0.0,    &
     &         (wvel(mgs)*DSSDZ) )      
      ELSE
      CN(mgs) =  &
     &    Min(ccnc(mgs), cnuc(mgs)*CCK*ssf(mgs)**CCKM*dtp*   &
     & Max(0.0,    &
     &         ( wvel(mgs)*DSSDZ) )  )

      ENDIF

      IF ( cn(mgs) .gt. 0.0 ) THEN
       IF ( ccnc(mgs) .lt. 5.e7 .and. cn(mgs) .ge. 5.e7 ) THEN
          cn(mgs) = 5.e7
          ccnc(mgs) = 0.0
       ELSEIF ( cn(mgs) .gt. ccnc(mgs) ) THEN
         cn(mgs) = ccnc(mgs)
         ccnc(mgs) = 0.0
       ENDIF
      cx(mgs,lc) = cx(mgs,lc) + cn(mgs)
      ccnc(mgs) = Max(0.0, ccnc(mgs) - cn(mgs))
      ENDIF

      ELSEIF ( irenuc == 2 ) THEN 
      
       CN(mgs) =   CCNE*Max(0.0,wvel(mgs))**cnexp 

       CN(mgs) = Min(cn(mgs), ccnc(mgs))
       
       cx(mgs,lc) = cx(mgs,lc) + cn(mgs)
       
       ccnc(mgs) = Max(0.0, ccnc(mgs) - cn(mgs))
       
      ENDIF 

      ccna(mgs) = ccna(mgs) + cn(mgs)

      ENDIF 

      IF( cx(mgs,lc) .GT. 0. .AND. qx(mgs,lc) .LE. qxmin(lc)) cx(mgs,lc)=0.
      GO TO 631


  613 CONTINUE

  631  CONTINUE




       ssmx = 1.1
       qv1 = qv0(mgs) + qwvp(mgs)
       qvs1 = qvs(mgs)

       IF ( qv1 .gt. (ssmx*qvs1) ) THEN
        
         ss1 = qv1/qvs1

        ssmx = 100.*(ssmx - 1.0)

        CALL QVEXCESS(ngs,mgs,qwvp,qv0,qx(1,lc),pres,thetap,theta0,qvex,   &
     &    pi0,tabqvs,nqsat,fqsat,cbw,fcqv1,felvcp,ssmx,pk,ngscnt)



        IF ( qvex .gt. 0.0 ) THEN
        thetap(mgs) = thetap(mgs) + felvcp(mgs)*qvex/(pi0(mgs))




        qwvp(mgs) = qwvp(mgs) - qvex
        qx(mgs,lc) = qx(mgs,lc) + qvex
        cn(mgs) = Min( ccwmx, qvex/Max( cwmasn5, xmas(mgs,lc) )  )
        ccnc(mgs) = Max( 0.0, ccnc(mgs) - cn(mgs) )
        cx(mgs,lc) = cx(mgs,lc) + cn(mgs)
        




        ENDIF

       
       ENDIF







      cx(mgs,lc) = Min( ccwmx, cx(mgs,lc) )
      IF ( cx(mgs,lc) .GT. 1.0e7 .AND. qx(mgs,lc) .GT. qxmin(lc)) THEN
        xmas(mgs,lc) = rho0(mgs)*qx(mgs,lc)/(cx(mgs,lc))
      ENDIF

      xmas(mgs,lc) = Min( xmas(mgs,lc), cwmasx )
      xmas(mgs,lc) = Max( xmas(mgs,lc), cwmasn )



















        

 681  CONTINUE
        
      IF ( ipconc .ge. 3 .and. rcond == 2 ) THEN

        
        IF (cx(mgs,lr) .GT. 0. .AND. qx(mgs,lr) .GT. qxmin(lr))    &
     &       xv(mgs,lr)=rho0(mgs)*qx(mgs,lr)/(xdn(mgs,lr)*cx(mgs,lr))
        IF (xv(mgs,lr) .GT. xvmx(lr)) xv(mgs,lr) = xvmx(lr)
        IF (xv(mgs,lr) .LT. xvmn(lr)) xv(mgs,lr) = xvmn(lr)

      ENDIF



      ENDDO 



      DO mgs=1,ngscnt
      IF ( ssf(mgs) .gt. ssmax(mgs)    &
     &  .and. ( idecss .eq. 0 .or. qx(mgs,lc) .gt. qxmin(lc)) ) THEN
        ssmax(mgs) = ssf(mgs)
      ENDIF
      ENDDO


      do mgs = 1,ngscnt
      an(igs(mgs),jy,kgs(mgs),lt) = theta0(mgs) + thetap(mgs)
      an(igs(mgs),jy,kgs(mgs),lv) =  qv0(mgs) + qwvp(mgs)


       if ( ido(lc) .eq. 1 )  then
        an(igs(mgs),jy,kgs(mgs),lc) = qx(mgs,lc) +    &
     &    min( an(igs(mgs),jy,kgs(mgs),lc), 0.0 )

       end if


       if ( ido(lr) .eq. 1 .and. rcond == 2 )  then
        an(igs(mgs),jy,kgs(mgs),lr) = qx(mgs,lr) +    &
     &    min( an(igs(mgs),jy,kgs(mgs),lr), 0.0 )

       end if



       IF (  ipconc .ge. 2 ) THEN
        an(igs(mgs),jy,kgs(mgs),lnc) = Max(cx(mgs,lc) , 0.0)
        IF ( lccn .gt. 1 ) THEN
          an(igs(mgs),jy,kgs(mgs),lccn) = Max(0.0, Min( ccwmx, ccnc(mgs) ) )
        ENDIF
       ENDIF
       IF (  ipconc .ge. 3 .and. rcond == 2 ) THEN
        an(igs(mgs),jy,kgs(mgs),lnr) = Max(cx(mgs,lr) , 0.0)
       ENDIF
      end do


29998 continue


      if ( kz .gt. nz-1 .and. ix .ge. nx) then
        if ( ix .ge. nx ) then
         go to 2200 
        else
         nzmpb = kz
        endif
      else
        nzmpb = kz
      end if

      if ( ix .ge. nx ) then
        nxmpb = 1
        nzmpb = kz+1
      else
       nxmpb = ix+1
      end if

 2000 continue 
 2200 continue








      frac = 1.0 





      do kz = 1,nz

      do ix = 1,nx
      
      zerocx(:) = .false.
      DO il = lc,lhab
        IF ( ln(il) > 1 ) zerocx(il) = ( an(ix,jy,kz,ln(il)) < cxmin )
        IF ( lz(il) > 1 ) zerocx(il) = ( zerocx(il) .or. an(ix,jy,kz,lz(il)) < zxmin )
      ENDDO

      IF ( lhl .gt. 1 ) THEN
      
      
      if ( an(ix,jy,kz,lhl) .lt. frac*qxmin(lhl) .or. zerocx(lhl) ) then


          an(ix,jy,kz,lv) = an(ix,jy,kz,lv) + an(ix,jy,kz,lhl)
          an(ix,jy,kz,lhl) = 0.0


        IF ( ipconc .ge. 5 ) THEN 
          an(ix,jy,kz,lnhl) = 0.0
        ENDIF

        IF ( lvhl .gt. 1 ) THEN
           an(ix,jy,kz,lvhl) = 0.0
        ENDIF

        IF ( lhlw .gt. 1 ) THEN
           an(ix,jy,kz,lhlw) = 0.0
        ENDIF
      
        IF ( lzhl .gt. 1 ) THEN
           an(ix,jy,kz,lzhl) = 0.0
        ENDIF

      ELSE
       IF ( lvol(lhl) .gt. 1 ) THEN  
        IF ( an(ix,jy,kz,lvhl) .gt. 0.0 ) THEN
         tmp = dn(ix,jy,kz)*an(ix,jy,kz,lhl)/an(ix,jy,kz,lvhl)
        ELSE 
         tmp = 0.5*( xdnmn(lhl) + xdnmx(lhl) )
          an(ix,jy,kz,lvhl) = dn(ix,jy,kz)*an(ix,jy,kz,lhl)/tmp
        ENDIF











        IF ( tmp .gt. xdnmx(lhl) .or. tmp .lt. xdnmn(lhl) ) THEN
          tmp = Min( xdnmx(lhl), Max( xdnmn(lhl) , tmp ) )
          an(ix,jy,kz,lvhl) = dn(ix,jy,kz)*an(ix,jy,kz,lhl)/tmp
        ENDIF
        
       ENDIF
       
       

       IF ( ipconc == 5 .and.  an(ix,jy,kz,lhl) .gt. qxmin(lhl) .and.  alphahl .le. 0.1 .and. lnhl .gt. 1 .and. lzhl == 0 ) THEN
       
         IF ( lvhl .gt. 1 ) THEN
           hwdn = dn(ix,jy,kz)*an(ix,jy,kz,lhl)/an(ix,jy,kz,lvhl)
         ELSE
           hwdn = xdn0(lhl)
         ENDIF
           tmp = (hwdn*an(ix,jy,kz,lnhl))/(dn(ix,jy,kz)*an(ix,jy,kz,lhl))
           tmpg = an(ix,jy,kz,lnhl)*(tmp*(3.14159))**(1./3.)
           IF ( tmpg .lt. cnohlmn ) THEN
             tmp = ( (hwdn)/(dn(ix,jy,kz)*an(ix,jy,kz,lhl))*(3.14159))**(1./3.)
              an(ix,jy,kz,lnhl) = (cnohlmn/tmp)**(3./4.)
           ENDIF
       
       ENDIF


      end if



      ENDIF 


      if ( an(ix,jy,kz,lh) .lt. frac*qxmin(lh) .or. zerocx(lh) ) then


          an(ix,jy,kz,lv) = an(ix,jy,kz,lv) + an(ix,jy,kz,lh)
          an(ix,jy,kz,lh) = 0.0


        IF ( ipconc .ge. 5 ) THEN 
          an(ix,jy,kz,lnh) = 0.0
        ENDIF

        IF ( lvh .gt. 1 ) THEN
           an(ix,jy,kz,lvh) = 0.0
        ENDIF
      
        IF ( lhw .gt. 1 ) THEN
           an(ix,jy,kz,lhw) = 0.0
        ENDIF
      
        IF ( lzh .gt. 1 ) THEN
           an(ix,jy,kz,lzh) = 0.0
        ENDIF

      ELSE
       IF ( lvol(lh) .gt. 1 ) THEN  
        IF ( an(ix,jy,kz,lvh) .gt. 0.0 ) THEN
         tmp = dn(ix,jy,kz)*an(ix,jy,kz,lh)/an(ix,jy,kz,lvh)
        ELSE
         tmp = rho_qh
          an(ix,jy,kz,lvh) = dn(ix,jy,kz)*an(ix,jy,kz,lh)/tmp
        ENDIF

        IF (  tmp .lt. xdnmn(lh) ) THEN
          tmp = Max( xdnmn(lh), tmp )
          an(ix,jy,kz,lvh) = dn(ix,jy,kz)*an(ix,jy,kz,lh)/tmp
        ENDIF

        IF ( tmp .gt. xdnmx(lh) .and. lhw .le. 0 ) THEN 
          tmp = Min( xdnmx(lh), tmp )
          an(ix,jy,kz,lvh) = dn(ix,jy,kz)*an(ix,jy,kz,lh)/tmp
        ELSEIF ( tmp .gt. xdnmx(lh) .and. lhw .gt. 1 ) THEN  
          IF ( tmp .gt. xdnmx(lh) .and. an(ix,jy,kz,lhw) .lt. qxmin(lh) ) THEN
            tmp = Min( xdnmx(lh), tmp )
            an(ix,jy,kz,lvh) = dn(ix,jy,kz)*an(ix,jy,kz,lh)/tmp
          ELSEIF ( tmp .gt. xdnmx(lr) ) THEN
            tmp =  xdnmn(lr)
            an(ix,jy,kz,lvh) = dn(ix,jy,kz)*an(ix,jy,kz,lh)/tmp
          ENDIF
        ENDIF

        IF ( lhw .gt. 1 ) THEN 
          IF ( an(ix,jy,kz,lhw) .gt. 0.98*an(ix,jy,kz,lh) ) THEN
           tmp = xdnmx(lr)
           an(ix,jy,kz,lvh) = dn(ix,jy,kz)*an(ix,jy,kz,lh)/tmp
          ENDIF
        ENDIF
        
       ENDIF


       IF ( ipconc == 5 .and.  an(ix,jy,kz,lh) .gt. qxmin(lh) .and.  alphah .le. 0.1 .and. lnh .gt. 1 .and. lzh == 0 ) THEN
       
         IF ( lvh .gt. 1 ) THEN
           IF ( an(ix,jy,kz,lvh) .gt. 0.0 ) THEN
             hwdn = dn(ix,jy,kz)*an(ix,jy,kz,lh)/an(ix,jy,kz,lvh)
           ELSE
             hwdn = xdn0(lh)
           ENDIF
           hwdn = Max( xdnmn(lh), hwdn )
         ELSE
           hwdn = xdn0(lh)
         ENDIF
           tmp = (hwdn*an(ix,jy,kz,lnh))/(dn(ix,jy,kz)*an(ix,jy,kz,lh))
           tmpg = an(ix,jy,kz,lnh)*(tmp*(3.14159))**(1./3.)
           IF ( tmpg .lt. cnohmn ) THEN


             tmp = ( (hwdn)/(dn(ix,jy,kz)*an(ix,jy,kz,lh))*(3.14159))**(1./3.)
              an(ix,jy,kz,lnh) = (cnohmn/tmp)**(3./4.)
           ENDIF
       
       ENDIF
        
      end if


      if ( an(ix,jy,kz,ls) .lt.  frac*qxmin(ls)  .or. zerocx(ls)  & 
     &         ) then
      IF ( t0(ix,jy,kz) .lt. 273.15 ) THEN

          an(ix,jy,kz,lv) = an(ix,jy,kz,lv) + an(ix,jy,kz,ls)
          an(ix,jy,kz,ls) = 0.0

      
        IF ( ipconc .ge. 4 ) THEN 

          an(ix,jy,kz,lns) = 0.0
        ENDIF
        
        IF ( lvs .gt. 1 ) THEN
           an(ix,jy,kz,lvs) = 0.0
        ENDIF

        IF ( lsw .gt. 1 ) THEN
           an(ix,jy,kz,lsw) = 0.0
        ENDIF

      ELSE

          an(ix,jy,kz,lv) = an(ix,jy,kz,lv) + an(ix,jy,kz,ls)
          an(ix,jy,kz,ls) = 0.0


        IF ( lvs .gt. 1 ) THEN
           an(ix,jy,kz,lvs) = 0.0
        ENDIF

        IF ( lsw .gt. 1 ) THEN
           an(ix,jy,kz,lsw) = 0.0
        ENDIF

        IF ( ipconc .ge. 4 ) THEN 

          an(ix,jy,kz,lns) = 0.0
        ENDIF

      ENDIF
      

      ELSEIF ( lvol(ls) .gt. 1 ) THEN  
        IF ( an(ix,jy,kz,lvs) .gt. 0.0 ) THEN
          tmp = dn(ix,jy,kz)*an(ix,jy,kz,ls)/an(ix,jy,kz,lvs)
          IF ( tmp .gt. xdnmx(ls) .or. tmp .lt. xdnmn(ls) ) THEN
            tmp = Min( xdnmx(ls), Max( xdnmn(ls), tmp ) )
            an(ix,jy,kz,lvs) = dn(ix,jy,kz)*an(ix,jy,kz,ls)/tmp
          ENDIF
        ELSE
          tmp = rho_qs
          an(ix,jy,kz,lvs) = dn(ix,jy,kz)*an(ix,jy,kz,ls)/tmp
        ENDIF


      end if


      if ( an(ix,jy,kz,lr) .lt. frac*qxmin(lr)  .or. zerocx(lr)  &
     &  ) then
        an(ix,jy,kz,lv) = an(ix,jy,kz,lv) + an(ix,jy,kz,lr)
        an(ix,jy,kz,lr) = 0.0
        IF ( ipconc .ge. 3 ) THEN

          an(ix,jy,kz,lnr) = 0.0
        ENDIF
        
      end if




      IF ( an(ix,jy,kz,li) .le. frac*qxmin(li) .or. zerocx(li)   & 
     &    ) THEN
      an(ix,jy,kz,lv) = an(ix,jy,kz,lv) + an(ix,jy,kz,li)
      an(ix,jy,kz,li)= 0.0
       IF ( ipconc .ge. 1 ) THEN
         an(ix,jy,kz,lni) = 0.0
       ENDIF
      ENDIF





      IF ( an(ix,jy,kz,lc) .le. frac*qxmin(lc) .or. zerocx(lc)   &
     &       ) THEN
      an(ix,jy,kz,lv) = an(ix,jy,kz,lv) + an(ix,jy,kz,lc)
      an(ix,jy,kz,lc)= 0.0
       IF ( ipconc .ge. 2 ) THEN
        IF ( lccn .gt. 1 ) THEN
         an(ix,jy,kz,lccn) =     &
     &      Min( ccwmx, an(ix,jy,kz,lccn) + Max(0.0,an(ix,jy,kz,lnc)) )
        ENDIF
         an(ix,jy,kz,lnc) = 0.0

       ENDIF

      ENDIF

      end do

      end do
      
      
      IF ( ndebug .ge. 1 ) write(6,*) 'END OF ICEZVD_DR'


   
   RETURN
   END SUBROUTINE NUCOND




      SUBROUTINE QVEXCESS(ngs,mgs,qwvp0,qv0,qcw1,pres,thetap0,theta0, &
     &    qvex,pi0,tabqvs,nqsat,fqsat,cbw,fcqv1,felvcp,ss1,pk,ngscnt)
      




      implicit none

      integer ngs,mgs,ngscnt
      
      real theta2temp
      
      real qvex
      
      integer nqsat
      real fqsat, cbw
      
      real ss1  



      real qv0(ngs), qcw1(ngscnt), pres(ngs), qwvp0(mgs)
      real thetap0(ngs), theta0(ngs)
      real fcqv1(ngs), felvcp(ngs), pi0(ngs)
      real pk(ngs)
      
      real tabqvs(nqsat)



      
      integer itertd
      integer ltemq
      real gamss
      real theta(ngs), qvap(ngs), pqs(ngs), qcw(ngs), qwv(ngs)
      real qcwtmp(ngs), qss(ngs), qvs(ngs), qwvp(ngs)
      real dqcw(ngs), dqwv(ngs), dqvcnd(ngs)
      real temg(ngs), temcg(ngs), thetap(ngs)
      
      real tfr
      parameter ( tfr = 273.15 )
            










      pqs(mgs) = (380.0)/(pres(mgs))
      thetap(mgs) = thetap0(mgs)
      theta(mgs) = thetap(mgs) + theta0(mgs)
      qwvp(mgs) = qwvp0(mgs)
      qvap(mgs) = max( (qwvp0(mgs) + qv0(mgs)), 0.0 )
      temg(mgs) = theta(mgs)*pk(mgs) 






      
      qwv(mgs) = max( 0.0, qvap(mgs) )
      qcw(mgs) = max( 0.0, qcw1(mgs) )


      qcwtmp(mgs) = qcw(mgs)
      temcg(mgs) = temg(mgs) - tfr
      ltemq = (temg(mgs)-163.15)/fqsat+1.5
      ltemq = Min( nqsat, Max(1,ltemq) )

      qvs(mgs) = pqs(mgs)*tabqvs(ltemq)
      qss(mgs) = (0.01*ss1 + 1.0)*qvs(mgs)



      do itertd = 1,2




      dqcw(mgs) = 0.0
      dqwv(mgs) = ( qwv(mgs) - qss(mgs) )



      if( dqwv(mgs) .lt. 0. ) then           
        if( qcw(mgs) .gt. -dqwv(mgs) ) then  
          dqcw(mgs) = dqwv(mgs)
          dqwv(mgs) = 0.
        else                                 
          dqcw(mgs) = -qcw(mgs)
          dqwv(mgs) = dqwv(mgs) + qcw(mgs)
        end if

        qwvp(mgs) = qwvp(mgs) - ( dqcw(mgs)  )  

        qcw(mgs) = qcw(mgs) + dqcw(mgs)

        thetap(mgs) = thetap(mgs) +  &
     &                1./pi0(mgs)*  &
     &                (felvcp(mgs)*dqcw(mgs) )

      end if  



      IF ( dqwv(mgs) .ge. 0. ) THEN

      dqvcnd(mgs) = dqwv(mgs)/(1. + fcqv1(mgs)*qss(mgs)/  &
     &  ((temg(mgs)-cbw)**2))


      dqcw(mgs) = dqvcnd(mgs)

      thetap(mgs) = thetap(mgs) +  &
     &   (felvcp(mgs)*dqcw(mgs) )    &
     & / (pi0(mgs))
      qwvp(mgs) = qwvp(mgs) - ( dqvcnd(mgs) )
      qcw(mgs) = qcw(mgs) + dqcw(mgs)

      END IF 

      theta(mgs) = thetap(mgs) + theta0(mgs)
      temg(mgs) = theta(mgs)*pk(mgs) 

      qvap(mgs) = Max((qwvp(mgs) + qv0(mgs)), 0.0)
      temcg(mgs) = temg(mgs) - tfr

      ltemq = (temg(mgs)-163.15)/fqsat+1.5
      ltemq = Min( nqsat, Max(1,ltemq) )
      qvs(mgs) = pqs(mgs)*tabqvs(ltemq)
      qcw(mgs) = max( 0.0, qcw(mgs) )
      qwv(mgs) = max( 0.0, qvap(mgs))
      qss(mgs) = (0.01*ss1 + 1.0)*qvs(mgs)
      end do




      qvex = Max(0.0, qcw(mgs) - qcw1(mgs) )

      RETURN
      END SUBROUTINE QVEXCESS

























































      subroutine nssl_2mom_gs   &
     &  (nx,ny,nz,na,jyslab  &
     &  ,nor,norz          &
     &  ,dtp,gz       &
     &  ,t0,t1,t2,t3,t4,t5,t6,t7,t8,t9      &
     &  ,an,dn,p2                  &
     &  ,pn,w,iunit                   &
     &  ,t00,t77,                             &
     &   ventr,ventc,c1sw,jgs,ido,    &
     &   xdnmx,xdnmn,               &

     &   cdx,                              &
     &   xdn0,tmp3d,timevtcalc  &
     & )





















































      implicit none





      
      integer jyslab,its,ids,ide,jds,jde 
      integer ng1
      integer, intent(in) :: iunit 
      parameter(ng1 = 1)
      real qvex
      integer iraincv, icgxconv
      parameter ( iraincv = 1, icgxconv = 1)
      real ffrz

      real qcitmp,cirdiatmp 
      real ccwtmp,ccitmp 
      real cpqc,cpci 
      real cpqc0,cpci0 
      real scfac 
      
      double precision dp1
      
      real    :: delqnw = -1.0e-10
      real    :: delqxw =  1.0e-10
      real :: tindmn = 233, tindmx = 298.0  

      double precision frac, frach, xvfrz

      integer iexy(lc:lqmx,lc:lqmx)
      integer :: ieswi = 1,  ieswc = 1, ieswr = 0
      integer :: iehlsw = 1, iehli = 1,  iehlc = 1, iehlr = 0
      integer :: iehwsw = 1, iehwi = 1,  iehwc = 1, iehwr = 0
      
      double precision :: timevtcalc
      double precision :: dpt1,dpt2
            




      real vtmax
      integer n,ndfall
      
      double precision chgneg,chgpos
      
      real temgtmp
      integer nx,ny,nz,na,nba,nv
      integer nor,norz,istag,jstag,kstag 
      integer iwrite
      real dtp,dx,dy,dz
      real gz(-nor+ng1:nx+nor,-nor+ng1:ny+nor,-norz+ng1:nz+norz)
      
      real qimax,xni0,roqi0


      real dv

      real dtptmp
      integer itest,nidx,id1,jd1,kd1
      parameter (itest=1)
      parameter (nidx=10)
      parameter (id1=1,jd1=1,kd1=1)
      integer ierr
      integer iend

      integer ix,kz, il, ic, ir, icp1, irp1, ip1,jp1,kp1
      integer :: jy
      integer i,j
      real slope1, slope2
      real x1, x2, x3
      real eps,eps2
      parameter (eps=1.e-20,eps2=1.e-5)



      real  temele
      real  trev
      
      logical ldovol, ishail, ltest




      integer mu,mv,mw
      parameter (mu=1,mv=2,mw=3)



      integer mqcw,mqxw,mtem,mrho,mtim
      parameter (mqcw=21,mqxw=21,mtem=21,mrho=5,mtim=6)

      real xftim,xftimi,yftim, xftem,yftem, xfqcw,yfqcw, xfqxw,yfqxw
      parameter (xftim=0.05,xftimi = 1./xftim,yftim=1.)
      parameter (xftem=0.5,yftem=1.)
      parameter (xfqcw=2000.,yfqcw=1.)
      parameter (xfqxw=2000.,yfqxw=1.)
      real dtfac
      parameter ( dtfac = 1.0 )
      integer ido(lc:lqmx)












       real delqnxa(lc:lqmx)
       real delqxxa(lc:lqmx)



      real t00(-nor+ng1:nx+nor,-nor+ng1:ny+nor,-norz+ng1:nz+norz)
      real t77(-nor+ng1:nx+nor,-nor+ng1:ny+nor,-norz+ng1:nz+norz)

      real t0(-nor+ng1:nx+nor,-nor+ng1:ny+nor,-norz+ng1:nz+norz)
      real t1(-nor+ng1:nx+nor,-nor+ng1:ny+nor,-norz+ng1:nz+norz)
      real t2(-nor+ng1:nx+nor,-nor+ng1:ny+nor,-norz+ng1:nz+norz)
      real t3(-nor+ng1:nx+nor,-nor+ng1:ny+nor,-norz+ng1:nz+norz)
      real t4(-nor+ng1:nx+nor,-nor+ng1:ny+nor,-norz+ng1:nz+norz)
      real t5(-nor+ng1:nx+nor,-nor+ng1:ny+nor,-norz+ng1:nz+norz)
      real t6(-nor+ng1:nx+nor,-nor+ng1:ny+nor,-norz+ng1:nz+norz)
      real t7(-nor+ng1:nx+nor,-nor+ng1:ny+nor,-norz+ng1:nz+norz)
      real t8(-nor+ng1:nx+nor,-nor+ng1:ny+nor,-norz+ng1:nz+norz)
      real t9(-nor+ng1:nx+nor,-nor+ng1:ny+nor,-norz+ng1:nz+norz)

      real p2(-nor+1:nx+nor,-nor+1:ny+nor,-norz+ng1:nz+norz)  
      real pn(-nor+1:nx+nor,-nor+1:ny+nor,-norz+ng1:nz+norz)
      real an(-nor+ng1:nx+nor,-nor+ng1:ny+nor,-norz+ng1:nz+norz,na)
      real dn(-nor+1:nx+nor,-nor+1:ny+nor,-norz+ng1:nz+norz)
      real w(-nor+1:nx+nor,-nor+1:ny+nor,-norz+ng1:nz+norz)

      real tmp3d(-nor+ng1:nx+nor,-nor+ng1:ny+nor,-norz+ng1:nz+norz)




      integer nxmpb,nzmpb,nxz
      integer jgs,mgs,ngs,numgs
      parameter (ngs=500) 
      integer, parameter :: ngsz = 500
      integer ntt
      parameter (ntt=300)

      integer ngscnt,igs(ngs),kgs(ngs)
      integer kgsp(ngs),kgsm(ngs),kgsm2(ngs)
      integer ncuse
      parameter (ncuse=0)
      integer il0(ngs),il5(ngs),il2(ngs),il3(ngs)


      real cai,caw,cbi,cbw
      real tdtol,temsav,tfrcbw,tfrcbi,thnuc
      
      real tfr,tfrh
      parameter ( tfr = 273.15, tfrh = 233.15)
      



      real  fimt1(ngs),fimta(ngs),fimt2(ngs) 
      real xcwmas






      real ccnc(ngs),ccin(ngs),cina(ngs),ccna(ngs)
      real sscb  
      parameter ( sscb = 2.0 )
      integer idecss  
      parameter ( idecss = 1 )
      integer iba 
                  
                  
      parameter (iba = 1)
      integer ifilt   
      parameter ( ifilt = 0 ) 
      real temp1,temp2 
      real :: mwat, mice, dice, mwshed, fwmax, fw, mwcrit, massfactor
      real, parameter :: shedalp = 3.  
      real ssmax(ngs)       
      real ssmx
      real dnnet,dqnet


      real bfnu, bfnu0, bfnu1
      parameter ( bfnu0 = (rnu + 2.0)/(rnu + 1.0)  )
      real ventr, ventc
      real volb, aa1, aa2
      double precision t2s, xdp
      double precision xl2p(ngs),rb(ngs)
      parameter ( aa1 = 9.44e15, aa2 = 5.78e3 ) 

      real cexs, cecs
      parameter ( cexs = 0.1, cecs = 0.5 )
      real rvt      
      parameter ( rvt = 0.104 )
      real kfrag    
      parameter ( kfrag = 1.0e-6 )
      real mfrag    
      parameter ( mfrag = 1.0e-10)
      double precision cautn(ngs), rh(ngs), nh(ngs)
      real ex1, ft, rhoinv(ngs)
      double precision ec0(ngs)
      
      real ac1,bc, taus, c1,d1,e1,f1,p380,tmp,tmp1,tmp2,tmp3,tmp4 
      real ratio, delx, dely
      real dbigg,volt
      real chgtmp,fac
      real x,y,del,r,alpr
      double precision :: vent1,vent2,dprwvent
      real g1palp
      real fqt 
      real bs
      real v1, v2
      real d1r, d1i, d1s, e1i
      real c1sw   
      real, parameter :: vr1mm = 5.23599e-10 
      real, parameter :: vr3mm = 5.23599e-10*(3.0/1.)**3   
      real, parameter :: vr4p5mm = 5.23599e-10*(4.5/1.)**3 
      real vmlt,vshd
      real rhosm
      parameter ( rhosm = 500. )
      integer nc 
      real dtcon,dtcon1,dtcon2 
      real delta
      integer ltemq1,ltemq1m 
      real dqv,qv1,ss1,ss2,qvs1,dqvs,dtemp,dt1   
      real ssi1, ssi2, dqvi, dqvis, dqvii,qis1
      real dqvr, dqc, dqr, dqi, dqs
      real qv1m,qvs1m,ss1m,ssi1m,qis1m
      real cwmastmp
      real  dcloud,dcloud2 
      real cn(ngs)
      double precision xvc, xvr
      real mwfac
      real  es(ngs) 
      real  eis(ngs)

      real rwmasn,rwmasx

      real vgra,vfrz
      parameter ( vgra = 0.523599*(1.0e-3)**3 )
     
      real epsi,d
      parameter (epsi = 0.622, d = 0.266)
      real r1,qevap 
      
      real vr,nrx,chw,g1,qr,z,z1,rdi,alp,xnutmp,xnuc,g1r
      
      real, parameter :: rhofrz = 900.   
      real, parameter :: rimedens = 500. 





      real raero,kaero 
      parameter ( raero = 3.e-7, kaero = 5.39e-3 )
      real kb   
      parameter (kb = 1.3807e-23)
      
      real knud(ngs),knuda(ngs) 
      real gtp(ngs)  
      real dfar(ngs) 
      real fn1(ngs),fn2(ngs),fnft(ngs)
      
      real ccia(ngs)
      real ctfzbd(ngs),ctfzth(ngs),ctfzdi(ngs)



      real ni,nr,d0
      real dqvcnd(ngs),dqwv(ngs),dqcw(ngs),dqci(ngs)
      real tempc(ngs)
      real temg(ngs),temcg(ngs),theta(ngs),qvap(ngs) 
      real temgkm1(ngs), temgkm2(ngs)
      real temgx(ngs),temcgx(ngs)
      real qvs(ngs),qis(ngs),qss(ngs),pqs(ngs)
      real elv(ngs),elf(ngs),els(ngs)
      real tsqr(ngs),ssi(ngs),ssw(ngs)
      real qcwtmp(ngs),qtmp,qtot(ngs) 
      real qcond(ngs)
      real ctmp, sctmp
      real cwmasn,cwmasx
      real cwmasn5
      real cwradn
      real cimasn,cimasx,ccimx
      real pid4
      real ar,br,cs,ds,gf7,gf6,gf5,gf4,gf3,gf2,gf1
      real gf73rds, gf83rds
      real gf43rds, gf53rds
      real aradcw,bradcw,cradcw,dradcw,cwrad,rwrad,rwradmn
      parameter ( rwradmn = 50.e-6 )
      real dh0
      
      real clionpmx,clionnmx
      parameter (clionpmx=1.e9,clionnmx=1.e9) 



      real fwet1(ngs),fwet2(ngs)   
      real fmlt1(ngs),fmlt2(ngs)  
      real fvds(ngs),fvce(ngs),fiinit(ngs) 
      real fvent(ngs),fraci(ngs),fracl(ngs)

      real fai(ngs),fav(ngs),fbi(ngs),fbv(ngs)
      real felv(ngs),fels(ngs),felf(ngs)
      real felvcp(ngs),felscp(ngs),felfcp(ngs)
      real felvs(ngs),felss(ngs)      
      real fwvdf(ngs),ftka(ngs),fthdf(ngs)
      real fadvisc(ngs),fakvisc(ngs)
      real fci(ngs),fcw(ngs)
      real fschm(ngs),fpndl(ngs)
      real fgamw(ngs),fgams(ngs)
      real fcqv1(ngs),fcqv2(ngs),fcc3(ngs) 
      
      real cvm

      real fcci(ngs), fcip(ngs)

      real :: sfm1(ngs),sfm2(ngs)
      real :: gfm1(ngs),gfm2(ngs)
      real :: hfm1(ngs),hfm2(ngs)

      logical :: wetsfc(ngs),wetsfchl(ngs)
      logical :: wetgrowth(ngs), wetgrowthhl(ngs)

       real qitmp(ngs)
       
      real rzxh(ngs), rzxhl(ngs), rzxhlh(ngs)
      real vt2ave(ngs)

      real ::  qx(ngs,lv:lhab)
      real ::  qxw(ngs,ls:lhab)
      real ::  cx(ngs,lc:lhab)
      real ::  cxmxd(ngs,lc:lhab)
      real ::  qxmxd(ngs,lv:lhab)
      real ::  scx(ngs,lc:lhab)
      real ::  xv(ngs,lc:lhab)

      real ::  vtxbar(ngs,lc:lhab,3)
      real ::  xmas(ngs,lc:lhab)
      real ::  xdn(ngs,lc:lhab)
      real ::  xdia(ngs,lc:lhab,3)
      real ::  rarx(ngs,ls:lhab)
      real ::  vx(ngs,li:lhab)
      real ::  rimdn(ngs,li:lhab)
      real ::  raindn(ngs,li:lhab)
      real ::  alpha(ngs,lr:lhab)
      real ::  dab0lh(ngs,lc:lhab,lr:lhab)
      real ::  dab1lh(ngs,lc:lhab,lr:lhab)
      
      
      real, parameter :: alpharaut = 0.0 
      real, parameter :: galpharaut = (6.+alpharaut)* &
     &                                (5.+alpharaut)* &
     &                                (4.+alpharaut)/ &
     &                               ((3.+alpharaut)* &
     &                                (2.+alpharaut)* &
     &                                (1.+alpharaut))
      
      real ventrx(ngs)
      real ventrxn(ngs)
      real g1shr, alphashr
      real g1mlr, alphamlr
      
      real swvent(ngs),hwvent(ngs),rwvent(ngs),hlvent(ngs)
      real civent(ngs)

      real xmascw(ngs)
      real xdnmx(lc:lhab), xdnmn(lc:lhab)
      real dnmx

      real cilen(ngs) 


      real rwcap(ngs),swcap(ngs)
      real hwcap(ngs)
      real hlcap(ngs)
      real cicap(ngs)

      real qvimxd(ngs)
      real qimxd(ngs),qcmxd(ngs),qrmxd(ngs),qsmxd(ngs),qhmxd(ngs),qhlmxd(ngs)
      real cimxd(ngs),ccmxd(ngs),crmxd(ngs),csmxd(ngs),chmxd(ngs)
      real cionpmxd(ngs),cionnmxd(ngs)
      real clionpmxd(ngs),clionnmxd(ngs)



      real chmul1(ngs),chlmul1(ngs),csmul1(ngs),csmul(ngs)
      real qhmul1(ngs),qhlmul1(ngs),qsmul1(ngs),qsmul(ngs)
      
      real csplinter(ngs),qsplinter(ngs)
      real csplinter2(ngs),qsplinter2(ngs)




      real :: chlcnh(ngs), vhlcnh(ngs), vhlcnhl(ngs)
      real cracif(ngs), ciacrf(ngs)
      real cracr(ngs)


      real ciint(ngs), crfrz(ngs), crfrzf(ngs), crfrzs(ngs)
      real cicint(ngs)
      real cipint(ngs)
      real ciacw(ngs), cwacii(ngs) 
      real ciacr(ngs), craci(ngs)
      real csacw(ngs)
      real csacr(ngs)
      real csaci(ngs),   csacs(ngs)
      real cracw(ngs) 
      real chacw(ngs), chacr(ngs)
      real :: chlacw(ngs) 
      real chaci(ngs), chacs(ngs)

      real :: chlacr(ngs)
      real :: chlaci(ngs), chlacs(ngs)
      real crcnw(ngs) 
      real cidpv(ngs),cisbv(ngs)
      real cimlr(ngs)

      real chlsbv(ngs), chldpv(ngs)
      real chlmlr(ngs), chlmlrr(ngs) 
      real chlshr(ngs), chlshrr(ngs)

      real chdpv(ngs),chsbv(ngs)
      real chmlr(ngs),chcev(ngs)
      real chmlrr(ngs)
      real chshr(ngs), chshrr(ngs)

      real csdpv(ngs),cssbv(ngs)
      real csmlr(ngs),cscev(ngs)
      real csshr(ngs)

      real crcev(ngs)
      real crshr(ngs)






      real qrcnw(ngs), qwcnr(ngs)
      real zrcnw(ngs),zracr(ngs),zracw(ngs),zrcev(ngs)


      real qracw(ngs) 
      real qiacw(ngs) 

      real qsacw(ngs) 
      real qhacw(ngs) 
      real :: qhlacw(ngs) 
      real vhacw(ngs), vsacw(ngs), vhlacw(ngs), vhlacr(ngs)


      real qsacws(ngs)




      real qsacr(ngs),qracs(ngs)
      real qhacr(ngs) 
      real vhacr(ngs), zhacr(ngs), zhacrf(ngs), zrach(ngs), zrachl(ngs)
      real qiacr(ngs),qraci(ngs)
      
      real ziacr(ngs)

      real qracif(ngs),qiacrf(ngs),qiacrs(ngs)

      real :: qhlacr(ngs) 
      real qsacrs(ngs) 



      real qsaci(ngs)
      real qhaci(ngs)
      real qhacs(ngs)

      real :: qhlaci(ngs) 
      real :: qhlacs(ngs) 



      real qrfrz(ngs) 
      real zrfrz(ngs), zrfrzf(ngs)
      real ziacrf(ngs), zhcnsh(ngs), zhcnih(ngs)
      real zhacw(ngs), zhacs(ngs)
      real zhmlr(ngs), zhdsv(ngs), zhsbv(ngs), zhlcnh(ngs), zhshr(ngs)
      real zhmlrr(ngs),zhlmlrr(ngs),zhshrr(ngs),zhlshrr(ngs)
      real zsmlr(ngs), zsmlrr(ngs), zsshr(ngs)
      real zhwdn(ngs) 
      real zhldn(ngs) 

      real zhlacw(ngs), zhlacs(ngs), zhlacr(ngs)
      real zhlmlr(ngs), zhldsv(ngs), zhlsbv(ngs), zhlshr(ngs)

      
      real vrfrzf(ngs), viacrf(ngs)
      real qrfrzs(ngs), qrfrzf(ngs)
      real qwfrz(ngs), qwctfz(ngs)
      real cwfrz(ngs), cwctfz(ngs)
      real qwfrzc(ngs), qwctfzc(ngs)
      real cwfrzc(ngs), cwctfzc(ngs)
      real qwfrzp(ngs), qwctfzp(ngs)
      real cwfrzp(ngs), cwctfzp(ngs)
      real xcolmn(ngs), xplate(ngs)
      real ciihr(ngs), qiihr(ngs)
      real cicichr(ngs), qicichr(ngs)
      real cipiphr(ngs), qipiphr(ngs)
      real qscni(ngs), cscni(ngs), cscnis(ngs)
      real qscnvi(ngs), cscnvi(ngs), cscnvis(ngs)
      real qhcns(ngs), chcns(ngs), chcnsh(ngs), vhcns(ngs)
      real qhcni(ngs), chcni(ngs), chcnih(ngs), vhcni(ngs)
      real qiint(ngs),qipipnt(ngs),qicicnt(ngs)
      real cninm(ngs),cnina(ngs),cninp(ngs),wvel(ngs),wvelkm1(ngs)
      real tke(ngs)
      real uvel(ngs),vvel(ngs)

      real qidpv(ngs),qisbv(ngs) 
      real qimlr(ngs),qidsv(ngs),qidsvp(ngs) 


      real qfdpv(ngs),qfsbv(ngs) 
      real qfmlr(ngs),qfdsv(ngs) 
      real qfwet(ngs),qfdry(ngs),qfshr(ngs)
      real qfshrp(ngs)

      real :: qhldpv(ngs), qhlsbv(ngs) 
      real :: qhlmlr(ngs), qhldsv(ngs) 
      real :: qhlwet(ngs), qhldry(ngs), qhlshr(ngs) 

      real :: qrfz(ngs),qsfz(ngs),qhfz(ngs),qhlfz(ngs)

      real qhdpv(ngs),qhsbv(ngs) 
      real qhmlr(ngs),qhdsv(ngs),qhcev(ngs),qhcndv(ngs),qhevv(ngs)
      real qhlcev(ngs), chlcev(ngs)
      real qhwet(ngs),qhdry(ngs),qhshr(ngs)
      real qhshrp(ngs)
      real qhshh(ngs) 
      real qhmlh(ngs) 
      real qhfzh(ngs) 
      real qhlfzhl(ngs) 

      real vhfzh(ngs) 
      real vhlfzhl(ngs) 

      real vhshdr(ngs) 
      real vhlshdr(ngs) 
      real vhmlr(ngs) 
      real vhlmlr(ngs) 
      real vhsoak(ngs) 
      real vhlsoak(ngs) 

      real qsdpv(ngs),qssbv(ngs) 
      real qsmlr(ngs),qsdsv(ngs),qscev(ngs),qscndv(ngs),qsevv(ngs)
      real qswet(ngs),qsdry(ngs),qsshr(ngs)
      real qsshrp(ngs)
      real qsfzs(ngs)


      real qipdpv(ngs),qipsbv(ngs)
      real qipmlr(ngs),qipdsv(ngs)

      real qirdpv(ngs),qirsbv(ngs)
      real qirmlr(ngs),qirdsv(ngs),qirmlw(ngs)

      real qgldpv(ngs),qglsbv(ngs)
      real qglmlr(ngs),qgldsv(ngs)
      real qglwet(ngs),qgldry(ngs),qglshr(ngs)
      real qglshrp(ngs)

      real qgmdpv(ngs),qgmsbv(ngs)
      real qgmmlr(ngs),qgmdsv(ngs)
      real qgmwet(ngs),qgmdry(ngs),qgmshr(ngs)
      real qgmshrp(ngs)
      real qghdpv(ngs),qghsbv(ngs)
      real qghmlr(ngs),qghdsv(ngs) 
      real qghwet(ngs),qghdry(ngs),qghshr(ngs)
      real qghshrp(ngs)

      real qrztot(ngs),qrzmax(ngs),qrzfac(ngs)
      real qrcev(ngs)
      real qrshr(ngs)
      real fsw(ngs),fhw(ngs),fhlw(ngs) 
      real qhcnf(ngs) 
      real :: qhlcnh(ngs) 
      real qhcngh(ngs),qhcngm(ngs),qhcngl(ngs)
      
      real :: qhcnhl(ngs), chcnhl(ngs), zhcnhl(ngs), vhcnhl(ngs) 

      real eiw(ngs),eii(ngs),eiri(ngs),eipir(ngs)
      real erw(ngs),esw(ngs),eglw(ngs),eghw(ngs),efw(ngs)
      real ehxw(ngs),ehlw(ngs),egmw(ngs),ehw(ngs)
      real err(ngs),esr(ngs),eglr(ngs),eghr(ngs),efr(ngs)
      real ehxr(ngs),ehlr(ngs),egmr(ngs) 
      real eri(ngs),esi(ngs),egli(ngs),eghi(ngs),efi(ngs)
      real ehxi(ngs),ehli(ngs),egmi(ngs),ehi(ngs) 
      real ers(ngs),ess(ngs),egls(ngs),eghs(ngs),efs(ngs),ehs(ngs)
      real ehscnv(ngs)
      real ehxs(ngs),ehls(ngs),egms(ngs),egmip(ngs) 

      real ew(8,6)
      real cwr(8,2)  
      data cwr / 2.0, 3.0, 4.0, 6.0,  8.0,  10.0, 15.0,  20.0 , & 
     &           1.0, 1.0, 0.5, 0.5,  0.5,   0.2,  0.2,  1.  /   
      integer icwr(ngs), igwr(ngs), irwr(ngs), ihlr(ngs)
      real grad(6,2) 
      data grad / 100., 200., 300., 400., 600., 1000.,   &
     &            1.e-2,1.e-2,1.e-2,5.e-3,2.5e-3, 1.    /

      data ew /0.03, 0.07, 0.17, 0.41, 0.58, 0.69, 0.82, 0.88,  & 

     &         0.10, 0.20, 0.34, 0.58, 0.70, 0.78, 0.88, 0.92,  & 
     &         0.15, 0.31, 0.44, 0.65, 0.75, 0.83, 0.96, 0.91,  & 
     &         0.17, 0.37, 0.50, 0.70, 0.81, 0.87, 0.93, 0.96,  & 
     &         0.17, 0.40, 0.54, 0.71, 0.83, 0.88, 0.94, 0.98,  & 
     &         0.15, 0.37, 0.52, 0.74, 0.82, 0.88, 0.94, 0.98 / 



      real da0lr(ngs)
      real da0lh(ngs)
      real da0lhl(ngs)



      real va0 (lc:lqmx)          
      real vab0(lc:lqmx,lc:lqmx)  
      real vab1(lc:lqmx,lc:lqmx)  
      real va1 (lc:lqmx)          
      real ehip(ngs),ehlip(ngs),ehlir(ngs)
      real erir(ngs),esir(ngs),eglir(ngs),egmir(ngs),eghir(ngs)
      real efir(ngs),ehir(ngs),eirw(ngs),eirir(ngs),ehr(ngs)
      real erip(ngs),esip(ngs),eglip(ngs),eghip(ngs)
      real efip(ngs),eipi(ngs),eipw(ngs),eipip(ngs)



      real ptotal(ngs) 

      real pqcwi(ngs),pqcii(ngs),pqrwi(ngs)
      real pqswi(ngs),pqhwi(ngs),pqwvi(ngs)
      real pqgli(ngs),pqghi(ngs),pqfwi(ngs)
      real pqgmi(ngs),pqhli(ngs) 
      real pqiri(ngs),pqipi(ngs) 
      real pqlwsi(ngs),pqlwhi(ngs),pqlwhli(ngs)
      
      real pvhwi(ngs), pvhwd(ngs)
      real pvhli(ngs), pvhld(ngs)
      real pvswi(ngs), pvswd(ngs)

      real pqcwd(ngs),pqcid(ngs),pqrwd(ngs)
      real pqswd(ngs),pqhwd(ngs),pqwvd(ngs)
      real pqgld(ngs),pqghd(ngs),pqfwd(ngs)
      real pqgmd(ngs),pqhld(ngs) 
      real pqird(ngs),pqipd(ngs) 
      real pqlwsd(ngs),pqlwhd(ngs),pqlwhld(ngs)



      real  pctot(ngs)
      real  pcipi(ngs), pcipd(ngs)
      real  pciri(ngs), pcird(ngs)
      real  pccwi(ngs), pccwd(ngs)
      real  pccii(ngs), pccid(ngs)
      real  pcrwi(ngs), pcrwd(ngs)
      real  pcswi(ngs), pcswd(ngs)
      real  pchwi(ngs), pchwd(ngs)
      real  pchli(ngs), pchld(ngs)
      real  pcfwi(ngs), pcfwd(ngs)
      real  pcgli(ngs), pcgld(ngs)
      real  pcgmi(ngs), pcgmd(ngs)
      real  pcghi(ngs), pcghd(ngs)

      real  pzrwi(ngs), pzrwd(ngs)
      real  pzhwi(ngs), pzhwd(ngs)
      real  pzhli(ngs), pzhld(ngs)
      real  pzswi(ngs), pzswd(ngs)




      real dqisdt(ngs) 

      real qss0(ngs)

      real advisc0,advisc1,tka0

      real qsacip(ngs)
      real pres(ngs)
      real pk(ngs)
      real rho0(ngs),pi0(ngs)
      real rhovt(ngs)
      real thetap(ngs),theta0(ngs),qwvp(ngs),qv0(ngs)
      real thsave(ngs)
      real ptwfzi(ngs),ptimlw(ngs)
      real psub(ngs),pvap(ngs),pfrz(ngs),ptem(ngs),pmlt(ngs),pevap(ngs),pdep(ngs)
      
      real cnostmp(ngs)   









      integer  iholef
      integer  iholen
      parameter (iholef = 1)
      parameter (iholen = 1)
      real  cqtotn,cqtotn1
      real  cctotn
      real  citotn
      real  crtotn
      real  cstotn
      real  cvtotn
      real  cftotn
      real  cgltotn
      real  cghtotn
      real  chtotn
      real  cqtotp,cqtotp1
      real  cctotp
      real  citotp
      real  ciptotp
      real  crtotp
      real  cstotp
      real  cvtotp
      real  cftotp
      real  chltotp
      real  cgltotp
      real  cgmtotp
      real  cghtotp
      real  chtotp
      real  cqfac
      real  ccfac
      real  cifac
      real  cipfac
      real  crfac
      real  csfac
      real  cvfac
      real  cffac
      real  cglfac
      real  cghfac
      real  chfac
      
      real ssifac, qvapor



      integer ireadqf,lrho,lqsw,lqgl,lqgm ,lqgh 
      integer lqrw
      real vt
      real arg  
      real erbnd1, fdgt1, costhe1
      real qeps
      real dyi2,dzi2,cp608,bta1,cnit,dragh,dnz00,rho00,pii
      real qccrit,gf4br,gf4ds,gf4p5, gf3ds, gf1ds,gr

      
      real xdn0(lc:lhab)
      real xdn_new,drhodt
      
      integer l ,ltemq,inumgs, idelq

      real c1f3,brz,arz,rw,temq

      real ssival,tqvcon
      real cdx(lc:lhab)
      real cnox
      real cval,aval,eval,fval,gval ,qsign,ftelwc,qconkq
      real qconm,qconn,cfce15,gf8,gf4i,gf3p5,gf1a,gf1p5,qdiff,argrcnw
      real c4,bradp,bl2,bt2,dtrh,hrifac, hdia0,hdia1,civenta,civentb
      real civentc,civentd,civente,civentf,civentg,cireyn,xcivent
      real cipventa,cipventb,cipventc,cipventd,cipreyn,cirventa
      real cirventb
      integer igmrwa,igmrwb,igmswa, igmswb,igmfwa,igmfwb,igmhwa,igmhwb
      real rwventa ,rwventb,swventa,swventb,fwventa,fwventb,fwventc
      real hwventa,hwventb
      real    hwventc, hlventa, hlventb,  hlventc
      real  glventa, glventb, glventc
      real   gmventa, gmventb,  gmventc, ghventa, ghventb, ghventc
      real  dzfacp,  dzfacm,  cmassin,  cwdiar 
      real  rimmas, rhobar
      real   argtim, argqcw, argqxw, argtem
      real   frcswsw, frcswgl, frcswgm, frcswgh, frcswfw, frcswsw1
      real   frcglgl, frcglgm, frcglgh,  frcglfw, frcglgl1
      real   frcgmgl, frcgmgm, frcgmgh,  frcgmfw, frcgmgm1
      real   frcghgl, frcghgm, frcghgh,  frcghfw,  frcghgh1
      real   frcfwgl, frcfwgm, frcfwgh, frcfwfw,  frcfwfw1
      real   frcswrsw, frcswrgl,  frcswrgm,  frcswrgh, frcswrfw
      real   frcswrsw1
      real   frcrswsw, frcrswgl, frcrswgm, frcrswgh, frcrswfw
      real  frcrswsw1
      real  frcglrgl, frcglrgm, frcglrgh,  frcglrfw, frcglrgl1
      real  frcrglgl
      real  frcrglgm,  frcrglgh, frcrglfw, frcrglgl1
      real  frcgmrgl, frcgmrgm, frcgmrgh, frcgmrfw,  frcgmrgm1
      real  frcrgmgl, frcrgmgm,  frcrgmgh, frcrgmfw, frcrgmgm1
      real  sum,  qweps,  gf2a, gf4a, dqldt, dqidt, dqdt
      real frcghrgl, frcghrgm, frcghrgh, frcghrfw, frcghrgh1, frcrghgl
      real frcrghgm, frcrghgh,  frcrghfw, frcrghgh1
      real    a1,a2,a3,a4,a5,a6
      real   gamss
      real cdw, cdi, denom1, denom2, delqci1, delqip1
      real cirtotn,  ciptotn, cgmtotn, chltotn,  cirtotp
      real  cgmfac, chlfac,  cirfac
      integer igmhla, igmhlb, igmgla, igmglb, igmgma,  igmgmb
      integer igmgha, igmghb
      integer idqis, item, itim0 
      integer  iqgl, iqgm, iqgh, iqrw, iqsw 
      integer  itertd, ia
      
      real tau, ewtmp
      
      integer cntnic_noliq
      real     q_noliqmn, q_noliqmx
      real     scsacimn, scsacimx
      
      double precision :: dtpinv
      


      integer nbin
      parameter (nbin=50)  
      real rn(nbin) 
      real rq(nbin),vtr(nbin) 
      
       real vtra(nbin)
       real hmmin,hjo

       parameter (hmmin = 1.e-11, hjo = 0.8*7.5 )
 
      integer itile,jtile,ktile
      integer ixend,jyend,kzend,kzbeg
      integer nxend,nyend,nzend,nzbeg
      
      real :: qaacw 











      iexy(:,:)=0; 


      iexy(ls,li) = ieswi
      iexy(ls,lc) = ieswc ; iexy(ls,lr) = ieswr ;


      iexy(lh,ls)  = iehwsw ; iexy(lh,li) = iehwi ;
      iexy(lh,lc) = iehwc ; iexy(lh,lr)  = iehwr ;


      IF (lhl .gt. 1 ) THEN
      iexy(lhl,ls)  = iehlsw ; iexy(lhl,li) = iehli ;
      iexy(lhl,lc) = iehlc ; iexy(lhl,lr)  = iehlr ;
      ENDIF


      itile = nx
      jtile = ny
      ktile = nz
      ixend = nx
      jyend = ny
      kzend = nz
      nxend = nx + 1
      nyend = ny + 1
      nzend = nz
      kzbeg = 1
      nzbeg = 1

      istag = 0
      jstag = 0
      kstag = 1





      IF ( ngs .lt. nz ) THEN


      ENDIF

      cntnic_noliq = 0
      q_noliqmn = 0.0
      q_noliqmx = 0.0
      scsacimn = 0.0
      scsacimx = 0.0

      ldovol = .false.

      DO il = lc,lhab
        ldovol = ldovol .or. ( lvol(il) .gt. 1 )
      ENDDO





      









      dtpinv = 1.d0/dtp








      qeps  = 1.0e-20







      cai = 21.87455
      caw = 17.2693882
      cbi = 7.66
      cbw = 35.86

      cp608 = 0.608
      ar = 841.99666  
      br = 0.8
      aradcw = -0.27544
      bradcw = 0.26249e+06
      cradcw = -1.8896e+10
      dradcw = 4.4626e+14
      bta1 = 0.6
      cnit = 1.0e-02
      dragh = 0.60
      dnz00 = 1.225
      rho00 = 1.225



      cs = 12.42
      ds = 0.42
      pii = piinv 
      pid4 = pi/4.0 

      gf1 = 1.0 
      gf1p5 = 0.8862269255  
      gf2 = 1.0 
      gf3 = 2.0 
      gf3p5 = 3.32335097 
      gf4 = 6.00 
      gf5 = 24.0 
      gf6 = 120.0 
      gf7 = 720.0 
      gf4br = 17.837861981813607 
      gf4ds = 10.41688578110938 
      gf4p5 = 11.63172839656745 
      gf3ds = 3.0458730354120997 
      gf1ds = 0.8863557896089221 
      gr = 9.8
      gf43rds = 0.8929795116 
      gf53rds = 0.9027452930 
      gf73rds = 1.190639349 
      gf83rds = 1.504575488 



      c1f3 = 1.0/3.0



      brz = 100.0
      arz = 0.66
      cai = 21.87455
      caw = 17.2693882
      cbi = 7.66
      cbw = 35.86
      
      bfnu1 = (4. + alphar)*(5. + alphar)*(6. + alphar)/((1. + alphar)*(2. + alphar)*(3. + alphar))
      
      vfrz = 0.523599*(dfrz)**3 
      vmlt = Min(xvmx(lr), 0.523599*(dmlt)**3 )
      vshd = Min(xvmx(lr), 0.523599*(dshd)**3 )

      

      tdtol = 1.0e-05
      thnuc = 233.15
      rw = 461.5              
      advisc0 = 1.832e-05
      advisc1 = 1.718e-05    
      tka0 = 2.43e-02        
      tfrcbw = tfr - cbw
      tfrcbi = tfr - cbi





      cwmasn = 5.23e-13  
      cwmasn5 =  5.23e-13
      cwradn = 5.0e-6
      cwmasx = 5.25e-10  
      mwfac = 6.0**(1./3.)
      IF ( ipconc .ge. 2 ) THEN
        cwmasn = xvmn(lc)*1000.
        cwradn = 1.0e-6
        cwmasx = xvmx(lc)*1000.
      ENDIF
        rwmasn = xvmn(lr)*1000.
        rwmasx = xvmx(lr)*1000.




      cimasn = Min(cimas0, 6.88e-13) 
      cimasx = 1.0e-8   
      ccimx = 5000.0e3   








      iend = 0
















      jy = jgs
      
      IF ( ipconc < 2 ) THEN 
        DO kz = 1,nz
         DO ix = 1,nx
           t9(ix,jy,kz) = an(ix,jy,kz,lc)
         ENDDO
        ENDDO
      ENDIF
      



      if ( ndebug .gt. 0 ) write(0,*) 'ICEZVD_GS: ENTER GATHER STAGE'


      nxmpb = 1
      nzmpb = 1
      nxz = nx*nz
      numgs = nxz/ngs + 1


      do 1000 inumgs = 1,numgs
      ngscnt = 0
      
      do kz = nzmpb,nz
      do ix = nxmpb,nx

      pqs(1) = t00(ix,jy,kz)


      theta(1) = an(ix,jy,kz,lt)
      temg(1) = t0(ix,jy,kz)
      temcg(1) = temg(1) - tfr
      tqvcon = temg(1)-cbw
      ltemq = (temg(1)-163.15)/fqsat+1.5
      ltemq = Min( nqsat, Max(1,ltemq) )
      qvs(1) = pqs(1)*tabqvs(ltemq)
      qis(1) = pqs(1)*tabqis(ltemq)

      qss(1) = qvs(1)





      if ( temg(1) .lt. tfr ) then





      qss(1) = qis(1)
      else




      end if

      ishail = .false.
      IF ( lhl > 1 ) THEN
        IF ( an(ix,jy,kz,lhl)  .gt. qxmin(lhl) ) ishail = .true.
      ENDIF
      
      if ( an(ix,jy,kz,lv)  .gt. qss(1) .or.   &
     &     an(ix,jy,kz,lc)  .gt. qxmin(lc)   .or.    &
     &     an(ix,jy,kz,li)  .gt. qxmin(li)   .or.   &
     &     an(ix,jy,kz,lr)  .gt. qxmin(lr)   .or.   &
     &     an(ix,jy,kz,ls)  .gt. qxmin(ls)   .or.   &
     &     an(ix,jy,kz,lh)  .gt. qxmin(lh)   .or.  ishail ) then
      ngscnt = ngscnt + 1
      igs(ngscnt) = ix
      kgs(ngscnt) = kz
      if ( ngscnt .eq. ngs ) goto 1100
      end if
      enddo 
      nxmpb = 1
      enddo 
 1100 continue

      if ( ngscnt .eq. 0 ) go to 9998

      if ( ndebug .gt. 0 ) write(0,*) 'ICEZVD_GS: dbg = 5'



      
      xv(:,:) = 0.0

      xmas(:,:) = 0.0
      vtxbar(:,:,:) = 0.0
      xdia(:,:,:) = 0.0
      raindn(:,:) = 900.
      cx(:,:) = 0.0
      alpha(:,:) = 0.0
      DO il = li,lhab
        DO mgs = 1,ngscnt
          rimdn(mgs,il)  = rimedens 
        ENDDO
      ENDDO



      do mgs = 1,ngscnt
      kgsm(mgs) = max(kgs(mgs)-1,1)
      kgsm2(mgs) = Max(kgs(mgs)-2,1)
      kgsp(mgs) = min(kgs(mgs)+1,nz-1)
      theta0(mgs) = 0.0
      thetap(mgs) = an(igs(mgs),jy,kgs(mgs),lt) - theta0(mgs)
      theta(mgs) = an(igs(mgs),jy,kgs(mgs),lt)
      qv0(mgs) = 0.0
      qwvp(mgs) = an(igs(mgs),jy,kgs(mgs),lv)  - qv0(mgs) 

      pres(mgs) = pn(igs(mgs),jy,kgs(mgs))
      rho0(mgs) = dn(igs(mgs),jy,kgs(mgs))
      rhoinv(mgs) = 1.0/rho0(mgs)
      rhovt(mgs) = Sqrt(rho00/rho0(mgs))
      pi0(mgs) = p2(igs(mgs),jy,kgs(mgs)) 
      temg(mgs) = t0(igs(mgs),jy,kgs(mgs))
      temgkm1(mgs) = t0(igs(mgs),jy,kgsm(mgs))
      temgkm2(mgs) = t0(igs(mgs),jy,kgsm2(mgs))
      pk(mgs)   = p2(igs(mgs),jy,kgs(mgs)) 
      temcg(mgs) = temg(mgs) - tfr
      qss0(mgs) = (380.0)/(pres(mgs))
      pqs(mgs) = (380.0)/(pres(mgs))
      ltemq = (temg(mgs)-163.15)/fqsat+1.5
      ltemq = Min( nqsat, Max(1,ltemq) )
      qvs(mgs) = pqs(mgs)*tabqvs(ltemq)
      qis(mgs) = pqs(mgs)*tabqis(ltemq)
      es(mgs)  = 6.1078e2*tabqvs(ltemq)
      eis(mgs) = 6.1078e2*tabqis(ltemq)
      cnostmp(mgs) = cno(ls)

      il5(mgs) = 0
      if ( temg(mgs) .lt. tfr ) then
      il5(mgs) = 1
      end if
      enddo 
      
      IF ( ipconc < 1 .and. lwsm6 ) THEN
        DO mgs = 1,ngscnt
          tmp = Min( 0.0, temcg(mgs) )
          cnostmp(mgs) = Min( 2.e8, 2.e6*exp(0.12*tmp) )
        ENDDO
      ENDIF





      do mgs = 1,ngscnt
         qhshr(mgs) = 0.0 
       end do



      DO il = lv,lhab
      do mgs = 1,ngscnt
        qx(mgs,il) = max(an(igs(mgs),jy,kgs(mgs),il), 0.0) 
      ENDDO
      end do

      qxw(:,:) = 0.0


        scx(:,:) = 0.0



      IF ( imurain == 1 ) THEN
        alpha(:,lr) = alphar
      ELSEIF ( imurain == 3 ) THEN
        alpha(:,lr) = xnu(lr)
      ENDIF

      DO il = lc,lhab
      do mgs = 1,ngscnt
        IF ( il .ge. lg ) alpha(mgs,il) = dnu(il)
        DO ic = lr,lhab
        dab0lh(mgs,il,ic) = dab0(ic,il)
        dab1lh(mgs,il,ic) = dab1(ic,il)
        ENDDO
      ENDDO
      end do
      
      

        da0lh(:) = da0(lh)
        da0lr(:) = da0(lr)
        IF ( lzh < 1 .or. lzhl < 1 ) THEN
          rzxhlh(:) = rzhl/rz
        ELSEIF ( lzh > 1 .and. lzhl > 1 ) THEN
          rzxhlh(:) = 1.
        ENDIF
        IF ( lzr > 1 ) THEN
          rzxh(:) = 1.
          rzxhl(:) = 1.
        ELSE
          rzxh(:) = rz
          rzxhl(:) = rzhl
        ENDIF
 
      
      IF ( lhl .gt. 1 ) THEN
      DO mgs = 1,ngscnt
        da0lhl(mgs) = da0(lhl)
      ENDDO
      ENDIF
      
      ventrx(:) = ventr
      ventrxn(:) = ventrn








      if ( ipconc .ge. 1 ) then
       do mgs = 1,ngscnt
        cx(mgs,li) = Max(an(igs(mgs),jy,kgs(mgs),lni), 0.0)
        IF ( lcina .gt. 1 ) THEN
         cina(mgs) = an(igs(mgs),jy,kgs(mgs),lcina)
        ELSE
         cina(mgs) = cx(mgs,li)
        ENDIF
        IF ( lcin > 1 ) THEN
         ccin(mgs) = an(igs(mgs),jy,kgs(mgs),lcin)
        ENDIF
        IF ( qx(mgs,li) .le. qxmin(li) .or. cx(mgs,li) .le. 0.0 ) THEN
          cx(mgs,li) = 0.0
          an(igs(mgs),jy,kgs(mgs),lni) = 0.0
          qx(mgs,lv) = qx(mgs,lv) +  qx(mgs,li)
          qx(mgs,li) = 0.0
        ENDIF
       end do
      end if
      if ( ipconc .ge. 2 ) then
       do mgs = 1,ngscnt
        cx(mgs,lc) = Max(an(igs(mgs),jy,kgs(mgs),lnc), 0.0)
        cx(mgs,lc) = Min( ccwmx, cx(mgs,lc) )
        IF ( qx(mgs,lc) .le. qxmin(lc) .or. cx(mgs,lc) .le. 0.0  ) THEN
          cx(mgs,lc) = 0.0
          an(igs(mgs),jy,kgs(mgs),lnc) = 0.0
          qx(mgs,lv) = qx(mgs,lv) +  qx(mgs,lc)
          qx(mgs,lc) = 0.0
        ENDIF
        IF ( lccn .gt. 1 ) THEN
         ccnc(mgs) = an(igs(mgs),jy,kgs(mgs),lccn)
        ELSE
         ccnc(mgs) = 0.0
        ENDIF
       end do


      end if
      if ( ipconc .ge. 3 ) then
       do mgs = 1,ngscnt
        cx(mgs,lr) = Max(an(igs(mgs),jy,kgs(mgs),lnr), 0.0)
        IF ( qx(mgs,lr) .le. qxmin(lr) .or. cx(mgs,lr) .le. 0.0 ) THEN
          cx(mgs,lr) = 0.0
          an(igs(mgs),jy,kgs(mgs),lnr) = 0.0
          qx(mgs,lv) = qx(mgs,lv) +  qx(mgs,lr)
          qx(mgs,lr) = 0.0
        ENDIF
        IF ( cx(mgs,lr) .eq. 0.0 .and. qx(mgs,lr) .lt. 3.0*qxmin(lr) ) THEN
          qx(mgs,lv) = qx(mgs,lv) + qx(mgs,lr)
          qx(mgs,lr) = 0.0
        ELSE
          cx(mgs,lr) = Max( 1.e-9, cx(mgs,lr) )
       IF ( .not. ( cx(mgs,lr) < 1.e30 .and. cx(mgs,lr) > -1.e20 ) ) THEN
         write(0,*) 'icezvd_gs: problem with cx(mgs,lr)! ',qx(mgs,lr),cx(mgs,lr)
         STOP
       ENDIF
        ENDIF

       end do
      end if
      if ( ipconc .ge. 4 ) then
       do mgs = 1,ngscnt
        cx(mgs,ls) = Max(an(igs(mgs),jy,kgs(mgs),lns), 0.0)
        IF ( qx(mgs,ls) .le. qxmin(ls) .or. cx(mgs,ls) .le. 0.0 ) THEN
          cx(mgs,ls) = 0.0
          an(igs(mgs),jy,kgs(mgs),lns) = 0.0
          qx(mgs,lv) = qx(mgs,lv) +  qx(mgs,ls)
          qx(mgs,ls) = 0.0
        ENDIF
        IF ( cx(mgs,ls) .eq. 0.0 .and. qx(mgs,ls) .lt. 3.0*qxmin(ls) ) THEN
          qx(mgs,lv) = qx(mgs,lv) + qx(mgs,ls)
          qx(mgs,ls) = 0.0
        ELSE
          cx(mgs,ls) = Max( 1.e-9, cx(mgs,ls) )

         IF ( ilimit .ge. ipc(ls) ) THEN
            tmp = (xdn0(ls)*cx(mgs,ls))/(rho0(mgs)*qx(mgs,ls))
            tmp2 = (tmp*(3.14159))**(1./3.)
            cnox = cx(mgs,ls)*(tmp2)
         IF ( cnox .gt. 3.0*cno(ls) ) THEN
           cx(mgs,ls) = 3.0*cno(ls)/tmp2
         ENDIF
         ENDIF
        ENDIF
       end do
      end if
      if ( ipconc .ge. 5 ) then
       do mgs = 1,ngscnt

        cx(mgs,lh) = Max(an(igs(mgs),jy,kgs(mgs),lnh), 0.0)
        IF ( qx(mgs,lh) .le. qxmin(lh) .or. cx(mgs,lh) .le. 0.0 ) THEN
          cx(mgs,lh) = 0.0
          an(igs(mgs),jy,kgs(mgs),lnh) = 0.0
          qx(mgs,lv) = qx(mgs,lv) +  qx(mgs,lh)
          qx(mgs,lh) = 0.0
        ENDIF
        IF ( cx(mgs,lh) .eq. 0.0 .and. qx(mgs,lh) .lt. 3.0*qxmin(lh) ) THEN
          qx(mgs,lv) = qx(mgs,lv) + qx(mgs,lh)
          qx(mgs,lh) = 0.0
        ELSE
          cx(mgs,lh) = Max( 1.e-9, cx(mgs,lh) )
         IF ( ilimit .ge. ipc(lh) ) THEN
            tmp = (xdn0(lh)*cx(mgs,lh))/(rho0(mgs)*qx(mgs,lh))
            tmp2 = (tmp*(3.14159))**(1./3.)
            cnox = cx(mgs,lh)*(tmp2)
         IF ( cnox .gt. 3.0*cno(lh) ) THEN
           cx(mgs,lh) = 3.0*cno(lh)/tmp2
         ENDIF
         ENDIF
        ENDIF
       end do
      end if

      if ( lhl .gt. 1 .and. ipconc .ge. 5 ) then
       do mgs = 1,ngscnt

        cx(mgs,lhl) = Max(an(igs(mgs),jy,kgs(mgs),lnhl), 0.0)
        IF ( qx(mgs,lhl) .le. qxmin(lhl) .or. cx(mgs,lhl) .le. 0.0 ) THEN
          cx(mgs,lhl) = 0.0
          an(igs(mgs),jy,kgs(mgs),lnhl) = 0.0
          qx(mgs,lv) = qx(mgs,lv) +  qx(mgs,lhl)
          qx(mgs,lhl) = 0.0
        ENDIF
        IF ( cx(mgs,lhl) .eq. 0.0 .and. qx(mgs,lhl) .lt. 3.0*qxmin(lhl) ) THEN
          qx(mgs,lv) = qx(mgs,lv) + qx(mgs,lhl)
          qx(mgs,lhl) = 0.0
        ELSE
          cx(mgs,lhl) = Max( 1.e-9, cx(mgs,lhl) )
         IF ( ilimit .ge. ipc(lhl) ) THEN
            tmp = (xdn0(lhl)*cx(mgs,lhl))/(rho0(mgs)*qx(mgs,lhl))
            tmp2 = (tmp*(3.14159))**(1./3.)
            cnox = cx(mgs,lhl)*(tmp2)
         IF ( cnox .gt. 3.0*cno(lhl) ) THEN
           cx(mgs,lhl) = 3.0*cno(lhl)/tmp2
         ENDIF
         ENDIF
        ENDIF
       end do
      end if




      IF ( ldovol ) THEN

      vx(:,:) = 0.0

       DO il = li,lhab

        IF ( lvol(il) .ge. 1 ) THEN

          DO mgs = 1,ngscnt
            vx(mgs,il) = Max(an(igs(mgs),jy,kgs(mgs),lvol(il)), 0.0)
          ENDDO

        ENDIF

       ENDDO

      ENDIF








      do mgs = 1,ngscnt

      ssi(mgs) = qx(mgs,lv)/qis(mgs)
      ssw(mgs) = qx(mgs,lv)/qvs(mgs)

      tsqr(mgs) = temg(mgs)**2

      temgx(mgs) = min(temg(mgs),313.15)
      temgx(mgs) = max(temgx(mgs),233.15)
      felv(mgs) = 2500837.367 * (273.15/temgx(mgs))**((0.167)+(3.67e-4)*temgx(mgs))

      temcgx(mgs) = min(temg(mgs),273.15)
      temcgx(mgs) = max(temcgx(mgs),223.15)
      temcgx(mgs) = temcgx(mgs)-273.15


      felf(mgs) = 333690.6098 + (2030.61425)*temcgx(mgs) - (10.46708312)*temcgx(mgs)**2

      fels(mgs) = felv(mgs) + felf(mgs)

      felvs(mgs) = felv(mgs)*felv(mgs)
      felss(mgs) = fels(mgs)*fels(mgs)
      
        IF ( eqtset <= 1 ) THEN
          felvcp(mgs) = felv(mgs)*cpi
          felscp(mgs) = fels(mgs)*cpi
          felfcp(mgs) = felf(mgs)*cpi
        ELSE
          tmp = qx(mgs,li)+qx(mgs,ls)+qx(mgs,lh)
          IF ( lhl > 1 ) tmp = tmp + qx(mgs,lhl)
          cvm = cv+cvv*qx(mgs,lv)+cpl*(qx(mgs,lc)+qx(mgs,lr))   &
                                  +cpigb*(tmp)
          felvcp(mgs) = (felv(mgs)-rw*temg(mgs))/cvm
          felscp(mgs) = (fels(mgs)-rw*temg(mgs))/cvm
          felfcp(mgs) = felf(mgs)/cvm
        ENDIF

      fgamw(mgs) = felvcp(mgs)/pi0(mgs)
      fgams(mgs) = felscp(mgs)/pi0(mgs)

      fcqv1(mgs) = 4098.0258*pi0(mgs)*fgamw(mgs)
      fcqv2(mgs) = 5807.6953*pi0(mgs)*fgams(mgs)
      fcc3(mgs) = felfcp(mgs)/pi0(mgs)


      fwvdf(mgs) = (2.11e-05)*((temg(mgs)/tfr)**1.94)*(101325.0/(pres(mgs)))


      fadvisc(mgs) = advisc0*(416.16/(temg(mgs)+120.0))*(temg(mgs)/296.0)**(1.5)

      fakvisc(mgs) = fadvisc(mgs)*rhoinv(mgs)

      temcgx(mgs) = min(temg(mgs),273.15)
      temcgx(mgs) = max(temcgx(mgs),233.15)
      temcgx(mgs) = temcgx(mgs)-273.15
      fci(mgs) = (2.118636 + 0.007371*(temcgx(mgs)))*(1.0e+03)

      if ( temg(mgs) .lt. 273.15 ) then
      temcgx(mgs) = min(temg(mgs),273.15)
      temcgx(mgs) = max(temcgx(mgs),233.15)
      temcgx(mgs) = temcgx(mgs)-273.15
      fcw(mgs) = 4203.1548  + (1.30572e-2)*((temcgx(mgs)-35.)**2)   &
     &                 + (1.60056e-5)*((temcgx(mgs)-35.)**4)
      end if
      if ( temg(mgs) .ge. 273.15 ) then
      temcgx(mgs) = min(temg(mgs),308.15)
      temcgx(mgs) = max(temcgx(mgs),273.15)
      temcgx(mgs) = temcgx(mgs)-273.15
      fcw(mgs) = 4243.1688  + (3.47104e-1)*(temcgx(mgs)**2)
      end if

      ftka(mgs) = tka0*fadvisc(mgs)/advisc1  


      fschm(mgs) = (fakvisc(mgs)/fwvdf(mgs))  


      fai(mgs) = (fels(mgs)**2)/(ftka(mgs)*rw*temg(mgs)**2)
      fbi(mgs) = (1.0/(rho0(mgs)*fwvdf(mgs)*qis(mgs)))
      fav(mgs) = (felv(mgs)**2)/(ftka(mgs)*rw*temg(mgs)**2)
      fbv(mgs) = (1.0/(rho0(mgs)*fwvdf(mgs)*qvs(mgs)))

      end do








      if (ndebug .gt. 0 ) write(0,*) 'ICEZVD_GS: Set density'


      do mgs = 1,ngscnt
        xdn(mgs,li) = xdn0(li)
        xdn(mgs,lc) = xdn0(lc)
        xdn(mgs,lr) = xdn0(lr)
        xdn(mgs,ls) = xdn0(ls)
        xdn(mgs,lh) = xdn0(lh)
        IF ( lvol(ls) .gt. 1 ) THEN
         IF ( vx(mgs,ls) .gt. 0.0 .and. qx(mgs,ls) .gt. qxmin(ls) ) THEN
           xdn(mgs,ls) = Min( xdnmx(ls), Max( xdnmn(ls), rho0(mgs)*qx(mgs,ls)/vx(mgs,ls) ) )
         ENDIF
        ENDIF

        IF ( lvol(lh) .gt. 1 ) THEN
         IF ( vx(mgs,lh) .gt. 0.0 .and. qx(mgs,lh) .gt. qxmin(lh) ) THEN
           IF ( mixedphase ) THEN
           ELSE
             dnmx = xdnmx(lh)
           ENDIF
           xdn(mgs,lh) = Min( dnmx, Max( xdnmn(lh), rho0(mgs)*qx(mgs,lh)/vx(mgs,lh) ) )
           vx(mgs,lh) = rho0(mgs)*qx(mgs,lh)/xdn(mgs,lh)
         ENDIF
        ENDIF

        IF ( lhl .gt. 1 ) THEN

          xdn(mgs,lhl) = xdn0(lhl)

          IF ( lvol(lhl) .gt. 1 ) THEN
           IF ( vx(mgs,lhl) .gt. 0.0 .and. qx(mgs,lhl) .gt. qxmin(lhl) ) THEN

           IF ( mixedphase .and. lhlw > 1 ) THEN
           ELSE
             dnmx = xdnmx(lhl)
           ENDIF

             xdn(mgs,lhl) = Min( dnmx, Max( xdnmn(lhl), rho0(mgs)*qx(mgs,lhl)/vx(mgs,lhl) ) )
             vx(mgs,lhl) = rho0(mgs)*qx(mgs,lhl)/xdn(mgs,lhl)
           ENDIF
          ENDIF

        ENDIF





          IF (qsdenmod) THEN
           IF(fsw(mgs) .gt. 0.01) THEN
            xdn(mgs,ls) = (1.-fsw(mgs))*rho_qs + fsw(mgs)*rho_qr        
            IF(fsw(mgs) .eq. 1.) xdn(mgs,ls) = rho_qr   
           ENDIF
          ENDIF

          IF (qhdenmod) THEN




          ENDIF


      end do





      do mgs = 1,ngscnt
      kp1 = Min(nz, kgs(mgs)+1 )
      wvel(mgs) = (0.5)*(w(igs(mgs),jgs,kp1)   &
     &                  +w(igs(mgs),jgs,kgs(mgs)))
      wvelkm1(mgs) = (0.5)*(w(igs(mgs),jgs,kgs(mgs))   &
     &                  +w(igs(mgs),jgs,Max(1,kgs(mgs)-1)))
      kgsm(mgs) = max(kgs(mgs)-1,1)
      kgsp(mgs) = min(kgs(mgs)+1,nz-1)
      cninm(mgs) = t7(igs(mgs),jgs,kgsm(mgs))
      cnina(mgs) = t7(igs(mgs),jgs,kgs(mgs))
      cninp(mgs) = t7(igs(mgs),jgs,kgsp(mgs))
      end do















      call setvtz(ngscnt,ngs,qx,qxmin,qxw,cx,rho0,rhovt,xdia,cno,cnostmp,   &
     &                 xmas,vtxbar,xdn,xvmn,xvmx,xv,cdx,   &
     &                 ipconc,ndebug,ngs,nz,kgs,fadvisc,   &
     &                 cwmasn,cwmasx,cwradn,cnina,cimn,cimx,   &
     &                 itype1,itype2,temcg,0,alpha,0)


       IF ( lwsm6 .and. ipconc == 0 ) THEN
         tmp = Max(qxmin(lh), qxmin(ls))
         DO mgs = 1,ngscnt
           sum = qx(mgs,lh) + qx(mgs,ls)
           IF ( sum > tmp ) THEN
             vt2ave(mgs) = (qx(mgs,lh)*vtxbar(mgs,lh,1) + qx(mgs,ls)*vtxbar(mgs,ls,1))/sum
           ELSE
             vt2ave(mgs) = 0.0
           ENDIF
         ENDDO
       ENDIF





      if ( ndebug .gt. 0 ) write(0,*) 'ICEZVD_GS: Set concentration'
      IF ( ipconc .lt. 1 ) THEN
         cina(1:ngscnt) = cx(1:ngscnt,li)
      ENDIF
      if ( ipconc .lt. 5 ) then
      do mgs = 1,ngscnt


      IF ( ipconc .lt. 3 ) THEN

      if ( qx(mgs,lr) .gt. qxmin(lh) )  then


      end if
      ENDIF

      IF ( ipconc .lt. 4 ) THEN


      if ( qx(mgs,ls) .gt. qxmin(ls) )  then


      end if
      ENDIF 

      IF ( ipconc .lt. 5 ) THEN



      if ( qx(mgs,lh) .gt. qxmin(lh) )  then



      end if

      ENDIF 

      end do
      end if

      IF ( ipconc .ge. 2 ) THEN
      DO mgs = 1,ngscnt
        rb(mgs) = 0.5*xdia(mgs,lc,1)*((1./(1.+cnu)))**(1./6.)
        xl2p(mgs) = Max(0.0d0, 2.7e-2*xdn(mgs,lc)*cx(mgs,lc)*xv(mgs,lc)*   &
     &           ((0.5e20*rb(mgs)**3*xdia(mgs,lc,1))-0.4) )
        IF ( rb(mgs) .gt. 3.51e-6 ) THEN

          rh(mgs) = Max( 41.d-6, 6.3d-4/(1.d6*(rb(mgs) - 3.5d-6)) )
        ELSE
          rh(mgs) = 41.d-6
        ENDIF
        IF ( xl2p(mgs) .gt. 0.0 ) THEN
          nh(mgs) = 4.2d9*xl2p(mgs)
        ELSE
          nh(mgs) = 1.e30
        ENDIF
      ENDDO
      ENDIF








      if( ndebug .ge. 0 ) THEN


      endif
      do mgs = 1,ngscnt
      qvimxd(mgs) = 0.70*(qx(mgs,lv)-qis(mgs))/dtp 
      qvimxd(mgs) = max(qvimxd(mgs), 0.0)






      frac = 0.1d0
      qimxd(mgs)  = frac*qx(mgs,li)/dtp
      qcmxd(mgs)  = frac*qx(mgs,lc)/dtp
      qrmxd(mgs)  = frac*qx(mgs,lr)/dtp
      qsmxd(mgs)  = frac*qx(mgs,ls)/dtp
      qhmxd(mgs)  = frac*qx(mgs,lh)/dtp
      IF ( lhl > 1 ) qhlmxd(mgs)  = frac*qx(mgs,lhl)/dtp
      end do

      if( ndebug .ge. 0 ) THEN


      endif

      do mgs = 1,ngscnt

      if ( qx(mgs,lc) .le. qxmin(lc) ) then
      ccmxd(mgs)  = 0.20*cx(mgs,lc)/dtp
      else
      IF ( ipconc .ge. 2 ) THEN
        ccmxd(mgs)  = frac*cx(mgs,lc)/dtp
      ELSE
        ccmxd(mgs)  = frac*qx(mgs,lc)/(xmas(mgs,lc)*rho0(mgs)*dtp)
      ENDIF
      end if

      if ( qx(mgs,li) .le. qxmin(li) ) then
      cimxd(mgs)  = frac*cx(mgs,li)/dtp
      else
      IF ( ipconc .ge. 1 ) THEN
        cimxd(mgs)  = frac*cx(mgs,li)/dtp
      ELSE
        cimxd(mgs)  = frac*qx(mgs,li)/(xmas(mgs,li)*rho0(mgs)*dtp)
      ENDIF
      end if


      crmxd(mgs)  = 0.10*cx(mgs,lr)/dtp
      csmxd(mgs)  = frac*cx(mgs,ls)/dtp
      chmxd(mgs)  = frac*cx(mgs,lh)/dtp

      ccmxd(mgs)  = frac*cx(mgs,lc)/dtp
      cimxd(mgs)  = frac*cx(mgs,li)/dtp
      crmxd(mgs)  = frac*cx(mgs,lr)/dtp
      csmxd(mgs)  = frac*cx(mgs,ls)/dtp
      chmxd(mgs)  = frac*cx(mgs,lh)/dtp

      qxmxd(mgs,lv) = Max(0.0, 0.1*(qx(mgs,lv) - qvs(mgs))/dtp)

      DO il = lc,lhab
       qxmxd(mgs,il) = frac*qx(mgs,il)/dtp
       cxmxd(mgs,il) = frac*cx(mgs,il)/dtp
      ENDDO


      end do












      if (ndebug .gt. 0 ) write(0,*) 'ICEZVD_GS: Set collection efficiencies'

      do mgs = 1,ngscnt



      erw(mgs) = 0.0
      esw(mgs) = 0.0
      ehw(mgs) = 0.0
      ehlw(mgs) = 0.0


      err(mgs) = 0.0
      esr(mgs) = 0.0
      il2(mgs) = 0
      il3(mgs) = 0
      ehr(mgs) = 0.0
      ehlr(mgs) = 0.0


      eri(mgs) = 0.0
      esi(mgs) = 0.0
      ehi(mgs) = 0.0
      ehli(mgs) = 0.0


      ers(mgs) = 0.0
      ess(mgs) = 0.0
      ehs(mgs) = 0.0
      ehls(mgs) = 0.0
      ehscnv(mgs) = 0.0


      eiw(mgs) = 0.0
      eii(mgs) = 0.0

      icwr(mgs) = 1
      IF ( qx(mgs,lc) .gt. qxmin(lc) ) THEN
       cwrad = 0.5*xdia(mgs,lc,1)
      DO il = 1,8
         IF ( cwrad .ge. 1.e-6*cwr(il,1) ) icwr(mgs) = il
      ENDDO
      ENDIF


      irwr(mgs) = 1
      IF ( qx(mgs,lr) .gt. qxmin(lr) ) THEN
         rwrad = 0.5*xdia(mgs,lr,3)  
      DO il = 1,6
         IF ( rwrad .ge. 1.e-6*grad(il,1) ) irwr(mgs) = il
      ENDDO
      ENDIF


      igwr(mgs) = 1



      IF ( qx(mgs,lh) .gt. qxmin(lh) ) THEN
         rwrad = 0.5*xdia(mgs,lh,3)  
      DO il = 1,6
         IF ( rwrad .ge. 1.e-6*grad(il,1) ) igwr(mgs) = il
      ENDDO
      ENDIF

      IF ( lhl .gt. 1 ) THEN 
      ihlr(mgs) = 1
      IF ( qx(mgs,lhl) .gt. qxmin(lhl) ) THEN
         rwrad = 0.5*xdia(mgs,lhl,3)  
      DO il = 1,6
         IF ( rwrad .ge. 1.e-6*grad(il,1) ) ihlr(mgs) = il
      ENDDO
      ENDIF
      ENDIF






      if ( qx(mgs,li) .gt. qxmin(li) ) then







        eii(mgs) = exp(0.025*Min(temcg(mgs),0.0))  

      if ( temg(mgs) .gt. 273.15 ) eii(mgs) = 1.0
      end if






      eiw(mgs) = 0.0
      if ( qx(mgs,li).gt.qxmin(li) .and. qx(mgs,lc).gt.qxmin(lc) ) then
      if (xdia(mgs,lc,1).gt.15.0e-06 .and. xdia(mgs,li,1).gt.30.0e-06) then


      eiw(mgs) = 0.5
      end if
      if ( temg(mgs) .ge. 273.15 ) eiw(mgs) = 0.0
      end if






      if ( qx(mgs,lr).gt.qxmin(lr) .and. qx(mgs,lc).gt.qxmin(lc) ) then

       IF ( lnr .gt. 1 ) THEN
       erw(mgs) = 1.0

       ELSE










       ic = icwr(mgs)
       icp1 = Min( 8, ic+1 )
       ir = irwr(mgs)
       irp1 = Min( 6, ir+1 )
       cwrad = 0.5*xdia(mgs,lc,3)
       rwrad = 0.5*xdia(mgs,lr,3)
       
       slope1 = (ew(icp1, ir  ) - ew(ic,ir  ))*cwr(ic,2)
       slope2 = (ew(icp1, irp1) - ew(ic,irp1))*cwr(ic,2)



       x1 = ew(ic,  ir) + slope1*Max(0.0, (cwrad - cwr(ic,1)) )
       x2 = ew(icp1,ir) + slope2*Max(0.0, (cwrad - cwr(ic,1)) )

       slope1 = (x2 - x1)*grad(ir,2)

       erw(mgs) = Max(0.0, x1 + slope1*Max(0.0, (rwrad - grad(ir,1)) ))




       erw(mgs) = Max(0.0, erw(mgs) )
       IF ( rwrad .lt. 50.e-6 ) THEN
         erw(mgs) = 0.0
       ELSEIF (  rwrad .lt. 100.e-6 ) THEN  
         erw(mgs) = erw(mgs)*(rwrad - 50.e-6)/50.e-6
       ENDIF

       ENDIF
      end if
      IF ( cx(mgs,lc) .le. 0.0 ) erw(mgs) = 0.0

      if ( qx(mgs,lr).gt.qxmin(lr) .and. qx(mgs,lr).gt.qxmin(lr) ) then
      err(mgs)=1.0
      end if

      if ( qx(mgs,lr).gt.qxmin(lr) .and. qx(mgs,ls).gt.qxmin(ls) ) then
      ers(mgs)=1.0
      end if

      if ( qx(mgs,lr).gt.qxmin(lr) .and. qx(mgs,li).gt.qxmin(li) ) then


         eri(mgs) = eri0






       if ( xdia(mgs,li,1) .lt. 40.e-6 ) eri(mgs)=0.0
      end if









      esw(mgs) = 0.0
      if ( qx(mgs,ls).gt.qxmin(ls) .and. qx(mgs,lc).gt.qxmin(lc) ) then
        esw(mgs) = 0.5
        if ( xdia(mgs,lc,1) .gt. 15.e-6 .and. xdia(mgs,ls,1) .gt. 100.e-6) then
          esw(mgs) = 0.5
        ELSEIF ( xdia(mgs,ls,1) .ge. 500.e-6 ) THEN
          esw(mgs) = Min(0.5, 0.05 + (0.8-0.05)/(40.e-6)*xdia(mgs,lc,1) )
        ENDIF
      end if

      if ( qx(mgs,ls).gt.qxmin(ls) .and. qx(mgs,lr).gt.qxmin(lr)  &
     &     .and. temg(mgs) .lt. tfr - 1.   &
     &                               ) then
      esr(mgs)=Exp(-(40.e-6)**3/xv(mgs,lr))*Exp(-40.e-6/xdia(mgs,ls,1))
      IF ( qx(mgs,ls) < 1.e-4 .and. qx(mgs,lr) < 1.e-4 ) il2(mgs) = 1
      end if
      
      IF ( ipconc < 3 .and. temg(mgs) < tfr .and. qx(mgs,lr).gt.qxmin(lr) .and. qx(mgs,lr) < 1.e-4 ) THEN
        il3(mgs) = 1
      ENDIF


      if ( temcg(mgs) < 0.0 ) then
      IF ( ipconc .lt. 4 .or. temcg(mgs) < -25. ) THEN
        ess(mgs) = 0.0


      ELSE
        IF ( temcg(mgs) > -25. .and. temcg(mgs) < -20. ) THEN
        ess(mgs) = ess0*Exp(ess1*(-20.) )*(temcg(mgs) + 25.)/5.
        ELSEIF ( temcg(mgs) >= -20.0 ) THEN
        ess(mgs) = ess0*Exp(ess1*Min( temcg(mgs), 0.0 ) )
        ENDIF
      ENDIF
      end if

      if ( qx(mgs,ls).gt.qxmin(ls) .and. qx(mgs,li).gt.qxmin(li) ) then

      IF ( ipconc < 1 .and. lwsm6 ) THEN
        esi(mgs) = exp(0.7*min(temcg(mgs),0.0))
      ELSE
        esi(mgs)=0.1*exp(0.1*min(temcg(mgs),0.0))
        esi(mgs)=min(0.1,esi(mgs))
      ENDIF
      IF ( ipconc .le. 3 ) THEN
       esi(mgs) =  exp(0.025*min(temcg(mgs),0.0)) 


      ENDIF



      if ( temg(mgs) .gt. 273.15 ) esi(mgs) = 0.0
      end if







       xmascw(mgs) = xmas(mgs,lc)
      if ( qx(mgs,lh).gt.qxmin(lh) .and. qx(mgs,lc).gt.qxmin(lc) ) then
       ehw(mgs) = 1.0
       IF ( iehw .eq. 0 ) THEN
       ehw(mgs) = ehw0  
       ELSEIF ( iehw .eq. 1 .or. iehw .eq. 10 ) THEN
      cwrad = 0.5*xdia(mgs,lc,1)
      ehw(mgs) = Min( ehw0,    &
     &  ewfac*min((aradcw + cwrad*(bradcw + cwrad*   &
     &  (cradcw + cwrad*(dradcw)))), 1.0) )
      
       ELSEIF ( iehw .eq. 2 .or. iehw .eq. 10 ) THEN
       ic = icwr(mgs)
       icp1 = Min( 8, ic+1 )
       ir = igwr(mgs)
       irp1 = Min( 6, ir+1 )
       cwrad = 0.5*xdia(mgs,lc,1)
       rwrad = 0.5*xdia(mgs,lh,3)  
       
       slope1 = (ew(icp1, ir  ) - ew(ic,ir  ))*cwr(ic,2)
       slope2 = (ew(icp1, irp1) - ew(ic,irp1))*cwr(ic,2)
 


       x1 = ew(ic,  ir) + slope1*Max(0.0, (cwrad - cwr(ic,1)) )
       x2 = ew(icp1,ir) + slope2*Max(0.0, (cwrad - cwr(ic,1)) )
       
       slope1 = (x2 - x1)*grad(ir,2)
       
       tmp = Max( 0.0, Min( 1.0, x1 + slope1*Max(0.0, (rwrad - grad(ir,1)) ) ) )
       ehw(mgs) = Min( ehw(mgs), tmp )









       ELSEIF ( iehw .eq. 3 .or. iehw .eq. 10 ) THEN 
         tmp = Exp(- (dmincw/xdia(mgs,lc,1))**3)
         xmascw(mgs) = xmas(mgs,lc) + xdn0(lc)*(pi*dmincw**3/6.0) 
         ehw(mgs) = Min( ehw(mgs), tmp )
       ELSEIF ( iehw .eq. 4 .or. iehw .eq. 10 ) THEN 
         tmp =  &
     &   2.0*xdn(mgs,lc)*vtxbar(mgs,lh,1)*(0.5*xdia(mgs,lc,1))**2 &
     &  /(9.0*fadvisc(mgs)*0.5*xdia(mgs,lh,3))
         tmp = Max( 1.5, Min(10.0, tmp) )
         ehw(mgs) = Min( ehw(mgs), 0.55*Log10(2.51*tmp) )
       ENDIF
      if ( xdia(mgs,lc,1) .lt. 2.4e-06 ) ehw(mgs)=0.0

       ehw(mgs) = Min( ehw0, ehw(mgs) )
       
       IF ( ibfc == -1 .and. temcg(mgs) < -41.0 ) THEN
        ehw(mgs) = 0.0
       ENDIF 

      end if

      if ( qx(mgs,lh).gt.qxmin(lh) .and. qx(mgs,lr).gt.qxmin(lr)    &
     &     .and. temg(mgs) .lt. tfr    &
     &                               ) then

      ehr(mgs) = 1.0
      end if

      IF ( qx(mgs,ls).gt.qxmin(ls) ) THEN
        IF ( ipconc .ge. 4 ) THEN
        ehscnv(mgs) = ehs0*exp(ehs1*min(temcg(mgs),0.0))
        ELSE
        ehscnv(mgs) = exp(0.09*min(temcg(mgs),0.0))
        ENDIF
        if ( qx(mgs,lh).gt.qxmin(lh)  ) then
          ehs(mgs) = ehscnv(mgs)
        end if
      ENDIF

      if ( qx(mgs,lh).gt.qxmin(lh) .and. qx(mgs,li).gt.qxmin(li) ) then
      ehi(mgs)=eii0*exp(eii1*min(temcg(mgs),0.0))
      ehi(mgs) = Min( ehimax, Max( ehi(mgs), ehimin ) )
      if ( temg(mgs) .gt. 273.15 ) ehi(mgs) = 0.0
      end if







      IF ( lhl .gt. 1 ) THEN

      if ( qx(mgs,lhl).gt.qxmin(lhl) .and. qx(mgs,lc).gt.qxmin(lc) ) then
       IF ( iehw == 3 ) iehlw = 3
       IF ( iehw == 4 ) iehlw = 4
       ehlw(mgs) = ehlw0
       IF ( iehlw .eq. 0 ) THEN
       ehlw(mgs) = ehlw0  
       ELSEIF ( iehlw .eq. 1 .or. iehlw .eq. 10 ) THEN
      cwrad = 0.5*xdia(mgs,lc,1)
      ehlw(mgs) = Min( ehlw0,    &
     &  ewfac*min((aradcw + cwrad*(bradcw + cwrad*   &
     &  (cradcw + cwrad*(dradcw)))), 1.0) )
      
       ELSEIF ( iehlw .eq. 2 .or. iehlw .eq. 10 ) THEN
       ic = icwr(mgs)
       icp1 = Min( 8, ic+1 )
       ir = ihlr(mgs)
       irp1 = Min( 6, ir+1 )
       cwrad = 0.5*xdia(mgs,lc,1)
       rwrad = 0.5*xdia(mgs,lhl,3)  
       
       slope1 = (ew(icp1, ir  ) - ew(ic,ir  ))*cwr(ic,2)
       slope2 = (ew(icp1, irp1) - ew(ic,irp1))*cwr(ic,2)
       
       x1 = ew(ic,  ir) + slope1*(cwrad - cwr(ic,1))
       x2 = ew(icp1,ir) + slope2*(cwrad - cwr(ic,1))
       
       slope1 = (x2 - x1)*grad(ir,2)
       
       tmp = Max( 0.0, Min( 1.0, x1 + slope1*(rwrad - grad(ir,1)) ) )
         ehlw(mgs) = Min( ehlw(mgs), tmp )
       ehlw(mgs) = Min( ehlw0, ehlw(mgs) )





       ELSEIF ( iehlw .eq. 3 .or. iehlw .eq. 10 ) THEN 
         tmp = Exp(- (dmincw/xdia(mgs,lc,1))**3)
         ehlw(mgs) = Min( ehlw(mgs), tmp )
       ELSEIF ( iehlw .eq. 4 .or. iehlw .eq. 10 ) THEN 
         tmp =  &
     &   2.0*xdn(mgs,lc)*vtxbar(mgs,lhl,1)*(0.5*xdia(mgs,lc,1))**2 &
     &  /(9.0*fadvisc(mgs)*0.5*xdia(mgs,lhl,3))
         tmp = Max( 1.5, Min(10.0, tmp) )
         ehlw(mgs) = Min( ehlw(mgs), 0.55*Log10(2.51*tmp) )
       ENDIF
      if ( xdia(mgs,lc,1) .lt. 2.4e-06 ) ehlw(mgs)=0.0
       ehlw(mgs) = Min( ehlw0, ehlw(mgs) )

       IF ( ibfc == -1 .and. temcg(mgs) < -41.0 ) THEN 
        ehlw(mgs) = 0.0
       ENDIF 

      end if

      if ( qx(mgs,lhl).gt.qxmin(lhl) .and. qx(mgs,lr).gt.qxmin(lr)    &
     &     .and. temg(mgs) .lt. tfr    &
     &                               ) then
        ehlr(mgs) = 1.0
      end if

      IF ( qx(mgs,ls).gt.qxmin(ls) ) THEN
        if ( qx(mgs,lhl).gt.qxmin(lhl)  ) then
          ehls(mgs) = ehscnv(mgs)
        end if
      ENDIF

      if ( qx(mgs,lhl).gt.qxmin(lhl) .and. qx(mgs,li).gt.qxmin(li) ) then
      ehli(mgs)=eii0hl*exp(eii1hl*min(temcg(mgs),0.0))
      ehli(mgs) = Min( ehimax, Max( ehli(mgs), ehimin ) )
      if ( temg(mgs) .gt. 273.15 ) ehli(mgs) = 1.0
      end if


      ENDIF 

      ENDDO  







      do mgs = 1,ngscnt

      xplate(mgs) = 0.0
      xcolmn(mgs) = 1.0





















      end do






      if (ndebug .gt. 0 ) write(0,*) 'Collection: rain collects xxxxx'

      do mgs = 1,ngscnt
      qracw(mgs) =  0.0
      IF ( qx(mgs,lr) .gt. qxmin(lr) .and. erw(mgs) .gt. 0.0 ) THEN
      IF ( ipconc .lt. 3 ) THEN
       IF ( erw(mgs) .gt. 0.0 .and. qx(mgs,lr) .gt. 1.e-7 ) THEN
       vt = (ar*(xdia(mgs,lc,1)**br))*rhovt(mgs)
       qracw(mgs) =    &
     &   (0.25)*pi*erw(mgs)*qx(mgs,lc)*cx(mgs,lr) &

     &  *Max(0.0, vtxbar(mgs,lr,1)-vt)   &
     &  *(  gf3*xdia(mgs,lr,2)    &
     &    + 2.0*gf2*xdia(mgs,lr,1)*xdia(mgs,lc,1)    &
     &    + gf1*xdia(mgs,lc,2) )





       ENDIF
      ELSE

      IF ( dmrauto <= 0 .or.  rho0(mgs)*qx(mgs,lr) > 1.2*xl2p(mgs) ) THEN 
       rwrad = 0.5*xdia(mgs,lr,3)
        IF ( rwrad .gt. rh(mgs) ) THEN 
         IF ( rwrad .gt. rwradmn ) THEN


           qracw(mgs) = erw(mgs)*aa2*cx(mgs,lr)*cx(mgs,lc)*xmas(mgs,lc)*   &
     &        ((cnu + 2.)*xv(mgs,lc)/(cnu + 1.) + xv(mgs,lr))/rho0(mgs) 
         ELSE

          IF ( imurain == 3 ) THEN








           qracw(mgs) = aa1*cx(mgs,lr)*qx(mgs,lc)*   &
     &        ((cnu + 3.)*(cnu + 2.)*xv(mgs,lc)**2/(cnu + 1.)**2 +    &
     &         (alpha(mgs,lr) + 2.)*xv(mgs,lr)**2/(alpha(mgs,lr) + 1.)) 
           
           ELSE 

           qracw(mgs) = aa1*cx(mgs,lr)*qx(mgs,lc)*   &
     &        ((cnu + 3.)*(cnu + 2.)*xv(mgs,lc)**2/(cnu + 1.)**2 +    &
     &         (alpha(mgs,lr) + 6.)*(alpha(mgs,lr) + 5.)*(alpha(mgs,lr) + 4.)*xv(mgs,lr)**2/ &
     &          ((alpha(mgs,lr) + 3.)*(alpha(mgs,lr) + 2.)*(alpha(mgs,lr) + 1.))) 
           
           ENDIF
           
         ENDIF
        ENDIF
        ENDIF
       ENDIF

       qracw(mgs) = Min(qracw(mgs), qcmxd(mgs))
       ENDIF
      end do

      do mgs = 1,ngscnt
      qraci(mgs) = 0.0
      craci(mgs) = 0.0
      IF ( eri(mgs) .gt. 0.0 .and. iacr .ge. 1 .and. xdia(mgs,lr,3) .gt. 2.*rwradmn ) THEN
        IF ( ipconc .ge. 3 ) THEN

           tmp = eri(mgs)*aa2*cx(mgs,lr)*cx(mgs,li)*   &
     &        ((cinu + 2.)*xv(mgs,li)/(cinu + 1.) + xv(mgs,lr))

        qraci(mgs) = Min( qxmxd(mgs,li), tmp*xmas(mgs,li)*rhoinv(mgs) )
        craci(mgs) = Min( cxmxd(mgs,li), tmp )





















        ELSE
          qraci(mgs) =    &
     &     min(   &
     &     (0.25)*pi*eri(mgs)*qx(mgs,li)*cx(mgs,lr)   &
     &    *abs(vtxbar(mgs,lr,1)-vtxbar(mgs,li,1))   &
     &    *(  gf3*xdia(mgs,lr,2)    &
     &      + 2.0*gf2*xdia(mgs,lr,1)*xdia(mgs,li,1)    &
     &      + gf1*xdia(mgs,li,2) )     &
     &    , qimxd(mgs))
        ENDIF
      if ( temg(mgs) .gt. 268.15 ) then
      qraci(mgs) = 0.0
      end if
      ENDIF
      end do

      do mgs = 1,ngscnt
      qracs(mgs) =  0.0
      IF ( ers(mgs) .gt. 0.0 .and. ipconc < 3 ) THEN
       IF ( lwsm6 .and. ipconc == 0 ) THEN
         vt = vt2ave(mgs)
       ELSE
         vt = vtxbar(mgs,ls,1)
       ENDIF
      qracs(mgs) =      &
     &   min(     &
     &   ((0.25)*pi/gf4)*ers(mgs)*qx(mgs,ls)*cx(mgs,lr)     &
     &  *abs(vtxbar(mgs,lr,1)-vt)     &
     &  *(  gf6*gf1*xdia(mgs,ls,2)     &
     &    + 2.0*gf5*gf2*xdia(mgs,ls,1)*xdia(mgs,lr,1)      &
     &    + gf4*gf3*xdia(mgs,lr,2) )      &
     &  , qsmxd(mgs))
      ENDIF
      end do



      if (ndebug .gt. 0 ) write(0,*) 'Collection: snow collects xxxxx'

      do mgs = 1,ngscnt
      qsacw(mgs) =  0.0
      csacw(mgs) =  0.0
      vsacw(mgs) =  0.0
      IF ( esw(mgs) .gt. 0.0 ) THEN

       IF ( ipconc .ge. 4 ) THEN





        tmp = 1.0*rvt*aa2*cx(mgs,ls)*cx(mgs,lc)*   &
     &        ((cnu + 2.)*xv(mgs,lc)/(cnu + 1.) + xv(mgs,ls))

        qsacw(mgs) = Min( qxmxd(mgs,lc), tmp*xmas(mgs,lc)*rhoinv(mgs) )
        csacw(mgs) = Min( cxmxd(mgs,lc), tmp )

          IF ( lvol(ls) .gt. 1 ) THEN
             IF ( temg(mgs) .lt. 273.15) THEN
             rimdn(mgs,ls) = rimc1*(-((0.5)*(1.e+06)*xdia(mgs,lc,1))   &
     &                *((0.60)*vtxbar(mgs,ls,1))   &
     &                /(temg(mgs)-273.15))**(rimc2)
             rimdn(mgs,ls) = Min( Max( rimc3, rimdn(mgs,ls) ), 900.0 )
             ELSE
             rimdn(mgs,ls) = 1000.
             ENDIF

           vsacw(mgs) = rho0(mgs)*qsacw(mgs)/rimdn(mgs,ls)

          ENDIF




       ELSE









            vt = abs(vtxbar(mgs,ls,1)-vtxbar(mgs,lc,1))

          qsacw(mgs) = 0.25*pi*esw(mgs)*cx(mgs,ls)*qx(mgs,lc)*vt*   &
     &         (  da0(ls)*xdia(mgs,ls,3)**2 +     &
     &            dab1(ls,lc)*xdia(mgs,ls,3)*xdia(mgs,lc,3) +    &
     &            da1(lc)*xdia(mgs,lc,3)**2 )
        qsacw(mgs) = Min( qsacw(mgs), qxmxd(mgs,ls) )
        csacw(mgs) = rho0(mgs)*qsacw(mgs)/xmas(mgs,lc)
       ENDIF
      ENDIF
      end do


      do mgs = 1,ngscnt
      qsaci(mgs) = 0.0
      csaci(mgs) = 0.0
      IF ( ipconc .ge. 4 ) THEN
      IF ( esi(mgs) .gt. 0.0 ) THEN



        tmp = esi(mgs)*rvt*aa2*cx(mgs,ls)*cx(mgs,li)*   &
     &        ((cinu + 2.)*xv(mgs,li)/(cinu + 1.) + xv(mgs,ls))

        qsaci(mgs) = Min( qxmxd(mgs,li), tmp*xmas(mgs,li)*rhoinv(mgs) )
        csaci(mgs) = Min( cxmxd(mgs,li), tmp )









      ENDIF
      ELSE 
      IF ( esi(mgs) .gt. 0.0 ) THEN
         qsaci(mgs) =    &
     &   min(   &
     &   ((0.25)*pi)*esi(mgs)*qx(mgs,li)*cx(mgs,ls)   &
     &  *abs(vtxbar(mgs,ls,1)-vtxbar(mgs,li,1))   &
     &  *(  gf3*xdia(mgs,ls,2)    &
     &    + 2.0*gf2*xdia(mgs,ls,1)*xdia(mgs,li,1)    &
     &    + gf1*xdia(mgs,li,2) )     &
     &  , qimxd(mgs))
      ENDIF
      ENDIF
      end do



      do mgs = 1,ngscnt
      qsacr(mgs) = 0.0
      qsacrs(mgs) = 0.0
      csacr(mgs) = 0.0
      IF ( esr(mgs) .gt. 0.0 ) THEN
      IF ( ipconc .ge. 3 ) THEN










      ELSE
       IF ( lwsm6 .and. ipconc == 0 ) THEN
         vt = vt2ave(mgs)
       ELSE
         vt = vtxbar(mgs,ls,1)
       ENDIF
       
       qsacr(mgs) =   &
     &   min(   &
     &   ((0.25)*pi/gf4)*esr(mgs)*qx(mgs,lr)*cx(mgs,ls)   &
     &  *abs(vtxbar(mgs,lr,1)-vt)   &
     &  *(  gf6*gf1*xdia(mgs,lr,2)   &
     &    + 2.0*gf5*gf2*xdia(mgs,lr,1)*xdia(mgs,ls,1)    &
     &    + gf4*gf3*xdia(mgs,ls,2) )    &
     &  , qrmxd(mgs))
      ENDIF
      ENDIF
      end do



      if (ndebug .gt. 0 ) write(0,*) 'Collection: graupel collects xxxxx'

      do mgs = 1,ngscnt
      qhacw(mgs) = 0.0
      rarx(mgs,lh) = 0.0
      vhacw(mgs) = 0.0
      vhsoak(mgs) = 0.0
      zhacw(mgs) = 0.0
      
      IF ( .false. ) THEN
        vtmax = (gz(igs(mgs),jgs,kgs(mgs))/dtp)
        vtxbar(mgs,lh,1) = Min( vtmax, vtxbar(mgs,lh,1))
        vtxbar(mgs,lh,2) = Min( vtmax, vtxbar(mgs,lh,2))
        vtxbar(mgs,lh,3) = Min( vtmax, vtxbar(mgs,lh,3))
      ENDIF
      IF ( ehw(mgs) .gt. 0.0 ) THEN

        IF ( ipconc .ge. 2 ) THEN

        IF ( .false. ) THEN  
        qhacw(mgs) = (ehw(mgs)*qx(mgs,lc)*cx(mgs,lh)*pi*   &
     &    abs(vtxbar(mgs,lh,1)-vtxbar(mgs,lc,1))*   &
     &    (2.0*xdia(mgs,lh,1)*(xdia(mgs,lh,1) +    &
     &         xdia(mgs,lc,1)*gf73rds) +    &
     &      xdia(mgs,lc,2)*gf83rds))/4.     
     
         ELSE  
            vt = abs(vtxbar(mgs,lh,1)-vtxbar(mgs,lc,1)) 

          qhacw(mgs) = 0.25*pi*ehw(mgs)*cx(mgs,lh)*qx(mgs,lc)*vt*   &
     &         (  da0lh(mgs)*xdia(mgs,lh,3)**2 +     &
     &            dab1lh(mgs,lc,lh)*xdia(mgs,lh,3)*xdia(mgs,lc,3) +    &
     &            da1(lc)*xdia(mgs,lc,3)**2 ) 
         
         ENDIF
          qhacw(mgs) = Min( qhacw(mgs), 0.5*qx(mgs,lc)/dtp )
        
         IF ( lzh .gt. 1 ) THEN
          tmp = qx(mgs,lh)/cx(mgs,lh)
          






         ENDIF
        
        ELSE
         qhacw(mgs) =    &
     &   min(   &
     &   ((0.25)*pi)*ehw(mgs)*qx(mgs,lc)*cx(mgs,lh)   &
     &  *abs(vtxbar(mgs,lh,1)-vtxbar(mgs,lc,1))   &
     &  *(  gf3*xdia(mgs,lh,2)    &
     &    + 2.0*gf2*xdia(mgs,lh,1)*xdia(mgs,lc,1)    &
     &    + gf1*xdia(mgs,lc,2) )     &
     &    , 0.5*qx(mgs,lc)/dtp)


       
       
         IF ( lwsm6 .and. qsacw(mgs) > 0.0 .and.  qhacw(mgs) > 0.0) THEN
           qaacw = ( qx(mgs,ls)*qsacw(mgs) + qx(mgs,lh)*qhacw(mgs) )/(qx(mgs,ls) + qx(mgs,lh))

           qsacw(mgs) = qaacw
           qhacw(mgs) = qaacw
         ENDIF
         
       ENDIF

          IF ( lvol(lh) .gt. 1 .or. lhl .gt. 1 ) THEN 
             
             IF ( temg(mgs) .lt. 273.15) THEN
             rimdn(mgs,lh) = rimc1*(-((0.5)*(1.e+06)*xdia(mgs,lc,1))   &
     &                *((0.60)*vtxbar(mgs,lh,1))   &
     &                /(temg(mgs)-273.15))**(rimc2)
             rimdn(mgs,lh) = Min( Max( rimc3, rimdn(mgs,lh) ), 900.0 )
             ELSE
             rimdn(mgs,lh) = 1000.
             ENDIF
             
             IF ( lvol(lh) > 1 ) vhacw(mgs) = rho0(mgs)*qhacw(mgs)/rimdn(mgs,lh)

          ENDIF
      
        IF ( qx(mgs,lh) .gt. qxmin(lh) .and. ipelec .gt. 0 ) THEN
         rarx(mgs,lh) =     &
     &    qhacw(mgs)*1.0e3*rho0(mgs)/((pi/2.0)*xdia(mgs,lh,2)*cx(mgs,lh))
        ENDIF
      
      ENDIF  
      end do   


      do mgs = 1,ngscnt
      qhaci(mgs) = 0.0
      IF ( ehi(mgs) .gt. 0.0 ) THEN
       IF (  ipconc .ge. 5 ) THEN

       vt = Sqrt((vtxbar(mgs,lh,1)-vtxbar(mgs,li,1))**2 +    &
     &            0.04*vtxbar(mgs,lh,1)*vtxbar(mgs,li,1) )

          qhaci(mgs) = 0.25*pi*ehi(mgs)*cx(mgs,lh)*qx(mgs,li)*vt*   &
     &         (  da0lh(mgs)*xdia(mgs,lh,3)**2 +     &
     &            dab1lh(mgs,li,lh)*xdia(mgs,lh,3)*xdia(mgs,li,3) +    &
     &            da1(li)*xdia(mgs,li,3)**2 ) 
          qhaci(mgs) = Min( qhaci(mgs), qimxd(mgs) )
       ELSE
        qhaci(mgs) =    &
     &  min(   &
     &  ((0.25)*pi)*ehi(mgs)*qx(mgs,li)*cx(mgs,lh)   &
     &  *abs(vtxbar(mgs,lh,1)-vtxbar(mgs,li,1))   &
     &  *(  gf3*xdia(mgs,lh,2)    &
     &    + 2.0*gf2*xdia(mgs,lh,1)*xdia(mgs,li,1)    &
     &    + gf1*xdia(mgs,li,2) )     &
     &  , qimxd(mgs))
       ENDIF
      ENDIF
      end do   


      do mgs = 1,ngscnt
      qhacs(mgs) = 0.0
      IF ( ehs(mgs) .gt. 0.0 ) THEN
       IF ( ipconc .ge. 5 ) THEN

       vt = Sqrt((vtxbar(mgs,lh,1)-vtxbar(mgs,ls,1))**2 +    &
     &            0.04*vtxbar(mgs,lh,1)*vtxbar(mgs,ls,1) )

          qhacs(mgs) = 0.25*pi*ehs(mgs)*cx(mgs,lh)*qx(mgs,ls)*vt*   &
     &         (  da0lh(mgs)*xdia(mgs,lh,3)**2 +     &
     &            dab1lh(mgs,ls,lh)*xdia(mgs,lh,3)*xdia(mgs,ls,3) +    &
     &            da1(ls)*xdia(mgs,ls,3)**2 ) 
      
          qhacs(mgs) = Min( qhacs(mgs), qsmxd(mgs) )

       ELSE
         qhacs(mgs) =   &
     &   min(   &
     &   ((0.25)*pi/gf4)*ehs(mgs)*qx(mgs,ls)*cx(mgs,lh)   &
     &  *abs(vtxbar(mgs,lh,1)-vtxbar(mgs,ls,1))   &
     &  *(  gf6*gf1*xdia(mgs,ls,2)   &
     &    + 2.0*gf5*gf2*xdia(mgs,ls,1)*xdia(mgs,lh,1)   &
     &    + gf4*gf3*xdia(mgs,lh,2) )   &
     &  , qsmxd(mgs))
        ENDIF
      ENDIF
      end do   

      do mgs = 1,ngscnt
      qhacr(mgs) = 0.0
      vhacr(mgs) = 0.0
      chacr(mgs) = 0.0
      zhacr(mgs) = 0.0
      IF ( temg(mgs) .gt. tfr ) raindn(mgs,lh) = 1000.0

      IF ( ehr(mgs) .gt. 0.0 ) THEN
      IF ( ipconc .ge. 3 ) THEN
       vt = Sqrt((vtxbar(mgs,lh,1)-vtxbar(mgs,lr,1))**2 +    &
     &            0.04*vtxbar(mgs,lh,1)*vtxbar(mgs,lr,1) )





     
       qhacr(mgs) = 0.25*pi*ehr(mgs)*cx(mgs,lh)*qx(mgs,lr)*vt*   &
     &         (  da0lh(mgs)*xdia(mgs,lh,3)**2 +     &
     &            dab1lh(mgs,lr,lh)*xdia(mgs,lh,3)*xdia(mgs,lr,3) +    &
     &            da1(lr)*xdia(mgs,lr,3)**2 )





        qhacr(mgs) = Min( qhacr(mgs), qxmxd(mgs,lr) )















        chacr(mgs) = qhacr(mgs)*cx(mgs,lr)/qx(mgs,lr)
        chacr(mgs) = min(chacr(mgs),crmxd(mgs))

      IF ( lzh .gt. 1 ) THEN
          tmp = qx(mgs,lh)/cx(mgs,lh)








      ENDIF
      
      ELSE
       IF ( lwsm6 .and. ipconc == 0 ) THEN
         vt = vt2ave(mgs)
       ELSE
         vt = vtxbar(mgs,lh,1)
       ENDIF

      qhacr(mgs) =   &
     &   min(   &
     &   ((0.25)*pi/gf4)*ehr(mgs)*qx(mgs,lr)*cx(mgs,lh)   &
     &  *abs(vt-vtxbar(mgs,lr,1))   &
     &  *(  gf6*gf1*xdia(mgs,lr,2)   &
     &    + 2.0*gf5*gf2*xdia(mgs,lr,1)*xdia(mgs,lh,1)   &
     &    + gf4*gf3*xdia(mgs,lh,2) )   &
     &  , qrmxd(mgs))
      ENDIF
        IF ( lvol(lh) .gt. 1 ) THEN
         vhacr(mgs) = rho0(mgs)*qhacr(mgs)/raindn(mgs,lh)
        ENDIF
      ENDIF
      end do



      if (ndebug .gt. 0 ) write(0,*) 'Collection: hail collects xxxxx'


      do mgs = 1,ngscnt
      qhlacw(mgs) = 0.0
      vhlacw(mgs) = 0.0
      vhlsoak(mgs) = 0.0
      IF ( lhl > 1 .and. .true.) THEN
        vtmax = (gz(igs(mgs),jgs,kgs(mgs))/dtp)
        vtxbar(mgs,lhl,1) = Min( vtmax, vtxbar(mgs,lhl,1))
        vtxbar(mgs,lhl,2) = Min( vtmax, vtxbar(mgs,lhl,2))
        vtxbar(mgs,lhl,3) = Min( vtmax, vtxbar(mgs,lhl,3))
      ENDIF

      IF ( lhl > 0 ) THEN
      rarx(mgs,lhl) = 0.0
      ENDIF

      IF ( lhl .gt. 1 .and. ehlw(mgs) .gt. 0.0 ) THEN




            vt = abs(vtxbar(mgs,lhl,1)-vtxbar(mgs,lc,1))

          qhlacw(mgs) = 0.25*pi*ehlw(mgs)*cx(mgs,lhl)*qx(mgs,lc)*vt*   &
     &         (  da0lhl(mgs)*xdia(mgs,lhl,3)**2 +     &
     &            dab1lh(mgs,lc,lhl)*xdia(mgs,lhl,3)*xdia(mgs,lc,3) +    &
     &            da1(lc)*xdia(mgs,lc,3)**2 )


          qhlacw(mgs) = Min( qhlacw(mgs), 0.5*qx(mgs,lc)/dtp )

          IF ( lvol(lhl) .gt. 1 ) THEN

             IF ( temg(mgs) .lt. 273.15) THEN
             rimdn(mgs,lhl) = rimc1*(-((0.5)*(1.e+06)*xdia(mgs,lc,1))   &
     &                *((0.60)*vtxbar(mgs,lhl,1))   &
     &                /(temg(mgs)-273.15))**(rimc2)
             rimdn(mgs,lhl) = Min( Max( rimc3, rimdn(mgs,lhl) ), 900.0 )
             ELSE
             rimdn(mgs,lhl) = 1000.
             ENDIF

             vhlacw(mgs) = rho0(mgs)*qhlacw(mgs)/rimdn(mgs,lhl)

          ENDIF


        IF ( qx(mgs,lhl) .gt. qxmin(lhl) .and. ipelec .gt. 0 ) THEN
         rarx(mgs,lhl) =     &
     &    qhlacw(mgs)*1.0e3*rho0(mgs)/((pi/2.0)*xdia(mgs,lhl,2)*cx(mgs,lhl))
        ENDIF

      ENDIF
      end do

      qhlaci(:) = 0.0
      IF ( lhl .gt. 1  ) THEN
      do mgs = 1,ngscnt
      IF ( ehli(mgs) .gt. 0.0 ) THEN
       IF (  ipconc .ge. 5 ) THEN

       vt = Sqrt((vtxbar(mgs,lhl,1)-vtxbar(mgs,li,1))**2 +    &
     &            0.04*vtxbar(mgs,lhl,1)*vtxbar(mgs,li,1) )

          qhlaci(mgs) = 0.25*pi*ehli(mgs)*cx(mgs,lhl)*qx(mgs,li)*vt*   &
     &         (  da0lhl(mgs)*xdia(mgs,lhl,3)**2 +     &
     &            dab1lh(mgs,li,lhl)*xdia(mgs,lhl,3)*xdia(mgs,li,3) +    &
     &            da1(li)*xdia(mgs,li,3)**2 )
          qhlaci(mgs) = Min( qhlaci(mgs), qimxd(mgs) )
       ENDIF
      ENDIF
      end do
      ENDIF

      qhlacs(:) = 0.0
      IF ( lhl .gt. 1 ) THEN
      do mgs = 1,ngscnt
      IF ( ehls(mgs) .gt. 0.0) THEN
       IF ( ipconc .ge. 5 ) THEN

       vt = Sqrt((vtxbar(mgs,lhl,1)-vtxbar(mgs,ls,1))**2 +    &
     &            0.04*vtxbar(mgs,lhl,1)*vtxbar(mgs,ls,1) )

          qhlacs(mgs) = 0.25*pi*ehli(mgs)*cx(mgs,lhl)*qx(mgs,ls)*vt*   &
     &         (  da0lhl(mgs)*xdia(mgs,lhl,3)**2 +     &
     &            dab1lh(mgs,ls,lhl)*xdia(mgs,lhl,3)*xdia(mgs,ls,3) +    &
     &            da1(ls)*xdia(mgs,ls,3)**2 )

          qhlacs(mgs) = Min( qhlacs(mgs), qsmxd(mgs) )

        ENDIF
      ENDIF
      end do
      ENDIF


      do mgs = 1,ngscnt
      qhlacr(mgs) = 0.0
      chlacr(mgs) = 0.0
      vhlacr(mgs) = 0.0
      IF ( lhl .gt. 1 .and. temg(mgs) .gt. tfr ) raindn(mgs,lhl) = 1000.0

      IF ( lhl .gt. 1 .and. ehlr(mgs) .gt. 0.0 ) THEN
      IF ( ipconc .ge. 3 ) THEN
       vt = Sqrt((vtxbar(mgs,lhl,1)-vtxbar(mgs,lr,1))**2 +    &
     &            0.04*vtxbar(mgs,lhl,1)*vtxbar(mgs,lr,1) )

       qhlacr(mgs) = 0.25*pi*ehlr(mgs)*cx(mgs,lhl)*qx(mgs,lr)*vt*   &
     &         (  da0lhl(mgs)*xdia(mgs,lhl,3)**2 +     &
     &            dab1lh(mgs,lr,lhl)*xdia(mgs,lhl,3)*xdia(mgs,lr,3) +    &
     &            da1(lr)*xdia(mgs,lr,3)**2 )





        qhlacr(mgs) = Min( qhlacr(mgs), qxmxd(mgs,lr) )


        chlacr(mgs) = 0.25*pi*ehlr(mgs)*cx(mgs,lhl)*cx(mgs,lr)*vt*   &
     &         (  da0lhl(mgs)*xdia(mgs,lhl,3)**2 +     &
     &            dab0(lhl,lr)*xdia(mgs,lhl,3)*xdia(mgs,lr,3) +    &
     &            da0(lr)*xdia(mgs,lr,3)**2 )

        chlacr(mgs) = min(chlacr(mgs),crmxd(mgs))

        IF ( lvol(lhl) .gt. 1 ) THEN
         vhlacr(mgs) = rho0(mgs)*qhlacr(mgs)/raindn(mgs,lhl)
        ENDIF
      ENDIF
      ENDIF
      end do







      if (ndebug .gt. 0 ) write(0,*) 'Collection: Cloud collects xxxxx'

      if (ndebug .gt. 0 ) write(0,*) 'Collection: cloud ice collects xxxx2'

      do mgs = 1,ngscnt
      qiacw(mgs) = 0.0
      IF ( eiw(mgs) .gt. 0.0 ) THEN

       vt = Sqrt((vtxbar(mgs,li,1)-vtxbar(mgs,lc,1))**2 +    &
     &            0.04*vtxbar(mgs,li,1)*vtxbar(mgs,lc,1) )

          qiacw(mgs) = 0.25*pi*eiw(mgs)*cx(mgs,li)*qx(mgs,lc)*vt*   &
     &         (  da0(li)*xdia(mgs,li,3)**2 +     &
     &            dab1(li,lc)*xdia(mgs,li,3)*xdia(mgs,lc,3) +    &
     &            da1(lc)*xdia(mgs,lc,3)**2 )

       qiacw(mgs) = Min( qiacw(mgs), qxmxd(mgs,lc) )
      ENDIF
      end do


      if (ndebug .gt. 0 ) write(0,*) 'Collection: cloud ice collects xxxx8'

      do mgs = 1,ngscnt
      qiacr(mgs) = 0.0
      qiacrf(mgs) = 0.0
      qiacrs(mgs) = 0.0
      ciacr(mgs) = 0.0
      ciacrf(mgs) = 0.0
      viacrf(mgs) = 0.0
      csplinter(mgs) = 0.0
      qsplinter(mgs) = 0.0
      csplinter2(mgs) = 0.0
      qsplinter2(mgs) = 0.0
      IF ( iacr .ge. 1 .and. eri(mgs) .gt. 0.0    &
     &     .and. temg(mgs) .le. 270.15 ) THEN
      IF ( ipconc .ge. 3 ) THEN
       ni = 0.0
         IF ( xdia(mgs,li,1) .ge. 10.e-6 ) THEN
          ni = ni + cx(mgs,li)*Exp(- (40.e-6/xdia(mgs,li,1))**3 )
         ENDIF
       IF ( imurain == 1 ) THEN 
           IF ( iacrsize .eq. 1 ) THEN
             ratio = 500.e-6/xdia(mgs,lr,1)
           ELSEIF ( iacrsize .eq. 2 ) THEN
             ratio = 300.e-6/xdia(mgs,lr,1)
           ELSEIF ( iacrsize .eq. 3 ) THEN
             ratio = 40.e-6/xdia(mgs,lr,1)
           ENDIF
           
           i = Int(Min(25.0,ratio))
           j = Int(Max(0.0,Min(15.,alpha(mgs,lr))))
           delx = ratio - float(i)
           dely = alpha(mgs,lr) - float(j)
           ip1 = Min( i+1, nqiacrratio )
           jp1 = Min( j+1, nqiacralpha )

           
           tmp1 = ciacrratio(i,j) + delx*(ciacrratio(ip1,j) - ciacrratio(i,j))
           tmp2 = ciacrratio(i,jp1) + delx*(ciacrratio(ip1,jp1) - ciacrratio(i,jp1))
           
           
           
           nr = (tmp1 + dely*(tmp2 - tmp1))*cx(mgs,lr)
           
           
           tmp1 = qiacrratio(i,j) + delx*(qiacrratio(ip1,j) - qiacrratio(i,j))
           tmp2 = qiacrratio(i,jp1) + delx*(qiacrratio(ip1,jp1) - qiacrratio(i,jp1))
           
           
           
           qr = (tmp1 + dely*(tmp2 - tmp1))*qx(mgs,lr)

          vt = Sqrt((vtxbar(mgs,lr,1)-vtxbar(mgs,li,1))**2 +     &
     &            0.04*vtxbar(mgs,lr,1)*vtxbar(mgs,li,1) )

          qiacr(mgs) = 0.25*pi*eri(mgs)*ni*qr*vt*   &
     &         (  da0(li)*xdia(mgs,li,3)**2 +     &
     &            dab1lh(mgs,lr,li)*xdia(mgs,lh,3)*xdia(mgs,li,3) +    &
     &            da1(lr)*xdia(mgs,lr,3)**2 ) 

          qiacr(mgs) = Min( qrmxd(mgs), qiacr(mgs) )

          ciacr(mgs) = 0.25*pi*eri(mgs)*ni*nr*vt*   &
     &         (  da0(li)*xdia(mgs,li,3)**2 +     &
     &            dab0lh(mgs,lr,li)*xdia(mgs,lr,3)*xdia(mgs,li,3) +    &
     &            da0(lr)*xdia(mgs,lr,3)**2 ) 

          ciacr(mgs) = Min( crmxd(mgs), ciacr(mgs) )
          





       ELSEIF ( imurain == 3 ) THEN 

         arg = 1000.*xdia(mgs,lr,3)


         IF ( ipconc .ge. 3 ) THEN
           IF ( iacrsize .eq. 1 ) THEN
            nr = cx(mgs,lr)*gaml02d500( arg )  
           ELSEIF ( iacrsize .eq. 2 ) THEN
            nr = cx(mgs,lr)*gaml02d300( arg )  
           ELSEIF ( iacrsize .eq. 3 ) THEN
            nr = cx(mgs,lr)*gaml02( arg ) 
           ELSEIF ( iacrsize .eq. 4 ) THEN
            nr = cx(mgs,lr) 
           ENDIF
         ELSE
         nr = cx(mgs,lr)*gaml02( arg )
         ENDIF



       IF ( ni .gt. 0.0 .and. nr .gt. 0.0 ) THEN
       d0 = xdia(mgs,lr,3)
       qiacr(mgs) = xdn(mgs,lr)*rhoinv(mgs)*   &
     &     (0.217239*(0.522295*(d0**5) +    &
     &      49711.81*(d0**6) -    &
     &      1.673016e7*(d0**7)+    &
     &      2.404471e9*(d0**8) -    &
     &      1.22872e11*(d0**9))*ni*nr)
      qiacr(mgs) = Min( qrmxd(mgs), qiacr(mgs) )
      ciacr(mgs) =   &
     &   (0.217239*(0.2301947*(d0**2) +    &
     &      15823.76*(d0**3) -    &
     &      4.167685e6*(d0**4) +    &
     &      4.920215e8*(d0**5) -    &
     &      2.133344e10*(d0**6))*ni*nr)
      ciacr(mgs) = Min( crmxd(mgs), ciacr(mgs) )

      ENDIF
       IF ( iacr .eq. 1 .or. iacr .eq. 3 ) THEN
         ciacrf(mgs) = Min(ciacr(mgs), qiacr(mgs)/(1.0*vr1mm*1000.0)*rho0(mgs) ) 
       ELSEIF ( iacr .eq. 2 ) THEN
         ciacrf(mgs) = ciacr(mgs) 
       ELSEIF ( iacr .eq. 4 ) THEN
         ciacrf(mgs) = Min(ciacr(mgs), qiacr(mgs)/(1.0*vfrz*1000.0)*rho0(mgs) ) 
       ELSEIF ( iacr .eq. 5 ) THEN
         ciacrf(mgs) = ciacr(mgs)*rzxh(mgs)
       ENDIF 

       ENDIF
      
      
      ELSE 
      qiacr(mgs) =    &
     &  min(        &
     &   ((0.25/gf4)*pi)*eri(mgs)*cx(mgs,li)*qx(mgs,lr)   &
     &  *abs(vtxbar(mgs,lr,1)-vtxbar(mgs,li,1))   &
     &  *(  gf6*gf1*xdia(mgs,lr,2)    &
     &    + 2.0*gf5*gf2*xdia(mgs,lr,1)*xdia(mgs,li,1)    &
     &    + gf4*gf3*xdia(mgs,li,2) )     &
     &  , qrmxd(mgs))
      ENDIF




      ENDIF

      IF ( ipconc .ge. 1 ) THEN
        IF ( nsplinter .ge. 0 ) THEN
          csplinter(mgs) = nsplinter*ciacr(mgs)
        ELSE
          csplinter(mgs) = -nsplinter*ciacrf(mgs)
        ENDIF
        qsplinter(mgs) = Min(0.1*qiacr(mgs), csplinter(mgs)*splintermass/rho0(mgs) ) 
      ENDIF
      
      qiacrf(mgs) = qiacr(mgs)





      IF ( lvol(lh) > 1 ) THEN
         viacrf(mgs) = rho0(mgs)*qiacrf(mgs)/rhofrz
      ENDIF
      
      end do






      if ( ipconc .ge. 4 .or. ipelec .ge. 100 ) then 
      do mgs = 1,ngscnt
      csacs(mgs) = 0.0
      IF ( ess(mgs) .gt. 0.0 ) THEN

      csacs(mgs) = rvt*aa2*ess(mgs)*cx(mgs,ls)**2*xv(mgs,ls)
      csacs(mgs) = min(csacs(mgs),csmxd(mgs))
      ENDIF
      end do
      end if


      if (ndebug .gt. 0 ) write(0,*) 'ICEZVD_GS: conc 11'
      if ( ipconc .ge. 2 .or. ipelec .ge. 9 ) then
      do mgs = 1,ngscnt
      ciacw(mgs) = 0.0
      IF ( eiw(mgs) .gt. 0.0 ) THEN

        ciacw(mgs) = qiacw(mgs)*rho0(mgs)/xmas(mgs,lc)
      ciacw(mgs) = min(ciacw(mgs),ccmxd(mgs))
      ENDIF
      end do
      end if

      if (ndebug .gt. 0 ) write(0,*) 'ICEZVD_GS: conc 18'
      if ( ipconc .ge. 2 .or. ipelec .ge. 1 ) then
      do mgs = 1,ngscnt
       cracw(mgs) = 0.0
       cracr(mgs) = 0.0
       ec0(mgs) = 1.e9
      IF ( qx(mgs,lc) .gt. qxmin(lc) .and. qx(mgs,lr) .gt. qxmin(lr)    &
     &      .and. qracw(mgs) .gt. 0.0 ) THEN

       IF ( ipconc .lt. 3 ) THEN
        IF ( erw(mgs) .gt. 0.0 ) THEN
        cracw(mgs) =   &
     &   ((0.25)*pi)*erw(mgs)*cx(mgs,lc)*cx(mgs,lr)   &
     &  *abs(vtxbar(mgs,lr,1)-vtxbar(mgs,lc,1))   &
     &  *(  gf1*xdia(mgs,lc,2)   &
     &    + 2.0*gf2*xdia(mgs,lc,1)*xdia(mgs,lr,1)   &
     &    + gf3*xdia(mgs,lr,2) )
        ENDIF
       ELSE 
        IF ( dmrauto <= 0 .or.  rho0(mgs)*qx(mgs,lr) > 1.2*xl2p(mgs) ) THEN  
        IF ( 0.5*xdia(mgs,lr,3) .gt. rh(mgs) ) THEN 

          IF ( 0.5*xdia(mgs,lr,3) .gt. rwradmn ) THEN 


            cracw(mgs) = aa2*cx(mgs,lr)*cx(mgs,lc)*(xv(mgs,lc) + xv(mgs,lr))
          ELSE
            IF ( imurain == 3 ) THEN

            cracw(mgs) = aa1*cx(mgs,lr)*cx(mgs,lc)*   &
     &          ((cnu + 2.)*xv(mgs,lc)**2/(cnu + 1.) +    &
     &          (alpha(mgs,lr) + 2.)*xv(mgs,lr)**2/(alpha(mgs,lr) + 1.))
            ELSE 
            cracw(mgs) = aa1*cx(mgs,lr)*cx(mgs,lc)*   &
     &          ((cnu + 2.)*xv(mgs,lc)**2/(cnu + 1.) +    &
     &          (alpha(mgs,lr) + 6.)*(alpha(mgs,lr) + 5.)*(alpha(mgs,lr) + 4.)*xv(mgs,lr)**2/  &
     &             ((alpha(mgs,lr) + 3.)*(alpha(mgs,lr) + 2.)*(alpha(mgs,lr) + 1.)) )
            ENDIF 
          ENDIF
        ENDIF 
        ENDIF 
       ENDIF 
      ENDIF 
        



        ec0(mgs) = 2.e9
        IF ( qx(mgs,lr) .gt. qxmin(lr) ) THEN
        rwrad = 0.5*xdia(mgs,lr,3)
        IF ( xdia(mgs,lr,3) .gt. 2.0e-3 ) THEN
          ec0(mgs) = 0.0
          cracr(mgs) = 0.0
        ELSE
         IF ( dmrauto <= 0 .or.  rho0(mgs)*qx(mgs,lr) > 1.2*xl2p(mgs) ) THEN 
          IF ( xdia(mgs,lr,3) .lt. 6.1e-4 ) THEN
            ec0(mgs) = 1.0
          ELSE
            ec0(mgs) = Exp(-50.0*(50.0*(xdia(mgs,lr,3) - 6.0e-4)))
          ENDIF
          

          IF ( rwrad .ge. 50.e-6 ) THEN
              cracr(mgs) = ec0(mgs)*aa2*cx(mgs,lr)**2*xv(mgs,lr)
          ELSE
            IF ( imurain == 3 ) THEN
             cracr(mgs) = ec0(mgs)*aa1*(cx(mgs,lr)*xv(mgs,lr))**2*   &
     &                   (alpha(mgs,lr) + 2.)/(alpha(mgs,lr) + 1.)
            ELSE 
             cracr(mgs) = ec0(mgs)*aa1*(cx(mgs,lr)*xv(mgs,lr))**2*   &
     &                   (alpha(mgs,lr) + 6.)*(alpha(mgs,lr) + 5.)*(alpha(mgs,lr) + 4.)/ &
     &                  ((alpha(mgs,lr) + 3.)*(alpha(mgs,lr) + 2.)*(alpha(mgs,lr) + 1.))
              
            ENDIF
          ENDIF

         ENDIF
        ENDIF
        ENDIF


      end do
      end if





      if (ndebug .gt. 0 ) write(0,*) 'ICEZVD_GS: conc 22ii'
      chacw(:) = 0.0
      if ( ipconc .ge. 1 .or. ipelec .ge. 1 ) then
      do mgs = 1,ngscnt

      IF ( ipconc .ge. 5 ) THEN
       IF ( qhacw(mgs) .gt. 0.0 .and. xmas(mgs,lc) .gt. 0.0 ) THEN













        chacw(mgs) = qhacw(mgs)*rho0(mgs)/xmascw(mgs)

        chacw(mgs) = Min( chacw(mgs), 0.5*cx(mgs,lc)/dtp )
       ELSE
        qhacw(mgs) = 0.0
       ENDIF
      ELSE
      chacw(mgs) =   &
     &   ((0.25)*pi)*ehw(mgs)*cx(mgs,lc)*cx(mgs,lh)   &
     &  *abs(vtxbar(mgs,lh,1)-vtxbar(mgs,lc,1))   &
     &  *(  gf1*xdia(mgs,lc,2)   &
     &    + 2.0*gf2*xdia(mgs,lc,1)*xdia(mgs,lh,1)   &
     &    + gf3*xdia(mgs,lh,2) )
      chacw(mgs) = min(chacw(mgs),0.5*cx(mgs,lc)/dtp)


      ENDIF
      end do
      end if

      if (ndebug .gt. 0 ) write(0,*) 'ICEZVD_GS: conc 22kk'
      chaci(:) = 0.0
      if ( ipconc .ge. 1 .or. ipelec .ge. 1 ) then
      do mgs = 1,ngscnt
      IF ( ehi(mgs) .gt. 0.0 ) THEN
       IF ( ipconc .ge. 5 ) THEN

       vt = Sqrt((vtxbar(mgs,lh,1)-vtxbar(mgs,li,1))**2 +    &
     &            0.04*vtxbar(mgs,lh,1)*vtxbar(mgs,li,1) )

          chaci(mgs) = 0.25*pi*ehi(mgs)*cx(mgs,lh)*cx(mgs,li)*vt*   &
     &         (  da0lh(mgs)*xdia(mgs,lh,3)**2 +     &
     &            dab0lh(mgs,li,lh)*xdia(mgs,lh,3)*xdia(mgs,li,3) +    &
     &            da0(li)*xdia(mgs,li,3)**2 )

       ELSE
        chaci(mgs) =   &
     &   ((0.25)*pi)*ehi(mgs)*cx(mgs,li)*cx(mgs,lh)   &
     &  *abs(vtxbar(mgs,lh,1)-vtxbar(mgs,li,1))   &
     &  *(  gf1*xdia(mgs,li,2)   &
     &    + 2.0*gf2*xdia(mgs,li,1)*xdia(mgs,lh,1)   &
     &    + gf3*xdia(mgs,lh,2) )
        ENDIF

        chaci(mgs) = min(chaci(mgs),cimxd(mgs))
       ENDIF
      end do
      end if


      if (ndebug .gt. 0 ) write(0,*) 'ICEZVD_GS: conc 22nn'
      chacs(:) = 0.0
      if ( ipconc .ge. 1 .or. ipelec .ge. 1 ) then
      do mgs = 1,ngscnt
      IF ( ehs(mgs) .gt. 0 ) THEN
       IF ( ipconc .ge. 5 ) THEN

       vt = Sqrt((vtxbar(mgs,lh,1)-vtxbar(mgs,ls,1))**2 +    &
     &            0.04*vtxbar(mgs,lh,1)*vtxbar(mgs,ls,1) )

          chacs(mgs) = 0.25*pi*ehs(mgs)*cx(mgs,lh)*cx(mgs,ls)*vt*   &
     &         (  da0lh(mgs)*xdia(mgs,lh,3)**2 +     &
     &            dab0lh(mgs,ls,lh)*xdia(mgs,lh,3)*xdia(mgs,ls,3) +    &
     &            da0(ls)*xdia(mgs,ls,3)**2 )

       ELSE
      chacs(mgs) =   &
     &   ((0.25)*pi)*ehs(mgs)*cx(mgs,ls)*cx(mgs,lh)   &
     &  *abs(vtxbar(mgs,lh,1)-vtxbar(mgs,ls,1))   &
     &  *(  gf3*gf1*xdia(mgs,ls,2)   &
     &    + 2.0*gf2*gf2*xdia(mgs,ls,1)*xdia(mgs,lh,1)   &
     &    + gf1*gf3*xdia(mgs,lh,2) )
      ENDIF
      chacs(mgs) = min(chacs(mgs),csmxd(mgs))
      ENDIF
      end do
      end if






      if (ndebug .gt. 0 ) write(0,*) 'ICEZVD_GS: conc 22ii'
      chlacw(:) = 0.0
      if ( ipconc .ge. 1 .or. ipelec .ge. 1 ) then
      do mgs = 1,ngscnt

      IF ( lhl .gt. 1 .and. ipconc .ge. 5 ) THEN
       IF ( qhlacw(mgs) .gt. 0.0 .and. xmas(mgs,lc) .gt. 0.0 ) THEN













        chlacw(mgs) = qhlacw(mgs)*rho0(mgs)/xmascw(mgs)

        chlacw(mgs) = Min( chlacw(mgs), 0.5*cx(mgs,lc)/dtp )
       ELSE
        qhlacw(mgs) = 0.0
       ENDIF










      ENDIF
      end do
      end if

      if (ndebug .gt. 0 ) write(0,*) 'ICEZVD_GS: conc 22kk'
      chlaci(:) = 0.0
      if ( ipconc .ge. 1 .or. ipelec .ge. 1 ) then
      do mgs = 1,ngscnt
      IF ( lhl .gt. 1 .and. ehli(mgs) .gt. 0.0 ) THEN
       IF ( ipconc .ge. 5 ) THEN

       vt = Sqrt((vtxbar(mgs,lhl,1)-vtxbar(mgs,li,1))**2 +    &
     &            0.04*vtxbar(mgs,lhl,1)*vtxbar(mgs,li,1) )

          chlaci(mgs) = 0.25*pi*ehli(mgs)*cx(mgs,lhl)*cx(mgs,li)*vt*   &
     &         (  da0lhl(mgs)*xdia(mgs,lhl,3)**2 +     &
     &            dab0(lhl,li)*xdia(mgs,lhl,3)*xdia(mgs,li,3) +    &
     &            da0(li)*xdia(mgs,li,3)**2 )








        ENDIF

        chlaci(mgs) = min(chlaci(mgs),cimxd(mgs))
       ENDIF
      end do
      end if


      if (ndebug .gt. 0 ) write(0,*) 'ICEZVD_GS: conc 22jj'
      chlacs(:) = 0.0
      if ( ipconc .ge. 1 .or. ipelec .ge. 1 ) then
      do mgs = 1,ngscnt
      IF ( lhl .gt. 1 .and. ehls(mgs) .gt. 0 ) THEN
       IF ( ipconc .ge. 5 ) THEN

       vt = Sqrt((vtxbar(mgs,lhl,1)-vtxbar(mgs,ls,1))**2 +    &
     &            0.04*vtxbar(mgs,lhl,1)*vtxbar(mgs,ls,1) )

          chlacs(mgs) = 0.25*pi*ehls(mgs)*cx(mgs,lhl)*cx(mgs,ls)*vt*   &
     &         (  da0lhl(mgs)*xdia(mgs,lhl,3)**2 +     &
     &            dab0(lhl,ls)*xdia(mgs,lhl,3)*xdia(mgs,ls,3) +    &
     &            da0(ls)*xdia(mgs,ls,3)**2 )








      ENDIF
      chlacs(mgs) = min(chlacs(mgs),csmxd(mgs))
      ENDIF
      end do
      end if





      IF ( ipconc .ge. 2 .and. ircnw /= -1) THEN 
      if (ndebug .gt. 0 ) write(0,*) 'conc 26a'
      
      DO mgs = 1,ngscnt
        zrcnw(mgs) = 0.0
        qrcnw(mgs) = 0.0
        crcnw(mgs) = 0.0
        cautn(mgs) = 0.0
      ENDDO
      
      DO mgs = 1,ngscnt


       IF ( qx(mgs,lc) .gt. qxmin(lc) .and. cx(mgs,lc) .gt. 1000. .and. temg(mgs) .gt. tfrh+4.) THEN
       
         volb = xv(mgs,lc)*(1./(1.+CNU))**(1./2.)
         cautn(mgs) = Min(ccmxd(mgs),   &
     &      ((CNU+2.)/(CNU+1.))*aa1*cx(mgs,lc)**2*xv(mgs,lc)**2)
         cautn(mgs) = Max( 0.0d0, cautn(mgs) )
         IF ( rb(mgs) .le. 7.51d-6 ) THEN
           t2s = 1.d30

         ELSE

         



           t2s = 3.72/(1.e6*(rb(mgs)-7.500d-6)*rho0(mgs)*qx(mgs,lc))

           qrcnw(mgs) = Max( 0.0d0, xl2p(mgs)/(t2s*rho0(mgs)) )
           crcnw(mgs) = Max( 0.0d0, Min(3.5e9*xl2p(mgs)/t2s,0.5*cautn(mgs)) )
           
           IF ( dmrauto == 0 ) THEN
             IF ( qx(mgs,lr)*rho0(mgs) > 1.2*xl2p(mgs) .and. cx(mgs,lr) > cxmin ) THEN 
               crcnw(mgs) = cx(mgs,lr)/qx(mgs,lr)*qrcnw(mgs)
             ENDIF
           ELSEIF ( dmrauto == 1  .and. cx(mgs,lr) > cxmin) THEN
             IF ( qx(mgs,lr) > qxmin(lr) ) THEN
               tmp = qrcnw(mgs)*cx(mgs,lr)/qx(mgs,lr)
               crcnw(mgs) = Min(tmp,crcnw(mgs) )
             ENDIF
           ELSEIF ( dmrauto == 2  .and. cx(mgs,lr) > cxmin) THEN
               tmp = crcnw(mgs)
               tmp2 = qrcnw(mgs)*cx(mgs,lr)/qx(mgs,lr)
               
               crcnw(mgs) = (tmp*qrcnw(mgs)+tmp2*qx(mgs,lr))/(qrcnw(mgs)+qx(mgs,lr))
           ELSEIF ( dmrauto == 3  .and. cx(mgs,lr) > cxmin) THEN 
              tmp = Max( 2.d0*rh(mgs), dble( xdia(mgs,lr,3) ) )
              crcnw(mgs) = rho0(mgs)*qrcnw(mgs)/(pi/6.*1000.*tmp**3)
           ENDIF
           
           IF ( crcnw(mgs) < 1.e-30 ) qrcnw(mgs) = 0.0

























         ENDIF


       ENDIF
      ENDDO



      ELSE





      if ( ircnw .eq. 4 ) then
      do mgs = 1,ngscnt

      qrcnw(mgs) =  0.0
      qdiff = max((qx(mgs,lc)-qminrncw),0.0)
      if ( qdiff .gt. 0.0 .and. xdia(mgs,lc,1) .gt. 20.0e-6 ) then
      argrcnw =   &
     &  ((1.2e-4)+(1.596e-12)*(cx(mgs,lc)*1.0e-6)   &
     &  /(cwdisp*qdiff*1.0e-3*rho0(mgs)))
      qrcnw(mgs) = (rho0(mgs)*1e-3)*(qdiff**2)/argrcnw

      qrcnw(mgs) = (max(qrcnw(mgs),0.0))
      end if
      end do

      ENDIF






      if ( ircnw .eq. 5 ) then
      do mgs = 1,ngscnt
      qrcnw(mgs) = 0.0
      qrcnw(mgs) =  0.0
      qccrit = (pi/6.)*(cx(mgs,lc)*cwdiap**3)*xdn(mgs,lc)/rho0(mgs)
      qdiff = max((qx(mgs,lc)-qccrit),0.)
      if ( qdiff .gt. 0.0 .and. cx(mgs,lc) .gt. 1.0 ) then
      argrcnw = &

     &  ((1.2e-4)+(1.596e-12)*cx(mgs,lc)*1.0e-3/(cwdisp*rho0(mgs)*qdiff))
      qrcnw(mgs) = &

     &  1.0e-3*rho0(mgs)*(qdiff**2)/argrcnw
      qrcnw(mgs) = Min(qxmxd(mgs,lc), (max(qrcnw(mgs),0.0)) )


      end if
      end do
      end if





      if ( ircnw .eq. 2 ) then
      do mgs = 1,ngscnt
      qrcnw(mgs) = 0.0
      qrcnw(mgs) = (0.001)*max((qx(mgs,lc)-qminrncw),0.0)
      end do
      end if





      if ( ircnw .eq. 1 ) then
      do mgs = 1,ngscnt
      qrcnw(mgs) = 0.0
      c1 = 0.2
      c4 = pi/(6.0)
      bradp =    &
     & (1.e+06) * ((c1/(0.38))**(1./3.)) * (xdia(mgs,lc,1)*(0.5))
      bl2 =   &
     & (0.027) * ((100.0)*(bradp**3)*(xdia(mgs,lc,1)*(0.5)) - (0.4))
      bt2 = (bradp -7.5) / (3.72)
      qrcnw(mgs) = 0.0
      if ( bl2 .gt. 0.0 .and. bt2 .gt. 0.0 ) then
      qrcnw(mgs) = bl2 * bt2 * rho0(mgs)   &
     &  * qx(mgs,lc) * qx(mgs,lc)
      end if
      end do
      end if



      ENDIF  






      if (ndebug .gt. 0 ) write(0,*) 'conc 27a'
      qrfrz(:) = 0.0
      qrfrzs(:) = 0.0
      qrfrzf(:) = 0.0
      vrfrzf(:) = 0.0
      crfrz(:) = 0.0
      crfrzs(:) = 0.0
      crfrzf(:) = 0.0
      zrfrz(:)  = 0.0
      zrfrzf(:)  = 0.0
      qwcnr(:) = 0.0
      
      IF ( .not. ( ipconc == 0 .and. lwsm6 ) ) THEN
      
      do mgs = 1,ngscnt 
      if ( qx(mgs,lr) .gt. qxmin(lr) .and. temcg(mgs) .lt. -5. ) then


       IF ( ipconc .lt. 3 ) THEN
       qrfrz(mgs) =    &
     &  min(   &
     &  (20.0)*(pi**2)*brz*(xdn(mgs,lr)/rho0(mgs))   &
     &   *cx(mgs,lr)*(xdia(mgs,lr,1)**6)   &
     &   *(exp(max(-arz*temcg(mgs), 0.0))-1.0)   &
     &  , qrmxd(mgs))
        qrfrzf(mgs) = qrfrz(mgs)


       ELSEIF ( ipconc .ge. 3 ) THEN



         frach = 1.0d0
         
         IF ( ibiggopt == 2 .and. imurain == 1 ) THEN
         
           
           volt = exp( 16.2 + 1.0*temcg(mgs) )* 1.0e-6 
                                               
                                               
           dbigg = (6./pi* volt )**(1./3.) 
           
           
           
             ratio = dbigg/xdia(mgs,lr,1)
           
           i = Int(Min(25.0,ratio))
           j = Int(Max(0.0,Min(15.,alpha(mgs,lr))))
           delx = ratio - float(i)
           dely = alpha(mgs,lr) - float(j)
           ip1 = Min( i+1, nqiacrratio )
           jp1 = Min( j+1, nqiacralpha )

           
           tmp1 = ciacrratio(i,j) + delx*(ciacrratio(ip1,j) - ciacrratio(i,j))
           tmp2 = ciacrratio(i,jp1) + delx*(ciacrratio(ip1,jp1) - ciacrratio(i,jp1))
           
           
           
           crfrz(mgs) = (tmp1 + dely*(tmp2 - tmp1))*cx(mgs,lr)/dtp
           
           
           tmp1 = qiacrratio(i,j) + delx*(qiacrratio(ip1,j) - qiacrratio(i,j))
           tmp2 = qiacrratio(i,jp1) + delx*(qiacrratio(ip1,jp1) - qiacrratio(i,jp1))
           
           
           
           qrfrz(mgs) = (tmp1 + dely*(tmp2 - tmp1))*qx(mgs,lr)/dtp
           
           
           
           IF ( dbigg < Max(dfrz,dhmn) .and. ibiggsnow > 0 ) THEN 
            
            crfrzs(mgs) = qrfrz(mgs)
            qrfrzs(mgs) = crfrz(mgs)


           
           ratio = Max(dfrz,dhmn)/xdia(mgs,lr,1)
           
           i = Int(Min(25.0,ratio))
           j = Int(Max(0.0,Min(15.,alpha(mgs,lr))))
           delx = ratio - float(i)
           dely = alpha(mgs,lr) - float(j)
           ip1 = Min( i+1, nqiacrratio )
           jp1 = Min( j+1, nqiacralpha )

           
           tmp1 = ciacrratio(i,j) + delx*(ciacrratio(ip1,j) - ciacrratio(i,j))
           tmp2 = ciacrratio(i,jp1) + delx*(ciacrratio(ip1,jp1) - ciacrratio(i,jp1))
           
           
           
           crfrz(mgs) = (tmp1 + dely*(tmp2 - tmp1))*cx(mgs,lr)/dtp
           
           
           tmp1 = qiacrratio(i,j) + delx*(qiacrratio(ip1,j) - qiacrratio(i,j))
           tmp2 = qiacrratio(i,jp1) + delx*(qiacrratio(ip1,jp1) - qiacrratio(i,jp1))
           
           
           
           qrfrz(mgs) = (tmp1 + dely*(tmp2 - tmp1))*qx(mgs,lr)/dtp

           
            crfrzs(mgs) = crfrzs(mgs) - crfrz(mgs)
            qrfrzs(mgs) = qrfrzs(mgs) - qrfrz(mgs)

           
           ELSE
            crfrzs(mgs) = 0.0
            qrfrzs(mgs) = 0.0
           ENDIF
           
           IF ( (qrfrzs(mgs) + qrfrz(mgs))*dtp > qx(mgs,lr) ) THEN
             fac = ( qrfrzs(mgs) + qrfrz(mgs) )*dtp/qx(mgs,lr)
             qrfrz(mgs) = fac*qrfrz(mgs)
             qrfrzs(mgs) = fac*qrfrzs(mgs)
             qrfrzf(mgs) = fac*qrfrzf(mgs)
             crfrz(mgs) = fac*crfrz(mgs)
             crfrzs(mgs) = fac*crfrzs(mgs)
             crfrzf(mgs) = fac*crfrzf(mgs)
           ENDIF





           
           qrfrzf(mgs) = qrfrz(mgs)
           crfrzf(mgs) = crfrz(mgs)
           
           qrfrz(mgs) = qrfrzf(mgs) + qrfrzs(mgs)
           crfrz(mgs) = crfrzf(mgs) + crfrzs(mgs)

           
         ELSE 
         
         tmp = xv(mgs,lr)*brz*cx(mgs,lr)*(Exp(Max( -arz*temcg(mgs), 0.0 )) - 1.0)
         IF ( .false. .and. tmp .gt. cxmxd(mgs,lr) ) THEN 



           crfrz(mgs) = cxmxd(mgs,lr) 
           qrfrz(mgs) = qxmxd(mgs,lr) 

         ELSE 
         crfrz(mgs) = tmp
 
 
 
 
 
 
         IF ( lzr < 1 ) THEN
           IF ( imurain == 3 ) THEN
             bfnu = bfnu0
           ELSE 
             bfnu = bfnu1
           ENDIF
         ELSE
 
           IF ( imurain == 3 ) THEN
             bfnu = (alpha(mgs,lr)+2.0)/(alpha(mgs,lr)+1.)
           ELSE 

            bfnu = (4. + alpha(mgs,lr))*(5. + alpha(mgs,lr))*(6. + alpha(mgs,lr))/  &
     &            ((1. + alpha(mgs,lr))*(2. + alpha(mgs,lr))*(3. + alpha(mgs,lr)))

           ENDIF
         ENDIF 
         qrfrz(mgs) = bfnu*xmas(mgs,lr)*rhoinv(mgs)*crfrz(mgs)

         qrfrz(mgs) = Min( qrfrz(mgs), 1.*qx(mgs,lr)/dtp ) 
         crfrz(mgs) = Min( crfrz(mgs), 1.*cx(mgs,lr)/dtp ) 
         qrfrz(mgs) = Min( qrfrz(mgs), qx(mgs,lr) )
         qrfrzf(mgs) = qrfrz(mgs)
         ENDIF 

         
         
         
         IF ( crfrz(mgs) .gt. 0.0 ) THEN


           
           IF ( ibiggsnow == 1 .or. ibiggsnow == 3 ) THEN
           xvfrz = rho0(mgs)*qrfrz(mgs)/(crfrz(mgs)*900.) 
           frach = 0.5 *(1. +  Tanh(0.2e12 *( xvfrz - 1.15*xvmn(lh))))

             qrfrzs(mgs) = (1.-frach)*qrfrz(mgs)
             crfrzs(mgs) = (1.-frach)*crfrz(mgs) 

           
           ENDIF
           
           IF ( ipconc .ge. 14 .and. 1.e-3*rho0(mgs)*qrfrz(mgs)/crfrz(mgs) .lt. xvmn(lh) ) THEN
             qrfrzs(mgs) = qrfrz(mgs)
             crfrzs(mgs) = crfrz(mgs) 
           ELSE


             qrfrzf(mgs) = frach*qrfrz(mgs)

            IF ( ibfr .le. 1 ) THEN
             crfrzf(mgs) = Min(frach*crfrz(mgs), dble(qrfrz(mgs)/(bfnu*1.0*vr1mm*1000.0)*rho0(mgs)) ) 
            ELSEIF ( ibfr .eq. 5 ) THEN
             crfrzf(mgs) = Min(frach*crfrz(mgs), dble(qrfrz(mgs)/(bfnu*vfrz*1000.0)*rho0(mgs)) )*rzxh(mgs)  
            ELSE
             crfrzf(mgs) = Min(frach*crfrz(mgs), dble(qrfrz(mgs)/(bfnu*vfrz*1000.0)*rho0(mgs)) ) 
            ENDIF




            
           ENDIF

         ELSE
          crfrz(mgs) = 0.0
          qrfrz(mgs) = 0.0
         ENDIF

         ENDIF 

          IF ( lvol(lh) .gt. 1 ) THEN
           vrfrzf(mgs) = rho0(mgs)*qrfrzf(mgs)/rhofrz
          ENDIF

        
        IF ( nsplinter .ne. 0 ) THEN
          IF ( nsplinter .gt. 0 ) THEN
            tmp = nsplinter*crfrz(mgs)
          ELSE
            tmp = -nsplinter*crfrzf(mgs)
          ENDIF
          csplinter2(mgs) = tmp
          qsplinter2(mgs) = Min(0.1*qrfrz(mgs), tmp*splintermass/rho0(mgs) ) 



        ENDIF










       ENDIF

      else

      end if
      end do
      
      ENDIF




      if (ndebug .gt. 0 ) write(0,*) 'conc 25b'
      do mgs = 1,ngscnt
      qwfrz(mgs) = 0.0
      cwfrz(mgs) = 0.0
      qwfrzc(mgs) = 0.0
      cwfrzc(mgs) = 0.0
      qwfrzp(mgs) = 0.0
      cwfrzp(mgs) = 0.0
      IF ( ibfc == 1 .and. temg(mgs) <= 268.15 ) THEN


      if ( qx(mgs,lc) .gt. qxmin(lc) .and. cx(mgs,lc) .gt. 0.0 ) THEN
      IF ( ipconc < 2 ) THEN
      qwfrz(mgs) = ((2.0)*(brz)/(xdn(mgs,lc)*cx(mgs,lc)))   &
     &  *(exp(max(-arz*temcg(mgs), 0.0))-1.0)   &
     &  *rho0(mgs)*(qx(mgs,lc)**2)
      qwfrz(mgs) = max(qwfrz(mgs), 0.0)
      qwfrz(mgs) = min(qwfrz(mgs),qcmxd(mgs))
         cwfrz(mgs) = qwfrz(mgs)*rho0(mgs)/xmas(mgs,li)
       ELSEIF ( ipconc .ge. 2 ) THEN
         IF ( xdia(mgs,lc,3) > 0.e-6 ) THEN
          volt = exp( 16.2 + 1.0*temcg(mgs) )* 1.0e-6 
                                               
                                               


         
         cwfrz(mgs) = cx(mgs,lc)*Exp(-volt/xv(mgs,lc))/dtp 



         qwfrz(mgs) = cwfrz(mgs)*xdn0(lc)*rhoinv(mgs)*(volt + xv(mgs,lc))

                                                       
                                                       
         IF ( temg(mgs) < tfrh - 3 ) THEN
          cwfrz(mgs) = cx(mgs,lc)
          qwfrz(mgs) = qx(mgs,lc)
         ENDIF






         ENDIF
       ENDIF
      if ( temg(mgs) .gt. 268.15 ) then
      qwfrz(mgs) = 0.0
      cwfrz(mgs) = 0.0
      end if
      end if
      ENDIF

      if ( xplate(mgs) .eq. 1 ) then
      qwfrzp(mgs) = qwfrz(mgs)
      cwfrzp(mgs) = cwfrz(mgs)
      end if

      if ( xcolmn(mgs) .eq. 1 ) then
      qwfrzc(mgs) = qwfrz(mgs)
      cwfrzc(mgs) = cwfrz(mgs)
      end if




      end do





      if (ndebug .gt. 0 ) write(0,*) 'conc 25a'
      do mgs = 1,ngscnt

       ccia(mgs) = 0.0

       cwctfz(mgs) = 0.0
       qwctfz(mgs) = 0.0
       ctfzbd(mgs) = 0.0
       ctfzth(mgs) = 0.0
       ctfzdi(mgs) = 0.0

       cwctfzc(mgs) = 0.0
       qwctfzc(mgs) = 0.0
       cwctfzp(mgs) = 0.0
       qwctfzp(mgs) = 0.0

       IF ( icfn .ge. 1 ) THEN

       IF ( temg(mgs) .lt. 271.15  .and. qx(mgs,lc) .gt. qxmin(lc)) THEN



        IF ( icfn .ge. 2 ) THEN
         ccia(mgs) = exp( 4.11 - (0.262)*temcg(mgs) )  
         




         knud(mgs) = 2.28e-5 * temg(mgs) / ( pres(mgs)*raero ) 
         knuda(mgs) = 1.257 + 0.4*exp(-1.1/knud(mgs))          
         gtp(mgs) = 1. / ( fai(mgs) + fbi(mgs) )               
         dfar(mgs) = kb*temg(mgs)*(1.+knuda(mgs)*knud(mgs))/(6.*pi*fadvisc(mgs)*raero) 
         fn1(mgs) = 2.*pi*xdia(mgs,lc,1)*cx(mgs,lc)*ccia(mgs)
         fn2(mgs) = -gtp(mgs)*(ssw(mgs)-1.)*felv(mgs)/pres(mgs)
         fnft(mgs) = 0.4*(1.+1.45*knud(mgs)+0.4*knud(mgs)*exp(-1./knud(mgs)))*(ftka(mgs)+2.5*knud(mgs)*kaero)      &
     &              / ( (1.+3.*knud(mgs))*(2*ftka(mgs)+5.*knud(mgs)*kaero+kaero) )



         ctfzbd(mgs) = fn1(mgs)*dfar(mgs)


         ctfzth(mgs) = fn1(mgs)*fn2(mgs)*fnft(mgs)/rho0(mgs)


         ctfzdi(mgs) = fn1(mgs)*fn2(mgs)*rw*temg(mgs)/(felv(mgs)*rho0(mgs))

         cwctfz(mgs) = max( ctfzbd(mgs) + ctfzth(mgs) + ctfzdi(mgs) , 0.)









        ELSEIF ( icfn .eq. 1 ) THEN
         IF ( wvel(mgs) .lt. -0.05 ) THEN 
           cwctfz(mgs) = cfnfac*exp( (-2.80) - (0.262)*temcg(mgs) )
           cwctfz(mgs) = Min((1.0e3)*cwctfz(mgs), ccmxd(mgs) )  
         ENDIF
        ENDIF   

        IF ( ipconc .ge. 2 ) THEN
         cwctfz(mgs) = Min( cwctfz(mgs)/dtp, ccmxd(mgs) )
         qwctfz(mgs) = xmas(mgs,lc)*cwctfz(mgs)/rho0(mgs)
        ELSE
         qwctfz(mgs) = (cimasn)*cwctfz(mgs)/(dtp*rho0(mgs))
         qwctfz(mgs) = max(qwctfz(mgs), 0.0)
         qwctfz(mgs) = min(qwctfz(mgs),qcmxd(mgs))
        ENDIF


        if ( xplate(mgs) .eq. 1 ) then
         qwctfzp(mgs) = qwctfz(mgs)
         cwctfzp(mgs) = cwctfz(mgs)
        end if

        if ( xcolmn(mgs) .eq. 1 ) then
         qwctfzc(mgs) = qwctfz(mgs)
         cwctfzc(mgs) = cwctfz(mgs)
        end if




       end if

       ENDIF 

      end do





      if (ndebug .gt. 0 ) write(0,*) 'conc 23a'
      dtrh = 300.0
      hrifac = (1.e-3)*((0.044)*(0.01**3))
      do mgs = 1,ngscnt
      ciihr(mgs) = 0.0
      qiihr(mgs) = 0.0
      cicichr(mgs) = 0.0
      qicichr(mgs) = 0.0
      cipiphr(mgs) = 0.0
      qipiphr(mgs) = 0.0
      IF ( ihrn .ge. 1 ) THEN
      if ( qx(mgs,lc) .gt. qxmin(lc) ) then
      if ( temg(mgs) .lt. 273.15 ) then










      IF ( Log(cx(mgs,lc)*(1.e-6)/(3.0)) .gt. 0.0 ) THEN
      ciihr(mgs) = ((1.69e17)/dtrh)   &
     & *(log(cx(mgs,lc)*(1.e-6)/(3.0)) *   &
     &  ((1.e-3)*rho0(mgs)*qx(mgs,lc))/(cx(mgs,lc)*(1.e-6)))**(7./3.)
      ciihr(mgs) = ciihr(mgs)*(1.0e6)
      qiihr(mgs) = hrifac*ciihr(mgs)/rho0(mgs)
      qiihr(mgs) = max(qiihr(mgs), 0.0)
      qiihr(mgs) = min(qiihr(mgs),qcmxd(mgs))
      ENDIF

      if ( xplate(mgs) .eq. 1 ) then
      qipiphr(mgs) = qiihr(mgs)
      cipiphr(mgs) = ciihr(mgs)
      end if

      if ( xcolmn(mgs) .eq. 1 ) then
      qicichr(mgs) = qiihr(mgs)
      cicichr(mgs) = ciihr(mgs)
      end if




      end if
      end if
      ENDIF 
      end do
























      hdia0 = 300.0e-6
      do mgs = 1,ngscnt
      qscnvi(mgs) = 0.0
      cscnvi(mgs) = 0.0
      cscnvis(mgs) = 0.0


      IF ( temg(mgs) .lt. tfr .and. qx(mgs,li) .gt. qxmin(li) ) THEN
        IF ( ipconc .ge. 4 .and. .false. ) THEN
         if ( cx(mgs,li) .gt. 10. .and. xdia(mgs,li,1) .gt. 50.e-6 ) then 
         cirdiatmp =   &
     &  (qx(mgs,li)*rho0(mgs)   &
     & /(pi*xdn(mgs,li)*cx(mgs,li)))**(1./3.)
          IF ( cirdiatmp .gt. 100.e-6 ) THEN 
          qscnvi(mgs) =   &
     &  ((pi*xdn(mgs,li)*cx(mgs,li)) / (6.0*rho0(mgs)*dtp))   &
     & *exp(-hdia0/cirdiatmp)   &
     & *( (hdia0**3) + 3.0*(hdia0**2)*cirdiatmp   &
     &  + 6.0*(hdia0)*(cirdiatmp**2) + 6.0*(cirdiatmp**3) )
      qscnvi(mgs) =   &
     &  min(qscnvi(mgs),qimxd(mgs))
          IF ( ipconc .ge. 4 ) THEN
            cscnvi(mgs) = Min( cimxd(mgs), cx(mgs,li)*Exp(-hdia0/cirdiatmp))
          ENDIF
         ENDIF  
        end if 

       ELSEIF ( ipconc .lt. 4 ) THEN

        qscnvi(mgs) = 0.001*eii(mgs)*max((qx(mgs,li)-1.e-3),0.0)
        qscnvi(mgs) = min(qscnvi(mgs),qxmxd(mgs,li))
        cscnvi(mgs) = qscnvi(mgs)*rho0(mgs)/xmas(mgs,li)
        cscnvis(mgs) = 0.5*cscnvi(mgs)

       ENDIF
      ENDIF

      end do




      do mgs = 1,ngscnt
      fvent(mgs) = (fschm(mgs)**(1./3.)) * (fakvisc(mgs)**(-0.5))
      end do


      if ( ndebug .gt. 0 ) write(0,*) 'civent'

      civenta = 1.258e4
      civentb = 2.331
      civentc = 5.662e4
      civentd = 2.373
      civente = 0.8241
      civentf = -0.042
      civentg = 1.70

      do mgs = 1,ngscnt
      IF ( icond .eq. 1 .or. temg(mgs) .le. tfrh    &
     &      .or. (qx(mgs,lr) .le. qxmin(lr) .and. qx(mgs,lc) .le. qxmin(lc)) ) THEN
      IF ( qx(mgs,li) .gt. qxmin(li) ) THEN
      cireyn =   &
     &  (civenta*xdia(mgs,li,1)**civentb   &
     &  +civentc*xdia(mgs,li,1)**civentd)   &
     &  /   &
     &  (civente*xdia(mgs,li,1)**civentf+civentg)
      xcivent = (fschm(mgs)**(1./3.))*((cireyn/fakvisc(mgs))**0.5)
      if ( xcivent .lt. 1.0 ) then
      civent(mgs) = 1.0 + 0.14*xcivent**2
      end if
      if ( xcivent .ge. 1.0 ) then
      civent(mgs) = 0.86 + 0.28*xcivent
      end if
      ELSE
       civent(mgs) = 0.0
      ENDIF
      ENDIF 
      end do



      igmrwa = 100.0*2.0
      igmrwb = 100.*((5.0+br)/2.0)
      rwventa = (0.78)*gmoi(igmrwa)  
      rwventb = (0.308)*gmoi(igmrwb) 
      do mgs = 1,ngscnt
      IF ( qx(mgs,lr) .gt. qxmin(lr) ) THEN
        IF ( ipconc .ge. 3 ) THEN
          IF ( imurain == 3 ) THEN
           IF ( izwisventr == 1 ) THEN
            rwvent(mgs) = ventrx(mgs)*(1.6 + 124.9*(1.e-3*rho0(mgs)*qx(mgs,lr))**.2046)
           ELSE 

          rwvent(mgs) =   &
     &  (0.78*ventrx(mgs) + 0.308*ventrxn(mgs)*fvent(mgs)   &
     &   *Sqrt((ar*rhovt(mgs)))   &
     &    *(xdia(mgs,lr,1)**((1.0+br)/2.0)) )
           ENDIF

          ELSE 
       





        IF ( iferwisventr == 1 ) THEN
 
         alpr = Min(alpharmax, alpha(mgs,lr))

        x =  1. + alpr

        tmp = 1 + alpr
        i = Int(dgami*(tmp))
        del = tmp - dgam*i
        g1palp = gmoi(i) + (gmoi(i+1) - gmoi(i))*del*dgami

        tmp = 2.5 + alpr + 0.5*bx(lr)
        i = Int(dgami*(tmp))
        del = tmp - dgam*i
        y = (gmoi(i) + (gmoi(i+1) - gmoi(i))*del*dgami)/g1palp 

        
         vent1 = dble(xdia(mgs,lr,1))**(-2. - alpr)
         vent2 = dble(1./xdia(mgs,lr,1) + 0.5*fx(lr))**dble(2.5+alpr+0.5*bx(lr))
        
        
        rwvent(mgs) =    &
     &    0.78*x +    &
     &    0.308*fvent(mgs)*y*   &
     &            Sqrt(ax(lr)*rhovt(mgs))*(vent1/vent2)
       
        ELSEIF ( iferwisventr == 2 ) THEN
          

         x =  1. + alpha(mgs,lr)

           rwvent(mgs) =   &
     &  (0.78*x + 0.308*ventrxn(mgs)*fvent(mgs)   &
     &   *Sqrt((ar*rhovt(mgs)))   &
     &    *(xdia(mgs,lr,1)**((1.0+br)/2.0)) )

          
          ENDIF 
          
          ENDIF 
        ELSE
         rwvent(mgs) =   &
     &  (rwventa + rwventb*fvent(mgs)   &
     &   *Sqrt((ar*rhovt(mgs)))   &
     &    *(xdia(mgs,lr,1)**((1.0+br)/2.0)) )
        ENDIF
      ELSE
       rwvent(mgs) = 0.0
      ENDIF
      end do

      igmswa = 100.0*2.0
      igmswb = 100.*((5.0+ds)/2.0)
      swventa = (0.78)*gmoi(igmswa)
      swventb = (0.308)*gmoi(igmswb)
      do mgs = 1,ngscnt
      IF ( qx(mgs,ls) .gt. qxmin(ls) ) THEN
      IF ( ipconc .ge. 4 ) THEN
      swvent(mgs) = 0.65 + 0.44*fvent(mgs)*Sqrt(vtxbar(mgs,ls,1)*xdia(mgs,ls,1))
      ELSE

       swvent(mgs) =   &
     &  (swventa + swventb*fvent(mgs)   &
     &   *Sqrt((cs*rhovt(mgs)))   &
     &   *(xdia(mgs,ls,1)**((1.0+ds)/2.0)) )
      ENDIF
      ELSE
      swvent(mgs) = 0.0
      ENDIF
      end do



      igmhwa = 100.0*2.0
      igmhwb = 100.0*2.75
      hwventa = (0.78)*gmoi(igmhwa)
      hwventb = (0.308)*gmoi(igmhwb)
      hwventc = (4.0*gr/(3.0*cdx(lh)))**(0.25)
      do mgs = 1,ngscnt
      IF ( qx(mgs,lh) .gt. qxmin(lh) ) THEN
       IF ( .false. .or. alpha(mgs,lh) .eq. 0.0 ) THEN
        hwvent(mgs) =   &
     &  ( hwventa + hwventb*hwventc*fvent(mgs)   &
     &    *((xdn(mgs,lh)/rho0(mgs))**(0.25))   &
     &    *(xdia(mgs,lh,1)**(0.75)))
       ELSE 
       




        
        x =  1. + alpha(mgs,lh)

        tmp = 1 + alpha(mgs,lh)
        i = Int(dgami*(tmp))
        del = tmp - dgam*i
        g1palp = gmoi(i) + (gmoi(i+1) - gmoi(i))*del*dgami

        tmp = 2.5 + alpha(mgs,lh) + 0.5*bx(lh)
        i = Int(dgami*(tmp))
        del = tmp - dgam*i
        y = (gmoi(i) + (gmoi(i+1) - gmoi(i))*del*dgami)/g1palp
        
        hwvent(mgs) =    &
     &  ( 0.78*x +    &
     &    0.308*fvent(mgs)*y*(xdia(mgs,lh,1)**(0.5 + 0.5*bx(lh)))*   &
     &            Sqrt(xdn(mgs,lh)*ax(lh)*rhovt(mgs)/rg0) )
       
       ENDIF
      ELSE
      hwvent(mgs) = 0.0
      ENDIF
      end do
      
      hlvent(:) = 0.0

      IF ( lhl .gt. 1 ) THEN
      igmhwa = 100.0*2.0
      igmhwb = 100.0*2.75
      hwventa = (0.78)*gmoi(igmhwa)
      hwventb = (0.308)*gmoi(igmhwb)
      hwventc = (4.0*gr/(3.0*cdx(lhl)))**(0.25)
      do mgs = 1,ngscnt
      IF ( qx(mgs,lhl) .gt. qxmin(lhl) ) THEN

       IF ( .false. .or. alpha(mgs,lhl) .eq. 0.0 ) THEN
        hlvent(mgs) =   &
     &  ( hwventa + hwventb*hwventc*fvent(mgs)   &
     &    *((xdn(mgs,lhl)/rho0(mgs))**(0.25))   &
     &    *(xdia(mgs,lhl,1)**(0.75)))
       ELSE 
       




        x =  1. + alpha(mgs,lhl)

        tmp = 1 + alpha(mgs,lhl)
        i = Int(dgami*(tmp))
        del = tmp - dgam*i
        g1palp = gmoi(i) + (gmoi(i+1) - gmoi(i))*del*dgami

        tmp = 2.5 + alpha(mgs,lhl) + 0.5*bx(lhl)
        i = Int(dgami*(tmp))
        del = tmp - dgam*i
        y = (gmoi(i) + (gmoi(i+1) - gmoi(i))*del*dgami)/g1palp 

        
        hlvent(mgs) =    &
     &  ( 0.78*x +    &
     &    0.308*fvent(mgs)*y*(xdia(mgs,lhl,1)**(0.5 + 0.5*bx(lhl)))*   &
     &            Sqrt(ax(lhl)*rhovt(mgs)))


        ENDIF
       ENDIF
      end do
      ENDIF






      do mgs = 1,ngscnt
      fwet1(mgs) =   &
     & (2.0*pi)*   &
     & ( felv(mgs)*fwvdf(mgs)*rho0(mgs)*(qss0(mgs)-qx(mgs,lv))   &
     &  -ftka(mgs)*temcg(mgs) )   &
     & / ( rho0(mgs)*(felf(mgs)+fcw(mgs)*temcg(mgs)) )
      fwet2(mgs) =   &
     &  (1.0)-fci(mgs)*temcg(mgs)   &
     & / ( felf(mgs)+fcw(mgs)*temcg(mgs) )
      end do



      do mgs = 1,ngscnt
      fmlt1(mgs) = (2.0*pi)*   &
     &  ( felv(mgs)*fwvdf(mgs)*(qss0(mgs)-qx(mgs,lv))   &
     &   -ftka(mgs)*temcg(mgs)/rho0(mgs) )    &
     &  / (felf(mgs))
      fmlt2(mgs) = -fcw(mgs)*temcg(mgs)/felf(mgs)
      end do



      do mgs = 1,ngscnt
      fvds(mgs) =    &
     &  (4.0*pi/rho0(mgs))*(ssi(mgs)-1.0)*   &
     &  (1.0/(fai(mgs)+fbi(mgs)))
      end do
      do mgs = 1,ngscnt
      fvce(mgs) =    &
     &  (4.0*pi/rho0(mgs))*(ssw(mgs)-1.0)*   &
     &  (1.0/(fav(mgs)+fbv(mgs)))
      end do




      qsmlr(:) = 0.0
      qimlr(:) = 0.0
      qhmlr(:) = 0.0
      qhlmlr(:) = 0.0
      qhfzh(:) = 0.0
      qhlfzhl(:) = 0.0
      vhfzh(:) = 0.0
      vhlfzhl(:) = 0.0
      qsfzs(:) = 0.0
      zsmlr(:) = 0.0
      zhmlr(:) = 0.0
      zhmlrr(:) = 0.0
      zhshr(:) = 0.0
      zhlmlr(:) = 0.0
      zhlshr(:) = 0.0

      zhshrr(:) = 0.0
      zhlmlrr(:) = 0.0
      zhlshrr(:) = 0.0

      csmlr(:) = 0.0
      chmlr(:) = 0.0
      chmlrr(:) = 0.0
      chlmlr(:) = 0.0
      chlmlrr(:) = 0.0

      if ( .not. mixedphase ) then 
      do mgs = 1,ngscnt

      IF ( temg(mgs) .gt. tfr ) THEN
      
      IF (  qx(mgs,ls) .gt. qxmin(ls) ) THEN
      qsmlr(mgs) =   &
     &   min(   &
     &  (c1sw*fmlt1(mgs)*cx(mgs,ls)*swvent(mgs)*xdia(mgs,ls,1) ) & 
     &   , 0.0 )
      ENDIF
      











      IF (  qx(mgs,lh) .gt. qxmin(lh) ) THEN
      qhmlr(mgs) =   &
     &   min(   &
     &  fmlt1(mgs)*cx(mgs,lh)*hwvent(mgs)*xdia(mgs,lh,1)   &
     &  + fmlt2(mgs)*(qhacr(mgs)+qhacw(mgs))    &
     &   , 0.0 )
     
       IF ( ivhmltsoak > 0 .and. qhmlr(mgs) < 0.0 .and. lvol(lh) > 1 .and. xdn(mgs,lh) .lt. xdnmx(lh) ) THEN
         
           v1 = (1. - xdn(mgs,lh)/xdnmx(lh))*(vx(mgs,lh) + rho0(mgs)*qhmlr(mgs)/xdn(mgs,lh) )/(dtp) 
           v2 = -1.0*rho0(mgs)*qhmlr(mgs)/xdnmx(lh)  
           
           vhsoak(mgs) = Min(v1,v2)
           
       ENDIF

      ENDIF

      
      IF ( lhl .gt. 1  .and. lhlw < 1 ) THEN
       IF ( qx(mgs,lhl) .gt. qxmin(lhl) ) THEN
        qhlmlr(mgs) =   &
     &   min(   &
     &  fmlt1(mgs)*cx(mgs,lhl)*hlvent(mgs)*xdia(mgs,lhl,1)   &
     &  + fmlt2(mgs)*(qhlacr(mgs)+qhlacw(mgs))    &
     &   , 0.0 )

       IF ( ivhmltsoak > 0 .and.  qhlmlr(mgs) < 0.0 .and. lvol(lhl) > 1 .and. xdn(mgs,lhl) .lt. xdnmx(lhl) ) THEN
         
           v1 = (1. - xdn(mgs,lhl)/xdnmx(lhl))*(vx(mgs,lhl) + rho0(mgs)*qhlmlr(mgs)/xdn(mgs,lhl) )/(dtp) 
           v2 = -1.0*rho0(mgs)*qhlmlr(mgs)/xdnmx(lhl)  
           
           vhlsoak(mgs) = Min(v1,v2)
           
       ENDIF
        
        ENDIF
       ENDIF

      ENDIF
      




      if ( .not. mixedphase ) qsmlr(mgs)  = max( qsmlr(mgs),  Min( -qsmxd(mgs), -0.7*qx(mgs,ls)/dtp ) ) 
      if ( .not. mixedphase ) qhmlr(mgs)  = max( qhmlr(mgs),  Min( -qhmxd(mgs), -0.5*qx(mgs,lh)/dtp ) ) 

      qhmlh(mgs)  = 0.


      


      IF ( lhl .gt. 1 .and. lhlw < 1 ) qhlmlr(mgs)  = max( qhlmlr(mgs),  Min( -qxmxd(mgs,lhl), -0.5*qx(mgs,lhl)/dtp ) )


      end do

      endif  

      if ( ipconc .ge. 1 ) then
      do mgs = 1,ngscnt
      cimlr(mgs)  = (cx(mgs,li)/(qx(mgs,li)+1.e-20))*qimlr(mgs)

      IF ( .not. mixedphase ) THEN
      IF ( xdia(mgs,ls,1) .gt. 1.e-6 .and. -qsmlr(mgs) .ge. 0.5*qxmin(ls) .and. ipconc .ge. 4 ) THEN
      csmlr(mgs)  = rho0(mgs)*qsmlr(mgs)/(xv(mgs,ls)*rhosm)
      ELSE
      csmlr(mgs)  = (cx(mgs,ls)/(qx(mgs,ls)+1.e-20))*qsmlr(mgs)
      ENDIF






       chmlr(mgs)  = (cx(mgs,lh)/(qx(mgs,lh)+1.e-20))*qhmlr(mgs)



     IF ( chmlr(mgs) < 0.0 ) THEN
      
      IF ( ihmlt .eq. 1 ) THEN
        chmlrr(mgs)  = Min( chmlr(mgs), rho0(mgs)*qhmlr(mgs)/(xdn(mgs,lr)*vmlt) ) 
      ELSEIF ( ihmlt .eq. 2 ) THEN
        IF ( xv(mgs,lh) .gt. 0.0 .and. chmlr(mgs) .lt. 0.0 ) THEN


          IF(imltshddmr > 0) THEN
            
            
            tmp = -rho0(mgs)*qhmlr(mgs)/(Min(xdn(mgs,lr)*xvmx(lr), xdn(mgs,lh)*xv(mgs,lh))) 
            tmp2 = -rho0(mgs)*qhmlr(mgs)/(xdn(mgs,lr)*vr3mm) 
            chmlrr(mgs) = tmp*(sheddiam0-xdia(mgs,lh,3))/(sheddiam0-sheddiam)+tmp2*(xdia(mgs,lh,3)-sheddiam)/(sheddiam0-sheddiam)
            chmlrr(mgs) = -Max(tmp,Min(tmp2,chmlrr(mgs)))
          ELSE 
            chmlrr(mgs) =  rho0(mgs)*qhmlr(mgs)/(Min(xdn(mgs,lr)*xvmx(lr), xdn(mgs,lh)*xv(mgs,lh)))  
          ENDIF
        ELSE
        chmlrr(mgs) = chmlr(mgs)
        ENDIF
      ELSEIF ( ihmlt .eq. 0 ) THEN
        chmlrr(mgs) = chmlr(mgs)
      ENDIF
      
      ENDIF 

      IF ( lhl .gt. 1 .and. lhlw < 1 .and. .not. mixedphase .and. qhlmlr(mgs) < 0.0 ) THEN 
      




      chlmlr(mgs)  = (cx(mgs,lhl)/(qx(mgs,lhl)+1.e-20))*qhlmlr(mgs)

      
      IF ( ihmlt .eq. 1 ) THEN
        chlmlrr(mgs)  = Min( chlmlr(mgs), rho0(mgs)*qhlmlr(mgs)/(xdn(mgs,lr)*vmlt) ) 
      ELSEIF ( ihmlt .eq. 2 ) THEN
        IF ( xv(mgs,lhl) .gt. 0.0 .and. chlmlr(mgs) .lt. 0.0 ) THEN


          IF(imltshddmr > 0) THEN
            tmp = -rho0(mgs)*qhlmlr(mgs)/(Min(xdn(mgs,lr)*xvmx(lr), xdn(mgs,lhl)*xv(mgs,lhl))) 
            tmp2 = -rho0(mgs)*qhlmlr(mgs)/(xdn(mgs,lr)*vr3mm) 
            chlmlrr(mgs) = tmp*(20.e-3-xdia(mgs,lhl,3))/(20.e-3-sheddiam)+tmp2*(xdia(mgs,lhl,3)-sheddiam)/(20.e-3-sheddiam)
            chlmlrr(mgs) = -Max(tmp,Min(tmp2,chlmlrr(mgs)))
          ELSE
            chlmlrr(mgs) = rho0(mgs)*qhlmlr(mgs)/(Min(xdn(mgs,lr)*xvmx(lr), xdn(mgs,lhl)*xv(mgs,lhl)))  
          ENDIF
        ELSE
        chlmlrr(mgs) = chlmlr(mgs)
        ENDIF
      ELSEIF ( ihmlt .eq. 0 ) THEN
        chlmlrr(mgs) = chlmlr(mgs)
      ENDIF
        
      ENDIF 

      ENDIF 




      end do
      end if




      DO mgs = 1,ngscnt

      rwcap(mgs) = (0.5)*xdia(mgs,lr,1)
      swcap(mgs) = (0.5)*xdia(mgs,ls,1)
      hwcap(mgs) = (0.5)*xdia(mgs,lh,1)
      IF ( lhl .gt. 1 ) hlcap(mgs) = (0.5)*xdia(mgs,lhl,1)

      if ( qx(mgs,li).gt.qxmin(li) .and. xdia(mgs,li,1) .gt. 0.0 ) then



        cilen(mgs)   = 0.4764*(xdia(mgs,li,1))**(0.958)
        cval = xdia(mgs,li,1)
        aval = cilen(mgs)
        eval = Sqrt(1.0-(aval**2)/(cval**2))
        fval = min(0.99,eval)
        gval = alog( abs( (1.+fval)/(1.-fval) ) )
        cicap(mgs) = cval*fval / gval
      ELSE
       cicap(mgs) = 0.0
      end if
      ENDDO


      qhldsv(:) = 0.0

      do mgs = 1,ngscnt
      IF ( icond .eq. 1 .or. temg(mgs) .le. tfrh    &
     &      .or. (qx(mgs,lr) .le. qxmin(lr) .and. qx(mgs,lc) .le. qxmin(lc)) ) THEN
        qidsv(mgs) =   &
     &    fvds(mgs)*cx(mgs,li)*civent(mgs)*cicap(mgs)
        qsdsv(mgs) =   &
     &    fvds(mgs)*cx(mgs,ls)*swvent(mgs)*swcap(mgs)





      ELSE
        qidsv(mgs) = 0.0
        qsdsv(mgs) = 0.0
      ENDIF
        qhdsv(mgs) =   &
     &    fvds(mgs)*cx(mgs,lh)*hwvent(mgs)*hwcap(mgs)

        IF ( lhl .gt. 1 ) qhldsv(mgs) = fvds(mgs)*cx(mgs,lhl)*hlvent(mgs)*hlcap(mgs)


      end do

      do mgs = 1,ngscnt
      IF ( icond .eq. 1 .or. temg(mgs) .le. tfrh    &
     &      .or. (qx(mgs,lr) .le. qxmin(lr) .and. qx(mgs,lc) .le. qxmin(lc)) ) THEN



        qisbv(mgs) = max( min(qidsv(mgs), 0.0),  Min( -qimxd(mgs), -0.7*qx(mgs,li)/dtp ) )
        qssbv(mgs) = max( min(qsdsv(mgs), 0.0),  Min( -qsmxd(mgs), -0.7*qx(mgs,ls)/dtp ) )
        qidpv(mgs) = Max(qidsv(mgs), 0.0)
        qsdpv(mgs) = Max(qsdsv(mgs), 0.0)
      ELSE
        qisbv(mgs) = 0.0
        qssbv(mgs) = 0.0
        qidpv(mgs) = 0.0
        qsdpv(mgs) = 0.0
      ENDIF

      qhsbv(mgs) = max( min(qhdsv(mgs), 0.0), -qhmxd(mgs) )


      qhdpv(mgs) = Max(qhdsv(mgs), 0.0)

      qhlsbv(mgs) = 0.0
      qhldpv(mgs) = 0.0
      IF ( lhl .gt. 1 ) THEN
        qhlsbv(mgs) = max( min(qhldsv(mgs), 0.0), -qxmxd(mgs,lhl) )
        qhldpv(mgs) = Max(qhldsv(mgs), 0.0)
      ENDIF

      temp1 = qidpv(mgs) + qsdpv(mgs) + qhdpv(mgs) + qhldpv(mgs)

      IF ( temp1 .gt. qvimxd(mgs) ) THEN

      frac = qvimxd(mgs)/temp1

      qidpv(mgs) = frac*qidpv(mgs)
      qsdpv(mgs) = frac*qsdpv(mgs)
      qhdpv(mgs) = frac*qhdpv(mgs)
      qhldpv(mgs) = frac*qhldpv(mgs)






      ENDIF

      end do


      if ( ipconc .ge. 1 ) then
      do mgs = 1,ngscnt
      cssbv(mgs)  = (cx(mgs,ls)/(qx(mgs,ls)+1.e-20))*qssbv(mgs)
      cisbv(mgs)  = (cx(mgs,li)/(qx(mgs,li)+1.e-20))*qisbv(mgs)
      chsbv(mgs)  = (cx(mgs,lh)/(qx(mgs,lh)+1.e-20))*qhsbv(mgs)
      IF ( lhl .gt. 1 ) chlsbv(mgs)  = (cx(mgs,lhl)/(qx(mgs,lhl)+1.e-20))*qhlsbv(mgs)
      csdpv(mgs)  = 0.0 
      cidpv(mgs) =  0.0 
      chdpv(mgs)  = 0.0 
      chldpv(mgs) = 0.0
      end do
      end if




      if (ndebug .gt. 0 ) write(0,*) 'conc 29a'
      do mgs = 1,ngscnt
      qscni(mgs) =  0.0
      cscni(mgs) = 0.0
      cscnis(mgs) = 0.0
      if ( ipconc .ge. 4 .and. iscni .ge. 1 .and. qx(mgs,li) .gt. qxmin(li) ) then
        IF ( iscni .eq. 1 ) THEN
         qscni(mgs) =    &
     &      pi*rho0(mgs)*((0.25)/(6.0))   &
     &      *eii(mgs)*(qx(mgs,li)**2)*(xdia(mgs,li,2))   &
     &      *vtxbar(mgs,li,1)/xmas(mgs,li)
         cscni(mgs) = Min(cimxd(mgs),qscni(mgs)*rho0(mgs)/xmas(mgs,li))
         cscnis(mgs) = 0.5*cscni(mgs)
        ELSEIF ( iscni .eq. 2 .or. iscni .eq. 4 ) THEN  
          IF ( qidpv(mgs) .gt. 0.0 .and.  xdia(mgs,li,3) .ge. 100.e-6 ) THEN



              qscni(mgs) = Min(0.5, xdia(mgs,li,3)/200.e-6)*qidpv(mgs)



            cscni(mgs) = 0.5*Min(cimxd(mgs),qscni(mgs)*rho0(mgs)/Max(xdn(mgs,ls)*xvsmn,xmas(mgs,li)))


              cscnis(mgs) = cscni(mgs)



          ENDIF

           IF ( iscni .ne. 4 ) THEN

             tmp = ess(mgs)*rvt*aa2*cx(mgs,li)*cx(mgs,li)*xv(mgs,li)




             qscni(mgs) = qscni(mgs) + Min( qxmxd(mgs,li), 2.0*tmp*xmas(mgs,li)*rhoinv(mgs) )
             cscni(mgs) = cscni(mgs) + Min( cxmxd(mgs,li), 2.0*tmp )
             cscnis(mgs) = cscnis(mgs) + Min( cxmxd(mgs,li), tmp )
           ENDIF
        ELSEIF ( iscni .eq. 3 ) THEN 
           qscni(mgs) = 0.001*eii(mgs)*max((qx(mgs,li)-1.e-3),0.0)
           qscni(mgs) = min(qscni(mgs),qxmxd(mgs,li))
           cscni(mgs) = qscni(mgs)*rho0(mgs)/xmas(mgs,li)
           cscnis(mgs) = 0.5*cscni(mgs)

        ENDIF

      ELSEIF ( ipconc < 4 ) THEN 
           IF ( lwsm6 ) THEN
             qimax = rhoinv(mgs)*roqimax
             qscni(mgs) = Min(0.9d0*qx(mgs,li), Max( 0.d0, (qx(mgs,li) - qimax)*dtpinv ) )
           ELSE
             qscni(mgs) = 0.001*eii(mgs)*max((qx(mgs,li)-1.e-3),0.0)
             qscni(mgs) = min(qscni(mgs),qxmxd(mgs,li))
           ENDIF
      else 
      if ( qx(mgs,li) .gt. qxmin(li) ) then
          qscni(mgs) =    &
     &    pi*rho0(mgs)*((0.25)/(6.0))   &
     &    *eii(mgs)*(qx(mgs,li)**2)*(xdia(mgs,li,2))   &
     &    *vtxbar(mgs,li,1)/xmas(mgs,li)
         cscni(mgs) = Min(cimxd(mgs),qscni(mgs)*rho0(mgs)/xmas(mgs,li))
        end if

      end if
      end do





      do mgs = 1,ngscnt

      qsdry(mgs)  = qsacr(mgs)    + qsacw(mgs)   &
     &            + qsaci(mgs)

      qhdry(mgs)  = qhaci(mgs)    + qhacs(mgs)   &
     &            + qhacr(mgs)   &
     &            + qhacw(mgs)

      qhldry(mgs) = 0.0
      IF ( lhl .gt. 1 ) THEN
      qhldry(mgs)  = qhlaci(mgs)    + qhlacs(mgs)   &
     &               + qhlacr(mgs)   &
     &               + qhlacw(mgs)
      ENDIF
      end do



      do mgs = 1,ngscnt










        qhwet(mgs) =   &
     &    ( xdia(mgs,lh,1)*hwvent(mgs)*cx(mgs,lh)*fwet1(mgs)   &
     &   + fwet2(mgs)*(qhaci(mgs) + qhacs(mgs)) )
       qhwet(mgs) = max( 0.0, qhwet(mgs))


       qhlwet(mgs) = 0.0
       IF ( lhl .gt. 1 ) THEN
       qhlwet(mgs) =   &
     &    ( xdia(mgs,lhl,1)*hlvent(mgs)*cx(mgs,lhl)*fwet1(mgs)   &
     &   + fwet2(mgs)*(qhlaci(mgs) + qhlacs(mgs)) )
       qhlwet(mgs) = max( 0.0, qhlwet(mgs))
       ENDIF



      end do



      qsshr(:)  =  0.0
      qhshr(:)  =  0.0
      qhlshr(:) =  0.0
      qhshh(:)  =  0.0
      csshr(:)  =  0.0
      chshr(:)  =  0.0
      chlshr(:)  =  0.0
      chshrr(:)  =  0.0
      chlshrr(:)  =  0.0
      vhshdr(:)  = 0.0
      vhlshdr(:)  = 0.0
      wetsfc(:)  = .false.
      wetgrowth(:)  = .false.
      wetsfchl(:)  = .false.
      wetgrowthhl(:)  = .false.


      do mgs = 1,ngscnt



      qhshr(mgs)  = Min( 0.0, qhwet(mgs) - qhdry(mgs) )  
      


      qhlshr(mgs)  =  Min( 0.0, qhlwet(mgs) - qhldry(mgs) )




      qsshr(mgs)  =  0.0




      if ( temg(mgs) .lt. 243.15 ) then
       qsshr(mgs)  =  0.0
       qhshr(mgs)  =  0.0
       qhlshr(mgs) =  0.0
       vhshdr(mgs)  = 0.0
       vhlshdr(mgs)  = 0.0
       wetsfc(mgs) = .false.
       wetgrowth(mgs) = .false.
       wetsfchl(mgs) = .false.
       wetgrowthhl(mgs) = .false.
      end if



      if ( temg(mgs) .gt. tfr ) then

       qsshr(mgs)   = -qsdry(mgs)
       qhlshr(mgs)  = -qhldry(mgs)

       qhshr(mgs)  = -qhdry(mgs)
       vhshdr(mgs)  = -vhacw(mgs) - vhacr(mgs)
       vhlshdr(mgs)  = -vhlacw(mgs)
       qhwet(mgs)  = 0.0
       qhlwet(mgs) = 0.0
      end if


        wetsfc(mgs) =  (qhshr(mgs) .lt. 0.0 .and. temg(mgs) < tfr ) .or. ( qhmlr(mgs) < -qxmin(lh) .and.  temg(mgs) > tfr )
        wetgrowth(mgs) = (qhshr(mgs) .lt. 0.0 .and. temg(mgs) < tfr )


      if (qhlshr(mgs) .lt. 0.0 .and. temg(mgs) < tfr ) THEN
        wetsfchl(mgs) = (qhlshr(mgs) .lt. 0.0 .and. temg(mgs) < tfr ) .or. ( qhlmlr(mgs) < -qxmin(lhl) .and.  temg(mgs) > tfr )
        wetgrowthhl(mgs) = (qhlshr(mgs) .lt. 0.0 .and. temg(mgs) < tfr )
      ENDIF

      end do

      if ( ipconc .ge. 1 ) then
      do mgs = 1,ngscnt
      csshr(mgs)  = 0.0 
      chshr(mgs)  = (cx(mgs,lh)/(qx(mgs,lh)+1.e-20))*qhshr(mgs)
      IF ( temg(mgs) < tfr ) THEN
         chshrr(mgs) = Min( chshr(mgs), rho0(mgs)*qhshr(mgs)/(xdn0(lr)*vshd) ) 
      ELSE
        IF(imltshddmr > 0) THEN
          
          
          tmp = -Min( chshr(mgs), rho0(mgs)*qhshr(mgs)/(xdn(mgs,lr)*xvmx(lr)) ) 
          tmp2 = -rho0(mgs)*qhshr(mgs)/(xdn(mgs,lr)*vr3mm) 
          chshrr(mgs) = tmp*(sheddiam0-xdia(mgs,lh,3))/(sheddiam0-sheddiam)+tmp2*(xdia(mgs,lh,3)-sheddiam)/(sheddiam0-sheddiam)
          chshrr(mgs) = -Max(tmp,Min(tmp2,chshrr(mgs)))
        ELSE
         chshrr(mgs) = Min( chshr(mgs), rho0(mgs)*qhshr(mgs)/(xdn(mgs,lr)*Min(vr4p5mm,xvmx(lr))) ) 

        ENDIF
      ENDIF
      chlshr(mgs) = 0.0
      chlshrr(mgs) = 0.0
      IF ( lhl .gt. 1 ) THEN 
         chlshr(mgs)  = (cx(mgs,lhl)/(qx(mgs,lhl)+1.e-20))*qhlshr(mgs)
        IF ( temg(mgs) < tfr ) THEN
          chlshrr(mgs) = Min( chlshr(mgs), rho0(mgs)*qhlshr(mgs)/(xdn0(lr)*vshd) ) 

        ELSE
          IF(imltshddmr > 0) THEN
            
            
            tmp = -Min( chlshr(mgs), rho0(mgs)*qhlshr(mgs)/(xdn(mgs,lr)*xvmx(lr)) ) 
            tmp2 = -rho0(mgs)*qhlshr(mgs)/(xdn(mgs,lr)*vr3mm) 
            chlshrr(mgs) = tmp*(sheddiam0-xdia(mgs,lhl,3))/(sheddiam0-sheddiam)+tmp2*(xdia(mgs,lhl,3)-sheddiam)/(sheddiam0-sheddiam)
            chlshrr(mgs) = -Max(tmp,Min(tmp2,chlshrr(mgs)))
          ELSE
           chlshrr(mgs) = Min( chlshr(mgs), rho0(mgs)*qhlshr(mgs)/(xdn(mgs,lr)*Min(vr4p5mm,xvmx(lr))) ) 

          ENDIF
        ENDIF
      ENDIF
      end do
      end if





      do mgs = 1,ngscnt



      if ( qsshr(mgs) .lt. 0.0 ) then
      qsdpv(mgs) = 0.0
      qssbv(mgs) = 0.0
      else
      qsshr(mgs) = 0.0
      end if










      if (mixedphase) then
        qsshr(mgs) = 0.0
        qhshr(mgs) = 0.0
        csshr(mgs) = 0.0
        chshr(mgs) = 0.0
        chshrr(mgs) = 0.0
        vhshdr(mgs) = 0.0
        IF ( lhlw > 1 ) THEN
          qhlshr(mgs) = 0.0
          vhlshdr(mgs) = 0.0
          chlshr(mgs) = 0.0
          chlshrr(mgs) = 0.0
        ENDIF
      end if




      if ( wetgrowth(mgs) .or. (mixedphase .and. fhw(mgs) .gt. 0.05 .and. temg(mgs) .gt. 243.15) ) then
      



        IF ( lvol(lh) .gt. 1 .and. .not. mixedphase) THEN
        
         rimdn(mgs,lh) = xdnmx(lh)
         raindn(mgs,lh) = xdnmx(lh)
         vhacw(mgs) = qhacw(mgs)*rho0(mgs)/rimdn(mgs,lh)
         vhacr(mgs) = qhacr(mgs)*rho0(mgs)/raindn(mgs,lh)

         IF ( xdn(mgs,lh) .lt. xdnmx(lh) ) THEN
         

           v1 = (1. - xdn(mgs,lh)/xdnmx(lh))*vx(mgs,lh)/(dtp) 

           v2 = rho0(mgs)*qhwet(mgs)/xdnmx(lh)  
           
           vhsoak(mgs) = Min(v1,v2)
           
         ENDIF

         vhshdr(mgs) = Min(0.0, rho0(mgs)*qhwet(mgs)/xdnmx(lh) - vhacw(mgs) - vhacr(mgs) )
         
        ELSEIF ( lvol(lh) .gt. 1  .and. mixedphase ) THEN


        ENDIF
        

      qhdpv(mgs) = 0.0

      chdpv(mgs) = 0.0




      IF ( ehi(mgs) .gt. 0.0 ) THEN
        qhaci(mgs) = Min(qimxd(mgs),qhaci(mgs)/ehi(mgs))  
      ENDIF
      IF ( ehs(mgs) .gt. 0.0 ) THEN

        qhacs(mgs) = qhacs(mgs)/ehs(mgs)                   
        ehs(mgs) = min(ehsfrac*ehs(mgs),ehsmax)            
        qhacs(mgs) = Min(qsmxd(mgs),qhacs(mgs)*ehs(mgs))   
      ENDIF


      wetsfc(mgs) = .true.

      else

      end if





      if ( wetgrowthhl(mgs) .or. (mixedphase .and. fhlw(mgs) .gt. 0.05 .and. temg(mgs) .gt. 243.15) ) then

       

      qhldpv(mgs) = 0.0

      chldpv(mgs) = 0.0





        IF ( lvol(lhl) .gt. 1  .and. .not. mixedphase ) THEN


         rimdn(mgs,lhl) = xdnmx(lhl) 
         raindn(mgs,lhl) = xdnmx(lhl) 
         vhlacw(mgs) = qhlacw(mgs)*rho0(mgs)/rimdn(mgs,lhl)
         vhlacr(mgs) = qhlacr(mgs)*rho0(mgs)/raindn(mgs,lhl)

         IF ( xdn(mgs,lhl) .lt. xdnmx(lhl) ) THEN
         

           v1 = (1. - xdn(mgs,lhl)/xdnmx(lhl))*vx(mgs,lhl)/(dtp) 

           v2 = rho0(mgs)*qhlwet(mgs)/xdnmx(lhl)  
           IF ( v1 > v2 ) THEN 
             vhlsoak(mgs) = v2
           ELSE  
             vhlsoak(mgs) = v1
           ENDIF


         ELSE
           vhlsoak(mgs) = 0.0


         
         ENDIF

         vhlshdr(mgs) = Min(0.0, rho0(mgs)*qhlwet(mgs)/xdnmx(lhl) - vhlacw(mgs) - vhlacr(mgs) )


        ELSEIF ( lvol(lhl) .gt. 1  .and. mixedphase ) THEN


        ENDIF

      IF ( ehli(mgs) .gt. 0.0 ) THEN
        qhlaci(mgs) = Min(qimxd(mgs),qhlaci(mgs)/ehli(mgs))
      ENDIF
      IF ( ehls(mgs) .gt. 0.0 ) THEN
        qhlacs(mgs) = Min(qsmxd(mgs),qhlacs(mgs)/ehls(mgs))
      ENDIF
      



      wetsfchl(mgs) = .true.


      else


      end if


      end do



      DO mgs = 1,ngscnt
      
      qhcni(mgs) = 0.0
      chcni(mgs) = 0.0
      chcnih(mgs) = 0.0
      vhcni(mgs) = 0.0
      
      IF ( iglcnvi .ge. 1 ) THEN
      IF ( temg(mgs) .lt. 273.0 .and. qiacw(mgs) - qidpv(mgs) .gt. 0.0 ) THEN
      
        
        tmp = rimc1*(-((0.5)*(1.e+06)*xdia(mgs,lc,1))   &
     &                *((0.60)*vtxbar(mgs,li,1))   &
     &                /(temg(mgs)-273.15))**(rimc2)
        tmp = Min( Max( rimc3, tmp ), 900.0 )
        
        
        
        
        

        
        IF ( tmp .ge. 200.0 .or. iglcnvi >= 2 ) THEN
          r = Max( 0.5*(xdn(mgs,li) + tmp), xdnmn(lh) )

          qhcni(mgs) = (qiacw(mgs) - qidpv(mgs)) 
          chcni(mgs) = cx(mgs,li)*qhcni(mgs)/qx(mgs,li)

          chcnih(mgs) = Min(chcni(mgs), rho0(mgs)*qhcni(mgs)/(r*xvmn(lh)) )

          vhcni(mgs) = rho0(mgs)*qhcni(mgs)/r
        ENDIF
      
      ENDIF

      
      ENDIF
      
      
      ENDDO
      
      
      qhlcnh(:) = 0.0
      chlcnh(:) = 0.0
      vhlcnh(:) = 0.0
      vhlcnhl(:) = 0.0
      zhlcnh(:) = 0.0

      qhcnhl(:) = 0.0
      chcnhl(:) = 0.0
      vhcnhl(:) = 0.0
      zhcnhl(:) = 0.0
      

      IF ( lhl .gt. 1  ) THEN
      
      IF ( ihlcnh == 1 ) THEN




      DO mgs = 1,ngscnt




        IF (  wetgrowth(mgs) .and. (xdn(mgs,lh) .gt. hldnmn .or. lvh < 1 ) .and.  & 

     &        rimdn(mgs,lh) .gt. 800. .and.   &
     &        xdia(mgs,lh,3) .gt. hlcnhdia .and. qx(mgs,lh) .gt. hlcnhqmin ) THEN


        IF ( qhacw(mgs) .gt. 0.0 .and. qhacw(mgs) .gt. qhaci(mgs) .and. temg(mgs) .le. tfr-2.0 ) THEN
        


          x = (1.1e4*(rho0(mgs)*qx(mgs,lc)) - 1.3e3*rho0(mgs)*qx(mgs,li) + 1.0e-3 )
          IF ( x > 1.e-20 ) THEN
          arg = Min(70.0, (-temcg(mgs)/x )) 
          dh0 = 0.01*(exp(arg) - 1.0)
          ELSE
           dh0 = 1.e30
          ENDIF

          


         IF ( xdia(mgs,lh,3)/dh0 .gt. 0.1 ) THEN 

           tmp = qhacw(mgs) + qhacr(mgs) + qhaci(mgs) + qhacs(mgs)

           qtmp = Min( 100.0, xdia(mgs,lh,3)/(2.0*dh0) )*(tmp)
           IF ( .false. .and. qx(mgs,lhl) + qtmp*dtp .lt. 0.5e-3 ) THEN
             hdia1 = Max(dh0, xdia(mgs,lh,3) )
            qtmp = qtmp + Min(qxmxd(mgs,lh), Max( 0.0,   &
     &      ((pi*xdn(mgs,lh)*cx(mgs,lh)) / (6.0*rho0(mgs)*dtp))   &
     &      *exp(-hdia1/xdia(mgs,lh,1))   &
     &      *( (hdia1**3) + 3.0*(hdia1**2)*xdia(mgs,lh,1)   &
     &      + 6.0*(hdia1)*(xdia(mgs,lh,1)**2) + 6.0*(xdia(mgs,lh,1)**3) ) ) )



           ENDIF



           qhlcnh(mgs) = Min(  qxmxd(mgs,lh), qtmp )
           
           IF ( ipconc .ge. 5 ) THEN

           dh0 = Min( dh0, 10.e-3 ) 

           chlcnh(mgs) = Min( cxmxd(mgs,lh), rho0(mgs)*qhlcnh(mgs)/(pi*xdn(mgs,lh)*dh0**3/6.0) )


           r = rho0(mgs)*qhlcnh(mgs)/(xdn(mgs,lh)*xv(mgs,lh))  


           chlcnh(mgs) = Max( chlcnh(mgs), r )

           ENDIF
           
           vhlcnh(mgs) = rho0(mgs)*qhlcnh(mgs)/xdn(mgs,lh)
           vhlcnhl(mgs) = rho0(mgs)*qhlcnh(mgs)/Max(xdnmn(lhl), xdn(mgs,lh))

          ENDIF


        ENDIF
        ENDIF
      
      ENDDO
      
      ELSEIF ( ihlcnh == 2 ) THEN 





      do mgs = 1,ngscnt


      if ( wetgrowth(mgs) .and. temg(mgs) .lt. tfr-5. .and. qx(mgs,lh) > qxmin(lh) ) then
      if ( qhacw(mgs).gt.1.e-6 .and. xdn(mgs,lh) > 700. ) then
      qhlcnh(mgs) =                                                   &
        ((pi*xdn(mgs,lh)*cx(mgs,lh)) / (6.0*rho0(mgs)*dtp))           &
       *exp(-hldia1/xdia(mgs,lh,1))                                    &
       *( (hldia1**3) + 3.0*(hldia1**2)*xdia(mgs,lh,1)                  &
        + 6.0*(hldia1)*(xdia(mgs,lh,1)**2) + 6.0*(xdia(mgs,lh,1)**3) )
      qhlcnh(mgs) =   min(qhlcnh(mgs),qhmxd(mgs))
      IF ( ipconc .ge. 5 ) THEN
        chlcnh(mgs) = Min( cxmxd(mgs,lh), cx(mgs,lh)*Exp(-hldia1/xdia(mgs,lh,1)))

      ENDIF
           vhlcnh(mgs) = rho0(mgs)*qhlcnh(mgs)/xdn(mgs,lh)
           vhlcnhl(mgs) = rho0(mgs)*qhlcnh(mgs)/Max(xdnmn(lhl), xdn(mgs,lh))
      end if
      end if
      end do

      ENDIF

      ENDIF 





      DO mgs = 1,ngscnt

      qhcns(mgs) = 0.0
      chcns(mgs) = 0.0
      chcnsh(mgs) = 0.0
      vhcns(mgs) = 0.0

      IF ( ipconc .ge. 5 ) THEN

        IF ( qx(mgs,ls) .gt. qxmin(ls) .and. qsacw(mgs) .gt. 0.0 ) THEN


















        IF ( iglcnvs .eq. 1 ) THEN  

        dnnet = cscnvis(mgs) + cscnis(mgs) - csacs(mgs)
        dqnet = qscnvi(mgs) + qscni(mgs) + qsacw(mgs) + qsdpv(mgs) + qssbv(mgs)

        a3 = 1./(rho0(mgs)*qx(mgs,ls))
        a1 = Exp( - xdn(mgs,ls)*cx(mgs,ls)*vgra*a3 )  

        a2 =  (1.-(cx(mgs,ls)*vgra*xdn(mgs,ls)*a3))*dnnet

        a4 = cx(mgs,ls)**2*vgra*xdn(mgs,ls)*a3/qx(mgs,ls)*dqnet

        chcns(mgs) = Max( 0.0, a1*(a2 + a4) )
        chcns(mgs) = Min( chcns(mgs), cxmxd(mgs,ls) )
        chcnsh(mgs) = chcns(mgs)

        qhcns(mgs) = Min( xdn(mgs,ls)*vgra*rhoinv(mgs)*chcns(mgs), qxmxd(mgs,ls) )
        vhcns(mgs) = rho0(mgs)*qhcns(mgs)/Max(xdn(mgs,ls),xdnmn(lh))


        ELSEIF ( iglcnvs .ge. 2  ) THEN  

          IF ( temg(mgs) .lt. 273.0 .and. qsacw(mgs) - qsdpv(mgs) .gt. 0.0 ) THEN


        tmp = rimc1*(-((0.5)*(1.e+06)*xdia(mgs,lc,1))   &
     &                *((0.60)*vtxbar(mgs,ls,1))   &
     &                /(temg(mgs)-273.15))**(rimc2)
        tmp = Min( Max( rimc3, tmp ), 900.0 )

        
        
        



        IF ( tmp .ge. 200.0 .or. iglcnvs >= 3 ) THEN
          r = Max( 0.5*(xdn(mgs,ls) + tmp), xdnmn(lh) )

          qhcns(mgs) = (qsacw(mgs) - qsdpv(mgs))
          chcns(mgs) = cx(mgs,ls)*qhcns(mgs)/qx(mgs,ls)

          chcnsh(mgs) = Min(chcns(mgs), rho0(mgs)*qhcns(mgs)/(r*xvmn(lh)) )

          vhcns(mgs) = rho0(mgs)*qhcns(mgs)/r
        ENDIF

      ENDIF

        ENDIF


        ENDIF

       ELSE 

        qhcns(mgs) = 0.001*ehscnv(mgs)*max((qx(mgs,ls)-6.e-4),0.0)
        qhcns(mgs) = min(qhcns(mgs),qxmxd(mgs,ls))
        IF ( lvol(lh) .ge. 1 ) vhcns(mgs) = rho0(mgs)*qhcns(mgs)/Max(xdn(mgs,ls),400.)

       ENDIF
      ENDDO






      if ( irwfrz .gt. 0 .and. .not. mixedphase) then

      do mgs = 1,ngscnt



      qrztot(mgs) = qrfrz(mgs) + qiacr(mgs) + qsacr(mgs)




      qrzmax(mgs) =   &
     &  ( xdia(mgs,lr,1)*rwvent(mgs)*cx(mgs,lr)*fwet1(mgs) )
      qrzmax(mgs) = max(qrzmax(mgs), 0.0)
      qrzmax(mgs) = min(qrztot(mgs), qrzmax(mgs))
      qrzmax(mgs) = min(qx(mgs,lr)/dtp, qrzmax(mgs))

      IF ( temcg(mgs) < -30. ) THEN 
        qrzmax(mgs) = qx(mgs,lr)/dtp
      ENDIF





      IF ( qrztot(mgs) .gt. qrzmax(mgs) .and. qrztot(mgs) .gt. qxmin(lr) ) THEN
        qrzfac(mgs) = qrzmax(mgs)/(qrztot(mgs))
      ELSE
        qrzfac(mgs) = 1.0
      ENDIF
      qrzfac(mgs) = min(1.0, qrzfac(mgs))

      end do





      do mgs = 1,ngscnt
      if ( temg(mgs) .le. 273.15 .and. qrzfac(mgs) .lt. 1.0 ) then
      qrfrz(mgs)   = qrzfac(mgs)*qrfrz(mgs)
      qrfrzs(mgs)  = qrzfac(mgs)*qrfrzs(mgs)
      qrfrzf(mgs)  = qrzfac(mgs)*qrfrzf(mgs)
      qiacr(mgs)   = qrzfac(mgs)*qiacr(mgs)
      qsacr(mgs)   = qrzfac(mgs)*qsacr(mgs)
      qiacrf(mgs)  = qrzfac(mgs)*qiacrf(mgs)
      crfrz(mgs)   = qrzfac(mgs)*crfrz(mgs)
      crfrzf(mgs)  = qrzfac(mgs)*crfrzf(mgs)
      crfrzs(mgs)  = qrzfac(mgs)*crfrzs(mgs)
      ciacr(mgs)   = qrzfac(mgs)*ciacr(mgs)
      ciacrf(mgs)  = qrzfac(mgs)*ciacrf(mgs)

      
       vrfrzf(mgs)  = qrzfac(mgs)*vrfrzf(mgs)
       viacrf(mgs)  = qrzfac(mgs)*viacrf(mgs)
      end if
      end do



      end if







      qrcev(:) = 0.0
      crcev(:) = 0.0


      do mgs = 1,ngscnt

      IF ( qx(mgs,lr) .gt. qxmin(lr) ) THEN

      qrcev(mgs) =   &
     &  fvce(mgs)*cx(mgs,lr)*rwvent(mgs)*rwcap(mgs)

      IF ( rcond .eq. 1 ) THEN
        qrcev(mgs) = min(qrcev(mgs), qxmxd(mgs,lv))

      ELSE
        qrcev(mgs) = min(qrcev(mgs), 0.0)
      ENDIF

      qrcev(mgs) = max(qrcev(mgs), -qrmxd(mgs))

      IF ( qrcev(mgs) .lt. 0. .and. lnr > 1 ) THEN


      crcev(mgs) = (cx(mgs,lr)/(qx(mgs,lr)))*qrcev(mgs)
      ELSE
         crcev(mgs) = 0.0
      ENDIF


      ENDIF

      end do



      qscev(:) = 0.0
      cscev(:) = 0.0
      qhcev(:) = 0.0
      chcev(:) = 0.0
      qhlcev(:) = 0.0
      chlcev(:) = 0.0







 
      chmul1(:) =  0.0
      chlmul1(:) =  0.0
      csmul1(:) = 0.0

      qhmul1(:) =  0.0
      qhlmul1(:) =  0.0
      qsmul1(:) =  0.0

      do mgs = 1,ngscnt
 
       ltest =  qx(mgs,lh) .gt. qxmin(lh)
       IF ( lhl > 1 )  ltest =  ltest .or. qx(mgs,lhl) .gt. qxmin(lhl)
       
      IF ( (itype1 .ge. 1 .or. itype2 .ge. 1 )   &
     &              .and. qx(mgs,lc) .gt. qxmin(lc)) THEN
      if ( temg(mgs) .ge. 265.15 .and. temg(mgs) .le. 271.15 ) then
       IF ( ipconc .ge. 2 ) THEN
        IF ( xv(mgs,lc) .gt. 0.0     &
     &     .and.  ltest &

     &       ) THEN



         ex1 = (1./250.)*Exp(-7.23e-15/xv(mgs,lc))
       IF ( itype2 .le. 2 ) THEN
         ft = Max(0.0,Min(1.0,-0.11*temcg(mgs)**2 - 1.1*temcg(mgs)-1.7))
       ELSE
        IF ( temg(mgs) .ge. 265.15 .and. temg(mgs) .le. 267.15 ) THEN
          ft = 0.5
        ELSEIF (temg(mgs) .ge. 267.15 .and. temg(mgs) .le. 269.15 ) THEN
          ft = 1.0
        ELSEIF (temg(mgs) .ge. 269.15 .and. temg(mgs) .le. 271.15 ) THEN
          ft = 0.5
        ELSE 
          ft = 0.0
        ENDIF
       ENDIF


        
       IF ( ft > 0.0 ) THEN
        
        IF ( itype2 > 0 ) THEN
         IF ( qx(mgs,lh) .gt. qxmin(lh) .and. (.not. wetsfc(mgs))  ) THEN
          chmul1(mgs) = (ft*ex1*chacw(mgs))
          qhmul1(mgs) = cimas0*chmul1(mgs)*rhoinv(mgs)
         ENDIF
         IF ( lhl .gt. 1 ) THEN
           IF ( qx(mgs,lhl) .gt. qxmin(lhl) .and. (.not. wetsfchl(mgs))  ) THEN
            chlmul1(mgs) = (ft*ex1*chlacw(mgs))
            qhlmul1(mgs) = cimas0*chlmul1(mgs)*rhoinv(mgs)
           ENDIF
         ENDIF
        ENDIF 

        IF ( itype1 > 0 ) THEN
         IF ( qx(mgs,lh) .gt. qxmin(lh) .and. (.not. wetsfc(mgs))  ) THEN
          tmp = ft*(3.5e+08)*rho0(mgs)*qhacw(mgs)
          chmul1(mgs) = chmul1(mgs) + tmp
          qhmul1(mgs) = qhmul1(mgs) + cimas0*tmp*rhoinv(mgs)
         ENDIF
         IF ( lhl .gt. 1 ) THEN
           IF ( qx(mgs,lhl) .gt. qxmin(lhl) .and. (.not. wetsfchl(mgs)) ) THEN
            tmp = ft*(3.5e+08)*rho0(mgs)*qhlacw(mgs)
            chlmul1(mgs) = chlmul1(mgs) + tmp
            qhlmul1(mgs) = qhlmul1(mgs) + cimas0*tmp*rhoinv(mgs)
           ENDIF
         ENDIF
        ENDIF 
        
        ENDIF 

        ENDIF 

       ELSE 



      fimt1(mgs) = 0.0



      if ( temg(mgs) .ge. 268.15 .and. temg(mgs) .le. 270.15 ) then
        fimt1(mgs) = 1.0 -(temg(mgs)-268.15)/2.0
      elseif (temg(mgs) .le. 268.15 .and. temg(mgs) .ge. 265.15 ) then
        fimt1(mgs) = 1.0 +(temg(mgs)-268.15)/3.0
      ELSE 
        fimt1(mgs) = 0.0
      end if



      if ( temg(mgs) .ge. 265.15 .and. temg(mgs) .le. 267.15 ) then
        fimt1(mgs) = 0.5
      elseif (temg(mgs) .ge. 267.15 .and. temg(mgs) .le. 269.15 ) then
        fimt1(mgs) = 1.0
      elseif (temg(mgs) .ge. 269.15 .and. temg(mgs) .le. 271.15 ) then
        fimt1(mgs) = 0.5
      ELSE 
        fimt1(mgs) = 0.0
      end if






      IF ( itype1 .ge. 1 ) THEN
       fimta(mgs) = (3.5e+08)*rho0(mgs)
      ELSE
       fimta(mgs) = 0.0
      ENDIF








      fimt2(mgs) = 0.0
      xcwmas = xmas(mgs,lc) * 1000.

      IF ( itype2 .ge. 1 ) THEN
      if ( xcwmas.lt.1.26e-9 ) then
        fimt2(mgs) = 0.0
      end if
      if ( xcwmas .le. 3.55e-9 .and. xcwmas .ge. 1.26e-9 ) then
        fimt2(mgs) = (2.27)*alog(xcwmas) + 13.39
      end if
      if ( xcwmas .gt. 3.55e-9 ) then
        fimt2(mgs) = 1.0
      end if

      fimt2(mgs) = min(fimt2(mgs),1.0)
      fimt2(mgs) = max(fimt2(mgs),0.0)
      
      ENDIF











      IF ( .not. wetsfc(mgs) ) THEN
      chmul1(mgs) =  fimt1(mgs)*(fimta(mgs) +   &
     &                           (4.0e-03)*fimt2(mgs))*qhacw(mgs)
      ENDIF

      qhmul1(mgs) =  chmul1(mgs)*(cimas0/rho0(mgs))


         IF ( lhl .gt. 1 ) THEN
           IF ( qx(mgs,lhl) .gt. qxmin(lhl) .and. (.not. wetsfchl(mgs)) ) THEN
            tmp = fimt1(mgs)*(fimta(mgs) +   &
     &                           (4.0e-03)*fimt2(mgs))*qhlacw(mgs)
            chlmul1(mgs) =  tmp
            qhlmul1(mgs) = cimas0*tmp*rhoinv(mgs)
           ENDIF
         ENDIF



      ENDIF 
      
      end if 
      
      ENDIF 

      end do












      csmul(:) = 0.0
      qsmul(:) = 0.0
      
      IF ( isnwfrac /= 0 ) THEN
      do mgs = 1,ngscnt
       IF (temg(mgs) .gt. 265.0) THEN 
        if (xdia(mgs,ls,1) .gt. 100.e-6 .and. xdia(mgs,ls,1) .lt. 2.0e-3) then  

        tmp = rhoinv(mgs)*pi*xdn(mgs,ls)*cx(mgs,ls)*(500.e-6)**3
        qsmul(mgs) = Max( kfrag*( qx(mgs,ls) - tmp ) , 0.0 )

        qsmul(mgs) = Min( qxmxd(mgs,li), qsmul(mgs) )
        csmul(mgs) = Min( cxmxd(mgs,li), rho0(mgs)*qsmul(mgs)/mfrag )

        endif
       ENDIF 
      enddo
      ENDIF










      do mgs = 1,ngscnt
      qracif(mgs) = qraci(mgs)
      cracif(mgs) = craci(mgs)

      end do























      cmassin = cimasn  
      do mgs = 1,ngscnt
      qiint(mgs) = 0.0
      ciint(mgs) = 0.0
      qicicnt(mgs) = 0.0
      cicint(mgs) = 0.0
      qipipnt(mgs) = 0.0
      cipint(mgs) = 0.0
      IF ( icenucopt == 1 ) THEN
      if ( ( temg(mgs) .lt. 268.15 .or.  &

     & ( imeyers5 .and. temg(mgs) .lt.  272.0 .and. temgkm2(mgs) .lt. tfr) ) .and.    &
     &    ciintmx .gt. (cx(mgs,li))  &

     &     ) then
       IF ( ipconc >= 4 .or. .not. lwsm6 ) THEN
      
      fiinit(mgs) = (felv(mgs)**2)/(cp*rw)
      dqisdt(mgs) = (qx(mgs,lv)-qis(mgs))/   &
     &  (1.0 + fiinit(mgs)*qis(mgs)/tsqr(mgs))

      idqis = 0
      if ( ssi(mgs) .gt. 1.0 ) THEN
      idqis = 1 
      dzfacp = max( float(kgsp(mgs)-kgs(mgs)), 0.0 )
      dzfacm = max( float(kgs(mgs)-kgsm(mgs)), 0.0 )
      qiint(mgs) =   &
     &  idqis*il5(mgs)   &
     &  *(cmassin/rho0(mgs))   &
     &  *max(0.0,wvel(mgs))   &
     &  *max((cninp(mgs)-cninm(mgs)),0.0)/gz(igs(mgs),jgs,kgs(mgs))   &
     &  /((dzfacp+dzfacm))

      qiint(mgs) = min(qiint(mgs), max(0.25*dqisdt(mgs),0.0)) 
      ciint(mgs) = qiint(mgs)*rho0(mgs)/cmassin
      
       ELSE 

        IF ( ssi(mgs) .gt. 1.0 ) THEN
          xni0 = 1.e3*exp(0.1*temcg(mgs))
          roqi0 = 4.92e-11*xni0**1.33

          qiint(mgs) = Max(0.0d0,dble(roqi0*rhoinv(mgs) - Max(qx(mgs,li),0.))*dtpinv)

        ENDIF
       ENDIF

      ENDIF





      IF ( ciint(mgs) .gt. (ciintmx - (cx(mgs,li)))) THEN
        ciint(mgs) = Max(0.0, ciintmx - (cx(mgs,li)) )
        qiint(mgs) = ciint(mgs)*cmassin/rho0(mgs)
      ENDIF

      end if
      ELSEIF ( icenucopt == 2 ) THEN


      
        IF ( ( temg(mgs) .lt. 268.15 .and. ssw(mgs) > 0.999 ) .or. ssi(mgs) > 1.05 ) THEN
          ciint(mgs) = Max( 0.0, cnina(mgs) - cina(mgs) )
          qiint(mgs) = ciint(mgs)*cmassin/rho0(mgs)

          fiinit(mgs) = (felv(mgs)**2)/(cp*rw)
          dqisdt(mgs) = (qx(mgs,lv)-qis(mgs))/(1.0 + fiinit(mgs)*qis(mgs)/tsqr(mgs))
          qiint(mgs) = min(qiint(mgs), max(0.25*dqisdt(mgs),0.0))
          ciint(mgs) = qiint(mgs)*rho0(mgs)/cmassin


        ENDIF
      
      
      
      ELSEIF ( icenucopt == 3 ) THEN
        IF (  temg(mgs) .lt. 268.15 ) THEN
          ciint(mgs) = Max( 0.0, cnina(mgs) - cina(mgs) )
          qiint(mgs) = ciint(mgs)*cmassin/rho0(mgs)
        ENDIF

      ENDIF

      if ( xplate(mgs) .eq. 1 ) then
      qipipnt(mgs) = qiint(mgs)
      cipint(mgs) = ciint(mgs)
      end if

      if ( xcolmn(mgs) .eq. 1 ) then
      qicicnt(mgs) = qiint(mgs)
      cicint(mgs) = ciint(mgs)
      end if




      end do






      if (ndebug .gt. 0 ) write(0,*) 'dbg = 8'


      if (ndebug .gt. 0 ) write(0,*) 'Collection: set 3-component'


















      do mgs = 1,ngscnt
      qrshr(mgs) = 0.0
      qsshrp(mgs) = 0.0
      qhshrp(mgs) = 0.0
      end do





      do mgs = 1,ngscnt
      qrshr(mgs) = qsshr(mgs) + qhshr(mgs) + qhlshr(mgs)
      crshr(mgs) = chshrr(mgs)/rzxh(mgs) + chlshrr(mgs)/rzxhl(mgs)
      IF ( ipconc .ge. 3 ) THEN

      ENDIF
      end do 








      IF ( ipconc .ge. 1 ) THEN








       pccwi(:) = 0.0
       pccwd(:) = 0.0
       pccii(:) = 0.0
       pccid(:) = 0.0
       pcrwi(:) = 0.0
       pcrwd(:) = 0.0
       pcswi(:) = 0.0
       pcswd(:) = 0.0
       pchwi(:) = 0.0
       pchwd(:) = 0.0
       pchli(:) = 0.0
       pchld(:) = 0.0






      IF ( warmonly < 0.5 ) THEN
      do mgs = 1,ngscnt
      pccii(mgs) =   &
     &   il5(mgs)*cicint(mgs) &


     &  +il5(mgs)*(cwfrzc(mgs)+cwctfzc(mgs)   &
     &  +cicichr(mgs))   &
     &  +chmul1(mgs)   &
     &  +chlmul1(mgs)    &
     &  + csplinter(mgs) + csplinter2(mgs)   &

     &  +csmul(mgs)
      pccid(mgs) =   &
     &   il5(mgs)*(-cscni(mgs) - cscnvi(mgs) & 
     &  -craci(mgs)    &
     &  -csaci(mgs)   &
     &  -chaci(mgs) - chlaci(mgs)   &
     &  -chcni(mgs))   &
     &  +il5(mgs)*cisbv(mgs)   &
     &  -(1.-il5(mgs))*cimlr(mgs)
      end do
      ELSEIF ( warmonly < 0.8 ) THEN
      do mgs = 1,ngscnt
      



      
      pccii(mgs) =   &
     &   il5(mgs)*cicint(mgs)  &
     &  +il5(mgs)*(cwfrzc(mgs)+cwctfzc(mgs)   &
     &  +cicichr(mgs))   &
     &  +chmul1(mgs)   &
     &  +chlmul1(mgs)    &
     &  + csplinter(mgs) + csplinter2(mgs)   &
     &  +csmul(mgs)
      pccid(mgs) =   &





     &  +il5(mgs)*cisbv(mgs)   &
     &  -(1.-il5(mgs))*cimlr(mgs)
      end do
      ENDIF 

      




      IF ( ipconc .ge. 2 ) THEN
      
      do mgs = 1,ngscnt
      pccwi(mgs) =  (0.0) 
      
      IF ( warmonly < 0.5 ) THEN
      pccwd(mgs) =    &
     &  - cautn(mgs) +   &
     &  il5(mgs)*(-ciacw(mgs)-cwfrzp(mgs)-cwctfzp(mgs)   &
     &  -cwfrzc(mgs)-cwctfzc(mgs)   &
     &   )   &
     &  -cracw(mgs) -csacw(mgs)  -chacw(mgs) - chlacw(mgs)
      ELSEIF ( warmonly < 0.8 ) THEN
      pccwd(mgs) =    &
     &  - cautn(mgs) +   &
     &  il5(mgs)*(  &
     & -ciacw(mgs)-cwfrzp(mgs)-cwctfzp(mgs)   &
     &  -cwfrzc(mgs)-cwctfzc(mgs)   &
     &   )   &
     &  -cracw(mgs) -chacw(mgs) -chlacw(mgs) 
      ELSE
      







       
       




      pccwd(mgs) =    &
     &  - cautn(mgs) -cracw(mgs)
      ENDIF


      IF ( -pccwd(mgs)*dtp .gt. cx(mgs,lc) ) THEN






       frac = -cx(mgs,lc)/(pccwd(mgs)*dtp)
       pccwd(mgs) = -cx(mgs,lc)/dtp

        ciacw(mgs)   = frac*ciacw(mgs)
        cwfrzp(mgs)  = frac*cwfrzp(mgs)
        cwctfzp(mgs) = frac*cwctfzp(mgs)
        cwfrzc(mgs)  = frac*cwfrzc(mgs)
        cwctfzc(mgs) = frac*cwctfzc(mgs)
        cracw(mgs)   = frac*cracw(mgs)
        csacw(mgs)   = frac*csacw(mgs)
        chacw(mgs)   = frac*chacw(mgs)
        cautn(mgs)   = frac*cautn(mgs)
       
        pccii(mgs) = pccii(mgs) - (1.-frac)*il5(mgs)*(cwfrzc(mgs)+cwctfzc(mgs))
        IF ( lhl .gt. 1 ) chlacw(mgs)   = frac*chlacw(mgs)


      ENDIF

      end do

      ENDIF 




      IF ( ipconc .ge. 3 ) THEN

      do mgs = 1,ngscnt

      IF ( warmonly < 0.5 ) THEN
      pcrwi(mgs) = &

     &   crcnw(mgs)   &
     &  +(1-il5(mgs))*(   &
     &    -chmlrr(mgs)/rzxh(mgs)   &
     &    -chlmlrr(mgs)/rzxhl(mgs)   &
     &    -csmlr(mgs)     &
     &   - cimlr(mgs) )   &
     &  -crshr(mgs)             
      pcrwd(mgs) =   &
     &   il5(mgs)*(-ciacr(mgs) - crfrz(mgs) ) & 

     &  - chacr(mgs) - chlacr(mgs)   &
     &  +crcev(mgs)   &
     &  - cracr(mgs)

      ELSEIF ( warmonly < 0.8 ) THEN
       pcrwi(mgs) = &
     &   crcnw(mgs)   &
     &  +(1-il5(mgs))*(   &
     &    -chmlrr(mgs)/rzxh(mgs)    &
     &    -chlmlrr(mgs)/rzxhl(mgs)   &
     &    -csmlr(mgs)     &
     &   - cimlr(mgs) )   &
     &  -crshr(mgs)             
      pcrwd(mgs) =   &
     &   il5(mgs)*( - crfrz(mgs) ) & 
     &  - chacr(mgs)    &
     &  - chlacr(mgs)    &
     &  +crcev(mgs)   &
     &  - cracr(mgs)
      ELSE
      pcrwi(mgs) =   &
     &   crcnw(mgs)
      pcrwd(mgs) =   &
     &  +crcev(mgs)   &
     &  - cracr(mgs)






      ENDIF


      frac = 0.0
      IF ( -pcrwd(mgs)*dtp .gt. cx(mgs,lr) ) THEN







       frac =  -cx(mgs,lr)/(pcrwd(mgs)*dtp)
       pcrwd(mgs) = -cx(mgs,lr)/dtp

        ciacr(mgs) = frac*ciacr(mgs)
        crfrz(mgs) = frac*crfrz(mgs)
        crfrzf(mgs) = frac*crfrzf(mgs)
        chacr(mgs) = frac*chacr(mgs)
        crcev(mgs) = frac*crcev(mgs)
        cracr(mgs) = frac*cracr(mgs)


      ENDIF

      end do

      ENDIF


      IF ( warmonly < 0.5 ) THEN




      IF ( ipconc .ge. 4 ) THEN 

      do mgs = 1,ngscnt
      pcswi(mgs) =   &
     &   il5(mgs)*(cscnis(mgs) + cscnvis(mgs) )    &
     &  + crfrzs(mgs)
      pcswd(mgs) = &

     &  -chacs(mgs) - chlacs(mgs)   &
     &  -chcns(mgs)   &
     &  +(1-il5(mgs))*csmlr(mgs) + csshr(mgs) & 

     &   + cssbv(mgs)   &
     &  - csacs(mgs)
      end do

      ENDIF




      IF ( ipconc .ge. 5 ) THEN 
      do mgs = 1,ngscnt
      pchwi(mgs) =   &
     &  +ifrzg*(crfrzf(mgs)   &
     & +il5(mgs)*(ciacrf(mgs) ))    &
     & + chcnsh(mgs) + chcnih(mgs)

      pchwd(mgs) =   &
     &  (1-il5(mgs))*chmlr(mgs) &

     &  + chsbv(mgs)   &
     &  - il5(mgs)*chlcnh(mgs)
      end do





      IF ( lhl .gt. 1 ) THEN 
      do mgs = 1,ngscnt
      pchli(mgs) = (1.0-ifrzg)*(crfrzf(mgs) +il5(mgs)*(ciacrf(mgs) ))  &
     & + chlcnh(mgs) *rzxhlh(mgs)

      pchld(mgs) =   &
     &  (1-il5(mgs))*chlmlr(mgs)   &

     &  + chlsbv(mgs)




      end do

      ENDIF


      ENDIF 

      ELSEIF ( warmonly < 0.8 ) THEN




      IF ( ipconc .ge. 5 ) THEN 
      do mgs = 1,ngscnt
      pchwi(mgs) =   &
     &  +ifrzg*(crfrzf(mgs) )

      pchwd(mgs) =   &
     &  (1-il5(mgs))*chmlr(mgs) &
     &  - il5(mgs)*chlcnh(mgs)
      end do



      IF ( lhl .gt. 1 ) THEN 
      do mgs = 1,ngscnt
      pchli(mgs) = & 
     & + chlcnh(mgs) *rzxhl(mgs)/rzxh(mgs)

      pchld(mgs) =   &
     &  (1-il5(mgs))*chlmlr(mgs) 






      end do

      ENDIF

      ENDIF 

      ENDIF 






      do mgs = 1,ngscnt
      pctot(mgs)   = pccwi(mgs) +pccwd(mgs) +   &
     &               pccii(mgs) +pccid(mgs) +   &
     &               pcrwi(mgs) +pcrwd(mgs) +   &
     &               pcswi(mgs) +pcswd(mgs) +   &
     &               pchwi(mgs) +pchwd(mgs) +   &
     &               pchli(mgs) +pchld(mgs)
      end do


      ENDIF 









       pqwvi(:) = 0.0
       pqwvd(:) = 0.0
       pqcwi(:) = 0.0
       pqcwd(:) = 0.0
       pqcii(:) = 0.0
       pqcid(:) = 0.0
       pqrwi(:) = 0.0
       pqrwd(:) = 0.0
       pqswi(:) = 0.0
       pqswd(:) = 0.0
       pqhwi(:) = 0.0
       pqhwd(:) = 0.0
       pqhli(:) = 0.0
       pqhld(:) = 0.0
       pqlwsi(:) = 0.0
       pqlwsd(:) = 0.0
       pqlwhi(:) = 0.0
       pqlwhd(:) = 0.0
       pqlwhli(:) = 0.0
       pqlwhld(:) = 0.0



      IF ( warmonly < 0.5 ) THEN
      do mgs = 1,ngscnt
      pqwvi(mgs) =    &
     &  -Min(0.0, qrcev(mgs))   &
     &  -Min(0.0, qhcev(mgs))   &
     &  -Min(0.0, qhlcev(mgs))   &
     &  -Min(0.0, qscev(mgs))   &

     &  -qhsbv(mgs) - qhlsbv(mgs)   &
     &  -qssbv(mgs)    &
     &  -il5(mgs)*qisbv(mgs)
      pqwvd(mgs) =     &
     &  -Max(0.0, qrcev(mgs))   &
     &  -Max(0.0, qhcev(mgs))   &
     &  -Max(0.0, qhlcev(mgs))   &
     &  -Max(0.0, qscev(mgs))   &
     &  +il5(mgs)*(-qiint(mgs)   &
     &  -qhdpv(mgs) -qsdpv(mgs) - qhldpv(mgs))   &
     &  -il5(mgs)*qidpv(mgs)
      end do

      ELSEIF ( warmonly < 0.8 ) THEN
      do mgs = 1,ngscnt
      pqwvi(mgs) =    &
     &  -Min(0.0, qrcev(mgs)) &
     &  -il5(mgs)*qisbv(mgs)
      pqwvd(mgs) =     &
     &  +il5(mgs)*(-qiint(mgs)   &

     &  -qhdpv(mgs) - qhldpv(mgs))   &

     &  -Max(0.0, qrcev(mgs))     &
     &  -il5(mgs)*qidpv(mgs)
      end do

      ELSE
      do mgs = 1,ngscnt
      pqwvi(mgs) =    &
     &  -Min(0.0, qrcev(mgs))
      pqwvd(mgs) =     &
     &  -Max(0.0, qrcev(mgs))
      end do

      ENDIF 



      do mgs = 1,ngscnt

      pqcwi(mgs) =  (0.0) + qwcnr(mgs)

      IF ( warmonly < 0.5 ) THEN
      pqcwd(mgs) =    &
     &  il5(mgs)*(-qiacw(mgs)-qwfrzc(mgs)-qwctfzc(mgs))   &
     &  -il5(mgs)*(qicichr(mgs))   &
     &  -qracw(mgs) -qsacw(mgs) -qrcnw(mgs) -qhacw(mgs) - qhlacw(mgs)

      ELSEIF ( warmonly < 0.8 ) THEN
      pqcwd(mgs) =    &
     &  il5(mgs)*(-qiacw(mgs)-qwfrzc(mgs)-qwctfzc(mgs))   &

     &  -il5(mgs)*(qicichr(mgs))   &
     &  -qracw(mgs) -qrcnw(mgs) -qhacw(mgs) -qhlacw(mgs)
      ELSE
      pqcwd(mgs) =    &
     &  -qracw(mgs) - qrcnw(mgs)
      ENDIF

      IF ( pqcwd(mgs) .lt. 0.0 .and. -pqcwd(mgs)*dtp .gt. qx(mgs,lc) ) THEN

       frac = -Max(0.0,qx(mgs,lc))/(pqcwd(mgs)*dtp)
       pqcwd(mgs) = -qx(mgs,lc)/dtp

        qiacw(mgs)   = frac*qiacw(mgs)


        qwfrzc(mgs)  = frac*qwfrzc(mgs)
        qwctfzc(mgs) = frac*qwctfzc(mgs)
        qracw(mgs)   = frac*qracw(mgs)
        qsacw(mgs)   = frac*qsacw(mgs)
        qhacw(mgs)   = frac*qhacw(mgs)
        vhacw(mgs)   = frac*vhacw(mgs)
        qrcnw(mgs)   = frac*qrcnw(mgs)
        IF ( lhl .gt. 1 ) THEN
          qhlacw(mgs)   = frac*qhlacw(mgs)
          vhlacw(mgs)   = frac*vhlacw(mgs)
        ENDIF



      ENDIF
      

      end do



      IF ( warmonly < 0.5 ) THEN

      do mgs = 1,ngscnt
      pqcii(mgs) =     &
     &   il5(mgs)*qicicnt(mgs)    &
     &  +il5(mgs)*qidpv(mgs)    &
     &  +il5(mgs)*qiacw(mgs)   & 
     &  +il5(mgs)*(qwfrzc(mgs)+qwctfzc(mgs))   &
     &  +il5(mgs)*(qicichr(mgs))   &
     &  +qsmul(mgs)               &
     &  +qhmul1(mgs) + qhlmul1(mgs)   &
     & + qsplinter(mgs) + qsplinter2(mgs)


      pqcid(mgs) =     &
     &   il5(mgs)*(-qscni(mgs) - qscnvi(mgs)    & 
     &  -qraci(mgs)    &
     &  -qsaci(mgs) )   &
     &  -qhaci(mgs)   &
     &  -qhlaci(mgs)    &
     &  +il5(mgs)*qisbv(mgs)    &
     &  +(1.-il5(mgs))*qimlr(mgs)   &
     &  - qhcni(mgs)
      end do

      ELSEIF ( warmonly < 0.8 ) THEN

      do mgs = 1,ngscnt
      pqcii(mgs) =     &
     &   il5(mgs)*qicicnt(mgs)     &
     &  +il5(mgs)*(qwfrzc(mgs)+qwctfzc(mgs))   &


     &  +qhmul1(mgs) + qhlmul1(mgs)   &
     & + qsplinter(mgs) + qsplinter2(mgs) &
     &  +il5(mgs)*qidpv(mgs)    &
     &  +il5(mgs)*qiacw(mgs)  






      pqcid(mgs) =     &





     &  +il5(mgs)*qisbv(mgs)    &
     &  +(1.-il5(mgs))*qimlr(mgs)  

      end do

      ENDIF




      do mgs = 1,ngscnt
      IF ( warmonly < 0.5 ) THEN
      pqrwi(mgs) =     &
     &   qracw(mgs) + qrcnw(mgs) + Max(0.0, qrcev(mgs))   &
     &  +(1-il5(mgs))*(   &
     &    -qhmlr(mgs)                 &            
     &    -qsmlr(mgs)  - qhlmlr(mgs)     &
     &    -qimlr(mgs))   &
     &    -qsshr(mgs)       &                      
     &    -qhshr(mgs)       &                      
     &    -qhlshr(mgs)
      pqrwd(mgs) =     &
     &  il5(mgs)*(-qiacr(mgs)-qrfrz(mgs))    &
     &  - qsacr(mgs) - qhacr(mgs) - qhlacr(mgs) - qwcnr(mgs)   &
     &  + Min(0.0,qrcev(mgs))
      ELSEIF ( warmonly < 0.8 ) THEN
      pqrwi(mgs) =     &
     &   qracw(mgs) + qrcnw(mgs) + Max(0.0, qrcev(mgs))   &
     &  +(1-il5(mgs))*(   &
     &    -qhmlr(mgs)                 &            
     &    -qhshr(mgs)                 &           
     &    -qhlmlr(mgs)                 &            
     &    -qhlshr(mgs) )                           
      pqrwd(mgs) =     &
     &  il5(mgs)*(-qrfrz(mgs))    &
     &   - qhacr(mgs)    &
     &   - qhlacr(mgs)    &
     &  + Min(0.0,qrcev(mgs))
      ELSE
      pqrwi(mgs) =     &
     &   qracw(mgs) + qrcnw(mgs) + Max(0.0, qrcev(mgs))
      pqrwd(mgs) =  Min(0.0,qrcev(mgs))
      ENDIF 


 
      IF ( pqrwd(mgs) .lt. 0.0 .and. -(pqrwd(mgs) + pqrwi(mgs))*dtp .gt. qx(mgs,lr)  ) THEN

       frac = (-qx(mgs,lr) + pqrwi(mgs)*dtp)/(pqrwd(mgs)*dtp)


       pqwvi(mgs) = pqwvi(mgs)    &
     &  + Min(0.0, qrcev(mgs))   &
     &  - frac*Min(0.0, qrcev(mgs))
       pqwvd(mgs) =  pqwvd(mgs)   &
     &  + Max(0.0, qrcev(mgs))   &
     &  - frac*Max(0.0, qrcev(mgs))

       qiacr(mgs)  = frac*qiacr(mgs)
       qiacrf(mgs) = frac*qiacrf(mgs)
       viacrf(mgs) = frac*viacrf(mgs)
       qrfrz(mgs)  = frac*qrfrz(mgs) 
       qrfrzs(mgs) = frac*qrfrzs(mgs) 
       qrfrzf(mgs) = frac*qrfrzf(mgs)
       vrfrzf(mgs) = frac*vrfrzf(mgs)
       qsacr(mgs)  = frac*qsacr(mgs)
       qhacr(mgs)  = frac*qhacr(mgs)
       vhacr(mgs)  = frac*vhacr(mgs)
       qrcev(mgs)  = frac*qrcev(mgs)
       qhlacr(mgs) = frac*qhlacr(mgs)
       vhlacr(mgs) = frac*vhlacr(mgs)



      IF ( warmonly < 0.5 ) THEN
       pqrwd(mgs) =     &
     &  il5(mgs)*(-qiacr(mgs)-qrfrz(mgs) - qsacr(mgs))    &
     &  - qhacr(mgs) - qhlacr(mgs) - qwcnr(mgs)   &
     &  + Min(0.0,qrcev(mgs))
      ELSEIF ( warmonly < 0.8 ) THEN
      pqrwd(mgs) =     &
     &  il5(mgs)*(-qrfrz(mgs))    &
     &   - qhacr(mgs)    &
     &   - qhlacr(mgs)    &
     &  + Min(0.0,qrcev(mgs))
      ELSE
       pqrwd(mgs) =  Min(0.0,qrcev(mgs))
      ENDIF 




      IF ( qrcev(mgs) .ne. 0.0 ) THEN
       pqwvi(mgs) =    &
     &  -Min(0.0, qrcev(mgs))   &
     &  -Min(0.0, qhcev(mgs))   &
     &  -Min(0.0, qhlcev(mgs))   &
     &  -Min(0.0, qscev(mgs))   &

     &  -qhsbv(mgs)  - qhlsbv(mgs)   &
     &  -qssbv(mgs)    &
     &  -il5(mgs)*qisbv(mgs)
       pqwvd(mgs) =     &
     &  -Max(0.0, qrcev(mgs))   &
     &  -Max(0.0, qhcev(mgs))   &
     &  -Max(0.0, qhlcev(mgs))   &
     &  -Max(0.0, qscev(mgs))   &
     &  +il5(mgs)*(-qiint(mgs)   &
     &  -qhdpv(mgs) -qsdpv(mgs) - qhldpv(mgs))   &
     &  -il5(mgs)*qidpv(mgs)
       ENDIF



      ENDIF
      end do

      IF ( warmonly < 0.5 ) THEN




      do mgs = 1,ngscnt
      pqswi(mgs) =     &
     &   il5(mgs)*(qscni(mgs)+qsaci(mgs)+qsdpv(mgs)   &
     &   + qscnvi(mgs) + qrfrzs(mgs) + il2(mgs)*qsacr(mgs))   &
     &   + il3(mgs)*(qiacrf(mgs)+qracif(mgs)) &
     &   + Max(0.0, qscev(mgs))   &
     &   + qsacw(mgs)
      pqswd(mgs) =    &

     &  -qracs(mgs)*(1-il2(mgs)) -qhacs(mgs) - qhlacs(mgs)   &
     &  -qhcns(mgs)   &
     &  +(1-il5(mgs))*qsmlr(mgs) + qsshr(mgs)    &    

     &  + (qssbv(mgs))   &
     &  + Min(0.0, qscev(mgs))  &
     &  -qsmul(mgs)
      
      
      end do 
      



      do mgs = 1,ngscnt
      pqhwi(mgs) =    &
     &  +il5(mgs)*ifrzg*(qrfrzf(mgs)  + (1-il3(mgs))*(qiacrf(mgs)+qracif(mgs)))   &
     &  + (1-il2(mgs))*(qracs(mgs) + qsacr(mgs))  &
     &  +il5(mgs)*(qhdpv(mgs))   &
     &  +Max(0.0, qhcev(mgs))   &
     &  +qhacr(mgs)+qhacw(mgs)   &
     &  +qhacs(mgs)+qhaci(mgs)   &
     &  + qhcns(mgs) + qhcni(mgs)
      pqhwd(mgs) =     &
     &   qhshr(mgs)                &    
     &  +(1-il5(mgs))*qhmlr(mgs)   &    

     &  + qhsbv(mgs)   &
     &  + Min(0.0, qhcev(mgs))   &
     &  -qhmul1(mgs) - qhlcnh(mgs)   &
     &  - qsplinter(mgs) - qsplinter2(mgs)

      end do




      IF ( lhl .gt. 1 ) THEN

      do mgs = 1,ngscnt
      pqhli(mgs) =    &
     &  +il5(mgs)*(qhldpv(mgs) + (1.0-ifrzg)*(qiacrf(mgs)+qrfrzf(mgs)  + qracif(mgs)))   &
     &  +Max(0.0, qhlcev(mgs))   &
     &  +qhlacr(mgs)+qhlacw(mgs)   &
     &  +qhlacs(mgs)+qhlaci(mgs)   &
     &  + qhlcnh(mgs)
      pqhld(mgs) =     &
     &   qhlshr(mgs)    &
     &  +(1-il5(mgs))*qhlmlr(mgs)    &

     &  + qhlsbv(mgs)   &
     &  + Min(0.0, qhlcev(mgs))   &
     &  -qhlmul1(mgs)
      end do
      
      ENDIF 

      ELSEIF ( warmonly < 0.8 ) THEN



      do mgs = 1,ngscnt
      pqhwi(mgs) =    &
     &  +il5(mgs)*ifrzg*(qrfrzf(mgs) )   &
     &  +il5(mgs)*(qhdpv(mgs))   &
     &  +qhacr(mgs)+qhacw(mgs)   
      pqhwd(mgs) =     &
     &   qhshr(mgs)                &    
     &  - qhlcnh(mgs)   &
     &  - qhmul1(mgs)   &
     &  - qsplinter(mgs) - qsplinter2(mgs) &
     &  +(1-il5(mgs))*qhmlr(mgs)        
       end do




      IF ( lhl .gt. 1 ) THEN

      do mgs = 1,ngscnt
      pqhli(mgs) =    &
     &  +il5(mgs)*(qhldpv(mgs) ) & 
     &  +qhlacr(mgs)+qhlacw(mgs)   &

     &  + qhlcnh(mgs)
      pqhld(mgs) =     &
     &   qhlshr(mgs)    &
     &  +(1-il5(mgs))*qhlmlr(mgs)    &

     &  + qhlsbv(mgs)   &
     &  -qhlmul1(mgs)

      end do

      ENDIF 

      ENDIF 





      vhmlr(:) = 0.0
      vhlmlr(:) = 0.0
      vhfzh(:) = 0.0
      vhlfzhl(:) = 0.0

      IF ( mixedphase ) THEN
      ELSE 
      

        vhmlr(:) = qhmlr(:) 



        vhlmlr(:) = qhlmlr(:) 


      
      ENDIF  






      IF ( lvol(ls) .gt. 1 ) THEN
      do mgs = 1,ngscnt


      pvswi(mgs) = rho0(mgs)*(    &


     &  +il5(mgs)*(qscni(mgs)+qsaci(mgs)+qsdpv(mgs)   &
     &   + qscnvi(mgs) + qrfrzs(mgs))/xdn0(ls)   &
     &    + (qsacr(mgs))/rimdn(mgs,ls) ) + vsacw(mgs)

      pvswd(mgs) = rho0(mgs)*( pqswd(mgs) )/xdn0(ls)  &




     &   -rho0(mgs)*qsmul(mgs)/xdn0(ls)



      end do






      ENDIF



      IF ( lvol(lh) .gt. 1 ) THEN
      DO mgs = 1,ngscnt





      pvhwi(mgs) = rho0(mgs)*(   &
     &  +il5(mgs)*( qracif(mgs))/rhofrz   &

     &  + (  il5(mgs)*qhdpv(mgs)   &
     &     + qhacs(mgs) + qhaci(mgs) )/xdnmn(lh) )   &
     &  +   rho0(mgs)*Max(0.0, qhcev(mgs))/1000.   & 

     &  + vhcns(mgs)   &
     &  + vhacr(mgs) + vhacw(mgs)  + vhfzh(mgs)   & 

     &  + vhcni(mgs) + viacrf(mgs) + vrfrzf(mgs)

      


      pvhwd(mgs) = rho0(mgs)*(   &


     &  +( (1-il5(mgs))*vhmlr(mgs)    &

     &     + qhsbv(mgs)   &
     &     + Min(0.0, qhcev(mgs))   &
     &     -qhmul1(mgs) )/xdn(mgs,lh) )   &
     &  - vhlcnh(mgs) + vhshdr(mgs) - vhsoak(mgs)






      IF ( .false. .and. ny .eq. 2 .and. kgs(mgs) .eq. 9 .and. igs(mgs) .eq. 19 ) THEN

      write(iunit,*)
      write(iunit,*)   'Graupel at ',igs(mgs),kgs(mgs)

      write(iunit,*)   il5(mgs)*qrfrzf(mgs), qrfrzf(mgs) - qrfrz(mgs)
      write(iunit,*)   il5(mgs)*qiacrf(mgs)
      write(iunit,*)   il5(mgs)*qracif(mgs)
      write(iunit,*)   'qhcns',qhcns(mgs)
      write(iunit,*)   'qhcni',qhcni(mgs)
      write(iunit,*)   il5(mgs)*(qhdpv(mgs))
      write(iunit,*)   'qhacr ',qhacr(mgs)
      write(iunit,*)   'qhacw', qhacw(mgs)
      write(iunit,*)   'qhacs', qhacs(mgs)
      write(iunit,*)   'qhaci', qhaci(mgs)
      write(iunit,*)   'pqhwi = ',pqhwi(mgs)
      write(iunit,*)
      write(iunit,*) 'qhcev',qhcev(mgs)
      write(iunit,*)
      write(iunit,*)   'qhshr',qhshr(mgs)
      write(iunit,*)  'qhmlr', (1-il5(mgs))*qhmlr(mgs)
      write(iunit,*)   'qhsbv', qhsbv(mgs)
      write(iunit,*)   'qhlcnh',-qhlcnh(mgs)
      write(iunit,*)   'qhmul1',-qhmul1(mgs)
      write(iunit,*)   'pqhwd = ', pqhwd(mgs)
      write(iunit,*)
      write(iunit,*)  'Volume'
      write(iunit,*)
      write(iunit,*)  'pvhwi',pvhwi(mgs)
      write(iunit,*)   'vhcns', vhcns(mgs)
      write(iunit,*)  'vhacr,vhacw',vhacr(mgs), vhacw(mgs) 
      write(iunit,*)  'vhcni',vhcni(mgs)
      write(iunit,*)
      write(iunit,*)  'pvhwd',pvhwd(mgs)
      write(iunit,*)  'vhlcnh,vhshdr,vhsoak ', vhlcnh(mgs),  vhshdr(mgs), vhsoak(mgs)
      write(iunit,*)  'vhmlr', vhmlr(mgs)
      write(iunit,*)



      write(iunit,*)  'Concentration'
      write(iunit,*)   pchwi(mgs),pchwd(mgs)
      write(iunit,*)  crfrzf(mgs)
      write(iunit,*)  chcns(mgs)
      write(iunit,*)  ciacrf(mgs)


      ENDIF


      ENDDO

      ENDIF







      IF ( lhl .gt. 1 ) THEN
      IF ( lvol(lhl) .gt. 1 ) THEN
      DO mgs = 1,ngscnt

      pvhli(mgs) = rho0(mgs)*(   &
     &  + (  il5(mgs)*qhldpv(mgs)   &


     &     + qhlacs(mgs) + qhlaci(mgs) )/xdnmn(lh) )   &  
     &  +   rho0(mgs)*Max(0.0, qhlcev(mgs))/1000.   &
     &  + vhlcnhl(mgs)   &
     &  + vhlacr(mgs) + vhlacw(mgs) + vhlfzhl(mgs) 
      
      pvhld(mgs) = rho0(mgs)*(   &
     &  +(  qhlsbv(mgs)   &
     &     + Min(0.0, qhlcev(mgs))   &
     &     -qhlmul1(mgs) )/xdn(mgs,lhl) ) &

     &   + rho0(mgs)*(1-il5(mgs))*vhlmlr(mgs)/xdn(mgs,lhl)  &
     &   + vhlshdr(mgs) - vhlsoak(mgs)


      ENDDO
      
      ENDIF
      ENDIF


      if ( ndebug .ge. 1 ) then
      do mgs = 1,ngscnt

      ptotal(mgs) = 0.
      ptotal(mgs) = ptotal(mgs)    &
     &  + pqwvi(mgs) + pqwvd(mgs)   &
     &  + pqcwi(mgs) + pqcwd(mgs)   &
     &  + pqcii(mgs) + pqcid(mgs)   &
     &  + pqrwi(mgs) + pqrwd(mgs)   &
     &  + pqswi(mgs) + pqswd(mgs)   &
     &  + pqhwi(mgs) + pqhwd(mgs)   &
     &  + pqhli(mgs) + pqhld(mgs)


      if ( ( (ndebug .ge. 1  ) .and. abs(ptotal(mgs)) .gt. eqtot )   &






     &  .or.  .not. (ptotal(mgs) .lt. 1.0 .and.   &
     &            ptotal(mgs) .gt. -1.0)    ) then
      write(iunit,*) 'YIKES! ','ptotal1',mgs,igs(mgs),jgs,   &
     &       kgs(mgs),ptotal(mgs)

      write(iunit,*) 't7: ', t7(igs(mgs),jgs,kgs(mgs))
      write(iunit,*)  'cci,ccw,crw,rdia: ',cx(mgs,li),cx(mgs,lc),cx(mgs,lr),0.5*xdia(mgs,lr,1)
      write(iunit,*)  'qc,qi,qr : ',qx(mgs,lc),qx(mgs,li),qx(mgs,lr)
      write(iunit,*)  'rmas, qrcalc : ',xmas(mgs,lr),xmas(mgs,lr)*cx(mgs,lr)/rho0(mgs)
      write(iunit,*)  'vti,vtc,eiw,vtr: ',vtxbar(mgs,li,1),vtxbar(mgs,lc,1),eiw(mgs),vtxbar(mgs,lr,1)
      write(iunit,*)  'cidia,cwdia,qcmxd: ', xdia(mgs,li,1),xdia(mgs,lc,1),qcmxd(mgs)
      write(iunit,*)  'snow: ',qx(mgs,ls),cx(mgs,ls),swvent(mgs),vtxbar(mgs,ls,1),xdia(mgs,ls,1)
      write(iunit,*)  'graupel: ',qx(mgs,lh),cx(mgs,lh),hwvent(mgs),vtxbar(mgs,lh,1),xdia(mgs,lh,1)
      IF ( lhl .gt. 1 ) write(iunit,*)  'hail: ',qx(mgs,lhl),cx(mgs,lhl),hlvent(mgs),vtxbar(mgs,lhl,1),xdia(mgs,lhl,1)


      write(iunit,*)  'li: ',xdia(mgs,li,1),xdia(mgs,li,2),xmas(mgs,li),qx(mgs,li),   &
     &         vtxbar(mgs,li,1)


      write(iunit,*)  'rain cx,xv : ',cx(mgs,lr),xv(mgs,lr)
      write(iunit,*)  'temcg = ', temcg(mgs)

      write(iunit,*)  pqwvi(mgs) ,pqwvd(mgs)
      write(iunit,*)  pqcwi(mgs) ,pqcwd(mgs)
      write(iunit,*)  pqcii(mgs) ,pqcid(mgs)
      write(iunit,*)  pqrwi(mgs) ,pqrwd(mgs)
      write(iunit,*)  pqswi(mgs) ,pqswd(mgs)
      write(iunit,*)  pqhwi(mgs) ,pqhwd(mgs)
      write(iunit,*)  pqhli(mgs) ,pqhld(mgs)
      write(iunit,*) 'END OF OUTPUT OF SOURCE AND SINK'




      write(iunit,*)
      write(iunit,*)   'Vapor'

      write(iunit,*)   -Min(0.0,qrcev(mgs))
      write(iunit,*)   -il5(mgs)*qhsbv(mgs)
      write(iunit,*)   -il5(mgs)*qhlsbv(mgs)
      write(iunit,*)   -il5(mgs)*qssbv(mgs)
      write(iunit,*)   -il5(mgs)*qisbv(mgs)
      write(iunit,*)    'pqwvi= ', pqwvi(mgs)
      write(iunit,*)   -Max(0.0,qrcev(mgs))
      write(iunit,*)   -il5(mgs)*qiint(mgs)
      write(iunit,*)   -il5(mgs)*qhdpv(mgs)
      write(iunit,*)   -il5(mgs)*qhldpv(mgs)
      write(iunit,*)   -il5(mgs)*qsdpv(mgs)
      write(iunit,*)   -il5(mgs)*qidpv(mgs)
      write(iunit,*)    'pqwvd = ', pqwvd(mgs)

      write(iunit,*)
      write(iunit,*)   'Cloud ice'

      write(iunit,*)   il5(mgs)*qicicnt(mgs)
      write(iunit,*)   il5(mgs)*qidpv(mgs)
      write(iunit,*)   il5(mgs)*qiacw(mgs)
      write(iunit,*)   il5(mgs)*qwfrz(mgs)
      write(iunit,*)   il5(mgs)*qwctfz(mgs)
      write(iunit,*)   il5(mgs)*qicichr(mgs)
      write(iunit,*)   qhmul1(mgs)
      write(iunit,*)   qhlmul1(mgs)
      write(iunit,*)   'pqcii = ', pqcii(mgs)
      write(iunit,*)   -il5(mgs)*qscni(mgs)
      write(iunit,*)   -il5(mgs)*qscnvi(mgs)
      write(iunit,*)   -il5(mgs)*qraci(mgs)
      write(iunit,*)   -il5(mgs)*qsaci(mgs)
      write(iunit,*)   -il5(mgs)*qhaci(mgs)
      write(iunit,*)   -il5(mgs)*qhlaci(mgs)
      write(iunit,*)   il5(mgs)*qisbv(mgs)
      write(iunit,*)   (1.-il5(mgs))*qimlr(mgs)
      write(iunit,*)   -il5(mgs)*qhcni(mgs)
      write(iunit,*)   'pqcid = ', pqcid(mgs)
      write(iunit,*)   ' Conc:'
      write(iunit,*)   pccii(mgs),pccid(mgs)
      write(iunit,*)   il5(mgs),cicint(mgs)
      write(iunit,*)   cwacii(mgs),cwfrzc(mgs),cwctfzc(mgs)
      write(iunit,*)   cicichr(mgs)
      write(iunit,*)   chmul1(mgs)
      write(iunit,*)   chlmul1(mgs)
      write(iunit,*)   csmul(mgs)




      write(iunit,*)
      write(iunit,*)   'Cloud water'

      write(iunit,*)   'pqcwi =', pqcwi(mgs)
      write(iunit,*)   -il5(mgs)*qiacw(mgs)
      write(iunit,*)   -il5(mgs)*qwfrzc(mgs)
      write(iunit,*)   -il5(mgs)*qwctfzc(mgs)


      write(iunit,*)   -il5(mgs)*qiihr(mgs)
      write(iunit,*)   -il5(mgs)*qicichr(mgs)
      write(iunit,*)   -il5(mgs)*qipiphr(mgs)
      write(iunit,*)   -qracw(mgs)
      write(iunit,*)   -qsacw(mgs)
      write(iunit,*)   -qrcnw(mgs)
      write(iunit,*)   -qhacw(mgs)
      write(iunit,*)   -qhlacw(mgs)
      write(iunit,*)   'pqcwd = ', pqcwd(mgs)


      write(iunit,*)
      write(iunit,*)  'Concentration:'
      write(iunit,*)   -cautn(mgs)
      write(iunit,*)   -cracw(mgs)
      write(iunit,*)   -csacw(mgs)
      write(iunit,*)   -chacw(mgs)
      write(iunit,*)  -ciacw(mgs)
      write(iunit,*)  -cwfrzp(mgs)
      write(iunit,*)  -cwctfzp(mgs)
      write(iunit,*)  -cwfrzc(mgs)
      write(iunit,*)  -cwctfzc(mgs)
      write(iunit,*)   pccwd(mgs)

      write(iunit,*)
      write(iunit,*)      'Rain '

      write(iunit,*)      qracw(mgs)
      write(iunit,*)      qrcnw(mgs)
      write(iunit,*)      Max(0.0, qrcev(mgs))
      write(iunit,*)       -(1-il5(mgs))*qhmlr(mgs)
      write(iunit,*)       -(1-il5(mgs))*qhlmlr(mgs)
      write(iunit,*)       -(1-il5(mgs))*qsmlr(mgs)
      write(iunit,*)       -(1-il5(mgs))*qimlr(mgs)
      write(iunit,*)       -qrshr(mgs)
      write(iunit,*)       'pqrwi = ', pqrwi(mgs)    
      write(iunit,*)        -qsshr(mgs)     
      write(iunit,*)        -qhshr(mgs)     
      write(iunit,*)        -qhlshr(mgs)
      write(iunit,*)        -il5(mgs)*qiacr(mgs),qiacr(mgs), qiacrf(mgs)
      write(iunit,*)        -il5(mgs)*qrfrz(mgs)
      write(iunit,*)        -qsacr(mgs)
      write(iunit,*)        -qhacr(mgs)
      write(iunit,*)        -qhlacr(mgs)
      write(iunit,*)        qrcev(mgs)
      write(iunit,*)       'pqrwd = ', pqrwd(mgs) 
      write(iunit,*)       'fhw, fhlw = ',fhw(mgs),fhlw(mgs)
      write(iunit,*)        'qrzfac = ', qrzfac(mgs)

      
      write(iunit,*)
      write(iunit,*)  'Rain concentration'
      write(iunit,*)  pcrwi(mgs) 
      write(iunit,*)    crcnw(mgs)
      write(iunit,*)    1-il5(mgs)
      write(iunit,*)   -chmlr(mgs),-csmlr(mgs)
      write(iunit,*)     -crshr(mgs)
      write(iunit,*)  pcrwd(mgs) 
      write(iunit,*)    il5(mgs)
      write(iunit,*)   -ciacr(mgs),-crfrz(mgs) 
      write(iunit,*)   -csacr(mgs),-chacr(mgs)
      write(iunit,*)   +crcev(mgs)
      write(iunit,*)   cracr(mgs)



      write(iunit,*)
      write(iunit,*)   'Snow'

      write(iunit,*)        il5(mgs)*qscni(mgs), qscnvi(mgs)
      write(iunit,*)        il5(mgs)*qsaci(mgs)
      write(iunit,*)        il5(mgs)*qrfrzs(mgs)
      write(iunit,*)        il5(mgs)*qsdpv(mgs)
      write(iunit,*)        qsacw(mgs)
      write(iunit,*)        qsacr(mgs)
      write(iunit,*)        'pqswi = ',pqswi(mgs)
      write(iunit,*)        -qhcns(mgs)

      write(iunit,*)        -qhacs(mgs)
      write(iunit,*)        -qhlacs(mgs)
      write(iunit,*)       (1-il5(mgs))*qsmlr(mgs)
      write(iunit,*)       qsshr(mgs)

      write(iunit,*)       il5(mgs)*(qssbv(mgs))
      write(iunit,*)       'pqswd = ', pqswd(mgs)


      write(iunit,*)
      write(iunit,*)   'Graupel'

      write(iunit,*)   il5(mgs)*qrfrzf(mgs), qrfrzf(mgs) - qrfrz(mgs)
      write(iunit,*)   il5(mgs)*qiacrf(mgs)
      write(iunit,*)   il5(mgs)*qracif(mgs)
      write(iunit,*)   qhcns(mgs)
      write(iunit,*)   qhcni(mgs)
      write(iunit,*)   il5(mgs)*(qhdpv(mgs))
      write(iunit,*)   qhacr(mgs)
      write(iunit,*)   qhacw(mgs)
      write(iunit,*)   qhacs(mgs)
      write(iunit,*)   qhaci(mgs)
      write(iunit,*)   'pqhwi = ',pqhwi(mgs)
      write(iunit,*)
      write(iunit,*)   qhshr(mgs)
      write(iunit,*)   (1-il5(mgs))*qhmlr(mgs)
      write(iunit,*)   il5(mgs),qhsbv(mgs)
      write(iunit,*)   -qhlcnh(mgs)
      write(iunit,*)   -qhmul1(mgs)
      write(iunit,*)   'pqhwd = ', pqhwd(mgs)
      write(iunit,*)  'Concentration'
      write(iunit,*)   pchwi(mgs),pchwd(mgs)
      write(iunit,*)  crfrzf(mgs)
      write(iunit,*)  chcns(mgs)
      write(iunit,*)  ciacrf(mgs)


      write(iunit,*)
      write(iunit,*)   'Hail'

      write(iunit,*)   qhlcnh(mgs)
      write(iunit,*)   il5(mgs)*(qhldpv(mgs))
      write(iunit,*)   qhlacr(mgs)
      write(iunit,*)   qhlacw(mgs)
      write(iunit,*)   qhlacs(mgs)
      write(iunit,*)   qhlaci(mgs)
      write(iunit,*)   pqhli(mgs)
      write(iunit,*)
      write(iunit,*)   qhlshr(mgs)
      write(iunit,*)   (1-il5(mgs))*qhlmlr(mgs)
      write(iunit,*)   il5(mgs)*qhlsbv(mgs)
      write(iunit,*)   pqhld(mgs)
      write(iunit,*)  'Concentration'
      write(iunit,*)   pchli(mgs),pchld(mgs)
      write(iunit,*)  chlcnh(mgs)




      write(iunit,*) 'END OF OUTPUT OF SOURCE AND SINK'
      write(iunit,*) 'PTOTAL',ptotal(mgs)

      end if

      end do


      end if 




      do mgs = 1,ngscnt
      IF ( warmonly < 0.5 ) THEN
      pfrz(mgs) =    &
     &  (1-il5(mgs))*   &
     &  (qhmlr(mgs)+qsmlr(mgs)+qhlmlr(mgs))   & 
     &  +il5(mgs)*(qhfzh(mgs)+qsfzs(mgs)+qhlfzhl(mgs))   &
     &  +il5(mgs)*(1-imixedphase)*(   &
     &   qsacw(mgs)+qhacw(mgs) + qhlacw(mgs)   &
     &  +qsacr(mgs)+qhacr(mgs) + qhlacr(mgs)   &
     &  +qsshr(mgs)   &
     &  +qhshr(mgs)   &
     &  +qhlshr(mgs) +qrfrz(mgs)+qiacr(mgs)  &
     &  )  &
     &  +il5(mgs)*(qwfrz(mgs)    &
     &  +qwctfz(mgs)+qiihr(mgs)   &
     &  +qiacw(mgs))
      pmlt(mgs) =    &
     &  (1-il5(mgs))*   &
     &  (qhmlr(mgs)+qsmlr(mgs)+qhlmlr(mgs))    
      psub(mgs) =    &
     &   il5(mgs)*(   &
     &  + qsdpv(mgs) + qhdpv(mgs)   &
     &  + qhldpv(mgs)    &
     &  + qidpv(mgs) + qisbv(mgs) )   &
     &   + qssbv(mgs)  + qhsbv(mgs) + qhlsbv(mgs)   &
     &  +il5(mgs)*(qiint(mgs))
      pvap(mgs) =    &
     &   qrcev(mgs) + qhcev(mgs) + qscev(mgs) + qhlcev(mgs)
      pevap(mgs) =    &
     &   Min(0.0,qrcev(mgs)) + Min(0.0,qhcev(mgs)) + Min(0.0,qscev(mgs)) + Min(0.0,qhlcev(mgs))
      pdep(mgs) =    &
     &   il5(mgs)*(   &
     &  + qsdpv(mgs) + qhdpv(mgs)   &
     &  + qhldpv(mgs)    &
     &  + qidpv(mgs)  )   &
     &  +il5(mgs)*(qiint(mgs))
      ELSEIF ( warmonly < 0.8 ) THEN
      pfrz(mgs) =    &
     &  (1-il5(mgs))*   &
     &  (qhmlr(mgs)+qhlmlr(mgs))   & 
     &  +il5(mgs)*(qhfzh(mgs)+qhlfzhl(mgs))   &
     &  +il5(mgs)*(   &
     &  +qhshr(mgs)   &
     &  +qhlshr(mgs)   &
     &  +qrfrz(mgs)+qwfrz(mgs)   &
     &  +qwctfz(mgs)+qiihr(mgs)   &
     &  +qiacw(mgs)                &
     & +qhacw(mgs) + qhlacw(mgs)   &
     & +qhacr(mgs) + qhlacr(mgs)  ) 
      psub(mgs) =  0.0 +  &
     &   il5(mgs)*(   &
     &  + qhdpv(mgs)   &
     &  + qhldpv(mgs)    &
     &  + qidpv(mgs) + qisbv(mgs) )   &
     &  +il5(mgs)*(qiint(mgs))
      pvap(mgs) =    &
     &   qrcev(mgs) + qhcev(mgs) + qhlcev(mgs) 
      ELSE
      pfrz(mgs) = 0.0
      psub(mgs) = 0.0
      pvap(mgs) = qrcev(mgs)
      ENDIF 
      ptem(mgs) =    &
     &  (1./pi0(mgs))*   &
     &  (felfcp(mgs)*pfrz(mgs)   &
     &  +felscp(mgs)*psub(mgs)    &
     &  +felvcp(mgs)*pvap(mgs))
      thetap(mgs) = thetap(mgs) + dtp*ptem(mgs)
      end do








      do mgs = 1,ngscnt
      qwvp(mgs) = qwvp(mgs) +        &
     &   dtp*(pqwvi(mgs)+pqwvd(mgs))
      qx(mgs,lc) = qx(mgs,lc) +   &
     &   dtp*(pqcwi(mgs)+pqcwd(mgs))



      qx(mgs,lr) = qx(mgs,lr) +   &
     &   dtp*(pqrwi(mgs)+pqrwd(mgs))




      qx(mgs,li) = qx(mgs,li) +   &
     &   dtp*(pqcii(mgs)+pqcid(mgs))
      qx(mgs,ls) = qx(mgs,ls) +   &
     &   dtp*(pqswi(mgs)+pqswd(mgs))
      qx(mgs,lh) = qx(mgs,lh) +    &
     &   dtp*(pqhwi(mgs)+pqhwd(mgs))
      IF ( lhl .gt. 1 ) THEN
      qx(mgs,lhl) = qx(mgs,lhl) +    &
     &   dtp*(pqhli(mgs)+pqhld(mgs))

      ENDIF


      end do



      IF ( ldovol ) THEN

      do mgs = 1,ngscnt

      IF ( lvol(ls) .gt. 1 ) THEN
      vx(mgs,ls) = vx(mgs,ls) +    &
     &   dtp*(pvswi(mgs)+pvswd(mgs))
      ENDIF

      IF ( lvol(lh) .gt. 1 ) THEN
      vx(mgs,lh) = vx(mgs,lh) +    &
     &   dtp*(pvhwi(mgs)+pvhwd(mgs))

      ENDIF

      IF ( lhl .gt. 1 ) THEN
      IF ( lvol(lhl) .gt. 1 ) THEN
      vx(mgs,lhl) = vx(mgs,lhl) +    &
     &   dtp*(pvhli(mgs)+pvhld(mgs))

      ENDIF
      ENDIF

      ENDDO

      ENDIF  






      if ( ipconc .ge. 1  ) then
      do mgs = 1,ngscnt
      cx(mgs,li) = cx(mgs,li) +   &
     &   dtp*(pccii(mgs)+pccid(mgs))
      IF ( ipconc .ge. 2 ) THEN
      cx(mgs,lc) = cx(mgs,lc) +   &
     &   dtp*(pccwi(mgs)+pccwd(mgs))
      ENDIF
      IF ( ipconc .ge. 3 ) THEN
      cx(mgs,lr) = cx(mgs,lr) +   &
     &   dtp*(pcrwi(mgs)+pcrwd(mgs))
      ENDIF
      IF ( ipconc .ge. 4 ) THEN
      cx(mgs,ls) = cx(mgs,ls) +   &
     &   dtp*(pcswi(mgs)+pcswd(mgs))
      ENDIF
      IF ( ipconc .ge. 5 ) THEN
      cx(mgs,lh) = cx(mgs,lh) +    &
     &   dtp*(pchwi(mgs)+pchwd(mgs))
       IF ( lhl .gt. 1 ) THEN
        cx(mgs,lhl) = cx(mgs,lhl) +    &
     &     dtp*(pchli(mgs)+pchld(mgs))



       ENDIF
      ENDIF
      end do
      end if





      if (ndebug .gt. 0 ) write(0,*) 'conc 30a'










      do mgs = 1,ngscnt
      pqs(mgs) = (380.0)/(pres(mgs))
      theta(mgs) = thetap(mgs) + theta0(mgs)
      qvap(mgs) = max( (qwvp(mgs) + qv0(mgs)), 0.0 )
      temg(mgs) = theta(mgs)*pk(mgs) 
      end do



      do mgs = 1,ngscnt
      qcwtmp(mgs) = qx(mgs,lc)
      ptimlw(mgs) = 0.0
      end do

      do mgs = 1,ngscnt
      qitmp(mgs) = qx(mgs,li)
      if( temg(mgs) .gt. tfr .and.   &
     &    qitmp(mgs) .gt. 0.0 ) then
      qx(mgs,lc) = qx(mgs,lc) + qitmp(mgs)

      ptem(mgs) =  ptem(mgs) +   &
     &  (1./pi0(mgs))*   &
     &  felfcp(mgs)*(- qitmp(mgs)/dtp)  
      pmlt(mgs) = pmlt(mgs) - qitmp(mgs)/dtp
      scx(mgs,lc) = scx(mgs,lc) + scx(mgs,li)
      thetap(mgs) = thetap(mgs) -   &
     &  fcc3(mgs)*qitmp(mgs)
      ptimlw(mgs) = -fcc3(mgs)*qitmp(mgs)/dtp
      cx(mgs,lc) = cx(mgs,lc) + cx(mgs,li)
      qx(mgs,li) = 0.0
      cx(mgs,li) = 0.0
      scx(mgs,li) = 0.0
      vx(mgs,li) = 0.0
      qitmp(mgs) = 0.0
      end if
      end do










      IF ( warmonly < 0.8 ) THEN

      do mgs = 1,ngscnt
      qcwtmp(mgs) = qx(mgs,lc)
      ptwfzi(mgs) = 0.0
      end do

      do mgs = 1,ngscnt





      ctmp = 0.0
      frac = 0.0
      qtmp = 0.0
      


      if( temg(mgs) .lt. thnuc + 0. .and.    &
     &  qx(mgs,lc) .gt. 0.0 .and. (ipconc < 2 .or. ibfc == 0 )) then

      IF ( ibfc /= 2 .or. ipconc < 2 ) THEN
      frac = Max( 0.25, Min( 1., ((thnuc + 2.) - temg(mgs) )/4.0 ) )
      ELSE
          volt = exp( 16.2 + 1.0*temcg(mgs) )* 1.0e-6 
                                               
                                               
         
         cwfrz(mgs) = cx(mgs,lc)*Exp(-volt/xv(mgs,lc)) 

         qtmp = cwfrz(mgs)*xdn0(lc)*rhoinv(mgs)*(volt + xv(mgs,lc))
         frac = qtmp/qx(mgs,lc) 
                                                       
                                                       
      
      ENDIF
      qtmp = frac*qx(mgs,lc)

      qx(mgs,li) = qx(mgs,li) + qtmp 
      pfrz(mgs) = pfrz(mgs) + qtmp/dtp
      ptem(mgs) =  ptem(mgs) +   &
     &  (1./pi0(mgs))*   &
     &  felfcp(mgs)*(qtmp/dtp)  

      IF ( lvol(li) .gt. 1 ) vx(mgs,li) = vx(mgs,li) + rho0(mgs)*qtmp/xdn0(li)

      IF ( ipconc .ge. 2 ) THEN
        ctmp = frac*cx(mgs,lc)

        cx(mgs,li) = cx(mgs,li) + ctmp
      ELSE 
        ctmp = 0.0
        IF ( t9(igs(mgs),jgs,kgs(mgs)-1) .gt. qx(mgs,lc) ) THEN
           qtmp = frac*t9(igs(mgs),jgs,kgs(mgs)-1)  


           ctmp = cx(mgs,lc)*qx(mgs,lc)*rho0(mgs)/qtmp
        ELSE
           cx(mgs,lc) = Max(0.0,wvel(mgs))*dtp*cwccn   &
     &      /gz(igs(mgs),jgs,kgs(mgs))
          cx(mgs,lc) = cwccn
        ENDIF

       IF ( ipconc .ge. 1 ) cx(mgs,li) = Min(ccimx, cx(mgs,li) + cx(mgs,lc))
      ENDIF

      sctmp = frac*scx(mgs,lc)

      scx(mgs,li) = scx(mgs,li) + sctmp





      thetap(mgs) = thetap(mgs) + fcc3(mgs)*qtmp
      ptwfzi(mgs) = fcc3(mgs)*qtmp/dtp
      qx(mgs,lc) = qx(mgs,lc) - qtmp
      cx(mgs,lc) = cx(mgs,lc) - ctmp
      scx(mgs,lc) = scx(mgs,lc) - sctmp
      end if
      end do

      ENDIF 







      qcond(:) = 0.0
      
      IF ( ipconc .le. 1 .and.  lwsm6 ) THEN 
       DO mgs = 1,ngscnt

        qcwtmp(mgs) = qx(mgs,lc)
        theta(mgs) = thetap(mgs) + theta0(mgs)
        temgtmp = temg(mgs)



        temg(mgs) = theta(mgs)*pk(mgs) 
        temcg(mgs) = temg(mgs) - tfr
        ltemq = (temg(mgs)-163.15)/fqsat+1.5
        ltemq = Min( nqsat, Max(1,ltemq) )

        qvs(mgs) = pqs(mgs)*tabqvs(ltemq)

        IF ( ( qwvp(mgs) > qvs(mgs) .or. qx(mgs,lc) > qxmin(lc) ) .and. temg(mgs) > tfrh ) THEN
          tmp = (qwvp(mgs) - qvs(mgs))/(1. + qvs(mgs)*felv(mgs)**2/(cp*rw*temg(mgs)**2) )
          qcond(mgs) = Min( Max( 0.0, tmp ), (qwvp(mgs)-qvs(mgs)) )
          IF ( qx(mgs,lc) > qxmin(lc) .and. tmp < 0.0 ) THEN 
            qcond(mgs) = Max( tmp, -qx(mgs,lc) )
          ENDIF
          qwvp(mgs) = qwvp(mgs) - qcond(mgs)
          qx(mgs,lc) = Max( 0.0, qx(mgs,lc) + qcond(mgs) )
          thetap(mgs) = thetap(mgs) + felvcp(mgs)*qcond(mgs)/(pi0(mgs))
          
        ENDIF
        
        ENDDO
      
      ENDIF
      
      
      IF ( ipconc .le. 1 .and. .not. lwsm6 ) THEN

      
      do mgs = 1,ngscnt
      qx(mgs,lv) = max( 0.0, qvap(mgs) )
      qx(mgs,lc) = max( 0.0, qx(mgs,lc) )
      qx(mgs,li) = max( 0.0, qx(mgs,li) )
      qitmp(mgs) = qx(mgs,li)
      end do


      do mgs = 1,ngscnt
      qcwtmp(mgs) = qx(mgs,lc)
      qitmp(mgs) = qx(mgs,li)
      theta(mgs) = thetap(mgs) + theta0(mgs)
      temgtmp = temg(mgs)
      temg(mgs) = theta(mgs)*(p2(igs(mgs),jgs,kgs(mgs)) ) 
      temsav = temg(mgs)
      thsave(mgs) = thetap(mgs)
      temcg(mgs) = temg(mgs) - tfr
      tqvcon = temg(mgs)-cbw
      ltemq = (temg(mgs)-163.15)/fqsat+1.5
      ltemq = Min( nqsat, Max(1,ltemq) )












      qvs(mgs) = pqs(mgs)*tabqvs(ltemq)
      qis(mgs) = pqs(mgs)*tabqis(ltemq)





















      qss(mgs) = qvs(mgs)
      if ( temg(mgs) .lt. tfr ) then
      if( qx(mgs,lc) .ge. 0.0 .and. qitmp(mgs) .le. qxmin(li) )   &
     &  qss(mgs) = qvs(mgs)
      if( qx(mgs,lc) .le. qxmin(lc) .and. qitmp(mgs) .gt. qxmin(li))   &
     &  qss(mgs) = qis(mgs)
      if( qx(mgs,lc) .gt. qxmin(lc) .and. qitmp(mgs) .gt. qxmin(li))   &
     &   qss(mgs) = (qx(mgs,lc)*qvs(mgs) + qitmp(mgs)*qis(mgs)) /   &
     &   (qx(mgs,lc) + qitmp(mgs))
      end if
      end do



      do itertd = 1,2

      do mgs = 1,ngscnt



      qitmp(mgs) = qx(mgs,li)
      fcci(mgs) = 0.0
      fcip(mgs) = 0.0
      dqcw(mgs) = 0.0
      dqci(mgs) = 0.0
      dqwv(mgs) = ( qx(mgs,lv) - qss(mgs) )



      if( dqwv(mgs) .lt. 0. ) then           
        if( qx(mgs,lc) .gt. -dqwv(mgs) ) then  
          dqcw(mgs) = dqwv(mgs)
          dqwv(mgs) = 0.
        else                                 
          dqcw(mgs) = -qx(mgs,lc)
          dqwv(mgs) = dqwv(mgs) + qx(mgs,lc)
        end if

        if( qitmp(mgs) .gt. -dqwv(mgs) ) then  
          dqci(mgs) = dqwv(mgs)
          dqwv(mgs) = 0.
        else                                  
          dqci(mgs) = -qitmp(mgs)
          dqwv(mgs) = dqwv(mgs) + qitmp(mgs)
        end if

       qwvp(mgs) = qwvp(mgs) - ( dqcw(mgs) + dqci(mgs) )  




      qitmp(mgs) = qx(mgs,li)
      IF ( qitmp(mgs) .gt. qxmin(li) ) THEN
        fcci(mgs) = qx(mgs,li)/(qitmp(mgs))
      ELSE
        fcci(mgs) = 0.0
      ENDIF
      qx(mgs,lc) = qx(mgs,lc) + dqcw(mgs)
      qx(mgs,li) = qx(mgs,li) + dqci(mgs) * fcci(mgs)
      thetap(mgs) = thetap(mgs) +   &
     &  1./pi0(mgs)*   &
     &  (felvcp(mgs)*dqcw(mgs) +felscp(mgs)*dqci(mgs))

      end if  



      IF ( dqwv(mgs) .ge. 0. ) THEN
      


        qitmp(mgs) = qx(mgs,li)
        fracl(mgs) = 1.0
        fraci(mgs) = 0.0
        if ( temg(mgs) .lt. tfr .and. temg(mgs) .gt. thnuc ) then
          fracl(mgs) = max(min(1.,(temg(mgs)-233.15)/(20.)),0.0)
          fraci(mgs) = 1.0-fracl(mgs)
        end if
        if ( temg(mgs) .le. thnuc ) then
           fraci(mgs) = 1.0
           fracl(mgs) = 0.0
         end if
        fraci(mgs) = 1.0-fracl(mgs)

       gamss = (felvcp(mgs)*fracl(mgs) + felscp(mgs)*fraci(mgs))   &
     &      / (pi0(mgs))

      IF ( temg(mgs) .lt. tfr ) then
        IF (qx(mgs,lc) .ge. 0.0 .and. qitmp(mgs) .le. qxmin(li) ) then
         dqvcnd(mgs) = dqwv(mgs)/(1. + fcqv1(mgs)*qss(mgs)/   &
     &  ((temg(mgs)-cbw)**2))
        END IF
        IF ( qx(mgs,lc) .eq. 0.0 .and. qitmp(mgs) .gt. qxmin(li) ) then
          dqvcnd(mgs) = dqwv(mgs)/(1. + fcqv2(mgs)*qss(mgs)/   &
     &  ((temg(mgs)-cbi)**2))
        END IF
        IF ( qx(mgs,lc) .gt. 0.0 .and. qitmp(mgs) .gt. qxmin(li) ) then
         cdw = caw*pi0(mgs)*tfrcbw/((temg(mgs)-cbw)**2)
         cdi = cai*pi0(mgs)*tfrcbi/((temg(mgs)-cbi)**2)
         denom1 = qx(mgs,lc) + qitmp(mgs)
         denom2 = 1.0 + gamss*   &
     &    (qx(mgs,lc)*qvs(mgs)*cdw + qitmp(mgs)*qis(mgs)*cdi) / denom1
         dqvcnd(mgs) =  dqwv(mgs) / denom2
        END IF 

      ENDIF  

      if ( temg(mgs) .ge. tfr ) then
      dqvcnd(mgs) = dqwv(mgs)/(1. + fcqv1(mgs)*qss(mgs)/   &
     &  ((temg(mgs)-cbw)**2))
      end if

      delqci1=qx(mgs,li)

      IF ( qitmp(mgs) .gt. qxmin(li) ) THEN
        fcci(mgs) = qx(mgs,li)/(qitmp(mgs))
      ELSE
        fcci(mgs) = 0.0
      ENDIF

      dqcw(mgs) = dqvcnd(mgs)*fracl(mgs)
      dqci(mgs) = dqvcnd(mgs)*fraci(mgs)

      thetap(mgs) = thetap(mgs) +   &
     &   (felvcp(mgs)*dqcw(mgs) + felscp(mgs)*dqci(mgs))   &
     & / (pi0(mgs))
      qwvp(mgs) = qwvp(mgs) - ( dqvcnd(mgs) )
      qx(mgs,lc) = qx(mgs,lc) + dqcw(mgs)
      IF ( qitmp(mgs) .gt. qxmin(li) ) THEN
        qx(mgs,li) = qx(mgs,li) + dqci(mgs)*fcci(mgs)
        qitmp(mgs) = qx(mgs,li)
      ENDIF



      END IF 
      end do

      do mgs = 1,ngscnt
      qitmp(mgs) = qx(mgs,li)
      theta(mgs) = thetap(mgs) + theta0(mgs)
      temg(mgs) = theta(mgs)*pk(mgs) 
      qvap(mgs) = Max((qwvp(mgs) + qv0(mgs)), 0.0)
      temcg(mgs) = temg(mgs) - tfr
      tqvcon = temg(mgs)-cbw
      ltemq = (temg(mgs)-163.15)/fqsat+1.5
      ltemq = Min( nqsat, Max(1,ltemq) )
      qvs(mgs) = pqs(mgs)*tabqvs(ltemq)
      qis(mgs) = pqs(mgs)*tabqis(ltemq)
      qx(mgs,lc) = max( 0.0, qx(mgs,lc) )
      qitmp(mgs) = max( 0.0, qitmp(mgs) )
      qx(mgs,lv) = max( 0.0, qvap(mgs))













      qss(mgs) = qvs(mgs)
      if ( temg(mgs) .lt. tfr ) then
      if( qx(mgs,lc) .ge. 0.0 .and. qitmp(mgs) .le. qxmin(li) )   &
     &  qss(mgs) = qvs(mgs)
      if( qx(mgs,lc) .le. qxmin(lc) .and. qitmp(mgs) .gt. qxmin(li))   &
     &  qss(mgs) = qis(mgs)
      if( qx(mgs,lc) .gt. qxmin(lc) .and. qitmp(mgs) .gt. qxmin(li))   &
     &   qss(mgs) = (qx(mgs,lc)*qvs(mgs) + qitmp(mgs)*qis(mgs)) /   &
     &   (qx(mgs,lc) + qitmp(mgs))
      end if


      end do



      end do

     ENDIF 








      if (ndebug .gt. 0 ) write(0,*) 'conc 30b'





! !DIR$ IVDEP
      do mgs = 1,ngscnt
      t0(igs(mgs),jy,kgs(mgs)) =  temg(mgs)
      end do





      if (ndebug .gt. 0 ) write(0,*) 'gs 11'

      do mgs = 1,ngscnt

      an(igs(mgs),jy,kgs(mgs),lt) =    &
     &  theta0(mgs) + thetap(mgs) 
      an(igs(mgs),jy,kgs(mgs),lv) = qwvp(mgs) 

      
      DO il = lc,lhab
        IF ( ido(il) .eq. 1 ) THEN
         an(igs(mgs),jy,kgs(mgs),il) = qx(mgs,il) +   &
     &     min( an(igs(mgs),jy,kgs(mgs),il), 0.0 )
         qx(mgs,il) = an(igs(mgs),jy,kgs(mgs),il)
        ENDIF
      ENDDO



      end do


      if ( ipconc .ge. 1 ) then
      DO il = lc,lhab 



       IF ( ipconc .ge. ipc(il) ) THEN 

         IF (  ipconc .ge. 4 .and. ipc(il) .ge. 3 ) THEN 




          IF ( lz(il) <= 1 .or. ioldlimiter == 1 ) THEN 
          

           DO mgs = 1,ngscnt
            IF ( qx(mgs,il) .le. 0.0 ) THEN
              cx(mgs,il) = 0.0
            ELSE
              xv(mgs,il) = rho0(mgs)*qx(mgs,il)/(xdn(mgs,il)*Max(1.0e-9,cx(mgs,il)))
              



              
              IF ( xv(mgs,il) .lt. xvmn(il) .or. xv(mgs,il) .gt. xvmx(il) ) THEN
                xv(mgs,il) = Min( xvmx(il), Max( xvmn(il),xv(mgs,il) ) )
                cx(mgs,il) = rho0(mgs)*qx(mgs,il)/(xv(mgs,il)*xdn(mgs,il))
               ENDIF





            ENDIF
           ENDDO 
          
          
          ENDIF 
          ENDIF 

          DO mgs = 1,ngscnt
            an(igs(mgs),jy,kgs(mgs),ln(il)) = Max(cx(mgs,il), 0.0)
          ENDDO
        ENDIF 
      ENDDO 

      IF (   lcin > 1 ) THEN
      do mgs = 1,ngscnt
        an(igs(mgs),jy,kgs(mgs),lcin) = Max(0.0, ccin(mgs))
      end do
      ENDIF

      IF (  ipconc .ge. 2 ) THEN
      do mgs = 1,ngscnt
        IF ( lccn > 1 ) THEN
        an(igs(mgs),jy,kgs(mgs),lccn) = Max(0.0, Min(ccwmx,ccnc(mgs)) )
        ENDIF
      end do
      ENDIF
      
      ELSEIF ( ipconc .eq. 0 .and. lni .gt. 1 ) THEN
      
          DO mgs = 1,ngscnt
            an(igs(mgs),jy,kgs(mgs),lni) = Max(cx(mgs,li), 0.0)
          ENDDO


      end if

      IF ( ldovol ) THEN

       DO il = li,lhab

        IF ( lvol(il) .ge. 1 ) THEN

          DO mgs = 1,ngscnt

           an(igs(mgs),jy,kgs(mgs),lvol(il)) = Max( 0.0, vx(mgs,il) )
          ENDDO
          
        ENDIF
      
       ENDDO
      
      ENDIF





      if (ndebug .gt. 0 ) write(0,*) 'gs 12'



      if (ndebug .gt. 0 ) write(0,*) 'gs 13'

 9998 continue

      if ( kz .gt. nz-1 .and. ix .ge. nx) then
        if ( ix .ge. nx ) then
         go to 1200 
        else
         nzmpb = kz
        endif
      else
        nzmpb = kz
      end if

      if ( ix .ge. nx ) then
        nxmpb = 1
        nzmpb = kz+1
      else
       nxmpb = ix+1
      end if

 1000 continue
 1200 continue





      return
      end subroutine nssl_2mom_gs








      SUBROUTINE setvtz(ngscnt,ngs0,qx,qxmin,qxw,cx,rho0,rhovt,xdia,cno,cnostmp, &
     &                 xmas,vtxbar,xdn,xvmn0,xvmx0,xv,cdx,            &
     &                 ipconc1,ndebug1,ngs,nz,kgs,fadvisc,   &
     &                 cwmasn,cwmasx,cwradn,cnina,cimna,cimxa,      &
     &                 itype1a,itype2a,temcg,infdo,alpha,ildo)


      implicit none



      
      integer ngscnt,ngs0,ngs,nz

      
      real xv(ngs0,lc:lhab)
      real qx(ngs0,lv:lhab)
      real qxw(ngs0,ls:lhab)
      real cx(ngs0,lc:lhab)
      real vtxbar(ngs0,lc:lhab,3)
      real xmas(ngs0,lc:lhab)
      real xdn(ngs0,lc:lhab)
      real xdia(ngs0,lc:lhab,3)
      real xvmn0(lc:lhab), xvmx0(lc:lhab)
      real qxmin(lc:lhab)
      real cdx(lc:lhab)
      real alpha(ngs0,lr:lhab)
      
      real rho0(ngs),rhovt(ngs),temcg(ngs)
      real cno(lc:lhab)
      real cnostmp(ngs)
      
      real cwc1, cimna, cimxa
      real cnina(ngs)
      integer kgs(ngs)
      real fadvisc(ngs)
      real fsw
      
      integer ipconc1
      integer ndebug1
      
      integer, intent (in) :: itype1a,itype2a,infdo
      integer, intent (in) :: ildo 
      


      real :: axh(ngs0),bxh(ngs0)
      real :: axhl(ngs0),bxhl(ngs0)

      real cd
      real cwc0 
      real :: cwch(ngscnt), cwchl(ngscnt)
      real :: cwchtmp,cwchltmp,xnutmp
      real pii
      real cimasx,cimasn
      real cwmasn,cwmasx,cwradn
      real cwrad
      real vr,rnux
      real alp
      
      real ccimx

      integer mgs
      
      real arx,frx,vtrain,fw
      real fwlo,fwhi,rfwdiff
      real ar,br,cs,ds


      real gr
      real rwrad,rwdia
      real mwfac
      integer il




      
      real bta1,cnit
      parameter ( bta1 = 0.6, cnit = 1.0e-02 )
      real x,y,tmp,del
      real aax,bbx,delrho
      integer :: indxr
      real mwt
      real, parameter :: rho00 = 1.225
      integer i
      real xvbarmax

      integer l1, l2









      fwlo = 0.2                
      fwhi = 0.4                
      rfwdiff = 1./(fwhi - fwlo)
      

      pii = piinv 

      arx = 10.
      frx = 516.575 

      ar = 841.99666  
      br = 0.8
      gr = 9.8

      cs = 12.42
      ds = 0.42

      IF ( ildo == 0 ) THEN
        l1 = lc
        l2 = lhab
      ELSE
        l1 = ildo
        l2 = ildo
      ENDIF










        
        IF ( lh  .gt. 1 ) THEN
          IF ( dmuh == 1.0 ) THEN
            cwchtmp = ((3. + dnu(lh))*(2. + dnu(lh))*(1.0 + dnu(lh)))**(-1./3.)
          ELSE
            cwchtmp = 6.0*pii*gamma( (xnu(lh) + 1.)/xmu(lh) )/gamma( (xnu(lh) + 2.)/xmu(lh) )
          ENDIF
        ENDIF
        IF ( lhl .gt. 1 ) THEN
          IF ( dmuhl == 1.0 ) THEN
            cwchltmp = ((3. + dnu(lhl))*(2. + dnu(lhl))*(1.0 + dnu(lhl)))**(-1./3.)
          ELSE
            cwchltmp = 6.0*pii*gamma( (xnu(lhl) + 1)/xmu(lhl) )/gamma( (xnu(lhl) + 2)/xmu(lhl) )
          ENDIF
        ENDIF

        IF ( ipconc .le. 5 ) THEN
          IF ( lh  .gt. 1 ) cwch(:) =  cwchtmp 
          IF ( lhl .gt. 1 ) cwchl(:) = cwchltmp
        ELSE
          DO mgs = 1,ngscnt
          
          IF ( lh  .gt. 1 .and. ( ildo == 0 .or. ildo == lh ) ) THEN
           IF ( qx(mgs,lh) .gt. qxmin(lh) ) THEN
            IF ( dmuh == 1.0 ) THEN
              cwch(mgs) = ((3. + alpha(mgs,lh))*(2. + alpha(mgs,lh))*(1.0 + alpha(mgs,lh)))**(-1./3.)
             ELSE
             xnutmp = (alpha(mgs,lh) - 2.0)/3.0
             cwch(mgs) =  6.0*pii*gamma( (xnutmp + 1.)/xmu(lh) )/gamma( (xnutmp + 2.)/xmu(lh) )
            ENDIF
           ELSE
             cwch(mgs) = cwchtmp
           ENDIF
          ENDIF
          IF ( lhl .gt. 1 .and. ( ildo == 0 .or. ildo == lhl ) ) THEN
           IF ( qx(mgs,lhl) .gt. qxmin(lhl) ) THEN
            IF ( dmuhl == 1.0 ) THEN
              cwchl(mgs) = ((3. + alpha(mgs,lhl))*(2. + alpha(mgs,lhl))*(1.0 + alpha(mgs,lhl)))**(-1./3.)
             ELSE
             xnutmp = (alpha(mgs,lhl) - 2.0)/3.0
             cwchl(mgs) = 6.0*pii*gamma( (xnutmp + 1)/xmu(lhl) )/gamma( (xnutmp + 2)/xmu(lhl) )
            ENDIF
           ELSE
             cwchl(mgs) = cwchltmp
           ENDIF
          ENDIF
          
          ENDDO
        
        ENDIF
       

      cimasn = Min( cimas0, 6.88e-13)
      cimasx = 1.0e-8
      ccimx = 5000.0e3   

      cwc1 = 6.0/(pi*1000.)
      cwc0 = pii 
      mwfac = 6.0**(1./3.)

      
      if (ndebug1 .gt. 0 ) write(0,*) 'SETVTZ: Set scale diameter'










      if ( ndebug1 .gt. 0 ) write(0,*) 'SETVTZ: Set cloud water variables'
      
      IF ( ildo == 0 .or. ildo == lc ) THEN
      
      do mgs = 1,ngscnt
      xv(mgs,lc) = 0.0
      
      IF ( qx(mgs,lc) .gt. qxmin(lc) ) THEN 
      
      IF ( ipconc .ge. 2 .and. cx(mgs,lc) .gt. 1.0e-9 ) THEN 
        xmas(mgs,lc) =  &
     &    min( max(qx(mgs,lc)*rho0(mgs)/cx(mgs,lc),cwmasn),cwmasx )
        xv(mgs,lc) = xmas(mgs,lc)/xdn(mgs,lc)
      ELSE
       IF ( ipconc .lt. 2 ) THEN
         cx(mgs,lc) = rho0(mgs)*ccn/rho00 
       ENDIF
       IF ( qx(mgs,lc) .gt. qxmin(lc) .and. cx(mgs,lc) .gt. 0.01 ) THEN 
        xmas(mgs,lc) =  &
     &     min( max(qx(mgs,lc)*rho0(mgs)/cx(mgs,lc),xdn(mgs,lc)*xvmn(lc)), &
     &      xdn(mgs,lc)*xvmx(lc) )
        
        xv(mgs,lc) = xmas(mgs,lc)/xdn(mgs,lc)
        cx(mgs,lc) = qx(mgs,lc)*rho0(mgs)/xmas(mgs,lc)
        
       ELSEIF ( qx(mgs,lc) .gt. qxmin(lc) .and. cx(mgs,lc) .le. 0.01 ) THEN
        xmas(mgs,lc) = xdn(mgs,lc)*4.*pi/3.*(5.0e-6)**3
        cx(mgs,lc) = rho0(mgs)*qx(mgs,lc)/xmas(mgs,lc)
        xv(mgs,lc) = xmas(mgs,lc)/xdn(mgs,lc)
        
       ELSE
        xmas(mgs,lc) = cwmasn
        xv(mgs,lc) = xmas(mgs,lc)/1000.

       ENDIF 
      ENDIF 








      xdia(mgs,lc,1) = (xmas(mgs,lc)*cwc1)**(1./3.)
      xdia(mgs,lc,2) = xdia(mgs,lc,1)**2
      xdia(mgs,lc,3) = xdia(mgs,lc,1)
      cwrad = 0.5*xdia(mgs,lc,1)
      IF ( fadvisc(mgs) > 0.0 ) THEN
      vtxbar(mgs,lc,1) =  &
     &   (2.0*gr*xdn(mgs,lc) *(cwrad**2)) &
     &  /(9.0*fadvisc(mgs))
      ELSE
       vtxbar(mgs,lc,1) = 0.0
      ENDIF

      
      ELSE
       xmas(mgs,lc) = cwmasn
       IF ( ipconc .le. 1 ) cx(mgs,lc) = 0.01
       xdia(mgs,lc,1) = 2.*cwradn
       xdia(mgs,lc,2) = 4.*cwradn**2
       xdia(mgs,lc,3) = xdia(mgs,lc,1)
       vtxbar(mgs,lc,1) = 0.0
       
      ENDIF 
      
      end do
      
      ENDIF











      if ( ndebug1 .gt. 0 ) write(0,*) 'SETVTZ: Set cip'
      
      IF ( li .gt. 1 .and. ( ildo == 0 .or. ildo == li ) ) THEN
      do mgs = 1,ngscnt
       xdn(mgs,li)  = 900.0
      IF ( ipconc .eq. 0 ) THEN

        cx(mgs,li) = cnina(mgs)
       IF ( cimna .gt. 1.0 ) THEN
         cx(mgs,li) = Max(cimna,cx(mgs,li))
       ENDIF
       IF ( cimxa .gt. 1.0 ) THEN
         cx(mgs,li) = Min(cimxa,cx(mgs,li))
       ENDIF

       IF ( itype1a .ge. 1 .or. itype2a .ge. 1 ) THEN
        cx(mgs,li) = Max(cx(mgs,li),qx(mgs,li)*rho0(mgs)/cimasx)
        cx(mgs,li) = Min(cx(mgs,li),qx(mgs,li)*rho0(mgs)/cimasn)
       ENDIF

       cx(mgs,li) = max(1.0e-20,cx(mgs,li))


      
      ELSEIF ( ipconc .ge. 1 ) THEN
        IF ( qx(mgs,li) .gt. qxmin(li) ) THEN
         cx(mgs,li) = Max(cx(mgs,li),qx(mgs,li)*rho0(mgs)/cimasx)
         cx(mgs,li) = Min(cx(mgs,li),qx(mgs,li)*rho0(mgs)/cimasn)

        ENDIF
      ENDIF
      
      IF ( qx(mgs,li) .gt. qxmin(li) ) THEN
      xmas(mgs,li) = &
     &     max( qx(mgs,li)*rho0(mgs)/cx(mgs,li), cimasn )

      



      if ( xmas(mgs,li) .gt. 0.0 ) THEN 




       IF ( ixtaltype == 1 ) THEN 
       xdia(mgs,li,1) = 0.1871*(xmas(mgs,li)**(0.3429))
       xdia(mgs,li,3) = 0.1871*(xmas(mgs,li)**(0.3429))
       ELSEIF  ( ixtaltype == 2 ) THEN 
        xdia(mgs,li,1) = 0.277823*xmas(mgs,li)**0.359971
        xdia(mgs,li,3) = 0.277823*xmas(mgs,li)**0.359971
       ENDIF
      end if




       IF ( ipconc .ge. 0 ) THEN


        xv(mgs,li) = xmas(mgs,li)/xdn(mgs,li)
        IF ( ixtaltype == 1 ) THEN 
        tmp = (67056.6300748612*rhovt(mgs))/  &
     &   (((1.0 + cinu)/xv(mgs,li))**0.4716666666666667*gfcinu1)
        vtxbar(mgs,li,2) = tmp*gfcinu1p47
        vtxbar(mgs,li,1) = tmp*gfcinu2p47/(1. + cinu)
        vtxbar(mgs,li,3) = vtxbar(mgs,li,1) 
        ELSEIF  ( ixtaltype == 2 ) THEN 
        tmp = (67056.6300748612*rhovt(mgs))/  &
     &   (((1.0 + cinu)/xv(mgs,li))**0.4716666666666667*gfcinu1)
        vtxbar(mgs,li,2) = tmp*gfcinu1p47
        vtxbar(mgs,li,1) = tmp*gfcinu2p47/(1. + cinu)
        vtxbar(mgs,li,3) = vtxbar(mgs,li,1) 
        
        ENDIF



        xdia(mgs,li,2) = xdia(mgs,li,1)**2

       ELSE
         xdia(mgs,li,1) = max(xdia(mgs,li,1), 10.e-6)
         xdia(mgs,li,1) = min(xdia(mgs,li,1), 1000.e-6)
         vtxbar(mgs,li,1) = (4.942e4)*(xdia(mgs,li,1)**(1.4150))

         xdn(mgs,li) = 900.0
         xdia(mgs,li,2) = xdia(mgs,li,1)**2
         vtxbar(mgs,li,1) = vtxbar(mgs,li,1)*rhovt(mgs)
         xv(mgs,li) = xmas(mgs,li)/xdn(mgs,li)
       ENDIF 
      ELSE
       xmas(mgs,li) = 1.e-13
       xdn(mgs,li)  = 900.0
       xdia(mgs,li,1) = 1.e-7
       xdia(mgs,li,2) = (1.e-14)
       xdia(mgs,li,3) = 1.e-7
       vtxbar(mgs,li,1) = 0.0


      ENDIF
      end do
      
      ENDIF 






      

      IF ( ildo == 0 .or. ildo == lr ) THEN
      do mgs = 1,ngscnt
      if ( qx(mgs,lr) .gt. qxmin(lr) ) then
      


      
      if ( ipconc .ge. 3 ) then
        xv(mgs,lr) = rho0(mgs)*qx(mgs,lr)/(xdn(mgs,lr)*Max(1.0e-11,cx(mgs,lr)))
        xvbarmax = xvmx(lr)
        IF ( imaxdiaopt == 1 ) THEN
          xvbarmax = xvmx(lr)
        ELSEIF ( imaxdiaopt == 2 ) THEN 
         IF ( imurain == 1 ) THEN
           xvbarmax = xvmx(lr)/((3. + alpha(mgs,lr))**3/((3. + alpha(mgs,lr))*(2. + alpha(mgs,lr))*(1. + alpha(mgs,lr))))
         ELSEIF ( imurain == 3 ) THEN
           
         ENDIF
        ELSEIF ( imaxdiaopt == 3 ) THEN 
         IF ( imurain == 1 ) THEN
           xvbarmax = xvmx(lr)/((4. + alpha(mgs,lr))**3/((3. + alpha(mgs,lr))*(2. + alpha(mgs,lr))*(1. + alpha(mgs,lr))))
         ELSEIF ( imurain == 3 ) THEN
           
         ENDIF
        ENDIF
       
        IF ( xv(mgs,lr) .gt. xvbarmax ) THEN
          xv(mgs,lr) = xvbarmax
          cx(mgs,lr) = rho0(mgs)*qx(mgs,lr)/(xvbarmax*xdn(mgs,lr))
        ELSEIF ( xv(mgs,lr) .lt. xvmn(lr) ) THEN
          xv(mgs,lr) = xvmn(lr)
          cx(mgs,lr) = rho0(mgs)*qx(mgs,lr)/(xvmn(lr)*xdn(mgs,lr))
        ENDIF


        xmas(mgs,lr) = xv(mgs,lr)*xdn(mgs,lr)
        xdia(mgs,lr,3) = (xmas(mgs,lr)*cwc1)**(1./3.) 
        IF ( imurain == 3 ) THEN

          xdia(mgs,lr,1) = xdia(mgs,lr,3) 
        ELSE 
          xdia(mgs,lr,1) = (6.*pii*xv(mgs,lr)/((alpha(mgs,lr)+3.)*(alpha(mgs,lr)+2.)*(alpha(mgs,lr)+1.)))**(1./3.)
        ENDIF






      ELSE
        xdia(mgs,lr,1) = &
     &  (qx(mgs,lr)*rho0(mgs)/(pi*xdn(mgs,lr)*cno(lr)))**(0.25) 
        xmas(mgs,lr) = xdn(mgs,lr)*(pi/6.)*xdia(mgs,lr,1)**3
        xdia(mgs,lr,3) = (xmas(mgs,lr)*cwc1)**(1./3.)
        cx(mgs,lr) = cno(lr)*xdia(mgs,lr,1)
        xv(mgs,lr) = rho0(mgs)*qx(mgs,lr)/(xdn(mgs,lr)*cx(mgs,lr))
      end if
      else
        xdia(mgs,lr,1) = 1.e-9
        xdia(mgs,lr,3) = 1.e-9
        xmas(mgs,lr) = xdn(mgs,lr)*(pi/6.)*xdia(mgs,lr,1)**3

      end if
      xdia(mgs,lr,2) = xdia(mgs,lr,1)**2

      end do
      
      ENDIF





      IF ( ls .gt. 1 .and. ( ildo == 0 .or. ildo == ls ) ) THEN
      
      do mgs = 1,ngscnt 
      if ( qx(mgs,ls) .gt. qxmin(ls) ) then
      if ( ipconc .ge. 4 ) then 

        xv(mgs,ls) = rho0(mgs)*qx(mgs,ls)/(xdn(mgs,ls)*Max(1.0e-9,cx(mgs,ls)))

        xmas(mgs,ls) = xv(mgs,ls)*xdn(mgs,ls)

        IF ( xv(mgs,ls) .lt. xvmn(ls) .or. xv(mgs,ls) .gt. xvmx(ls) ) THEN
          xv(mgs,ls) = Min( xvmx(ls), Max( xvmn(ls),xv(mgs,ls) ) )
          xmas(mgs,ls) = xv(mgs,ls)*xdn(mgs,ls)
          cx(mgs,ls) = rho0(mgs)*qx(mgs,ls)/(xmas(mgs,ls))
        ENDIF

        xdia(mgs,ls,1) = (xv(mgs,ls)*cwc0*6.0)**(1./3.)
        xdia(mgs,ls,3) = xdia(mgs,ls,1)

      ELSE
        xdia(mgs,ls,1) =  &
     &    (qx(mgs,ls)*rho0(mgs)/(pi*xdn(mgs,ls)*cnostmp(mgs)))**(0.25) 
        cx(mgs,ls) = cnostmp(mgs)*xdia(mgs,ls,1)
        xv(mgs,ls) = rho0(mgs)*qx(mgs,ls)/(xdn(mgs,ls)*cx(mgs,ls))
        xdia(mgs,ls,3) = (xv(mgs,ls)*cwc0*6.0)**(1./3.)
      end if
      else
      xdia(mgs,ls,1) = 1.e-9
      xdia(mgs,ls,3) = 1.e-9
      cx(mgs,ls) = 0.0
      end if
      xdia(mgs,ls,2) = xdia(mgs,ls,1)**2


      end do
      
      ENDIF 







      IF ( lh .gt. 1 .and. ( ildo == 0 .or. ildo == lh ) ) THEN
      
      do mgs = 1,ngscnt 
      if ( qx(mgs,lh) .gt. qxmin(lh) ) then
      if ( ipconc .ge. 5 ) then

        xv(mgs,lh) = rho0(mgs)*qx(mgs,lh)/(xdn(mgs,lh)*Max(1.0e-9,cx(mgs,lh)))
        xmas(mgs,lh) = xv(mgs,lh)*xdn(mgs,lh)

        IF ( xv(mgs,lh) .lt. xvmn(lh) .or. xv(mgs,lh) .gt. xvmx(lh) ) THEN
          xv(mgs,lh) = Min( xvmx(lh), Max( xvmn(lh),xv(mgs,lh) ) )
          xmas(mgs,lh) = xv(mgs,lh)*xdn(mgs,lh)
          cx(mgs,lh) = rho0(mgs)*qx(mgs,lh)/(xmas(mgs,lh))
        ENDIF

         xdia(mgs,lh,3) = (xv(mgs,lh)*6.*pii)**(1./3.) 
         IF ( dmuh == 1.0 ) THEN
           xdia(mgs,lh,1) = cwch(mgs)*xdia(mgs,lh,3)
         ELSE
           xdia(mgs,lh,1) = (xv(mgs,lh)*cwch(mgs))**(1./3.)
         ENDIF

      ELSE
      xdia(mgs,lh,1) =  &
     &  (qx(mgs,lh)*rho0(mgs)/(pi*xdn(mgs,lh)*cno(lh)))**(0.25) 
      cx(mgs,lh) = cno(lh)*xdia(mgs,lh,1)
      xv(mgs,lh) = Max(xvmn(lh), rho0(mgs)*qx(mgs,lh)/(xdn(mgs,lh)*cx(mgs,lh)) )
      xdia(mgs,lh,3) = (xv(mgs,lh)*6./pi)**(1./3.) 
      end if
      else
      xdia(mgs,lh,1) = 1.e-9
      xdia(mgs,lh,3) = 1.e-9
      end if
      xdia(mgs,lh,2) = xdia(mgs,lh,1)**2


      end do
      
      ENDIF







      IF ( lhl .gt. 1 .and. ( ildo == 0 .or. ildo == lhl ) ) THEN
      
      do mgs = 1,ngscnt 
      if ( qx(mgs,lhl) .gt. qxmin(lhl) ) then
      if ( ipconc .ge. 5 ) then

        xv(mgs,lhl) = rho0(mgs)*qx(mgs,lhl)/(xdn(mgs,lhl)*Max(1.0e-9,cx(mgs,lhl)))
        xmas(mgs,lhl) = xv(mgs,lhl)*xdn(mgs,lhl)


        IF ( xv(mgs,lhl) .lt. xvmn(lhl) .or. xv(mgs,lhl) .gt. xvmx(lhl) ) THEN
          xv(mgs,lhl) = Min( xvmx(lhl), Max( xvmn(lhl),xv(mgs,lhl) ) )
          xmas(mgs,lhl) = xv(mgs,lhl)*xdn(mgs,lhl)
          cx(mgs,lhl) = rho0(mgs)*qx(mgs,lhl)/(xmas(mgs,lhl))
        ENDIF

        xdia(mgs,lhl,3) = (xv(mgs,lhl)*6./pi)**(1./3.) 
         IF ( dmuhl == 1.0 ) THEN
           xdia(mgs,lhl,1) = cwchl(mgs)*xdia(mgs,lhl,3)
         ELSE
           xdia(mgs,lhl,1) = (xv(mgs,lhl)*cwchl(mgs))**(1./3.)
         ENDIF
        

      ELSE
      xdia(mgs,lhl,1) = &
     &  (qx(mgs,lhl)*rho0(mgs)/(pi*xdn(mgs,lhl)*cno(lhl)))**(0.25) 
      cx(mgs,lhl) = cno(lhl)*xdia(mgs,lhl,1)
      xv(mgs,lhl) = Max(xvmn(lhl), rho0(mgs)*qx(mgs,lhl)/(xdn(mgs,lhl)*cx(mgs,lhl)) )
      xdia(mgs,lhl,3) = (xv(mgs,lhl)*6./pi)**(1./3.) 
      end if
      else
      xdia(mgs,lhl,1) = 1.e-9
      xdia(mgs,lhl,3) = 1.e-9
      end if
      xdia(mgs,lhl,2) = xdia(mgs,lhl,1)**2


      end do
      
      ENDIF















      if ( ndebug1 .gt. 0 ) write(0,*) 'SETVTZ: Set terminal velocities'






      IF ( ildo == 0 .or. ildo == lr ) THEN
      do mgs = 1,ngscnt
      if ( qx(mgs,lr) .gt. qxmin(lr) ) then
      IF ( ipconc .lt. 3 ) THEN
        vtxbar(mgs,lr,1) = rainfallfac*(ar*gf4br/6.0)*(xdia(mgs,lr,1)**br)*rhovt(mgs)

      ELSE
        
        IF ( imurain == 1 ) THEN 
        
        
        

        
          alp = alpha(mgs,lr)
          
          vtxbar(mgs,lr,1) = rhovt(mgs)*arx*(1.0 - (1.0 + frx*xdia(mgs,lr,1))**(-alp - 4.0) ) 
          
          IF ( infdo .ge. 1 .and. rssflg == 1 ) THEN
            vtxbar(mgs,lr,2) = rhovt(mgs)*arx*(1.0 - (1.0 + frx*xdia(mgs,lr,1))**(-alp - 1.0) ) 
          ELSE
            vtxbar(mgs,lr,2) = vtxbar(mgs,lr,1)
          ENDIF
          
          IF ( infdo .ge. 2 .and. rssflg == 1 ) THEN
            vtxbar(mgs,lr,3) = rhovt(mgs)*arx*(1.0 - (1.0 + frx*xdia(mgs,lr,1))**(-alp - 7.0) ) 
          ELSE
            vtxbar(mgs,lr,3) = vtxbar(mgs,lr,1)
          ENDIF
          


        ELSEIF ( imurain == 3 ) THEN 
        
        IF ( lzr < 1 ) THEN 
        rwdia = Min( xdia(mgs,lr,1), 8.0e-3 )
        
         vtxbar(mgs,lr,1) = rhovt(mgs)*6.0*pii*( 0.04771 + 3788.0*rwdia -  &
     &        1.105e6*rwdia**2 + 1.412e8*rwdia**3 - 6.527e9*rwdia**4)
        
        IF ( infdo .ge. 1 ) THEN
         vtxbar(mgs,lr,2) = (0.09112 + 2714.0*rwdia - 4.872e5*rwdia**2 +  &
     &            4.495e7*rwdia**3 - 1.626e9*rwdia**4)*rhovt(mgs)
        ENDIF
        
        IF ( infdo .ge. 2 ) THEN 
        vtxbar(mgs,lr,3)  = rhovt(mgs)*(  &
     &       0.0911229 +                  &
     &  9246.494*(rwdia) -               &
     &  3.2839926e6*(rwdia**2) +          &
     &  4.944093e8*(rwdia**3) -          &
     &  2.631718e10*(rwdia**4) )
        ENDIF
        
        ELSE 

        vr = xv(mgs,lr)
        rnux = alpha(mgs,lr)
        
        IF ( infdo .ge. 1 .and. rssflg == 1) THEN 
        vtxbar(mgs,lr,2) = rhovt(mgs)*                             &
     &     (((1. + rnux)/vr)**(-1.333333)*                         &
     &    (0.0911229*((1. + rnux)/vr)**1.333333*Gamma(1. + rnux) + &
     &      (5430.3131*(1. + rnux)*Gamma(4./3. + rnux))/           &
     &       vr - 1.0732802e6*((1. + rnux)/vr)**0.6666667*         &
     &       Gamma(1.666667 + rnux) +                              &
     &      8.584110982429507e7*((1. + rnux)/vr)**(1./3.)*         &
     &       Gamma(2. + rnux) -                                    &
     &      2.3303765697228556e9*Gamma(7./3. + rnux)))/            &
     &  Gamma(1. + rnux)
        ENDIF


       vtxbar(mgs,lr,1)  = rhovt(mgs)*                                                 &
     &   (0.0911229*(1 + rnux)**1.3333333333333333*Gamma(2. + rnux) +                  &
     &    5430.313059683277*(1 + rnux)*vr**0.3333333333333333*                         &
     &     Gamma(2.333333333333333 + rnux) -                                           &
     &    1.0732802065650471e6*(1 + rnux)**0.6666666666666666*vr**0.6666666666666666*  &
     &     Gamma(2.6666666666666667 + rnux) +                                          &
     &    8.584110982429507e7*(1 + rnux)**0.3333333333333333*vr*Gamma(3 + rnux) -      &
     &    2.3303765697228556e9*vr**1.3333333333333333*                                 &
     &     Gamma(3.333333333333333 + rnux))/                                           &
     &  ((1 + rnux)**2.333333333333333*Gamma(1 + rnux)) 
     
        IF(infdo .ge. 1 .and. rssflg == 0) THEN 
          vtxbar(mgs,lr,2) = vtxbar(mgs,lr,1)
        ENDIF     
      
        IF ( infdo .ge. 2 .and. rssflg == 1) THEN 
        vtxbar(mgs,lr,3)  =   rhovt(mgs)*                                          &
     &  ((1. + rnux)*(0.0911229*(1 + rnux)**1.3333333333333333*Gamma(3. + rnux) +  &
     &      5430.313059683277*(1 + rnux)*vr**0.3333333333333333*                   &
     &       Gamma(3.3333333333333335 + rnux) -                                    &
     &      1.0732802065650471e6*(1 + rnux)**0.6666666666666666*                   &
     &       vr**0.6666666666666666*Gamma(3.6666666666666665 + rnux) +             &
     &      8.5841109824295e7*(1 + rnux)**0.3333333333333333*vr*Gamma(4. + rnux) - &
     &      2.3303765697228556e9*vr**1.3333333333333333*                           &
     &       Gamma(4.333333333333333 + rnux)))/                                    &
     &  ((1 + rnux)**3.3333333333333335*(2 + rnux)*Gamma(1 + rnux))
        


        
        ELSEIF (infdo .ge. 2) THEN 
          vtxbar(mgs,lr,3) = vtxbar(mgs,lr,1)
        ENDIF
        
        
        ENDIF
       ENDIF 











      ENDIF 
      else  
      vtxbar(mgs,lr,1) = 0.0
      vtxbar(mgs,lr,2) = 0.0
      end if
      end do
      if ( ndebug1 .gt. 0 ) write(0,*) 'SETVTZ: Set rain vt'
      
      ENDIF





      IF ( ls .gt. 1 .and. ( ildo == 0 .or. ildo == ls ) ) THEN
      do mgs = 1,ngscnt
      if ( qx(mgs,ls) .gt. qxmin(ls) ) then
        IF ( ipconc .ge. 4 ) THEN
         if ( mixedphase .and. qsvtmod ) then
         else
           vtxbar(mgs,ls,1) = 5.72462*rhovt(mgs)*(xv(mgs,ls))**(1./12.)
          IF(sssflg == 1) THEN
            vtxbar(mgs,ls,2) = 4.04091*rhovt(mgs)*(xv(mgs,ls))**(1./12.)
          ELSE
            vtxbar(mgs,ls,2) = vtxbar(mgs,ls,1)
          ENDIF
          vtxbar(mgs,ls,3) = vtxbar(mgs,ls,1)
         endif
        ELSE
         vtxbar(mgs,ls,1) = (cs*gf4ds/6.0)*(xdia(mgs,ls,1)**ds)*rhovt(mgs)
         vtxbar(mgs,ls,2) = vtxbar(mgs,ls,1)
        ENDIF
      else
      vtxbar(mgs,ls,1) = 0.0
      end if
      end do
      if ( ndebug1 .gt. 0 ) write(0,*) 'SETVTZ: Set snow vt'
      
      ENDIF 






      IF ( lh .gt. 1 .and. ( ildo == 0 .or. ildo == lh ) ) THEN
      
      do mgs = 1,ngscnt
      vtxbar(mgs,lh,1) = 0.0
      if ( qx(mgs,lh) .gt. qxmin(lh) ) then
       IF ( icdx .eq. 1 ) THEN
         cd = cdx(lh)
       ELSEIF ( icdx .eq. 2 ) THEN


         cd = Max(0.45, Min(1.0, 0.45 + 0.35*(800.0 - Max( 500., Min( 800.0, xdn(mgs,lh) ) ) )/(800. - 500.) ) )

       ELSEIF ( icdx .eq. 3 ) THEN

         cd = Max(0.45, Min(1.2, 0.45 + 0.55*(800.0 - Max( 170.0, Min( 800.0, xdn(mgs,lh) ) ) )/(800. - 170.0) ) )
       ELSEIF ( icdx .eq. 4 ) THEN
         cd = Max(cdhmin, Min(cdhmax, cdhmin + (cdhmax-cdhmin)* &
     &        (cdhdnmax - Max( cdhdnmin, Min( cdhdnmax, xdn(mgs,lh) ) ) )/(cdhdnmax - cdhdnmin) ) )
       ELSEIF ( icdx .eq. 5 ) THEN
         cd = cdx(lh)*(xdn(mgs,lh)/rho_qh)**(2/3)
       ELSEIF ( icdx .eq. 6 ) THEN 
         indxr = Int( (xdn(mgs,lh)-50.)/100. ) + 1
         indxr = Min( ngdnmm, Max(1,indxr) )
         
         
         delrho = Max( 0.0, 0.01*(xdn(mgs,lh) - mmgraupvt(indxr,1)) )
         IF ( indxr < ngdnmm ) THEN
          
          axh(mgs) = mmgraupvt(indxr,2) + delrho*(mmgraupvt(indxr+1,2) - mmgraupvt(indxr,2) )
          bxh(mgs) = mmgraupvt(indxr,3) + delrho*(mmgraupvt(indxr+1,3) - mmgraupvt(indxr,3) )

          
         ELSE
          axh(mgs) = mmgraupvt(indxr,2)
          bxh(mgs) = mmgraupvt(indxr,3)
         ENDIF
         
         aax = axh(mgs)
         bbx = bxh(mgs)
         
       ENDIF
       
      IF ( alpha(mgs,lh) .eq. 0.0 .and. icdx > 0 .and. icdx /= 6 ) THEN
      vtxbar(mgs,lh,1) = (gf4p5/6.0)*  &
     &  Sqrt( (xdn(mgs,lh)*xdia(mgs,lh,1)*4.0*gr) /  &
     &    (3.0*cd*rho0(mgs)) )
      ELSE
        IF ( icdx /= 6 ) bbx = bx(lh)
        tmp = 4. + alpha(mgs,lh) + bbx
        i = Int(dgami*(tmp))
        del = tmp - dgam*i
        x = gmoi(i) + (gmoi(i+1) - gmoi(i))*del*dgami

        tmp = 4. + alpha(mgs,lh)
        i = Int(dgami*(tmp))
        del = tmp - dgam*i
        y = gmoi(i) + (gmoi(i+1) - gmoi(i))*del*dgami
        


        
        IF ( icdx > 0 .and. icdx /= 6) THEN
          aax = Sqrt(4.0*xdn(mgs,lh)*gr/(3.0*cd*rho00))
          vtxbar(mgs,lh,1) =  rhovt(mgs)*aax* Sqrt(xdia(mgs,lh,1)) * x/y
        ELSEIF (icdx == 6 ) THEN
          vtxbar(mgs,lh,1) =  rhovt(mgs)*aax* xdia(mgs,lh,1)**bbx * x/y
        ELSE
          vtxbar(mgs,lh,1) =  rhovt(mgs)*ax(lh)*(xdia(mgs,lh,1)**bx(lh)*x)/y          
        ENDIF


      ENDIF

      IF ( lwsm6 .and. ipconc == 0 ) THEN

         vtxbar(mgs,lh,1) = (330.*gf4br/6.0)*(xdia(mgs,lh,1)**br)*rhovt(mgs)
      ENDIF
      
      end if
      end do
      if ( ndebug1 .gt. 0 ) write(0,*) 'SETVTZ: Set hail vt'
      
      ENDIF 






      IF ( lhl .gt. 1 .and. ( ildo == 0 .or. ildo == lhl ) ) THEN
      
      do mgs = 1,ngscnt
      vtxbar(mgs,lhl,1) = 0.0
      if ( qx(mgs,lhl) .gt. qxmin(lhl) ) then

       IF ( icdxhl .eq. 1 ) THEN
         cd = cdx(lhl)
       ELSEIF ( icdxhl .eq. 3 ) THEN

         cd = Max(0.45, Min(1.2, 0.45 + 0.55*(800.0 - Max( 170.0, Min( 800.0, xdn(mgs,lhl) ) ) )/(800. - 170.0) ) )
       ELSEIF ( icdxhl .eq. 4 ) THEN
         cd = Max(cdhlmin, Min(cdhlmax, cdhlmin + (cdhlmax-cdhlmin)*  &
     &       (cdhldnmax - Max( cdhldnmin, Min( cdhldnmax, xdn(mgs,lhl) ) ) )/(cdhldnmax - cdhldnmin) ) )
       ELSEIF ( icdxhl .eq. 6 ) THEN 
         indxr = Int( (xdn(mgs,lhl)-50.)/100. ) + 1
         indxr = Min( ngdnmm, Max(1,indxr) )
         
         
         delrho = Max( 0.0, 0.01*(xdn(mgs,lhl) - mmgraupvt(indxr,1)) )
         IF ( indxr < ngdnmm ) THEN
          
          axhl(mgs) = mmgraupvt(indxr,2) + delrho*(mmgraupvt(indxr+1,2) - mmgraupvt(indxr,2) )
          bxhl(mgs) = mmgraupvt(indxr,3) + delrho*(mmgraupvt(indxr+1,3) - mmgraupvt(indxr,3) )

          
         ELSE
          axhl(mgs) = mmgraupvt(indxr,2)
          bxhl(mgs) = mmgraupvt(indxr,3)
         ENDIF
         
         aax = axhl(mgs)
         bbx = bxhl(mgs)
         
       ELSE


         cd = Max(0.45, Min(0.6, 0.45 + 0.15*(800.0 - Max( 500., Min( 800.0, xdn(mgs,lhl) ) ) )/(800. - 500.) ) )
       ENDIF

      IF ( alpha(mgs,lhl) .eq. 0.0 .and. icdxhl > 0 ) THEN
      vtxbar(mgs,lhl,1) = (gf4p5/6.0)* &
     &  Sqrt( (xdn(mgs,lhl)*xdia(mgs,lhl,1)*4.0*gr) / &
     &    (3.0*cd*rho0(mgs)) )
      ELSE
        IF ( icdx /= 6 ) bbx = bx(lhl)
        tmp = 4. + alpha(mgs,lhl) + bbx
        i = Int(dgami*(tmp))
        del = tmp - dgam*i
        x = gmoi(i) + (gmoi(i+1) - gmoi(i))*del*dgami

        tmp = 4. + alpha(mgs,lhl)
        i = Int(dgami*(tmp))
        del = tmp - dgam*i
        y = gmoi(i) + (gmoi(i+1) - gmoi(i))*del*dgami

        IF ( icdxhl > 0 .and. icdxhl /= 6) THEN
          aax = Sqrt(4.0*xdn(mgs,lhl)*gr/(3.0*cd*rho00))
          vtxbar(mgs,lhl,1) =  rhovt(mgs)*aax* Sqrt(xdia(mgs,lhl,1)) * x/y
        ELSEIF ( icdx == 6 ) THEN
          vtxbar(mgs,lhl,1) =  rhovt(mgs)*aax* (xdia(mgs,lhl,1))**bbx * x/y
        ELSE
         vtxbar(mgs,lhl,1) =  rhovt(mgs)*(ax(lhl)*xdia(mgs,lhl,1)**bx(lhl)*x)/y
        ENDIF
        

      ENDIF


      end if
      end do
      if ( ndebug1 .gt. 0 ) write(0,*) 'SETVTZ: Set hail vt'
      
      ENDIF 


      IF ( infdo .ge. 1 ) THEN



        DO mgs = 1,ngscnt
          vtxbar(mgs,lc,2) = vtxbar(mgs,lc,1)
        IF ( li .gt. 1 ) THEN










        ENDIF

        ENDDO

        IF ( lg .gt. lr ) THEN

        DO il = lg,lhab
         IF ( ildo == 0 .or. ildo == il ) THEN

            DO mgs = 1,ngscnt
             IF ( qx(mgs,il) .gt. qxmin(il) ) THEN
              IF ( (il .eq. lh .and. hssflg == 1) .or. ( lhl .gt. 1 .and. il .eq. lhl .and. hlssflg == 1) ) THEN 
              
              
              

              IF ( il .eq. lh ) THEN 
             
               IF ( icdx .eq. 1 ) THEN
                 cd = cdx(lh)
               ELSEIF ( icdx .eq. 2 ) THEN


                 cd = Max(0.45, Min(1.0, 0.45 + 0.35*(800.0 - Max( 500., Min( 800.0, xdn(mgs,lh) ) ) )/(800. - 500.) ) )

               ELSEIF ( icdx .eq. 3 ) THEN

                 cd = Max(0.45, Min(1.2, 0.45 + 0.55*(800.0 - Max( 170.0, Min( 800.0, xdn(mgs,lh) ) ) )/(800. - 170.0) ) )
               ELSEIF ( icdx .eq. 4 ) THEN
                 cd = Max(cdhmin, Min(cdhmax, cdhmin + (cdhmax-cdhmin)* &
     &            (cdhdnmax - Max( cdhdnmin, Min( cdhdnmax, xdn(mgs,lh) ) ) )/(cdhdnmax - cdhdnmin) ) )
               ELSEIF ( icdx .eq. 5 ) THEN
                 cd = cdx(lh)*(xdn(mgs,lh)/rho_qh)**(2/3)
               ELSEIF ( icdx .eq. 6 ) THEN 
                  aax = axh(mgs)
                  bbx = bxh(mgs)
               ENDIF
               
              ELSEIF ( lhl .gt. 1 .and. il .eq. lhl ) THEN
             
               IF ( icdxhl .eq. 1 ) THEN
                 cd = cdx(lhl)
               ELSEIF ( icdxhl .eq. 3 ) THEN

                cd = Max(0.45, Min(1.2, 0.45 + 0.55*(800.0 - Max( 170.0, Min( 800.0, xdn(mgs,lhl) ) ) )/(800. - 170.0) ) )
               ELSEIF ( icdxhl .eq. 4 ) THEN
                cd = Max(cdhlmin, Min(cdhlmax, cdhlmin + (cdhlmax-cdhlmin)*  &
     &               (cdhldnmax - Max( cdhldnmin, Min( cdhldnmax, xdn(mgs,lhl) ) ) )/(cdhldnmax - cdhldnmin) ) )
               ELSEIF ( icdxhl == 5 ) THEN


                 cd = Max(0.45, Min(0.6, 0.45 + 0.15*(800.0 - Max( 500., Min( 800.0, xdn(mgs,lhl) ) ) )/(800. - 500.) ) )
               ELSEIF ( icdxhl .eq. 6 ) THEN 
                  aax = axhl(mgs)
                  bbx = bxhl(mgs)
               ENDIF
               
              ENDIF 

               IF ( alpha(mgs,il) .eq. 0. .and. infdo .lt. 2 .and.   &
               ( ( il==lh .and. icdx > 0 .and. icdx /= 6) .or. ( il==lhl .and. icdxhl > 0 .and. icdxhl /= 6 ) ) ) THEN 
                 vtxbar(mgs,il,2) =   &
     &              Sqrt( (xdn(mgs,il)*xdia(mgs,il,1)*pi*gr) / &
     &                (3.0*cd*rho0(mgs)) )

               ELSE
               IF ( il == lh  .and. icdx   /= 6 ) bbx = bx(il)
               IF ( il == lhl .and. icdxhl /= 6 ) bbx = bx(il)
               tmp = 1. + alpha(mgs,il) + bbx
               i = Int(dgami*(tmp))
               del = tmp - dgam*i
               x = gmoi(i) + (gmoi(i+1) - gmoi(i))*del*dgami
  
               tmp = 1. + alpha(mgs,il)
               i = Int(dgami*(tmp))
               del = tmp - dgam*i
               y = gmoi(i) + (gmoi(i+1) - gmoi(i))*del*dgami

                 IF ( il .eq. lh  .or. il .eq. lhl) THEN 
                   IF ( ( il==lh .and. icdx > 0 ) ) THEN
                     IF ( icdx /= 6 ) THEN
                      aax = Sqrt(4.0*xdn(mgs,il)*gr/(3.0*cd*rho00))
                      vtxbar(mgs,il,2) =  rhovt(mgs)*aax* xdia(mgs,il,1)**0.5 * x/y
                     ELSE 
                       vtxbar(mgs,il,2) =  rhovt(mgs)*aax* xdia(mgs,il,1)**bbx * x/y
                     ENDIF





                   ELSEIF ( ( il==lhl .and. icdxhl > 0 ) ) THEN
                     IF ( icdxhl /= 6 ) THEN
                       aax = Sqrt(4.0*xdn(mgs,il)*gr/(3.0*cd*rho00))
                       vtxbar(mgs,il,2) =  rhovt(mgs)*aax* xdia(mgs,il,1)**0.5 * x/y
                     ELSE 
                       vtxbar(mgs,il,2) =  rhovt(mgs)*aax* xdia(mgs,il,1)**bbx * x/y
                     ENDIF
                   ELSE
                     aax = ax(il)
                     vtxbar(mgs,il,2) =  rhovt(mgs)*ax(il)*(xdia(mgs,il,1)**bx(il)*x)/y
                   ENDIF







                  IF ( infdo .ge. 2 ) THEN 
                   vtxbar(mgs,il,3) = rhovt(mgs)*                 &
     &                (aax*(1.0/xdia(mgs,il,1) )**(- bx(il))*  &
     &                 Gamma(7.0 + alpha(mgs,il) + bx(il)))/Gamma(7. + alpha(mgs,il))
                  ENDIF

      if ( ndebug1 .gt. 0 ) write(0,*) 'SETVTZ: Set hail vt3'

                 ELSE 
                  vtxbar(mgs,il,2) =  &
     &               rhovt(mgs)*(ax(il)*xdia(mgs,il,1)**bx(il)* &
     &               x)/y

                 IF ( infdo .ge. 2 ) THEN 
                  vtxbar(mgs,il,3) = rhovt(mgs)*                 &
     &              (ax(il)*(1.0/xdia(mgs,il,1) )**(- bx(il))*  &
     &               Gamma(7.0 + alpha(mgs,il) + bx(il)))/Gamma(7. + alpha(mgs,il))
                  ENDIF

      if ( ndebug1 .gt. 0 ) write(0,*) 'SETVTZ: Set hail vt4'

                 ENDIF 

               ENDIF 










             ELSEIF ( (il .eq. lh .and. hssflg == 0) .or. ( lhl .gt. 1 .and. il .eq. lhl .and. hlssflg == 0) ) THEN 
              vtxbar(mgs,il,2) = vtxbar(mgs,il,1)
              vtxbar(mgs,il,3) = vtxbar(mgs,il,1)
             ELSE 
              vtxbar(mgs,il,2) = &
     &            Sqrt( (xdn(mgs,il)*xdia(mgs,il,1)*pi*gr) /  &
     &              (3.0*cdx(il)*rho0(mgs)) )
              vtxbar(mgs,il,3) = vtxbar(mgs,il,1)

      if ( ndebug1 .gt. 0 ) write(0,*) 'SETVTZ: Set graupel vt5'


              ENDIF
             ELSE 
              vtxbar(mgs,il,2) = 0.0

      if ( ndebug1 .gt. 0 ) write(0,*) 'SETVTZ: Set graupel vt6'

             ENDIF
           ENDDO 

      if ( ndebug1 .gt. 0 ) write(0,*) 'SETVTZ: Set graupel vt7'

        ENDIF
        ENDDO 

      if ( ndebug1 .gt. 0 ) write(0,*) 'SETVTZ: Set graupel vt8'

        ENDIF 
        



      if ( ndebug1 .gt. 0 ) write(0,*) 'SETVTZ: Set graupel vt9'








      ENDIF 
      
      if ( ndebug1 .gt. 0 ) write(0,*) 'SETVTZ: END OF ROUTINE'



      RETURN
      END SUBROUTINE setvtz










      subroutine ziegfall1d(nx,ny,nz,nor,norz,na,dtp,jgs,ixcol, &
     &  xvt, rhovtzx,                                           &
     &  an,dn,ipconc0,t0,t7,cwmasn,cwmasx,       &
     &  cwradn,                                   &
     &  qxmin,xdnmx,xdnmn,cdx,cno,xdn0,xvmn,xvmx,  &
     &  ngs,qx,qxw,cx,xv,vtxbar,xmas,xdn,xdia,vx,alpha,zx,igs,kgs, &
     &  rho0,temcg,temg,rhovt,cwnc,cinc,fadvisc,cwdia,cipmas,cnina,cimas, &
     &  cnostmp,                     &
     &  infdo,ildo,timesetvt)













      
      implicit none
      integer ng1
      parameter(ng1 = 1)
      
      integer, intent(in) :: ixcol 
      integer, intent(in) :: ildo
      
      integer nx,ny,nz,nor,norz,ngt,jgs,na
      real an(-nor+ng1:nx+nor,-nor+ng1:ny+nor,-nor+ng1:nz+nor,na)
      real dn(-nor+ng1:nx+nor,-nor+ng1:ny+nor,-nor+ng1:nz+nor)
      real t0(-nor+ng1:nx+nor,-nor+ng1:ny+nor,-nor+ng1:nz+nor)
      real t7(-nor+ng1:nx+nor,-nor+ng1:ny+nor,-nor+ng1:nz+nor)
      real dtp,dtz1
      
      real :: rhovtzx(nz,nx)
      
      integer ndebugzf
      parameter (ndebugzf = 0)

      integer ix,jy,kz,i,j,k,il
      integer infdo


      real xvt(nz+1,nx,3,lc:lhab) 

      real qxmin(lc:lhab)
      real xdn0(lc:lhab)
      real xvmn(lc:lhab), xvmx(lc:lhab)
      double precision,optional :: timesetvt

      integer :: ngs
      integer :: ngscnt,mgs,ipconc0

      
      real ::  qx(ngs,lv:lhab) 
      real ::  qxw(ngs,ls:lhab) 
      real ::  cx(ngs,lc:lhab) 
      real ::  xv(ngs,lc:lhab) 
      real ::  vtxbar(ngs,lc:lhab,3) 
      real ::  xmas(ngs,lc:lhab) 
      real ::  xdn(ngs,lc:lhab) 
      real ::  xdia(ngs,lc:lhab,3) 
      real ::  vx(ngs,li:lhab) 
      real ::  alpha(ngs,lr:lhab) 

      real ::  zx(ngs,lr:lhab) 


      real xdnmx(lc:lhab), xdnmn(lc:lhab)




      real cdx(lc:lhab)



      real cno(lc:lhab)
      
      real cwccn0,cwmasn,cwmasx,cwradn


      integer nxmpb,nzmpb,nxz,numgs,inumgs
      integer kstag
      parameter (kstag=1)

      integer igs(ngs),kgs(ngs)
      
      real rho0(ngs),temcg(ngs)

      real temg(ngs)
      
      real rhovt(ngs)
      
      real cwnc(ngs),cinc(ngs)
      real fadvisc(ngs),cwdia(ngs),cipmas(ngs)
      

      real :: cnina(ngs),cimas(ngs)
      
      real :: cnostmp(ngs)










      
      logical flag
      logical ldoliq
      
    
      real chw, qr, z, rd, alp, z1, g1, vr, nrx, tmp
      
      real vtmax
      real xvbarmax
      
      integer l1, l2
      
      double precision :: dpt1, dpt2





      integer :: ixb, jyb, kzb
      integer :: ixe, jye, kze

      logical :: debug_mpi = .false.


      if (ndebugzf .gt. 0 ) write(0,*) "ZIEGFALL: ENTERED SUBROUTINE"









      ldoliq = .false.
      IF ( ls .gt. 1 ) THEN
      DO il = ls,lhab
        ldoliq = ldoliq .or. ( lliq(il) .gt. 1 )
      ENDDO
      ENDIF
      

















      




      









      jy = jgs
      nxmpb = ixcol
      nzmpb = 1
      nxz = 1*nz

      numgs = 1

      IF ( ildo == 0 ) THEN
        l1 = lc
        l2 = lhab
      ELSE
        l1 = ildo
        l2 = ildo
      ENDIF


      do inumgs = 1,numgs
       ngscnt = 0


       do kz = nzmpb,nz
        do ix = ixcol,ixcol
        flag = .false.

        
        DO il = l1,l2
          flag =  flag .or. ( an(ix,jy,kz,il)  .gt. qxmin(il) ) 
        ENDDO

        if ( flag ) then


        ngscnt = ngscnt + 1
        igs(ngscnt) = ix
        kgs(ngscnt) = kz
        if ( ngscnt .eq. ngs ) goto 1100
        end if

        end do 

        nxmpb = 1
       end do 



 1100 continue

      if ( ngscnt .eq. 0 ) go to 9998








      do mgs = 1,ngscnt

       rho0(mgs) = dn(igs(mgs),jy,kgs(mgs))
       rhovt(mgs) = rhovtzx(kgs(mgs),ixcol) 
       temg(mgs) = t0(igs(mgs),jy,kgs(mgs))
       temcg(mgs) = temg(mgs) - tfr

        

      end do


      IF ( lc .gt. 1 .and. (ildo == 0 .or. ildo == lc ) ) then
        do mgs = 1,ngscnt
         fadvisc(mgs) = advisc0*(416.16/(temg(mgs)+120.0))* &
     &   (temg(mgs)/296.0)**(1.5)
        end do
      ENDIF

      IF ( ipconc .eq. 0 ) THEN
      do mgs = 1,ngscnt
      cnina(mgs) = t7(igs(mgs),jgs,kgs(mgs))
      end do
      ENDIF


      IF ( ildo > 0 ) THEN
        vtxbar(:,ildo,:) = 0.0
      ELSE
        vtxbar(:,:,:) = 0.0
      ENDIF
      



      DO il = l1,l2
      do mgs = 1,ngscnt
        qx(mgs,il) = max(an(igs(mgs),jy,kgs(mgs),il), 0.0) 
      ENDDO
      end do
      
      cnostmp(:) = cno(ls)
      IF ( ipconc < 1 .and. lwsm6 .and. (ildo == 0 .or. ildo == ls )) THEN
        DO mgs = 1,ngscnt
          tmp = Min( 0.0, temcg(mgs) )
          cnostmp(mgs) = Min( 2.e8, 2.e6*exp(0.12*tmp) )
        ENDDO
      ENDIF





      cx(:,:) = 0.0
      
      if ( ipconc .ge. 1 .and. li .gt. 1 .and. (ildo == 0 .or. ildo == li ) ) then
       do mgs = 1,ngscnt
        cx(mgs,li) = Max(an(igs(mgs),jy,kgs(mgs),lni), 0.0)
       end do
      end if
      if ( ipconc .ge. 2 .and. lc .gt. 1 .and. (ildo == 0 .or. ildo == lc ) ) then
       do mgs = 1,ngscnt
        cx(mgs,lc) = Max(an(igs(mgs),jy,kgs(mgs),lnc), 0.0)
        cx(mgs,lc) = Min( ccwmx, cx(mgs,lc) )
       end do
      end if
      if ( ipconc .ge. 3 .and. lr .gt. 1 .and. (ildo == 0 .or. ildo == lr ) ) then
       do mgs = 1,ngscnt
        cx(mgs,lr) = Max(an(igs(mgs),jy,kgs(mgs),lnr), 0.0)




       end do
      end if
      if ( ipconc .ge. 4  .and. ls .gt. 1 .and. (ildo == 0 .or. ildo == ls ) ) then
       do mgs = 1,ngscnt
        cx(mgs,ls) = Max(an(igs(mgs),jy,kgs(mgs),lns), 0.0)




       end do
      end if

      if ( ipconc .ge. 5  .and. lh .gt. 1 .and. (ildo == 0 .or. ildo == lh ) ) then
       do mgs = 1,ngscnt

        cx(mgs,lh) = Max(an(igs(mgs),jy,kgs(mgs),lnh), 0.0)





       end do
      ENDIF

      if ( ipconc .ge. 5  .and. lhl .gt. 1 .and. (ildo == 0 .or. ildo == lhl ) ) then
       do mgs = 1,ngscnt

        cx(mgs,lhl) = Max(an(igs(mgs),jy,kgs(mgs),lnhl), 0.0)








       end do
      end if
       
      do mgs = 1,ngscnt
        xdn(mgs,lc) = xdn0(lc)
        xdn(mgs,lr) = xdn0(lr)


        IF ( li .gt. 1 )  xdn(mgs,li) = xdn0(li)
        IF ( ls .gt. 1 )  xdn(mgs,ls) = xdn0(ls)
        IF ( lh .gt. 1 )  xdn(mgs,lh) = xdn0(lh)
        IF ( lhl .gt. 1 ) xdn(mgs,lhl) = xdn0(lhl)
      end do




      IF ( ldovol .and. (ildo == 0 .or. ildo >= li ) ) THEN
      
      vx(:,:) = 0.0
      
       DO il = l1,l2
        
        IF ( lvol(il) .ge. 1 ) THEN
        
          DO mgs = 1,ngscnt
            vx(mgs,il) = Max(an(igs(mgs),jy,kgs(mgs),lvol(il)), 0.0)
            IF ( vx(mgs,il) .gt. rho0(mgs)*qxmin(il)*1.e-3 .and. qx(mgs,il) .gt. qxmin(il) ) THEN
              xdn(mgs,il) = Min( xdnmx(il), Max( xdnmn(il), rho0(mgs)*qx(mgs,il)/vx(mgs,il) ) )
            ENDIF
          ENDDO
          
        ENDIF
      
       ENDDO
      
      ENDIF

      DO il = lg,lhab
      DO mgs = 1,ngscnt
        alpha(mgs,il) = dnu(il)
      ENDDO
      ENDDO
      
      alpha(:,lr) = xnu(lr)
       









      if (ndebugzf .gt. 0 ) write(0,*)  'ZIEGFALL: call setvtz'

      
      call setvtz(ngscnt,ngs,qx,qxmin,qxw,cx,rho0,rhovt,xdia,cno,cnostmp,   &
     &                 xmas,vtxbar,xdn,xvmn,xvmx,xv,cdx,        &
     &                 ipconc,ndebugzf,ngs,nz,kgs,fadvisc, &
     &                 cwmasn,cwmasx,cwradn,cnina,cimn,cimx,    &
     &                 itype1,itype2,temcg,infdo,alpha,ildo)





      DO il = l1,l2
      do mgs = 1,ngscnt
       
       vtmax = 150.0

       
       IF ( vtxbar(mgs,il,2) .gt. vtxbar(mgs,il,1)  .or. &
     &      ( vtxbar(mgs,il,1) .gt. vtxbar(mgs,il,3) .and. vtxbar(mgs,il,3) > 0.0) ) THEN
          
          
          
          vtxbar(mgs,il,1) = Max( vtxbar(mgs,il,1), vtxbar(mgs,il,2) )
          vtxbar(mgs,il,3) = Max( vtxbar(mgs,il,3), vtxbar(mgs,il,1) )
          
       ENDIF

       
       IF ( vtxbar(mgs,il,1) .gt. vtmax .or. vtxbar(mgs,il,2) .gt. vtmax .or. &
     &      vtxbar(mgs,il,3) .gt. vtmax ) THEN
       
        vtxbar(mgs,il,1) = Min(vtmax,vtxbar(mgs,il,1) )
        vtxbar(mgs,il,2) = Min(vtmax,vtxbar(mgs,il,2) )
        vtxbar(mgs,il,3) = Min(vtmax,vtxbar(mgs,il,3) )
        

       ENDIF


       xvt(kgs(mgs),igs(mgs),1,il) = vtxbar(mgs,il,1)
       xvt(kgs(mgs),igs(mgs),2,il) = vtxbar(mgs,il,2)
       IF ( infdo .ge. 2 ) THEN
       xvt(kgs(mgs),igs(mgs),3,il) = vtxbar(mgs,il,3)
       ELSE
       xvt(kgs(mgs),igs(mgs),3,il) = 0.0
       ENDIF



      enddo
      ENDDO


      if (ndebugzf .gt. 0 ) write(0,*)  'ZIEGFALL: COPIED FALL SPEEDS'



 9998 continue

      if (ndebugzf .gt. 0 ) write(0,*)  'ZIEGFALL: DONE WITH LOOP'

      if ( kz .gt. nz-1 ) then
        go to 1200
      else
        nzmpb = kz 
      end if

      if (ndebugzf .gt. 0 ) write(0,*) 'ZIEGFALL: SET NZMPB'

      end do 

      if (ndebugzf .gt. 0 ) write(0,*) 'ZIEGFALL: SET NXMPB'

 1200 continue






      if (ndebugzf .gt. 0 ) write(0,*) "ZIEGFALL: EXITING SUBROUTINE"


      RETURN
      END subroutine ziegfall1d









      subroutine radardd02(nx,ny,nz,nor,na,an,temk,         &
     &    dbz,db,nzdbz,cnoh0t,hwdn1t,ipconc, iunit)











































      implicit none
      
      character(LEN=15), parameter :: microp = 'ZVD'
      integer nx,ny,nz,nor,na,ngt
      integer nzdbz    
      
      integer ng1,n10
      integer iunit
      integer, parameter :: printyn = 0

      parameter( ng1 = 1 )
      
      real cnoh0t,hwdn1t
      integer ipconc
      real vr


      integer imapz,mzdist
      
      integer vzflag
      integer, parameter :: norz = 3
      real an(-nor+ng1:nx+nor,-nor+ng1:ny+nor,-nor+ng1:nz+nor,na)
      real db(-nor+ng1:nx+nor,-nor+ng1:ny+nor,-nor+ng1:nz+nor)  

      real temk(-nor+ng1:nx+nor,-nor+ng1:ny+nor,-nor+ng1:nz+nor)  
      real dbz(-nor+ng1:nx+nor,-nor+ng1:ny+nor,-nor+ng1:nz+nor)   
      real gz(-nor+1:nz+nor) 
      

      real cr1, cr2 ,  hwdnsq,swdnsq
      real rwdnsq, dhmin, qrmin, qsmin, qhmin, qhlmin, tfr, tfrh, zrc
      real reflectmin,  kw_sq
      real const_ki_sn, const_ki_h, ki_sq_sn
      real ki_sq_h, dielf_sn, dielf_h
      real pi
      logical ltest


       real gtmp     (nx,nz)
       real dtmp     (nx,nz)
       real tmp

       real*8 dtmps, dtmpr, dtmph, dtmphl, g1, zx, ze, x

       integer i,j,k,ix,jy,kz,ihcnt

        real*8 xcnoh, xcnos, dadh, dads, zhdryc, zsdryc, zhwetc,zswetc
        real*8 dadr
        real dbzmax,dbzmin
        parameter ( dbzmin = 0 )

      real cnow,cnoi,cnoip,cnoir,cnor,cnos
      real cnogl,cnogm,cnogh,cnof,cnoh,cnohl

      real swdn, rwdn ,hwdn,gldn,gmdn,ghdn,fwdn,hldn
      real swdn0

      real rwdnmx,cwdnmx,cidnmx,xidnmx,swdnmx,gldnmx,gmdnmx
      real ghdnmx,fwdnmx,hwdnmx,hldnmx
      real rwdnmn,cwdnmn,cidnmn,xidnmn,swdnmn,gldnmn,gmdnmn
      real ghdnmn,fwdnmn,hwdnmn,hldnmn
 
      real gldnsq,gmdnsq,ghdnsq,fwdnsq,hldnsq

      real dadgl,dadgm,dadgh,dadhl,dadf
      real zgldryc,zglwetc,zgmdryc, zgmwetc,zghdryc,zghwetc
      real zhldryc,zhlwetc,zfdryc,zfwetc

      real dielf_gl,dielf_gm,dielf_gh,dielf_hl,dielf_fw
      
      integer imx,jmx,kmx
      
      real swdia,gldia,gmdia,ghdia,fwdia,hwdia,hldia
      
      real csw,cgl,cgm,cgh,cfw,chw,chl
      real xvs,xvgl,xvgm,xvgh,xvf,xvh,xvhl
      
      real cwc0
      integer izieg
      integer ice10
      real rhos
      parameter ( rhos = 0.1 )
      
      real qxw    

      real, parameter :: cwmasn = 5.23e-13   
      real, parameter :: cwmasx = 5.25e-10   
      real, parameter :: cwradn = 5.0e-6     

      real cwnccn(nz)
      
      real :: vzsnow, vzrain, vzgraupel, vzhail
      real :: dtp




      vzflag = 0
      
      izieg = 0
      ice10 = 0








      pi = 4.0*ATan(1.)
      cwc0 = piinv 
      
      cnoh = cnoh0t
      hwdn = hwdn1t

      rwdn = 1000.0
      swdn = 100.0

      qrmin = 1.0e-05
      qsmin = 1.0e-06
      qhmin = 1.0e-05




      cnow  = 1.0e+08
      cnoi  = 1.0e+08
      cnoip = 1.0e+08 
      cnoir = 1.0e+08 
      cnor  = 8.0e+06 
      cnos  = 8.0e+06 
      cnogl = 4.0e+05 
      cnogm = 4.0e+05 
      cnogh = 4.0e+05 
      cnof  = 4.0e+05
      cnohl = 1.0e+03


      imx = 1
      jmx = 1
      kmx = 1
      i = 1


       IF ( microp(1:4) .eq. 'ZIEG' ) THEN 


         izieg = 1

         hwdn = hwdn1t 


         cnor  = cno(lr)
         cnos  = cno(ls)
         cnoh  = cno(lh)
         qrmin = qxmin(lr)
         qsmin = qxmin(ls)
         qhmin = qxmin(lh)
         IF ( lhl .gt. 1 ) THEN
            cnohl  = cno(lhl)
            qhlmin = qxmin(lhl)
         ENDIF

       ELSEIF ( microp(1:3) .eq. 'ZVD' ) THEN 

         izieg = 1
         
         swdn0 = swdn

         cnor  = cno(lr)
         cnos  = cno(ls)
         cnoh  = cno(lh)
         
         qrmin = qxmin(lr)
         qsmin = qxmin(ls)
         qhmin = qxmin(lh)
         IF ( lhl .gt. 1 ) THEN
            cnohl  = cno(lhl)
            qhlmin = qxmin(lhl)
         ENDIF



        ENDIF















































































      rwdnmx = 1000.0
      cwdnmx = 1000.0
      cidnmx =  917.0
      xidnmx =  917.0
      swdnmx =  200.0
      gldnmx =  400.0
      gmdnmx =  600.0
      ghdnmx =  800.0
      fwdnmx =  900.0
      hwdnmx =  900.0
      hldnmx =  900.0

      rwdnmn = 1000.0
      cwdnmn = 1000.0
      xidnmn =  001.0
      cidnmn =  001.0
      swdnmn =  001.0
      gldnmn =  200.0
      gmdnmn =  400.0
      ghdnmn =  600.0
      fwdnmn =  700.0
      hwdnmn =  700.0
      hldnmn =  900.0

      
      gldn = (0.5)*(gldnmn+gldnmx)  
      gmdn = (0.5)*(gmdnmn+gmdnmx)  
      ghdn = (0.5)*(ghdnmn+ghdnmx)  
      fwdn = (0.5)*(fwdnmn+fwdnmx)  
      hldn = (0.5)*(hldnmn+hldnmx)  


      cr1  = 7.2e+20
      cr2  = 7.295e+19
      hwdnsq = hwdn**2
      swdnsq = swdn**2
      rwdnsq = rwdn**2

      gldnsq = gldn**2
      gmdnsq = gmdn**2
      ghdnsq = ghdn**2
      fwdnsq = fwdn**2
      hldnsq = hldn**2
      
      dhmin = 0.005
      tfr   = 273.16
      tfrh  = tfr - 8.0
      zrc   = cr1*cnor
      reflectmin = 0.0
      kw_sq = 0.93
      dbzmax = dbzmin
      
      ihcnt=0

            







      const_ki_sn = 0.5 - (0.5-0.46)/(917.-220.)*(swdn-220.)
      const_ki_h  = 0.5 - (0.5-0.46)/(917.-220.)*(hwdn-220.)
      ki_sq_sn = (swdnsq/rwdnsq) * const_ki_sn**2
      ki_sq_h  = (hwdnsq/rwdnsq) * const_ki_h**2
      dielf_sn = ki_sq_sn / kw_sq
      dielf_h  = ki_sq_h  / kw_sq
            





      dielf_sn = (swdnsq/rwdnsq)*.21/ kw_sq
      dielf_h  = (hwdnsq/rwdnsq)*.21/ kw_sq

      dielf_gl  = (gldnsq/rwdnsq)*.21/ kw_sq
      dielf_gm  = (gmdnsq/rwdnsq)*.21/ kw_sq
      dielf_gh  = (ghdnsq/rwdnsq)*.21/ kw_sq
      dielf_hl  = (hldnsq/rwdnsq)*.21/ kw_sq
      dielf_fw  = (fwdnsq/rwdnsq)*.21/ kw_sq

























      DO jy=1,1

        DO kz = 1,nz
         
          DO ix=1,nx
            dbz(ix,jy,kz) = 0.0
                      
          vzsnow = 0.0
          vzrain = 0.0
          vzgraupel = 0.0
          vzhail = 0.0
          
          dtmph = 0.0
          dtmps = 0.0
          dtmphl = 0.0
          dtmpr = 0.0
           dadr = (db(ix,jy,kz)/(pi*rwdn*cnor))**(0.25)



           
           dtmp(ix,kz) = 0.0
           gtmp(ix,kz) = 0.0
           IF ( an(ix,jy,kz,lr) .ge. qrmin ) THEN
             IF ( ipconc .le. 2 ) THEN
               gtmp(ix,kz) = dadr*an(ix,jy,kz,lr)**(0.25)
               dtmp(ix,kz) = zrc*gtmp(ix,kz)**7
             ELSEIF ( an(ix,jy,kz,lnr) .gt. 1.e-3 ) THEN
               IF ( imurain == 3 ) THEN
                 vr = db(ix,jy,kz)*an(ix,jy,kz,lr)/(1000.*an(ix,jy,kz,lnr))
                 dtmp(ix,kz) = 3.6e18*(rnu+2.)*an(ix,jy,kz,lnr)*vr**2/(rnu+1.)
               ELSE 
                g1 = (6.0 + alphar)*(5.0 + alphar)*(4.0 + alphar)/((3.0 + alphar)*(2.0 + alphar)*(1.0 + alphar))
                zx = g1*(db(ix,jy,kz)*an(ix,jy,kz,lr))**2/an(ix,jy,kz,lnr)
                ze =1.e18*zx*(6./(pi*1000.))**2 
                dtmp(ix,kz) = ze
               ENDIF
             ENDIF
             dtmpr = dtmp(ix,kz)
           ENDIF
           






          IF( lhab .gt. lr ) THEN


























        xcnoh    = cnoh
        xcnos    = cnos

















        IF ( ls .gt. 1 ) THEN 
        
        IF ( lvs .gt. 1 ) THEN
          IF ( an(ix,jy,kz,lvs) .gt. 0.0 ) THEN
            swdn = db(ix,jy,kz)*an(ix,jy,kz,ls)/an(ix,jy,kz,lvs)
            swdn = Min( 300., Max( 100., swdn ) )
          ELSE 
            swdn = swdn0
          ENDIF
        
        ENDIF 
        
        IF ( ipconc .ge. 5 ) THEN 

        xvs = db(ix,jy,kz)*an(ix,jy,kz,ls)/  &
     &      (swdn*Max(1.0e-3,an(ix,jy,kz,lns)))
        IF ( xvs .lt. xvsmn .or. xvs .gt. xvsmx ) THEN
          xvs = Min( xvsmx, Max( xvsmn,xvs ) )
          csw = db(ix,jy,kz)*an(ix,jy,kz,ls)/(xvs*swdn)
        ENDIF

         swdia = (xvs*cwc0)**(1./3.)
         xcnos = an(ix,jy,kz,ls)*db(ix,jy,kz)/(xvs*swdn*swdia)
         
         ENDIF 
         ENDIF  













        IF ( lh .gt. 1 ) THEN 

        IF ( lvh .gt. 1 ) THEN
          IF ( an(ix,jy,kz,lvh) .gt. 0.0 ) THEN
            hwdn = db(ix,jy,kz)*an(ix,jy,kz,lh)/an(ix,jy,kz,lvh)
            hwdn = Min( 900., Max( 170., hwdn ) )
          ELSE 
            hwdn = 500. 
          ENDIF
        ELSE
          hwdn = hwdn1t
        ENDIF 
        
        IF ( ipconc .ge. 5 ) THEN 

        xvh = db(ix,jy,kz)*an(ix,jy,kz,lh)/       &
     &      (hwdn*Max(1.0e-3,an(ix,jy,kz,lnh)))
        IF ( xvh .lt. xvhmn .or. xvh .gt. xvhmx ) THEN
          xvh = Min( xvhmx, Max( xvhmn,xvh ) )
          chw = db(ix,jy,kz)*an(ix,jy,kz,lh)/(xvh*hwdn)
        ENDIF

         hwdia = (xvh*cwc0)**(1./3.)
         xcnoh = an(ix,jy,kz,lh)*db(ix,jy,kz)/(xvh*hwdn*hwdia)
         
        ENDIF 
 
        ENDIF 

        dadh = 0.0
        dadhl = 0.0
        dads = 0.0
        IF ( xcnoh .gt. 0.0 ) THEN 
          dadh = ( db(ix,jy,kz) /(pi*hwdn*xcnoh) )**(.25)
          zhdryc = 0.224*cr2*(db(ix,jy,kz)/rwdn)**2/xcnoh 
                                        
                                        
                                        
        ELSE
          dadh = 0.0
          zhdryc = 0.0
        ENDIF
        
        IF ( xcnos .gt. 0.0 ) THEN
          dads = ( db(ix,jy,kz) /(pi*swdn*xcnos) )**(.25)
          zsdryc = 0.224*cr2*(db(ix,jy,kz)/rwdn)**2/xcnos 
        ELSE
          dads = 0.0
          zsdryc = 0.0
        ENDIF
        zhwetc = zhdryc 
        zswetc = zsdryc 



          IF ( ls .gt. 1 ) THEN
          
          gtmp(ix,kz) = 0.0 
          qxw = 0.0 
          dtmps = 0.0
           IF ( an(ix,jy,kz,ls) .ge. qsmin ) THEN 
            IF ( ipconc .ge. 4 ) THEN  

             if (lsw .gt. 1) qxw = an(ix,jy,kz,lsw)

             vr = xvs 

             
             IF ( an(ix,jy,kz,lns) .gt. 1.e-5 ) THEN
             gtmp(ix,kz) = 3.6e18*(snu+2.)*( 0.224*an(ix,jy,kz,ls) + 0.776*qxw)*an(ix,jy,kz,ls)/ &
     &           (an(ix,jy,kz,lns)*(snu+1.)*rwdn**2)
             ENDIF
             
             tmp = Min(1.0,1.e3*(an(ix,jy,kz,ls))*db(ix,jy,kz))
             gtmp(ix,kz) = Max( 1.0*gtmp(ix,kz), 750.0*(tmp)**1.98)
             dtmps = gtmp(ix,kz)
             dtmp(ix,kz) = dtmp(ix,kz) + gtmp(ix,kz)
            ELSE
             gtmp(ix,kz) = dads*an(ix,jy,kz,ls)**(0.25)
             
             IF ( gtmp(ix,kz) .gt. 0.0 ) THEN 
             dtmps = zsdryc*an(ix,jy,kz,ls)**2/gtmp(ix,kz)
             IF ( temk(ix,jy,kz) .lt. tfr ) THEN
               dtmp(ix,kz) = dtmp(ix,kz) +          &
     &                   zsdryc*an(ix,jy,kz,ls)**2/gtmp(ix,kz)
             ELSE
               dtmp(ix,kz) = dtmp(ix,kz) +          &
     &                  zswetc*an(ix,jy,kz,ls)**2/gtmp(ix,kz)
             ENDIF
             ENDIF 
            ENDIF 
           
           ENDIF 
           
           ENDIF





         IF ( li .gt. 1 .and. idbzci .ne. 0 ) THEN
          
          gtmp(ix,kz) = 0.0 
           IF ( an(ix,jy,kz,li) .ge. 0.1e-3 ) THEN
             gtmp(ix,kz) = Min(1.0,1.e3*(an(ix,jy,kz,li))*db(ix,jy,kz))
             dtmp(ix,kz) = dtmp(ix,kz) + 750.0*(gtmp(ix,kz))**1.98
           ENDIF
           
          ENDIF
          



         IF ( lh .gt. 1 ) THEN 
           gtmp(ix,kz) = 0.0 
           dtmph = 0.0
           qxw = 0.0

          IF ( izieg .ge. 1 .and. ipconc .ge. 5 ) THEN

           ltest = .false.
           
           IF ( ltest .or. (an(ix,jy,kz,lh) .ge. qhmin .and. an(ix,jy,kz,lnh) .gt. 1.e-6 )) THEN
            
            IF ( lvh .gt. 1 ) THEN
             
             IF ( an(ix,jy,kz,lvh) .gt. 0.0 ) THEN
               hwdn = db(ix,jy,kz)*an(ix,jy,kz,lh)/an(ix,jy,kz,lvh)
               hwdn = Min( 900., Max( 100., hwdn ) )
              ELSE 
               hwdn = 500. 
              ENDIF

             ENDIF

             chw = an(ix,jy,kz,lnh)
            IF ( chw .gt. 0.0 ) THEN                                         
             xvh = db(ix,jy,kz)*an(ix,jy,kz,lh)/(hwdn*Max(1.0e-3,chw))
             IF ( xvh .lt. xvhmn .or. xvh .gt. xvhmx ) THEN
              xvh = Min( xvhmx, Max( xvhmn,xvh ) )
              chw = db(ix,jy,kz)*an(ix,jy,kz,lh)/(xvh*hwdn)
             ENDIF
             
             IF ( lhw .gt. 1 ) THEN
               IF ( iusewetgraupel .eq. 1 ) THEN
                  qxw = an(ix,jy,kz,lhw)
               ELSEIF ( iusewetgraupel .eq. 2 ) THEN
                  IF ( hwdn .lt. 300. ) THEN
                    qxw = an(ix,jy,kz,lhw)
                  ENDIF
               ENDIF
             ENDIF
             
             IF ( lzh .gt. 1 ) THEN
             ELSE
             g1 = (6.0 + alphah)*(5.0 + alphah)*(4.0 + alphah)/((3.0 + alphah)*(2.0 + alphah)*(1.0 + alphah))


             zx = g1*db(ix,jy,kz)**2*( 0.224*an(ix,jy,kz,lh) + 0.776*qxw)*an(ix,jy,kz,lh)/chw
             ze =1.e18*zx*(6./(pi*1000.))**2
             dtmp(ix,kz) = dtmp(ix,kz) + ze
             dtmph = ze
             ENDIF
             
            ENDIF
             
        
           ENDIF
          
          ELSE
          
          dtmph = 0.0
          
           IF ( an(ix,jy,kz,lh) .ge. qhmin ) THEN
             gtmp(ix,kz) = dadh*an(ix,jy,kz,lh)**(0.25)
             IF ( gtmp(ix,kz) .gt. 0.0 ) THEN
             dtmph =  zhdryc*an(ix,jy,kz,lh)**2/gtmp(ix,kz)
             IF ( temk(ix,jy,kz) .lt. tfr ) THEN
               dtmp(ix,kz) = dtmp(ix,kz) +                   &
     &                  zhdryc*an(ix,jy,kz,lh)**2/gtmp(ix,kz)
             ELSE

                 dtmp(ix,kz) = dtmp(ix,kz) +                   &
     &                  zhdryc*an(ix,jy,kz,lh)**2/gtmp(ix,kz)





             ENDIF
             ENDIF
           ENDIF
          
         
          
          ENDIF
 

          ENDIF 
          
          ENDIF 

        
        IF ( izieg .ge. 1 .and. lhl .gt. 1 ) THEN

        hldn = 900.0
        gtmp(ix,kz) = 0.0
        dtmphl = 0.0
        qxw = 0.0
        

        IF ( lvhl .gt. 1 ) THEN
          IF ( an(ix,jy,kz,lvhl) .gt. 0.0 ) THEN
            hldn = db(ix,jy,kz)*an(ix,jy,kz,lhl)/an(ix,jy,kz,lvhl)
            hldn = Min( 900., Max( 300., hldn ) )
          ELSE 
            hldn = 900. 
          ENDIF
        ELSE
          hldn = rho_qhl
        ENDIF 


        IF ( ipconc .ge. 5 ) THEN

           ltest = .false.

          IF ( ltest .or. ( an(ix,jy,kz,lhl) .ge. qhlmin .and. an(ix,jy,kz,lnhl) .gt. 0.) ) THEN 
            chl = an(ix,jy,kz,lnhl)
            IF ( chl .gt. 0.0 ) THEN 
             xvhl = db(ix,jy,kz)*an(ix,jy,kz,lhl)/         &
     &        (hldn*Max(1.0e-9,an(ix,jy,kz,lnhl)))
            IF ( xvhl .lt. xvhlmn .or. xvhl .gt. xvhlmx ) THEN 
              xvhl = Min( xvhlmx, Max( xvhlmn,xvhl ) )
              chl = db(ix,jy,kz)*an(ix,jy,kz,lhl)/(xvhl*hldn)
              an(ix,jy,kz,lnhl) = chl
            ENDIF 

             IF ( lhlw .gt. 1 ) THEN
               IF ( iusewethail .eq. 1 ) THEN
                  qxw = an(ix,jy,kz,lhlw)
               ELSEIF ( iusewethail .eq. 2 ) THEN
                  IF ( hldn .lt. 300. ) THEN
                    qxw = an(ix,jy,kz,lhlw)
                  ENDIF
               ENDIF
             ENDIF
            
             IF ( lzhl .gt. 1 ) THEN 
             ELSE 

             g1 = (6.0 + alphahl)*(5.0 + alphahl)*(4.0 + alphahl)/((3.0 + alphahl)*(2.0 + alphahl)*(1.0 + alphahl))
             zx = g1*db(ix,jy,kz)**2*( 0.224*an(ix,jy,kz,lhl) + 0.776*qxw)*an(ix,jy,kz,lhl)/chl

             ze = 0.224*1.e18*zx*(6./(pi*1000.))**2
             dtmp(ix,kz) = dtmp(ix,kz) + ze
             dtmphl = ze
             
             ENDIF 
            ENDIF
        
           ENDIF

          
          ELSE
          
          
           IF ( an(ix,jy,kz,lhl) .ge. qhlmin ) THEN 
            dadhl = ( db(ix,jy,kz) /(pi*hldn*cnohl) )**(.25)
             gtmp(ix,kz) = dadhl*an(ix,jy,kz,lhl)**(0.25)
             IF ( gtmp(ix,kz) .gt. 0.0 ) THEN 

              zhldryc = 0.224*cr2*( db(ix,jy,kz)/rwdn)**2/cnohl 

             dtmphl =  zhldryc*an(ix,jy,kz,lhl)**2/gtmp(ix,kz)

             IF ( temk(ix,jy,kz) .lt. tfr ) THEN
               dtmp(ix,kz) = dtmp(ix,kz) +                   &
     &                  zhldryc*an(ix,jy,kz,lhl)**2/gtmp(ix,kz)
             ELSE

                 dtmp(ix,kz) = dtmp(ix,kz) +                   &
     &                  zhldryc*an(ix,jy,kz,lhl)**2/gtmp(ix,kz)





             ENDIF
             ENDIF 
           
           ENDIF 
          
         ENDIF 


        ENDIF 

          
           
          IF ( dtmp(ix,kz) .gt. 0.0 ) THEN
            dbz(ix,jy,kz) = Max(dbzmin, 10.0*Log10(dtmp(ix,kz)) )
            
            IF ( dbz(ix,jy,kz) .gt. dbzmax ) THEN
              dbzmax = Max(dbzmax,dbz(ix,jy,kz))
              imx = ix
              jmx = jy
              kmx = kz
            ENDIF
          ELSE 
             dbz(ix,jy,kz) = dbzmin
             IF ( lh > 1 .and. lhl > 1) THEN
               IF ( an(ix,jy,kz,lh) > 1.0e-3 ) THEN
                 write(0,*) 'radardbz: qr,qh,qhl = ',an(ix,jy,kz,lr), an(ix,jy,kz,lh),an(ix,jy,kz,lhl)
                 write(0,*) 'radardbz: dtmps,dtmph,dadh,dadhl,dtmphl = ',dtmps,dtmph,dadh,dadhl,dtmphl
                 
                 IF ( lzh>1 .and. lzhl>1 ) write(0,*) 'radardbz: zh, zhl = ',an(ix,jy,kz,lzh),an(ix,jy,kz,lzhl)
               ENDIF
             ENDIF
          ENDIF









        IF ( .not. dtmp(ix,kz) .lt. 1.e30 .or. dbz(ix,jy,kz) > 190.0 ) THEN


          write(0,*) 'ix,jy,kz = ',ix,jy,kz
          write(0,*) 'dbz = ',dbz(ix,jy,kz)
          write(0,*) 'db, zhdryc = ',db(ix,jy,kz),zhdryc
          write(0,*) 'Hail intercept: ',xcnoh,ix,kz
          write(0,*) 'Hail,snow q: ',an(ix,jy,kz,lh),an(ix,jy,kz,ls)
          write(0,*) 'graupel density hwdn = ',hwdn
          write(0,*) 'rain q: ',an(ix,jy,kz,lr)
          write(0,*) 'ice q: ',an(ix,jy,kz,li)
          IF ( lhl .gt. 1 ) write(0,*) 'Hail (lhl): ',an(ix,jy,kz,lhl)
          IF (ipconc .ge. 3 ) write(0,*) 'rain c: ',an(ix,jy,kz,lnr)
          IF ( lzr > 1 ) write(0,*) 'rain Z: ',an(ix,jy,kz,lzr)
          IF ( ipconc .ge. 5 ) THEN
          write(0,*) 'Hail,snow c: ',an(ix,jy,kz,lnh),an(ix,jy,kz,lns)
          IF ( lhl .gt. 1 ) write(0,*) 'Hail (lnhl): ',an(ix,jy,kz,lnhl)
          IF ( lzhl .gt. 1 ) THEN 
            write(0,*) 'Hail (lzhl): ',an(ix,jy,kz,lzhl)
            write(0,*) 'chl,xvhl,dhl = ',chl,xvhl,(xvhl*6./3.14159)**(1./3.)
            write(0,*) 'xvhlmn,xvhlmx = ',xvhlmn,xvhlmx
          ENDIF
          ENDIF
          write(0,*) 'chw,xvh = ', chw,xvh
          write(0,*) 'dtmps,dtmph,dadh,dadhl,dtmphl = ',dtmps,dtmph,dadh,dadhl,dtmphl
          write(0,*) 'dtmpr = ',dtmpr
          write(0,*) 'gtmp = ',gtmp(ix,kz),dtmp(ix,kz)
          IF ( .not. (dbz(ix,jy,kz) .gt. -100 .and. dbz(ix,jy,kz) .lt. 200 ) ) THEN
            write(0,*) 'dbz out of bounds! STOP!'

          ENDIF
         ENDIF

           
          ENDDO 
         ENDDO 
      ENDDO 
            
      
      
      

      IF ( printyn .eq. 1 ) THEN

        write(iunit,*) 'maxdbz,ijk = ',dbzmax,imx,jmx,kmx
        write(iunit,*) 'qrw = ',an(imx,jmx,kmx,lr)
        
        IF ( lh .gt. 1 ) THEN
          write(iunit,*) 'qi  = ',an(imx,jmx,kmx,li)
          write(iunit,*) 'qsw = ',an(imx,jmx,kmx,ls)
          write(iunit,*) 'qhw = ',an(imx,jmx,kmx,lh)
          IF ( lhl .gt. 1 ) write(iunit,*) 'qhl = ',an(imx,jmx,kmx,lhl)
        ENDIF

      
      ENDIF
      
      
      RETURN
      END subroutine radardd02
      





END MODULE module_mp_nssl_2mom
