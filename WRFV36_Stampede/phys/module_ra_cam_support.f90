MODULE module_ra_cam_support
  use module_cam_support, only: endrun
  implicit none
      integer, parameter :: r8 = 8
      real(r8), parameter:: inf = 1.e20 
      integer, parameter:: bigint = O'17777777777'           

      integer :: ixcldliq 
      integer :: ixcldice

      integer, parameter :: nbands = 2          
      integer, parameter :: naer_all = 12 + 1
      integer, parameter :: naer = 10 + 1
      integer, parameter :: bnd_nbr_LW=7 
      integer, parameter :: ndstsz = 4    
      integer :: idxSUL
      integer :: idxSSLT
      integer :: idxDUSTfirst
      integer :: idxCARBONfirst
      integer :: idxOCPHO
      integer :: idxBCPHO
      integer :: idxOCPHI
      integer :: idxBCPHI
      integer :: idxBG  
      integer :: idxVOLC

  integer :: mxaerl                            




  integer, parameter :: &
      numDUST         = 4, &
      numCARBON      = 4




  real(r8) :: sulscl_rf  = 0._r8 
  real(r8) :: carscl_rf  = 0._r8
  real(r8) :: ssltscl_rf = 0._r8
  real(r8) :: dustscl_rf = 0._r8
  real(r8) :: bgscl_rf   = 0._r8
  real(r8) :: volcscl_rf = 0._r8


  real(r8) :: tauback = 0._r8



  real(r8) :: sulscl  = 1._r8
  real(r8) :: carscl  = 1._r8
  real(r8) :: ssltscl = 1._r8
  real(r8) :: dustscl = 1._r8
  real(r8) :: volcscl = 1._r8


     integer, parameter :: idx_LW_0500_0650=3
     integer, parameter :: idx_LW_0650_0800=4
     integer, parameter :: idx_LW_0800_1000=5
     integer, parameter :: idx_LW_1000_1200=6
     integer, parameter :: idx_LW_1200_2000=7









      real(r8) :: abs_cff_mss_aer(bnd_nbr_LW) = &
         (/ 70.257384, 285.282943, &
         1.0273851e+02, 6.3073303e+01, 1.2039569e+02, &
         3.6343643e+02, 2.7138528e+02 /)


      real(r8), parameter:: min_tp_h2o = 160.0        
      real(r8), parameter:: max_tp_h2o = 349.999999   
      real(r8), parameter:: dtp_h2o = 21.111111111111 
      real(r8), parameter:: min_te_h2o = -120.0       
      real(r8), parameter:: max_te_h2o = 79.999999    
      real(r8), parameter:: dte_h2o  = 10.0           
      real(r8), parameter:: min_rh_h2o = 0.0          
      real(r8), parameter:: max_rh_h2o = 1.19999999   
      real(r8), parameter:: drh_h2o = 0.2             
      real(r8), parameter:: min_lu_h2o = -8.0         
      real(r8), parameter:: min_u_h2o  = 1.0e-8       
      real(r8), parameter:: max_lu_h2o =  3.9999999   
      real(r8), parameter:: dlu_h2o  = 0.5            
      real(r8), parameter:: min_lp_h2o = -3.0         
      real(r8), parameter:: min_p_h2o = 1.0e-3        
      real(r8), parameter:: max_lp_h2o = -0.0000001   
      real(r8), parameter:: dlp_h2o = 0.3333333333333 
      integer, parameter :: n_u = 25   
      integer, parameter :: n_p = 10   
      integer, parameter :: n_tp = 10  
      integer, parameter :: n_te = 21  
      integer, parameter :: n_rh = 7   
      real(r8):: c16,c17,c26,c27,c28,c29,c30,c31
      real(r8):: fwcoef      
      real(r8):: fwc1,fwc2   
      real(r8):: fc1         
      real(r8):: amco2 
      real(r8):: amd   
      real(r8):: p0    


  real(r8), allocatable, dimension(:,:,:,:,:)  :: ah2onw   
  real(r8), allocatable, dimension(:,:,:,:,:)  :: eh2onw   
  real(r8), allocatable, dimension(:,:,:,:,:)  :: ah2ow    
  real(r8), allocatable, dimension(:,:,:,:,:)  :: cn_ah2ow 
  real(r8), allocatable, dimension(:,:,:,:,:)  :: cn_eh2ow 
  real(r8), allocatable, dimension(:,:,:,:,:)  :: ln_ah2ow 
  real(r8), allocatable, dimension(:,:,:,:,:)  :: ln_eh2ow 







  real(r8):: coefh(2,4) = reshape(  &
         (/ (/5.46557e+01,-7.30387e-02/), &
            (/1.09311e+02,-1.46077e-01/), &
            (/5.11479e+01,-6.82615e-02/), &
            (/1.02296e+02,-1.36523e-01/) /), (/2,4/) )

  real(r8):: coefj(3,2) = reshape( &
            (/ (/2.82096e-02,2.47836e-04,1.16904e-06/), &
               (/9.27379e-02,8.04454e-04,6.88844e-06/) /), (/3,2/) )

  real(r8):: coefk(3,2) = reshape( &
            (/ (/2.48852e-01,2.09667e-03,2.60377e-06/) , &
               (/1.03594e+00,6.58620e-03,4.04456e-06/) /), (/3,2/) )

  integer, parameter :: ntemp = 192 
  real(r8) :: estblh2o(0:ntemp)       
  integer, parameter :: o_fa = 6   
  integer, parameter :: o_fe = 6   












  real(r8), parameter:: fat(o_fa,nbands) = reshape( (/ &
       (/-1.06665373E-01,  2.90617375E-02, -2.70642049E-04,   &   
          1.07595511E-06, -1.97419681E-09,  1.37763374E-12/), &   
       (/ 1.10666537E+00, -2.90617375E-02,  2.70642049E-04,   &   
         -1.07595511E-06,  1.97419681E-09, -1.37763374E-12/) /) & 
       , (/o_fa,nbands/) )



  real(r8), parameter:: fet(o_fe,nbands) = reshape( (/ &
      (/3.46148163E-01,  1.51240299E-02, -1.21846479E-04,   &   
        4.04970123E-07, -6.15368936E-10,  3.52415071E-13/), &   
      (/6.53851837E-01, -1.51240299E-02,  1.21846479E-04,   &   
       -4.04970123E-07,  6.15368936E-10, -3.52415071E-13/) /) & 
      , (/o_fa,nbands/) )


      real(r8) ::  gravit     
      real(r8) ::  rga        
      real(r8) ::  gravmks    
      real(r8) ::  cpair      
      real(r8) ::  epsilo     
      real(r8) ::  epsqs      
      real(r8) ::  sslp       
      real(r8) ::  stebol     
      real(r8) ::  rgsslp     
      real(r8) ::  dpfo3      
      real(r8) ::  dpfco2     
      real(r8) ::  dayspy     
      real(r8) ::  pie        
      real(r8) ::  mwdry      
      real(r8) ::  scon       
      real(r8) ::  co2mmr
real(r8) ::   mwco2              
real(r8) ::   mwh2o              
real(r8) ::   mwch4              
real(r8) ::   mwn2o              
real(r8) ::   mwf11              
real(r8) ::   mwf12              
real(r8) ::   cappa              
real(r8) ::   rair               
real(r8) ::   tmelt              
real(r8) ::   r_universal        
real(r8) ::   latvap             
real(r8) ::   latice             
real(r8) ::   zvir               
  integer plenest  
  parameter (plenest=250)





real(r8) estbl(plenest)      
real(r8) tmin       
real(r8) tmax       
real(r8) pcf(6)     







integer, parameter :: idxVIS = 8     
integer, parameter :: nrh = 1000   
integer, parameter :: nspint = 19   


real(r8), allocatable, dimension(:,:) :: ksul    
real(r8), allocatable, dimension(:,:) :: wsul    
real(r8), allocatable, dimension(:,:) :: gsul    
real(r8), allocatable, dimension(:,:) :: ksslt   
real(r8), allocatable, dimension(:,:) :: wsslt   
real(r8), allocatable, dimension(:,:) :: gsslt   
real(r8), allocatable, dimension(:,:) :: kcphil  
real(r8), allocatable, dimension(:,:) :: wcphil  
real(r8), allocatable, dimension(:,:) :: gcphil  

real(r8) :: kbg(nspint)          
real(r8) :: wbg(nspint)          
real(r8) :: gbg(nspint)          
real(r8) :: kcphob(nspint)       
real(r8) :: wcphob(nspint)       
real(r8) :: gcphob(nspint)       
real(r8) :: kcb(nspint)          
real(r8) :: wcb(nspint)          
real(r8) :: gcb(nspint)          
real(r8) :: kvolc(nspint)        
real(r8) :: wvolc(nspint)        
real(r8) :: gvolc(nspint)        

real(r8) :: kdst(ndstsz, nspint) 
real(r8) :: wdst(ndstsz, nspint) 
real(r8) :: gdst(ndstsz, nspint) 


      real(r8) cplos    
      real(r8) cplol    





   real(r8) :: co2vmr = 3.550e-4         
   real(r8) :: n2ovmr = 0.311e-6         
   real(r8) :: ch4vmr = 1.714e-6         
   real(r8) :: f11vmr = 0.280e-9         
   real(r8) :: f12vmr = 0.503e-9         

integer, parameter :: cyr = 233  

   integer  :: yrdata(cyr) = &
 (/ 1869, 1870, 1871, 1872, 1873, 1874, 1875, &
    1876, 1877, 1878, 1879, 1880, 1881, 1882, &
    1883, 1884, 1885, 1886, 1887, 1888, 1889, &
    1890, 1891, 1892, 1893, 1894, 1895, 1896, &
    1897, 1898, 1899, 1900, 1901, 1902, 1903, &
    1904, 1905, 1906, 1907, 1908, 1909, 1910, &
    1911, 1912, 1913, 1914, 1915, 1916, 1917, &
    1918, 1919, 1920, 1921, 1922, 1923, 1924, &
    1925, 1926, 1927, 1928, 1929, 1930, 1931, &
    1932, 1933, 1934, 1935, 1936, 1937, 1938, &
    1939, 1940, 1941, 1942, 1943, 1944, 1945, &
    1946, 1947, 1948, 1949, 1950, 1951, 1952, &
    1953, 1954, 1955, 1956, 1957, 1958, 1959, &
    1960, 1961, 1962, 1963, 1964, 1965, 1966, &
    1967, 1968, 1969, 1970, 1971, 1972, 1973, &
    1974, 1975, 1976, 1977, 1978, 1979, 1980, &
    1981, 1982, 1983, 1984, 1985, 1986, 1987, &
    1988, 1989, 1990, 1991, 1992, 1993, 1994, &
    1995, 1996, 1997, 1998, 1999, 2000, 2001, &
    2002, 2003, 2004, 2005, 2006, 2007, 2008, &
    2009, 2010, 2011, 2012, 2013, 2014, 2015, &
    2016, 2017, 2018, 2019, 2020, 2021, 2022, &
    2023, 2024, 2025, 2026, 2027, 2028, 2029, &
    2030, 2031, 2032, 2033, 2034, 2035, 2036, &
    2037, 2038, 2039, 2040, 2041, 2042, 2043, &
    2044, 2045, 2046, 2047, 2048, 2049, 2050, &
    2051, 2052, 2053, 2054, 2055, 2056, 2057, &
    2058, 2059, 2060, 2061, 2062, 2063, 2064, &
    2065, 2066, 2067, 2068, 2069, 2070, 2071, &
    2072, 2073, 2074, 2075, 2076, 2077, 2078, &
    2079, 2080, 2081, 2082, 2083, 2084, 2085, &
    2086, 2087, 2088, 2089, 2090, 2091, 2092, &
    2093, 2094, 2095, 2096, 2097, 2098, 2099, &
    2100, 2101                               /)


    real(r8)  :: co2(cyr) = &
 (/ 289.263, 289.263, 289.416, 289.577, 289.745, 289.919, 290.102, &
    290.293, 290.491, 290.696, 290.909, 291.129, 291.355, 291.587, 291.824, &
    292.066, 292.313, 292.563, 292.815, 293.071, 293.328, 293.586, 293.843, &
    294.098, 294.35, 294.598, 294.842, 295.082, 295.32, 295.558, 295.797,   &
    296.038, 296.284, 296.535, 296.794, 297.062, 297.338, 297.62, 297.91,   &
    298.204, 298.504, 298.806, 299.111, 299.419, 299.729, 300.04, 300.352,  &
    300.666, 300.98, 301.294, 301.608, 301.923, 302.237, 302.551, 302.863,  &
    303.172, 303.478, 303.779, 304.075, 304.366, 304.651, 304.93, 305.206,  &
    305.478, 305.746, 306.013, 306.28, 306.546, 306.815, 307.087, 307.365,  &
    307.65, 307.943, 308.246, 308.56, 308.887, 309.228, 309.584, 309.956,   &
    310.344, 310.749, 311.172, 311.614, 312.077, 312.561, 313.068, 313.599, &
    314.154, 314.737, 315.347, 315.984, 316.646, 317.328, 318.026, 318.742, &
    319.489, 320.282, 321.133, 322.045, 323.021, 324.06, 325.155, 326.299,  &
    327.484, 328.698, 329.933, 331.194, 332.499, 333.854, 335.254, 336.69,  &
    338.15, 339.628, 341.125, 342.65, 344.206, 345.797, 347.397, 348.98,    &
    350.551, 352.1, 354.3637, 355.7772, 357.1601, 358.5306, 359.9046,       &
    361.4157, 363.0445, 364.7761, 366.6064, 368.5322, 370.534, 372.5798,    &
    374.6564, 376.7656, 378.9087, 381.0864, 383.2994, 385.548, 387.8326,    &
    390.1536, 392.523, 394.9625, 397.4806, 400.075, 402.7444, 405.4875,     &
    408.3035, 411.1918, 414.1518, 417.1831, 420.2806, 423.4355, 426.6442,   &
    429.9076, 433.2261, 436.6002, 440.0303, 443.5168, 447.06, 450.6603,     &
    454.3059, 457.9756, 461.6612, 465.3649, 469.0886, 472.8335, 476.6008,   &
    480.3916, 484.2069, 488.0473, 491.9184, 495.8295, 499.7849, 503.7843,   &
    507.8278, 511.9155, 516.0476, 520.2243, 524.4459, 528.7127, 533.0213,   &
    537.3655, 541.7429, 546.1544, 550.6005, 555.0819, 559.5991, 564.1525,   &
    568.7429, 573.3701, 578.0399, 582.7611, 587.5379, 592.3701, 597.2572,   &
    602.1997, 607.1975, 612.2507, 617.3596, 622.524, 627.7528, 633.0616,    &
    638.457, 643.9384, 649.505, 655.1568, 660.8936, 666.7153, 672.6219,     &
    678.6133, 684.6945, 690.8745, 697.1569, 703.5416, 710.0284, 716.6172,   &
    723.308, 730.1008, 736.9958, 743.993, 751.0975, 758.3183, 765.6594,     &
    773.1207, 780.702, 788.4033, 796.2249, 804.1667, 812.2289, 820.4118,    &
    828.6444, 828.6444 /)

      integer  :: ntoplw      

      logical :: masterproc = .true.
      logical :: ozncyc            


      logical :: indirect          

      logical :: radforce   = .false.          
      logical :: trace_gas=.false.             
      logical :: strat_volcanic   = .false.    

    real(r8) retab(95)
    
    
    
    
    data retab / 						&
         5.92779, 6.26422, 6.61973, 6.99539, 7.39234,	&
         7.81177, 8.25496, 8.72323, 9.21800, 9.74075, 10.2930,	&
         10.8765, 11.4929, 12.1440, 12.8317, 13.5581, 14.2319, 	&
         15.0351, 15.8799, 16.7674, 17.6986, 18.6744, 19.6955,	&
         20.7623, 21.8757, 23.0364, 24.2452, 25.5034, 26.8125,	&
         27.7895, 28.6450, 29.4167, 30.1088, 30.7306, 31.2943, 	&
         31.8151, 32.3077, 32.7870, 33.2657, 33.7540, 34.2601, 	&
         34.7892, 35.3442, 35.9255, 36.5316, 37.1602, 37.8078,	&
         38.4720, 39.1508, 39.8442, 40.5552, 41.2912, 42.0635,	&
         42.8876, 43.7863, 44.7853, 45.9170, 47.2165, 48.7221,	&
         50.4710, 52.4980, 54.8315, 57.4898, 60.4785, 63.7898,	&
         65.5604, 71.2885, 75.4113, 79.7368, 84.2351, 88.8833,	&
         93.6658, 98.5739, 103.603, 108.752, 114.025, 119.424, 	&
         124.954, 130.630, 136.457, 142.446, 148.608, 154.956,	&
         161.503, 168.262, 175.248, 182.473, 189.952, 197.699,	&
         205.728, 214.055, 222.694, 231.661, 240.971, 250.639/	
    
    save retab
contains



subroutine sortarray(n, ain, indxa) 










   implicit none



   integer , intent(in) :: n             
   integer , intent(inout) :: indxa(n)   
   real(r8), intent(inout) :: ain(n)     



   integer :: i, j                
   integer :: ni                  
   integer :: itmp                
   real(r8):: atmp                
 
   ni = 1 
   do while(.TRUE.) 
      ni = 3*ni + 1 
      if (ni <= n) cycle  
      exit  
   end do 
 
   do while(.TRUE.) 
      ni = ni/3 
      do i = ni + 1, n 
         atmp = ain(i) 
         itmp = indxa(i) 
         j = i 
         do while(.TRUE.) 
            if (ain(j-ni) <= atmp) exit  
            ain(j) = ain(j-ni) 
            indxa(j) = indxa(j-ni) 
            j = j - ni 
            if (j > ni) cycle  
            exit  
         end do 
         ain(j) = atmp 
         indxa(j) = itmp 
      end do 
      if (ni > 1) cycle  
      exit  
   end do 
   return  
 
end subroutine sortarray
subroutine trcab(lchnk   ,ncol    ,pcols, pverp,               &
                 k1      ,k2      ,ucfc11  ,ucfc12  ,un2o0   , &
                 un2o1   ,uch4    ,uco211  ,uco212  ,uco213  , &
                 uco221  ,uco222  ,uco223  ,bn2o0   ,bn2o1   , &
                 bch4    ,to3co2  ,pnm     ,dw      ,pnew    , &
                 s2c     ,uptype  ,dplh2o  ,abplnk1 ,tco2    , &
                 th2o    ,to3     ,abstrc  , &
                 aer_trn_ttl)
















   implicit none





   integer, intent(in) :: lchnk                    
   integer, intent(in) :: ncol                     
   integer, intent(in) :: pcols, pverp
   integer, intent(in) :: k1,k2                    

   real(r8), intent(in) :: to3co2(pcols)           
   real(r8), intent(in) :: pnm(pcols,pverp)        
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
   real(r8), intent(in) :: dw(pcols)               
   real(r8), intent(in) :: pnew(pcols)             
   real(r8), intent(in) :: s2c(pcols,pverp)        
   real(r8), intent(in) :: uptype(pcols,pverp)     

   real(r8), intent(in) :: dplh2o(pcols)           
   real(r8), intent(in) :: abplnk1(14,pcols,pverp) 
   real(r8), intent(in) :: tco2(pcols)             
   real(r8), intent(in) :: th2o(pcols)             
   real(r8), intent(in) :: to3(pcols)              

   real(r8), intent(in) :: aer_trn_ttl(pcols,pverp,pverp,bnd_nbr_LW) 




   real(r8), intent(out) :: abstrc(pcols)           



   integer  i,l                     

   real(r8) sqti(pcols)             
   real(r8) du1                     
   real(r8) du2                     
   real(r8) acfc1                   
   real(r8) acfc2                   

   real(r8) acfc3                   
   real(r8) acfc4                   
   real(r8) acfc5                   
   real(r8) acfc6                   
   real(r8) acfc7                   

   real(r8) acfc8                   
   real(r8) du01                    
   real(r8) dbeta01                 
   real(r8) dbeta11                 
   real(r8) an2o1                   

   real(r8) du02                    
   real(r8) dbeta02                 
   real(r8) an2o2                   
   real(r8) du03                    
   real(r8) dbeta03                 

   real(r8) an2o3                   
   real(r8) duch4                   
   real(r8) dbetac                  
   real(r8) ach4                    
   real(r8) du11                    

   real(r8) du12                    
   real(r8) du13                    
   real(r8) dbetc1                  
   real(r8) dbetc2                  
   real(r8) aco21                   

   real(r8) du21                    
   real(r8) du22                    
   real(r8) du23                    
   real(r8) aco22                   
   real(r8) tt(pcols)               

   real(r8) psi1                    
   real(r8) phi1                    
   real(r8) p1                      
   real(r8) w1                      
   real(r8) ds2c(pcols)             

   real(r8) duptyp(pcols)           
   real(r8) tw(pcols,6)             
   real(r8) g1(6)                   
   real(r8) g2(6)                   
   real(r8) g3(6)                   

   real(r8) g4(6)                   
   real(r8) ab(6)                   
   real(r8) bb(6)                   
   real(r8) abp(6)                  
   real(r8) bbp(6)                  

   real(r8) tcfc3                   
   real(r8) tcfc4                   
   real(r8) tcfc6                   
   real(r8) tcfc7                   
   real(r8) tcfc8                   

   real(r8) tlw                     
   real(r8) tch4                    



   data g1 /0.0468556,0.0397454,0.0407664,0.0304380,0.0540398,0.0321962/
   data g2 /14.4832,4.30242,5.23523,3.25342,0.698935,16.5599/
   data g3 /26.1898,18.4476,15.3633,12.1927,9.14992,8.07092/
   data g4 /0.0261782,0.0369516,0.0307266,0.0243854,0.0182932,0.0161418/
   data ab /3.0857e-2,2.3524e-2,1.7310e-2,2.6661e-2,2.8074e-2,2.2915e-2/
   data bb /-1.3512e-4,-6.8320e-5,-3.2609e-5,-1.0228e-5,-9.5743e-5,-1.0304e-4/
   data abp/2.9129e-2,2.4101e-2,1.9821e-2,2.6904e-2,2.9458e-2,1.9892e-2/
   data bbp/-1.3139e-4,-5.5688e-5,-4.6380e-5,-8.0362e-5,-1.0115e-4,-8.8061e-5/



   real(r8) func, u, b
   func(u,b) = u/sqrt(4.0 + u*(1.0 + 1.0 / b))



   do i = 1,ncol
      sqti(i) = sqrt(to3co2(i))



      tt(i) = abs(to3co2(i) - 250.0)
      ds2c(i) = abs(s2c(i,k1) - s2c(i,k2))
      duptyp(i) = abs(uptype(i,k1) - uptype(i,k2))
   end do

   do l = 1,6
      do i = 1,ncol
         psi1 = exp(abp(l)*tt(i) + bbp(l)*tt(i)*tt(i))
         phi1 = exp(ab(l)*tt(i) + bb(l)*tt(i)*tt(i))
         p1 = pnew(i)*(psi1/phi1)/sslp
         w1 = dw(i)*phi1
         tw(i,l) = exp(-g1(l)*p1*(sqrt(1.0 + g2(l)*(w1/p1)) - 1.0) - &
                   g3(l)*ds2c(i)-g4(l)*duptyp(i))
      end do
   end do

   do i=1,ncol
      tw(i,1)=tw(i,1)*(0.7*aer_trn_ttl(i,k1,k2,idx_LW_0650_0800)+&
                       0.3*aer_trn_ttl(i,k1,k2,idx_LW_0800_1000)) 
      tw(i,2)=tw(i,2)*aer_trn_ttl(i,k1,k2,idx_LW_0800_1000) 
      tw(i,3)=tw(i,3)*aer_trn_ttl(i,k1,k2,idx_LW_0800_1000) 
      tw(i,4)=tw(i,4)*aer_trn_ttl(i,k1,k2,idx_LW_0800_1000) 
      tw(i,5)=tw(i,5)*aer_trn_ttl(i,k1,k2,idx_LW_1000_1200) 
      tw(i,6)=tw(i,6)*aer_trn_ttl(i,k1,k2,idx_LW_1000_1200) 
   end do                    
   do i = 1,ncol
      du1 = abs(ucfc11(i,k1) - ucfc11(i,k2))
      du2 = abs(ucfc12(i,k1) - ucfc12(i,k2))



      tcfc3 = exp(-175.005*du1)
      tcfc4 = exp(-1202.18*du1)
      tcfc6 = exp(-5786.73*du2)
      tcfc7 = exp(-2873.51*du2)
      tcfc8 = exp(-2085.59*du2)



      acfc1 =  50.0*(1.0 - exp(-54.09*du1))*tw(i,1)*abplnk1(7,i,k2)
      acfc2 =  60.0*(1.0 - exp(-5130.03*du1))*tw(i,2)*abplnk1(8,i,k2)
      acfc3 =  60.0*(1.0 - tcfc3)*tw(i,4)*tcfc6*abplnk1(9,i,k2)
      acfc4 = 100.0*(1.0 - tcfc4)*tw(i,5)*abplnk1(10,i,k2)



      acfc5 = 45.0*(1.0 - exp(-1272.35*du2))*tw(i,3)*abplnk1(11,i,k2)
      acfc6 = 50.0*(1.0 - tcfc6)* tw(i,4) * abplnk1(12,i,k2)
      acfc7 = 80.0*(1.0 - tcfc7)* tw(i,5) * tcfc4*abplnk1(13,i,k2)
      acfc8 = 70.0*(1.0 - tcfc8)* tw(i,6) * abplnk1(14,i,k2)



      tlw = exp(-1.0*sqrt(dplh2o(i)))
      tlw=tlw*aer_trn_ttl(i,k1,k2,idx_LW_1200_2000)
      duch4 = abs(uch4(i,k1) - uch4(i,k2))
      dbetac = abs(bch4(i,k1) - bch4(i,k2))/duch4
      ach4 = 6.00444*sqti(i)*log(1.0 + func(duch4,dbetac))*tlw*abplnk1(3,i,k2)
      tch4 = 1.0/(1.0 + 0.02*func(duch4,dbetac))



      du01 = abs(un2o0(i,k1) - un2o0(i,k2))
      du11 = abs(un2o1(i,k1) - un2o1(i,k2))
      dbeta01 = abs(bn2o0(i,k1) - bn2o0(i,k2))/du01
      dbeta11 = abs(bn2o1(i,k1) - bn2o1(i,k2))/du11



      an2o1 = 2.35558*sqti(i)*log(1.0 + func(du01,dbeta01) &
              + func(du11,dbeta11))*tlw*tch4*abplnk1(4,i,k2)
      du02 = 0.100090*du01
      du12 = 0.0992746*du11
      dbeta02 = 0.964282*dbeta01



      an2o2 = 2.65581*sqti(i)*log(1.0 + func(du02,dbeta02) + &
              func(du12,dbeta02))*th2o(i)*tco2(i)*abplnk1(5,i,k2)
      du03 = 0.0333767*du01
      dbeta03 = 0.982143*dbeta01



      an2o3 = 2.54034*sqti(i)*log(1.0 + func(du03,dbeta03))* &
              tw(i,6)*tcfc8*abplnk1(6,i,k2)



      du11 = abs(uco211(i,k1) - uco211(i,k2))
      du12 = abs(uco212(i,k1) - uco212(i,k2))
      du13 = abs(uco213(i,k1) - uco213(i,k2))
      dbetc1 = 2.97558*abs(pnm(i,k1) + pnm(i,k2))/(2.0*sslp*sqti(i))
      dbetc2 = 2.0*dbetc1
      aco21 = 3.7571*sqti(i)*log(1.0 + func(du11,dbetc1) &
              + func(du12,dbetc2) + func(du13,dbetc2)) &
              *to3(i)*tw(i,5)*tcfc4*tcfc7*abplnk1(2,i,k2)



      du21 = abs(uco221(i,k1) - uco221(i,k2))
      du22 = abs(uco222(i,k1) - uco222(i,k2))
      du23 = abs(uco223(i,k1) - uco223(i,k2))
      aco22 = 3.8443*sqti(i)*log(1.0 + func(du21,dbetc1) &
              + func(du22,dbetc1) + func(du23,dbetc2)) &
              *tw(i,4)*tcfc3*tcfc6*abplnk1(1,i,k2)



      abstrc(i) = acfc1 + acfc2 + acfc3 + acfc4 + acfc5 + acfc6 + &
                  acfc7 + acfc8 + an2o1 + an2o2 + an2o3 + ach4 + &
                  aco21 + aco22
   end do

   return

end subroutine trcab



subroutine trcabn(lchnk   ,ncol    ,pcols, pverp,               &
                  k2      ,kn      ,ucfc11  ,ucfc12  ,un2o0   , &
                  un2o1   ,uch4    ,uco211  ,uco212  ,uco213  , &
                  uco221  ,uco222  ,uco223  ,tbar    ,bplnk   , &
                  winpl   ,pinpl   ,tco2    ,th2o    ,to3     , &
                  uptype  ,dw      ,s2c     ,up2     ,pnew    , &
                  abstrc  ,uinpl   , &
                  aer_trn_ngh)
















   implicit none
 




   integer, intent(in) :: lchnk                 
   integer, intent(in) :: ncol                  
   integer, intent(in) :: pcols, pverp
   integer, intent(in) :: k2                    
   integer, intent(in) :: kn                    

   real(r8), intent(in) :: tbar(pcols,4)        
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
   real(r8), intent(in) :: bplnk(14,pcols,4)    
   real(r8), intent(in) :: winpl(pcols,4)       
   real(r8), intent(in) :: pinpl(pcols,4)       

   real(r8), intent(in) :: tco2(pcols)          
   real(r8), intent(in) :: th2o(pcols)          
   real(r8), intent(in) :: to3(pcols)           
   real(r8), intent(in) :: dw(pcols)            
   real(r8), intent(in) :: pnew(pcols)          

   real(r8), intent(in) :: s2c(pcols,pverp)     
   real(r8), intent(in) :: uptype(pcols,pverp)  
   real(r8), intent(in) :: up2(pcols)           
   real(r8), intent(in) :: uinpl(pcols,4)       
   real(r8), intent(in) :: aer_trn_ngh(pcols,bnd_nbr_LW) 
                             
                             



   real(r8), intent(out) :: abstrc(pcols)        




   integer i,l                   

   real(r8) sqti(pcols)          
   real(r8) rsqti(pcols)         
   real(r8) du1                  
   real(r8) du2                  
   real(r8) acfc1                

   real(r8) acfc2                
   real(r8) acfc3                
   real(r8) acfc4                
   real(r8) acfc5                
   real(r8) acfc6                

   real(r8) acfc7                
   real(r8) acfc8                
   real(r8) du01                 
   real(r8) dbeta01              
   real(r8) dbeta11              

   real(r8)  an2o1               
   real(r8) du02                 
   real(r8) dbeta02              
   real(r8) an2o2                
   real(r8) du03                 

   real(r8) dbeta03              
   real(r8) an2o3                
   real(r8) duch4                
   real(r8) dbetac               
   real(r8) ach4                 

   real(r8) du11                 
   real(r8) du12                 
   real(r8) du13                 
   real(r8) dbetc1               
   real(r8) dbetc2               

   real(r8) aco21                
   real(r8) du21                 
   real(r8) du22                 
   real(r8) du23                 
   real(r8) aco22                

   real(r8) tt(pcols)            
   real(r8) psi1                 
   real(r8) phi1                 
   real(r8) p1                   
   real(r8) w1                   

   real(r8) ds2c(pcols)          
   real(r8) duptyp(pcols)        
   real(r8) tw(pcols,6)          
   real(r8) g1(6)                
   real(r8) g2(6)                

   real(r8) g3(6)                
   real(r8) g4(6)                
   real(r8) ab(6)                
   real(r8) bb(6)                
   real(r8) abp(6)               

   real(r8) bbp(6)               
   real(r8) tcfc3                
   real(r8) tcfc4                
   real(r8) tcfc6                
   real(r8) tcfc7                

   real(r8) tcfc8                
   real(r8) tlw                  
   real(r8) tch4                 



   data g1 /0.0468556,0.0397454,0.0407664,0.0304380,0.0540398,0.0321962/
   data g2 /14.4832,4.30242,5.23523,3.25342,0.698935,16.5599/
   data g3 /26.1898,18.4476,15.3633,12.1927,9.14992,8.07092/
   data g4 /0.0261782,0.0369516,0.0307266,0.0243854,0.0182932,0.0161418/
   data ab /3.0857e-2,2.3524e-2,1.7310e-2,2.6661e-2,2.8074e-2,2.2915e-2/
   data bb /-1.3512e-4,-6.8320e-5,-3.2609e-5,-1.0228e-5,-9.5743e-5,-1.0304e-4/
   data abp/2.9129e-2,2.4101e-2,1.9821e-2,2.6904e-2,2.9458e-2,1.9892e-2/
   data bbp/-1.3139e-4,-5.5688e-5,-4.6380e-5,-8.0362e-5,-1.0115e-4,-8.8061e-5/



   real(r8) func, u, b
   func(u,b) = u/sqrt(4.0 + u*(1.0 + 1.0 / b))



   do i = 1,ncol
      sqti(i) = sqrt(tbar(i,kn))
      rsqti(i) = 1. / sqti(i)



      tt(i) = abs(tbar(i,kn) - 250.0)
      ds2c(i) = abs(s2c(i,k2+1) - s2c(i,k2))*uinpl(i,kn)
      duptyp(i) = abs(uptype(i,k2+1) - uptype(i,k2))*uinpl(i,kn)
   end do

   do l = 1,6
      do i = 1,ncol
         psi1 = exp(abp(l)*tt(i)+bbp(l)*tt(i)*tt(i))
         phi1 = exp(ab(l)*tt(i)+bb(l)*tt(i)*tt(i))
         p1 = pnew(i) * (psi1/phi1) / sslp
         w1 = dw(i) * winpl(i,kn) * phi1
         tw(i,l) = exp(- g1(l)*p1*(sqrt(1.0+g2(l)*(w1/p1))-1.0) &
                   - g3(l)*ds2c(i)-g4(l)*duptyp(i))
      end do
   end do

   do i=1,ncol
      tw(i,1)=tw(i,1)*(0.7*aer_trn_ngh(i,idx_LW_0650_0800)+&
                       0.3*aer_trn_ngh(i,idx_LW_0800_1000))
      tw(i,2)=tw(i,2)*aer_trn_ngh(i,idx_LW_0800_1000) 
      tw(i,3)=tw(i,3)*aer_trn_ngh(i,idx_LW_0800_1000) 
      tw(i,4)=tw(i,4)*aer_trn_ngh(i,idx_LW_0800_1000) 
      tw(i,5)=tw(i,5)*aer_trn_ngh(i,idx_LW_1000_1200) 
      tw(i,6)=tw(i,6)*aer_trn_ngh(i,idx_LW_1000_1200) 
   end do                    

   do i = 1,ncol

      du1 = abs(ucfc11(i,k2+1) - ucfc11(i,k2)) * winpl(i,kn)
      du2 = abs(ucfc12(i,k2+1) - ucfc12(i,k2)) * winpl(i,kn)



      tcfc3 = exp(-175.005*du1)
      tcfc4 = exp(-1202.18*du1)
      tcfc6 = exp(-5786.73*du2)
      tcfc7 = exp(-2873.51*du2)
      tcfc8 = exp(-2085.59*du2)



      acfc1 = 50.0*(1.0 - exp(-54.09*du1)) * tw(i,1)*bplnk(7,i,kn)
      acfc2 = 60.0*(1.0 - exp(-5130.03*du1))*tw(i,2)*bplnk(8,i,kn)
      acfc3 = 60.0*(1.0 - tcfc3)*tw(i,4)*tcfc6 * bplnk(9,i,kn)
      acfc4 = 100.0*(1.0 - tcfc4)* tw(i,5) * bplnk(10,i,kn)



      acfc5 = 45.0*(1.0 - exp(-1272.35*du2))*tw(i,3)*bplnk(11,i,kn)
      acfc6 = 50.0*(1.0 - tcfc6)*tw(i,4)*bplnk(12,i,kn)
      acfc7 = 80.0*(1.0 - tcfc7)* tw(i,5)*tcfc4 *bplnk(13,i,kn)
      acfc8 = 70.0*(1.0 - tcfc8)*tw(i,6)*bplnk(14,i,kn)



      tlw = exp(-1.0*sqrt(up2(i)))
      tlw=tlw*aer_trn_ngh(i,idx_LW_1200_2000)
      duch4 = abs(uch4(i,k2+1) - uch4(i,k2)) * winpl(i,kn)
      dbetac = 2.94449 * pinpl(i,kn) * rsqti(i) / sslp
      ach4 = 6.00444*sqti(i)*log(1.0 + func(duch4,dbetac)) * tlw * bplnk(3,i,kn)
      tch4 = 1.0/(1.0 + 0.02*func(duch4,dbetac))



      du01 = abs(un2o0(i,k2+1) - un2o0(i,k2)) * winpl(i,kn)
      du11 = abs(un2o1(i,k2+1) - un2o1(i,k2)) * winpl(i,kn)
      dbeta01 = 19.399 *  pinpl(i,kn) * rsqti(i) / sslp
      dbeta11 = dbeta01



      an2o1 = 2.35558*sqti(i)*log(1.0 + func(du01,dbeta01) &
              + func(du11,dbeta11)) * tlw * tch4 * bplnk(4,i,kn)
      du02 = 0.100090*du01
      du12 = 0.0992746*du11
      dbeta02 = 0.964282*dbeta01



      an2o2 = 2.65581*sqti(i)*log(1.0 + func(du02,dbeta02) &
              +  func(du12,dbeta02)) * tco2(i) * th2o(i) * bplnk(5,i,kn)
      du03 = 0.0333767*du01
      dbeta03 = 0.982143*dbeta01



      an2o3 = 2.54034*sqti(i)*log(1.0 + func(du03,dbeta03)) * &
              tw(i,6) * tcfc8 * bplnk(6,i,kn)



      du11 = abs(uco211(i,k2+1) - uco211(i,k2)) * winpl(i,kn)
      du12 = abs(uco212(i,k2+1) - uco212(i,k2)) * winpl(i,kn)
      du13 = abs(uco213(i,k2+1) - uco213(i,k2)) * winpl(i,kn)
      dbetc1 = 2.97558 * pinpl(i,kn) * rsqti(i) / sslp
      dbetc2 = 2.0 * dbetc1
      aco21 = 3.7571*sqti(i)*log(1.0 + func(du11,dbetc1) &
              + func(du12,dbetc2) + func(du13,dbetc2)) &
              * to3(i) * tw(i,5) * tcfc4 * tcfc7 * bplnk(2,i,kn)



      du21 = abs(uco221(i,k2+1) - uco221(i,k2)) * winpl(i,kn)
      du22 = abs(uco222(i,k2+1) - uco222(i,k2)) * winpl(i,kn)
      du23 = abs(uco223(i,k2+1) - uco223(i,k2)) * winpl(i,kn)
      aco22 = 3.8443*sqti(i)*log(1.0 + func(du21,dbetc1) &
              + func(du22,dbetc1) + func(du23,dbetc2)) &
              * tw(i,4) * tcfc3 * tcfc6 * bplnk(1,i,kn)



      abstrc(i) = acfc1 + acfc2 + acfc3 + acfc4 + acfc5 + acfc6 + &
                  acfc7 + acfc8 + an2o1 + an2o2 + an2o3 + ach4 + &
                  aco21 + aco22
   end do

   return

end subroutine trcabn



subroutine trcems(lchnk   ,ncol    ,pcols, pverp,               &
                  k       ,co2t    ,pnm     ,ucfc11  ,ucfc12  , &
                  un2o0   ,un2o1   ,bn2o0   ,bn2o1   ,uch4    , &
                  bch4    ,uco211  ,uco212  ,uco213  ,uco221  , &
                  uco222  ,uco223  ,uptype  ,w       ,s2c     , &
                  up2     ,emplnk  ,th2o    ,tco2    ,to3     , &
                  emstrc  , &
                 aer_trn_ttl)















   implicit none






   integer, intent(in) :: lchnk                 
   integer, intent(in) :: ncol                  
   integer, intent(in) :: pcols, pverp

   real(r8), intent(in) :: co2t(pcols,pverp)    
   real(r8), intent(in) :: pnm(pcols,pverp)     
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
   real(r8), intent(in) :: emplnk(14,pcols)     
   real(r8), intent(in) :: th2o(pcols)          
   real(r8), intent(in) :: tco2(pcols)          

   real(r8), intent(in) :: to3(pcols)           
   real(r8), intent(in) :: s2c(pcols,pverp)     
   real(r8), intent(in) :: w(pcols,pverp)       
   real(r8), intent(in) :: up2(pcols)           

   integer, intent(in) :: k                 

   real(r8), intent(in) :: aer_trn_ttl(pcols,pverp,pverp,bnd_nbr_LW) 




   real(r8), intent(out) :: emstrc(pcols,pverp)  




   integer i,l               

   real(r8) sqti(pcols)          
   real(r8) ecfc1                
   real(r8) ecfc2                
   real(r8) ecfc3                
   real(r8) ecfc4                

   real(r8) ecfc5                
   real(r8) ecfc6                
   real(r8) ecfc7                
   real(r8) ecfc8                
   real(r8) u01                  

   real(r8) u11                  
   real(r8) beta01               
   real(r8) beta11               
   real(r8) en2o1                
   real(r8) u02                  

   real(r8) u12                  
   real(r8) beta02               
   real(r8) en2o2                
   real(r8) u03                  
   real(r8) beta03               

   real(r8) en2o3                
   real(r8) betac                
   real(r8) ech4                 
   real(r8) betac1               
   real(r8) betac2               

   real(r8) eco21                
   real(r8) eco22                
   real(r8) tt(pcols)            
   real(r8) psi1                 
   real(r8) phi1                 

   real(r8) p1                   
   real(r8) w1                   
   real(r8) tw(pcols,6)          
   real(r8) g1(6)                
   real(r8) g2(6)                

   real(r8) g3(6)                
   real(r8) g4(6)                
   real(r8) ab(6)                
   real(r8) bb(6)                
   real(r8) abp(6)               

   real(r8) bbp(6)               
   real(r8) tcfc3                
   real(r8) tcfc4                
   real(r8) tcfc6                
   real(r8) tcfc7                

   real(r8) tcfc8                
   real(r8) tlw                  
   real(r8) tch4                 



   data g1 /0.0468556,0.0397454,0.0407664,0.0304380,0.0540398,0.0321962/
   data g2 /14.4832,4.30242,5.23523,3.25342,0.698935,16.5599/
   data g3 /26.1898,18.4476,15.3633,12.1927,9.14992,8.07092/
   data g4 /0.0261782,0.0369516,0.0307266,0.0243854,0.0182932,0.0161418/
   data ab /3.0857e-2,2.3524e-2,1.7310e-2,2.6661e-2,2.8074e-2,2.2915e-2/
   data bb /-1.3512e-4,-6.8320e-5,-3.2609e-5,-1.0228e-5,-9.5743e-5,-1.0304e-4/
   data abp/2.9129e-2,2.4101e-2,1.9821e-2,2.6904e-2,2.9458e-2,1.9892e-2/
   data bbp/-1.3139e-4,-5.5688e-5,-4.6380e-5,-8.0362e-5,-1.0115e-4,-8.8061e-5/



   real(r8) func, u, b
   func(u,b) = u/sqrt(4.0 + u*(1.0 + 1.0 / b))



   do i = 1,ncol
      sqti(i) = sqrt(co2t(i,k))



      tt(i) = abs(co2t(i,k) - 250.0)
   end do

   do l = 1,6
      do i = 1,ncol
         psi1 = exp(abp(l)*tt(i)+bbp(l)*tt(i)*tt(i))
         phi1 = exp(ab(l)*tt(i)+bb(l)*tt(i)*tt(i))
         p1 = pnm(i,k) * (psi1/phi1) / sslp
         w1 = w(i,k) * phi1
         tw(i,l) = exp(- g1(l)*p1*(sqrt(1.0+g2(l)*(w1/p1))-1.0) &
                   - g3(l)*s2c(i,k)-g4(l)*uptype(i,k))
      end do
   end do




      do i=1,ncol
         tw(i,1)=tw(i,1)*(0.7*aer_trn_ttl(i,k,1,idx_LW_0650_0800)+&
                          0.3*aer_trn_ttl(i,k,1,idx_LW_0800_1000))
         tw(i,2)=tw(i,2)*aer_trn_ttl(i,k,1,idx_LW_0800_1000) 
         tw(i,3)=tw(i,3)*aer_trn_ttl(i,k,1,idx_LW_0800_1000) 
         tw(i,4)=tw(i,4)*aer_trn_ttl(i,k,1,idx_LW_0800_1000) 
         tw(i,5)=tw(i,5)*aer_trn_ttl(i,k,1,idx_LW_1000_1200) 
         tw(i,6)=tw(i,6)*aer_trn_ttl(i,k,1,idx_LW_1000_1200) 
      end do                    

   do i = 1,ncol



      tcfc3 = exp(-175.005*ucfc11(i,k))
      tcfc4 = exp(-1202.18*ucfc11(i,k))
      tcfc6 = exp(-5786.73*ucfc12(i,k))
      tcfc7 = exp(-2873.51*ucfc12(i,k))
      tcfc8 = exp(-2085.59*ucfc12(i,k))



      ecfc1 = 50.0*(1.0 - exp(-54.09*ucfc11(i,k))) * tw(i,1) * emplnk(7,i)
      ecfc2 = 60.0*(1.0 - exp(-5130.03*ucfc11(i,k)))* tw(i,2) * emplnk(8,i)
      ecfc3 = 60.0*(1.0 - tcfc3)*tw(i,4)*tcfc6*emplnk(9,i)
      ecfc4 = 100.0*(1.0 - tcfc4)*tw(i,5)*emplnk(10,i)



      ecfc5 = 45.0*(1.0 - exp(-1272.35*ucfc12(i,k)))*tw(i,3)*emplnk(11,i)
      ecfc6 = 50.0*(1.0 - tcfc6)*tw(i,4)*emplnk(12,i)
      ecfc7 = 80.0*(1.0 - tcfc7)*tw(i,5)* tcfc4 * emplnk(13,i)
      ecfc8 = 70.0*(1.0 - tcfc8)*tw(i,6) * emplnk(14,i)



      tlw = exp(-1.0*sqrt(up2(i)))




            tlw=tlw*aer_trn_ttl(i,k,1,idx_LW_1200_2000)
      betac = bch4(i,k)/uch4(i,k)
      ech4 = 6.00444*sqti(i)*log(1.0 + func(uch4(i,k),betac)) *tlw * emplnk(3,i)
      tch4 = 1.0/(1.0 + 0.02*func(uch4(i,k),betac))



      u01 = un2o0(i,k)
      u11 = un2o1(i,k)
      beta01 = bn2o0(i,k)/un2o0(i,k)
      beta11 = bn2o1(i,k)/un2o1(i,k)



      en2o1 = 2.35558*sqti(i)*log(1.0 + func(u01,beta01) + &
              func(u11,beta11))*tlw*tch4*emplnk(4,i)
      u02 = 0.100090*u01
      u12 = 0.0992746*u11
      beta02 = 0.964282*beta01



      en2o2 = 2.65581*sqti(i)*log(1.0 + func(u02,beta02) + &
              func(u12,beta02)) * tco2(i) * th2o(i) * emplnk(5,i)
      u03 = 0.0333767*u01
      beta03 = 0.982143*beta01



      en2o3 = 2.54034*sqti(i)*log(1.0 + func(u03,beta03)) * &
              tw(i,6) * tcfc8 * emplnk(6,i)



      betac1 = 2.97558*pnm(i,k) / (sslp*sqti(i))
      betac2 = 2.0 * betac1
      eco21 = 3.7571*sqti(i)*log(1.0 + func(uco211(i,k),betac1) &
              + func(uco212(i,k),betac2) + func(uco213(i,k),betac2)) &
              * to3(i) * tw(i,5) * tcfc4 * tcfc7 * emplnk(2,i)



      eco22 = 3.8443*sqti(i)*log(1.0 + func(uco221(i,k),betac1) &
              + func(uco222(i,k),betac1) + func(uco223(i,k),betac2)) &
              * tw(i,4) * tcfc3 * tcfc6 * emplnk(1,i)



      emstrc(i,k) = ecfc1 + ecfc2 + ecfc3 + ecfc4 + ecfc5 +ecfc6 + &
                    ecfc7 + ecfc8 + en2o1 + en2o2 + en2o3 + ech4 + &
                    eco21 + eco22
   end do

   return

end subroutine trcems

subroutine trcmix(lchnk   ,ncol     ,pcols, pver, &
                  pmid    ,clat, n2o      ,ch4     ,          &
                  cfc11   , cfc12   )






















   implicit none





   integer, intent(in) :: lchnk                    
   integer, intent(in) :: ncol                     
   integer, intent(in) :: pcols, pver

   real(r8), intent(in) :: pmid(pcols,pver)        
   real(r8), intent(in) :: clat(pcols)             



   real(r8), intent(out) :: n2o(pcols,pver)         
   real(r8), intent(out) :: ch4(pcols,pver)         
   real(r8), intent(out) :: cfc11(pcols,pver)       
   real(r8), intent(out) :: cfc12(pcols,pver)       




   real(r8) :: rmwn2o       
   real(r8) :: rmwch4       
   real(r8) :: rmwf11       
   real(r8) :: rmwf12       

   integer i                
   integer k                


   real(r8) coslat(pcols)       
   real(r8) dlat                
   real(r8) ptrop               
   real(r8) pratio              

   real(r8) xn2o                
   real(r8) xch4                
   real(r8) xcfc11              
   real(r8) xcfc12              

   real(r8) ch40                
   real(r8) n2o0                
   real(r8) cfc110              
   real(r8) cfc120              


   rmwn2o = mwn2o/mwdry      
   rmwch4 = mwch4/mwdry      
   rmwf11 = mwf11/mwdry      
   rmwf12 = mwf12/mwdry      




   do i = 1, ncol
      coslat(i) = cos(clat(i))
   end do



   ch40   = rmwch4 * ch4vmr
   n2o0   = rmwn2o * n2ovmr
   cfc110 = rmwf11 * f11vmr
   cfc120 = rmwf12 * f12vmr

   do i = 1, ncol
      coslat(i) = cos(clat(i))
   end do

   do k = 1,pver
      do i = 1,ncol


         dlat = abs(57.2958 * clat(i))
         if(dlat.le.45.0) then
            xn2o = 0.3478 + 0.00116 * dlat
            xch4 = 0.2353
            xcfc11 = 0.7273 + 0.00606 * dlat
            xcfc12 = 0.4000 + 0.00222 * dlat
         else
            xn2o = 0.4000 + 0.013333 * (dlat - 45)
            xch4 = 0.2353 + 0.0225489 * (dlat - 45)
            xcfc11 = 1.00 + 0.013333 * (dlat - 45)
            xcfc12 = 0.50 + 0.024444 * (dlat - 45)
         end if


         ptrop = 250.0e2 - 150.0e2*coslat(i)**2.0


         if (pmid(i,k) >= ptrop) then
            ch4(i,k) = ch40
            n2o(i,k) = n2o0
            cfc11(i,k) = cfc110
            cfc12(i,k) = cfc120
         else
            pratio = pmid(i,k)/ptrop
            ch4(i,k) = ch40 * (pratio)**xch4
            n2o(i,k) = n2o0 * (pratio)**xn2o
            cfc11(i,k) = cfc110 * (pratio)**xcfc11
            cfc12(i,k) = cfc120 * (pratio)**xcfc12
         end if
      end do
   end do

   return

end subroutine trcmix





subroutine trcplk(lchnk   ,ncol    ,pcols, pver, pverp,         &
                  tint    ,tlayr   ,tplnke  ,emplnk  ,abplnk1 , &
                  abplnk2 )















   implicit none




   integer, intent(in) :: lchnk                
   integer, intent(in) :: ncol                 
   integer, intent(in) :: pcols, pver, pverp

   real(r8), intent(in) :: tint(pcols,pverp)   
   real(r8), intent(in) :: tlayr(pcols,pverp)  
   real(r8), intent(in) :: tplnke(pcols)       



   real(r8), intent(out) :: emplnk(14,pcols)         
   real(r8), intent(out) :: abplnk1(14,pcols,pverp)  
   real(r8), intent(out) :: abplnk2(14,pcols,pverp)  




   integer wvl                   
   integer i,k                   

   real(r8) f1(14)                   
   real(r8) f2(14)                   
   real(r8) f3(14)                   



   data f1 /5.85713e8,7.94950e8,1.47009e9,1.40031e9,1.34853e8, &
            1.05158e9,3.35370e8,3.99601e8,5.35994e8,8.42955e8, &
            4.63682e8,5.18944e8,8.83202e8,1.03279e9/
   data f2 /2.02493e11,3.04286e11,6.90698e11,6.47333e11, &
            2.85744e10,4.41862e11,9.62780e10,1.21618e11, &
            1.79905e11,3.29029e11,1.48294e11,1.72315e11, &
            3.50140e11,4.31364e11/
   data f3 /1383.0,1531.0,1879.0,1849.0,848.0,1681.0, &
            1148.0,1217.0,1343.0,1561.0,1279.0,1328.0, &
            1586.0,1671.0/





   do wvl = 1,14
      do i = 1,ncol
         emplnk(wvl,i) = f1(wvl)/(tplnke(i)**4.0*(exp(f3(wvl)/tplnke(i))-1.0))
      end do
   end do



   do wvl = 1,14
      do k = ntoplw, pverp
         do i = 1, ncol



            abplnk1(wvl,i,k) = (f2(wvl)*exp(f3(wvl)/tint(i,k)))  &
                               /(tint(i,k)**5.0*(exp(f3(wvl)/tint(i,k))-1.0)**2.0)



            abplnk2(wvl,i,k) = (f2(wvl)*exp(f3(wvl)/tlayr(i,k))) &
                               /(tlayr(i,k)**5.0*(exp(f3(wvl)/tlayr(i,k))-1.0)**2.0)
         end do
      end do
   end do

   return
end subroutine trcplk

subroutine trcpth(lchnk   ,ncol    ,pcols, pver, pverp,         &
                  tnm     ,pnm     ,cfc11   ,cfc12   ,n2o     , &
                  ch4     ,qnm     ,ucfc11  ,ucfc12  ,un2o0   , &
                  un2o1   ,uch4    ,uco211  ,uco212  ,uco213  , &
                  uco221  ,uco222  ,uco223  ,bn2o0   ,bn2o1   , &
                  bch4    ,uptype  )
















   implicit none





   integer, intent(in) :: lchnk                 
   integer, intent(in) :: ncol                  
   integer, intent(in) :: pcols, pver, pverp

   real(r8), intent(in) :: tnm(pcols,pver)      
   real(r8), intent(in) :: pnm(pcols,pverp)     
   real(r8), intent(in) :: qnm(pcols,pver)      
   real(r8), intent(in) :: cfc11(pcols,pver)    

   real(r8), intent(in) :: cfc12(pcols,pver)    
   real(r8), intent(in) :: n2o(pcols,pver)      
   real(r8), intent(in) :: ch4(pcols,pver)      




   real(r8), intent(out) :: ucfc11(pcols,pverp)  
   real(r8), intent(out) :: ucfc12(pcols,pverp)  
   real(r8), intent(out) :: un2o0(pcols,pverp)   
   real(r8), intent(out) :: un2o1(pcols,pverp)   
   real(r8), intent(out) :: uch4(pcols,pverp)    

   real(r8), intent(out) :: uco211(pcols,pverp)  
   real(r8), intent(out) :: uco212(pcols,pverp)  
   real(r8), intent(out) :: uco213(pcols,pverp)  
   real(r8), intent(out) :: uco221(pcols,pverp)  
   real(r8), intent(out) :: uco222(pcols,pverp)  

   real(r8), intent(out) :: uco223(pcols,pverp)  
   real(r8), intent(out) :: bn2o0(pcols,pverp)   
   real(r8), intent(out) :: bn2o1(pcols,pverp)   
   real(r8), intent(out) :: bch4(pcols,pverp)    
   real(r8), intent(out) :: uptype(pcols,pverp)  




   integer   i               
   integer   k               

   real(r8) co2fac(pcols,1)      
   real(r8) alpha1(pcols)        
   real(r8) alpha2(pcols)        
   real(r8) rt(pcols)            
   real(r8) rsqrt(pcols)         

   real(r8) pbar(pcols)          
   real(r8) dpnm(pcols)          
   real(r8) diff                 



   data diff /1.66/





   do i = 1,ncol
      ucfc11(i,ntoplw) = 1.8 * cfc11(i,ntoplw) * pnm(i,ntoplw) * rga
      ucfc12(i,ntoplw) = 1.8 * cfc12(i,ntoplw) * pnm(i,ntoplw) * rga
      un2o0(i,ntoplw) = diff * 1.02346e5 * n2o(i,ntoplw) * pnm(i,ntoplw) * rga / sqrt(tnm(i,ntoplw))
      un2o1(i,ntoplw) = diff * 2.01909 * un2o0(i,ntoplw) * exp(-847.36/tnm(i,ntoplw))
      uch4(i,ntoplw)  = diff * 8.60957e4 * ch4(i,ntoplw) * pnm(i,ntoplw) * rga / sqrt(tnm(i,ntoplw))
      co2fac(i,1)     = diff * co2mmr * pnm(i,ntoplw) * rga
      alpha1(i) = (1.0 - exp(-1540.0/tnm(i,ntoplw)))**3.0/sqrt(tnm(i,ntoplw))
      alpha2(i) = (1.0 - exp(-1360.0/tnm(i,ntoplw)))**3.0/sqrt(tnm(i,ntoplw))
      uco211(i,ntoplw) = 3.42217e3 * co2fac(i,1) * alpha1(i) * exp(-1849.7/tnm(i,ntoplw))
      uco212(i,ntoplw) = 6.02454e3 * co2fac(i,1) * alpha1(i) * exp(-2782.1/tnm(i,ntoplw))
      uco213(i,ntoplw) = 5.53143e3 * co2fac(i,1) * alpha1(i) * exp(-3723.2/tnm(i,ntoplw))
      uco221(i,ntoplw) = 3.88984e3 * co2fac(i,1) * alpha2(i) * exp(-1997.6/tnm(i,ntoplw))
      uco222(i,ntoplw) = 3.67108e3 * co2fac(i,1) * alpha2(i) * exp(-3843.8/tnm(i,ntoplw))
      uco223(i,ntoplw) = 6.50642e3 * co2fac(i,1) * alpha2(i) * exp(-2989.7/tnm(i,ntoplw))
      bn2o0(i,ntoplw) = diff * 19.399 * pnm(i,ntoplw)**2.0 * n2o(i,ntoplw) * &
                   1.02346e5 * rga / (sslp*tnm(i,ntoplw))
      bn2o1(i,ntoplw) = bn2o0(i,ntoplw) * exp(-847.36/tnm(i,ntoplw)) * 2.06646e5
      bch4(i,ntoplw) = diff * 2.94449 * ch4(i,ntoplw) * pnm(i,ntoplw)**2.0 * rga * &
                  8.60957e4 / (sslp*tnm(i,ntoplw))
      uptype(i,ntoplw) = diff * qnm(i,ntoplw) * pnm(i,ntoplw)**2.0 *  &
                    exp(1800.0*(1.0/tnm(i,ntoplw) - 1.0/296.0)) * rga / sslp
   end do



   do k = ntoplw,pver
      do i = 1,ncol
         rt(i) = 1./tnm(i,k)
         rsqrt(i) = sqrt(rt(i))
         pbar(i) = 0.5 * (pnm(i,k+1) + pnm(i,k)) / sslp
         dpnm(i) = (pnm(i,k+1) - pnm(i,k)) * rga
         alpha1(i) = diff * rsqrt(i) * (1.0 - exp(-1540.0/tnm(i,k)))**3.0
         alpha2(i) = diff * rsqrt(i) * (1.0 - exp(-1360.0/tnm(i,k)))**3.0
         ucfc11(i,k+1) = ucfc11(i,k) +  1.8 * cfc11(i,k) * dpnm(i)
         ucfc12(i,k+1) = ucfc12(i,k) +  1.8 * cfc12(i,k) * dpnm(i)
         un2o0(i,k+1) = un2o0(i,k) + diff * 1.02346e5 * n2o(i,k) * rsqrt(i) * dpnm(i)
         un2o1(i,k+1) = un2o1(i,k) + diff * 2.06646e5 * n2o(i,k) * &
                        rsqrt(i) * exp(-847.36/tnm(i,k)) * dpnm(i)
         uch4(i,k+1) = uch4(i,k) + diff * 8.60957e4 * ch4(i,k) * rsqrt(i) * dpnm(i)
         uco211(i,k+1) = uco211(i,k) + 1.15*3.42217e3 * alpha1(i) * &
                         co2mmr * exp(-1849.7/tnm(i,k)) * dpnm(i)
         uco212(i,k+1) = uco212(i,k) + 1.15*6.02454e3 * alpha1(i) * &
                         co2mmr * exp(-2782.1/tnm(i,k)) * dpnm(i)
         uco213(i,k+1) = uco213(i,k) + 1.15*5.53143e3 * alpha1(i) * &
                         co2mmr * exp(-3723.2/tnm(i,k)) * dpnm(i)
         uco221(i,k+1) = uco221(i,k) + 1.15*3.88984e3 * alpha2(i) * &
                         co2mmr * exp(-1997.6/tnm(i,k)) * dpnm(i)
         uco222(i,k+1) = uco222(i,k) + 1.15*3.67108e3 * alpha2(i) * &
                         co2mmr * exp(-3843.8/tnm(i,k)) * dpnm(i)
         uco223(i,k+1) = uco223(i,k) + 1.15*6.50642e3 * alpha2(i) * &
                         co2mmr * exp(-2989.7/tnm(i,k)) * dpnm(i)
         bn2o0(i,k+1) = bn2o0(i,k) + diff * 19.399 * pbar(i) * rt(i) &
                        * 1.02346e5 * n2o(i,k) * dpnm(i)
         bn2o1(i,k+1) = bn2o1(i,k) + diff * 19.399 * pbar(i) * rt(i) &
                        * 2.06646e5 * exp(-847.36/tnm(i,k)) * n2o(i,k)*dpnm(i)
         bch4(i,k+1) = bch4(i,k) + diff * 2.94449 * rt(i) * pbar(i) &
                       * 8.60957e4 * ch4(i,k) * dpnm(i)
         uptype(i,k+1) = uptype(i,k) + diff *qnm(i,k) * &
                         exp(1800.0*(1.0/tnm(i,k) - 1.0/296.0)) * pbar(i) * dpnm(i)
      end do
   end do

   return
end subroutine trcpth



subroutine aqsat(t       ,p       ,es      ,qs        ,ii      , &
                 ilen    ,kk      ,kstart  ,kend      )



















   integer, intent(in) :: ii             
   integer, intent(in) :: kk             
   integer, intent(in) :: ilen           
   integer, intent(in) :: kstart         
   integer, intent(in) :: kend           
   real(r8), intent(in) :: t(ii,kk)          
   real(r8), intent(in) :: p(ii,kk)          



   real(r8), intent(out) :: es(ii,kk)         
   real(r8), intent(out) :: qs(ii,kk)         



   real(r8) omeps             
   integer i, k           



   omeps = 1.0 - epsqs
   do k=kstart,kend
      do i=1,ilen
         es(i,k) = estblf(t(i,k))



         qs(i,k) = epsqs*es(i,k)/(p(i,k) - omeps*es(i,k))




         qs(i,k) = min(1.0_r8,qs(i,k))

         if (qs(i,k) < 0.0) then
            qs(i,k) = 1.0
            es(i,k) = p(i,k)
         end if
      end do
   end do

   return
end subroutine aqsat

  subroutine cldefr(lchnk   ,ncol    ,pcols, pver, pverp, &
       landfrac,t       ,rel     ,rei     ,ps      ,pmid    , landm, icefrac, snowh)












    implicit none




    integer, intent(in) :: lchnk                 
    integer, intent(in) :: ncol                  
    integer, intent(in) :: pcols, pver, pverp

    real(r8), intent(in) :: landfrac(pcols)      
    real(r8), intent(in) :: icefrac(pcols)       
    real(r8), intent(in) :: t(pcols,pver)        
    real(r8), intent(in) :: ps(pcols)            
    real(r8), intent(in) :: pmid(pcols,pver)     
    real(r8), intent(in) :: landm(pcols)
    real(r8), intent(in) :: snowh(pcols)         



    real(r8), intent(out) :: rel(pcols,pver)      
    real(r8), intent(out) :: rei(pcols,pver)      




         call reltab(ncol, pcols, pver, t, landfrac, landm, icefrac, rel, snowh)


         call reitab(ncol, pcols, pver, t, rei)



    return
  end subroutine cldefr


subroutine background(lchnk, ncol, pint, pcols, pverr, pverrp, mmr)


















   implicit none






   integer, intent(in) :: lchnk                 
   integer, intent(in) :: ncol                  
   integer, intent(in) :: pcols,pverr,pverrp

   real(r8), intent(in) :: pint(pcols,pverrp)   



   real(r8), intent(out) :: mmr(pcols,pverr)    



   integer i          
   integer k          

   real(r8) mass2mmr  
   real(r8) mass      



   do i=1,ncol
      mass2mmr =  gravmks / (pint(i,pverrp)-pint(i,pverrp-mxaerl))
      do k=1,pverr




        if ( k >= pverrp-mxaerl ) then



            mass = tauback / (1.e3 * kbg(idxVIS))
            mmr(i,k) = mass2mmr*mass
         else
            mmr(i,k) = 0._r8
         endif

      enddo
   enddo

   return
end subroutine background

subroutine scale_aerosols(AEROSOLt, pcols, pver, ncol, lchnk, scale)



  integer, intent(in) :: ncol, lchnk 
  integer, intent(in) :: pcols, pver
  real(r8), intent(in) :: scale(naer_all) 
  real(r8), intent(inout) :: AEROSOLt(pcols, pver, naer_all) 
  integer m

  do m = 1, naer_all
     AEROSOLt(:ncol, :, m) = scale(m)*AEROSOLt(:ncol, :, m)
  end do

  return
end subroutine scale_aerosols

subroutine get_int_scales(scales)
  real(r8), intent(out)::scales(naer_all)  
  integer i                                  


  scales = 1.

  scales(idxBG) = 1._r8
  scales(idxSUL) = sulscl 
  scales(idxSSLT) = ssltscl  
  
  do i = idxCARBONfirst, idxCARBONfirst+numCARBON-1
    scales(i) = carscl
  enddo
  
  do i = idxDUSTfirst, idxDUSTfirst+numDUST-1
    scales(i) = dustscl
  enddo

  scales(idxVOLC) = volcscl

  return
end subroutine get_int_scales

subroutine vert_interpolate (Match_ps, aerosolc, m_hybi, paerlev, naer_c, pint, n, AEROSOL_mmr, pcols, pver, pverp, ncol, c)















   integer, intent(in)  :: paerlev,naer_c,pcols,pver,pverp
   real(r8), intent(out) :: AEROSOL_mmr(pcols,pver,naer)  
   real(r8), intent(in) :: Match_ps(pcols)                
   real(r8), intent(in) :: pint(pcols,pverp)              
   real(r8), intent(in) :: aerosolc(pcols,paerlev,naer_c)
   real(r8), intent(in) :: m_hybi(paerlev)

   integer, intent(in) :: ncol,c                          
   integer, intent(in) :: n                               



   integer m                           
   integer kupper(pcols)               
   integer i, k, kk, kkstart, kount    
   integer isv, ksv, msv               

   logical bad                         
   logical lev_interp_comp             

   real(r8) AEROSOL(pcols,pverp,naer)  
                                       
   real(r8) dpl, dpu                   
   real(r8) v_coord                    
   real(r8) m_to_mmr                   
   real(r8) AER_diff                   






   do i=1,ncol
      kupper(i) = 1
   end do



   
   do i=1,ncol
   do m=1,naer
   AEROSOL(i,1,m) = AEROSOLc(i,1,m)
   enddo
   enddo



   do k=2,pver




      kkstart = paerlev
      do i=1,ncol
         kkstart = min0(kkstart,kupper(i))
      end do
      kount = 0






      lev_interp_comp = .false.
      do kk=kkstart,paerlev-1
         if(.not.lev_interp_comp) then
         do i=1,ncol
            v_coord = pint(i,k)
            if (M_hybi(kk)*Match_ps(i) .lt. v_coord .and. v_coord .le. M_hybi(kk+1)*Match_ps(i)) then
               kupper(i) = kk
               kount = kount + 1
            end if
         end do






         if (kount.eq.ncol) then
            do i=1,ncol
             do m=1,naer
               dpu = pint(i,k) - M_hybi(kupper(i))*Match_ps(i)
               dpl = M_hybi(kupper(i)+1)*Match_ps(i) - pint(i,k)
               AEROSOL(i,k,m) = &
                    (AEROSOLc(i,kupper(i)  ,m)*dpl + &
                     AEROSOLc(i,kupper(i)+1,m)*dpu)/(dpl + dpu)
             enddo
            enddo 
            lev_interp_comp = .true.
         end if
         end if
      end do







      if(.not.lev_interp_comp) then
         do i=1,ncol
          do m=1,naer 
            if (pint(i,k) .lt. M_hybi(1)*Match_ps(i)) then
               AEROSOL(i,k,m) =  AEROSOLc(i,1,m)
            else if (pint(i,k) .gt. M_hybi(paerlev)*Match_ps(i)) then
               AEROSOL(i,k,m) = 0.0
            else
               dpu = pint(i,k) - M_hybi(kupper(i))*Match_ps(i)
               dpl = M_hybi(kupper(i)+1)*Match_ps(i) - pint(i,k)
               AEROSOL(i,k,m) = &
                    (AEROSOLc(i,kupper(i)  ,m)*dpl + &
                     AEROSOLc(i,kupper(i)+1,m)*dpu)/(dpl + dpu)
            end if
          enddo
         end do

         if (kount.gt.ncol) then
            call endrun ('VERT_INTERPOLATE: Bad data: non-monotonicity suspected in dependent variable')
         end if
      end if
   end do





   AEROSOL(1:ncol,pverp,:) = 0.




   do m = 1, naer
      do k = 1, pver
         do i = 1, ncol
            if (AEROSOL(i,k,m) < 1.e-40_r8) AEROSOL(i,k,m) = 0.
         end do
      end do
   end do






   do m = 1, naer
      do k = 1, pver
         do i = 1, ncol
            AER_diff = AEROSOL(i,k,m) - AEROSOL(i,k+1,m)
            if( abs(AER_diff) < 1e-15*AEROSOL(i,1,m)) then
               AER_diff = 0.
            end if
            m_to_mmr = gravmks / (pint(i,k+1)-pint(i,k))
            AEROSOL_mmr(i,k,m)= AER_diff * m_to_mmr
            if (AEROSOL_mmr(i,k,m) < 0) then
               write(6,*)'vert_interpolate: mmr < 0, m, col, lev, mmr',m, i, k, AEROSOL_mmr(i,k,m)
               write(6,*)'vert_interpolate: aerosol(k),(k+1)',AEROSOL(i,k,m),AEROSOL(i,k+1,m)
               write(6,*)'vert_interpolate: pint(k+1),(k)',pint(i,k+1),pint(i,k)
               write(6,*)'n,c',n,c


               call endrun()
            end if
         end do
      end do
   end do




   return
end subroutine vert_interpolate



  subroutine cldems(lchnk   ,ncol    ,pcols, pver, pverp, clwp    ,fice    ,rei     ,emis    )













    implicit none


    real(r8) kabsl                  
    parameter (kabsl = 0.090361)





    integer, intent(in) :: lchnk                   
    integer, intent(in) :: ncol                    
    integer, intent(in) :: pcols, pver, pverp

    real(r8), intent(in) :: clwp(pcols,pver)       
    real(r8), intent(in) :: rei(pcols,pver)        
    real(r8), intent(in) :: fice(pcols,pver)       



    real(r8), intent(out) :: emis(pcols,pver)       



    integer i,k                 
    real(r8) kabs                   
    real(r8) kabsi                  



    do k=1,pver
       do i=1,ncol
          kabsi = 0.005 + 1./rei(i,k)
          kabs = kabsl*(1.-fice(i,k)) + kabsi*fice(i,k)
          emis(i,k) = 1. - exp(-1.66*kabs*clwp(i,k))
       end do
    end do

    return
  end subroutine cldems


  subroutine cldovrlap(lchnk   ,ncol    ,pcols, pver, pverp, pint    ,cld     ,nmxrgn  ,pmxrgn  )

















    implicit none



    integer, intent(in) :: lchnk                
    integer, intent(in) :: ncol                 
    integer, intent(in) :: pcols, pver, pverp

    real(r8), intent(in) :: pint(pcols,pverp)   
    real(r8), intent(in) :: cld(pcols,pver)     



    real(r8), intent(out) :: pmxrgn(pcols,pverp)




    integer nmxrgn(pcols)                    



    integer i                    
    integer k                    
    integer n                    

    real(r8) pnm(pcols,pverp)    

    logical cld_found            
    logical cld_layer(pver)      




    do i = 1, ncol
       cld_found = .false.
       cld_layer(:) = cld(i,:) > 0.0_r8
       pmxrgn(i,:) = 0.0
       pnm(i,:)=pint(i,:)*10.
       n = 1
       do k = 1, pver
          if (cld_layer(k) .and.  .not. cld_found) then
             cld_found = .true.
          else if ( .not. cld_layer(k) .and. cld_found) then
             cld_found = .false.
             if (count(cld_layer(k:pver)) == 0) then
                exit
             endif
             pmxrgn(i,n) = pnm(i,k)
             n = n + 1
          endif
       end do
       pmxrgn(i,n) = pnm(i,pverp)
       nmxrgn(i) = n
    end do

    return
  end subroutine cldovrlap


  subroutine cldclw(lchnk   ,ncol    ,pcols, pver, pverp, zi      ,clwp    ,tpw     ,hl      )













    implicit none




    integer, intent(in) :: lchnk                 
    integer, intent(in) :: ncol                  
    integer, intent(in) :: pcols, pver, pverp

    real(r8), intent(in) :: zi(pcols,pverp)      
    real(r8), intent(in) :: tpw(pcols)           



    real(r8) clwp(pcols,pver)     
    real(r8) hl(pcols)            
    real(r8) rhl(pcols)           




    integer i,k               
    real(r8) clwc0                
    real(r8) emziohl(pcols,pverp) 





    clwc0 = 0.21



    do i=1,ncol
       hl(i)  = 700.0*log(max(tpw(i)+1.0_r8,1.0_r8))
       rhl(i) = 1.0/hl(i)
    end do



    do k=1,pverp
       do i=1,ncol
          emziohl(i,k) = exp(-zi(i,k)*rhl(i))
       end do
    end do
    do k=1,pver
       do i=1,ncol
          clwp(i,k) = clwc0*hl(i)*(emziohl(i,k+1) - emziohl(i,k))
       end do
    end do

    return
  end subroutine cldclw



  subroutine reltab(ncol, pcols, pver, t, landfrac, landm, icefrac, rel, snowh)












    implicit none




    integer, intent(in) :: ncol
    integer, intent(in) :: pcols, pver
    real(r8), intent(in) :: landfrac(pcols)      
    real(r8), intent(in) :: icefrac(pcols)       
    real(r8), intent(in) :: snowh(pcols)         
    real(r8), intent(in) :: landm(pcols)         
    real(r8), intent(in) :: t(pcols,pver)        




    real(r8), intent(out) :: rel(pcols,pver)      



    integer i,k               
    real(r8) rliqland         
    real(r8) rliqocean        
    real(r8) rliqice          



    rliqocean = 14.0_r8
    rliqice   = 14.0_r8
    rliqland  = 8.0_r8
    do k=1,pver
       do i=1,ncol

          
          
          rel(i,k) = rliqland + (rliqocean-rliqland) * min(1.0_r8,max(0.0_r8,(tmelt-t(i,k))*0.05))
          
          rel(i,k) = rel(i,k) + (rliqocean-rel(i,k)) * min(1.0_r8,max(0.0_r8,snowh(i)*10.))
          
          rel(i,k) = rel(i,k) + (rliqocean-rel(i,k)) * min(1.0_r8,max(0.0_r8,1.0-landm(i)))
          
          rel(i,k) = rel(i,k) + (rliqice-rel(i,k)) * min(1.0_r8,max(0.0_r8,icefrac(i)))

       end do
    end do
  end subroutine reltab

  subroutine reitab(ncol, pcols, pver, t, re)
    

    integer, intent(in) :: ncol, pcols, pver
    real(r8), intent(out) :: re(pcols,pver)
    real(r8), intent(in) :: t(pcols,pver)
    real(r8) corr
    integer i
    integer k
    integer index
    
    do k=1,pver
       do i=1,ncol
          index = int(t(i,k)-179.)
          index = min(max(index,1),94)
          corr = t(i,k) - int(t(i,k))
          re(i,k) = retab(index)*(1.-corr)		&
               +retab(index+1)*corr
          
       end do
    end do
    
    return
  end subroutine reitab
  
  function exp_interpol(x, f, y) result(g)

    
    
    
    
    
    

    



    implicit none

    real(r8), intent(in), dimension(:) :: x  
    real(r8), intent(in), dimension(:) :: f  
    real(r8), intent(in) :: y                
    real(r8) :: g                            

    integer :: k  
    integer :: n  
    real(r8) :: a

    n = size(x)

    
    

    if (y <= x(1)) then
      k = 1
    else if (y >= x(n)) then
      k = n - 1
    else
      k = 1
      do while (y > x(k+1) .and. k < n)
        k = k + 1
      end do
    end if

    
    a = (  log( f(k+1) / f(k) )  ) / ( x(k+1) - x(k) )
    g = f(k) * exp( a * (y - x(k)) )

  end function exp_interpol

  function lin_interpol(x, f, y) result(g)
    
    
    
    
    
    
    

    



    implicit none
    
    real(r8), intent(in), dimension(:) :: x  
    real(r8), intent(in), dimension(:) :: f  
    real(r8), intent(in) :: y                
    real(r8) :: g                            
    
    integer :: k  
    integer :: n  
    real(r8) :: a

    n = size(x)

    
    

    if (y <= x(1)) then 
      k = 1 
    else if (y >= x(n)) then
      k = n - 1
    else 
      k = 1 
      do while (y > x(k+1) .and. k < n)
        k = k + 1
      end do
    end if

    
    a = (  f(k+1) - f(k) ) / ( x(k+1) - x(k) )
    g = f(k) + a * (y - x(k))

  end function lin_interpol

  function lin_interpol2(x, f, y) result(g)

    
    
    
    
    
    

    

    implicit none

    real, intent(in), dimension(:) :: x  
    real, intent(in), dimension(:) :: f  
    real, intent(in) :: y                
    real :: g                            

    integer :: k  
    integer :: n  
    real    :: a

    n = size(x)

    
    

    if (y <= x(1)) then
      k = 1
    else if (y >= x(n)) then
      k = n - 1
    else
      k = 1
      do while (y > x(k+1) .and. k < n)
        k = k + 1
      end do
    end if

    
    a = (  f(k+1) - f(k) ) / ( x(k+1) - x(k) )
    g = f(k) + a * (y - x(k))

  end function lin_interpol2    


subroutine getfactors (cycflag, np1, cdayminus, cdayplus, cday, &
                       fact1, fact2)














   implicit none



   logical, intent(in) :: cycflag             

   integer, intent(in) :: np1                 

   real(r8), intent(in) :: cdayminus          
   real(r8), intent(in) :: cdayplus           
   real(r8), intent(in) :: cday               
   real(r8), intent(out) :: fact1             
   real(r8), intent(out) :: fact2             





   real(r8) :: deltat                         
   real(r8), parameter :: daysperyear = 365.  











   if (cycflag) then
      if ((cday < 1.) .or. (cday > (daysperyear+1.))) then
         write(6,*) 'GETFACTORS:', ' bad cday=',cday
         call endrun ()
      end if
   else
      if (cday < 1.) then
         write(6,*) 'GETFACTORS:',  ' bad cday=',cday
         call endrun ()
      end if
   end if




   if (cycflag .and. np1 == 1) then                     
      deltat = cdayplus + daysperyear - cdayminus
      if (cday > cdayplus) then                         
         fact1 = (cdayplus + daysperyear - cday)/deltat
         fact2 = (cday - cdayminus)/deltat
      else                                              
         fact1 = (cdayplus - cday)/deltat
         fact2 = (cday + daysperyear - cdayminus)/deltat
      end if
   else
      deltat = cdayplus - cdayminus
      fact1 = (cdayplus - cday)/deltat
      fact2 = (cday - cdayminus)/deltat
   end if

   if (.not. validfactors (fact1, fact2)) then
      write(6,*) 'GETFACTORS: ', ' bad fact1 and/or fact2=', fact1, fact2
      call endrun ()
   end if

   return
end subroutine getfactors

logical function validfactors (fact1, fact2)





   implicit none

   real(r8), intent(in) :: fact1, fact2           

   validfactors = .true.
   if (abs(fact1+fact2-1.) > 1.e-6 .or. &
       fact1 > 1.000001 .or. fact1 < -1.e-6 .or. &
       fact2 > 1.000001 .or. fact2 < -1.e-6) then

      validfactors = .false.
   end if

   return
end function validfactors

subroutine get_rf_scales(scales)

  real(r8), intent(out)::scales(naer_all)  

  integer i                                  

  scales(idxBG) = bgscl_rf
  scales(idxSUL) = sulscl_rf
  scales(idxSSLT) = ssltscl_rf

  do i = idxCARBONfirst, idxCARBONfirst+numCARBON-1
    scales(i) = carscl_rf
  enddo

  do i = idxDUSTfirst, idxDUSTfirst+numDUST-1
    scales(i) = dustscl_rf
  enddo

  scales(idxVOLC) = volcscl_rf

end subroutine get_rf_scales

function psi(tpx,iband)



























   real(r8),intent(in):: tpx      
   integer, intent(in):: iband    
   real(r8) psi                   
   real(r8),parameter ::  psi_r0(nbands) = (/ 5.65308452E-01, -7.30087891E+01/)
   real(r8),parameter ::  psi_r1(nbands) = (/ 4.07519005E-03,  1.22199547E+00/)
   real(r8),parameter ::  psi_r2(nbands) = (/-1.04347237E-05, -7.12256227E-03/)
   real(r8),parameter ::  psi_r3(nbands) = (/ 1.23765354E-08,  1.47852825E-05/)

   psi = (((psi_r3(iband) * tpx) + psi_r2(iband)) * tpx + psi_r1(iband)) * tpx + psi_r0(iband)
end function psi

function phi(tpx,iband)



























   real(r8),intent(in):: tpx      
   integer, intent(in):: iband    
   real(r8) phi                   
   real(r8),parameter ::  phi_r0(nbands) = (/ 9.60917711E-01, -2.21031342E+01/)
   real(r8),parameter ::  phi_r1(nbands) = (/ 4.86076751E-04,  4.24062610E-01/)
   real(r8),parameter ::  phi_r2(nbands) = (/-1.84806265E-06, -2.95543415E-03/)
   real(r8),parameter ::  phi_r3(nbands) = (/ 2.11239959E-09,  7.52470896E-06/)

   phi = (((phi_r3(iband) * tpx) + phi_r2(iband)) * tpx + phi_r1(iband)) &
          * tpx + phi_r0(iband)
end function phi

function fh2oself( temp )























   real(r8),intent(in) :: temp     
   real(r8) fh2oself               

   fh2oself = 2.0727484**((296.0 - temp) / 36.0)
end function fh2oself



subroutine esinti(epslon  ,latvap  ,latice  ,rh2o    ,cpair   ,tmelt   )














   implicit none




   real(r8), intent(in) :: epslon          
   real(r8), intent(in) :: latvap          
   real(r8), intent(in) :: latice          
   real(r8), intent(in) :: rh2o            
   real(r8), intent(in) :: cpair           
   real(r8), intent(in) :: tmelt           



   real(r8) tmn             
   real(r8) tmx             
   real(r8) trice           
   logical ip           





   tmn   = 173.16
   tmx   = 375.16
   trice =  20.00
   ip    = .true.



   call gestbl(tmn     ,tmx     ,trice   ,ip      ,epslon  , &
               latvap  ,latice  ,rh2o    ,cpair   ,tmelt )

   return
end subroutine esinti

subroutine gestbl(tmn     ,tmx     ,trice   ,ip      ,epsil   , &
                  latvap  ,latice  ,rh2o    ,cpair   ,tmeltx   )















   implicit none




   real(r8), intent(in) :: tmn           
   real(r8), intent(in) :: tmx           
   real(r8), intent(in) :: epsil         
   real(r8), intent(in) :: trice         
   real(r8), intent(in) :: latvap        
   real(r8), intent(in) :: latice        
   real(r8), intent(in) :: rh2o          
   real(r8), intent(in) :: cpair         
   real(r8), intent(in) :: tmeltx        



   real(r8) t             
   real(r8) rgasv 
   real(r8) cp
   real(r8) hlatf
   real(r8) ttrice
   real(r8) hlatv
   integer n          
   integer lentbl     
   integer itype      


   logical ip         
   logical icephs





   tmin   = tmn       
   tmax   = tmx       
   ttrice = trice     
   icephs = ip        



   epsqs  = epsil
   hlatv  = latvap
   hlatf  = latice
   rgasv  = rh2o
   cp     = cpair
   tmelt  = tmeltx

   lentbl = INT(tmax-tmin+2.000001)
   if (lentbl .gt. plenest) then
      write(6,9000) tmax, tmin, plenest
      call endrun ('GESTBL')    
   end if





   if (icephs) then
      if (ttrice /= 0.0) then
         itype = -ttrice
      else
         itype = 1
      end if
   else
      itype = 0
   end if

   t = tmin - 1.0
   do n=1,lentbl
      t = t + 1.0
      call gffgch(t,estbl(n),itype)
   end do

   do n=lentbl+1,plenest
      estbl(n) = -99999.0
   end do








   pcf(1) =  5.04469588506e-01
   pcf(2) = -5.47288442819e+00
   pcf(3) = -3.67471858735e-01
   pcf(4) = -8.95963532403e-03
   pcf(5) = -7.78053686625e-05










   if (masterproc) then
      write(6,*)' *** SATURATION VAPOR PRESSURE TABLE COMPLETED ***'
   end if

   return

9000 format('GESTBL: FATAL ERROR *********************************',/, &
            ' TMAX AND TMIN REQUIRE A LARGER DIMENSION ON THE LENGTH', &
            ' OF THE SATURATION VAPOR PRESSURE TABLE ESTBL(PLENEST)',/, &
            ' TMAX, TMIN, AND PLENEST => ', 2f7.2, i3)

end subroutine gestbl

subroutine gffgch(t       ,es      ,itype   )




























    
   implicit none




   real(r8), intent(in) :: t          



   integer, intent(inout) :: itype   

   real(r8), intent(out) :: es         



   real(r8) e1         
   real(r8) e2         
   real(r8) eswtr      
   real(r8) f          
   real(r8) f1         
   real(r8) f2         
   real(r8) f3         
   real(r8) f4         
   real(r8) f5         
   real(r8) ps         
   real(r8) t0         
   real(r8) term1      
   real(r8) term2      
   real(r8) term3      
   real(r8) tr         
   real(r8) ts         
   real(r8) weight     
   integer itypo   





   if (itype < 0) then
      tr    = abs(float(itype))
      itypo = itype
      itype = 1
   else
      tr    = 0.0
      itypo = itype
   end if
   if (tr > 40.0) then
      write(6,900) tr
      call endrun ('GFFGCH')                
   end if

   if(t < (tmelt - tr) .and. itype == 1) go to 10



   ps = 1013.246
   ts = 373.16
   e1 = 11.344*(1.0 - t/ts)
   e2 = -3.49149*(ts/t - 1.0)
   f1 = -7.90298*(ts/t - 1.0)
   f2 = 5.02808*log10(ts/t)
   f3 = -1.3816*(10.0**e1 - 1.0)/10000000.0
   f4 = 8.1328*(10.0**e2 - 1.0)/1000.0
   f5 = log10(ps)
   f  = f1 + f2 + f3 + f4 + f5
   es = (10.0**f)*100.0
   eswtr = es

   if(t >= tmelt .or. itype == 0) go to 20



10 continue
   t0    = tmelt
   term1 = 2.01889049/(t0/t)
   term2 = 3.56654*log(t0/t)
   term3 = 20.947031*(t0/t)
   es    = 575.185606e10*exp(-(term1 + term2 + term3))

   if (t < (tmelt - tr)) go to 20



   weight = min((tmelt - t)/tr,1.0_r8)
   es = weight*es + (1.0 - weight)*eswtr

20 continue
   itype = itypo
   return

900 format('GFFGCH: FATAL ERROR ******************************',/, &
           'TRANSITION RANGE FOR WATER TO ICE SATURATION VAPOR', &
           ' PRESSURE, TR, EXCEEDS MAXIMUM ALLOWABLE VALUE OF', &
           ' 40.0 DEGREES C',/, ' TR = ',f7.2)

end subroutine gffgch

   real(r8) function estblf( td )



   real(r8), intent(in) :: td         

   real(r8) :: e       
   real(r8) :: ai
   integer  :: i

   e = max(min(td,tmax),tmin)   
   i = int(e-tmin)+1
   ai = aint(e-tmin)
   estblf = (tmin+ai-e+1.)* &
            estbl(i)-(tmin+ai-e)* &
            estbl(i+1)
   end function estblf


function findvalue(ix,n,ain,indxa)














   implicit none



   integer, intent(in) :: ix                
   integer, intent(in) :: n                 
   integer, intent(inout):: indxa(n)        
   real(r8), intent(in) :: ain(n)           

   integer findvalue                        



   integer i,j
   integer il,im,ir

   integer ia
   integer itmp



   il=1
   ir=n
   do
      if (ir-il <= 1) then
         if (ir-il == 1) then
            if (ain(indxa(ir)) < ain(indxa(il))) then
               itmp=indxa(il)
               indxa(il)=indxa(ir)
               indxa(ir)=itmp
            endif
         endif
         findvalue=indxa(ix)
         return
      else
         im=(il+ir)/2
         itmp=indxa(im)
         indxa(im)=indxa(il+1)
         indxa(il+1)=itmp
         if (ain(indxa(il+1)) > ain(indxa(ir))) then
            itmp=indxa(il+1)
            indxa(il+1)=indxa(ir)
            indxa(ir)=itmp
         endif
         if (ain(indxa(il)) > ain(indxa(ir))) then
            itmp=indxa(il)
            indxa(il)=indxa(ir)
            indxa(ir)=itmp
         endif
         if (ain(indxa(il+1)) > ain(indxa(il))) then
            itmp=indxa(il+1)
            indxa(il+1)=indxa(il)
            indxa(il)=itmp
         endif
         i=il+1
         j=ir
         ia=indxa(il)
         do
            do
               i=i+1
               if (ain(indxa(i)) >= ain(ia)) exit
            end do
            do
               j=j-1
               if (ain(indxa(j)) <= ain(ia)) exit
            end do
            if (j < i) exit
            itmp=indxa(i)
            indxa(i)=indxa(j)
            indxa(j)=itmp
         end do
         indxa(il)=indxa(j)
         indxa(j)=ia
         if (j >= ix)ir=j-1
         if (j <= ix)il=i
      endif
   end do
end function findvalue


subroutine radini(gravx   ,cpairx  ,epsilox ,stebolx, pstdx )



















   implicit none





   real, intent(in) :: gravx      
   real, intent(in) :: cpairx     
   real, intent(in) :: epsilox    
   real, intent(in) :: stebolx    
   real(r8), intent(in) :: pstdx      



   integer k       

   real(r8) v0         
   real(r8) p0         
   real(r8) amd        
   real(r8) goz        





   gravit  =  100.*gravx
   rga     =  1./gravit
   gravmks =  gravx
   cpair   =  1.e4*cpairx
   epsilo  =  epsilox
   sslp    =  1.013250e6
   stebol  =  1.e3*stebolx
   rgsslp  =  0.5/(gravit*sslp)
   dpfo3   =  2.5e-3
   dpfco2  =  5.0e-3
   dayspy  =  365.
   pie     =  4.*atan(1.)



   v0  = 22.4136         
   p0  = 0.1*sslp        
   amd = 28.9644         
   goz = gravx           




   cplos = v0/(amd*goz)       *100.0
   cplol = v0/(amd*goz*p0)*0.5*100.0










      ntoplw = 1





   call radaeini( pstdx, mwdry, mwco2 )
   return
end subroutine radini

subroutine oznini(ozmixm,pin,levsiz,num_months,XLAT,                &
                     ids, ide, jds, jde, kds, kde,                  &
                     ims, ime, jms, jme, kms, kme,                  &
                     its, ite, jts, jte, kts, kte)





      IMPLICIT NONE

   INTEGER,      INTENT(IN   )    ::   ids,ide, jds,jde, kds,kde, &
                                       ims,ime, jms,jme, kms,kme, &
                                       its,ite, jts,jte, kts,kte   

   INTEGER,      INTENT(IN   )    ::   levsiz, num_months

   REAL,  DIMENSION( ims:ime, jms:jme ), INTENT(IN   )  ::     XLAT

   REAL,  DIMENSION( ims:ime, levsiz, jms:jme, num_months ),      &
          INTENT(OUT   ) ::                                  OZMIXM

   REAL,  DIMENSION(levsiz), INTENT(OUT )  ::                   PIN


   INTEGER, PARAMETER :: latsiz = 64
   INTEGER, PARAMETER :: lonsiz = 1
   INTEGER :: i, j, k, itf, jtf, ktf, m, pin_unit, lat_unit, oz_unit
   REAL    :: interp_pt
   CHARACTER*256 :: message

   REAL,  DIMENSION( lonsiz, levsiz, latsiz, num_months )    ::   &
                                                            OZMIXIN

   REAL,  DIMENSION(latsiz)                ::             lat_ozone

   jtf=min0(jte,jde-1)
   ktf=min0(kte,kde-1)
   itf=min0(ite,ide-1)




     WRITE(message,*)'num_months = ',num_months
     CALL wrf_debug(50,message)

      pin_unit = 27
        OPEN(pin_unit, FILE='ozone_plev.formatted',FORM='FORMATTED',STATUS='OLD')
        do k = 1,levsiz
        READ (pin_unit,*)pin(k)
        end do
      close(27)

      do k=1,levsiz
        pin(k) = pin(k)*100.
      end do



      lat_unit = 28
        OPEN(lat_unit, FILE='ozone_lat.formatted',FORM='FORMATTED',STATUS='OLD')
        do j = 1,latsiz
        READ (lat_unit,*)lat_ozone(j)
        end do
      close(28)




      oz_unit = 29
      OPEN(oz_unit, FILE='ozone.formatted',FORM='FORMATTED',STATUS='OLD')

      do m=2,num_months
      do j=1,latsiz 
      do k=1,levsiz 
      do i=1,lonsiz 
        READ (oz_unit,*)ozmixin(i,k,j,m)
      enddo
      enddo
      enddo
      enddo
      close(29)
















      do m=2,num_months
      do j=jts,jtf
      do k=1,levsiz
      do i=its,itf
         interp_pt=XLAT(i,j)
         ozmixm(i,k,j,m)=lin_interpol2(lat_ozone(:),ozmixin(1,k,:,m),interp_pt)
      enddo
      enddo
      enddo
      enddo

























END SUBROUTINE oznini


subroutine aerosol_init(m_psp,m_psn,m_hybi,aerosolcp,aerosolcn,paerlev,naer_c,shalf,pptop,    &
                     ids, ide, jds, jde, kds, kde,                  &
                     ims, ime, jms, jme, kms, kme,                  &
                     its, ite, jts, jte, kts, kte)




      IMPLICIT NONE

   INTEGER,      INTENT(IN   )    ::   ids,ide, jds,jde, kds,kde, &
                                       ims,ime, jms,jme, kms,kme, &
                                       its,ite, jts,jte, kts,kte

   INTEGER,      INTENT(IN   )    ::   paerlev,naer_c 

   REAL,     intent(in)                        :: pptop
   REAL,     DIMENSION( kms:kme ), intent(in)  :: shalf

   REAL,  DIMENSION( ims:ime, paerlev, jms:jme, naer_c ),      &
          INTENT(INOUT   ) ::                                  aerosolcn , aerosolcp

   REAL,  DIMENSION(paerlev), INTENT(OUT )  ::                m_hybi
   REAL,  DIMENSION( ims:ime, jms:jme),  INTENT(OUT )  ::       m_psp,m_psn 

   REAL ::                                                      psurf
   real, dimension(29) :: hybi  
   integer k 

   INTEGER :: i, j, itf, jtf, ktf,m

   data hybi/0, 0.0065700002014637, 0.0138600002974272, 0.023089999333024, &
    0.0346900001168251, 0.0491999983787537, 0.0672300010919571,      &
     0.0894500017166138, 0.116539999842644, 0.149159997701645,       &
    0.187830001115799, 0.232859998941422, 0.284209996461868,         &
    0.341369986534119, 0.403340011835098, 0.468600004911423,         &
    0.535290002822876, 0.601350009441376, 0.66482001543045,          &
    0.724009990692139, 0.777729988098145, 0.825269997119904,         & 
    0.866419970989227, 0.901350021362305, 0.930540025234222,         & 
    0.954590022563934, 0.974179983139038, 0.990000009536743, 1/

   jtf=min0(jte,jde-1)
   ktf=min0(kte,kde-1)
   itf=min0(ite,ide-1)

    do k=1,paerlev
      m_hybi(k)=hybi(k)
    enddo






   psurf = 1.e05
   mxaerl = 0

   do k=kms,kme-1

      if (shalf(k)*psurf+pptop  >= 9.e4) mxaerl = mxaerl + 1
   end do
   mxaerl = max(mxaerl,1)

      write(6,*)'AEROSOLS:  Background aerosol will be limited to ', &
                'bottom ',mxaerl,' model interfaces.'




     DO j=jts,jtf
     DO i=its,itf
      m_psp(i,j)=psurf
      m_psn(i,j)=psurf
     ENDDO
     ENDDO

     DO j=jts,jtf
     DO i=its,itf
     DO k=1,paerlev


      aerosolcp(i,k,j,idxSUL)=1.e-7*(1.-hybi(k))
      aerosolcn(i,k,j,idxSUL)=1.e-7*(1.-hybi(k))
      aerosolcp(i,k,j,idxSSLT)=1.e-22*(1.-hybi(k))
      aerosolcn(i,k,j,idxSSLT)=1.e-22*(1.-hybi(k))
      aerosolcp(i,k,j,idxDUSTfirst)=1.e-7*(1.-hybi(k))
      aerosolcn(i,k,j,idxDUSTfirst)=1.e-7*(1.-hybi(k))
      aerosolcp(i,k,j,idxDUSTfirst+1)=1.e-7*(1.-hybi(k))
      aerosolcn(i,k,j,idxDUSTfirst+1)=1.e-7*(1.-hybi(k))
      aerosolcp(i,k,j,idxDUSTfirst+2)=1.e-7*(1.-hybi(k))
      aerosolcn(i,k,j,idxDUSTfirst+2)=1.e-7*(1.-hybi(k))
      aerosolcp(i,k,j,idxDUSTfirst+3)=1.e-7*(1.-hybi(k))
      aerosolcn(i,k,j,idxDUSTfirst+3)=1.e-7*(1.-hybi(k))
      aerosolcp(i,k,j,idxOCPHO)=1.e-7*(1.-hybi(k))
      aerosolcn(i,k,j,idxOCPHO)=1.e-7*(1.-hybi(k))
      aerosolcp(i,k,j,idxBCPHO)=1.e-9*(1.-hybi(k))
      aerosolcn(i,k,j,idxBCPHO)=1.e-9*(1.-hybi(k))
      aerosolcp(i,k,j,idxOCPHI)=1.e-7*(1.-hybi(k))
      aerosolcn(i,k,j,idxOCPHI)=1.e-7*(1.-hybi(k))
      aerosolcp(i,k,j,idxBCPHI)=1.e-8*(1.-hybi(k))
      aerosolcn(i,k,j,idxBCPHI)=1.e-8*(1.-hybi(k))
     ENDDO
     ENDDO
     ENDDO

     call aer_optics_initialize
 

END subroutine aerosol_init

  subroutine aer_optics_initialize

USE module_wrf_error








    implicit none




    integer :: nrh_opac  
    integer :: nbnd      
    real(r8), parameter :: wgt_sscm = 6.0 / 7.0
    integer :: krh_opac  
    integer :: krh       
    integer :: ksz       
    integer :: kbnd      

    real(r8) :: rh   

    integer, parameter :: irh=8
    real(r8) :: rh_opac(irh)        
    real(r8) :: ksul_opac(irh,nspint)    
    real(r8) :: wsul_opac(irh,nspint)    
    real(r8) :: gsul_opac(irh,nspint)    
    real(r8) :: ksslt_opac(irh,nspint)   
    real(r8) :: wsslt_opac(irh,nspint)
    real(r8) :: gsslt_opac(irh,nspint)
    real(r8) :: kssam_opac(irh,nspint)   
    real(r8) :: wssam_opac(irh,nspint)
    real(r8) :: gssam_opac(irh,nspint)
    real(r8) :: ksscm_opac(irh,nspint)   
    real(r8) :: wsscm_opac(irh,nspint)
    real(r8) :: gsscm_opac(irh,nspint)
    real(r8) :: kcphil_opac(irh,nspint)  
    real(r8) :: wcphil_opac(irh,nspint)
    real(r8) :: gcphil_opac(irh,nspint)
    real(r8) :: dummy(nspint)

      LOGICAL                 :: opened
      LOGICAL , EXTERNAL      :: wrf_dm_on_monitor

      CHARACTER*80 errmess
      INTEGER cam_aer_unit
      integer :: i



      IF ( wrf_dm_on_monitor() ) THEN
        DO i = 10,99
          INQUIRE ( i , OPENED = opened )
          IF ( .NOT. opened ) THEN
            cam_aer_unit = i
            GOTO 2010
          ENDIF
        ENDDO
        cam_aer_unit = -1
 2010   CONTINUE
      ENDIF
      CALL wrf_dm_bcast_bytes ( cam_aer_unit , 4 )
      IF ( cam_aer_unit < 0 ) THEN
        CALL wrf_error_fatal3("<stdin>",3579,&
'module_ra_cam: aer_optics_initialize: Can not find unused fortran unit to read in lookup table.' )
      ENDIF

        IF ( wrf_dm_on_monitor() ) THEN
          OPEN(cam_aer_unit,FILE='CAM_AEROPT_DATA',                  &
               FORM='UNFORMATTED',STATUS='OLD',ERR=9010)
          call wrf_debug(50,'reading CAM_AEROPT_DATA')
        ENDIF


         IF ( wrf_dm_on_monitor() ) then
         READ (cam_aer_unit,ERR=9010) dummy
         READ (cam_aer_unit,ERR=9010) rh_opac 
         READ (cam_aer_unit,ERR=9010) ksul_opac 
         READ (cam_aer_unit,ERR=9010) wsul_opac 
         READ (cam_aer_unit,ERR=9010) gsul_opac 
         READ (cam_aer_unit,ERR=9010) kssam_opac 
         READ (cam_aer_unit,ERR=9010) wssam_opac 
         READ (cam_aer_unit,ERR=9010) gssam_opac 
         READ (cam_aer_unit,ERR=9010) ksscm_opac 
         READ (cam_aer_unit,ERR=9010) wsscm_opac 
         READ (cam_aer_unit,ERR=9010) gsscm_opac
         READ (cam_aer_unit,ERR=9010) kcphil_opac 
         READ (cam_aer_unit,ERR=9010) wcphil_opac 
         READ (cam_aer_unit,ERR=9010) gcphil_opac 
         READ (cam_aer_unit,ERR=9010) kcb 
         READ (cam_aer_unit,ERR=9010) wcb 
         READ (cam_aer_unit,ERR=9010) gcb 
         READ (cam_aer_unit,ERR=9010) kdst 
         READ (cam_aer_unit,ERR=9010) wdst 
         READ (cam_aer_unit,ERR=9010) gdst 
         READ (cam_aer_unit,ERR=9010) kbg 
         READ (cam_aer_unit,ERR=9010) wbg 
         READ (cam_aer_unit,ERR=9010) gbg
         READ (cam_aer_unit,ERR=9010) kvolc 
         READ (cam_aer_unit,ERR=9010) wvolc 
         READ (cam_aer_unit,ERR=9010) gvolc
         endif

         CALL wrf_dm_bcast_bytes ( rh_opac , size ( rh_opac ) * r8 )
         CALL wrf_dm_bcast_bytes ( ksul_opac , size ( ksul_opac ) * r8 )
         CALL wrf_dm_bcast_bytes ( wsul_opac , size ( wsul_opac ) * r8 )
         CALL wrf_dm_bcast_bytes ( gsul_opac , size ( gsul_opac ) * r8 )
         CALL wrf_dm_bcast_bytes ( kssam_opac , size ( kssam_opac ) * r8 )
         CALL wrf_dm_bcast_bytes ( wssam_opac , size ( wssam_opac ) * r8 )
         CALL wrf_dm_bcast_bytes ( gssam_opac , size ( gssam_opac ) * r8 )
         CALL wrf_dm_bcast_bytes ( ksscm_opac , size ( ksscm_opac ) * r8 )
         CALL wrf_dm_bcast_bytes ( wsscm_opac , size ( wsscm_opac ) * r8 )
         CALL wrf_dm_bcast_bytes ( gsscm_opac , size ( gsscm_opac ) * r8 )
         CALL wrf_dm_bcast_bytes ( kcphil_opac , size ( kcphil_opac ) * r8 )
         CALL wrf_dm_bcast_bytes ( wcphil_opac , size ( wcphil_opac ) * r8 )
         CALL wrf_dm_bcast_bytes ( gcphil_opac , size ( gcphil_opac ) * r8 )
         CALL wrf_dm_bcast_bytes ( kcb , size ( kcb ) * r8 )
         CALL wrf_dm_bcast_bytes ( wcb , size ( wcb ) * r8 )
         CALL wrf_dm_bcast_bytes ( gcb , size ( gcb ) * r8 )
         CALL wrf_dm_bcast_bytes ( kvolc , size ( kvolc ) * r8 )
         CALL wrf_dm_bcast_bytes ( wvolc , size ( wvolc ) * r8 )
         CALL wrf_dm_bcast_bytes ( gvolc , size ( gvolc ) * r8 )
         CALL wrf_dm_bcast_bytes ( kdst , size ( kdst ) * r8 )
         CALL wrf_dm_bcast_bytes ( wdst , size ( wdst ) * r8 )
         CALL wrf_dm_bcast_bytes ( gdst , size ( gdst ) * r8 )
         CALL wrf_dm_bcast_bytes ( kbg , size ( kbg ) * r8 )
         CALL wrf_dm_bcast_bytes ( wbg , size ( wbg ) * r8 )
         CALL wrf_dm_bcast_bytes ( gbg , size ( gbg ) * r8 )

         IF ( wrf_dm_on_monitor() ) CLOSE (cam_aer_unit)

    
    
    
    
    
    
    

    ksslt_opac(:,:) = (1.0 - wgt_sscm) * kssam_opac(:,:) + wgt_sscm * ksscm_opac(:,:)

    wsslt_opac(:,:) = ( (1.0 - wgt_sscm) * kssam_opac(:,:) * wssam_opac(:,:) &
                  + wgt_sscm * ksscm_opac(:,:) * wsscm_opac(:,:) ) &
                  / ksslt_opac(:,:)

    gsslt_opac(:,:) = ( (1.0 - wgt_sscm) * kssam_opac(:,:) * wssam_opac(:,:) * gssam_opac(:,:) &
                  + wgt_sscm * ksscm_opac(:,:) * wsscm_opac(:,:) * gsscm_opac(:,:) ) &
                   / ( ksslt_opac(:,:) * wsslt_opac(:,:) )

    do i=1,nspint
    kcphob(i) = kcphil_opac(1,i)
    wcphob(i) = wcphil_opac(1,i)
    gcphob(i) = gcphil_opac(1,i)
    end do

    
    

    nbnd = nspint

    do krh = 1, nrh
      rh = 1.0_r8 / nrh * (krh - 1)
      do kbnd = 1, nbnd
        ksul(krh, kbnd) = exp_interpol( rh_opac, &
          ksul_opac(:, kbnd) / ksul_opac(1, kbnd), rh ) * ksul_opac(1, kbnd)
        wsul(krh, kbnd) = lin_interpol( rh_opac, &
          wsul_opac(:, kbnd) / wsul_opac(1, kbnd), rh ) * wsul_opac(1, kbnd)
        gsul(krh, kbnd) = lin_interpol( rh_opac, &
          gsul_opac(:, kbnd) / gsul_opac(1, kbnd), rh ) * gsul_opac(1, kbnd)
        ksslt(krh, kbnd) = exp_interpol( rh_opac, &
          ksslt_opac(:, kbnd) / ksslt_opac(1, kbnd), rh ) * ksslt_opac(1, kbnd)
        wsslt(krh, kbnd) = lin_interpol( rh_opac, &
          wsslt_opac(:, kbnd) / wsslt_opac(1, kbnd), rh ) * wsslt_opac(1, kbnd)
        gsslt(krh, kbnd) = lin_interpol( rh_opac, &
          gsslt_opac(:, kbnd) / gsslt_opac(1, kbnd), rh ) * gsslt_opac(1, kbnd)
        kcphil(krh, kbnd) = exp_interpol( rh_opac, &
          kcphil_opac(:, kbnd) / kcphil_opac(1, kbnd), rh ) * kcphil_opac(1, kbnd)
        wcphil(krh, kbnd) = lin_interpol( rh_opac, &
          wcphil_opac(:, kbnd) / wcphil_opac(1, kbnd), rh ) * wcphil_opac(1, kbnd)
        gcphil(krh, kbnd) = lin_interpol( rh_opac, &
          gcphil_opac(:, kbnd) / gcphil_opac(1, kbnd), rh )  * gcphil_opac(1, kbnd)
      end do
    end do

     RETURN
9010 CONTINUE
     WRITE( errmess , '(A35,I4)' ) 'module_ra_cam: error reading unit ',cam_aer_unit
     CALL wrf_error_fatal3("<stdin>",3703,&
errmess)

END subroutine aer_optics_initialize


subroutine radaeini( pstdx, mwdryx, mwco2x )

USE module_wrf_error







   real(r8), intent(in) :: pstdx   
   real(r8), intent(in) :: mwdryx  
   real(r8), intent(in) :: mwco2x  



   integer ncid_ae                

   integer pdimid                 
   integer psize                  

   integer tpdimid                
   integer tpsize                 

   integer tedimid                
   integer tesize                 

   integer udimid                 
   integer usize                  

   integer rhdimid                
   integer rhsize                 

   integer    ah2onwid            
   integer    eh2onwid            
   integer    ah2owid             
   integer cn_ah2owid             
   integer cn_eh2owid             
   integer ln_ah2owid             
   integer ln_eh2owid             
   

   character(len=256) locfn       
   integer tmptype                
   integer ndims                  

   integer natt                   



   integer t                     
   integer tmin                  
   integer tmax                  
   integer itype                 
   integer i
   real(r8) tdbl

      LOGICAL                 :: opened
      LOGICAL , EXTERNAL      :: wrf_dm_on_monitor

      CHARACTER*80 errmess
      INTEGER cam_abs_unit




   p0     = pstdx
   amd    = mwdryx
   amco2  = mwco2x




   c16  = coefj(3,1)/coefj(2,1)
   c17  = coefk(3,1)/coefk(2,1)
   c26  = coefj(3,2)/coefj(2,2)
   c27  = coefk(3,2)/coefk(2,2)
   c28  = .5
   c29  = .002053
   c30  = .1
   c31  = 3.0e-5








   fwcoef = .1           
   fwc1   = .30          
   fwc2   = 4.5          
   fc1    = 2.6          

      IF ( wrf_dm_on_monitor() ) THEN
        DO i = 10,99
          INQUIRE ( i , OPENED = opened )
          IF ( .NOT. opened ) THEN
            cam_abs_unit = i
            GOTO 2010
          ENDIF
        ENDDO
        cam_abs_unit = -1
 2010   CONTINUE
      ENDIF
      CALL wrf_dm_bcast_bytes ( cam_abs_unit , 4 )
      IF ( cam_abs_unit < 0 ) THEN
        CALL wrf_error_fatal3("<stdin>",3816,&
'module_ra_cam: radaeinit: Can not find unused fortran unit to read in lookup table.' )
      ENDIF

        IF ( wrf_dm_on_monitor() ) THEN
          OPEN(cam_abs_unit,FILE='CAM_ABS_DATA',                  &
               FORM='UNFORMATTED',STATUS='OLD',ERR=9010)
          call wrf_debug(50,'reading CAM_ABS_DATA')
        ENDIF


         IF ( wrf_dm_on_monitor() ) then
         READ (cam_abs_unit,ERR=9010) ah2onw
         READ (cam_abs_unit,ERR=9010) eh2onw 
         READ (cam_abs_unit,ERR=9010) ah2ow 
         READ (cam_abs_unit,ERR=9010) cn_ah2ow 
         READ (cam_abs_unit,ERR=9010) cn_eh2ow 
         READ (cam_abs_unit,ERR=9010) ln_ah2ow 
         READ (cam_abs_unit,ERR=9010) ln_eh2ow 

         endif

         CALL wrf_dm_bcast_bytes ( ah2onw , size ( ah2onw ) * r8 )
         CALL wrf_dm_bcast_bytes ( eh2onw , size ( eh2onw ) * r8 )
         CALL wrf_dm_bcast_bytes ( ah2ow , size ( ah2ow ) * r8 )
         CALL wrf_dm_bcast_bytes ( cn_ah2ow , size ( cn_ah2ow ) * r8 )
         CALL wrf_dm_bcast_bytes ( cn_eh2ow , size ( cn_eh2ow ) * r8 )
         CALL wrf_dm_bcast_bytes ( ln_ah2ow , size ( ln_ah2ow ) * r8 )
         CALL wrf_dm_bcast_bytes ( ln_eh2ow , size ( ln_eh2ow ) * r8 )

         IF ( wrf_dm_on_monitor() ) CLOSE (cam_abs_unit)
      






      tmin = nint(min_tp_h2o)
      tmax = nint(max_tp_h2o)+1
      itype = 0
      do t = tmin, tmax

         tdbl = t
         call gffgch(tdbl,estblh2o(t-tmin),itype)
      end do

     RETURN
9010 CONTINUE
     WRITE( errmess , '(A35,I4)' ) 'module_ra_cam: error reading unit ',cam_abs_unit
     CALL wrf_error_fatal3("<stdin>",3866,&
errmess)
end subroutine radaeini

 
end MODULE module_ra_cam_support
