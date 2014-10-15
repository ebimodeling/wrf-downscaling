








MODULE shr_const_mod

   use shr_kind_mod

   integer(SHR_KIND_IN),parameter,private :: R8 = SHR_KIND_R8 

   
   
   
   public

   real(R8),parameter :: SHR_CONST_PI      = 3.14159265358979323846_R8  
   real(R8),parameter :: SHR_CONST_CDAY    = 86400.0_R8      
   real(R8),parameter :: SHR_CONST_SDAY    = 86164.0_R8      
   real(R8),parameter :: SHR_CONST_OMEGA   = 2.0_R8*SHR_CONST_PI/SHR_CONST_SDAY 
   real(R8),parameter :: SHR_CONST_REARTH  = 6.37122e6_R8    
   real(R8),parameter :: SHR_CONST_G       = 9.80616_R8      

   real(R8),parameter :: SHR_CONST_STEBOL  = 5.67e-8_R8      
   real(R8),parameter :: SHR_CONST_BOLTZ   = 1.38065e-23_R8  
   real(R8),parameter :: SHR_CONST_AVOGAD  = 6.02214e26_R8   
   real(R8),parameter :: SHR_CONST_RGAS    = SHR_CONST_AVOGAD*SHR_CONST_BOLTZ       
   real(R8),parameter :: SHR_CONST_MWDAIR  = 28.966_R8       
   real(R8),parameter :: SHR_CONST_MWWV    = 18.016_R8       
   real(R8),parameter :: SHR_CONST_RDAIR   = SHR_CONST_RGAS/SHR_CONST_MWDAIR        
   real(R8),parameter :: SHR_CONST_RWV     = SHR_CONST_RGAS/SHR_CONST_MWWV          
   real(R8),parameter :: SHR_CONST_ZVIR    = (SHR_CONST_RWV/SHR_CONST_RDAIR)-1.0_R8 
   real(R8),parameter :: SHR_CONST_KARMAN  = 0.4_R8          
   real(R8),parameter :: SHR_CONST_PSTD    = 101325.0_R8     
   real(R8),parameter :: SHR_CONST_PDB     = 0.0112372_R8    
 
   real(R8),parameter :: SHR_CONST_TKTRIP  = 273.16_R8       
   real(R8),parameter :: SHR_CONST_TKFRZ   = 273.15_R8       
   real(R8),parameter :: SHR_CONST_TKFRZSW = SHR_CONST_TKFRZ - 1.8_R8 

   real(R8),parameter :: SHR_CONST_RHODAIR = &               
                         SHR_CONST_PSTD/(SHR_CONST_RDAIR*SHR_CONST_TKFRZ)
   real(R8),parameter :: SHR_CONST_RHOFW   = 1.000e3_R8      
   real(R8),parameter :: SHR_CONST_RHOSW   = 1.026e3_R8      
   real(R8),parameter :: SHR_CONST_RHOICE  = 0.917e3_R8      
   real(R8),parameter :: SHR_CONST_CPDAIR  = 1.00464e3_R8    
   real(R8),parameter :: SHR_CONST_CPWV    = 1.810e3_R8      
   real(R8),parameter :: SHR_CONST_CPVIR   = (SHR_CONST_CPWV/SHR_CONST_CPDAIR)-1.0_R8 
   real(R8),parameter :: SHR_CONST_CPFW    = 4.188e3_R8      
   real(R8),parameter :: SHR_CONST_CPSW    = 3.996e3_R8      
   real(R8),parameter :: SHR_CONST_CPICE   = 2.11727e3_R8    
   real(R8),parameter :: SHR_CONST_LATICE  = 3.337e5_R8      
   real(R8),parameter :: SHR_CONST_LATVAP  = 2.501e6_R8      
   real(R8),parameter :: SHR_CONST_LATSUB  = &               
                         SHR_CONST_LATICE + SHR_CONST_LATVAP
   real(R8),parameter :: SHR_CONST_OCN_REF_SAL = 34.7_R8     
   real(R8),parameter :: SHR_CONST_ICE_REF_SAL =  4.0_R8     

   real(R8),parameter :: SHR_CONST_SPVAL   = 1.0e30_R8       

END MODULE shr_const_mod
