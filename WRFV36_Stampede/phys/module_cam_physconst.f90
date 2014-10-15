







module physconst

   

   use shr_kind_mod, only: r8 => shr_kind_r8
   use shr_const_mod, only: shr_const_g,      shr_const_stebol, shr_const_tkfrz,  &
                            shr_const_mwdair, shr_const_rdair,  shr_const_mwwv,   &
                            shr_const_latice, shr_const_latvap, shr_const_cpdair, &
                            shr_const_rhofw,  shr_const_cpwv,   shr_const_rgas,   &
                            shr_const_karman, shr_const_pstd,   shr_const_rhodair,&
                            shr_const_avogad, shr_const_boltz,  shr_const_cpfw,   &
                            shr_const_rwv,    shr_const_zvir,   shr_const_pi,     &
                            shr_const_rearth, shr_const_sday,   shr_const_cday,   &
                            shr_const_spval         
   implicit none
   private
   public  :: physconst_readnl
   save
   
   real(r8), public, parameter :: avogad      = shr_const_avogad     
   real(r8), public, parameter :: boltz       = shr_const_boltz      
   real(r8), public, parameter :: cday        = shr_const_cday       
   real(r8), public, parameter :: cpair       = shr_const_cpdair     
   real(r8), public, parameter :: cpliq       = shr_const_cpfw       
   real(r8), public, parameter :: karman      = shr_const_karman     
   real(r8), public, parameter :: latice      = shr_const_latice     
   real(r8), public, parameter :: latvap      = shr_const_latvap     
   real(r8), public, parameter :: pi          = shr_const_pi         
   real(r8), public, parameter :: pstd        = shr_const_pstd       
   real(r8), public, parameter :: r_universal = shr_const_rgas       
   real(r8), public, parameter :: rhoh2o      = shr_const_rhofw      
   real(r8), public, parameter :: spval       = shr_const_spval      
   real(r8), public, parameter :: stebol      = shr_const_stebol     

   real(r8), public, parameter :: c0          = 2.99792458e8_r8      
   real(r8), public, parameter :: planck      = 6.6260755e-34_r8     

   
   real(r8), public, parameter :: mwco2       =  44._r8             
   real(r8), public, parameter :: mwn2o       =  44._r8             
   real(r8), public, parameter :: mwch4       =  16._r8             
   real(r8), public, parameter :: mwf11       = 136._r8             
   real(r8), public, parameter :: mwf12       = 120._r8             
   real(r8), public, parameter :: mwo3        =  48._r8             
   real(r8), public, parameter :: mwso2       =  64._r8
   real(r8), public, parameter :: mwso4       =  96._r8
   real(r8), public, parameter :: mwh2o2      =  34._r8
   real(r8), public, parameter :: mwdms       =  62._r8


   

   real(r8), public           :: gravit       = shr_const_g     
   real(r8), public           :: sday         = shr_const_sday  
   real(r8), public           :: mwh2o        = shr_const_mwwv  
   real(r8), public           :: cpwv         = shr_const_cpwv  
   real(r8), public           :: mwdry        = shr_const_mwdair
   real(r8), public           :: rearth       = shr_const_rearth
   real(r8), public           :: tmelt        = shr_const_tkfrz 



   real(r8), public           :: rga          = 1._r8/shr_const_g                 
   real(r8), public           :: ra           = 1._r8/shr_const_rearth            
   real(r8), public           :: omega        = 2.0_R8*shr_const_pi/shr_const_sday
   real(r8), public           :: rh2o         = shr_const_rgas/shr_const_mwwv     
   real(r8), public           :: rair         = shr_const_rdair   
   real(r8), public           :: epsilo       = shr_const_mwwv/shr_const_mwdair   
   real(r8), public           :: zvir         = (shr_const_rwv/shr_const_rdair)-1.0_R8 
   real(r8), public           :: cpvir        = (shr_const_cpwv/shr_const_cpdair)-1.0_R8 
   real(r8), public           :: rhodair      = shr_const_pstd/(shr_const_rdair*shr_const_tkfrz)
   real(r8), public           :: cappa        = (shr_const_rgas/shr_const_mwdair)/shr_const_cpdair  
   real(r8), public           :: ez           
   real(r8), public           :: Cpd_on_Cpv   = shr_const_cpdair/shr_const_cpwv
                         

contains


   
   subroutine physconst_readnl(nlfile)
      character(len=*), intent(in) :: nlfile  
    end subroutine physconst_readnl
  end module physconst
