












MODULE shr_kind_mod

   
   
   
   public
   integer,parameter :: SHR_KIND_R8 = selected_real_kind(12) 
   integer,parameter :: SHR_KIND_R4 = selected_real_kind( 6) 
   integer,parameter :: SHR_KIND_RN = kind(1.0)              
   integer,parameter :: SHR_KIND_I8 = selected_int_kind (13) 
   integer,parameter :: SHR_KIND_I4 = selected_int_kind ( 6) 
   integer,parameter :: SHR_KIND_IN = kind(1)                
   integer,parameter :: SHR_KIND_CS = 80                     
   integer,parameter :: SHR_KIND_CL = 256                    
   integer,parameter :: SHR_KIND_CX = 512                    

END MODULE shr_kind_mod
