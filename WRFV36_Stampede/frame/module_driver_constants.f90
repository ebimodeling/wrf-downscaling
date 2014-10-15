




MODULE module_driver_constants

   
   

   INTEGER , PARAMETER :: DATA_ORDER_XYZ = 1
   INTEGER , PARAMETER :: DATA_ORDER_YXZ = 2
   INTEGER , PARAMETER :: DATA_ORDER_ZXY = 3
   INTEGER , PARAMETER :: DATA_ORDER_ZYX = 4
   INTEGER , PARAMETER :: DATA_ORDER_XZY = 5
   INTEGER , PARAMETER :: DATA_ORDER_YZX = 6
   INTEGER , PARAMETER :: DATA_ORDER_XY = DATA_ORDER_XYZ
   INTEGER , PARAMETER :: DATA_ORDER_YX = DATA_ORDER_YXZ








INTEGER , PARAMETER :: model_data_order   = DATA_ORDER_XZY


   
   
   

   
   

   INTEGER , PARAMETER :: max_levels      =  20

   

   INTEGER , PARAMETER :: max_nests        =  20

   

   INTEGER , PARAMETER :: max_parents      =  1

   

   INTEGER , PARAMETER :: max_domains     =   ( 21 - 1 ) / 2 + 1

   

   INTEGER , PARAMETER :: max_moves       =   50

   

   INTEGER , PARAMETER :: max_eta         =   501

   

   INTEGER , PARAMETER :: max_ocean       =   501

   

   INTEGER , PARAMETER :: max_plevs       =   100

   

   INTEGER , PARAMETER :: max_trackchem   =   100

   

   INTEGER , PARAMETER :: max_outer_iterations = 10

   

   INTEGER , PARAMETER :: max_instruments =   30

   

   INTEGER , PARAMETER :: num_ob_indexes  =   28


   

   INTEGER , PARAMETER :: max_bogus =  5

   

   INTEGER , PARAMETER :: max_cplfld = 20

   

   INTEGER , PARAMETER :: max_extdomains = 5

   

   INTEGER , PARAMETER :: max_comms       =   1024

   

   
   

   INTEGER , PARAMETER :: min_file_unit = 10
   INTEGER , PARAMETER :: max_file_unit = 99

   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   




   
   REAL , PARAMETER :: P2SI = 1.0

 CONTAINS
   SUBROUTINE init_module_driver_constants
   END SUBROUTINE init_module_driver_constants
 END MODULE module_driver_constants




 SUBROUTINE inquire_of_wrf_data_order_xyz( data_order )
   USE module_driver_constants, ONLY : DATA_ORDER_XYZ
   IMPLICIT NONE
   INTEGER, INTENT(OUT) :: data_order
   data_order = DATA_ORDER_XYZ
 END SUBROUTINE inquire_of_wrf_data_order_xyz

 SUBROUTINE inquire_of_wrf_data_order_xzy( data_order )
   USE module_driver_constants, ONLY : DATA_ORDER_XZY
   IMPLICIT NONE
   INTEGER, INTENT(OUT) :: data_order
   data_order = DATA_ORDER_XZY
 END SUBROUTINE inquire_of_wrf_data_order_xzy

 SUBROUTINE inquire_of_wrf_iwordsize( iwordsz )
   IMPLICIT NONE
   INTEGER, INTENT(OUT) :: iwordsz
   iwordsz = 4
 END SUBROUTINE inquire_of_wrf_iwordsize

 SUBROUTINE inquire_of_wrf_rwordsize( rwordsz )
   IMPLICIT NONE
   INTEGER, INTENT(OUT) :: rwordsz
   rwordsz = 4
 END SUBROUTINE inquire_of_wrf_rwordsize

