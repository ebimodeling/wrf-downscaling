

SUBROUTINE init_modules( phase )
 USE module_bc                  , ONLY : init_module_bc
 USE module_configure           , ONLY : init_module_configure
 USE module_driver_constants    , ONLY : init_module_driver_constants
 USE module_model_constants     , ONLY : init_module_model_constants
 USE module_domain              , ONLY : init_module_domain
 USE module_machine             , ONLY : init_module_machine
 USE module_nesting             , ONLY : init_module_nesting
 USE module_timing              , ONLY : init_module_timing
 USE module_tiles               , ONLY : init_module_tiles
 USE module_io_wrf              , ONLY : init_module_io_wrf
 USE module_io                  , ONLY : init_module_io

 USE module_wrf_quilt           , ONLY : init_module_wrf_quilt
 USE module_dm                  , ONLY : init_module_dm, split_communicator


 USE module_ext_internal        , ONLY : init_module_ext_internal

 USE module_wrf_error           , ONLY : init_module_wrf_error




























 INTEGER, INTENT(IN) :: phase    
                                 
IF ( phase == 1 ) THEN
 CALL init_module_bc
 CALL init_module_configure
 CALL init_module_driver_constants
 CALL init_module_model_constants
 CALL init_module_domain
 CALL init_module_machine


 CALL init_module_ext_internal  


 CALL split_communicator
 CALL init_module_wrf_quilt    

 

 CALL init_module_dm

ELSE
 CALL init_module_wrf_error 

 CALL init_module_nesting
 CALL init_module_timing
 CALL init_module_tiles
 CALL init_module_io_wrf
 CALL init_module_io




 CALL init_modules_em





ENDIF
 
END SUBROUTINE init_modules

