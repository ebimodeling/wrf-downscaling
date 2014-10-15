!STARTOFREGISTRYGENERATEDINCLUDE 'inc/config_reads.inc'
!
! WARNING This file is generated automatically by use_registry
! using the data base in the file named Registry.
! Do not edit.  Your changes to this file will be lost.
!
! Contains namelist statements for module_config.F.
#ifndef NAMELIST_READ_UNIT
#  define NAMELIST_READ_UNIT nml_read_unit
#endif
#ifndef NAMELIST_WRITE_UNIT
#  define NAMELIST_WRITE_UNIT nml_write_unit
#endif
!
 nml_read_error = .FALSE.
 NML_LOOP : DO i=1,15
    REWIND ( UNIT = NAMELIST_READ_UNIT )
    SELECT CASE ( i )
       CASE ( 1 ) 
          nml_name = "time_control"
          READ   ( UNIT = NAMELIST_READ_UNIT , NML = time_control , ERR=9201, END=9202 )
#ifndef NO_NAMELIST_PRINT
          WRITE ( UNIT = NAMELIST_WRITE_UNIT, NML = time_control )
#endif
          CYCLE NML_LOOP
       CASE ( 2 ) 
          nml_name = "dfi_control"
          READ   ( UNIT = NAMELIST_READ_UNIT , NML = dfi_control , ERR=9201, END=9202 )
#ifndef NO_NAMELIST_PRINT
          WRITE ( UNIT = NAMELIST_WRITE_UNIT, NML = dfi_control )
#endif
          CYCLE NML_LOOP
       CASE ( 3 ) 
          nml_name = "domains"
          READ   ( UNIT = NAMELIST_READ_UNIT , NML = domains , ERR=9201, END=9202 )
#ifndef NO_NAMELIST_PRINT
          WRITE ( UNIT = NAMELIST_WRITE_UNIT, NML = domains )
#endif
          CYCLE NML_LOOP
       CASE ( 4 ) 
          nml_name = "tc"
          READ   ( UNIT = NAMELIST_READ_UNIT , NML = tc , ERR=9201, END=9202 )
#ifndef NO_NAMELIST_PRINT
          WRITE ( UNIT = NAMELIST_WRITE_UNIT, NML = tc )
#endif
          CYCLE NML_LOOP
       CASE ( 5 ) 
          nml_name = "physics"
          READ   ( UNIT = NAMELIST_READ_UNIT , NML = physics , ERR=9201, END=9202 )
#ifndef NO_NAMELIST_PRINT
          WRITE ( UNIT = NAMELIST_WRITE_UNIT, NML = physics )
#endif
          CYCLE NML_LOOP
       CASE ( 6 ) 
          nml_name = "afwa"
          READ   ( UNIT = NAMELIST_READ_UNIT , NML = afwa , ERR=9201, END=9202 )
#ifndef NO_NAMELIST_PRINT
          WRITE ( UNIT = NAMELIST_WRITE_UNIT, NML = afwa )
#endif
          CYCLE NML_LOOP
       CASE ( 7 ) 
          nml_name = "noah_mp"
          READ   ( UNIT = NAMELIST_READ_UNIT , NML = noah_mp , ERR=9201, END=9202 )
#ifndef NO_NAMELIST_PRINT
          WRITE ( UNIT = NAMELIST_WRITE_UNIT, NML = noah_mp )
#endif
          CYCLE NML_LOOP
       CASE ( 8 ) 
          nml_name = "fdda"
          READ   ( UNIT = NAMELIST_READ_UNIT , NML = fdda , ERR=9201, END=9202 )
#ifndef NO_NAMELIST_PRINT
          WRITE ( UNIT = NAMELIST_WRITE_UNIT, NML = fdda )
#endif
          CYCLE NML_LOOP
       CASE ( 9 ) 
          nml_name = "scm"
          READ   ( UNIT = NAMELIST_READ_UNIT , NML = scm , ERR=9201, END=9202 )
#ifndef NO_NAMELIST_PRINT
          WRITE ( UNIT = NAMELIST_WRITE_UNIT, NML = scm )
#endif
          CYCLE NML_LOOP
       CASE ( 10 ) 
          nml_name = "dynamics"
          READ   ( UNIT = NAMELIST_READ_UNIT , NML = dynamics , ERR=9201, END=9202 )
#ifndef NO_NAMELIST_PRINT
          WRITE ( UNIT = NAMELIST_WRITE_UNIT, NML = dynamics )
#endif
          CYCLE NML_LOOP
       CASE ( 11 ) 
          nml_name = "bdy_control"
          READ   ( UNIT = NAMELIST_READ_UNIT , NML = bdy_control , ERR=9201, END=9202 )
#ifndef NO_NAMELIST_PRINT
          WRITE ( UNIT = NAMELIST_WRITE_UNIT, NML = bdy_control )
#endif
          CYCLE NML_LOOP
       CASE ( 12 ) 
          nml_name = "grib2"
          READ   ( UNIT = NAMELIST_READ_UNIT , NML = grib2 , ERR=9201, END=9202 )
#ifndef NO_NAMELIST_PRINT
          WRITE ( UNIT = NAMELIST_WRITE_UNIT, NML = grib2 )
#endif
          CYCLE NML_LOOP
       CASE ( 13 ) 
          nml_name = "fire"
          READ   ( UNIT = NAMELIST_READ_UNIT , NML = fire , ERR=9201, END=9202 )
#ifndef NO_NAMELIST_PRINT
          WRITE ( UNIT = NAMELIST_WRITE_UNIT, NML = fire )
#endif
          CYCLE NML_LOOP
       CASE ( 14 ) 
          nml_name = "stoch"
          READ   ( UNIT = NAMELIST_READ_UNIT , NML = stoch , ERR=9201, END=9202 )
#ifndef NO_NAMELIST_PRINT
          WRITE ( UNIT = NAMELIST_WRITE_UNIT, NML = stoch )
#endif
          CYCLE NML_LOOP
       CASE ( 15 ) 
          nml_name = "diags"
          READ   ( UNIT = NAMELIST_READ_UNIT , NML = diags , ERR=9201, END=9202 )
#ifndef NO_NAMELIST_PRINT
          WRITE ( UNIT = NAMELIST_WRITE_UNIT, NML = diags )
#endif
          CYCLE NML_LOOP
    END SELECT
9201 CALL wrf_message("  ------ ERROR while reading namelist "//TRIM(nml_name)//" ------")
    nml_read_error = .TRUE.
    CALL wrf_alt_nml_obsolete(nml_read_unit, TRIM(nml_name))
    CYCLE NML_LOOP
9202 CALL wrf_debug(1,"Namelist "//TRIM(nml_name)//" not found in namelist.input.")
     CALL wrf_debug(1," --> Using registry defaults for variables in "//TRIM(nml_name))
 END DO NML_LOOP
 
 IF ( nml_read_error ) CALL wrf_error_fatal("ERRORS while reading one or more namelists from namelist.input.")
!ENDOFREGISTRYGENERATEDINCLUDE
