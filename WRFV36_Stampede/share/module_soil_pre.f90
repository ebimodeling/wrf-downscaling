

MODULE module_soil_pre

   USE module_date_time
   USE module_state_description

   CHARACTER (LEN=3) :: num_cat_count
   INTEGER , PARAMETER , DIMENSION(0:300) :: ints = &
   (/    0,    1,    2,    3,    4,    5,    6,    7,    8,    9, &
        10,   11,   12,   13,   14,   15,   16,   17,   18,   19, &
        20,   21,   22,   23,   24,   25,   26,   27,   28,   29, &
        30,   31,   32,   33,   34,   35,   36,   37,   38,   39, &
        40,   41,   42,   43,   44,   45,   46,   47,   48,   49, &
        50,   51,   52,   53,   54,   55,   56,   57,   58,   59, &
        60,   61,   62,   63,   64,   65,   66,   67,   68,   69, &
        70,   71,   72,   73,   74,   75,   76,   77,   78,   79, &
        80,   81,   82,   83,   84,   85,   86,   87,   88,   89, &
        90,   91,   92,   93,   94,   95,   96,   97,   98,   99, &
       100,  101,  102,  103,  104,  105,  106,  107,  108,  109, &
       110,  111,  112,  113,  114,  115,  116,  117,  118,  119, &
       120,  121,  122,  123,  124,  125,  126,  127,  128,  129, &
       130,  131,  132,  133,  134,  135,  136,  137,  138,  139, &
       140,  141,  142,  143,  144,  145,  146,  147,  148,  149, &
       150,  151,  152,  153,  154,  155,  156,  157,  158,  159, &
       160,  161,  162,  163,  164,  165,  166,  167,  168,  169, &
       170,  171,  172,  173,  174,  175,  176,  177,  178,  179, &
       180,  181,  182,  183,  184,  185,  186,  187,  188,  189, &
       190,  191,  192,  193,  194,  195,  196,  197,  198,  199, &
       200,  201,  202,  203,  204,  205,  206,  207,  208,  209, &
       210,  211,  212,  213,  214,  215,  216,  217,  218,  219, &
       220,  221,  222,  223,  224,  225,  226,  227,  228,  229, &
       230,  231,  232,  233,  234,  235,  236,  237,  238,  239, &
       240,  241,  242,  243,  244,  245,  246,  247,  248,  249, &
       250,  251,  252,  253,  254,  255,  256,  257,  258,  259, &
       260,  261,  262,  263,  264,  265,  266,  267,  268,  269, &
       270,  271,  272,  273,  274,  275,  276,  277,  278,  279, &
       280,  281,  282,  283,  284,  285,  286,  287,  288,  289, &
       290,  291,  292,  293,  294,  295,  296,  297,  298,  299, 300 /)

   

   LOGICAL , SAVE :: hold_ups
   INTEGER , SAVE :: em_width

   LOGICAL , EXTERNAL :: skip_middle_points_t

CONTAINS

   SUBROUTINE adjust_for_seaice_pre ( xice , landmask , tsk , ivgtyp , vegcat , lu_index , &
                                      xland , landusef , isltyp , soilcat , soilctop , &
                                      soilcbot , tmn , &
                                      seaice_threshold , &
                                      fractional_seaice, &
                                      num_veg_cat , num_soil_top_cat , num_soil_bot_cat , &
                                      iswater , isice , &
                                      scheme , &
                                      ids , ide , jds , jde , kds , kde , &
                                      ims , ime , jms , jme , kms , kme , &
                                      its , ite , jts , jte , kts , kte )

      IMPLICIT NONE

      INTEGER , INTENT(IN) :: ids , ide , jds , jde , kds , kde , &
                              ims , ime , jms , jme , kms , kme , &
                              its , ite , jts , jte , kts , kte , &
                              iswater , isice
      INTEGER , INTENT(IN) :: num_veg_cat , num_soil_top_cat , num_soil_bot_cat , scheme

      REAL , DIMENSION(ims:ime,1:num_veg_cat,jms:jme) , INTENT(INOUT):: landusef
      REAL , DIMENSION(ims:ime,1:num_soil_top_cat,jms:jme) , INTENT(INOUT):: soilctop
      REAL , DIMENSION(ims:ime,1:num_soil_bot_cat,jms:jme) , INTENT(INOUT):: soilcbot
      INTEGER , DIMENSION(ims:ime,jms:jme), INTENT(OUT) :: isltyp , ivgtyp
      REAL , DIMENSION(ims:ime,jms:jme) , INTENT(INOUT) :: landmask , xice , tsk , lu_index , &
                                                           vegcat, xland , soilcat , tmn
      REAL , INTENT(IN) :: seaice_threshold

      INTEGER :: i , j , num_seaice_changes , loop
      CHARACTER (LEN=132) :: message

      INTEGER, INTENT(IN) :: fractional_seaice
      REAL :: XICE_THRESHOLD

      IF ( FRACTIONAL_SEAICE == 0 ) THEN
         xice_threshold = 0.5
      ELSEIF ( FRACTIONAL_SEAICE == 1 ) THEN
         xice_threshold = 0.02
      ENDIF

      num_seaice_changes = 0
      fix_seaice : SELECT CASE ( scheme )

         CASE ( SLABSCHEME )
            DO j = jts , MIN(jde-1,jte)
               DO i = its , MIN(ide-1,ite)
                  IF ( skip_middle_points_t ( ids , ide , jds , jde , i , j , em_width , hold_ups ) ) CYCLE 
                  IF ( xice(i,j) .GT. 200.0 ) THEN
                     xice(i,j) = 0.
                     num_seaice_changes = num_seaice_changes + 1
                  END IF
               END DO
            END DO
            IF ( num_seaice_changes .GT. 0 ) THEN
               WRITE ( message , FMT='(A,I6)' ) &
               'Total pre number of sea ice locations removed (due to FLAG values) = ', &
               num_seaice_changes
               CALL wrf_debug ( 0 , message )
            END IF
            num_seaice_changes = 0
            DO j = jts , MIN(jde-1,jte)
               DO i = its , MIN(ide-1,ite)
                  IF ( skip_middle_points_t ( ids , ide , jds , jde , i , j , em_width , hold_ups ) ) CYCLE 
                  IF ( ( xice(i,j) .GE. xice_threshold ) .OR. &
                       ( ( landmask(i,j) .LT. 0.5 ) .AND. ( tsk(i,j) .LT. seaice_threshold ) ) ) THEN
                     IF ( FRACTIONAL_SEAICE == 0 ) THEN
                        xice(i,j) = 1.0
                     ENDIF
                     num_seaice_changes = num_seaice_changes + 1
                     if(landmask(i,j) .LT. 0.5 )tmn(i,j) = 271.4
                     vegcat(i,j)=isice
                     ivgtyp(i,j)=isice
                     lu_index(i,j)=isice
                     landmask(i,j)=1.
                     xland(i,j)=1.
                     DO loop=1,num_veg_cat
                        landusef(i,loop,j)=0.
                     END DO
                     landusef(i,ivgtyp(i,j),j)=1.

                     isltyp(i,j) = 16
                     soilcat(i,j)=isltyp(i,j)
                     DO loop=1,num_soil_top_cat
                        soilctop(i,loop,j)=0
                     END DO
                     DO loop=1,num_soil_bot_cat
                        soilcbot(i,loop,j)=0
                     END DO
                     soilctop(i,isltyp(i,j),j)=1.
                     soilcbot(i,isltyp(i,j),j)=1.
                  ELSE
                     xice(i,j) = 0.0
                  END IF
               END DO
            END DO
            IF ( num_seaice_changes .GT. 0 ) THEN
               WRITE ( message , FMT='(A,I6)' ) &
               'Total pre number of sea ice location changes (water to land) = ', num_seaice_changes
               CALL wrf_debug ( 0 , message )
            END IF

         CASE ( LSMSCHEME , NOAHMPSCHEME , RUCLSMSCHEME ,CLMSCHEME,SSIBSCHEME)   
            num_seaice_changes = 0
            DO j = jts , MIN(jde-1,jte)
               DO i = its , MIN(ide-1,ite)
                  IF ( skip_middle_points_t ( ids , ide , jds , jde , i , j , em_width , hold_ups ) ) CYCLE 
                  IF ( landmask(i,j) .GT. 0.5 ) THEN
                     if (xice(i,j).gt.0) num_seaice_changes = num_seaice_changes + 1
                     xice(i,j) = 0.
                  END IF
               END DO
            END DO
            IF ( num_seaice_changes .GT. 0 ) THEN
               WRITE ( message , FMT='(A,I6)' ) &
               'Total pre number of land location changes (seaice set to zero) = ', num_seaice_changes
               CALL wrf_debug ( 0 , message )
            END IF

      END SELECT fix_seaice

   END SUBROUTINE adjust_for_seaice_pre

   SUBROUTINE adjust_for_seaice_post ( xice , landmask , tsk_old , tsk , ivgtyp , vegcat , lu_index , &
                                      xland , landusef , isltyp , soilcat , soilctop , &
                                      soilcbot , tmn , vegfra , &
                                      tslb , smois , sh2o , &
                                      seaice_threshold , &
                                      sst , flag_sst , &
                                      fractional_seaice, &
                                      num_veg_cat , num_soil_top_cat , num_soil_bot_cat , &
                                      num_soil_layers , &
                                      iswater , isice , &
                                      scheme , &
                                      ids , ide , jds , jde , kds , kde , &
                                      ims , ime , jms , jme , kms , kme , &
                                      its , ite , jts , jte , kts , kte )

      IMPLICIT NONE

      INTEGER , INTENT(IN) :: ids , ide , jds , jde , kds , kde , &
                              ims , ime , jms , jme , kms , kme , &
                              its , ite , jts , jte , kts , kte , &
                              iswater , isice
      INTEGER , INTENT(IN) :: num_veg_cat , num_soil_top_cat , num_soil_bot_cat , scheme
      INTEGER , INTENT(IN) :: num_soil_layers

      REAL , DIMENSION(ims:ime,1:num_veg_cat,jms:jme) , INTENT(INOUT):: landusef
      REAL , DIMENSION(ims:ime,1:num_soil_top_cat,jms:jme) , INTENT(INOUT):: soilctop
      REAL , DIMENSION(ims:ime,1:num_soil_bot_cat,jms:jme) , INTENT(INOUT):: soilcbot
      REAL , DIMENSION(ims:ime,1:num_soil_layers,jms:jme) , INTENT(INOUT):: tslb , smois , sh2o
      REAL , DIMENSION(ims:ime,jms:jme)                   , INTENT(IN):: sst
      INTEGER , DIMENSION(ims:ime,jms:jme), INTENT(OUT) :: isltyp , ivgtyp
      REAL , DIMENSION(ims:ime,jms:jme) , INTENT(INOUT) :: landmask , xice , tsk , lu_index , &
                                                           vegcat, xland , soilcat , tmn , &
                                                           tsk_old , vegfra
      INTEGER , INTENT(IN) :: flag_sst
      REAL , INTENT(IN) :: seaice_threshold
      REAL :: total_depth , mid_point_depth

      INTEGER :: i , j , num_seaice_changes , loop
      CHARACTER (LEN=132) :: message


      INTEGER, INTENT(IN) :: fractional_seaice
      real :: xice_threshold

      IF ( FRACTIONAL_SEAICE == 0 ) THEN
         xice_threshold = 0.5
      ELSEIF ( FRACTIONAL_SEAICE == 1 ) THEN
         xice_threshold = 0.02
      ENDIF

      num_seaice_changes = 0
      fix_seaice : SELECT CASE ( scheme )

         CASE ( SLABSCHEME )

         CASE ( LSMSCHEME , NOAHMPSCHEME , CLMSCHEME, SSIBSCHEME )   
            DO j = jts , MIN(jde-1,jte)
               DO i = its , MIN(ide-1,ite)
                  IF ( skip_middle_points_t ( ids , ide , jds , jde , i , j , em_width , hold_ups ) ) CYCLE 
                  IF ( xice(i,j) .GT. 200.0 ) THEN
                     xice(i,j) = 0.
                     num_seaice_changes = num_seaice_changes + 1
                  END IF
               END DO
            END DO
            IF ( num_seaice_changes .GT. 0 ) THEN
               WRITE ( message , FMT='(A,I6)' ) &
               'Total post number of sea ice locations removed (due to FLAG values) = ', &
               num_seaice_changes
               CALL wrf_debug ( 0 , message )
            END IF
            num_seaice_changes = 0
            DO j = jts , MIN(jde-1,jte)
               DO i = its , MIN(ide-1,ite)
                  IF ( skip_middle_points_t ( ids , ide , jds , jde , i , j , em_width , hold_ups ) ) CYCLE 
                  IF ( ( ( tsk(i,j) .LT. 170 ) .OR. ( tsk(i,j) .GT. 400 ) ) .AND. &
                       ( ( tsk_old(i,j) .GT. 170 ) .AND. ( tsk_old(i,j) .LT. 400 ) ) )THEN
                     tsk(i,j) = tsk_old(i,j)
                  END IF
                  IF ( ( ( tsk(i,j) .LT. 170 ) .OR. ( tsk(i,j) .GT. 400 ) ) .AND. &
                       ( ( tsk_old(i,j) .LT. 170 ) .OR. ( tsk_old(i,j) .GT. 400 ) ) )THEN
                     print *,'TSK woes in seaice post, i,j=',i,j,'  tsk = ',tsk(i,j), tsk_old(i,j)
                     CALL wrf_error_fatal3("<stdin>",254,&
'TSK is unrealistic, problems for seaice post')
                  ELSE IF ( ( xice(i,j) .GE. xice_threshold ) .OR. &
                       ( ( landmask(i,j) .LT. 0.5 ) .AND. ( tsk(i,j) .LT. seaice_threshold ) ) ) THEN
                     IF ( FRACTIONAL_SEAICE == 0 ) THEN
                        xice(i,j) = 1.0
                     ENDIF
                     num_seaice_changes = num_seaice_changes + 1
                     if(landmask(i,j) .LT. 0.5 )tmn(i,j) = 271.4
                     vegcat(i,j)=isice
                     ivgtyp(i,j)=isice
                     lu_index(i,j)=isice
                     landmask(i,j)=1.
                     xland(i,j)=1.
                     vegfra(i,j)=0.
                     DO loop=1,num_veg_cat
                        landusef(i,loop,j)=0.
                     END DO
                     landusef(i,ivgtyp(i,j),j)=1.

                     tsk_old(i,j) = tsk(i,j)

                     isltyp(i,j) = 16
                     soilcat(i,j)=isltyp(i,j)
                     DO loop=1,num_soil_top_cat
                        soilctop(i,loop,j)=0
                     END DO
                     DO loop=1,num_soil_bot_cat
                        soilcbot(i,loop,j)=0
                     END DO
                     soilctop(i,isltyp(i,j),j)=1.
                     soilcbot(i,isltyp(i,j),j)=1.

                     total_depth = 3. 
                     DO loop = 1,num_soil_layers
                        mid_point_depth=(total_depth/num_soil_layers)/2. + &
                                        (loop-1)*(total_depth/num_soil_layers)
                        tslb(i,loop,j) = ( (total_depth-mid_point_depth)*tsk(i,j) + &
                                            mid_point_depth*tmn(i,j) ) / total_depth
                     END DO

                     DO loop=1,num_soil_layers
                        smois(i,loop,j) = 1.0
                        sh2o(i,loop,j)  = 0.0
                     END DO
                  ELSE IF ( xice(i,j) .LT. xice_threshold ) THEN
                     xice(i,j) = 0.
                  END IF
               END DO
            END DO
            IF ( num_seaice_changes .GT. 0 ) THEN
               WRITE ( message , FMT='(A,I6)' ) &
               'Total post number of sea ice location changes (water to land) = ', num_seaice_changes
               CALL wrf_debug ( 0 , message )
            END IF

        CASE ( RUCLSMSCHEME )
            DO j = jts , MIN(jde-1,jte)
               DO i = its , MIN(ide-1,ite)
                  IF ( skip_middle_points_t ( ids , ide , jds , jde , i , j , em_width , hold_ups ) ) CYCLE 
                  IF ( xice(i,j) .GT. 200.0 ) THEN
                     xice(i,j) = 0.
                     num_seaice_changes = num_seaice_changes + 1
                  END IF
               END DO
            END DO
            IF ( num_seaice_changes .GT. 0 ) THEN
               WRITE ( message , FMT='(A,I6)' ) &
               'Total post number of sea ice locations removed (due to FLAG values) = ', &
               num_seaice_changes
               CALL wrf_debug ( 0 , message )
            END IF
            num_seaice_changes = 0
            DO j = jts , MIN(jde-1,jte)
               DO i = its , MIN(ide-1,ite)
                  IF ( skip_middle_points_t ( ids , ide , jds , jde , i , j , em_width , hold_ups ) ) CYCLE 
                  IF ( ( ( tsk(i,j) .LT. 170 ) .OR. ( tsk(i,j) .GT. 400 ) ) .AND. &
                       ( ( tsk_old(i,j) .GT. 170 ) .AND. ( tsk_old(i,j) .LT. 400 ) ) )THEN
                     tsk(i,j) = tsk_old(i,j)
                  END IF
                  IF ( ( ( tsk(i,j) .LT. 170 ) .OR. ( tsk(i,j) .GT. 400 ) ) .AND. &
                       ( ( tsk_old(i,j) .LT. 170 ) .OR. ( tsk_old(i,j) .GT. 400 ) ) )THEN
                     print *,'TSK woes in seaice post, i,j=',i,j,'  tsk = ',tsk(i,j), tsk_old(i,j)
                     CALL wrf_error_fatal3("<stdin>",337,&
'TSK is unrealistic, problems for seaice post')
                  ELSE IF ( ( xice(i,j) .GE. xice_threshold ) .OR. &
                       ( ( landmask(i,j) .LT. 0.5 ) .AND. ( tsk(i,j) .LT. seaice_threshold ) ) ) THEN
                       IF ( FRACTIONAL_SEAICE == 0 ) THEN
                          xice(i,j) = 1.0
                       ELSE
                          xice(i,j)=max(0.25,xice(i,j))
                       ENDIF
                     num_seaice_changes = num_seaice_changes + 1
                     if(landmask(i,j) .LT. 0.5 )tmn(i,j) = 271.4
                     vegcat(i,j)=isice
                     ivgtyp(i,j)=isice
                     lu_index(i,j)=isice
                     landmask(i,j)=1.
                     xland(i,j)=1.
                     vegfra(i,j)=0.
                     DO loop=1,num_veg_cat
                        landusef(i,loop,j)=0.
                     END DO
                     landusef(i,ivgtyp(i,j),j)=1.


                   if(flag_sst.eq.1) then
                     tsk(i,j) = xice(i,j)*(min(271.4,tsk(i,j)))  &
                                +(1-xice(i,j))*sst(i,j)
                   else
                     tsk(i,j) = xice(i,j)*(min(271.4,tsk(i,j)))  &
                                +(1-xice(i,j))*tsk(i,j)
                   endif
                     tsk_old(i,j) = tsk(i,j)

                     isltyp(i,j) = 16
                     soilcat(i,j)=isltyp(i,j)
                     DO loop=1,num_soil_top_cat
                        soilctop(i,loop,j)=0
                     END DO
                     DO loop=1,num_soil_bot_cat
                        soilcbot(i,loop,j)=0
                     END DO
                     soilctop(i,isltyp(i,j),j)=1.
                     soilcbot(i,isltyp(i,j),j)=1.

                 total_depth = 3. 
                       tslb(i,1,j) = tsk(i,j)
                       tslb(i,num_soil_layers,j) = tmn(i,j)
                     DO loop = 2,num_soil_layers-1
                        mid_point_depth=(total_depth/num_soil_layers)/4. + &
                                        (loop-2)*(total_depth/num_soil_layers)
                        tslb(i,loop,j) = ( (total_depth-mid_point_depth)*tsk(i,j) + &
                                            mid_point_depth*tmn(i,j) ) / total_depth
                     END DO

                     DO loop=1,num_soil_layers
                        smois(i,loop,j) = 1.0
                        sh2o(i,loop,j)  = 0.0
                     END DO
                  ELSE IF ( xice(i,j) .LT. xice_threshold ) THEN
                     xice(i,j) = 0.
                  END IF
               END DO
            END DO
            IF ( num_seaice_changes .GT. 0 ) THEN
               WRITE ( message , FMT='(A,I6)' ) &
               'Total post number of sea ice location changes (water to land) = ', num_seaice_changes
               CALL wrf_debug ( 0 , message )
            END IF


      END SELECT fix_seaice

   END SUBROUTINE adjust_for_seaice_post

   SUBROUTINE process_percent_cat_new ( landmask ,  &
                                landuse_frac , soil_top_cat , soil_bot_cat , &
                                isltyp , ivgtyp , &
                                num_veg_cat , num_soil_top_cat , num_soil_bot_cat , &
                                ids , ide , jds , jde , kds , kde , &
                                ims , ime , jms , jme , kms , kme , &
                                its , ite , jts , jte , kts , kte , &
                                iswater )

      IMPLICIT NONE

      INTEGER , INTENT(IN) :: ids , ide , jds , jde , kds , kde , &
                              ims , ime , jms , jme , kms , kme , &
                              its , ite , jts , jte , kts , kte , &
                              iswater
      INTEGER , INTENT(IN) :: num_veg_cat , num_soil_top_cat , num_soil_bot_cat
      REAL , DIMENSION(ims:ime,1:num_veg_cat,jms:jme) , INTENT(INOUT):: landuse_frac
      REAL , DIMENSION(ims:ime,1:num_soil_top_cat,jms:jme) , INTENT(IN):: soil_top_cat
      REAL , DIMENSION(ims:ime,1:num_soil_bot_cat,jms:jme) , INTENT(IN):: soil_bot_cat
      INTEGER , DIMENSION(ims:ime,jms:jme), INTENT(OUT) :: isltyp , ivgtyp
      REAL , DIMENSION(ims:ime,jms:jme) , INTENT(INOUT) :: landmask

      INTEGER :: i , j , l , ll, dominant_index
      REAL :: dominant_value





      REAL :: lwthresh = .50


      INTEGER , PARAMETER :: iswater_soil = 14
      INTEGER :: iforce
      CHARACTER (LEN=132) :: message
      CHARACTER(LEN=256) :: mminlu
      LOGICAL :: aggregate_lu
integer :: change_water , change_land
change_water = 0
change_land = 0

      

      DO j = jts , MIN(jde-1,jte)
         DO i = its , MIN(ide-1,ite)
            IF ( skip_middle_points_t ( ids , ide , jds , jde , i , j , em_width , hold_ups ) ) CYCLE 
            dominant_value = landuse_frac(i,iswater,j)
            IF ( dominant_value .EQ. lwthresh ) THEN
               DO l = 1 , num_veg_cat
                  IF ( l .EQ. iswater ) CYCLE
                  IF ( ( landuse_frac(i,l,j) .EQ. lwthresh ) .AND. ( landmask(i,j) .LT. 0.5 ) ) THEN
                     PRINT *,i,j,' water and category ',l,' both at 50%, landmask is ',landmask(i,j)
                     landuse_frac(i,l,j) = lwthresh - .01
                     landuse_frac(i,iswater,j) = lwthresh + 0.01
                  ELSE IF ( ( landuse_frac(i,l,j) .EQ. lwthresh ) .AND. ( landmask(i,j) .GT. 0.5 ) ) THEN
                     PRINT *,i,j,' water and category ',l,' both at 50%, landmask is ',landmask(i,j)
                     landuse_frac(i,l,j) = lwthresh + .01
                     landuse_frac(i,iswater,j) = lwthresh - 0.01
                  END IF
               END DO
            END IF
         END DO
      END DO

      
      
      
      
 
      CALL nl_get_mminlu       ( 1 , mminlu       )
      CALL nl_get_aggregate_lu ( 1 , aggregate_lu )
      IF ( aggregate_lu ) THEN
         DO j = jts , MIN(jde-1,jte)
            DO i = its , MIN(ide-1,ite)
               IF ( skip_middle_points_t ( ids , ide , jds , jde , i , j , em_width , hold_ups ) ) CYCLE 
               CALL aggregate_categories_part1 ( landuse_frac , iswater , num_veg_cat , mminlu(1:4) )
            END DO
         END DO
      END IF

      

      DO j = jts , MIN(jde-1,jte)
         DO i = its , MIN(ide-1,ite)
            IF ( skip_middle_points_t ( ids , ide , jds , jde , i , j , em_width , hold_ups ) ) CYCLE 
            dominant_value = landuse_frac(i,1,j)
            dominant_index = 1
            DO l = 2 , num_veg_cat
               IF        ( l .EQ. iswater ) THEN
                  
               ELSE IF ( ( l .NE. iswater ) .AND. ( landuse_frac(i,l,j) .GT. dominant_value ) ) THEN
                  dominant_value = landuse_frac(i,l,j)
                  dominant_index = l
               END IF
            END DO
            IF ( landuse_frac(i,iswater,j) .GT. lwthresh ) THEN
               dominant_value = landuse_frac(i,iswater,j)
               dominant_index = iswater
            ELSE IF ( ( landuse_frac(i,iswater,j) .EQ. lwthresh) .AND. &
                      ( landmask(i,j) .LT. 0.5) .AND. &
                      ( dominant_value .EQ. lwthresh) ) THEN
               dominant_value = landuse_frac(i,iswater,j)
               dominant_index = iswater
            ELSE IF ( ( landuse_frac(i,iswater,j) .EQ. lwthresh) .AND. &
                      ( landmask(i,j) .GT. 0.5) .AND. &
                      ( dominant_value .EQ. lwthresh) ) THEN
               
            ELSE IF ( ( landuse_frac(i,iswater,j) .EQ. lwthresh ) .AND. &
                      ( dominant_value .LT. lwthresh ) ) THEN
               dominant_value = landuse_frac(i,iswater,j)
               dominant_index = iswater
            END IF
            IF      ( dominant_index .EQ. iswater ) THEN
if(landmask(i,j).gt.lwthresh) then






change_water=change_water+1
endif
               landmask(i,j) = 0
            ELSE IF ( dominant_index .NE. iswater ) THEN
if(landmask(i,j).lt.lwthresh) then






change_land=change_land+1
endif
               landmask(i,j) = 1
            END IF
            ivgtyp(i,j) = dominant_index
         END DO
      END DO

      

      iforce = 0
      DO i = its , MIN(ide-1,ite)
         DO j = jts , MIN(jde-1,jte)
            IF ( skip_middle_points_t ( ids , ide , jds , jde , i , j , em_width , hold_ups ) ) CYCLE 
            dominant_value = soil_top_cat(i,1,j)
            dominant_index = 1
            IF ( landmask(i,j) .GT. lwthresh ) THEN
               DO l = 2 , num_soil_top_cat
                  IF ( ( l .NE. iswater_soil ) .AND. ( soil_top_cat(i,l,j) .GT. dominant_value ) ) THEN
                     dominant_value = soil_top_cat(i,l,j)
                     dominant_index = l
                  END IF
               END DO
               IF ( dominant_value .LT. 0.01 ) THEN
                  iforce = iforce + 1
                  WRITE ( message , FMT = '(A,I4,I4)' ) &
                  'based on landuse, changing soil to land at point ',i,j
                  CALL wrf_debug(1,message)
                  WRITE ( num_cat_count , FMT = '(I3)' ) num_soil_top_cat
                  WRITE ( message , FMT = '('//num_cat_count//'(i3,1x))' ) (ints(l),l=1,num_soil_top_cat)
                  CALL wrf_debug(1,message)
                  WRITE ( message , FMT = '('//num_cat_count//'(i3,1x))' ) &
                    ((nint(soil_top_cat(i,ints(l),j)*100)), l=1,num_soil_top_cat)
                  CALL wrf_debug(1,message)
                  dominant_index = 8
               END IF
            ELSE
               dominant_index = iswater_soil
            END IF
            isltyp(i,j) = dominant_index
         END DO
      END DO

if(iforce.ne.0)then
WRITE(message,FMT='(A,I4,A,I6)' ) &
'forcing artificial silty clay loam at ',iforce,' points, out of ',&
(MIN(ide-1,ite)-its+1)*(MIN(jde-1,jte)-jts+1)
CALL wrf_debug(0,message)
endif
print *,'LAND  CHANGE = ',change_land
print *,'WATER CHANGE = ',change_water

   END SUBROUTINE process_percent_cat_new

   SUBROUTINE process_soil_real ( tsk , tmn , tavgsfc, &
                                landmask , sst , ht, toposoil, &
                                st_input , sm_input , sw_input , &
                                st_levels_input , sm_levels_input , sw_levels_input , &
                                zs , dzs , tslb , smois , sh2o , &
                                flag_sst , flag_tavgsfc, flag_soilhgt, &
                                flag_soil_layers, flag_soil_levels, &
                                ids , ide , jds , jde , kds , kde , &
                                ims , ime , jms , jme , kms , kme , &
                                its , ite , jts , jte , kts , kte , &
                                sf_surface_physics , num_soil_layers , real_data_init_type , &
                                num_st_levels_input , num_sm_levels_input , num_sw_levels_input , &
                                num_st_levels_alloc , num_sm_levels_alloc , num_sw_levels_alloc )

      IMPLICIT NONE

      INTEGER , INTENT(IN) :: ids , ide , jds , jde , kds , kde , &
                              ims , ime , jms , jme , kms , kme , &
                              its , ite , jts , jte , kts , kte , &
                              sf_surface_physics , num_soil_layers , real_data_init_type , &
                              num_st_levels_input , num_sm_levels_input , num_sw_levels_input , &
                              num_st_levels_alloc , num_sm_levels_alloc , num_sw_levels_alloc

      INTEGER , INTENT(IN) :: flag_sst, flag_tavgsfc
      INTEGER , INTENT(IN) :: flag_soil_layers, flag_soil_levels, flag_soilhgt

      REAL , DIMENSION(ims:ime,jms:jme) , INTENT(IN) :: landmask , sst

      INTEGER , DIMENSION(1:num_st_levels_input) , INTENT(INOUT) :: st_levels_input
      INTEGER , DIMENSION(1:num_sm_levels_input) , INTENT(INOUT) :: sm_levels_input
      INTEGER , DIMENSION(1:num_sw_levels_input) , INTENT(INOUT) :: sw_levels_input
      REAL , DIMENSION(ims:ime,1:num_st_levels_alloc,jms:jme) , INTENT(INOUT) :: st_input
      REAL , DIMENSION(ims:ime,1:num_sm_levels_alloc,jms:jme) , INTENT(INOUT) :: sm_input
      REAL , DIMENSION(ims:ime,1:num_sw_levels_alloc,jms:jme) , INTENT(INOUT) :: sw_input

      REAL, DIMENSION(1:num_soil_layers), INTENT(OUT)  ::  zs,dzs
      REAL , DIMENSION(ims:ime,num_soil_layers,jms:jme) , INTENT(OUT) :: tslb , smois , sh2o
      REAL , DIMENSION(ims:ime,jms:jme) , INTENT(IN) :: tavgsfc, ht, toposoil
      REAL , DIMENSION(ims:ime,jms:jme) , INTENT(INOUT) :: tsk, tmn

      INTEGER :: i , j , k, l , dominant_index , num_soil_cat , num_veg_cat, closest_layer
      REAL :: dominant_value, closest_depth, diff_cm
      REAL , ALLOCATABLE , DIMENSION(:) :: depth     
      REAL, PARAMETER :: get_temp_closest_to = 30.   
      REAL, PARAMETER :: something_big = 1.e6        
      INTEGER :: something_far = 1000                
      CHARACTER (LEN=132) :: message

      
      
      
      
      

      fix_bottom_level_for_temp : SELECT CASE ( sf_surface_physics )
         CASE (SLABSCHEME)
            IF ( flag_tavgsfc  .EQ. 1 ) THEN
               CALL wrf_debug ( 0 , 'Using average surface temperature for tmn')
               DO j = jts , MIN(jde-1,jte)
                  DO i = its , MIN(ide-1,ite)
                     IF ( skip_middle_points_t ( ids , ide , jds , jde , i , j , em_width , hold_ups ) ) CYCLE 
                     tmn(i,j) = tavgsfc(i,j)
                  END DO
               END DO
            ELSE
            
               closest_layer = something_far
               closest_depth = something_big
               DO k = 1, num_st_levels_input
                  diff_cm = abs( st_levels_input(k) - get_temp_closest_to )
                  IF ( diff_cm < closest_depth ) THEN
                     closest_depth = diff_cm
                     closest_layer = k
                  END IF
               END DO
               IF ( closest_layer == something_far ) THEN
                  CALL wrf_debug ( 0 , 'No soil temperature data for grid%tmn near 30 cm')
                  CALL wrf_debug ( 0 , 'Using 1 degree static annual mean temps' )
               ELSE
                  write(message, FMT='(A,F7.2,A,I3)')&
                     'Soil temperature closest to ',get_temp_closest_to, &
                     ' at level ',st_levels_input(closest_layer)
                  CALL wrf_debug ( 0 , message )
                  DO j = jts , MIN(jde-1,jte)
                     DO i = its , MIN(ide-1,ite)
                        IF ( skip_middle_points_t ( ids , ide , jds , jde , i , j , em_width , hold_ups ) ) CYCLE 
                        tmn(i,j) = st_input(i,closest_layer+1,j)
                     END DO
                  END DO
               END IF
            END IF

         CASE (LSMSCHEME)

         CASE (NOAHMPSCHEME)

         CASE (CLMSCHEME)

         CASE (RUCLSMSCHEME)

         CASE (PXLSMSCHEME)


            IF ( flag_tavgsfc  .EQ. 1 ) THEN
               CALL wrf_debug ( 0 , 'Using average surface temperature for tmn')
               DO j = jts , MIN(jde-1,jte)
                  DO i = its , MIN(ide-1,ite)
                     IF ( skip_middle_points_t ( ids , ide , jds , jde , i , j , em_width , hold_ups ) ) CYCLE 
                     tmn(i,j) = tavgsfc(i,j)
                  END DO
               END DO
            ELSE
            
               closest_layer = num_st_levels_input+1
               closest_depth = something_big
               DO k = 1, num_st_levels_input
                  diff_cm = abs( st_levels_input(k) - get_temp_closest_to )
                  IF ( diff_cm < closest_depth ) THEN
                     closest_depth = diff_cm
                     closest_layer = k
                  END IF
               END DO
               IF ( closest_layer == num_st_levels_input + 1 ) THEN
                  CALL wrf_debug ( 0 , 'No soil temperature data for grid%tmn near 30 cm')
                  CALL wrf_debug ( 0 , 'Using 1 degree static annual mean temps' )
               ELSE
                  write(message, FMT='(A,F7.2,A,I3)')&
                     'Soil temperature closest to ',get_temp_closest_to, &
                     ' at level ',st_levels_input(closest_layer)
                  CALL wrf_debug ( 0 , message )
                  DO j = jts , MIN(jde-1,jte)
                     DO i = its , MIN(ide-1,ite)
                        IF ( skip_middle_points_t ( ids , ide , jds , jde , i , j , em_width , hold_ups ) ) CYCLE 
                        tmn(i,j) = st_input(i,closest_layer+1,j)
                     END DO
                  END DO
               END IF
            END IF


      END SELECT fix_bottom_level_for_temp

      
      
      

      adjust_soil : SELECT CASE ( sf_surface_physics )

         CASE ( SLABSCHEME,LSMSCHEME,NOAHMPSCHEME,RUCLSMSCHEME,PXLSMSCHEME,CLMSCHEME,SSIBSCHEME )
            CALL adjust_soil_temp_new ( tmn , sf_surface_physics , tsk , ht ,            &
                                        toposoil , landmask , st_input, st_levels_input, &
                                        flag_soilhgt , flag_tavgsfc ,                    &
                                        flag_soil_layers , flag_soil_levels,             &
                                        num_st_levels_input, num_st_levels_alloc,        &
                                        ids , ide , jds , jde , kds , kde ,              &
                                        ims , ime , jms , jme , kms , kme ,              &
                                        its , ite , jts , jte , kts , kte )

      END SELECT adjust_soil

      
   
      IF      ( ( sf_surface_physics .EQ. SLABSCHEME ) .AND. ( num_soil_layers .GT. 1 ) ) THEN
         CALL init_soil_depth_1 ( zs , dzs , num_soil_layers )
         CALL init_soil_1_real ( tsk , tmn , tslb , zs , dzs , num_soil_layers , real_data_init_type , &
                                 landmask , sst , flag_sst , &
                                 ids , ide , jds , jde , kds , kde , &
                                 ims , ime , jms , jme , kms , kme , &
                                 its , ite , jts , jte , kts , kte )

      ELSE IF ( ( sf_surface_physics .EQ. LSMSCHEME ) .AND. ( num_soil_layers .GT. 1 ) ) THEN
         CALL init_soil_depth_2 ( zs , dzs , num_soil_layers )
         CALL init_soil_2_real ( tsk , tmn , smois , sh2o , tslb , &
                                 st_input , sm_input , sw_input , landmask , sst , &
                                 zs , dzs , &
                                 st_levels_input , sm_levels_input , sw_levels_input , &
                                 num_soil_layers , num_st_levels_input , num_sm_levels_input , num_sw_levels_input , &
                                 num_st_levels_alloc , num_sm_levels_alloc , num_sw_levels_alloc , &
                                 flag_sst , flag_soil_layers , flag_soil_levels , &
                                 ids , ide , jds , jde , kds , kde , &
                                 ims , ime , jms , jme , kms , kme , &
                                 its , ite , jts , jte , kts , kte )
      ELSE IF ( ( sf_surface_physics .EQ. NOAHMPSCHEME ) .AND. ( num_soil_layers .GT. 1 ) ) THEN
         CALL init_soil_depth_2 ( zs , dzs , num_soil_layers )
         CALL init_soil_2_real ( tsk , tmn , smois , sh2o , tslb , &
                                 st_input , sm_input , sw_input , landmask , sst , &
                                 zs , dzs , &
                                 st_levels_input , sm_levels_input , sw_levels_input , &
                                 num_soil_layers , num_st_levels_input , num_sm_levels_input , num_sw_levels_input , &
                                 num_st_levels_alloc , num_sm_levels_alloc , num_sw_levels_alloc , &
                                 flag_sst , flag_soil_layers , flag_soil_levels , &
                                 ids , ide , jds , jde , kds , kde , &
                                 ims , ime , jms , jme , kms , kme , &
                                 its , ite , jts , jte , kts , kte )
      ELSE IF ( ( sf_surface_physics .EQ. RUCLSMSCHEME ) .AND. ( num_soil_layers .GT. 1 ) ) THEN
         CALL init_soil_depth_3 ( zs , dzs , num_soil_layers )
         CALL init_soil_3_real ( tsk , tmn , smois , tslb , &
                                 st_input , sm_input , landmask , sst , &
                                 zs , dzs , &
                                 st_levels_input , sm_levels_input , &
                                 num_soil_layers , num_st_levels_input , num_sm_levels_input , &
                                 num_st_levels_alloc , num_sm_levels_alloc , &
                                 flag_sst , flag_soil_layers , flag_soil_levels , &
                                 ids , ide , jds , jde , kds , kde , &
                                 ims , ime , jms , jme , kms , kme , &
                                 its , ite , jts , jte , kts , kte )

      ELSE IF ( ( sf_surface_physics .EQ. CLMSCHEME ) .AND. ( num_soil_layers .GT. 1 ) ) THEN
         CALL init_soil_depth_4 ( zs , dzs , num_soil_layers )
         CALL init_soil_4_real ( tsk , tmn , smois , sh2o , tslb , &
                                 st_input , sm_input , sw_input , landmask , sst , &
                                 zs , dzs , &
                                 st_levels_input , sm_levels_input , sw_levels_input , &
                                 num_soil_layers , num_st_levels_input , num_sm_levels_input , num_sw_levels_input , &
                                 num_st_levels_alloc , num_sm_levels_alloc , num_sw_levels_alloc , &
                                 flag_sst , flag_soil_layers , flag_soil_levels , &
                                 ids , ide , jds , jde , kds , kde , &
                                 ims , ime , jms , jme , kms , kme , &
                                 its , ite , jts , jte , kts , kte )
      ELSE IF ( ( sf_surface_physics .EQ. PXLSMSCHEME ) .AND. ( num_soil_layers .GT. 1 ) ) THEN
         CALL init_soil_depth_7 ( zs , dzs , num_soil_layers )
         CALL init_soil_7_real ( tsk , tmn , smois , sh2o, tslb , &
                                 st_input , sm_input , sw_input, landmask , sst , &
                                 zs , dzs , &
                                 st_levels_input , sm_levels_input , sw_levels_input, &
                                 num_soil_layers , num_st_levels_input , num_sm_levels_input , num_sw_levels_input , &
                                 num_st_levels_alloc , num_sm_levels_alloc , num_sw_levels_alloc , &
                                 flag_sst , flag_soil_layers , flag_soil_levels , &
                                 ids , ide , jds , jde , kds , kde , &
                                 ims , ime , jms , jme , kms , kme , &
                                 its , ite , jts , jte , kts , kte )
      ELSE IF ( ( sf_surface_physics .EQ. 8 ) .AND. ( num_soil_layers .GT. 1 ) ) THEN
         CALL init_soil_depth_8 ( zs , dzs , num_soil_layers )
         CALL init_soil_3_real ( tsk , tmn , smois , tslb , &
                                 st_input , sm_input , landmask , sst , &
                                 zs , dzs , &
                                 st_levels_input , sm_levels_input , &
                                 num_soil_layers , num_st_levels_input , num_sm_levels_input , &
                                 num_st_levels_alloc , num_sm_levels_alloc , &
                                 flag_sst , flag_soil_layers , flag_soil_levels , &
                                 ids , ide , jds , jde , kds , kde , &
                                 ims , ime , jms , jme , kms , kme , &
                                 its , ite , jts , jte , kts , kte )
      END IF

   END SUBROUTINE process_soil_real

   SUBROUTINE process_soil_ideal ( xland,xice,vegfra,snow,canwat,  &
                                   ivgtyp,isltyp,tslb,smois, &
                                   tsk,tmn,zs,dzs,           &
                                   num_soil_layers,          &
                                   sf_surface_physics ,      &
                                   ids,ide, jds,jde, kds,kde,&
                                   ims,ime, jms,jme, kms,kme,&
                                   its,ite, jts,jte, kts,kte )

      IMPLICIT NONE

      INTEGER, INTENT(IN) ::ids,ide, jds,jde, kds,kde,  &
                            ims,ime, jms,jme, kms,kme,  &
                            its,ite, jts,jte, kts,kte

      INTEGER, INTENT(IN) :: num_soil_layers , sf_surface_physics

      REAL, DIMENSION( ims:ime, num_soil_layers, jms:jme ) , INTENT(INOUT) :: smois, tslb

      REAL, DIMENSION(num_soil_layers), INTENT(OUT) :: dzs,zs

      REAL, DIMENSION( ims:ime, jms:jme ) , INTENT(INOUT) :: tsk, tmn
      REAL, DIMENSION( ims:ime, jms:jme ) , INTENT(OUT) :: xland, snow, canwat, xice, vegfra
      INTEGER, DIMENSION( ims:ime, jms:jme ) , INTENT(OUT) :: ivgtyp, isltyp

      

      INTEGER :: itf,jtf

      itf=MIN(ite,ide-1)
      jtf=MIN(jte,jde-1)

      IF      ( ( sf_surface_physics .EQ. SLABSCHEME ) .AND. ( num_soil_layers .GT. 1 ) ) THEN
         CALL init_soil_depth_1 ( zs , dzs , num_soil_layers )
         CALL init_soil_1_ideal(tsk,tmn,tslb,xland,                      &
                                ivgtyp,zs,dzs,num_soil_layers,           &
                                ids,ide, jds,jde, kds,kde,               &
                                ims,ime, jms,jme, kms,kme,               &
                                its,ite, jts,jte, kts,kte                )
      ELSE IF ( ( sf_surface_physics .EQ. LSMSCHEME ) .AND. ( num_soil_layers .GT. 1 ) ) THEN
         CALL init_soil_depth_2 ( zs , dzs , num_soil_layers )
         CALL init_soil_2_ideal ( xland,xice,vegfra,snow,canwat,         &
                                  ivgtyp,isltyp,tslb,smois,tmn,          &
                                  num_soil_layers,                       &
                                  ids,ide, jds,jde, kds,kde,             &
                                  ims,ime, jms,jme, kms,kme,             &
                                  its,ite, jts,jte, kts,kte              )
      ELSE IF ( ( sf_surface_physics .EQ. NOAHMPSCHEME ) .AND. ( num_soil_layers .GT. 1 ) ) THEN
         CALL init_soil_depth_2 ( zs , dzs , num_soil_layers )
         CALL init_soil_2_ideal ( xland,xice,vegfra,snow,canwat,         &
                                  ivgtyp,isltyp,tslb,smois,tmn,          &
                                  num_soil_layers,                       &
                                  ids,ide, jds,jde, kds,kde,             &
                                  ims,ime, jms,jme, kms,kme,             &
                                  its,ite, jts,jte, kts,kte              )
      ELSE IF ( ( sf_surface_physics .EQ. RUCLSMSCHEME ) .AND. ( num_soil_layers .GT. 1 ) ) THEN
         CALL init_soil_depth_3 ( zs , dzs , num_soil_layers )

      END IF

   END SUBROUTINE process_soil_ideal

   SUBROUTINE adjust_soil_temp_new ( tmn , sf_surface_physics , tsk , ter ,            &
                                     toposoil , landmask , st_input , st_levels_input, &
                                     flag_toposoil , flag_tavgsfc ,                    &
                                     flag_soil_layers , flag_soil_levels,              &
                                     num_st_levels_input, num_st_levels_alloc,         &
                                     ids , ide , jds , jde , kds , kde ,               &
                                     ims , ime , jms , jme , kms , kme ,               &
                                     its , ite , jts , jte , kts , kte )

      IMPLICIT NONE

      INTEGER , INTENT(IN) :: ids , ide , jds , jde , kds , kde , &
                              ims , ime , jms , jme , kms , kme , &
                              its , ite , jts , jte , kts , kte 
      INTEGER , INTENT(IN) :: num_st_levels_input, num_st_levels_alloc

      REAL , DIMENSION(ims:ime,jms:jme) , INTENT(IN)    :: ter , toposoil , landmask
      REAL , DIMENSION(ims:ime,jms:jme) , INTENT(INOUT) :: tmn , tsk
      REAL , DIMENSION(ims:ime,1:num_st_levels_alloc,jms:jme) , INTENT(INOUT) :: st_input
      INTEGER , DIMENSION(1:num_st_levels_input) , INTENT(IN) :: st_levels_input

      INTEGER , INTENT(IN) :: sf_surface_physics , flag_toposoil , flag_tavgsfc
      INTEGER , INTENT(IN) :: flag_soil_layers , flag_soil_levels
 
      INTEGER :: i , j, k , st_near_sfc

      REAL :: soil_elev_min_val ,  soil_elev_max_val , soil_elev_min_dif , soil_elev_max_dif

      
      
      
      

       SELECT CASE ( sf_surface_physics )

         CASE ( LSMSCHEME , NOAHMPSCHEME,CLMSCHEME )
            DO j = jts , MIN(jde-1,jte)
               DO i = its , MIN(ide-1,ite)
                  IF ( skip_middle_points_t ( ids , ide , jds , jde , i , j , em_width , hold_ups ) ) CYCLE 
                  IF (landmask(i,j) .GT. 0.5 ) THEN
                     tmn(i,j) = tmn(i,j) - 0.0065 * ter(i,j)
                  END IF
               END DO
            END DO

         CASE (RUCLSMSCHEME)
            DO j = jts , MIN(jde-1,jte)
               DO i = its , MIN(ide-1,ite)
                  IF ( skip_middle_points_t ( ids , ide , jds , jde , i , j , em_width , hold_ups ) ) CYCLE 
                  IF (landmask(i,j) .GT. 0.5 ) THEN
                     tmn(i,j) = tmn(i,j) - 0.0065 * ter(i,j)
                  END IF
               END DO
            END DO

      END SELECT


      

      IF ( flag_toposoil .EQ. 1 ) THEN

         DO j = jts , MIN(jde-1,jte)
            DO i = its , MIN(ide-1,ite)

               IF ( skip_middle_points_t ( ids , ide , jds , jde , i , j , em_width , hold_ups ) ) CYCLE 
               
               
               
               
               
               
               

               soil_elev_min_val = toposoil(i,j)
               soil_elev_max_val = toposoil(i,j)
               soil_elev_min_dif = ter(i,j) - toposoil(i,j)
               soil_elev_max_dif = ter(i,j) - toposoil(i,j)

               IF      ( ( soil_elev_min_val .LT. -1000 ) .AND. ( landmask(i,j) .LT. 0.5 ) ) THEN
                  CYCLE
               ELSE IF ( ( soil_elev_min_val .LT. -1000 ) .AND. ( landmask(i,j) .GT. 0.5 ) ) THEN

cycle

               ENDIF

               IF      ( ( soil_elev_max_val .GT. 10000 ) .AND. ( landmask(i,j) .LT. 0.5 ) ) THEN
                  CYCLE
               ELSE IF ( ( soil_elev_max_val .GT. 10000 ) .AND. ( landmask(i,j) .GT. 0.5 ) ) THEN
print *,'no soil temperature elevation adjustment, soil height too high = ',toposoil(i,j)
cycle
                  CALL wrf_error_fatal3("<stdin>",996,&
'TOPOSOIL values have large positive values > 10,000 m , unrealistic.' )
               ENDIF

               IF      ( ( ( soil_elev_min_dif .LT. -3000 ) .OR. ( soil_elev_max_dif .GT. 3000 ) ) .AND. &
                           ( landmask(i,j) .LT. 0.5 ) ) THEN
                  CYCLE
               ELSE IF ( ( ( soil_elev_min_dif .LT. -3000 ) .OR. ( soil_elev_max_dif .GT. 3000 ) ) .AND. &
                           ( landmask(i,j) .GT. 0.5 ) ) THEN
print *,'no soil temperature elevation adjustment, diff of soil height and terrain = ',ter(i,j) - toposoil(i,j)
cycle
                  CALL wrf_error_fatal3("<stdin>",1007,&
'TOPOSOIL difference with terrain elevation differs by more than 3000 m, unrealistic' )
               ENDIF

               
               
               

               IF (landmask(i,j) .GT. 0.5 ) THEN
                  IF ( sf_surface_physics .EQ. SLABSCHEME ) THEN
                     st_near_sfc = 0                             
                     DO k = 1, num_st_levels_input
                        IF ( st_levels_input(k) .LE. 40 ) THEN
                           st_near_sfc = 1
                        END IF
                     END DO
                     IF ( ( flag_tavgsfc  == 1 ) .OR. ( st_near_sfc == 1 ) ) THEN
                        tmn(i,j) = tmn(i,j) - 0.0065 * ( ter(i,j) - toposoil(i,j) )
                     ELSE
                        tmn(i,j) = tmn(i,j) - 0.0065 * ter(i,j)
                     END IF
                  END IF

                  tsk(i,j) = tsk(i,j) - 0.0065 * ( ter(i,j) - toposoil(i,j) )
      
                  IF ( flag_soil_layers == 1 ) THEN
                     DO k = 2, num_st_levels_input+1
                        st_input(i,k,j) = st_input(i,k,j) - 0.0065 * ( ter(i,j) - toposoil(i,j) )
                     END DO
                  ELSE
                     DO k = 1, num_st_levels_input
                        st_input(i,k,j) = st_input(i,k,j) - 0.0065 * ( ter(i,j) - toposoil(i,j) )
                     END DO
                  END IF

               END IF
            END DO
         END DO

      END IF

   END SUBROUTINE adjust_soil_temp_new


   SUBROUTINE init_soil_depth_1 ( zs , dzs , num_soil_layers )

      IMPLICIT NONE

      INTEGER, INTENT(IN) :: num_soil_layers

      REAL, DIMENSION(1:num_soil_layers), INTENT(OUT)  ::  zs,dzs

      INTEGER                   ::      l

      
      

      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      

      IF ( num_soil_layers .NE. 5 ) THEN
         PRINT '(A)','Usually, the 5-layer diffusion uses 5 layers.  Change this in the namelist.'
         CALL wrf_error_fatal3("<stdin>",1088,&
'5-layer_diffusion_uses_5_layers' )
      END IF

      dzs(1)=.01
      zs(1)=.5*dzs(1)

      DO l=2,num_soil_layers
         dzs(l)=2*dzs(l-1)
         zs(l)=zs(l-1)+.5*dzs(l-1)+.5*dzs(l)
      ENDDO

   END SUBROUTINE init_soil_depth_1

   SUBROUTINE init_soil_depth_2 ( zs , dzs , num_soil_layers )

      IMPLICIT NONE

      INTEGER, INTENT(IN) :: num_soil_layers

      REAL, DIMENSION(1:num_soil_layers), INTENT(OUT)  ::  zs,dzs

      INTEGER                   ::      l

      dzs = (/ 0.1 , 0.3 , 0.6 , 1.0 /)

      IF ( num_soil_layers .NE. 4 ) THEN
         PRINT '(A)','Usually, the LSM uses 4 layers.  Change this in the namelist.'
         CALL wrf_error_fatal3("<stdin>",1116,&
'LSM_uses_4_layers' )
      END IF

      zs(1)=.5*dzs(1)

      DO l=2,num_soil_layers
         zs(l)=zs(l-1)+.5*dzs(l-1)+.5*dzs(l)
      ENDDO

   END SUBROUTINE init_soil_depth_2

   SUBROUTINE init_soil_depth_3 ( zs , dzs , num_soil_layers )

      IMPLICIT NONE

      INTEGER, INTENT(IN) :: num_soil_layers

      REAL, DIMENSION(1:num_soil_layers), INTENT(OUT)  ::  zs,dzs

      INTEGER                   ::      l

      CHARACTER (LEN=132) :: message







     IF ( num_soil_layers .EQ. 6) THEN
      zs  = (/ 0.00 , 0.05 , 0.20 , 0.40 , 1.60 , 3.00 /)
     ELSEIF ( num_soil_layers .EQ. 9) THEN
      zs  = (/ 0.00 , 0.01 , 0.04 , 0.10 , 0.30, 0.60, 1.00 , 1.60, 3.00 /)


     ENDIF

      IF ( num_soil_layers .EQ. 4 .OR. num_soil_layers .EQ. 5 ) THEN
         write (message, FMT='(A)') 'The RUC LSM uses 6, 9 or more levels.  Change this in the namelist.'
         CALL wrf_error_fatal3("<stdin>",1156,&
message )
      END IF

   END SUBROUTINE init_soil_depth_3

   SUBROUTINE init_soil_depth_4 ( zs , dzs , num_soil_layers )

      IMPLICIT NONE

      INTEGER, INTENT(IN) :: num_soil_layers

      REAL, DIMENSION(1:num_soil_layers), INTENT(OUT)  ::  zs,dzs
      REAL, PARAMETER :: scalez = 0.025

      INTEGER                   ::      l

      
      
      
      

      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      

      do l = 1, num_soil_layers
         zs(l) = scalez*(exp(0.5*(l-0.5))-1.)    
      enddo
       dzs(1) = 0.5*(zs(1)+zs(2))             
       do l = 2,num_soil_layers-1
         dzs(l)= 0.5*(zs(l+1)-zs(l-1))
       enddo
       dzs(num_soil_layers) = zs(num_soil_layers)-zs(num_soil_layers-1)

   END SUBROUTINE init_soil_depth_4

   SUBROUTINE init_soil_depth_7 ( zs , dzs , num_soil_layers )

      IMPLICIT NONE

      INTEGER, INTENT(IN) :: num_soil_layers

      REAL, DIMENSION(1:num_soil_layers), INTENT(OUT)  ::  zs,dzs

      INTEGER                   ::      l

      dzs = (/ 0.01 , 0.99 /)

      IF ( num_soil_layers .NE. 2 ) THEN
         PRINT '(A)','Usually, the PX LSM uses 2 layers.  Change this in the namelist.'
         CALL wrf_error_fatal3("<stdin>",1227,&
'PXLSM_uses_2_layers' )
      END IF

      zs(1) = 0.5 * dzs(1)
      zs(2) = dzs(1) + 0.5 * dzs(2)

   END SUBROUTINE init_soil_depth_7

   SUBROUTINE init_soil_depth_8 ( zs , dzs , num_soil_layers )

      IMPLICIT NONE

      INTEGER, INTENT(IN) :: num_soil_layers

      REAL, DIMENSION(1:num_soil_layers), INTENT(OUT)  ::  zs,dzs

      INTEGER                   ::      l

      zs(1) = 0.00
      zs(2) = 0.05
      zs(3) = 1.05

   END SUBROUTINE init_soil_depth_8

   SUBROUTINE init_soil_1_real ( tsk , tmn , tslb , zs , dzs , &
                                 num_soil_layers , real_data_init_type , &
                                 landmask , sst , flag_sst , &
                                 ids , ide , jds , jde , kds , kde , &
                                 ims , ime , jms , jme , kms , kme , &
                                 its , ite , jts , jte , kts , kte )

      IMPLICIT NONE

      INTEGER , INTENT(IN) :: num_soil_layers , real_data_init_type , &
                              ids , ide , jds , jde , kds , kde , &
                              ims , ime , jms , jme , kms , kme , &
                              its , ite , jts , jte , kts , kte

      INTEGER , INTENT(IN) :: flag_sst

      REAL , DIMENSION(ims:ime,jms:jme) , INTENT(IN) :: landmask , sst
      REAL , DIMENSION(ims:ime,jms:jme) , INTENT(IN) :: tsk , tmn

      REAL , DIMENSION(num_soil_layers) :: zs , dzs

      REAL , DIMENSION(ims:ime,num_soil_layers,jms:jme) , INTENT(OUT) :: tslb

      INTEGER :: i , j , l

      
      
      
      

      DO j = jts , MIN(jde-1,jte)
         DO i = its , MIN(ide-1,ite)
            IF ( skip_middle_points_t ( ids , ide , jds , jde , i , j , em_width , hold_ups ) ) CYCLE 
            IF ( landmask(i,j) .GT. 0.5 ) THEN
               DO l = 1 , num_soil_layers
                  tslb(i,l,j)= ( tsk(i,j) * ( zs(num_soil_layers) - zs(l) )   + &
                                 tmn(i,j) * ( zs(              l) - zs(1) ) ) / &
                                            ( zs(num_soil_layers) - zs(1) )
               END DO
            ELSE
               IF ( ( real_data_init_type .EQ. 1 ) .AND. ( flag_sst .EQ. 1 ) ) THEN
                  DO l = 1 , num_soil_layers
                     tslb(i,l,j)= sst(i,j)
                  END DO
               ELSE
                  DO l = 1 , num_soil_layers
                     tslb(i,l,j)= tsk(i,j)
                  END DO
               END IF
            END IF
         END DO
      END DO

   END SUBROUTINE init_soil_1_real

   SUBROUTINE init_soil_1_ideal(tsk,tmn,tslb,xland,             &
                       ivgtyp,ZS,DZS,num_soil_layers,           &
                       ids,ide, jds,jde, kds,kde,               &
                       ims,ime, jms,jme, kms,kme,               &
                       its,ite, jts,jte, kts,kte                )

      IMPLICIT NONE

      INTEGER, INTENT(IN   )    ::      ids,ide, jds,jde, kds,kde, &
                                        ims,ime, jms,jme, kms,kme, &
                                        its,ite, jts,jte, kts,kte

      INTEGER, INTENT(IN   )    ::      num_soil_layers

      REAL, DIMENSION( ims:ime , num_soil_layers , jms:jme ), INTENT(OUT) :: tslb
      REAL, DIMENSION( ims:ime , jms:jme ), INTENT(OUT) :: xland
      INTEGER, DIMENSION( ims:ime , jms:jme ), INTENT(OUT) :: ivgtyp

      REAL, DIMENSION(1:), INTENT(IN) :: dzs,zs

      REAL, DIMENSION( ims:ime, jms:jme ) , INTENT(IN) :: tsk, tmn

      

      INTEGER :: l,j,i,itf,jtf

      itf=MIN(ite,ide-1)
      jtf=MIN(jte,jde-1)

      IF (num_soil_layers.NE.1)THEN
         DO j=jts,jtf
            IF ( skip_middle_points_t ( ids , ide , jds , jde , i , j , em_width , hold_ups ) ) CYCLE 
            DO l=1,num_soil_layers
               DO i=its,itf
                 tslb(i,l,j)=( tsk(i,j)*(zs(num_soil_layers)-zs(l)) + tmn(i,j)*(zs(l)-zs(1)) ) / &
                             ( zs(num_soil_layers)-zs(1) )
               ENDDO
            ENDDO
         ENDDO
      ENDIF








   END SUBROUTINE init_soil_1_ideal

   SUBROUTINE init_soil_2_real ( tsk , tmn , smois , sh2o , tslb , &
                                 st_input , sm_input , sw_input , landmask , sst , &
                                 zs , dzs , &
                                 st_levels_input , sm_levels_input , sw_levels_input , &
                                 num_soil_layers , num_st_levels_input , num_sm_levels_input ,  num_sw_levels_input , &
                                 num_st_levels_alloc , num_sm_levels_alloc ,  num_sw_levels_alloc , &
                                 flag_sst , flag_soil_layers , flag_soil_levels , &
                                 ids , ide , jds , jde , kds , kde , &
                                 ims , ime , jms , jme , kms , kme , &
                                 its , ite , jts , jte , kts , kte )

      IMPLICIT NONE

      INTEGER , INTENT(IN) :: num_soil_layers , &
                              num_st_levels_input , num_sm_levels_input , num_sw_levels_input , &
                              num_st_levels_alloc , num_sm_levels_alloc , num_sw_levels_alloc , &
                              ids , ide , jds , jde , kds , kde , &
                              ims , ime , jms , jme , kms , kme , &
                              its , ite , jts , jte , kts , kte

      INTEGER , INTENT(IN) :: flag_sst, flag_soil_layers, flag_soil_levels

      INTEGER , DIMENSION(1:num_st_levels_input) , INTENT(INOUT) :: st_levels_input
      INTEGER , DIMENSION(1:num_sm_levels_input) , INTENT(INOUT) :: sm_levels_input
      INTEGER , DIMENSION(1:num_sw_levels_input) , INTENT(INOUT) :: sw_levels_input

      REAL , DIMENSION(ims:ime,1:num_st_levels_alloc,jms:jme) , INTENT(INOUT) :: st_input
      REAL , DIMENSION(ims:ime,1:num_sm_levels_alloc,jms:jme) , INTENT(INOUT) :: sm_input
      REAL , DIMENSION(ims:ime,1:num_sw_levels_alloc,jms:jme) , INTENT(INOUT) :: sw_input
      REAL , DIMENSION(ims:ime,jms:jme) , INTENT(IN) :: landmask , sst

      REAL , DIMENSION(ims:ime,jms:jme) , INTENT(IN) :: tmn
      REAL , DIMENSION(ims:ime,jms:jme) , INTENT(INOUT) :: tsk
      REAL , DIMENSION(num_soil_layers) :: zs , dzs

      REAL , DIMENSION(ims:ime,num_soil_layers,jms:jme) , INTENT(OUT) :: tslb , smois , sh2o

      REAL , ALLOCATABLE , DIMENSION(:) :: zhave

      INTEGER :: i , j , l , lout , lin , lwant , lhave , num
      REAL :: temp
      LOGICAL :: found_levels

      CHARACTER (LEN=132) :: message

      

      num = num_st_levels_input * num_sm_levels_input

      IF ( num .GE. 1 ) THEN

         


         IF ( flag_soil_levels == 1 ) THEN
           write(message, FMT='(A)') ' Assume RUC LSM 6-level input'
           CALL wrf_message ( message )
           ALLOCATE ( zhave( MAX(num_st_levels_input,num_sm_levels_input,num_sw_levels_input)  ) )
         ELSE
           write(message, FMT='(A)') ' Assume Noah LSM input'
           CALL wrf_message ( message )
         ALLOCATE ( zhave( MAX(num_st_levels_input,num_sm_levels_input,num_sw_levels_input) +2) )
         END IF


         

         outert : DO lout = 1 , num_st_levels_input-1
            innert : DO lin = lout+1 , num_st_levels_input
               IF ( st_levels_input(lout) .GT. st_levels_input(lin) ) THEN
                  temp = st_levels_input(lout)
                  st_levels_input(lout) = st_levels_input(lin)
                  st_levels_input(lin) = NINT(temp)
                  DO j = jts , MIN(jde-1,jte)
                     DO i = its , MIN(ide-1,ite)
                        IF ( skip_middle_points_t ( ids , ide , jds , jde , i , j , em_width , hold_ups ) ) CYCLE 
                        temp = st_input(i,lout+1,j)
                        st_input(i,lout+1,j) = st_input(i,lin+1,j)
                        st_input(i,lin+1,j) = temp
                     END DO
                  END DO
               END IF
            END DO innert
         END DO outert

      IF ( flag_soil_layers == 1 ) THEN
         DO j = jts , MIN(jde-1,jte)
            DO i = its , MIN(ide-1,ite)
               IF ( skip_middle_points_t ( ids , ide , jds , jde , i , j , em_width , hold_ups ) ) CYCLE 
               st_input(i,1,j) = tsk(i,j)
               st_input(i,num_st_levels_input+2,j) = tmn(i,j)
            END DO
         END DO
      ENDIF

         

         outerm: DO lout = 1 , num_sm_levels_input-1
            innerm : DO lin = lout+1 , num_sm_levels_input
               IF ( sm_levels_input(lout) .GT. sm_levels_input(lin) ) THEN
                  temp = sm_levels_input(lout)
                  sm_levels_input(lout) = sm_levels_input(lin)
                  sm_levels_input(lin) = NINT(temp)
                  DO j = jts , MIN(jde-1,jte)
                     DO i = its , MIN(ide-1,ite)
                        IF ( skip_middle_points_t ( ids , ide , jds , jde , i , j , em_width , hold_ups ) ) CYCLE 
                        temp = sm_input(i,lout+1,j)
                        sm_input(i,lout+1,j) = sm_input(i,lin+1,j)
                        sm_input(i,lin+1,j) = temp
                     END DO
                  END DO
               END IF
            END DO innerm
         END DO outerm

      IF ( flag_soil_layers == 1 ) THEN
         DO j = jts , MIN(jde-1,jte)
            DO i = its , MIN(ide-1,ite)
               IF ( skip_middle_points_t ( ids , ide , jds , jde , i , j , em_width , hold_ups ) ) CYCLE 
               sm_input(i,1,j) = sm_input(i,2,j)
               sm_input(i,num_sm_levels_input+2,j) = sm_input(i,num_sm_levels_input+1,j)
            END DO
         END DO
      ENDIF

         

         outerw: DO lout = 1 , num_sw_levels_input-1
            innerw : DO lin = lout+1 , num_sw_levels_input
               IF ( sw_levels_input(lout) .GT. sw_levels_input(lin) ) THEN
                  temp = sw_levels_input(lout)
                  sw_levels_input(lout) = sw_levels_input(lin)
                  sw_levels_input(lin) = NINT(temp)
                  DO j = jts , MIN(jde-1,jte)
                     DO i = its , MIN(ide-1,ite)
                        IF ( skip_middle_points_t ( ids , ide , jds , jde , i , j , em_width , hold_ups ) ) CYCLE 
                        temp = sw_input(i,lout+1,j)
                        sw_input(i,lout+1,j) = sw_input(i,lin+1,j)
                        sw_input(i,lin+1,j) = temp
                     END DO
                  END DO
               END IF
            END DO innerw
         END DO outerw
         IF ( num_sw_levels_input .GT. 1 ) THEN
            DO j = jts , MIN(jde-1,jte)
               DO i = its , MIN(ide-1,ite)
                  IF ( skip_middle_points_t ( ids , ide , jds , jde , i , j , em_width , hold_ups ) ) CYCLE 
                  sw_input(i,1,j) = sw_input(i,2,j)
                  sw_input(i,num_sw_levels_input+2,j) = sw_input(i,num_sw_levels_input+1,j)
               END DO
            END DO
         END IF

         found_levels = .TRUE.

      ELSE IF ( ( num .LE. 0 ) .AND. (  start_date .NE. current_date ) ) THEN

         found_levels = .FALSE.

      ELSE
         CALL wrf_error_fatal3("<stdin>",1518,&
         'No input soil level data (temperature, moisture or liquid, or all are missing). Required for LSM.' )
      END IF

      

      IF ( found_levels ) THEN

         

         IF ( flag_soil_levels == 1 ) THEN
            DO l = 1 , num_st_levels_input
               zhave(l) = st_levels_input(l) / 100.
            END DO

         

         z_wantt : DO lwant = 1 , num_soil_layers
            z_havet : DO lhave = 1 , num_st_levels_input -1
               IF ( ( zs(lwant) .GE. zhave(lhave  ) ) .AND. &
                    ( zs(lwant) .LE. zhave(lhave+1) ) ) THEN
                  DO j = jts , MIN(jde-1,jte)
                     DO i = its , MIN(ide-1,ite)
                        IF ( skip_middle_points_t ( ids , ide , jds , jde , i , j , em_width , hold_ups ) ) CYCLE 
                        tslb(i,lwant,j)= ( st_input(i,lhave,j ) * ( zhave(lhave+1) - zs   (lwant) ) + &
                                           st_input(i,lhave+1,j) * ( zs   (lwant  ) - zhave(lhave) ) ) / &
                                                                   ( zhave(lhave+1) - zhave(lhave) )
                     END DO
                  END DO
                  EXIT z_havet
               END IF
            END DO z_havet
         END DO z_wantt

         ELSE

         
         

         zhave(1) = 0.
         DO l = 1 , num_st_levels_input
            zhave(l+1) = st_levels_input(l) / 100.
         END DO
         zhave(num_st_levels_input+2) = 300. / 100.

         

         z_wantt_2: DO lwant = 1 , num_soil_layers
            z_havet_2 : DO lhave = 1 , num_st_levels_input +2 -1
               IF ( ( zs(lwant) .GE. zhave(lhave  ) ) .AND. &
                    ( zs(lwant) .LE. zhave(lhave+1) ) ) THEN
                  DO j = jts , MIN(jde-1,jte)
                     DO i = its , MIN(ide-1,ite)
                        IF ( skip_middle_points_t ( ids , ide , jds , jde , i , j , em_width , hold_ups ) ) CYCLE 
                        tslb(i,lwant,j)= ( st_input(i,lhave  ,j) * ( zhave(lhave+1) - zs   (lwant) ) + &
                                           st_input(i,lhave+1,j) * ( zs   (lwant  ) - zhave(lhave) ) ) / &
                                                                   ( zhave(lhave+1) - zhave(lhave) )
                     END DO
                  END DO
                  EXIT z_havet_2
               END IF
            END DO z_havet_2
         END DO z_wantt_2

      END IF


      IF ( flag_soil_levels == 1 ) THEN
         DO l = 1 , num_sm_levels_input
            zhave(l) = sm_levels_input(l) / 100.
         END DO

      

      z_wantm : DO lwant = 1 , num_soil_layers
         z_havem : DO lhave = 1 , num_sm_levels_input -1
            IF ( ( zs(lwant) .GE. zhave(lhave  ) ) .AND. &
                 ( zs(lwant) .LE. zhave(lhave+1) ) ) THEN
               DO j = jts , MIN(jde-1,jte)
                  DO i = its , MIN(ide-1,ite)
                     IF ( skip_middle_points_t ( ids , ide , jds , jde , i , j , em_width , hold_ups ) ) CYCLE 
                     smois(i,lwant,j)= ( sm_input(i,lhave,j ) * ( zhave(lhave+1) - zs   (lwant) ) + &
                                         sm_input(i,lhave+1,j) * ( zs   (lwant  ) - zhave(lhave) ) ) / &
                                                                 ( zhave(lhave+1) - zhave(lhave) )
                  END DO
               END DO
               EXIT z_havem
            END IF
         END DO z_havem
      END DO z_wantm

      ELSE
         
         
         
         

         zhave(1) = 0.
         DO l = 1 , num_sm_levels_input
            zhave(l+1) = sm_levels_input(l) / 100.
         END DO
         zhave(num_sm_levels_input+2) = 300. / 100.

         

         z_wantm_2 : DO lwant = 1 , num_soil_layers
            z_havem_2 : DO lhave = 1 , num_sm_levels_input +2 -1
               IF ( ( zs(lwant) .GE. zhave(lhave  ) ) .AND. &
                    ( zs(lwant) .LE. zhave(lhave+1) ) ) THEN
                  DO j = jts , MIN(jde-1,jte)
                     DO i = its , MIN(ide-1,ite)
                        IF ( skip_middle_points_t ( ids , ide , jds , jde , i , j , em_width , hold_ups ) ) CYCLE 
                        smois(i,lwant,j)= ( sm_input(i,lhave  ,j) * ( zhave(lhave+1) - zs   (lwant) ) + &
                                            sm_input(i,lhave+1,j) * ( zs   (lwant  ) - zhave(lhave) ) ) / &
                                                                    ( zhave(lhave+1) - zhave(lhave) )
                     END DO
                  END DO
                  EXIT z_havem_2
               END IF
            END DO z_havem_2
         END DO z_wantm_2
       ENDIF

         

         IF ( num_sw_levels_input .GT. 1 ) THEN
      
      
      IF ( flag_soil_levels == 1 ) THEN

      z_wantw : DO lwant = 1 , num_soil_layers
         z_havew : DO lhave = 1 , num_sw_levels_input -1
            IF ( ( zs(lwant) .GE. zhave(lhave  ) ) .AND. &
                 ( zs(lwant) .LE. zhave(lhave+1) ) ) THEN
               DO j = jts , MIN(jde-1,jte)
                  DO i = its , MIN(ide-1,ite)
                     IF ( skip_middle_points_t ( ids , ide , jds , jde , i , j , em_width , hold_ups ) ) CYCLE
                     sh2o(i,lwant,j)= ( sw_input(i,lhave,j ) * ( zhave(lhave+1) - zs   (lwant) ) + &
                                         sw_input(i,lhave+1,j) * ( zs   (lwant) - zhave(lhave) ) ) / &
                                                                 ( zhave(lhave+1) - zhave(lhave) )
                  END DO
               END DO
               EXIT z_havew
            END IF
         END DO z_havew
      END DO z_wantw

     ELSE
            zhave(1) = 0.
            DO l = 1 , num_sw_levels_input
               zhave(l+1) = sw_levels_input(l) / 100.
            END DO
            zhave(num_sw_levels_input+2) = 300. / 100.

            

            z_wantw_2 : DO lwant = 1 , num_soil_layers
               z_havew_2 : DO lhave = 1 , num_sw_levels_input +2 -1
                  IF ( ( zs(lwant) .GE. zhave(lhave  ) ) .AND. &
                       ( zs(lwant) .LE. zhave(lhave+1) ) ) THEN
                     DO j = jts , MIN(jde-1,jte)
                        DO i = its , MIN(ide-1,ite)
                           IF ( skip_middle_points_t ( ids , ide , jds , jde , i , j , em_width , hold_ups ) ) CYCLE
                           sh2o(i,lwant,j)= ( sw_input(i,lhave  ,j) * ( zhave(lhave+1) - zs   (lwant) ) + &
                                               sw_input(i,lhave+1,j) * ( zs (lwant  ) - zhave(lhave) ) ) / &
                                                                       ( zhave(lhave+1) - zhave(lhave) )
                        END DO
                     END DO
                     EXIT z_havew_2
                  END IF
               END DO z_havew_2
            END DO z_wantw_2
       ENDIF

         END IF


         
         

         IF ( flag_sst .EQ. 1 ) THEN
            DO j = jts , MIN(jde-1,jte)
               DO i = its , MIN(ide-1,ite)
                  IF ( skip_middle_points_t ( ids , ide , jds , jde , i , j , em_width , hold_ups ) ) CYCLE 
                  IF ( landmask(i,j) .LT. 0.5 ) THEN
                     DO l = 1 , num_soil_layers
                        tslb(i,l,j)= sst(i,j)
                        smois(i,l,j)= 1.0
                        sh2o (i,l,j)= 1.0
                     END DO
                  END IF
               END DO
            END DO
         ELSE
            DO j = jts , MIN(jde-1,jte)
               DO i = its , MIN(ide-1,ite)
                  IF ( skip_middle_points_t ( ids , ide , jds , jde , i , j , em_width , hold_ups ) ) CYCLE 
                  IF ( landmask(i,j) .LT. 0.5 ) THEN
                     DO l = 1 , num_soil_layers
                        tslb(i,l,j)= tsk(i,j)
                        smois(i,l,j)= 1.0
                        sh2o (i,l,j)= 1.0
                     END DO
                  END IF
               END DO
            END DO
         END IF

         DEALLOCATE (zhave)

      END IF

   END SUBROUTINE init_soil_2_real

   SUBROUTINE init_soil_2_ideal ( xland,xice,vegfra,snow,canwat,     &
                     ivgtyp,isltyp,tslb,smois,tmn,                  &
                     num_soil_layers,                               &
                     ids,ide, jds,jde, kds,kde,                     &
                     ims,ime, jms,jme, kms,kme,                     &
                     its,ite, jts,jte, kts,kte                      )

      IMPLICIT NONE

      INTEGER, INTENT(IN) ::ids,ide, jds,jde, kds,kde,  &
                            ims,ime, jms,jme, kms,kme,  &
                            its,ite, jts,jte, kts,kte

      INTEGER, INTENT(IN) ::num_soil_layers

      REAL, DIMENSION( ims:ime, num_soil_layers, jms:jme ) , INTENT(OUT) :: smois, tslb

      REAL, DIMENSION( ims:ime, jms:jme ) , INTENT(IN)   :: xland
      REAL, DIMENSION( ims:ime, jms:jme ) , INTENT(OUT)  :: snow, canwat, xice, vegfra, tmn

      INTEGER, DIMENSION( ims:ime, jms:jme ) , INTENT(OUT) :: ivgtyp, isltyp

      INTEGER :: icm,jcm,itf,jtf
      INTEGER ::  i,j,l

      itf=min0(ite,ide-1)
      jtf=min0(jte,jde-1)

      icm = ide/2
      jcm = jde/2

      DO j=jts,jtf
         DO l=1,num_soil_layers
            DO i=its,itf
               IF ( skip_middle_points_t ( ids , ide , jds , jde , i , j , em_width , hold_ups ) ) CYCLE 
               if (xland(i,j) .lt. 1.5) then
               smois(i,1,j)=0.30
               smois(i,2,j)=0.30
               smois(i,3,j)=0.30
               smois(i,4,j)=0.30

               tslb(i,1,j)=290.
               tslb(i,2,j)=290.
               tslb(i,3,j)=290.
               tslb(i,4,j)=290.
               endif
            ENDDO
         ENDDO
      ENDDO

   END SUBROUTINE init_soil_2_ideal

   SUBROUTINE init_soil_3_real ( tsk , tmn , smois , tslb , &
                                 st_input , sm_input , landmask, sst, &
                                 zs , dzs , &
                                 st_levels_input , sm_levels_input , &
                                 num_soil_layers , num_st_levels_input , num_sm_levels_input ,  &
                                 num_st_levels_alloc , num_sm_levels_alloc , &
                                 flag_sst , flag_soil_layers , flag_soil_levels , &
                                 ids , ide , jds , jde , kds , kde , &
                                 ims , ime , jms , jme , kms , kme , &
                                 its , ite , jts , jte , kts , kte )

      IMPLICIT NONE

      INTEGER , INTENT(IN) :: num_soil_layers , &
                              num_st_levels_input , num_sm_levels_input , &
                              num_st_levels_alloc , num_sm_levels_alloc , &
                              ids , ide , jds , jde , kds , kde , &
                              ims , ime , jms , jme , kms , kme , &
                              its , ite , jts , jte , kts , kte

      INTEGER , INTENT(IN) :: flag_sst, flag_soil_layers, flag_soil_levels

      INTEGER , DIMENSION(1:num_st_levels_input) , INTENT(INOUT) :: st_levels_input
      INTEGER , DIMENSION(1:num_sm_levels_input) , INTENT(INOUT) :: sm_levels_input

      REAL , DIMENSION(ims:ime,1:num_st_levels_alloc,jms:jme) , INTENT(INOUT) :: st_input
      REAL , DIMENSION(ims:ime,1:num_sm_levels_alloc,jms:jme) , INTENT(INOUT) :: sm_input
      REAL , DIMENSION(ims:ime,jms:jme) , INTENT(IN) :: landmask , sst

      REAL , DIMENSION(ims:ime,jms:jme) , INTENT(IN) :: tmn
      REAL , DIMENSION(ims:ime,jms:jme) , INTENT(INOUT) :: tsk
      REAL , DIMENSION(num_soil_layers) :: zs , dzs

      REAL , DIMENSION(ims:ime,num_soil_layers,jms:jme) , INTENT(OUT) :: tslb , smois

      REAL , ALLOCATABLE , DIMENSION(:) :: zhave

      INTEGER :: i , j , l , lout , lin , lwant , lhave, k
      REAL :: temp

      CHARACTER (LEN=132) :: message

      

      IF ( ( num_st_levels_input .LE. 0 ) .OR. &
           ( num_sm_levels_input .LE. 0 ) ) THEN
         write (message, FMT='(A)')&
'No input soil level data (either temperature or moisture, or both are missing).  Required for RUC LSM.'
         CALL wrf_error_fatal3("<stdin>",1832,&
message )
      ELSE
         IF ( flag_soil_levels == 1 ) THEN
           write(message, FMT='(A)') ' Assume RUC LSM 6-level input'
           CALL wrf_message ( message )
           ALLOCATE ( zhave( MAX(num_st_levels_input,num_sm_levels_input)  ) )
         ELSE
           write(message, FMT='(A)') ' Assume non-RUC LSM input'
           CALL wrf_message ( message )
           ALLOCATE ( zhave( MAX(num_st_levels_input,num_soil_layers)  ) )
         END IF
      END IF

      

      outert : DO lout = 1 , num_st_levels_input-1
         innert : DO lin = lout+1 , num_st_levels_input
            IF ( st_levels_input(lout) .GT. st_levels_input(lin) ) THEN
               temp = st_levels_input(lout)
               st_levels_input(lout) = st_levels_input(lin)
               st_levels_input(lin) = NINT(temp)
               DO j = jts , MIN(jde-1,jte)
                  DO i = its , MIN(ide-1,ite)
                     IF ( skip_middle_points_t ( ids , ide , jds , jde , i , j , em_width , hold_ups ) ) CYCLE 
                     temp = st_input(i,lout,j)
                     st_input(i,lout,j) = st_input(i,lin,j)
                     st_input(i,lin,j) = temp
                  END DO
               END DO
            END IF
         END DO innert
      END DO outert

      IF ( flag_soil_layers == 1 ) THEN
      DO j = jts , MIN(jde-1,jte)
         DO i = its , MIN(ide-1,ite)
            IF ( skip_middle_points_t ( ids , ide , jds , jde , i , j , em_width , hold_ups ) ) CYCLE 
            st_input(i,1,j) = tsk(i,j)
            st_input(i,num_st_levels_input+2,j) = tmn(i,j)
         END DO
      END DO
      END IF

      

      outerm: DO lout = 1 , num_sm_levels_input-1
         innerm : DO lin = lout+1 , num_sm_levels_input
            IF ( sm_levels_input(lout) .GT. sm_levels_input(lin) ) THEN
               temp = sm_levels_input(lout)
               sm_levels_input(lout) = sm_levels_input(lin)
               sm_levels_input(lin) = NINT(temp)
               DO j = jts , MIN(jde-1,jte)
                  DO i = its , MIN(ide-1,ite)
                     IF ( skip_middle_points_t ( ids , ide , jds , jde , i , j , em_width , hold_ups ) ) CYCLE 
                     temp = sm_input(i,lout,j)
                     sm_input(i,lout,j) = sm_input(i,lin,j)
                     sm_input(i,lin,j) = temp
                  END DO
               END DO
            END IF
         END DO innerm
      END DO outerm

      IF ( flag_soil_layers == 1 ) THEN
      DO j = jts , MIN(jde-1,jte)
         DO i = its , MIN(ide-1,ite)
            IF ( skip_middle_points_t ( ids , ide , jds , jde , i , j , em_width , hold_ups ) ) CYCLE 
            sm_input(i,1,j) = (sm_input(i,2,j)-sm_input(i,3,j))/   &
                              (st_levels_input(2)-st_levels_input(1))*st_levels_input(1)+  &
                              sm_input(i,2,j)

            sm_input(i,num_sm_levels_input+2,j) = sm_input(i,num_sm_levels_input+1,j)
         END DO
      END DO
      END IF

      

      IF ( flag_soil_levels == 1 ) THEN
         DO l = 1 , num_st_levels_input
            zhave(l) = st_levels_input(l) / 100.
         END DO

      


      z_wantt : DO lwant = 1 , num_soil_layers
         z_havet : DO lhave = 1 , num_st_levels_input -1
            IF ( ( zs(lwant) .GE. zhave(lhave  ) ) .AND. &
                 ( zs(lwant) .LE. zhave(lhave+1) ) ) THEN
               DO j = jts , MIN(jde-1,jte)
                  DO i = its , MIN(ide-1,ite)
                     IF ( skip_middle_points_t ( ids , ide , jds , jde , i , j , em_width , hold_ups ) ) CYCLE 
                     tslb(i,lwant,j)= ( st_input(i,lhave,j ) * ( zhave(lhave+1) - zs   (lwant) ) + &
                                        st_input(i,lhave+1,j) * ( zs   (lwant  ) - zhave(lhave) ) ) / &
                                                                ( zhave(lhave+1) - zhave(lhave) )
                  END DO
               END DO
               EXIT z_havet
            END IF
         END DO z_havet
      END DO z_wantt

      ELSE

         zhave(1) = 0.
         DO l = 1 , num_st_levels_input
            zhave(l+1) = st_levels_input(l) / 100.
         END DO
         zhave(num_st_levels_input+2) = 300. / 100.

      

      z_wantt_2 : DO lwant = 1 , num_soil_layers
         z_havet_2 : DO lhave = 1 , num_st_levels_input +2
            IF ( ( zs(lwant) .GE. zhave(lhave  ) ) .AND. &
                 ( zs(lwant) .LE. zhave(lhave+1) ) ) THEN
               DO j = jts , MIN(jde-1,jte)
                  DO i = its , MIN(ide-1,ite)
                     IF ( skip_middle_points_t ( ids , ide , jds , jde , i , j , em_width , hold_ups ) ) CYCLE 
                     tslb(i,lwant,j)= ( st_input(i,lhave,j ) * ( zhave(lhave+1) - zs   (lwant) ) + &
                                        st_input(i,lhave+1,j) * ( zs   (lwant  ) - zhave(lhave) ) ) / &
                                                                ( zhave(lhave+1) - zhave(lhave) )
                  END DO
               END DO
               EXIT z_havet_2
            END IF
         END DO z_havet_2
      END DO z_wantt_2

      END IF

      

      IF ( flag_soil_levels .EQ. 1 ) THEN
         DO l = 1 , num_sm_levels_input
            zhave(l) = sm_levels_input(l) / 100.
         END DO

      

      z_wantm : DO lwant = 1 , num_soil_layers
         z_havem : DO lhave = 1 , num_sm_levels_input -1
            IF ( ( zs(lwant) .GE. zhave(lhave  ) ) .AND. &
                 ( zs(lwant) .LE. zhave(lhave+1) ) ) THEN
               DO j = jts , MIN(jde-1,jte)
                  DO i = its , MIN(ide-1,ite)
                     IF ( skip_middle_points_t ( ids , ide , jds , jde , i , j , em_width , hold_ups ) ) CYCLE 
                     smois(i,lwant,j)= ( sm_input(i,lhave,j ) * ( zhave(lhave+1) - zs   (lwant) ) + &
                                         sm_input(i,lhave+1,j) * ( zs   (lwant  ) - zhave(lhave) ) ) / &
                                                                 ( zhave(lhave+1) - zhave(lhave) )
                  END DO
               END DO
               EXIT z_havem
            END IF
         END DO z_havem
      END DO z_wantm

      ELSE

         zhave(1) = 0.
         DO l = 1 , num_sm_levels_input
            zhave(l+1) = sm_levels_input(l) / 100.
         END DO
         zhave(num_sm_levels_input+2) = 300. / 100.

      z_wantm_2 : DO lwant = 1 , num_soil_layers
         z_havem_2 : DO lhave = 1 , num_sm_levels_input +2
            IF ( ( zs(lwant) .GE. zhave(lhave  ) ) .AND. &
                 ( zs(lwant) .LE. zhave(lhave+1) ) ) THEN
               DO j = jts , MIN(jde-1,jte)
                  DO i = its , MIN(ide-1,ite)
                     IF ( skip_middle_points_t ( ids , ide , jds , jde , i , j , em_width , hold_ups ) ) CYCLE 
                     smois(i,lwant,j)= ( sm_input(i,lhave,j ) * ( zhave(lhave+1) - zs   (lwant) ) + &
                                         sm_input(i,lhave+1,j) * ( zs   (lwant  ) - zhave(lhave) ) ) / &
                                                                 ( zhave(lhave+1) - zhave(lhave) )
                  END DO
               END DO
               EXIT z_havem_2
            END IF
         END DO z_havem_2
      END DO z_wantm_2

      END IF
      
      

      IF ( flag_sst .EQ. 1 ) THEN
         DO j = jts , MIN(jde-1,jte)
            DO i = its , MIN(ide-1,ite)
               IF ( skip_middle_points_t ( ids , ide , jds , jde , i , j , em_width , hold_ups ) ) CYCLE 
               IF ( landmask(i,j) .LT. 0.5 ) THEN
                  DO l = 1 , num_soil_layers
                     tslb(i,l,j) = sst(i,j)
                     tsk(i,j)    = sst(i,j)
                     smois(i,l,j)= 1.0
                  END DO
               END IF
            END DO
         END DO
      ELSE
         DO j = jts , MIN(jde-1,jte)
            DO i = its , MIN(ide-1,ite)
               IF ( skip_middle_points_t ( ids , ide , jds , jde , i , j , em_width , hold_ups ) ) CYCLE 
               IF ( landmask(i,j) .LT. 0.5 ) THEN
                  DO l = 1 , num_soil_layers
                     tslb(i,l,j)= tsk(i,j)
                     smois(i,l,j)= 1.0
                  END DO
               END IF
            END DO
         END DO
      END IF

      DEALLOCATE (zhave)

   END SUBROUTINE init_soil_3_real

  SUBROUTINE init_soil_4_real ( tsk , tmn , smois , sh2o , tslb , &
                                 st_input , sm_input , sw_input , landmask , sst , &
                                 zs , dzs , &
                                 st_levels_input , sm_levels_input , sw_levels_input , &
                                 num_soil_layers , num_st_levels_input , num_sm_levels_input ,  num_sw_levels_input , &
                                 num_st_levels_alloc , num_sm_levels_alloc , num_sw_levels_alloc , &
                                 flag_sst , flag_soil_layers , flag_soil_levels , &
                                 ids , ide , jds , jde , kds , kde , &
                                 ims , ime , jms , jme , kms , kme , &
                                 its , ite , jts , jte , kts , kte )

      IMPLICIT NONE

      INTEGER , INTENT(IN) :: num_soil_layers , &
                              num_st_levels_input , num_sm_levels_input , num_sw_levels_input , &
                              num_st_levels_alloc , num_sm_levels_alloc , num_sw_levels_alloc , &
                              ids , ide , jds , jde , kds , kde , &
                              ims , ime , jms , jme , kms , kme , &
                              its , ite , jts , jte , kts , kte

      INTEGER , INTENT(IN) :: flag_sst, flag_soil_layers, flag_soil_levels

      INTEGER , DIMENSION(1:num_st_levels_input) , INTENT(INOUT) :: st_levels_input
      INTEGER , DIMENSION(1:num_sm_levels_input) , INTENT(INOUT) :: sm_levels_input
      INTEGER , DIMENSION(1:num_sw_levels_input) , INTENT(INOUT) :: sw_levels_input

      REAL , DIMENSION(ims:ime,1:num_st_levels_alloc,jms:jme) , INTENT(INOUT) :: st_input
      REAL , DIMENSION(ims:ime,1:num_sm_levels_alloc,jms:jme) , INTENT(INOUT) :: sm_input
      REAL , DIMENSION(ims:ime,1:num_sw_levels_alloc,jms:jme) , INTENT(INOUT) :: sw_input
      REAL , DIMENSION(ims:ime,jms:jme) , INTENT(IN) :: landmask , sst

      REAL , DIMENSION(ims:ime,jms:jme) , INTENT(IN) :: tmn
      REAL , DIMENSION(ims:ime,jms:jme) , INTENT(INOUT) :: tsk
      REAL , DIMENSION(num_soil_layers) :: zs , dzs

      REAL , DIMENSION(ims:ime,num_soil_layers,jms:jme) , INTENT(OUT) :: tslb , smois , sh2o

      REAL , ALLOCATABLE , DIMENSION(:) :: zhave

      INTEGER :: i , j , l , lout , lin , lwant , lhave , num
      REAL :: temp
      LOGICAL :: found_levels

      CHARACTER (LEN=132) :: message

      
      

      num = num_st_levels_input * num_sm_levels_input

      IF ( num .GE. 1 ) THEN

         


         IF ( flag_soil_levels == 1 ) THEN
           write(message, FMT='(A)') ' Assume CLM input'
           CALL wrf_message ( message )
           ALLOCATE ( zhave( MAX(num_st_levels_input,num_sm_levels_input,num_sw_levels_input)  ) )
         ELSE
           write(message, FMT='(A)') ' Assume non-CLM input'
           CALL wrf_message ( message )
        ALLOCATE ( zhave( MAX(num_st_levels_input,num_sm_levels_input,num_sw_levels_input) +2) )
      
         END IF


         

         outert : DO lout = 1 , num_st_levels_input-1
            innert : DO lin = lout+1 , num_st_levels_input
               IF ( st_levels_input(lout) .GT. st_levels_input(lin) ) THEN
                  temp = st_levels_input(lout)
                  st_levels_input(lout) = st_levels_input(lin)
                  st_levels_input(lin) = NINT(temp)
                  DO j = jts , MIN(jde-1,jte)
                     DO i = its , MIN(ide-1,ite)
                        temp = st_input(i,lout+1,j)
                        st_input(i,lout+1,j) = st_input(i,lin+1,j)
                        st_input(i,lin+1,j) = temp
                     END DO
                  END DO
               END IF
            END DO innert
         END DO outert

      IF ( flag_soil_layers == 1 ) THEN
         DO j = jts , MIN(jde-1,jte)
            DO i = its , MIN(ide-1,ite)
               st_input(i,1,j) = tsk(i,j)
               st_input(i,num_st_levels_input+2,j) = tmn(i,j)
            END DO
         END DO
      ENDIF
        

        outerm: DO lout = 1 , num_sm_levels_input-1
            innerm : DO lin = lout+1 , num_sm_levels_input
               IF ( sm_levels_input(lout) .GT. sm_levels_input(lin) ) THEN
                  temp = sm_levels_input(lout)
                  sm_levels_input(lout) = sm_levels_input(lin)
                  sm_levels_input(lin) = NINT(temp)
                  DO j = jts , MIN(jde-1,jte)
                     DO i = its , MIN(ide-1,ite)
                        temp = sm_input(i,lout+1,j)
                        sm_input(i,lout+1,j) = sm_input(i,lin+1,j)
                        sm_input(i,lin+1,j) = temp
                     END DO
                  END DO
               END IF
            END DO innerm
         END DO outerm

      IF ( flag_soil_layers == 1 ) THEN
         DO j = jts , MIN(jde-1,jte)
            DO i = its , MIN(ide-1,ite)
               sm_input(i,1,j) = sm_input(i,2,j)
               sm_input(i,num_sm_levels_input+2,j) = sm_input(i,num_sm_levels_input+1,j)
            END DO
         END DO
      ENDIF

         

         outerw: DO lout = 1 , num_sw_levels_input-1
            innerw : DO lin = lout+1 , num_sw_levels_input
               IF ( sw_levels_input(lout) .GT. sw_levels_input(lin) ) THEN
                  temp = sw_levels_input(lout)
                  sw_levels_input(lout) = sw_levels_input(lin)
                  sw_levels_input(lin) = NINT(temp)
                  DO j = jts , MIN(jde-1,jte)
                     DO i = its , MIN(ide-1,ite)
                        temp = sw_input(i,lout+1,j)
                        sw_input(i,lout+1,j) = sw_input(i,lin+1,j)
                        sw_input(i,lin+1,j) = temp
                     END DO
                  END DO
               END IF
            END DO innerw
         END DO outerw
         IF ( num_sw_levels_input .GT. 1 ) THEN
            DO j = jts , MIN(jde-1,jte)
               DO i = its , MIN(ide-1,ite)
                  sw_input(i,1,j) = sw_input(i,2,j)
                  sw_input(i,num_sw_levels_input+2,j) = sw_input(i,num_sw_levels_input+1,j)
               END DO
            END DO
         END IF

         found_levels = .TRUE.

      ELSE IF ( ( num .LE. 0 ) .AND. (  start_date .NE. current_date ) ) THEN

         found_levels = .FALSE.

      ELSE
         CALL wrf_error_fatal3("<stdin>",2207,&
         'No input soil level data (temperature, moisture or liquid, or all are missing). Required for LSM.' )
      END IF

      

      IF ( found_levels ) THEN

         

         IF ( flag_soil_levels == 1 ) THEN
            DO l = 1 , num_st_levels_input
               zhave(l) = st_levels_input(l) / 100.
            END DO

         

         z_wantt : DO lwant = 1 , num_soil_layers
            z_havet : DO lhave = 1 , num_st_levels_input -1
               IF ( ( zs(lwant) .GE. zhave(lhave  ) ) .AND. &
                    ( zs(lwant) .LE. zhave(lhave+1) ) ) THEN
                  DO j = jts , MIN(jde-1,jte)
                     DO i = its , MIN(ide-1,ite)
                        tslb(i,lwant,j)= ( st_input(i,lhave,j ) * ( zhave(lhave+1) - zs   (lwant) ) + &
                                           st_input(i,lhave+1,j) * ( zs   (lwant) - zhave(lhave) ) ) / &
                                                                   ( zhave(lhave+1) - zhave(lhave) )
                     END DO
                  END DO
                  EXIT z_havet
               END IF
            END DO z_havet
         END DO z_wantt

         ELSE

         
         

         zhave(1) = 0.
         DO l = 1 , num_st_levels_input
            zhave(l+1) = st_levels_input(l) / 100.
         END DO
         zhave(num_st_levels_input+2) = 300. / 100.

         

         z_wantt_2: DO lwant = 1 , num_soil_layers
            z_havet_2 : DO lhave = 1 , num_st_levels_input +2 -1
               IF ( ( zs(lwant) .GE. zhave(lhave  ) ) .AND. &
                    ( zs(lwant) .LE. zhave(lhave+1) ) ) THEN
                  DO j = jts , MIN(jde-1,jte)
                     DO i = its , MIN(ide-1,ite)
                        tslb(i,lwant,j)= ( st_input(i,lhave  ,j) * ( zhave(lhave+1) - zs   (lwant) ) + &
                                           st_input(i,lhave+1,j) * ( zs   (lwant) - zhave(lhave) ) ) / &
                                                                   ( zhave(lhave+1) - zhave(lhave) )
                     END DO
                  END DO
                  EXIT z_havet_2
               END IF
            END DO z_havet_2
         END DO z_wantt_2

      END IF


      IF ( flag_soil_levels == 1 ) THEN
         DO l = 1 , num_sm_levels_input
            zhave(l) = sm_levels_input(l) / 100.
         END DO

      

      z_wantm : DO lwant = 1 , num_soil_layers
         z_havem : DO lhave = 1 , num_sm_levels_input -1
            IF ( ( zs(lwant) .GE. zhave(lhave  ) ) .AND. &
                 ( zs(lwant) .LE. zhave(lhave+1) ) ) THEN
               DO j = jts , MIN(jde-1,jte)
                  DO i = its , MIN(ide-1,ite)
                     smois(i,lwant,j)= ( sm_input(i,lhave,j ) * ( zhave(lhave+1) - zs   (lwant) ) + &
                                         sm_input(i,lhave+1,j) * ( zs   (lwant) - zhave(lhave) ) ) / &
                                                                 ( zhave(lhave+1) - zhave(lhave) )
                    if(smois(i,lwant,j)<=0.0) smois(i,lwant,j) = 0.005
                  END DO
               END DO
               EXIT z_havem
            END IF
         END DO z_havem
      END DO z_wantm

      ELSE
         
         
         
         

         zhave(1) = 0.
         DO l = 1 , num_sm_levels_input
            zhave(l+1) = sm_levels_input(l) / 100.
         END DO
         zhave(num_sm_levels_input+2) = 300. / 100.

         

         z_wantm_2 : DO lwant = 1 , num_soil_layers
            z_havem_2 : DO lhave = 1 , num_sm_levels_input +2 -1
               IF ( ( zs(lwant) .GE. zhave(lhave  ) ) .AND. &
                    ( zs(lwant) .LE. zhave(lhave+1) ) ) THEN
                  DO j = jts , MIN(jde-1,jte)
                     DO i = its , MIN(ide-1,ite)
                        smois(i,lwant,j)= ( sm_input(i,lhave  ,j) * ( zhave(lhave+1) - zs   (lwant) ) + &
                                            sm_input(i,lhave+1,j) * ( zs (lwant  ) - zhave(lhave) ) ) / &
                                                                    ( zhave(lhave+1) - zhave(lhave) )
                     if(smois(i,lwant,j)<=0.0) smois(i,lwant,j) = 0.005
                     END DO
                  END DO
                  EXIT z_havem_2
               END IF
            END DO z_havem_2
         END DO z_wantm_2
       ENDIF

         

         IF ( num_sw_levels_input .GT. 1 ) THEN

            zhave(1) = 0.
            DO l = 1 , num_sw_levels_input
               zhave(l+1) = sw_levels_input(l) / 100.
            END DO
            zhave(num_sw_levels_input+2) = 300. / 100.

            

            z_wantw : DO lwant = 1 , num_soil_layers
               z_havew : DO lhave = 1 , num_sw_levels_input +2 -1
                  IF ( ( zs(lwant) .GE. zhave(lhave  ) ) .AND. &
                       ( zs(lwant) .LE. zhave(lhave+1) ) ) THEN
                     DO j = jts , MIN(jde-1,jte)
                        DO i = its , MIN(ide-1,ite)
                           sh2o(i,lwant,j)= ( sw_input(i,lhave  ,j) * ( zhave(lhave+1) - zs   (lwant) ) + &
                                               sw_input(i,lhave+1,j) * ( zs (lwant  ) - zhave(lhave) ) ) / &
                                                                       ( zhave(lhave+1) - zhave(lhave) )
                        END DO
                     END DO
                     EXIT z_havew
                  END IF
               END DO z_havew
            END DO z_wantw

         END IF


         
         

         IF ( flag_sst .EQ. 1 ) THEN
            DO j = jts , MIN(jde-1,jte)
               DO i = its , MIN(ide-1,ite)
                  IF ( landmask(i,j) .LT. 0.5 ) THEN
                     DO l = 1 , num_soil_layers
                        tslb(i,l,j)= sst(i,j)
                        smois(i,l,j)= 1.0
                        sh2o (i,l,j)= 1.0
                     END DO
                  END IF
               END DO
            END DO
         ELSE
            DO j = jts , MIN(jde-1,jte)
               DO i = its , MIN(ide-1,ite)
                  IF ( landmask(i,j) .LT. 0.5 ) THEN
                     DO l = 1 , num_soil_layers
                        tslb(i,l,j)= tsk(i,j)
                        smois(i,l,j)= 1.0
                        sh2o (i,l,j)= 1.0
                     END DO
                  END IF
               END DO
            END DO
         END IF

         DEALLOCATE (zhave)

      END IF

   END SUBROUTINE init_soil_4_real

   SUBROUTINE init_soil_7_real ( tsk , tmn , smois , sh2o , tslb , &
                                 st_input , sm_input , sw_input , landmask , sst ,     &
                                 zs , dzs ,                                            &
                                 st_levels_input , sm_levels_input , sw_levels_input , &
                                 num_soil_layers , num_st_levels_input ,               &
                                 num_sm_levels_input ,  num_sw_levels_input ,          &
                                 num_st_levels_alloc , num_sm_levels_alloc ,           &
                                 num_sw_levels_alloc ,                                 &
                                 flag_sst , flag_soil_layers , flag_soil_levels ,      &
                                 ids , ide , jds , jde , kds , kde ,                   &
                                 ims , ime , jms , jme , kms , kme ,                   &
                                 its , ite , jts , jte , kts , kte )

      

      IMPLICIT NONE

      INTEGER , INTENT(IN) :: num_soil_layers , &
                              num_st_levels_input , num_sm_levels_input , num_sw_levels_input , &
                              num_st_levels_alloc , num_sm_levels_alloc , num_sw_levels_alloc , &
                              ids , ide , jds , jde , kds , kde , &
                              ims , ime , jms , jme , kms , kme , &
                              its , ite , jts , jte , kts , kte

      INTEGER , INTENT(IN) :: flag_sst, flag_soil_layers, flag_soil_levels

      INTEGER , DIMENSION(1:num_st_levels_input) , INTENT(INOUT) :: st_levels_input
      INTEGER , DIMENSION(1:num_sm_levels_input) , INTENT(INOUT) :: sm_levels_input
      INTEGER , DIMENSION(1:num_sw_levels_input) , INTENT(INOUT) :: sw_levels_input

      REAL , DIMENSION(ims:ime,1:num_st_levels_alloc,jms:jme) , INTENT(INOUT) :: st_input
      REAL , DIMENSION(ims:ime,1:num_sm_levels_alloc,jms:jme) , INTENT(INOUT) :: sm_input
      REAL , DIMENSION(ims:ime,1:num_sw_levels_alloc,jms:jme) , INTENT(INOUT) :: sw_input
      REAL , DIMENSION(ims:ime,jms:jme) , INTENT(IN) :: landmask , sst

      REAL , DIMENSION(ims:ime,jms:jme) , INTENT(IN) :: tmn
      REAL , DIMENSION(ims:ime,jms:jme) , INTENT(INOUT) :: tsk
      REAL , DIMENSION(num_soil_layers) :: zs , dzs

      REAL , DIMENSION(ims:ime,num_soil_layers,jms:jme) , INTENT(OUT) :: tslb , smois , sh2o

      REAL , ALLOCATABLE , DIMENSION(:) :: zhave

      INTEGER :: i , j , l , lout , lin , lwant , lhave , num
      REAL    :: temp
      LOGICAL :: found_levels

      

      num = num_st_levels_input * num_sm_levels_input

      IF ( num .GE. 1 ) THEN

         

         ALLOCATE ( zhave( MAX(num_st_levels_input,num_sm_levels_input,num_sw_levels_input) +2) )

         
         outert : DO lout = 1 , num_st_levels_input-1
            innert : DO lin = lout+1 , num_st_levels_input
               IF ( st_levels_input(lout) .GT. st_levels_input(lin) ) THEN
                  temp = st_levels_input(lout)
                  st_levels_input(lout) = st_levels_input(lin)
                  st_levels_input(lin) = NINT(temp)
                  DO j = jts , MIN(jde-1,jte)
                     DO i = its , MIN(ide-1,ite)
                        IF ( skip_middle_points_t ( ids , ide , jds , jde , i , j , em_width , hold_ups ) ) CYCLE 
                        temp = st_input(i,lout+1,j)
                        st_input(i,lout+1,j) = st_input(i,lin+1,j)
                        st_input(i,lin+1,j) = temp
                     END DO
                  END DO
               END IF
            END DO innert
         END DO outert
         DO j = jts , MIN(jde-1,jte)
            DO i = its , MIN(ide-1,ite)
               IF ( skip_middle_points_t ( ids , ide , jds , jde , i , j , em_width , hold_ups ) ) CYCLE 
               st_input(i,1,j) = tsk(i,j)
               st_input(i,num_st_levels_input+2,j) = tmn(i,j)
            END DO
         END DO

         

         outerm: DO lout = 1 , num_sm_levels_input-1
            innerm : DO lin = lout+1 , num_sm_levels_input
              IF ( sm_levels_input(lout) .GT. sm_levels_input(lin) ) THEN
                  temp = sm_levels_input(lout)
                  sm_levels_input(lout) = sm_levels_input(lin)
                  sm_levels_input(lin) = NINT(temp)
                  DO j = jts , MIN(jde-1,jte)
                     DO i = its , MIN(ide-1,ite)
                        IF ( skip_middle_points_t ( ids , ide , jds , jde , i , j , em_width , hold_ups ) ) CYCLE 
                        temp = sm_input(i,lout+1,j)
                        sm_input(i,lout+1,j) = sm_input(i,lin+1,j)
                        sm_input(i,lin+1,j) = temp
                     END DO
                  END DO
               END IF
            END DO innerm
         END DO outerm
         DO j = jts , MIN(jde-1,jte)
            DO i = its , MIN(ide-1,ite)
               IF ( skip_middle_points_t ( ids , ide , jds , jde , i , j , em_width , hold_ups ) ) CYCLE 
               sm_input(i,1,j) = sm_input(i,2,j)
               sm_input(i,num_sm_levels_input+2,j) = sm_input(i,num_sm_levels_input+1,j)
            END DO
         END DO

         

         outerw: DO lout = 1 , num_sw_levels_input-1
            innerw : DO lin = lout+1 , num_sw_levels_input
               IF ( sw_levels_input(lout) .GT. sw_levels_input(lin) ) THEN
                  temp = sw_levels_input(lout)
                  sw_levels_input(lout) = sw_levels_input(lin)
                  sw_levels_input(lin) = NINT(temp)
                  DO j = jts , MIN(jde-1,jte)
                     DO i = its , MIN(ide-1,ite)
                        IF ( skip_middle_points_t ( ids , ide , jds , jde , i , j , em_width , hold_ups ) ) CYCLE 
                        temp = sw_input(i,lout+1,j)
                        sw_input(i,lout+1,j) = sw_input(i,lin+1,j)
                        sw_input(i,lin+1,j) = temp
                     END DO
                  END DO
               END IF
            END DO innerw
         END DO outerw
        IF ( num_sw_levels_input .GT. 1 ) THEN
            DO j = jts , MIN(jde-1,jte)
               DO i = its , MIN(ide-1,ite)
                  IF ( skip_middle_points_t ( ids , ide , jds , jde , i , j , em_width , hold_ups ) ) CYCLE 
                  sw_input(i,1,j) = sw_input(i,2,j)
                  sw_input(i,num_sw_levels_input+2,j) = sw_input(i,num_sw_levels_input+1,j)
               END DO
            END DO
         END IF

         found_levels = .TRUE.

      ELSE IF ( ( num .LE. 0 ) .AND. (  start_date .NE. current_date ) ) THEN

         found_levels = .FALSE.

      ELSE
         CALL wrf_error_fatal3("<stdin>",2540,&
         'No input soil level data (temperature, moisture or liquid, or all are missing). Required for PX LSM.' )
      END IF

      

      IF ( found_levels ) THEN

         
         

         zhave(1) = 0.
         DO l = 1 , num_st_levels_input
            zhave(l+1) = st_levels_input(l) / 100.
        END DO
         zhave(num_st_levels_input+2) = 300. / 100.

         

         z_wantt : DO lwant = 1 , num_soil_layers
            z_havet : DO lhave = 1 , num_st_levels_input +2 -1
               IF ( ( zs(lwant) .GE. zhave(lhave  ) ) .AND. &
                    ( zs(lwant) .LE. zhave(lhave+1) ) ) THEN
                  DO j = jts , MIN(jde-1,jte)
                     DO i = its , MIN(ide-1,ite)
                        IF ( skip_middle_points_t ( ids , ide , jds , jde , i , j , em_width , hold_ups ) ) CYCLE 
                        tslb(i,lwant,j)= ( st_input(i,lhave  ,j) * ( zhave(lhave+1) - zs   (lwant) ) + &
                                           st_input(i,lhave+1,j) * ( zs   (lwant  ) - zhave(lhave) ) ) / &
                                                                   ( zhave(lhave+1) - zhave(lhave) )
                     END DO
                  END DO
                  EXIT z_havet
               END IF
            END DO z_havet
         END DO z_wantt

         
         
         
         

         zhave(1) = 0.
         DO l = 1 , num_sm_levels_input
            zhave(l+1) = sm_levels_input(l) / 100.
         END DO
        zhave(num_sm_levels_input+2) = 300. / 100.

         

         z_wantm : DO lwant = 1 , num_soil_layers
            z_havem : DO lhave = 1 , num_sm_levels_input +2 -1
               IF ( ( zs(lwant) .GE. zhave(lhave  ) ) .AND. &
                    ( zs(lwant) .LE. zhave(lhave+1) ) ) THEN
                  DO j = jts , MIN(jde-1,jte)
                     DO i = its , MIN(ide-1,ite)
                        IF ( skip_middle_points_t ( ids , ide , jds , jde , i , j , em_width , hold_ups ) ) CYCLE 
                        smois(i,lwant,j)= ( sm_input(i,lhave  ,j) * ( zhave(lhave+1) - zs   (lwant) ) + &
                                            sm_input(i,lhave+1,j) * ( zs   (lwant  ) - zhave(lhave) ) ) / &
                                                                    ( zhave(lhave+1) - zhave(lhave) )
                     END DO
                  END DO
                  EXIT z_havem
               END IF
            END DO z_havem
         END DO z_wantm

         

         IF ( num_sw_levels_input .GT. 1 ) THEN

            zhave(1) = 0.
            DO l = 1 , num_sw_levels_input
               zhave(l+1) = sw_levels_input(l) / 100.
            END DO
            zhave(num_sw_levels_input+2) = 300. / 100.

          

            z_wantw : DO lwant = 1 , num_soil_layers
               z_havew : DO lhave = 1 , num_sw_levels_input +2 -1
                  IF ( ( zs(lwant) .GE. zhave(lhave  ) ) .AND. &
                       ( zs(lwant) .LE. zhave(lhave+1) ) ) THEN
                     DO j = jts , MIN(jde-1,jte)
                        DO i = its , MIN(ide-1,ite)
                           IF ( skip_middle_points_t ( ids , ide , jds , jde , i , j , em_width , hold_ups ) ) CYCLE 
                           sh2o(i,lwant,j)= ( sw_input(i,lhave  ,j) * ( zhave(lhave+1) - zs   (lwant) ) + &
                                               sw_input(i,lhave+1,j) * ( zs   (lwant  ) - zhave(lhave) ) ) / &
                                                                       ( zhave(lhave+1) - zhave(lhave) )
                        END DO
                     END DO
                     EXIT z_havew
                  END IF
               END DO z_havew
            END DO z_wantw

         END IF


        
        

     DO j = jts , MIN(jde-1,jte)
        DO i = its , MIN(ide-1,ite)
             IF ( skip_middle_points_t ( ids , ide , jds , jde , i , j , em_width , hold_ups ) ) CYCLE 
             tslb(i,1,j)= tsk(i,j)
             tslb(i,2,j)= tmn(i,j)
        END DO
     END DO

         IF ( flag_sst .EQ. 1 ) THEN
            DO j = jts , MIN(jde-1,jte)
               DO i = its , MIN(ide-1,ite)
                  IF ( skip_middle_points_t ( ids , ide , jds , jde , i , j , em_width , hold_ups ) ) CYCLE 
                  IF ( landmask(i,j) .LT. 0.5 ) THEN
                     DO l = 1 , num_soil_layers
                        tslb(i,l,j)= sst(i,j)
                        smois(i,l,j)= 1.0
                        sh2o (i,l,j)= 1.0
                     END DO
                  END IF
               END DO
            END DO
         ELSE
            DO j = jts , MIN(jde-1,jte)
               DO i = its , MIN(ide-1,ite)
                  IF ( skip_middle_points_t ( ids , ide , jds , jde , i , j , em_width , hold_ups ) ) CYCLE 
                  IF ( landmask(i,j) .LT. 0.5 ) THEN
                     DO l = 1 , num_soil_layers
                        tslb(i,l,j)= tsk(i,j)
                        smois(i,l,j)= 1.0
                        sh2o (i,l,j)= 1.0
                     END DO
                  END IF
               END DO
            END DO
         END IF

         DEALLOCATE (zhave)

      END IF

   END SUBROUTINE init_soil_7_real

   SUBROUTINE init_soil_8_real ( tsk , tmn , smois , sh2o , tslb , &
                                 st_input , sm_input , sw_input , landmask , sst , &
                                 zs , dzs , &
                                 st_levels_input , sm_levels_input , sw_levels_input , &
           num_soil_layers , num_st_levels_input , num_sm_levels_input ,  num_sw_levels_input , &
                             num_st_levels_alloc , num_sm_levels_alloc ,  num_sw_levels_alloc , &
                                 flag_sst , flag_soil_layers , flag_soil_levels , &
                                 st000010, st010200, sm000010, sm010200, &
                                 st010040 , st040100 , st100200, &
                                 sm010040 , sm040100 , sm100200, &
                                 ids , ide , jds , jde , kds , kde , &
                                 ims , ime , jms , jme , kms , kme , &
                                 its , ite , jts , jte , kts , kte )

      IMPLICIT NONE

      INTEGER , INTENT(IN) :: num_soil_layers , &
                              num_st_levels_input , num_sm_levels_input , num_sw_levels_input , &
                              num_st_levels_alloc , num_sm_levels_alloc , num_sw_levels_alloc , &
                              ids , ide , jds , jde , kds , kde , &
                              ims , ime , jms , jme , kms , kme , &
                              its , ite , jts , jte , kts , kte

      INTEGER , INTENT(IN) :: flag_sst, flag_soil_layers , flag_soil_levels

      INTEGER , DIMENSION(1:num_st_levels_input) , INTENT(INOUT) :: st_levels_input
      INTEGER , DIMENSION(1:num_sm_levels_input) , INTENT(INOUT) :: sm_levels_input
      INTEGER , DIMENSION(1:num_sw_levels_input) , INTENT(INOUT) :: sw_levels_input

      REAL , DIMENSION(ims:ime,1:num_st_levels_alloc,jms:jme) , INTENT(INOUT) :: st_input
      REAL , DIMENSION(ims:ime,1:num_sm_levels_alloc,jms:jme) , INTENT(INOUT) :: sm_input
      REAL , DIMENSION(ims:ime,1:num_sw_levels_alloc,jms:jme) , INTENT(INOUT) :: sw_input
      REAL , DIMENSION(ims:ime,jms:jme) , INTENT(IN) :: landmask , sst
      REAL , DIMENSION(ims:ime,jms:jme) , INTENT(IN) :: st000010, st010200, st010040 , st040100 , st100200
      REAL , DIMENSION(ims:ime,jms:jme) , INTENT(IN) :: sm000010, sm010200, sm010040 , sm040100 , sm100200

      REAL , DIMENSION(ims:ime,jms:jme) , INTENT(IN) :: tmn
      REAL , DIMENSION(ims:ime,jms:jme) , INTENT(INOUT) :: tsk
      REAL , DIMENSION(num_soil_layers) :: zs , dzs

      REAL , DIMENSION(ims:ime,num_soil_layers,jms:jme) , INTENT(OUT) :: tslb , smois , sh2o

      REAL , ALLOCATABLE , DIMENSION(:) :: zhave

      INTEGER :: i , j , l , lout , lin , lwant , lhave , num
      REAL :: temp
      LOGICAL :: found_levels

      found_levels = .false.




      IF ( flag_soil_layers == 1 ) THEN
                  DO j = jts , MIN(jde-1,jte)
                     DO i = its , MIN(ide-1,ite)
                        tslb(i,1,j) = st000010(i,j)
                        tslb(i,2,j) = st010040(i,j)
                        tslb(i,3,j) = st040100(i,j)
                        smois(i,1,j) = sm000010(i,j)
                        smois(i,2,j) = sm010040(i,j)
                        smois(i,3,j) = sm040100(i,j)

                        if(tslb(i,2,j) .lt. 10.)tslb(i,2,j) = st000010(i,j)
                        if(smois(i,2,j) .lt. 0.01)smois(i,2,j) = sm000010(i,j)
                        if(tslb(i,3,j) .lt. 10.)tslb(i,3,j) = st010200(i,j)
                        if(smois(i,3,j) .lt. 0.01)smois(i,3,j) = sm010200(i,j)
                     END DO
                  END DO
      ELSE
          CALL wrf_debug ( 0 , 'SSiB LSM needs 0-10 cm soil t')
          CALL wrf_debug ( 0 , 'SSiB LSM needs 0-10 cm soil m')
          CALL wrf_debug ( 0 , 'SSiB LSM needs 10-200 cm soil t')
          CALL wrf_debug ( 0 , 'SSiB LSM needs 10-200 cm soil m')
      ENDIF

         
         

         IF ( flag_sst .EQ. 1 ) THEN
            DO j = jts , MIN(jde-1,jte)
               DO i = its , MIN(ide-1,ite)
                  IF ( landmask(i,j) .LT. 0.5 ) THEN
                     DO l = 1 , num_soil_layers
                        tslb(i,l,j)= sst(i,j)
                        smois(i,l,j)= 1.0
                        sh2o (i,l,j)= 1.0
                     END DO
                  END IF
               END DO
            END DO
         ELSE
            DO j = jts , MIN(jde-1,jte)
               DO i = its , MIN(ide-1,ite)
                  IF ( landmask(i,j) .LT. 0.5 ) THEN
                     DO l = 1 , num_soil_layers
                        tslb(i,l,j)= tsk(i,j)
                        smois(i,l,j)= 1.0
                        sh2o (i,l,j)= 1.0
                     END DO
                  END IF
               END DO
            END DO
         END IF

   END SUBROUTINE init_soil_8_real


   SUBROUTINE aggregate_categories_part1  ( landuse_frac , iswater , num_veg_cat , mminlu )
      IMPLICIT NONE
      INTEGER , INTENT(IN) :: iswater
      INTEGER , INTENT(IN) :: num_veg_cat
      CHARACTER (LEN=4) , INTENT(IN) :: mminlu
      REAL , DIMENSION(1:num_veg_cat) , INTENT(INOUT):: landuse_frac

      

      INTEGER , PARAMETER :: num_special_bins =  3 
      INTEGER , PARAMETER :: max_cats_per_bin =  8 
                                                   
      INTEGER , PARAMETER :: fl               = -1 
      INTEGER , DIMENSION ( max_cats_per_bin * num_special_bins ) :: cib_usgs = &
      (/  2 ,  3 ,  4 ,  5 ,  6 ,  7 , 10 , fl , & 
          8 ,  9 , fl , fl , fl , fl , fl , fl , & 
         11 , 12 , 13 , 14 , 15 , fl , fl , fl /)  
      INTEGER , DIMENSION ( max_cats_per_bin * num_special_bins ) :: cib_modis = &
      (/  9 , 10 , 12 , 14 , fl , fl , fl , fl , & 
          6 ,  7 ,  8 , fl , fl , fl , fl , fl , & 
          1 ,  2 ,  3 ,  4 ,  5 , fl , fl , fl /)  
 
      IF      ( mminlu(1:4) .EQ. 'USGS' ) THEN
         CALL aggregate_categories_part2  ( landuse_frac , iswater , num_veg_cat , &
                                            num_special_bins , max_cats_per_bin , fl , cib_usgs  )
      ELSE IF ( mminlu(1:4) .EQ. 'MODI' ) THEN
         CALL aggregate_categories_part2  ( landuse_frac , iswater , num_veg_cat , &
                                            num_special_bins , max_cats_per_bin , fl , cib_modis )
      END IF

   END SUBROUTINE aggregate_categories_part1

   SUBROUTINE aggregate_categories_part2  ( landuse_frac , iswater , num_veg_cat , &
                                            num_special_bins , max_cats_per_bin , fl , cib )
      IMPLICIT NONE

      INTEGER , INTENT(IN) :: iswater
      INTEGER , INTENT(IN) :: num_veg_cat
      REAL , DIMENSION(1:num_veg_cat) , INTENT(INOUT):: landuse_frac
      INTEGER , INTENT(IN) :: num_special_bins , max_cats_per_bin , fl
      INTEGER , DIMENSION ( max_cats_per_bin * num_special_bins ) , INTENT(IN) :: cib

      

      REAL , DIMENSION(1:num_veg_cat) :: landuse_frac_work 
      INTEGER , DIMENSION ( max_cats_per_bin , num_special_bins ) :: cats_in_bin
      INTEGER :: ib , ic 
      REAL    , DIMENSION ( num_special_bins ) :: bin_max_val , bin_sum 
      INTEGER , DIMENSION ( num_special_bins ) :: bin_max_idx 
      INTEGER :: bin_work , bin_orig 
      INTEGER :: max_cat_orig , max_cat_work
      REAL    :: max_val_orig , max_val_work

      

      DO ic = 1 , num_veg_cat
         IF ( landuse_frac(ic) .GE. 0.5 ) THEN
            RETURN
         END IF
      END DO

      

      cats_in_bin = RESHAPE ( cib , (/  max_cats_per_bin , num_special_bins /) )

      
      

      landuse_frac_work = landuse_frac

      

      DO ib = 1 , num_special_bins

         
         
         

         bin_sum    (ib) =  0
         bin_max_val(ib) = fl

         cat_loop_accumulate : DO ic = 1 , max_cats_per_bin
            
            
            
      
            IF      ( cats_in_bin(ic,ib) .EQ. fl ) THEN
               EXIT cat_loop_accumulate
            END IF

            bin_sum(ib) = bin_sum(ib) + landuse_frac(cats_in_bin(ic,ib))

            IF ( landuse_frac(cats_in_bin(ic,ib)) .GT. bin_max_val(ib) ) THEN
               bin_max_val(ib) = landuse_frac(cats_in_bin(ic,ib))
               bin_max_idx(ib) = cats_in_bin(ic,ib)
            END IF

         END DO cat_loop_accumulate

         cat_loop_assign : DO ic = 1 , max_cats_per_bin
            
            
            
      
            IF      ( cats_in_bin(ic,ib) .EQ. fl ) THEN
               EXIT cat_loop_assign
            ELSE IF ( cats_in_bin(ic,ib) .EQ. bin_max_idx(ib) ) THEN
               landuse_frac_work(cats_in_bin(ic,ib)) = bin_sum(ib)
            ELSE
               landuse_frac_work(cats_in_bin(ic,ib)) = 0
            END IF

         END DO cat_loop_assign

      END DO

      
      
      

      max_cat_orig = fl
      max_val_orig =  0
      max_cat_work = fl
      max_val_work =  0

      DO ic = 1 , num_veg_cat
         IF ( ic .EQ. iswater ) THEN
            CYCLE
         END IF
         IF ( landuse_frac(ic) .GT. max_val_orig ) THEN
            max_val_orig = landuse_frac(ic)
            max_cat_orig = ic
         END IF
         IF ( landuse_frac_work(ic) .GT. max_val_work ) THEN
            max_val_work = landuse_frac_work(ic)
            max_cat_work = ic
         END IF
      END DO

      

      bin_orig = -1
      bin_loop_orig : DO ib = 1 , num_special_bins
         cat_loop_orig : DO ic = 1 , max_cats_per_bin
            IF      ( cats_in_bin(ic,ib) .EQ. fl           ) THEN
               EXIT cat_loop_orig
            ELSE IF ( cats_in_bin(ic,ib) .EQ. max_cat_orig ) THEN
               bin_orig = ib
               EXIT bin_loop_orig
            END IF
         END DO cat_loop_orig
      END DO bin_loop_orig

      

      bin_work = -1
      bin_loop_work : DO ib = 1 , num_special_bins
         cat_loop_work : DO ic = 1 , max_cats_per_bin
            IF      ( cats_in_bin(ic,ib) .EQ. fl           ) THEN
               EXIT cat_loop_work
            ELSE IF ( cats_in_bin(ic,ib) .EQ. max_cat_work ) THEN
               bin_work = ib
               EXIT bin_loop_work
            END IF
         END DO cat_loop_work
      END DO bin_loop_work

      
      
      
      

      IF ( bin_work .EQ. bin_orig ) THEN
         
      ELSE
         DO ic = 1 , max_cats_per_bin
            landuse_frac(cats_in_bin(ic,bin_work)) = landuse_frac_work(cats_in_bin(ic,bin_work))
         END DO
      END IF
            
   END SUBROUTINE aggregate_categories_part2

END MODULE module_soil_pre

FUNCTION skip_middle_points_t ( ids , ide , jds , jde , &
                                i_in , j_in , width ,   &
                                subtleties_exist )      &
                       RESULT ( skip_it )
 
   IMPLICIT NONE

   INTEGER , INTENT(IN) :: ids , ide , jds , jde
   INTEGER , INTENT(IN) :: i_in , j_in , width
   LOGICAL , INTENT(IN) :: subtleties_exist
   
   LOGICAL              :: skip_it

   INTEGER , PARAMETER :: slop = 0

   IF ( ( subtleties_exist ) .OR. ( ide .EQ. 3 ) .OR. ( jde .EQ. 3 ) ) THEN
      skip_it = .FALSE.
   ELSE
      IF ( ( i_in .GE. ids+width+slop ) .AND. ( i_in .LE. ide-1-width-slop )   .AND. &
           ( j_in .GE. jds+width+slop ) .AND. ( j_in .LE. jde-1-width-slop ) ) THEN
         skip_it = .TRUE.
      ELSE
         skip_it = .FALSE.
      END IF
   END IF

END FUNCTION skip_middle_points_t

