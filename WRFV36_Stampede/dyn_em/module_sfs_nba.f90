








































MODULE module_sfs_nba

  USE module_configure, ONLY : grid_config_rec_type

  IMPLICIT NONE

  REAL :: c1, c2, c3, ce, cb, cs 

CONTAINS



SUBROUTINE calc_mij_constants( )







  IMPLICIT NONE

  REAL :: sk, pi 



  sk = 0.5
  pi = 3.1415927
  cb = 0.36

  cs = ( ( 8.0*( 1.0+cb ) )/( 27.0*pi**2 ) )**0.5
  c1 = ( ( 960.0**0.5 )*cb )/( 7.0*( 1.0+cb )*sk )
  c2 = c1
  ce = ( ( 8.0*pi/27.0 )**( 1.0/3.0 ) )*cs**( 4.0/3.0 )
  c3 = ( ( 27.0/( 8.0*pi ) )**( 1.0/3.0 ) )*cs**( 2.0/3.0 )

  RETURN

END SUBROUTINE calc_mij_constants



SUBROUTINE calc_smnsmn( smnsmn,                       &                
                        s11, s22, s33,                &
                        s12, s13, s23,                &
                        config_flags,                 &
                        ids, ide, jds, jde, kds, kde, &
                        ims, ime, jms, jme, kms, kme, &
                        ips, ipe, jps, jpe, kps, kpe, &
                        its, ite, jts, jte, kts, kte  )







  IMPLICIT NONE

  REAL, DIMENSION( ims:ime, kms:kme, jms:jme ),          INTENT( OUT ) &
  :: smnsmn        
 
  REAL, DIMENSION( ims:ime, kms:kme, jms:jme ),          INTENT( IN  ) &
  :: s11         & 
   , s22         & 
   , s33         & 
   , s12         & 
   , s13         & 
   , s23           

  TYPE (grid_config_rec_type),                           INTENT( IN  ) &
  :: config_flags

  INTEGER,                                               INTENT( IN  ) &
  :: ids, ide, jds, jde, kds, kde, &
     ims, ime, jms, jme, kms, kme, &
     ips, ipe, jps, jpe, kps, kpe, &
     its, ite, jts, jte, kts, kte


             
  REAL :: tmp

  INTEGER :: i, j, k, i_start, i_end, j_start, j_end, ktf







   
  ktf = min(kte,kde-1)

  i_start = its
  i_end   = MIN(ite,ide-1)
  j_start = jts
  j_end   = MIN(jte,jde-1)

  IF ( config_flags%open_xs .or. config_flags%specified .or. &
       config_flags%nested) i_start = MAX(ids+1,its)
  IF ( config_flags%open_xe .or. config_flags%specified .or. &
       config_flags%nested) i_end   = MIN(ide-2,ite)
  IF ( config_flags%open_ys .or. config_flags%specified .or. &
       config_flags%nested) j_start = MAX(jds+1,jts)
  IF ( config_flags%open_ye .or. config_flags%specified .or. &
       config_flags%nested) j_end   = MIN(jde-2,jte)
  IF ( config_flags%periodic_x ) i_start = its
  IF ( config_flags%periodic_x ) i_end = MIN( ite, ide-1 )






  DO j=j_start,j_end
  DO k=kts,ktf
  DO i=i_start,i_end

    smnsmn(i,k,j) = 0.25*( s11(i,k,j)*s11(i,k,j) + &
                           s22(i,k,j)*s22(i,k,j) + &
                           s33(i,k,j)*s33(i,k,j) )

  END DO
  END DO
  END DO




  DO j=j_start,j_end
  DO k=kts,ktf
  DO i=i_start,i_end

    tmp = 0.125*( s12(i  ,k,j) + s12(i  ,k,j+1) + &
                  s12(i+1,k,j) + s12(i+1,k,j+1) )
    smnsmn(i,k,j) = smnsmn(i,k,j) + 2.0*tmp*tmp

  END DO
  END DO
  END DO

  DO j=j_start,j_end
  DO k=kts,ktf
  DO i=i_start,i_end

    tmp = 0.125*( s13(i  ,k+1,j) + s13(i  ,k,j) + &
                  s13(i+1,k+1,j) + s13(i+1,k,j) )
    smnsmn(i,k,j) = smnsmn(i,k,j) + 2.0*tmp*tmp

  END DO
  END DO
  END DO

  DO j=j_start,j_end
  DO k=kts,ktf
  DO i=i_start,i_end

    tmp = 0.125*( s23(i,k+1,j  ) + s23(i,k,j  ) + &
                  s23(i,k+1,j+1) + s23(i,k,j+1) )
    smnsmn(i,k,j) = smnsmn(i,k,j) + 2.0*tmp*tmp
  
  END DO
  END DO
  END DO

  RETURN

END SUBROUTINE calc_smnsmn



SUBROUTINE calc_mii( m11, m22, m33,                &
                     s11, s22, s33,                &
                     s12, s13, s23,                &
                     r12, r13, r23, smnsmn,        &
                     tke, rdzw, dx, dy,            &
                     config_flags,                 &
                     ids, ide, jds, jde, kds, kde, &
                     ims, ime, jms, jme, kms, kme, &
                     ips, ipe, jps, jpe, kps, kpe, &
                     its, ite, jts, jte, kts, kte  )







  IMPLICIT NONE

  REAL, DIMENSION( ims:ime, kms:kme, jms:jme ),          INTENT( OUT ) &
  :: m11         & 
   , m22         & 
   , m33           

  REAL, DIMENSION( ims:ime, kms:kme, jms:jme ),          INTENT( IN  ) &
  :: s11         & 
   , s22         & 
   , s33         & 
   , s12         & 
   , s13         & 
   , s23         & 
   , r12         & 
   , r13         & 
   , r23         & 
   , smnsmn      & 
   , tke         & 
   , rdzw          

  REAL,                                                  INTENT( IN  ) &
  :: dx          & 
   , dy            

  TYPE (grid_config_rec_type),                           INTENT( IN  ) &
  :: config_flags

  INTEGER,                                               INTENT( IN  ) &
  :: ids, ide, jds, jde, kds, kde, &
     ims, ime, jms, jme, kms, kme, &
     ips, ipe, jps, jpe, kps, kpe, &
     its, ite, jts, jte, kts, kte



  REAL, DIMENSION( its-1:ite+1, kms:kme, jts-1:jte+1 ) & 
  :: ss11        & 
   , ss22        & 
   , ss33        & 
   , ss12        & 
   , ss13        & 
   , ss23        &          
   , rr12        & 
   , rr13        & 
   , rr23  

  REAL, DIMENSION( its-1:ite+1, kms:kme, jts-1:jte+1 ) & 
  :: ss12c       & 
   , rr12c       & 
   , ss13c       & 
   , rr13c       & 
   , ss23c       & 
   , rr23c    

  REAL :: delta, a, b

  INTEGER :: i, j, k, i_start, i_end, j_start, j_end, ktf, is_ext, js_ext








  ktf = MIN( kte, kde-1 )

  i_start = its
  i_end   = ite
  j_start = jts
  j_end   = jte

    IF ( config_flags%open_xs .OR. config_flags%specified .OR. &
         config_flags%nested) i_start = MAX( ids+1, its )
    IF ( config_flags%open_xe .OR. config_flags%specified .OR. &
         config_flags%nested) i_end   = MIN( ide-1, ite )
    IF ( config_flags%open_ys .OR. config_flags%specified .OR. &
         config_flags%nested) j_start = MAX( jds+1, jts )
    IF ( config_flags%open_ye .OR. config_flags%specified .OR. &
         config_flags%nested) j_end   = MIN( jde-1, jte )
      IF ( config_flags%periodic_x ) i_start = its
      IF ( config_flags%periodic_x ) i_end = ite

  is_ext = 1
  js_ext = 1

  i_start = i_start - is_ext  
  j_start = j_start - js_ext   







  DO j=j_start,j_end+1
  DO k=kts,ktf
  DO i=i_start,i_end+1

    ss11(i,k,j)=s11(i,k,j)/2.0
    ss22(i,k,j)=s22(i,k,j)/2.0
    ss33(i,k,j)=s33(i,k,j)/2.0
    ss12(i,k,j)=s12(i,k,j)/2.0
    ss13(i,k,j)=s13(i,k,j)/2.0
    ss23(i,k,j)=s23(i,k,j)/2.0
    rr12(i,k,j)=r12(i,k,j)/2.0
    rr13(i,k,j)=r13(i,k,j)/2.0
    rr23(i,k,j)=r23(i,k,j)/2.0

  END DO
  END DO
  END DO

  DO j=j_start,j_end+1
  DO i=i_start,i_end+1

    ss13(i,kde,j) = 0.0
    ss23(i,kde,j) = 0.0
    rr13(i,kde,j) = 0.0
    rr23(i,kde,j) = 0.0

  END DO
  END DO







  DO j = j_start, j_end
  DO k = kts, ktf
  DO i = i_start, i_end

    ss12c(i,k,j) = 0.25*( ss12(i  ,k  ,j  ) + ss12(i  ,k  ,j+1) + &
                          ss12(i+1,k  ,j  ) + ss12(i+1,k  ,j+1) )

    rr12c(i,k,j) = 0.25*( rr12(i  ,k  ,j  ) + rr12(i  ,k  ,j+1) + &
                          rr12(i+1,k  ,j  ) + rr12(i+1,k  ,j+1) )

    ss13c(i,k,j) = 0.25*( ss13(i  ,k+1,j  ) + ss13(i  ,k  ,j  ) + &
                          ss13(i+1,k+1,j  ) + ss13(i+1,k  ,j  ) )

    rr13c(i,k,j) = 0.25*( rr13(i  ,k+1,j  ) + rr13(i  ,k  ,j  ) + &
                          rr13(i+1,k+1,j  ) + rr13(i+1,k  ,j  ) )

    ss23c(i,k,j) = 0.25*( ss23(i  ,k+1,j  ) + ss23(i  ,k  ,j  ) + &
                          ss23(i  ,k+1,j+1) + ss23(i  ,k  ,j+1) )

    rr23c(i,k,j) = 0.25*( rr23(i  ,k+1,j  ) + rr23(i  ,k  ,j  ) + &
                          rr23(i  ,k+1,j+1) + rr23(i  ,k  ,j+1) )

  ENDDO
  ENDDO
  ENDDO







  IF ( config_flags%sfs_opt .EQ. 1 ) THEN 

    DO j=j_start,j_end
    DO k=kts,ktf
    DO i=i_start,i_end

      delta = ( dx * dy / rdzw(i,k,j) )**0.33333333
      a = -1.0*( cs*delta )**2

      m11(i,k,j) = a*(   2.0*sqrt( 2.0*smnsmn(i,k,j) )*ss11(i,k,j) &
                       + c1*(   ss11(i,k,j) *ss11(i,k,j)           &
                              + ss12c(i,k,j)*ss12c(i,k,j)          &
                              + ss13c(i,k,j)*ss13c(i,k,j)          &
                              - smnsmn(i,k,j)/3.0                  &
                            )                                      &
                       + c2*( -2.0*(   ss12c(i,k,j)*rr12c(i,k,j)   &
                                     + ss13c(i,k,j)*rr13c(i,k,j)   &
                                   )                               &
                            )                                      &
                     )

      m22(i,k,j) = a*(   2.0*sqrt( 2.0*smnsmn(i,k,j) )*ss22(i,k,j) &
                       + c1*(   ss22(i,k,j) *ss22(i,k,j)           &
                              + ss12c(i,k,j)*ss12c(i,k,j)          &
                              + ss23c(i,k,j)*ss23c(i,k,j)          &
                              - smnsmn(i,k,j)/3.0                  &
                            )                                      &
                       + c2*(  2.0*(   ss12c(i,k,j)*rr12c(i,k,j)   &
                                     - ss23c(i,k,j)*rr23c(i,k,j)   &
                                   )                               &
                            )                                      &
                     )

      m33(i,k,j) = a*(   2.0*sqrt( 2.0*smnsmn(i,k,j) )*ss33(i,k,j) &
                       + c1*(   ss33(i,k,j) *ss33(i,k,j)            &
                              + ss13c(i,k,j)*ss13c(i,k,j)          &
                              + ss23c(i,k,j)*ss23c(i,k,j)          &
                              - smnsmn(i,k,j)/3.0                  &
                            )                                      &
                       + c2*(  2.0*(   ss13c(i,k,j)*rr13c(i,k,j)   &
                                     + ss23c(i,k,j)*rr23c(i,k,j)   &
                                   )                               &
                            )                                      &
                     )

    ENDDO
    ENDDO
    ENDDO
 
  ELSE 

    DO j=j_start,j_end
    DO k=kts,ktf
    DO i=i_start,i_end

      delta = ( dx * dy / rdzw(i,k,j) )**0.33333333
      a = -1.0*ce*delta
      b = c3*delta

      m11(i,k,j) = a*(   2.0*sqrt( tke(i,k,j) )*ss11(i,k,j)            &
                       + b*(                                           &
                               c1*(   ss11(i,k,j) *ss11(i,k,j)         &
                                    + ss12c(i,k,j)*ss12c(i,k,j)        &
                                    + ss13c(i,k,j)*ss13c(i,k,j)        &
                                    - smnsmn(i,k,j)/3.0                &
                                          )                            &
                             + c2*( -2.0*(   ss12c(i,k,j)*rr12c(i,k,j) &
                                           + ss13c(i,k,j)*rr13c(i,k,j) &
                                         )                             &
                                  )                                    &
                           )                                           &
                     )

      m22(i,k,j) = a*(   2.0*sqrt( tke(i,k,j) )*ss22(i,k,j)            &
                       + b*(                                           &
                               c1*(   ss22(i,k,j) *ss22(i,k,j)         &
                                    + ss12c(i,k,j)*ss12c(i,k,j)        &
                                    + ss23c(i,k,j)*ss23c(i,k,j)        &
                                    - smnsmn(i,k,j)/3.0                &
                                            )                          &
                             + c2*(  2.0*(   ss12c(i,k,j)*rr12c(i,k,j) &
                                           - ss23c(i,k,j)*rr23c(i,k,j) &
                                         )                             &
                                  )                                    &
                           )                                           &
                     )

      m33(i,k,j) = a*(   2.0*sqrt( tke(i,k,j) )*ss33(i,k,j)            &
                       + b*(                                           &
                               c1*(   ss33(i,k,j) *ss33(i,k,j)         &
                                    + ss13c(i,k,j)*ss13c(i,k,j)        &
                                    + ss23c(i,k,j)*ss23c(i,k,j)        &
                                    - smnsmn(i,k,j)/3.0                &
                                  )                                    &
                             + c2*(  2.0*(   ss13c(i,k,j)*rr13c(i,k,j) &
                                           + ss23c(i,k,j)*rr23c(i,k,j) &
                                         )                             &
                                  )                                    &
                           )                                           &
                     )
 

    ENDDO
    ENDDO
    ENDDO

  ENDIF

  RETURN

END SUBROUTINE calc_mii



SUBROUTINE calc_m12( m12,                          &
                     s11, s22,                     &
                     s12, s13, s23,                &
                     r12, r13, r23, smnsmn,        &
                     tke, rdzw, dx, dy,            &
                     config_flags,                 &
                     ids, ide, jds, jde, kds, kde, &
                     ims, ime, jms, jme, kms, kme, &
                     ips, ipe, jps, jpe, kps, kpe, &
                     its, ite, jts, jte, kts, kte  )







  IMPLICIT NONE

  REAL, DIMENSION( ims:ime, kms:kme, jms:jme ),          INTENT( OUT ) &
  :: m12           

  REAL, DIMENSION( ims:ime, kms:kme, jms:jme ),          INTENT( IN  ) &
  :: s11         & 
   , s22         & 
   , s12         & 
   , s13         & 
   , s23         & 
   , r12         & 
   , r13         & 
   , r23         & 
   , smnsmn      & 
   , tke         & 
   , rdzw          

  REAL,                                                  INTENT( IN  ) &
  :: dx          & 
   , dy            

  TYPE (grid_config_rec_type),                           INTENT( IN  ) &
  :: config_flags

  INTEGER,                                               INTENT( IN  ) &
  :: ids, ide, jds, jde, kds, kde, &
     ims, ime, jms, jme, kms, kme, &
     ips, ipe, jps, jpe, kps, kpe, &
     its, ite, jts, jte, kts, kte



  REAL, DIMENSION( its-1:ite+1, kms:kme, jts-1:jte+1 ) & 
  :: ss11        & 
   , ss22        &  
   , ss12        &  
   , ss13        & 
   , ss23        & 
   , rr12        & 
   , rr13        & 
   , rr23  


  REAL, DIMENSION( its-1:ite+1, kms:kme, jts-1:jte+1 ) & 
  :: tked        & 
   , ss11d       & 
   , ss22d       & 
   , ss13d       & 
   , ss23d       & 
   , rr13d       & 
   , rr23d       &          
   , smnsmnd    

  REAL :: delta, a, b

  INTEGER :: i, j, k, i_start, i_end, j_start, j_end, ktf, je_ext, ie_ext








  ktf = MIN( kte, kde-1 )



  i_start = its
  i_end   = ite
  j_start = jts
  j_end   = jte

    IF ( config_flags%open_xs .OR. config_flags%specified .OR. &
         config_flags%nested ) i_start = MAX( ids+1, its )
    IF ( config_flags%open_xe .OR. config_flags%specified .OR. &
         config_flags%nested ) i_end   = MIN( ide-1, ite )
    IF ( config_flags%open_ys .OR. config_flags%specified .OR. &
         config_flags%nested ) j_start = MAX( jds+1, jts )
    IF ( config_flags%open_ye .OR. config_flags%specified .OR. &
         config_flags%nested ) j_end   = MIN( jde-1, jte )
      IF ( config_flags%periodic_x ) i_start = its
      IF ( config_flags%periodic_x ) i_end = ite

  je_ext = 1
  ie_ext = 1

  i_end = i_end + ie_ext  
  j_end = j_end + je_ext   







  DO j=j_start-1,j_end
  DO k=kts,ktf
  DO i=i_start-1,i_end

    ss11(i,k,j)=s11(i,k,j)/2.0
    ss22(i,k,j)=s22(i,k,j)/2.0
    ss12(i,k,j)=s12(i,k,j)/2.0
    ss13(i,k,j)=s13(i,k,j)/2.0
    ss23(i,k,j)=s23(i,k,j)/2.0
    rr12(i,k,j)=r12(i,k,j)/2.0
    rr13(i,k,j)=r13(i,k,j)/2.0
    rr23(i,k,j)=r23(i,k,j)/2.0

  END DO
  END DO
  END DO

  DO j=j_start-1,j_end
  DO i=i_start-1,i_end

    ss13(i,kde,j) = 0.0
    ss23(i,kde,j) = 0.0
    rr13(i,kde,j) = 0.0
    rr23(i,kde,j) = 0.0

  END DO
  END DO







  DO j = j_start, j_end
  DO k = kts, ktf
  DO i = i_start, i_end

    tked(i,k,j) = 0.25*( tke(i-1,k  ,j  ) + tke(i  ,k  ,j  ) + &
                         tke(i-1,k  ,j-1) + tke(i  ,k  ,j-1) )

    smnsmnd(i,k,j) = 0.25*( smnsmn(i-1,k  ,j  ) + smnsmn(i  ,k  ,j  ) + &
                            smnsmn(i-1,k  ,j-1) + smnsmn(i  ,k  ,j-1) )

    ss11d(i,k,j) = 0.25*( ss11(i-1,k  ,j  ) + ss11(i  ,k  ,j  ) + &
                          ss11(i-1,k  ,j-1) + ss11(i  ,k  ,j-1) )

    ss22d(i,k,j) = 0.25*( ss22(i-1,k  ,j  ) + ss22(i  ,k  ,j  ) + &
                          ss22(i-1,k  ,j-1) + ss22(i  ,k  ,j-1) )

    ss13d(i,k,j) = 0.25*( ss13(i  ,k+1,j  ) + ss13(i  ,k+1,j-1) + &
                          ss13(i  ,k  ,j  ) + ss13(i  ,k  ,j-1) )

    rr13d(i,k,j) = 0.25*( rr13(i  ,k+1,j  ) + rr13(i  ,k+1,j-1) + &
                          rr13(i  ,k  ,j  ) + rr13(i  ,k  ,j-1) )

    ss23d(i,k,j) = 0.25*( ss23(i  ,k+1,j  ) + ss23(i-1,k+1,j  ) + &
                          ss23(i  ,k  ,j  ) + ss23(i-1,k  ,j  ) )

    rr23d(i,k,j) = 0.25*( rr23(i  ,k+1,j  ) + rr23(i-1,k+1,j  ) + &
                          rr23(i  ,k  ,j  ) + rr23(i-1,k  ,j  ) )

  END DO
  END DO
  END DO







  IF ( config_flags%sfs_opt .EQ. 1 ) THEN 

    DO j=j_start,j_end
    DO k=kts,ktf
    DO i=i_start,i_end

      delta = ( dx * dy / rdzw(i,k,j) )**0.33333333
      a = -1.0*( cs*delta )**2

      m12(i,k,j) = a*(   2.0*sqrt( 2.0*smnsmnd(i,k,j) )*ss12(i,k,j) &
                       + c1*(   ss11d(i,k,j)*ss12(i,k,j)            &
                              + ss22d(i,k,j)*ss12(i,k,j)            &
                              + ss13d(i,k,j)*ss23d(i,k,j)           &
                            )                                       &
                       + c2*(   ss11d(i,k,j)*rr12(i,k,j)            &
                              - ss13d(i,k,j)*rr23d(i,k,j)           & 
                              - ss22d(i,k,j)*rr12(i,k,j)            &
                              - ss23d(i,k,j)*rr13d(i,k,j)           &
                            )                                       &
                      )

    ENDDO
    ENDDO
    ENDDO

  ELSE 

    DO j=j_start,j_end
    DO k=kts,ktf
    DO i=i_start,i_end 

      delta = ( dx * dy / rdzw(i,k,j) )**0.33333333
      a = -1.0*ce*delta
      b = c3*delta

      m12(i,k,j) = a*(   2.0*sqrt( tked(i,k,j) )*s12(i,k,j)     &
                       + b*(                                    &
                               c1*(   ss11d(i,k,j)*ss12(i,k,j)  &
                                    + ss22d(i,k,j)*ss12(i,k,j)  &
                                    + ss13d(i,k,j)*ss23d(i,k,j) &
                                  )                             &
                             + c2*(   ss11d(i,k,j)*rr12(i,k,j)  &
                                    - ss13d(i,k,j)*rr23d(i,k,j) &
                                    - ss22d(i,k,j)*rr12(i,k,j)  &
                                    - ss23d(i,k,j)*rr13d(i,k,j) &
                                  )                             &
                           )                                    &
                     )
    ENDDO
    ENDDO
    ENDDO

  ENDIF

  RETURN

END SUBROUTINE calc_m12



SUBROUTINE calc_m13( m13,                          &
                     s11, s33,                     &
                     s12, s13, s23,                &
                     r12, r13, r23, smnsmn,        &
                     tke, rdzw, dx, dy,            &
                     fnm, fnp,                     &
                     config_flags,                 &
                     ids, ide, jds, jde, kds, kde, &
                     ims, ime, jms, jme, kms, kme, &
                     ips, ipe, jps, jpe, kps, kpe, &
                     its, ite, jts, jte, kts, kte  )







  IMPLICIT NONE

  REAL, DIMENSION( ims:ime, kms:kme, jms:jme ),          INTENT( OUT ) &
  :: m13           

  REAL, DIMENSION( ims:ime, kms:kme, jms:jme ),          INTENT( IN  ) &
  :: s11         & 
   , s33         & 
   , s12         & 
   , s13         & 
   , s23         & 
   , r12         & 
   , r13         & 
   , r23         & 
   , smnsmn      & 
   , tke         & 
   , rdzw          

  REAL,                                                  INTENT( IN  ) &
  :: dx          & 
   , dy            

  REAL, DIMENSION( kms:kme ),                            INTENT( IN  ) &
  :: fnm         & 
   , fnp           

  TYPE (grid_config_rec_type),                           INTENT( IN  ) &
  :: config_flags

  INTEGER,                                               INTENT( IN  ) &
  :: ids, ide, jds, jde, kds, kde, &
     ims, ime, jms, jme, kms, kme, &
     ips, ipe, jps, jpe, kps, kpe, &
     its, ite, jts, jte, kts, kte



  REAL, DIMENSION( its-1:ite+1, kms:kme, jts-1:jte+1 ) & 
  :: ss11        & 
   , ss33        &  
   , ss12        &  
   , ss13        & 
   , ss23        & 
   , rr12        & 
   , rr13        & 
   , rr23  

  REAL, DIMENSION( its-1:ite+1, kms:kme, jts-1:jte+1 ) & 
  :: tkee        & 
   , ss11e       & 
   , ss33e       & 
   , ss12e       & 
   , ss23e       & 
   , rr12e       & 
   , rr23e       &          
   , smnsmne 

  REAL :: delta, a, b

  INTEGER :: i, j, k, i_start, i_end, j_start, j_end, ktf, ie_ext








  ktf = MIN( kte, kde-1 )



  i_start = its
  i_end   = ite
  j_start = jts
  j_end   = MIN( jte, jde-1 )

    IF ( config_flags%open_xs .OR. config_flags%specified .OR. &
         config_flags%nested) i_start = MAX( ids+1, its )
    IF ( config_flags%open_xe .OR. config_flags%specified .OR. &
         config_flags%nested) i_end   = MIN( ide-1, ite )
    IF ( config_flags%open_ys .OR. config_flags%specified .OR. &
         config_flags%nested) j_start = MAX( jds+1, jts )
    IF ( config_flags%open_ye .OR. config_flags%specified .OR. &
         config_flags%nested) j_end   = MIN( jde-2, jte )
      IF ( config_flags%periodic_x ) i_start = its
      IF ( config_flags%periodic_x ) i_end = ite

  ie_ext = 1
  i_end = i_end + ie_ext   







  DO j=j_start,j_end+1
  DO k=kts,ktf
  DO i=i_start-1,i_end

    ss11(i,k,j)=s11(i,k,j)/2.0
    ss33(i,k,j)=s33(i,k,j)/2.0
    ss12(i,k,j)=s12(i,k,j)/2.0
    ss13(i,k,j)=s13(i,k,j)/2.0
    ss23(i,k,j)=s23(i,k,j)/2.0
    rr12(i,k,j)=r12(i,k,j)/2.0
    rr13(i,k,j)=r13(i,k,j)/2.0
    rr23(i,k,j)=r23(i,k,j)/2.0

  END DO
  END DO
  END DO







  DO j = j_start, j_end
  DO k = kts+1, ktf
  DO i = i_start, i_end

    tkee(i,k,j) = 0.5*( fnm(k)*( tke(i,k  ,j) + tke(i-1,k  ,j) ) + &
                        fnp(k)*( tke(i,k-1,j) + tke(i-1,k-1,j) ) )

    smnsmne(i,k,j) = 0.5*( fnm(k)*( smnsmn(i,k  ,j) + smnsmn(i-1,k  ,j) ) + &
                           fnp(k)*( smnsmn(i,k-1,j) + smnsmn(i-1,k-1,j) ) )

    ss11e(i,k,j) = 0.5*( fnm(k)*( ss11(i  ,k  ,j  ) + ss11(i-1,k  ,j  ) ) + &
                         fnp(k)*( ss11(i  ,k-1,j  ) + ss11(i-1,k-1,j  ) ) )

    ss33e(i,k,j) = 0.5*( fnm(k)*( ss33(i  ,k  ,j  ) + ss33(i-1,k  ,j  ) ) + &
                         fnp(k)*( ss33(i  ,k-1,j  ) + ss33(i-1,k-1,j  ) ) )
 
    ss12e(i,k,j) = 0.5*( fnm(k)*( ss12(i  ,k  ,j  ) + ss12(i  ,k  ,j+1) ) + &
                         fnp(k)*( ss12(i  ,k-1,j  ) + ss12(i  ,k-1,j+1) ) )

    rr12e(i,k,j) = 0.5*( fnm(k)*( rr12(i  ,k  ,j  ) + rr12(i  ,k  ,j+1) ) + &
                         fnp(k)*( rr12(i  ,k-1,j  ) + rr12(i  ,k-1,j+1) ) )

    ss23e(i,k,j) = 0.25*( ss23(i  ,k  ,j) + ss23(i  ,k  ,j+1) + &
                          ss23(i-1,k  ,j) + ss23(i-1,k  ,j+1) )

    rr23e(i,k,j) = 0.25*( rr23(i  ,k  ,j) + rr23(i  ,k  ,j+1) + &
                          rr23(i-1,k  ,j) + rr23(i-1,k  ,j+1) )

  END DO
  END DO
  END DO








  IF ( config_flags%sfs_opt .EQ. 1 ) THEN 

    DO j=j_start,j_end
    DO k=kts+1,ktf
    DO i=i_start,i_end

      delta = ( dx * dy / rdzw(i,k,j) )**0.33333333
      a = -1.0*( cs*delta )**2

      m13(i,k,j) = a*(   2.0*sqrt( 2.0*smnsmne(i,k,j) )*ss13(i,k,j) &
                       + c1*(   ss11e(i,k,j)*ss13(i,k,j)            &
                              + ss12e(i,k,j)*ss23e(i,k,j)           &
                              + ss13(i,k,j)*ss33e(i,k,j)            &
                            )                                       &
                       + c2*(   ss11e(i,k,j)*rr13(i,k,j)            &
                              + ss12e(i,k,j)*rr23e(i,k,j)           &
                              - ss23e(i,k,j)*rr12e(i,k,j)           &
                              - ss33e(i,k,j)*rr13(i,k,j)            &
                            )                                       &
                     )

    ENDDO
    ENDDO
    ENDDO

  ELSE 

    DO j=j_start,j_end
    DO k=kts+1,ktf
    DO i=i_start,i_end

      delta = ( dx * dy / rdzw(i,k,j) )**0.33333333
      a = -1.0*ce*delta
      b = c3*delta

      m13(i,k,j) = a*(   2.0*sqrt( tkee(i,k,j) )*ss13(i,k,j)    &
                       + b*(                                    &
                               c1*(   ss11e(i,k,j)*ss13(i,k,j)  &
                                    + ss12e(i,k,j)*ss23e(i,k,j) &
                                    + ss13(i,k,j)*ss33e(i,k,j)  &
                                  )                             &
                             + c2*(   ss11e(i,k,j)*rr13(i,k,j)  &
                                    + ss12e(i,k,j)*rr23e(i,k,j) &
                                    - ss23e(i,k,j)*rr12e(i,k,j) &
                                    - ss33e(i,k,j)*rr13(i,k,j)  &
                                  )                             &
                           )                                    &
                     )

    ENDDO
    ENDDO
    ENDDO

  ENDIF

  RETURN

END SUBROUTINE calc_m13



SUBROUTINE calc_m23( m23,                          &
                     s22, s33,                     &
                     s12, s13, s23,                &
                     r12, r13, r23, smnsmn,        &
                     tke, rdzw, dx, dy,            &
                     fnm, fnp,                     &
                     config_flags,                 &
                     ids, ide, jds, jde, kds, kde, &
                     ims, ime, jms, jme, kms, kme, &
                     ips, ipe, jps, jpe, kps, kpe, &
                     its, ite, jts, jte, kts, kte  )







  IMPLICIT NONE

  REAL, DIMENSION( ims:ime, kms:kme, jms:jme ),          INTENT( OUT ) &
  :: m23           

  REAL, DIMENSION( ims:ime, kms:kme, jms:jme ),          INTENT(  IN ) &
  :: s22         & 
   , s33         & 
   , s12         & 
   , s13         & 
   , s23         & 
   , r12         & 
   , r13         & 
   , r23         & 
   , smnsmn      & 
   , tke         & 
   , rdzw          

  REAL,                                                  INTENT( IN  ) &
  :: dx          & 
   , dy            

  REAL, DIMENSION( kms:kme ),                            INTENT( IN  ) &
  :: fnm         & 
   , fnp           

  TYPE (grid_config_rec_type),                           INTENT( IN  ) &
  :: config_flags

  INTEGER,                                               INTENT( IN  ) &
  :: ids, ide, jds, jde, kds, kde, &
     ims, ime, jms, jme, kms, kme, &
     ips, ipe, jps, jpe, kps, kpe, &
     its, ite, jts, jte, kts, kte



  REAL, DIMENSION( its-1:ite+1, kms:kme, jts-1:jte+1 ) & 
  :: ss22        & 
   , ss33        &  
   , ss12        &  
   , ss13        & 
   , ss23        & 
   , rr12        & 
   , rr13        & 
   , rr23  

  REAL, DIMENSION( its-1:ite+1, kms:kme, jts-1:jte+1 ) & 
  :: tkef        & 
   , ss22f       & 
   , ss33f       & 
   , ss12f       & 
   , ss13f       & 
   , rr12f       & 
   , rr13f       &          
   , smnsmnf    

  REAL :: delta, a, b

  INTEGER :: i, j, k, i_start, i_end, j_start, j_end, ktf, je_ext








  ktf = MIN( kte, kde-1 )



  i_start = its
  i_end   = MIN( ite, ide-1 )
  j_start = jts
  j_end   = jte

    IF ( config_flags%open_xs .OR. config_flags%specified .OR. &
         config_flags%nested) i_start = MAX( ids+1, its )
    IF ( config_flags%open_xe .OR. config_flags%specified .OR. &
         config_flags%nested) i_end   = MIN( ide-2, ite )
    IF ( config_flags%open_ys .OR. config_flags%specified .OR. &
         config_flags%nested) j_start = MAX( jds+1, jts )
    IF ( config_flags%open_ye .OR. config_flags%specified .OR. &
         config_flags%nested) j_end   = MIN( jde-1, jte )
      IF ( config_flags%periodic_x ) i_start = its
      IF ( config_flags%periodic_x ) i_end = MIN( ite, ide-1 )

  je_ext = 1
  j_end = j_end + je_ext   







  DO j=j_start-1,j_end
  DO k=kts,ktf
  DO i=i_start,i_end+1

    ss22(i,k,j)=s22(i,k,j)/2.0
    ss33(i,k,j)=s33(i,k,j)/2.0
    ss12(i,k,j)=s12(i,k,j)/2.0
    ss13(i,k,j)=s13(i,k,j)/2.0
    ss23(i,k,j)=s23(i,k,j)/2.0
    rr12(i,k,j)=r12(i,k,j)/2.0
    rr13(i,k,j)=r13(i,k,j)/2.0
    rr23(i,k,j)=r23(i,k,j)/2.0

  END DO
  END DO
  END DO







  DO j = j_start, j_end
  DO k = kts+1, ktf
  DO i = i_start, i_end

    tkef(i,k,j) = 0.5*( fnm(k)*( tke(i  ,k  ,j  ) + tke(i  ,k  ,j-1) ) + &
                        fnp(k)*( tke(i  ,k-1,j  ) + tke(i  ,k-1,j-1) ) )

    smnsmnf(i,k,j) = 0.5*( fnm(k)*( smnsmn(i  ,k  ,j  ) + smnsmn(i  ,k  ,j-1) ) + &
                           fnp(k)*( smnsmn(i  ,k-1,j  ) + smnsmn(i  ,k-1,j-1) ) )

    ss22f(i,k,j) = 0.5*( fnm(k)*( ss22(i  ,k  ,j  ) + ss22(i  ,k  ,j-1) ) + &
                         fnp(k)*( ss22(i  ,k-1,j  ) + ss22(i  ,k-1,j-1) ) )

    ss33f(i,k,j) = 0.5*( fnm(k)*( ss33(i  ,k  ,j  ) + ss33(i  ,k  ,j-1) ) + &
                         fnp(k)*( ss33(i  ,k-1,j  ) + ss33(i  ,k-1,j-1) ) )

    ss12f(i,k,j) = 0.5*( fnm(k)*( ss12(i  ,k  ,j  ) + ss12(i+1,k  ,j  ) ) + &
                         fnp(k)*( ss12(i  ,k-1,j  ) + ss12(i+1,k-1,j  ) ) )

    rr12f(i,k,j) = 0.5*( fnm(k)*( rr12(i  ,k  ,j  ) + rr12(i+1,k  ,j  ) ) + &
                         fnp(k)*( rr12(i  ,k-1,j  ) + rr12(i+1,k-1,j  ) ) )

    ss13f(i,k,j) = 0.25*( ss13(i  ,k  ,j  ) + ss13(i  ,k  ,j-1) + &
                          ss13(i+1,k  ,j-1) + ss13(i+1,k  ,j  ) )

    rr13f(i,k,j) = 0.25*( rr13(i  ,k  ,j  ) + rr13(i  ,k  ,j-1) + &
                          rr13(i+1,k  ,j-1) + rr13(i+1,k  ,j  ) )

  END DO
  END DO
  END DO
 






  IF ( config_flags%sfs_opt .EQ. 1 ) THEN 

    DO j=j_start,j_end
    DO k=kts+1,ktf
    DO i=i_start,i_end

      delta = ( dx * dy / rdzw(i,k,j) )**0.33333333
      a = -1.0*( cs*delta )**2

      m23(i,k,j) = a*(   2.0*sqrt( 2.0*smnsmnf(i,k,j) )*ss23(i,k,j) &
                       + c1*(   ss12f(i,k,j)*ss13f(i,k,j)           &
                              + ss22f(i,k,j)*ss23(i,k,j)            &
                              + ss23(i,k,j) *ss33f(i,k,j)           &
                             )                                      &
                       + c2*(   ss12f(i,k,j)*rr13f(i,k,j)           &
                              + ss22f(i,k,j)*rr23(i,k,j)            &
                              + ss13f(i,k,j)*rr12f(i,k,j)           &
                              - ss33f(i,k,j)*rr23(i,k,j)            &
                            )                                       &
                     )

    ENDDO
    ENDDO
    ENDDO

  ELSE 

    DO j=j_start,j_end
    DO k=kts+1,ktf
    DO i=i_start,i_end

      delta = ( dx * dy / rdzw(i,k,j) )**0.33333333
      a = -1.0*ce*delta
      b = c3*delta

      m23(i,k,j) = a*(   2.0*sqrt( tkef(i,k,j) )*ss23(i,k,j)    &
                       + b*(                                    &
                               c1*(   ss12f(i,k,j)*ss13f(i,k,j) &
                                    + ss22f(i,k,j)*ss23(i,k,j)  &
                                    + ss23(i,k,j) *ss33f(i,k,j) &
                                  )                             &
                             + c2*(   ss12f(i,k,j)*rr13f(i,k,j) &
                                    + ss22f(i,k,j)*rr23(i,k,j)  &
                                    + ss13f(i,k,j)*rr12f(i,k,j) &
                                    - ss33f(i,k,j)*rr23(i,k,j)  &
                                  )                             &
                           )                                    &
                     )

    ENDDO
    ENDDO
    ENDDO

  ENDIF

  RETURN

END SUBROUTINE calc_m23



END MODULE module_sfs_nba
